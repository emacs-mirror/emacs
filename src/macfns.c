/* Graphical user interface functions for Mac OS.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#include <config.h>

#include <signal.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <errno.h>

#include "lisp.h"
#include "charset.h"
#include "macterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "fontset.h"
#include "intervals.h"
#include "keyboard.h"
#include "blockinput.h"
#include "epaths.h"
#include "termhooks.h"
#include "coding.h"
#include "ccl.h"
#include "systime.h"

/* #include "bitmaps/gray.xbm" */
#define gray_width 2
#define gray_height 2
static unsigned char gray_bits[] = {
   0x01, 0x02};

/*#include <commdlg.h>
#include <shellapi.h>*/
#include <ctype.h>

#include <stdlib.h>
#include <string.h>
#ifndef MAC_OSX
#include <alloca.h>
#endif

#ifdef MAC_OSX
#undef mktime
#undef DEBUG
#undef Z
#undef free
#undef malloc
#undef realloc
/* Macros max and min defined in lisp.h conflict with those in
   precompiled header Carbon.h.  */
#undef max
#undef min
#include <Carbon/Carbon.h>
#undef Z
#define Z (current_buffer->text->z)
#undef free
#define free unexec_free
#undef malloc
#define malloc unexec_malloc
#undef realloc
#define realloc unexec_realloc
#undef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#undef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#else /* not MAC_OSX */ 
#include <Windows.h>
#include <Gestalt.h>
#include <TextUtils.h>
#endif /* not MAC_OSX */

/*extern void free_frame_menubar ();
extern double atof ();
extern int w32_console_toggle_lock_key (int vk_code, Lisp_Object new_state);
extern int quit_char;*/

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

/* The gray bitmap `bitmaps/gray'.  This is done because macterm.c uses
   it, and including `bitmaps/gray' more than once is a problem when
   config.h defines `static' as an empty replacement string.  */

int gray_bitmap_width = gray_width;
int gray_bitmap_height = gray_height;
unsigned char *gray_bitmap_bits = gray_bits;

/* The name we're using in resource queries.  */

Lisp_Object Vx_resource_name;

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

/* Nonzero if using Windows.  */

static int mac_in_use;

/* Non nil if no window manager is in use.  */

Lisp_Object Vx_no_window_manager;

/* Search path for bitmap files.  */

Lisp_Object Vx_bitmap_file_path;

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */

Lisp_Object Vx_pixel_size_width_font_regexp;

/* Evaluate this expression to rebuild the section of syms_of_macfns
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

extern Lisp_Object Qtop;
extern Lisp_Object Qdisplay;
Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
extern Lisp_Object Qtool_bar_lines;

/* These are defined in frame.c.  */
extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;
extern Lisp_Object Qtool_bar_lines;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;

extern int mac_initialized;

/* Functions in macterm.c.  */
extern void x_set_offset (struct frame *, int, int, int);
extern void x_wm_set_icon_position (struct frame *, int, int);
extern void x_display_cursor (struct window *, int, int, int, int, int);
extern void x_set_window_size (struct frame *, int, int, int);
extern void x_make_frame_visible (struct frame *);
extern struct mac_display_info *mac_term_init (Lisp_Object, char *, char *);
extern struct font_info *x_get_font_info (FRAME_PTR, int);
extern struct font_info *x_load_font (struct frame *, char *, int);
extern void x_find_ccl_program (struct font_info *);
extern struct font_info *x_query_font (struct frame *, char *);
extern void mac_initialize ();

/* compare two strings ignoring case */

static int
stricmp (const char *s, const char *t)
{
  for ( ; tolower (*s) == tolower (*t); s++, t++)
    if (*s == '\0')
      return 0;
  return tolower (*s) - tolower (*t);
}

/* compare two strings up to n characters, ignoring case */

static int
strnicmp (const char *s, const char *t, unsigned int n)
{
  for ( ; n-- > 0 && tolower (*s) == tolower (*t); s++, t++)
    if (*s == '\0')
      return 0;
  return n == 0 ? 0 : tolower (*s) - tolower (*t);
}


/* Error if we are not running on Mac OS.  */

void
check_mac ()
{
  if (! mac_in_use)
    error ("Mac OS not in use or not initialized");
}

/* Nonzero if we can use mouse menus.
   You should not call this unless HAVE_MENUS is defined.  */
  
int
have_menus_p ()
{
  return mac_in_use;
}

/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for Mac.  */

FRAME_PTR
check_x_frame (frame)
     Lisp_Object frame;
{
  FRAME_PTR f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  if (! FRAME_MAC_P (f))
    error ("non-mac frame used");
  return f;
}

/* Let the user specify an display with a frame.
   nil stands for the selected frame--or, if that is not a mac frame,
   the first display on the list.  */

static struct mac_display_info *
check_x_display_info (frame)
     Lisp_Object frame;
{
  if (!mac_initialized)
    {
      mac_initialize ();
      mac_initialized = 1;
    }

  if (NILP (frame))
    {
      struct frame *sf = XFRAME (selected_frame);
      
      if (FRAME_MAC_P (sf) && FRAME_LIVE_P (sf))
	return FRAME_MAC_DISPLAY_INFO (sf);
      else
	return &one_mac_display_info;
    }
  else if (STRINGP (frame))
    return x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      if (! FRAME_MAC_P (f))
	error ("non-mac frame used");
      return FRAME_MAC_DISPLAY_INFO (f);
    }
}

/* Return the Emacs frame-object corresponding to an mac window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use GC_xxx type test macros.  */

struct frame *
x_window_to_frame (dpyinfo, wdesc)
     struct mac_display_info *dpyinfo;
     WindowPtr wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_W32_P (f) || FRAME_MAC_DISPLAY_INFO (f) != dpyinfo)
	continue;
      /*if (f->output_data.w32->hourglass_window == wdesc)
        return f;*/

      /* MAC_TODO: Check tooltips when supported.  */
      if (FRAME_MAC_WINDOW (f) == wdesc)
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
  return FRAME_MAC_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

int
x_bitmap_width (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_MAC_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

#if 0 /* MAC_TODO : not used anywhere (?) */
int
x_bitmap_pixmap (f, id)
     FRAME_PTR f;
     int id;
{
  return (int) FRAME_MAC_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}
#endif

/* Allocate a new bitmap record.  Returns index of new record.  */

static int
x_allocate_bitmap_record (f)
     FRAME_PTR f;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->bitmaps == NULL)
    {
      dpyinfo->bitmaps_size = 10;
      dpyinfo->bitmaps = (struct mac_bitmap_record *)
	xmalloc (dpyinfo->bitmaps_size * sizeof (struct mac_bitmap_record));
      dpyinfo->bitmaps_last = 1;
      return 1;
    }

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps_size *= 2;
  dpyinfo->bitmaps = (struct mac_bitmap_record *)
    xrealloc (dpyinfo->bitmaps,
	      dpyinfo->bitmaps_size * sizeof (struct mac_bitmap_record));
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id
   ID.  */

void
x_reference_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  ++FRAME_MAC_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at
   BITS.  */

int
x_create_bitmap_from_data (f, bits, width, height)
     struct frame *f;
     char *bits;
     unsigned int width, height;
{
  struct x_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  int id;

  /* MAC_TODO: for now fail if width is not mod 16 (toolbox requires it) */

  id = x_allocate_bitmap_record (f);
  
  if (width % 16 != 0)
    return -1;

  dpyinfo->bitmaps[id - 1].bitmap_data = (char *) xmalloc (height * width);
  if (! dpyinfo->bitmaps[id - 1].bitmap_data)
    return -1;

  bcopy (bits, dpyinfo->bitmaps[id - 1].bitmap_data, height * width);

  dpyinfo->bitmaps[id - 1].refcount = 1;
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
#if 0 /* MAC_TODO : bitmap support */
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
  fd = openp (Vx_bitmap_file_path, file, "", &found, Qnil);
  if (fd < 0)
    return -1;
  /* LoadLibraryEx won't handle special files handled by Emacs handler.  */
  if (fd == 0)
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
#endif  /* MAC_TODO */
}

/* Remove reference to bitmap with id number ID.  */

void
x_destroy_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  if (id > 0)
    {
      --dpyinfo->bitmaps[id - 1].refcount;
      if (dpyinfo->bitmaps[id - 1].refcount == 0)
	{
	  BLOCK_INPUT;
	  dpyinfo->bitmaps[id - 1].bitmap_data = NULL;
	  UNBLOCK_INPUT;
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

static void
x_destroy_all_bitmaps (dpyinfo)
     struct mac_display_info *dpyinfo;
{
  int i;
  for (i = 0; i < dpyinfo->bitmaps_last; i++)
    if (dpyinfo->bitmaps[i].refcount > 0)
      xfree (dpyinfo->bitmaps[i].bitmap_data);
  dpyinfo->bitmaps_last = 0;
}

/* Connect the frame-parameter names for W32 frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   but can be interpreted as an `enum x_frame_parm' in C.  */

struct x_frame_parm_table
{
  char *name;
  void (*setter) P_ ((struct frame *, Lisp_Object, Lisp_Object));
};

void x_set_foreground_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_line_spacing P_ ((struct frame *, Lisp_Object, Lisp_Object));
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
#if 0 /* MAC_TODO: no icons for Mac */
  "icon-type", x_set_icon_type,
#endif
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
#if 0 /* MAC_TODO: cannot set color of scroll bar on the Mac? */
  "scroll-bar-foreground", x_set_scroll_bar_foreground,
  "scroll-bar-background", x_set_scroll_bar_background,
#endif
  "screen-gamma", x_set_screen_gamma,
  "line-spacing", x_set_line_spacing
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to Mac.  */

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
      if (f->output_data.mac->left_pos < 0)
	left = Fcons (Qplus,
		      Fcons (make_number (f->output_data.mac->left_pos),
			     Qnil));
      else
	XSETINT (left, f->output_data.mac->left_pos);
    }
  if (EQ (top, Qunbound))
    {
      top_no_change = 1;
      if (f->output_data.mac->top_pos < 0)
	top = Fcons (Qplus,
		     Fcons (make_number (f->output_data.mac->top_pos), Qnil));
      else
	XSETINT (top, f->output_data.mac->top_pos);
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
	&& ! (NUMBERP (left) && XINT (left) == f->output_data.mac->left_pos
	      && NUMBERP (top) && XINT (top) == f->output_data.mac->top_pos))
      {
	int leftpos = 0;
	int toppos = 0;

	/* Record the signs.  */
	f->output_data.mac->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->output_data.mac->size_hint_flags |= XNegative;
	else if (INTEGERP (left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->output_data.mac->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->output_data.mac->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->output_data.mac->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->output_data.mac->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->output_data.mac->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
	  }


	/* Store the numeric value of the position.  */
	f->output_data.mac->top_pos = toppos;
	f->output_data.mac->left_pos = leftpos;

	f->output_data.mac->win_gravity = NorthWestGravity;

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
  Point pt;
  GrafPtr oldport;

#ifdef TARGET_API_MAC_CARBON
  {
    Rect r;
    
    GetWindowPortBounds (f->output_data.mac->mWP, &r);
    SetPt (&pt, r.left, r.top);
  }
#else /* not TARGET_API_MAC_CARBON */
  SetPt (&pt,
	 f->output_data.mac->mWP->portRect.left,
	 f->output_data.mac->mWP->portRect.top);
#endif /* not TARGET_API_MAC_CARBON */
  GetPort (&oldport);
  LocalToGlobal (&pt);
  SetPort (oldport);
  
  *xptr = pt.h;
  *yptr = pt.v;
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to Mac and whose values are not
   correctly recorded in the frame's param_alist need to be considered
   here.  */

void
x_report_frame_params (f, alistptr)
     struct frame *f;
     Lisp_Object *alistptr;
{
  char buf[16];
  Lisp_Object tem;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->output_data.mac->left_pos);
  if (f->output_data.mac->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->output_data.mac->top_pos);
  if (f->output_data.mac->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
       	   make_number (f->output_data.mac->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
       	   make_number (f->output_data.mac->internal_border_width));
  sprintf (buf, "%ld", (long) FRAME_MAC_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
       	   build_string (buf));
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_MAC_DISPLAY_INFO (f)->name_list_element));
}

/* The default colors for the Mac color map */
typedef struct colormap_t 
{
  unsigned long color;
  char *name;
} colormap_t;

colormap_t mac_color_map[] = 
{
  { RGB_TO_ULONG(255, 250, 250), "snow" },
  { RGB_TO_ULONG(248, 248, 255), "ghost white" },
  { RGB_TO_ULONG(248, 248, 255), "GhostWhite" },
  { RGB_TO_ULONG(245, 245, 245), "white smoke" },
  { RGB_TO_ULONG(245, 245, 245), "WhiteSmoke" },
  { RGB_TO_ULONG(220, 220, 220), "gainsboro" },
  { RGB_TO_ULONG(255, 250, 240), "floral white" },
  { RGB_TO_ULONG(255, 250, 240), "FloralWhite" },
  { RGB_TO_ULONG(253, 245, 230), "old lace" },
  { RGB_TO_ULONG(253, 245, 230), "OldLace" },
  { RGB_TO_ULONG(250, 240, 230), "linen" },
  { RGB_TO_ULONG(250, 235, 215), "antique white" },
  { RGB_TO_ULONG(250, 235, 215), "AntiqueWhite" },
  { RGB_TO_ULONG(255, 239, 213), "papaya whip" },
  { RGB_TO_ULONG(255, 239, 213), "PapayaWhip" },
  { RGB_TO_ULONG(255, 235, 205), "blanched almond" },
  { RGB_TO_ULONG(255, 235, 205), "BlanchedAlmond" },
  { RGB_TO_ULONG(255, 228, 196), "bisque" },
  { RGB_TO_ULONG(255, 218, 185), "peach puff" },
  { RGB_TO_ULONG(255, 218, 185), "PeachPuff" },
  { RGB_TO_ULONG(255, 222, 173), "navajo white" },
  { RGB_TO_ULONG(255, 222, 173), "NavajoWhite" },
  { RGB_TO_ULONG(255, 228, 181), "moccasin" },
  { RGB_TO_ULONG(255, 248, 220), "cornsilk" },
  { RGB_TO_ULONG(255, 255, 240), "ivory" },
  { RGB_TO_ULONG(255, 250, 205), "lemon chiffon" },
  { RGB_TO_ULONG(255, 250, 205), "LemonChiffon" },
  { RGB_TO_ULONG(255, 245, 238), "seashell" },
  { RGB_TO_ULONG(240, 255, 240), "honeydew" },
  { RGB_TO_ULONG(245, 255, 250), "mint cream" },
  { RGB_TO_ULONG(245, 255, 250), "MintCream" },
  { RGB_TO_ULONG(240, 255, 255), "azure" },
  { RGB_TO_ULONG(240, 248, 255), "alice blue" },
  { RGB_TO_ULONG(240, 248, 255), "AliceBlue" },
  { RGB_TO_ULONG(230, 230, 250), "lavender" },
  { RGB_TO_ULONG(255, 240, 245), "lavender blush" },
  { RGB_TO_ULONG(255, 240, 245), "LavenderBlush" },
  { RGB_TO_ULONG(255, 228, 225), "misty rose" },
  { RGB_TO_ULONG(255, 228, 225), "MistyRose" },
  { RGB_TO_ULONG(255, 255, 255), "white" },
  { RGB_TO_ULONG(0  , 0  , 0  ), "black" },
  { RGB_TO_ULONG(47 , 79 , 79 ), "dark slate gray" },
  { RGB_TO_ULONG(47 , 79 , 79 ), "DarkSlateGray" },
  { RGB_TO_ULONG(47 , 79 , 79 ), "dark slate grey" },
  { RGB_TO_ULONG(47 , 79 , 79 ), "DarkSlateGrey" },
  { RGB_TO_ULONG(105, 105, 105), "dim gray" },
  { RGB_TO_ULONG(105, 105, 105), "DimGray" },
  { RGB_TO_ULONG(105, 105, 105), "dim grey" },
  { RGB_TO_ULONG(105, 105, 105), "DimGrey" },
  { RGB_TO_ULONG(112, 128, 144), "slate gray" },
  { RGB_TO_ULONG(112, 128, 144), "SlateGray" },
  { RGB_TO_ULONG(112, 128, 144), "slate grey" },
  { RGB_TO_ULONG(112, 128, 144), "SlateGrey" },
  { RGB_TO_ULONG(119, 136, 153), "light slate gray" },
  { RGB_TO_ULONG(119, 136, 153), "LightSlateGray" },
  { RGB_TO_ULONG(119, 136, 153), "light slate grey" },
  { RGB_TO_ULONG(119, 136, 153), "LightSlateGrey" },
  { RGB_TO_ULONG(190, 190, 190), "gray" },
  { RGB_TO_ULONG(190, 190, 190), "grey" },
  { RGB_TO_ULONG(211, 211, 211), "light grey" },
  { RGB_TO_ULONG(211, 211, 211), "LightGrey" },
  { RGB_TO_ULONG(211, 211, 211), "light gray" },
  { RGB_TO_ULONG(211, 211, 211), "LightGray" },
  { RGB_TO_ULONG(25 , 25 , 112), "midnight blue" },
  { RGB_TO_ULONG(25 , 25 , 112), "MidnightBlue" },
  { RGB_TO_ULONG(0  , 0  , 128), "navy" },
  { RGB_TO_ULONG(0  , 0  , 128), "navy blue" },
  { RGB_TO_ULONG(0  , 0  , 128), "NavyBlue" },
  { RGB_TO_ULONG(100, 149, 237), "cornflower blue" },
  { RGB_TO_ULONG(100, 149, 237), "CornflowerBlue" },
  { RGB_TO_ULONG(72 , 61 , 139), "dark slate blue" },
  { RGB_TO_ULONG(72 , 61 , 139), "DarkSlateBlue" },
  { RGB_TO_ULONG(106, 90 , 205), "slate blue" },
  { RGB_TO_ULONG(106, 90 , 205), "SlateBlue" },
  { RGB_TO_ULONG(123, 104, 238), "medium slate blue" },
  { RGB_TO_ULONG(123, 104, 238), "MediumSlateBlue" },
  { RGB_TO_ULONG(132, 112, 255), "light slate blue" },
  { RGB_TO_ULONG(132, 112, 255), "LightSlateBlue" },
  { RGB_TO_ULONG(0  , 0  , 205), "medium blue" },
  { RGB_TO_ULONG(0  , 0  , 205), "MediumBlue" },
  { RGB_TO_ULONG(65 , 105, 225), "royal blue" },
  { RGB_TO_ULONG(65 , 105, 225), "RoyalBlue" },
  { RGB_TO_ULONG(0  , 0  , 255), "blue" },
  { RGB_TO_ULONG(30 , 144, 255), "dodger blue" },
  { RGB_TO_ULONG(30 , 144, 255), "DodgerBlue" },
  { RGB_TO_ULONG(0  , 191, 255), "deep sky blue" },
  { RGB_TO_ULONG(0  , 191, 255), "DeepSkyBlue" },
  { RGB_TO_ULONG(135, 206, 235), "sky blue" },
  { RGB_TO_ULONG(135, 206, 235), "SkyBlue" },
  { RGB_TO_ULONG(135, 206, 250), "light sky blue" },
  { RGB_TO_ULONG(135, 206, 250), "LightSkyBlue" },
  { RGB_TO_ULONG(70 , 130, 180), "steel blue" },
  { RGB_TO_ULONG(70 , 130, 180), "SteelBlue" },
  { RGB_TO_ULONG(176, 196, 222), "light steel blue" },
  { RGB_TO_ULONG(176, 196, 222), "LightSteelBlue" },
  { RGB_TO_ULONG(173, 216, 230), "light blue" },
  { RGB_TO_ULONG(173, 216, 230), "LightBlue" },
  { RGB_TO_ULONG(176, 224, 230), "powder blue" },
  { RGB_TO_ULONG(176, 224, 230), "PowderBlue" },
  { RGB_TO_ULONG(175, 238, 238), "pale turquoise" },
  { RGB_TO_ULONG(175, 238, 238), "PaleTurquoise" },
  { RGB_TO_ULONG(0  , 206, 209), "dark turquoise" },
  { RGB_TO_ULONG(0  , 206, 209), "DarkTurquoise" },
  { RGB_TO_ULONG(72 , 209, 204), "medium turquoise" },
  { RGB_TO_ULONG(72 , 209, 204), "MediumTurquoise" },
  { RGB_TO_ULONG(64 , 224, 208), "turquoise" },
  { RGB_TO_ULONG(0  , 255, 255), "cyan" },
  { RGB_TO_ULONG(224, 255, 255), "light cyan" },
  { RGB_TO_ULONG(224, 255, 255), "LightCyan" },
  { RGB_TO_ULONG(95 , 158, 160), "cadet blue" },
  { RGB_TO_ULONG(95 , 158, 160), "CadetBlue" },
  { RGB_TO_ULONG(102, 205, 170), "medium aquamarine" },
  { RGB_TO_ULONG(102, 205, 170), "MediumAquamarine" },
  { RGB_TO_ULONG(127, 255, 212), "aquamarine" },
  { RGB_TO_ULONG(0  , 100, 0  ), "dark green" },
  { RGB_TO_ULONG(0  , 100, 0  ), "DarkGreen" },
  { RGB_TO_ULONG(85 , 107, 47 ), "dark olive green" },
  { RGB_TO_ULONG(85 , 107, 47 ), "DarkOliveGreen" },
  { RGB_TO_ULONG(143, 188, 143), "dark sea green" },
  { RGB_TO_ULONG(143, 188, 143), "DarkSeaGreen" },
  { RGB_TO_ULONG(46 , 139, 87 ), "sea green" },
  { RGB_TO_ULONG(46 , 139, 87 ), "SeaGreen" },
  { RGB_TO_ULONG(60 , 179, 113), "medium sea green" },
  { RGB_TO_ULONG(60 , 179, 113), "MediumSeaGreen" },
  { RGB_TO_ULONG(32 , 178, 170), "light sea green" },
  { RGB_TO_ULONG(32 , 178, 170), "LightSeaGreen" },
  { RGB_TO_ULONG(152, 251, 152), "pale green" },
  { RGB_TO_ULONG(152, 251, 152), "PaleGreen" },
  { RGB_TO_ULONG(0  , 255, 127), "spring green" },
  { RGB_TO_ULONG(0  , 255, 127), "SpringGreen" },
  { RGB_TO_ULONG(124, 252, 0  ), "lawn green" },
  { RGB_TO_ULONG(124, 252, 0  ), "LawnGreen" },
  { RGB_TO_ULONG(0  , 255, 0  ), "green" },
  { RGB_TO_ULONG(127, 255, 0  ), "chartreuse" },
  { RGB_TO_ULONG(0  , 250, 154), "medium spring green" },
  { RGB_TO_ULONG(0  , 250, 154), "MediumSpringGreen" },
  { RGB_TO_ULONG(173, 255, 47 ), "green yellow" },
  { RGB_TO_ULONG(173, 255, 47 ), "GreenYellow" },
  { RGB_TO_ULONG(50 , 205, 50 ), "lime green" },
  { RGB_TO_ULONG(50 , 205, 50 ), "LimeGreen" },
  { RGB_TO_ULONG(154, 205, 50 ), "yellow green" },
  { RGB_TO_ULONG(154, 205, 50 ), "YellowGreen" },
  { RGB_TO_ULONG(34 , 139, 34 ), "forest green" },
  { RGB_TO_ULONG(34 , 139, 34 ), "ForestGreen" },
  { RGB_TO_ULONG(107, 142, 35 ), "olive drab" },
  { RGB_TO_ULONG(107, 142, 35 ), "OliveDrab" },
  { RGB_TO_ULONG(189, 183, 107), "dark khaki" },
  { RGB_TO_ULONG(189, 183, 107), "DarkKhaki" },
  { RGB_TO_ULONG(240, 230, 140), "khaki" },
  { RGB_TO_ULONG(238, 232, 170), "pale goldenrod" },
  { RGB_TO_ULONG(238, 232, 170), "PaleGoldenrod" },
  { RGB_TO_ULONG(250, 250, 210), "light goldenrod yellow" },
  { RGB_TO_ULONG(250, 250, 210), "LightGoldenrodYellow" },
  { RGB_TO_ULONG(255, 255, 224), "light yellow" },
  { RGB_TO_ULONG(255, 255, 224), "LightYellow" },
  { RGB_TO_ULONG(255, 255, 0  ), "yellow" },
  { RGB_TO_ULONG(255, 215, 0  ), "gold" },
  { RGB_TO_ULONG(238, 221, 130), "light goldenrod" },
  { RGB_TO_ULONG(238, 221, 130), "LightGoldenrod" },
  { RGB_TO_ULONG(218, 165, 32 ), "goldenrod" },
  { RGB_TO_ULONG(184, 134, 11 ), "dark goldenrod" },
  { RGB_TO_ULONG(184, 134, 11 ), "DarkGoldenrod" },
  { RGB_TO_ULONG(188, 143, 143), "rosy brown" },
  { RGB_TO_ULONG(188, 143, 143), "RosyBrown" },
  { RGB_TO_ULONG(205, 92 , 92 ), "indian red" },
  { RGB_TO_ULONG(205, 92 , 92 ), "IndianRed" },
  { RGB_TO_ULONG(139, 69 , 19 ), "saddle brown" },
  { RGB_TO_ULONG(139, 69 , 19 ), "SaddleBrown" },
  { RGB_TO_ULONG(160, 82 , 45 ), "sienna" },
  { RGB_TO_ULONG(205, 133, 63 ), "peru" },
  { RGB_TO_ULONG(222, 184, 135), "burlywood" },
  { RGB_TO_ULONG(245, 245, 220), "beige" },
  { RGB_TO_ULONG(245, 222, 179), "wheat" },
  { RGB_TO_ULONG(244, 164, 96 ), "sandy brown" },
  { RGB_TO_ULONG(244, 164, 96 ), "SandyBrown" },
  { RGB_TO_ULONG(210, 180, 140), "tan" },
  { RGB_TO_ULONG(210, 105, 30 ), "chocolate" },
  { RGB_TO_ULONG(178, 34 , 34 ), "firebrick" },
  { RGB_TO_ULONG(165, 42 , 42 ), "brown" },
  { RGB_TO_ULONG(233, 150, 122), "dark salmon" },
  { RGB_TO_ULONG(233, 150, 122), "DarkSalmon" },
  { RGB_TO_ULONG(250, 128, 114), "salmon" },
  { RGB_TO_ULONG(255, 160, 122), "light salmon" },
  { RGB_TO_ULONG(255, 160, 122), "LightSalmon" },
  { RGB_TO_ULONG(255, 165, 0  ), "orange" },
  { RGB_TO_ULONG(255, 140, 0  ), "dark orange" },
  { RGB_TO_ULONG(255, 140, 0  ), "DarkOrange" },
  { RGB_TO_ULONG(255, 127, 80 ), "coral" },
  { RGB_TO_ULONG(240, 128, 128), "light coral" },
  { RGB_TO_ULONG(240, 128, 128), "LightCoral" },
  { RGB_TO_ULONG(255, 99 , 71 ), "tomato" },
  { RGB_TO_ULONG(255, 69 , 0  ), "orange red" },
  { RGB_TO_ULONG(255, 69 , 0  ), "OrangeRed" },
  { RGB_TO_ULONG(255, 0  , 0  ), "red" },
  { RGB_TO_ULONG(255, 105, 180), "hot pink" },
  { RGB_TO_ULONG(255, 105, 180), "HotPink" },
  { RGB_TO_ULONG(255, 20 , 147), "deep pink" },
  { RGB_TO_ULONG(255, 20 , 147), "DeepPink" },
  { RGB_TO_ULONG(255, 192, 203), "pink" },
  { RGB_TO_ULONG(255, 182, 193), "light pink" },
  { RGB_TO_ULONG(255, 182, 193), "LightPink" },
  { RGB_TO_ULONG(219, 112, 147), "pale violet red" },
  { RGB_TO_ULONG(219, 112, 147), "PaleVioletRed" },
  { RGB_TO_ULONG(176, 48 , 96 ), "maroon" },
  { RGB_TO_ULONG(199, 21 , 133), "medium violet red" },
  { RGB_TO_ULONG(199, 21 , 133), "MediumVioletRed" },
  { RGB_TO_ULONG(208, 32 , 144), "violet red" },
  { RGB_TO_ULONG(208, 32 , 144), "VioletRed" },
  { RGB_TO_ULONG(255, 0  , 255), "magenta" },
  { RGB_TO_ULONG(238, 130, 238), "violet" },
  { RGB_TO_ULONG(221, 160, 221), "plum" },
  { RGB_TO_ULONG(218, 112, 214), "orchid" },
  { RGB_TO_ULONG(186, 85 , 211), "medium orchid" },
  { RGB_TO_ULONG(186, 85 , 211), "MediumOrchid" },
  { RGB_TO_ULONG(153, 50 , 204), "dark orchid" },
  { RGB_TO_ULONG(153, 50 , 204), "DarkOrchid" },
  { RGB_TO_ULONG(148, 0  , 211), "dark violet" },
  { RGB_TO_ULONG(148, 0  , 211), "DarkViolet" },
  { RGB_TO_ULONG(138, 43 , 226), "blue violet" },
  { RGB_TO_ULONG(138, 43 , 226), "BlueViolet" },
  { RGB_TO_ULONG(160, 32 , 240), "purple" },
  { RGB_TO_ULONG(147, 112, 219), "medium purple" },
  { RGB_TO_ULONG(147, 112, 219), "MediumPurple" },
  { RGB_TO_ULONG(216, 191, 216), "thistle" },
  { RGB_TO_ULONG(255, 250, 250), "snow1" },
  { RGB_TO_ULONG(238, 233, 233), "snow2" },
  { RGB_TO_ULONG(205, 201, 201), "snow3" },
  { RGB_TO_ULONG(139, 137, 137), "snow4" },
  { RGB_TO_ULONG(255, 245, 238), "seashell1" },
  { RGB_TO_ULONG(238, 229, 222), "seashell2" },
  { RGB_TO_ULONG(205, 197, 191), "seashell3" },
  { RGB_TO_ULONG(139, 134, 130), "seashell4" },
  { RGB_TO_ULONG(255, 239, 219), "AntiqueWhite1" },
  { RGB_TO_ULONG(238, 223, 204), "AntiqueWhite2" },
  { RGB_TO_ULONG(205, 192, 176), "AntiqueWhite3" },
  { RGB_TO_ULONG(139, 131, 120), "AntiqueWhite4" },
  { RGB_TO_ULONG(255, 228, 196), "bisque1" },
  { RGB_TO_ULONG(238, 213, 183), "bisque2" },
  { RGB_TO_ULONG(205, 183, 158), "bisque3" },
  { RGB_TO_ULONG(139, 125, 107), "bisque4" },
  { RGB_TO_ULONG(255, 218, 185), "PeachPuff1" },
  { RGB_TO_ULONG(238, 203, 173), "PeachPuff2" },
  { RGB_TO_ULONG(205, 175, 149), "PeachPuff3" },
  { RGB_TO_ULONG(139, 119, 101), "PeachPuff4" },
  { RGB_TO_ULONG(255, 222, 173), "NavajoWhite1" },
  { RGB_TO_ULONG(238, 207, 161), "NavajoWhite2" },
  { RGB_TO_ULONG(205, 179, 139), "NavajoWhite3" },
  { RGB_TO_ULONG(139, 121, 94), "NavajoWhite4" },
  { RGB_TO_ULONG(255, 250, 205), "LemonChiffon1" },
  { RGB_TO_ULONG(238, 233, 191), "LemonChiffon2" },
  { RGB_TO_ULONG(205, 201, 165), "LemonChiffon3" },
  { RGB_TO_ULONG(139, 137, 112), "LemonChiffon4" },
  { RGB_TO_ULONG(255, 248, 220), "cornsilk1" },
  { RGB_TO_ULONG(238, 232, 205), "cornsilk2" },
  { RGB_TO_ULONG(205, 200, 177), "cornsilk3" },
  { RGB_TO_ULONG(139, 136, 120), "cornsilk4" },
  { RGB_TO_ULONG(255, 255, 240), "ivory1" },
  { RGB_TO_ULONG(238, 238, 224), "ivory2" },
  { RGB_TO_ULONG(205, 205, 193), "ivory3" },
  { RGB_TO_ULONG(139, 139, 131), "ivory4" },
  { RGB_TO_ULONG(240, 255, 240), "honeydew1" },
  { RGB_TO_ULONG(224, 238, 224), "honeydew2" },
  { RGB_TO_ULONG(193, 205, 193), "honeydew3" },
  { RGB_TO_ULONG(131, 139, 131), "honeydew4" },
  { RGB_TO_ULONG(255, 240, 245), "LavenderBlush1" },
  { RGB_TO_ULONG(238, 224, 229), "LavenderBlush2" },
  { RGB_TO_ULONG(205, 193, 197), "LavenderBlush3" },
  { RGB_TO_ULONG(139, 131, 134), "LavenderBlush4" },
  { RGB_TO_ULONG(255, 228, 225), "MistyRose1" },
  { RGB_TO_ULONG(238, 213, 210), "MistyRose2" },
  { RGB_TO_ULONG(205, 183, 181), "MistyRose3" },
  { RGB_TO_ULONG(139, 125, 123), "MistyRose4" },
  { RGB_TO_ULONG(240, 255, 255), "azure1" },
  { RGB_TO_ULONG(224, 238, 238), "azure2" },
  { RGB_TO_ULONG(193, 205, 205), "azure3" },
  { RGB_TO_ULONG(131, 139, 139), "azure4" },
  { RGB_TO_ULONG(131, 111, 255), "SlateBlue1" },
  { RGB_TO_ULONG(122, 103, 238), "SlateBlue2" },
  { RGB_TO_ULONG(105, 89 , 205), "SlateBlue3" },
  { RGB_TO_ULONG(71 , 60 , 139), "SlateBlue4" },
  { RGB_TO_ULONG(72 , 118, 255), "RoyalBlue1" },
  { RGB_TO_ULONG(67 , 110, 238), "RoyalBlue2" },
  { RGB_TO_ULONG(58 , 95 , 205), "RoyalBlue3" },
  { RGB_TO_ULONG(39 , 64 , 139), "RoyalBlue4" },
  { RGB_TO_ULONG(0  , 0  , 255), "blue1" },
  { RGB_TO_ULONG(0  , 0  , 238), "blue2" },
  { RGB_TO_ULONG(0  , 0  , 205), "blue3" },
  { RGB_TO_ULONG(0  , 0  , 139), "blue4" },
  { RGB_TO_ULONG(30 , 144, 255), "DodgerBlue1" },
  { RGB_TO_ULONG(28 , 134, 238), "DodgerBlue2" },
  { RGB_TO_ULONG(24 , 116, 205), "DodgerBlue3" },
  { RGB_TO_ULONG(16 , 78 , 139), "DodgerBlue4" },
  { RGB_TO_ULONG(99 , 184, 255), "SteelBlue1" },
  { RGB_TO_ULONG(92 , 172, 238), "SteelBlue2" },
  { RGB_TO_ULONG(79 , 148, 205), "SteelBlue3" },
  { RGB_TO_ULONG(54 , 100, 139), "SteelBlue4" },
  { RGB_TO_ULONG(0  , 191, 255), "DeepSkyBlue1" },
  { RGB_TO_ULONG(0  , 178, 238), "DeepSkyBlue2" },
  { RGB_TO_ULONG(0  , 154, 205), "DeepSkyBlue3" },
  { RGB_TO_ULONG(0  , 104, 139), "DeepSkyBlue4" },
  { RGB_TO_ULONG(135, 206, 255), "SkyBlue1" },
  { RGB_TO_ULONG(126, 192, 238), "SkyBlue2" },
  { RGB_TO_ULONG(108, 166, 205), "SkyBlue3" },
  { RGB_TO_ULONG(74 , 112, 139), "SkyBlue4" },
  { RGB_TO_ULONG(176, 226, 255), "LightSkyBlue1" },
  { RGB_TO_ULONG(164, 211, 238), "LightSkyBlue2" },
  { RGB_TO_ULONG(141, 182, 205), "LightSkyBlue3" },
  { RGB_TO_ULONG(96 , 123, 139), "LightSkyBlue4" },
  { RGB_TO_ULONG(198, 226, 255), "SlateGray1" },
  { RGB_TO_ULONG(185, 211, 238), "SlateGray2" },
  { RGB_TO_ULONG(159, 182, 205), "SlateGray3" },
  { RGB_TO_ULONG(108, 123, 139), "SlateGray4" },
  { RGB_TO_ULONG(202, 225, 255), "LightSteelBlue1" },
  { RGB_TO_ULONG(188, 210, 238), "LightSteelBlue2" },
  { RGB_TO_ULONG(162, 181, 205), "LightSteelBlue3" },
  { RGB_TO_ULONG(110, 123, 139), "LightSteelBlue4" },
  { RGB_TO_ULONG(191, 239, 255), "LightBlue1" },
  { RGB_TO_ULONG(178, 223, 238), "LightBlue2" },
  { RGB_TO_ULONG(154, 192, 205), "LightBlue3" },
  { RGB_TO_ULONG(104, 131, 139), "LightBlue4" },
  { RGB_TO_ULONG(224, 255, 255), "LightCyan1" },
  { RGB_TO_ULONG(209, 238, 238), "LightCyan2" },
  { RGB_TO_ULONG(180, 205, 205), "LightCyan3" },
  { RGB_TO_ULONG(122, 139, 139), "LightCyan4" },
  { RGB_TO_ULONG(187, 255, 255), "PaleTurquoise1" },
  { RGB_TO_ULONG(174, 238, 238), "PaleTurquoise2" },
  { RGB_TO_ULONG(150, 205, 205), "PaleTurquoise3" },
  { RGB_TO_ULONG(102, 139, 139), "PaleTurquoise4" },
  { RGB_TO_ULONG(152, 245, 255), "CadetBlue1" },
  { RGB_TO_ULONG(142, 229, 238), "CadetBlue2" },
  { RGB_TO_ULONG(122, 197, 205), "CadetBlue3" },
  { RGB_TO_ULONG(83 , 134, 139), "CadetBlue4" },
  { RGB_TO_ULONG(0  , 245, 255), "turquoise1" },
  { RGB_TO_ULONG(0  , 229, 238), "turquoise2" },
  { RGB_TO_ULONG(0  , 197, 205), "turquoise3" },
  { RGB_TO_ULONG(0  , 134, 139), "turquoise4" },
  { RGB_TO_ULONG(0  , 255, 255), "cyan1" },
  { RGB_TO_ULONG(0  , 238, 238), "cyan2" },
  { RGB_TO_ULONG(0  , 205, 205), "cyan3" },
  { RGB_TO_ULONG(0  , 139, 139), "cyan4" },
  { RGB_TO_ULONG(151, 255, 255), "DarkSlateGray1" },
  { RGB_TO_ULONG(141, 238, 238), "DarkSlateGray2" },
  { RGB_TO_ULONG(121, 205, 205), "DarkSlateGray3" },
  { RGB_TO_ULONG(82 , 139, 139), "DarkSlateGray4" },
  { RGB_TO_ULONG(127, 255, 212), "aquamarine1" },
  { RGB_TO_ULONG(118, 238, 198), "aquamarine2" },
  { RGB_TO_ULONG(102, 205, 170), "aquamarine3" },
  { RGB_TO_ULONG(69 , 139, 116), "aquamarine4" },
  { RGB_TO_ULONG(193, 255, 193), "DarkSeaGreen1" },
  { RGB_TO_ULONG(180, 238, 180), "DarkSeaGreen2" },
  { RGB_TO_ULONG(155, 205, 155), "DarkSeaGreen3" },
  { RGB_TO_ULONG(105, 139, 105), "DarkSeaGreen4" },
  { RGB_TO_ULONG(84 , 255, 159), "SeaGreen1" },
  { RGB_TO_ULONG(78 , 238, 148), "SeaGreen2" },   
  { RGB_TO_ULONG(67 , 205, 128), "SeaGreen3" },
  { RGB_TO_ULONG(46 , 139, 87 ), "SeaGreen4" },
  { RGB_TO_ULONG(154, 255, 154), "PaleGreen1" },
  { RGB_TO_ULONG(144, 238, 144), "PaleGreen2" },
  { RGB_TO_ULONG(124, 205, 124), "PaleGreen3" },
  { RGB_TO_ULONG(84 , 139, 84 ), "PaleGreen4" },
  { RGB_TO_ULONG(0  , 255, 127), "SpringGreen1" },
  { RGB_TO_ULONG(0  , 238, 118), "SpringGreen2" },
  { RGB_TO_ULONG(0  , 205, 102), "SpringGreen3" },
  { RGB_TO_ULONG(0  , 139, 69 ), "SpringGreen4" },
  { RGB_TO_ULONG(0  , 255, 0  ), "green1" },
  { RGB_TO_ULONG(0  , 238, 0  ), "green2" },
  { RGB_TO_ULONG(0  , 205, 0  ), "green3" },
  { RGB_TO_ULONG(0  , 139, 0  ), "green4" },
  { RGB_TO_ULONG(127, 255, 0  ), "chartreuse1" },
  { RGB_TO_ULONG(118, 238, 0  ), "chartreuse2" },
  { RGB_TO_ULONG(102, 205, 0  ), "chartreuse3" },
  { RGB_TO_ULONG(69 , 139, 0  ), "chartreuse4" },
  { RGB_TO_ULONG(192, 255, 62 ), "OliveDrab1" },
  { RGB_TO_ULONG(179, 238, 58 ), "OliveDrab2" },
  { RGB_TO_ULONG(154, 205, 50 ), "OliveDrab3" },
  { RGB_TO_ULONG(105, 139, 34 ), "OliveDrab4" },
  { RGB_TO_ULONG(202, 255, 112), "DarkOliveGreen1" },
  { RGB_TO_ULONG(188, 238, 104), "DarkOliveGreen2" },
  { RGB_TO_ULONG(162, 205, 90 ), "DarkOliveGreen3" },
  { RGB_TO_ULONG(110, 139, 61 ), "DarkOliveGreen4" },
  { RGB_TO_ULONG(255, 246, 143), "khaki1" },
  { RGB_TO_ULONG(238, 230, 133), "khaki2" },
  { RGB_TO_ULONG(205, 198, 115), "khaki3" },
  { RGB_TO_ULONG(139, 134, 78 ), "khaki4" },
  { RGB_TO_ULONG(255, 236, 139), "LightGoldenrod1" },
  { RGB_TO_ULONG(238, 220, 130), "LightGoldenrod2" },
  { RGB_TO_ULONG(205, 190, 112), "LightGoldenrod3" },
  { RGB_TO_ULONG(139, 129, 76 ), "LightGoldenrod4" },
  { RGB_TO_ULONG(255, 255, 224), "LightYellow1" },
  { RGB_TO_ULONG(238, 238, 209), "LightYellow2" },
  { RGB_TO_ULONG(205, 205, 180), "LightYellow3" },
  { RGB_TO_ULONG(139, 139, 122), "LightYellow4" },
  { RGB_TO_ULONG(255, 255, 0  ), "yellow1" },
  { RGB_TO_ULONG(238, 238, 0  ), "yellow2" },
  { RGB_TO_ULONG(205, 205, 0  ), "yellow3" },
  { RGB_TO_ULONG(139, 139, 0  ), "yellow4" },
  { RGB_TO_ULONG(255, 215, 0  ), "gold1" },
  { RGB_TO_ULONG(238, 201, 0  ), "gold2" },
  { RGB_TO_ULONG(205, 173, 0  ), "gold3" },
  { RGB_TO_ULONG(139, 117, 0  ), "gold4" },
  { RGB_TO_ULONG(255, 193, 37 ), "goldenrod1" },
  { RGB_TO_ULONG(238, 180, 34 ), "goldenrod2" },
  { RGB_TO_ULONG(205, 155, 29 ), "goldenrod3" },
  { RGB_TO_ULONG(139, 105, 20 ), "goldenrod4" },
  { RGB_TO_ULONG(255, 185, 15 ), "DarkGoldenrod1" },
  { RGB_TO_ULONG(238, 173, 14 ), "DarkGoldenrod2" },
  { RGB_TO_ULONG(205, 149, 12 ), "DarkGoldenrod3" },
  { RGB_TO_ULONG(139, 101, 8  ), "DarkGoldenrod4" },
  { RGB_TO_ULONG(255, 193, 193), "RosyBrown1" },
  { RGB_TO_ULONG(238, 180, 180), "RosyBrown2" },
  { RGB_TO_ULONG(205, 155, 155), "RosyBrown3" },
  { RGB_TO_ULONG(139, 105, 105), "RosyBrown4" },
  { RGB_TO_ULONG(255, 106, 106), "IndianRed1" },
  { RGB_TO_ULONG(238, 99 , 99 ), "IndianRed2" },
  { RGB_TO_ULONG(205, 85 , 85 ), "IndianRed3" },
  { RGB_TO_ULONG(139, 58 , 58 ), "IndianRed4" },
  { RGB_TO_ULONG(255, 130, 71 ), "sienna1" },
  { RGB_TO_ULONG(238, 121, 66 ), "sienna2" },
  { RGB_TO_ULONG(205, 104, 57 ), "sienna3" },
  { RGB_TO_ULONG(139, 71 , 38 ), "sienna4" },
  { RGB_TO_ULONG(255, 211, 155), "burlywood1" },
  { RGB_TO_ULONG(238, 197, 145), "burlywood2" },
  { RGB_TO_ULONG(205, 170, 125), "burlywood3" },
  { RGB_TO_ULONG(139, 115, 85 ), "burlywood4" },
  { RGB_TO_ULONG(255, 231, 186), "wheat1" },
  { RGB_TO_ULONG(238, 216, 174), "wheat2" },
  { RGB_TO_ULONG(205, 186, 150), "wheat3" },
  { RGB_TO_ULONG(139, 126, 102), "wheat4" },
  { RGB_TO_ULONG(255, 165, 79 ), "tan1" },
  { RGB_TO_ULONG(238, 154, 73 ), "tan2" },
  { RGB_TO_ULONG(205, 133, 63 ), "tan3" },
  { RGB_TO_ULONG(139, 90 , 43 ), "tan4" },
  { RGB_TO_ULONG(255, 127, 36 ), "chocolate1" },
  { RGB_TO_ULONG(238, 118, 33 ), "chocolate2" },
  { RGB_TO_ULONG(205, 102, 29 ), "chocolate3" },
  { RGB_TO_ULONG(139, 69 , 19 ), "chocolate4" },
  { RGB_TO_ULONG(255, 48 , 48 ), "firebrick1" },
  { RGB_TO_ULONG(238, 44 , 44 ), "firebrick2" },
  { RGB_TO_ULONG(205, 38 , 38 ), "firebrick3" },
  { RGB_TO_ULONG(139, 26 , 26 ), "firebrick4" },
  { RGB_TO_ULONG(255, 64 , 64 ), "brown1" },
  { RGB_TO_ULONG(238, 59 , 59 ), "brown2" },
  { RGB_TO_ULONG(205, 51 , 51 ), "brown3" },
  { RGB_TO_ULONG(139, 35 , 35 ), "brown4" },
  { RGB_TO_ULONG(255, 140, 105), "salmon1" },
  { RGB_TO_ULONG(238, 130, 98 ), "salmon2" },
  { RGB_TO_ULONG(205, 112, 84 ), "salmon3" },
  { RGB_TO_ULONG(139, 76 , 57 ), "salmon4" },
  { RGB_TO_ULONG(255, 160, 122), "LightSalmon1" },
  { RGB_TO_ULONG(238, 149, 114), "LightSalmon2" },
  { RGB_TO_ULONG(205, 129, 98 ), "LightSalmon3" },
  { RGB_TO_ULONG(139, 87 , 66 ), "LightSalmon4" },
  { RGB_TO_ULONG(255, 165, 0  ), "orange1" },
  { RGB_TO_ULONG(238, 154, 0  ), "orange2" },
  { RGB_TO_ULONG(205, 133, 0  ), "orange3" },
  { RGB_TO_ULONG(139, 90 , 0  ), "orange4" },
  { RGB_TO_ULONG(255, 127, 0  ), "DarkOrange1" },
  { RGB_TO_ULONG(238, 118, 0  ), "DarkOrange2" },
  { RGB_TO_ULONG(205, 102, 0  ), "DarkOrange3" },
  { RGB_TO_ULONG(139, 69 , 0  ), "DarkOrange4" },
  { RGB_TO_ULONG(255, 114, 86 ), "coral1" },
  { RGB_TO_ULONG(238, 106, 80 ), "coral2" },
  { RGB_TO_ULONG(205, 91 , 69 ), "coral3" },
  { RGB_TO_ULONG(139, 62 , 47 ), "coral4" },
  { RGB_TO_ULONG(255, 99 , 71 ), "tomato1" },
  { RGB_TO_ULONG(238, 92 , 66 ), "tomato2" },
  { RGB_TO_ULONG(205, 79 , 57 ), "tomato3" },
  { RGB_TO_ULONG(139, 54 , 38 ), "tomato4" },
  { RGB_TO_ULONG(255, 69 , 0  ), "OrangeRed1" },
  { RGB_TO_ULONG(238, 64 , 0  ), "OrangeRed2" },
  { RGB_TO_ULONG(205, 55 , 0  ), "OrangeRed3" },
  { RGB_TO_ULONG(139, 37 , 0  ), "OrangeRed4" },
  { RGB_TO_ULONG(255, 0  , 0  ), "red1" },
  { RGB_TO_ULONG(238, 0  , 0  ), "red2" },
  { RGB_TO_ULONG(205, 0  , 0  ), "red3" },
  { RGB_TO_ULONG(139, 0  , 0  ), "red4" },
  { RGB_TO_ULONG(255, 20 , 147), "DeepPink1" },
  { RGB_TO_ULONG(238, 18 , 137), "DeepPink2" },
  { RGB_TO_ULONG(205, 16 , 118), "DeepPink3" },
  { RGB_TO_ULONG(139, 10 , 80 ), "DeepPink4" },
  { RGB_TO_ULONG(255, 110, 180), "HotPink1" },
  { RGB_TO_ULONG(238, 106, 167), "HotPink2" },
  { RGB_TO_ULONG(205, 96 , 144), "HotPink3" },
  { RGB_TO_ULONG(139, 58 , 98 ), "HotPink4" },
  { RGB_TO_ULONG(255, 181, 197), "pink1" },
  { RGB_TO_ULONG(238, 169, 184), "pink2" },
  { RGB_TO_ULONG(205, 145, 158), "pink3" },
  { RGB_TO_ULONG(139, 99 , 108), "pink4" },
  { RGB_TO_ULONG(255, 174, 185), "LightPink1" },
  { RGB_TO_ULONG(238, 162, 173), "LightPink2" },
  { RGB_TO_ULONG(205, 140, 149), "LightPink3" },
  { RGB_TO_ULONG(139, 95 , 101), "LightPink4" },
  { RGB_TO_ULONG(255, 130, 171), "PaleVioletRed1" },
  { RGB_TO_ULONG(238, 121, 159), "PaleVioletRed2" },
  { RGB_TO_ULONG(205, 104, 137), "PaleVioletRed3" },
  { RGB_TO_ULONG(139, 71 , 93 ), "PaleVioletRed4" },
  { RGB_TO_ULONG(255, 52 , 179), "maroon1" },
  { RGB_TO_ULONG(238, 48 , 167), "maroon2" },
  { RGB_TO_ULONG(205, 41 , 144), "maroon3" },
  { RGB_TO_ULONG(139, 28 , 98 ), "maroon4" },
  { RGB_TO_ULONG(255, 62 , 150), "VioletRed1" },
  { RGB_TO_ULONG(238, 58 , 140), "VioletRed2" },
  { RGB_TO_ULONG(205, 50 , 120), "VioletRed3" },
  { RGB_TO_ULONG(139, 34 , 82 ), "VioletRed4" },
  { RGB_TO_ULONG(255, 0  , 255), "magenta1" },
  { RGB_TO_ULONG(238, 0  , 238), "magenta2" },
  { RGB_TO_ULONG(205, 0  , 205), "magenta3" },
  { RGB_TO_ULONG(139, 0  , 139), "magenta4" },
  { RGB_TO_ULONG(255, 131, 250), "orchid1" },
  { RGB_TO_ULONG(238, 122, 233), "orchid2" },
  { RGB_TO_ULONG(205, 105, 201), "orchid3" },
  { RGB_TO_ULONG(139, 71 , 137), "orchid4" },
  { RGB_TO_ULONG(255, 187, 255), "plum1" },
  { RGB_TO_ULONG(238, 174, 238), "plum2" },
  { RGB_TO_ULONG(205, 150, 205), "plum3" },
  { RGB_TO_ULONG(139, 102, 139), "plum4" },
  { RGB_TO_ULONG(224, 102, 255), "MediumOrchid1" },
  { RGB_TO_ULONG(209, 95 , 238), "MediumOrchid2" },
  { RGB_TO_ULONG(180, 82 , 205), "MediumOrchid3" },
  { RGB_TO_ULONG(122, 55 , 139), "MediumOrchid4" },
  { RGB_TO_ULONG(191, 62 , 255), "DarkOrchid1" },
  { RGB_TO_ULONG(178, 58 , 238), "DarkOrchid2" },
  { RGB_TO_ULONG(154, 50 , 205), "DarkOrchid3" },
  { RGB_TO_ULONG(104, 34 , 139), "DarkOrchid4" },
  { RGB_TO_ULONG(155, 48 , 255), "purple1" },
  { RGB_TO_ULONG(145, 44 , 238), "purple2" },
  { RGB_TO_ULONG(125, 38 , 205), "purple3" },
  { RGB_TO_ULONG(85 , 26 , 139), "purple4" },
  { RGB_TO_ULONG(171, 130, 255), "MediumPurple1" },
  { RGB_TO_ULONG(159, 121, 238), "MediumPurple2" },
  { RGB_TO_ULONG(137, 104, 205), "MediumPurple3" },
  { RGB_TO_ULONG(93 , 71 , 139), "MediumPurple4" },
  { RGB_TO_ULONG(255, 225, 255), "thistle1" },
  { RGB_TO_ULONG(238, 210, 238), "thistle2" },
  { RGB_TO_ULONG(205, 181, 205), "thistle3" },
  { RGB_TO_ULONG(139, 123, 139), "thistle4" },
  { RGB_TO_ULONG(0  , 0  , 0  ), "gray0" },
  { RGB_TO_ULONG(0  , 0  , 0  ), "grey0" },
  { RGB_TO_ULONG(3  , 3  , 3  ), "gray1" },
  { RGB_TO_ULONG(3  , 3  , 3  ), "grey1" },
  { RGB_TO_ULONG(5  , 5  , 5  ), "gray2" },
  { RGB_TO_ULONG(5  , 5  , 5  ), "grey2" },
  { RGB_TO_ULONG(8  , 8  , 8  ), "gray3" },
  { RGB_TO_ULONG(8  , 8  , 8  ), "grey3" },
  { RGB_TO_ULONG(10 , 10 , 10 ), "gray4" },
  { RGB_TO_ULONG(10 , 10 , 10 ), "grey4" },
  { RGB_TO_ULONG(13 , 13 , 13 ), "gray5" },
  { RGB_TO_ULONG(13 , 13 , 13 ), "grey5" },
  { RGB_TO_ULONG(15 , 15 , 15 ), "gray6" },
  { RGB_TO_ULONG(15 , 15 , 15 ), "grey6" },
  { RGB_TO_ULONG(18 , 18 , 18 ), "gray7" },
  { RGB_TO_ULONG(18 , 18 , 18 ), "grey7" },
  { RGB_TO_ULONG(20 , 20 , 20 ), "gray8" },
  { RGB_TO_ULONG(20 , 20 , 20 ), "grey8" },
  { RGB_TO_ULONG(23 , 23 , 23 ), "gray9" },
  { RGB_TO_ULONG(23 , 23 , 23 ), "grey9" },
  { RGB_TO_ULONG(26 , 26 , 26 ), "gray10" },
  { RGB_TO_ULONG(26 , 26 , 26 ), "grey10" },
  { RGB_TO_ULONG(28 , 28 , 28 ), "gray11" },
  { RGB_TO_ULONG(28 , 28 , 28 ), "grey11" },
  { RGB_TO_ULONG(31 , 31 , 31 ), "gray12" },
  { RGB_TO_ULONG(31 , 31 , 31 ), "grey12" },
  { RGB_TO_ULONG(33 , 33 , 33 ), "gray13" },
  { RGB_TO_ULONG(33 , 33 , 33 ), "grey13" },
  { RGB_TO_ULONG(36 , 36 , 36 ), "gray14" },
  { RGB_TO_ULONG(36 , 36 , 36 ), "grey14" },
  { RGB_TO_ULONG(38 , 38 , 38 ), "gray15" },
  { RGB_TO_ULONG(38 , 38 , 38 ), "grey15" },
  { RGB_TO_ULONG(41 , 41 , 41 ), "gray16" },
  { RGB_TO_ULONG(41 , 41 , 41 ), "grey16" },
  { RGB_TO_ULONG(43 , 43 , 43 ), "gray17" },
  { RGB_TO_ULONG(43 , 43 , 43 ), "grey17" },
  { RGB_TO_ULONG(46 , 46 , 46 ), "gray18" },
  { RGB_TO_ULONG(46 , 46 , 46 ), "grey18" },
  { RGB_TO_ULONG(48 , 48 , 48 ), "gray19" },
  { RGB_TO_ULONG(48 , 48 , 48 ), "grey19" },
  { RGB_TO_ULONG(51 , 51 , 51 ), "gray20" },
  { RGB_TO_ULONG(51 , 51 , 51 ), "grey20" },
  { RGB_TO_ULONG(54 , 54 , 54 ), "gray21" },
  { RGB_TO_ULONG(54 , 54 , 54 ), "grey21" },
  { RGB_TO_ULONG(56 , 56 , 56 ), "gray22" },
  { RGB_TO_ULONG(56 , 56 , 56 ), "grey22" },
  { RGB_TO_ULONG(59 , 59 , 59 ), "gray23" },
  { RGB_TO_ULONG(59 , 59 , 59 ), "grey23" },
  { RGB_TO_ULONG(61 , 61 , 61 ), "gray24" },
  { RGB_TO_ULONG(61 , 61 , 61 ), "grey24" },
  { RGB_TO_ULONG(64 , 64 , 64 ), "gray25" },
  { RGB_TO_ULONG(64 , 64 , 64 ), "grey25" },
  { RGB_TO_ULONG(66 , 66 , 66 ), "gray26" },
  { RGB_TO_ULONG(66 , 66 , 66 ), "grey26" },
  { RGB_TO_ULONG(69 , 69 , 69 ), "gray27" },
  { RGB_TO_ULONG(69 , 69 , 69 ), "grey27" },
  { RGB_TO_ULONG(71 , 71 , 71 ), "gray28" },
  { RGB_TO_ULONG(71 , 71 , 71 ), "grey28" },
  { RGB_TO_ULONG(74 , 74 , 74 ), "gray29" },
  { RGB_TO_ULONG(74 , 74 , 74 ), "grey29" },
  { RGB_TO_ULONG(77 , 77 , 77 ), "gray30" },
  { RGB_TO_ULONG(77 , 77 , 77 ), "grey30" },
  { RGB_TO_ULONG(79 , 79 , 79 ), "gray31" },
  { RGB_TO_ULONG(79 , 79 , 79 ), "grey31" },
  { RGB_TO_ULONG(82 , 82 , 82 ), "gray32" },
  { RGB_TO_ULONG(82 , 82 , 82 ), "grey32" },
  { RGB_TO_ULONG(84 , 84 , 84 ), "gray33" },
  { RGB_TO_ULONG(84 , 84 , 84 ), "grey33" },
  { RGB_TO_ULONG(87 , 87 , 87 ), "gray34" },
  { RGB_TO_ULONG(87 , 87 , 87 ), "grey34" },
  { RGB_TO_ULONG(89 , 89 , 89 ), "gray35" },
  { RGB_TO_ULONG(89 , 89 , 89 ), "grey35" },
  { RGB_TO_ULONG(92 , 92 , 92 ), "gray36" },
  { RGB_TO_ULONG(92 , 92 , 92 ), "grey36" },
  { RGB_TO_ULONG(94 , 94 , 94 ), "gray37" },
  { RGB_TO_ULONG(94 , 94 , 94 ), "grey37" },
  { RGB_TO_ULONG(97 , 97 , 97 ), "gray38" },
  { RGB_TO_ULONG(97 , 97 , 97 ), "grey38" },
  { RGB_TO_ULONG(99 , 99 , 99 ), "gray39" },
  { RGB_TO_ULONG(99 , 99 , 99 ), "grey39" },
  { RGB_TO_ULONG(102, 102, 102), "gray40" },
  { RGB_TO_ULONG(102, 102, 102), "grey40" },
  { RGB_TO_ULONG(105, 105, 105), "gray41" },
  { RGB_TO_ULONG(105, 105, 105), "grey41" },
  { RGB_TO_ULONG(107, 107, 107), "gray42" },
  { RGB_TO_ULONG(107, 107, 107), "grey42" },
  { RGB_TO_ULONG(110, 110, 110), "gray43" },
  { RGB_TO_ULONG(110, 110, 110), "grey43" },
  { RGB_TO_ULONG(112, 112, 112), "gray44" },
  { RGB_TO_ULONG(112, 112, 112), "grey44" },
  { RGB_TO_ULONG(115, 115, 115), "gray45" },
  { RGB_TO_ULONG(115, 115, 115), "grey45" },
  { RGB_TO_ULONG(117, 117, 117), "gray46" },
  { RGB_TO_ULONG(117, 117, 117), "grey46" },
  { RGB_TO_ULONG(120, 120, 120), "gray47" },
  { RGB_TO_ULONG(120, 120, 120), "grey47" },
  { RGB_TO_ULONG(122, 122, 122), "gray48" },
  { RGB_TO_ULONG(122, 122, 122), "grey48" },
  { RGB_TO_ULONG(125, 125, 125), "gray49" },
  { RGB_TO_ULONG(125, 125, 125), "grey49" },
  { RGB_TO_ULONG(127, 127, 127), "gray50" },
  { RGB_TO_ULONG(127, 127, 127), "grey50" },
  { RGB_TO_ULONG(130, 130, 130), "gray51" },
  { RGB_TO_ULONG(130, 130, 130), "grey51" },
  { RGB_TO_ULONG(133, 133, 133), "gray52" },
  { RGB_TO_ULONG(133, 133, 133), "grey52" },
  { RGB_TO_ULONG(135, 135, 135), "gray53" },
  { RGB_TO_ULONG(135, 135, 135), "grey53" },
  { RGB_TO_ULONG(138, 138, 138), "gray54" },
  { RGB_TO_ULONG(138, 138, 138), "grey54" },
  { RGB_TO_ULONG(140, 140, 140), "gray55" },
  { RGB_TO_ULONG(140, 140, 140), "grey55" },
  { RGB_TO_ULONG(143, 143, 143), "gray56" },
  { RGB_TO_ULONG(143, 143, 143), "grey56" },
  { RGB_TO_ULONG(145, 145, 145), "gray57" },
  { RGB_TO_ULONG(145, 145, 145), "grey57" },
  { RGB_TO_ULONG(148, 148, 148), "gray58" },
  { RGB_TO_ULONG(148, 148, 148), "grey58" },
  { RGB_TO_ULONG(150, 150, 150), "gray59" },
  { RGB_TO_ULONG(150, 150, 150), "grey59" },
  { RGB_TO_ULONG(153, 153, 153), "gray60" },
  { RGB_TO_ULONG(153, 153, 153), "grey60" },
  { RGB_TO_ULONG(156, 156, 156), "gray61" },
  { RGB_TO_ULONG(156, 156, 156), "grey61" },
  { RGB_TO_ULONG(158, 158, 158), "gray62" },
  { RGB_TO_ULONG(158, 158, 158), "grey62" },
  { RGB_TO_ULONG(161, 161, 161), "gray63" },
  { RGB_TO_ULONG(161, 161, 161), "grey63" },
  { RGB_TO_ULONG(163, 163, 163), "gray64" },
  { RGB_TO_ULONG(163, 163, 163), "grey64" },
  { RGB_TO_ULONG(166, 166, 166), "gray65" },
  { RGB_TO_ULONG(166, 166, 166), "grey65" },
  { RGB_TO_ULONG(168, 168, 168), "gray66" },
  { RGB_TO_ULONG(168, 168, 168), "grey66" },
  { RGB_TO_ULONG(171, 171, 171), "gray67" },
  { RGB_TO_ULONG(171, 171, 171), "grey67" },
  { RGB_TO_ULONG(173, 173, 173), "gray68" },
  { RGB_TO_ULONG(173, 173, 173), "grey68" },
  { RGB_TO_ULONG(176, 176, 176), "gray69" },
  { RGB_TO_ULONG(176, 176, 176), "grey69" },
  { RGB_TO_ULONG(179, 179, 179), "gray70" },
  { RGB_TO_ULONG(179, 179, 179), "grey70" },
  { RGB_TO_ULONG(181, 181, 181), "gray71" },
  { RGB_TO_ULONG(181, 181, 181), "grey71" },
  { RGB_TO_ULONG(184, 184, 184), "gray72" },
  { RGB_TO_ULONG(184, 184, 184), "grey72" },
  { RGB_TO_ULONG(186, 186, 186), "gray73" },
  { RGB_TO_ULONG(186, 186, 186), "grey73" },
  { RGB_TO_ULONG(189, 189, 189), "gray74" },
  { RGB_TO_ULONG(189, 189, 189), "grey74" },
  { RGB_TO_ULONG(191, 191, 191), "gray75" },
  { RGB_TO_ULONG(191, 191, 191), "grey75" },
  { RGB_TO_ULONG(194, 194, 194), "gray76" },
  { RGB_TO_ULONG(194, 194, 194), "grey76" },
  { RGB_TO_ULONG(196, 196, 196), "gray77" },
  { RGB_TO_ULONG(196, 196, 196), "grey77" },
  { RGB_TO_ULONG(199, 199, 199), "gray78" },
  { RGB_TO_ULONG(199, 199, 199), "grey78" },
  { RGB_TO_ULONG(201, 201, 201), "gray79" },
  { RGB_TO_ULONG(201, 201, 201), "grey79" },
  { RGB_TO_ULONG(204, 204, 204), "gray80" },
  { RGB_TO_ULONG(204, 204, 204), "grey80" },
  { RGB_TO_ULONG(207, 207, 207), "gray81" },
  { RGB_TO_ULONG(207, 207, 207), "grey81" },
  { RGB_TO_ULONG(209, 209, 209), "gray82" },
  { RGB_TO_ULONG(209, 209, 209), "grey82" },
  { RGB_TO_ULONG(212, 212, 212), "gray83" },
  { RGB_TO_ULONG(212, 212, 212), "grey83" },
  { RGB_TO_ULONG(214, 214, 214), "gray84" },
  { RGB_TO_ULONG(214, 214, 214), "grey84" },
  { RGB_TO_ULONG(217, 217, 217), "gray85" },
  { RGB_TO_ULONG(217, 217, 217), "grey85" },
  { RGB_TO_ULONG(219, 219, 219), "gray86" },
  { RGB_TO_ULONG(219, 219, 219), "grey86" },
  { RGB_TO_ULONG(222, 222, 222), "gray87" },
  { RGB_TO_ULONG(222, 222, 222), "grey87" },
  { RGB_TO_ULONG(224, 224, 224), "gray88" },
  { RGB_TO_ULONG(224, 224, 224), "grey88" },
  { RGB_TO_ULONG(227, 227, 227), "gray89" },
  { RGB_TO_ULONG(227, 227, 227), "grey89" },
  { RGB_TO_ULONG(229, 229, 229), "gray90" },
  { RGB_TO_ULONG(229, 229, 229), "grey90" },
  { RGB_TO_ULONG(232, 232, 232), "gray91" },
  { RGB_TO_ULONG(232, 232, 232), "grey91" },
  { RGB_TO_ULONG(235, 235, 235), "gray92" },
  { RGB_TO_ULONG(235, 235, 235), "grey92" },
  { RGB_TO_ULONG(237, 237, 237), "gray93" },
  { RGB_TO_ULONG(237, 237, 237), "grey93" },
  { RGB_TO_ULONG(240, 240, 240), "gray94" },
  { RGB_TO_ULONG(240, 240, 240), "grey94" },
  { RGB_TO_ULONG(242, 242, 242), "gray95" },
  { RGB_TO_ULONG(242, 242, 242), "grey95" },
  { RGB_TO_ULONG(245, 245, 245), "gray96" },
  { RGB_TO_ULONG(245, 245, 245), "grey96" },
  { RGB_TO_ULONG(247, 247, 247), "gray97" },
  { RGB_TO_ULONG(247, 247, 247), "grey97" },
  { RGB_TO_ULONG(250, 250, 250), "gray98" },
  { RGB_TO_ULONG(250, 250, 250), "grey98" },
  { RGB_TO_ULONG(252, 252, 252), "gray99" },
  { RGB_TO_ULONG(252, 252, 252), "grey99" },
  { RGB_TO_ULONG(255, 255, 255), "gray100" },
  { RGB_TO_ULONG(255, 255, 255), "grey100" },
  { RGB_TO_ULONG(169, 169, 169), "dark grey" },
  { RGB_TO_ULONG(169, 169, 169), "DarkGrey" },
  { RGB_TO_ULONG(169, 169, 169), "dark gray" },
  { RGB_TO_ULONG(169, 169, 169), "DarkGray" },
  { RGB_TO_ULONG(0  , 0  , 139), "dark blue" },
  { RGB_TO_ULONG(0  , 0  , 139), "DarkBlue" },
  { RGB_TO_ULONG(0  , 139, 139), "dark cyan" },
  { RGB_TO_ULONG(0  , 139, 139), "DarkCyan" },
  { RGB_TO_ULONG(139, 0  , 139), "dark magenta" },
  { RGB_TO_ULONG(139, 0  , 139), "DarkMagenta" },
  { RGB_TO_ULONG(139, 0  , 0  ), "dark red" },
  { RGB_TO_ULONG(139, 0  , 0  ), "DarkRed" },
  { RGB_TO_ULONG(144, 238, 144), "light green" },
  { RGB_TO_ULONG(144, 238, 144), "LightGreen" }
};

unsigned long
mac_color_map_lookup (colorname)
     char *colorname;
{
  Lisp_Object ret = Qnil;
  int i;

  BLOCK_INPUT;
  
  for (i = 0; i < sizeof (mac_color_map) / sizeof (mac_color_map[0]); i++)
    if (stricmp (colorname, mac_color_map[i].name) == 0)
      {
        ret = mac_color_map[i].color;
        break;
      }

  UNBLOCK_INPUT;

  return ret;
}

Lisp_Object 
x_to_mac_color (colorname)
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
	  unsigned long colorval;
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
      unsigned long colorval;
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
      unsigned long colorval;
      int i, pos;
      pos = 0;

      colorval = 0;
      color = colorname + 5;
      for (i = 0; i < 3; i++)
	{
	  char *end;
	  double value;
	  unsigned long val;

	  value = strtod(color, &end);
	  if (errno == ERANGE)
	    break;
	  if (value < 0.0 || value > 1.0)
	    break;
	  val = (unsigned long)(0x100 * value);
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

  ret = mac_color_map_lookup (colorname);
  
  UNBLOCK_INPUT;
  return ret;
}

/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (f, color)
     struct frame *f;
     unsigned long *color;
{
  if (f->gamma)
    {
      unsigned long red, green, blue;

      red = pow (RED_FROM_ULONG (*color) / 255.0, f->gamma) * 255.0 + 0.5;
      green = pow (GREEN_FROM_ULONG (*color) / 255.0, f->gamma) * 255.0 + 0.5;
      blue = pow (BLUE_FROM_ULONG (*color) / 255.0, f->gamma) * 255.0 + 0.5;
      *color = RGB_TO_ULONG (red, green, blue);
    }
}

/* Decide if color named COLOR is valid for the display associated
   with the selected frame; if so, return the rgb values in COLOR_DEF.
   If ALLOC is nonzero, allocate a new colormap cell.  */

int
mac_defined_color (f, color, color_def, alloc)
     FRAME_PTR f;
     char *color;
     XColor *color_def;
     int alloc;
{
  register Lisp_Object tem;
  unsigned long mac_color_ref;

  tem = x_to_mac_color (color);

  if (!NILP (tem)) 
    {
      if (f)
        {
          /* Apply gamma correction.  */
          mac_color_ref = XUINT (tem);
          gamma_correct (f, &mac_color_ref);
          XSETINT (tem, mac_color_ref);
        }

      color_def->pixel = mac_color_ref;
      color_def->red = RED_FROM_ULONG (mac_color_ref);
      color_def->green = GREEN_FROM_ULONG (mac_color_ref);
      color_def->blue = BLUE_FROM_ULONG (mac_color_ref);

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

#if 0
  if ((FRAME_MAC_DISPLAY_INFO (f)->n_planes
       * FRAME_MAC_DISPLAY_INFO (f)->n_cbits) == 1)
    return def;
#endif

  if (mac_defined_color (f, XSTRING (arg)->data, &cdef, 1))
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
    Fsignal (Qerror, Fcons (build_string ("Illegal line-spacing"),
			    Fcons (new_value, Qnil)));
  if (FRAME_VISIBLE_P (f))
    redraw_frame (f);
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

   If FRAME_MAC_WINDOW (f) is 0,
   the frame is being created and its window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

void
x_set_foreground_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  FRAME_FOREGROUND_PIXEL (f)
    = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  if (FRAME_MAC_WINDOW (f) != 0)
    {
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

  if (FRAME_MAC_WINDOW (f) != 0)
    {
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
    f->output_data.mac->mouse_pixel
      = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  mask_color = FRAME_BACKGROUND_PIXEL (f);

  /* Don't let pointers be invisible.  */
  if (mask_color == f->output_data.mac->mouse_pixel
	&& mask_color == FRAME_BACKGROUND_PIXEL (f))
    f->output_data.mac->mouse_pixel = FRAME_FOREGROUND_PIXEL (f);

#if 0 /* MAC_TODO : cursor changes */
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
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_sb_h_double_arrow);

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
#endif /* MAC_TODO */
}

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel;

  if (!NILP (Vx_cursor_fore_pixel))
    fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				 WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);
  f->output_data.mac->cursor_pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  
  /* Make sure that the cursor color differs from the background color.  */
  if (f->output_data.mac->cursor_pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      f->output_data.mac->cursor_pixel = f->output_data.mac->mouse_pixel;
      if (f->output_data.mac->cursor_pixel == fore_pixel)
	fore_pixel = FRAME_BACKGROUND_PIXEL (f);
    }
  FRAME_FOREGROUND_PIXEL (f) = fore_pixel;

#if 0 /* MAC_TODO: cannot figure out what to do (wrong number of params) */
  if (FRAME_MAC_WINDOW (f) != 0)
    {
      if (FRAME_VISIBLE_P (f))
	{
	  x_display_cursor (f, 0);
	  x_display_cursor (f, 1);
	}
    }
#endif

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
  f->output_data.mac->border_pixel = pix;

  if (FRAME_MAC_WINDOW (f) != 0 && f->output_data.mac->border_width > 0)
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
  f->output_data.mac->cursor_width = width;

  /* Make sure the cursor gets redrawn.  This is overkill, but how
     often do people change cursor types?  */
  update_mode_lines++;
}

#if 0 /* MAC_TODO: really no icon for Mac */
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
#endif /* MAC_TODO */

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

#if 0 /* MAC_TODO */
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
#endif /* MAC_TODO */
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

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg);

  if (XINT (arg) == f->output_data.mac->border_width)
    return;

#if 0 /* MAC_TODO */
  if (FRAME_MAC_WINDOW (f) != 0)
    error ("Cannot change the border width of a window");
#endif

  f->output_data.mac->border_width = XINT (arg);
}

void
x_set_internal_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int old = f->output_data.mac->internal_border_width;

  CHECK_NUMBER (arg);
  f->output_data.mac->internal_border_width = XINT (arg);
  if (f->output_data.mac->internal_border_width < 0)
    f->output_data.mac->internal_border_width = 0;

  if (f->output_data.mac->internal_border_width == old)
    return;

  if (FRAME_MAC_WINDOW (f) != 0)
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
  if (FRAME_MAC_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
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
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		    0, y, width, height, 0);
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
      if (!strcmp (FRAME_MAC_DISPLAY_INFO (f)->mac_id_name,
		   XSTRING (f->name)->data))
	return;
      name = build_string (FRAME_MAC_DISPLAY_INFO (f)->mac_id_name);
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

  if (FRAME_MAC_WINDOW (f))
    {
      if (STRING_MULTIBYTE (name))
#if 0 /* MAC_TODO: encoding title string */
	name = ENCODE_SYSTEM (name);
#else
        return;
#endif

      BLOCK_INPUT;
      
      {
	Str255 windowTitle;
	if (strlen (XSTRING (name)->data) < 255)
	  {
	    strcpy (windowTitle, XSTRING (name)->data);
	    c2pstr (windowTitle);
	    SetWTitle (FRAME_MAC_WINDOW (f), windowTitle);
	  }
      }

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

  if (FRAME_MAC_WINDOW (f))
    {
      if (STRING_MULTIBYTE (name))
#if 0 /* MAC_TODO: encoding title string */
	name = ENCODE_SYSTEM (name);
#else
        return;
#endif

      BLOCK_INPUT;

      {
	Str255 windowTitle;
	if (strlen (XSTRING (name)->data) < 255)
	  {
	    strcpy (windowTitle, XSTRING (name)->data);
	    c2pstr (windowTitle);
	    SetWTitle (FRAME_MAC_WINDOW (f), windowTitle);
	  }
      }

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

      /* We set this parameter before creating the window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_MAC_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
}

void
x_set_scroll_bar_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  /* Imitate X without X Toolkit */

  int wid = FONT_WIDTH (f->output_data.mac->font);

  if (NILP (arg))
    {
#ifdef MAC_OSX
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = 16;  /* Aqua scroll bars.  */
      FRAME_SCROLL_BAR_COLS (f) = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) +
                                   wid - 1) / wid;
#else /* not MAC_OSX */
      /* Make the actual width at least 14 pixels and a multiple of a
	 character width.  */
      FRAME_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
      
      /* Use all of that space (aside from required margins) for the
	 scroll bar.  */
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = 0;
#endif /* not MAC_OSX */
      if (FRAME_MAC_WINDOW (f))
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
      if (FRAME_MAC_WINDOW (f))
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


#if 0 /* MAC_TODO: implement resource strings */
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
#endif /* MAC_TODO */

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
mac_get_arg (alist, param, attribute, class, type)
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

#if 0 /* MAC_TODO: search resource also */
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
#endif /* MAC_TODO */
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

  tem = mac_get_arg (alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}

/* XParseGeometry copied from w32xfns.c */

/*
 *   XParseGeometry parses strings of the form
 *   "=<width>x<height>{+-}<xoffset>{+-}<yoffset>", where
 *   width, height, xoffset, and yoffset are unsigned integers.
 *   Example:  "=80x24+300-49"
 *   The equal sign is optional.
 *   It returns a bitmask that indicates which of the four values
 *   were actually found in the string.  For each value found,
 *   the corresponding argument is updated;  for each value
 *   not found, the corresponding argument is left unchanged. 
 */

static int
read_integer (string, NextString)
     register char *string;
     char **NextString;
{
  register int Result = 0;
  int Sign = 1;
  
  if (*string == '+')
    string++;
  else if (*string == '-')
    {
      string++;
      Sign = -1;
    }
  for (; (*string >= '0') && (*string <= '9'); string++)
    {
      Result = (Result * 10) + (*string - '0');
    }
  *NextString = string;
  if (Sign >= 0)
    return (Result);
  else
    return (-Result);
}

int 
XParseGeometry (string, x, y, width, height)
     char *string;
     int *x, *y;
     unsigned int *width, *height;    /* RETURN */
{
  int mask = NoValue;
  register char *strind;
  unsigned int tempWidth, tempHeight;
  int tempX, tempY;
  char *nextCharacter;
  
  if ((string == NULL) || (*string == '\0')) return (mask);
  if (*string == '=')
    string++;  /* ignore possible '=' at beg of geometry spec */
  
  strind = (char *)string;
  if (*strind != '+' && *strind != '-' && *strind != 'x') 
    {
      tempWidth = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter) 
	return (0);
      strind = nextCharacter;
      mask |= WidthValue;
    }
  
  if (*strind == 'x' || *strind == 'X') 
    {	
      strind++;
      tempHeight = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter)
	return (0);
      strind = nextCharacter;
      mask |= HeightValue;
    }
  
  if ((*strind == '+') || (*strind == '-')) 
    {
      if (*strind == '-') 
	{
	  strind++;
	  tempX = -read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	  mask |= XNegative;

	}
      else
	{	
	  strind++;
	  tempX = read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	}
      mask |= XValue;
      if ((*strind == '+') || (*strind == '-')) 
	{
	  if (*strind == '-') 
	    {
	      strind++;
	      tempY = -read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	      mask |= YNegative;

	    }
	  else
	    {
	      strind++;
	      tempY = read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	    }
	  mask |= YValue;
	}
    }
  
  /* If strind isn't at the end of the string the it's an invalid
     geometry specification. */
  
  if (*strind != '\0') return (0);
  
  if (mask & XValue)
    *x = tempX;
  if (mask & YValue)
    *y = tempY;
  if (mask & WidthValue)
    *width = tempWidth;
  if (mask & HeightValue)
    *height = tempHeight;
  return (mask);
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
  f->output_data.mac->top_pos = 0;
  f->output_data.mac->left_pos = 0;

  tem0 = mac_get_arg (parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = mac_get_arg (parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = mac_get_arg (parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
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

  f->output_data.mac->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.mac->font)));

  x_compute_fringe_widths (f, 0);

  f->output_data.mac->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  f->output_data.mac->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = mac_get_arg (parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = mac_get_arg (parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = mac_get_arg (parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->output_data.mac->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qminus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.mac->top_pos = - XINT (XCAR (XCDR (tem0)));
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qplus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.mac->top_pos = XINT (XCAR (XCDR (tem0)));
	}
      else if (EQ (tem0, Qunbound))
	f->output_data.mac->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0);
	  f->output_data.mac->top_pos = XINT (tem0);
	  if (f->output_data.mac->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->output_data.mac->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qminus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.mac->left_pos = - XINT (XCAR (XCDR (tem1)));
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qplus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.mac->left_pos = XINT (XCAR (XCDR (tem1)));
	}
      else if (EQ (tem1, Qunbound))
	f->output_data.mac->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1);
	  f->output_data.mac->left_pos = XINT (tem1);
	  if (f->output_data.mac->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  return window_prompting;
}


#if 0 /* MAC_TODO */
/* Create and set up the Mac window for frame F.  */

static void
mac_window (f, window_prompting, minibuffer_only)
     struct frame *f;
     long window_prompting;
     int minibuffer_only;
{
  Rect r;

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

  SetRect (&r, f->output_data.mac->left_pos, f->output_data.mac->top_pos,
           f->output_data.mac->left_pos + PIXEL_WIDTH (f),
           f->output_data.mac->top_pos + PIXEL_HEIGHT (f));
  FRAME_MAC_WINDOW (f)
    = NewCWindow (NULL, &r, "\p", 1, zoomDocProc, (WindowPtr) -1, 1, (long) f->output_data.mac);

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

  ShowWindow (FRAME_MAC_WINDOW (f));

  UNBLOCK_INPUT;

  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    initialize_frame_menubar (f);

  if (FRAME_MAC_WINDOW (f) == 0)
    error ("Unable to create window");
}
#endif /* MAC_TODO */

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
  icon_x = mac_get_arg (parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = mac_get_arg (parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
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


void
x_make_gc (f)
     struct frame *f;
{
  XGCValues gc_values;

  BLOCK_INPUT;

  /* Create the GC's of this frame.
     Note that many default values are used.  */

  /* Normal video */
  gc_values.font = f->output_data.mac->font;
  gc_values.foreground = FRAME_FOREGROUND_PIXEL (f);
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  f->output_data.mac->normal_gc = XCreateGC (FRAME_MAC_DISPLAY (f),
				             FRAME_MAC_WINDOW (f),
				             GCFont | GCForeground | GCBackground,
				             &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = FRAME_FOREGROUND_PIXEL (f);
  f->output_data.mac->reverse_gc = XCreateGC (FRAME_MAC_DISPLAY (f),
					      FRAME_MAC_WINDOW (f),
					      GCFont | GCForeground | GCBackground,
					      &gc_values);

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = f->output_data.mac->cursor_pixel;
  f->output_data.mac->cursor_gc = XCreateGC (FRAME_MAC_DISPLAY (f),
					     FRAME_MAC_WINDOW (f),
					     GCFont | GCForeground | GCBackground,
					     &gc_values);

  /* Reliefs.  */
  f->output_data.mac->white_relief.gc = 0;
  f->output_data.mac->black_relief.gc = 0;

  UNBLOCK_INPUT;
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
  struct mac_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;
  char x_frame_name[10];
  static int x_frame_count = 2;  /* begins at 2 because terminal frame is F1 */

  check_mac ();

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = mac_get_arg (parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = mac_get_arg (parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = mac_get_arg (parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = mac_get_arg (parms, Qminibuffer, "minibuffer", "Minibuffer",
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

  if (EQ (name, Qunbound) || NILP (name))
    {
      sprintf (x_frame_name, "F%d", x_frame_count++);
      f->name = build_string (x_frame_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
    }

  XSETFRAME (frame, f);

  /* Note that X Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;

  f->output_method = output_mac;
  f->output_data.mac = (struct mac_output *) xmalloc (sizeof (struct mac_output));
  bzero (f->output_data.mac, sizeof (struct mac_output));
  FRAME_FONTSET (f) = -1;
  f->output_data.mac->scroll_bar_foreground_pixel = -1;
  f->output_data.mac->scroll_bar_background_pixel = -1;

#if 0
  FRAME_FONTSET (f) = -1;
#endif

  f->icon_name
    = mac_get_arg (parms, Qicon_name, "iconName", "Title", RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

/*  FRAME_W32_DISPLAY_INFO (f) = dpyinfo; */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

  /* Specify the parent under which to make this window.  */

  if (!NILP (parent))
    {
      f->output_data.mac->parent_desc = (Window) parent;
      f->output_data.mac->explicit_parent = 1;
    }
  else
    {
      f->output_data.mac->parent_desc = FRAME_MAC_DISPLAY_INFO (f)->root_window;
      f->output_data.mac->explicit_parent = 0;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->mac_id_name);
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

    font = mac_get_arg (parms, Qfont, "font", "Font", RES_TYPE_STRING);

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
    if (! STRINGP (font))
      font = x_new_font (f, "-ETL-fixed-medium-r-*--*-160-*-*-*-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (!STRINGP (font))
      font = x_new_font (f, "-*-monaco-*-12-*-mac-roman");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-courier-*-10-*-mac-roman");
    if (! STRINGP (font))
      error ("Cannot find any usable font");
    UNBLOCK_INPUT;

    x_default_parameter (f, parms, Qfont, font, 
			 "font", "Font", RES_TYPE_STRING);
  }

  x_default_parameter (f, parms, Qborder_width, make_number (0),
		       "borderwidth", "BorderWidth", RES_TYPE_NUMBER);
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = mac_get_arg (parms, Qinternal_border_width,
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
  x_default_parameter (f, parms, Qtool_bar_lines, make_number (0),
                       "toolBar", "ToolBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);

  f->output_data.mac->parent_desc = FRAME_MAC_DISPLAY_INFO (f)->root_window;
  window_prompting = x_figure_window_size (f, parms);

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.mac->win_gravity = SouthEastGravity;
      else
	f->output_data.mac->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.mac->win_gravity = SouthWestGravity;
      else
	f->output_data.mac->win_gravity = NorthWestGravity;
    }

  f->output_data.mac->size_hint_flags = window_prompting;

  tem = mac_get_arg (parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Create the window. Add the tool-bar height to the initial frame
     height so that the user gets a text display area of the size he
     specified with -g or via the registry. Later changes of the
     tool-bar height don't change the frame size. This is done so that
     users can create tall Emacs frames without having to guess how
     tall the tool-bar will get. */
  f->height += FRAME_TOOL_BAR_LINES (f);

  /* mac_window (f, window_prompting, minibuffer_only); */
  make_mac_frame (f);

  x_icon (f, parms);

  x_make_gc (f);

  /* Now consider the frame official.  */
  FRAME_MAC_DISPLAY_INFO (f)->reference_count++;
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

  /* Set up faces after all frame parameters are known.  */
  call1 (Qface_set_after_frame_default, frame);

#if 0 /* MAC_TODO: when we have window manager hints */
  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;
#endif

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (! f->output_data.mac->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = mac_get_arg (parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;

#if 0 /* MAC_TODO: really no iconify on Mac */
      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else
#endif
      if (! NILP (visibility))
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->x_focus_frame);
  return xfocus;
}

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see.  */)
  (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (mac_defined_color (f, XSTRING (color)->data, &foo, 0))
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

  if (mac_defined_color (f, XSTRING (color)->data, &foo, 0))
    {
      Lisp_Object rgb[3];

      rgb[0] = make_number ((RED_FROM_ULONG (foo.pixel) << 8)
                            | RED_FROM_ULONG (foo.pixel));
      rgb[1] = make_number ((GREEN_FROM_ULONG (foo.pixel) << 8)
                            | GREEN_FROM_ULONG (foo.pixel));
      rgb[2] = make_number ((BLUE_FROM_ULONG (foo.pixel) << 8)
                            | BLUE_FROM_ULONG (foo.pixel));
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 2)
    return Qnil;

  return Qt;
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 1)
    return Qnil;

  return Qt;
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->height);
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Returns the number of bitplanes of the display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes * dpyinfo->n_cbits);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Returns the number of color cells of the display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);
  
  /* MAC_TODO: check whether this is right */
  return make_number ((unsigned long) (pow (2, dpyinfo->n_cbits)));
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Returns the maximum request size of the server of display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.   */)
  (display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number (1);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Returns the vendor ID string of the Mac OS system (Apple).
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return build_string ("Apple Computers");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Returns the version numbers of the server of display DISPLAY.
The value is a list of three integers: the major and minor
version numbers, and the vendor-specific release
number.  See also the function `x-server-vendor'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  int mac_major_version, mac_minor_version;
  SInt32 response;

  if (Gestalt (gestaltSystemVersion, &response) != noErr)
    error ("Cannot get Mac OS version");
  
  mac_major_version = (response >> 8) & 0xf;
  mac_minor_version = (response >> 4) & 0xf;

  return Fcons (make_number (mac_major_version),
		Fcons (make_number (mac_minor_version), Qnil));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the server of display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return make_number (1);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  /* MAC_TODO: this is an approximation, and only of the main display */  

  struct mac_display_info *dpyinfo = check_x_display_info (display);
  short h, v;
  
  ScreenRes (&h, &v);
  
  return make_number ((int) (v / 72.0 * 25.4));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  /* MAC_TODO: this is an approximation, and only of the main display */  

  struct mac_display_info *dpyinfo = check_x_display_info (display);
  short h, v;
  
  ScreenRes (&h, &v);
  
  return make_number ((int) (h / 72.0 * 25.4));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Returns an indication of whether display DISPLAY does backing store.
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
       doc: /* Returns the visual class of the display DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
	(display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);

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
#endif /* 0 */

  error ("Display has an unknown visual class");
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Returns t if the display DISPLAY supports the save-under feature.
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
  return FONT_WIDTH (f->output_data.mac->font);
}

int
x_char_height (f)
     register struct frame *f;
{
  return f->output_data.mac->line_height;
}

int
x_screen_planes (f)
     register struct frame *f;
{
  return FRAME_MAC_DISPLAY_INFO (f)->n_planes;
}

/* Return the display structure for the display named NAME.
   Open a new connection if necessary.  */

struct mac_display_info *
x_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct mac_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = &one_mac_display_info, names = x_display_name_list;
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

  dpyinfo = mac_term_init (name, (unsigned char *) 0,
			   (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", XSTRING (name)->data);

  mac_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 3);

  return dpyinfo;
}

#if 0 /* MAC_TODO: implement network support */
DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to a server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.  */)
  (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned char *xrm_option;
  struct mac_display_info *dpyinfo;

  CHECK_STRING (display);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

  if (! EQ (Vwindow_system, intern ("mac")))
    error ("Not using Mac OS");

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) XSTRING (xrm_string)->data;
  else
    xrm_option = (unsigned char *) 0;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = mac_term_init (display, xrm_option,
			     (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to server %s.\n",
	       XSTRING (display)->data);
      else
	error ("Cannot connect to server %s", XSTRING (display)->data);
    }

  mac_in_use = 1;

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
  struct mac_display_info *dpyinfo = check_x_display_info (display);
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
        x_unload_font (dpyinfo, dpyinfo->font_table[i].font);
      }
  x_destroy_all_bitmaps (dpyinfo);

  x_delete_display (dpyinfo);
  UNBLOCK_INPUT;

  return Qnil;
}
#endif /* 0 */

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
       doc: /* If ON is non-nil, report errors as soon as the erring request is made.
If ON is nil, allow buffering of requests.
This is a noop on Mac OS systems.
The optional second argument DISPLAY specifies which display to act on.
DISPLAY should be either a frame or a display name (a string).
If DISPLAY is omitted or nil, that stands for the selected frame's display.  */)
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
    ascent = height * img->ascent / 100.0;

  return ascent;
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
#if 0 /* MAC_TODO: W32 image support  */

  if (img->pixmap)
    {
      BLOCK_INPUT;
      XFreePixmap (NULL, img->pixmap);
      img->pixmap = 0;
      UNBLOCK_INPUT;
    }

  if (img->ncolors)
    {
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
      
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }
#endif /* MAC_TODO */
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
#if 0 /* MAC_TODO: allocing colors.  */
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
#endif /* MAC_TODO */
  return 0;
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
	if (FRAME_MAC_P (XFRAME (frame)))
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
      BLOCK_INPUT;
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
	  Lisp_Object ascent, margin, relief;

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
  if (FRAME_LIVE_P (f) && FRAME_MAC_P (f))
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
			    Mac support code
 ***********************************************************************/

#if 0 /* MAC_TODO: Mac specific image code.  */

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
#if 0 /* MAC_TODO: Image support for Mac */
  Display *display = FRAME_W32_DISPLAY (f);
  Screen *screen = FRAME_X_SCREEN (f);
  Window window = FRAME_W32_WINDOW (f);

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
  if (*pixmap == 0)
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Unable to create X pixmap", Qnil, Qnil);
      return 0;
    }
#endif /* MAC_TODO */
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
  gc = XCreateGC (NULL, pixmap, 0, NULL);
  XPutImage (NULL, pixmap, gc, ximg, 0, 0, 0, 0, width, height);
  XFreeGC (NULL, gc);
}

#endif /* MAC_TODO */


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
  fd = openp (search_path, file, Qnil, &file_found, Qnil);
  
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
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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
      image_error ("Cannot find image file `%s'", specified_file, Qnil);
      UNGCPRO;
      return 0;
    }
	  
  rc = xbm_read_bitmap_file_data (XSTRING (file)->data, &img->width,
				  &img->height, &data);
  if (rc)
    {
      int depth = one_mac_display_info.n_cbits;
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

#if 0 /* MAC_TODO : Port image display to Mac */
      BLOCK_INPUT;
      img->pixmap
	= XCreatePixmapFromBitmapData (FRAME_W32_DISPLAY (f),
				       FRAME_W32_WINDOW (f),
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
#endif /* MAC_TODO */
    }
  else
    image_error ("Error loading XBM image `%s'", img->spec, Qnil);

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

#if 0 /* MAC_TODO : port Mac display code */
      /* Create the pixmap.  */
      depth = DefaultDepthOfScreen (FRAME_X_SCREEN (f));
      img->pixmap
	= XCreatePixmapFromBitmapData (FRAME_W32_DISPLAY (f),
				       FRAME_W32_WINDOW (f),
				       bits,
				       img->width, img->height,
				       foreground, background,
				       depth);
#endif /* MAC_TODO */

      if (img->pixmap)
	success_p = 1;
      else
	{
	  image_error ("Unable to create pixmap for XBM image `%s'",
                       img->spec, Qnil);
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
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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


#if 0 /* MAC_TODO : Color tables on Mac.  */
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

      color = RGB_TO_ULONG (r, g, b);

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

#endif /* MAC_TODO */


/***********************************************************************
			      Algorithms
 ***********************************************************************/

#if 0 /* MAC_TODO : Mac versions of low level algorithms */
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

  XQueryColors (NULL, cmap, colors, width);
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
#endif /* MAC_TODO */

/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

static void
x_laplace (f, img)
     struct frame *f;
     struct image *img;
{
#if 0 /* MAC_TODO : Mac version */
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
  ximg = XGetImage (NULL, img->pixmap,
		    0, 0, img->width, img->height, ~0, ZPixmap);

  /* Allocate 3 input rows, and one output row of colors.  */
  for (i = 0; i < 3; ++i)
    in[i] = (XColor *) alloca (img->width * sizeof (XColor));
  out = (long *) alloca (img->width * sizeof (long));

  /* Create an X image for output.  */
  rc = x_create_x_image_and_pixmap (f, img->width, img->height, 0,
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
#endif /* MAC_TODO */
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
#if 0 /* MAC_TODO : Mac version */
  Display *dpy = FRAME_W32_DISPLAY (f);
  XImage *ximg, *mask_img;
  int x, y, rc, look_at_corners_p;
  unsigned long bg;

  BLOCK_INPUT;
  
  /* Create an image and pixmap serving as mask.  */
  rc = x_create_x_image_and_pixmap (f, img->width, img->height, 1,
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
#endif /* MAC_TODO */

  return 1;
}



/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/
#ifdef HAVE_PBM

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

      contents = pbm_read_file (file, &size);
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

  BLOCK_INPUT;
  if (!x_create_x_image_and_pixmap (f, width, height, 0,
				    &ximg, &img->pixmap))
    {
      UNBLOCK_INPUT;
      goto error;
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
		  c = *p++;
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      g = pbm_scan_number (&p, end);

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
		UNBLOCK_INPUT;
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
  
  /* Put the image into a pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNBLOCK_INPUT;
      
  img->width = width;
  img->height = height;

  UNGCPRO;
  xfree (contents);
  return 1;
}
#endif /* HAVE_PBM */


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
  FILE *fp = NULL;
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
	  XQueryColor (FRAME_W32_DISPLAY (f), cmap, &color);
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
  if (fp)
    {
      fclose (fp);
      fp = NULL;
    }

  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg,
				    &img->pixmap))
    {
      UNBLOCK_INPUT;
      goto error;
    }
  
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
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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
  mgr.pub.error_exit = my_error_exit;
  cinfo.err = jpeg_std_error (&mgr.pub);
  
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
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg,
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
  if (fp)
    fclose (fp);
  
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
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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

  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
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
  GIF_DATA,
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
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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

  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
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

  /* Read the GIF image into the X image.  We use a local variable
     `raster' here because RasterBits below is a char *, and invites
     problems with bytes >= 0x80.  */
  raster = (unsigned char *) gif->SavedImages[ino].RasterBits;

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

#ifdef HAVE_GHOSTSCRIPT
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
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
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
  img->width = in_width * FRAME_W32_DISPLAY_INFO (f)->resx;
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = XFASTINT (pt_height) / 72.0;
  img->height = in_height * FRAME_W32_DISPLAY_INFO (f)->resy;

  /* Create the pixmap.  */
  BLOCK_INPUT;
  xassert (img->pixmap == 0);
  img->pixmap = XCreatePixmap (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
			       img->width, img->height,
			       DefaultDepthOfScreen (FRAME_X_SCREEN (f)));
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
	    {
	      Colormap cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
	      XFreeColors (FRAME_W32_DISPLAY (f), cmap,
			   img->colors, img->ncolors, 0);
	    }
#endif
	}
      else
	image_error ("Cannot get X image of `%s'; colors will not be freed",
		     img->spec, Qnil);
      
      UNBLOCK_INPUT;
    }
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
#if 0 /* MAC_TODO : port window properties to Mac */
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

#endif /* MAC_TODO */

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 2, 0,
       doc: /* Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.  Value is PROP.  */)
  (prop, frame)
     Lisp_Object prop, frame;
{
#if 0 /* MAC_TODO : port window properties to Mac */

  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), XSTRING (prop)->data, False);
  XDeleteProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f), prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;
#endif  /* MAC_TODO */

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
#if 0 /* MAC_TODO : port window properties to Mac */

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

#endif /* MAC_TODO */
  return Qnil;
}



/***********************************************************************
				Hourglass cursor
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
#if 0 /* MAC_TODO: cursor shape changes.  */
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
#endif /* MAC_TODO */
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
#if 0  /* MAC_TODO: cursor shape changes.  */
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
#endif /* MAC_TODO */
}


/* Hide the hourglass cursor on all frames, if it is currently shown.  */

static void
hide_hourglass ()
{
#if 0 /* MAC_TODO: cursor shape changes.  */
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
#endif /* MAC_TODO */
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame P_ ((struct mac_display_info *,
					   Lisp_Object));
     
/* The frame of a currently visible tooltip, or null.  */

Lisp_Object tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

Lisp_Object tip_timer;
Window tip_window;

/* If non-nil, a vector of 3 elements containing the last args
   with which x-show-tip was called.  See there.  */

Lisp_Object last_show_tip_args;

/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  Value is the frame.  */

static Lisp_Object
x_create_tip_frame (dpyinfo, parms)
     struct mac_display_info *dpyinfo;
     Lisp_Object parms;
{
#if 0 /* MAC_TODO : Mac version */
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
  name = w32_get_arg (parms, Qname, "name", "Name", RES_TYPE_STRING);
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

  f->output_method = output_w32;
  f->output_data.w32 =
    (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));
#if 0
  f->output_data.w32->icon_bitmap = -1;
#endif
  f->output_data.w32->fontset = -1;
  f->icon_name = Qnil;

#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;
  f->output_data.w32->explicit_parent = 0;

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
      = FRAME_W32_WINDOW (f)
      = XCreateWindow (FRAME_W32_DISPLAY (f),
		       FRAME_W32_DISPLAY_INFO (f)->root_window,
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
  FRAME_W32_DISPLAY_INFO (f)->reference_count++;

  return unbind_to (count, frame);
#endif /* MAC_TODO */
  return Qnil;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc : /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small window displaying a string.

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
DY added (default is 10).  */)
  (string, frame, parms, timeout, dx, dy)
     Lisp_Object string, frame, parms, timeout, dx, dy;
{
  struct frame *f;
  struct window *w;
  Window root, child;
  Lisp_Object buffer, top, left;
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

#if 0 /* MAC_TODO : Mac specifics */
	  BLOCK_INPUT;
	  compute_tip_xy (f, parms, dx, dy, &root_x, &root_y);
	  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       root_x, root_y - PIXEL_HEIGHT (f));
	  UNBLOCK_INPUT;
#endif /* MAC_TODO */
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
  frame = x_create_tip_frame (FRAME_MAC_DISPLAY_INFO (f), parms);
  f = XFRAME (frame);

  /* Set up the frame's root window.  Currently we use a size of 80
     columns x 40 lines.  If someone wants to show a larger tip, he
     will loose.  I don't think this is a realistic case.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left = w->top = make_number (0);
  w->width = make_number (80);
  w->height = make_number (40);
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  Ferase_buffer ();
  Finsert (1, &string);
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
#if 0 /* TODO : Mac specifics */
  compute_tip_xy (f, parms, dx, dy, &root_x, &root_y);

  BLOCK_INPUT;
  XQueryPointer (FRAME_W32_DISPLAY (f), FRAME_W32_DISPLAY_INFO (f)->root_window,
		 &root, &child, &root_x, &root_y, &win_x, &win_y, &pmask);
  XMoveResizeWindow (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
		     root_x + 5, root_y - height - 5, width, height);
  XMapRaised (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f));
  UNBLOCK_INPUT;
#endif /* MAC_TODO */

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
Value is t is tooltip was open, nil otherwise.  */)
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

#if 0 /* MAC_TODO: can standard file dialog */
extern Lisp_Object Qfile_name_history;

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 4, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.
Select DEFAULT-FILENAME in the dialog's file selection box, if
specified.  Don't let the user enter a file name in the file
selection dialog's entry field, if MUSTMATCH is non-nil.  */)
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
      char *filename_file;

      /* Prevent redisplay.  */
      specbind (Qinhibit_redisplay, Qt);
      BLOCK_INPUT;

      bzero (&file_details, sizeof (file_details));
      file_details.lStructSize = sizeof (file_details);
      file_details.hwndOwner = FRAME_W32_WINDOW (f);
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
          file = build_string (filename);
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
#endif /* MAC_TODO */



/***********************************************************************
				Tests
 ***********************************************************************/

#if GLYPH_DEBUG

DEFUN ("imagep", Fimagep, Simagep, 1, 1, 0,
       doc: /* Value is non-nil if SPEC is a valid image specification.  */)
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



void
syms_of_macfns ()
{
  /* Certainly running on Mac.  */
  mac_in_use = 1;

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
	       doc: /* The shape of the pointer when Emacs is hourglass.
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

  DEFVAR_LISP ("x-cursor-fore-pixel", &Vx_cursor_fore_pixel,
	       doc: /* A string indicating the foreground color of the cursor box.  */);
  Vx_cursor_fore_pixel = Qnil;

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

#if 0 /* MAC_TODO: implement get X resource */
  defsubr (&Sx_get_resource);
#endif
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
#if 0 /* MAC_TODO: implement XParseGeometry */
  defsubr (&Sx_parse_geometry);
#endif
  defsubr (&Sx_create_frame);
#if 0 /* MAC_TODO: implement network support */
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
#endif
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
  check_window_system_func = check_mac;

#if 0 /* MAC_TODO: Image support for Mac Images.  */
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

#if GLYPH_DEBUG
  defsubr (&Simagep);
  defsubr (&Slookup_image);
#endif
#endif /* MAC_TODO */

  hourglass_atimer = NULL;
  hourglass_shown_p = 0;

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  staticpro (&tip_timer);
  tip_timer = Qnil;

#if 0 /* MAC_TODO */
  defsubr (&Sx_file_dialog);
#endif
}


void
init_xfns ()
{
  image_types = NULL;
  Vimage_types = Qnil;

  define_image_type (&xbm_type);
#if 0 /* NTEMACS_TODO : Image support for W32 */
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
#endif /* NTEMACS_TODO */
}
