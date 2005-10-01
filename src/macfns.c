/* Graphical user interface functions for Mac OS.
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include "lisp.h"
#include "macterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "intervals.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include <epaths.h>
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"
#include "atimer.h"

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <sys/param.h>

extern void free_frame_menubar ();

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

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */

Lisp_Object Vx_pixel_size_width_font_regexp;

Lisp_Object Qnone;
Lisp_Object Qsuppress_icon;
Lisp_Object Qundefined_color;
Lisp_Object Qcancel_timer;

/* In dispnew.c */

extern Lisp_Object Vwindow_system_version;

#if GLYPH_DEBUG
int image_cache_refcount, dpyinfo_refcount;
#endif


#if 0 /* Use xstricmp instead.  */
/* compare two strings ignoring case */

static int
stricmp (const char *s, const char *t)
{
  for ( ; tolower (*s) == tolower (*t); s++, t++)
    if (*s == '\0')
      return 0;
  return tolower (*s) - tolower (*t);
}
#endif

/* compare two strings up to n characters, ignoring case */

static int
strnicmp (const char *s, const char *t, unsigned int n)
{
  for ( ; n > 0 && tolower (*s) == tolower (*t); n--, s++, t++)
    if (*s == '\0')
      return 0;
  return n == 0 ? 0 : tolower (*s) - tolower (*t);
}


/* Error if we are not running on Mac OS.  */

void
check_mac ()
{
  if (! mac_in_use)
    error ("Mac native windows not in use or not initialized");
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
    error ("Non-Mac frame used");
  return f;
}

/* Let the user specify a display with a frame.
   nil stands for the selected frame--or, if that is not a mac frame,
   the first display on the list.  */

struct mac_display_info *
check_x_display_info (frame)
     Lisp_Object frame;
{
  struct mac_display_info *dpyinfo = NULL;

  if (NILP (frame))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_MAC_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_MAC_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
	error ("Mac native windows are not in use or not initialized");
    }
  else if (STRINGP (frame))
    dpyinfo = x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f = check_x_frame (frame);
      dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
    }

  return dpyinfo;
}



static Lisp_Object unwind_create_frame P_ ((Lisp_Object));
static Lisp_Object unwind_create_tip_frame P_ ((Lisp_Object));

void x_set_foreground_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_background_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_mouse_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_border_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_explicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_title P_ ((struct frame *, Lisp_Object, Lisp_Object));
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

extern void mac_get_window_bounds P_ ((struct frame *, Rect *, Rect *));



/* Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */

void
x_real_positions (f, xptr, yptr)
     FRAME_PTR f;
     int *xptr, *yptr;
{
  Rect inner, outer;

  mac_get_window_bounds (f, &inner, &outer);

  f->x_pixels_diff = inner.left - outer.left;
  f->y_pixels_diff = inner.top - outer.top;

  *xptr = outer.left;
  *yptr = outer.top;
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

Lisp_Object
mac_color_map_lookup (colorname)
     char *colorname;
{
  Lisp_Object ret = Qnil;
  int i;

  BLOCK_INPUT;

  for (i = 0; i < sizeof (mac_color_map) / sizeof (mac_color_map[0]); i++)
    if (xstricmp (colorname, mac_color_map[i].name) == 0)
      {
        ret = make_number (mac_color_map[i].color);
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
	  pos = 16;
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
	      pos -= 8;
	      if (i == 2)
		{
		  UNBLOCK_INPUT;
		  return make_number (colorval);
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
	      return make_number (colorval);
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
	  /* We used 0x100 instead of 0xFF to give a continuous
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
	      return make_number (colorval);
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
      color_def->red = RED16_FROM_ULONG (mac_color_ref);
      color_def->green = GREEN16_FROM_ULONG (mac_color_ref);
      color_def->blue = BLUE16_FROM_ULONG (mac_color_ref);

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

  if (strcmp (SDATA (arg), "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (SDATA (arg), "white") == 0)
    return WHITE_PIX_DEFAULT (f);

#if 0
  if (FRAME_MAC_DISPLAY_INFO (f)->n_planes) == 1)
    return def;
#endif

  if (mac_defined_color (f, SDATA (arg), &cdef, 1))
    return cdef.pixel;

  /* defined_color failed; return an ultimate default.  */
  return def;
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
  struct mac_output *mac = f->output_data.mac;
  unsigned long fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_MAC_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_MAC_DISPLAY (f);

      BLOCK_INPUT;
      XSetForeground (dpy, mac->normal_gc, fg);
      XSetBackground (dpy, mac->reverse_gc, fg);

      if (mac->cursor_pixel == old_fg)
	{
	  unload_color (f, mac->cursor_pixel);
	  mac->cursor_pixel = fg;
	  XSetBackground (dpy, mac->cursor_gc, mac->cursor_pixel);
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
  struct mac_output *mac = f->output_data.mac;
  unsigned long bg;

  bg = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  unload_color (f, FRAME_BACKGROUND_PIXEL (f));
  FRAME_BACKGROUND_PIXEL (f) = bg;

  if (FRAME_MAC_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_MAC_DISPLAY (f);

      BLOCK_INPUT;
      XSetBackground (dpy, mac->normal_gc, bg);
      XSetForeground (dpy, mac->reverse_gc, bg);
      XSetWindowBackground (dpy, FRAME_MAC_WINDOW (f), bg);
      XSetForeground (dpy, mac->cursor_gc, bg);

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
  Display *dpy = FRAME_MAC_DISPLAY (f);
  Cursor cursor, nontext_cursor, mode_cursor, hand_cursor;
  Cursor hourglass_cursor, horizontal_drag_cursor;
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = x->background_pixel;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    pixel = x->foreground_pixel;

  f->output_data.mac->mouse_pixel = pixel;

  if (!NILP (Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape);
      cursor = XINT (Vx_pointer_shape);
    }
  else
    cursor = kThemeIBeamCursor;

  if (!NILP (Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape);
      nontext_cursor = XINT (Vx_nontext_pointer_shape);
    }
  else
    nontext_cursor = kThemeArrowCursor;

  if (!NILP (Vx_hourglass_pointer_shape))
    {
      CHECK_NUMBER (Vx_hourglass_pointer_shape);
      hourglass_cursor = XINT (Vx_hourglass_pointer_shape);
    }
  else
    hourglass_cursor = kThemeWatchCursor;

  if (!NILP (Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape);
      mode_cursor = XINT (Vx_mode_pointer_shape);
    }
  else
    mode_cursor = kThemeArrowCursor;

  if (!NILP (Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape);
      hand_cursor = XINT (Vx_sensitive_text_pointer_shape);
    }
  else
    hand_cursor = kThemePointingHandCursor;

  if (!NILP (Vx_window_horizontal_drag_shape))
    {
      CHECK_NUMBER (Vx_window_horizontal_drag_shape);
      horizontal_drag_cursor = XINT (Vx_window_horizontal_drag_shape);
    }
  else
    horizontal_drag_cursor = kThemeResizeLeftRightCursor;

#if 0 /* MAC_TODO: cursor color changes */
  {
    XColor fore_color, back_color;

    fore_color.pixel = f->output_data.mac->mouse_pixel;
    x_query_color (f, &fore_color);
    back_color.pixel = mask_color;
    x_query_color (f, &back_color);

    XRecolorCursor (dpy, cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, nontext_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, mode_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hand_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hourglass_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, horizontal_drag_cursor, &fore_color, &back_color);
  }
#endif

  BLOCK_INPUT;

  rif->define_frame_cursor (f, cursor);

  f->output_data.mac->text_cursor = cursor;
  f->output_data.mac->nontext_cursor = nontext_cursor;
  f->output_data.mac->hourglass_cursor = hourglass_cursor;
  f->output_data.mac->modeline_cursor = mode_cursor;
  f->output_data.mac->hand_cursor = hand_cursor;
  f->output_data.mac->horizontal_drag_cursor = horizontal_drag_cursor;

  UNBLOCK_INPUT;

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

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
      pixel = f->output_data.mac->mouse_pixel;
      if (pixel == fore_pixel)
	fore_pixel = FRAME_BACKGROUND_PIXEL (f);
    }

  f->output_data.mac->cursor_foreground_pixel = fore_pixel;
  f->output_data.mac->cursor_pixel = pixel;

  if (FRAME_MAC_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      /* Update frame's cursor_gc.  */
      XSetBackground (FRAME_MAC_DISPLAY (f),
		      f->output_data.mac->cursor_gc, pixel);
      XSetForeground (FRAME_MAC_DISPLAY (f),
		      f->output_data.mac->cursor_gc, fore_pixel);
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
   F has a window.  */

void
x_set_border_pixel (f, pix)
     struct frame *f;
     int pix;
{

  f->output_data.mac->border_pixel = pix;

  if (FRAME_MAC_WINDOW (f) != 0 && f->border_width > 0)
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


void
x_set_cursor_type (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  set_frame_cursor_types (f, arg);

  /* Make sure the cursor gets redrawn.  */
  cursor_type_changed = 1;
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
			(char *) SDATA ((!NILP (f->icon_name)
					 ? f->icon_name
					 : !NILP (f->title)
					 ? f->title
					 : f->name)));

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
      x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
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
  root_height = WINDOW_TOTAL_LINES (XWINDOW (root_window));
  if (root_height - delta < 1)
    {
      delta = root_height - 1;
      nlines = FRAME_TOOL_BAR_LINES (f) + delta;
    }

  FRAME_TOOL_BAR_LINES (f) = nlines;
  change_window_heights (root_window, delta);
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
      int width = FRAME_PIXEL_WIDTH (f);
      int y = nlines * FRAME_LINE_HEIGHT (f);

      BLOCK_INPUT;
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		    0, y, width, height, 0);
      UNBLOCK_INPUT;

      if (WINDOWP (f->tool_bar_window))
	clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);
    }
}



/* Set the Mac window title to NAME for frame F.  */

static void
x_set_name_internal (f, name)
     FRAME_PTR f;
     Lisp_Object name;
{
  if (FRAME_MAC_WINDOW (f))
    {
      if (STRING_MULTIBYTE (name))
#if TARGET_API_MAC_CARBON
	name = ENCODE_UTF_8 (name);
#else
	name = ENCODE_SYSTEM (name);
#endif

      BLOCK_INPUT;

      {
#if TARGET_API_MAC_CARBON
	CFStringRef windowTitle =
	  cfstring_create_with_utf8_cstring (SDATA (name));

	SetWindowTitleWithCFString (FRAME_MAC_WINDOW (f), windowTitle);
	CFRelease (windowTitle);
#else
	Str255 windowTitle;
	if (strlen (SDATA (name)) < 255)
	  {
	    strcpy (windowTitle, SDATA (name));
	    c2pstr (windowTitle);
	    SetWTitle (FRAME_MAC_WINDOW (f), windowTitle);
	  }
#endif
      }

      UNBLOCK_INPUT;
    }
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       mac_id_name.

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

  /* If NAME is nil, set the name to the mac_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_MAC_DISPLAY_INFO (f)->mac_id_name,
		   SDATA (f->name)))
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

  x_set_name_internal (f, name);
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

  x_set_name_internal (f, name);
}

void
x_set_scroll_bar_default_width (f)
     struct frame *f;
{
  /* Imitate X without X Toolkit */

  int wid = FRAME_COLUMN_WIDTH (f);

#ifdef MAC_OSX
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = 16;  /* Aqua scroll bars.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) +
				      wid - 1) / wid;
#else /* not MAC_OSX */
  /* Make the actual width at least 14 pixels and a multiple of a
     character width.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;

  /* Use all of that space (aside from required margins) for the
     scroll bar.  */
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = 0;
#endif /* not MAC_OSX */
}


/* Subroutines of creating a frame.  */

/* Retrieve the string resource specified by NAME with CLASS from
   database RDB.

   The return value points to the contents of a Lisp string.  So it
   will not be valid after the next GC where string compaction will
   occur.  */

char *
x_get_string_resource (rdb, name, class)
     XrmDatabase rdb;
     char *name, *class;
{
  Lisp_Object value = xrm_get_resource (rdb, name, class);

  if (STRINGP (value))
    return SDATA (value);
  else
    return NULL;
}

/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   mac_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

static Lisp_Object
mac_get_arg (alist, param, attribute, class, type)
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  return x_get_arg (check_x_display_info (Qnil),
		    alist, param, attribute, class, type);
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


/* Create and set up the Mac window for frame F.  */

static void
mac_window (f)
     struct frame *f;
{
  Rect r;

  BLOCK_INPUT;

  SetRect (&r, f->left_pos, f->top_pos,
           f->left_pos + FRAME_PIXEL_WIDTH (f),
           f->top_pos + FRAME_PIXEL_HEIGHT (f));
#if TARGET_API_MAC_CARBON
  CreateNewWindow (kDocumentWindowClass,
		   kWindowStandardDocumentAttributes
		   /* | kWindowToolbarButtonAttribute */,
		   &r, &FRAME_MAC_WINDOW (f));
  if (FRAME_MAC_WINDOW (f))
    {
      SetWRefCon (FRAME_MAC_WINDOW (f), (long) f->output_data.mac);
      if (install_window_handler (FRAME_MAC_WINDOW (f)) != noErr)
	{
	  DisposeWindow (FRAME_MAC_WINDOW (f));
	  FRAME_MAC_WINDOW (f) = NULL;
	}
    }
#else
  FRAME_MAC_WINDOW (f)
    = NewCWindow (NULL, &r, "\p", false, zoomDocProc,
		  (WindowPtr) -1, 1, (long) f->output_data.mac);
#endif
  /* so that update events can find this mac_output struct */
  f->output_data.mac->mFP = f;  /* point back to emacs frame */

#ifndef MAC_OSX
  if (FRAME_MAC_WINDOW (f))
    {
      ControlRef root_control;

      if (CreateRootControl (FRAME_MAC_WINDOW (f), &root_control) != noErr)
	{
	  DisposeWindow (FRAME_MAC_WINDOW (f));
	  FRAME_MAC_WINDOW (f) = NULL;
	}
    }
#endif
  if (FRAME_MAC_WINDOW (f))
    XSetWindowBackground (FRAME_MAC_DISPLAY(f), FRAME_MAC_WINDOW (f),
			  FRAME_BACKGROUND_PIXEL (f));

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

  if (FRAME_MAC_WINDOW (f) == 0)
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

  x_text_icon (f, (char *) SDATA ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name)));
#endif

  UNBLOCK_INPUT;
}


void
x_make_gc (f)
     struct frame *f;
{
  XGCValues gc_values;

  BLOCK_INPUT;

  /* Create the GCs of this frame.
     Note that many default values are used.  */

  /* Normal video */
  gc_values.font = FRAME_FONT (f);
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

#if 0
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
#endif

  UNBLOCK_INPUT;
}


/* Free what was was allocated in x_make_gc.  */

void
x_free_gcs (f)
     struct frame *f;
{
  Display *dpy = FRAME_MAC_DISPLAY (f);

  BLOCK_INPUT;

  if (f->output_data.mac->normal_gc)
    {
      XFreeGC (dpy, f->output_data.mac->normal_gc);
      f->output_data.mac->normal_gc = 0;
    }

  if (f->output_data.mac->reverse_gc)
    {
      XFreeGC (dpy, f->output_data.mac->reverse_gc);
      f->output_data.mac->reverse_gc = 0;
    }

  if (f->output_data.mac->cursor_gc)
    {
      XFreeGC (dpy, f->output_data.mac->cursor_gc);
      f->output_data.mac->cursor_gc = 0;
    }

#if 0
  if (f->output_data.mac->border_tile)
    {
      XFreePixmap (dpy, f->output_data.mac->border_tile);
      f->output_data.mac->border_tile = 0;
    }
#endif

  if (f->output_data.mac->white_relief.gc)
    {
      XFreeGC (dpy, f->output_data.mac->white_relief.gc);
      f->output_data.mac->white_relief.gc = 0;
    }

  if (f->output_data.mac->black_relief.gc)
    {
      XFreeGC (dpy, f->output_data.mac->black_relief.gc);
      f->output_data.mac->black_relief.gc = 0;
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

#if GLYPH_DEBUG
      /* Check that reference counts are indeed correct.  */
      xassert (dpyinfo->reference_count == dpyinfo_refcount);
      xassert (dpyinfo->image_cache->refcount == image_cache_refcount);
#endif
      return Qt;
    }

  return Qnil;
}


DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new window, which is called a "frame" in Emacs terms.
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
  int count = SPECPDL_INDEX ();
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
  record_unwind_protect (unwind_create_frame, frame);

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
      f->output_data.mac->parent_desc = (Window) XFASTINT (parent);
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
	  font = x_new_fontset (f, SDATA (tem));
	else
	  font = x_new_font (f, SDATA (font));
      }

    /* Try out a font which we hope has bold and italic variations.  */
#if USE_ATSUI
    if (! STRINGP (font))
      font = x_new_font (f, "-*-monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1");
#endif
    if (! STRINGP (font))
      font = x_new_font (f, "-ETL-fixed-medium-r-*--*-160-*-*-*-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_fontset (f, "fontset-mac");
    if (! STRINGP (font))
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
  x_default_parameter (f, parms, Qtool_bar_lines, make_number (1),
                       "toolBar", "ToolBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  f->output_data.mac->parent_desc = FRAME_MAC_DISPLAY_INFO (f)->root_window;

  /* Compute the size of the window.  */
  window_prompting = x_figure_window_size (f, parms, 1);

  tem = mac_get_arg (parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  mac_window (f);

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
		       "scrollBarWidth", "ScrollBarWidth",
		       RES_TYPE_NUMBER);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);

  SET_FRAME_COLS (f, 0);
  FRAME_LINES (f) = 0;
  change_frame_size (f, height, width, 1, 0, 0);

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;

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

  if (mac_defined_color (f, SDATA (color), &foo, 0))
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

  if (mac_defined_color (f, SDATA (color), &foo, 0))
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  if (!dpyinfo->color_p)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* Return t if DISPLAY supports shades of gray.
Note that color displays do support shades of gray.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  if (dpyinfo->n_planes <= 1)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Returns the width in pixels of DISPLAY.
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
       doc: /* Returns the height in pixels of DISPLAY.
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
       doc: /* Returns the number of bitplanes of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes);
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_number (1 << min (dpyinfo->n_planes, 24));
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
  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number (1);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Returns the "vendor ID" string of the Mac OS system (Apple).
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  return build_string ("Apple Computers");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Returns the version numbers of the Mac OS system.
The value is a list of three integers: the major and minor
version numbers, and the vendor-specific release
number.  See also the function `x-server-vendor'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  int mac_major_version;
  SInt32 response;
  OSErr err;

  BLOCK_INPUT;
  err = Gestalt (gestaltSystemVersion, &response);
  UNBLOCK_INPUT;

  if (err != noErr)
    error ("Cannot get Mac OS version");

  mac_major_version = (response >> 8) & 0xff;
  /* convert BCD to int */
  mac_major_version -= (mac_major_version >> 4) * 6;

  return Fcons (make_number (mac_major_version),
		Fcons (make_number ((response >> 4) & 0xf),
		       Fcons (make_number (response & 0xf),
			      Qnil)));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the server of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  return make_number (1);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  /* MAC_TODO: this is an approximation, and only of the main display */

  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number ((int) (dpyinfo->height * 25.4 / dpyinfo->resy));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  /* MAC_TODO: this is an approximation, and only of the main display */

  struct mac_display_info *dpyinfo = check_x_display_info (display);

  return make_number ((int) (dpyinfo->width * 25.4 / dpyinfo->resx));
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

  return (intern ("true-color"));
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
  return FRAME_PIXEL_WIDTH (f);
}

int
x_pixel_height (f)
     register struct frame *f;
{
  return FRAME_PIXEL_HEIGHT (f);
}

int
x_char_width (f)
     register struct frame *f;
{
  return FRAME_COLUMN_WIDTH (f);
}

int
x_char_height (f)
     register struct frame *f;
{
  return FRAME_LINE_HEIGHT (f);
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

  if (! EQ (Vwindow_system, intern ("mac")))
    error ("Not using Mac native windows");

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
			   (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", SDATA (name));

  mac_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 3);

  return dpyinfo;
}

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
    error ("Not using Mac native windows");

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) SDATA (xrm_string);
  else
    xrm_option = (unsigned char *) 0;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = mac_term_init (display, xrm_option,
			     (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to server %s.\n",
	       SDATA (display));
      else
	error ("Cannot connect to server %s", SDATA (display));
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
        mac_unload_font (dpyinfo, dpyinfo->font_table[i].font);
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
  for (tail = x_display_name_list; ! NILP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* This is a noop on Mac OS systems.  */)
    (on, display)
    Lisp_Object display, on;
{
  return Qnil;
}


/***********************************************************************
                           Window properties
 ***********************************************************************/

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 6, 0,
       doc: /* Change window property PROP to VALUE on the X window of FRAME.
VALUE may be a string or a list of conses, numbers and/or strings.
If an element in the list is a string, it is converted to
an Atom and the value of the Atom is used.  If an element is a cons,
it is converted to a 32 bit number where the car is the 16 top bits and the
cdr is the lower 16 bits.
FRAME nil or omitted means use the selected frame.
If TYPE is given and non-nil, it is the name of the type of VALUE.
If TYPE is not given or nil, the type is STRING.
FORMAT gives the size in bits of each element if VALUE is a list.
It must be one of 8, 16 or 32.
If VALUE is a string or FORMAT is nil or not given, FORMAT defaults to 8.
If OUTER_P is non-nil, the property is changed for the outer X window of
FRAME.  Default is to change on the edit X window.

Value is VALUE.  */)
     (prop, value, frame, type, format, outer_p)
     Lisp_Object prop, value, frame, type, format, outer_p;
{
#if 0 /* MAC_TODO : port window properties to Mac */
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  CHECK_STRING (value);

  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
  XChangeProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
		   prop_atom, XA_STRING, 8, PropModeReplace,
		   SDATA (value), SCHARS (value));

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
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
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
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
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

/* Return non-zero if houglass timer has been started or hourglass is shown.  */

int
hourglass_started ()
{
  return hourglass_shown_p || hourglass_atimer != NULL;
}


/* Cancel a currently active hourglass timer, and start a new one.  */

void
start_hourglass ()
{
#ifdef MAC_OSX
  EMACS_TIME delay;
  int secs, usecs = 0;

  /* Don't bother for ttys.  */
  if (NILP (Vwindow_system))
    return;

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
#endif /* MAC_OSX */
}


/* Cancel the hourglass cursor timer if active, hide a busy cursor if
   shown.  */

void
cancel_hourglass ()
{
#ifdef MAC_OSX
  if (hourglass_atimer)
    {
      cancel_atimer (hourglass_atimer);
      hourglass_atimer = NULL;
    }

  if (hourglass_shown_p)
    hide_hourglass ();
#endif /* MAC_OSX */
}


/* Timer function of hourglass_atimer.  TIMER is equal to
   hourglass_atimer.

   On Mac, busy status is shown by the progress indicator (chasing
   arrows) at the upper-right corner of each frame instead of the
   hourglass pointer.  */

static void
show_hourglass (timer)
     struct atimer *timer;
{
#if TARGET_API_MAC_CARBON
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

	  if (FRAME_LIVE_P (f) && FRAME_MAC_P (f)
	      && FRAME_MAC_WINDOW (f) != tip_window)
	    {
	      if (!f->output_data.mac->hourglass_control)
		{
		  Window w = FRAME_MAC_WINDOW (f);
		  Rect r;
		  ControlRef c;

		  GetWindowPortBounds (w, &r);
		  r.left = r.right - HOURGLASS_WIDTH;
		  r.bottom = r.top + HOURGLASS_HEIGHT;
		  if (CreateChasingArrowsControl (w, &r, &c) == noErr)
		    f->output_data.mac->hourglass_control = c;
		}

	      if (f->output_data.mac->hourglass_control)
		ShowControl (f->output_data.mac->hourglass_control);
	    }
	}

      hourglass_shown_p = 1;
      UNBLOCK_INPUT;
    }
#endif /* TARGET_API_MAC_CARBON */
}


/* Hide the progress indicators on all frames, if it is currently
   shown.  */

static void
hide_hourglass ()
{
#if TARGET_API_MAC_CARBON
  if (hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_MAC_P (f)
	      /* Watch out for newly created frames.  */
	      && f->output_data.mac->hourglass_control)
	    HideControl (f->output_data.mac->hourglass_control);
	}

      hourglass_shown_p = 0;
      UNBLOCK_INPUT;
    }
#endif /* TARGET_API_MAC_CARBON */
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame P_ ((struct mac_display_info *,
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
     struct mac_display_info *dpyinfo;
     Lisp_Object parms, text;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  long window_prompting = 0;
  int width, height;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct kboard *kb;
  int face_change_count_before = face_change_count;
  Lisp_Object buffer;
  struct buffer *old_buffer;

  check_mac ();


#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  /* Get the name of the frame to use for resource lookup.  */
  name = mac_get_arg (parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  frame = Qnil;
  GCPRO3 (parms, name, frame);
  f = make_frame (1);
  XSETFRAME (frame, f);

  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer, Qnil);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  current_buffer->truncate_lines = Qnil;
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  Ferase_buffer ();
  Finsert (1, &text);
  set_buffer_internal_1 (old_buffer);

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  record_unwind_protect (unwind_create_tip_frame, frame);

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_mac;
  f->output_data.mac =
    (struct mac_output *) xmalloc (sizeof (struct mac_output));
  bzero (f->output_data.mac, sizeof (struct mac_output));

  FRAME_FONTSET (f)  = -1;
  f->icon_name = Qnil;

#if GLYPH_DEBUG
  image_cache_refcount = FRAME_X_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif
  f->output_data.mac->parent_desc = FRAME_MAC_DISPLAY_INFO (f)->root_window;
  f->output_data.mac->explicit_parent = 0;

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

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = mac_get_arg (parms, Qfont, "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
	tem = Fquery_fontset (font, Qnil);
	if (STRINGP (tem))
	  font = x_new_fontset (f, SDATA (tem));
	else
	  font = x_new_font (f, SDATA (font));
      }

    /* Try out a font which we hope has bold and italic variations.  */
#if USE_ATSUI
    if (! STRINGP (font))
      font = x_new_font (f, "-*-monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1");
#endif
    if (! STRINGP (font))
      font = x_new_font (f, "-ETL-fixed-medium-r-*--*-160-*-*-*-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_fontset (f, "fontset-mac");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-monaco-*-12-*-mac-roman");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-courier-*-10-*-mac-roman");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      error ("Cannot find any usable font");

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

      value = mac_get_arg (parms, Qinternal_border_width,
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

  f->output_data.mac->parent_desc = FRAME_MAC_DISPLAY_INFO (f)->root_window;

  window_prompting = x_figure_window_size (f, parms, 0);

  {
    Rect r;

    BLOCK_INPUT;
    SetRect (&r, 0, 0, 1, 1);
#if TARGET_API_MAC_CARBON
    if (CreateNewWindow (kHelpWindowClass,
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
			 kWindowIgnoreClicksAttribute |
#endif
			 kWindowNoUpdatesAttribute |
			 kWindowNoActivatesAttribute,
			 &r, &tip_window) == noErr)
#else
    if (tip_window = NewCWindow (NULL, &r, "\p", false, plainDBox,
				 NULL, false, 0L))
#endif
      {
	FRAME_MAC_WINDOW (f) = tip_window;
	XSetWindowBackground (FRAME_MAC_DISPLAY(f), tip_window,
			      FRAME_BACKGROUND_PIXEL (f));
	SetWRefCon (tip_window, (long) f->output_data.mac);
	/* so that update events can find this mac_output struct */
	f->output_data.mac->mFP = f;
      }
    UNBLOCK_INPUT;
  }

  x_make_gc (f);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);
  SET_FRAME_COLS (f, 0);
  FRAME_LINES (f) = 0;
  change_frame_size (f, height, width, 1, 0, 0);

  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, intern ("tooltip"))))
    Fmodify_frame_parameters (frame, Fcons (Fcons (intern ("tooltip"), Qt),
					    Qnil));

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
  FRAME_MAC_DISPLAY_INFO (f)->reference_count++;

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
      Point mouse_pos;

      BLOCK_INPUT;
      GetMouse (&mouse_pos);
      LocalToGlobal (&mouse_pos);
      *root_x = mouse_pos.h;
      *root_y = mouse_pos.v;
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
  else if (*root_x + XINT (dx) + width <= FRAME_MAC_DISPLAY_INFO (f)->width)
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left-justified on the screen -- it ought to fit that way.  */
    *root_x = 0;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small window displaying a string.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARMS contains a `left' parameter,
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
  int count = SPECPDL_INDEX ();

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
	  compute_tip_xy (f, parms, dx, dy, FRAME_PIXEL_WIDTH (f),
			  FRAME_PIXEL_HEIGHT (f), &root_x, &root_y);
	  MoveWindow (FRAME_MAC_WINDOW (f), root_x, root_y, false);
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
  frame = x_create_tip_frame (FRAME_MAC_DISPLAY_INFO (f), parms, string);
  f = XFRAME (frame);

  /* Set up the frame's root window.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left_col = w->top_line = make_number (0);

  if (CONSP (Vx_max_tooltip_size)
      && INTEGERP (XCAR (Vx_max_tooltip_size))
      && XINT (XCAR (Vx_max_tooltip_size)) > 0
      && INTEGERP (XCDR (Vx_max_tooltip_size))
      && XINT (XCDR (Vx_max_tooltip_size)) > 0)
    {
      w->total_cols = XCAR (Vx_max_tooltip_size);
      w->total_lines = XCDR (Vx_max_tooltip_size);
    }
  else
    {
      w->total_cols = make_number (80);
      w->total_lines = make_number (40);
    }

  FRAME_TOTAL_COLS (f) = XINT (w->total_cols);
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (XWINDOW (FRAME_ROOT_WINDOW (f))->buffer));
  current_buffer->truncate_lines = Qnil;
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (FRAME_ROOT_WINDOW (f), pos, 0);

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
  MoveWindow (FRAME_MAC_WINDOW (f), root_x, root_y, false);
  SizeWindow (FRAME_MAC_WINDOW (f), width, height, true);
  ShowWindow (FRAME_MAC_WINDOW (f));
  BringToFront (FRAME_MAC_WINDOW (f));
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

  count = SPECPDL_INDEX ();
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



#if TARGET_API_MAC_CARBON
/***********************************************************************
			File selection dialog
 ***********************************************************************/

static pascal void mac_nav_event_callback P_ ((NavEventCallbackMessage,
					       NavCBRecPtr, void *));

/**
   There is a relatively standard way to do this using applescript to run
   a (choose file) method.  However, this doesn't do "the right thing"
   by working only if the find-file occurred during a menu or toolbar
   click.  So we must do the file dialog by hand, using the navigation
   manager.  This also has more flexibility in determining the default
   directory and whether or not we are going to choose a file.
 **/

extern Lisp_Object Qfile_name_history;

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.
Select DEFAULT-FILENAME in the dialog's file selection box, if
specified.  Ensure that file exists if MUSTMATCH is non-nil.
If ONLY-DIR-P is non-nil, the user can only select directories.  */)
     (prompt, dir, default_filename, mustmatch, only_dir_p)
     Lisp_Object prompt, dir, default_filename, mustmatch, only_dir_p;
{
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
  char filename[MAXPATHLEN];
  int default_filter_index = 1; /* 1: All Files, 2: Directories only  */
  static NavEventUPP mac_nav_event_callbackUPP = NULL;

  GCPRO6 (prompt, dir, default_filename, mustmatch, file, only_dir_p);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);

  {
    OSStatus status;
    NavDialogCreationOptions options;
    NavDialogRef dialogRef;
    NavTypeListHandle fileTypes = NULL;
    NavUserAction userAction;
    CFStringRef message=NULL, saveName = NULL;

    BLOCK_INPUT;
    /* No need for a callback function because we are modal */
    NavGetDefaultDialogCreationOptions(&options);
    options.modality = kWindowModalityAppModal;
    options.location.h = options.location.v = -1;
    options.optionFlags = kNavDefaultNavDlogOptions;
    options.optionFlags |= kNavAllFilesInPopup;  /* All files allowed */
    options.optionFlags |= kNavSelectAllReadableItem;
    options.optionFlags &= ~kNavAllowMultipleFiles;
    if (!NILP(prompt))
      {
	message = cfstring_create_with_string (prompt);
	options.message = message;
      }
    /* Don't set the application, let it use default.
    options.clientName = CFSTR ("Emacs");
    */

    if (mac_nav_event_callbackUPP == NULL)
      mac_nav_event_callbackUPP = NewNavEventUPP (mac_nav_event_callback);

    if (!NILP (only_dir_p))
      status = NavCreateChooseFolderDialog(&options, mac_nav_event_callbackUPP,
					   NULL, NULL, &dialogRef);
    else if (NILP (mustmatch))
      {
	/* This is a save dialog */
	options.optionFlags |= kNavDontConfirmReplacement;
	options.actionButtonLabel = CFSTR ("Ok");
	options.windowTitle = CFSTR ("Enter name");

	if (STRINGP (default_filename))
	  {
	    Lisp_Object utf8 = ENCODE_UTF_8 (default_filename);
	    char *begPtr = SDATA(utf8);
	    char *filePtr = begPtr + SBYTES(utf8);
	    while (filePtr != begPtr && !IS_DIRECTORY_SEP(filePtr[-1]))
	      filePtr--;
	    saveName = cfstring_create_with_utf8_cstring (filePtr);
	    options.saveFileName = saveName;
	    options.optionFlags |= kNavSelectDefaultLocation;
	  }
	  status = NavCreatePutFileDialog(&options,
					  'TEXT', kNavGenericSignature,
					  mac_nav_event_callbackUPP, NULL,
					  &dialogRef);
	}
    else
      {
	/* This is an open dialog*/
	status = NavCreateChooseFileDialog(&options, fileTypes,
					   mac_nav_event_callbackUPP, NULL,
					   NULL, NULL, &dialogRef);
      }

    /* Set the default location and continue*/
    if (status == noErr)
      {
	AEDesc defLocAed;
#ifdef MAC_OSX
	FSRef defLoc;
	status = FSPathMakeRef(SDATA(ENCODE_FILE(dir)), &defLoc, NULL);
#else
	FSSpec defLoc;
	status = posix_pathname_to_fsspec (SDATA (ENCODE_FILE (dir)), &defLoc);
#endif
	if (status == noErr)
	  {
#ifdef MAC_OSX
	    AECreateDesc(typeFSRef, &defLoc, sizeof(FSRef), &defLocAed);
#else
	    AECreateDesc(typeFSS, &defLoc, sizeof(FSSpec), &defLocAed);
#endif
	    NavCustomControl(dialogRef, kNavCtlSetLocation, (void*) &defLocAed);
	    AEDisposeDesc(&defLocAed);
	  }
	status = NavDialogRun(dialogRef);
      }

    if (saveName) CFRelease(saveName);
    if (message) CFRelease(message);

    if (status == noErr) {
      userAction = NavDialogGetUserAction(dialogRef);
      switch (userAction)
	{
	case kNavUserActionNone:
	case kNavUserActionCancel:
	  break;		/* Treat cancel like C-g */
	case kNavUserActionOpen:
	case kNavUserActionChoose:
	case kNavUserActionSaveAs:
	  {
	    NavReplyRecord reply;
	    AEDesc aed;
#ifdef MAC_OSX
	    FSRef fsRef;
#else
	    FSSpec fs;
#endif
	    status = NavDialogGetReply(dialogRef, &reply);

#ifdef MAC_OSX
	    AECoerceDesc(&reply.selection, typeFSRef, &aed);
	    AEGetDescData(&aed, (void *) &fsRef, sizeof (FSRef));
	    FSRefMakePath(&fsRef, (UInt8 *) filename, sizeof (filename));
#else
	    AECoerceDesc (&reply.selection, typeFSS, &aed);
	    AEGetDescData (&aed, (void *) &fs, sizeof (FSSpec));
	    fsspec_to_posix_pathname (&fs, filename, sizeof (filename) - 1);
#endif
	    AEDisposeDesc(&aed);
	    if (reply.saveFileName)
	      {
		/* If it was a saved file, we need to add the file name */
		int len = strlen(filename);
		if (len && filename[len-1] != '/')
		  filename[len++] = '/';
		CFStringGetCString(reply.saveFileName, filename+len,
				   sizeof (filename) - len,
#if MAC_OSX
				   kCFStringEncodingUTF8
#else
				   CFStringGetSystemEncoding ()
#endif
				   );
	      }
	    file = DECODE_FILE (make_unibyte_string (filename,
						     strlen (filename)));
	    NavDisposeReply(&reply);
	  }
	  break;
	}
      NavDialogDispose(dialogRef);
      UNBLOCK_INPUT;
    }
    else {
      UNBLOCK_INPUT;
      /* Fall back on minibuffer if there was a problem */
      file = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			       dir, mustmatch, dir, Qfile_name_history,
			       default_filename, Qnil);
    }
  }

  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, file);
}


/* Need to register some event callback function for enabling drag and
   drop in Navigation Service dialogs.  */
static pascal void
mac_nav_event_callback (selector, parms, data)
     NavEventCallbackMessage selector;
     NavCBRecPtr parms;
     void *data ;
{
}
#endif

/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Keep this list in the same order as frame_parms in frame.c.
   Use 0 for unsupported frame parameters.  */

frame_parm_handler mac_frame_parm_handlers[] =
{
  x_set_autoraise,
  x_set_autolower,
  x_set_background_color,
  x_set_border_color,
  x_set_border_width,
  x_set_cursor_color,
  x_set_cursor_type,
  x_set_font,
  x_set_foreground_color,
  x_set_icon_name,
  0, /* MAC_TODO: x_set_icon_type, */
  x_set_internal_border_width,
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  x_set_scroll_bar_width,
  x_set_title,
  x_set_unsplittable,
  x_set_vertical_scroll_bars,
  x_set_visibility,
  x_set_tool_bar_lines,
  0, /* MAC_TODO: x_set_scroll_bar_foreground, */
  0, /* MAC_TODO: x_set_scroll_bar_background, */
  x_set_screen_gamma,
  x_set_line_spacing,
  x_set_fringe_width,
  x_set_fringe_width,
  0, /* x_set_wait_for_wm, */
  x_set_fullscreen,
};

void
syms_of_macfns ()
{
#ifdef MAC_OSX
  /* This is zero if not using Mac native windows.  */
  mac_in_use = 0;
#else
  /* Certainly running on Mac native windows.  */
  mac_in_use = 1;
#endif

  /* The section below is built by the lisp expression at the top of the file,
     just above where these variables are declared.  */
  /*&&& init symbols here &&&*/
  Qnone = intern ("none");
  staticpro (&Qnone);
  Qsuppress_icon = intern ("suppress-icon");
  staticpro (&Qsuppress_icon);
  Qundefined_color = intern ("undefined-color");
  staticpro (&Qundefined_color);
  Qcancel_timer = intern ("cancel-timer");
  staticpro (&Qcancel_timer);
  /* This is the end of symbol initialization.  */

  /* Text property `display' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qdisplay, Qt), Vtext_property_default_nonsticky);


  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
    doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

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
  defsubr (&Sx_create_frame);
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
  check_window_system_func = check_mac;

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

#if TARGET_API_MAC_CARBON
  defsubr (&Sx_file_dialog);
#endif
}

/* arch-tag: d7591289-f374-4377-b245-12f5dbbb8edc
   (do not change this comment) */
