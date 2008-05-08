/* Graphical user interface functions for the Microsoft W32 API.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
                 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Added by Kevin Gallo */

#include <config.h>

#include <signal.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <math.h>

#include "lisp.h"
#include "w32term.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "intervals.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include "epaths.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "ccl.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"
#include "w32heap.h"

#include "bitmaps/gray.xbm"

#include <commctrl.h>
#include <commdlg.h>
#include <shellapi.h>
#include <ctype.h>
#include <winspool.h>
#include <objbase.h>

#include <dlgs.h>
#include <imm.h>
#define FILE_NAME_TEXT_FIELD edt1

#ifdef USE_FONT_BACKEND
#include "font.h"
#endif

void syms_of_w32fns ();
void globals_of_w32fns ();

extern void free_frame_menubar ();
extern double atof ();
extern int w32_console_toggle_lock_key P_ ((int, Lisp_Object));
extern void w32_menu_display_help P_ ((HWND, HMENU, UINT, UINT));
extern void w32_free_menu_strings P_ ((HWND));
extern XCharStruct *w32_per_char_metric P_ ((XFontStruct *, wchar_t *, int));

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
int w32_quit_key;

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
int w32_mouse_button_tolerance;

/* Minimum interval between mouse movement (and scroll bar drag)
   events that are passed on to the event loop. */
int w32_mouse_move_interval;

/* Flag to indicate if XBUTTON events should be passed on to Windows.  */
static int w32_pass_extra_mouse_buttons_to_system;

/* Flag to indicate if media keys should be passed on to Windows.  */
static int w32_pass_multimedia_buttons_to_system;

/* Non nil if no window manager is in use.  */
Lisp_Object Vx_no_window_manager;

/* Non-zero means we're allowed to display a hourglass pointer.  */

int display_hourglass_p;

/* If non-zero, a w32 timer that, when it expires, displays an
   hourglass cursor on all frames.  */
static unsigned hourglass_timer = 0;
static HWND hourglass_hwnd = NULL;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */

Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;
Lisp_Object Vx_hourglass_pointer_shape, Vx_window_horizontal_drag_shape;

/* The shape when over mouse-sensitive text.  */

Lisp_Object Vx_sensitive_text_pointer_shape;

#ifndef IDC_HAND
#define IDC_HAND MAKEINTRESOURCE(32649)
#endif

/* Color of chars displayed in cursor box.  */

Lisp_Object Vx_cursor_fore_pixel;

/* Nonzero if using Windows.  */

static int w32_in_use;

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */

Lisp_Object Vx_pixel_size_width_font_regexp;

/* Alist of bdf fonts and the files that define them.  */
Lisp_Object Vw32_bdf_filename_alist;

/* A flag to control whether fonts are matched strictly or not.  */
static int w32_strict_fontnames;

/* A flag to control whether we should only repaint if GetUpdateRect
   indicates there is an update region.  */
static int w32_strict_painting;

/* Associative list linking character set strings to Windows codepages. */
static Lisp_Object Vw32_charset_info_alist;

/* VIETNAMESE_CHARSET is not defined in some versions of MSVC.  */
#ifndef VIETNAMESE_CHARSET
#define VIETNAMESE_CHARSET 163
#endif

Lisp_Object Qnone;
Lisp_Object Qsuppress_icon;
Lisp_Object Qundefined_color;
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

/* The ANSI codepage.  */
int w32_ansi_code_page;

/* Prefix for system colors.  */
#define SYSTEM_COLOR_PREFIX "System"
#define SYSTEM_COLOR_PREFIX_LEN (sizeof (SYSTEM_COLOR_PREFIX) - 1)

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

/* Multi-monitor API definitions that are not pulled from the headers
   since we are compiling for NT 4.  */
#ifndef MONITOR_DEFAULT_TO_NEAREST
#define MONITOR_DEFAULT_TO_NEAREST 2
#endif
/* MinGW headers define MONITORINFO unconditionally, but MSVC ones don't.
   To avoid a compile error on one or the other, redefine with a new name.  */
struct MONITOR_INFO
{
    DWORD   cbSize;
    RECT    rcMonitor;
    RECT    rcWork;
    DWORD   dwFlags;
};

typedef BOOL (WINAPI * TrackMouseEvent_Proc)
  (IN OUT LPTRACKMOUSEEVENT lpEventTrack);
typedef LONG (WINAPI * ImmGetCompositionString_Proc)
  (IN HIMC context, IN DWORD index, OUT LPVOID buffer, IN DWORD bufLen);
typedef HIMC (WINAPI * ImmGetContext_Proc) (IN HWND window);
typedef HMONITOR (WINAPI * MonitorFromPoint_Proc) (IN POINT pt, IN DWORD flags);
typedef BOOL (WINAPI * GetMonitorInfo_Proc)
  (IN HMONITOR monitor, OUT struct MONITOR_INFO* info);

TrackMouseEvent_Proc track_mouse_event_fn = NULL;
ClipboardSequence_Proc clipboard_sequence_fn = NULL;
ImmGetCompositionString_Proc get_composition_string_fn = NULL;
ImmGetContext_Proc get_ime_context_fn = NULL;
MonitorFromPoint_Proc monitor_from_point_fn = NULL;
GetMonitorInfo_Proc get_monitor_info_fn = NULL;

extern AppendMenuW_Proc unicode_append_menu;

/* Flag to selectively ignore WM_IME_CHAR messages.  */
static int ignore_ime_char = 0;

/* W95 mousewheel handler */
unsigned int msh_mousewheel = 0;

/* Timers */
#define MOUSE_BUTTON_ID	1
#define MOUSE_MOVE_ID	2
#define MENU_FREE_ID 3
#define HOURGLASS_ID 4
/* The delay (milliseconds) before a menu is freed after WM_EXITMENULOOP
   is received.  */
#define MENU_FREE_DELAY 1000
static unsigned menu_free_timer = 0;

/* The below are defined in frame.c.  */

extern Lisp_Object Vwindow_system_version;

#ifdef GLYPH_DEBUG
int image_cache_refcount, dpyinfo_refcount;
#endif


/* From w32term.c. */
extern int w32_num_mouse_buttons;
extern Lisp_Object Vw32_recognize_altgr;

extern HWND w32_system_caret_hwnd;

extern int w32_system_caret_height;
extern int w32_system_caret_x;
extern int w32_system_caret_y;
extern int w32_use_visible_system_caret;

static HWND w32_visible_system_caret_hwnd;

/* From w32menu.c  */
extern HMENU current_popup_menu;
static int menubar_in_use = 0;

/* From w32uniscribe.c  */
#ifdef USE_FONT_BACKEND
extern void syms_of_w32uniscribe ();
extern int uniscribe_available;
#endif

/* Function prototypes for hourglass support.  */
static void show_hourglass P_ ((struct frame *));
static void hide_hourglass P_ ((void));



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
    error ("Non-W32 frame used");
  return f;
}

/* Let the user specify a display with a frame.
   nil stands for the selected frame--or, if that is not a w32 frame,
   the first display on the list.  */

struct w32_display_info *
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
	error ("Non-W32 frame used");
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

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_W32_P (f) || FRAME_W32_DISPLAY_INFO (f) != dpyinfo)
	continue;

      if (FRAME_W32_WINDOW (f) == wdesc)
        return f;
    }
  return 0;
}


static Lisp_Object unwind_create_frame P_ ((Lisp_Object));
static Lisp_Object unwind_create_tip_frame P_ ((Lisp_Object));
static void my_create_window P_ ((struct frame *));
static void my_create_tip_window P_ ((struct frame *));

/* TODO: Native Input Method support; see x_create_im.  */
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
static void x_edge_detection P_ ((struct frame *, struct image *, Lisp_Object,
				  Lisp_Object));




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

  /* Get the bounds of the WM window.  */
  GetWindowRect (FRAME_W32_WINDOW (f), &rect);

  pt.x = 0;
  pt.y = 0;

  /* Convert (0, 0) in the client area to screen co-ordinates.  */
  ClientToScreen (FRAME_W32_WINDOW (f), &pt);

  /* Remember x_pixels_diff and y_pixels_diff.  */
  f->x_pixels_diff = pt.x - rect.left;
  f->y_pixels_diff = pt.y - rect.top;

  *xptr = rect.left;
  *yptr = rect.top;
}



DEFUN ("w32-define-rgb-color", Fw32_define_rgb_color,
       Sw32_define_rgb_color, 4, 4, 0,
       doc: /* Convert RGB numbers to a Windows color reference and associate with NAME.
This adds or updates a named color to `w32-color-map', making it
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

  XSETINT (rgb, RGB (XUINT (red), XUINT (green), XUINT (blue)));

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
Assign this value to `w32-color-map' to replace the existing color map.

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

  fp = fopen (SDATA (filename), "rt");
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

static Lisp_Object
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

static Lisp_Object
w32_color_map_lookup (colorname)
     char *colorname;
{
  Lisp_Object tail, ret = Qnil;

  BLOCK_INPUT;

  for (tail = Vw32_color_map; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem;

      elt = XCAR (tail);
      if (!CONSP (elt)) continue;

      tem = Fcar (elt);

      if (lstrcmpi (SDATA (tem), colorname) == 0)
	{
	  ret = Fcdr (elt);
	  break;
	}

      QUIT;
    }


  UNBLOCK_INPUT;

  return ret;
}


static void
add_system_logical_colors_to_map (system_colors)
     Lisp_Object *system_colors;
{
  HKEY colors_key;

  /* Other registry operations are done with input blocked.  */
  BLOCK_INPUT;

  /* Look for "Control Panel/Colors" under User and Machine registry
     settings.  */
  if (RegOpenKeyEx (HKEY_CURRENT_USER, "Control Panel\\Colors", 0,
		    KEY_READ, &colors_key) == ERROR_SUCCESS
      || RegOpenKeyEx (HKEY_LOCAL_MACHINE, "Control Panel\\Colors", 0,
		       KEY_READ, &colors_key) == ERROR_SUCCESS)
    {
      /* List all keys.  */
      char color_buffer[64];
      char full_name_buffer[MAX_PATH + SYSTEM_COLOR_PREFIX_LEN];
      int index = 0;
      DWORD name_size, color_size;
      char *name_buffer = full_name_buffer + SYSTEM_COLOR_PREFIX_LEN;

      name_size = sizeof (full_name_buffer) - SYSTEM_COLOR_PREFIX_LEN;
      color_size = sizeof (color_buffer);

      strcpy (full_name_buffer, SYSTEM_COLOR_PREFIX);

      while (RegEnumValueA (colors_key, index, name_buffer, &name_size,
			    NULL, NULL, color_buffer, &color_size)
	     == ERROR_SUCCESS)
	{
	  int r, g, b;
	  if (sscanf (color_buffer, " %u %u %u", &r, &g, &b) == 3)
	    *system_colors = Fcons (Fcons (build_string (full_name_buffer),
					   make_number (RGB (r, g, b))),
				    *system_colors);

	  name_size = sizeof (full_name_buffer) - SYSTEM_COLOR_PREFIX_LEN;
	  color_size = sizeof (color_buffer);
	  index++;
	}
      RegCloseKey (colors_key);
    }

  UNBLOCK_INPUT;
}


static Lisp_Object
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

      size = strlen (color);
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
	      if (!isxdigit (color[0]) || color[1] == 'x')
		break;
	      t = color[size];
	      color[size] = '\0';
	      value = strtoul (color, &end, 16);
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
		  XSETINT (ret, colorval);
		  return ret;
		}
	      color = end;
	    }
	}
    }
  else if (strnicmp (colorname, "rgb:", 4) == 0)
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
	  if (!isxdigit (color[0]) || color[1] == 'x')
	    break;
	  value = strtoul (color, &end, 16);
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
	      XSETINT (ret, colorval);
	      return ret;
	    }
	  if (*end != '/')
	    break;
	  color = end + 1;
	}
    }
  else if (strnicmp (colorname, "rgbi:", 5) == 0)
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

	  value = strtod (color, &end);
	  if (errno == ERANGE)
	    break;
	  if (value < 0.0 || value > 1.0)
	    break;
	  val = (UINT)(0x100 * value);
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
	      XSETINT (ret, colorval);
	      return ret;
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
      color_def->red = GetRValue (w32_color_ref) * 256;
      color_def->green = GetGValue (w32_color_ref) * 256;
      color_def->blue = GetBValue (w32_color_ref) * 256;

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

  if ((FRAME_W32_DISPLAY_INFO (f)->n_planes * FRAME_W32_DISPLAY_INFO (f)->n_cbits) == 1)
    return def;

  /* w32_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (w32_defined_color (f, SDATA (arg), &cdef, 1))
    return cdef.pixel;

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
  Cursor cursor, nontext_cursor, mode_cursor, hand_cursor;
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
      hand_cursor
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    hand_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_crosshair);

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
    XRecolorCursor (FRAME_W32_DISPLAY (f), hand_cursor,
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

  if (hand_cursor != f->output_data.w32->hand_cursor
      && f->output_data.w32->hand_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->hand_cursor);
  f->output_data.w32->hand_cursor = hand_cursor;

  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;

  update_face_from_frame_parameter (f, Qmouse_color, arg);
#endif /* TODO */
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
   F has a window.  */

void
x_set_border_pixel (f, pix)
     struct frame *f;
     int pix;
{

  f->output_data.w32->border_pixel = pix;

  if (FRAME_W32_WINDOW (f) != 0 && f->border_width > 0)
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
  else if (!NILP (arg) || NILP (oldval))
    return;

  f->icon_name = arg;

#if 0
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
#endif
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
  if (FRAME_W32_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
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
		   SDATA (f->name)))
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
      SetWindowText (FRAME_W32_WINDOW (f), SDATA (name));
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
   If NAME is nil, use the frame name as the title.  */

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
      SetWindowText (FRAME_W32_WINDOW (f), SDATA (name));
      UNBLOCK_INPUT;
    }
}


void x_set_scroll_bar_default_width (f)
     struct frame *f;
{
  int wid = FRAME_COLUMN_WIDTH (f);

  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = GetSystemMetrics (SM_CXVSCROLL);
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) +
				      wid - 1) / wid;
}


/* Subroutines of creating a frame.  */


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
  return x_get_arg (check_x_display_info (Qnil),
		    alist, param, attribute, class, type);
}


Cursor
w32_load_cursor (LPCTSTR name)
{
  /* Try first to load cursor from application resource.  */
  Cursor cursor = LoadImage ((HINSTANCE) GetModuleHandle (NULL),
			     name, IMAGE_CURSOR, 0, 0,
			     LR_DEFAULTCOLOR | LR_DEFAULTSIZE | LR_SHARED);
  if (!cursor)
    {
      /* Then try to load a shared predefined cursor.  */
      cursor = LoadImage (NULL, name, IMAGE_CURSOR, 0, 0,
			  LR_DEFAULTCOLOR | LR_DEFAULTSIZE | LR_SHARED);
    }
  return cursor;
}

extern LRESULT CALLBACK w32_wnd_proc ();

static BOOL
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
  wc.hCursor = w32_load_cursor (IDC_ARROW);
  wc.hbrBackground = NULL; /* GetStockObject (WHITE_BRUSH);  */
  wc.lpszMenuName = NULL;
  wc.lpszClassName = EMACS_CLASS;

  return (RegisterClass (&wc));
}

static HWND
w32_createscrollbar (f, bar)
     struct frame *f;
     struct scroll_bar * bar;
{
  return (CreateWindow ("SCROLLBAR", "", SBS_VERT | WS_CHILD | WS_VISIBLE,
			/* Position and size of scroll bar.  */
			XINT (bar->left) + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
                        XINT (bar->top),
			XINT (bar->width) - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
                        XINT (bar->height),
			FRAME_W32_WINDOW (f),
			NULL,
			hinst,
			NULL));
}

static void
w32_createwindow (f)
     struct frame *f;
{
  HWND hwnd;
  RECT rect;
  Lisp_Object top = Qunbound;
  Lisp_Object left = Qunbound;

  rect.left = rect.top = 0;
  rect.right = FRAME_PIXEL_WIDTH (f);
  rect.bottom = FRAME_PIXEL_HEIGHT (f);

  AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		    FRAME_EXTERNAL_MENU_BAR (f));

  /* Do first time app init */

  if (!hprevinst)
    {
      w32_init_class (hinst);
    }

  if (f->size_hint_flags & USPosition || f->size_hint_flags & PPosition)
    {
      XSETINT (left, f->left_pos);
      XSETINT (top, f->top_pos);
    }
  else if (EQ (left, Qunbound) && EQ (top, Qunbound))
    {
      /* When called with RES_TYPE_NUMBER, w32_get_arg will return zero
	 for anything that is not a number and is not Qunbound.  */
      left = w32_get_arg (Qnil, Qleft, "left", "Left", RES_TYPE_NUMBER);
      top = w32_get_arg (Qnil, Qtop, "top", "Top", RES_TYPE_NUMBER);
    }

  FRAME_W32_WINDOW (f) = hwnd
    = CreateWindow (EMACS_CLASS,
		    f->namebuf,
		    f->output_data.w32->dwStyle | WS_CLIPCHILDREN,
		    EQ (left, Qunbound) ? CW_USEDEFAULT : XINT (left),
		    EQ (top, Qunbound) ? CW_USEDEFAULT : XINT (top),
		    rect.right - rect.left,
		    rect.bottom - rect.top,
		    NULL,
		    NULL,
		    hinst,
		    NULL);

  if (hwnd)
    {
      SetWindowLong (hwnd, WND_FONTWIDTH_INDEX, FRAME_COLUMN_WIDTH (f));
      SetWindowLong (hwnd, WND_LINEHEIGHT_INDEX, FRAME_LINE_HEIGHT (f));
      SetWindowLong (hwnd, WND_BORDER_INDEX, FRAME_INTERNAL_BORDER_WIDTH (f));
      SetWindowLong (hwnd, WND_SCROLLBAR_INDEX, f->scroll_bar_actual_width);
      SetWindowLong (hwnd, WND_BACKGROUND_INDEX, FRAME_BACKGROUND_PIXEL (f));

      /* Enable drag-n-drop.  */
      DragAcceptFiles (hwnd, TRUE);

      /* Do this to discard the default setting specified by our parent. */
      ShowWindow (hwnd, SW_HIDE);

      /* Update frame positions. */
      GetWindowRect (hwnd, &rect);
      f->left_pos = rect.left;
      f->top_pos = rect.top;
    }
}

static void
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

static unsigned int
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
   but Emacs should grab instead.  Not directly visible to lisp, to
   simplify synchronization.  Each item is an integer encoding a virtual
   key code and modifier combination to capture.  */
static Lisp_Object w32_grabbed_keys;

#define HOTKEY(vk, mods)      make_number (((vk) & 255) | ((mods) << 8))
#define HOTKEY_ID(k)          (XFASTINT (k) & 0xbfff)
#define HOTKEY_VK_CODE(k)     (XFASTINT (k) & 255)
#define HOTKEY_MODIFIERS(k)   (XFASTINT (k) >> 8)

#define RAW_HOTKEY_ID(k)        ((k) & 0xbfff)
#define RAW_HOTKEY_VK_CODE(k)   ((k) & 255)
#define RAW_HOTKEY_MODIFIERS(k) ((k) >> 8)

/* Register hot-keys for reserved key combinations when Emacs has
   keyboard focus, since this is the only way Emacs can receive key
   combinations like Alt-Tab which are used by the system.  */

static void
register_hot_keys (hwnd)
     HWND hwnd;
{
  Lisp_Object keylist;

  /* Use CONSP, since we are called asynchronously.  */
  for (keylist = w32_grabbed_keys; CONSP (keylist); keylist = XCDR (keylist))
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

  for (keylist = w32_grabbed_keys; CONSP (keylist); keylist = XCDR (keylist))
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
              /* Initialize COM for this window. Even though we don't use it,
                 some third party shell extensions can cause it to be used in
                 system dialogs, which causes a crash if it is not initialized.
                 This is a known bug in Windows, which was fixed long ago, but
                 the patch for XP is not publically available until XP SP3,
                 and older versions will never be patched.  */
              CoInitialize (NULL);
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
				RAW_HOTKEY_ID (msg.wParam),
				RAW_HOTKEY_MODIFIERS (msg.wParam),
				RAW_HOTKEY_VK_CODE (msg.wParam));
	      /* Reply is not expected.  */
	      break;
	    case WM_EMACS_UNREGISTER_HOT_KEY:
	      focus_window = GetFocus ();
	      if (focus_window != NULL)
		UnregisterHotKey (focus_window, RAW_HOTKEY_ID (msg.wParam));
	      /* Mark item as erased.  NB: this code must be
                 thread-safe.  The next line is okay because the cons
                 cell is never made into garbage and is not relocated by
                 GC.  */
	      XSETCAR ((Lisp_Object) ((EMACS_INT) msg.lParam), Qnil);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
		abort ();
	      break;
	    case WM_EMACS_TOGGLE_LOCK_KEY:
	      {
		int vk_code = (int) msg.wParam;
		int cur_state = (GetKeyState (vk_code) & 1);
		Lisp_Object new_state = (Lisp_Object) ((EMACS_INT) msg.lParam);

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
#ifdef MSG_DEBUG
              /* Broadcast messages make it here, so you need to be looking
                 for something in particular for this to be useful.  */
	    default:
	      DebPrint (("msg %x not expected by w32_msg_pump\n", msg.message));
#endif
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
    /* Message may have been cancelled, so don't abort.  */
    return;

  msg_buf->result = result;
  msg_buf->completed = 1;

  /* Ensure input thread is woken so it notices the completion.  */
  PostThreadMessage (dwWindowsThreadId, WM_NULL, 0, 0);
}

static void
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

DWORD WINAPI
w32_msg_worker (void *arg)
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

  /* This is the initial message loop which should only exit when the
     application quits.  */
  w32_msg_pump (&dummy_buf);

  return 0;
}

static void
signal_user_input ()
{
  /* Interrupt any lisp that wants to be interrupted by input.  */
  if (!NILP (Vthrow_on_input))
    {
      Vquit_flag = Vthrow_on_input;
      /* If we're inside a function that wants immediate quits,
	 do it now.  */
      if (immediate_quit && NILP (Vinhibit_quit))
	{
	  immediate_quit = 0;
	  QUIT;
	}
    }
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
     woken up if blocked in sys_select, but we do NOT want to post
     the quit_char message itself (because it will usually be as if
     the user had typed quit_char twice).  Instead, we post a dummy
     message that has no particular effect. */
  {
    int c = wParam;
    if (isalpha (c) && wmsg.dwModifiers == ctrl_modifier)
      c = make_ctrl_char (c) & 0377;
    if (c == quit_char
	|| (wmsg.dwModifiers == 0 &&
	    w32_quit_key && wParam == w32_quit_key))
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
    else
      signal_user_input ();
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
	GetClientRect (hwnd, &wmsg.rect);
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
	  if (wParam < 256 && lispy_function_keys[wParam])
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
	  if (wParam > 255 || !lispy_function_keys[wParam])
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
			    (hwnd, WM_CHAR,
                             (unsigned char) key.uChar.AsciiChar, lParam,
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

    case WM_UNICHAR:
      /* WM_UNICHAR looks promising from the docs, but the exact
         circumstances in which TranslateMessage sends it is one of those
         Microsoft secret API things that EU and US courts are supposed
         to have put a stop to already. Spy++ shows it being sent to Notepad
         and other MS apps, but never to Emacs.

         Some third party IMEs send it in accordance with the official
         documentation though, so handle it here.

         UNICODE_NOCHAR is used to test for support for this message.
         TRUE indicates that the message is supported.  */
      if (wParam == UNICODE_NOCHAR)
        return TRUE;

      {
        W32Msg wmsg;
        wmsg.dwModifiers = w32_get_key_modifiers (wParam, lParam);
        signal_user_input ();
        my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      }
      break;

    case WM_IME_CHAR:
      /* If we can't get the IME result as unicode, use default processing,
         which will at least allow characters decodable in the system locale
         get through.  */
      if (!get_composition_string_fn)
        goto dflt;

      else if (!ignore_ime_char)
        {
          wchar_t * buffer;
          int size, i;
          W32Msg wmsg;
          HIMC context = get_ime_context_fn (hwnd);
          wmsg.dwModifiers = w32_get_key_modifiers (wParam, lParam);
          /* Get buffer size.  */
          size = get_composition_string_fn (context, GCS_RESULTSTR, buffer, 0);
          buffer = alloca(size);
          size = get_composition_string_fn (context, GCS_RESULTSTR,
                                            buffer, size);
          signal_user_input ();
          for (i = 0; i < size / sizeof (wchar_t); i++)
            {
              my_post_msg (&wmsg, hwnd, WM_UNICHAR, (WPARAM) buffer[i],
                           lParam);
            }
          /* We output the whole string above, so ignore following ones
             until we are notified of the end of composition.  */
          ignore_ime_char = 1;
        }
      break;

    case WM_IME_ENDCOMPOSITION:
      ignore_ime_char = 0;
      goto dflt;

      /* Simulate middle mouse button events when left and right buttons
	 are used together, but only if user has two button mouse. */
    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
      if (w32_num_mouse_buttons > 2)
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
	    signal_user_input ();

	    /* Clear message buffer. */
	    saved_mouse_button_msg.msg.hwnd = 0;
	  }
	else
	  {
	    /* Hold onto message for now. */
	    mouse_button_timer =
	      SetTimer (hwnd, MOUSE_BUTTON_ID,
			w32_mouse_button_tolerance, NULL);
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
      if (w32_num_mouse_buttons > 2)
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
	signal_user_input ();

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

	/* Ignore middle and extra buttons as long as the menu is active.  */
	f = x_window_to_frame (dpyinfo, hwnd);
	if (f && f->output_data.w32->menubar_active)
	  return 0;

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
      signal_user_input ();

      /* Need to return true for XBUTTON messages, false for others,
         to indicate that we processed the message.  */
      return (msg == WM_XBUTTONDOWN || msg == WM_XBUTTONUP);

    case WM_MOUSEMOVE:
      /* Ignore mouse movements as long as the menu is active.  These
	 movements are processed by the window manager anyway, and
	 it's wrong to handle them as if they happened on the
	 underlying frame.  */
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f && f->output_data.w32->menubar_active)
	return 0;

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
      if (w32_mouse_move_interval <= 0
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
		    w32_mouse_move_interval, NULL);

      /* Hold onto message for now. */
      saved_mouse_move_msg.msg.hwnd = hwnd;
      saved_mouse_move_msg.msg.message = msg;
      saved_mouse_move_msg.msg.wParam = wParam;
      saved_mouse_move_msg.msg.lParam = lParam;
      saved_mouse_move_msg.msg.time = GetMessageTime ();
      saved_mouse_move_msg.dwModifiers = w32_get_modifiers ();

      return 0;

    case WM_MOUSEWHEEL:
    case WM_DROPFILES:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      signal_user_input ();
      return 0;

    case WM_APPCOMMAND:
      if (w32_pass_multimedia_buttons_to_system)
        goto dflt;
      /* Otherwise, pass to lisp, the same way we do with mousehwheel.  */
    case WM_MOUSEHWHEEL:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      signal_user_input ();
      /* Non-zero must be returned when WM_MOUSEHWHEEL messages are
         handled, to prevent the system trying to handle it by faking
         scroll bar events.  */
      return 1;

    case WM_TIMER:
      /* Flush out saved messages if necessary. */
      if (wParam == mouse_button_timer)
	{
	  if (saved_mouse_button_msg.msg.hwnd)
	    {
	      post_msg (&saved_mouse_button_msg);
	      signal_user_input ();
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
          /* If a popup menu is active, don't wipe its strings.  */
	  if (menubar_in_use
              && current_popup_menu == NULL)
	    {
	      /* Free memory used by owner-drawn and help-echo strings.  */
	      w32_free_menu_strings (hwnd);
	      f->output_data.w32->menubar_active = 0;
              menubar_in_use = 0;
	    }
	}
      else if (wParam == hourglass_timer)
	{
	  KillTimer (hwnd, hourglass_timer);
	  hourglass_timer = 0;
	  show_hourglass (x_window_to_frame (dpyinfo, hwnd));
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

        menubar_in_use = 1;

	return send_deferred_msg (&msg_buf, hwnd, msg, wParam, lParam);
      }

    case WM_EXITMENULOOP:
      f = x_window_to_frame (dpyinfo, hwnd);

      /* If a menu is still active, check again after a short delay,
	 since Windows often (always?) sends the WM_EXITMENULOOP
	 before the corresponding WM_COMMAND message.
         Don't do this if a popup menu is active, since it is only
         menubar menus that require cleaning up in this way.
      */
      if (f && menubar_in_use && current_popup_menu == NULL)
	menu_free_timer = SetTimer (hwnd, MENU_FREE_ID, MENU_FREE_DELAY, NULL);

      /* If hourglass cursor should be displayed, display it now.  */
      if (f && f->output_data.w32->hourglass_p)
	SetCursor (f->output_data.w32->hourglass_cursor);

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
		  if (unicode_append_menu)
		    GetTextExtentPoint32W (hdc, (WCHAR *) title,
					   wcslen ((WCHAR *) title),
					   &size);
		  else
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
		  if (unicode_append_menu)
		    ExtTextOutW (hdc,
				 pDis->rcItem.left
				 + GetSystemMetrics (SM_CXMENUCHECK),
				 pDis->rcItem.top,
				 ETO_OPAQUE, &pDis->rcItem,
				 (WCHAR *) title,
				 wcslen ((WCHAR *) title), NULL);
		  else
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
      menubar_in_use = 0;
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f && HIWORD (wParam) == 0)
	{
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

    case WM_DESTROY:
      CoUninitialize ();
      return 0;

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

	    wp.length = sizeof (wp);
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

		int cx_mintrack = GetSystemMetrics (SM_CXMINTRACK);
		int cy_mintrack = GetSystemMetrics (SM_CYMINTRACK);

		lppos->cx = max (lppos->cx - wdiff, cx_mintrack);
		lppos->cy = max (lppos->cy - hdiff, cy_mintrack);

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
      /* Hack to allow resizing the Emacs frame above the screen size.
	 Note that Windows 9x limits coordinates to 16-bits.  */
      ((LPMINMAXINFO) lParam)->ptMaxTrackSize.x = 32767;
      ((LPMINMAXINFO) lParam)->ptMaxTrackSize.y = 32767;
      return 0;

    case WM_SETCURSOR:
      if (LOWORD (lParam) == HTCLIENT)
	{
	  f = x_window_to_frame (dpyinfo, hwnd);
	  if (f->output_data.w32->hourglass_p && !menubar_in_use
	      && !current_popup_menu)
	    SetCursor (f->output_data.w32->hourglass_cursor);
	  else
	    SetCursor (f->output_data.w32->current_cursor);
	  return 0;
	}
      goto dflt;

    case WM_EMACS_SETCURSOR:
      {
	Cursor cursor = (Cursor) wParam;
	f = x_window_to_frame (dpyinfo, hwnd);
	if (f && cursor)
	  {
	    f->output_data.w32->current_cursor = cursor;
	    if (!f->output_data.w32->hourglass_p)
	      SetCursor (cursor);
	  }
	return 0;
      }

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
	  signal_user_input ();
	  return 0;
	}

    dflt:
      return DefWindowProc (hwnd, msg, wParam, lParam);
    }


  /* The most common default return code for handled messages is 0.  */
  return 0;
}

static void
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
static void
my_create_tip_window (f)
     struct frame *f;
{
  RECT rect;

  rect.left = rect.top = 0;
  rect.right = FRAME_PIXEL_WIDTH (f);
  rect.bottom = FRAME_PIXEL_HEIGHT (f);

  AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		    FRAME_EXTERNAL_MENU_BAR (f));

  tip_window = FRAME_W32_WINDOW (f)
    = CreateWindow (EMACS_CLASS,
		    f->namebuf,
		    f->output_data.w32->dwStyle,
		    f->left_pos,
		    f->top_pos,
		    rect.right - rect.left,
		    rect.bottom - rect.top,
		    FRAME_W32_WINDOW (SELECTED_FRAME ()), /* owner */
		    NULL,
		    hinst,
		    NULL);

  if (tip_window)
    {
      SetWindowLong (tip_window, WND_FONTWIDTH_INDEX, FRAME_COLUMN_WIDTH (f));
      SetWindowLong (tip_window, WND_LINEHEIGHT_INDEX, FRAME_LINE_HEIGHT (f));
      SetWindowLong (tip_window, WND_BORDER_INDEX, FRAME_INTERNAL_BORDER_WIDTH (f));
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
    char *str = (char *) SDATA (Vx_resource_name);
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

  x_text_icon (f, (char *) SDATA ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name)));
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
  gc_values.font = FRAME_FONT (f);

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

#ifdef USE_FONT_BACKEND
static void
x_default_font_parameter (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  Lisp_Object font = x_get_arg (dpyinfo, parms, Qfont, "font", "Font",
                                RES_TYPE_STRING);

  if (!STRINGP (font))
    {
      int i;
      static char *names[]
        = { "Courier New-10",
            "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1",
            "-*-Fixedsys-normal-r-*-*-12-*-*-*-c-*-iso8859-1",
            "Fixedsys",
            NULL };

      for (i = 0; names[i]; i++)
        {
          font = font_open_by_name (f, names[i]);
          if (! NILP (font))
            break;
        }
      if (NILP (font))
        error ("No suitable font was found");
    }
  x_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}
#endif

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new window, which is called a \"frame\" in Emacs terms.
Return an Emacs frame object.
PARAMETERS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use,
then `default-minibuffer-frame' must be a frame whose minibuffer can
be shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
  (parameters)
     Lisp_Object parameters;
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
  struct w32_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;

  check_w32 ();

  /* Make copy of frame parameters because the original is in pure
     storage now. */
  parameters = Fcopy_alist (parameters);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = w32_get_arg (parameters, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
#ifdef MULTI_KBOARD
  kb = dpyinfo->terminal->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = w32_get_arg (parameters, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = w32_get_arg (parameters, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parameters, parent, name, frame);
  tem = w32_get_arg (parameters, Qminibuffer, "minibuffer", "Minibuffer",
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
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = GetSystemMetrics (SM_CXVSCROLL);

  f->terminal = dpyinfo->terminal;
  f->terminal->reference_count++;

  f->output_method = output_w32;
  f->output_data.w32 =
    (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));
  FRAME_FONTSET (f) = -1;
  record_unwind_protect (unwind_create_frame, frame);

  f->icon_name
    = w32_get_arg (parameters, Qicon_name, "iconName", "Title", RES_TYPE_STRING);
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

  f->resx = dpyinfo->resx;
  f->resy = dpyinfo->resy;

#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    {
      /* Perhaps, we must allow frame parameter, say `font-backend',
	 to specify which font backends to use.  */
      if (uniscribe_available)
	register_font_driver (&uniscribe_font_driver, f);
      register_font_driver (&w32font_driver, f);

      x_default_parameter (f, parameters, Qfont_backend, Qnil,
			   "fontBackend", "FontBackend", RES_TYPE_STRING);
    }
#endif	/* USE_FONT_BACKEND */

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    x_default_font_parameter (f, parameters);
  else
#endif
  {
    Lisp_Object font;

    font = w32_get_arg (parameters, Qfont, "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
        tem = Fquery_fontset (font, Qnil);
        if (STRINGP (tem))
          font = x_new_fontset (f, tem);
        else
          font = x_new_font (f, SDATA (font));
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

    x_default_parameter (f, parameters, Qfont, font,
			 "font", "Font", RES_TYPE_STRING);
  }

  x_default_parameter (f, parameters, Qborder_width, make_number (2),
		       "borderWidth", "BorderWidth", RES_TYPE_NUMBER);
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parameters)))
    {
      Lisp_Object value;

      value = w32_get_arg (parameters, Qinternal_border_width,
                           "internalBorder", "InternalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parameters = Fcons (Fcons (Qinternal_border_width, value),
                            parameters);
    }
  /* Default internalBorderWidth to 0 on Windows to match other programs.  */
  x_default_parameter (f, parameters, Qinternal_border_width, make_number (0),
		       "internalBorderWidth", "InternalBorder", RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qvertical_scroll_bars, Qright,
		       "verticalScrollBars", "ScrollBars", RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parameters, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qcursor_color, build_string ("black"),
		       "cursorColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qscreen_gamma, Qnil,
		       "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  x_default_parameter (f, parameters, Qline_spacing, Qnil,
		       "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qleft_fringe, Qnil,
		       "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qright_fringe, Qnil,
		       "rightFringe", "RightFringe", RES_TYPE_NUMBER);


  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);

  x_default_parameter (f, parameters, Qmenu_bar_lines, make_number (1),
		       "menuBar", "MenuBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qtool_bar_lines, make_number (1),
                       "toolBar", "ToolBar", RES_TYPE_NUMBER);

  x_default_parameter (f, parameters, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parameters, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  f->output_data.w32->dwStyle = WS_OVERLAPPEDWINDOW;
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;

  f->output_data.w32->text_cursor = w32_load_cursor (IDC_IBEAM);
  f->output_data.w32->nontext_cursor = w32_load_cursor (IDC_ARROW);
  f->output_data.w32->modeline_cursor = w32_load_cursor (IDC_ARROW);
  f->output_data.w32->hand_cursor = w32_load_cursor (IDC_HAND);
  f->output_data.w32->hourglass_cursor = w32_load_cursor (IDC_WAIT);
  f->output_data.w32->horizontal_drag_cursor = w32_load_cursor (IDC_SIZEWE);

  f->output_data.w32->current_cursor = f->output_data.w32->nontext_cursor;

  window_prompting = x_figure_window_size (f, parameters, 1);

  tem = w32_get_arg (parameters, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  w32_window (f, window_prompting, minibuffer_only);
  x_icon (f, parameters);

  x_make_gc (f);

  /* Now consider the frame official.  */
  FRAME_W32_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the window, so that the
     icon-creation functions can say whose icon they're describing.  */
  x_default_parameter (f, parameters, Qicon_type, Qnil,
		       "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);

  x_default_parameter (f, parameters, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parameters, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parameters, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);
  x_default_parameter (f, parameters, Qscroll_bar_width, Qnil,
		       "scrollBarWidth", "ScrollBarWidth", RES_TYPE_NUMBER);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);

  FRAME_LINES (f) = 0;
  SET_FRAME_COLS (f, 0);
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
  if (! f->output_data.w32->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = w32_get_arg (parameters, Qvisibility, 0, 0, RES_TYPE_SYMBOL);
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

  /* Initialize `default-minibuffer-frame' in case this is the first
     frame on this terminal.  */
  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (kb->Vdefault_minibuffer_frame)
          || !FRAME_LIVE_P (XFRAME (kb->Vdefault_minibuffer_frame))))
    kb->Vdefault_minibuffer_frame = frame;

  /* All remaining specified parameters, which have not been "used"
     by x_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parameters; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      f->param_alist = Fcons (XCAR (tem), f->param_alist);

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

DEFUN ("x-focus-frame", Fx_focus_frame, Sx_focus_frame, 1, 1, 0,
       doc: /* Give FRAME input focus, raising to foreground if necessary.  */)
  (frame)
     Lisp_Object frame;
{
  x_focus_on_frame (check_x_frame (frame));
  return Qnil;
}


/* Return the charset portion of a font name.  */
char *
xlfd_charset_of_font (char * fontname)
{
  char *charset, *encoding;

  encoding = strrchr (fontname, '-');
  if (!encoding || encoding == fontname)
    return NULL;

  for (charset = encoding - 1; charset >= fontname; charset--)
    if (*charset == '-')
      break;

  if (charset == fontname || strcmp (charset, "-*-*") == 0)
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
w32_load_system_font (f, fontname, size)
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
                           SDATA (XCAR (tail)))
                  || !strcmp (dpyinfo->font_table[i].full_name,
                              SDATA (XCAR (tail)))))
	    return (dpyinfo->font_table + i);

      fontname = (char *) SDATA (XCAR (font_names));
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

    lf.lfQuality = DEFAULT_QUALITY;

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
	    /* font->double_byte_p = GetFontLanguageInfo (hdc) & GCP_DBCS; */
	    CPINFO cpi = {0};
	    GetCPInfo (codepage, &cpi);
	    font->double_byte_p = cpi.MaxCharSize > 1;
	  }

	SelectObject (hdc, oldobj);
	ReleaseDC (dpyinfo->root_window, hdc);
        /* Fill out details in lf according to the font that was
           actually loaded.  */
        lf.lfHeight = font->tm.tmInternalLeading - font->tm.tmHeight;
        lf.lfWidth = font->tm.tmMaxCharWidth;
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
    bzero (fontp, sizeof (*fontp));
    fontp->font = font;
    fontp->font_idx = i;
    fontp->name = (char *) xmalloc (strlen (fontname) + 1);
    bcopy (fontname, fontp->name, strlen (fontname) + 1);

    if ((lf.lfPitchAndFamily & 0x03) == FIXED_PITCH)
      {
	/* Fixed width font.  */
	fontp->average_width = fontp->space_width = FONT_AVG_WIDTH (font);
      }
    else
      {
	wchar_t space = 32;
	XCharStruct* pcm;
	pcm = w32_per_char_metric (font, &space, ANSI_FONT);
	if (pcm)
	  fontp->space_width = pcm->width;
	else
	  fontp->space_width = FONT_AVG_WIDTH (font);

	fontp->average_width = font->tm.tmAveCharWidth;
      }

    fontp->charset = -1;
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
       uses this font.  So, we set information in fontp->encoding_type
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */

    /* SJIS fonts need to be set to type 4, all others seem to work as
       type FONT_ENCODING_NOT_DECIDED.  */
    encoding = strrchr (fontp->name, '-');
    if (encoding && strnicmp (encoding+1, "sjis", 4) == 0)
      fontp->encoding_type = 4;
    else
      fontp->encoding_type = FONT_ENCODING_NOT_DECIDED;

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
w32_load_font (f, fontname, size)
     struct frame *f;
     char * fontname;
     int size;
{
  Lisp_Object bdf_fonts;
  struct font_info *retval = NULL;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  bdf_fonts = w32_list_bdf_fonts (build_string (fontname), 1);

  while (!retval && CONSP (bdf_fonts))
    {
      char *bdf_name, *bdf_file;
      Lisp_Object bdf_pair;
      int i;

      bdf_name = SDATA (XCAR (bdf_fonts));
      bdf_pair = Fassoc (XCAR (bdf_fonts), Vw32_bdf_filename_alist);
      bdf_file = SDATA (XCDR (bdf_pair));

      /* If the font is already loaded, do not load it again.  */
      for (i = 0; i < dpyinfo->n_fonts; i++)
	{
	  if ((dpyinfo->font_table[i].name
	       && !strcmp (dpyinfo->font_table[i].name, bdf_name))
	      || (dpyinfo->font_table[i].full_name
		  && !strcmp (dpyinfo->font_table[i].full_name, bdf_name)))
	    return dpyinfo->font_table + i;
	}

      retval = w32_load_bdf_font (f, bdf_name, size, bdf_file);

      bdf_fonts = XCDR (bdf_fonts);
    }

  if (retval)
    return retval;

  return w32_load_system_font (f, fontname, size);
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

      if (font->hfont) DeleteObject (font->hfont);
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

  if (stricmp (lpw, "heavy") == 0)             return FW_HEAVY;
  else if (stricmp (lpw, "extrabold") == 0)    return FW_EXTRABOLD;
  else if (stricmp (lpw, "bold") == 0)         return FW_BOLD;
  else if (stricmp (lpw, "demibold") == 0)     return FW_SEMIBOLD;
  else if (stricmp (lpw, "semibold") == 0)     return FW_SEMIBOLD;
  else if (stricmp (lpw, "medium") == 0)       return FW_MEDIUM;
  else if (stricmp (lpw, "normal") == 0)       return FW_NORMAL;
  else if (stricmp (lpw, "light") == 0)        return FW_LIGHT;
  else if (stricmp (lpw, "extralight") == 0)   return FW_EXTRALIGHT;
  else if (stricmp (lpw, "thin") == 0)         return FW_THIN;
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

LONG
x_to_w32_charset (lpcs)
    char * lpcs;
{
  Lisp_Object this_entry, w32_charset;
  char *charset;
  int len = strlen (lpcs);

  /* Support "*-#nnn" format for unknown charsets.  */
  if (strncmp (lpcs, "*-#", 3) == 0)
    return atoi (lpcs + 3);

  /* All Windows fonts qualify as unicode.  */
  if (!strncmp (lpcs, "iso10646", 8))
    return DEFAULT_CHARSET;

  /* Handle wildcards by ignoring them; eg. treat "big5*-*" as "big5".  */
  charset = alloca (len + 1);
  strcpy (charset, lpcs);
  lpcs = strchr (charset, '*');
  if (lpcs)
    *lpcs = '\0';

  /* Look through w32-charset-info-alist for the character set.
     Format of each entry is
       (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
  */
  this_entry = Fassoc (build_string (charset), Vw32_charset_info_alist);

  if (NILP (this_entry))
    {
      /* At startup, we want iso8859-1 fonts to come up properly. */
      if (stricmp (charset, "iso8859-1") == 0)
        return ANSI_CHARSET;
      else
        return DEFAULT_CHARSET;
    }

  w32_charset = Fcar (Fcdr (this_entry));

  /* Translate Lisp symbol to number.  */
  if (EQ (w32_charset, Qw32_charset_ansi))
    return ANSI_CHARSET;
  if (EQ (w32_charset, Qw32_charset_symbol))
    return SYMBOL_CHARSET;
  if (EQ (w32_charset, Qw32_charset_shiftjis))
    return SHIFTJIS_CHARSET;
  if (EQ (w32_charset, Qw32_charset_hangeul))
    return HANGEUL_CHARSET;
  if (EQ (w32_charset, Qw32_charset_chinesebig5))
    return CHINESEBIG5_CHARSET;
  if (EQ (w32_charset, Qw32_charset_gb2312))
    return GB2312_CHARSET;
  if (EQ (w32_charset, Qw32_charset_oem))
    return OEM_CHARSET;
#ifdef JOHAB_CHARSET
  if (EQ (w32_charset, Qw32_charset_johab))
    return JOHAB_CHARSET;
  if (EQ (w32_charset, Qw32_charset_easteurope))
    return EASTEUROPE_CHARSET;
  if (EQ (w32_charset, Qw32_charset_turkish))
    return TURKISH_CHARSET;
  if (EQ (w32_charset, Qw32_charset_baltic))
    return BALTIC_CHARSET;
  if (EQ (w32_charset, Qw32_charset_russian))
    return RUSSIAN_CHARSET;
  if (EQ (w32_charset, Qw32_charset_arabic))
    return ARABIC_CHARSET;
  if (EQ (w32_charset, Qw32_charset_greek))
    return GREEK_CHARSET;
  if (EQ (w32_charset, Qw32_charset_hebrew))
    return HEBREW_CHARSET;
  if (EQ (w32_charset, Qw32_charset_vietnamese))
    return VIETNAMESE_CHARSET;
  if (EQ (w32_charset, Qw32_charset_thai))
    return THAI_CHARSET;
  if (EQ (w32_charset, Qw32_charset_mac))
    return MAC_CHARSET;
#endif /* JOHAB_CHARSET */
#ifdef UNICODE_CHARSET
  if (EQ (w32_charset, Qw32_charset_unicode))
    return UNICODE_CHARSET;
#endif

  return DEFAULT_CHARSET;
}


char *
w32_to_x_charset (fncharset, matching)
    int fncharset;
    char *matching;
{
  static char buf[32];
  Lisp_Object charset_type;
  int match_len = 0;

  if (matching)
    {
      /* If fully specified, accept it as it is.  Otherwise use a
	 substring match. */
      char *wildcard = strchr (matching, '*');
      if (wildcard)
	*wildcard = '\0';
      else if (strchr (matching, '-'))
	return matching;

      match_len = strlen (matching);
    }

  switch (fncharset)
    {
    case ANSI_CHARSET:
      /* Handle startup case of w32-charset-info-alist not
         being set up yet. */
      if (NILP (Vw32_charset_info_alist))
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
    int matching_found = 0;

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

        x_charset = SDATA (XCAR (this_entry));
        w32_charset = XCAR (XCDR (this_entry));
        codepage = XCDR (XCDR (this_entry));

        /* Look for Same charset and a valid codepage (or non-int
           which means ignore).  */
        if (EQ (w32_charset, charset_type)
            && (!INTEGERP (codepage) || XINT (codepage) == CP_DEFAULT
                || IsValidCodePage (XINT (codepage))))
          {
            /* If we don't have a match already, then this is the
               best.  */
            if (!best_match)
	      {
		best_match = x_charset;
		if (matching && !strnicmp (x_charset, matching, match_len))
		  matching_found = 1;
	      }
	    /* If we already found a match for MATCHING, then
	       only consider other matches.  */
	    else if (matching_found
		     && strnicmp (x_charset, matching, match_len))
	      continue;
	    /* If this matches what we want, and the best so far doesn't,
	       then this is better.  */
	    else if (!matching_found && matching
		     && !strnicmp (x_charset, matching, match_len))
	      {
		best_match = x_charset;
		matching_found = 1;
	      }
	    /* If this is fully specified, and the best so far isn't,
	       then this is better.  */
	    else if ((!strchr (best_match, '-') && strchr (x_charset, '-'))
	    /* If this is an ISO codepage, and the best so far isn't,
	       then this is better, but only if it fully specifies the
	       encoding.  */
		|| (strnicmp (best_match, "iso", 3) != 0
		    && strnicmp (x_charset, "iso", 3) == 0
		    && strchr (x_charset, '-')))
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

    strncpy (buf, best_match, 31);
    /* If the charset is not fully specified, put -0 on the end.  */
    if (!strchr (best_match, '-'))
      {
	int pos = strlen (best_match);
	/* Charset specifiers shouldn't be very long.  If it is a made
	   up one, truncating it should not do any harm since it isn't
	   recognized anyway.  */
	if (pos > 29)
	  pos = 29;
	strcpy (buf + pos, "-0");
      }
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
      if (NILP (Vw32_charset_info_alist))
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
       Only return fully specified charsets for codepages which are
       installed.

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

	if (!strchr (SDATA (x_charset), '-'))
	  continue;

        /* Look for Same charset and a valid codepage (or non-int
           which means ignore).  */
        if (EQ (w32_charset, charset_type)
            && (!INTEGERP (codepage) || XINT (codepage) == CP_DEFAULT
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

  if (!strcmp (charset, "iso10646"))
    return CP_UNICODE;

  if (NILP (Vw32_charset_info_alist))
    return CP_DEFAULT;

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
  /* We explicitely disable composition handling because selection
     data should not contain any composition sequence.  */
  coding.common_flags &= ~CODING_ANNOTATION_MASK;

  coding.dst_bytes = LF_FACESIZE * 2;
  coding.destination = (unsigned char *) xmalloc (coding.dst_bytes + 1);
  decode_coding_c_string (&coding, lplogfont->lfFaceName,
			  strlen(lplogfont->lfFaceName), Qnil);
  fontname = coding.destination;

  *(fontname + coding.produced) = '\0';

  /* Replace dashes with underscores so the dashes are not
     misinterpreted.  */
  fontname_dash = fontname;
  while (fontname_dash = strchr (fontname_dash, '-'))
      *fontname_dash = '_';

  if (lplogfont->lfHeight)
    {
      sprintf (height_pixels, "%u", eabs (lplogfont->lfHeight));
      sprintf (height_dpi, "%u",
	       eabs (lplogfont->lfHeight) * 720 / display_resy);
    }
  else
    {
      strcpy (height_pixels, "*");
      strcpy (height_dpi, "*");
    }

#if 0 /* Never put the width in the xfld. It fails on fonts with
	 double-width characters.  */
  if (lplogfont->lfWidth)
    sprintf (width_pixels, "%u", lplogfont->lfWidth * 10);
  else
#endif
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
             w32_to_x_charset (lplogfont->lfCharSet, specific_charset)
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
	  Lisp_Object string = build_string (name);
          setup_coding_system
            (Fcheck_coding_system (Vlocale_coding_system), &coding);
          coding.mode |= (CODING_MODE_SAFE_ENCODING | CODING_MODE_LAST_BLOCK);
	  /* Disable composition/charset annotation.   */
	  coding.common_flags &= ~CODING_ANNOTATION_MASK;
	  coding.dst_bytes = SCHARS (string) * 2;

	  coding.destination = (unsigned char *) xmalloc (coding.dst_bytes);
          encode_coding_object (&coding, string, 0, 0,
				SCHARS (string), SBYTES (string), Qnil);
	  if (coding.produced >= LF_FACESIZE)
	    coding.produced = LF_FACESIZE - 1;

	  coding.destination[coding.produced] = '\0';

	  strcpy (lplogfont->lfFaceName, coding.destination);
	  xfree (coding.destination);
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
        {
          if (pitch == 'p')
            lplogfont->lfPitchAndFamily = VARIABLE_PITCH | FF_DONTCARE;
          else if (pitch == 'c')
            lplogfont->lfPitchAndFamily = FIXED_PITCH | FF_DONTCARE;
        }

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
	  strncpy (lplogfont->lfFaceName, name, LF_FACESIZE);
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
  lplogfont->lfHeight = - eabs (lplogfont->lfHeight);

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
  char *ptr;
  char *font_name_copy;
  char *regex = alloca (strlen (pattern) * 2 + 3);

  font_name_copy = alloca (strlen (fontname) + 1);
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

  return (fast_string_match_ignore_case (build_string (regex),
                                         build_string (font_name_copy)) >= 0);
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
  Lisp_Object pattern;
  Lisp_Object list;
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

  if (FontType == RASTER_FONTTYPE)
    {
      /* DBCS raster fonts have problems displaying, so skip them.  */
      int charset = lplf->elfLogFont.lfCharSet;
      if (charset == SHIFTJIS_CHARSET
	  || charset == HANGEUL_CHARSET
	  || charset == CHINESEBIG5_CHARSET
	  || charset == GB2312_CHARSET
#ifdef JOHAB_CHARSET
	  || charset == JOHAB_CHARSET
#endif
	  )
	return 1;
    }

  {
    char buf[100];
    Lisp_Object width = Qnil;
    Lisp_Object charset_list = Qnil;
    char *charset = NULL;

    /* Truetype fonts do not report their true metrics until loaded */
    if (FontType != RASTER_FONTTYPE)
      {
	if (!NILP (lpef->pattern))
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

    if (!NILP (lpef->pattern))
      {
        charset = xlfd_charset_of_font (SDATA (lpef->pattern));

	/* We already checked charsets above, but DEFAULT_CHARSET
           slipped through.  So only allow exact matches for DEFAULT_CHARSET.  */
	if (charset
	    && strncmp (charset, "*-*", 3) != 0
	    && lpef->logfont.lfCharSet == DEFAULT_CHARSET
	    && strcmp (charset, w32_to_x_charset (DEFAULT_CHARSET, NULL)) != 0)
	  return 1;

        /* Reject raster fonts if we are looking for a unicode font.  */
        if (charset
            && FontType == RASTER_FONTTYPE
            && strncmp (charset, "iso10646", 8) == 0)
          return 1;
      }

    if (charset)
      charset_list = Fcons (build_string (charset), Qnil);
    else
      /* Always prefer unicode.  */
      charset_list
	= Fcons (build_string ("iso10646-1"),
		 w32_to_all_x_charsets (lplf->elfLogFont.lfCharSet));

    /* Loop through the charsets.  */
    for ( ; CONSP (charset_list); charset_list = Fcdr (charset_list))
      {
	Lisp_Object this_charset = Fcar (charset_list);
	charset = SDATA (this_charset);

	/* Don't list  raster fonts as unicode.	 */
	if (charset
	    && FontType == RASTER_FONTTYPE
	    && strncmp (charset, "iso10646", 8) == 0)
	  continue;

	enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
				     charset, width);

	/* List bold and italic variations if w32-enable-synthesized-fonts
	   is non-nil and this is a plain font.  */
	if (w32_enable_synthesized_fonts
	    && lplf->elfLogFont.lfWeight == FW_NORMAL
	    && lplf->elfLogFont.lfItalic == FALSE)
	  {
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

  if (NILP (lpef->pattern)
      || w32_font_match (buf, SDATA (lpef->pattern)))
    {
      /* Check if we already listed this font.  This may happen if
         w32_enable_synthesized_fonts is non-nil, and there are real
         bold and italic versions of the font.  */
      Lisp_Object font_name = build_string (buf);
      if (NILP (Fmember (font_name, lpef->list)))
	{
	  Lisp_Object entry = Fcons (font_name, width);
	  lpef->list = Fcons (entry, lpef->list);
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

static Lisp_Object
w32_list_bdf_fonts (Lisp_Object pattern, int max_names)
{
  char *fontname, *ptnstr;
  Lisp_Object list, tem, newlist = Qnil;
  int n_fonts = 0;

  list = Vw32_bdf_filename_alist;
  ptnstr = SDATA (pattern);

  for ( ; CONSP (list); list = XCDR (list))
    {
      tem = XCAR (list);
      if (CONSP (tem))
        fontname = SDATA (XCAR (tem));
      else if (STRINGP (tem))
        fontname = SDATA (tem);
      else
        continue;

      if (w32_font_match (fontname, ptnstr))
        {
          newlist = Fcons (XCAR (tem), newlist);
          n_fonts++;
          if (max_names >= 0 && n_fonts >= max_names)
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
   MAXNAMES sets a limit on how many fonts to match.  If MAXNAMES is
   negative, then all matching fonts are returned.  */

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
      codepage = w32_codepage_for_font (SDATA (tpat));
      if (codepage != CP_8BIT && codepage != CP_UNICODE
          && codepage != CP_DEFAULT && codepage != CP_UNKNOWN
	  && !IsValidCodePage (codepage))
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
      ef.pattern = tpat;
      ef.list = Qnil;
      ef.numFonts = 0;

      /* Use EnumFontFamiliesEx where it is available, as it knows
         about character sets.  Fall back to EnumFontFamilies for
         older versions of NT that don't support the 'Ex function.  */
      x_to_w32_font (SDATA (tpat), &ef.logfont);
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
      list = ef.list;

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
              if (maxnames >= 0 && n_fonts >= maxnames)
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

              if (!x_to_w32_font (SDATA (XCAR (tem)), &lf))
                continue;

              BLOCK_INPUT;
              thisinfo.bdf = NULL;
              thisinfo.hfont = CreateFontIndirect (&lf);
              if (thisinfo.hfont == NULL)
                continue;

              hdc = GetDC (dpyinfo->root_window);
              oldobj = SelectObject (hdc, thisinfo.hfont);
              if (GetTextMetrics (hdc, &thisinfo.tm))
                XSETCDR (tem, make_number (FONT_AVG_WIDTH (&thisinfo)));
              else
                XSETCDR (tem, make_number (0));
              SelectObject (hdc, oldobj);
              ReleaseDC (dpyinfo->root_window, hdc);
              DeleteObject (thisinfo.hfont);
              UNBLOCK_INPUT;
            }
          found_size = XINT (XCDR (tem));
          if (found_size == size)
            {
              newlist = Fcons (XCAR (tem), newlist);
              n_fonts++;
              if (maxnames >= 0 && n_fonts >= maxnames)
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
  if (n_fonts < maxnames || maxnames < 0)
  {
    Lisp_Object combined[2];
    combined[0] = w32_list_bdf_fonts (pattern, maxnames - n_fonts);
    combined[1] = newlist;
    newlist = Fnconc (2, combined);
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
      if (stricmp (pfi->name, fontname) == 0) return pfi;
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

/* directory-files from dired.c.  */
Lisp_Object Fdirectory_files P_ ((Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object));


/* Find BDF files in a specified directory.  (use GCPRO when calling,
   as this calls lisp to get a directory listing).  */
static Lisp_Object
w32_find_bdf_fonts_in_dir (Lisp_Object directory)
{
  Lisp_Object filelist, list = Qnil;
  char fontname[100];

  if (!STRINGP (directory))
    return Qnil;

  filelist = Fdirectory_files (directory, Qt,
			       build_string (".*\\.[bB][dD][fF]"), Qt);

  for ( ; CONSP (filelist); filelist = XCDR (filelist))
    {
      Lisp_Object filename = XCAR (filelist);
      if (w32_BDF_to_x_font (SDATA (filename), fontname, 100))
          store_in_alist (&list, build_string (fontname), filename);
    }
  return list;
}

DEFUN ("w32-find-bdf-fonts", Fw32_find_bdf_fonts, Sw32_find_bdf_fonts,
       1, 1, 0,
       doc: /* Return a list of BDF fonts in DIRECTORY.
The list is suitable for appending to `w32-bdf-filename-alist'.
Fonts which do not contain an xlfd description will not be included
in the list.  DIRECTORY may be a list of directories.  */)
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
      pair[1] = w32_find_bdf_fonts_in_dir ( XCAR (directory) );
      list = Fnconc ( 2, pair );
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

  if (w32_defined_color (f, SDATA (color), &foo, 0))
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

  if (w32_defined_color (f, SDATA (color), &foo, 0))
    return list3 (make_number ((GetRValue (foo.pixel) << 8)
			       | GetRValue (foo.pixel)),
		  make_number ((GetGValue (foo.pixel) << 8)
			       | GetGValue (foo.pixel)),
		  make_number ((GetBValue (foo.pixel) << 8)
			       | GetBValue (foo.pixel)));
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
       doc: /* Return t if DISPLAY supports shades of gray.
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
       doc: /* Return the width in pixels of DISPLAY.
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
       doc: /* Return the height in pixels of DISPLAY.
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
       doc: /* Return the number of bitplanes of DISPLAY.
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
       doc: /* Return the number of color cells of DISPLAY.
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
    cap = GetDeviceCaps (hdc, SIZEPALETTE);
  else
    cap = GetDeviceCaps (hdc, NUMCOLORS);

  /* We force 24+ bit depths to 24-bit, both to prevent an overflow
     and because probably is more meaningful on Windows anyway */
  if (cap < 0)
    cap = 1 << min (dpyinfo->n_planes * dpyinfo->n_cbits, 24);

  ReleaseDC (dpyinfo->root_window, hdc);

  return make_number (cap);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Return the maximum request size of the server of DISPLAY.
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
       doc: /* Return the "vendor ID" string of the W32 system (Microsoft).
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return build_string ("Microsoft Corp.");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Return the version numbers of the server of DISPLAY.
The value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the distributor-specific
release number.  See also the function `x-server-vendor'.

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
       doc: /* Return the number of screens on the server of DISPLAY.
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
       doc: /* Return the height in millimeters of DISPLAY.
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
       doc: /* Return the width in millimeters of DISPLAY.
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
       doc: /* Return an indication of whether DISPLAY does backing store.
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
       doc: /* Return the visual class of DISPLAY.
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
       doc: /* Return t if DISPLAY supports the save-under feature.
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
			     (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", SDATA (name));

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

#if 0
  if (! EQ (Vwindow_system, intern ("w32")))
    error ("Not using Microsoft Windows");
#endif

  /* Allow color mapping to be defined externally; first look in user's
     HOME directory, then in Emacs etc dir for a file called rgb.txt. */
  {
    Lisp_Object color_file;
    struct gcpro gcpro1;

    color_file = build_string ("~/rgb.txt");

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

  /* Merge in system logical colors.  */
  add_system_logical_colors_to_map (&Vw32_color_map);

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) SDATA (xrm_string);
  else
    xrm_option = (unsigned char *) 0;

  /* Use this general default value to start with.  */
  /* First remove .exe suffix from invocation-name - it looks ugly. */
  {
    char basename[ MAX_PATH ], *str;

    strcpy (basename, SDATA (Vinvocation_name));
    str = strrchr (basename, '.');
    if (str) *str = 0;
    Vinvocation_name = build_string (basename);
  }
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = w32_term_init (display, xrm_option,
			     (char *) SDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to server %s.\n",
	       SDATA (display));
      else
	error ("Cannot connect to server %s", SDATA (display));
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
  for (tail = w32_display_name_list; CONSP (tail); tail = XCDR (tail))
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
#if 0 /* TODO : port window properties to W32 */
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
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
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

#endif /* TODO */
  return Qnil;
}



/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* Non-zero means an hourglass cursor is currently shown.  */

static int hourglass_shown_p;

/* Number of seconds to wait before displaying an hourglass cursor.  */

static Lisp_Object Vhourglass_delay;

/* Default number of seconds to wait before displaying an hourglass
   cursor.  */

#define DEFAULT_HOURGLASS_DELAY 1

/* Return non-zero if houglass timer has been started or hourglass is shown.  */

int
hourglass_started ()
{
  return hourglass_shown_p || hourglass_timer;
}

/* Cancel a currently active hourglass timer, and start a new one.  */

void
start_hourglass ()
{
  DWORD delay;
  int secs, msecs = 0;
  struct frame * f = SELECTED_FRAME ();

  /* No cursors on non GUI frames.  */
  if (!FRAME_W32_P (f))
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
      msecs = (XFLOAT_DATA (Vhourglass_delay) - secs) * 1000;
    }
  else
    secs = DEFAULT_HOURGLASS_DELAY;

  delay = secs * 1000 + msecs;
  hourglass_hwnd = FRAME_W32_WINDOW (f);
  hourglass_timer = SetTimer (hourglass_hwnd, HOURGLASS_ID, delay, NULL);
}


/* Cancel the hourglass cursor timer if active, hide an hourglass
   cursor if shown.  */

void
cancel_hourglass ()
{
  if (hourglass_timer)
    {
      KillTimer (hourglass_hwnd, hourglass_timer);
      hourglass_timer = 0;
    }

  if (hourglass_shown_p)
    hide_hourglass ();
}


/* Timer function of hourglass_timer.

   Display an hourglass cursor.  Set the hourglass_p flag in display info
   to indicate that an hourglass cursor is shown.  */

static void
show_hourglass (f)
     struct frame *f;
{
  if (!hourglass_shown_p)
    {
      f->output_data.w32->hourglass_p = 1;
      if (!menubar_in_use && !current_popup_menu)
	SetCursor (f->output_data.w32->hourglass_cursor);
      hourglass_shown_p = 1;
    }
}


/* Hide the hourglass cursor on all frames, if it is currently shown.  */

static void
hide_hourglass ()
{
  if (hourglass_shown_p)
    {
      struct frame *f = x_window_to_frame (&one_w32_display_info,
					   hourglass_hwnd);

      f->output_data.w32->hourglass_p = 0;
      SetCursor (f->output_data.w32->current_cursor);
      hourglass_shown_p = 0;
    }
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
  int count = SPECPDL_INDEX ();
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
  kb = dpyinfo->terminal->kboard;
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
  f->terminal = dpyinfo->terminal;
  f->terminal->reference_count++;
  f->output_method = output_w32;
  f->output_data.w32 =
    (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));

  FRAME_FONTSET (f)  = -1;
  f->icon_name = Qnil;

#if 0 /* GLYPH_DEBUG TODO: image support.  */
  image_cache_refcount = FRAME_IMAGE_CACHE (f)->refcount;
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

  f->resx = dpyinfo->resx;
  f->resy = dpyinfo->resy;

#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    {
      /* Perhaps, we must allow frame parameter, say `font-backend',
	 to specify which font backends to use.  */
      register_font_driver (&w32font_driver, f);

      x_default_parameter (f, parms, Qfont_backend, Qnil,
			   "fontBackend", "FontBackend", RES_TYPE_STRING);
    }
#endif	/* USE_FONT_BACKEND */

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    x_default_font_parameter (f, parms);
  else
#endif	/* USE_FONT_BACKEND */
  {
    Lisp_Object font;

    font = w32_get_arg (parms, Qfont, "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
	tem = Fquery_fontset (font, Qnil);
	if (STRINGP (tem))
	  font = x_new_fontset (f, tem);
	else
	  font = x_new_font (f, SDATA (font));
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

  window_prompting = x_figure_window_size (f, parms, 0);

  /* No fringes on tip frame.  */
  f->fringe_cols = 0;
  f->left_fringe_width = 0;
  f->right_fringe_width = 0;

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

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);
  FRAME_LINES (f) = 0;
  SET_FRAME_COLS (f, 0);
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
  int min_x, min_y, max_x, max_y;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if (!INTEGERP (left) || !INTEGERP (top))
    {
      POINT pt;

      /* Default min and max values.  */
      min_x = 0;
      min_y = 0;
      max_x = FRAME_W32_DISPLAY_INFO (f)->width;
      max_y = FRAME_W32_DISPLAY_INFO (f)->height;

      BLOCK_INPUT;
      GetCursorPos (&pt);
      *root_x = pt.x;
      *root_y = pt.y;
      UNBLOCK_INPUT;

      /* If multiple monitor support is available, constrain the tip onto
	 the current monitor. This improves the above by allowing negative
	 co-ordinates if monitor positions are such that they are valid, and
	 snaps a tooltip onto a single monitor if we are close to the edge
	 where it would otherwise flow onto the other monitor (or into
	 nothingness if there is a gap in the overlap).  */
      if (monitor_from_point_fn && get_monitor_info_fn)
	{
	  struct MONITOR_INFO info;
	  HMONITOR monitor
	    = monitor_from_point_fn (pt, MONITOR_DEFAULT_TO_NEAREST);
	  info.cbSize = sizeof (info);

	  if (get_monitor_info_fn (monitor, &info))
	    {
	      min_x = info.rcWork.left;
	      min_y = info.rcWork.top;
	      max_x = info.rcWork.right;
	      max_y = info.rcWork.bottom;
	    }
	}
    }

  if (INTEGERP (top))
    *root_y = XINT (top);
  else if (*root_y + XINT (dy) <= min_y)
    *root_y = min_y; /* Can happen for negative dy */
  else if (*root_y + XINT (dy) + height <= max_y)
    /* It fits below the pointer */
      *root_y += XINT (dy);
  else if (height + XINT (dy) + min_y <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XINT (dy);
  else
    /* Put it on the top.  */
    *root_y = min_y;

  if (INTEGERP (left))
    *root_x = XINT (left);
  else if (*root_x + XINT (dx) <= min_x)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XINT (dx) + width <= max_x)
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) + min_x <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = min_x;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a \"tooltip\" window on frame FRAME.
A tooltip window is a small window displaying a string.

This is an internal function; Lisp code should call `tooltip-show'.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be
used to change the tooltip's appearance.

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

    /* Position and size tooltip, and put it in the topmost group.
       The add-on of 3 to the 5th argument is a kludge: without it,
       some fonts cause the last character of the tip to be truncated,
       for some obscure reason.  */
    SetWindowPos (FRAME_W32_WINDOW (f), HWND_TOPMOST,
		  root_x, root_y, rect.right - rect.left + 3,
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



/***********************************************************************
			File selection dialog
 ***********************************************************************/
extern Lisp_Object Qfile_name_history;

/* Callback for altering the behaviour of the Open File dialog.
   Makes the Filename text field contain "Current Directory" and be
   read-only when "Directories" is selected in the filter.  This
   allows us to work around the fact that the standard Open File
   dialog does not support directories.  */
UINT CALLBACK
file_dialog_callback (hwnd, msg, wParam, lParam)
     HWND hwnd;
     UINT msg;
     WPARAM wParam;
     LPARAM lParam;
{
  if (msg == WM_NOTIFY)
    {
      OFNOTIFY * notify = (OFNOTIFY *)lParam;
      /* Detect when the Filter dropdown is changed.  */
      if (notify->hdr.code == CDN_TYPECHANGE
	  || notify->hdr.code == CDN_INITDONE)
	{
	  HWND dialog = GetParent (hwnd);
	  HWND edit_control = GetDlgItem (dialog, FILE_NAME_TEXT_FIELD);

	  /* Directories is in index 2.  */
	  if (notify->lpOFN->nFilterIndex == 2)
	    {
	      CommDlg_OpenSave_SetControlText (dialog, FILE_NAME_TEXT_FIELD,
					       "Current Directory");
	      EnableWindow (edit_control, FALSE);
	    }
	  else
	    {
	      /* Don't override default filename on init done.  */
	      if (notify->hdr.code == CDN_TYPECHANGE)
		CommDlg_OpenSave_SetControlText (dialog,
						 FILE_NAME_TEXT_FIELD, "");
	      EnableWindow (edit_control, TRUE);
	    }
	}
    }
  return 0;
}

/* Since we compile with _WIN32_WINNT set to 0x0400 (for NT4 compatibility)
   we end up with the old file dialogs. Define a big enough struct for the
   new dialog to trick GetOpenFileName into giving us the new dialogs on
   Windows 2000 and XP.  */
typedef struct
{
  OPENFILENAME real_details;
  void * pReserved;
  DWORD dwReserved;
  DWORD FlagsEx;
} NEWOPENFILENAME;


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
  char filename[MAX_PATH + 1];
  char init_dir[MAX_PATH + 1];
  int default_filter_index = 1; /* 1: All Files, 2: Directories only  */

  GCPRO6 (prompt, dir, default_filename, mustmatch, only_dir_p, file);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  strncpy (init_dir, SDATA (ENCODE_FILE (dir)), MAX_PATH);
  init_dir[MAX_PATH] = '\0';
  unixtodos_filename (init_dir);

  if (STRINGP (default_filename))
    {
      char *file_name_only;
      char *full_path_name = SDATA (ENCODE_FILE (default_filename));

      unixtodos_filename (full_path_name);

      file_name_only = strrchr (full_path_name, '\\');
      if (!file_name_only)
        file_name_only = full_path_name;
      else
	file_name_only++;

      strncpy (filename, file_name_only, MAX_PATH);
      filename[MAX_PATH] = '\0';
    }
  else
    filename[0] = '\0';

  {
    NEWOPENFILENAME new_file_details;
    BOOL file_opened = FALSE;
    OPENFILENAME * file_details = &new_file_details.real_details;

    /* Prevent redisplay.  */
    specbind (Qinhibit_redisplay, Qt);
    BLOCK_INPUT;

    bzero (&new_file_details, sizeof (new_file_details));
    /* Apparently NT4 crashes if you give it an unexpected size.
       I'm not sure about Windows 9x, so play it safe.  */
    if (w32_major_version > 4 && w32_major_version < 95)
      file_details->lStructSize = sizeof (NEWOPENFILENAME);
    else
      file_details->lStructSize = sizeof (OPENFILENAME);

    file_details->hwndOwner = FRAME_W32_WINDOW (f);
    /* Undocumented Bug in Common File Dialog:
       If a filter is not specified, shell links are not resolved.  */
    file_details->lpstrFilter = "All Files (*.*)\0*.*\0Directories\0*|*\0\0";
    file_details->lpstrFile = filename;
    file_details->nMaxFile = sizeof (filename);
    file_details->lpstrInitialDir = init_dir;
    file_details->lpstrTitle = SDATA (prompt);

    if (! NILP (only_dir_p))
      default_filter_index = 2;

    file_details->nFilterIndex = default_filter_index;

    file_details->Flags = (OFN_HIDEREADONLY | OFN_NOCHANGEDIR
			  | OFN_EXPLORER | OFN_ENABLEHOOK);
    if (!NILP (mustmatch))
      {
	/* Require that the path to the parent directory exists.  */
	file_details->Flags |= OFN_PATHMUSTEXIST;
	/* If we are looking for a file, require that it exists.  */
	if (NILP (only_dir_p))
	  file_details->Flags |= OFN_FILEMUSTEXIST;
      }

    file_details->lpfnHook = (LPOFNHOOKPROC) file_dialog_callback;

    file_opened = GetOpenFileName (file_details);

    UNBLOCK_INPUT;

    if (file_opened)
      {
	dostounix_filename (filename);

	if (file_details->nFilterIndex == 2)
	  {
	    /* "Directories" selected - strip dummy file name.  */
	    char * last = strrchr (filename, '/');
	    *last = '\0';
	  }

	file = DECODE_FILE (build_string (filename));
      }
    /* User cancelled the dialog without making a selection.  */
    else if (!CommDlgExtendedError ())
      file = Qnil;
    /* An error occurred, fallback on reading from the mini-buffer.  */
    else
      file = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			       dir, mustmatch, dir, Qfile_name_history,
			       default_filename, Qnil);

    file = unbind_to (count, file);
  }

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
       doc: /* Select a font for the named FRAME using the W32 font dialog.
Return an X-style font string corresponding to the selection.

If FRAME is omitted or nil, it defaults to the selected frame.
If INCLUDE-PROPORTIONAL is non-nil, include proportional fonts
in the font selection dialog. */)
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
Some useful values for COMMAND are #xf030 to maximize frame (#xf020
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

OPERATION is either nil or a string that names a supported operation.
What operations can be used depends on the particular DOCUMENT and its
handler application, but typically it is one of the following common
operations:

 \"open\"    - open DOCUMENT, which could be a file, a directory, or an
               executable program.  If it is an application, that
               application is launched in the current buffer's default
               directory.  Otherwise, the application associated with
               DOCUMENT is launched in the buffer's default directory.
 \"print\"   - print DOCUMENT, which must be a file
 \"explore\" - start the Windows Explorer on DOCUMENT
 \"edit\"    - launch an editor and open DOCUMENT for editing; which
               editor is launched depends on the association for the
               specified DOCUMENT
 \"find\"    - initiate search starting from DOCUMENT which must specify
               a directory
 nil       - invoke the default OPERATION, or \"open\" if default is
               not defined or unavailable

DOCUMENT is typically the name of a document file or a URL, but can
also be a program executable to run, or a directory to open in the
Windows Explorer.

If DOCUMENT is a program executable, the optional third arg PARAMETERS
can be a string containing command line parameters that will be passed
to the program; otherwise, PARAMETERS should be nil or unspecified.

Optional fourth argument SHOW-FLAG can be used to control how the
application will be displayed when it is invoked.  If SHOW-FLAG is nil
or unspecified, the application is displayed normally, otherwise it is
an integer representing a ShowWindow flag:

  0 - start hidden
  1 - start normally
  3 - start maximized
  6 - start minimized  */)
  (operation, document, parameters, show_flag)
     Lisp_Object operation, document, parameters, show_flag;
{
  Lisp_Object current_dir;

  CHECK_STRING (document);

  /* Encode filename, current directory and parameters.  */
  current_dir = ENCODE_FILE (current_buffer->directory);
  document = ENCODE_FILE (document);
  if (STRINGP (parameters))
    parameters = ENCODE_SYSTEM (parameters);

  if ((int) ShellExecute (NULL,
			  (STRINGP (operation) ?
			   SDATA (operation) : NULL),
			  SDATA (document),
			  (STRINGP (parameters) ?
			   SDATA (parameters) : NULL),
			  SDATA (current_dir),
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
    if (lispy_function_keys[i]
	&& strcmp (lispy_function_keys[i], key) == 0)
      return i;

  return -1;
}

/* Convert a one-element vector style key sequence to a hot key
   definition.  */
static Lisp_Object
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
      lisp_modifiers = XINT (Fcar (Fcdr (c)));
      c = Fcar (c);
      if (!SYMBOLP (c))
	abort ();
      vk_code = lookup_vk_code (SDATA (SYMBOL_NAME (c)));
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

  if (!NILP (key) && NILP (Fmemq (key, w32_grabbed_keys)))
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
#ifdef USE_LISP_UNION_TYPE
      PostThreadMessage (dwWindowsThreadId, WM_EMACS_REGISTER_HOT_KEY,
			 (WPARAM) key.i, 0);
#else
      PostThreadMessage (dwWindowsThreadId, WM_EMACS_REGISTER_HOT_KEY,
			 (WPARAM) key, 0);
#endif
    }

  return key;
}

DEFUN ("w32-unregister-hot-key", Fw32_unregister_hot_key,
       Sw32_unregister_hot_key, 1, 1, 0,
       doc: /* Unregister KEY as a hot-key combination.  */)
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
#ifdef USE_LISP_UNION_TYPE
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_UNREGISTER_HOT_KEY,
			     (WPARAM) XINT (XCAR (item)), (LPARAM) item.i))
#else
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_UNREGISTER_HOT_KEY,
			     (WPARAM) XINT (XCAR (item)), (LPARAM) item))
#endif
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
  return Fdelq (Qnil, Fcopy_sequence (w32_grabbed_keys));
}

DEFUN ("w32-reconstruct-hot-key", Fw32_reconstruct_hot_key,
       Sw32_reconstruct_hot_key, 1, 1, 0,
       doc: /* Convert hot-key ID to a lisp key combination.
usage: (w32-reconstruct-hot-key ID)  */)
  (hotkeyid)
     Lisp_Object hotkeyid;
{
  int vk_code, w32_modifiers;
  Lisp_Object key;

  CHECK_NUMBER (hotkeyid);

  vk_code = HOTKEY_VK_CODE (hotkeyid);
  w32_modifiers = HOTKEY_MODIFIERS (hotkeyid);

  if (vk_code < 256 && lispy_function_keys[vk_code])
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

#ifdef USE_LISP_UNION_TYPE
  if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_TOGGLE_LOCK_KEY,
			 (WPARAM) vk_code, (LPARAM) new_state.i))
#else
  if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_TOGGLE_LOCK_KEY,
			 (WPARAM) vk_code, (LPARAM) new_state))
#endif
    {
      MSG msg;
      GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
      return make_number (msg.wParam);
    }
  return Qnil;
}

DEFUN ("w32-window-exists-p", Fw32_window_exists_p, Sw32_window_exists_p,
       2, 2, 0,
       doc: /* Return non-nil if a window exists with the specified CLASS and NAME.

This is a direct interface to the Windows API FindWindow function.  */)
  (class, name)
Lisp_Object class, name;
{
  HWND hnd;

  if (!NILP (class))
    CHECK_STRING (class);
  if (!NILP (name))
    CHECK_STRING (name);

  hnd = FindWindow (STRINGP (class) ? ((LPCTSTR) SDATA (class)) : NULL,
		    STRINGP (name)  ? ((LPCTSTR) SDATA (name))  : NULL);
  if (!hnd)
    return Qnil;
  return Qt;
}

DEFUN ("w32-battery-status", Fw32_battery_status, Sw32_battery_status, 0, 0, 0,
       doc: /* Get power status information from Windows system.

The following %-sequences are provided:
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'  */)
  ()
{
  Lisp_Object status = Qnil;

  SYSTEM_POWER_STATUS system_status;
  if (GetSystemPowerStatus (&system_status))
    {
      Lisp_Object line_status, battery_status, battery_status_symbol;
      Lisp_Object load_percentage, seconds, minutes, hours, remain;
      Lisp_Object sequences[8];

      long seconds_left = (long) system_status.BatteryLifeTime;

      if (system_status.ACLineStatus == 0)
	line_status = build_string ("off-line");
      else if (system_status.ACLineStatus == 1)
	line_status = build_string ("on-line");
      else
	line_status = build_string ("N/A");

      if (system_status.BatteryFlag & 128)
	{
	  battery_status = build_string ("N/A");
	  battery_status_symbol = build_string ("");
	}
      else if (system_status.BatteryFlag & 8)
	{
	  battery_status = build_string ("charging");
	  battery_status_symbol = build_string ("+");
	  if (system_status.BatteryFullLifeTime != -1L)
	    seconds_left = system_status.BatteryFullLifeTime - seconds_left;
	}
      else if (system_status.BatteryFlag & 4)
	{
	  battery_status = build_string ("critical");
	  battery_status_symbol = build_string ("!");
	}
      else if (system_status.BatteryFlag & 2)
	{
	  battery_status = build_string ("low");
	  battery_status_symbol = build_string ("-");
	}
      else if (system_status.BatteryFlag & 1)
	{
	  battery_status = build_string ("high");
	  battery_status_symbol = build_string ("");
	}
      else
	{
	  battery_status = build_string ("medium");
	  battery_status_symbol = build_string ("");
	}

      if (system_status.BatteryLifePercent > 100)
	load_percentage = build_string ("N/A");
      else
	{
	  char buffer[16];
	  _snprintf (buffer, 16, "%d", system_status.BatteryLifePercent);
	  load_percentage = build_string (buffer);
	}

      if (seconds_left < 0)
	seconds = minutes = hours = remain = build_string ("N/A");
      else
	{
	  long m;
	  float h;
	  char buffer[16];
	  _snprintf (buffer, 16, "%ld", seconds_left);
	  seconds = build_string (buffer);

	  m = seconds_left / 60;
	  _snprintf (buffer, 16, "%ld", m);
	  minutes = build_string (buffer);

	  h = seconds_left / 3600.0;
	  _snprintf (buffer, 16, "%3.1f", h);
	  hours = build_string (buffer);

	  _snprintf (buffer, 16, "%ld:%02ld", m / 60, m % 60);
	  remain = build_string (buffer);
	}
      sequences[0] = Fcons (make_number ('L'), line_status);
      sequences[1] = Fcons (make_number ('B'), battery_status);
      sequences[2] = Fcons (make_number ('b'), battery_status_symbol);
      sequences[3] = Fcons (make_number ('p'), load_percentage);
      sequences[4] = Fcons (make_number ('s'), seconds);
      sequences[5] = Fcons (make_number ('m'), minutes);
      sequences[6] = Fcons (make_number ('h'), hours);
      sequences[7] = Fcons (make_number ('t'), remain);

      status = Flist (8, sequences);
    }
  return status;
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
    char *name = SDATA (encoded);

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

	if (pfn_GetDiskFreeSpaceEx (rootname,
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

	if (GetDiskFreeSpace (rootname,
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

DEFUN ("default-printer-name", Fdefault_printer_name, Sdefault_printer_name,
       0, 0, 0, doc: /* Return the name of Windows default printer device.  */)
     ()
{
  static char pname_buf[256];
  int err;
  HANDLE hPrn;
  PRINTER_INFO_2 *ppi2 = NULL;
  DWORD dwNeeded = 0, dwReturned = 0;

  /* Retrieve the default string from Win.ini (the registry).
   * String will be in form "printername,drivername,portname".
   * This is the most portable way to get the default printer. */
  if (GetProfileString ("windows", "device", ",,", pname_buf, sizeof (pname_buf)) <= 0)
    return Qnil;
  /* printername precedes first "," character */
  strtok (pname_buf, ",");
  /* We want to know more than the printer name */
  if (!OpenPrinter (pname_buf, &hPrn, NULL))
    return Qnil;
  GetPrinter (hPrn, 2, NULL, 0, &dwNeeded);
  if (dwNeeded == 0)
    {
      ClosePrinter (hPrn);
      return Qnil;
    }
  /* Allocate memory for the PRINTER_INFO_2 struct */
  ppi2 = (PRINTER_INFO_2 *) xmalloc (dwNeeded);
  if (!ppi2)
    {
      ClosePrinter (hPrn);
      return Qnil;
    }
  /* Call GetPrinter again with big enouth memory block */
  err = GetPrinter (hPrn, 2, (LPBYTE)ppi2, dwNeeded, &dwReturned);
  ClosePrinter (hPrn);
  if (!err)
    {
      xfree (ppi2);
      return Qnil;
    }

  if (ppi2)
    {
      if (ppi2->Attributes & PRINTER_ATTRIBUTE_SHARED && ppi2->pServerName)
        {
	  /* a remote printer */
	  if (*ppi2->pServerName == '\\')
	    _snprintf (pname_buf, sizeof (pname_buf), "%s\\%s", ppi2->pServerName,
		       ppi2->pShareName);
	  else
	    _snprintf (pname_buf, sizeof (pname_buf), "\\\\%s\\%s", ppi2->pServerName,
		       ppi2->pShareName);
	  pname_buf[sizeof (pname_buf) - 1] = '\0';
	}
      else
        {
	  /* a local printer */
	  strncpy (pname_buf, ppi2->pPortName, sizeof (pname_buf));
	  pname_buf[sizeof (pname_buf) - 1] = '\0';
	  /* `pPortName' can include several ports, delimited by ','.
	   * we only use the first one. */
	  strtok (pname_buf, ",");
	}
      xfree (ppi2);
    }

  return build_string (pname_buf);
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Keep this list in the same order as frame_parms in frame.c.
   Use 0 for unsupported frame parameters.  */

frame_parm_handler w32_frame_parm_handlers[] =
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
  x_set_icon_type,
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
  0, /* x_set_scroll_bar_foreground, */
  0, /* x_set_scroll_bar_background, */
  x_set_screen_gamma,
  x_set_line_spacing,
  x_set_fringe_width,
  x_set_fringe_width,
  0, /* x_set_wait_for_wm, */
  x_set_fullscreen,
#ifdef USE_FONT_BACKEND
  x_set_font_backend
#endif
};

void
syms_of_w32fns ()
{
  globals_of_w32fns ();
  /* This is zero if not using MS-Windows.  */
  w32_in_use = 0;
  track_mouse_window = NULL;

  w32_visible_system_caret_hwnd = NULL;

  DEFSYM (Qnone, "none");
  DEFSYM (Qsuppress_icon, "suppress-icon");
  DEFSYM (Qundefined_color, "undefined-color");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qctrl, "ctrl");
  DEFSYM (Qcontrol, "control");
  DEFSYM (Qshift, "shift");
  /* This is the end of symbol initialization.  */

  /* Text property `display' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qdisplay, Qt), Vtext_property_default_nonsticky);


  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  staticpro (&w32_grabbed_keys);
  w32_grabbed_keys = Qnil;

  DEFVAR_LISP ("w32-color-map", &Vw32_color_map,
	       doc: /* An array of color name mappings for Windows.  */);
  Vw32_color_map = Qnil;

  DEFVAR_LISP ("w32-pass-alt-to-system", &Vw32_pass_alt_to_system,
	       doc: /* Non-nil if Alt key presses are passed on to Windows.
When non-nil, for example, Alt pressed and released and then space will
open the System menu.  When nil, Emacs processes the Alt key events, and
then silently swallows them.  */);
  Vw32_pass_alt_to_system = Qnil;

  DEFVAR_LISP ("w32-alt-is-meta", &Vw32_alt_is_meta,
	       doc: /* Non-nil if the Alt key is to be considered the same as the META key.
When nil, Emacs will translate the Alt key to the ALT modifier, not to META.  */);
  Vw32_alt_is_meta = Qt;

  DEFVAR_INT ("w32-quit-key", &w32_quit_key,
	       doc: /* If non-zero, the virtual key code for an alternative quit key.  */);
  w32_quit_key = 0;

  DEFVAR_LISP ("w32-pass-lwindow-to-system",
	       &Vw32_pass_lwindow_to_system,
	       doc: /* If non-nil, the left \"Windows\" key is passed on to Windows.

When non-nil, the Start menu is opened by tapping the key.
If you set this to nil, the left \"Windows\" key is processed by Emacs
according to the value of `w32-lwindow-modifier', which see.

Note that some combinations of the left \"Windows\" key with other keys are
caught by Windows at low level, and so binding them in Emacs will have no
effect.  For example, <lwindow>-r always pops up the Windows Run dialog,
<lwindow>-<Pause> pops up the "System Properties" dialog, etc.  However, see
the doc string of `w32-phantom-key-code'.  */);
  Vw32_pass_lwindow_to_system = Qt;

  DEFVAR_LISP ("w32-pass-rwindow-to-system",
	       &Vw32_pass_rwindow_to_system,
	       doc: /* If non-nil, the right \"Windows\" key is passed on to Windows.

When non-nil, the Start menu is opened by tapping the key.
If you set this to nil, the right \"Windows\" key is processed by Emacs
according to the value of `w32-rwindow-modifier', which see.

Note that some combinations of the right \"Windows\" key with other keys are
caught by Windows at low level, and so binding them in Emacs will have no
effect.  For example, <rwindow>-r always pops up the Windows Run dialog,
<rwindow>-<Pause> pops up the "System Properties" dialog, etc.  However, see
the doc string of `w32-phantom-key-code'.  */);
  Vw32_pass_rwindow_to_system = Qt;

  DEFVAR_LISP ("w32-phantom-key-code",
	       &Vw32_phantom_key_code,
	       doc: /* Virtual key code used to generate \"phantom\" key presses.
Value is a number between 0 and 255.

Phantom key presses are generated in order to stop the system from
acting on \"Windows\" key events when `w32-pass-lwindow-to-system' or
`w32-pass-rwindow-to-system' is nil.  */);
  /* Although 255 is technically not a valid key code, it works and
     means that this hack won't interfere with any real key code.  */
  XSETINT (Vw32_phantom_key_code, 255);

  DEFVAR_LISP ("w32-enable-num-lock",
	       &Vw32_enable_num_lock,
	       doc: /* If non-nil, the Num Lock key acts normally.
Set to nil to handle Num Lock as the `kp-numlock' key.  */);
  Vw32_enable_num_lock = Qt;

  DEFVAR_LISP ("w32-enable-caps-lock",
	       &Vw32_enable_caps_lock,
	       doc: /* If non-nil, the Caps Lock key acts normally.
Set to nil to handle Caps Lock as the `capslock' key.  */);
  Vw32_enable_caps_lock = Qt;

  DEFVAR_LISP ("w32-scroll-lock-modifier",
	       &Vw32_scroll_lock_modifier,
	       doc: /* Modifier to use for the Scroll Lock ON state.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to handle Scroll Lock as the `scroll' key.
Any other value will cause the Scroll Lock key to be ignored.  */);
  Vw32_scroll_lock_modifier = Qt;

  DEFVAR_LISP ("w32-lwindow-modifier",
	       &Vw32_lwindow_modifier,
	       doc: /* Modifier to use for the left \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `lwindow' key.
Any other value will cause the key to be ignored.  */);
  Vw32_lwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-rwindow-modifier",
	       &Vw32_rwindow_modifier,
	       doc: /* Modifier to use for the right \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `rwindow' key.
Any other value will cause the key to be ignored.  */);
  Vw32_rwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-apps-modifier",
	       &Vw32_apps_modifier,
	       doc: /* Modifier to use for the \"Apps\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `apps' key.
Any other value will cause the key to be ignored.  */);
  Vw32_apps_modifier = Qnil;

  DEFVAR_BOOL ("w32-enable-synthesized-fonts", &w32_enable_synthesized_fonts,
	       doc: /* Non-nil enables selection of artificially italicized and bold fonts.  */);
  w32_enable_synthesized_fonts = 0;

  DEFVAR_LISP ("w32-enable-palette", &Vw32_enable_palette,
	       doc: /* Non-nil enables Windows palette management to map colors exactly.  */);
  Vw32_enable_palette = Qt;

  DEFVAR_INT ("w32-mouse-button-tolerance",
	      &w32_mouse_button_tolerance,
	      doc: /* Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left and right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.  */);
  w32_mouse_button_tolerance = GetDoubleClickTime () / 2;

  DEFVAR_INT ("w32-mouse-move-interval",
	      &w32_mouse_move_interval,
	      doc: /* Minimum interval between mouse move events.
The value is the minimum time in milliseconds that must elapse between
successive mouse move (or scroll bar drag) events before they are
reported as lisp events.  */);
  w32_mouse_move_interval = 0;

  DEFVAR_BOOL ("w32-pass-extra-mouse-buttons-to-system",
	       &w32_pass_extra_mouse_buttons_to_system,
	       doc: /* If non-nil, the fourth and fifth mouse buttons are passed to Windows.
Recent versions of Windows support mice with up to five buttons.
Since most applications don't support these extra buttons, most mouse
drivers will allow you to map them to functions at the system level.
If this variable is non-nil, Emacs will pass them on, allowing the
system to handle them.  */);
  w32_pass_extra_mouse_buttons_to_system = 0;

  DEFVAR_BOOL ("w32-pass-multimedia-buttons-to-system",
               &w32_pass_multimedia_buttons_to_system,
               doc: /* If non-nil, media buttons are passed to Windows.
Some modern keyboards contain buttons for controlling media players, web
browsers and other applications.  Generally these buttons are handled on a
system wide basis, but by setting this to nil they are made available
to Emacs for binding.  Depending on your keyboard, additional keys that
may be available are:

browser-back, browser-forward, browser-refresh, browser-stop,
browser-search, browser-favorites, browser-home,
mail, mail-reply, mail-forward, mail-send,
app-1, app-2,
help, find, new, open, close, save, print, undo, redo, copy, cut, paste,
spell-check, correction-list, toggle-dictate-command,
media-next, media-previous, media-stop, media-play-pause, media-select,
media-play, media-pause, media-record, media-fast-forward, media-rewind,
media-channel-up, media-channel-down,
volume-mute, volume-up, volume-down,
mic-volume-mute, mic-volume-down, mic-volume-up, mic-toggle,
bass-down, bass-boost, bass-up, treble-down, treble-up  */);
  w32_pass_multimedia_buttons_to_system = 1;

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
	       doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

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
Set this to nil to get the old behavior for repainting; this should
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
w32-charset-johab, w32-charset-hebrew,
w32-charset-arabic, w32-charset-greek, w32-charset-turkish,
w32-charset-vietnamese, w32-charset-thai, w32-charset-easteurope,
w32-charset-russian, w32-charset-mac, w32-charset-baltic,
w32-charset-unicode,
or w32-charset-oem.
CODEPAGE should be an integer specifying the codepage that should be used
to display the character set, t to do no translation and output as Unicode,
or nil to do no translation and output as 8 bit (or multibyte on far-east
versions of Windows) characters.  */);
  Vw32_charset_info_alist = Qnil;

  DEFSYM (Qw32_charset_ansi, "w32-charset-ansi");
  DEFSYM (Qw32_charset_symbol, "w32-charset-symbol");
  DEFSYM (Qw32_charset_default, "w32-charset-default");
  DEFSYM (Qw32_charset_shiftjis, "w32-charset-shiftjis");
  DEFSYM (Qw32_charset_hangeul, "w32-charset-hangeul");
  DEFSYM (Qw32_charset_chinesebig5, "w32-charset-chinesebig5");
  DEFSYM (Qw32_charset_gb2312, "w32-charset-gb2312");
  DEFSYM (Qw32_charset_oem, "w32-charset-oem");

#ifdef JOHAB_CHARSET
  {
    static int w32_extra_charsets_defined = 1;
    DEFVAR_BOOL ("w32-extra-charsets-defined", &w32_extra_charsets_defined,
		 doc: /* Internal variable.  */);

    DEFSYM (Qw32_charset_johab, "w32-charset-johab");
    DEFSYM (Qw32_charset_easteurope, "w32-charset-easteurope");
    DEFSYM (Qw32_charset_turkish, "w32-charset-turkish");
    DEFSYM (Qw32_charset_baltic, "w32-charset-baltic");
    DEFSYM (Qw32_charset_russian, "w32-charset-russian");
    DEFSYM (Qw32_charset_arabic, "w32-charset-arabic");
    DEFSYM (Qw32_charset_greek, "w32-charset-greek");
    DEFSYM (Qw32_charset_hebrew, "w32-charset-hebrew");
    DEFSYM (Qw32_charset_vietnamese, "w32-charset-vietnamese");
    DEFSYM (Qw32_charset_thai, "w32-charset-thai");
    DEFSYM (Qw32_charset_mac, "w32-charset-mac");
  }
#endif

#ifdef UNICODE_CHARSET
  {
    static int w32_unicode_charset_defined = 1;
    DEFVAR_BOOL ("w32-unicode-charset-defined",
                 &w32_unicode_charset_defined,
		 doc: /* Internal variable.  */);
    DEFSYM (Qw32_charset_unicode, "w32-charset-unicode");
  }
#endif

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
  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_synchronize);
  defsubr (&Sx_focus_frame);

  /* W32 specific functions */

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
  defsubr (&Sw32_window_exists_p);
  defsubr (&Sw32_find_bdf_fonts);
  defsubr (&Sw32_battery_status);

  defsubr (&Sfile_system_info);
  defsubr (&Sdefault_printer_name);

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
  get_font_repertory_func = x_get_font_repertory;
  check_window_system_func = check_w32;


  hourglass_timer = 0;
  hourglass_hwnd = NULL;
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


/*
	globals_of_w32fns is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
	globals_of_w32fns is called from syms_of_w32fns when the global
	variable initialized is 0 and directly from main when initialized
	is non zero.
 */
void
globals_of_w32fns ()
{
  HMODULE user32_lib = GetModuleHandle ("user32.dll");
  /*
    TrackMouseEvent not available in all versions of Windows, so must load
    it dynamically.  Do it once, here, instead of every time it is used.
  */
  track_mouse_event_fn = (TrackMouseEvent_Proc)
    GetProcAddress (user32_lib, "TrackMouseEvent");
  /* ditto for GetClipboardSequenceNumber.  */
  clipboard_sequence_fn = (ClipboardSequence_Proc)
    GetProcAddress (user32_lib, "GetClipboardSequenceNumber");

  monitor_from_point_fn = (MonitorFromPoint_Proc)
    GetProcAddress (user32_lib, "MonitorFromPoint");
  get_monitor_info_fn = (GetMonitorInfo_Proc)
    GetProcAddress (user32_lib, "GetMonitorInfoA");

  {
    HMODULE imm32_lib = GetModuleHandle ("imm32.dll");
    get_composition_string_fn = (ImmGetCompositionString_Proc)
      GetProcAddress (imm32_lib, "ImmGetCompositionStringW");
    get_ime_context_fn = (ImmGetContext_Proc)
      GetProcAddress (imm32_lib, "ImmGetContext");
  }
  DEFVAR_INT ("w32-ansi-code-page",
	      &w32_ansi_code_page,
	      doc: /* The ANSI code page used by the system.  */);
  w32_ansi_code_page = GetACP ();

  /* MessageBox does not work without this when linked to comctl32.dll 6.0.  */
  InitCommonControls ();

#ifdef USE_FONT_BACKEND
  syms_of_w32uniscribe ();
#endif
}

#undef abort

void
w32_abort ()
{
  int button;
  button = MessageBox (NULL,
		       "A fatal error has occurred!\n\n"
		       "Would you like to attach a debugger?\n\n"
		       "Select YES to debug, NO to abort Emacs"
#if __GNUC__
		       "\n\n(type \"gdb -p <emacs-PID>\" and\n"
		       "\"continue\" inside GDB before clicking YES.)"
#endif
		       , "Emacs Abort Dialog",
		       MB_ICONEXCLAMATION | MB_TASKMODAL
		       | MB_SETFOREGROUND | MB_YESNO);
  switch (button)
    {
    case IDYES:
      DebugBreak ();
      exit (2);	/* tell the compiler we will never return */
    case IDNO:
    default:
      abort ();
      break;
    }
}

/* For convenience when debugging.  */
int
w32_last_error ()
{
  return GetLastError ();
}

/* arch-tag: 707589ab-b9be-4638-8cdd-74629cc9b446
   (do not change this comment) */
