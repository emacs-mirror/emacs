/* Graphical user interface functions for the Microsoft Windows API.

Copyright (C) 1989, 1992-2018 Free Software Foundation, Inc.

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

/* Added by Kevin Gallo */

#include <config.h>
/* Override API version to get the latest functionality.  */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600

#include <signal.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include <unistd.h>

#include <c-ctype.h>

#include "lisp.h"
#include "w32term.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "keyboard.h"
#include "blockinput.h"
#include "coding.h"

#include "w32common.h"
#include "w32inevt.h"

#ifdef WINDOWSNT
#include <mbstring.h>
#endif /* WINDOWSNT */

#if CYGWIN
#include "cygw32.h"
#else
#include "w32.h"
#endif

#include <basetyps.h>
#include <unknwn.h>
#include <commctrl.h>
#include <commdlg.h>
#include <shellapi.h>
#include <shlwapi.h>
#include <ctype.h>
#include <winspool.h>
#include <objbase.h>

#include <dlgs.h>
#include <imm.h>
#include <windowsx.h>

#ifndef FOF_NO_CONNECTED_ELEMENTS
#define FOF_NO_CONNECTED_ELEMENTS 0x2000
#endif

extern int w32_console_toggle_lock_key (int, Lisp_Object);
extern void w32_menu_display_help (HWND, HMENU, UINT, UINT);
extern void w32_free_menu_strings (HWND);
extern const char *map_w32_filename (const char *, const char **);

#ifndef IDC_HAND
#define IDC_HAND MAKEINTRESOURCE(32649)
#endif

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
#ifndef MONITORINFOF_PRIMARY
#define MONITORINFOF_PRIMARY 1
#endif
#ifndef SM_XVIRTUALSCREEN
#define SM_XVIRTUALSCREEN 76
#endif
#ifndef SM_YVIRTUALSCREEN
#define SM_YVIRTUALSCREEN 77
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

#if _WIN32_WINDOWS >= 0x0410
#define C_CHILDREN_TITLEBAR CCHILDREN_TITLEBAR
typedef TITLEBARINFO TITLEBAR_INFO;
#else
#define C_CHILDREN_TITLEBAR 5
typedef struct
{
  DWORD cbSize;
  RECT  rcTitleBar;
  DWORD rgstate[C_CHILDREN_TITLEBAR+1];
} TITLEBAR_INFO, *PTITLEBAR_INFO;
#endif

#ifndef CCHDEVICENAME
#define CCHDEVICENAME 32
#endif
struct MONITOR_INFO_EX
{
    DWORD   cbSize;
    RECT    rcMonitor;
    RECT    rcWork;
    DWORD   dwFlags;
    char    szDevice[CCHDEVICENAME];
};

/* Reportedly, MSVC does not have this in its headers.  */
#if defined (_MSC_VER) && _WIN32_WINNT < 0x0500
DECLARE_HANDLE(HMONITOR);
#endif

typedef BOOL (WINAPI * TrackMouseEvent_Proc)
  (IN OUT LPTRACKMOUSEEVENT lpEventTrack);
typedef LONG (WINAPI * ImmGetCompositionString_Proc)
  (IN HIMC context, IN DWORD index, OUT LPVOID buffer, IN DWORD bufLen);
typedef HIMC (WINAPI * ImmGetContext_Proc) (IN HWND window);
typedef BOOL (WINAPI * ImmReleaseContext_Proc) (IN HWND wnd, IN HIMC context);
typedef BOOL (WINAPI * ImmSetCompositionWindow_Proc) (IN HIMC context,
						      IN COMPOSITIONFORM *form);
typedef HMONITOR (WINAPI * MonitorFromPoint_Proc) (IN POINT pt, IN DWORD flags);
typedef BOOL (WINAPI * GetMonitorInfo_Proc)
  (IN HMONITOR monitor, OUT struct MONITOR_INFO* info);
typedef HMONITOR (WINAPI * MonitorFromWindow_Proc)
  (IN HWND hwnd, IN DWORD dwFlags);
typedef BOOL CALLBACK (* MonitorEnum_Proc)
  (IN HMONITOR monitor, IN HDC hdc, IN RECT *rcMonitor, IN LPARAM dwData);
typedef BOOL (WINAPI * EnumDisplayMonitors_Proc)
  (IN HDC hdc, IN RECT *rcClip, IN MonitorEnum_Proc fnEnum, IN LPARAM dwData);
typedef BOOL (WINAPI * GetTitleBarInfo_Proc)
  (IN HWND hwnd, OUT TITLEBAR_INFO* info);

TrackMouseEvent_Proc track_mouse_event_fn = NULL;
ImmGetCompositionString_Proc get_composition_string_fn = NULL;
ImmGetContext_Proc get_ime_context_fn = NULL;
ImmReleaseContext_Proc release_ime_context_fn = NULL;
ImmSetCompositionWindow_Proc set_ime_composition_window_fn = NULL;
MonitorFromPoint_Proc monitor_from_point_fn = NULL;
GetMonitorInfo_Proc get_monitor_info_fn = NULL;
MonitorFromWindow_Proc monitor_from_window_fn = NULL;
EnumDisplayMonitors_Proc enum_display_monitors_fn = NULL;
GetTitleBarInfo_Proc get_title_bar_info_fn = NULL;

extern AppendMenuW_Proc unicode_append_menu;

/* Flag to selectively ignore WM_IME_CHAR messages.  */
static int ignore_ime_char = 0;

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

#ifdef GLYPH_DEBUG
static ptrdiff_t image_cache_refcount;
static int dpyinfo_refcount;
#endif

static HWND w32_visible_system_caret_hwnd;

static int w32_unicode_gui;

/* From w32menu.c  */
int menubar_in_use = 0;

/* From w32uniscribe.c  */
extern void syms_of_w32uniscribe (void);
extern int uniscribe_available;

#ifdef WINDOWSNT
/* From w32inevt.c */
extern int faked_key;
#endif /* WINDOWSNT */

/* This gives us the page size and the size of the allocation unit on NT.  */
SYSTEM_INFO sysinfo_cache;

/* This gives us version, build, and platform identification.  */
OSVERSIONINFO osinfo_cache;

DWORD_PTR syspage_mask = 0;

/* The major and minor versions of NT.  */
int w32_major_version;
int w32_minor_version;
int w32_build_number;

/* Distinguish between Windows NT and Windows 95.  */
int os_subtype;

#ifdef HAVE_NTGUI
HINSTANCE hinst = NULL;
#endif

static unsigned int sound_type = 0xFFFFFFFF;
#define MB_EMACS_SILENT (0xFFFFFFFF - 1)

/* Special virtual key code for indicating "any" key.  */
#define VK_ANY 0xFF

#ifndef WM_WTSSESSION_CHANGE
/* 32-bit MinGW does not define these constants.  */
# define WM_WTSSESSION_CHANGE  0x02B1
# define WTS_SESSION_LOCK      0x7
#endif

#ifndef WS_EX_NOACTIVATE
#define WS_EX_NOACTIVATE 0x08000000L
#endif

/* Keyboard hook state data.  */
static struct
{
  int hook_count; /* counter, if several windows are created */
  HHOOK hook;     /* hook handle */
  HWND console;   /* console window handle */

  int lwindown;      /* Left Windows key currently pressed (and hooked) */
  int rwindown;      /* Right Windows key currently pressed (and hooked) */
  int winsdown;      /* Number of handled keys currently pressed */
  int send_win_up;   /* Pass through the keyup for this Windows key press? */
  int suppress_lone; /* Suppress simulated Windows keydown-keyup for this press? */
  int winseen;       /* Windows keys seen during this press? */

  char alt_hooked[256];  /* hook Alt+[this key]? */
  char lwin_hooked[256]; /* hook left Win+[this key]? */
  char rwin_hooked[256]; /* hook right Win+[this key]? */
} kbdhook;
typedef HWND (WINAPI *GetConsoleWindow_Proc) (void);

typedef BOOL (WINAPI *IsDebuggerPresent_Proc) (void);

/* stdin, from w32console.c */
extern HANDLE keyboard_handle;

/* Let the user specify a display with a frame.
   nil stands for the selected frame--or, if that is not a w32 frame,
   the first display on the list.  */

struct w32_display_info *
check_x_display_info (Lisp_Object object)
{
  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_W32_P (sf) && FRAME_LIVE_P (sf))
	return FRAME_DISPLAY_INFO (sf);
      else
	return &one_w32_display_info;
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type != output_w32)
	error ("Terminal %d is not a W32 display", t->id);

      return t->display_info.w32;
    }
  else if (STRINGP (object))
    return x_display_info_for_name (object);
  else
    {
      struct frame *f;

      CHECK_LIVE_FRAME (object);
      f = XFRAME (object);
      if (! FRAME_W32_P (f))
	error ("Non-W32 frame used");
      return FRAME_DISPLAY_INFO (f);
    }
}

/* Return the Emacs frame-object corresponding to an w32 window.
   It could be the frame's main window or an icon window.  */

struct frame *
x_window_to_frame (struct w32_display_info *dpyinfo, HWND wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (!FRAME_W32_P (f) || FRAME_DISPLAY_INFO (f) != dpyinfo)
	continue;

      if (FRAME_W32_WINDOW (f) == wdesc)
	return f;
    }
  return 0;
}


static Lisp_Object unwind_create_frame (Lisp_Object);
static void unwind_create_tip_frame (Lisp_Object);
static void my_create_window (struct frame *);
static void my_create_tip_window (struct frame *);

/* TODO: Native Input Method support; see x_create_im.  */
void x_set_foreground_color (struct frame *, Lisp_Object, Lisp_Object);
void x_set_background_color (struct frame *, Lisp_Object, Lisp_Object);
void x_set_mouse_color (struct frame *, Lisp_Object, Lisp_Object);
void x_set_cursor_color (struct frame *, Lisp_Object, Lisp_Object);
void x_set_border_color (struct frame *, Lisp_Object, Lisp_Object);
void x_set_cursor_type (struct frame *, Lisp_Object, Lisp_Object);
void x_set_icon_type (struct frame *, Lisp_Object, Lisp_Object);
void x_set_icon_name (struct frame *, Lisp_Object, Lisp_Object);
void x_explicitly_set_name (struct frame *, Lisp_Object, Lisp_Object);
void x_set_title (struct frame *, Lisp_Object, Lisp_Object);


/* Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */

void
x_real_positions (struct frame *f, int *xptr, int *yptr)
{
  RECT rect;

  /* Get the bounds of the WM window.  */
  GetWindowRect (FRAME_W32_WINDOW (f), &rect);

  if (FRAME_PARENT_FRAME (f))
    {
      /* For a child window we have to get its coordinates wrt its
	 parent.  */
      HWND parent_hwnd = FRAME_W32_WINDOW (FRAME_PARENT_FRAME (f));

      if (parent_hwnd)
	MapWindowPoints (HWND_DESKTOP, parent_hwnd, (LPPOINT) &rect, 2);
    }

  *xptr = rect.left;
  *yptr = rect.top;
}

/* Returns the window rectangle appropriate for the given fullscreen mode.
   The normal rect parameter was the window's rectangle prior to entering
   fullscreen mode.  If multiple monitor support is available, the nearest
   monitor to the window is chosen.  */

void
w32_fullscreen_rect (HWND hwnd, int fsmode, RECT normal, RECT *rect)
{
  struct MONITOR_INFO mi = { sizeof(mi) };
  if (monitor_from_window_fn && get_monitor_info_fn)
    {
      HMONITOR monitor =
	monitor_from_window_fn (hwnd, MONITOR_DEFAULT_TO_NEAREST);
      get_monitor_info_fn (monitor, &mi);
    }
  else
    {
      mi.rcMonitor.left = 0;
      mi.rcMonitor.top = 0;
      mi.rcMonitor.right = GetSystemMetrics (SM_CXSCREEN);
      mi.rcMonitor.bottom = GetSystemMetrics (SM_CYSCREEN);
      mi.rcWork.left = 0;
      mi.rcWork.top = 0;
      mi.rcWork.right = GetSystemMetrics (SM_CXMAXIMIZED);
      mi.rcWork.bottom = GetSystemMetrics (SM_CYMAXIMIZED);
    }

  switch (fsmode)
    {
    case FULLSCREEN_BOTH:
      rect->left = mi.rcMonitor.left;
      rect->top = mi.rcMonitor.top;
      rect->right = mi.rcMonitor.right;
      rect->bottom = mi.rcMonitor.bottom;
      break;
    case FULLSCREEN_WIDTH:
      rect->left = mi.rcWork.left;
      rect->top = normal.top;
      rect->right = mi.rcWork.right;
      rect->bottom = normal.bottom;
      break;
    case FULLSCREEN_HEIGHT:
      rect->left = normal.left;
      rect->top = mi.rcWork.top;
      rect->right = normal.right;
      rect->bottom = mi.rcWork.bottom;
      break;
    default:
      *rect = normal;
      break;
    }
}



DEFUN ("w32-define-rgb-color", Fw32_define_rgb_color,
       Sw32_define_rgb_color, 4, 4, 0,
       doc: /* Convert RGB numbers to a Windows color reference and associate with NAME.
This adds or updates a named color to `w32-color-map', making it
available for use.  The original entry's RGB ref is returned, or nil
if the entry is new.  */)
  (Lisp_Object red, Lisp_Object green, Lisp_Object blue, Lisp_Object name)
{
  Lisp_Object rgb;
  Lisp_Object oldrgb = Qnil;
  Lisp_Object entry;

  CHECK_NUMBER (red);
  CHECK_NUMBER (green);
  CHECK_NUMBER (blue);
  CHECK_STRING (name);

  XSETINT (rgb, RGB (XUINT (red), XUINT (green), XUINT (blue)));

  block_input ();

  /* replace existing entry in w32-color-map or add new entry. */
  entry = Fassoc (name, Vw32_color_map, Qnil);
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

  unblock_input ();

  return (oldrgb);
}

/* The default colors for the w32 color map */
typedef struct colormap_t
{
  const char *name;
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

static Lisp_Object
w32_default_color_map (void)
{
  int i;
  colormap_t *pc = w32_color_map;
  Lisp_Object cmap;

  block_input ();

  cmap = Qnil;

  for (i = 0; i < ARRAYELTS (w32_color_map); pc++, i++)
    cmap = Fcons (Fcons (build_string (pc->name),
			 make_number (pc->colorref)),
		  cmap);

  unblock_input ();

  return (cmap);
}

DEFUN ("w32-default-color-map", Fw32_default_color_map, Sw32_default_color_map,
       0, 0, 0, doc: /* Return the default color map.  */)
  (void)
{
  return w32_default_color_map ();
}

static Lisp_Object
w32_color_map_lookup (const char *colorname)
{
  Lisp_Object tail, ret = Qnil;

  block_input ();

  for (tail = Vw32_color_map; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem;

      elt = XCAR (tail);
      if (!CONSP (elt)) continue;

      tem = XCAR (elt);

      if (lstrcmpi (SSDATA (tem), colorname) == 0)
	{
	  ret = Fcdr (elt);
	  break;
	}

      maybe_quit ();
    }

  unblock_input ();

  return ret;
}


static void
add_system_logical_colors_to_map (Lisp_Object *system_colors)
{
  HKEY colors_key;

  /* Other registry operations are done with input blocked.  */
  block_input ();

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
			    NULL, NULL, (LPBYTE)color_buffer, &color_size)
	     == ERROR_SUCCESS)
	{
	  unsigned r, g, b;
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

  unblock_input ();
}


static Lisp_Object
x_to_w32_color (const char * colorname)
{
  register Lisp_Object ret = Qnil;

  block_input ();

  if (colorname[0] == '#')
    {
      /* Could be an old-style RGB Device specification.  */
      int size = strlen (colorname + 1);
      char *color = alloca (size + 1);

      strcpy (color, colorname + 1);
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
		  unblock_input ();
		  XSETINT (ret, colorval);
		  return ret;
		}
	      color = end;
	    }
	}
    }
  else if (strnicmp (colorname, "rgb:", 4) == 0)
    {
      const char *color;
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
	      unblock_input ();
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
      const char *color;
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
	      unblock_input ();
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

  unblock_input ();
  return ret;
}

void
w32_regenerate_palette (struct frame *f)
{
  struct w32_palette_entry * list;
  LOGPALETTE *          log_palette;
  HPALETTE              new_palette;
  int                   i;

  /* don't bother trying to create palette if not supported */
  if (! FRAME_DISPLAY_INFO (f)->has_palette)
    return;

  log_palette = (LOGPALETTE *)
    alloca (sizeof (LOGPALETTE) +
	     FRAME_DISPLAY_INFO (f)->num_colors * sizeof (PALETTEENTRY));
  log_palette->palVersion = 0x300;
  log_palette->palNumEntries = FRAME_DISPLAY_INFO (f)->num_colors;

  list = FRAME_DISPLAY_INFO (f)->color_list;
  for (i = 0;
       i < FRAME_DISPLAY_INFO (f)->num_colors;
       i++, list = list->next)
    log_palette->palPalEntry[i] = list->entry;

  new_palette = CreatePalette (log_palette);

  enter_crit ();

  if (FRAME_DISPLAY_INFO (f)->palette)
    DeleteObject (FRAME_DISPLAY_INFO (f)->palette);
  FRAME_DISPLAY_INFO (f)->palette = new_palette;

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
w32_map_color (struct frame *f, COLORREF color)
{
  struct w32_palette_entry * list = FRAME_DISPLAY_INFO (f)->color_list;

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
  list = xmalloc (sizeof (struct w32_palette_entry));
  SET_W32_COLOR (list->entry, color);
  list->refcount = 1;
  list->next = FRAME_DISPLAY_INFO (f)->color_list;
  FRAME_DISPLAY_INFO (f)->color_list = list;
  FRAME_DISPLAY_INFO (f)->num_colors++;

  /* set flag that palette must be regenerated */
  FRAME_DISPLAY_INFO (f)->regen_palette = TRUE;
}

void
w32_unmap_color (struct frame *f, COLORREF color)
{
  struct w32_palette_entry * list = FRAME_DISPLAY_INFO (f)->color_list;
  struct w32_palette_entry **prev = &FRAME_DISPLAY_INFO (f)->color_list;

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
	      FRAME_DISPLAY_INFO (f)->num_colors--;
	      break;
	    }
	  else
	    return;
	}
      prev = &list->next;
      list = list->next;
    }

  /* set flag that palette must be regenerated */
  FRAME_DISPLAY_INFO (f)->regen_palette = TRUE;
}
#endif


/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (struct frame *f, COLORREF *color)
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
w32_defined_color (struct frame *f, const char *color, XColor *color_def,
		   bool alloc_p)
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

	  if (entry == NULL && alloc_p)
	    {
	      /* not already mapped, so add to list */
	      entry = xmalloc (sizeof (struct w32_palette_entry));
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

static int
x_decode_color (struct frame *f, Lisp_Object arg, int def)
{
  XColor cdef;

  CHECK_STRING (arg);

  if (strcmp (SSDATA (arg), "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (SSDATA (arg), "white") == 0)
    return WHITE_PIX_DEFAULT (f);

  if ((FRAME_DISPLAY_INFO (f)->n_planes * FRAME_DISPLAY_INFO (f)->n_cbits) == 1)
    return def;

  /* w32_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (w32_defined_color (f, SSDATA (arg), &cdef, true))
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
x_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct w32_output *x = f->output_data.w32;
  PIX_TYPE fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_W32_WINDOW (f) != 0)
    {
      if (x->cursor_pixel == old_fg)
	{
	  x->cursor_pixel = fg;
	  x->cursor_gc->background = fg;
	}

      update_face_from_frame_parameter (f, Qforeground_color, arg);
      if (FRAME_VISIBLE_P (f))
	redraw_frame (f);
    }
}

void
x_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
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
x_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
#if 0
  Cursor cursor, nontext_cursor, mode_cursor, hand_cursor;
  int count;
#endif
  int mask_color;

  if (!EQ (Qnil, arg))
    f->output_data.w32->mouse_pixel
      = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  mask_color = FRAME_BACKGROUND_PIXEL (f);

  /* Don't let pointers be invisible.  */
  if (mask_color == f->output_data.w32->mouse_pixel
	&& mask_color == FRAME_BACKGROUND_PIXEL (f))
    f->output_data.w32->mouse_pixel = FRAME_FOREGROUND_PIXEL (f);

#if 0 /* TODO : Mouse cursor customization.  */
  block_input ();

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
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_sb_h_double_arrow);

  if (!NILP (Vx_window_vertical_drag_shape))
    {
      CHECK_NUMBER (Vx_window_vertical_drag_shape);
      vertical_drag_cursor
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_window_vertical_drag_shape));
    }
  else
    vertical_drag_cursor
      = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_sb_v_double_arrow);

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
  unblock_input ();

  update_face_from_frame_parameter (f, Qmouse_color, arg);
#endif /* TODO */
}

void
x_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
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
      block_input ();
      /* Update frame's cursor_gc.  */
      f->output_data.w32->cursor_gc->foreground = fore_pixel;
      f->output_data.w32->cursor_gc->background = pixel;

      unblock_input ();

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

static void
x_set_border_pixel (struct frame *f, int pix)
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
x_set_border_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  x_set_border_pixel (f, pix);
  update_face_from_frame_parameter (f, Qborder_color, arg);
}


void
x_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

void
x_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  bool result;

  if (NILP (arg) && NILP (oldval))
    return;

  if (STRINGP (arg) && STRINGP (oldval)
      && EQ (Fstring_equal (oldval, arg), Qt))
    return;

  if (SYMBOLP (arg) && SYMBOLP (oldval) && EQ (arg, oldval))
    return;

  block_input ();

  result = x_bitmap_icon (f, arg);
  if (result)
    {
      unblock_input ();
      error ("No icon window available");
    }

  unblock_input ();
}

void
x_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!NILP (arg) || NILP (oldval))
    return;

  fset_icon_name (f, arg);

#if 0
  if (f->output_data.w32->icon_bitmap != 0)
    return;

  block_input ();

  result = x_text_icon (f,
			SSDATA ((!NILP (f->icon_name)
				 ? f->icon_name
				 : !NILP (f->title)
				 ? f->title
				 : f->name)));

  if (result)
    {
      unblock_input ();
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
  unblock_input ();
#endif
}

/**
 * x_clear_under_internal_border:
 *
 * Clear area of frame F's internal border.  If the internal border face
 * of F has been specified (is not null), fill the area with that face.
 */
void
x_clear_under_internal_border (struct frame *f)
{
  int border = FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Clear border if it's larger than before.  */
  if (border != 0)
    {
      HDC hdc = get_frame_dc (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      struct face *face = FACE_FROM_ID_OR_NULL (f, INTERNAL_BORDER_FACE_ID);

      block_input ();
      if (face)
	{
	  /* Fill border with internal border face.  */
	  unsigned long color = face->background;

	  w32_fill_area (f, hdc, color, 0, FRAME_TOP_MARGIN_HEIGHT (f), width, border);
	  w32_fill_area (f, hdc, color, 0, 0, border, height);
	  w32_fill_area (f, hdc, color, width - border, 0, border, height);
	  w32_fill_area (f, hdc, color, 0, height - border, width, border);
	}
      else
	{
	  w32_clear_area (f, hdc, 0, FRAME_TOP_MARGIN_HEIGHT (f), width, border);
	  w32_clear_area (f, hdc, 0, 0, border, height);
	  w32_clear_area (f, hdc, width - border, 0, border, height);
	  w32_clear_area (f, hdc, 0, height - border, width, border);
	}
      release_frame_dc (f, hdc);
      unblock_input ();
    }
}


/**
 * x_set_internal_border_width:
 *
 * Set width of frame F's internal border to ARG pixels.  ARG < 0 is
 * treated like ARG = 0.
 */
void
x_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int border;

  CHECK_TYPE_RANGED_INTEGER (int, arg);
  border = max (XINT (arg), 0);

  if (border != FRAME_INTERNAL_BORDER_WIDTH (f))
    {
      f->internal_border_width = border;

      if (FRAME_X_WINDOW (f) != 0)
	{
	  adjust_frame_size (f, -1, -1, 3, false, Qinternal_border_width);

	  if (FRAME_VISIBLE_P (f))
	    x_clear_under_internal_border (f);
	}
    }
}


/**
 * x_set_menu_bar_lines:
 *
 * Set number of lines of frame F's menu bar to VALUE.  An integer
 * greater zero specifies 1 line and turns the menu bar on if it was off
 * before.  Any other value specifies 0 lines and turns the menu bar off
 * if it was on before.
 */
void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers in
     or split the minibuffer window.  Child frames don't like menu bars
     either.  */
  if (!FRAME_MINIBUF_ONLY_P (f) && !FRAME_PARENT_FRAME (f))
    {
      boolean old = FRAME_EXTERNAL_MENU_BAR (f);
      boolean new = (INTEGERP (value) && XINT (value) > 0) ? true : false;

      FRAME_MENU_BAR_LINES (f) = 0;
      FRAME_MENU_BAR_HEIGHT (f) = 0;

      if (old != new)
	{
	  FRAME_EXTERNAL_MENU_BAR (f) = new;

	  if (!old)
	    /* Make menu bar when there was none.  Emacs 25 waited until
	       the next redisplay for this to take effect.  */
	    set_frame_menubar (f, false, true);
	  else
	    {
	      /* Remove menu bar.  */
	      free_frame_menubar (f);

	      /* Adjust the frame size so that the client (text) dimensions
		 remain the same.  Note that we resize twice: The first time
		 upon a request from the window manager who wants to keep
		 the height of the outer rectangle (including decorations)
		 unchanged, and a second time because we want to keep the
		 height of the inner rectangle (without the decorations
		 unchanged).  */
	      adjust_frame_size (f, -1, -1, 2, false, Qmenu_bar_lines);
	    }

	  if (FRAME_W32_WINDOW (f))
	    x_clear_under_internal_border (f);

	  /* Don't store anything but 1 or 0 in the parameter.  */
	  store_frame_param (f, Qmenu_bar_lines, make_number (new ? 1 : 0));
	}
    }
}


/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL is
   the old number of tool bar lines (and is unused).  This function may
   change the height of all windows on frame F to match the new tool bar
   height.  By design, the frame's height doesn't change (but maybe it
   should if we don't get enough space otherwise).  */

void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an integer >= 0.  */
  if (INTEGERP (value) && XINT (value) >= 0)
    nlines = XFASTINT (value);
  else
    nlines = 0;

  x_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}


/* Set the pixel height of the tool bar of frame F to HEIGHT.  */
void
x_change_tool_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TOOL_BAR_HEIGHT (f);
  int lines = (height + unit - 1) / unit;
  Lisp_Object fullscreen;

  /* Make sure we redisplay all windows in this frame.  */
  windows_or_buffers_changed = 23;

  /* Recalculate tool bar and frame text sizes.  */
  FRAME_TOOL_BAR_HEIGHT (f) = height;
  FRAME_TOOL_BAR_LINES (f) = lines;
  /* Store `tool-bar-lines' and `height' frame parameters.  */
  store_frame_param (f, Qtool_bar_lines, make_number (lines));
  store_frame_param (f, Qheight, make_number (FRAME_LINES (f)));

  if (FRAME_W32_WINDOW (f) && FRAME_TOOL_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tool_bar_window))
    clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);

  /* Recalculate toolbar height.  */
  f->n_tool_bar_rows = 0;
  if (old_height == 0
      && (!f->after_make_frame
	  || NILP (frame_inhibit_implied_resize)
	  || (CONSP (frame_inhibit_implied_resize)
	      && NILP (Fmemq (Qtool_bar_lines, frame_inhibit_implied_resize)))))
    f->tool_bar_redisplayed = f->tool_bar_resized = false;

  adjust_frame_size (f, -1, -1,
		     ((!f->tool_bar_resized
		       && (NILP (fullscreen =
				 get_frame_param (f, Qfullscreen))
			   || EQ (fullscreen, Qfullwidth))) ? 1
		      : (old_height == 0 || height == 0) ? 2
		      : 4),
		     false, Qtool_bar_lines);

  f->tool_bar_resized = f->tool_bar_redisplayed;

  /* adjust_frame_size might not have done anything, garbage frame
     here.  */
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);
  if (FRAME_W32_WINDOW (f))
    x_clear_under_internal_border (f);
}

static void
w32_set_title_bar_text (struct frame *f, Lisp_Object name)
{
  if (FRAME_W32_WINDOW (f))
    {
      block_input ();
#ifdef __CYGWIN__
      GUI_FN (SetWindowText) (FRAME_W32_WINDOW (f),
			      GUI_SDATA (GUI_ENCODE_SYSTEM (name)));
#else
      /* The frame's title many times shows the name of the file
	 visited in the selected window's buffer, so it makes sense to
	 support non-ASCII characters outside of the current system
	 codepage in the title.  */
      if (w32_unicode_filenames)
	{
	  Lisp_Object encoded_title = ENCODE_UTF_8 (name);
	  wchar_t *title_w;
	  int tlen = pMultiByteToWideChar (CP_UTF8, 0, SSDATA (encoded_title),
					   -1, NULL, 0);

	  if (tlen > 0)
	    {
	      /* Windows truncates the title text beyond what fits on
		 a single line, so we can limit the length to some
		 reasonably large value, and use alloca.  */
	      if (tlen > 10000)
		tlen = 10000;
	      title_w = alloca ((tlen + 1) * sizeof (wchar_t));
	      pMultiByteToWideChar (CP_UTF8, 0, SSDATA (encoded_title), -1,
				    title_w, tlen);
	      title_w[tlen] = L'\0';
	      SetWindowTextW (FRAME_W32_WINDOW (f), title_w);
	    }
	  else	/* Conversion to UTF-16 failed, so we punt.  */
	    SetWindowTextA (FRAME_W32_WINDOW (f),
			    SSDATA (ENCODE_SYSTEM (name)));
	}
      else
	SetWindowTextA (FRAME_W32_WINDOW (f), SSDATA (ENCODE_SYSTEM (name)));
#endif
      unblock_input ();
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

static void
x_set_name (struct frame *f, Lisp_Object name, bool explicit)
{
  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 25;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the w32_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_DISPLAY_INFO (f)->w32_id_name,
		   SSDATA (f->name)))
	return;
      name = build_string (FRAME_DISPLAY_INFO (f)->w32_id_name);
    }
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  w32_set_title_bar_text (f, name);
}

/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
void
x_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, true);
}

/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, false);
}

/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

void
x_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 26;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;

  w32_set_title_bar_text (f, name);
}

void
x_set_scroll_bar_default_width (struct frame *f)
{
  int unit = FRAME_COLUMN_WIDTH (f);

  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = GetSystemMetrics (SM_CXVSCROLL);
  FRAME_CONFIG_SCROLL_BAR_COLS (f)
    = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
}


void
x_set_scroll_bar_default_height (struct frame *f)
{
  int unit = FRAME_LINE_HEIGHT (f);

  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = GetSystemMetrics (SM_CXHSCROLL);
  FRAME_CONFIG_SCROLL_BAR_LINES (f)
    = (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) + unit - 1) / unit;
}

/**
 * x_set_undecorated:
 *
 * Set frame F's `undecorated' parameter.  If non-nil, F's window-system
 * window is drawn without decorations, title, minimize/maximize boxes
 * and external borders.  This usually means that the window cannot be
 * dragged, resized, iconified, maximized or deleted with the mouse.  If
 * nil, draw the frame with all the elements listed above unless these
 * have been suspended via window manager settings.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  HWND hwnd = FRAME_W32_WINDOW (f);
  DWORD dwStyle = GetWindowLong (hwnd, GWL_STYLE);
  Lisp_Object border_width = Fcdr (Fassq (Qborder_width, f->param_alist));

  block_input ();
  if (!NILP (new_value) && !FRAME_UNDECORATED (f))
    {
      dwStyle = ((dwStyle & ~WS_THICKFRAME & ~WS_CAPTION)
		 | ((NUMBERP (border_width) && (XINT (border_width) > 0))
		    ? WS_BORDER : false));
      SetWindowLong (hwnd, GWL_STYLE, dwStyle);
      SetWindowPos (hwnd, HWND_TOP, 0, 0, 0, 0,
		    SWP_NOSIZE | SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE
		    | SWP_FRAMECHANGED);
      FRAME_UNDECORATED (f) = true;
    }
  else if (NILP (new_value) && FRAME_UNDECORATED (f))
    {
      SetWindowLong (hwnd, GWL_STYLE, dwStyle | WS_THICKFRAME | WS_CAPTION
		     | WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_SYSMENU);
      SetWindowPos (hwnd, HWND_TOP, 0, 0, 0, 0,
		    SWP_NOSIZE | SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE
		    | SWP_FRAMECHANGED);
      FRAME_UNDECORATED (f) = false;
    }
  unblock_input ();
}

/**
 * x_set_parent_frame:
 *
 * Set frame F's `parent-frame' parameter.  If non-nil, make F a child
 * frame of the frame specified by that parameter.  Technically, this
 * makes F's window-system window a child window of the parent frame's
 * window-system window.  If nil, make F's window-system window a
 * top-level window--a child of its display's root window.
 *
 * A child frame is clipped at the native edges of its parent frame.
 * Its `left' and `top' parameters specify positions relative to the
 * top-left corner of its parent frame's native rectangle.  Usually,
 * moving a parent frame moves all its child frames too, keeping their
 * position relative to the parent unaltered.  When a parent frame is
 * iconified or made invisible, its child frames are made invisible.
 * When a parent frame is deleted, its child frames are deleted too.
 *
 * A visible child frame always appears on top of its parent frame thus
 * obscuring parts of it.  When a frame has more than one child frame,
 * their stacking order is specified just as that of non-child frames
 * relative to their display.
 *
 * Whether a child frame has a menu or tool bar may be window-system or
 * window manager dependent.  It's advisable to disable both via the
 * frame parameter settings.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_parent_frame (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  struct frame *p = NULL;

  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_W32_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      error ("Invalid specification of `parent-frame'");
    }

  if (p != FRAME_PARENT_FRAME (f))
    {
      HWND hwnd = FRAME_W32_WINDOW (f);
      HWND hwnd_parent = p ? FRAME_W32_WINDOW (p) : NULL;
      HWND hwnd_value;

      block_input ();
      hwnd_value = SetParent (hwnd, hwnd_parent);
      unblock_input ();

      if (hwnd_value)
	fset_parent_frame (f, new_value);
      else
	{
	  store_frame_param (f, Qparent_frame, old_value);
	  error ("Reparenting frame failed");
	}
    }
}

/**
 * x_set_skip_taskbar:
 *
 * Set frame F's `skip-taskbar' parameter.  If non-nil, this should
 * remove F's icon from the taskbar associated with the display of F's
 * window-system window and inhibit switching to F's window via
 * <Alt>-<TAB>.  On Windows iconifying F will "roll in" its window at
 * the bottom of the desktop.  If nil, lift these restrictions.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_skip_taskbar (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      HWND hwnd = FRAME_W32_WINDOW (f);
      DWORD exStyle = GetWindowLong (hwnd, GWL_EXSTYLE);

      block_input ();
      /* Temporarily hide the window while changing its WS_EX_NOACTIVATE
	 setting.  */
      ShowWindow (hwnd, SW_HIDE);
      if (!NILP (new_value))
	SetWindowLong (hwnd, GWL_EXSTYLE, exStyle | WS_EX_NOACTIVATE);
      else
	SetWindowLong (hwnd, GWL_EXSTYLE, exStyle & ~WS_EX_NOACTIVATE);
      ShowWindow (hwnd, SW_SHOWNOACTIVATE);
      unblock_input ();

      FRAME_SKIP_TASKBAR (f) = !NILP (new_value);
    }
}

/**
 * x_set_no_focus_on_map:
 *
 * Set frame F's `no-focus-on-map' parameter which, if non-nil, means
 * that F's window-system window does not want to receive input focus
 * when it is mapped.  (A frame's window is mapped when the frame is
 * displayed for the first time and when the frame changes its state
 * from `iconified' or `invisible' to `visible'.)
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_no_focus_on_map (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
}

/**
 * x_set_no_accept_focus:
 *
 * Set frame F's `no-accept-focus' parameter which, if non-nil, hints
 * that F's window-system window does not want to receive input focus
 * via mouse clicks or by moving the mouse into it.
 *
 * If non-nil, this may have the unwanted side-effect that a user cannot
 * scroll a non-selected frame with the mouse.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
}

/**
 * x_set_z_group:
 *
 * Set frame F's `z-group' parameter.  If `above', F's window-system
 * window is displayed above all windows that do not have the `above'
 * property set.  If nil, F's window is shown below all windows that
 * have the `above' property set and above all windows that have the
 * `below' property set.  If `below', F's window is displayed below all
 * windows that do not have the `below' property set.
 *
 * Some window managers may not honor this parameter.  The value `below'
 * is not supported on Windows.
 */
static void
x_set_z_group (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  HWND hwnd = FRAME_W32_WINDOW (f);

  if (NILP (new_value))
    {
      block_input ();
      SetWindowPos (hwnd, HWND_NOTOPMOST, 0, 0, 0, 0,
		    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE
		    | SWP_NOOWNERZORDER);
      unblock_input ();
      FRAME_Z_GROUP (f) = z_group_none;
    }
  else if (EQ (new_value, Qabove))
    {
      block_input ();
      SetWindowPos (hwnd, HWND_TOPMOST, 0, 0, 0, 0,
		    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE
		    | SWP_NOOWNERZORDER);
      unblock_input ();
      FRAME_Z_GROUP (f) = z_group_above;
    }
  else if (EQ (new_value, Qabove_suspended))
    {
      block_input ();
      SetWindowPos (hwnd, HWND_NOTOPMOST, 0, 0, 0, 0,
		    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE
		    | SWP_NOOWNERZORDER);
      unblock_input ();
      FRAME_Z_GROUP (f) = z_group_above_suspended;
    }
  else if (EQ (new_value, Qbelow))
    error ("Value `below' for z-group is not supported on Windows");
  else
    error ("Invalid z-group specification");
}

/* Subroutines for creating a frame.  */

Cursor w32_load_cursor (LPCTSTR);

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

static LRESULT CALLBACK w32_wnd_proc (HWND, UINT, WPARAM, LPARAM);

#define INIT_WINDOW_CLASS(WC)			  \
  (WC).style = CS_HREDRAW | CS_VREDRAW;		  \
  (WC).lpfnWndProc = (WNDPROC) w32_wnd_proc;      \
  (WC).cbClsExtra = 0;                            \
  (WC).cbWndExtra = WND_EXTRA_BYTES;              \
  (WC).hInstance = hinst;                         \
  (WC).hIcon = LoadIcon (hinst, EMACS_CLASS);     \
  (WC).hCursor = w32_load_cursor (IDC_ARROW);     \
  (WC).hbrBackground = NULL;                      \
  (WC).lpszMenuName = NULL;                       \

static BOOL
w32_init_class (HINSTANCE hinst)
{
  if (w32_unicode_gui)
    {
      WNDCLASSW  uwc;
      INIT_WINDOW_CLASS(uwc);
      uwc.lpszClassName = L"Emacs";

      return RegisterClassW (&uwc);
    }
  else
    {
      WNDCLASS  wc;
      INIT_WINDOW_CLASS(wc);
      wc.lpszClassName = EMACS_CLASS;

      return RegisterClassA (&wc);
    }
}

static HWND
w32_createvscrollbar (struct frame *f, struct scroll_bar * bar)
{
  return CreateWindow ("SCROLLBAR", "",
		       /* Clip siblings so we don't draw over child
			  frames.  Apparently this is not always
			  sufficient so we also try to make bar windows
			  bottommost.  */
		       SBS_VERT | WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS,
		       /* Position and size of scroll bar.  */
		       bar->left, bar->top, bar->width, bar->height,
		       FRAME_W32_WINDOW (f), NULL, hinst, NULL);
}

static HWND
w32_createhscrollbar (struct frame *f, struct scroll_bar * bar)
{
  return CreateWindow ("SCROLLBAR", "",
		       /* Clip siblings so we don't draw over child
			  frames.  Apparently this is not always
			  sufficient so we also try to make bar windows
			  bottommost.  */
		       SBS_HORZ | WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS,
		       /* Position and size of scroll bar.  */
		       bar->left, bar->top, bar->width, bar->height,
		       FRAME_W32_WINDOW (f), NULL, hinst, NULL);
}

static void
w32_createwindow (struct frame *f, int *coords)
{
  HWND hwnd = NULL, parent_hwnd = NULL;
  RECT rect;
  int top, left;
  Lisp_Object border_width = Fcdr (Fassq (Qborder_width, f->param_alist));

  if (FRAME_PARENT_FRAME (f) && FRAME_W32_P (FRAME_PARENT_FRAME (f)))
    {
      parent_hwnd = FRAME_W32_WINDOW (FRAME_PARENT_FRAME (f));
      f->output_data.w32->dwStyle = WS_CHILD | WS_CLIPSIBLINGS;

      if (FRAME_UNDECORATED (f))
	{
	  /* If we want a thin border, specify it here.  */
	  if (NUMBERP (border_width) && (XINT (border_width) > 0))
	    f->output_data.w32->dwStyle |= WS_BORDER;
	}
      else
	/* To decorate a child frame, list all needed elements.  */
	f->output_data.w32->dwStyle |= (WS_THICKFRAME | WS_CAPTION
					| WS_MAXIMIZEBOX | WS_MINIMIZEBOX
					| WS_SYSMENU);
    }
  else if (FRAME_UNDECORATED (f))
    {
      /* All attempts to start with ~WS_OVERLAPPEDWINDOW or overlapped
	 with all other style elements negated failed here.  */
      f->output_data.w32->dwStyle = WS_POPUP;

      /* If we want a thin border, specify it here.  */
      if (NUMBERP (border_width) && (XINT (border_width) > 0))
	f->output_data.w32->dwStyle |= WS_BORDER;
    }
  else
    f->output_data.w32->dwStyle = WS_OVERLAPPEDWINDOW;

  /* Always clip children.  */
  f->output_data.w32->dwStyle |= WS_CLIPCHILDREN;

  rect.left = rect.top = 0;
  rect.right = FRAME_PIXEL_WIDTH (f);
  rect.bottom = FRAME_PIXEL_HEIGHT (f);

  AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		    FRAME_EXTERNAL_MENU_BAR (f) && !parent_hwnd);

  /* Do first time app init */
  w32_init_class (hinst);

  if (f->size_hint_flags & USPosition || f->size_hint_flags & PPosition)
    {
      left = f->left_pos;
      top = f->top_pos;
    }
  else
    {
      left = coords[0];
      top = coords[1];
    }

  FRAME_W32_WINDOW (f) = hwnd
    = CreateWindow (EMACS_CLASS, f->namebuf, f->output_data.w32->dwStyle,
		    left, top, rect.right - rect.left, rect.bottom - rect.top,
		    parent_hwnd, NULL, hinst, NULL);

  if (hwnd)
    {
      if (FRAME_SKIP_TASKBAR (f))
	SetWindowLong (hwnd, GWL_EXSTYLE,
		       GetWindowLong (hwnd, GWL_EXSTYLE) | WS_EX_NOACTIVATE);

      SetWindowLong (hwnd, WND_FONTWIDTH_INDEX, FRAME_COLUMN_WIDTH (f));
      SetWindowLong (hwnd, WND_LINEHEIGHT_INDEX, FRAME_LINE_HEIGHT (f));
      SetWindowLong (hwnd, WND_BORDER_INDEX, FRAME_INTERNAL_BORDER_WIDTH (f));
      SetWindowLong (hwnd, WND_VSCROLLBAR_INDEX, FRAME_SCROLL_BAR_AREA_WIDTH (f));
      SetWindowLong (hwnd, WND_HSCROLLBAR_INDEX, FRAME_SCROLL_BAR_AREA_HEIGHT (f));
      SetWindowLong (hwnd, WND_BACKGROUND_INDEX, FRAME_BACKGROUND_PIXEL (f));

      /* Enable drag-n-drop.  */
      DragAcceptFiles (hwnd, TRUE);

      /* Do this to discard the default setting specified by our parent. */
      ShowWindow (hwnd, SW_HIDE);

      /* Update frame positions. */
      GetWindowRect (hwnd, &rect);

      if (parent_hwnd)
	/* For a child window we have to get its coordinates wrt its
	   parent.  */
	MapWindowPoints (HWND_DESKTOP, parent_hwnd, (LPPOINT) &rect, 2);

      f->left_pos = rect.left;
      f->top_pos = rect.top;
    }
}

static void
my_post_msg (W32Msg * wmsg, HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  wmsg->msg.hwnd = hwnd;
  wmsg->msg.message = msg;
  wmsg->msg.wParam = wParam;
  wmsg->msg.lParam = lParam;
  wmsg->msg.time = GetMessageTime ();

  post_msg (wmsg);
}

#ifdef WINDOWSNT
/* The Windows keyboard hook callback.  */
static LRESULT CALLBACK
funhook (int code, WPARAM w, LPARAM l)
{
  INPUT inputs[2];
  HWND focus = GetFocus ();
  int console = 0;
  KBDLLHOOKSTRUCT const *hs = (KBDLLHOOKSTRUCT*)l;

  if (code < 0 || (hs->flags & LLKHF_INJECTED))
    return CallNextHookEx (0, code, w, l);

  /* The keyboard hook sees keyboard input on all processes (except
     elevated ones, when Emacs itself is not elevated).  As such,
     care must be taken to only filter out keyboard input when Emacs
     itself is on the foreground.

     GetFocus returns a non-NULL window if another application is active,
     and always for a console Emacs process.  For a console Emacs, determine
     focus by checking if the current foreground window is the process's
     console window.  */
  if (focus == NULL && kbdhook.console != NULL)
    {
      if (GetForegroundWindow () == kbdhook.console)
	{
	  focus = kbdhook.console;
	  console = 1;
	}
    }

  /* First, check hooks for the left and right Windows keys.  */
  if (hs->vkCode == VK_LWIN || hs->vkCode == VK_RWIN)
    {
      if (focus != NULL && (w == WM_KEYDOWN || w == WM_SYSKEYDOWN))
	{
	  /* The key is being pressed in an Emacs window.  */
	  if (hs->vkCode == VK_LWIN && !kbdhook.lwindown)
	    {
	      kbdhook.lwindown = 1;
	      kbdhook.winseen = 1;
	      kbdhook.winsdown++;
	    }
	  else if (hs->vkCode == VK_RWIN && !kbdhook.rwindown)
	    {
	      kbdhook.rwindown = 1;
	      kbdhook.winseen = 1;
	      kbdhook.winsdown++;
	    }
	  /* Returning 1 here drops the keypress without further processing.
	     If the keypress was allowed to go through, the normal Windows
	     hotkeys would take over.  */
	  return 1;
	}
      else if (kbdhook.winsdown > 0 && (w == WM_KEYUP || w == WM_SYSKEYUP))
	{
	  /* A key that has been captured earlier is being released now.  */
	  if (hs->vkCode == VK_LWIN && kbdhook.lwindown)
	    {
	      kbdhook.lwindown = 0;
	      kbdhook.winsdown--;
	    }
	  else if (hs->vkCode == VK_RWIN && kbdhook.rwindown)
	    {
	      kbdhook.rwindown = 0;
	      kbdhook.winsdown--;
	    }
	  if (kbdhook.winsdown == 0 && kbdhook.winseen)
	    {
	      if (!kbdhook.suppress_lone)
	        {
		  /* The Windows key was pressed, then released,
		     without any other key pressed simultaneously.
		     Normally, this opens the Start menu, but the user
		     can prevent this by setting the
		     w32-pass-[lr]window-to-system variable to
		     NIL.  */
		  if ((hs->vkCode == VK_LWIN && !NILP (Vw32_pass_lwindow_to_system)) ||
		      (hs->vkCode == VK_RWIN && !NILP (Vw32_pass_rwindow_to_system)))
		    {
		      /* Not prevented - Simulate the keypress to the system.  */
		      memset (inputs, 0, sizeof (inputs));
		      inputs[0].type = INPUT_KEYBOARD;
		      inputs[0].ki.wVk = hs->vkCode;
		      inputs[0].ki.wScan = hs->vkCode;
		      inputs[0].ki.dwFlags = KEYEVENTF_EXTENDEDKEY;
		      inputs[0].ki.time = 0;
		      inputs[1].type = INPUT_KEYBOARD;
		      inputs[1].ki.wVk = hs->vkCode;
		      inputs[1].ki.wScan = hs->vkCode;
		      inputs[1].ki.dwFlags
			= KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP;
		      inputs[1].ki.time = 0;
		      SendInput (2, inputs, sizeof (INPUT));
		    }
		  else if (focus != NULL)
		    {
		      /* When not passed to system, must simulate privately to Emacs.	 */
		      PostMessage (focus, WM_SYSKEYDOWN, hs->vkCode, 0);
		      PostMessage (focus, WM_SYSKEYUP, hs->vkCode, 0);
		    }
		}
	    }
	  if (kbdhook.winsdown == 0)
	    {
	      /* No Windows keys pressed anymore - clear the state flags.  */
	      kbdhook.suppress_lone = 0;
	      kbdhook.winseen = 0;
	    }
	  if (!kbdhook.send_win_up)
	    {
	      /* Swallow this release message, as not to confuse
		 applications who did not get to see the original
		 WM_KEYDOWN message either.  */
	      return 1;
	    }
	  kbdhook.send_win_up = 0;
	}
    }
  else if (kbdhook.winsdown > 0)
    {
      /* Some other key was pressed while a captured Win key is down.
	 This is either an Emacs registered hotkey combination, or a
	 system hotkey.	 */
      if ((kbdhook.lwindown && kbdhook.lwin_hooked[hs->vkCode]) ||
	  (kbdhook.rwindown && kbdhook.rwin_hooked[hs->vkCode]))
	{
	  /* Hooked Win-x combination, do not pass the keypress to Windows.  */
	  kbdhook.suppress_lone = 1;
	}
      else if (!kbdhook.suppress_lone)
	{
	  /* Unhooked S-x combination; simulate the combination now
	     (will be seen by the system).  */
	  memset (inputs, 0, sizeof (inputs));
	  inputs[0].type = INPUT_KEYBOARD;
	  inputs[0].ki.wVk = kbdhook.lwindown ? VK_LWIN : VK_RWIN;
	  inputs[0].ki.wScan = kbdhook.lwindown ? VK_LWIN : VK_RWIN;
	  inputs[0].ki.dwFlags = KEYEVENTF_EXTENDEDKEY;
	  inputs[0].ki.time = 0;
	  inputs[1].type = INPUT_KEYBOARD;
	  inputs[1].ki.wVk = hs->vkCode;
	  inputs[1].ki.wScan = hs->scanCode;
	  inputs[1].ki.dwFlags =
	    (hs->flags & LLKHF_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0;
	  inputs[1].ki.time = 0;
	  SendInput (2, inputs, sizeof (INPUT));
	  /* Stop processing of this Win sequence here; the
	     corresponding keyup messages will come through the normal
	     channel when the keys are released.  */
	  kbdhook.suppress_lone = 1;
	  kbdhook.send_win_up = 1;
	  /* Swallow the original keypress (as we want the Win key
	     down message simulated above to precede this real message).  */
	  return 1;
	}
    }

  /* Next, handle the registered Alt-* combinations.  */
  if ((w == WM_SYSKEYDOWN || w == WM_KEYDOWN)
      && kbdhook.alt_hooked[hs->vkCode]
      && focus != NULL
      && (GetAsyncKeyState (VK_MENU) & 0x8000))
    {
      /* Prevent the system from getting this Alt-* key - suppress the
	 message and post as a normal keypress to Emacs.  */
      if (console)
	{
	  INPUT_RECORD rec;
	  DWORD n;
	  rec.EventType = KEY_EVENT;
	  rec.Event.KeyEvent.bKeyDown = TRUE;
	  rec.Event.KeyEvent.wVirtualKeyCode = hs->vkCode;
	  rec.Event.KeyEvent.wVirtualScanCode = hs->scanCode;
	  rec.Event.KeyEvent.uChar.UnicodeChar = 0;
	  rec.Event.KeyEvent.dwControlKeyState =
	    ((GetAsyncKeyState (VK_LMENU) & 0x8000) ? LEFT_ALT_PRESSED : 0)
	    | ((GetAsyncKeyState (VK_RMENU) & 0x8000) ? RIGHT_ALT_PRESSED : 0)
	    | ((GetAsyncKeyState (VK_LCONTROL) & 0x8000) ? LEFT_CTRL_PRESSED : 0)
	    | ((GetAsyncKeyState (VK_RCONTROL) & 0x8000) ? RIGHT_CTRL_PRESSED : 0)
	    | ((GetAsyncKeyState (VK_SHIFT) & 0x8000) ? SHIFT_PRESSED : 0)
	    | ((hs->flags & LLKHF_EXTENDED) ? ENHANCED_KEY : 0);
	  if (w32_console_unicode_input)
	    WriteConsoleInputW (keyboard_handle, &rec, 1, &n);
	  else
	    WriteConsoleInputA (keyboard_handle, &rec, 1, &n);
	}
      else
	PostMessage (focus, w, hs->vkCode, 1 | (1<<29));
      return 1;
    }

  /* The normal case - pass the message through.  */
  return CallNextHookEx (0, code, w, l);
}

/* Set up the hook; can be called several times, with matching
   remove_w32_kbdhook calls.  */
void
setup_w32_kbdhook (void)
{
  kbdhook.hook_count++;

  /* This hook gets in the way of debugging, since when Emacs stops,
     its input thread stops, and there's nothing to process keyboard
     events, whereas this hook is global, and is invoked in the
     context of the thread that installed it.  So we don't install the
     hook if the process is being debugged. */
  if (w32_kbdhook_active)
    {
      IsDebuggerPresent_Proc is_debugger_present = (IsDebuggerPresent_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"), "IsDebuggerPresent");
      if (is_debugger_present && is_debugger_present ())
	return;
    }

  /* Hooking is only available on NT architecture systems, as
     indicated by the w32_kbdhook_active variable.  */
  if (kbdhook.hook_count == 1 && w32_kbdhook_active)
    {
      /* Get the handle of the Emacs console window.  As the
	 GetConsoleWindow function is only available on Win2000+, a
	 hackish workaround described in Microsoft KB article 124103
	 (https://support.microsoft.com/en-us/kb/124103) is used for
	 NT 4 systems.  */
      GetConsoleWindow_Proc get_console = (GetConsoleWindow_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"), "GetConsoleWindow");

      if (get_console != NULL)
	kbdhook.console = get_console ();
      else
        {
	  GUID guid;
	  wchar_t *oldTitle = malloc (1024 * sizeof(wchar_t));
	  wchar_t newTitle[64];
	  int i;

	  CoCreateGuid (&guid);
	  StringFromGUID2 (&guid, newTitle, 64);
	  if (newTitle != NULL)
	    {
	      GetConsoleTitleW (oldTitle, 1024);
	      SetConsoleTitleW (newTitle);
	      for (i = 0; i < 25; i++)
	        {
		  Sleep (40);
		  kbdhook.console = FindWindowW (NULL, newTitle);
		  if (kbdhook.console != NULL)
		    break;
		}
	      SetConsoleTitleW (oldTitle);
	    }
	  free (oldTitle);
	}

      /* Set the hook.  */
      kbdhook.hook = SetWindowsHookEx (WH_KEYBOARD_LL, funhook,
				       GetModuleHandle (NULL), 0);
    }
}

/* Remove the hook.  */
void
remove_w32_kbdhook (void)
{
  kbdhook.hook_count--;
  if (kbdhook.hook_count == 0 && w32_kbdhook_active)
    {
      UnhookWindowsHookEx (kbdhook.hook);
      kbdhook.hook = NULL;
    }
}
#endif	/* WINDOWSNT */

/* Mark a specific key combination as hooked, preventing it to be
   handled by the system.  */
static void
hook_w32_key (int hook, int modifier, int vkey)
{
  char *tbl = NULL;

  switch (modifier)
    {
    case VK_MENU:
      tbl = kbdhook.alt_hooked;
      break;
    case VK_LWIN:
      tbl = kbdhook.lwin_hooked;
      break;
    case VK_RWIN:
      tbl = kbdhook.rwin_hooked;
      break;
    }

  if (tbl != NULL && vkey >= 0 && vkey <= 255)
    {
       /* VK_ANY hooks all keys for this modifier */
       if (vkey == VK_ANY)
	 memset (tbl, (char)hook, 256);
       else
	 tbl[vkey] = (char)hook;
       /* Alt-<modifier>s should go through */
       kbdhook.alt_hooked[VK_MENU] = 0;
       kbdhook.alt_hooked[VK_LMENU] = 0;
       kbdhook.alt_hooked[VK_RMENU] = 0;
       kbdhook.alt_hooked[VK_CONTROL] = 0;
       kbdhook.alt_hooked[VK_LCONTROL] = 0;
       kbdhook.alt_hooked[VK_RCONTROL] = 0;
       kbdhook.alt_hooked[VK_SHIFT] = 0;
       kbdhook.alt_hooked[VK_LSHIFT] = 0;
       kbdhook.alt_hooked[VK_RSHIFT] = 0;
    }
}

#ifdef WINDOWSNT
/* Check the current Win key pressed state.  */
int
check_w32_winkey_state (int vkey)
{
  /* The hook code handles grabbing of the Windows keys and Alt-* key
     combinations reserved by the system.  Handling Alt is a bit
     easier, as Windows intends Alt-* shortcuts for application use in
     Windows; hotkeys such as Alt-tab and Alt-escape are special
     cases.  Win-* hotkeys, on the other hand, are primarily meant for
     system use.

     As a result, when we want Emacs to be able to grab the Win-*
     keys, we must swallow all Win key presses in a low-level keyboard
     hook.  Unfortunately, this means that the Emacs window procedure
     (and console input handler) never see the keypresses either.
     Thus, to check the modifier states properly, Emacs code must use
     the check_w32_winkey_state function that uses the flags directly
     updated by the hook callback.  */
  switch (vkey)
    {
    case VK_LWIN:
      return kbdhook.lwindown;
    case VK_RWIN:
      return kbdhook.rwindown;
    }
  return 0;
}
#endif	/* WINDOWSNT */

/* Reset the keyboard hook state.  Locking the workstation with Win-L
   leaves the Win key(s) "down" from the hook's point of view - the
   keyup event is never seen.  Thus, this function must be called when
   the system is locked.  */
static void
reset_w32_kbdhook_state (void)
{
  kbdhook.lwindown = 0;
  kbdhook.rwindown = 0;
  kbdhook.winsdown = 0;
  kbdhook.send_win_up = 0;
  kbdhook.suppress_lone = 0;
  kbdhook.winseen = 0;
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
reset_modifiers (void)
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

    memset (keystate, 0, sizeof (keystate));
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
sync_modifiers (void)
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
  /* Warning: The fact that VK_NUMLOCK is not treated as the other 2
     toggle keys is not an omission!  If you want to add it, you will
     have to make changes in the default sub-case of the WM_KEYDOWN
     switch, because if the NUMLOCK modifier is set, the code there
     will directly convert any key that looks like an ASCII letter,
     and also downcase those that look like upper-case ASCII.  */
  if (vkey == VK_CAPITAL)
    {
      if (NILP (Vw32_enable_caps_lock))
	return 0;
      else
	return (GetKeyState (vkey) & 0x1);
    }
  if (vkey == VK_SCROLL)
    {
      if (NILP (Vw32_scroll_lock_modifier)
	  /* w32-scroll-lock-modifier can be any non-nil value that is
	     not one of the modifiers, in which case it shall be ignored.  */
	  || !(   EQ (Vw32_scroll_lock_modifier, Qhyper)
	       || EQ (Vw32_scroll_lock_modifier, Qsuper)
	       || EQ (Vw32_scroll_lock_modifier, Qmeta)
	       || EQ (Vw32_scroll_lock_modifier, Qalt)
	       || EQ (Vw32_scroll_lock_modifier, Qcontrol)
	       || EQ (Vw32_scroll_lock_modifier, Qshift)))
	return 0;
      else
	return (GetKeyState (vkey) & 0x1);
    }
#ifdef WINDOWSNT
  if (w32_kbdhook_active && (vkey == VK_LWIN || vkey == VK_RWIN))
    return check_w32_winkey_state (vkey);
#endif

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
unsigned int w32_key_to_modifier (int);

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

  /* NB. This code runs in the input thread, asynchronously to the lisp
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
w32_get_modifiers (void)
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
construct_console_modifiers (void)
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

unsigned int map_keypad_keys (unsigned int, unsigned int);

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
   key code and modifier combination to capture.
   Note: This code is not used if keyboard hooks are active
   (Windows 2000 and later).  */
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
register_hot_keys (HWND hwnd)
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
unregister_hot_keys (HWND hwnd)
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

#if EMACSDEBUG
const char*
w32_name_of_message (UINT msg)
{
  unsigned i;
  static char buf[64];
  static const struct {
    UINT msg;
    const char* name;
  } msgnames[] = {
#define M(msg) { msg, # msg }
      M (WM_PAINT),
      M (WM_TIMER),
      M (WM_USER),
      M (WM_MOUSEMOVE),
      M (WM_LBUTTONUP),
      M (WM_KEYDOWN),
      M (WM_EMACS_KILL),
      M (WM_EMACS_CREATEWINDOW),
      M (WM_EMACS_DONE),
      M (WM_EMACS_CREATEVSCROLLBAR),
      M (WM_EMACS_CREATEHSCROLLBAR),
      M (WM_EMACS_SHOWWINDOW),
      M (WM_EMACS_SETWINDOWPOS),
      M (WM_EMACS_DESTROYWINDOW),
      M (WM_EMACS_TRACKPOPUPMENU),
      M (WM_EMACS_SETFOCUS),
      M (WM_EMACS_SETFOREGROUND),
      M (WM_EMACS_SETLOCALE),
      M (WM_EMACS_SETKEYBOARDLAYOUT),
      M (WM_EMACS_REGISTER_HOT_KEY),
      M (WM_EMACS_UNREGISTER_HOT_KEY),
      M (WM_EMACS_TOGGLE_LOCK_KEY),
      M (WM_EMACS_TRACK_CARET),
      M (WM_EMACS_DESTROY_CARET),
      M (WM_EMACS_SHOW_CARET),
      M (WM_EMACS_HIDE_CARET),
      M (WM_EMACS_SETCURSOR),
      M (WM_EMACS_SHOWCURSOR),
      M (WM_EMACS_PAINT),
      M (WM_CHAR),
#undef M
      { 0, 0 }
  };

  for (i = 0; msgnames[i].name; ++i)
    if (msgnames[i].msg == msg)
      return msgnames[i].name;

  sprintf (buf, "message 0x%04x", (unsigned)msg);
  return buf;
}
#endif /* EMACSDEBUG */

/* Here's an overview of how Emacs input works in GUI sessions on
   MS-Windows.  (For description of non-GUI input, see the commentary
   before w32_console_read_socket in w32inevt.c.)

   System messages are read and processed by w32_msg_pump below.  This
   function runs in a separate thread.  It handles a small number of
   custom WM_EMACS_* messages (posted by the main thread, look for
   PostMessage calls), and dispatches the rest to w32_wnd_proc, which
   is the main window procedure for the entire Emacs application.

   w32_wnd_proc also runs in the same separate input thread.  It
   handles some messages, mostly those that need GDI calls, by itself.
   For the others, it calls my_post_msg, which inserts the messages
   into the input queue serviced by w32_read_socket.

   w32_read_socket runs in the main (a.k.a. "Lisp") thread, and is
   called synchronously from keyboard.c when it is known or suspected
   that some input is available.  w32_read_socket either handles
   messages immediately, or converts them into Emacs input events and
   stuffs them into kbd_buffer, where kbd_buffer_get_event can get at
   them and process them when read_char and its callers require
   input.

   Under Cygwin with the W32 toolkit, the use of /dev/windows with
   select(2) takes the place of w32_read_socket.

   */

/* Main message dispatch loop. */

static void
w32_msg_pump (deferred_msg * msg_buf)
{
  MSG msg;
  WPARAM result;
  HWND focus_window;

  msh_mousewheel = RegisterWindowMessage (MSH_MOUSEWHEEL);

  while ((w32_unicode_gui ? GetMessageW : GetMessageA) (&msg, NULL, 0, 0))
    {

      /* DebPrint (("w32_msg_pump: %s time:%u\n", */
      /*            w32_name_of_message (msg.message), msg.time)); */

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
		 the patch for XP is not publicly available until XP SP3,
		 and older versions will never be patched.  */
	      CoInitialize (NULL);
	      w32_createwindow ((struct frame *) msg.wParam,
				(int *) msg.lParam);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
		emacs_abort ();
	      break;
	    case WM_EMACS_SETLOCALE:
	      SetThreadLocale (msg.wParam);
	      /* Reply is not expected.  */
	      break;
	    case WM_EMACS_SETKEYBOARDLAYOUT:
	      result = (WPARAM) ActivateKeyboardLayout ((HKL) msg.wParam, 0);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE,
				      result, 0))
		emacs_abort ();
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
	      XSETCAR (make_lisp_ptr ((void *)msg.lParam, Lisp_Cons), Qnil);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
		emacs_abort ();
	      break;
	    case WM_EMACS_TOGGLE_LOCK_KEY:
	      {
		int vk_code = (int) msg.wParam;
		int cur_state = (GetKeyState (vk_code) & 1);
		int new_state = msg.lParam;

		if (new_state == -1
		    || ((new_state & 1) != cur_state))
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
		  emacs_abort ();
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
	  if (w32_unicode_gui)
	    DispatchMessageW (&msg);
	  else
	    DispatchMessageA (&msg);
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
    emacs_abort ();

  /* It is an error to send a message that is already deferred.  */
  if (find_deferred_msg (hwnd, msg) != NULL)
    emacs_abort ();

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
    /* Message may have been canceled, so don't abort.  */
    return;

  msg_buf->result = result;
  msg_buf->completed = 1;

  /* Ensure input thread is woken so it notices the completion.  */
  PostThreadMessage (dwWindowsThreadId, WM_NULL, 0, 0);
}

static void
cancel_all_deferred_msgs (void)
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

DWORD WINAPI w32_msg_worker (void *);

DWORD WINAPI
w32_msg_worker (void *arg)
{
  MSG msg;
  deferred_msg dummy_buf;

  /* Ensure our message queue is created */

  PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

  if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
    emacs_abort ();

  memset (&dummy_buf, 0, sizeof (dummy_buf));
  dummy_buf.w32msg.msg.hwnd = NULL;
  dummy_buf.w32msg.msg.message = WM_NULL;

  /* This is the initial message loop which should only exit when the
     application quits.  */
  w32_msg_pump (&dummy_buf);

  return 0;
}

static void
signal_user_input (void)
{
  /* Interrupt any lisp that wants to be interrupted by input.  */
  if (!NILP (Vthrow_on_input))
    {
      Vquit_flag = Vthrow_on_input;
      /* Calling maybe_quit from this thread is a bad idea, since this
	 unwinds the stack of the Lisp thread, and the Windows runtime
	 rightfully barfs.  */
    }
}


static void
post_character_message (HWND hwnd, UINT msg,
			WPARAM wParam, LPARAM lParam,
			DWORD modifiers)
{
  W32Msg wmsg;

  wmsg.dwModifiers = modifiers;

  /* Detect quit_char and set quit-flag directly.  Note that we
     still need to post a message to ensure the main thread will be
     woken up if blocked in sys_select, but we do NOT want to post
     the quit_char message itself (because it will usually be as if
     the user had typed quit_char twice).  Instead, we post a dummy
     message that has no particular effect.  */
  {
    int c = wParam;
    if (isalpha (c) && wmsg.dwModifiers == ctrl_modifier)
      c = make_ctrl_char (c) & 0377;
    if (c == quit_char
	|| (wmsg.dwModifiers == 0
	    && w32_quit_key && wParam == w32_quit_key))
      {
	Vquit_flag = Qt;

	/* The choice of message is somewhat arbitrary, as long as
	   the main thread handler just ignores it.  */
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
	   a response from the lisp thread (although that would at least
	   solve the deadlock problem above), because we want to be able
	   to receive C-g to interrupt the lisp thread.  */
	cancel_all_deferred_msgs ();
      }
    else
      signal_user_input ();
  }

  my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
}

static int
get_wm_chars (HWND aWnd, int *buf, int buflen, int ignore_ctrl, int ctrl,
	      int *ctrl_cnt, int *is_dead, int vk, int exp)
{
  MSG msg;
  /* If doubled is at the end, ignore it.  */
  int i = buflen, doubled = 0, code_unit;

  if (ctrl_cnt)
    *ctrl_cnt = 0;
  if (is_dead)
    *is_dead = -1;
  eassert (w32_unicode_gui);
  while (buflen
	 /* Should be called only when w32_unicode_gui:  */
	 && PeekMessageW (&msg, aWnd, WM_KEYFIRST, WM_KEYLAST,
			  PM_NOREMOVE | PM_NOYIELD)
	 && (msg.message == WM_CHAR || msg.message == WM_SYSCHAR
	     || msg.message == WM_DEADCHAR || msg.message == WM_SYSDEADCHAR
	     || msg.message == WM_UNICHAR))
    {
      /* We extract character payload, but in this call we handle only the
	 characters which come BEFORE the next keyup/keydown message.  */
      int dead;

      GetMessageW (&msg, aWnd, msg.message, msg.message);
      dead = (msg.message == WM_DEADCHAR || msg.message == WM_SYSDEADCHAR);
      if (is_dead)
	*is_dead = (dead ? msg.wParam : -1);
      if (dead)
	continue;
      code_unit = msg.wParam;
      if (doubled)
	{
	  /* Had surrogate.  */
	  if (msg.message == WM_UNICHAR
	      || code_unit < 0xDC00 || code_unit > 0xDFFF)
	    { /* Mismatched first surrogate.
		 Pass both code units as if they were two characters.  */
	      *buf++ = doubled;
	      if (!--buflen)
		return i; /* Drop the 2nd char if at the end of the buffer.  */
	    }
	  else /* see https://en.wikipedia.org/wiki/UTF-16 */
	    code_unit = (doubled << 10) + code_unit - 0x35FDC00;
	  doubled = 0;
	}
      else if (code_unit >= 0xD800 && code_unit <= 0xDBFF)
	{
	  /* Handle mismatched 2nd surrogate the same as a normal character.  */
	  doubled = code_unit;
	  continue;
	}

      /* The only "fake" characters delivered by ToUnicode() or
	 TranslateMessage() are:
	 0x01 .. 0x1a for Ctrl-letter, Enter, Tab, Ctrl-Break, Esc, Backspace
	 0x00 and 0x1b .. 0x1f for Control- []\@^_
	 0x7f for Control-BackSpace
	 0x20 for Control-Space */
      if (ignore_ctrl
	  && (code_unit < 0x20 || code_unit == 0x7f
	      || (code_unit == 0x20 && ctrl)))
	{
	  /* Non-character payload in a WM_CHAR
	     (Ctrl-something pressed, see above).  Ignore, and report.  */
	  if (ctrl_cnt)
	    (*ctrl_cnt)++;
	  continue;
	}
      /* Traditionally, Emacs would ignore the character payload of VK_NUMPAD*
	 keys, and would treat them later via `function-key-map'.  In addition
	 to usual 102-key NUMPAD keys, this map also treats `kp-'-variants of
	 space, tab, enter, separator, equal.  TAB  and EQUAL, apparently,
	 cannot be generated on Win-GUI branch.  ENTER is already handled
	 by the code above.  According to `lispy_function_keys', kp_space is
	 generated by not-extended VK_CLEAR.  (kp-tab !=  VK_OEM_NEC_EQUAL!).

	 We do similarly for backward-compatibility, but ignore only the
	 characters restorable later by `function-key-map'.  */
      if (code_unit < 0x7f
	  && ((vk >= VK_NUMPAD0 && vk <= VK_DIVIDE)
	      || (exp && ((vk >= VK_PRIOR && vk <= VK_DOWN) ||
		     vk == VK_INSERT || vk == VK_DELETE || vk == VK_CLEAR)))
	  && strchr ("0123456789/*-+.,", code_unit))
	continue;
      *buf++ = code_unit;
      buflen--;
    }
  return i - buflen;
}

#ifdef DBG_WM_CHARS
#  define FPRINTF_WM_CHARS(ARG)	fprintf ARG
#else
#  define FPRINTF_WM_CHARS(ARG)	(void)0
#endif

/* This is a heuristic only.  This is supposed to track the state of the
   finite automaton in the language environment of Windows.

   However, separate windows (if with the same different language
   environments!) should  have different values.  Moreover, switching to a
   non-Emacs window with the same language environment, and using (dead)keys
   there would change the value stored in the kernel, but not this value.  */
/* A layout may emit deadkey=0.  It looks like this would reset the state
   of the kernel's finite automaton (equivalent to emiting 0-length string,
   which is otherwise impossible in the dead-key map of a layout).
   Be ready to treat the case when this delivers WM_(SYS)DEADCHAR. */
static int after_deadkey = -1;

static int
deliver_wm_chars (int do_translate, HWND hwnd, UINT msg, UINT wParam,
		  UINT lParam, int legacy_alt_meta)
{
  /* An "old style" keyboard description may assign up to 125 UTF-16 code
     points to a keypress.
     (However, the "old style" TranslateMessage() would deliver at most 16 of
     them.)  Be on a safe side, and prepare to treat many more.  */
  int ctrl_cnt, buf[1024], count, is_dead, after_dead = (after_deadkey > 0);

  /* Since the keypress processing logic of Windows has a lot of state, it
     is important to call TranslateMessage() for every keyup/keydown, AND
     do it exactly once.  (The actual change of state is done by
     ToUnicode[Ex](), which is called by TranslateMessage().  So one can
     call ToUnicode[Ex]() instead.)

     The "usual" message pump calls TranslateMessage() for EVERY event.
     Emacs calls TranslateMessage() very selectively (is it needed for doing
     some tricky stuff with Win95???  With newer Windows, selectiveness is,
     most probably, not needed -- and harms a lot).

     So, with the usual message pump, the following call to TranslateMessage()
     is not needed (and is going to be VERY harmful).  With Emacs' message
     pump, the call is needed.  */
  if (do_translate)
    {
      MSG windows_msg = { hwnd, msg, wParam, lParam, 0, {0,0} };

      windows_msg.time = GetMessageTime ();
      TranslateMessage (&windows_msg);
    }
  count = get_wm_chars (hwnd, buf, sizeof (buf)/sizeof (*buf), 1,
			/* The message may have been synthesized by
			   who knows what; be conservative.  */
			modifier_set (VK_LCONTROL)
			  || modifier_set (VK_RCONTROL)
			  || modifier_set (VK_CONTROL),
			&ctrl_cnt, &is_dead, wParam,
			(lParam & 0x1000000L) != 0);
  if (count)
    {
      W32Msg wmsg;
      DWORD console_modifiers = construct_console_modifiers ();
      int *b = buf, strip_ExtraMods = 1, hairy = 0;
      const char *type_CtrlAlt = NULL;

      /*  XXXX In fact, there may be another case when we need to do the same:
	       What happens if the string defined in the LIGATURES has length
	       0?  Probably, we will get count==0, but the state of the finite
	       automaton would reset to 0???  */
     after_deadkey = -1;

      /* wParam is checked when converting CapsLock to Shift; this is a clone
	 of w32_get_key_modifiers ().  */
      wmsg.dwModifiers = w32_kbd_mods_to_emacs (console_modifiers, wParam);

      /* What follows is just heuristics; the correct treatement requires
	 non-destructive ToUnicode():
	   http://search.cpan.org/~ilyaz/UI-KeyboardLayout/lib/UI/KeyboardLayout.pm#Can_an_application_on_Windows_accept_keyboard_events?_Part_IV:_application-specific_modifiers

	 What one needs to find is:
	   * which of the present modifiers AFFECT the resulting char(s)
	     (so should be stripped, since their EFFECT is "already
	      taken into account" in the string in buf), and
	   * which modifiers are not affecting buf, so should be reported to
	     the application for further treatment.

	 Example: assume that we know:
	   (A) lCtrl+rCtrl+rAlt modifiers with VK_A key produce a Latin "f"
	       ("may be logical" in JCUKEN-flavored Russian keyboard flavors);
	   (B) removing any of lCtrl, rCtrl, rAlt changes the produced char;
	   (C) Win-modifier is not affecting the produced character
	       (this is the common case: happens with all "standard" layouts).

	 Suppose the user presses Win+lCtrl+rCtrl+rAlt modifiers with VK_A.
	 What is the intent of the user?  We need to guess the intent to decide
	 which event to deliver to the application.

	 This looks like a reasonable logic: since Win- modifier doesn't affect
	 the output string, the user was pressing Win for SOME OTHER purpose.
	 So the user wanted to generate Win-SOMETHING event.  Now, what is
	 something?  If one takes the mantra that "character payload is more
	 important than the combination of keypresses which resulted in this
	 payload", then one should ignore lCtrl+rCtrl+rAlt, ignore VK_A, and
	 assume that the user wanted to generate Win-f.

	 Unfortunately, without non-destructive ToUnicode(), checking (B),(C)
	 is out of question.  So we use heuristics (hopefully, covering
	 99.9999% of cases).  */

      /* Another thing to watch for is a possibility to use AltGr-* and
	 Ctrl-Alt-* with different semantic.

	 Background: the layout defining the KLLF_ALTGR bit are treated
	 specially by the kernel: when VK_RMENU (=rightAlt, =AltGr) is pressed
	 (released), a press (release) of VK_LCONTROL is emulated (unless Ctrl
	 is already down).  As a result, any press/release of AltGr is seen
	 by applications as a press/release of lCtrl AND rAlt.  This is
	 applicable, in particular, to ToUnicode[Ex]().  (Keyrepeat is covered
	 the same way!)

	   NOTE: it IS possible to see bare rAlt even with KLLF_ALTGR; but this
	   requires a good finger coordination: doing (physically)
	     Down-lCtrl Down-rAlt Up-lCtrl Down-a
	   (doing quick enough, so that key repeat of rAlt [which would
	   generate new "fake" Down-lCtrl events] does not happens before 'a'
	   is down) results in no "fake" events, so the application will see
	   only rAlt down when 'a' is pressed.  (However, fake Up-lCtrl WILL
	   be generated when rAlt goes UP.)

	   In fact, note also that KLLF_ALTGR does not prohibit construction of
	   rCtrl-rAlt (just press them in this order!).

	 Moreover: "traditional" layouts do not define distinct modifier-masks
	 for VK_LMENU and VK_RMENU (same for VK_L/RCONTROL).  Instead, they
	 rely on the KLLF_ALTGR bit to make the behavior of VK_LMENU and
	 VK_RMENU distinct.  As a corollary, for such layouts, the produced
	 character is the same for AltGr-* (=rAlt-*) and Ctrl-Alt-* (in any
	 combination of handedness).  For description of masks, see

	   http://search.cpan.org/~ilyaz/UI-KeyboardLayout/lib/UI/KeyboardLayout.pm#Keyboard_input_on_Windows,_Part_I:_what_is_the_kernel_doing?

	 By default, Emacs was using these coincidences via the following
	 heuristics: it was treating:
	  (*) keypresses with lCtrl-rAlt modifiers as if they are carrying
	      ONLY the character payload (no matter what the actual keyboard
	      was defining: if lCtrl-lAlt-b was delivering U+05df=beta, then
	      Emacs saw [beta]; if lCtrl-lAlt-b was undefined in the layout,
	      the keypress was completely ignored), and
	  (*) keypresses with the other combinations of handedness of Ctrl-Alt
	      modifiers (e.g., lCtrl-lAlt) as if they NEVER carry a character
	      payload (so they were reported "raw": if lCtrl-lAlt-b was
	      delivering beta, then Emacs saw event [C-A-b], and not [beta]).
	 This worked good for "traditional" layouts: users could type both
	 AltGr-x and Ctrl-Alt-x, and one was a character, another a bindable
	 event.

	 However, for layouts which deliver different characters for AltGr-x
	 and lCtrl-lAlt-x, this scheme makes the latter character unaccessible
	 in Emacs.  While it is easy to access functionality of [C-M-x] in
	 Emacs by other means (for example, by the `controlify' prefix, or
	 using lCtrl-rCtrl-x, or rCtrl-rAlt-x [in this order]), missing
	 characters cannot be reconstructed without a tedious manual work.  */

      /* These two cases are often going to be distinguishable, since at most
	 one of these character is defined with KBDCTRL | KBDMENU modifier
	 bitmap.  (This heuristic breaks if both lCtrl-lAlt- AND lCtrl-rAlt-
	 are translated to modifier bitmaps distinct from KBDCTRL | KBDMENU,
	 or in the cases when lCtrl-lAlt-* and lCtrl-rAlt-* are generally
	 different, but lCtrl-lAlt-x and lCtrl-rAlt-x happen to deliver the
	 same character.)

	 So we have 2 chunks of info:
	   (A) is it lCtrl-rAlt-, or lCtrl-lAlt, or some other combination?
	   (B) is the delivered character defined with KBDCTRL | KBDMENU bits?
	 Basing on (A) and (B), we should decide whether to ignore the
	 delivered character.  (Before, Emacs was completely ignoring (B), and
	 was treating the 3-state of (A) as a bit.)  This means that we have 6
	 bits of customization.

	 Additionally, a presence of two Ctrl down may be AltGr-rCtrl-.  */

      /* Strip all non-Shift modifiers if:
	- more than one UTF-16 code point delivered (can't call VkKeyScanW ())
	- or the character is a result of combining with a prefix key.  */
      if (!after_dead && count == 1 && *b < 0x10000)
	{
	  if (console_modifiers & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED)
	      && console_modifiers & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	    {
	      type_CtrlAlt = "bB";   /* generic bindable Ctrl-Alt- modifiers */
	      if ((console_modifiers & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
		  == (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
		 /* double-Ctrl:
		    e.g. AltGr-rCtrl on some layouts (in this order!) */
		type_CtrlAlt = "dD";
	      else if ((console_modifiers
			& (LEFT_CTRL_PRESSED | LEFT_ALT_PRESSED))
		       == (LEFT_CTRL_PRESSED | LEFT_ALT_PRESSED))
		type_CtrlAlt = "lL"; /* Ctrl-Alt- modifiers on the left */
	      else if (!NILP (Vw32_recognize_altgr)
		       && ((console_modifiers
			    & (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED)))
			  == (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
		type_CtrlAlt = "gG"; /* modifiers as in AltGr */
	    }
	  else if (wmsg.dwModifiers & (alt_modifier | meta_modifier)
		   || ((console_modifiers
			& (LEFT_WIN_PRESSED | RIGHT_WIN_PRESSED
			   | APPS_PRESSED | SCROLLLOCK_ON))))
	    {
	      /* Pure Alt (or combination of Alt, Win, APPS, scrolllock.  */
	      type_CtrlAlt = "aA";
	    }
	  if (type_CtrlAlt)
	    {
	      /* Out of bound bitmap:  */
	      SHORT r = VkKeyScanW (*b), bitmap = 0x1FF;

	      FPRINTF_WM_CHARS((stderr, "VkKeyScanW %#06x %#04x\n", (int)r,
				wParam));
	      if ((r & 0xFF) == wParam)
		bitmap = r>>8; /* *b is reachable via simple interface */
	      else
		{
		  /* VkKeyScanW() (essentially) returns the FIRST key with
		     the specified character; so here the pressed key is the
		     SECONDARY key producing the character.

		     Essentially, we have no information about the "role" of
		     modifiers on this key: which contribute into the
		     produced character (so "are consumed"), and which are
		     "extra" (must attache to bindable events).

		     The default above would consume ALL modifiers, so the
		     character is reported "as is".  However, on many layouts
		     the ordering of the keys (in the layout table) is not
		     thought out well, so the "secondary" keys are often those
		     which the users would prefer to use with Alt-CHAR.
		     (Moreover - with e.g. Czech-QWERTY - the ASCII
		     punctuation is accessible from two equally [nu]preferable
		     AltGr-keys.)

		     SO:   Heuristic: if the reported char is ASCII, AND Meta
		     modifier is a candidate, behave as if Meta is present
		     (fallback to the legacy branch; bug#23251).

		     (This would break layouts
		     - delivering ASCII characters
		     - on SECONDARY keys
		     - with not Shift/AltGr-like modifier combinations.
		     All 3 conditions together must be pretty exotic
		     cases - and a workaround exists: use "primary" keys!) */
		  if (*b < 0x80
		      && (wmsg.dwModifiers
			  & (alt_modifier | meta_modifier
			     | super_modifier | hyper_modifier)))
		    return 0;
		}
	      if (*type_CtrlAlt == 'a') /* Simple Alt seen */
		{
		  if ((bitmap & ~1) == 0) /* 1: KBDSHIFT */
		    {
		      /* In "traditional" layouts, Alt without Ctrl does not
			 change the delivered character.  This detects this
			 situation; it is safe to report this as Alt-something
			  -- as opposed to delivering the reported character
			  without modifiers.  */
		      if (legacy_alt_meta
			  && *b > 0x7f && ('A' <= wParam && wParam <= 'Z'))
			/* For backward-compatibility with older Emacsen, let
			   this be processed by another branch below (which
			   would convert it to Alt-Latin char via wParam).  */
			return 0;
		    }
		  else
		    hairy = 1;
		}
	      /* Check whether the delivered character(s) is accessible via
		 KBDCTRL | KBDALT ( | KBDSHIFT ) modifier mask (which is 7).  */
	      else if ((bitmap & ~1) != 6)
		{
		  /* The character is not accessible via plain Ctrl-Alt(-Shift)
		     (which is, probably, same as AltGr) modifiers.
		     Either it was after a prefix key, or is combined with
		     modifier keys which we don't see, or there is an asymmetry
		     between left-hand and right-hand modifiers, or other hairy
		     stuff.  */
		  hairy = 1;
		}
	      /* The best solution is to delegate these tough (but rarely
		 needed) choices to the user.  Temporarily (???), it is
		 implemented as C macros.

		 Essentially, there are 3 things to do: return 0 (handle to the
		 legacy processing code [ignoring the character payload]; keep
		 some modifiers (so that they will be processed by the binding
		 system [on top of the character payload]; strip modifiers [so
		 that `self-insert' is going to be triggered with the character
		 payload]).

		 The default below should cover 99.9999% of cases:
		   (a) strip Alt- in the hairy case only;
		       (stripping = not ignoring)
		   (l) for lAlt-lCtrl, ignore the char in simple cases only;
		   (g) for what looks like AltGr, ignore the modifiers;
		   (d) for what looks like lCtrl-rCtrl-Alt (probably
		       AltGr-rCtrl), ignore the character in simple cases only;
		   (b) for other cases of Ctrl-Alt, ignore the character in
		       simple cases only.

		 Essentially, in all hairy cases, and in looks-like-AltGr case,
		 we keep the character, ignoring the modifiers.  In all the
		 other cases, we ignore the delivered character.  */
#define S_TYPES_TO_IGNORE_CHARACTER_PAYLOAD "aldb"
#define S_TYPES_TO_REPORT_CHARACTER_PAYLOAD_WITH_MODIFIERS ""
	      if (strchr (S_TYPES_TO_IGNORE_CHARACTER_PAYLOAD,
			  type_CtrlAlt[hairy]))
		return 0;
	      /* If in neither list, report all the modifiers we see COMBINED
		 WITH the reported character.  */
	      if (strchr (S_TYPES_TO_REPORT_CHARACTER_PAYLOAD_WITH_MODIFIERS,
			  type_CtrlAlt[hairy]))
		strip_ExtraMods = 0;
	    }
	}
      if (strip_ExtraMods)
	wmsg.dwModifiers = wmsg.dwModifiers & shift_modifier;

      signal_user_input ();
      while (count--)
	{
	  FPRINTF_WM_CHARS((stderr, "unichar %#06x\n", *b));
	  my_post_msg (&wmsg, hwnd, WM_UNICHAR, *b++, lParam);
	}
      if (!ctrl_cnt) /* Process ALSO as ctrl */
	return 1;
      else
	FPRINTF_WM_CHARS((stderr, "extra ctrl char\n"));
      return -1;
    }
  else if (is_dead >= 0)
    {
      FPRINTF_WM_CHARS((stderr, "dead %#06x\n", is_dead));
      after_deadkey = is_dead;
      return 1;
    }
  return 0;
}

/* Main window procedure */

static LRESULT CALLBACK
w32_wnd_proc (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
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
	memset (&update_rect, 0, sizeof (update_rect));

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

	    /* Change the message type to prevent Windows from
	       combining WM_PAINT messages in the Lisp thread's queue,
	       since Windows assumes that each message queue is
	       dedicated to one frame and does not bother checking
	       that hwnd matches before combining them.  */
	    my_post_msg (&wmsg, hwnd, WM_EMACS_PAINT, wParam, lParam);

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

      /* The state of the finite automaton is separate per every input
	 language environment (so it does not change when one switches
	 to a different window with the same environment).  Moreover,
	 the experiments show that the state is not remembered when
	 one switches back to the pre-previous environment.  */
      after_deadkey = -1;

      /* XXXX??? What follows is a COMPLETE misunderstanding of Windows!  */

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
      if (w32_use_fallback_wm_chars_method)
	wParam = map_keypad_keys (wParam, (lParam & 0x1000000L) != 0);

      windows_translate = 0;

      switch (wParam)
	{
	case VK_LWIN:
	  if (!w32_kbdhook_active && NILP (Vw32_pass_lwindow_to_system))
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
	  if (!w32_kbdhook_active && NILP (Vw32_pass_rwindow_to_system))
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
	  if (w32_unicode_gui && !w32_use_fallback_wm_chars_method)
	    {
	      /* If this event generates characters or deadkeys, do
		 not interpret it as a "raw combination of modifiers
		 and keysym".  Hide deadkeys, and use the generated
		 character(s) instead of the keysym.  (Backward
		 compatibility: exceptions for numpad keys generating
		 0-9 . , / * - +, and for extra-Alt combined with a
		 non-Latin char.)

		 Try to not report modifiers which have effect on
		 which character or deadkey is generated.

		 Example (contrived): if rightAlt-? generates f (on a
		 Cyrillic keyboard layout), and Ctrl, leftAlt do not
		 affect the generated character, one wants to report
		 Ctrl-leftAlt-f if the user presses
		 Ctrl-leftAlt-rightAlt-?.  */
	      int res;
#if 0
	      /* Some of WM_CHAR may be fed to us directly, some are
		 results of TranslateMessage().  Using 0 as the first
		 argument (in a separate call) might help us
		 distinguish these two cases.

		 However, the keypress feeders would most probably
		 expect the "standard" message pump, when
		 TranslateMessage() is called on EVERY KeyDown/KeyUp
		 event.  So they may feed us Down-Ctrl Down-FAKE
		 Char-o and expect us to recognize it as Ctrl-o.
		 Using 0 as the first argument would interfere with
		 this.   */
	      deliver_wm_chars (0, hwnd, msg, wParam, lParam, 1);
#endif
	      /* Processing the generated WM_CHAR messages *WHILE* we
		 handle KEYDOWN/UP event is the best choice, since
		 without any fuss, we know all 3 of: scancode, virtual
		 keycode, and expansion.  (Additionally, one knows
		 boundaries of expansion of different keypresses.)  */
	      res = deliver_wm_chars (1, hwnd, msg, wParam, lParam, 1);
	      windows_translate = -(res != 0);
	      if (res > 0) /* Bound to character(s) or a deadkey */
		break;
	      /* deliver_wm_chars may make some branches after this vestigal.  */
	    }
	  wParam = map_keypad_keys (wParam, (lParam & 0x1000000L) != 0);
	  /* If not defined as a function key, change it to a WM_CHAR message.  */
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
		      KEY_EVENT_RECORD key;

		      key.bKeyDown = TRUE;
		      key.wRepeatCount = 1;
		      key.wVirtualKeyCode = wParam;
		      key.wVirtualScanCode = (lParam & 0xFF0000) >> 16;
		      key.uChar.AsciiChar = 0;
		      key.dwControlKeyState = modifiers;

		      add = w32_kbd_patch_key (&key, w32_keyboard_codepage);
		      /* 0 means an unrecognized keycode, negative means
			 dead key.  Ignore both.  */
		      while (--add >= 0)
			{
			  /* Forward asciified character sequence.  */
			  post_character_message
			    (hwnd, WM_CHAR,
			     (unsigned char) key.uChar.AsciiChar, lParam,
			     w32_get_key_modifiers (wParam, lParam));
			  w32_kbd_patch_key (&key, w32_keyboard_codepage);
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

    if (windows_translate == -1)
      break;
    translate:
      if (windows_translate)
	{
	  MSG windows_msg = { hwnd, msg, wParam, lParam, 0, {0,0} };
	  windows_msg.time = GetMessageTime ();
	  TranslateMessage (&windows_msg);
	  goto dflt;
	}
      /* Fall through */
      FALLTHROUGH;

    case WM_SYSCHAR:
    case WM_CHAR:
      if (wParam > 255 )
	{
	  W32Msg wmsg;

	  wmsg.dwModifiers = w32_get_key_modifiers (wParam, lParam);
	  signal_user_input ();
	  my_post_msg (&wmsg, hwnd, WM_UNICHAR, wParam, lParam);

	}
      else
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
      /* If we can't get the IME result as Unicode, use default processing,
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
	  size = get_composition_string_fn (context, GCS_RESULTSTR, NULL, 0);
	  buffer = alloca (size);
	  size = get_composition_string_fn (context, GCS_RESULTSTR,
					    buffer, size);
	  release_ime_context_fn (hwnd, context);

	  signal_user_input ();
	  for (i = 0; i < size / sizeof (wchar_t); i++)
	    {
	      my_post_msg (&wmsg, hwnd, WM_UNICHAR, (WPARAM) buffer[i],
			   lParam);
	    }
	  /* Ignore the messages for the rest of the
	     characters in the string that was output above.  */
	  ignore_ime_char = (size / sizeof (wchar_t)) - 1;
	}
      else
	ignore_ime_char--;

      break;

    case WM_IME_STARTCOMPOSITION:
      if (!set_ime_composition_window_fn)
	goto dflt;
      else
	{
	  COMPOSITIONFORM form;
	  HIMC context;
	  struct window *w;

	  /* Implementation note: The code below does something that
	     one shouldn't do: it accesses the window object from a
	     separate thread, while the main (a.k.a. "Lisp") thread
	     runs and can legitimately delete and even GC it.  That is
	     why we are extra careful not to futz with a window that
	     is different from the one recorded when the system caret
	     coordinates were last modified.  That is also why we are
	     careful not to move the IME window if the window
	     described by W was deleted, as indicated by its buffer
	     field being reset to nil.  */
	  f = x_window_to_frame (dpyinfo, hwnd);
	  if (!(f && FRAME_LIVE_P (f)))
	    goto dflt;
	  w = XWINDOW (FRAME_SELECTED_WINDOW (f));
	  /* Punt if someone changed the frame's selected window
	     behind our back. */
	  if (w != w32_system_caret_window)
	    goto dflt;

	  form.dwStyle = CFS_RECT;
	  form.ptCurrentPos.x = w32_system_caret_x;
	  form.ptCurrentPos.y = w32_system_caret_y;

	  form.rcArea.left = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, 0);
	  form.rcArea.top = (WINDOW_TOP_EDGE_Y (w)
			     + w32_system_caret_hdr_height);
	  form.rcArea.right = (WINDOW_BOX_RIGHT_EDGE_X (w)
			       - WINDOW_RIGHT_MARGIN_WIDTH (w)
			       - WINDOW_RIGHT_FRINGE_WIDTH (w));
	  form.rcArea.bottom = (WINDOW_BOTTOM_EDGE_Y (w)
				- WINDOW_BOTTOM_DIVIDER_WIDTH (w)
				- w32_system_caret_mode_height);

	  /* Punt if the window was deleted behind our back.  */
	  if (!BUFFERP (w->contents))
	    goto dflt;

	  context = get_ime_context_fn (hwnd);

	  if (!context)
	    goto dflt;

	  set_ime_composition_window_fn (context, &form);
	  release_ime_context_fn (hwnd, context);
	}
      /* We should "goto dflt" here to pass WM_IME_STARTCOMPOSITION to
	 DefWindowProc, so that the composition window will actually
	 be displayed.  But doing so causes trouble with displaying
	 dialog boxes, such as the file selection dialog or font
	 selection dialog.  So something else is needed to fix the
	 former without breaking the latter.  See bug#11732.  */
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

		if (button_state) emacs_abort ();
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
      FALLTHROUGH;
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

      if (f && (msg == WM_LBUTTONDOWN || msg == WM_RBUTTONDOWN
		|| msg == WM_MBUTTONDOWN ||msg == WM_XBUTTONDOWN)
	  && !FRAME_NO_ACCEPT_FOCUS (f))
	/* When clicking into a child frame or when clicking into a
	   parent frame with the child frame selected and
	   `no-accept-focus' is not set, select the clicked frame.  */
	{
	  struct frame *p = FRAME_PARENT_FRAME (XFRAME (selected_frame));

	  if (FRAME_PARENT_FRAME (f) || f == p)
	    {
	      SetFocus (hwnd);
	      SetWindowPos (hwnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
	    }
	}

      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      signal_user_input ();

      /* Need to return true for XBUTTON messages, false for others,
	 to indicate that we processed the message.  */
      return (msg == WM_XBUTTONDOWN || msg == WM_XBUTTONUP);

    case WM_MOUSEMOVE:
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f)
	{
	  /* Ignore mouse movements as long as the menu is active.
	     These movements are processed by the window manager
	     anyway, and it's wrong to handle them as if they happened
	     on the underlying frame.  */
	  if (f->output_data.w32->menubar_active)
	    return 0;

	  /* If the mouse moved, and the mouse pointer is invisible,
	     make it visible again.  We do this here so as to be able
	     to show the mouse pointer even when the main
	     (a.k.a. "Lisp") thread is busy doing something.  */
	  static int last_x, last_y;
	  int x = GET_X_LPARAM (lParam);
	  int y = GET_Y_LPARAM (lParam);

	  if (f->pointer_invisible
	      && (x != last_x || y != last_y))
	    f->pointer_invisible = false;

	  last_x = x;
	  last_y = y;
	}

      /* If the mouse has just moved into the frame, start tracking
	 it, so we will be notified when it leaves the frame.  Mouse
	 tracking only works under W98 and NT4 and later. On earlier
	 versions, there is no way of telling when the mouse leaves the
	 frame, so we just have to put up with help-echo and mouse
	 highlighting remaining while the frame is not active.  */
      if (track_mouse_event_fn && !track_mouse_window
	  /* If the menu bar is active, turning on tracking of mouse
	     movement events might send these events to the tooltip
	     frame, if the user happens to move the mouse pointer over
	     the tooltip.  But since we don't process events for
	     tooltip frames, this causes Windows to present a
	     hourglass cursor, which is ugly and unexpected.  So don't
	     enable tracking mouse events in this case; they will be
	     restarted when the menu pops down.  (Confusingly, the
	     menubar_active member of f->output_data.w32, tested
	     above, is only set when a menu was popped up _not_ from
	     the frame's menu bar, but via x-popup-menu.)  */
	  && !menubar_in_use)
	{
	  TRACKMOUSEEVENT tme;
	  tme.cbSize = sizeof (tme);
	  tme.dwFlags = TME_LEAVE;
	  tme.hwndTrack = hwnd;
	  tme.dwHoverTime = HOVER_DEFAULT;

	  track_mouse_event_fn (&tme);
	  track_mouse_window = hwnd;
	}
      FALLTHROUGH;
    case WM_HSCROLL:
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
      FALLTHROUGH;

      /* FIXME!!!  This is never reached so what's the purpose?  If the
	 non-zero return remark below is right we're doing it wrong all
	 the time.  */
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
	      if (f)
		f->output_data.w32->menubar_active = 0;
	      menubar_in_use = 0;
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
	  emacs_abort ();

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

    case WM_MOUSEACTIVATE:
      /* WM_MOUSEACTIVATE is the only way on Windows to implement the
	 `no-accept-focus' frame parameter.  This means that one can't
	 use the mouse to scroll a window on a non-selected frame.  */

      /* Still not right - can't distinguish between clicks in the
	 client area of the frame from clicks forwarded from the scroll
	 bars - may have to hook WM_NCHITTEST to remember the mouse
	 position and then check if it is in the client area
	 ourselves.  */

      /* Discard the mouse click that activates a frame, allowing the
	 user to click anywhere without changing point (or worse!).
	 Don't eat mouse clicks on scrollbars though!!  */

      if ((f = x_window_to_frame (dpyinfo, hwnd))
	  && FRAME_NO_ACCEPT_FOCUS (f)
	  /* Ignore child frames, they don't accept focus anyway.  */
	  && !FRAME_PARENT_FRAME (f))
	{
	  Lisp_Object frame;

	  XSETFRAME (frame, f);
	  if (!EQ (selected_frame, frame))
	    /* Don't discard the message, GTK doesn't either.  */
	    return MA_NOACTIVATE; /* ANDEAT; */
	}
      goto dflt;

    case WM_MOUSELEAVE:
      /* No longer tracking mouse.  */
      track_mouse_window = NULL;
      FALLTHROUGH;

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
      if (!w32_kbdhook_active)
	register_hot_keys (hwnd);
      goto command;
    case WM_KILLFOCUS:
      if (!w32_kbdhook_active)
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
      FALLTHROUGH;
    case WM_MOVE:
    case WM_SIZE:
    command:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      goto dflt;

#ifdef WINDOWSNT
    case WM_CREATE:
      setup_w32_kbdhook ();
      goto dflt;
#endif

    case WM_DESTROY:
#ifdef WINDOWSNT
      remove_w32_kbdhook ();
#endif
      CoUninitialize ();
      return 0;

    case WM_WTSSESSION_CHANGE:
      if (wParam == WTS_SESSION_LOCK)
        reset_w32_kbdhook_state ();
      goto dflt;

    case WM_CLOSE:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_ENDSESSION:
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      /* Allow time for Emacs to attempt an orderly shutdown.  If we
         return, the process will be terminated immediately.  FIXME:
         1000 seconds is too long to sleep if the shutdown attempt
         fails (see bug#25875).  But if it fails, we want to find out
         about it, so let's leave 1000 for now.  */
      sleep (1000);
      FALLTHROUGH;

    case WM_WINDOWPOSCHANGING:
      /* Don't restrict the sizing of any kind of frames.  If the window
	 manager doesn't, there's no reason to do it ourselves.  */
      return 0;

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
	  if (f)
	    {
	      if (f->output_data.w32->hourglass_p
		  && !menubar_in_use && !current_popup_menu)
		SetCursor (f->output_data.w32->hourglass_cursor);
	      else if (f->pointer_invisible)
		SetCursor (NULL);
	      else
		SetCursor (f->output_data.w32->current_cursor);
	    }

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
	    /* Don't change the cursor while menu-bar menu is in use.  */
	    if (!f->output_data.w32->menubar_active
		&& !f->output_data.w32->hourglass_p)
	      {
		if (f->pointer_invisible)
		  SetCursor (NULL);
		else
		  SetCursor (cursor);
	      }
	  }
	return 0;
      }

    case WM_EMACS_SHOWCURSOR:
      {
	ShowCursor ((BOOL) wParam);

	return 0;
      }

    case WM_EMACS_CREATEVSCROLLBAR:
      return (LRESULT) w32_createvscrollbar ((struct frame *) wParam,
					     (struct scroll_bar *) lParam);

    case WM_EMACS_CREATEHSCROLLBAR:
      return (LRESULT) w32_createhscrollbar ((struct frame *) wParam,
					     (struct scroll_bar *) lParam);

    case WM_EMACS_SHOWWINDOW:
      return ShowWindow ((HWND) wParam, (WPARAM) lParam);

    case WM_EMACS_BRINGTOTOP:
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
	if (msg == WM_EMACS_BRINGTOTOP)
	  retval = BringWindowToTop ((HWND) wParam);

	/* Detach from the previous foreground thread.  */
	if (foreground_thread)
	  AttachThreadInput (GetCurrentThreadId (),
			     foreground_thread, FALSE);

	/* SetFocus to give/remove focus to/from a child window.  */
	if (msg == WM_EMACS_SETFOREGROUND)
	  SetFocus ((HWND) wParam);

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
	     unnecessarily, as it confuses screen reader software.  */
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
	   so for safety, we make sure the capture is canceled now.  */
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
    case WM_EMACS_FILENOTIFY:
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 1;

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
      return (w32_unicode_gui ? DefWindowProcW :  DefWindowProcA) (hwnd, msg, wParam, lParam);
    }

  /* The most common default return code for handled messages is 0.  */
  return 0;
}

static void
my_create_window (struct frame * f)
{
  MSG msg;
  static int coords[2];
  Lisp_Object left, top;
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  /* When called with RES_TYPE_NUMBER, x_get_arg will return zero for
     anything that is not a number and is not Qunbound.  */
  left = x_get_arg (dpyinfo, Qnil, Qleft, "left", "Left", RES_TYPE_NUMBER);
  top = x_get_arg (dpyinfo, Qnil, Qtop, "top", "Top", RES_TYPE_NUMBER);
  if (EQ (left, Qunbound))
    coords[0] = CW_USEDEFAULT;
  else
    coords[0] = XINT (left);
  if (EQ (top, Qunbound))
    coords[1] = CW_USEDEFAULT;
  else
    coords[1] = XINT (top);

  if (!PostThreadMessage (dwWindowsThreadId, WM_EMACS_CREATEWINDOW,
			  (WPARAM)f, (LPARAM)coords))
    emacs_abort ();
  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
}


/* Create a tooltip window. Unlike my_create_window, we do not do this
   indirectly via the Window thread, as we do not need to process Window
   messages for the tooltip.  Creating tooltips indirectly also creates
   deadlocks when tooltips are created for menu items.  */
static void
my_create_tip_window (struct frame *f)
{
  RECT rect;

  rect.left = rect.top = 0;
  rect.right = FRAME_PIXEL_WIDTH (f);
  rect.bottom = FRAME_PIXEL_HEIGHT (f);

  AdjustWindowRect (&rect, f->output_data.w32->dwStyle, false);

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
      SetWindowLong (tip_window, WND_VSCROLLBAR_INDEX, 0);
      SetWindowLong (tip_window, WND_HSCROLLBAR_INDEX, 0);

      /* Do this to discard the default setting specified by our parent. */
      ShowWindow (tip_window, SW_HIDE);
    }
}


/* Create and set up the w32 window for frame F.  */

static void
w32_window (struct frame *f, long window_prompting, bool minibuffer_only)
{
  block_input ();

  /* Use the resource name as the top-level window name
     for looking up resources.  Make a non-Lisp copy
     for the window manager, so GC relocation won't bother it.

     Elsewhere we specify the window name for the window manager.  */
  f->namebuf = xlispstrdup (Vx_resource_name);

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
    fset_name (f, Qnil);
    x_set_name (f, name, explicit);
  }

  unblock_input ();

  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f)
      && !FRAME_PARENT_FRAME (f))
    initialize_frame_menubar (f);

  if (FRAME_W32_WINDOW (f) == 0)
    error ("Unable to create window");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
x_icon (struct frame *f, Lisp_Object parms)
{
  Lisp_Object icon_x, icon_y;
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  /* Set the position of the icon.  Note that Windows 95 groups all
     icons in the tray.  */
  icon_x = x_get_arg (dpyinfo, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_get_arg (dpyinfo, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");

  block_input ();

#if 0 /* TODO */
  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (x_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL), Qicon)
	 ? IconicState
	 : NormalState));

  x_text_icon (f, SSDATA ((!NILP (f->icon_name)
			   ? f->icon_name
			   : f->name)));
#endif

  unblock_input ();
}


static void
x_make_gc (struct frame *f)
{
  XGCValues gc_values;

  block_input ();

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

  unblock_input ();
}


/* Handler for signals raised during x_create_frame and
   x_create_tip_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
#ifdef GLYPH_DEBUG
      struct w32_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

      /* If the frame's image cache refcount is still the same as our
	 private shadow variable, it means we are unwinding a frame
	 for which we didn't yet call init_frame_faces, where the
	 refcount is incremented.  Therefore, we increment it here, so
	 that free_frame_faces, called in x_free_frame_resources
	 below, will not mistakenly decrement the counter that was not
	 incremented yet to account for this new frame.  */
      if (FRAME_IMAGE_CACHE (f) != NULL
	  && FRAME_IMAGE_CACHE (f)->refcount == image_cache_refcount)
	FRAME_IMAGE_CACHE (f)->refcount++;
#endif

      x_free_frame_resources (f);
      free_glyphs (f);

#ifdef GLYPH_DEBUG
      /* Check that reference counts are indeed correct.  */
      eassert (dpyinfo->reference_count == dpyinfo_refcount);
      eassert ((dpyinfo->terminal->image_cache == NULL
		&& image_cache_refcount == 0)
	       || (dpyinfo->terminal->image_cache != NULL
		   && dpyinfo->terminal->image_cache->refcount == image_cache_refcount));
#endif
      return Qt;
    }

  return Qnil;
}

static void
do_unwind_create_frame (Lisp_Object frame)
{
  unwind_create_frame (frame);
}

static void
x_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct w32_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param = x_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
				RES_TYPE_STRING);
  Lisp_Object font;
  if (EQ (font_param, Qunbound))
    font_param = Qnil;
  font = !NILP (font_param) ? font_param
    : x_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);

  if (!STRINGP (font))
    {
      int i;
      static const char *names[]
	= { "Courier New-10",
	    "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1",
	    "-*-Fixedsys-normal-r-*-*-12-*-*-*-c-*-iso8859-1",
	    "Fixedsys",
	    NULL };

      for (i = 0; names[i]; i++)
	{
	  font = font_open_by_name (f, build_unibyte_string (names[i]));
	  if (! NILP (font))
	    break;
	}
      if (NILP (font))
	error ("No suitable font was found");
    }
  else if (!NILP (font_param))
    {
      /* Remember the explicit font parameter, so we can re-apply it after
	 we've applied the `default' face settings.  */
      x_set_frame_parameters (f, Fcons (Fcons (Qfont_parameter, font_param),
					Qnil));
    }
  x_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object parameters)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only = false;
  long window_prompting = 0;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct w32_display_info *dpyinfo = NULL;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;
  int x_width = 0, x_height = 0;

  if (!FRAME_W32_P (SELECTED_FRAME ())
      && !FRAME_INITIAL_P (SELECTED_FRAME ()))
    error ("Cannot create a GUI frame in a -nw session");

  /* Make copy of frame parameters because the original is in pure
     storage now. */
  parameters = Fcopy_alist (parameters);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = x_get_arg (dpyinfo, parameters, Qterminal, 0, 0, RES_TYPE_NUMBER);
  if (EQ (display, Qunbound))
    display = x_get_arg (dpyinfo, parameters, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = x_get_arg (dpyinfo, parameters, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = x_get_arg (dpyinfo, parameters, Qparent_id, NULL, NULL,
		      RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  else if (!NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  tem = x_get_arg (dpyinfo, parameters, Qminibuffer, "minibuffer", "Minibuffer",
		   RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = true;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (true);

  XSETFRAME (frame, f);

  parent_frame = x_get_arg (dpyinfo, parameters, Qparent_frame, NULL, NULL,
			    RES_TYPE_SYMBOL);
  /* Apply `parent-frame' parameter only when no `parent-id' was
     specified.  */
  if (!NILP (parent_frame)
      && (!NILP (parent)
	  || !FRAMEP (parent_frame)
	  || !FRAME_LIVE_P (XFRAME (parent_frame))
	  || !FRAME_W32_P (XFRAME (parent_frame))))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  tem = x_get_arg (dpyinfo, parameters, Qundecorated, NULL, NULL,
		   RES_TYPE_BOOLEAN);
  FRAME_UNDECORATED (f) = !NILP (tem) && !EQ (tem, Qunbound);
  store_frame_param (f, Qundecorated, FRAME_UNDECORATED (f) ? Qt : Qnil);

  tem = x_get_arg (dpyinfo, parameters, Qskip_taskbar, NULL, NULL,
		   RES_TYPE_BOOLEAN);
  FRAME_SKIP_TASKBAR (f) = !NILP (tem) && !EQ (tem, Qunbound);
  store_frame_param (f, Qskip_taskbar,
		     (NILP (tem) || EQ (tem, Qunbound)) ? Qnil : Qt);

  /* By default, make scrollbars the system standard width and height. */
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = GetSystemMetrics (SM_CXVSCROLL);
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = GetSystemMetrics (SM_CXHSCROLL);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_w32;
  f->output_data.w32 = xzalloc (sizeof (struct w32_output));
  FRAME_FONTSET (f) = -1;

  fset_icon_name
    (f, x_get_arg (dpyinfo, parameters, Qicon_name, "iconName", "Title",
		   RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  /*  FRAME_DISPLAY_INFO (f) = dpyinfo; */

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (do_unwind_create_frame, frame);

#ifdef GLYPH_DEBUG
  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */

  /* Specify the parent under which to make this window - this seems to
     have no effect on Windows because parent_desc is explicitly reset
     below.  */
  if (!NILP (parent))
    {
      /* Cast to UINT_PTR shuts up compiler warnings about cast to
	 pointer from integer of different size.  */
      f->output_data.w32->parent_desc = (Window) (UINT_PTR) XFASTINT (parent);
      f->output_data.w32->explicit_parent = true;
    }
  else
    {
      f->output_data.w32->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
      f->output_data.w32->explicit_parent = false;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->w32_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* Use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  if (uniscribe_available)
    register_font_driver (&uniscribe_font_driver, f);
  register_font_driver (&w32font_driver, f);

  x_default_parameter (f, parameters, Qfont_backend, Qnil,
		       "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  x_default_font_parameter (f, parameters);

  /* Default BorderWidth to 0 to match other platforms.  */
  x_default_parameter (f, parameters, Qborder_width, make_number (0),
		       "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* We recognize either internalBorderWidth or internalBorder
     (which is what xterm calls it).  */
  if (NILP (Fassq (Qinternal_border_width, parameters)))
    {
      Lisp_Object value;

      value = x_get_arg (dpyinfo, parameters, Qinternal_border_width,
			 "internalBorder", "InternalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parameters = Fcons (Fcons (Qinternal_border_width, value),
			    parameters);
    }

  x_default_parameter (f, parameters, Qinternal_border_width, make_number (0),
		       "internalBorderWidth", "InternalBorder", RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qright_divider_width, make_number (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qbottom_divider_width, make_number (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  x_default_parameter (f, parameters, Qvertical_scroll_bars, Qright,
		       "verticalScrollBars", "ScrollBars", RES_TYPE_SYMBOL);
  x_default_parameter (f, parameters, Qhorizontal_scroll_bars, Qnil,
		       "horizontalScrollBars", "ScrollBars", RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parameters, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parameters, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
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
  x_default_parameter (f, parameters, Qno_focus_on_map, Qnil,
		       NULL, NULL, RES_TYPE_BOOLEAN);
  x_default_parameter (f, parameters, Qno_accept_focus, Qnil,
		       NULL, NULL, RES_TYPE_BOOLEAN);
  x_default_parameter (f, parameters, Qno_special_glyphs, Qnil,
		       NULL, NULL, RES_TYPE_BOOLEAN);

  /* Process alpha here (Bug#16619).  On XP this fails with child
     frames.  For `no-focus-on-map' frames delay processing of alpha
     until the frame becomes visible.  */
  if (!FRAME_NO_FOCUS_ON_MAP (f))
    x_default_parameter (f, parameters, Qalpha, Qnil,
			 "alpha", "Alpha", RES_TYPE_NUMBER);

  /* Init faces first since we need the frame's column width/line
     height in various occasions.  */
  init_frame_faces (f);

  /* We have to call adjust_frame_size here since otherwise
     x_set_tool_bar_lines will already work with the character sizes
     installed by init_frame_faces while the frame's pixel size is still
     calculated from a character size of 1 and we subsequently hit the
     (height >= 0) assertion in window_box_height.

     The non-pixelwise code apparently worked around this because it
     had one frame line vs one toolbar line which left us with a zero
     root window height which was obviously wrong as well ...

     Also process `min-width' and `min-height' parameters right here
     because `frame-windows-min-size' needs them.  */
  tem = x_get_arg (dpyinfo, parameters, Qmin_width, NULL, NULL,
		   RES_TYPE_NUMBER);
  if (NUMBERP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = x_get_arg (dpyinfo, parameters, Qmin_height, NULL, NULL,
		   RES_TYPE_NUMBER);
  if (NUMBERP (tem))
    store_frame_param (f, Qmin_height, tem);
  adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		     FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 5, true,
		     Qx_create_frame_1);

  /* The X resources controlling the menu-bar and tool-bar are
     processed specially at startup, and reflected in the mode
     variables; ignore them here.  */
  if (NILP (parent_frame))
    {
      x_default_parameter (f, parameters, Qmenu_bar_lines,
			   NILP (Vmenu_bar_mode)
			   ? make_number (0) : make_number (1),
			   NULL, NULL, RES_TYPE_NUMBER);
    }
  else
    /* No menu bar for child frames.  */
    store_frame_param (f, Qmenu_bar_lines, make_number (0));

  x_default_parameter (f, parameters, Qtool_bar_lines,
		       NILP (Vtool_bar_mode)
		       ? make_number (0) : make_number (1),
		       NULL, NULL, RES_TYPE_NUMBER);

  x_default_parameter (f, parameters, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parameters, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);

  f->output_data.w32->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
  f->output_data.w32->text_cursor = w32_load_cursor (IDC_IBEAM);
  f->output_data.w32->nontext_cursor = w32_load_cursor (IDC_ARROW);
  f->output_data.w32->modeline_cursor = w32_load_cursor (IDC_ARROW);
  f->output_data.w32->hand_cursor = w32_load_cursor (IDC_HAND);
  f->output_data.w32->hourglass_cursor = w32_load_cursor (IDC_WAIT);
  f->output_data.w32->horizontal_drag_cursor = w32_load_cursor (IDC_SIZEWE);
  f->output_data.w32->vertical_drag_cursor = w32_load_cursor (IDC_SIZENS);
  f->output_data.w32->left_edge_cursor = w32_load_cursor (IDC_SIZEWE);
  f->output_data.w32->top_left_corner_cursor = w32_load_cursor (IDC_SIZENWSE);
  f->output_data.w32->top_edge_cursor = w32_load_cursor (IDC_SIZENS);
  f->output_data.w32->top_right_corner_cursor = w32_load_cursor (IDC_SIZENESW);
  f->output_data.w32->right_edge_cursor = w32_load_cursor (IDC_SIZEWE);
  f->output_data.w32->bottom_right_corner_cursor = w32_load_cursor (IDC_SIZENWSE);
  f->output_data.w32->bottom_edge_cursor = w32_load_cursor (IDC_SIZENS);
  f->output_data.w32->bottom_left_corner_cursor = w32_load_cursor (IDC_SIZENESW);

  f->output_data.w32->current_cursor = f->output_data.w32->nontext_cursor;

  window_prompting = x_figure_window_size (f, parameters, true, &x_width, &x_height);

  tem = x_get_arg (dpyinfo, parameters, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  w32_window (f, window_prompting, minibuffer_only);
  x_icon (f, parameters);

  x_make_gc (f);

  /* Now consider the frame official.  */
  f->terminal->reference_count++;
  FRAME_DISPLAY_INFO (f)->reference_count++;
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
  x_default_parameter (f, parameters, Qscroll_bar_height, Qnil,
		       "scrollBarHeight", "ScrollBarHeight", RES_TYPE_NUMBER);

  /* Allow x_set_window_size, now.  */
  f->can_x_set_window_size = true;

  if (x_width > 0)
    SET_FRAME_WIDTH (f, x_width);
  if (x_height > 0)
    SET_FRAME_HEIGHT (f, x_height);

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  block_input ();
  x_wm_set_size_hint (f, window_prompting, false);
  unblock_input ();

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f), 0, true,
		     Qx_create_frame_2);

  /* Process fullscreen parameter here in the hope that normalizing a
     fullheight/fullwidth frame will produce the size set by the last
     adjust_frame_size call.  */
  x_default_parameter (f, parameters, Qfullscreen, Qnil,
		       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);
  x_default_parameter (f, parameters, Qz_group, Qnil,
		       NULL, NULL, RES_TYPE_SYMBOL);

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (!f->output_data.w32->explicit_parent)
    {
      Lisp_Object visibility
	= x_get_arg (dpyinfo, parameters, Qvisibility, 0, 0, RES_TYPE_SYMBOL);

      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else
	{
	  if (EQ (visibility, Qunbound))
	    visibility = Qt;

	  if (!NILP (visibility))
	    x_make_frame_visible (f);
	}

      store_frame_param (f, Qvisibility, visibility);
    }

  /* For `no-focus-on-map' frames set alpha here.  */
  if (FRAME_NO_FOCUS_ON_MAP (f))
    x_default_parameter (f, parameters, Qalpha, Qnil,
			 "alpha", "Alpha", RES_TYPE_NUMBER);

  /* Initialize `default-minibuffer-frame' in case this is the first
     frame on this terminal.  */
  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
	  || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
    kset_default_minibuffer_frame (kb, frame);

  /* All remaining specified parameters, which have not been "used"
     by x_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parameters; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}

/* FRAME is used only to get a handle on the X display.  We don't pass the
   display info directly because we're called from frame.c, which doesn't
   know about that structure.  */
Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct w32_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->w32_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->w32_focus_frame);
  return xfocus;
}

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (w32_defined_color (f, SSDATA (color), &foo, false))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (w32_defined_color (f, SSDATA (color), &foo, false))
    return list3i ((GetRValue (foo.pixel) << 8) | GetRValue (foo.pixel),
		   (GetGValue (foo.pixel) << 8) | GetGValue (foo.pixel),
		   (GetBValue (foo.pixel) << 8) | GetBValue (foo.pixel));
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 2)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p,
       Sx_display_grayscale_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 1)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width,
       Sx_display_pixel_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (x_display_pixel_width (dpyinfo));
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (x_display_pixel_height (dpyinfo));
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes * dpyinfo->n_cbits);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  int cap;

  /* Don't use NCOLORS: it returns incorrect results under remote
   * desktop.  We force 24+ bit depths to 24-bit, both to prevent an
   * overflow and because probably is more meaningful on Windows
   * anyway.  */

  cap = 1 << min (dpyinfo->n_planes * dpyinfo->n_cbits, 24);
  return make_number (cap);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  return make_number (1);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return build_string ("Microsoft Corp.");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return list3i (w32_major_version, w32_minor_version, w32_build_number);
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  return make_number (1);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height,
       Sx_display_mm_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  HDC hdc;
  double mm_per_pixel;

  hdc = GetDC (NULL);
  mm_per_pixel = ((double) GetDeviceCaps (hdc, VERTSIZE)
		  / GetDeviceCaps (hdc, VERTRES));
  ReleaseDC (NULL, hdc);

  return make_number (x_display_pixel_height (dpyinfo) * mm_per_pixel + 0.5);
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  HDC hdc;
  double mm_per_pixel;

  hdc = GetDC (NULL);
  mm_per_pixel = ((double) GetDeviceCaps (hdc, HORZSIZE)
		  / GetDeviceCaps (hdc, HORZRES));
  ReleaseDC (NULL, hdc);

  return make_number (x_display_pixel_width (dpyinfo) * mm_per_pixel + 0.5);
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  return intern ("not-useful");
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  Lisp_Object result = Qnil;

  if (dpyinfo->has_palette)
      result = intern ("pseudo-color");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits == 1)
      result = intern ("static-gray");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits == 4)
      result = intern ("static-color");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits > 8)
      result = intern ("true-color");

  return result;
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  return Qnil;
}

static BOOL CALLBACK ALIGN_STACK
w32_monitor_enum (HMONITOR monitor, HDC hdc, RECT *rcMonitor, LPARAM dwData)
{
  Lisp_Object *monitor_list = (Lisp_Object *) dwData;

  *monitor_list = Fcons (make_save_ptr (monitor), *monitor_list);

  return TRUE;
}

static Lisp_Object
w32_display_monitor_attributes_list (void)
{
  Lisp_Object attributes_list = Qnil, primary_monitor_attributes = Qnil;
  Lisp_Object monitor_list = Qnil, monitor_frames, rest, frame;
  int i, n_monitors;
  HMONITOR *monitors;

  if (!(enum_display_monitors_fn && get_monitor_info_fn
	&& monitor_from_window_fn))
    return Qnil;

  if (!enum_display_monitors_fn (NULL, NULL, w32_monitor_enum,
				 (LPARAM) &monitor_list)
      || NILP (monitor_list))
    return Qnil;

  n_monitors = 0;
  for (rest = monitor_list; CONSP (rest); rest = XCDR (rest))
    n_monitors++;

  monitors = xmalloc (n_monitors * sizeof (*monitors));
  for (i = 0; i < n_monitors; i++)
    {
      monitors[i] = XSAVE_POINTER (XCAR (monitor_list), 0);
      monitor_list = XCDR (monitor_list);
    }

  monitor_frames = Fmake_vector (make_number (n_monitors), Qnil);
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_W32_P (f) && !FRAME_TOOLTIP_P (f))
	{
	  HMONITOR monitor =
	    monitor_from_window_fn (FRAME_W32_WINDOW (f),
				    MONITOR_DEFAULT_TO_NEAREST);

	  for (i = 0; i < n_monitors; i++)
	    if (monitors[i] == monitor)
	      break;

	  if (i < n_monitors)
	    ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
	}
    }

  for (i = 0; i < n_monitors; i++)
    {
      Lisp_Object geometry, workarea, name, attributes = Qnil;
      HDC hdc;
      int width_mm, height_mm;
      struct MONITOR_INFO_EX mi;

      mi.cbSize = sizeof (mi);
      if (!get_monitor_info_fn (monitors[i], (struct MONITOR_INFO *) &mi))
	continue;

      hdc = CreateDCA ("DISPLAY", mi.szDevice, NULL, NULL);
      if (hdc == NULL)
	continue;
      width_mm = GetDeviceCaps (hdc, HORZSIZE);
      height_mm = GetDeviceCaps (hdc, VERTSIZE);
      DeleteDC (hdc);

      attributes = Fcons (Fcons (Qframes, AREF (monitor_frames, i)),
			  attributes);

      name = DECODE_SYSTEM (build_unibyte_string (mi.szDevice));

      attributes = Fcons (Fcons (Qname, name), attributes);

      attributes = Fcons (Fcons (Qmm_size, list2i (width_mm, height_mm)),
			  attributes);

      workarea = list4i (mi.rcWork.left, mi.rcWork.top,
			 mi.rcWork.right - mi.rcWork.left,
			 mi.rcWork.bottom - mi.rcWork.top);
      attributes = Fcons (Fcons (Qworkarea, workarea), attributes);

      geometry = list4i (mi.rcMonitor.left, mi.rcMonitor.top,
			 mi.rcMonitor.right - mi.rcMonitor.left,
			 mi.rcMonitor.bottom - mi.rcMonitor.top);
      attributes = Fcons (Fcons (Qgeometry, geometry), attributes);

      if (mi.dwFlags & MONITORINFOF_PRIMARY)
	primary_monitor_attributes = attributes;
      else
	attributes_list = Fcons (attributes, attributes_list);
    }

  if (!NILP (primary_monitor_attributes))
    attributes_list = Fcons (primary_monitor_attributes, attributes_list);

  xfree (monitors);

  return attributes_list;
}

static Lisp_Object
w32_display_monitor_attributes_list_fallback (struct w32_display_info *dpyinfo)
{
  Lisp_Object geometry, workarea, frames, rest, frame, attributes = Qnil;
  HDC hdc;
  double mm_per_pixel;
  int pixel_width, pixel_height, width_mm, height_mm;
  RECT workarea_rect;

  /* Fallback: treat (possibly) multiple physical monitors as if they
     formed a single monitor as a whole.  This should provide a
     consistent result at least on single monitor environments.  */
  attributes = Fcons (Fcons (Qname, build_string ("combined screen")),
		      attributes);

  frames = Qnil;
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_W32_P (f) && !FRAME_TOOLTIP_P (f))
	frames = Fcons (frame, frames);
    }
  attributes = Fcons (Fcons (Qframes, frames), attributes);

  pixel_width = x_display_pixel_width (dpyinfo);
  pixel_height = x_display_pixel_height (dpyinfo);

  hdc = GetDC (NULL);
  mm_per_pixel = ((double) GetDeviceCaps (hdc, HORZSIZE)
		  / GetDeviceCaps (hdc, HORZRES));
  width_mm = pixel_width * mm_per_pixel + 0.5;
  mm_per_pixel = ((double) GetDeviceCaps (hdc, VERTSIZE)
		  / GetDeviceCaps (hdc, VERTRES));
  height_mm = pixel_height * mm_per_pixel + 0.5;
  ReleaseDC (NULL, hdc);
  attributes = Fcons (Fcons (Qmm_size, list2i (width_mm, height_mm)),
		      attributes);

  /* GetSystemMetrics below may return 0 for Windows 95 or NT 4.0, but
     we don't care.  */
  geometry = list4i (GetSystemMetrics (SM_XVIRTUALSCREEN),
		     GetSystemMetrics (SM_YVIRTUALSCREEN),
		     pixel_width, pixel_height);
  if (SystemParametersInfo (SPI_GETWORKAREA, 0, &workarea_rect, 0))
    workarea = list4i (workarea_rect.left, workarea_rect.top,
		       workarea_rect.right - workarea_rect.left,
		       workarea_rect.bottom - workarea_rect.top);
  else
    workarea = geometry;
  attributes = Fcons (Fcons (Qworkarea, workarea), attributes);

  attributes = Fcons (Fcons (Qgeometry, geometry), attributes);

  return list1 (attributes);
}

DEFUN ("w32-display-monitor-attributes-list", Fw32_display_monitor_attributes_list,
       Sw32_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the W32 display DISPLAY.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  Lisp_Object attributes_list;

  block_input ();
  attributes_list = w32_display_monitor_attributes_list ();
  if (NILP (attributes_list))
    attributes_list = w32_display_monitor_attributes_list_fallback (dpyinfo);
  unblock_input ();

  return attributes_list;
}

DEFUN ("set-message-beep", Fset_message_beep, Sset_message_beep, 1, 1, 0,
       doc: /* Set the sound generated when the bell is rung.
SOUND is `asterisk', `exclamation', `hand', `question', `ok', or `silent'
to use the corresponding system sound for the bell.  The `silent' sound
prevents Emacs from making any sound at all.
SOUND is nil to use the normal beep.  */)
  (Lisp_Object sound)
{
  CHECK_SYMBOL (sound);

  if (NILP (sound))
      sound_type = 0xFFFFFFFF;
  else if (EQ (sound, intern ("asterisk")))
      sound_type = MB_ICONASTERISK;
  else if (EQ (sound, intern ("exclamation")))
      sound_type = MB_ICONEXCLAMATION;
  else if (EQ (sound, intern ("hand")))
      sound_type = MB_ICONHAND;
  else if (EQ (sound, intern ("question")))
      sound_type = MB_ICONQUESTION;
  else if (EQ (sound, intern ("ok")))
      sound_type = MB_OK;
  else if (EQ (sound, intern ("silent")))
      sound_type = MB_EMACS_SILENT;
  else
      sound_type = 0xFFFFFFFF;

  return sound;
}

#if 0	/* unused */
int
x_screen_planes (register struct frame *f)
{
  return FRAME_DISPLAY_INFO (f)->n_planes;
}
#endif

/* Return the display structure for the display named NAME.
   Open a new connection if necessary.  */

struct w32_display_info *
x_display_info_for_name (Lisp_Object name)
{
  struct w32_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = &one_w32_display_info; dpyinfo; dpyinfo = dpyinfo->next)
    if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element), name)))
      return dpyinfo;

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = w32_term_init (name, NULL, SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", SDATA (name));

  XSETFASTINT (Vwindow_system_version, w32_major_version);

  return dpyinfo;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display, Lisp_Object xrm_string, Lisp_Object must_succeed)
{
  char *xrm_option;
  struct w32_display_info *dpyinfo;

  CHECK_STRING (display);

  /* Signal an error in order to encourage correct use from callers.
   * If we ever support multiple window systems in the same Emacs,
   * we'll need callers to be precise about what window system they
   * want.  */

  if (strcmp (SSDATA (display), "w32") != 0)
    error ("The name of the display in this Emacs must be \"w32\"");

  /* If initialization has already been done, return now to avoid
     overwriting critical parts of one_w32_display_info.  */
  if (window_system_available (NULL))
    return Qnil;

  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

  /* Allow color mapping to be defined externally; first look in user's
     HOME directory, then in Emacs etc dir for a file called rgb.txt. */
  {
    Lisp_Object color_file;

    color_file = build_string ("~/rgb.txt");

    if (NILP (Ffile_readable_p (color_file)))
      color_file =
	Fexpand_file_name (build_string ("rgb.txt"),
			   Fsymbol_value (intern ("data-directory")));

    Vw32_color_map = Fx_load_color_file (color_file);
  }
  if (NILP (Vw32_color_map))
    Vw32_color_map = w32_default_color_map ();

  /* Merge in system logical colors.  */
  add_system_logical_colors_to_map (&Vw32_color_map);

  if (! NILP (xrm_string))
    xrm_option = SSDATA (xrm_string);
  else
    xrm_option = NULL;

  /* Use this general default value to start with.  */
  /* First remove .exe suffix from invocation-name - it looks ugly. */
  {
    char basename[ MAX_PATH ], *str;

    lispstpcpy (basename, Vinvocation_name);
    str = strrchr (basename, '.');
    if (str) *str = 0;
    Vinvocation_name = build_string (basename);
  }
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = w32_term_init (display, xrm_option, SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to server %s.\n",
	       SDATA (display));
      else
	error ("Cannot connect to server %s", SDATA (display));
    }

  XSETFASTINT (Vwindow_system_version, w32_major_version);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  block_input ();
  x_destroy_all_bitmaps (dpyinfo);

  x_delete_display (dpyinfo);
  unblock_input ();

  return Qnil;
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct w32_display_info *wdi;

  for (wdi = x_display_list; wdi; wdi = wdi->next)
    result = Fcons (XCAR (wdi->name_list_element), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object on, Lisp_Object display)
{
  return Qnil;
}



/***********************************************************************
			   Window properties
 ***********************************************************************/

#if 0 /* TODO : port window properties to W32 */

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object prop, Lisp_Object value, Lisp_Object frame,
   Lisp_Object type, Lisp_Object format, Lisp_Object outer_p)
{
  struct frame *f = decode_window_system_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  CHECK_STRING (value);

  block_input ();
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
  XChangeProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
		   prop_atom, XA_STRING, 8, PropModeReplace,
		   SDATA (value), SCHARS (value));

  /* Make sure the property is set when we return.  */
  XFlush (FRAME_W32_DISPLAY (f));
  unblock_input ();

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object prop, Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  block_input ();
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), SDATA (prop), False);
  XDeleteProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f), prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_W32_DISPLAY (f));
  unblock_input ();

  return prop;
}


DEFUN ("x-window-property", Fx_window_property, Sx_window_property,
       1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object prop, Lisp_Object frame, Lisp_Object type,
   Lisp_Object source, Lisp_Object delete_p, Lisp_Object vector_ret_p)
{
  struct frame *f = decode_window_system_frame (frame);
  Atom prop_atom;
  int rc;
  Lisp_Object prop_value = Qnil;
  char *tmp_data = NULL;
  Atom actual_type;
  int actual_format;
  unsigned long actual_size, bytes_remaining;

  CHECK_STRING (prop);
  block_input ();
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

  unblock_input ();

  return prop_value;

  return Qnil;
}

#endif /* TODO */

/***********************************************************************
				Tool tips
 ***********************************************************************/

static void compute_tip_xy (struct frame *, Lisp_Object, Lisp_Object,
			    Lisp_Object, int, int, int *, int *);

/* The frame of the currently visible tooltip.  */
Lisp_Object tip_frame;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
Window tip_window;

/* A timer that hides or deletes the currently visible tooltip when it
   fires.  */
Lisp_Object tip_timer;

/* STRING argument of last `x-show-tip' call.  */
Lisp_Object tip_last_string;

/* Normalized FRAME argument of last `x-show-tip' call.  */
Lisp_Object tip_last_frame;

/* PARMS argument of last `x-show-tip' call.  */
Lisp_Object tip_last_parms;


static void
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = NULL;
      tip_frame = Qnil;
    }
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  Value is the frame.

   Note that functions called here, esp. x_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
x_create_tip_frame (struct w32_display_info *dpyinfo, Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame;
  Lisp_Object name;
  int width, height;
  ptrdiff_t count = SPECPDL_INDEX ();
  struct kboard *kb;
  bool face_change_before = face_change;
  int x_width = 0, x_height = 0;

  /* Use this general default value to start with until we know if
     this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  kb = dpyinfo->terminal->kboard;

  /* The calls to x_get_arg remove elements from PARMS, so copy it to
     avoid destructive changes behind our caller's back.  */
  parms = Fcopy_alist (parms);

  /* Get the name of the frame to use for resource lookup.  */
  name = x_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");
  Vx_resource_name = name;

  frame = Qnil;
  /* Make a frame without minibuffer nor mode-line.  */
  f = make_frame (false);
  f->wants_modeline = false;
  XSETFRAME (frame, f);

  record_unwind_protect (unwind_create_tip_frame, frame);

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->terminal = dpyinfo->terminal;
  f->output_method = output_w32;
  f->output_data.w32 = xzalloc (sizeof (struct w32_output));

  FRAME_FONTSET (f)  = -1;
  fset_icon_name (f, Qnil);
  f->tooltip = true;

#ifdef GLYPH_DEBUG
  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
  FRAME_KBOARD (f) = kb;

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->w32_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* Use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  if (uniscribe_available)
    register_font_driver (&uniscribe_font_driver, f);
  register_font_driver (&w32font_driver, f);

  x_default_parameter (f, parms, Qfont_backend, Qnil,
		       "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  x_default_font_parameter (f, parms);

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
  x_default_parameter (f, parms, Qno_special_glyphs, Qt,
		       NULL, NULL, RES_TYPE_BOOLEAN);

  /* Init faces before x_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  f->output_data.w32->dwStyle = WS_BORDER | WS_POPUP | WS_DISABLED;
  f->output_data.w32->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
  f->output_data.w32->explicit_parent = false;

  x_figure_window_size (f, parms, true, &x_width, &x_height);

  /* No fringes on tip frame.  */
  f->fringe_cols = 0;
  f->left_fringe_width = 0;
  f->right_fringe_width = 0;
  /* No dividers on tip frame.  */
  f->right_divider_width = 0;
  f->bottom_divider_width = 0;

  block_input ();
  my_create_tip_window (f);
  unblock_input ();

  x_make_gc (f);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);
  /* Process alpha here (Bug#17344).  */
  x_default_parameter (f, parms, Qalpha, Qnil,
		       "alpha", "Alpha", RES_TYPE_NUMBER);

  /* Dimensions, especially FRAME_LINES (f), must be done via
     change_frame_size.  Change will not be effected unless different
     from the current FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);
  SET_FRAME_COLS (f, 0);
  SET_FRAME_LINES (f, 0);
  adjust_frame_size (f, width * FRAME_COLUMN_WIDTH (f),
		     height * FRAME_LINE_HEIGHT (f), 0, true, Qtip_frame);
  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, Qtooltip)))
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtooltip, Qt), Qnil));

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame get's set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);
    Lisp_Object fg = Fframe_parameter (frame, Qforeground_color);
    Lisp_Object colors = Qnil;

    call2 (Qface_set_after_frame_default, frame, Qnil);

    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      colors = Fcons (Fcons (Qbackground_color, bg), colors);
    if (!EQ (fg, Fframe_parameter (frame, Qforeground_color)))
      colors = Fcons (Fcons (Qforeground_color, fg), colors);

    if (!NILP (colors))
      Fmodify_frame_parameters (frame, colors);
  }

  f->no_split = true;

  /* Now that the frame is official, it counts as a reference to
     its display.  */
  FRAME_DISPLAY_INFO (f)->reference_count++;
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);
  f->can_x_set_window_size = true;

  /* Setting attributes of faces of the tooltip frame from resources
     and similar will set face_change, which leads to the
     clearing of all current matrices.  Since this isn't necessary
     here, avoid it by resetting face_change to the value it
     had before we created the tip frame.  */
  face_change = face_change_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}


/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f,
		Lisp_Object parms, Lisp_Object dx, Lisp_Object dy,
		int width, int height, int *root_x, int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int min_x = 0, min_y = 0, max_x = 0, max_y = 0;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if ((!INTEGERP (left) && !INTEGERP (right))
      || (!INTEGERP (top) && !INTEGERP (bottom)))
    {
      POINT pt;

      /* Default min and max values.  */
      min_x = 0;
      min_y = 0;
      max_x = x_display_pixel_width (FRAME_DISPLAY_INFO (f));
      max_y = x_display_pixel_height (FRAME_DISPLAY_INFO (f));

      block_input ();
      GetCursorPos (&pt);
      *root_x = pt.x;
      *root_y = pt.y;
      unblock_input ();

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
  else if (INTEGERP (bottom))
    *root_y = XINT (bottom) - height;
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
  else if (INTEGERP (right))
    *root_x = XINT (right) - width;
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

/**
 * x_hide_tip:
 *
 * Hide currently visible tooltip and cancel its timer.
 *
 * This will try to make tooltip_frame invisible (if DELETE is false)
 * or delete tooltip_frame (if DELETE is true).
 *
 * Return Qt if the tooltip was either deleted or made invisible, Qnil
 * otherwise.
 */
static Lisp_Object
x_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      call1 (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

  if (NILP (tip_frame)
      || (!delete && FRAMEP (tip_frame)
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    return Qnil;
  else
    {
      ptrdiff_t count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      if (FRAMEP (tip_frame))
	{
	  if (FRAME_LIVE_P (XFRAME (tip_frame)))
	    {
	      if (delete)
		{
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		x_make_frame_invisible (XFRAME (tip_frame));

	      was_open = Qt;
	    }
	  else
	    tip_frame = Qnil;
	}
      else
	tip_frame = Qnil;

      return unbind_to (count, was_open);
    }
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
  struct frame *tip_f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int width, height;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  ptrdiff_t count = SPECPDL_INDEX ();
  ptrdiff_t count_1;
  Lisp_Object window, size, tip_buf;
  AUTO_STRING (tip, " *tip*");

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);

  if (NILP (frame))
    frame = selected_frame;
  decode_window_system_frame (frame);

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

  if (FRAMEP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	  && EQ (frame, tip_last_frame)
	  && !NILP (Fequal_including_properties (string, tip_last_string))
	  && !NILP (Fequal (parms, tip_last_parms)))
	{
	  /* Only DX and DY have changed.  */
	  tip_f = XFRAME (tip_frame);
	  if (!NILP (tip_timer))
	    {
	      Lisp_Object timer = tip_timer;

	      tip_timer = Qnil;
	      call1 (Qcancel_timer, timer);
	    }

	  block_input ();
	  compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			  FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);

	  /* Put tooltip in topmost group and in position.  */
	  SetWindowPos (FRAME_W32_WINDOW (tip_f), HWND_TOPMOST,
			root_x, root_y, 0, 0,
			SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOOWNERZORDER);

	  /* Ensure tooltip is on top of other topmost windows (eg menus).  */
	  SetWindowPos (FRAME_W32_WINDOW (tip_f), HWND_TOP,
			0, 0, 0, 0,
			SWP_NOMOVE | SWP_NOSIZE
			| SWP_NOACTIVATE | SWP_NOOWNERZORDER);

	  /* Let redisplay know that we have made the frame visible already.  */
	  SET_FRAME_VISIBLE (tip_f, 1);
	  ShowWindow (FRAME_W32_WINDOW (tip_f), SW_SHOWNOACTIVATE);
	  unblock_input ();

	  goto start_timer;
	}
      else if (tooltip_reuse_hidden_frame && EQ (frame, tip_last_frame))
	{
	  bool delete = false;
	  Lisp_Object tail, elt, parm, last;

	  /* Check if every parameter in PARMS has the same value in
	     tip_last_parms.  This may destruct tip_last_parms
	     which, however, will be recreated below.  */
	  for (tail = parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      /* The left, top, right and bottom parameters are handled
		 by compute_tip_xy so they can be ignored here.  */
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop)
		  && !EQ (parm, Qright) && !EQ (parm, Qbottom))
		{
		  last = Fassq (parm, tip_last_parms);
		  if (NILP (Fequal (Fcdr (elt), Fcdr (last))))
		    {
		      /* We lost, delete the old tooltip.  */
		      delete = true;
		      break;
		    }
		  else
		    tip_last_parms =
		      call2 (Qassq_delete_all, parm, tip_last_parms);
		}
	      else
		tip_last_parms =
		  call2 (Qassq_delete_all, parm, tip_last_parms);
	    }

	  /* Now check if there's a parameter left in tip_last_parms with a
	     non-nil value.  */
	  for (tail = tip_last_parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop) && !EQ (parm, Qright)
		  && !EQ (parm, Qbottom) && !NILP (Fcdr (elt)))
		{
		  /* We lost, delete the old tooltip.  */
		  delete = true;
		  break;
		}
	    }

	  x_hide_tip (delete);
	}
      else
	x_hide_tip (true);
    }
  else
    x_hide_tip (true);

  tip_last_frame = frame;
  tip_last_string = string;
  tip_last_parms = parms;

  /* Block input until the tip has been fully drawn, to avoid crashes
     when drawing tips in menus.  */
  block_input ();

  if (!FRAMEP (tip_frame) || !FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      /* Add default values to frame parameters.  */
      if (NILP (Fassq (Qname, parms)))
	parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
      if (NILP (Fassq (Qinternal_border_width, parms)))
	parms = Fcons (Fcons (Qinternal_border_width, make_number (3)), parms);
      if (NILP (Fassq (Qborder_width, parms)))
	parms = Fcons (Fcons (Qborder_width, make_number (1)), parms);
      if (NILP (Fassq (Qborder_color, parms)))
	parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")),
		       parms);
      if (NILP (Fassq (Qbackground_color, parms)))
	parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		       parms);

      /* Create a frame for the tooltip and record it in the global
	 variable tip_frame.  */
      struct frame *f;		/* The value is unused.  */
      if (NILP (tip_frame = x_create_tip_frame (FRAME_DISPLAY_INFO (f), parms)))
	{
	  /* Creating the tip frame failed.  */
	  unblock_input ();
	  return unbind_to (count, Qnil);
	}
    }

  tip_f = XFRAME (tip_frame);
  window = FRAME_ROOT_WINDOW (tip_f);
  tip_buf = Fget_buffer_create (tip);
  /* We will mark the tip window a "pseudo-window" below, and such
     windows cannot have display margins.  */
  bset_left_margin_cols (XBUFFER (tip_buf), make_number (0));
  bset_right_margin_cols (XBUFFER (tip_buf), make_number (0));
  set_window_buffer (window, tip_buf, false, false);
  w = XWINDOW (window);
  w->pseudo_window_p = true;

  /* Set up the frame's root window.  Note: The following code does not
     try to size the window or its frame correctly.  Its only purpose is
     to make the subsequent text size calculations work.  The right
     sizes should get installed when the toolkit gets back to us.  */
  w->left_col = 0;
  w->top_line = 0;
  w->pixel_left = 0;
  w->pixel_top = 0;

  if (CONSP (Vx_max_tooltip_size)
      && RANGED_INTEGERP (1, XCAR (Vx_max_tooltip_size), INT_MAX)
      && RANGED_INTEGERP (1, XCDR (Vx_max_tooltip_size), INT_MAX))
    {
      w->total_cols = XFASTINT (XCAR (Vx_max_tooltip_size));
      w->total_lines = XFASTINT (XCDR (Vx_max_tooltip_size));
    }
  else
    {
      w->total_cols = 80;
      w->total_lines = 40;
    }

  w->pixel_width = w->total_cols * FRAME_COLUMN_WIDTH (tip_f);
  w->pixel_height = w->total_lines * FRAME_LINE_HEIGHT (tip_f);
  FRAME_TOTAL_COLS (tip_f) = WINDOW_TOTAL_COLS (w);
  adjust_frame_glyphs (tip_f);

  /* Insert STRING into the root window's buffer and fit the frame to
     the buffer.  */
  count_1 = SPECPDL_INDEX ();
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (w->contents));
  bset_truncate_lines (current_buffer, Qnil);
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  specbind (Qinhibit_point_motion_hooks, Qt);
  Ferase_buffer ();
  Finsert (1, &string);
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (window, pos, TRY_WINDOW_IGNORE_FONTS_CHANGE);
  /* Calculate size of tooltip window.  */
  size = Fwindow_text_pixel_size (window, Qnil, Qnil, Qnil,
				  make_number (w->pixel_height), Qnil);
  /* Add the frame's internal border to calculated size.  */
  width = XINT (Fcar (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  height = XINT (Fcdr (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  /* Calculate position of tooltip frame.  */
  compute_tip_xy (tip_f, parms, dx, dy, width, height, &root_x, &root_y);

  /* Show tooltip frame.  */
  {
    RECT rect;
    int pad = (NUMBERP (Vw32_tooltip_extra_pixels)
	       ? max (0, XINT (Vw32_tooltip_extra_pixels))
	       : FRAME_COLUMN_WIDTH (tip_f));

    rect.left = rect.top = 0;
    rect.right = width;
    rect.bottom = height;
    AdjustWindowRect (&rect, tip_f->output_data.w32->dwStyle,
		      FRAME_EXTERNAL_MENU_BAR (tip_f));

    /* Position and size tooltip and put it in the topmost group.  */
    SetWindowPos (FRAME_W32_WINDOW (tip_f), HWND_TOPMOST,
		  root_x, root_y,
		  rect.right - rect.left + pad,
		  rect.bottom - rect.top, SWP_NOACTIVATE | SWP_NOOWNERZORDER);

    /* Ensure tooltip is on top of other topmost windows (eg menus).  */
    SetWindowPos (FRAME_W32_WINDOW (tip_f), HWND_TOP,
		  0, 0, 0, 0,
		  SWP_NOMOVE | SWP_NOSIZE
		  | SWP_NOACTIVATE | SWP_NOOWNERZORDER);

    /* Let redisplay know that we have made the frame visible already.  */
    SET_FRAME_VISIBLE (tip_f, 1);

    ShowWindow (FRAME_W32_WINDOW (tip_f), SW_SHOWNOACTIVATE);
  }

  w->must_be_updated_p = true;
  update_single_window (w);
  set_buffer_internal_1 (old_buffer);
  unbind_to (count_1, Qnil);
  unblock_input ();
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  return x_hide_tip (!tooltip_reuse_hidden_frame);
}

/***********************************************************************
			File selection dialog
 ***********************************************************************/

#define FILE_NAME_TEXT_FIELD edt1
#define FILE_NAME_COMBO_BOX cmb13
#define FILE_NAME_LIST lst1

/* Callback for altering the behavior of the Open File dialog.
   Makes the Filename text field contain "Current Directory" and be
   read-only when "Directories" is selected in the filter.  This
   allows us to work around the fact that the standard Open File
   dialog does not support directories.  */
static UINT_PTR CALLBACK
file_dialog_callback (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  if (msg == WM_NOTIFY)
    {
      OFNOTIFYW * notify_w = (OFNOTIFYW *)lParam;
      OFNOTIFYA * notify_a = (OFNOTIFYA *)lParam;
      int dropdown_changed;
      int dir_index;
#ifdef NTGUI_UNICODE
      const int use_unicode = 1;
#else /* !NTGUI_UNICODE */
      int use_unicode = w32_unicode_filenames;
#endif /* NTGUI_UNICODE */

      /* Detect when the Filter dropdown is changed.  */
      if (use_unicode)
	dropdown_changed =
	  notify_w->hdr.code == CDN_TYPECHANGE
	  || notify_w->hdr.code == CDN_INITDONE;
      else
	dropdown_changed =
	  notify_a->hdr.code == CDN_TYPECHANGE
	  || notify_a->hdr.code == CDN_INITDONE;
      if (dropdown_changed)
	{
	  HWND dialog = GetParent (hwnd);
	  HWND edit_control = GetDlgItem (dialog, FILE_NAME_TEXT_FIELD);
	  HWND list = GetDlgItem (dialog, FILE_NAME_LIST);
	  int hdr_code;

	  /* At least on Windows 7, the above attempt to get the window handle
	     to the File Name Text Field fails.	 The following code does the
	     job though.  Note that this code is based on my examination of the
	     window hierarchy using Microsoft Spy++.  bk */
	  if (edit_control == NULL)
	    {
	      HWND tmp = GetDlgItem (dialog, FILE_NAME_COMBO_BOX);
	      if (tmp)
		{
		  tmp = GetWindow (tmp, GW_CHILD);
		  if (tmp)
		    edit_control = GetWindow (tmp, GW_CHILD);
		}
	    }

	  /* Directories is in index 2.	 */
	  if (use_unicode)
	    {
	      dir_index = notify_w->lpOFN->nFilterIndex;
	      hdr_code = notify_w->hdr.code;
	    }
	  else
	    {
	      dir_index = notify_a->lpOFN->nFilterIndex;
	      hdr_code = notify_a->hdr.code;
	    }
	  if (dir_index == 2)
	    {
	      if (use_unicode)
		SendMessageW (dialog, CDM_SETCONTROLTEXT, FILE_NAME_TEXT_FIELD,
			      (LPARAM)L"Current Directory");
	      else
		SendMessageA (dialog, CDM_SETCONTROLTEXT, FILE_NAME_TEXT_FIELD,
			      (LPARAM)"Current Directory");
	      EnableWindow (edit_control, FALSE);
	      /* Note that at least on Windows 7, the above call to EnableWindow
		 disables the window that would ordinarily have focus.	If we
		 do not set focus to some other window here, focus will land in
		 no man's land and the user will be unable to tab through the
		 dialog box (pressing tab will only result in a beep).
		 Avoid that problem by setting focus to the list here.	*/
	      if (hdr_code == CDN_INITDONE)
		SetFocus (list);
	    }
	  else
	    {
	      /* Don't override default filename on init done.  */
	      if (hdr_code == CDN_TYPECHANGE)
		{
		  if (use_unicode)
		    SendMessageW (dialog, CDM_SETCONTROLTEXT,
				  FILE_NAME_TEXT_FIELD, (LPARAM)L"");
		  else
		    SendMessageA (dialog, CDM_SETCONTROLTEXT,
				  FILE_NAME_TEXT_FIELD, (LPARAM)"");
		}
	      EnableWindow (edit_control, TRUE);
	    }
	}
    }
  return 0;
}

void
w32_dialog_in_progress (Lisp_Object in_progress)
{
  Lisp_Object frames, frame;

  /* Don't let frames in `above' z-group obscure popups.  */
  FOR_EACH_FRAME (frames, frame)
    {
      struct frame *f = XFRAME (frame);

      if (!NILP (in_progress) && FRAME_Z_GROUP_ABOVE (f))
	x_set_z_group (f, Qabove_suspended, Qabove);
      else if (NILP (in_progress) && FRAME_Z_GROUP_ABOVE_SUSPENDED (f))
	x_set_z_group (f, Qabove, Qabove_suspended);
    }
}

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  /* Filter index: 1: All Files, 2: Directories only  */
  static const wchar_t filter_w[] = L"All Files (*.*)\0*.*\0Directories\0*|*\0";
#ifndef NTGUI_UNICODE
  static const char filter_a[] = "All Files (*.*)\0*.*\0Directories\0*|*\0";
#endif

  Lisp_Object filename = default_filename;
  struct frame *f = SELECTED_FRAME ();
  BOOL file_opened = FALSE;
  Lisp_Object orig_dir = dir;
  Lisp_Object orig_prompt = prompt;

  /* If we compile with _WIN32_WINNT set to 0x0400 (for NT4
     compatibility) we end up with the old file dialogs. Define a big
     enough struct for the new dialog to trick GetOpenFileName into
     giving us the new dialogs on newer versions of Windows.  */
  struct {
    OPENFILENAMEW details;
#if _WIN32_WINNT < 0x500 /* < win2k */
      PVOID pvReserved;
      DWORD dwReserved;
      DWORD FlagsEx;
#endif /* < win2k */
  } new_file_details_w;

#ifdef NTGUI_UNICODE
  wchar_t filename_buf_w[32*1024 + 1]; /* NT kernel maximum */
  OPENFILENAMEW * file_details_w = &new_file_details_w.details;
  const int use_unicode = 1;
#else /* not NTGUI_UNICODE */
  struct {
    OPENFILENAMEA details;
#if _WIN32_WINNT < 0x500 /* < win2k */
      PVOID pvReserved;
      DWORD dwReserved;
      DWORD FlagsEx;
#endif /* < win2k */
  } new_file_details_a;
  wchar_t filename_buf_w[MAX_PATH + 1], dir_w[MAX_PATH];
  char filename_buf_a[MAX_PATH + 1], dir_a[MAX_PATH];
  OPENFILENAMEW * file_details_w = &new_file_details_w.details;
  OPENFILENAMEA * file_details_a = &new_file_details_a.details;
  int use_unicode = w32_unicode_filenames;
  wchar_t *prompt_w;
  char *prompt_a UNINIT;
  int len;
  char fname_ret[MAX_UTF8_PATH];
#endif /* NTGUI_UNICODE */

  {
    /* Note: under NTGUI_UNICODE, we do _NOT_ use ENCODE_FILE: the
       system file encoding expected by the platform APIs (e.g. Cygwin's
       POSIX implementation) may not be the same as the encoding expected
       by the Windows "ANSI" APIs!  */

    CHECK_STRING (prompt);
    CHECK_STRING (dir);

    dir = Fexpand_file_name (dir, Qnil);

    if (STRINGP (filename))
      filename = Ffile_name_nondirectory (filename);
    else
      filename = empty_unibyte_string;

#ifdef CYGWIN
    dir = Fcygwin_convert_file_name_to_windows (dir, Qt);
    if (SCHARS (filename) > 0)
      filename = Fcygwin_convert_file_name_to_windows (filename, Qnil);
#endif

    CHECK_STRING (dir);
    CHECK_STRING (filename);

    /* The code in file_dialog_callback that attempts to set the text
       of the file name edit window when handling the CDN_INITDONE
       WM_NOTIFY message does not work.  Setting filename to "Current
       Directory" in the only_dir_p case here does work however.  */
    if (SCHARS (filename) == 0 && ! NILP (only_dir_p))
      filename = build_string ("Current Directory");

    /* Convert the values we've computed so far to system form.  */
#ifdef NTGUI_UNICODE
    to_unicode (prompt, &prompt);
    to_unicode (dir, &dir);
    to_unicode (filename, &filename);
    if (SBYTES (filename) + 1 > sizeof (filename_buf_w))
      report_file_error ("filename too long", default_filename);

    memcpy (filename_buf_w, SDATA (filename), SBYTES (filename) + 1);
#else /* !NTGUI_UNICODE */
    prompt = ENCODE_FILE (prompt);
    dir = ENCODE_FILE (dir);
    filename = ENCODE_FILE (filename);

    /* We modify these in-place, so make copies for safety.  */
    dir = Fcopy_sequence (dir);
    unixtodos_filename (SSDATA (dir));
    filename = Fcopy_sequence (filename);
    unixtodos_filename (SSDATA (filename));
    if (SBYTES (filename) >= MAX_UTF8_PATH)
      report_file_error ("filename too long", default_filename);
    if (w32_unicode_filenames)
      {
	filename_to_utf16 (SSDATA (dir), dir_w);
	if (filename_to_utf16 (SSDATA (filename), filename_buf_w) != 0)
	  {
	    /* filename_to_utf16 sets errno to ENOENT when the file
	       name is too long or cannot be converted to UTF-16.  */
	    if (errno == ENOENT && filename_buf_w[MAX_PATH - 1] != 0)
	      report_file_error ("filename too long", default_filename);
	  }
	len = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
				    SSDATA (prompt), -1, NULL, 0);
	if (len > 32768)
	  len = 32768;
	prompt_w = alloca (len * sizeof (wchar_t));
	pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
			      SSDATA (prompt), -1, prompt_w, len);
      }
    else
      {
	filename_to_ansi (SSDATA (dir), dir_a);
	if (filename_to_ansi (SSDATA (filename), filename_buf_a) != '\0')
	  {
	    /* filename_to_ansi sets errno to ENOENT when the file
	       name is too long or cannot be converted to UTF-16.  */
	    if (errno == ENOENT && filename_buf_a[MAX_PATH - 1] != 0)
	      report_file_error ("filename too long", default_filename);
	  }
	len = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
				    SSDATA (prompt), -1, NULL, 0);
	if (len > 32768)
	  len = 32768;
	prompt_w = alloca (len * sizeof (wchar_t));
	pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
			      SSDATA (prompt), -1, prompt_w, len);
	len = pWideCharToMultiByte (CP_ACP, 0, prompt_w, -1, NULL, 0, NULL, NULL);
	if (len > 32768)
	  len = 32768;
	prompt_a = alloca (len);
	pWideCharToMultiByte (CP_ACP, 0, prompt_w, -1, prompt_a, len, NULL, NULL);
      }
#endif /* NTGUI_UNICODE */

    /* Fill in the structure for the call to GetOpenFileName below.
       For NTGUI_UNICODE builds (which run only on NT), we just use
       the actual size of the structure.  For non-NTGUI_UNICODE
       builds, we tell the OS we're using an old version of the
       structure if the OS isn't new enough to support the newer
       version.  */
    if (use_unicode)
      {
	memset (&new_file_details_w, 0, sizeof (new_file_details_w));
	if (w32_major_version > 4 && w32_major_version < 95)
	  file_details_w->lStructSize = sizeof (new_file_details_w);
	else
	  file_details_w->lStructSize = sizeof (*file_details_w);
	/* Set up the inout parameter for the selected file name.  */
	file_details_w->lpstrFile = filename_buf_w;
	file_details_w->nMaxFile =
	  sizeof (filename_buf_w) / sizeof (*filename_buf_w);
	file_details_w->hwndOwner = FRAME_W32_WINDOW (f);
	/* Undocumented Bug in Common File Dialog:
	   If a filter is not specified, shell links are not resolved.  */
	file_details_w->lpstrFilter = filter_w;
#ifdef NTGUI_UNICODE
	file_details_w->lpstrInitialDir = (wchar_t*) SDATA (dir);
	file_details_w->lpstrTitle = (guichar_t*) SDATA (prompt);
#else
	file_details_w->lpstrInitialDir = dir_w;
	file_details_w->lpstrTitle = prompt_w;
#endif
	file_details_w->nFilterIndex = NILP (only_dir_p) ? 1 : 2;
	file_details_w->Flags = (OFN_HIDEREADONLY | OFN_NOCHANGEDIR
				 | OFN_EXPLORER | OFN_ENABLEHOOK);
	if (!NILP (mustmatch))
	  {
	    /* Require that the path to the parent directory exists.  */
	    file_details_w->Flags |= OFN_PATHMUSTEXIST;
	    /* If we are looking for a file, require that it exists.  */
	    if (NILP (only_dir_p))
	      file_details_w->Flags |= OFN_FILEMUSTEXIST;
	  }
      }
#ifndef NTGUI_UNICODE
    else
      {
	memset (&new_file_details_a, 0, sizeof (new_file_details_a));
	if (w32_major_version > 4 && w32_major_version < 95)
	  file_details_a->lStructSize = sizeof (new_file_details_a);
	else
	  file_details_a->lStructSize = sizeof (*file_details_a);
	file_details_a->lpstrFile = filename_buf_a;
	file_details_a->nMaxFile =
	  sizeof (filename_buf_a) / sizeof (*filename_buf_a);
	file_details_a->hwndOwner = FRAME_W32_WINDOW (f);
	file_details_a->lpstrFilter = filter_a;
	file_details_a->lpstrInitialDir = dir_a;
	file_details_a->lpstrTitle = prompt_a;
	file_details_a->nFilterIndex = NILP (only_dir_p) ? 1 : 2;
	file_details_a->Flags = (OFN_HIDEREADONLY | OFN_NOCHANGEDIR
				 | OFN_EXPLORER | OFN_ENABLEHOOK);
	if (!NILP (mustmatch))
	  {
	    /* Require that the path to the parent directory exists.  */
	    file_details_a->Flags |= OFN_PATHMUSTEXIST;
	    /* If we are looking for a file, require that it exists.  */
	    if (NILP (only_dir_p))
	      file_details_a->Flags |= OFN_FILEMUSTEXIST;
	  }
      }
#endif	/* !NTGUI_UNICODE */

    {
      int count = SPECPDL_INDEX ();

      w32_dialog_in_progress (Qt);

      /* Prevent redisplay.  */
      specbind (Qinhibit_redisplay, Qt);
      record_unwind_protect (w32_dialog_in_progress, Qnil);
      block_input ();
      if (use_unicode)
	{
	  file_details_w->lpfnHook = file_dialog_callback;

	  file_opened = GetOpenFileNameW (file_details_w);
	}
#ifndef NTGUI_UNICODE
      else
	{
	  file_details_a->lpfnHook = file_dialog_callback;

	  file_opened = GetOpenFileNameA (file_details_a);
	}
#endif	/* !NTGUI_UNICODE */
      unblock_input ();
      unbind_to (count, Qnil);
    }

    if (file_opened)
      {
	/* Get an Emacs string from the value Windows gave us.  */
#ifdef NTGUI_UNICODE
	filename = from_unicode_buffer (filename_buf_w);
#else /* !NTGUI_UNICODE */
	if (use_unicode)
	  filename_from_utf16 (filename_buf_w, fname_ret);
	else
	  filename_from_ansi (filename_buf_a, fname_ret);
	dostounix_filename (fname_ret);
	filename = DECODE_FILE (build_unibyte_string (fname_ret));
#endif /* NTGUI_UNICODE */

#ifdef CYGWIN
	filename = Fcygwin_convert_file_name_from_windows (filename, Qt);
#endif /* CYGWIN */

	/* Strip the dummy filename off the end of the string if we
	   added it to select a directory.  */
	if ((use_unicode && file_details_w->nFilterIndex == 2)
#ifndef NTGUI_UNICODE
	    || (!use_unicode && file_details_a->nFilterIndex == 2)
#endif
	    )
	  filename = Ffile_name_directory (filename);
      }
    /* User canceled the dialog without making a selection.  */
    else if (!CommDlgExtendedError ())
      filename = Qnil;
    /* An error occurred, fallback on reading from the mini-buffer.  */
    else
      filename = Fcompleting_read (
	orig_prompt,
	intern ("read-file-name-internal"),
	orig_dir,
	mustmatch,
	orig_dir,
	Qfile_name_history,
	default_filename,
	Qnil);
  }

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (filename))
    quit ();

  return filename;
}


#ifdef WINDOWSNT
/* Moving files to the system recycle bin.
   Used by `move-file-to-trash' instead of the default moving to ~/.Trash  */
DEFUN ("system-move-file-to-trash", Fsystem_move_file_to_trash,
       Ssystem_move_file_to_trash, 1, 1, 0,
       doc: /* Move file or directory named FILENAME to the recycle bin.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;
  Lisp_Object encoded_file;
  Lisp_Object operation;

  operation = Qdelete_file;
  if (!NILP (Ffile_directory_p (filename))
      && NILP (Ffile_symlink_p (filename)))
    {
      operation = intern ("delete-directory");
      filename = Fdirectory_file_name (filename);
    }

  /* Must have fully qualified file names for moving files to Recycle
     Bin. */
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename, operation);
  if (!NILP (handler))
    return call2 (handler, operation, filename);
  else
    {
      const char * path;
      int result;

      encoded_file = ENCODE_FILE (filename);

      path = map_w32_filename (SSDATA (encoded_file), NULL);

      /* The Unicode version of SHFileOperation is not supported on
	 Windows 9X. */
      if (w32_unicode_filenames && os_subtype != OS_9X)
	{
	  SHFILEOPSTRUCTW file_op_w;
	  /* We need one more element beyond MAX_PATH because this is
	     a list of file names, with the last element double-null
	     terminated. */
	  wchar_t tmp_path_w[MAX_PATH + 1];

	  memset (tmp_path_w, 0, sizeof (tmp_path_w));
	  filename_to_utf16 (path, tmp_path_w);

	  /* On Windows, write permission is required to delete/move files.  */
	  _wchmod (tmp_path_w, 0666);

	  memset (&file_op_w, 0, sizeof (file_op_w));
	  file_op_w.hwnd = HWND_DESKTOP;
	  file_op_w.wFunc = FO_DELETE;
	  file_op_w.pFrom = tmp_path_w;
	  file_op_w.fFlags = FOF_SILENT | FOF_NOCONFIRMATION | FOF_ALLOWUNDO
	    | FOF_NOERRORUI | FOF_NO_CONNECTED_ELEMENTS;
	  file_op_w.fAnyOperationsAborted = FALSE;

	  result = SHFileOperationW (&file_op_w);
	}
      else
	{
	  SHFILEOPSTRUCTA file_op_a;
	  char tmp_path_a[MAX_PATH + 1];

	  memset (tmp_path_a, 0, sizeof (tmp_path_a));
	  filename_to_ansi (path, tmp_path_a);

	  /* If a file cannot be represented in ANSI codepage, don't
	     let them inadvertently delete other files because some
	     characters are interpreted as a wildcards.  */
	  if (_mbspbrk ((unsigned char *)tmp_path_a,
			(const unsigned char *)"?*"))
	    result = ERROR_FILE_NOT_FOUND;
	  else
	    {
	      _chmod (tmp_path_a, 0666);

	      memset (&file_op_a, 0, sizeof (file_op_a));
	      file_op_a.hwnd = HWND_DESKTOP;
	      file_op_a.wFunc = FO_DELETE;
	      file_op_a.pFrom = tmp_path_a;
	      file_op_a.fFlags = FOF_SILENT | FOF_NOCONFIRMATION | FOF_ALLOWUNDO
		| FOF_NOERRORUI | FOF_NO_CONNECTED_ELEMENTS;
	      file_op_a.fAnyOperationsAborted = FALSE;

	      result = SHFileOperationA (&file_op_a);
	    }
	}
      if (result != 0)
	report_file_error ("Removing old name", list1 (filename));
    }
  return Qnil;
}

#endif /* WINDOWSNT */


/***********************************************************************
			 w32 specialized functions
 ***********************************************************************/

DEFUN ("w32-send-sys-command", Fw32_send_sys_command,
       Sw32_send_sys_command, 1, 2, 0,
       doc: /* Send frame a Windows WM_SYSCOMMAND message of type COMMAND.
Some useful values for COMMAND are #xf030 to maximize frame (#xf020
to minimize), #xf120 to restore frame to original size, and #xf100
to activate the menubar for keyboard access.  #xf140 activates the
screen saver if defined.

If optional parameter FRAME is not specified, use selected frame.  */)
  (Lisp_Object command, Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);

  CHECK_NUMBER (command);

  if (FRAME_W32_P (f))
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
	       executable program (application).  If it is an application,
	       that application is launched in the current buffer's default
	       directory.  Otherwise, the application associated with
	       DOCUMENT is launched in the buffer's default directory.
 \"opennew\" - like \"open\", but instruct the application to open
	       DOCUMENT in a new window.
 \"openas\"  - open the \"Open With\" dialog for DOCUMENT.
 \"print\"   - print DOCUMENT, which must be a file.
 \"printto\" - print DOCUMENT, which must be a file, to a specified printer.
	       The printer should be provided in PARAMETERS, see below.
 \"explore\" - start the Windows Explorer on DOCUMENT.
 \"edit\"    - launch an editor and open DOCUMENT for editing; which
	       editor is launched depends on the association for the
	       specified DOCUMENT.
 \"find\"    - initiate search starting from DOCUMENT, which must specify
	       a directory.
 \"delete\"  - move DOCUMENT, a file or a directory, to Recycle Bin.
 \"copy\"    - copy DOCUMENT, which must be a file or a directory, into
	       the clipboard.
 \"cut\"     - move DOCUMENT, a file or a directory, into the clipboard.
 \"paste\"   - paste the file whose name is in the clipboard into DOCUMENT,
	       which must be a directory.
 \"pastelink\"
	   - create a shortcut in DOCUMENT (which must be a directory)
	       the file or directory whose name is in the clipboard.
 \"runas\"   - run DOCUMENT, which must be an excutable file, with
	       elevated privileges (a.k.a. \"as Administrator\").
 \"properties\"
	   - open the property sheet dialog for DOCUMENT.
 nil       - invoke the default OPERATION, or \"open\" if default is
	       not defined or unavailable.

DOCUMENT is typically the name of a document file or a URL, but can
also be an executable program to run, or a directory to open in the
Windows Explorer.  If it is a file or a directory, it must be a local
one; this function does not support remote file names.

If DOCUMENT is an executable program, the optional third arg PARAMETERS
can be a string containing command line parameters, separated by blanks,
that will be passed to the program.  Some values of OPERATION also require
parameters (e.g., \"printto\" requires the printer address).  Otherwise,
PARAMETERS should be nil or unspecified.  Note that double quote characters
in PARAMETERS must each be enclosed in 2 additional quotes, as in \"\"\".

Optional fourth argument SHOW-FLAG can be used to control how the
application will be displayed when it is invoked.  If SHOW-FLAG is nil
or unspecified, the application is displayed as if SHOW-FLAG of 10 was
specified, otherwise it is an integer between 0 and 11 representing
a ShowWindow flag:

  0 - start hidden
  1 - start as normal-size window
  3 - start in a maximized window
  6 - start in a minimized window
 10 - start as the application itself specifies; this is the default.  */)
  (Lisp_Object operation, Lisp_Object document, Lisp_Object parameters, Lisp_Object show_flag)
{
  char *errstr;
  Lisp_Object current_dir = BVAR (current_buffer, directory);;
  wchar_t *doc_w = NULL, *params_w = NULL, *ops_w = NULL;
#ifdef CYGWIN
  intptr_t result;
#else
  int use_unicode = w32_unicode_filenames;
  char *doc_a = NULL, *params_a = NULL, *ops_a = NULL;
  Lisp_Object absdoc, handler;
  BOOL success;
#endif

  CHECK_STRING (document);

#ifdef CYGWIN
  current_dir = Fcygwin_convert_file_name_to_windows (current_dir, Qt);
  document = Fcygwin_convert_file_name_to_windows (document, Qt);

  /* Encode filename, current directory and parameters.  */
  current_dir = GUI_ENCODE_FILE (current_dir);
  document = GUI_ENCODE_FILE (document);
  doc_w = GUI_SDATA (document);
  if (STRINGP (parameters))
    {
      parameters = GUI_ENCODE_SYSTEM (parameters);
      params_w = GUI_SDATA (parameters);
    }
  if (STRINGP (operation))
    {
      operation = GUI_ENCODE_SYSTEM (operation);
      ops_w = GUI_SDATA (operation);
    }
  result = (intptr_t) ShellExecuteW (NULL, ops_w, doc_w, params_w,
				     GUI_SDATA (current_dir),
				     (INTEGERP (show_flag)
				      ? XINT (show_flag) : SW_SHOWDEFAULT));

  if (result > 32)
    return Qt;

  switch (result)
    {
    case SE_ERR_ACCESSDENIED:
      errstr = w32_strerror (ERROR_ACCESS_DENIED);
      break;
    case SE_ERR_ASSOCINCOMPLETE:
    case SE_ERR_NOASSOC:
      errstr = w32_strerror (ERROR_NO_ASSOCIATION);
      break;
    case SE_ERR_DDEBUSY:
    case SE_ERR_DDEFAIL:
      errstr = w32_strerror (ERROR_DDE_FAIL);
      break;
    case SE_ERR_DDETIMEOUT:
      errstr = w32_strerror (ERROR_TIMEOUT);
      break;
    case SE_ERR_DLLNOTFOUND:
      errstr = w32_strerror (ERROR_DLL_NOT_FOUND);
      break;
    case SE_ERR_FNF:
      errstr = w32_strerror (ERROR_FILE_NOT_FOUND);
      break;
    case SE_ERR_OOM:
      errstr = w32_strerror (ERROR_NOT_ENOUGH_MEMORY);
      break;
    case SE_ERR_PNF:
      errstr = w32_strerror (ERROR_PATH_NOT_FOUND);
      break;
    case SE_ERR_SHARE:
      errstr = w32_strerror (ERROR_SHARING_VIOLATION);
      break;
    default:
      errstr = w32_strerror (0);
      break;
    }

#else  /* !CYGWIN */

  const char file_url_str[] = "file:///";
  const int file_url_len = sizeof (file_url_str) - 1;
  int doclen;

  if (strncmp (SSDATA (document), file_url_str, file_url_len) == 0)
    {
      /* Passing "file:///" URLs to ShellExecute causes shlwapi.dll to
	 start a thread in some rare system configurations, for
	 unknown reasons.  That thread is started in the context of
	 the Emacs process, but out of control of our code, and seems
	 to never exit afterwards.  Each such thread reserves 8MB of
	 stack space (because that's the value recorded in the Emacs
	 executable at link time: Emacs needs a large stack).  So a
	 large enough number of invocations of w32-shell-execute can
	 potentially cause the Emacs process to run out of available
	 address space, which is nasty.  To work around this, we
	 convert such URLs to local file names, which seems to prevent
	 those threads from starting.  See bug #20220.  */
      char *p = SSDATA (document) + file_url_len;

      if (c_isalpha (*p) && p[1] == ':' && IS_DIRECTORY_SEP (p[2]))
	document = Fsubstring_no_properties (document,
					     make_number (file_url_len), Qnil);
    }
  /* We have a situation here.  If DOCUMENT is a relative file name,
     but its name includes leading directories, i.e. it lives not in
     CURRENT_DIR, but in its subdirectory, then ShellExecute below
     will fail to find it.  So we need to make the file name is
     absolute.  But DOCUMENT does not have to be a file, it can be a
     URL, for example.  So we make it absolute only if it is an
     existing file; if it is a file that does not exist, tough.  */
  absdoc = Fexpand_file_name (document, Qnil);
  /* Don't call file handlers for file-exists-p, since they might
     attempt to access the file, which could fail or produce undesired
     consequences, see bug#16558 for an example.  */
  handler = Ffind_file_name_handler (absdoc, Qfile_exists_p);
  if (NILP (handler))
    {
      Lisp_Object absdoc_encoded = ENCODE_FILE (absdoc);

      if (faccessat (AT_FDCWD, SSDATA (absdoc_encoded), F_OK, AT_EACCESS) == 0)
	{
	  /* ShellExecute fails if DOCUMENT is a UNC with forward
	     slashes (expand-file-name above converts all backslashes
	     to forward slashes).  Now that we know DOCUMENT is a
	     file, we can mirror all forward slashes into backslashes.  */
	  unixtodos_filename (SSDATA (absdoc_encoded));
	  document = absdoc_encoded;
	}
      else
	document = ENCODE_FILE (document);
    }
  else
    document = ENCODE_FILE (document);

  current_dir = ENCODE_FILE (current_dir);
  /* Cannot use filename_to_utf16/ansi with DOCUMENT, since it could
     be a URL that is not limited to MAX_PATH chararcters.  */
  doclen = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
				 SSDATA (document), -1, NULL, 0);
  doc_w = xmalloc (doclen * sizeof (wchar_t));
  pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
			SSDATA (document), -1, doc_w, doclen);
  if (use_unicode)
    {
      wchar_t current_dir_w[MAX_PATH];
      SHELLEXECUTEINFOW shexinfo_w;

      /* Encode the current directory and parameters, and convert
	 operation to UTF-16.  */
      filename_to_utf16 (SSDATA (current_dir), current_dir_w);
      if (STRINGP (parameters))
	{
	  int len;

	  parameters = ENCODE_SYSTEM (parameters);
	  len = pMultiByteToWideChar (CP_ACP, multiByteToWideCharFlags,
				      SSDATA (parameters), -1, NULL, 0);
	  if (len > 32768)
	    len = 32768;
	  params_w = alloca (len * sizeof (wchar_t));
	  pMultiByteToWideChar (CP_ACP, multiByteToWideCharFlags,
				SSDATA (parameters), -1, params_w, len);
	  params_w[len - 1] = 0;
	}
      if (STRINGP (operation))
	{
	  /* Assume OPERATION is pure ASCII.  */
	  const char *s = SSDATA (operation);
	  wchar_t *d;
	  int len = SBYTES (operation) + 1;

	  if (len > 32768)
	    len = 32768;
	  d = ops_w = alloca (len * sizeof (wchar_t));
	  while (d < ops_w + len - 1)
	    *d++ = *s++;
	  *d = 0;
	}

      /* Using ShellExecuteEx and setting the SEE_MASK_INVOKEIDLIST
	 flag succeeds with more OPERATIONs (a.k.a. "verbs"), as it is
	 able to invoke verbs from shortcut menu extensions, not just
	 static verbs listed in the Registry.  */
      memset (&shexinfo_w, 0, sizeof (shexinfo_w));
      shexinfo_w.cbSize = sizeof (shexinfo_w);
      shexinfo_w.fMask =
	SEE_MASK_INVOKEIDLIST | SEE_MASK_FLAG_DDEWAIT | SEE_MASK_FLAG_NO_UI;
      shexinfo_w.hwnd = NULL;
      shexinfo_w.lpVerb = ops_w;
      shexinfo_w.lpFile = doc_w;
      shexinfo_w.lpParameters = params_w;
      shexinfo_w.lpDirectory = current_dir_w;
      shexinfo_w.nShow =
	(INTEGERP (show_flag) ? XINT (show_flag) : SW_SHOWDEFAULT);
      success = ShellExecuteExW (&shexinfo_w);
      xfree (doc_w);
    }
  else
    {
      char current_dir_a[MAX_PATH];
      SHELLEXECUTEINFOA shexinfo_a;
      int codepage = codepage_for_filenames (NULL);
      int ldoc_a = pWideCharToMultiByte (codepage, 0, doc_w, -1, NULL, 0,
					 NULL, NULL);

      doc_a = xmalloc (ldoc_a);
      pWideCharToMultiByte (codepage, 0, doc_w, -1, doc_a, ldoc_a, NULL, NULL);
      filename_to_ansi (SSDATA (current_dir), current_dir_a);
      if (STRINGP (parameters))
	{
	  parameters = ENCODE_SYSTEM (parameters);
	  params_a = SSDATA (parameters);
	}
      if (STRINGP (operation))
	{
	  /* Assume OPERATION is pure ASCII.  */
	  ops_a = SSDATA (operation);
	}
      memset (&shexinfo_a, 0, sizeof (shexinfo_a));
      shexinfo_a.cbSize = sizeof (shexinfo_a);
      shexinfo_a.fMask =
	SEE_MASK_INVOKEIDLIST | SEE_MASK_FLAG_DDEWAIT | SEE_MASK_FLAG_NO_UI;
      shexinfo_a.hwnd = NULL;
      shexinfo_a.lpVerb = ops_a;
      shexinfo_a.lpFile = doc_a;
      shexinfo_a.lpParameters = params_a;
      shexinfo_a.lpDirectory = current_dir_a;
      shexinfo_a.nShow =
	(INTEGERP (show_flag) ? XINT (show_flag) : SW_SHOWDEFAULT);
      success = ShellExecuteExA (&shexinfo_a);
      xfree (doc_w);
      xfree (doc_a);
    }

  if (success)
    return Qt;

  errstr = w32_strerror (0);

#endif /* !CYGWIN */

  /* The error string might be encoded in the locale's encoding.  */
  if (!NILP (Vlocale_coding_system))
    {
      Lisp_Object decoded =
	code_convert_string_norecord (build_unibyte_string (errstr),
				      Vlocale_coding_system, 0);
      errstr = SSDATA (decoded);
    }
  error ("ShellExecute failed: %s", errstr);
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

  if (w32_kbdhook_active)
    {
      /* Alphanumerics map to themselves.  */
      if (key[1] == 0)
      {
	if ((key[0] >= 'A' && key[0] <= 'Z')
	    || (key[0] >= '0' && key[0] <= '9'))
	  return key[0];
	if (key[0] >= 'a' && key[0] <= 'z')
	  return toupper(key[0]);
      }
    }

  return -1;
}

/* Convert a one-element vector style key sequence to a hot key
   definition.  */
static Lisp_Object
w32_parse_and_hook_hot_key (Lisp_Object key, int hook)
{
  /* Copied from Fdefine_key and store_in_keymap.  */
  register Lisp_Object c;
  int vk_code = 0;
  int lisp_modifiers = 0;
  int w32_modifiers;
  Lisp_Object res = Qnil;
  char* vkname;

  CHECK_VECTOR (key);

  if (ASIZE (key) != 1)
    return Qnil;

  c = AREF (key, 0);

  if (CONSP (c) && lucid_event_type_list_p (c))
    c = Fevent_convert_list (c);

  if (! INTEGERP (c) && ! SYMBOLP (c))
    error ("Key definition is invalid");

  /* Work out the base key and the modifiers.  */
  if (SYMBOLP (c))
    {
      c = parse_modifiers (c);
      lisp_modifiers = XINT (Fcar (Fcdr (c)));
      c = Fcar (c);
      if (!SYMBOLP (c))
	emacs_abort ();
      vkname = SSDATA (SYMBOL_NAME (c));
      /* [s-], [M-], [h-]: Register all keys for this modifier */
      if (w32_kbdhook_active && vkname[0] == 0)
        vk_code = VK_ANY;
      else
        vk_code = lookup_vk_code (vkname);
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

  if (w32_kbdhook_active)
    {
      /* Register Alt-x combinations.  */
      if (lisp_modifiers & alt_modifier)
        {
          hook_w32_key (hook, VK_MENU, vk_code);
          res = Qt;
        }
      /* Register Win-x combinations based on modifier mappings.  */
      if (((lisp_modifiers & hyper_modifier)
	   && EQ (Vw32_lwindow_modifier, Qhyper))
	  || ((lisp_modifiers & super_modifier)
	      && EQ (Vw32_lwindow_modifier, Qsuper)))
        {
          hook_w32_key (hook, VK_LWIN, vk_code);
          res = Qt;
        }
      if (((lisp_modifiers & hyper_modifier)
	   && EQ (Vw32_rwindow_modifier, Qhyper))
	  || ((lisp_modifiers & super_modifier)
	      && EQ (Vw32_rwindow_modifier, Qsuper)))
        {
          hook_w32_key (hook, VK_RWIN, vk_code);
          res = Qt;
        }
      return res;
    }
  else
    {
      /* Convert lisp modifiers to Windows hot-key form.  */
      w32_modifiers  = (lisp_modifiers & hyper_modifier)    ? MOD_WIN : 0;
      w32_modifiers |= (lisp_modifiers & alt_modifier)      ? MOD_ALT : 0;
      w32_modifiers |= (lisp_modifiers & ctrl_modifier)     ? MOD_CONTROL : 0;
      w32_modifiers |= (lisp_modifiers & shift_modifier)    ? MOD_SHIFT : 0;

      return HOTKEY (vk_code, w32_modifiers);
    }
}

DEFUN ("w32-register-hot-key", Fw32_register_hot_key,
       Sw32_register_hot_key, 1, 1, 0,
       doc: /* Register KEY as a hot-key combination.
Certain key combinations like Alt-Tab and Win-R are reserved for
system use on Windows, and therefore are normally intercepted by the
system.  These key combinations can be received by registering them
as hot-keys, except for Win-L which always locks the computer.

On Windows 98 and ME, KEY must be a one element key definition in
vector form that would be acceptable to `define-key' (e.g. [A-tab] for
Alt-Tab).  The meta modifier is interpreted as Alt if
`w32-alt-is-meta' is t, and hyper is always interpreted as the Windows
modifier keys.  The return value is the hotkey-id if registered, otherwise nil.

On Windows versions since NT, KEY can also be specified as [M-], [s-] or
[h-] to indicate that all combinations of that key should be processed
by Emacs instead of the operating system.  The super and hyper
modifiers are interpreted according to the current values of
`w32-lwindow-modifier' and `w32-rwindow-modifier'.  For instance,
setting `w32-lwindow-modifier' to `super' and then calling
`(register-hot-key [s-])' grabs all combinations of the left Windows
key to Emacs, but leaves the right Windows key free for the operating
system keyboard shortcuts.  The return value is t if the call affected
any key combinations, otherwise nil.  */)
  (Lisp_Object key)
{
  key = w32_parse_and_hook_hot_key (key, 1);

  if (!w32_kbdhook_active
      && !NILP (key) && NILP (Fmemq (key, w32_grabbed_keys)))
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
			 (WPARAM) XINT (key), 0);
    }

  return key;
}

DEFUN ("w32-unregister-hot-key", Fw32_unregister_hot_key,
       Sw32_unregister_hot_key, 1, 1, 0,
       doc: /* Unregister KEY as a hot-key combination.  */)
  (Lisp_Object key)
{
  Lisp_Object item;

  if (!INTEGERP (key))
    key = w32_parse_and_hook_hot_key (key, 0);

  if (w32_kbdhook_active)
    return key;

  item = Fmemq (key, w32_grabbed_keys);

  if (!NILP (item))
    {
      LPARAM lparam;

      eassert (CONSP (item));
      /* Pass the tail of the list as a pointer to a Lisp_Cons cell,
	 so that it works in a --with-wide-int build as well.  */
      lparam = (LPARAM) XUNTAG (item, Lisp_Cons);

      /* Notify input thread about hot-key definition being removed, so
	 that it takes effect without needing focus switch.  */
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_UNREGISTER_HOT_KEY,
			     (WPARAM) XINT (XCAR (item)), lparam))
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
  (void)
{
  return Fdelq (Qnil, Fcopy_sequence (w32_grabbed_keys));
}

DEFUN ("w32-reconstruct-hot-key", Fw32_reconstruct_hot_key,
       Sw32_reconstruct_hot_key, 1, 1, 0,
       doc: /* Convert hot-key ID to a lisp key combination.
usage: (w32-reconstruct-hot-key ID)  */)
  (Lisp_Object hotkeyid)
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
is set to off if the low bit of NEW-STATE is zero, otherwise on.
If NEW-STATE is omitted or nil, the function toggles the state,

Value is the new state of the key, or nil if the function failed
to change the state.  */)
  (Lisp_Object key, Lisp_Object new_state)
{
  int vk_code;
  LPARAM lparam;

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

  if (NILP (new_state))
    lparam = -1;
  else
    lparam = (XUINT (new_state)) & 1;
  if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_TOGGLE_LOCK_KEY,
			 (WPARAM) vk_code, lparam))
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
  (Lisp_Object class, Lisp_Object name)
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

DEFUN ("w32-frame-geometry", Fw32_frame_geometry, Sw32_frame_geometry, 0, 1, 0,
       doc: /* Return geometric attributes of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is an association list of the attributes listed below.  All height
and width values are in pixels.

`outer-position' is a cons of the outer left and top edges of FRAME
  relative to the origin - the position (0, 0) - of FRAME's display.

`outer-size' is a cons of the outer width and height of FRAME.  The
  outer size includes the title bar and the external borders as well as
  any menu and/or tool bar of frame.

`external-border-size' is a cons of the horizontal and vertical width of
  FRAME's external borders as supplied by the window manager.

`title-bar-size' is a cons of the width and height of the title bar of
  FRAME as supplied by the window manager.  If both of them are zero,
  FRAME has no title bar.  If only the width is zero, Emacs was not
  able to retrieve the width information.

`menu-bar-external', if non-nil, means the menu bar is external (never
  included in the inner edges of FRAME).

`menu-bar-size' is a cons of the width and height of the menu bar of
  FRAME.

`tool-bar-external', if non-nil, means the tool bar is external (never
  included in the inner edges of FRAME).

`tool-bar-position' tells on which side the tool bar on FRAME is and can
  be one of `left', `top', `right' or `bottom'.  If this is nil, FRAME
  has no tool bar.

`tool-bar-size' is a cons of the width and height of the tool bar of
  FRAME.

`internal-border-width' is the width of the internal border of
  FRAME.  */)
  (Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);

  MENUBARINFO menu_bar;
  WINDOWINFO window;
  int left, top, right, bottom;
  unsigned int external_border_width, external_border_height;
  int title_bar_width = 0, title_bar_height = 0;
  int single_menu_bar_height, wrapped_menu_bar_height, menu_bar_height;
  int tool_bar_height = FRAME_TOOL_BAR_HEIGHT (f);
  int internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);

  if (FRAME_INITIAL_P (f) || !FRAME_W32_P (f))
    return Qnil;

  block_input ();
  /* Outer rectangle and borders.  */
  window.cbSize = sizeof (window);
  GetWindowInfo (FRAME_W32_WINDOW (f), &window);
  external_border_width = window.cxWindowBorders;
  external_border_height = window.cyWindowBorders;
  /* Title bar.  */
  if (get_title_bar_info_fn)
    {
      TITLEBAR_INFO title_bar;

      title_bar.cbSize = sizeof (title_bar);
      title_bar.rcTitleBar.left = title_bar.rcTitleBar.right = 0;
      title_bar.rcTitleBar.top = title_bar.rcTitleBar.bottom = 0;
      for (int i = 0; i < 6; i++)
	title_bar.rgstate[i] = 0;
      if (get_title_bar_info_fn (FRAME_W32_WINDOW (f), &title_bar)
	  && !(title_bar.rgstate[0] & 0x00008001))
	{
	  title_bar_width
	    = title_bar.rcTitleBar.right - title_bar.rcTitleBar.left;
	  title_bar_height
	    = title_bar.rcTitleBar.bottom - title_bar.rcTitleBar.top;
	}
    }
  else if ((window.dwStyle & WS_CAPTION) == WS_CAPTION)
    title_bar_height = GetSystemMetrics (SM_CYCAPTION);
  /* Menu bar.  */
  menu_bar.cbSize = sizeof (menu_bar);
  menu_bar.rcBar.right = menu_bar.rcBar.left = 0;
  menu_bar.rcBar.top = menu_bar.rcBar.bottom = 0;
  GetMenuBarInfo (FRAME_W32_WINDOW (f), 0xFFFFFFFD, 0, &menu_bar);
  single_menu_bar_height = GetSystemMetrics (SM_CYMENU);
  wrapped_menu_bar_height = GetSystemMetrics (SM_CYMENUSIZE);
  unblock_input ();

  left = window.rcWindow.left;
  top = window.rcWindow.top;
  right = window.rcWindow.right;
  bottom = window.rcWindow.bottom;

  /* Menu bar.  */
  menu_bar_height = menu_bar.rcBar.bottom - menu_bar.rcBar.top;
  /* Fix menu bar height reported by GetMenuBarInfo.  */
  if (menu_bar_height > single_menu_bar_height)
    /* A wrapped menu bar.  */
    menu_bar_height += single_menu_bar_height - wrapped_menu_bar_height;
  else if (menu_bar_height > 0)
    /* A single line menu bar.  */
    menu_bar_height = single_menu_bar_height;

  return listn (CONSTYPE_HEAP, 10,
		Fcons (Qouter_position,
		       Fcons (make_number (left), make_number (top))),
		Fcons (Qouter_size,
		       Fcons (make_number (right - left),
			      make_number (bottom - top))),
		Fcons (Qexternal_border_size,
		       Fcons (make_number (external_border_width),
			      make_number (external_border_height))),
		Fcons (Qtitle_bar_size,
		       Fcons (make_number (title_bar_width),
			      make_number (title_bar_height))),
		Fcons (Qmenu_bar_external, Qt),
		Fcons (Qmenu_bar_size,
		       Fcons (make_number
			      (menu_bar.rcBar.right - menu_bar.rcBar.left),
			      make_number (menu_bar_height))),
		Fcons (Qtool_bar_external, Qnil),
		Fcons (Qtool_bar_position, tool_bar_height ? Qtop : Qnil),
		Fcons (Qtool_bar_size,
		       Fcons (make_number
			      (tool_bar_height
			       ? (right - left - 2 * external_border_width
				  - 2 * internal_border_width)
			       : 0),
			      make_number (tool_bar_height))),
		Fcons (Qinternal_border_width,
		       make_number (internal_border_width)));
}

DEFUN ("w32-frame-edges", Fw32_frame_edges, Sw32_frame_edges, 0, 2, 0,
       doc: /* Return edge coordinates of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is a list of the form (LEFT, TOP, RIGHT, BOTTOM).  All values are
in pixels relative to the origin - the position (0, 0) - of FRAME's
display.

If optional argument TYPE is the symbol `outer-edges', return the outer
edges of FRAME.  The outer edges comprise the decorations of the window
manager (like the title bar or external borders) as well as any external
menu or tool bar of FRAME.  If optional argument TYPE is the symbol
`native-edges' or nil, return the native edges of FRAME.  The native
edges exclude the decorations of the window manager and any external
menu or tool bar of FRAME.  If TYPE is the symbol `inner-edges', return
the inner edges of FRAME.  These edges exclude title bar, any borders,
menu bar or tool bar of FRAME.  */)
  (Lisp_Object frame, Lisp_Object type)
{
  struct frame *f = decode_live_frame (frame);

  if (FRAME_INITIAL_P (f) || !FRAME_W32_P (f))
    return Qnil;

  if (EQ (type, Qouter_edges))
    {
      RECT rectangle;
      BOOL success = false;

      block_input ();
      /* Outer frame rectangle, including outer borders and title bar. */
      success = GetWindowRect (FRAME_W32_WINDOW (f), &rectangle);
      unblock_input ();

      if (success)
	return list4 (make_number (rectangle.left),
		      make_number (rectangle.top),
		      make_number (rectangle.right),
		      make_number (rectangle.bottom));
      else
	return Qnil;
    }
  else
    {
      RECT rectangle;
      POINT pt;
      int left, top, right, bottom;
      BOOL success;

      block_input ();
      /* Inner frame rectangle, excluding borders and title bar.  */
      success = GetClientRect (FRAME_W32_WINDOW (f), &rectangle);
      /* Get top-left corner of native rectangle in screen
	 coordinates.  */
      if (!success)
	{
	  unblock_input ();
	  return Qnil;
	}

      pt.x = 0;
      pt.y = 0;
      success = ClientToScreen (FRAME_W32_WINDOW (f), &pt);
      unblock_input ();

      if (!success)
	return Qnil;

      left = pt.x;
      top = pt.y;
      right = left + rectangle.right;
      bottom = top + rectangle.bottom;

      if (EQ (type, Qinner_edges))
	{
	  int internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);

	  return list4 (make_number (left + internal_border_width),
			make_number (top
				     + FRAME_TOOL_BAR_HEIGHT (f)
				     + internal_border_width),
			make_number (right - internal_border_width),
			make_number (bottom - internal_border_width));
	}
      else
	return list4 (make_number (left), make_number (top),
		      make_number (right), make_number (bottom));
    }
}

/**
 * w32_frame_list_z_order:
 *
 * Recursively add list of all frames on the display specified via
 * DPYINFO and whose window-system window's parent is specified by
 * WINDOW to FRAMES and return FRAMES.
 */
static Lisp_Object
w32_frame_list_z_order (struct w32_display_info *dpyinfo, HWND window)
{
  Lisp_Object frame, frames = Qnil;

  while (window)
    {
      struct frame *f = x_window_to_frame (dpyinfo, window);

      if (f)
	{
	  XSETFRAME (frame, f);
	  frames = Fcons (frame, frames);
	}

      block_input ();
      window = GetNextWindow (window, GW_HWNDNEXT);
      unblock_input ();
    }

  return Fnreverse (frames);
}

DEFUN ("w32-frame-list-z-order", Fw32_frame_list_z_order,
       Sw32_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs' frames, in Z (stacking) order.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).  If
omitted or nil, that stands for the selected frame's display.

As a special case, if DISPLAY is non-nil and specifies a live frame,
return the child frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).  */)
  (Lisp_Object display)
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  HWND window;

  block_input ();
  if (FRAMEP (display) && FRAME_LIVE_P (XFRAME (display)))
    window = GetWindow (FRAME_W32_WINDOW (XFRAME (display)), GW_CHILD);
  else
    window = GetTopWindow (NULL);
  unblock_input ();

  return w32_frame_list_z_order (dpyinfo, window);
}

/**
 * w32_frame_restack:
 *
 * Restack frame F1 below frame F2, above if ABOVE_FLAG is non-nil.  In
 * practice this is a two-step action: The first step removes F1's
 * window-system window from the display.  The second step reinserts
 * F1's window below (above if ABOVE_FLAG is true) that of F2.
 */
static void
w32_frame_restack (struct frame *f1, struct frame *f2, bool above_flag)
{
  HWND hwnd1 = FRAME_W32_WINDOW (f1);
  HWND hwnd2 = FRAME_W32_WINDOW (f2);

  block_input ();
  if (above_flag)
    /* Put F1 above F2 in the z-order.  */
    {
      if (GetNextWindow (hwnd1, GW_HWNDNEXT) != hwnd2)
	{
	  /* Make sure F1 is below F2 first because we must not
	     change the relative position of F2 wrt any other
	     window but F1.  */
	  if (GetNextWindow (hwnd2, GW_HWNDNEXT) != hwnd1)
	    SetWindowPos (hwnd1, hwnd2, 0, 0, 0, 0,
			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE
			  | SWP_FRAMECHANGED);
	  /* Now put F1 above F2.  */
	  SetWindowPos (hwnd2, hwnd1, 0, 0, 0, 0,
			SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE
			| SWP_FRAMECHANGED);
	}
    }
  else if (GetNextWindow (hwnd2, GW_HWNDNEXT) != hwnd1)
    /* Put F1 below F2 in the z-order.  */
    SetWindowPos (hwnd1, hwnd2, 0, 0, 0, 0,
		  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE
		  | SWP_FRAMECHANGED);
  unblock_input ();
}

DEFUN ("w32-frame-restack", Fw32_frame_restack, Sw32_frame_restack, 2, 3, 0,
       doc: /* Restack FRAME1 below FRAME2.
This means that if both frames are visible and the display areas of
these frames overlap, FRAME2 (partially) obscures FRAME1.  If optional
third argument ABOVE is non-nil, restack FRAME1 above FRAME2.  This
means that if both frames are visible and the display areas of these
frames overlap, FRAME1 (partially) obscures FRAME2.

This may be thought of as an atomic action performed in two steps: The
first step removes FRAME1's window-system window from the display.  The
second step reinserts FRAME1's window below (above if ABOVE is true)
that of FRAME2.  Hence the position of FRAME2 in its display's Z
\(stacking) order relative to all other frames excluding FRAME1 remains
unaltered.

Some window managers may refuse to restack windows.  */)
     (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object above)
{
  struct frame *f1 = decode_live_frame (frame1);
  struct frame *f2 = decode_live_frame (frame2);

  if (FRAME_W32_P (f1) && FRAME_W32_P (f2))
    {
      w32_frame_restack (f1, f2, !NILP (above));
      return Qt;
    }
  else
    {
      error ("Cannot restack frames");
      return Qnil;
    }
}

DEFUN ("w32-mouse-absolute-pixel-position", Fw32_mouse_absolute_pixel_position,
       Sw32_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the coordinates of
the mouse cursor position in pixels relative to a position (0, 0) of the
selected frame's display.  */)
  (void)
{
  POINT pt;

  block_input ();
  GetCursorPos (&pt);
  unblock_input ();

  return Fcons (make_number (pt.x), make_number (pt.y));
}

DEFUN ("w32-set-mouse-absolute-pixel-position", Fw32_set_mouse_absolute_pixel_position,
       Sw32_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to absolute pixel position (X, Y).
The coordinates X and Y are interpreted in pixels relative to a position
\(0, 0) of the selected frame's display.  */)
  (Lisp_Object x, Lisp_Object y)
{
  UINT trail_num = 0;
  BOOL ret = false;

  CHECK_TYPE_RANGED_INTEGER (int, x);
  CHECK_TYPE_RANGED_INTEGER (int, y);

  block_input ();
  /* When "mouse trails" are in effect, moving the mouse cursor
     sometimes leaves behind an annoying "ghost" of the pointer.
     Avoid that by momentarily switching off mouse trails.  */
  if (os_subtype == OS_NT
      && w32_major_version + w32_minor_version >= 6)
    ret = SystemParametersInfo (SPI_GETMOUSETRAILS, 0, &trail_num, 0);
  SetCursorPos (XINT (x), XINT (y));
  if (ret)
    SystemParametersInfo (SPI_SETMOUSETRAILS, trail_num, NULL, 0);
  unblock_input ();

  return Qnil;
}


#ifdef WINDOWSNT
typedef BOOL (WINAPI *GetDiskFreeSpaceExW_Proc)
  (LPCWSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);
typedef BOOL (WINAPI *GetDiskFreeSpaceExA_Proc)
  (LPCSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);

DEFUN ("file-system-info", Ffile_system_info, Sfile_system_info, 1, 1, 0,
       doc: /* SKIP: Real doc in fileio.c.  */)
  (Lisp_Object filename)
{
  Lisp_Object encoded, value;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  encoded = ENCODE_FILE (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (encoded, Qfile_system_info);
  if (!NILP (handler))
    {
      value = call2 (handler, Qfile_system_info, encoded);
      if (CONSP (value) || NILP (value))
	return value;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  value = Qnil;

  /* Determining the required information on Windows turns out, sadly,
     to be more involved than one would hope.  The original Windows API
     call for this will return bogus information on some systems, but we
     must dynamically probe for the replacement api, since that was
     added rather late on.  */
  {
    HMODULE hKernel = GetModuleHandle ("kernel32");
    GetDiskFreeSpaceExW_Proc pfn_GetDiskFreeSpaceExW =
      (GetDiskFreeSpaceExW_Proc) GetProcAddress (hKernel, "GetDiskFreeSpaceExW");
    GetDiskFreeSpaceExA_Proc pfn_GetDiskFreeSpaceExA =
      (GetDiskFreeSpaceExA_Proc) GetProcAddress (hKernel, "GetDiskFreeSpaceExA");
    bool have_pfn_GetDiskFreeSpaceEx =
      ((w32_unicode_filenames && pfn_GetDiskFreeSpaceExW)
       || (!w32_unicode_filenames && pfn_GetDiskFreeSpaceExA));

    /* On Windows, we may need to specify the root directory of the
       volume holding FILENAME.  */
    char rootname[MAX_UTF8_PATH];
    wchar_t rootname_w[MAX_PATH];
    char rootname_a[MAX_PATH];
    char *name = SSDATA (encoded);
    BOOL result;

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

    if (w32_unicode_filenames)
      filename_to_utf16 (rootname, rootname_w);
    else
      filename_to_ansi (rootname, rootname_a);

    if (have_pfn_GetDiskFreeSpaceEx)
      {
	/* Unsigned large integers cannot be cast to double, so
	   use signed ones instead.  */
	LARGE_INTEGER availbytes;
	LARGE_INTEGER freebytes;
	LARGE_INTEGER totalbytes;

	if (w32_unicode_filenames)
	  result = pfn_GetDiskFreeSpaceExW (rootname_w,
					    (ULARGE_INTEGER *)&availbytes,
					    (ULARGE_INTEGER *)&totalbytes,
					    (ULARGE_INTEGER *)&freebytes);
	else
	  result = pfn_GetDiskFreeSpaceExA (rootname_a,
					    (ULARGE_INTEGER *)&availbytes,
					    (ULARGE_INTEGER *)&totalbytes,
					    (ULARGE_INTEGER *)&freebytes);
	if (result)
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

	if (w32_unicode_filenames)
	  result = GetDiskFreeSpaceW (rootname_w,
				      &sectors_per_cluster,
				      &bytes_per_sector,
				      &free_clusters,
				      &total_clusters);
	else
	  result = GetDiskFreeSpaceA (rootname_a,
				      &sectors_per_cluster,
				      &bytes_per_sector,
				      &free_clusters,
				      &total_clusters);
	if (result)
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
#endif /* WINDOWSNT */


#ifdef WINDOWSNT
DEFUN ("default-printer-name", Fdefault_printer_name, Sdefault_printer_name,
       0, 0, 0, doc: /* Return the name of Windows default printer device.  */)
  (void)
{
  static char pname_buf[256];
  int err;
  HANDLE hPrn;
  PRINTER_INFO_2W *ppi2w = NULL;
  PRINTER_INFO_2A *ppi2a = NULL;
  DWORD dwNeeded = 0, dwReturned = 0;
  char server_name[MAX_UTF8_PATH], share_name[MAX_UTF8_PATH];
  char port_name[MAX_UTF8_PATH];

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
  /* GetPrinterW is not supported by unicows.dll.  */
  if (w32_unicode_filenames && os_subtype != OS_9X)
    GetPrinterW (hPrn, 2, NULL, 0, &dwNeeded);
  else
    GetPrinterA (hPrn, 2, NULL, 0, &dwNeeded);
  if (dwNeeded == 0)
    {
      ClosePrinter (hPrn);
      return Qnil;
    }
  /* Call GetPrinter again with big enough memory block.  */
  if (w32_unicode_filenames && os_subtype != OS_9X)
    {
      /* Allocate memory for the PRINTER_INFO_2 struct.  */
      ppi2w = xmalloc (dwNeeded);
      err = GetPrinterW (hPrn, 2, (LPBYTE)ppi2w, dwNeeded, &dwReturned);
      ClosePrinter (hPrn);
      if (!err)
	{
	  xfree (ppi2w);
	  return Qnil;
	}

      if ((ppi2w->Attributes & PRINTER_ATTRIBUTE_SHARED)
	  && ppi2w->pServerName)
	{
	  filename_from_utf16 (ppi2w->pServerName, server_name);
	  filename_from_utf16 (ppi2w->pShareName, share_name);
	}
      else
	{
	  server_name[0] = '\0';
	  filename_from_utf16 (ppi2w->pPortName, port_name);
	}
    }
  else
    {
      ppi2a = xmalloc (dwNeeded);
      err = GetPrinterA (hPrn, 2, (LPBYTE)ppi2a, dwNeeded, &dwReturned);
      ClosePrinter (hPrn);
      if (!err)
	{
	  xfree (ppi2a);
	  return Qnil;
	}

      if ((ppi2a->Attributes & PRINTER_ATTRIBUTE_SHARED)
	  && ppi2a->pServerName)
	{
	  filename_from_ansi (ppi2a->pServerName, server_name);
	  filename_from_ansi (ppi2a->pShareName, share_name);
	}
      else
	{
	  server_name[0] = '\0';
	  filename_from_ansi (ppi2a->pPortName, port_name);
	}
    }

  if (server_name[0])
    {
      /* a remote printer */
      if (server_name[0] == '\\')
	snprintf (pname_buf, sizeof (pname_buf), "%s\\%s", server_name,
		  share_name);
      else
	snprintf (pname_buf, sizeof (pname_buf), "\\\\%s\\%s", server_name,
		  share_name);
      pname_buf[sizeof (pname_buf) - 1] = '\0';
    }
  else
    {
      /* a local printer */
      strncpy (pname_buf, port_name, sizeof (pname_buf));
      pname_buf[sizeof (pname_buf) - 1] = '\0';
      /* `pPortName' can include several ports, delimited by ','.
       * we only use the first one. */
      strtok (pname_buf, ",");
    }

  return DECODE_FILE (build_unibyte_string (pname_buf));
}
#endif	/* WINDOWSNT */


/* Equivalent of strerror for W32 error codes.  */
char *
w32_strerror (int error_no)
{
  static char buf[500];
  DWORD ret;

  if (error_no == 0)
    error_no = GetLastError ();

  ret = FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM |
		       FORMAT_MESSAGE_IGNORE_INSERTS,
		       NULL,
		       error_no,
		       0, /* choose most suitable language */
		       buf, sizeof (buf), NULL);

  while (ret > 0 && (buf[ret - 1] == '\n' ||
		     buf[ret - 1] == '\r' ))
      --ret;
  buf[ret] = '\0';
  if (!ret)
    sprintf (buf, "w32 error %d", error_no);

  return buf;
}

/* For convenience when debugging.  (You cannot call GetLastError
   directly from GDB: it will crash, because it uses the __stdcall
   calling convention, not the _cdecl convention assumed by GDB.)  */
DWORD w32_last_error (void);

DWORD
w32_last_error (void)
{
  return GetLastError ();
}

/* Cache information describing the NT system for later use.  */
void
cache_system_info (void)
{
  union
    {
      struct info
	{
	  char  major;
	  char  minor;
	  short platform;
	} info;
      DWORD data;
    } version;

  /* Cache the module handle of Emacs itself.  */
  hinst = GetModuleHandle (NULL);

  /* Cache the version of the operating system.  */
  version.data = GetVersion ();
  w32_major_version = version.info.major;
  w32_minor_version = version.info.minor;

  if (version.info.platform & 0x8000)
    os_subtype = OS_9X;
  else
    os_subtype = OS_NT;

  /* Cache page size, allocation unit, processor type, etc.  */
  GetSystemInfo (&sysinfo_cache);
  syspage_mask = (DWORD_PTR)sysinfo_cache.dwPageSize - 1;

  /* Cache os info.  */
  osinfo_cache.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
  GetVersionEx (&osinfo_cache);

  w32_build_number = osinfo_cache.dwBuildNumber;
  if (os_subtype == OS_9X)
    w32_build_number &= 0xffff;

  w32_num_mouse_buttons = GetSystemMetrics (SM_CMOUSEBUTTONS);
}

#ifdef EMACSDEBUG
void
_DebPrint (const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);
#if CYGWIN
  fprintf (stderr, "%s", buf);
#endif
  OutputDebugString (buf);
}
#endif

int
w32_console_toggle_lock_key (int vk_code, Lisp_Object new_state)
{
  int cur_state = (GetKeyState (vk_code) & 1);

  if (NILP (new_state)
      || (NUMBERP (new_state)
	  && ((XUINT (new_state)) & 1) != cur_state))
    {
#ifdef WINDOWSNT
      faked_key = vk_code;
#endif /* WINDOWSNT */

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

  return cur_state;
}

/* Translate console modifiers to emacs modifiers.
   German keyboard support (Kai Morgan Zeise 2/18/95).  */
int
w32_kbd_mods_to_emacs (DWORD mods, WORD key)
{
  int retval = 0;

  /* If we recognize right-alt and left-ctrl as AltGr, and it has been
     pressed, first remove those modifiers.  */
  if (!NILP (Vw32_recognize_altgr)
      && (mods & (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
      == (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
    mods &= ~ (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED);

  if (mods & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED))
    retval = ((NILP (Vw32_alt_is_meta)) ? alt_modifier : meta_modifier);

  if (mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
    {
      retval |= ctrl_modifier;
      if ((mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	  == (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	retval |= meta_modifier;
    }

  if (mods & LEFT_WIN_PRESSED)
    retval |= w32_key_to_modifier (VK_LWIN);
  if (mods & RIGHT_WIN_PRESSED)
    retval |= w32_key_to_modifier (VK_RWIN);
  if (mods & APPS_PRESSED)
    retval |= w32_key_to_modifier (VK_APPS);
  if (mods & SCROLLLOCK_ON)
    retval |= w32_key_to_modifier (VK_SCROLL);

  /* Just in case someone wanted the original behavior, make it
     optional by setting w32-capslock-is-shiftlock to t.  */
  if (NILP (Vw32_capslock_is_shiftlock)
      /* Keys that should _not_ be affected by CapsLock.  */
      && (    (key == VK_BACK)
	   || (key == VK_TAB)
	   || (key == VK_CLEAR)
	   || (key == VK_RETURN)
	   || (key == VK_ESCAPE)
	   || ((key >= VK_SPACE) && (key <= VK_HELP))
	   || ((key >= VK_NUMPAD0) && (key <= VK_F24))
	   || ((key >= VK_NUMPAD_CLEAR) && (key <= VK_NUMPAD_DELETE))
	 ))
    {
      /* Only consider shift state.  */
      if ((mods & SHIFT_PRESSED) != 0)
	retval |= shift_modifier;
    }
  else
    {
      /* Ignore CapsLock state if not enabled.  */
      if (NILP (Vw32_enable_caps_lock))
	mods &= ~CAPSLOCK_ON;
      if ((mods & (SHIFT_PRESSED | CAPSLOCK_ON)) != 0)
	retval |= shift_modifier;
    }

  return retval;
}

/* The return code indicates key code size.  cpID is the codepage to
   use for translation to Unicode; -1 means use the current console
   input codepage.  */
int
w32_kbd_patch_key (KEY_EVENT_RECORD *event, int cpId)
{
  unsigned int key_code = event->wVirtualKeyCode;
  unsigned int mods = event->dwControlKeyState;
  BYTE keystate[256];
  static BYTE ansi_code[4];
  static int isdead = 0;

  if (isdead == 2)
    {
      event->uChar.AsciiChar = ansi_code[2];
      isdead = 0;
      return 1;
    }
  if (event->uChar.AsciiChar != 0)
    return 1;

  memset (keystate, 0, sizeof (keystate));
  keystate[key_code] = 0x80;
  if (mods & SHIFT_PRESSED)
    keystate[VK_SHIFT] = 0x80;
  if (mods & CAPSLOCK_ON)
    keystate[VK_CAPITAL] = 1;
  /* If we recognize right-alt and left-ctrl as AltGr, set the key
     states accordingly before invoking ToAscii.  */
  if (!NILP (Vw32_recognize_altgr)
      && (mods & LEFT_CTRL_PRESSED) && (mods & RIGHT_ALT_PRESSED))
    {
      keystate[VK_CONTROL] = 0x80;
      keystate[VK_LCONTROL] = 0x80;
      keystate[VK_MENU] = 0x80;
      keystate[VK_RMENU] = 0x80;
    }

#if 0
  /* Because of an OS bug, ToAscii corrupts the stack when called to
     convert a dead key in console mode on NT4.  Unfortunately, trying
     to check for dead keys using MapVirtualKey doesn't work either -
     these functions apparently use internal information about keyboard
     layout which doesn't get properly updated in console programs when
     changing layout (though apparently it gets partly updated,
     otherwise ToAscii wouldn't crash).  */
  if (is_dead_key (event->wVirtualKeyCode))
    return 0;
#endif

  /* On NT, call ToUnicode instead and then convert to the current
     console input codepage.  */
  if (os_subtype == OS_NT)
    {
      WCHAR buf[128];

      isdead = ToUnicode (event->wVirtualKeyCode, event->wVirtualScanCode,
			  keystate, buf, 128, 0);
      if (isdead > 0)
	{
	  /* When we are called from the GUI message processing code,
	     we are passed the current keyboard codepage, a positive
	     number, to use below.  */
	  if (cpId == -1)
	    cpId = GetConsoleCP ();

	  event->uChar.UnicodeChar = buf[isdead - 1];
	  isdead = WideCharToMultiByte (cpId, 0, buf, isdead,
					(LPSTR)ansi_code, 4, NULL, NULL);
	}
      else
	isdead = 0;
    }
  else
    {
      isdead = ToAscii (event->wVirtualKeyCode, event->wVirtualScanCode,
			keystate, (LPWORD) ansi_code, 0);
    }

  if (isdead == 0)
    return 0;
  event->uChar.AsciiChar = ansi_code[0];
  return isdead;
}


void
w32_sys_ring_bell (struct frame *f)
{
  if (sound_type == 0xFFFFFFFF)
    {
      Beep (666, 100);
    }
  else if (sound_type == MB_EMACS_SILENT)
    {
      /* Do nothing.  */
    }
  else
    MessageBeep (sound_type);
}

DEFUN ("w32--menu-bar-in-use", Fw32__menu_bar_in_use, Sw32__menu_bar_in_use,
       0, 0, 0,
       doc: /* Return non-nil when a menu-bar menu is being used.
Internal use only.  */)
  (void)
{
  return menubar_in_use ? Qt : Qnil;
}

#if defined WINDOWSNT && !defined HAVE_DBUS

/***********************************************************************
			  Tray notifications
 ***********************************************************************/
/* A private struct declaration to avoid compile-time limits.  */
typedef struct MY_NOTIFYICONDATAW {
  DWORD cbSize;
  HWND hWnd;
  UINT uID;
  UINT uFlags;
  UINT uCallbackMessage;
  HICON hIcon;
  WCHAR szTip[128];
  DWORD dwState;
  DWORD dwStateMask;
  WCHAR szInfo[256];
  _ANONYMOUS_UNION union {
    UINT uTimeout;
    UINT uVersion;
  } DUMMYUNIONNAME;
  WCHAR szInfoTitle[64];
  DWORD dwInfoFlags;
  GUID guidItem;
  HICON hBalloonIcon;
} MY_NOTIFYICONDATAW;

#define MYNOTIFYICONDATAW_V1_SIZE offsetof (MY_NOTIFYICONDATAW, szTip[64])
#define MYNOTIFYICONDATAW_V2_SIZE offsetof (MY_NOTIFYICONDATAW, guidItem)
#define MYNOTIFYICONDATAW_V3_SIZE offsetof (MY_NOTIFYICONDATAW, hBalloonIcon)
#ifndef NIF_INFO
# define NIF_INFO     0x00000010
#endif
#ifndef NIIF_NONE
# define NIIF_NONE    0x00000000
#endif
#ifndef NIIF_INFO
# define NIIF_INFO    0x00000001
#endif
#ifndef NIIF_WARNING
# define NIIF_WARNING 0x00000002
#endif
#ifndef NIIF_ERROR
# define NIIF_ERROR   0x00000003
#endif


#define EMACS_TRAY_NOTIFICATION_ID  42	/* arbitrary */
#define EMACS_NOTIFICATION_MSG      (WM_APP + 1)

enum NI_Severity {
  Ni_None,
  Ni_Info,
  Ni_Warn,
  Ni_Err
};

/* Report the version of a DLL given by its name.  The return value is
   constructed using MAKEDLLVERULL.  */
static ULONGLONG
get_dll_version (const char *dll_name)
{
  ULONGLONG version = 0;
  HINSTANCE hdll = LoadLibrary (dll_name);

  if (hdll)
    {
      DLLGETVERSIONPROC pDllGetVersion
	= (DLLGETVERSIONPROC) GetProcAddress (hdll, "DllGetVersion");

      if (pDllGetVersion)
	{
	  DLLVERSIONINFO dvi;
	  HRESULT result;

	  memset (&dvi, 0, sizeof(dvi));
	  dvi.cbSize = sizeof(dvi);
	  result = pDllGetVersion (&dvi);
	  if (SUCCEEDED (result))
	    version = MAKEDLLVERULL (dvi.dwMajorVersion, dvi.dwMinorVersion,
				     0, 0);
	}
      FreeLibrary (hdll);
    }

  return version;
}

/* Return the number of bytes in UTF-8 encoded string STR that
   corresponds to at most LIM characters.  If STR ends before LIM
   characters, return the number of bytes in STR including the
   terminating null byte.  */
static int
utf8_mbslen_lim (const char *str, int lim)
{
  const char *p = str;
  int mblen = 0, nchars = 0;

  while (*p && nchars < lim)
    {
      int nbytes = CHAR_BYTES (*p);

      mblen += nbytes;
      nchars++;
      p += nbytes;
    }

  if (!*p && nchars < lim)
    mblen++;

  return mblen;
}

/* Low-level subroutine to show tray notifications.  All strings are
   supposed to be unibyte UTF-8 encoded by the caller.  */
static EMACS_INT
add_tray_notification (struct frame *f, const char *icon, const char *tip,
		       enum NI_Severity severity, unsigned timeout,
		       const char *title, const char *msg)
{
  EMACS_INT retval = EMACS_TRAY_NOTIFICATION_ID;

  if (FRAME_W32_P (f))
    {
      MY_NOTIFYICONDATAW nidw;
      ULONGLONG shell_dll_version = get_dll_version ("Shell32.dll");
      wchar_t tipw[128], msgw[256], titlew[64];
      int tiplen;

      memset (&nidw, 0, sizeof(nidw));

      /* MSDN says the full struct is supported since Vista, whose
	 Shell32.dll version is said to be 6.0.6.  But DllGetVersion
	 cannot report the 3rd field value, it reports "build number"
	 instead, which is something else.  So we use the Windows 7's
	 version 6.1 as cutoff, and Vista loses.  (Actually, the loss
	 is not a real one, since we don't expose the hBalloonIcon
	 member of the struct to Lisp.)  */
      if (shell_dll_version >= MAKEDLLVERULL (6, 1, 0, 0)) /* >= Windows 7 */
	nidw.cbSize = sizeof (nidw);
      else if (shell_dll_version >= MAKEDLLVERULL (6, 0, 0, 0)) /* XP */
	nidw.cbSize = MYNOTIFYICONDATAW_V3_SIZE;
      else if (shell_dll_version >= MAKEDLLVERULL (5, 0, 0, 0)) /* W2K */
	nidw.cbSize = MYNOTIFYICONDATAW_V2_SIZE;
      else
	nidw.cbSize = MYNOTIFYICONDATAW_V1_SIZE;		/* < W2K */
      nidw.hWnd = FRAME_W32_WINDOW (f);
      nidw.uID = EMACS_TRAY_NOTIFICATION_ID;
      nidw.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_INFO;
      nidw.uCallbackMessage = EMACS_NOTIFICATION_MSG;
      if (!*icon)
	nidw.hIcon = LoadIcon (hinst, EMACS_CLASS);
      else
	{
	  if (w32_unicode_filenames)
	    {
	      wchar_t icon_w[MAX_PATH];

	      if (filename_to_utf16 (icon, icon_w) != 0)
		{
		  errno = ENOENT;
		  return -1;
		}
	      nidw.hIcon = LoadImageW (NULL, icon_w, IMAGE_ICON, 0, 0,
				       LR_DEFAULTSIZE | LR_LOADFROMFILE);
	    }
	  else
	    {
	      char icon_a[MAX_PATH];

	      if (filename_to_ansi (icon, icon_a) != 0)
		{
		  errno = ENOENT;
		  return -1;
		}
	      nidw.hIcon = LoadImageA (NULL, icon_a, IMAGE_ICON, 0, 0,
				       LR_DEFAULTSIZE | LR_LOADFROMFILE);
	    }
	}
      if (!nidw.hIcon)
	{
	  switch (GetLastError ())
	    {
	    case ERROR_FILE_NOT_FOUND:
	      errno = ENOENT;
	      break;
	    default:
	      errno = ENOMEM;
	      break;
	    }
	  return -1;
	}

      /* Windows 9X and NT4 support only 64 characters in the Tip,
	 later versions support up to 128.  */
      if (nidw.cbSize == MYNOTIFYICONDATAW_V1_SIZE)
	{
	  tiplen = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
					 tip, utf8_mbslen_lim (tip, 63),
					 tipw, 64);
	  if (tiplen >= 63)
	    tipw[63] = 0;
	}
      else
	{
	  tiplen = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
					 tip, utf8_mbslen_lim (tip, 127),
					 tipw, 128);
	  if (tiplen >= 127)
	    tipw[127] = 0;
	}
      if (tiplen == 0)
	{
	  errno = EINVAL;
	  retval = -1;
	  goto done;
	}
      wcscpy (nidw.szTip, tipw);

      /* The rest of the structure is only supported since Windows 2000.  */
      if (nidw.cbSize > MYNOTIFYICONDATAW_V1_SIZE)
	{
	  int slen;

	  slen = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
					     msg, utf8_mbslen_lim (msg, 255),
					     msgw, 256);
	  if (slen >= 255)
	    msgw[255] = 0;
	  else if (slen == 0)
	    {
	      errno = EINVAL;
	      retval = -1;
	      goto done;
	    }
	  wcscpy (nidw.szInfo, msgw);
	  nidw.uTimeout = timeout;
	  slen = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
				       title, utf8_mbslen_lim (title, 63),
				       titlew, 64);
	  if (slen >= 63)
	    titlew[63] = 0;
	  else if (slen == 0)
	    {
	      errno = EINVAL;
	      retval = -1;
	      goto done;
	    }
	  wcscpy (nidw.szInfoTitle, titlew);

	  switch (severity)
	    {
	    case Ni_None:
	      nidw.dwInfoFlags = NIIF_NONE;
	      break;
	    case Ni_Info:
	    default:
	      nidw.dwInfoFlags = NIIF_INFO;
	      break;
	    case Ni_Warn:
	      nidw.dwInfoFlags = NIIF_WARNING;
	      break;
	    case Ni_Err:
	      nidw.dwInfoFlags = NIIF_ERROR;
	      break;
	    }
	}

      if (!Shell_NotifyIconW (NIM_ADD, (PNOTIFYICONDATAW)&nidw))
	{
	  /* GetLastError returns meaningless results when
	     Shell_NotifyIcon fails.  */
	  DebPrint (("Shell_NotifyIcon ADD failed (err=%d)\n",
		     GetLastError ()));
	  errno = EINVAL;
	  retval = -1;
	}
    done:
      if (*icon && !DestroyIcon (nidw.hIcon))
	DebPrint (("DestroyIcon failed (err=%d)\n", GetLastError ()));
    }
  return retval;
}

/* Low-level subroutine to remove a tray notification.  Note: we only
   pass the minimum data about the notification: its ID and the handle
   of the window to which it sends messages.  MSDN doesn't say this is
   enough, but it works in practice.  This allows us to avoid keeping
   the notification data around after we show the notification.  */
static void
delete_tray_notification (struct frame *f, int id)
{
  if (FRAME_W32_P (f))
    {
      MY_NOTIFYICONDATAW nidw;

      memset (&nidw, 0, sizeof(nidw));
      nidw.hWnd = FRAME_W32_WINDOW (f);
      nidw.uID = id;

      if (!Shell_NotifyIconW (NIM_DELETE, (PNOTIFYICONDATAW)&nidw))
	{
	  /* GetLastError returns meaningless results when
	     Shell_NotifyIcon fails.  */
	  DebPrint (("Shell_NotifyIcon DELETE failed\n"));
	  errno = EINVAL;
	  return;
	}
    }
  return;
}

DEFUN ("w32-notification-notify",
       Fw32_notification_notify, Sw32_notification_notify,
       0, MANY, 0,
       doc: /* Display an MS-Windows tray notification as specified by PARAMS.

Value is the integer unique ID of the notification that can be used
to remove the notification using `w32-notification-close', which see.
If the function fails, the return value is nil.

Tray notifications, a.k.a. \"taskbar messages\", are messages that
inform the user about events unrelated to the current user activity,
such as a significant system event, by briefly displaying informative
text in a balloon from an icon in the notification area of the taskbar.

Parameters in PARAMS are specified as keyword/value pairs.  All the
parameters are optional, but if no parameters are specified, the
function will do nothing and return nil.

The following parameters are supported:

:icon ICON       -- Display ICON in the system tray.  If ICON is a string,
                    it should specify a file name from which to load the
                    icon; the specified file should be a .ico Windows icon
                    file.  If ICON is not a string, or if this parameter
                    is not specified, the standard Emacs icon will be used.

:tip TIP         -- Use TIP as the tooltip for the notification.  If TIP
                    is a string, this is the text of a tooltip that will
                    be shown when the mouse pointer hovers over the tray
                    icon added by the notification.  If TIP is not a
                    string, or if this parameter is not specified, the
                    default tooltip text is \"Emacs notification\".  The
                    tooltip text can be up to 127 characters long (63
                    on Windows versions before W2K).  Longer strings
                    will be truncated.

:level LEVEL     -- Notification severity level, one of `info',
                    `warning', or `error'.  If given, the value
                    determines the icon displayed to the left of the
                    notification title, but only if the `:title'
                    parameter (see below) is also specified and is a
                    string.

:title TITLE     -- The title of the notification.  If TITLE is a string,
                    it is displayed in a larger font immediately above
                    the body text.  The title text can be up to 63
                    characters long; longer text will be truncated.

:body BODY       -- The body of the notification.  If BODY is a string,
                    it specifies the text of the notification message.
                    Use embedded newlines to control how the text is
                    broken into lines.  The body text can be up to 255
                    characters long, and will be truncated if it's longer.

Note that versions of Windows before W2K support only `:icon' and `:tip'.
You can pass the other parameters, but they will be ignored on those
old systems.

There can be at most one active notification at any given time.  An
active notification must be removed by calling `w32-notification-close'
before a new one can be shown.

usage: (w32-notification-notify &rest PARAMS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object arg_plist, lres;
  EMACS_INT retval;
  char *icon, *tip, *title, *msg;
  enum NI_Severity severity;
  unsigned timeout = 0;

  if (nargs == 0)
    return Qnil;

  arg_plist = Flist (nargs, args);

  /* Icon.  */
  lres = Fplist_get (arg_plist, QCicon);
  if (STRINGP (lres))
    icon = SSDATA (ENCODE_FILE (Fexpand_file_name (lres, Qnil)));
  else
    icon = (char *)"";

  /* Tip.  */
  lres = Fplist_get (arg_plist, QCtip);
  if (STRINGP (lres))
    tip = SSDATA (code_convert_string_norecord (lres, Qutf_8, 1));
  else
    tip = (char *)"Emacs notification";

  /* Severity.  */
  lres = Fplist_get (arg_plist, QClevel);
  if (NILP (lres))
    severity = Ni_None;
  else if (EQ (lres, Qinfo))
    severity = Ni_Info;
  else if (EQ (lres, Qwarning))
    severity = Ni_Warn;
  else if (EQ (lres, Qerror))
    severity = Ni_Err;
  else
    severity = Ni_Info;

  /* Title.  */
  lres = Fplist_get (arg_plist, QCtitle);
  if (STRINGP (lres))
    title = SSDATA (code_convert_string_norecord (lres, Qutf_8, 1));
  else
    title = (char *)"";

  /* Notification body text.  */
  lres = Fplist_get (arg_plist, QCbody);
  if (STRINGP (lres))
    msg = SSDATA (code_convert_string_norecord (lres, Qutf_8, 1));
  else
    msg = (char *)"";

  /* Do it!  */
  retval = add_tray_notification (f, icon, tip, severity, timeout, title, msg);
  return (retval < 0 ? Qnil : make_number (retval));
}

DEFUN ("w32-notification-close",
       Fw32_notification_close, Sw32_notification_close,
       1, 1, 0,
       doc: /* Remove the MS-Windows tray notification specified by its ID.  */)
  (Lisp_Object id)
{
  struct frame *f = SELECTED_FRAME ();

  if (INTEGERP (id))
    delete_tray_notification (f, XINT (id));

  return Qnil;
}

#endif	/* WINDOWSNT && !HAVE_DBUS */


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
  x_set_right_divider_width,
  x_set_bottom_divider_width,
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  x_set_scroll_bar_width,
  x_set_scroll_bar_height,
  x_set_title,
  x_set_unsplittable,
  x_set_vertical_scroll_bars,
  x_set_horizontal_scroll_bars,
  x_set_visibility,
  x_set_tool_bar_lines,
  0, /* x_set_scroll_bar_foreground, */
  0, /* x_set_scroll_bar_background, */
  x_set_screen_gamma,
  x_set_line_spacing,
  x_set_left_fringe,
  x_set_right_fringe,
  0, /* x_set_wait_for_wm, */
  x_set_fullscreen,
  x_set_font_backend,
  x_set_alpha,
  0, /* x_set_sticky */
  0, /* x_set_tool_bar_position */
  0, /* x_set_inhibit_double_buffering */
  x_set_undecorated,
  x_set_parent_frame,
  x_set_skip_taskbar,
  x_set_no_focus_on_map,
  x_set_no_accept_focus,
  x_set_z_group,
  0, /* x_set_override_redirect */
  x_set_no_special_glyphs,
};

void
syms_of_w32fns (void)
{
  globals_of_w32fns ();
  track_mouse_window = NULL;

  w32_visible_system_caret_hwnd = NULL;

  DEFSYM (Qundefined_color, "undefined-color");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qctrl, "ctrl");
  DEFSYM (Qcontrol, "control");
  DEFSYM (Qshift, "shift");
  DEFSYM (Qfont_parameter, "font-parameter");
  DEFSYM (Qgeometry, "geometry");
  DEFSYM (Qworkarea, "workarea");
  DEFSYM (Qmm_size, "mm-size");
  DEFSYM (Qframes, "frames");
  DEFSYM (Qtip_frame, "tip-frame");
  DEFSYM (Qassq_delete_all, "assq-delete-all");
  DEFSYM (Qunicode_sip, "unicode-sip");
#if defined WINDOWSNT && !defined HAVE_DBUS
  DEFSYM (QCicon, ":icon");
  DEFSYM (QCtip, ":tip");
  DEFSYM (QClevel, ":level");
  DEFSYM (Qinfo, "info");
  DEFSYM (Qwarning, "warning");
  DEFSYM (QCtitle, ":title");
  DEFSYM (QCbody, ":body");
#endif

  /* Symbols used elsewhere, but only in MS-Windows-specific code.  */
  DEFSYM (Qgnutls, "gnutls");
  DEFSYM (Qlibxml2, "libxml2");
  DEFSYM (Qserif, "serif");
  DEFSYM (Qzlib, "zlib");
  DEFSYM (Qlcms2, "lcms2");
  DEFSYM (Qjson, "json");

  Fput (Qundefined_color, Qerror_conditions,
	listn (CONSTYPE_PURE, 2, Qundefined_color, Qerror));
  Fput (Qundefined_color, Qerror_message,
	build_pure_c_string ("Undefined color"));

  staticpro (&w32_grabbed_keys);
  w32_grabbed_keys = Qnil;

  DEFVAR_LISP ("w32-color-map", Vw32_color_map,
	       doc: /* An array of color name mappings for Windows.  */);
  Vw32_color_map = Qnil;

  DEFVAR_LISP ("w32-pass-alt-to-system", Vw32_pass_alt_to_system,
	       doc: /* Non-nil if Alt key presses are passed on to Windows.
When non-nil, for example, Alt pressed and released and then space will
open the System menu.  When nil, Emacs processes the Alt key events, and
then silently swallows them.  */);
  Vw32_pass_alt_to_system = Qnil;

  DEFVAR_LISP ("w32-alt-is-meta", Vw32_alt_is_meta,
	       doc: /* Non-nil if the Alt key is to be considered the same as the META key.
When nil, Emacs will translate the Alt key to the ALT modifier, not to META.  */);
  Vw32_alt_is_meta = Qt;

  DEFVAR_INT ("w32-quit-key", w32_quit_key,
	       doc: /* If non-zero, the virtual key code for an alternative quit key.  */);
  w32_quit_key = 0;

  DEFVAR_LISP ("w32-pass-lwindow-to-system",
	       Vw32_pass_lwindow_to_system,
	       doc: /* If non-nil, the left \"Windows\" key is passed on to Windows.

When non-nil, the Start menu is opened by tapping the key.
If you set this to nil, the left \"Windows\" key is processed by Emacs
according to the value of `w32-lwindow-modifier', which see.

Note that some combinations of the left \"Windows\" key with other
keys are caught by Windows at low level.  For example, <lwindow>-r
pops up the Windows Run dialog, <lwindow>-<Pause> pops up the "System
Properties" dialog, etc.  On Windows 10, no \"Windows\" key
combinations are normally handed to applications.  To enable Emacs to
process \"Windows\" key combinations, use the function
`w32-register-hot-key`.

For Windows 98/ME, see the doc string of `w32-phantom-key-code'.  */);
  Vw32_pass_lwindow_to_system = Qt;

  DEFVAR_LISP ("w32-pass-rwindow-to-system",
	       Vw32_pass_rwindow_to_system,
	       doc: /* If non-nil, the right \"Windows\" key is passed on to Windows.

When non-nil, the Start menu is opened by tapping the key.
If you set this to nil, the right \"Windows\" key is processed by Emacs
according to the value of `w32-rwindow-modifier', which see.

Note that some combinations of the right \"Windows\" key with other
keys are caught by Windows at low level.  For example, <rwindow>-r
pops up the Windows Run dialog, <rwindow>-<Pause> pops up the "System
Properties" dialog, etc.  On Windows 10, no \"Windows\" key
combinations are normally handed to applications.  To enable Emacs to
process \"Windows\" key combinations, use the function
`w32-register-hot-key`.

For Windows 98/ME, see the doc string of `w32-phantom-key-code'.  */);
  Vw32_pass_rwindow_to_system = Qt;

  DEFVAR_LISP ("w32-phantom-key-code",
	       Vw32_phantom_key_code,
	       doc: /* Virtual key code used to generate \"phantom\" key presses.
Value is a number between 0 and 255.

Phantom key presses are generated in order to stop the system from
acting on \"Windows\" key events when `w32-pass-lwindow-to-system' or
`w32-pass-rwindow-to-system' is nil.

This variable is only used on Windows 98 and ME.  For other Windows
versions, see the documentation of the `w32-register-hot-key`
function.  */);
  /* Although 255 is technically not a valid key code, it works and
     means that this hack won't interfere with any real key code.  */
  XSETINT (Vw32_phantom_key_code, 255);

  DEFVAR_LISP ("w32-enable-num-lock",
	       Vw32_enable_num_lock,
	       doc: /* If non-nil, the Num Lock key acts normally.
Set to nil to handle Num Lock as the `kp-numlock' key.  */);
  Vw32_enable_num_lock = Qt;

  DEFVAR_LISP ("w32-enable-caps-lock",
	       Vw32_enable_caps_lock,
	       doc: /* If non-nil, the Caps Lock key acts normally.
Set to nil to handle Caps Lock as the `capslock' key.  */);
  Vw32_enable_caps_lock = Qt;

  DEFVAR_LISP ("w32-scroll-lock-modifier",
	       Vw32_scroll_lock_modifier,
	       doc: /* Modifier to use for the Scroll Lock ON state.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to handle Scroll Lock as the `scroll' key.
Any other value will cause the Scroll Lock key to be ignored by Emacs,
and it will have the same effect as in other applications.  */);
  Vw32_scroll_lock_modifier = Qnil;

  DEFVAR_LISP ("w32-lwindow-modifier",
	       Vw32_lwindow_modifier,
	       doc: /* Modifier to use for the left \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `lwindow' key.
Any other value will cause the key to be ignored.

Also see the documentation of the `w32-register-hot-key` function.  */);
  Vw32_lwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-rwindow-modifier",
	       Vw32_rwindow_modifier,
	       doc: /* Modifier to use for the right \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `rwindow' key.
Any other value will cause the key to be ignored.

Also see the documentation of the `w32-register-hot-key` function.  */);
  Vw32_rwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-apps-modifier",
	       Vw32_apps_modifier,
	       doc: /* Modifier to use for the \"Apps\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the `apps' key.
Any other value will cause the key to be ignored.  */);
  Vw32_apps_modifier = Qnil;

  DEFVAR_BOOL ("w32-enable-synthesized-fonts", w32_enable_synthesized_fonts,
	       doc: /* Non-nil enables selection of artificially italicized and bold fonts.  */);
  w32_enable_synthesized_fonts = 0;

  DEFVAR_LISP ("w32-enable-palette", Vw32_enable_palette,
	       doc: /* Non-nil enables Windows palette management to map colors exactly.  */);
  Vw32_enable_palette = Qt;

  DEFVAR_INT ("w32-mouse-button-tolerance",
	      w32_mouse_button_tolerance,
	      doc: /* Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left and right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.  */);
  w32_mouse_button_tolerance = GetDoubleClickTime () / 2;

  DEFVAR_INT ("w32-mouse-move-interval",
	      w32_mouse_move_interval,
	      doc: /* Minimum interval between mouse move events.
The value is the minimum time in milliseconds that must elapse between
successive mouse move (or scroll bar drag) events before they are
reported as lisp events.  */);
  w32_mouse_move_interval = 0;

  DEFVAR_BOOL ("w32-pass-extra-mouse-buttons-to-system",
	       w32_pass_extra_mouse_buttons_to_system,
	       doc: /* If non-nil, the fourth and fifth mouse buttons are passed to Windows.
Recent versions of Windows support mice with up to five buttons.
Since most applications don't support these extra buttons, most mouse
drivers will allow you to map them to functions at the system level.
If this variable is non-nil, Emacs will pass them on, allowing the
system to handle them.  */);
  w32_pass_extra_mouse_buttons_to_system = 0;

  DEFVAR_BOOL ("w32-pass-multimedia-buttons-to-system",
	       w32_pass_multimedia_buttons_to_system,
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

#if 0 /* TODO: Mouse cursor customization.  */
  DEFVAR_LISP ("x-pointer-shape", Vx_pointer_shape,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_pointer_shape = Qnil;

  Vx_nontext_pointer_shape = Qnil;

  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-hourglass-pointer-shape", Vx_hourglass_pointer_shape,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_hourglass_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	       Vx_sensitive_text_pointer_shape,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-horizontal-drag-cursor",
	       Vx_window_horizontal_drag_shape,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("x-window-vertical-drag-cursor",
	       Vx_window_vertical_drag_shape,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_window_vertical_drag_shape = Qnil;
#endif

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_max_tooltip_size = Fcons (make_number (80), make_number (40));

  DEFVAR_LISP ("x-no-window-manager", Vx_no_window_manager,
	       doc: /* SKIP: real doc in xfns.c.  */);
  /* We don't have any way to find this out, so set it to nil
     and maybe the user would like to set it to t.  */
  Vx_no_window_manager = Qnil;

  DEFVAR_LISP ("x-pixel-size-width-font-regexp",
	       Vx_pixel_size_width_font_regexp,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_pixel_size_width_font_regexp = Qnil;

  DEFVAR_LISP ("w32-bdf-filename-alist",
	       Vw32_bdf_filename_alist,
	       doc: /* List of bdf fonts and their corresponding filenames.  */);
  Vw32_bdf_filename_alist = Qnil;

  DEFVAR_BOOL ("w32-strict-fontnames",
	       w32_strict_fontnames,
	       doc: /* Non-nil means only use fonts that are exact matches for those requested.
Default is nil, which allows old fontnames that are not XLFD compliant,
and allows third-party CJK display to work by specifying false charset
fields to trick Emacs into translating to Big5, SJIS etc.
Setting this to t will prevent wrong fonts being selected when
fontsets are automatically created.  */);
  w32_strict_fontnames = 0;

  DEFVAR_BOOL ("w32-strict-painting",
	       w32_strict_painting,
	       doc: /* Non-nil means use strict rules for repainting frames.
Set this to nil to get the old behavior for repainting; this should
only be necessary if the default setting causes problems.  */);
  w32_strict_painting = 1;

  DEFVAR_BOOL ("w32-use-fallback-wm-chars-method",
	       w32_use_fallback_wm_chars_method,
	       doc: /* Non-nil means use old method of processing character keys.
This is intended only for debugging of the new processing method.
Default is nil.

This variable has effect only on NT family of systems, not on Windows 9X.  */);
  w32_use_fallback_wm_chars_method = 0;

  DEFVAR_BOOL ("w32-disable-new-uniscribe-apis",
	       w32_disable_new_uniscribe_apis,
	       doc: /* Non-nil means don't use new Uniscribe APIs.
The new APIs are used to access OTF features supported by fonts.
This is intended only for debugging of the new Uniscribe-related code.
Default is nil.

This variable has effect only on Windows Vista and later.  */);
  w32_disable_new_uniscribe_apis = 0;

  DEFVAR_LISP ("w32-tooltip-extra-pixels",
	       Vw32_tooltip_extra_pixels,
	       doc: /* Number of pixels added after tooltip text.
On Windows some fonts may cause the last character of a tooltip be
truncated or wrapped around to the next line.  Adding some extra space
at the end of the toooltip works around this problem.

This variable specifies the number of pixels that shall be added.  The
default value t means to add the width of one canonical character of the
tip frame.  */);
  Vw32_tooltip_extra_pixels = Qt;

  DEFVAR_BOOL ("w32-disable-abort-dialog",
	       w32_disable_abort_dialog,
	       doc: /* Non-nil means don't display the abort dialog when aborting.  */);
  w32_disable_abort_dialog = 0;

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
  defsubr (&Sw32_frame_geometry);
  defsubr (&Sw32_frame_edges);
  defsubr (&Sw32_frame_list_z_order);
  defsubr (&Sw32_frame_restack);
  defsubr (&Sw32_mouse_absolute_pixel_position);
  defsubr (&Sw32_set_mouse_absolute_pixel_position);
  defsubr (&Sx_synchronize);

  /* W32 specific functions */

  defsubr (&Sw32_define_rgb_color);
  defsubr (&Sw32_default_color_map);
  defsubr (&Sw32_display_monitor_attributes_list);
  defsubr (&Sw32_send_sys_command);
  defsubr (&Sw32_shell_execute);
  defsubr (&Sw32_register_hot_key);
  defsubr (&Sw32_unregister_hot_key);
  defsubr (&Sw32_registered_hot_keys);
  defsubr (&Sw32_reconstruct_hot_key);
  defsubr (&Sw32_toggle_lock_key);
  defsubr (&Sw32_window_exists_p);
  defsubr (&Sw32__menu_bar_in_use);
#if defined WINDOWSNT && !defined HAVE_DBUS
  defsubr (&Sw32_notification_notify);
  defsubr (&Sw32_notification_close);
#endif

#ifdef WINDOWSNT
  defsubr (&Sfile_system_info);
  defsubr (&Sdefault_printer_name);
#endif

  defsubr (&Sset_message_beep);
  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);
  tip_last_frame = Qnil;
  staticpro (&tip_last_frame);
  tip_last_string = Qnil;
  staticpro (&tip_last_string);
  tip_last_parms = Qnil;
  staticpro (&tip_last_parms);

  defsubr (&Sx_file_dialog);
#ifdef WINDOWSNT
  defsubr (&Ssystem_move_file_to_trash);
#endif
}



/* Crashing and reporting backtrace.  */

#ifndef CYGWIN
static LONG CALLBACK my_exception_handler (EXCEPTION_POINTERS *);
static LPTOP_LEVEL_EXCEPTION_FILTER prev_exception_handler;
#endif
static DWORD except_code;
static PVOID except_addr;

#ifndef CYGWIN

/* Stack overflow recovery.  */

/* MinGW headers don't declare this (should be in malloc.h).  Also,
   the function is not present pre-W2K, so make the call through
   a function pointer.  */
typedef int (__cdecl *_resetstkoflw_proc) (void);
static _resetstkoflw_proc resetstkoflw;

/* Re-establish the guard page at stack limit.  This is needed because
   when a stack overflow is detected, Windows removes the guard bit
   from the guard page, so if we don't re-establish that protection,
   the next stack overflow will cause a crash.  */
void
w32_reset_stack_overflow_guard (void)
{
  if (resetstkoflw == NULL)
    resetstkoflw =
      (_resetstkoflw_proc)GetProcAddress (GetModuleHandle ("msvcrt.dll"),
					  "_resetstkoflw");
  /* We ignore the return value.  If _resetstkoflw fails, the next
     stack overflow will crash the program.  */
  if (resetstkoflw != NULL)
    (void)resetstkoflw ();
}

static void
stack_overflow_handler (void)
{
  /* Hard GC error may lead to stack overflow caused by
     too nested calls to mark_object.  No way to survive.  */
  if (gc_in_progress)
    terminate_due_to_signal (SIGSEGV, 40);
#ifdef _WIN64
  /* See ms-w32.h: MinGW64's longjmp crashes if invoked in this context.  */
  __builtin_longjmp (return_to_command_loop, 1);
#else
  sys_longjmp (return_to_command_loop, 1);
#endif
}

/* This handler records the exception code and the address where it
   was triggered so that this info could be included in the backtrace.
   Without that, the backtrace in some cases has no information
   whatsoever about the offending code, and looks as if the top-level
   exception handler in the MinGW startup code was the one that
   crashed.  We also recover from stack overflow, by calling our stack
   overflow handler that jumps back to top level.  */
static LONG CALLBACK
my_exception_handler (EXCEPTION_POINTERS * exception_data)
{
  except_code = exception_data->ExceptionRecord->ExceptionCode;
  except_addr = exception_data->ExceptionRecord->ExceptionAddress;

  /* If this is a stack overflow exception, attempt to recover.  */
  if (exception_data->ExceptionRecord->ExceptionCode == EXCEPTION_STACK_OVERFLOW
      && exception_data->ExceptionRecord->NumberParameters == 2
      /* We can only longjmp to top level from the main thread.  */
      && GetCurrentThreadId () == dwMainThreadId)
    {
      /* Call stack_overflow_handler ().  */
#ifdef _WIN64
      exception_data->ContextRecord->Rip = (DWORD_PTR) &stack_overflow_handler;
#else
      exception_data->ContextRecord->Eip = (DWORD_PTR) &stack_overflow_handler;
#endif
      /* Zero this out, so the stale address of the stack overflow
	 exception we handled is not displayed in some future
	 unrelated crash.  */
      except_addr = 0;
      return EXCEPTION_CONTINUE_EXECUTION;
    }

  if (prev_exception_handler)
    return prev_exception_handler (exception_data);
  return EXCEPTION_EXECUTE_HANDLER;
}
#endif

typedef USHORT (WINAPI * CaptureStackBackTrace_proc) (ULONG, ULONG, PVOID *,
						      PULONG);

#define BACKTRACE_LIMIT_MAX 62

static int
w32_backtrace (void **buffer, int limit)
{
  static CaptureStackBackTrace_proc s_pfn_CaptureStackBackTrace = NULL;
  HMODULE hm_kernel32 = NULL;

  if (!s_pfn_CaptureStackBackTrace)
    {
      hm_kernel32 = LoadLibrary ("Kernel32.dll");
      s_pfn_CaptureStackBackTrace =
	(CaptureStackBackTrace_proc) GetProcAddress (hm_kernel32,
						     "RtlCaptureStackBackTrace");
    }
  if (s_pfn_CaptureStackBackTrace)
    return s_pfn_CaptureStackBackTrace (0, min (BACKTRACE_LIMIT_MAX, limit),
					buffer, NULL);
  return 0;
}

void
emacs_abort (void)
{
  if (w32_disable_abort_dialog)
    abort ();

  int button;
  button = MessageBox (NULL,
		       "A fatal error has occurred!\n\n"
		       "Would you like to attach a debugger?\n\n"
		       "Select:\n"
		       "YES -- to debug Emacs, or\n"
		       "NO  -- to abort Emacs and produce a backtrace\n"
		       "       (emacs_backtrace.txt in current directory)."
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
      {
	void *stack[BACKTRACE_LIMIT_MAX + 1];
	int i = w32_backtrace (stack, BACKTRACE_LIMIT_MAX + 1);

	if (i)
	  {
	    int errfile_fd = -1;
	    int j;
	    char buf[sizeof ("\r\nException  at this address:\r\n\r\n")
		     /* The type below should really be 'void *', but
			INT_BUFSIZE_BOUND cannot handle that without
			triggering compiler warnings (under certain
			pedantic warning switches), it wants an
			integer type.  */
		     + 2 * INT_BUFSIZE_BOUND (intptr_t)];
#ifdef CYGWIN
	    int stderr_fd = 2;
#else
	    HANDLE errout = GetStdHandle (STD_ERROR_HANDLE);
	    int stderr_fd = -1;

	    if (errout && errout != INVALID_HANDLE_VALUE)
	      stderr_fd = _open_osfhandle ((intptr_t)errout, O_APPEND | O_BINARY);
#endif

	    /* We use %p, not 0x%p, as %p produces a leading "0x" on XP,
	       but not on Windows 7.  addr2line doesn't mind a missing
	       "0x", but will be confused by an extra one.  */
	    if (except_addr)
	      sprintf (buf, "\r\nException 0x%x at this address:\r\n%p\r\n",
		       (unsigned int) except_code, except_addr);
	    if (stderr_fd >= 0)
	      {
		if (except_addr)
		  write (stderr_fd, buf, strlen (buf));
		write (stderr_fd, "\r\nBacktrace:\r\n", 14);
	      }
#ifdef CYGWIN
#define _open open
#endif
	    errfile_fd = _open ("emacs_backtrace.txt", O_RDWR | O_CREAT | O_BINARY, S_IREAD | S_IWRITE);
	    if (errfile_fd >= 0)
	      {
		lseek (errfile_fd, 0L, SEEK_END);
		if (except_addr)
		  write (errfile_fd, buf, strlen (buf));
		write (errfile_fd, "\r\nBacktrace:\r\n", 14);
	      }

	    for (j = 0; j < i; j++)
	      {
		/* stack[] gives the return addresses, whereas we want
		   the address of the call, so decrease each address
		   by approximate size of 1 CALL instruction.  */
		sprintf (buf, "%p\r\n", (char *)stack[j] - sizeof(void *));
		if (stderr_fd >= 0)
		  write (stderr_fd, buf, strlen (buf));
		if (errfile_fd >= 0)
		  write (errfile_fd, buf, strlen (buf));
	      }
	    if (i == BACKTRACE_LIMIT_MAX)
	      {
		if (stderr_fd >= 0)
		  write (stderr_fd, "...\r\n", 5);
		if (errfile_fd >= 0)
		  write (errfile_fd, "...\r\n", 5);
	      }
	    if (errfile_fd >= 0)
	      close (errfile_fd);
	  }
	abort ();
	break;
      }
    }
}



/* Initialization.  */

/*
	globals_of_w32fns is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
	globals_of_w32fns is called from syms_of_w32fns when the global
	variable initialized is 0 and directly from main when initialized
	is non zero.
 */
void
globals_of_w32fns (void)
{
  HMODULE user32_lib = GetModuleHandle ("user32.dll");
  /*
    TrackMouseEvent not available in all versions of Windows, so must load
    it dynamically.  Do it once, here, instead of every time it is used.
  */
  track_mouse_event_fn = (TrackMouseEvent_Proc)
    GetProcAddress (user32_lib, "TrackMouseEvent");

  monitor_from_point_fn = (MonitorFromPoint_Proc)
    GetProcAddress (user32_lib, "MonitorFromPoint");
  get_monitor_info_fn = (GetMonitorInfo_Proc)
    GetProcAddress (user32_lib, "GetMonitorInfoA");
  monitor_from_window_fn = (MonitorFromWindow_Proc)
    GetProcAddress (user32_lib, "MonitorFromWindow");
  enum_display_monitors_fn = (EnumDisplayMonitors_Proc)
    GetProcAddress (user32_lib, "EnumDisplayMonitors");
  get_title_bar_info_fn = (GetTitleBarInfo_Proc)
    GetProcAddress (user32_lib, "GetTitleBarInfo");

  {
    HMODULE imm32_lib = GetModuleHandle ("imm32.dll");
    get_composition_string_fn = (ImmGetCompositionString_Proc)
      GetProcAddress (imm32_lib, "ImmGetCompositionStringW");
    get_ime_context_fn = (ImmGetContext_Proc)
      GetProcAddress (imm32_lib, "ImmGetContext");
    release_ime_context_fn = (ImmReleaseContext_Proc)
      GetProcAddress (imm32_lib, "ImmReleaseContext");
    set_ime_composition_window_fn = (ImmSetCompositionWindow_Proc)
      GetProcAddress (imm32_lib, "ImmSetCompositionWindow");
  }

  except_code = 0;
  except_addr = 0;
#ifndef CYGWIN
  prev_exception_handler = SetUnhandledExceptionFilter (my_exception_handler);
  resetstkoflw = NULL;
#endif

  DEFVAR_INT ("w32-ansi-code-page",
	      w32_ansi_code_page,
	      doc: /* The ANSI code page used by the system.  */);
  w32_ansi_code_page = GetACP ();

  if (os_subtype == OS_NT)
    w32_unicode_gui = 1;
  else
    w32_unicode_gui = 0;

  after_deadkey = -1;

  /* MessageBox does not work without this when linked to comctl32.dll 6.0.  */
  InitCommonControls ();

  syms_of_w32uniscribe ();
}

#ifdef NTGUI_UNICODE

Lisp_Object
ntgui_encode_system (Lisp_Object str)
{
  Lisp_Object encoded;
  to_unicode (str, &encoded);
  return encoded;
}

#endif /* NTGUI_UNICODE */
