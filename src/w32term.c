/* Implementation of GUI terminal on the Microsoft W32 API.
   Copyright (C) 1989, 93, 94, 95, 96, 1997, 1998, 1999, 2000, 2001
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

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include "lisp.h"
#include "charset.h"
#include "blockinput.h"

#include "w32heap.h"
#include "w32term.h"
#include "w32bdf.h"
#include <shellapi.h>

#include "systty.h"
#include "systime.h"
#include "atimer.h"
#include "keymap.h"

#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#include "keyboard.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "gnu.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "intervals.h"
#include "composite.h"
#include "coding.h"

#define abs(x)	((x) < 0 ? -(x) : (x))

#define BETWEEN(X, LOWER, UPPER)  ((X) >= (LOWER) && (X) < (UPPER))


/* Fringe bitmaps.  */

enum fringe_bitmap_type
{
  NO_FRINGE_BITMAP,
  LEFT_TRUNCATION_BITMAP,
  RIGHT_TRUNCATION_BITMAP,
  OVERLAY_ARROW_BITMAP,
  CONTINUED_LINE_BITMAP,
  CONTINUATION_LINE_BITMAP,
  ZV_LINE_BITMAP
};

/* Bitmaps are all unsigned short, as Windows requires bitmap data to
   be Word aligned.  For some reason they are horizontally reflected
   compared to how they appear on X, so changes in xterm.c should be
   reflected here.  */

/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */

#define zv_width 8
#define zv_height 72
#define zv_period 3
static unsigned short zv_bits[] = {
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00};
static HBITMAP zv_bmp;

/* An arrow like this: `<-'.  */

#define left_width 8
#define left_height 8
static unsigned short left_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};
static HBITMAP left_bmp;

/* Right truncation arrow bitmap `->'.  */

#define right_width 8
#define right_height 8
static unsigned short right_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};
static HBITMAP right_bmp;

/* Marker for continued lines.  */

#define continued_width 8
#define continued_height 8
static unsigned short continued_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};
static HBITMAP continued_bmp;

/* Marker for continuation lines.  */

#define continuation_width 8
#define continuation_height 8
static unsigned short continuation_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};
static HBITMAP continuation_bmp;

/* Overlay arrow bitmap.  */

#if 0
/* A bomb.  */
#define ov_width 8
#define ov_height 8
static unsigned short ov_bits[] = {
   0x0c, 0x10, 0x3c, 0x7e, 0x5e, 0x5e, 0x46, 0x3c};
#else
/* A triangular arrow.  */
#define ov_width 8
#define ov_height 8
static unsigned short ov_bits[] = {
   0xc0, 0xf0, 0xf8, 0xfc, 0xfc, 0xf8, 0xf0, 0xc0};
#endif
static HBITMAP ov_bmp;

extern Lisp_Object Qhelp_echo;


/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* If a string, w32_read_socket generates an event to display that string.
   (The display is done in read_char.)  */
   
static Lisp_Object help_echo;
static Lisp_Object help_echo_window;
static Lisp_Object help_echo_object;
static int help_echo_pos;

/* Temporary variable for w32_read_socket.  */

static Lisp_Object previous_help_echo;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Non-zero means autoselect window with the mouse cursor.  */

int autoselect_window_p;

/* Non-zero means draw block and hollow cursor as wide as the glyph
   under it.  For example, if a block cursor is over a tab, it will be
   drawn as wide as that tab on the display.  */

int x_stretch_cursor_p;

/* Non-zero means make use of UNDERLINE_POSITION font properties.  */

int x_use_underline_position_properties;

extern unsigned int msh_mousewheel;

extern void free_frame_menubar ();

extern int w32_codepage_for_font (char *fontname);

extern glyph_metric *w32_BDF_TextMetric(bdffont *fontp,
					unsigned char *text, int dim);
extern Lisp_Object Vwindow_system;

#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame


/* This is display since w32 does not support multiple ones.  */
struct w32_display_info one_w32_display_info;
struct w32_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of w32_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
Lisp_Object w32_display_name_list;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the
   w32 functions.  It is zero while not inside an update.
   In that case, the w32 functions assume that `SELECTED_FRAME ()'
   is the frame to apply to.  */
extern struct frame *updating_frame;

/* This is a frame waiting to be autoraised, within w32_read_socket.  */
struct frame *pending_autoraise_frame;

/* Nominal cursor position -- where to draw output.
   HPOS and VPOS are window relative glyph matrix coordinates.
   X and Y are window relative pixel coordinates.  */

struct cursor_pos output_cursor;

/* The handle of the frame that currently owns the system caret.  */
HWND w32_system_caret_hwnd;
int w32_system_caret_height;
int w32_system_caret_x;
int w32_system_caret_y;
int w32_use_visible_system_caret;

/* Flag to enable Unicode output in case users wish to use programs
   like Twinbridge on '95 rather than installed system level support
   for Far East languages.  */
int w32_enable_unicode_output;

DWORD dwWindowsThreadId = 0;
HANDLE hWindowsThread = NULL;
DWORD dwMainThreadId = 0;
HANDLE hMainThread = NULL;

#ifndef SIF_ALL
/* These definitions are new with Windows 95. */
#define SIF_RANGE           0x0001
#define SIF_PAGE            0x0002
#define SIF_POS             0x0004
#define SIF_DISABLENOSCROLL 0x0008
#define SIF_TRACKPOS        0x0010
#define SIF_ALL             (SIF_RANGE | SIF_PAGE | SIF_POS | SIF_TRACKPOS)

typedef struct tagSCROLLINFO
{
    UINT    cbSize;
    UINT    fMask;
    int     nMin;
    int     nMax;
    UINT    nPage;
    int     nPos;
    int     nTrackPos;
}   SCROLLINFO, FAR *LPSCROLLINFO;
typedef SCROLLINFO CONST FAR *LPCSCROLLINFO;
#endif /* SIF_ALL */

/* Dynamic linking to new proportional scroll bar functions. */
int (PASCAL *pfnSetScrollInfo) (HWND hwnd, int fnBar, LPSCROLLINFO lpsi, BOOL fRedraw);
BOOL (PASCAL *pfnGetScrollInfo) (HWND hwnd, int fnBar, LPSCROLLINFO lpsi);

int vertical_scroll_bar_min_handle;
int vertical_scroll_bar_top_border;
int vertical_scroll_bar_bottom_border;

int last_scroll_bar_drag_pos;

/* Mouse movement. */

/* Where the mouse was last time we reported a mouse event.  */

FRAME_PTR last_mouse_frame;
static RECT last_mouse_glyph;
static Lisp_Object last_mouse_press_frame;

Lisp_Object Vw32_num_mouse_buttons;

Lisp_Object Vw32_swap_mouse_buttons;

/* Control whether x_raise_frame also sets input focus.  */
Lisp_Object Vw32_grab_focus_on_raise;

/* Control whether Caps Lock affects non-ascii characters.  */
Lisp_Object Vw32_capslock_is_shiftlock;

/* Control whether right-alt and left-ctrl should be recognized as AltGr.  */
Lisp_Object Vw32_recognize_altgr;

/* The scroll bar in which the last motion event occurred.

   If the last motion event occurred in a scroll bar, we set this
   so w32_mouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell w32_mouse_position to return an ordinary motion event.  */
static Lisp_Object last_mouse_scroll_bar;
static int last_mouse_scroll_bar_pos;

/* This is a hack.  We would really prefer that w32_mouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the time-stamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */

static Time last_mouse_movement_time;

/* Incremented by w32_read_socket whenever it really tries to read
   events.  */

#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

extern Lisp_Object Vcommand_line_args, Vsystem_name;

extern Lisp_Object Qface, Qmouse_face;

#ifndef USE_CRT_DLL
extern int errno;
#endif

/* A mask of extra modifier bits to put into every keyboard char.  */

extern EMACS_INT extra_keyboard_modifiers;

/* Enumeration for overriding/changing the face to use for drawing
   glyphs in x_draw_glyphs.  */

enum draw_glyphs_face
{
  DRAW_NORMAL_TEXT,
  DRAW_INVERSE_VIDEO,
  DRAW_CURSOR,
  DRAW_MOUSE_FACE,
  DRAW_IMAGE_RAISED,
  DRAW_IMAGE_SUNKEN
};

static void x_update_window_end P_ ((struct window *, int, int));
static void frame_to_window_pixel_xy P_ ((struct window *, int *, int *));
void w32_delete_display P_ ((struct w32_display_info *));
static int fast_find_position P_ ((struct window *, int, int *, int *,
				   int *, int *, Lisp_Object));
static int fast_find_string_pos P_ ((struct window *, int, Lisp_Object,
				     int *, int *, int *, int *, int));
static void set_output_cursor P_ ((struct cursor_pos *));
static struct glyph *x_y_to_hpos_vpos P_ ((struct window *, int, int,
					   int *, int *, int *, int));
static void note_mode_line_highlight P_ ((struct window *, int, int));
static void note_mouse_highlight P_ ((struct frame *, int, int));
static void note_tool_bar_highlight P_ ((struct frame *f, int, int));
static void w32_handle_tool_bar_click P_ ((struct frame *,
                                          struct input_event *));
static void show_mouse_face P_ ((struct w32_display_info *,
				 enum draw_glyphs_face));
static int cursor_in_mouse_face_p P_ ((struct window *));
static int clear_mouse_face P_ ((struct w32_display_info *));

void x_lower_frame P_ ((struct frame *));
void x_scroll_bar_clear P_ ((struct frame *));
void x_wm_set_size_hint P_ ((struct frame *, long, int));
void x_raise_frame P_ ((struct frame *));
void x_set_window_size P_ ((struct frame *, int, int, int));
void x_wm_set_window_state P_ ((struct frame *, int));
void x_wm_set_icon_pixmap P_ ((struct frame *, int));
void w32_initialize P_ ((void));
static void x_font_min_bounds P_ ((XFontStruct *, int *, int *));
int x_compute_min_glyph_bounds P_ ((struct frame *));
static void x_draw_phys_cursor_glyph P_ ((struct window *,
					  struct glyph_row *,
					  enum draw_glyphs_face));
static void x_update_end P_ ((struct frame *));
static void w32_frame_up_to_date P_ ((struct frame *));
static void w32_set_terminal_modes P_ ((void));
static void w32_reset_terminal_modes P_ ((void));
static void w32_cursor_to P_ ((int, int, int, int));
static void x_write_glyphs P_ ((struct glyph *, int));
static void x_clear_end_of_line P_ ((int));
static void x_clear_frame P_ ((void));
static void x_clear_cursor P_ ((struct window *));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void x_new_focus_frame P_ ((struct w32_display_info *,
				   struct frame *));
static void w32_frame_rehighlight P_ ((struct frame *));
static void x_frame_rehighlight P_ ((struct w32_display_info *));
static void x_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void x_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int));
static void expose_frame P_ ((struct frame *, int, int, int, int));
static int expose_window_tree P_ ((struct window *, RECT *));
static int expose_window P_ ((struct window *, RECT *));
static void expose_area P_ ((struct window *, struct glyph_row *,
			     RECT *, enum glyph_row_area));
static int expose_line P_ ((struct window *, struct glyph_row *,
			    RECT *));
void x_update_cursor P_ ((struct frame *, int));
static void x_update_cursor_in_window_tree P_ ((struct window *, int));
static void x_update_window_cursor P_ ((struct window *, int));
static void x_erase_phys_cursor P_ ((struct window *));
void x_display_cursor P_ ((struct window *w, int, int, int, int, int));
void x_display_and_set_cursor P_ ((struct window *, int, int, int, int, int));
static void w32_draw_fringe_bitmap P_ ((struct window *, HDC hdc,
					struct glyph_row *, 
					enum fringe_bitmap_type, int left_p));
static void w32_clip_to_row P_ ((struct window *, struct glyph_row *,
                                 HDC, int));
static int x_phys_cursor_in_rect_p P_ ((struct window *, RECT *));
static void x_draw_row_fringe_bitmaps P_ ((struct window *,
					   struct glyph_row *));
static void notice_overwritten_cursor P_ ((struct window *,
					   enum glyph_row_area,
					   int, int, int, int));

static Lisp_Object Qvendor_specific_keysyms;


/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* This is a function useful for recording debugging information about
   the sequence of occurrences in this file.  */

struct record 
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

record_event (locus, type)
     char *locus;
     int type;
{
  if (event_record_index == sizeof (event_record) / sizeof (struct record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif /* 0 */


void XChangeGC (void * ignore, XGCValues* gc, unsigned long mask,
                XGCValues *xgcv)
{
  if (mask & GCForeground)
    gc->foreground = xgcv->foreground;
  if (mask & GCBackground)
    gc->background = xgcv->background;
  if (mask & GCFont)
    gc->font = xgcv->font;
}

XGCValues *XCreateGC (void * ignore, Window window, unsigned long mask,
                      XGCValues *xgcv)
{
  XGCValues *gc = (XGCValues *) xmalloc (sizeof (XGCValues));
  bzero (gc, sizeof (XGCValues));

  XChangeGC (ignore, gc, mask, xgcv);

  return gc;
}

void XGetGCValues (void* ignore, XGCValues *gc,
                   unsigned long mask, XGCValues *xgcv)
{
  XChangeGC (ignore, xgcv, mask, gc);
}

static void
w32_set_clip_rectangle (HDC hdc, RECT *rect)
{
  if (rect)
    {
      HRGN clip_region = CreateRectRgnIndirect (rect);
      SelectClipRgn (hdc, clip_region);
      DeleteObject (clip_region);
    }
  else
    SelectClipRgn (hdc, NULL);
}


/* Draw a hollow rectangle at the specified position.  */
void
w32_draw_rectangle (HDC hdc, XGCValues *gc, int x, int y,
                    int width, int height)
{
  HBRUSH hb, oldhb;
  HPEN hp, oldhp;

  hb = CreateSolidBrush (gc->background);
  hp = CreatePen (PS_SOLID, 0, gc->foreground);
  oldhb = SelectObject (hdc, hb);
  oldhp = SelectObject (hdc, hp);

  Rectangle (hdc, x, y, x + width, y + height);

  SelectObject (hdc, oldhb);
  SelectObject (hdc, oldhp);
  DeleteObject (hb);
  DeleteObject (hp);
}

/* Draw a filled rectangle at the specified position. */
void 
w32_fill_rect (f, hdc, pix, lprect)
     FRAME_PTR f;
     HDC hdc;
     COLORREF pix;
     RECT * lprect;
{
  HBRUSH hb;

  hb = CreateSolidBrush (pix);
  FillRect (hdc, lprect, hb);
  DeleteObject (hb);
}

void 
w32_clear_window (f)
     FRAME_PTR f;
{
  RECT rect;
  HDC hdc = get_frame_dc (f);

  /* Under certain conditions, this can be called at startup with
     a console frame pointer before the GUI frame is created. An HDC
     of 0 indicates this. */
  if (hdc)
    {
      GetClientRect (FRAME_W32_WINDOW (f), &rect);
      w32_clear_rect (f, hdc, &rect);
    }

  release_frame_dc (f, hdc);
}


/***********************************************************************
		    Starting and ending an update
 ***********************************************************************/
									
/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to x_update_window_begin for
   each window being updated.  */

static void
x_update_begin (f)
     struct frame *f;
{
  struct w32_display_info *display_info = FRAME_W32_DISPLAY_INFO (f);

  if (! FRAME_W32_P (f))
    return;

  /* Regenerate display palette before drawing if list of requested
     colors has changed. */
  if (display_info->regen_palette)
  {
    w32_regenerate_palette (f);
    display_info->regen_palette = FALSE;
  }
}


/* Start update of window W.  Set the global variable updated_window
   to the window being updated and set output_cursor to the cursor
   position of W.  */

static void
x_update_window_begin (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct w32_display_info *display_info = FRAME_W32_DISPLAY_INFO (f);

  /* Hide the system caret during an update.  */
  if (w32_use_visible_system_caret)
    SendMessage (w32_system_caret_hwnd, WM_EMACS_HIDE_CARET, 0, 0);

  updated_window = w;
  set_output_cursor (&w->cursor);

  BLOCK_INPUT;

  if (f == display_info->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      display_info->mouse_face_defer = 1;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	display_info->mouse_face_window = Qnil;

#if 0 /* Rows in a current matrix containing glyphs in mouse-face have
	 their mouse_face_p flag set, which means that they are always
	 unequal to rows in a desired matrix which never have that
	 flag set.  So, rows containing mouse-face glyphs are never
	 scrolled, and we don't have to switch the mouse highlight off
	 here to prevent it from being scrolled.  */
      
      /* Can we tell that this update does not affect the window
	 where the mouse highlight is?  If so, no need to turn off.
	 Likewise, don't do anything if the frame is garbaged;
	 in that case, the frame's current matrix that we would use
	 is all wrong, and we will redisplay that line anyway.  */
      if (!NILP (display_info->mouse_face_window)
	  && w == XWINDOW (display_info->mouse_face_window))
	{
	  int i;

          for (i = 0; i < w->desired_matrix->nrows; ++i)
	    if (MATRIX_ROW_ENABLED_P (w->desired_matrix, i))
	      break;

	  if (i < w->desired_matrix->nrows)
	    clear_mouse_face (display_info);
	}
#endif /* 0 */
    }

  UNBLOCK_INPUT;
}


/* Draw a vertical window border to the right of window W if W doesn't
   have vertical scroll bars.  */

static void
x_draw_vertical_border (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  
  /* Redraw borders between horizontally adjacent windows.  Don't
     do it for frames with vertical scroll bars because either the
     right scroll bar of a window, or the left scroll bar of its
     neighbor will suffice as a border.  */
  if (!WINDOW_RIGHTMOST_P (w)
      && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      RECT r;
      HDC hdc;

      window_box_edges (w, -1, (int *) &r.left, (int *) &r.top,
			(int *) &r.right, (int *) &r.bottom);
      r.left = r.right + FRAME_X_RIGHT_FRINGE_WIDTH (f);
      r.right = r.left + 1;
      r.bottom -= 1;

      hdc = get_frame_dc (f);
      w32_fill_rect (f, hdc, FRAME_FOREGROUND_PIXEL (f), &r);
      release_frame_dc (f, hdc);
    }
}

   
/* End update of window W (which is equal to updated_window).

   Draw vertical borders between horizontally adjacent windows, and
   display W's cursor if CURSOR_ON_P is non-zero.

   MOUSE_FACE_OVERWRITTEN_P non-zero means that some row containing
   glyphs in mouse-face were overwritten.  In that case we have to
   make sure that the mouse-highlight is properly redrawn.

   W may be a menu bar pseudo-window in case we don't have X toolkit
   support. Such windows don't have a cursor, so don't display it
   here. */

static void
x_update_window_end (w, cursor_on_p, mouse_face_overwritten_p)
     struct window *w;
     int cursor_on_p, mouse_face_overwritten_p;
{
  struct w32_display_info *dpyinfo
    = FRAME_W32_DISPLAY_INFO (XFRAME (w->frame));

  if (!w->pseudo_window_p)
    {
      BLOCK_INPUT;

      if (cursor_on_p)
	x_display_and_set_cursor (w, 1, output_cursor.hpos,
				  output_cursor.vpos,
				  output_cursor.x, output_cursor.y);
      
      x_draw_vertical_border (w);
      UNBLOCK_INPUT;
    }

  /* If a row with mouse-face was overwritten, arrange for
     XTframe_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }

  /* Unhide the caret.  This won't actually show the cursor, unless it
     was visible before the corresponding call to HideCaret in
     x_update_window_begin.  */
  if (w32_use_visible_system_caret)
    SendMessage (w32_system_caret_hwnd, WM_EMACS_SHOW_CARET, 0, 0);

  updated_window = NULL;
}


/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
x_update_end (f)
     struct frame *f;
{
  if (! FRAME_W32_P (f))
    return;

  /* Mouse highlight may be displayed again.  */
  FRAME_W32_DISPLAY_INFO (f)->mouse_face_defer = 0;
}


/* This function is called from various places in xdisp.c whenever a
   complete update has been performed.  The global variable
   updated_window is not available here.  */

static void
w32_frame_up_to_date (f)
     struct frame *f;
{
  if (FRAME_W32_P (f))
    {
      struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
      if (dpyinfo->mouse_face_deferred_gc
	  || f == dpyinfo->mouse_face_mouse_frame)
	{
	  BLOCK_INPUT;
	  if (dpyinfo->mouse_face_mouse_frame)
	    note_mouse_highlight (dpyinfo->mouse_face_mouse_frame,
				  dpyinfo->mouse_face_mouse_x,
				  dpyinfo->mouse_face_mouse_y);
	  dpyinfo->mouse_face_deferred_gc = 0;
	  UNBLOCK_INPUT;
	}
    }
}


/* Draw truncation mark bitmaps, continuation mark bitmaps, overlay
   arrow bitmaps, or clear the fringes if no bitmaps are required
   before DESIRED_ROW is made current.  The window being updated is
   found in updated_window.  This function is called from
   update_window_line only if it is known that there are differences
   between bitmaps to be drawn between current row and DESIRED_ROW.  */

static void
x_after_update_window_line (desired_row)
     struct glyph_row *desired_row;
{
  struct window *w = updated_window;
  struct frame *f;
  int width, height;

  xassert (w);
  
  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    {
      BLOCK_INPUT;
      x_draw_row_fringe_bitmaps (w, desired_row);
      UNBLOCK_INPUT;
    }

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  Could
     check here if updated_window is the leftmost/rightmost window,
     but I guess it's not worth doing since vertically split windows
     are almost never used, internal border is rarely set, and the
     overhead is very small.  */
  if (windows_or_buffers_changed
      && desired_row->full_width_p
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));
      /* Internal border is drawn below the tool bar.  */
      if (WINDOWP (f->tool_bar_window)
	  && w == XWINDOW (f->tool_bar_window))
	y -= width;

      BLOCK_INPUT;
      {
	HDC hdc = get_frame_dc (f);
	w32_clear_area (f, hdc, 0, y, width, height);
	w32_clear_area (f, hdc, f->output_data.w32->pixel_width - width,
			y, width, height);
	release_frame_dc (f, hdc);
      }
      UNBLOCK_INPUT;
    }
}


/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.  */

static void
w32_draw_fringe_bitmap (w, hdc, row, which, left_p)
     struct window *w;
     HDC hdc;
     struct glyph_row *row;
     enum fringe_bitmap_type which;
     int left_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Window window = FRAME_W32_WINDOW (f);
  HDC compat_hdc;
  int x, y, wd, h, dy;
  int b1, b2;
  HBITMAP pixmap;
  HANDLE horig_obj;
  struct face *face;

  /* Must clip because of partially visible lines.  */
  w32_clip_to_row (w, row, hdc, 1);

  /* Convert row to frame coordinates.  */
  y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  switch (which)
    {
    case NO_FRINGE_BITMAP:
      wd = 0;
      h = 0;
      break;

    case LEFT_TRUNCATION_BITMAP:
      wd = left_width;
      h = left_height;
      pixmap = left_bmp;
      break;
      
    case OVERLAY_ARROW_BITMAP:
      wd = ov_width;
      h = ov_height;
      pixmap = ov_bmp;
      break;
      
    case RIGHT_TRUNCATION_BITMAP:
      wd = right_width;
      h = right_height;
      pixmap = right_bmp;
      break;

    case CONTINUED_LINE_BITMAP:
      wd = continued_width;
      h = continued_height;
      pixmap = continued_bmp;
      break;
      
    case CONTINUATION_LINE_BITMAP:
      wd = continuation_width;
      h = continuation_height;
      pixmap = continuation_bmp;
      break;

    case ZV_LINE_BITMAP:
      wd = zv_width;
      h = zv_height - (y % zv_period);
      pixmap = zv_bmp;
      break;

    default:
      abort ();
    }

  /* Clip bitmap if too high.  */
  if (h > row->height)
    h = row->height;

  /* Set dy to the offset in the row to start drawing the bitmap.  */
  dy = (row->height - h) / 2;

  /* Draw the bitmap.  */
  face = FACE_FROM_ID (f, FRINGE_FACE_ID);
  PREPARE_FACE_FOR_DISPLAY (f, face);

  /* Clear left fringe if no bitmap to draw or if bitmap doesn't fill
     the fringe.  */
  b1 = -1;
  if (left_p)
    {
      if (wd > FRAME_X_LEFT_FRINGE_WIDTH (f))
	wd = FRAME_X_LEFT_FRINGE_WIDTH (f);
      x = (WINDOW_TO_FRAME_PIXEL_X (w, 0)
	   - wd
	   - (FRAME_X_LEFT_FRINGE_WIDTH (f) - wd) / 2);
      if (wd < FRAME_X_LEFT_FRINGE_WIDTH (f) || row->height > h)
	{
	  /* If W has a vertical border to its left, don't draw over it.  */
	  int border = ((XFASTINT (w->left) > 0
			 && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
			? 1 : 0);
	  b1 = (window_box_left (w, -1)
		- FRAME_X_LEFT_FRINGE_WIDTH (f)
		+ border);
	  b2 = (FRAME_X_LEFT_FRINGE_WIDTH (f) - border);
	}
    }
  else
    {
      if (wd > FRAME_X_RIGHT_FRINGE_WIDTH (f))
	wd = FRAME_X_RIGHT_FRINGE_WIDTH (f);
      x = (window_box_right (w, -1)
	   + (FRAME_X_RIGHT_FRINGE_WIDTH (f) - wd) / 2);
      /* Clear right fringe if no bitmap to draw of if bitmap doesn't fill
	 the fringe.  */
      if (wd < FRAME_X_RIGHT_FRINGE_WIDTH (f) || row->height > h)
	{
	  b1 = window_box_right (w, -1);
	  b2 = FRAME_X_RIGHT_FRINGE_WIDTH (f);
	}
    }

  if (b1 >= 0)
    {
      int header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      
      w32_fill_area (f, hdc, face->background,
                     b1,
                     WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
                                                      row->y)),
                     b2,
                     row->visible_height);
    }

  if (which == NO_FRINGE_BITMAP)
    return;

  compat_hdc = CreateCompatibleDC (hdc);
  SaveDC (hdc);

  horig_obj = SelectObject (compat_hdc, pixmap);
  SetTextColor (hdc, face->background);
  SetBkColor (hdc, face->foreground);

  BitBlt (hdc, x, y + dy, wd, h, compat_hdc, 0,
	  (which == ZV_LINE_BITMAP ? (row->y % zv_period) : 0),
	  SRCCOPY);

  SelectObject (compat_hdc, horig_obj);
  DeleteDC (compat_hdc);
  RestoreDC (hdc, -1);
}


/* Draw fringe bitmaps for glyph row ROW on window W.  Call this
   function with input blocked.  */

static void
x_draw_row_fringe_bitmaps (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct frame *f = XFRAME (w->frame);
  enum fringe_bitmap_type bitmap;
  HDC hdc;

  xassert (interrupt_input_blocked);

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

  hdc = get_frame_dc (f);

  if (FRAME_X_LEFT_FRINGE_WIDTH (f) != 0)
    {
      /* Decide which bitmap to draw in the left fringe.  */
      if (row->overlay_arrow_p)
	bitmap = OVERLAY_ARROW_BITMAP;
      else if (row->truncated_on_left_p)
	bitmap = LEFT_TRUNCATION_BITMAP;
      else if (MATRIX_ROW_CONTINUATION_LINE_P (row))
	bitmap = CONTINUATION_LINE_BITMAP;
      else if (row->indicate_empty_line_p)
	bitmap = ZV_LINE_BITMAP;
      else
	bitmap = NO_FRINGE_BITMAP;

      w32_draw_fringe_bitmap (w, hdc, row, bitmap, 1);
    }

  if (FRAME_X_RIGHT_FRINGE_WIDTH (f) != 0)
    {
      /* Decide which bitmap to draw in the right fringe.  */
      if (row->truncated_on_right_p)
	bitmap = RIGHT_TRUNCATION_BITMAP;
      else if (row->continued_p)
	bitmap = CONTINUED_LINE_BITMAP;
      else if (row->indicate_empty_line_p && FRAME_X_LEFT_FRINGE_WIDTH (f) == 0)
	bitmap = ZV_LINE_BITMAP;
      else
	bitmap = NO_FRINGE_BITMAP;

      w32_draw_fringe_bitmap (w, hdc, row, bitmap, 0);
    }

  release_frame_dc (f, hdc);
}


/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
w32_set_terminal_modes (void)
{
}

/* This is called when exiting or suspending Emacs. Exiting will make
   the W32 windows go away, and suspending requires no action. */

static void
w32_reset_terminal_modes (void)
{
}



/***********************************************************************
			    Output Cursor
 ***********************************************************************/

/* Set the global variable output_cursor to CURSOR.  All cursor
   positions are relative to updated_window.  */

static void
set_output_cursor (cursor)
    struct cursor_pos *cursor;
{
  output_cursor.hpos = cursor->hpos;
  output_cursor.vpos = cursor->vpos;
  output_cursor.x = cursor->x;
  output_cursor.y = cursor->y;
}


/* Set a nominal cursor position.

   HPOS and VPOS are column/row positions in a window glyph matrix.  X
   and Y are window text area relative pixel positions.
   
   If this is done during an update, updated_window will contain the
   window that is being updated and the position is the future output
   cursor position for that window.  If updated_window is null, use
   selected_window and display the cursor at the given position.  */

static void
w32_cursor_to (vpos, hpos, y, x)
     int vpos, hpos, y, x;
{
  struct window *w;

  /* If updated_window is not set, work on selected_window.  */
  if (updated_window)
    w = updated_window;
  else
    w = XWINDOW (selected_window);

  /* Set the output cursor.  */
  output_cursor.hpos = hpos;
  output_cursor.vpos = vpos;
  output_cursor.x = x;
  output_cursor.y = y;

  /* If not called as part of an update, really display the cursor.
     This will also set the cursor position of W.  */
  if (updated_window == NULL)
    {
      BLOCK_INPUT;
      x_display_cursor (w, 1, hpos, vpos, x, y);
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static struct face *x_get_glyph_face_and_encoding P_ ((struct frame *,
						       struct glyph *,
						       wchar_t *,
                                                       int *));
static struct face *x_get_char_face_and_encoding P_ ((struct frame *, int,
						      int, wchar_t *, int));
static XCharStruct *w32_per_char_metric P_ ((XFontStruct *,
                                             wchar_t *,
                                             enum w32_char_font_type));
static enum w32_char_font_type
  w32_encode_char P_ ((int, wchar_t *, struct font_info *, int *));
static void x_append_glyph P_ ((struct it *));
static void x_append_composite_glyph P_ ((struct it *));
static void x_append_stretch_glyph P_ ((struct it *it, Lisp_Object,
					int, int, double));
static void x_produce_glyphs P_ ((struct it *));
static void x_produce_image_glyph P_ ((struct it *it));


/* Dealing with bits of wchar_t as if they were an XChar2B.  */
#define BUILD_WCHAR_T(byte1, byte2) \
 ((wchar_t)((((byte1) & 0x00ff) << 8) | ((byte2) & 0x00ff)))


#define BYTE1(ch) \
 (((ch) & 0xff00) >> 8)

#define BYTE2(ch) \
 ((ch) & 0x00ff)


/* Get metrics of character CHAR2B in FONT.  Value is always non-null.
   If CHAR2B is not contained in FONT, the font's default character
   metric is returned. */

static int
w32_bdf_per_char_metric (font, char2b, dim, pcm)
     XFontStruct *font;
     wchar_t *char2b;
     int dim;
     XCharStruct * pcm;
{
  glyph_metric * bdf_metric;
  char buf[2];

  if (dim == 1)
    buf[0] = (char)(*char2b);
  else
    {
      buf[0] = BYTE1 (*char2b);
      buf[1] = BYTE2 (*char2b);
    }

  bdf_metric = w32_BDF_TextMetric (font->bdf, buf, dim);

  if (bdf_metric)
    {
      pcm->width = bdf_metric->dwidth;
      pcm->lbearing = bdf_metric->bbox;
      pcm->rbearing = bdf_metric->dwidth
                    - (bdf_metric->bbox + bdf_metric->bbw);
      pcm->ascent = bdf_metric->bboy + bdf_metric->bbh;
      pcm->descent = -bdf_metric->bboy;

      return 1;
    }
  return 0;
}


static int
w32_native_per_char_metric (font, char2b, font_type, pcm)
     XFontStruct *font;
     wchar_t *char2b;
     enum w32_char_font_type font_type;
     XCharStruct * pcm;
{
  HDC hdc = GetDC (NULL);
  HFONT old_font;
  BOOL retval = FALSE;

  xassert (font && char2b);
  xassert (font->hfont);
  xassert (font_type == UNICODE_FONT || font_type == ANSI_FONT);

  old_font = SelectObject (hdc, font->hfont);

  if ((font->tm.tmPitchAndFamily & TMPF_TRUETYPE) != 0)
    {
      ABC char_widths;

      if (font_type == UNICODE_FONT)
	retval = GetCharABCWidthsW (hdc, *char2b, *char2b, &char_widths);
      else
	retval = GetCharABCWidthsA (hdc, *char2b, *char2b, &char_widths);

      if (retval)
	{
#if 0
	  /* Disabled until we can find a way to get the right results
	     on all versions of Windows.  */

	  /* Don't trust the ABC widths.  For synthesized fonts they are
	     wrong, and so is the result of GetCharWidth()!  */
	  int real_width;
	  GetCharWidth (hdc, *char2b, *char2b, &real_width);
#endif
	  pcm->width = char_widths.abcA + char_widths.abcB + char_widths.abcC;
#if 0
	  /* As far as I can tell, this is the best way to determine what
	     ExtTextOut will do with the broken font.  */
	  if (pcm->width != real_width)
	    pcm->width = (pcm->width + real_width) / 2;
#endif
	  pcm->lbearing = char_widths.abcA;
	  pcm->rbearing = char_widths.abcA + char_widths.abcB;
	  pcm->ascent = FONT_BASE (font);
	  pcm->descent = FONT_DESCENT (font);
	}
    }

  if (!retval)
    {
      /* Either font is not a True-type font, or GetCharABCWidthsW
	 failed (it is not supported on Windows 9x for instance), so we
	 can't determine the full info we would like.  All is not lost
	 though - we can call GetTextExtentPoint32 to get rbearing and
	 deduce width based on the font's per-string overhang.  lbearing
	 is assumed to be zero.  */

      /* TODO: Some Thai characters (and other composites if Windows
         supports them) do have lbearing, and report their total width
         as zero. Need some way of handling them when
         GetCharABCWidthsW fails. */
      SIZE sz;

      if (font_type == UNICODE_FONT)
	retval = GetTextExtentPoint32W (hdc, char2b, 1, &sz);
      else
	retval = GetTextExtentPoint32A (hdc, (char*)char2b, 1, &sz);

      if (retval)
	{
	  pcm->width = sz.cx - font->tm.tmOverhang;
	  pcm->rbearing = sz.cx;
	  pcm->lbearing = 0;
	  pcm->ascent = FONT_BASE (font);
	  pcm->descent = FONT_DESCENT (font);
	}
    }


  if (pcm->width == 0 && (pcm->rbearing - pcm->lbearing) == 0)
    {
      retval = FALSE;
    }

  SelectObject (hdc, old_font);
  ReleaseDC (NULL, hdc);

  return retval;
}


static XCharStruct *
w32_per_char_metric (font, char2b, font_type)
     XFontStruct *font;
     wchar_t *char2b;
     enum w32_char_font_type font_type;
{
  /* The result metric information.  */
  XCharStruct *pcm;
  BOOL retval;

  xassert (font && char2b);
  xassert (font_type != UNKNOWN_FONT);

  /* Handle the common cases quickly.  */
  if (!font->bdf && font->per_char == NULL)
    /* TODO: determine whether char2b exists in font?  */
    return &font->max_bounds;
  else if (!font->bdf && *char2b < 128)
    return &font->per_char[*char2b];

  pcm = &font->scratch;

  if (font_type == BDF_1D_FONT)
    retval = w32_bdf_per_char_metric (font, char2b, 1, pcm);
  else if (font_type == BDF_2D_FONT)
    retval = w32_bdf_per_char_metric (font, char2b, 2, pcm);
  else
    retval = w32_native_per_char_metric (font, char2b, font_type, pcm);

  if (retval)
    return pcm;

  return NULL;
}

void
w32_cache_char_metrics (font)
     XFontStruct *font;
{
  wchar_t char2b = L'x';

  /* Cache char metrics for the common cases.  */
  if (font->bdf)
    {
      /* TODO: determine whether font is fixed-pitch.  */
      if (!w32_bdf_per_char_metric (font, &char2b, 1, &font->max_bounds))
        {
          /* Use the font width and height as max bounds, as not all BDF
             fonts contain the letter 'x'. */
          font->max_bounds.width = FONT_MAX_WIDTH (font);
          font->max_bounds.lbearing = -font->bdf->llx;
          font->max_bounds.rbearing = FONT_MAX_WIDTH (font) - font->bdf->urx;
          font->max_bounds.ascent = FONT_BASE (font);
          font->max_bounds.descent = FONT_DESCENT (font);
        }
    }
  else
    {
      if (((font->tm.tmPitchAndFamily & TMPF_FIXED_PITCH) != 0)
          /* Some fonts (eg DBCS fonts) are marked as fixed width even
             though they contain characters of different widths. */
          || (font->tm.tmMaxCharWidth != font->tm.tmAveCharWidth))
	{
	  /* Font is not fixed pitch, so cache per_char info for the
             ASCII characters.  It would be much more work, and probably
             not worth it, to cache other chars, since we may change
             between using Unicode and ANSI text drawing functions at
             run-time.  */
	  int i;

	  font->per_char = xmalloc (128 * sizeof(XCharStruct));
	  for (i = 0; i < 128; i++)
	    {
	      char2b = i;
	      w32_native_per_char_metric (font, &char2b, ANSI_FONT,
					  &font->per_char[i]);
	    }
	}
      else
	w32_native_per_char_metric (font, &char2b, ANSI_FONT,
				    &font->max_bounds);
    }
}


/* Determine if a font is double byte. */
int w32_font_is_double_byte (XFontStruct *font)
{
  return font->double_byte_p;
}


static BOOL 
w32_use_unicode_for_codepage (codepage)
     int codepage;
{
  /* If the current codepage is supported, use Unicode for output. */
  return (w32_enable_unicode_output
          && codepage != CP_8BIT
          && (codepage == CP_UNICODE || IsValidCodePage (codepage)));
}

/* Encode CHAR2B using encoding information from FONT_INFO.  CHAR2B is
   the two-byte form of C.  Encoding is returned in *CHAR2B.  */

static INLINE enum w32_char_font_type
w32_encode_char (c, char2b, font_info, two_byte_p)
     int c;
     wchar_t *char2b;
     struct font_info *font_info;
     int * two_byte_p;
{
  int charset = CHAR_CHARSET (c);
  int codepage;
  int unicode_p = 0;

  XFontStruct *font = font_info->font;

  xassert (two_byte_p);

  *two_byte_p = w32_font_is_double_byte (font);

  /* FONT_INFO may define a scheme by which to encode byte1 and byte2.
     This may be either a program in a special encoder language or a
     fixed encoding.  */
  if (font_info->font_encoder)
    {
      /* It's a program.  */
      struct ccl_program *ccl = font_info->font_encoder;

      if (CHARSET_DIMENSION (charset) == 1)
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = BYTE2 (*char2b);
	}
      else
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = BYTE1 (*char2b);
	  ccl->reg[2] = BYTE2 (*char2b);
	}

      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);

      /* We assume that MSBs are appropriately set/reset by CCL
	 program.  */
      if (!*two_byte_p)	/* 1-byte font */
	*char2b = BUILD_WCHAR_T (0, ccl->reg[1]);
      else
	*char2b = BUILD_WCHAR_T (ccl->reg[1], ccl->reg[2]);
    }
  else if (font_info->encoding[charset])
    {
      /* Fixed encoding scheme.  See fontset.h for the meaning of the
	 encoding numbers.  */
      int enc = font_info->encoding[charset];
      
      if ((enc == 1 || enc == 2)
	  && CHARSET_DIMENSION (charset) == 2)
	*char2b = BUILD_WCHAR_T (BYTE1 (*char2b) | 0x80, BYTE2 (*char2b));
      
      if (enc == 1 || enc == 3
          || (enc == 4 && CHARSET_DIMENSION (charset) == 1))
	*char2b = BUILD_WCHAR_T (BYTE1 (*char2b), BYTE2 (*char2b) | 0x80);
      else if (enc == 4)
        {
          int sjis1, sjis2;

          ENCODE_SJIS (BYTE1 (*char2b), BYTE2 (*char2b),
                       sjis1, sjis2);
          *char2b = BUILD_WCHAR_T (sjis1, sjis2);
        }
    }
  codepage = font_info->codepage;

  /* If charset is not ASCII or Latin-1, may need to move it into
     Unicode space.  */
  if ( font && !font->bdf && w32_use_unicode_for_codepage (codepage)
       && charset != CHARSET_ASCII && charset != charset_latin_iso8859_1
       && charset != CHARSET_8_BIT_CONTROL && charset != CHARSET_8_BIT_GRAPHIC)
    {
      char temp[3];
      temp[0] = BYTE1 (*char2b);
      temp[1] = BYTE2 (*char2b);
      temp[2] = '\0';
      if (codepage != CP_UNICODE)
        {
          if (temp[0])
            MultiByteToWideChar (codepage, 0, temp, 2, char2b, 1);
          else
            MultiByteToWideChar (codepage, 0, temp+1, 1, char2b, 1);
        }
      unicode_p = 1;
      *two_byte_p = 1;
    }
  if (!font)
    return UNKNOWN_FONT;
  else if (font->bdf && CHARSET_DIMENSION (charset) == 1)
    return BDF_1D_FONT;
  else if (font->bdf)
    return BDF_2D_FONT;
  else if (unicode_p)
    return UNICODE_FONT;
  else
    return ANSI_FONT;
}


/* Get face and two-byte form of character C in face FACE_ID on frame
   F.  The encoding of C is returned in *CHAR2B.  MULTIBYTE_P non-zero
   means we want to display multibyte text.  Value is a pointer to a
   realized face that is ready for display.  */

static INLINE struct face *
x_get_char_face_and_encoding (f, c, face_id, char2b, multibyte_p)
     struct frame *f;
     int c, face_id;
     wchar_t *char2b;
     int multibyte_p;
{
  struct face *face = FACE_FROM_ID (f, face_id);

  if (!multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      *char2b = BUILD_WCHAR_T (0, c);
      face_id = FACE_FOR_CHAR (f, face, c);
      face = FACE_FROM_ID (f, face_id);
    }
  else if (c < 128 && face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      *char2b = BUILD_WCHAR_T (0, c);
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (c, charset, c1, c2);
      if (c2 > 0)
	*char2b = BUILD_WCHAR_T (c1, c2);
      else
	*char2b = BUILD_WCHAR_T (0, c1);
  
      /* Maybe encode the character in *CHAR2B.  */
      if (face->font != NULL)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	      w32_encode_char (c, char2b, font_info, &multibyte_p);
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  
  return face;
}


/* Get face and two-byte form of character glyph GLYPH on frame F.
   The encoding of GLYPH->u.ch is returned in *CHAR2B.  Value is
   a pointer to a realized face that is ready for display.  */

static INLINE struct face *
x_get_glyph_face_and_encoding (f, glyph, char2b, two_byte_p)
     struct frame *f;
     struct glyph *glyph;
     wchar_t *char2b;
     int *two_byte_p;
{
  struct face *face;
  int dummy = 0;

  xassert (glyph->type == CHAR_GLYPH);
  face = FACE_FROM_ID (f, glyph->face_id);

  if (two_byte_p)
    *two_byte_p = 0;
  else
    two_byte_p = &dummy;

  if (!glyph->multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      *char2b = BUILD_WCHAR_T (0, glyph->u.ch);
    }
  else if (glyph->u.ch < 128
	   && glyph->face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      *char2b = BUILD_WCHAR_T (0, glyph->u.ch);
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (glyph->u.ch, charset, c1, c2);
      if (c2 > 0)
	*char2b = BUILD_WCHAR_T (c1, c2);
      else
	*char2b = BUILD_WCHAR_T (0, c1);

      /* Maybe encode the character in *CHAR2B.  */
      if (charset != CHARSET_ASCII)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	    {
	      glyph->w32_font_type
                = w32_encode_char (glyph->u.ch, char2b, font_info, two_byte_p);
	    }
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  return face;
}


/* Store one glyph for IT->char_to_display in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
x_append_glyph (it)
     struct it *it;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  xassert (it->char_to_display != '\n' && it->char_to_display != '\t');
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = CHAR_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = it->glyph_not_available_p;
      glyph->face_id = it->face_id;
      glyph->u.ch = it->char_to_display;
      glyph->w32_font_type = UNKNOWN_FONT;
      ++it->glyph_row->used[area];
    }
}

/* Store one glyph for the composition IT->cmp_id in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
x_append_composite_glyph (it)
     struct it *it;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = COMPOSITE_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.cmp_id = it->cmp_id;
      glyph->w32_font_type = UNKNOWN_FONT;
      ++it->glyph_row->used[area];
    }
}


/* Change IT->ascent and IT->height according to the setting of
   IT->voffset.  */

static INLINE void
take_vertical_position_into_account (it)
     struct it *it;
{
  if (it->voffset)
    {
      if (it->voffset < 0)
	/* Increase the ascent so that we can display the text higher
	   in the line.  */
	it->ascent += abs (it->voffset);
      else
	/* Increase the descent so that we can display the text lower
	   in the line.  */
	it->descent += it->voffset;
    }
}


/* Produce glyphs/get display metrics for the image IT is loaded with.
   See the description of struct display_iterator in dispextern.h for
   an overview of struct display_iterator.  */

static void
x_produce_image_glyph (it)
     struct it *it;
{
  struct image *img;
  struct face *face;

  xassert (it->what == IT_IMAGE);

  face = FACE_FROM_ID (it->f, it->face_id);
  img = IMAGE_FROM_ID (it->f, it->image_id);
  xassert (img);

  /* Make sure X resources of the face and image are loaded.  */
  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  prepare_image_for_display (it->f, img);

  it->ascent = it->phys_ascent = image_ascent (img, face);
  it->descent = it->phys_descent = img->height + 2 * img->vmargin - it->ascent;
  it->pixel_width = img->width + 2 * img->hmargin;

  it->nglyphs = 1;
  
  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }

  take_vertical_position_into_account (it);
  
  if (it->glyph_row)
    {
      struct glyph *glyph;
      enum glyph_row_area area = it->area;
      
      glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
      if (glyph < it->glyph_row->glyphs[area + 1])
	{
	  glyph->charpos = CHARPOS (it->position);
	  glyph->object = it->object;
	  glyph->pixel_width = it->pixel_width;
	  glyph->voffset = it->voffset;
	  glyph->type = IMAGE_GLYPH;
	  glyph->multibyte_p = it->multibyte_p;
	  glyph->left_box_line_p = it->start_of_box_run_p;
	  glyph->right_box_line_p = it->end_of_box_run_p;
	  glyph->overlaps_vertically_p = 0;
          glyph->padding_p = 0;
	  glyph->glyph_not_available_p = 0;
	  glyph->face_id = it->face_id;
	  glyph->u.img_id = img->id;
          glyph->w32_font_type = UNKNOWN_FONT;
	  ++it->glyph_row->used[area];
	}
    }
}


/* Append a stretch glyph to IT->glyph_row.  OBJECT is the source
   of the glyph, WIDTH and HEIGHT are the width and height of the 
   stretch.  ASCENT is the percentage/100 of HEIGHT to use for the 
   ascent of the glyph (0 <= ASCENT <= 1).  */
  
static void
x_append_stretch_glyph (it, object, width, height, ascent)
     struct it *it;
     Lisp_Object object;
     int width, height;
     double ascent;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (ascent >= 0 && ascent <= 1);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = object;
      glyph->pixel_width = width;
      glyph->voffset = it->voffset;
      glyph->type = STRETCH_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = 0;
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.stretch.ascent = height * ascent;
      glyph->u.stretch.height = height;
      glyph->w32_font_type = UNKNOWN_FONT;
      ++it->glyph_row->used[area];
    }
}


/* Produce a stretch glyph for iterator IT.  IT->object is the value
   of the glyph property displayed.  The value must be a list
   `(space KEYWORD VALUE ...)' with the following KEYWORD/VALUE pairs
   being recognized:

   1. `:width WIDTH' specifies that the space should be WIDTH *
   canonical char width wide.  WIDTH may be an integer or floating 
   point number.

   2. `:relative-width FACTOR' specifies that the width of the stretch
   should be computed from the width of the first character having the
   `glyph' property, and should be FACTOR times that width.

   3. `:align-to HPOS' specifies that the space should be wide enough
   to reach HPOS, a value in canonical character units.

   Exactly one of the above pairs must be present.  

   4. `:height HEIGHT' specifies that the height of the stretch produced
   should be HEIGHT, measured in canonical character units.

   5. `:relative-height FACTOR' specifies that the height of the
   stretch should be FACTOR times the height of the characters having
   the glyph property.

   Either none or exactly one of 4 or 5 must be present.

   6. `:ascent ASCENT'  specifies that ASCENT percent of the height
   of the stretch should be used for the ascent of the stretch.
   ASCENT must be in the range 0 <= ASCENT <= 100.  */

#define NUMVAL(X)				\
     ((INTEGERP (X) || FLOATP (X))		\
      ? XFLOATINT (X)				\
      : - 1)


static void
x_produce_stretch_glyph (it)
     struct it *it;
{
  /* (space :width WIDTH :height HEIGHT.  */
#if GLYPH_DEBUG
  extern Lisp_Object Qspace;
#endif
  extern Lisp_Object QCwidth, QCheight, QCascent;
  extern Lisp_Object QCrelative_width, QCrelative_height;
  extern Lisp_Object QCalign_to;
  Lisp_Object prop, plist;
  double width = 0, height = 0, ascent = 0;
  struct face *face = FACE_FROM_ID (it->f, it->face_id);
  XFontStruct *font = face->font ? face->font : FRAME_FONT (it->f);

  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  
  /* List should start with `space'.  */
  xassert (CONSP (it->object) && EQ (XCAR (it->object), Qspace));
  plist = XCDR (it->object);

  /* Compute the width of the stretch.  */
  if (prop = Fplist_get (plist, QCwidth),
      NUMVAL (prop) > 0)
    /* Absolute width `:width WIDTH' specified and valid.  */
    width = NUMVAL (prop) * CANON_X_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_width),
	   NUMVAL (prop) > 0)
    {
      /* Relative width `:relative-width FACTOR' specified and valid.
	 Compute the width of the characters having the `glyph'
	 property.  */
      struct it it2;
      unsigned char *p = BYTE_POS_ADDR (IT_BYTEPOS (*it));
      
      it2 = *it;
      if (it->multibyte_p)
	{
	  int maxlen = ((IT_BYTEPOS (*it) >= GPT ? ZV : GPT)
			- IT_BYTEPOS (*it));
	  it2.c = STRING_CHAR_AND_LENGTH (p, maxlen, it2.len);
	}
      else
	it2.c = *p, it2.len = 1;

      it2.glyph_row = NULL;
      it2.what = IT_CHARACTER;
      x_produce_glyphs (&it2);
      width = NUMVAL (prop) * it2.pixel_width;
    }
  else if (prop = Fplist_get (plist, QCalign_to),
	   NUMVAL (prop) > 0)
    width = NUMVAL (prop) * CANON_X_UNIT (it->f) - it->current_x;
  else
    /* Nothing specified -> width defaults to canonical char width.  */
    width = CANON_X_UNIT (it->f);
  
  /* Compute height.  */
  if (prop = Fplist_get (plist, QCheight),
      NUMVAL (prop) > 0)
    height = NUMVAL (prop) * CANON_Y_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_height),
	   NUMVAL (prop) > 0)
    height = FONT_HEIGHT (font) * NUMVAL (prop);
  else
    height = FONT_HEIGHT (font);

  /* Compute percentage of height used for ascent.  If 
     `:ascent ASCENT' is present and valid, use that.  Otherwise,
     derive the ascent from the font in use.  */
  if (prop = Fplist_get (plist, QCascent),
      NUMVAL (prop) > 0 && NUMVAL (prop) <= 100)
    ascent = NUMVAL (prop) / 100.0;
  else
    ascent = (double) FONT_BASE (font) / FONT_HEIGHT (font);

  if (width <= 0)
    width = 1;
  if (height <= 0)
    height = 1;

  if (it->glyph_row)
    {
      Lisp_Object object = it->stack[it->sp - 1].string;
      if (!STRINGP (object))
	object = it->w->buffer;
      x_append_stretch_glyph (it, object, width, height, ascent);
    }

  it->pixel_width = width;
  it->ascent = it->phys_ascent = height * ascent;
  it->descent = it->phys_descent = height - it->ascent;
  it->nglyphs = 1;

  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }
  
  take_vertical_position_into_account (it);
}

/* Return proper value to be used as baseline offset of font that has
   ASCENT and DESCENT to draw characters by the font at the vertical
   center of the line of frame F.

   Here, out task is to find the value of BOFF in the following figure;

	-------------------------+-----------+-
	 -+-+---------+-+        |           |
	  | |         | |        |           |
	  | |         | |        F_ASCENT    F_HEIGHT
	  | |         | ASCENT   |           |
     HEIGHT |         | |        |           |
	  | |         |-|-+------+-----------|------- baseline
	  | |         | | BOFF   |           |
	  | |---------|-+-+      |           |
	  | |         | DESCENT  |           |
	 -+-+---------+-+        F_DESCENT   |
	-------------------------+-----------+-

	-BOFF + DESCENT + (F_HEIGHT - HEIGHT) / 2 = F_DESCENT
	BOFF = DESCENT +  (F_HEIGHT - HEIGHT) / 2 - F_DESCENT
	DESCENT = FONT->descent
	HEIGHT = FONT_HEIGHT (FONT)
	F_DESCENT = (F->output_data.x->font->descent
		     - F->output_data.x->baseline_offset)
	F_HEIGHT = FRAME_LINE_HEIGHT (F)
*/

#define VCENTER_BASELINE_OFFSET(FONT, F)			\
  (FONT_DESCENT (FONT)						\
   + (FRAME_LINE_HEIGHT ((F)) - FONT_HEIGHT ((FONT))		\
      + (FRAME_LINE_HEIGHT ((F)) > FONT_HEIGHT ((FONT)))) / 2	\
   - (FONT_DESCENT (FRAME_FONT (F)) - FRAME_BASELINE_OFFSET (F)))

/* Produce glyphs/get display metrics for the display element IT is
   loaded with.  See the description of struct display_iterator in
   dispextern.h for an overview of struct display_iterator.  */

static void
x_produce_glyphs (it)
     struct it *it;
{
  it->glyph_not_available_p = 0;

  if (it->what == IT_CHARACTER)
    {
      wchar_t char2b;
      XFontStruct *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      XCharStruct *pcm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;                 /* baseline offset */
      /* We may change it->multibyte_p upon unibyte<->multibyte
	 conversion.  So, save the current value now and restore it
	 later.

	 Note: It seems that we don't have to record multibyte_p in
	 struct glyph because the character code itself tells if or
	 not the character is multibyte.  Thus, in the future, we must
	 consider eliminating the field `multibyte_p' in the struct
	 glyph.
      */
      int saved_multibyte_p = it->multibyte_p;

      /* Maybe translate single-byte characters to multibyte, or the
         other way.  */
      it->char_to_display = it->c;
      if (!ASCII_BYTE_P (it->c))
        {
          if (unibyte_display_via_language_environment
              && SINGLE_BYTE_CHAR_P (it->c)
              && (it->c >= 0240
                  || !NILP (Vnonascii_translation_table)))
            {
              it->char_to_display = unibyte_char_to_multibyte (it->c);
              it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
	  else if (!SINGLE_BYTE_CHAR_P (it->c)
		   && !it->multibyte_p)
	    {
              it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
        }
      
      /* Get font to use.  Encode IT->char_to_display.  */
      x_get_char_face_and_encoding (it->f, it->char_to_display,
                                    it->face_id, &char2b,
                                    it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
        {
          font = FRAME_FONT (it->f);
	  boff = it->f->output_data.w32->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = FONT_INFO_FROM_ID (it->f, face->font_info_id);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      if (it->char_to_display >= ' '
	  && (!it->multibyte_p || it->char_to_display < 128))
	{
	  /* Either unibyte or ASCII.  */
	  int stretched_p;

	  it->nglyphs = 1;

          pcm = w32_per_char_metric (font, &char2b,
                                     font->bdf ? BDF_1D_FONT : ANSI_FONT);
	  it->ascent = FONT_BASE (font) + boff;
	  it->descent = FONT_DESCENT (font) - boff;

          if (pcm)
            {
              it->phys_ascent = pcm->ascent + boff;
              it->phys_descent = pcm->descent - boff;
              it->pixel_width = pcm->width;
            }
          else
            {
              it->glyph_not_available_p = 1;
              it->phys_ascent = FONT_BASE (font) + boff;
              it->phys_descent = FONT_DESCENT (font) - boff;
              it->pixel_width = FONT_WIDTH (font);
            }
          
	  /* If this is a space inside a region of text with
	     `space-width' property, change its width.  */
	  stretched_p = it->char_to_display == ' ' && !NILP (it->space_width);
	  if (stretched_p)
	    it->pixel_width *= XFLOATINT (it->space_width);

	  /* If face has a box, add the box thickness to the character
	     height.  If character has a box line to the left and/or
	     right, add the box line width to the character's width.  */
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;
	      
	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = -thick;
	      
	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }

	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  /* If we have to actually produce glyphs, do it.  */
	  if (it->glyph_row)
	    {
	      if (stretched_p)
		{
		  /* Translate a space with a `space-width' property
		     into a stretch glyph.  */
		  double ascent = (double) FONT_BASE (font)
                                / FONT_HEIGHT (font);
		  x_append_stretch_glyph (it, it->object, it->pixel_width, 
					  it->ascent + it->descent, ascent);
		}
	      else
		x_append_glyph (it);

	      /* If characters with lbearing or rbearing are displayed
		 in this line, record that fact in a flag of the
		 glyph row.  This is used to optimize X output code.  */
	      if (pcm && (pcm->lbearing < 0 || pcm->rbearing > pcm->width))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	}
      else if (it->char_to_display == '\n')
	{
	  /* A newline has no width but we need the height of the line.  */
	  it->pixel_width = 0;
	  it->nglyphs = 0;
	  it->ascent = it->phys_ascent = FONT_BASE (font) + boff;
	  it->descent = it->phys_descent = FONT_DESCENT (font) - boff;
      
	  if (face->box != FACE_NO_BOX
	      && face->box_line_width > 0)
	    {
	      it->ascent += face->box_line_width;
	      it->descent += face->box_line_width;
	    }
	}
      else if (it->char_to_display == '\t')
	{
	  int tab_width = it->tab_width * CANON_X_UNIT (it->f);
	  int x = it->current_x + it->continuation_lines_width;
	  int next_tab_x = ((1 + x + tab_width - 1) / tab_width) * tab_width;
      
	  /* If the distance from the current position to the next tab
	     stop is less than a canonical character width, use the
	     tab stop after that.  */
	  if (next_tab_x - x < CANON_X_UNIT (it->f))
	    next_tab_x += tab_width;

	  it->pixel_width = next_tab_x - x;
	  it->nglyphs = 1;
	  it->ascent = it->phys_ascent = FONT_BASE (font) + boff;
	  it->descent = it->phys_descent = FONT_DESCENT (font) - boff;
	  
	  if (it->glyph_row)
	    {
	      double ascent = (double) it->ascent / (it->ascent + it->descent);
	      x_append_stretch_glyph (it, it->object, it->pixel_width, 
				      it->ascent + it->descent, ascent);
	    }
	}
      else 
	{
	  /* A multi-byte character.
             If we found a font, this font should give us the right
             metrics.  If we didn't find a font, use the frame's
             default font and calculate the width of the character
             from the charset width; this is what old redisplay code
             did.  */
          enum w32_char_font_type type;

          if (font->bdf && CHARSET_DIMENSION (CHAR_CHARSET (it->c)) == 1)
            type = BDF_1D_FONT;
          else if (font->bdf)
            type = BDF_2D_FONT;
          else
            type = UNICODE_FONT;

          pcm = w32_per_char_metric (font, &char2b, type);

	  if (font_not_found_p || !pcm)
	    {
	      int charset = CHAR_CHARSET (it->char_to_display);

	      it->glyph_not_available_p = 1;
	      it->pixel_width = (FONT_WIDTH (FRAME_FONT (it->f))
				 * CHARSET_WIDTH (charset));
	      it->phys_ascent = FONT_BASE (font) + boff;
	      it->phys_descent = FONT_DESCENT (font) - boff;
	    }
	  else
	    {
	      it->pixel_width = pcm->width;
              it->phys_ascent = pcm->ascent + boff;
              it->phys_descent = pcm->descent - boff;
              if (it->glyph_row
                  && (pcm->lbearing < 0
                      || pcm->rbearing > pcm->width))
                it->glyph_row->contains_overlapping_glyphs_p = 1;
            }
          it->nglyphs = 1;
          it->ascent = FONT_BASE (font) + boff;
          it->descent = FONT_DESCENT (font) - boff;
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;

	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = - thick;
	  
	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }
  
	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  if (it->glyph_row)
	    x_append_glyph (it);
	}
      it->multibyte_p = saved_multibyte_p;
    }
  else if (it->what == IT_COMPOSITION)
    {
      /* Note: A composition is represented as one glyph in the
	 glyph matrix.  There are no padding glyphs.  */
      wchar_t char2b;
      XFontStruct *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      XCharStruct *pcm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;			/* baseline offset */
      struct composition *cmp = composition_table[it->cmp_id];

      /* Maybe translate single-byte characters to multibyte.  */
      it->char_to_display = it->c;
      if (unibyte_display_via_language_environment
	  && SINGLE_BYTE_CHAR_P (it->c)
	  && (it->c >= 0240
	      || (it->c >= 0200
		  && !NILP (Vnonascii_translation_table))))
	{
	  it->char_to_display = unibyte_char_to_multibyte (it->c);
	}

      /* Get face and font to use.  Encode IT->char_to_display.  */
      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
      face = FACE_FROM_ID (it->f, it->face_id);
      x_get_char_face_and_encoding (it->f, it->char_to_display,
				    it->face_id, &char2b, it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
	{
	  font = FRAME_FONT (it->f);
	  boff = it->f->output_data.w32->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = FONT_INFO_FROM_ID (it->f, face->font_info_id);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      /* There are no padding glyphs, so there is only one glyph to
	 produce for the composition.  Important is that pixel_width,
	 ascent and descent are the values of what is drawn by
	 draw_glyphs (i.e. the values of the overall glyphs composed).  */
      it->nglyphs = 1;

      /* If we have not yet calculated pixel size data of glyphs of
	 the composition for the current face font, calculate them
	 now.  Theoretically, we have to check all fonts for the
	 glyphs, but that requires much time and memory space.  So,
	 here we check only the font of the first glyph.  This leads
	 to incorrect display very rarely, and C-l (recenter) can
	 correct the display anyway.  */
      if (cmp->font != (void *) font)
	{
	  /* Ascent and descent of the font of the first character of
	     this composition (adjusted by baseline offset).  Ascent
	     and descent of overall glyphs should not be less than
	     them respectively.  */
	  int font_ascent = FONT_BASE (font) + boff;
	  int font_descent = FONT_DESCENT (font) - boff;
	  /* Bounding box of the overall glyphs.  */
	  int leftmost, rightmost, lowest, highest;
	  int i, width, ascent, descent;
          enum w32_char_font_type font_type;

	  cmp->font = (void *) font;

          if (font->bdf && CHARSET_DIMENSION (CHAR_CHARSET (it->c)) == 1)
            font_type = BDF_1D_FONT;
          else if (font->bdf)
            font_type = BDF_2D_FONT;
          else
            font_type = UNICODE_FONT;

	  /* Initialize the bounding box.  */
	  if (font_info
              && (pcm = w32_per_char_metric (font, &char2b, font_type)))
	    {
	      width = pcm->width;
	      ascent = pcm->ascent;
	      descent = pcm->descent;
	    }
	  else
	    {
	      width = FONT_WIDTH (font);
	      ascent = FONT_BASE (font);
	      descent = FONT_DESCENT (font);
	    }
	  
	  rightmost = width;
	  lowest = - descent + boff;
	  highest = ascent + boff;
	  leftmost = 0;
	  
	  if (font_info
	      && font_info->default_ascent
	      && CHAR_TABLE_P (Vuse_default_ascent)
	      && !NILP (Faref (Vuse_default_ascent,
			       make_number (it->char_to_display))))
	    highest = font_info->default_ascent + boff;

	  /* Draw the first glyph at the normal position.  It may be
	     shifted to right later if some other glyphs are drawn at
	     the left.  */
	  cmp->offsets[0] = 0;
	  cmp->offsets[1] = boff;

	  /* Set cmp->offsets for the remaining glyphs.  */
	  for (i = 1; i < cmp->glyph_len; i++)
	    {
	      int left, right, btm, top;
	      int ch = COMPOSITION_GLYPH (cmp, i);
	      int face_id = FACE_FOR_CHAR (it->f, face, ch);

	      face = FACE_FROM_ID (it->f, face_id);
	      x_get_char_face_and_encoding (it->f, ch, face->id, &char2b,
					    it->multibyte_p);
	      font = face->font;
	      if (font == NULL)
		{
		  font = FRAME_FONT (it->f);
		  boff = it->f->output_data.w32->baseline_offset;
		  font_info = NULL;
		}
	      else
		{
		  font_info
		    = FONT_INFO_FROM_ID (it->f, face->font_info_id);
		  boff = font_info->baseline_offset;
		  if (font_info->vertical_centering)
		    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
		}

              if (font->bdf && CHARSET_DIMENSION (CHAR_CHARSET (ch)) == 1)
                font_type = BDF_1D_FONT;
              else if (font->bdf)
                font_type = BDF_2D_FONT;
              else
                font_type = UNICODE_FONT;

	      if (font_info
                  && (pcm = w32_per_char_metric (font, &char2b, font_type)))
		{
		  width = pcm->width;
		  ascent = pcm->ascent;
		  descent = pcm->descent;
		}
	      else
		{
		  width = FONT_WIDTH (font);
		  ascent = 1;
		  descent = 0;
		}

	      if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
		{
		  /* Relative composition with or without
		     alternate chars.  */
		  left = (leftmost + rightmost - width) / 2;
		  btm = - descent + boff;
		  if (font_info && font_info->relative_compose
		      && (! CHAR_TABLE_P (Vignore_relative_composition)
			  || NILP (Faref (Vignore_relative_composition,
					  make_number (ch)))))
		    {

		      if (- descent >= font_info->relative_compose)
			/* One extra pixel between two glyphs.  */
			btm = highest + 1;
		      else if (ascent <= 0)
			/* One extra pixel between two glyphs.  */
			btm = lowest - 1 - ascent - descent;
		    }
		}
	      else
		{
		  /* A composition rule is specified by an integer
		     value that encodes global and new reference
		     points (GREF and NREF).  GREF and NREF are
		     specified by numbers as below:

			0---1---2 -- ascent
			|       |
			|       |
			|       |
			9--10--11 -- center
			|       |
		     ---3---4---5--- baseline
			|       |
			6---7---8 -- descent
		  */
		  int rule = COMPOSITION_RULE (cmp, i);
		  int gref, nref, grefx, grefy, nrefx, nrefy;

		  COMPOSITION_DECODE_RULE (rule, gref, nref);
		  grefx = gref % 3, nrefx = nref % 3;
		  grefy = gref / 3, nrefy = nref / 3;

		  left = (leftmost
			  + grefx * (rightmost - leftmost) / 2
			  - nrefx * width / 2);
		  btm = ((grefy == 0 ? highest
			  : grefy == 1 ? 0
			  : grefy == 2 ? lowest
			  : (highest + lowest) / 2)
			 - (nrefy == 0 ? ascent + descent
			    : nrefy == 1 ? descent - boff
			    : nrefy == 2 ? 0
			    : (ascent + descent) / 2));
		}

	      cmp->offsets[i * 2] = left;
	      cmp->offsets[i * 2 + 1] = btm + descent;

	      /* Update the bounding box of the overall glyphs. */
	      right = left + width;
	      top = btm + descent + ascent;
	      if (left < leftmost)
		leftmost = left;
	      if (right > rightmost)
		rightmost = right;
	      if (top > highest)
		highest = top;
	      if (btm < lowest)
		lowest = btm;
	    }

	  /* If there are glyphs whose x-offsets are negative,
	     shift all glyphs to the right and make all x-offsets
	     non-negative.  */
	  if (leftmost < 0)
	    {
	      for (i = 0; i < cmp->glyph_len; i++)
		cmp->offsets[i * 2] -= leftmost;
	      rightmost -= leftmost;
	    }

	  cmp->pixel_width = rightmost;
	  cmp->ascent = highest;
	  cmp->descent = - lowest;
	  if (cmp->ascent < font_ascent)
	    cmp->ascent = font_ascent;
	  if (cmp->descent < font_descent)
	    cmp->descent = font_descent;
	}

      it->pixel_width = cmp->pixel_width;
      it->ascent = it->phys_ascent = cmp->ascent;
      it->descent = it->phys_descent = cmp->descent;

      if (face->box != FACE_NO_BOX)
	{
	  int thick = face->box_line_width;

	  if (thick > 0)
	    {
	      it->ascent += thick;
	      it->descent += thick;
	    }
	  else
	    thick = - thick;
	  
	  if (it->start_of_box_run_p)
	    it->pixel_width += thick;
	  if (it->end_of_box_run_p)
	    it->pixel_width += thick;
	}
  
      /* If face has an overline, add the height of the overline
	 (1 pixel) and a 1 pixel margin to the character height.  */
      if (face->overline_p)
	it->ascent += 2;

      take_vertical_position_into_account (it);
  
      if (it->glyph_row)
	x_append_composite_glyph (it);
    }
  else if (it->what == IT_IMAGE)
    x_produce_image_glyph (it);
  else if (it->what == IT_STRETCH)
    x_produce_stretch_glyph (it);

  /* Accumulate dimensions.  Note: can't assume that it->descent > 0
     because this isn't true for images with `:ascent 100'.  */
  xassert (it->ascent >= 0 && it->descent >= 0);
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;

  it->descent += it->extra_line_spacing;

  it->max_ascent = max (it->max_ascent, it->ascent);
  it->max_descent = max (it->max_descent, it->descent);
  it->max_phys_ascent = max (it->max_phys_ascent, it->phys_ascent);
  it->max_phys_descent = max (it->max_phys_descent, it->phys_descent);
}


/* Estimate the pixel height of the mode or top line on frame F.
   FACE_ID specifies what line's height to estimate.  */

int
x_estimate_mode_line_height (f, face_id)
     struct frame *f;
     enum face_id face_id;
{
  int height = FONT_HEIGHT (FRAME_FONT (f));

  /* This function is called so early when Emacs starts that the face
     cache and mode line face are not yet initialized.  */
  if (FRAME_FACE_CACHE (f))
      {
	struct face *face = FACE_FROM_ID (f, face_id);
	if (face)
          {
            if (face->font)
              height = FONT_HEIGHT (face->font);
	    if (face->box_line_width > 0)
	      height += 2 * face->box_line_width;
          }
      }
  
  return height;
}


/***********************************************************************
			    Glyph display
 ***********************************************************************/

/* A sequence of glyphs to be drawn in the same face.

   This data structure is not really completely X specific, so it
   could possibly, at least partially, be useful for other systems.  It
   is currently not part of the external redisplay interface because
   it's not clear what other systems will need.  */

struct glyph_string
{
  /* X-origin of the string.  */
  int x;

  /* Y-origin and y-position of the base line of this string.  */
  int y, ybase;

  /* The width of the string, not including a face extension.  */
  int width;

  /* The width of the string, including a face extension.  */
  int background_width;

  /* The height of this string.  This is the height of the line this
     string is drawn in, and can be different from the height of the
     font the string is drawn in.  */
  int height;

  /* Number of pixels this string overwrites in front of its x-origin.
     This number is zero if the string has an lbearing >= 0; it is
     -lbearing, if the string has an lbearing < 0.  */
  int left_overhang;

  /* Number of pixels this string overwrites past its right-most
     nominal x-position, i.e. x + width.  Zero if the string's
     rbearing is <= its nominal width, rbearing - width otherwise.  */
  int right_overhang;

  /* The frame on which the glyph string is drawn.  */
  struct frame *f;

  /* The window on which the glyph string is drawn.  */
  struct window *w;

  /* X display and window for convenience.  */
  Window window;

  /* The glyph row for which this string was built.  It determines the
     y-origin and height of the string.  */
  struct glyph_row *row;

  /* The area within row.  */
  enum glyph_row_area area;

  /* Characters to be drawn, and number of characters.  */
  wchar_t *char2b;
  int nchars;

  /* A face-override for drawing cursors, mouse face and similar.  */
  enum draw_glyphs_face hl;

  /* Face in which this string is to be drawn.  */
  struct face *face;

  /* Font in which this string is to be drawn.  */
  XFontStruct *font;

  /* Font info for this string.  */
  struct font_info *font_info;

  /* Non-null means this string describes (part of) a composition.
     All characters from char2b are drawn composed.  */
  struct composition *cmp;

  /* Index of this glyph string's first character in the glyph
     definition of CMP.  If this is zero, this glyph string describes
     the first character of a composition.  */
  int gidx;

  /* 1 means this glyph strings face has to be drawn to the right end
     of the window's drawing area.  */
  unsigned extends_to_end_of_line_p : 1;

  /* 1 means the background of this string has been drawn.  */
  unsigned background_filled_p : 1;

  /* 1 means glyph string must be drawn with 16-bit functions.  */
  unsigned two_byte_p : 1;

  /* 1 means that the original font determined for drawing this glyph
     string could not be loaded.  The member `font' has been set to
     the frame's default font in this case.  */
  unsigned font_not_found_p : 1;

  /* 1 means that the face in which this glyph string is drawn has a
     stipple pattern.  */
  unsigned stippled_p : 1;

  /* 1 means only the foreground of this glyph string must be drawn,
     and we should use the physical height of the line this glyph
     string appears in as clip rect.  */
  unsigned for_overlaps_p : 1;

  /* The GC to use for drawing this glyph string.  */
  XGCValues *gc;

  HDC hdc;

  /* A pointer to the first glyph in the string.  This glyph
     corresponds to char2b[0].  Needed to draw rectangles if
     font_not_found_p is 1.  */
  struct glyph *first_glyph;

  /* Image, if any.  */
  struct image *img;

  struct glyph_string *next, *prev;
};


/* Encapsulate the different ways of displaying text under W32.  */

static void
w32_text_out (s, x, y,chars,nchars)
     struct glyph_string * s;
     int x, y;
     wchar_t * chars;
     int nchars;
{
  int charset_dim = w32_font_is_double_byte (s->gc->font) ? 2 : 1;
  if (s->gc->font->bdf)
    w32_BDF_TextOut (s->gc->font->bdf, s->hdc,
                     x, y, (char *) chars, charset_dim,
                     nchars * charset_dim, 0);
  else if (s->first_glyph->w32_font_type == UNICODE_FONT)
    ExtTextOutW (s->hdc, x, y, 0, NULL, chars, nchars, NULL);
  else
    ExtTextOutA (s->hdc, x, y, 0, NULL, (char *) chars,
		 nchars * charset_dim, NULL);
}

#if GLYPH_DEBUG

static void
x_dump_glyph_string (s)
     struct glyph_string *s;
{
  fprintf (stderr, "glyph string\n");
  fprintf (stderr, "  x, y, w, h = %d, %d, %d, %d\n",
	   s->x, s->y, s->width, s->height);
  fprintf (stderr, "  ybase = %d\n", s->ybase);
  fprintf (stderr, "  hl = %d\n", s->hl);
  fprintf (stderr, "  left overhang = %d, right = %d\n",
	   s->left_overhang, s->right_overhang);
  fprintf (stderr, "  nchars = %d\n", s->nchars);
  fprintf (stderr, "  extends to end of line = %d\n",
	   s->extends_to_end_of_line_p);
  fprintf (stderr, "  font height = %d\n", FONT_HEIGHT (s->font));
  fprintf (stderr, "  bg width = %d\n", s->background_width);
}

#endif /* GLYPH_DEBUG */



static void x_append_glyph_string_lists P_ ((struct glyph_string **,
					     struct glyph_string **,
					     struct glyph_string *,
					     struct glyph_string *));
static void x_prepend_glyph_string_lists P_ ((struct glyph_string **,
					      struct glyph_string **,
					      struct glyph_string *,
					      struct glyph_string *));
static void x_append_glyph_string P_ ((struct glyph_string **,
				       struct glyph_string **,
				       struct glyph_string *));
static int x_left_overwritten P_ ((struct glyph_string *));
static int x_left_overwriting P_ ((struct glyph_string *));
static int x_right_overwritten P_ ((struct glyph_string *));
static int x_right_overwriting P_ ((struct glyph_string *));
static int x_fill_glyph_string P_ ((struct glyph_string *, int, int, int,
                                    int));
static void w32_init_glyph_string P_ ((struct glyph_string *, HDC hdc,
                                       wchar_t *, struct window *,
                                       struct glyph_row *,
                                       enum glyph_row_area, int, 
                                       enum draw_glyphs_face));
static int x_draw_glyphs P_ ((struct window *, int , struct glyph_row *,
			      enum glyph_row_area, int, int,
			      enum draw_glyphs_face, int));
static void x_set_glyph_string_clipping P_ ((struct glyph_string *));
static void x_set_glyph_string_gc P_ ((struct glyph_string *));
static void x_draw_glyph_string_background P_ ((struct glyph_string *,
						int));
static void x_draw_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_composite_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_glyph_string_box P_ ((struct glyph_string *));
static void x_draw_glyph_string  P_ ((struct glyph_string *));
static void x_compute_glyph_string_overhangs P_ ((struct glyph_string *));
static void x_set_cursor_gc P_ ((struct glyph_string *));
static void x_set_mode_line_face_gc P_ ((struct glyph_string *));
static void x_set_mouse_face_gc P_ ((struct glyph_string *));
static void w32_get_glyph_overhangs P_ ((HDC hdc, struct glyph *,
                                         struct frame *,
                                         int *, int *));
static void x_compute_overhangs_and_x P_ ((struct glyph_string *, int, int));
static int w32_alloc_lighter_color (struct frame *, COLORREF *, double, int);
static void w32_setup_relief_color P_ ((struct frame *, struct relief *,
                                        double, int, COLORREF));
static void x_setup_relief_colors P_ ((struct glyph_string *));
static void x_draw_image_glyph_string P_ ((struct glyph_string *));
static void x_draw_image_relief P_ ((struct glyph_string *));
static void x_draw_image_foreground P_ ((struct glyph_string *));
static void w32_draw_image_foreground_1 P_ ((struct glyph_string *, HBITMAP));
static void x_fill_image_glyph_string P_ ((struct glyph_string *));
static void x_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					   int, int, int));
static void w32_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				    int, int, int, int, RECT *));
static void w32_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				 int, int, int, RECT *));
static void x_fix_overlapping_area P_ ((struct window *, struct glyph_row *,
					enum glyph_row_area));
static int x_fill_stretch_glyph_string P_ ((struct glyph_string *,
					    struct glyph_row *,
					    enum glyph_row_area, int, int));

#if GLYPH_DEBUG
static void x_check_font P_ ((struct frame *, XFontStruct *));
#endif

     
/* Append the list of glyph strings with head H and tail T to the list
   with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the result.  */

static INLINE void
x_append_glyph_string_lists (head, tail, h, t)
     struct glyph_string **head, **tail;
     struct glyph_string *h, *t;
{
  if (h)
    {
      if (*head)
	(*tail)->next = h;
      else
	*head = h;
      h->prev = *tail;
      *tail = t;
    }
}


/* Prepend the list of glyph strings with head H and tail T to the
   list with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the
   result.  */

static INLINE void
x_prepend_glyph_string_lists (head, tail, h, t)
     struct glyph_string **head, **tail;
     struct glyph_string *h, *t;
{
  if (h)
    {
      if (*head)
	(*head)->prev = t;
      else
	*tail = t;
      t->next = *head;
      *head = h;
    }
}


/* Append glyph string S to the list with head *HEAD and tail *TAIL.
   Set *HEAD and *TAIL to the resulting list.  */

static INLINE void
x_append_glyph_string (head, tail, s)
     struct glyph_string **head, **tail;
     struct glyph_string *s;
{
  s->next = s->prev = NULL;
  x_append_glyph_string_lists (head, tail, s, s);
}


/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
x_set_cursor_gc (s)
     struct glyph_string *s;
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.w32->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.w32->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.w32->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      IF_DEBUG (x_check_font (s->f, s->font));
      xgcv.font = s->font;
      mask = GCForeground | GCBackground | GCFont;

      if (FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (NULL, FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (NULL, s->window, mask, &xgcv);

      s->gc = FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */
   
static void
x_set_mouse_face_gc (s)
     struct glyph_string *s;
{     
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = FRAME_W32_DISPLAY_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
  
  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0);
  s->face = FACE_FROM_ID (s->f, face_id);
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  /* If font in this face is same as S->font, use it.  */
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 but font FONT.  */
      XGCValues xgcv;
      unsigned long mask;
      
      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;
      IF_DEBUG (x_check_font (s->f, s->font));
      xgcv.font = s->font;
      mask = GCForeground | GCBackground | GCFont;
      
      if (FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (NULL, FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (NULL, s->window, mask, &xgcv);
      
      s->gc = FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }

  xassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static INLINE void
x_set_mode_line_face_gc (s)
     struct glyph_string *s;
{     
  s->gc = s->face->gc;
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static INLINE void
x_set_glyph_string_gc (s)
     struct glyph_string *s;
{
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);
  
  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      x_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      x_set_cursor_gc (s);
      s->stippled_p = 0;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      x_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }

  /* GC must have been set.  */
  xassert (s->gc != 0);
}


/* Return in *R the clipping rectangle for glyph string S.  */

static void
w32_get_glyph_string_clip_rect (s, r)
     struct glyph_string *s;
     RECT *r;
{
  int r_height, r_width;

  if (s->row->full_width_p)
    {
      /* Draw full-width.  X coordinates are relative to S->w->left.  */
      int canon_x = CANON_X_UNIT (s->f);
      
      r->left = WINDOW_LEFT_MARGIN (s->w) * canon_x;
      r_width = XFASTINT (s->w->width) * canon_x;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (s->f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (s->f) * canon_x;
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (s->f))
	    r->left -= width;
	}
      
      r->left += FRAME_INTERNAL_BORDER_WIDTH (s->f);

      /* Unless displaying a mode or menu bar line, which are always
	 fully visible, clip to the visible part of the row.  */
      if (s->w->pseudo_window_p)
	r_height = s->row->visible_height;
      else
	r_height = s->height;
    }
  else
    {
      /* This is a text line that may be partially visible.  */
      r->left = WINDOW_AREA_TO_FRAME_PIXEL_X (s->w, s->area, 0);
      r_width = window_box_width (s->w, s->area);
      r_height = s->row->visible_height;
    }

  /* If S draws overlapping rows, it's sufficient to use the top and
     bottom of the window for clipping because this glyph string
     intentionally draws over other lines.  */
  if (s->for_overlaps_p)
    {
      r->top = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      r_height = window_text_bottom_y (s->w) - r->top;
    }
  else
    {
      /* Don't use S->y for clipping because it doesn't take partially
	 visible lines into account.  For example, it can be negative for
	 partially visible lines at the top of a window.  */
      if (!s->row->full_width_p
	  && MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (s->w, s->row))
	r->top = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      else
	r->top = max (0, s->row->y);

      /* If drawing a tool-bar window, draw it over the internal border
	 at the top of the window.  */
      if (s->w == XWINDOW (s->f->tool_bar_window))
	r->top -= s->f->output_data.w32->internal_border_width;
    }

  r->top = WINDOW_TO_FRAME_PIXEL_Y (s->w, r->top);

  r->bottom = r->top + r_height;
  r->right = r->left + r_width;
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static INLINE void
x_set_glyph_string_clipping (s)
     struct glyph_string *s;
{
  RECT r;
  w32_get_glyph_string_clip_rect (s, &r);
  w32_set_clip_rectangle (s->hdc, &r);
}


/* Compute left and right overhang of glyph string S.  If S is a glyph
   string for a composition, assume overhangs don't exist.  */

static INLINE void
x_compute_glyph_string_overhangs (s)
     struct glyph_string *s;
{
  /* TODO: Windows does not appear to have a method for
     getting this info without getting the ABC widths for each
     individual character and working it out manually. */
}


/* Compute overhangs and x-positions for glyph string S and its
   predecessors, or successors.  X is the starting x-position for S.
   BACKWARD_P non-zero means process predecessors.  */
   
static void
x_compute_overhangs_and_x (s, x, backward_p)
     struct glyph_string *s;
     int x;
     int backward_p;
{
  if (backward_p)
    {
      while (s)
	{
	  x_compute_glyph_string_overhangs (s);
	  x -= s->width;
	  s->x = x;
	  s = s->prev;
	}
    }
  else
    {
      while (s)
	{
	  x_compute_glyph_string_overhangs (s);
	  s->x = x;
	  x += s->width;
	  s = s->next;
	}
    }
}


/* Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
   frame F.  Overhangs of glyphs other than type CHAR_GLYPH are
   assumed to be zero.  */

static void
w32_get_glyph_overhangs (hdc, glyph, f, left, right)
     HDC hdc;
     struct glyph *glyph;
     struct frame *f;
     int *left, *right;
{
  *left = *right = 0;
  
  if (glyph->type == CHAR_GLYPH)
    {
      XFontStruct *font;
      struct face *face;
      wchar_t char2b;
      XCharStruct *pcm;

      face = x_get_glyph_face_and_encoding (f, glyph, &char2b, NULL);
      font = face->font;

      if (font
          && (pcm = w32_per_char_metric (font, &char2b,
                                         glyph->w32_font_type)))
	{
	  if (pcm->rbearing > pcm->width)
	    *right = pcm->rbearing - pcm->width;
	  if (pcm->lbearing < 0)
	    *left = -pcm->lbearing;
	}
    }
}


static void
x_get_glyph_overhangs (glyph, f, left, right)
     struct glyph *glyph;
     struct frame *f;
     int *left, *right;
{
  HDC hdc = get_frame_dc (f);
  /* Convert to unicode! */
  w32_get_glyph_overhangs (hdc, glyph, f, left, right);
  release_frame_dc (f, hdc);
}


/* Return the index of the first glyph preceding glyph string S that
   is overwritten by S because of S's left overhang.  Value is -1
   if no glyphs are overwritten.  */

static int
x_left_overwritten (s)
     struct glyph_string *s;
{
  int k;
    
  if (s->left_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = s->first_glyph - glyphs;

      for (i = first - 1; i >= 0 && x > -s->left_overhang; --i)
	x -= glyphs[i].pixel_width;

      k = i + 1;
    }
  else
    k = -1;

  return k;
}


/* Return the index of the first glyph preceding glyph string S that
   is overwriting S because of its right overhang.  Value is -1 if no
   glyph in front of S overwrites S.  */

static int
x_left_overwriting (s)
     struct glyph_string *s;
{
  int i, k, x;
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = s->first_glyph - glyphs;

  k = -1;
  x = 0;
  for (i = first - 1; i >= 0; --i)
    {
      int left, right;
      w32_get_glyph_overhangs (s->hdc, glyphs + i, s->f, &left, &right);
      if (x + right > 0)
	k = i;
      x -= glyphs[i].pixel_width;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that is
   not overwritten by S because of S's right overhang.  Value is -1 if
   no such glyph is found.  */

static int
x_right_overwritten (s)
     struct glyph_string *s;
{
  int k = -1;

  if (s->right_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);
      int end = s->row->used[s->area];
      
      for (i = first; i < end && s->right_overhang > x; ++i)
	x += glyphs[i].pixel_width;

      k = i;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that
   overwrites S because of its left overhang.  Value is negative
   if no such glyph is found.  */

static int
x_right_overwriting (s)
     struct glyph_string *s;
{
  int i, k, x;
  int end = s->row->used[s->area];
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);

  k = -1;
  x = 0;
  for (i = first; i < end; ++i)
    {
      int left, right;
      w32_get_glyph_overhangs (s->hdc, glyphs + i, s->f, &left, &right);
      if (x - left < 0)
	k = i;
      x += glyphs[i].pixel_width;
    }

  return k;
}


/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static INLINE void
x_clear_glyph_string_rect (s, x, y, w, h)
     struct glyph_string *s;
     int x, y, w, h;
{
  int real_x = x;
  int real_y = y;
  int real_w = w;
  int real_h = h;
#if 0
  /* Take clipping into account.  */
  if (s->gc->clip_mask == Rect)
    {
      real_x = max (real_x, s->gc->clip_rectangle.left);
      real_y = max (real_y, s->gc->clip_rectangle.top);
      real_w = min (real_w, s->gc->clip_rectangle.right
                    - s->gc->clip_rectangle.left);
      real_h = min (real_h, s->gc->clip_rectangle.bottom
                    - s->gc->clip_rectangle.top);
    }
#endif
  w32_fill_area (s->f, s->hdc, s->gc->background, real_x, real_y,
                 real_w, real_h);
}


/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
x_draw_glyph_string_background (s, force_p)
     struct glyph_string *s;
     int force_p;
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_line_width, 0);

#if 0 /* TODO: stipple */
      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */
	  XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	  XFillRectangle (s->display, s->window, s->gc, s->x,
			  s->y + box_line_width,
			  s->background_width,
			  s->height - 2 * box_line_width);
	  XSetFillStyle (s->display, s->gc, FillSolid);
	  s->background_filled_p = 1;
	}
      else
#endif
        if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
               || s->font->bdf
	       || force_p)
	{
	  x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
				     s->background_width,
				     s->height - 2 * box_line_width);
	  s->background_filled_p = 1;
	}
    }
}


/* Draw the foreground of glyph string S.  */

static void
x_draw_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, x;
  HFONT old_font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  if (s->for_overlaps_p || (s->background_filled_p && s->hl != DRAW_CURSOR))
    SetBkMode (s->hdc, TRANSPARENT);
  else
    SetBkMode (s->hdc, OPAQUE);

  SetTextColor (s->hdc, s->gc->foreground);
  SetBkColor (s->hdc, s->gc->background);
  SetTextAlign (s->hdc, TA_BASELINE | TA_LEFT);

  if (s->font && s->font->hfont)
    old_font = SelectObject (s->hdc, s->font->hfont);

  /* Draw characters of S as rectangles if S's font could not be
     loaded. */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
        {
          struct glyph *g = s->first_glyph + i;

          w32_draw_rectangle (s->hdc, s->gc, x, s->y, g->pixel_width - 1,
                              s->height - 1);
          x += g->pixel_width;
        }
    }
  else
    {
      char *char1b = (char *) s->char2b;
      int boff = s->font_info->baseline_offset;

      if (s->font_info->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (s->font, s->f) - boff;

      /* If we can use 8-bit functions, condense S->char2b.  */
      if (!s->two_byte_p)
        for (i = 0; i < s->nchars; ++i)
          char1b[i] = BYTE2 (s->char2b[i]);

      /* Draw text with TextOut and friends. */
      w32_text_out (s, x, s->ybase - boff, s->char2b, s->nchars);
    }
  if (s->font && s->font->hfont)
    SelectObject (s->hdc, old_font);
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, x;
  HFONT old_font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->gidx is the index of
     the first character drawn for glyphs of this composition.
     S->gidx == 0 means we are drawing the very first character of
     this composition.  */

  SetTextColor (s->hdc, s->gc->foreground);
  SetBkColor (s->hdc, s->gc->background);
  SetBkMode (s->hdc, TRANSPARENT);
  SetTextAlign (s->hdc, TA_BASELINE | TA_LEFT);

  if (s->font && s->font->hfont)
    old_font = SelectObject (s->hdc, s->font->hfont);

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->gidx == 0)
        w32_draw_rectangle (s->hdc, s->gc, x, s->y, s->width - 1,
                            s->height - 1);
    }
  else
    {
      for (i = 0; i < s->nchars; i++, ++s->gidx)
          w32_text_out (s, x + s->cmp->offsets[s->gidx * 2],
                       s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
                       s->char2b + i, 1);
    }
  if (s->font && s->font->hfont)
    SelectObject (s->hdc, old_font);
}


/* Brightness beyond which a color won't have its highlight brightness
   boosted.

   Nominally, highlight colors for `3d' faces are calculated by
   brightening an object's color by a constant scale factor, but this
   doesn't yield good results for dark colors, so for colors who's
   brightness is less than this value (on a scale of 0-255) have to
   use an additional additive factor.

   The value here is set so that the default menu-bar/mode-line color
   (grey75) will not have its highlights changed at all.  */
#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 187


/* Allocate a color which is lighter or darker than *COLOR by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as COLOR, try a color where all RGB
   values have DELTA added.  Return the allocated color in *COLOR.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static int
w32_alloc_lighter_color (f, color, factor, delta)
     struct frame *f;
     COLORREF *color;
     double factor;
     int delta;
{
  COLORREF new;
  long bright;

  /* On Windows, RGB values are 0-255, not 0-65535, so scale delta. */
  delta /= 256;

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  xassert (factor >= 0);
  new = PALETTERGB (min (0xff, factor * GetRValue (*color)),
                    min (0xff, factor * GetGValue (*color)),
                    min (0xff, factor * GetBValue (*color)));

  /* Calculate brightness of COLOR.  */
  bright = (2 * GetRValue (*color) + 3 * GetGValue (*color)
            + GetBValue (*color)) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double)bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;
      
      if (factor < 1)
        new = PALETTERGB (max (0, min (0xff, min_delta - GetRValue (*color))),
                          max (0, min (0xff, min_delta - GetGValue (*color))),
                          max (0, min (0xff, min_delta - GetBValue (*color))));
      else
        new = PALETTERGB (max (0, min (0xff, min_delta + GetRValue (*color))),
                          max (0, min (0xff, min_delta + GetGValue (*color))),
                          max (0, min (0xff, min_delta + GetBValue (*color))));
    }
  
  if (new == *color)
    new = PALETTERGB (max (0, min (0xff, delta + GetRValue (*color))),
                      max (0, min (0xff, delta + GetGValue (*color))),
                      max (0, min (0xff, delta + GetBValue (*color))));

  /* TODO: Map to palette and retry with delta if same? */
  /* TODO: Free colors (if using palette)? */

  if (new == *color)
    return 0;

  *color = new;

  return 1;
}


/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.x->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */
   
static void
w32_setup_relief_color (f, relief, factor, delta, default_pixel)
     struct frame *f;
     struct relief *relief;
     double factor;
     int delta;
     COLORREF default_pixel;
{
  XGCValues xgcv;
  struct w32_output *di = f->output_data.w32;
  unsigned long mask = GCForeground;
  COLORREF pixel;
  COLORREF background = di->relief_background;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  /* TODO: Free colors (if using palette)? */

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (w32_alloc_lighter_color (f, &pixel, factor, delta))
    {
      relief->allocated_p = 1;
      xgcv.foreground = relief->pixel = pixel;
    }
  
  if (relief->gc == 0)
    {
#if 0 /* TODO: stipple */
      xgcv.stipple = dpyinfo->gray;
      mask |= GCStipple;
#endif
      relief->gc = XCreateGC (NULL, FRAME_W32_WINDOW (f), mask, &xgcv);
    }
  else
    XChangeGC (NULL, relief->gc, mask, &xgcv);
}


/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (s)
     struct glyph_string *s;
{
  struct w32_output *di = s->f->output_data.w32;
  COLORREF color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND  (s->img, s->f, 0);
  else
    color = s->gc->background;

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      w32_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
                              WHITE_PIX_DEFAULT (s->f));
      w32_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
                              BLACK_PIX_DEFAULT (s->f));
    }
}


/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the relief
   to draw, it must be >= 0.  RAISED_P non-zero means draw a raised
   relief.  LEFT_P non-zero means draw a relief on the left side of
   the rectangle.  RIGHT_P non-zero means draw a relief on the right
   side of the rectangle.  CLIP_RECT is the clipping rectangle to use
   when drawing.  */

static void
w32_draw_relief_rect (f, left_x, top_y, right_x, bottom_y, width,
                      raised_p, left_p, right_p, clip_rect)
     struct frame *f;
     int left_x, top_y, right_x, bottom_y, left_p, right_p, raised_p;
     RECT *clip_rect;
{
  int i;
  XGCValues gc;
  HDC hdc = get_frame_dc (f);

  if (raised_p)
    gc.foreground = f->output_data.w32->white_relief.gc->foreground;
  else
    gc.foreground = f->output_data.w32->black_relief.gc->foreground;

  w32_set_clip_rectangle (hdc, clip_rect);

  /* Top.  */
  for (i = 0; i < width; ++i)
    w32_fill_area (f, hdc, gc.foreground,
		   left_x + i * left_p, top_y + i,
		   right_x - left_x - i * (left_p + right_p ) + 1, 1);

  /* Left.  */
  if (left_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     left_x + i, top_y + i, 1,
		     bottom_y - top_y - 2 * i + 1);

  if (raised_p)
    gc.foreground = f->output_data.w32->black_relief.gc->foreground;
  else
    gc.foreground = f->output_data.w32->white_relief.gc->foreground;
  
  /* Bottom.  */
  for (i = 0; i < width; ++i)
    w32_fill_area (f, hdc, gc.foreground, 
		   left_x + i * left_p, bottom_y - i,
		   right_x - left_x - i * (left_p + right_p) + 1, 1);

  /* Right.  */
  if (right_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     right_x - i, top_y + i + 1, 1,
		     bottom_y - top_y - 2 * i - 1);

  w32_set_clip_rectangle (hdc, NULL);
  
  release_frame_dc (f, hdc);
}


/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P non-zero means draw a line on the
   left side of the rectangle.  RIGHT_P non-zero means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
w32_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
                   left_p, right_p, clip_rect)
     struct glyph_string *s;
     int left_x, top_y, right_x, bottom_y, width, left_p, right_p;
     RECT *clip_rect;
{
  w32_set_clip_rectangle (s->hdc, clip_rect);
  
  /* Top.  */
  w32_fill_area (s->f, s->hdc, s->face->box_color,
		  left_x, top_y, right_x - left_x + 1, width);

  /* Left.  */
  if (left_p)
    {
      w32_fill_area (s->f, s->hdc, s->face->box_color,
                     left_x, top_y, width, bottom_y - top_y + 1);
    }
  
  /* Bottom.  */
  w32_fill_area (s->f, s->hdc, s->face->box_color,
                 left_x, bottom_y - width + 1, right_x - left_x + 1, width);
  
  /* Right.  */
  if (right_p)
    {
      w32_fill_area (s->f, s->hdc, s->face->box_color,
                     right_x - width + 1, top_y, width, bottom_y - top_y + 1);
    }

  w32_set_clip_rectangle (s->hdc, NULL);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (s)
     struct glyph_string *s;
{
  int width, left_x, right_x, top_y, bottom_y, last_x, raised_p;
  int left_p, right_p;
  struct glyph *last_glyph;
  RECT clip_rect;

  last_x = window_box_right (s->w, s->area);
  if (s->row->full_width_p
      && !s->w->pseudo_window_p)
    {
      last_x += FRAME_X_RIGHT_FRINGE_WIDTH (s->f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (s->f))
	last_x += FRAME_SCROLL_BAR_WIDTH (s->f) * CANON_X_UNIT (s->f);
    }
  
  /* The glyph that may have a right box line.  */
  last_glyph = (s->cmp || s->img
		? s->first_glyph
		: s->first_glyph + s->nchars - 1);

  width = abs (s->face->box_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = ((s->row->full_width_p && s->extends_to_end_of_line_p
	      ? last_x - 1
	      : min (last_x, s->x + s->background_width) - 1));
  top_y = s->y;
  bottom_y = top_y + s->height - 1;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));
  
  w32_get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    w32_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
                       left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      w32_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
                            width, raised_p, left_p, right_p, &clip_rect);
    }
}


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (s)
     struct glyph_string *s;
{
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;

  SaveDC (s->hdc);

  if (s->img->pixmap)
    {
#if 0 /* TODO: image mask */
      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;
	  XRectangle clip_rect, image_rect, r;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x;
	  xgcv.clip_y_origin = y;
	  xgcv.function = GXcopy;
	  XChangeGC (s->display, s->gc, mask, &xgcv);
	  
	  w32_get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->img->width;
	  image_rect.height = s->img->height;
	  if (IntersectRect (&r, &clip_rect, &image_rect))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       r.x - x, r.y - y, r.width, r.height, r.x, r.y);
	}
      else
#endif
	{
          HDC compat_hdc = CreateCompatibleDC (s->hdc);
          HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
          HBRUSH orig_brush = SelectObject (s->hdc, fg_brush);
          HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);
          x_set_glyph_string_clipping (s);

          SetTextColor (s->hdc, s->gc->foreground);
          SetBkColor (s->hdc, s->gc->background);
#if 0 /* From w32bdf.c (which is from Meadow).  */
          BitBlt (s->hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, SRCCOPY);
          BitBlt (s->hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, 0xB8074A);
#else
          BitBlt (s->hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, 0xE20746);
#endif
          SelectObject (s->hdc, orig_brush);
          DeleteObject (fg_brush);
	  SelectObject (compat_hdc, orig_obj);
          DeleteDC (compat_hdc);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
            w32_draw_rectangle (s->hdc, s->gc, x, y, s->img->width - 1,
                                s->img->height - 1);
          w32_set_clip_rectangle (s->hdc, NULL);
	}
    }
  else
    w32_draw_rectangle (s->hdc, s->gc, x, y, s->img->width -1,
                        s->img->height - 1);

  RestoreDC (s->hdc ,-1);
}



/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (s)
     struct glyph_string *s;
{
  int x0, y0, x1, y1, thick, raised_p;
  RECT r;
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);
 
  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;
  
  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;
  
  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      thick = tool_bar_button_relief >= 0 ? tool_bar_button_relief : DEFAULT_TOOL_BAR_BUTTON_RELIEF;
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = abs (s->img->relief);
      raised_p = s->img->relief > 0;
    }
  
  x0 = x - thick;
  y0 = y - thick;
  x1 = x + s->img->width + thick - 1;
  y1 = y + s->img->height + thick - 1;
  
  x_setup_relief_colors (s);
  w32_get_glyph_string_clip_rect (s, &r);
  w32_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p, 1, 1, &r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
w32_draw_image_foreground_1 (s, pixmap)
     struct glyph_string *s;
     HBITMAP pixmap;
{
  HDC hdc = CreateCompatibleDC (s->hdc);
  HGDIOBJ orig_hdc_obj = SelectObject (hdc, pixmap);
  int x;
  int y = s->ybase - s->y - image_ascent (s->img, s->face);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = abs (s->face->box_line_width);
  else
    x = 0;

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;

  if (s->img->pixmap)
    {
#if 0 /* TODO: image mask */
      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x;
	  xgcv.clip_y_origin = y;
	  xgcv.function = GXcopy;
	  XChangeGC (s->display, s->gc, mask, &xgcv);

	  XCopyArea (s->display, s->img->pixmap, pixmap, s->gc,
		     0, 0, s->img->width, s->img->height, x, y);
	  XSetClipMask (s->display, s->gc, None);
	}
      else
#endif
	{
          HDC compat_hdc = CreateCompatibleDC (hdc);
          HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
          HBRUSH orig_brush = SelectObject (hdc, fg_brush);
          HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);

          SetTextColor (hdc, s->gc->foreground);
          SetBkColor (hdc, s->gc->background);
#if 0 /* From w32bdf.c (which is from Meadow).  */
          BitBlt (hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, SRCCOPY);
          BitBlt (hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, 0xB8074A);
#else
          BitBlt (hdc, x, y, s->img->width, s->img->height,
                  compat_hdc, 0, 0, 0xE20746);
#endif
          SelectObject (hdc, orig_brush);
          DeleteObject (fg_brush);
	  SelectObject (compat_hdc, orig_obj);
          DeleteDC (compat_hdc);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
            w32_draw_rectangle (hdc, s->gc, x, y, s->img->width - 1,
                                s->img->height - 1);
	}
    }
  else
    w32_draw_rectangle (hdc, s->gc, x, y, s->img->width - 1,
                        s->img->height - 1);

  SelectObject (hdc, orig_hdc_obj);
  DeleteDC (hdc);
}


/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (s, x, y, w, h)
     struct glyph_string *s;
     int x, y, w, h;
{
#if 0 /* TODO: stipple */
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
      XFillRectangle (s->display, s->window, s->gc, x, y, w, h);
      XSetFillStyle (s->display, s->gc, FillSolid);
    }
  else
#endif
    x_clear_glyph_string_rect (s, x, y, w, h);
}


/* Draw image glyph string S.  

            s->y
   s->x      +-------------------------
	     |   s->face->box
	     |
	     |     +-------------------------
	     |     |  s->img->vmargin
	     |     |
	     |     |       +-------------------
	     |     |       |  the image

 */

static void
x_draw_image_glyph_string (s)
     struct glyph_string *s;
{
  int x, y;
  int box_line_hwidth = abs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;
  HBITMAP pixmap = 0;

  height = s->height - 2 * box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->img->height
      || s->img->hmargin
      || s->img->vmargin
#if 0 /* TODO: image mask */
      || s->img->mask
#endif
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (box_line_hwidth && s->first_glyph->left_box_line_p)
	x = s->x + box_line_hwidth;
      else
	x = s->x;
      
      y = s->y + box_line_vwidth;
#if 0 /* TODO: image mask */
      if (s->img->mask)
	{
	  /* Create a pixmap as large as the glyph string.  Fill it
	     with the background color.  Copy the image to it, using
	     its mask.  Copy the temporary pixmap to the display.  */
	  Screen *screen = FRAME_X_SCREEN (s->f);
	  int depth = DefaultDepthOfScreen (screen);

	  /* Create a pixmap as large as the glyph string.  */
 	  pixmap = XCreatePixmap (s->display, s->window,
				  s->background_width,
				  s->height, depth);
	  
	  /* Don't clip in the following because we're working on the
	     pixmap.  */
	  XSetClipMask (s->display, s->gc, None);

	  /* Fill the pixmap with the background color/stipple.  */
	  if (s->stippled_p)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetFillStyle (s->display, s->gc, FillSolid);
	    }
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground | GCBackground,
			    &xgcv);
	      XSetForeground (s->display, s->gc, xgcv.background);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}
      else
#endif
	x_draw_glyph_string_bg_rect (s, x, y, s->background_width, height);
      
      s->background_filled_p = 1;
    }

  /* Draw the foreground.  */
  if (pixmap != 0)
    {
      w32_draw_image_foreground_1 (s, pixmap);
      x_set_glyph_string_clipping (s);
      {
        HDC compat_hdc = CreateCompatibleDC (s->hdc);
        HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
        HBRUSH orig_brush = SelectObject (s->hdc, fg_brush);
        HGDIOBJ orig_obj = SelectObject (compat_hdc, pixmap);

        SetTextColor (s->hdc, s->gc->foreground);
        SetBkColor (s->hdc, s->gc->background);
#if 0 /* From w32bdf.c (which is from Meadow).  */
        BitBlt (s->hdc, s->x, s->y, s->background_width, s->height,
                compat_hdc, 0, 0, SRCCOPY);
        BitBlt (s->hdc, s->x, s->y, s->background_width, s->height,
                compat_hdc, 0, 0, 0xB8074A);
#else
        BitBlt (s->hdc, s->x, s->y, s->background_width, s->height,
                compat_hdc, 0, 0, 0xE20746);
#endif
        SelectObject (s->hdc, orig_brush);
        DeleteObject (fg_brush);
        SelectObject (compat_hdc, orig_obj);
        DeleteDC (compat_hdc);
      }
      DeleteObject (pixmap);
      pixmap = 0;
    }
  else
    x_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    x_draw_image_relief (s);
}


/* Draw stretch glyph string S.  */

static void
x_draw_stretch_glyph_string (s)
     struct glyph_string *s;
{
  xassert (s->first_glyph->type == STRETCH_GLYPH);
  s->stippled_p = s->face->stipple != 0;

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-block-cursor' is nil, don't draw a block cursor
	 as wide as the stretch glyph.  */
      int width = min (CANON_X_UNIT (s->f), s->background_width);

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, s->x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < s->background_width)
	{
	  XGCValues *gc = s->face->gc;
	  int x = s->x + width, y = s->y;
	  int w = s->background_width - width, h = s->height;
	  RECT r;
          HDC hdc = s->hdc;

	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;
  
	  w32_get_glyph_string_clip_rect (s, &r);
	  w32_set_clip_rectangle (hdc, &r);

#if 0 /* TODO: stipple */
	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, gc, FillOpaqueStippled);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetFillStyle (s->display, gc, FillSolid);
	    }
	  else
#endif
            {
              w32_fill_area (s->f, s->hdc, gc->background, x, y, w, h);
            }
        }
    }
  else if (!s->background_filled_p)
    x_draw_glyph_string_bg_rect (s, s->x, s->y, s->background_width,
				 s->height);
  
  s->background_filled_p = 1;
}


/* Draw glyph string S.  */

static void
x_draw_glyph_string (s)
     struct glyph_string *s;
{
  int relief_drawn_p = 0;

  /* If S draws into the background of its successor, draw the
     background of the successor first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps_p)
    {
      xassert (s->next->img == NULL);
      x_set_glyph_string_gc (s->next);
      x_set_glyph_string_clipping (s->next);
      x_draw_glyph_string_background (s->next, 1);
    }

  /* Set up S->gc, set clipping and draw S.  */
  x_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps_p
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      x_set_glyph_string_clipping (s);
      x_draw_glyph_string_background (s, 1);
      x_draw_glyph_string_box (s);
      x_set_glyph_string_clipping (s);
      relief_drawn_p = 1;
    }
  else
    x_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      x_draw_image_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      x_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps_p)
	s->background_filled_p = 1;
      else
        x_draw_glyph_string_background (s, 0);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps_p || s->gidx > 0)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 1);
      x_draw_composite_glyph_string_foreground (s);
      break;

    default:
      abort ();
    }

  if (!s->for_overlaps_p)
    {
      /* Draw underline.  */
      if (s->face->underline_p
          && (s->font->bdf || !s->font->tm.tmUnderlined))
        {
          unsigned long h = 1;
          unsigned long dy = s->height - h;

	  /* TODO: Use font information for positioning and thickness
	     of underline.  See OUTLINETEXTMETRIC, and xterm.c.  */
          if (s->face->underline_defaulted_p)
            {
              w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x,
                             s->y + dy, s->width, 1);
            }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->underline_color, s->x,
                             s->y + dy, s->width, 1);
            }
        }

      /* Draw overline.  */
      if (s->face->overline_p)
        {
          unsigned long dy = 0, h = 1;

          if (s->face->overline_color_defaulted_p)
        {
          w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x,
                         s->y + dy, s->width, h);
        }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->underline_color, s->x,
                             s->y + dy, s->width, h);
            }
        }

      /* Draw strike-through.  */
      if (s->face->strike_through_p
          && (s->font->bdf || !s->font->tm.tmStruckOut))
        {
          unsigned long h = 1;
          unsigned long dy = (s->height - h) / 2;

          if (s->face->strike_through_color_defaulted_p)
            {
              w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x, s->y + dy,
                             s->width, h);
            }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->underline_color, s->x,
                             s->y + dy, s->width, h);
            }
        }

      /* Draw relief.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
        x_draw_glyph_string_box (s);
    }

  /* Reset clipping.  */
  w32_set_clip_rectangle (s->hdc, NULL);
}


static int x_fill_composite_glyph_string P_ ((struct glyph_string *,
					      struct face **, int));


/* Fill glyph string S with composition components specified by S->cmp.
   
   FACES is an array of faces for all components of this composition.
   S->gidx is the index of the first component for S.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of a component not in S.  */

static int
x_fill_composite_glyph_string (s, faces, overlaps_p)
     struct glyph_string *s;
     struct face **faces;
     int overlaps_p;
{
  int i;

  xassert (s);

  s->for_overlaps_p = overlaps_p;

  s->face = faces[s->gidx];
  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);

  /* For all glyphs of this composition, starting at the offset
     S->gidx, until we reach the end of the definition or encounter a
     glyph that requires the different face, add it to S.  */
  ++s->nchars;
  for (i = s->gidx + 1; i < s->cmp->glyph_len && faces[i] == s->face; ++i)
    ++s->nchars;

  /* All glyph strings for the same composition has the same width,
     i.e. the width set for the first component of the composition.  */

  s->width = s->first_glyph->pixel_width;

  /* If the specified font could not be loaded, use the frame's
     default font, but record the fact that we couldn't load it in
     the glyph string so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
  
  xassert (s->face && s->face->gc);

  /* This glyph string must always be drawn with 16-bit functions.  */
  s->two_byte_p = 1;

  return s->gidx + s->nchars;
}


/* Fill glyph string S from a sequence of character glyphs.
   
   FACE_ID is the face id of the string.  START is the index of the
   first glyph to consider, END is the index of the last + 1.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of the first glyph not in S.  */

static int
x_fill_glyph_string (s, face_id, start, end, overlaps_p)
     struct glyph_string *s;
     int face_id;
     int start, end, overlaps_p;
{
  struct glyph *glyph, *last;
  int voffset;
  int glyph_not_available_p;

  xassert (s->f == XFRAME (s->w->frame));
  xassert (s->nchars == 0);
  xassert (start >= 0 && end > start);

  s->for_overlaps_p = overlaps_p;
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  voffset = glyph->voffset;

  glyph_not_available_p = glyph->glyph_not_available_p;

  while (glyph < last
	 && glyph->type == CHAR_GLYPH
	 && glyph->voffset == voffset
	 /* Same face id implies same font, nowadays.  */
	 && glyph->face_id == face_id
         && glyph->glyph_not_available_p == glyph_not_available_p)
    {
      int two_byte_p;

      s->face = x_get_glyph_face_and_encoding (s->f, glyph,
					       s->char2b + s->nchars,
                                               &two_byte_p);
      s->two_byte_p = two_byte_p;
      ++s->nchars;
      xassert (s->nchars <= end - start);
      s->width += glyph->pixel_width;
      ++glyph;
    }

  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);
  
  /* If the specified font could not be loaded, use the frame's font,
     but record the fact that we couldn't load it in
     S->font_not_found_p so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL || glyph_not_available_p)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;
  
  xassert (s->face && s->face->gc);
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from image glyph S->first_glyph.  */

static void
x_fill_image_glyph_string (s)
     struct glyph_string *s;
{
  xassert (s->first_glyph->type == IMAGE_GLYPH);
  s->img = IMAGE_FROM_ID (s->f, s->first_glyph->u.img_id);
  xassert (s->img);
  s->face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
  s->font = s->face->font;
  s->width = s->first_glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
}


/* Fill glyph string S from a sequence of stretch glyphs.

   ROW is the glyph row in which the glyphs are found, AREA is the
   area within the row.  START is the index of the first glyph to
   consider, END is the index of the last + 1.

   Value is the index of the first glyph not in S.  */

static int
x_fill_stretch_glyph_string (s, row, area, start, end)
     struct glyph_string *s;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start, end;
{
  struct glyph *glyph, *last;
  int voffset, face_id;
  
  xassert (s->first_glyph->type == STRETCH_GLYPH);
  
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  face_id = glyph->face_id;
  s->face = FACE_FROM_ID (s->f, face_id);
  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);
  s->width = glyph->pixel_width;
  voffset = glyph->voffset;

  for (++glyph;
       (glyph < last
	&& glyph->type == STRETCH_GLYPH
	&& glyph->voffset == voffset
	&& glyph->face_id == face_id);
       ++glyph)
    s->width += glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  xassert (s->face);
  return glyph - s->row->glyphs[s->area];
}


/* Initialize glyph string S.  CHAR2B is a suitably allocated vector
   of XChar2b structures for S; it can't be allocated in
   x_init_glyph_string because it must be allocated via `alloca'.  W
   is the window on which S is drawn.  ROW and AREA are the glyph row
   and area within the row from which S is constructed.  START is the
   index of the first glyph structure covered by S.  HL is a
   face-override for drawing S.  */
   
static void
w32_init_glyph_string (s, hdc, char2b, w, row, area, start, hl)
     struct glyph_string *s;
     HDC hdc;
     wchar_t *char2b;
     struct window *w;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start;
     enum draw_glyphs_face hl;
{
  bzero (s, sizeof *s);
  s->w = w;
  s->f = XFRAME (w->frame);
  s->hdc = hdc;
  s->window = FRAME_W32_WINDOW (s->f);
  s->char2b = char2b;
  s->hl = hl;
  s->row = row;
  s->area = area;
  s->first_glyph = row->glyphs[area] + start;
  s->height = row->height;
  s->y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  /* Display the internal border below the tool-bar window.  */
  if (s->w == XWINDOW (s->f->tool_bar_window))
    s->y -= s->f->output_data.w32->internal_border_width;
  
  s->ybase = s->y + row->ascent;
}


/* Set background width of glyph string S.  START is the index of the
   first glyph following S.  LAST_X is the right-most x-position + 1
   in the drawing area.  */

static INLINE void
x_set_glyph_string_background_width (s, start, last_x)
     struct glyph_string *s;
     int start;
     int last_x;
{
  /* If the face of this glyph string has to be drawn to the end of
     the drawing area, set S->extends_to_end_of_line_p.  */
  struct face *default_face = FACE_FROM_ID (s->f, DEFAULT_FACE_ID);
  
  if (start == s->row->used[s->area]
      && s->area == TEXT_AREA
      && ((s->hl == DRAW_NORMAL_TEXT
	   && (s->row->fill_line_p
	       || s->face->background != default_face->background
	       || s->face->stipple != default_face->stipple
	       || s->row->mouse_face_p))
	  || s->hl == DRAW_MOUSE_FACE
	  || ((s->hl == DRAW_IMAGE_RAISED || s->hl == DRAW_IMAGE_SUNKEN)
	      && s->row->fill_line_p)))
    s->extends_to_end_of_line_p = 1;
  
  /* If S extends its face to the end of the line, set its
     background_width to the distance to the right edge of the drawing
     area.  */
  if (s->extends_to_end_of_line_p)
    s->background_width = last_x - s->x + 1;
  else
    s->background_width = s->width;
}


/* Add a glyph string for a stretch glyph to the list of strings
   between HEAD and TAIL.  START is the index of the stretch glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

#define BUILD_STRETCH_GLYPH_STRING(hdc, W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X)		    \
     do									    \
       {								    \
	 s = (struct glyph_string *) alloca (sizeof *s);		    \
	 w32_init_glyph_string (s, hdc, NULL, W, ROW, AREA, START, HL);	    \
	 START = x_fill_stretch_glyph_string (s, ROW, AREA, START, END);	    \
	 x_append_glyph_string (&HEAD, &TAIL, s);			    \
         s->x = (X);							    \
       }								    \
     while (0)


/* Add a glyph string for an image glyph to the list of strings
   between HEAD and TAIL.  START is the index of the image glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

#define BUILD_IMAGE_GLYPH_STRING(hdc, W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X)			\
     do									\
       {								\
	 s = (struct glyph_string *) alloca (sizeof *s);		\
	 w32_init_glyph_string (s, hdc, NULL, W, ROW, AREA, START, HL); \
	 x_fill_image_glyph_string (s);					\
	 x_append_glyph_string (&HEAD, &TAIL, s);			\
	 ++START;							\
         s->x = (X);							\
       }								\
     while (0)


/* Add a glyph string for a sequence of character glyphs to the list
   of strings between HEAD and TAIL.  START is the index of the first
   glyph in row area AREA of glyph row ROW that is part of the new
   glyph string.  END is the index of the last glyph in that glyph row
   area.  X is the current output position assigned to the new glyph
   string constructed.  HL overrides that face of the glyph; e.g. it
   is DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the
   right-most x-position of the drawing area.  */

#define BUILD_CHAR_GLYPH_STRINGS(hdc, W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)			   \
     do									   \
       {								   \
	 int c, face_id;					   \
	 wchar_t *char2b;						   \
									   \
	 c = (ROW)->glyphs[AREA][START].u.ch;				   \
	 face_id = (ROW)->glyphs[AREA][START].face_id;			   \
									   \
	 s = (struct glyph_string *) alloca (sizeof *s);		   \
	 char2b = (wchar_t *) alloca ((END - START) * sizeof *char2b);	   \
	 w32_init_glyph_string (s, hdc, char2b, W, ROW, AREA, START, HL);  \
	 x_append_glyph_string (&HEAD, &TAIL, s);			   \
	 s->x = (X);							   \
	 START = x_fill_glyph_string (s, face_id, START, END,		   \
                                          OVERLAPS_P);			   \
       }								   \
     while (0)
     

/* Add a glyph string for a composite sequence to the list of strings
   between HEAD and TAIL.  START is the index of the first glyph in
   row area AREA of glyph row ROW that is part of the new glyph
   string.  END is the index of the last glyph in that glyph row area.
   X is the current output position assigned to the new glyph string
   constructed.  HL overrides that face of the glyph; e.g. it is
   DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the right-most
   x-position of the drawing area.  */

#define BUILD_COMPOSITE_GLYPH_STRING(hdc, W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)	  \
  do {									  \
    int cmp_id = (ROW)->glyphs[AREA][START].u.cmp_id;			  \
    int face_id = (ROW)->glyphs[AREA][START].face_id;			  \
    struct face *base_face = FACE_FROM_ID (XFRAME (w->frame), face_id);	  \
    struct composition *cmp = composition_table[cmp_id];		  \
    int glyph_len = cmp->glyph_len;					  \
    wchar_t *char2b;							  \
    struct face **faces;						  \
    struct glyph_string *first_s = NULL;				  \
    int n;								  \
    									  \
    base_face = base_face->ascii_face;					  \
    char2b = (wchar_t *) alloca ((sizeof *char2b) * glyph_len);		  \
    faces = (struct face **) alloca ((sizeof *faces) * glyph_len);	  \
    /* At first, fill in `char2b' and `faces'.  */			  \
    for (n = 0; n < glyph_len; n++)					  \
      {									  \
	int c = COMPOSITION_GLYPH (cmp, n);				  \
	int this_face_id = FACE_FOR_CHAR (XFRAME (w->frame), base_face, c); \
	faces[n] = FACE_FROM_ID (XFRAME (w->frame), this_face_id);	  \
	x_get_char_face_and_encoding (XFRAME (w->frame), c,		  \
				      this_face_id, char2b + n, 1);	  \
      }									  \
    									  \
    /* Make glyph_strings for each glyph sequence that is drawable by	  \
       the same face, and append them to HEAD/TAIL.  */			  \
    for (n = 0; n < cmp->glyph_len;)					  \
           {								  \
	s = (struct glyph_string *) alloca (sizeof *s);			  \
	w32_init_glyph_string (s, hdc, char2b + n, W, ROW, AREA, START, HL); \
	x_append_glyph_string (&(HEAD), &(TAIL), s);			  \
	s->cmp = cmp;							  \
	s->gidx = n;							  \
	s->x = (X);							  \
									  \
	if (n == 0)							  \
	  first_s = s;							  \
									  \
	n = x_fill_composite_glyph_string (s, faces, OVERLAPS_P);	  \
      }									  \
    									  \
    ++START;								  \
    s = first_s;							  \
  } while (0)


/* Build a list of glyph strings between HEAD and TAIL for the glyphs
   of AREA of glyph row ROW on window W between indices START and END.
   HL overrides the face for drawing glyph strings, e.g. it is
   DRAW_CURSOR to draw a cursor.  X and LAST_X are start and end
   x-positions of the drawing area.

   This is an ugly monster macro construct because we must use alloca
   to allocate glyph strings (because x_draw_glyphs can be called
   asynchronously).  */

#define BUILD_GLYPH_STRINGS(hdc, W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)			   \
     do									   \
       {								   \
	 HEAD = TAIL = NULL;						   \
	 while (START < END)						   \
	   {								   \
             struct glyph *first_glyph = (ROW)->glyphs[AREA] + START;	   \
             switch (first_glyph->type)					   \
	       {							   \
	       case CHAR_GLYPH:						   \
                 BUILD_CHAR_GLYPH_STRINGS (hdc, W, ROW, AREA, START, END,  \
					   HEAD, TAIL, HL, X, LAST_X,	   \
                                           OVERLAPS_P);			   \
		 break;							   \
									   \
               case COMPOSITE_GLYPH:                                       \
                 BUILD_COMPOSITE_GLYPH_STRING (hdc, W, ROW, AREA, START,   \
                                               END, HEAD, TAIL, HL, X,     \
                                               LAST_X, OVERLAPS_P);        \
		 break;							   \
									   \
	       case STRETCH_GLYPH:					   \
		 BUILD_STRETCH_GLYPH_STRING (hdc, W, ROW, AREA, START, END,\
					     HEAD, TAIL, HL, X, LAST_X);   \
		 break;							   \
									   \
	       case IMAGE_GLYPH:					   \
		 BUILD_IMAGE_GLYPH_STRING (hdc, W, ROW, AREA, START, END,  \
					   HEAD, TAIL, HL, X, LAST_X);	   \
		 break;							   \
									   \
	       default:							   \
		 abort ();						   \
	       }							   \
									   \
             x_set_glyph_string_background_width (s, START, LAST_X);	   \
	     (X) += s->width;						   \
            }								   \
       }								   \
     while (0)


/* Draw glyphs between START and END in AREA of ROW on window W,
   starting at x-position X.  X is relative to AREA in W.  HL is a
   face-override with the following meaning:

   DRAW_NORMAL_TEXT	draw normally
   DRAW_CURSOR		draw in cursor face
   DRAW_MOUSE_FACE	draw in mouse face.
   DRAW_INVERSE_VIDEO	draw in mode line face
   DRAW_IMAGE_SUNKEN	draw an image with a sunken relief around it
   DRAW_IMAGE_RAISED	draw an image with a raised relief around it

   If OVERLAPS_P is non-zero, draw only the foreground of characters
   and clip to the physical height of ROW.

   Value is the x-position reached, relative to AREA of W.  */
     
static int
x_draw_glyphs (w, x, row, area, start, end, hl, overlaps_p)
     struct window *w;
     int x;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start, end;
     enum draw_glyphs_face hl;
     int overlaps_p;
{
  struct glyph_string *head, *tail;
  struct glyph_string *s;
  int last_x, area_width;
  int x_reached;
  int i, j;
  HDC hdc = get_frame_dc (XFRAME (WINDOW_FRAME (w)));

  /* Let's rather be paranoid than getting a SEGV.  */
  end = min (end, row->used[area]);
  start = max (0, start);
  start = min (end, start);

  /* Translate X to frame coordinates.  Set last_x to the right
     end of the drawing area.  */
  if (row->full_width_p)
    {
      /* X is relative to the left edge of W, without scroll bars
	 or fringes.  */
      struct frame *f = XFRAME (WINDOW_FRAME (w));
      int window_left_x = WINDOW_LEFT_MARGIN (w) * CANON_X_UNIT (f);

      x += window_left_x;
      area_width = XFASTINT (w->width) * CANON_X_UNIT (f);
      last_x = window_left_x + area_width;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f);
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
	    last_x += width;
	  else
	    x -= width;
	}

      x += FRAME_INTERNAL_BORDER_WIDTH (f);
      last_x -= FRAME_INTERNAL_BORDER_WIDTH (f);
    }
  else
    {
      x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, x);
      area_width = window_box_width (w, area);
      last_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, area_width);
    }

  /* Build a doubly-linked list of glyph_string structures between
     head and tail from what we have to draw.  Note that the macro
     BUILD_GLYPH_STRINGS will modify its start parameter.  That's
     the reason we use a separate variable `i'.  */
  i = start;
  BUILD_GLYPH_STRINGS (hdc, w, row, area, i, end, head, tail, hl, x, last_x,
                       overlaps_p);
  if (tail)
    x_reached = tail->x + tail->background_width;
  else
    x_reached = x;

  /* If there are any glyphs with lbearing < 0 or rbearing > width in
     the row, redraw some glyphs in front or following the glyph
     strings built above.  */
  if (head && !overlaps_p && row->contains_overlapping_glyphs_p)
    {
      int dummy_x = 0;
      struct glyph_string *h, *t;

      /* Compute overhangs for all glyph strings.  */
      for (s = head; s; s = s->next)
	x_compute_glyph_string_overhangs (s);

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that are overwritten because of the first glyph
	 string's left overhang.  The background of all strings
	 prepended must be drawn because the first glyph string 
	 draws over it.  */
      i = x_left_overwritten (head);
      if (i >= 0)
	{
	  j = i;
	  BUILD_GLYPH_STRINGS (hdc, w, row, area, j, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
                               overlaps_p);
	  start = i;
	  x_compute_overhangs_and_x (t, head->x, 1);
	  x_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that overwrite that glyph string because of their
	 right overhang.  For these strings, only the foreground must
	 be drawn, because it draws over the glyph string at `head'.
	 The background must not be drawn because this would overwrite
	 right overhangs of preceding glyphs for which no glyph
	 strings exist.  */
      i = x_left_overwriting (head);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (hdc, w, row, area, i, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
                               overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  x_compute_overhangs_and_x (t, head->x, 1);
	  x_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyphs strings for glyphs following the last glyph
	 string tail that are overwritten by tail.  The background of
	 these strings has to be drawn because tail's foreground draws
	 over it.  */
      i = x_right_overwritten (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (hdc, w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
                               overlaps_p);
	  x_compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  x_append_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyph strings for glyphs following the last glyph
	 string tail that overwrite tail.  The foreground of such
	 glyphs has to be drawn because it writes into the background
	 of tail.  The background must not be drawn because it could
	 paint over the foreground of following glyphs.  */
      i = x_right_overwriting (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (hdc, w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
                               overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  x_compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  x_append_glyph_string_lists (&head, &tail, h, t);
	}
    }

  /* Draw all strings.  */
  for (s = head; s; s = s->next)
    x_draw_glyph_string (s);

  if (area == TEXT_AREA
      && !row->full_width_p
      /* When drawing overlapping rows, only the glyph strings'
	 foreground is drawn, which doesn't erase a cursor
	 completely. */
      && !overlaps_p)
    {
      int x0 = head ? head->x : x;
      int x1 = tail ? tail->x + tail->background_width : x;
      
      x0 = FRAME_TO_WINDOW_PIXEL_X (w, x0);
      x1 = FRAME_TO_WINDOW_PIXEL_X (w, x1);
      
      if (!row->full_width_p && XFASTINT (w->left_margin_width) != 0)
	{
	  int left_area_width = window_box_width (w, LEFT_MARGIN_AREA);
	  x0 -= left_area_width;
	  x1 -= left_area_width;
	}

      notice_overwritten_cursor (w, area, x0, x1,
				 row->y, MATRIX_ROW_BOTTOM_Y (row));
    }

  /* Value is the x-position up to which drawn, relative to AREA of W.
     This doesn't include parts drawn because of overhangs.  */
  x_reached = FRAME_TO_WINDOW_PIXEL_X (w, x_reached);
  if (!row->full_width_p)
    {
      if (area > LEFT_MARGIN_AREA)
	x_reached -= window_box_width (w, LEFT_MARGIN_AREA);
      if (area > TEXT_AREA)
	x_reached -= window_box_width (w, TEXT_AREA);
    }

  release_frame_dc (XFRAME (WINDOW_FRAME (w)), hdc);

  return x_reached;
}


/* Fix the display of area AREA of overlapping row ROW in window W.  */

static void
x_fix_overlapping_area (w, row, area)
     struct window *w;
     struct glyph_row *row;
     enum glyph_row_area area;
{
  int i, x;
  
  BLOCK_INPUT;
  
  if (area == LEFT_MARGIN_AREA)
    x = 0;
  else if (area == TEXT_AREA)
    x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
  else
    x = (window_box_width (w, LEFT_MARGIN_AREA)
	 + window_box_width (w, TEXT_AREA));

  for (i = 0; i < row->used[area];)
    {
      if (row->glyphs[area][i].overlaps_vertically_p)
	{
	  int start = i, start_x = x;

	  do
	    {
	      x += row->glyphs[area][i].pixel_width;
	      ++i;
	    }
	  while (i < row->used[area]
		 && row->glyphs[area][i].overlaps_vertically_p);

	  x_draw_glyphs (w, start_x, row, area, start, i,
			 DRAW_NORMAL_TEXT, 1);
	}
      else
	{
	  x += row->glyphs[area][i].pixel_width;
	  ++i;
	}
    }
  
  UNBLOCK_INPUT;
}


/* Output LEN glyphs starting at START at the nominal cursor position.
   Advance the nominal cursor over the text.  The global variable
   updated_window contains the window being updated, updated_row is
   the glyph row being updated, and updated_area is the area of that
   row being updated.  */

static void
x_write_glyphs (start, len)
     struct glyph *start;
     int len;
{
  int x, hpos;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  
  /* Write glyphs.  */

  hpos = start - updated_row->glyphs[updated_area];
  x = x_draw_glyphs (updated_window, output_cursor.x,
		     updated_row, updated_area,
		     hpos, hpos + len,
		     DRAW_NORMAL_TEXT, 0);

  UNBLOCK_INPUT;
  
  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x = x;
}


/* Insert LEN glyphs from START at the nominal cursor position.   */

static void
x_insert_glyphs (start, len)
     struct glyph *start;
     register int len;
{
  struct frame *f;
  struct window *w;
  int line_height, shift_by_width, shifted_region_width;
  struct glyph_row *row;
  struct glyph *glyph;
  int frame_x, frame_y, hpos;
  HDC hdc;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  w = updated_window;
  f = XFRAME (WINDOW_FRAME (w));
  hdc = get_frame_dc (f);

  /* Get the height of the line we are in.  */
  row = updated_row;
  line_height = row->height;

  /* Get the width of the glyphs to insert.  */
  shift_by_width = 0;
  for (glyph = start; glyph < start + len; ++glyph)
    shift_by_width += glyph->pixel_width;

  /* Get the width of the region to shift right.  */
  shifted_region_width = (window_box_width (w, updated_area)
			  - output_cursor.x
			  - shift_by_width);

  /* Shift right.  */
  frame_x = window_box_left (w, updated_area) + output_cursor.x;
  frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, output_cursor.y);
  BitBlt (hdc, frame_x + shift_by_width, frame_y,
          shifted_region_width, line_height,
          hdc, frame_x, frame_y, SRCCOPY);

  /* Write the glyphs.  */
  hpos = start - row->glyphs[updated_area];
  x_draw_glyphs (w, output_cursor.x, row, updated_area, hpos, hpos + len,
		 DRAW_NORMAL_TEXT, 0);

  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x += shift_by_width;
  release_frame_dc (f, hdc);

  UNBLOCK_INPUT;
}


/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (n)
     register int n;
{
  struct frame *f;

  if (updating_frame)
    f = updating_frame;
  else
    f = SELECTED_FRAME ();

  if (! FRAME_W32_P (f))
    return;

  abort ();
}


/* Erase the current text line from the nominal cursor position
   (inclusive) to pixel column TO_X (exclusive).  The idea is that
   everything from TO_X onward is already erased.

   TO_X is a pixel position relative to updated_area of
   updated_window.  TO_X == -1 means clear to the end of this area.  */

static void
x_clear_end_of_line (to_x)
     int to_x;
{
  struct frame *f;
  struct window *w = updated_window;
  int max_x, min_y, max_y;
  int from_x, from_y, to_y;
  
  xassert (updated_window && updated_row);
  f = XFRAME (w->frame);
  
  if (updated_row->full_width_p)
    {
      max_x = XFASTINT (w->width) * CANON_X_UNIT (f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
	  && !w->pseudo_window_p)
	max_x += FRAME_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f);
    }
  else
    max_x = window_box_width (w, updated_area);
  max_y = window_text_bottom_y (w);

  /* TO_X == 0 means don't do anything.  TO_X < 0 means clear to end
     of window.  For TO_X > 0, truncate to end of drawing area.  */
  if (to_x == 0)
    return;
  else if (to_x < 0)
    to_x = max_x;
  else
    to_x = min (to_x, max_x);

  to_y = min (max_y, output_cursor.y + updated_row->height);
  
  /* Notice if the cursor will be cleared by this operation.  */
  if (!updated_row->full_width_p)
    notice_overwritten_cursor (w, updated_area,
			       output_cursor.x, -1,
			       updated_row->y,
			       MATRIX_ROW_BOTTOM_Y (updated_row));

  from_x = output_cursor.x;

  /* Translate to frame coordinates.  */
  if (updated_row->full_width_p)
    {
      from_x = WINDOW_TO_FRAME_PIXEL_X (w, from_x);
      to_x = WINDOW_TO_FRAME_PIXEL_X (w, to_x);
    }
  else
    {
      from_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, from_x);
      to_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, to_x);
    }
  
  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, max (min_y, output_cursor.y));
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, to_y);
  
  /* Prevent inadvertently clearing to end of the X window.  */
  if (to_x > from_x && to_y > from_y)
    {
      HDC hdc;
      BLOCK_INPUT;
      hdc = get_frame_dc (f);

      w32_clear_area (f, hdc, from_x, from_y, to_x - from_x, to_y - from_y);
      release_frame_dc (f, hdc);
      UNBLOCK_INPUT;
    }
}


/* Clear entire frame.  If updating_frame is non-null, clear that
   frame.  Otherwise clear the selected frame.  */

static void
x_clear_frame ()
{
  struct frame *f;

  if (updating_frame)
    f = updating_frame;
  else
    f = SELECTED_FRAME ();

  if (! FRAME_W32_P (f))
    return;

  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  /* We don't set the output cursor here because there will always
     follow an explicit cursor_to.  */
  BLOCK_INPUT;

  w32_clear_window (f);

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  UNBLOCK_INPUT;
}


/* Make audible bell.  */

static void
w32_ring_bell (void)
{
  struct frame *f;

  f = SELECTED_FRAME ();

  BLOCK_INPUT;

  if (FRAME_W32_P (f) && visible_bell)
    {
      int i;
      HWND hwnd = FRAME_W32_WINDOW (SELECTED_FRAME ());

      for (i = 0; i < 5; i++) 
	{
	  FlashWindow (hwnd, TRUE);
	  Sleep (10);
	}
      FlashWindow (hwnd, FALSE);
    }
  else
      w32_sys_ring_bell ();

  UNBLOCK_INPUT;
}


/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to x_update_begin and x_update_end.  */

static void
w32_set_terminal_window (n)
     register int n;
{
  /* This function intentionally left blank.  */
}



/***********************************************************************
			      Line Dance
 ***********************************************************************/

/* Perform an insert-lines or delete-lines operation, inserting N
   lines or deleting -N lines at vertical position VPOS.  */

static void
x_ins_del_lines (vpos, n)
     int vpos, n;
{
  struct frame *f;

  if (updating_frame)
    f = updating_frame;
  else
    f = SELECTED_FRAME ();

  if (! FRAME_W32_P (f))
    return;

  abort ();
}


/* Scroll part of the display as described by RUN.  */

static void
x_scroll_run (w, run)
     struct window *w;
     struct run *run;
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;
  HWND hwnd = FRAME_W32_WINDOW (f);
  HRGN expect_dirty;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringes of W.  */
  window_box (w, -1, &x, &y, &width, &height);
  width += FRAME_X_FRINGE_WIDTH (f);
  x -= FRAME_X_LEFT_FRINGE_WIDTH (f);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y + height, x + width, bottom_y);
    }
  else
    {
      /* Scolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y, x + width, to_y);
    }

  BLOCK_INPUT;
  
  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  updated_window = w;
  x_clear_cursor (w);

  {
    RECT from;
    RECT to;
    HRGN dirty = CreateRectRgn (0, 0, 0, 0);
    HRGN combined = CreateRectRgn (0, 0, 0, 0);

    from.left = to.left = x;
    from.right = to.right = x + width;
    from.top = from_y;
    from.bottom = from_y + height;
    to.top = y;
    to.bottom = bottom_y;

    ScrollWindowEx (hwnd, 0, to_y - from_y, &from, &to, dirty,
		    NULL, SW_INVALIDATE);

    /* Combine this with what we expect to be dirty. This covers the
       case where not all of the region we expect is actually dirty.  */
    CombineRgn (combined, dirty, expect_dirty, RGN_OR);

    /* If the dirty region is not what we expected, redraw the entire frame.  */
    if (!EqualRgn (combined, expect_dirty))
      SET_FRAME_GARBAGED (f);
  }

  UNBLOCK_INPUT;
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/
									
/* Redisplay an exposed area of frame F.  X and Y are the upper-left
   corner of the exposed rectangle.  W and H are width and height of
   the exposed area.  All are pixel values.  W or H zero means redraw
   the entire frame.  */

static void
expose_frame (f, x, y, w, h)
     struct frame *f;
     int x, y, w, h;
{
  RECT r;
  int mouse_face_overwritten_p = 0;

  TRACE ((stderr, "expose_frame "));

  /* No need to redraw if frame will be redrawn soon.  */
  if (FRAME_GARBAGED_P (f))
    {
      TRACE ((stderr, " garbaged\n"));
      return;
    }

  /* If basic faces haven't been realized yet, there is no point in
     trying to redraw anything.  This can happen when we get an expose
     event while Emacs is starting, e.g. by moving another window.  */
  if (FRAME_FACE_CACHE (f) == NULL
      || FRAME_FACE_CACHE (f)->used < BASIC_FACE_ID_SENTINEL)
    {
      TRACE ((stderr, " no faces\n"));
      return;
    }

  if (w == 0 || h == 0)
    {
      r.left = r.top = 0;
      r.right = CANON_X_UNIT (f) * f->width;
      r.bottom = CANON_Y_UNIT (f) * f->height;
    }
  else
    {
      r.left = x;
      r.top = y;
      r.right = x + w;
      r.bottom = y + h;
    }

  TRACE ((stderr, "(%d, %d, %d, %d)\n", r.left, r.top, r.right, r.bottom));
  mouse_face_overwritten_p = expose_window_tree (XWINDOW (f->root_window), &r);

  if (WINDOWP (f->tool_bar_window))
    mouse_face_overwritten_p
      |= expose_window (XWINDOW (f->tool_bar_window), &r);

  /* Some window managers support a focus-follows-mouse style with
     delayed raising of frames.  Imagine a partially obscured frame,
     and moving the mouse into partially obscured mouse-face on that
     frame.  The visible part of the mouse-face will be highlighted,
     then the WM raises the obscured frame.  With at least one WM, KDE
     2.1, Emacs is not getting any event for the raising of the frame
     (even tried with SubstructureRedirectMask), only Expose events.
     These expose events will draw text normally, i.e. not
     highlighted.  Which means we must redo the highlight here.
     Subsume it under ``we love X''.  --gerd 2001-08-15  */
  /* Included in Windows version because Windows most likely does not
     do the right thing if any third party tool offers
     focus-follows-mouse with delayed raise.  --jason 2001-10-12  */
  if (mouse_face_overwritten_p && !FRAME_GARBAGED_P (f))
    {
      struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
      if (f == dpyinfo->mouse_face_mouse_frame)
	{
	  int x = dpyinfo->mouse_face_mouse_x;
	  int y = dpyinfo->mouse_face_mouse_y;
	  clear_mouse_face (dpyinfo);
	  note_mouse_highlight (f, x, y);
	}
    }
}


/* Redraw (parts) of all windows in the window tree rooted at W that
   intersect R.  R contains frame pixel coordinates.  */

static int
expose_window_tree (w, r)
     struct window *w;
     RECT *r;
{
  struct frame *f = XFRAME (w->frame);
  int mouse_face_overwritten_p = 0;

  while (w && !FRAME_GARBAGED_P (f))
    {
      if (!NILP (w->hchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->hchild), r);
      else if (!NILP (w->vchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->vchild), r);
      else
	mouse_face_overwritten_p |= expose_window (w, r);

      w = NILP (w->next) ? NULL : XWINDOW (w->next);
    }

  return mouse_face_overwritten_p;
}


/* Redraw the part of glyph row area AREA of glyph row ROW on window W
   which intersects rectangle R.  R is in window-relative coordinates.  */

static void
expose_area (w, row, r, area)
     struct window *w;
     struct glyph_row *row;
     RECT *r;
     enum glyph_row_area area;
{
  struct glyph *first = row->glyphs[area];
  struct glyph *end = row->glyphs[area] + row->used[area];
  struct glyph *last;
  int first_x, start_x, x;

  if (area == TEXT_AREA && row->fill_line_p)
    /* If row extends face to end of line write the whole line.  */
    x_draw_glyphs (w, 0, row, area,
		   0, row->used[area],
		   DRAW_NORMAL_TEXT, 0);
  else
    {
      /* Set START_X to the window-relative start position for drawing glyphs of
	 AREA.  The first glyph of the text area can be partially visible.
	 The first glyphs of other areas cannot.  */
      if (area == LEFT_MARGIN_AREA)
	start_x = 0;
      else if (area == TEXT_AREA)
	start_x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
      else
	start_x = (window_box_width (w, LEFT_MARGIN_AREA)
		   + window_box_width (w, TEXT_AREA));
      x = start_x;

      /* Find the first glyph that must be redrawn.  */
      while (first < end
             && x + first->pixel_width < r->left)
        {
          x += first->pixel_width;
          ++first;
        }
  
      /* Find the last one.  */
      last = first;
      first_x = x;
      while (last < end
             && x < r->right)
        {
          x += last->pixel_width;
          ++last;
        }

      /* Repaint.  */
      if (last > first)
        x_draw_glyphs (w, first_x - start_x, row, area,
                       first - row->glyphs[area],
                       last - row->glyphs[area],
                       DRAW_NORMAL_TEXT, 0);
    }
}


/* Redraw the parts of the glyph row ROW on window W intersecting
   rectangle R.  R is in window-relative coordinates.  Value is
   non-zero if mouse face was overwritten.  */

static int
expose_line (w, row, r)
     struct window *w;
     struct glyph_row *row;
     RECT *r;
{
  xassert (row->enabled_p);
  
  if (row->mode_line_p || w->pseudo_window_p)
    x_draw_glyphs (w, 0, row, TEXT_AREA, 0, row->used[TEXT_AREA],
		   DRAW_NORMAL_TEXT, 0);
  else
    {
      if (row->used[LEFT_MARGIN_AREA])
	expose_area (w, row, r, LEFT_MARGIN_AREA);
      if (row->used[TEXT_AREA])
	expose_area (w, row, r, TEXT_AREA);
      if (row->used[RIGHT_MARGIN_AREA])
	expose_area (w, row, r, RIGHT_MARGIN_AREA);
      x_draw_row_fringe_bitmaps (w, row);
    }

  return row->mouse_face_p;
}


/* Return non-zero if W's cursor intersects rectangle R.  */

static int
x_phys_cursor_in_rect_p (w, r)
     struct window *w;
     RECT *r;
{
  RECT cr, result;
  struct glyph *cursor_glyph;

  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph)
    {
      cr.left = w->phys_cursor.x;
      cr.top = w->phys_cursor.y;
      cr.right = cr.left + cursor_glyph->pixel_width;
      cr.bottom = cr.top + w->phys_cursor_height;
      return IntersectRect (&result, &cr, r);
    }
  else
    return 0;
}


/* Redraw the part of window W intersection rectagle FR.  Pixel
   coordinates in FR are frame relative.  Call this function with
   input blocked.  Value is non-zero if the exposure overwrites
   mouse-face.  */

static int
expose_window (w, fr)
     struct window *w;
     RECT *fr;
{
  struct frame *f = XFRAME (w->frame);
  RECT wr, r;
  int mouse_face_overwritten_p = 0;

  /* If window is not yet fully initialized, do nothing.  This can
     happen when toolkit scroll bars are used and a window is split.
     Reconfiguring the scroll bar will generate an expose for a newly
     created window.  */
  if (w->current_matrix == NULL)
    return 0;

  /* When we're currently updating the window, display and current
     matrix usually don't agree.  Arrange for a thorough display
     later.  */
  if (w == updated_window)
    {
      SET_FRAME_GARBAGED (f);
      return 0;
    }

  /* Frame-relative pixel rectangle of W.  */
  wr.left = XFASTINT (w->left) * CANON_X_UNIT (f);
  wr.top = XFASTINT (w->top) * CANON_Y_UNIT (f);
  wr.right = wr.left + XFASTINT (w->width) * CANON_X_UNIT (f);
  wr.bottom = wr.top + XFASTINT (w->height) * CANON_Y_UNIT (f);

  if (IntersectRect(&r, fr, &wr))
    {
      int yb = window_text_bottom_y (w);
      struct glyph_row *row;
      int cursor_cleared_p;

      TRACE ((stderr, "expose_window (%d, %d, %d, %d)\n",
	      r.left, r.top, r.right, r.bottom));

      /* Convert to window coordinates.  */
      r.left = FRAME_TO_WINDOW_PIXEL_X (w, r.left);
      r.right = FRAME_TO_WINDOW_PIXEL_X (w, r.right);
      r.top = FRAME_TO_WINDOW_PIXEL_Y (w, r.top);
      r.bottom = FRAME_TO_WINDOW_PIXEL_Y (w, r.bottom);

      /* Turn off the cursor.  */
      if (!w->pseudo_window_p
	  && x_phys_cursor_in_rect_p (w, &r))
	{
	  x_clear_cursor (w);
	  cursor_cleared_p = 1;
	}
      else
	cursor_cleared_p = 0;

      /* Find the first row intersecting the rectangle R.  */
      for (row = w->current_matrix->rows;
	   row->enabled_p;
	   ++row)
	{
	  int y0 = row->y;
	  int y1 = MATRIX_ROW_BOTTOM_Y (row);

	  if ((y0 >= r.top && y0 < r.bottom)
	      || (y1 > r.top && y1 < r.bottom)
	      || (r.top >= y0 && r.top < y1)
	      || (r.bottom > y0 && r.bottom < y1))
	    {
	      if (expose_line (w, row, &r))
		mouse_face_overwritten_p = 1;
	    }

	  if (y1 >= yb)
	    break;
	}

      /* Display the mode line if there is one.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && (row = MATRIX_MODE_LINE_ROW (w->current_matrix),
	      row->enabled_p)
	  && row->y < r.bottom)
	{
	  if (expose_line (w, row, &r))
	    mouse_face_overwritten_p = 1;
	}

      if (!w->pseudo_window_p)
	{
	  /* Draw border between windows.  */
	  x_draw_vertical_border (w);

	  /* Turn the cursor on again.  */
	  if (cursor_cleared_p)
	    x_update_window_cursor (w, 1);
	}
    }

  return mouse_face_overwritten_p;
}


static void
frame_highlight (f)
     struct frame *f;
{
  x_update_cursor (f, 1);
}

static void
frame_unhighlight (f)
     struct frame *f;
{
  x_update_cursor (f, 1);
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (dpyinfo, frame)
     struct w32_display_info *dpyinfo;
     struct frame *frame;
{
  struct frame *old_focus = dpyinfo->w32_focus_frame;

  if (frame != dpyinfo->w32_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of w32_focus_frame.  */
      dpyinfo->w32_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

      if (dpyinfo->w32_focus_frame && dpyinfo->w32_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->w32_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  x_new_focus_frame (dpyinfo, dpyinfo->w32_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
w32_frame_rehighlight (frame)
     struct frame *frame;
{
  if (! FRAME_W32_P (frame))
    return;
  x_frame_rehighlight (FRAME_W32_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  struct frame *old_highlight = dpyinfo->w32_highlight_frame;

  if (dpyinfo->w32_focus_frame)
    {
      dpyinfo->w32_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame))
	   : dpyinfo->w32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->w32_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame) = Qnil;
	  dpyinfo->w32_highlight_frame = dpyinfo->w32_focus_frame;
	}
    }
  else
    dpyinfo->w32_highlight_frame = 0;

  if (dpyinfo->w32_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->w32_highlight_frame)
	frame_highlight (dpyinfo->w32_highlight_frame);
    }
}

/* Keyboard processing - modifier keys, etc. */

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (keysym)
    int keysym;
{
  /* Make static so we can always return it */
  static char value[100];

  BLOCK_INPUT;
  GetKeyNameText (keysym, value, 100);
  UNBLOCK_INPUT;

  return value;
}



/* Mouse clicks and mouse movement.  Rah.  */

/* Given a pixel position (PIX_X, PIX_Y) on frame F, return glyph
   co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle that the
   glyph at X, Y occupies, if BOUNDS != 0.  If NOCLIP is non-zero, do
   not force the value into range.  */

void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     RECT *bounds;
     int noclip;
{
  /* Support tty mode: if Vwindow_system is nil, behave correctly. */
  if (NILP (Vwindow_system))
    {
      *x = pix_x;
      *y = pix_y;
      return;
    }

  /* Arrange for the division in PIXEL_TO_CHAR_COL etc. to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH (FRAME_FONT (f)) - 1;
  if (pix_y < 0)
    pix_y -= (f)->output_data.w32->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->left = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->top = CHAR_TO_PIXEL_ROW (f, pix_y);
      bounds->right  = bounds->left + FONT_WIDTH  (FRAME_FONT (f)) - 1;
      bounds->bottom = bounds->top + f->output_data.w32->line_height - 1;
    }

  if (!noclip)
    {
      if (pix_x < 0)
	pix_x = 0;
      else if (pix_x > FRAME_WINDOW_WIDTH (f))
	pix_x = FRAME_WINDOW_WIDTH (f);

      if (pix_y < 0)
	pix_y = 0;
      else if (pix_y > f->height)
	pix_y = f->height;
    }

  *x = pix_x;
  *y = pix_y;
}


/* Given HPOS/VPOS in the current matrix of W, return corresponding
   frame-relative pixel positions in *FRAME_X and *FRAME_Y.  If we
   can't tell the positions because W's display is not up to date,
   return 0.  */

int
glyph_to_pixel_coords (w, hpos, vpos, frame_x, frame_y)
     struct window *w;
     int hpos, vpos;
     int *frame_x, *frame_y;
{
  int success_p;

  xassert (hpos >= 0 && hpos < w->current_matrix->matrix_w);
  xassert (vpos >= 0 && vpos < w->current_matrix->matrix_h);

  if (display_completed)
    {
      struct glyph_row *row = MATRIX_ROW (w->current_matrix, vpos);
      struct glyph *glyph = row->glyphs[TEXT_AREA];
      struct glyph *end = glyph + min (hpos, row->used[TEXT_AREA]);

      *frame_y = row->y;
      *frame_x = row->x;
      while (glyph < end)
	{
	  *frame_x += glyph->pixel_width;
	  ++glyph;
	}

      success_p = 1;
    }
  else
    {
      *frame_y = *frame_x = 0;
      success_p = 0;
    }

  *frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, *frame_y);
  *frame_x = WINDOW_TO_FRAME_PIXEL_X (w, *frame_x);
  return success_p;
}

/* Parse a button MESSAGE. The button index is returned in PBUTTON, and
   the state in PUP. XBUTTON provides extra information for extended mouse
   button messages. Returns FALSE if unable to parse the message.  */
BOOL 
parse_button (message, xbutton, pbutton, pup)
     int message;
     int xbutton;
     int * pbutton;
     int * pup;
{
  int button = 0;
  int up = 0;
  
  switch (message)
    {
    case WM_LBUTTONDOWN:
      button = 0;
      up = 0;
      break;
    case WM_LBUTTONUP:
      button = 0;
      up = 1;
      break;
    case WM_MBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 0;
      break;
    case WM_MBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 1;
      break;
    case WM_RBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 0;
      break;
    case WM_RBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 1;
      break;
    case WM_XBUTTONDOWN:
      button = xbutton + 2;
      up = 0;
      break;
    case WM_XBUTTONUP:
      button = xbutton + 2;
      up = 1;
      break;
    default:
      return (FALSE);
    }
  
  if (pup) *pup = up;
  if (pbutton) *pbutton = button;
  
  return (TRUE);
}


/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  int button;
  int up;

  parse_button (msg->msg.message, HIWORD (msg->msg.wParam),
		&button, &up);

  /* Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = mouse_click;
  result->code = button;
  result->timestamp = msg->msg.time;
  result->modifiers = (msg->dwModifiers
		       | (up
			  ? up_modifier
			  : down_modifier));

  XSETINT (result->x, LOWORD (msg->msg.lParam));
  XSETINT (result->y, HIWORD (msg->msg.lParam));
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

static Lisp_Object
construct_mouse_wheel (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  POINT p;
  result->kind = mouse_wheel;
  result->code = (short) HIWORD (msg->msg.wParam);
  result->timestamp = msg->msg.time;
  result->modifiers = msg->dwModifiers;
  p.x = LOWORD (msg->msg.lParam);
  p.y = HIWORD (msg->msg.lParam);
  ScreenToClient (msg->msg.hwnd, &p);
  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

static Lisp_Object
construct_drag_n_drop (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  Lisp_Object files;
  Lisp_Object frame;
  HDROP hdrop;
  POINT p;
  WORD num_files;
  char *name;
  int i, len;

  result->kind = drag_n_drop;
  result->code = 0;
  result->timestamp = msg->msg.time;
  result->modifiers = msg->dwModifiers;

  hdrop = (HDROP) msg->msg.wParam;
  DragQueryPoint (hdrop, &p);

#if 0
  p.x = LOWORD (msg->msg.lParam);
  p.y = HIWORD (msg->msg.lParam);
  ScreenToClient (msg->msg.hwnd, &p);
#endif

  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);

  num_files = DragQueryFile (hdrop, 0xFFFFFFFF, NULL, 0);
  files = Qnil;

  for (i = 0; i < num_files; i++)
    {
      len = DragQueryFile (hdrop, i, NULL, 0);
      if (len <= 0)
	continue;
      name = alloca (len + 1);
      DragQueryFile (hdrop, i, name, len + 1);
      files = Fcons (build_string (name), files);
    }

  DragFinish (hdrop);

  XSETFRAME (frame, f);
  result->frame_or_window = Fcons (frame, files);
  result->arg = Qnil;
  return Qnil;
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static MSG last_mouse_motion_event;
static Lisp_Object last_mouse_motion_frame;

static void remember_mouse_glyph P_ ((struct frame *, int, int));

static void
note_mouse_movement (frame, msg)
     FRAME_PTR frame;
     MSG *msg;
{
  int mouse_x = LOWORD (msg->lParam);
  int mouse_y = HIWORD (msg->lParam);

  last_mouse_movement_time = msg->time;
  memcpy (&last_mouse_motion_event, msg, sizeof (last_mouse_motion_event));
  XSETFRAME (last_mouse_motion_frame, frame);

#if 0 /* Calling Lisp asynchronously is not safe.  */
  if (autoselect_window_p)
    {
      int area;
      Lisp_Object window;
      static Lisp_Object last_window;

      window = window_from_coordinates (frame, mouse_x, mouse_y, &area, 0);

      /* Window will be selected only when it is not selected now and
	 last mouse movement event was not in it.  Minibuffer window
	 will be selected iff it is active.  */
      if (!EQ (window, last_window)
	  && !EQ (window, selected_window)
	  && (!MINI_WINDOW_P (XWINDOW (window))
	      || (EQ (window, minibuf_window) && minibuf_level > 0)))
	Fselect_window (window);

      last_window=window;
    }
#endif

  if (msg->hwnd != FRAME_W32_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (mouse_x < last_mouse_glyph.left
	   || mouse_x > last_mouse_glyph.right
	   || mouse_y < last_mouse_glyph.top
	   || mouse_y > last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, mouse_x, mouse_y);
      /* Remember the mouse position here, as w32_mouse_position only
	 gets called when mouse tracking is enabled but we also need
	 to keep track of the mouse for help_echo and highlighting at
	 other times.  */
      remember_mouse_glyph (frame, mouse_x, mouse_y);
    }
}


/************************************************************************
			      Mouse Face
 ************************************************************************/

/* Find the glyph under window-relative coordinates X/Y in window W.
   Consider only glyphs from buffer text, i.e. no glyphs from overlay
   strings.  Return in *HPOS and *VPOS the row and column number of
   the glyph found.  Return in *AREA the glyph area containing X.
   Value is a pointer to the glyph found or null if X/Y is not on
   text, or we can't tell because W's current matrix is not up to
   date.  */

static struct glyph *
x_y_to_hpos_vpos (w, x, y, hpos, vpos, area, buffer_only_p)
     struct window *w;
     int x, y;
     int *hpos, *vpos, *area;
     int buffer_only_p;
{
  struct glyph *glyph, *end;
  struct glyph_row *row = NULL;
  int x0, i, left_area_width;

  /* Find row containing Y.  Give up if some row is not enabled.  */
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      row = MATRIX_ROW (w->current_matrix, i);
      if (!row->enabled_p)
	return NULL;
      if (y >= row->y && y < MATRIX_ROW_BOTTOM_Y (row))
	break;
    }

  *vpos = i;
  *hpos = 0;

  /* Give up if Y is not in the window.  */
  if (i == w->current_matrix->nrows)
    return NULL;

  /* Get the glyph area containing X.  */
  if (w->pseudo_window_p)
    {
      *area = TEXT_AREA;
      x0 = 0;
    }
  else
    {
      left_area_width = window_box_width (w, LEFT_MARGIN_AREA);
      if (x < left_area_width)
	{
	  *area = LEFT_MARGIN_AREA;
	  x0 = 0;
	}
      else if (x < left_area_width + window_box_width (w, TEXT_AREA))
	{
	  *area = TEXT_AREA;
	  x0 = row->x + left_area_width;
	}
      else
	{
	  *area = RIGHT_MARGIN_AREA;
	  x0 = left_area_width + window_box_width (w, TEXT_AREA);
	}
    }

  /* Find glyph containing X.  */
  glyph = row->glyphs[*area];
  end = glyph + row->used[*area];
  while (glyph < end)
    {
      if (x < x0 + glyph->pixel_width)
	{
	  if (w->pseudo_window_p)
	    break;
	  else if (!buffer_only_p || BUFFERP (glyph->object))
	    break;
	}
      
      x0 += glyph->pixel_width;
      ++glyph;
    }

  if (glyph == end)
    return NULL;

  *hpos = glyph - row->glyphs[*area];
  return glyph;
}


/* Convert frame-relative x/y to coordinates relative to window W.
   Takes pseudo-windows into account.  */

static void
frame_to_window_pixel_xy (w, x, y)
     struct window *w;
     int *x, *y;
{
  if (w->pseudo_window_p)
    {
      /* A pseudo-window is always full-width, and starts at the
	 left edge of the frame, plus a frame border.  */
      struct frame *f = XFRAME (w->frame);
      *x -= FRAME_INTERNAL_BORDER_WIDTH_SAFE (f);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
  else
    {
      *x = FRAME_TO_WINDOW_PIXEL_X (w, *x);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
}


/* Take proper action when mouse has moved to the mode or header line of
   window W, x-position X.  MODE_LINE_P non-zero means mouse is on the
   mode line.  X is relative to the start of the text display area of
   W, so the width of fringes and scroll bars must be subtracted
   to get a position relative to the start of the mode line.  */

static void
note_mode_line_highlight (w, x, mode_line_p)
     struct window *w;
     int x, mode_line_p;
{
  struct frame *f = XFRAME (w->frame);
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  Cursor cursor = dpyinfo->vertical_scroll_bar_cursor;
  struct glyph_row *row;

  if (mode_line_p)
    row = MATRIX_MODE_LINE_ROW (w->current_matrix);
  else
    row = MATRIX_HEADER_LINE_ROW (w->current_matrix);

  if (row->enabled_p)
    {
      struct glyph *glyph, *end;
      Lisp_Object help, map;
      int x0;
      
      /* Find the glyph under X.  */
      glyph = row->glyphs[TEXT_AREA];
      end = glyph + row->used[TEXT_AREA];
      x0 = - (FRAME_LEFT_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f)
	      + FRAME_X_LEFT_FRINGE_WIDTH (f));
      
      while (glyph < end
	     && x >= x0 + glyph->pixel_width)
	{
	  x0 += glyph->pixel_width;
	  ++glyph;
	}

      if (glyph < end
	  && STRINGP (glyph->object)
	  && XSTRING (glyph->object)->intervals
	  && glyph->charpos >= 0
	  && glyph->charpos < XSTRING (glyph->object)->size)
	{
	  /* If we're on a string with `help-echo' text property,
	     arrange for the help to be displayed.  This is done by
	     setting the global variable help_echo to the help string.  */
	  help = Fget_text_property (make_number (glyph->charpos),
				     Qhelp_echo, glyph->object);
	  if (!NILP (help))
            {
              help_echo = help;
              XSETWINDOW (help_echo_window, w);
              help_echo_object = glyph->object;
              help_echo_pos = glyph->charpos;
            }

	  /* Change the mouse pointer according to what is under X/Y.  */
	  map = Fget_text_property (make_number (glyph->charpos),
				    Qlocal_map, glyph->object);
	  if (KEYMAPP (map))
	    cursor = f->output_data.w32->nontext_cursor;
	  else
	    {
	      map = Fget_text_property (make_number (glyph->charpos),
					Qkeymap, glyph->object);
	      if (KEYMAPP (map))
		cursor = f->output_data.w32->nontext_cursor;
	    }
	}
    }

#if 0 /* TODO: mouse cursor */
  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
#endif
}


/* Take proper action when the mouse has moved to position X, Y on
   frame F as regards highlighting characters that have mouse-face
   properties.  Also de-highlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

static void
note_mouse_highlight (f, x, y)
     struct frame *f;
     int x, y;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  int portion;
  Lisp_Object window;
  struct window *w;
  struct buffer *b;

  /* When a menu is active, don't highlight because this looks odd. */
  if (popup_activated ())
    return;

  if (NILP (Vmouse_highlight)
      || !f->glyphs_initialized_p)
    return;

  dpyinfo->mouse_face_mouse_x = x;
  dpyinfo->mouse_face_mouse_y = y;
  dpyinfo->mouse_face_mouse_frame = f;

  if (dpyinfo->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      dpyinfo->mouse_face_deferred_gc = 1;
      return;
    }

  /* Which window is that in?  */
  window = window_from_coordinates (f, x, y, &portion, 1);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);

  /* Not on a window -> return.  */
  if (!WINDOWP (window))
    return;

  /* Reset help_echo. It will get recomputed below.  */
  help_echo = Qnil;

  /* Convert to window-relative pixel coordinates.  */
  w = XWINDOW (window);
  frame_to_window_pixel_xy (w, &x, &y);

  /* Handle tool-bar window differently since it doesn't display a
     buffer.  */
  if (EQ (window, f->tool_bar_window))
    {
      note_tool_bar_highlight (f, x, y);
      return;
    }

  /* Mouse is on the mode or header line?  */
  if (portion == 1 || portion == 3)
    {
      note_mode_line_highlight (w, x, portion == 1);
      return;
    }
#if 0 /* TODO: mouse cursor */
  if (portion == 2)
    cursor = f->output_data.x->horizontal_drag_cursor;
  else
    cursor = f->output_data.x->text_cursor;
#endif
  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  b = XBUFFER (w->buffer);
  if (/* Within text portion of the window.  */
      portion == 0
      && EQ (w->window_end_valid, w->buffer)
      && XFASTINT (w->last_modified) == BUF_MODIFF (b)
      && XFASTINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
    {
      int hpos, vpos, pos, i, area;
      struct glyph *glyph;
      Lisp_Object object;
      Lisp_Object mouse_face = Qnil, overlay = Qnil, position;
      Lisp_Object *overlay_vec = NULL;
      int len, noverlays;
      struct buffer *obuf;
      int obegv, ozv, same_region;

      /* Find the glyph under X/Y.  */
      glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, &area, 0);

      /* Clear mouse face if X/Y not over text.  */
      if (glyph == NULL
	  || area != TEXT_AREA
	  || !MATRIX_ROW (w->current_matrix, vpos)->displays_text_p)
	{
	  clear_mouse_face (dpyinfo);
	  /* TODO: mouse cursor */
	  goto set_cursor;
	}

      pos = glyph->charpos;
      object = glyph->object;
      if (!STRINGP (object) && !BUFFERP (object))
	goto set_cursor;

      /* If we get an out-of-range value, return now; avoid an error.  */
      if (BUFFERP (object) && pos > BUF_Z (b))
	goto set_cursor;

      /* Make the window's buffer temporarily current for
	 overlays_at and compute_char_face.  */
      obuf = current_buffer;
      current_buffer = b;
      obegv = BEGV;
      ozv = ZV;
      BEGV = BEG;
      ZV = Z;

      /* Is this char mouse-active or does it have help-echo?  */
      position = make_number (pos);

      if (BUFFERP (object))
	{
	  /* Put all the overlays we want in a vector in overlay_vec.
	     Store the length in len.  If there are more than 10, make
	     enough space for all, and try again.  */
	  len = 10;
	  overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	  noverlays =  overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL, 0);
	  if (noverlays > len)
	    {
	      len = noverlays;
	      overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	      noverlays = overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL,0);
	    }

	  /* Sort overlays into increasing priority order.  */
	  noverlays = sort_overlays (overlay_vec, noverlays, w);
	}
      else
	noverlays = 0;

      same_region = (EQ (window, dpyinfo->mouse_face_window)
		     && vpos >= dpyinfo->mouse_face_beg_row
		     && vpos <= dpyinfo->mouse_face_end_row
		     && (vpos > dpyinfo->mouse_face_beg_row
			 || hpos >= dpyinfo->mouse_face_beg_col)
		     && (vpos < dpyinfo->mouse_face_end_row
			 || hpos < dpyinfo->mouse_face_end_col
			 || dpyinfo->mouse_face_past_end));

      /* TODO: if (same_region)
	 mouse cursor */

      /* Check mouse-face highlighting.  */
      if (! same_region
	  /* If there exists an overlay with mouse-face overlapping
	     the one we are currently highlighting, we have to
	     check if we enter the overlapping overlay, and then
	     highlight that.  */
	  || (OVERLAYP (dpyinfo->mouse_face_overlay)
	      && mouse_face_overlay_overlaps (dpyinfo->mouse_face_overlay)))
	{
	  /* Find the highest priority overlay that has a mouse-face
	     property.  */
	  overlay = Qnil;
	  for (i = noverlays - 1; i >= 0 && NILP (overlay); --i)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		overlay = overlay_vec[i];
	    }

	  /* If we're actually highlighting the same overlay as
	     before, there's no need to do that again.  */
	  if (!NILP (overlay)
	      && EQ (overlay, dpyinfo->mouse_face_overlay))
	    goto check_help_echo;

	  dpyinfo->mouse_face_overlay = overlay;

	  /* Clear the display of the old active region, if any.  */
	  clear_mouse_face (dpyinfo);
	  /* TODO: mouse cursor changes.  */

	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    mouse_face = Fget_text_property (position, Qmouse_face, object);

	  /* Handle the overlay case.  */
	  if (!NILP (overlay))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after;
	      int ignore;

	      before = Foverlay_start (overlay);
	      after = Foverlay_end (overlay);
	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);

	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;

	      dpyinfo->mouse_face_face_id
		= face_at_buffer_position (w, pos, 0, 0,
					   &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      /* TODO: mouse cursor changes.  */
	    }
	  /* Handle the text property case.  */
	  else if (! NILP (mouse_face) && BUFFERP (object))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after, beginning, end;
	      int ignore;

	      beginning = Fmarker_position (w->start);
	      end = make_number (BUF_Z (XBUFFER (object))
				 - XFASTINT (w->window_end_pos));
	      before
		= Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, beginning);
	      after
		= Fnext_single_property_change (position, Qmouse_face,
						object, end);

	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);
	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;

	      if (BUFFERP (object))
		dpyinfo->mouse_face_face_id
		  = face_at_buffer_position (w, pos, 0, 0,
					     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      /* TODO: mouse cursor changes.  */
	    }
	  else if (!NILP (mouse_face) && STRINGP (object))
	    {
	      Lisp_Object b, e;
	      int ignore;

	      b = Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, Qnil);
	      e = Fnext_single_property_change (position, Qmouse_face,
						object, Qnil);
	      if (NILP (b))
		b = make_number (0);
	      if (NILP (e))
		e = make_number (XSTRING (object)->size - 1);
	      fast_find_string_pos (w, XINT (b), object,
				    &dpyinfo->mouse_face_beg_col,
				    &dpyinfo->mouse_face_beg_row,
				    &dpyinfo->mouse_face_beg_x,
				    &dpyinfo->mouse_face_beg_y, 0);
	      fast_find_string_pos (w, XINT (e), object,
				    &dpyinfo->mouse_face_end_col,
				    &dpyinfo->mouse_face_end_row,
				    &dpyinfo->mouse_face_end_x,
				    &dpyinfo->mouse_face_end_y, 1);
	      dpyinfo->mouse_face_past_end = 0;
	      dpyinfo->mouse_face_window = window;
	      dpyinfo->mouse_face_face_id
		= face_at_string_position (w, object, pos, 0, 0, 0, &ignore,
					   glyph->face_id, 1);
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      /* TODO: mouse cursor changes.  */
	    }
	  else if (STRINGP (object) && NILP (mouse_face))
	    {
	      /* A string which doesn't have mouse-face, but
		 the text ``under'' it might have.  */
	      struct glyph_row *r = MATRIX_ROW (w->current_matrix, vpos);
	      int start = MATRIX_ROW_START_CHARPOS (r);
	      
	      pos = string_buffer_position (w, object, start);
	      if (pos > 0)
		mouse_face = get_char_property_and_overlay (make_number (pos),
							    Qmouse_face,
							    w->buffer,
							    &overlay);
	      if (!NILP (mouse_face) && !NILP (overlay))
		{
		  Lisp_Object before = Foverlay_start (overlay);
		  Lisp_Object after = Foverlay_end (overlay);
		  int ignore;

		  /* Note that we might not be able to find position
		     BEFORE in the glyph matrix if the overlay is
		     entirely covered by a `display' property.  In
		     this case, we overshoot.  So let's stop in
		     the glyph matrix before glyphs for OBJECT.  */
		  fast_find_position (w, XFASTINT (before),
				      &dpyinfo->mouse_face_beg_col,
				      &dpyinfo->mouse_face_beg_row,
				      &dpyinfo->mouse_face_beg_x,
				      &dpyinfo->mouse_face_beg_y,
				      object);
		       
		  dpyinfo->mouse_face_past_end
		    = !fast_find_position (w, XFASTINT (after),
					   &dpyinfo->mouse_face_end_col,
					   &dpyinfo->mouse_face_end_row,
					   &dpyinfo->mouse_face_end_x,
					   &dpyinfo->mouse_face_end_y,
					   Qnil);
		  dpyinfo->mouse_face_window = window;
		  dpyinfo->mouse_face_face_id
		    = face_at_buffer_position (w, pos, 0, 0,
					       &ignore, pos + 1, 1);

		  /* Display it as active.  */
		  show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
		  /* TODO: mouse cursor changes.  */
		}
	    }
	}

    check_help_echo:

      /* Look for a `help-echo' property.  */
      {
	Lisp_Object help, overlay;

	/* Check overlays first.  */
	help = overlay = Qnil;
	for (i = noverlays - 1; i >= 0 && NILP (help); --i)
	  {
	    overlay = overlay_vec[i];
	    help = Foverlay_get (overlay, Qhelp_echo); 
	  }

	if (!NILP (help))
	  {
	    help_echo = help;
	    help_echo_window = window;
	    help_echo_object = overlay;
	    help_echo_pos = pos;
	  }
	else
	  {
	    Lisp_Object object = glyph->object;
	    int charpos = glyph->charpos;

	    /* Try text properties.  */
	    if (STRINGP (object)
		&& charpos >= 0
		&& charpos < XSTRING (object)->size)
	      {
		help = Fget_text_property (make_number (charpos),
					   Qhelp_echo, object);
		if (NILP (help))
		  {
		    /* If the string itself doesn't specify a help-echo,
		       see if the buffer text ``under'' it does.  */
		    struct glyph_row *r
		      = MATRIX_ROW (w->current_matrix, vpos);
		    int start = MATRIX_ROW_START_CHARPOS (r);
		    int pos = string_buffer_position (w, object, start);
		    if (pos > 0)
		      {
			help = Fget_char_property (make_number (pos),
						   Qhelp_echo, w->buffer);
			if (!NILP (help))
			  {
			    charpos = pos;
			    object = w->buffer;
			  }
		      }
		  }
	      }
	    else if (BUFFERP (object)
		     && charpos >= BEGV
		     && charpos < ZV)
	      help = Fget_text_property (make_number (charpos), Qhelp_echo,
					 object);
	    
	    if (!NILP (help))
	      {
		help_echo = help;
		help_echo_window = window;
		help_echo_object = object;
		help_echo_pos = charpos;
	      }
	  }
      }

      BEGV = obegv;
      ZV = ozv;
      current_buffer = obuf;
    }

 set_cursor:
  /* TODO: mouse cursor changes. */
  ;
}

static void
redo_mouse_highlight ()
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  LOWORD (last_mouse_motion_event.lParam),
			  HIWORD (last_mouse_motion_event.lParam));
}



/***********************************************************************
			       Tool-bars
 ***********************************************************************/

static int x_tool_bar_item P_ ((struct frame *, int, int,
			       struct glyph **, int *, int *, int *));

/* Tool-bar item index of the item on which a mouse button was pressed
   or -1.  */

static int last_tool_bar_item;


/* Get information about the tool-bar item at position X/Y on frame F.
   Return in *GLYPH a pointer to the glyph of the tool-bar item in
   the current matrix of the tool-bar window of F, or NULL if not
   on a tool-bar item.  Return in *PROP_IDX the index of the tool-bar
   item in F->tool_bar_items.  Value is

   -1	if X/Y is not on a tool-bar item
   0	if X/Y is on the same item that was highlighted before.
   1	otherwise.  */

static int
x_tool_bar_item (f, x, y, glyph, hpos, vpos, prop_idx)
     struct frame *f;
     int x, y;
     struct glyph **glyph;
     int *hpos, *vpos, *prop_idx;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int area;

  /* Find the glyph under X/Y.  */
  *glyph = x_y_to_hpos_vpos (w, x, y, hpos, vpos, &area, 0);
  if (*glyph == NULL)
    return -1;

  /* Get the start of this tool-bar item's properties in
     f->tool_bar_items.  */
  if (!tool_bar_item_info (f, *glyph, prop_idx))
    return -1;

  /* Is mouse on the highlighted item?  */
  if (EQ (f->tool_bar_window, dpyinfo->mouse_face_window)
      && *vpos >= dpyinfo->mouse_face_beg_row
      && *vpos <= dpyinfo->mouse_face_end_row
      && (*vpos > dpyinfo->mouse_face_beg_row
	  || *hpos >= dpyinfo->mouse_face_beg_col)
      && (*vpos < dpyinfo->mouse_face_end_row
	  || *hpos < dpyinfo->mouse_face_end_col
	  || dpyinfo->mouse_face_past_end))
    return 0;
  
  return 1;
}


/* Handle mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  EVENT_TYPE is either ButtionPress
   or ButtonRelase.  */

static void
w32_handle_tool_bar_click (f, button_event)
     struct frame *f;
     struct input_event *button_event;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int hpos, vpos, prop_idx;
  struct glyph *glyph;
  Lisp_Object enabled_p;
  int x = XFASTINT (button_event->x);
  int y = XFASTINT (button_event->y);
  
  /* If not on the highlighted tool-bar item, return.  */
  frame_to_window_pixel_xy (w, &x, &y);
  if (x_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx) != 0)
    return;

  /* If item is disabled, do nothing.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (NILP (enabled_p))
    return;
  
  if (button_event->modifiers & down_modifier)
    {
      /* Show item in pressed state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_SUNKEN);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_SUNKEN;
      last_tool_bar_item = prop_idx;
    }
  else
    {
      Lisp_Object key, frame;
      struct input_event event;

      /* Show item in released state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_RAISED);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_RAISED;

      key = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_KEY);

      XSETFRAME (frame, f);
      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = frame;
      kbd_buffer_store_event (&event);

      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = key;
      /* The keyboard buffer doesn't like the up modifier being set.  */
      event.modifiers = button_event->modifiers & ~up_modifier;
      kbd_buffer_store_event (&event);
      last_tool_bar_item = -1;
    }
}


/* Possibly highlight a tool-bar item on frame F when mouse moves to
   tool-bar window-relative coordinates X/Y.  Called from
   note_mouse_highlight.  */

static void
note_tool_bar_highlight (f, x, y)
     struct frame *f;
     int x, y;
{
  Lisp_Object window = f->tool_bar_window;
  struct window *w = XWINDOW (window);
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  int hpos, vpos;
  struct glyph *glyph;
  struct glyph_row *row;
  int i;
  Lisp_Object enabled_p;
  int prop_idx;
  enum draw_glyphs_face draw = DRAW_IMAGE_RAISED;
  int mouse_down_p, rc;

  /* Function note_mouse_highlight is called with negative x(y
     values when mouse moves outside of the frame.  */
  if (x <= 0 || y <= 0)
    {
      clear_mouse_face (dpyinfo);
      return;
    }

  rc = x_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx);
  if (rc < 0)
    {
      /* Not on tool-bar item.  */
      clear_mouse_face (dpyinfo);
      return;
    }
  else if (rc == 0)
    /* On same tool-bar item as before.  */
    goto set_help_echo;

  clear_mouse_face (dpyinfo);
  
  /* Mouse is down, but on different tool-bar item?  */
  mouse_down_p = (dpyinfo->grabbed
		  && f == last_mouse_frame
		  && FRAME_LIVE_P (f));
  if (mouse_down_p
      && last_tool_bar_item != prop_idx)
    return;

  dpyinfo->mouse_face_image_state = DRAW_NORMAL_TEXT;
  draw = mouse_down_p ? DRAW_IMAGE_SUNKEN : DRAW_IMAGE_RAISED;
  
  /* If tool-bar item is not enabled, don't highlight it.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (!NILP (enabled_p))
    {
      /* Compute the x-position of the glyph.  In front and past the
	 image is a space.  We include this is the highlighted area.  */
      row = MATRIX_ROW (w->current_matrix, vpos);
      for (i = x = 0; i < hpos; ++i)
	x += row->glyphs[TEXT_AREA][i].pixel_width;
      
      /* Record this as the current active region.  */
      dpyinfo->mouse_face_beg_col = hpos;
      dpyinfo->mouse_face_beg_row = vpos;
      dpyinfo->mouse_face_beg_x = x;
      dpyinfo->mouse_face_beg_y = row->y;
      dpyinfo->mouse_face_past_end = 0;
      
      dpyinfo->mouse_face_end_col = hpos + 1;
      dpyinfo->mouse_face_end_row = vpos;
      dpyinfo->mouse_face_end_x = x + glyph->pixel_width;
      dpyinfo->mouse_face_end_y = row->y;
      dpyinfo->mouse_face_window = window;
      dpyinfo->mouse_face_face_id = TOOL_BAR_FACE_ID;
      
      /* Display it as active.  */
      show_mouse_face (dpyinfo, draw);
      dpyinfo->mouse_face_image_state = draw;
    }
      
 set_help_echo:
  
  /* Set help_echo to a help string.to display for this tool-bar item.
     w32_read_socket does the rest.  */
  help_echo_object = help_echo_window = Qnil;
  help_echo_pos = -1;
  help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_HELP);
  if (NILP (help_echo))
    help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_CAPTION);
}



/* Find the glyph matrix position of buffer position CHARPOS in window
   *W.  HPOS, *VPOS, *X, and *Y are set to the positions found.  W's
   current glyphs must be up to date.  If CHARPOS is above window
   start return (0, 0, 0, 0).  If CHARPOS is after end of W, return end
   of last line in W.  In the row containing CHARPOS, stop before glyphs
   having STOP as object.  */

#if 0 /* This is a version of fast_find_position that's more correct
	 in the presence of hscrolling, for example.  I didn't install
	 it right away because the problem fixed is minor, it failed
	 in 20.x as well, and I think it's too risky to install 
	 so near the release of 21.1.  2001-09-25 gerd.  */

static int
fast_find_position (w, charpos, hpos, vpos, x, y, stop)
     struct window *w;
     int charpos;
     int *hpos, *vpos, *x, *y;
     Lisp_Object stop;
{
  struct glyph_row *row, *first;
  struct glyph *glyph, *end;
  int i, past_end = 0;

  first = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row = row_containing_pos (w, charpos, first, NULL, 0);
  if (row == NULL)
    {
      if (charpos < MATRIX_ROW_START_CHARPOS (first))
	{
	  *x = *y = *hpos = *vpos = 0;
	  return 0;
	}
      else
	{
	  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
	  past_end = 1;
	}
    }

  *x = row->x;
  *y = row->y;
  *vpos = MATRIX_ROW_VPOS (row, w->current_matrix);
  
  glyph = row->glyphs[TEXT_AREA];
  end = glyph + row->used[TEXT_AREA];
  
  /* Skip over glyphs not having an object at the start of the row.
     These are special glyphs like truncation marks on terminal
     frames.  */
  if (row->displays_text_p)
    while (glyph < end
	   && INTEGERP (glyph->object)
	   && !EQ (stop, glyph->object)
	   && glyph->charpos < 0)
      {
	*x += glyph->pixel_width;
	++glyph;
      }

  while (glyph < end
	 && !INTEGERP (glyph->object)
	 && !EQ (stop, glyph->object)
	 && (!BUFFERP (glyph->object)
	     || glyph->charpos < charpos))
    {
      *x += glyph->pixel_width;
      ++glyph;
    }

  *hpos = glyph - row->glyphs[TEXT_AREA];
  return past_end;
}

#else /* not 0 */

static int
fast_find_position (w, pos, hpos, vpos, x, y, stop)
     struct window *w;
     int pos;
     int *hpos, *vpos, *x, *y;
     Lisp_Object stop;
{
  int i;
  int lastcol;
  int maybe_next_line_p = 0;
  int line_start_position;
  int yb = window_text_bottom_y (w);
  struct glyph_row *row, *best_row;
  int row_vpos, best_row_vpos;
  int current_x;

  row = best_row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row_vpos = best_row_vpos = MATRIX_ROW_VPOS (row, w->current_matrix);

  while (row->y < yb)
    {
      if (row->used[TEXT_AREA])
	line_start_position = row->glyphs[TEXT_AREA]->charpos;
      else
	line_start_position = 0;

      if (line_start_position > pos)
	break;
      /* If the position sought is the end of the buffer,
	 don't include the blank lines at the bottom of the window.  */
      else if (line_start_position == pos
               && pos == BUF_ZV (XBUFFER (w->buffer)))
	{
	  maybe_next_line_p = 1;
	  break;
	}
      else if (line_start_position > 0)
        {
          best_row = row;
          best_row_vpos = row_vpos;
        }

      if (row->y + row->height >= yb)
        break;

      ++row;
      ++row_vpos;
    }

  /* Find the right column within BEST_ROW.  */
  lastcol = 0;
  current_x = best_row->x;
  for (i = 0; i < best_row->used[TEXT_AREA]; i++)
    {
      struct glyph *glyph = best_row->glyphs[TEXT_AREA] + i;
      int charpos = glyph->charpos;

      if (BUFFERP (glyph->object))
	{
	  if (charpos == pos)
	    {
	      *hpos = i;
	      *vpos = best_row_vpos;
	      *x = current_x;
	      *y = best_row->y;
	      return 1;
	    }
	  else if (charpos > pos)
	    break;
	}
      else if (EQ (glyph->object, stop))
	break;

      if (charpos > 0)
	lastcol = i;
      current_x += glyph->pixel_width;
    }

  /* If we're looking for the end of the buffer,
     and we didn't find it in the line we scanned,
     use the start of the following line.  */
  if (maybe_next_line_p)
    {
      ++best_row;
      ++best_row_vpos;
      lastcol = 0;
      current_x = best_row->x;
    }

  *vpos = best_row_vpos;
  *hpos = lastcol + 1;
  *x = current_x;
  *y = best_row->y;
  return 0;
}

#endif /* not 0 */


/* Find the position of the glyph for position POS in OBJECT in
   window W's current matrix, and return in *X/*Y the pixel
   coordinates, and return in *HPOS/*VPOS the column/row of the glyph.

   RIGHT_P non-zero means return the position of the right edge of the
   glyph, RIGHT_P zero means return the left edge position.

   If no glyph for POS exists in the matrix, return the position of
   the glyph with the next smaller position that is in the matrix, if
   RIGHT_P is zero.  If RIGHT_P is non-zero, and no glyph for POS
   exists in the matrix, return the position of the glyph with the
   next larger position in OBJECT.

   Value is non-zero if a glyph was found.  */

static int
fast_find_string_pos (w, pos, object, hpos, vpos, x, y, right_p)
     struct window *w;
     int pos;
     Lisp_Object object;
     int *hpos, *vpos, *x, *y;
     int right_p;
{
  int yb = window_text_bottom_y (w);
  struct glyph_row *r;
  struct glyph *best_glyph = NULL;
  struct glyph_row *best_row = NULL;
  int best_x = 0;

  for (r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       r->enabled_p && r->y < yb;
       ++r)
    {
      struct glyph *g = r->glyphs[TEXT_AREA];
      struct glyph *e = g + r->used[TEXT_AREA];
      int gx;

      for (gx = r->x; g < e; gx += g->pixel_width, ++g)
	if (EQ (g->object, object))
	  {
	    if (g->charpos == pos)
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
		goto found;
	      }
	    else if (best_glyph == NULL
		     || ((abs (g->charpos - pos)
			 < abs (best_glyph->charpos - pos))
			 && (right_p
			     ? g->charpos < pos
			     : g->charpos > pos)))
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
	      }
	  }
    }

 found:

  if (best_glyph)
    {
      *x = best_x;
      *hpos = best_glyph - best_row->glyphs[TEXT_AREA];

      if (right_p)
	{
	  *x += best_glyph->pixel_width;
	  ++*hpos;
	}
      
      *y = best_row->y;
      *vpos = best_row - w->current_matrix->rows;
    }

  return best_glyph != NULL;
}


/* Display the active region described by mouse_face_*
   in its mouse-face if HL > 0, in its normal face if HL = 0.  */

static void
show_mouse_face (dpyinfo, draw)
     struct w32_display_info *dpyinfo;
     enum draw_glyphs_face draw;
{
  struct window *w = XWINDOW (dpyinfo->mouse_face_window);
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  
  if (/* If window is in the process of being destroyed, don't bother
	 to do anything.  */
      w->current_matrix != NULL
      /* Don't update mouse highlight if hidden */
      && (draw != DRAW_MOUSE_FACE || !dpyinfo->mouse_face_hidden)
      /* Recognize when we are called to operate on rows that don't exist
	 anymore.  This can happen when a window is split.  */
      && dpyinfo->mouse_face_end_row < w->current_matrix->nrows)
    {
      int phys_cursor_on_p = w->phys_cursor_on_p;
      struct glyph_row *row, *first, *last;

      first = MATRIX_ROW (w->current_matrix, dpyinfo->mouse_face_beg_row);
      last = MATRIX_ROW (w->current_matrix, dpyinfo->mouse_face_end_row);
      
      for (row = first; row <= last && row->enabled_p; ++row)
	{
	  int start_hpos, end_hpos, start_x;

	  /* For all but the first row, the highlight starts at column 0.  */
	  if (row == first)
	    {
	      start_hpos = dpyinfo->mouse_face_beg_col;
	      start_x = dpyinfo->mouse_face_beg_x;
	    }
	  else
	    {
	      start_hpos = 0;
	      start_x = 0;
	    }

	  if (row == last)
	    end_hpos = dpyinfo->mouse_face_end_col;
	  else
	    end_hpos = row->used[TEXT_AREA];

	  if (end_hpos > start_hpos)
	    {
	      x_draw_glyphs (w, start_x, row, TEXT_AREA, 
			     start_hpos, end_hpos, draw, 0);

	      row->mouse_face_p
		= draw == DRAW_MOUSE_FACE || draw == DRAW_IMAGE_RAISED;
	    }
	}

      /* When we've written over the cursor, arrange for it to
	 be displayed again.  */
      if (phys_cursor_on_p && !w->phys_cursor_on_p)
	x_display_cursor (w, 1,
			  w->phys_cursor.hpos, w->phys_cursor.vpos,
			  w->phys_cursor.x, w->phys_cursor.y);
    }

#if 0 /* TODO: mouse cursor */
  /* Change the mouse cursor.  */
  if (draw == DRAW_NORMAL_TEXT)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->text_cursor);
  else if (draw == DRAW_MOUSE_FACE)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->cross_cursor);
  else
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->nontext_cursor);
#endif
}

/* Clear out the mouse-highlighted active region.
   Redraw it un-highlighted first.  */

static int
clear_mouse_face (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  int cleared = 0;

  if (! NILP (dpyinfo->mouse_face_window))
    {
      show_mouse_face (dpyinfo, DRAW_NORMAL_TEXT);
      cleared = 1;
    }

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  return cleared;
}


/* Clear any mouse-face on window W.  This function is part of the
   redisplay interface, and is called from try_window_id and similar
   functions to ensure the mouse-highlight is off.  */

static void
x_clear_mouse_face (w)
     struct window *w;
{
  struct w32_display_info *dpyinfo
    = FRAME_W32_DISPLAY_INFO (XFRAME (w->frame));
  Lisp_Object window;

  BLOCK_INPUT;
  XSETWINDOW (window, w);
  if (EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);
  UNBLOCK_INPUT;
}


/* Just discard the mouse face information for frame F, if any.
   This is used when the size of F is changed.  */

void
cancel_mouse_face (f)
     FRAME_PTR f;
{
  Lisp_Object window;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  window = dpyinfo->mouse_face_window;
  if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }
}

static struct scroll_bar *x_window_to_scroll_bar ();
static void x_scroll_bar_report_motion ();
static void x_check_fullscreen P_ ((struct frame *));
static void x_check_fullscreen_move P_ ((struct frame *));
static int glyph_rect P_ ((struct frame *f, int, int, RECT *));


/* Try to determine frame pixel position and size of the glyph under
   frame pixel coordinates X/Y on frame F .  Return the position and
   size in *RECT.  Value is non-zero if we could compute these
   values.  */

static int
glyph_rect (f, x, y, rect)
     struct frame *f;
     int x, y;
     RECT *rect;
{
  Lisp_Object window;
  int part;

  window = window_from_coordinates (f, x, y, &part, 0);
  if (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      struct glyph_row *r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
      struct glyph_row *end = r + w->current_matrix->nrows - 1;

      frame_to_window_pixel_xy (w, &x, &y);

      for (; r < end && r->enabled_p; ++r)
	if (r->y <= y && r->y + r->height > y)
	  {
	    /* Found the row at y.  */
	    struct glyph *g = r->glyphs[TEXT_AREA];
	    struct glyph *end = g + r->used[TEXT_AREA];
	    int gx;

	    rect->top = WINDOW_TO_FRAME_PIXEL_Y (w, r->y);
	    rect->bottom = rect->top + r->height;

	    if (x < r->x)
	      {
		/* x is to the left of the first glyph in the row.  */
		rect->left = XINT (w->left);
		rect->right = WINDOW_TO_FRAME_PIXEL_X (w, r->x);
		return 1;
	      }

	    for (gx = r->x; g < end; gx += g->pixel_width, ++g)
	      if (gx <= x && gx + g->pixel_width > x)
		{
		  /* x is on a glyph.  */
		  rect->left = WINDOW_TO_FRAME_PIXEL_X (w, gx);
		  rect->right = rect->left + g->pixel_width;
		  return 1;
		}

	    /* x is to the right of the last glyph in the row.  */
	    rect->left = WINDOW_TO_FRAME_PIXEL_X (w, gx);
	    rect->right = XINT (w->left) + XINT (w->width);
	    return 1;
	  }
    }

  /* The y is not on any row.  */
  return 0;
}

/* Record the position of the mouse in last_mouse_glyph.  */
static void
remember_mouse_glyph (f1, gx, gy)
     struct frame * f1;
     int gx, gy;
{
  if (!glyph_rect (f1, gx, gy, &last_mouse_glyph))
    {
      int width = FRAME_SMALLEST_CHAR_WIDTH (f1);
      int height = FRAME_SMALLEST_FONT_HEIGHT (f1);

      /* Arrange for the division in PIXEL_TO_CHAR_COL etc. to
	 round down even for negative values.  */
      if (gx < 0)
	gx -= width - 1;
      if (gy < 0)
	gy -= height - 1;
#if 0
      /* This was the original code from XTmouse_position, but it seems
	 to give the position of the glyph diagonally next to the one
	 the mouse is over.  */
      gx = (gx + width - 1) / width * width;
      gy = (gy + height - 1) / height * height;
#else
      gx = gx / width * width;
      gy = gy / height * height;
#endif

      last_mouse_glyph.left = gx;
      last_mouse_glyph.top = gy;
      last_mouse_glyph.right  = gx + width;
      last_mouse_glyph.bottom = gy + height;
    }
}

/* Return the current position of the mouse.
   *fp should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *fp, *bar_window,
   and *part to the frame, window, and scroll bar part that the mouse
   is over.  Set *x and *y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *fp to the frame the
   mouse is on, *bar_window to nil, and *x and *y to the character cell
   the mouse is over.

   Set *time to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
w32_mouse_position (fp, insist, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     int insist;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar) && insist == 0)
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      POINT pt;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = Qnil;
      
      GetCursorPos (&pt);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	if (FRAME_W32_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    /* If mouse was grabbed on a frame, give coords for that frame
	       even if the mouse is now outside it.  */
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    /* Is window under mouse one of our frames?  */
	    f1 = x_any_window_to_frame (FRAME_W32_DISPLAY_INFO (*fp),
                                    WindowFromPoint (pt));
	  }

	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar
              = x_window_to_scroll_bar (WindowFromPoint (pt));

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
	      }
	  }

	if (f1 == 0 && insist > 0)
	  f1 = SELECTED_FRAME ();

	if (f1)
	  {
	    /* Ok, we found a frame.  Store all the values.
	       last_mouse_glyph is a rectangle used to reduce the
	       generation of mouse events.  To not miss any motion
	       events, we must divide the frame into rectangles of the
	       size of the smallest character that could be displayed
	       on it, i.e. into the same rectangles that matrices on
	       the frame are divided into.  */

#if OLD_REDISPLAY_CODE
	    int ignore1, ignore2;

	    ScreenToClient (FRAME_W32_WINDOW (f1), &pt);

	    pixel_to_glyph_coords (f1, pt.x, pt.y, &ignore1, &ignore2,
				   &last_mouse_glyph,
				   FRAME_W32_DISPLAY_INFO (f1)->grabbed
				   || insist);
#else
	    ScreenToClient (FRAME_W32_WINDOW (f1), &pt);
	    remember_mouse_glyph (f1, pt.x, pt.y);
#endif

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, pt.x);
	    XSETINT (*y, pt.y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}


/* Scroll bar support.  */

/* Given a window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */

static struct scroll_bar *
x_window_to_scroll_bar (window_id)
     Window window_id;
{
  Lisp_Object tail;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCDR (tail))
    {
      Lisp_Object frame, bar, condemned;

      frame = XCAR (tail);
      /* All elements of Vframe_list should be frames.  */
      if (! GC_FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
	 right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
	      condemned scroll bar lists with one loop.  */
	   ! GC_NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! GC_NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar)) == window_id)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}



/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

static void
w32_set_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  double range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
  int sb_page, sb_pos;
  BOOL draggingp = !NILP (bar->dragging) ? TRUE : FALSE;

  if (whole)
    {
      /* Position scroll bar at rock bottom if the bottom of the
         buffer is visible. This avoids shinking the thumb away
         to nothing if it is held at the bottom of the buffer.  */
      if (position + portion >= whole)
        {
          sb_page = range * (whole - position) / whole
            + VERTICAL_SCROLL_BAR_MIN_HANDLE;
          sb_pos = range;
        }

      sb_page = portion * range / whole + VERTICAL_SCROLL_BAR_MIN_HANDLE;
      sb_pos = position * range / whole;
    }
  else
    {
      sb_page = range;
      sb_pos = 0;
    }

  BLOCK_INPUT;

  if (pfnSetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      /* Only update page size if currently dragging, to reduce
         flicker effects.  */
      if (draggingp)
        si.fMask = SIF_PAGE;
      else
        si.fMask = SIF_PAGE | SIF_POS;
      si.nPage = sb_page;
      si.nPos = sb_pos;

      pfnSetScrollInfo (w, SB_CTL, &si, !draggingp);
    }
  else
    SetScrollPos (w, SB_CTL, sb_pos, !draggingp);

  UNBLOCK_INPUT;
}


/************************************************************************
			 Scroll bars, general
 ************************************************************************/
									 
HWND 
my_create_scrollbar (f, bar)
     struct frame * f;
     struct scroll_bar * bar;
{
  return (HWND) SendMessage (FRAME_W32_WINDOW (f),
			     WM_EMACS_CREATESCROLLBAR, (WPARAM) f, 
			     (LPARAM) bar);
}

/*#define ATTACH_THREADS*/

BOOL
my_show_window (FRAME_PTR f, HWND hwnd, int how)
{
#ifndef ATTACH_THREADS
  return SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SHOWWINDOW,
		      (WPARAM) hwnd, (LPARAM) how);
#else
  return ShowWindow (hwnd, how);
#endif
}

void
my_set_window_pos (HWND hwnd, HWND hwndAfter,
		   int x, int y, int cx, int cy, UINT flags)
{
#ifndef ATTACH_THREADS
  WINDOWPOS pos;
  pos.hwndInsertAfter = hwndAfter;
  pos.x = x;
  pos.y = y;
  pos.cx = cx;
  pos.cy = cy;
  pos.flags = flags;
  SendMessage (hwnd, WM_EMACS_SETWINDOWPOS, (WPARAM) &pos, 0);
#else
  SetWindowPos (hwnd, hwndAfter, x, y, cx, cy, flags);
#endif
}

void
my_set_focus (f, hwnd)
     struct frame * f;
     HWND hwnd;
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SETFOCUS, 
	       (WPARAM) hwnd, 0);
}

void
my_set_foreground_window (hwnd)
     HWND hwnd;
{
  SendMessage (hwnd, WM_EMACS_SETFOREGROUND, (WPARAM) hwnd, 0);
}

void
my_destroy_window (f, hwnd)
     struct frame * f;
     HWND hwnd;
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_DESTROYWINDOW, 
	       (WPARAM) hwnd, 0);
}

/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are.the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (w, top, left, width, height)
     struct window *w;
     int top, left, width, height;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HWND hwnd;
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

  BLOCK_INPUT;

  XSETWINDOW (bar->window, w);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Requires geometry to be set before call to create the real window */

  hwnd = my_create_scrollbar (f, bar);

  if (pfnSetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      si.fMask = SIF_ALL;
      si.nMin = 0;
      si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height)
      	+ VERTICAL_SCROLL_BAR_MIN_HANDLE;
      si.nPage = si.nMax;
      si.nPos = 0;

      pfnSetScrollInfo (hwnd, SB_CTL, &si, FALSE);
    }
  else
    {
      SetScrollRange (hwnd, SB_CTL, 0,
                      VERTICAL_SCROLL_BAR_TOP_RANGE (f, height), FALSE);
      SetScrollPos (hwnd, SB_CTL, 0, FALSE);
    }

  SET_SCROLL_BAR_W32_WINDOW (bar, hwnd);

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  UNBLOCK_INPUT;

  return bar;
}


/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil. */

static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the window.  */
  my_destroy_window (f, SCROLL_BAR_W32_WINDOW (bar));

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
w32_set_vertical_scroll_bar (w, portion, whole, position)
     struct window *w;
     int portion, whole, position;
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar;
  int top, height, left, sb_left, width, sb_width;
  int window_x, window_y, window_width, window_height;

  /* Get window dimensions.  */
  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);
  top  = window_y;
  width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    left = XINT (w->left) + XINT (w->width) - FRAME_SCROLL_BAR_COLS (f);
  else
    left = XFASTINT (w->left);
  left *= CANON_X_UNIT (f);
  left += FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0)
    sb_width = FRAME_SCROLL_BAR_PIXEL_WIDTH (f);
  else
    sb_width = width;

  /* Compute the left edge of the scroll bar.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    sb_left = left + width - sb_width - (width - sb_width) / 2; 
  else
    sb_left = left + (width - sb_width) / 2;

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      HDC hdc;
      BLOCK_INPUT;
      if (width > 0 && height > 0)
	{
	  hdc = get_frame_dc (f);
	  w32_clear_area (f, hdc, left, top, width, height);
	  release_frame_dc (f, hdc);
	}
      UNBLOCK_INPUT;

      bar = x_scroll_bar_create (w, top, sb_left, sb_width, height);
    }
  else
    {
      /* It may just need to be moved and resized.  */      
      HWND hwnd;

      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      hwnd = SCROLL_BAR_W32_WINDOW (bar);

      /* If already correctly positioned, do nothing.  */
      if ( XINT (bar->left) == sb_left
           && XINT (bar->top) == top
           && XINT (bar->width) ==  sb_width
           && XINT (bar->height) == height )
        {
          /* Redraw after clear_frame. */
          if (!my_show_window (f, hwnd, SW_NORMAL))
            InvalidateRect (hwnd, NULL, FALSE);
        }
      else
        {
          HDC hdc;
          BLOCK_INPUT;
	  if (width && height)
	    {
	      hdc = get_frame_dc (f);
	      /* Since Windows scroll bars are smaller than the space reserved
		 for them on the frame, we have to clear "under" them.  */
	      w32_clear_area (f, hdc,
			      left,
			      top,
			      width,
			      height);
	      release_frame_dc (f, hdc);
	    }
          /* Make sure scroll bar is "visible" before moving, to ensure the
             area of the parent window now exposed will be refreshed.  */
          my_show_window (f, hwnd, SW_HIDE);
          MoveWindow (hwnd, sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
		      top, sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
		      max (height, 1), TRUE);
          if (pfnSetScrollInfo)
            {
              SCROLLINFO si;

              si.cbSize = sizeof (si);
              si.fMask = SIF_RANGE;
              si.nMin = 0;
              si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height)
                + VERTICAL_SCROLL_BAR_MIN_HANDLE;

              pfnSetScrollInfo (hwnd, SB_CTL, &si, FALSE);
            }
          else
            SetScrollRange (hwnd, SB_CTL, 0,
                            VERTICAL_SCROLL_BAR_TOP_RANGE (f, height), FALSE);
          my_show_window (f, hwnd, SW_NORMAL);
          /* InvalidateRect (w, NULL, FALSE);  */

          /* Remember new settings.  */
          XSETINT (bar->left, sb_left);
          XSETINT (bar->top, top);
          XSETINT (bar->width, sb_width);
          XSETINT (bar->height, height);

          UNBLOCK_INPUT;
        }
    }
  w32_set_scroll_bar_thumb (bar, portion, position, whole);

  XSETVECTOR (w->vertical_scroll_bar, bar);
}


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

static void
w32_condemn_scroll_bars (frame)
     FRAME_PTR frame;
{
  /* Transfer all the scroll bars to FRAME_CONDEMNED_SCROLL_BARS.  */
  while (! NILP (FRAME_SCROLL_BARS (frame)))
    {
      Lisp_Object bar;
      bar = FRAME_SCROLL_BARS (frame);
      FRAME_SCROLL_BARS (frame) = XSCROLL_BAR (bar)->next;
      XSCROLL_BAR (bar)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
      XSCROLL_BAR (bar)->prev = Qnil;
      if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = bar;
      FRAME_CONDEMNED_SCROLL_BARS (frame) = bar;
    }
}


/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
w32_redeem_scroll_bar (window)
     struct window *window;
{
  struct scroll_bar *bar;
  struct frame *f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  f = XFRAME (WINDOW_FRAME (window));
  if (NILP (bar->prev))
    {
      /* If the prev pointer is nil, it must be the first in one of
         the lists.  */
      if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
        /* It's not condemned.  Everything's fine.  */
        return;
      else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
                   window->vertical_scroll_bar))
        FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
      else
        /* If its prev pointer is nil, it must be at the front of
           one or the other!  */
        abort ();
    }
  else
    XSCROLL_BAR (bar->prev)->next = bar->next;

  if (! NILP (bar->next))
    XSCROLL_BAR (bar->next)->prev = bar->prev;

  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */

static void
w32_judge_scroll_bars (f)
     FRAME_PTR f;
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static int
w32_scroll_bar_handle_click (bar, msg, emacs_event)
     struct scroll_bar *bar;
     W32Msg *msg;
     struct input_event *emacs_event;
{
  if (! GC_WINDOWP (bar->window))
    abort ();

  emacs_event->kind = w32_scroll_bar_click;
  emacs_event->code = 0;
  /* not really meaningful to distinguish up/down */
  emacs_event->modifiers = msg->dwModifiers;
  emacs_event->frame_or_window = bar->window;
  emacs_event->arg = Qnil;
  emacs_event->timestamp = msg->msg.time;

  {
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
    int y;
    int dragging = !NILP (bar->dragging);

    if (pfnGetScrollInfo)
      {
	SCROLLINFO si;

	si.cbSize = sizeof (si);
	si.fMask = SIF_POS;

	pfnGetScrollInfo ((HWND) msg->msg.lParam, SB_CTL, &si);
	y = si.nPos;
      }
    else
      y = GetScrollPos ((HWND) msg->msg.lParam, SB_CTL);

    bar->dragging = Qnil;


    last_mouse_scroll_bar_pos = msg->msg.wParam;
 
    switch (LOWORD (msg->msg.wParam))
      {
      case SB_LINEDOWN:
	emacs_event->part = scroll_bar_down_arrow;
	break;
      case SB_LINEUP:
	emacs_event->part = scroll_bar_up_arrow;
	break;
      case SB_PAGEUP:
	emacs_event->part = scroll_bar_above_handle;
	break;
      case SB_PAGEDOWN:
	emacs_event->part = scroll_bar_below_handle;
	break;
      case SB_TOP:
	emacs_event->part = scroll_bar_handle;
	y = 0;
	break;
      case SB_BOTTOM:
	emacs_event->part = scroll_bar_handle;
	y = top_range;
	break;
      case SB_THUMBTRACK:
      case SB_THUMBPOSITION:
	if (VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height)) <= 0xffff)
          y = HIWORD (msg->msg.wParam);
	bar->dragging = Qt;
	emacs_event->part = scroll_bar_handle;

	/* "Silently" update current position.  */
	if (pfnSetScrollInfo)
	  {
	    SCROLLINFO si;

	    si.cbSize = sizeof (si);
	    si.fMask = SIF_POS;
	    si.nPos = y;
	    /* Remember apparent position (we actually lag behind the real
	       position, so don't set that directly.  */
	    last_scroll_bar_drag_pos = y;

	    pfnSetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, FALSE);
	  }
	else
	  SetScrollPos (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, y, FALSE);
	break;
      case SB_ENDSCROLL:
	/* If this is the end of a drag sequence, then reset the scroll
	   handle size to normal and do a final redraw.  Otherwise do
	   nothing.  */
	if (dragging)
	  {
	    if (pfnSetScrollInfo)
	      {
		SCROLLINFO si;
		int start = XINT (bar->start);
		int end = XINT (bar->end);

		si.cbSize = sizeof (si);
		si.fMask = SIF_PAGE | SIF_POS;
                si.nPage = end - start + VERTICAL_SCROLL_BAR_MIN_HANDLE;
		si.nPos = last_scroll_bar_drag_pos;
		pfnSetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, TRUE);
	      }
	    else
	      SetScrollPos (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, y, TRUE);
	  }
	/* fall through */
      default:
	emacs_event->kind = no_event;
	return FALSE;
      }

    XSETINT (emacs_event->x, y);
    XSETINT (emacs_event->y, top_range);

    return TRUE;
  }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

static void
x_scroll_bar_report_motion (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int pos;
  int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

  BLOCK_INPUT;

  *fp = f;
  *bar_window = bar->window;

  if (pfnGetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      si.fMask = SIF_POS | SIF_PAGE | SIF_RANGE;

      pfnGetScrollInfo (w, SB_CTL, &si);
      pos = si.nPos;
      top_range = si.nMax - si.nPage + 1;
    }
  else
    pos = GetScrollPos (w, SB_CTL);

  switch (LOWORD (last_mouse_scroll_bar_pos))
  {
  case SB_THUMBPOSITION:
  case SB_THUMBTRACK:
      *part = scroll_bar_handle;
      if (VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height)) <= 0xffff)
	  pos = HIWORD (last_mouse_scroll_bar_pos);
      break;
  case SB_LINEDOWN:
      *part = scroll_bar_handle;
      pos++;
      break;
  default:
      *part = scroll_bar_handle;
      break;
  }

  XSETINT (*x, pos);
  XSETINT (*y, top_range);

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}


/* The screen has been cleared so we may have changed foreground or
   background colors, and the scroll bars may need to be redrawn.
   Clear out the scroll bars, and ask for expose events, so we can
   redraw them.  */

void
x_scroll_bar_clear (f)
     FRAME_PTR f;
{
  Lisp_Object bar;

  /* We can have scroll bars even if this is 0,
     if we just turned off scroll bar mode.
     But in that case we should not clear them.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
         bar = XSCROLL_BAR (bar)->next)
      {
        HWND window = SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar));
        HDC hdc = GetDC (window);
        RECT rect;

        /* Hide scroll bar until ready to repaint.  x_scroll_bar_move
           arranges to refresh the scroll bar if hidden.  */
        my_show_window (f, window, SW_HIDE);

        GetClientRect (window, &rect);
        select_palette (f, hdc);
        w32_clear_rect (f, hdc, &rect);
        deselect_palette (f, hdc);

        ReleaseDC (window, hdc);
      }
}


/* The main W32 event-reading loop - w32_read_socket.  */

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */

static int temp_index;
static short temp_buffer[100];


/* Read events coming from the W32 shell.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   EXPECTED is nonzero if the caller knows input is available.  

   Some of these messages are reposted back to the message queue since the
   system calls the windows proc directly in a context where we cannot return 
   the data nor can we guarantee the state we are in.  So if we dispatch  them
   we will get into an infinite loop.  To prevent this from ever happening we
   will set a variable to indicate we are in the read_socket call and indicate
   which message we are processing since the windows proc gets called 
   recursively with different messages by the system.
*/

int
w32_read_socket (sd, bufp, numchars, expected)
     register int sd;
     /* register */ struct input_event *bufp;
     /* register */ int numchars;
     int expected;
{
  int count = 0;
  int check_visibility = 0;
  W32Msg msg;
  struct frame *f;
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  if (numchars <= 0)
    abort ();                   /* Don't think this happens. */

  /* TODO: tool-bars, ghostscript integration, mouse
     cursors. */
  while (get_next_msg (&msg, FALSE))
    {
      switch (msg.msg.message)
	{
	case WM_PAINT:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f) 
	    {
	      if (msg.rect.right == msg.rect.left ||
		  msg.rect.bottom == msg.rect.top)
		{
		  /* We may get paint messages even though the client
		     area is clipped - these are not expose events. */
		  DebPrint (("clipped frame %p (%s) got WM_PAINT - ignored\n", f,
			     XSTRING (f->name)->data));
		}
	      else if (f->async_visible != 1)
		{
		  /* Definitely not obscured, so mark as visible.  */
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  SET_FRAME_GARBAGED (f);
		  DebPrint (("frame %p (%s) reexposed by WM_PAINT\n", f,
			     XSTRING (f->name)->data));

		  /* WM_PAINT serves as MapNotify as well, so report
		     visibility changes properly.  */
		  if (f->iconified)
		    {
		      bufp->kind = deiconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp->arg = Qnil;
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else if (! NILP (Vframe_list)
			   && ! NILP (XCDR (Vframe_list)))
		    /* Force a redisplay sooner or later to update the
		       frame titles in case this is the second frame.  */
		    record_asynch_buffer_change ();
		}
	      else
		{
		  HDC hdc = get_frame_dc (f);

		  /* Erase background again for safety.  */
		  w32_clear_rect (f, hdc, &msg.rect);
		  release_frame_dc (f, hdc);
		  expose_frame (f,
				msg.rect.left,
				msg.rect.top,
				msg.rect.right - msg.rect.left,
				msg.rect.bottom - msg.rect.top);
		}
	    }
	  break;

	case WM_INPUTLANGCHANGE:
	  /* Generate a language change event.  */
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	  
	      bufp->kind = language_change_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->arg = Qnil;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.msg.lParam & 0xffff;
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
		{
		  dpyinfo->mouse_face_hidden = 1;
		  clear_mouse_face (dpyinfo);
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      bufp->kind = non_ascii_keystroke;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.dwModifiers;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->arg = Qnil;
	      bufp->timestamp = msg.msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;

	case WM_SYSCHAR:
	case WM_CHAR:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
		{
		  dpyinfo->mouse_face_hidden = 1;
		  clear_mouse_face (dpyinfo);
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      bufp->kind = ascii_keystroke;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.dwModifiers;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->arg = Qnil;
	      bufp->timestamp = msg.msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;

	case WM_MOUSEMOVE:
          previous_help_echo = help_echo;
          help_echo_object = help_echo_window = Qnil;
          help_echo_pos = -1;

	  if (dpyinfo->grabbed && last_mouse_frame
	      && FRAME_LIVE_P (last_mouse_frame))
	    f = last_mouse_frame;
	  else
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (dpyinfo->mouse_face_hidden)
	    {
	      dpyinfo->mouse_face_hidden = 0;
	      clear_mouse_face (dpyinfo);
	    }

	  if (f)
	    note_mouse_movement (f, &msg.msg);
	  else
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (dpyinfo);
            }

          /* If the contents of the global variable help_echo
             has changed, generate a HELP_EVENT.  */
          if (help_echo != previous_help_echo)
            {
              Lisp_Object frame;
              int n;

              if (f)
                XSETFRAME (frame, f);
              else
                frame = Qnil;

              any_help_event_p = 1;
              n = gen_help_event (bufp, numchars, help_echo, frame,
				  help_echo_window, help_echo_object,
				  help_echo_pos);
              bufp += n, count += n, numchars -= n;
            }
          break;

	case WM_LBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_MBUTTONDOWN:
	case WM_MBUTTONUP:
	case WM_RBUTTONDOWN:
	case WM_RBUTTONUP:
	case WM_XBUTTONDOWN:
	case WM_XBUTTONUP:
	  {
            /* If we decide we want to generate an event to be seen
               by the rest of Emacs, we put it here.  */
            struct input_event emacs_event;
            int tool_bar_p = 0;
	    int button;
	    int up;

            emacs_event.kind = no_event;
	    
	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	    
	    if (f)
	      {
                construct_mouse_click (&emacs_event, &msg, f);
                
                /* Is this in the tool-bar?  */
                if (WINDOWP (f->tool_bar_window)
                    && XFASTINT (XWINDOW (f->tool_bar_window)->height))
                  {
                    Lisp_Object window;
                    int p, x, y;

		    x = XFASTINT (emacs_event.x);
		    y = XFASTINT (emacs_event.y);

                    /* Set x and y.  */
                    window = window_from_coordinates (f, x, y, &p, 1);

                    if (EQ (window, f->tool_bar_window))
                      {
                        w32_handle_tool_bar_click (f, &emacs_event);
                        tool_bar_p = 1;
                      }
                  }

                if (!tool_bar_p)
                  if (!dpyinfo->w32_focus_frame
                      || f == dpyinfo->w32_focus_frame
                      && (numchars >= 1))
                    {
                      construct_mouse_click (bufp, &msg, f);
                      bufp++;
                      count++;
                      numchars--;
                    }
	      }
	    
	    parse_button (msg.msg.message, HIWORD (msg.msg.wParam),
			  &button, &up);

	    if (up)
	      {
		dpyinfo->grabbed &= ~ (1 << button);
	      }
	    else
	      {
		dpyinfo->grabbed |= (1 << button);
		last_mouse_frame = f;
                /* Ignore any mouse motion that happened
                   before this event; any subsequent mouse-movement
                   Emacs events should reflect only motion after
                   the ButtonPress.  */
                if (f != 0)
                  f->mouse_moved = 0;

                if (!tool_bar_p)
                  last_tool_bar_item = -1;
	      }
	    break;
	  }
	  
      case WM_MOUSEWHEEL:
          if (dpyinfo->grabbed && last_mouse_frame
              && FRAME_LIVE_P (last_mouse_frame))
            f = last_mouse_frame;
          else
            f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

          if (f)
            {
              if ((!dpyinfo->w32_focus_frame 
                   || f == dpyinfo->w32_focus_frame)
                  && (numchars >= 1))
                {
                  construct_mouse_wheel (bufp, &msg, f);
                  bufp++;
                  count++;
                  numchars--;
                }
            }
	  break;

	case WM_DROPFILES:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      construct_drag_n_drop (bufp, &msg, f);
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_VSCROLL:
	  {
	    struct scroll_bar *bar =
	      x_window_to_scroll_bar ((HWND)msg.msg.lParam);
	      
	    if (bar && numchars >= 1)
	      {
		if (w32_scroll_bar_handle_click (bar, &msg, bufp))
		  {
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
	    break;
	  }
	  
	case WM_WINDOWPOSCHANGED:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    {
	      x_check_fullscreen_move(f);
	      if (f->output_data.w32->want_fullscreen & FULLSCREEN_WAIT)
		f->output_data.w32->want_fullscreen &=
		  ~(FULLSCREEN_WAIT|FULLSCREEN_BOTH);
	    }
	  check_visibility = 1;
	  break;

	case WM_ACTIVATE:
	case WM_ACTIVATEAPP:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    x_check_fullscreen (f);
	  check_visibility = 1;
	  break;

	case WM_MOVE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->async_iconified)
	    {
	      int x, y;

	      x_real_positions (f, &x, &y);
	      f->output_data.w32->left_pos = x;
	      f->output_data.w32->top_pos = y;
	    }

	  check_visibility = 1;
	  break;

	case WM_SHOWWINDOW:
	  /* wParam non-zero means Window is about to be shown, 0 means
	     about to be hidden.  */
	  /* Redo the mouse-highlight after the tooltip has gone.  */
	  if (!msg.msg.wParam && msg.msg.hwnd == tip_window)
	    {
	      tip_window = NULL;
	      redo_mouse_highlight ();
	    }

	  /* If window has been obscured or exposed by another window
	     being maximised or minimised/restored, then recheck
	     visibility of all frames.  Direct changes to our own
	     windows get handled by WM_SIZE.  */
#if 0
	  if (msg.msg.lParam != 0)
	    check_visibility = 1;
	  else
	    {
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      f->async_visible = msg.msg.wParam;
	    }
#endif

	  check_visibility = 1;
	  break;

	case WM_SIZE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  /* Inform lisp of whether frame has been iconified etc. */
	  if (f)
	    {
	      switch (msg.msg.wParam)
		{
		case SIZE_MINIMIZED:
		  f->async_visible = 0;
		  f->async_iconified = 1;
		  
		  bufp->kind = iconify_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp->arg = Qnil;
		  bufp++;
		  count++;
		  numchars--;
		  break;

		case SIZE_MAXIMIZED:
		case SIZE_RESTORED:
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  
		  /* wait_reading_process_input will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);
		  
		  if (f->iconified)
		    {
                      int x, y;

                      /* Reset top and left positions of the Window
                         here since Windows sends a WM_MOVE message
                         BEFORE telling us the Window is minimized
                         when the Window is iconified, with 3000,3000
                         as the co-ords. */
                      x_real_positions (f, &x, &y);
                      f->output_data.w32->left_pos = x;
                      f->output_data.w32->top_pos = y;

		      bufp->kind = deiconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp->arg = Qnil;
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else if (! NILP (Vframe_list)
			   && ! NILP (XCDR (Vframe_list)))
		    /* Force a redisplay sooner or later
		       to update the frame titles
		       in case this is the second frame.  */
		    record_asynch_buffer_change ();
		  break;
		}
	    }

	  if (f && !f->async_iconified && msg.msg.wParam != SIZE_MINIMIZED)
	    {
	      RECT rect;
	      int rows;
	      int columns;
	      int width;
	      int height;
	      
	      GetClientRect (msg.msg.hwnd, &rect);
	      
	      height = rect.bottom - rect.top;
	      width = rect.right - rect.left;
	      
	      rows = PIXEL_TO_CHAR_HEIGHT (f, height);
	      columns = PIXEL_TO_CHAR_WIDTH (f, width);
	      
	      /* TODO: Clip size to the screen dimensions.  */
	      
	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */
	      
	      if (columns != f->width
		  || rows != f->height
		  || width != f->output_data.w32->pixel_width
		  || height != f->output_data.w32->pixel_height)
		{
		  change_frame_size (f, rows, columns, 0, 1, 0);
		  SET_FRAME_GARBAGED (f);
		  cancel_mouse_face (f);
		  f->output_data.w32->pixel_width = width;
		  f->output_data.w32->pixel_height = height;
		  f->output_data.w32->win_gravity = NorthWestGravity;
		}
	    }

	  check_visibility = 1;
	  break;

	case WM_MOUSELEAVE:
	  f = x_any_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    {
	      if (f == dpyinfo->mouse_face_mouse_frame)
		{
		  /* If we move outside the frame, then we're
		     certainly no longer on any text in the frame.  */
		  clear_mouse_face (dpyinfo);
		  dpyinfo->mouse_face_mouse_frame = 0;
		}

	      /* Generate a nil HELP_EVENT to cancel a help-echo.
		 Do it only if there's something to cancel.
		 Otherwise, the startup message is cleared when
		 the mouse leaves the frame.  */
	      if (any_help_event_p)
		{
		  Lisp_Object frame;
		  int n;

		  XSETFRAME (frame, f);
		  help_echo = Qnil;
		  n = gen_help_event (bufp, numchars,
				      Qnil, frame, Qnil, Qnil, 0);
		  bufp += n, count += n, numchars -= n;
		}
	    }
	  break;
	      
	case WM_SETFOCUS:
	  f = x_any_window_to_frame (dpyinfo, msg.msg.hwnd);

          dpyinfo->w32_focus_event_frame = f;
          
          if (f)
            x_new_focus_frame (dpyinfo, f);


	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_KILLFOCUS:
          /* TODO: some of this belongs in MOUSE_LEAVE */
	  f = x_top_window_to_frame (dpyinfo, msg.msg.hwnd);

          if (f)
            {
              if (f == dpyinfo->w32_focus_event_frame)
                dpyinfo->w32_focus_event_frame = 0;

              if (f == dpyinfo->w32_focus_frame)
                x_new_focus_frame (dpyinfo, 0);

              if (f == dpyinfo->mouse_face_mouse_frame)
                {
                  /* If we move outside the frame, then we're
                     certainly no longer on any text in the frame.  */
                  clear_mouse_face (dpyinfo);
                  dpyinfo->mouse_face_mouse_frame = 0;
                }

              /* Generate a nil HELP_EVENT to cancel a help-echo.
                 Do it only if there's something to cancel.
                 Otherwise, the startup message is cleared when
                 the mouse leaves the frame.  */
              if (any_help_event_p)
                {
                  Lisp_Object frame;
                  int n;

                  XSETFRAME (frame, f);
                  help_echo = Qnil;
                  n = gen_help_event (bufp, numchars,
                                      Qnil, frame, Qnil, Qnil, 0);
                  bufp += n, count += n, numchars -=n;
                }
            }

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_CLOSE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	      
	      bufp->kind = delete_window_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->arg = Qnil;
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_INITMENU:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	  
	      bufp->kind = menu_bar_activate_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->arg = Qnil;
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_COMMAND:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      extern void menubar_selection_callback
		(FRAME_PTR f, void * client_data);
	      menubar_selection_callback (f, (void *)msg.msg.wParam);
	    }

	  check_visibility = 1;
	  break;

	case WM_DISPLAYCHANGE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f) 
	    {
	      dpyinfo->width = (short) LOWORD (msg.msg.lParam);
	      dpyinfo->height = (short) HIWORD (msg.msg.lParam);
	      dpyinfo->n_cbits = msg.msg.wParam;
	      DebPrint (("display change: %d %d\n", dpyinfo->width,
			 dpyinfo->height));
	    }
	  
	  check_visibility = 1;
	  break;

	default:
	  /* Check for messages registered at runtime. */
	  if (msg.msg.message == msh_mousewheel)
	    {
	      if (dpyinfo->grabbed && last_mouse_frame 
		  && FRAME_LIVE_P (last_mouse_frame))
		f = last_mouse_frame;
	      else
		f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
          
	      if (f)
		{
		  if ((!dpyinfo->w32_focus_frame 
		       || f == dpyinfo->w32_focus_frame)
		      && (numchars >= 1))
		    {
		      construct_mouse_wheel (bufp, &msg, f);
		      bufp++;
		      count++;
		      numchars--;
		    }
		}
	    }
	  break;
	}
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  /* Check which frames are still visisble, if we have enqueued any user
     events or been notified of events that may affect visibility.  We
     do this here because there doesn't seem to be any direct
     notification from Windows that the visibility of a window has
     changed (at least, not in all cases).  */
  if (count > 0 || check_visibility)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
	{
	  FRAME_PTR f = XFRAME (frame);
	  /* The tooltip has been drawn already.  Avoid the
	     SET_FRAME_GARBAGED below.  */
	  if (EQ (frame, tip_frame))
	    continue;

	  /* Check "visible" frames and mark each as obscured or not.
	     Note that async_visible is nonzero for unobscured and
	     obscured frames, but zero for hidden and iconified frames.  */
	  if (FRAME_W32_P (f) && f->async_visible)
	    {
	      RECT clipbox;
	      HDC  hdc;

	      enter_crit ();
	      /* Query clipping rectangle for the entire window area
                 (GetWindowDC), not just the client portion (GetDC).
                 Otherwise, the scrollbars and menubar aren't counted as
                 part of the visible area of the frame, and we may think
                 the frame is obscured when really a scrollbar is still
                 visible and gets WM_PAINT messages above.  */
	      hdc = GetWindowDC (FRAME_W32_WINDOW (f));
	      GetClipBox (hdc, &clipbox);
	      ReleaseDC (FRAME_W32_WINDOW (f), hdc);
	      leave_crit ();

	      if (clipbox.right == clipbox.left
		  || clipbox.bottom == clipbox.top)
		{
		  /* Frame has become completely obscured so mark as
		     such (we do this by setting async_visible to 2 so
		     that FRAME_VISIBLE_P is still true, but redisplay
		     will skip it).  */
		  f->async_visible = 2;

		  if (!FRAME_OBSCURED_P (f))
		    {
		      DebPrint (("frame %p (%s) obscured\n", f,
				 XSTRING (f->name)->data));
		    }
		}
	      else
		{
		  /* Frame is not obscured, so mark it as such.  */
		  f->async_visible = 1;

		  if (FRAME_OBSCURED_P (f))
		    {
		      SET_FRAME_GARBAGED (f);
		      DebPrint (("obscured frame %p (%s) found to be visible\n", f,
				 XSTRING (f->name)->data));

		      /* Force a redisplay sooner or later.  */
		      record_asynch_buffer_change ();
		    }
		}
	    }
	}
    }

  UNBLOCK_INPUT;
  return count;
}




/***********************************************************************
			     Text Cursor
 ***********************************************************************/

/* Notice if the text cursor of window W has been overwritten by a
   drawing operation that outputs glyphs starting at START_X and
   ending at END_X in the line given by output_cursor.vpos.
   Coordinates are area-relative.  END_X < 0 means all the rest
   of the line after START_X has been written.  */

static void
notice_overwritten_cursor (w, area, x0, x1, y0, y1)
     struct window *w;
     enum glyph_row_area area;
     int x0, x1, y0, y1;
{
  if (area == TEXT_AREA
      && w->phys_cursor_on_p
      && y0 <= w->phys_cursor.y
      && y1 >= w->phys_cursor.y + w->phys_cursor_height
      && x0 <= w->phys_cursor.x
      && (x1 < 0 || x1 > w->phys_cursor.x))
    w->phys_cursor_on_p = 0;
}


/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.
   WHOLE_LINE_P non-zero means include the areas used for truncation
   mark display and alike in the clipping rectangle.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
w32_clip_to_row (w, row, hdc, whole_line_p)
     struct window *w;
     struct glyph_row *row;
     HDC hdc;
     int whole_line_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  RECT clip_rect;
  int window_x, window_y, window_width, window_height;

  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);

  clip_rect.left = WINDOW_TO_FRAME_PIXEL_X (w, 0);
  clip_rect.top = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  clip_rect.top = max (clip_rect.top, window_y);
  clip_rect.right = clip_rect.left + window_width;
  clip_rect.bottom = clip_rect.top + row->visible_height;

  /* If clipping to the whole line, including trunc marks, extend
     the rectangle to the left and increase its width.  */
  if (whole_line_p)
    {
      clip_rect.left -= FRAME_X_LEFT_FRINGE_WIDTH (f);
      clip_rect.right += FRAME_X_FRINGE_WIDTH (f);
    }

  w32_set_clip_rectangle (hdc, &clip_rect);
}


/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HDC hdc;
  RECT rect;
  int wd;
  struct glyph *cursor_glyph;
  HBRUSH hb = CreateSolidBrush (f->output_data.w32->cursor_pixel);

  /* Compute frame-relative coordinates from window-relative
     coordinates.  */
  rect.left = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
  rect.top = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
              + row->ascent - w->phys_cursor_ascent);
  rect.bottom = rect.top + row->height;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute the width of the rectangle to draw.  If on a stretch
     glyph, and `x-stretch-block-cursor' is nil, don't draw a
     rectangle as wide as the glyph, but use a canonical character
     width instead.  */
  wd = cursor_glyph->pixel_width;
  if (cursor_glyph->type == STRETCH_GLYPH
      && !x_stretch_cursor_p)
    wd = min (CANON_X_UNIT (f), wd);

  rect.right = rect.left + wd;
  hdc = get_frame_dc (f);
  FrameRect (hdc, &rect, hb);
  DeleteObject (hb);

  release_frame_dc (f, hdc);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (w, row, width)
     struct window *w;
     struct glyph_row *row;
     int width;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;
  int x;
  HDC hdc;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *row;
      row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      x_draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
    }
  else
    {
      COLORREF cursor_color = f->output_data.w32->cursor_pixel;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);

      if (width < 0)
        width = f->output_data.w32->cursor_width;

      /* If the glyph's background equals the color we normally draw
	 the bar cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == cursor_color)
	cursor_color = face->foreground;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
      hdc = get_frame_dc (f);
      w32_clip_to_row (w, row, hdc, 0);
      w32_fill_area (f, hdc, cursor_color, x,
                     WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
                     min (cursor_glyph->pixel_width, width),
                     row->height);
      release_frame_dc (f, hdc);
    }
}


/* Clear the cursor of window W to background color, and mark the
   cursor as not shown.  This is used when the text where the cursor
   is is about to be rewritten.  */

static void
x_clear_cursor (w)
     struct window *w;
{
  if (FRAME_VISIBLE_P (XFRAME (w->frame)) && w->phys_cursor_on_p)
    x_update_window_cursor (w, 0);
}


/* Draw the cursor glyph of window W in glyph row ROW.  See the
   comment of x_draw_glyphs for the meaning of HL.  */

static void
x_draw_phys_cursor_glyph (w, row, hl)
     struct window *w;
     struct glyph_row *row;
     enum draw_glyphs_face hl;
{
  /* If cursor hpos is out of bounds, don't draw garbage.  This can
     happen in mini-buffer windows when switching between echo area
     glyphs and mini-buffer.  */
  if (w->phys_cursor.hpos < row->used[TEXT_AREA])
    {
      int on_p = w->phys_cursor_on_p;
      x_draw_glyphs (w, w->phys_cursor.x, row, TEXT_AREA,
                     w->phys_cursor.hpos, w->phys_cursor.hpos + 1,
                     hl, 0);
      w->phys_cursor_on_p = on_p;

      /* When we erase the cursor, and ROW is overlapped by other
	 rows, make sure that these overlapping parts of other rows
	 are redrawn.  */
      if (hl == DRAW_NORMAL_TEXT && row->overlapped_p)
	{
	  if (row > w->current_matrix->rows
	      && MATRIX_ROW_OVERLAPS_SUCC_P (row - 1))
	    x_fix_overlapping_area (w, row - 1, TEXT_AREA);

	  if (MATRIX_ROW_BOTTOM_Y (row) < window_text_bottom_y (w)
	      && MATRIX_ROW_OVERLAPS_PRED_P (row + 1))
	    x_fix_overlapping_area (w, row + 1, TEXT_AREA);
	}
    }
}


/* Erase the image of a cursor of window W from the screen.  */

static void
x_erase_phys_cursor (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  int hpos = w->phys_cursor.hpos;
  int vpos = w->phys_cursor.vpos;
  int mouse_face_here_p = 0;
  struct glyph_matrix *active_glyphs = w->current_matrix;
  struct glyph_row *cursor_row;
  struct glyph *cursor_glyph;
  enum draw_glyphs_face hl;

  /* No cursor displayed or row invalidated => nothing to do on the
     screen.  */
  if (w->phys_cursor_type == NO_CURSOR)
    goto mark_cursor_off;
	   
  /* VPOS >= active_glyphs->nrows means that window has been resized.
     Don't bother to erase the cursor.  */
  if (vpos >= active_glyphs->nrows)
    goto mark_cursor_off;

  /* If row containing cursor is marked invalid, there is nothing we
     can do.  */
  cursor_row = MATRIX_ROW (active_glyphs, vpos);
  if (!cursor_row->enabled_p)
    goto mark_cursor_off;
  
  /* If row is completely invisible, don't attempt to delete a cursor which
     isn't there.  This may happen if cursor is at top of window, and
     we switch to a buffer with a header line in that window.  */
  if (cursor_row->visible_height <= 0)
    goto mark_cursor_off;
  
  /* This can happen when the new row is shorter than the old one.
     In this case, either x_draw_glyphs or clear_end_of_line
     should have cleared the cursor.  Note that we wouldn't be
     able to erase the cursor in this case because we don't have a
     cursor glyph at hand.  */
  if (w->phys_cursor.hpos >= cursor_row->used[TEXT_AREA])
    goto mark_cursor_off;
	 
  /* If the cursor is in the mouse face area, redisplay that when
     we clear the cursor.  */
  if (! NILP (dpyinfo->mouse_face_window)
      && w == XWINDOW (dpyinfo->mouse_face_window)
      && (vpos > dpyinfo->mouse_face_beg_row
	  || (vpos == dpyinfo->mouse_face_beg_row
	      && hpos >= dpyinfo->mouse_face_beg_col))
      && (vpos < dpyinfo->mouse_face_end_row
	  || (vpos == dpyinfo->mouse_face_end_row
	      && hpos < dpyinfo->mouse_face_end_col))
      /* Don't redraw the cursor's spot in mouse face if it is at the
	 end of a line (on a newline).  The cursor appears there, but
	 mouse highlighting does not.  */
      && cursor_row->used[TEXT_AREA] > hpos)
    mouse_face_here_p = 1;

  /* Maybe clear the display under the cursor.  */
  if (w->phys_cursor_type == HOLLOW_BOX_CURSOR)
    {
      int x;
      int header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      HDC hdc;

      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	goto mark_cursor_off;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

      hdc = get_frame_dc (f);
      w32_clear_area (f, hdc, x,
                      WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
                                                       cursor_row->y)),
                      cursor_glyph->pixel_width,
                      cursor_row->visible_height);
      release_frame_dc (f, hdc);
    }
  
  /* Erase the cursor by redrawing the character underneath it.  */
  if (mouse_face_here_p)
    hl = DRAW_MOUSE_FACE;
  else
    hl = DRAW_NORMAL_TEXT;
  x_draw_phys_cursor_glyph (w, cursor_row, hl);

 mark_cursor_off:
  w->phys_cursor_on_p = 0;
  w->phys_cursor_type = NO_CURSOR;
}


/* Non-zero if physical cursor of window W is within mouse face.  */

static int
cursor_in_mouse_face_p (w)
     struct window *w;
{
  struct w32_display_info *dpyinfo
    = FRAME_W32_DISPLAY_INFO (XFRAME (w->frame));
  int in_mouse_face = 0;
  
  if (WINDOWP (dpyinfo->mouse_face_window)
      && XWINDOW (dpyinfo->mouse_face_window) == w)
    {
      int hpos = w->phys_cursor.hpos;
      int vpos = w->phys_cursor.vpos;

      if (vpos >= dpyinfo->mouse_face_beg_row
	  && vpos <= dpyinfo->mouse_face_end_row
	  && (vpos > dpyinfo->mouse_face_beg_row
	      || hpos >= dpyinfo->mouse_face_beg_col)
	  && (vpos < dpyinfo->mouse_face_end_row
	      || hpos < dpyinfo->mouse_face_end_col
	      || dpyinfo->mouse_face_past_end))
	in_mouse_face = 1;
    }

  return in_mouse_face;
}


/* Display or clear cursor of window W.  If ON is zero, clear the
   cursor.  If it is non-zero, display the cursor.  If ON is nonzero,
   where to put the cursor is specified by HPOS, VPOS, X and Y.  */

void
x_display_and_set_cursor (w, on, hpos, vpos, x, y)
     struct window *w;
     int on, hpos, vpos, x, y;
{
  struct frame *f = XFRAME (w->frame);
  int new_cursor_type;
  int new_cursor_width;
  struct glyph_matrix *current_glyphs;
  struct glyph_row *glyph_row;
  struct glyph *glyph;
  int cursor_non_selected;
  int active_cursor = 1;

  /* This is pointless on invisible frames, and dangerous on garbaged
     windows and frames; in the latter case, the frame or window may
     be in the midst of changing its size, and x and y may be off the
     window.  */
  if (! FRAME_VISIBLE_P (f)
      || FRAME_GARBAGED_P (f)
      || vpos >= w->current_matrix->nrows
      || hpos >= w->current_matrix->matrix_w)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && !w->phys_cursor_on_p)
    return;

  current_glyphs = w->current_matrix;
  glyph_row = MATRIX_ROW (current_glyphs, vpos);
  glyph = glyph_row->glyphs[TEXT_AREA] + hpos;
  
  /* If cursor row is not enabled, we don't really know where to 
     display the cursor.  */
  if (!glyph_row->enabled_p)
    {
      w->phys_cursor_on_p = 0;
      return;
    }

  xassert (interrupt_input_blocked);

  /* Set new_cursor_type to the cursor we want to be displayed.  In a
     mini-buffer window, we want the cursor only to appear if we are
     reading input from this window.  For the selected window, we want
     the cursor type given by the frame parameter.  If explicitly
     marked off, draw no cursor.  In all other cases, we want a hollow
     box cursor.  */
  cursor_non_selected 
    = !NILP (Fbuffer_local_value (Qcursor_in_non_selected_windows,
				  w->buffer));
  new_cursor_width = -1;
  if (cursor_in_echo_area
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
    {
      if (w == XWINDOW (echo_area_window))
	new_cursor_type = FRAME_DESIRED_CURSOR (f);
      else
	{
	  if (cursor_non_selected)
	    new_cursor_type = HOLLOW_BOX_CURSOR;
	  else
	    new_cursor_type = NO_CURSOR;
	  active_cursor = 0;
	}
    }
  else
    {
      if (f != FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame
          || w != XWINDOW (f->selected_window))
        {
	  active_cursor = 0;

          if (MINI_WINDOW_P (w) 
              || !cursor_non_selected
              || NILP (XBUFFER (w->buffer)->cursor_type))
            new_cursor_type = NO_CURSOR;
          else
            new_cursor_type = HOLLOW_BOX_CURSOR;
        }
      else
        {
	  struct buffer *b = XBUFFER (w->buffer);

	  if (EQ (b->cursor_type, Qt))
            new_cursor_type = FRAME_DESIRED_CURSOR (f);
	  else
	    new_cursor_type = x_specified_cursor_type (b->cursor_type, 
						       &new_cursor_width);
	  if (w->cursor_off_p)
	    {
	      if (new_cursor_type == FILLED_BOX_CURSOR)
		new_cursor_type = HOLLOW_BOX_CURSOR;
	      else if (new_cursor_type == BAR_CURSOR && new_cursor_width > 1)
		new_cursor_width = 1;
	      else
		new_cursor_type = NO_CURSOR;
	    }
	}
    }

  /* If cursor is currently being shown and we don't want it to be or
     it is in the wrong place, or the cursor type is not what we want,
     erase it.  */
  if (w->phys_cursor_on_p
      && (!on
	  || w->phys_cursor.x != x
	  || w->phys_cursor.y != y
	  || new_cursor_type != w->phys_cursor_type
	  || (new_cursor_type == BAR_CURSOR
	      && new_cursor_width != w->phys_cursor_width)))
    x_erase_phys_cursor (w);

  /* If the cursor is now invisible and we want it to be visible,
     display it.  */
  if (on && !w->phys_cursor_on_p)
    {
      w->phys_cursor_ascent = glyph_row->ascent;
      w->phys_cursor_height = glyph_row->height;
      
      /* Set phys_cursor_.* before x_draw_.* is called because some
	 of them may need the information.  */
      w->phys_cursor.x = x;
      w->phys_cursor.y = glyph_row->y;
      w->phys_cursor.hpos = hpos;
      w->phys_cursor.vpos = vpos;

      /* If the user wants to use the system caret, make sure our own
	 cursor remains invisible.  */
      if (w32_use_visible_system_caret)
	{
	  if (w->phys_cursor_type != NO_CURSOR)
	    x_erase_phys_cursor (w);

	  new_cursor_type = w->phys_cursor_type = NO_CURSOR;
	  w->phys_cursor_width = -1;
	}
      else
	{
	  w->phys_cursor_type = new_cursor_type;
	  w->phys_cursor_width = new_cursor_width;
	}

      w->phys_cursor_on_p = 1;

      /* If this is the active cursor, we need to track it with the
	 system caret, so third party software like screen magnifiers
	 and speech synthesizers can follow the cursor.  */
      if (active_cursor)
	{
	  HWND hwnd = FRAME_W32_WINDOW (f);

	  w32_system_caret_x
	    = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
	  w32_system_caret_y
	    = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
	       + glyph_row->ascent - w->phys_cursor_ascent);

	  /* If the size of the active cursor changed, destroy the old
	     system caret.  */
	  if (w32_system_caret_hwnd
	      && (w32_system_caret_height != w->phys_cursor_height))
	    PostMessage (hwnd, WM_EMACS_DESTROY_CARET, 0, 0);

	  w32_system_caret_height = w->phys_cursor_height;

	  /* Move the system caret.  */
	  PostMessage (hwnd, WM_EMACS_TRACK_CARET, 0, 0);
	}

      switch (new_cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  x_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  x_draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, new_cursor_width);
	  break;

	case NO_CURSOR:
	  break;

	default:
	  abort ();
	}
    }
}


/* Display the cursor on window W, or clear it.  X and Y are window
   relative pixel coordinates.  HPOS and VPOS are glyph matrix
   positions.  If W is not the selected window, display a hollow
   cursor.  ON non-zero means display the cursor at X, Y which
   correspond to HPOS, VPOS, otherwise it is cleared.  */

void
x_display_cursor (w, on, hpos, vpos, x, y)
     struct window *w;
     int on, hpos, vpos, x, y;
{
  BLOCK_INPUT;
  x_display_and_set_cursor (w, on, hpos, vpos, x, y);
  UNBLOCK_INPUT;
}


/* Display the cursor on window W, or clear it, according to ON_P.
   Don't change the cursor's position.  */

void
x_update_cursor (f, on_p)
     struct frame *f;
     int on_p;
{
  x_update_cursor_in_window_tree (XWINDOW (f->root_window), on_p);
}


/* Call x_update_window_cursor with parameter ON_P on all leaf windows
   in the window tree rooted at W.  */

static void
x_update_cursor_in_window_tree (w, on_p)
     struct window *w;
     int on_p;
{
  while (w)
    {
      if (!NILP (w->hchild))
	x_update_cursor_in_window_tree (XWINDOW (w->hchild), on_p);
      else if (!NILP (w->vchild))
	x_update_cursor_in_window_tree (XWINDOW (w->vchild), on_p);
      else
	x_update_window_cursor (w, on_p);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Switch the display of W's cursor on or off, according to the value
   of ON.  */

static void
x_update_window_cursor (w, on)
     struct window *w;
     int on;
{
  /* Don't update cursor in windows whose frame is in the process
     of being deleted.  */
  if (w->current_matrix)
    {
      BLOCK_INPUT;
      x_display_and_set_cursor (w, on, w->phys_cursor.hpos,
                                w->phys_cursor.vpos, w->phys_cursor.x,
                                w->phys_cursor.y);
      UNBLOCK_INPUT;
    }
}




/* Icons.  */

int
x_bitmap_icon (f, icon)
     struct frame *f;
     Lisp_Object icon;
{
  HANDLE hicon;

  if (FRAME_W32_WINDOW (f) == 0)
    return 1;

  if (NILP (icon))
    hicon = LoadIcon (hinst, EMACS_CLASS);
  else if (STRINGP (icon))
    hicon = LoadImage (NULL, (LPCTSTR) XSTRING (icon)->data, IMAGE_ICON, 0, 0,
		       LR_DEFAULTSIZE | LR_LOADFROMFILE);
  else if (SYMBOLP (icon))
    {
      LPCTSTR name;

      if (EQ (icon, intern ("application")))
	name = (LPCTSTR) IDI_APPLICATION;
      else if (EQ (icon, intern ("hand")))
	name = (LPCTSTR) IDI_HAND;
      else if (EQ (icon, intern ("question")))
	name = (LPCTSTR) IDI_QUESTION;
      else if (EQ (icon, intern ("exclamation")))
	name = (LPCTSTR) IDI_EXCLAMATION;
      else if (EQ (icon, intern ("asterisk")))
	name = (LPCTSTR) IDI_ASTERISK;
      else if (EQ (icon, intern ("winlogo")))
	name = (LPCTSTR) IDI_WINLOGO;
      else
	return 1;

      hicon = LoadIcon (NULL, name);
    }
  else
    return 1;

  if (hicon == NULL)
    return 1;

  PostMessage (FRAME_W32_WINDOW (f), WM_SETICON, (WPARAM) ICON_BIG,
               (LPARAM) hicon);

  return 0;
}


/************************************************************************
			  Handling X errors
 ************************************************************************/

/* Display Error Handling functions not used on W32. Listing them here
   helps diff stay in step when comparing w32term.c with xterm.c.

x_error_catcher (display, error)
x_catch_errors (dpy)
x_catch_errors_unwind (old_val)
x_check_errors (dpy, format)
x_had_errors_p (dpy)
x_clear_errors (dpy)
x_uncatch_errors (dpy, count)
x_trace_wire ()
x_connection_signal (signalnum)
x_connection_closed (dpy, error_message)
x_error_quitter (display, error)
x_error_handler (display, error)
x_io_error_quitter (display)

 */


/* Changing the font of the frame.  */

/* Give frame F the font named FONTNAME as its default font, and
   return the full name of that font.  FONTNAME may be a wildcard
   pattern; in that case, we choose some font that fits the pattern.
   The return value shows which font we chose.  */

Lisp_Object
x_new_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  struct font_info *fontp
    = FS_LOAD_FONT (f, 0, fontname, -1);

  if (!fontp)
    return Qnil;

  FRAME_FONT (f) = (XFontStruct *) (fontp->font);
  FRAME_BASELINE_OFFSET (f) = fontp->baseline_offset;
  FRAME_FONTSET (f) = -1;

  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FONT_WIDTH (FRAME_FONT (f));
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    {
      int wid = FONT_WIDTH (FRAME_FONT (f));
      f->scroll_bar_cols = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_W32_WINDOW (f) != 0)
    {
      frame_update_line_height (f);
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
        x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->output_data.w32->line_height = FONT_HEIGHT (FRAME_FONT (f));

  return build_string (fontp->full_name);
}

/* Give frame F the fontset named FONTSETNAME as its default font, and
   return the full name of that fontset.  FONTSETNAME may be a wildcard
   pattern; in that case, we choose some fontset that fits the pattern.
   The return value shows which fontset we chose.  */

Lisp_Object
x_new_fontset (f, fontsetname)
     struct frame *f;
     char *fontsetname;
{
  int fontset = fs_query_fontset (build_string (fontsetname), 0);
  Lisp_Object result;

  if (fontset < 0)
    return Qnil;

  if (FRAME_FONTSET (f) == fontset)
    /* This fontset is already set in frame F.  There's nothing more
       to do.  */
    return fontset_name (fontset);

  result = x_new_font (f, (XSTRING (fontset_ascii (fontset))->data));

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  FRAME_FONTSET(f) = fontset;

  return build_string (fontsetname);
}

/* Compute actual fringe widths */

void
x_compute_fringe_widths (f, redraw)
     struct frame *f;
     int redraw;
{
  int o_left = f->output_data.w32->left_fringe_width;
  int o_right = f->output_data.w32->right_fringe_width;
  int o_cols = f->output_data.w32->fringe_cols;

  Lisp_Object left_fringe = Fassq (Qleft_fringe, f->param_alist);
  Lisp_Object right_fringe = Fassq (Qright_fringe, f->param_alist);
  int left_fringe_width, right_fringe_width;

  if (!NILP (left_fringe))
    left_fringe = Fcdr (left_fringe);
  if (!NILP (right_fringe))
    right_fringe = Fcdr (right_fringe);

  left_fringe_width = ((NILP (left_fringe) || !INTEGERP (left_fringe)) ? 8 :
		       XINT (left_fringe));
  right_fringe_width = ((NILP (right_fringe) || !INTEGERP (right_fringe)) ? 8 :
			XINT (right_fringe));

  if (left_fringe_width || right_fringe_width)
    {
      int left_wid = left_fringe_width >= 0 ? left_fringe_width : -left_fringe_width;
      int right_wid = right_fringe_width >= 0 ? right_fringe_width : -right_fringe_width;
      int conf_wid = left_wid + right_wid;
      int font_wid = FONT_WIDTH (f->output_data.w32->font);
      int cols = (left_wid + right_wid + font_wid-1) / font_wid;
      int real_wid = cols * font_wid;
      if (left_wid && right_wid)
	{
	  if (left_fringe_width < 0)
	    {
	      /* Left fringe width is fixed, adjust right fringe if necessary */
	      f->output_data.w32->left_fringe_width = left_wid;
	      f->output_data.w32->right_fringe_width = real_wid - left_wid;
	    }
	  else if (right_fringe_width < 0)
	    {
	      /* Right fringe width is fixed, adjust left fringe if necessary */
	      f->output_data.w32->left_fringe_width = real_wid - right_wid;
	      f->output_data.w32->right_fringe_width = right_wid;
	    }
	  else
	    {
	      /* Adjust both fringes with an equal amount.
		 Note that we are doing integer arithmetic here, so don't
		 lose a pixel if the total width is an odd number.  */
	      int fill = real_wid - conf_wid;
	      f->output_data.w32->left_fringe_width = left_wid + fill/2;
	      f->output_data.w32->right_fringe_width = right_wid + fill - fill/2;
	    }
	}
      else if (left_fringe_width)
	{
	  f->output_data.w32->left_fringe_width = real_wid;
	  f->output_data.w32->right_fringe_width = 0;
	}
      else
	{
	  f->output_data.w32->left_fringe_width = 0;
	  f->output_data.w32->right_fringe_width = real_wid;
	}
      f->output_data.w32->fringe_cols = cols;
      f->output_data.w32->fringes_extra = real_wid;
    }
  else
    {
      f->output_data.w32->left_fringe_width = 0;
      f->output_data.w32->right_fringe_width = 0;
      f->output_data.w32->fringe_cols = 0;
      f->output_data.w32->fringes_extra = 0;
    }

  if (redraw && FRAME_VISIBLE_P (f))
    if (o_left != f->output_data.w32->left_fringe_width ||
	o_right != f->output_data.w32->right_fringe_width ||
	o_cols != f->output_data.w32->fringe_cols)
      redraw_frame (f);
}

/***********************************************************************
	TODO: W32 Input Methods
 ***********************************************************************/
/* Listing missing functions from xterm.c helps diff stay in step.

xim_destroy_callback (xim, client_data, call_data)
xim_open_dpy (dpyinfo, resource_name)
struct xim_inst_t
xim_instantiate_callback (display, client_data, call_data)
xim_initialize (dpyinfo, resource_name)
xim_close_dpy (dpyinfo)

 */


/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

void
x_calc_absolute_position (f)
     struct frame *f;
{
  POINT pt;
  int flags = f->output_data.w32->size_hint_flags;

  pt.x = pt.y = 0;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.
     But do this only if we will need the results.  */
  if (f->output_data.w32->parent_desc != FRAME_W32_DISPLAY_INFO (f)->root_window)
    {
      BLOCK_INPUT;
      MapWindowPoints (FRAME_W32_WINDOW (f),
		       f->output_data.w32->parent_desc,
		       &pt, 1);
      UNBLOCK_INPUT;
    }

  {
      RECT rt;
      rt.left = rt.right = rt.top = rt.bottom = 0;
      
      BLOCK_INPUT;
      AdjustWindowRect(&rt, f->output_data.w32->dwStyle,
		       FRAME_EXTERNAL_MENU_BAR (f));
      UNBLOCK_INPUT;

      pt.x += (rt.right - rt.left);
      pt.y += (rt.bottom - rt.top);
  }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.w32->left_pos = (FRAME_W32_DISPLAY_INFO (f)->width
			      - 2 * f->output_data.w32->border_width - pt.x
			      - PIXEL_WIDTH (f)
			      + f->output_data.w32->left_pos);

  if (flags & YNegative)
    f->output_data.w32->top_pos = (FRAME_W32_DISPLAY_INFO (f)->height
			     - 2 * f->output_data.w32->border_width - pt.y
			     - PIXEL_HEIGHT (f)
			     + f->output_data.w32->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.w32->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

void
x_set_offset (f, xoff, yoff, change_gravity)
     struct frame *f;
     register int xoff, yoff;
     int change_gravity;
{
  int modified_top, modified_left;

  if (change_gravity > 0)
    {
      f->output_data.w32->top_pos = yoff;
      f->output_data.w32->left_pos = xoff;
      f->output_data.w32->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.w32->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.w32->size_hint_flags |= YNegative;
      f->output_data.w32->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->output_data.w32->left_pos;
  modified_top = f->output_data.w32->top_pos;

  my_set_window_pos (FRAME_W32_WINDOW (f),
		     NULL,
		     modified_left, modified_top,
		     0, 0,
		     SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}


/* Check if we need to resize the frame due to a fullscreen request.
   If so needed, resize the frame. */
static void
x_check_fullscreen (f)
     struct frame *f;
{
  if (f->output_data.w32->want_fullscreen & FULLSCREEN_BOTH)
    {
      int width, height, ign;
                      
      x_real_positions (f, &f->output_data.w32->left_pos,
                        &f->output_data.w32->top_pos);

      x_fullscreen_adjust (f, &width, &height, &ign, &ign);
                  
      /* We do not need to move the window, it shall be taken care of
         when setting WM manager hints.
         If the frame is visible already, the position is checked by
         x_check_fullscreen_move. */
      if (f->width != width || f->height != height)
        {
          change_frame_size (f, height, width, 0, 1, 0);
          SET_FRAME_GARBAGED (f);
          cancel_mouse_face (f);

          /* Wait for the change of frame size to occur */
          f->output_data.w32->want_fullscreen |= FULLSCREEN_WAIT;
        }
    }
}

/* If frame parameters are set after the frame is mapped, we need to move
   the window.  This is done in xfns.c.
   Some window managers moves the window to the right position, some
   moves the outer window manager window to the specified position.
   Here we check that we are in the right spot.  If not, make a second
   move, assuming we are dealing with the second kind of window manager. */
static void
x_check_fullscreen_move (f)
     struct frame *f;
{
  if (f->output_data.w32->want_fullscreen & FULLSCREEN_MOVE_WAIT)
  {
    int expect_top = f->output_data.w32->top_pos;
    int expect_left = f->output_data.w32->left_pos;

    if (f->output_data.w32->want_fullscreen & FULLSCREEN_HEIGHT)
      expect_top = 0;
    if (f->output_data.w32->want_fullscreen & FULLSCREEN_WIDTH)
      expect_left = 0;
    
    if (expect_top != f->output_data.w32->top_pos
        || expect_left != f->output_data.w32->left_pos)
      x_set_offset (f, expect_left, expect_top, 1);

    /* Just do this once */
    f->output_data.w32->want_fullscreen &= ~FULLSCREEN_MOVE_WAIT;
  }
}


/* Calculate fullscreen size.  Return in *TOP_POS and *LEFT_POS the
   wanted positions of the WM window (not emacs window).
   Return in *WIDTH and *HEIGHT the wanted width and height of Emacs
   window (FRAME_X_WINDOW).
 */
void
x_fullscreen_adjust (f, width, height, top_pos, left_pos)
     struct frame *f;
     int *width;
     int *height;
     int *top_pos;
     int *left_pos;
{
  int newwidth = f->width, newheight = f->height;

  *top_pos = f->output_data.w32->top_pos;
  *left_pos = f->output_data.w32->left_pos;
  
  if (f->output_data.w32->want_fullscreen & FULLSCREEN_HEIGHT)
    {
      int ph;
      
      ph = FRAME_X_DISPLAY_INFO (f)->height;
      newheight = PIXEL_TO_CHAR_HEIGHT (f, ph);
      ph = CHAR_TO_PIXEL_HEIGHT (f, newheight)
        - f->output_data.w32->y_pixels_diff;
      newheight = PIXEL_TO_CHAR_HEIGHT (f, ph);
      *top_pos = 0;
    }

  if (f->output_data.w32->want_fullscreen & FULLSCREEN_WIDTH)
    {
      int pw;
      
      pw = FRAME_X_DISPLAY_INFO (f)->width;
      newwidth = PIXEL_TO_CHAR_WIDTH (f, pw);
      pw = CHAR_TO_PIXEL_WIDTH (f, newwidth)
        - f->output_data.w32->x_pixels_diff;
      newwidth = PIXEL_TO_CHAR_WIDTH (f, pw);
      *left_pos = 0;
    }

  *width = newwidth;
  *height = newheight;
}


/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

void
x_set_window_size (f, change_gravity, cols, rows)
     struct frame *f;
     int change_gravity;
     int cols, rows;
{
  int pixelwidth, pixelheight;
  
  BLOCK_INPUT;
  
  check_frame_size (f, &rows, &cols);
  f->output_data.w32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.w32->font)));

  x_compute_fringe_widths (f, 0);

  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);
  
  f->output_data.w32->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);
  
  {
    RECT rect;

    rect.left = rect.top = 0;
    rect.right = pixelwidth;
    rect.bottom = pixelheight;
      
    AdjustWindowRect(&rect, f->output_data.w32->dwStyle,
		     FRAME_EXTERNAL_MENU_BAR (f));
      
    my_set_window_pos (FRAME_W32_WINDOW (f),
		       NULL, 
		       0, 0,
		       rect.right - rect.left,
		       rect.bottom - rect.top,
		       SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
  }
  
  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.
     
     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the Lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.

     We pass 1 for DELAY since we can't run Lisp code inside of
     a BLOCK_INPUT.  */
  change_frame_size (f, rows, cols, 0, 1, 0);
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size. 
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  UNBLOCK_INPUT;
}

/* Mouse warping.  */

void x_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y);

void
x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->output_data.w32->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.w32->line_height / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > PIXEL_WIDTH (f)) pix_x = PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > PIXEL_HEIGHT (f)) pix_y = PIXEL_HEIGHT (f);

  x_set_mouse_pixel_position (f, pix_x, pix_y);
}

void
x_set_mouse_pixel_position (f, pix_x, pix_y)
     struct frame *f;
     int pix_x, pix_y;
{
  RECT rect;
  POINT pt;

  BLOCK_INPUT;

  GetClientRect (FRAME_W32_WINDOW (f), &rect);
  pt.x = rect.left + pix_x;
  pt.y = rect.top + pix_y;
  ClientToScreen (FRAME_W32_WINDOW (f), &pt);

  SetCursorPos (pt.x, pt.y);

  UNBLOCK_INPUT;
}


/* focus shifting, raising and lowering.  */

void
x_focus_on_frame (f)
     struct frame *f;
{
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  /* Give input focus to frame.  */
  BLOCK_INPUT;
#if 0
  /* Try not to change its Z-order if possible.  */
  if (x_window_to_frame (dpyinfo, GetForegroundWindow ()))
    my_set_focus (f, FRAME_W32_WINDOW (f));
  else
#endif
    my_set_foreground_window (FRAME_W32_WINDOW (f));
  UNBLOCK_INPUT;
}

void
x_unfocus_frame (f)
     struct frame *f;
{
}

/* Raise frame F.  */
void
x_raise_frame (f)
     struct frame *f;
{
  BLOCK_INPUT;

  /* Strictly speaking, raise-frame should only change the frame's Z
     order, leaving input focus unchanged.  This is reasonable behaviour
     on X where the usual policy is point-to-focus.  However, this
     behaviour would be very odd on Windows where the usual policy is
     click-to-focus.

     On X, if the mouse happens to be over the raised frame, it gets
     input focus anyway (so the window with focus will never be
     completely obscured) - if not, then just moving the mouse over it
     is sufficient to give it focus.  On Windows, the user must actually
     click on the frame (preferrably the title bar so as not to move
     point), which is more awkward.  Also, no other Windows program
     raises a window to the top but leaves another window (possibly now
     completely obscured) with input focus.

     Because there is a system setting on Windows that allows the user
     to choose the point to focus policy, we make the strict semantics
     optional, but by default we grab focus when raising.  */

  if (NILP (Vw32_grab_focus_on_raise))
    {
      /* The obvious call to my_set_window_pos doesn't work if Emacs is
	 not already the foreground application: the frame is raised
	 above all other frames belonging to us, but not above the
	 current top window.  To achieve that, we have to resort to this
	 more cumbersome method.  */

      HDWP handle = BeginDeferWindowPos (2);
      if (handle)
	{
	  DeferWindowPos (handle,
			  FRAME_W32_WINDOW (f),
  			  HWND_TOP,
  			  0, 0, 0, 0,
  			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  DeferWindowPos (handle,
			  GetForegroundWindow (),
			  FRAME_W32_WINDOW (f),
			  0, 0, 0, 0,
			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  EndDeferWindowPos (handle);
	}
    }
  else
    {
      my_set_foreground_window (FRAME_W32_WINDOW (f));
    }

  UNBLOCK_INPUT;
}

/* Lower frame F.  */
void
x_lower_frame (f)
     struct frame *f;
{
  BLOCK_INPUT;
  my_set_window_pos (FRAME_W32_WINDOW (f),
		     HWND_BOTTOM,
		     0, 0, 0, 0,
		     SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}

static void
w32_frame_raise_lower (f, raise_flag)
     FRAME_PTR f;
     int raise_flag;
{
  if (! FRAME_W32_P (f))
    return;

  if (raise_flag)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

void
x_make_frame_visible (f)
     struct frame *f;
{
  Lisp_Object type;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! f->output_data.w32->asked_for_visible)
	x_set_offset (f, f->output_data.w32->left_pos, f->output_data.w32->top_pos, 0);

      f->output_data.w32->asked_for_visible = 1;

/*      my_show_window (f, FRAME_W32_WINDOW (f), f->async_iconified ? SW_RESTORE : SW_SHOW);  */
      my_show_window (f, FRAME_W32_WINDOW (f), SW_SHOWNORMAL);
    }

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    int count;

    /* This must come after we set COUNT.  */
    UNBLOCK_INPUT;

    XSETFRAME (frame, f);

    /* Wait until the frame is visible.  Process X events until a
       MapNotify event has been seen, or until we think we won't get a
       MapNotify at all..  */
    for (count = input_signal_count + 10;
	 input_signal_count < count && !FRAME_VISIBLE_P (f);)
      {
	/* Force processing of queued events.  */
        /* TODO: x_sync equivalent?  */

	/* Machines that do polling rather than SIGIO have been observed
	   to go into a busy-wait here.  So we'll fake an alarm signal
	   to let the handler know that there's something to be read.
	   We used to raise a real alarm, but it seems that the handler
	   isn't always enabled here.  This is probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while processing
	       the fake one.  Turn it off and let the handler reset it.  */
	    int old_poll_suppress_count = poll_suppress_count;
	    poll_suppress_count = 1;
	    poll_for_input_1 ();
	    poll_suppress_count = old_poll_suppress_count;
	  }
      }
    FRAME_SAMPLE_VISIBILITY (f);
  }
}

/* Change from mapped state to withdrawn state. */

/* Make the frame visible (mapped and not iconified).  */

x_make_frame_invisible (f)
     struct frame *f;
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame = 0;
  
  BLOCK_INPUT;
  
  my_show_window (f, FRAME_W32_WINDOW (f), SW_HIDE);
  
  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;
  
  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */

void
x_iconify_frame (f)
     struct frame *f;
{
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  /* Simulate the user minimizing the frame.  */
  SendMessage (FRAME_W32_WINDOW (f), WM_SYSCOMMAND, SC_MINIMIZE, 0);

  UNBLOCK_INPUT;
}


/* Free X resources of frame F.  */

void
x_free_frame_resources (f)
     struct frame *f;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  BLOCK_INPUT;

  if (FRAME_W32_WINDOW (f))
    my_destroy_window (f, FRAME_W32_WINDOW (f));
      
  free_frame_menubar (f);

  unload_color (f, f->output_data.x->foreground_pixel);
  unload_color (f, f->output_data.x->background_pixel);
  unload_color (f, f->output_data.w32->cursor_pixel);
  unload_color (f, f->output_data.w32->cursor_foreground_pixel);
  unload_color (f, f->output_data.w32->border_pixel);
  unload_color (f, f->output_data.w32->mouse_pixel);
  if (f->output_data.w32->white_relief.allocated_p)
    unload_color (f, f->output_data.w32->white_relief.pixel);
  if (f->output_data.w32->black_relief.allocated_p)
    unload_color (f, f->output_data.w32->black_relief.pixel);

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);
      
  xfree (f->output_data.w32);
  f->output_data.w32 = NULL;
  
  if (f == dpyinfo->w32_focus_frame)
    dpyinfo->w32_focus_frame = 0;
  if (f == dpyinfo->w32_focus_event_frame)
    dpyinfo->w32_focus_event_frame = 0;
  if (f == dpyinfo->w32_highlight_frame)
    dpyinfo->w32_highlight_frame = 0;

  if (f == dpyinfo->mouse_face_mouse_frame)
    {
      dpyinfo->mouse_face_beg_row
	= dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row
	= dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
      dpyinfo->mouse_face_deferred_gc = 0;
      dpyinfo->mouse_face_mouse_frame = 0;
    }

  UNBLOCK_INPUT;
}


/* Destroy the window of frame F.  */

x_destroy_window (f)
     struct frame *f;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  x_free_frame_resources (f);

  dpyinfo->reference_count--;
}


/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the USPosition
   flag (this is useful when FLAGS is 0).  */
void
x_wm_set_size_hint (f, flags, user_position)
     struct frame *f;
     long flags;
     int user_position;
{
  Window window = FRAME_W32_WINDOW (f);

  enter_crit ();

  SetWindowLong (window, WND_FONTWIDTH_INDEX, FONT_WIDTH (f->output_data.w32->font));
  SetWindowLong (window, WND_LINEHEIGHT_INDEX, f->output_data.w32->line_height);
  SetWindowLong (window, WND_BORDER_INDEX, f->output_data.w32->internal_border_width);
  SetWindowLong (window, WND_SCROLLBAR_INDEX, f->output_data.w32->vertical_scroll_bar_extra);

  leave_crit ();
}

/* Window manager things */
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#if 0
  Window window = FRAME_W32_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
#endif
}


/***********************************************************************
				Fonts
 ***********************************************************************/

/* The following functions are listed here to help diff stay in step
   with xterm.c.  See w32fns.c for definitions.

x_get_font_info (f, font_idx)
x_list_fonts (f, pattern, size, maxnames)

 */

#if GLYPH_DEBUG

/* Check that FONT is valid on frame F.  It is if it can be found in F's
   font table.  */

static void
x_check_font (f, font)
     struct frame *f;
     XFontStruct *font;
{
  int i;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  xassert (font != NULL);

  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name 
	&& font == dpyinfo->font_table[i].font)
      break;

  xassert (i < dpyinfo->n_fonts);
}

#endif /* GLYPH_DEBUG != 0 */

/* Set *W to the minimum width, *H to the minimum font height of FONT.
   Note: There are (broken) X fonts out there with invalid XFontStruct
   min_bounds contents.  For example, handa@etl.go.jp reports that
   "-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1" fonts
   have font->min_bounds.width == 0.  */

static INLINE void
x_font_min_bounds (font, w, h)
     XFontStruct *font;
     int *w, *h;
{
  /*
   * TODO: Windows does not appear to offer min bound, only
   * average and maximum width, and maximum height.
   */
  *h = FONT_HEIGHT (font);
  *w = FONT_WIDTH (font);
}


/* Compute the smallest character width and smallest font height over
   all fonts available on frame F.  Set the members smallest_char_width
   and smallest_font_height in F's x_display_info structure to
   the values computed.  Value is non-zero if smallest_font_height or
   smallest_char_width become smaller than they were before.  */

int
x_compute_min_glyph_bounds (f)
     struct frame *f;
{
  int i;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  XFontStruct *font;
  int old_width = dpyinfo->smallest_char_width;
  int old_height = dpyinfo->smallest_font_height;
  
  dpyinfo->smallest_font_height = 100000;
  dpyinfo->smallest_char_width = 100000;
  
  for (i = 0; i < dpyinfo->n_fonts; ++i)
    if (dpyinfo->font_table[i].name)
      {
	struct font_info *fontp = dpyinfo->font_table + i;
	int w, h;
	
	font = (XFontStruct *) fontp->font;
	xassert (font != (XFontStruct *) ~0);
	x_font_min_bounds (font, &w, &h);
	
	dpyinfo->smallest_font_height = min (dpyinfo->smallest_font_height, h);
	dpyinfo->smallest_char_width = min (dpyinfo->smallest_char_width, w);
      }

  xassert (dpyinfo->smallest_char_width > 0
	   && dpyinfo->smallest_font_height > 0);

  return (dpyinfo->n_fonts == 1
	  || dpyinfo->smallest_char_width < old_width
	  || dpyinfo->smallest_font_height < old_height);
}

/* The following functions are listed here to help diff stay in step
   with xterm.c.  See w32fns.c for definitions.

x_load_font (f, fontname, size)
x_query_font (f, fontname)
x_find_ccl_program (fontp)

*/

/***********************************************************************
			    Initialization
 ***********************************************************************/

static int w32_initialized = 0;

void
w32_initialize_display_info (display_name)
     Lisp_Object display_name;
{
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  bzero (dpyinfo, sizeof (*dpyinfo));

  /* Put it on w32_display_name_list.  */
  w32_display_name_list = Fcons (Fcons (display_name, Qnil),
                                 w32_display_name_list);
  dpyinfo->name_list_element = XCAR (w32_display_name_list);
  
  dpyinfo->w32_id_name
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
  sprintf (dpyinfo->w32_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

  /* Default Console mode values - overridden when running in GUI mode
     with values obtained from system metrics.  */
  dpyinfo->resx = 1;
  dpyinfo->resy = 1;
  dpyinfo->height_in = 1;
  dpyinfo->width_in = 1;
  dpyinfo->n_planes = 1;
  dpyinfo->n_cbits = 4;
  dpyinfo->n_fonts = 0;
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  dpyinfo->mouse_face_hidden = 0;
  /* TODO: dpyinfo->gray */

}

struct w32_display_info *
w32_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  struct w32_display_info *dpyinfo;
  HDC hdc;
  
  BLOCK_INPUT;
  
  if (!w32_initialized)
    {
      w32_initialize ();
      w32_initialized = 1;
    }
  
  {
    int argc = 0;
    char *argv[3];

    argv[0] = "";
    argc = 1;
    if (xrm_option)
      {
	argv[argc++] = "-xrm";
	argv[argc++] = xrm_option;
      }
  }
  
  w32_initialize_display_info (display_name);

  dpyinfo = &one_w32_display_info;

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;
  
  hdc = GetDC (GetDesktopWindow ());

  dpyinfo->height = GetDeviceCaps (hdc, VERTRES);
  dpyinfo->width = GetDeviceCaps (hdc, HORZRES);
  dpyinfo->root_window = GetDesktopWindow ();
  dpyinfo->n_planes = GetDeviceCaps (hdc, PLANES);
  dpyinfo->n_cbits = GetDeviceCaps (hdc, BITSPIXEL);
  dpyinfo->resx = GetDeviceCaps (hdc, LOGPIXELSX);
  dpyinfo->resy = GetDeviceCaps (hdc, LOGPIXELSY);
  dpyinfo->has_palette = GetDeviceCaps (hdc, RASTERCAPS) & RC_PALETTE;
  dpyinfo->image_cache = make_image_cache ();
  dpyinfo->height_in = dpyinfo->height / dpyinfo->resx;
  dpyinfo->width_in = dpyinfo->width / dpyinfo->resy;
  ReleaseDC (GetDesktopWindow (), hdc);

  /* initialise palette with white and black */
  {
    COLORREF color;
    w32_defined_color (0, "white", &color, 1);
    w32_defined_color (0, "black", &color, 1);
  }

  /* Create Row Bitmaps and store them for later use.  */
  left_bmp = CreateBitmap (left_width, left_height, 1, 1, left_bits);
  ov_bmp = CreateBitmap (ov_width, ov_height, 1, 1, ov_bits);
  right_bmp = CreateBitmap (right_width, right_height, 1, 1, right_bits);
  continued_bmp = CreateBitmap (continued_width, continued_height, 1,
                                1, continued_bits);
  continuation_bmp = CreateBitmap (continuation_width, continuation_height,
                                   1, 1, continuation_bits);
  zv_bmp = CreateBitmap (zv_width, zv_height, 1, 1, zv_bits);

#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (connection, F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone.  */

void
x_delete_display (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  /* Discard this display from w32_display_name_list and w32_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (w32_display_name_list)
      && EQ (XCAR (w32_display_name_list), dpyinfo->name_list_element))
    w32_display_name_list = XCDR (w32_display_name_list);
  else
    {
      Lisp_Object tail;

      tail = w32_display_name_list;
      while (CONSP (tail) && CONSP (XCDR (tail)))
	{
	  if (EQ (XCAR (XCDR (tail)), dpyinfo->name_list_element))
	    {
	      XSETCDR (tail, XCDR (XCDR (tail)));
	      break;
	    }
	  tail = XCDR (tail);
	}
    }

  /* free palette table */
  {
    struct w32_palette_entry * plist;

    plist = dpyinfo->color_list;
    while (plist)
    {
      struct w32_palette_entry * pentry = plist;
      plist = plist->next;
      xfree (pentry);
    }
    dpyinfo->color_list = NULL;
    if (dpyinfo->palette)
      DeleteObject(dpyinfo->palette);
  }
  xfree (dpyinfo->font_table);
  xfree (dpyinfo->w32_id_name);

  /* Destroy row bitmaps.  */
  DeleteObject (left_bmp);
  DeleteObject (ov_bmp);
  DeleteObject (right_bmp);
  DeleteObject (continued_bmp);
  DeleteObject (continuation_bmp);
  DeleteObject (zv_bmp);
}

/* Set up use of W32.  */

DWORD w32_msg_worker ();

void
x_flush (struct frame * f)
{ /* Nothing to do */ }

static struct redisplay_interface w32_redisplay_interface =
{
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  x_scroll_run,
  x_after_update_window_line,
  x_update_window_begin,
  x_update_window_end,
  w32_cursor_to,
  x_flush,
  x_clear_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area
};

void
w32_initialize ()
{
  rif = &w32_redisplay_interface;

  /* MSVC does not type K&R functions with no arguments correctly, and
     so we must explicitly cast them.  */
  clear_frame_hook = (void (*)(void)) x_clear_frame;
  ring_bell_hook = (void (*)(void)) w32_ring_bell;
  update_begin_hook = x_update_begin;
  update_end_hook = x_update_end;

  read_socket_hook = w32_read_socket;

  frame_up_to_date_hook = w32_frame_up_to_date;

  mouse_position_hook = w32_mouse_position;
  frame_rehighlight_hook = w32_frame_rehighlight;
  frame_raise_lower_hook = w32_frame_raise_lower;
  set_vertical_scroll_bar_hook = w32_set_vertical_scroll_bar;
  condemn_scroll_bars_hook = w32_condemn_scroll_bars;
  redeem_scroll_bar_hook = w32_redeem_scroll_bar;
  judge_scroll_bars_hook = w32_judge_scroll_bars;
  estimate_mode_line_height_hook = x_estimate_mode_line_height;

  scroll_region_ok = 1;         /* we'll scroll partial frames */
  char_ins_del_ok = 1;
  line_ins_del_ok = 1;          /* we'll just blt 'em */
  fast_clear_end_of_line = 1;   /* X does this well */
  memory_below_frame = 0;       /* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  w32_system_caret_hwnd = NULL;
  w32_system_caret_height = 0;
  w32_system_caret_x = 0;
  w32_system_caret_y = 0;

  last_tool_bar_item = -1;
  any_help_event_p = 0;

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_number (2), Qnil);

  /* Create the window thread - it will terminate itself or when the app terminates */

  init_crit ();

  dwMainThreadId = GetCurrentThreadId ();
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (), 
		   GetCurrentProcess (), &hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);

  /* Wait for thread to start */

  {
    MSG msg;

    PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

    hWindowsThread = CreateThread (NULL, 0, 
			       (LPTHREAD_START_ROUTINE) w32_msg_worker, 
			       0, 0, &dwWindowsThreadId);

    GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
  }
  
  /* It is desirable that mainThread should have the same notion of
     focus window and active window as windowsThread.  Unfortunately, the
     following call to AttachThreadInput, which should do precisely what
     we need, causes major problems when Emacs is linked as a console
     program.  Unfortunately, we have good reasons for doing that, so
     instead we need to send messages to windowsThread to make some API
     calls for us (ones that affect, or depend on, the active/focus
     window state.  */
#ifdef ATTACH_THREADS
  AttachThreadInput (dwMainThreadId, dwWindowsThreadId, TRUE);
#endif

  /* Dynamically link to optional system components. */
  {
    HANDLE user_lib = LoadLibrary ("user32.dll");

#define LOAD_PROC(fn) pfn##fn = (void *) GetProcAddress (user_lib, #fn)

    /* New proportional scroll bar functions. */
    LOAD_PROC (SetScrollInfo);
    LOAD_PROC (GetScrollInfo);

#undef LOAD_PROC

    FreeLibrary (user_lib);

    /* If using proportional scroll bars, ensure handle is at least 5 pixels;
       otherwise use the fixed height.  */
    vertical_scroll_bar_min_handle = (pfnSetScrollInfo != NULL) ? 5 :
      GetSystemMetrics (SM_CYVTHUMB);

    /* For either kind of scroll bar, take account of the arrows; these
       effectively form the border of the main scroll bar range.  */
    vertical_scroll_bar_top_border = vertical_scroll_bar_bottom_border
      = GetSystemMetrics (SM_CYVSCROLL);
  }
}

void
syms_of_w32term ()
{
  staticpro (&w32_display_name_list);
  w32_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  DEFVAR_INT ("w32-num-mouse-buttons",
	      &Vw32_num_mouse_buttons,
	      doc: /* Number of physical mouse buttons.  */);
  Vw32_num_mouse_buttons = Qnil;

  DEFVAR_LISP ("w32-swap-mouse-buttons",
	      &Vw32_swap_mouse_buttons,
	       doc: /* Swap the mapping of middle and right mouse buttons.
When nil, middle button is mouse-2 and right button is mouse-3.  */);
  Vw32_swap_mouse_buttons = Qnil;

  DEFVAR_LISP ("w32-grab-focus-on-raise",
	       &Vw32_grab_focus_on_raise,
	       doc: /* Raised frame grabs input focus.
When t, `raise-frame' grabs input focus as well.  This fits well
with the normal Windows click-to-focus policy, but might not be
desirable when using a point-to-focus policy.  */);
  Vw32_grab_focus_on_raise = Qt;

  DEFVAR_LISP ("w32-capslock-is-shiftlock",
	       &Vw32_capslock_is_shiftlock,
	       doc: /* Apply CapsLock state to non character input keys.
When nil, CapsLock only affects normal character input keys.  */);
  Vw32_capslock_is_shiftlock = Qnil;

  DEFVAR_LISP ("w32-recognize-altgr",
	       &Vw32_recognize_altgr,
	       doc: /* Recognize right-alt and left-ctrl as AltGr.
When nil, the right-alt and left-ctrl key combination is
interpreted normally.  */); 
  Vw32_recognize_altgr = Qt;

  DEFVAR_BOOL ("w32-enable-unicode-output",
               &w32_enable_unicode_output,
               doc: /* Enable the use of Unicode for text output if non-nil.
Unicode output may prevent some third party applications for displaying
Far-East Languages on Windows 95/98 from working properly.
NT uses Unicode internally anyway, so this flag will probably have no
affect on NT machines.  */);
  w32_enable_unicode_output = 1;

  help_echo = Qnil;
  staticpro (&help_echo);
  help_echo_object = Qnil;
  staticpro (&help_echo_object);
  help_echo_window = Qnil;
  staticpro (&help_echo_window);
  previous_help_echo = Qnil;
  staticpro (&previous_help_echo);
  help_echo_pos = -1;

  DEFVAR_BOOL ("autoselect-window", &autoselect_window_p,
    doc: /* *Non-nil means autoselect window with mouse pointer.  */);
  autoselect_window_p = 0;

  DEFVAR_BOOL ("w32-use-visible-system-caret",
	       &w32_use_visible_system_caret,
	       doc: /* Flag to make the system caret visible.
When this is non-nil, Emacs will indicate the position of point by
using the system caret instead of drawing its own cursor.  Some screen
reader software does not track the system cursor properly when it is
invisible, and gets confused by Emacs drawing its own cursor, so this
variable is initialized to t when Emacs detects that screen reader 
software is running as it starts up.

When this variable is set, other variables affecting the appearance of
the cursor have no effect.  */);

  /* Initialize w32_use_visible_system_caret based on whether a screen
     reader is in use.  */
  if (!SystemParametersInfo (SPI_GETSCREENREADER, 0,
			     &w32_use_visible_system_caret, 0))
    w32_use_visible_system_caret = 0;

  DEFVAR_BOOL ("x-stretch-cursor", &x_stretch_cursor_p,
	       doc: /* *Non-nil means draw block cursor as wide as the glyph under it.
For example, if a block cursor is over a tab, it will be drawn as
wide as that tab on the display.  */);
  x_stretch_cursor_p = 0;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       &x_use_underline_position_properties,
	       doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.  */);
  x_use_underline_position_properties = 1;

  DEFVAR_LISP ("x-toolkit-scroll-bars", &Vx_toolkit_scroll_bars,
	       doc: /* If not nil, Emacs uses toolkit scroll bars.  */);
  Vx_toolkit_scroll_bars = Qt;

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;
}
