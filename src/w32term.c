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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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


/* Fringe bitmaps.  */

static int max_fringe_bmp = 0;
static HBITMAP *fringe_bmp = 0;

/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* Temporary variables for w32_read_socket.  */

static int last_mousemove_x = 0;
static int last_mousemove_y = 0;

/* Define GET_WHEEL_DELTA_WPARAM macro if system headers don't.  */
#ifndef GET_WHEEL_DELTA_WPARAM
#define GET_WHEEL_DELTA_WPARAM(wparam) ((short)HIWORD (wparam))
#endif

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Last window where we saw the mouse.  Used by mouse-autoselect-window.  */
static Lisp_Object last_window;

/* Non-zero means make use of UNDERLINE_POSITION font properties.
   (Not yet supported, see TODO in x_draw_glyph_string.)  */
int x_use_underline_position_properties;

extern unsigned int msh_mousewheel;

extern void free_frame_menubar ();

extern int w32_codepage_for_font (char *fontname);
extern Cursor w32_load_cursor (LPCTSTR name);

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

/* Flag to enable Cleartype hack for font metrics.  */
static int cleartype_active;

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

static RECT last_mouse_glyph;
static Lisp_Object last_mouse_press_frame;

int w32_num_mouse_buttons;

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

#ifndef USE_CRT_DLL
extern int errno;
#endif

/* A mask of extra modifier bits to put into every keyboard char.  */

extern EMACS_INT extra_keyboard_modifiers;

static void x_update_window_end P_ ((struct window *, int, int));
void w32_delete_display P_ ((struct w32_display_info *));
static void w32_handle_tool_bar_click P_ ((struct frame *,
                                          struct input_event *));
void w32_define_cursor P_ ((Window, Cursor));

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
static void x_update_end P_ ((struct frame *));
static void w32_frame_up_to_date P_ ((struct frame *));
static void w32_set_terminal_modes P_ ((void));
static void w32_reset_terminal_modes P_ ((void));
static void x_clear_frame P_ ((void));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void x_new_focus_frame P_ ((struct w32_display_info *,
				   struct frame *));
static void x_focus_changed P_ ((int, int, struct w32_display_info *,
				  struct frame *, struct input_event *));
static void w32_detect_focus_change P_ ((struct w32_display_info *,
                                       W32Msg *, struct input_event *));
static void w32_frame_rehighlight P_ ((struct frame *));
static void x_frame_rehighlight P_ ((struct w32_display_info *));
static void x_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void x_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int,
				   enum text_cursor_kinds));
static void w32_clip_to_row P_ ((struct window *, struct glyph_row *, int, HDC));

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


void
XChangeGC (void * ignore, XGCValues* gc, unsigned long mask,
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

void
XGetGCValues (void* ignore, XGCValues *gc,
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
  if (w32_use_visible_system_caret && w32_system_caret_hwnd)
    {
      SendMessage (w32_system_caret_hwnd, WM_EMACS_HIDE_CARET, 0, 0);
    }

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

/* Draw a vertical window border from (x,y0) to (x,y1)  */

static void
w32_draw_vertical_window_border (w, x, y0, y1)
     struct window *w;
     int x, y0, y1;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  RECT r;
  HDC hdc;
  struct face *face;

  r.left = x;
  r.right = x + 1;
  r.top = y0;
  r.bottom = y1;

  hdc = get_frame_dc (f);
  face = FACE_FROM_ID (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    w32_fill_rect (f, hdc, face->foreground, &r);
  else
    w32_fill_rect (f, hdc, FRAME_FOREGROUND_PIXEL (f), &r);

  release_frame_dc (f, hdc);
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
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (XFRAME (w->frame));

  if (!w->pseudo_window_p)
    {
      BLOCK_INPUT;

      if (cursor_on_p)
	display_and_set_cursor (w, 1, output_cursor.hpos,
				output_cursor.vpos,
				output_cursor.x, output_cursor.y);

      if (draw_window_fringes (w, 1))
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
  if (w32_use_visible_system_caret && w32_system_caret_hwnd)
    {
      SendMessage (w32_system_caret_hwnd, WM_EMACS_SHOW_CARET, 0, 0);
    }

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
    desired_row->redraw_fringe_bitmaps_p = 1;

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
	w32_clear_area (f, hdc, FRAME_PIXEL_WIDTH (f) - width,
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
w32_draw_fringe_bitmap (w, row, p)
     struct window *w;
     struct glyph_row *row;
     struct draw_fringe_bitmap_params *p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HDC hdc;
  struct face *face = p->face;
  int rowY;

  hdc = get_frame_dc (f);

  /* Must clip because of partially visible lines.  */
  rowY = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  if (p->y < rowY)
    {
      /* Adjust position of "bottom aligned" bitmap on partially
	 visible last row.  */
      int oldY = row->y;
      int oldVH = row->visible_height;
      row->visible_height = p->h;
      row->y -= rowY - p->y;
      w32_clip_to_row (w, row, -1, hdc);
      row->y = oldY;
      row->visible_height = oldVH;
    }
  else
    w32_clip_to_row (w, row, -1, hdc);

  if (p->bx >= 0 && !p->overlay_p)
    {
      w32_fill_area (f, hdc, face->background,
		     p->bx, p->by, p->nx, p->ny);
    }

  if (p->which && p->which < max_fringe_bmp)
    {
      HBITMAP pixmap = fringe_bmp[p->which];
      HDC compat_hdc;
      HANDLE horig_obj;

      compat_hdc = CreateCompatibleDC (hdc);

      SaveDC (hdc);

      horig_obj = SelectObject (compat_hdc, pixmap);

      /* Paint overlays transparently.  */
      if (p->overlay_p)
	{
	  HBRUSH h_brush, h_orig_brush;

	  SetTextColor (hdc, BLACK_PIX_DEFAULT (f));
	  SetBkColor (hdc, WHITE_PIX_DEFAULT (f));
	  h_brush = CreateSolidBrush (face->foreground);
	  h_orig_brush = SelectObject (hdc, h_brush);

	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  DSTINVERT);
	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  0x2E064A);
	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  DSTINVERT);

	  SelectObject (hdc, h_orig_brush);
	  DeleteObject (h_brush);
	}
      else
	{
	  SetTextColor (hdc, face->background);
	  SetBkColor (hdc, (p->cursor_p
			    ? f->output_data.w32->cursor_pixel
			    : face->foreground));

	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  SRCCOPY);
	}

      SelectObject (compat_hdc, horig_obj);
      DeleteDC (compat_hdc);
      RestoreDC (hdc, -1);
    }

  w32_set_clip_rectangle (hdc, NULL);

  release_frame_dc (f, hdc);
}

static void
w32_define_fringe_bitmap (which, bits, h, wd)
     int which;
     unsigned short *bits;
     int h, wd;
{
  if (which >= max_fringe_bmp)
    {
      int i = max_fringe_bmp;
      max_fringe_bmp = which + 20;
      fringe_bmp = (HBITMAP *) xrealloc (fringe_bmp, max_fringe_bmp * sizeof (HBITMAP));
      while (i < max_fringe_bmp)
	fringe_bmp[i++] = 0;
    }

  fringe_bmp[which] = CreateBitmap (wd, h, 1, 1, bits);
}

static void
w32_destroy_fringe_bitmap (which)
     int which;
{
  if (which >= max_fringe_bmp)
    return;

  if (fringe_bmp[which])
    DeleteObject (fringe_bmp[which]);
  fringe_bmp[which] = 0;
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
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

XCharStruct *w32_per_char_metric P_ ((XFontStruct *, wchar_t *, int));
static int w32_encode_char P_ ((int, wchar_t *, struct font_info *, int *));


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
      buf[0] = XCHAR2B_BYTE1 (char2b);
      buf[1] = XCHAR2B_BYTE2 (char2b);
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
	  if (cleartype_active)
	    {
	      /* Cleartype antialiasing causes characters to overhang
		 by a pixel on each side compared with what GetCharABCWidths
		 reports.  */
	      char_widths.abcA -= 1;
	      char_widths.abcC -= 1;
	      char_widths.abcB += 2;
	    }

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


XCharStruct *
w32_per_char_metric (font, char2b, font_type)
     XFontStruct *font;
     wchar_t *char2b;
     int /* enum w32_char_font_type */ font_type;
{
  /* The result metric information.  */
  XCharStruct *pcm;
  BOOL retval;

  xassert (font && char2b);

  /* TODO: This function is currently called through the RIF, and in
     some cases font_type is UNKNOWN_FONT. We currently allow the
     cached metrics to be used, which seems to work, but in cases
     where font_type is UNKNOWN_FONT, we probably haven't encoded
     char2b appropriately. All callers need checking to see what they
     are passing.  This is most likely to affect variable width fonts
     outside the Latin-1 range, particularly in languages like Thai
     that rely on rbearing and lbearing to provide composition. I
     don't think that is working currently anyway, but we don't seem
     to have anyone testing such languages on Windows.  */

  /* Handle the common cases quickly.  */
  if (!font->bdf && font->per_char == NULL)
    /* TODO: determine whether char2b exists in font?  */
    return &font->max_bounds;
  else if (!font->bdf && *char2b < 128)
    return &font->per_char[*char2b];

  xassert (font_type != UNKNOWN_FONT);

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

static int /* enum w32_char_font_type */
w32_encode_char (c, char2b, font_info, two_byte_p)
     int c;
     wchar_t *char2b;
     struct font_info *font_info;
     int * two_byte_p;
{
  int charset = CHAR_CHARSET (c);
  int codepage;
  int unicode_p = 0;
  int internal_two_byte_p = 0;

  XFontStruct *font = font_info->font;

  internal_two_byte_p = w32_font_is_double_byte (font);

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
	  ccl->reg[1] = XCHAR2B_BYTE2 (char2b);
	  ccl->reg[2] = -1;
	}
      else
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = XCHAR2B_BYTE1 (char2b);
	  ccl->reg[2] = XCHAR2B_BYTE2 (char2b);
	}

      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);

      /* We assume that MSBs are appropriately set/reset by CCL
	 program.  */
      if (!internal_two_byte_p)	/* 1-byte font */
	STORE_XCHAR2B (char2b, 0, ccl->reg[1]);
      else
	STORE_XCHAR2B (char2b, ccl->reg[1], ccl->reg[2]);
    }
  else if (font_info->encoding[charset])
    {
      /* Fixed encoding scheme.  See fontset.h for the meaning of the
	 encoding numbers.  */
      int enc = font_info->encoding[charset];

      if ((enc == 1 || enc == 2)
	  && CHARSET_DIMENSION (charset) == 2)
	STORE_XCHAR2B (char2b, XCHAR2B_BYTE1 (char2b) | 0x80, XCHAR2B_BYTE2 (char2b));

      if (enc == 1 || enc == 3
          || (enc == 4 && CHARSET_DIMENSION (charset) == 1))
	STORE_XCHAR2B (char2b, XCHAR2B_BYTE1 (char2b), XCHAR2B_BYTE2 (char2b) | 0x80);
      else if (enc == 4)
        {
          int sjis1, sjis2;

          ENCODE_SJIS (XCHAR2B_BYTE1 (char2b), XCHAR2B_BYTE2 (char2b),
                       sjis1, sjis2);
          STORE_XCHAR2B (char2b, sjis1, sjis2);
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
      temp[0] = XCHAR2B_BYTE1 (char2b);
      temp[1] = XCHAR2B_BYTE2 (char2b);
      temp[2] = '\0';
      if (codepage != CP_UNICODE)
        {
          if (temp[0])
            MultiByteToWideChar (codepage, 0, temp, 2, char2b, 1);
          else
            MultiByteToWideChar (codepage, 0, temp+1, 1, char2b, 1);
        }
      unicode_p = 1;
      internal_two_byte_p = 1;
    }

  if (two_byte_p)
    *two_byte_p = internal_two_byte_p;

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



/***********************************************************************
			    Glyph display
 ***********************************************************************/


/* Encapsulate the different ways of displaying text under W32.  */

static void
w32_text_out (s, x, y,chars,nchars)
     struct glyph_string * s;
     int x, y;
     wchar_t * chars;
     int nchars;
{
  int charset_dim = w32_font_is_double_byte (s->font) ? 2 : 1;
  if (s->font->bdf)
    w32_BDF_TextOut (s->font->bdf, s->hdc,
                     x, y, (char *) chars, charset_dim,
                     nchars * charset_dim, 0);
  else if (s->first_glyph->font_type == UNICODE_FONT)
    ExtTextOutW (s->hdc, x, y, 0, NULL, chars, nchars, NULL);
  else
    ExtTextOutA (s->hdc, x, y, 0, NULL, (char *) chars,
		 nchars * charset_dim, NULL);
}


static void x_set_glyph_string_clipping P_ ((struct glyph_string *));
static void x_set_glyph_string_gc P_ ((struct glyph_string *));
static void x_draw_glyph_string_background P_ ((struct glyph_string *,
						int));
static void x_draw_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_composite_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_glyph_string_box P_ ((struct glyph_string *));
static void x_draw_glyph_string  P_ ((struct glyph_string *));
static void x_set_cursor_gc P_ ((struct glyph_string *));
static void x_set_mode_line_face_gc P_ ((struct glyph_string *));
static void x_set_mouse_face_gc P_ ((struct glyph_string *));
static int w32_alloc_lighter_color (struct frame *, COLORREF *, double, int);
static void w32_setup_relief_color P_ ((struct frame *, struct relief *,
                                        double, int, COLORREF));
static void x_setup_relief_colors P_ ((struct glyph_string *));
static void x_draw_image_glyph_string P_ ((struct glyph_string *));
static void x_draw_image_relief P_ ((struct glyph_string *));
static void x_draw_image_foreground P_ ((struct glyph_string *));
static void w32_draw_image_foreground_1 P_ ((struct glyph_string *, HBITMAP));
static void x_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					   int, int, int));
static void w32_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				      int, int, int, int, int, int,
				      RECT *));
static void w32_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				 int, int, int, RECT *));

#if GLYPH_DEBUG
static void x_check_font P_ ((struct frame *, XFontStruct *));
#endif


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


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static INLINE void
x_set_glyph_string_clipping (s)
     struct glyph_string *s;
{
  RECT r;
  get_glyph_string_clip_rect (s, &r);
  w32_set_clip_rectangle (s->hdc, &r);
}


/* RIF:
   Compute left and right overhang of glyph string S.  If S is a glyph
   string for a composition, assume overhangs don't exist.  */

static void
w32_compute_glyph_string_overhangs (s)
     struct glyph_string *s;
{
  /* TODO: Windows does not appear to have a method for
     getting this info without getting the ABC widths for each
     individual character and working it out manually. */
}


static void
w32_get_glyph_overhangs (glyph, f, left, right)
     struct glyph *glyph;
     struct frame *f;
     int *left, *right;
{
  HDC hdc = get_frame_dc (f);
  /* Convert to unicode! */
  x_get_glyph_overhangs (glyph, f, left, right);
  release_frame_dc (f, hdc);
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
          char1b[i] = XCHAR2B_BYTE2 (&s->char2b[i]);

      /* Draw text with TextOut and friends. */
      w32_text_out (s, x, s->ybase - boff, s->char2b, s->nchars);

      if (s->face->overstrike)
	{
	  /* For overstriking (to simulate bold-face), draw the
	     characters again shifted to the right by one pixel.  */
	  w32_text_out (s, x + 1, s->ybase - boff, s->char2b, s->nchars);
	}
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
	{
	  w32_text_out (s, x + s->cmp->offsets[s->gidx * 2],
			s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
			s->char2b + i, 1);
	  if (s->face->overstrike)
	    w32_text_out (s, x + s->cmp->offsets[s->gidx * 2] + 1,
			  s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
			  s->char2b + i, 1);
	}
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
                      raised_p, top_p, bot_p, left_p, right_p, clip_rect)
     struct frame *f;
     int left_x, top_y, right_x, bottom_y, width;
     int top_p, bot_p, left_p, right_p, raised_p;
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
  if (top_p)
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
  if (bot_p)
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

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

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

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    w32_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
                       left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      w32_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
                            width, raised_p, 1, 1, left_p, right_p, &clip_rect);
    }
}


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (s)
     struct glyph_string *s;
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += abs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  SaveDC (s->hdc);

  if (s->img->pixmap)
    {
      HDC compat_hdc = CreateCompatibleDC (s->hdc);
      HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
      HBRUSH orig_brush = SelectObject (s->hdc, fg_brush);
      HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);
      SetBkColor (compat_hdc, RGB (255, 255, 255));
      SetTextColor (s->hdc, RGB (0, 0, 0));
      x_set_glyph_string_clipping (s);

      if (s->img->mask)
	{
	  HDC mask_dc = CreateCompatibleDC (s->hdc);
	  HGDIOBJ mask_orig_obj = SelectObject (mask_dc, s->img->mask);

	  SetTextColor (s->hdc, RGB (255, 255, 255));
	  SetBkColor (s->hdc, RGB (0, 0, 0));

	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);
	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  mask_dc, s->slice.x, s->slice.y, SRCAND);
	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);

	  SelectObject (mask_dc, mask_orig_obj);
	  DeleteDC (mask_dc);
	}
      else
	{
	  SetTextColor (s->hdc, s->gc->foreground);
	  SetBkColor (s->hdc, s->gc->background);

          BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
                  compat_hdc, s->slice.x, s->slice.y, SRCCOPY);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      w32_draw_rectangle (s->hdc, s->gc, x - r, y - r ,
				  s->slice.width + r*2 - 1,
				  s->slice.height + r*2 - 1);
	    }
	}

      w32_set_clip_rectangle (s->hdc, NULL);
      SelectObject (s->hdc, orig_brush);
      DeleteObject (fg_brush);
      SelectObject (compat_hdc, orig_obj);
      DeleteDC (compat_hdc);
    }
  else
    w32_draw_rectangle (s->hdc, s->gc, x, y,
			s->slice.width - 1, s->slice.height - 1);

  RestoreDC (s->hdc ,-1);
}


/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (s)
     struct glyph_string *s;
{
  int x0, y0, x1, y1, thick, raised_p;
  RECT r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += abs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
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
  x1 = x + s->slice.width + thick - 1;
  y1 = y + s->slice.height + thick - 1;

  x_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  w32_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p,
			s->slice.y == 0,
			s->slice.y + s->slice.height == s->img->height,
			s->slice.x == 0,
			s->slice.x + s->slice.width == s->img->width,
			&r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
w32_draw_image_foreground_1 (s, pixmap)
     struct glyph_string *s;
     HBITMAP pixmap;
{
  HDC hdc = CreateCompatibleDC (s->hdc);
  HGDIOBJ orig_hdc_obj = SelectObject (hdc, pixmap);
  int x = 0;
  int y = s->ybase - s->y - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += abs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      HDC compat_hdc = CreateCompatibleDC (hdc);
      HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
      HBRUSH orig_brush = SelectObject (hdc, fg_brush);
      HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);

      if (s->img->mask)
	{
	  HDC mask_dc = CreateCompatibleDC (hdc);
	  HGDIOBJ mask_orig_obj = SelectObject (mask_dc, s->img->mask);

	  SetTextColor (hdc, RGB (0, 0, 0));
	  SetBkColor (hdc, RGB (255, 255, 255));
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  mask_dc, s->slice.x, s->slice.y, SRCAND);
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);

	  SelectObject (mask_dc, mask_orig_obj);
	  DeleteDC (mask_dc);
	}
      else
	{
	  SetTextColor (hdc, s->gc->foreground);
	  SetBkColor (hdc, s->gc->background);

          BitBlt (hdc, x, y, s->slice.width, s->slice.height,
                  compat_hdc, s->slice.x, s->slice.y, SRCCOPY);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      w32_draw_rectangle (hdc, s->gc, x - r, y - r,
				  s->slice.width + r*2 - 1,
				  s->slice.height + r*2 - 1);
	    }
	}

      SelectObject (hdc, orig_brush);
      DeleteObject (fg_brush);
      SelectObject (compat_hdc, orig_obj);
      DeleteDC (compat_hdc);
    }
  else
    w32_draw_rectangle (hdc, s->gc, x, y,
			s->slice.width - 1, s->slice.height - 1);

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
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      x = s->x;
      if (s->first_glyph->left_box_line_p
	  && s->slice.x == 0)
	x += box_line_hwidth;

      y = s->y;
      if (s->slice.y == 0)
	y += box_line_vwidth;

#if 0 /* TODO: figure out if we need to do this on Windows.  */
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
        BitBlt (s->hdc, s->x, s->y, s->background_width, s->height,
                compat_hdc, 0, 0, SRCCOPY);

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
      int width = min (FRAME_COLUMN_WIDTH (s->f), s->background_width);

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

	  get_glyph_string_clip_rect (s, &r);
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
	     of underline.  See OUTLINETEXTMETRIC, and xterm.c.
	     Note: If you make this work, don't forget to change the
	     doc string of x-use-underline-position-properties below.  */
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
              w32_fill_area (s->f, s->hdc, s->face->overline_color, s->x,
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


/* Shift display to make room for inserted glyphs.   */

void
w32_shift_glyphs_for_insert (f, x, y, width, height, shift_by)
     struct frame *f;
     int x, y, width, height, shift_by;
{
  HDC hdc;

  hdc = get_frame_dc (f);
  BitBlt (hdc, x + shift_by, y, width, height,
          hdc, x, y, SRCCOPY);

  release_frame_dc (f, hdc);
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

    DeleteObject (dirty);
    DeleteObject (combined);
  }

  UNBLOCK_INPUT;
  DeleteObject (expect_dirty);
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/

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


/* Handle FocusIn and FocusOut state changes for FRAME.
   If FRAME has focus and there exists more than one frame, puts
   a FOCUS_IN_EVENT into *BUFP.  */

static void
x_focus_changed (type, state, dpyinfo, frame, bufp)
     int type;
     int state;
     struct w32_display_info *dpyinfo;
     struct frame *frame;
     struct input_event *bufp;
{
  if (type == WM_SETFOCUS)
    {
      if (dpyinfo->w32_focus_event_frame != frame)
        {
          x_new_focus_frame (dpyinfo, frame);
          dpyinfo->w32_focus_event_frame = frame;

          /* Don't stop displaying the initial startup message
             for a switch-frame event we don't need.  */
          if (GC_NILP (Vterminal_frame)
              && GC_CONSP (Vframe_list)
              && !GC_NILP (XCDR (Vframe_list)))
            {
              bufp->kind = FOCUS_IN_EVENT;
              XSETFRAME (bufp->frame_or_window, frame);
            }
        }

      frame->output_data.x->focus_state |= state;

      /* TODO: IME focus?  */
    }
  else if (type == WM_KILLFOCUS)
    {
      frame->output_data.x->focus_state &= ~state;

      if (dpyinfo->w32_focus_event_frame == frame)
        {
          dpyinfo->w32_focus_event_frame = 0;
          x_new_focus_frame (dpyinfo, 0);
        }

      /* TODO: IME focus?  */
    }
}


/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

static void
w32_detect_focus_change (dpyinfo, event, bufp)
     struct w32_display_info *dpyinfo;
     W32Msg *event;
     struct input_event *bufp;
{
  struct frame *frame;

  frame = x_any_window_to_frame (dpyinfo, event->msg.hwnd);
  if (! frame)
    return;

  /* On w32, this is only called from focus events, so no switch needed.  */
  x_focus_changed (event->msg.message,
		   (event->msg.message == WM_KILLFOCUS ?
		    FOCUS_IMPLICIT : FOCUS_EXPLICIT),
		   dpyinfo, frame, bufp);
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
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  if (dpyinfo->w32_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame))
	   : dpyinfo->w32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->x_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame) = Qnil;
	  dpyinfo->x_highlight_frame = dpyinfo->w32_focus_frame;
	}
    }
  else
    dpyinfo->x_highlight_frame = 0;

  if (dpyinfo->x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->x_highlight_frame)
	frame_highlight (dpyinfo->x_highlight_frame);
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

  /* Make the event type NO_EVENT; we'll change that when we decide
     otherwise.  */
  result->kind = MOUSE_CLICK_EVENT;
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
  int delta;

  result->kind = WHEEL_EVENT;
  result->code = 0;
  result->timestamp = msg->msg.time;

  /* A WHEEL_DELTA positive value indicates that the wheel was rotated
     forward, away from the user (up); a negative value indicates that
     the wheel was rotated backward, toward the user (down).  */
  delta = GET_WHEEL_DELTA_WPARAM (msg->msg.wParam);

  /* The up and down modifiers indicate if the wheel was rotated up or
     down based on WHEEL_DELTA value.  */
  result->modifiers = (msg->dwModifiers
                       | ((delta < 0 ) ? down_modifier : up_modifier));

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

  result->kind = DRAG_N_DROP_EVENT;
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
      files = Fcons (DECODE_FILE (build_string (name)), files);
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

static struct scroll_bar *x_window_to_scroll_bar ();
static void x_scroll_bar_report_motion ();
static void x_check_fullscreen P_ ((struct frame *));
static int glyph_rect P_ ((struct frame *f, int, int, RECT *));


static void
redo_mouse_highlight ()
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  LOWORD (last_mouse_motion_event.lParam),
			  HIWORD (last_mouse_motion_event.lParam));
}

void
w32_define_cursor (window, cursor)
     Window window;
     Cursor cursor;
{
  PostMessage (window, WM_EMACS_SETCURSOR, (WPARAM) cursor, 0);
}

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

  window = window_from_coordinates (f, x, y, 0, &x, &y, 0);

  if (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      struct glyph_row *r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
      struct glyph_row *end = r + w->current_matrix->nrows - 1;

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
		/* Shouldn't this be a pixel value?
		   WINDOW_LEFT_EDGE_X (w) seems to be the right value.
		   ++KFS */
		rect->left = WINDOW_LEFT_EDGE_COL (w);
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
	    /* Shouldn't this be a pixel value?
	       WINDOW_RIGHT_EDGE_X (w) seems to be the right value.
	       ++KFS */
	    rect->right = WINDOW_RIGHT_EDGE_COL (w);
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

      /* Arrange for the division in FRAME_PIXEL_X_TO_COL etc. to
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


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* Handle mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  EVENT_TYPE is either ButtionPress
   or ButtonRelase.  */

static void
w32_handle_tool_bar_click (f, button_event)
     struct frame *f;
     struct input_event *button_event;
{
  int x = XFASTINT (button_event->x);
  int y = XFASTINT (button_event->y);

  if (button_event->modifiers & down_modifier)
    handle_tool_bar_click (f, x, y, 1, 0);
  else
    handle_tool_bar_click (f, x, y, 0,
			   button_event->modifiers & ~up_modifier);
}



/***********************************************************************
			       Scroll bars
 ***********************************************************************/

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
  int window_y, window_height;

  /* Get window dimensions.  */
  window_box (w, -1, 0, &window_y, 0, &window_height);
  top  = window_y;
  width = WINDOW_CONFIG_SCROLL_BAR_COLS (w) * FRAME_COLUMN_WIDTH (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  left = WINDOW_SCROLL_BAR_AREA_X (w);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (WINDOW_CONFIG_SCROLL_BAR_WIDTH (w) > 0)
    sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);
  else
    sb_width = width;

  /* Compute the left edge of the scroll bar.  */
  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w))
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
   is set to something other than NO_EVENT, it is enqueued.

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

  emacs_event->kind = W32_SCROLL_BAR_CLICK_EVENT;
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
	emacs_event->kind = NO_EVENT;
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
w32_read_socket (sd, expected, hold_quit)
     register int sd;
     int expected;
     struct input_event *hold_quit;
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

  /* TODO: ghostscript integration. */
  while (get_next_msg (&msg, FALSE))
    {
      struct input_event inev;
      int do_help = 0;

      EVENT_INIT (inev);
      inev.kind = NO_EVENT;
      inev.arg = Qnil;

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
			     SDATA (f->name)));
		}
	      else if (f->async_visible != 1)
		{
		  /* Definitely not obscured, so mark as visible.  */
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  SET_FRAME_GARBAGED (f);
		  DebPrint (("frame %p (%s) reexposed by WM_PAINT\n", f,
			     SDATA (f->name)));

		  /* WM_PAINT serves as MapNotify as well, so report
		     visibility changes properly.  */
		  if (f->iconified)
		    {
		      inev.kind = DEICONIFY_EVENT;
		      XSETFRAME (inev.frame_or_window, f);
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
	      inev.kind = LANGUAGE_CHANGE_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.code = msg.msg.wParam;
	      inev.modifiers = msg.msg.lParam & 0xffff;
	    }
	  break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->iconified)
	    {
	      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
		{
		  clear_mouse_face (dpyinfo);
		  dpyinfo->mouse_face_hidden = 1;
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      inev.kind = NON_ASCII_KEYSTROKE_EVENT;
	      inev.code = msg.msg.wParam;
	      inev.modifiers = msg.dwModifiers;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.timestamp = msg.msg.time;
	    }
	  break;

	case WM_SYSCHAR:
	case WM_CHAR:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->iconified)
	    {
	      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
		{
		  clear_mouse_face (dpyinfo);
		  dpyinfo->mouse_face_hidden = 1;
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      inev.kind = ASCII_KEYSTROKE_EVENT;
	      inev.code = msg.msg.wParam;
	      inev.modifiers = msg.dwModifiers;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.timestamp = msg.msg.time;
	    }
	  break;

	case WM_MOUSEMOVE:
	  /* Ignore non-movement.  */
	  {
	    int x = LOWORD (msg.msg.lParam);
	    int y = HIWORD (msg.msg.lParam);
	    if (x == last_mousemove_x && y == last_mousemove_y)
	      break;
	    last_mousemove_x = x;
	    last_mousemove_y = y;
	  }

          previous_help_echo_string = help_echo_string;

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
	    {
	      /* Generate SELECT_WINDOW_EVENTs when needed.  */
	      if (mouse_autoselect_window)
		{
		  Lisp_Object window;
		  int x = LOWORD (msg.msg.lParam);
		  int y = HIWORD (msg.msg.lParam);

		  window = window_from_coordinates (f, x, y, 0, 0, 0, 0);

		  /* Window will be selected only when it is not
		     selected now and last mouse movement event was
		     not in it.  Minibuffer window will be selected
		     iff it is active.  */
		  if (WINDOWP(window)
		      && !EQ (window, last_window)
		      && !EQ (window, selected_window))
		    {
		      inev.kind = SELECT_WINDOW_EVENT;
		      inev.frame_or_window = window;
		    }

		  last_window=window;
		}
	      note_mouse_movement (f, &msg.msg);
	    }
	  else
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (dpyinfo);
            }

          /* If the contents of the global variable help_echo_string
             has changed, generate a HELP_EVENT.  */
#if 0 /* The below is an invalid comparison when USE_LISP_UNION_TYPE.
	 But it was originally changed to this to fix a bug, so I have
	 not removed it completely in case the bug is still there.  */
          if (help_echo_string != previous_help_echo_string ||
	      (!NILP (help_echo_string) && !STRINGP (help_echo_string) && f->mouse_moved))
#else /* This is what xterm.c does.  */
	    if (!NILP (help_echo_string)
		|| !NILP (previous_help_echo_string))
	    do_help = 1;
#endif
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
            int tool_bar_p = 0;
	    int button;
	    int up;

	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f)
	      {
                construct_mouse_click (&inev, &msg, f);

                /* Is this in the tool-bar?  */
                if (WINDOWP (f->tool_bar_window)
                    && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
                  {
                    Lisp_Object window;
		    int x = XFASTINT (inev.x);
		    int y = XFASTINT (inev.y);

                    window = window_from_coordinates (f, x, y, 0, 0, 0, 1);

                    if (EQ (window, f->tool_bar_window))
                      {
                        w32_handle_tool_bar_click (f, &inev);
                        tool_bar_p = 1;
                      }
                  }

                if (tool_bar_p
		    || (dpyinfo->w32_focus_frame
			&& f != dpyinfo->w32_focus_frame))
		  inev.kind = NO_EVENT;
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
	  {
	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f)
	      {

		if (!dpyinfo->w32_focus_frame
		    || f == dpyinfo->w32_focus_frame)
		  {
		    /* Emit an Emacs wheel-up/down event.  */
		    construct_mouse_wheel (&inev, &msg, f);
		  }
		/* Ignore any mouse motion that happened before this
		   event; any subsequent mouse-movement Emacs events
		   should reflect only motion after the
		   ButtonPress.	 */
		f->mouse_moved = 0;
	      }
	    last_mouse_frame = f;
	    last_tool_bar_item = -1;
	  }
	  break;

	case WM_DROPFILES:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    construct_drag_n_drop (&inev, &msg, f);
	  break;

	case WM_VSCROLL:
	  {
	    struct scroll_bar *bar =
	      x_window_to_scroll_bar ((HWND)msg.msg.lParam);

	    if (bar)
	      w32_scroll_bar_handle_click (bar, &msg, &inev);
	    break;
	  }

	case WM_WINDOWPOSCHANGED:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    {
	      if (f->want_fullscreen & FULLSCREEN_WAIT)
		f->want_fullscreen &= ~(FULLSCREEN_WAIT|FULLSCREEN_BOTH);
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
	      f->left_pos = x;
	      f->top_pos = y;
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

		  inev.kind = ICONIFY_EVENT;
		  XSETFRAME (inev.frame_or_window, f);
		  break;

		case SIZE_MAXIMIZED:
		case SIZE_RESTORED:
		  f->async_visible = 1;
		  f->async_iconified = 0;

		  /* wait_reading_process_output will notice this and update
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
                      f->left_pos = x;
                      f->top_pos = y;

		      inev.kind = DEICONIFY_EVENT;
		      XSETFRAME (inev.frame_or_window, f);
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

	      rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, height);
	      columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, width);

	      /* TODO: Clip size to the screen dimensions.  */

	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */

	      if (columns != FRAME_COLS (f)
		  || rows != FRAME_LINES (f)
		  || width != FRAME_PIXEL_WIDTH (f)
		  || height != FRAME_PIXEL_HEIGHT (f))
		{
		  change_frame_size (f, rows, columns, 0, 1, 0);
		  SET_FRAME_GARBAGED (f);
		  cancel_mouse_face (f);
		  FRAME_PIXEL_WIDTH (f) = width;
		  FRAME_PIXEL_HEIGHT (f) = height;
		  f->win_gravity = NorthWestGravity;
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
		do_help = -1;
	    }
	  break;

	case WM_SETFOCUS:
	  w32_detect_focus_change (dpyinfo, &msg, &inev);

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_KILLFOCUS:
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
		do_help = -1;
            }

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_CLOSE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      inev.kind = DELETE_WINDOW_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
	    }
	  break;

	case WM_INITMENU:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      inev.kind = MENU_BAR_ACTIVATE_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
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
	  /* Check for messages registered at runtime.  */
	  if (msg.msg.message == msh_mousewheel)
	    {
	      /* Forward MSH_MOUSEWHEEL as WM_MOUSEWHEEL.  */
	      msg.msg.message = WM_MOUSEWHEEL;
	      prepend_msg (&msg);
	    }
	  break;
	}

      if (inev.kind != NO_EVENT)
	{
	  kbd_buffer_store_event_hold (&inev, hold_quit);
	  count++;
	}

      if (do_help
	  && !(hold_quit && hold_quit->kind != NO_EVENT))
	{
	  Lisp_Object frame;

	  if (f)
	    XSETFRAME (frame, f);
	  else
	    frame = Qnil;

	  if (do_help > 0)
	    {
	      if (NILP (help_echo_string))
		{
		  help_echo_object = help_echo_window = Qnil;
		  help_echo_pos = -1;
		}

	      any_help_event_p = 1;
	      gen_help_event (help_echo_string, frame, help_echo_window,
			      help_echo_object, help_echo_pos);
	    }
	  else
	    {
	      help_echo_string = Qnil;
	      gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	    }
	  count++;
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
			       SDATA (f->name)));
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
			       SDATA (f->name)));

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

/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
w32_clip_to_row (w, row, area, hdc)
     struct window *w;
     struct glyph_row *row;
     int area;
     HDC hdc;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  RECT clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.left = window_x;
  clip_rect.top = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  clip_rect.top = max (clip_rect.top, window_y);
  clip_rect.right = clip_rect.left + window_width;
  clip_rect.bottom = clip_rect.top + row->visible_height;

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
  int h;
  struct glyph *cursor_glyph;
  HBRUSH hb = CreateSolidBrush (f->output_data.w32->cursor_pixel);

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  rect.left = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
  rect.top = get_phys_cursor_geometry (w, row, cursor_glyph, &h);
  rect.bottom = rect.top + h;
  rect.right = rect.left + w->phys_cursor_width;

  hdc = get_frame_dc (f);
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  w32_clip_to_row (w, row, TEXT_AREA, hdc);
  FrameRect (hdc, &rect, hb);
  DeleteObject (hb);
  w32_set_clip_rectangle (hdc, NULL);
  release_frame_dc (f, hdc);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (w, row, width, kind)
     struct window *w;
     struct glyph_row *row;
     int width;
     enum text_cursor_kinds kind;
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
      draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
    }
  else
    {
      COLORREF cursor_color = f->output_data.w32->cursor_pixel;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);

      /* If the glyph's background equals the color we normally draw
	 the bar cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == cursor_color)
	cursor_color = face->foreground;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

      if (width < 0)
        width = FRAME_CURSOR_WIDTH (f);
      width = min (cursor_glyph->pixel_width, width);

      w->phys_cursor_width = width;


      hdc = get_frame_dc (f);
      w32_clip_to_row (w, row, TEXT_AREA, hdc);

      if (kind == BAR_CURSOR)
	{
	  w32_fill_area (f, hdc, cursor_color, x,
			 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
			 width, row->height);
	}
      else
	{
	  w32_fill_area (f, hdc, cursor_color, x,
			 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
						  row->height - width),
			 cursor_glyph->pixel_width, width);
	}

      w32_set_clip_rectangle (hdc, NULL);
      release_frame_dc (f, hdc);
    }
}


/* RIF: Define cursor CURSOR on frame F.  */

static void
w32_define_frame_cursor (f, cursor)
     struct frame *f;
     Cursor cursor;
{
  w32_define_cursor (FRAME_W32_WINDOW (f), cursor);
}


/* RIF: Clear area on frame F.  */

static void
w32_clear_frame_area (f, x, y, width, height)
     struct frame *f;
     int x, y, width, height;
{
  HDC hdc;

  hdc = get_frame_dc (f);
  w32_clear_area (f, hdc, x, y, width, height);
  release_frame_dc (f, hdc);
}

/* RIF: Draw or clear cursor on window W.  */

static void
w32_draw_window_cursor (w, glyph_row, x, y, cursor_type, cursor_width, on_p, active_p)
     struct window *w;
     struct glyph_row *glyph_row;
     int x, y;
     int cursor_type, cursor_width;
     int on_p, active_p;
{
  if (on_p)
    {
      /* If the user wants to use the system caret, make sure our own
	 cursor remains invisible.  */
      if (w32_use_visible_system_caret)
	{
	  /* Call to erase_phys_cursor here seems to use the
	     wrong values of w->phys_cursor, as they have been
	     overwritten before this function was called. */
	  if (w->phys_cursor_type != NO_CURSOR)
	    erase_phys_cursor (w);

	  cursor_type = w->phys_cursor_type = NO_CURSOR;
	  w->phys_cursor_width = -1;
	}
      else
	{
	  w->phys_cursor_type = cursor_type;
	}

      w->phys_cursor_on_p = 1;

      /* If this is the active cursor, we need to track it with the
	 system caret, so third party software like screen magnifiers
	 and speech synthesizers can follow the cursor.  */
      if (active_p)
	{
	  struct frame *f = XFRAME (WINDOW_FRAME (w));
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

      if (glyph_row->exact_window_width_line_p
	  && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
	{
	  glyph_row->cursor_in_fringe_p = 1;
	  draw_fringe_bitmap (w, glyph_row, 0);
	  return;
	}

      switch (cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  x_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	  break;

	case HBAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	  break;

	case NO_CURSOR:
	  w->phys_cursor_width = 0;
	  break;

	default:
	  abort ();
	}
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
    hicon = LoadImage (NULL, (LPCTSTR) SDATA (icon), IMAGE_ICON, 0, 0,
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

  FRAME_COLUMN_WIDTH (f) = fontp->average_width;
  FRAME_SPACE_WIDTH (f) = fontp->space_width;
  FRAME_LINE_HEIGHT (f) = FONT_HEIGHT (FRAME_FONT (f));

  compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid-1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_W32_WINDOW (f) != 0)
    {
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
        x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
    }

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

  result = x_new_font (f, (SDATA (fontset_ascii (fontset))));

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  FRAME_FONTSET(f) = fontset;

  return build_string (fontsetname);
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
  int flags = f->size_hint_flags;

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->left_pos = (FRAME_W32_DISPLAY_INFO (f)->width
		   - FRAME_PIXEL_WIDTH (f)
		   + f->left_pos);

  if (flags & YNegative)
    f->top_pos = (FRAME_W32_DISPLAY_INFO (f)->height
		  - FRAME_PIXEL_HEIGHT (f)
		  + f->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->size_hint_flags &= ~ (XNegative | YNegative);
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
      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->left_pos;
  modified_top = f->top_pos;

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
  if (f->want_fullscreen & FULLSCREEN_BOTH)
    {
      int width, height, ign;

      x_real_positions (f, &f->left_pos, &f->top_pos);

      x_fullscreen_adjust (f, &width, &height, &ign, &ign);

      /* We do not need to move the window, it shall be taken care of
         when setting WM manager hints.  */
      if (FRAME_COLS (f) != width || FRAME_LINES (f) != height)
        {
          change_frame_size (f, height, width, 0, 1, 0);
          SET_FRAME_GARBAGED (f);
          cancel_mouse_face (f);

          /* Wait for the change of frame size to occur */
          f->want_fullscreen |= FULLSCREEN_WAIT;
        }
    }
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
  f->scroll_bar_actual_width
    = FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f);

  compute_fringe_widths (f, 0);

  pixelwidth = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, cols);
  pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

  f->win_gravity = NorthWestGravity;
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
  FRAME_PIXEL_WIDTH (f) = pixelwidth;
  FRAME_PIXEL_HEIGHT (f) = pixelheight;

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

  pix_x = FRAME_COL_TO_PIXEL_X (f, x) + FRAME_COLUMN_WIDTH (f) / 2;
  pix_y = FRAME_LINE_TO_PIXEL_Y (f, y) + FRAME_LINE_HEIGHT (f) / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > FRAME_PIXEL_WIDTH (f)) pix_x = FRAME_PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > FRAME_PIXEL_HEIGHT (f)) pix_y = FRAME_PIXEL_HEIGHT (f);

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
	x_set_offset (f, f->left_pos, f->top_pos, 0);

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
  if (FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame = 0;

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
  if (FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame = 0;

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
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;

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

  SetWindowLong (window, WND_FONTWIDTH_INDEX, FRAME_COLUMN_WIDTH (f));
  SetWindowLong (window, WND_LINEHEIGHT_INDEX, FRAME_LINE_HEIGHT (f));
  SetWindowLong (window, WND_BORDER_INDEX, FRAME_INTERNAL_BORDER_WIDTH (f));
  SetWindowLong (window, WND_SCROLLBAR_INDEX, f->scroll_bar_actual_width);

  leave_crit ();
}

/* Window manager things */
void
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
    = (char *) xmalloc (SCHARS (Vinvocation_name)
			+ SCHARS (Vsystem_name)
			+ 2);
  sprintf (dpyinfo->w32_id_name, "%s@%s",
	   SDATA (Vinvocation_name), SDATA (Vsystem_name));

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

  dpyinfo->vertical_scroll_bar_cursor = w32_load_cursor (IDC_ARROW);
  /* TODO: dpyinfo->gray */

}

/* Create an xrdb-style database of resources to supercede registry settings.
   The database is just a concatenation of C strings, finished by an additional
   \0.  The string are submitted to some basic normalization, so

     [ *]option[ *]:[ *]value...

   becomes

     option:value...

   but any whitespace following value is not removed.  */

static char *
w32_make_rdb (xrm_option)
     char *xrm_option;
{
  char *buffer = xmalloc (strlen (xrm_option) + 2);
  char *current = buffer;
  char ch;
  int in_option = 1;
  int before_value = 0;

  do {
    ch = *xrm_option++;

    if (ch == '\n')
      {
        *current++ = '\0';
        in_option = 1;
        before_value = 0;
      }
    else if (ch != ' ')
      {
        *current++ = ch;
        if (in_option && (ch == ':'))
          {
            in_option = 0;
            before_value = 1;
          }
        else if (before_value)
          {
            before_value = 0;
          }
      }
    else if (!(in_option || before_value))
      {
        *current++ = ch;
      }
  } while (ch);

  *current = '\0';

  return buffer;
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

  w32_initialize_display_info (display_name);

  dpyinfo = &one_w32_display_info;

  dpyinfo->xrdb = xrm_option ? w32_make_rdb (xrm_option) : NULL;

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
    XColor color;
    w32_defined_color (0, "white", &color, 1);
    w32_defined_color (0, "black", &color, 1);
  }

  /* Create Fringe Bitmaps and store them for later use.

     On W32, bitmaps are all unsigned short, as Windows requires
     bitmap data to be Word aligned.  For some reason they are
     horizontally reflected compared to how they appear on X, so we
     need to bitswap and convert to unsigned shorts before creating
     the bitmaps.  */
  w32_init_fringe ();

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

  w32_reset_fringes ();
}

/* Set up use of W32.  */

DWORD w32_msg_worker ();

void
x_flush (struct frame * f)
{ /* Nothing to do */ }

extern frame_parm_handler w32_frame_parm_handlers[];

static struct redisplay_interface w32_redisplay_interface =
{
  w32_frame_parm_handlers,
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  x_scroll_run,
  x_after_update_window_line,
  x_update_window_begin,
  x_update_window_end,
  x_cursor_to,
  x_flush,
  0,  /* flush_display_optional */
  x_clear_window_mouse_face,
  w32_get_glyph_overhangs,
  x_fix_overlapping_area,
  w32_draw_fringe_bitmap,
  w32_define_fringe_bitmap,
  w32_destroy_fringe_bitmap,
  w32_per_char_metric,
  w32_encode_char,
  NULL, /* w32_compute_glyph_string_overhangs */
  x_draw_glyph_string,
  w32_define_frame_cursor,
  w32_clear_frame_area,
  w32_draw_window_cursor,
  w32_draw_vertical_window_border,
  w32_shift_glyphs_for_insert
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

  /* Initialize w32_use_visible_system_caret based on whether a screen
     reader is in use.  */
  if (!SystemParametersInfo (SPI_GETSCREENREADER, 0,
			     &w32_use_visible_system_caret, 0))
    w32_use_visible_system_caret = 0;

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
    UINT smoothing_type;
    BOOL smoothing_enabled;

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

    /* Constants that are not always defined by the system headers
       since they only exist on certain versions of Windows.  */
#ifndef SPI_GETFONTSMOOTHING
#define SPI_GETFONTSMOOTHING 0x4A
#endif
#ifndef SPI_GETFONTSMOOTHINGTYPE
#define SPI_GETFONTSMOOTHINGTYPE 0x0200A
#endif
#ifndef FE_FONTSMOOTHINGCLEARTYPE
#define FE_FONTSMOOTHINGCLEARTYPE 0x2
#endif

    /* Determine if Cleartype is in use.  Used to enable a hack in
       the char metric calculations which adds extra pixels to
       compensate for the "sub-pixels" that are not counted by the
       system APIs. */
    cleartype_active =
      SystemParametersInfo (SPI_GETFONTSMOOTHING, 0, &smoothing_enabled, 0)
      && smoothing_enabled
      && SystemParametersInfo (SPI_GETFONTSMOOTHINGTYPE, 0, &smoothing_type, 0)
      && smoothing_type == FE_FONTSMOOTHINGCLEARTYPE;
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
	      &w32_num_mouse_buttons,
	      doc: /* Number of physical mouse buttons.  */);
  w32_num_mouse_buttons = 2;

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

  w32_use_visible_system_caret = 0;

  /* We don't yet support this, but defining this here avoids whining
     from cus-start.el and other places, like "M-x set-variable".  */
  DEFVAR_BOOL ("x-use-underline-position-properties",
	       &x_use_underline_position_properties,
     doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.

NOTE: Not supported on MS-Windows yet.  */);
  x_use_underline_position_properties = 0;

  DEFVAR_LISP ("x-toolkit-scroll-bars", &Vx_toolkit_scroll_bars,
	       doc: /* If not nil, Emacs uses toolkit scroll bars.  */);
  Vx_toolkit_scroll_bars = Qt;

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;
}

/* arch-tag: 5fa70624-ab86-499c-8a85-473958ee4646
   (do not change this comment) */
