/* Implementation of GUI terminal on the Mac OS.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

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
#include <stdlib.h>
#include "lisp.h"
#include "charset.h"
#include "blockinput.h"

#include "macterm.h"

#ifndef MAC_OSX
#include <alloca.h>
#endif

#ifdef MAC_OSX
#undef mktime
#undef DEBUG
#undef free
#undef malloc
#undef realloc
/* Macros max and min defined in lisp.h conflict with those in
   precompiled header Carbon.h.  */
#undef max
#undef min
#include <Carbon/Carbon.h>
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
#include <Quickdraw.h>
#include <ToolUtils.h>
#include <Sound.h>
#include <Events.h>
#include <Script.h>
#include <Resources.h>
#include <Fonts.h>
#include <TextUtils.h>
#include <LowMem.h>
#include <Controls.h>
#if defined (__MRC__) || (__MSL__ >= 0x6000)
#include <ControlDefinitions.h>
#endif
#include <Gestalt.h>

#if __profile__
#include <profiler.h>
#endif
#endif /* not MAC_OSX */

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

/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */

#define zv_width 8
#define zv_height 72
#define zv_period 3
static unsigned char zv_bits[] = {
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00};

/* An arrow like this: `<-'.  */

#define left_width 8
#define left_height 8
static unsigned char left_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};

/* Right truncation arrow bitmap `->'.  */

#define right_width 8
#define right_height 8
static unsigned char right_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};

/* Marker for continued lines.  */

#define continued_width 8
#define continued_height 8
static unsigned char continued_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};

/* Marker for continuation lines.  */

#define continuation_width 8
#define continuation_height 8
static unsigned char continuation_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};

/* Overlay arrow bitmap.  */

#if 0
/* A bomb.  */
#define ov_width 8
#define ov_height 8
static unsigned char ov_bits[] = {
   0x0c, 0x10, 0x3c, 0x7e, 0x5e, 0x5e, 0x46, 0x3c};
#else
/* A triangular arrow.  */
#define ov_width 8
#define ov_height 8
static unsigned char ov_bits[] = {
   0xc0, 0xf0, 0xf8, 0xfc, 0xfc, 0xf8, 0xf0, 0xc0};
#endif

extern Lisp_Object Qhelp_echo;


/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* If a string, XTread_socket generates an event to display that string.
   (The display is done in read_char.)  */
   
static Lisp_Object help_echo;
static Lisp_Object help_echo_window;
static Lisp_Object help_echo_object;
static int help_echo_pos;

/* Temporary variable for XTread_socket.  */

static Lisp_Object previous_help_echo;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Non-zero means autoselect window with the mouse cursor.  */

int x_autoselect_window_p;

/* Non-zero means draw block and hollow cursor as wide as the glyph
   under it.  For example, if a block cursor is over a tab, it will be
   drawn as wide as that tab on the display.  */

int x_stretch_cursor_p;

/* Non-zero means make use of UNDERLINE_POSITION font properties.  */

int x_use_underline_position_properties;

/* This is a chain of structures for all the X displays currently in
   use.  */

struct x_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME
   . FONT-LIST-CACHE), one for each element of x_display_list and in
   the same order.  NAME is the name of the frame.  FONT-LIST-CACHE
   records previous values returned by x-list-fonts.  */

Lisp_Object x_display_name_list;

/* This is display since Mac does not support multiple ones.  */
struct mac_display_info one_mac_display_info;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the XT functions.
   It is zero while not inside an update.  In that case, the XT
   functions assume that `selected_frame' is the frame to apply to.  */

extern struct frame *updating_frame;

extern int waiting_for_input;

/* This is a frame waiting to be auto-raised, within XTread_socket.  */

struct frame *pending_autoraise_frame;

/* Nominal cursor position -- where to draw output.  
   HPOS and VPOS are window relative glyph matrix coordinates.
   X and Y are window relative pixel coordinates.  */

struct cursor_pos output_cursor;

/* Non-zero means user is interacting with a toolkit scroll bar.  */

static int toolkit_scroll_bar_interaction;

/* Mouse movement.

   Formerly, we used PointerMotionHintMask (in standard_event_mask)
   so that we would have to call XQueryPointer after each MotionNotify
   event to ask for another such event.  However, this made mouse tracking
   slow, and there was a bug that made it eventually stop.

   Simply asking for MotionNotify all the time seems to work better.

   In order to avoid asking for motion events and then throwing most
   of them away or busy-polling the server for mouse positions, we ask
   the server for pointer motion hints.  This means that we get only
   one event per group of mouse movements.  "Groups" are delimited by
   other kinds of events (focus changes and button clicks, for
   example), or by XQueryPointer calls; when one of these happens, we
   get another MotionNotify event the next time the mouse moves.  This
   is at least as efficient as getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.  */

/* Where the mouse was last time we reported a mouse event.  */

FRAME_PTR last_mouse_frame;
static Rect last_mouse_glyph;
static Lisp_Object last_mouse_press_frame;

/* The scroll bar in which the last X motion event occurred.

   If the last X motion event occurred in a scroll bar, we set this so
   XTmouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last X motion event didn't occur in a scroll bar, we set
   this to Qnil, to tell XTmouse_position to return an ordinary motion
   event.  */

static Lisp_Object last_mouse_scroll_bar;

/* This is a hack.  We would really prefer that XTmouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the time-stamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */

static Time last_mouse_movement_time;

enum mouse_tracking_type {
  mouse_tracking_none,
  mouse_tracking_mouse_movement,
  mouse_tracking_scroll_bar
};

enum mouse_tracking_type mouse_tracking_in_progress = mouse_tracking_none;

struct scroll_bar *tracked_scroll_bar = NULL;

/* Incremented by XTread_socket whenever it really tries to read
   events.  */

#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

/* Used locally within XTread_socket.  */

static int x_noop_count;

/* Initial values of argv and argc.  */

extern char **initial_argv;
extern int initial_argc;

extern Lisp_Object Vcommand_line_args, Vsystem_name;

/* Tells if a window manager is present or not.  */

extern Lisp_Object Vx_no_window_manager;

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */

extern int extra_keyboard_modifiers;

static Lisp_Object Qvendor_specific_keysyms;

#if 0
extern XrmDatabase x_load_resources P_ ((Display *, char *, char *, char *));
#endif

extern Lisp_Object x_icon_type P_ ((struct frame *));


#if __MRC__
QDGlobals qd;  /* QuickDraw global information structure.  */
#endif


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

struct frame * x_window_to_frame (struct mac_display_info *, WindowPtr);
struct mac_display_info *mac_display_info_for_display (Display *);
static void x_update_window_end P_ ((struct window *, int, int));
static void frame_to_window_pixel_xy P_ ((struct window *, int *, int *));
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
static void x_handle_tool_bar_click P_ ((struct frame *, EventRecord *));
static void show_mouse_face P_ ((struct x_display_info *,
				 enum draw_glyphs_face));
static int cursor_in_mouse_face_p P_ ((struct window *));
static int clear_mouse_face P_ ((struct mac_display_info *));
static int x_io_error_quitter P_ ((Display *));
int x_catch_errors P_ ((Display *));
void x_uncatch_errors P_ ((Display *, int));
void x_lower_frame P_ ((struct frame *));
void x_scroll_bar_clear P_ ((struct frame *));
int x_had_errors_p P_ ((Display *));
void x_wm_set_size_hint P_ ((struct frame *, long, int));
void x_raise_frame P_ ((struct frame *));
void x_set_window_size P_ ((struct frame *, int, int, int));
void x_wm_set_window_state P_ ((struct frame *, int));
void x_wm_set_icon_pixmap P_ ((struct frame *, int));
void mac_initialize P_ ((void));
static void x_font_min_bounds P_ ((XFontStruct *, int *, int *));
static int x_compute_min_glyph_bounds P_ ((struct frame *));
enum text_cursor_kinds x_specified_cursor_type P_ ((Lisp_Object, int *));
static void x_draw_phys_cursor_glyph P_ ((struct window *,
					  struct glyph_row *,
					  enum draw_glyphs_face));
static void x_update_end P_ ((struct frame *));
static void XTframe_up_to_date P_ ((struct frame *));
static void XTreassert_line_highlight P_ ((int, int));
static void x_change_line_highlight P_ ((int, int, int, int));
static void XTset_terminal_modes P_ ((void));
static void XTreset_terminal_modes P_ ((void));
static void XTcursor_to P_ ((int, int, int, int));
static void x_write_glyphs P_ ((struct glyph *, int));
static void x_clear_end_of_line P_ ((int));
static void x_clear_frame P_ ((void));
static void x_clear_cursor P_ ((struct window *));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void x_new_focus_frame P_ ((struct x_display_info *, struct frame *));
static void XTframe_rehighlight P_ ((struct frame *));
static void x_frame_rehighlight P_ ((struct x_display_info *));
static void x_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void x_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int));
static int x_intersect_rectangles P_ ((Rect *, Rect *, Rect *));
static void expose_frame P_ ((struct frame *, int, int, int, int));
static int expose_window_tree P_ ((struct window *, Rect *));
static int expose_window P_ ((struct window *, Rect *));
static void expose_area P_ ((struct window *, struct glyph_row *,
			     Rect *, enum glyph_row_area));
static int expose_line P_ ((struct window *, struct glyph_row *,
			    Rect *));
void x_display_cursor (struct window *, int, int, int, int, int);
void x_update_cursor P_ ((struct frame *, int));
static void x_update_cursor_in_window_tree P_ ((struct window *, int));
static void x_update_window_cursor P_ ((struct window *, int));
static void x_erase_phys_cursor P_ ((struct window *));
void x_display_and_set_cursor P_ ((struct window *, int, int, int, int, int));
static void x_draw_fringe_bitmap P_ ((struct window *, struct glyph_row *, 
				      enum fringe_bitmap_type, int left_p));
static void x_clip_to_row P_ ((struct window *, struct glyph_row *,
			       GC, int));
static int x_phys_cursor_in_rect_p P_ ((struct window *, Rect *));
static void x_draw_row_fringe_bitmaps P_ ((struct window *, struct glyph_row *));
static void notice_overwritten_cursor P_ ((struct window *,
					   enum glyph_row_area,
					   int, int, int, int));
static void x_flush P_ ((struct frame *f));
static void x_update_begin P_ ((struct frame *));
static void x_update_window_begin P_ ((struct window *));
static void x_draw_vertical_border P_ ((struct window *));
static void x_after_update_window_line P_ ((struct glyph_row *));
static INLINE void take_vertical_position_into_account P_ ((struct it *));
static void x_produce_stretch_glyph P_ ((struct it *));

static void activate_scroll_bars (FRAME_PTR);
static void deactivate_scroll_bars (FRAME_PTR);

extern int image_ascent (struct image *, struct face *);
void x_set_offset (struct frame *, int, int, int);
int x_bitmap_icon (struct frame *, Lisp_Object);
void x_make_frame_visible (struct frame *);

extern void window_scroll (Lisp_Object, int, int, int);

/* Defined in macmenu.h.  */
extern void menubar_selection_callback (FRAME_PTR, int);
extern void set_frame_menubar (FRAME_PTR, int, int);

/* X display function emulation */

/* Structure borrowed from Xlib.h to represent two-byte characters in
   dumpglyphs.  */

typedef struct {
  unsigned char byte1;
  unsigned char byte2;
} XChar2b;

static void
XFreePixmap (display, pixmap)
     Display *display;
     Pixmap pixmap;
{
  PixMap *p = (PixMap *) pixmap;
  
  xfree (p->baseAddr);
  xfree (p);
}


/* Set foreground color for subsequent QuickDraw commands.  Assume
   graphic port has already been set.  */

static void
mac_set_forecolor (unsigned long color)
{
  RGBColor fg_color;
						
  fg_color.red = RED_FROM_ULONG (color) * 256;
  fg_color.green = GREEN_FROM_ULONG (color) * 256;
  fg_color.blue = BLUE_FROM_ULONG (color) * 256;
			
  RGBForeColor (&fg_color);  
}


/* Set background color for subsequent QuickDraw commands.  Assume
   graphic port has already been set.  */

static void
mac_set_backcolor (unsigned long color)
{
  RGBColor bg_color;
						
  bg_color.red = RED_FROM_ULONG (color) * 256;
  bg_color.green = GREEN_FROM_ULONG (color) * 256;
  bg_color.blue = BLUE_FROM_ULONG (color) * 256;
			
  RGBBackColor (&bg_color);  
}

/* Set foreground and background color for subsequent QuickDraw
   commands.  Assume that the graphic port has already been set.  */

static void
mac_set_colors (GC gc)
{
  mac_set_forecolor (gc->foreground);
  mac_set_backcolor (gc->background);
}

/* Mac version of XDrawLine.  */

static void
XDrawLine (display, w, gc, x1, y1, x2, y2)
     Display *display;
     WindowPtr w;
     GC gc;
     int x1, y1, x2, y2;
{
#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);

  MoveTo (x1, y1);
  LineTo (x2, y2);
}

/* Mac version of XClearArea.  */

void
XClearArea (display, w, x, y, width, height, exposures)
     Display *display;
     WindowPtr w;
     int x, y;
     unsigned int width, height;
     int exposures;
{
  struct mac_output *mwp = (mac_output *) GetWRefCon (w);
  Rect r;
  XGCValues xgc;

  xgc.foreground = mwp->x_compatible.foreground_pixel;
  xgc.background = mwp->x_compatible.background_pixel;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (&xgc);
  SetRect (&r, x, y, x + width, y + height);

  EraseRect (&r);
}

/* Mac version of XClearWindow.  */

static void
XClearWindow (display, w)
     Display *display;
     WindowPtr w;
{
  struct mac_output *mwp = (mac_output *) GetWRefCon (w);
  XGCValues xgc;

  xgc.foreground = mwp->x_compatible.foreground_pixel;
  xgc.background = mwp->x_compatible.background_pixel;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (&xgc);

#if TARGET_API_MAC_CARBON
  {
    Rect r;
    
    GetWindowPortBounds (w, &r);
    EraseRect (&r);
  }
#else /* not TARGET_API_MAC_CARBON */
  EraseRect (&(w->portRect));
#endif /* not TARGET_API_MAC_CARBON */
}


/* Mac replacement for XCopyArea.  */

static void
mac_draw_bitmap (display, w, gc, x, y, bitmap)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     BitMap *bitmap;
{
  Rect r;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);
  SetRect (&r, x, y, x + bitmap->bounds.right, y + bitmap->bounds.bottom);

#if TARGET_API_MAC_CARBON
  {
    PixMapHandle pmh;

    LockPortBits (GetWindowPort (w));
    pmh = GetPortPixMap (GetWindowPort (w));
    CopyBits (bitmap, (BitMap *) *pmh, &(bitmap->bounds), &r, srcCopy, 0);
    UnlockPortBits (GetWindowPort (w));
  }
#else /* not TARGET_API_MAC_CARBON */
  CopyBits (bitmap, &(w->portBits), &(bitmap->bounds), &r, srcCopy, 0);	
#endif /* not TARGET_API_MAC_CARBON */
}


/* Mac replacement for XSetClipRectangles.  */

static void
mac_set_clip_rectangle (display, w, r)
     Display *display;
     WindowPtr w;
     Rect *r;
{
#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  ClipRect (r);
}


/* Mac replacement for XSetClipMask.  */

static void
mac_reset_clipping (display, w)
     Display *display;
     WindowPtr w;
{
  Rect r;
  
#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  SetRect (&r, -32767, -32767, 32767, 32767);
  ClipRect (&r);
}


/* Mac replacement for XCreateBitmapFromBitmapData.  */

static void
mac_create_bitmap_from_bitmap_data (bitmap, bits, w, h)
     BitMap *bitmap;
     char *bits;
     int w, h;
{
  int bytes_per_row, i, j;

  bitmap->rowBytes = (w + 15) / 16 * 2;  /* must be on word boundary */
  bitmap->baseAddr = xmalloc (bitmap->rowBytes * h);
  if (!bitmap->baseAddr)
    abort ();

  bzero (bitmap->baseAddr, bitmap->rowBytes * h);
  for (i = 0; i < h; i++)
    for (j = 0; j < w; j++)
      if (BitTst (bits, i * w + j))
        BitSet (bitmap->baseAddr, i * bitmap->rowBytes * 8 + j);

  SetRect (&(bitmap->bounds), 0, 0, w, h);
}


static void
mac_free_bitmap (bitmap)
     BitMap *bitmap;
{
  xfree (bitmap->baseAddr);
}

/* Mac replacement for XFillRectangle.  */

static void
XFillRectangle (display, w, gc, x, y, width, height)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     unsigned int width, height;
{
  Rect r;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);
  SetRect (&r, x, y, x + width, y + height);

  PaintRect (&r); /* using foreground color of gc */
}


/* Mac replacement for XDrawRectangle: dest is a window.  */

static void
mac_draw_rectangle (display, w, gc, x, y, width, height)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     unsigned int width, height;
{
  Rect r;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);
  SetRect (&r, x, y, x + width + 1, y + height + 1);

  FrameRect (&r); /* using foreground color of gc */
}


/* Mac replacement for XDrawRectangle: dest is a Pixmap.  */

static void
mac_draw_rectangle_to_pixmap (display, p, gc, x, y, width, height)
     Display *display;
     Pixmap p;
     GC gc;
     int x, y;
     unsigned int width, height;
{
#if 0 /* MAC_TODO: draw a rectangle in a PixMap */
  Rect r;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);
  SetRect (&r, x, y, x + width, y + height);

  FrameRect (&r); /* using foreground color of gc */
#endif /* 0 */
}


static void
mac_draw_string_common (display, w, gc, x, y, buf, nchars, mode,
			bytes_per_char)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     char *buf;
     int nchars, mode, bytes_per_char;
{
#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  mac_set_colors (gc);

  TextFont (gc->font->mac_fontnum);
  TextSize (gc->font->mac_fontsize);
  TextFace (gc->font->mac_fontface);
  TextMode (mode);

  MoveTo (x, y);
  DrawText (buf, 0, nchars * bytes_per_char);
}


/* Mac replacement for XDrawString.  */

static void
XDrawString (display, w, gc, x, y, buf, nchars)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     char *buf;
     int nchars;
{
  mac_draw_string_common (display, w, gc, x, y, buf, nchars, srcOr, 1);
}


/* Mac replacement for XDrawString16. */

static void
XDrawString16 (display, w, gc, x, y, buf, nchars)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     XChar2b *buf;
     int nchars;
{
  mac_draw_string_common (display, w, gc, x, y, (char *) buf, nchars, srcOr,
			  2);
}


/* Mac replacement for XDrawImageString.  */

static void
XDrawImageString (display, w, gc, x, y, buf, nchars)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     char *buf;
     int nchars;
{
  mac_draw_string_common (display, w, gc, x, y, buf, nchars, srcCopy, 1);
}


/* Mac replacement for XDrawString16.  */

static void
XDrawImageString16 (display, w, gc, x, y, buf, nchars)
     Display *display;
     WindowPtr w;
     GC gc;
     int x, y;
     XChar2b *buf;
     int nchars;
{
  mac_draw_string_common (display, w, gc, x, y, (char *) buf, nchars, srcCopy,
			  2);
}


/* Mac replacement for XCopyArea: dest must be window.  */

static void
mac_copy_area (display, src, dest, gc, src_x, src_y, width, height, dest_x,
	       dest_y)
     Display *display;
     Pixmap src;
     WindowPtr dest;
     GC gc;
     int src_x, src_y;
     unsigned int width, height;
     int dest_x, dest_y;
{
  Rect src_r, dest_r;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (dest));
#else
  SetPort (dest);
#endif

  mac_set_colors (gc);

  SetRect (&src_r, src_x, src_y, src_x + width, src_y + height);
  SetRect (&dest_r, dest_x, dest_y, dest_x + width, dest_y + height);

#if TARGET_API_MAC_CARBON
  {
    PixMapHandle pmh;

    LockPortBits (GetWindowPort (dest));
    pmh = GetPortPixMap (GetWindowPort (dest));
    CopyBits ((BitMap *) &src, (BitMap *) *pmh, &src_r, &dest_r, srcCopy, 0);
    UnlockPortBits (GetWindowPort (dest));
  }
#else /* not TARGET_API_MAC_CARBON */
  CopyBits ((BitMap *) &src, &(dest->portBits), &src_r, &dest_r, srcCopy, 0);
#endif /* not TARGET_API_MAC_CARBON */
}


#if 0
/* Convert a pair of local coordinates to global (screen) coordinates.
   Assume graphic port has been properly set.  */
static void
local_to_global_coord (short *h, short *v)
{
  Point p;
  
  p.h = *h;
  p.v = *v;
  
  LocalToGlobal (&p);
  
  *h = p.h;
  *v = p.v;
}
#endif

/* Mac replacement for XCopyArea: used only for scrolling.  */

static void
mac_scroll_area (display, w, gc, src_x, src_y, width, height, dest_x, dest_y)
     Display *display;
     WindowPtr w;
     GC gc;
     int src_x, src_y;
     unsigned int width, height;
     int dest_x, dest_y;
{
#if TARGET_API_MAC_CARBON
  Rect gw_r, src_r, dest_r;
  PixMapHandle pmh;

  SetRect (&src_r, src_x, src_y, src_x + width, src_y + height);
  SetRect (&dest_r, dest_x, dest_y, dest_x + width, dest_y + height);

  SetPort (GetWindowPort (w));
  mac_set_colors (gc);

  LockPortBits (GetWindowPort (w));
  pmh = GetPortPixMap (GetWindowPort (w));
  CopyBits ((BitMap *) *pmh, (BitMap *) *pmh, &src_r, &dest_r, srcCopy, 0);
  UnlockPortBits (GetWindowPort (w));
#else /* not TARGET_API_MAC_CARBON */
  Rect src_r, dest_r;

  SetPort (w);
#if 0
  mac_set_colors (gc);
#endif

  SetRect (&src_r, src_x, src_y, src_x + width, src_y + height);
  SetRect (&dest_r, dest_x, dest_y, dest_x + width, dest_y + height);

#if 0
  /* Need to use global coordinates and screenBits since src and dest
     areas overlap in general.  */
  local_to_global_coord (&src_r.left, &src_r.top);
  local_to_global_coord (&src_r.right, &src_r.bottom);
  local_to_global_coord (&dest_r.left, &dest_r.top);
  local_to_global_coord (&dest_r.right, &dest_r.bottom);

  CopyBits (&qd.screenBits, &qd.screenBits, &src_r, &dest_r, srcCopy, 0);
#else
  /* In Color QuickDraw, set ForeColor and BackColor as follows to avoid
     color mapping in CopyBits.  Otherwise, it will be slow.  */
  ForeColor (blackColor);
  BackColor (whiteColor);
  CopyBits (&(w->portBits), &(w->portBits), &src_r, &dest_r, srcCopy, 0);
  
  mac_set_colors (gc);
#endif
#endif /* not TARGET_API_MAC_CARBON */
}


/* Mac replacement for XCopyArea: dest must be Pixmap.  */

static void
mac_copy_area_to_pixmap (display, src, dest, gc, src_x, src_y, width, height, 
                     dest_x, dest_y)
     Display *display;
     Pixmap src;
     Pixmap dest;
     GC gc;
     int src_x, src_y;
     unsigned int width, height;
     int dest_x, dest_y;
{
  Rect src_r, dest_r;
  int src_right = ((PixMap *) src)->bounds.right;
  int src_bottom = ((PixMap *) src)->bounds.bottom;
  int w = src_right - src_x;
  int h = src_bottom - src_y;

  mac_set_colors (gc);
    
  SetRect (&src_r, src_x, src_y, src_right, src_bottom);
  SetRect (&dest_r, dest_x, dest_y, dest_x + w, dest_y + h);

  CopyBits ((BitMap *) &src, (BitMap *) &dest, &src_r, &dest_r, srcCopy, 0);
}


/* Mac replacement for XChangeGC.  */

static void
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


/* Mac replacement for XCreateGC.  */

XGCValues *
XCreateGC (void * ignore, Window window, unsigned long mask,
                      XGCValues *xgcv)
{
  XGCValues *gc = (XGCValues *) xmalloc (sizeof (XGCValues));
  bzero (gc, sizeof (XGCValues));

  XChangeGC (ignore, gc, mask, xgcv);

  return gc;
}


/* Used in xfaces.c.  */

void
XFreeGC (display, gc)
     Display *display;
     GC gc;
{
  xfree (gc);
}


/* Mac replacement for XGetGCValues.  */

static void
XGetGCValues (void* ignore, XGCValues *gc,
                   unsigned long mask, XGCValues *xgcv)
{
  XChangeGC (ignore, xgcv, mask, gc);
}


/* Mac replacement for XSetForeground.  */

static void
XSetForeground (display, gc, color)
     Display *display;
     GC gc;
     unsigned long color;
{
  gc->foreground = color;
}


/* Mac replacement for XSetFont.  */

static void
XSetFont (display, gc, font)
     Display *display;
     GC gc;
     XFontStruct *font;
{
  gc->font = font;
}


static void
XTextExtents16 (XFontStruct *font, XChar2b *text, int nchars,
                     int *direction,int *font_ascent,
                     int *font_descent, XCharStruct *cs)
{
  /* MAC_TODO: Use GetTextMetrics to do this and inline it below. */
}


/* x_sync is a no-op on Mac.  */
void
x_sync (f)
     void *f;
{
}


/* Remove calls to XFlush by defining XFlush to an empty replacement.
   Calls to XFlush should be unnecessary because the X output buffer
   is flushed automatically as needed by calls to XPending,
   XNextEvent, or XWindowEvent according to the XFlush man page.
   XTread_socket calls XPending.  Removing XFlush improves
   performance.  */

#if TARGET_API_MAC_CARBON
#define XFlush(DISPLAY) QDFlushPortBuffer (GetQDGlobalsThePort (), NULL)
#else
#define XFlush(DISPLAY)	(void) 0 
#endif

/* Flush display of frame F, or of all frames if F is null.  */

void
x_flush (f)
     struct frame *f;
{
#if TARGET_API_MAC_CARBON
  BLOCK_INPUT;
  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
	x_flush (XFRAME (frame));
    }
  else if (FRAME_X_P (f))
    XFlush (FRAME_MAC_DISPLAY (f));
  UNBLOCK_INPUT;
#endif /* TARGET_API_MAC_CARBON */
}



/* Return the struct mac_display_info corresponding to DPY.  There's
   only one.  */

struct mac_display_info *
mac_display_info_for_display (dpy)
     Display *dpy;
{
  return &one_mac_display_info;
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
  /* Nothing to do.  */
}


/* Start update of window W.  Set the global variable updated_window
   to the window being updated and set output_cursor to the cursor
   position of W.  */

static void
x_update_window_begin (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct mac_display_info *display_info = FRAME_MAC_DISPLAY_INFO (f);
  
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
      int x0, x1, y0, y1;

      window_box_edges (w, -1, &x0, &y0, &x1, &y1);
      x1 += FRAME_X_RIGHT_FRINGE_WIDTH (f);
      y1 -= 1;
      
      XDrawLine (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), 
		 f->output_data.mac->normal_gc, x1, y0, x1, y1);
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
  struct mac_display_info *dpyinfo
    = FRAME_MAC_DISPLAY_INFO (XFRAME (w->frame));

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

#if 0
  /* Unhide the caret.  This won't actually show the cursor, unless it
     was visible before the corresponding call to HideCaret in
     x_update_window_begin.  */
  if (w32_use_visible_system_caret)
    SendMessage (w32_system_caret_hwnd, WM_EMACS_SHOW_CARET, 0, 0);
#endif

  updated_window = NULL;
}


/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
x_update_end (f)
     struct frame *f;
{
  /* Reset the background color of Mac OS Window to that of the frame after
     update so that it is used by Mac Toolbox to clear the update region before
     an update event is generated.  */
#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (FRAME_MAC_WINDOW (f)));
#else
  SetPort (FRAME_MAC_WINDOW (f));
#endif

  mac_set_backcolor (FRAME_BACKGROUND_PIXEL (f));
  
  /* Mouse highlight may be displayed again.  */
  FRAME_MAC_DISPLAY_INFO (f)->mouse_face_defer = 0;

  BLOCK_INPUT;
  XFlush (FRAME_MAC_DISPLAY (f));
  UNBLOCK_INPUT;
}


/* This function is called from various places in xdisp.c whenever a
   complete update has been performed.  The global variable
   updated_window is not available here.  */

static void
XTframe_up_to_date (f)
     struct frame *f;
{
  if (FRAME_X_P (f))
    {
      struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

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

      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		  0, y, width, height, 0);
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		  f->output_data.mac->pixel_width - width, y,
		  width, height, 0);
      
      UNBLOCK_INPUT;
    }
}


/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.  */

static void
x_draw_fringe_bitmap (w, row, which, left_p)
     struct window *w;
     struct glyph_row *row;
     enum fringe_bitmap_type which;
     int left_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Display *display = FRAME_MAC_DISPLAY (f);
  WindowPtr window = FRAME_MAC_WINDOW (f);
  int x, y, wd, h, dy;
  int b1, b2;
  unsigned char *bits;
  BitMap bitmap;
  XGCValues gcv;
  GC gc = f->output_data.mac->normal_gc;
  struct face *face;

  /* Must clip because of partially visible lines.  */
  x_clip_to_row (w, row, gc, 1);

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
      bits = left_bits;
      break;
      
    case OVERLAY_ARROW_BITMAP:
      wd = ov_width;
      h = ov_height;
      bits = ov_bits;
      break;
      
    case RIGHT_TRUNCATION_BITMAP:
      wd = right_width;
      h = right_height;
      bits = right_bits;
      break;

    case CONTINUED_LINE_BITMAP:
      wd = continued_width;
      h = continued_height;
      bits = continued_bits;
      break;
      
    case CONTINUATION_LINE_BITMAP:
      wd = continuation_width;
      h = continuation_height;
      bits = continuation_bits;
      break;

    case ZV_LINE_BITMAP:
      wd = zv_width;
      h = zv_height - (y % zv_period);
      bits = zv_bits + (y % zv_period);
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
      XGCValues gcv;
      gcv.foreground = face->background;

#if 0  /* MAC_TODO: stipple */
      /* In case the same realized face is used for fringes and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      if (face->stipple)
	XSetFillStyle (FRAME_X_DISPLAY (f), face->gc, FillOpaqueStippled);
      else
	XSetForeground (FRAME_X_DISPLAY (f), face->gc, face->background);
#endif

      XFillRectangle (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		      &gcv,
		      b1,
		      WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
						       row->y)),
		      b2,
		      row->visible_height);

#if 0  /* MAC_TODO: stipple */
      if (!face->stipple)
	XSetForeground (FRAME_X_DISPLAY (f), face->gc, face->foreground);
#endif
    }

  if (which == NO_FRINGE_BITMAP)
    {
      mac_reset_clipping (display, window);
      return;
    }

  mac_create_bitmap_from_bitmap_data (&bitmap, bits, wd, h);
  gcv.foreground = face->foreground;
  gcv.background = face->background;

  mac_draw_bitmap (display, window, &gcv, x, y + dy, &bitmap);

  mac_free_bitmap (&bitmap);
  mac_reset_clipping (display, window);
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

  xassert (interrupt_input_blocked);

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

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

      x_draw_fringe_bitmap (w, row, bitmap, 1);
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

      x_draw_fringe_bitmap (w, row, bitmap, 0);
    }
}


/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
XTset_terminal_modes ()
{
}

/* This is called when exiting or suspending Emacs.  Exiting will make
   the windows go away, and suspending requires no action.  */

static void
XTreset_terminal_modes ()
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
XTcursor_to (vpos, hpos, y, x)
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
      XFlush (FRAME_X_DISPLAY (SELECTED_FRAME ()));
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static struct face *x_get_glyph_face_and_encoding P_ ((struct frame *,
						       struct glyph *,
						       XChar2b *,
						       int *));
static struct face *x_get_char_face_and_encoding P_ ((struct frame *, int,
						      int, XChar2b *, int));
static XCharStruct *x_per_char_metric P_ ((XFontStruct *, XChar2b *));
static void x_encode_char P_ ((int, XChar2b *, struct font_info *));
static void x_append_glyph P_ ((struct it *));
static void x_append_composite_glyph P_ ((struct it *));
static void x_append_stretch_glyph P_ ((struct it *it, Lisp_Object,
					int, int, double));
static void x_produce_glyphs P_ ((struct it *));
static void x_produce_image_glyph P_ ((struct it *it));


/* Return a pointer to per-char metric information in FONT of a
   character pointed by B which is a pointer to an XChar2b.  */

#define PER_CHAR_METRIC(font, b)					   \
  ((font)->per_char							   \
   ? ((font)->per_char + (b)->byte2 - (font)->min_char_or_byte2		   \
      + (((font)->min_byte1 || (font)->max_byte1)			   \
	 ? (((b)->byte1 - (font)->min_byte1)				   \
	    * ((font)->max_char_or_byte2 - (font)->min_char_or_byte2 + 1)) \
	 : 0))								   \
   : &((font)->max_bounds))


/* Get metrics of character CHAR2B in FONT.  Value is null if CHAR2B
   is not contained in the font.  */

static INLINE XCharStruct *
x_per_char_metric (font, char2b)
     XFontStruct *font;
     XChar2b *char2b;
{
  /* The result metric information.  */
  XCharStruct *pcm = NULL;

  xassert (font && char2b);

  if (font->per_char != NULL)
    {
      if (font->min_byte1 == 0 && font->max_byte1 == 0)
	{
	  /* min_char_or_byte2 specifies the linear character index
	     corresponding to the first element of the per_char array,
	     max_char_or_byte2 is the index of the last character.  A
	     character with non-zero CHAR2B->byte1 is not in the font.
	     A character with byte2 less than min_char_or_byte2 or
	     greater max_char_or_byte2 is not in the font.  */
	  if (char2b->byte1 == 0
	      && char2b->byte2 >= font->min_char_or_byte2
	      && char2b->byte2 <= font->max_char_or_byte2)
	    pcm = font->per_char + char2b->byte2 - font->min_char_or_byte2;
	}
      else
	{
	  /* If either min_byte1 or max_byte1 are nonzero, both
	     min_char_or_byte2 and max_char_or_byte2 are less than
	     256, and the 2-byte character index values corresponding
	     to the per_char array element N (counting from 0) are:

	     byte1 = N/D + min_byte1
	     byte2 = N\D + min_char_or_byte2

	     where:

	     D = max_char_or_byte2 - min_char_or_byte2 + 1
	     / = integer division
	     \ = integer modulus  */
	  if (char2b->byte1 >= font->min_byte1
	      && char2b->byte1 <= font->max_byte1
	      && char2b->byte2 >= font->min_char_or_byte2
	      && char2b->byte2 <= font->max_char_or_byte2)
	    {
	      pcm = (font->per_char
		     + ((font->max_char_or_byte2 - font->min_char_or_byte2 + 1)
			* (char2b->byte1 - font->min_byte1))
		     + (char2b->byte2 - font->min_char_or_byte2));
	    }
	}
    }
  else
    {
      /* If the per_char pointer is null, all glyphs between the first
	 and last character indexes inclusive have the same
	 information, as given by both min_bounds and max_bounds.  */
      if (char2b->byte2 >= font->min_char_or_byte2
	  && char2b->byte2 <= font->max_char_or_byte2)
	pcm = &font->max_bounds;
    }

  return ((pcm == NULL
	   || (pcm->width == 0 && (pcm->rbearing - pcm->lbearing) == 0))
	  ? NULL : pcm);
}


/* Encode CHAR2B using encoding information from FONT_INFO.  CHAR2B is
   the two-byte form of C.  Encoding is returned in *CHAR2B.  */

static INLINE void
x_encode_char (c, char2b, font_info)
     int c;
     XChar2b *char2b;
     struct font_info *font_info;
{
  int charset = CHAR_CHARSET (c);
  XFontStruct *font = font_info->font;

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
	  ccl->reg[1] = char2b->byte2;
	}
      else
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = char2b->byte1;
	  ccl->reg[2] = char2b->byte2;
	}
      
      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
      
      /* We assume that MSBs are appropriately set/reset by CCL
	 program.  */
      if (font->max_byte1 == 0)	/* 1-byte font */
	char2b->byte1 = 0, char2b->byte2 = ccl->reg[1];
      else
	char2b->byte1 = ccl->reg[1], char2b->byte2 = ccl->reg[2];
    }
  else if (font_info->encoding[charset])
    {
      /* Fixed encoding scheme.  See fontset.h for the meaning of the
	 encoding numbers.  */
      int enc = font_info->encoding[charset];
      
      if ((enc == 1 || enc == 2)
	  && CHARSET_DIMENSION (charset) == 2)
	char2b->byte1 |= 0x80;
      
      if (enc == 1 || enc == 3)
	char2b->byte2 |= 0x80;

      if (enc == 4)
        {
          int sjis1, sjis2;

          ENCODE_SJIS (char2b->byte1, char2b->byte2, sjis1, sjis2);
          char2b->byte1 = sjis1;
          char2b->byte2 = sjis2;
        }
    }
}


/* Get face and two-byte form of character C in face FACE_ID on frame
   F.  The encoding of C is returned in *CHAR2B.  MULTIBYTE_P non-zero
   means we want to display multibyte text.  Value is a pointer to a
   realized face that is ready for display.  */

static INLINE struct face *
x_get_char_face_and_encoding (f, c, face_id, char2b, multibyte_p)
     struct frame *f;
     int c, face_id;
     XChar2b *char2b;
     int multibyte_p;
{
  struct face *face = FACE_FROM_ID (f, face_id);

  if (!multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      char2b->byte1 = 0;
      char2b->byte2 = c;
      face_id = FACE_FOR_CHAR (f, face, c);
      face = FACE_FROM_ID (f, face_id);
    }
  else if (c < 128 && face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      char2b->byte1 = 0;
      char2b->byte2 = c;
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (c, charset, c1, c2);
      if (c2 > 0)
	char2b->byte1 = c1, char2b->byte2 = c2;
      else
	char2b->byte1 = 0, char2b->byte2 = c1;

      /* Maybe encode the character in *CHAR2B.  */
      if (face->font != NULL)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	    x_encode_char (c, char2b, font_info);
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
     XChar2b *char2b;
     int *two_byte_p;
{
  struct face *face;

  xassert (glyph->type == CHAR_GLYPH);
  face = FACE_FROM_ID (f, glyph->face_id);

  if (two_byte_p)
    *two_byte_p = 0;

  if (!glyph->multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      char2b->byte1 = 0;
      char2b->byte2 = glyph->u.ch;
    }
  else if (glyph->u.ch < 128
	   && glyph->face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      char2b->byte1 = 0;
      char2b->byte2 = glyph->u.ch;
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (glyph->u.ch, charset, c1, c2);
      if (c2 > 0)
	char2b->byte1 = c1, char2b->byte2 = c2;
      else
	char2b->byte1 = 0, char2b->byte2 = c1;

      /* Maybe encode the character in *CHAR2B.  */
      if (charset != CHARSET_ASCII)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	    {
	      x_encode_char (glyph->u.ch, char2b, font_info);
	      if (two_byte_p)
		*two_byte_p
		  = ((XFontStruct *) (font_info->font))->max_byte1 > 0;
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
      XChar2b char2b;
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
	  boff = it->f->output_data.mac->baseline_offset;
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

          pcm = x_per_char_metric (font, &char2b);
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
	  /* A multi-byte character.  Assume that the display width of the
	     character is the width of the character multiplied by the
	     width of the font.  */

	  /* If we found a font, this font should give us the right
	     metrics.  If we didn't find a font, use the frame's
	     default font and calculate the width of the character
	     from the charset width; this is what old redisplay code
	     did.  */
	  pcm = x_per_char_metric (font, &char2b);
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
      XChar2b char2b;
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
	  boff = it->f->output_data.mac->baseline_offset;
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

	  cmp->font = (void *) font;

	  /* Initialize the bounding box.  */
	  pcm = x_per_char_metric (font, &char2b);
	  if (pcm)
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
		  boff = it->f->output_data.mac->baseline_offset;
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

	      pcm = x_per_char_metric (font, &char2b);
	      if (pcm)
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
  Display *display;
  Window window;

  /* The glyph row for which this string was built.  It determines the
     y-origin and height of the string.  */
  struct glyph_row *row;

  /* The area within row.  */
  enum glyph_row_area area;

  /* Characters to be drawn, and number of characters.  */
  XChar2b *char2b;
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
  GC gc;

  /* A pointer to the first glyph in the string.  This glyph
     corresponds to char2b[0].  Needed to draw rectangles if
     font_not_found_p is 1.  */
  struct glyph *first_glyph;

  /* Image, if any.  */
  struct image *img;

  struct glyph_string *next, *prev;
};


#if 0

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
static void x_init_glyph_string P_ ((struct glyph_string *,
					XChar2b *, struct window *,
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
static void x_get_glyph_overhangs P_ ((struct glyph *, struct frame *,
				       int *, int *));
static void x_compute_overhangs_and_x P_ ((struct glyph_string *, int, int));
/*static int x_alloc_lighter_color P_ ((struct frame *, Display *, Colormap,
  unsigned long *, double, int));*/
static void x_setup_relief_color P_ ((struct frame *, struct relief *,
				      double, int, unsigned long));
static void x_setup_relief_colors P_ ((struct glyph_string *));
static void x_draw_image_glyph_string P_ ((struct glyph_string *));
static void x_draw_image_relief P_ ((struct glyph_string *));
static void x_draw_image_foreground P_ ((struct glyph_string *));
static void x_draw_image_foreground_1 P_ ((struct glyph_string *, Pixmap));
static void x_fill_image_glyph_string P_ ((struct glyph_string *));
static void x_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					   int, int, int));
static void x_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				    int, int, int, int, Rect *));
static void x_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				 int, int, int, Rect *));
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
    s->gc = s->f->output_data.mac->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.mac->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.mac->cursor_foreground_pixel;
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

      if (FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (s->display, FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (s->display, s->window, mask, &xgcv);

      s->gc = FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc;
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
  face_id = FRAME_X_DISPLAY_INFO (s->f)->mouse_face_face_id;
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
      
      if (FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (s->display, FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (s->display, s->window, mask, &xgcv);
      
      s->gc = FRAME_MAC_DISPLAY_INFO (s->f)->scratch_cursor_gc;
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
x_get_glyph_string_clip_rect (s, r)
     struct glyph_string *s;
     Rect *r;
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
	r->top -= s->f->output_data.mac->internal_border_width;
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
  Rect r;
  x_get_glyph_string_clip_rect (s, &r);
  mac_set_clip_rectangle (s->display, s->window, &r);
}


/* Compute left and right overhang of glyph string S.  If S is a glyph
   string for a composition, assume overhangs don't exist.  */

static INLINE void
x_compute_glyph_string_overhangs (s)
     struct glyph_string *s;
{
  if (s->cmp == NULL
      && s->first_glyph->type == CHAR_GLYPH)
    {
      XCharStruct cs;
      int direction, font_ascent, font_descent;
      XTextExtents16 (s->font, s->char2b, s->nchars, &direction,
		      &font_ascent, &font_descent, &cs);
      s->right_overhang = cs.rbearing > cs.width ? cs.rbearing - cs.width : 0;
      s->left_overhang = cs.lbearing < 0 ? -cs.lbearing : 0;
    }
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

void
x_get_glyph_overhangs (glyph, f, left, right)
     struct glyph *glyph;
     struct frame *f;
     int *left, *right;
{
  *left = *right = 0;
  
  if (glyph->type == CHAR_GLYPH)
    {
      XFontStruct *font;
      struct face *face;
      struct font_info *font_info;
      XChar2b char2b;
      XCharStruct *pcm;

      face = x_get_glyph_face_and_encoding (f, glyph, &char2b, NULL);
      font = face->font;
      font_info = FONT_INFO_FROM_ID (f, face->font_info_id);
      if (font
	  && (pcm = x_per_char_metric (font, &char2b)))
	{
	  if (pcm->rbearing > pcm->width)
	    *right = pcm->rbearing - pcm->width;
	  if (pcm->lbearing < 0)
	    *left = -pcm->lbearing;
	}
    }
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
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
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
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
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
  XGCValues xgcv;

  xgcv.foreground = s->gc->background;
  XFillRectangle (s->display, s->window, &xgcv, x, y, w, h);
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

#if 0 /* MAC_TODO: stipple */
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

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  mac_draw_rectangle (s->display, s->window,
			    s->gc, x, s->y, g->pixel_width - 1,
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
	  char1b[i] = s->char2b[i].byte2;

      /* Draw text with XDrawString if background has already been
	 filled.  Otherwise, use XDrawImageString.  (Note that
	 XDrawImageString is usually faster than XDrawString.)  Always
	 use XDrawImageString when drawing the cursor so that there is
	 no chance that characters under a box cursor are invisible.  */
      if (s->for_overlaps_p
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	{
	  /* Draw characters with 16-bit or 8-bit functions.  */
	  if (s->two_byte_p)
	    XDrawString16 (s->display, s->window, s->gc, x,
			   s->ybase - boff, s->char2b, s->nchars);
	  else
	    XDrawString (s->display, s->window, s->gc, x,
			 s->ybase - boff, char1b, s->nchars);
	}
      else
	{
	  if (s->two_byte_p)
	    XDrawImageString16 (s->display, s->window, s->gc, x,
			        s->ybase - boff, s->char2b, s->nchars);
	  else
	    XDrawImageString (s->display, s->window, s->gc, x,
			      s->ybase - boff, char1b, s->nchars);
	}
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, x;

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

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->gidx == 0)
	mac_draw_rectangle (s->display, s->window, s->gc, x, s->y,
			  s->width - 1, s->height - 1);
    }
  else
    {
      for (i = 0; i < s->nchars; i++, ++s->gidx)
	XDrawString16 (s->display, s->window, s->gc,
		       x + s->cmp->offsets[s->gidx * 2],
		       s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
		       s->char2b + i, 1);
    }
}


#ifdef USE_X_TOOLKIT

static struct frame *x_frame_of_widget P_ ((Widget));


/* Return the frame on which widget WIDGET is used.. Abort if frame
   cannot be determined.  */

static struct frame *
x_frame_of_widget (widget)
     Widget widget;
{
  struct x_display_info *dpyinfo;
  Lisp_Object tail;
  struct frame *f;
  
  dpyinfo = x_display_info_for_display (XtDisplay (widget));
  
  /* Find the top-level shell of the widget.  Note that this function
     can be called when the widget is not yet realized, so XtWindow
     (widget) == 0.  That's the reason we can't simply use
     x_any_window_to_frame.  */
  while (!XtIsTopLevelShell (widget))
    widget = XtParent (widget);

  /* Look for a frame with that top-level widget.  Allocate the color
     on that frame to get the right gamma correction value.  */
  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    if (GC_FRAMEP (XCAR (tail))
	&& (f = XFRAME (XCAR (tail)),
	    (f->output_data.nothing != 1
	     && FRAME_X_DISPLAY_INFO (f) == dpyinfo))
	&& f->output_data.x->widget == widget)
      return f;

  abort ();
}


/* Allocate the color COLOR->pixel on the screen and display of
   widget WIDGET in colormap CMAP.  If an exact match cannot be
   allocated, try the nearest color available.  Value is non-zero
   if successful.  This is called from lwlib.  */

int
x_alloc_nearest_color_for_widget (widget, cmap, color)
     Widget widget;
     Colormap cmap;
     XColor *color;
{
  struct frame *f = x_frame_of_widget (widget);
  return x_alloc_nearest_color (f, cmap, color);
}


#endif /* USE_X_TOOLKIT */

#if 0 /* MAC_TODO */

/* Allocate the color COLOR->pixel on SCREEN of DISPLAY, colormap
   CMAP.  If an exact match can't be allocated, try the nearest color
   available.  Value is non-zero if successful.  Set *COLOR to the
   color allocated.  */

int
x_alloc_nearest_color (f, cmap, color)
     struct frame *f;
     Colormap cmap;
     XColor *color;
{
  Display *display = FRAME_X_DISPLAY (f);
  Screen *screen = FRAME_X_SCREEN (f);
  int rc;

  gamma_correct (f, color);
  rc = XAllocColor (display, cmap, color);
  if (rc == 0)
    {
      /* If we got to this point, the colormap is full, so we're going
	 to try to get the next closest color.  The algorithm used is
	 a least-squares matching, which is what X uses for closest
	 color matching with StaticColor visuals.  */
      int nearest, i;
      unsigned long nearest_delta = ~0;
      int ncells = XDisplayCells (display, XScreenNumberOfScreen (screen));
      XColor *cells = (XColor *) alloca (ncells * sizeof *cells);

      for (i = 0; i < ncells; ++i)
	cells[i].pixel = i;
      XQueryColors (display, cmap, cells, ncells);

      for (nearest = i = 0; i < ncells; ++i)
	{
	  long dred   = (color->red   >> 8) - (cells[i].red   >> 8);
	  long dgreen = (color->green >> 8) - (cells[i].green >> 8);
	  long dblue  = (color->blue  >> 8) - (cells[i].blue  >> 8);
	  unsigned long delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (delta < nearest_delta)
	    {
	      nearest = i;
	      nearest_delta = delta;
	    }
	}
      
      color->red   = cells[nearest].red;
      color->green = cells[nearest].green;
      color->blue  = cells[nearest].blue;
      rc = XAllocColor (display, cmap, color);
    }

#ifdef DEBUG_X_COLORS
  if (rc)
    register_color (color->pixel);
#endif /* DEBUG_X_COLORS */
  
  return rc;
}


/* Allocate color PIXEL on frame F.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  XQueryColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &color);
  XAllocColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}


/* Allocate color PIXEL on display DPY.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_dpy_color (dpy, cmap, pixel)
     Display *dpy;
     Colormap cmap;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  XQueryColor (dpy, cmap, &color);
  XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}

#endif /* MAC_TODO */

/* Allocate a color which is lighter or darker than *COLOR by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as COLOR, try a color where all RGB
   values have DELTA added.  Return the allocated color in *COLOR.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static int
mac_alloc_lighter_color (f, color, factor, delta)
     struct frame *f;
     unsigned long *color;
     double factor;
     int delta;
{
  unsigned long new;

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  xassert (factor >= 0);
  new = RGB_TO_ULONG (min (0xff, (int) (factor * RED_FROM_ULONG (*color))),
                    min (0xff, (int) (factor * GREEN_FROM_ULONG (*color))),
                    min (0xff, (int) (factor * BLUE_FROM_ULONG (*color))));
  if (new == *color)
    new = RGB_TO_ULONG (max (0, min (0xff, (int) (delta + RED_FROM_ULONG (*color)))),
                      max (0, min (0xff, (int) (delta + GREEN_FROM_ULONG (*color)))),
                      max (0, min (0xff, (int) (delta + BLUE_FROM_ULONG (*color)))));

  /* MAC_TODO: Map to palette and retry with delta if same? */
  /* MAC_TODO: Free colors (if using palette)? */

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
x_setup_relief_color (f, relief, factor, delta, default_pixel)
     struct frame *f;
     struct relief *relief;
     double factor;
     int delta;
     unsigned long default_pixel;
{
  XGCValues xgcv;
  struct mac_output *di = f->output_data.mac;
  unsigned long mask = GCForeground;
  unsigned long pixel;
  unsigned long background = di->relief_background;
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  /* MAC_TODO: Free colors (if using palette)? */

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (mac_alloc_lighter_color (f, &pixel, factor, delta))
    {
      relief->allocated_p = 1;
      xgcv.foreground = relief->pixel = pixel;
    }
  
  if (relief->gc == 0)
    {
#if 0 /* MAC_TODO: stipple */
      xgcv.stipple = dpyinfo->gray;
      mask |= GCStipple;
#endif
      relief->gc = XCreateGC (NULL, FRAME_MAC_WINDOW (f), mask, &xgcv);
    }
  else
    XChangeGC (NULL, relief->gc, mask, &xgcv);
}


/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (s)
     struct glyph_string *s;
{
  struct mac_output *di = s->f->output_data.mac;
  unsigned long color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else
    {
      XGCValues xgcv;
      
      /* Get the background color of the face.  */
      XGetGCValues (s->display, s->gc, GCBackground, &xgcv);
      color = xgcv.background;
    }

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      x_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
			    WHITE_PIX_DEFAULT (s->f));
      x_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
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
x_draw_relief_rect (f, left_x, top_y, right_x, bottom_y, width,
		    raised_p, left_p, right_p, clip_rect)
     struct frame *f;
     int left_x, top_y, right_x, bottom_y, left_p, right_p, raised_p;
     Rect *clip_rect;
{
  int i;
  GC gc;
  
  if (raised_p)
    gc = f->output_data.mac->white_relief.gc;
  else
    gc = f->output_data.mac->black_relief.gc;
  mac_set_clip_rectangle (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), clip_rect);

  /* Top.  */
  for (i = 0; i < width; ++i)
    XDrawLine (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), gc,
	       left_x + i * left_p, top_y + i,
	       right_x + 1 - i * right_p, top_y + i);

  /* Left.  */
  if (left_p)
    for (i = 0; i < width; ++i)
      XDrawLine (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), gc,
		 left_x + i, top_y + i, left_x + i, bottom_y - i);

  mac_reset_clipping (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f));
  if (raised_p)
    gc = f->output_data.mac->black_relief.gc;
  else
    gc = f->output_data.mac->white_relief.gc;
  mac_set_clip_rectangle (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
			  clip_rect);
  
  /* Bottom.  */
  for (i = 0; i < width; ++i)
    XDrawLine (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), gc,
	       left_x + i * left_p, bottom_y - i,
	       right_x + 1 - i * right_p, bottom_y - i);
  
  /* Right.  */
  if (right_p)
    for (i = 0; i < width; ++i)
      XDrawLine (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), gc,
		 right_x - i, top_y + i + 1, right_x - i, bottom_y - i);

  mac_reset_clipping (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f));
}


/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P non-zero means draw a line on the
   left side of the rectangle.  RIGHT_P non-zero means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
		 left_p, right_p, clip_rect)
     struct glyph_string *s;
     int left_x, top_y, right_x, bottom_y, left_p, right_p;
     Rect *clip_rect;
{
  XGCValues xgcv;
  
  xgcv.foreground = s->face->box_color;
  mac_set_clip_rectangle (s->display, s->window, clip_rect);
  
  /* Top.  */
  XFillRectangle (s->display, s->window, &xgcv,
		  left_x, top_y, right_x - left_x, width);

  /* Left.  */
  if (left_p)
    XFillRectangle (s->display, s->window, &xgcv,
		    left_x, top_y, width, bottom_y - top_y);

  /* Bottom.  */
  XFillRectangle (s->display, s->window, &xgcv,
		  left_x, bottom_y - width, right_x - left_x, width);
  
  /* Right.  */
  if (right_p)
    XFillRectangle (s->display, s->window, &xgcv,
		    right_x - width, top_y, width, bottom_y - top_y);

  mac_reset_clipping (s->display, s->window);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (s)
     struct glyph_string *s;
{
  int width, left_x, right_x, top_y, bottom_y, last_x, raised_p;
  int left_p, right_p;
  struct glyph *last_glyph;
  Rect clip_rect;

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
  
  x_get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
		     left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      x_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
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

  if (s->img->pixmap)
    {
#if 0 /* MAC_TODO: image mask */
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
	  
	  x_get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->img->width;
	  image_rect.height = s->img->height;
	  if (x_intersect_rectangles (&clip_rect, &image_rect, &r))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       r.x - x, r.y - y, r.width, r.height, r.x, r.y);
	}
      else
#endif /* MAC_TODO */
	{
	  mac_copy_area (s->display, s->img->pixmap, s->window, s->gc,
		       0, 0, s->img->width, s->img->height, x, y);
	  
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
	      mac_draw_rectangle (s->display, s->window, s->gc, x - r, y - r,
				  s->img->width + r*2 - 1, s->img->height + r*2 - 1);
	    }
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    mac_draw_rectangle (s->display, s->window, s->gc, x, y,
		      s->img->width - 1, s->img->height - 1);
}



/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (s)
     struct glyph_string *s;
{
  int x0, y0, x1, y1, thick, raised_p;
  Rect r;
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
  x_get_glyph_string_clip_rect (s, &r);
  x_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p, 1, 1, &r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
x_draw_image_foreground_1 (s, pixmap)
     struct glyph_string *s;
     Pixmap pixmap;
{
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
#if 0 /* MAC_TODO: image mask */
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
#endif /* MAC_TODO */
	{
	  mac_copy_area_to_pixmap (s->display, s->img->pixmap, pixmap, s->gc,
		               0, 0, s->img->width, s->img->height, x, y);
	  
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
	      mac_draw_rectangle_to_pixmap (s->display, pixmap, s->gc, x - r, y - r,
				  s->img->width + r*2 - 1, s->img->height + r*2 - 1);
	    }
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    mac_draw_rectangle_to_pixmap (s->display, pixmap, s->gc, x, y,
		              s->img->width - 1, s->img->height - 1);
}


/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (s, x, y, w, h)
     struct glyph_string *s;
     int x, y, w, h;
{
#if 0 /* MAC_TODO: stipple */
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
      XFillRectangle (s->display, s->window, s->gc, x, y, w, h);
      XSetFillStyle (s->display, s->gc, FillSolid);
    }
  else
#endif /* MAC_TODO */
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
  Pixmap pixmap = 0;

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
      x_draw_image_foreground_1 (s, pixmap);
      x_set_glyph_string_clipping (s);
      mac_copy_area (s->display, pixmap, s->window, s->gc,
		   0, 0, s->background_width, s->height, s->x, s->y);
      mac_reset_clipping (s->display, s->window);
      XFreePixmap (s->display, pixmap);
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
	  GC gc = s->face->gc;
	  int x = s->x + width, y = s->y;
	  int w = s->background_width - width, h = s->height;
	  Rect r;

	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;
  
	  x_get_glyph_string_clip_rect (s, &r);
	  mac_set_clip_rectangle (s->display, s->window, &r);

#if 0 /* MAC_TODO: stipple */
	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, gc, FillOpaqueStippled);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetFillStyle (s->display, gc, FillSolid);
	    }
	  else
#endif /* MAC_TODO */
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, gc, GCForeground | GCBackground, &xgcv);
	      XSetForeground (s->display, gc, xgcv.background);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetForeground (s->display, gc, xgcv.foreground);
	    }

	  mac_reset_clipping (s->display, s->window);
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
      if (s->face->underline_p)
	{
          unsigned long h = 1;
          unsigned long dy = s->height - h;
      
	  if (s->face->underline_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->underline_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->overline_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}
  
      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  unsigned long h = 1;
	  unsigned long dy = (s->height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->strike_through_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}
  
      /* Draw relief.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
        x_draw_glyph_string_box (s);
    }

  /* Reset clipping.  */
  mac_reset_clipping (s->display, s->window);
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
x_init_glyph_string (s, char2b, w, row, area, start, hl)
     struct glyph_string *s;
     XChar2b *char2b;
     struct window *w;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start;
     enum draw_glyphs_face hl;
{
  bzero (s, sizeof *s);
  s->w = w;
  s->f = XFRAME (w->frame);
  s->display = FRAME_MAC_DISPLAY (s->f);
  s->window = FRAME_MAC_WINDOW (s->f);
  s->char2b = char2b;
  s->hl = hl;
  s->row = row;
  s->area = area;
  s->first_glyph = row->glyphs[area] + start;
  s->height = row->height;
  s->y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  /* Display the internal border below the tool-bar window.  */
  if (s->w == XWINDOW (s->f->tool_bar_window))
    s->y -= s->f->output_data.mac->internal_border_width;
  
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

/* SunOS 4 bundled cc, barfed on continuations in the arg lists here
   and below -- keep them on one line.  */
#define BUILD_STRETCH_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									    \
       {								    \
	 s = (struct glyph_string *) alloca (sizeof *s);		    \
	 x_init_glyph_string (s, NULL, W, ROW, AREA, START, HL);	    \
	 START = x_fill_stretch_glyph_string (s, ROW, AREA, START, END);    \
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

#define BUILD_IMAGE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									\
       {								\
	 s = (struct glyph_string *) alloca (sizeof *s);		\
	 x_init_glyph_string (s, NULL, W, ROW, AREA, START, HL);        \
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

#define BUILD_CHAR_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 int c, face_id;						   \
	 XChar2b *char2b;						   \
									   \
	 c = (ROW)->glyphs[AREA][START].u.ch;				   \
	 face_id = (ROW)->glyphs[AREA][START].face_id;			   \
									   \
	 s = (struct glyph_string *) alloca (sizeof *s);		   \
	 char2b = (XChar2b *) alloca ((END - START) * sizeof *char2b);	   \
	 x_init_glyph_string (s, char2b, W, ROW, AREA, START, HL);	   \
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

#define BUILD_COMPOSITE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)	  \
  do {									  \
    int cmp_id = (ROW)->glyphs[AREA][START].u.cmp_id;			  \
    int face_id = (ROW)->glyphs[AREA][START].face_id;			  \
    struct face *base_face = FACE_FROM_ID (XFRAME (w->frame), face_id);	  \
    struct composition *cmp = composition_table[cmp_id];		  \
    int glyph_len = cmp->glyph_len;					  \
    XChar2b *char2b;							  \
    struct face **faces;						  \
    struct glyph_string *first_s = NULL;				  \
    int n;								  \
    									  \
    base_face = base_face->ascii_face;					  \
    char2b = (XChar2b *) alloca ((sizeof *char2b) * glyph_len);		  \
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
      {									  \
	s = (struct glyph_string *) alloca (sizeof *s);			  \
	x_init_glyph_string (s, char2b + n, W, ROW, AREA, START, HL);	  \
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

#define BUILD_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 HEAD = TAIL = NULL;						   \
	 while (START < END)						   \
	   {								   \
             struct glyph *first_glyph = (ROW)->glyphs[AREA] + START;	   \
             switch (first_glyph->type)					   \
	       {							   \
	       case CHAR_GLYPH:						   \
                 BUILD_CHAR_GLYPH_STRINGS (W, ROW, AREA, START, END, HEAD, \
		                           TAIL, HL, X, LAST_X,		   \
                                           OVERLAPS_P);	                   \
		 break;							   \
									   \
	       case COMPOSITE_GLYPH:					   \
                 BUILD_COMPOSITE_GLYPH_STRING (W, ROW, AREA, START, END,   \
						 HEAD, TAIL, HL, X, LAST_X,\
						 OVERLAPS_P);		   \
		 break;							   \
									   \
	       case STRETCH_GLYPH:					   \
		 BUILD_STRETCH_GLYPH_STRING (W, ROW, AREA, START, END,	   \
					     HEAD, TAIL, HL, X, LAST_X);   \
		 break;							   \
									   \
	       case IMAGE_GLYPH:					   \
		 BUILD_IMAGE_GLYPH_STRING (W, ROW, AREA, START, END, HEAD, \
					   TAIL, HL, X, LAST_X);	   \
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
  BUILD_GLYPH_STRINGS (w, row, area, i, end, head, tail, hl, x, last_x,
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
	  BUILD_GLYPH_STRINGS (w, row, area, j, start, h, t,
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
	  BUILD_GLYPH_STRINGS (w, row, area, i, start, h, t,
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
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
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
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
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

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  w = updated_window;
  f = XFRAME (WINDOW_FRAME (w));

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

  mac_scroll_area (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
	         f->output_data.mac->normal_gc,
	         frame_x, frame_y,
	         shifted_region_width, line_height,
	         frame_x + shift_by_width, frame_y);

  /* Write the glyphs.  */
  hpos = start - row->glyphs[updated_area];
  x_draw_glyphs (w, output_cursor.x, row, updated_area, hpos, hpos + len,
		 DRAW_NORMAL_TEXT, 0);

  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x += shift_by_width;
  UNBLOCK_INPUT;
}


/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (n)
     register int n;
{
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
      BLOCK_INPUT;
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		  from_x, from_y, to_x - from_x, to_y - from_y,
		  0);
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

  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  /* We don't set the output cursor here because there will always
     follow an explicit cursor_to.  */
  BLOCK_INPUT;
  XClearWindow (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f));

#if 0  /* Clearing frame on Mac OS clears scroll bars.  */
  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);
#endif

  XFlush (FRAME_MAC_DISPLAY (f));
  UNBLOCK_INPUT;
}



/* Invert the middle quarter of the frame for .15 sec.  */

/* We use the select system call to do the waiting, so we have to make
   sure it's available.  If it isn't, we just won't do visual bells.  */

#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)

/* Subtract the `struct timeval' values X and Y, storing the result in
   *RESULT.  Return 1 if the difference is negative, otherwise 0.  */

static int
timeval_subtract (result, x, y)
     struct timeval *result, x, y;
{
  /* Perform the carry for the later subtraction by updating y.  This
     is safer because on some systems the tv_sec member is unsigned.  */
  if (x.tv_usec < y.tv_usec)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000 + 1;
      y.tv_usec -= 1000000 * nsec;
      y.tv_sec += nsec;
    }
  
  if (x.tv_usec - y.tv_usec > 1000000)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000;
      y.tv_usec += 1000000 * nsec;
      y.tv_sec -= nsec;
    }

  /* Compute the time remaining to wait.  tv_usec is certainly
     positive.  */
  result->tv_sec = x.tv_sec - y.tv_sec;
  result->tv_usec = x.tv_usec - y.tv_usec;

  /* Return indication of whether the result should be considered
     negative.  */
  return x.tv_sec < y.tv_sec;
}

void
XTflash (f)
     struct frame *f;
{
  BLOCK_INPUT;

  FlashMenuBar (0);

  {
    struct timeval wakeup;

    EMACS_GET_TIME (wakeup);

    /* Compute time to wait until, propagating carry from usecs.  */
    wakeup.tv_usec += 150000;
    wakeup.tv_sec += (wakeup.tv_usec / 1000000);
    wakeup.tv_usec %= 1000000;

    /* Keep waiting until past the time wakeup.  */
    while (1)
      {
        struct timeval timeout;

        EMACS_GET_TIME (timeout);

        /* In effect, timeout = wakeup - timeout.
           Break if result would be negative.  */
        if (timeval_subtract (&timeout, wakeup, timeout))
          break;

        /* Try to wait that long--but we might wake up sooner.  */
        select (0, NULL, NULL, NULL, &timeout);
      }
  }
  
  FlashMenuBar (0);

  UNBLOCK_INPUT;
}

#endif /* defined (HAVE_TIMEVAL) && defined (HAVE_SELECT) */


/* Make audible bell.  */

void
XTring_bell ()
{
  struct frame *f = SELECTED_FRAME ();
  
#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)
  if (visible_bell)
    XTflash (f);
  else
#endif
    {
      BLOCK_INPUT;
      SysBeep (1);
      XFlush (FRAME_MAC_DISPLAY (f));
      UNBLOCK_INPUT;
    }
}



/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to x_update_begin and x_update_end.  */

void
XTset_terminal_window (n)
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
    }
  else
    {
      /* Scolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  BLOCK_INPUT;
  
  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  updated_window = w;
  x_clear_cursor (w);

  mac_scroll_area (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
	         f->output_data.mac->normal_gc,
	         x, from_y,
	         width, height,
	         x, to_y);
  
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
  Rect r;
  int mouse_face_overwritten_p = 0;

  TRACE ((stderr, "expose_frame "));

  /* No need to redraw if frame will be redrawn soon.  */
  if (FRAME_GARBAGED_P (f))
    {
      TRACE ((stderr, " garbaged\n"));
      return;
    }

  /* MAC_TODO: this is a kludge, but if scroll bars are not activated
     or deactivated here, for unknown reasons, activated scroll bars
     are shown in deactivated frames in some instances.  */
  if (f == FRAME_MAC_DISPLAY_INFO (f)->x_focus_frame)
    activate_scroll_bars (f);
  else
    deactivate_scroll_bars (f);

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
      struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
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
     Rect *r;
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
     Rect *r;
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
     Rect *r;
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
     Rect *r;
{
  Rect cr, result;
  struct glyph *cursor_glyph;

  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph)
    {
      cr.left = w->phys_cursor.x;
      cr.top = w->phys_cursor.y;
      cr.right = cr.left + cursor_glyph->pixel_width;
      cr.bottom = cr.top + w->phys_cursor_height;
      return x_intersect_rectangles (&cr, r, &result);
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
     Rect *fr;
{
  struct frame *f = XFRAME (w->frame);
  Rect wr, r;
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

  if (x_intersect_rectangles (fr, &wr, &r))
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

  /* Display scroll bar for this window.  */
  if (!NILP (w->vertical_scroll_bar))
    {
      ControlHandle ch
	= SCROLL_BAR_CONTROL_HANDLE (XSCROLL_BAR (w->vertical_scroll_bar));

      Draw1Control (ch);
    }

  return mouse_face_overwritten_p;
}

static int
x_intersect_rectangles (r1, r2, result)
     Rect *r1, *r2, *result;
{
  Rect *left, *right;
  Rect *upper, *lower;
  int intersection_p = 0;
  
  /* Rerrange so that R1 is the left-most rectangle.  */
  if (r1->left < r2->left)
    left = r1, right = r2;
  else
    left = r2, right = r1;

  /* X0 of the intersection is right.x0, if this is inside R1,
     otherwise there is no intersection.  */
  if (right->left <= left->right)
    {
      result->left = right->left;
      
      /* The right end of the intersection is the minimum of the
	 the right ends of left and right.  */
      result->right = min (left->right, right->right);

      /* Same game for Y.  */
      if (r1->top < r2->top)
	upper = r1, lower = r2;
      else
	upper = r2, lower = r1;

      /* The upper end of the intersection is lower.y0, if this is inside
	 of upper.  Otherwise, there is no intersection.  */
      if (lower->top <= upper->bottom)
	{
	  result->top = lower->top;
	  
	  /* The lower end of the intersection is the minimum of the lower
	     ends of upper and lower.  */
	  result->bottom = min (lower->bottom, upper->bottom);
	  intersection_p = 1;
	}
    }

  return intersection_p;
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
     struct x_display_info *dpyinfo;
     struct frame *frame;
{
  struct frame *old_focus = dpyinfo->x_focus_frame;

  if (frame != dpyinfo->x_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->x_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

#if 0
      selected_frame = frame;
      XSETFRAME (XWINDOW (selected_frame->selected_window)->frame,
		 selected_frame);
      Fselect_window (selected_frame->selected_window);
      choose_minibuf_frame ();
#endif /* ! 0 */

      if (dpyinfo->x_focus_frame && dpyinfo->x_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->x_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (dpyinfo)
     struct x_display_info *dpyinfo;
{
  x_new_focus_frame (dpyinfo, dpyinfo->x_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
XTframe_rehighlight (frame)
     struct frame *frame;
{
  x_frame_rehighlight (FRAME_X_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (dpyinfo)
     struct x_display_info *dpyinfo;
{
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
	   : dpyinfo->x_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->x_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame) = Qnil;
	  dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame;
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



/* Keyboard processing - modifier keys, vendor-specific keysyms, etc.  */

#if 0 /* MAC_TODO */
/* Initialize mode_switch_bit and modifier_meaning.  */
static void
x_find_modifier_meanings (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int min_code, max_code;
  KeySym *syms;
  int syms_per_code;
  XModifierKeymap *mods;

  dpyinfo->meta_mod_mask = 0;
  dpyinfo->shift_lock_mask = 0;
  dpyinfo->alt_mod_mask = 0;
  dpyinfo->super_mod_mask = 0;
  dpyinfo->hyper_mod_mask = 0;

#ifdef HAVE_X11R4
  XDisplayKeycodes (dpyinfo->display, &min_code, &max_code);
#else
  min_code = dpyinfo->display->min_keycode;
  max_code = dpyinfo->display->max_keycode;
#endif

  syms = XGetKeyboardMapping (dpyinfo->display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);
  mods = XGetModifierMapping (dpyinfo->display);

  /* Scan the modifier table to see which modifier bits the Meta and
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table.  */

    for (row = 3; row < 8; row++)
      for (col = 0; col < mods->max_keypermod; col++)
	{
	  KeyCode code
	    = mods->modifiermap[(row * mods->max_keypermod) + col];

	  /* Zeroes are used for filler.  Skip them.  */
	  if (code == 0)
	    continue;

	  /* Are any of this keycode's keysyms a meta key?  */
	  {
	    int code_col;

	    for (code_col = 0; code_col < syms_per_code; code_col++)
	      {
		int sym = syms[((code - min_code) * syms_per_code) + code_col];

		switch (sym)
		  {
		  case XK_Meta_L:
		  case XK_Meta_R:
		    dpyinfo->meta_mod_mask |= (1 << row);
		    break;

		  case XK_Alt_L:
		  case XK_Alt_R:
		    dpyinfo->alt_mod_mask |= (1 << row);
		    break;

		  case XK_Hyper_L:
		  case XK_Hyper_R:
		    dpyinfo->hyper_mod_mask |= (1 << row);
		    break;

		  case XK_Super_L:
		  case XK_Super_R:
		    dpyinfo->super_mod_mask |= (1 << row);
		    break;

		  case XK_Shift_Lock:
		    /* Ignore this if it's not on the lock modifier.  */
		    if ((1 << row) == LockMask)
		      dpyinfo->shift_lock_mask = LockMask;
		    break;
		  }
	      }
	  }
	}
  }

  /* If we couldn't find any meta keys, accept any alt keys as meta keys.  */
  if (! dpyinfo->meta_mod_mask)
    {
      dpyinfo->meta_mod_mask = dpyinfo->alt_mod_mask;
      dpyinfo->alt_mod_mask = 0;
    }

  /* If some keys are both alt and meta,
     make them just meta, not alt.  */
  if (dpyinfo->alt_mod_mask & dpyinfo->meta_mod_mask)
    {
      dpyinfo->alt_mod_mask &= ~dpyinfo->meta_mod_mask;
    }

  XFree ((char *) syms);
  XFreeModifiermap (mods);
}

#endif /* MAC_TODO */

/* Convert between the modifier bits X uses and the modifier bits
   Emacs uses.  */

static unsigned int
x_mac_to_emacs_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned short state;
{
  return (((state & shiftKey) ? shift_modifier : 0)
	  | ((state & controlKey) ? ctrl_modifier : 0)
	  | ((state & cmdKey) ? meta_modifier : 0)
	  | ((state & optionKey) ? alt_modifier : 0));
}

#if 0 /* MAC_TODO */
static unsigned short
x_emacs_to_x_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned int state;
{
  return (  ((state & alt_modifier)	? dpyinfo->alt_mod_mask   : 0)
	  | ((state & super_modifier)	? dpyinfo->super_mod_mask : 0)
	  | ((state & hyper_modifier)	? dpyinfo->hyper_mod_mask : 0)
	  | ((state & shift_modifier)	? ShiftMask        : 0)
	  | ((state & ctrl_modifier)	? ControlMask      : 0)
	  | ((state & meta_modifier)	? dpyinfo->meta_mod_mask  : 0));
}
#endif /* MAC_TODO */

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (keysym)
     int keysym;
{
  char *value;

  BLOCK_INPUT;
#if 0
  value = XKeysymToString (keysym);
#else
  value = 0;
#endif
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
     Rect *bounds;
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
    pix_y -= (f)->output_data.mac->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->left = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->top = CHAR_TO_PIXEL_ROW (f, pix_y);
      bounds->right  = bounds->left + FONT_WIDTH  (FRAME_FONT (f)) - 1;
      bounds->bottom = bounds->top + f->output_data.mac->line_height - 1;
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


/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (result, event, f)
     struct input_event *result;
     EventRecord *event;
     struct frame *f;
{
  Point mouseLoc;

  result->kind = MOUSE_CLICK_EVENT;
  result->code = 0;  /* only one mouse button */
  result->timestamp = event->when;
  result->modifiers = event->what == mouseDown ? down_modifier : up_modifier;

  mouseLoc = event->where;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (FRAME_MAC_WINDOW (f)));
#else
  SetPort (FRAME_MAC_WINDOW (f));
#endif

  GlobalToLocal (&mouseLoc);
  XSETINT (result->x, mouseLoc.h);
  XSETINT (result->y, mouseLoc.v);

  XSETFRAME (result->frame_or_window, f);

  result->arg = Qnil;
  return Qnil;
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static Point last_mouse_motion_position;
static Lisp_Object last_mouse_motion_frame;

static void
note_mouse_movement (frame, pos)
     FRAME_PTR frame;
     Point *pos;
{
#if TARGET_API_MAC_CARBON
  Rect r;
#endif

  last_mouse_movement_time = TickCount () * (1000 / 60);  /* to milliseconds */
  last_mouse_motion_position = *pos;
  XSETFRAME (last_mouse_motion_frame, frame);

#if TARGET_API_MAC_CARBON
  if (!PtInRect (*pos, GetWindowPortBounds (FRAME_MAC_WINDOW (frame), &r)))
#else
  if (!PtInRect (*pos, &FRAME_MAC_WINDOW (frame)->portRect))
#endif
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
    }
  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (pos->h < last_mouse_glyph.left
	   || pos->h >= last_mouse_glyph.right
	   || pos->v < last_mouse_glyph.top
	   || pos->v >= last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, pos->h, pos->v);
    }
}

/* This is used for debugging, to turn off note_mouse_highlight.  */

int disable_mouse_highlight;



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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  struct Cursor *cursor = dpyinfo->vertical_scroll_bar_cursor;
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
	    cursor = f->output_data.mac->nontext_cursor;
	  else
	    {
	      map = Fget_text_property (make_number (glyph->charpos),
					Qkeymap, glyph->object);
	      if (KEYMAPP (map))
		cursor = f->output_data.mac->nontext_cursor;
	    }
	}
    }

#if 0 /* MAC_TODO: mouse cursor */
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  int portion;
  Lisp_Object window;
  struct window *w;
  struct buffer *b;

#if 0
  /* When a menu is active, don't highlight because this looks odd. */
  if (popup_activated ())
    return;
#endif

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
			  last_mouse_motion_position.h,
			  last_mouse_motion_position.v);
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
   item in F->current_tool_bar_items.  Value is

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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int area;

  /* Find the glyph under X/Y.  */
  *glyph = x_y_to_hpos_vpos (w, x, y, hpos, vpos, &area, 0);
  if (*glyph == NULL)
    return -1;

  /* Get the start of this tool-bar item's properties in
     f->current_tool_bar_items.  */
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
x_handle_tool_bar_click (f, button_event)
     struct frame *f;
     EventRecord *button_event;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int hpos, vpos, prop_idx;
  struct glyph *glyph;
  Lisp_Object enabled_p;
  int x = button_event->where.h;
  int y = button_event->where.v;
  
  /* If not on the highlighted tool-bar item, return.  */
  frame_to_window_pixel_xy (w, &x, &y);
  if (x_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx) != 0)
    return;

  /* If item is disabled, do nothing.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (NILP (enabled_p))
    return;
  
  if (button_event->what == mouseDown)
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
      event.modifiers = x_mac_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
						  button_event->modifiers);
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
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
     XTread_socket does the rest.  */
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
     struct mac_display_info *dpyinfo;
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

#if 0 /* MAC_TODO: mouse cursor */
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
     struct mac_display_info *dpyinfo;
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (XFRAME (w->frame));
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

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
static int glyph_rect P_ ((struct frame *f, int, int, Rect *));


/* Try to determine frame pixel position and size of the glyph under
   frame pixel coordinates X/Y on frame F .  Return the position and
   size in *RECT.  Value is non-zero if we could compute these
   values.  */

static int
glyph_rect (f, x, y, rect)
     struct frame *f;
     int x, y;
     Rect *rect;
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
XTmouse_position (fp, insist, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     int insist;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  Point mouse_pos;
  int ignore1, ignore2;
  WindowPtr wp = FrontWindow ();
  struct frame *f = ((mac_output *) GetWRefCon (wp))->mFP;            
  Lisp_Object frame, tail;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar) && insist == 0)
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
        XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = Qnil;

#if TARGET_API_MAC_CARBON
      SetPort (GetWindowPort (wp));
#else
      SetPort (wp);
#endif

      GetMouse (&mouse_pos);

      pixel_to_glyph_coords (f, mouse_pos.h, mouse_pos.v, &ignore1, &ignore2,
                             &last_mouse_glyph, insist);

      *bar_window = Qnil;
      *part = scroll_bar_handle;
      *fp = f;
      XSETINT (*x, mouse_pos.h);
      XSETINT (*y, mouse_pos.v);
      *time = last_mouse_movement_time;
    }
  
  UNBLOCK_INPUT;
}


/************************************************************************
			 Scroll bars, general
 ************************************************************************/
									 
/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (w, top, left, width, height, disp_top, disp_height)
     struct window *w;
     int top, left, width, height, disp_top, disp_height;
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));
  Rect r;
  ControlHandle ch;

  BLOCK_INPUT;

  r.left = left;
  r.top = disp_top;
  r.right = left + width;
  r.bottom = disp_top + disp_height;
  
#ifdef TARGET_API_MAC_CARBON
  ch = NewControl (FRAME_MAC_WINDOW (f), &r, "\p", 1, 0, 0, 0,
		   kControlScrollBarProc, 0L);
#else
  ch = NewControl (FRAME_MAC_WINDOW (f), &r, "\p", 1, 0, 0, 0, scrollBarProc,
		   0L);
#endif
  SET_SCROLL_BAR_CONTROL_HANDLE (bar, ch);
  SetControlReference (ch, (long) bar);

  XSETWINDOW (bar->window, w);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  UNBLOCK_INPUT;
  return bar;
}


/* Draw BAR's handle in the proper position.
   
   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll
   bar handle, we want to let them drag it down all the way, so that
   the bar's top is as far down as it goes; otherwise, there's no way
   to move to the very end of the buffer.  */

static void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  ControlHandle ch = SCROLL_BAR_CONTROL_HANDLE (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
  int length = end - start;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  /* Make sure the values are reasonable, and try to preserve the
     distance between start and end.  */
  if (start < 0)
    start = 0;
  else if (start > top_range)
    start = top_range;
  end = start + length;
  
  if (end < start)
    end = start;
  else if (end > top_range && ! dragging)
    end = top_range;

  /* Store the adjusted setting in the scroll bar.  */
  XSETINT (bar->start, start);
  XSETINT (bar->end, end);

  /* Clip the end position, just for display.  */
  if (end > top_range)
    end = top_range;

  /* Draw bottom positions VERTICAL_SCROLL_BAR_MIN_HANDLE pixels below
     top positions, to make sure the handle is always at least that
     many pixels tall.  */
  end += VERTICAL_SCROLL_BAR_MIN_HANDLE;

  SetControlMinimum (ch, 0);
  /* Don't inadvertently activate deactivated scroll bars */
  if (GetControlMaximum (ch) != -1)
    SetControlMaximum (ch, top_range + VERTICAL_SCROLL_BAR_MIN_HANDLE
		       - (end - start));
  SetControlValue (ch, start);
#if TARGET_API_MAC_CARBON
  SetControlViewSize (ch, end - start);
#endif

  UNBLOCK_INPUT;
}


/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil.  */

static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the Mac scroll bar control  */
  DisposeControl (SCROLL_BAR_CONTROL_HANDLE (bar));

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
XTset_vertical_scroll_bar (w, portion, whole, position)
     struct window *w;
     int portion, whole, position;
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar;
  int top, height, left, sb_left, width, sb_width, disp_top, disp_height;
  int window_x, window_y, window_width, window_height;

  /* Get window dimensions.  */
  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);
  top = window_y;
#ifdef MAC_OSX
  width = 16;
#else
  width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
#endif
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
  
  /* Adjustments according to Inside Macintosh to make it look nice */
  disp_top = top;
  disp_height = height;
  if (disp_top == 0)
    {
      disp_top = -1;
      disp_height++;
    }
  else if (disp_top == PIXEL_HEIGHT (f) - 16)
    {
      disp_top++;
      disp_height--;
    }
    
  if (sb_left + sb_width == PIXEL_WIDTH (f))
    sb_left++;
  
  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      BLOCK_INPUT;
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		  left, top, width, height, 0);
      UNBLOCK_INPUT;
      bar = x_scroll_bar_create (w, top, sb_left, sb_width, height, disp_top,
				 disp_height);
      XSETVECTOR (w->vertical_scroll_bar, bar);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      ControlHandle ch;
            
      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      ch = SCROLL_BAR_CONTROL_HANDLE (bar);

      BLOCK_INPUT;

      /* If already correctly positioned, do nothing.  */
      if (XINT (bar->left) == sb_left
          && XINT (bar->top) == top
          && XINT (bar->width) == sb_width
          && XINT (bar->height) == height)
        Draw1Control (ch);
      else
        {
	  /* Clear areas not covered by the scroll bar because it's not as
	     wide as the area reserved for it .  This makes sure a
	     previous mode line display is cleared after C-x 2 C-x 1, for
	     example.  */
	  int area_width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
	  XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		      left, top, area_width, height, 0);

#if 0
          if (sb_left + sb_width >= PIXEL_WIDTH (f))
            XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		        sb_left - 1, top, 1, height, 0);
#endif

          HideControl (ch);
          MoveControl (ch, sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM, disp_top);
          SizeControl (ch, sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
		       disp_height);
          ShowControl (ch);
      
          /* Remember new settings.  */
          XSETINT (bar->left, sb_left);
          XSETINT (bar->top, top);
          XSETINT (bar->width, sb_width);
          XSETINT (bar->height, height);
        }

      UNBLOCK_INPUT;
    }

  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (NILP (bar->dragging))
    {
      int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, top_range, 0);
      else
	{
	  int start = ((double) position * top_range) / whole;
	  int end = ((double) (position + portion) * top_range) / whole;
	  x_scroll_bar_set_handle (bar, start, end, 0);
	}
    }
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
XTcondemn_scroll_bars (frame)
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
XTredeem_scroll_bar (window)
     struct window *window;
{
  struct scroll_bar *bar;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  {
    FRAME_PTR f = XFRAME (WINDOW_FRAME (window));

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
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */

static void
XTjudge_scroll_bars (f)
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


static void
activate_scroll_bars (frame)
     FRAME_PTR frame;
{
  Lisp_Object bar;
  ControlHandle ch;
  
  bar = FRAME_SCROLL_BARS (frame);
  while (! NILP (bar))
    {
      ch = SCROLL_BAR_CONTROL_HANDLE (XSCROLL_BAR (bar));
#ifdef TARGET_API_MAC_CARBON
      ActivateControl (ch);
#else
      SetControlMaximum (ch,
			 VERTICAL_SCROLL_BAR_TOP_RANGE (frame,
							XINT (XSCROLL_BAR (bar)
							      ->height)) - 1);
#endif
      bar = XSCROLL_BAR (bar)->next;
    }
}


static void
deactivate_scroll_bars (frame)
     FRAME_PTR frame;
{
  Lisp_Object bar;
  ControlHandle ch;
  
  bar = FRAME_SCROLL_BARS (frame);
  while (! NILP (bar))
    {
      ch = SCROLL_BAR_CONTROL_HANDLE (XSCROLL_BAR (bar));
#ifdef TARGET_API_MAC_CARBON
      DeactivateControl (ch);
#else
      SetControlMaximum (ch, XINT (-1));
#endif      
      bar = XSCROLL_BAR (bar)->next;
    }
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than NO_EVENT, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_handle_click (bar, part_code, er, bufp)
     struct scroll_bar *bar;
     int part_code;
     EventRecord *er;
     struct input_event *bufp;
{
  if (! GC_WINDOWP (bar->window))
    abort ();

  bufp->kind = SCROLL_BAR_CLICK_EVENT;
  bufp->frame_or_window = bar->window;
  bufp->arg = Qnil;

  bar->dragging = Qnil;
 
  switch (part_code)
    {
    case kControlUpButtonPart:
      bufp->part = scroll_bar_up_arrow;
      break;
    case kControlDownButtonPart:
      bufp->part = scroll_bar_down_arrow;
      break;
    case kControlPageUpPart:
      bufp->part = scroll_bar_above_handle;
      break;
    case kControlPageDownPart:
      bufp->part = scroll_bar_below_handle;
      break;
#ifdef TARGET_API_MAC_CARBON
    default:
#else
    case kControlIndicatorPart:
#endif
      if (er->what == mouseDown)
        bar->dragging = make_number (0);
      XSETVECTOR (last_mouse_scroll_bar, bar);
      bufp->part = scroll_bar_handle;
      break;
    }
}


/* Handle some mouse motion while someone is dragging the scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_note_movement (bar, y_pos, t)
     struct scroll_bar *bar;
     int y_pos;
     Time t;
{
  FRAME_PTR f = XFRAME (XWINDOW (bar->window)->frame);

  last_mouse_movement_time = t;

  f->mouse_moved = 1;
  XSETVECTOR (last_mouse_scroll_bar, bar);

  /* If we're dragging the bar, display it.  */
  if (! GC_NILP (bar->dragging))
    {
      /* Where should the handle be now?  */
      int new_start = y_pos - 24;

      if (new_start != XINT (bar->start))
	{
	  int new_end = new_start + (XINT (bar->end) - XINT (bar->start));

	  x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	}
    }
}


/* Return information to the user about the current position of the
   mouse on the scroll bar.  */

static void
x_scroll_bar_report_motion (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  WindowPtr wp = FrontWindow ();
  Point mouse_pos;
  struct frame *f = ((mac_output *) GetWRefCon (wp))->mFP;
  int win_y, top_range;

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (wp));
#else
  SetPort (wp);
#endif

  GetMouse (&mouse_pos);

  win_y = mouse_pos.v - XINT (bar->top);
  top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

  win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

  win_y -= 24;

  if (! NILP (bar->dragging))
    win_y -= XINT (bar->dragging);

  if (win_y < 0)
    win_y = 0;
  if (win_y > top_range)
    win_y = top_range;

  *fp = f;
  *bar_window = bar->window;

  if (! NILP (bar->dragging))
    *part = scroll_bar_handle;
  else if (win_y < XINT (bar->start))
    *part = scroll_bar_above_handle;
  else if (win_y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
    *part = scroll_bar_handle;
  else
    *part = scroll_bar_below_handle;

  XSETINT (*x, win_y);
  XSETINT (*y, top_range);

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;
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
x_clip_to_row (w, row, gc, whole_line_p)
     struct window *w;
     struct glyph_row *row;
     GC gc;
     int whole_line_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Rect clip_rect;
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

  mac_set_clip_rectangle (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f), &clip_rect);
}


/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  Display *dpy = FRAME_MAC_DISPLAY (f);
  int x, y, wd, h;
  XGCValues xgcv;
  struct glyph *cursor_glyph;
  GC gc;

  /* Compute frame-relative coordinates from window-relative
     coordinates.  */
  x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
  y = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
       + row->ascent - w->phys_cursor_ascent);
  h = row->height - 1;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute the width of the rectangle to draw.  If on a stretch
     glyph, and `x-stretch-block-cursor' is nil, don't draw a
     rectangle as wide as the glyph, but use a canonical character
     width instead.  */
  wd = cursor_glyph->pixel_width - 1;
  if (cursor_glyph->type == STRETCH_GLYPH
      && !x_stretch_cursor_p)
    wd = min (CANON_X_UNIT (f), wd);
  
  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  xgcv.foreground = f->output_data.mac->cursor_pixel;
  if (dpyinfo->scratch_cursor_gc)
    XChangeGC (dpy, dpyinfo->scratch_cursor_gc, GCForeground, &xgcv);
  else
    dpyinfo->scratch_cursor_gc = XCreateGC (dpy, FRAME_MAC_WINDOW (f),
					    GCForeground, &xgcv);
  gc = dpyinfo->scratch_cursor_gc;

  /* Set clipping, draw the rectangle, and reset clipping again.  */
  x_clip_to_row (w, row, gc, 0);
  mac_draw_rectangle (dpy, FRAME_MAC_WINDOW (f), gc, x, y, wd, h);
  mac_reset_clipping (dpy, FRAME_MAC_WINDOW (f));
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
  /* If cursor hpos is out of bounds, don't draw garbage.  This can
     happen in mini-buffer windows when switching between echo area
     glyphs and mini-buffer.  */
  if (w->phys_cursor.hpos < row->used[TEXT_AREA])
    {
      struct frame *f = XFRAME (w->frame);
      struct glyph *cursor_glyph;
      GC gc;
      int x;
      unsigned long mask;
      XGCValues xgcv;
      Display *dpy;
      Window window;
      
      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	return;

      xgcv.background = f->output_data.mac->cursor_pixel;
      xgcv.foreground = f->output_data.mac->cursor_pixel;
      mask = GCForeground | GCBackground;
      dpy = FRAME_MAC_DISPLAY (f);
      window = FRAME_MAC_WINDOW (f);
      gc = FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc;
      
      if (gc)
	XChangeGC (dpy, gc, mask, &xgcv);
      else
	{
	  gc = XCreateGC (dpy, window, mask, &xgcv);
	  FRAME_MAC_DISPLAY_INFO (f)->scratch_cursor_gc = gc;
	}

      if (width < 0)
	width = f->output_data.mac->cursor_width;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
      x_clip_to_row (w, row, gc, 0);
      XFillRectangle (dpy, window, gc,
		      x,
		      WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
		      min (cursor_glyph->pixel_width, width),
		      row->height);
      mac_reset_clipping (dpy, FRAME_MAC_WINDOW (f));
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
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
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

      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	goto mark_cursor_off;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x),
      
      XClearArea (FRAME_MAC_DISPLAY (f), FRAME_MAC_WINDOW (f),
		  x,
		  WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
						   cursor_row->y)),
		  cursor_glyph->pixel_width,
		  cursor_row->visible_height,
		  0);
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
  struct mac_display_info *dpyinfo
    = FRAME_MAC_DISPLAY_INFO (XFRAME (w->frame));
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
      if (f != FRAME_MAC_DISPLAY_INFO (f)->x_highlight_frame
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
      w->phys_cursor_type = new_cursor_type;
      w->phys_cursor_width = new_cursor_width;
      w->phys_cursor_on_p = 1;

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

#if 0 /* MAC_TODO: no icon support yet.  */
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
#endif /* MAC_TODO */

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
  if (FRAME_MAC_WINDOW (f) != 0)
    {
      frame_update_line_height (f);
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
        x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->output_data.mac->line_height = FONT_HEIGHT (FRAME_FONT (f));

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
  int o_left = f->output_data.mac->left_fringe_width;
  int o_right = f->output_data.mac->right_fringe_width;
  int o_cols = f->output_data.mac->fringe_cols;

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
      int font_wid = FONT_WIDTH (f->output_data.mac->font);
      int cols = (left_wid + right_wid + font_wid-1) / font_wid;
      int real_wid = cols * font_wid;
      if (left_wid && right_wid)
	{
	  if (left_fringe_width < 0)
	    {
	      /* Left fringe width is fixed, adjust right fringe if necessary */
	      f->output_data.mac->left_fringe_width = left_wid;
	      f->output_data.mac->right_fringe_width = real_wid - left_wid;
	    }
	  else if (right_fringe_width < 0)
	    {
	      /* Right fringe width is fixed, adjust left fringe if necessary */
	      f->output_data.mac->left_fringe_width = real_wid - right_wid;
	      f->output_data.mac->right_fringe_width = right_wid;
	    }
	  else
	    {
	      /* Adjust both fringes with an equal amount.
		 Note that we are doing integer arithmetic here, so don't
		 lose a pixel if the total width is an odd number.  */
	      int fill = real_wid - conf_wid;
	      f->output_data.mac->left_fringe_width = left_wid + fill/2;
	      f->output_data.mac->right_fringe_width = right_wid + fill - fill/2;
	    }
	}
      else if (left_fringe_width)
	{
	  f->output_data.mac->left_fringe_width = real_wid;
	  f->output_data.mac->right_fringe_width = 0;
	}
      else
	{
	  f->output_data.mac->left_fringe_width = 0;
	  f->output_data.mac->right_fringe_width = real_wid;
	}
      f->output_data.mac->fringe_cols = cols;
      f->output_data.mac->fringes_extra = real_wid;
    }
  else
    {
      f->output_data.mac->left_fringe_width = 0;
      f->output_data.mac->right_fringe_width = 0;
      f->output_data.mac->fringe_cols = 0;
      f->output_data.mac->fringes_extra = 0;
    }

  if (redraw && FRAME_VISIBLE_P (f))
    if (o_left != f->output_data.mac->left_fringe_width ||
	o_right != f->output_data.mac->right_fringe_width ||
	o_cols != f->output_data.mac->fringe_cols)
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
  Point pt;
  int flags = f->output_data.mac->size_hint_flags;

  pt.h = pt.v = 0;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.  */
  if (f->output_data.mac->parent_desc != FRAME_MAC_DISPLAY_INFO (f)->root_window)
    {
      GrafPtr savePort;
      GetPort (&savePort);

#if TARGET_API_MAC_CARBON
      SetPort (GetWindowPort (FRAME_MAC_WINDOW (f)));
#else
      SetPort (FRAME_MAC_WINDOW (f));
#endif

#if TARGET_API_MAC_CARBON
      {
        Rect r;
    
        GetWindowPortBounds (FRAME_MAC_WINDOW (f), &r);
        SetPt(&pt, r.left,  r.top);
      }
#else /* not TARGET_API_MAC_CARBON */
      SetPt(&pt, FRAME_MAC_WINDOW (f)->portRect.left,  FRAME_MAC_WINDOW (f)->portRect.top);
#endif /* not TARGET_API_MAC_CARBON */
      LocalToGlobal (&pt);
      SetPort (savePort);
    }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.mac->left_pos = (FRAME_MAC_DISPLAY_INFO (f)->width
			      - 2 * f->output_data.mac->border_width - pt.h
			      - PIXEL_WIDTH (f)
			      + f->output_data.mac->left_pos);
  /* NTEMACS_TODO: Subtract menubar height?  */
  if (flags & YNegative)
    f->output_data.mac->top_pos = (FRAME_MAC_DISPLAY_INFO (f)->height
			     - 2 * f->output_data.mac->border_width - pt.v
			     - PIXEL_HEIGHT (f)
			     + f->output_data.mac->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.mac->size_hint_flags &= ~ (XNegative | YNegative);
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
      f->output_data.mac->top_pos = yoff;
      f->output_data.mac->left_pos = xoff;
      f->output_data.mac->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.mac->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.mac->size_hint_flags |= YNegative;
      f->output_data.mac->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->output_data.mac->left_pos;
  modified_top = f->output_data.mac->top_pos;

  MoveWindow (f->output_data.mac->mWP, modified_left + 6,
	      modified_top + 42, false);

  UNBLOCK_INPUT;
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
  f->output_data.mac->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.mac->font)));

  x_compute_fringe_widths (f, 0);

  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);

  f->output_data.mac->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);

  SizeWindow (FRAME_MAC_WINDOW (f), pixelwidth, pixelheight, 0);

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

  XFlush (FRAME_X_DISPLAY (f));

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

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->output_data.mac->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.mac->line_height / 2;

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
#if 0 /* MAC_TODO: CursorDeviceMoveTo is non-Carbon */
  BLOCK_INPUT;

  XWarpPointer (FRAME_X_DISPLAY (f), None, FRAME_X_WINDOW (f),
		0, 0, 0, 0, pix_x, pix_y);
  UNBLOCK_INPUT;
#endif
}


/* focus shifting, raising and lowering.  */

void
x_focus_on_frame (f)
     struct frame *f;
{
#if 0  /* This proves to be unpleasant.  */
  x_raise_frame (f);
#endif
#if 0
  /* I don't think that the ICCCM allows programs to do things like this
     without the interaction of the window manager.  Whatever you end up
     doing with this code, do it to x_unfocus_frame too.  */
  XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
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
  if (f->async_visible)
    SelectWindow (FRAME_MAC_WINDOW (f));
}

/* Lower frame F.  */
void
x_lower_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    SendBehind (FRAME_MAC_WINDOW (f), nil);
}

static void
XTframe_raise_lower (f, raise_flag)
     FRAME_PTR f;
     int raise_flag;
{
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
  int original_top, original_left;

  BLOCK_INPUT;

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! f->output_data.mac->asked_for_visible)
	x_set_offset (f, f->output_data.mac->left_pos,
		      f->output_data.mac->top_pos, 0);

      f->output_data.mac->asked_for_visible = 1;
      
      ShowWindow (FRAME_MAC_WINDOW (f));
    }

  XFlush (FRAME_MAC_DISPLAY (f));

#if 0 /* MAC_TODO */
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
	x_sync (f);

	/* Machines that do polling rather than SIGIO have been
	   observed to go into a busy-wait here.  So we'll fake an
	   alarm signal to let the handler know that there's something
	   to be read.  We used to raise a real alarm, but it seems
	   that the handler isn't always enabled here.  This is
	   probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while
	       processing the fake one.  Turn it off and let the
	       handler reset it.  */
	    extern void poll_for_input_1 P_ ((void));
	    int old_poll_suppress_count = poll_suppress_count;
	    poll_suppress_count = 1;
	    poll_for_input_1 ();
	    poll_suppress_count = old_poll_suppress_count;
	  }

	/* See if a MapNotify event has been processed.  */
	FRAME_SAMPLE_VISIBILITY (f);
      }
  }
#endif /* MAC_TODO */
}

/* Change from mapped state to withdrawn state.  */

/* Make the frame visible (mapped and not iconified).  */

void
x_make_frame_invisible (f)
     struct frame *f;
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_MAC_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_MAC_DISPLAY_INFO (f)->x_highlight_frame = 0;
  
  BLOCK_INPUT;
  
  HideWindow (FRAME_MAC_WINDOW (f));
  
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

/* Change window state from mapped to iconified.  */

void
x_iconify_frame (f)
     struct frame *f;
{
#if 0 /* MAC_TODO: really no iconify on Mac */
  int result;
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_X_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_X_DISPLAY_INFO (f)->x_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  FRAME_SAMPLE_VISIBILITY (f);

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

#ifdef USE_X_TOOLKIT

  if (! FRAME_VISIBLE_P (f))
    {
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, IconicState);
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->output_data.x->widget);
      /* The server won't give us any event to indicate
	 that an invisible frame was changed to an icon,
	 so we have to record it here.  */
      f->iconified = 1;
      f->visible = 1;
      f->async_iconified = 1;
      f->async_visible = 0;
      UNBLOCK_INPUT;
      return;
    }

  result = XIconifyWindow (FRAME_X_DISPLAY (f),
			   XtWindow (f->output_data.x->widget),
			   DefaultScreen (FRAME_X_DISPLAY (f)));
  UNBLOCK_INPUT;

  if (!result)
    error ("Can't notify window manager of iconification");

  f->async_iconified = 1;
  f->async_visible = 0;


  BLOCK_INPUT;
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#else /* not USE_X_TOOLKIT */

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f) && !FRAME_ICONIFIED_P (f))
    x_set_offset (f, f->output_data.x->left_pos, f->output_data.x->top_pos, 0);

  /* Since we don't know which revision of X we're running, we'll use both
     the X11R3 and X11R4 techniques.  I don't know if this is a good idea.  */

  /* X11R4: send a ClientMessage to the window manager using the
     WM_CHANGE_STATE type.  */
  {
    XEvent message;

    message.xclient.window = FRAME_X_WINDOW (f);
    message.xclient.type = ClientMessage;
    message.xclient.message_type = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_change_state;
    message.xclient.format = 32;
    message.xclient.data.l[0] = IconicState;

    if (! XSendEvent (FRAME_X_DISPLAY (f),
		      DefaultRootWindow (FRAME_X_DISPLAY (f)),
		      False,
		      SubstructureRedirectMask | SubstructureNotifyMask,
		      &message))
      {
	UNBLOCK_INPUT_RESIGNAL;
	error ("Can't notify window manager of iconification");
      }
  }

  /* X11R3: set the initial_state field of the window manager hints to
     IconicState.  */
  x_wm_set_window_state (f, IconicState);

  if (!FRAME_VISIBLE_P (f))
    {
      /* If the frame was withdrawn, before, we must map it.  */
      XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
    }

  f->async_iconified = 1;
  f->async_visible = 0;

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#endif /* not USE_X_TOOLKIT */
#endif /* MAC_TODO */
}


/* Destroy the X window of frame F.  */

void
x_destroy_window (f)
     struct frame *f;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  BLOCK_INPUT;

  DisposeWindow (FRAME_MAC_WINDOW (f));

  free_frame_menubar (f);
  free_frame_faces (f);

  xfree (f->output_data.mac);
  f->output_data.mac = 0;
  if (f == dpyinfo->x_focus_frame)
    dpyinfo->x_focus_frame = 0;
  if (f == dpyinfo->x_focus_event_frame)
    dpyinfo->x_focus_event_frame = 0;
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;

  dpyinfo->reference_count--;

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
#if 0 /* MAC_TODO: connect this to the Appearance Manager */
  XSizeHints size_hints;

#ifdef USE_X_TOOLKIT
  Arg al[2];
  int ac = 0;
  Dimension widget_width, widget_height;
  Window window = XtWindow (f->output_data.x->widget);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);
#endif /* not USE_X_TOOLKIT */

  /* Setting PMaxSize caused various problems.  */
  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  size_hints.x = f->output_data.x->left_pos;
  size_hints.y = f->output_data.x->top_pos;

#ifdef USE_X_TOOLKIT
  XtSetArg (al[ac], XtNwidth, &widget_width); ac++;
  XtSetArg (al[ac], XtNheight, &widget_height); ac++;
  XtGetValues (f->output_data.x->widget, al, ac);
  size_hints.height = widget_height;
  size_hints.width = widget_width;
#else /* not USE_X_TOOLKIT */
  size_hints.height = PIXEL_HEIGHT (f);
  size_hints.width = PIXEL_WIDTH (f);
#endif /* not USE_X_TOOLKIT */

  size_hints.width_inc = FONT_WIDTH (f->output_data.x->font);
  size_hints.height_inc = f->output_data.x->line_height;
  size_hints.max_width
    = FRAME_X_DISPLAY_INFO (f)->width - CHAR_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height
    = FRAME_X_DISPLAY_INFO (f)->height - CHAR_TO_PIXEL_HEIGHT (f, 0);

  /* Calculate the base and minimum sizes.

     (When we use the X toolkit, we don't do it here.
     Instead we copy the values that the widgets are using, below.)  */
#ifndef USE_X_TOOLKIT
  {
    int base_width, base_height;
    int min_rows = 0, min_cols = 0;

    base_width = CHAR_TO_PIXEL_WIDTH (f, 0);
    base_height = CHAR_TO_PIXEL_HEIGHT (f, 0);

    check_frame_size (f, &min_rows, &min_cols);

    /* The window manager uses the base width hints to calculate the
       current number of rows and columns in the frame while
       resizing; min_width and min_height aren't useful for this
       purpose, since they might not give the dimensions for a
       zero-row, zero-column frame.

       We use the base_width and base_height members if we have
       them; otherwise, we set the min_width and min_height members
       to the size for a zero x zero frame.  */

#ifdef HAVE_X11R4
    size_hints.flags |= PBaseSize;
    size_hints.base_width = base_width;
    size_hints.base_height = base_height;
    size_hints.min_width  = base_width + min_cols * size_hints.width_inc;
    size_hints.min_height = base_height + min_rows * size_hints.height_inc;
#else
    size_hints.min_width = base_width;
    size_hints.min_height = base_height;
#endif
  }

  /* If we don't need the old flags, we don't need the old hint at all.  */
  if (flags)
    {
      size_hints.flags |= flags;
      goto no_read;
    }
#endif /* not USE_X_TOOLKIT */

  {
    XSizeHints hints;		/* Sometimes I hate X Windows... */
    long supplied_return;
    int value;

#ifdef HAVE_X11R4
    value = XGetWMNormalHints (FRAME_X_DISPLAY (f), window, &hints,
			       &supplied_return);
#else
    value = XGetNormalHints (FRAME_X_DISPLAY (f), window, &hints);
#endif

#ifdef USE_X_TOOLKIT
    size_hints.base_height = hints.base_height;
    size_hints.base_width = hints.base_width;
    size_hints.min_height = hints.min_height;
    size_hints.min_width = hints.min_width;
#endif

    if (flags)
      size_hints.flags |= flags;
    else
      {
	if (value == 0)
	  hints.flags = 0;
	if (hints.flags & PSize)
	  size_hints.flags |= PSize;
	if (hints.flags & PPosition)
	  size_hints.flags |= PPosition;
	if (hints.flags & USPosition)
	  size_hints.flags |= USPosition;
	if (hints.flags & USSize)
	  size_hints.flags |= USSize;
      }
  }

#ifndef USE_X_TOOLKIT
 no_read:
#endif

#ifdef PWinGravity
  size_hints.win_gravity = f->output_data.x->win_gravity;
  size_hints.flags |= PWinGravity;

  if (user_position)
    {
      size_hints.flags &= ~ PPosition;
      size_hints.flags |= USPosition;
    }
#endif /* PWinGravity */

#ifdef HAVE_X11R4
  XSetWMNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
#else
  XSetNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
#endif
#endif /* MAC_TODO */
}

#if 0 /* MAC_TODO: hide application instead of iconify? */
/* Used for IconicState or NormalState */

void
x_wm_set_window_state (f, state)
     struct frame *f;
     int state;
{
#ifdef USE_X_TOOLKIT
  Arg al[1];

  XtSetArg (al[0], XtNinitialState, state);
  XtSetValues (f->output_data.x->widget, al, 1);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);

  f->output_data.x->wm_hints.flags |= StateHint;
  f->output_data.x->wm_hints.initial_state = state;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
#endif /* not USE_X_TOOLKIT */
}

void
x_wm_set_icon_pixmap (f, pixmap_id)
     struct frame *f;
     int pixmap_id;
{
  Pixmap icon_pixmap;

#ifndef USE_X_TOOLKIT
  Window window = FRAME_X_WINDOW (f);
#endif

  if (pixmap_id > 0)
    {
      icon_pixmap = x_bitmap_pixmap (f, pixmap_id);
      f->output_data.x->wm_hints.icon_pixmap = icon_pixmap;
    }
  else
    {
      /* It seems there is no way to turn off use of an icon pixmap.
	 The following line does it, only if no icon has yet been created,
	 for some window managers.  But with mwm it crashes.
	 Some people say it should clear the IconPixmapHint bit in this case,
	 but that doesn't work, and the X consortium said it isn't the
	 right thing at all.  Since there is no way to win,
	 best to explicitly give up.  */
#if 0
      f->output_data.x->wm_hints.icon_pixmap = None;
#else
      return;
#endif
    }

#ifdef USE_X_TOOLKIT /* same as in x_wm_set_window_state.  */

  {
    Arg al[1];
    XtSetArg (al[0], XtNiconPixmap, icon_pixmap);
    XtSetValues (f->output_data.x->widget, al, 1);
  }

#else /* not USE_X_TOOLKIT */
  
  f->output_data.x->wm_hints.flags |= IconPixmapHint;
  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);

#endif /* not USE_X_TOOLKIT */
}

#endif /* MAC_TODO */

void
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#if 0 /* MAC_TODO: no icons on Mac */
#ifdef USE_X_TOOLKIT
  Window window = XtWindow (f->output_data.x->widget);
#else
  Window window = FRAME_X_WINDOW (f);
#endif

  f->output_data.x->wm_hints.flags |= IconPositionHint;
  f->output_data.x->wm_hints.icon_x = icon_x;
  f->output_data.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
#endif /* MAC_TODO */
}


/***********************************************************************
				Fonts
 ***********************************************************************/

/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */

struct font_info *
x_get_font_info (f, font_idx)
     FRAME_PTR f;
     int font_idx;
{
  return (FRAME_MAC_FONT_TABLE (f) + font_idx);
}

/* the global font name table */
char **font_name_table = NULL;
int font_name_table_size = 0;
int font_name_count = 0;

/* compare two strings ignoring case */
static int
stricmp (const char *s, const char *t)
{
  for ( ; tolower (*s) == tolower (*t); s++, t++)
    if (*s == '\0')
      return 0;
  return tolower (*s) - tolower (*t);
}

/* compare two strings ignoring case and handling wildcard */
static int
wildstrieq (char *s1, char *s2)
{
  if (strcmp (s1, "*") == 0 || strcmp (s2, "*") == 0)
    return true;

  return stricmp (s1, s2) == 0;
}

/* Assume parameter 1 is fully qualified, no wildcards. */
static int 
mac_font_pattern_match (fontname, pattern)
    char * fontname;
    char * pattern;
{
  char *regex = (char *) alloca (strlen (pattern) * 2 + 3);
  char *font_name_copy = (char *) alloca (strlen (fontname) + 1);
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

  return (fast_c_string_match_ignore_case (build_string (regex),
                                           font_name_copy) >= 0);
}

/* Two font specs are considered to match if their foundry, family,
   weight, slant, and charset match.  */
static int 
mac_font_match (char *mf, char *xf)
{
  char m_foundry[50], m_family[50], m_weight[20], m_slant[2], m_charset[20];
  char x_foundry[50], x_family[50], x_weight[20], x_slant[2], x_charset[20];

  if (sscanf (mf, "-%49[^-]-%49[^-]-%19[^-]-%1[^-]-%*[^-]--%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%19s",
              m_foundry, m_family, m_weight, m_slant, m_charset) != 5)
    return mac_font_pattern_match (mf, xf);

  if (sscanf (xf, "-%49[^-]-%49[^-]-%19[^-]-%1[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%19s",
              x_foundry, x_family, x_weight, x_slant, x_charset) != 5)
    return mac_font_pattern_match (mf, xf);

  return (wildstrieq (m_foundry, x_foundry)
          && wildstrieq (m_family, x_family)
          && wildstrieq (m_weight, x_weight)
          && wildstrieq (m_slant, x_slant)
          && wildstrieq (m_charset, x_charset))
         || mac_font_pattern_match (mf, xf);
}


static char *
mac_to_x_fontname (char *name, int size, Style style, short scriptcode)
{
  char foundry[32], family[32], cs[32];
  char xf[255], *result, *p;

  if (sscanf (name, "%31[^-]-%31[^-]-%31s", foundry, family, cs) != 3)
    {
      strcpy(foundry, "Apple");
      strcpy(family, name);

      switch (scriptcode)
      {
      case smTradChinese:
        strcpy(cs, "big5-0");
        break;
      case smSimpChinese:
        strcpy(cs, "gb2312.1980-0");
        break;
      case smJapanese:
        strcpy(cs, "jisx0208.1983-sjis");
        break;
      case -smJapanese:
	/* Each Apple Japanese font is entered into the font table
	   twice: once as a jisx0208.1983-sjis font and once as a
	   jisx0201.1976-0 font.  The latter can be used to display
	   the ascii charset and katakana-jisx0201 charset.  A
	   negative script code signals that the name of this latter
	   font is being built.  */
	strcpy(cs, "jisx0201.1976-0");
	break;
      case smKorean:
        strcpy(cs, "ksc5601.1989-0");
        break;        
      default:
        strcpy(cs, "mac-roman");
        break;
      }
    }

  sprintf(xf, "-%s-%s-%s-%c-normal--%d-%d-75-75-m-%d-%s",
          foundry, family, style & bold ? "bold" : "medium",
	  style & italic ? 'i' : 'r', size, size * 10, size * 10, cs);
  
  result = (char *) xmalloc (strlen (xf) + 1);
  strcpy (result, xf);
  for (p = result; *p; p++)
    *p = tolower(*p);
  return result;
}
                                                                        

/* Convert an X font spec to the corresponding mac font name, which
   can then be passed to GetFNum after conversion to a Pascal string.
   For ordinary Mac fonts, this should just be their names, like
   "monaco", "Taipei", etc.  Fonts converted from the GNU intlfonts
   collection contain their charset designation in their names, like
   "ETL-Fixed-iso8859-1", "ETL-Fixed-koi8-r", etc.  Both types of font
   names are handled accordingly.  */
static void
x_font_name_to_mac_font_name (char *xf, char *mf)
{
  char foundry[32], family[32], weight[20], slant[2], cs[32];

  strcpy (mf, "");

  if (sscanf (xf, "-%31[^-]-%31[^-]-%19[^-]-%1[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%31s",
              foundry, family, weight, slant, cs) != 5 &&
      sscanf (xf, "-%31[^-]-%31[^-]-%19[^-]-%1[^-]-%*[^-]--%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%31s",
              foundry, family, weight, slant, cs) != 5)
    return;

  if (strcmp (cs, "big5-0") == 0 || strcmp (cs, "gb2312.1980-0") == 0
      || strcmp (cs, "jisx0208.1983-sjis") == 0
      || strcmp (cs, "jisx0201.1976-0") == 0
      || strcmp (cs, "ksc5601.1989-0") == 0 || strcmp (cs, "mac-roman") == 0)
    strcpy(mf, family);
  else
    sprintf(mf, "%s-%s-%s", foundry, family, cs);
}


/* Sets up the table font_name_table to contain the list of all
   monospace fonts in the system the first time the table is used so
   that the Resource Manager need not be accessed every time this
   information is needed.  */

static void
init_font_name_table ()
{
#if TARGET_API_MAC_CARBON
  SInt32 sv;
  
  if (Gestalt (gestaltSystemVersion, &sv) == noErr && sv >= 0x1000)
    {
      FMFontFamilyIterator ffi;
      FMFontFamilyInstanceIterator ffii;
      FMFontFamily ff;

      /* Create a dummy instance iterator here to avoid creating and
	 destroying it in the loop.  */
      if (FMCreateFontFamilyInstanceIterator (0, &ffii) != noErr)
	return;
      /* Create an iterator to enumerate the font families.  */
      if (FMCreateFontFamilyIterator (NULL, NULL, kFMDefaultOptions, &ffi)
	  != noErr)
	{
	  FMDisposeFontFamilyInstanceIterator (&ffii);
	  return;
	}

      while (FMGetNextFontFamily (&ffi, &ff) == noErr)
	{
	  Str255 name;
	  FMFont font;
	  FMFontStyle style;
	  FMFontSize size;
	  SInt16 sc;
      
	  if (FMGetFontFamilyName (ff, name) != noErr)
	    break;
	  p2cstr (name);
      
	  sc = FontToScript (ff);
	  
	  /* Point the instance iterator at the current font family.  */
	  if (FMResetFontFamilyInstanceIterator(ff, &ffii) != noErr)
	    break;
      
	  while (FMGetNextFontFamilyInstance (&ffii, &font, &style, &size)
		 == noErr)
	    {
	      if (font_name_table_size == 0)
		{
		  font_name_table_size = 16;
		  font_name_table = (char **)
		    xmalloc (font_name_table_size * sizeof (char *));
		}
	      else if (font_name_count + 1 >= font_name_table_size)
		{
		  font_name_table_size += 16;
		  font_name_table = (char **)
		    xrealloc (font_name_table,
			      font_name_table_size * sizeof (char *));
		}
	      font_name_table[font_name_count++]
		= mac_to_x_fontname (name, size, style, sc);
	    }
	}
  
      /* Dispose of the iterators.  */
      FMDisposeFontFamilyIterator (&ffi);
      FMDisposeFontFamilyInstanceIterator (&ffii);
    }
  else
    {
#endif  /* TARGET_API_MAC_CARBON */
      GrafPtr port;
      SInt16 fontnum, old_fontnum;
      int num_mac_fonts = CountResources('FOND');
      int i, j;
      Handle font_handle, font_handle_2;
      short id, scriptcode;
      ResType type;
      Str32 name;
      struct FontAssoc *fat;
      struct AsscEntry *assc_entry;
      
      GetPort (&port);  /* save the current font number used */
#if TARGET_API_MAC_CARBON
      old_fontnum = GetPortTextFont (port);
#else
      old_fontnum = port->txFont;
#endif

      for (i = 1; i <= num_mac_fonts; i++)  /* get all available fonts */
	{
	  font_handle = GetIndResource ('FOND', i);
	  if (!font_handle)
	    continue;
	  
	  GetResInfo (font_handle, &id, &type, name);
	  GetFNum (name, &fontnum);
	  p2cstr (name);
	  if (fontnum == 0)
	    continue;
	  
	  TextFont (fontnum);
	  scriptcode = FontToScript (fontnum);
	  do
	    {
	      HLock (font_handle);
	      
	      if (GetResourceSizeOnDisk (font_handle)
		  >= sizeof (struct FamRec))
		{
		  fat = (struct FontAssoc *) (*font_handle
					      + sizeof (struct FamRec));
		  assc_entry
		    = (struct AsscEntry *) (*font_handle
					    + sizeof (struct FamRec)
					    + sizeof (struct FontAssoc));
		  
		  for (j = 0; j <= fat->numAssoc; j++, assc_entry++)
		    {
		      if (font_name_table_size == 0)
			{
			  font_name_table_size = 16;
			  font_name_table = (char **)
			    xmalloc (font_name_table_size * sizeof (char *));
			}
		      else if (font_name_count >= font_name_table_size)
			{
			  font_name_table_size += 16;
			  font_name_table = (char **)
			    xrealloc (font_name_table,
				      font_name_table_size * sizeof (char *));
			}
		      font_name_table[font_name_count++]
			= mac_to_x_fontname (name,
					     assc_entry->fontSize,
					     assc_entry->fontStyle,
					     scriptcode);
		      /* Both jisx0208.1983-sjis and
			 jisx0201.1976-sjis parts are contained in
			 Apple Japanese (SJIS) font.  */
		      if (smJapanese == scriptcode)
			{
			  font_name_table[font_name_count++]
			    = mac_to_x_fontname (name,
						 assc_entry->fontSize,
						 assc_entry->fontStyle,
						 smRoman);
			}
		    }
		}
	      
	      HUnlock (font_handle);
	      font_handle_2 = GetNextFOND (font_handle);
	      ReleaseResource (font_handle);
	      font_handle = font_handle_2;
	    }
	  while (ResError () == noErr && font_handle);
	}
  
      TextFont (old_fontnum);
#if TARGET_API_MAC_CARBON
    }
#endif  /* TARGET_API_MAC_CARBON */
}


/* Return a list of at most MAXNAMES font specs matching the one in
   PATTERN.  Note that each '*' in the PATTERN matches exactly one
   field of the font spec, unlike X in which an '*' in a font spec can
   match a number of fields.  The result is in the Mac implementation
   all fonts must be specified by a font spec with all 13 fields
   (although many of these can be "*'s").  */

Lisp_Object
x_list_fonts (struct frame *f,
              Lisp_Object pattern,
              int size,
              int maxnames)
{
  char *ptnstr;
  Lisp_Object newlist = Qnil;
  int n_fonts = 0;
  int i;
  struct gcpro gcpro1, gcpro2;

  if (font_name_table == NULL)  /* Initialize when first used.  */
    init_font_name_table ();

  ptnstr = XSTRING (pattern)->data;

  GCPRO2 (pattern, newlist);

  /* Scan and matching bitmap fonts.  */
  for (i = 0; i < font_name_count; i++)
    {
      if (mac_font_pattern_match (font_name_table[i], ptnstr))
        {
          newlist = Fcons (build_string (font_name_table[i]), newlist);

          n_fonts++;
          if (n_fonts >= maxnames)
            break;
        }
    }
  
  /* MAC_TODO: add code for matching outline fonts here */

  UNGCPRO;

  return newlist;
}


#if GLYPH_DEBUG

/* Check that FONT is valid on frame F.  It is if it can be found in F's
   font table.  */

static void
x_check_font (f, font)
     struct frame *f;
     XFontStruct *font;
{
  int i;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

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
     MacFontStruct *font;
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
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  MacFontStruct *font;
  int old_width = dpyinfo->smallest_char_width;
  int old_height = dpyinfo->smallest_font_height;
  
  dpyinfo->smallest_font_height = 100000;
  dpyinfo->smallest_char_width = 100000;
  
  for (i = 0; i < dpyinfo->n_fonts; ++i)
    if (dpyinfo->font_table[i].name)
      {
	struct font_info *fontp = dpyinfo->font_table + i;
	int w, h;
	
	font = (MacFontStruct *) fontp->font;
	xassert (font != (MacFontStruct *) ~0);
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


/* Determine whether given string is a fully-specified XLFD: all 14
   fields are present, none is '*'.  */

static int
is_fully_specified_xlfd (char *p)
{
  int i;
  char *q;

  if (*p != '-')
    return 0;
  
  for (i = 0; i < 13; i++)
    {
      q = strchr (p + 1, '-');
      if (q == NULL)
        return 0;
      if (q - p == 2 && *(p + 1) == '*')
        return 0;
      p = q;
    }

  if (strchr (p + 1, '-') != NULL)
    return 0;
  
  if (*(p + 1) == '*' && *(p + 2) == '\0')
    return 0;

  return 1;
}


const int kDefaultFontSize = 9;


/* XLoadQueryFont creates and returns an internal representation for a
   font in a MacFontStruct struct.  There is really no concept
   corresponding to "loading" a font on the Mac.  But we check its
   existence and find the font number and all other information for it
   and store them in the returned MacFontStruct.  */

static MacFontStruct *
XLoadQueryFont (Display *dpy, char *fontname)
{
  int i, size, is_two_byte_font, char_width;
  char *name;
  GrafPtr port;
  SInt16 old_fontnum, old_fontsize;
  Style old_fontface;
  Str32 mfontname;
  SInt16 fontnum;
  Style fontface = normal;
  MacFontStruct *font;
  FontInfo the_fontinfo;
  char s_weight[7], c_slant;

  if (is_fully_specified_xlfd (fontname))
    name = fontname;
  else
    {
      for (i = 0; i < font_name_count; i++)
        if (mac_font_pattern_match (font_name_table[i], fontname))
          break;

      if (i >= font_name_count)
        return NULL;
  
      name = font_name_table[i];
    }

  GetPort (&port);  /* save the current font number used */
#if TARGET_API_MAC_CARBON
  old_fontnum = GetPortTextFont (port);
  old_fontsize = GetPortTextSize (port);
  old_fontface = GetPortTextFace (port);
#else
  old_fontnum = port->txFont;
  old_fontsize = port->txSize;
  old_fontface = port->txFace;
#endif

  if (sscanf (name, "-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]--%d-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%*s", &size) != 1)
    size = kDefaultFontSize;

  if (sscanf (name, "-%*[^-]-%*[^-]-%6[^-]-%*c-%*[^-]--%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%*s", s_weight) == 1)
    if (strcmp (s_weight, "bold") == 0)
      fontface |= bold;

  if (sscanf (name, "-%*[^-]-%*[^-]-%*[^-]-%c-%*[^-]--%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%*s", &c_slant) == 1)
    if (c_slant == 'i')
      fontface |= italic;

  x_font_name_to_mac_font_name (name, mfontname);
  c2pstr (mfontname);
  GetFNum (mfontname, &fontnum);
  if (fontnum == 0)
    return NULL;
    
  font = (MacFontStruct *) xmalloc (sizeof (struct MacFontStruct));
  
  font->fontname = (char *) xmalloc (strlen (name) + 1);
  bcopy (name, font->fontname, strlen (name) + 1);

  font->mac_fontnum = fontnum;
  font->mac_fontsize = size;
  font->mac_fontface = fontface;
  font->mac_scriptcode = FontToScript (fontnum);

  /* Apple Japanese (SJIS) font is listed as both
     "*-jisx0208.1983-sjis" (Japanese script) and "*-jisx0201.1976-0"
     (Roman script) in init_font_name_table ().  The latter should be
     treated as a one-byte font.  */
  {
    char cs[32];

    if (sscanf (name, 
		"-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]--%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*c-%*[^-]-%31s",
		cs) == 1
	&& 0 == strcmp (cs, "mac-roman"))  
      font->mac_scriptcode = smRoman;
  }
  
  is_two_byte_font = font->mac_scriptcode == smJapanese ||
                     font->mac_scriptcode == smTradChinese ||
                     font->mac_scriptcode == smSimpChinese ||
                     font->mac_scriptcode == smKorean;

  TextFont (fontnum);
  TextSize (size);
  TextFace (fontface);
  
  GetFontInfo (&the_fontinfo);

  font->ascent = the_fontinfo.ascent;
  font->descent = the_fontinfo.descent;

  font->min_byte1 = 0;
  if (is_two_byte_font)
    font->max_byte1 = 1;
  else
    font->max_byte1 = 0;
  font->min_char_or_byte2 = 0x20;
  font->max_char_or_byte2 = 0xff;
  
  if (is_two_byte_font)
    {
      /* Use the width of an "ideographic space" of that font because
         the_fontinfo.widMax returns the wrong width for some fonts.  */
      switch (font->mac_scriptcode)
        {
        case smJapanese:
          char_width = StringWidth("\p\x81\x40");
          break;
        case smTradChinese:
          char_width = StringWidth("\p\xa1\x40");
          break;
        case smSimpChinese:
          char_width = StringWidth("\p\xa1\xa1");
          break;
        case smKorean:
          char_width = StringWidth("\p\xa1\xa1");
          break;
        }
    }
  else
    /* Do this instead of use the_fontinfo.widMax, which incorrectly
       returns 15 for 12-point Monaco! */
    char_width = CharWidth ('m');

  font->max_bounds.rbearing = char_width;
  font->max_bounds.lbearing = 0;
  font->max_bounds.width = char_width;
  font->max_bounds.ascent = the_fontinfo.ascent;
  font->max_bounds.descent = the_fontinfo.descent;

  font->min_bounds = font->max_bounds;

  if (is_two_byte_font || CharWidth ('m') == CharWidth ('i'))
    font->per_char = NULL;
  else
    {
      font->per_char = (XCharStruct *)
	xmalloc (sizeof (XCharStruct) * (0xff - 0x20 + 1));
      {
        int c;
    
        for (c = 0x20; c <= 0xff; c++)
          {
            font->per_char[c - 0x20] = font->max_bounds;
            font->per_char[c - 0x20].width = CharWidth (c);
          }
      }
    }
  
  TextFont (old_fontnum);  /* restore previous font number, size and face */
  TextSize (old_fontsize);
  TextFace (old_fontface);
  
  return font;
}


/* Load font named FONTNAME of the size SIZE for frame F, and return a
   pointer to the structure font_info while allocating it dynamically.
   If SIZE is 0, load any size of font.
   If loading is failed, return NULL.  */

struct font_info *
x_load_font (f, fontname, size)
     struct frame *f;
     register char *fontname;
     int size;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  Lisp_Object font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have by comparing names.  */
  font_names = x_list_fonts (f, build_string (fontname), size, 1);

  if (!NILP (font_names))
    {
      Lisp_Object tail;
      int i;

      for (i = 0; i < dpyinfo->n_fonts; i++)
	for (tail = font_names; CONSP (tail); tail = XCDR (tail))
	  if (dpyinfo->font_table[i].name
	      && (!strcmp (dpyinfo->font_table[i].name,
			   XSTRING (XCAR (tail))->data)
		  || !strcmp (dpyinfo->font_table[i].full_name,
			      XSTRING (XCAR (tail))->data)))
	    return (dpyinfo->font_table + i);
    }

  /* Load the font and add it to the table.  */
  {
    char *full_name;
    struct MacFontStruct *font;
    struct font_info *fontp;
    unsigned long value;
    int i;

    /* If we have found fonts by x_list_font, load one of them.  If
       not, we still try to load a font by the name given as FONTNAME
       because XListFonts (called in x_list_font) of some X server has
       a bug of not finding a font even if the font surely exists and
       is loadable by XLoadQueryFont.  */
    if (size > 0 && !NILP (font_names))
      fontname = (char *) XSTRING (XCAR (font_names))->data;

    font = (MacFontStruct *) XLoadQueryFont (FRAME_MAC_DISPLAY (f), fontname);
    if (!font)
      return NULL;

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
    fontp->name = (char *) xmalloc (strlen (font->fontname) + 1);
    bcopy (font->fontname, fontp->name, strlen (font->fontname) + 1);

    fontp->full_name = fontp->name;

    fontp->size = font->max_bounds.width;
    fontp->height = FONT_HEIGHT (font);
    {
      /* For some font, ascent and descent in max_bounds field is
	 larger than the above value.  */
      int max_height = font->max_bounds.ascent + font->max_bounds.descent;
      if (max_height > fontp->height)
	fontp->height = max_height;
    }

    /* The slot `encoding' specifies how to map a character
       code-points (0x20..0x7F or 0x2020..0x7F7F) of each charset to
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF), or
       (0:0x2020..0x7F7F, 1:0xA0A0..0xFFFF, 3:0x20A0..0x7FFF,
       2:0xA020..0xFF7F).  For the moment, we don't know which charset
       uses this font.  So, we set information in fontp->encoding[1]
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */
    if (font->mac_scriptcode == smJapanese)
      fontp->encoding[1] = 4;
    else
      {
        fontp->encoding[1]
           = (font->max_byte1 == 0
	      /* 1-byte font */
	      ? (font->min_char_or_byte2 < 0x80
	         ? (font->max_char_or_byte2 < 0x80
	            ? 0		/* 0x20..0x7F */
	            : FONT_ENCODING_NOT_DECIDED) /* 0x20..0xFF */
	         : 1)		/* 0xA0..0xFF */
	      /* 2-byte font */
	      : (font->min_byte1 < 0x80
	         ? (font->max_byte1 < 0x80
	            ? (font->min_char_or_byte2 < 0x80
		       ? (font->max_char_or_byte2 < 0x80
		          ? 0		/* 0x2020..0x7F7F */
		          : FONT_ENCODING_NOT_DECIDED) /* 0x2020..0x7FFF */
		       : 3)		/* 0x20A0..0x7FFF */
	            : FONT_ENCODING_NOT_DECIDED) /* 0x20??..0xA0?? */
	         : (font->min_char_or_byte2 < 0x80
	            ? (font->max_char_or_byte2 < 0x80
		       ? 2		/* 0xA020..0xFF7F */
		       : FONT_ENCODING_NOT_DECIDED) /* 0xA020..0xFFFF */
	            : 1)));		/* 0xA0A0..0xFFFF */
      }

#if 0 /* MAC_TODO: fill these out with more reasonably values */
    fontp->baseline_offset
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_BASELINE_OFFSET, &value)
	 ? (long) value : 0);
    fontp->relative_compose
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_RELATIVE_COMPOSE, &value)
	 ? (long) value : 0);
    fontp->default_ascent
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_DEFAULT_ASCENT, &value)
	 ? (long) value : 0);
#else
    fontp->baseline_offset = 0;
    fontp->relative_compose = 0;
    fontp->default_ascent = 0;
#endif

    /* Set global flag fonts_changed_p to non-zero if the font loaded
       has a character with a smaller width than any other character
       before, or if the font loaded has a smalle>r height than any
       other font loaded before.  If this happens, it will make a
       glyph matrix reallocation necessary.  */
    fonts_changed_p = x_compute_min_glyph_bounds (f);
    UNBLOCK_INPUT;
    return fontp;
  }
}


/* Return a pointer to struct font_info of a font named FONTNAME for
   frame F.  If no such font is loaded, return NULL.  */

struct font_info *
x_query_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  int i;

  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name
	&& (!strcmp (dpyinfo->font_table[i].name, fontname)
	    || !strcmp (dpyinfo->font_table[i].full_name, fontname)))
      return (dpyinfo->font_table + i);
  return NULL;
}


/* Find a CCL program for a font specified by FONTP, and set the member
 `encoder' of the structure.  */

void
x_find_ccl_program (fontp)
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



/***********************************************************************
			    Initialization
 ***********************************************************************/

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {"-geometry",	".geometry", XrmoptionSepArg, NULL},
  {"-iconic",	".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",	"*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

  {"-T",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",	"*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",	"*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};
#endif /* USE_X_TOOLKIT */

static int x_initialized;

#ifdef MULTI_KBOARD
/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static int
same_x_server (name1, name2)
     char *name1, *name2;
{
  int seen_colon = 0;
  unsigned char *system_name = XSTRING (Vsystem_name)->data;
  int system_name_length = strlen (system_name);
  int length_until_period = 0;

  while (system_name[length_until_period] != 0
	 && system_name[length_until_period] != '.')
    length_until_period++;

  /* Treat `unix' like an empty host name.  */
  if (! strncmp (name1, "unix:", 5))
    name1 += 4;
  if (! strncmp (name2, "unix:", 5))
    name2 += 4;
  /* Treat this host's name like an empty host name.  */
  if (! strncmp (name1, system_name, system_name_length)
      && name1[system_name_length] == ':')
    name1 += system_name_length;
  if (! strncmp (name2, system_name, system_name_length)
      && name2[system_name_length] == ':')
    name2 += system_name_length;
  /* Treat this host's domainless name like an empty host name.  */
  if (! strncmp (name1, system_name, length_until_period)
      && name1[length_until_period] == ':')
    name1 += length_until_period;
  if (! strncmp (name2, system_name, length_until_period)
      && name2[length_until_period] == ':')
    name2 += length_until_period;

  for (; *name1 != '\0' && *name1 == *name2; name1++, name2++)
    {
      if (*name1 == ':')
	seen_colon++;
      if (seen_colon && *name1 == '.')
	return 1;
    }
  return (seen_colon
	  && (*name1 == '.' || *name1 == '\0')
	  && (*name2 == '.' || *name2 == '\0'));
}
#endif


/* The Mac Event loop code */

#ifndef MAC_OSX
#include <Events.h>
#include <Quickdraw.h>
#include <Balloons.h>
#include <Devices.h>
#include <Fonts.h>
#include <Gestalt.h>
#include <Menus.h>
#include <Processes.h>
#include <Sound.h>
#include <ToolUtils.h>
#include <TextUtils.h>
#include <Dialogs.h>
#include <Script.h>
#include <Types.h>
#include <TextEncodingConverter.h>
#include <Resources.h>

#if __MWERKS__
#include <unix.h>
#endif
#endif /* ! MAC_OSX */

#define M_APPLE 128
#define I_ABOUT 1

#define WINDOW_RESOURCE 128
#define TERM_WINDOW_RESOURCE 129

#define DEFAULT_NUM_COLS 80

#define MIN_DOC_SIZE 64
#define MAX_DOC_SIZE 32767

/* sleep time for WaitNextEvent */
#define WNE_SLEEP_AT_SUSPEND 10
#define WNE_SLEEP_AT_RESUME  1

/* true when cannot handle any Mac OS events */
static int handling_window_update = 0;

/* the flag appl_is_suspended is used both for determining the sleep
   time to be passed to WaitNextEvent and whether the cursor should be
   drawn when updating the display.  The cursor is turned off when
   Emacs is suspended.  Redrawing it is unnecessary and what needs to
   be done depends on whether the cursor lies inside or outside the
   redraw region.  So we might as well skip drawing it when Emacs is
   suspended.  */
static Boolean app_is_suspended = false;
static long app_sleep_time = WNE_SLEEP_AT_RESUME;

#define EXTRA_STACK_ALLOC (256 * 1024)

#define ARGV_STRING_LIST_ID 129
#define ABOUT_ALERT_ID	128
#define RAM_TOO_LARGE_ALERT_ID 129

Boolean	terminate_flag = false;

/* true if using command key as meta key */
Lisp_Object Vmac_command_key_is_meta;

/* convert input from Mac keyboard (assumed to be in Mac Roman coding)
   to this text encoding */
int mac_keyboard_text_encoding;
int current_mac_keyboard_text_encoding = kTextEncodingMacRoman;

/* Set in term/mac-win.el to indicate that event loop can now generate
   drag and drop events.  */
Lisp_Object Qmac_ready_for_drag_n_drop;

Lisp_Object drag_and_drop_file_list;

Point saved_menu_event_location;

/* Apple Events */
static void init_required_apple_events(void);
static pascal OSErr
do_ae_open_application(const AppleEvent *, AppleEvent *, long);
static pascal OSErr
do_ae_print_documents(const AppleEvent *, AppleEvent *, long);
static pascal OSErr do_ae_open_documents(AppleEvent *, AppleEvent *, long);
static pascal OSErr do_ae_quit_application(AppleEvent *, AppleEvent *, long);

extern void init_emacs_passwd_dir ();
extern int emacs_main (int, char **, char **);
extern void check_alarm ();

extern void initialize_applescript();
extern void terminate_applescript();


static void
do_get_menus (void)
{
  Handle menubar_handle;
  MenuHandle menu_handle;
	
  menubar_handle = GetNewMBar (128);
  if(menubar_handle == NULL)
    abort ();
  SetMenuBar (menubar_handle);
  DrawMenuBar ();

  menu_handle = GetMenuHandle (M_APPLE);
  if(menu_handle != NULL)
    AppendResMenu (menu_handle,'DRVR');
  else
    abort ();
}


static void
do_init_managers (void)
{
#if !TARGET_API_MAC_CARBON
  InitGraf (&qd.thePort);
  InitFonts ();
  FlushEvents (everyEvent, 0);
  InitWindows ();
  InitMenus ();
  TEInit ();
  InitDialogs (NULL);
#endif /* !TARGET_API_MAC_CARBON */
  InitCursor ();
	
#if !TARGET_API_MAC_CARBON
  /* set up some extra stack space for use by emacs */
  SetApplLimit ((Ptr) ((long) GetApplLimit () - EXTRA_STACK_ALLOC));

  /* MaxApplZone must be called for AppleScript to execute more
     complicated scripts */
  MaxApplZone ();
  MoreMasters ();
#endif /* !TARGET_API_MAC_CARBON */
}

static void
do_check_ram_size (void)
{
  SInt32 physical_ram_size, logical_ram_size;
  
  if (Gestalt (gestaltPhysicalRAMSize, &physical_ram_size) != noErr
      || Gestalt (gestaltLogicalRAMSize, &logical_ram_size) != noErr
      || physical_ram_size > 256 * 1024 * 1024
      || logical_ram_size > 256 * 1024 * 1024)
    {
      StopAlert (RAM_TOO_LARGE_ALERT_ID, NULL);
      exit (1);
    }
}

static void
do_window_update (WindowPtr win)
{
  struct mac_output *mwp = (mac_output *) GetWRefCon (win);
  struct frame *f = mwp->mFP;

  if (f)
    {
      if (f->async_visible == 0)
        {
          f->async_visible = 1;
          f->async_iconified = 0;
          SET_FRAME_GARBAGED (f);
          
          /* An update event is equivalent to MapNotify on X, so report
             visibility changes properly.  */
          if (! NILP(Vframe_list) && ! NILP (XCDR (Vframe_list)))
            /* Force a redisplay sooner or later to update the
               frame titles in case this is the second frame.  */
            record_asynch_buffer_change ();
        }
      else
        {
          BeginUpdate (win);
          handling_window_update = 1;

          expose_frame (f, 0, 0, 0, 0);

          handling_window_update = 0;
          EndUpdate (win);
        }
    }
}

static int
is_emacs_window (WindowPtr win)
{
  Lisp_Object tail, frame;

  if (!win)
    return 0;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_MAC_P (XFRAME (frame)))
      if (FRAME_MAC_WINDOW (XFRAME (frame)) == win)
	return 1;

  return 0;
}

static void
do_window_activate (WindowPtr win)
{
  mac_output *mwp;
  struct frame *f;

  if (is_emacs_window (win))
    {
      mwp = (mac_output *) GetWRefCon (win);
      f = mwp->mFP;
      
      if (f)
	{
	  x_new_focus_frame (FRAME_MAC_DISPLAY_INFO (f), f);
	  activate_scroll_bars (f);
	}
    }
}

static void
do_window_deactivate (WindowPtr win)
{
  mac_output *mwp;
  struct frame *f;

  if (is_emacs_window (win))
    {
      mwp = (mac_output *) GetWRefCon (win);
      f = mwp->mFP;
      
      if (f == FRAME_MAC_DISPLAY_INFO (f)->x_focus_frame)
	{
	  x_new_focus_frame (FRAME_MAC_DISPLAY_INFO (f), 0);
	  deactivate_scroll_bars (f);
	}
    }
}

static void
do_app_resume ()
{
  WindowPtr wp;
  mac_output *mwp;
  struct frame *f;

  wp = FrontWindow();
  if (is_emacs_window (wp))
    {
      mwp = (mac_output *) GetWRefCon (wp);
      f = mwp->mFP;

      if (f)
	{
	  x_new_focus_frame (FRAME_MAC_DISPLAY_INFO (f), f);
	  activate_scroll_bars (f);
	}
    }

  app_is_suspended = false;
  app_sleep_time = WNE_SLEEP_AT_RESUME;
}

static void
do_app_suspend ()
{
  WindowPtr wp;
  mac_output *mwp;
  struct frame *f;

  wp = FrontWindow();
  if (is_emacs_window (wp))
    {
      mwp = (mac_output *) GetWRefCon (wp);
      f = mwp->mFP;

      if (f == FRAME_MAC_DISPLAY_INFO (f)->x_focus_frame)
	{
	  x_new_focus_frame (FRAME_MAC_DISPLAY_INFO (f), 0);
	  deactivate_scroll_bars (f);
	}
    }

  app_is_suspended = true;
  app_sleep_time = WNE_SLEEP_AT_SUSPEND;
}


static void
do_mouse_moved (Point mouse_pos)
{
  WindowPtr wp = FrontWindow ();
  struct frame *f = ((mac_output *) GetWRefCon (wp))->mFP;            

#if TARGET_API_MAC_CARBON
      SetPort (GetWindowPort (wp));
#else
      SetPort (wp);
#endif

  GlobalToLocal (&mouse_pos);

  note_mouse_movement (f, &mouse_pos);
}


static void
do_os_event (EventRecord *erp)
{
  switch((erp->message >> 24) & 0x000000FF)
    {
    case suspendResumeMessage:
      if((erp->message & resumeFlag) == 1)
	do_app_resume ();
      else
	do_app_suspend ();
      break;
				
    case mouseMovedMessage:
      do_mouse_moved (erp->where);
      break;
    }
}

static void
do_events (EventRecord *erp)
{
  switch (erp->what)
    {
    case updateEvt:
      do_window_update ((WindowPtr) erp->message);
      break;

    case osEvt:
      do_os_event (erp);
      break;

    case activateEvt:
      if ((erp->modifiers & activeFlag) != 0)
	do_window_activate ((WindowPtr) erp->message);
      else
	do_window_deactivate ((WindowPtr) erp->message);
      break;
    }
}

static void
do_apple_menu (SInt16 menu_item)
{
#if !TARGET_API_MAC_CARBON
  Str255 item_name;
  SInt16 da_driver_refnum;
	
  if (menu_item == I_ABOUT)
    NoteAlert (ABOUT_ALERT_ID, NULL);
  else
    {
      GetMenuItemText (GetMenuHandle (M_APPLE), menu_item, item_name);
      da_driver_refnum = OpenDeskAcc (item_name);
    }
#endif /* !TARGET_API_MAC_CARBON */
}

void
do_menu_choice (SInt32 menu_choice)
{
  SInt16 menu_id, menu_item;
  
  menu_id = HiWord (menu_choice);
  menu_item = LoWord (menu_choice);
  
  if (menu_id == 0)
    return;
  
  switch (menu_id)
    {
    case M_APPLE:
      do_apple_menu (menu_item);
      break;

    default:
      {
        WindowPtr wp = FrontWindow ();
        struct frame *f = ((mac_output *) GetWRefCon (wp))->mFP;            
        MenuHandle menu = GetMenuHandle (menu_id);
        if (menu)
          {
            UInt32 refcon;
            
            GetMenuItemRefCon (menu, menu_item, &refcon);
            menubar_selection_callback (f, refcon);
          }
      }
    }
  
  HiliteMenu (0);
}


/* Handle drags in size box.  Based on code contributed by Ben
   Mesander and IM - Window Manager A.  */

static void
do_grow_window (WindowPtr w, EventRecord *e)
{
  long grow_size;
  Rect limit_rect;
  int rows, columns;
  mac_output *mwp = (mac_output *) GetWRefCon (w);
  struct frame *f = mwp->mFP;
  
  SetRect(&limit_rect, MIN_DOC_SIZE, MIN_DOC_SIZE, MAX_DOC_SIZE, MAX_DOC_SIZE);
  
  grow_size = GrowWindow (w, e->where, &limit_rect);
  
  /* see if it really changed size */
  if (grow_size != 0)
    {
      rows = PIXEL_TO_CHAR_HEIGHT (f, HiWord (grow_size));
      columns = PIXEL_TO_CHAR_WIDTH (f, LoWord (grow_size));
      
      x_set_window_size (f, 0, columns, rows);
    }
}


/* Handle clicks in zoom box.  Calculation of "standard state" based
   on code in IM - Window Manager A and code contributed by Ben
   Mesander.  The standard state of an Emacs window is 80-characters
   wide (DEFAULT_NUM_COLS) and as tall as will fit on the screen.  */

static void
do_zoom_window (WindowPtr w, int zoom_in_or_out)
{
  GrafPtr save_port;
  Rect zoom_rect, port_rect;
  Point top_left;
  int w_title_height, columns, rows, width, height, dummy, x, y;
  mac_output *mwp = (mac_output *) GetWRefCon (w);
  struct frame *f = mwp->mFP;
   
  GetPort (&save_port);

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (w));
#else
  SetPort (w);
#endif

  /* Clear window to avoid flicker.  */
#if TARGET_API_MAC_CARBON
  {
    Rect r;
    BitMap bm;
    
    GetWindowPortBounds (w, &r);
    EraseRect (&r);

    if (zoom_in_or_out == inZoomOut)
      {
        /* calculate height of window's title bar (hard card it for now).  */
        w_title_height = 20 + GetMBarHeight ();

        /* get maximum height of window into zoom_rect.bottom -
	   zoom_rect.top */
        GetQDGlobalsScreenBits (&bm);
        zoom_rect = bm.bounds;
        zoom_rect.top += w_title_height;
        InsetRect (&zoom_rect, 8, 4);  /* not too tight */
    
        zoom_rect.right = zoom_rect.left
	  + CHAR_TO_PIXEL_WIDTH (f, DEFAULT_NUM_COLS);

        SetWindowStandardState (w, &zoom_rect);
      }
  }
#else /* not TARGET_API_MAC_CARBON */
  EraseRect (&(w->portRect));
  if (zoom_in_or_out == inZoomOut)
    {
      SetPt (&top_left, w->portRect.left, w->portRect.top);
      LocalToGlobal (&top_left);

      /* calculate height of window's title bar */
      w_title_height = top_left.v - 1
	- (**((WindowPeek) w)->strucRgn).rgnBBox.top + GetMBarHeight ();

      /* get maximum height of window into zoom_rect.bottom - zoom_rect.top */
      zoom_rect = qd.screenBits.bounds;
      zoom_rect.top += w_title_height;
      InsetRect (&zoom_rect, 8, 4);  /* not too tight */
      
      zoom_rect.right = zoom_rect.left
	+ CHAR_TO_PIXEL_WIDTH (f, DEFAULT_NUM_COLS);

      (**((WStateDataHandle) ((WindowPeek) w)->dataHandle)).stdState
	= zoom_rect;
    }
#endif /* not TARGET_API_MAC_CARBON */

  ZoomWindow (w, zoom_in_or_out, w == FrontWindow ());

  /* retrieve window size and update application values */
#if TARGET_API_MAC_CARBON
  GetWindowPortBounds (w, &port_rect);
#else
  port_rect = w->portRect;
#endif
  rows = PIXEL_TO_CHAR_HEIGHT (f, port_rect.bottom - port_rect.top);
  columns = PIXEL_TO_CHAR_WIDTH (f, port_rect.right - port_rect.left);
  x_set_window_size (mwp->mFP, 0, columns, rows);

  SetPort (save_port);
}


/* Intialize AppleEvent dispatcher table for the required events.  */
void
init_required_apple_events ()
{
  OSErr err;
  long result;

  /* Make sure we have apple events before starting.  */
  err = Gestalt (gestaltAppleEventsAttr, &result);
  if (err != noErr)
    abort ();

  if (!(result & (1 << gestaltAppleEventsPresent)))
    abort ();
  
#if TARGET_API_MAC_CARBON
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication,
			      NewAEEventHandlerUPP
			      ((AEEventHandlerProcPtr) do_ae_open_application),
                              0L, false);
#else
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication,
			      NewAEEventHandlerProc
			      ((AEEventHandlerProcPtr) do_ae_open_application),
                              0L, false);
#endif
  if (err != noErr)
    abort ();

#if TARGET_API_MAC_CARBON
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments,
                              NewAEEventHandlerUPP
			      ((AEEventHandlerProcPtr) do_ae_open_documents),
                              0L, false);
#else
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments,
                              NewAEEventHandlerProc
			      ((AEEventHandlerProcPtr) do_ae_open_documents),
                              0L, false);
#endif
  if (err != noErr)
    abort ();

#if TARGET_API_MAC_CARBON
  err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments,
                              NewAEEventHandlerUPP
			      ((AEEventHandlerProcPtr) do_ae_print_documents),
                              0L, false);
#else
  err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments,
                              NewAEEventHandlerProc
			      ((AEEventHandlerProcPtr) do_ae_print_documents),
                              0L, false);
#endif
  if (err != noErr)
    abort ();

#if TARGET_API_MAC_CARBON
  err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication,
                              NewAEEventHandlerUPP
			      ((AEEventHandlerProcPtr) do_ae_quit_application),
                              0L, false);
#else
  err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication,
                              NewAEEventHandlerProc
			      ((AEEventHandlerProcPtr) do_ae_quit_application),
                              0L, false);
#endif
  if (err != noErr)
    abort ();
}


/* Open Application Apple Event */
static pascal OSErr
do_ae_open_application(const AppleEvent *pae, AppleEvent *preply, long prefcon)
{
  return noErr;
}


/* Defined in mac.c.  */
extern int
path_from_vol_dir_name (char *, int, short, long, char *);


/* Called when we receive an AppleEvent with an ID of
   "kAEOpenDocuments".  This routine gets the direct parameter,
   extracts the FSSpecs in it, and puts their names on a list.  */
static pascal OSErr     
do_ae_open_documents(AppleEvent *message, AppleEvent *reply, long refcon)
{
  OSErr err, err2;
  AEDesc the_desc;
  AEKeyword keyword;
  DescType actual_type;
  Size actual_size;

  err = AEGetParamDesc (message, keyDirectObject, typeAEList, &the_desc);
  if (err != noErr)
    goto descriptor_error_exit;

  /* Check to see that we got all of the required parameters from the
     event descriptor.  For an 'odoc' event this should just be the
     file list.  */
  err = AEGetAttributePtr(message, keyMissedKeywordAttr, typeWildCard, 
			  &actual_type, (Ptr) &keyword,
                          sizeof (keyword), &actual_size);
  /* No error means that we found some unused parameters.
     errAEDescNotFound means that there are no more parameters.  If we
     get an error code other than that, flag it.  */
  if ((err == noErr) || (err != errAEDescNotFound))
    {
      err = errAEEventNotHandled;
      goto error_exit;
    }
  err = noErr;

  /* Got all the parameters we need.  Now, go through the direct
     object list and parse it up.  */
  {
    long num_files_to_open;

    err = AECountItems (&the_desc, &num_files_to_open);
    if (err == noErr)
      {
        int i;
        
        /* AE file list is one based so just use that for indexing here.  */
        for (i = 1; (err == noErr) && (i <= num_files_to_open); i++) {
          FSSpec fs;
	  Str255 path_name, unix_path_name;

          err = AEGetNthPtr(&the_desc, i, typeFSS, &keyword, &actual_type,
			    (Ptr) &fs, sizeof (fs), &actual_size);
          if (err != noErr) break;

	  if (path_from_vol_dir_name (path_name, 255, fs.vRefNum, fs.parID,
				      fs.name) &&
	      mac_to_posix_pathname (path_name, unix_path_name, 255))
            drag_and_drop_file_list = Fcons (build_string (unix_path_name),
					     drag_and_drop_file_list);
        }
      }
  }

error_exit:
  /* Nuke the coerced file list in any case */
  err2 = AEDisposeDesc(&the_desc);

descriptor_error_exit:
  /* InvalRect(&(gFrontMacWindowP->mWP->portRect)); */
  return err;
}


/* Print Document Apple Event */
static pascal OSErr
do_ae_print_documents (const AppleEvent *pAE, AppleEvent *reply, long refcon)
{
  return errAEEventNotHandled;
}


static pascal OSErr
do_ae_quit_application (AppleEvent* message, AppleEvent *reply, long refcon)
{
  /* FixMe: Do we need an unwind-protect or something here?  And what
     do we do about unsaved files. Currently just forces quit rather
     than doing recursive callback to get user input.  */

  terminate_flag = true;

  /* Fkill_emacs doesn't return.  We have to return. (TI) */
  return noErr;
}


#if __profile__
void
profiler_exit_proc ()
{
  ProfilerDump ("\pEmacs.prof");
  ProfilerTerm ();
}
#endif

/* These few functions implement Emacs as a normal Mac application
   (almost): set up the heap and the Toolbox, handle necessary
   system events plus a few simple menu events.  They also set up
   Emacs's access to functions defined in the rest of this file.
   Emacs uses function hooks to perform all its terminal I/O.  A
   complete list of these functions appear in termhooks.h.  For what
   they do, read the comments there and see also w32term.c and
   xterm.c.  What's noticeably missing here is the event loop, which
   is normally present in most Mac application.  After performing the
   necessary Mac initializations, main passes off control to
   emacs_main (corresponding to main in emacs.c).  Emacs_main calls
   mac_read_socket (defined further below) to read input.  This is
   where WaitNextEvent is called to process Mac events.  This is also
   where check_alarm in sysdep.c is called to simulate alarm signals.
   This makes the cursor jump back to its correct position after
   briefly jumping to that of the matching parenthesis, print useful
   hints and prompts in the minibuffer after the user stops typing for
   a wait, etc.  */

#if !TARGET_API_MAC_CARBON
#undef main
int 
main (void)
{
#if __profile__  /* is the profiler on? */
  if (ProfilerInit(collectDetailed, bestTimeBase, 5000, 200))
    exit(1);
#endif

#if __MWERKS__
  /* set creator and type for files created by MSL */
  _fcreator = 'EMAx';
  _ftype = 'TEXT';
#endif

  do_init_managers ();
	
  do_get_menus ();
	
  do_check_ram_size ();

  init_emacs_passwd_dir ();

  init_environ ();

  initialize_applescript ();

  init_required_apple_events ();
	
  {
    char **argv;
    int argc = 0;

    /* set up argv array from STR# resource */
    get_string_list (&argv, ARGV_STRING_LIST_ID);
    while (argv[argc])
      argc++;

    /* free up AppleScript resources on exit */
    atexit (terminate_applescript);

#if __profile__  /* is the profiler on? */
    atexit (profiler_exit_proc);
#endif

    /* 3rd param "envp" never used in emacs_main */
    (void) emacs_main (argc, argv, 0);
  }

  /* Never reached - real exit in Fkill_emacs */
  return 0;
}
#endif

/* Table for translating Mac keycode to X keysym values.  Contributed
   by Sudhir Shenoy.  */
static unsigned char keycode_to_xkeysym_table[] = {
/* 0x00 - 0x3f */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 0x40 */
  0, '\xae' /* kp. */, 0, '\xaa' /* kp* */, 
  0, '\xab' /* kp+ */, 0, '\x7f' /* kp_clr */,
  0, 0, 0, '\xaf' /* kp/ */,
  '\x8d' /* kp_ent */, 0, '\xad' /* kp- */, 0,
/* 0x50 */
  0, '\xbd' /* kp= */, '\xb0' /* kp0 */, '\xb1' /* kp1 */,
  '\xb2' /* kp2 */, '\xb3' /* kp3 */, '\xb4' /* kp4 */, '\xb5' /* kp5 */,
  '\xb6' /* kp6 */, '\xb7' /* kp7 */, 0, '\xb8' /* kp8 */,
  '\xb9' /* kp9 */, 0, 0, 0,
/* 0x60 */
  '\xc2' /* F5 */, '\xc3' /* F6 */, '\xc4' /* F7 */, '\xc0' /* F3 */,
  '\xc5' /* F8 */, '\xc6' /* F9 */, 0, '\xc8' /* F11 */, 
  0, '\xca' /* F13 */, 0, '\xcb' /* F14 */, 
  0, '\xc7' /* F10 */, 0, '\xc9' /* F12 */,   
/* 0x70 */
  0, '\xcc' /* F15 */, '\x9e' /* ins */, '\x95' /* home */,
  '\x9a' /* pgup */, '\x9f' /* del */, '\xc1' /* F4 */, '\x9c' /* end */,
  '\xbf' /* F2 */, '\x9b' /* pgdown */, '\xbe' /* F1 */, '\x51' /* left */,
  '\x53' /* right */, '\x54' /* down */, '\x52' /* up */, 0
};

static int
keycode_to_xkeysym (int keyCode, int *xKeySym)
{
  *xKeySym = keycode_to_xkeysym_table [keyCode & 0x7f];
  return *xKeySym != 0;
}

/* Emacs calls this whenever it wants to read an input event from the
   user. */
int
XTread_socket (int sd, struct input_event *bufp, int numchars, int expected)
{
  int count = 0;
  EventRecord er;
  int the_modifiers;
  EventMask event_mask;

#if 0
  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }
#endif

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  if (numchars <= 0)
    abort ();

  /* Don't poll for events to process (specifically updateEvt) if
     window update currently already in progress.  A call to redisplay
     (in do_window_update) can be preempted by another call to
     redisplay, causing blank regions to be left on the screen and the
     cursor to be left at strange places.  */
  if (handling_window_update)
    {
      UNBLOCK_INPUT;
      return 0;
    }

  if (terminate_flag)
    Fkill_emacs (make_number (1));

  /* It is necessary to set this (additional) argument slot of an
     event to nil because keyboard.c protects incompletely processed
     event from being garbage collected by placing them in the
     kbd_buffer_gcpro vector.  */
  bufp->arg = Qnil;

  event_mask = everyEvent;
  if (NILP (Fboundp (Qmac_ready_for_drag_n_drop)))
    event_mask -= highLevelEventMask;

  if (WaitNextEvent (event_mask, &er, (expected ? app_sleep_time : 0L), NULL))
    switch (er.what)
      {
      case mouseDown:
      case mouseUp:
	{
	  WindowPtr window_ptr = FrontWindow ();
	  SInt16 part_code;

          if (mouse_tracking_in_progress == mouse_tracking_scroll_bar
	      && er.what == mouseUp)
            {
	      struct mac_output *mwp = (mac_output *) GetWRefCon (window_ptr);
	      Point mouse_loc = er.where;
	      
	      /* Convert to local coordinates of new window.  */
#if TARGET_API_MAC_CARBON
              SetPort (GetWindowPort (window_ptr));
#else
              SetPort (window_ptr);
#endif

	      GlobalToLocal (&mouse_loc);
		  
	      bufp->code = 0;  /* only one mouse button */
              bufp->kind = SCROLL_BAR_CLICK_EVENT;
              bufp->frame_or_window = tracked_scroll_bar->window;
              bufp->part = scroll_bar_handle;
              bufp->modifiers = up_modifier;
	      bufp->timestamp = er.when * (1000 / 60);
	        /* ticks to milliseconds */

              XSETINT (bufp->x, tracked_scroll_bar->left + 2);
              XSETINT (bufp->y, mouse_loc.v - 24);
              tracked_scroll_bar->dragging = Qnil;		      
              mouse_tracking_in_progress = mouse_tracking_none;
              tracked_scroll_bar = NULL;
              count++;
              break;
            }

	  part_code = FindWindow (er.where, &window_ptr);
					
	  switch (part_code)
	    {
	    case inMenuBar:
              {
                struct frame *f = ((mac_output *)
				   GetWRefCon (FrontWindow ()))->mFP;
                saved_menu_event_location = er.where;
                bufp->kind = MENU_BAR_ACTIVATE_EVENT;
                XSETFRAME (bufp->frame_or_window, f);
                count++;
              }
	      break;

	    case inContent:
	      if (window_ptr != FrontWindow ())
		SelectWindow (window_ptr);
	      else
	        {
		  SInt16 control_part_code;
		  ControlHandle ch;
		  struct mac_output *mwp = (mac_output *)
		    GetWRefCon (window_ptr);
		  Point mouse_loc = er.where;
		  
		  /* convert to local coordinates of new window */
#if TARGET_API_MAC_CARBON
                  SetPort (GetWindowPort (window_ptr));
#else
                  SetPort (window_ptr);
#endif

		  GlobalToLocal (&mouse_loc);
#if TARGET_API_MAC_CARBON
		  ch = FindControlUnderMouse (mouse_loc, window_ptr,
					      &control_part_code);
#else
		  control_part_code = FindControl (mouse_loc, window_ptr, &ch);
#endif		  
	          bufp->code = 0;  /* only one mouse button */
		  XSETINT (bufp->x, mouse_loc.h);
		  XSETINT (bufp->y, mouse_loc.v);
		  bufp->timestamp = er.when * (1000 / 60);
		    /* ticks to milliseconds */

#if TARGET_API_MAC_CARBON		  
		  if (ch != 0)
#else
		  if (control_part_code != 0)
#endif
		    {
		      struct scroll_bar *bar = (struct scroll_bar *)
			GetControlReference (ch);
		      x_scroll_bar_handle_click (bar, control_part_code, &er,
						 bufp);
		      if (er.what == mouseDown
			  && control_part_code == kControlIndicatorPart)
		        {
		          mouse_tracking_in_progress
			    = mouse_tracking_scroll_bar;
		          tracked_scroll_bar = bar;
		        }
		      else
		        {
		          mouse_tracking_in_progress = mouse_tracking_none;
		          tracked_scroll_bar = NULL;
		        }
		    }
		  else
	            {
	              bufp->kind = MOUSE_CLICK_EVENT;
		      XSETFRAME (bufp->frame_or_window, mwp->mFP);
		      if (er.what == mouseDown)
		        mouse_tracking_in_progress
			  = mouse_tracking_mouse_movement;
		      else
		        mouse_tracking_in_progress = mouse_tracking_none;
	            }
	      	  		
	          switch (er.what)
		    {
		    case mouseDown:
		      bufp->modifiers = down_modifier;
		      break;
		    case mouseUp:
		      bufp->modifiers = up_modifier;
		      break;
		    }
								
	          count++;
	        }
	      break;

	    case inDrag:
#if TARGET_API_MAC_CARBON
	      {
	        BitMap bm;
	        
	        GetQDGlobalsScreenBits (&bm);
	        DragWindow (window_ptr, er.where, &bm.bounds);
	      }
#else /* not TARGET_API_MAC_CARBON */
	      DragWindow (window_ptr, er.where, &qd.screenBits.bounds);
#endif /* not TARGET_API_MAC_CARBON */
	      break;

	    case inGoAway:
	      if (TrackGoAway (window_ptr, er.where))
	        {
	          bufp->kind = DELETE_WINDOW_EVENT;
	          XSETFRAME (bufp->frame_or_window,
			     ((mac_output *) GetWRefCon (window_ptr))->mFP);
 	          count++;
	        }
	      break;

	    /* window resize handling added --ben */
	    case inGrow:
	      do_grow_window(window_ptr, &er);
	      break;
	    
	    /* window zoom handling added --ben */
	    case inZoomIn:
	    case inZoomOut:
	      if (TrackBox (window_ptr, er.where, part_code))
	        do_zoom_window (window_ptr, part_code);
	      break;

	    default:
	      break;
	    }
	}
	break;
	
      case updateEvt:
      case osEvt:
      case activateEvt:
    	do_events (&er);
	break;
	
      case keyDown:
      case autoKey:
	{
	  int keycode = (er.message & keyCodeMask) >> 8;
	  int xkeysym;
	  
	  ObscureCursor ();

	  if (keycode == 0x33)  /* delete key (charCode translated to 0x8) */
	    {
	      bufp->code = 0x7f;
	      bufp->kind = ASCII_KEYSTROKE_EVENT;
	    }
	  else if (keycode_to_xkeysym (keycode, &xkeysym))
	    {
	      bufp->code = 0xff00 | xkeysym;
	      bufp->kind = NON_ASCII_KEYSTROKE_EVENT;
	    }	      
	  else
	    {
	      if (er.modifiers
		  & (NILP (Vmac_command_key_is_meta) ? optionKey : cmdKey))
	        {
	          /* This code comes from Keyboard Resource, Appendix
		     C of IM - Text.  This is necessary since shift is
		     ignored in KCHR table translation when option or
		     command is pressed. */
	          int new_modifiers = er.modifiers & 0xf600;
		    /* mask off option and command */
	          int new_keycode = keycode | new_modifiers;
	          Ptr kchr_ptr = (Ptr) GetScriptManagerVariable (smKCHRCache);
	          unsigned long some_state = 0;
	          bufp->code = KeyTranslate (kchr_ptr, new_keycode,
					     &some_state) & 0xff;
	        }
	      else
	        bufp->code = er.message & charCodeMask;
	      bufp->kind = ASCII_KEYSTROKE_EVENT;
	    }
	}

        /* If variable mac-convert-keyboard-input-to-latin-1 is non-nil,
           convert non-ASCII characters typed at the Mac keyboard
           (presumed to be in the Mac Roman encoding) to iso-latin-1
           encoding before they are passed to Emacs.  This enables the
           Mac keyboard to be used to enter non-ASCII iso-latin-1
           characters directly.  */
        if (mac_keyboard_text_encoding != kTextEncodingMacRoman
	    && bufp->kind == ASCII_KEYSTROKE_EVENT && bufp->code >= 128)
	  {
            static TECObjectRef converter = NULL;
            OSStatus the_err = noErr;
            OSStatus convert_status = noErr;

            if (converter ==  NULL)
              {
                the_err = TECCreateConverter (&converter,
					      kTextEncodingMacRoman,
					      mac_keyboard_text_encoding);
                current_mac_keyboard_text_encoding
		  = mac_keyboard_text_encoding;
              }
            else if (mac_keyboard_text_encoding
		     != current_mac_keyboard_text_encoding)
              {
                /* Free the converter for the current encoding before
                   creating a new one.  */
                TECDisposeConverter (converter);
                the_err = TECCreateConverter (&converter,
					      kTextEncodingMacRoman,
					      mac_keyboard_text_encoding);
                current_mac_keyboard_text_encoding
		  = mac_keyboard_text_encoding;
              } 
              
            if (the_err == noErr)
              {
                unsigned char ch = bufp->code;
                ByteCount actual_input_length, actual_output_length;
                unsigned char outch;
                  
                convert_status = TECConvertText (converter, &ch, 1,
						 &actual_input_length,
                                                 &outch, 1,
						 &actual_output_length);
                if (convert_status == noErr
		    && actual_input_length == 1
		    && actual_output_length == 1)
                  bufp->code = outch;
              }
	  }

	the_modifiers = 0;
	if (er.modifiers & shiftKey)
	  the_modifiers |= shift_modifier;
	if (er.modifiers & controlKey)
	  the_modifiers |= ctrl_modifier;
	/* use option or command key as meta depending on value of
	   mac-command-key-is-meta */
	if (er.modifiers
	    & (NILP (Vmac_command_key_is_meta) ? optionKey : cmdKey))
	  the_modifiers |= meta_modifier;
	bufp->modifiers	= the_modifiers;
				
	{
	  mac_output *mwp = (mac_output *) GetWRefCon (FrontWindow ());
	  XSETFRAME (bufp->frame_or_window, mwp->mFP);
	}

	bufp->timestamp = er.when * (1000 / 60);  /* ticks to milliseconds */

	count++;
	break;

      case kHighLevelEvent:
        drag_and_drop_file_list = Qnil;

        AEProcessAppleEvent(&er);
        
        /* Build a DRAG_N_DROP_EVENT type event as is done in
           constuct_drag_n_drop in w32term.c.  */
        if (!NILP (drag_and_drop_file_list))
          {
            struct frame *f;
            WindowPtr wp;
            Lisp_Object frame;

            wp = FrontWindow ();
            if (!wp)
              f = NULL;
            else
              f = ((mac_output *) GetWRefCon (wp))->mFP;            
            
            bufp->kind = DRAG_N_DROP_EVENT;
            bufp->code = 0;
            bufp->timestamp = er.when * (1000 / 60);
	      /* ticks to milliseconds */
            bufp->modifiers = 0;

            XSETINT (bufp->x, 0);
            XSETINT (bufp->y, 0);

            XSETFRAME (frame, f);
            bufp->frame_or_window = Fcons (frame, drag_and_drop_file_list);

            /* Regardless of whether Emacs was suspended or in the
               foreground, ask it to redraw its entire screen.
               Otherwise parts of the screen can be left in an
               inconsistent state.  */
            if (wp)
#if TARGET_API_MAC_CARBON
	      {
	        Rect r;
	        
	        GetWindowPortBounds (wp, &r);
	        InvalWindowRect (wp, &r);
	      }
#else /* not TARGET_API_MAC_CARBON */
              InvalRect (&(wp->portRect));
#endif /* not TARGET_API_MAC_CARBON */
            
            count++;
          }
        
      default:
	break;
      }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

#if !TARGET_API_MAC_CARBON
  check_alarm ();  /* simulate the handling of a SIGALRM */
#endif

  {
    static Point old_mouse_pos = { -1, -1 };
    
    if (app_is_suspended)
      {
        old_mouse_pos.h = -1;
        old_mouse_pos.v = -1;
      }
    else
      {
        Point mouse_pos;
        WindowPtr wp;
        struct frame *f;
        Lisp_Object bar;
        struct scroll_bar *sb;         

        wp = FrontWindow ();
	if (is_emacs_window (wp))
	  {
	    f = ((mac_output *) GetWRefCon (wp))->mFP;

#if TARGET_API_MAC_CARBON
	    SetPort (GetWindowPort (wp));
#else
	    SetPort (wp);
#endif

	    GetMouse (&mouse_pos);

	    if (!EqualPt (mouse_pos, old_mouse_pos))
	      {
		if (mouse_tracking_in_progress == mouse_tracking_scroll_bar
		    && tracked_scroll_bar)
		  x_scroll_bar_note_movement (tracked_scroll_bar,
					      mouse_pos.v
					      - XINT (tracked_scroll_bar->top),
					      TickCount() * (1000 / 60));
		else
		  note_mouse_movement (f, &mouse_pos);
		
		old_mouse_pos = mouse_pos;            
	      }
	  }
      }
  }
  
  UNBLOCK_INPUT;
  
  return count;
}


/* Need to override CodeWarrior's input function so no conversion is
   done on newlines Otherwise compiled functions in .elc files will be
   read incorrectly.  Defined in ...:MSL C:MSL
   Common:Source:buffer_io.c.  */
#ifdef __MWERKS__
void
__convert_to_newlines (unsigned char * p, size_t * n)
{
#pragma unused(p,n)
}

void
__convert_from_newlines (unsigned char * p, size_t * n)
{
#pragma unused(p,n)
}
#endif


/* Initialize the struct pointed to by MW to represent a new COLS x
   ROWS Macintosh window, using font with name FONTNAME and size
   FONTSIZE.  */
void
NewMacWindow (FRAME_PTR fp)
{
  mac_output *mwp;
#if TARGET_API_MAC_CARBON
  static int making_terminal_window = 0;
#else
  static int making_terminal_window = 1;
#endif

  mwp = fp->output_data.mac;

  if (making_terminal_window)
    {
      if (!(mwp->mWP = GetNewCWindow (TERM_WINDOW_RESOURCE, NULL,
				      (WindowPtr) -1)))
        abort ();
      making_terminal_window = 0;
    }
  else
    if (!(mwp->mWP = GetNewCWindow (WINDOW_RESOURCE, NULL, (WindowPtr) -1)))
      abort ();
  

  SetWRefCon (mwp->mWP, (long) mwp);
    /* so that update events can find this mac_output struct */
  mwp->mFP = fp;  /* point back to emacs frame */

#if TARGET_API_MAC_CARBON
  SetPort (GetWindowPort (mwp->mWP));
#else
  SetPort (mwp->mWP);
#endif

  mwp->fontset = -1;
	
  SizeWindow (mwp->mWP, mwp->pixel_width, mwp->pixel_height, false);
  ShowWindow (mwp->mWP);
	
}


void make_mac_frame (struct frame *f)
{
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_right;
  
  NewMacWindow(f);
  FRAME_BACKGROUND_PIXEL (f) = 0xffffff;
  FRAME_FOREGROUND_PIXEL (f) = 0;

  f->output_data.mac->cursor_pixel = 0;
  f->output_data.mac->border_pixel = 0x00ff00;
  f->output_data.mac->mouse_pixel = 0xff00ff;
  f->output_data.mac->cursor_foreground_pixel = 0x0000ff;

  f->output_data.mac->desired_cursor = FILLED_BOX_CURSOR;

  f->output_data.mac->fontset = -1;
  f->output_data.mac->scroll_bar_foreground_pixel = -1;
  f->output_data.mac->scroll_bar_background_pixel = -1;
  f->output_data.mac->left_pos = 4;
  f->output_data.mac->top_pos = 4;
  f->output_data.mac->border_width = 0;
  f->output_data.mac->explicit_parent = 0;
  
  f->output_data.mac->internal_border_width = 0;

  f->output_method = output_mac;

  f->auto_raise = 1;
  f->auto_lower = 1;
  
  f->new_width = 0;
  f->new_height = 0;
}

void make_mac_terminal_frame (struct frame *f)
{
  Lisp_Object frame;

  XSETFRAME (frame, f);

  f->output_method = output_mac;
  f->output_data.mac = (struct mac_output *)
    xmalloc (sizeof (struct mac_output));
  bzero (f->output_data.mac, sizeof (struct mac_output));
  f->output_data.mac->fontset = -1;
  f->output_data.mac->scroll_bar_foreground_pixel = -1;
  f->output_data.mac->scroll_bar_background_pixel = -1;
  
  XSETFRAME (FRAME_KBOARD (f)->Vdefault_minibuffer_frame, f);

  f->width = 96;
  f->height = 4;

  make_mac_frame (f);

  x_make_gc (f);
  
  /* Need to be initialized for unshow_buffer in window.c.  */
  selected_window = f->selected_window;

  Fmodify_frame_parameters (frame,
                            Fcons (Fcons (Qfont,
                                          build_string ("-*-monaco-medium-r-*--*-90-*-*-*-*-mac-roman")), Qnil));
  Fmodify_frame_parameters (frame,
                            Fcons (Fcons (Qforeground_color,
                                          build_string ("black")), Qnil));
  Fmodify_frame_parameters (frame,
                            Fcons (Fcons (Qbackground_color,
                                          build_string ("white")), Qnil));
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {"-geometry",	".geometry", XrmoptionSepArg, NULL},
  {"-iconic",	".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",	"*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

  {"-T",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",	"*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",	"*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};
#endif /* USE_X_TOOLKIT */

#ifdef MULTI_KBOARD
/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static int
same_x_server (name1, name2)
     char *name1, *name2;
{
  int seen_colon = 0;
  unsigned char *system_name = XSTRING (Vsystem_name)->data;
  int system_name_length = strlen (system_name);
  int length_until_period = 0;

  while (system_name[length_until_period] != 0
	 && system_name[length_until_period] != '.')
    length_until_period++;

  /* Treat `unix' like an empty host name.  */
  if (! strncmp (name1, "unix:", 5))
    name1 += 4;
  if (! strncmp (name2, "unix:", 5))
    name2 += 4;
  /* Treat this host's name like an empty host name.  */
  if (! strncmp (name1, system_name, system_name_length)
      && name1[system_name_length] == ':')
    name1 += system_name_length;
  if (! strncmp (name2, system_name, system_name_length)
      && name2[system_name_length] == ':')
    name2 += system_name_length;
  /* Treat this host's domainless name like an empty host name.  */
  if (! strncmp (name1, system_name, length_until_period)
      && name1[length_until_period] == ':')
    name1 += length_until_period;
  if (! strncmp (name2, system_name, length_until_period)
      && name2[length_until_period] == ':')
    name2 += length_until_period;

  for (; *name1 != '\0' && *name1 == *name2; name1++, name2++)
    {
      if (*name1 == ':')
	seen_colon++;
      if (seen_colon && *name1 == '.')
	return 1;
    }
  return (seen_colon
	  && (*name1 == '.' || *name1 == '\0')
	  && (*name2 == '.' || *name2 == '\0'));
}
#endif

int mac_initialized = 0;

void
mac_initialize_display_info ()
{
  struct mac_display_info *dpyinfo = &one_mac_display_info;
  GDHandle main_device_handle;

  bzero (dpyinfo, sizeof (*dpyinfo));

  /* Put it on x_display_name_list.  */
  x_display_name_list = Fcons (Fcons (build_string ("Mac"), Qnil),
                               x_display_name_list);
  dpyinfo->name_list_element = XCAR (x_display_name_list);
  
#if 0
  dpyinfo->mac_id_name
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
  sprintf (dpyinfo->mac_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);
#else
  dpyinfo->mac_id_name = (char *) xmalloc (strlen ("Mac Display") + 1);
  strcpy (dpyinfo->mac_id_name, "Mac Display");
#endif

  main_device_handle = LMGetMainDevice();

  dpyinfo->reference_count = 0;
  dpyinfo->resx = 75.0;
  dpyinfo->resy = 75.0;
  dpyinfo->n_planes = 1;
  dpyinfo->n_cbits = 16;
  dpyinfo->height = (**main_device_handle).gdRect.bottom;
  dpyinfo->width = (**main_device_handle).gdRect.right;
  dpyinfo->grabbed = 0;
  dpyinfo->root_window = NULL;

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  dpyinfo->mouse_face_window = Qnil;
}

struct mac_display_info *
mac_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  struct mac_display_info *dpyinfo;
  GDHandle main_device_handle;

  if (!mac_initialized)
    {
      mac_initialize ();
      mac_initialized = 1;
    }

  mac_initialize_display_info (display_name);

  dpyinfo = &one_mac_display_info;

  main_device_handle = LMGetMainDevice();

  dpyinfo->height = (**main_device_handle).gdRect.bottom;
  dpyinfo->width = (**main_device_handle).gdRect.right;

  return dpyinfo;
}

/* Set up use of X before we make the first connection.  */

static struct redisplay_interface x_redisplay_interface =
{
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  x_scroll_run,
  x_after_update_window_line,
  x_update_window_begin,
  x_update_window_end,
  XTcursor_to,
  x_flush,
  x_clear_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area
};

void
mac_initialize ()
{
  rif = &x_redisplay_interface;

  clear_frame_hook = x_clear_frame;
  ins_del_lines_hook = x_ins_del_lines;
  delete_glyphs_hook = x_delete_glyphs;
  ring_bell_hook = XTring_bell;
  reset_terminal_modes_hook = XTreset_terminal_modes;
  set_terminal_modes_hook = XTset_terminal_modes;
  update_begin_hook = x_update_begin;
  update_end_hook = x_update_end;
  set_terminal_window_hook = XTset_terminal_window;
  read_socket_hook = XTread_socket;
  frame_up_to_date_hook = XTframe_up_to_date;
  mouse_position_hook = XTmouse_position;
  frame_rehighlight_hook = XTframe_rehighlight;
  frame_raise_lower_hook = XTframe_raise_lower;

  set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  redeem_scroll_bar_hook = XTredeem_scroll_bar;
  judge_scroll_bars_hook = XTjudge_scroll_bars;

  estimate_mode_line_height_hook = x_estimate_mode_line_height;

  scroll_region_ok = 1;         /* we'll scroll partial frames */
  char_ins_del_ok = 1;
  line_ins_del_ok = 1;          /* we'll just blt 'em */
  fast_clear_end_of_line = 1;   /* X does this well */
  memory_below_frame = 0;       /* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  x_noop_count = 0;
  last_tool_bar_item = -1;
  any_help_event_p = 0;
  
  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

#ifdef USE_X_TOOLKIT
  XtToolkitInitialize ();
  Xt_app_con = XtCreateApplicationContext ();
  XtAppSetFallbackResources (Xt_app_con, Xt_default_resources);

  /* Install an asynchronous timer that processes Xt timeout events
     every 0.1s.  This is necessary because some widget sets use
     timeouts internally, for example the LessTif menu bar, or the
     Xaw3d scroll bar.  When Xt timouts aren't processed, these
     widgets don't behave normally.  */
  {
    EMACS_TIME interval;
    EMACS_SET_SECS_USECS (interval, 0, 100000);
    start_atimer (ATIMER_CONTINUOUS, interval, x_process_timeouts, 0);
  }
#endif
  
#if USE_TOOLKIT_SCROLL_BARS
  xaw3d_arrow_scroll = False;
  xaw3d_pick_top = True;
#endif

#if 0
  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_quitter);

  /* Disable Window Change signals;  they are handled by X events.  */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* ! defined (SIGWINCH) */

  signal (SIGPIPE, x_connection_signal);
#endif

  mac_initialize_display_info ();
}


void
syms_of_macterm ()
{
#if 0
  staticpro (&x_error_message_string);
  x_error_message_string = Qnil;
#endif

  staticpro (&x_display_name_list);
  x_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  staticpro (&last_mouse_press_frame);
  last_mouse_press_frame = Qnil;

  Qmac_ready_for_drag_n_drop = intern ("mac-ready-for-drag-n-drop");
  staticpro (&Qmac_ready_for_drag_n_drop);

  help_echo = Qnil;
  staticpro (&help_echo);
  help_echo_object = Qnil;
  staticpro (&help_echo_object);
  help_echo_window = Qnil;
  staticpro (&help_echo_window);
  previous_help_echo = Qnil;
  staticpro (&previous_help_echo);
  help_echo_pos = -1;

  DEFVAR_BOOL ("x-autoselect-window", &x_autoselect_window_p,
    doc: /* *Non-nil means autoselect window with mouse pointer.  */);
  x_autoselect_window_p = 0;

  DEFVAR_BOOL ("x-stretch-cursor", &x_stretch_cursor_p,
    doc: /* *Non-nil means draw block cursor as wide as the glyph under it.
For example, if a block cursor is over a tab, it will be drawn as
wide as that tab on the display.  */);
  x_stretch_cursor_p = 0;

#if 0 /* TODO: Setting underline position from font properties.  */
  DEFVAR_BOOL ("x-use-underline-position-properties",
	       &x_use_underline_position_properties,
    doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.  */);
  x_use_underline_position_properties = 1;
#endif

  DEFVAR_LISP ("x-toolkit-scroll-bars", &Vx_toolkit_scroll_bars,
	       doc: /* If not nil, Emacs uses toolkit scroll bars.  */);
  Vx_toolkit_scroll_bars = Qt;

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;
  
  DEFVAR_LISP ("mac-command-key-is-meta", &Vmac_command_key_is_meta,
    doc: /* Non-nil means that the command key is used as the Emacs meta key.
Otherwise the option key is used.  */);
  Vmac_command_key_is_meta = Qt;

  DEFVAR_INT ("mac-keyboard-text-encoding", &mac_keyboard_text_encoding,
    doc: /* One of the Text Encoding Base constant values defined in the
Basic Text Constants section of Inside Macintosh - Text Encoding
Conversion Manager.  Its value determines the encoding characters
typed at the Mac keyboard (presumed to be in the MacRoman encoding)
will convert into.  E.g., if it is set to kTextEncodingMacRoman (0),
its default value, no conversion takes place.  If it is set to
kTextEncodingISOLatin1 (0x201) or kTextEncodingISOLatin2 (0x202),
characters typed on Mac keyboard are first converted into the
ISO Latin-1 or ISO Latin-2 encoding, respectively before being
passed to Emacs.  Together with Emacs's set-keyboard-coding-system
command, this enables the Mac keyboard to be used to enter non-ASCII
characters directly.  */);
  mac_keyboard_text_encoding = kTextEncodingMacRoman;
}
