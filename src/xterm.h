/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_X11
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#else
#include <X/Xlib.h>
#endif /* HAVE_X11 */

/* Define a queue for X-events.  One such queue is used for mouse clicks.
   Another is used for expose events.  */

#define EVENT_BUFFER_SIZE 64

/* Max and Min sizes in character columns. */
#define MINWIDTH 10
#define MINHEIGHT 10
#define MAXWIDTH 300
#define MAXHEIGHT 80

#ifdef HAVE_X11

/* It turns out that we can auto-detect whether we're being compiled
   with X11R3 or X11R4 by looking for the flag macros for R4 structure
   members that R3 doesn't have.  */
#ifdef PBaseSize
#define HAVE_X11R4
#endif

#define PIX_TYPE unsigned long
#define XDISPLAY x_current_display,
#define XFlushQueue() XFlush(x_current_display)
#define BLACK_PIX_DEFAULT BlackPixel (x_current_display, \
				      XDefaultScreen (x_current_display))
#define WHITE_PIX_DEFAULT WhitePixel (x_current_display, \
				      XDefaultScreen (x_current_display))
#define DISPLAY_SCREEN_ARG x_current_display, \
				      XDefaultScreen (x_current_display)
#define DISPLAY_CELLS DisplayCells (x_current_display, XDefaultScreen (x_current_display))
#define ROOT_WINDOW RootWindow (x_current_display, XDefaultScreen (x_current_display))
#define FONT_TYPE XFontStruct
#define Color XColor

#define XExposeRegionEvent XExposeEvent
#define Bitmap Pixmap			/* In X11, Bitmaps are are kind of
					   Pixmap. */
#define WINDOWINFO_TYPE XWindowAttributes
#define XGetWindowInfo(w, i) XGetWindowAttributes (x_current_display, \
						   (w), (i))
#define XGetFont(f) XLoadQueryFont (x_current_display, (f))
#define XLoseFont(f) XFreeFont (x_current_display, (f))
#define XStuffPending() XPending (x_current_display)
#define XClear(w) XClearWindow (x_current_display, (w))
#define XWarpMousePointer(w,x,y) XWarpPointer (x_current_display, None, w, \
					       0,0,0,0, x, y)
#define XHandleError XSetErrorHandler
#define XHandleIOError XSetIOErrorHandler

#define XChangeWindowSize(w,x,y) XResizeWindow(x_current_display,w,x,y)

#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define FONT_BASE(f)    ((f)->ascent)

/* GC values used for drawing non-standard (other face) text. */
extern XGCValues face_gc_values;				      

/* The mask of events that text windows always want to receive.  This
   does not include mouse movement events.  It is used when the window
   is created (in x_window) and when we ask/unask for mouse movement
   events (in XTmouse_tracking_enable).

   We do include ButtonReleases in this set because elisp isn't always
   fast enough to catch them when it wants them, and they're rare
   enough that they don't use much processor time.  */

#define STANDARD_EVENT_SET      \
  (KeyPressMask			\
   | ExposureMask		\
   | ButtonPressMask		\
   | ButtonReleaseMask		\
   | PointerMotionMask		\
   | PointerMotionHintMask	\
   | StructureNotifyMask	\
   | FocusChangeMask		\
   | LeaveWindowMask		\
   | EnterWindowMask		\
   | VisibilityChangeMask)

#else	/* X10 */

#define ConnectionNumber(dpy) dpyno()
#define PIX_TYPE int
#define XDISPLAY
#define XFlushQueue() XFlush()
#define BLACK_PIX_DEFAULT BlackPixel
#define WHITE_PIX_DEFAULT WhitePixel
#define DISPLAY_SCREEN_ARG
#define DISPLAY_CELLS DisplayCells ()
#define ROOT_WINDOW RootWindow
#define XFree free
#define FONT_TYPE FontInfo

#define WINDOWINFO_TYPE WindowInfo
#define XGetWindowInfo(w, i) XQueryWindow ((w), (i))
#define XGetFont(f) XOpenFont ((f))
#define XLoseFont(f) XCloseFont ((f))
#define XStuffPending() XPending ()
#define XWarpMousePointer(w,x,y) XWarpMouse (w,x,y)
#define XHandleError XErrorHandler
#define XHandleIOError XIOErrorHandler

#define FONT_WIDTH(f)	((f)->width)
#define FONT_HEIGHT(f)	((f)->height)
#define FONT_BASE(f)    ((f)->base)

#define XChangeWindowSize(w,x,y) XChangeWindow(w,x,y)

#endif /* X10 */

struct event_queue
  {
    int rindex;			/* Index at which to fetch next. */
    int windex;			/* Index at which to store next.  */
    XEvent xrep[EVENT_BUFFER_SIZE];
  };

/* Queue for mouse clicks.  */
extern struct event_queue x_mouse_queue;

/* Mechanism for interlocking between main program level
   and input interrupt level.  */

/* Nonzero during a critical section.  At such a time, an input interrupt
   does nothing but set `x_pending_input'.  */
extern int x_input_blocked;

/* Nonzero means an input interrupt has arrived
   during the current critical section.  */
extern int x_pending_input;

/* Begin critical section. */
#define BLOCK_INPUT (x_input_blocked++)

/* End critical section. */
#define UNBLOCK_INPUT \
  (x_input_blocked--, (x_input_blocked < 0 ? (abort (), 0) : 0))

#define TOTALLY_UNBLOCK_INPUT (x_input_blocked = 0)
#define UNBLOCK_INPUT_RESIGNAL UNBLOCK_INPUT

/* This is the X connection that we are using.  */

extern Display *x_current_display;

extern struct frame *x_window_to_frame ();

/* The frame (if any) which has the X window that has keyboard focus.
   Zero if none.  This is examined by Ffocus_frame in xfns.c */

struct frame *x_focus_frame;

#ifdef HAVE_X11
/* Variables associated with the X display screen this emacs is using. */

/* How many screens this X display has. */
extern Lisp_Object x_screen_count;

/* The vendor supporting this X server. */
extern Lisp_Object Vx_vendor;

/* The vendor's release number for this X server. */
extern Lisp_Object x_release;

/* Height of this X screen in pixels. */
extern Lisp_Object x_screen_height;

/* Height of this X screen in millimeters. */
extern Lisp_Object x_screen_height_mm;

/* Width of this X screen in pixels. */
extern Lisp_Object x_screen_width;

/* Width of this X screen in millimeters. */
extern Lisp_Object x_screen_width_mm;

/* Does this X screen do backing store? */
extern Lisp_Object Vx_backing_store;

/* Does this X screen do save-unders? */
extern Lisp_Object x_save_under;

/* Number of planes for this screen. */
extern Lisp_Object x_screen_planes;

/* X Visual type of this screen. */
extern Lisp_Object Vx_screen_visual;

#endif /* HAVE_X11 */

enum text_cursor_kinds {
  filled_box_cursor, hollow_box_cursor, bar_cursor
};

#define PIXEL_WIDTH(s) ((s)->display.x->pixel_width)
#define PIXEL_HEIGHT(s) ((s)->display.x->pixel_height)

/* Each X frame object points to its own struct x_display object
   in the display.x field.  The x_display structure contains all
   the information that is specific to X windows.  */

struct x_display
{
  /* Position of the X window (x and y offsets in root window).  */
  int left_pos;
  int top_pos;

  /* Border width of the X window as known by the X window system.  */
  int border_width;

  /* Size of the X window in pixels. */
  int pixel_height, pixel_width;

#ifdef HAVE_X11
  /* The tiled border used when the mouse is out of the frame. */
  Pixmap border_tile;

  /* Here are the Graphics Contexts for the default font. */
  GC normal_gc;				/* Normal video */
  GC reverse_gc;			/* Reverse video */
  GC cursor_gc;				/* cursor drawing */
#endif /* HAVE_X11 */

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  When the frame is selected,
     a highlighting is displayed inside the internal border.  */
  int internal_border_width;

  /* The X window used for this frame.
     May be zero while the frame object is being created
     and the X window has not yet been created.  */
  Window window_desc;

  /* The X window used for the bitmap icon;
     or 0 if we don't have a bitmap icon.  */
  Window icon_desc;

  /* The X window that is the parent of this X window.
     Usually but not always RootWindow.  */
  Window parent_desc;

  /* 1 for bitmap icon, 0 for text icon.  */
  int icon_bitmap_flag;

  FONT_TYPE *font;

  /* Pixel values used for various purposes.
     border_pixel may be -1 meaning use a gray tile.  */
  PIX_TYPE background_pixel;
  PIX_TYPE foreground_pixel;
  PIX_TYPE cursor_pixel;
  PIX_TYPE border_pixel;
  PIX_TYPE mouse_pixel;

  /* Windows for scrollbars */
  Window v_scrollbar;
  Window v_thumbup;
  Window v_thumbdown;
  Window v_slider;

  Window h_scrollbar;
  Window h_thumbleft;
  Window h_thumbright;
  Window h_slider;

  /* Scrollbar info */

  int v_scrollbar_width;
  int h_scrollbar_height;

  /* Descriptor for the cursor in use for this window.  */
#ifdef HAVE_X11
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
#else
  Cursor cursor;
#endif

  /* The name that was associated with the icon, the last time
     it was refreshed.  Usually the same as the name of the
     buffer in the currently selected window in the frame */
  char *icon_label;

  /* Flag to set when the X window needs to be completely repainted. */
  int needs_exposure;

  /* What kind of text cursor is drawn in this window right now?  (If
     there is no cursor (phys_cursor_x < 0), then this means nothing.  */
  enum text_cursor_kinds text_cursor_kind;

  /* These are the current window manager hints.  It seems that
     XSetWMHints, when presented with an unset bit in the `flags'
     member of the hints structure, does not leave the corresponding
     attribute unchanged; rather, it resets that attribute to its
     default value.  For example, unless you set the `icon_pixmap'
     field and the `IconPixmapHint' bit, XSetWMHints will forget what
     your icon pixmap was.  This is rather troublesome, since some of
     the members (for example, `input' and `icon_pixmap') want to stay
     the same throughout the execution of Emacs.  So, we keep this
     structure around, just leaving values in it and adding new bits
     to the mask as we go.  */
  XWMHints wm_hints;
};

/* When X windows are used, a glyf may be a 16 bit unsigned datum.
   The high order byte is the face number and is used as an index
   in the face table.  A face is a font plus:
    1) the unhighlighted foreground color,
    2) the unhighlighted background color.
   For highlighting, the two colors are exchanged.
   Face number 0 is unused.  The low order byte of a glyf gives
   the character within the font.  All fonts are assumed to be
   fixed width, and to have the same height and width. */

#ifdef HAVE_X11
/* Table of GC's used for this frame. */
GC *gc_table;

/* How many GCs are in the table. */
int gcs_in_use;

struct face
{
  GC face_gc;
  unsigned int foreground;
  unsigned int background;
  Pixmap stipple;
  XFontStruct *font;
};

#else	/* X10 */

struct face
{
  FONT_TYPE *font;	/* Font info for specified font. */
  int  fg;		/* Unhighlighted foreground. */
  int  bg;		/* Unhighlighted background. */
};
#endif	/* X10 */

#define MAX_FACES_AND_GLYPHS 256
extern struct face *x_face_table[];
