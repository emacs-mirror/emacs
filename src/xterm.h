/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.

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

/* HAVE_X11R4 is defined if we have the features of X11R4.  It should
   be defined when we're using X11R5, since X11R5 has the features of
   X11R4.  If, in the future, we find we need more of these flags
   (HAVE_X11R5, for example), code should always be written to test
   the most recent flag first:

      #ifdef HAVE_X11R5
        ...
      #elif HAVE_X11R4
        ...
      #elif HAVE_X11
        ...
      #endif

   If you ever find yourself writing a "#ifdef HAVE_FOO" clause that
   looks a lot like another one, consider moving the text into a macro
   whose definition is configuration-dependent, but whose usage is
   universal - like the stuff in systime.h.

   It turns out that we can auto-detect whether we're being compiled
   with X11R3 or X11R4 by looking for the flag macros for R4 structure
   members that R3 doesn't have.  */
#ifdef PBaseSize
/* AIX 3.1's X is somewhere between X11R3 and X11R4.  It has
   PBaseSize, but not XWithdrawWindow, XSetWMName, XSetWMNormalHints,
   XSetWMIconName.  */
#ifndef AIX
#define HAVE_X11R4
#endif
#endif

#ifdef XlibSpecificationRelease
#if XlibSpecificationRelease >= 5
#define HAVE_X11R5
#endif
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
#define ROOT_WINDOW RootWindow (x_current_display, DefaultScreen (x_current_display))
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

/* This is the X connection that we are using.  */

extern Display *x_current_display;

extern struct frame *x_window_to_frame ();

/* The frame (if any) which has the X window that has keyboard focus.
   Zero if none.  This is examined by Ffocus_frame in xfns.c */

extern struct frame *x_focus_frame;

#ifdef HAVE_X11
/* Variables associated with the X display screen this emacs is using. */

/* How many screens this X display has. */
extern int x_screen_count;

/* The vendor supporting this X server. */
extern Lisp_Object Vx_vendor;

/* The vendor's release number for this X server. */
extern int x_release;

/* Height of this X screen in pixels. */
extern int x_screen_height;

/* Height of this X screen in millimeters. */
extern int x_screen_height_mm;

/* Width of this X screen in pixels. */
extern int x_screen_width;

/* Width of this X screen in millimeters. */
extern int x_screen_width_mm;

/* Does this X screen do backing store? */
extern Lisp_Object Vx_backing_store;

/* Does this X screen do save-unders? */
extern int x_save_under;

/* Number of planes for this screen. */
extern int x_screen_planes;

/* X Visual type of this screen. */
extern Lisp_Object Vx_screen_visual;

#endif /* HAVE_X11 */

enum text_cursor_kinds {
  filled_box_cursor, hollow_box_cursor, bar_cursor
};

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
  PIX_TYPE cursor_foreground_pixel;

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

  /* What kind of text cursor is drawn in this window right now?
     (If there is no cursor (phys_cursor_x < 0), then this means nothing.)  */
  enum text_cursor_kinds current_cursor;

  /* What kind of text cursor should we draw in the future?
     This should always be filled_box_cursor or bar_cursor.  */
  enum text_cursor_kinds desired_cursor;

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

  /* The size of the extra width currently allotted for vertical
     scroll bars, in pixels.  */
  int vertical_scroll_bar_extra;

  /* Table of parameter faces for this frame.  Any X resources (pixel
     values, fonts) referred to here have been allocated explicitly
     for this face, and should be freed if we change the face.  */
  struct face **param_faces;
  int n_param_faces;

  /* Table of computed faces for this frame.  These are the faces
     whose indexes go into the upper bits of a glyph, computed by
     combining the parameter faces specified by overlays, text
     properties, and what have you.  The X resources mentioned here
     are all shared with parameter faces.  */
  struct face **computed_faces;
  int n_computed_faces;		/* How many are valid */
  int size_computed_faces;	/* How many are allocated */
};

/* Get at the computed faces of an X window frame.  */
#define FRAME_PARAM_FACES(f) ((f)->display.x->param_faces)
#define FRAME_N_PARAM_FACES(f) ((f)->display.x->n_param_faces)
#define FRAME_DEFAULT_PARAM_FACE(f) (FRAME_PARAM_FACES (f)[0])
#define FRAME_MODE_LINE_PARAM_FACE(f) (FRAME_PARAM_FACES (f)[1])

#define FRAME_COMPUTED_FACES(f) ((f)->display.x->computed_faces)
#define FRAME_N_COMPUTED_FACES(f) ((f)->display.x->n_computed_faces)
#define FRAME_SIZE_COMPUTED_FACES(f) ((f)->display.x->size_computed_faces)
#define FRAME_DEFAULT_FACE(f) ((f)->display.x->computed_faces[0])
#define FRAME_MODE_LINE_FACE(f) ((f)->display.x->computed_faces[1])

/* Return the window associated with the frame F.  */
#define FRAME_X_WINDOW(f) ((f)->display.x->window_desc)

/* These two really ought to be called FRAME_PIXEL_{WIDTH,HEIGHT}.  */
#define PIXEL_WIDTH(f) ((f)->display.x->pixel_width)
#define PIXEL_HEIGHT(f) ((f)->display.x->pixel_height)

#define FRAME_DESIRED_CURSOR(f) ((f)->display.x->desired_cursor)


/* When X windows are used, a glyph may be a 16 bit unsigned datum.
   The high order byte is the face number and is used as an index
   in the face table.  A face is a font plus:
    1) the unhighlighted foreground color,
    2) the unhighlighted background color.
   For highlighting, the two colors are exchanged.
   Face number 0 is unused.  The low order byte of a glyph gives
   the character within the font.  All fonts are assumed to be
   fixed width, and to have the same height and width. */

#ifdef HAVE_X11

/* Face declared in dispextern.h */

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


/* X-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar {

  /* These fields are shared by all vectors.  */
  int size_from_Lisp_Vector_struct;
  struct Lisp_Vector *next_from_Lisp_Vector_struct;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* The X window representing this scroll bar.  Since this is a full
     32-bit quantity, we store it split into two 32-bit values.  */
  Lisp_Object x_window_low, x_window_high;

  /* The position and size of the scroll bar in pixels, relative to the
     frame.  */
  Lisp_Object top, left, width, height;

  /* The starting and ending positions of the handle, relative to the
     handle area (i.e. zero is the top position, not
     SCROLL_BAR_TOP_BORDER).  If they're equal, that means the handle
     hasn't been drawn yet.

     These are not actually the locations where the beginning and end
     are drawn; in order to keep handles from becoming invisible when
     editing large files, we establish a minimum height by always
     drawing handle bottoms VERTICAL_SCROLL_BAR_MIN_HANDLE pixels below
     where they would be normally; the bottom and top are in a
     different co-ordinate system.  */
  Lisp_Object start, end;

  /* If the scroll bar handle is currently being dragged by the user,
     this is the number of pixels from the top of the handle to the
     place where the user grabbed it.  If the handle isn't currently
     being dragged, this is Qnil.  */
  Lisp_Object dragging;
};

/* The number of elements a vector holding a struct scroll_bar needs.  */
#define SCROLL_BAR_VEC_SIZE \
  ((sizeof (struct scroll_bar) - sizeof (int) - sizeof (struct Lisp_Vector *)) \
   / sizeof (Lisp_Object))

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XPNTR (vec))


/* Building a 32-bit C integer from two 16-bit lisp integers.  */
#define SCROLL_BAR_PACK(low, high) (XINT (high) << 16 | XINT (low))

/* Setting two lisp integers to the low and high words of a 32-bit C int.  */
#define SCROLL_BAR_UNPACK(low, high, int32) \
  (XSET ((low),  Lisp_Int,  (int32)        & 0xffff), \
   XSET ((high), Lisp_Int, ((int32) >> 16) & 0xffff))


/* Extract the X window id of the scroll bar from a struct scroll_bar.  */
#define SCROLL_BAR_X_WINDOW(ptr) \
  ((Window) SCROLL_BAR_PACK ((ptr)->x_window_low, (ptr)->x_window_high))

/* Store a window id in a struct scroll_bar.  */
#define SET_SCROLL_BAR_X_WINDOW(ptr, id) \
  (SCROLL_BAR_UNPACK ((ptr)->x_window_low, (ptr)->x_window_high, (int) id))


/* Return the outside pixel width for a vertical scroll bar on frame F.  */
#define VERTICAL_SCROLL_BAR_PIXEL_WIDTH(f) (2*FONT_WIDTH ((f)->display.x->font))

/* Return the outside pixel height for a vertical scroll bar HEIGHT
   rows high on frame F.  */
#define VERTICAL_SCROLL_BAR_PIXEL_HEIGHT(f, height) \
  ((height) * FONT_HEIGHT ((f)->display.x->font))

/* Return the inside width of a vertical scroll bar, given the outside
   width.  */
#define VERTICAL_SCROLL_BAR_INSIDE_WIDTH(width) \
  ((width) - VERTICAL_SCROLL_BAR_LEFT_BORDER - VERTICAL_SCROLL_BAR_RIGHT_BORDER)

/* Return the length of the rectangle within which the top of the
   handle must stay.  This isn't equivalent to the inside height,
   because the scroll bar handle has a minimum height.  

   This is the real range of motion for the scroll bar, so when we're
   scaling buffer positions to scroll bar positions, we use this, not
   VERTICAL_SCROLL_BAR_INSIDE_HEIGHT.  */
#define VERTICAL_SCROLL_BAR_TOP_RANGE(height) \
  (VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (height) - VERTICAL_SCROLL_BAR_MIN_HANDLE)

/* Return the inside height of vertical scroll bar, given the outside
   height.  See VERTICAL_SCROLL_BAR_TOP_RANGE too.  */
#define VERTICAL_SCROLL_BAR_INSIDE_HEIGHT(height) \
  ((height) - VERTICAL_SCROLL_BAR_TOP_BORDER - VERTICAL_SCROLL_BAR_BOTTOM_BORDER)


/* Border widths for scroll bars.

   Scroll bar windows don't have any X borders; their border width is
   set to zero, and we redraw borders ourselves.  This makes the code
   a bit cleaner, since we don't have to convert between outside width
   (used when relating to the rest of the screen) and inside width
   (used when sizing and drawing the scroll bar window itself).

   The handle moves up and down/back and forth in a rectangle inset
   from the edges of the scroll bar.  These are widths by which we
   inset the handle boundaries from the scroll bar edges.  */
#define VERTICAL_SCROLL_BAR_LEFT_BORDER (2)
#define VERTICAL_SCROLL_BAR_RIGHT_BORDER (3)
#define VERTICAL_SCROLL_BAR_TOP_BORDER (2)
#define VERTICAL_SCROLL_BAR_BOTTOM_BORDER (2)

/* Minimum lengths for scroll bar handles, in pixels.  */
#define VERTICAL_SCROLL_BAR_MIN_HANDLE (5)


/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

/* Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */
#define CHAR_TO_PIXEL_ROW(f, row) \
  ((f)->display.x->internal_border_width \
   + (row) * FONT_HEIGHT ((f)->display.x->font))
#define CHAR_TO_PIXEL_COL(f, col) \
  ((f)->display.x->internal_border_width \
   + (col) * FONT_WIDTH ((f)->display.x->font))

/* Return the pixel width/height of frame F if it has
   WIDTH columns/HEIGHT rows.  */
#define CHAR_TO_PIXEL_WIDTH(f, width) \
  (CHAR_TO_PIXEL_COL (f, width) \
   + (f)->display.x->vertical_scroll_bar_extra \
   + (f)->display.x->internal_border_width)
#define CHAR_TO_PIXEL_HEIGHT(f, height) \
  (CHAR_TO_PIXEL_ROW (f, height) \
   + (f)->display.x->internal_border_width)


/* Return the row/column (zero-based) of the character cell containing 
   the pixel on FRAME at ROW/COL.  */
#define PIXEL_TO_CHAR_ROW(f, row) \
  (((row) - (f)->display.x->internal_border_width) \
   / FONT_HEIGHT ((f)->display.x->font))
#define PIXEL_TO_CHAR_COL(f, col) \
  (((col) - (f)->display.x->internal_border_width) \
   / FONT_WIDTH ((f)->display.x->font))

/* How many columns/rows of text can we fit in WIDTH/HEIGHT pixels on
   frame F?  */
#define PIXEL_TO_CHAR_WIDTH(f, width) \
  (PIXEL_TO_CHAR_COL (f, ((width) \
			  - (f)->display.x->internal_border_width \
			  - (f)->display.x->vertical_scroll_bar_extra)))
#define PIXEL_TO_CHAR_HEIGHT(f, height) \
  (PIXEL_TO_CHAR_ROW (f, ((height) \
			  - (f)->display.x->internal_border_width)))

/* If a struct input_event has a kind which is selection_request_event
   or selection_clear_event, then its contents are really described
   by this structure.  */

/* For an event of kind selection_request_event,
   this structure really describes the contents.  */
struct selection_input_event
{
  int kind;
  Display *display;
  Window requestor;
  Atom selection, target, property;
  Time time;
};

#define SELECTION_EVENT_DISPLAY(eventp)	\
  (((struct selection_input_event *) (eventp))->display)
#define SELECTION_EVENT_REQUESTOR(eventp)	\
  (((struct selection_input_event *) (eventp))->requestor)
#define SELECTION_EVENT_SELECTION(eventp)	\
  (((struct selection_input_event *) (eventp))->selection)
#define SELECTION_EVENT_TARGET(eventp)	\
  (((struct selection_input_event *) (eventp))->target)
#define SELECTION_EVENT_PROPERTY(eventp)	\
  (((struct selection_input_event *) (eventp))->property)
#define SELECTION_EVENT_TIME(eventp)	\
  (((struct selection_input_event *) (eventp))->time)


/* Interface to the face code functions.  */

/* Create the first two computed faces for a frame -- the ones that
   have GC's.  */
extern void init_frame_faces (/* FRAME_PTR */);

/* Free the resources for the faces associated with a frame.  */
extern void free_frame_faces (/* FRAME_PTR */);

/* Given a computed face, find or make an equivalent display face
   in face_vector, and return a pointer to it.  */
extern struct face *intern_face (/* FRAME_PTR, struct face * */);

/* Given a frame and a face name, return the face's ID number, or
   zero if it isn't a recognized face name.  */
extern int face_name_id_number (/* FRAME_PTR, Lisp_Object */);

/* Return non-zero if FONT1 and FONT2 have the same size bounding box.
   We assume that they're both character-cell fonts.  */
extern int same_size_fonts (/* XFontStruct *, XFontStruct * */);

/* Recompute the GC's for the default and modeline faces.
   We call this after changing frame parameters on which those GC's
   depend.  */
extern void recompute_basic_faces (/* FRAME_PTR */);

/* Return the face ID associated with a buffer position POS.  Store
   into *ENDPTR the next position at which a different face is
   needed.  This does not take account of glyphs that specify their
   own face codes.  F is the frame in use for display, and W is a
   window displaying the current buffer.

   REGION_BEG, REGION_END delimit the region, so it can be highlighted.  */
extern int compute_char_face (/* FRAME_PTR frame,
				 struct window *w,
				 int pos,
				 int region_beg, int region_end,
				 int *endptr */);
/* Return the face ID to use to display a special glyph which selects
   FACE_CODE as the face ID, assuming that ordinarily the face would
   be BASIC_FACE.  F is the frame.  */
extern int compute_glyph_face (/* FRAME_PTR, int */);
