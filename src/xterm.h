/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989, 1993, 1994 Free Software Foundation, Inc.

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

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#ifdef USE_X_TOOLKIT
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* foul, but we need this to use our own
				   window inside a widget instead of one 
				   that Xt creates... */
#include <X11/StringDefs.h>
#endif

/* The class of this X application.  */
#define EMACS_CLASS "Emacs"

/* Bookkeeping to distinguish X versions.  */

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
   XSetWMIconName.  
   AIX 3.2 is at least X11R4.  */
#if (!defined AIX) || (defined AIX3_2)
#define HAVE_X11R4
#endif
#endif

#ifdef XlibSpecificationRelease
#if XlibSpecificationRelease >= 5
#define HAVE_X11R5
/* In case someone has X11R5 on AIX 3.1,
   make sure HAVE_X11R4 is defined as well as HAVE_X11R5.  */
#define HAVE_X11R4
#endif
#endif

#define BLACK_PIX_DEFAULT(f) BlackPixel (FRAME_X_DISPLAY (f), \
					 XScreenNumberOfScreen (FRAME_X_SCREEN (f)))
#define WHITE_PIX_DEFAULT(f) WhitePixel (FRAME_X_DISPLAY (f), \
					 XScreenNumberOfScreen (FRAME_X_SCREEN (f)))

#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define FONT_BASE(f)    ((f)->ascent)

#define CHECK_X_FRAME(f, frame)			\
  if (NILP (frame))				\
    f = selected_frame;				\
  else						\
    {						\
      CHECK_LIVE_FRAME (frame, 0);		\
      f = XFRAME (frame);			\
    }						\
  if (! FRAME_X_P (f))

  

/* The mask of events that text windows always want to receive.  This
   does not include mouse movement events.  It is used when the window
   is created (in x_window) and and in selection processing.

   We do include ButtonReleases in this set because Emacs isn't always
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

/* This checks to make sure we have a display.  */
extern void check_x ();

extern struct frame *x_window_to_frame ();

#ifdef USE_X_TOOLKIT
extern struct frame *x_any_window_to_frame ();
extern struct frame *x_top_window_to_frame ();
#endif

extern Visual *select_visual ();

enum text_cursor_kinds {
  filled_box_cursor, hollow_box_cursor, bar_cursor
};

/* This data type is used for the font_table field
   of struct x_display_info.  */

struct font_info
{
  XFontStruct *font;
  char *name;
  char *full_name;
};

/* Structure recording X pixmap and reference count.
   If REFCOUNT is 0 then this record is free to be reused.  */

struct x_bitmap_record
{
  Pixmap pixmap;
  char *file;
  int refcount;
  /* Record some info about this pixmap.  */
  int height, width, depth;
};

/* For each X display, we have a structure that records
   information about it.  */

struct x_display_info
{
  /* Chain of all x_display_info structures.  */
  struct x_display_info *next;
  /* Connection number (normally a file descriptor number).  */
  int connection;
  /* This says how to access this display in Xlib.  */
  Display *display;
  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).
     The same cons cell also appears in x_display_name_list.  */
  Lisp_Object name_list_element;
  /* Number of frames that are on this display.  */
  int reference_count;
  /* The Screen this connection is connected to.  */
  Screen *screen;
  /* The Visual being used for this display.  */
  Visual *visual;
  /* Number of panes on this screen.  */
  int n_planes;
  /* Dimensions of this screen.  */
  int height, width;
  /* Mask of things that cause the mouse to be grabbed.  */
  int grabbed;
  /* Emacs bitmap-id of the default icon bitmap for this frame.
     Or -1 if none has been allocated yet.  */
  int icon_bitmap_id;
  /* The root window of this screen.  */
  Window root_window;
  /* The cursor to use for vertical scroll bars.  */
  Cursor vertical_scroll_bar_cursor;
  /* X Resource data base */
  XrmDatabase xrdb;

  /* A table of all the fonts we have already loaded.  */
  struct font_info *font_table;

  /* The current capacity of x_font_table.  */
  int font_table_size;

  /* Reusable Graphics Context for drawing a cursor in a non-default face. */
  GC scratch_cursor_gc;

  /* These variables describe the range of text currently shown
     in its mouse-face, together with the window they apply to.
     As long as the mouse stays within this range, we need not
     redraw anything on its account.  */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;

  /* 1 if a mouse motion event came and we didn't handle it right away because
     gc was in progress.  */
  int mouse_face_deferred_gc;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero means defer mouse-motion highlighting.  */
  int mouse_face_defer;

  char *x_id_name;

  /* The number of fonts actually stored in x_font_table.
     font_table[n] is used and valid iff 0 <= n < n_fonts.
     0 <= n_fonts <= font_table_size.  */
  int n_fonts;

  /* Pointer to bitmap records.  */
  struct x_bitmap_record *bitmaps;

  /* Allocated size of bitmaps field.  */
  int bitmaps_size;

  /* Last used bitmap index.  */
  int bitmaps_last;

  /* Which modifier keys are on which modifier bits?

     With each keystroke, X returns eight bits indicating which modifier
     keys were held down when the key was pressed.  The interpretation
     of the top five modifier bits depends on what keys are attached
     to them.  If the Meta_L and Meta_R keysyms are on mod5, then mod5
     is the meta bit.

     meta_mod_mask is a mask containing the bits used for the meta key.
     It may have more than one bit set, if more than one modifier bit
     has meta keys on it.  Basically, if EVENT is a KeyPress event,
     the meta key is pressed if (EVENT.state & meta_mod_mask) != 0.

     shift_lock_mask is LockMask if the XK_Shift_Lock keysym is on the
     lock modifier bit, or zero otherwise.  Non-alphabetic keys should
     only be affected by the lock modifier bit if XK_Shift_Lock is in
     use; XK_Caps_Lock should only affect alphabetic keys.  With this
     arrangement, the lock modifier should shift the character if
     (EVENT.state & shift_lock_mask) != 0.  */
  int meta_mod_mask, shift_lock_mask;

  /* These are like meta_mod_mask, but for different modifiers.  */
  int alt_mod_mask, super_mod_mask, hyper_mod_mask;

  /* Communication with window managers.  */
  Atom Xatom_wm_protocols;
  /* Kinds of protocol things we may receive.  */
  Atom Xatom_wm_take_focus;
  Atom Xatom_wm_save_yourself;
  Atom Xatom_wm_delete_window;
  /* Atom for indicating window state to the window manager.  */
  Atom Xatom_wm_change_state;
  /* Other WM communication */
  Atom Xatom_wm_configure_denied; /* When our config request is denied */
  Atom Xatom_wm_window_moved;     /* When the WM moves us.  */
  /* EditRes protocol */
  Atom Xatom_editres;

  /* More atoms, which are selection types.  */
  Atom Xatom_CLIPBOARD, Xatom_TIMESTAMP, Xatom_TEXT, Xatom_DELETE,
  Xatom_MULTIPLE, Xatom_INCR, Xatom_EMACS_TMP, Xatom_TARGETS, Xatom_NULL,
  Xatom_ATOM_PAIR;
#ifdef MULTI_KBOARD
  struct kboard *kboard;
#endif
  int cut_buffers_initialized; /* Whether we're sure they all exist */

  /* The frame (if any) which has the X window that has keyboard focus.
     Zero if none.  This is examined by Ffocus_frame in xfns.c.  Note
     that a mere EnterNotify event can set this; if you need to know the
     last frame specified in a FocusIn or FocusOut event, use
     x_focus_event_frame.  */
  struct frame *x_focus_frame;

  /* The last frame mentioned in a FocusIn or FocusOut event.  This is
     separate from x_focus_frame, because whether or not LeaveNotify
     events cause us to lose focus depends on whether or not we have
     received a FocusIn event for it.  */
  struct frame *x_focus_event_frame;

  /* The frame which currently has the visual highlight, and should get
     keyboard input (other sorts of input have the frame encoded in the
     event).  It points to the X focus frame's selected window's
     frame.  It differs from x_focus_frame when we're using a global
     minibuffer.  */
  struct frame *x_highlight_frame;
};

/* This is a chain of structures for all the X displays currently in use.  */
extern struct x_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of x_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
extern Lisp_Object x_display_name_list;

extern struct x_display_info *x_display_info_for_display ();
extern struct x_display_info *x_display_info_for_name ();

extern struct x_display_info *x_term_init ();

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

  /* Size of the X window in pixels.  */
  int pixel_height, pixel_width;

  /* Height of a line, in pixels.  */
  int line_height;

  /* The tiled border used when the mouse is out of the frame.  */
  Pixmap border_tile;

  /* Here are the Graphics Contexts for the default font.  */
  GC normal_gc;				/* Normal video */
  GC reverse_gc;			/* Reverse video */
  GC cursor_gc;				/* cursor drawing */

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
     Usually this is a window that was made by the window manager,
     but it can be the root window, and it can be explicitly specified
     (see the explicit_parent field, below).  */
  Window parent_desc;

#ifdef USE_X_TOOLKIT
  /* The widget of this screen.  This is the window of a "shell" widget.  */
  Widget widget;
  /* The XmPanedWindows...  */
  Widget column_widget;
  /* The widget of the edit portion of this screen; the window in
     "window_desc" is inside of this.  */
  Widget edit_widget;

  Widget menubar_widget;
#endif

  /* If >=0, a bitmap index.  The indicated bitmap is used for the
     icon. */
  int icon_bitmap;

  XFontStruct *font;

  /* Pixel values used for various purposes.
     border_pixel may be -1 meaning use a gray tile.  */
  unsigned long background_pixel;
  unsigned long foreground_pixel;
  unsigned long cursor_pixel;
  unsigned long border_pixel;
  unsigned long mouse_pixel;
  unsigned long cursor_foreground_pixel;

  /* Descriptor for the cursor in use for this window.  */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor cross_cursor;

  /* The name that was associated with the icon, the last time
     it was refreshed.  Usually the same as the name of the
     buffer in the currently selected window in the frame */
  char *icon_label;

  /* Flag to set when the X window needs to be completely repainted.  */
  int needs_exposure;

  /* What kind of text cursor is drawn in this window right now?
     (If there is no cursor (phys_cursor_x < 0), then this means nothing.)  */
  enum text_cursor_kinds current_cursor;

  /* What kind of text cursor should we draw in the future?
     This should always be filled_box_cursor or bar_cursor.  */
  enum text_cursor_kinds desired_cursor;

  /* Width of bar cursor (if we are using that).  */
  int cursor_width;

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

  /* This is the gravity value for the specified window position.  */
  int win_gravity;

  /* The geometry flags for this window.  */
  int size_hint_flags;

  /* This is the Emacs structure for the X display this frame is on.  */
  struct x_display_info *display_info;

  /* Nonzero means our parent is another application's window
     and was explicitly specified.  */
  char explicit_parent;
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

#define FRAME_FOREGROUND_PIXEL(f) ((f)->display.x->foreground_pixel)
#define FRAME_BACKGROUND_PIXEL(f) ((f)->display.x->background_pixel)
#define FRAME_FONT(f) ((f)->display.x->font)

/* This gives the x_display_info structure for the display F is on.  */
#define FRAME_X_DISPLAY_INFO(f) ((f)->display.x->display_info)

/* This is the `Display *' which frame F is on.  */
#define FRAME_X_DISPLAY(f) (FRAME_X_DISPLAY_INFO (f)->display)

/* This is the `Screen *' which frame F is on.  */
#define FRAME_X_SCREEN(f) (FRAME_X_DISPLAY_INFO (f)->screen)

/* These two really ought to be called FRAME_PIXEL_{WIDTH,HEIGHT}.  */
#define PIXEL_WIDTH(f) ((f)->display.x->pixel_width)
#define PIXEL_HEIGHT(f) ((f)->display.x->pixel_height)

#define FRAME_DESIRED_CURSOR(f) ((f)->display.x->desired_cursor)


/* X-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar {

  /* These fields are shared by all vectors.  */
  EMACS_INT size_from_Lisp_Vector_struct;
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
#define SCROLL_BAR_VEC_SIZE					\
  ((sizeof (struct scroll_bar)					\
    - sizeof (EMACS_INT) - sizeof (struct Lisp_Vector *))	\
   / sizeof (Lisp_Object))

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))


/* Building a 32-bit C integer from two 16-bit lisp integers.  */
#define SCROLL_BAR_PACK(low, high) (XINT (high) << 16 | XINT (low))

/* Setting two lisp integers to the low and high words of a 32-bit C int.  */
#define SCROLL_BAR_UNPACK(low, high, int32) \
  (XSETINT ((low),   (int32)        & 0xffff), \
   XSETINT ((high), ((int32) >> 16) & 0xffff))


/* Extract the X window id of the scroll bar from a struct scroll_bar.  */
#define SCROLL_BAR_X_WINDOW(ptr) \
  ((Window) SCROLL_BAR_PACK ((ptr)->x_window_low, (ptr)->x_window_high))

/* Store a window id in a struct scroll_bar.  */
#define SET_SCROLL_BAR_X_WINDOW(ptr, id) \
  (SCROLL_BAR_UNPACK ((ptr)->x_window_low, (ptr)->x_window_high, (int) id))


/* Return the outside pixel height for a vertical scroll bar HEIGHT
   rows high on frame F.  */
#define VERTICAL_SCROLL_BAR_PIXEL_HEIGHT(f, height) \
  ((height) * (f)->display.x->line_height)

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
#define VERTICAL_SCROLL_BAR_RIGHT_BORDER (2)
#define VERTICAL_SCROLL_BAR_TOP_BORDER (2)
#define VERTICAL_SCROLL_BAR_BOTTOM_BORDER (2)

/* Minimum lengths for scroll bar handles, in pixels.  */
#define VERTICAL_SCROLL_BAR_MIN_HANDLE (5)


/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

   Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */
#define CHAR_TO_PIXEL_ROW(f, row) \
  ((f)->display.x->internal_border_width \
   + (row) * (f)->display.x->line_height)
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
   / (f)->display.x->line_height)
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
