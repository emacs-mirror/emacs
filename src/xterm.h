/* Definitions and headers for communication with X protocol.
   Copyright (C) 1989, 1993, 1994, 1998, 1999, 2000, 2001, 2001
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

#ifdef HAVE_X11R5
/* In case someone has X11R5 on AIX 3.1,
   make sure HAVE_X11R4 is defined as well as HAVE_X11R5.  */
#define HAVE_X11R4
#endif

#ifdef HAVE_X_I18N
#include <X11/Xlocale.h>
#endif

#define BLACK_PIX_DEFAULT(f) BlackPixel (FRAME_X_DISPLAY (f), \
					 XScreenNumberOfScreen (FRAME_X_SCREEN (f)))
#define WHITE_PIX_DEFAULT(f) WhitePixel (FRAME_X_DISPLAY (f), \
					 XScreenNumberOfScreen (FRAME_X_SCREEN (f)))

#define FONT_WIDTH(f)	((f)->max_bounds.width)
#define FONT_HEIGHT(f)	((f)->ascent + (f)->descent)
#define FONT_BASE(f)    ((f)->ascent)

/* The mask of events that text windows always want to receive.  This
   includes mouse movement events, since handling the mouse-font text property
   means that we must track mouse motion all the time.  */

#define STANDARD_EVENT_SET      \
  (KeyPressMask			\
   | ExposureMask		\
   | ButtonPressMask		\
   | ButtonReleaseMask		\
   | PointerMotionMask		\
   | StructureNotifyMask	\
   | FocusChangeMask		\
   | LeaveWindowMask		\
   | EnterWindowMask		\
   | VisibilityChangeMask)

enum text_cursor_kinds
{
  NO_CURSOR = -1,
  FILLED_BOX_CURSOR,
  HOLLOW_BOX_CURSOR,
  BAR_CURSOR
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

  /* Dots per inch of the screen.  */
  double resx, resy;
  
  /* The Visual being used for this display.  */
  Visual *visual;

  /* The colormap being used.  */
  Colormap cmap;
  
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

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /* Reusable Graphics Context for drawing a cursor in a non-default face. */
  GC scratch_cursor_gc;

  /* These variables describe the range of text currently shown in its
     mouse-face, together with the window they apply to.  As long as
     the mouse stays within this range, we need not redraw anything on
     its account.  Rows and columns are glyph matrix positions in
     MOUSE_FACE_WINDOW.  */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_beg_x, mouse_face_beg_y;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_end_x, mouse_face_end_y;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;
  Lisp_Object mouse_face_overlay;

  /* 1 if a mouse motion event came and we didn't handle it right away because
     gc was in progress.  */
  int mouse_face_deferred_gc;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero means defer mouse-motion highlighting.  */
  int mouse_face_defer;

  /* Nonzero means that the mouse highlight should not be shown.  */
  int mouse_face_hidden;

  int mouse_face_image_state;

  char *x_id_name;

  /* The number of fonts actually stored in x_font_table.
     font_table[n] is used and valid iff 0 <= n < n_fonts.  0 <=
     n_fonts <= font_table_size and font_table[i].name != 0.  */
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
  Xatom_COMPOUND_TEXT,
  Xatom_MULTIPLE, Xatom_INCR, Xatom_EMACS_TMP, Xatom_TARGETS, Xatom_NULL,
  Xatom_ATOM_PAIR;

  /* More atoms for font properties.  The last three are private
     properties, see the comments in src/fontset.h.  */
  Atom Xatom_PIXEL_SIZE,
  Xatom_MULE_BASELINE_OFFSET, Xatom_MULE_RELATIVE_COMPOSE,
  Xatom_MULE_DEFAULT_ASCENT;

  /* More atoms for Ghostscript support.  */
  Atom Xatom_DONE, Xatom_PAGE;

  /* Atom used in toolkit scroll bar client messages.  */
  Atom Xatom_Scrollbar;

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

  /* The null pixel used for filling a character background with
     background color of a gc.  */
  Pixmap null_pixel;

  /* The gray pixmap.  */
  Pixmap gray;

  /* Cache of images.  */
  struct image_cache *image_cache;

#ifdef HAVE_X_I18N
  /* XIM (X Input method).  */
  XIM xim;
  XIMStyles *xim_styles;
#endif

  /* If non-null, a cache of the colors in the color map.  Don't
     use this directly, call x_color_cells instead.  */
  XColor *color_cells;
  int ncolor_cells;
};

/* This checks to make sure we have a display.  */
extern void check_x P_ ((void));

extern struct frame *x_window_to_frame P_ ((struct x_display_info *, int));

#ifdef USE_X_TOOLKIT
extern struct frame *x_any_window_to_frame P_ ((struct x_display_info *, int));
extern struct frame *x_non_menubar_window_to_frame P_ ((struct x_display_info *, int));
extern struct frame *x_top_window_to_frame P_ ((struct x_display_info *, int));
#endif

/* This is a chain of structures for all the X displays currently in use.  */
extern struct x_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of x_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
extern Lisp_Object x_display_name_list;

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */
extern Lisp_Object Vx_pixel_size_width_font_regexp;

/* A flag to control how to display unibyte 8-bit character.  */
extern int unibyte_display_via_language_environment;

extern struct x_display_info *x_display_info_for_display P_ ((Display *));
extern struct x_display_info *x_display_info_for_name P_ ((Lisp_Object));

extern struct x_display_info *x_term_init P_ ((Lisp_Object, char *, char *));

extern Lisp_Object x_list_fonts P_ ((struct frame *, Lisp_Object, int, int));
extern void select_visual P_ ((struct x_display_info *));
extern struct font_info *x_get_font_info P_ ((struct frame *f, int));
extern struct font_info *x_load_font P_ ((struct frame *, char *, int));
extern struct font_info *x_query_font P_ ((struct frame *, char *));
extern void x_find_ccl_program P_ ((struct font_info *));

/* Each X frame object points to its own struct x_output object
   in the output_data.x field.  The x_output structure contains
   the information that is specific to X windows.  */

struct x_output
{
  /* Position of the X window (x and y offsets in root window).  */
  int left_pos;
  int top_pos;

  /* Border width of the X window as known by the X window system.  */
  int border_width;

  /* Size of the X window in pixels.  */
  int pixel_height, pixel_width;

  /* Height of menu bar widget, in pixels.
     Zero if not using the X toolkit.
     When using the toolkit, this value is not meaningful
     if the menubar is turned off.  */
  int menubar_height;

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

  /* Default ASCII font of this frame.  */
  XFontStruct *font;

  /* The baseline offset of the default ASCII font.  */
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset;

  /* Pixel values used for various purposes.
     border_pixel may be -1 meaning use a gray tile.  */
  unsigned long background_pixel;
  unsigned long foreground_pixel;
  unsigned long cursor_pixel;
  unsigned long border_pixel;
  unsigned long mouse_pixel;
  unsigned long cursor_foreground_pixel;

  /* Foreground color for scroll bars.  A value of -1 means use the
     default (black for non-toolkit scroll bars).  */
  unsigned long scroll_bar_foreground_pixel;
  
  /* Background color for scroll bars.  A value of -1 means use the
     default (background color of the frame for non-toolkit scroll
     bars).  */
  unsigned long scroll_bar_background_pixel;

  /* Top and bottom shadow colors for 3d toolkit scrollbars.  -1 means
     let the scroll compute them itself.  */
  unsigned long scroll_bar_top_shadow_pixel;
  unsigned long scroll_bar_bottom_shadow_pixel;

  /* Descriptor for the cursor in use for this window.  */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor cross_cursor;
  Cursor hourglass_cursor;
  Cursor horizontal_drag_cursor;

  /* Window whose cursor is hourglass_cursor.  This window is temporarily
     mapped to display an hourglass cursor.  */
  Window hourglass_window;
  
  /* Non-zero means hourglass cursor is currently displayed.  */
  unsigned hourglass_p : 1;

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

  /* The extra width currently allotted for the areas in which
     truncation marks, continuation marks, and overlay arrows are
     displayed.  */
  int left_fringe_width, right_fringe_width;
  int fringe_cols, fringes_extra;

  /* This is the gravity value for the specified window position.  */
  int win_gravity;

  /* The geometry flags for this window.  */
  int size_hint_flags;

  /* This is the Emacs structure for the X display this frame is on.  */
  struct x_display_info *display_info;

  /* This is a button event that wants to activate the menubar.
     We save it here until the command loop gets to think about it.  */
  XEvent *saved_menu_event;

  /* This is the widget id used for this frame's menubar in lwlib.  */
#ifdef USE_X_TOOLKIT
  int id;
#endif

  /* Nonzero means our parent is another application's window
     and was explicitly specified.  */
  char explicit_parent;

  /* Nonzero means tried already to make this frame visible.  */
  char asked_for_visible;

  /* Nonzero if this frame was ever previously visible.  */
  char has_been_visible;

#ifdef HAVE_X_I18N
  /* Input context (currently, this means Compose key handler setup).  */
  XIC xic;
  XIMStyle xic_style;
  XFontSet xic_xfs;
#endif

  /* Relief GCs, colors etc.  */
  struct relief
  {
    GC gc;
    unsigned long pixel;
    int allocated_p;
  }
  black_relief, white_relief;

  /* The background for which the above relief GCs were set up.
     They are changed only when a different background is involved.  */
  unsigned long relief_background;

  /* Xt waits for a ConfigureNotify event from the window manager in
     EmacsFrameSetCharSize when the shell widget is resized.  For some
     window managers like fvwm2 2.2.5 and KDE 2.1 this event doesn't
     arrive for an unknown reason and Emacs hangs in Xt.  If this is
     zero, tell Xt not to wait.  */
  int wait_for_wm;

  /* See enum below */
  int want_fullscreen;

  /* This many pixels are the difference between the outer window (i.e. the
     left of the window manager decoration) and FRAME_X_WINDOW. */
  int x_pixels_diff;

  /* This many pixels are the difference between the outer window (i.e. the
     top of the window manager titlebar) and FRAME_X_WINDOW. */
  int y_pixels_diff;

  /* As x_pixels_diff, but to FRAME_OUTER_WINDOW.  For some reason the
     two might differ by a pixel, depending on WM */
  int x_pixels_outer_diff;
  
  /* As y_pixels_diff, but to FRAME_OUTER_WINDOW.  In the toolkit version,
     these may differ because this does not take into account possible
     menubar.  y_pixels_diff is with menubar height included */
  int y_pixels_outer_diff;
};

enum
{
  /* Values used as a bit mask, BOTH == WIDTH | HEIGHT.  */
  FULLSCREEN_NONE       = 0,
  FULLSCREEN_WIDTH      = 1,
  FULLSCREEN_HEIGHT     = 2,
  FULLSCREEN_BOTH       = 3,
  FULLSCREEN_WAIT       = 4,
  FULLSCREEN_MOVE_WAIT  = 8,
};

/* Return the X window used for displaying data in frame F.  */
#define FRAME_X_WINDOW(f) ((f)->output_data.x->window_desc)

/* Return the outermost X window associated with the frame F.  */
#ifdef USE_X_TOOLKIT
#define FRAME_OUTER_WINDOW(f) ((f)->output_data.x->widget ?             \
                               XtWindow ((f)->output_data.x->widget) :  \
                               FRAME_X_WINDOW (f))
#else
#define FRAME_OUTER_WINDOW(f) (FRAME_X_WINDOW (f))
#endif

#define FRAME_FONT(f) ((f)->output_data.x->font)
#define FRAME_FONTSET(f) ((f)->output_data.x->fontset)
#define FRAME_INTERNAL_BORDER_WIDTH(f) ((f)->output_data.x->internal_border_width)
#define FRAME_MENUBAR_HEIGHT(f) ((f)->output_data.x->menubar_height)
#define FRAME_LINE_HEIGHT(f) ((f)->output_data.x->line_height)

/* Width of the default font of frame F.  Must be defined by each
   terminal specific header.  */
#define FRAME_DEFAULT_FONT_WIDTH(F) 	FONT_WIDTH (FRAME_FONT (F))

/* This gives the x_display_info structure for the display F is on.  */
#define FRAME_X_DISPLAY_INFO(f) ((f)->output_data.x->display_info)

/* This is the `Display *' which frame F is on.  */
#define FRAME_X_DISPLAY(f) (FRAME_X_DISPLAY_INFO (f)->display)

/* This is the `Screen *' which frame F is on.  */
#define FRAME_X_SCREEN(f) (FRAME_X_DISPLAY_INFO (f)->screen)
#define FRAME_X_SCREEN_NUMBER(f) XScreenNumberOfScreen (FRAME_X_SCREEN (f))

/* This is the Visual which frame F is on.  */
#define FRAME_X_VISUAL(f) FRAME_X_DISPLAY_INFO (f)->visual

/* This is the Colormap which frame F uses.  */
#define FRAME_X_COLORMAP(f) FRAME_X_DISPLAY_INFO (f)->cmap

/* This is the 'font_info *' which frame F has.  */
#define FRAME_X_FONT_TABLE(f) (FRAME_X_DISPLAY_INFO (f)->font_table)

/* These two really ought to be called FRAME_PIXEL_{WIDTH,HEIGHT}.  */
#define PIXEL_WIDTH(f) ((f)->output_data.x->pixel_width)
#define PIXEL_HEIGHT(f) ((f)->output_data.x->pixel_height)

#define FRAME_DESIRED_CURSOR(f) ((f)->output_data.x->desired_cursor)

#define FRAME_XIC(f) ((f)->output_data.x->xic)
#define FRAME_X_XIM(f) (FRAME_X_DISPLAY_INFO (f)->xim)
#define FRAME_X_XIM_STYLES(f) (FRAME_X_DISPLAY_INFO (f)->xim_styles)
#define FRAME_XIC_STYLE(f) ((f)->output_data.x->xic_style)
#define FRAME_XIC_FONTSET(f) ((f)->output_data.x->xic_xfs)

/* Value is the smallest width of any character in any font on frame F.  */

#define FRAME_SMALLEST_CHAR_WIDTH(F) \
     FRAME_X_DISPLAY_INFO(F)->smallest_char_width

/* Value is the smallest height of any font on frame F.  */

#define FRAME_SMALLEST_FONT_HEIGHT(F) \
     FRAME_X_DISPLAY_INFO(F)->smallest_font_height

/* Return a pointer to the image cache of frame F.  */

#define FRAME_X_IMAGE_CACHE(F) FRAME_X_DISPLAY_INFO ((F))->image_cache


/* Total width of fringes reserved for drawing truncation bitmaps,
   continuation bitmaps and alike.  The width is in canonical char
   units of the frame.  This must currently be the case because window
   sizes aren't pixel values.  If it weren't the case, we wouldn't be
   able to split windows horizontally nicely.  */

#define FRAME_X_FRINGE_COLS(F)	((F)->output_data.x->fringe_cols)

/* Total width of fringes in pixels.  */

#define FRAME_X_FRINGE_WIDTH(F) ((F)->output_data.x->fringes_extra)

/* Pixel-width of the left and right fringe.  */

#define FRAME_X_LEFT_FRINGE_WIDTH(F) ((F)->output_data.x->left_fringe_width)
#define FRAME_X_RIGHT_FRINGE_WIDTH(F) ((F)->output_data.x->right_fringe_width)



/* X-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar
{
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

/* Extract the X widget of the scroll bar from a struct scroll_bar.
   XtWindowToWidget should be fast enough since Xt uses a hash table
   to map windows to widgets.  */

#define SCROLL_BAR_X_WIDGET(dpy, ptr) \
  XtWindowToWidget (dpy, SCROLL_BAR_X_WINDOW (ptr))

/* Store a widget id in a struct scroll_bar.  */

#define SET_SCROLL_BAR_X_WIDGET(ptr, w)		\
  do {						\
    Window window = XtWindow (w);		\
    SET_SCROLL_BAR_X_WINDOW (ptr, window);	\
} while (0)


/* Return the inside width of a vertical scroll bar, given the outside
   width.  */
#define VERTICAL_SCROLL_BAR_INSIDE_WIDTH(f, width) \
  ((width) \
   - VERTICAL_SCROLL_BAR_LEFT_BORDER \
   - VERTICAL_SCROLL_BAR_RIGHT_BORDER \
   - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2)

/* Return the length of the rectangle within which the top of the
   handle must stay.  This isn't equivalent to the inside height,
   because the scroll bar handle has a minimum height.  

   This is the real range of motion for the scroll bar, so when we're
   scaling buffer positions to scroll bar positions, we use this, not
   VERTICAL_SCROLL_BAR_INSIDE_HEIGHT.  */
#define VERTICAL_SCROLL_BAR_TOP_RANGE(f, height) \
  (VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, height) - VERTICAL_SCROLL_BAR_MIN_HANDLE)

/* Return the inside height of vertical scroll bar, given the outside
   height.  See VERTICAL_SCROLL_BAR_TOP_RANGE too.  */
#define VERTICAL_SCROLL_BAR_INSIDE_HEIGHT(f, height) \
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

/* Trimming off a few pixels from each side prevents
   text from glomming up against the scroll bar */
#define VERTICAL_SCROLL_BAR_WIDTH_TRIM (0)


/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

   Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */
#define CHAR_TO_PIXEL_ROW(f, row) \
  ((f)->output_data.x->internal_border_width \
   + (row) * (f)->output_data.x->line_height)
#define CHAR_TO_PIXEL_COL(f, col) \
  ((f)->output_data.x->internal_border_width \
   + (col) * FONT_WIDTH ((f)->output_data.x->font))

/* Return the pixel width/height of frame F if it has
   WIDTH columns/HEIGHT rows.  */
#define CHAR_TO_PIXEL_WIDTH(f, width) \
  (CHAR_TO_PIXEL_COL (f, width) \
   + (f)->output_data.x->vertical_scroll_bar_extra \
   + (f)->output_data.x->fringes_extra \
   + (f)->output_data.x->internal_border_width)
#define CHAR_TO_PIXEL_HEIGHT(f, height) \
  (CHAR_TO_PIXEL_ROW (f, height) \
   + (f)->output_data.x->internal_border_width)


/* Return the row/column (zero-based) of the character cell containing 
   the pixel on FRAME at ROW/COL.  */
#define PIXEL_TO_CHAR_ROW(f, row) \
  (((row) - (f)->output_data.x->internal_border_width) \
   / (f)->output_data.x->line_height)
#define PIXEL_TO_CHAR_COL(f, col) \
  (((col) - (f)->output_data.x->internal_border_width) \
   / FONT_WIDTH ((f)->output_data.x->font))

/* How many columns/rows of text can we fit in WIDTH/HEIGHT pixels on
   frame F?  */
#define PIXEL_TO_CHAR_WIDTH(f, width) \
  (PIXEL_TO_CHAR_COL (f, ((width) \
			  - (f)->output_data.x->internal_border_width \
			  - (f)->output_data.x->fringes_extra \
			  - (f)->output_data.x->vertical_scroll_bar_extra)))
#define PIXEL_TO_CHAR_HEIGHT(f, height) \
  (PIXEL_TO_CHAR_ROW (f, ((height) \
			  - (f)->output_data.x->internal_border_width)))


/* If a struct input_event has a kind which is selection_request_event
   or selection_clear_event, then its contents are really described
   by this structure.  */

/* For an event of kind selection_request_event,
   this structure really describes the contents.
   **Don't make this struct longer!**
   If it overlaps the frame_or_window field of struct input_event,
   that will cause GC to crash.  */
struct selection_input_event
{
  int kind;
  Display *display;
  /* We spell it with an "o" here because X does.  */
  Window requestor;
  Atom selection, target, property;
  Time time;
};

#define SELECTION_EVENT_DISPLAY(eventp)	\
  (((struct selection_input_event *) (eventp))->display)
/* We spell it with an "o" here because X does.  */
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


struct window;
struct glyph_matrix;
struct frame;
struct input_event;
struct face;
struct image;

/* From xselect.c.  */

void x_handle_selection_notify P_ ((XSelectionEvent *));
void x_handle_property_notify P_ ((XPropertyEvent *));

/* From xfns.c.  */

Lisp_Object display_x_get_resource P_ ((struct x_display_info *,
					Lisp_Object, Lisp_Object,
					Lisp_Object, Lisp_Object));
struct frame *check_x_frame P_ ((Lisp_Object));
EXFUN (Fx_display_color_p, 1);
EXFUN (Fx_display_grayscale_p, 1);
int image_ascent P_ ((struct image *, struct face *));
extern void x_free_gcs P_ ((struct frame *));

/* From xrdb.c.  */

char *x_get_string_resource P_ ((XrmDatabase, char *, char *));
char *x_get_customization_string P_ ((XrmDatabase, char *, char *));
XrmDatabase x_load_resources P_ ((Display *, char *, char *, char *));
int x_get_resource P_ ((XrmDatabase, char *, char *,
			XrmRepresentation, XrmValue *));
void x_delete_display P_ ((struct x_display_info *));
void x_make_frame_visible P_ ((struct frame *));
void x_iconify_frame P_ ((struct frame *));
void x_wm_set_size_hint P_ ((struct frame *, long, int));
void x_set_offset P_ ((struct frame *, int, int, int));
void x_wm_set_icon_position P_ ((struct frame *, int, int));
int x_catch_errors P_ ((Display *));
int x_had_errors_p P_ ((Display *));
void x_uncatch_errors P_ ((Display *, int));
void x_check_errors P_ ((Display *, char *));
int x_text_icon P_ ((struct frame *, char *));
int x_bitmap_icon P_ ((struct frame *, Lisp_Object));
void x_set_window_size P_ ((struct frame *, int, int, int));
void x_wm_set_window_state P_ ((struct frame *, int));
int x_alloc_nearest_color P_ ((struct frame *, Colormap, XColor *));

extern void pixel_to_glyph_coords P_ ((struct frame *, int, int,
				       int *, int *, XRectangle *, int));

/* Defined in xterm.c */

extern void cancel_mouse_face P_ ((struct frame *));
extern void x_scroll_bar_clear P_ ((struct frame *));
extern void x_start_queuing_selection_requests P_ ((Display *));
extern void x_stop_queuing_selection_requests P_ ((Display *));
extern void x_update_cursor P_ ((struct frame *, int));
extern int x_text_icon P_ ((struct frame *, char *));
extern int x_bitmap_icon P_ ((struct frame *, Lisp_Object));
extern int x_catch_errors P_ ((Display *));
extern void x_check_errors P_ ((Display *, char *));
extern int x_had_errors_p P_ ((Display *));
extern void x_uncatch_errors P_ ((Display *, int));
extern Lisp_Object x_new_font P_ ((struct frame *, char *));
extern Lisp_Object x_new_fontset P_ ((struct frame *, char *));
extern void x_compute_fringe_widths P_ ((struct frame *, int));
extern void x_set_offset P_ ((struct frame *, int, int, int));
extern void x_set_window_size P_ ((struct frame *, int, int, int));
extern void x_set_mouse_position P_ ((struct frame *, int, int));
extern void x_set_mouse_pixel_position P_ ((struct frame *, int, int));
extern void x_raise_frame P_ ((struct frame *));
extern void x_lower_frame P_ ((struct frame *));
extern void x_make_frame_visible P_ ((struct frame *));
extern void x_make_frame_invisible P_ ((struct frame *));
extern void x_iconify_frame P_ ((struct frame *));
extern void x_free_frame_resources P_ ((struct frame *));
extern void x_destroy_window P_ ((struct frame *));
extern void x_wm_set_size_hint P_ ((struct frame *, long, int));
extern void x_wm_set_window_state P_ ((struct frame *, int));
extern void x_wm_set_icon_pixmap P_ ((struct frame *, int));
extern void x_wm_set_icon_position P_ ((struct frame *, int, int));
extern void x_delete_display P_ ((struct x_display_info *));
extern void x_initialize P_ ((void));
extern void x_display_cursor P_ ((struct window *, int, int, int, int, int));
extern unsigned long x_copy_color P_ ((struct frame *, unsigned long));
#ifdef USE_X_TOOLKIT
extern XtAppContext Xt_app_con;
#endif
extern void x_query_colors P_ ((struct frame *f, XColor *, int));
extern void x_query_color P_ ((struct frame *f, XColor *));
extern void x_clear_area P_ ((Display *, Window, int, int, int, int, int));

extern void x_fullscreen_adjust P_ ((struct frame *f, int *, int *,
                                     int *, int *));


/* Defined in xselect.c */

extern void x_handle_property_notify P_ ((XPropertyEvent *));
extern void x_handle_selection_notify P_ ((XSelectionEvent *));
extern void x_handle_selection_request P_ ((struct input_event *));
extern void x_handle_selection_clear P_ ((struct input_event *));
extern void x_clear_frame_selections P_ ((struct frame *));

/* Defined in xfns.c */

extern int have_menus_p P_ ((void));
extern int x_bitmap_height P_ ((struct frame *, int));
extern int x_bitmap_width P_ ((struct frame *, int));
extern int x_bitmap_pixmap P_ ((struct frame *, int));
extern void x_reference_bitmap P_ ((struct frame *, int));
extern int x_create_bitmap_from_data P_ ((struct frame *, char *,
					  unsigned int, unsigned int));
extern int x_create_bitmap_from_file P_ ((struct frame *, Lisp_Object));
extern void x_destroy_bitmap P_ ((struct frame *, int));
extern void x_set_frame_parameters P_ ((struct frame *, Lisp_Object));
extern void x_real_positions P_ ((struct frame *, int *, int *));
extern void x_report_frame_params P_ ((struct frame *, Lisp_Object *));
extern int defined_color P_ ((struct frame *, char *, XColor *, int));
extern void x_set_border_pixel P_ ((struct frame *, int));
extern void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
extern unsigned char * x_encode_text P_ ((Lisp_Object, Lisp_Object, int,
					  int *, int *));
extern void x_implicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
extern void create_frame_xic P_ ((struct frame *));
extern void destroy_frame_xic P_ ((struct frame *));
extern void xic_set_preeditarea P_ ((struct window *, int, int));
extern void xic_set_statusarea P_ ((struct frame *));
extern void xic_set_xfontset P_ ((struct frame *, char *));
extern int x_pixel_width P_ ((struct frame *));
extern int x_pixel_height P_ ((struct frame *));
extern int x_char_width P_ ((struct frame *));
extern int x_char_height P_ ((struct frame *));
extern int x_screen_planes P_ ((struct frame *));
extern void x_sync P_ ((struct frame *));
extern enum text_cursor_kinds x_specified_cursor_type P_ ((Lisp_Object, int *));
extern int x_defined_color P_ ((struct frame *, char *, XColor *, int));
#ifdef HAVE_X_I18N
extern void free_frame_xic P_ ((struct frame *));
#endif
extern void x_set_tool_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));

/* Defined in xfaces.c */

extern int frame_update_line_height P_ ((struct frame *));
extern int compute_glyph_face P_ ((struct frame *, int, int));
extern int compute_glyph_face_1 P_ ((struct frame *, Lisp_Object, int));
extern void x_free_dpy_colors P_ ((Display *, Screen *, Colormap,
				   unsigned long *, int));

/* Defined in xmenu.c */

extern void x_activate_menubar P_ ((struct frame *));
extern int popup_activated P_ ((void));
extern void initialize_frame_menubar P_ ((struct frame *));

/* Defined in widget.c */

#ifdef USE_X_TOOLKIT
extern void widget_store_internal_border P_ ((Widget));
#endif

/* Defined in xsmfns.c */
#ifdef HAVE_X_SM
extern void x_session_initialize P_ ((void));
extern int x_session_check_input P_ ((struct input_event *bufp,
                                      int *numchars));
extern int x_session_have_connection P_ ((void));
#endif
