/* Definitions and headers for communication with pure Gtk+3.
   Copyright (C) 1989, 1993, 2005, 2008-2017 Free Software Foundation,
   Inc.

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


#include "dispextern.h"
#include "frame.h"
#include "character.h"
#include "font.h"
#include "sysselect.h"

#ifdef HAVE_PGTK

#include <gtk/gtk.h>

// #define PGTK_DEBUG 1

#ifdef PGTK_DEBUG
extern void pgtk_log(const char *file, int lineno, const char *fmt, ...)
  ATTRIBUTE_FORMAT_PRINTF (3, 4);
#define PGTK_TRACE(fmt, ...) pgtk_log(__FILE__, __LINE__, fmt, ## __VA_ARGS__)
extern void pgtk_backtrace(const char *file, int lineno);
#define PGTK_BACKTRACE() pgtk_backtrace(__FILE__, __LINE__)
#else
#define PGTK_TRACE(fmt, ...) ((void) 0)
#define PGTK_BACKTRACE() ((void) 0)
#endif

/* could use list to store these, but rest of emacs has a big infrastructure
   for managing a table of bitmap "records" */
struct pgtk_bitmap_record
{
  void *img;
  char *file;
  int refcount;
  int height, width, depth;
  cairo_pattern_t *pattern;
};

#define RGB_TO_ULONG(r, g, b) (((r) << 16) | ((g) << 8) | (b))
#define ARGB_TO_ULONG(a, r, g, b) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b))

#define ALPHA_FROM_ULONG(color) ((color) >> 24)
#define RED_FROM_ULONG(color) (((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color) (((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color) ((color) & 0xff)

struct scroll_bar
{
  /* These fields are shared by all vectors.  */
  union vectorlike_header header;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* Fields from `x_window' down will not be traced by the GC.  */

  /* The X window representing this scroll bar.  */
  Window x_window;

  /* The position and size of the scroll bar in pixels, relative to the
     frame.  */
  int top, left, width, height;

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
  int start, end;

  /* If the scroll bar handle is currently being dragged by the user,
     this is the number of pixels from the top of the handle to the
     place where the user grabbed it.  If the handle isn't currently
     being dragged, this is -1.  */
  int dragging;

#if defined (USE_TOOLKIT_SCROLL_BARS) && defined (USE_LUCID)
  /* Last scroll bar part seen in xaw_jump_callback and xaw_scroll_callback.  */
  enum scroll_bar_part last_seen_part;
#endif

#if defined (USE_TOOLKIT_SCROLL_BARS) && !defined (USE_GTK)
  /* Last value of whole for horizontal scrollbars.  */
  int whole;
#endif

  /* True if the scroll bar is horizontal.  */
  bool horizontal;
};


/* init'd in pgtk_initialize_display_info () */
struct pgtk_display_info
{
  /* Chain of all pgtk_display_info structures.  */
  struct pgtk_display_info *next;

  /* The generic display parameters corresponding to this PGTK display. */
  struct terminal *terminal;

  /* This says how to access this display in Gdk.  */
  GdkDisplay *gdpy;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).  */
  Lisp_Object name_list_element;

  /* Number of frames that are on this display.  */
  int reference_count;

  /* Logical identifier of this display.  */
  unsigned x_id;

  /* Default name for all frames on this display.  */
  char *x_id_name;

  /* The number of fonts loaded. */
  int n_fonts;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  struct pgtk_bitmap_record *bitmaps;
  ptrdiff_t bitmaps_size;
  ptrdiff_t bitmaps_last;

  /* DPI resolution of this screen */
  double resx, resy;

  /* Mask of things that cause the mouse to be grabbed */
  int grabbed;

  int n_planes;

  int color_p;

  Window root_window;

  /* Xism */
  XrmDatabase rdb;

  /* The cursor to use for vertical scroll bars. */
  Emacs_Cursor vertical_scroll_bar_cursor;

  /* The cursor to use for horizontal scroll bars. */
  Emacs_Cursor horizontal_scroll_bar_cursor;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  struct frame *highlight_frame;
  struct frame *x_focus_frame;

  /* The last frame mentioned in a FocusIn or FocusOut event.  This is
     separate from x_focus_frame, because whether or not LeaveNotify
     events cause us to lose focus depends on whether or not we have
     received a FocusIn event for it.  */
  struct frame *x_focus_event_frame;

  /* The frame where the mouse was last time we reported a mouse event.  */
  struct frame *last_mouse_frame;

  /* The frame where the mouse was last time we reported a mouse motion.  */
  struct frame *last_mouse_motion_frame;

  /* Position where the mouse was last time we reported a motion.
     This is a position on last_mouse_motion_frame.  */
  int last_mouse_motion_x;
  int last_mouse_motion_y;

  /* Where the mouse was last time we reported a mouse position.  */
  XRectangle last_mouse_glyph;

  /* Time of last mouse movement.  */
  Time last_mouse_movement_time;

  /* The scroll bar in which the last motion event occurred.  */
  void *last_mouse_scroll_bar;

  /* The invisible cursor used for pointer blanking.
     Unused if this display supports Xfixes extension.  */
  Emacs_Cursor invisible_cursor;

  /* Function used to toggle pointer visibility on this display.  */
  void (*toggle_visible_pointer) (struct frame *, bool);

  /* The GDK cursor for scroll bars and popup menus.  */
  GdkCursor *xg_cursor;


  /* The frame where the mouse was last time we reported a mouse position.  */
  struct frame *last_mouse_glyph_frame;

  /* Modifier masks in gdk */
  int meta_mod_mask, alt_mod_mask;

  /* The last click event. */
  GdkEvent *last_click_event;

  /* input method */
  struct {
    GtkIMContext *context;
    struct frame *focused_frame;
  } im;
};

/* This is a chain of structures for all the PGTK displays currently in use.  */
extern struct pgtk_display_info *x_display_list;

struct pgtk_output
{
#if 0
  void *view;
  void *miniimage;
#endif
  unsigned long foreground_color;
  unsigned long background_color;
  void *toolbar;

  /* Cursors */
  Emacs_Cursor current_cursor;
  Emacs_Cursor text_cursor;
  Emacs_Cursor nontext_cursor;
  Emacs_Cursor modeline_cursor;
  Emacs_Cursor hand_cursor;
  Emacs_Cursor hourglass_cursor;
  Emacs_Cursor horizontal_drag_cursor;
  Emacs_Cursor vertical_drag_cursor;
  Emacs_Cursor left_edge_cursor;
  Emacs_Cursor top_left_corner_cursor;
  Emacs_Cursor top_edge_cursor;
  Emacs_Cursor top_right_corner_cursor;
  Emacs_Cursor right_edge_cursor;
  Emacs_Cursor bottom_right_corner_cursor;
  Emacs_Cursor bottom_edge_cursor;
  Emacs_Cursor bottom_left_corner_cursor;

  /* PGTK-specific */
  Emacs_Cursor current_pointer;

  /* border color */
  unsigned long border_pixel;
  GtkCssProvider *border_color_css_provider;

  /* scrollbar color */
  GtkCssProvider *scrollbar_foreground_css_provider;
  GtkCssProvider *scrollbar_background_css_provider;

  /* Widget whose cursor is hourglass_cursor.  This widget is temporarily
     mapped to display an hourglass cursor.  */
  GtkWidget *hourglass_widget;

  Emacs_GC cursor_xgcv;

  /* lord knows why Emacs needs to know about our Window ids.. */
  Window window_desc, parent_desc;
  char explicit_parent;

  struct font *font;
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset; /* only used with font_backend */

  unsigned long mouse_color;
  unsigned long cursor_color;
  unsigned long cursor_foreground_color;

  int icon_top;
  int icon_left;

  /* The size of the extra width currently allotted for vertical
     scroll bars, in pixels.  */
  int vertical_scroll_bar_extra;

  /* The height of the titlebar decoration (included in PGTKWindow's frame). */
  int titlebar_height;

  /* The height of the toolbar if displayed, else 0. */
  int toolbar_height;

  /* This is the Emacs structure for the PGTK display this frame is on.  */
  struct pgtk_display_info *display_info;

  /* Non-zero if we are zooming (maximizing) the frame.  */
  int zooming;

  /* Non-zero if we are doing an animation, e.g. toggling the tool bar. */
  int in_animation;

  /* The last size hints set.  */
  GdkGeometry size_hints;
  long hint_flags;
  int preferred_width, preferred_height;

  /* The widget of this screen.  This is the window of a top widget.  */
  GtkWidget *widget;
  /* The widget of the edit portion of this screen; the window in
     "window_desc" is inside of this.  */
  GtkWidget *edit_widget;
  /* The widget used for laying out widgets vertically.  */
  GtkWidget *vbox_widget;
  /* The widget used for laying out widgets horizontally.  */
  GtkWidget *hbox_widget;
  /* The menubar in this frame.  */
  GtkWidget *menubar_widget;
  /* The tool bar in this frame  */
  GtkWidget *toolbar_widget;
  /* True if tool bar is packed into the hbox widget (i.e. vertical).  */
  bool_bf toolbar_in_hbox : 1;
  bool_bf toolbar_is_packed : 1;

  GtkTooltip *ttip_widget;
  GtkWidget *ttip_lbl;
  GtkWindow *ttip_window;

  /* Height of menu bar widget, in pixels.  This value
     is not meaningful if the menubar is turned off.  */
  int menubar_height;

  /* Height of tool bar widget, in pixels.  top_height is used if tool bar
     at top, bottom_height if tool bar is at the bottom.
     Zero if not using an external tool bar or if tool bar is vertical.  */
  int toolbar_top_height, toolbar_bottom_height;

  /* Width of tool bar widget, in pixels.  left_width is used if tool bar
     at left, right_width if tool bar is at the right.
     Zero if not using an external tool bar or if tool bar is horizontal.  */
  int toolbar_left_width, toolbar_right_width;

#ifdef USE_CAIRO
  /* Cairo drawing contexts.  */
  cairo_t *cr_context, *cr_active;
  int cr_surface_desired_width, cr_surface_desired_height;
  /* Cairo surface for double buffering */
  cairo_surface_t *cr_surface_visible_bell;
#endif
  struct atimer *atimer_visible_bell;

  int has_been_visible;

  /* Relief GCs, colors etc.  */
  struct relief
  {
    Emacs_GC xgcv;
    unsigned long pixel;
  }
  black_relief, white_relief;

  /* The background for which the above relief GCs were set up.
     They are changed only when a different background is involved.  */
  unsigned long relief_background;

  /* Keep track of focus.  May be EXPLICIT if we received a FocusIn for this
     frame, or IMPLICIT if we received an EnterNotify.
     FocusOut and LeaveNotify clears EXPLICIT/IMPLICIT. */
  int focus_state;
};

/* this dummy decl needed to support TTYs */
struct x_output
{
  int unused;
};

enum
{
  /* Values for focus_state, used as bit mask.
     EXPLICIT means we received a FocusIn for the frame and know it has
     the focus.  IMPLICIT means we received an EnterNotify and the frame
     may have the focus if no window manager is running.
     FocusOut and LeaveNotify clears EXPLICIT/IMPLICIT. */
  FOCUS_NONE     = 0,
  FOCUS_IMPLICIT = 1,
  FOCUS_EXPLICIT = 2
};

/* This gives the pgtk_display_info structure for the display F is on.  */
#define FRAME_X_OUTPUT(f)         ((f)->output_data.pgtk)
#define FRAME_OUTPUT_DATA(f)      FRAME_X_OUTPUT (f)

#define FRAME_DISPLAY_INFO(f)     (FRAME_X_OUTPUT(f)->display_info)
#define FRAME_FOREGROUND_COLOR(f) (FRAME_X_OUTPUT(f)->foreground_color)
#define FRAME_BACKGROUND_COLOR(f) (FRAME_X_OUTPUT(f)->background_color)
#define FRAME_CURSOR_COLOR(f)     (FRAME_X_OUTPUT(f)->cursor_color)
#define FRAME_POINTER_TYPE(f)     (FRAME_X_OUTPUT(f)->current_pointer)
#define FRAME_FONT(f)             (FRAME_X_OUTPUT(f)->font)
#define FRAME_GTK_OUTER_WIDGET(f) (FRAME_X_OUTPUT(f)->widget)
#define FRAME_GTK_WIDGET(f)       (FRAME_X_OUTPUT(f)->edit_widget)

/* aliases */
#define FRAME_PGTK_VIEW(f)         FRAME_GTK_WIDGET(f)
#define FRAME_X_WINDOW(f)          FRAME_GTK_OUTER_WIDGET(f)
#define FRAME_NATIVE_WINDOW(f)     GTK_WINDOW(FRAME_X_WINDOW(f))

#define FRAME_X_DISPLAY(f)        (FRAME_DISPLAY_INFO(f)->gdpy)

#define DEFAULT_GDK_DISPLAY() gdk_display_get_default()

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))

#define PGTK_FACE_FOREGROUND(f) ((f)->foreground)
#define PGTK_FACE_BACKGROUND(f) ((f)->background)
#define FRAME_DEFAULT_FACE(f) FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID)

/* Compute pixel height of the frame's titlebar. */
#define FRAME_PGTK_TITLEBAR_HEIGHT(f)                                     0

/* Compute pixel size for vertical scroll bars */
#define PGTK_SCROLL_BAR_WIDTH(f)					\
  (FRAME_HAS_VERTICAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_WIDTH (f)				\
	   : (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)))	\
   : 0)

/* Compute pixel size for horizontal scroll bars */
#define PGTK_SCROLL_BAR_HEIGHT(f)					\
  (FRAME_HAS_HORIZONTAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_HEIGHT (f)				\
	   : (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)))	\
   : 0)

/* Difference btwn char-column-calculated and actual SB widths.
   This is only a concern for rendering when SB on left. */
#define PGTK_SCROLL_BAR_ADJUST(w, f)				\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) ?			\
   (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)		\
    - PGTK_SCROLL_BAR_WIDTH (f)) : 0)

/* Difference btwn char-line-calculated and actual SB heights.
   This is only a concern for rendering when SB on top. */
#define PGTK_SCROLL_BAR_ADJUST_HORIZONTALLY(w, f)		\
  (WINDOW_HAS_HORIZONTAL_SCROLL_BARS (w) ?		\
   (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)	\
    - PGTK_SCROLL_BAR_HEIGHT (f)) : 0)

#define FRAME_MENUBAR_HEIGHT(f) (FRAME_X_OUTPUT(f)->menubar_height)

/* Calculate system coordinates of the left and top of the parent
   window or, if there is no parent window, the screen. */
#define PGTK_PARENT_WINDOW_LEFT_POS(f)                                    \
  (FRAME_PARENT_FRAME (f) != NULL                                       \
   ? [[FRAME_PGTK_VIEW (f) window] parentWindow].frame.origin.x : 0)
#define PGTK_PARENT_WINDOW_TOP_POS(f)                                     \
  (FRAME_PARENT_FRAME (f) != NULL                                       \
   ? ([[FRAME_PGTK_VIEW (f) window] parentWindow].frame.origin.y          \
      + [[FRAME_PGTK_VIEW (f) window] parentWindow].frame.size.height     \
      - FRAME_PGTK_TITLEBAR_HEIGHT (FRAME_PARENT_FRAME (f)))              \
   : [[[PGTKScreen screepgtk] objectAtIndex: 0] frame].size.height)

#define FRAME_PGTK_FONT_TABLE(f) (FRAME_DISPLAY_INFO (f)->font_table)

#define FRAME_TOOLBAR_TOP_HEIGHT(f) ((f)->output_data.pgtk->toolbar_top_height)
#define FRAME_TOOLBAR_BOTTOM_HEIGHT(f) \
  ((f)->output_data.pgtk->toolbar_bottom_height)
#define FRAME_TOOLBAR_HEIGHT(f) \
  (FRAME_TOOLBAR_TOP_HEIGHT (f) + FRAME_TOOLBAR_BOTTOM_HEIGHT (f))
#define FRAME_TOOLBAR_LEFT_WIDTH(f) ((f)->output_data.pgtk->toolbar_left_width)
#define FRAME_TOOLBAR_RIGHT_WIDTH(f) ((f)->output_data.pgtk->toolbar_right_width)
#define FRAME_TOOLBAR_WIDTH(f) \
  (FRAME_TOOLBAR_LEFT_WIDTH (f) + FRAME_TOOLBAR_RIGHT_WIDTH (f))

#define FRAME_FONTSET(f) (FRAME_X_OUTPUT(f)->fontset)

#define FRAME_BASELINE_OFFSET(f) (FRAME_X_OUTPUT(f)->baseline_offset)
#define BLACK_PIX_DEFAULT(f) 0x000000
#define WHITE_PIX_DEFAULT(f) 0xFFFFFF

/* First position where characters can be shown (instead of scrollbar, if
   it is on left. */
#define FIRST_CHAR_POSITION(f)				\
  (! (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)) ? 0	\
   : FRAME_SCROLL_BAR_COLS (f))


/* Display init/shutdown functions implemented in pgtkterm.c */
extern struct pgtk_display_info *pgtk_term_init (Lisp_Object display_name, char *resource_name);
extern void pgtk_term_shutdown (int sig);

/* Implemented in pgtkterm, published in or needed from pgtkfns. */
extern void pgtk_clear_frame (struct frame *f);
extern char *pgtk_xlfd_to_fontname (const char *xlfd);

/* Implemented in pgtkfns. */
extern void pgtk_set_doc_edited (void);
extern const char *pgtk_get_defaults_value (const char *key);
extern const char *pgtk_get_string_resource (XrmDatabase rdb, const char *name, const char *class);
extern void pgtk_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval);

/* Color management implemented in pgtkterm. */
extern bool pgtk_defined_color (struct frame *f,
				const char *name,
				Emacs_Color *color_def, bool alloc,
				bool makeIndex);
extern void pgtk_query_color (struct frame *f, Emacs_Color *color);
extern void pgtk_query_colors (struct frame *f, Emacs_Color *colors, int ncolors);
extern int pgtk_parse_color (struct frame *f, const char *color_name, Emacs_Color *color);

/* Implemented in pgtkterm.c */
extern void pgtk_clear_area (struct frame *f, int x, int y, int width, int height);
extern int pgtk_gtk_to_emacs_modifiers (struct pgtk_display_info *dpyinfo, int state);
extern void pgtk_clear_under_internal_border (struct frame *f);
extern void pgtk_set_event_handler(struct frame *f);

/* Implemented in pgtkterm.c */
extern int x_display_pixel_height (struct pgtk_display_info *);
extern int x_display_pixel_width (struct pgtk_display_info *);

/* Implemented in pgtkterm.c */
extern void x_destroy_window (struct frame *f);
extern void x_set_parent_frame (struct frame *f, Lisp_Object new_value,
                                Lisp_Object old_value);
extern void x_set_no_focus_on_map (struct frame *f, Lisp_Object new_value,
                                   Lisp_Object old_value);
extern void x_set_no_accept_focus (struct frame *f, Lisp_Object new_value,
                                   Lisp_Object old_value);
extern void x_set_z_group (struct frame *f, Lisp_Object new_value,
                           Lisp_Object old_value);
extern int pgtk_select (int nfds, fd_set *readfds, fd_set *writefds,
			fd_set *exceptfds, struct timespec *timeout,
			sigset_t *sigmask);

/* Cairo related functions implemented in pgtkterm.c */
extern void pgtk_cr_update_surface_desired_size (struct frame *, int, int);
extern cairo_t *pgtk_begin_cr_clip (struct frame *f);
extern void pgtk_end_cr_clip (struct frame *f);
extern void pgtk_set_cr_source_with_gc_foreground (struct frame *f, Emacs_GC *gc);
extern void pgtk_set_cr_source_with_gc_background (struct frame *f, Emacs_GC *gc);
extern void pgtk_set_cr_source_with_color (struct frame *f, unsigned long color);
extern void pgtk_cr_draw_frame (cairo_t *cr, struct frame *f);
extern void pgtk_cr_destroy_frame_context(struct frame *f);

/* Defined in pgtkmenu.c */
extern Lisp_Object pgtk_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents);
extern Lisp_Object pgtk_dialog_show (struct frame *f, Lisp_Object title, Lisp_Object header, const char **error_name);
extern void initialize_frame_menubar (struct frame *);


/* Symbol initializations implemented in each pgtk sources. */
extern void syms_of_pgtkterm (void);
extern void syms_of_pgtkfns (void);
extern void syms_of_pgtkmenu (void);
extern void syms_of_pgtkselect (void);
extern void syms_of_pgtkim (void);

/* Implemented in pgtkselect. */
extern void nxatoms_of_pgtkselect (void);

/* Initialization and marking implemented in pgtkterm.c */
extern void init_pgtkterm (void);
extern void mark_pgtkterm(void);
extern void pgtk_delete_terminal (struct terminal *terminal);

extern void pgtk_make_frame_visible (struct frame *f);
extern void pgtk_make_frame_invisible (struct frame *f);
extern void x_wm_set_size_hint (struct frame *, long, bool);
extern void x_free_frame_resources (struct frame *);
extern void pgtk_iconify_frame (struct frame *f);
extern void pgtk_focus_frame (struct frame *f, bool noactivate);
extern void pgtk_set_scroll_bar_default_width (struct frame *f);
extern void pgtk_set_scroll_bar_default_height (struct frame *f);
extern Lisp_Object x_get_focus_frame (struct frame *frame);

extern void pgtk_frame_rehighlight (struct pgtk_display_info *dpyinfo);

extern void x_change_tab_bar_height (struct frame *, int);

extern struct pgtk_display_info *check_pgtk_display_info (Lisp_Object object);

extern void pgtk_default_font_parameter (struct frame *f, Lisp_Object parms);

extern void pgtk_menu_set_in_use (bool in_use);


extern void pgtk_enqueue_string(struct frame *f, gchar *str);
extern void pgtk_enqueue_preedit(struct frame *f, Lisp_Object image_data);
extern void pgtk_im_focus_in(struct frame *f);
extern void pgtk_im_focus_out(struct frame *f);
extern bool pgtk_im_filter_keypress(struct frame *f, GdkEventKey *ev);
extern void pgtk_im_init(struct pgtk_display_info *dpyinfo);
extern void pgtk_im_finish(struct pgtk_display_info *dpyinfo);

extern bool xg_set_icon (struct frame *, Lisp_Object);
extern bool xg_set_icon_from_xpm_data (struct frame *f, const char **data);

extern bool pgtk_text_icon (struct frame *f, const char *icon_name);

#endif	/* HAVE_PGTK */
