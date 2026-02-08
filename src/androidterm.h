/* Communication module for Android terminals.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#ifndef _ANDROID_TERM_H_
#define _ANDROID_TERM_H_

#include "androidgui.h"
#include "frame.h"
#include "character.h"
#include "dispextern.h"
#include "font.h"
#include "termhooks.h"

struct android_bitmap_record
{
  /* The image backing the bitmap and its mask.  */
  android_pixmap pixmap, mask;

  /* The file from which it comes.  */
  char *file;

  /* The number of references to it.  */
  int refcount;

  /* The height and width and the depth.  */
  int height, width, depth;

  /* Whether or not there is a mask.  */
  bool have_mask;
};

struct android_display_info
{
  /* Chain of all struct android_display_info structures.  */
  struct android_display_info *next;

  /* The terminal.  */
  struct terminal *terminal;

  /* The root window.  This field is unused.  */
  Emacs_Window root_window;

  /* List possibly used only for the font cache but probably used for
     something else too.  */
  Lisp_Object name_list_element;

  /* List of predefined X colors.  */
  Lisp_Object color_map;

  /* DPI of the display.  */
  double resx, resy;

  /* DPI used to convert font point sizes into pixel dimensions.
     This is resy adjusted by a fixed scaling factor specified by
     the user.  */
  double font_resolution;

  /* Scratch GC for drawing a cursor in a non-default face. */
  struct android_gc *scratch_cursor_gc;

  /* Mouse highlight information.  */
  Mouse_HLInfo mouse_highlight;

  /* Number of planes on this screen, and the same for the purposes of
     image processing.  */
  int n_planes, n_image_planes;

  /* Mask of things causing the mouse to be grabbed.  */
  int grabbed;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /* Default name for all frames on this display.  */
  char *x_id_name;

  /* The number of fonts opened for this display.  */
  int n_fonts;

  /* Pointer to bitmap records.  */
  struct android_bitmap_record *bitmaps;

  /* Allocated size of bitmaps field.  */
  ptrdiff_t bitmaps_size;

  /* Last used bitmap index.  */
  ptrdiff_t bitmaps_last;

  /* The frame currently with the input focus.  */
  struct frame *focus_frame;

  /* The last frame mentioned in a focus event.  */
  struct frame *x_focus_event_frame;

  /* The frame which currently has the visual highlight, and should
     get keyboard input.  It points to the focus frame's selected
     window's frame, but can differ.  */
  struct frame *highlight_frame;

  /* The frame waiting to be auto-raised in android_read_socket.  */
  struct frame *pending_autoraise_frame;

  /* The frame where the mouse was the last time a button event
     happened.  */
  struct frame *last_mouse_frame;

  /* The frame where the mouse was the last time the mouse glyph
     changed.  */
  struct frame *last_mouse_glyph_frame;

  /* The frame where the mouse was the last time mouse motion
     happened.  */
  struct frame *last_mouse_motion_frame;

  /* Position where the mouse was last time we reported a motion.
     This is a position on last_mouse_motion_frame.  It is used in to
     report the mouse position as well: see
     android_mouse_position.  */
  int last_mouse_motion_x, last_mouse_motion_y;

  /* Where the mouse was the last time the mouse moved.  */
  Emacs_Rectangle last_mouse_glyph;

  /* The time of the last mouse movement.  */
  Time last_mouse_movement_time;

  /* ID of the last menu event received.  -1 means Emacs is waiting
     for a context menu event.  */
  int menu_event_id;

  /* The invisible cursor used for pointer blanking.  */
  android_cursor invisible_cursor;
};

/* Structure representing a single tool (finger or stylus) pressed
   onto a frame.  */

struct android_touch_point
{
  /* The next tool on this list.  */
  struct android_touch_point *next;

  /* The tool ID and the last known X and Y positions.  */
  int tool_id, x, y;

  /* Whether or not the tool is pressed on the tool bar.  */
  bool tool_bar_p;
};

struct android_output
{
  /* Graphics contexts for the default font.  */
  struct android_gc *normal_gc, *reverse_gc, *cursor_gc;

  /* The window used for this frame.  */
  Emacs_Window window;

  /* Unused field.  */
  Emacs_Window parent_desc;

  /* Default ASCII font of this frame.  */
  struct font *font;

  /* The baseline offset of the default ASCII font.  */
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset;

  /* Various colors.  */
  unsigned long cursor_pixel;
  unsigned long mouse_pixel;
  unsigned long cursor_foreground_pixel;

  /* Foreground color for scroll bars.  A value of -1 means use the
     default (black for non-toolkit scroll bars).  */
  unsigned long scroll_bar_foreground_pixel;

  /* Background color for scroll bars.  A value of -1 means use the
     default (background color of the frame for non-toolkit scroll
     bars).  */
  unsigned long scroll_bar_background_pixel;

  /* Cursors associated with this frame.  */
  Emacs_Cursor text_cursor;
  Emacs_Cursor nontext_cursor;
  Emacs_Cursor modeline_cursor;
  Emacs_Cursor hand_cursor;
  Emacs_Cursor hourglass_cursor;
  Emacs_Cursor horizontal_drag_cursor;
  Emacs_Cursor vertical_drag_cursor;
  Emacs_Cursor current_cursor;
  Emacs_Cursor left_edge_cursor;
  Emacs_Cursor top_left_corner_cursor;
  Emacs_Cursor top_edge_cursor;
  Emacs_Cursor top_right_corner_cursor;
  Emacs_Cursor right_edge_cursor;
  Emacs_Cursor bottom_right_corner_cursor;
  Emacs_Cursor bottom_edge_cursor;
  Emacs_Cursor bottom_left_corner_cursor;

  /* Whether or not the hourglass cursor is being displayed.  */
  bool hourglass;

  /* This is the Emacs structure for the display this frame is on.  */
  struct android_display_info *display_info;

  /* True if this frame was ever previously visible.  */
  bool_bf has_been_visible : 1;

  /* True if this frame's alpha value is the same for both the active
     and inactive states.  */
  bool_bf alpha_identical_p : 1;

  /* Flag that indicates whether or not the frame contents are
     complete and can be safely flushed while handling async
     input.  */
  bool_bf complete : 1;

  /* True that indicates whether or not a buffer flip is required
     because the frame contents have been dirtied.  */
  bool_bf need_buffer_flip : 1;

  /* Whether or not the input method should be notified every time the
     position of this frame's selected window changes.  */
  bool_bf need_cursor_updates : 1;

  /* Relief GCs, colors etc.  */
  struct relief {
    struct android_gc *gc;
    unsigned long pixel;
  } black_relief, white_relief;

  /* The background for which the above relief GCs were set up.
     They are changed only when a different background is involved.  */
  unsigned long relief_background;

  /* Focus state.  Only present for consistency with X; it is actually
     a boolean.  */
  int focus_state;

  /* List of all tools (either styluses or fingers) pressed onto the
     frame.  */
  struct android_touch_point *touch_points;

  /* Flags associated with the last request to obtain ``extracted
     text''.  */
  int extracted_text_flags;

  /* Token associated with that request.  */
  int extracted_text_token;

  /* The number of characters of extracted text wanted by the IM.  */
  int extracted_text_hint;
};

enum
  {
    /* Values for focus_state, used as bit mask.  EXPLICIT means we
       received a FocusIn for the frame and know it has the focus.
       IMPLICIT means we received an EnterNotify and the frame may
       have the focus if no window manager is running.  FocusOut and
       LeaveNotify clears EXPLICIT/IMPLICIT. */
    FOCUS_NONE     = 0,
    FOCUS_IMPLICIT = 1,
    FOCUS_EXPLICIT = 2
  };

/* Return the Android output data for frame F.  */
#define FRAME_ANDROID_OUTPUT(f)	((f)->output_data.android)
#define FRAME_OUTPUT_DATA(f)	((f)->output_data.android)

/* Return the Android window used for displaying data in frame F.  */
#define FRAME_ANDROID_WINDOW(f)	((f)->output_data.android->window)
#define FRAME_NATIVE_WINDOW(f)	((f)->output_data.android->window)

/* Return the need-buffer-flip flag for frame F.  */
#define FRAME_ANDROID_NEED_BUFFER_FLIP(f)	\
  ((f)->output_data.android->need_buffer_flip)

/* Return the drawable used for rendering to frame F and mark the
   frame as needing a buffer flip later.  There's no easy way to run
   code after any drawing command, but code can be run whenever
   someone asks for the handle necessary to draw.  */
#define FRAME_ANDROID_DRAWABLE(f)			\
  ((f)->output_data.android->need_buffer_flip = true, \
   FRAME_ANDROID_WINDOW (f))

/* Return whether or not the frame F has been completely drawn.  Used
   while handling async input.  */
#define FRAME_ANDROID_COMPLETE_P(f)		\
  ((f)->output_data.android->complete)

#define FRAME_FONT(f)		((f)->output_data.android->font)
#define FRAME_FONTSET(f)	((f)->output_data.android->fontset)

#define FRAME_BASELINE_OFFSET(f)		\
  ((f)->output_data.android->baseline_offset)

/* This gives the android_display_info structure for the display F is
   on.  */
#define FRAME_DISPLAY_INFO(f) ((f)->output_data.android->display_info)

/* Some things for X compatibility.  */
#define BLACK_PIX_DEFAULT(f) 0
#define WHITE_PIX_DEFAULT(f) 0xffffffff

/* Android-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar
{
  /* These fields are shared by all vectors.  */
  union vectorlike_header header;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* Fields after 'prev' are not traced by the GC.  */

  /* The X window representing this scroll bar.  */
  Emacs_Window x_window;

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
     different coordinate system.  */
  int start, end;

  /* If the scroll bar handle is currently being dragged by the user,
     this is the number of pixels from the top of the handle to the
     place where the user grabbed it.  If the handle isn't currently
     being dragged, this is -1.  */
  int dragging;

  /* True if the scroll bar is horizontal.  */
  bool horizontal;
};

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))



/* This is a chain of structures for all the Android displays
   currently in use.  There is only ever one, but the rest of Emacs is
   written with systems on which there can be many in mind.  */
extern struct android_display_info *x_display_list;



/* Start of function definitions.  These should be a neat subset of
   the same ones in xterm.h, and come in the same order.  */

/* From androidfns.c.  */

extern frame_parm_handler android_frame_parm_handlers[];
extern void android_free_gcs (struct frame *);
extern void android_default_font_parameter (struct frame *, Lisp_Object);
extern void android_set_preeditarea (struct window *, int, int);

/* Defined in androidterm.c.  */

extern void android_term_init (void);
extern void android_set_window_size (struct frame *, bool, int, int);
extern void android_set_window_size_and_position (struct frame *, int, int);
extern void android_iconify_frame (struct frame *);
extern void android_make_frame_visible (struct frame *);
extern void android_make_frame_invisible (struct frame *);
extern void android_free_frame_resources (struct frame *);

extern int android_parse_color (struct frame *, const char *,
				Emacs_Color *);
extern bool android_alloc_nearest_color (struct frame *, Emacs_Color *);
extern void android_query_colors (struct frame *, Emacs_Color *, int);
extern void android_clear_under_internal_border (struct frame *);

extern void syms_of_androidterm (void);
extern void mark_androidterm (void);

/* Defined in androidfns.c.  */

extern void android_change_tab_bar_height (struct frame *, int);
extern void android_change_tool_bar_height (struct frame *, int);
extern void android_set_scroll_bar_default_width (struct frame *);
extern void android_set_scroll_bar_default_height (struct frame *);
extern bool android_defined_color (struct frame *, const char *,
				   Emacs_Color *, bool, bool);
extern void android_implicitly_set_name (struct frame *, Lisp_Object,
					 Lisp_Object);
extern void android_explicitly_set_name (struct frame *, Lisp_Object,
					 Lisp_Object);

extern void syms_of_androidfns (void);

/* Defined in androidfont.c.  */

extern struct font_driver androidfont_driver;

extern void init_androidfont (void);
extern void syms_of_androidfont (void);

extern void android_finalize_font_entity (struct font_entity *);

/* Defined in androidmenu.c.  */

#ifndef ANDROID_STUBIFY

extern unsigned int current_menu_serial;

#endif

extern Lisp_Object android_menu_show (struct frame *, int, int, int,
				      Lisp_Object, const char **);
extern Lisp_Object android_popup_dialog (struct frame *, Lisp_Object,
					 Lisp_Object);

extern void init_androidmenu (void);
extern void syms_of_androidmenu (void);

/* Defined in sfntfont-android.c.  */

extern const struct font_driver android_sfntfont_driver;

extern void sfntfont_android_shrink_scanline_buffer (void);
extern void init_sfntfont_android (void);
extern void syms_of_sfntfont_android (void);

/* Defined in androidselect.c.  */

#ifndef ANDROID_STUBIFY

extern void android_notification_deleted (struct android_notification_event *,
					  struct input_event *);
extern void android_notification_action (struct android_notification_event *,
					 struct input_event *, Lisp_Object);

extern void init_androidselect (void);
extern void syms_of_androidselect (void);

/* Defined in androidvfs.c.  */
extern void syms_of_androidvfs (void);

#endif



#define RGB_TO_ULONG(r, g, b)   (((r) << 16) | ((g) << 8) | (b))
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)



#endif /* _ANDROID_TERM_H_ */
