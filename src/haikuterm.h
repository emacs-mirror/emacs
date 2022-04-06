/* Haiku window system support
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#ifndef _HAIKU_TERM_H_
#define _HAIKU_TERM_H_

#include <pthread.h>

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

#include "haikugui.h"
#include "frame.h"
#include "character.h"
#include "dispextern.h"
#include "font.h"
#include "systime.h"

#define HAVE_CHAR_CACHE_MAX 65535

extern int popup_activated_p;

extern void be_app_quit (void);

struct haikufont_info
{
  struct font font;
  haiku be_font;
  struct font_metrics **metrics;
  short metrics_nrows;

  unsigned short **glyphs;
};

struct haiku_bitmap_record
{
  haiku img;
  char *file;
  int refcount;
  int height, width, depth;
};

struct haiku_display_info
{
  /* Chain of all haiku_display_info structures. */
  struct haiku_display_info *next;
  struct terminal *terminal;

  Lisp_Object name_list_element;
  Lisp_Object color_map;

  int n_fonts;

  int smallest_char_width;
  int smallest_font_height;

  struct frame *focused_frame;
  struct frame *focus_event_frame;
  struct frame *last_mouse_glyph_frame;

  struct haiku_bitmap_record *bitmaps;
  ptrdiff_t bitmaps_size;
  ptrdiff_t bitmaps_last;

  int grabbed;
  int n_planes;
  int color_p;

  Lisp_Object rdb;

  Emacs_Cursor vertical_scroll_bar_cursor;
  Emacs_Cursor horizontal_scroll_bar_cursor;

  Mouse_HLInfo mouse_highlight;

  struct frame *highlight_frame;
  struct frame *last_mouse_frame;
  struct frame *last_mouse_motion_frame;

  int last_mouse_motion_x;
  int last_mouse_motion_y;

  struct haiku_rect last_mouse_glyph;

  void *last_mouse_scroll_bar;

  haiku display;

  double resx, resy;

  Time last_mouse_movement_time;

  Window root_window;

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
  Emacs_Cursor no_cursor;
};

struct haiku_output
{
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
  Emacs_Cursor no_cursor;

  Emacs_Cursor current_cursor;

  struct haiku_display_info *display_info;

  int baseline_offset;
  int fontset;

  Emacs_Color cursor_color;

  Window window_desc, parent_desc;
  char explicit_parent;

  int titlebar_height;
  int toolbar_height;

  haiku window;
  haiku view;
  haiku menubar;

  int menu_up_to_date_p;
  int zoomed_p;

  int menu_bar_open_p;

  struct font *font;

  int hourglass_p;
  uint32_t cursor_fg;
  bool dirty_p;

  /* The pending position we're waiting for. */
  int pending_top, pending_left;

  /* Whether or not adjust_frame_size and haiku_set_offset have yet
     been called by haiku_create_frame.  */
  bool configury_done;
};

struct x_output
{
  /* Unused, makes term.c happy. */
};

extern struct haiku_display_info *x_display_list;
extern struct font_driver const haikufont_driver;

extern Lisp_Object tip_frame;
extern struct frame *haiku_dnd_frame;

struct scroll_bar
{
  /* These fields are shared by all vectors.  */
  union vectorlike_header header;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* Fields after 'prev' are not traced by the GC.  */

  /* The position and size of the scroll bar in pixels, relative to the
     frame.  */
  int top, left, width, height;

  /* The actual scrollbar. */
  void *scroll_bar;

  /* Non-nil if the scroll bar handle is currently being dragged by
     the user.  */
  int dragging;

  /* The update position if we are waiting for a scrollbar update, or
     -1. */
  int update;

  /* The last known position of this scrollbar. */
  int position;

  /* The total number of units inside this scrollbar. */
  int total;

  /* True if the scroll bar is horizontal.  */
  bool horizontal;

  int page_size;
};

#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))

#define FRAME_DIRTY_P(f) (FRAME_OUTPUT_DATA (f)->dirty_p)
#define MAKE_FRAME_DIRTY(f) (FRAME_DIRTY_P (f) = 1)
#define FRAME_OUTPUT_DATA(f) ((f)->output_data.haiku)
#define FRAME_HAIKU_WINDOW(f) (FRAME_OUTPUT_DATA (f)->window)
#define FRAME_HAIKU_VIEW(f) ((MAKE_FRAME_DIRTY (f)), FRAME_OUTPUT_DATA (f)->view)
#define FRAME_HAIKU_MENU_BAR(f) (FRAME_OUTPUT_DATA (f)->menubar)
#define FRAME_DISPLAY_INFO(f) (FRAME_OUTPUT_DATA (f)->display_info)
#define FRAME_FONT(f) (FRAME_OUTPUT_DATA (f)->font)
#define FRAME_FONTSET(f) (FRAME_OUTPUT_DATA (f)->fontset)
#define FRAME_NATIVE_WINDOW(f) (FRAME_OUTPUT_DATA (f)->window)
#define FRAME_BASELINE_OFFSET(f) (FRAME_OUTPUT_DATA (f)->baseline_offset)
#define FRAME_CURSOR_COLOR(f) (FRAME_OUTPUT_DATA (f)->cursor_color)

#ifdef USE_BE_CAIRO
#define FRAME_CR_CONTEXT(f)					\
  (FRAME_HAIKU_VIEW (f)						\
   ? EmacsView_cairo_context (FRAME_HAIKU_VIEW (f))		\
   : NULL)
#endif

extern void syms_of_haikuterm (void);
extern void syms_of_haikufns (void);
extern void syms_of_haikumenu (void);
extern void syms_of_haikufont (void);
extern void syms_of_haikuselect (void);
extern void init_haiku_select (void);

extern void haiku_iconify_frame (struct frame *);
extern void haiku_visualize_frame (struct frame *);
extern void haiku_unvisualize_frame (struct frame *);
extern void haiku_set_offset (struct frame *, int, int, int);
extern void haiku_set_frame_visible_invisible (struct frame *, bool);
extern void haiku_free_frame_resources (struct frame *f);
extern void haiku_scroll_bar_remove (struct scroll_bar *bar);
extern void haiku_clear_under_internal_border (struct frame *f);
extern void haiku_set_name (struct frame *f, Lisp_Object name, bool explicit_p);
extern Lisp_Object haiku_message_to_lisp (void *);

extern struct haiku_display_info *haiku_term_init (void);

extern void mark_haiku_display (void);

extern int haiku_get_color (const char *name, Emacs_Color *color);
extern void haiku_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval);
extern void haiku_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval);
extern void haiku_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval);
extern void haiku_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval);
extern void haiku_change_tab_bar_height (struct frame *f, int height);
extern void haiku_change_tool_bar_height (struct frame *f, int height);

extern void haiku_query_color (uint32_t col, Emacs_Color *color);

extern unsigned long haiku_get_pixel (haiku bitmap, int x, int y);
extern void haiku_put_pixel (haiku bitmap, int x, int y, unsigned long pixel);

extern Lisp_Object haiku_menu_show (struct frame *f, int x, int y, int menu_flags,
				    Lisp_Object title, const char **error_name);
extern Lisp_Object haiku_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents);
extern void haiku_note_drag_motion (void);

extern void initialize_frame_menubar (struct frame *f);

extern void run_menu_bar_help_event (struct frame *f, int mb_idx);
extern void put_xrm_resource (Lisp_Object name, Lisp_Object val);

#ifdef HAVE_NATIVE_IMAGE_API
extern bool haiku_can_use_native_image_api (Lisp_Object type);
extern int haiku_load_image (struct frame *f, struct image *img,
			     Lisp_Object spec_file, Lisp_Object spec_data);
extern void syms_of_haikuimage (void);
#endif

#ifdef USE_BE_CAIRO
extern cairo_t *
haiku_begin_cr_clip (struct frame *f, struct glyph_string *s);

extern void
haiku_end_cr_clip (cairo_t *cr);
#endif

extern void haiku_merge_cursor_foreground (struct glyph_string *, unsigned long *,
					   unsigned long *);
#endif /* _HAIKU_TERM_H_ */
