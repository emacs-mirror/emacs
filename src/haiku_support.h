/* Haiku window system support.  Hey Emacs, this is -*- C++ -*-
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

#ifndef _HAIKU_SUPPORT_H
#define _HAIKU_SUPPORT_H

#include <stdint.h>

#ifdef HAVE_FREETYPE
#include <ft2build.h>
#include <fontconfig/fontconfig.h>
#include FT_FREETYPE_H
#include FT_SIZES_H
#endif

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

#include <math.h>

#include <kernel/OS.h>

enum haiku_cursor
  {
    CURSOR_ID_NO_CURSOR = 12,
    CURSOR_ID_RESIZE_NORTH = 15,
    CURSOR_ID_RESIZE_EAST = 16,
    CURSOR_ID_RESIZE_SOUTH = 17,
    CURSOR_ID_RESIZE_WEST = 18,
    CURSOR_ID_RESIZE_NORTH_EAST = 19,
    CURSOR_ID_RESIZE_NORTH_WEST = 20,
    CURSOR_ID_RESIZE_SOUTH_EAST = 21,
    CURSOR_ID_RESIZE_SOUTH_WEST = 22,
    CURSOR_ID_RESIZE_NORTH_SOUTH = 23,
    CURSOR_ID_RESIZE_EAST_WEST = 24,
    CURSOR_ID_RESIZE_NORTH_EAST_SOUTH_WEST = 25,
    CURSOR_ID_RESIZE_NORTH_WEST_SOUTH_EAST = 26
  };

enum haiku_alert_type
  {
    HAIKU_EMPTY_ALERT = 0,
    HAIKU_INFO_ALERT,
    HAIKU_IDEA_ALERT,
    HAIKU_WARNING_ALERT,
    HAIKU_STOP_ALERT
  };

enum haiku_event_type
  {
    QUIT_REQUESTED,
    FRAME_RESIZED,
    FRAME_EXPOSED,
    KEY_DOWN,
    KEY_UP,
    ACTIVATION,
    MOUSE_MOTION,
    BUTTON_DOWN,
    BUTTON_UP,
    ICONIFICATION,
    MOVE_EVENT,
    SCROLL_BAR_VALUE_EVENT,
    SCROLL_BAR_PART_EVENT,
    SCROLL_BAR_DRAG_EVENT,
    WHEEL_MOVE_EVENT,
    MENU_BAR_RESIZE,
    MENU_BAR_OPEN,
    MENU_BAR_SELECT_EVENT,
    MENU_BAR_CLOSE,
    FILE_PANEL_EVENT,
    MENU_BAR_HELP_EVENT,
    ZOOM_EVENT,
    DRAG_AND_DROP_EVENT,
    APP_QUIT_REQUESTED_EVENT,
    DUMMY_EVENT,
    MENU_BAR_LEFT
  };

struct haiku_quit_requested_event
{
  void *window;
};

struct haiku_resize_event
{
  void *window;
  float px_heightf;
  float px_widthf;
};

struct haiku_expose_event
{
  void *window;
  int x;
  int y;
  int width;
  int height;
};

struct haiku_drag_and_drop_event
{
  void *window;
  int x, y;
  void *message;
};

struct haiku_app_quit_requested_event
{
  char dummy;
};

struct haiku_dummy_event
{
  char dummy;
};

#define HAIKU_MODIFIER_ALT (1)
#define HAIKU_MODIFIER_CTRL (1 << 1)
#define HAIKU_MODIFIER_SHIFT (1 << 2)
#define HAIKU_MODIFIER_SUPER (1 << 3)

struct haiku_key_event
{
  void *window;
  int modifiers;
  unsigned keysym;
  uint32_t multibyte_char;

  /* Time the keypress occurred, in microseconds.  */
  bigtime_t time;
};

struct haiku_activation_event
{
  void *window;
  int activated_p;
};

struct haiku_mouse_motion_event
{
  void *window;
  bool just_exited_p;
  int x;
  int y;
  bigtime_t time;
  bool dnd_message;
};

struct haiku_menu_bar_left_event
{
  void *window;
  int x, y;
};

struct haiku_button_event
{
  void *window;
  int btn_no;
  int modifiers;
  int x;
  int y;
  bigtime_t time;
};

struct haiku_iconification_event
{
  void *window;
  int iconified_p;
};

struct haiku_move_event
{
  void *window;
  int x;
  int y;
};

struct haiku_wheel_move_event
{
  void *window;
  int modifiers;
  float delta_x;
  float delta_y;
};

struct haiku_menu_bar_select_event
{
  void *window;
  void *ptr;
};

struct haiku_file_panel_event
{
  void *ptr;
};

struct haiku_menu_bar_help_event
{
  void *window;
  int mb_idx;
  void *data;
  bool highlight_p;
};

struct haiku_zoom_event
{
  void *window;

  bool zoomed;
};

#define FSPEC_FAMILY 1
#define FSPEC_STYLE (1 << 1)
#define FSPEC_SLANT (1 << 2)
#define FSPEC_WEIGHT (1 << 3)
#define FSPEC_SPACING (1 << 4)
#define FSPEC_WANTED (1 << 5)
#define FSPEC_NEED_ONE_OF (1 << 6)
#define FSPEC_WIDTH (1 << 7)
#define FSPEC_LANGUAGE (1 << 8)

typedef char haiku_font_family_or_style[64];

enum haiku_font_slant
  {
    NO_SLANT = -1,
    SLANT_OBLIQUE,
    SLANT_REGULAR,
    SLANT_ITALIC
  };

enum haiku_font_width
  {
    NO_WIDTH = -1,
    ULTRA_CONDENSED,
    EXTRA_CONDENSED,
    CONDENSED,
    SEMI_CONDENSED,
    NORMAL_WIDTH,
    SEMI_EXPANDED,
    EXPANDED,
    EXTRA_EXPANDED,
    ULTRA_EXPANDED
  };

enum haiku_font_language
  {
    LANGUAGE_CN,
    LANGUAGE_KO,
    LANGUAGE_JP,
    MAX_LANGUAGE /* This isn't a language. */
  };

struct haiku_font_pattern
{
  int specified;
  struct haiku_font_pattern *next;
  /* The next two fields are only temporarily used during the font
     discovery process! Do not rely on them being correct outside
     BFont_find.  */
  struct haiku_font_pattern *last;
  struct haiku_font_pattern *next_family;
  haiku_font_family_or_style family;
  haiku_font_family_or_style style;
  int weight;
  int mono_spacing_p;
  int want_chars_len;
  int need_one_of_len;
  enum haiku_font_slant slant;
  enum haiku_font_width width;
  enum haiku_font_language language;
  uint32_t *wanted_chars;
  uint32_t *need_one_of;

  int oblique_seen_p;
};

struct haiku_scroll_bar_value_event
{
  void *scroll_bar;
  void *window;
  int position;
};

struct haiku_scroll_bar_drag_event
{
  void *scroll_bar;
  void *window;
  int dragging_p;
};

enum haiku_scroll_bar_part
  {
    HAIKU_SCROLL_BAR_UP_BUTTON,
    HAIKU_SCROLL_BAR_DOWN_BUTTON
  };

struct haiku_scroll_bar_part_event
{
  void *scroll_bar;
  void *window;
  enum haiku_scroll_bar_part part;
};

struct haiku_menu_bar_resize_event
{
  void *window;
  int width;
  int height;
};

struct haiku_menu_bar_state_event
{
  void *window;
  bool no_lock;
};

#define HAIKU_THIN 0
#define HAIKU_ULTRALIGHT 20
#define HAIKU_EXTRALIGHT 40
#define HAIKU_LIGHT 50
#define HAIKU_SEMI_LIGHT 75
#define HAIKU_REGULAR 100
#define HAIKU_SEMI_BOLD 180
#define HAIKU_BOLD 200
#define HAIKU_EXTRA_BOLD 205
#define HAIKU_ULTRA_BOLD 210
#define HAIKU_BOOK 400
#define HAIKU_HEAVY 800
#define HAIKU_ULTRA_HEAVY 900
#define HAIKU_BLACK 1000
#define HAIKU_MEDIUM 2000

#ifdef __cplusplus
/* Haiku's built in Height and Width functions for calculating
   rectangle sizes are broken, probably for compatibility with BeOS:
   they do not round up in a reasonable fashion, and they return the
   numerical difference between the end and start sides in both
   directions, instead of the actual size.

   For example:

     BRect (1, 1, 5, 5).IntegerWidth ()

   Will return 4, when in reality the rectangle is 5 pixels wide,
   since the left corner is also a pixel!

   All code in Emacs should use the macros below to calculate the
   dimensions of a BRect, instead of relying on the broken Width and
   Height functions.  */

#define BE_RECT_HEIGHT(rect) (ceil (((rect).bottom - (rect).top) + 1))
#define BE_RECT_WIDTH(rect) (ceil (((rect).right - (rect).left) + 1))
#endif /* __cplusplus */

/* C++ code cannot include lisp.h, but file dialogs need to be able
   to bind to the specpdl and handle quitting correctly.  */

#ifdef __cplusplus

#if SIZE_MAX > 0xffffffff
#define WRAP_SPECPDL_REF 1
#endif
#ifdef WRAP_SPECPDL_REF
typedef struct { ptrdiff_t bytes; } specpdl_ref;
#else
typedef ptrdiff_t specpdl_ref;
#endif

#else
#include "lisp.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif
#include <pthread.h>
#include <OS.h>

#ifdef __cplusplus
  typedef void *haiku;

  extern void
  haiku_put_pixel (haiku bitmap, int x, int y, unsigned long pixel);

  extern unsigned long
  haiku_get_pixel (haiku bitmap, int x, int y);
#endif

  extern port_id port_application_to_emacs;
  extern port_id port_popup_menu_to_emacs;

  extern void haiku_io_init (void);
  extern void haiku_io_init_in_app_thread (void);

  extern void
  haiku_read_size (ssize_t *len, bool popup_menu_p);

  extern int
  haiku_read (enum haiku_event_type *type, void *buf, ssize_t len);

  extern int
  haiku_read_with_timeout (enum haiku_event_type *type, void *buf, ssize_t len,
			   bigtime_t timeout, bool popup_menu_p);

  extern int
  haiku_write (enum haiku_event_type type, void *buf);

  extern int
  haiku_write_without_signal (enum haiku_event_type type, void *buf,
			      bool popup_menu_p);

  extern void
  rgb_color_hsl (uint32_t rgb, double *h, double *s, double *l);

  extern void
  hsl_color_rgb (double h, double s, double l, uint32_t *rgb);

  extern void *
  BBitmap_new (int width, int height, int mono_p);

  extern void *
  BBitmap_data (void *bitmap);

  extern int
  BBitmap_convert (void *bitmap, void **new_bitmap);

  extern void
  BBitmap_free (void *bitmap);

  extern void
  BBitmap_dimensions (void *bitmap, int *left, int *top,
		      int *right, int *bottom, int32_t *bytes_per_row,
		      int *mono_p);

  extern void *
  BApplication_setup (void);

  extern void *
  BWindow_new (void *view);

  extern void
  BWindow_quit (void *window);

  extern void
  BWindow_set_offset (void *window, int x, int y);

  extern void
  BWindow_iconify (void *window);

  extern void
  BWindow_set_visible (void *window, int visible_p);

  extern void
  BFont_close (void *font);

  extern void
  BFont_dat (void *font, int *px_size, int *min_width, int *max_width,
	     int *avg_width, int *height, int *space_width, int *ascent,
	     int *descent, int *underline_position, int *underline_thickness);

  extern int
  BFont_have_char_p (void *font, int32_t chr);

  extern int
  BFont_have_char_block (void *font, int32_t beg, int32_t end);

  extern void
  BFont_char_bounds (void *font, const char *mb_str, int *advance,
		     int *lb, int *rb);

  extern void
  BFont_nchar_bounds (void *font, const char *mb_str, int *advance,
		      int *lb, int *rb, int32_t n);

  extern void
  BWindow_retitle (void *window, const char *title);

  extern void
  BWindow_resize (void *window, int width, int height);

  extern void
  BWindow_activate (void *window);

  extern void
  BView_StartClip (void *view);

  extern void
  BView_EndClip (void *view);

  extern void
  BView_SetHighColor (void *view, uint32_t color);

  extern void
  BView_SetHighColorForVisibleBell (void *view, uint32_t color);

  extern void
  BView_SetLowColor (void *view, uint32_t color);

  extern void
  BView_SetPenSize (void *view, int u);

  extern void
  BView_SetFont (void *view, void *font);

  extern void
  BView_MovePenTo (void *view, int x, int y);

  extern void
  BView_DrawString (void *view, const char *chr, ptrdiff_t len);

  extern void
  BView_DrawChar (void *view, char chr);

  extern void
  BView_FillRectangle (void *view, int x, int y, int width, int height);

  extern void
  BView_FillRectangleAbs (void *view, int x, int y, int x1, int y1);

  extern void
  BView_FillTriangle (void *view, int x1, int y1,
		      int x2, int y2, int x3, int y3);

  extern void
  BView_StrokeRectangle (void *view, int x, int y, int width, int height);

  extern void
  BView_SetViewColor (void *view, uint32_t color);

  extern void
  BView_ClipToRect (void *view, int x, int y, int width, int height);

  extern void
  BView_ClipToInverseRect (void *view, int x, int y, int width, int height);

  extern void
  BView_StrokeLine (void *view, int sx, int sy, int tx, int ty);

  extern void
  BView_CopyBits (void *view, int x, int y, int width, int height,
		  int tox, int toy, int towidth, int toheight);

  extern void
  BView_DrawBitmap (void *view, void *bitmap, int x, int y,
		    int width, int height, int vx, int vy, int vwidth,
		    int vheight);

  extern void
  BView_DrawBitmapWithEraseOp (void *view, void *bitmap, int x,
			       int y, int width, int height);

  extern void
  BView_DrawMask (void *src, void *view,
		  int x, int y, int width, int height,
		  int vx, int vy, int vwidth, int vheight,
		  uint32_t color);

  extern void
  BView_InvertRect (void *view, int x, int y, int width, int height);

  extern void *
  BBitmap_transform_bitmap (void *bitmap, void *mask, uint32_t m_color,
			    double rot, int desw, int desh);

  extern void
  BScreen_px_dim (int *width, int *height);

  extern void
  BView_resize_to (void *view, int width, int height);

  /* Functions for creating and freeing cursors.  */
  extern void *
  BCursor_create_default (void);

  extern void *
  BCursor_from_id (enum haiku_cursor cursor);

  extern void *
  BCursor_create_modeline (void);

  extern void *
  BCursor_create_i_beam (void);

  extern void *
  BCursor_create_progress_cursor (void);

  extern void *
  BCursor_create_grab (void);

  extern void
  BCursor_delete (void *cursor);

  extern void
  BView_set_view_cursor (void *view, void *cursor);

  extern void
  BWindow_Flush (void *window);

  extern void *
  BScrollBar_make_for_view (void *view, int horizontal_p,
			    int x, int y, int x1, int y1,
			    void *scroll_bar_ptr);

  extern void
  BScrollBar_delete (void *sb);

  extern void
  BView_move_frame (void *view, int x, int y, int x1, int y1);

  extern void
  BView_scroll_bar_update (void *sb, int portion, int whole, int position,
			   int dragging, bool can_overscroll);

  extern int
  BScrollBar_default_size (int horizontal_p);

  extern void
  BView_invalidate (void *view);

  extern void
  BView_draw_lock (void *view, bool invalidate_region,
		   int x, int y, int width, int height);

  extern void
  BView_invalidate_region (void *view, int x, int y, int width, int height);

  extern void
  BView_draw_unlock (void *view);

  extern void
  BWindow_center_on_screen (void *window);

  extern void
  BView_mouse_moved (void *view, int x, int y, uint32_t transit);

  extern void
  BView_mouse_down (void *view, int x, int y);

  extern void
  BView_mouse_up (void *view, int x, int y);

  extern void
  BBitmap_import_fringe_bitmap (void *bitmap, unsigned short *bits,
				int wd, int h);

  extern void
  BBitmap_import_mono_bits (void *bitmap, void *bits, int wd, int h);

  extern void
  haiku_font_pattern_free (struct haiku_font_pattern *pt);

  extern struct haiku_font_pattern *
  BFont_find (struct haiku_font_pattern *pt);

  extern int
  BFont_open_pattern (struct haiku_font_pattern *pat, void **font, float size);

  extern void
  BFont_populate_fixed_family (struct haiku_font_pattern *ptn);

  extern void
  BFont_populate_plain_family (struct haiku_font_pattern *ptn);

  extern void
  BView_publish_scroll_bar (void *view, int x, int y, int width, int height);

  extern void
  BView_forget_scroll_bar (void *view, int x, int y, int width, int height);

  extern bool
  BView_inside_scroll_bar (void *view, int x, int y);

  extern void
  BView_get_mouse (void *view, int *x, int *y);

  extern void
  BView_convert_to_screen (void *view, int *x, int *y);

  extern void
  BView_convert_from_screen (void *view, int *x, int *y);

  extern void
  BWindow_change_decoration (void *window, int decorate_p);

  extern void
  BWindow_set_tooltip_decoration (void *window);

  extern void
  BWindow_set_avoid_focus (void *window, int avoid_focus_p);

  extern void
  BView_emacs_delete (void *view);

  extern uint32_t
  haiku_current_workspace (void);

  extern uint32_t
  BWindow_workspaces (void *window);

  extern void *
  BPopUpMenu_new (const char *name);

  extern void
  BMenu_add_item (void *menu, const char *label, void *ptr, bool enabled_p,
		  bool marked_p, bool mbar_p, void *mbw_ptr, const char *key,
		  const char *help);

  extern void
  BMenu_add_separator (void *menu);

  extern void *
  BMenu_new_submenu (void *menu, const char *label, bool enabled_p);

  extern void *
  BMenu_new_menu_bar_submenu (void *menu, const char *label);

  extern int
  BMenu_count_items (void *menu);

  extern void *
  BMenu_item_at (void *menu, int idx);

  extern void *
  BMenu_run (void *menu, int x, int y,
	     void (*run_help_callback) (void *, void *),
	     void (*block_input_function) (void),
	     void (*unblock_input_function) (void),
	     struct timespec (*process_pending_signals_function) (void),
	     void *run_help_callback_data);

  extern void
  BPopUpMenu_delete (void *menu);

  extern void *
  BMenuBar_new (void *view);

  extern void
  BMenu_delete_all (void *menu);

  extern void
  BMenuBar_delete (void *menubar);

  extern void
  BMenu_item_set_label (void *item, const char *label);

  extern void *
  BMenu_item_get_menu (void *item);

  extern void
  BMenu_delete_from (void *menu, int start, int count);

  extern void
  haiku_ring_bell (void);

  extern void *
  BAlert_new (const char *text, enum haiku_alert_type type);

  extern void *
  BAlert_add_button (void *alert, const char *text);

  extern void
  BAlert_set_offset_spacing (void *alert);

  extern int32
  BAlert_go (void *alert,
	     void (*block_input_function) (void),
	     void (*unblock_input_function) (void),
	     void (*process_pending_signals_function) (void));

  extern void
  BButton_set_enabled (void *button, int enabled_p);

  extern void
  BView_set_tooltip (void *view, const char *tooltip);

  extern void
  BAlert_delete (void *alert);

  extern void
  BScreen_res (double *rrsx, double *rrsy);

  extern void
  EmacsWindow_parent_to (void *window, void *other_window);

  extern void
  EmacsWindow_unparent (void *window);

  extern int
  BFont_string_width (void *font, const char *utf8);

  extern void
  be_get_version_string (char *version, int len);

  extern int
  be_get_display_planes (void);

  extern int
  be_get_display_color_cells (void);

  extern void
  be_warp_pointer (int x, int y);

  extern void
  EmacsWindow_move_weak_child (void *window, void *child, int xoff, int yoff);

  extern void
  EmacsView_set_up_double_buffering (void *vw);

  extern void
  EmacsView_disable_double_buffering (void *vw);

  extern void
  EmacsView_flip_and_blit (void *vw);

  extern int
  EmacsView_double_buffered_p (void *vw);

  extern char *
  be_popup_file_dialog (int open_p, const char *default_dir, int must_match_p,
			int dir_only_p,	void *window, const char *save_text,
			const char *prompt,
			void (*block_input_function) (void),
			void (*unblock_input_function) (void),
			void (*maybe_quit_function) (void));

  extern void
  record_c_unwind_protect_from_cxx (void (*) (void *), void *);

  extern specpdl_ref c_specpdl_idx_from_cxx (void);

  extern void
  c_unbind_to_nil_from_cxx (specpdl_ref idx);

  extern void
  BWindow_zoom (void *window);

  extern void
  EmacsWindow_make_fullscreen (void *window, int fullscreen_p);

  extern void
  EmacsWindow_unzoom (void *window);

#ifdef HAVE_NATIVE_IMAGE_API
  extern int
  be_can_translate_type_to_bitmap_p (const char *mime);

  extern void *
  be_translate_bitmap_from_file_name (const char *filename);

  extern void *
  be_translate_bitmap_from_memory (const void *buf, size_t bytes);
#endif

  extern void
  BMenuBar_start_tracking (void *mbar);

  extern size_t
  BBitmap_bytes_length (void *bitmap);

  extern void
  BView_show_tooltip (void *view);

#ifdef USE_BE_CAIRO
  extern cairo_t *
  EmacsView_cairo_context (void *view);

  extern void
  BView_cr_dump_clipping (void *view, cairo_t *ctx);

  extern void
  EmacsWindow_begin_cr_critical_section (void *window);

  extern void
  EmacsWindow_end_cr_critical_section (void *window);
#endif

  extern void
  BView_set_and_show_sticky_tooltip (void *view, const char *tooltip,
				     int x, int y);

  extern void
  BMenu_add_title (void *menu, const char *text);

  extern int
  be_plain_font_height (void);

  extern int
  be_string_width_with_plain_font (const char *str);

  extern int
  be_get_display_screens (void);

  extern void
  BWindow_set_min_size (void *window, int width, int height);

  extern void
  BWindow_set_size_alignment (void *window, int align_width, int align_height);

  extern void
  BWindow_sync (void *window);

  extern void
  BWindow_send_behind (void *window, void *other_window);

  extern bool
  BWindow_is_active (void *window);

  extern bool
  be_use_subpixel_antialiasing (void);

  extern void
  BWindow_set_override_redirect (void *window, bool override_redirect_p);

  extern const char *
  be_find_setting (const char *name);

  extern void
  EmacsWindow_signal_menu_update_complete (void *window);

  extern haiku_font_family_or_style *
  be_list_font_families (size_t *length);

  extern void
  BWindow_dimensions (void *window, int *width, int *height);

  extern void
  BMessage_delete (void *message);

  extern bool
  be_drag_message (void *view, void *message, bool allow_same_view,
		   void (*block_input_function) (void),
		   void (*unblock_input_function) (void),
		   void (*process_pending_signals_function) (void),
		   bool (*should_quit_function) (void));

  extern bool
  be_drag_and_drop_in_progress (void);

#ifdef __cplusplus
  extern void *
  find_appropriate_view_for_draw (void *vw);
}

extern _Noreturn void
gui_abort (const char *msg);
#endif /* _cplusplus */

/* Borrowed from X.Org keysymdef.h */
#define XK_BackSpace                     0xff08  /* Back space, back char */
#define XK_Tab                           0xff09
#define XK_Linefeed                      0xff0a  /* Linefeed, LF */
#define XK_Clear                         0xff0b
#define XK_Return                        0xff0d  /* Return, enter */
#define XK_Pause                         0xff13  /* Pause, hold */
#define XK_Scroll_Lock                   0xff14
#define XK_Sys_Req                       0xff15
#define XK_Escape                        0xff1b
#define XK_Delete                        0xffff  /* Delete, rubout */
#define XK_Home                          0xff50
#define XK_Left                          0xff51  /* Move left, left arrow */
#define XK_Up                            0xff52  /* Move up, up arrow */
#define XK_Right                         0xff53  /* Move right, right arrow */
#define XK_Down                          0xff54  /* Move down, down arrow */
#define XK_Prior                         0xff55  /* Prior, previous */
#define XK_Page_Up                       0xff55
#define XK_Next                          0xff56  /* Next */
#define XK_Page_Down                     0xff56
#define XK_End                           0xff57  /* EOL */
#define XK_Begin                         0xff58  /* BOL */
#define XK_Select                        0xff60  /* Select, mark */
#define XK_Print                         0xff61
#define XK_Execute                       0xff62  /* Execute, run, do */
#define XK_Insert                        0xff63  /* Insert, insert here */
#define XK_Undo                          0xff65
#define XK_Redo                          0xff66  /* Redo, again */
#define XK_Menu                          0xff67
#define XK_Find                          0xff68  /* Find, search */
#define XK_Cancel                        0xff69  /* Cancel, stop, abort, exit */
#define XK_Help                          0xff6a  /* Help */
#define XK_Break                         0xff6b
#define XK_Mode_switch                   0xff7e  /* Character set switch */
#define XK_script_switch                 0xff7e  /* Alias for mode_switch */
#define XK_Num_Lock                      0xff7f
#define XK_F1                            0xffbe

#endif /* _HAIKU_SUPPORT_H_ */
