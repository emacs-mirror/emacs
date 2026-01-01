/* Haiku window system support.  Hey Emacs, this is -*- C++ -*-
   Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
    CURSOR_ID_SYSTEM_DEFAULT		   = 1,
    CURSOR_ID_CONTEXT_MENU		   = 3,
    CURSOR_ID_COPY			   = 4,
    CURSOR_ID_CREATE_LINK		   = 29,
    CURSOR_ID_CROSS_HAIR		   = 5,
    CURSOR_ID_FOLLOW_LINK		   = 6,
    CURSOR_ID_GRAB			   = 7,
    CURSOR_ID_GRABBING			   = 8,
    CURSOR_ID_HELP			   = 9,
    CURSOR_ID_I_BEAM			   = 2,
    CURSOR_ID_I_BEAM_HORIZONTAL		   = 10,
    CURSOR_ID_MOVE			   = 11,
    CURSOR_ID_NO_CURSOR			   = 12,
    CURSOR_ID_NOT_ALLOWED		   = 13,
    CURSOR_ID_PROGRESS			   = 14,
    CURSOR_ID_RESIZE_NORTH		   = 15,
    CURSOR_ID_RESIZE_EAST		   = 16,
    CURSOR_ID_RESIZE_SOUTH		   = 17,
    CURSOR_ID_RESIZE_WEST		   = 18,
    CURSOR_ID_RESIZE_NORTH_EAST		   = 19,
    CURSOR_ID_RESIZE_NORTH_WEST		   = 20,
    CURSOR_ID_RESIZE_SOUTH_EAST		   = 21,
    CURSOR_ID_RESIZE_SOUTH_WEST		   = 22,
    CURSOR_ID_RESIZE_NORTH_SOUTH	   = 23,
    CURSOR_ID_RESIZE_EAST_WEST		   = 24,
    CURSOR_ID_RESIZE_NORTH_EAST_SOUTH_WEST = 25,
    CURSOR_ID_RESIZE_NORTH_WEST_SOUTH_EAST = 26,
    CURSOR_ID_ZOOM_IN			   = 27,
    CURSOR_ID_ZOOM_OUT			   = 28
  };

enum haiku_z_group
  {
    Z_GROUP_ABOVE,
    Z_GROUP_NONE,
    Z_GROUP_BELOW,
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
    MENU_BAR_CLICK,
    MENU_BAR_OPEN,
    MENU_BAR_SELECT_EVENT,
    MENU_BAR_CLOSE,
    MENU_BAR_HELP_EVENT,
    ZOOM_EVENT,
    DRAG_AND_DROP_EVENT,
    APP_QUIT_REQUESTED_EVENT,
    DUMMY_EVENT,
    SCREEN_CHANGED_EVENT,
    MENU_BAR_LEFT,
    CLIPBOARD_CHANGED_EVENT,
    FONT_CHANGE_EVENT,
    NOTIFICATION_CLICK_EVENT,
  };

struct haiku_clipboard_changed_event
{
  char dummy;
};

struct haiku_screen_changed_event
{
  bigtime_t when;
};

struct haiku_quit_requested_event
{
  void *window;
};

struct haiku_resize_event
{
  void *window;
  float width;
  float height;
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

enum haiku_modifier_specification
  {
    HAIKU_MODIFIER_ALT	 = 1,
    HAIKU_MODIFIER_CTRL	 = (1 << 1),
    HAIKU_MODIFIER_SHIFT = (1 << 2),
    HAIKU_MODIFIER_SUPER = (1 << 3),
  };

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

struct haiku_menu_bar_click_event
{
  void *window;
  int x, y;
};

struct haiku_button_event
{
  void *window;
  void *scroll_bar;
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
  int x, y;
  int decorator_width;
  int decorator_height;
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
  int fullscreen_mode;
};

enum haiku_font_specification
  {
    FSPEC_FAMILY      = 1,
    FSPEC_STYLE	      = 1 << 1,
    FSPEC_SLANT	      = 1 << 2,
    FSPEC_WEIGHT      = 1 << 3,
    FSPEC_SPACING     = 1 << 4,
    FSPEC_WANTED      = 1 << 5,
    FSPEC_NEED_ONE_OF = 1 << 6,
    FSPEC_WIDTH	      = 1 << 7,
    FSPEC_LANGUAGE    = 1 << 8,
    FSPEC_INDICES     = 1 << 9,
    FSPEC_ANTIALIAS   = 1 << 10,
  };

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

enum haiku_font_weight
  {
    NO_WEIGHT	      = -1,
    HAIKU_THIN	      = 0,
    HAIKU_EXTRALIGHT  = 40,
    HAIKU_LIGHT	      = 50,
    HAIKU_SEMI_LIGHT  = 75,
    HAIKU_REGULAR     = 100,
    HAIKU_SEMI_BOLD   = 180,
    HAIKU_BOLD	      = 200,
    HAIKU_EXTRA_BOLD  = 205,
    HAIKU_BOOK	      = 400,
    HAIKU_HEAVY	      = 800,
    HAIKU_ULTRA_HEAVY = 900,
    HAIKU_BLACK	      = 1000,
    HAIKU_MEDIUM      = 2000,
  };

enum haiku_fullscreen_mode
  {
    FULLSCREEN_MODE_NONE,
    FULLSCREEN_MODE_WIDTH,
    FULLSCREEN_MODE_HEIGHT,
    FULLSCREEN_MODE_BOTH,
    FULLSCREEN_MODE_MAXIMIZED,
  };

struct haiku_font_pattern
{
  /* Bitmask indicating which fields are set.  */
  int specified;

  /* The next font in this list.  */
  struct haiku_font_pattern *next;

  /* The last font in the list during font lookup.  */
  struct haiku_font_pattern *last;

  /* The next font in the list whose family differs from this one.
     Only valid during font lookup.  */
  struct haiku_font_pattern *next_family;

  /* The family of the font.  */
  haiku_font_family_or_style family;

  /* The style of the font.  */
  haiku_font_family_or_style style;

  /* Whether or the font is monospace.  */
  int mono_spacing_p;

  /* The slant of the font.  */
  enum haiku_font_slant slant;

  /* The width of the font.  */
  enum haiku_font_width width;

  /* The language of the font.  Used during font lookup.  */
  enum haiku_font_language language;

  /* The weight of the font.  */
  enum haiku_font_weight weight;

  /* List of characters that must be present in the font for the match
     to succeed.  */
  int *wanted_chars;

  /* The number of characters in `wanted_chars'.  */
  int want_chars_len;

  /* List of characters.  The font must fulfill at least one of
     them for the match to succeed.  */
  int *need_one_of;

  /* The number of characters in `need_one_of'.  */
  int need_one_of_len;

  /* The index of the family of the font this pattern represents.  */
  int family_index;

  /* The index of the style of the font this pattern represents.  */
  int style_index;

  /* Temporary field used during font enumeration.  */
  int oblique_seen_p;

  /* Whether or not to enable antialiasing in the font.  This field is
     special in that it's not handled by `BFont_open_pattern'.  */
  int use_antialiasing;
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
};

enum haiku_what_font
  {
    FIXED_FAMILY,
    FIXED_STYLE,
    DEFAULT_FAMILY,
    DEFAULT_STYLE,
  };

struct haiku_font_change_event
{
  /* New family, style and size of the font.  */
  haiku_font_family_or_style new_family;
  haiku_font_family_or_style new_style;
  int new_size;

  /* What changed.  FIXED_FAMILY means this is the new fixed font.
     DEFAULT_FAMILY means this is the new plain font.  The other enums
     have no meaning.  */
  enum haiku_what_font what;
};

struct haiku_notification_click_event
{
  /* ID uniquely designating a single notification.  */
  intmax_t id;
};

struct haiku_session_manager_reply
{
  bool quit_reply;
};

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

#define BE_RECT_HEIGHT(rect)	(ceil (((rect).bottom - (rect).top) + 1))
#define BE_RECT_WIDTH(rect)	(ceil (((rect).right - (rect).left) + 1))
#endif /* __cplusplus */

#ifdef __cplusplus
extern "C"
{
#endif
#include <OS.h>

#ifdef __cplusplus
typedef void *haiku;

extern void haiku_put_pixel (haiku, int, int, unsigned long);
extern unsigned long haiku_get_pixel (haiku, int, int);
#endif

extern port_id port_application_to_emacs;
extern port_id port_popup_menu_to_emacs;
extern port_id port_emacs_to_session_manager;

extern void haiku_io_init (void);
extern void haiku_io_init_in_app_thread (void);

extern void haiku_read_size (ssize_t *, bool);

extern int haiku_read (enum haiku_event_type *, void *, ssize_t);
extern int haiku_read_with_timeout (enum haiku_event_type *, void *, ssize_t,
				    bigtime_t, bool);
extern int haiku_write (enum haiku_event_type, void *);
extern int haiku_write_without_signal (enum haiku_event_type, void *, bool);

extern void rgb_color_hsl (uint32_t, double *, double *, double *);
extern void hsl_color_rgb (double, double, double, uint32_t *);

extern void *BBitmap_new (int, int, int);
extern void *BBitmap_data (void *);
extern int BBitmap_convert (void *, void **);
extern void be_draw_cross_on_pixmap (void *, int, int, int, int,
				     uint32_t);

extern void BBitmap_free (void *);

extern void BBitmap_dimensions (void *, int *, int *,  int *, int *,
				int32_t *, int *);
extern void *BApplication_setup (void);
extern void *BWindow_new (void **);
extern void BWindow_quit (void *);

extern void BWindow_set_offset (void *, int, int);
extern void BWindow_iconify (void *);
extern void BWindow_set_visible (void *, int);
extern void BWindow_retitle (void *, const char *);
extern void BWindow_resize (void *, int, int);
extern void BWindow_activate (void *);
extern void BWindow_center_on_screen (void *);
extern void BWindow_change_decoration (void *, int);
extern void BWindow_set_tooltip_decoration (void *);
extern void BWindow_set_avoid_focus (void *, int);
extern void BWindow_set_size_alignment (void *, int, int);
extern void BWindow_sync (void *);
extern void BWindow_send_behind (void *, void *);
extern bool BWindow_is_active (void *);
extern void BWindow_set_override_redirect (void *, bool);
extern void BWindow_dimensions (void *, int *, int *);
extern void BWindow_set_z_group (void *, enum haiku_z_group);
extern void BWindow_set_sticky (void *, bool);
extern void BWindow_Flush (void *);

extern void BFont_close (void *);
extern void BFont_metrics (void *, int *, int *, int *, int *,
			   int *, int *, int *, int *, int *, int *);
extern int BFont_have_char_p (void *, int32_t);
extern int BFont_have_char_block (void *, int32_t, int32_t);
extern void BFont_char_bounds (void *, const char *, int *, int *, int *);
extern void BFont_nchar_bounds (void *, const char *, int *, int *,
				int *, int32_t);
extern struct haiku_font_pattern *BFont_find (struct haiku_font_pattern *);

extern void BView_StartClip (void *);
extern void BView_EndClip (void *);
extern void BView_SetHighColor (void *, uint32_t);
extern void BView_SetLowColor (void *, uint32_t);
extern void BView_SetPenSize (void *, int);
extern void BView_SetFont (void *, void *);
extern void BView_MovePenTo (void *, int, int);
extern void BView_DrawString (void *, const char *, ptrdiff_t);
extern void BView_DrawChar (void *, char);
extern void BView_FillRectangle (void *, int, int, int, int);
extern void BView_FillRectangleAbs (void *, int, int, int, int);
extern void BView_FillTriangle (void *, int, int, int, int, int, int);
extern void BView_StrokeRectangle (void *, int, int, int, int);
extern void BView_SetViewColor (void *, uint32_t);
extern void BView_ClipToRect (void *, int, int, int, int);
extern void BView_ClipToInverseRect (void *, int, int, int, int);
extern void BView_StrokeLine (void *, int, int, int, int);
extern void BView_CopyBits (void *, int, int, int, int, int, int, int, int);
extern void BView_InvertRect (void *, int, int, int, int);
extern void BView_DrawBitmap (void *, void *, int, int, int, int, int, int,
			      int, int, bool);
extern void BView_DrawBitmapWithEraseOp (void *, void *, int, int, int, int);
extern void BView_DrawBitmapTiled (void *, void *, int, int,
				   int, int, int, int, int, int);

extern void BView_resize_to (void *, int, int);
extern void BView_set_view_cursor (void *, void *);
extern void BView_move_frame (void *, int, int, int, int);
extern void BView_scroll_bar_update (void *, int, int, int, int, bool);

extern void *be_transform_bitmap (void *, void *, uint32_t, double,
				  int, int, bool);
extern void be_apply_affine_transform (void *, double, double, double,
				       double, double, double);
extern void be_apply_inverse_transform (double (*)[3], int, int, int *, int *);
extern void be_draw_image_mask (void *, void *, int, int, int, int, int, int,
				int, int, uint32_t);
extern void be_draw_bitmap_with_mask (void *, void *, void *, int, int, int,
				      int, int, int, int, int, bool);

extern void be_get_display_resolution (double *, double *);
extern void be_get_screen_dimensions (int *, int *);

/* Functions for creating and freeing cursors.  */
extern void *be_create_cursor_from_id (int);
extern void *be_create_pixmap_cursor (void *, int, int);
extern void be_delete_cursor (void *);

extern void *be_make_scroll_bar_for_view (void *, int, int, int, int, int);
extern void BScrollBar_delete (void *);
extern int BScrollBar_default_size (int);

extern void BView_invalidate (void *);
extern void BView_draw_lock (void *, bool, int, int, int, int);
extern void BView_invalidate_region (void *, int, int, int, int);
extern void BView_draw_unlock (void *);
extern void BBitmap_import_fringe_bitmap (void *, unsigned short *, int, int);

extern void haiku_font_pattern_free (struct haiku_font_pattern *);

extern int BFont_open_pattern (struct haiku_font_pattern *, void **, float);
extern void BFont_populate_fixed_family (struct haiku_font_pattern *);
extern void BFont_populate_plain_family (struct haiku_font_pattern *);

extern void BView_publish_scroll_bar (void *, int, int, int, int);
extern void BView_forget_scroll_bar (void *, int, int, int, int);
extern bool BView_inside_scroll_bar (void *, int, int);
extern void BView_get_mouse (void *, int *, int *);
extern void BView_convert_to_screen (void *, int *, int *);
extern void BView_convert_from_screen (void *, int *, int *);

extern void BView_emacs_delete (void *);

extern void *BPopUpMenu_new (const char *);

extern void BMenu_add_item (void *, const char *, void *, bool,
			    bool, bool, void *, const char *,
			    const char *);
extern void BMenu_add_separator (void *);
extern void *BMenu_new_submenu (void *, const char *, bool);
extern void *BMenu_new_menu_bar_submenu (void *, const char *);
extern int BMenu_count_items (void *);
extern void *BMenu_item_at (void *, int);
extern void *BMenu_run (void *, int, int, void (*) (void *, void *),
			void (*) (void), void (*) (void),
			struct timespec (*) (void), void *);
extern void BPopUpMenu_delete (void *);
extern void *BMenuBar_new (void *);
extern void BMenu_delete_all (void *);
extern void BMenuBar_delete (void *);
extern void BMenu_item_set_label (void *, const char *);
extern void *BMenu_item_get_menu (void *);
extern void BMenu_delete_from (void *, int, int);

extern void haiku_ring_bell (void);

extern void *BAlert_new (const char *, enum haiku_alert_type);
extern void *BAlert_add_button (void *, const char *);
extern void BAlert_set_offset_spacing (void *);
extern int32 BAlert_go (void *, void (*) (void), void (*) (void),
			void (*) (void));
extern void BButton_set_enabled (void *, int);
extern void BView_set_tooltip (void *, const char *);
extern void BView_show_tooltip (void *);
extern void be_show_sticky_tooltip (void *, const char *, int, int);

extern void BAlert_delete (void *);

extern void EmacsWindow_parent_to (void *, void *);
extern void EmacsWindow_unparent (void *);
extern void EmacsWindow_move_weak_child (void *, void *, int, int);

extern void be_get_version_string (char *, int);
extern int be_get_display_planes (void);
extern int be_get_display_color_cells (void);
extern bool be_is_display_grayscale (void);
extern void be_warp_pointer (int, int);

extern bool haiku_should_pass_control_tab_to_system (void);

extern void EmacsView_set_up_double_buffering (void *);
extern void EmacsView_disable_double_buffering (void *);
extern void EmacsView_flip_and_blit (void *);
extern int EmacsView_double_buffered_p (void *);

extern char *be_popup_file_dialog (int, const char *, int,
				   int, void *, const char *,
				   const char *, void (*) (void));

#ifdef HAVE_NATIVE_IMAGE_API
extern int be_can_translate_type_to_bitmap_p (const char *);
extern void *be_translate_bitmap_from_file_name (const char *);
extern void *be_translate_bitmap_from_memory (const void *, size_t);
#endif

extern bool BMenuBar_start_tracking (void *);
extern size_t BBitmap_bytes_length (void *);

#ifdef USE_BE_CAIRO
extern cairo_t *EmacsView_cairo_context (void *);
extern void BView_cr_dump_clipping (void *, cairo_t *);
extern void EmacsWindow_begin_cr_critical_section (void *);
extern void EmacsWindow_end_cr_critical_section (void *);
#endif

extern void BMenu_add_title (void *, const char *);

extern int be_plain_font_height (void);
extern int be_string_width_with_plain_font (const char *);
extern void be_init_font_data (void);
extern void be_evict_font_cache (void);
extern int be_get_display_screens (void);
extern bool be_use_subpixel_antialiasing (void);
extern const char *be_find_setting (const char *);
extern haiku_font_family_or_style *be_list_font_families (size_t *);
extern void be_font_style_to_flags (const char *, struct haiku_font_pattern *);
extern void *be_open_font_at_index (int, int, float);
extern void be_set_font_antialiasing (void *, bool);
extern int be_get_ui_color (const char *, uint32_t *);

extern void BMessage_delete (void *);

extern bool be_drag_message (void *, void *, bool, void (*) (void),
			     void (*) (void), void (*) (void),
			     bool (*) (void));
extern bool be_drag_and_drop_in_progress (void);

extern bool be_replay_menu_bar_event (void *, struct haiku_menu_bar_click_event *);
extern bool be_select_font (void (*) (void), bool (*) (void),
			    haiku_font_family_or_style *,
			    haiku_font_family_or_style *,
			    int *, bool, int, int, int,
			    bool, bool *);

extern int be_find_font_indices (struct haiku_font_pattern *, int *, int *);
extern status_t be_roster_launch (const char *, const char *, char **,
				  ptrdiff_t, void *, team_id *);
extern void be_get_window_decorator_dimensions (void *, int *, int *, int *, int *);
extern void be_get_window_decorator_frame (void *, int *, int *, int *, int *);
extern void be_send_move_frame_event (void *);
extern void be_set_window_fullscreen_mode (void *, enum haiku_fullscreen_mode);

extern status_t be_write_node_message (const char *, const char *, void *);
extern void be_send_message (const char *, void *);

extern void be_lock_window (void *);
extern void be_unlock_window (void *);
extern bool be_get_explicit_workarea (int *, int *, int *, int *);
extern void be_clear_grab_view (void);
extern void be_set_use_frame_synchronization (void *, bool);

extern void be_listen_font_settings (void);

extern bool be_lock_font_defaults (void);
extern const char *be_get_font_default (enum haiku_what_font);
extern int be_get_font_size (enum haiku_what_font);
extern void be_unlock_font_defaults (void);
#ifdef __cplusplus
}

extern _Noreturn void gui_abort (const char *);
extern void *find_appropriate_view_for_draw (void *);
#endif /* _cplusplus */

#endif /* _HAIKU_SUPPORT_H_ */

// Local Variables:
// eval: (setf (alist-get 'inextern-lang c-offsets-alist) 0)
// End:
