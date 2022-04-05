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

#include <config.h>

#include "dispextern.h"
#include "frame.h"
#include "lisp.h"
#include "haikugui.h"
#include "keyboard.h"
#include "haikuterm.h"
#include "blockinput.h"
#include "termchar.h"
#include "termhooks.h"
#include "menu.h"
#include "buffer.h"
#include "haiku_support.h"
#include "thread.h"
#include "window.h"

#include <math.h>
#include <stdlib.h>

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

/* Minimum and maximum values used for Haiku scroll bars.  */
#define BE_SB_MAX 12000000

struct haiku_display_info *x_display_list = NULL;
extern frame_parm_handler haiku_frame_parm_handlers[];

static void **fringe_bmps;
static int max_fringe_bmp = 0;

static Lisp_Object rdb;

struct unhandled_event
{
  struct unhandled_event *next;
  enum haiku_event_type type;
  uint8_t buffer[200];
};

static bool any_help_event_p;

char *
get_keysym_name (int keysym)
{
  static char value[16];
  sprintf (value, "%d", keysym);
  return value;
}

static struct frame *
haiku_window_to_frame (void *window)
{
  Lisp_Object tail, tem;
  struct frame *f;

  FOR_EACH_FRAME (tail, tem)
    {
      f = XFRAME (tem);
      if (!FRAME_HAIKU_P (f))
	continue;

      eassert (FRAME_DISPLAY_INFO (f) == x_display_list);

      if (FRAME_HAIKU_WINDOW (f) == window)
	return f;
    }

  return 0;
}

static void
haiku_coords_from_parent (struct frame *f, int *x, int *y)
{
  struct frame *p = FRAME_PARENT_FRAME (f);
  eassert (p);

  for (struct frame *parent = p; parent;
       parent = FRAME_PARENT_FRAME (parent))
    {
      *x -= parent->left_pos;
      *y -= parent->top_pos;
    }
}

static void
haiku_toolkit_position (struct frame *f, int x, int y,
			bool *menu_bar_p, bool *tool_bar_p)
{
  if (FRAME_OUTPUT_DATA (f)->menubar)
    *menu_bar_p = (x >= 0 && x < FRAME_PIXEL_WIDTH (f)
		   && y >= 0 && y < FRAME_MENU_BAR_HEIGHT (f));
}

static void
haiku_delete_terminal (struct terminal *terminal)
{
  struct haiku_display_info *dpyinfo = terminal->display_info.haiku;
  struct terminal *t;

  if (!terminal->name)
    return;

  block_input ();

  be_app_quit ();
  delete_port (port_application_to_emacs);

  BCursor_delete (dpyinfo->text_cursor);
  BCursor_delete (dpyinfo->nontext_cursor);
  BCursor_delete (dpyinfo->modeline_cursor);
  BCursor_delete (dpyinfo->hand_cursor);
  BCursor_delete (dpyinfo->hourglass_cursor);
  BCursor_delete (dpyinfo->horizontal_drag_cursor);
  BCursor_delete (dpyinfo->vertical_drag_cursor);
  BCursor_delete (dpyinfo->left_edge_cursor);
  BCursor_delete (dpyinfo->top_left_corner_cursor);
  BCursor_delete (dpyinfo->top_edge_cursor);
  BCursor_delete (dpyinfo->top_right_corner_cursor);
  BCursor_delete (dpyinfo->right_edge_cursor);
  BCursor_delete (dpyinfo->bottom_right_corner_cursor);
  BCursor_delete (dpyinfo->bottom_edge_cursor);
  BCursor_delete (dpyinfo->bottom_left_corner_cursor);
  BCursor_delete (dpyinfo->no_cursor);

  /* Close all frames and delete the generic struct terminal.  */
  for (t = terminal_list; t; t = t->next_terminal)
    {
      if (t->type == output_haiku && t->display_info.haiku == dpyinfo)
	{
	  delete_terminal (t);
	  break;
	}
    }
  unblock_input ();
}

static const char *
get_string_resource (void *ignored, const char *name, const char *class)
{
  const char *native;

  if (!name)
    return NULL;

  Lisp_Object lval = assoc_no_quit (build_string (name), rdb);

  if (!NILP (lval))
    return SSDATA (XCDR (lval));

  if ((native = be_find_setting (name)))
    return native;

  return NULL;
}

static void
haiku_update_size_hints (struct frame *f)
{
  int base_width, base_height;
  eassert (FRAME_HAIKU_P (f) && FRAME_HAIKU_WINDOW (f));

  if (f->tooltip)
    return;

  base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
  base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0);

  block_input ();
  BWindow_set_size_alignment (FRAME_HAIKU_WINDOW (f),
			      frame_resize_pixelwise ? 1 : FRAME_COLUMN_WIDTH (f),
			      frame_resize_pixelwise ? 1 : FRAME_LINE_HEIGHT (f));
  BWindow_set_min_size (FRAME_HAIKU_WINDOW (f), base_width,
			base_height
			+ FRAME_TOOL_BAR_HEIGHT (f)
			+ FRAME_MENU_BAR_HEIGHT (f));
  unblock_input ();
}

static void
haiku_clip_to_string (struct glyph_string *s)
{
  struct haiku_rect r[2];
  int n = get_glyph_string_clip_rects (s, (struct haiku_rect *) &r, 2);

  if (n)
    {
      /* If n[FOO].width is 0, it means to not draw at all, so set the
	 clipping to some impossible value.  */
      if (r[0].width <= 0)
	BView_ClipToRect (FRAME_HAIKU_VIEW (s->f),
			  FRAME_PIXEL_WIDTH (s->f),
			  FRAME_PIXEL_HEIGHT (s->f),
			  10, 10);
      else
	{
	  BView_ClipToRect (FRAME_HAIKU_VIEW (s->f), r[0].x,
			    r[0].y, r[0].width, r[0].height);
	  BView_invalidate_region (FRAME_HAIKU_VIEW (s->f), r[0].x,
				   r[0].y, r[0].width, r[0].height);
	}
    }

  if (n > 1)
    {
      /* If n[FOO].width is 0, it means to not draw at all, so set the
	 clipping to some impossible value.  */
      if (r[1].width <= 0)
	BView_ClipToRect (FRAME_HAIKU_VIEW (s->f),
			  FRAME_PIXEL_WIDTH (s->f),
			  FRAME_PIXEL_HEIGHT (s->f),
			  10, 10);
      else
	{
	  BView_ClipToRect (FRAME_HAIKU_VIEW (s->f), r[1].x, r[1].y,
			    r[1].width, r[1].height);
	  BView_invalidate_region (FRAME_HAIKU_VIEW (s->f), r[1].x,
				   r[1].y, r[1].width, r[1].height);
	}
    }
}

static void
haiku_clip_to_string_exactly (struct glyph_string *s, struct glyph_string *dst)
{
  BView_ClipToRect (FRAME_HAIKU_VIEW (s->f), s->x, s->y,
		    s->width, s->height);
}

static void
haiku_flip_buffers (struct frame *f)
{
  void *view = FRAME_OUTPUT_DATA (f)->view;
  block_input ();

  BView_draw_lock (view, false, 0, 0, 0, 0);
  FRAME_DIRTY_P (f) = 0;
  EmacsView_flip_and_blit (view);
  BView_draw_unlock (view);

  unblock_input ();
}

static void
haiku_frame_up_to_date (struct frame *f)
{
  block_input ();
  FRAME_MOUSE_UPDATE (f);
  if (FRAME_DIRTY_P (f) && !buffer_flipping_blocked_p ())
    haiku_flip_buffers (f);
  unblock_input ();
}

static void
haiku_buffer_flipping_unblocked_hook (struct frame *f)
{
  if (FRAME_DIRTY_P (f))
    haiku_flip_buffers (f);
}

static void
haiku_clear_frame_area (struct frame *f, int x, int y,
			int width, int height)
{
  void *vw = FRAME_HAIKU_VIEW (f);
  block_input ();
  BView_draw_lock (vw, true, x, y, width, height);
  BView_StartClip (vw);
  BView_ClipToRect (vw, x, y, width, height);
  BView_SetHighColor (vw, FRAME_BACKGROUND_PIXEL (f));
  BView_FillRectangle (vw, x, y, width, height);
  BView_EndClip (vw);
  BView_draw_unlock (vw);
  unblock_input ();
}

static void
haiku_clear_frame (struct frame *f)
{
  void *view = FRAME_HAIKU_VIEW (f);

  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  block_input ();
  BView_draw_lock (view, true, 0, 0, FRAME_PIXEL_WIDTH (f),
		   FRAME_PIXEL_HEIGHT (f));
  BView_StartClip (view);
  BView_ClipToRect (view, 0, 0, FRAME_PIXEL_WIDTH (f),
		    FRAME_PIXEL_HEIGHT (f));
  BView_SetHighColor (view, FRAME_BACKGROUND_PIXEL (f));
  BView_FillRectangle (view, 0, 0, FRAME_PIXEL_WIDTH (f) ,
		       FRAME_PIXEL_HEIGHT (f));
  BView_EndClip (view);
  BView_draw_unlock (view);
  unblock_input ();
}

/* Give frame F the font FONT-OBJECT as its default font.  The return
   value is FONT-OBJECT.  FONTSET is an ID of the fontset for the
   frame.  If it is negative, generate a new fontset from
   FONT-OBJECT.  */

static Lisp_Object
haiku_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);
  if (fontset < 0)
    fontset = fontset_from_font (font_object);

  FRAME_FONTSET (f) = fontset;
  if (FRAME_FONT (f) == font)
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;

  int ascent, descent;
  get_font_ascent_descent (font, &ascent, &descent);
  FRAME_LINE_HEIGHT (f) = ascent + descent;
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  int unit = FRAME_COLUMN_WIDTH (f);
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    FRAME_CONFIG_SCROLL_BAR_COLS (f)
      = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
  else
    FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;

  if (FRAME_HAIKU_WINDOW (f) && !FRAME_TOOLTIP_P (f))
    {
      adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
			 FRAME_LINES (f) * FRAME_LINE_HEIGHT (f),
			 3, false, Qfont);

      haiku_clear_under_internal_border (f);
    }
  return font_object;
}

static int
haiku_valid_modifier_p (Lisp_Object sym)
{
  return EQ (sym, Qcommand) || EQ (sym, Qshift)
    || EQ (sym, Qcontrol) || EQ (sym, Qoption);
}

#define MODIFIER_OR(obj, def) (haiku_valid_modifier_p (obj) ? obj : def)

static void
haiku_add_modifier (int modifier, int toput, Lisp_Object qtem, int *modifiers)
{
  if ((modifier & HAIKU_MODIFIER_ALT && EQ (qtem, Qcommand))
      || (modifier & HAIKU_MODIFIER_SHIFT && EQ (qtem, Qshift))
      || (modifier & HAIKU_MODIFIER_CTRL && EQ (qtem, Qcontrol))
      || (modifier & HAIKU_MODIFIER_SUPER && EQ (qtem, Qoption)))
    *modifiers |= toput;
}

static int
haiku_modifiers_to_emacs (int haiku_key)
{
  int modifiers = 0;
  haiku_add_modifier (haiku_key, shift_modifier,
		      MODIFIER_OR (Vhaiku_shift_keysym, Qshift), &modifiers);
  haiku_add_modifier (haiku_key, super_modifier,
		      MODIFIER_OR (Vhaiku_super_keysym, Qoption), &modifiers);
  haiku_add_modifier (haiku_key, meta_modifier,
		      MODIFIER_OR (Vhaiku_meta_keysym, Qcommand), &modifiers);
  haiku_add_modifier (haiku_key, ctrl_modifier,
		      MODIFIER_OR (Vhaiku_control_keysym, Qcontrol), &modifiers);
  return modifiers;
}

#undef MODIFIER_OR

static void
haiku_rehighlight (void)
{
  eassert (x_display_list && !x_display_list->next);

  block_input ();

  struct frame *old_hl = x_display_list->highlight_frame;

  if (x_display_list->focused_frame)
    {
      x_display_list->highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (x_display_list->focused_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (x_display_list->focused_frame))
	   : x_display_list->focused_frame);
      if (!FRAME_LIVE_P (x_display_list->highlight_frame))
	{
	  fset_focus_frame (x_display_list->focused_frame, Qnil);
	  x_display_list->highlight_frame = x_display_list->focused_frame;
	}
    }
  else
    x_display_list->highlight_frame = 0;

  if (old_hl)
    gui_update_cursor (old_hl, true);

  if (x_display_list->highlight_frame)
    gui_update_cursor (x_display_list->highlight_frame, true);
  unblock_input ();
}

static void
haiku_frame_raise_lower (struct frame *f, bool raise_p)
{
  if (raise_p)
    {
      block_input ();
      BWindow_activate (FRAME_HAIKU_WINDOW (f));
      BWindow_sync (FRAME_HAIKU_WINDOW (f));
      unblock_input ();
    }
  else
    {
      block_input ();
      BWindow_send_behind (FRAME_HAIKU_WINDOW (f), NULL);
      BWindow_sync (FRAME_HAIKU_WINDOW (f));
      unblock_input ();
    }
}

static struct frame *
haiku_mouse_or_wdesc_frame (void *window)
{
  struct frame *lm_f = (gui_mouse_grabbed (x_display_list)
			? x_display_list->last_mouse_frame
			: NULL);

  if (lm_f && !EQ (track_mouse, Qdropping)
      && !EQ (track_mouse, Qdrag_source))
    return lm_f;
  else
    {
      struct frame *w_f = haiku_window_to_frame (window);

      /* Do not return a tooltip frame.  */
      if (!w_f || FRAME_TOOLTIP_P (w_f))
	return EQ (track_mouse, Qdropping) ? lm_f : NULL;
      else
	/* When dropping it would be probably nice to raise w_f
	   here.  */
	return w_f;
    }
}

/* Set the thumb size and position of scroll bar BAR.  We are
   currently displaying PORTION out of a whole WHOLE, and our position
   POSITION.  */

static void
haiku_set_scroll_bar_thumb (struct scroll_bar *bar, int portion,
			    int position, int whole)
{
  void *scroll_bar = bar->scroll_bar;
  double top, shown, size, value;

  if (scroll_bar_adjust_thumb_portion_p)
    {
      /* We use an estimate of 30 chars per line rather than the real
         `portion' value.  This has the disadvantage that the thumb
         size is not very representative, but it makes our life a lot
         easier.  Otherwise, we have to constantly adjust the thumb
         size, which we can't always do quickly enough: while
         dragging, the size of the thumb might prevent the user from
         dragging the thumb all the way to the end.  */
      portion = WINDOW_TOTAL_LINES (XWINDOW (bar->window)) * 30;
      /* When the thumb is at the bottom, position == whole.  So we
         need to increase `whole' to make space for the thumb.  */
      whole += portion;
    }
  else
    bar->page_size = 0;

  if (whole <= 0)
    top = 0, shown = 1;
  else
    {
      top = (double) position / whole;
      shown = (double) portion / whole;
    }

  /* Slider size.  Must be in the range [1 .. MAX - MIN] where MAX
     is the scroll bar's maximum and MIN is the scroll bar's minimum
     value.  */
  size = clip_to_bounds (1, shown * BE_SB_MAX, BE_SB_MAX);

  /* Position.  Must be in the range [MIN .. MAX - SLIDER_SIZE].  */
  value = top * BE_SB_MAX;
  value = min (value, BE_SB_MAX - size);

  if (!bar->dragging && scroll_bar_adjust_thumb_portion_p)
    bar->page_size = size;

  BView_scroll_bar_update (scroll_bar, lrint (size),
			   BE_SB_MAX, ceil (value),
			   (scroll_bar_adjust_thumb_portion_p
			    ? bar->dragging : bar->dragging ? -1 : 0),
			   !scroll_bar_adjust_thumb_portion_p);
}

static void
haiku_set_horizontal_scroll_bar_thumb (struct scroll_bar *bar, int portion,
				       int position, int whole)
{
  void *scroll_bar = bar->scroll_bar;
  double size, value, shown, top;

  shown = (double) portion / whole;
  top = (double) position / whole;

  size = shown * BE_SB_MAX;
  value = top * BE_SB_MAX;

  if (!bar->dragging)
    bar->page_size = size;

  BView_scroll_bar_update (scroll_bar, lrint (size), BE_SB_MAX,
			   ceil (value), bar->dragging ? -1 : 0, true);
}

static struct scroll_bar *
haiku_scroll_bar_from_widget (void *scroll_bar, void *window)
{
  Lisp_Object tem;
  struct frame *frame = haiku_window_to_frame (window);

  if (!frame)
    return NULL;

  if (!NILP (FRAME_SCROLL_BARS (frame)))
    {
      for (tem = FRAME_SCROLL_BARS (frame); !NILP (tem);
	   tem = XSCROLL_BAR (tem)->next)
	{
	  if (XSCROLL_BAR (tem)->scroll_bar == scroll_bar)
	    return XSCROLL_BAR (tem);
	}
    }

  return NULL;
}

/* Unfortunately, NOACTIVATE is not implementable on Haiku.  */
static void
haiku_focus_frame (struct frame *frame, bool noactivate)
{
  if (x_display_list->focused_frame != frame)
    haiku_frame_raise_lower (frame, 1);
}

static void
haiku_new_focus_frame (struct frame *frame)
{
  eassert (x_display_list && !x_display_list->next);

  block_input ();
  if (frame != x_display_list->focused_frame)
    {
      if (x_display_list->focused_frame &&
	  x_display_list->focused_frame->auto_lower)
	haiku_frame_raise_lower (x_display_list->focused_frame, 0);

      x_display_list->focused_frame = frame;

      if (frame && frame->auto_raise && !popup_activated_p)
	haiku_frame_raise_lower (frame, 1);
    }
  unblock_input ();

  haiku_rehighlight ();
}

static void
haiku_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  haiku_set_name (f, arg, 0);
}

static void
haiku_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
{
  haiku_query_color (FRAME_BACKGROUND_PIXEL (f), bgcolor);
}

static bool
haiku_defined_color (struct frame *f,
		     const char *name,
		     Emacs_Color *color,
		     bool alloc,
		     bool make_index)
{
  return !haiku_get_color (name, color);
}

/* Adapted from xterm `x_draw_box_rect'.  */
static void
haiku_draw_box_rect (struct glyph_string *s,
		     int left_x, int top_y, int right_x, int bottom_y, int hwidth,
		     int vwidth, bool left_p, bool right_p, struct haiku_rect *clip_rect)
{
  void *view = FRAME_HAIKU_VIEW (s->f);
  struct face *face = s->face;

  BView_SetHighColor (view, face->box_color);
  if (clip_rect)
    BView_ClipToRect (view, clip_rect->x, clip_rect->y, clip_rect->width,
		      clip_rect->height);
  BView_FillRectangle (view, left_x, top_y, right_x - left_x + 1, hwidth);
  if (left_p)
    BView_FillRectangle (view, left_x, top_y, vwidth, bottom_y - top_y + 1);

  BView_FillRectangle (view, left_x, bottom_y - hwidth + 1,
		       right_x - left_x + 1, hwidth);
  if (right_p)
    BView_FillRectangle (view, right_x - vwidth + 1,
			 top_y, vwidth, bottom_y - top_y + 1);
}

static void
haiku_calculate_relief_colors (struct glyph_string *s, uint32_t *rgbout_w,
			       uint32_t *rgbout_b)
{
  struct face *face = s->face;

  prepare_face_for_display (s->f, s->face);

  uint32_t rgbin = face->use_box_color_for_shadows_p
    ?  face->box_color : face->background;

  if (s->hl == DRAW_CURSOR)
    rgbin = FRAME_CURSOR_COLOR (s->f).pixel;

  double h, cs, l;
  rgb_color_hsl (rgbin, &h, &cs, &l);

  hsl_color_rgb (h, cs, fmin (1.0, fmax (0.2, l) * 0.6), rgbout_b);
  hsl_color_rgb (h, cs, fmin (1.0, fmax (0.2, l) * 1.2), rgbout_w);
}

static void
haiku_draw_relief_rect (struct glyph_string *s,
			int left_x, int top_y, int right_x, int bottom_y,
			int hwidth, int vwidth, bool raised_p, bool top_p, bool bot_p,
			bool left_p, bool right_p,
			struct haiku_rect *clip_rect, bool fancy_p)
{
  uint32_t color_white;
  uint32_t color_black;

  haiku_calculate_relief_colors (s, &color_white, &color_black);

  void *view = FRAME_HAIKU_VIEW (s->f);
  BView_SetHighColor (view, raised_p ? color_white : color_black);
  if (clip_rect)
    {
      BView_StartClip (view);
      haiku_clip_to_string (s);
      BView_ClipToRect (view, clip_rect->x, clip_rect->y, clip_rect->width,
			clip_rect->height);
    }
  if (top_p)
    BView_FillRectangle (view, left_x, top_y, right_x - left_x + 1, hwidth);
  if (left_p)
    BView_FillRectangle (view, left_x, top_y, vwidth, bottom_y - top_y + 1);
  BView_SetHighColor (view, !raised_p ? color_white : color_black);

  if (bot_p)
    BView_FillRectangle (view, left_x, bottom_y - hwidth + 1,
			 right_x - left_x + 1, hwidth);
  if (right_p)
    BView_FillRectangle (view, right_x - vwidth + 1, top_y,
			 vwidth, bottom_y - top_y + 1);

  /* Draw the triangle for the bottom-left corner.  */
  if (bot_p && left_p)
    {
      BView_SetHighColor (view, raised_p ? color_white : color_black);
      BView_FillTriangle (view, left_x, bottom_y - hwidth, left_x + vwidth,
			  bottom_y - hwidth, left_x, bottom_y);
    }

  /* Now draw the triangle for the top-right corner.  */
  if (top_p && right_p)
    {
      BView_SetHighColor (view, raised_p ? color_white : color_black);
      BView_FillTriangle (view, right_x - vwidth, top_y,
			  right_x, top_y,
			  right_x - vwidth, top_y + hwidth);
    }

  /* If (h/v)width is > 1, we draw the outer-most line on each side in the
     black relief color.  */

  BView_SetHighColor (view, color_black);

  if (hwidth > 1 && top_p)
    BView_StrokeLine (view, left_x, top_y, right_x, top_y);
  if (hwidth > 1 && bot_p)
    BView_StrokeLine (view, left_x, bottom_y, right_x, bottom_y);
  if (vwidth > 1 && left_p)
    BView_StrokeLine (view, left_x, top_y, left_x, bottom_y);
  if (vwidth > 1 && right_p)
    BView_StrokeLine (view, right_x, top_y, right_x, bottom_y);

  BView_SetHighColor (view, s->face->background);

  /* Omit corner pixels.  */
  if (hwidth > 1 || vwidth > 1)
    {
      if (left_p && top_p)
	BView_FillRectangle (view, left_x, top_y, 1, 1);
      if (left_p && bot_p)
	BView_FillRectangle (view, left_x, bottom_y, 1, 1);
      if (right_p && top_p)
	BView_FillRectangle (view, right_x, top_y, 1, 1);
      if (right_p && bot_p)
	BView_FillRectangle (view, right_x, bottom_y, 1, 1);
    }

  if (clip_rect)
    BView_EndClip (view);
}

static void
haiku_draw_underwave (struct glyph_string *s, int width, int x)
{
  int wave_height = 3, wave_length = 2;
  int y, dx, dy, odd, xmax;
  dx = wave_length;
  dy = wave_height - 1;
  y = s->ybase - wave_height + 3;

  float ax, ay, bx, by;
  xmax = x + width;

  void *view = FRAME_HAIKU_VIEW (s->f);

  BView_StartClip (view);
  haiku_clip_to_string (s);
  BView_ClipToRect (view, x, y, width, wave_height);
  ax = x - ((int) (x) % dx) + (float) 0.5;
  bx = ax + dx;
  odd = (int) (ax / dx) % 2;
  ay = by = y + 0.5;

  if (odd)
    ay += dy;
  else
    by += dy;

  while (ax <= xmax)
    {
      BView_StrokeLine (view, ax, ay, bx, by);
      ax = bx, ay = by;
      bx += dx, by = y + 0.5 + odd * dy;
      odd = !odd;
    }
  BView_EndClip (view);
}

static void
haiku_draw_text_decoration (struct glyph_string *s, struct face *face,
			    int width, int x)
{
  unsigned long cursor_color;

  if (s->for_overlaps)
    return;

  if (s->hl == DRAW_CURSOR)
    haiku_merge_cursor_foreground (s, &cursor_color, NULL);

  void *view = FRAME_HAIKU_VIEW (s->f);

  if (face->underline)
    {
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, cursor_color);
      else if (!face->underline_defaulted_p)
	BView_SetHighColor (view, face->underline_color);
      else
	BView_SetHighColor (view, face->foreground);

      if (face->underline == FACE_UNDER_WAVE)
	haiku_draw_underwave (s, width, x);
      else if (face->underline == FACE_UNDER_LINE)
	{
	  unsigned long thickness, position;
	  int y;

	  if (s->prev
	      && s->prev->face->underline == FACE_UNDER_LINE
	      && (s->prev->face->underline_at_descent_line_p
		  == s->face->underline_at_descent_line_p)
	      && (s->prev->face->underline_pixels_above_descent_line
		  == s->face->underline_pixels_above_descent_line))
	    {
	      /* We use the same underline style as the previous one.  */
	      thickness = s->prev->underline_thickness;
	      position = s->prev->underline_position;
	    }
	  else
	    {
	      struct font *font = font_for_underline_metrics (s);
	      unsigned long minimum_offset;
	      bool underline_at_descent_line;
	      bool use_underline_position_properties;
	      Lisp_Object val = (WINDOW_BUFFER_LOCAL_VALUE
				 (Qunderline_minimum_offset, s->w));

	      if (FIXNUMP (val))
		minimum_offset = max (0, XFIXNUM (val));
	      else
		minimum_offset = 1;

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_underline_at_descent_line, s->w));
	      underline_at_descent_line
		= (!(NILP (val) || EQ (val, Qunbound))
		   || s->face->underline_at_descent_line_p);

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_use_underline_position_properties, s->w));
	      use_underline_position_properties
		= !(NILP (val) || EQ (val, Qunbound));

	      /* Get the underline thickness.  Default is 1 pixel.  */
	      if (font && font->underline_thickness > 0)
		thickness = font->underline_thickness;
	      else
		thickness = 1;
	      if (underline_at_descent_line)
		position = ((s->height - thickness)
			    - (s->ybase - s->y)
			    - s->face->underline_pixels_above_descent_line);
	      else
		{
		  /* Get the underline position.  This is the
		     recommended vertical offset in pixels from
		     the baseline to the top of the underline.
		     This is a signed value according to the
		     specs, and its default is

		     ROUND ((maximum descent) / 2), with
		     ROUND(x) = floor (x + 0.5)  */

		  if (use_underline_position_properties
		      && font && font->underline_position >= 0)
		    position = font->underline_position;
		  else if (font)
		    position = (font->descent + 1) / 2;
		  else
		    position = minimum_offset;
		}
	      position = max (position, minimum_offset);
	    }
	  /* Check the sanity of thickness and position.  We should
	     avoid drawing underline out of the current line area.  */
	  if (s->y + s->height <= s->ybase + position)
	    position = (s->height - 1) - (s->ybase - s->y);
	  if (s->y + s->height < s->ybase + position + thickness)
	    thickness = (s->y + s->height) - (s->ybase + position);
	  s->underline_thickness = thickness;
	  s->underline_position = position;
	  y = s->ybase + position;

	  BView_FillRectangle (view, s->x, y, s->width, thickness);
	}
    }

  if (face->overline_p)
    {
      unsigned long dy = 0, h = 1;
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, cursor_color);
      else if (!face->overline_color_defaulted_p)
	BView_SetHighColor (view, face->overline_color);
      else
	BView_SetHighColor (view, face->foreground);

      BView_FillRectangle (view, s->x, s->y + dy, s->width, h);
    }

  if (face->strike_through_p)
    {
      /* Y-coordinate and height of the glyph string's first
	 glyph.  We cannot use s->y and s->height because those
	 could be larger if there are taller display elements
	 (e.g., characters displayed with a larger font) in the
	 same glyph row.  */
      int glyph_y = s->ybase - s->first_glyph->ascent;
      int glyph_height = s->first_glyph->ascent + s->first_glyph->descent;
      /* Strike-through width and offset from the glyph string's
	 top edge.  */
      unsigned long h = 1;
      unsigned long dy = (glyph_height - h) / 2;

      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, cursor_color);
      else if (!face->strike_through_color_defaulted_p)
	BView_SetHighColor (view, face->strike_through_color);
      else
	BView_SetHighColor (view, face->foreground);

      BView_FillRectangle (view, s->x, glyph_y + dy, s->width, h);
    }
}

static void
haiku_draw_string_box (struct glyph_string *s)
{
  int hwidth, vwidth, left_x, right_x, top_y, bottom_y, last_x;
  bool raised_p, left_p, right_p;
  struct glyph *last_glyph;
  struct face *face = s->face;

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

  /* The glyph that may have a right box line.  For static
     compositions and images, the right-box flag is on the first glyph
     of the glyph string; for other types it's on the last glyph.  */
  if (s->cmp || s->img)
    last_glyph = s->first_glyph;
  else if (s->first_glyph->type == COMPOSITE_GLYPH
	   && s->first_glyph->u.cmp.automatic)
    {
      /* For automatic compositions, we need to look up the last glyph
	 in the composition.  */
        struct glyph *end = s->row->glyphs[s->area] + s->row->used[s->area];
	struct glyph *g = s->first_glyph;
	for (last_glyph = g++;
	     g < end && g->u.cmp.automatic && g->u.cmp.id == s->cmp_id
	       && g->slice.cmp.to < s->cmp_to;
	     last_glyph = g++)
	  ;
    }
  else
    last_glyph = s->first_glyph + s->nchars - 1;

  vwidth = eabs (face->box_vertical_line_width);
  hwidth = eabs (face->box_horizontal_line_width);
  raised_p = face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = (s->row->full_width_p && s->extends_to_end_of_line_p
	     ? last_x - 1
	     : min (last_x, s->x + s->background_width) - 1);

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

  if (face->box == FACE_SIMPLE_BOX)
    haiku_draw_box_rect (s, left_x, top_y, right_x, bottom_y, hwidth,
			 vwidth, left_p, right_p, NULL);
  else
    haiku_draw_relief_rect (s, left_x, top_y, right_x, bottom_y, hwidth,
			    vwidth, raised_p, true, true, left_p, right_p,
			    NULL, 1);
}

static void
haiku_draw_plain_background (struct glyph_string *s, struct face *face,
			     int box_line_hwidth, int box_line_vwidth)
{
  void *view = FRAME_HAIKU_VIEW (s->f);
  unsigned long cursor_color;
  if (s->hl == DRAW_CURSOR)
    {
      haiku_merge_cursor_foreground (s, NULL, &cursor_color);
      BView_SetHighColor (view, cursor_color);
    }
  else
    BView_SetHighColor (view, face->background_defaulted_p ?
			FRAME_BACKGROUND_PIXEL (s->f) :
		      face->background);

  BView_FillRectangle (view, s->x,
		       s->y + box_line_hwidth,
		       s->background_width,
		       s->height - 2 * box_line_hwidth);
}

static void
haiku_draw_stipple_background (struct glyph_string *s, struct face *face,
			       int box_line_hwidth, int box_line_vwidth)
{
}

static void
haiku_maybe_draw_background (struct glyph_string *s, int force_p)
{
  if ((s->first_glyph->type != IMAGE_GLYPH) && !s->background_filled_p)
    {
      struct face *face = s->face;
      int box_line_width = max (face->box_horizontal_line_width, 0);
      int box_vline_width = max (face->box_vertical_line_width, 0);

      if (FONT_HEIGHT (s->font) < s->height - 2 * box_vline_width
	  || FONT_TOO_HIGH (s->font)
          || s->font_not_found_p || s->extends_to_end_of_line_p || force_p)
	{
	  if (!face->stipple)
	    haiku_draw_plain_background (s, face, box_line_width,
					 box_vline_width);
	  else
	    haiku_draw_stipple_background (s, face, box_line_width,
					   box_vline_width);
	  s->background_filled_p = 1;
	}
    }
}

static void
haiku_mouse_face_colors (struct glyph_string *s, uint32_t *fg,
			 uint32_t *bg)
{
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = MOUSE_HL_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID_OR_NULL (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);

  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch, -1, Qnil);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0, -1, Qnil);

  face = FACE_FROM_ID (s->f, face_id);
  prepare_face_for_display (s->f, s->face);

  if (fg)
    *fg = face->foreground;
  if (bg)
    *bg = face->background;
}

static void
haiku_draw_glyph_string_foreground (struct glyph_string *s)
{
  struct face *face = s->face;

  int i, x;
  if (face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (face->box_vertical_line_width, 0);
  else
    x = s->x;

  void *view = FRAME_HAIKU_VIEW (s->f);

  if (s->font_not_found_p)
    {
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, FRAME_OUTPUT_DATA (s->f)->cursor_fg);
      else
	BView_SetHighColor (view, face->foreground);
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  BView_StrokeRectangle (view, x, s->y, g->pixel_width,
				 s->height);
	  x += g->pixel_width;
	}
    }
  else
    {
      struct font *ft = s->font;
      int off = ft->baseline_offset;
      int y;

      if (ft->vertical_centering)
	off = VCENTER_BASELINE_OFFSET (ft, s->f) - off;
      y = s->ybase - off;
      if (s->for_overlaps || (s->background_filled_p && s->hl != DRAW_CURSOR))
	ft->driver->draw (s, 0, s->nchars, x, y, false);
      else
	ft->driver->draw (s, 0, s->nchars, x, y, true);

      if (face->overstrike)
	ft->driver->draw (s, 0, s->nchars, x + 1, y, false);
    }
}

static void
haiku_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  unsigned char2b[8];
  int x, i, j;
  struct face *face = s->face;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (face && face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (face->box_vertical_line_width, 0);
  else
    x = s->x;

  s->char2b = char2b;

  for (i = 0; i < s->nchars; i++, glyph++)
    {
#ifdef GCC_LINT
      enum { PACIFY_GCC_BUG_81401 = 1 };
#else
      enum { PACIFY_GCC_BUG_81401 = 0 };
#endif
      char buf[7 + PACIFY_GCC_BUG_81401];
      char *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 0
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  unsigned int ch = glyph->u.glyphless.ch;
	  eassume (ch <= MAX_CHAR);
	  sprintf (buf, "%0*X", ch < 0x10000 ? 4 : 6, ch);
	  str = buf;
	}

      if (str)
	{
	  int upper_len = (len + 1) / 2;

	  /* It is assured that all LEN characters in STR is ASCII.  */
	  for (j = 0; j < len; j++)
            char2b[j] = s->font->driver->encode_char (s->font, str[j]) & 0xFFFF;

	  s->font->driver->draw (s, 0, upper_len,
				 x + glyph->slice.glyphless.upper_xoff,
				 s->ybase + glyph->slice.glyphless.upper_yoff,
				 false);
	  s->font->driver->draw (s, upper_len, len,
				 x + glyph->slice.glyphless.lower_xoff,
				 s->ybase + glyph->slice.glyphless.lower_yoff,
				 false);
	}
      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
	BView_FillRectangle (FRAME_HAIKU_VIEW (s->f),
			     x, s->ybase - glyph->ascent,
			     glyph->pixel_width - 1,
			     glyph->ascent + glyph->descent - 1);
      x += glyph->pixel_width;
   }
}

static void
haiku_draw_stretch_glyph_string (struct glyph_string *s)
{
  eassert (s->first_glyph->type == STRETCH_GLYPH);

  struct face *face = s->face;

  if (s->hl == DRAW_CURSOR && !x_stretch_cursor_p)
    {
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right (s->w, TEXT_AREA);
	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}

      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      void *view = FRAME_HAIKU_VIEW (s->f);
      unsigned long cursor_color;

      haiku_merge_cursor_foreground (s, NULL, &cursor_color);
      BView_SetHighColor (view, cursor_color);
      BView_FillRectangle (view, x, s->y, width, s->height);

      if (width < background_width)
	{
	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;

	  int y = s->y;
	  int w = background_width - width, h = s->height;

	  if (!face->stipple)
	    {
	      uint32_t bkg;
	      if (s->row->mouse_face_p && cursor_in_mouse_face_p (s->w))
		haiku_mouse_face_colors (s, NULL, &bkg);
	      else
		bkg = face->background;

	      BView_SetHighColor (view, bkg);
	      BView_FillRectangle (view, x, y, w, h);
	    }
	}
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, text_left_x = window_box_left (s->w, TEXT_AREA);

      /* Don't draw into left fringe or scrollbar area except for
         header line and mode line.  */
      if (s->area == TEXT_AREA
	  && x < text_left_x && !s->row->mode_line_p)
	{
	  background_width -= text_left_x - x;
	  x = text_left_x;
	}

      if (background_width > 0)
	{
	  void *view = FRAME_HAIKU_VIEW (s->f);
	  unsigned long bkg;
	  if (s->hl == DRAW_CURSOR)
	    haiku_merge_cursor_foreground (s, NULL, &bkg);
	  else
	    bkg = s->face->background;

	  BView_SetHighColor (view, bkg);
	  BView_FillRectangle (view, x, s->y, background_width, s->height);
	}
    }
  s->background_filled_p = 1;
}

static void
haiku_start_clip (struct glyph_string *s)
{
  void *view = FRAME_HAIKU_VIEW (s->f);
  BView_StartClip (view);
}

static void
haiku_end_clip (struct glyph_string *s)
{
  void *view = FRAME_HAIKU_VIEW (s->f);
  BView_EndClip (view);
}

static void
haiku_clip_to_row (struct window *w, struct glyph_row *row,
		   enum glyph_row_area area)
{
  struct frame *f = WINDOW_XFRAME (w);
  int window_x, window_y, window_width;
  int x, y, width, height;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  x = window_x;
  y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  y = max (y, window_y);
  width = window_width;
  height = row->visible_height;

  BView_ClipToRect (FRAME_HAIKU_VIEW (f), x, y, width, height);
}

static void
haiku_update_begin (struct frame *f)
{
}

static void
haiku_update_end (struct frame *f)
{
  MOUSE_HL_INFO (f)->mouse_face_defer = false;
  BWindow_Flush (FRAME_HAIKU_WINDOW (f));
}

static void
haiku_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;
  void *view = FRAME_HAIKU_VIEW (s->f);
  struct face *face = s->face;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (face && face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */

  if (s->font_not_found_p && !s->cmp_from)
    {
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, FRAME_OUTPUT_DATA (s->f)->cursor_fg);
      else
	BView_SetHighColor (view, s->face->foreground);
      BView_StrokeRectangle (view, s->x, s->y, s->width - 1, s->height - 1);
    }
  else if (!s->first_glyph->u.cmp.automatic)
    {
      int y = s->ybase;

      for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	/* TAB in a composition means display glyphs with padding
	   space on the left or right.  */
	if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	  {
	    int xx = x + s->cmp->offsets[j * 2];
	    int yy = y - s->cmp->offsets[j * 2 + 1];

	    font->driver->draw (s, j, j + 1, xx, yy, false);
	    if (face->overstrike)
	      font->driver->draw (s, j, j + 1, xx + 1, yy, false);
	  }
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
      Lisp_Object glyph;
      int y = s->ybase;
      int width = 0;

      for (i = j = s->cmp_from; i < s->cmp_to; i++)
	{
	  glyph = LGSTRING_GLYPH (gstring, i);
	  if (NILP (LGLYPH_ADJUSTMENT (glyph)))
	    width += LGLYPH_WIDTH (glyph);
	  else
	    {
	      int xoff, yoff, wadjust;

	      if (j < i)
		{
		  font->driver->draw (s, j, i, x, y, false);
		  if (s->face->overstrike)
		    font->driver->draw (s, j, i, x + 1, y, false);
		  x += width;
		}
	      xoff = LGLYPH_XOFF (glyph);
	      yoff = LGLYPH_YOFF (glyph);
	      wadjust = LGLYPH_WADJUST (glyph);
	      font->driver->draw (s, i, i + 1, x + xoff, y + yoff, false);
	      if (face->overstrike)
		font->driver->draw (s, i, i + 1, x + xoff + 1, y + yoff,
				    false);
	      x += wadjust;
	      j = i + 1;
	      width = 0;
	    }
	}
      if (j < i)
	{
	  font->driver->draw (s, j, i, x, y, false);
	  if (face->overstrike)
	    font->driver->draw (s, j, i, x + 1, y, false);
	}
    }
}

static void
haiku_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
  struct haiku_rect r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  struct face *face = s->face;

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      if (s->face->id == TAB_BAR_FACE_ID)
	thick = (tab_bar_button_relief < 0
		 ? DEFAULT_TAB_BAR_BUTTON_RELIEF
		 : min (tab_bar_button_relief, 1000000));
      else
	thick = (tool_bar_button_relief < 0
		 ? DEFAULT_TOOL_BAR_BUTTON_RELIEF
		 : min (tool_bar_button_relief, 1000000));
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x1 = x + s->slice.width - 1;
  y1 = y + s->slice.height - 1;

  extra_x = extra_y = 0;

  if (s->face->id == TAB_BAR_FACE_ID)
    {
      if (CONSP (Vtab_bar_button_margin)
	  && FIXNUMP (XCAR (Vtab_bar_button_margin))
	  && FIXNUMP (XCDR (Vtab_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtab_bar_button_margin)) - thick;
	  extra_y = XFIXNUM (XCDR (Vtab_bar_button_margin)) - thick;
	}
      else if (FIXNUMP (Vtab_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtab_bar_button_margin) - thick;
    }

  if (s->face->id == TOOL_BAR_FACE_ID)
    {
      if (CONSP (Vtool_bar_button_margin)
	  && FIXNUMP (XCAR (Vtool_bar_button_margin))
	  && FIXNUMP (XCDR (Vtool_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtool_bar_button_margin));
	  extra_y = XFIXNUM (XCDR (Vtool_bar_button_margin));
	}
      else if (FIXNUMP (Vtool_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtool_bar_button_margin);
    }

  top_p = bot_p = left_p = right_p = 0;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = 1;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = 1;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = 1;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = 1;

  get_glyph_string_clip_rect (s, &r);
  haiku_draw_relief_rect (s, x, y, x1, y1, thick, thick, raised_p,
			  top_p, bot_p, left_p, right_p, &r, 0);
}

static void
haiku_draw_image_glyph_string (struct glyph_string *s)
{
  struct face *face = s->face;

  int box_line_hwidth = max (face->box_vertical_line_width, 0);
  int box_line_vwidth = max (face->box_horizontal_line_width, 0);

  int x, y;
  int height, width;

  height = s->height;
  if (s->slice.y == 0)
    height -= box_line_vwidth;
  if (s->slice.y + s->slice.height >= s->img->height)
    height -= box_line_vwidth;

  width = s->background_width;
  x = s->x;
  if (s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    {
      x += box_line_hwidth;
      width -= box_line_hwidth;
    }

  y = s->y;
  if (s->slice.y == 0)
    y += box_line_vwidth;

  void *view = FRAME_HAIKU_VIEW (s->f);
  void *bitmap = s->img->pixmap;

  s->stippled_p = face->stipple != 0;

  BView_SetHighColor (view, face->background);
  BView_FillRectangle (view, x, y, width, height);

  if (bitmap)
    {
      struct haiku_rect nr;
      Emacs_Rectangle cr, ir, r;

      get_glyph_string_clip_rect (s, &nr);
      CONVERT_TO_EMACS_RECT (cr, nr);
      x = s->x;
      y = s->ybase - image_ascent (s->img, face, &s->slice);

      if (s->slice.x == 0)
	x += s->img->hmargin;
      if (s->slice.y == 0)
	y += s->img->vmargin;

      if (face->box != FACE_NO_BOX
	  && s->first_glyph->left_box_line_p
	  && s->slice.x == 0)
	x += max (face->box_vertical_line_width, 0);

      ir.x = x;
      ir.y = y;
      ir.width = s->slice.width;
      ir.height = s->slice.height;
      r = ir;

      void *mask = s->img->mask;

      if (gui_intersect_rectangles (&cr, &ir, &r))
	{
	  if (s->img->have_be_transforms_p)
	    {
	      bitmap = BBitmap_transform_bitmap (bitmap,
						 s->img->mask,
						 face->background,
						 s->img->be_rotate,
						 s->img->width,
						 s->img->height);
	      mask = NULL;
	    }

	  BView_DrawBitmap (view, bitmap,
			    s->slice.x + r.x - x,
			    s->slice.y + r.y - y,
			    r.width, r.height,
			    r.x, r.y, r.width, r.height);
	  if (mask)
	    {
	      BView_DrawMask (mask, view,
			      s->slice.x + r.x - x,
			      s->slice.y + r.y - y,
			      r.width, r.height,
			      r.x, r.y, r.width, r.height,
			      face->background);
	    }

	  if (s->img->have_be_transforms_p)
	    BBitmap_free (bitmap);
	}

      if (s->hl == DRAW_CURSOR)
	{
	  BView_SetPenSize (view, 1);
	  BView_SetHighColor (view, FRAME_CURSOR_COLOR (s->f).pixel);
	  BView_StrokeRectangle (view, r.x, r.y, r.width, r.height);
	}
    }

  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    haiku_draw_image_relief (s);
}

static void
haiku_draw_glyph_string (struct glyph_string *s)
{
  void *view;

  block_input ();
  view = FRAME_HAIKU_VIEW (s->f);
  BView_draw_lock (view, false, 0, 0, 0, 0);
  prepare_face_for_display (s->f, s->face);

  struct face *face = s->face;
  if (face != s->face)
    prepare_face_for_display (s->f, face);

  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
          {
	    prepare_face_for_display (s->f, s->next->face);
	    haiku_start_clip (s->next);
	    haiku_clip_to_string (s->next);
            if (next->first_glyph->type != STRETCH_GLYPH)
	      haiku_maybe_draw_background (s->next, 1);
            else
	      haiku_draw_stretch_glyph_string (s->next);
	    haiku_end_clip (s);
          }
    }

  haiku_start_clip (s);

  int box_filled_p = 0;

  if (!s->for_overlaps && face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      haiku_clip_to_string (s);
      haiku_maybe_draw_background (s, 1);
      box_filled_p = 1;
      haiku_draw_string_box (s);
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
	   && !s->clip_tail
	   && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	       || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    haiku_clip_to_string_exactly (s, s);
  else
    haiku_clip_to_string (s);

  if (s->for_overlaps)
    s->background_filled_p = 1;

  switch (s->first_glyph->type)
    {
    case COMPOSITE_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = 1;
      else
	haiku_maybe_draw_background (s, 1);
      haiku_draw_composite_glyph_string_foreground (s);
      break;
    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = 1;
      else
	haiku_maybe_draw_background (s, 0);
      haiku_draw_glyph_string_foreground (s);
      break;
    case STRETCH_GLYPH:
      haiku_draw_stretch_glyph_string (s);
      break;
    case IMAGE_GLYPH:
      haiku_draw_image_glyph_string (s);
      break;
    case GLYPHLESS_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = 1;
      else
	haiku_maybe_draw_background (s, 1);
      haiku_draw_glyphless_glyph_string_foreground (s);
      break;
    default:
      emacs_abort ();
    }

  if (!s->for_overlaps)
    {
      if (!box_filled_p && face->box != FACE_NO_BOX)
	haiku_draw_string_box (s);
      else
	haiku_draw_text_decoration (s, face, s->width, s->x);

      if (s->prev)
	{
	  struct glyph_string *prev;

	  for (prev = s->prev; prev; prev = prev->prev)
	    if (prev->hl != s->hl
		&& prev->x + prev->width + prev->right_overhang > s->x)
	      {
		/* As prev was drawn while clipped to its own area, we
		   must draw the right_overhang part using s->hl now.  */
		enum draw_glyphs_face save = prev->hl;

		prev->hl = s->hl;
		haiku_start_clip (s);
		haiku_clip_to_string (s);
		haiku_clip_to_string_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  haiku_draw_glyph_string_foreground (prev);
		else
		  haiku_draw_composite_glyph_string_foreground (prev);
		haiku_end_clip (s);
		prev->hl = save;
	      }
	}

      if (s->next)
	{
	  struct glyph_string *next;

	  for (next = s->next; next; next = next->next)
	    if (next->hl != s->hl
		&& next->x - next->left_overhang < s->x + s->width)
	      {
		/* As next will be drawn while clipped to its own area,
		   we must draw the left_overhang part using s->hl now.  */
		enum draw_glyphs_face save = next->hl;

		next->hl = s->hl;
		haiku_start_clip (s);
		haiku_clip_to_string (s);
		haiku_clip_to_string_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  haiku_draw_glyph_string_foreground (next);
		else
		  haiku_draw_composite_glyph_string_foreground (next);
		haiku_end_clip (s);

		next->hl = save;
		next->clip_head = s->next;
	      }
	}
    }
  haiku_end_clip (s);
  BView_draw_unlock (view);
  unblock_input ();
}

static void
haiku_after_update_window_line (struct window *w,
				struct glyph_row *desired_row)
{
  eassert (w);
  struct frame *f;
  int width, height;

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = true;

  if (windows_or_buffers_changed
      && desired_row->full_width_p
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));
      int face_id =
        !NILP (Vface_remapping_alist)
        ? lookup_basic_face (NULL, f, INTERNAL_BORDER_FACE_ID)
        : INTERNAL_BORDER_FACE_ID;
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

      block_input ();
      if (face)
	{
	  void *view = FRAME_HAIKU_VIEW (f);
	  BView_draw_lock (view, false, 0, 0, 0, 0);
	  BView_StartClip (view);
	  BView_SetHighColor (view, face->background_defaulted_p ?
			      FRAME_BACKGROUND_PIXEL (f) : face->background);
	  BView_FillRectangle (view, 0, y, width, height);
	  BView_FillRectangle (view, FRAME_PIXEL_WIDTH (f) - width,
			       y, width, height);
	  BView_invalidate_region (FRAME_HAIKU_VIEW (f),
				   0, y, width, height);
	  BView_invalidate_region (view, FRAME_PIXEL_WIDTH (f) - width,
				   y, width, height);
	  BView_EndClip (view);
	  BView_draw_unlock (view);
	}
      else
	{
	  haiku_clear_frame_area (f, 0, y, width, height);
	  haiku_clear_frame_area (f, FRAME_PIXEL_WIDTH (f) - width,
				  y, width, height);
	}
      unblock_input ();
    }
}

static void
haiku_set_window_size (struct frame *f, bool change_gravity,
		       int width, int height)
{
  Lisp_Object frame;

  /* On X Windows, window managers typically disallow resizing a
     window when it is fullscreen.  Do the same here.  */

  XSETFRAME (frame, f);
  if (!NILP (Fframe_parameter (frame, Qfullscreen))
      /* Only do this if the fullscreen status has actually been
	 applied.  */
      && f->want_fullscreen == FULLSCREEN_NONE
      /* And if the configury during frame completion has been
	 completed.  Otherwise, there will be no valid "old size" to
	 go back to.  */
      && FRAME_OUTPUT_DATA (f)->configury_done)
    return;

  haiku_update_size_hints (f);

  if (FRAME_HAIKU_WINDOW (f))
    {
      block_input ();
      BWindow_resize (FRAME_HAIKU_WINDOW (f), width, height);
      unblock_input ();
    }
}

static void
haiku_draw_window_cursor (struct window *w,
			  struct glyph_row *glyph_row,
			  int x, int y,
			  enum text_cursor_kinds cursor_type,
			  int cursor_width, bool on_p, bool active_p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;
  struct glyph *phys_cursor_glyph;
  struct glyph *cursor_glyph;

  void *view = FRAME_HAIKU_VIEW (f);

  int fx, fy, h, cursor_height;

  if (!on_p)
    return;

  if (cursor_type == NO_CURSOR)
    {
      w->phys_cursor_width = 0;
      return;
    }

  w->phys_cursor_on_p = true;
  w->phys_cursor_type = cursor_type;

  phys_cursor_glyph = get_phys_cursor_glyph (w);

  if (!phys_cursor_glyph)
    {
      if (glyph_row->exact_window_width_line_p
          && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
        {
          glyph_row->cursor_in_fringe_p = 1;
          draw_fringe_bitmap (w, glyph_row, 0);
        }
      return;
    }

  get_phys_cursor_geometry (w, glyph_row, phys_cursor_glyph, &fx, &fy, &h);

  if (cursor_type == BAR_CURSOR)
    {
      if (cursor_width < 1)
	cursor_width = max (FRAME_CURSOR_WIDTH (f), 1);
      if (cursor_width < w->phys_cursor_width)
        w->phys_cursor_width = cursor_width;
    }
  else if (cursor_type == HBAR_CURSOR)
    {
      cursor_height = (cursor_width < 1) ? lrint (0.25 * h) : cursor_width;
      if (cursor_height > glyph_row->height)
        cursor_height = glyph_row->height;
      if (h > cursor_height)
        fy += h - cursor_height;
      h = cursor_height;
    }

  BView_draw_lock (view, false, 0, 0, 0, 0);
  BView_StartClip (view);

  if (cursor_type == BAR_CURSOR)
    {
      cursor_glyph = get_phys_cursor_glyph (w);
      face = FACE_FROM_ID (f, cursor_glyph->face_id);
    }

  /* If the glyph's background equals the color we normally draw the
     bar cursor in, our cursor in its normal color is invisible.  Use
     the glyph's foreground color instead in this case, on the
     assumption that the glyph's colors are chosen so that the glyph
     is legible.  */

  /* xterm.c only does this for bar cursors, and nobody has
     complained, so it would be best to do that here as well.  */
  if (cursor_type == BAR_CURSOR
      && face->background == FRAME_CURSOR_COLOR (f).pixel)
    BView_SetHighColor (view, face->foreground);
  else
    BView_SetHighColor (view, FRAME_CURSOR_COLOR (f).pixel);
  haiku_clip_to_row (w, glyph_row, TEXT_AREA);

  switch (cursor_type)
    {
    default:
    case DEFAULT_CURSOR:
    case NO_CURSOR:
      break;
    case HBAR_CURSOR:
      BView_FillRectangle (view, fx, fy, w->phys_cursor_width, h);
      BView_invalidate_region (view, fx, fy, w->phys_cursor_width, h);
      break;
    case BAR_CURSOR:
      if (cursor_glyph->resolved_level & 1)
	{
	  BView_FillRectangle (view, fx + cursor_glyph->pixel_width - w->phys_cursor_width,
			       fy, w->phys_cursor_width, h);
	  BView_invalidate_region (view, fx + cursor_glyph->pixel_width - w->phys_cursor_width,
				   fy, w->phys_cursor_width, h);
	}
      else
	BView_FillRectangle (view, fx, fy, w->phys_cursor_width, h);

      BView_invalidate_region (view, fx, fy, w->phys_cursor_width, h);
      break;
    case HOLLOW_BOX_CURSOR:
      if (phys_cursor_glyph->type != IMAGE_GLYPH)
	{
	  BView_SetPenSize (view, 1);
	  BView_StrokeRectangle (view, fx, fy, w->phys_cursor_width, h);
	}
      else
	draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);

      BView_invalidate_region (view, fx, fy, w->phys_cursor_width, h);
      break;
    case FILLED_BOX_CURSOR:
      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
    }
  BView_EndClip (view);
  BView_draw_unlock (view);
}

static void
haiku_show_hourglass (struct frame *f)
{
  if (FRAME_OUTPUT_DATA (f)->hourglass_p)
    return;

  block_input ();
  FRAME_OUTPUT_DATA (f)->hourglass_p = 1;

  if (FRAME_HAIKU_VIEW (f))
    BView_set_view_cursor (FRAME_HAIKU_VIEW (f),
			   FRAME_OUTPUT_DATA (f)->hourglass_cursor);
  unblock_input ();
}

static void
haiku_hide_hourglass (struct frame *f)
{
  if (!FRAME_OUTPUT_DATA (f)->hourglass_p)
    return;

  block_input ();
  FRAME_OUTPUT_DATA (f)->hourglass_p = 0;

  if (FRAME_HAIKU_VIEW (f))
    BView_set_view_cursor (FRAME_HAIKU_VIEW (f),
			   FRAME_OUTPUT_DATA (f)->current_cursor);
  unblock_input ();
}

static void
haiku_compute_glyph_string_overhangs (struct glyph_string *s)
{
  if (s->cmp == NULL
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      struct font_metrics metrics;

      if (s->first_glyph->type == CHAR_GLYPH)
	{
	  struct font *font = s->font;
	  font->driver->text_extents (font, s->char2b, s->nchars, &metrics);
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);

	  composition_gstring_width (gstring, s->cmp_from, s->cmp_to, &metrics);
	}
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? - metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}

static void
haiku_draw_vertical_window_border (struct window *w,
				   int x, int y_0, int y_1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);
  void *view = FRAME_HAIKU_VIEW (f);
  BView_draw_lock (view, true, x, y_0, 1, y_1);
  BView_StartClip (view);
  if (face)
    BView_SetHighColor (view, face->foreground);
  BView_StrokeLine (view, x, y_0, x, y_1);
  BView_EndClip (view);
  BView_draw_unlock (view);
}

static void
haiku_set_scroll_bar_default_width (struct frame *f)
{
  int unit = FRAME_COLUMN_WIDTH (f);
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = BScrollBar_default_size (0) + 1;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) =
    (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
}

static void
haiku_set_scroll_bar_default_height (struct frame *f)
{
  int height = FRAME_LINE_HEIGHT (f);
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = BScrollBar_default_size (1) + 1;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) =
    (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) + height - 1) / height;
}

static void
haiku_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FACE_ID);
  struct face *face_first
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
  struct face *face_last
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);
  unsigned long color = face ? face->foreground : FRAME_FOREGROUND_PIXEL (f);
  unsigned long color_first = (face_first
			       ? face_first->foreground
			       : FRAME_FOREGROUND_PIXEL (f));
  unsigned long color_last = (face_last
			      ? face_last->foreground
			      : FRAME_FOREGROUND_PIXEL (f));
  void *view = FRAME_HAIKU_VIEW (f);

  BView_draw_lock (view, true, x0, y0, x1 - x0 + 1, y1 - y0 + 1);
  BView_StartClip (view);

  if ((y1 - y0 > x1 - x0) && (x1 - x0 >= 3))
    /* A vertical divider, at least three pixels wide: Draw first and
       last pixels differently.  */
    {
      BView_SetHighColor (view, color_first);
      BView_StrokeLine (view, x0, y0, x0, y1 - 1);
      BView_SetHighColor (view, color);
      BView_FillRectangle (view, x0 + 1, y0, x1 - x0 - 2, y1 - y0);
      BView_SetHighColor (view, color_last);
      BView_StrokeLine (view, x1 - 1, y0, x1 - 1, y1 - 1);
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first and
       last pixels differently.  */
    {
      BView_SetHighColor (view, color_first);
      BView_StrokeLine (view, x0, y0, x1 - 1, y0);
      BView_SetHighColor (view, color);
      BView_FillRectangle (view, x0, y0 + 1, x1 - x0, y1 - y0 - 2);
      BView_SetHighColor (view, color_last);
      BView_FillRectangle (view, x0, y1 - 1, x1 - x0, 1);
    }
  else
    {
      BView_SetHighColor (view, color);
      BView_FillRectangleAbs (view, x0, y0, x1, y1);
    }
  BView_EndClip (view);
  BView_draw_unlock (view);
}

static void
haiku_condemn_scroll_bars (struct frame *frame)
{
  if (!NILP (FRAME_SCROLL_BARS (frame)))
    {
      if (!NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	{
	  /* Prepend scrollbars to already condemned ones.  */
	  Lisp_Object last = FRAME_SCROLL_BARS (frame);

	  while (!NILP (XSCROLL_BAR (last)->next))
	    last = XSCROLL_BAR (last)->next;

	  XSCROLL_BAR (last)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
	  XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = last;
	}

      fset_condemned_scroll_bars (frame, FRAME_SCROLL_BARS (frame));
      fset_scroll_bars (frame, Qnil);
    }
}

static void
haiku_redeem_scroll_bar (struct window *w)
{
  struct scroll_bar *bar;
  Lisp_Object barobj;
  struct frame *f;

  if (!NILP (w->vertical_scroll_bar) && WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->vertical_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    goto horizontal;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->vertical_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }
 horizontal:
  if (!NILP (w->horizontal_scroll_bar) && WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->horizontal_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->horizontal_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    return;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->horizontal_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }
}

static void
haiku_judge_scroll_bars (struct frame *f)
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  fset_condemned_scroll_bars (f, Qnil);

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      haiku_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}

static struct scroll_bar *
haiku_scroll_bar_create (struct window *w, int left, int top,
			 int width, int height, bool horizontal_p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Lisp_Object barobj;

  void *sb = NULL;
  void *vw = FRAME_HAIKU_VIEW (f);

  block_input ();
  struct scroll_bar *bar
    = ALLOCATE_PSEUDOVECTOR (struct scroll_bar, prev, PVEC_OTHER);

  XSETWINDOW (bar->window, w);
  bar->top = top;
  bar->left = left;
  bar->width = width;
  bar->height = height;
  bar->position = 0;
  bar->total = 0;
  bar->dragging = 0;
  bar->update = -1;
  bar->horizontal = horizontal_p;

  sb = BScrollBar_make_for_view (vw, horizontal_p,
				 left, top, left + width - 1,
				 top + height - 1, bar);

  BView_publish_scroll_bar (vw, left, top, width, height);

  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  bar->scroll_bar = sb;
  XSETVECTOR (barobj, bar);
  fset_scroll_bars (f, barobj);

  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  unblock_input ();
  return bar;
}

static void
haiku_set_horizontal_scroll_bar (struct window *w, int portion, int whole, int position)
{
  eassert (WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w));
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_x, window_width;

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, &window_x, 0, &window_width, 0);
  left = window_x;
  width = window_width;
  top = WINDOW_SCROLL_BAR_AREA_Y (w);
  height = WINDOW_CONFIG_SCROLL_BAR_HEIGHT (w);

  block_input ();

  if (NILP (w->horizontal_scroll_bar))
    {
      bar = haiku_scroll_bar_create (w, left, top, width, height, true);
      bar->update = position;
      bar->position = position;
      bar->total = whole;
    }
  else
    {
      bar = XSCROLL_BAR (w->horizontal_scroll_bar);

      if (bar->left != left || bar->top != top ||
	  bar->width != width || bar->height != height)
	{
	  void *view = FRAME_HAIKU_VIEW (WINDOW_XFRAME (w));
	  BView_forget_scroll_bar (view, bar->left, bar->top,
				   bar->width, bar->height);
	  BView_move_frame (bar->scroll_bar, left, top,
			    left + width - 1, top + height - 1);
	  BView_publish_scroll_bar (view, left, top, width, height);
	  bar->left = left;
	  bar->top = top;
	  bar->width = width;
	  bar->height = height;
	}
    }

  haiku_set_horizontal_scroll_bar_thumb (bar, portion, position, whole);
  bar->position = position;
  bar->total = whole;
  XSETVECTOR (barobj, bar);
  wset_horizontal_scroll_bar (w, barobj);
  unblock_input ();
}

static void
haiku_set_vertical_scroll_bar (struct window *w,
			       int portion, int whole, int position)
{
  eassert (WINDOW_HAS_VERTICAL_SCROLL_BAR (w));
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_y, window_height;

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;

  /* Compute the left edge and the width of the scroll bar area.  */
  left = WINDOW_SCROLL_BAR_AREA_X (w);
  width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);
  block_input ();

  if (NILP (w->vertical_scroll_bar))
    {
      bar = haiku_scroll_bar_create (w, left, top, width, height, false);
      bar->position = position;
      bar->total = whole;
    }
  else
    {
      bar = XSCROLL_BAR (w->vertical_scroll_bar);

      if (bar->left != left || bar->top != top ||
	  bar->width != width || bar->height != height)
	{
	  void *view = FRAME_HAIKU_VIEW (WINDOW_XFRAME (w));
	  BView_forget_scroll_bar (view, bar->left, bar->top,
				   bar->width, bar->height);
	  BView_move_frame (bar->scroll_bar, left, top,
			    left + width - 1, top + height - 1);
	  BView_publish_scroll_bar (view, left, top, width, height);
	  bar->left = left;
	  bar->top = top;
	  bar->width = width;
	  bar->height = height;
	}
    }

  haiku_set_scroll_bar_thumb (bar, portion, position, whole);
  bar->position = position;
  bar->total = whole;

  XSETVECTOR (barobj, bar);
  wset_vertical_scroll_bar (w, barobj);
  unblock_input ();
}

static void
haiku_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
			  struct draw_fringe_bitmap_params *p)
{
  void *view = FRAME_HAIKU_VIEW (XFRAME (WINDOW_FRAME (w)));
  struct face *face = p->face;

  block_input ();
  BView_draw_lock (view, true, p->x, p->y, p->wd, p->h);
  BView_StartClip (view);

  haiku_clip_to_row (w, row, ANY_AREA);
  if (p->bx >= 0 && !p->overlay_p)
    {
      BView_SetHighColor (view, face->background);
      BView_FillRectangle (view, p->bx, p->by, p->nx, p->ny);
    }

  if (p->which
      && p->which < max_fringe_bmp
      && p->which < max_used_fringe_bitmap)
    {
      void *bitmap = fringe_bmps[p->which];

      if (!bitmap)
	{
	  /* This fringe bitmap is known to fringe.c, but lacks the
	     BBitmap which shadows that bitmap.  This is typical to
	     define-fringe-bitmap being called when the selected frame
	     was not a GUI frame, for example, when packages that
	     define fringe bitmaps are loaded by a daemon Emacs.
	     Create the missing pattern now.  */
	  gui_define_fringe_bitmap (WINDOW_XFRAME (w), p->which);
	  bitmap = fringe_bmps[p->which];
	}

      uint32_t col;

      if (!p->cursor_p)
	col = face->foreground;
      else if (p->overlay_p)
	col = face->background;
      else
	col = FRAME_CURSOR_COLOR (XFRAME (WINDOW_FRAME (w))).pixel;

      if (!p->overlay_p)
        {
	  BView_SetHighColor (view, face->background);
	  BView_FillRectangle (view, p->x, p->y, p->wd, p->h);
	}

      BView_SetLowColor (view, col);
      BView_DrawBitmapWithEraseOp (view, bitmap, p->x, p->y, p->wd, p->h);
    }
  BView_EndClip (view);
  BView_draw_unlock (view);
  unblock_input ();
}

static void
haiku_define_fringe_bitmap (int which, unsigned short *bits,
			    int h, int wd)
{
  if (which >= max_fringe_bmp)
    {
      int i = max_fringe_bmp;
      max_fringe_bmp = which + 20;
      fringe_bmps = !i ? xmalloc (max_fringe_bmp * sizeof (void *)) :
	xrealloc (fringe_bmps, max_fringe_bmp * sizeof (void *));

      while (i < max_fringe_bmp)
	fringe_bmps[i++] = NULL;
    }

  block_input ();
  fringe_bmps[which] = BBitmap_new (wd, h, 1);
  if (!fringe_bmps[which])
    memory_full (SIZE_MAX);
  BBitmap_import_fringe_bitmap (fringe_bmps[which], bits, wd, h);
  unblock_input ();
}

static void
haiku_destroy_fringe_bitmap (int which)
{
  if (which >= max_fringe_bmp)
    return;

  if (fringe_bmps[which])
    BBitmap_free (fringe_bmps[which]);
  fringe_bmps[which] = NULL;
}

static void
haiku_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  void *view = FRAME_HAIKU_VIEW (f);
  int x, y, width, height, from_y, to_y, bottom_y;
  window_box (w, ANY_AREA, &x, &y, &width, &height);

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
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  block_input ();
  gui_clear_cursor (w);

  BView_draw_lock (view, true, x, to_y, width, height);
  BView_StartClip (view);
  BView_CopyBits (view, x, from_y, width, height,
		  x, to_y, width, height);
  BView_EndClip (view);
  BView_draw_unlock (view);

  unblock_input ();
}

/* Haiku doesn't provide any way to get the frame actually underneath
   the pointer, so we typically return dpyinfo->last_mouse_frame if
   the display is grabbed and `track-mouse' is not `dropping' or
   `drag-source'; failing that, we return the selected frame, and
   finally a random window system frame (as long as `track-mouse' is
   not `drag-source') if that didn't work either.  */
static void
haiku_mouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
		      enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
		      Time *timestamp)
{
  Lisp_Object frame, tail;
  struct frame *f1;
  int screen_x, screen_y;
  void *view;

  if (!fp)
    return;

  f1 = NULL;
  block_input ();

  FOR_EACH_FRAME (tail, frame)
    {
      if (FRAME_HAIKU_P (XFRAME (frame)))
	XFRAME (frame)->mouse_moved = false;
    }

  if (gui_mouse_grabbed (x_display_list)
      && !EQ (track_mouse, Qdropping)
      && !EQ (track_mouse, Qdrag_source))
    f1 = x_display_list->last_mouse_frame;
  else
    f1 = x_display_list->last_mouse_motion_frame;

  if (!f1 && FRAME_HAIKU_P (SELECTED_FRAME ()))
    f1 = SELECTED_FRAME ();

  if (!f1 || (!FRAME_HAIKU_P (f1) && (insist > 0)))
    FOR_EACH_FRAME (tail, frame)
      if (FRAME_HAIKU_P (XFRAME (frame)) &&
	  !FRAME_TOOLTIP_P (XFRAME (frame)))
	f1 = XFRAME (frame);

  if (f1 && FRAME_TOOLTIP_P (f1))
    f1 = NULL;

  if (f1 && FRAME_HAIKU_P (f1))
    {
      view = FRAME_HAIKU_VIEW (f1);

      if (view)
	{
	  BView_get_mouse (view, &screen_x, &screen_y);
	  remember_mouse_glyph (f1, screen_x, screen_y,
				&x_display_list->last_mouse_glyph);
	  x_display_list->last_mouse_glyph_frame = f1;

	  *bar_window = Qnil;
	  *part = scroll_bar_above_handle;

	  /* If track-mouse is `drag-source' and the mouse pointer is
	     certain to not be actually under the chosen frame, return
	     NULL in FP to at least try being consistent with X.  */
	  if (EQ (track_mouse, Qdrag_source)
	      && (screen_x < 0 || screen_y < 0
		  || screen_x >= FRAME_PIXEL_WIDTH (f1)
		  || screen_y >= FRAME_PIXEL_HEIGHT (f1)))
	    *fp = NULL;
	  else
	    *fp = f1;

	  *timestamp = x_display_list->last_mouse_movement_time;
	  XSETINT (*x, screen_x);
	  XSETINT (*y, screen_y);
	}
    }

  unblock_input ();
}

static void
haiku_flush (struct frame *f)
{
  /* This is needed for tooltip frames to work properly with double
     buffering.  */
  if (FRAME_DIRTY_P (f) && !buffer_flipping_blocked_p ())
    haiku_flip_buffers (f);

  if (FRAME_VISIBLE_P (f) && !FRAME_TOOLTIP_P (f))
    BWindow_Flush (FRAME_HAIKU_WINDOW (f));
}

static void
haiku_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  if (f->tooltip)
    return;
  block_input ();
  if (!f->pointer_invisible && FRAME_HAIKU_VIEW (f)
      && !FRAME_OUTPUT_DATA (f)->hourglass_p)
    BView_set_view_cursor (FRAME_HAIKU_VIEW (f), cursor);
  unblock_input ();
  FRAME_OUTPUT_DATA (f)->current_cursor = cursor;
}

static void
haiku_update_window_end (struct window *w, bool cursor_on_p,
			 bool mouse_face_overwritten_p)
{

}

static void
haiku_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct haiku_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param = gui_display_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
                                                RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font_param))
    {
      /* System font should take precedence over X resources.  We suggest this
         regardless of font-use-system-font because .emacs may not have been
         read yet.  */
      struct haiku_font_pattern ptn;
      ptn.specified = 0;

      BFont_populate_fixed_family (&ptn);

      if (ptn.specified & FSPEC_FAMILY)
	font = font_open_by_name (f, build_unibyte_string (ptn.family));
    }

  if (NILP (font))
      font = !NILP (font_param) ? font_param
      : gui_display_get_arg (dpyinfo, parms, Qfont, "font", "Font",
                             RES_TYPE_STRING);

  if (! FONTP (font) && ! STRINGP (font))
    {
      const char **names = (const char *[]) { "monospace-12",
					      "Noto Sans Mono-12",
					      "Source Code Pro-12",
					      NULL };
      int i;

      for (i = 0; names[i]; i++)
        {
          font
            = font_open_by_name (f, build_unibyte_string (names[i]));
          if (!NILP (font))
            break;
        }
      if (NILP (font))
        error ("No suitable font was found");
    }
  else if (!NILP (font_param))
    {
      /* Remember the explicit font parameter, so we can re-apply it
         after we've applied the `default' face settings.  */
      AUTO_FRAME_ARG (arg, Qfont_parameter, font_param);
      gui_set_frame_parameters (f, arg);
    }

  gui_default_parameter (f, parms, Qfont, font, "font", "Font",
                         RES_TYPE_STRING);
}

static struct redisplay_interface haiku_redisplay_interface =
  {
    haiku_frame_parm_handlers,
    gui_produce_glyphs,
    gui_write_glyphs,
    gui_insert_glyphs,
    gui_clear_end_of_line,
    haiku_scroll_run,
    haiku_after_update_window_line,
    NULL,
    haiku_update_window_end,
    haiku_flush,
    gui_clear_window_mouse_face,
    gui_get_glyph_overhangs,
    gui_fix_overlapping_area,
    haiku_draw_fringe_bitmap,
    haiku_define_fringe_bitmap,
    haiku_destroy_fringe_bitmap,
    haiku_compute_glyph_string_overhangs,
    haiku_draw_glyph_string,
    haiku_define_frame_cursor,
    haiku_clear_frame_area,
    haiku_clear_under_internal_border,
    haiku_draw_window_cursor,
    haiku_draw_vertical_window_border,
    haiku_draw_window_divider,
    0, /* shift glyphs for insert */
    haiku_show_hourglass,
    haiku_hide_hourglass,
    haiku_default_font_parameter,
  };

static void
haiku_make_fullscreen_consistent (struct frame *f)
{
  Lisp_Object lval = get_frame_param (f, Qfullscreen);

  if (!EQ (lval, Qmaximized) && FRAME_OUTPUT_DATA (f)->zoomed_p)
    lval = Qmaximized;
  else if (EQ (lval, Qmaximized) && !FRAME_OUTPUT_DATA (f)->zoomed_p)
    lval = Qnil;

  store_frame_param (f, Qfullscreen, lval);
}

static void
flush_dirty_back_buffers (void)
{
  block_input ();
  Lisp_Object tail, frame;
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_LIVE_P (f) &&
          FRAME_HAIKU_P (f) &&
          FRAME_HAIKU_WINDOW (f) &&
          !FRAME_GARBAGED_P (f) &&
          !buffer_flipping_blocked_p () &&
          FRAME_DIRTY_P (f))
        haiku_flip_buffers (f);
    }
  unblock_input ();
}

static int
haiku_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  block_input ();
  int message_count = 0;
  static void *buf = NULL;
  ssize_t b_size;
  struct unhandled_event *unhandled_events = NULL;
  int button_or_motion_p;
  int need_flush = 0;
  int do_help = 0;

  if (!buf)
    buf = xmalloc (200);
  haiku_read_size (&b_size, false);
  while (b_size >= 0)
    {
      enum haiku_event_type type;
      struct input_event inev, inev2;

      if (b_size > 200)
	emacs_abort ();

      EVENT_INIT (inev);
      EVENT_INIT (inev2);
      inev.kind = NO_EVENT;
      inev2.kind = NO_EVENT;
      inev.arg = Qnil;
      inev2.arg = Qnil;

      button_or_motion_p = 0;

      haiku_read (&type, buf, b_size);

      switch (type)
	{
	case QUIT_REQUESTED:
	  {
	    struct haiku_quit_requested_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    inev.kind = DELETE_WINDOW_EVENT;
	    XSETFRAME (inev.frame_or_window, f);
	    break;
	  }
	case FRAME_RESIZED:
	  {
	    struct haiku_resize_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    int width = lrint (b->px_widthf);
	    int height = lrint (b->px_heightf);

	    if (FRAME_TOOLTIP_P (f))
	      {
		if (FRAME_PIXEL_WIDTH (f) != width
		    || FRAME_PIXEL_HEIGHT (f) != height)
		  SET_FRAME_GARBAGED (f);

		FRAME_PIXEL_WIDTH (f) = width;
		FRAME_PIXEL_HEIGHT (f) = height;

		haiku_clear_under_internal_border (f);
		continue;
	      }

	    BView_draw_lock (FRAME_HAIKU_VIEW (f), false, 0, 0, 0, 0);
	    BView_resize_to (FRAME_HAIKU_VIEW (f), width, height);
	    BView_draw_unlock (FRAME_HAIKU_VIEW (f));

	    if (width != FRAME_PIXEL_WIDTH (f)
		|| height != FRAME_PIXEL_HEIGHT (f)
		|| (f->new_size_p
		    && ((f->new_width >= 0 && width != f->new_width)
			|| (f->new_height >= 0 && height != f->new_height))))
	      {
		change_frame_size (f, width, height, false, true, false);
		SET_FRAME_GARBAGED (f);
		cancel_mouse_face (f);
		haiku_clear_under_internal_border (f);
	      }
	    break;
	  }
	case FRAME_EXPOSED:
	  {
	    struct haiku_expose_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    expose_frame (f, b->x, b->y, b->width, b->height);

	    haiku_clear_under_internal_border (f);
	    break;
	  }
	case KEY_DOWN:
	  {
	    struct haiku_key_event *b = buf;
	    Mouse_HLInfo *hlinfo = &x_display_list->mouse_highlight;
	    struct frame *f = haiku_window_to_frame (b->window);

	    /* If mouse-highlight is an integer, input clears out
	       mouse highlighting.  */
	    if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
		&& (f == 0
		    || !EQ (f->tool_bar_window, hlinfo->mouse_face_window)
		    || !EQ (f->tab_bar_window, hlinfo->mouse_face_window)))
	      {
		clear_mouse_face (hlinfo);
		hlinfo->mouse_face_hidden = true;
		need_flush = 1;
	      }

	    if (!f)
	      continue;

	    inev.code = b->keysym ? b->keysym : b->multibyte_char;

	    if (b->keysym)
	      inev.kind = NON_ASCII_KEYSTROKE_EVENT;
	    else
	      inev.kind = inev.code > 127 ? MULTIBYTE_CHAR_KEYSTROKE_EVENT :
		ASCII_KEYSTROKE_EVENT;

	    inev.timestamp = b->time / 1000;
	    inev.modifiers = (haiku_modifiers_to_emacs (b->modifiers)
			      | (extra_keyboard_modifiers
				 & (meta_modifier
				    | hyper_modifier
				    | ctrl_modifier
				    | alt_modifier
				    | shift_modifier)));

	    XSETFRAME (inev.frame_or_window, f);
	    break;
	  }
	case ACTIVATION:
	  {
	    struct haiku_activation_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    if ((x_display_list->focus_event_frame != f && b->activated_p) ||
		(x_display_list->focus_event_frame == f && !b->activated_p))
	      {
		haiku_new_focus_frame (b->activated_p ? f : NULL);
		if (b->activated_p)
		  x_display_list->focus_event_frame = f;
		else
		  x_display_list->focus_event_frame = NULL;
		inev.kind = b->activated_p ? FOCUS_IN_EVENT : FOCUS_OUT_EVENT;
		XSETFRAME (inev.frame_or_window, f);
	      }

	    break;
	  }
	case MENU_BAR_LEFT:
	  {
	    struct haiku_menu_bar_left_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    if (b->y > 0 && b->y <= FRAME_PIXEL_HEIGHT (f)
		&& b->x > 0 && b->x <= FRAME_PIXEL_WIDTH (f))
	      break;

	    if (f->auto_lower && !popup_activated_p)
	      haiku_frame_raise_lower (f, 0);

	    break;
	  }
	case MOUSE_MOTION:
	  {
	    struct haiku_mouse_motion_event *b = buf;
	    struct frame *f = haiku_mouse_or_wdesc_frame (b->window);
	    Mouse_HLInfo *hlinfo = &x_display_list->mouse_highlight;
	    Lisp_Object frame;

	    if (!f)
	      continue;

	    if (FRAME_TOOLTIP_P (f))
	      {
		/* Dismiss the tooltip if the mouse moves onto a
		   tooltip frame.  FIXME: for some reason we don't get
		   leave notification events for this.  */

		if (any_help_event_p)
		  do_help = -1;

		break;
	      }

	    XSETFRAME (frame, f);

	    x_display_list->last_mouse_movement_time = b->time / 1000;
	    button_or_motion_p = 1;

	    if (hlinfo->mouse_face_hidden)
	      {
		hlinfo->mouse_face_hidden = false;
		clear_mouse_face (hlinfo);
		need_flush = 1;
	      }

	    if (b->just_exited_p)
	      {
		Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
		if (f == hlinfo->mouse_face_mouse_frame)
		  {
		    /* If we move outside the frame, then we're
		       certainly no longer on any text in the frame.  */
		    clear_mouse_face (hlinfo);
		    hlinfo->mouse_face_mouse_frame = 0;

		    need_flush = 1;
		  }

		if (f->auto_lower && !popup_activated_p
		    /* Don't do this if the mouse entered a scroll bar.  */
		    && !BView_inside_scroll_bar (FRAME_HAIKU_VIEW (f),
						 b->x, b->y))
		  {
		    /* If we're leaving towards the menu bar, don't
		       auto-lower here, and wait for a exit
		       notification from the menu bar instead.  */
		    if (b->x > FRAME_PIXEL_WIDTH (f)
			|| b->y >= FRAME_MENU_BAR_HEIGHT (f)
			|| b->x < 0
			|| b->y < 0)
		      haiku_frame_raise_lower (f, 0);
		  }

		haiku_new_focus_frame (x_display_list->focused_frame);

		if (any_help_event_p)
		  do_help = -1;
	      }
	    else
	      {
		struct haiku_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
		struct haiku_rect r = dpyinfo->last_mouse_glyph;

		/* For an unknown reason Haiku sends phantom motion events when a
		   tooltip frame is visible.  FIXME */
		if (FRAMEP (tip_frame)
		    && FRAME_LIVE_P (XFRAME (tip_frame))
		    && FRAME_VISIBLE_P (XFRAME (tip_frame))
		    && f == dpyinfo->last_mouse_motion_frame
		    && b->x == dpyinfo->last_mouse_motion_x
		    && b->y == dpyinfo->last_mouse_motion_y)
		  continue;

		dpyinfo->last_mouse_motion_x = b->x;
		dpyinfo->last_mouse_motion_y = b->y;
		dpyinfo->last_mouse_motion_frame = f;

		previous_help_echo_string = help_echo_string;
		help_echo_string = Qnil;

		/* A LeaveNotify event (well, the closest equivalent on Haiku, which
		   is a B_MOUSE_MOVED event with `transit' set to B_EXITED_VIEW) might
		   be sent out-of-order with regards to motion events from other
		   windows, such as when the mouse pointer rapidly moves from an
		   undecorated child frame to its parent.  This can cause a failure to
		   clear the mouse face on the former if an event for the latter is
		   read by Emacs first and ends up showing the mouse face there.

		   In case the `movement_locker' (also see the comment
		   there) doesn't take care of the problem, work
		   around it by clearing the mouse face now, if it is
		   currently shown on a different frame.  */

		if (hlinfo->mouse_face_hidden
		    || (f != hlinfo->mouse_face_mouse_frame
			&& !NILP (hlinfo->mouse_face_window)))
		  {
		    hlinfo->mouse_face_hidden = 0;
		    clear_mouse_face (hlinfo);
		  }

		if (f != dpyinfo->last_mouse_glyph_frame
		    || b->x < r.x || b->x >= r.x + r.width
		    || b->y < r.y || b->y >= r.y + r.height)
		  {
		    f->mouse_moved = true;
		    dpyinfo->last_mouse_scroll_bar = NULL;
		    note_mouse_highlight (f, b->x, b->y);
		    remember_mouse_glyph (f, b->x, b->y,
					  &FRAME_DISPLAY_INFO (f)->last_mouse_glyph);
		    dpyinfo->last_mouse_glyph_frame = f;
		  }
		else
		  help_echo_string = previous_help_echo_string;

		if (!NILP (Vmouse_autoselect_window))
		  {
		    static Lisp_Object last_mouse_window;
		    Lisp_Object window = window_from_coordinates (f, b->x, b->y, 0, 0, 0);

		    if (WINDOWP (window)
			&& !EQ (window, last_mouse_window)
			&& !EQ (window, selected_window)
			&& !popup_activated_p
			&& !MINI_WINDOW_P (XWINDOW (selected_window))
			&& (!NILP (focus_follows_mouse)
			    || f == SELECTED_FRAME ()))
		      {
			inev2.kind = SELECT_WINDOW_EVENT;
			inev2.frame_or_window = window;
		      }

		    last_mouse_window = window;
		  }

		if (f->auto_raise)
		  {
		    if (!BWindow_is_active (FRAME_HAIKU_WINDOW (f)))
		      haiku_frame_raise_lower (f, 1);
		  }

		if (!NILP (help_echo_string)
		    || !NILP (previous_help_echo_string))
		  do_help = 1;

		if (b->dnd_message)
		  {
		    /* It doesn't make sense to show tooltips when
		       another program is dragging stuff over us.  */

		    do_help = -1;

		    if (!be_drag_and_drop_in_progress ())
		      {
			inev.kind = DRAG_N_DROP_EVENT;
			inev.arg = Qlambda;

			XSETINT (inev.x, b->x);
			XSETINT (inev.y, b->y);
			XSETFRAME (inev.frame_or_window, f);
		      }
		    else
		      haiku_note_drag_motion ();

		    break;
		  }
	      }

	    if (FRAME_DIRTY_P (f))
	      need_flush = 1;
	    break;
	  }
	case BUTTON_UP:
	case BUTTON_DOWN:
	  {
	    struct haiku_button_event *b = buf;
	    struct frame *f = haiku_mouse_or_wdesc_frame (b->window);
	    Lisp_Object tab_bar_arg = Qnil;
	    int tab_bar_p = 0, tool_bar_p = 0;
	    bool up_okay_p = false;

	    if (popup_activated_p || !f)
	      continue;

	    struct haiku_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

	    inev.modifiers = haiku_modifiers_to_emacs (b->modifiers);

	    x_display_list->last_mouse_glyph_frame = 0;
	    x_display_list->last_mouse_movement_time = b->time / 1000;
	    button_or_motion_p = 1;

	    /* Is this in the tab-bar?  */
	    if (WINDOWP (f->tab_bar_window)
		&& WINDOW_TOTAL_LINES (XWINDOW (f->tab_bar_window)))
	      {
		Lisp_Object window;
		int x = b->x;
		int y = b->y;

		window = window_from_coordinates (f, x, y, 0, true, true);
		tab_bar_p = EQ (window, f->tab_bar_window);

		if (tab_bar_p)
		  {
		    tab_bar_arg = handle_tab_bar_click
		      (f, x, y, type == BUTTON_DOWN, inev.modifiers);
		    need_flush = 1;
		  }
	      }

	    if (WINDOWP (f->tool_bar_window)
		&& WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
	      {
		Lisp_Object window;
		int x = b->x;
		int y = b->y;

		window = window_from_coordinates (f, x, y, 0, true, true);
		tool_bar_p = EQ (window, f->tool_bar_window);

		if (tool_bar_p)
		  {
		    handle_tool_bar_click
		      (f, x, y, type == BUTTON_DOWN, inev.modifiers);
		    need_flush = 1;
		  }
	      }

	    if (type == BUTTON_UP)
	      {
		inev.modifiers |= up_modifier;
		up_okay_p = (dpyinfo->grabbed & (1 << b->btn_no));
		dpyinfo->grabbed &= ~(1 << b->btn_no);
	      }
	    else
	      {
		up_okay_p = true;
		inev.modifiers |= down_modifier;
		dpyinfo->last_mouse_frame = f;
		dpyinfo->grabbed |= (1 << b->btn_no);
		if (f && !tab_bar_p)
		  f->last_tab_bar_item = -1;
		if (f && !tool_bar_p)
		  f->last_tool_bar_item = -1;
	      }

	    if (up_okay_p
		&& !(tab_bar_p && NILP (tab_bar_arg))
		&& !tool_bar_p)
	      inev.kind = MOUSE_CLICK_EVENT;
	    inev.arg = tab_bar_arg;
	    inev.code = b->btn_no;

	    f->mouse_moved = false;

	    XSETINT (inev.x, b->x);
	    XSETINT (inev.y, b->y);

	    XSETFRAME (inev.frame_or_window, f);
	    break;
	  }
	case ICONIFICATION:
	  {
	    struct haiku_iconification_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    if (!b->iconified_p)
	      {
		SET_FRAME_VISIBLE (f, 1);
		SET_FRAME_ICONIFIED (f, 0);
		inev.kind = DEICONIFY_EVENT;


		/* Haiku doesn't expose frames on deiconification, but
		   if we are double-buffered, the previous screen
		   contents should have been preserved. */
		if (!EmacsView_double_buffered_p (FRAME_HAIKU_VIEW (f)))
		  {
		    SET_FRAME_GARBAGED (f);
		    expose_frame (f, 0, 0, 0, 0);
		  }
	      }
	    else
	      {
		SET_FRAME_VISIBLE (f, 0);
		SET_FRAME_ICONIFIED (f, 1);
		inev.kind = ICONIFY_EVENT;
	      }

	    XSETFRAME (inev.frame_or_window, f);
	    break;
	  }
	case MOVE_EVENT:
	  {
	    struct haiku_move_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    if (FRAME_PARENT_FRAME (f))
	      haiku_coords_from_parent (f, &b->x, &b->y);

	    if (b->x != f->left_pos || b->y != f->top_pos)
	      {
		inev.kind = MOVE_FRAME_EVENT;

		XSETINT (inev.x, b->x);
		XSETINT (inev.y, b->y);

		f->left_pos = b->x;
		f->top_pos = b->y;

		struct frame *p;

		if ((p = FRAME_PARENT_FRAME (f)))
		  {
		    void *window = FRAME_HAIKU_WINDOW (p);
		    EmacsWindow_move_weak_child (window, b->window, b->x, b->y);
		  }

		XSETFRAME (inev.frame_or_window, f);
	      }

	    haiku_make_fullscreen_consistent (f);
	    break;
	  }
	case SCROLL_BAR_VALUE_EVENT:
	  {
	    struct haiku_scroll_bar_value_event *b = buf;
	    struct scroll_bar *bar
	      = haiku_scroll_bar_from_widget (b->scroll_bar, b->window);
	    int portion, whole;

	    if (!bar)
	      continue;

	    struct window *w = XWINDOW (bar->window);

	    if (bar->update != -1)
	      {
		bar->update = -1;
		break;
	      }

	    if (bar->position != b->position)
	      {
		inev.kind = (bar->horizontal
			     ? HORIZONTAL_SCROLL_BAR_CLICK_EVENT :
			     SCROLL_BAR_CLICK_EVENT);
		inev.part = bar->horizontal ?
		  scroll_bar_horizontal_handle : scroll_bar_handle;

		if (bar->horizontal)
		  {
		    portion = bar->total * ((float) b->position
					    / BE_SB_MAX);
		    whole = (bar->total
			     * ((float) (BE_SB_MAX - bar->page_size)
				/ BE_SB_MAX));
		    portion = min (portion, whole);
		  }
		else
		  {
		    whole = BE_SB_MAX - bar->page_size;
		    portion = min (b->position, whole);
		  }

		XSETINT (inev.x, portion);
		XSETINT (inev.y, whole);
		XSETWINDOW (inev.frame_or_window, w);
	      }
	    break;
	  }
	case SCROLL_BAR_PART_EVENT:
	  {
	    struct haiku_scroll_bar_part_event *b = buf;
	    struct scroll_bar *bar
	      = haiku_scroll_bar_from_widget (b->scroll_bar, b->window);

	    if (!bar)
	      continue;

	    inev.kind = (bar->horizontal ? HORIZONTAL_SCROLL_BAR_CLICK_EVENT
			 : SCROLL_BAR_CLICK_EVENT);

	    bar->dragging = 0;

	    switch (b->part)
	      {
	      case HAIKU_SCROLL_BAR_UP_BUTTON:
		inev.part = (bar->horizontal
			     ? scroll_bar_left_arrow
			     : scroll_bar_up_arrow);
		break;
	      case HAIKU_SCROLL_BAR_DOWN_BUTTON:
		inev.part = (bar->horizontal
			     ? scroll_bar_right_arrow
			     : scroll_bar_down_arrow);
		break;
	      }

	    XSETINT (inev.x, 0);
	    XSETINT (inev.y, 0);
	    inev.frame_or_window = bar->window;

	    break;
	  }
	case SCROLL_BAR_DRAG_EVENT:
	  {
	    struct haiku_scroll_bar_drag_event *b = buf;
	    struct scroll_bar *bar
	      = haiku_scroll_bar_from_widget (b->scroll_bar, b->window);

	    if (!bar)
	      continue;

	    bar->dragging = b->dragging_p;
	    if (!b->dragging_p && bar->horizontal)
	      set_horizontal_scroll_bar (XWINDOW (bar->window));
	    else if (!b->dragging_p)
	      set_vertical_scroll_bar (XWINDOW (bar->window));
	    break;
	  }
	case WHEEL_MOVE_EVENT:
	  {
	    struct haiku_wheel_move_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);
	    int x, y, scroll_width, scroll_height;
	    static float px = 0.0f, py = 0.0f;
	    Lisp_Object wheel_window;

	    if (!f)
	      continue;

	    BView_get_mouse (FRAME_HAIKU_VIEW (f), &x, &y);

	    wheel_window = window_from_coordinates (f, x, y, 0, false, false);

	    if (NILP (wheel_window))
	      {
		scroll_width = FRAME_PIXEL_WIDTH (f);
		scroll_height = FRAME_PIXEL_HEIGHT (f);
	      }
	    else
	      {
		scroll_width = XWINDOW (wheel_window)->pixel_width;
		scroll_height = XWINDOW (wheel_window)->pixel_height;
	      }

	    inev.modifiers = haiku_modifiers_to_emacs (b->modifiers);

	    inev2.modifiers = inev.modifiers;

	    if (signbit (px) != signbit (b->delta_x))
	      px = 0;

	    if (signbit (py) != signbit (b->delta_y))
	      py = 0;

	    px += (b->delta_x
		   * powf (scroll_width, 2.0f / 3.0f));
	    py += (b->delta_y
		   * powf (scroll_height, 2.0f / 3.0f));

	    if (fabsf (py) >= FRAME_LINE_HEIGHT (f)
		|| fabsf (px) >= FRAME_COLUMN_WIDTH (f)
		|| !mwheel_coalesce_scroll_events)
	      {
		inev.kind = (fabsf (px) > fabsf (py)
			     ? HORIZ_WHEEL_EVENT
			     : WHEEL_EVENT);
		inev.code = 0;

		XSETINT (inev.x, x);
		XSETINT (inev.y, y);
		inev.arg = list3 (Qnil, make_float (-px),
				  make_float (-py));
		XSETFRAME (inev.frame_or_window, f);

		inev.modifiers |= (signbit (inev.kind == HORIZ_WHEEL_EVENT
					    ? px : py)
				   ? up_modifier
				   : down_modifier);
		py = 0.0f;
		px = 0.0f;
	      }

	    break;
	  }

	case MENU_BAR_RESIZE:
	  {
	    struct haiku_menu_bar_resize_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f))
	      continue;

	    int old_height = FRAME_MENU_BAR_HEIGHT (f);

	    FRAME_MENU_BAR_HEIGHT (f) = b->height + 1;
	    FRAME_MENU_BAR_LINES (f) =
	      (b->height + FRAME_LINE_HEIGHT (f)) / FRAME_LINE_HEIGHT (f);

	    if (old_height != b->height)
	      {
		adjust_frame_size (f, -1, -1, 3, true, Qmenu_bar_lines);
		haiku_clear_under_internal_border (f);
	      }
	    break;
	  }
	case MENU_BAR_OPEN:
	case MENU_BAR_CLOSE:
	  {
	    struct haiku_menu_bar_state_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f))
	      continue;

	    if (type == MENU_BAR_OPEN)
	      {
		/* b->no_lock means that MenusBeginning was called
		   from the main thread, which means tracking was
		   started manually, and we have already updated the
		   menu bar.  */
		if (!b->no_lock)
		  {
		    BView_draw_lock (FRAME_HAIKU_VIEW (f), false, 0, 0, 0, 0);
		    /* This shouldn't be here, but nsmenu does it, so
		       it should probably be safe.  */
		    int was_waiting_for_input_p = waiting_for_input;
		    if (waiting_for_input)
		      waiting_for_input = 0;
		    set_frame_menubar (f, 1);
		    waiting_for_input = was_waiting_for_input_p;
		    BView_draw_unlock (FRAME_HAIKU_VIEW (f));
		  }

		/* But set the flag anyway, because the menu will end
		   from the window thread.  */
		FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 1;
		popup_activated_p += 1;

		if (!b->no_lock)
		  EmacsWindow_signal_menu_update_complete (b->window);
	      }
	    else
	      {
		if (!popup_activated_p)
		  emacs_abort ();
		if (FRAME_OUTPUT_DATA (f)->menu_bar_open_p)
		  {
		    FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 0;
		    popup_activated_p -= 1;
		  }
	      }
	    break;
	  }
	case MENU_BAR_SELECT_EVENT:
	  {
	    struct haiku_menu_bar_select_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f))
	      continue;

	    if (FRAME_OUTPUT_DATA (f)->menu_up_to_date_p)
	      find_and_call_menu_selection (f, f->menu_bar_items_used,
					    f->menu_bar_vector, b->ptr);
	    break;
	  }
	case FILE_PANEL_EVENT:
	  {
	    if (!popup_activated_p)
	      continue;

	    struct unhandled_event *ev = xmalloc (sizeof *ev);
	    ev->next = unhandled_events;
	    ev->type = type;
	    memcpy (&ev->buffer, buf, 200);

	    unhandled_events = ev;
	    break;
	  }
	case MENU_BAR_HELP_EVENT:
	  {
	    struct haiku_menu_bar_help_event *b = buf;

	    if (!popup_activated_p)
	      continue;

	    struct frame *f = haiku_window_to_frame (b->window);
	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f) ||
		!FRAME_OUTPUT_DATA (f)->menu_bar_open_p)
	      continue;

	    run_menu_bar_help_event (f, b->mb_idx);

	    break;
	  }
	case ZOOM_EVENT:
	  {
	    struct haiku_zoom_event *b = buf;

	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      continue;

	    FRAME_OUTPUT_DATA (f)->zoomed_p = b->zoomed;
	    haiku_make_fullscreen_consistent (f);
	    break;
	  }
	case DRAG_AND_DROP_EVENT:
	  {
	    struct haiku_drag_and_drop_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f)
	      {
		BMessage_delete (b->message);
		continue;
	      }

	    inev.kind = DRAG_N_DROP_EVENT;
	    inev.arg = haiku_message_to_lisp (b->message);

	    XSETINT (inev.x, b->x);
	    XSETINT (inev.y, b->y);
	    XSETFRAME (inev.frame_or_window, f);

	    BMessage_delete (b->message);
	    break;
	  }
	case APP_QUIT_REQUESTED_EVENT:
	case KEY_UP:
	case DUMMY_EVENT:
	default:
	  break;
	}

      haiku_read_size (&b_size, false);

      if (inev.kind != NO_EVENT)
	{
	  if (inev.kind != HELP_EVENT && !inev.timestamp)
	    inev.timestamp = (button_or_motion_p
			      ? x_display_list->last_mouse_movement_time
			      : system_time () / 1000);
	  kbd_buffer_store_event_hold (&inev, hold_quit);
	  ++message_count;
	}

      if (inev2.kind != NO_EVENT)
	{
	  if (inev2.kind != HELP_EVENT && !inev.timestamp)
	    inev2.timestamp = (button_or_motion_p
			       ? x_display_list->last_mouse_movement_time
			       : system_time () / 1000);
	  kbd_buffer_store_event_hold (&inev2, hold_quit);
	  ++message_count;
	}
    }

  for (struct unhandled_event *ev = unhandled_events; ev;)
    {
      haiku_write_without_signal (ev->type, &ev->buffer, false);
      struct unhandled_event *old = ev;
      ev = old->next;
      xfree (old);
    }

  if (do_help && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object help_frame = Qnil;

      if (x_display_list->last_mouse_frame)
	XSETFRAME (help_frame,
		   x_display_list->last_mouse_frame);

      if (do_help > 0)
	{
	  any_help_event_p = true;
	  gen_help_event (help_echo_string, help_frame,
			  help_echo_window, help_echo_object,
			  help_echo_pos);
	}
      else
	{
	  help_echo_string = Qnil;
	  gen_help_event (Qnil, help_frame, Qnil, Qnil, 0);
	}
    }

  if (need_flush)
    flush_dirty_back_buffers ();

  unblock_input ();

  return message_count;
}

static void
haiku_frame_rehighlight (struct frame *frame)
{
  haiku_rehighlight ();
}

static void
haiku_delete_window (struct frame *f)
{
  check_window_system (f);
  haiku_free_frame_resources (f);
}

static void
haiku_free_pixmap (struct frame *f, Emacs_Pixmap pixmap)
{
  BBitmap_free (pixmap);
}

static void
haiku_flash (struct frame *f)
{
  /* Get the height not including a menu bar widget.  */
  int height = FRAME_PIXEL_HEIGHT (f);
  /* Height of each line to flash.  */
  int flash_height = FRAME_LINE_HEIGHT (f);
  /* These will be the left and right margins of the rectangles.  */
  int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
  int flash_right = FRAME_PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);
  int width = flash_right - flash_left;
  void *view = FRAME_HAIKU_VIEW (f);
  struct timespec delay, wakeup, current, timeout;

  delay = make_timespec (0, 150 * 1000 * 1000);
  wakeup = timespec_add (current_timespec (), delay);

  BView_draw_lock (view, true, 0, 0, FRAME_PIXEL_WIDTH (f),
		   FRAME_PIXEL_HEIGHT (f));
  BView_StartClip (view);
  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      BView_InvertRect (view, flash_left,
			(FRAME_INTERNAL_BORDER_WIDTH (f)
			 + FRAME_TOP_MARGIN_HEIGHT (f)),
			width, flash_height);

      BView_InvertRect (view, flash_left,
			(height - flash_height
			 - FRAME_INTERNAL_BORDER_WIDTH (f)),
			width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    BView_InvertRect (view, flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
		      width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));
  BView_EndClip (view);
  BView_draw_unlock (view);

  flush_frame (f);

  if (EmacsView_double_buffered_p (view))
    haiku_flip_buffers (f);

  /* Keep waiting until past the time wakeup or any input gets
     available.  */
  while (!detect_input_pending ())
    {
      current = current_timespec ();

      /* Break if result would not be positive.  */
      if (timespec_cmp (wakeup, current) <= 0)
	break;

      /* How long `select' should wait.  */
      timeout = make_timespec (0, 10 * 1000 * 1000);

      /* Try to wait that long--but we might wake up sooner.  */
      pselect (0, NULL, NULL, NULL, &timeout, NULL);
    }

  BView_draw_lock (view, true, 0, 0, FRAME_PIXEL_WIDTH (f),
		   FRAME_PIXEL_HEIGHT (f));
  BView_StartClip (view);
  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      BView_InvertRect (view, flash_left,
			(FRAME_INTERNAL_BORDER_WIDTH (f)
			 + FRAME_TOP_MARGIN_HEIGHT (f)),
			width, flash_height);

      BView_InvertRect (view, flash_left,
			(height - flash_height
			 - FRAME_INTERNAL_BORDER_WIDTH (f)),
			width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    BView_InvertRect (view, flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
		      width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));
  BView_EndClip (view);
  BView_draw_unlock (view);

  flush_frame (f);
  if (EmacsView_double_buffered_p (view))
    haiku_flip_buffers (f);
}

static void
haiku_beep (struct frame *f)
{
  if (visible_bell)
    {
      void *view = FRAME_HAIKU_VIEW (f);
      if (view)
	{
	  block_input ();
	  haiku_flash (f);
	  unblock_input ();
	}
    }
  else
    haiku_ring_bell ();
}

static void
haiku_toggle_invisible_pointer (struct frame *f, bool invisible_p)
{
  void *view = FRAME_HAIKU_VIEW (f);

  if (view)
    {
      block_input ();
      BView_set_view_cursor (view, invisible_p ?
			     FRAME_OUTPUT_DATA (f)->no_cursor :
			     FRAME_OUTPUT_DATA (f)->current_cursor);
      f->pointer_invisible = invisible_p;
      unblock_input ();
    }
}

static void
haiku_fullscreen (struct frame *f)
{
  /* When FRAME_OUTPUT_DATA (f)->configury_done is false, the frame is
     being created, and its regular width and height have not yet been
     set.  This function will be called again by haiku_create_frame,
     so do nothing.  */
  if (!FRAME_OUTPUT_DATA (f)->configury_done)
    return;

  if (f->want_fullscreen == FULLSCREEN_MAXIMIZED)
    BWindow_zoom (FRAME_HAIKU_WINDOW (f));
  else if (f->want_fullscreen == FULLSCREEN_BOTH)
    EmacsWindow_make_fullscreen (FRAME_HAIKU_WINDOW (f), 1);
  else
    {
      EmacsWindow_make_fullscreen (FRAME_HAIKU_WINDOW (f), 0);
      EmacsWindow_unzoom (FRAME_HAIKU_WINDOW (f));
    }

  f->want_fullscreen = FULLSCREEN_NONE;

  haiku_update_size_hints (f);
}

static struct terminal *
haiku_create_terminal (struct haiku_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_haiku, &haiku_redisplay_interface);

  terminal->display_info.haiku = dpyinfo;
  dpyinfo->terminal = terminal;
  terminal->kboard = allocate_kboard (Qhaiku);

  terminal->iconify_frame_hook = haiku_iconify_frame;
  terminal->focus_frame_hook = haiku_focus_frame;
  terminal->ring_bell_hook = haiku_beep;
  terminal->popup_dialog_hook = haiku_popup_dialog;
  terminal->frame_visible_invisible_hook = haiku_set_frame_visible_invisible;
  terminal->set_frame_offset_hook = haiku_set_offset;
  terminal->delete_terminal_hook = haiku_delete_terminal;
  terminal->get_string_resource_hook = get_string_resource;
  terminal->set_new_font_hook = haiku_new_font;
  terminal->defined_color_hook = haiku_defined_color;
  terminal->set_window_size_hook = haiku_set_window_size;
  terminal->read_socket_hook = haiku_read_socket;
  terminal->implicit_set_name_hook = haiku_implicitly_set_name;
  terminal->mouse_position_hook = haiku_mouse_position;
  terminal->delete_frame_hook = haiku_delete_window;
  terminal->frame_up_to_date_hook = haiku_frame_up_to_date;
  terminal->buffer_flipping_unblocked_hook = haiku_buffer_flipping_unblocked_hook;
  terminal->clear_frame_hook = haiku_clear_frame;
  terminal->change_tab_bar_height_hook = haiku_change_tab_bar_height;
  terminal->change_tool_bar_height_hook = haiku_change_tool_bar_height;
  terminal->set_vertical_scroll_bar_hook = haiku_set_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = haiku_set_horizontal_scroll_bar;
  terminal->set_scroll_bar_default_height_hook = haiku_set_scroll_bar_default_height;
  terminal->set_scroll_bar_default_width_hook = haiku_set_scroll_bar_default_width;
  terminal->judge_scroll_bars_hook = haiku_judge_scroll_bars;
  terminal->condemn_scroll_bars_hook = haiku_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = haiku_redeem_scroll_bar;
  terminal->update_begin_hook = haiku_update_begin;
  terminal->update_end_hook = haiku_update_end;
  terminal->frame_rehighlight_hook = haiku_frame_rehighlight;
  terminal->query_frame_background_color = haiku_query_frame_background_color;
  terminal->free_pixmap = haiku_free_pixmap;
  terminal->frame_raise_lower_hook = haiku_frame_raise_lower;
  terminal->menu_show_hook = haiku_menu_show;
  terminal->toggle_invisible_pointer_hook = haiku_toggle_invisible_pointer;
  terminal->fullscreen_hook = haiku_fullscreen;
  terminal->toolkit_position_hook = haiku_toolkit_position;

  return terminal;
}

struct haiku_display_info *
haiku_term_init (void)
{
  struct haiku_display_info *dpyinfo;
  struct terminal *terminal;

  Lisp_Object color_file, color_map;

  block_input ();
  Fset_input_interrupt_mode (Qt);

  baud_rate = 19200;

  dpyinfo = xzalloc (sizeof *dpyinfo);

  haiku_io_init ();

  if (port_application_to_emacs < B_OK)
    emacs_abort ();

  color_file = Fexpand_file_name (build_string ("rgb.txt"),
				  Fsymbol_value (intern ("data-directory")));

  color_map = Fx_load_color_file (color_file);
  if (NILP (color_map))
    fatal ("Could not read %s.\n", SDATA (color_file));

  dpyinfo->color_map = color_map;

  dpyinfo->display = BApplication_setup ();

  BScreen_res (&dpyinfo->resx, &dpyinfo->resy);

  dpyinfo->next = x_display_list;
  dpyinfo->n_planes = be_get_display_planes ();
  x_display_list = dpyinfo;

  terminal = haiku_create_terminal (dpyinfo);
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;

  terminal->kboard->reference_count++;
  /* Never delete haiku displays -- there can only ever be one,
     anyhow.  */
  terminal->reference_count++;
  terminal->name = xstrdup ("be");

  dpyinfo->name_list_element = Fcons (build_string ("be"), Qnil);
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  gui_init_fringe (terminal->rif);

#define ASSIGN_CURSOR(cursor, be_cursor) (dpyinfo->cursor = be_cursor)
  ASSIGN_CURSOR (text_cursor, BCursor_create_i_beam ());
  ASSIGN_CURSOR (nontext_cursor, BCursor_create_default ());
  ASSIGN_CURSOR (modeline_cursor, BCursor_create_modeline ());
  ASSIGN_CURSOR (hand_cursor, BCursor_create_grab ());
  ASSIGN_CURSOR (hourglass_cursor, BCursor_create_progress_cursor ());
  ASSIGN_CURSOR (horizontal_drag_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_EAST_WEST));
  ASSIGN_CURSOR (vertical_drag_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_NORTH_SOUTH));
  ASSIGN_CURSOR (left_edge_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_WEST));
  ASSIGN_CURSOR (top_left_corner_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_NORTH_WEST));
  ASSIGN_CURSOR (top_edge_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_NORTH));
  ASSIGN_CURSOR (top_right_corner_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_NORTH_EAST));
  ASSIGN_CURSOR (right_edge_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_EAST));
  ASSIGN_CURSOR (bottom_right_corner_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_SOUTH_EAST));
  ASSIGN_CURSOR (bottom_edge_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_SOUTH));
  ASSIGN_CURSOR (bottom_left_corner_cursor,
		 BCursor_from_id (CURSOR_ID_RESIZE_SOUTH_WEST));
  ASSIGN_CURSOR (no_cursor,
		 BCursor_from_id (CURSOR_ID_NO_CURSOR));
#undef ASSIGN_CURSOR

  unblock_input ();

  return dpyinfo;
}

void
put_xrm_resource (Lisp_Object name, Lisp_Object val)
{
  eassert (STRINGP (name));
  eassert (STRINGP (val) || NILP (val));

  Lisp_Object lval = assoc_no_quit (name, rdb);
  if (!NILP (lval))
    Fsetcdr (lval, val);
  else
    rdb = Fcons (Fcons (name, val), rdb);
}

void
haiku_clear_under_internal_border (struct frame *f)
{
  if (FRAME_INTERNAL_BORDER_WIDTH (f) > 0
      /* This is needed because tooltip frames set up the internal
	 border before init_frame_faces.  */
      && FRAME_FACE_CACHE (f))
    {
      int border = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      int margin = FRAME_TOP_MARGIN_HEIGHT (f);
      int face_id =
	(FRAME_PARENT_FRAME (f)
	 ? (!NILP (Vface_remapping_alist)
	    ? lookup_basic_face (NULL, f, CHILD_FRAME_BORDER_FACE_ID)
	    : CHILD_FRAME_BORDER_FACE_ID)
	 : (!NILP (Vface_remapping_alist)
	    ? lookup_basic_face (NULL, f, INTERNAL_BORDER_FACE_ID)
	    : INTERNAL_BORDER_FACE_ID));
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);
      void *view = FRAME_HAIKU_VIEW (f);
      block_input ();
      BView_draw_lock (view, true, 0, 0, FRAME_PIXEL_WIDTH (f),
		       FRAME_PIXEL_HEIGHT (f));
      BView_StartClip (view);
      BView_ClipToRect (view, 0, 0, FRAME_PIXEL_WIDTH (f),
			FRAME_PIXEL_HEIGHT (f));

      if (face)
	BView_SetHighColor (view, face->background);
      else
	BView_SetHighColor (view, FRAME_BACKGROUND_PIXEL (f));

      BView_FillRectangle (view, 0, margin, width, border);
      BView_FillRectangle (view, 0, 0, border, height);
      BView_FillRectangle (view, 0, margin, width, border);
      BView_FillRectangle (view, width - border, 0, border, height);
      BView_FillRectangle (view, 0, height - border, width, border);
      BView_EndClip (view);
      BView_draw_unlock (view);
      unblock_input ();
    }
}

void
mark_haiku_display (void)
{
  if (x_display_list)
    mark_object (x_display_list->color_map);
}

void
haiku_scroll_bar_remove (struct scroll_bar *bar)
{
  block_input ();
  void *view = FRAME_HAIKU_VIEW (WINDOW_XFRAME (XWINDOW (bar->window)));
  BView_forget_scroll_bar (view, bar->left, bar->top, bar->width, bar->height);
  BScrollBar_delete (bar->scroll_bar);
  expose_frame (WINDOW_XFRAME (XWINDOW (bar->window)),
		bar->left, bar->top, bar->width, bar->height);

  if (bar->horizontal)
    wset_horizontal_scroll_bar (XWINDOW (bar->window), Qnil);
  else
    wset_vertical_scroll_bar (XWINDOW (bar->window), Qnil);

  unblock_input ();
};

void
haiku_set_offset (struct frame *frame, int x, int y,
		  int change_gravity)
{
  if (change_gravity > 0)
    {
      frame->top_pos = y;
      frame->left_pos = x;
      frame->size_hint_flags &= ~ (XNegative | YNegative);
      if (x < 0)
	frame->size_hint_flags |= XNegative;
      if (y < 0)
	frame->size_hint_flags |= YNegative;
      frame->win_gravity = NorthWestGravity;
    }

  haiku_update_size_hints (frame);

  block_input ();
  if (change_gravity)
    BWindow_set_offset (FRAME_HAIKU_WINDOW (frame), x, y);
  unblock_input ();
}

#ifdef USE_BE_CAIRO
cairo_t *
haiku_begin_cr_clip (struct frame *f, struct glyph_string *s)
{
  cairo_t *cr = FRAME_CR_CONTEXT (f);

  if (!cr)
    return NULL;

  cairo_save (cr);
  return cr;
}

void
haiku_end_cr_clip (cairo_t *cr)
{
  if (!cr)
    return;

  cairo_restore (cr);
}
#endif

void
haiku_merge_cursor_foreground (struct glyph_string *s,
			       unsigned long *foreground_out,
			       unsigned long *background_out)
{
  unsigned long background = FRAME_CURSOR_COLOR (s->f).pixel;
  unsigned long foreground = s->face->background;

  if (background == foreground)
    foreground = s->face->background;
  if (background == foreground)
    foreground = FRAME_OUTPUT_DATA (s->f)->cursor_fg;
  if (background == foreground)
    foreground = s->face->foreground;

  if (background == s->face->background
      || foreground == s->face->foreground)
    {
      background = s->face->foreground;
      foreground = s->face->background;
    }

  if (foreground_out)
    *foreground_out = foreground;
  if (background_out)
    *background_out = background;
}

void
syms_of_haikuterm (void)
{
  DEFVAR_BOOL ("haiku-initialized", haiku_initialized,
     doc: /* Non-nil if the Haiku terminal backend has been initialized.  */);

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_use_underline_position_properties = 1;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_underline_at_descent_line = 0;

  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
     doc: /* SKIP: real doc in xterm.c.  */);
  Vx_toolkit_scroll_bars = Qt;

  DEFVAR_BOOL ("haiku-debug-on-fatal-error", haiku_debug_on_fatal_error,
     doc: /* If non-nil, Emacs will launch the system debugger upon a fatal error.  */);
  haiku_debug_on_fatal_error = 1;

  DEFSYM (Qshift, "shift");
  DEFSYM (Qcontrol, "control");
  DEFSYM (Qoption, "option");
  DEFSYM (Qcommand, "command");

  DEFVAR_LISP ("haiku-meta-keysym", Vhaiku_meta_keysym,
     doc: /* Which key Emacs uses as the meta modifier.
This is either one of the symbols `shift', `control', `command', and
`option', or nil, in which case it is treated as `command'.

Setting it to any other value is equivalent to `command'.  */);
  Vhaiku_meta_keysym = Qnil;

  DEFVAR_LISP ("haiku-control-keysym", Vhaiku_control_keysym,
     doc: /* Which key Emacs uses as the control modifier.
This is either one of the symbols `shift', `control', `command', and
`option', or nil, in which case it is treated as `control'.

Setting it to any other value is equivalent to `control'.  */);
  Vhaiku_control_keysym = Qnil;

  DEFVAR_LISP ("haiku-super-keysym", Vhaiku_super_keysym,
     doc: /* Which key Emacs uses as the super modifier.
This is either one of the symbols `shift', `control', `command', and
`option', or nil, in which case it is treated as `option'.

Setting it to any other value is equivalent to `option'.  */);
  Vhaiku_super_keysym = Qnil;

  DEFVAR_LISP ("haiku-shift-keysym", Vhaiku_shift_keysym,
     doc: /* Which key Emacs uses as the shift modifier.
This is either one of the symbols `shift', `control', `command', and
`option', or nil, in which case it is treated as `shift'.

Setting it to any other value is equivalent to `shift'.  */);
  Vhaiku_shift_keysym = Qnil;


  DEFSYM (Qx_use_underline_position_properties,
	  "x-use-underline-position-properties");

  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");

  rdb = Qnil;
  staticpro (&rdb);

  Fprovide (Qhaiku, Qnil);
#ifdef USE_BE_CAIRO
  Fprovide (intern_c_string ("cairo"), Qnil);
#endif
}
