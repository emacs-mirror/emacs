/* Haiku window system support
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
#include "haikuselect.h"

#include <math.h>
#include <stdlib.h>

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

/* Minimum and maximum values used for Haiku scroll bars.  */
#define BE_SB_MAX 12000000

/* The single Haiku display (if any).  */
struct haiku_display_info *x_display_list;

/* This is used to determine when to evict the font lookup cache,
   which we do every 50 updates.  */
static int up_to_date_count;

/* List of defined fringe bitmaps.  */
static void **fringe_bmps;

/* The amount of fringe bitmaps in that list.  */
static int max_fringe_bmp;

/* Alist of resources to their values.  */
static Lisp_Object rdb;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */
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

  *x -= FRAME_OUTPUT_DATA (p)->frame_x;
  *y -= FRAME_OUTPUT_DATA (p)->frame_y;
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
  error ("The Haiku terminal cannot be deleted");
}

static const char *
haiku_get_string_resource (void *ignored, const char *name,
			   const char *class)
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
  if (f->tooltip)
    return;

  block_input ();
  BWindow_set_size_alignment (FRAME_HAIKU_WINDOW (f),
			      (frame_resize_pixelwise
			       ? 1 : FRAME_COLUMN_WIDTH (f)),
			      (frame_resize_pixelwise
			       ? 1 : FRAME_LINE_HEIGHT (f)));
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
	BView_ClipToRect (FRAME_HAIKU_DRAWABLE (s->f),
			  FRAME_PIXEL_WIDTH (s->f),
			  FRAME_PIXEL_HEIGHT (s->f),
			  10, 10);
      else
	{
	  BView_ClipToRect (FRAME_HAIKU_DRAWABLE (s->f), r[0].x,
			    r[0].y, r[0].width, r[0].height);
	  BView_invalidate_region (FRAME_HAIKU_DRAWABLE (s->f), r[0].x,
				   r[0].y, r[0].width, r[0].height);
	}
    }

  if (n > 1)
    {
      /* If n[FOO].width is 0, it means to not draw at all, so set the
	 clipping to some impossible value.  */
      if (r[1].width <= 0)
	BView_ClipToRect (FRAME_HAIKU_DRAWABLE (s->f),
			  FRAME_PIXEL_WIDTH (s->f),
			  FRAME_PIXEL_HEIGHT (s->f),
			  10, 10);
      else
	{
	  BView_ClipToRect (FRAME_HAIKU_DRAWABLE (s->f), r[1].x, r[1].y,
			    r[1].width, r[1].height);
	  BView_invalidate_region (FRAME_HAIKU_DRAWABLE (s->f), r[1].x,
				   r[1].y, r[1].width, r[1].height);
	}
    }
}

static void
haiku_clip_to_string_exactly (struct glyph_string *s, struct glyph_string *dst)
{
  BView_ClipToRect (FRAME_HAIKU_DRAWABLE (s->f), s->x, s->y,
		    s->width, s->height);
  BView_invalidate_region (FRAME_HAIKU_DRAWABLE (s->f), s->x,
			   s->y, s->width, s->height);
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

  up_to_date_count++;
  if (up_to_date_count == 50)
    {
      be_evict_font_cache ();
      up_to_date_count = 0;
    }

  /* Mark the frame as complete.  */
  FRAME_COMPLETE_P (f) = true;
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
  void *vw = FRAME_HAIKU_DRAWABLE (f);
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
  void *view = FRAME_HAIKU_DRAWABLE (f);

  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  FRAME_COMPLETE_P (f) = false;

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
  struct font *font;
  int ascent, descent, unit;

  font = XFONT_OBJECT (font_object);

  if (fontset < 0)
    fontset = fontset_from_font (font_object);

  FRAME_FONTSET (f) = fontset;

  if (FRAME_FONT (f) == font)
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;

  get_font_ascent_descent (font, &ascent, &descent);
  FRAME_LINE_HEIGHT (f) = ascent + descent;
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  unit = FRAME_COLUMN_WIDTH (f);
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    FRAME_CONFIG_SCROLL_BAR_COLS (f)
      = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
  else
    FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;

  if (FRAME_HAIKU_WINDOW (f) && !FRAME_TOOLTIP_P (f))
    adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f),
		       3, false, Qfont);

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
haiku_mouse_or_wdesc_frame (void *window, bool accept_tooltip)
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
      if (!w_f || (FRAME_TOOLTIP_P (w_f) && !accept_tooltip))
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

  if (!scroll_bar)
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
haiku_defined_color (struct frame *f, const char *name,
		     Emacs_Color *color, bool alloc, bool make_index)
{
  int rc;

  rc = !haiku_get_color (name, color);

  if (rc && f->gamma && alloc)
    gamma_correct (f, color);

  return rc;
}

/* Adapted from xterm `x_draw_box_rect'.  */
static void
haiku_draw_box_rect (struct glyph_string *s, int left_x, int top_y,
		     int right_x, int bottom_y, int hwidth, int vwidth,
		     bool left_p, bool right_p, struct haiku_rect *clip_rect)
{
  void *view = FRAME_HAIKU_DRAWABLE (s->f);
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
  double h, cs, l;
  uint32_t rgbin;
  struct haiku_output *di;

  if (s->face->use_box_color_for_shadows_p)
    rgbin = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    rgbin = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    rgbin = s->face->background;

  di = FRAME_OUTPUT_DATA (s->f);

  if (s->hl == DRAW_CURSOR)
    rgbin = FRAME_CURSOR_COLOR (s->f).pixel;

  if (di->relief_background != rgbin)
    {
      di->relief_background = rgbin & 0xffffffff;

      rgb_color_hsl (rgbin, &h, &cs, &l);
      hsl_color_rgb (h, cs, fmin (1.0, fmax (0.2, l) * 0.6),
		     &di->black_relief_pixel);
      hsl_color_rgb (h, cs, fmin (1.0, fmax (0.2, l) * 1.2),
		     &di->white_relief_pixel);
    }

  *rgbout_w = di->white_relief_pixel;
  *rgbout_b = di->black_relief_pixel;
}

static void
haiku_draw_relief_rect (struct glyph_string *s, int left_x, int top_y,
			int right_x, int bottom_y, int hwidth, int vwidth,
			bool raised_p, bool top_p, bool bot_p, bool left_p,
			bool right_p, struct haiku_rect *clip_rect)
{
  uint32_t color_white, color_black;
  void *view;

  view = FRAME_HAIKU_DRAWABLE (s->f);
  haiku_calculate_relief_colors (s, &color_white, &color_black);

  BView_SetHighColor (view, raised_p ? color_white : color_black);

  if (clip_rect)
    {
      BView_StartClip (view);
      haiku_clip_to_string (s);
      BView_ClipToRect (view, clip_rect->x, clip_rect->y,
			clip_rect->width, clip_rect->height);
    }

  if (top_p)
    BView_FillRectangle (view, left_x, top_y,
			 right_x - left_x + 1, hwidth);

  if (left_p)
    BView_FillRectangle (view, left_x, top_y,
			 vwidth, bottom_y - top_y + 1);

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

  BView_SetHighColor (view, FRAME_BACKGROUND_PIXEL (s->f));

  /* Omit corner pixels.  */
  if (hwidth > 1 && vwidth > 1)
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
haiku_get_scale_factor (int *scale_x, int *scale_y)
{
  struct haiku_display_info *dpyinfo = x_display_list;

  if (dpyinfo->resx > 96)
    *scale_x = floor (dpyinfo->resx / 96);
  if (dpyinfo->resy > 96)
    *scale_y = floor (dpyinfo->resy / 96);
}

static void
haiku_draw_underwave (struct glyph_string *s, int width, int x)
{
  int wave_height, wave_length;
  int y, dx, dy, odd, xmax, scale_x, scale_y;
  float ax, ay, bx, by;
  void *view;

  scale_x = 1;
  scale_y = 1;
  haiku_get_scale_factor (&scale_x, &scale_y);
  wave_height = 3 * scale_y;
  wave_length = 2 * scale_x;

  dx = wave_length;
  dy = wave_height - 1;
  y = s->ybase - wave_height + 3;
  xmax = x + width;
  view = FRAME_HAIKU_DRAWABLE (s->f);

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

  BView_SetPenSize (view, scale_y);

  while (ax <= xmax)
    {
      BView_StrokeLine (view, ax, ay, bx, by);
      ax = bx, ay = by;
      bx += dx, by = y + 0.5 + odd * dy;
      odd = !odd;
    }

  BView_SetPenSize (view, 1);
  BView_EndClip (view);
}

/* Draw a dashed underline of thickness THICKNESS and width WIDTH onto F
   at a vertical offset of OFFSET from the position of the glyph string
   S, with each segment SEGMENT pixels in length.  */

static void
haiku_draw_dash (struct frame *f, struct glyph_string *s, int width,
		 int segment, int offset, int thickness)
{
  int y_center, which, length, x, doffset;
  void *view;

  /* Configure the thickness of the view's strokes.  */
  view = FRAME_HAIKU_VIEW (s->f);
  BView_SetPenSize (view, thickness);

  /* Offset the origin of the line by half the line width. */
  y_center = s->ybase + offset + thickness / 2;

  /* Remove redundant portions of OFFSET.  */
  doffset = s->x % (segment * 2);

  /* Set which to the phase of the first dash that ought to be drawn and
     length to its length.  */
  which = doffset < segment;
  length = segment - (s->x % segment);

  /* Begin drawing this dash.  */
  for (x = s->x; x < s->x + width; x += length, length = segment)
    {
      if (which)
	BView_StrokeLine (view, x, y_center,
			  min (x + length - 1,
			       s->x + width - 1),
			  y_center);

      which = !which;
    }
}

/* Draw an underline of STYLE onto F at an offset of POSITION from the
   baseline of the glyph string S, S->WIDTH in length, and THICKNESS in
   height.  */

static void
haiku_fill_underline (struct frame *f, struct glyph_string *s,
		      enum face_underline_type style, int position,
		      int thickness)
{
  int segment;
  void *view;

  segment = thickness * 3;
  view = FRAME_HAIKU_VIEW (f);

  switch (style)
    {
      /* FACE_UNDERLINE_DOUBLE_LINE is treated identically to SINGLE, as
	 the second line will be filled by another invocation of this
	 function.  */
    case FACE_UNDERLINE_SINGLE:
    case FACE_UNDERLINE_DOUBLE_LINE:
      BView_FillRectangle (view, s->x, s->ybase + position,
			   s->width, thickness);
      break;

    case FACE_UNDERLINE_DOTS:
      segment = thickness;
      FALLTHROUGH;

    case FACE_UNDERLINE_DASHES:
      haiku_draw_dash (f, s, s->width, segment, position, thickness);
      break;

    case FACE_NO_UNDERLINE:
    case FACE_UNDERLINE_WAVE:
    default:
      emacs_abort ();
    }
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

  void *view = FRAME_HAIKU_DRAWABLE (s->f);

  if (face->underline)
    {
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, cursor_color);
      else if (!face->underline_defaulted_p)
	BView_SetHighColor (view, face->underline_color);
      else
	BView_SetHighColor (view, face->foreground);

      if (face->underline == FACE_UNDERLINE_WAVE)
	haiku_draw_underwave (s, width, x);
      else if (face->underline >= FACE_UNDERLINE_SINGLE)
	{
	  unsigned long thickness, position;

	  if (s->prev
	      && (s->prev->face->underline != FACE_UNDERLINE_WAVE
		  && s->prev->face->underline >= FACE_UNDERLINE_SINGLE)
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
		= (!(NILP (val) || BASE_EQ (val, Qunbound))
		   || s->face->underline_at_descent_line_p);

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_use_underline_position_properties, s->w));
	      use_underline_position_properties
		= !(NILP (val) || BASE_EQ (val, Qunbound));

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

	  haiku_fill_underline (s->f, s, s->face->underline,
				position, thickness);

	  /* Place a second underline above the first if this was
	     requested in the face specification.  */

	  if (s->face->underline == FACE_UNDERLINE_DOUBLE_LINE)
	    {
	      /* Compute the position of the second underline.  */
	      position = position - thickness - 1;
	      haiku_fill_underline (s->f, s, s->face->underline,
				    position, thickness);
	    }
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
			    NULL);
}

static void
haiku_draw_plain_background (struct glyph_string *s, struct face *face,
			     int x, int y, int width, int height)
{
  void *view = FRAME_HAIKU_DRAWABLE (s->f);
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

  BView_FillRectangle (view, x, y, width, height);
}

static struct haiku_bitmap_record *
haiku_get_bitmap_rec (struct frame *f, ptrdiff_t id)
{
  return &FRAME_DISPLAY_INFO (f)->bitmaps[id - 1];
}

static void
haiku_update_bitmap_rec (struct haiku_bitmap_record *rec,
			 uint32_t new_foreground,
			 uint32_t new_background)
{
  char *bits;
  int x, y, bytes_per_line;

  if (new_foreground == rec->stipple_foreground
      && new_background == rec->stipple_background)
    return;

  bits = rec->stipple_bits;
  bytes_per_line = (rec->width + 7) / 8;

  for (y = 0; y < rec->height; y++)
    {
      for (x = 0; x < rec->width; x++)
	haiku_put_pixel (rec->img, x, y,
			 ((bits[x / 8] >> (x % 8)) & 1
			  ? new_foreground : new_background));

      bits += bytes_per_line;
    }

  rec->stipple_foreground = new_foreground;
  rec->stipple_background = new_background;
}

static void
haiku_draw_stipple_background (struct glyph_string *s, struct face *face,
			       int x, int y, int width, int height,
			       bool explicit_colors_p,
			       uint32 explicit_background,
			       uint32 explicit_foreground)
{
  struct haiku_bitmap_record *rec;
  unsigned long foreground, background;
  void *view;

  view = FRAME_HAIKU_DRAWABLE (s->f);
  rec = haiku_get_bitmap_rec (s->f, s->face->stipple);

  if (explicit_colors_p)
    {
      background = explicit_background;
      foreground = explicit_foreground;
    }
  else if (s->hl == DRAW_CURSOR)
    haiku_merge_cursor_foreground (s, &foreground, &background);
  else
    {
      foreground = s->face->foreground;
      background = s->face->background;
    }

  haiku_update_bitmap_rec (rec, foreground, background);

  BView_StartClip (view);
  haiku_clip_to_string (s);
  BView_ClipToRect (view, x, y, width, height);
  BView_DrawBitmapTiled (view, rec->img, 0, 0, -1, -1,
			 0, 0, x + width, y + height);
  BView_EndClip (view);
}

void
haiku_draw_background_rect (struct glyph_string *s, struct face *face,
			    int x, int y, int width, int height)
{
  if (!s->stippled_p)
    haiku_draw_plain_background (s, face, x, y, width, height);
  else
    haiku_draw_stipple_background (s, face, x, y, width, height,
				   false, 0, 0);
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
	  haiku_draw_background_rect (s, s->face, s->x, s->y + box_line_width,
				      s->background_width,
				      s->height - 2 * box_line_width);

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

  void *view = FRAME_HAIKU_DRAWABLE (s->f);

  if (s->font_not_found_p)
    {
      if (s->hl == DRAW_CURSOR)
	BView_SetHighColor (view, FRAME_OUTPUT_DATA (s->f)->cursor_fg);
      else
	BView_SetHighColor (view, face->foreground);
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;

	  BView_SetPenSize (view, 1);
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
  static unsigned char2b[8];
  int x, i, j;
  struct face *face = s->face;
  unsigned long color;

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
	      if (CONSP (acronym))
		acronym = XCAR (acronym);
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
	{
	  if (s->hl == DRAW_CURSOR)
	    haiku_merge_cursor_foreground (s, NULL, &color);
	  else
	    color = s->face->foreground;

	  BView_SetHighColor (FRAME_HAIKU_DRAWABLE (s->f), color);
	  BView_SetPenSize (FRAME_HAIKU_DRAWABLE (s->f), 1);
	  BView_StrokeRectangle (FRAME_HAIKU_DRAWABLE (s->f),
				 x, s->ybase - glyph->ascent,
				 glyph->pixel_width,
				 glyph->ascent + glyph->descent);
	}
      x += glyph->pixel_width;
   }
}

static void
haiku_draw_stretch_glyph_string (struct glyph_string *s)
{
  struct face *face = s->face;
  uint32_t bkg;

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

      void *view = FRAME_HAIKU_DRAWABLE (s->f);
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

	  /* Draw stipples manually because we want the background
	     part of a stretch glyph to have a stipple even if the
	     cursor is visible on top.  */
	  if (!face->stipple)
	    {
	      if (s->row->mouse_face_p && cursor_in_mouse_face_p (s->w))
		haiku_mouse_face_colors (s, NULL, &bkg);
	      else
		bkg = face->background;

	      BView_SetHighColor (view, bkg);
	      BView_FillRectangle (view, x, y, w, h);
	    }
	  else
	    {
	      if (s->row->mouse_face_p && cursor_in_mouse_face_p (s->w))
		haiku_mouse_face_colors (s, NULL, &bkg);
	      else
		bkg = face->background;

	      haiku_draw_stipple_background (s, s->face, x, y, w, h,
					     true, bkg, face->foreground);
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
	haiku_draw_background_rect (s, s->face, x, s->y,
				    background_width, s->height);
    }
  s->background_filled_p = 1;
}

static void
haiku_start_clip (struct glyph_string *s)
{
  void *view = FRAME_HAIKU_DRAWABLE (s->f);
  BView_StartClip (view);
}

static void
haiku_end_clip (struct glyph_string *s)
{
  void *view = FRAME_HAIKU_DRAWABLE (s->f);
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

  BView_ClipToRect (FRAME_HAIKU_DRAWABLE (f), x, y, width, height);
}

static void
haiku_update_begin (struct frame *f)
{
  /* Mark the frame as incomplete so it is not flushed upon handling
     input.  */
  FRAME_COMPLETE_P (f) = false;
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
  void *view = FRAME_HAIKU_DRAWABLE (s->f);
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

      BView_SetPenSize (view, 1);
      BView_StrokeRectangle (view, s->x, s->y,
			     s->width, s->height);
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
			  top_p, bot_p, left_p, right_p, &r);
}

static void
haiku_translate_transform (double (*transform)[3], double dx,
			   double dy)
{
  transform[0][2] += dx;
  transform[1][2] += dy;
}

static void
haiku_draw_image_glyph_string (struct glyph_string *s)
{
  struct face *face = s->face;
  void *view, *bitmap, *mask;
  int box_line_hwidth = max (face->box_vertical_line_width, 0);
  int box_line_vwidth = max (face->box_horizontal_line_width, 0);
  int x, y, height, width, relief;
  struct haiku_rect nr;
  Emacs_Rectangle cr, ir, r;
  unsigned long background;
  double image_transform[3][3];

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

  view = FRAME_HAIKU_DRAWABLE (s->f);
  bitmap = s->img->pixmap;

  s->stippled_p = face->stipple != 0;

  if (s->hl == DRAW_CURSOR)
    haiku_merge_cursor_foreground (s, NULL, &background);
  else
    background = face->background;

  haiku_draw_background_rect (s, face, x, y,
			      width, height);

  if (bitmap)
    {
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

      mask = s->img->mask;

      if (gui_intersect_rectangles (&cr, &ir, &r))
	{
	  memcpy (&image_transform, &s->img->transform,
		  sizeof image_transform);

	  if (s->slice.x != x || s->slice.y != y
	      || s->slice.width != s->img->width
	      || s->slice.height != s->img->height)
	    {
	      BView_StartClip (view);
	      BView_ClipToRect (view, r.x, r.y, r.width, r.height);
	    }

	  haiku_translate_transform (image_transform,
				     x - s->slice.x,
				     y - s->slice.y);

	  be_apply_affine_transform (view,
				     image_transform[0][0],
				     image_transform[0][1],
				     image_transform[0][2],
				     image_transform[1][0],
				     image_transform[1][1],
				     image_transform[1][2]);

	  if (!s->stippled_p || !mask)
	    {
	      BView_DrawBitmap (view, bitmap, 0, 0,
				s->img->original_width,
				s->img->original_height,
				0, 0,
				s->img->original_width,
				s->img->original_height,
				s->img->use_bilinear_filtering);

	      if (mask)
		be_draw_image_mask (mask, view, 0, 0,
				    s->img->original_width,
				    s->img->original_height,
				    0, 0,
				    s->img->original_width,
				    s->img->original_height,
				    background);
	    }
	  else
	    /* In order to make sure the stipple background remains
	       visible, use the mask for the alpha channel of BITMAP
	       and composite it onto the view instead.  */
	    be_draw_bitmap_with_mask (view, bitmap, mask, 0, 0,
				      s->img->original_width,
				      s->img->original_height,
				      0, 0,
				      s->img->original_width,
				      s->img->original_height,
				      s->img->use_bilinear_filtering);

	  if (s->slice.x != x || s->slice.y != y
	      || s->slice.width != s->img->width
	      || s->slice.height != s->img->height)
	    BView_EndClip (view);

	  be_apply_affine_transform (view, 1, 0, 0, 0, 1, 0);
	}

      if (!s->img->mask)
	{
	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */

	  if (s->hl == DRAW_CURSOR)
	    {
	      relief = eabs (s->img->relief);

	      BView_SetPenSize (view, 1);
	      BView_SetHighColor (view, FRAME_CURSOR_COLOR (s->f).pixel);
	      BView_StrokeRectangle (view, x - relief, y - relief,
				     s->slice.width + relief * 2,
				     s->slice.height + relief * 2);
	    }
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
  void *view = FRAME_HAIKU_DRAWABLE (s->f);;
  struct face *face = s->face;

  block_input ();
  BView_draw_lock (view, false, 0, 0, 0, 0);
  prepare_face_for_display (s->f, s->face);

  s->stippled_p = s->hl != DRAW_CURSOR && face->stipple;

  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
          {
	    prepare_face_for_display (s->f, next->face);
	    next->stippled_p
	      = next->hl != DRAW_CURSOR && next->face->stipple;

	    haiku_start_clip (next);
	    haiku_clip_to_string (next);
            if (next->first_glyph->type != STRETCH_GLYPH)
	      haiku_maybe_draw_background (next, true);
            else
	      haiku_draw_stretch_glyph_string (next);
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

  /* Set the stipple_p flag indicating whether or not a stipple was
     drawn in s->row.  That is the case either when s is a stretch
     glyph string and s->face->stipple is not NULL, or when
     s->face->stipple exists and s->hl is not DRAW_CURSOR.  */
  if (s->face->stipple
      && (s->first_glyph->type == STRETCH_GLYPH
	  || s->hl != DRAW_CURSOR))
    s->row->stipple_p = true;

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
	  void *view = FRAME_HAIKU_DRAWABLE (f);
	  BView_draw_lock (view, false, 0, 0, 0, 0);
	  BView_StartClip (view);
	  BView_SetHighColor (view, (face->background_defaulted_p
				     ? FRAME_BACKGROUND_PIXEL (f)
				     : face->background));
	  BView_FillRectangle (view, 0, y, width, height);
	  BView_FillRectangle (view, FRAME_PIXEL_WIDTH (f) - width,
			       y, width, height);
	  BView_invalidate_region (FRAME_HAIKU_DRAWABLE (f),
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
      /* And if the configury during frame creation has been
	 completed.  Otherwise, there will be no valid "old size" to
	 go back to.  */
      && FRAME_OUTPUT_DATA (f)->configury_done)
    return;

  haiku_update_size_hints (f);

  if (FRAME_HAIKU_WINDOW (f))
    {
      block_input ();
      BWindow_resize (FRAME_HAIKU_WINDOW (f),
		      width, height);

      if (FRAME_VISIBLE_P (f)
	  && (width != FRAME_PIXEL_WIDTH (f)
	      || height != FRAME_PIXEL_HEIGHT (f)))
	haiku_wait_for_event (f, FRAME_RESIZED);
      unblock_input ();
    }

  do_pending_window_change (false);
}

static void
haiku_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f;
  int x, y, wd, h;
  struct glyph *cursor_glyph;
  uint32_t foreground;
  void *view;

  f = XFRAME (WINDOW_FRAME (w));
  view = FRAME_HAIKU_DRAWABLE (f);

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  foreground = FRAME_CURSOR_COLOR (f).pixel;

  /* When on R2L character, show cursor at the right edge of the
     glyph, unless the cursor box is as wide as the glyph or wider
     (the latter happens when x-stretch-cursor is non-nil).  */
  if ((cursor_glyph->resolved_level & 1) != 0
      && cursor_glyph->pixel_width > wd)
    x += cursor_glyph->pixel_width - wd;

  /* Set clipping, draw the rectangle, and reset clipping again.
     This also marks the region as invalidated.  */

  BView_draw_lock (view, true, x, y, wd, h);
  BView_StartClip (view);
  haiku_clip_to_row (w, row, TEXT_AREA);

  /* Now set the foreground color and pen size.  */
  BView_SetHighColor (view, foreground);
  BView_SetPenSize (view, 1);

  /* Actually draw the rectangle.  */
  BView_StrokeRectangle (view, x, y, wd, h);

  /* Reset clipping.  */
  BView_EndClip (view);
  BView_draw_unlock (view);
}

static void
haiku_draw_bar_cursor (struct window *w, struct glyph_row *row,
		       int width, enum text_cursor_kinds kind)
{
  struct frame *f;
  struct glyph *cursor_glyph;
  struct glyph_row *r;
  struct face *face;
  uint32_t foreground;
  void *view;
  int x, y, dummy_x, dummy_y, dummy_h;

  f = XFRAME (w->frame);

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      r = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, r, DRAW_CURSOR);
    }
  else
    {
      view = FRAME_HAIKU_DRAWABLE (f);
      face = FACE_FROM_ID (f, cursor_glyph->face_id);

      /* If the glyph's background equals the color we normally draw
	 the bars cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == FRAME_CURSOR_COLOR (f).pixel)
	foreground = face->foreground;
      else
	foreground = FRAME_CURSOR_COLOR (f).pixel;

      BView_draw_lock (view, false, 0, 0, 0, 0);
      BView_StartClip (view);
      BView_SetHighColor (view, foreground);
      haiku_clip_to_row (w, row, TEXT_AREA);

      if (kind == BAR_CURSOR)
	{
	  x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
	  y = WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y);

	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  BView_FillRectangle (view, x, y, width, row->height);
	  BView_invalidate_region (view, x, y, width, row->height);
	}
      else /* HBAR_CURSOR */
	{
	  x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
	  y = WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
					   row->height - width);

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);

	  if ((cursor_glyph->resolved_level & 1) != 0
	      && cursor_glyph->pixel_width > w->phys_cursor_width - 1)
	    x += cursor_glyph->pixel_width - w->phys_cursor_width + 1;

	  BView_FillRectangle (view, x, y, w->phys_cursor_width - 1,
			       width);
	  BView_invalidate_region (view, x, y, w->phys_cursor_width - 1,
				   width);
	}

      BView_EndClip (view);
      BView_draw_unlock (view);
    }
}

static void
haiku_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
			  int x, int y, enum text_cursor_kinds cursor_type,
			  int cursor_width, bool on_p, bool active_p)
{
  if (on_p)
    {
      w->phys_cursor_type = cursor_type;
      w->phys_cursor_on_p = true;

      if (glyph_row->exact_window_width_line_p
	  && (glyph_row->reversed_p
	      ? (w->phys_cursor.hpos < 0)
	      : (w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])))
	{
	  glyph_row->cursor_in_fringe_p = true;
	  draw_fringe_bitmap (w, glyph_row, glyph_row->reversed_p);
	}
      else
	{
	  switch (cursor_type)
	    {
	    case HOLLOW_BOX_CURSOR:
	      haiku_draw_hollow_cursor (w, glyph_row);
	      break;

	    case FILLED_BOX_CURSOR:
	      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	      break;

	    case BAR_CURSOR:
	      haiku_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	      break;

	    case HBAR_CURSOR:
	      haiku_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	      break;

	    case NO_CURSOR:
	      w->phys_cursor_width = 0;
	      break;

	    default:
	      emacs_abort ();
	    }
	}
    }
}

static void
haiku_show_hourglass (struct frame *f)
{
  if (FRAME_TOOLTIP_P (f)
      || FRAME_OUTPUT_DATA (f)->hourglass_p)
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
  if (FRAME_TOOLTIP_P (f)
      || !FRAME_OUTPUT_DATA (f)->hourglass_p)
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
  void *view = FRAME_HAIKU_DRAWABLE (f);
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
  int unit, size;

  unit = FRAME_COLUMN_WIDTH (f);
  size = BScrollBar_default_size (0) + 1;

  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = size;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (size + unit - 1) / unit;
}

static void
haiku_set_scroll_bar_default_height (struct frame *f)
{
  int height, size;

  height = FRAME_LINE_HEIGHT (f);
  size = BScrollBar_default_size (true) + 1;

  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = size;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (size + height - 1) / height;
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
  void *view = FRAME_HAIKU_DRAWABLE (f);

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
  struct frame *f;
  Lisp_Object barobj;
  struct scroll_bar *bar;
  void *scroll_bar;
  void *view;

  f = XFRAME (WINDOW_FRAME (w));
  view = FRAME_HAIKU_DRAWABLE (f);

  block_input ();
  bar = ALLOCATE_PSEUDOVECTOR (struct scroll_bar, prev, PVEC_OTHER);

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

  scroll_bar = be_make_scroll_bar_for_view (view, horizontal_p,
					    left, top, left + width - 1,
					    top + height - 1);
  BView_publish_scroll_bar (view, left, top, width, height);

  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  bar->scroll_bar = scroll_bar;
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
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_x, window_width;
  void *view;

  eassert (WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w));
  /* Get window dimensions.  */
  window_box (w, ANY_AREA, &window_x, 0, &window_width, 0);
  left = window_x;
  width = window_width;
  top = WINDOW_SCROLL_BAR_AREA_Y (w);
  height = WINDOW_CONFIG_SCROLL_BAR_HEIGHT (w);
  view = FRAME_HAIKU_DRAWABLE (WINDOW_XFRAME (w));

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

      if (bar->left != left || bar->top != top
	  || bar->width != width || bar->height != height)
	{
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
haiku_set_vertical_scroll_bar (struct window *w, int portion, int whole, int position)
{
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_y, window_height;
  void *view;

  eassert (WINDOW_HAS_VERTICAL_SCROLL_BAR (w));

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;

  /* Compute the left edge and the width of the scroll bar area.  */
  left = WINDOW_SCROLL_BAR_AREA_X (w);
  width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);

  view = FRAME_HAIKU_DRAWABLE (WINDOW_XFRAME (w));

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

      if (bar->left != left || bar->top != top
	  || bar->width != width || bar->height != height)
	{
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
  struct face *face;
  struct frame *f;
  struct haiku_bitmap_record *rec;
  void *view, *bitmap;
  uint32 col;

  f = XFRAME (WINDOW_FRAME (w));
  view = FRAME_HAIKU_DRAWABLE (f);
  face = p->face;

  block_input ();
  BView_draw_lock (view, true, 0, 0, 0, 0);
  BView_StartClip (view);

  if (p->wd && p->h)
    BView_invalidate_region (view, p->x, p->y, p->wd, p->h);

  haiku_clip_to_row (w, row, ANY_AREA);

  if (p->bx >= 0 && !p->overlay_p)
    {
      BView_invalidate_region (view, p->bx, p->by, p->nx, p->ny);

      if (!face->stipple)
	{
	  BView_SetHighColor (view, face->background);
	  BView_FillRectangle (view, p->bx, p->by, p->nx, p->ny);
	}
      else
	{
	  rec = haiku_get_bitmap_rec (f, face->stipple);
	  haiku_update_bitmap_rec (rec, face->foreground,
				   face->background);

	  BView_StartClip (view);
	  haiku_clip_to_row (w, row, ANY_AREA);
	  BView_ClipToRect (view, p->bx, p->by, p->nx, p->ny);
	  BView_DrawBitmapTiled (view, rec->img, 0, 0, -1, -1,
				 0, 0, FRAME_PIXEL_WIDTH (f),
				 FRAME_PIXEL_HEIGHT (f));
	  BView_EndClip (view);

	  row->stipple_p = true;
	}
    }

  if (p->which
      && p->which < max_fringe_bmp
      && p->which < max_used_fringe_bitmap)
    {
      bitmap = fringe_bmps[p->which];

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
  void *view = FRAME_HAIKU_DRAWABLE (f);
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
	  *part = scroll_bar_nowhere;

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

  /* The frame is complete again as its contents were just
     flushed.  */
  FRAME_COMPLETE_P (f) = true;

  if (FRAME_VISIBLE_P (f) && !FRAME_TOOLTIP_P (f))
    BWindow_Flush (FRAME_HAIKU_WINDOW (f));
}

static void
haiku_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  if (FRAME_TOOLTIP_P (f))
    return;

  block_input ();
  if (!f->pointer_invisible && FRAME_HAIKU_VIEW (f)
      && !FRAME_OUTPUT_DATA (f)->hourglass_p)
    BView_set_view_cursor (FRAME_HAIKU_VIEW (f), cursor);
  unblock_input ();
  FRAME_OUTPUT_DATA (f)->current_cursor = cursor;
}

static void
haiku_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct haiku_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param = gui_display_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
                                                RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (BASE_EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font_param))
    /* System font should take precedence over X resources.  We
       suggest this regardless of font-use-system-font because .emacs
       may not have been read yet.  Returning a font-spec is Haiku
       specific behavior.  */
    font = font_open_by_spec (f, Ffont_get_system_font ());

  if (NILP (font))
    font = (!NILP (font_param)
	    ? font_param
	    : gui_display_get_arg (dpyinfo, parms, Qfont,
				   "font", "Font",
				   RES_TYPE_STRING));

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
    NULL, /* update_window_begin */
    NULL, /* update_window_end */
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
    NULL, /* shift glyphs for insert */
    haiku_show_hourglass,
    haiku_hide_hourglass,
    haiku_default_font_parameter,
  };

static void
haiku_make_fullscreen_consistent (struct frame *f)
{
  Lisp_Object lval;
  struct haiku_output *output;

  output = FRAME_OUTPUT_DATA (f);

  if (output->fullscreen_mode == FULLSCREEN_MODE_BOTH)
    lval = Qfullboth;
  else if (output->fullscreen_mode == FULLSCREEN_MODE_WIDTH)
    lval = Qfullwidth;
  else if (output->fullscreen_mode == FULLSCREEN_MODE_HEIGHT)
    lval = Qfullheight;
  else if (output->fullscreen_mode == FULLSCREEN_MODE_MAXIMIZED)
    lval = Qmaximized;
  else
    lval = Qnil;

  store_frame_param (f, Qfullscreen, lval);
}

static void
haiku_flush_dirty_back_buffer_on (struct frame *f)
{
  if (FRAME_GARBAGED_P (f)
      || buffer_flipping_blocked_p ()
      /* If the frame is not already up to date, do not flush buffers
	 on input, as that will result in flicker.  */
      || !FRAME_COMPLETE_P (f)
      || !FRAME_DIRTY_P (f))
    return;

  haiku_flip_buffers (f);
}

/* N.B. that support for TYPE must be explicitly added to
   haiku_read_socket.  */
void
haiku_wait_for_event (struct frame *f, int type)
{
  int input_blocked_to;
  object_wait_info info;
  specpdl_ref depth;

  input_blocked_to = interrupt_input_blocked;
  info.object = port_application_to_emacs;
  info.type = B_OBJECT_TYPE_PORT;
  info.events = B_EVENT_READ;

  depth = SPECPDL_INDEX ();
  specbind (Qinhibit_quit, Qt);

  FRAME_OUTPUT_DATA (f)->wait_for_event_type = type;

  while (FRAME_OUTPUT_DATA (f)->wait_for_event_type == type)
    {
      if (wait_for_objects (&info, 1) < B_OK)
	continue;

      pending_signals = true;
      /* This will call the read_socket_hook.  */
      totally_unblock_input ();
      interrupt_input_blocked = input_blocked_to;
      info.events = B_EVENT_READ;
    }

  unbind_to (depth, Qnil);
}

static int
haiku_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  int message_count;
  void *buf;
  ssize_t b_size;
  int button_or_motion_p, do_help;
  enum haiku_event_type type;
  struct input_event inev, inev2;
  struct frame *mouse_frame;

  message_count = 0;
  button_or_motion_p = 0;
  do_help = 0;

  buf = alloca (200);

  block_input ();
  haiku_read_size (&b_size, false);
  while (b_size >= 0)
    {
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

	    int width = lrint (b->width);
	    int height = lrint (b->height);

	    if (FRAME_OUTPUT_DATA (f)->wait_for_event_type
		== FRAME_RESIZED)
	      FRAME_OUTPUT_DATA (f)->wait_for_event_type = -1;

	    if (FRAME_TOOLTIP_P (f))
	      {
		if (FRAME_PIXEL_WIDTH (f) != width
		    || FRAME_PIXEL_HEIGHT (f) != height)
		  SET_FRAME_GARBAGED (f);

		FRAME_PIXEL_WIDTH (f) = width;
		FRAME_PIXEL_HEIGHT (f) = height;

		haiku_clear_under_internal_border (f);

		/* Flush the frame and flip buffers here.  It is
		   necessary for tooltips displayed inside menus, as
		   redisplay cannot happen.  */
		haiku_flush (f);
		continue;
	      }

	    BView_draw_lock (FRAME_HAIKU_DRAWABLE (f), false, 0, 0, 0, 0);
	    BView_resize_to (FRAME_HAIKU_DRAWABLE (f), width, height);
	    BView_draw_unlock (FRAME_HAIKU_DRAWABLE (f));

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

	    if (!f)
	      continue;

	    /* If mouse-highlight is an integer, input clears out
	       mouse highlighting.  */
	    if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
		&& (f == NULL
		    || (!EQ (f->tool_bar_window, hlinfo->mouse_face_window)
			&& !EQ (f->tab_bar_window, hlinfo->mouse_face_window))))
	      {
		mouse_frame = hlinfo->mouse_face_mouse_frame;

		clear_mouse_face (hlinfo);
		hlinfo->mouse_face_hidden = true;

		if (mouse_frame)
		  haiku_flush_dirty_back_buffer_on (mouse_frame);
	      }

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

	    if ((x_display_list->focus_event_frame != f && b->activated_p)
		|| (x_display_list->focus_event_frame == f && !b->activated_p))
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
	    struct frame *f = haiku_mouse_or_wdesc_frame (b->window, true);
	    Mouse_HLInfo *hlinfo = &x_display_list->mouse_highlight;
	    Lisp_Object frame;

	    if (!f)
	      continue;

	    if (FRAME_TOOLTIP_P (f))
	      {
		/* Dismiss the tooltip if the mouse moves onto a
		   tooltip frame (except when drag-and-drop is in
		   progress and we are trying to move the tooltip
		   along with the mouse pointer).  FIXME: for some
		   reason we don't get leave notification events for
		   this.  */

		if (any_help_event_p
		    && !(be_drag_and_drop_in_progress ()
			 && haiku_dnd_follow_tooltip)
		    && !((EQ (track_mouse, Qdrag_source)
			  || EQ (track_mouse, Qdropping))
			 && gui_mouse_grabbed (x_display_list)))
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
		haiku_flush_dirty_back_buffer_on (f);
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

		    haiku_flush_dirty_back_buffer_on (f);
		  }

		if (f == x_display_list->last_mouse_glyph_frame)
		  x_display_list->last_mouse_glyph_frame = NULL;

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

		if (any_help_event_p
		    && !((EQ (track_mouse, Qdrag_source)
			  || EQ (track_mouse, Qdropping))
			 && gui_mouse_grabbed (x_display_list)))
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

		/* A crossing event might be sent out-of-order with
		   regard to motion events from other windows, such as
		   when the mouse pointer rapidly moves from an
		   undecorated child frame to its parent.  This can
		   cause a failure to clear the mouse face on the
		   former if an event for the latter is read by Emacs
		   first and ends up showing the mouse face there.

		   Work around the problem by clearing the mouse face
		   now if it is currently shown on a different
		   frame.  */

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
		    note_mouse_highlight (f, b->x, b->y);
		    remember_mouse_glyph (f, b->x, b->y,
					  &FRAME_DISPLAY_INFO (f)->last_mouse_glyph);
		    dpyinfo->last_mouse_glyph_frame = f;
		  }
		else
		  help_echo_string = previous_help_echo_string;

		if (!NILP (Vmouse_autoselect_window))
		  {
		    Lisp_Object window = window_from_coordinates (f, b->x, b->y, 0, 0, 0, 0);

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

		    if (any_help_event_p || do_help)
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
	      haiku_flush_dirty_back_buffer_on (f);
	    break;
	  }
	case BUTTON_UP:
	case BUTTON_DOWN:
	  {
	    struct haiku_button_event *b = buf;
	    struct frame *f = haiku_mouse_or_wdesc_frame (b->window, false);
	    Lisp_Object tab_bar_arg = Qnil;
	    int tab_bar_p = 0, tool_bar_p = 0;
	    bool up_okay_p = false;
	    struct scroll_bar *bar;

	    if (popup_activated_p || !f)
	      continue;

	    inev.modifiers = haiku_modifiers_to_emacs (b->modifiers);
	    bar = haiku_scroll_bar_from_widget (b->scroll_bar, b->window);

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

		window = window_from_coordinates (f, x, y, 0, true, true, true);
		tab_bar_p = EQ (window, f->tab_bar_window);

		if (tab_bar_p)
		  {
		    tab_bar_arg = handle_tab_bar_click
		      (f, x, y, type == BUTTON_DOWN, inev.modifiers);
		    haiku_flush_dirty_back_buffer_on (f);
		  }
	      }

	    if (WINDOWP (f->tool_bar_window)
		&& WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
	      {
		Lisp_Object window;
		int x = b->x;
		int y = b->y;

		window = window_from_coordinates (f, x, y, 0, true, true, true);
		tool_bar_p = (EQ (window, f->tool_bar_window)
			      && (type != BUTTON_UP
				  || f->last_tool_bar_item != -1));

		if (tool_bar_p)
		  {
		    handle_tool_bar_click
		      (f, x, y, type == BUTTON_DOWN, inev.modifiers);
		    haiku_flush_dirty_back_buffer_on (f);
		  }
	      }

	    if (type == BUTTON_UP)
	      {
		inev.modifiers |= up_modifier;
		up_okay_p = (x_display_list->grabbed & (1 << b->btn_no));
		x_display_list->grabbed &= ~(1 << b->btn_no);
	      }
	    else
	      {
		up_okay_p = true;
		inev.modifiers |= down_modifier;
		x_display_list->last_mouse_frame = f;
		x_display_list->grabbed |= (1 << b->btn_no);
		if (f && !tab_bar_p)
		  f->last_tab_bar_item = -1;
		if (f && !tool_bar_p)
		  f->last_tool_bar_item = -1;
	      }

	    if (bar)
	      {
		inev.kind = (bar->horizontal
			     ? HORIZONTAL_SCROLL_BAR_CLICK_EVENT
			     : SCROLL_BAR_CLICK_EVENT);
		inev.part = (bar->horizontal
			     ? scroll_bar_horizontal_handle
			     : scroll_bar_handle);
	      }
	    else if (up_okay_p
		     && !(tab_bar_p && NILP (tab_bar_arg))
		     && !tool_bar_p)
	      inev.kind = MOUSE_CLICK_EVENT;

	    inev.arg = tab_bar_arg;
	    inev.code = b->btn_no;

	    f->mouse_moved = false;

	    if (bar)
	      {
		if (bar->horizontal)
		  {
		    XSETINT (inev.x, min (max (0, b->x - bar->left),
					  bar->width));
		    XSETINT (inev.y, bar->width);
		  }
		else
		  {
		    XSETINT (inev.x, min (max (0, b->y - bar->top),
					  bar->height));
		    XSETINT (inev.y, bar->height);
		  }

		inev.frame_or_window = bar->window;
	      }
	    else
	      {
		XSETINT (inev.x, b->x);
		XSETINT (inev.y, b->y);
		XSETFRAME (inev.frame_or_window, f);
	      }

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
	    int top, left;
	    struct frame *p;

	    if (!f)
	      continue;

	    FRAME_OUTPUT_DATA (f)->frame_x = b->x;
	    FRAME_OUTPUT_DATA (f)->frame_y = b->y;

	    if (FRAME_PARENT_FRAME (f))
	      haiku_coords_from_parent (f, &b->x, &b->y);

	    left = b->x - b->decorator_width;
	    top = b->y - b->decorator_height;

	    if (left != f->left_pos || top != f->top_pos)
	      {
		inev.kind = MOVE_FRAME_EVENT;

		XSETINT (inev.x, left);
		XSETINT (inev.y, top);

		f->left_pos = left;
		f->top_pos = top;

		p = FRAME_PARENT_FRAME (f);

		if (p)
		  EmacsWindow_move_weak_child (FRAME_HAIKU_WINDOW (p),
					       b->window, left, top);

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
		inev.part = (bar->horizontal
			     ? scroll_bar_horizontal_handle
			     : scroll_bar_handle);

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

	    wheel_window = window_from_coordinates (f, x, y, 0, false, false, false);

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

		if (be_drag_and_drop_in_progress ())
		  haiku_note_drag_wheel (&inev);
	      }

	    break;
	  }
	case MENU_BAR_RESIZE:
	  {
	    struct haiku_menu_bar_resize_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f))
	      continue;

	    if (FRAME_OUTPUT_DATA (f)->wait_for_event_type
		== MENU_BAR_RESIZE)
	      FRAME_OUTPUT_DATA (f)->wait_for_event_type = -1;

	    int old_height = FRAME_MENU_BAR_HEIGHT (f);

	    FRAME_MENU_BAR_HEIGHT (f) = b->height;
	    FRAME_MENU_BAR_LINES (f)
	      = (b->height + FRAME_LINE_HEIGHT (f)) / FRAME_LINE_HEIGHT (f);

	    if (old_height != b->height)
	      {
		adjust_frame_size (f, -1, -1, 3, true, Qmenu_bar_lines);
		haiku_clear_under_internal_border (f);
	      }
	    break;
	  }
	case MENU_BAR_CLICK:
	  {
	    struct haiku_menu_bar_click_event *b = buf;
	    struct frame *f = haiku_window_to_frame (b->window);

	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f))
	      continue;

	    if (!FRAME_OUTPUT_DATA (f)->saved_menu_event)
	      FRAME_OUTPUT_DATA (f)->saved_menu_event = xmalloc (sizeof *b);
	    *FRAME_OUTPUT_DATA (f)->saved_menu_event = *b;
	    inev.kind = MENU_BAR_ACTIVATE_EVENT;
	    XSETFRAME (inev.frame_or_window, f);
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
		FRAME_OUTPUT_DATA (f)->menu_bar_open_p = 1;
		popup_activated_p += 1;
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

	    find_and_call_menu_selection (f, f->menu_bar_items_used,
					  f->menu_bar_vector, b->ptr);
	    break;
	  }
	case MENU_BAR_HELP_EVENT:
	  {
	    struct haiku_menu_bar_help_event *b = buf;

	    if (!popup_activated_p)
	      continue;

	    struct frame *f = haiku_window_to_frame (b->window);
	    if (!f || !FRAME_EXTERNAL_MENU_BAR (f)
		|| !FRAME_OUTPUT_DATA (f)->menu_bar_open_p)
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

	    if (b->fullscreen_mode == FULLSCREEN_MODE_MAXIMIZED)
	      f->want_fullscreen = FULLSCREEN_NONE;
	    else
	      f->want_fullscreen = FULLSCREEN_MAXIMIZED;

	    FRAME_TERMINAL (f)->fullscreen_hook (f);
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
	case SCREEN_CHANGED_EVENT:
	  {
	    struct haiku_screen_changed_event *b = buf;

	    inev.kind = MONITORS_CHANGED_EVENT;
	    XSETTERMINAL (inev.arg, x_display_list->terminal);
	    inev.timestamp = b->when / 1000;
	    break;
	  }
	case CLIPBOARD_CHANGED_EVENT:
	  be_handle_clipboard_changed_message ();
	  break;
	case APP_QUIT_REQUESTED_EVENT:
	  inev.kind = SAVE_SESSION_EVENT;
	  inev.arg = Qt;
	  break;
	case FONT_CHANGE_EVENT:
	  /* This generates CONFIG_CHANGED_EVENTs, which are then
	     handled in Lisp.  */
	  haiku_handle_font_change_event (buf, &inev);
	  break;

	case NOTIFICATION_CLICK_EVENT:
	  /* This code doesn't function, but the why is unknown.  */
#if 0
	  {
	    struct haiku_notification_click_event *b = buf;

	    inev.kind = NOTIFICATION_CLICKED_EVENT;
	    inev.arg  = make_int (b->id);
	    break;
	  }
#endif /* 0 */

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

  unblock_input ();

  return message_count;
}

static Lisp_Object
haiku_get_focus_frame (struct frame *f)
{
  Lisp_Object lisp_focus;
  struct frame *focus;

  focus = FRAME_DISPLAY_INFO (f)->focused_frame;

  if (!focus)
    return Qnil;

  XSETFRAME (lisp_focus, focus);
  return lisp_focus;
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
  void *view = FRAME_HAIKU_DRAWABLE (f);
  object_wait_info info;
  bigtime_t wakeup;

  info.object = port_application_to_emacs;
  info.type = B_OBJECT_TYPE_PORT;
  info.events = B_EVENT_READ;
  wakeup = system_time () + 150000;

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
			 - FRAME_INTERNAL_BORDER_WIDTH (f)
			 - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
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
      /* Break if result would not be positive.  */
      if (wakeup < system_time ())
	break;

      /* Try to wait that long--but we might wake up sooner.  */
      wait_for_objects_etc (&info, 1, B_ABSOLUTE_TIMEOUT, wakeup);

      if (info.events & B_EVENT_READ)
	break;

      info.events = B_EVENT_READ;
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
			 - FRAME_INTERNAL_BORDER_WIDTH (f)
			 - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
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

  if (view && !FRAME_TOOLTIP_P (f))
    {
      block_input ();
      BView_set_view_cursor (view, (invisible_p
				    ? FRAME_OUTPUT_DATA (f)->no_cursor
				    : FRAME_OUTPUT_DATA (f)->current_cursor));
      f->pointer_invisible = invisible_p;
      unblock_input ();
    }
}

static void
haiku_fullscreen (struct frame *f)
{
  enum haiku_fullscreen_mode mode;

  /* When FRAME_OUTPUT_DATA (f)->configury_done is false, the frame is
     being created, and its regular width and height have not yet been
     set.  This function will be called again by haiku_create_frame,
     so do nothing.  */
  if (!FRAME_OUTPUT_DATA (f)->configury_done)
    return;

  if (f->want_fullscreen == FULLSCREEN_MAXIMIZED)
    mode = FULLSCREEN_MODE_MAXIMIZED;
  else if (f->want_fullscreen == FULLSCREEN_BOTH)
    mode = FULLSCREEN_MODE_BOTH;
  else if (f->want_fullscreen == FULLSCREEN_WIDTH)
    mode = FULLSCREEN_MODE_WIDTH;
  else if (f->want_fullscreen == FULLSCREEN_HEIGHT)
    mode = FULLSCREEN_MODE_HEIGHT;
  else
    mode = FULLSCREEN_MODE_NONE;

  f->want_fullscreen = FULLSCREEN_NONE;
  be_set_window_fullscreen_mode (FRAME_HAIKU_WINDOW (f), mode);
  FRAME_OUTPUT_DATA (f)->fullscreen_mode = mode;

  haiku_update_size_hints (f);
  haiku_make_fullscreen_consistent (f);
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
  terminal->get_string_resource_hook = haiku_get_string_resource;
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
  terminal->activate_menubar_hook = haiku_activate_menubar;
  terminal->get_focus_frame = haiku_get_focus_frame;

  return terminal;
}

struct haiku_display_info *
haiku_term_init (void)
{
  struct haiku_display_info *dpyinfo;
  struct terminal *terminal;
  Lisp_Object color_file, color_map, system_name;
  ptrdiff_t nbytes;
  void *name_buffer;

  block_input ();

  Fset_input_interrupt_mode (Qt);
  baud_rate = 19200;
  dpyinfo = xzalloc (sizeof *dpyinfo);
  haiku_io_init ();

  if (port_application_to_emacs < B_OK
      || port_emacs_to_session_manager < B_OK)
    emacs_abort ();

  color_file = Fexpand_file_name (build_string ("rgb.txt"),
				  Fsymbol_value (Qdata_directory));
  color_map = Fx_load_color_file (color_file);

  if (NILP (color_map))
    fatal ("Could not read %s.\n", SDATA (color_file));

  dpyinfo->color_map = color_map;
  dpyinfo->display = BApplication_setup ();
  dpyinfo->next = x_display_list;
  dpyinfo->n_planes = be_get_display_planes ();
  be_get_display_resolution (&dpyinfo->resx, &dpyinfo->resy);

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

#define ASSIGN_CURSOR(cursor, cursor_id)			\
  (dpyinfo->cursor = be_create_cursor_from_id (cursor_id))
  ASSIGN_CURSOR (text_cursor,			CURSOR_ID_I_BEAM);
  ASSIGN_CURSOR (nontext_cursor,		CURSOR_ID_SYSTEM_DEFAULT);
  ASSIGN_CURSOR (modeline_cursor,		CURSOR_ID_CONTEXT_MENU);
  ASSIGN_CURSOR (hand_cursor,			CURSOR_ID_GRAB);
  ASSIGN_CURSOR (hourglass_cursor,		CURSOR_ID_PROGRESS);
  ASSIGN_CURSOR (horizontal_drag_cursor,	CURSOR_ID_RESIZE_EAST_WEST);
  ASSIGN_CURSOR (vertical_drag_cursor,		CURSOR_ID_RESIZE_NORTH_SOUTH);
  ASSIGN_CURSOR (left_edge_cursor,		CURSOR_ID_RESIZE_WEST);
  ASSIGN_CURSOR (top_left_corner_cursor,	CURSOR_ID_RESIZE_NORTH_WEST);
  ASSIGN_CURSOR (top_edge_cursor,		CURSOR_ID_RESIZE_NORTH);
  ASSIGN_CURSOR (top_right_corner_cursor,	CURSOR_ID_RESIZE_NORTH_EAST);
  ASSIGN_CURSOR (right_edge_cursor,		CURSOR_ID_RESIZE_EAST);
  ASSIGN_CURSOR (bottom_right_corner_cursor,	CURSOR_ID_RESIZE_SOUTH_EAST);
  ASSIGN_CURSOR (bottom_edge_cursor,		CURSOR_ID_RESIZE_SOUTH);
  ASSIGN_CURSOR (bottom_left_corner_cursor,	CURSOR_ID_RESIZE_SOUTH_WEST);
  ASSIGN_CURSOR (no_cursor,			CURSOR_ID_NO_CURSOR);
#undef ASSIGN_CURSOR

  system_name = Fsystem_name ();

  if (STRINGP (system_name))
    {
      nbytes = sizeof "GNU Emacs" + sizeof " at ";

      if (ckd_add (&nbytes, nbytes, SBYTES (system_name)))
	memory_full (SIZE_MAX);

      name_buffer = alloca (nbytes);
      sprintf (name_buffer, "%s%s%s", "GNU Emacs",
	       " at ", SDATA (system_name));
      dpyinfo->default_name = build_string (name_buffer);
    }
  else
    dpyinfo->default_name = build_string ("GNU Emacs");

  haiku_start_watching_selections ();

  /* Start listening for font configuration changes.  */
  be_listen_font_settings ();
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
      int bottom_margin = FRAME_BOTTOM_MARGIN_HEIGHT (f);
      int face_id = (FRAME_PARENT_FRAME (f)
		     ? (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     CHILD_FRAME_BORDER_FACE_ID)
			: CHILD_FRAME_BORDER_FACE_ID)
		     : (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     INTERNAL_BORDER_FACE_ID)
			: INTERNAL_BORDER_FACE_ID));
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);
      void *view = FRAME_HAIKU_DRAWABLE (f);

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
      BView_FillRectangle (view, 0, height - bottom_margin - border,
			   width, border);
      BView_EndClip (view);
      BView_draw_unlock (view);
      unblock_input ();
    }
}

void
mark_haiku_display (void)
{
  if (x_display_list)
    {
      mark_object (x_display_list->color_map);
      mark_object (x_display_list->default_name);
    }
}

void
haiku_scroll_bar_remove (struct scroll_bar *bar)
{
  void *view;
  struct frame *f;

  f = WINDOW_XFRAME (XWINDOW (bar->window));
  view = FRAME_HAIKU_DRAWABLE (f);

  block_input ();
  BView_forget_scroll_bar (view, bar->left, bar->top,
			   bar->width, bar->height);
  BScrollBar_delete (bar->scroll_bar);
  expose_frame (WINDOW_XFRAME (XWINDOW (bar->window)),
		bar->left, bar->top, bar->width, bar->height);

  if (bar->horizontal)
    wset_horizontal_scroll_bar (XWINDOW (bar->window), Qnil);
  else
    wset_vertical_scroll_bar (XWINDOW (bar->window), Qnil);
  unblock_input ();
};



/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

static void
haiku_calc_absolute_position (struct frame *f)
{
  int flags = f->size_hint_flags;
  struct frame *p = FRAME_PARENT_FRAME (f);
  int screen_width, screen_height;

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (!((flags & XNegative) || (flags & YNegative)))
    return;

  be_get_screen_dimensions (&screen_width, &screen_height);

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    {
      int width = FRAME_PIXEL_WIDTH (f);

      /* A frame that has been visible at least once should have outer
	 edges.  */
      if (!p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  edges = Fhaiku_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    width = (XFIXNUM (Fnth (make_fixnum (2), edges))
		     - XFIXNUM (Fnth (make_fixnum (0), edges)));
	}

      if (p)
	f->left_pos = (FRAME_PIXEL_WIDTH (p) - width - 2 * f->border_width
		       + f->left_pos);
      else
	f->left_pos = (screen_width - width + f->left_pos);

    }

  if (flags & YNegative)
    {
      int height = FRAME_PIXEL_HEIGHT (f);

      if (!p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  if (NILP (edges))
	    edges = Fhaiku_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    height = (XFIXNUM (Fnth (make_fixnum (3), edges))
		      - XFIXNUM (Fnth (make_fixnum (1), edges)));
	}

      if (p)
	f->top_pos = (FRAME_PIXEL_HEIGHT (p) - height - 2 * f->border_width
		       + f->top_pos);
      else
	f->top_pos = (screen_height - height + f->top_pos);
  }

  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->size_hint_flags &= ~(XNegative | YNegative);
}

void
haiku_set_offset (struct frame *frame, int x, int y,
		  int change_gravity)
{
  Lisp_Object lframe;

  /* Don't allow moving a fullscreen frame: the semantics of that are
     unclear.  */

  XSETFRAME (lframe, frame);
  if (EQ (Fframe_parameter (lframe, Qfullscreen), Qfullboth)
      /* Only do this if the fullscreen status has actually been
	 applied.  */
      && frame->want_fullscreen == FULLSCREEN_NONE
      /* And if the configury during frame creation has been
	 completed.  Otherwise, there will be no valid "old position"
	 to go back to.  */
      && FRAME_OUTPUT_DATA (frame)->configury_done)
    return;

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
  haiku_calc_absolute_position (frame);

  block_input ();
  if (change_gravity)
    BWindow_set_offset (FRAME_HAIKU_WINDOW (frame), frame->left_pos,
			frame->top_pos);
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
      && foreground == s->face->foreground)
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

  DEFSYM (Qdata_directory, "data-directory");

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
