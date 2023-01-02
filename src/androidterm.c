/* Communication module for Android terminals.

Copyright (C) 2023 Free Software Foundation, Inc.

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
#include <stdio.h>

#include "lisp.h"
#include "androidterm.h"
#include "keyboard.h"
#include "blockinput.h"
#include "android.h"
#include "buffer.h"
#include "window.h"

/* This is a chain of structures for all the X displays currently in
   use.  */

struct android_display_info *x_display_list;



/* Android terminal interface functions.  */

#ifndef ANDROID_STUBIFY

#include <android/log.h>

enum
  {
    ANDROID_EVENT_NORMAL,
    ANDROID_EVENT_GOTO_OUT,
    ANDROID_EVENT_DROP,
  };

static struct frame *
android_window_to_frame (struct android_display_info *dpyinfo,
			 android_window wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (wdesc == ANDROID_NONE)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);

      if (!FRAME_ANDROID_P (f) || FRAME_DISPLAY_INFO (f) != dpyinfo)
	continue;

      if (FRAME_ANDROID_WINDOW (f) == wdesc)
        return f;
    }

  return NULL;
}

static void
android_clear_frame (struct frame *f)
{
  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  android_clear_window (FRAME_ANDROID_WINDOW (f));
}

static void
android_ring_bell (struct frame *f)
{
  /* TODO */
}

static void
android_toggle_invisible_pointer (struct frame *f, bool invisible)
{
  /* TODO */
}

/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to gui_update_window_begin
   for each window being updated.  Currently, there is nothing to do
   here because all interesting stuff is done on a window basis.  */

static void
android_update_begin (struct frame *f)
{
  /* The frame is no longer complete, as it is in the midst of an
     update.  */
  FRAME_ANDROID_COMPLETE_P (f) = false;
}

/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
android_update_end (struct frame *f)
{
  /* Mouse highlight may be displayed again.  */
  MOUSE_HL_INFO (f)->mouse_face_defer = false;
}

static void
show_back_buffer (struct frame *f)
{
  struct android_swap_info swap_info;

  memset (&swap_info, 0, sizeof (swap_info));
  swap_info.swap_window = FRAME_ANDROID_WINDOW (f);
  swap_info.swap_action = ANDROID_COPIED;
  android_swap_buffers (&swap_info, 1);
}

/* Flip back buffers on F if it has undrawn content.  */

static void
android_flush_dirty_back_buffer_on (struct frame *f)
{
  if (FRAME_GARBAGED_P (f)
      || buffer_flipping_blocked_p ()
      /* If the frame is not already up to date, do not flush buffers
	 on input, as that will result in flicker.  */
      || !FRAME_ANDROID_COMPLETE_P (f))
    return;

  show_back_buffer (f);
}

/* Convert between the modifier bits Android uses and the modifier
   bits Emacs uses.  */

static int
android_android_to_emacs_modifiers (struct android_display_info *dpyinfo,
				    int state)
{
  return ((state & ANDROID_CONTROL_MASK) ? ctrl_modifier : 0
	  | (state & ANDROID_SHIFT_MASK) ? shift_modifier : 0
	  | (state & ANDROID_ALT_MASK) ? meta_modifier : 0);
}

static int
android_emacs_to_android_modifiers (struct android_display_info *dpyinfo,
				    intmax_t state)
{
  return ((state & ctrl_modifier) ? ANDROID_CONTROL_MASK : 0
	  | (state & shift_modifier) ? ANDROID_SHIFT_MASK : 0
	  | (state & meta_modifier) ? ANDROID_ALT_MASK : 0);
}

static void android_frame_rehighlight (struct android_display_info *);

static void
android_lower_frame (struct frame *f)
{
  /* TODO.  */
}

static void
android_raise_frame (struct frame *f)
{
  /* TODO.  */
}

static void
android_new_focus_frame (struct android_display_info *dpyinfo,
			 struct frame *frame)
{
  struct frame *old_focus;

  old_focus = dpyinfo->focus_frame;

  if (frame != dpyinfo->focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	android_lower_frame (old_focus);

      if (dpyinfo->focus_frame && dpyinfo->focus_frame->auto_raise)
	dpyinfo->pending_autoraise_frame = dpyinfo->focus_frame;
      else
	dpyinfo->pending_autoraise_frame = NULL;
    }

  android_frame_rehighlight (dpyinfo);
}

static void
android_focus_changed (int type, int state,
		       struct android_display_info *dpyinfo,
		       struct frame *frame, struct input_event *bufp)
{
  if (type == ANDROID_FOCUS_IN)
    {
      if (dpyinfo->x_focus_event_frame != frame)
        {
          android_new_focus_frame (dpyinfo, frame);
          dpyinfo->x_focus_event_frame = frame;
          bufp->kind = FOCUS_IN_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      frame->output_data.android->focus_state |= state;
    }
  else if (type == ANDROID_FOCUS_OUT)
    {
      frame->output_data.android->focus_state &= ~state;

      if (dpyinfo->x_focus_event_frame == frame)
        {
          dpyinfo->x_focus_event_frame = 0;
          android_new_focus_frame (dpyinfo, 0);

          bufp->kind = FOCUS_OUT_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      if (frame->pointer_invisible)
        android_toggle_invisible_pointer (frame, false);
    }
}

static void
android_detect_focus_change (struct android_display_info *dpyinfo,
			     struct frame *frame,
			     union android_event *event,
			     struct input_event *bufp)
{
  if (!frame)
    return;

  switch (event->type)
    {
    case ANDROID_FOCUS_IN:
    case ANDROID_FOCUS_OUT:
      android_focus_changed (event->type, FOCUS_EXPLICIT,
			     dpyinfo, frame, bufp);
      break;

    default:
      break;
    }
}

static int
handle_one_android_event (struct android_display_info *dpyinfo,
			  union android_event *event, int *finish,
			  struct input_event *hold_quit)
{
  union android_event configureEvent;
  struct frame *f, *any, *mouse_frame;
  Mouse_HLInfo *hlinfo;
  union buffered_input_event inev;
  int modifiers, count;

  /* It is okay for this to not resemble handle_one_xevent so much.
     Differences in event handling code are much less nasty than
     stuble differences in the graphics code.  */

  count = 0;
  hlinfo = &dpyinfo->mouse_highlight;
  *finish = ANDROID_EVENT_NORMAL;
  any = android_window_to_frame (dpyinfo, event->xany.window);

  EVENT_INIT (inev.ie);

  switch (event->type)
    {
    case ANDROID_CONFIGURE_NOTIFY:
      configureEvent = *event;

      f = android_window_to_frame (dpyinfo,
				   configureEvent.xconfigure.window);

      int width = configureEvent.xconfigure.width;
      int height = configureEvent.xconfigure.height;

      if (CONSP (frame_size_history))
	frame_size_history_extra (f, build_string ("ConfigureNotify"),
				  FRAME_PIXEL_WIDTH (f),
				  FRAME_PIXEL_HEIGHT (f),
				  width, height, f->new_width,
				  f->new_height);

      /* Even if the number of character rows and columns has
	 not changed, the font size may have changed, so we need
	 to check the pixel dimensions as well.  */

      if (width != FRAME_PIXEL_WIDTH (f)
	  || height != FRAME_PIXEL_HEIGHT (f)
	  || (f->new_size_p
	      && ((f->new_width >= 0 && width != f->new_width)
		  || (f->new_height >= 0 && height != f->new_height))))
	{
	  change_frame_size (f, width, height, false, true, false);
	  android_clear_under_internal_border (f);
	  SET_FRAME_GARBAGED (f);
	  cancel_mouse_face (f);
	}

      goto OTHER;

    case ANDROID_KEY_PRESS:

      /* Set f to any.  There are no ``outer windows'' on Android.  */
      f = any;

      /* If mouse-highlight is an integer, input clears out
	 mouse highlighting.  */
      if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
	  && (any == 0
	      || !EQ (any->tool_bar_window, hlinfo->mouse_face_window)
	      || !EQ (any->tab_bar_window, hlinfo->mouse_face_window)))
        {
	  mouse_frame = hlinfo->mouse_face_mouse_frame;

	  clear_mouse_face (hlinfo);
	  hlinfo->mouse_face_hidden = true;

	  if (mouse_frame)
	    android_flush_dirty_back_buffer_on (mouse_frame);
	}

      event->xkey.state
	|= android_emacs_to_android_modifiers (dpyinfo,
					       extra_keyboard_modifiers);
      modifiers = event->xkey.state;

      /* Common for all keysym input events.  */
      XSETFRAME (inev.ie.frame_or_window, any);
      inev.ie.modifiers
	= android_android_to_emacs_modifiers (dpyinfo, modifiers);
      inev.ie.timestamp = event->xkey.time;

      /* First deal with keysyms which have defined translations to
	 characters.  */

      if (event->xkey.unicode_char >= 32
	  && event->xkey.unicode_char < 128)
	{
	  inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	  inev.ie.code = event->xkey.unicode_char;
	}
      else if (event->xkey.unicode_char < 32)
	{
	  /* If the key is a modifier key, just return.  */
	  if (ANDROID_IS_MODIFIER_KEY (event->xkey.keycode))
	    goto done_keysym;

	  /* Next, deal with special ``characters'' by giving the
	     keycode to keyboard.c.  */
	  inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
	  inev.ie.code = event->xkey.keycode;
	}
      else
	{
	  /* Finally, deal with Unicode characters.  */
	  inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  inev.ie.code = event->xkey.unicode_char;
	}

      goto done_keysym;

    done_keysym:
      goto OTHER;

    case ANDROID_FOCUS_IN:
    case ANDROID_FOCUS_OUT:
      android_detect_focus_change (dpyinfo, any, event, &inev.ie);
      goto OTHER;

    case ANDROID_WINDOW_ACTION:

      f = any;

      if (event->xaction.action == 0)
	{
	  /* Action 0 either means to destroy a frame or to create a
	     new frame, depending on whether or not
	     event->xaction.window exists.  */

	  if (event->xaction.window)
	    {
	      if (!f)
		goto OTHER;

	      inev.ie.kind = DELETE_WINDOW_EVENT;
	      XSETFRAME (inev.ie.frame_or_window, f);
	    }
	  else
	    /* A new frame must be created.  */;
	}

      goto OTHER;

    default:
      goto OTHER;
    }

 OTHER:
  if (inev.ie.kind != NO_EVENT)
    {
      kbd_buffer_store_buffered_event (&inev, hold_quit);
      count++;
    }

  return count;
}

static int
android_read_socket (struct terminal *terminal,
		     struct input_event *hold_quit)
{
  int count = 0;
  struct android_display_info *dpyinfo;

  dpyinfo = terminal->display_info.android;

  block_input ();
  while (android_pending ())
    {
      int finish;
      union android_event event;

      android_next_event (&event);
      count += handle_one_android_event (dpyinfo, &event, &finish,
					 hold_quit);

      if (finish == ANDROID_EVENT_GOTO_OUT)
	break;
    }
  unblock_input ();

  /* If the focus was just given to an auto-raising frame, raise it
     now.  */
  if (dpyinfo->pending_autoraise_frame)
    {
      /* android_raise_frame (dpyinfo->pending_autoraise_frame);
	 TODO */
      dpyinfo->pending_autoraise_frame = NULL;
    }

  return count;
}

static void
android_frame_up_to_date (struct frame *f)
{
  eassert (FRAME_ANDROID_P (f));
  block_input ();
  FRAME_MOUSE_UPDATE (f);

  if (!buffer_flipping_blocked_p ())
    show_back_buffer (f);

  /* The frame is now complete, as its contents have been drawn.  */
  FRAME_ANDROID_COMPLETE_P (f) = true;
  unblock_input ();
}

static void
android_buffer_flipping_unblocked_hook (struct frame *f)
{
  block_input ();
  show_back_buffer (f);
  unblock_input ();
}

static void
android_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
{
  unsigned long background;

  background = FRAME_BACKGROUND_PIXEL (f);
  bgcolor->pixel = background;

  android_query_colors (f, bgcolor, 1);
}

int
android_parse_color (struct frame *f, const char *color_name,
		     Emacs_Color *color)
{
  unsigned short r, g, b;
  Lisp_Object tem, tem1;
  unsigned long lisp_color;

  if (parse_color_spec (color_name, &r, &g, &b))
    {
      color->red = r;
      color->green = g;
      color->blue = b;

      return 1;
    }

  tem = x_display_list->color_map;
  for (; CONSP (tem); tem = XCDR (tem))
    {
      tem1 = XCAR (tem);

      if (CONSP (tem1)
	  && !xstrcasecmp (SSDATA (XCAR (tem1)), color_name))
	{
	  lisp_color = XFIXNUM (XCDR (tem1));
	  color->red = RED_FROM_ULONG (lisp_color) * 257;
	  color->green = GREEN_FROM_ULONG (lisp_color) * 257;
	  color->blue = BLUE_FROM_ULONG (lisp_color) * 257;
	  return 1;
	}
    }

  return 0;
}

bool
android_alloc_nearest_color (struct frame *f, Emacs_Color *color)
{
  gamma_correct (f, color);
  color->pixel = RGB_TO_ULONG (color->red / 256,
			       color->green / 256,
			       color->blue / 256);

  return true;
}

void
android_query_colors (struct frame *f, Emacs_Color *colors, int ncolors)
{
  int i;

  for (i = 0; i < ncolors; ++i)
    {
      colors[i].red = RED_FROM_ULONG (colors[i].pixel) * 257;
      colors[i].green = RED_FROM_ULONG (colors[i].pixel) * 257;
      colors[i].blue = RED_FROM_ULONG (colors[i].pixel) * 257;
    }
}

static void
android_mouse_position (struct frame **fp, int insist,
			Lisp_Object *bar_window,
			enum scroll_bar_part *part, Lisp_Object *x,
			Lisp_Object *y, Time *timestamp)
{
  /* TODO */
}

static Lisp_Object
android_get_focus_frame (struct frame *f)
{
  Lisp_Object lisp_focus;
  struct frame *focus;

  focus = FRAME_DISPLAY_INFO (f)->focus_frame;

  if (!focus)
    return Qnil;

  XSETFRAME (lisp_focus, focus);
  return lisp_focus;
}

static void
android_focus_frame (struct frame *f, bool noactivate)
{
  /* TODO */
}

static void
android_frame_rehighlight (struct android_display_info *dpyinfo)
{
  struct frame *old_highlight;

  old_highlight = dpyinfo->highlight_frame;

  if (dpyinfo->focus_frame)
    {
      dpyinfo->highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->focus_frame))
	   : dpyinfo->focus_frame);
      if (!FRAME_LIVE_P (dpyinfo->highlight_frame))
	{
	  fset_focus_frame (dpyinfo->focus_frame, Qnil);
	  dpyinfo->highlight_frame = dpyinfo->focus_frame;
	}
    }
  else
    dpyinfo->highlight_frame = 0;

  if (dpyinfo->highlight_frame != old_highlight)
    {
      /* This is not yet required on Android.  */
#if 0
      if (old_highlight)
	android_frame_unhighlight (old_highlight);
      if (dpyinfo->highlight_frame)
	android_frame_highlight (dpyinfo->highlight_frame);
#endif
    }
}

static void
android_frame_rehighlight_hook (struct frame *f)
{
  android_frame_rehighlight (FRAME_DISPLAY_INFO (f));
}

static void
android_frame_raise_lower (struct frame *f, bool raise_flag)
{
  if (raise_flag)
    android_raise_frame (f);
  else
    android_lower_frame (f);
}

void
android_make_frame_visible (struct frame *f)
{
  android_map_window (FRAME_ANDROID_WINDOW (f));

  SET_FRAME_VISIBLE (f, true);
  SET_FRAME_ICONIFIED (f, false);
}

void
android_make_frame_invisible (struct frame *f)
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_DISPLAY_INFO (f)->highlight_frame == f)
    FRAME_DISPLAY_INFO (f)->highlight_frame = 0;

  android_unmap_window (FRAME_ANDROID_WINDOW (f));

  SET_FRAME_VISIBLE (f, false);
  SET_FRAME_ICONIFIED (f, false);
}

static void
android_make_frame_visible_invisible (struct frame *f, bool visible)
{
  if (visible)
    android_make_frame_visible (f);
  else
    android_make_frame_invisible (f);
}

static void
android_fullscreen_hook (struct frame *f)
{
  /* TODO */
}

void
android_iconify_frame (struct frame *f)
{
  /* TODO */
}

static void
android_set_window_size_1 (struct frame *f, bool change_gravity,
			   int width, int height)
{
  if (change_gravity)
    f->win_gravity = NorthWestGravity;

  android_resize_window (FRAME_ANDROID_WINDOW (f), width,
			 height);

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);
}

void
android_set_window_size (struct frame *f, bool change_gravity,
			 int width, int height)
{
  block_input ();

  android_set_window_size_1 (f, change_gravity, width, height);
  android_clear_under_internal_border (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  unblock_input ();

  do_pending_window_change (false);
}

static void
android_set_offset (struct frame *f, int xoff, int yoff,
		    int change_gravity)
{
  if (change_gravity > 0)
    {
      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }

  android_move_window (FRAME_ANDROID_WINDOW (f), xoff, yoff);
}

static void
android_set_alpha (struct frame *f)
{
  /* TODO */
}

static Lisp_Object
android_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);
  int unit, font_ascent, font_descent;

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;
  if (FRAME_FONT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  get_font_ascent_descent (font, &font_ascent, &font_descent);
  FRAME_LINE_HEIGHT (f) = font_ascent + font_descent;

  /* We could use a more elaborate calculation here.  */
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  /* Compute character columns occupied by scrollbar.

     Don't do things differently for non-toolkit scrollbars
     (Bug#17163).  */
  unit = FRAME_COLUMN_WIDTH (f);
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    FRAME_CONFIG_SCROLL_BAR_COLS (f)
      = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
  else
    FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;


  /* Don't change the size of a tip frame; there's no point in doing it
     because it's done in Fx_show_tip, and it leads to problems because
     the tip frame has no widget.  */
  if (FRAME_ANDROID_WINDOW (f) != 0 && !FRAME_TOOLTIP_P (f))
    adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 3,
		       false, Qfont);

  return font_object;
}

static bool
android_bitmap_icon (struct frame *f, Lisp_Object file)
{
  /* TODO */
  return false;
}

static void
android_free_pixmap_hook (struct frame *f, Emacs_Pixmap pixmap)
{
  android_free_pixmap (pixmap);
}

void
android_free_frame_resources (struct frame *f)
{
  struct android_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  hlinfo = &dpyinfo->mouse_highlight;

  block_input ();
  free_frame_faces (f);

  /* FRAME_ANDROID_WINDOW can be 0 if frame creation failed.  */
  if (FRAME_ANDROID_WINDOW (f))
    android_destroy_window (FRAME_ANDROID_WINDOW (f));

  android_free_gcs (f);

  /* Free extra GCs allocated by android_setup_relief_colors.  */
  if (f->output_data.android->white_relief.gc)
    {
      android_free_gc (f->output_data.android->white_relief.gc);
      f->output_data.android->white_relief.gc = 0;
    }
  if (f->output_data.android->black_relief.gc)
    {
      android_free_gc (f->output_data.android->black_relief.gc);
      f->output_data.android->black_relief.gc = 0;
    }

  if (f == dpyinfo->focus_frame)
    dpyinfo->focus_frame = 0;
  if (f == dpyinfo->x_focus_event_frame)
    dpyinfo->x_focus_event_frame = 0;
  if (f == dpyinfo->highlight_frame)
    dpyinfo->highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  unblock_input ();
}

static void
android_delete_frame (struct frame *f)
{
  android_free_frame_resources (f);
  xfree (f->output_data.android);
  f->output_data.android = NULL;
}

static void
android_delete_terminal (struct terminal *terminal)
{
  error ("Cannot terminate connection to Android display server");
}



/* RIF functions.  */

static void
android_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
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

  /* Cursor off.  Will be switched on again in gui_update_window_end.  */
  gui_clear_cursor (w);

  android_copy_area (FRAME_ANDROID_WINDOW (f),
		     FRAME_ANDROID_WINDOW (f),
		     f->output_data.android->normal_gc,
		     x, from_y, width, height, x, to_y);

  unblock_input ();
}

static void
android_after_update_window_line (struct window *w, struct glyph_row *desired_row)
{
  eassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = true;
}

static void
android_flip_and_flush (struct frame *f)
{
  block_input ();
  show_back_buffer (f);

  /* The frame is complete again as its contents were just
     flushed.  */
  FRAME_ANDROID_COMPLETE_P (f) = true;
  unblock_input ();
}

static void
android_clear_rectangle (struct frame *f, struct android_gc *gc, int x,
			 int y, int width, int height)
{
  struct android_gc_values xgcv;

  android_get_gc_values (gc, (ANDROID_GC_BACKGROUND
			      | ANDROID_GC_FOREGROUND),
			 &xgcv);
  android_set_foreground (gc, xgcv.background);
  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc,
			  x, y, width, height);
  android_set_foreground (gc, xgcv.foreground);
}

static void
android_reset_clip_rectangles (struct frame *f, struct android_gc *gc)
{
  android_set_clip_mask (gc, ANDROID_NONE);
}

static void
android_clip_to_row (struct window *w, struct glyph_row *row,
		     enum glyph_row_area area, struct android_gc *gc)
{
  struct android_rectangle clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.x = window_x;
  clip_rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.y = max (clip_rect.y, window_y);
  clip_rect.width = window_width;
  clip_rect.height = row->visible_height;

  android_set_clip_rectangles (gc, 0, 0, &clip_rect, 1);
}

static void
android_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
			    struct draw_fringe_bitmap_params *p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct android_gc *gc = f->output_data.android->normal_gc;
  struct face *face = p->face;

  /* Must clip because of partially visible lines.  */
  android_clip_to_row (w, row, ANY_AREA, gc);

  if (p->bx >= 0 && !p->overlay_p)
    {
      /* In case the same realized face is used for fringes and for
	 something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 ANDROID_FILL_SOLID in
	 android_draw_glyph_string_background.  */
      if (face->stipple)
	{
	  android_set_fill_style (face->gc, ANDROID_FILL_OPAQUE_STIPPLED);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), face->gc,
				  p->bx, p->by, p->nx, p->ny);
	  android_set_fill_style (face->gc, ANDROID_FILL_SOLID);

	  row->stipple_p = true;
	}
      else
	{
	  android_set_background (face->gc, face->background);
	  android_clear_rectangle (f, face->gc, p->bx, p->by, p->nx, p->ny);
	  android_set_foreground (face->gc, face->foreground);
	}
    }

  if (p->which)
    {
      android_drawable drawable;
      char *bits;
      android_pixmap pixmap, clipmask;
      struct android_gc_values gcv;
      unsigned long background, cursor_pixel;
      int depth;

      drawable = FRAME_ANDROID_WINDOW (f);
      clipmask = ANDROID_NONE;
      background = face->background;
      cursor_pixel = f->output_data.android->cursor_pixel;
      depth = FRAME_DISPLAY_INFO (f)->n_planes;

      if (p->wd > 8)
	bits = (char *) (p->bits + p->dh);
      else
	bits = (char *) p->bits + p->dh;

      pixmap = android_create_pixmap_from_bitmap_data (bits, p->wd, p->h,
						       (p->cursor_p
							? (p->overlay_p
							   ? face->background
							   : cursor_pixel)
							: face->foreground),
						       background, depth);

      if (p->overlay_p)
	{
	  clipmask = android_create_pixmap_from_bitmap_data (bits, p->wd, p->h,
							     1, 0, 1);

	  gcv.clip_mask = clipmask;
	  gcv.clip_x_origin = p->x;
	  gcv.clip_y_origin = p->y;
	  android_change_gc (gc, (ANDROID_GC_CLIP_MASK
				  | ANDROID_GC_CLIP_X_ORIGIN
				  | ANDROID_GC_CLIP_Y_ORIGIN),
			     &gcv);
	}

      android_copy_area (pixmap, drawable, gc, 0, 0, p->wd, p->h,
			 p->x, p->y);
      android_free_pixmap (pixmap);

      if (p->overlay_p)
	{
	  gcv.clip_mask = ANDROID_NONE;
	  android_change_gc (gc, ANDROID_GC_CLIP_MASK, &gcv);
	  android_free_pixmap (clipmask);
	}
    }

  android_reset_clip_rectangles (f, gc);
}

/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
android_set_cursor_gc (struct glyph_string *s)
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.android->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      struct android_gc_values xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.android->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.android->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      mask = (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	android_change_gc (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
			   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = android_create_gc (mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
android_set_mouse_face_gc (struct glyph_string *s)
{
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 except for FONT.  */
      struct android_gc_values xgcv;
      unsigned long mask;

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;

      mask = (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	android_change_gc (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
			   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = android_create_gc (mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }

  eassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static void
android_set_mode_line_face_gc (struct glyph_string *s)
{
  s->gc = s->face->gc;
}

/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static void
android_set_glyph_string_gc (struct glyph_string *s)
{
  prepare_face_for_display (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      android_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      android_set_cursor_gc (s);
      s->stippled_p = false;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      android_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    emacs_abort ();

  /* GC must have been set.  */
  eassert (s->gc != 0);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static void
android_set_glyph_string_clipping (struct glyph_string *s)
{
  struct android_rectangle *r = s->clip;
  int n = get_glyph_string_clip_rects (s, r, 2);

  if (n > 0)
    android_set_clip_rectangles (s->gc, 0, 0, r, n);
  s->num_clips = n;
}


/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
android_set_glyph_string_clipping_exactly (struct glyph_string *src,
					   struct glyph_string *dst)
{
  struct android_rectangle r;

  r.x = src->x;
  r.width = src->width;
  r.y = src->y;
  r.height = src->height;
  dst->clip[0] = r;
  dst->num_clips = 1;
  android_set_clip_rectangles (dst->gc, 0, 0, &r, 1);
}

static void
android_compute_glyph_string_overhangs (struct glyph_string *s)
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
android_clear_glyph_string_rect (struct glyph_string *s, int x, int y,
				 int w, int h)
{
  android_clear_rectangle (s->f, s->gc, x, y, w, h);
}

static void
android_draw_glyph_string_background (struct glyph_string *s, bool force_p)
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_horizontal_line_width, 0);

      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */
	  android_set_fill_style (s->gc, ANDROID_FILL_OPAQUE_STIPPLED);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
				  s->x, s->y + box_line_width,
				  s->background_width,
				  s->height - 2 * box_line_width);
	  android_set_fill_style (s->gc, ANDROID_FILL_SOLID);
	  s->background_filled_p = true;
	}
      else if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       /* When xdisp.c ignores FONT_HEIGHT, we cannot trust
		  font dimensions, since the actual glyphs might be
		  much smaller.  So in that case we always clear the
		  rectangle with background color.  */
	       || FONT_TOO_HIGH (s->font)
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  android_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
					   s->background_width,
					   s->height - 2 * box_line_width);
	  s->background_filled_p = true;
	}
    }
}

static void
android_fill_triangle (struct frame *f, struct android_gc *gc,
		       struct android_point point1,
		       struct android_point point2,
		       struct android_point point3)
{
  struct android_point abc[3];

  abc[0] = point1;
  abc[1] = point2;
  abc[2] = point3;

  android_fill_polygon (FRAME_ANDROID_WINDOW (f),
			gc, abc, 3, ANDROID_CONVEX,
			ANDROID_COORD_MODE_ORIGIN);
}

static struct android_point
android_make_point (int x, int y)
{
  struct android_point pt;

  pt.x = x;
  pt.y = y;

  return pt;
}

static bool
android_inside_rect_p (struct android_rectangle *rects, int nrects, int x,
		       int y)
{
  int i;

  for (i = 0; i < nrects; ++i)
    {
      if (x >= rects[i].x && y >= rects[i].y
	  && x < rects[i].x + rects[i].width
	  && y < rects[i].y + rects[i].height)
	return true;
    }

  return false;
}

static void
android_clear_point (struct frame *f, struct android_gc *gc,
		     int x, int y)
{
  struct android_gc_values xgcv;

  android_get_gc_values (gc, ANDROID_GC_BACKGROUND | ANDROID_GC_FOREGROUND,
			 &xgcv);
  android_set_foreground (gc, xgcv.background);
  android_draw_point (FRAME_ANDROID_WINDOW (f), gc, x, y);
  android_set_foreground (gc, xgcv.foreground);
}

static void
android_draw_relief_rect (struct frame *f, int left_x, int top_y, int right_x,
			  int bottom_y, int hwidth, int vwidth, bool raised_p,
			  bool top_p, bool bot_p, bool left_p, bool right_p,
			  struct android_rectangle *clip_rect)
{
  struct android_gc *gc, *white_gc, *black_gc, *normal_gc;
  android_drawable drawable;

  /* This code is more complicated than it has to be, because of two
     minor hacks to make the boxes look nicer: (i) if width > 1, draw
     the outermost line using the black relief.  (ii) Omit the four
     corner pixels.  */

  white_gc = f->output_data.android->white_relief.gc;
  black_gc = f->output_data.android->black_relief.gc;
  normal_gc = f->output_data.android->normal_gc;

  drawable = FRAME_ANDROID_WINDOW (f);

  android_set_clip_rectangles (white_gc, 0, 0, clip_rect, 1);
  android_set_clip_rectangles (black_gc, 0, 0, clip_rect, 1);

  if (raised_p)
    gc = white_gc;
  else
    gc = black_gc;

  /* Draw lines.  */

  if (top_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, left_x, top_y,
			    right_x - left_x + 1, hwidth);

  if (left_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, left_x, top_y,
			    vwidth, bottom_y - top_y + 1);

  if (raised_p)
    gc = black_gc;
  else
    gc = white_gc;

  if (bot_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, left_x,
			    bottom_y - hwidth + 1,
			    right_x - left_x + 1, hwidth);

  if (right_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc,
			    right_x - vwidth + 1,
			    top_y, vwidth, bottom_y - top_y + 1);

  /* Draw corners.  */

  if (bot_p && left_p)
    android_fill_triangle (f, raised_p ? white_gc : black_gc,
			   android_make_point (left_x, bottom_y - hwidth),
			   android_make_point (left_x + vwidth,
					       bottom_y - hwidth),
			   android_make_point (left_x, bottom_y));

  if (top_p && right_p)
    android_fill_triangle (f, raised_p ? white_gc : black_gc,
			   android_make_point (right_x - vwidth, top_y),
			   android_make_point (right_x, top_y),
			   android_make_point (right_x - vwidth,
					       top_y + hwidth));

  /* Draw outer line.  */

  if (top_p && left_p && bot_p && right_p
      && hwidth > 1 && vwidth > 1)
    android_draw_rectangle (FRAME_ANDROID_WINDOW (f),
			    black_gc, left_x, top_y,
			    right_x - left_x, bottom_y - top_y);
  else
    {
      if (top_p && hwidth > 1)
	android_draw_line (drawable, black_gc, left_x, top_y,
			   right_x + 1, top_y);

      if (bot_p && hwidth > 1)
	android_draw_line (drawable, black_gc, left_x, bottom_y,
			   right_x + 1, bottom_y);

      if (left_p && vwidth > 1)
	android_draw_line (drawable, black_gc, left_x, top_y,
			   left_x, bottom_y + 1);

      if (right_p && vwidth > 1)
	android_draw_line (drawable, black_gc, right_x, top_y,
			   right_x, bottom_y + 1);
    }

  /* Erase corners.  */

  if (hwidth > 1 && vwidth > 1)
    {
      if (left_p && top_p && android_inside_rect_p (clip_rect, 1,
						    left_x, top_y))
	android_clear_point (f, normal_gc, left_x, top_y);

      if (left_p && bot_p && android_inside_rect_p (clip_rect, 1,
						    left_x, bottom_y))
	android_clear_point (f, normal_gc, left_x, bottom_y);

      if (right_p && top_p && android_inside_rect_p (clip_rect, 1,
						     right_x, top_y))
	android_clear_point (f, normal_gc, right_x, top_y);

      if (right_p && bot_p && android_inside_rect_p (clip_rect, 1,
						     right_x, bottom_y))
	android_clear_point (f, normal_gc, right_x, bottom_y);
    }

  android_reset_clip_rectangles (f, white_gc);
  android_reset_clip_rectangles (f, black_gc);
}

static void
android_draw_box_rect (struct glyph_string *s,
		       int left_x, int top_y, int right_x, int bottom_y,
		       int hwidth, int vwidth, bool left_p, bool right_p,
		       struct android_rectangle *clip_rect)
{
  struct android_gc_values xgcv;

  android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
  android_set_foreground (s->gc, s->face->box_color);
  android_set_clip_rectangles (s->gc, 0, 0, clip_rect, 1);

  /* Top.  */
  android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, left_x,
			  left_x, right_x - left_x + 1, hwidth);

  /* Left.  */
  if (left_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, left_x,
			    top_y, vwidth, bottom_y - top_y + 1);

  /* Bottom.  */
  android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, left_x,
			  bottom_y - hwidth + 1, right_x - left_x + 1,
			  hwidth);

  /* Right.  */
  if (right_p)
    android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
			    right_x - vwidth + 1, top_y, vwidth,
			    bottom_y - top_y + 1);

  android_set_foreground (s->gc, xgcv.foreground);
  android_reset_clip_rectangles (s->f, s->gc);
}

#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 48000

static bool
android_alloc_lighter_color (struct frame *f, unsigned long *pixel,
			     double factor, int delta)
{
  Emacs_Color color, new;
  long bright;
  bool success_p;

  /* Get RGB color values.  */
  color.pixel = *pixel;
  android_query_colors (f, &color, 1);

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  eassert (factor >= 0);
  new.red = min (0xffff, factor * color.red);
  new.green = min (0xffff, factor * color.green);
  new.blue = min (0xffff, factor * color.blue);

  /* Calculate brightness of COLOR.  */
  bright = (2 * color.red + 3 * color.green + color.blue) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double) bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;

      if (factor < 1)
	{
	  new.red =   max (0, new.red -   min_delta);
	  new.green = max (0, new.green - min_delta);
	  new.blue =  max (0, new.blue -  min_delta);
	}
      else
	{
	  new.red =   min (0xffff, min_delta + new.red);
	  new.green = min (0xffff, min_delta + new.green);
	  new.blue =  min (0xffff, min_delta + new.blue);
	}
    }

  /* Try to allocate the color.  */
  success_p = android_alloc_nearest_color (f, &new);

  if (success_p)
    {
      if (new.pixel == *pixel)
	{
	  /* If we end up with the same color as before, try adding
	     delta to the RGB values.  */
	  new.red = min (0xffff, delta + color.red);
	  new.green = min (0xffff, delta + color.green);
	  new.blue = min (0xffff, delta + color.blue);
	  success_p = android_alloc_nearest_color (f, &new);
	}
      else
	success_p = true;

      *pixel = new.pixel;
    }

  return success_p;
}

/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.android->relief_background.  If such a color
   cannot be allocated, use DEFAULT_PIXEL, instead.  */

static void
android_setup_relief_color (struct frame *f, struct relief *relief,
			    double factor, int delta,
			    unsigned long default_pixel)
{
  struct android_gc_values xgcv;
  struct android_output *di = f->output_data.android;
  unsigned long mask = ANDROID_GC_FOREGROUND;
  unsigned long pixel;
  unsigned long background = di->relief_background;
  struct android_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (relief->gc && relief->pixel != -1)
    relief->pixel = -1;

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;

  if (dpyinfo->n_planes != 1
      && android_alloc_lighter_color (f, &pixel, factor, delta))
    xgcv.foreground = relief->pixel = pixel;

  if (relief->gc == 0)
    relief->gc = android_create_gc (mask, &xgcv);
  else
    android_change_gc (relief->gc, mask, &xgcv);
}

/* Set up colors for the relief lines around glyph string S.  */

static void
android_setup_relief_colors (struct glyph_string *s)
{
  struct android_output *di;
  unsigned long color;

  di = s->f->output_data.android;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    {
      struct android_gc_values xgcv;

      /* Get the background color of the face.  */
      android_get_gc_values (s->gc, ANDROID_GC_BACKGROUND, &xgcv);
      color = xgcv.background;
    }

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      android_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
				  WHITE_PIX_DEFAULT (s->f));
      android_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
				  BLACK_PIX_DEFAULT (s->f));
    }
}

static void
android_draw_glyph_string_box (struct glyph_string *s)
{
  int hwidth, vwidth, left_x, right_x, top_y, bottom_y, last_x;
  bool raised_p, left_p, right_p;
  struct glyph *last_glyph;
  struct android_rectangle clip_rect;

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

  vwidth = eabs (s->face->box_vertical_line_width);
  hwidth = eabs (s->face->box_horizontal_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
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

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    android_draw_box_rect (s, left_x, top_y, right_x, bottom_y, hwidth,
			   vwidth, left_p, right_p, &clip_rect);
  else
    {
      android_setup_relief_colors (s);
      android_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y, hwidth,
				vwidth, raised_p, true, true, left_p, right_p,
				&clip_rect);
    }
}

static void
android_draw_glyph_string_bg_rect (struct glyph_string *s, int x, int y,
				   int w, int h)
{
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      android_set_fill_style (s->gc, ANDROID_FILL_OPAQUE_STIPPLED);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, x,
			      y, w, h);
      android_set_fill_style (s->gc, ANDROID_FILL_SOLID);
    }
  else
    android_clear_glyph_string_rect (s, x, y, w, h);
}

static void
android_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
  struct android_rectangle r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

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

  top_p = bot_p = left_p = right_p = false;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = true;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = true;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = true;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = true;

  android_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  android_draw_relief_rect (s->f, x, y, x1, y1, thick, thick, raised_p,
			    top_p, bot_p, left_p, right_p, &r);
}

static void
android_draw_image_foreground (struct glyph_string *s)
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      unsigned long mask = (ANDROID_GC_CLIP_MASK
			    | ANDROID_GC_CLIP_X_ORIGIN
			    | ANDROID_GC_CLIP_Y_ORIGIN
			    | ANDROID_GC_FUNCTION);
      struct android_gc_values xgcv;
      struct android_rectangle clip_rect, image_rect, r;

      xgcv.clip_mask = s->img->mask;
      xgcv.clip_x_origin = x - s->slice.x;
      xgcv.clip_y_origin = y - s->slice.y;
      xgcv.function = ANDROID_GC_COPY;
      android_change_gc (s->gc, mask, &xgcv);

      get_glyph_string_clip_rect (s, &clip_rect);
      image_rect.x = x;
      image_rect.y = y;
      image_rect.width = s->slice.width;
      image_rect.height = s->slice.height;

      if (gui_intersect_rectangles (&clip_rect, &image_rect, &r))
	android_copy_area (s->img->pixmap,
			   FRAME_ANDROID_WINDOW (s->f),
			   s->gc, s->slice.x + r.x - x,
			   s->slice.y + r.y - y,
			   r.width, r.height, r.x, r.y);

      /* When the image has a mask, we can expect that at least part
	 of a mouse highlight or a block cursor will be visible.  If
	 the image doesn't have a mask, make a block cursor visible by
	 drawing a rectangle around the image.  I believe it's looking
	 better if we do nothing here for mouse-face.  */
      if (s->hl == DRAW_CURSOR && !s->img->mask)
	{
	  int relief = eabs (s->img->relief);
	  android_draw_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
				  x - relief, y - relief,
				  s->slice.width + relief*2 - 1,
				  s->slice.height + relief*2 - 1);
	}

      android_set_clip_mask (s->gc, ANDROID_NONE);
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    android_draw_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, x, y,
			    s->slice.width - 1, s->slice.height - 1);
}

static void
android_draw_image_glyph_string (struct glyph_string *s)
{
  int box_line_hwidth = max (s->face->box_vertical_line_width, 0);
  int box_line_vwidth = max (s->face->box_horizontal_line_width, 0);
  int height;

  height = s->height;
  if (s->slice.y == 0)
    height -= box_line_vwidth;
  if (s->slice.y + s->slice.height >= s->img->height)
    height -= box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (s->stippled_p)
	s->row->stipple_p = true;

      int x = s->x;
      int y = s->y;
      int width = s->background_width;

      if (s->first_glyph->left_box_line_p
	  && s->slice.x == 0)
	{
	  x += box_line_hwidth;
	  width -= box_line_hwidth;
	}

      if (s->slice.y == 0)
	y += box_line_vwidth;

      android_draw_glyph_string_bg_rect (s, x, y, width, height);

      s->background_filled_p = true;
    }

  /* Draw the foreground.  */
  android_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    android_draw_image_relief (s);
}

static void
android_draw_stretch_glyph_string (struct glyph_string *s)
{
  eassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
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

      /* Draw cursor.  */
      android_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;
	  struct android_rectangle r;
	  struct android_gc *gc;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      android_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;

	  get_glyph_string_clip_rect (s, &r);
	  android_set_clip_rectangles (gc, 0, 0, &r, 1);

	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      android_set_fill_style (gc, ANDROID_FILL_OPAQUE_STIPPLED);
	      android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f),
				      gc, x, y, w, h);
	      android_set_fill_style (gc, ANDROID_FILL_SOLID);

	      s->row->stipple_p = true;
	    }
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (gc, (ANDROID_GC_FOREGROUND
					  | ANDROID_GC_BACKGROUND),
				     &xgcv);
	      android_set_foreground (gc, xgcv.background);
	      android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f),
				      gc, x, y, w, h);
	      android_set_foreground (gc, xgcv.foreground);
	    }

	  android_reset_clip_rectangles (s->f, gc);
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

      if (!s->row->stipple_p)
	s->row->stipple_p = s->stippled_p;

      if (background_width > 0)
	android_draw_glyph_string_bg_rect (s, x, s->y,
					   background_width,
					   s->height);
    }

  s->background_filled_p = true;
}

static void
android_draw_underwave (struct glyph_string *s, int decoration_width)
{
  int wave_height = 3, wave_length = 2;
  int dx, dy, x0, y0, width, x1, y1, x2, y2, xmax;
  bool odd;
  struct android_rectangle wave_clip, string_clip, final_clip;

  dx = wave_length;
  dy = wave_height - 1;
  x0 = s->x;
  y0 = s->ybase + wave_height / 2;
  width = decoration_width;
  xmax = x0 + width;

  /* Find and set clipping rectangle */

  wave_clip.x = x0;
  wave_clip.y = y0;
  wave_clip.width = width;
  wave_clip.height = wave_height;
  get_glyph_string_clip_rect (s, &string_clip);

  if (!gui_intersect_rectangles (&wave_clip, &string_clip, &final_clip))
    return;

  android_set_clip_rectangles (s->gc, 0, 0, &final_clip, 1);

  /* Draw the waves */

  x1 = x0 - (x0 % dx);
  x2 = x1 + dx;
  odd = (x1 / dx) & 1;
  y1 = y2 = y0;

  if (odd)
    y1 += dy;
  else
    y2 += dy;

  if (INT_MAX - dx < xmax)
    emacs_abort ();

  while (x1 <= xmax)
    {
      android_draw_line (FRAME_ANDROID_WINDOW (s->f), s->gc,
			 x1, y1, x2, y2);
      x1  = x2, y1 = y2;
      x2 += dx, y2 = y0 + odd*dy;
      odd = !odd;
    }

  /* Restore previous clipping rectangle(s) */
  android_set_clip_rectangles (s->gc, 0, 0, s->clip, s->num_clips);
}

static void
android_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  android_draw_rectangle (FRAME_ANDROID_WINDOW (s->f),
				  s->gc, x, s->y,
				  g->pixel_width - 1,
				  s->height - 1);
	  x += g->pixel_width;
	}
    }
  else
    {
      struct font *font = s->font;
      int boff = font->baseline_offset;
      int y;

      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, s->f) - boff;

      y = s->ybase - boff;
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	font->driver->draw (s, 0, s->nchars, x, y, false);
      else
	font->driver->draw (s, 0, s->nchars, x, y, true);
      if (s->face->overstrike)
	font->driver->draw (s, 0, s->nchars, x + 1, y, false);
    }
}

static void
android_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->cmp_from == 0)
	android_draw_rectangle (FRAME_ANDROID_WINDOW (s->f),
				s->gc, x, s->y,
				s->width - 1, s->height - 1);
    }
  else if (! s->first_glyph->u.cmp.automatic)
    {
      int y = s->ybase;

      for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	/* TAB in a composition means display glyphs with
	   padding space on the left or right.  */
	if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	  {
	    int xx = x + s->cmp->offsets[j * 2];
	    int yy = y - s->cmp->offsets[j * 2 + 1];

	    font->driver->draw (s, j, j + 1, xx, yy, false);
	    if (s->face->overstrike)
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
	      if (s->face->overstrike)
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
	  if (s->face->overstrike)
	    font->driver->draw (s, j, i, x + 1, y, false);
	}
    }
}

static void
android_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  unsigned char2b[8];
  int x, i, j;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
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
	android_draw_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
				x, s->ybase - glyph->ascent,
				glyph->pixel_width - 1,
				glyph->ascent + glyph->descent - 1);
      x += glyph->pixel_width;
   }

  /* Defend against hypothetical bad code elsewhere that uses
     s->char2b after this function returns.  */
  s->char2b = NULL;
}

static void
android_draw_glyph_string (struct glyph_string *s)
{
  bool relief_drawn_p = false;

  /* If S draws into the background of its successors, draw the
     background of the successors first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
	  {
	    android_set_glyph_string_gc (next);
	    android_set_glyph_string_clipping (next);
	    if (next->first_glyph->type == STRETCH_GLYPH)
	      android_draw_stretch_glyph_string (next);
	    else
	      android_draw_glyph_string_background (next, true);
	    next->num_clips = 0;
	  }
    }

  /* Set up S->gc, set clipping and draw S.  */
  android_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      android_set_glyph_string_clipping (s);
      android_draw_glyph_string_background (s, true);
      android_draw_glyph_string_box (s);
      android_set_glyph_string_clipping (s);
      relief_drawn_p = true;
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
	   && !s->clip_tail
	   && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	       || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    android_set_glyph_string_clipping_exactly (s, s);
  else
    android_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      android_draw_image_glyph_string (s);
      break;

    case XWIDGET_GLYPH:
      emacs_abort ();
      break;

    case STRETCH_GLYPH:
      android_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, false);
      android_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, true);
      android_draw_composite_glyph_string_foreground (s);
      break;

    case GLYPHLESS_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, true);
      android_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  if (!s->for_overlaps)
    {
      int area_x, area_y, area_width, area_height;
      int area_max_x, decoration_width;

      /* Prevent the underline from overwriting surrounding areas
	 and the fringe.  */
      window_box (s->w, s->area, &area_x, &area_y,
		  &area_width, &area_height);
      area_max_x = area_x + area_width - 1;

      decoration_width = s->width;
      if (!s->row->mode_line_p
	  && !s->row->tab_line_p
	  && area_max_x < (s->x + decoration_width - 1))
	decoration_width -= (s->x + decoration_width - 1) - area_max_x;

      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
	android_draw_glyph_string_box (s);

      /* Draw underline.  */
      if (s->face->underline)
        {
          if (s->face->underline == FACE_UNDER_WAVE)
            {
              if (s->face->underline_defaulted_p)
                android_draw_underwave (s, decoration_width);
              else
                {
                  struct android_gc_values xgcv;
                  android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
                  android_set_foreground (s->gc, s->face->underline_color);
                  android_draw_underwave (s, decoration_width);
                  android_set_foreground (s->gc, xgcv.foreground);
                }
            }
          else if (s->face->underline == FACE_UNDER_LINE)
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

		  /* Ignore minimum_offset if the amount of pixels was
		     explicitly specified.  */
		  if (!s->face->underline_pixels_above_descent_line)
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
              if (s->face->underline_defaulted_p)
                android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
					s->x, y, decoration_width, thickness);
              else
                {
                  struct android_gc_values xgcv;
                  android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
                  android_set_foreground (s->gc, s->face->underline_color);
                  android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc,
					  s->x, y, decoration_width, thickness);
                  android_set_foreground (s->gc, xgcv.foreground);
                }
            }
        }
      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f),
				    s->gc, s->x, s->y + dy,
				    decoration_width, h);
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
	      android_set_foreground (s->gc, s->face->overline_color);
	      android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, s->x,
				      s->y + dy, decoration_width, h);
	      android_set_foreground (s->gc, xgcv.foreground);
	    }
	}

      /* Draw strike-through.  */
      if (s->face->strike_through_p)
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

	  if (s->face->strike_through_color_defaulted_p)
	    android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f),
				    s->gc, s->x, glyph_y + dy,
				    s->width, h);
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
	      android_set_foreground (s->gc, s->face->strike_through_color);
	      android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f), s->gc, s->x,
				      glyph_y + dy, decoration_width, h);
	      android_set_foreground (s->gc, xgcv.foreground);
	    }
	}

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
		android_set_glyph_string_gc (prev);
		android_set_glyph_string_clipping_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  android_draw_glyph_string_foreground (prev);
		else
		  android_draw_composite_glyph_string_foreground (prev);
		android_reset_clip_rectangles (prev->f, prev->gc);
		prev->hl = save;
		prev->num_clips = 0;
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
		android_set_glyph_string_gc (next);
		android_set_glyph_string_clipping_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  android_draw_glyph_string_foreground (next);
		else
		  android_draw_composite_glyph_string_foreground (next);
		android_reset_clip_rectangles (next->f, next->gc);
		next->hl = save;
		next->num_clips = 0;
		next->clip_head = s->next;
	      }
	}
    }

  /* Reset clipping.  */
  android_reset_clip_rectangles (s->f, s->gc);
  s->num_clips = 0;

  /* Set the stippled flag that tells redisplay whether or not a
     stipple was actually draw.  */

  if (s->first_glyph->type != STRETCH_GLYPH
      && s->first_glyph->type != IMAGE_GLYPH
      && !s->row->stipple_p)
    s->row->stipple_p = s->stippled_p;
}

static void
android_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  /* Not supported, because cursors are not supported on Android.  */
}

static void
android_clear_frame_area (struct frame *f, int x, int y,
			  int width, int height)
{
  android_clear_area (FRAME_ANDROID_WINDOW (f),
		      x, y, width, height);
}

void
android_clear_under_internal_border (struct frame *f)
{
  if (FRAME_INTERNAL_BORDER_WIDTH (f) > 0)
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

      if (face)
	{
	  unsigned long color = face->background;
	  struct android_gc *gc = f->output_data.android->normal_gc;

	  android_set_foreground (gc, color);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, 0, margin,
				  width, border);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, 0, 0,
				  border, height);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, width - border,
				  0, border, height);
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, 0,
				  height - border, width, border);
	  android_set_foreground (gc, FRAME_FOREGROUND_PIXEL (f));
	}
      else
	{
	  android_clear_area (FRAME_ANDROID_WINDOW (f), 0, 0,
			      border, height);
	  android_clear_area (FRAME_ANDROID_WINDOW (f), 0,
			      margin, width, border);
	  android_clear_area (FRAME_ANDROID_WINDOW (f), width - border,
			      0, border, height);
	  android_clear_area (FRAME_ANDROID_WINDOW (f), 0,
			      height - border, width, border);
	}
    }
}

static void
android_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct android_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  int x, y, wd, h;
  struct android_gc_values xgcv;
  struct glyph *cursor_glyph;
  struct android_gc *gc;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width - 1;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  xgcv.foreground = f->output_data.android->cursor_pixel;
  if (dpyinfo->scratch_cursor_gc)
    android_change_gc (dpyinfo->scratch_cursor_gc,
		       ANDROID_GC_FOREGROUND, &xgcv);
  else
    dpyinfo->scratch_cursor_gc
      =  android_create_gc (ANDROID_GC_FOREGROUND, &xgcv);
  gc = dpyinfo->scratch_cursor_gc;

  /* When on R2L character, show cursor at the right edge of the
     glyph, unless the cursor box is as wide as the glyph or wider
     (the latter happens when x-stretch-cursor is non-nil).  */
  if ((cursor_glyph->resolved_level & 1) != 0
      && cursor_glyph->pixel_width > wd)
    {
      x += cursor_glyph->pixel_width - wd;
      if (wd > 0)
	wd -= 1;
    }
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  android_clip_to_row (w, row, TEXT_AREA, gc);
  android_draw_rectangle (FRAME_ANDROID_WINDOW (f), gc, x, y, wd, h - 1);
  android_reset_clip_rectangles (f, gc);
}

static void
android_draw_bar_cursor (struct window *w, struct glyph_row *row, int width,
			 enum text_cursor_kinds kind)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;
  int cursor_start_y;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Experimental avoidance of cursor on xwidget.  */
  if (cursor_glyph->type == XWIDGET_GLYPH)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *r;
      r = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, r, DRAW_CURSOR);
    }
  else
    {
      struct android_gc *gc = FRAME_DISPLAY_INFO (f)->scratch_cursor_gc;
      unsigned long mask = ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      struct android_gc_values xgcv;

      /* If the glyph's background equals the color we normally draw
	 the bars cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == f->output_data.android->cursor_pixel)
	xgcv.background = xgcv.foreground = face->foreground;
      else
	xgcv.background = xgcv.foreground = f->output_data.android->cursor_pixel;

      if (gc)
	android_change_gc (gc, mask, &xgcv);
      else
	{
          gc = android_create_gc (mask, &xgcv);
	  FRAME_DISPLAY_INFO (f)->scratch_cursor_gc = gc;
	}

      android_clip_to_row (w, row, TEXT_AREA, gc);

      if (kind == BAR_CURSOR)
	{
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, x,
				  WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
				  width, row->height);
	}
      else /* HBAR_CURSOR */
	{
	  int dummy_x, dummy_y, dummy_h;
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);

	  cursor_start_y = WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y
						    + row->height - width);

	  if ((cursor_glyph->resolved_level & 1) != 0
	      && cursor_glyph->pixel_width > w->phys_cursor_width - 1)
	    x += cursor_glyph->pixel_width - w->phys_cursor_width + 1;
	  android_fill_rectangle (FRAME_ANDROID_WINDOW (f), gc, x,
				  cursor_start_y,
				  w->phys_cursor_width - 1, width);
	}

      android_reset_clip_rectangles (f, gc);
    }
}

static void
android_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
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
	      android_draw_hollow_cursor (w, glyph_row);
	      break;

	    case FILLED_BOX_CURSOR:
	      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	      break;

	    case BAR_CURSOR:
	      android_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	      break;

	    case HBAR_CURSOR:
	      android_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
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
android_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    android_set_foreground (f->output_data.android->normal_gc,
			    face->foreground);

  android_draw_line (FRAME_ANDROID_WINDOW (f),
		     f->output_data.android->normal_gc,
		     x, y0, x, y1);
}

static void
android_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
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

  if ((y1 - y0 > x1 - x0) && (x1 - x0 >= 3))
    /* A vertical divider, at least three pixels wide: Draw first and
       last pixels differently.  */
    {
      android_set_foreground (f->output_data.android->normal_gc,
			      color_first);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0, y0, 1, y1 - y0);
      android_set_foreground (f->output_data.android->normal_gc,
			      color);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0 + 1, y0, x1 - x0 - 2, y1 - y0);
      android_set_foreground (f->output_data.android->normal_gc,
			      color_last);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x1 - 1, y0, 1, y1 - y0);
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first
       and last pixels differently.  */
    {
      android_set_foreground (f->output_data.android->normal_gc,
			      color_first);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0, y0, x1 - x0, 1);
      android_set_foreground (f->output_data.android->normal_gc, color);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0, y0 + 1, x1 - x0, y1 - y0 - 2);
      android_set_foreground (f->output_data.android->normal_gc,
			      color_last);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0, y1 - 1, x1 - x0, 1);
    }
  else
    {
      /* In any other case do not draw the first and last pixels
	 differently.  */
      android_set_foreground (f->output_data.android->normal_gc, color);
      android_fill_rectangle (FRAME_ANDROID_WINDOW (f),
			      f->output_data.android->normal_gc,
			      x0, y0, x1 - x0, y1 - y0);
    }
}



extern frame_parm_handler android_frame_parm_handlers[];

#endif /* !ANDROID_STUBIFY */

static struct redisplay_interface android_redisplay_interface =
  {
#ifndef ANDROID_STUBIFY
    android_frame_parm_handlers,
    gui_produce_glyphs,
    gui_write_glyphs,
    gui_insert_glyphs,
    gui_clear_end_of_line,
    android_scroll_run,
    android_after_update_window_line,
    NULL, /* update_window_begin */
    NULL, /* update_window_end   */
    android_flip_and_flush,
    gui_clear_window_mouse_face,
    gui_get_glyph_overhangs,
    gui_fix_overlapping_area,
    android_draw_fringe_bitmap,
    NULL, /* define_fringe_bitmap */
    NULL, /* destroy_fringe_bitmap */
    android_compute_glyph_string_overhangs,
    android_draw_glyph_string,
    android_define_frame_cursor,
    android_clear_frame_area,
    android_clear_under_internal_border,
    android_draw_window_cursor,
    android_draw_vertical_window_border,
    android_draw_window_divider,
    NULL,
    NULL,
    NULL,
    android_default_font_parameter,
#endif
  };



void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
  /* This cannot be implemented on Android, and as such is left
     blank.  */
}

char *
get_keysym_name (int keysym)
{
  return (char *) "UNKNOWN KEYSYM";
}



/* Create a struct terminal, initialize it with the Android specific
   functions and make DISPLAY->TERMINAL point to it.  */

static struct terminal *
android_create_terminal (struct android_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_android,
			      &android_redisplay_interface);
  terminal->display_info.android = dpyinfo;
  dpyinfo->terminal = terminal;

  /* kboard is initialized in android_term_init.  */

#ifndef ANDROID_STUBIFY

  terminal->clear_frame_hook = android_clear_frame;
  terminal->ring_bell_hook = android_ring_bell;
  terminal->toggle_invisible_pointer_hook
    = android_toggle_invisible_pointer;
  terminal->update_begin_hook = android_update_begin;
  terminal->update_end_hook = android_update_end;
  terminal->read_socket_hook = android_read_socket;
  terminal->frame_up_to_date_hook = android_frame_up_to_date;
  terminal->buffer_flipping_unblocked_hook
    = android_buffer_flipping_unblocked_hook;
  terminal->defined_color_hook = android_defined_color;
  terminal->query_frame_background_color
    = android_query_frame_background_color;
  terminal->query_colors = android_query_colors;
  terminal->mouse_position_hook = android_mouse_position;
  terminal->get_focus_frame = android_get_focus_frame;
  terminal->focus_frame_hook = android_focus_frame;
  terminal->frame_rehighlight_hook = android_frame_rehighlight_hook;
  terminal->frame_raise_lower_hook = android_frame_raise_lower;
  terminal->frame_visible_invisible_hook
    = android_make_frame_visible_invisible;
  terminal->fullscreen_hook = android_fullscreen_hook;
  terminal->iconify_frame_hook = android_iconify_frame;
  terminal->set_window_size_hook = android_set_window_size;
  terminal->set_frame_offset_hook = android_set_offset;
  terminal->set_frame_alpha_hook = android_set_alpha;
  terminal->set_new_font_hook = android_new_font;
  terminal->set_bitmap_icon_hook = android_bitmap_icon;
  terminal->implicit_set_name_hook = android_implicitly_set_name;
  /* terminal->menu_show_hook = android_menu_show; XXX */
  terminal->change_tab_bar_height_hook = android_change_tab_bar_height;
  terminal->change_tool_bar_height_hook = android_change_tool_bar_height;
  /* terminal->set_vertical_scroll_bar_hook */
  /*   = android_set_vertical_scroll_bar; */
  /* terminal->set_horizontal_scroll_bar_hook */
  /*   = android_set_horizontal_scroll_bar; */
  terminal->set_scroll_bar_default_width_hook
    = android_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook
    = android_set_scroll_bar_default_height;
  /* terminal->condemn_scroll_bars_hook = android_condemn_scroll_bars; */
  /* terminal->redeem_scroll_bars_hook = android_redeem_scroll_bars; */
  /* terminal->judge_scroll_bars_hook = android_judge_scroll_bars; */
  terminal->free_pixmap = android_free_pixmap_hook;
  terminal->delete_frame_hook = android_delete_frame;
  terminal->delete_terminal_hook = android_delete_terminal;

#else
  emacs_abort ();
#endif

  return terminal;
}

/* Initialize the Android terminal interface.  The display connection
   has already been set up by the system at this point.  */

void
android_term_init (void)
{
  struct terminal *terminal;
  struct android_display_info *dpyinfo;
  Lisp_Object color_file, color_map;

  dpyinfo = xzalloc (sizeof *dpyinfo);
  terminal = android_create_terminal (dpyinfo);
  terminal->kboard = allocate_kboard (Qandroid);
  terminal->kboard->reference_count++;

  dpyinfo->n_planes = 24;

  /* This function should only be called once at startup.  */
  eassert (!x_display_list);
  x_display_list = dpyinfo;

  dpyinfo->name_list_element
    = Fcons (build_pure_c_string ("android"), Qnil);

  color_file = Fexpand_file_name (build_string ("rgb.txt"),
				  Vdata_directory);
  color_map = Fx_load_color_file (color_file);

  if (NILP (color_map))
    fatal ("Could not read %s.\n", SDATA (color_file));

  dpyinfo->color_map = color_map;

#ifndef ANDROID_STUBIFY

  dpyinfo->resx = android_pixel_density_x;
  dpyinfo->resy = android_pixel_density_y;

#endif

  /* https://lists.gnu.org/r/emacs-devel/2015-11/msg00194.html  */
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  terminal->name = xstrdup ("android");

  /* The display "connection" is now set up, and it must never go
     away.  */
  terminal->reference_count = 30000;

  /* Set the baud rate to the same value it gets set to on X.  */
  baud_rate = 19200;
}



void
syms_of_androidterm (void)
{
  Fprovide (Qandroid, Qnil);

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_use_underline_position_properties = true;
  DEFSYM (Qx_use_underline_position_properties,
	  "x-use-underline-position-properties");

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_underline_at_descent_line = false;
  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");
}

void
mark_androidterm (void)
{
  if (x_display_list)
    mark_object (x_display_list->color_map);
}
