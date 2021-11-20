/* Haiku window system support
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#include <math.h>

#include "lisp.h"
#include "frame.h"
#include "blockinput.h"
#include "termchar.h"
#include "font.h"
#include "keyboard.h"
#include "buffer.h"
#include "dispextern.h"

#include "haikugui.h"
#include "haikuterm.h"
#include "haiku_support.h"
#include "termhooks.h"

#include <stdlib.h>

#include <kernel/OS.h>

#define RGB_TO_ULONG(r, g, b) \
  (((r) << 16) | ((g) << 8) | (b));
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)

/* The frame of the currently visible tooltip.  */
static Lisp_Object tip_frame;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
static Window tip_window;

/* A timer that hides or deletes the currently visible tooltip when it
   fires.  */
static Lisp_Object tip_timer;

/* STRING argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_string;

/* Normalized FRAME argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_frame;

/* PARMS argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_parms;

static void
haiku_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval);
static void
haiku_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name);

static ptrdiff_t image_cache_refcount;

static Lisp_Object
get_geometry_from_preferences (struct haiku_display_info *dpyinfo,
                               Lisp_Object parms)
{
  struct {
    const char *val;
    const char *cls;
    Lisp_Object tem;
  } r[] = {
    { "width",  "Width", Qwidth },
    { "height", "Height", Qheight },
    { "left", "Left", Qleft },
    { "top", "Top", Qtop },
  };

  int i;
  for (i = 0; i < ARRAYELTS (r); ++i)
    {
      if (NILP (Fassq (r[i].tem, parms)))
        {
          Lisp_Object value
            = gui_display_get_arg (dpyinfo, parms, r[i].tem, r[i].val, r[i].cls,
                                   RES_TYPE_NUMBER);
          if (! EQ (value, Qunbound))
            parms = Fcons (Fcons (r[i].tem, value), parms);
        }
    }

  return parms;
}

void
haiku_change_tool_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TOOL_BAR_HEIGHT (f);
  int lines = (height + unit - 1) / unit;
  Lisp_Object fullscreen = get_frame_param (f, Qfullscreen);

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  FRAME_TOOL_BAR_HEIGHT (f) = height;
  FRAME_TOOL_BAR_LINES (f) = lines;
  store_frame_param (f, Qtool_bar_lines, make_fixnum (lines));

  if (FRAME_HAIKU_WINDOW (f) && FRAME_TOOL_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tool_bar_window))
    clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);

  if (!f->tool_bar_resized)
    {
      /* As long as tool_bar_resized is false, effectively try to change
	 F's native height.  */
      if (NILP (fullscreen) || EQ (fullscreen, Qfullwidth))
	adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
			   1, false, Qtool_bar_lines);
      else
	adjust_frame_size (f, -1, -1, 4, false, Qtool_bar_lines);

      f->tool_bar_resized =  f->tool_bar_redisplayed;
    }
  else
    /* Any other change may leave the native size of F alone.  */
    adjust_frame_size (f, -1, -1, 3, false, Qtool_bar_lines);

  /* adjust_frame_size might not have done anything, garbage frame
     here.  */
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);

  if (FRAME_HAIKU_WINDOW (f))
    haiku_clear_under_internal_border (f);
}

void
haiku_change_tab_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TAB_BAR_HEIGHT (f);
  int lines = (height + unit - 1) / unit;
  Lisp_Object fullscreen = get_frame_param (f, Qfullscreen);

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  /* Recalculate tab bar and frame text sizes.  */
  FRAME_TAB_BAR_HEIGHT (f) = height;
  FRAME_TAB_BAR_LINES (f) = lines;
  store_frame_param (f, Qtab_bar_lines, make_fixnum (lines));

  if (FRAME_HAIKU_WINDOW (f) && FRAME_TAB_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tab_bar_window))
    clear_glyph_matrix (XWINDOW (f->tab_bar_window)->current_matrix);

  if (!f->tab_bar_resized)
    {
      /* As long as tab_bar_resized is false, effectively try to change
	 F's native height.  */
      if (NILP (fullscreen) || EQ (fullscreen, Qfullwidth))
	adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
			   1, false, Qtab_bar_lines);
      else
	adjust_frame_size (f, -1, -1, 4, false, Qtab_bar_lines);

      f->tab_bar_resized = f->tab_bar_redisplayed;
    }
  else
    /* Any other change may leave the native size of F alone.  */
    adjust_frame_size (f, -1, -1, 3, false, Qtab_bar_lines);

  /* adjust_frame_size might not have done anything, garbage frame
     here.  */
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);
  if (FRAME_HAIKU_WINDOW (f))
    haiku_clear_under_internal_border (f);
}

static void
haiku_set_no_focus_on_map (struct frame *f, Lisp_Object value,
			   Lisp_Object oldval)
{
  if (!EQ (value, oldval))
    FRAME_NO_FOCUS_ON_MAP (f) = !NILP (value);
}

static void
haiku_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  if (FRAME_TOOLTIP_P (f))
    return;
  int nlines;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  haiku_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}

static void
haiku_set_tab_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  if (FRAME_TOOLTIP_P (f))
    return;
  int olines = FRAME_TAB_BAR_LINES (f);
  int nlines;

  /* Treat tab bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  if (nlines != olines && (olines == 0 || nlines == 0))
    haiku_change_tab_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}


int
haiku_get_color (const char *name, Emacs_Color *color)
{
  unsigned short r16, g16, b16;
  Lisp_Object tem;

  if (parse_color_spec (name, &r16, &g16, &b16))
    {
      color->pixel = RGB_TO_ULONG (r16 / 256, g16 / 256, b16 / 256);
      color->red = r16;
      color->green = g16;
      color->blue = b16;
      return 0;
    }
  else
    {
      block_input ();
      eassert (x_display_list && !NILP (x_display_list->color_map));
      tem = x_display_list->color_map;
      for (; CONSP (tem); tem = XCDR (tem))
	{
	  Lisp_Object col = XCAR (tem);
	  if (CONSP (col) && !xstrcasecmp (SSDATA (XCAR (col)), name))
	    {
	      int32_t clr = XFIXNUM (XCDR (col));
	      color->pixel = clr;
	      color->red = RED_FROM_ULONG (clr) * 257;
	      color->green = GREEN_FROM_ULONG (clr) * 257;
	      color->blue = BLUE_FROM_ULONG (clr) * 257;
	      unblock_input ();
	      return 0;
	  }
	}

      unblock_input ();
    }

  return 1;
}

static struct haiku_display_info *
haiku_display_info_for_name (Lisp_Object name)
{
  CHECK_STRING (name);

  if (!NILP (Fstring_equal (name, build_string ("be"))))
    {
      if (!x_display_list)
	return x_display_list;

      error ("Be windowing not initialized");
    }

  error ("Be displays can only be named \"be\"");
}

static struct haiku_display_info *
check_haiku_display_info (Lisp_Object object)
{
  struct haiku_display_info *dpyinfo = NULL;

  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_HAIKU_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_DISPLAY_INFO (sf);
      else if (x_display_list)
	dpyinfo = x_display_list;
      else
	error ("Be windowing not present");
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type != output_haiku)
	error ("Terminal %d is not a Be display", t->id);

      dpyinfo = t->display_info.haiku;
    }
  else if (STRINGP (object))
    dpyinfo = haiku_display_info_for_name (object);
  else
    {
      struct frame *f = decode_window_system_frame (object);
      dpyinfo = FRAME_DISPLAY_INFO (f);
    }

  return dpyinfo;
}

static void
haiku_set_title_bar_text (struct frame *f, Lisp_Object text)
{
  if (FRAME_HAIKU_WINDOW (f))
    {
      block_input ();
      BWindow_retitle (FRAME_HAIKU_WINDOW (f), SSDATA (ENCODE_UTF_8 (text)));
      unblock_input ();
    }
}

static void
haiku_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 26;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;

  haiku_set_title_bar_text (f, name);
}

static void
haiku_set_child_frame_border_width (struct frame *f,
				    Lisp_Object arg, Lisp_Object oldval)
{
  int border;

  if (NILP (arg))
    border = -1;
  else if (RANGED_FIXNUMP (0, arg, INT_MAX))
    border = XFIXNAT (arg);
  else
    signal_error ("Invalid child frame border width", arg);

  if (border != FRAME_CHILD_FRAME_BORDER_WIDTH (f))
    {
      f->child_frame_border_width = border;

      if (FRAME_HAIKU_WINDOW (f))
	adjust_frame_size (f, -1, -1, 3, 0, Qchild_frame_border_width);

      SET_FRAME_GARBAGED (f);
    }
}

static void
haiku_set_parent_frame (struct frame *f,
			Lisp_Object new_value, Lisp_Object old_value)
{
  struct frame *p = NULL;
  block_input ();
  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_HAIKU_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      unblock_input ();
      error ("Invalid specification of `parent-frame'");
    }

  if (EQ (new_value, old_value))
    {
      unblock_input ();
      return;
    }

  if (!NILP (old_value))
    EmacsWindow_unparent (FRAME_HAIKU_WINDOW (f));
  if (!NILP (new_value))
    {
      EmacsWindow_parent_to (FRAME_HAIKU_WINDOW (f),
			     FRAME_HAIKU_WINDOW (p));
      BWindow_set_offset (FRAME_HAIKU_WINDOW (f),
			  f->left_pos, f->top_pos);
    }
  fset_parent_frame (f, new_value);
  unblock_input ();
}

static void
haiku_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  haiku_set_name (f, arg, 1);
}

static void
haiku_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  block_input ();
  if (!EQ (new_value, old_value))
    FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);

  if (FRAME_HAIKU_WINDOW (f))
    {
      BWindow_set_avoid_focus (FRAME_HAIKU_WINDOW (f),
			       FRAME_NO_ACCEPT_FOCUS (f));
    }
  unblock_input ();
}

static void
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is already dead, nothing to do.  This can happen if the
     display is disconnected after the frame has become official, but
     before x_create_frame removes the unwind protect.  */
  if (!FRAME_LIVE_P (f))
    return;

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      struct haiku_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

      /* If the frame's image cache refcount is still the same as our
	 private shadow variable, it means we are unwinding a frame
	 for which we didn't yet call init_frame_faces, where the
	 refcount is incremented.  Therefore, we increment it here, so
	 that free_frame_faces, called in free_frame_resources later,
	 will not mistakenly decrement the counter that was not
	 incremented yet to account for this new frame.  */
      if (FRAME_IMAGE_CACHE (f) != NULL
	  && FRAME_IMAGE_CACHE (f)->refcount == image_cache_refcount)
	FRAME_IMAGE_CACHE (f)->refcount++;

      haiku_free_frame_resources (f);
      free_glyphs (f);

#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      /* Check that reference counts are indeed correct.  */
      if (dpyinfo->terminal->image_cache)
	eassert (dpyinfo->terminal->image_cache->refcount == image_cache_refcount);
#endif
    }
}

static void
unwind_create_tip_frame (Lisp_Object frame)
{
  unwind_create_frame (frame);
  tip_window = NULL;
  tip_frame = Qnil;
}

static void
haiku_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct haiku_output *output = FRAME_OUTPUT_DATA (f);
  unsigned long old_fg;

  Emacs_Color color;

  if (haiku_get_color (SSDATA (arg), &color))
    {
      store_frame_param (f, Qforeground_color, oldval);
      unblock_input ();
      error ("Bad color");
    }

  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = color.pixel;

  if (FRAME_HAIKU_WINDOW (f))
    {

      block_input ();
      if (output->cursor_color.pixel == old_fg)
	{
	  output->cursor_color.pixel = old_fg;
	  output->cursor_color.red = RED_FROM_ULONG (old_fg);
	  output->cursor_color.green = GREEN_FROM_ULONG (old_fg);
	  output->cursor_color.blue = BLUE_FROM_ULONG (old_fg);
	}

      unblock_input ();

      update_face_from_frame_parameter (f, Qforeground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

static void
unwind_popup (void)
{
  if (!popup_activated_p)
    emacs_abort ();
  --popup_activated_p;
}

static Lisp_Object
haiku_create_frame (Lisp_Object parms, int ttip_p)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only = false;
  bool face_change_before = face_change;
  long window_prompting = 0;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct haiku_display_info *dpyinfo = NULL;
  struct kboard *kb;

  parms = Fcopy_alist (parms);

  Vx_resource_name = Vinvocation_name;

  display = gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0,
                                 RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_haiku_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = gui_display_get_arg (dpyinfo, parms, Qname, 0, 0,
                              RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  block_input ();

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  tem = gui_display_get_arg (dpyinfo, parms, Qminibuffer,
                             "minibuffer", "Minibuffer",
                             RES_TYPE_SYMBOL);
  if (ttip_p)
    f = make_frame (0);
  else if (EQ (tem, Qnone) || NILP (tem))
      f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (WINDOWP (tem))
      f = make_frame_without_minibuffer (tem, kb, display);
  else
      f = make_frame (1);
  XSETFRAME (frame, f);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_haiku;
  f->output_data.haiku = xzalloc (sizeof *f->output_data.haiku);

  f->output_data.haiku->pending_zoom_x = INT_MIN;
  f->output_data.haiku->pending_zoom_y = INT_MIN;
  f->output_data.haiku->pending_zoom_width = INT_MIN;
  f->output_data.haiku->pending_zoom_height = INT_MIN;

  if (ttip_p)
    f->wants_modeline = false;

  fset_icon_name (f, gui_display_get_arg (dpyinfo, parms, Qicon_name,
                                          "iconName", "Title",
                                          RES_TYPE_STRING));
  if (! STRINGP (f->icon_name) || ttip_p)
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  if (!ttip_p)
    record_unwind_protect (unwind_create_frame, frame);
  else
    record_unwind_protect (unwind_create_tip_frame, frame);

  FRAME_OUTPUT_DATA (f)->parent_desc = NULL;
  FRAME_OUTPUT_DATA (f)->explicit_parent = 0;

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name) || ! STRINGP (name))
    {
      fset_name (f, Vinvocation_name);
      f->explicit_name = 0;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = 1;
      specbind (Qx_resource_name, name);
    }

#ifdef USE_BE_CAIRO
  register_font_driver (&ftcrfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&ftcrhbfont_driver, f);
#endif
#endif
  register_font_driver (&haikufont_driver, f);

  f->tooltip = ttip_p;

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  FRAME_RIF (f)->default_font_parameter (f, parms);

  unblock_input ();

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                         "borderwidth", "BorderWidth", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qinternal_border_width, make_fixnum (ttip_p ? 1 : 2),
                         "internalBorderWidth", "InternalBorderWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qchild_frame_border_width, Qnil,
			 "childFrameBorderWidth", "childFrameBorderWidth",
			 RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qvertical_scroll_bars, !ttip_p ? Qt : Qnil,
			 "verticalScrollBars", "VerticalScrollBars",
			 RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil,
                         "horizontalScrollBars", "HorizontalScrollBars",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qforeground_color, build_string ("black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("white"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qline_spacing, Qnil,
                         "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qleft_fringe, Qnil,
                         "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_fringe, Qnil,
                         "rightFringe", "RightFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qno_special_glyphs, ttip_p ? Qnil : Qt,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  init_frame_faces (f);

  /* Read comment about this code in corresponding place in xfns.c.  */
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_width, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_height, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_height, tem);
  adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		     FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 5, 1,
		     Qx_create_frame_1);

  if (!ttip_p)
    {
      gui_default_parameter (f, parms, Qz_group, Qnil, NULL, NULL, RES_TYPE_SYMBOL);
      gui_default_parameter (f, parms, Qno_focus_on_map, Qnil,
			     NULL, NULL, RES_TYPE_BOOLEAN);
      gui_default_parameter (f, parms, Qno_accept_focus, Qnil,
			     NULL, NULL, RES_TYPE_BOOLEAN);

      /* The resources controlling the menu-bar, tool-bar, and tab-bar are
	 processed specially at startup, and reflected in the mode
	 variables; ignore them here.  */
      gui_default_parameter (f, parms, Qmenu_bar_lines,
			     NILP (Vmenu_bar_mode)
			     ? make_fixnum (0) : make_fixnum (1),
			     NULL, NULL, RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qtab_bar_lines,
			     NILP (Vtab_bar_mode)
			     ? make_fixnum (0) : make_fixnum (1),
			     NULL, NULL, RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qtool_bar_lines,
			     NILP (Vtool_bar_mode)
			     ? make_fixnum (0) : make_fixnum (1),
			     NULL, NULL, RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qbuffer_predicate, Qnil, "bufferPredicate",
			     "BufferPredicate", RES_TYPE_SYMBOL);
      gui_default_parameter (f, parms, Qtitle, Qnil, "title", "Title",
			     RES_TYPE_STRING);
    }

  parms = get_geometry_from_preferences (dpyinfo, parms);
  window_prompting = gui_figure_window_size (f, parms, false, true);

  if (ttip_p)
    {
       /* No fringes on tip frame.  */
      f->fringe_cols = 0;
      f->left_fringe_width = 0;
      f->right_fringe_width = 0;
      /* No dividers on tip frame.  */
      f->right_divider_width = 0;
      f->bottom_divider_width = 0;
    }

  tem = gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0,
                             RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || (!EQ (tem, Qunbound) && !NILP (tem));

  /* Add `tooltip' frame parameter's default value.  */
  if (NILP (Fframe_parameter (frame, Qtooltip)) && ttip_p)
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtooltip, Qt), Qnil));

#define ASSIGN_CURSOR(cursor, be_cursor) \
  (FRAME_OUTPUT_DATA (f)->cursor = be_cursor)

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

  ASSIGN_CURSOR (current_cursor, FRAME_OUTPUT_DATA (f)->text_cursor);
#undef ASSIGN_CURSOR


  if (ttip_p)
    f->no_split = true;
  f->terminal->reference_count++;

  FRAME_OUTPUT_DATA (f)->window = BWindow_new (&FRAME_OUTPUT_DATA (f)->view);
  if (!FRAME_OUTPUT_DATA (f)->window)
    xsignal1 (Qerror, build_unibyte_string ("Could not create window"));

  if (!minibuffer_only && !ttip_p && FRAME_EXTERNAL_MENU_BAR (f))
    initialize_frame_menubar (f);

  FRAME_OUTPUT_DATA (f)->window_desc = FRAME_OUTPUT_DATA (f)->window;

  Vframe_list = Fcons (frame, Vframe_list);

  Lisp_Object parent_frame = gui_display_get_arg (dpyinfo, parms, Qparent_frame, NULL, NULL,
						  RES_TYPE_SYMBOL);

  if (EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  if (!NILP (parent_frame))
    haiku_set_parent_frame (f, parent_frame, Qnil);

  gui_default_parameter (f, parms, Qundecorated, Qnil, NULL, NULL, RES_TYPE_BOOLEAN);

  gui_default_parameter (f, parms, Qicon_type, Qnil,
                         "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);
  if (ttip_p)
    {
      gui_default_parameter (f, parms, Qundecorated, Qt, NULL, NULL, RES_TYPE_BOOLEAN);
      gui_default_parameter (f, parms, Qno_accept_focus, Qt, NULL, NULL,
			     RES_TYPE_BOOLEAN);
    }
  else
    {
      gui_default_parameter (f, parms, Qauto_raise, Qnil,
			     "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
      gui_default_parameter (f, parms, Qauto_lower, Qnil,
			     "autoLower", "AutoLower", RES_TYPE_BOOLEAN);
      gui_default_parameter (f, parms, Qcursor_type, Qbox,
			     "cursorType", "CursorType", RES_TYPE_SYMBOL);
      gui_default_parameter (f, parms, Qscroll_bar_width, Qnil,
			     "scrollBarWidth", "ScrollBarWidth",
			     RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qscroll_bar_height, Qnil,
			     "scrollBarHeight", "ScrollBarHeight",
			     RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qalpha, Qnil,
			     "alpha", "Alpha", RES_TYPE_NUMBER);
      gui_default_parameter (f, parms, Qfullscreen, Qnil,
			     "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);
    }

  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
			 "inhibitDoubleBuffering", "InhibitDoubleBuffering",
			 RES_TYPE_BOOLEAN);

  if (ttip_p)
    {
      Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

      call2 (Qface_set_after_frame_default, frame, Qnil);

      if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
	{
	  AUTO_FRAME_ARG (arg, Qbackground_color, bg);
	  Fmodify_frame_parameters (frame, arg);
	}
    }

  if (ttip_p)
    face_change = face_change_before;

  f->can_set_window_size = true;

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, ttip_p ? Qtip_frame : Qx_create_frame_2);

  if (!FRAME_OUTPUT_DATA (f)->explicit_parent && !ttip_p)
    {
      Lisp_Object visibility;

      visibility = gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
                                        RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;
      if (EQ (visibility, Qicon))
	haiku_iconify_frame (f);
      else if (!NILP (visibility))
	haiku_visualize_frame (f);
      else /* Qnil */
	{
	  f->was_invisible = true;
	}
    }

  if (!ttip_p)
    {
      if (FRAME_HAS_MINIBUF_P (f)
	  && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
	      || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
	kset_default_minibuffer_frame (kb, frame);
    }

  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  if (window_prompting & (USPosition | PPosition))
    haiku_set_offset (f, f->left_pos, f->top_pos, 1);
  else
    BWindow_center_on_screen (FRAME_HAIKU_WINDOW (f));

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  if (ttip_p)
    adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		       0, true, Qtip_frame);

  return unbind_to (count, frame);
}

static void
compute_tip_xy (struct frame *f,
		Lisp_Object parms, Lisp_Object dx, Lisp_Object dy,
		int width, int height, int *root_x, int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int min_x = 0, min_y = 0, max_x = 0, max_y = 0;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if ((!FIXNUMP (left) && !FIXNUMP (right))
      || (!FIXNUMP (top) && !FIXNUMP (bottom)))
    {
      int x, y;

      /* Default min and max values.  */
      min_x = 0;
      min_y = 0;
      BScreen_px_dim (&max_x, &max_y);

      block_input ();
      BView_get_mouse (FRAME_HAIKU_VIEW (f), &x, &y);
      BView_convert_to_screen (FRAME_HAIKU_VIEW (f), &x, &y);
      *root_x = x;
      *root_y = y;
      unblock_input ();
    }

  if (FIXNUMP (top))
    *root_y = XFIXNUM (top);
  else if (FIXNUMP (bottom))
    *root_y = XFIXNUM (bottom) - height;
  else if (*root_y + XFIXNUM (dy) <= min_y)
    *root_y = min_y; /* Can happen for negative dy */
  else if (*root_y + XFIXNUM (dy) + height <= max_y)
    /* It fits below the pointer */
      *root_y += XFIXNUM (dy);
  else if (height + XFIXNUM (dy) + min_y <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XFIXNUM (dy);
  else
    /* Put it on the top.  */
    *root_y = min_y;

  if (FIXNUMP (left))
    *root_x = XFIXNUM (left);
  else if (FIXNUMP (right))
    *root_x = XFIXNUM (right) - width;
  else if (*root_x + XFIXNUM (dx) <= min_x)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XFIXNUM (dx) + width <= max_x)
    /* It fits to the right of the pointer.  */
    *root_x += XFIXNUM (dx);
  else if (width + XFIXNUM (dx) + min_x <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XFIXNUM (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = min_x;
}

static Lisp_Object
haiku_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      call1 (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

  Lisp_Object it, frame;
  FOR_EACH_FRAME (it, frame)
    if (FRAME_WINDOW_P (XFRAME (frame)) &&
	FRAME_HAIKU_VIEW (XFRAME (frame)))
      BView_set_tooltip (FRAME_HAIKU_VIEW (XFRAME (frame)), NULL);

  if (NILP (tip_frame)
      || (!delete && !NILP (tip_frame)
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    return Qnil;
  else
    {
      ptrdiff_t count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      if (!NILP (tip_frame))
	{
	  if (FRAME_LIVE_P (XFRAME (tip_frame)))
	    {
	      if (delete)
		{
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		haiku_unvisualize_frame (XFRAME (tip_frame));

	      was_open = Qt;
	    }
	  else
	    tip_frame = Qnil;
	}
      else
	tip_frame = Qnil;

      return unbind_to (count, was_open);
    }
}

static void
haiku_set_undecorated (struct frame *f, Lisp_Object new_value,
		       Lisp_Object old_value)
{
  if (EQ (new_value, old_value))
    return;

  block_input ();
  FRAME_UNDECORATED (f) = !NILP (new_value);
  BWindow_change_decoration (FRAME_HAIKU_WINDOW (f), NILP (new_value));
  unblock_input ();
}

static void
haiku_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  if (FRAME_TOOLTIP_P (f))
    return;
  int nlines;
  if (TYPE_RANGED_FIXNUMP (int, value))
    nlines = XFIXNUM (value);
  else
    nlines = 0;

  fset_redisplay (f);

  FRAME_MENU_BAR_LINES (f) = 0;
  FRAME_MENU_BAR_HEIGHT (f) = 0;

  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      if (FRAME_HAIKU_P (f) && !FRAME_HAIKU_MENU_BAR (f))
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = 1;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f))
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
      if (FRAME_HAIKU_P (f))
	FRAME_HAIKU_MENU_BAR (f) = 0;
    }

  adjust_frame_glyphs (f);
}

/* Return geometric attributes of FRAME.  According to the value of
   ATTRIBUTES return the outer edges of FRAME (Qouter_edges), the inner
   edges of FRAME, the root window edges of frame (Qroot_edges).  Any
   other value means to return the geometry as returned by
   Fx_frame_geometry.  */
static Lisp_Object
frame_geometry (Lisp_Object frame, Lisp_Object attribute)
{
  struct frame *f = decode_live_frame (frame);
  check_window_system (f);

  if (EQ (attribute, Qouter_edges))
    return list4i (f->left_pos, f->top_pos,
		   f->left_pos, f->top_pos);
  else if (EQ (attribute, Qnative_edges))
    return list4i (f->left_pos, f->top_pos,
		   f->left_pos + FRAME_PIXEL_WIDTH (f),
		   f->top_pos + FRAME_PIXEL_HEIGHT (f));
  else if (EQ (attribute, Qinner_edges))
    return list4i (f->left_pos + FRAME_INTERNAL_BORDER_WIDTH (f),
		   f->top_pos + FRAME_INTERNAL_BORDER_WIDTH (f) +
		   FRAME_MENU_BAR_HEIGHT (f) + FRAME_TOOL_BAR_HEIGHT (f),
		   f->left_pos - FRAME_INTERNAL_BORDER_WIDTH (f) +
		   FRAME_PIXEL_WIDTH (f),
		   f->top_pos + FRAME_PIXEL_HEIGHT (f) -
		   FRAME_INTERNAL_BORDER_WIDTH (f));

  else
    return
      list (Fcons (Qouter_position,
		   Fcons (make_fixnum (f->left_pos),
			  make_fixnum (f->top_pos))),
	    Fcons (Qouter_size,
		   Fcons (make_fixnum (FRAME_PIXEL_WIDTH (f)),
			  make_fixnum (FRAME_PIXEL_HEIGHT (f)))),
	    Fcons (Qexternal_border_size,
		   Fcons (make_fixnum (0), make_fixnum (0))),
	    Fcons (Qtitle_bar_size,
		   Fcons (make_fixnum (0), make_fixnum (0))),
	    Fcons (Qmenu_bar_external, Qnil),
	    Fcons (Qmenu_bar_size, Fcons (make_fixnum (FRAME_PIXEL_WIDTH (f) -
						       (FRAME_INTERNAL_BORDER_WIDTH (f) * 2)),
					  make_fixnum (FRAME_MENU_BAR_HEIGHT (f)))),
	    Fcons (Qtool_bar_external, Qnil),
	    Fcons (Qtool_bar_position, Qtop),
	    Fcons (Qtool_bar_size, Fcons (make_fixnum (FRAME_PIXEL_WIDTH (f) -
						       (FRAME_INTERNAL_BORDER_WIDTH (f) * 2)),
					  make_fixnum (FRAME_TOOL_BAR_HEIGHT (f)))),
	    Fcons (Qinternal_border_width, make_fixnum (FRAME_INTERNAL_BORDER_WIDTH (f))));
}

void
haiku_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  CHECK_STRING (arg);

  block_input ();
  Emacs_Color color;

  if (haiku_get_color (SSDATA (arg), &color))
    {
      store_frame_param (f, Qbackground_color, oldval);
      unblock_input ();
      error ("Bad color");
    }

  FRAME_OUTPUT_DATA (f)->cursor_fg = color.pixel;
  FRAME_BACKGROUND_PIXEL (f) = color.pixel;

  if (FRAME_HAIKU_VIEW (f))
    {
      struct face *defface;

      BView_draw_lock (FRAME_HAIKU_VIEW (f));
      BView_SetViewColor (FRAME_HAIKU_VIEW (f), color.pixel);
      BView_draw_unlock (FRAME_HAIKU_VIEW (f));

      defface = FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID);
      if (defface)
	{
	  defface->background = color.pixel;
	  update_face_from_frame_parameter (f, Qbackground_color, arg);
	  clear_frame (f);
	}
    }

  if (FRAME_VISIBLE_P (f))
    SET_FRAME_GARBAGED (f);
  unblock_input ();
}

void
haiku_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  CHECK_STRING (arg);

  block_input ();
  Emacs_Color color;

  if (haiku_get_color (SSDATA (arg), &color))
    {
      store_frame_param (f, Qcursor_color, oldval);
      unblock_input ();
      error ("Bad color");
    }

  FRAME_CURSOR_COLOR (f) = color;
  if (FRAME_VISIBLE_P (f))
    {
      gui_update_cursor (f, 0);
      gui_update_cursor (f, 1);
    }
  update_face_from_frame_parameter (f, Qcursor_color, arg);
  unblock_input ();
}

void
haiku_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

unsigned long
haiku_get_pixel (haiku bitmap, int x, int y)
{
  unsigned char *data;
  int32_t bytes_per_row;
  int mono_p;
  int left;
  int right;
  int top;
  int bottom;

  data = BBitmap_data (bitmap);
  BBitmap_dimensions (bitmap, &left, &top, &right, &bottom,
		      &bytes_per_row, &mono_p);

  if (x < left || x > right || y < top || y > bottom)
    emacs_abort ();

  if (!mono_p)
    return ((uint32_t *) (data + (bytes_per_row * y)))[x];

  int byte = y * bytes_per_row + x / 8;
  return data[byte] & (1 << (x % 8));
}

void
haiku_put_pixel (haiku bitmap, int x, int y, unsigned long pixel)
{
  unsigned char *data;
  int32_t bytes_per_row;
  int mono_p;
  int left;
  int right;
  int top;
  int bottom;

  data = BBitmap_data (bitmap);
  BBitmap_dimensions (bitmap, &left, &top, &right, &bottom,
		      &bytes_per_row, &mono_p);

  if (x < left || x > right || y < top || y > bottom)
    emacs_abort ();

  if (mono_p)
    {
      ptrdiff_t off = y * bytes_per_row;
      ptrdiff_t bit = x % 8;
      ptrdiff_t xoff = x / 8;

      unsigned char *byte = data + off + xoff;
      if (!pixel)
	*byte &= ~(1 << bit);
      else
	*byte |= 1 << bit;
    }
  else
    ((uint32_t *) (data + (bytes_per_row * y)))[x] = pixel;
}

void
haiku_free_frame_resources (struct frame *f)
{
  haiku window, drawable, mbar;
  Mouse_HLInfo *hlinfo;
  struct haiku_display_info *dpyinfo;
  Lisp_Object bar;
  struct scroll_bar *b;

  block_input ();
  check_window_system (f);

  hlinfo = MOUSE_HL_INFO (f);
  window = FRAME_HAIKU_WINDOW (f);
  drawable = FRAME_HAIKU_VIEW (f);
  mbar = FRAME_HAIKU_MENU_BAR (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  free_frame_faces (f);

  /* Free scroll bars */
  for (bar = FRAME_SCROLL_BARS (f); !NILP (bar); bar = b->next)
    {
      b = XSCROLL_BAR (bar);
      haiku_scroll_bar_remove (b);
    }

  if (f == dpyinfo->highlight_frame)
    dpyinfo->highlight_frame = 0;
  if (f == dpyinfo->focused_frame)
    dpyinfo->focused_frame = 0;
  if (f == dpyinfo->last_mouse_motion_frame)
    dpyinfo->last_mouse_motion_frame = NULL;
  if (f == dpyinfo->last_mouse_frame)
    dpyinfo->last_mouse_frame = NULL;
  if (f == dpyinfo->focus_event_frame)
    dpyinfo->focus_event_frame = NULL;

  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  if (mbar)
    {
      BMenuBar_delete (mbar);
      if (f->output_data.haiku->menu_bar_open_p)
	{
	  --popup_activated_p;
	  f->output_data.haiku->menu_bar_open_p = 0;
	}
    }

  if (drawable)
    BView_emacs_delete (drawable);

  if (window)
    BWindow_quit (window);

  /* Free cursors */

  BCursor_delete (f->output_data.haiku->text_cursor);
  BCursor_delete (f->output_data.haiku->nontext_cursor);
  BCursor_delete (f->output_data.haiku->modeline_cursor);
  BCursor_delete (f->output_data.haiku->hand_cursor);
  BCursor_delete (f->output_data.haiku->hourglass_cursor);
  BCursor_delete (f->output_data.haiku->horizontal_drag_cursor);
  BCursor_delete (f->output_data.haiku->vertical_drag_cursor);
  BCursor_delete (f->output_data.haiku->left_edge_cursor);
  BCursor_delete (f->output_data.haiku->top_left_corner_cursor);
  BCursor_delete (f->output_data.haiku->top_edge_cursor);
  BCursor_delete (f->output_data.haiku->top_right_corner_cursor);
  BCursor_delete (f->output_data.haiku->right_edge_cursor);
  BCursor_delete (f->output_data.haiku->bottom_right_corner_cursor);
  BCursor_delete (f->output_data.haiku->bottom_edge_cursor);
  BCursor_delete (f->output_data.haiku->bottom_left_corner_cursor);
  BCursor_delete (f->output_data.haiku->no_cursor);

  xfree (FRAME_OUTPUT_DATA (f));
  FRAME_OUTPUT_DATA (f) = NULL;

  unblock_input ();
}

void
haiku_iconify_frame (struct frame *frame)
{
  if (FRAME_ICONIFIED_P (frame))
    return;

  block_input ();

  SET_FRAME_VISIBLE (frame, false);
  SET_FRAME_ICONIFIED (frame, true);

  BWindow_iconify (FRAME_HAIKU_WINDOW (frame));

  unblock_input ();
}

void
haiku_visualize_frame (struct frame *f)
{
  block_input ();

  if (!FRAME_VISIBLE_P (f))
    {
      if (FRAME_NO_FOCUS_ON_MAP (f))
	BWindow_set_avoid_focus (FRAME_HAIKU_WINDOW (f), 1);
      BWindow_set_visible (FRAME_HAIKU_WINDOW (f), 1);
      if (FRAME_NO_FOCUS_ON_MAP (f) &&
	  !FRAME_NO_ACCEPT_FOCUS (f))
	BWindow_set_avoid_focus (FRAME_HAIKU_WINDOW (f), 0);

      haiku_set_offset (f, f->left_pos, f->top_pos, 0);

      SET_FRAME_VISIBLE (f, 1);
      SET_FRAME_ICONIFIED (f, 0);
    }

  unblock_input ();
}

void
haiku_unvisualize_frame (struct frame *f)
{
  block_input ();

  BWindow_set_visible (FRAME_HAIKU_WINDOW (f), 0);
  SET_FRAME_VISIBLE (f, 0);
  SET_FRAME_ICONIFIED (f, 0);

  unblock_input ();
}

void
haiku_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int old_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  int new_width = check_int_nonnegative (arg);

  if (new_width == old_width)
    return;
  f->internal_border_width = new_width;

  if (FRAME_HAIKU_WINDOW (f))
    {
      adjust_frame_size (f, -1, -1, 3, 0, Qinternal_border_width);
      haiku_clear_under_internal_border (f);
    }

  SET_FRAME_GARBAGED (f);
}

void
haiku_set_frame_visible_invisible (struct frame *f, bool visible_p)
{
  if (visible_p)
    haiku_visualize_frame (f);
  else
    haiku_unvisualize_frame (f);
}

void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
  block_input ();

  BView_convert_to_screen (FRAME_HAIKU_VIEW (f), &pix_x, &pix_y);
  be_warp_pointer (pix_x, pix_y);

  unblock_input ();
}

void
haiku_query_color (uint32_t col, Emacs_Color *color_def)
{
  color_def->red = RED_FROM_ULONG (col) * 257;
  color_def->green = GREEN_FROM_ULONG (col) * 257;
  color_def->blue = BLUE_FROM_ULONG (col) * 257;

  color_def->pixel = col;
}

Display_Info *
check_x_display_info (Lisp_Object object)
{
  return check_haiku_display_info (object);
}

/* Rename frame F to NAME.  If NAME is nil, set F's name to "GNU
   Emacs".  If EXPLICIT_P is non-zero, that indicates Lisp code is
   setting the name, not redisplay; in that case, set F's name to NAME
   and set F->explicit_name; if NAME is nil, clear F->explicit_name.

   If EXPLICIT_P is zero, it means redisplay is setting the name; the
   name provided will be ignored if explicit_name is set.  */
void
haiku_set_name (struct frame *f, Lisp_Object name, bool explicit_p)
{
  if (explicit_p)
    {
      if (f->explicit_name && NILP (name))
	update_mode_lines = 24;

      f->explicit_name = !NILP (name);
    }
  else if (f->explicit_name)
    return;

  if (NILP (name))
    name = build_unibyte_string ("GNU Emacs");

  if (!NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  if (!NILP (f->title))
    name = f->title;

  haiku_set_title_bar_text (f, name);
}

static void
haiku_set_inhibit_double_buffering (struct frame *f,
				    Lisp_Object new_value,
				    Lisp_Object old_value)
{
  block_input ();
  if (FRAME_HAIKU_WINDOW (f))
    {
      if (NILP (new_value))
	{
	  EmacsView_set_up_double_buffering (FRAME_HAIKU_VIEW (f));
	  if (!NILP (old_value))
	    {
	      SET_FRAME_GARBAGED (f);
	      expose_frame (f, 0, 0, 0, 0);
	    }
	}
      else
	EmacsView_disable_double_buffering (FRAME_HAIKU_VIEW (f));
    }
  unblock_input ();
}



DEFUN ("haiku-set-mouse-absolute-pixel-position",
       Fhaiku_set_mouse_absolute_pixel_position,
       Shaiku_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to a pixel position at (X, Y).  The
coordinates X and Y are interpreted to start from the top-left
corner of the screen.  */)
  (Lisp_Object x, Lisp_Object y)
{
  int xval = check_integer_range (x, INT_MIN, INT_MAX);
  int yval = check_integer_range (y, INT_MIN, INT_MAX);

  if (!x_display_list)
    error ("Window system not initialized");

  block_input ();
  be_warp_pointer (xval, yval);
  unblock_input ();
  return Qnil;
}

DEFUN ("haiku-mouse-absolute-pixel-position", Fhaiku_mouse_absolute_pixel_position,
       Shaiku_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the coordinates of
the mouse cursor position in pixels relative to a position (0, 0) of the
selected frame's display.  */)
  (void)
{
  if (!x_display_list)
    return Qnil;

  struct frame *f = SELECTED_FRAME ();

  if (FRAME_INITIAL_P (f) || !FRAME_HAIKU_P (f)
      || !FRAME_HAIKU_VIEW (f))
    return Qnil;

  block_input ();
  void *view = FRAME_HAIKU_VIEW (f);

  int x, y;
  BView_get_mouse (view, &x, &y);
  BView_convert_to_screen (view, &x, &y);
  unblock_input ();

  return Fcons (make_fixnum (x), make_fixnum (y));
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object terminal)
{
  return Qt;
}

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;
  CHECK_STRING (color);
  decode_window_system_frame (frame);

  return haiku_get_color (SSDATA (color), &col) ? Qnil : Qt;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;
  CHECK_STRING (color);
  decode_window_system_frame (frame);

  block_input ();
  if (haiku_get_color (SSDATA (color), &col))
    {
      unblock_input ();
      return Qnil;
    }
  unblock_input ();
  return list3i (lrint (col.red), lrint (col.green), lrint (col.blue));
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return Qnil;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object display, Lisp_Object resource_string, Lisp_Object must_succeed)
{
  struct haiku_display_info *dpy_info;
  CHECK_STRING (display);

  if (NILP (Fstring_equal (display, build_string ("be"))))
    !NILP (must_succeed) ? fatal ("Bad display") : error ("Bad display");
  dpy_info = haiku_term_init ();

  if (!dpy_info)
    !NILP (must_succeed) ? fatal ("Display not responding") :
      error ("Display not responding");

  return Qnil;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)

{
  check_haiku_display_info (terminal);

  int width, height;
  BScreen_px_dim (&width, &height);
  return make_fixnum (width);
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height, Sx_display_pixel_height,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)

{
  check_haiku_display_info (terminal);

  int width, height;
  BScreen_px_dim (&width, &height);
  return make_fixnum (width);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct haiku_display_info *dpyinfo = check_haiku_display_info (terminal);

  int width, height;
  BScreen_px_dim (&width, &height);

  return make_fixnum (height / (dpyinfo->resy / 25.4));
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct haiku_display_info *dpyinfo = check_haiku_display_info (terminal);

  int width, height;
  BScreen_px_dim (&width, &height);

  return make_fixnum (height / (dpyinfo->resy / 25.4));
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object parms)
{
  return haiku_create_frame (parms, 0);
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);

  int planes = be_get_display_planes ();

  if (planes == 8)
    return intern ("static-color");
  else if (planes == 16 || planes == 15)
    return intern ("pseudo-color");

  return intern ("direct-color");
}

DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
  struct frame *tip_f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int width, height;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  ptrdiff_t count = SPECPDL_INDEX ();
  ptrdiff_t count_1;
  Lisp_Object window, size, tip_buf;

  AUTO_STRING (tip, " *tip*");

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);

  if (NILP (frame))
    frame = selected_frame;
  decode_window_system_frame (frame);

  if (NILP (timeout))
    timeout = make_fixnum (5);
  else
    CHECK_FIXNAT (timeout);

  if (NILP (dx))
    dx = make_fixnum (5);
  else
    CHECK_FIXNUM (dx);

  if (NILP (dy))
    dy = make_fixnum (-10);
  else
    CHECK_FIXNUM (dy);

  if (haiku_use_system_tooltips)
    {
      int root_x, root_y;
      CHECK_STRING (string);
      if (STRING_MULTIBYTE (string))
	string = ENCODE_UTF_8 (string);

      if (NILP (frame))
	frame = selected_frame;

      struct frame *f = decode_window_system_frame (frame);
      block_input ();

      char *str = xstrdup (SSDATA (string));
      int height = be_plain_font_height ();
      int width;
      char *tok = strtok (str, "\n");
      width = be_string_width_with_plain_font (tok);

      while ((tok = strtok (NULL, "\n")))
	{
	  height = be_plain_font_height ();
	  int w = be_string_width_with_plain_font (tok);
	  if (w > width)
	    w = width;
	}
      free (str);

      height += 16; /* Default margin.  */
      width += 16; /* Ditto.  Unfortunately there isn't a more
		      reliable way to get it.  */
      compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);
      BView_convert_from_screen (FRAME_HAIKU_VIEW (f), &root_x, &root_y);
      BView_set_and_show_sticky_tooltip (FRAME_HAIKU_VIEW (f), SSDATA (string),
					 root_x, root_y);
      unblock_input ();
      goto start_timer;
    }

  if (!NILP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	  && EQ (frame, tip_last_frame)
	  && !NILP (Fequal_including_properties (string, tip_last_string))
	  && !NILP (Fequal (parms, tip_last_parms)))
	{
	  /* Only DX and DY have changed.  */
	  tip_f = XFRAME (tip_frame);
	  if (!NILP (tip_timer))
	    {
	      Lisp_Object timer = tip_timer;

	      tip_timer = Qnil;
	      call1 (Qcancel_timer, timer);
	    }

	  block_input ();
	  compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			  FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);
	  haiku_set_offset (tip_f, root_x, root_y, 1);
	  haiku_visualize_frame (tip_f);
	  unblock_input ();

	  goto start_timer;
	}
      else if (tooltip_reuse_hidden_frame && EQ (frame, tip_last_frame))
	{
	  bool delete = false;
	  Lisp_Object tail, elt, parm, last;

	  /* Check if every parameter in PARMS has the same value in
	     tip_last_parms.  This may destruct tip_last_parms
	     which, however, will be recreated below.  */
	  for (tail = parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      /* The left, top, right and bottom parameters are handled
		 by compute_tip_xy so they can be ignored here.  */
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop)
		  && !EQ (parm, Qright) && !EQ (parm, Qbottom))
		{
		  last = Fassq (parm, tip_last_parms);
		  if (NILP (Fequal (Fcdr (elt), Fcdr (last))))
		    {
		      /* We lost, delete the old tooltip.  */
		      delete = true;
		      break;
		    }
		  else
		    tip_last_parms =
		      call2 (Qassq_delete_all, parm, tip_last_parms);
		}
	      else
		tip_last_parms =
		  call2 (Qassq_delete_all, parm, tip_last_parms);
	    }

	  /* Now check if there's a parameter left in tip_last_parms with a
	     non-nil value.  */
	  for (tail = tip_last_parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop) && !EQ (parm, Qright)
		  && !EQ (parm, Qbottom) && !NILP (Fcdr (elt)))
		{
		  /* We lost, delete the old tooltip.  */
		  delete = true;
		  break;
		}
	    }

	  haiku_hide_tip (delete);
	}
      else
	haiku_hide_tip (true);
    }
  else
    haiku_hide_tip (true);

  tip_last_frame = frame;
  tip_last_string = string;
  tip_last_parms = parms;

  /* Block input until the tip has been fully drawn, to avoid crashes
     when drawing tips in menus.  */
  block_input ();

  if (NILP (tip_frame) || !FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      /* Add default values to frame parameters.  */
      if (NILP (Fassq (Qname, parms)))
	parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
      if (NILP (Fassq (Qinternal_border_width, parms)))
	parms = Fcons (Fcons (Qinternal_border_width, make_fixnum (3)), parms);
      if (NILP (Fassq (Qborder_width, parms)))
	parms = Fcons (Fcons (Qborder_width, make_fixnum (1)), parms);
      if (NILP (Fassq (Qborder_color, parms)))
	parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")),
		       parms);
      if (NILP (Fassq (Qbackground_color, parms)))
	parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		       parms);

      /* Create a frame for the tooltip and record it in the global
	 variable tip_frame.  */

      if (NILP (tip_frame = haiku_create_frame (parms, 1)))
	{
	  /* Creating the tip frame failed.  */
	  unblock_input ();
	  return unbind_to (count, Qnil);
	}
    }

  tip_f = XFRAME (tip_frame);
  window = FRAME_ROOT_WINDOW (tip_f);
  tip_buf = Fget_buffer_create (tip, Qnil);
  /* We will mark the tip window a "pseudo-window" below, and such
     windows cannot have display margins.  */
  bset_left_margin_cols (XBUFFER (tip_buf), make_fixnum (0));
  bset_right_margin_cols (XBUFFER (tip_buf), make_fixnum (0));
  set_window_buffer (window, tip_buf, false, false);
  w = XWINDOW (window);
  w->pseudo_window_p = true;
  /* Try to avoid that `other-window' select us (Bug#47207).  */
  Fset_window_parameter (window, Qno_other_window, Qt);

  /* Set up the frame's root window.  Note: The following code does not
     try to size the window or its frame correctly.  Its only purpose is
     to make the subsequent text size calculations work.  The right
     sizes should get installed when the toolkit gets back to us.  */
  w->left_col = 0;
  w->top_line = 0;
  w->pixel_left = 0;
  w->pixel_top = 0;

  if (CONSP (Vx_max_tooltip_size)
      && RANGED_FIXNUMP (1, XCAR (Vx_max_tooltip_size), INT_MAX)
      && RANGED_FIXNUMP (1, XCDR (Vx_max_tooltip_size), INT_MAX))
    {
      w->total_cols = XFIXNAT (XCAR (Vx_max_tooltip_size));
      w->total_lines = XFIXNAT (XCDR (Vx_max_tooltip_size));
    }
  else
    {
      w->total_cols = 80;
      w->total_lines = 40;
    }

  w->pixel_width = w->total_cols * FRAME_COLUMN_WIDTH (tip_f);
  w->pixel_height = w->total_lines * FRAME_LINE_HEIGHT (tip_f);
  FRAME_TOTAL_COLS (tip_f) = WINDOW_TOTAL_COLS (w);
  adjust_frame_glyphs (tip_f);

  /* Insert STRING into the root window's buffer and fit the frame to
     the buffer.  */
  count_1 = SPECPDL_INDEX ();
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (w->contents));
  bset_truncate_lines (current_buffer, Qnil);
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  specbind (Qinhibit_point_motion_hooks, Qt);
  Ferase_buffer ();
  Finsert (1, &string);
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (window, pos, TRY_WINDOW_IGNORE_FONTS_CHANGE);
  /* Calculate size of tooltip window.  */
  size = Fwindow_text_pixel_size (window, Qnil, Qnil, Qnil,
				  make_fixnum (w->pixel_height), Qnil);
  /* Add the frame's internal border to calculated size.  */
  width = XFIXNUM (Fcar (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  height = XFIXNUM (Fcdr (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  /* Calculate position of tooltip frame.  */
  compute_tip_xy (tip_f, parms, dx, dy, width, height, &root_x, &root_y);
  BWindow_resize (FRAME_HAIKU_WINDOW (tip_f), width, height);
  haiku_set_offset (tip_f, root_x, root_y, 1);
  BWindow_set_tooltip_decoration (FRAME_HAIKU_WINDOW (tip_f));
  BView_set_view_cursor (FRAME_HAIKU_VIEW (tip_f),
			 FRAME_OUTPUT_DATA (XFRAME (frame))->current_cursor);
  SET_FRAME_VISIBLE (tip_f, 1);
  BWindow_set_visible (FRAME_HAIKU_WINDOW (tip_f), 1);

  w->must_be_updated_p = true;
  flush_frame (tip_f);
  update_single_window (w);
  set_buffer_internal_1 (old_buffer);
  unbind_to (count_1, Qnil);
  unblock_input ();
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  return unbind_to (count, Qnil);
}

DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  return haiku_hide_tip (!tooltip_reuse_hidden_frame);
}

DEFUN ("x-close-connection", Fx_close_connection, Sx_close_connection, 1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */
       attributes: noreturn)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);

  error ("Cannot close Haiku displays");
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c. */)
  (void)
{
  if (!x_display_list)
    return Qnil;

  return list1 (XCAR (x_display_list->name_list_element));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);
  return build_string ("Haiku, Inc.");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c. */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);
  return list3i (5, 1, 1);
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);
  return make_fixnum (be_get_display_screens ());
}

DEFUN ("haiku-get-version-string", Fhaiku_get_version_string,
       Shaiku_get_version_string, 0, 0, 0,
       doc: /* Return a string describing the current Haiku version.  */)
  (void)
{
  char buf[1024];

  be_get_version_string ((char *) &buf, sizeof buf);
  return build_string (buf);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);

  return make_fixnum (be_get_display_color_cells ());
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);

  return make_fixnum (be_get_display_planes ());
}

DEFUN ("x-double-buffered-p", Fx_double_buffered_p, Sx_double_buffered_p,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);
  check_window_system (f);

  return EmacsView_double_buffered_p (FRAME_HAIKU_VIEW (f)) ? Qt : Qnil;
}

DEFUN ("x-display-backing-store", Fx_display_backing_store, Sx_display_backing_store,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  if (FRAMEP (terminal))
    {
      CHECK_LIVE_FRAME (terminal);
      struct frame *f = decode_window_system_frame (terminal);

      if (FRAME_HAIKU_VIEW (f) &&
	  EmacsView_double_buffered_p (FRAME_HAIKU_VIEW (f)))
	return FRAME_PARENT_FRAME (f) ? Qwhen_mapped : Qalways;
      else
	return Qnot_useful;
    }
  else
    {
      check_haiku_display_info (terminal);
      return Qnot_useful;
    }
}

DEFUN ("haiku-frame-geometry", Fhaiku_frame_geometry, Shaiku_frame_geometry, 0, 1, 0,
       doc: /* Return geometric attributes of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is an association list of the attributes listed below.  All height
and width values are in pixels.

`outer-position' is a cons of the outer left and top edges of FRAME
  relative to the origin - the position (0, 0) - of FRAME's display.

`outer-size' is a cons of the outer width and height of FRAME.  The
  outer size includes the title bar and the external borders as well as
  any menu and/or tool bar of frame.

`external-border-size' is a cons of the horizontal and vertical width of
  FRAME's external borders as supplied by the window manager.

`title-bar-size' is a cons of the width and height of the title bar of
  FRAME as supplied by the window manager.  If both of them are zero,
  FRAME has no title bar.  If only the width is zero, Emacs was not
  able to retrieve the width information.

`menu-bar-external', if non-nil, means the menu bar is external (never
  included in the inner edges of FRAME).

`menu-bar-size' is a cons of the width and height of the menu bar of
  FRAME.

`tool-bar-external', if non-nil, means the tool bar is external (never
  included in the inner edges of FRAME).

`tool-bar-position' tells on which side the tool bar on FRAME is and can
  be one of `left', `top', `right' or `bottom'.  If this is nil, FRAME
  has no tool bar.

`tool-bar-size' is a cons of the width and height of the tool bar of
  FRAME.

`internal-border-width' is the width of the internal border of
  FRAME.  */)
  (Lisp_Object frame)
{
  return frame_geometry (frame, Qnil);
}

DEFUN ("haiku-frame-edges", Fhaiku_frame_edges, Shaiku_frame_edges, 0, 2, 0,
       doc: /* Return edge coordinates of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is a list of the form (LEFT, TOP, RIGHT, BOTTOM).  All values are
in pixels relative to the origin - the position (0, 0) - of FRAME's
display.

If optional argument TYPE is the symbol `outer-edges', return the outer
edges of FRAME.  The outer edges comprise the decorations of the window
manager (like the title bar or external borders) as well as any external
menu or tool bar of FRAME.  If optional argument TYPE is the symbol
`native-edges' or nil, return the native edges of FRAME.  The native
edges exclude the decorations of the window manager and any external
menu or tool bar of FRAME.  If TYPE is the symbol `inner-edges', return
the inner edges of FRAME.  These edges exclude title bar, any borders,
menu bar or tool bar of FRAME.  */)
  (Lisp_Object frame, Lisp_Object type)
{
  return frame_geometry (frame, ((EQ (type, Qouter_edges)
				  || EQ (type, Qinner_edges))
				 ? type
				 : Qnative_edges));
}

DEFUN ("haiku-read-file-name", Fhaiku_read_file_name, Shaiku_read_file_name, 1, 6, 0,
       doc: /* Use a graphical panel to read a file name, using prompt PROMPT.
Optional arg FRAME specifies a frame on which to display the file panel.
If it is nil, the current frame is used instead.
The frame being used will be brought to the front of
the display after the file panel is closed.
Optional arg DIR, if non-nil, supplies a default directory.
Optional arg MUSTMATCH, if non-nil, means the returned file or
directory must exist.
Optional arg DIR_ONLY_P, if non-nil, means choose only directories.
Optional arg SAVE_TEXT, if non-nil, specifies some text to show in the entry field.  */)
  (Lisp_Object prompt, Lisp_Object frame,
   Lisp_Object dir, Lisp_Object mustmatch,
   Lisp_Object dir_only_p, Lisp_Object save_text)
{
  ptrdiff_t idx;
  if (!x_display_list)
    error ("Be windowing not initialized");

  if (!NILP (dir))
    CHECK_STRING (dir);

  if (!NILP (save_text))
    CHECK_STRING (save_text);

  if (NILP (frame))
    frame = selected_frame;

  CHECK_STRING (prompt);

  CHECK_LIVE_FRAME (frame);
  check_window_system (XFRAME (frame));

  idx = SPECPDL_INDEX ();
  record_unwind_protect_void (unwind_popup);

  struct frame *f = XFRAME (frame);

  FRAME_DISPLAY_INFO (f)->focus_event_frame = f;

  ++popup_activated_p;
  char *fn = be_popup_file_dialog (!NILP (mustmatch) || !NILP (dir_only_p),
				   !NILP (dir) ? SSDATA (ENCODE_UTF_8 (dir)) : NULL,
				   !NILP (mustmatch), !NILP (dir_only_p),
				   FRAME_HAIKU_WINDOW (f),
				   !NILP (save_text) ? SSDATA (ENCODE_UTF_8 (save_text)) : NULL,
				   SSDATA (ENCODE_UTF_8 (prompt)),
				   block_input, unblock_input);

  unbind_to (idx, Qnil);

  block_input ();
  BWindow_activate (FRAME_HAIKU_WINDOW (f));
  unblock_input ();

  if (!fn)
    return Qnil;

  Lisp_Object p = build_string_from_utf8 (fn);
  free (fn);
  return p;
}

DEFUN ("haiku-put-resource", Fhaiku_put_resource, Shaiku_put_resource,
       2, 2, 0, doc: /* Place STRING by the key RESOURCE in the resource database.
It can later be retrieved with `x-get-resource'.  */)
  (Lisp_Object resource, Lisp_Object string)
{
  CHECK_STRING (resource);
  if (!NILP (string))
    CHECK_STRING (string);

  put_xrm_resource (resource, string);
  return Qnil;
}

DEFUN ("haiku-frame-list-z-order", Fhaiku_frame_list_z_order,
       Shaiku_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs' frames, in Z (stacking) order.
If TERMINAL is non-nil and specifies a live frame, return the child
frames of that frame in Z (stacking) order.

As it is impossible to reliably determine the frame stacking order on
Haiku, the selected frame is always the first element of the returned
list, while the rest are not guaranteed to be in any particular order.

Frames are listed from topmost (first) to bottommost (last).  */)
  (Lisp_Object terminal)
{
  Lisp_Object frames = Qnil;
  Lisp_Object head, tail;
  Lisp_Object sel = Qnil;

  FOR_EACH_FRAME (head, tail)
    {
      struct frame *f = XFRAME (tail);
      if (!FRAME_HAIKU_P (f) ||
	  (FRAMEP (terminal) &&
	   FRAME_LIVE_P (XFRAME (terminal)) &&
	   !EQ (terminal, get_frame_param (f, Qparent_frame))))
	continue;

      if (EQ (tail, selected_frame))
	sel = tail;
      else
	frames = Fcons (tail, frames);
    }

  if (NILP (sel))
    return frames;
  return Fcons (sel, frames);
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_haiku_display_info (terminal);

  if (FRAMEP (terminal))
    {
      struct frame *f = decode_window_system_frame (terminal);
      return FRAME_HAIKU_VIEW (f) && EmacsView_double_buffered_p (FRAME_HAIKU_VIEW (f)) ?
	Qt : Qnil;
    }

  return Qnil;
}

frame_parm_handler haiku_frame_parm_handlers[] =
  {
    gui_set_autoraise,
    gui_set_autolower,
    haiku_set_background_color,
    NULL, /* x_set_border_color */
    gui_set_border_width,
    haiku_set_cursor_color,
    haiku_set_cursor_type,
    gui_set_font,
    haiku_set_foreground_color,
    NULL, /* set icon name */
    NULL, /* set icon type */
    haiku_set_child_frame_border_width,
    haiku_set_internal_border_width,
    gui_set_right_divider_width,
    gui_set_bottom_divider_width,
    haiku_set_menu_bar_lines,
    NULL, /* set mouse color */
    haiku_explicitly_set_name,
    gui_set_scroll_bar_width,
    gui_set_scroll_bar_height,
    haiku_set_title,
    gui_set_unsplittable,
    gui_set_vertical_scroll_bars,
    gui_set_horizontal_scroll_bars,
    gui_set_visibility,
    haiku_set_tab_bar_lines,
    haiku_set_tool_bar_lines,
    NULL, /* set scroll bar fg */
    NULL, /* set scroll bar bkg */
    gui_set_screen_gamma,
    gui_set_line_spacing,
    gui_set_left_fringe,
    gui_set_right_fringe,
    NULL, /* x wait for wm */
    gui_set_fullscreen,
    gui_set_font_backend,
    gui_set_alpha,
    NULL, /* set sticky */
    NULL, /* set tool bar pos */
    haiku_set_inhibit_double_buffering,
    haiku_set_undecorated,
    haiku_set_parent_frame,
    NULL, /* set skip taskbar */
    haiku_set_no_focus_on_map,
    haiku_set_no_accept_focus,
    NULL, /* set z group */
    NULL, /* set override redir */
    gui_set_no_special_glyphs
  };

void
syms_of_haikufns (void)
{
  DEFSYM (Qfont_parameter, "font-parameter");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qassq_delete_all, "assq-delete-all");

  DEFSYM (Qalways, "always");
  DEFSYM (Qnot_useful, "not-useful");
  DEFSYM (Qwhen_mapped, "when-mapped");

  defsubr (&Sx_hide_tip);
  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_create_frame);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sxw_color_values);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_show_tip);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_screens);
  defsubr (&Shaiku_get_version_string);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_planes);
  defsubr (&Shaiku_set_mouse_absolute_pixel_position);
  defsubr (&Shaiku_mouse_absolute_pixel_position);
  defsubr (&Shaiku_frame_geometry);
  defsubr (&Shaiku_frame_edges);
  defsubr (&Sx_double_buffered_p);
  defsubr (&Sx_display_backing_store);
  defsubr (&Shaiku_read_file_name);
  defsubr (&Shaiku_put_resource);
  defsubr (&Shaiku_frame_list_z_order);
  defsubr (&Sx_display_save_under);

  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);
  tip_last_frame = Qnil;
  staticpro (&tip_last_frame);
  tip_last_string = Qnil;
  staticpro (&tip_last_string);
  tip_last_parms = Qnil;
  staticpro (&tip_last_parms);

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_max_tooltip_size = Fcons (make_fixnum (80), make_fixnum (40));

  DEFVAR_BOOL ("haiku-use-system-tooltips", haiku_use_system_tooltips,
	       doc: /* When non-nil, Emacs will display tooltips using the App Kit.
This can avoid a great deal of consing that does not play
well with the Haiku memory allocator, but comes with the
disadvantage of not being able to use special display properties
within tooltips.  */);
  haiku_use_system_tooltips = 1;

#ifdef USE_BE_CAIRO
  DEFVAR_LISP ("cairo-version-string", Vcairo_version_string,
               doc: /* Version info for cairo.  */);
  {
    char cairo_version[sizeof ".." + 3 * INT_STRLEN_BOUND (int)];
    int len = sprintf (cairo_version, "%d.%d.%d",
		       CAIRO_VERSION_MAJOR, CAIRO_VERSION_MINOR,
                       CAIRO_VERSION_MICRO);
    Vcairo_version_string = make_pure_string (cairo_version, len, len, false);
  }
#endif

  return;
}
