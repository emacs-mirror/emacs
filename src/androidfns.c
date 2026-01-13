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

#include <config.h>
#include <math.h>
#include <stdlib.h>

#include "lisp.h"
#include "android.h"
#include "androidterm.h"
#include "blockinput.h"
#include "keyboard.h"
#include "buffer.h"
#include "androidgui.h"
#include "pdumper.h"

#ifndef ANDROID_STUBIFY

/* The frame of the currently visible tooltip, or nil if none.  */
static Lisp_Object tip_frame;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
static android_window tip_window;

/* The X and Y deltas of the last call to `x-show-tip'.  */
static Lisp_Object tip_dx, tip_dy;

/* A timer that hides or deletes the currently visible tooltip when it
   fires.  */
static Lisp_Object tip_timer;

/* STRING argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_string;

/* Normalized FRAME argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_frame;

/* PARMS argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_parms;

#endif

static struct android_display_info *
android_display_info_for_name (Lisp_Object name)
{
  struct android_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
      if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element),
				name)))
	return dpyinfo;
    }

  error ("Cannot connect to Android if it was not initialized"
	 " at startup");
}

static struct android_display_info *
check_android_display_info (Lisp_Object object)
{
  struct android_display_info *dpyinfo;
  struct frame *sf, *f;
  struct terminal *t;

  if (NILP (object))
    {
      sf = XFRAME (selected_frame);

      if (FRAME_ANDROID_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_DISPLAY_INFO (sf);
      else if (x_display_list)
	dpyinfo = x_display_list;
      else
	error ("Android windows are not in use or not initialized");
    }
  else if (TERMINALP (object))
    {
      t = decode_live_terminal (object);

      if (t->type != output_android)
        error ("Terminal %d is not an Android display", t->id);

      dpyinfo = t->display_info.android;
    }
  else if (STRINGP (object))
    dpyinfo = android_display_info_for_name (object);
  else
    {
      f = decode_window_system_frame (object);
      dpyinfo = FRAME_DISPLAY_INFO (f);
    }

  return dpyinfo;
}

Display_Info *
check_x_display_info (Lisp_Object object)
{
  return check_android_display_info (object);
}



#ifndef ANDROID_STUBIFY

void
gamma_correct (struct frame *f, Emacs_Color *color)
{
  if (f->gamma)
    {
      color->red = pow (color->red / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->green = pow (color->green / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->blue = pow (color->blue / 65535.0, f->gamma) * 65535.0 + 0.5;
    }
}

/* Decide if color named COLOR_NAME is valid for use on frame F.  If
   so, return the RGB values in COLOR.  If ALLOC_P, allocate the
   color.  Value is false if COLOR_NAME is invalid, or no color could
   be allocated.  MAKE_INDEX is some mysterious argument used on
   NS. */

bool
android_defined_color (struct frame *f, const char *color_name,
		       Emacs_Color *color, bool alloc_p,
		       bool make_index)
{
  bool success_p;

  success_p = false;

  block_input ();
  success_p = android_parse_color (f, color_name, color);
  if (success_p && alloc_p)
    success_p = android_alloc_nearest_color (f, color);
  unblock_input ();

  return success_p;
}

/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG
   says.  Signal an error if color can't be allocated.  */

static unsigned long
android_decode_color (struct frame *f, Lisp_Object color_name, int mono_color)
{
  Emacs_Color cdef;

  CHECK_STRING (color_name);

  if (android_defined_color (f, SSDATA (color_name), &cdef,
			     true, false))
    return cdef.pixel;

  signal_error ("Undefined color", color_name);
}

static void
android_set_parent_frame (struct frame *f, Lisp_Object new_value,
			  Lisp_Object old_value)
{
  struct frame *p;

  p = NULL;

  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_ANDROID_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      error ("Invalid specification of `parent-frame'");
    }

  if (p != FRAME_PARENT_FRAME (f))
    {
      block_input ();
      android_reparent_window (FRAME_ANDROID_WINDOW (f),
			       (p ? FRAME_ANDROID_WINDOW (p)
				: FRAME_DISPLAY_INFO (f)->root_window),
			       f->left_pos, f->top_pos);
      unblock_input ();

      fset_parent_frame (f, new_value);
    }

  /* Update the fullscreen frame parameter as well.  */
  FRAME_TERMINAL (f)->fullscreen_hook (f);
}

/* Set the WM name to NAME for frame F. Also set the icon name.
   If the frame already has an icon name, use that, otherwise set the
   icon name to NAME.  */

static void
android_set_name_internal (struct frame *f, Lisp_Object name)
{
  jstring java_name;

  if (FRAME_ANDROID_WINDOW (f))
    {
      java_name = android_build_string (name, NULL);
      android_set_wm_name (FRAME_ANDROID_WINDOW (f), java_name);
      ANDROID_DELETE_LOCAL_REF (java_name);
    }
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       x_id_name.

   If EXPLICIT is true, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is false, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

static void
android_set_name (struct frame *f, Lisp_Object name, bool explicit)
{
  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 37;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the x_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_DISPLAY_INFO (f)->x_id_name,
		   SSDATA (f->name)))
	return;
      name = build_string (FRAME_DISPLAY_INFO (f)->x_id_name);
    }
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  android_set_name_internal (f, name);
}

void
android_implicitly_set_name (struct frame *f, Lisp_Object arg,
			     Lisp_Object oldval)
{
  android_set_name (f, arg, false);
}

void
android_explicitly_set_name (struct frame *f, Lisp_Object arg,
			     Lisp_Object oldval)
{
  android_set_name (f, arg, true);
}

/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tool bar lines.  This function changes the
   height of all windows on frame F to match the new tool bar height.
   The frame's height doesn't change.  */

static void
android_set_tool_bar_lines (struct frame *f, Lisp_Object value,
			    Lisp_Object oldval)
{
  int nlines;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  android_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}

static void
android_set_tool_bar_position (struct frame *f,
			       Lisp_Object new_value,
			       Lisp_Object old_value)
{
  if (!EQ (new_value, Qtop) && !EQ (new_value, Qbottom))
    error ("Tool bar position must be either `top' or `bottom'");

  if (EQ (new_value, old_value))
    return;

  /* Set the tool bar position.  */
  fset_tool_bar_position (f, new_value);

  /* Now reconfigure frame glyphs to place the tool bar at the
     bottom.  While the inner height has not changed, call
     `resize_frame_windows' to place each of the windows at its
     new position.  */

  adjust_frame_size (f, -1, -1, 3, false, Qtool_bar_position);
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);

  if (FRAME_ANDROID_WINDOW (f))
    android_clear_under_internal_border (f);
}

void
android_change_tool_bar_height (struct frame *f, int height)
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

  if (FRAME_ANDROID_WINDOW (f) && FRAME_TOOL_BAR_HEIGHT (f) == 0)
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
}

/* Set the number of lines used for the tab bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tab bar lines.  This function may change the
   height of all windows on frame F to match the new tab bar height.
   The frame's height may change if frame_inhibit_implied_resize was
   set accordingly.  */

static void
android_set_tab_bar_lines (struct frame *f, Lisp_Object value,
			   Lisp_Object oldval)
{
  int olines;
  int nlines;

  olines = FRAME_TAB_BAR_LINES (f);

  /* Treat tab bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  if (nlines != olines && (olines == 0 || nlines == 0))
    android_change_tab_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}

void
android_change_tab_bar_height (struct frame *f, int height)
{
  int unit, old_height, lines;
  Lisp_Object fullscreen;

  unit = FRAME_LINE_HEIGHT (f);
  old_height = FRAME_TAB_BAR_HEIGHT (f);
  fullscreen = get_frame_param (f, Qfullscreen);

  /* This differs from the tool bar code in that the tab bar height is
     not rounded up.  Otherwise, if redisplay_tab_bar decides to grow
     the tab bar by even 1 pixel, FRAME_TAB_BAR_LINES will be changed,
     leading to the tab bar height being incorrectly set upon the next
     call to android_set_font.  (bug#59285) */

  lines = height / unit;

  /* Even so, HEIGHT might be less than unit if the tab bar face is
     not so tall as the frame's font height; which if true lines will
     be set to 0 and the tab bar will thus vanish.  */

  if (lines == 0 && height != 0)
    lines = 1;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  /* Recalculate tab bar and frame text sizes.  */
  FRAME_TAB_BAR_HEIGHT (f) = height;
  FRAME_TAB_BAR_LINES (f) = lines;
  store_frame_param (f, Qtab_bar_lines, make_fixnum (lines));

  if (FRAME_ANDROID_WINDOW (f) && FRAME_TAB_BAR_HEIGHT (f) == 0)
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
}

void
android_set_scroll_bar_default_height (struct frame *f)
{
  int height;

  height = FRAME_LINE_HEIGHT (f);

  /* The height of a non-toolkit scrollbar is 14 pixels.  */
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (14 + height - 1) / height;

  /* Use all of that space (aside from required margins) for the
     scroll bar.  */
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = 14;
}

void
android_set_scroll_bar_default_width (struct frame *f)
{
  int unit;

  unit = FRAME_COLUMN_WIDTH (f);

  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f)
    = FRAME_CONFIG_SCROLL_BAR_COLS (f) * unit;
}


/* Verify that the icon position args for this window are valid.  */

static void
android_icon_verify (struct frame *f, Lisp_Object parms)
{
  Lisp_Object icon_x, icon_y;

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  icon_x = gui_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0,
					 RES_TYPE_NUMBER);
  icon_y = gui_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0,
					 RES_TYPE_NUMBER);

  if (!BASE_EQ (icon_x, Qunbound) && !BASE_EQ (icon_y, Qunbound))
    {
      CHECK_FIXNUM (icon_x);
      CHECK_FIXNUM (icon_y);
    }
  else if (!BASE_EQ (icon_x, Qunbound) || !BASE_EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
android_icon (struct frame *f, Lisp_Object parms)
{
  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  Lisp_Object icon_x
    = gui_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0,
				    RES_TYPE_NUMBER);
  Lisp_Object icon_y
    = gui_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0,
				    RES_TYPE_NUMBER);

  bool xgiven = !BASE_EQ (icon_x, Qunbound);
  bool ygiven = !BASE_EQ (icon_y, Qunbound);

  if (xgiven != ygiven)
    error ("Both left and top icon corners of icon must be specified");

  if (xgiven)
    {
      check_integer_range (icon_x, INT_MIN, INT_MAX);
      check_integer_range (icon_y, INT_MIN, INT_MAX);
    }

  /* Now return as this is not supported on Android.  */
}

/* Make the GCs needed for this window, setting the background
   color.  */

static void
android_make_gc (struct frame *f)
{
  struct android_gc_values gc_values;

  block_input ();

  /* Create the GCs of this frame.
     Note that many default values are used.  */

  gc_values.foreground = FRAME_FOREGROUND_PIXEL (f);
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  f->output_data.android->normal_gc
    = android_create_gc (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND,
			 &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = FRAME_FOREGROUND_PIXEL (f);
  f->output_data.android->reverse_gc
    = android_create_gc (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND,
			 &gc_values);

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = f->output_data.android->cursor_pixel;
  f->output_data.android->cursor_gc
    = android_create_gc (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND,
			 &gc_values);
  unblock_input ();
}


/* Free what was allocated in android_make_gc.  */

void
android_free_gcs (struct frame *f)
{
  block_input ();

  if (f->output_data.android->normal_gc)
    {
      android_free_gc (f->output_data.android->normal_gc);
      f->output_data.android->normal_gc = 0;
    }

  if (f->output_data.android->reverse_gc)
    {
      android_free_gc (f->output_data.android->reverse_gc);
      f->output_data.android->reverse_gc = 0;
    }

  if (f->output_data.android->cursor_gc)
    {
      android_free_gc (f->output_data.android->cursor_gc);
      f->output_data.android->cursor_gc = 0;
    }

  unblock_input ();
}

/* Handler for signals raised during x_create_frame and
   Fx_create_tip_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is already dead, nothing to do.  This can happen if the
     display is disconnected after the frame has become official, but
     before Fx_create_frame removes the unwind protect.  */
  if (!FRAME_LIVE_P (f))
    return Qnil;

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
      android_free_frame_resources (f);
      free_glyphs (f);
      return Qt;
    }

  return Qnil;
}

static void
do_unwind_create_frame (Lisp_Object frame)
{
  unwind_create_frame (frame);
}

void
android_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct android_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param = gui_display_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
                                                RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (BASE_EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font))
    font = (!NILP (font_param)
	    ? font_param
	    : gui_display_get_arg (dpyinfo, parms,
				   Qfont, "font", "Font",
				   RES_TYPE_STRING));

  if (! FONTP (font) && ! STRINGP (font))
    {
      const char *names[] = {
	"Droid Sans Mono-12",
	"Monospace-12",
	"DroidSansMono-12",
	NULL
      };
      int i;

      for (i = 0; names[i]; i++)
	{
	  font = font_open_by_name (f, build_unibyte_string (names[i]));
	  if (! NILP (font))
	    break;
	}

      if (NILP (font))
	error ("No suitable font was found");
    }

  gui_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}

static void
android_create_frame_window (struct frame *f)
{
  struct android_set_window_attributes attributes;
  enum android_window_value_mask attribute_mask;

  attributes.background_pixel = FRAME_BACKGROUND_PIXEL (f);
  attribute_mask = ANDROID_CW_BACK_PIXEL;

  block_input ();
  FRAME_ANDROID_WINDOW (f)
    = android_create_window (FRAME_DISPLAY_INFO (f)->root_window,
			     f->left_pos,
			     f->top_pos,
			     FRAME_PIXEL_WIDTH (f),
			     FRAME_PIXEL_HEIGHT (f),
			     attribute_mask, &attributes);
  unblock_input ();
}

#endif /* ANDROID_STUBIFY */



DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object parms)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only;
  bool undecorated, override_redirect;
  long window_prompting;
  specpdl_ref count;
  Lisp_Object display;
  struct android_display_info *dpyinfo;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;

  minibuffer_only = false;
  undecorated = false;
  override_redirect = false;
  window_prompting = 0;
  count = SPECPDL_INDEX ();
  dpyinfo = NULL;

  /* Not actually used, but be consistent with X.  */
  ((void) window_prompting);

  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0,
                                 RES_TYPE_NUMBER);
  if (BASE_EQ (display, Qunbound))
    display = gui_display_get_arg (dpyinfo, parms, Qdisplay, 0, 0,
                                   RES_TYPE_STRING);
  if (BASE_EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_android_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name",
                              RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! BASE_EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = gui_display_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL,
                                RES_TYPE_NUMBER);
  if (BASE_EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_FIXNUM (parent);

  frame = Qnil;
  tem = gui_display_get_arg (dpyinfo,
                             parms, Qminibuffer, "minibuffer", "Minibuffer",
                             RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = true;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (true);

  parent_frame = gui_display_get_arg (dpyinfo,
                                      parms,
                                      Qparent_frame,
                                      NULL,
                                      NULL,
                                      RES_TYPE_SYMBOL);
  /* Accept parent-frame iff parent-id was not specified.  */
  if (!NILP (parent)
      || BASE_EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame))
      || !FRAME_ANDROID_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo,
                                         parms,
                                         Qundecorated,
                                         NULL,
                                         NULL,
                                         RES_TYPE_BOOLEAN)))
      && !(BASE_EQ (tem, Qunbound)))
    undecorated = true;

  FRAME_UNDECORATED (f) = undecorated;
  store_frame_param (f, Qundecorated, undecorated ? Qt : Qnil);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo,
                                         parms,
                                         Qoverride_redirect,
                                         NULL,
                                         NULL,
                                         RES_TYPE_BOOLEAN)))
      && !(BASE_EQ (tem, Qunbound)))
    override_redirect = true;

  FRAME_OVERRIDE_REDIRECT (f) = override_redirect;
  store_frame_param (f, Qoverride_redirect, override_redirect ? Qt : Qnil);

  XSETFRAME (frame, f);

  frame_set_id_from_params (f, parms);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_android;
  f->output_data.android = xzalloc (sizeof *f->output_data.android);
  FRAME_FONTSET (f) = -1;
  f->output_data.android->scroll_bar_foreground_pixel = -1;
  f->output_data.android->scroll_bar_background_pixel = -1;
  f->output_data.android->white_relief.pixel = -1;
  f->output_data.android->black_relief.pixel = -1;

  fset_icon_name (f, gui_display_get_arg (dpyinfo,
                                          parms,
                                          Qicon_name,
                                          "iconName",
                                          "Title",
                                          RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (do_unwind_create_frame, frame);

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!

     (Not really on Android, but it's best to be consistent with
     X.) */
  {
    Lisp_Object black;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.android->cursor_pixel = -1;
    f->output_data.android->cursor_foreground_pixel = -1;
    f->output_data.android->mouse_pixel = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_foreground_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->mouse_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (BASE_EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string ("GNU Emacs"));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* Use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  register_font_driver (&androidfont_driver, f);
  register_font_driver (&android_sfntfont_driver, f);

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  android_default_font_parameter (f, parms);
  if (!FRAME_FONT (f))
    {
      delete_frame (frame, Qnoelisp);
      error ("Invalid frame font");
    }

  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
                                   "internalBorder", "internalBorder",
                                   RES_TYPE_NUMBER);
      if (! BASE_EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }

  gui_default_parameter (f, parms, Qinternal_border_width,
			 make_fixnum (0),
			 "internalBorderWidth", "internalBorderWidth",
			 RES_TYPE_NUMBER);

  /* Same for child frames.  */
  if (NILP (Fassq (Qchild_frame_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qchild_frame_border_width,
                                   "childFrameBorder", "childFrameBorder",
                                   RES_TYPE_NUMBER);
      if (! BASE_EQ (value, Qunbound))
	parms = Fcons (Fcons (Qchild_frame_border_width, value),
		       parms);
    }

  gui_default_parameter (f, parms, Qchild_frame_border_width, Qnil,
			 "childFrameBorderWidth", "childFrameBorderWidth",
			 RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);

  /* `vertical-scroll-bars' defaults to nil on Android as a
     consequence of scroll bars not being supported at all.  */

  gui_default_parameter (f, parms, Qvertical_scroll_bars, Qnil,
                         "verticalScrollBars", "ScrollBars",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil,
                         "horizontalScrollBars", "ScrollBars",
                         RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  gui_default_parameter (f, parms, Qforeground_color, build_string ("black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("white"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qmouse_color, build_string ("black"),
                         "pointerColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qborder_color, build_string ("black"),
                         "borderColor", "BorderColor", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qscreen_gamma, Qnil,
                         "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  gui_default_parameter (f, parms, Qline_spacing, Qnil,
                         "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qleft_fringe, Qnil,
                         "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_fringe, Qnil,
                         "rightFringe", "RightFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qno_special_glyphs, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

#if 0
  android_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_foreground,
					      "scrollBarForeground",
					      "ScrollBarForeground", true);
  android_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_background,
					      "scrollBarBackground",
					      "ScrollBarBackground", false);
#endif

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not
     happen.  */

  init_frame_faces (f);

  tem = gui_display_get_arg (dpyinfo, parms, Qmin_width, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_height, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_height, tem);

  adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		     FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 5, true,
		     Qx_create_frame_1);

  /* Set the menu-bar-lines and tool-bar-lines parameters.  We don't
     look up the X resources controlling the menu-bar and tool-bar
     here; they are processed specially at startup, and reflected in
     the values of the mode variables.  */

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

  gui_default_parameter (f, parms, Qbuffer_predicate, Qnil,
                         "bufferPredicate", "BufferPredicate",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qtitle, Qnil,
                         "title", "Title", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qwait_for_wm, Qt,
                         "waitForWM", "WaitForWM", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qtool_bar_position,
                         FRAME_TOOL_BAR_POSITION (f), 0, 0, RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  /* Compute the size of the X window.  */
  window_prompting = gui_figure_window_size (f, parms, true, true);

  tem = gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0,
                             RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  android_icon_verify (f, parms);
  android_create_frame_window (f);
  android_icon (f, parms);
  android_make_gc (f);

  /* Now consider the frame official.  */
  f->terminal->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the window, so that the
     icon-creation functions can say whose icon they're
     describing.  */
  gui_default_parameter (f, parms, Qicon_type, Qt,
                         "bitmapIcon", "BitmapIcon", RES_TYPE_BOOLEAN);

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
                         "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
                         "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
                         "cursorType", "CursorType", RES_TYPE_SYMBOL);
  /* Scroll bars are not supported on Android, as they are near
     useless.  */
  gui_default_parameter (f, parms, Qscroll_bar_width, Qnil,
                         "scrollBarWidth", "ScrollBarWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qscroll_bar_height, Qnil,
                         "scrollBarHeight", "ScrollBarHeight",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qalpha, Qnil,
                         "alpha", "Alpha", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qalpha_background, Qnil,
                         "alphaBackground", "AlphaBackground", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qborders_respect_alpha_background, Qnil,
                         "bordersRespectAlphaBackground",
                         "BordersRespectAlphaBackground", RES_TYPE_NUMBER);

  if (!NILP (parent_frame))
    {
      struct frame *p = XFRAME (parent_frame);

      block_input ();
      android_reparent_window (FRAME_ANDROID_WINDOW (f),
			       FRAME_ANDROID_WINDOW (p),
			       f->left_pos, f->top_pos);
      unblock_input ();
    }

  gui_default_parameter (f, parms, Qno_focus_on_map, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qno_accept_focus, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  /* Consider frame official, now.  */
  f->can_set_window_size = true;

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, Qx_create_frame_2);

  /* Process fullscreen parameter here in the hope that normalizing a
     fullheight/fullwidth frame will produce the size set by the last
     adjust_frame_size call.  Note that Android only supports the
     `maximized' state.  */
  gui_default_parameter (f, parms, Qfullscreen, Qmaximized,
                         "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  /* When called from `x-create-frame-with-faces' visibility is
     always explicitly nil.  */
  Lisp_Object visibility
    = gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
			   RES_TYPE_SYMBOL);
  Lisp_Object height
    = gui_display_get_arg (dpyinfo, parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  Lisp_Object width
    = gui_display_get_arg (dpyinfo, parms, Qwidth, 0, 0, RES_TYPE_NUMBER);

  if (EQ (visibility, Qicon))
    {
      f->was_invisible = true;
      android_iconify_frame (f);
    }
  else
    {
      if (BASE_EQ (visibility, Qunbound))
	visibility = Qt;

      if (!NILP (visibility))
	android_make_frame_visible (f);
      else
	f->was_invisible = true;
    }

  /* Leave f->was_invisible true only if height or width were
     specified too.  This takes effect only when we are not called
     from `x-create-frame-with-faces' (see above comment).  */
  f->was_invisible
    = (f->was_invisible
       && (!BASE_EQ (height, Qunbound) || !BASE_EQ (width, Qunbound)));

  store_frame_param (f, Qvisibility, visibility);

  /* Set whether or not frame synchronization is enabled.  */
  gui_default_parameter (f, parms, Quse_frame_synchronization, Qt,
			 NULL, NULL, RES_TYPE_BOOLEAN);

  /* Works iff frame has been already mapped.  */
  gui_default_parameter (f, parms, Qskip_taskbar, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);
  /* The `z-group' parameter works only for visible frames.  */
  gui_default_parameter (f, parms, Qz_group, Qnil,
                         NULL, NULL, RES_TYPE_SYMBOL);

  /* Initialize `default-minibuffer-frame' in case this is the first
     frame on this terminal.  */
  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
          || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
    kset_default_minibuffer_frame (kb, frame);

  /* All remaining specified parameters, which have not been "used" by
     gui_display_get_arg and friends, now go in the misc. alist of the
     frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
#endif
}

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p,
       1, 2, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  Emacs_Color foo;
  struct frame *f;

  f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (android_defined_color (f, SSDATA (color), &foo, false, false))
    return Qt;
  else
    return Qnil;
#endif
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2,
       0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  Emacs_Color foo;
  struct frame *f;

  f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (android_defined_color (f, SSDATA (color), &foo, false, false))
    return list3i (foo.red, foo.green, foo.blue);
  else
    return Qnil;
#endif
}

DEFUN ("xw-display-color-p", Fxw_display_color_p,
       Sxw_display_color_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct android_display_info *dpyinfo;

  dpyinfo = check_android_display_info (terminal);
  return dpyinfo->n_planes > 8 ? Qt : Qnil;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p,
       Sx_display_grayscale_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct android_display_info *dpyinfo;

  dpyinfo = check_android_display_info (terminal);
  return dpyinfo->n_planes > 1 ? Qt : Qnil;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width,
       Sx_display_pixel_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  return make_fixnum (android_get_screen_width ());
#endif
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  return make_fixnum (android_get_screen_height ());
#endif
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct android_display_info *dpyinfo;

  dpyinfo = check_android_display_info (terminal);

  return make_fixnum (dpyinfo->n_planes);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct android_display_info *dpyinfo;
  int nr_planes;

  dpyinfo = check_android_display_info (terminal);
  nr_planes = dpyinfo->n_planes;

  /* Truncate nr_planes to 24 to avoid integer overflow.  */

  if (nr_planes > 24)
    nr_planes = 24;

  return make_fixnum (1 << nr_planes);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  check_android_display_info (terminal);
  return Vandroid_build_manufacturer;
#endif
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  check_android_display_info (terminal);
  return list3i (android_get_current_api_level (), 0, 0);
#endif
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens,
       0, 1, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_android_display_info (terminal);
  return make_fixnum (1);
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width,
       0, 1, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  check_android_display_info (terminal);
  return make_fixnum (android_get_mm_width ());
#endif
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height,
       0, 1, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  check_android_display_info (terminal);
  return make_fixnum (android_get_mm_height ());
#endif
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_android_display_info (terminal);

  /* Window contents are preserved insofar as they remain mapped, in a
     fashion tantamount to WhenMapped.  */
  return Qwhen_mapped;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct android_display_info *dpyinfo;

  dpyinfo = check_android_display_info (terminal);

  if (dpyinfo->n_planes < 24)
    return Qstatic_gray;

  return Qtrue_color;
}

#ifndef ANDROID_STUBIFY

static Lisp_Object
android_make_monitor_attribute_list (struct MonitorInfo *monitors,
				     int n_monitors,
				     int primary_monitor)
{
  Lisp_Object monitor_frames;
  Lisp_Object frame, rest;
  struct frame *f;

  monitor_frames = make_nil_vector (n_monitors);

  FOR_EACH_FRAME (rest, frame)
    {
      f = XFRAME (frame);

      /* Associate all frames with the primary monitor.  */

      if (FRAME_WINDOW_P (f)
	  && !FRAME_TOOLTIP_P (f))
	ASET (monitor_frames, primary_monitor,
	      Fcons (frame, AREF (monitor_frames,
				  primary_monitor)));
    }

  return make_monitor_attribute_list (monitors, n_monitors,
				      primary_monitor,
                                      monitor_frames, NULL);
}

#endif

DEFUN ("android-display-monitor-attributes-list",
       Fandroid_display_monitor_attributes_list,
       Sandroid_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  struct MonitorInfo monitor;

  check_android_display_info (terminal);
  memset (&monitor, 0, sizeof monitor);
  monitor.geom.width = android_get_screen_width ();
  monitor.geom.height = android_get_screen_height ();
  monitor.mm_width = android_get_mm_width ();
  monitor.mm_height = android_get_mm_height ();
  monitor.work = monitor.geom;
  monitor.name = (char *) "Android device monitor";

  return android_make_monitor_attribute_list (&monitor, 1, 0);
#endif
}

#ifndef ANDROID_STUBIFY

static Lisp_Object
frame_geometry (Lisp_Object frame, Lisp_Object attribute)
{
  struct frame *f = decode_live_frame (frame);
  android_window rootw;
  unsigned int native_width, native_height, x_border_width = 0;
  int x_native = 0, y_native = 0, xptr = 0, yptr = 0;
  int left_off = 0, right_off = 0, top_off = 0, bottom_off = 0;
  int outer_left, outer_top, outer_right, outer_bottom;
  int native_left, native_top, native_right, native_bottom;
  int inner_left, inner_top, inner_right, inner_bottom;
  int internal_border_width;
  bool menu_bar_external = false, tool_bar_external = false;
  int menu_bar_height = 0, menu_bar_width = 0;
  int tab_bar_height = 0, tab_bar_width = 0;
  int tool_bar_height = 0, tool_bar_width = 0;

  if (FRAME_INITIAL_P (f) || !FRAME_ANDROID_P (f)
      || !FRAME_ANDROID_WINDOW (f))
    return Qnil;

  block_input ();
  android_get_geometry (FRAME_ANDROID_WINDOW (f),
			&rootw, &x_native, &y_native,
			&native_width, &native_height, &x_border_width);
  unblock_input ();

  if (FRAME_PARENT_FRAME (f))
    {
      Lisp_Object parent, edges;

      XSETFRAME (parent, FRAME_PARENT_FRAME (f));
      edges = Fandroid_frame_edges (parent, Qnative_edges);
      if (!NILP (edges))
	{
	  x_native += XFIXNUM (Fnth (make_fixnum (0), edges));
	  y_native += XFIXNUM (Fnth (make_fixnum (1), edges));
	}

      outer_left = x_native;
      outer_top = y_native;
      outer_right = outer_left + native_width + 2 * x_border_width;
      outer_bottom = outer_top + native_height + 2 * x_border_width;

      native_left = x_native + x_border_width;
      native_top = y_native + x_border_width;
      native_right = native_left + native_width;
      native_bottom = native_top + native_height;
    }
  else
    {
      outer_left = xptr;
      outer_top = yptr;
      outer_right = outer_left + left_off + native_width + right_off;
      outer_bottom = outer_top + top_off + native_height + bottom_off;

      native_left = outer_left + left_off;
      native_top = outer_top + top_off;
      native_right = native_left + native_width;
      native_bottom = native_top + native_height;
    }

  internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  inner_left = native_left + internal_border_width;
  inner_top = native_top + internal_border_width;
  inner_right = native_right - internal_border_width;
  inner_bottom = native_bottom - internal_border_width;

  menu_bar_height = FRAME_MENU_BAR_HEIGHT (f);
  inner_top += menu_bar_height;
  menu_bar_width = menu_bar_height ? native_width : 0;

  tab_bar_height = FRAME_TAB_BAR_HEIGHT (f);
  tab_bar_width = (tab_bar_height
		   ? native_width - 2 * internal_border_width
		    : 0);
  inner_top += tab_bar_height;

  tool_bar_height = FRAME_TOOL_BAR_HEIGHT (f);
  tool_bar_width = (tool_bar_height
		    ? native_width - 2 * internal_border_width
		    : 0);

  /* Subtract or add to the inner dimensions based on the tool bar
     position.  */

  if (EQ (FRAME_TOOL_BAR_POSITION (f), Qtop))
    inner_top += tool_bar_height;
  else
    inner_bottom -= tool_bar_height;

  /* Construct list.  */
  if (EQ (attribute, Qouter_edges))
    return list4i (outer_left, outer_top, outer_right, outer_bottom);
  else if (EQ (attribute, Qnative_edges))
    return list4i (native_left, native_top, native_right, native_bottom);
  else if (EQ (attribute, Qinner_edges))
    return list4i (inner_left, inner_top, inner_right, inner_bottom);
  else
    return
       list (Fcons (Qouter_position,
		    Fcons (make_fixnum (outer_left),
			   make_fixnum (outer_top))),
	     Fcons (Qouter_size,
		    Fcons (make_fixnum (outer_right - outer_left),
			   make_fixnum (outer_bottom - outer_top))),
	     /* Approximate.  */
	     Fcons (Qexternal_border_size,
		    Fcons (make_fixnum (right_off),
			   make_fixnum (bottom_off))),
	     Fcons (Qouter_border_width, make_fixnum (x_border_width)),
	     /* Approximate.  */
	     Fcons (Qtitle_bar_size,
		    Fcons (make_fixnum (0),
			   make_fixnum (top_off - bottom_off))),
	     Fcons (Qmenu_bar_external, menu_bar_external ? Qt : Qnil),
	     Fcons (Qmenu_bar_size,
		    Fcons (make_fixnum (menu_bar_width),
			   make_fixnum (menu_bar_height))),
	     Fcons (Qtab_bar_size,
		    Fcons (make_fixnum (tab_bar_width),
			   make_fixnum (tab_bar_height))),
	     Fcons (Qtool_bar_external, tool_bar_external ? Qt : Qnil),
	     Fcons (Qtool_bar_position, FRAME_TOOL_BAR_POSITION (f)),
	     Fcons (Qtool_bar_size,
		    Fcons (make_fixnum (tool_bar_width),
			   make_fixnum (tool_bar_height))),
	     Fcons (Qinternal_border_width,
		    make_fixnum (internal_border_width)));
}

#endif

DEFUN ("android-frame-geometry", Fandroid_frame_geometry,
       Sandroid_frame_geometry,
       0, 1, 0,
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
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  return frame_geometry (frame, Qnil);
#endif
}

DEFUN ("android-frame-edges", Fandroid_frame_edges,
       Sandroid_frame_edges, 0, 2, 0,
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
#ifndef ANDROID_STUBIFY
  return frame_geometry (frame, ((EQ (type, Qouter_edges)
				  || EQ (type, Qinner_edges))
				 ? type
				 : Qnative_edges));
#else
  return Qnil;
#endif
}

#ifndef ANDROID_STUBIFY

static Lisp_Object
android_frame_list_z_order (struct android_display_info *dpyinfo,
			    android_window window)
{
  android_window root, parent, *children;
  unsigned int nchildren;
  unsigned long i;
  Lisp_Object frames;

  frames = Qnil;

  if (android_query_tree (window, &root, &parent,
			  &children, &nchildren))
    {
      for (i = 0; i < nchildren; i++)
	{
	  Lisp_Object frame, tail;

	  FOR_EACH_FRAME (tail, frame)
            {
              struct frame *cf = XFRAME (frame);

              if (FRAME_ANDROID_P (cf)
                  && (FRAME_ANDROID_WINDOW (cf) == children[i]))
                frames = Fcons (frame, frames);
            }
	}

      if (children)
	xfree (children);
    }

  return frames;
}

#endif

DEFUN ("android-frame-list-z-order", Fandroid_frame_list_z_order,
       Sandroid_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs's frames, in Z (stacking) order.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be either a frame or a display name (a string).  If
omitted or nil, that stands for the selected frame's display.  Return
nil if TERMINAL contains no Emacs frame.

As a special case, if TERMINAL is non-nil and specifies a live frame,
return the child frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).

On Android, the order of the frames returned is undefined unless
TERMINAL is a frame.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  struct android_display_info *dpyinfo;
  android_window window;

  dpyinfo = check_android_display_info (terminal);

  if (FRAMEP (terminal) && FRAME_LIVE_P (XFRAME (terminal)))
    window = FRAME_ANDROID_WINDOW (XFRAME (terminal));
  else
    window = dpyinfo->root_window;

  return android_frame_list_z_order (dpyinfo, window);
#endif
}

#ifndef ANDROID_STUBIFY

static void
android_frame_restack (struct frame *f1, struct frame *f2,
		       bool above_flag)
{
  android_window window1;
  struct android_window_changes wc;
  unsigned long mask;

  window1 = FRAME_ANDROID_WINDOW (f1);
  wc.sibling = FRAME_ANDROID_WINDOW (f2);
  wc.stack_mode = above_flag ? ANDROID_ABOVE : ANDROID_BELOW;
  mask = ANDROID_CW_SIBLING | ANDROID_CW_STACK_MODE;

  block_input ();
  android_reconfigure_wm_window (window1, mask, &wc);
  unblock_input ();
}

#endif /* !ANDROID_STUBIFY */

DEFUN ("android-frame-restack", Fandroid_frame_restack,
       Sandroid_frame_restack, 2, 3, 0,
       doc: /* Restack FRAME1 below FRAME2.
This means that if both frames are visible and the display areas of
these frames overlap, FRAME2 (partially) obscures FRAME1.  If optional
third argument ABOVE is non-nil, restack FRAME1 above FRAME2.  This
means that if both frames are visible and the display areas of these
frames overlap, FRAME1 (partially) obscures FRAME2.

This may be thought of as an atomic action performed in two steps: The
first step removes FRAME1's window-step window from the display.  The
second step reinserts FRAME1's window below (above if ABOVE is true)
that of FRAME2.  Hence the position of FRAME2 in its display's Z
\(stacking) order relative to all other frames excluding FRAME1 remains
unaltered.

Android does not facilitate restacking top-level windows managed by
its own window manager; nor is it possible to restack frames that are
children of different parents.  Consequently, this function only
functions when FRAME1 and FRAME2 are both child frames subordinate to
the same parent frame.  */)
  (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object above)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else /* !ANDROID_STUBIFY */
  struct frame *f1 = decode_live_frame (frame1);
  struct frame *f2 = decode_live_frame (frame2);

  if (!(FRAME_ANDROID_WINDOW (f1) && FRAME_ANDROID_WINDOW (f2)))
    error ("Cannot restack frames");
  android_frame_restack (f1, f2, !NILP (above));
  return Qt;
#endif /* ANDROID_STUBIFY */
}

DEFUN ("android-mouse-absolute-pixel-position",
       Fandroid_mouse_absolute_pixel_position,
       Sandroid_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the coordinates of
the mouse cursor position in pixels relative to a position (0, 0) of the
selected frame's display.  This does not work on Android.  */)
  (void)
{
  /* This cannot be implemented on Android.  */
  return Qnil;
}

DEFUN ("android-set-mouse-absolute-pixel-position",
       Fandroid_set_mouse_absolute_pixel_position,
       Sandroid_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to a pixel position at (X, Y).  The
coordinates X and Y are interpreted to start from the top-left corner
of the screen.  This does not work on Android.  */)
  (Lisp_Object x, Lisp_Object y)
{
  /* This cannot be implemented on Android.  */
  return Qnil;
}

DEFUN ("android-get-connection", Fandroid_get_connection,
       Sandroid_get_connection, 0, 0, 0,
       doc: /* Get the connection to the display server.
Return the terminal if it exists, else nil.

Emacs cannot open a connection to the display server itself under
Android, so there is no equivalent of `x-open-connection'.  */)
  (void)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  Lisp_Object terminal;

  terminal = Qnil;

  if (x_display_list)
    {
      XSETTERMINAL (terminal, x_display_list->terminal);

      /* Update the display's bit depth from
	 `android_display_planes'.  */
      x_display_list->n_planes
	= (android_display_planes > 8
	   ? 24 : (android_display_planes > 1
		   ? android_display_planes : 1));
    }

  return terminal;
#endif
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  Lisp_Object result;

  result = Qnil;

  if (x_display_list)
    result = Fcons (XCAR (x_display_list->name_list_element),
		    result);

  return result;
}

#ifndef ANDROID_STUBIFY

static void
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = ANDROID_NONE;
      tip_frame = Qnil;
    }
}

static Lisp_Object
android_create_tip_frame (struct android_display_info *dpyinfo,
			  Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame;
  Lisp_Object name;
  specpdl_ref count = SPECPDL_INDEX ();
  bool face_change_before = face_change;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  parms = Fcopy_alist (parms);

  /* Get the name of the frame to use for resource lookup.  */
  name = gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name",
                              RES_TYPE_STRING);
  if (!STRINGP (name)
      && !BASE_EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  frame = Qnil;
  f = make_frame (false);
  f->wants_modeline = false;
  XSETFRAME (frame, f);
  record_unwind_protect (unwind_create_tip_frame, frame);

  f->terminal = dpyinfo->terminal;

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_android;
  f->output_data.android = xzalloc (sizeof *f->output_data.android);
  FRAME_FONTSET (f) = -1;
  f->output_data.android->white_relief.pixel = -1;
  f->output_data.android->black_relief.pixel = -1;

  f->tooltip = true;
  fset_icon_name (f, Qnil);
  FRAME_DISPLAY_INFO (f) = dpyinfo;
  f->output_data.android->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;

    /* Function android_decode_color can signal an error.  Make sure
       to initialize color slots so that we won't try to free colors
       we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.android->cursor_pixel = -1;
    f->output_data.android->cursor_foreground_pixel = -1;
    f->output_data.android->mouse_pixel = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_foreground_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->mouse_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (BASE_EQ (name, Qunbound) || NILP (name))
    f->explicit_name = false;
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  register_font_driver (&androidfont_driver, f);
  register_font_driver (&android_sfntfont_driver, f);

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  android_default_font_parameter (f, parms);

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                         "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 1 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
                                   "internalBorder", "internalBorder",
                                   RES_TYPE_NUMBER);
      if (! BASE_EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }

  gui_default_parameter (f, parms, Qinternal_border_width, make_fixnum (1),
                         "internalBorderWidth", "internalBorderWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);

  /* Also do the stuff which must be set before the window exists.  */
  gui_default_parameter (f, parms, Qforeground_color, build_string ("black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("white"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qmouse_color, build_string ("black"),
                         "pointerColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qcursor_color, build_string ("black"),
                         "cursorColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qborder_color, build_string ("black"),
                         "borderColor", "BorderColor", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qno_special_glyphs, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  {
    struct android_set_window_attributes attrs;
    unsigned long mask;

    block_input ();
    mask = ANDROID_CW_OVERRIDE_REDIRECT | ANDROID_CW_BACK_PIXEL;

    attrs.override_redirect = true;
    attrs.background_pixel = FRAME_BACKGROUND_PIXEL (f);
    tip_window
      = FRAME_ANDROID_WINDOW (f)
      = android_create_window (FRAME_DISPLAY_INFO (f)->root_window,
			       /* x, y, width, height, value-mask,
				  attrs.  */
			       0, 0, 1, 1, mask, &attrs);
    unblock_input ();
  }

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  gui_figure_window_size (f, parms, false, false);

  f->output_data.android->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;

  android_make_gc (f);

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
                         "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
                         "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
                         "cursorType", "CursorType", RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qalpha, Qnil,
                         "alpha", "Alpha", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qalpha_background, Qnil,
                         "alphaBackground", "AlphaBackground", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qborders_respect_alpha_background, Qnil,
                         "bordersRespectAlphaBackground",
                         "BordersRespectAlphaBackground", RES_TYPE_NUMBER);

  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, Qtooltip)))
    {
      AUTO_FRAME_ARG (arg, Qtooltip, Qt);
      Fmodify_frame_parameters (frame, arg);
    }

  /* FIXME - can this be done in a similar way to normal frames?
     https://lists.gnu.org/r/emacs-devel/2007-10/msg00641.html */

  /* Set the `display-type' frame parameter before setting up faces. */
  {
    Lisp_Object disptype;

    disptype = Qcolor;

    if (NILP (Fframe_parameter (frame, Qdisplay_type)))
      {
	AUTO_FRAME_ARG (arg, Qdisplay_type, disptype);
	Fmodify_frame_parameters (frame, arg);
      }
  }

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    calln (Qface_set_after_frame_default, frame, Qnil);

    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      {
	AUTO_FRAME_ARG (arg, Qbackground_color, bg);
	Fmodify_frame_parameters (frame, arg);
      }
  }

  f->no_split = true;

  /* Now that the frame will be official, it counts as a reference to
     its display and terminal.  */
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);
  f->can_set_window_size = true;
  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, Qtip_frame);

  /* Setting attributes of faces of the tooltip frame from resources
     and similar will set face_change, which leads to the clearing of
     all current matrices.  Since this isn't necessary here, avoid it
     by resetting face_change to the value it had before we created
     the tip frame.  */
  face_change = face_change_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}

static Lisp_Object
android_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      calln (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

  if (NILP (tip_frame)
      || (!delete
	  && !NILP (tip_frame)
	  && FRAME_LIVE_P (XFRAME (tip_frame))
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    return Qnil;
  else
    {
      Lisp_Object was_open = Qnil;

      specpdl_ref count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      if (!NILP (tip_frame))
	{
	  struct frame *f = XFRAME (tip_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (delete)
		{
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		android_make_frame_invisible (XFRAME (tip_frame));

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
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx,
		Lisp_Object dy, int width, int height, int *root_x,
		int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int min_x, min_y, max_x, max_y = -1;
  android_window window;
  struct frame *mouse_frame;

  /* Initialize these values in case there is no mouse frame.  */
  *root_x = 0;
  *root_y = 0;

  /* User-specified position?  */
  left = CDR (Fassq (Qleft, parms));
  top  = CDR (Fassq (Qtop, parms));
  right = CDR (Fassq (Qright, parms));
  bottom = CDR (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer was last seen.
     Resize and show it.  */
  if ((!FIXNUMP (left) && !FIXNUMP (right))
      || (!FIXNUMP (top) && !FIXNUMP (bottom)))
    {
      if (x_display_list->last_mouse_motion_frame)
	{
	  *root_x = x_display_list->last_mouse_motion_x;
	  *root_y = x_display_list->last_mouse_motion_y;
	  mouse_frame = x_display_list->last_mouse_motion_frame;
	  window = FRAME_ANDROID_WINDOW (mouse_frame);

	  /* Translate the coordinates to the screen.  */
	  android_translate_coordinates (window, *root_x, *root_y,
					 root_x, root_y);
	}
    }

  min_x = 0;
  min_y = 0;
  max_x = android_get_screen_width ();
  max_y = android_get_screen_height ();

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

#endif

DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  struct frame *f, *tip_f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int width, height;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object window, size, tip_buf;
  bool displayed;
#ifdef ENABLE_CHECKING
  struct glyph_row *row, *end;
#endif
  AUTO_STRING (tip, " *tip*");

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);
  if (SCHARS (string) == 0)
    string = make_unibyte_string (" ", 1);

  if (NILP (frame))
    frame = selected_frame;
  f = decode_window_system_frame (frame);

  if (NILP (timeout))
    timeout = Vx_show_tooltip_timeout;
  CHECK_FIXNAT (timeout);

  if (NILP (dx))
    dx = make_fixnum (5);
  else
    CHECK_FIXNUM (dx);

  if (NILP (dy))
    dy = make_fixnum (-10);
  else
    CHECK_FIXNUM (dy);

  tip_dx = dx;
  tip_dy = dy;

  if (!NILP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	  && !NILP (Fequal_including_properties (tip_last_string,
						 string))
	  && !NILP (Fequal (tip_last_parms, parms)))
	{
	  /* Only DX and DY have changed.  */
	  tip_f = XFRAME (tip_frame);
	  if (!NILP (tip_timer))
	    {
	      calln (Qcancel_timer, tip_timer);
	      tip_timer = Qnil;
	    }

	  block_input ();
	  compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			  FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);
	  android_move_window (FRAME_ANDROID_WINDOW (tip_f),
			       root_x, root_y);
	  unblock_input ();

	  goto start_timer;
	}
      else if (tooltip_reuse_hidden_frame && BASE_EQ (frame, tip_last_frame))
	{
	  bool delete = false;
	  Lisp_Object tail, elt, parm, last;

	  /* Check if every parameter in PARMS has the same value in
	     tip_last_parms.  This may destruct tip_last_parms which,
	     however, will be recreated below.  */
	  for (tail = parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = CAR (elt);
	      /* The left, top, right and bottom parameters are handled
		 by compute_tip_xy so they can be ignored here.  */
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop)
		  && !EQ (parm, Qright) && !EQ (parm, Qbottom))
		{
		  last = Fassq (parm, tip_last_parms);
		  if (NILP (Fequal (CDR (elt), CDR (last))))
		    {
		      /* We lost, delete the old tooltip.  */
		      delete = true;
		      break;
		    }
		  else
		    tip_last_parms
		      = calln (Qassq_delete_all, parm, tip_last_parms);
		}
	      else
		tip_last_parms
		  = calln (Qassq_delete_all, parm, tip_last_parms);
	    }

	  /* Now check if every parameter in what is left of
	     tip_last_parms with a non-nil value has an association in
	     PARMS.  */
	  for (tail = tip_last_parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = CAR (elt);
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop) && !EQ (parm, Qright)
		  && !EQ (parm, Qbottom) && !NILP (CDR (elt)))
		{
		  /* We lost, delete the old tooltip.  */
		  delete = true;
		  break;
		}
	    }

	  android_hide_tip (delete);
	}
      else
	android_hide_tip (true);
    }
  else
    android_hide_tip (true);

  tip_last_frame = frame;
  tip_last_string = string;
  tip_last_parms = parms;

  if (NILP (tip_frame) || !FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      /* Add default values to frame parameters.  */
      if (NILP (Fassq (Qname, parms)))
	parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
      if (NILP (Fassq (Qinternal_border_width, parms)))
	parms = Fcons (Fcons (Qinternal_border_width, make_fixnum (3)),
		       parms);
      if (NILP (Fassq (Qborder_width, parms)))
	parms = Fcons (Fcons (Qborder_width, make_fixnum (1)), parms);
      if (NILP (Fassq (Qborder_color, parms)))
	parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")),
		       parms);
      if (NILP (Fassq (Qbackground_color, parms)))
	parms = Fcons (Fcons (Qbackground_color,
			      build_string ("lightyellow")),
		       parms);

      /* Create a frame for the tooltip, and record it in the global
	 variable tip_frame.  */
      if (NILP (tip_frame = android_create_tip_frame (FRAME_DISPLAY_INFO (f),
						      parms)))
	/* Creating the tip frame failed.  */
	return unbind_to (count, Qnil);
    }
  else
    tip_window = FRAME_ANDROID_WINDOW (XFRAME (tip_frame));

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
  FRAME_TOTAL_COLS (tip_f) = w->total_cols;
  adjust_frame_glyphs (tip_f);

  /* Insert STRING into root window's buffer and fit the frame to the
     buffer.  */
  specpdl_ref count_1 = SPECPDL_INDEX ();
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
  displayed = try_window (window, pos, TRY_WINDOW_IGNORE_FONTS_CHANGE);

  if (!displayed && NILP (Vx_max_tooltip_size))
    {
#ifdef ENABLE_CHECKING
      row = w->desired_matrix->rows;
      end = w->desired_matrix->rows + w->desired_matrix->nrows;

      while (row < end)
	{
	  if (!row->displays_text_p
	      || row->ends_at_zv_p)
	    break;
	  ++row;
	}

      eassert (row < end && row->ends_at_zv_p);
#endif
    }

  /* Calculate size of tooltip window.  */
  size = Fwindow_text_pixel_size (window, Qnil, Qnil, Qnil,
				  make_fixnum (w->pixel_height), Qnil,
				  Qnil);
  /* Add the frame's internal border to calculated size.  */
  width = XFIXNUM (CAR (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  height = XFIXNUM (CDR (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);

  /* Calculate position of tooltip frame.  */
  compute_tip_xy (tip_f, parms, dx, dy, width, height, &root_x, &root_y);

  /* Show tooltip frame.  */
  block_input ();
  android_move_resize_window (FRAME_ANDROID_WINDOW (tip_f),
			      root_x, root_y, width,
			      height);
  android_map_raised (FRAME_ANDROID_WINDOW (tip_f));
  unblock_input ();

  /* Garbage the tip frame too.  */
  SET_FRAME_GARBAGED (tip_f);

  /* Block input around `update_single_window' and `flush_frame', lest a
     ConfigureNotify and Expose event arrive during the update, and set
     flags, e.g. garbaged_p, that are cleared once the update completes,
     leaving the requested exposure or configuration outstanding.  */
  block_input ();
  w->must_be_updated_p = true;
  update_single_window (w);
  flush_frame (tip_f);
  unblock_input ();

  set_buffer_internal_1 (old_buffer);
  unbind_to (count_1, Qnil);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

  /* MapNotify events are not sent on Android, so make the frame
     visible.  */

  SET_FRAME_VISIBLE (tip_f, true);

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = calln (Qrun_at_time, timeout, Qnil, Qx_hide_tip);

  return unbind_to (count, Qnil);
#endif
}

DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
#ifdef ANDROID_STUBIFY
  /* Fx_hide_tip is called from pre-command-hook (in turn called from
     the tests.)  Since signaling here prevents any tests from being
     run, refrain from protesting if this stub is called.  */
#if 0
  error ("Android cross-compilation stub called!");
#endif /* 0 */
  return Qnil;
#else /* !ANDROID_STUBIFY */
  return android_hide_tip (!tooltip_reuse_hidden_frame);
#endif /* ANDROID_STUBIFY */
}

DEFUN ("android-detect-mouse", Fandroid_detect_mouse,
       Sandroid_detect_mouse, 0, 0, 0,
       doc: /* Figure out whether or not there is a mouse.
Return non-nil if a mouse is connected to this computer, and nil if
there is no mouse.  */)
  (void)
{
#ifndef ANDROID_STUBIFY
  /* If no display connection is present, just return nil.  */

  if (!android_init_gui)
    return Qnil;

  return android_detect_mouse () ? Qt : Qnil;
#else
  return Qnil;
#endif
}

DEFUN ("android-detect-keyboard", Fandroid_detect_keyboard,
       Sandroid_detect_keyboard, 0, 0, 0,
       doc: /* Return whether a keyboard is connected.
Return non-nil if a key is connected to this computer, or nil
if there is no keyboard.  */)
  (void)
{
#ifndef ANDROID_STUBIFY
  /* If no display connection is present, just return nil.  */

  if (!android_init_gui)
    return Qnil;

  return android_detect_keyboard () ? Qt : Qnil;
#else /* ANDROID_STUBIFY */
  return Qt;
#endif /* ANDROID_STUBIFY */
}

DEFUN ("android-toggle-on-screen-keyboard",
       Fandroid_toggle_on_screen_keyboard,
       Sandroid_toggle_on_screen_keyboard, 2, 2, 0,
       doc: /* Display or hide the on-screen keyboard.
If HIDE is non-nil, hide the on screen keyboard if it is currently
being displayed.  Else, request that the system display it on behalf
of FRAME.  This request may be rejected if FRAME does not have the
input focus.  */)
  (Lisp_Object frame, Lisp_Object hide)
{
#ifndef ANDROID_STUBIFY
  struct frame *f;

  f = decode_window_system_frame (frame);

  block_input ();
  android_toggle_on_screen_keyboard (FRAME_ANDROID_WINDOW (f),
				     NILP (hide));
  unblock_input ();
#endif

  return Qnil;
}



#ifndef ANDROID_STUBIFY

static void
android_set_background_color (struct frame *f, Lisp_Object arg,
			      Lisp_Object oldval)
{
  struct android_output *x;
  unsigned long bg;

  x = f->output_data.android;
  bg = android_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  FRAME_BACKGROUND_PIXEL (f) = bg;

  if (FRAME_ANDROID_WINDOW (f) != 0)
    {
      block_input ();
      android_set_background (x->normal_gc, bg);
      android_set_foreground (x->reverse_gc, bg);
      android_set_window_background (FRAME_ANDROID_WINDOW (f), bg);
      android_set_foreground (x->cursor_gc, bg);
      unblock_input ();

      update_face_from_frame_parameter (f, Qbackground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

static void
android_set_border_color (struct frame *f, Lisp_Object arg,
			  Lisp_Object oldval)
{
  /* Left unimplemented because Android has no window borders.  */
  CHECK_STRING (arg);
  android_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  update_face_from_frame_parameter (f, Qborder_color, arg);
}

static void
android_set_cursor_color (struct frame *f, Lisp_Object arg,
			  Lisp_Object oldval)
{
  unsigned long fore_pixel, pixel;
  struct android_output *x;

  x = f->output_data.android;

  if (!NILP (Vx_cursor_fore_pixel))
    fore_pixel = android_decode_color (f, Vx_cursor_fore_pixel,
				       WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  pixel = android_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      pixel = FRAME_FOREGROUND_PIXEL (f);
      if (pixel == fore_pixel)
	fore_pixel = FRAME_BACKGROUND_PIXEL (f);
    }

  x->cursor_foreground_pixel = fore_pixel;
  x->cursor_pixel = pixel;

  if (FRAME_ANDROID_WINDOW (f) != 0)
    {
      block_input ();
      android_set_background (x->cursor_gc, x->cursor_pixel);
      android_set_foreground (x->cursor_gc, fore_pixel);
      unblock_input ();

      if (FRAME_VISIBLE_P (f))
	{
	  gui_update_cursor (f, false);
	  gui_update_cursor (f, true);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

static void
android_set_cursor_type (struct frame *f, Lisp_Object arg,
			 Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

static void
android_set_foreground_color (struct frame *f, Lisp_Object arg,
			      Lisp_Object oldval)
{
  struct android_output *x;
  unsigned long fg, old_fg;

  x = f->output_data.android;

  fg = android_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_ANDROID_WINDOW (f) != 0)
    {
      block_input ();
      android_set_foreground (x->normal_gc, fg);
      android_set_background (x->reverse_gc, fg);

      if (x->cursor_pixel == old_fg)
	{
	  x->cursor_pixel = fg;
	  android_set_background (x->cursor_gc, x->cursor_pixel);
	}

      unblock_input ();

      update_face_from_frame_parameter (f, Qforeground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

static void
android_set_child_frame_border_width (struct frame *f, Lisp_Object arg,
				      Lisp_Object oldval)
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

      if (FRAME_ANDROID_WINDOW (f))
	{
	  adjust_frame_size (f, -1, -1, 3, false, Qchild_frame_border_width);
	  android_clear_under_internal_border (f);
	}
    }
}

static void
android_set_internal_border_width (struct frame *f, Lisp_Object arg,
				   Lisp_Object oldval)
{
  int border = check_int_nonnegative (arg);

  if (border != FRAME_INTERNAL_BORDER_WIDTH (f))
    {
      f->internal_border_width = border;

      if (FRAME_ANDROID_WINDOW (f))
	{
	  adjust_frame_size (f, -1, -1, 3, false, Qinternal_border_width);
	  android_clear_under_internal_border (f);
	}
    }
}

static void
android_set_menu_bar_lines (struct frame *f, Lisp_Object value,
			    Lisp_Object oldval)
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f) || FRAME_PARENT_FRAME (f))
    return;

  if (TYPE_RANGED_FIXNUMP (int, value))
    nlines = XFIXNUM (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  FRAME_MENU_BAR_LINES (f) = nlines;
  FRAME_MENU_BAR_HEIGHT (f) = nlines * FRAME_LINE_HEIGHT (f);
  if (FRAME_ANDROID_WINDOW (f))
    android_clear_under_internal_border (f);

  /* If the menu bar height gets changed, the internal border below
     the top margin has to be cleared.  Also, if the menu bar gets
     larger, the area for the added lines has to be cleared except for
     the first menu bar line that is to be drawn later.  */
  if (nlines != olines)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int y;

      adjust_frame_size (f, -1, -1, 3, true, Qmenu_bar_lines);

      /* height can be zero here. */
      if (FRAME_ANDROID_WINDOW (f) && height > 0 && width > 0)
	{
	  y = FRAME_TOP_MARGIN_HEIGHT (f);

	  block_input ();
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f),
			      0, y, width, height);
	  unblock_input ();
	}

      if (nlines > 1 && nlines > olines)
	{
	  y = (olines == 0 ? 1 : olines) * FRAME_LINE_HEIGHT (f);
	  height = nlines * FRAME_LINE_HEIGHT (f) - y;

	  block_input ();
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f), 0, y,
			      width, height);
	  unblock_input ();
	}

      if (nlines == 0 && WINDOWP (f->menu_bar_window))
	clear_glyph_matrix (XWINDOW (f->menu_bar_window)->current_matrix);
    }

  adjust_frame_glyphs (f);
}



/* These enums must stay in sync with the mouse_cursor_types array
   below!  */

enum mouse_cursor
  {
    mouse_cursor_text,
    mouse_cursor_nontext,
    mouse_cursor_hourglass,
    mouse_cursor_mode,
    mouse_cursor_hand,
    mouse_cursor_horizontal_drag,
    mouse_cursor_vertical_drag,
    mouse_cursor_left_edge,
    mouse_cursor_top_left_corner,
    mouse_cursor_top_edge,
    mouse_cursor_top_right_corner,
    mouse_cursor_right_edge,
    mouse_cursor_bottom_right_corner,
    mouse_cursor_bottom_edge,
    mouse_cursor_bottom_left_corner,
    mouse_cursor_max
  };

struct mouse_cursor_types
{
  /* Printable name for error messages (optional).  */
  const char *name;

  /* Lisp variable controlling the cursor shape.  */
  /* FIXME: A couple of these variables are defined in the C code but
     are not actually accessible from Lisp.  They should probably be
     made accessible or removed.  */
  Lisp_Object *shape_var_ptr;

  /* The default shape.  */
  int default_shape;
};

/* This array must stay in sync with enum mouse_cursor above!  */

static const struct mouse_cursor_types mouse_cursor_types[] =
  {
    {"text", &Vx_pointer_shape, ANDROID_XC_XTERM, },
    {"nontext", &Vx_nontext_pointer_shape, ANDROID_XC_LEFT_PTR, },
    {"hourglass", &Vx_hourglass_pointer_shape, ANDROID_XC_WATCH, },
    {"modeline", &Vx_mode_pointer_shape, ANDROID_XC_XTERM, },
    {NULL, &Vx_sensitive_text_pointer_shape, ANDROID_XC_HAND2, },
    {NULL, &Vx_window_horizontal_drag_shape, ANDROID_XC_SB_H_DOUBLE_ARROW, },
    {NULL, &Vx_window_vertical_drag_shape, ANDROID_XC_SB_V_DOUBLE_ARROW, },
    {NULL, &Vx_window_left_edge_shape, ANDROID_XC_LEFT_SIDE, },
    {NULL, &Vx_window_top_left_corner_shape, ANDROID_XC_TOP_LEFT_CORNER, },
    {NULL, &Vx_window_top_edge_shape, ANDROID_XC_TOP_SIDE, },
    {NULL, &Vx_window_top_right_corner_shape, ANDROID_XC_TOP_RIGHT_CORNER, },
    {NULL, &Vx_window_right_edge_shape, ANDROID_XC_RIGHT_SIDE, },
    {NULL, &Vx_window_bottom_right_corner_shape,
     ANDROID_XC_BOTTOM_RIGHT_CORNER, },
    {NULL, &Vx_window_bottom_edge_shape, ANDROID_XC_BOTTOM_SIDE, },
    {NULL, &Vx_window_bottom_left_corner_shape,
	ANDROID_XC_BOTTOM_LEFT_CORNER, },
  };

struct mouse_cursor_data
{
  /* Cursor numbers chosen.  */
  unsigned int cursor_num[mouse_cursor_max];

  /* Allocated Cursor values, or zero for failed attempts.  */
  android_cursor cursor[mouse_cursor_max];
};



static void
android_set_mouse_color (struct frame *f, Lisp_Object arg,
			 Lisp_Object oldval)
{
  struct android_output *x = f->output_data.android;
  struct mouse_cursor_data cursor_data = { -1, -1 };
  unsigned long pixel = android_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = FRAME_BACKGROUND_PIXEL (f);
  int i;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    pixel = FRAME_FOREGROUND_PIXEL (f);

  x->mouse_pixel = pixel;

  for (i = 0; i < mouse_cursor_max; i++)
    {
      Lisp_Object shape_var = *mouse_cursor_types[i].shape_var_ptr;
      cursor_data.cursor_num[i]
	= (!NILP (shape_var)
	   ? check_uinteger_max (shape_var, UINT_MAX)
	   : mouse_cursor_types[i].default_shape);
    }

  block_input ();

  for (i = 0; i < mouse_cursor_max; i++)
    cursor_data.cursor[i]
      = android_create_font_cursor (cursor_data.cursor_num[i]);

  if (FRAME_ANDROID_WINDOW (f))
    {
      f->output_data.android->current_cursor
	= cursor_data.cursor[mouse_cursor_text];
      android_define_cursor (FRAME_ANDROID_WINDOW (f),
			     f->output_data.android->current_cursor);
    }

#define INSTALL_CURSOR(FIELD, SHORT_INDEX)				\
   eassert (x->FIELD							\
	    != cursor_data.cursor[mouse_cursor_ ## SHORT_INDEX]);	\
   if (x->FIELD != 0)							\
     android_free_cursor (x->FIELD);					\
   x->FIELD = cursor_data.cursor[mouse_cursor_ ## SHORT_INDEX];

  INSTALL_CURSOR (text_cursor, text);
  INSTALL_CURSOR (nontext_cursor, nontext);
  INSTALL_CURSOR (hourglass_cursor, hourglass);
  INSTALL_CURSOR (modeline_cursor, mode);
  INSTALL_CURSOR (hand_cursor, hand);
  INSTALL_CURSOR (horizontal_drag_cursor, horizontal_drag);
  INSTALL_CURSOR (vertical_drag_cursor, vertical_drag);
  INSTALL_CURSOR (left_edge_cursor, left_edge);
  INSTALL_CURSOR (top_left_corner_cursor, top_left_corner);
  INSTALL_CURSOR (top_edge_cursor, top_edge);
  INSTALL_CURSOR (top_right_corner_cursor, top_right_corner);
  INSTALL_CURSOR (right_edge_cursor, right_edge);
  INSTALL_CURSOR (bottom_right_corner_cursor, bottom_right_corner);
  INSTALL_CURSOR (bottom_edge_cursor, bottom_edge);
  INSTALL_CURSOR (bottom_left_corner_cursor, bottom_left_corner);

#undef INSTALL_CURSOR

  unblock_input ();

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

static void
android_set_title (struct frame *f, Lisp_Object name,
		   Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 38;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  android_set_name_internal (f, name);
}

static void
android_set_alpha (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  double alpha = 1.0;
  double newval[2];
  int i;
  Lisp_Object item;

  /* N.B. that setting the window alpha is actually unsupported under
     Android.  */

  for (i = 0; i < 2; i++)
    {
      newval[i] = 1.0;
      if (CONSP (arg))
        {
          item = CAR (arg);
          arg  = CDR (arg);
        }
      else
        item = arg;

      if (NILP (item))
	alpha = - 1.0;
      else if (FLOATP (item))
	{
	  alpha = XFLOAT_DATA (item);
	  if (! (0 <= alpha && alpha <= 1.0))
	    args_out_of_range (make_float (0.0), make_float (1.0));
	}
      else if (FIXNUMP (item))
	{
	  EMACS_INT ialpha = XFIXNUM (item);
	  if (! (0 <= ialpha && ialpha <= 100))
	    args_out_of_range (make_fixnum (0), make_fixnum (100));
	  alpha = ialpha / 100.0;
	}
      else
	wrong_type_argument (Qnumberp, item);
      newval[i] = alpha;
    }

  for (i = 0; i < 2; i++)
    f->alpha[i] = newval[i];

  if (FRAME_TERMINAL (f)->set_frame_alpha_hook)
    {
      block_input ();
      FRAME_TERMINAL (f)->set_frame_alpha_hook (f);
      unblock_input ();
    }
}

static void
android_set_no_focus_on_map (struct frame *f, Lisp_Object new_value,
			     Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      android_set_dont_focus_on_map (FRAME_ANDROID_WINDOW (f),
				     !NILP (new_value));
      FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
    }
}

static void
android_set_no_accept_focus (struct frame *f, Lisp_Object new_value,
			     Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      android_set_dont_accept_focus (FRAME_ANDROID_WINDOW (f),
				     !NILP (new_value));
      FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
    }
}

frame_parm_handler android_frame_parm_handlers[] =
{
  gui_set_autoraise,
  gui_set_autolower,
  android_set_background_color,
  android_set_border_color,
  gui_set_border_width,
  android_set_cursor_color,
  android_set_cursor_type,
  gui_set_font,
  android_set_foreground_color,
  NULL,
  NULL,
  android_set_child_frame_border_width,
  android_set_internal_border_width,
  gui_set_right_divider_width,
  gui_set_bottom_divider_width,
  android_set_menu_bar_lines,
  android_set_mouse_color,
  android_explicitly_set_name,
  gui_set_scroll_bar_width,
  gui_set_scroll_bar_height,
  android_set_title,
  gui_set_unsplittable,
  gui_set_vertical_scroll_bars,
  gui_set_horizontal_scroll_bars,
  gui_set_visibility,
  android_set_tab_bar_lines,
  android_set_tool_bar_lines,
  NULL,
  NULL,
  gui_set_screen_gamma,
  gui_set_line_spacing,
  gui_set_left_fringe,
  gui_set_right_fringe,
  NULL,
  gui_set_fullscreen,
  gui_set_font_backend,
  android_set_alpha,
  NULL,
  android_set_tool_bar_position,
  NULL,
  NULL,
  android_set_parent_frame,
  NULL,
  android_set_no_focus_on_map,
  android_set_no_accept_focus,
  NULL,
  NULL,
  gui_set_no_special_glyphs,
  NULL,
  gui_set_borders_respect_alpha_background,
  NULL,
};



/* Battery information support.  */

DEFUN ("android-query-battery", Fandroid_query_battery,
       Sandroid_query_battery, 0, 0, 0,
       doc: /* Perform a query for battery information.
Value is nil upon failure, or a list of the form:

  (CAPACITY CHARGE-COUNTER CURRENT-AVERAGE CURRENT-NOW STATUS
   REMAINING PLUGGED TEMP)

where REMAINING, CURRENT-AVERAGE, and CURRENT-NOW are undefined prior
to Android 5.0.

See the documentation at

  https://developer.android.com/reference/android/os/BatteryManager

for more details about these values.  */)
  (void)
{
  struct android_battery_state state;

  /* Make sure the Android libraries have been initialized.  */

  if (!android_init_gui)
    return Qnil;

  /* Perform the query.  */

  if (android_query_battery (&state))
    return Qnil;

  return list (make_int (state.capacity),
	       make_fixnum (state.charge_counter),
	       make_int (state.current_average),
	       make_int (state.current_now),
	       make_fixnum (state.status),
	       make_int (state.remaining),
	       make_fixnum (state.plugged),
	       make_fixnum (state.temperature));
}



/* SAF directory access management.  */

DEFUN ("android-request-directory-access", Fandroid_request_directory_access,
       Sandroid_request_directory_access, 0, 0, "",
       doc: /* Request access to a directory within external storage.
On Android 5.0 and later, prompt for a directory within external or
application storage, and grant access to it; some of these directories
cannot be accessed through the regular `/sdcard' filesystem.

If access to the directory is granted, it will eventually appear
within the directory `/content/storage'.  */)
  (void)
{
  if (android_get_current_api_level () < 21)
    error ("Emacs can only access application storage on"
	   " Android 5.0 and later");

  if (!android_init_gui)
    return Qnil;

  android_request_directory_access ();
  return Qnil;
}



/* Functions concerning storage permissions.  */

DEFUN ("android-external-storage-available-p",
       Fandroid_external_storage_available_p,
       Sandroid_external_storage_available_p, 0, 0, 0,
       doc: /* Return non-nil if Emacs is entitled to access external storage.
Return nil if the requisite permissions for external storage access
have not been granted to Emacs, t otherwise.  Such permissions can be
requested by means of the `android-request-storage-access'
command.

External storage on Android encompasses the `/sdcard' and
`/storage/emulated' directories, access to which is denied to programs
absent these permissions.  */)
  (void)
{
  /* Implement a rather undependable fallback when no GUI is
     available.  */
  if (!android_init_gui)
    return Ffile_accessible_directory_p (build_string ("/sdcard"));

  return android_external_storage_available_p () ? Qt : Qnil;
}

DEFUN ("android-request-storage-access", Fandroid_request_storage_access,
       Sandroid_request_storage_access, 0, 0, "",
       doc: /* Request permissions to access external storage.

Return nil regardless of whether access permissions are granted or not,
immediately after displaying the permissions request dialog.

Use `android-external-storage-available-p' (which see) to verify
whether Emacs has actually received such access permissions.  */)
  (void)
{
  if (!android_init_gui)
    return Qnil;

  android_request_storage_access ();
  return Qnil;
}



/* Miscellaneous input method related stuff.  */

/* Report X, Y, by the phys cursor width and height as the cursor
   anchor rectangle for W's frame.  */

void
android_set_preeditarea (struct window *w, int x, int y)
{
  struct frame *f;

  f = WINDOW_XFRAME (w);

  /* Convert the window coordinates to the frame's coordinate
     space.  */
  x = (WINDOW_TO_FRAME_PIXEL_X (w, x)
       + WINDOW_LEFT_FRINGE_WIDTH (w)
       + WINDOW_LEFT_MARGIN_WIDTH (w));
  y = WINDOW_TO_FRAME_PIXEL_Y (w, y);

  /* Note that calculating the baseline is too hard, so the bottom of
     the cursor is used instead.  */
  android_update_cursor_anchor_info (FRAME_ANDROID_WINDOW (f), x,
				     y, y + w->phys_cursor_height,
				     y + w->phys_cursor_height);
}



/* Debugging.  */

DEFUN ("android-recreate-activity", Fandroid_recreate_activity,
       Sandroid_recreate_activity, 0, 0, "",
       doc: /* Recreate the activity attached to the current frame.
This function exists for debugging purposes and is of no interest to
users.  */)
  (void)
{
  struct frame *f;

  f = decode_window_system_frame (Qnil);
  android_recreate_activity (FRAME_ANDROID_WINDOW (f));
  return Qnil;
}

#endif /* !ANDROID_STUBIFY */



#ifndef ANDROID_STUBIFY

static void
syms_of_androidfns_for_pdumper (void)
{
  jclass locale;
  jmethodID method;
  jobject object;
  jstring string;
  Lisp_Object language, country, script, variant;
  const char *data;
  FILE *fd;
  char *line;
  size_t size;
  long pid;

  /* Find the Locale class.  */

  locale = (*android_java_env)->FindClass (android_java_env,
					   "java/util/Locale");
  if (!locale)
    emacs_abort ();

  /* And the method from which the default locale can be
     extracted.  */

  method = (*android_java_env)->GetStaticMethodID (android_java_env,
						   locale,
						   "getDefault",
						   "()Ljava/util/Locale;");
  if (!method)
    emacs_abort ();

  /* Retrieve the default locale.  */

  object = (*android_java_env)->CallStaticObjectMethod (android_java_env,
							locale, method);
  android_exception_check_1 (locale);

  if (!object)
    emacs_abort ();

  /* Retrieve its language field.  Each of these methods is liable to
     return the empty string, though if language is empty, the locale
     is malformed.  */

  method = (*android_java_env)->GetMethodID (android_java_env, locale,
					     "getLanguage",
					     "()Ljava/lang/String;");
  if (!method)
    emacs_abort ();

  string = (*android_java_env)->CallObjectMethod (android_java_env, object,
						  method);
  android_exception_check_2 (object, locale);

  if (!string)
    language = empty_unibyte_string;
  else
    {
      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      android_exception_check_3 (object, locale, string);

      if (!data)
	language = empty_unibyte_string;
      else
	{
	  language = build_unibyte_string (data);
	  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						      string, data);
	}
    }

  /* Delete the reference to this string.  */
  ANDROID_DELETE_LOCAL_REF (string);

  /* Proceed to retrieve the country code.  */

  method = (*android_java_env)->GetMethodID (android_java_env, locale,
					     "getCountry",
					     "()Ljava/lang/String;");
  if (!method)
    emacs_abort ();

  string = (*android_java_env)->CallObjectMethod (android_java_env, object,
						  method);
  android_exception_check_2 (object, locale);

  if (!string)
    country = empty_unibyte_string;
  else
    {
      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      android_exception_check_3 (object, locale, string);

      if (!data)
	country = empty_unibyte_string;
      else
	{
	  country = build_unibyte_string (data);
	  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						      string, data);
	}
    }

  ANDROID_DELETE_LOCAL_REF (string);

  /* Proceed to retrieve the script.  */

  if (android_get_current_api_level () < 21)
    script = empty_unibyte_string;
  else
    {
      method = (*android_java_env)->GetMethodID (android_java_env, locale,
						 "getScript",
						 "()Ljava/lang/String;");
      if (!method)
	emacs_abort ();

      string = (*android_java_env)->CallObjectMethod (android_java_env,
						      object, method);
      android_exception_check_2 (object, locale);

      if (!string)
	script = empty_unibyte_string;
      else
	{
	  data = (*android_java_env)->GetStringUTFChars (android_java_env,
							 string, NULL);
	  android_exception_check_3 (object, locale, string);

	  if (!data)
	    script = empty_unibyte_string;
	  else
	    {
	      script = build_unibyte_string (data);
	      (*android_java_env)->ReleaseStringUTFChars (android_java_env,
							  string, data);
	    }
	}

      ANDROID_DELETE_LOCAL_REF (string);
    }

  /* And variant.  */

  method = (*android_java_env)->GetMethodID (android_java_env, locale,
					     "getVariant",
					     "()Ljava/lang/String;");
  if (!method)
    emacs_abort ();

  string = (*android_java_env)->CallObjectMethod (android_java_env, object,
						  method);
  android_exception_check_2 (object, locale);

  if (!string)
    variant = empty_unibyte_string;
  else
    {
      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      android_exception_check_3 (object, locale, string);

      if (!data)
        variant = empty_unibyte_string;
      else
	{
	  variant = build_unibyte_string (data);
	  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						      string, data);
	}
    }

  /* Delete the reference to this string.  */
  ANDROID_DELETE_LOCAL_REF (string);

  /* And other remaining local references.  */
  ANDROID_DELETE_LOCAL_REF (object);
  ANDROID_DELETE_LOCAL_REF (locale);

  /* Set Vandroid_os_language.  */
  Vandroid_os_language = list4 (language, country, script, variant);

  /* Detect whether Emacs is running under libloader.so or another
     process tracing mechanism, and disable `android_use_exec_loader' if
     so, leaving subprocesses started by Emacs to the care of that
     loader instance.  */

  if (android_get_current_api_level () >= 29) /* Q */
    {
      fd = fopen ("/proc/self/status", "r");
      if (!fd)
	return;

      line = NULL;
      while (getline (&line, &size, fd) != -1)
	{
	  if (strncmp (line, "TracerPid:", sizeof "TracerPid:" - 1))
	    continue;

	  pid = atol (line + sizeof "TracerPid:" - 1);

	  if (pid)
	    android_use_exec_loader = false;

	  break;
	}

      free (line);
      fclose (fd);
    }
}

#endif /* ANDROID_STUBIFY */

void
syms_of_androidfns (void)
{
  /* Miscellaneous symbols used by some functions here.  */
  DEFSYM (Qtrue_color, "true-color");
  DEFSYM (Qstatic_gray, "static-color");
  DEFSYM (Qwhen_mapped, "when-mapped");

  DEFVAR_LISP ("x-pointer-shape", Vx_pointer_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_pointer_shape = Qnil;

#if false /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-nontext-pointer-shape", Vx_nontext_pointer_shape,
    doc: /* SKIP: real doc in xfns.c.  */);
#endif
  Vx_nontext_pointer_shape = Qnil;

  DEFVAR_LISP ("x-hourglass-pointer-shape", Vx_hourglass_pointer_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_hourglass_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	      Vx_sensitive_text_pointer_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-horizontal-drag-cursor",
	      Vx_window_horizontal_drag_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("x-window-vertical-drag-cursor",
	      Vx_window_vertical_drag_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_vertical_drag_shape = Qnil;

  DEFVAR_LISP ("x-window-left-edge-cursor",
	       Vx_window_left_edge_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_left_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-top-left-corner-cursor",
	       Vx_window_top_left_corner_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_top_left_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-top-edge-cursor",
	       Vx_window_top_edge_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_top_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-top-right-corner-cursor",
	       Vx_window_top_right_corner_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_top_right_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-right-edge-cursor",
	       Vx_window_right_edge_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_right_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-right-corner-cursor",
	       Vx_window_bottom_right_corner_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_bottom_right_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-edge-cursor",
	       Vx_window_bottom_edge_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_bottom_edge_shape = Qnil;

#if false /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-mode-pointer-shape", Vx_mode_pointer_shape,
    doc: /* SKIP: real doc in xfns.c.  */);
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-left-corner-cursor",
	       Vx_window_bottom_left_corner_shape,
    doc: /* SKIP: real text in xfns.c.  */);
  Vx_window_bottom_left_corner_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
    doc: /* SKIP: real doc in xfns.c.  */);
  Vx_cursor_fore_pixel = Qnil;

  /* Used by Fx_show_tip.  */
  DEFSYM (Qrun_at_time, "run-at-time");
  DEFSYM (Qx_hide_tip, "x-hide-tip");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qassq_delete_all, "assq-delete-all");
  DEFSYM (Qcolor, "color");

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
    doc: /* SKIP: real doc in xfns.c.  */);
  Vx_max_tooltip_size = Qnil;

  DEFVAR_BOOL ("android-pass-multimedia-buttons-to-system",
	       android_pass_multimedia_buttons_to_system,
    doc: /* Whether or not to pass volume control buttons to the system.
Generally, the `volume-up', `volume-down' and `volume-mute' keys are
processed by Emacs, but setting this to non-nil they are passed to the
operating system instead of being intercepted by Emacs.

Note that if you set this, you will no longer be able to quit Emacs
using the volume down button.  */);
  android_pass_multimedia_buttons_to_system = false;

  DEFVAR_BOOL ("android-intercept-control-space",
	       android_intercept_control_space,
    doc: /* Whether Emacs should intercept C-SPC.
When this variable is set, Emacs intercepts C-SPC events as they are
delivered to a frame before they are registered and filtered by the
input method.

For no apparent purpose, Android input methods customarily discard SPC
events with the Ctrl modifier set without delivering them to Emacs
afterwards, which is an impediment to typing key sequences
incorporating such keys.  */);
  android_intercept_control_space = true;

  DEFVAR_BOOL ("android-use-exec-loader", android_use_exec_loader,
    doc: /* Whether or not to bypass system restrictions on program execution.

Android 10 and later prevent programs from executing files installed
in writable directories, such as the application data directory.

When non-nil, Emacs will bypass this restriction by running such
executables under system call tracing, and replacing the `execve'
system call with a version which ignores the system's security
restrictions.

This option has no effect on Android 9 and earlier.  */);
  android_use_exec_loader = true;

  DEFVAR_INT ("android-keyboard-bell-duration",
	      android_keyboard_bell_duration,
    doc: /* Number of milliseconds to vibrate after ringing the keyboard bell.
The keyboard bell under Android systems takes the form of a vibrating
element that is activated for a given number of milliseconds upon the
bell being rung.  */);
  android_keyboard_bell_duration = 50;

  DEFVAR_LISP ("android-os-language", Vandroid_os_language,
    doc: /* A list representing the configured system language on Android.
This list has four elements: LANGUAGE, COUNTRY, SCRIPT and VARIANT, where:

LANGUAGE and COUNTRY are ISO language and country codes identical to
those found in POSIX locale specifications.

SCRIPT is an ISO 15924 script tag, representing the script used
if available, or if required to disambiguate between distinct
writing systems for the same combination of language and country.

VARIANT is an arbitrary string representing the variant of the
LANGUAGE or SCRIPT.

Each of these fields might be empty or nil, but the locale is invalid
if LANGUAGE is empty.  Users of this variable should consider the
language to be US English if LANGUAGE is empty.  */);
  Vandroid_os_language = Qnil;

  /* Functions defined.  */
  defsubr (&Sx_create_frame);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sandroid_display_monitor_attributes_list);
  defsubr (&Sandroid_frame_geometry);
  defsubr (&Sandroid_frame_edges);
  defsubr (&Sandroid_frame_list_z_order);
  defsubr (&Sandroid_frame_restack);
  defsubr (&Sandroid_mouse_absolute_pixel_position);
  defsubr (&Sandroid_set_mouse_absolute_pixel_position);
  defsubr (&Sandroid_get_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  defsubr (&Sandroid_detect_mouse);
  defsubr (&Sandroid_detect_keyboard);
  defsubr (&Sandroid_toggle_on_screen_keyboard);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
#ifndef ANDROID_STUBIFY
  defsubr (&Sandroid_query_battery);
  defsubr (&Sandroid_request_directory_access);
  defsubr (&Sandroid_external_storage_available_p);
  defsubr (&Sandroid_request_storage_access);
  defsubr (&Sandroid_recreate_activity);

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
  tip_dx = Qnil;
  staticpro (&tip_dx);
  tip_dy = Qnil;
  staticpro (&tip_dy);

  pdumper_do_now_and_after_load (syms_of_androidfns_for_pdumper);
#endif /* !ANDROID_STUBIFY */
}
