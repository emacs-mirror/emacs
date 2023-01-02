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
#include <math.h>

#include "lisp.h"
#include "androidterm.h"
#include "blockinput.h"
#include "keyboard.h"

#ifndef ANDROID_STUBIFY

/* Some kind of reference count for the image cache.  */
static ptrdiff_t image_cache_refcount;

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

void
android_implicitly_set_name (struct frame *f, Lisp_Object arg,
			     Lisp_Object oldval)
{

}

void
android_explicitly_set_name (struct frame *f, Lisp_Object arg,
			     Lisp_Object oldval)
{

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
      /* If the frame's image cache refcount is still the same as our
	 private shadow variable, it means we are unwinding a frame
	 for which we didn't yet call init_frame_faces, where the
	 refcount is incremented.  Therefore, we increment it here, so
	 that free_frame_faces, called in x_free_frame_resources
	 below, will not mistakenly decrement the counter that was not
	 incremented yet to account for this new frame.  */
      if (FRAME_IMAGE_CACHE (f) != NULL
	  && FRAME_IMAGE_CACHE (f)->refcount == image_cache_refcount)
	FRAME_IMAGE_CACHE (f)->refcount++;

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
	/* This will find the normal font.  The default font size on
	   Android is 8.  */
	"monospace",
	"DroidSansMono",
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

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_pixel
      = android_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.android->cursor_foreground_pixel
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

  image_cache_refcount = (FRAME_IMAGE_CACHE (f)
			  ? FRAME_IMAGE_CACHE (f)->refcount
			  : 0);

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

  /* gui_default_parameter (f, parms, Qvertical_scroll_bars, */
  /*                        Qleft, */
  /*                        "verticalScrollBars", "ScrollBars", */
  /*                        RES_TYPE_SYMBOL); */
  /* gui_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil, */
  /*                        "horizontalScrollBars", "ScrollBars", */
  /*                        RES_TYPE_SYMBOL);  TODO */

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
#endif /* TODO */

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
     adjust_frame_size call.  */
  gui_default_parameter (f, parms, Qfullscreen, Qnil,
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
  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p,
       Sx_display_grayscale_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return Qnil;
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
  error ("Not implemented");
  return Qnil;
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
  error ("Not implemented");
  return Qnil;
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
  error ("Not implemented");
  return Qnil;
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
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_android_display_info (terminal);

  /* The Java part is implemented in a way that it always does the
     equivalent of backing store.  */
  return Qalways;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_android_display_info (terminal);

  return Qtrue_color;
}

DEFUN ("x-display-monitor-attributes-list", Fx_display_monitor_attributes_list,
       Sx_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-frame-geometry", Fx_frame_geometry, Sx_frame_geometry,
       0, 1, 0, doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-frame-list-z-order", Fx_frame_list_z_order,
       Sx_frame_list_z_order, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-frame-restack", Fx_frame_restack, Sx_frame_restack, 2, 3, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object frame3)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-mouse-absolute-pixel-position", Fx_mouse_absolute_pixel_position,
       Sx_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  /* TODO: figure out how to implement this.  */
  return Qnil;
}

DEFUN ("x-set-mouse-absolute-pixel-position",
       Fx_set_mouse_absolute_pixel_position,
       Sx_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object x, Lisp_Object y)
{
  /* TODO: figure out how to implement this.  */
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
    XSETTERMINAL (terminal, x_display_list->terminal);

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

DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
}

DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
#ifdef ANDROID_STUBIFY
  error ("Android cross-compilation stub called!");
  return Qnil;
#else
  error ("Not implemented");
  return Qnil;
#endif
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
	  android_clear_area (FRAME_ANDROID_WINDOW (f),
			      0, y, width, height);
	  unblock_input ();
	}

      if (nlines > 1 && nlines > olines)
	{
	  y = (olines == 0 ? 1 : olines) * FRAME_LINE_HEIGHT (f);
	  height = nlines * FRAME_LINE_HEIGHT (f) - y;

	  block_input ();
	  android_clear_area (FRAME_ANDROID_WINDOW (f), 0, y,
			      width, height);
	  unblock_input ();
	}

      if (nlines == 0 && WINDOWP (f->menu_bar_window))
	clear_glyph_matrix (XWINDOW (f->menu_bar_window)->current_matrix);
    }

  adjust_frame_glyphs (f);
}

static void
android_set_mouse_color (struct frame *f, Lisp_Object arg,
			 Lisp_Object oldval)
{
  /* Changing the mouse color is unsupported under Android, so this is
     left intact.  */
  android_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
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
  NULL,
  NULL,
  NULL, /* x_set_undecorated, */
  NULL, /* x_set_parent_frame, */
  NULL, /* x_set_skip_taskbar, */
  NULL, /* x_set_no_focus_on_map, */
  NULL, /* x_set_no_accept_focus, */
  NULL, /* x_set_z_group, */
  NULL, /* x_set_override_redirect, */
  gui_set_no_special_glyphs,
  NULL, /* x_set_alpha_background, */
  NULL, /* x_set_use_frame_synchronization, */
};

#endif



void
syms_of_androidfns (void)
{
  /* Miscellaneous symbols used by some functions here.  */
  DEFSYM (Qtrue_color, "true-color");
  DEFSYM (Qalways, "always");

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
    doc: /* SKIP: real doc in xfns.c.  */);
  Vx_cursor_fore_pixel = Qnil;

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
  defsubr (&Sx_display_monitor_attributes_list);
  defsubr (&Sx_frame_geometry);
  defsubr (&Sx_frame_list_z_order);
  defsubr (&Sx_frame_restack);
  defsubr (&Sx_mouse_absolute_pixel_position);
  defsubr (&Sx_set_mouse_absolute_pixel_position);
  defsubr (&Sandroid_get_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
}
