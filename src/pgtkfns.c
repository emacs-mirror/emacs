/* Functions for the pure Gtk+-3.

Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2020, 2022-2026 Free
Software Foundation, Inc.

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
#include <c-strcase.h>

#include "lisp.h"
#include "blockinput.h"
#include "gtkutil.h"
#include "window.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "fontset.h"
#include "font.h"
#include "xsettings.h"
#include "atimer.h"

static int x_decode_color (struct frame *f, Lisp_Object color_name,
			   int mono_color);
static struct pgtk_display_info *pgtk_display_info_for_name (Lisp_Object);

static const char *pgtk_app_name = "Emacs";

/* Scale factor manually set per monitor.  */
static Lisp_Object monitor_scale_factor_alist;

/* ==========================================================================

    Internal utility functions

   ========================================================================== */

static double
pgtk_get_monitor_scale_factor (const char *model)
{
  if (model == NULL)
    return 0.0;

  Lisp_Object mdl = build_string (model);
  Lisp_Object tem = Fassoc (mdl, monitor_scale_factor_alist, Qnil);
  if (NILP (tem))
    return 0;
  Lisp_Object cdr = Fcdr (tem);
  if (NILP (cdr))
    return 0;
  if (FIXNUMP (cdr))
    return XFIXNUM (cdr);
  else if (FLOATP (cdr))
    return XFLOAT_DATA (cdr);
  else
    error ("Unknown type of scale-factor");
}

struct pgtk_display_info *
check_pgtk_display_info (Lisp_Object object)
{
  struct pgtk_display_info *dpyinfo = NULL;

  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_PGTK_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
	error ("Frames are not in use or not initialized");
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type != output_pgtk)
	error ("Terminal %d is not a display", t->id);

      dpyinfo = t->display_info.pgtk;
    }
  else if (STRINGP (object))
    dpyinfo = pgtk_display_info_for_name (object);
  else
    {
      struct frame *f = decode_window_system_frame (object);
      dpyinfo = FRAME_DISPLAY_INFO (f);
    }

  return dpyinfo;
}

/* On Wayland, even if without WAYLAND_DISPLAY, --display DISPLAY
   works, but gdk_display_get_name always return "wayland-0", which
   may be different from DISPLAY.  If with WAYLAND_DISPLAY, then it
   always returns WAYLAND_DISPLAY.  So pgtk Emacs is confused and
   enters multi display environment.  To workaround this situation,
   treat all the wayland-* as the same display.  */
static Lisp_Object
is_wayland_display (Lisp_Object dpyname)
{
  const char *p = SSDATA (dpyname);
  if (strncmp (p, "wayland-", 8) != 0)
    return Qnil;
  p += 8;
  do {
    if (*p < '0' || *p > '9')
      return Qnil;
  } while (*++p != '\0');
  return Qt;
}

/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */
static struct pgtk_display_info *
pgtk_display_info_for_name (Lisp_Object name)
{
  struct pgtk_display_info *dpyinfo;

  CHECK_STRING (name);

  if (!NILP (is_wayland_display (name)))
    {
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
	if (!NILP (is_wayland_display (XCAR (dpyinfo->name_list_element))))
	  return dpyinfo;
    }
  else
    {
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
	if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element), name)))
	  return dpyinfo;
    }

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = pgtk_term_init (name, SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to display server %s", SDATA (name));

  return dpyinfo;
}

/* ==========================================================================

    Frame parameter setters

   ========================================================================== */


static void
pgtk_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long fg, old_fg;

  block_input ();
  old_fg = FRAME_FOREGROUND_COLOR (f);
  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  FRAME_FOREGROUND_PIXEL (f) = fg;
  FRAME_X_OUTPUT (f)->foreground_color = fg;

  if (FRAME_GTK_WIDGET (f))
    {
      if (FRAME_X_OUTPUT (f)->cursor_color == old_fg)
	{
	  FRAME_X_OUTPUT (f)->cursor_color = fg;
	  FRAME_X_OUTPUT (f)->cursor_xgcv.background = fg;
	}

      update_face_from_frame_parameter (f, Qforeground_color, arg);
      if (FRAME_VISIBLE_P (f))
	SET_FRAME_GARBAGED (f);
    }
  unblock_input ();
}


static void
pgtk_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long bg;

  block_input ();
  bg = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  FRAME_BACKGROUND_PIXEL (f) = bg;

  /* Clear the frame.  */
  if (FRAME_VISIBLE_P (f))
    pgtk_clear_frame (f);

  FRAME_X_OUTPUT (f)->background_color = bg;
  FRAME_X_OUTPUT (f)->cursor_xgcv.foreground = bg;

  xg_set_background_color (f, bg);
  update_face_from_frame_parameter (f, Qbackground_color, arg);

  if (FRAME_VISIBLE_P (f))
    SET_FRAME_GARBAGED (f);
  unblock_input ();
}

static void
pgtk_set_alpha_background (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  gui_set_alpha_background (f, arg, oldval);

  /* This prevents GTK from painting the window's background, which
     interferes with transparent background in some environments */

  gtk_widget_set_app_paintable (FRAME_GTK_OUTER_WIDGET (f),
				f->alpha_background != 1.0);

  if (FRAME_GTK_OUTER_WIDGET (f)
      && gtk_widget_get_realized (FRAME_GTK_OUTER_WIDGET (f))
      && f->alpha_background != 1.0)
    gdk_window_set_opaque_region (gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f)),
				  NULL);
}

static void
pgtk_set_border_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  FRAME_X_OUTPUT (f)->border_pixel = pix;
  pgtk_frame_rehighlight (FRAME_DISPLAY_INFO (f));
}

static void
pgtk_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long fore_pixel, pixel;
  struct pgtk_output *x = f->output_data.pgtk;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				   WHITE_PIX_DEFAULT (f));
    }
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      pixel = x->mouse_color;
      if (pixel == fore_pixel)
	{
	  fore_pixel = FRAME_BACKGROUND_PIXEL (f);
	}
    }

  x->cursor_foreground_color = fore_pixel;
  x->cursor_color = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      x->cursor_xgcv.background = x->cursor_color;
      x->cursor_xgcv.foreground = fore_pixel;

      if (FRAME_VISIBLE_P (f))
	{
	  gui_update_cursor (f, false);
	  gui_update_cursor (f, true);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

static void
pgtk_set_name_internal (struct frame *f, Lisp_Object name)
{
  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      block_input ();
      {
	Lisp_Object encoded_name;

	/* As ENCODE_UTF_8 may cause GC and relocation of string data,
	   we use it before x_encode_text that may return string data.  */
	encoded_name = ENCODE_UTF_8 (name);

	gtk_window_set_title (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			      SSDATA (encoded_name));
      }
      unblock_input ();
    }
}

static void
pgtk_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
         update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 12;

      f->explicit_name = !NILP (name);
    }
  else if (f->explicit_name)
    return;

  if (NILP (name))
    name = build_string (pgtk_app_name);
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (!NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* Title overrides explicit name.  */
  if (!NILP (f->title))
    name = f->title;

  pgtk_set_name_internal (f, name);
}


/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
pgtk_explicitly_set_name (struct frame *f, Lisp_Object arg,
			  Lisp_Object oldval)
{
  pgtk_set_name (f, arg, true);
}


/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
pgtk_implicitly_set_name (struct frame *f, Lisp_Object arg,
			  Lisp_Object oldval)
{
  pgtk_set_name (f, arg, false);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
pgtk_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 22;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  pgtk_set_name_internal (f, name);
}

static void
pgtk_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
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

  FRAME_MENU_BAR_LINES (f) = 0;
  FRAME_MENU_BAR_HEIGHT (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      if (FRAME_PGTK_P (f) && f->output_data.pgtk->menubar_widget == 0)
	/* Make sure next redisplay shows the menu bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = true;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
      if (FRAME_X_P (f))
	f->output_data.pgtk->menubar_widget = 0;
    }

  adjust_frame_glyphs (f);
}

/* Set the number of lines used for the tab bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tab bar lines.  This function changes the
   height of all windows on frame F to match the new tab bar height.
   The frame's height doesn't change.  */

static void
pgtk_set_tab_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  /* Treat tab bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  pgtk_change_tab_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}

/* Set the pixel height of the tab bar of frame F to HEIGHT.  */
void
pgtk_change_tab_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TAB_BAR_HEIGHT (f);

  /* This differs from the tool bar code in that the tab bar height is
     not rounded up.  Otherwise, if redisplay_tab_bar decides to grow
     the tab bar by even 1 pixel, FRAME_TAB_BAR_LINES will be changed,
     leading to the tab bar height being incorrectly set upon the next
     call to x_set_font.  (bug#59285) */
  int lines = height / unit;

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

  if (FRAME_X_WINDOW (f) && FRAME_TAB_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tab_bar_window))
    clear_glyph_matrix (XWINDOW (f->tab_bar_window)->current_matrix);

  if (!f->tab_bar_resized)
    {
      Lisp_Object fullscreen = get_frame_param (f, Qfullscreen);

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
  if (FRAME_X_WINDOW (f))
    pgtk_clear_under_internal_border (f);
}

/* Set the pixel height of the tool bar of frame F to HEIGHT.  */
static void
x_change_tool_bar_height (struct frame *f, int height)
{
  FRAME_TOOL_BAR_LINES (f) = 0;
  FRAME_TOOL_BAR_HEIGHT (f) = 0;
  if (height)
    {
      FRAME_EXTERNAL_TOOL_BAR (f) = true;
      if (FRAME_X_P (f) && f->output_data.pgtk->toolbar_widget == 0)
	/* Make sure next redisplay shows the tool bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = true;
      update_frame_tool_bar (f);
    }
  else
    {
      if (FRAME_EXTERNAL_TOOL_BAR (f))
	free_frame_tool_bar (f);
      FRAME_EXTERNAL_TOOL_BAR (f) = false;
      /* Make sure implied resizing of frames without initial tool bar
	 can be inhibited too.  */
      f->tool_bar_resized = true;
    }
}

/* Toolbar support.  */
static void
pgtk_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    {
      /* Make sure implied resizing can be inhibited for minibuffer-only
	 frames too.  */
      f->tool_bar_resized = true;

      return;
    }

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  x_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));

}

static void
pgtk_set_child_frame_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
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

      if (FRAME_GTK_WIDGET (f))
	{
	  adjust_frame_size (f, -1, -1, 3,
			     false, Qchild_frame_border_width);
	  pgtk_clear_under_internal_border (f);
	}
    }
}

static void
pgtk_set_internal_border_width (struct frame *f, Lisp_Object arg,
				Lisp_Object oldval)
{
  int border = check_int_nonnegative (arg);

  if (border != FRAME_INTERNAL_BORDER_WIDTH (f))
    {
      f->internal_border_width = border;

      if (FRAME_X_WINDOW (f))
	{
	  adjust_frame_size (f, -1, -1, 3, false, Qinternal_border_width);
	  pgtk_clear_under_internal_border (f);
	}
    }
}

static void
pgtk_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  bool result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && BASE_EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && NILP (oldval) == NILP (arg))
    return;

  block_input ();
  if (NILP (arg))
    result = pgtk_text_icon (f,
			     SSDATA ((!NILP (f->icon_name)
				      ? f->icon_name : f->name)));
  else
    result = FRAME_TERMINAL (f)->set_bitmap_icon_hook (f, arg);

  if (result)
    {
      unblock_input ();
      error ("No icon window available");
    }

  unblock_input ();
}

static void
pgtk_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  bool result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && BASE_EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!NILP (arg) || NILP (oldval))
    return;

  fset_icon_name (f, arg);

  block_input ();

  result = pgtk_text_icon (f,
			   SSDATA ((!NILP (f->icon_name)
				    ? f->icon_name
				    : !NILP (f->title)
				    ? f->title : f->name)));

  if (result)
    {
      unblock_input ();
      error ("No icon window available");
    }

  unblock_input ();
}

static void
pgtk_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

static void
pgtk_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
}

static void
pgtk_set_undecorated (struct frame *f, Lisp_Object new_value,
		   Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      FRAME_UNDECORATED (f) = NILP (new_value) ? false : true;
      xg_set_undecorated (f, new_value);
    }
}

static void
pgtk_set_skip_taskbar (struct frame *f, Lisp_Object new_value,
		    Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      xg_set_skip_taskbar (f, new_value);
      FRAME_SKIP_TASKBAR (f) = !NILP (new_value);
    }
}

static void
pgtk_set_override_redirect (struct frame *f, Lisp_Object new_value,
			    Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      /* Here (xfwm) override_redirect can be changed for invisible
         frames only.  */
      pgtk_make_frame_invisible (f);

      xg_set_override_redirect (f, new_value);

      pgtk_make_frame_visible (f);
      FRAME_OVERRIDE_REDIRECT (f) = !NILP (new_value);
    }
}

/* Set icon from FILE for frame F.  */
bool
xg_set_icon (struct frame *f, Lisp_Object file)
{
  bool result = false;
  Lisp_Object found;

  if (!FRAME_GTK_OUTER_WIDGET (f))
    return false;

  found = image_find_image_file (file);

  if (!NILP (found))
    {
      GdkPixbuf *pixbuf;
      GError *err = NULL;
      char *filename = SSDATA (ENCODE_FILE (found));
      block_input ();

      pixbuf = gdk_pixbuf_new_from_file (filename, &err);

      if (pixbuf)
	{
	  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			       pixbuf);
	  g_object_unref (pixbuf);

	  result = true;
	}
      else
	g_error_free (err);

      unblock_input ();
    }

  return result;
}

bool
xg_set_icon_from_xpm_data (struct frame *f, const char **data)
{
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data (data);

  if (!pixbuf)
    return false;

  if (!FRAME_GTK_OUTER_WIDGET (f))
    return false;

  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), pixbuf);
  g_object_unref (pixbuf);
  return true;
}

static void
pgtk_set_sticky (struct frame *f, Lisp_Object new_value,
		 Lisp_Object old_value)
{
  if (!FRAME_GTK_OUTER_WIDGET (f))
    return;

  if (!NILP (new_value))
    gtk_window_stick (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
  else
    gtk_window_unstick (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
}

static void
pgtk_set_tool_bar_position (struct frame *f,
			    Lisp_Object new_value, Lisp_Object old_value)
{
  Lisp_Object choice = list4 (Qleft, Qright, Qtop, Qbottom);

  if (!NILP (Fmemq (new_value, choice)))
    {
      if (!EQ (new_value, old_value))
	{
	  xg_change_toolbar_position (f, new_value);
	  fset_tool_bar_position (f, new_value);
	}
    }
  else
    wrong_choice (choice, new_value);
}

static void
pgtk_set_scroll_bar_foreground (struct frame *f, Lisp_Object new_value,
				Lisp_Object old_value)
{
  GtkCssProvider *css_provider =
    FRAME_X_OUTPUT (f)->scrollbar_foreground_css_provider;

  if (FRAME_TOOLTIP_P (f))
    return;

  if (NILP (new_value))
    {
      gtk_css_provider_load_from_data (css_provider, "", -1, NULL);
      update_face_from_frame_parameter (f, Qscroll_bar_foreground, new_value);
    }
  else if (STRINGP (new_value))
    {
      Emacs_Color rgb;

      if (!pgtk_parse_color (f, SSDATA (new_value), &rgb))
	error ("Unknown color");

      char css[64];
      sprintf (css, "scrollbar slider { background-color: #%06x; }",
	       (unsigned int) rgb.pixel & 0xffffff);
      gtk_css_provider_load_from_data (css_provider, css, -1, NULL);
      update_face_from_frame_parameter (f, Qscroll_bar_foreground, new_value);

    }
  else
    error ("Invalid scroll-bar-foreground");
}

static void
pgtk_set_scroll_bar_background (struct frame *f, Lisp_Object new_value,
				Lisp_Object old_value)
{
  GtkCssProvider *css_provider =
    FRAME_X_OUTPUT (f)->scrollbar_background_css_provider;

  if (NILP (new_value))
    {
      gtk_css_provider_load_from_data (css_provider, "", -1, NULL);
      update_face_from_frame_parameter (f, Qscroll_bar_background, new_value);
    }
  else if (STRINGP (new_value))
    {
      Emacs_Color rgb;

      if (!pgtk_parse_color (f, SSDATA (new_value), &rgb))
	error ("Unknown color");

      /* On pgtk, this frame parameter should be ignored, and honor
	 gtk theme.  (It honors the GTK theme if not explicitly set, so
	 I see no harm in letting users tinker a bit more.)  */
      char css[64];
      sprintf (css, "scrollbar trough { background-color: #%06x; }",
	       (unsigned int) rgb.pixel & 0xffffff);
      gtk_css_provider_load_from_data (css_provider, css, -1, NULL);
      update_face_from_frame_parameter (f, Qscroll_bar_background, new_value);

    }
  else
    error ("Invalid scroll-bar-background");
}


/***********************************************************************
			       Printing
 ***********************************************************************/


DEFUN ("x-export-frames", Fx_export_frames, Sx_export_frames, 0, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object frames, Lisp_Object type)
{
  Lisp_Object rest, tmp;
  cairo_surface_type_t surface_type;

  if (!CONSP (frames))
    frames = list1 (frames);

  tmp = Qnil;
  for (rest = frames; CONSP (rest); rest = XCDR (rest))
    {
      struct frame *f = decode_window_system_frame (XCAR (rest));
      Lisp_Object frame;

      XSETFRAME (frame, f);
      if (!FRAME_VISIBLE_P (f))
	error ("Frames to be exported must be visible");
      tmp = Fcons (frame, tmp);
    }
  frames = Fnreverse (tmp);

#ifdef CAIRO_HAS_PDF_SURFACE
  if (NILP (type) || EQ (type, Qpdf))
    surface_type = CAIRO_SURFACE_TYPE_PDF;
  else
#endif
#ifdef CAIRO_HAS_PNG_FUNCTIONS
  if (EQ (type, Qpng))
    {
      if (!NILP (XCDR (frames)))
	error ("PNG export cannot handle multiple frames");
      surface_type = CAIRO_SURFACE_TYPE_IMAGE;
    }
  else
#endif
#ifdef CAIRO_HAS_PS_SURFACE
  if (EQ (type, Qpostscript))
    surface_type = CAIRO_SURFACE_TYPE_PS;
  else
#endif
#ifdef CAIRO_HAS_SVG_SURFACE
  if (EQ (type, Qsvg))
    {
      /* For now, we stick to SVG 1.1.  */
      if (!NILP (XCDR (frames)))
	error ("SVG export cannot handle multiple frames");
      surface_type = CAIRO_SURFACE_TYPE_SVG;
    }
  else
#endif
    error ("Unsupported export type");

  return pgtk_cr_export_frames (frames, surface_type);
}

extern frame_parm_handler pgtk_frame_parm_handlers[];
frame_parm_handler pgtk_frame_parm_handlers[] =
  {
    gui_set_autoraise,		/* generic OK */
    gui_set_autolower,		/* generic OK */
    pgtk_set_background_color,
    pgtk_set_border_color,
    gui_set_border_width,
    pgtk_set_cursor_color,
    pgtk_set_cursor_type,
    gui_set_font,		/* generic OK */
    pgtk_set_foreground_color,
    pgtk_set_icon_name,
    pgtk_set_icon_type,
    pgtk_set_child_frame_border_width,
    pgtk_set_internal_border_width,	/* generic OK */
    gui_set_right_divider_width,
    gui_set_bottom_divider_width,
    pgtk_set_menu_bar_lines,
    pgtk_set_mouse_color,
    pgtk_explicitly_set_name,
    gui_set_scroll_bar_width,	/* generic OK */
    gui_set_scroll_bar_height,	/* generic OK */
    pgtk_set_title,
    gui_set_unsplittable,	/* generic OK */
    gui_set_vertical_scroll_bars,	/* generic OK */
    gui_set_horizontal_scroll_bars,	/* generic OK */
    gui_set_visibility,		/* generic OK */
    pgtk_set_tab_bar_lines,
    pgtk_set_tool_bar_lines,
    pgtk_set_scroll_bar_foreground,
    pgtk_set_scroll_bar_background,
    gui_set_screen_gamma,	/* generic OK */
    gui_set_line_spacing,	/* generic OK, sets f->extra_line_spacing to int */
    gui_set_left_fringe,	/* generic OK */
    gui_set_right_fringe,	/* generic OK */
    0,
    gui_set_fullscreen,		/* generic OK */
    gui_set_font_backend,	/* generic OK */
    gui_set_alpha,
    pgtk_set_sticky,
    pgtk_set_tool_bar_position,
    0,
    pgtk_set_undecorated,
    pgtk_set_parent_frame,
    pgtk_set_skip_taskbar,
    pgtk_set_no_focus_on_map,
    pgtk_set_no_accept_focus,
    pgtk_set_z_group,
    pgtk_set_override_redirect,
    gui_set_no_special_glyphs,
    pgtk_set_alpha_background,
    gui_set_borders_respect_alpha_background,
    NULL,
  };


/* Handler for signals raised during x_create_frame and
   x_create_tip_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is already dead, nothing to do.  This can happen if the
     display is disconnected after the frame has become official, but
     before x_create_frame removes the unwind protect.  */
  if (!FRAME_LIVE_P (f))
    return Qnil;

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
      pgtk_free_frame_resources (f);
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

/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG says.
   Signal an error if color can't be allocated.  */

static int
x_decode_color (struct frame *f, Lisp_Object color_name, int mono_color)
{
  Emacs_Color cdef;

  CHECK_STRING (color_name);

  /* Return MONO_COLOR for monochrome frames.  */
  if (FRAME_DISPLAY_INFO (f)->n_planes == 1)
    return mono_color;

  /* x_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (pgtk_defined_color (f, SSDATA (color_name), &cdef, true, 0))
    return cdef.pixel;

  signal_error ("Undefined color", color_name);
}

void
pgtk_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param =
    gui_display_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
			 RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (BASE_EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font_param))
    {
      /* System font should take precedence over X resources.  We suggest this
         regardless of font-use-system-font because .emacs may not have been
         read yet.  */
      const char *system_font = xsettings_get_system_font ();
      if (system_font)
	font = font_open_by_name (f, build_unibyte_string (system_font));
    }

  if (NILP (font))
    font = !NILP (font_param) ? font_param
      : gui_display_get_arg (dpyinfo, parms, Qfont, "font", "Font",
			     RES_TYPE_STRING);

  if (!FONTP (font) && !STRINGP (font))
    {
      const char *names[] = {
	"monospace-10",
	"-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
	"-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	"-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	/* This was formerly the first thing tried, but it finds
	   too many fonts and takes too long.  */
	"-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1",
	/* If those didn't work, look for something which will
	   at least work.  */
	"-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1",
	"fixed",
	NULL
      };
      int i;

      for (i = 0; names[i]; i++)
	{
	  font = font_open_by_name (f, build_unibyte_string (names[i]));
	  if (!NILP (font))
	    break;
	}
      if (NILP (font))
	error ("No suitable font was found");
    }

  /* This call will make X resources override any system font setting.  */
  gui_default_parameter (f, parms, Qfont, font, "font", "Font",
			 RES_TYPE_STRING);
}

static void
update_watched_scale_factor (struct atimer *timer)
{
  struct frame *f = timer->client_data;
  double scale_factor = FRAME_SCALE_FACTOR (f);

  if (scale_factor != FRAME_X_OUTPUT (f)->watched_scale_factor)
    {
      FRAME_X_OUTPUT (f)->watched_scale_factor = scale_factor;
      pgtk_cr_update_surface_desired_size (f,
					   FRAME_CR_SURFACE_DESIRED_WIDTH (f),
					   FRAME_CR_SURFACE_DESIRED_HEIGHT (f),
					   true);
    }
}

/* ==========================================================================

    Lisp definitions

   ========================================================================== */

DEFUN ("pgtk-set-monitor-scale-factor", Fpgtk_set_monitor_scale_factor,
       Spgtk_set_monitor_scale_factor, 2, 2, 0,
       doc: /* Set monitor MONITOR-MODEL's scale factor to SCALE-FACTOR.
Since Gdk's scale factor is integer, physical pixel width/height is
incorrect when you specify fractional scale factor in compositor.
If you set scale factor by this function, it is used instead of Gdk's one.

Pass nil as SCALE-FACTOR if you want to reset the specified monitor's
scale factor.  */)
  (Lisp_Object monitor_model, Lisp_Object scale_factor)
{
  CHECK_STRING (monitor_model);
  if (!NILP (scale_factor))
    {
      CHECK_NUMBER (scale_factor);
      if (FIXNUMP (scale_factor))
	{
	  if (XFIXNUM (scale_factor) <= 0)
	    error ("Scale factor must be > 0");
	}
      else if (FLOATP (scale_factor))
	{
	  if (XFLOAT_DATA (scale_factor) <= 0.0)
	    error ("Scale factor must be > 0");
	}
      else
	error ("Unknown type of scale-factor");
    }

  Lisp_Object tem = Fassoc (monitor_model, monitor_scale_factor_alist, Qnil);
  if (NILP (tem))
    {
      if (!NILP (scale_factor))
	monitor_scale_factor_alist = Fcons (Fcons (monitor_model, scale_factor),
					    monitor_scale_factor_alist);
    }
  else
    Fsetcdr (tem, scale_factor);

  return scale_factor;
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame, 1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */ )
  (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only = false;
  bool undecorated = false, override_redirect = false;
  long window_prompting = 0;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct pgtk_display_info *dpyinfo = NULL;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;

  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display =
    gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0, RES_TYPE_NUMBER);
  if (BASE_EQ (display, Qunbound))
    display =
      gui_display_get_arg (dpyinfo, parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (BASE_EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_pgtk_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name =
    gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name",
			 RES_TYPE_STRING);
  if (!STRINGP (name) && !BASE_EQ (name, Qunbound) && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent =
    gui_display_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL,
			 RES_TYPE_NUMBER);
  if (BASE_EQ (parent, Qunbound))
    parent = Qnil;
  if (!NILP (parent))
    CHECK_NUMBER (parent);

  frame = Qnil;
  tem =
    gui_display_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer",
			 "Minibuffer", RES_TYPE_SYMBOL);
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

  parent_frame =
    gui_display_get_arg (dpyinfo, parms, Qparent_frame, NULL, NULL,
			 RES_TYPE_SYMBOL);
  /* Accept parent-frame iff parent-id was not specified.  */
  if (!NILP (parent)
      || BASE_EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame))
      || !FRAME_PGTK_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  if (!NILP
      (tem =
       (gui_display_get_arg
	(dpyinfo, parms, Qundecorated, NULL, NULL, RES_TYPE_BOOLEAN)))
      && !(BASE_EQ (tem, Qunbound)))
    undecorated = true;

  FRAME_UNDECORATED (f) = undecorated;
  store_frame_param (f, Qundecorated, undecorated ? Qt : Qnil);

  if (!NILP
      (tem =
       (gui_display_get_arg
	(dpyinfo, parms, Qoverride_redirect, NULL, NULL, RES_TYPE_BOOLEAN)))
      && !(BASE_EQ (tem, Qunbound)))
    override_redirect = true;

  FRAME_OVERRIDE_REDIRECT (f) = override_redirect;
  store_frame_param (f, Qoverride_redirect, override_redirect ? Qt : Qnil);

  XSETFRAME (frame, f);

  frame_set_id_from_params (f, parms);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_pgtk;
  FRAME_X_OUTPUT (f) = xzalloc (sizeof *FRAME_X_OUTPUT (f));
  FRAME_FONTSET (f) = -1;
  FRAME_X_OUTPUT (f)->white_relief.pixel = -1;
  FRAME_X_OUTPUT (f)->black_relief.pixel = -1;

  FRAME_X_OUTPUT (f)->scrollbar_foreground_css_provider =
    gtk_css_provider_new ();
  FRAME_X_OUTPUT (f)->scrollbar_background_css_provider =
    gtk_css_provider_new ();

  fset_icon_name (f,
		  gui_display_get_arg (dpyinfo, parms, Qicon_name, "iconName",
				       "Title", RES_TYPE_STRING));
  if (!STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (do_unwind_create_frame, frame);

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    FRAME_X_OUTPUT (f)->cursor_color = -1;
    FRAME_X_OUTPUT (f)->cursor_foreground_color = -1;
    FRAME_X_OUTPUT (f)->border_pixel = -1;
    FRAME_X_OUTPUT (f)->mouse_color = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT (f)->cursor_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT (f)->cursor_foreground_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT (f)->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT (f)->mouse_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Specify the parent under which to make this X window.  */
  if (!NILP (parent))
    {
      FRAME_X_OUTPUT (f)->parent_desc = (Window) XFIXNAT (parent);
      FRAME_X_OUTPUT (f)->explicit_parent = true;
    }
  else
    {
      FRAME_X_OUTPUT (f)->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
      FRAME_X_OUTPUT (f)->explicit_parent = false;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (BASE_EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->x_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* Use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  register_font_driver (&ftcrfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&ftcrhbfont_driver, f);
#endif	/* HAVE_HARFBUZZ */

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
			 "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  pgtk_default_font_parameter (f, parms);
  if (!FRAME_FONT (f))
    {
      delete_frame (frame, Qnoelisp);
      error ("Invalid frame font");
    }

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
			 "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
				   "internalBorder", "internalBorder",
				   RES_TYPE_NUMBER);
      if (!BASE_EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value), parms);
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
  gui_default_parameter (f, parms, Qvertical_scroll_bars,
			 Qright,
			 "verticalScrollBars", "ScrollBars", RES_TYPE_SYMBOL);
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

  gui_default_parameter (f, parms, Qscroll_bar_foreground, Qnil,
			 "scrollBarForeground", "ScrollBarForeground",
			 RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qscroll_bar_background, Qnil,
			 "scrollBarBackground", "ScrollBarBackground",
			 RES_TYPE_STRING);

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  /* We have to call adjust_frame_size here since otherwise
     pgtk_set_tool_bar_lines will already work with the character
     sizes installed by init_frame_faces while the frame's pixel size
     is still calculated from a character size of 1 and we
     subsequently hit the (height >= 0) assertion in
     window_box_height.

     The non-pixelwise code apparently worked around this because it
     had one frame line vs one toolbar line which left us with a zero
     root window height which was obviously wrong as well ...

     Also process `min-width' and `min-height' parameters right here
     because `frame-windows-min-size' needs them.  */
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_width, NULL, NULL,
			     RES_TYPE_NUMBER);
  if (NUMBERP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_height, NULL, NULL,
			     RES_TYPE_NUMBER);
  if (NUMBERP (tem))
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
  window_prompting =
    gui_figure_window_size (f, parms, true, true);

  tem =
    gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0,
			 RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  xg_create_frame_widgets (f);
  pgtk_set_event_handler (f);

  if (FRAME_GTK_OUTER_WIDGET (f))
    gtk_widget_realize (FRAME_GTK_OUTER_WIDGET (f));

  /* Many callers (including the Lisp functions that call
     FRAME_SCALE_FACTOR) expect the widget to be realized.  */
  if (FRAME_GTK_WIDGET (f))
    gtk_widget_realize (FRAME_GTK_WIDGET (f));

#define INSTALL_CURSOR(FIELD, NAME) \
  FRAME_X_OUTPUT (f)->FIELD = gdk_cursor_new_for_display (FRAME_X_DISPLAY (f), GDK_ ## NAME)

  INSTALL_CURSOR (text_cursor, XTERM);
  INSTALL_CURSOR (nontext_cursor, LEFT_PTR);
  INSTALL_CURSOR (modeline_cursor, XTERM);
  INSTALL_CURSOR (hand_cursor, HAND2);
  INSTALL_CURSOR (hourglass_cursor, WATCH);
  INSTALL_CURSOR (horizontal_drag_cursor, SB_H_DOUBLE_ARROW);
  INSTALL_CURSOR (vertical_drag_cursor, SB_V_DOUBLE_ARROW);
  INSTALL_CURSOR (left_edge_cursor, LEFT_SIDE);
  INSTALL_CURSOR (right_edge_cursor, RIGHT_SIDE);
  INSTALL_CURSOR (top_edge_cursor, TOP_SIDE);
  INSTALL_CURSOR (bottom_edge_cursor, BOTTOM_SIDE);
  INSTALL_CURSOR (top_left_corner_cursor, TOP_LEFT_CORNER);
  INSTALL_CURSOR (top_right_corner_cursor, TOP_RIGHT_CORNER);
  INSTALL_CURSOR (bottom_right_corner_cursor, BOTTOM_RIGHT_CORNER);
  INSTALL_CURSOR (bottom_left_corner_cursor, BOTTOM_LEFT_CORNER);

#undef INSTALL_CURSOR

  /* Now consider the frame official.  */
  f->terminal->reference_count++;
  FRAME_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  gui_default_parameter (f, parms, Qicon_type, Qt,
			 "bitmapIcon", "BitmapIcon", RES_TYPE_BOOLEAN);

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
			 "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
			 "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
			 "cursorType", "CursorType", RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qscroll_bar_width, Qnil,
			 "scrollBarWidth", "ScrollBarWidth", RES_TYPE_NUMBER);
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

      GtkWidget *fixed = FRAME_GTK_WIDGET (f);
      GtkWidget *fixed_of_p = FRAME_GTK_WIDGET (p);
      GtkWidget *whbox_of_f = gtk_widget_get_parent (fixed);
      g_object_ref (fixed);
      gtk_container_remove (GTK_CONTAINER (whbox_of_f), fixed);
      gtk_fixed_put (GTK_FIXED (fixed_of_p), fixed, f->left_pos, f->top_pos);
      gtk_widget_show_all (fixed);
      g_object_unref (fixed);

      gtk_widget_destroy (FRAME_GTK_OUTER_WIDGET (f));
      FRAME_GTK_OUTER_WIDGET (f) = NULL;
      FRAME_OUTPUT_DATA (f)->vbox_widget = NULL;
      FRAME_OUTPUT_DATA (f)->hbox_widget = NULL;
      FRAME_OUTPUT_DATA (f)->menubar_widget = NULL;
      FRAME_OUTPUT_DATA (f)->toolbar_widget = NULL;
      FRAME_OUTPUT_DATA (f)->ttip_widget = NULL;
      FRAME_OUTPUT_DATA (f)->ttip_lbl = NULL;
      FRAME_OUTPUT_DATA (f)->ttip_window = NULL;

      unblock_input ();
    }

  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      GList *w = gtk_container_get_children (GTK_CONTAINER (FRAME_GTK_OUTER_WIDGET (f)));
      for (; w != NULL; w = w->next)
	gtk_widget_show_all (GTK_WIDGET (w->data));
    }

  gui_default_parameter (f, parms, Qno_focus_on_map, Qnil,
			 NULL, NULL, RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qno_accept_focus, Qnil,
			 NULL, NULL, RES_TYPE_BOOLEAN);

  /* Create the menu bar.  */
  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    {
      /* If this signals an error, we haven't set size hints for the
         frame and we didn't make it visible.  */
      initialize_frame_menubar (f);

    }

  /* Consider frame official, now.  */
  f->can_set_window_size = true;

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  block_input ();
  xg_wm_set_size_hint (f, window_prompting, false);
  unblock_input ();

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, Qx_create_frame_2);

  /* Process fullscreen parameter here in the hope that normalizing a
     fullheight/fullwidth frame will produce the size set by the last
     adjust_frame_size call.  */
  gui_default_parameter (f, parms, Qfullscreen, Qnil,
			 "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (!FRAME_X_OUTPUT (f)->explicit_parent)
    {
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
	  pgtk_iconify_frame (f);
	}
      else
	{
	  if (BASE_EQ (visibility, Qunbound))
	    visibility = Qt;

	  if (!NILP (visibility))
	    pgtk_make_frame_visible (f);
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
    }

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

  /* All remaining specified parameters, which have not been "used"
     by gui_display_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  FRAME_X_OUTPUT (f)->border_color_css_provider = NULL;

  FRAME_X_OUTPUT (f)->cr_surface_visible_bell = NULL;
  FRAME_X_OUTPUT (f)->atimer_visible_bell = NULL;
  FRAME_X_OUTPUT (f)->watched_scale_factor = 1.0;
  struct timespec ts = make_timespec (1, 0);
  FRAME_X_OUTPUT (f)->scale_factor_atimer = start_atimer(ATIMER_CONTINUOUS,
							 ts,
							 update_watched_scale_factor,
							 f);

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}

/* Restack frame F1 below frame F2, above if ABOVE_FLAG is non-nil.
   In practice this is a two-step action: The first step removes F1's
   window-system window from the display.  The second step reinserts
   F1's window below (above if ABOVE_FLAG is true) that of F2.  */
static void
pgtk_frame_restack (struct frame *f1, struct frame *f2, bool above_flag)
{
  block_input ();
  xg_frame_restack (f1, f2, above_flag);
  unblock_input ();
}

DEFUN ("pgtk-frame-restack", Fpgtk_frame_restack, Spgtk_frame_restack, 2, 3, 0,
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

Some window managers may refuse to restack windows.  */)
  (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object above)
{
  struct frame *f1 = decode_live_frame (frame1);
  struct frame *f2 = decode_live_frame (frame2);

  if (!(FRAME_GTK_OUTER_WIDGET (f1) && FRAME_GTK_OUTER_WIDGET (f2)))
    error ("Cannot restack frames");
  pgtk_frame_restack (f1, f2, !NILP (above));
  return Qt;
}

#ifdef HAVE_GSETTINGS

#define RESOURCE_KEY_MAX_LEN 128
#define SCHEMA_ID "org.gnu.emacs.defaults"
#define PATH_FOR_CLASS_TYPE "/org/gnu/emacs/defaults-by-class/"
#define PATH_PREFIX_FOR_NAME_TYPE "/org/gnu/emacs/defaults-by-name/"
#define PATH_MAX_LEN \
  (max (sizeof PATH_FOR_CLASS_TYPE, sizeof PATH_PREFIX_FOR_NAME_TYPE))

static inline int
pgtk_is_lower_char (int c)
{
  return c >= 'a' && c <= 'z';
}

static inline int
pgtk_is_upper_char (int c)
{
  return c >= 'A' && c <= 'Z';
}

static inline int
pgtk_is_numeric_char (int c)
{
  return c >= '0' && c <= '9';
}

static GSettings *
parse_resource_key (const char *res_key, char *setting_key)
{
  char path[PATH_MAX_LEN + RESOURCE_KEY_MAX_LEN];
  const char *sp = res_key;
  char *dp;

  /*
   * res_key="emacs.cursorBlink"
   *   -> path="/org/gnu/emacs/defaults-by-name/emacs/"
   *      setting_key="cursor-blink"
   *
   * res_key="Emacs.CursorBlink"
   *   -> path="/org/gnu/emacs/defaults-by-class/"
   *      setting_key="cursor-blink"
   *
   * Returns GSettings* if setting_key exists in schema, otherwise NULL.
   */

  /* generate path */
  if (pgtk_is_upper_char (*sp))
    {
      /* First letter is upper case.  It should be "Emacs",
       * but don't care.
       */
      strcpy (path, PATH_FOR_CLASS_TYPE);
      while (*sp != '\0')
	{
	  if (*sp == '.')
	    break;
	  sp++;
	}
    }
  else
    {
      strcpy (path, PATH_PREFIX_FOR_NAME_TYPE);
      dp = path + strlen (path);
      while (*sp != '\0')
	{
	  int c = *sp;
	  if (c == '.')
	    break;
	  if (pgtk_is_lower_char (c))
	    (void) 0;		/* lower -> NOP */
	  else if (pgtk_is_upper_char (c))
	    c = c - 'A' + 'a';	/* upper -> lower */
	  else if (pgtk_is_numeric_char (c))
	    (void) 0;		/* numeric -> NOP */
	  else
	    return NULL;	/* invalid */
	  *dp++ = c;
	  sp++;
	}
      *dp++ = '/';		/* must end with '/' */
      *dp = '\0';
    }

  if (*sp++ != '.')
    return NULL;

  /* generate setting_key */
  dp = setting_key;
  while (*sp != '\0')
    {
      int c = *sp;
      if (pgtk_is_lower_char (c))
	(void) 0;		/* lower -> NOP */
      else if (pgtk_is_upper_char (c))
	{
	  c = c - 'A' + 'a';	/* upper -> lower */
	  if (dp != setting_key)
	    *dp++ = '-';	/* store '-' unless first char */
	}
      else if (pgtk_is_numeric_char (c))
	(void) 0;		/* numeric -> NOP */
      else
	return NULL;		/* invalid */

      *dp++ = c;
      sp++;
    }
  *dp = '\0';

  /* check existence of setting_key */
  GSettingsSchemaSource *ssrc = g_settings_schema_source_get_default ();
  GSettingsSchema *scm = g_settings_schema_source_lookup (ssrc, SCHEMA_ID, TRUE);
  if (!scm)
    return NULL;	/* *.schema.xml is not installed. */
  if (!g_settings_schema_has_key (scm, setting_key))
    {
      g_settings_schema_unref (scm);
      return NULL;
    }

  /* create GSettings, and return it */
  GSettings *gs = g_settings_new_full (scm, NULL, path);

  g_settings_schema_unref (scm);
  return gs;
}

static void
pgtk_check_resource_key_length (const char *key)
{
  if (strnlen (key, RESOURCE_KEY_MAX_LEN) >= RESOURCE_KEY_MAX_LEN)
    error ("Resource key too long");
}

const char *
pgtk_get_defaults_value (const char *key)
{
  char skey[(RESOURCE_KEY_MAX_LEN + 1) * 2];

  pgtk_check_resource_key_length (key);

  GSettings *gs = parse_resource_key (key, skey);
  if (gs == NULL)
    return NULL;

  gchar *str = g_settings_get_string (gs, skey);

  /* There is no timing to free str.
   * So, copy it here and free it.
   *
   * MEMO: Resource values for emacs shouldn't need such a long string value.
   */
  static char holder[128];
  strncpy (holder, str, 128);
  holder[127] = '\0';

  g_object_unref (gs);
  g_free (str);
  return holder[0] != '\0' ? holder : NULL;
}

static void
pgtk_set_defaults_value (const char *key, const char *value)
{
  char skey[(RESOURCE_KEY_MAX_LEN + 1) * 2];

  pgtk_check_resource_key_length (key);

  GSettings *gs = parse_resource_key (key, skey);
  if (gs == NULL)
    error ("Unknown resource key");

  if (value != NULL)
    g_settings_set_string (gs, skey, value);
  else
    g_settings_reset (gs, skey);

  g_object_unref (gs);
}

#undef RESOURCE_KEY_MAX_LEN
#undef SCHEMA_ID
#undef PATH_FOR_CLASS_TYPE
#undef PATH_PREFIX_FOR_NAME_TYPE
#undef PATH_MAX_LEN

#else /* not HAVE_GSETTINGS */

const char *
pgtk_get_defaults_value (const char *key)
{
  return NULL;
}

static void
pgtk_set_defaults_value (const char *key, const char *value)
{
  error ("gsettings not supported");
}

#endif


DEFUN ("pgtk-set-resource", Fpgtk_set_resource, Spgtk_set_resource, 2, 2, 0,
       doc: /* Set the value of ATTRIBUTE, of class CLASS, as VALUE, into defaults database. */ )
  (Lisp_Object attribute, Lisp_Object value)
{
  check_window_system (NULL);

  CHECK_STRING (attribute);
  if (!NILP (value))
    CHECK_STRING (value);

  char *res = SSDATA (Vx_resource_name);
  char *attr = SSDATA (attribute);
  if (attr[0] >= 'A' && attr[0] <= 'Z')
    res = SSDATA (Vx_resource_class);

  char *key = g_strdup_printf ("%s.%s", res, attr);

  pgtk_set_defaults_value (key, NILP (value) ? NULL : SSDATA (value));

  return Qnil;
}


DEFUN ("x-server-max-request-size", Fx_server_max_request_size, Sx_server_max_request_size, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */ )
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  /* This function has no real equivalent under PGTK.  Return nil to
     indicate this. */
  return Qnil;
}


DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return make_fixnum (1);
}


DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy;
  gint n_monitors, i;
  int height_mm_at_0 = 0, height_mm_at_other = 0;

  block_input ();
  gdpy = dpyinfo->gdpy;
  n_monitors = gdk_display_get_n_monitors (gdpy);

  for (i = 0; i < n_monitors; ++i)
    {
      GdkRectangle rec;

      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      gdk_monitor_get_geometry (monitor, &rec);

      int mm = gdk_monitor_get_height_mm (monitor);

      if (rec.y == 0)
	height_mm_at_0 = max (height_mm_at_0, mm);
      else
	height_mm_at_other += mm;
    }

  unblock_input ();

  return make_fixnum (height_mm_at_0 + height_mm_at_other);
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy;
  gint n_monitors, i;
  int width_mm_at_0 = 0, width_mm_at_other = 0;

  block_input ();
  gdpy = dpyinfo->gdpy;
  n_monitors = gdk_display_get_n_monitors (gdpy);

  for (i = 0; i < n_monitors; ++i)
    {
      GdkRectangle rec;

      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      gdk_monitor_get_geometry (monitor, &rec);

      int mm = gdk_monitor_get_width_mm (monitor);

      if (rec.x == 0)
	width_mm_at_0 = max (width_mm_at_0, mm);
      else
	width_mm_at_other += mm;
    }

  unblock_input ();

  return make_fixnum (width_mm_at_0 + width_mm_at_other);
}


DEFUN ("x-display-backing-store", Fx_display_backing_store, Sx_display_backing_store, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qnil;
}


DEFUN ("x-display-visual-class", Fx_display_visual_class, Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return Qtrue_color;
}


DEFUN ("x-display-save-under", Fx_display_save_under, Sx_display_save_under, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qnil;
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection, 1, 3, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object display, Lisp_Object resource_string, Lisp_Object must_succeed)
{
  struct pgtk_display_info *dpyinfo;

  if (NILP (display))
    display = build_string ("");

  CHECK_STRING (display);

  dpyinfo = pgtk_term_init (display, SSDATA (Vx_resource_name));
  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Display on %s not responding.\n", SSDATA (display));
      else
	error ("Display on %s not responding.\n", SSDATA (display));
    }

  return Qnil;
}


DEFUN ("x-close-connection", Fx_close_connection, Sx_close_connection, 1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  pgtk_delete_terminal (dpyinfo->terminal);

  return Qnil;
}


DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct pgtk_display_info *ndi;

  for (ndi = x_display_list; ndi; ndi = ndi->next)
    result = Fcons (XCAR (ndi->name_list_element), result);

  return result;
}

DEFUN ("pgtk-font-name", Fpgtk_font_name, Spgtk_font_name, 1, 1, 0,
       doc: /* Determine font PostScript or family name for font NAME.
NAME should be a string containing either the font name or an XLFD
font descriptor.  If string contains `fontset' and not
`fontset-startup', it is left alone. */)
  (Lisp_Object name)
{
  char *nm;
  CHECK_STRING (name);
  nm = SSDATA (name);

  if (nm[0] != '-')
    return name;
  if (strstr (nm, "fontset") && !strstr (nm, "fontset-startup"))
    return name;

  char *str = pgtk_xlfd_to_fontname (SSDATA (name));
  name = build_string (str);
  xfree (str);
  return name;
}

/* ==========================================================================

    Miscellaneous functions not called through hooks

   ========================================================================== */

/* Called from frame.c.  */
struct pgtk_display_info *
check_x_display_info (Lisp_Object frame)
{
  return check_pgtk_display_info (frame);
}

void
pgtk_set_scroll_bar_default_width (struct frame *f)
{
  int unit = FRAME_COLUMN_WIDTH (f);
  int minw = xg_get_default_scrollbar_width (f);
  /* A minimum width of 14 doesn't look good for toolkit scroll bars.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (minw + unit - 1) / unit;
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = minw;
}

void
pgtk_set_scroll_bar_default_height (struct frame *f)
{
  int height = FRAME_LINE_HEIGHT (f);
  int min_height = xg_get_default_scrollbar_height (f);
  /* A minimum height of 14 doesn't look good for toolkit scroll bars.  */
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = min_height;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (min_height + height - 1) / height;
}

/* Terminals implement this instead of x-get-resource directly.  */
const char *
pgtk_get_string_resource (XrmDatabase rdb, const char *name,
			  const char *class)
{
  check_window_system (NULL);

  if (inhibit_x_resources)
    /* --quick was passed, so this is a no-op.  */
    return NULL;

  const char *res = pgtk_get_defaults_value (name);
  if (res == NULL)
    res = pgtk_get_defaults_value (class);

  if (res == NULL)
    return NULL;

  if (c_strncasecmp (res, "YES", 3) == 0)
    return "true";

  if (c_strncasecmp (res, "NO", 2) == 0)
    return "false";

  return res;
}

Lisp_Object
pgtk_get_focus_frame (struct frame *frame)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  Lisp_Object focus;

  if (!dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (focus, dpyinfo->x_focus_frame);
  return focus;
}

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (pgtk_defined_color (f, SSDATA (color), &col, false, false))
    return Qt;
  else
    return Qnil;
}


DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (pgtk_defined_color (f, SSDATA (color), &col, false, false))
    return list3i (col.red, col.green, col.blue);
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  return Qt;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy;
  gint n_monitors, i;
  int width = 0;

  block_input ();
  gdpy = dpyinfo->gdpy;
  n_monitors = gdk_display_get_n_monitors (gdpy);

  for (i = 0; i < n_monitors; ++i)
    {
      GdkRectangle rec;
      double scale = 1;

      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      gdk_monitor_get_geometry (monitor, &rec);

      /* GTK returns scaled sizes for the workareas.  */
      scale = pgtk_get_monitor_scale_factor (gdk_monitor_get_model (monitor));
      if (scale == 0.0)
	scale = gdk_monitor_get_scale_factor (monitor);
      rec.x = rec.x * scale + 0.5;
      rec.y = rec.y * scale + 0.5;
      rec.width = rec.width * scale + 0.5;
      rec.height = rec.height * scale + 0.5;

      width = max (width, rec.x + rec.width);
    }

  unblock_input ();

  return make_fixnum (width);
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height, Sx_display_pixel_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy;
  gint n_monitors, i;
  int height = 0;

  block_input ();
  gdpy = dpyinfo->gdpy;
  n_monitors = gdk_display_get_n_monitors (gdpy);

  for (i = 0; i < n_monitors; ++i)
    {
      GdkRectangle rec;
      double scale = 1;

      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      gdk_monitor_get_geometry (monitor, &rec);

      /* GTK returns scaled sizes for the workareas.  */
      scale = pgtk_get_monitor_scale_factor (gdk_monitor_get_model (monitor));
      if (scale == 0.0)
	scale = gdk_monitor_get_scale_factor (monitor);
      rec.x = rec.x * scale + 0.5;
      rec.y = rec.y * scale + 0.5;
      rec.width = rec.width * scale + 0.5;
      rec.height = rec.height * scale + 0.5;

      height = max (height, rec.y + rec.height);
    }

  unblock_input ();

  return make_fixnum (height);
}

DEFUN ("pgtk-display-monitor-attributes-list", Fpgtk_display_monitor_attributes_list,
       Spgtk_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

In addition to the standard attribute keys listed in
`display-monitor-attributes-list', the following keys are contained in
the attributes:

 source -- String describing the source from which multi-monitor
	   information is obtained, \"Gdk\"

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  Lisp_Object attributes_list = Qnil;

  GdkDisplay *gdpy;
  gint primary_monitor = 0, n_monitors, i;
  Lisp_Object monitor_frames, rest, frame;
  static const char *source = "Gdk";
  struct MonitorInfo *monitors;

  block_input ();
  gdpy = dpyinfo->gdpy;
  n_monitors = gdk_display_get_n_monitors (gdpy);
  monitor_frames = make_nil_vector (n_monitors);
  monitors = xzalloc (n_monitors * sizeof *monitors);

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_PGTK_P (f)
	  && FRAME_DISPLAY_INFO (f) == dpyinfo
	  && !FRAME_TOOLTIP_P (f))
	{
	  GdkWindow *gwin = gtk_widget_get_window (FRAME_GTK_WIDGET (f));

          for (i = 0; i < n_monitors; i++)
            if (gdk_display_get_monitor_at_window (gdpy, gwin)
                == gdk_display_get_monitor (gdpy, i))
              break;
	  ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
	}
    }

  for (i = 0; i < n_monitors; ++i)
    {
      gint width_mm, height_mm;
      GdkRectangle rec, work;
      struct MonitorInfo *mi = &monitors[i];
      double scale = 1;

      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      if (gdk_monitor_is_primary (monitor))
        primary_monitor = i;
      gdk_monitor_get_geometry (monitor, &rec);

      width_mm = gdk_monitor_get_width_mm (monitor);
      height_mm = gdk_monitor_get_height_mm (monitor);
      gdk_monitor_get_workarea (monitor, &work);

      /* GTK returns scaled sizes for the workareas.  */
      scale = pgtk_get_monitor_scale_factor (gdk_monitor_get_model (monitor));
      if (scale == 0.0)
	scale = gdk_monitor_get_scale_factor (monitor);
      rec.x = rec.x * scale + 0.5;
      rec.y = rec.y * scale + 0.5;
      rec.width = rec.width * scale + 0.5;
      rec.height = rec.height * scale + 0.5;
      work.x = work.x * scale + 0.5;
      work.y = work.y * scale + 0.5;
      work.width = work.width * scale + 0.5;
      work.height = work.height * scale + 0.5;

      mi->geom.x = rec.x;
      mi->geom.y = rec.y;
      mi->geom.width = rec.width;
      mi->geom.height = rec.height;
      mi->work.x = work.x;
      mi->work.y = work.y;
      mi->work.width = work.width;
      mi->work.height = work.height;
      mi->mm_width = width_mm;
      mi->mm_height = height_mm;
      mi->scale_factor = scale;

      dupstring (&mi->name, (gdk_monitor_get_model (monitor)));
    }

  attributes_list = make_monitor_attribute_list (monitors,
                                                 n_monitors,
                                                 primary_monitor,
                                                 monitor_frames,
                                                 source);
  free_monitors (monitors, n_monitors);
  unblock_input ();

  return attributes_list;
}

double
pgtk_frame_scale_factor (struct frame *f)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  GdkDisplay *gdpy = dpyinfo->gdpy;

  block_input ();

  GdkWindow *gwin = gtk_widget_get_window (FRAME_GTK_WIDGET (f));
  GdkMonitor *gmon = gdk_display_get_monitor_at_window (gdpy, gwin);

  /* GTK returns scaled sizes for the workareas.  */
  double scale = pgtk_get_monitor_scale_factor (gdk_monitor_get_model (gmon));
  if (scale == 0.0)
    scale = gdk_monitor_get_scale_factor (gmon);

  unblock_input ();

  return scale;
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return make_fixnum (32);
}


DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_fixnum (1 << min (dpyinfo->n_planes, 24));
}

/***********************************************************************
				Tool tips
 ***********************************************************************/

/* The frame of the currently visible tooltip.  */
static Lisp_Object tip_frame;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
static GtkWidget *tip_window;

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
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = NULL;
      tip_frame = Qnil;
    }
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  TEXT is the string to
   display in the tip frame.  Value is the frame.

   Note that functions called here, esp. gui_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
pgtk_create_tip_frame (struct pgtk_display_info *dpyinfo, Lisp_Object parms, struct frame *p)
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
  f->output_method = output_pgtk;
  f->output_data.pgtk = xzalloc (sizeof *f->output_data.pgtk);
  FRAME_FONTSET (f) = -1;
  f->output_data.pgtk->white_relief.pixel = -1;
  f->output_data.pgtk->black_relief.pixel = -1;

  f->tooltip = true;
  fset_icon_name (f, Qnil);
  FRAME_DISPLAY_INFO (f) = dpyinfo;
  f->output_data.pgtk->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
  f->output_data.pgtk->explicit_parent = false;

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.pgtk->border_pixel = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.pgtk->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (BASE_EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->x_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  register_font_driver (&ftcrfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&ftcrhbfont_driver, f);
#endif	/* HAVE_HARFBUZZ */

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  pgtk_default_font_parameter (f, parms);

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                         "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 2 in order to match xterm.  We recognize either
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

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  f->output_data.pgtk->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;

  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  gui_figure_window_size (f, parms, false, false);

  xg_create_frame_widgets (f);
  pgtk_set_event_handler (f);
  tip_window = FRAME_GTK_OUTER_WIDGET (f);
  gtk_window_set_transient_for (GTK_WINDOW (tip_window),
				GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (p)));
  gtk_window_set_attached_to (GTK_WINDOW (tip_window), FRAME_GTK_WIDGET (p));
  gtk_window_set_destroy_with_parent (GTK_WINDOW (tip_window), TRUE);
  gtk_window_set_decorated (GTK_WINDOW (tip_window), FALSE);
  gtk_window_set_type_hint (GTK_WINDOW (tip_window), GDK_WINDOW_TYPE_HINT_TOOLTIP);
  f->output_data.pgtk->current_cursor = f->output_data.pgtk->text_cursor;

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
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame gets set, which lets the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
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
  FRAME_DISPLAY_INFO (f)->reference_count++;
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

/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx,
		Lisp_Object dy, int width, int height, int *root_x,
		int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int min_x, min_y, max_x, max_y = -1;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if ((!FIXNUMP (left) && !FIXNUMP (right))
      || (!FIXNUMP (top) && !FIXNUMP (bottom)))
    {
      Lisp_Object frame, attributes, monitor, geometry;
      GdkSeat *seat =
	gdk_display_get_default_seat (FRAME_DISPLAY_INFO (f)->gdpy);
      GdkDevice *dev = gdk_seat_get_pointer (seat);
      GdkScreen *scr;

      block_input ();
      gdk_device_get_position (dev, &scr, root_x, root_y);
      unblock_input ();

      XSETFRAME (frame, f);
      attributes = Fpgtk_display_monitor_attributes_list (frame);

      /* Try to determine the monitor where the mouse pointer is and
         its geometry.  See bug#22549.  */
      while (CONSP (attributes))
	{
	  monitor = XCAR (attributes);
	  geometry = Fassq (Qgeometry, monitor);
	  if (CONSP (geometry))
	    {
	      min_x = XFIXNUM (Fnth (make_fixnum (1), geometry));
	      min_y = XFIXNUM (Fnth (make_fixnum (2), geometry));
	      max_x = min_x + XFIXNUM (Fnth (make_fixnum (3), geometry));
	      max_y = min_y + XFIXNUM (Fnth (make_fixnum (4), geometry));
	      if (min_x <= *root_x && *root_x < max_x
		  && min_y <= *root_y && *root_y < max_y)
		{
		  break;
		}
	      max_y = -1;
	    }

	  attributes = XCDR (attributes);
	}
    }

  /* It was not possible to determine the monitor's geometry, so we
     assign some sane defaults here: */
  if (max_y < 0)
    {
      min_x = 0;
      min_y = 0;
      max_x = pgtk_display_pixel_width (FRAME_DISPLAY_INFO (f));
      max_y = pgtk_display_pixel_height (FRAME_DISPLAY_INFO (f));
    }

  if (FIXNUMP (top))
    *root_y = XFIXNUM (top);
  else if (FIXNUMP (bottom))
    *root_y = XFIXNUM (bottom) - height;
  else if (*root_y + XFIXNUM (dy) <= min_y)
    *root_y = min_y;		/* Can happen for negative dy */
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
    *root_x = 0;		/* Can happen for negative dx */
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


/* Hide tooltip.  Delete its frame if DELETE is true.  */
static Lisp_Object
pgtk_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      calln (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

  /* Any GTK+ system tooltip can be found via the x_output structure of
     tip_last_frame, provided that frame is still live.  Any Emacs
     tooltip is found via the tip_frame variable.  Note that the current
     value of x_gtk_use_system_tooltips might not be the same as used
     for the tooltip we have to hide, see Bug#30399.  */
  if ((NILP (tip_last_frame) && NILP (tip_frame))
      || (!use_system_tooltips
	  && !delete
	  && FRAMEP (tip_frame)
	  && FRAME_LIVE_P (XFRAME (tip_frame))
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    /* Either there's no tooltip to hide or it's an already invisible
       Emacs tooltip and we don't want to change its type.  Return
       quickly.  */
    return Qnil;
  else
    {
      Lisp_Object was_open = Qnil;

      specpdl_ref count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      /* Try to hide the GTK+ system tip first.  */
      if (FRAMEP (tip_last_frame))
	{
	  struct frame *f = XFRAME (tip_last_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (xg_hide_tooltip (f))
		was_open = Qt;
	    }
	}

      /* When using GTK+ system tooltips (compare Bug#41200) reset
	 tip_last_frame.  It will be reassigned when showing the next
	 GTK+ system tooltip.  */
      if (use_system_tooltips)
	tip_last_frame = Qnil;

      /* Now look whether there's an Emacs tip around.  */
      if (FRAMEP (tip_frame))
	{
	  struct frame *f = XFRAME (tip_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (delete || use_system_tooltips)
		{
		  /* Delete the Emacs tooltip frame when DELETE is true
		     or we change the tooltip type from an Emacs one to
		     a GTK+ system one.  */
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		pgtk_make_frame_invisible (f);

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

DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
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

  if (!FRAME_GTK_OUTER_WIDGET (f))
    return unbind_to (count, Qnil);

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

  if (use_system_tooltips)
    {
      bool ok;

      /* Hide a previous tip, if any.  */
      Fx_hide_tip ();

      block_input ();

      ok = true;
      xg_show_tooltip (f, string);
      tip_last_frame = frame;

      unblock_input ();
      if (ok) goto start_timer;
    }

  if (FRAMEP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	  && EQ (frame, tip_last_frame)
	  && !NILP (Fequal_including_properties (tip_last_string, string))
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
	  gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (tip_f)), root_x, root_y);
	  unblock_input ();

	  goto start_timer;
	}
      else if (tooltip_reuse_hidden_frame && EQ (frame, tip_last_frame))
	{
	  bool delete = false;
	  Lisp_Object tail, elt, parm, last;

	  /* Check if every parameter in PARMS has the same value in
	     tip_last_parms.  This may destruct tip_last_parms which,
	     however, will be recreated below.  */
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
		      calln (Qassq_delete_all, parm, tip_last_parms);
		}
	      else
		tip_last_parms =
		  calln (Qassq_delete_all, parm, tip_last_parms);
	    }

	  /* Now check if every parameter in what is left of
	     tip_last_parms with a non-nil value has an association in
	     PARMS.  */
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

	  pgtk_hide_tip (delete);
	}
      else
	pgtk_hide_tip (true);
    }
  else
    pgtk_hide_tip (true);

  tip_last_frame = frame;
  tip_last_string = string;
  tip_last_parms = parms;

  if (!FRAMEP (tip_frame) || !FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      /* Add default values to frame parameters.  */
      if (NILP (Fassq (Qname, parms)))
	parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
      if (NILP (Fassq (Qinternal_border_width, parms)))
	parms = Fcons (Fcons (Qinternal_border_width, make_fixnum (3)), parms);
      if (NILP (Fassq (Qborder_width, parms)))
	parms = Fcons (Fcons (Qborder_width, make_fixnum (1)), parms);
      if (NILP (Fassq (Qborder_color, parms)))
	parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
      if (NILP (Fassq (Qbackground_color, parms)))
	parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		       parms);

      /* Create a frame for the tooltip, and record it in the global
	 variable tip_frame.  */
      if (NILP ((tip_frame = pgtk_create_tip_frame (FRAME_DISPLAY_INFO (f),
						    parms, f))))
	/* Creating the tip frame failed.  */
	return unbind_to (count, Qnil);
    }
  else
    tip_window = FRAME_X_WINDOW (XFRAME (tip_frame));

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
  width = XFIXNUM (Fcar (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  height = XFIXNUM (Fcdr (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  width += FRAME_COLUMN_WIDTH (tip_f);

  /* Calculate position of tooltip frame.  */
  compute_tip_xy (tip_f, parms, dx, dy, width, height, &root_x, &root_y);

  /* Show tooltip frame.  */
  block_input ();
  gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (tip_f)), width, height);
  gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (tip_f)), root_x, root_y);
  gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (tip_f));
  SET_FRAME_VISIBLE (tip_f, 1);
  gdk_window_set_cursor (gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (tip_f)),
			 f->output_data.pgtk->current_cursor);

  unblock_input ();

  pgtk_cr_update_surface_desired_size (tip_f, width, height, false);

  w->must_be_updated_p = true;
  update_single_window (w);
  flush_frame (tip_f);
  set_buffer_internal_1 (old_buffer);
  unbind_to (count_1, Qnil);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = calln (Qrun_at_time, timeout, Qnil, Qx_hide_tip);

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  return pgtk_hide_tip (!tooltip_reuse_hidden_frame);
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
  Lisp_Object fullscreen_symbol = Fframe_parameter (frame, Qfullscreen);
  bool fullscreen = (EQ (fullscreen_symbol, Qfullboth)
		     || EQ (fullscreen_symbol, Qfullscreen));
  int border = fullscreen ? 0 : f->border_width;
  int title_height = 0;
  int native_width = FRAME_PIXEL_WIDTH (f);
  int native_height = FRAME_PIXEL_HEIGHT (f);
  int outer_width = native_width + 2 * border;
  int outer_height = native_height + 2 * border + title_height;

  /* Get these here because they can't be got in configure_event(). */
  int left_pos, top_pos;

  if (FRAME_GTK_OUTER_WIDGET (f))
    gtk_window_get_position (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			     &left_pos, &top_pos);
  else
    {
      GtkAllocation alloc;

      if (FRAME_GTK_WIDGET (f) == NULL)
	return Qnil;    /* This can occur while creating a frame.  */

      gtk_widget_get_allocation (FRAME_GTK_WIDGET (f), &alloc);
      left_pos = alloc.x;
      top_pos = alloc.y;
    }

  int native_left = left_pos + border;
  int native_top = top_pos + border + title_height;
  int native_right = left_pos + outer_width - border;
  int native_bottom = top_pos + outer_height - border;
  int internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  int tab_bar_height = 0, tab_bar_width = 0;
  int tool_bar_height = FRAME_TOOLBAR_HEIGHT (f);
  int tool_bar_width = (tool_bar_height
			? outer_width - 2 * internal_border_width : 0);

  tab_bar_height = FRAME_TAB_BAR_HEIGHT (f);
  tab_bar_width = (tab_bar_height
		   ? native_width - 2 * internal_border_width : 0);

  /* Construct list.  */
  if (EQ (attribute, Qouter_edges))
    return list4 (make_fixnum (left_pos), make_fixnum (top_pos),
		  make_fixnum (left_pos + outer_width),
		  make_fixnum (top_pos + outer_height));
  else if (EQ (attribute, Qnative_edges))
    return list4 (make_fixnum (native_left), make_fixnum (native_top),
		  make_fixnum (native_right), make_fixnum (native_bottom));
  else if (EQ (attribute, Qinner_edges))
    return list4 (make_fixnum (native_left + internal_border_width),
		  make_fixnum (native_top
			       + tab_bar_height
			       + FRAME_TOOL_BAR_TOP_HEIGHT (f)
			       + internal_border_width),
		  make_fixnum (native_right - internal_border_width),
		  make_fixnum (native_bottom - internal_border_width
			       - FRAME_TOOL_BAR_BOTTOM_HEIGHT (f)));
  else
    return
      list (Fcons (Qouter_position,
		   Fcons (make_fixnum (left_pos),
			  make_fixnum (top_pos))),
	    Fcons (Qouter_size,
		   Fcons (make_fixnum (outer_width),
			  make_fixnum (outer_height))),
	    Fcons (Qexternal_border_size,
		   (fullscreen
		    ? Fcons (make_fixnum (0), make_fixnum (0))
		    : Fcons (make_fixnum (border), make_fixnum (border)))),
	    Fcons (Qtitle_bar_size,
		   Fcons (make_fixnum (0), make_fixnum (title_height))),
	    Fcons (Qmenu_bar_external, Qnil),
	    Fcons (Qmenu_bar_size, Fcons (make_fixnum (0), make_fixnum (0))),
	    Fcons (Qtab_bar_size,
		   Fcons (make_fixnum (tab_bar_width),
			  make_fixnum (tab_bar_height))),
	    Fcons (Qtool_bar_external,
		   FRAME_EXTERNAL_TOOL_BAR (f) ? Qt : Qnil),
	    Fcons (Qtool_bar_position, FRAME_TOOL_BAR_POSITION (f)),
	    Fcons (Qtool_bar_size,
		   Fcons (make_fixnum (tool_bar_width),
			  make_fixnum (tool_bar_height))),
	    Fcons (Qinternal_border_width,
		   make_fixnum (internal_border_width)));
}

DEFUN ("pgtk-frame-geometry", Fpgtk_frame_geometry, Spgtk_frame_geometry, 0, 1, 0,
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

DEFUN ("pgtk-frame-edges", Fpgtk_frame_edges, Spgtk_frame_edges, 0, 2, 0,
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
				 ? type : Qnative_edges));
}

DEFUN ("pgtk-set-mouse-absolute-pixel-position",
       Fpgtk_set_mouse_absolute_pixel_position,
       Spgtk_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to absolute pixel position (X, Y).
The coordinates X and Y are interpreted in pixels relative to a position
\(0, 0) of the selected frame's display.  */)
  (Lisp_Object x, Lisp_Object y)
{
  struct frame *f = SELECTED_FRAME ();
  GtkWidget *widget = gtk_widget_get_toplevel (FRAME_WIDGET (f));
  GdkWindow *window = gtk_widget_get_window (widget);
  GdkDisplay *gdpy = gdk_window_get_display (window);
  GdkScreen *gscr = gdk_window_get_screen (window);
  GdkSeat *seat = gdk_display_get_default_seat (gdpy);
  GdkDevice *device = gdk_seat_get_pointer (seat);

  gdk_device_warp (device, gscr, XFIXNUM (x), XFIXNUM (y));	/* No effect on wayland. */

  return Qnil;
}

DEFUN ("pgtk-mouse-absolute-pixel-position",
       Fpgtk_mouse_absolute_pixel_position,
       Spgtk_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the
coordinates of the mouse cursor position in pixels relative to a
position (0, 0) of the selected frame's terminal. */)
  (void)
{
  struct frame *f = SELECTED_FRAME ();
  GtkWidget *widget = gtk_widget_get_toplevel (FRAME_WIDGET (f));
  GdkWindow *window = gtk_widget_get_window (widget);
  GdkDisplay *gdpy = gdk_window_get_display (window);
  GdkScreen *gscr;
  GdkSeat *seat = gdk_display_get_default_seat (gdpy);
  GdkDevice *device = gdk_seat_get_pointer (seat);
  int x = 0, y = 0;

  gdk_device_get_position (device, &gscr, &x, &y);	/* can't get on wayland? */

  return Fcons (make_fixnum (x), make_fixnum (y));
}


DEFUN ("pgtk-page-setup-dialog", Fpgtk_page_setup_dialog,
       Spgtk_page_setup_dialog, 0, 0, 0,
       doc: /* Pop up a page setup dialog.
The current page setup can be obtained using `x-get-page-setup'.  */)
  (void)
{
  block_input ();
  xg_page_setup_dialog ();
  unblock_input ();

  return Qnil;
}

DEFUN ("pgtk-get-page-setup", Fpgtk_get_page_setup,
       Spgtk_get_page_setup, 0, 0, 0,
       doc: /* Return the value of the current page setup.
The return value is an alist containing the following keys:

orientation: page orientation (symbol `portrait', `landscape',
`reverse-portrait', or `reverse-landscape').
width, height: page width/height in points not including margins.
left-margin, right-margin, top-margin, bottom-margin: print margins,
which is the parts of the page that the printer cannot print
on, in points.

The paper width can be obtained as the sum of width, left-margin, and
right-margin values if the page orientation is `portrait' or
`reverse-portrait'.  Otherwise, it is the sum of width, top-margin,
and bottom-margin values.  Likewise, the paper height is the sum of
height, top-margin, and bottom-margin values if the page orientation
is `portrait' or `reverse-portrait'.  Otherwise, it is the sum of
height, left-margin, and right-margin values.  */)
  (void)
{
  Lisp_Object result;

  block_input ();
  result = xg_get_page_setup ();
  unblock_input ();

  return result;
}

DEFUN ("pgtk-print-frames-dialog", Fpgtk_print_frames_dialog, Spgtk_print_frames_dialog, 0, 1, "",
       doc: /* Pop up a print dialog to print the current contents of FRAMES.
FRAMES should be nil (the selected frame), a frame, or a list of
frames (each of which corresponds to one page).  Each frame should be
visible.  */)
  (Lisp_Object frames)
{
  Lisp_Object rest, tmp;

  if (!CONSP (frames))
    frames = list1 (frames);

  tmp = Qnil;
  for (rest = frames; CONSP (rest); rest = XCDR (rest))
    {
      struct frame *f = decode_window_system_frame (XCAR (rest));
      Lisp_Object frame;

      XSETFRAME (frame, f);
      if (!FRAME_VISIBLE_P (f))
	error ("Frames to be printed must be visible");
      tmp = Fcons (frame, tmp);
    }
  frames = Fnreverse (tmp);

  /* Make sure the current matrices are up-to-date.  */
  redisplay_preserve_echo_area (32);

  block_input ();
  xg_print_frames_dialog (frames);
  unblock_input ();

  return Qnil;
}

static void
clean_up_dialog (void)
{
  pgtk_menu_set_in_use (false);
}

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename,
   Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  struct frame *f = SELECTED_FRAME ();
  char *fn;
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  specpdl_ref count = SPECPDL_INDEX ();
  char *cdef_file;

  check_window_system (f);

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");
  else
    pgtk_menu_set_in_use (true);

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect_void (clean_up_dialog);

  block_input ();

  if (STRINGP (default_filename))
    cdef_file = SSDATA (default_filename);
  else
    cdef_file = SSDATA (dir);

  fn = xg_get_file_name (f, SSDATA (prompt), cdef_file,
			 !NILP (mustmatch), !NILP (only_dir_p));

  if (fn)
    {
      file = build_string (fn);
      xfree (fn);
    }

  unblock_input ();

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    quit ();

  decoded_file = DECODE_FILE (file);

  return unbind_to (count, decoded_file);
}

DEFUN ("pgtk-backend-display-class", Fpgtk_backend_display_class, Spgtk_backend_display_class, 0, 1, "",
       doc: /* Return the name of the Gdk backend display class of TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy = dpyinfo->gdpy;
  const gchar *type_name = G_OBJECT_TYPE_NAME (G_OBJECT (gdpy));
  return build_string (type_name);
}

DEFUN ("x-select-font", Fx_select_font, Sx_select_font, 0, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object frame, Lisp_Object ignored)
{
  struct frame *f = decode_window_system_frame (frame);
  Lisp_Object font;
  Lisp_Object font_param;
  char *default_name = NULL;
  specpdl_ref count = SPECPDL_INDEX ();

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");
  else
    pgtk_menu_set_in_use (true);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect_void (clean_up_dialog);

  block_input ();

  XSETFONT (font, FRAME_FONT (f));
  font_param = Ffont_get (font, QCname);
  if (STRINGP (font_param))
    default_name = xlispstrdup (font_param);
  else
    {
      font_param = Fframe_parameter (frame, Qfont_parameter);
      if (STRINGP (font_param))
        default_name = xlispstrdup (font_param);
    }

  font = xg_get_font (f, default_name);
  xfree (default_name);

  unblock_input ();

  if (NILP (font))
    quit ();

  return unbind_to (count, font);
}

DEFUN ("x-gtk-debug", Fx_gtk_debug, Sx_gtk_debug, 1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object enable)
{
  gboolean enable_debug = !NILP (enable);

  block_input ();
  gtk_window_set_interactive_debugging (enable_debug);
  unblock_input ();

  return NILP (enable) ? Qnil : Qt;
}

static void
unwind_gerror_ptr (void* data)
{
  GError* error = *(GError**)data;
  if (error)
    g_error_free (error);
}

DEFUN ("x-gtk-launch-uri", Fx_gtk_launch_uri, Sx_gtk_launch_uri, 2, 2, 0,
       doc: /* Tell GTK to launch the default application to show given URI.

This function is only available on PGTK.  */)
  (Lisp_Object frame, Lisp_Object uri)
{
  CHECK_FRAME (frame);

  if (!FRAME_LIVE_P (XFRAME (frame)) ||
      !FRAME_PGTK_P (XFRAME (frame)) ||
      !FRAME_GTK_OUTER_WIDGET (XFRAME (frame)))
    error ("GTK URI launch not available for this frame");

  CHECK_STRING (uri);
  guint32 timestamp = gtk_get_current_event_time ();

  GError *err = NULL;
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_protect_ptr (unwind_gerror_ptr, &err);

  gtk_show_uri_on_window (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (XFRAME (frame))),
			  SSDATA (uri),
			  timestamp,
			  &err);

  if (err)
    error ("Failed to launch URI via GTK: %s", err->message);

  return unbind_to (count, Qnil);
}

void
syms_of_pgtkfns (void)
{
  DEFSYM (Qfont_parameter, "font-parameter");
  DEFSYM (Qfontsize, "fontsize");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qframe_title_format, "frame-title-format");
  DEFSYM (Qicon_title_format, "icon-title-format");
  DEFSYM (Qdark, "dark");
  DEFSYM (Qhide, "hide");
  DEFSYM (Qresize_mode, "resize-mode");

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_cursor_fore_pixel = Qnil;

  Fprovide (intern_c_string ("gtk"), Qnil);

  DEFVAR_LISP ("gtk-version-string", Vgtk_version_string,
	       doc: /* Version info for GTK+.  */);
  {
    char *ver = g_strdup_printf ("%d.%d.%d",
				 GTK_MAJOR_VERSION, GTK_MINOR_VERSION,
				 GTK_MICRO_VERSION);
    int len = strlen (ver);
    Vgtk_version_string = make_specified_string (ver, len, len, false);
    g_free (ver);
  }


  Fprovide (intern_c_string ("cairo"), Qnil);

  DEFVAR_LISP ("cairo-version-string", Vcairo_version_string,
	       doc: /* Version info for cairo.  */);
  {
    char *ver = g_strdup_printf ("%d.%d.%d",
				 CAIRO_VERSION_MAJOR, CAIRO_VERSION_MINOR,
				 CAIRO_VERSION_MICRO);
    int len = strlen (ver);
    Vcairo_version_string = make_specified_string (ver, len, len, false);
    g_free (ver);
  }

  defsubr (&Spgtk_set_resource);
  defsubr (&Sxw_display_color_p);	/* this and next called directly by C code */
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Spgtk_font_name);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Spgtk_display_monitor_attributes_list);
  defsubr (&Spgtk_frame_geometry);
  defsubr (&Spgtk_frame_edges);
  defsubr (&Spgtk_frame_restack);
  defsubr (&Spgtk_set_mouse_absolute_pixel_position);
  defsubr (&Spgtk_mouse_absolute_pixel_position);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_save_under);
  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_gtk_debug);
  defsubr (&Sx_gtk_launch_uri);

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);

  defsubr (&Sx_export_frames);
  defsubr (&Spgtk_page_setup_dialog);
  defsubr (&Spgtk_get_page_setup);
  defsubr (&Spgtk_print_frames_dialog);
  defsubr (&Spgtk_backend_display_class);

  defsubr (&Spgtk_set_monitor_scale_factor);

  defsubr (&Sx_file_dialog);
  defsubr (&Sx_select_font);

  monitor_scale_factor_alist = Qnil;
  staticpro (&monitor_scale_factor_alist);

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

  /* This is not ifdef:ed, so other builds than GTK can customize it.  */
  DEFVAR_BOOL ("x-gtk-use-old-file-dialog", x_gtk_use_old_file_dialog,
	       doc: /* SKIP: real doc in xfns.c.  */);
  x_gtk_use_old_file_dialog = false;

  DEFVAR_BOOL ("x-gtk-show-hidden-files", x_gtk_show_hidden_files,
	       doc: /* SKIP: real doc in xfns.c.  */);
  x_gtk_show_hidden_files = false;

  DEFVAR_BOOL ("x-gtk-file-dialog-help-text", x_gtk_file_dialog_help_text,
	       doc: /* SKIP: real doc in xfns.c.  */);
  x_gtk_file_dialog_help_text = true;

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
	       doc: /* SKIP: real doc in xfns.c.  */);
  Vx_max_tooltip_size = Qnil;

  DEFSYM (Qmono, "mono");
  DEFSYM (Qassq_delete_all, "assq-delete-all");

  DEFSYM (Qpdf, "pdf");

  DEFSYM (Qorientation, "orientation");
  DEFSYM (Qtop_margin, "top-margin");
  DEFSYM (Qbottom_margin, "bottom-margin");
  DEFSYM (Qportrait, "portrait");
  DEFSYM (Qlandscape, "landscape");
  DEFSYM (Qreverse_portrait, "reverse-portrait");
  DEFSYM (Qreverse_landscape, "reverse-landscape");
  DEFSYM (Qtrue_color, "true-color");
  DEFSYM (Qcolor, "color");
  DEFSYM (Qrun_at_time, "run-at-time");
  DEFSYM (Qx_hide_tip, "x-hide-tip");
}
