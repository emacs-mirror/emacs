/* Functions for the pure Gtk+-3.

Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2017 Free Software
Foundation, Inc.

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

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
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


#ifdef HAVE_PGTK

//static EmacsTooltip *pgtk_tooltip = nil;

/* Static variables to handle applescript execution.  */
static Lisp_Object as_script, *as_result;
static int as_status;

static ptrdiff_t image_cache_refcount;

static struct pgtk_display_info *pgtk_display_info_for_name (Lisp_Object);
static void pgtk_set_name_as_filename (struct frame *);

static const char *pgtk_app_name = "Emacs";

/* ==========================================================================

    Internal utility functions

   ========================================================================== */

static struct pgtk_display_info *
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

/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */
static struct pgtk_display_info *
pgtk_display_info_for_name (Lisp_Object name)
{
  struct pgtk_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element), name)))
      return dpyinfo;

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = pgtk_term_init (name, SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to display server %s", SDATA (name));

  XSETFASTINT (Vwindow_system_version, 11);

  return dpyinfo;
}

/* ==========================================================================

    Frame parameter setters

   ========================================================================== */


static void
x_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  Emacs_Color col;

  /* Must block_input, because pgtk_lisp_to_color does block/unblock_input
     which means that col may be deallocated in its unblock_input if there
     is user input, unless we also block_input.  */
  block_input ();
  if (pgtk_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qforeground_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  FRAME_X_OUTPUT(f)->foreground_color = col.pixel;

  FRAME_FOREGROUND_PIXEL (f) = col.pixel;

  if (FRAME_GTK_WIDGET (f))
    {
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      /*recompute_basic_faces (f); */
      if (FRAME_VISIBLE_P (f))
        SET_FRAME_GARBAGED (f);
    }
  unblock_input ();
}


static void
x_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  Emacs_Color col;

  block_input ();
  if (pgtk_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qbackground_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  /* clear the frame */
  if (FRAME_VISIBLE_P (f))
    pgtk_clear_frame (f);

  PGTK_TRACE("x_set_background_color: col.pixel=%08lx.", col.pixel);
  FRAME_X_OUTPUT(f)->background_color = col.pixel;
  FRAME_BACKGROUND_PIXEL (f) =
    ARGB_TO_ULONG ((unsigned int)(0xff), (unsigned int)(col.red>>8), (unsigned int)(col.green>>8), (unsigned int)(col.blue>>8));

  xg_set_background_color(f, col.pixel);
  update_face_from_frame_parameter (f, Qbackground_color, arg);

  PGTK_TRACE("visible_p=%d.", FRAME_VISIBLE_P(f));
  if (FRAME_VISIBLE_P (f))
    SET_FRAME_GARBAGED (f);

  unblock_input ();
}


static void
x_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long fore_pixel, pixel;
  struct pgtk_output *x = f->output_data.pgtk;
  Emacs_Color col;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      if (pgtk_lisp_to_color(Vx_cursor_fore_pixel, &col))
	signal_error ("Undefined color", Vx_cursor_fore_pixel);
      fore_pixel = col.pixel;
    }
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  if (pgtk_lisp_to_color(arg, &col))
    signal_error ("Undefined color", arg);
  pixel = col.pixel;

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
x_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  GtkWidget *widget = FRAME_GTK_OUTER_WIDGET(f);
  PGTK_TRACE ("x_set_icon_name");

  /* see if it's changed */
  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
        return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  fset_icon_name (f, arg);

  if (NILP (arg))
    {
      if (!NILP (f->title))
        arg = f->title;
      else
        /* Explicit name and no icon-name -> explicit_name.  */
        if (f->explicit_name)
          arg = f->name;
        else
          {
            /* No explicit name and no icon-name ->
               name has to be rebuild from icon_title_format.  */
            windows_or_buffers_changed = 67;
            return;
          }
    }

  gtk_window_set_icon_name(GTK_WINDOW(widget), SSDATA(arg));
}

static void
pgtk_set_name_internal (struct frame *f, Lisp_Object name)
{
  Lisp_Object encoded_name, encoded_icon_name;
  GtkWidget *widget = FRAME_GTK_OUTER_WIDGET (f);

  encoded_name = ENCODE_UTF_8 (name);
  gtk_window_set_title(GTK_WINDOW(widget), SSDATA (encoded_name));

  if (!STRINGP (f->icon_name))
    encoded_icon_name = encoded_name;
  else
    encoded_icon_name = ENCODE_UTF_8 (f->icon_name);
  gtk_window_set_icon_name(GTK_WINDOW(widget), SSDATA (encoded_icon_name));
}

static void
pgtk_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  PGTK_TRACE ("pgtk_set_name");

  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
         update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
        update_mode_lines = 12;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  if (NILP (name))
    name = build_string (pgtk_app_name);
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* Title overrides explicit name.  */
  if (! NILP (f->title))
    name = f->title;

  pgtk_set_name_internal (f, name);
}


/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
x_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  PGTK_TRACE ("x_explicitly_set_name");
  pgtk_set_name (f, arg, 1);
}


/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
pgtk_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  PGTK_TRACE ("x_implicitly_set_name");

  Lisp_Object frame_title = buffer_local_value
    (Qframe_title_format, XWINDOW (f->selected_window)->contents);
  Lisp_Object icon_title = buffer_local_value
    (Qicon_title_format, XWINDOW (f->selected_window)->contents);

  if (FRAME_PGTK_P (f) && ((FRAME_ICONIFIED_P (f) && EQ (icon_title, Qt))
                         || EQ (frame_title, Qt)))
    pgtk_set_name_as_filename (f);
  else
    pgtk_set_name (f, arg, 0);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
x_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  PGTK_TRACE ("x_set_title");
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
pgtk_set_name_as_filename (struct frame *f)
{
  GtkWidget *widget;
  Lisp_Object name, filename;
  Lisp_Object buf = XWINDOW (f->selected_window)->contents;
  const char *title;
  Lisp_Object encoded_name, encoded_filename;
  const char *str;
  PGTK_TRACE ("pgtk_set_name_as_filename");

  if (f->explicit_name || ! NILP (f->title))
    return;

  block_input ();
  filename = BVAR (XBUFFER (buf), filename);
  name = BVAR (XBUFFER (buf), name);

  if (NILP (name))
    {
      if (! NILP (filename))
        name = Ffile_name_nondirectory (filename);
      else
        name = build_string (pgtk_app_name);
    }

  encoded_name = ENCODE_UTF_8 (name);

  widget = FRAME_GTK_OUTER_WIDGET (f);

  title = FRAME_ICONIFIED_P (f) ? gtk_window_get_icon_name(GTK_WINDOW(widget))
				: gtk_window_get_title(GTK_WINDOW(widget));

  if (title && (! strcmp (title, SSDATA (encoded_name))))
    {
      unblock_input ();
      return;
    }

  str = SSDATA (encoded_name);
  if (str == NULL) str = "Bad coding";

  if (FRAME_ICONIFIED_P (f))
    gtk_window_set_icon_name(GTK_WINDOW(widget), str);
  else
    {
      const char *fstr;

      if (! NILP (filename))
        {
          encoded_filename = ENCODE_UTF_8 (filename);

          fstr = SSDATA (encoded_filename);
          if (fstr == NULL) fstr = "";
        }
      else
        fstr = "";

      gtk_window_set_title(GTK_WINDOW(widget), str);
      fset_name (f, name);
    }

  unblock_input ();
}


void
pgtk_set_doc_edited (void)
{
}


static void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
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
    }
}


/* toolbar support */
static void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
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

  x_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));

}


static void
x_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int old_width = FRAME_INTERNAL_BORDER_WIDTH (f);

  CHECK_TYPE_RANGED_INTEGER (int, arg);
  f->internal_border_width = XFIXNUM (arg);
  if (FRAME_INTERNAL_BORDER_WIDTH (f) < 0)
    f->internal_border_width = 0;

  if (FRAME_INTERNAL_BORDER_WIDTH (f) == old_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    adjust_frame_size (f, -1, -1, 3, 0, Qinternal_border_width);

  SET_FRAME_GARBAGED (f);
}


static void
x_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  /* This does not work if on Wayland, or if icon is defined in emacs.desktop
   * even if on X11.
   */
  GdkPixbuf *pixbuf;
  if (NILP (arg) || EQ (arg, Qt))
    pixbuf = NULL;
  else {
    GError *err = NULL;
    CHECK_STRING (arg);
    pixbuf = gdk_pixbuf_new_from_file (SSDATA (arg), &err);
    if (pixbuf == NULL) {
      Lisp_Object msg = build_string (err->message);
      g_error_free (err);
      error ("%s", SSDATA (msg));
    }
  }

  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), pixbuf);
  if (pixbuf != NULL)
    g_object_unref (pixbuf);
}

/* This is the same as the xfns.c definition.  */
static void
x_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

/* called to set mouse pointer color, but all other terms use it to
   initialize pointer types (and don't set the color ;) */
static void
x_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
}


static void
x_icon (struct frame *f, Lisp_Object parms)
/* --------------------------------------------------------------------------
   Strangely-named function to set icon position parameters in frame.
   This is irrelevant under macOS, but might be needed under GNUstep,
   depending on the window manager used.  Note, this is not a standard
   frame parameter-setter; it is called directly from x-create-frame.
   -------------------------------------------------------------------------- */
{
#if 0
  Lisp_Object icon_x, icon_y;
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (Qnil);

  FRAME_X_OUTPUT(f)->icon_top = -1;
  FRAME_X_OUTPUT(f)->icon_left = -1;

  /* Set the position of the icon.  */
  icon_x = gui_display_get_arg (dpyinfo, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = gui_display_get_arg (dpyinfo, parms, Qicon_top, 0, 0,  RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
      FRAME_X_OUTPUT(f)->icon_top = XFIXNUM (icon_y);
      FRAME_X_OUTPUT(f)->icon_left = XFIXNUM (icon_x);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
#endif
}

/**
 * x_set_undecorated:
 *
 * Set frame F's `undecorated' parameter.  If non-nil, F's window-system
 * window is drawn without decorations, title, minimize/maximize boxes
 * and external borders.  This usually means that the window cannot be
 * dragged, resized, iconified, maximized or deleted with the mouse.  If
 * nil, draw the frame with all the elements listed above unless these
 * have been suspended via window manager settings.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      FRAME_UNDECORATED (f) = NILP (new_value) ? false : true;
      xg_set_undecorated (f, new_value);
    }
}

/**
 * x_set_skip_taskbar:
 *
 * Set frame F's `skip-taskbar' parameter.  If non-nil, this should
 * remove F's icon from the taskbar associated with the display of F's
 * window-system window and inhibit switching to F's window via
 * <Alt>-<TAB>.  If nil, lift these restrictions.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_skip_taskbar (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      xg_set_skip_taskbar (f, new_value);
      FRAME_SKIP_TASKBAR (f) = !NILP (new_value);
    }
}

/**
 * x_set_override_redirect:
 *
 * Set frame F's `override_redirect' parameter which, if non-nil, hints
 * that the window manager doesn't want to deal with F.  Usually, such
 * frames have no decorations and always appear on top of all frames.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_override_redirect (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
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

/* Note: see frame.c for template, also where generic functions are impl */
frame_parm_handler pgtk_frame_parm_handlers[] =
{
  gui_set_autoraise, /* generic OK */
  gui_set_autolower, /* generic OK */
  x_set_background_color,
  0, /* x_set_border_color,  may be impossible under Nextstep */
  0, /* x_set_border_width,  may be impossible under Nextstep */
  x_set_cursor_color,
  x_set_cursor_type,
  gui_set_font, /* generic OK */
  x_set_foreground_color,
  x_set_icon_name,
  x_set_icon_type,
  x_set_internal_border_width, /* generic OK */
  gui_set_right_divider_width,
  gui_set_bottom_divider_width,
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  gui_set_scroll_bar_width, /* generic OK */
  gui_set_scroll_bar_height, /* generic OK */
  x_set_title,
  gui_set_unsplittable, /* generic OK */
  gui_set_vertical_scroll_bars, /* generic OK */
  gui_set_horizontal_scroll_bars, /* generic OK */
  gui_set_visibility, /* generic OK */
  x_set_tool_bar_lines,
  0, /* x_set_scroll_bar_foreground, will ignore (not possible on NS) */
  0, /* x_set_scroll_bar_background,  will ignore (not possible on NS) */
  gui_set_screen_gamma, /* generic OK */
  gui_set_line_spacing, /* generic OK, sets f->extra_line_spacing to int */
  gui_set_left_fringe, /* generic OK */
  gui_set_right_fringe, /* generic OK */
  0, /* x_set_wait_for_wm, will ignore */
  gui_set_fullscreen, /* generic OK */
  gui_set_font_backend, /* generic OK */
  gui_set_alpha,
  0, /* x_set_sticky */
  0, /* x_set_tool_bar_position */
  0, /* x_set_inhibit_double_buffering */
  x_set_undecorated,
  0, /* x_set_parent_frame, */
  x_set_skip_taskbar,
  x_set_no_focus_on_map,
  x_set_no_accept_focus,
  x_set_z_group,
  x_set_override_redirect,
  gui_set_no_special_glyphs,
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

      x_free_frame_resources (f);
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

static void
x_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
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
      const char *system_font = xsettings_get_system_font ();
      if (system_font)
	font = font_open_by_name (f, build_unibyte_string (system_font));
    }

  if (NILP (font))
      font = !NILP (font_param) ? font_param
      : gui_display_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);

  if (! FONTP (font) && ! STRINGP (font))
    {
      const char *names[]
	= {
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
	    NULL };
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
  else if (!NILP (font_param))
    {
      /* Remember the explicit font parameter, so we can re-apply it after
	 we've applied the `default' face settings.  */
      AUTO_FRAME_ARG (arg, Qfont_parameter, font_param);
      gui_set_frame_parameters (f, arg);
    }

  /* This call will make X resources override any system font setting.  */
  gui_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}

/* ==========================================================================

    Lisp definitions

   ========================================================================== */

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new X window, which is called a "frame" in Emacs terms.
Return an Emacs frame object.  PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use, then
`default-minibuffer-frame' must be a frame whose minibuffer can be
shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
  (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only = false;
  bool undecorated = false, override_redirect = false;
  long window_prompting = 0;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct pgtk_display_info *dpyinfo = NULL;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;
  int x_width = 0, x_height = 0;

  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0, RES_TYPE_NUMBER);
  if (EQ (display, Qunbound))
    display = gui_display_get_arg (dpyinfo, parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_pgtk_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = gui_display_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  frame = Qnil;
  tem = gui_display_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
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

  parent_frame = gui_display_get_arg (dpyinfo, parms, Qparent_frame, NULL, NULL,
			    RES_TYPE_SYMBOL);
  /* Accept parent-frame iff parent-id was not specified.  */
  if (!NILP (parent)
      || EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame))
      || !FRAME_PGTK_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo, parms, Qundecorated, NULL, NULL,
			       RES_TYPE_BOOLEAN)))
      && !(EQ (tem, Qunbound)))
    undecorated = true;

  FRAME_UNDECORATED (f) = undecorated;
  store_frame_param (f, Qundecorated, undecorated ? Qt : Qnil);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo, parms, Qoverride_redirect, NULL, NULL,
			       RES_TYPE_BOOLEAN)))
      && !(EQ (tem, Qunbound)))
    override_redirect = true;

  FRAME_OVERRIDE_REDIRECT (f) = override_redirect;
  store_frame_param (f, Qoverride_redirect, override_redirect ? Qt : Qnil);

  XSETFRAME (frame, f);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_pgtk;
  FRAME_X_OUTPUT(f) = xzalloc (sizeof *FRAME_X_OUTPUT(f));
#if 0
  FRAME_X_OUTPUT(f)->icon_bitmap = -1;
#endif
  FRAME_FONTSET (f) = -1;
#if 0
  FRAME_X_OUTPUT(f)->scroll_bar_foreground_pixel = -1;
  FRAME_X_OUTPUT(f)->scroll_bar_background_pixel = -1;
#endif
  FRAME_X_OUTPUT(f)->white_relief.pixel = -1;
  FRAME_X_OUTPUT(f)->black_relief.pixel = -1;

  fset_icon_name (f,
		  gui_display_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
			     RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
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
    FRAME_X_OUTPUT(f)->cursor_color = -1;
    FRAME_X_OUTPUT(f)->cursor_foreground_color = -1;
#if 0
    FRAME_X_OUTPUT(f)->border_pixel = -1;
#endif
    FRAME_X_OUTPUT(f)->mouse_color = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT(f)->cursor_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_X_OUTPUT(f)->cursor_foreground_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
#if 0
    FRAME_X_OUTPUT(f)->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
#endif
    FRAME_X_OUTPUT(f)->mouse_color
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Specify the parent under which to make this X window.  */
  if (!NILP (parent))
    {
      FRAME_X_OUTPUT(f)->parent_desc = (Window) XFIXNAT (parent);
      FRAME_X_OUTPUT(f)->explicit_parent = true;
    }
  else
    {
      FRAME_X_OUTPUT(f)->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
      FRAME_X_OUTPUT(f)->explicit_parent = false;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
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

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
#if 0
#ifdef GLYPH_DEBUG
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#endif

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
		       "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  x_default_font_parameter (f, parms);
  if (!FRAME_FONT (f))
    {
      delete_frame (frame, Qnoelisp);
      error ("Invalid frame font");
    }

  /* Frame contents get displaced if an embedded X window has a border.  */
#if 0
  if (! FRAME_X_EMBEDDED_P (f))
#endif
    gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
			 "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 1 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
			 "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  gui_default_parameter (f, parms, Qinternal_border_width,
		       make_fixnum (0),
		       "internalBorderWidth", "internalBorderWidth",
		       RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qvertical_scroll_bars,
		       Qright,
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
  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_foreground,
					"scrollBarForeground",
					"ScrollBarForeground", true);
  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_background,
					"scrollBarBackground",
					"ScrollBarBackground", false);
#endif

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  /* We have to call adjust_frame_size here since otherwise
     x_set_tool_bar_lines will already work with the character sizes
     installed by init_frame_faces while the frame's pixel size is still
     calculated from a character size of 1 and we subsequently hit the
     (height >= 0) assertion in window_box_height.

     The non-pixelwise code apparently worked around this because it
     had one frame line vs one toolbar line which left us with a zero
     root window height which was obviously wrong as well ...

     Also process `min-width' and `min-height' parameters right here
     because `frame-windows-min-size' needs them.  */
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_width, NULL, NULL, RES_TYPE_NUMBER);
  if (NUMBERP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_height, NULL, NULL, RES_TYPE_NUMBER);
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
  window_prompting = gui_figure_window_size (f, parms, true, &x_width, &x_height);

  tem = gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

#if 0
  x_icon_verify (f, parms);
#endif

  /* Create the X widget or window.  */
  // x_window (f);
  xg_create_frame_widgets (f);
  pgtk_set_event_handler(f);


#define INSTALL_CURSOR(FIELD, NAME) \
  FRAME_X_OUTPUT(f)->FIELD = gdk_cursor_new_for_display(FRAME_X_DISPLAY(f), GDK_ ## NAME)

  INSTALL_CURSOR(text_cursor, XTERM);
  INSTALL_CURSOR(nontext_cursor, LEFT_PTR);
  INSTALL_CURSOR(modeline_cursor, XTERM);
  INSTALL_CURSOR(hand_cursor, HAND2);
  INSTALL_CURSOR(hourglass_cursor, WATCH);
  INSTALL_CURSOR(horizontal_drag_cursor, SB_H_DOUBLE_ARROW);
  INSTALL_CURSOR(vertical_drag_cursor, SB_V_DOUBLE_ARROW);
  INSTALL_CURSOR(left_edge_cursor, LEFT_SIDE);
  INSTALL_CURSOR(right_edge_cursor, RIGHT_SIDE);
  INSTALL_CURSOR(top_edge_cursor, TOP_SIDE);
  INSTALL_CURSOR(bottom_edge_cursor, BOTTOM_SIDE);
  INSTALL_CURSOR(top_left_corner_cursor, TOP_LEFT_CORNER);
  INSTALL_CURSOR(top_right_corner_cursor, TOP_RIGHT_CORNER);
  INSTALL_CURSOR(bottom_right_corner_cursor, BOTTOM_RIGHT_CORNER);
  INSTALL_CURSOR(bottom_left_corner_cursor, BOTTOM_LEFT_CORNER);

#undef INSTALL_CURSOR

  x_icon (f, parms);
#if 0
  x_make_gc (f);
#endif

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
		       "scrollBarWidth", "ScrollBarWidth",
		       RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qscroll_bar_height, Qnil,
		       "scrollBarHeight", "ScrollBarHeight",
		       RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qalpha, Qnil,
		       "alpha", "Alpha", RES_TYPE_NUMBER);

#if 0
  if (!NILP (parent_frame))
    {
      struct frame *p = XFRAME (parent_frame);

      block_input ();
      XReparentWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		       FRAME_X_WINDOW (p), f->left_pos, f->top_pos);
      unblock_input ();
    }
#endif

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

  if (x_width > 0)
    SET_FRAME_WIDTH (f, x_width);
  if (x_height > 0)
    SET_FRAME_HEIGHT (f, x_height);

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  block_input ();
  x_wm_set_size_hint (f, window_prompting, false);
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
  if (!FRAME_X_OUTPUT(f)->explicit_parent)
    {
      Lisp_Object visibility
	= gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL);

      if (EQ (visibility, Qicon))
	pgtk_iconify_frame (f);
      else
	{
	  if (EQ (visibility, Qunbound))
	    visibility = Qt;

	  if (!NILP (visibility))
	    pgtk_make_frame_visible (f);
	}

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

  FRAME_X_OUTPUT(f)->cr_surface_visible_bell = NULL;
  FRAME_X_OUTPUT(f)->atimer_visible_bell = NULL;

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

 return unbind_to (count, frame);
}


#if 0
static int
pgtk_window_is_ancestor (PGTKWindow *win, PGTKWindow *candidate)
/* Test whether CANDIDATE is an ancestor window of WIN. */
{
  if (candidate == NULL)
    return 0;
  else if (win == candidate)
    return 1;
  else
    return pgtk_window_is_ancestor(win, [candidate parentWindow]);
}
#endif

DEFUN ("pgtk-frame-list-z-order", Fpgtk_frame_list_z_order,
       Spgtk_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs' frames, in Z (stacking) order.
If TERMINAL is non-nil and specifies a live frame, return the child
frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).  */)
  (Lisp_Object terminal)
{
  Lisp_Object frames = Qnil;
#if 0
  PGTKWindow *parent = nil;

  if (FRAMEP (terminal) && FRAME_LIVE_P (XFRAME (terminal)))
    parent = [FRAME_PGTK_VIEW (XFRAME (terminal)) window];

  for (PGTKWindow *win in [[NSApp orderedWindows] reverseObjectEnumerator])
    {
      Lisp_Object frame;

      /* Check against [win parentWindow] so that it doesn't match itself. */
      if (parent == nil || pgtk_window_is_ancestor (parent, [win parentWindow]))
        {
          XSETFRAME (frame, ((EmacsView *)[win delegate])->emacsframe);
          frames = Fcons(frame, frames);
        }
    }
#endif

  return frames;
}

DEFUN ("pgtk-frame-restack", Fpgtk_frame_restack, Spgtk_frame_restack, 2, 3, 0,
       doc: /* Restack FRAME1 below FRAME2.
This means that if both frames are visible and the display areas of
these frames overlap, FRAME2 (partially) obscures FRAME1.  If optional
third argument ABOVE is non-nil, restack FRAME1 above FRAME2.  This
means that if both frames are visible and the display areas of these
frames overlap, FRAME1 (partially) obscures FRAME2.

Some window managers may refuse to restack windows.  */)
     (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object above)
{
  struct frame *f1 = decode_live_frame (frame1);
  struct frame *f2 = decode_live_frame (frame2);

  if (FRAME_PGTK_VIEW (f1) && FRAME_PGTK_VIEW (f2))
    {
#if 0
      PGTKWindow *window = [FRAME_PGTK_VIEW (f1) window];
      NSInteger window2 = [[FRAME_PGTK_VIEW (f2) window] windowNumber];
      PGTKWindowOrderingMode flag = NILP (above) ? PGTKWindowBelow : PGTKWindowAbove;

      [window orderWindow: flag
               relativeTo: window2];

#endif
      return Qt;
    }
  else
    {
      error ("Cannot restack frames");
      return Qnil;
    }
}

const char *
pgtk_get_defaults_value (const char *key)
{
  return NULL;
}

DEFUN ("pgtk-set-resource", Fpgtk_set_resource, Spgtk_set_resource, 3, 3, 0,
       doc: /* Set property NAME of OWNER to VALUE, from the defaults database.
If OWNER is nil, Emacs is assumed.
If VALUE is nil, the default is removed.  */)
     (Lisp_Object owner, Lisp_Object name, Lisp_Object value)
{
  check_window_system (NULL);
  if (NILP (owner))
    owner = build_string (pgtk_app_name);
  CHECK_STRING (name);

  return Qnil;
}


DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* This function is a no-op.  It is only present for completeness.  */)
     (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  /* This function has no real equivalent under PGTK.  Return nil to
     indicate this. */
  return Qnil;
}


DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Return the "vendor ID" string of the display server TERMINAL.
\(Labeling every distributor as a "vendor" embodies the false assumption
that operating systems cannot be developed and distributed noncommercially.)
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qnil;
}


DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Return the version numbers of the server of display TERMINAL.
The value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the distributor-specific release
number.  See also the function `x-server-vendor'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  /*NOTE: it is unclear what would best correspond with "protocol";
          we return 10.3, meaning Panther, since this is roughly the
          level that GNUstep's APIs correspond to.
          The last number is where we distinguish between the Apple
          and GNUstep implementations ("distributor-specific release
          number") and give int'ized versions of major.minor. */
  return list3i (0, 0, 0);
}


DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the display server TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

Note: "screen" here is not in X11's.  For the number of physical monitors,
 use `(length \(display-monitor-attributes-list TERMINAL))' instead.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return make_fixnum (1);
}


DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the height in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy = dpyinfo->gdpy;
  GdkMonitor *gmon = gdk_display_get_monitor_at_point(gdpy, 0, 0);
  return make_fixnum (gdk_monitor_get_height_mm(gmon));
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the width in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy = dpyinfo->gdpy;
  GdkMonitor *gmon = gdk_display_get_monitor_at_point(gdpy, 0, 0);
  return make_fixnum (gdk_monitor_get_width_mm(gmon));
}


DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Return an indication of whether the the display TERMINAL does backing store.
The value may be `buffered', `retained', or `non-retained'.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qnil;
}


DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Return the visual class of the the display TERMINAL.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On PGTK, always return true-color.  */)
  (Lisp_Object terminal)
{
  return intern ("true-color");
}


DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Return t if TERMINAL supports the save-under feature.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qnil;
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to a display server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.  */)
     (Lisp_Object display, Lisp_Object resource_string, Lisp_Object must_succeed)
{
  struct pgtk_display_info *dpyinfo;

  if (NILP(display))
    display = build_string("");

  CHECK_STRING (display);

  nxatoms_of_pgtkselect ();
  dpyinfo = pgtk_term_init (display, SSDATA(Vx_resource_name));
  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
        fatal ("Display on %s not responding.\n",
               SSDATA (display));
      else
        error ("Display on %s not responding.\n",
               SSDATA (display));
    }

  return Qnil;
}


DEFUN ("x-close-connection", Fx_close_connection, Sx_close_connection,
       1, 1, 0,
       doc: /* Close the connection to TERMINAL's display server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame's
terminal.  */)
     (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  pgtk_delete_terminal (dpyinfo->terminal);

  return Qnil;
}


DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
     (void)
{
  Lisp_Object result = Qnil;
  struct pgtk_display_info *ndi;

  for (ndi = x_display_list; ndi; ndi = ndi->next)
    result = Fcons (XCAR (ndi->name_list_element), result);

  return result;
}


DEFUN ("pgtk-hide-others", Fpgtk_hide_others, Spgtk_hide_others,
       0, 0, 0,
       doc: /* Hides all applications other than Emacs.  */)
     (void)
{
  check_window_system (NULL);
  return Qnil;
}

DEFUN ("pgtk-hide-emacs", Fpgtk_hide_emacs, Spgtk_hide_emacs,
       1, 1, 0,
       doc: /* If ON is non-nil, the entire Emacs application is hidden.
Otherwise if Emacs is hidden, it is unhidden.
If ON is equal to `activate', Emacs is unhidden and becomes
the active application.  */)
     (Lisp_Object on)
{
  check_window_system (NULL);
  return Qnil;
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
  xfree(str);
  return name;
}

/* ==========================================================================

    Miscellaneous functions not called through hooks

   ========================================================================== */

/* called from frame.c */
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

/* terms impl this instead of x-get-resource directly */
const char *
pgtk_get_string_resource (XrmDatabase rdb, const char *name, const char *class)
{
  /* remove appname prefix; TODO: allow for !="Emacs" */
  const char *res, *toCheck = class + (!strncmp (class, "Emacs.", 6) ? 6 : 0);

  check_window_system (NULL);

  if (inhibit_x_resources)
    /* --quick was passed, so this is a no-op.  */
    return NULL;

  res = pgtk_get_defaults_value (toCheck);
  return (char *) (!res ? NULL
		   : !c_strncasecmp (res, "YES", 3) ? "true"
		   : !c_strncasecmp (res, "NO", 2) ? "false"
		   : res);
}


Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  Lisp_Object focus;

  if (!dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (focus, dpyinfo->x_focus_frame);
  return focus;
}

/* ==========================================================================

    Lisp definitions that, for whatever reason, we can't alias as 'ns-XXX'.

   ========================================================================== */


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see.  */)
     (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;
  return pgtk_lisp_to_color (color, &col) ? Qnil : Qt;
}


DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Internal function called by `color-values', which see.  */)
     (Lisp_Object color, Lisp_Object frame)
{
  Emacs_Color col;

  CHECK_STRING (color);

  block_input ();

  if (pgtk_lisp_to_color (color, &col))
    {
      unblock_input ();
      return Qnil;
    }

  unblock_input ();

  return list3i (col.red, col.green, col.blue);
}


DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Internal function called by `display-color-p', which see.  */)
     (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return Qt;
}


DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* Return t if the display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  return Qnil;
}


DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Return the width in pixels of the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel width for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);

  return make_fixnum (x_display_pixel_width (dpyinfo));
}


DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Return the height in pixels of the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel height for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);

  return make_fixnum (x_display_pixel_height (dpyinfo));
}

DEFUN ("pgtk-display-monitor-attributes-list",
       Fpgtk_display_monitor_attributes_list,
       Spgtk_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object terminal)
{
  struct terminal *term = decode_live_terminal (terminal);
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy = dpyinfo->gdpy;
  GdkMonitor **gmons;
  int i, n_monitors, primary_index;
  struct MonitorInfo *monitors;
  Lisp_Object monitor_frames = Qnil;
  Lisp_Object frame = Qnil, rest = Qnil;
  Lisp_Object rv = Qnil;

  if (term->type != output_pgtk)
    return Qnil;

  n_monitors = gdk_display_get_n_monitors(gdpy);
  if (n_monitors == 0)
    return Qnil;

  gmons = xmalloc(sizeof *gmons * n_monitors);
  for (i = 0; i < n_monitors; i++)
    gmons[i] = gdk_display_get_monitor(gdpy, i);

  monitors = xzalloc (n_monitors * sizeof *monitors);
  for (i = 0; i < n_monitors; i++) {
    struct MonitorInfo *mon = &monitors[i];
    GdkMonitor *gmon = gmons[i];
    if (gmon != NULL) {
      GdkRectangle geom;
      gdk_monitor_get_geometry(gmon, &geom);
      mon->geom.x = geom.x;
      mon->geom.y = geom.y;
      mon->geom.width = geom.width;
      mon->geom.height = geom.height;

      gdk_monitor_get_workarea(gmon, &geom);
      mon->work.x = geom.x;
      mon->work.y = geom.y;
      mon->work.width = geom.width;
      mon->work.height = geom.height;

      mon->mm_width = gdk_monitor_get_width_mm(gmon);
      mon->mm_height = gdk_monitor_get_height_mm(gmon);

      mon->name = xstrdup(gdk_monitor_get_model(gmon));
    }
  }

  monitor_frames = Fmake_vector (make_fixnum (n_monitors), Qnil);
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_PGTK_P (f))
	{
	  GtkWidget *widget = FRAME_GTK_WIDGET(f);
	  GdkMonitor *gmon = gdk_display_get_monitor_at_window(gdpy, gtk_widget_get_window(widget));

	  if (gmon != NULL) {
	    for (i = 0; i < n_monitors; i++) {
	      if (gmons[i] == gmon) {
		ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
		break;
	      }
	    }
	  }
	}
    }

  primary_index = -1;
  for (i = 0; i < n_monitors; i++) {
    if (gmons[i] != NULL && gdk_monitor_is_primary(gmons[i])) {
      primary_index = i;
      break;
    }
  }

  rv = make_monitor_attribute_list(monitors, n_monitors, primary_index, monitor_frames, "Gdk");

  free_monitors(monitors, n_monitors);
  xfree(gmons);

  return rv;
}


DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Return the number of bitplanes of the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_pgtk_display_info (terminal);
  return make_fixnum(32);
}


DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Returns the number of color cells of the display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_fixnum (1 << min (dpyinfo->n_planes, 24));
}

/***********************************************************************
				Tool tips
 ***********************************************************************/

/* The frame of a currently visible tooltip.  */

Lisp_Object tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

static Lisp_Object tip_timer;

/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx, Lisp_Object dy, int width, int height, int *root_x, int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int min_x, min_y, max_x, max_y = -1;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if ((!INTEGERP (left) && !INTEGERP (right))
      || (!INTEGERP (top) && !INTEGERP (bottom)))
    {
      Lisp_Object frame, attributes, monitor, geometry;
      GdkSeat *seat = gdk_display_get_default_seat(FRAME_DISPLAY_INFO(f)->gdpy);
      GdkDevice *dev = gdk_seat_get_pointer(seat);
      GdkScreen *scr;

      block_input ();
      gdk_device_get_position(dev, &scr, root_x, root_y);
      unblock_input ();

      XSETFRAME(frame, f);
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
  if ( max_y < 0 )
    {
      min_x = 0;
      min_y = 0;
      max_x = x_display_pixel_width (FRAME_DISPLAY_INFO (f));
      max_y = x_display_pixel_height (FRAME_DISPLAY_INFO (f));
    }

  if (INTEGERP (top))
    *root_y = XFIXNUM (top);
  else if (INTEGERP (bottom))
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

  if (INTEGERP (left))
    *root_x = XFIXNUM (left);
  else if (INTEGERP (right))
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


/* Hide tooltip.  Delete its frame if DELETE is true.  */
static Lisp_Object
x_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      call1 (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

  if (NILP (tip_frame)
      || (!delete && FRAMEP (tip_frame)
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    return Qnil;
  else
    {
      ptrdiff_t count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      {
	/* When using system tooltip, tip_frame is the Emacs frame on
	   which the tip is shown.  */
	struct frame *f = XFRAME (tip_frame);

	if (FRAME_LIVE_P (f) && xg_hide_tooltip (f))
	  {
	    tip_frame = Qnil;
	    was_open = Qt;
	  }
      }

      if (FRAMEP (tip_frame))
	{
	  if (delete)
	    {
	      delete_frame (tip_frame, Qnil);
	      tip_frame = Qnil;
	    }
	  else
	    pgtk_make_frame_invisible (XFRAME (tip_frame));

	  was_open = Qt;
	}
      else
	tip_frame = Qnil;

      return unbind_to (count, was_open);
    }
}

DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small X window displaying a string.

This is an internal function; Lisp code should call `tooltip-show'.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARMS contains a `left' parameter,
display the tooltip at that x-position.  If the list of frame parameters
PARMS contains no `left' but a `right' parameter, display the tooltip
right-adjusted at that x-position. Otherwise display it at the
x-position of the mouse, with offset DX added (default is 5 if DX isn't
specified).

Likewise for the y-position: If a `top' frame parameter is specified, it
determines the position of the upper edge of the tooltip window.  If a
`bottom' parameter but no `top' frame parameter is specified, it
determines the position of the lower edge of the tooltip window.
Otherwise display the tooltip window at the y-position of the mouse,
with offset DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms, Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
  struct frame *f;
  int root_x, root_y;
  int width, height;
  ptrdiff_t count = SPECPDL_INDEX ();

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);
  if (SCHARS (string) == 0)
    string = make_unibyte_string (" ", 1);

  f = decode_window_system_frame (frame);
  if (NILP (timeout))
    timeout = make_fixnum (5);
  else
    CHECK_FIXNAT (timeout);

  if (NILP (dx))
    dx = make_fixnum (5);
  else
    CHECK_NUMBER (dx);

  if (NILP (dy))
    dy = make_fixnum (-10);
  else
    CHECK_NUMBER (dy);

  {
    bool ok;

    /* Hide a previous tip, if any.  */
    Fx_hide_tip ();

    block_input ();
    ok = xg_prepare_tooltip (f, string, &width, &height);
    if (ok)
      {
	compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);
	xg_show_tooltip (f, root_x, root_y);
	/* This is used in Fx_hide_tip.  */
	XSETFRAME (tip_frame, f);
      }
    unblock_input ();
    if (ok) goto start_timer;
  }

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
  (void)
{
  return x_hide_tip (!tooltip_reuse_hidden_frame);
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
  int native_left = f->left_pos + border;
  int native_top = f->top_pos + border + title_height;
  int native_right = f->left_pos + outer_width - border;
  int native_bottom = f->top_pos + outer_height - border;
  int internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  int tool_bar_height = FRAME_TOOLBAR_HEIGHT (f);
  int tool_bar_width = (tool_bar_height
			? outer_width - 2 * internal_border_width
			: 0);

  /* Construct list.  */
  if (EQ (attribute, Qouter_edges))
    return list4 (make_fixnum (f->left_pos), make_fixnum (f->top_pos),
		  make_fixnum (f->left_pos + outer_width),
		  make_fixnum (f->top_pos + outer_height));
  else if (EQ (attribute, Qnative_edges))
    return list4 (make_fixnum (native_left), make_fixnum (native_top),
		  make_fixnum (native_right), make_fixnum (native_bottom));
  else if (EQ (attribute, Qinner_edges))
    return list4 (make_fixnum (native_left + internal_border_width),
		  make_fixnum (native_top
			       + tool_bar_height
			       + internal_border_width),
		  make_fixnum (native_right - internal_border_width),
		  make_fixnum (native_bottom - internal_border_width));
  else
    return
      list (Fcons (Qouter_position,
		   Fcons (make_fixnum (f->left_pos),
			  make_fixnum (f->top_pos))),
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
				 ? type
				 : Qnative_edges));
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
  GtkWidget *widget = FRAME_GTK_OUTER_WIDGET(f);
  GdkWindow *window = gtk_widget_get_window(widget);
  GdkDisplay *gdpy = gdk_window_get_display(window);
  GdkScreen *gscr = gdk_window_get_screen(window);
  GdkSeat *seat = gdk_display_get_default_seat(gdpy);
  GdkDevice *device = gdk_seat_get_pointer(seat);

  PGTK_TRACE("pgtk-set-mouse-absolute-pixel-position:");
  gdk_device_warp(device, gscr, XFIXNUM(x), XFIXNUM(y));  /* No effect on wayland. */

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
  GtkWidget *widget = FRAME_GTK_OUTER_WIDGET(f);
  GdkWindow *window = gtk_widget_get_window(widget);
  GdkDisplay *gdpy = gdk_window_get_display(window);
  GdkScreen *gscr;
  GdkSeat *seat = gdk_display_get_default_seat(gdpy);
  GdkDevice *device = gdk_seat_get_pointer(seat);
  int x = 0, y = 0;

  gdk_device_get_position(device, &gscr, &x, &y);  /* can't get on wayland? */

  return Fcons(make_fixnum(x), make_fixnum(y));
}


DEFUN ("pgtk-page-setup-dialog", Fpgtk_page_setup_dialog, Spgtk_page_setup_dialog, 0, 0, 0,
       doc: /* Pop up a page setup dialog.
The current page setup can be obtained using `x-get-page-setup'.  */)
     (void)
{
  block_input ();
  xg_page_setup_dialog ();
  unblock_input ();

  return Qnil;
}

DEFUN ("pgtk-get-page-setup", Fpgtk_get_page_setup, Spgtk_get_page_setup, 0, 0, 0,
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
  int count;

  if (!CONSP (frames))
    frames = list1 (frames);

  tmp = Qnil;
  for (rest = frames; CONSP (rest); rest = XCDR (rest))
    {
      struct frame *f = decode_window_system_frame (XCAR (rest));
      Lisp_Object frame;

      XSETFRAME (frame, f);
      if (!FRAME_VISIBLE_P (f))
	error ("Frames to be printed must be visible.");
      tmp = Fcons (frame, tmp);
    }
  frames = Fnreverse (tmp);

  /* Make sure the current matrices are up-to-date.  */
  count = SPECPDL_INDEX ();
  specbind (Qredisplay_dont_pause, Qt);
  redisplay_preserve_echo_area (32);
  unbind_to (count, Qnil);

  block_input ();
  xg_print_frames_dialog (frames);
  unblock_input ();

  return Qnil;
}

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.  Select DEFAULT-FILENAME in the dialog's file
selection box, if specified.  If MUSTMATCH is non-nil, the returned file
or directory must exist.

This function is defined only on PGTK, NS, MS Windows, and X Windows with the
Motif or Gtk toolkits.  With the Motif toolkit, ONLY-DIR-P is ignored.
Otherwise, if ONLY-DIR-P is non-nil, the user can select only directories.
On MS Windows 7 and later, the file selection dialog "remembers" the last
directory where the user selected a file, and will open that directory
instead of DIR on subsequent invocations of this function with the same
value of DIR as in previous invocations; this is standard MS Windows behavior.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  struct frame *f = SELECTED_FRAME ();
  char *fn;
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  ptrdiff_t count = SPECPDL_INDEX ();
  char *cdef_file;

  check_window_system (f);

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
#if 0
  record_unwind_protect_void (clean_up_dialog);
#endif

  block_input ();

  if (STRINGP (default_filename))
    cdef_file = SSDATA (default_filename);
  else
    cdef_file = SSDATA (dir);

  fn = xg_get_file_name (f, SSDATA (prompt), cdef_file,
                         ! NILP (mustmatch),
                         ! NILP (only_dir_p));

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

DEFUN ("pgtk-backend-display-class", Fpgtk_backend_display_class, Spgtk_backend_display_class,
       0, 1, "",
       doc: /* Returns the name of the Gdk backend display class of the TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);
  GdkDisplay *gdpy = dpyinfo->gdpy;
  const gchar *type_name = G_OBJECT_TYPE_NAME(G_OBJECT(gdpy));
  return build_string(type_name);
}

/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */

void
syms_of_pgtkfns (void)
{
  DEFSYM (Qfont_parameter, "font-parameter");
  DEFSYM (Qfontsize, "fontsize");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qframe_title_format, "frame-title-format");
  DEFSYM (Qicon_title_format, "icon-title-format");
  DEFSYM (Qdark, "dark");

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
    doc: /* A string indicating the foreground color of the cursor box.  */);
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("pgtk-icon-type-alist", Vpgtk_icon_type_alist,
               doc: /* Alist of elements (REGEXP . IMAGE) for images of icons associated to frames.
If the title of a frame matches REGEXP, then IMAGE.tiff is
selected as the image of the icon representing the frame when it's
miniaturized.  If an element is t, then Emacs tries to select an icon
based on the filetype of the visited file.

The images have to be installed in a folder called English.lproj in the
Emacs folder.  You have to restart Emacs after installing new icons.

Example: Install an icon Gnus.tiff and execute the following code

  (setq pgtk-icon-type-alist
        (append pgtk-icon-type-alist
                \\='((\"^\\\\*\\\\(Group\\\\*$\\\\|Summary \\\\|Article\\\\*$\\\\)\"
                   . \"Gnus\"))))

When you miniaturize a Group, Summary or Article frame, Gnus.tiff will
be used as the image of the icon representing the frame.  */);
  Vpgtk_icon_type_alist = list1 (Qt);


  /* Provide x-toolkit also for GTK.  Internally GTK does not use Xt so it
     is not an X toolkit in that sense (USE_X_TOOLKIT is not defined).
     But for a user it is a toolkit for X, and indeed, configure
     accepts --with-x-toolkit=gtk.  */
  Fprovide (intern_c_string ("x-toolkit"), Qnil);
  Fprovide (intern_c_string ("gtk"), Qnil);
  Fprovide (intern_c_string ("move-toolbar"), Qnil);

  DEFVAR_LISP ("gtk-version-string", Vgtk_version_string,
               doc: /* Version info for GTK+.  */);
  {
    char *ver = g_strdup_printf("%d.%d.%d",
				GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
    int len = strlen(ver);
    Vgtk_version_string = make_pure_string (ver, len, len, false);
    g_free(ver);
  }


  Fprovide (intern_c_string ("cairo"), Qnil);

  DEFVAR_LISP ("cairo-version-string", Vcairo_version_string,
               doc: /* Version info for cairo.  */);
  {
    char *ver = g_strdup_printf("%d.%d.%d",
				CAIRO_VERSION_MAJOR, CAIRO_VERSION_MINOR,
				CAIRO_VERSION_MICRO);
    int len = strlen(ver);
    Vcairo_version_string = make_pure_string (ver, len, len, false);
    g_free(ver);
  }


  defsubr (&Spgtk_set_resource);
  defsubr (&Sxw_display_color_p); /* this and next called directly by C code */
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Spgtk_font_name);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Spgtk_display_monitor_attributes_list);
  defsubr (&Spgtk_frame_geometry);
  defsubr (&Spgtk_frame_edges);
  defsubr (&Spgtk_frame_list_z_order);
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

  defsubr (&Spgtk_hide_others);
  defsubr (&Spgtk_hide_emacs);

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);

  // defsubr (&Spgtk_export_frames);
  defsubr (&Spgtk_page_setup_dialog);
  defsubr (&Spgtk_get_page_setup);
  defsubr (&Spgtk_print_frames_dialog);
  defsubr (&Spgtk_backend_display_class);

  defsubr (&Sx_file_dialog);

  as_status = 0;
  as_script = Qnil;
  as_result = 0;

  tip_frame = Qnil;
  staticpro(&tip_frame);
  tip_timer = Qnil;
  staticpro(&tip_timer);

/* This is not ifdef:ed, so other builds than GTK can customize it.  */
  DEFVAR_BOOL ("x-gtk-use-old-file-dialog", x_gtk_use_old_file_dialog,
    doc: /* Non-nil means prompt with the old GTK file selection dialog.
If nil or if the file selection dialog is not available, the new GTK file
chooser is used instead.  To turn off all file dialogs set the
variable `use-file-dialog'.  */);
  x_gtk_use_old_file_dialog = false;

  DEFVAR_BOOL ("x-gtk-show-hidden-files", x_gtk_show_hidden_files,
    doc: /* If non-nil, the GTK file chooser will by default show hidden files.
Note that this is just the default, there is a toggle button on the file
chooser to show or not show hidden files on a case by case basis.  */);
  x_gtk_show_hidden_files = false;

  DEFVAR_BOOL ("x-gtk-file-dialog-help-text", x_gtk_file_dialog_help_text,
    doc: /* If non-nil, the GTK file chooser will show additional help text.
If more space for files in the file chooser dialog is wanted, set this to nil
to turn the additional text off.  */);
  x_gtk_file_dialog_help_text = true;

  DEFVAR_BOOL ("x-gtk-use-system-tooltips", x_gtk_use_system_tooltips,
    doc: /* If non-nil with a Gtk+ built Emacs, the Gtk+ tooltip is used.
Otherwise use Emacs own tooltip implementation.
When using Gtk+ tooltips, the tooltip face is not used.  */);
  x_gtk_use_system_tooltips = true;


  DEFSYM (Qmono, "mono");

  DEFSYM (Qpdf, "pdf");

  DEFSYM (Qorientation, "orientation");
  DEFSYM (Qtop_margin, "top-margin");
  DEFSYM (Qbottom_margin, "bottom-margin");
  DEFSYM (Qportrait, "portrait");
  DEFSYM (Qlandscape, "landscape");
  DEFSYM (Qreverse_portrait, "reverse-portrait");
  DEFSYM (Qreverse_landscape, "reverse-landscape");
}

#ifdef PGTK_DEBUG

#include <stdarg.h>
#include <time.h>
void pgtk_log(const char *file, int lineno, const char *fmt, ...)
{
  struct timespec ts;
  struct tm tm;
  char timestr[32];
  va_list ap;

  clock_gettime(CLOCK_REALTIME, &ts);

  localtime_r(&ts.tv_sec, &tm);
  strftime(timestr, sizeof timestr, "%H:%M:%S", &tm);

  fprintf(stderr, "%s.%06ld %.10s:%04d ", timestr, ts.tv_nsec / 1000, file, lineno);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
}

void pgtk_backtrace(const char *file, int lineno)
{
  Lisp_Object bt = make_uninit_vector(10);
  for (int i = 0; i < 10; i++)
    ASET(bt, i, Qnil);

  struct timespec ts;
  struct tm tm;
  char timestr[32];

  clock_gettime(CLOCK_REALTIME, &ts);

  localtime_r(&ts.tv_sec, &tm);
  strftime(timestr, sizeof timestr, "%H:%M:%S", &tm);

  fprintf(stderr, "%s.%06ld %.10s:%04d ********\n", timestr, ts.tv_nsec / 1000, file, lineno);

  get_backtrace(bt);
  for (int i = 0; i < 10; i++) {
    Lisp_Object stk = AREF(bt, i);
    if (!NILP(stk)) {
      Lisp_Object args[2] = { build_string("%S"), stk };
      Lisp_Object str = Fformat(2, args);
      fprintf(stderr, "%s %.10s:%04d %s\n", timestr, file, lineno, SSDATA(str));
    }
  }

  fprintf(stderr, "%s %.10s:%04d ********\n", timestr, file, lineno);
}

#endif

#endif
