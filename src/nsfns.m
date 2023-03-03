/* Functions for the NeXT/Open/GNUstep and macOS window system.

Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2023 Free Software
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

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
macOS/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include <math.h>
#include <c-strcase.h>

#include "lisp.h"
#include "blockinput.h"
#include "nsterm.h"
#include "window.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "fontset.h"
#include "font.h"

#ifdef NS_IMPL_COCOA
#include <IOKit/graphics/IOGraphicsLib.h>
#include "macfont.h"

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
#include <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#if MAC_OS_X_VERSION_MIN_REQUIRED >= 120000
#define IOMasterPort IOMainPort
#endif
#endif
#endif

#ifdef HAVE_NS

static EmacsTooltip *ns_tooltip = nil;

/* The frame of the currently visible tooltip, or nil if none.  */
static Lisp_Object tip_frame;

/* The X and Y deltas of the last call to `x-show-tip'.  */
static Lisp_Object tip_dx, tip_dy;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
static NSWindow *tip_window;

/* A timer that hides or deletes the currently visible tooltip when it
   fires.  */
static Lisp_Object tip_timer;

/* STRING argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_string;

/* Normalized FRAME argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_frame;

/* PARMS argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_parms;

/* Static variables to handle AppleScript execution.  */
static Lisp_Object as_script, *as_result;
static int as_status;

static ptrdiff_t image_cache_refcount;

static struct ns_display_info *ns_display_info_for_name (Lisp_Object);

/* ==========================================================================

    Internal utility functions

   ========================================================================== */

/* Let the user specify a Nextstep display with a Lisp object.
   OBJECT may be nil, a frame or a terminal object.
   nil stands for the selected frame--or, if that is not a Nextstep frame,
   the first Nextstep display on the list.  */

static struct ns_display_info *
check_ns_display_info (Lisp_Object object)
{
  struct ns_display_info *dpyinfo = NULL;

  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_NS_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
        error ("Nextstep windows are not in use or not initialized");
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type != output_ns)
        error ("Terminal %d is not a Nextstep display", t->id);

      dpyinfo = t->display_info.ns;
    }
  else if (STRINGP (object))
    dpyinfo = ns_display_info_for_name (object);
  else
    {
      struct frame *f = decode_window_system_frame (object);
      dpyinfo = FRAME_DISPLAY_INFO (f);
    }

  return dpyinfo;
}


static id
ns_get_window (Lisp_Object maybeFrame)
{
  id view =nil, window =nil;

  if (!FRAMEP (maybeFrame) || !FRAME_NS_P (XFRAME (maybeFrame)))
    maybeFrame = selected_frame; /* wrong_type_argument (Qframep, maybeFrame); */

  if (!NILP (maybeFrame))
    view = FRAME_NS_VIEW (XFRAME (maybeFrame));
  if (view) window =[view window];

  return window;
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */
static struct ns_display_info *
ns_display_info_for_name (Lisp_Object name)
{
  struct ns_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element), name)))
      return dpyinfo;

  error ("Emacs for Nextstep does not yet support multi-display");

  Fx_open_connection (name, Qnil, Qnil);
  dpyinfo = x_display_list;

  if (dpyinfo == 0)
    error ("Display on %s not responding.\n", SDATA (name));

  return dpyinfo;
}

static NSString *
ns_filename_from_panel (NSSavePanel *panel)
{
#ifdef NS_IMPL_COCOA
  NSURL *url = [panel URL];
  NSString *str = [url path];
  return str;
#else
  return [panel filename];
#endif
}

static NSString *
ns_directory_from_panel (NSSavePanel *panel)
{
#ifdef NS_IMPL_COCOA
  NSURL *url = [panel directoryURL];
  NSString *str = [url path];
  return str;
#else
  return [panel directory];
#endif
}

#ifndef NS_IMPL_COCOA
static Lisp_Object
interpret_services_menu (NSMenu *menu, Lisp_Object prefix, Lisp_Object old)
/* --------------------------------------------------------------------------
   Turn the input menu (an NSMenu) into a lisp list for tracking on lisp side.
   -------------------------------------------------------------------------- */
{
  int i, count;
  NSMenuItem *item;
  const char *name;
  Lisp_Object nameStr;
  unsigned short key;
  NSString *keys;
  Lisp_Object res;

  count = [menu numberOfItems];
  for (i = 0; i<count; i++)
    {
      item = [menu itemAtIndex: i];
      name = [[item title] UTF8String];
      if (!name) continue;

      nameStr = build_string (name);

      if ([item hasSubmenu])
        {
          old = interpret_services_menu ([item submenu],
                                        Fcons (nameStr, prefix), old);
        }
      else
        {
          keys = [item keyEquivalent];
          if (keys && [keys length] )
            {
              key = [keys characterAtIndex: 0];
              res = make_fixnum (key|super_modifier);
            }
          else
            {
              res = Qundefined;
            }
          old = Fcons (Fcons (res,
                            Freverse (Fcons (nameStr,
                                           prefix))),
                    old);
        }
    }
  return old;
}
#endif


/* ==========================================================================

    Frame parameter setters

   ========================================================================== */


static void
ns_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSColor *col;

  /* Must block_input, because ns_lisp_to_color does block/unblock_input
     which means that col may be deallocated in its unblock_input if there
     is user input, unless we also block_input.  */
  block_input ();
  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qforeground_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  [col retain];
  [f->output_data.ns->foreground_color release];
  f->output_data.ns->foreground_color = col;

  FRAME_FOREGROUND_PIXEL (f) = [col unsignedLong];

  if (FRAME_NS_VIEW (f))
    {
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      /* recompute_basic_faces (f); */
      if (FRAME_VISIBLE_P (f))
        SET_FRAME_GARBAGED (f);
    }
  unblock_input ();
}


static void
ns_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct face *face;
  NSColor *col;
  NSView *view = FRAME_NS_VIEW (f);
  EmacsCGFloat alpha;

  block_input ();
  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qbackground_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  [col retain];
  [f->output_data.ns->background_color release];
  f->output_data.ns->background_color = col;

  FRAME_BACKGROUND_PIXEL (f) = [col unsignedLong];
  alpha = [col alphaComponent];

  if (view != nil)
    {
      [[view window] setBackgroundColor: col];

      if (alpha != (EmacsCGFloat) 1.0)
          [[view window] setOpaque: NO];
      else
          [[view window] setOpaque: YES];

      face = FRAME_DEFAULT_FACE (f);
      if (face)
        {
          col = [NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)];
          face->background = [[col colorWithAlphaComponent: alpha]
                               unsignedLong];

          update_face_from_frame_parameter (f, Qbackground_color, arg);
        }

      if (FRAME_VISIBLE_P (f))
        {
          SET_FRAME_GARBAGED (f);
          ns_clear_frame (f);
        }
    }
  unblock_input ();
}


static void
ns_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSColor *col;

  block_input ();
  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qcursor_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  [FRAME_CURSOR_COLOR (f) release];
  FRAME_CURSOR_COLOR (f) = [col retain];

  if (FRAME_VISIBLE_P (f))
    {
      gui_update_cursor (f, 0);
      gui_update_cursor (f, 1);
    }
  update_face_from_frame_parameter (f, Qcursor_color, arg);
  unblock_input ();
}


static void
ns_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE ("ns_set_icon_name");

  /* See if it's changed.  */
  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && BASE_EQ (Fstring_equal (oldval, arg), Qt))
        return;
    }
  else if (!STRINGP (oldval) && NILP (oldval) == NILP (arg))
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
            windows_or_buffers_changed = 62;
            return;
          }
    }

  /* Don't change the name if it's already NAME.  */
  if ([[view window] miniwindowTitle]
      && ([[[view window] miniwindowTitle]
             isEqualToString: [NSString stringWithLispString:arg]]))
    return;

  [[view window] setMiniwindowTitle:
        [NSString stringWithLispString:arg]];
}

static void
ns_set_name_internal (struct frame *f, Lisp_Object name)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSString *str = [NSString stringWithLispString: name];

  /* Don't change the name if it's already NAME.  */
  if (! [[[view window] title] isEqualToString: str])
    [[view window] setTitle: str];

  if (STRINGP (f->icon_name))
    str = [NSString stringWithLispString: f->icon_name];

  if ([[view window] miniwindowTitle]
      && ! [[[view window] miniwindowTitle] isEqualToString: str])
    [[view window] setMiniwindowTitle: str];

}

static void
ns_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  NSTRACE ("ns_set_name");

  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
         update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
        update_mode_lines = 21;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  if (NILP (name))
    name = [ns_app_name lispString];
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* Title overrides explicit name.  */
  if (! NILP (f->title))
    name = f->title;

  ns_set_name_internal (f, name);
}

static void
ns_set_represented_filename (struct frame *f)
{
  Lisp_Object filename;
  Lisp_Object buf = XWINDOW (f->selected_window)->contents;
  NSAutoreleasePool *pool;
  NSString *fstr;
  NSView *view = FRAME_NS_VIEW (f);

  NSTRACE ("ns_set_represented_filename");

  if (f->explicit_name || ! NILP (f->title))
    return;

  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  filename = BVAR (XBUFFER (buf), filename);

  if (! NILP (filename))
    {
      fstr = [NSString stringWithLispString:filename];
      if (fstr == nil) fstr = @"";
    }
  else
    fstr = @"";

#if defined (NS_IMPL_COCOA) && defined (MAC_OS_X_VERSION_10_7)
  /* Work around for Mach port leaks on macOS 10.15 (bug#38618).  */
  NSURL *fileURL = [NSURL fileURLWithPath:fstr isDirectory:NO];
  NSNumber *isUbiquitousItem = [NSNumber numberWithBool:YES];
  [fileURL getResourceValue:(id *)&isUbiquitousItem
                     forKey:NSURLIsUbiquitousItemKey
                      error:nil];
  if ([isUbiquitousItem boolValue])
    fstr = @"";
#endif

#ifdef NS_IMPL_COCOA
  /* Work around a bug observed on 10.3 and later where
     setTitleWithRepresentedFilename does not clear out previous state
     if given filename does not exist.  */
  if (! [[NSFileManager defaultManager] fileExistsAtPath: fstr])
    [[view window] setRepresentedFilename: @""];
#endif
  [[view window] setRepresentedFilename: fstr];

  [pool release];
  unblock_input ();
}


/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
ns_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE ("ns_explicitly_set_name");
  ns_set_name (f, arg, 1);
}


/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
ns_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE ("ns_implicitly_set_name");

  if (ns_use_proxy_icon)
    ns_set_represented_filename (f);

  ns_set_name (f, arg, 0);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
ns_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  NSTRACE ("ns_set_title");
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 22;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  ns_set_name_internal (f, name);
}

void
ns_set_doc_edited (void)
{
  NSAutoreleasePool *pool;
  Lisp_Object tail, frame;
  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  FOR_EACH_FRAME (tail, frame)
    {
      BOOL edited = NO;
      struct frame *f = XFRAME (frame);
      struct window *w;
      NSView *view;

      if (! FRAME_NS_P (f)) continue;
      w = XWINDOW (FRAME_SELECTED_WINDOW (f));
      view = FRAME_NS_VIEW (f);
      if (!MINI_WINDOW_P (w))
        edited = ! NILP (Fbuffer_modified_p (w->contents)) &&
          ! NILP (Fbuffer_file_name (w->contents));
      [[view window] setDocumentEdited: edited];
    }

  [pool release];
  unblock_input ();
}


static void
ns_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (TYPE_RANGED_FIXNUMP (int, value))
    nlines = XFIXNUM (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      /* Does for all frames, whereas we just want for one frame
	 [NSMenu setMenuBarVisible: YES]; */
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
        free_frame_menubar (f);
      /* [NSMenu setMenuBarVisible: NO]; */
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
    }
}

void
ns_change_tab_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TAB_BAR_HEIGHT (f);

  /* This differs from the tool bar code in that the tab bar height is
     not rounded up.  Otherwise, if redisplay_tab_bar decides to grow
     the tab bar by even 1 pixel, FRAME_TAB_BAR_LINES will be changed,
     leading to the tab bar height being incorrectly set upon the next
     call to x_set_font.  (bug#59285) */
  int lines = height / unit;
  if (lines == 0 && height != 0)
    lines = 1;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  /* Recalculate tab bar and frame text sizes.  */
  FRAME_TAB_BAR_HEIGHT (f) = height;
  FRAME_TAB_BAR_LINES (f) = lines;
  store_frame_param (f, Qtab_bar_lines, make_fixnum (lines));

  if (FRAME_NS_WINDOW (f) && FRAME_TAB_BAR_HEIGHT (f) == 0)
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
}

/* tabbar support */
static void
ns_set_tab_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
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
    ns_change_tab_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}


/* toolbar support */
static void
ns_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  /* Currently, when the tool bar changes state, the frame is resized.

     TODO: It would be better if this didn't occur when 1) the frame
     is full height or maximized or 2) when specified by
     `frame-inhibit-implied-resize'.  */
  int nlines;

  NSTRACE ("ns_set_tool_bar_lines");

  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  if (nlines)
    {
      FRAME_EXTERNAL_TOOL_BAR (f) = 1;
      update_frame_tool_bar (f);
    }
  else
    {
      if (FRAME_EXTERNAL_TOOL_BAR (f))
        {
          free_frame_tool_bar (f);
          FRAME_EXTERNAL_TOOL_BAR (f) = 0;

          {
            EmacsView *view = FRAME_NS_VIEW (f);
            int fs_state = [view fullscreenState];

            if (fs_state == FULLSCREEN_MAXIMIZED)
              {
                [view setFSValue:FULLSCREEN_WIDTH];
              }
            else if (fs_state == FULLSCREEN_HEIGHT)
              {
                [view setFSValue:FULLSCREEN_NONE];
              }
          }
       }
    }

  adjust_frame_size (f, -1, -1, 2, false, Qtool_bar_lines);
}

static void
ns_set_child_frame_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
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

      if (FRAME_NATIVE_WINDOW (f) != 0)
	adjust_frame_size (f, -1, -1, 3, 0, Qchild_frame_border_width);

      SET_FRAME_GARBAGED (f);
    }
}

static void
ns_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int old_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  int new_width = check_int_nonnegative (arg);

  if (new_width == old_width)
    return;
  f->internal_border_width = new_width;

  if (FRAME_NATIVE_WINDOW (f) != 0)
    adjust_frame_size (f, -1, -1, 3, 0, Qinternal_border_width);

  SET_FRAME_GARBAGED (f);
}


static void
ns_implicitly_set_icon_type (struct frame *f)
{
  Lisp_Object tem;
  EmacsView *view = FRAME_NS_VIEW (f);
  id image = nil;
  Lisp_Object chain, elt;
  NSAutoreleasePool *pool;
  BOOL setMini = YES;
  NSWorkspace *workspace;

  NSTRACE ("ns_implicitly_set_icon_type");

  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  workspace = [NSWorkspace sharedWorkspace];
  if (f->output_data.ns->miniimage
      && [[NSString stringWithLispString:f->name]
               isEqualToString: [(NSImage *)f->output_data.ns->miniimage name]])
    {
      [pool release];
      unblock_input ();
      return;
    }

  tem = assq_no_quit (Qicon_type, f->param_alist);
  if (CONSP (tem) && ! NILP (XCDR (tem)))
    {
      [pool release];
      unblock_input ();
      return;
    }

  for (chain = Vns_icon_type_alist;
       image == nil && CONSP (chain);
       chain = XCDR (chain))
    {
      elt = XCAR (chain);
      /* Special case: t means go by file type.  */
      if (SYMBOLP (elt) && EQ (elt, Qt) && SSDATA (f->name)[0] == '/')
        {
          NSString *str
	     = [NSString stringWithLispString:f->name];
          if ([[NSFileManager defaultManager] fileExistsAtPath: str])
            image = [[[NSWorkspace sharedWorkspace] iconForFile: str] retain];
        }
      else if (CONSP (elt) &&
               STRINGP (XCAR (elt)) &&
               STRINGP (XCDR (elt)) &&
               fast_string_match (XCAR (elt), f->name) >= 0)
        {
          image = [EmacsImage allocInitFromFile: XCDR (elt)];
          if (image == nil)
            image = [[NSImage imageNamed:
                               [NSString stringWithLispString:XCDR (elt)]] retain];
        }
    }

  if (image == nil)
    {
#ifndef NS_IMPL_GNUSTEP
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
      if ([workspace respondsToSelector: @selector (iconForContentType:)])
#endif
	image = [[workspace iconForContentType:
			      [UTType typeWithIdentifier: @"text"]] retain];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
      else
#endif
#endif
#endif
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
	image = [[workspace iconForFileType: @"text"] retain];
#endif
      setMini = NO;
    }

  [f->output_data.ns->miniimage release];
  f->output_data.ns->miniimage = image;
  [view setMiniwindowImage: setMini];
  [pool release];
  unblock_input ();
}


static void
ns_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  EmacsView *view = FRAME_NS_VIEW (f);
  id image = nil;
  BOOL setMini = YES;

  NSTRACE ("ns_set_icon_type");

  if (!NILP (arg) && SYMBOLP (arg))
    {
      arg =build_string (SSDATA (SYMBOL_NAME (arg)));
      store_frame_param (f, Qicon_type, arg);
    }

  /* Do it the implicit way.  */
  if (NILP (arg))
    {
      ns_implicitly_set_icon_type (f);
      return;
    }

  CHECK_STRING (arg);

  image = [EmacsImage allocInitFromFile: arg];
  if (image == nil)
    image =[NSImage imageNamed: [NSString stringWithLispString:arg]];

  if (image == nil)
    {
      image = [NSImage imageNamed: @"text"];
      setMini = NO;
    }

  f->output_data.ns->miniimage = image;
  [view setMiniwindowImage: setMini];
}

/* This is the same as the xfns.c definition.  */
static void
ns_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

/* called to set mouse pointer color, but all other terms use it to
   initialize pointer types (and don't set the color ;) */
static void
ns_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  /* Don't think we can do this on Nextstep.  */
}


#define Str(x) #x
#define Xstr(x) Str(x)

static Lisp_Object
ns_appkit_version_str (void)
{
  NSString *tmp;
  Lisp_Object string;
  NSAutoreleasePool *autorelease;

  autorelease = [[NSAutoreleasePool alloc] init];
#ifdef NS_IMPL_GNUSTEP
  tmp = [NSString stringWithFormat:@"gnustep-gui-%s", Xstr(GNUSTEP_GUI_VERSION)];
#elif defined (NS_IMPL_COCOA)
  tmp = [NSString stringWithFormat:@"appkit-%.2f %@",
                  NSAppKitVersionNumber,
                  [[NSProcessInfo processInfo] operatingSystemVersionString]];
#else
  tmp = [NSString initWithUTF8String:@"ns-unknown"];
#endif
  string = [tmp lispString];
  [autorelease release];

  return string;
}


/* This is for use by x-server-version and collapses all version info we
   have into a single int.  For a better picture of the implementation
   running, use ns_appkit_version_str.  */
static int
ns_appkit_version_int (void)
{
#ifdef NS_IMPL_GNUSTEP
  return GNUSTEP_GUI_MAJOR_VERSION * 100 + GNUSTEP_GUI_MINOR_VERSION;
#elif defined (NS_IMPL_COCOA)
  return (int)NSAppKitVersionNumber;
#endif
  return 0;
}


static void
ns_icon (struct frame *f, Lisp_Object parms)
/* --------------------------------------------------------------------------
   Strangely-named function to set icon position parameters in frame.
   This is irrelevant under macOS, but might be needed under GNUstep,
   depending on the window manager used.  Note, this is not a standard
   frame parameter-setter; it is called directly from x-create-frame.
   -------------------------------------------------------------------------- */
{
  Lisp_Object icon_x, icon_y;
  struct ns_display_info *dpyinfo = check_ns_display_info (Qnil);

  f->output_data.ns->icon_top = -1;
  f->output_data.ns->icon_left = -1;

  /* Set the position of the icon.  */
  icon_x = gui_display_get_arg (dpyinfo, parms, Qicon_left, 0, 0,
                                RES_TYPE_NUMBER);
  icon_y = gui_display_get_arg (dpyinfo, parms, Qicon_top, 0, 0,
                                RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_FIXNUM (icon_x);
      CHECK_FIXNUM (icon_y);
      f->output_data.ns->icon_top = XFIXNUM (icon_y);
      f->output_data.ns->icon_left = XFIXNUM (icon_x);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}


/* Note: see frame.c for template, also where generic functions are
   implemented.  */
frame_parm_handler ns_frame_parm_handlers[] =
{
  gui_set_autoraise, /* generic OK */
  gui_set_autolower, /* generic OK */
  ns_set_background_color,
  0, /* x_set_border_color,  may be impossible under Nextstep */
  0, /* x_set_border_width,  may be impossible under Nextstep */
  ns_set_cursor_color,
  ns_set_cursor_type,
  gui_set_font, /* generic OK */
  ns_set_foreground_color,
  ns_set_icon_name,
  ns_set_icon_type,
  ns_set_child_frame_border_width,
  ns_set_internal_border_width,
  gui_set_right_divider_width, /* generic OK */
  gui_set_bottom_divider_width, /* generic OK */
  ns_set_menu_bar_lines,
  ns_set_mouse_color,
  ns_explicitly_set_name,
  gui_set_scroll_bar_width, /* generic OK */
  gui_set_scroll_bar_height, /* generic OK */
  ns_set_title,
  gui_set_unsplittable, /* generic OK */
  gui_set_vertical_scroll_bars, /* generic OK */
  gui_set_horizontal_scroll_bars, /* generic OK */
  gui_set_visibility, /* generic OK */
  ns_set_tab_bar_lines,
  ns_set_tool_bar_lines,
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
  ns_set_undecorated,
  ns_set_parent_frame,
  0, /* x_set_skip_taskbar */
  ns_set_no_focus_on_map,
  ns_set_no_accept_focus,
  ns_set_z_group,
  0, /* x_set_override_redirect */
  gui_set_no_special_glyphs,
  gui_set_alpha_background,
  NULL,
#ifdef NS_IMPL_COCOA
  ns_set_appearance,
  ns_set_transparent_titlebar,
#endif
};


/* Handler for signals raised during x_create_frame.
   FRAME is the frame which is partially constructed.  */

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
#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

      /* If the frame's image cache refcount is still the same as our
	 private shadow variable, it means we are unwinding a frame
	 for which we didn't yet call init_frame_faces, where the
	 refcount is incremented.  Therefore, we increment it here, so
	 that free_frame_faces, called in ns_free_frame_resources
	 below, will not mistakenly decrement the counter that was not
	 incremented yet to account for this new frame.  */
      if (FRAME_IMAGE_CACHE (f) != NULL
	  && FRAME_IMAGE_CACHE (f)->refcount == image_cache_refcount)
	FRAME_IMAGE_CACHE (f)->refcount++;

      ns_free_frame_resources (f);
      free_glyphs (f);

#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      /* Check that reference counts are indeed correct.  */
      eassert (dpyinfo->terminal->image_cache->refcount == image_cache_refcount);
#endif

      return Qt;
    }

  return Qnil;
}


static void
do_unwind_create_frame (Lisp_Object frame)
{
  unwind_create_frame (frame);
}

/*
 * Read geometry related parameters from preferences if not in PARMS.
 * Returns the union of parms and any preferences read.
 */

static Lisp_Object
get_geometry_from_preferences (struct ns_display_info *dpyinfo,
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

/* ==========================================================================

    Lisp definitions

   ========================================================================== */

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct ns_display_info *dpyinfo = NULL;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;
  static int desc_ctr = 1;
  NSWindow *main_window = [NSApp mainWindow];

  /* gui_display_get_arg modifies parms.  */
  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0,
                                 RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_ns_display_info (display);
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

  parent = gui_display_get_arg (dpyinfo, parms, Qparent_id, 0, 0,
                                RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_FIXNUM (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  tem = gui_display_get_arg (dpyinfo, parms, Qminibuffer,
                             "minibuffer", "Minibuffer",
                             RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
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

  f->output_method = output_ns;
  f->output_data.ns = xzalloc (sizeof *f->output_data.ns);

  FRAME_FONTSET (f) = -1;

  fset_icon_name (f, gui_display_get_arg (dpyinfo, parms, Qicon_name,
                                          "iconName", "Title",
                                          RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (do_unwind_create_frame, frame);

  f->output_data.ns->window_desc = desc_ctr++;
  if (TYPE_RANGED_FIXNUMP (Window, parent))
    {
      f->output_data.ns->parent_desc = XFIXNAT (parent);
      f->output_data.ns->explicit_parent = 1;
    }
  else
    {
      f->output_data.ns->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
      f->output_data.ns->explicit_parent = 0;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name) || ! STRINGP (name))
    {
      fset_name (f, [ns_app_name lispString]);
      f->explicit_name = 0;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = 1;
      specbind (Qx_resource_name, name);
    }

  block_input ();

#ifdef NS_IMPL_COCOA
    mac_register_font_driver (f);
#else
    register_font_driver (&nsfont_driver, f);
#endif

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  {
#ifdef NS_IMPL_COCOA
    /* use for default font name */
    id font = [NSFont userFixedPitchFontOfSize: -1.0]; /* default */
    gui_default_parameter (f, parms, Qfontsize,
                           make_fixnum (0 /* (int)[font pointSize] */),
                           "fontSize", "FontSize", RES_TYPE_NUMBER);
    // Remove ' Regular', not handled by backends.
    char *fontname = xstrdup ([[font displayName] UTF8String]);
    int len = strlen (fontname);
    if (len > 8 && strcmp (fontname + len - 8, " Regular") == 0)
      fontname[len-8] = '\0';
    gui_default_parameter (f, parms, Qfont,
                           build_string (fontname),
                           "font", "Font", RES_TYPE_STRING);
    xfree (fontname);
#else
    gui_default_parameter (f, parms, Qfont,
                           build_string ("fixed"),
                           "font", "Font", RES_TYPE_STRING);
#endif
  }
  unblock_input ();

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                         "borderwidth", "BorderWidth", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qinternal_border_width, make_fixnum (2),
                         "internalBorderWidth", "InternalBorderWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qchild_frame_border_width, Qnil,
			 "childFrameBorderWidth", "childFrameBorderWidth",
			 RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
		       NULL, NULL, RES_TYPE_NUMBER);

  /* default vertical scrollbars on right on Mac */
  {
      Lisp_Object spos
#ifdef NS_IMPL_GNUSTEP
          = Qt;
#else
          = Qright;
#endif
      gui_default_parameter (f, parms, Qvertical_scroll_bars, spos,
                             "verticalScrollBars", "VerticalScrollBars",
                             RES_TYPE_SYMBOL);
  }
  gui_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil,
                         "horizontalScrollBars", "HorizontalScrollBars",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qforeground_color, build_string ("Black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("White"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qline_spacing, Qnil,
                         "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qleft_fringe, Qnil,
                         "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_fringe, Qnil,
                         "rightFringe", "RightFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qno_special_glyphs, Qnil,
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

  tem = gui_display_get_arg (dpyinfo, parms, Qundecorated, NULL, NULL,
                             RES_TYPE_BOOLEAN);
  FRAME_UNDECORATED (f) = !NILP (tem) && !EQ (tem, Qunbound);
  store_frame_param (f, Qundecorated, FRAME_UNDECORATED (f) ? Qt : Qnil);

#ifdef NS_IMPL_COCOA
  tem = gui_display_get_arg (dpyinfo, parms, Qns_appearance, NULL, NULL,
                             RES_TYPE_SYMBOL);
  if (EQ (tem, Qdark))
    FRAME_NS_APPEARANCE (f) = ns_appearance_vibrant_dark;
  else if (EQ (tem, Qlight))
    FRAME_NS_APPEARANCE (f) = ns_appearance_aqua;
  else
    FRAME_NS_APPEARANCE (f) = ns_appearance_system_default;
  store_frame_param (f, Qns_appearance,
                     (!NILP (tem) && !EQ (tem, Qunbound)) ? tem : Qnil);

  tem = gui_display_get_arg (dpyinfo, parms, Qns_transparent_titlebar,
                             NULL, NULL, RES_TYPE_BOOLEAN);
  FRAME_NS_TRANSPARENT_TITLEBAR (f) = !NILP (tem) && !EQ (tem, Qunbound);
  store_frame_param (f, Qns_transparent_titlebar,
                     FRAME_NS_TRANSPARENT_TITLEBAR (f) ? Qt : Qnil);
#endif

  parent_frame = gui_display_get_arg (dpyinfo, parms, Qparent_frame, NULL, NULL,
                                      RES_TYPE_SYMBOL);
  /* Accept parent-frame iff parent-id was not specified.  */
  if (!NILP (parent)
      || EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  gui_default_parameter (f, parms, Qz_group, Qnil, NULL, NULL, RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qno_focus_on_map, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qno_accept_focus, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  /* The resources controlling the menu-bar and tool-bar are
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

  parms = get_geometry_from_preferences (dpyinfo, parms);
  window_prompting = gui_figure_window_size (f, parms, false, true);

  tem = gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0,
                             RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || (!EQ (tem, Qunbound) && !NILP (tem));

  /* NOTE: on other terms, this is done in set_mouse_color, however this
     was not getting called under Nextstep.  */
  f->output_data.ns->text_cursor = [NSCursor IBeamCursor];
  f->output_data.ns->nontext_cursor = [NSCursor arrowCursor];
  f->output_data.ns->modeline_cursor = [NSCursor pointingHandCursor];
  f->output_data.ns->hand_cursor = [NSCursor pointingHandCursor];
  f->output_data.ns->hourglass_cursor = [NSCursor disappearingItemCursor];
  f->output_data.ns->horizontal_drag_cursor = [NSCursor resizeLeftRightCursor];
  f->output_data.ns->vertical_drag_cursor = [NSCursor resizeUpDownCursor];
  f->output_data.ns->left_edge_cursor = [NSCursor resizeLeftRightCursor];
  f->output_data.ns->top_left_corner_cursor = [NSCursor arrowCursor];
  f->output_data.ns->top_edge_cursor = [NSCursor resizeUpDownCursor];
  f->output_data.ns->top_right_corner_cursor = [NSCursor arrowCursor];
  f->output_data.ns->right_edge_cursor = [NSCursor resizeLeftRightCursor];
  f->output_data.ns->bottom_right_corner_cursor = [NSCursor arrowCursor];
  f->output_data.ns->bottom_edge_cursor = [NSCursor resizeUpDownCursor];
  f->output_data.ns->bottom_left_corner_cursor = [NSCursor arrowCursor];

  FRAME_DISPLAY_INFO (f)->vertical_scroll_bar_cursor
     = [NSCursor arrowCursor];
  FRAME_DISPLAY_INFO (f)->horizontal_scroll_bar_cursor
     = [NSCursor arrowCursor];
  f->output_data.ns->current_pointer = f->output_data.ns->text_cursor;

  f->output_data.ns->in_animation = NO;

#ifdef NS_IMPL_COCOA
  /* If the app has previously been disabled, start it up again.  */
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
#endif

  [[EmacsView alloc] initFrameFromEmacs: f];

  ns_icon (f, parms);

  /* ns_display_info does not have a reference_count.  */
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error
     below.  The frame needs to be on Vframe_list or making it visible
     won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);

  gui_default_parameter (f, parms, Qicon_type, Qnil,
                         "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);

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
  gui_default_parameter (f, parms, Qalpha_background, Qnil,
                         "alphaBackground", "AlphaBackground", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qfullscreen, Qnil,
                         "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  /* Allow set_window_size_hook, now.  */
  f->can_set_window_size = true;

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, Qx_create_frame_2);

  if (! f->output_data.ns->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
                                        RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;

      if (EQ (visibility, Qicon))
	ns_iconify_frame (f);
      else if (! NILP (visibility))
	{
	  ns_make_frame_visible (f);
	  [[FRAME_NS_VIEW (f) window] makeKeyWindow];
	}
      else
        {
	  /* Must have been Qnil.  */
	  f->was_invisible = true;
        }
    }

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

  /* This cascading behavior (which is the job of the window manager
     on X-based systems) is something NS applications are expected to
     implement themselves.  At least one person tells me he used
     Carbon Emacs solely for this behavior.  */
  if (window_prompting & (USPosition | PPosition) || FRAME_PARENT_FRAME (f))
    ns_set_offset (f, f->left_pos, f->top_pos, 1);
  else
    {
      NSWindow *frame_window = [FRAME_NS_VIEW (f) window];
      NSPoint top_left;

      if (main_window)
	{
	  top_left = NSMakePoint (NSMinX ([main_window frame]),
				  NSMaxY ([main_window frame]));
	  top_left = [frame_window cascadeTopLeftFromPoint: top_left];
	  [frame_window cascadeTopLeftFromPoint: top_left];
	}
      else
	[frame_window center];
    }

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}

static BOOL
ns_window_is_ancestor (NSWindow *win, NSWindow *candidate)
/* Test whether CANDIDATE is an ancestor window of WIN.  */
{
  if (candidate == NULL)
    return NO;
  else if (win == candidate)
    return YES;
  else
    return ns_window_is_ancestor(win, [candidate parentWindow]);
}

DEFUN ("ns-frame-list-z-order", Fns_frame_list_z_order,
       Sns_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs' frames, in Z (stacking) order.
If TERMINAL is non-nil and specifies a live frame, return the child
frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).  */)
  (Lisp_Object terminal)
{
  Lisp_Object frames = Qnil;
  NSWindow *parent = nil;

  if (FRAMEP (terminal) && FRAME_LIVE_P (XFRAME (terminal)))
    parent = [FRAME_NS_VIEW (XFRAME (terminal)) window];

  for (NSWindow *win in [[NSApp orderedWindows] reverseObjectEnumerator])
    {
      Lisp_Object frame;

      /* Check against [win parentWindow] so that it doesn't match itself. */
      if ([[win delegate] isKindOfClass:[EmacsView class]]
          && (parent == nil || ns_window_is_ancestor (parent, [win parentWindow])))
        {
          XSETFRAME (frame, ((EmacsView *)[win delegate])->emacsframe);
          frames = Fcons(frame, frames);
        }
    }

  return frames;
}

DEFUN ("ns-frame-restack", Fns_frame_restack, Sns_frame_restack, 2, 3, 0,
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

  if (FRAME_NS_VIEW (f1) && FRAME_NS_VIEW (f2))
    {
      EmacsWindow *window = (EmacsWindow *)[FRAME_NS_VIEW (f1) window];
      NSWindow *window2 = [FRAME_NS_VIEW (f2) window];

      if ([window restackWindow:window2 above:!NILP (above)])
        return Qt;
      else
        return Qnil;
    }
  else
    {
      error ("Cannot restack frames");
      return Qnil;
    }
}

DEFUN ("x-select-font", Fx_select_font, Sx_select_font, 0, 2, 0,
       doc: /* Read a font using a Nextstep dialog.
Return a font specification describing the selected font.

FRAME is the frame on which to pop up the font chooser.  If omitted or
nil, it defaults to the selected frame. */)
  (Lisp_Object frame, Lisp_Object ignored)
{
  struct frame *f = decode_window_system_frame (frame);
  Lisp_Object font = [FRAME_NS_VIEW (f) showFontPanel];

  if (NILP (font))
    quit ();

  return font;
}

DEFUN ("ns-popup-color-panel", Fns_popup_color_panel, Sns_popup_color_panel,
       0, 1, "",
       doc: /* Pop up the color panel.  */)
     (Lisp_Object frame)
{
  check_window_system (NULL);
  [NSApp orderFrontColorPanel: NSApp];
  return Qnil;
}

static struct
{
  id panel;
  BOOL ret;
#ifdef NS_IMPL_GNUSTEP
  NSString *dirS, *initS;
  BOOL no_types;
#endif
} ns_fd_data;

void
ns_run_file_dialog (void)
{
  if (ns_fd_data.panel == nil) return;
#ifdef NS_IMPL_COCOA
  ns_fd_data.ret = [ns_fd_data.panel runModal];
#else
  if (ns_fd_data.no_types)
    {
      ns_fd_data.ret = [ns_fd_data.panel
                           runModalForDirectory: ns_fd_data.dirS
                           file: ns_fd_data.initS];
    }
  else
    {
      ns_fd_data.ret = [ns_fd_data.panel
                           runModalForDirectory: ns_fd_data.dirS
                           file: ns_fd_data.initS
                           types: nil];
    }
#endif
  ns_fd_data.panel = nil;
}

#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
#define MODAL_OK_RESPONSE NSModalResponseOK
#endif
#endif
#ifndef MODAL_OK_RESPONSE
#define MODAL_OK_RESPONSE NSOKButton
#endif

DEFUN ("ns-read-file-name", Fns_read_file_name, Sns_read_file_name, 1, 5, 0,
       doc: /* Use a graphical panel to read a file name, using prompt PROMPT.
Optional arg DIR, if non-nil, supplies a default directory.
Optional arg MUSTMATCH, if non-nil, means the returned file or
directory must exist.
Optional arg INIT, if non-nil, provides a default file name to use.
Optional arg DIR-ONLY-P, if non-nil, means choose only directories.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object mustmatch,
   Lisp_Object init, Lisp_Object dir_only_p)
{
  static id fileDelegate = nil;
  BOOL isSave = NILP (mustmatch) && NILP (dir_only_p);
  id panel;
  Lisp_Object fname = Qnil;
  NSString *promptS, *dirS, *initS, *str;
  NSEvent *nxev;

  promptS = (NILP (prompt) || !STRINGP (prompt)
	     ? nil : [NSString stringWithLispString: prompt]);
  dirS = (NILP (dir) || !STRINGP (dir)
	  ? [NSString stringWithLispString:
			ENCODE_FILE (BVAR (current_buffer, directory))] :
	  [NSString stringWithLispString: ENCODE_FILE (dir)]);
  initS = (NILP (init) || !STRINGP (init)
	   ? nil : [NSString stringWithLispString: init]);

  check_window_system (NULL);

  if (fileDelegate == nil)
    fileDelegate = [EmacsFileDelegate new];

  [NSCursor setHiddenUntilMouseMoves: NO];

  if ([dirS characterAtIndex: 0] == '~')
    dirS = [dirS stringByExpandingTildeInPath];

  panel = isSave ?
    (id)[NSSavePanel savePanel] : (id)[NSOpenPanel openPanel];

  [panel setTitle: promptS];

  [panel setAllowsOtherFileTypes: YES];
  [panel setTreatsFilePackagesAsDirectories: YES];
  [panel setDelegate: fileDelegate];

  if (! NILP (dir_only_p))
    {
      [panel setCanChooseDirectories: YES];
      [panel setCanChooseFiles: NO];
    }
  else if (! isSave)
    {
      /* This is not quite what the documentation says, but it is compatible
         with the Gtk+ code.  Also, the menu entry says "Open File...".  */
      [panel setCanChooseDirectories: NO];
      [panel setCanChooseFiles: YES];
    }

  block_input ();
  ns_fd_data.panel = panel;
  ns_fd_data.ret = NO;
#ifdef NS_IMPL_COCOA
  if (! NILP (mustmatch) || ! NILP (dir_only_p))
    {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
      if ([panel respondsToSelector: @selector (setAllowedContentTypes:)])
#endif
	[panel setAllowedContentTypes: [NSArray array]];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
      else
#endif
#endif
#if MAC_OS_X_VERSION_MIN_REQUIRED < 120000
	[panel setAllowedFileTypes: nil];
#endif
    }
  if (dirS) [panel setDirectoryURL: [NSURL fileURLWithPath: dirS]];
  if (initS && NILP (Ffile_directory_p (init)))
    [panel setNameFieldStringValue: [initS lastPathComponent]];
  else
    [panel setNameFieldStringValue: @""];

#else
  ns_fd_data.no_types = NILP (mustmatch) && NILP (dir_only_p);
  ns_fd_data.dirS = dirS;
  ns_fd_data.initS = initS;
#endif

  /* runModalForDirectory/runModal restarts the main event loop when done,
     so we must start an event loop and then pop up the file dialog.
     The file dialog may pop up a confirm dialog after Ok has been pressed,
     so we can not simply pop down on the Ok/Cancel press.
   */
  nxev = [NSEvent otherEventWithType: NSEventTypeApplicationDefined
                            location: NSMakePoint (0, 0)
                       modifierFlags: 0
                           timestamp: 0
                        windowNumber: [[NSApp mainWindow] windowNumber]
                             context: [NSApp context]
                             subtype: 0
                               data1: 0
                               data2: NSAPP_DATA2_RUNFILEDIALOG];

  [NSApp postEvent: nxev atStart: NO];
  while (ns_fd_data.panel != nil)
    [NSApp run];

  if (ns_fd_data.ret == MODAL_OK_RESPONSE)
    {
      str = ns_filename_from_panel (panel);

      if (!str)
	str = ns_directory_from_panel (panel);
      if (str)
	fname = [str lispString];

      if (!NILP (fname))
	fname = DECODE_FILE (fname);
    }

  [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];
  unblock_input ();

  return fname;
}

const char *
ns_get_defaults_value (const char *key)
{
  NSObject *obj = [[NSUserDefaults standardUserDefaults]
                    objectForKey: [NSString stringWithUTF8String: key]];

  if (!obj) return NULL;

  return [[NSString stringWithFormat: @"%@", obj] UTF8String];
}


DEFUN ("ns-get-resource", Fns_get_resource, Sns_get_resource, 2, 2, 0,
       doc: /* Return the value of the property NAME of OWNER from the defaults database.
If OWNER is nil, Emacs is assumed.  */)
  (Lisp_Object owner, Lisp_Object name)
{
  const char *value;

  check_window_system (NULL);
  if (NILP (owner))
    owner = [ns_app_name lispString];
  CHECK_STRING (name);

  value = ns_get_defaults_value (SSDATA (name));

  if (value)
    return build_string (value);
  return Qnil;
}


DEFUN ("ns-set-resource", Fns_set_resource, Sns_set_resource, 3, 3, 0,
       doc: /* Set property NAME of OWNER to VALUE, from the defaults database.
If OWNER is nil, Emacs is assumed.
If VALUE is nil, the default is removed.  */)
  (Lisp_Object owner, Lisp_Object name, Lisp_Object value)
{
  check_window_system (NULL);
  if (NILP (owner))
    owner = [ns_app_name lispString];
  CHECK_STRING (name);
  if (NILP (value))
    {
      [[NSUserDefaults standardUserDefaults] removeObjectForKey:
                         [NSString stringWithLispString:name]];
    }
  else
    {
      CHECK_STRING (value);
      [[NSUserDefaults standardUserDefaults] setObject:
                [NSString stringWithLispString:value]
                                        forKey: [NSString stringWithLispString:name]];
    }

  return Qnil;
}


DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  /* This function has no real equivalent under Nextstep.  Return nil to
     indicate this.  */
  return Qnil;
}


DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
#ifdef NS_IMPL_GNUSTEP
  return build_string ("GNU");
#else
  return build_string ("Apple");
#endif
}


DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  /* NOTE: it is unclear what would best correspond with "protocol";
           we return 10.3, meaning Panther, since this is roughly the
           level that GNUstep's APIs correspond to.  The last number
           is where we distinguish between the Apple and GNUstep
           implementations ("distributor-specific release number") and
           give int'ized versions of major.minor.  */
  return list3i (10, 3, ns_appkit_version_int ());
}


DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  return make_fixnum (1);
}


DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_fixnum (ns_display_pixel_height (dpyinfo) / (92.0/25.4));
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_fixnum (ns_display_pixel_width (dpyinfo) / (92.0/25.4));
}


DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  /* Note that the xfns.c version has different return values.  */
  switch ([ns_get_window (terminal) backingType])
    {
    case NSBackingStoreBuffered:
      return intern ("buffered");
#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101300
    case NSBackingStoreRetained:
      return intern ("retained");
    case NSBackingStoreNonretained:
      return intern ("non-retained");
#endif
    default:
      error ("Strange value for backingType parameter of frame");
    }
  return Qnil;  /* not reached, shut compiler up */
}


DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  NSWindowDepth depth;

  check_ns_display_info (terminal);
  depth = [[[NSScreen screens] objectAtIndex:0] depth];

  if ( depth == NSBestDepth (NSCalibratedWhiteColorSpace, 2, 2, YES, NULL))
    return intern ("static-gray");
  else if (depth == NSBestDepth (NSCalibratedWhiteColorSpace, 8, 8, YES, NULL))
    return intern ("gray-scale");
  else if ( depth == NSBestDepth (NSCalibratedRGBColorSpace, 8, 8, YES, NULL))
    return intern ("pseudo-color");
  else if ( depth == NSBestDepth (NSCalibratedRGBColorSpace, 4, 12, NO, NULL))
    return intern ("true-color");
  else if ( depth == NSBestDepth (NSCalibratedRGBColorSpace, 8, 24, NO, NULL))
    return intern ("direct-color");
  else
    /* Color management as far as we do it is really handled by
       Nextstep itself anyway.  */
    return intern ("direct-color");
}


DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  switch ([ns_get_window (terminal) backingType])
    {
    case NSBackingStoreBuffered:
      return Qt;

#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101300
    case NSBackingStoreRetained:
    case NSBackingStoreNonretained:
      return Qnil;
#endif

    default:
      error ("Strange value for backingType parameter of frame");
    }
  return Qnil;  /* not reached, shut compiler up */
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object display, Lisp_Object resource_string, Lisp_Object must_succeed)
{
  struct ns_display_info *dpyinfo;

  CHECK_STRING (display);

  nxatoms_of_nsselect ();
  dpyinfo = ns_term_init (display);
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
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  [NSApp terminate: NSApp];
  return Qnil;
}


DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (void)
{
  Lisp_Object result = Qnil;
  struct ns_display_info *ndi;

  for (ndi = x_display_list; ndi; ndi = ndi->next)
    result = Fcons (XCAR (ndi->name_list_element), result);

  return result;
}


DEFUN ("ns-hide-others", Fns_hide_others, Sns_hide_others,
       0, 0, 0,
       doc: /* Hides all applications other than Emacs.  */)
     (void)
{
  check_window_system (NULL);
  [NSApp hideOtherApplications: NSApp];
  return Qnil;
}

DEFUN ("ns-hide-emacs", Fns_hide_emacs, Sns_hide_emacs,
       1, 1, 0,
       doc: /* If ON is non-nil, the entire Emacs application is hidden.
Otherwise if Emacs is hidden, it is unhidden.
If ON is equal to `activate', Emacs is unhidden and becomes
the active application.
If ON is equal to `activate-front', Emacs is unhidden and
becomes the active application, but only the selected frame
is layered in front of the windows of other applications.  */)
  (Lisp_Object on)
{
  check_window_system (NULL);
  if (EQ (on, intern ("activate")))
    {
      [NSApp unhide: NSApp];
      [NSApp activateIgnoringOtherApps: YES];
    }
#if GNUSTEP_GUI_MAJOR_VERSION > 0 || GNUSTEP_GUI_MINOR_VERSION >= 27
  else if (EQ (on, intern ("activate-front")))
    {
      [NSApp unhide: NSApp];
      [[NSRunningApplication currentApplication]
        activateWithOptions: NSApplicationActivateIgnoringOtherApps];
    }
#endif
  else if (NILP (on))
    [NSApp unhide: NSApp];
  else
    [NSApp hide: NSApp];
  return Qnil;
}


DEFUN ("ns-emacs-info-panel", Fns_emacs_info_panel, Sns_emacs_info_panel,
       0, 0, 0,
       doc: /* Shows the `Info' or `About' panel for Emacs.  */)
     (void)
{
  check_window_system (NULL);
  [NSApp orderFrontStandardAboutPanel: nil];
  return Qnil;
}


DEFUN ("ns-font-name", Fns_font_name, Sns_font_name, 1, 1, 0,
       doc: /* Determine font PostScript or family name for font NAME.
NAME should be a string containing either the font name or an XLFD
font descriptor.  If string contains `fontset' and not
`fontset-startup', it is left alone.  */)
     (Lisp_Object name)
{
  char *nm;
  CHECK_STRING (name);
  nm = SSDATA (name);

  if (nm[0] != '-')
    return name;
  if (strstr (nm, "fontset") && !strstr (nm, "fontset-startup"))
    return name;

  return build_string (ns_xlfd_to_fontname (SSDATA (name)));
}


DEFUN ("ns-list-colors", Fns_list_colors, Sns_list_colors, 0, 1, 0,
       doc: /* Return a list of all available colors.
The optional argument FRAME is currently ignored.  */)
     (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  NSEnumerator *colorlists;
  NSColorList *clist;
  NSAutoreleasePool *pool;

  if (!NILP (frame))
    {
      CHECK_FRAME (frame);
      if (! FRAME_NS_P (XFRAME (frame)))
        error ("non-Nextstep frame used in `ns-list-colors'");
    }

  block_input ();
  /* This can be called during dumping, so we need to set up a
     temporary autorelease pool.  */
  pool = [[NSAutoreleasePool alloc] init];
  colorlists = [[NSColorList availableColorLists] objectEnumerator];
  while ((clist = [colorlists nextObject]))
    {
      if ([[clist name] length] < 7 ||
          [[clist name] rangeOfString: @"PANTONE"].location == 0)
        {
          NSEnumerator *cnames = [[clist allKeys] reverseObjectEnumerator];
          NSString *cname;
          while ((cname = [cnames nextObject]))
            list = Fcons ([cname lispString], list);
        }
    }
  [pool release];
  unblock_input ();

  return list;
}


DEFUN ("ns-list-services", Fns_list_services, Sns_list_services, 0, 0, 0,
       doc: /* List available Nextstep services by querying NSApp.  */)
     (void)
{
#ifdef NS_IMPL_COCOA
  /* You can't get services like this in 10.6+.  */
  return Qnil;
#else
  Lisp_Object ret = Qnil;
  NSMenu *svcs;

  check_window_system (NULL);
  svcs = [[NSMenu alloc] initWithTitle: @"Services"];
  [NSApp setServicesMenu: svcs];
  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: ns_return_types];

  [svcs setAutoenablesItems: NO];

  ret = interpret_services_menu (svcs, Qnil, ret);
  return ret;
#endif
}


DEFUN ("ns-perform-service", Fns_perform_service, Sns_perform_service,
       2, 2, 0,
       doc: /* Perform Nextstep SERVICE on SEND.
SEND should be either a string or nil.
The return value is the result of the service, as string, or nil if
there was no result.  */)
     (Lisp_Object service, Lisp_Object send)
{
  id pb;
  NSString *svcName;

  CHECK_STRING (service);
  check_window_system (NULL);

  svcName = [NSString stringWithLispString:service];

  pb =[NSPasteboard pasteboardWithUniqueName];
  ns_string_to_pasteboard (pb, send);

  if (NSPerformService (svcName, pb) == NO)
    Fsignal (Qquit, list1 (build_string ("service not available")));

  if ([[pb types] count] == 0)
    return build_string ("");
  return ns_string_from_pasteboard (pb);
}


#ifdef NS_IMPL_COCOA

/* Compile and execute the AppleScript SCRIPT and return the error
   status as function value.  A zero is returned if compilation and
   execution is successful, in which case *RESULT is set to a Lisp
   string or a number containing the resulting script value.  Otherwise,
   1 is returned.  */
static int
ns_do_applescript (Lisp_Object script, Lisp_Object *result)
{
  NSAppleEventDescriptor *desc;
  NSDictionary *errorDict;
  NSAppleEventDescriptor *returnDescriptor = NULL;

  NSAppleScript *scriptObject =
    [[NSAppleScript alloc] initWithSource:
			     [NSString stringWithLispString:script]];

  returnDescriptor = [scriptObject executeAndReturnError: &errorDict];
  [scriptObject release];
  *result = Qnil;

  if (returnDescriptor != NULL)
    {
      // successful execution
      if (kAENullEvent != [returnDescriptor descriptorType])
        {
	  *result = Qt;
	  // script returned an AppleScript result
	  if ((typeUnicodeText == [returnDescriptor descriptorType]) ||
#if defined (NS_IMPL_COCOA)
	      (typeUTF16ExternalRepresentation
	       == [returnDescriptor descriptorType]) ||
#endif
	      (typeUTF8Text == [returnDescriptor descriptorType]) ||
	      (typeCString == [returnDescriptor descriptorType]))
	    {
	      desc = [returnDescriptor coerceToDescriptorType: typeUTF8Text];
	      if (desc)
		*result = [[desc stringValue] lispString];
	    }
	  else
            {
	      /* use typeUTF16ExternalRepresentation? */
	      // coerce the result to the appropriate ObjC type
	      desc = [returnDescriptor coerceToDescriptorType: typeUTF8Text];
	      if (desc)
		*result = make_fixnum([desc int32Value]);
            }
        }
    }
  else
    {
      // no script result, return error
      return 1;
    }
  return 0;
}

/* Helper function called from sendEvent to run AppleScript
   from within the main event loop.  */

void
ns_run_ascript (void)
{
  if (! NILP (as_script))
    as_status = ns_do_applescript (as_script, as_result);
  as_script = Qnil;
}

DEFUN ("ns-do-applescript", Fns_do_applescript, Sns_do_applescript, 1, 1, 0,
       doc: /* Execute AppleScript SCRIPT and return the result.
If compilation and execution are successful, the resulting script value
is returned as a string, a number or, in the case of other constructs, t.
In case the execution fails, an error is signaled.  */)
     (Lisp_Object script)
{
  Lisp_Object result;
  int status;
  NSEvent *nxev;
  struct input_event ev;

  CHECK_STRING (script);
  check_window_system (NULL);

  block_input ();

  as_script = script;
  as_result = &result;

  /* Executing AppleScript requires the event loop to run, otherwise
     errors aren't returned and executeAndReturnError hangs forever.
     Post an event that runs AppleScript and then start the event
     loop.  The event loop is exited when the script is done.  */
  nxev = [NSEvent otherEventWithType: NSEventTypeApplicationDefined
                            location: NSMakePoint (0, 0)
                       modifierFlags: 0
                           timestamp: 0
                        windowNumber: [[NSApp mainWindow] windowNumber]
                             context: [NSApp context]
                             subtype: 0
                               data1: 0
                               data2: NSAPP_DATA2_RUNASSCRIPT];

  [NSApp postEvent: nxev atStart: NO];

  /* If there are other events, the event loop may exit.  Keep running
     until the script has been handled.  */
  ns_init_events (&ev);
  while (! NILP (as_script))
    [NSApp run];
  ns_finish_events ();

  status = as_status;
  as_status = 0;
  as_result = 0;
  unblock_input ();
  if (status == 0)
    return result;
  else if (!STRINGP (result))
    error ("AppleScript error %d", status);
  else
    error ("%s", SSDATA (result));
}
#endif



/* ==========================================================================

    Miscellaneous functions not called through hooks

   ========================================================================== */

/* called from frame.c */
struct ns_display_info *
check_x_display_info (Lisp_Object frame)
{
  return check_ns_display_info (frame);
}


void
ns_set_scroll_bar_default_width (struct frame *f)
{
  int wid = FRAME_COLUMN_WIDTH (f);
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = NS_SCROLL_BAR_WIDTH_DEFAULT;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) +
                                      wid - 1) / wid;
}

void
ns_set_scroll_bar_default_height (struct frame *f)
{
  int height = FRAME_LINE_HEIGHT (f);
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = NS_SCROLL_BAR_WIDTH_DEFAULT;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) +
				       height - 1) / height;
}

/* Terms implement this instead of x-get-resource directly.  */
const char *
ns_get_string_resource (void *_rdb, const char *name, const char *class)
{
  /* remove appname prefix; TODO: allow for !="Emacs" */
  const char *res, *toCheck = class + (!strncmp (class, "Emacs.", 6) ? 6 : 0);

  check_window_system (NULL);

  if (inhibit_x_resources)
    /* --quick was passed, so this is a no-op.  */
    return NULL;

  res = ns_get_defaults_value (toCheck);
  return (const char *) (!res ? NULL
                         : !c_strncasecmp (res, "YES", 3) ? "true"
                         : !c_strncasecmp (res, "NO", 2) ? "false"
                         : res);
}

/* ==========================================================================

    Lisp definitions that, for whatever reason, we can't alias as 'ns-XXX'.

   ========================================================================== */


#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1080
/* Moving files to the system recycle bin.
   Used by `move-file-to-trash' instead of the default moving to ~/.Trash  */
DEFUN ("system-move-file-to-trash", Fsystem_move_file_to_trash,
       Ssystem_move_file_to_trash, 1, 1, 0,
       doc: /* Move file or directory named FILENAME to the recycle bin.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;
  Lisp_Object operation;

  operation = Qdelete_file;
  if (!NILP (Ffile_directory_p (filename))
      && NILP (Ffile_symlink_p (filename)))
    {
      operation = intern ("delete-directory");
      filename = Fdirectory_file_name (filename);
    }

  /* Must have fully qualified file names for moving files to Trash. */
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename, operation);
  if (!NILP (handler))
    return call2 (handler, operation, filename);
  else
    {
      NSFileManager *fm = [NSFileManager defaultManager];
      BOOL result = NO;
      NSURL *fileURL = [NSURL fileURLWithPath:[NSString stringWithLispString:filename]
                                  isDirectory:!NILP (Ffile_directory_p (filename))];
      if ([fm respondsToSelector:@selector(trashItemAtURL:resultingItemURL:error:)])
        result = [fm trashItemAtURL:fileURL resultingItemURL:nil error:nil];

      if (!result)
	report_file_error ("Removing old name", list1 (filename));
    }
  return Qnil;
}
#endif

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object color, Lisp_Object frame)
{
  NSColor * col;
  check_window_system (NULL);
  return ns_lisp_to_color (color, &col) ? Qnil : Qt;
}


DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object color, Lisp_Object frame)
{
  NSColor * col;
  EmacsCGFloat red, green, blue, alpha;

  check_window_system (NULL);
  CHECK_STRING (color);

  block_input ();
  if (ns_lisp_to_color (color, &col))
    {
      unblock_input ();
      return Qnil;
    }

  [[col colorUsingDefaultColorSpace]
        getRed: &red green: &green blue: &blue alpha: &alpha];
  unblock_input ();
  return list3i (lrint (red * 65535), lrint (green * 65535),
		 lrint (blue * 65535));
}


DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
     (Lisp_Object terminal)
{
  NSWindowDepth depth;
  NSString *colorSpace;

  check_ns_display_info (terminal);
  depth = [[[NSScreen screens] objectAtIndex:0] depth];
  colorSpace = NSColorSpaceFromDepth (depth);

  return    [colorSpace isEqualToString: NSDeviceWhiteColorSpace]
         || [colorSpace isEqualToString: NSCalibratedWhiteColorSpace]
      ? Qnil : Qt;
}


DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  NSWindowDepth depth;

  check_ns_display_info (terminal);
  depth = [[[NSScreen screens] objectAtIndex:0] depth];

  return NSBitsPerPixelFromDepth (depth) > 1 ? Qt : Qnil;
}


DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_fixnum (ns_display_pixel_width (dpyinfo));
}


DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_fixnum (ns_display_pixel_height (dpyinfo));
}

#ifdef NS_IMPL_COCOA

/* Returns the name for the screen that OBJ represents, or NULL.
   Caller must free return value.
*/

static char *
ns_get_name_from_ioreg (io_object_t obj)
{
  char *name = NULL;

  NSDictionary *info = (NSDictionary *)
    IODisplayCreateInfoDictionary (obj, kIODisplayOnlyPreferredName);
  NSDictionary *names = [info objectForKey:
                                [NSString stringWithUTF8String:
                                            kDisplayProductName]];

  if ([names count] > 0)
    {
      NSString *n = [names objectForKey: [[names allKeys]
                                                 objectAtIndex:0]];
      if (n != nil) name = xstrdup ([n UTF8String]);
    }

  [info release];

  return name;
}

/* Returns the name for the screen that DID came from, or NULL.
   Caller must free return value.
*/

static char *
ns_screen_name (CGDirectDisplayID did)
{
  char *name = NULL;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1090
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1090
  if (CGDisplayIOServicePort == NULL)
#endif
    {
      mach_port_t masterPort;
      io_iterator_t it;
      io_object_t obj;

      /* CGDisplayIOServicePort is deprecated.  Do it another (harder) way.

         Is this code OK for macOS < 10.9, and GNUstep?  I suspect it is,
         in which case is it worth keeping the other method in here?  */

      if (IOMasterPort (MACH_PORT_NULL, &masterPort) != kIOReturnSuccess
          || IOServiceGetMatchingServices (masterPort,
                                           IOServiceMatching ("IONDRVDevice"),
                                           &it) != kIOReturnSuccess)
        return name;

      /* Must loop until we find a name.  Many devices can have the same unit
         number (represents different GPU parts), but only one has a name.  */
      while (! name && (obj = IOIteratorNext (it)))
        {
          CFMutableDictionaryRef props;
          const void *val;

          if (IORegistryEntryCreateCFProperties (obj,
                                                 &props,
                                                 kCFAllocatorDefault,
                                                 kNilOptions) == kIOReturnSuccess
              && props != nil
              && (val = CFDictionaryGetValue(props, @"IOFBDependentIndex")))
            {
              unsigned nr = [(NSNumber *)val unsignedIntegerValue];
              if (nr == CGDisplayUnitNumber (did))
                name = ns_get_name_from_ioreg (obj);
            }

          CFRelease (props);
          IOObjectRelease (obj);
        }

      IOObjectRelease (it);
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1090
  else
#endif
#endif /* #if MAC_OS_X_VERSION_MAX_ALLOWED >= 1090 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1090
    name = ns_get_name_from_ioreg (CGDisplayIOServicePort (did));
#endif
  return name;
}
#endif /* NS_IMPL_COCOA */

static Lisp_Object
ns_make_monitor_attribute_list (struct MonitorInfo *monitors,
                                int n_monitors,
                                int primary_monitor,
                                const char *source)
{
  Lisp_Object monitor_frames = make_nil_vector (n_monitors);
  Lisp_Object frame, rest;
  NSArray *screens = [NSScreen screens];
  int i;

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_NS_P (f))
	{
          NSView *view = FRAME_NS_VIEW (f);
          NSScreen *screen = [[view window] screen];
          NSUInteger k;

          i = -1;
          for (k = 0; i == -1 && k < [screens count]; ++k)
            {
              if ([screens objectAtIndex: k] == screen)
                i = (int)k;
            }

          if (i > -1)
            ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
	}
    }

  return make_monitor_attribute_list (monitors, n_monitors, primary_monitor,
                                      monitor_frames, source);
}

DEFUN ("ns-display-monitor-attributes-list",
       Fns_display_monitor_attributes_list,
       Sns_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

In addition to the standard attribute keys listed in
`display-monitor-attributes-list', the following keys are contained in
the attributes:

 source -- String describing the source from which multi-monitor
	   information is obtained, \"NS\" is always the source."

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object terminal)
{
  struct terminal *term = decode_live_terminal (terminal);
  NSArray *screens;
  NSUInteger i, n_monitors;
  struct MonitorInfo *monitors;
  Lisp_Object attributes_list = Qnil;
  CGFloat primary_display_height = 0;

  if (term->type != output_ns)
    return Qnil;

  screens = [NSScreen screens];
  n_monitors = [screens count];
  if (n_monitors == 0)
    return Qnil;

  monitors = xzalloc (n_monitors * sizeof *monitors);

  for (i = 0; i < [screens count]; ++i)
    {
      NSScreen *s = [screens objectAtIndex:i];
      struct MonitorInfo *m = &monitors[i];
      NSRect fr = [s frame];
      NSRect vfr = [s visibleFrame];
      short y, vy;

#ifdef NS_IMPL_COCOA
      NSDictionary *dict = [s deviceDescription];
      NSNumber *nid = [dict objectForKey:@"NSScreenNumber"];
      CGDirectDisplayID did = [nid unsignedIntValue];
#endif
      if (i == 0)
        {
          primary_display_height = fr.size.height;
          y = (short) fr.origin.y;
          vy = (short) vfr.origin.y;
        }
      else
        {
          /* Flip y coordinate as NS screen coordinates originate from
	     the bottom.  */
          y = (short) (primary_display_height - fr.size.height - fr.origin.y);
          vy = (short) (primary_display_height -
                        vfr.size.height - vfr.origin.y);
        }

      m->geom.x = (short) fr.origin.x;
      m->geom.y = y;
      m->geom.width = (unsigned short) fr.size.width;
      m->geom.height = (unsigned short) fr.size.height;

      m->work.x = (short) vfr.origin.x;
      /* y is flipped on NS, so vy - y are pixels missing at the
	 bottom, and fr.size.height - vfr.size.height are pixels
	 missing in total.

	 Pixels missing at top are fr.size.height - vfr.size.height -
	 vy + y.  work.y is then pixels missing at top + y.  */
      m->work.y = (short) (fr.size.height - vfr.size.height) - vy + y + y;
      m->work.width = (unsigned short) vfr.size.width;
      m->work.height = (unsigned short) vfr.size.height;

#ifdef NS_IMPL_COCOA
      m->name = ns_screen_name (did);

      {
        CGSize mms = CGDisplayScreenSize (did);
        m->mm_width = (int) mms.width;
        m->mm_height = (int) mms.height;
      }

#else
      /* Assume 92 dpi as x-display-mm-height and x-display-mm-width
	 do.  */
      m->mm_width = (int) (25.4 * fr.size.width / 92.0);
      m->mm_height = (int) (25.4 * fr.size.height / 92.0);
#endif
    }

  /* Primary monitor is always ordered first for NS.  */
  attributes_list = ns_make_monitor_attribute_list (monitors, n_monitors,
                                                    0, "NS");

  free_monitors (monitors, n_monitors);
  return attributes_list;
}


DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  return make_fixnum
    (NSBitsPerPixelFromDepth ([[[NSScreen screens] objectAtIndex:0] depth]));
}


DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);
  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_fixnum (1 << min (dpyinfo->n_planes, 24));
}

static void
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx,
		Lisp_Object dy, int width, int height, int *root_x,
		int *root_y)
{
  Lisp_Object left, top, right, bottom;
  NSPoint pt;
  NSScreen *screen;

  /* Start with user-specified or mouse position.  */
  left = Fcdr (Fassq (Qleft, parms));
  top = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  if ((!FIXNUMP (left) && !FIXNUMP (right))
      || (!FIXNUMP (top) && !FIXNUMP (bottom)))
    pt = [NSEvent mouseLocation];
  else
    {
      /* Absolute coordinates.  */
      pt.x = FIXNUMP (left) ? XFIXNUM (left) : XFIXNUM (right);
      pt.y = (ns_display_pixel_height (FRAME_DISPLAY_INFO (f))
	      - (FIXNUMP (top) ? XFIXNUM (top) : XFIXNUM (bottom))
	      - height);
    }

  /* Find the screen that pt is on.  */
  for (screen in [NSScreen screens])
    if (pt.x >= screen.frame.origin.x
        && pt.x < screen.frame.origin.x + screen.frame.size.width
        && pt.y >= screen.frame.origin.y
        && pt.y < screen.frame.origin.y + screen.frame.size.height)
      break;

  /* We could use this instead of the if above:

         if (CGRectContainsPoint ([screen frame], pt))

     which would be neater, but it causes problems building on old
     versions of macOS and in GNUstep.  */

  /* Ensure in bounds.  (Note, screen origin = lower left.) */
  if (FIXNUMP (left) || FIXNUMP (right))
    *root_x = pt.x;
  else if (pt.x + XFIXNUM (dx) <= screen.frame.origin.x)
    *root_x = screen.frame.origin.x;
  else if (pt.x + XFIXNUM (dx) + width
	   <= screen.frame.origin.x + screen.frame.size.width)
    /* It fits to the right of the pointer.  */
    *root_x = pt.x + XFIXNUM (dx);
  else if (width + XFIXNUM (dx) <= pt.x)
    /* It fits to the left of the pointer.  */
    *root_x = pt.x - width - XFIXNUM (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = screen.frame.origin.x;

  if (FIXNUMP (top) || FIXNUMP (bottom))
    *root_y = pt.y;
  else if (pt.y - XFIXNUM (dy) - height >= screen.frame.origin.y)
    /* It fits below the pointer.  */
    *root_y = pt.y - height - XFIXNUM (dy);
  else if (pt.y + XFIXNUM (dy) + height
	   <= screen.frame.origin.y + screen.frame.size.height)
    /* It fits above the pointer.  */
      *root_y = pt.y + XFIXNUM (dy);
  else
    /* Put it on the top.  */
    *root_y = screen.frame.origin.y + screen.frame.size.height - height;
}

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
ns_create_tip_frame (struct ns_display_info *dpyinfo, Lisp_Object parms)
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
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  frame = Qnil;
  f = make_frame (false);
  f->wants_modeline = false;
  XSETFRAME (frame, f);
  record_unwind_protect (unwind_create_tip_frame, frame);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_ns;
  f->output_data.ns = xzalloc (sizeof *f->output_data.ns);
  f->tooltip = true;

  FRAME_FONTSET (f) = -1;
  FRAME_DISPLAY_INFO (f) = dpyinfo;

  block_input ();
#ifdef NS_IMPL_COCOA
  mac_register_font_driver (f);
#else
  register_font_driver (&nsfont_driver, f);
#endif
  unblock_input ();

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  {
#ifdef NS_IMPL_COCOA
    /* use for default font name */
    id font = [NSFont userFixedPitchFontOfSize: -1.0]; /* default */
    gui_default_parameter (f, parms, Qfontsize,
                           make_fixnum (0 /* (int)[font pointSize] */),
                           "fontSize", "FontSize", RES_TYPE_NUMBER);
    // Remove ' Regular', not handled by backends.
    char *fontname = xstrdup ([[font displayName] UTF8String]);
    int len = strlen (fontname);
    if (len > 8 && strcmp (fontname + len - 8, " Regular") == 0)
      fontname[len-8] = '\0';
    gui_default_parameter (f, parms, Qfont,
                           build_string (fontname),
                           "font", "Font", RES_TYPE_STRING);
    xfree (fontname);
#else
    gui_default_parameter (f, parms, Qfont,
                           build_string ("fixed"),
                           "font", "Font", RES_TYPE_STRING);
#endif
  }

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
      if (! EQ (value, Qunbound))
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

  f->output_data.ns->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;

  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  gui_figure_window_size (f, parms, false, false);

  block_input ();
  [[EmacsView alloc] initFrameFromEmacs: f];
  ns_icon (f, parms);
  unblock_input ();

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
                         "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
                         "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
                         "cursorType", "CursorType", RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qalpha, Qnil,
                         "alpha", "Alpha", RES_TYPE_NUMBER);

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
    Lisp_Object disptype = intern ("color");

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
     attribute of the frame gets set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    call2 (Qface_set_after_frame_default, frame, Qnil);

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
x_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      call1 (intern ("cancel-timer"), tip_timer);
      tip_timer = Qnil;
    }

  if (!(ns_tooltip == nil || ![ns_tooltip isActive]))
    {
      [ns_tooltip hide];
      tip_last_frame = Qnil;
      return Qt;
    }

  if ((NILP (tip_last_frame) && NILP (tip_frame))
      || (!use_system_tooltips
	  && !delete
	  && !NILP (tip_frame)
	  && FRAME_LIVE_P (XFRAME (tip_frame))
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    /* Either there's no tooltip to hide or it's an already invisible
       Emacs tooltip and we don't want to change its type.  Return
       quickly.  */
    return Qnil;
  else
    {
      specpdl_ref count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      /* Now look whether there's an Emacs tip around.  */
      if (!NILP (tip_frame))
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
		ns_make_frame_invisible (f);

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
  int root_x, root_y;
  specpdl_ref count = SPECPDL_INDEX ();
  struct frame *f, *tip_f;
  struct window *w;
  struct buffer *old_buffer;
  struct text_pos pos;
  int width, height;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  specpdl_ref count_1;
  Lisp_Object window, size, tip_buf;
  char *str;
  NSWindow *nswindow;
  bool displayed;
#ifdef ENABLE_CHECKING
  struct glyph_row *row, *end;
#endif

  AUTO_STRING (tip, " *tip*");

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);
  str = SSDATA (string);
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

  if (use_system_tooltips)
    {
      NSSize size;
      NSColor *color;
      Lisp_Object t;

      block_input ();
      if (ns_tooltip == nil)
	ns_tooltip = [[EmacsTooltip alloc] init];
      else
	Fx_hide_tip ();

      t = gui_display_get_arg (NULL, parms, Qbackground_color, NULL, NULL,
			       RES_TYPE_STRING);
      if (ns_lisp_to_color (t, &color) == 0)
	[ns_tooltip setBackgroundColor: color];

      t = gui_display_get_arg (NULL, parms, Qforeground_color, NULL, NULL,
			       RES_TYPE_STRING);
      if (ns_lisp_to_color (t, &color) == 0)
	[ns_tooltip setForegroundColor: color];

      [ns_tooltip setText: str];
      size = [ns_tooltip frame].size;

      /* Move the tooltip window where the mouse pointer is.  Resize and
	 show it.  */
      compute_tip_xy (f, parms, dx, dy, (int) size.width, (int) size.height,
		      &root_x, &root_y);

      [ns_tooltip showAtX: root_x Y: root_y for: XFIXNUM (timeout)];
      unblock_input ();
    }
  else
    {
      if (!NILP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
	{
	  if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	      && !NILP (Fequal_including_properties (tip_last_string, string))
	      && !NILP (Fequal (tip_last_parms, parms)))
	    {
	      /* Only DX and DY have changed.  */
	      tip_f = XFRAME (tip_frame);
	      if (!NILP (tip_timer))
		{
		  call1 (intern ("cancel-timer"), tip_timer);
		  tip_timer = Qnil;
		}

	      nswindow = [FRAME_NS_VIEW (tip_f) window];

	      block_input ();
	      compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			      FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);
	      [nswindow setFrame: NSMakeRect (root_x, root_y,
					      FRAME_PIXEL_WIDTH (tip_f),
					      FRAME_PIXEL_HEIGHT (tip_f))
			 display: YES];
	      [nswindow setLevel: NSPopUpMenuWindowLevel];
	      [nswindow orderFront: NSApp];
	      [nswindow display];

	      SET_FRAME_VISIBLE (tip_f, 1);
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
			  call2 (intern ("assq-delete-all"), parm, tip_last_parms);
		    }
		  else
		    tip_last_parms =
		      call2 (intern ("assq-delete-all"), parm, tip_last_parms);
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

	      x_hide_tip (delete);
	    }
	  else
	    x_hide_tip (true);
	}
      else
	x_hide_tip (true);

      tip_last_frame = frame;
      tip_last_string = string;
      tip_last_parms = parms;

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
	    parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
	  if (NILP (Fassq (Qbackground_color, parms)))
	    parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
			   parms);

	  /* Create a frame for the tooltip, and record it in the global
	     variable tip_frame.  */
	  if (NILP (tip_frame = ns_create_tip_frame (FRAME_DISPLAY_INFO (f), parms)))
	    /* Creating the tip frame failed.  */
	    return unbind_to (count, Qnil);
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
      FRAME_TOTAL_COLS (tip_f) = w->total_cols;
      adjust_frame_glyphs (tip_f);

      /* Insert STRING into root window's buffer and fit the frame to the
	 buffer.  */
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

      /* Calculate position of tooltip frame.  */
      compute_tip_xy (tip_f, parms, dx, dy, width,
		      height, &root_x, &root_y);

      block_input ();
      nswindow = [FRAME_NS_VIEW (tip_f) window];
      [nswindow setFrame: NSMakeRect (root_x, root_y,
				      width, height)
		 display: YES];
      [nswindow setLevel: NSPopUpMenuWindowLevel];
      [nswindow orderFront: NSApp];
      [nswindow display];

      SET_FRAME_VISIBLE (tip_f, YES);
      FRAME_PIXEL_WIDTH (tip_f) = width;
      FRAME_PIXEL_HEIGHT (tip_f) = height;
      unblock_input ();

      w->must_be_updated_p = true;
      update_single_window (w);
      flush_frame (tip_f);
      set_buffer_internal_1 (old_buffer);
      unbind_to (count_1, Qnil);
      windows_or_buffers_changed = old_windows_or_buffers_changed;

    start_timer:
      /* Let the tip disappear after timeout seconds.  */
      tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
			 intern ("x-hide-tip"));
    }

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
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
  int title_height = fullscreen ? 0 : FRAME_NS_TITLEBAR_HEIGHT (f);
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
    return list4i (f->left_pos, f->top_pos,
		   f->left_pos + outer_width,
		   f->top_pos + outer_height);
  else if (EQ (attribute, Qnative_edges))
    return list4i (native_left, native_top,
		   native_right, native_bottom);
  else if (EQ (attribute, Qinner_edges))
    return list4i (native_left + internal_border_width,
		   native_top + tool_bar_height + internal_border_width,
		   native_right - internal_border_width,
		   native_bottom - internal_border_width);
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

DEFUN ("ns-frame-geometry", Fns_frame_geometry, Sns_frame_geometry, 0, 1, 0,
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

DEFUN ("ns-frame-edges", Fns_frame_edges, Sns_frame_edges, 0, 2, 0,
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

DEFUN ("ns-set-mouse-absolute-pixel-position",
       Fns_set_mouse_absolute_pixel_position,
       Sns_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to absolute pixel position (X, Y).
The coordinates X and Y are interpreted in pixels relative to a position
\(0, 0) of the selected frame's display.  */)
       (Lisp_Object x, Lisp_Object y)
{
#ifdef NS_IMPL_COCOA
  /* GNUstep doesn't support CGWarpMouseCursorPosition, so none of
     this will work.  */
  struct frame *f = SELECTED_FRAME ();
  EmacsView *view = FRAME_NS_VIEW (f);
  NSScreen *screen = [[view window] screen];
  NSRect screen_frame = [screen frame];
  int mouse_x, mouse_y;

  NSScreen *primary_screen = [[NSScreen screens] objectAtIndex:0];
  NSRect primary_screen_frame = [primary_screen frame];
  CGFloat primary_screen_height = primary_screen_frame.size.height;

  if (FRAME_INITIAL_P (f) || !FRAME_NS_P (f))
    return Qnil;

  int xval = check_integer_range (x, INT_MIN, INT_MAX);
  int yval = check_integer_range (y, INT_MIN, INT_MAX);

  mouse_x = screen_frame.origin.x + xval;

  if (screen == primary_screen)
    mouse_y = screen_frame.origin.y + yval;
  else
    mouse_y = (primary_screen_height - screen_frame.size.height
               - screen_frame.origin.y) + yval;

  CGPoint mouse_pos = CGPointMake(mouse_x, mouse_y);
  CGWarpMouseCursorPosition (mouse_pos);
#endif /* NS_IMPL_COCOA */

  return Qnil;
}

DEFUN ("ns-mouse-absolute-pixel-position",
       Fns_mouse_absolute_pixel_position,
       Sns_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the
coordinates of the mouse cursor position in pixels relative to a
position (0, 0) of the selected frame's terminal.  */)
     (void)
{
  struct frame *f = SELECTED_FRAME ();
  EmacsView *view = FRAME_NS_VIEW (f);
  NSScreen *screen = [[view window] screen];
  NSPoint pt = [NSEvent mouseLocation];

  return Fcons(make_fixnum(pt.x - screen.frame.origin.x),
               make_fixnum(screen.frame.size.height -
                           (pt.y - screen.frame.origin.y)));
}

DEFUN ("ns-show-character-palette",
       Fns_show_character_palette,
       Sns_show_character_palette, 0, 0, 0,
       doc: /* Show the macOS character palette.  */)
       (void)
{
  struct frame *f = SELECTED_FRAME ();
  EmacsView *view = FRAME_NS_VIEW (f);
  [NSApp orderFrontCharacterPalette:view];

  return Qnil;
}

/* ==========================================================================

    Class implementations

   ========================================================================== */

@implementation EmacsFileDelegate
/* --------------------------------------------------------------------------
   Delegate methods for Open/Save panels
   -------------------------------------------------------------------------- */
- (BOOL)panel: (id)sender isValidFilename: (NSString *)filename
{
  return YES;
}
- (BOOL)panel: (id)sender shouldShowFilename: (NSString *)filename
{
  return YES;
}
- (NSString *)panel: (id)sender userEnteredFilename: (NSString *)filename
          confirmed: (BOOL)okFlag
{
  return filename;
}
@end

#endif


/* Whether N bytes at STR are in the [1,127] range.  */
static bool
all_nonzero_ascii (unsigned char *str, ptrdiff_t n)
{
  for (ptrdiff_t i = 0; i < n; i++)
    if (str[i] < 1 || str[i] > 127)
      return false;
  return true;
}

@implementation NSString (EmacsString)
/* Make an NSString from a Lisp string.  STRING must not be in an
   encoded form (e.g. UTF-8).  */
+ (NSString *)stringWithLispString:(Lisp_Object)string
{
  if (!STRINGP (string))
    return nil;

  /* Shortcut for the common case.  */
  if (all_nonzero_ascii (SDATA (string), SBYTES (string)))
    return [NSString stringWithCString: SSDATA (string)
                              encoding: NSASCIIStringEncoding];
  string = string_to_multibyte (string);

  /* Now the string is multibyte; convert to UTF-16.  */
  unichar *chars = xmalloc (4 * SCHARS (string));
  unichar *d = chars;
  const unsigned char *s = SDATA (string);
  const unsigned char *end = s + SBYTES (string);
  while (s < end)
    {
      int c = string_char_advance (&s);
      /* We pass unpaired surrogates through, because they are typically
         handled fairly well by the NS libraries (displayed with distinct
         glyphs etc).  */
      if (c <= 0xffff)
        *d++ = c;
      else if (c <= 0x10ffff)
        {
          *d++ = 0xd800 + ((c - 0x10000) >> 10);
          *d++ = 0xdc00 + (c & 0x3ff);
        }
      else
        *d++ = 0xfffd;          /* Not valid for UTF-16.  */
    }
  NSString *str = [NSString stringWithCharacters: chars
                                          length: d - chars];
  xfree (chars);
  return str;
}

/* Make a Lisp string from an NSString.  */
- (Lisp_Object)lispString
{
  return build_string ([self UTF8String]);
}
@end

void
ns_move_tooltip_to_mouse_location (NSPoint screen_point)
{
  int root_x, root_y;
  NSSize size;
  NSWindow *window;
  struct frame *tip_f;

  window = nil;

  if (!FIXNUMP (tip_dx) || !FIXNUMP (tip_dy))
    return;

  if (ns_tooltip)
    size = [ns_tooltip frame].size;
  else if (!FRAMEP (tip_frame)
	   || !FRAME_LIVE_P (XFRAME (tip_frame))
	   || !FRAME_VISIBLE_P (XFRAME (tip_frame)))
    return;
  else
    {
      tip_f = XFRAME (tip_frame);
      window = [FRAME_NS_VIEW (tip_f) window];
      size = [window frame].size;
    }

  root_x = screen_point.x;
  root_y = screen_point.y;

  /* We can directly use `compute_tip_xy' here, since it doesn't cons
     nearly as much as it does on X.  */
  compute_tip_xy (NULL, Qnil, tip_dx, tip_dy, (int) size.width,
		  (int) size.height, &root_x, &root_y);

  if (ns_tooltip)
    [ns_tooltip moveTo: NSMakePoint (root_x, root_y)];
  else
    [window setFrame: NSMakeRect (root_x, root_y,
				  size.width, size.height)
	     display: YES];
}

/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */

void
syms_of_nsfns (void)
{
  DEFSYM (Qfontsize, "fontsize");
  DEFSYM (Qframe_title_format, "frame-title-format");
  DEFSYM (Qicon_title_format, "icon-title-format");
  DEFSYM (Qdark, "dark");
  DEFSYM (Qlight, "light");

  DEFVAR_LISP ("ns-icon-type-alist", Vns_icon_type_alist,
               doc: /* Alist of elements (REGEXP . IMAGE) for images of icons associated to frames.
If the title of a frame matches REGEXP, then IMAGE.tiff is
selected as the image of the icon representing the frame when it's
miniaturized.  If an element is t, then Emacs tries to select an icon
based on the filetype of the visited file.

The images have to be installed in a folder called English.lproj in the
Emacs folder.  You have to restart Emacs after installing new icons.

Example: Install an icon Gnus.tiff and execute the following code

  (setq ns-icon-type-alist
        (append ns-icon-type-alist
                \\='((\"^\\\\*\\\\(Group\\\\*$\\\\|Summary \\\\|Article\\\\*$\\\\)\"
                   . \"Gnus\"))))

When you miniaturize a Group, Summary or Article frame, Gnus.tiff will
be used as the image of the icon representing the frame.  */);
  Vns_icon_type_alist = list1 (Qt);

  DEFVAR_LISP ("ns-version-string", Vns_version_string,
               doc: /* Toolkit version for NS Windowing.  */);
  Vns_version_string = ns_appkit_version_str ();

  DEFVAR_BOOL ("ns-use-proxy-icon", ns_use_proxy_icon,
               doc: /* When non-nil display a proxy icon in the titlebar.
Default is t.  */);
  ns_use_proxy_icon = true;

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
    doc: /* SKIP: real doc in xfns.c.  */);
  Vx_max_tooltip_size = Qnil;

  defsubr (&Sns_read_file_name);
  defsubr (&Sns_get_resource);
  defsubr (&Sns_set_resource);
  defsubr (&Sxw_display_color_p); /* this and next called directly by C code */
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sns_font_name);
  defsubr (&Sns_list_colors);
#ifdef NS_IMPL_COCOA
  defsubr (&Sns_do_applescript);
#endif
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sns_display_monitor_attributes_list);
  defsubr (&Sns_frame_geometry);
  defsubr (&Sns_frame_edges);
  defsubr (&Sns_frame_list_z_order);
  defsubr (&Sns_frame_restack);
  defsubr (&Sns_set_mouse_absolute_pixel_position);
  defsubr (&Sns_mouse_absolute_pixel_position);
  defsubr (&Sns_show_character_palette);
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

  defsubr (&Sns_hide_others);
  defsubr (&Sns_hide_emacs);
  defsubr (&Sns_emacs_info_panel);
  defsubr (&Sns_list_services);
  defsubr (&Sns_perform_service);
  defsubr (&Sx_select_font);
  defsubr (&Sns_popup_color_panel);

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);

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

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1080
  defsubr (&Ssystem_move_file_to_trash);
#endif

  as_status = 0;
  as_script = Qnil;
  staticpro (&as_script);
  as_result = 0;
}
