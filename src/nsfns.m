/* Functions for the NeXT/Open/GNUstep and MacOSX window system.

Copyright (C) 1989, 1992-1994, 2005-2006, 2008-2015 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
MacOSX/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
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
#endif

#if 0
int fns_trace_num = 1;
#define NSTRACE(x)        fprintf (stderr, "%s:%d: [%d] " #x "\n",        \
                                  __FILE__, __LINE__, ++fns_trace_num)
#else
#define NSTRACE(x)
#endif

#ifdef HAVE_NS

extern NSArray *ns_send_types, *ns_return_types, *ns_drag_types;

EmacsTooltip *ns_tooltip = nil;

/* Need forward declaration here to preserve organizational integrity of file */
Lisp_Object Fx_open_connection (Lisp_Object, Lisp_Object, Lisp_Object);

/* Static variables to handle applescript execution.  */
static Lisp_Object as_script, *as_result;
static int as_status;

static ptrdiff_t image_cache_refcount;


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
    maybeFrame = selected_frame;/*wrong_type_argument (Qframep, maybeFrame); */

  if (!NILP (maybeFrame))
    view = FRAME_NS_VIEW (XFRAME (maybeFrame));
  if (view) window =[view window];

  return window;
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */
struct ns_display_info *
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

static Lisp_Object
interpret_services_menu (NSMenu *menu, Lisp_Object prefix, Lisp_Object old)
/* --------------------------------------------------------------------------
   Turn the input menu (an NSMenu) into a lisp list for tracking on lisp side
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
              res = make_number (key|super_modifier);
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



/* ==========================================================================

    Frame parameter setters

   ========================================================================== */


static void
x_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSColor *col;
  EmacsCGFloat r, g, b, alpha;

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

  [col getRed: &r green: &g blue: &b alpha: &alpha];
  FRAME_FOREGROUND_PIXEL (f) =
    ARGB_TO_ULONG ((int)(alpha*0xff), (int)(r*0xff), (int)(g*0xff), (int)(b*0xff));

  if (FRAME_NS_VIEW (f))
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
  struct face *face;
  NSColor *col;
  NSView *view = FRAME_NS_VIEW (f);
  EmacsCGFloat r, g, b, alpha;

  block_input ();
  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qbackground_color, oldval);
      unblock_input ();
      error ("Unknown color");
    }

  /* clear the frame; in some instances the NS-internal GC appears not to
     update, or it does update and cannot clear old text properly */
  if (FRAME_VISIBLE_P (f))
    ns_clear_frame (f);

  [col retain];
  [f->output_data.ns->background_color release];
  f->output_data.ns->background_color = col;

  [col getRed: &r green: &g blue: &b alpha: &alpha];
  FRAME_BACKGROUND_PIXEL (f) =
    ARGB_TO_ULONG ((int)(alpha*0xff), (int)(r*0xff), (int)(g*0xff), (int)(b*0xff));

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
          col = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f);
          face->background = ns_index_color
            ([col colorWithAlphaComponent: alpha], f);

          update_face_from_frame_parameter (f, Qbackground_color, arg);
        }

      if (FRAME_VISIBLE_P (f))
        SET_FRAME_GARBAGED (f);
    }
  unblock_input ();
}


static void
x_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
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
      x_update_cursor (f, 0);
      x_update_cursor (f, 1);
    }
  update_face_from_frame_parameter (f, Qcursor_color, arg);
  unblock_input ();
}


static void
x_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE (x_set_icon_name);

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
            windows_or_buffers_changed = 62;
            return;
          }
    }

  /* Don't change the name if it's already NAME.  */
  if ([[view window] miniwindowTitle]
      && ([[[view window] miniwindowTitle]
             isEqualToString: [NSString stringWithUTF8String:
					  SSDATA (arg)]]))
    return;

  [[view window] setMiniwindowTitle:
        [NSString stringWithUTF8String: SSDATA (arg)]];
}

static void
ns_set_name_internal (struct frame *f, Lisp_Object name)
{
  Lisp_Object encoded_name, encoded_icon_name;
  NSString *str;
  NSView *view = FRAME_NS_VIEW (f);

  encoded_name = ENCODE_UTF_8 (name);

  str = [NSString stringWithUTF8String: SSDATA (encoded_name)];

  /* Don't change the name if it's already NAME.  */
  if (! [[[view window] title] isEqualToString: str])
    [[view window] setTitle: str];

  if (!STRINGP (f->icon_name))
    encoded_icon_name = encoded_name;
  else
    encoded_icon_name = ENCODE_UTF_8 (f->icon_name);

  str = [NSString stringWithUTF8String: SSDATA (encoded_icon_name)];

  if ([[view window] miniwindowTitle]
      && ! [[[view window] miniwindowTitle] isEqualToString: str])
    [[view window] setMiniwindowTitle: str];

}

static void
ns_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  NSTRACE (ns_set_name);

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
    name = build_string ([ns_app_name UTF8String]);
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


/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
x_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE (x_explicitly_set_name);
  ns_set_name (f, arg, 1);
}


/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE (x_implicitly_set_name);

  /* Deal with NS specific format t.  */
  if (FRAME_NS_P (f) && ((FRAME_ICONIFIED_P (f) && EQ (Vicon_title_format, Qt))
                         || EQ (Vframe_title_format, Qt)))
    ns_set_name_as_filename (f);
  else
    ns_set_name (f, arg, 0);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
x_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  NSTRACE (x_set_title);
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
ns_set_name_as_filename (struct frame *f)
{
  NSView *view;
  Lisp_Object name, filename;
  Lisp_Object buf = XWINDOW (f->selected_window)->contents;
  const char *title;
  NSAutoreleasePool *pool;
  Lisp_Object encoded_name, encoded_filename;
  NSString *str;
  NSTRACE (ns_set_name_as_filename);

  if (f->explicit_name || ! NILP (f->title))
    return;

  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  filename = BVAR (XBUFFER (buf), filename);
  name = BVAR (XBUFFER (buf), name);

  if (NILP (name))
    {
      if (! NILP (filename))
        name = Ffile_name_nondirectory (filename);
      else
        name = build_string ([ns_app_name UTF8String]);
    }

  encoded_name = ENCODE_UTF_8 (name);

  view = FRAME_NS_VIEW (f);

  title = FRAME_ICONIFIED_P (f) ? [[[view window] miniwindowTitle] UTF8String]
                                : [[[view window] title] UTF8String];

  if (title && (! strcmp (title, SSDATA (encoded_name))))
    {
      [pool release];
      unblock_input ();
      return;
    }

  str = [NSString stringWithUTF8String: SSDATA (encoded_name)];
  if (str == nil) str = @"Bad coding";

  if (FRAME_ICONIFIED_P (f))
    [[view window] setMiniwindowTitle: str];
  else
    {
      NSString *fstr;

      if (! NILP (filename))
        {
          encoded_filename = ENCODE_UTF_8 (filename);

          fstr = [NSString stringWithUTF8String: SSDATA (encoded_filename)];
          if (fstr == nil) fstr = @"";
        }
      else
        fstr = @"";

      ns_set_represented_filename (fstr, f);
      [[view window] setTitle: str];
      fset_name (f, name);
    }

  [pool release];
  unblock_input ();
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


void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (TYPE_RANGED_INTEGERP (int, value))
    nlines = XINT (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      /* does for all frames, whereas we just want for one frame
	 [NSMenu setMenuBarVisible: YES]; */
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
        free_frame_menubar (f);
      /*      [NSMenu setMenuBarVisible: NO]; */
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
    }
}


/* toolbar support */
void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (RANGED_INTEGERP (0, value, INT_MAX))
    nlines = XFASTINT (value);
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
        }
    }

  x_set_window_size (f, 0, f->text_cols, f->text_lines, 0);
}


void
x_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int old_width = FRAME_INTERNAL_BORDER_WIDTH (f);

  CHECK_TYPE_RANGED_INTEGER (int, arg);
  FRAME_INTERNAL_BORDER_WIDTH (f) = XINT (arg);
  if (FRAME_INTERNAL_BORDER_WIDTH (f) < 0)
    FRAME_INTERNAL_BORDER_WIDTH (f) = 0;

  if (FRAME_INTERNAL_BORDER_WIDTH (f) == old_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
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

  NSTRACE (ns_implicitly_set_icon_type);

  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  if (f->output_data.ns->miniimage
      && [[NSString stringWithUTF8String: SSDATA (f->name)]
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
      /* special case: t means go by file type */
      if (SYMBOLP (elt) && EQ (elt, Qt) && SSDATA (f->name)[0] == '/')
        {
          NSString *str
	     = [NSString stringWithUTF8String: SSDATA (f->name)];
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
                               [NSString stringWithUTF8String:
					    SSDATA (XCDR (elt))]] retain];
        }
    }

  if (image == nil)
    {
      image = [[[NSWorkspace sharedWorkspace] iconForFileType: @"text"] retain];
      setMini = NO;
    }

  [f->output_data.ns->miniimage release];
  f->output_data.ns->miniimage = image;
  [view setMiniwindowImage: setMini];
  [pool release];
  unblock_input ();
}


static void
x_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  EmacsView *view = FRAME_NS_VIEW (f);
  id image = nil;
  BOOL setMini = YES;

  NSTRACE (x_set_icon_type);

  if (!NILP (arg) && SYMBOLP (arg))
    {
      arg =build_string (SSDATA (SYMBOL_NAME (arg)));
      store_frame_param (f, Qicon_type, arg);
    }

  /* do it the implicit way */
  if (NILP (arg))
    {
      ns_implicitly_set_icon_type (f);
      return;
    }

  CHECK_STRING (arg);

  image = [EmacsImage allocInitFromFile: arg];
  if (image == nil)
    image =[NSImage imageNamed: [NSString stringWithUTF8String:
                                            SSDATA (arg)]];

  if (image == nil)
    {
      image = [NSImage imageNamed: @"text"];
      setMini = NO;
    }

  f->output_data.ns->miniimage = image;
  [view setMiniwindowImage: setMini];
}


/* TODO: move to nsterm? */
int
ns_lisp_to_cursor_type (Lisp_Object arg)
{
  char *str;
  if (XTYPE (arg) == Lisp_String)
    str = SSDATA (arg);
  else if (XTYPE (arg) == Lisp_Symbol)
    str = SSDATA (SYMBOL_NAME (arg));
  else return -1;
  if (!strcmp (str, "box"))	return FILLED_BOX_CURSOR;
  if (!strcmp (str, "hollow"))	return HOLLOW_BOX_CURSOR;
  if (!strcmp (str, "hbar"))	return HBAR_CURSOR;
  if (!strcmp (str, "bar"))	return BAR_CURSOR;
  if (!strcmp (str, "no"))	return NO_CURSOR;
  return -1;
}


Lisp_Object
ns_cursor_type_to_lisp (int arg)
{
  switch (arg)
    {
    case FILLED_BOX_CURSOR: return Qbox;
    case HOLLOW_BOX_CURSOR: return Qhollow;
    case HBAR_CURSOR:	    return Qhbar;
    case BAR_CURSOR:	    return Qbar;
    case NO_CURSOR:
    default:		    return intern ("no");
    }
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
  /* don't think we can do this on Nextstep */
}


#define Str(x) #x
#define Xstr(x) Str(x)

static Lisp_Object
ns_appkit_version_str (void)
{
  char tmp[256];

#ifdef NS_IMPL_GNUSTEP
  sprintf(tmp, "gnustep-gui-%s", Xstr(GNUSTEP_GUI_VERSION));
#elif defined (NS_IMPL_COCOA)
  NSString *osversion
    = [[NSProcessInfo processInfo] operatingSystemVersionString];
  sprintf(tmp, "appkit-%.2f %s",
          NSAppKitVersionNumber,
          [osversion UTF8String]);
#else
  tmp = "ns-unknown";
#endif
  return build_string (tmp);
}


/* This is for use by x-server-version and collapses all version info we
   have into a single int.  For a better picture of the implementation
   running, use ns_appkit_version_str.*/
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
x_icon (struct frame *f, Lisp_Object parms)
/* --------------------------------------------------------------------------
   Strangely-named function to set icon position parameters in frame.
   This is irrelevant under OS X, but might be needed under GNUstep,
   depending on the window manager used.  Note, this is not a standard
   frame parameter-setter; it is called directly from x-create-frame.
   -------------------------------------------------------------------------- */
{
  Lisp_Object icon_x, icon_y;
  struct ns_display_info *dpyinfo = check_ns_display_info (Qnil);

  f->output_data.ns->icon_top = -1;
  f->output_data.ns->icon_left = -1;

  /* Set the position of the icon.  */
  icon_x = x_get_arg (dpyinfo, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_get_arg (dpyinfo, parms, Qicon_top, 0, 0,  RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
      f->output_data.ns->icon_top = XINT (icon_y);
      f->output_data.ns->icon_left = XINT (icon_x);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}


/* Note: see frame.c for template, also where generic functions are impl */
frame_parm_handler ns_frame_parm_handlers[] =
{
  x_set_autoraise, /* generic OK */
  x_set_autolower, /* generic OK */
  x_set_background_color,
  0, /* x_set_border_color,  may be impossible under Nextstep */
  0, /* x_set_border_width,  may be impossible under Nextstep */
  x_set_cursor_color,
  x_set_cursor_type,
  x_set_font, /* generic OK */
  x_set_foreground_color,
  x_set_icon_name,
  x_set_icon_type,
  x_set_internal_border_width, /* generic OK */
  0, /* x_set_right_divider_width */
  0, /* x_set_bottom_divider_width */
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  x_set_scroll_bar_width, /* generic OK */
  x_set_scroll_bar_height, /* generic OK */
  x_set_title,
  x_set_unsplittable, /* generic OK */
  x_set_vertical_scroll_bars, /* generic OK */
  x_set_horizontal_scroll_bars, /* generic OK */
  x_set_visibility, /* generic OK */
  x_set_tool_bar_lines,
  0, /* x_set_scroll_bar_foreground, will ignore (not possible on NS) */
  0, /* x_set_scroll_bar_background,  will ignore (not possible on NS) */
  x_set_screen_gamma, /* generic OK */
  x_set_line_spacing, /* generic OK, sets f->extra_line_spacing to int */
  x_set_left_fringe, /* generic OK */
  x_set_right_fringe, /* generic OK */
  0, /* x_set_wait_for_wm, will ignore */
  x_set_fullscreen, /* generic OK */
  x_set_font_backend, /* generic OK */
  x_set_alpha,
  0, /* x_set_sticky */
  0, /* x_set_tool_bar_position */
};


/* Handler for signals raised during x_create_frame.
   FRAME is the frame which is partially constructed.  */

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
      struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

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

#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      /* Check that reference counts are indeed correct.  */
      eassert (dpyinfo->terminal->image_cache->refcount == image_cache_refcount);
#endif
    }
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
            = x_get_arg (dpyinfo, parms, r[i].tem, r[i].val, r[i].cls,
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
       doc: /* Make a new Nextstep window, called a "frame" in Emacs terms.
Return an Emacs frame object.
PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use,
then `default-minibuffer-frame' must be a frame whose minibuffer can
be shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
     (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  ptrdiff_t count = specpdl_ptr - specpdl;
  Lisp_Object display;
  struct ns_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;
  static int desc_ctr = 1;

  /* x_get_arg modifies parms.  */
  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = x_get_arg (dpyinfo, parms, Qterminal, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_ns_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = x_get_arg (dpyinfo, parms, Qname, 0, 0, RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  parent = x_get_arg (dpyinfo, parms, Qparent_id, 0, 0, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  tem = x_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
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

  fset_icon_name (f, x_get_arg (dpyinfo, parms, Qicon_name,
				"iconName", "Title",
				RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (unwind_create_frame, frame);

  f->output_data.ns->window_desc = desc_ctr++;
  if (TYPE_RANGED_INTEGERP (Window, parent))
    {
      f->output_data.ns->parent_desc = XFASTINT (parent);
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
      fset_name (f, build_string ([ns_app_name UTF8String]));
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

  x_default_parameter (f, parms, Qfont_backend, Qnil,
			"fontBackend", "FontBackend", RES_TYPE_STRING);

  {
    /* use for default font name */
    id font = [NSFont userFixedPitchFontOfSize: -1.0]; /* default */
    x_default_parameter (f, parms, Qfontsize,
                                    make_number (0 /*(int)[font pointSize]*/),
                                    "fontSize", "FontSize", RES_TYPE_NUMBER);
    // Remove ' Regular', not handled by backends.
    char *fontname = xstrdup ([[font displayName] UTF8String]);
    int len = strlen (fontname);
    if (len > 8 && strcmp (fontname + len - 8, " Regular") == 0)
      fontname[len-8] = '\0';
    x_default_parameter (f, parms, Qfont,
                                 build_string (fontname),
                                 "font", "Font", RES_TYPE_STRING);
    xfree (fontname);
  }
  unblock_input ();

  x_default_parameter (f, parms, Qborder_width, make_number (0),
		       "borderwidth", "BorderWidth", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qinternal_border_width, make_number (2),
                      "internalBorderWidth", "InternalBorderWidth",
                      RES_TYPE_NUMBER);

  /* default vertical scrollbars on right on Mac */
  {
      Lisp_Object spos
#ifdef NS_IMPL_GNUSTEP
          = Qt;
#else
          = Qright;
#endif
      x_default_parameter (f, parms, Qvertical_scroll_bars, spos,
			   "verticalScrollBars", "VerticalScrollBars",
			   RES_TYPE_SYMBOL);
  }
  x_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil,
		       "horizontalScrollBars", "HorizontalScrollBars",
		       RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qforeground_color, build_string ("Black"),
                      "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("White"),
                      "background", "Background", RES_TYPE_STRING);
  /* FIXME: not supported yet in Nextstep */
  x_default_parameter (f, parms, Qline_spacing, Qnil,
		       "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qleft_fringe, Qnil,
		       "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qright_fringe, Qnil,
		       "rightFringe", "RightFringe", RES_TYPE_NUMBER);

  init_frame_faces (f);

  /* Read comment about this code in corresponding place in xfns.c.  */
  adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		     FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 5, 1,
		     Qx_create_frame_1);

  /* The resources controlling the menu-bar and tool-bar are
     processed specially at startup, and reflected in the mode
     variables; ignore them here.  */
  x_default_parameter (f, parms, Qmenu_bar_lines,
		       NILP (Vmenu_bar_mode)
		       ? make_number (0) : make_number (1),
		       NULL, NULL, RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qtool_bar_lines,
		       NILP (Vtool_bar_mode)
		       ? make_number (0) : make_number (1),
		       NULL, NULL, RES_TYPE_NUMBER);

  x_default_parameter (f, parms, Qbuffer_predicate, Qnil, "bufferPredicate",
                       "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil, "title", "Title",
                       RES_TYPE_STRING);

  parms = get_geometry_from_preferences (dpyinfo, parms);
  window_prompting = x_figure_window_size (f, parms, 1);

  tem = x_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || (!EQ (tem, Qunbound) && !EQ (tem, Qnil));

  /* NOTE: on other terms, this is done in set_mouse_color, however this
     was not getting called under Nextstep */
  f->output_data.ns->text_cursor = [NSCursor IBeamCursor];
  f->output_data.ns->nontext_cursor = [NSCursor arrowCursor];
  f->output_data.ns->modeline_cursor = [NSCursor pointingHandCursor];
  f->output_data.ns->hand_cursor = [NSCursor pointingHandCursor];
  f->output_data.ns->hourglass_cursor = [NSCursor disappearingItemCursor];
  f->output_data.ns->horizontal_drag_cursor = [NSCursor resizeLeftRightCursor];
  f->output_data.ns->vertical_drag_cursor = [NSCursor resizeUpDownCursor];
  FRAME_DISPLAY_INFO (f)->vertical_scroll_bar_cursor
     = [NSCursor arrowCursor];
  FRAME_DISPLAY_INFO (f)->horizontal_scroll_bar_cursor
     = [NSCursor arrowCursor];
  f->output_data.ns->current_pointer = f->output_data.ns->text_cursor;

  [[EmacsView alloc] initFrameFromEmacs: f];

  x_icon (f, parms);

  /* ns_display_info does not have a reference_count.  */
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error below.
     The frame needs to be on Vframe_list or making it visible won't work. */
  Vframe_list = Fcons (frame, Vframe_list);

  x_default_parameter (f, parms, Qicon_type, Qnil,
                       "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
                       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
                       "autoLower", "AutoLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
                       "cursorType", "CursorType", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qscroll_bar_width, Qnil,
                       "scrollBarWidth", "ScrollBarWidth",
                       RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qscroll_bar_height, Qnil,
                       "scrollBarHeight", "ScrollBarHeight",
                       RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qalpha, Qnil,
                       "alpha", "Alpha", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  /* Allow x_set_window_size, now.  */
  f->can_x_set_window_size = true;

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f), 0, 1,
		     Qx_create_frame_2);

  if (! f->output_data.ns->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = x_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
                              RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;

      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else if (! NILP (visibility))
	{
	  x_make_frame_visible (f);
	  [[FRAME_NS_VIEW (f) window] makeKeyWindow];
	}
      else
        {
	  /* Must have been Qnil.  */
        }
    }

  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
          || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
    kset_default_minibuffer_frame (kb, frame);

  /* All remaining specified parameters, which have not been "used"
     by x_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  if (window_prompting & USPosition)
    x_set_offset (f, f->left_pos, f->top_pos, 1);

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}

void
x_focus_frame (struct frame *f)
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (dpyinfo->x_focus_frame != f)
    {
      EmacsView *view = FRAME_NS_VIEW (f);
      block_input ();
      [NSApp activateIgnoringOtherApps: YES];
      [[view window] makeKeyAndOrderFront: view];
      unblock_input ();
    }
}


DEFUN ("ns-popup-font-panel", Fns_popup_font_panel, Sns_popup_font_panel,
       0, 1, "",
       doc: /* Pop up the font panel. */)
     (Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);
  id fm = [NSFontManager sharedFontManager];
  struct font *font = f->output_data.ns->font;
  NSFont *nsfont;
#ifdef NS_IMPL_GNUSTEP
  nsfont = ((struct nsfont_info *)font)->nsfont;
#endif
#ifdef NS_IMPL_COCOA
  nsfont = (NSFont *) macfont_get_nsctfont (font);
#endif
  [fm setSelectedFont: nsfont isMultiple: NO];
  [fm orderFrontFontPanel: NSApp];
  return Qnil;
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
#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_9
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
Optional arg DIR_ONLY_P, if non-nil, means choose only directories.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object mustmatch,
   Lisp_Object init, Lisp_Object dir_only_p)
{
  static id fileDelegate = nil;
  BOOL isSave = NILP (mustmatch) && NILP (dir_only_p);
  id panel;
  Lisp_Object fname = Qnil;

  NSString *promptS = NILP (prompt) || !STRINGP (prompt) ? nil :
    [NSString stringWithUTF8String: SSDATA (prompt)];
  NSString *dirS = NILP (dir) || !STRINGP (dir) ?
    [NSString stringWithUTF8String: SSDATA (BVAR (current_buffer, directory))] :
    [NSString stringWithUTF8String: SSDATA (dir)];
  NSString *initS = NILP (init) || !STRINGP (init) ? nil :
    [NSString stringWithUTF8String: SSDATA (init)];
  NSEvent *nxev;

  check_window_system (NULL);

  if (fileDelegate == nil)
    fileDelegate = [EmacsFileDelegate new];

  [NSCursor setHiddenUntilMouseMoves: NO];

  if ([dirS characterAtIndex: 0] == '~')
    dirS = [dirS stringByExpandingTildeInPath];

  panel = isSave ?
    (id)[EmacsSavePanel savePanel] : (id)[EmacsOpenPanel openPanel];

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
    [panel setAllowedFileTypes: nil];
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
  nxev = [NSEvent otherEventWithType: NSApplicationDefined
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
      NSString *str = ns_filename_from_panel (panel);
      if (! str) str = ns_directory_from_panel (panel);
      if (str) fname = build_string ([str UTF8String]);
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
    owner = build_string([ns_app_name UTF8String]);
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
    owner = build_string ([ns_app_name UTF8String]);
  CHECK_STRING (name);
  if (NILP (value))
    {
      [[NSUserDefaults standardUserDefaults] removeObjectForKey:
                         [NSString stringWithUTF8String: SSDATA (name)]];
    }
  else
    {
      CHECK_STRING (value);
      [[NSUserDefaults standardUserDefaults] setObject:
                [NSString stringWithUTF8String: SSDATA (value)]
                                        forKey: [NSString stringWithUTF8String:
                                                         SSDATA (name)]];
    }

  return Qnil;
}


DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* This function is a no-op.  It is only present for completeness.  */)
     (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  /* This function has no real equivalent under NeXTstep.  Return nil to
     indicate this. */
  return Qnil;
}


DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Return the "vendor ID" string of Nextstep display server TERMINAL.
\(Labeling every distributor as a "vendor" embodies the false assumption
that operating systems cannot be developed and distributed noncommercially.)
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
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
       doc: /* Return the version numbers of the server of display TERMINAL.
The value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the distributor-specific release
number.  See also the function `x-server-vendor'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  /*NOTE: it is unclear what would best correspond with "protocol";
          we return 10.3, meaning Panther, since this is roughly the
          level that GNUstep's APIs correspond to.
          The last number is where we distinguish between the Apple
          and GNUstep implementations ("distributor-specific release
          number") and give int'ized versions of major.minor. */
  return list3i (10, 3, ns_appkit_version_int ());
}


DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on Nextstep display server TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

Note: "screen" here is not in Nextstep terminology but in X11's.  For
the number of physical monitors, use `(length
(display-monitor-attributes-list TERMINAL))' instead.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  return make_number (1);
}


DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the height in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_number (x_display_pixel_height (dpyinfo) / (92.0/25.4));
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the width in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_number (x_display_pixel_width (dpyinfo) / (92.0/25.4));
}


DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Return an indication of whether the Nextstep display TERMINAL does backing store.
The value may be `buffered', `retained', or `non-retained'.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  switch ([ns_get_window (terminal) backingType])
    {
    case NSBackingStoreBuffered:
      return intern ("buffered");
    case NSBackingStoreRetained:
      return intern ("retained");
    case NSBackingStoreNonretained:
      return intern ("non-retained");
    default:
      error ("Strange value for backingType parameter of frame");
    }
  return Qnil;  /* not reached, shut compiler up */
}


DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Return the visual class of the Nextstep display TERMINAL.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
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
    /* color mgmt as far as we do it is really handled by Nextstep itself anyway */
    return intern ("direct-color");
}


DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Return t if TERMINAL supports the save-under feature.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  switch ([ns_get_window (terminal) backingType])
    {
    case NSBackingStoreBuffered:
      return Qt;

    case NSBackingStoreRetained:
    case NSBackingStoreNonretained:
      return Qnil;

    default:
      error ("Strange value for backingType parameter of frame");
    }
  return Qnil;  /* not reached, shut compiler up */
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to a display server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.
\(In the Nextstep version, the last two arguments are currently ignored.)  */)
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
       doc: /* Close the connection to TERMINAL's Nextstep display server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame's
terminal.  */)
     (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  [NSApp terminate: NSApp];
  return Qnil;
}


DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
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
the active application.  */)
     (Lisp_Object on)
{
  check_window_system (NULL);
  if (EQ (on, intern ("activate")))
    {
      [NSApp unhide: NSApp];
      [NSApp activateIgnoringOtherApps: YES];
    }
  else if (NILP (on))
    [NSApp unhide: NSApp];
  else
    [NSApp hide: NSApp];
  return Qnil;
}


DEFUN ("ns-emacs-info-panel", Fns_emacs_info_panel, Sns_emacs_info_panel,
       0, 0, 0,
       doc: /* Shows the 'Info' or 'About' panel for Emacs.  */)
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

  if (!NILP (frame))
    {
      CHECK_FRAME (frame);
      if (! FRAME_NS_P (XFRAME (frame)))
        error ("non-Nextstep frame used in `ns-list-colors'");
    }

  block_input ();

  colorlists = [[NSColorList availableColorLists] objectEnumerator];
  while ((clist = [colorlists nextObject]))
    {
      if ([[clist name] length] < 7 ||
          [[clist name] rangeOfString: @"PANTONE"].location == 0)
        {
          NSEnumerator *cnames = [[clist allKeys] reverseObjectEnumerator];
          NSString *cname;
          while ((cname = [cnames nextObject]))
            list = Fcons (build_string ([cname UTF8String]), list);
/*           for (i = [[clist allKeys] count] - 1; i >= 0; i--)
               list = Fcons (build_string ([[[clist allKeys] objectAtIndex: i]
                                             UTF8String]), list); */
        }
    }

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
#ifdef NS_IMPL_COCOA
  id delegate;
#endif

  check_window_system (NULL);
  svcs = [[NSMenu alloc] initWithTitle: @"Services"];
  [NSApp setServicesMenu: svcs];
  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: ns_return_types];

/* On Tiger, services menu updating was made lazier (waits for user to
   actually click on the menu), so we have to force things along: */
#ifdef NS_IMPL_COCOA
  delegate = [svcs delegate];
  if (delegate != nil)
    {
      if ([delegate respondsToSelector: @selector (menuNeedsUpdate:)])
        [delegate menuNeedsUpdate: svcs];
      if ([delegate respondsToSelector:
                       @selector (menu:updateItem:atIndex:shouldCancel:)])
        {
          int i, len = [delegate numberOfItemsInMenu: svcs];
          for (i =0; i<len; i++)
            [svcs addItemWithTitle: @"" action: NULL keyEquivalent: @""];
          for (i =0; i<len; i++)
            if (![delegate menu: svcs
                     updateItem: (NSMenuItem *)[svcs itemAtIndex: i]
                        atIndex: i shouldCancel: NO])
              break;
        }
    }
#endif

  [svcs setAutoenablesItems: NO];
#ifdef NS_IMPL_COCOA
  [svcs update]; /* on OS X, converts from '/' structure */
#endif

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
  char *utfStr;

  CHECK_STRING (service);
  check_window_system (NULL);

  utfStr = SSDATA (service);
  svcName = [NSString stringWithUTF8String: utfStr];

  pb =[NSPasteboard pasteboardWithUniqueName];
  ns_string_to_pasteboard (pb, send);

  if (NSPerformService (svcName, pb) == NO)
    Fsignal (Qquit, list1 (build_string ("service not available")));

  if ([[pb types] count] == 0)
    return build_string ("");
  return ns_string_from_pasteboard (pb);
}


DEFUN ("ns-convert-utf8-nfd-to-nfc", Fns_convert_utf8_nfd_to_nfc,
       Sns_convert_utf8_nfd_to_nfc, 1, 1, 0,
       doc: /* Return an NFC string that matches the UTF-8 NFD string STR.  */)
     (Lisp_Object str)
{
/* TODO: If GNUstep ever implements precomposedStringWithCanonicalMapping,
         remove this. */
  NSString *utfStr;
  Lisp_Object ret = Qnil;
  NSAutoreleasePool *pool;

  CHECK_STRING (str);
  pool = [[NSAutoreleasePool alloc] init];
  utfStr = [NSString stringWithUTF8String: SSDATA (str)];
#ifdef NS_IMPL_COCOA
  if (utfStr)
    utfStr = [utfStr precomposedStringWithCanonicalMapping];
#endif
  if (utfStr)
    {
      const char *cstr = [utfStr UTF8String];
      if (cstr)
        ret = build_string (cstr);
    }

  [pool release];
  if (NILP (ret))
    error ("Invalid UTF-8");

  return ret;
}


#ifdef NS_IMPL_COCOA

/* Compile and execute the AppleScript SCRIPT and return the error
   status as function value.  A zero is returned if compilation and
   execution is successful, in which case *RESULT is set to a Lisp
   string or a number containing the resulting script value.  Otherwise,
   1 is returned. */
static int
ns_do_applescript (Lisp_Object script, Lisp_Object *result)
{
  NSAppleEventDescriptor *desc;
  NSDictionary* errorDict;
  NSAppleEventDescriptor* returnDescriptor = NULL;

  NSAppleScript* scriptObject =
    [[NSAppleScript alloc] initWithSource:
			     [NSString stringWithUTF8String: SSDATA (script)]];

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
		*result = build_string([[desc stringValue] UTF8String]);
	    }
	  else
            {
	      /* use typeUTF16ExternalRepresentation? */
	      // coerce the result to the appropriate ObjC type
	      desc = [returnDescriptor coerceToDescriptorType: typeUTF8Text];
	      if (desc)
		*result = make_number([desc int32Value]);
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

/* Helper function called from sendEvent to run applescript
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
In case the execution fails, an error is signaled. */)
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

  /* executing apple script requires the event loop to run, otherwise
     errors aren't returned and executeAndReturnError hangs forever.
     Post an event that runs applescript and then start the event loop.
     The event loop is exited when the script is done.  */
  nxev = [NSEvent otherEventWithType: NSApplicationDefined
                            location: NSMakePoint (0, 0)
                       modifierFlags: 0
                           timestamp: 0
                        windowNumber: [[NSApp mainWindow] windowNumber]
                             context: [NSApp context]
                             subtype: 0
                               data1: 0
                               data2: NSAPP_DATA2_RUNASSCRIPT];

  [NSApp postEvent: nxev atStart: NO];

  // If there are other events, the event loop may exit.  Keep running
  // until the script has been handled.  */
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
x_set_scroll_bar_default_width (struct frame *f)
{
  int wid = FRAME_COLUMN_WIDTH (f);
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = NS_SCROLL_BAR_WIDTH_DEFAULT;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) +
                                      wid - 1) / wid;
}

void
x_set_scroll_bar_default_height (struct frame *f)
{
  int height = FRAME_LINE_HEIGHT (f);
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = NS_SCROLL_BAR_WIDTH_DEFAULT;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) +
				       height - 1) / height;
}

/* terms impl this instead of x-get-resource directly */
char *
x_get_string_resource (XrmDatabase rdb, const char *name, const char *class)
{
  /* remove appname prefix; TODO: allow for !="Emacs" */
  const char *res, *toCheck = class + (!strncmp (class, "Emacs.", 6) ? 6 : 0);

  check_window_system (NULL);

  if (inhibit_x_resources)
    /* --quick was passed, so this is a no-op.  */
    return NULL;

  res = ns_get_defaults_value (toCheck);
  return (!res ? NULL :
	  (!c_strncasecmp (res, "YES", 3) ? "true" :
	   (!c_strncasecmp (res, "NO", 2) ? "false" : (char *) res)));
}


Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  Lisp_Object nsfocus;

  if (!dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (nsfocus, dpyinfo->x_focus_frame);
  return nsfocus;
}

/* ==========================================================================

    Lisp definitions that, for whatever reason, we can't alias as 'ns-XXX'.

   ========================================================================== */


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see.
\(Note that the Nextstep version of this function ignores FRAME.)  */)
     (Lisp_Object color, Lisp_Object frame)
{
  NSColor * col;
  check_window_system (NULL);
  return ns_lisp_to_color (color, &col) ? Qnil : Qt;
}


DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Internal function called by `color-values', which see.  */)
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
  return list3i (lrint (red * 65280), lrint (green * 65280),
		 lrint (blue * 65280));
}


DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Internal function called by `display-color-p', which see.  */)
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
       doc: /* Return t if the Nextstep display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  NSWindowDepth depth;

  check_ns_display_info (terminal);
  depth = [[[NSScreen screens] objectAtIndex:0] depth];

  return NSBitsPerPixelFromDepth (depth) > 1 ? Qt : Qnil;
}


DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Return the width in pixels of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel width for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_number (x_display_pixel_width (dpyinfo));
}


DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Return the height in pixels of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel height for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);

  return make_number (x_display_pixel_height (dpyinfo));
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

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_9
  mach_port_t masterPort;
  io_iterator_t it;
  io_object_t obj;

  // CGDisplayIOServicePort is deprecated.  Do it another (harder) way.

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

#else

  name = ns_get_name_from_ioreg (CGDisplayIOServicePort (did));

#endif
  return name;
}
#endif

static Lisp_Object
ns_make_monitor_attribute_list (struct MonitorInfo *monitors,
                                int n_monitors,
                                int primary_monitor,
                                const char *source)
{
  Lisp_Object monitor_frames = Fmake_vector (make_number (n_monitors), Qnil);
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
          // Flip y coordinate as NS has y starting from the bottom.
          y = (short) (primary_display_height - fr.size.height - fr.origin.y);
          vy = (short) (primary_display_height -
                        vfr.size.height - vfr.origin.y);
        }

      m->geom.x = (short) fr.origin.x;
      m->geom.y = y;
      m->geom.width = (unsigned short) fr.size.width;
      m->geom.height = (unsigned short) fr.size.height;

      m->work.x = (short) vfr.origin.x;
      // y is flipped on NS, so vy - y are pixels missing at the bottom,
      // and fr.size.height - vfr.size.height are pixels missing in total.
      // Pixels missing at top are
      // fr.size.height - vfr.size.height - vy + y.
      // work.y is then pixels missing at top + y.
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
      // Assume 92 dpi as x-display-mm-height/x-display-mm-width does.
      m->mm_width = (int) (25.4 * fr.size.width / 92.0);
      m->mm_height = (int) (25.4 * fr.size.height / 92.0);
#endif
    }

  // Primary monitor is always first for NS.
  attributes_list = ns_make_monitor_attribute_list (monitors, n_monitors,
                                                    0, "NS");

  free_monitors (monitors, n_monitors);
  return attributes_list;
}


DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Return the number of bitplanes of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  check_ns_display_info (terminal);
  return make_number
    (NSBitsPerPixelFromDepth ([[[NSScreen screens] objectAtIndex:0] depth]));
}


DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Returns the number of color cells of the Nextstep display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct ns_display_info *dpyinfo = check_ns_display_info (terminal);
  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_number (1 << min (dpyinfo->n_planes, 24));
}


/* Unused dummy def needed for compatibility. */
Lisp_Object tip_frame;

/* TODO: move to xdisp or similar */
static void
compute_tip_xy (struct frame *f,
                Lisp_Object parms,
                Lisp_Object dx,
                Lisp_Object dy,
                int width,
                int height,
                int *root_x,
                int *root_y)
{
  Lisp_Object left, top, right, bottom;
  EmacsView *view = FRAME_NS_VIEW (f);
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  NSPoint pt;

  /* Start with user-specified or mouse position.  */
  left = Fcdr (Fassq (Qleft, parms));
  top = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  if ((!INTEGERP (left) && !INTEGERP (right))
      || (!INTEGERP (top) && !INTEGERP (bottom)))
    {
      pt.x = dpyinfo->last_mouse_motion_x;
      pt.y = dpyinfo->last_mouse_motion_y;
      /* Convert to screen coordinates */
      pt = [view convertPoint: pt toView: nil];
#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7
      pt = [[view window] convertBaseToScreen: pt];
#else
      {
        NSRect r = NSMakeRect (pt.x, pt.y, 0, 0);
        r = [[view window] convertRectToScreen: r];
        pt.x = r.origin.x;
        pt.y = r.origin.y;
      }
#endif
    }
  else
    {
      /* Absolute coordinates.  */
      pt.x = INTEGERP (left) ? XINT (left) : XINT (right);
      pt.y = (x_display_pixel_height (FRAME_DISPLAY_INFO (f))
	      - (INTEGERP (top) ? XINT (top) : XINT (bottom))
	      - height);
    }

  /* Ensure in bounds.  (Note, screen origin = lower left.) */
  if (INTEGERP (left) || INTEGERP (right))
    *root_x = pt.x;
  else if (pt.x + XINT (dx) <= 0)
    *root_x = 0; /* Can happen for negative dx */
  else if (pt.x + XINT (dx) + width
	   <= x_display_pixel_width (FRAME_DISPLAY_INFO (f)))
    /* It fits to the right of the pointer.  */
    *root_x = pt.x + XINT (dx);
  else if (width + XINT (dx) <= pt.x)
    /* It fits to the left of the pointer.  */
    *root_x = pt.x - width - XINT (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = 0;

  if (INTEGERP (top) || INTEGERP (bottom))
    *root_y = pt.y;
  else if (pt.y - XINT (dy) - height >= 0)
    /* It fits below the pointer.  */
    *root_y = pt.y - height - XINT (dy);
  else if (pt.y + XINT (dy) + height
	   <= x_display_pixel_height (FRAME_DISPLAY_INFO (f)))
    /* It fits above the pointer */
      *root_y = pt.y + XINT (dy);
  else
    /* Put it on the top.  */
    *root_y = x_display_pixel_height (FRAME_DISPLAY_INFO (f)) - height;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a \"tooltip\" window on frame FRAME.
A tooltip window is a small window displaying a string.

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
  int root_x, root_y;
  ptrdiff_t count = SPECPDL_INDEX ();
  struct frame *f;
  char *str;
  NSSize size;

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);
  str = SSDATA (string);
  f = decode_window_system_frame (frame);
  if (NILP (timeout))
    timeout = make_number (5);
  else
    CHECK_NATNUM (timeout);

  if (NILP (dx))
    dx = make_number (5);
  else
    CHECK_NUMBER (dx);

  if (NILP (dy))
    dy = make_number (-10);
  else
    CHECK_NUMBER (dy);

  block_input ();
  if (ns_tooltip == nil)
    ns_tooltip = [[EmacsTooltip alloc] init];
  else
    Fx_hide_tip ();

  [ns_tooltip setText: str];
  size = [ns_tooltip frame].size;

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  compute_tip_xy (f, parms, dx, dy, (int)size.width, (int)size.height,
		  &root_x, &root_y);

  [ns_tooltip showAtX: root_x Y: root_y for: XINT (timeout)];
  unblock_input ();

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
     (void)
{
  if (ns_tooltip == nil || ![ns_tooltip isActive])
    return Qnil;
  [ns_tooltip hide];
  return Qt;
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
    return list4 (make_number (f->left_pos), make_number (f->top_pos),
		  make_number (f->left_pos + outer_width),
		  make_number (f->top_pos + outer_height));
  else if (EQ (attribute, Qnative_edges))
    return list4 (make_number (native_left), make_number (native_top),
		  make_number (native_right), make_number (native_bottom));
  else if (EQ (attribute, Qinner_edges))
    return list4 (make_number (native_left + internal_border_width),
		  make_number (native_top
			       + tool_bar_height
			       + internal_border_width),
		  make_number (native_right - internal_border_width),
		  make_number (native_bottom - internal_border_width));
  else
    return
      listn (CONSTYPE_HEAP, 10,
	     Fcons (Qouter_position,
		    Fcons (make_number (f->left_pos),
			   make_number (f->top_pos))),
	     Fcons (Qouter_size,
		    Fcons (make_number (outer_width),
			   make_number (outer_height))),
	     Fcons (Qexternal_border_size,
		    (fullscreen
		     ? Fcons (make_number (0), make_number (0))
		     : Fcons (make_number (border), make_number (border)))),
	     Fcons (Qtitle_bar_size,
		    Fcons (make_number (0), make_number (title_height))),
	     Fcons (Qmenu_bar_external, Qnil),
	     Fcons (Qmenu_bar_size, Fcons (make_number (0), make_number (0))),
	     Fcons (Qtool_bar_external,
		    FRAME_EXTERNAL_TOOL_BAR (f) ? Qt : Qnil),
	     Fcons (Qtool_bar_position, FRAME_TOOL_BAR_POSITION (f)),
	     Fcons (Qtool_bar_size,
		    Fcons (make_number (tool_bar_width),
			   make_number (tool_bar_height))),
	     Fcons (Qinternal_border_width,
		    make_number (internal_border_width)));
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

/* ==========================================================================

    Class implementations

   ========================================================================== */

/*
  Handle arrow/function/control keys and copy/paste/cut in file dialogs.
  Return YES if handled, NO if not.
 */
static BOOL
handlePanelKeys (NSSavePanel *panel, NSEvent *theEvent)
{
  NSString *s;
  int i;
  BOOL ret = NO;

  if ([theEvent type] != NSKeyDown) return NO;
  s = [theEvent characters];

  for (i = 0; i < [s length]; ++i)
    {
      int ch = (int) [s characterAtIndex: i];
      switch (ch)
        {
        case NSHomeFunctionKey:
        case NSDownArrowFunctionKey:
        case NSUpArrowFunctionKey:
        case NSLeftArrowFunctionKey:
        case NSRightArrowFunctionKey:
        case NSPageUpFunctionKey:
        case NSPageDownFunctionKey:
        case NSEndFunctionKey:
          /* Don't send command modified keys, as those are handled in the
             performKeyEquivalent method of the super class.
          */
          if (! ([theEvent modifierFlags] & NSCommandKeyMask))
            {
              [panel sendEvent: theEvent];
              ret = YES;
            }
          break;
          /* As we don't have the standard key commands for
             copy/paste/cut/select-all in our edit menu, we must handle
             them here.  TODO: handle Emacs key bindings for copy/cut/select-all
             here, paste works, because we have that in our Edit menu.
             I.e. refactor out code in nsterm.m, keyDown: to figure out the
             correct modifier.
          */
        case 'x': // Cut
        case 'c': // Copy
        case 'v': // Paste
        case 'a': // Select all
          if ([theEvent modifierFlags] & NSCommandKeyMask)
            {
              [NSApp sendAction:
                       (ch == 'x'
                        ? @selector(cut:)
                        : (ch == 'c'
                           ? @selector(copy:)
                           : (ch == 'v'
                              ? @selector(paste:)
                              : @selector(selectAll:))))
                             to:nil from:panel];
              ret = YES;
            }
        default:
          // Send all control keys, as the text field supports C-a, C-f, C-e
          // C-b and more.
          if ([theEvent modifierFlags] & NSControlKeyMask)
            {
              [panel sendEvent: theEvent];
              ret = YES;
            }
          break;
        }
    }


  return ret;
}

@implementation EmacsSavePanel
- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  BOOL ret = handlePanelKeys (self, theEvent);
  if (! ret)
    ret = [super performKeyEquivalent:theEvent];
  return ret;
}
@end


@implementation EmacsOpenPanel
- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  // NSOpenPanel inherits NSSavePanel, so passing self is OK.
  BOOL ret = handlePanelKeys (self, theEvent);
  if (! ret)
    ret = [super performKeyEquivalent:theEvent];
  return ret;
}
@end


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


/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */


void
syms_of_nsfns (void)
{
  DEFSYM (Qfontsize, "fontsize");

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
                '((\"^\\\\*\\\\(Group\\\\*$\\\\|Summary \\\\|Article\\\\*$\\\\)\"
                   . \"Gnus\"))))

When you miniaturize a Group, Summary or Article frame, Gnus.tiff will
be used as the image of the icon representing the frame.  */);
  Vns_icon_type_alist = list1 (Qt);

  DEFVAR_LISP ("ns-version-string", Vns_version_string,
               doc: /* Toolkit version for NS Windowing.  */);
  Vns_version_string = ns_appkit_version_str ();

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
  defsubr (&Sns_convert_utf8_nfd_to_nfc);
  defsubr (&Sns_popup_font_panel);
  defsubr (&Sns_popup_color_panel);

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);

  as_status = 0;
  as_script = Qnil;
  as_result = 0;
}
