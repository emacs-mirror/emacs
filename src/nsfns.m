/* Functions for the NeXT/Open/GNUstep and MacOSX window system.
   Copyright (C) 1989, 1992, 1993, 1994, 2005, 2006, 2008
     Free Software Foundation, Inc.

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

#include <signal.h>
#include <math.h>
#include "config.h"
#include "lisp.h"
#include "blockinput.h"
#include "nsterm.h"
#include "window.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "fontset.h"

#include "character.h"
#include "font.h"

#if 0
int fns_trace_num = 1;
#define NSTRACE(x)        fprintf (stderr, "%s:%d: [%d] " #x "\n",        \
                                  __FILE__, __LINE__, ++fns_trace_num)
#else
#define NSTRACE(x)
#endif

#ifdef HAVE_NS

extern NSArray *ns_send_types, *ns_return_types, *ns_drag_types;

extern Lisp_Object Qforeground_color;
extern Lisp_Object Qbackground_color;
extern Lisp_Object Qcursor_color;
extern Lisp_Object Qinternal_border_width;
extern Lisp_Object Qvisibility;
extern Lisp_Object Qcursor_type;
extern Lisp_Object Qicon_type;
extern Lisp_Object Qicon_name;
extern Lisp_Object Qicon_left;
extern Lisp_Object Qicon_top;
extern Lisp_Object Qleft;
extern Lisp_Object Qright;
extern Lisp_Object Qtop;
extern Lisp_Object Qdisplay;
extern Lisp_Object Qvertical_scroll_bars;
extern Lisp_Object Qauto_raise;
extern Lisp_Object Qauto_lower;
extern Lisp_Object Qbox;
extern Lisp_Object Qscroll_bar_width;
extern Lisp_Object Qx_resource_name;
extern Lisp_Object Qface_set_after_frame_default;
extern Lisp_Object Qunderline, Qundefined;
extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;

Lisp_Object Qnone;
Lisp_Object Qns_frame_parameter;
Lisp_Object Qbuffered;
Lisp_Object Qfontsize;

/* hack for OS X file panels */
char panelOK = 0;

/* Alist of elements (REGEXP . IMAGE) for images of icons associated
   to frames.*/
Lisp_Object Vns_icon_type_alist;

EmacsTooltip *ns_tooltip;

/* Need forward declaration here to preserve organizational integrity of file */
Lisp_Object Fx_open_connection (Lisp_Object, Lisp_Object, Lisp_Object);

extern BOOL ns_in_resize;


/* ==========================================================================

    Internal utility functions

   ========================================================================== */


void
check_ns (void)
{
 if (NSApp == nil)
   error ("OpenStep is not in use or not initialized");
}


/* Nonzero if we can use mouse menus. */
int
have_menus_p ()
{
  return NSApp != nil;
}


/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for NS.  */
static FRAME_PTR
check_ns_frame (Lisp_Object frame)
{
  FRAME_PTR f;

  if (NILP (frame))
      f = SELECTED_FRAME ();
  else
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
    }
  if (! FRAME_NS_P (f))
    error ("non-Nextstep frame used");
  return f;
}


/* Let the user specify an Nextstep display with a frame.
   nil stands for the selected frame--or, if that is not an Nextstep frame,
   the first Nextstep display on the list.  */
static struct ns_display_info *
check_ns_display_info (Lisp_Object frame)
{
  if (NILP (frame))
    {
      struct frame *f = SELECTED_FRAME ();
      if (FRAME_NS_P (f) && FRAME_LIVE_P (f) )
        return FRAME_NS_DISPLAY_INFO (f);
      else if (x_display_list != 0)
        return x_display_list;
      else
        error ("Nextstep windows are not in use or not initialized");
    }
  else if (INTEGERP (frame))
    {
      struct terminal *t = get_terminal (frame, 1);

      if (t->type != output_ns)
        error ("Terminal %d is not a Nextstep display", XINT (frame));

      return t->display_info.ns;
    }
  else if (STRINGP (frame))
    return ns_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      if (! FRAME_NS_P (f))
        error ("non-Nextstep frame used");
      return FRAME_NS_DISPLAY_INFO (f);
    }
  return NULL;  /* shut compiler up */
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


static NSScreen *
ns_get_screen (Lisp_Object anythingUnderTheSun)
{
  id window =nil;
  NSScreen *screen = 0;

  struct terminal *terminal;
  struct ns_display_info *dpyinfo;
  struct frame *f = NULL;
  Lisp_Object frame;

  if (INTEGERP (anythingUnderTheSun)) {
    /* we got a terminal */
    terminal = get_terminal (anythingUnderTheSun, 1);
    dpyinfo = terminal->display_info.ns;
    f = dpyinfo->x_focus_frame;
    if (!f)
      f = dpyinfo->x_highlight_frame;

  } else if (FRAMEP (anythingUnderTheSun) &&
             FRAME_NS_P (XFRAME (anythingUnderTheSun))) {
    /* we got a frame */
    f = XFRAME (anythingUnderTheSun);

  } else if (STRINGP (anythingUnderTheSun)) { /* FIXME/cl for multi-display */
  }

  if (!f)
    f = SELECTED_FRAME ();
  if (f)
    {
      XSETFRAME (frame, f);
      window = ns_get_window (frame);
    }

  if (window)
    screen = [window screen];
  if (!screen)
    screen = [NSScreen mainScreen];

  return screen;
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */
struct ns_display_info *
ns_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct ns_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = x_display_list, names = ns_display_name_list;
       dpyinfo;
       dpyinfo = dpyinfo->next, names = XCDR (names))
    {
      Lisp_Object tem;
      tem = Fstring_equal (XCAR (XCAR (names)), name);
      if (!NILP (tem))
        return dpyinfo;
    }

  error ("Emacs for OpenStep does not yet support multi-display.");

  Fx_open_connection (name, Qnil, Qnil);
  dpyinfo = x_display_list;

  if (dpyinfo == 0)
    error ("OpenStep on %s not responding.\n", SDATA (name));

  return dpyinfo;
}


static Lisp_Object
interpret_services_menu (NSMenu *menu, Lisp_Object prefix, Lisp_Object old)
/* --------------------------------------------------------------------------
   Turn the input menu (an NSMenu) into a lisp list for tracking on lisp side
   -------------------------------------------------------------------------- */
{
  int i, count;
  id<NSMenuItem> item;
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
ns_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSColor *col;

  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qforeground_color, oldval);
      error ("Unknown color");
    }

  [col retain];
  [f->output_data.ns->foreground_color release];
  f->output_data.ns->foreground_color = col;

  if (FRAME_NS_VIEW (f))
    {
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      /*recompute_basic_faces (f); */
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}


static void
ns_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct face *face;
  NSColor *col;
  NSView *view = FRAME_NS_VIEW (f);
  float alpha;

  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qbackground_color, oldval);
      error ("Unknown color");
    }

  /* clear the frame; in some instances the NS-internal GC appears not to
     update, or it does update and cannot clear old text properly */
  if (FRAME_VISIBLE_P (f))
    ns_clear_frame (f);

  [col retain];
  [f->output_data.ns->background_color release];
  f->output_data.ns->background_color = col;
  if (view != nil)
    {
      [[view window] setBackgroundColor: col];
      alpha = [col alphaComponent];

#ifdef NS_IMPL_COCOA
      /* the alpha code below only works on 10.4, so we need to do something
         else (albeit less good) otherwise.
         Check NSApplication.h for useful NSAppKitVersionNumber values. */
      if (NSAppKitVersionNumber < 744.0)
          [[view window] setAlphaValue: alpha];
#endif

      if (alpha != 1.0)
          [[view window] setOpaque: NO];
      else
          [[view window] setOpaque: YES];

      face = FRAME_DEFAULT_FACE (f);
      if (face)
        {
          col = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f);
          face->background
	     = (EMACS_UINT) [[col colorWithAlphaComponent: alpha] retain];
          [col release];

          update_face_from_frame_parameter (f, Qbackground_color, arg);
        }

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}


static void
ns_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSColor *col;

  if (ns_lisp_to_color (arg, &col))
    {
      store_frame_param (f, Qcursor_color, oldval);
      error ("Unknown color");
    }

  [f->output_data.ns->desired_cursor_color release];
  f->output_data.ns->desired_cursor_color = [col retain];

  if (FRAME_VISIBLE_P (f))
    {
      x_update_cursor (f, 0);
      x_update_cursor (f, 1);
    }
  update_face_from_frame_parameter (f, Qcursor_color, arg);
}


static void
ns_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE (ns_set_icon_name);

  if (ns_in_resize)
    return;

  /* see if it's changed */
  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
        return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  f->icon_name = arg;

  if (NILP (arg))
    {
      if (!NILP (f->title))
        arg = f->title;
      else
        /* explicit name and no icon-name -> explicit_name */
        if (f->explicit_name)
          arg = f->name;
        else
          {
            /* no explicit name and no icon-name ->
               name has to be rebuild from icon_title_format */
            windows_or_buffers_changed++;
            return;
          }
    }

  /* Don't change the name if it's already NAME.  */
  if ([[view window] miniwindowTitle] &&
      ([[[view window] miniwindowTitle]
             isEqualToString: [NSString stringWithUTF8String:
                                           SDATA (arg)]]))
    return;

  [[view window] setMiniwindowTitle:
        [NSString stringWithUTF8String: SDATA (arg)]];
}


static void
ns_set_name_iconic (struct frame *f, Lisp_Object name, int explicit)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE (ns_set_name_iconic);

  if (ns_in_resize)
    return;

  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
         update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
        update_mode_lines = 1;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    name = f->name;

  /* title overrides explicit name */
  if (! NILP (f->title))
    name = f->title;

  /* icon_name overrides title and explicit name */
  if (! NILP (f->icon_name))
    name = f->icon_name;

  if (NILP (name))
    name = build_string
        ([[[NSProcessInfo processInfo] processName] UTF8String]);
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if ([[view window] miniwindowTitle] &&
      ([[[view window] miniwindowTitle]
             isEqualToString: [NSString stringWithUTF8String:
                                           SDATA (name)]]))
    return;

  [[view window] setMiniwindowTitle:
        [NSString stringWithUTF8String: SDATA (name)]];
}


static void
ns_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE (ns_set_name);

  if (ns_in_resize)
    return;

  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
         update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
        update_mode_lines = 1;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  if (NILP (name))
    name = build_string
        ([[[NSProcessInfo processInfo] processName] UTF8String]);

  f->name = name;

  /* title overrides explicit name */
  if (! NILP (f->title))
    name = f->title;

  CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if ([[[view window] title]
            isEqualToString: [NSString stringWithUTF8String:
                                          SDATA (name)]])
    return;
  [[view window] setTitle: [NSString stringWithUTF8String:
                                        SDATA (name)]];
}


/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
ns_explicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE (ns_explicitly_set_name);
  ns_set_name_iconic (f, arg, 1);
  ns_set_name (f, arg, 1);
}


/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  NSTRACE (x_implicitly_set_name);
  if (FRAME_ICONIFIED_P (f))
    ns_set_name_iconic (f, arg, 0);
  else
    ns_set_name (f, arg, 0);
}


/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
   name; if NAME is a string, set F's name to NAME and set
   F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
   suggesting a new name, which lisp code should override; if
   F->explicit_name is set, ignore the new name; otherwise, set it.  */
static void
ns_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  NSTRACE (ns_set_title);
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;
}


void
ns_set_name_as_filename (struct frame *f)
{
  NSView *view = FRAME_NS_VIEW (f);
  Lisp_Object name;
  Lisp_Object buf = XWINDOW (f->selected_window)->buffer;
  const char *title;
  NSAutoreleasePool *pool;
  NSTRACE (ns_set_name_as_filename);

  if (f->explicit_name || ! NILP (f->title) || ns_in_resize)
    return;

  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];
  name =XBUFFER (buf)->filename;
  if (NILP (name) || FRAME_ICONIFIED_P (f)) name =XBUFFER (buf)->name;

  if (FRAME_ICONIFIED_P (f) && !NILP (f->icon_name))
    name = f->icon_name;

  if (NILP (name))
    name = build_string
        ([[[NSProcessInfo processInfo] processName] UTF8String]);
  else
    CHECK_STRING (name);

  title = FRAME_ICONIFIED_P (f) ? [[[view window] miniwindowTitle] UTF8String]
                                : [[[view window] title] UTF8String];

  if (title && (! strcmp (title, SDATA (name))))
    {
      [pool release];
      UNBLOCK_INPUT;
      return;
    }

  if (! FRAME_ICONIFIED_P (f))
    {
#ifdef NS_IMPL_COCOA
      /* work around a bug observed on 10.3 where
         setTitleWithRepresentedFilename does not clear out previous state
         if given filename does not exist */
      NSString *str = [NSString stringWithUTF8String: SDATA (name)];
      if (![[NSFileManager defaultManager] fileExistsAtPath: str])
        {
          [[view window] setTitleWithRepresentedFilename: @""];
          [[view window] setTitle: str];
        }
      else
        {
          [[view window] setTitleWithRepresentedFilename: str];
        }
#else
      [[view window] setTitleWithRepresentedFilename:
                         [NSString stringWithUTF8String: SDATA (name)]];
#endif
      f->name = name;
    }
  else
    {
      [[view window] setMiniwindowTitle:
            [NSString stringWithUTF8String: SDATA (name)]];
    }
  [pool release];
  UNBLOCK_INPUT;
}


void
ns_set_doc_edited (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  NSView *view = FRAME_NS_VIEW (f);
  NSAutoreleasePool *pool;
  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];
  [[view window] setDocumentEdited: !NILP (arg)];
  [pool release];
  UNBLOCK_INPUT;
}


void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
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


/* 23: toolbar support */
void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  Lisp_Object root_window;

  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value) && XINT (value) >= 0)
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

  x_set_window_size (f, 0, f->text_cols, f->text_lines);
}


void
ns_implicitly_set_icon_type (struct frame *f)
{
  Lisp_Object tem;
  EmacsView *view = FRAME_NS_VIEW (f);
  id image =nil;
  Lisp_Object chain, elt;
  NSAutoreleasePool *pool;
  BOOL setMini = YES;

  NSTRACE (ns_implicitly_set_icon_type);

  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];
  if (f->output_data.ns->miniimage
      && [[NSString stringWithUTF8String: SDATA (f->name)]
               isEqualToString: [(NSImage *)f->output_data.ns->miniimage name]])
    {
      [pool release];
      UNBLOCK_INPUT;
      return;
    }

  tem = assq_no_quit (Qicon_type, f->param_alist);
  if (CONSP (tem) && ! NILP (XCDR (tem)))
    {
      [pool release];
      UNBLOCK_INPUT;
      return;
    }

  for (chain = Vns_icon_type_alist;
       (image = nil) && CONSP (chain);
       chain = XCDR (chain))
    {
      elt = XCAR (chain);
      /* special case: 't' means go by file type */
      if (SYMBOLP (elt) && EQ (elt, Qt) && SDATA (f->name)[0] == '/')
        {
          NSString *str
	     = [NSString stringWithUTF8String: SDATA (f->name)];
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
					    SDATA (XCDR (elt))]] retain];
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
  UNBLOCK_INPUT;
}


static void
ns_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  EmacsView *view = FRAME_NS_VIEW (f);
  id image = nil;
  BOOL setMini = YES;

  NSTRACE (ns_set_icon_type);

  if (!NILP (arg) && SYMBOLP (arg))
    {
      arg =build_string (SDATA (SYMBOL_NAME (arg)));
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
                                            SDATA (arg)]];

  if (image == nil)
    {
      image = [NSImage imageNamed: @"text"];
      setMini = NO;
    }

  f->output_data.ns->miniimage = image;
  [view setMiniwindowImage: setMini];
}


/* 23: added Xism; we stub out (we do implement this in ns-win.el) */
int
XParseGeometry (char *string, int *x, int *y,
                unsigned int *width, unsigned int *height)
{
  message1 ("Warning: XParseGeometry not supported under NS.\n");
  return 0;
}


/* TODO: move to nsterm? */
int
ns_lisp_to_cursor_type (Lisp_Object arg)
{
  char *str;
  if (XTYPE (arg) == Lisp_String)
    str = SDATA (arg);
  else if (XTYPE (arg) == Lisp_Symbol)
    str = SDATA (SYMBOL_NAME (arg));
  else return -1;
  if (!strcmp (str, "box"))	 return filled_box;
  if (!strcmp (str, "hollow"))	 return hollow_box;
  if (!strcmp (str, "underscore")) return underscore;
  if (!strcmp (str, "bar"))	 return bar;
  if (!strcmp (str, "no"))	 return no_highlight;
  return -1;
}


Lisp_Object
ns_cursor_type_to_lisp (int arg)
{
  switch (arg)
    {
    case filled_box: return Qbox;
    case hollow_box: return intern ("hollow");
    case underscore: return intern ("underscore");
    case bar:	     return intern ("bar");
    case no_highlight:
    default:	     return intern ("no");
    }
}


static void
ns_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int val;

  val = ns_lisp_to_cursor_type (arg);
  if (val >= 0)
    {
      f->output_data.ns->desired_cursor =val;
    }
  else
    {
      store_frame_param (f, Qcursor_type, oldval);
      error ("the `cursor-type' frame parameter should be either `no', `box', \
`hollow', `underscore' or `bar'.");
    }

  update_mode_lines++;
}


/* 23: called to set mouse pointer color, but all other terms use it to
       initialize pointer types (and don't set the color ;) */
static void
ns_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  /* don't think we can do this on Nextstep */
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

  f->output_data.ns->icon_top = Qnil;
  f->output_data.ns->icon_left = Qnil;

  /* Set the position of the icon.  */
  icon_x = x_get_arg (dpyinfo, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_get_arg (dpyinfo, parms, Qicon_top, 0, 0,  RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
      f->output_data.ns->icon_top = icon_y;
      f->output_data.ns->icon_left = icon_x;
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}


/* 23 Note: commented out ns_... entries are no longer used in 23.
            commented out x_... entries have not been implemented yet.
   see frame.c for template, also where all generic OK functions are impl */
frame_parm_handler ns_frame_parm_handlers[] =
{
  x_set_autoraise, /* generic OK */
  x_set_autolower, /* generic OK */
  ns_set_background_color,
  0, /* x_set_border_color,  may be impossible under Nextstep */
  0, /* x_set_border_width,  may be impossible under Nextstep */
  ns_set_cursor_color,
  ns_set_cursor_type,
  x_set_font, /* generic OK */
  ns_set_foreground_color,
  ns_set_icon_name,
  ns_set_icon_type,
  x_set_internal_border_width, /* generic OK */
  x_set_menu_bar_lines,
  ns_set_mouse_color,
  ns_explicitly_set_name,
  x_set_scroll_bar_width, /* generic OK */
  ns_set_title,
  x_set_unsplittable, /* generic OK */
  x_set_vertical_scroll_bars, /* generic OK */
  x_set_visibility, /* generic OK */
  x_set_tool_bar_lines,
  0, /* x_set_scroll_bar_foreground, will ignore (not possible on NS) */
  0, /* x_set_scroll_bar_background,  will ignore (not possible on NS) */
  x_set_screen_gamma, /* generic OK */
  x_set_line_spacing, /* generic OK, sets f->extra_line_spacing to int */
  x_set_fringe_width, /* generic OK */
  x_set_fringe_width, /* generic OK */
  0, /* x_set_wait_for_wm, will ignore */
  0,  /* x_set_fullscreen will ignore */
  x_set_font_backend /* generic OK */
};


DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new Nextstep window, called a \"frame\" in Emacs terms.
Return an Emacs frame object.
PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use,
then `default-minibuffer-frame' must be a frame whose minibuffer can
be shared by the new frame.  */)
     (parms)
     Lisp_Object parms;
{
  static int desc_ctr = 1;
  struct frame *f;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  int count = specpdl_ptr - specpdl;
  Lisp_Object display;
  struct ns_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;
  Lisp_Object tfont, tfontsize;
  int window_prompting = 0;
  int width, height;

  check_ns ();

  display = x_get_arg (dpyinfo, parms, Qterminal, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_ns_display_info (display);

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  kb = dpyinfo->terminal->kboard;

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

  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);

  tem = x_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
                  RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    {
      f = make_frame_without_minibuffer (Qnil, kb, display);
    }
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (WINDOWP (tem))
    {
      f = make_frame_without_minibuffer (tem, kb, display);
    }
  else
    {
      f = make_frame (1);
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name) || (XTYPE (name) != Lisp_String))
    {
      f->name
	 = build_string ([[[NSProcessInfo processInfo] processName] UTF8String]);
      f->explicit_name =0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      specbind (Qx_resource_name, name);
    }

  XSETFRAME (frame, f);
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;

  f->terminal = dpyinfo->terminal;
  f->terminal->reference_count++;

  f->output_method = output_ns;
  f->output_data.ns = (struct ns_output *)xmalloc (sizeof *(f->output_data.ns));
  bzero (f->output_data.ns, sizeof (*(f->output_data.ns)));

  FRAME_FONTSET (f) = -1;

  /* record_unwind_protect (unwind_create_frame, frame); safety; maybe later? */

  f->icon_name = x_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
                            RES_TYPE_STRING);
  if (EQ (f->icon_name, Qunbound) || (XTYPE (f->icon_name) != Lisp_String))
    f->icon_name = Qnil;

  FRAME_NS_DISPLAY_INFO (f) = dpyinfo;

  f->output_data.ns->window_desc = desc_ctr++;
  if (!NILP (parent))
    {
      f->output_data.ns->parent_desc = (Window) XFASTINT (parent);
      f->output_data.ns->explicit_parent = 1;
    }
  else
    {
      f->output_data.ns->parent_desc = FRAME_NS_DISPLAY_INFO (f)->root_window;
      f->output_data.ns->explicit_parent = 0;
    }

  f->resx = dpyinfo->resx;
  f->resy = dpyinfo->resy;

  BLOCK_INPUT;
  register_font_driver (&nsfont_driver, f);
  x_default_parameter (f, parms, Qfont_backend, Qnil,
			"fontBackend", "FontBackend", RES_TYPE_STRING);

  {
    /* use for default font name */
    id font = [NSFont userFixedPitchFontOfSize: -1.0]; /* default */
    tfontsize = x_default_parameter (f, parms, Qfontsize,
                                    make_number (0 /*(int)[font pointSize]*/),
                                    "fontSize", "FontSize", RES_TYPE_NUMBER);
    tfont = x_default_parameter (f, parms, Qfont,
                                 build_string ([[font fontName] UTF8String]),
                                 "font", "Font", RES_TYPE_STRING);
  }
  UNBLOCK_INPUT;

  x_default_parameter (f, parms, Qborder_width, make_number (0),
		       "borderwidth", "BorderWidth", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qinternal_border_width, make_number (2),
                      "internalBorderWidth", "InternalBorderWidth",
                      RES_TYPE_NUMBER);

  /* default scrollbars on right on Mac */
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
  x_default_parameter (f, parms, Qforeground_color, build_string ("Black"),
                      "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("White"),
                      "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qcursor_color, build_string ("grey"),
                      "cursorColor", "CursorColor", RES_TYPE_STRING);
  /* FIXME: not suppported yet in Nextstep */
  x_default_parameter (f, parms, Qline_spacing, Qnil,
		       "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qleft_fringe, Qnil,
		       "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qright_fringe, Qnil,
		       "rightFringe", "RightFringe", RES_TYPE_NUMBER);
  /* end PENDING */

  init_frame_faces (f);

  x_default_parameter (f, parms, Qmenu_bar_lines, make_number (0), "menuBar",
                      "menuBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qtool_bar_lines, make_number (0), "toolBar",
                      "toolBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil, "bufferPredicate",
                       "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil, "title", "Title",
                       RES_TYPE_STRING);

/* TODO: other terms seem to get away w/o this complexity.. */
  if (NILP (Fassq (Qwidth, parms)))
    {
      Lisp_Object value
	 = x_get_arg (dpyinfo, parms, Qwidth, "width", "Width",
		      RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qwidth, value), parms);
    }
  if (NILP (Fassq (Qheight, parms)))
    {
      Lisp_Object value
	 = x_get_arg (dpyinfo, parms, Qheight, "height", "Height",
		      RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qheight, value), parms);
    }
  if (NILP (Fassq (Qleft, parms)))
    {
      Lisp_Object value
	 = x_get_arg (dpyinfo, parms, Qleft, "left", "Left", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qleft, value), parms);
    }
  if (NILP (Fassq (Qtop, parms)))
    {
      Lisp_Object value
	 = x_get_arg (dpyinfo, parms, Qtop, "top", "Top", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qtop, value), parms);
    }

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
  FRAME_NS_DISPLAY_INFO (f)->vertical_scroll_bar_cursor
     = [NSCursor arrowCursor];
  f->output_data.ns->current_pointer = f->output_data.ns->text_cursor;

  [[EmacsView alloc] initFrameFromEmacs: f];

  x_icon (f, parms);

  /* It is now ok to make the frame official even if we get an error below.
     The frame needs to be on Vframe_list or making it visible won't work. */
  Vframe_list = Fcons (frame, Vframe_list);
  /*FRAME_NS_DISPLAY_INFO (f)->reference_count++; */

  x_default_parameter (f, parms, Qcursor_type, Qbox, "cursorType", "CursorType",
                      RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qscroll_bar_width, Qnil, "scrollBarWidth",
                      "ScrollBarWidth", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qicon_type, Qnil, "bitmapIcon", "BitmapIcon",
                      RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qauto_raise, Qnil, "autoRaise", "AutoRaise",
                      RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil, "autoLower", "AutoLower",
                      RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qbuffered, Qt, "buffered", "Buffered",
                      RES_TYPE_BOOLEAN);

  width = FRAME_COLS (f);
  height = FRAME_LINES (f);

  SET_FRAME_COLS (f, 0);
  FRAME_LINES (f) = 0;
  change_frame_size (f, height, width, 1, 0, 0);

  if (! f->output_data.ns->explicit_parent)
    {
        tem = x_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_BOOLEAN);
        if (EQ (tem, Qunbound))
            tem = Qnil;

        x_set_visibility (f, tem, Qnil);
        if (EQ (tem, Qt))
            [[FRAME_NS_VIEW (f) window] makeKeyWindow];
    }

  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (kb->Vdefault_minibuffer_frame)
          || !FRAME_LIVE_P (XFRAME (kb->Vdefault_minibuffer_frame))))
    kb->Vdefault_minibuffer_frame = frame;

  /* All remaining specified parameters, which have not been "used"
     by x_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      f->param_alist = Fcons (XCAR (tem), f->param_alist);

  UNGCPRO;
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}


/* ==========================================================================

    Lisp definitions

   ========================================================================== */

DEFUN ("x-focus-frame", Fx_focus_frame, Sx_focus_frame, 1, 1, 0,
       doc: /* Set the input focus to FRAME.
FRAME nil means use the selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f = check_ns_frame (frame);
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (f);

  if (dpyinfo->x_focus_frame != f)
    {
      EmacsView *view = FRAME_NS_VIEW (f);
      BLOCK_INPUT;
      [[view window] makeKeyAndOrderFront: view];
      UNBLOCK_INPUT;
    }

  return Qnil;
}


DEFUN ("ns-popup-prefs-panel", Fns_popup_prefs_panel, Sns_popup_prefs_panel,
       0, 0, "",
       doc: /* Pop up the preferences panel. */)
     ()
{
  check_ns ();
  [(EmacsApp *)NSApp showPreferencesWindow: NSApp];
  return Qnil;
}


DEFUN ("ns-popup-font-panel", Fns_popup_font_panel, Sns_popup_font_panel,
       0, 1, "",
       doc: /* Pop up the font panel. */)
     (frame)
     Lisp_Object frame;
{
  id fm;
  struct frame *f;

  check_ns ();
  fm = [NSFontManager new];
  if (NILP (frame))
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  [fm setSelectedFont: ((struct nsfont_info *)f->output_data.ns->font)->nsfont
           isMultiple: NO];
  [fm orderFrontFontPanel: NSApp];
  return Qnil;
}


DEFUN ("ns-popup-color-panel", Fns_popup_color_panel, Sns_popup_color_panel, 
       0, 1, "",
       doc: /* Pop up the color panel.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;

  check_ns ();
  if (NILP (frame))
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  [NSApp orderFrontColorPanel: NSApp];
  return Qnil;
}


DEFUN ("ns-read-file-name", Fns_read_file_name, Sns_read_file_name, 1, 4, 0,
       doc: /* Use a graphical panel to read a file name, using prompt PROMPT.
Optional arg DIR, if non-nil, supplies a default directory.
Optional arg ISLOAD, if non-nil, means read a file name for saving.
Optional arg INIT, if non-nil, provides a default file name to use.  */)
     (prompt, dir, isLoad, init)
     Lisp_Object prompt, dir, isLoad, init;
{
  static id fileDelegate = nil;
  int ret;
  id panel;
  NSString *fname;

  NSString *promptS = NILP (prompt) || !STRINGP (prompt) ? nil :
    [NSString stringWithUTF8String: SDATA (prompt)];
  NSString *dirS = NILP (dir) || !STRINGP (dir) ?
    [NSString stringWithUTF8String: SDATA (current_buffer->directory)] :
    [NSString stringWithUTF8String: SDATA (dir)];
  NSString *initS = NILP (init) || !STRINGP (init) ? nil :
    [NSString stringWithUTF8String: SDATA (init)];

  check_ns ();

  if (fileDelegate == nil)
    fileDelegate = [EmacsFileDelegate new];

  [NSCursor setHiddenUntilMouseMoves: NO];

  if ([dirS characterAtIndex: 0] == '~')
    dirS = [dirS stringByExpandingTildeInPath];

  panel = NILP (isLoad) ?
    [EmacsSavePanel savePanel] : [EmacsOpenPanel openPanel];

  [panel setTitle: promptS];

  /* Puma (10.1) does not have */
  if ([panel respondsToSelector: @selector (setAllowsOtherFileTypes:)])
    [panel setAllowsOtherFileTypes: YES];

  [panel setTreatsFilePackagesAsDirectories: YES];
  [panel setDelegate: fileDelegate];

  panelOK = 0;
  if (NILP (isLoad))
    {
      ret = [panel runModalForDirectory: dirS file: initS];
    }
  else
    {
      [panel setCanChooseDirectories: YES];
      ret = [panel runModalForDirectory: dirS file: initS types: nil];
    }

  ret = (ret = NSOKButton) || panelOK;

  fname = [panel filename];

  [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];

  return ret ? build_string ([fname UTF8String]) : Qnil;
}


DEFUN ("ns-get-resource", Fns_get_resource, Sns_get_resource, 2, 2, 0,
       doc: /* Return the value of the property NAME of OWNER from the defaults database.
If OWNER is nil, Emacs is assumed.  */)
     (owner, name)
     Lisp_Object owner, name;
{
  const char *value;

  check_ns ();
  if (NILP (owner))
    owner = build_string
        ([[[NSProcessInfo processInfo] processName] UTF8String]);
  /* CHECK_STRING (owner);  this should be just "Emacs" */
  CHECK_STRING (name);
/*fprintf (stderr, "ns-get-resource checking resource '%s'\n", SDATA (name)); */

  value =[[[NSUserDefaults standardUserDefaults]
            objectForKey: [NSString stringWithUTF8String: SDATA (name)]]
           UTF8String];

  if (value)
    return build_string (value);
  return Qnil;
}


DEFUN ("ns-set-resource", Fns_set_resource, Sns_set_resource, 3, 3, 0,
       doc: /* Set property NAME of OWNER to VALUE, from the defaults database.
If OWNER is nil, Emacs is assumed.
If VALUE is nil, the default is removed.  */)
     (owner, name, value)
     Lisp_Object owner, name, value;
{
  check_ns ();
  if (NILP (owner))
    owner
       = build_string ([[[NSProcessInfo processInfo] processName] UTF8String]);
  CHECK_STRING (owner);
  CHECK_STRING (name);
  if (NILP (value))
    {
      [[NSUserDefaults standardUserDefaults] removeObjectForKey:
                         [NSString stringWithUTF8String: SDATA (name)]];
    }
  else
    {
      CHECK_STRING (value);
      [[NSUserDefaults standardUserDefaults] setObject:
                [NSString stringWithUTF8String: SDATA (value)]
                                        forKey: [NSString stringWithUTF8String:
                                                         SDATA (name)]];
    }

  return Qnil;
}


DEFUN ("ns-set-alpha", Fns_set_alpha, Sns_set_alpha, 2, 2, 0,
       doc: /* Return a color equivalent to COLOR with alpha setting ALPHA.
The argument ALPHA should be a number between 0 and 1, where 0 is full
transparency and 1 is opaque.  */)
     (color, alpha)
     Lisp_Object color;
     Lisp_Object alpha;
{
  NSColor *col;
  float a;

  CHECK_STRING (color);
  CHECK_NUMBER_OR_FLOAT (alpha);

  if (ns_lisp_to_color (color, &col))
    error ("Unknown color.");

  a = XFLOATINT (alpha);
  if (a < 0.0 || a > 1.0)
    error ("Alpha value should be between 0 and 1 inclusive.");

  col = [col colorWithAlphaComponent: a];
  return ns_color_to_lisp (col);
}


DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* This function is a no-op.  It is only present for completeness.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  /* This function has no real equivalent under NeXTstep.  Return nil to
     indicate this. */
  return Qnil;
}


DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Return the vendor ID string of Nextstep display server DISPLAY.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
#ifdef NS_IMPL_GNUSTEP
  return build_string ("GNU");
#else
  return build_string ("Apple");
#endif
}


DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Return the version number of Nextstep display server DISPLAY.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, the selected frame's display is used.
See also the function `ns-server-vendor'.  */)
     (display)
     Lisp_Object display;
{
  /* FIXME: return GUI version on GNUSTEP, ?? on OS X */
  return build_string ("1.0");
}


DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on Nextstep display server DISPLAY.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  int num;

  check_ns ();
  num = [[NSScreen screens] count];

  return (num != 0) ? make_number (num) : Qnil;
}


DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height,
       0, 1, 0,
       doc: /* Return the height of Nextstep display server DISPLAY, in millimeters.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  return make_number ((int)
                     ([ns_get_screen (display) frame].size.height/(92.0/25.4)));
}


DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width,
       0, 1, 0,
       doc: /* Return the width of Nextstep display server DISPLAY, in millimeters.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  return make_number ((int)
                     ([ns_get_screen (display) frame].size.width/(92.0/25.4)));
}


DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Return whether the Nexstep display DISPLAY supports backing store.
The value may be `buffered', `retained', or `non-retained'.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  switch ([ns_get_window (display) backingType])
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
       doc: /* Return the visual class of the Nextstep display server DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  NSWindowDepth depth;
  check_ns ();
  depth = [ns_get_screen (display) depth];

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
       doc: /* Non-nil if the Nextstep display server supports the save-under feature.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be a frame, the display name as a string, or a terminal ID.
If omitted or nil, the selected frame's display is used.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  switch ([ns_get_window (display) backingType])
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
       doc: /* Open a connection to a Nextstep display server.
DISPLAY is the name of the display to connect to.
Optional arguments XRM-STRING and MUST-SUCCEED are currently ignored.  */)
     (display, resource_string, must_succeed)
     Lisp_Object display, resource_string, must_succeed;
{
  struct ns_display_info *dpyinfo;

  CHECK_STRING (display);

  nxatoms_of_nsselect ();
  dpyinfo = ns_term_init (display);
  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
        fatal ("OpenStep on %s not responding.\n",
               SDATA (display));
      else
        error ("OpenStep on %s not responding.\n",
               SDATA (display));
    }

  /* Register our external input/output types, used for determining
     applicable services and also drag/drop eligibility. */
  ns_send_types = [[NSArray arrayWithObject: NSStringPboardType] retain];
  ns_return_types = [[NSArray arrayWithObject: NSStringPboardType] retain];
  ns_drag_types = [[NSArray arrayWithObjects:
                            NSStringPboardType,
                            NSTabularTextPboardType,
                            NSFilenamesPboardType,
                            NSURLPboardType,
                            NSColorPboardType,
                            NSFontPboardType, nil] retain];

  return Qnil;
}


DEFUN ("x-close-connection", Fx_close_connection, Sx_close_connection,
       1, 1, 0,
       doc: /* Close the connection to the current Nextstep display server.
The second argument DISPLAY is currently ignored.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
#ifdef NS_IMPL_COCOA
  PSFlush ();
#endif
  /*ns_delete_terminal (dpyinfo->terminal); */
  [NSApp terminate: NSApp];
  return Qnil;
}


DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
     ()
{
  Lisp_Object tail, result;

  result = Qnil;
  for (tail = ns_display_name_list; CONSP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}


DEFUN ("ns-hide-others", Fns_hide_others, Sns_hide_others,
       0, 0, 0,
       doc: /* Hides all applications other than emacs.  */)
     ()
{
  check_ns ();
  [NSApp hideOtherApplications: NSApp];
  return Qnil;
}

DEFUN ("ns-hide-emacs", Fns_hide_emacs, Sns_hide_emacs,
       1, 1, 0,
       doc: /* If ON is non-nil, the entire emacs application is hidden.
Otherwise if emacs is hidden, it is unhidden.
If ON is equal to `activate', emacs is unhidden and becomes
the active application.  */)
     (on)
     Lisp_Object on;
{
  check_ns ();
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
     ()
{
  check_ns ();
  [NSApp orderFrontStandardAboutPanel: nil];
  return Qnil;
}


DEFUN ("ns-font-name", Fns_font_name, Sns_font_name, 1, 1, 0,
       doc: /* Determine font postscript or family name for font NAME.
NAME should be a string containing either the font name or an XLFD
font descriptor.  If string contains `fontset' and not
`fontset-startup', it is left alone. */)
     (name)
     Lisp_Object name;
{
  char *nm;
  CHECK_STRING (name);
  nm = SDATA (name);

  if (nm[0] != '-')
    return name;
  if (strstr (nm, "fontset") && !strstr (nm, "fontset-startup"))
    return name;

  return build_string (ns_xlfd_to_fontname (SDATA (name)));
}


DEFUN ("ns-list-colors", Fns_list_colors, Sns_list_colors, 0, 1, 0,
       doc: /* Return a list of all available colors.
The optional argument FRAME is currently ignored.  */)
     (frame)
     Lisp_Object frame;
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

  BLOCK_INPUT;

  colorlists = [[NSColorList availableColorLists] objectEnumerator];
  while (clist = [colorlists nextObject])
    {
      if ([[clist name] length] < 7 ||
          [[clist name] rangeOfString: @"PANTONE"].location == 0)
        {
          NSEnumerator *cnames = [[clist allKeys] reverseObjectEnumerator];
          NSString *cname;
          while (cname = [cnames nextObject])
            list = Fcons (build_string ([cname UTF8String]), list);
/*           for (i = [[clist allKeys] count] - 1; i >= 0; i--)
               list = Fcons (build_string ([[[clist allKeys] objectAtIndex: i]
                                             UTF8String]), list); */
        }
    }

  UNBLOCK_INPUT;

  return list;
}


DEFUN ("ns-list-services", Fns_list_services, Sns_list_services, 0, 0, 0,
       doc: /* List available Nextstep services by querying NSApp.  */)
     ()
{
  Lisp_Object ret = Qnil;
  NSMenu *svcs;
  id delegate;

  check_ns ();
  svcs = [[NSMenu alloc] initWithTitle: @"Services"];
  [NSApp setServicesMenu: svcs];  /* this and next rebuild on <10.4 */
  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: ns_return_types];

/* On Tiger, services menu updating was made lazier (waits for user to
   actually click on the menu), so we have to force things along: */
#ifdef NS_IMPL_COCOA
  if (NSAppKitVersionNumber >= 744.0)
    {
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
    }
#endif

  [svcs setAutoenablesItems: NO];
#ifdef NS_IMPL_COCOA
  [svcs update]; /* on OS X, converts from '/' structure */
#endif

  ret = interpret_services_menu (svcs, Qnil, ret);
  return ret;
}


DEFUN ("ns-perform-service", Fns_perform_service, Sns_perform_service,
       2, 2, 0,
       doc: /* Perform Nextstep SERVICE on SEND.
SEND should be either a string or nil.
The return value is the result of the service, as string, or nil if
there was no result.  */)
     (service, send)
     Lisp_Object service, send;
{
  id pb;
  NSString *svcName;
  char *utfStr;
  int len;

  CHECK_STRING (service);
  check_ns ();

  utfStr = SDATA (service);
  svcName = [NSString stringWithUTF8String: utfStr];

  pb =[NSPasteboard pasteboardWithUniqueName];
  ns_string_to_pasteboard (pb, send);

  if (NSPerformService (svcName, pb) == NO)
    Fsignal (Qquit, Fcons (build_string ("service not available"), Qnil));

  if ([[pb types] count] == 0)
    return build_string ("");
  return ns_string_from_pasteboard (pb);
}


DEFUN ("ns-convert-utf8-nfd-to-nfc", Fns_convert_utf8_nfd_to_nfc,
       Sns_convert_utf8_nfd_to_nfc, 1, 1, 0,
       doc: /* Return an NFC string that matches  the UTF-8 NFD string STR.  */)
    (str)
    Lisp_Object str;
{
  NSString *utfStr;

  CHECK_STRING (str);
  utfStr = [[NSString stringWithUTF8String: SDATA (str)]
             precomposedStringWithCanonicalMapping];
  return build_string ([utfStr UTF8String]);
}


/* ==========================================================================

    Miscellaneous functions not called through hooks

   ========================================================================== */


/* 23: call in image.c */
FRAME_PTR
check_x_frame (Lisp_Object frame)
{
  return check_ns_frame (frame);
}

/* 23: added, due to call in frame.c */
struct ns_display_info *
check_x_display_info (Lisp_Object frame)
{
  return check_ns_display_info (frame);
}


/* 23: new function; we don't have much in the way of flexibility though */
void
x_set_scroll_bar_default_width (f)
     struct frame *f;
{
  int wid = FRAME_COLUMN_WIDTH (f);
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = NS_SCROLL_BAR_WIDTH_DEFAULT;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) +
                                      wid - 1) / wid;
}


/* 23: terms now impl this instead of x-get-resource directly */
const char *
x_get_string_resource (XrmDatabase rdb, char *name, char *class)
{
  /* remove appname prefix; TODO: allow for !="Emacs" */
  char *toCheck = class + (!strncmp (class, "Emacs.", 6) ? 6 : 0);
  const char *res;
  check_ns ();

  /* Support emacs-20-style face resources for backwards compatibility */
  if (!strncmp (toCheck, "Face", 4))
    toCheck = name + (!strncmp (name, "emacs.", 6) ? 6 : 0);

/*fprintf (stderr, "Checking '%s'\n", toCheck); */
  
  res = [[[NSUserDefaults standardUserDefaults] objectForKey:
                   [NSString stringWithUTF8String: toCheck]] UTF8String];
  return !res ? NULL :
      (!strncasecmp (res, "YES", 3) ? "true" :
          (!strncasecmp (res, "NO", 2) ? "false" : res));
}


Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (frame);
  Lisp_Object nsfocus;

  if (!dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (nsfocus, dpyinfo->x_focus_frame);
  return nsfocus;
}


int
x_pixel_width (struct frame *f)
{
  return FRAME_PIXEL_WIDTH (f);
}


int
x_pixel_height (struct frame *f)
{
  return FRAME_PIXEL_HEIGHT (f);
}


int
x_char_width (struct frame *f)
{
  return FRAME_COLUMN_WIDTH (f);
}


int
x_char_height (struct frame *f)
{
  return FRAME_LINE_HEIGHT (f);
}


int
x_screen_planes (struct frame *f)
{
  return FRAME_NS_DISPLAY_INFO (f)->n_planes;
}


void
x_sync (Lisp_Object frame)
{
  /* XXX Not implemented XXX */
  return;
}



/* ==========================================================================

    Lisp definitions that, for whatever reason, we can't alias as 'ns-XXX'.

   ========================================================================== */


#ifdef NS_IMPL_COCOA

/* Compile and execute the AppleScript SCRIPT and return the error
   status as function value.  A zero is returned if compilation and
   execution is successful, in which case *RESULT is set to a Lisp
   string or a number containing the resulting script value.  Otherwise,
   1 is returned. */

static int
do_applescript (script, result)
     Lisp_Object script, *result;
{
  NSAppleEventDescriptor *desc;
  NSDictionary* errorDict;
  NSAppleEventDescriptor* returnDescriptor = NULL;

  NSAppleScript* scriptObject =
    [[NSAppleScript alloc] initWithSource:
			     [NSString stringWithUTF8String: SDATA (script)]];

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
	      (typeUTF16ExternalRepresentation 
	       == [returnDescriptor descriptorType]) ||
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

DEFUN ("do-applescript", Fdo_applescript, Sdo_applescript, 1, 1, 0,
       doc: /* Execute AppleScript SCRIPT and return the result.  If
compilation and execution are successful, the resulting script value
is returned as a string, a number or, in the case of other constructs,
t.  In case the execution fails, an error is signaled. */)
    (script)
    Lisp_Object script;
{
  Lisp_Object result;
  long status;

  CHECK_STRING (script);
  check_ns ();

  BLOCK_INPUT;
  status = do_applescript (script, &result);
  UNBLOCK_INPUT;
  if (status == 0)
    return result;
  else if (!STRINGP (result))
    error ("AppleScript error %d", status);
  else
    error ("%s", SDATA (result));
}
#endif

DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Return t if the current Nextstep display supports the color COLOR.
The optional argument FRAME is currently ignored.  */)
     (color, frame)
     Lisp_Object color, frame;
{
  NSColor * col;
  check_ns ();
  return ns_lisp_to_color (color, &col) ? Qnil : Qt;
}


DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Return a description of the color named COLOR.
The value is a list of integer RGBA values--(RED GREEN BLUE ALPHA).
These values appear to range from 0 to 65280; white is (65280 65280 65280 0).
The optional argument FRAME is currently ignored.  */)
     (color, frame)
     Lisp_Object color, frame;
{
  NSColor * col;
  float red, green, blue, alpha;
  Lisp_Object rgba[4];

  check_ns ();
  CHECK_STRING (color);

  if (ns_lisp_to_color (color, &col))
    return Qnil;

  [[col colorUsingColorSpaceName: NSCalibratedRGBColorSpace]
        getRed: &red green: &green blue: &blue alpha: &alpha];
  rgba[0] = make_number (lrint (red*65280));
  rgba[1] = make_number (lrint (green*65280));
  rgba[2] = make_number (lrint (blue*65280));
  rgba[3] = make_number (lrint (alpha*65280));

  return Flist (4, rgba);
}


DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Return t if the Nextstep display supports color.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  NSWindowDepth depth;
  NSString *colorSpace;
  check_ns ();
  depth = [ns_get_screen (display) depth];
  colorSpace = NSColorSpaceFromDepth (depth);

  return    [colorSpace isEqualToString: NSDeviceWhiteColorSpace]
         || [colorSpace isEqualToString: NSCalibratedWhiteColorSpace]
      ? Qnil : Qt;
}


DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p,
       Sx_display_grayscale_p, 0, 1, 0,
       doc: /* Return t if the Nextstep display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display. */)
     (display)
     Lisp_Object display;
{
  NSWindowDepth depth;
  check_ns ();
  depth = [ns_get_screen (display) depth];

  return NSBitsPerPixelFromDepth (depth) > 1 ? Qt : Qnil;
}


DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Returns the width in pixels of the Nextstep display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  return make_number ((int) [ns_get_screen (display) frame].size.width);
}


DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Returns the height in pixels of the Nextstep display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  return make_number ((int) [ns_get_screen (display) frame].size.height);
}

DEFUN ("display-usable-bounds", Fns_display_usable_bounds,
       Sns_display_usable_bounds, 0, 1, 0,
       doc: /*Return the bounds of the usable part of the screen.
The return value is a list of integers (LEFT TOP WIDTH HEIGHT), which
are the boundaries of the usable part of the screen, excluding areas
reserved for the Mac menu, dock, and so forth.

The screen queried corresponds to DISPLAY, which should be either a
frame, a display name (a string), or terminal ID.  If omitted or nil,
that stands for the selected frame's display. */)
     (display)
     Lisp_Object display;
{
  int top;
  NSRect vScreen;

  check_ns ();
  vScreen = [ns_get_screen (display) visibleFrame];
  top = vScreen.origin.y == 0.0 ?
    (int) [ns_get_screen (display) frame].size.height - vScreen.size.height : 0;

  return list4 (make_number ((int) vScreen.origin.x),
                make_number (top),
                make_number ((int) vScreen.size.width),
                make_number ((int) vScreen.size.height));
}


DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Returns the number of bitplanes of the Nextstep display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  return make_number
    (NSBitsPerSampleFromDepth ([ns_get_screen (display) depth]));
}


DEFUN ("x-display-color-cells", Fx_display_color_cells,
       Sx_display_color_cells, 0, 1, 0,
       doc: /* Returns the number of color cells of the Nextstep display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame, a display name (a string), or terminal ID.
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  check_ns ();
  struct ns_display_info *dpyinfo = check_ns_display_info (display);

  /* We force 24+ bit depths to 24-bit to prevent an overflow.  */
  return make_number (1 << min (dpyinfo->n_planes, 24));
}


/* Unused dummy def needed for compatibility. */
Lisp_Object tip_frame;

/* TODO: move to xdisp or similar */
static void
compute_tip_xy (f, parms, dx, dy, width, height, root_x, root_y)
     struct frame *f;
     Lisp_Object parms, dx, dy;
     int width, height;
     int *root_x, *root_y;
{
  Lisp_Object left, top;
  EmacsView *view = FRAME_NS_VIEW (f);
  NSPoint pt;
  
  /* Start with user-specified or mouse position.  */
  left = Fcdr (Fassq (Qleft, parms));
  if (INTEGERP (left))
    pt.x = XINT (left);
  else
    pt.x = last_mouse_motion_position.x;
  top = Fcdr (Fassq (Qtop, parms));
  if (INTEGERP (top))
    pt.y = XINT (top);
  else
    pt.y = last_mouse_motion_position.y;

  /* Convert to screen coordinates */
  pt = [view convertPoint: pt toView: nil];
  pt = [[view window] convertBaseToScreen: pt];

  /* Ensure in bounds.  (Note, screen origin = lower left.) */
  if (pt.x + XINT (dx) <= 0)
    *root_x = 0; /* Can happen for negative dx */
  else if (pt.x + XINT (dx) + width <= FRAME_NS_DISPLAY_INFO (f)->width)
    /* It fits to the right of the pointer.  */
    *root_x = pt.x + XINT (dx);
  else if (width + XINT (dx) <= pt.x)
    /* It fits to the left of the pointer.  */
    *root_x = pt.x - width - XINT (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = 0;

  if (pt.y - XINT (dy) - height >= 0)
    /* It fits below the pointer.  */
    *root_y = pt.y - height - XINT (dy);
  else if (pt.y + XINT (dy) + height <= FRAME_NS_DISPLAY_INFO (f)->height)
    /* It fits above the pointer */
      *root_y = pt.y + XINT (dy);
  else
    /* Put it on the top.  */
    *root_y = FRAME_NS_DISPLAY_INFO (f)->height - height;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small window displaying a string.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARMS contains a `left' parameter,
the tooltip is displayed at that x-position.  Otherwise it is
displayed at the mouse position, with offset DX added (default is 5 if
DX isn't specified).  Likewise for the y-position; if a `top' frame
parameter is specified, it determines the y-position of the tooltip
window, otherwise it is displayed at the mouse position, with offset
DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
     (string, frame, parms, timeout, dx, dy)
     Lisp_Object string, frame, parms, timeout, dx, dy;
{
  int root_x, root_y;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int count = SPECPDL_INDEX ();
  struct frame *f;
  char *str;
  NSSize size;

  specbind (Qinhibit_redisplay, Qt);

  GCPRO4 (string, parms, frame, timeout);

  CHECK_STRING (string);
  str = SDATA (string);
  f = check_x_frame (frame);
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

  BLOCK_INPUT;
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
  UNBLOCK_INPUT;

  UNGCPRO;
  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
     ()
{
  if (ns_tooltip == nil || ![ns_tooltip isActive])
    return Qnil;
  [ns_tooltip hide];
  return Qt;
}


/* ==========================================================================

    Class implementations

   ========================================================================== */


@implementation EmacsSavePanel
#ifdef NS_IMPL_COCOA
/* --------------------------------------------------------------------------
   These are overridden to intercept on OS X: ending panel restarts NSApp
   event loop if it is stopped.  Not sure if this is correct behavior,
   perhaps should check if running and if so send an appdefined.
   -------------------------------------------------------------------------- */
- (void) ok: (id)sender
{
  [super ok: sender];
  panelOK = 1;
  [NSApp stop: self];
}
- (void) cancel: (id)sender
{
  [super cancel: sender];
  [NSApp stop: self];
}
#endif
@end


@implementation EmacsOpenPanel
#ifdef NS_IMPL_COCOA
/* --------------------------------------------------------------------------
   These are overridden to intercept on OS X: ending panel restarts NSApp
   event loop if it is stopped.  Not sure if this is correct behavior,
   perhaps should check if running and if so send an appdefined.
   -------------------------------------------------------------------------- */
- (void) ok: (id)sender
{
  [super ok: sender];
  panelOK = 1;
  [NSApp stop: self];
}
- (void) cancel: (id)sender
{
  [super cancel: sender];
  [NSApp stop: self];
}
#endif
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
syms_of_nsfns ()
{
  int i;

  Qns_frame_parameter = intern ("ns-frame-parameter");
  staticpro (&Qns_frame_parameter);
  Qnone = intern ("none");
  staticpro (&Qnone);
  Qbuffered = intern ("bufferd");
  staticpro (&Qbuffered);
  Qfontsize = intern ("fontsize");
  staticpro (&Qfontsize);

  DEFVAR_LISP ("ns-icon-type-alist", &Vns_icon_type_alist,
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
  Vns_icon_type_alist = Fcons (Qt, Qnil);

  defsubr (&Sns_read_file_name);
  defsubr (&Sns_get_resource);
  defsubr (&Sns_set_resource);
  defsubr (&Sxw_display_color_p); /* this and next called directly by C code */
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sns_font_name);
  defsubr (&Sns_list_colors);
#ifdef NS_IMPL_COCOA
  defsubr (&Sdo_applescript);
#endif
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sns_display_usable_bounds);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_save_under);
  defsubr (&Sx_create_frame);
  defsubr (&Sns_set_alpha);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);

  defsubr (&Sns_hide_others);
  defsubr (&Sns_hide_emacs);
  defsubr (&Sns_emacs_info_panel);
  defsubr (&Sns_list_services);
  defsubr (&Sns_perform_service);
  defsubr (&Sns_convert_utf8_nfd_to_nfc);
  defsubr (&Sx_focus_frame);
  defsubr (&Sns_popup_prefs_panel);
  defsubr (&Sns_popup_font_panel);
  defsubr (&Sns_popup_color_panel);

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);

  /* used only in fontset.c */
  check_window_system_func = check_ns;

}

// arch-tag: dc2a3f74-1123-4daa-8eed-fb78db6a5642
