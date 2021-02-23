/* NeXT/Open/GNUstep / macOS communication module.      -*- coding: utf-8 -*-

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2021 Free Software
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

#include <fcntl.h>
#include <math.h>
#include <pthread.h>
#include <sys/types.h>
#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <stdbool.h>

#include <c-ctype.h>
#include <c-strcase.h>
#include <ftoastr.h>

#include "lisp.h"
#include "blockinput.h"
#include "sysselect.h"
#include "nsterm.h"
#include "systime.h"
#include "character.h"
#include "xwidget.h"
#include "fontset.h"
#include "composite.h"
#include "ccl.h"

#include "termhooks.h"
#include "termchar.h"
#include "menu.h"
#include "window.h"
#include "keyboard.h"
#include "buffer.h"
#include "font.h"
#include "pdumper.h"

#ifdef NS_IMPL_GNUSTEP
#include "process.h"
#endif

#ifdef NS_IMPL_COCOA
#include "macfont.h"
#include <Carbon/Carbon.h>
#endif

#ifdef NS_DRAW_TO_BUFFER
#include <IOSurface/IOSurface.h>
#endif

static EmacsMenu *dockMenu;
#ifdef NS_IMPL_COCOA
static EmacsMenu *mainMenu;
#endif

/* ==========================================================================

   NSTRACE, Trace support.

   ========================================================================== */

#if NSTRACE_ENABLED

/* The following use "volatile" since they can be accessed from
   parallel threads.  */
volatile int nstrace_num = 0;
volatile int nstrace_depth = 0;

/* When 0, no trace is emitted.  This is used by NSTRACE_WHEN and
   NSTRACE_UNLESS to silence functions called.

   TODO: This should really be a thread-local variable, to avoid that
   a function with disabled trace thread silence trace output in
   another.  However, in practice this seldom is a problem.  */
volatile int nstrace_enabled_global = 1;

/* Called when nstrace_enabled goes out of scope.  */
void nstrace_leave(int * pointer_to_nstrace_enabled)
{
  if (*pointer_to_nstrace_enabled)
    {
      --nstrace_depth;
    }
}


/* Called when nstrace_saved_enabled_global goes out of scope.  */
void nstrace_restore_global_trace_state(int * pointer_to_saved_enabled_global)
{
  nstrace_enabled_global = *pointer_to_saved_enabled_global;
}


char const * nstrace_fullscreen_type_name (int fs_type)
{
  switch (fs_type)
    {
    case -1:                   return "-1";
    case FULLSCREEN_NONE:      return "FULLSCREEN_NONE";
    case FULLSCREEN_WIDTH:     return "FULLSCREEN_WIDTH";
    case FULLSCREEN_HEIGHT:    return "FULLSCREEN_HEIGHT";
    case FULLSCREEN_BOTH:      return "FULLSCREEN_BOTH";
    case FULLSCREEN_MAXIMIZED: return "FULLSCREEN_MAXIMIZED";
    default:                   return "FULLSCREEN_?????";
    }
}
#endif


/* ==========================================================================

   NSColor, EmacsColor category.

   ========================================================================== */
@implementation NSColor (EmacsColor)
+ (NSColor *)colorForEmacsRed:(CGFloat)red green:(CGFloat)green
                         blue:(CGFloat)blue alpha:(CGFloat)alpha
{
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  if (ns_use_srgb_colorspace
      && NSAppKitVersionNumber >= NSAppKitVersionNumber10_7)
    return [NSColor colorWithSRGBRed: red
                               green: green
                                blue: blue
                               alpha: alpha];
#endif
  return [NSColor colorWithCalibratedRed: red
                                   green: green
                                    blue: blue
                                   alpha: alpha];
}

- (NSColor *)colorUsingDefaultColorSpace
{
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  if (ns_use_srgb_colorspace
      && NSAppKitVersionNumber >= NSAppKitVersionNumber10_7)
    return [self colorUsingColorSpace: [NSColorSpace sRGBColorSpace]];
#endif
  return [self colorUsingColorSpace: [NSColorSpace deviceRGBColorSpace]];
}

@end

/* ==========================================================================

    Local declarations

   ========================================================================== */

/* Convert a symbol indexed with an NSxxx value to a value as defined
   in keyboard.c (lispy_function_key). I hope this is a correct way
   of doing things...  */
static unsigned convert_ns_to_X_keysym[] =
{
  NSHomeFunctionKey,            0x50,
  NSLeftArrowFunctionKey,       0x51,
  NSUpArrowFunctionKey,         0x52,
  NSRightArrowFunctionKey,      0x53,
  NSDownArrowFunctionKey,       0x54,
  NSPageUpFunctionKey,          0x55,
  NSPageDownFunctionKey,        0x56,
  NSEndFunctionKey,             0x57,
  NSBeginFunctionKey,           0x58,
  NSSelectFunctionKey,          0x60,
  NSPrintFunctionKey,           0x61,
  NSClearLineFunctionKey,       0x0B,
  NSExecuteFunctionKey,         0x62,
  NSInsertFunctionKey,          0x63,
  NSUndoFunctionKey,            0x65,
  NSRedoFunctionKey,            0x66,
  NSMenuFunctionKey,            0x67,
  NSFindFunctionKey,            0x68,
  NSHelpFunctionKey,            0x6A,
  NSBreakFunctionKey,           0x6B,

  NSF1FunctionKey,              0xBE,
  NSF2FunctionKey,              0xBF,
  NSF3FunctionKey,              0xC0,
  NSF4FunctionKey,              0xC1,
  NSF5FunctionKey,              0xC2,
  NSF6FunctionKey,              0xC3,
  NSF7FunctionKey,              0xC4,
  NSF8FunctionKey,              0xC5,
  NSF9FunctionKey,              0xC6,
  NSF10FunctionKey,             0xC7,
  NSF11FunctionKey,             0xC8,
  NSF12FunctionKey,             0xC9,
  NSF13FunctionKey,             0xCA,
  NSF14FunctionKey,             0xCB,
  NSF15FunctionKey,             0xCC,
  NSF16FunctionKey,             0xCD,
  NSF17FunctionKey,             0xCE,
  NSF18FunctionKey,             0xCF,
  NSF19FunctionKey,             0xD0,
  NSF20FunctionKey,             0xD1,
  NSF21FunctionKey,             0xD2,
  NSF22FunctionKey,             0xD3,
  NSF23FunctionKey,             0xD4,
  NSF24FunctionKey,             0xD5,

  NSBackspaceCharacter,         0x08,  /* 8: Not on some KBs.  */
  NSDeleteCharacter,            0xFF,  /* 127: Big 'delete' key upper right.  */
  NSDeleteFunctionKey,          0x9F,  /* 63272: Del forw key off main array.  */

  NSTabCharacter,		0x09,
  0x19,				0x09,  /* left tab->regular since pass shift */
  NSCarriageReturnCharacter,	0x0D,
  NSNewlineCharacter,		0x0D,
  NSEnterCharacter,		0x8D,

  0x41|NSEventModifierFlagNumericPad,	0xAE,  /* KP_Decimal */
  0x43|NSEventModifierFlagNumericPad,	0xAA,  /* KP_Multiply */
  0x45|NSEventModifierFlagNumericPad,	0xAB,  /* KP_Add */
  0x4B|NSEventModifierFlagNumericPad,	0xAF,  /* KP_Divide */
  0x4E|NSEventModifierFlagNumericPad,	0xAD,  /* KP_Subtract */
  0x51|NSEventModifierFlagNumericPad,	0xBD,  /* KP_Equal */
  0x52|NSEventModifierFlagNumericPad,	0xB0,  /* KP_0 */
  0x53|NSEventModifierFlagNumericPad,	0xB1,  /* KP_1 */
  0x54|NSEventModifierFlagNumericPad,	0xB2,  /* KP_2 */
  0x55|NSEventModifierFlagNumericPad,	0xB3,  /* KP_3 */
  0x56|NSEventModifierFlagNumericPad,	0xB4,  /* KP_4 */
  0x57|NSEventModifierFlagNumericPad,	0xB5,  /* KP_5 */
  0x58|NSEventModifierFlagNumericPad,	0xB6,  /* KP_6 */
  0x59|NSEventModifierFlagNumericPad,	0xB7,  /* KP_7 */
  0x5B|NSEventModifierFlagNumericPad,	0xB8,  /* KP_8 */
  0x5C|NSEventModifierFlagNumericPad,	0xB9,  /* KP_9 */

  0x1B,				0x1B   /* escape */
};

/* On macOS picks up the default NSGlobalDomain AppleAntiAliasingThreshold,
   the maximum font size to NOT antialias.  On GNUstep there is currently
   no way to control this behavior.  */
float ns_antialias_threshold;

NSArray *ns_send_types = 0, *ns_return_types = 0;
static NSArray *ns_drag_types = 0;
NSString *ns_app_name = @"Emacs";  /* default changed later */

/* Display variables */
struct ns_display_info *x_display_list; /* Chain of existing displays */
long context_menu_value = 0;

/* display update */
static struct frame *ns_updating_frame;
#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
static NSView *focus_view = NULL;
#endif
static int ns_window_num = 0;
static BOOL gsaved = NO;
static BOOL ns_fake_keydown = NO;
#ifdef NS_IMPL_COCOA
static BOOL ns_menu_bar_is_hidden = NO;
#endif
/* static int debug_lock = 0; */

/* event loop */
static BOOL send_appdefined = YES;
#define NO_APPDEFINED_DATA (-8)
static int last_appdefined_event_data = NO_APPDEFINED_DATA;
static NSTimer *timed_entry = 0;
static NSTimer *scroll_repeat_entry = nil;
static fd_set select_readfds, select_writefds;
enum { SELECT_HAVE_READ = 1, SELECT_HAVE_WRITE = 2, SELECT_HAVE_TMO = 4 };
static int select_nfds = 0, select_valid = 0;
static struct timespec select_timeout = { 0, 0 };
static int selfds[2] = { -1, -1 };
static pthread_mutex_t select_mutex;
static NSAutoreleasePool *outerpool;
static struct input_event *emacs_event = NULL;
static struct input_event *q_event_ptr = NULL;
static int n_emacs_events_pending = 0;
static NSMutableArray *ns_pending_files, *ns_pending_service_names,
  *ns_pending_service_args;
static BOOL ns_do_open_file = NO;
static BOOL ns_last_use_native_fullscreen;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static BOOL any_help_event_p = NO;

static struct {
  struct input_event *q;
  int nr, cap;
} hold_event_q = {
  NULL, 0, 0
};

/* Convert modifiers in a NeXTstep event to emacs style modifiers.  */
#define NS_FUNCTION_KEY_MASK 0x800000
#define NSLeftControlKeyMask    (0x000001 | NSEventModifierFlagControl)
#define NSRightControlKeyMask   (0x002000 | NSEventModifierFlagControl)
#define NSLeftCommandKeyMask    (0x000008 | NSEventModifierFlagCommand)
#define NSRightCommandKeyMask   (0x000010 | NSEventModifierFlagCommand)
#define NSLeftAlternateKeyMask  (0x000020 | NSEventModifierFlagOption)
#define NSRightAlternateKeyMask (0x000040 | NSEventModifierFlagOption)

/* MODIFIER if a symbol; otherwise its property KIND, if a symbol.  */
static Lisp_Object
mod_of_kind (Lisp_Object modifier, Lisp_Object kind)
{
  if (SYMBOLP (modifier))
    return modifier;
  else
    {
      Lisp_Object val = Fplist_get (modifier, kind);
      return SYMBOLP (val) ? val : Qnil;
    }
}

static unsigned int
ev_modifiers_helper (unsigned int flags, unsigned int left_mask,
                     unsigned int right_mask, unsigned int either_mask,
                     Lisp_Object left_modifier, Lisp_Object right_modifier)
{
  unsigned int modifiers = 0;

  if (flags & either_mask)
    {
      BOOL left_key = (flags & left_mask) == left_mask;
      BOOL right_key = (flags & right_mask) == right_mask
        && ! EQ (right_modifier, Qleft);

      if (right_key)
        modifiers |= parse_solitary_modifier (right_modifier);

      /* GNUstep (and possibly macOS in certain circumstances) doesn't
         differentiate between the left and right keys, so if we can't
         identify which key it is, we use the left key setting.  */
      if (left_key || ! right_key)
        modifiers |= parse_solitary_modifier (left_modifier);
    }

  return modifiers;
}

#define EV_MODIFIERS2(flags, kind)                                      \
  (((flags & NSEventModifierFlagHelp) ?                                 \
    hyper_modifier : 0)                                                 \
   | ((flags & NSEventModifierFlagShift) ?                              \
      shift_modifier : 0)                                               \
   | ((flags & NS_FUNCTION_KEY_MASK)                                    \
      ? parse_solitary_modifier (mod_of_kind (ns_function_modifier,     \
                                              kind))                    \
      : 0)                                                              \
   | ev_modifiers_helper (flags, NSLeftControlKeyMask,                  \
                          NSRightControlKeyMask,                        \
                          NSEventModifierFlagControl,                   \
                          mod_of_kind (ns_control_modifier, kind),      \
                          mod_of_kind (ns_right_control_modifier,       \
                                       kind))                           \
   | ev_modifiers_helper (flags, NSLeftCommandKeyMask,                  \
                          NSRightCommandKeyMask,                        \
                          NSEventModifierFlagCommand,                   \
                          mod_of_kind (ns_command_modifier, kind),      \
                          mod_of_kind (ns_right_command_modifier,       \
                                       kind))                           \
   | ev_modifiers_helper (flags, NSLeftAlternateKeyMask,                \
                          NSRightAlternateKeyMask,                      \
                          NSEventModifierFlagOption,                    \
                          mod_of_kind (ns_alternate_modifier, kind),    \
                          mod_of_kind (ns_right_alternate_modifier,     \
                                       kind)))

#define EV_MODIFIERS(e) EV_MODIFIERS2 ([e modifierFlags], QCmouse)

#define EV_UDMODIFIERS(e)                                      \
    ((([e type] == NSEventTypeLeftMouseDown) ? down_modifier : 0)       \
     | (([e type] == NSEventTypeRightMouseDown) ? down_modifier : 0)    \
     | (([e type] == NSEventTypeOtherMouseDown) ? down_modifier : 0)    \
     | (([e type] == NSEventTypeLeftMouseDragged) ? down_modifier : 0)  \
     | (([e type] == NSEventTypeRightMouseDragged) ? down_modifier : 0) \
     | (([e type] == NSEventTypeOtherMouseDragged) ? down_modifier : 0) \
     | (([e type] == NSEventTypeLeftMouseUp)   ? up_modifier   : 0)     \
     | (([e type] == NSEventTypeRightMouseUp)   ? up_modifier   : 0)    \
     | (([e type] == NSEventTypeOtherMouseUp)   ? up_modifier   : 0))

#define EV_BUTTON(e)                                                         \
    ((([e type] == NSEventTypeLeftMouseDown) || ([e type] == NSEventTypeLeftMouseUp)) ? 0 :    \
      (([e type] == NSEventTypeRightMouseDown) || ([e type] == NSEventTypeRightMouseUp)) ? 2 : \
     [e buttonNumber] - 1)

/* Convert the time field to a timestamp in milliseconds.  */
#define EV_TIMESTAMP(e) ([e timestamp] * 1000)

/* This is a piece of code which is common to all the event handling
   methods.  Maybe it should even be a function.  */
#define EV_TRAILER(e)                                                   \
  {                                                                     \
    XSETFRAME (emacs_event->frame_or_window, emacsframe);               \
    EV_TRAILER2 (e);                                                    \
  }

#define EV_TRAILER2(e)                                                  \
  {                                                                     \
      if (e) emacs_event->timestamp = EV_TIMESTAMP (e);                 \
      if (q_event_ptr)                                                  \
        {                                                               \
          Lisp_Object tem = Vinhibit_quit;                              \
          Vinhibit_quit = Qt;                                           \
          n_emacs_events_pending++;                                     \
          kbd_buffer_store_event_hold (emacs_event, q_event_ptr);       \
          Vinhibit_quit = tem;                                          \
        }                                                               \
      else                                                              \
        hold_event (emacs_event);                                       \
      EVENT_INIT (*emacs_event);                                        \
      ns_send_appdefined (-1);                                          \
    }


/* These flags will be OR'd or XOR'd with the NSWindow's styleMask
   property depending on what we're doing.  */
#define FRAME_DECORATED_FLAGS (NSWindowStyleMaskTitled              \
                               | NSWindowStyleMaskResizable         \
                               | NSWindowStyleMaskMiniaturizable    \
                               | NSWindowStyleMaskClosable)
#define FRAME_UNDECORATED_FLAGS NSWindowStyleMaskBorderless

/* TODO: Get rid of need for these forward declarations.  */
static void ns_condemn_scroll_bars (struct frame *f);
static void ns_judge_scroll_bars (struct frame *f);


/* ==========================================================================

    Utilities

   ========================================================================== */

void
ns_init_events (struct input_event *ev)
{
  EVENT_INIT (*ev);
  emacs_event = ev;
}

void
ns_finish_events (void)
{
  emacs_event = NULL;
}

static void
hold_event (struct input_event *event)
{
  if (hold_event_q.nr == hold_event_q.cap)
    {
      if (hold_event_q.cap == 0) hold_event_q.cap = 10;
      else hold_event_q.cap *= 2;
      hold_event_q.q =
        xrealloc (hold_event_q.q, hold_event_q.cap * sizeof *hold_event_q.q);
    }

  hold_event_q.q[hold_event_q.nr++] = *event;
  /* Make sure ns_read_socket is called, i.e. we have input.  */
  raise (SIGIO);
  send_appdefined = YES;
}

static Lisp_Object
append2 (Lisp_Object list, Lisp_Object item)
/* --------------------------------------------------------------------------
   Utility to append to a list
   -------------------------------------------------------------------------- */
{
  return nconc2 (list, list (item));
}


const char *
ns_etc_directory (void)
/* If running as a self-contained app bundle, return as a string the
   filename of the etc directory, if present; else nil.  */
{
  NSBundle *bundle = [NSBundle mainBundle];
  NSString *resourceDir = [bundle resourcePath];
  NSString *resourcePath;
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;

  resourcePath = [resourceDir stringByAppendingPathComponent: @"etc"];
  if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
    {
      if (isDir) return [resourcePath UTF8String];
    }
  return NULL;
}


const char *
ns_exec_path (void)
/* If running as a self-contained app bundle, return as a path string
   the filenames of the libexec and bin directories, ie libexec:bin.
   Otherwise, return nil.
   Normally, Emacs does not add its own bin/ directory to the PATH.
   However, a self-contained NS build has a different layout, with
   bin/ and libexec/ subdirectories in the directory that contains
   Emacs.app itself.
   We put libexec first, because init_callproc_1 uses the first
   element to initialize exec-directory.  An alternative would be
   for init_callproc to check for invocation-directory/libexec.
*/
{
  NSBundle *bundle = [NSBundle mainBundle];
  NSString *resourceDir = [bundle resourcePath];
  NSString *binDir = [bundle bundlePath];
  NSString *resourcePath, *resourcePaths;
  NSRange range;
  NSString *pathSeparator = [NSString stringWithFormat: @"%c", SEPCHAR];
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSArray *paths;
  NSEnumerator *pathEnum;
  BOOL isDir;

  range = [resourceDir rangeOfString: @"Contents"];
  if (range.location != NSNotFound)
    {
      binDir = [binDir stringByAppendingPathComponent: @"Contents"];
#ifdef NS_IMPL_COCOA
      binDir = [binDir stringByAppendingPathComponent: @"MacOS"];
#endif
    }

  paths = [binDir stringsByAppendingPaths:
                [NSArray arrayWithObjects: @"libexec", @"bin", nil]];
  pathEnum = [paths objectEnumerator];
  resourcePaths = @"";

  while ((resourcePath = [pathEnum nextObject]))
    {
      if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
        if (isDir)
          {
            if ([resourcePaths length] > 0)
              resourcePaths
                = [resourcePaths stringByAppendingString: pathSeparator];
            resourcePaths
              = [resourcePaths stringByAppendingString: resourcePath];
          }
    }
  if ([resourcePaths length] > 0) return [resourcePaths UTF8String];

  return NULL;
}


const char *
ns_load_path (void)
/* If running as a self-contained app bundle, return as a path string
   the filenames of the site-lisp and lisp directories.
   Ie, site-lisp:lisp.  Otherwise, return nil.  */
{
  NSBundle *bundle = [NSBundle mainBundle];
  NSString *resourceDir = [bundle resourcePath];
  NSString *resourcePath, *resourcePaths;
  NSString *pathSeparator = [NSString stringWithFormat: @"%c", SEPCHAR];
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  NSArray *paths = [resourceDir stringsByAppendingPaths:
                              [NSArray arrayWithObjects:
                                         @"site-lisp", @"lisp", nil]];
  NSEnumerator *pathEnum = [paths objectEnumerator];
  resourcePaths = @"";

  /* Hack to skip site-lisp.  */
  if (no_site_lisp) resourcePath = [pathEnum nextObject];

  while ((resourcePath = [pathEnum nextObject]))
    {
      if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
        if (isDir)
          {
            if ([resourcePaths length] > 0)
              resourcePaths
                = [resourcePaths stringByAppendingString: pathSeparator];
            resourcePaths
              = [resourcePaths stringByAppendingString: resourcePath];
          }
    }
  if ([resourcePaths length] > 0) return [resourcePaths UTF8String];

  return NULL;
}


void
ns_init_locale (void)
/* macOS doesn't set any environment variables for the locale when run
   from the GUI. Get the locale from the OS and set LANG.  */
{
  NSLocale *locale = [NSLocale currentLocale];

  NSTRACE ("ns_init_locale");

  @try
    {
      /* It seems macOS should probably use UTF-8 everywhere.
         'localeIdentifier' does not specify the encoding, and I can't
         find any way to get the OS to tell us which encoding to use,
         so hard-code '.UTF-8'.  */
      NSString *localeID = [NSString stringWithFormat:@"%@.UTF-8",
                                     [locale localeIdentifier]];

      /* Set LANG to locale, but not if LANG is already set.  */
      setenv("LANG", [localeID UTF8String], 0);
    }
  @catch (NSException *e)
    {
      NSLog (@"Locale detection failed: %@: %@", [e name], [e reason]);
    }
}


void
ns_release_object (void *obj)
/* --------------------------------------------------------------------------
    Release an object (callable from C)
   -------------------------------------------------------------------------- */
{
    [(id)obj release];
}


void
ns_retain_object (void *obj)
/* --------------------------------------------------------------------------
     Retain an object (callable from C)
   -------------------------------------------------------------------------- */
{
    [(id)obj retain];
}


void *
ns_alloc_autorelease_pool (void)
/* --------------------------------------------------------------------------
     Allocate a pool for temporary objects (callable from C)
   -------------------------------------------------------------------------- */
{
  return [[NSAutoreleasePool alloc] init];
}


void
ns_release_autorelease_pool (void *pool)
/* --------------------------------------------------------------------------
     Free a pool and temporary objects it refers to (callable from C)
   -------------------------------------------------------------------------- */
{
  ns_release_object (pool);
}


static BOOL
ns_menu_bar_should_be_hidden (void)
/* True, if the menu bar should be hidden.  */
{
  return !NILP (ns_auto_hide_menu_bar)
    && [NSApp respondsToSelector:@selector(setPresentationOptions:)];
}


struct EmacsMargins
{
  CGFloat top;
  CGFloat bottom;
  CGFloat left;
  CGFloat right;
};


static struct EmacsMargins
ns_screen_margins (NSScreen *screen)
/* The parts of SCREEN used by the operating system.  */
{
  NSTRACE ("ns_screen_margins");

  struct EmacsMargins margins;

  NSRect screenFrame = [screen frame];
  NSRect screenVisibleFrame = [screen visibleFrame];

  /* Sometimes, visibleFrame isn't up-to-date with respect to a hidden
     menu bar, check this explicitly.  */
  if (ns_menu_bar_should_be_hidden())
    {
      margins.top = 0;
    }
  else
    {
      CGFloat frameTop = screenFrame.origin.y + screenFrame.size.height;
      CGFloat visibleFrameTop = (screenVisibleFrame.origin.y
                                 + screenVisibleFrame.size.height);

      margins.top = frameTop - visibleFrameTop;
    }

  {
    CGFloat frameRight = screenFrame.origin.x + screenFrame.size.width;
    CGFloat visibleFrameRight = (screenVisibleFrame.origin.x
                                 + screenVisibleFrame.size.width);
    margins.right = frameRight - visibleFrameRight;
  }

  margins.bottom = screenVisibleFrame.origin.y - screenFrame.origin.y;
  margins.left   = screenVisibleFrame.origin.x - screenFrame.origin.x;

  NSTRACE_MSG ("left:%g right:%g top:%g bottom:%g",
               margins.left,
               margins.right,
               margins.top,
               margins.bottom);

  return margins;
}


/* A screen margin between 1 and DOCK_IGNORE_LIMIT (inclusive) is
   assumed to contain a hidden dock.  macOS currently use 4 pixels for
   this, however, to be future compatible, a larger value is used.  */
#define DOCK_IGNORE_LIMIT 6

static struct EmacsMargins
ns_screen_margins_ignoring_hidden_dock (NSScreen *screen)
/* The parts of SCREEN used by the operating system, excluding the parts
   reserved for a hidden dock.  */
{
  NSTRACE ("ns_screen_margins_ignoring_hidden_dock");

  struct EmacsMargins margins = ns_screen_margins(screen);

  /* macOS (currently) reserved 4 pixels along the edge where a hidden
     dock is located.  Unfortunately, it's not possible to find the
     location and information about if the dock is hidden.  Instead,
     it is assumed that if the margin of an edge is less than
     DOCK_IGNORE_LIMIT, it contains a hidden dock.  */
  if (margins.left <= DOCK_IGNORE_LIMIT)
    {
      margins.left = 0;
    }
  if (margins.right <= DOCK_IGNORE_LIMIT)
    {
      margins.right = 0;
    }
  if (margins.top <= DOCK_IGNORE_LIMIT)
    {
      margins.top = 0;
    }
  /* Note: This doesn't occur in current versions of macOS, but
     included for completeness and future compatibility.  */
  if (margins.bottom <= DOCK_IGNORE_LIMIT)
    {
      margins.bottom = 0;
    }

  NSTRACE_MSG ("left:%g right:%g top:%g bottom:%g",
               margins.left,
               margins.right,
               margins.top,
               margins.bottom);

  return margins;
}


static CGFloat
ns_menu_bar_height (NSScreen *screen)
/* The height of the menu bar, if visible.

   Note: Don't use this when fullscreen is enabled -- the screen
   sometimes includes, sometimes excludes the menu bar area.  */
{
  struct EmacsMargins margins = ns_screen_margins(screen);

  CGFloat res = margins.top;

  NSTRACE ("ns_menu_bar_height " NSTRACE_FMT_RETURN " %.0f", res);

  return res;
}


/* Get the frame rect, in system coordinates, of the parent window or,
   if there is no parent window, the main screen.  */
static inline NSRect
ns_parent_window_rect (struct frame *f)
{
  NSRect parentRect;

  if (FRAME_PARENT_FRAME (f) != NULL)
    {
      EmacsView *parentView = FRAME_NS_VIEW (FRAME_PARENT_FRAME (f));
      parentRect = [parentView convertRect:[parentView frame]
                                    toView:nil];
      parentRect = [[parentView window] convertRectToScreen:parentRect];
    }
  else
    parentRect = [[[NSScreen screens] objectAtIndex:0] frame];

  return parentRect;
}

/* Calculate system coordinates of the left and top of the parent
   window or, if there is no parent window, the main screen.  */
#define NS_PARENT_WINDOW_LEFT_POS(f) NSMinX (ns_parent_window_rect (f))
#define NS_PARENT_WINDOW_TOP_POS(f) NSMaxY (ns_parent_window_rect (f))


static NSRect
ns_row_rect (struct window *w, struct glyph_row *row,
               enum glyph_row_area area)
/* Get the row as an NSRect.  */
{
  NSRect rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  rect.origin.x = window_x;
  rect.origin.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  rect.origin.y = max (rect.origin.y, window_y);
  rect.size.width = window_width;
  rect.size.height = row->visible_height;

  return rect;
}


double
ns_frame_scale_factor (struct frame *f)
{
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED > 1060
  return [[FRAME_NS_VIEW (f) window] backingScaleFactor];
#else
  return [[FRAME_NS_VIEW (f) window] userSpaceScaleFactor];
#endif
}


/* ==========================================================================

    Focus (clipping) and screen update

   ========================================================================== */

//
// Window constraining
// -------------------
//
// To ensure that the windows are not placed under the menu bar, they
// are typically moved by the call-back constrainFrameRect. However,
// by overriding it, it's possible to inhibit this, leaving the window
// in it's original position.
//
// It's possible to hide the menu bar. However, technically, it's only
// possible to hide it when the application is active. To ensure that
// this work properly, the menu bar and window constraining are
// deferred until the application becomes active.
//
// Even though it's not possible to manually move a window above the
// top of the screen, it is allowed if it's done programmatically,
// when the menu is hidden. This allows the editable area to cover the
// full screen height.
//
// Test cases
// ----------
//
// Use the following extra files:
//
//    init.el:
//       ;; Hide menu and place frame slightly above the top of the screen.
//       (setq ns-auto-hide-menu-bar t)
//       (set-frame-position (selected-frame) 0 -20)
//
// Test 1:
//
//    emacs -Q -l init.el
//
//    Result: No menu bar, and the title bar should be above the screen.
//
// Test 2:
//
//    emacs -Q
//
//    Result: Menu bar visible, frame placed immediately below the menu.
//

static NSRect constrain_frame_rect(NSRect frameRect, bool isFullscreen)
{
  NSTRACE ("constrain_frame_rect(" NSTRACE_FMT_RECT ")",
             NSTRACE_ARG_RECT (frameRect));

  // --------------------
  // Collect information about the screen the frame is covering.
  //

  NSArray *screens = [NSScreen screens];
  NSUInteger nr_screens = [screens count];

  int i;

  // The height of the menu bar, if present in any screen the frame is
  // displayed in.
  int menu_bar_height = 0;

  // A rectangle covering all the screen the frame is displayed in.
  NSRect multiscreenRect = NSMakeRect(0, 0, 0, 0);
  for (i = 0; i < nr_screens; ++i )
    {
      NSScreen *s = [screens objectAtIndex: i];
      NSRect scrRect = [s frame];

      NSTRACE_MSG ("Screen %d: " NSTRACE_FMT_RECT,
                   i, NSTRACE_ARG_RECT (scrRect));

      if (NSIntersectionRect (frameRect, scrRect).size.height != 0)
        {
          multiscreenRect = NSUnionRect (multiscreenRect, scrRect);

          if (!isFullscreen)
            {
              CGFloat screen_menu_bar_height = ns_menu_bar_height (s);
              menu_bar_height = max(menu_bar_height, screen_menu_bar_height);
            }
        }
    }

  NSTRACE_RECT ("multiscreenRect", multiscreenRect);

  NSTRACE_MSG ("menu_bar_height: %d", menu_bar_height);

  if (multiscreenRect.size.width == 0
      || multiscreenRect.size.height == 0)
    {
      // Failed to find any monitor, give up.
      NSTRACE_MSG ("multiscreenRect empty");
      NSTRACE_RETURN_RECT (frameRect);
      return frameRect;
    }


  // --------------------
  // Find a suitable placement.
  //

  if (ns_menu_bar_should_be_hidden())
    {
      // When the menu bar is hidden, the user may place part of the
      // frame above the top of the screen, for example to hide the
      // title bar.
      //
      // Hence, keep the original position.
    }
  else
    {
      // Ensure that the frame is below the menu bar, or below the top
      // of the screen.
      //
      // This assume that the menu bar is placed at the top in the
      // rectangle that covers the monitors.  (It doesn't have to be,
      // but if it's not it's hard to do anything useful.)
      CGFloat topOfWorkArea = (multiscreenRect.origin.y
                               + multiscreenRect.size.height
                               - menu_bar_height);

      CGFloat topOfFrame = frameRect.origin.y + frameRect.size.height;
      if (topOfFrame > topOfWorkArea)
        {
          frameRect.origin.y -= topOfFrame - topOfWorkArea;
          NSTRACE_RECT ("After placement adjust", frameRect);
        }
    }

  // Include the following section to restrict frame to the screens.
  // (If so, update it to allow the frame to stretch down below the
  // screen.)
#if 0
  // --------------------
  // Ensure frame doesn't stretch below the screens.
  //

  CGFloat diff = multiscreenRect.origin.y - frameRect.origin.y;

  if (diff > 0)
    {
      frameRect.origin.y = multiscreenRect.origin.y;
      frameRect.size.height -= diff;
    }
#endif

  NSTRACE_RETURN_RECT (frameRect);
  return frameRect;
}


static void
ns_constrain_all_frames (void)
/* --------------------------------------------------------------------------
     Ensure that the menu bar doesn't cover any frames.
   -------------------------------------------------------------------------- */
{
  Lisp_Object tail, frame;

  NSTRACE ("ns_constrain_all_frames");

  block_input ();

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_NS_P (f))
        {
          EmacsView *view = FRAME_NS_VIEW (f);

          if (![view isFullscreen])
            {
              [[view window]
                setFrame:constrain_frame_rect([[view window] frame], false)
                 display:NO];
            }
        }
    }

  unblock_input ();
}


static void
ns_update_auto_hide_menu_bar (void)
/* --------------------------------------------------------------------------
     Show or hide the menu bar, based on user setting.
   -------------------------------------------------------------------------- */
{
#ifdef NS_IMPL_COCOA
  NSTRACE ("ns_update_auto_hide_menu_bar");

  block_input ();

  if (NSApp != nil && [NSApp isActive])
    {
      // Note, "setPresentationOptions" triggers an error unless the
      // application is active.
      BOOL menu_bar_should_be_hidden = ns_menu_bar_should_be_hidden ();

      if (menu_bar_should_be_hidden != ns_menu_bar_is_hidden)
        {
          NSApplicationPresentationOptions options
            = NSApplicationPresentationDefault;

          if (menu_bar_should_be_hidden)
            options |= NSApplicationPresentationAutoHideMenuBar
              | NSApplicationPresentationAutoHideDock;

          [NSApp setPresentationOptions: options];

          ns_menu_bar_is_hidden = menu_bar_should_be_hidden;

          if (!ns_menu_bar_is_hidden)
            {
              ns_constrain_all_frames ();
            }
        }
    }

  unblock_input ();
#endif
}


static void
ns_update_begin (struct frame *f)
/* --------------------------------------------------------------------------
   Prepare for a grouped sequence of drawing calls
   external (RIF) call; whole frame, called before gui_update_window_begin
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_begin");

  ns_update_auto_hide_menu_bar ();

#ifdef NS_IMPL_COCOA
  if ([view isFullscreen] && [view fsIsNative])
  {
    // Fix reappearing tool bar in fullscreen for Mac OS X 10.7
    BOOL tbar_visible = FRAME_EXTERNAL_TOOL_BAR (f) ? YES : NO;
    NSToolbar *toolbar = [FRAME_NS_VIEW (f) toolbar];
    if (! tbar_visible != ! [toolbar isVisible])
      [toolbar setVisible: tbar_visible];
  }
#endif

  ns_updating_frame = f;
#ifdef NS_DRAW_TO_BUFFER
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  if ([FRAME_NS_VIEW (f) wantsUpdateLayer])
    {
#endif
      [view focusOnDrawingBuffer];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
  else
    {
#endif
#endif /* NS_DRAW_TO_BUFFER */

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      [view lockFocus];
#endif
#if defined (NS_DRAW_TO_BUFFER) && MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
#endif

}


static void
ns_update_end (struct frame *f)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for whole frame, called after gui_update_window_end
   -------------------------------------------------------------------------- */
{
#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  EmacsView *view = FRAME_NS_VIEW (f);
#endif

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_end");

/*   if (f == MOUSE_HL_INFO (f)->mouse_face_mouse_frame) */
  MOUSE_HL_INFO (f)->mouse_face_defer = 0;

#ifdef NS_DRAW_TO_BUFFER
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  if ([FRAME_NS_VIEW (f) wantsUpdateLayer])
    {
#endif
      [FRAME_NS_VIEW (f) unfocusDrawingBuffer];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
  else
    {
#endif
#endif /* NS_DRAW_TO_BUFFER */

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      block_input ();

      [view unlockFocus];
      [[view window] flushWindow];

      unblock_input ();
#endif
#if defined (NS_DRAW_TO_BUFFER) && MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
#endif
  ns_updating_frame = NULL;
}

static void
ns_focus (struct frame *f, NSRect *r, int n)
/* --------------------------------------------------------------------------
   Internal: Focus on given frame.  During small local updates this is used to
     draw, however during large updates, ns_update_begin and ns_update_end are
     called to wrap the whole thing, in which case these calls are stubbed out.
     Except, on GNUstep, we accumulate the rectangle being drawn into, because
     the back end won't do this automatically, and will just end up flushing
     the entire window.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);

  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "ns_focus");
  if (r != NULL)
    {
      NSTRACE_RECT ("r", *r);
    }

  if (f != ns_updating_frame)
    {
#ifdef NS_DRAW_TO_BUFFER
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      if ([FRAME_NS_VIEW (f) wantsUpdateLayer])
        {
#endif
          [view focusOnDrawingBuffer];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
        }
      else
        {
#endif
#endif /* NS_DRAW_TO_BUFFER */

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
          if (view != focus_view)
            {
              if (focus_view != NULL)
                {
                  [focus_view unlockFocus];
                  [[focus_view window] flushWindow];
                }

              if (view)
                [view lockFocus];
              focus_view = view;
            }
#endif
#if defined (NS_DRAW_TO_BUFFER) && MAC_OS_X_VERSION_MIN_REQUIRED < 101400
        }
#endif
    }


  /* clipping */
  if (r)
    {
      [[NSGraphicsContext currentContext] saveGraphicsState];
      if (n == 2)
        NSRectClipList (r, 2);
      else
        NSRectClip (*r);
      gsaved = YES;
    }
}


static void
ns_unfocus (struct frame *f)
/* --------------------------------------------------------------------------
     Internal: Remove focus on given frame
   -------------------------------------------------------------------------- */
{
  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "ns_unfocus");

  if (gsaved)
    {
      [[NSGraphicsContext currentContext] restoreGraphicsState];
      gsaved = NO;
    }

#ifdef NS_DRAW_TO_BUFFER
  #if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  if ([FRAME_NS_VIEW (f) wantsUpdateLayer])
    {
#endif
      if (! ns_updating_frame)
        [FRAME_NS_VIEW (f) unfocusDrawingBuffer];
      [FRAME_NS_VIEW (f) setNeedsDisplay:YES];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
  else
    {
#endif
#endif /* NS_DRAW_TO_BUFFER */

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      if (f != ns_updating_frame)
        {
          if (focus_view != NULL)
            {
              [focus_view unlockFocus];
              [[focus_view window] flushWindow];
              focus_view = NULL;
            }
        }
#endif
#if defined (NS_DRAW_TO_BUFFER) && MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
#endif
}


/* ==========================================================================

    Visible bell and beep.

   ========================================================================== */


// This bell implementation shows the visual bell image asynchronously
// from the rest of Emacs. This is done by adding a NSView to the
// superview of the Emacs window and removing it using a timer.
//
// Unfortunately, some Emacs operations, like scrolling, is done using
// low-level primitives that copy the content of the window, including
// the bell image. To some extent, this is handled by removing the
// image prior to scrolling and marking that the window is in need for
// redisplay.
//
// To test this code, make sure that there is no artifacts of the bell
// image in the following situations. Use a non-empty buffer (like the
// tutorial) to ensure that a scroll is performed:
//
// * Single-window: C-g C-v
//
// * Side-by-windows: C-x 3 C-g C-v
//
// * Windows above each other: C-x 2 C-g C-v

@interface EmacsBell : NSImageView
{
  // Number of currently active bells.
  unsigned int nestCount;
  NSView * mView;
  bool isAttached;
}
- (void)show:(NSView *)view;
- (void)hide;
- (void)remove;
@end

@implementation EmacsBell

- (id)init
{
  NSTRACE ("[EmacsBell init]");
  if ((self = [super init]))
    {
      nestCount = 0;
      isAttached = false;
#ifdef NS_IMPL_GNUSTEP
      // GNUstep doesn't provide named images.  This was reported in
      // 2011, see https://savannah.gnu.org/bugs/?33396
      //
      // As a drop in replacement, a semitransparent gray square is used.
      self.image = [[NSImage alloc] initWithSize:NSMakeSize(32 * 5, 32 * 5)];
      [self.image lockFocus];
      [[NSColor colorForEmacsRed:0.5 green:0.5 blue:0.5 alpha:0.5] set];
      NSRectFill(NSMakeRect(0, 0, 32, 32));
      [self.image unlockFocus];
#else
      self.image = [NSImage imageNamed:NSImageNameCaution];
      [self.image setSize:NSMakeSize(self.image.size.width * 5,
                                     self.image.size.height * 5)];
#endif
    }
  return self;
}

- (void)show:(NSView *)view
{
  NSTRACE ("[EmacsBell show:]");
  NSTRACE_MSG ("nestCount: %u", nestCount);

  // Show the image, unless it's already shown.
  if (nestCount == 0)
    {
      NSRect rect = [view bounds];
      NSPoint pos;
      pos.x = rect.origin.x + (rect.size.width  - self.image.size.width )/2;
      pos.y = rect.origin.y + (rect.size.height - self.image.size.height)/2;

      [self setFrameOrigin:pos];
      [self setFrameSize:self.image.size];

      isAttached = true;
      mView = view;
      [[[view window] contentView] addSubview:self
                                   positioned:NSWindowAbove
                                   relativeTo:nil];
    }

  ++nestCount;

  [self performSelector:@selector(hide) withObject:self afterDelay:0.5];
}


- (void)hide
{
  // Note: Trace output from this method isn't shown, reason unknown.
  // NSTRACE ("[EmacsBell hide]");

  if (nestCount > 0)
    --nestCount;

  // Remove the image once the last bell became inactive.
  if (nestCount == 0)
    {
      [self remove];
    }
}


-(void)remove
{
  NSTRACE ("[EmacsBell remove]");
  if (isAttached)
    {
      NSTRACE_MSG ("removeFromSuperview");
      [self removeFromSuperview];
      mView.needsDisplay = YES;
      isAttached = false;
    }
}

@end


static EmacsBell * bell_view = nil;

static void
ns_ring_bell (struct frame *f)
/* --------------------------------------------------------------------------
     "Beep" routine
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_ring_bell");
  if (visible_bell)
    {
      struct frame *frame = SELECTED_FRAME ();
      NSView *view;

      if (bell_view == nil)
        {
          bell_view = [[EmacsBell alloc] init];
          [bell_view retain];
        }

      block_input ();

      view = FRAME_NS_VIEW (frame);
      if (view != nil)
        {
          [bell_view show:view];
        }

      unblock_input ();
    }
  else
    {
      NSBeep ();
    }
}

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
static void
hide_bell (void)
/* --------------------------------------------------------------------------
     Ensure the bell is hidden.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("hide_bell");

  if (bell_view != nil)
    {
      [bell_view remove];
    }
}
#endif


/* ==========================================================================

    Frame / window manager related functions

   ========================================================================== */

static Lisp_Object
ns_get_focus_frame (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook)
   -------------------------------------------------------------------------- */
{
  Lisp_Object lisp_focus;

  struct frame *focus =  FRAME_DISPLAY_INFO (f)->ns_focus_frame;

  if (!focus)
    return Qnil;

  XSETFRAME (lisp_focus, focus);
  return lisp_focus;
}

static void
ns_focus_frame (struct frame *f, bool noactivate)
/* --------------------------------------------------------------------------
     External (hook)
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (dpyinfo->ns_focus_frame != f)
    {
      EmacsView *view = FRAME_NS_VIEW (f);
      block_input ();
      [NSApp activateIgnoringOtherApps: YES];
      [[view window] makeKeyAndOrderFront: view];
      unblock_input ();
    }
}

static void
ns_raise_frame (struct frame *f, BOOL make_key)
/* --------------------------------------------------------------------------
     Bring window to foreground and if make_key is YES, give it focus.
   -------------------------------------------------------------------------- */
{
  NSView *view;

  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  block_input ();
  if (FRAME_VISIBLE_P (f))
    {
      if (make_key)
        [[view window] makeKeyAndOrderFront: NSApp];
      else
        [[view window] orderFront: NSApp];
    }
  unblock_input ();
}


static void
ns_lower_frame (struct frame *f)
/* --------------------------------------------------------------------------
     Send window to back
   -------------------------------------------------------------------------- */
{
  NSView *view;

  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  block_input ();
  [[view window] orderBack: NSApp];
  unblock_input ();
}


static void
ns_frame_raise_lower (struct frame *f, bool raise)
/* --------------------------------------------------------------------------
     External (hook)
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_frame_raise_lower");

  if (raise)
    ns_raise_frame (f, YES);
  else
    ns_lower_frame (f);
}

static void ns_set_frame_alpha (struct frame *f);

static void
ns_frame_rehighlight (struct frame *frame)
/* --------------------------------------------------------------------------
     External (hook): called on things like window switching within frame
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  struct frame *old_highlight = dpyinfo->highlight_frame;

  NSTRACE ("ns_frame_rehighlight");
  if (dpyinfo->ns_focus_frame)
    {
      dpyinfo->highlight_frame
	= (FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->ns_focus_frame))
           ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->ns_focus_frame))
           : dpyinfo->ns_focus_frame);
      if (!FRAME_LIVE_P (dpyinfo->highlight_frame))
        {
          fset_focus_frame (dpyinfo->ns_focus_frame, Qnil);
          dpyinfo->highlight_frame = dpyinfo->ns_focus_frame;
        }
    }
  else
      dpyinfo->highlight_frame = 0;

  if (dpyinfo->highlight_frame &&
         dpyinfo->highlight_frame != old_highlight)
    {
      if (old_highlight)
	{
          gui_update_cursor (old_highlight, 1);
	  ns_set_frame_alpha (old_highlight);
	}
      if (dpyinfo->highlight_frame)
	{
          gui_update_cursor (dpyinfo->highlight_frame, 1);
          ns_set_frame_alpha (dpyinfo->highlight_frame);
	}
    }
}


void
ns_make_frame_visible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Show the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_make_frame_visible");
  /* XXX: at some points in past this was not needed, as the only place that
     called this (frame.c:Fraise_frame ()) also called raise_lower;
     if this ends up the case again, comment this out again.  */
  if (!FRAME_VISIBLE_P (f))
    {
      EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
      NSWindow *window = [view window];

      SET_FRAME_VISIBLE (f, 1);
      ns_raise_frame (f, ! FRAME_NO_FOCUS_ON_MAP (f));

      /* Making a new frame from a fullscreen frame will make the new frame
         fullscreen also.  So skip handleFS as this will print an error.  */
      if ([view fsIsNative] && [view isFullscreen])
        {
          // maybe it is not necessary to wait
          [view waitFullScreenTransition];
          return;
        }

      if (f->want_fullscreen != FULLSCREEN_NONE)
        {
          block_input ();
          [view handleFS];
          unblock_input ();
        }

      /* Making a frame invisible seems to break the parent->child
         relationship, so reinstate it.  */
      if ([window parentWindow] == nil && FRAME_PARENT_FRAME (f) != NULL)
        {
          NSWindow *parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];

          block_input ();
          [parent addChildWindow: window
                         ordered: NSWindowAbove];
          unblock_input ();

          /* If the parent frame moved while the child frame was
             invisible, the child frame's position won't have been
             updated.  Make sure it's in the right place now.  */
          ns_set_offset(f, f->left_pos, f->top_pos, 0);
        }
    }
}


static void
ns_make_frame_invisible (struct frame *f)
/* --------------------------------------------------------------------------
     Hide the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSView *view;
  NSTRACE ("ns_make_frame_invisible");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  [[view window] orderOut: NSApp];
  SET_FRAME_VISIBLE (f, 0);
  SET_FRAME_ICONIFIED (f, 0);
}

static void
ns_make_frame_visible_invisible (struct frame *f, bool visible)
/* --------------------------------------------------------------------------
     External (hook)
   -------------------------------------------------------------------------- */
{
  if (visible)
    ns_make_frame_visible (f);
  else
    ns_make_frame_invisible (f);
}

void
ns_iconify_frame (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook): Iconify window
   -------------------------------------------------------------------------- */
{
  NSView *view;
  struct ns_display_info *dpyinfo;

  NSTRACE ("ns_iconify_frame");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (dpyinfo->highlight_frame == f)
    dpyinfo->highlight_frame = 0;

  if ([[view window] windowNumber] <= 0)
    {
      /* The window is still deferred.  Make it very small, bring it
         on screen and order it out.  */
      NSRect s = { { 100, 100}, {0, 0} };
      NSRect t;
      t = [[view window] frame];
      [[view window] setFrame: s display: NO];
      [[view window] orderBack: NSApp];
      [[view window] orderOut: NSApp];
      [[view window] setFrame: t display: NO];
    }

  /* Processing input while Emacs is being minimized can cause a
     crash, so block it for the duration.  */
  block_input();
  [[view window] miniaturize: NSApp];
  unblock_input();
}

/* Free resources of frame F.  */

void
ns_free_frame_resources (struct frame *f)
{
  NSView *view;
  struct ns_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;

  NSTRACE ("ns_free_frame_resources");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);
  hlinfo = MOUSE_HL_INFO (f);

  [(EmacsView *)view setWindowClosing: YES]; /* may not have been informed */

  block_input ();

  free_frame_menubar (f);
  free_frame_faces (f);

  if (f == dpyinfo->ns_focus_frame)
    dpyinfo->ns_focus_frame = 0;
  if (f == dpyinfo->highlight_frame)
    dpyinfo->highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  if (f->output_data.ns->miniimage != nil)
    [f->output_data.ns->miniimage release];

  [[view window] close];
  [view release];

  xfree (f->output_data.ns);
  f->output_data.ns = NULL;

  unblock_input ();
}

static void
ns_destroy_window (struct frame *f)
/* --------------------------------------------------------------------------
     External: Delete the window
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_destroy_window");

  check_window_system (f);

  /* If this frame has a parent window, detach it as not doing so can
     cause a crash in GNUStep.  */
  if (FRAME_PARENT_FRAME (f) != NULL)
    {
      NSWindow *child = [FRAME_NS_VIEW (f) window];
      NSWindow *parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];

      [parent removeChildWindow: child];
    }

  [[FRAME_NS_VIEW (f) window] close];
  ns_free_frame_resources (f);
  ns_window_num--;
}


void
ns_set_offset (struct frame *f, int xoff, int yoff, int change_grav)
/* --------------------------------------------------------------------------
     External: Position the window
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSRect windowFrame = [[view window] frame];
  NSPoint topLeft;

  NSTRACE ("ns_set_offset");

  block_input ();

  if (FRAME_PARENT_FRAME (f))
    {
      /* Convert the parent frame's view rectangle into screen
         coords.  */
      EmacsView *parentView = FRAME_NS_VIEW (FRAME_PARENT_FRAME (f));
      NSRect parentRect = [parentView convertRect:[parentView frame]
                                           toView:nil];
      parentRect = [[parentView window] convertRectToScreen:parentRect];

      if (f->size_hint_flags & XNegative)
        topLeft.x = NSMaxX (parentRect) - NSWidth (windowFrame) + xoff;
      else
        topLeft.x = NSMinX (parentRect) + xoff;

      if (f->size_hint_flags & YNegative)
        topLeft.y = NSMinY (parentRect) + NSHeight (windowFrame) - yoff;
      else
        topLeft.y = NSMaxY (parentRect) - yoff;
    }
  else
    {
      /* If there is no parent frame then just convert to screen
         coordinates, UNLESS we have negative values, in which case I
         think it's best to position from the bottom and right of the
         current screen rather than the main screen or whole
         display.  */
      NSRect screenFrame = [[[view window] screen] frame];

      if (f->size_hint_flags & XNegative)
        topLeft.x = NSMaxX (screenFrame) - NSWidth (windowFrame) + xoff;
      else
        topLeft.x = xoff;

      if (f->size_hint_flags & YNegative)
        topLeft.y = NSMinY (screenFrame) + NSHeight (windowFrame) - yoff;
      else
        topLeft.y = NSMaxY ([[[NSScreen screens] objectAtIndex:0] frame]) - yoff;

#ifdef NS_IMPL_GNUSTEP
      /* Don't overlap the menu.

         FIXME: Surely there's a better way than just hardcoding 100
         in here?  */
      topLeft.x = 100;
#endif
    }

  NSTRACE_POINT ("setFrameTopLeftPoint", topLeft);
  [[view window] setFrameTopLeftPoint:topLeft];
  f->size_hint_flags &= ~(XNegative|YNegative);

  unblock_input ();
}


static void
ns_set_window_size (struct frame *f,
                    bool change_gravity,
                    int width,
                    int height,
                    bool pixelwise)
/* --------------------------------------------------------------------------
     Adjust window pixel size based on given character grid size
     Impl is a bit more complex than other terms, need to do some
     internal clipping.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSWindow *window = [view window];
  NSRect wr = [window frame];
  int pixelwidth, pixelheight;
  int orig_height = wr.size.height;

  NSTRACE ("ns_set_window_size");

  if (view == nil)
    return;

  NSTRACE_RECT ("current", wr);
  NSTRACE_MSG ("Width:%d Height:%d Pixelwise:%d", width, height, pixelwise);
  NSTRACE_MSG ("Font %d x %d", FRAME_COLUMN_WIDTH (f), FRAME_LINE_HEIGHT (f));

  block_input ();

  if (pixelwise)
    {
      pixelwidth = FRAME_TEXT_TO_PIXEL_WIDTH (f, width);
      pixelheight = FRAME_TEXT_TO_PIXEL_HEIGHT (f, height);
    }
  else
    {
      pixelwidth =  FRAME_TEXT_COLS_TO_PIXEL_WIDTH   (f, width);
      pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, height);
    }

  wr.size.width = pixelwidth + f->border_width;
  wr.size.height = pixelheight;
  if (! [view isFullscreen])
    wr.size.height += FRAME_NS_TITLEBAR_HEIGHT (f)
      + FRAME_TOOLBAR_HEIGHT (f);

  /* Do not try to constrain to this screen.  We may have multiple
     screens, and want Emacs to span those.  Constraining to screen
     prevents that, and that is not nice to the user.  */
 if (f->output_data.ns->zooming)
   f->output_data.ns->zooming = 0;
 else
   wr.origin.y += orig_height - wr.size.height;

 frame_size_history_add
   (f, Qx_set_window_size_1, width, height,
    list5 (Fcons (make_fixnum (pixelwidth), make_fixnum (pixelheight)),
	   Fcons (make_fixnum (wr.size.width), make_fixnum (wr.size.height)),
	   make_fixnum (f->border_width),
	   make_fixnum (FRAME_NS_TITLEBAR_HEIGHT (f)),
	   make_fixnum (FRAME_TOOLBAR_HEIGHT (f))));

 /* Usually it seems safe to delay changing the frame size, but when a
    series of actions are taken with no redisplay between them then we
    can end up using old values so don't delay here.  */
 change_frame_size (f,
                    FRAME_PIXEL_TO_TEXT_WIDTH (f, pixelwidth),
                    FRAME_PIXEL_TO_TEXT_HEIGHT (f, pixelheight),
                    0, NO, 0, 1);

  [window setFrame:wr display:NO];

  unblock_input ();
}

#ifdef NS_IMPL_COCOA
void
ns_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* --------------------------------------------------------------------------
     Set frame F's `undecorated' parameter.  If non-nil, F's window-system
     window is drawn without decorations, title, minimize/maximize boxes
     and external borders.  This usually means that the window cannot be
     dragged, resized, iconified, maximized or deleted with the mouse.  If
     nil, draw the frame with all the elements listed above unless these
     have been suspended via window manager settings.

     GNUStep cannot change an existing window's style.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  NSTRACE ("ns_set_undecorated");

  if (!EQ (new_value, old_value))
    {
      block_input ();

      if (NILP (new_value))
        {
          FRAME_UNDECORATED (f) = false;
          [window setStyleMask: ((window.styleMask | FRAME_DECORATED_FLAGS)
                                  ^ FRAME_UNDECORATED_FLAGS)];

          [view createToolbar: f];
        }
      else
        {
          [window setToolbar: nil];
          /* Do I need to release the toolbar here?  */

          FRAME_UNDECORATED (f) = true;
          [window setStyleMask: ((window.styleMask | FRAME_UNDECORATED_FLAGS)
                                 ^ FRAME_DECORATED_FLAGS)];
        }

      /* At this point it seems we don't have an active NSResponder,
         so some key presses (TAB) are swallowed by the system.  */
      [window makeFirstResponder: view];

      unblock_input ();
    }
}
#endif /* NS_IMPL_COCOA */

void
ns_set_parent_frame (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* --------------------------------------------------------------------------
     Set frame F's `parent-frame' parameter.  If non-nil, make F a child
     frame of the frame specified by that parameter.  Technically, this
     makes F's window-system window a child window of the parent frame's
     window-system window.  If nil, make F's window-system window a
     top-level window--a child of its display's root window.

     A child frame's `left' and `top' parameters specify positions
     relative to the top-left corner of its parent frame's native
     rectangle.  On macOS moving a parent frame moves all its child
     frames too, keeping their position relative to the parent
     unaltered.  When a parent frame is iconified or made invisible, its
     child frames are made invisible.  When a parent frame is deleted,
     its child frames are deleted too.

     Whether a child frame has a tool bar may be window-system or window
     manager dependent.  It's advisable to disable it via the frame
     parameter settings.

     Some window managers may not honor this parameter.
   -------------------------------------------------------------------------- */
{
  struct frame *p = NULL;
  NSWindow *parent, *child;

  NSTRACE ("ns_set_parent_frame");

  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_NS_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      error ("Invalid specification of `parent-frame'");
    }

  if (p != FRAME_PARENT_FRAME (f))
    {
      block_input ();
      child = [FRAME_NS_VIEW (f) window];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
      EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
#endif

      if ([child parentWindow] != nil)
        {
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
          parent = [child parentWindow];
#endif

          [[child parentWindow] removeChildWindow:child];
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
          if ([child respondsToSelector:@selector(setAccessibilitySubrole:)])
#endif
              [child setAccessibilitySubrole:NSAccessibilityStandardWindowSubrole];
#endif
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
          if (NILP (new_value))
            {
              NSTRACE ("child setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary");
              [child setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
              // if current parent in fullscreen and no new parent make child fullscreen
              while (parent) {
                if (([parent styleMask] & NSWindowStyleMaskFullScreen) != 0)
                  {
                    [view toggleFullScreen:child];
                    break;
                  }
                // check all parents
                parent = [parent parentWindow];
              }
            }
#endif
        }

      if (!NILP (new_value))
        {
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
          // child frame must not be in fullscreen
          if ([view fsIsNative] && [view isFullscreen])
            {
              // in case child is going fullscreen
              [view waitFullScreenTransition];
              [view toggleFullScreen:child];
            }
          NSTRACE ("child setCollectionBehavior:NSWindowCollectionBehaviorFullScreenAuxiliary");
          [child setCollectionBehavior:NSWindowCollectionBehaviorFullScreenAuxiliary];
#endif
          parent = [FRAME_NS_VIEW (p) window];

          [parent addChildWindow: child
                         ordered: NSWindowAbove];
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
          if ([child respondsToSelector:@selector(setAccessibilitySubrole:)])
#endif
              [child setAccessibilitySubrole:NSAccessibilityFloatingWindowSubrole];
#endif
        }

      unblock_input ();

      fset_parent_frame (f, new_value);
    }
}

void
ns_set_no_focus_on_map (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `no-focus-on-map' parameter which, if non-nil, means
 * that F's window-system window does not want to receive input focus
 * when it is mapped.  (A frame's window is mapped when the frame is
 * displayed for the first time and when the frame changes its state
 * from `iconified' or `invisible' to `visible'.)
 *
 * Some window managers may not honor this parameter.  */
{
  NSTRACE ("ns_set_no_focus_on_map");

  if (!EQ (new_value, old_value))
    {
      FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
    }
}

void
ns_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/*  Set frame F's `no-accept-focus' parameter which, if non-nil, hints
 * that F's window-system window does not want to receive input focus
 * via mouse clicks or by moving the mouse into it.
 *
 * If non-nil, this may have the unwanted side-effect that a user cannot
 * scroll a non-selected frame with the mouse.
 *
 * Some window managers may not honor this parameter.  */
{
  NSTRACE ("ns_set_no_accept_focus");

  if (!EQ (new_value, old_value))
    FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
}

void
ns_set_z_group (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `z-group' parameter.  If `above', F's window-system
   window is displayed above all windows that do not have the `above'
   property set.  If nil, F's window is shown below all windows that
   have the `above' property set and above all windows that have the
   `below' property set.  If `below', F's window is displayed below
   all windows that do.

   Some window managers may not honor this parameter.  */
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  NSTRACE ("ns_set_z_group");

  if (NILP (new_value))
    {
      window.level = NSNormalWindowLevel;
      FRAME_Z_GROUP (f) = z_group_none;
    }
  else if (EQ (new_value, Qabove))
    {
      window.level = NSNormalWindowLevel + 1;
      FRAME_Z_GROUP (f) = z_group_above;
    }
  else if (EQ (new_value, Qabove_suspended))
    {
      /* Not sure what level this should be.  */
      window.level = NSNormalWindowLevel + 1;
      FRAME_Z_GROUP (f) = z_group_above_suspended;
    }
  else if (EQ (new_value, Qbelow))
    {
      window.level = NSNormalWindowLevel - 1;
      FRAME_Z_GROUP (f) = z_group_below;
    }
  else
    error ("Invalid z-group specification");
}

#ifdef NS_IMPL_COCOA
void
ns_set_appearance (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  EmacsWindow *window = (EmacsWindow *)[view window];

  NSTRACE ("ns_set_appearance");

  if (NSAppKitVersionNumber < NSAppKitVersionNumber10_10)
    return;

  if (EQ (new_value, Qdark))
    FRAME_NS_APPEARANCE (f) = ns_appearance_vibrant_dark;
  else if (EQ (new_value, Qlight))
    FRAME_NS_APPEARANCE (f) = ns_appearance_aqua;
  else
    FRAME_NS_APPEARANCE (f) = ns_appearance_system_default;

  [window setAppearance];
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 101000 */
}

void
ns_set_transparent_titlebar (struct frame *f, Lisp_Object new_value,
                             Lisp_Object old_value)
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  NSTRACE ("ns_set_transparent_titlebar");

  if ([window respondsToSelector: @selector(titlebarAppearsTransparent)]
      && !EQ (new_value, old_value))
    {
      window.titlebarAppearsTransparent = !NILP (new_value);
      FRAME_NS_TRANSPARENT_TITLEBAR (f) = !NILP (new_value);
    }
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 101000 */
}
#endif /* NS_IMPL_COCOA */

static void
ns_fullscreen_hook (struct frame *f)
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);

  NSTRACE ("ns_fullscreen_hook");

  if (!FRAME_VISIBLE_P (f))
    return;

   if (! [view fsIsNative] && f->want_fullscreen == FULLSCREEN_BOTH)
    {
      /* Old style fs don't initiate correctly if created from
         init/default-frame alist, so use a timer (not nice...).  */
      [NSTimer scheduledTimerWithTimeInterval: 0.5 target: view
                                     selector: @selector (handleFS)
                                     userInfo: nil repeats: NO];
      return;
    }

  block_input ();
  [view handleFS];
  unblock_input ();
}

/* ==========================================================================

    Color management

   ========================================================================== */


NSColor *
ns_lookup_indexed_color (unsigned long idx, struct frame *f)
{
  struct ns_color_table *color_table = FRAME_DISPLAY_INFO (f)->color_table;
  if (idx < 1 || idx >= color_table->avail)
    return nil;
  return color_table->colors[idx];
}


unsigned long
ns_index_color (NSColor *color, struct frame *f)
{
  struct ns_color_table *color_table = FRAME_DISPLAY_INFO (f)->color_table;
  ptrdiff_t idx;
  ptrdiff_t i;

  if (!color_table->colors)
    {
      color_table->size = NS_COLOR_CAPACITY;
      color_table->avail = 1; /* skip idx=0 as marker */
      color_table->colors = xmalloc (color_table->size * sizeof (NSColor *));
      color_table->colors[0] = nil;
      color_table->empty_indices = [[NSMutableSet alloc] init];
    }

  /* Do we already have this color?  */
  for (i = 1; i < color_table->avail; i++)
    if (color_table->colors[i] && [color_table->colors[i] isEqual: color])
      return i;

  if ([color_table->empty_indices count] > 0)
    {
      NSNumber *index = [color_table->empty_indices anyObject];
      [color_table->empty_indices removeObject: index];
      idx = [index unsignedLongValue];
    }
  else
    {
      if (color_table->avail == color_table->size)
	color_table->colors =
	  xpalloc (color_table->colors, &color_table->size, 1,
		   min (ULONG_MAX, PTRDIFF_MAX), sizeof *color_table->colors);
      idx = color_table->avail++;
    }

  color_table->colors[idx] = color;
  [color retain];
  /* fprintf(stderr, "color_table: allocated %d\n",idx); */
  return idx;
}


static int
ns_get_color (const char *name, NSColor **col)
/* --------------------------------------------------------------------------
     Parse a color name
   -------------------------------------------------------------------------- */
/* On *Step, we attempt to mimic the X11 platform here, down to installing an
   X11 rgb.txt-compatible color list in Emacs.clr (see ns_term_init()).
   See https://lists.gnu.org/r/emacs-devel/2009-07/msg01203.html.  */
{
  NSColor *new = nil;
  NSString *nsname = [NSString stringWithUTF8String: name];

  NSTRACE ("ns_get_color(%s, **)", name);

  block_input ();

  if ([nsname isEqualToString: @"ns_selection_bg_color"])
    {
#ifdef NS_IMPL_COCOA
      NSString *defname = [[NSUserDefaults standardUserDefaults]
                            stringForKey: @"AppleHighlightColor"];
      if (defname != nil)
        nsname = defname;
      else
#endif
      if ((new = [NSColor selectedTextBackgroundColor]) != nil)
        {
          *col = [new colorUsingDefaultColorSpace];
          unblock_input ();
          return 0;
        }
      else
        nsname = NS_SELECTION_BG_COLOR_DEFAULT;

      name = [nsname UTF8String];
    }
  else if ([nsname isEqualToString: @"ns_selection_fg_color"])
    {
      /* NOTE: macOS applications normally don't set foreground
         selection, but text may be unreadable if we don't.  */
      if ((new = [NSColor selectedTextColor]) != nil)
        {
          *col = [new colorUsingDefaultColorSpace];
          unblock_input ();
          return 0;
        }

      nsname = NS_SELECTION_FG_COLOR_DEFAULT;
      name = [nsname UTF8String];
    }

  /* First, check for some sort of numeric specification.  */
  unsigned short r16, g16, b16;
  if (parse_color_spec (name, &r16, &g16, &b16))
    {
      *col = [NSColor colorForEmacsRed: r16 / 65535.0
                                 green: g16 / 65535.0
                                  blue: b16 / 65535.0
                                 alpha: 1.0];
      unblock_input ();
      return 0;
    }
  else if (name[0] == '0' || name[0] == '1' || name[0] == '.')
    {
      /* RGB decimal */
      NSScanner *scanner = [NSScanner scannerWithString: nsname];
      float r, g, b;
      if (   [scanner scanFloat: &r] && r >= 0 && r <= 1
          && [scanner scanFloat: &g] && g >= 0 && g <= 1
          && [scanner scanFloat: &b] && b >= 0 && b <= 1)
        {
          *col = [NSColor colorForEmacsRed: r green: g blue: b alpha: 1.0];
          unblock_input ();
          return 0;
        }
    }

  /* Otherwise, color is expected to be from a list */
  {
    NSEnumerator *lenum, *cenum;
    NSString *name;
    NSColorList *clist;

#ifdef NS_IMPL_GNUSTEP
    /* XXX: who is wrong, the requestor or the implementation?  */
    if ([nsname compare: @"Highlight" options: NSCaseInsensitiveSearch]
        == NSOrderedSame)
      nsname = @"highlightColor";
#endif

    lenum = [[NSColorList availableColorLists] objectEnumerator];
    while ( (clist = [lenum nextObject]) && new == nil)
      {
        cenum = [[clist allKeys] objectEnumerator];
        while ( (name = [cenum nextObject]) && new == nil )
          {
            if ([name compare: nsname
                      options: NSCaseInsensitiveSearch] == NSOrderedSame )
              new = [clist colorWithKey: name];
          }
      }
  }

  if (new)
    *col = [new colorUsingDefaultColorSpace];
  unblock_input ();
  return new ? 0 : 1;
}


int
ns_lisp_to_color (Lisp_Object color, NSColor **col)
/* --------------------------------------------------------------------------
     Convert a Lisp string object to a NS color.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_lisp_to_color");
  if (STRINGP (color))
    return ns_get_color (SSDATA (color), col);
  else if (SYMBOLP (color))
    return ns_get_color (SSDATA (SYMBOL_NAME (color)), col);
  return 1;
}

/* Convert an index into the color table into an RGBA value.  Used in
   xdisp.c:extend_face_to_end_of_line when comparing faces and frame
   color values.  */

unsigned long
ns_color_index_to_rgba(int idx, struct frame *f)
{
  NSColor *col;
  col = ns_lookup_indexed_color (idx, f);

  EmacsCGFloat r, g, b, a;
  [col getRed: &r green: &g blue: &b alpha: &a];

  return ARGB_TO_ULONG((unsigned long) (a * 255),
                       (unsigned long) (r * 255),
                       (unsigned long) (g * 255),
                       (unsigned long) (b * 255));
}

void
ns_query_color(void *col, Emacs_Color *color_def, bool setPixel)
/* --------------------------------------------------------------------------
         Get ARGB values out of NSColor col and put them into color_def.
         If setPixel, set the pixel to a concatenated version.
         and set color_def pixel to the resulting index.
   -------------------------------------------------------------------------- */
{
  EmacsCGFloat r, g, b, a;

  [((NSColor *)col) getRed: &r green: &g blue: &b alpha: &a];
  color_def->red   = r * 65535;
  color_def->green = g * 65535;
  color_def->blue  = b * 65535;

  if (setPixel == YES)
    color_def->pixel
      = ARGB_TO_ULONG((unsigned long) (a * 255),
                      (unsigned long) (r * 255),
                      (unsigned long) (g * 255),
                      (unsigned long) (b * 255));
}

bool
ns_defined_color (struct frame *f,
                  const char *name,
                  Emacs_Color *color_def,
                  bool alloc,
                  bool makeIndex)
/* --------------------------------------------------------------------------
         Return true if named color found, and set color_def rgb accordingly.
         If makeIndex and alloc are nonzero put the color in the color_table,
         and set color_def pixel to the resulting index.
         If makeIndex is zero, set color_def pixel to ARGB.
         Return false if not found.
   -------------------------------------------------------------------------- */
{
  NSColor *col;
  NSTRACE_WHEN (NSTRACE_GROUP_COLOR, "ns_defined_color");

  block_input ();
  if (ns_get_color (name, &col) != 0) /* Color not found  */
    {
      unblock_input ();
      return 0;
    }
  if (makeIndex && alloc)
    color_def->pixel = ns_index_color (col, f);
  ns_query_color (col, color_def, !makeIndex);
  unblock_input ();
  return 1;
}

static void
ns_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
/* --------------------------------------------------------------------------
     External (hook): Store F's background color into *BGCOLOR
   -------------------------------------------------------------------------- */
{
  ns_query_color (FRAME_BACKGROUND_COLOR (f), bgcolor, true);
}

static void
ns_set_frame_alpha (struct frame *f)
/* --------------------------------------------------------------------------
     change the entire-frame transparency
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  double alpha = 1.0;
  double alpha_min = 1.0;

  NSTRACE ("ns_set_frame_alpha");

  if (dpyinfo->highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (FIXNUMP (Vframe_alpha_lower_limit))
    alpha_min = (XFIXNUM (Vframe_alpha_lower_limit)) / 100.0;

  if (alpha < 0.0)
    return;
  else if (1.0 < alpha)
    alpha = 1.0;
  else if (0.0 <= alpha && alpha < alpha_min && alpha_min <= 1.0)
    alpha = alpha_min;

#ifdef NS_IMPL_COCOA
  {
    EmacsView *view = FRAME_NS_VIEW (f);
  [[view window] setAlphaValue: alpha];
  }
#endif
}


/* ==========================================================================

    Mouse handling

   ========================================================================== */


void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
/* --------------------------------------------------------------------------
     Programmatically reposition mouse pointer in pixel coordinates
   -------------------------------------------------------------------------- */
{
  NSTRACE ("frame_set_mouse_pixel_position");

  /* FIXME: what about GNUstep?  */
#ifdef NS_IMPL_COCOA
  CGPoint mouse_pos =
    CGPointMake(f->left_pos + pix_x,
                f->top_pos + pix_y +
                FRAME_NS_TITLEBAR_HEIGHT(f) + FRAME_TOOLBAR_HEIGHT(f));
  CGWarpMouseCursorPosition (mouse_pos);
#endif
}

static int
ns_note_mouse_movement (struct frame *frame, CGFloat x, CGFloat y,
			BOOL dragging)
/*   ------------------------------------------------------------------------
     Called by EmacsView on mouseMovement events.  Passes on
     to emacs mainstream code if we moved off of a rect of interest
     known as last_mouse_glyph.
     ------------------------------------------------------------------------ */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  NSRect *r;
  BOOL force_update = NO;

  // NSTRACE ("note_mouse_movement");

  dpyinfo->last_mouse_motion_frame = frame;
  r = &dpyinfo->last_mouse_glyph;

  /* If the last rect is too large (ex, xwidget webkit), update at
     every move, or resizing by dragging modeline or vertical split is
     very hard to make its way.  */
  if (dragging && (r->size.width > 32 || r->size.height > 32))
    force_update = YES;

  /* Note, this doesn't get called for enter/leave, since we don't have a
     position.  Those are taken care of in the corresponding NSView methods.  */

  /* Has movement gone beyond last rect we were tracking?  */
  if (force_update || x < r->origin.x || x >= r->origin.x + r->size.width
      || y < r->origin.y || y >= r->origin.y + r->size.height)
    {
      ns_update_begin (frame);
      frame->mouse_moved = 1;
      note_mouse_highlight (frame, x, y);
      remember_mouse_glyph (frame, x, y, r);
      ns_update_end (frame);
      return 1;
    }

  return 0;
}


static void
ns_mouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
                   enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
                   Time *time)
/* --------------------------------------------------------------------------
    External (hook): inform emacs about mouse position and hit parts.
    If a scrollbar is being dragged, set bar_window, part, x, y, time.
    x & y should be position in the scrollbar (the whole bar, not the handle)
    and length of scrollbar respectively.
   -------------------------------------------------------------------------- */
{
  id view;
  NSPoint view_position;
  Lisp_Object frame, tail;
  struct frame *f = NULL;
  struct ns_display_info *dpyinfo;

  NSTRACE ("ns_mouse_position");

  if (*fp == NULL)
    {
      fputs ("Warning: ns_mouse_position () called with null *fp.\n", stderr);
      return;
    }

  dpyinfo = FRAME_DISPLAY_INFO (*fp);

  block_input ();

  /* Clear the mouse-moved flag for every frame on this display.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_NS_P (XFRAME (frame)))
      XFRAME (frame)->mouse_moved = 0;

  dpyinfo->last_mouse_scroll_bar = nil;

#ifdef NS_IMPL_COCOA
  /* Find the uppermost Emacs frame under the mouse pointer.

     This doesn't work on GNUstep, although in recent versions there
     is compatibility code that makes it a noop.  */

  NSPoint screen_position = [NSEvent mouseLocation];
  NSInteger window_number = 0;
  do
    {
      NSWindow *w;

      window_number = [NSWindow windowNumberAtPoint:screen_position
                        belowWindowWithWindowNumber:window_number];
      w = [NSApp windowWithWindowNumber:window_number];

      if (w && [[w delegate] isKindOfClass:[EmacsView class]])
        f = ((EmacsView *)[w delegate])->emacsframe;
    }
  while (window_number > 0 && !f);
#endif

  if (!f)
    f = dpyinfo->ns_focus_frame ? dpyinfo->ns_focus_frame : SELECTED_FRAME ();

  /* While dropping, use the last mouse frame only if there is no
     currently focused frame.  */
  if (!f
      && EQ (track_mouse, Qdropping)
      && dpyinfo->last_mouse_frame
      && FRAME_LIVE_P (dpyinfo->last_mouse_frame))
    f = dpyinfo->last_mouse_frame;

  if (f && FRAME_NS_P (f))
    {
      view = FRAME_NS_VIEW (f);

      view_position = [[view window] mouseLocationOutsideOfEventStream];
      view_position = [view convertPoint: view_position fromView: nil];
      remember_mouse_glyph (f, view_position.x, view_position.y,
                            &dpyinfo->last_mouse_glyph);
      NSTRACE_POINT ("view_position", view_position);

      if (bar_window) *bar_window = Qnil;
      if (part) *part = scroll_bar_above_handle;

      if (x) XSETINT (*x, lrint (view_position.x));
      if (y) XSETINT (*y, lrint (view_position.y));
      if (time)
        *time = dpyinfo->last_mouse_movement_time;
      *fp = f;
    }

  unblock_input ();
}


static void
ns_frame_up_to_date (struct frame *f)
/* --------------------------------------------------------------------------
    External (hook): Fix up mouse highlighting right after a full update.
    Can't use FRAME_MOUSE_UPDATE due to ns_frame_begin and ns_frame_end calls.
   -------------------------------------------------------------------------- */
{
  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_frame_up_to_date");

  if (FRAME_NS_P (f))
    {
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
      if (f == hlinfo->mouse_face_mouse_frame)
	{
	  block_input ();
	  ns_update_begin(f);
	  note_mouse_highlight (hlinfo->mouse_face_mouse_frame,
				hlinfo->mouse_face_mouse_x,
				hlinfo->mouse_face_mouse_y);
	  ns_update_end(f);
	  unblock_input ();
	}
    }
}


static void
ns_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
/* --------------------------------------------------------------------------
    External (RIF): set frame mouse pointer type.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_define_frame_cursor");
  if (FRAME_POINTER_TYPE (f) != cursor)
    {
      EmacsView *view = FRAME_NS_VIEW (f);
      FRAME_POINTER_TYPE (f) = cursor;
      [[view window] invalidateCursorRectsForView: view];
      /* Redisplay assumes this function also draws the changed frame
         cursor, but this function doesn't, so do it explicitly.  */
      gui_update_cursor (f, 1);
    }
}



/* ==========================================================================

    Keyboard handling

   ========================================================================== */


static unsigned
ns_convert_key (unsigned code)
/* --------------------------------------------------------------------------
    Internal call used by NSView-keyDown.
   -------------------------------------------------------------------------- */
{
  const unsigned last_keysym = ARRAYELTS (convert_ns_to_X_keysym);
  unsigned keysym;
  /* An array would be faster, but less easy to read.  */
  for (keysym = 0; keysym < last_keysym; keysym += 2)
    if (code == convert_ns_to_X_keysym[keysym])
      return 0xFF00 | convert_ns_to_X_keysym[keysym+1];
  return 0;
/* if decide to use keyCode and Carbon table, use this line:
     return code > 0xff ? 0 : 0xFF00 | ns_keycode_to_xkeysym_table[code]; */
}


char *
get_keysym_name (int keysym)
/* --------------------------------------------------------------------------
    Called by keyboard.c.  Not sure if the return val is important, except
    that it be unique.
   -------------------------------------------------------------------------- */
{
  static char value[16];
  NSTRACE ("get_keysym_name");
  sprintf (value, "%d", keysym);
  return value;
}

#ifdef NS_IMPL_COCOA
static Lisp_Object
right_mod (Lisp_Object left, Lisp_Object right)
{
  return EQ (right, Qleft) ? left : right;
}

static bool
nil_or_none (Lisp_Object val)
{
  return NILP (val) || EQ (val, Qnone);
}

static UniChar
ns_get_shifted_character (NSEvent *event)
/* Look up the character corresponding to the key pressed on the
   current keyboard layout and the currently configured shift-like
   modifiers.  This ignores the control-like modifiers that cause
   [event characters] to give us the wrong result.

   Although UCKeyTranslate doesn't require the Carbon framework, some
   of the surrounding paraphernalia does, so this function makes
   Carbon a requirement.  */
{
  static UInt32 dead_key_state;

  /* UCKeyTranslate may return up to 255 characters.  If the buffer
     isn't large enough then it produces an error.  What kind of
     keyboard inputs 255 characters in a single keypress?  */
  UniChar buf[255];
  UniCharCount max_string_length = 255;
  UniCharCount actual_string_length = 0;
  OSStatus result;

  CFDataRef layout_ref = (CFDataRef) TISGetInputSourceProperty
    (TISCopyCurrentKeyboardLayoutInputSource (), kTISPropertyUnicodeKeyLayoutData);
  UCKeyboardLayout* layout = (UCKeyboardLayout*) CFDataGetBytePtr (layout_ref);

  UInt32 flags = [event modifierFlags];
  UInt32 modifiers = (flags & NSEventModifierFlagShift) ? shiftKey : 0;

  NSTRACE ("ns_get_shifted_character");

  if ((flags & NSRightAlternateKeyMask) == NSRightAlternateKeyMask
      && nil_or_none (mod_of_kind (right_mod (ns_alternate_modifier,
                                              ns_right_alternate_modifier),
                                   QCordinary)))
    modifiers |= rightOptionKey;

  if ((flags & NSLeftAlternateKeyMask) == NSLeftAlternateKeyMask
      && nil_or_none (mod_of_kind (ns_alternate_modifier, QCordinary)))
    modifiers |= optionKey;

  if ((flags & NSRightCommandKeyMask) == NSRightCommandKeyMask
      && nil_or_none (mod_of_kind (right_mod (ns_command_modifier,
                                              ns_right_command_modifier),
                                   QCordinary)))
    /* Carbon doesn't differentiate between left and right command
       keys.  */
    modifiers |= cmdKey;

  if ((flags & NSLeftCommandKeyMask) == NSLeftCommandKeyMask
      && nil_or_none (mod_of_kind (ns_command_modifier, QCordinary)))
    modifiers |= cmdKey;

  result = UCKeyTranslate (layout, [event keyCode], kUCKeyActionDown,
                           (modifiers >> 8) & 0xFF, LMGetKbdType (),
                           kUCKeyTranslateNoDeadKeysBit, &dead_key_state,
                           max_string_length, &actual_string_length, buf);

  if (result != 0)
    {
      NSLog(@"Failed to translate character '%@' with modifiers %x",
            [event characters], modifiers);
      return 0;
    }

  /* FIXME: What do we do if more than one code unit is returned?  */
  if (actual_string_length > 0)
    return buf[0];

  return 0;
}
#endif /* NS_IMPL_COCOA */

/* ==========================================================================

    Block drawing operations

   ========================================================================== */


#if 0
/* FIXME: Remove this function. */
static void
ns_redraw_scroll_bars (struct frame *f)
{
  int i;
  id view;
  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];
  NSTRACE ("ns_redraw_scroll_bars");
  for (i =[subviews count]-1; i >= 0; i--)
    {
      view = [subviews objectAtIndex: i];
      if (![view isKindOfClass: [EmacsScroller class]]) continue;
      [view display];
    }
}
#endif


void
ns_clear_frame (struct frame *f)
/* --------------------------------------------------------------------------
      External (hook): Erase the entire frame
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSRect r;

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_clear_frame");

 /* comes on initial frame because we have
    after-make-frame-functions = select-frame */
 if (!FRAME_DEFAULT_FACE (f))
   return;

  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  r = [view bounds];

  block_input ();
  ns_focus (f, &r, 1);
  [ns_lookup_indexed_color (NS_FACE_BACKGROUND
			    (FACE_FROM_ID (f, DEFAULT_FACE_ID)), f) set];
  NSRectFill (r);
  ns_unfocus (f);

  /* as of 2006/11 or so this is now needed */
  /* FIXME: I don't see any reason for this and removing it makes no
     difference here.  Do we need it for GNUstep?  */
  //ns_redraw_scroll_bars (f);
  unblock_input ();
}


static void
ns_clear_frame_area (struct frame *f, int x, int y, int width, int height)
/* --------------------------------------------------------------------------
    External (RIF):  Clear section of frame
   -------------------------------------------------------------------------- */
{
  NSRect r = NSMakeRect (x, y, width, height);
  NSView *view = FRAME_NS_VIEW (f);
  struct face *face = FRAME_DEFAULT_FACE (f);

  if (!view || !face)
    return;

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_clear_frame_area");

  r = NSIntersectionRect (r, [view frame]);
  ns_focus (f, &r, 1);
  [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f) set];

  NSRectFill (r);

  ns_unfocus (f);
  return;
}


static void
ns_scroll_run (struct window *w, struct run *run)
/* --------------------------------------------------------------------------
    External (RIF):  Insert or delete n lines at line vpos.
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  NSTRACE ("ns_scroll_run");

  /* begin copy from other terms */
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
  /* end copy from other terms */

  if (height == 0)
      return;

  block_input ();

  gui_clear_cursor (w);

  {
    NSRect srcRect = NSMakeRect (x, from_y, width, height);
    NSRect dstRect = NSMakeRect (x, to_y, width, height);
    EmacsView *view = FRAME_NS_VIEW (f);

    [view copyRect:srcRect to:dstRect];
#ifdef NS_IMPL_COCOA
    [view setNeedsDisplayInRect:srcRect];
#endif
  }

  unblock_input ();
}


static void
ns_clear_under_internal_border (struct frame *f)
{
  NSTRACE ("ns_clear_under_internal_border");

  if (FRAME_LIVE_P (f) && FRAME_INTERNAL_BORDER_WIDTH (f) > 0)
    {
      int border_width = FRAME_INTERNAL_BORDER_WIDTH (f);
      NSView *view = FRAME_NS_VIEW (f);
      NSRect edge_rect, frame_rect = [view bounds];
      NSRectEdge edge[] = {NSMinXEdge, NSMinYEdge, NSMaxXEdge, NSMaxYEdge};

      int face_id =
        (FRAME_PARENT_FRAME (f)
         ? (!NILP (Vface_remapping_alist)
            ? lookup_basic_face (NULL, f, CHILD_FRAME_BORDER_FACE_ID)
            : CHILD_FRAME_BORDER_FACE_ID)
         : (!NILP (Vface_remapping_alist)
            ? lookup_basic_face (NULL, f, INTERNAL_BORDER_FACE_ID)
            : INTERNAL_BORDER_FACE_ID));
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (!face)
        face = FRAME_DEFAULT_FACE (f);

      /* Sometimes with new frames we reach this point and have no
         face.  I'm not sure why we have a live frame but no face, so
         just give up.  */
      if (!face)
        return;

      ns_focus (f, NULL, 1);
      [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f) set];
      for (int i = 0; i < 4 ; i++)
        {
          NSDivideRect (frame_rect, &edge_rect, &frame_rect, border_width, edge[i]);

          NSRectFill (edge_rect);
        }
      ns_unfocus (f);
    }
}


static void
ns_after_update_window_line (struct window *w, struct glyph_row *desired_row)
/* --------------------------------------------------------------------------
    External (RIF): preparatory to fringe update after text was updated
   -------------------------------------------------------------------------- */
{
  struct frame *f;
  int width, height;

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_after_update_window_line");

  /* begin copy from other terms */
  eassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = 1;

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  */
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
          NSRect r = NSMakeRect (0, y, FRAME_PIXEL_WIDTH (f), height);
          ns_focus (f, &r, 1);

          [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f) set];
          NSRectFill (NSMakeRect (0, y, width, height));
          NSRectFill (NSMakeRect (FRAME_PIXEL_WIDTH (f) - width,
                                  y, width, height));

          ns_unfocus (f);
        }
      else
        {
          ns_clear_frame_area (f, 0, y, width, height);
          ns_clear_frame_area (f,
                               FRAME_PIXEL_WIDTH (f) - width,
                               y, width, height);
        }
      unblock_input ();
    }
}


static void
ns_shift_glyphs_for_insert (struct frame *f,
                           int x, int y, int width, int height,
                           int shift_by)
/* --------------------------------------------------------------------------
    External (RIF): copy an area horizontally, don't worry about clearing src
   -------------------------------------------------------------------------- */
{
  NSRect srcRect = NSMakeRect (x, y, width, height);
  NSRect dstRect = NSMakeRect (x+shift_by, y, width, height);

  NSTRACE ("ns_shift_glyphs_for_insert");

  [FRAME_NS_VIEW (f) copyRect:srcRect to:dstRect];
}



/* ==========================================================================

    Character encoding and metrics

   ========================================================================== */


static void
ns_compute_glyph_string_overhangs (struct glyph_string *s)
/* --------------------------------------------------------------------------
     External (RIF); compute left/right overhang of whole string and set in s
   -------------------------------------------------------------------------- */
{
  struct font *font = s->font;

  if (s->char2b)
    {
      struct font_metrics metrics;
      unsigned int codes[2];
      codes[0] = *(s->char2b);
      codes[1] = *(s->char2b + s->nchars - 1);

      font->driver->text_extents (font, codes, 2, &metrics);
      s->left_overhang = -metrics.lbearing;
      s->right_overhang
	= metrics.rbearing > metrics.width
	? metrics.rbearing - metrics.width : 0;
    }
  else
    {
      s->left_overhang = 0;
#ifdef NS_IMPL_GNUSTEP
      if (EQ (font->driver->type, Qns))
        s->right_overhang = ((struct nsfont_info *)font)->ital ?
          FONT_HEIGHT (font) * 0.2 : 0;
      else
#endif
        s->right_overhang = 0;
    }
}



/* ==========================================================================

    Fringe and cursor drawing

   ========================================================================== */


extern int max_used_fringe_bitmap;
static void
ns_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
                      struct draw_fringe_bitmap_params *p)
/* --------------------------------------------------------------------------
    External (RIF); fringe-related
   -------------------------------------------------------------------------- */
{
  /* Fringe bitmaps comes in two variants, normal and periodic.  A
     periodic bitmap is used to create a continuous pattern.  Since a
     bitmap is rendered one text line at a time, the start offset (dh)
     of the bitmap varies.  Concretely, this is used for the empty
     line indicator.

     For a bitmap, "h + dh" is the full height and is always
     invariant.  For a normal bitmap "dh" is zero.

     For example, when the period is three and the full height is 72
     the following combinations exists:

       h=72 dh=0
       h=71 dh=1
       h=70 dh=2 */

  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = p->face;
  static EmacsImage **bimgs = NULL;
  static int nBimgs = 0;
  NSRect clearRect = NSZeroRect;
  NSRect imageRect = NSZeroRect;
  NSRect rowRect = ns_row_rect (w, row, ANY_AREA);

  NSTRACE_WHEN (NSTRACE_GROUP_FRINGE, "ns_draw_fringe_bitmap");
  NSTRACE_MSG ("which:%d cursor:%d overlay:%d width:%d height:%d period:%d",
               p->which, p->cursor_p, p->overlay_p, p->wd, p->h, p->dh);

  /* grow bimgs if needed */
  if (nBimgs < max_used_fringe_bitmap)
    {
      bimgs = xrealloc (bimgs, max_used_fringe_bitmap * sizeof *bimgs);
      memset (bimgs + nBimgs, 0,
	      (max_used_fringe_bitmap - nBimgs) * sizeof *bimgs);
      nBimgs = max_used_fringe_bitmap;
    }

  /* Work out the rectangle we will composite into.  */
  if (p->which)
    imageRect = NSMakeRect (p->x, p->y, p->wd, p->h);

  /* Work out the rectangle we will need to clear.  Because we're
     compositing rather than blitting, we need to clear the area under
     the image regardless of anything else.  */
  if (p->bx >= 0 && !p->overlay_p)
    {
      clearRect = NSMakeRect (p->bx, p->by, p->nx, p->ny);
      clearRect = NSUnionRect (clearRect, imageRect);
    }
  else
    {
      clearRect = imageRect;
    }

  /* Handle partially visible rows.  */
  clearRect = NSIntersectionRect (clearRect, rowRect);

  /* The visible portion of imageRect will always be contained within
     clearRect.  */
  ns_focus (f, &clearRect, 1);
  if (! NSIsEmptyRect (clearRect))
    {
      NSTRACE_RECT ("clearRect", clearRect);

      [ns_lookup_indexed_color(face->background, f) set];
      NSRectFill (clearRect);
    }

  if (p->which)
    {
      EmacsImage *img = bimgs[p->which - 1];

      if (!img)
        {
          // Note: For "periodic" images, allocate one EmacsImage for
          // the base image, and use it for all dh:s.
          unsigned short *bits = p->bits;
          int full_height = p->h + p->dh;
          int i;
          unsigned char *cbits = xmalloc (full_height);

          for (i = 0; i < full_height; i++)
            cbits[i] = bits[i];
          img = [[EmacsImage alloc] initFromXBM: cbits width: 8
                                         height: full_height
                                             fg: 0 bg: 0
                                   reverseBytes: NO];
          bimgs[p->which - 1] = img;
          xfree (cbits);
        }


      {
        NSColor *bm_color;
        if (!p->cursor_p)
          bm_color = ns_lookup_indexed_color(face->foreground, f);
        else if (p->overlay_p)
          bm_color = ns_lookup_indexed_color(face->background, f);
        else
          bm_color = f->output_data.ns->cursor_color;
        [img setXBMColor: bm_color];
      }

      // Note: For periodic images, the full image height is "h + hd".
      // By using the height h, a suitable part of the image is used.
      NSRect fromRect = NSMakeRect(0, 0, p->wd, p->h);

      NSTRACE_RECT ("fromRect", fromRect);

      [img drawInRect: imageRect
             fromRect: fromRect
            operation: NSCompositingOperationSourceOver
             fraction: 1.0
           respectFlipped: YES
                hints: nil];
    }
  ns_unfocus (f);
}


static void
ns_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
		       int x, int y, enum text_cursor_kinds cursor_type,
		       int cursor_width, bool on_p, bool active_p)
/* --------------------------------------------------------------------------
     External call (RIF): draw cursor.
     Note that CURSOR_WIDTH is meaningful only for (h)bar cursors.
   -------------------------------------------------------------------------- */
{
  NSRect r, s;
  int fx, fy, h, cursor_height;
  struct frame *f = WINDOW_XFRAME (w);
  struct glyph *phys_cursor_glyph;
  struct glyph *cursor_glyph;
  struct face *face;
  NSColor *hollow_color = FRAME_BACKGROUND_COLOR (f);

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */

  NSTRACE ("ns_draw_window_cursor");

  if (!on_p)
    return;

  w->phys_cursor_type = cursor_type;
  w->phys_cursor_on_p = on_p;

  if (cursor_type == NO_CURSOR)
    {
      w->phys_cursor_width = 0;
      return;
    }

  if ((phys_cursor_glyph = get_phys_cursor_glyph (w)) == NULL)
    {
      if (glyph_row->exact_window_width_line_p
          && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
        {
          glyph_row->cursor_in_fringe_p = 1;
          draw_fringe_bitmap (w, glyph_row, 0);
        }
      return;
    }

  /* We draw the cursor (with NSRectFill), then draw the glyph on top
     (other terminals do it the other way round).  We must set
     w->phys_cursor_width to the cursor width.  For bar cursors, that
     is CURSOR_WIDTH; for box cursors, it is the glyph width.  */
  get_phys_cursor_geometry (w, glyph_row, phys_cursor_glyph, &fx, &fy, &h);

  /* The above get_phys_cursor_geometry call set w->phys_cursor_width
     to the glyph width; replace with CURSOR_WIDTH for (V)BAR cursors.  */
  if (cursor_type == BAR_CURSOR)
    {
      if (cursor_width < 1)
	cursor_width = max (FRAME_CURSOR_WIDTH (f), 1);

      /* The bar cursor should never be wider than the glyph.  */
      if (cursor_width < w->phys_cursor_width)
        w->phys_cursor_width = cursor_width;
    }
  /* If we have an HBAR, "cursor_width" MAY specify height.  */
  else if (cursor_type == HBAR_CURSOR)
    {
      cursor_height = (cursor_width < 1) ? lrint (0.25 * h) : cursor_width;
      if (cursor_height > glyph_row->height)
        cursor_height = glyph_row->height;
      if (h > cursor_height) // Cursor smaller than line height, move down
        fy += h - cursor_height;
      h = cursor_height;
    }

  r.origin.x = fx, r.origin.y = fy;
  r.size.height = h;
  r.size.width = w->phys_cursor_width;

  /* Prevent the cursor from being drawn outside the text area.  */
  r = NSIntersectionRect (r, ns_row_rect (w, glyph_row, TEXT_AREA));

  ns_focus (f, &r, 1);

  face = FACE_FROM_ID_OR_NULL (f, phys_cursor_glyph->face_id);
  if (face && NS_FACE_BACKGROUND (face)
      == ns_index_color (FRAME_CURSOR_COLOR (f), f))
    {
      [ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), f) set];
      hollow_color = FRAME_CURSOR_COLOR (f);
    }
  else
    [FRAME_CURSOR_COLOR (f) set];

  switch (cursor_type)
    {
    case DEFAULT_CURSOR:
    case NO_CURSOR:
      break;
    case FILLED_BOX_CURSOR:
      NSRectFill (r);
      break;
    case HOLLOW_BOX_CURSOR:
      NSRectFill (r);
      [hollow_color set];
      NSRectFill (NSInsetRect (r, 1, 1));
      [FRAME_CURSOR_COLOR (f) set];
      break;
    case HBAR_CURSOR:
      NSRectFill (r);
      break;
    case BAR_CURSOR:
      s = r;
      /* If the character under cursor is R2L, draw the bar cursor
         on the right of its glyph, rather than on the left.  */
      cursor_glyph = get_phys_cursor_glyph (w);
      if ((cursor_glyph->resolved_level & 1) != 0)
        s.origin.x += cursor_glyph->pixel_width - s.size.width;

      NSRectFill (s);
      break;
    }
  ns_unfocus (f);

  /* Draw the character under the cursor.  Other terms only draw
     the character on top of box cursors, so do the same here.  */
  if (cursor_type == FILLED_BOX_CURSOR || cursor_type == HOLLOW_BOX_CURSOR)
    draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
}


static void
ns_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
/* --------------------------------------------------------------------------
     External (RIF): Draw a vertical line.
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;
  NSRect r = NSMakeRect (x, y0, 1, y1-y0);

  NSTRACE ("ns_draw_vertical_window_border");

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);

  ns_focus (f, &r, 1);
  if (face)
    [ns_lookup_indexed_color(face->foreground, f) set];

  NSRectFill(r);
  ns_unfocus (f);
}


static void
ns_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
/* --------------------------------------------------------------------------
     External (RIF): Draw a window divider.
   -------------------------------------------------------------------------- */
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
  NSRect divider = NSMakeRect (x0, y0, x1-x0, y1-y0);

  NSTRACE ("ns_draw_window_divider");

  ns_focus (f, &divider, 1);

  if ((y1 - y0 > x1 - x0) && (x1 - x0 >= 3))
    /* A vertical divider, at least three pixels wide: Draw first and
       last pixels differently.  */
    {
      [ns_lookup_indexed_color(color_first, f) set];
      NSRectFill(NSMakeRect (x0, y0, 1, y1 - y0));
      [ns_lookup_indexed_color(color, f) set];
      NSRectFill(NSMakeRect (x0 + 1, y0, x1 - x0 - 2, y1 - y0));
      [ns_lookup_indexed_color(color_last, f) set];
      NSRectFill(NSMakeRect (x1 - 1, y0, 1, y1 - y0));
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first and
       last pixels differently.  */
    {
      [ns_lookup_indexed_color(color_first, f) set];
      NSRectFill(NSMakeRect (x0, y0, x1 - x0, 1));
      [ns_lookup_indexed_color(color, f) set];
      NSRectFill(NSMakeRect (x0, y0 + 1, x1 - x0, y1 - y0 - 2));
      [ns_lookup_indexed_color(color_last, f) set];
      NSRectFill(NSMakeRect (x0, y1 - 1, x1 - x0, 1));
    }
  else
    {
      /* In any other case do not draw the first and last pixels
         differently.  */
      [ns_lookup_indexed_color(color, f) set];
      NSRectFill(divider);
    }

  ns_unfocus (f);
}


static void
ns_show_hourglass (struct frame *f)
{
  /* TODO: add NSProgressIndicator to all frames.  */
}

static void
ns_hide_hourglass (struct frame *f)
{
  /* TODO: remove NSProgressIndicator from all frames.  */
}

/* ==========================================================================

    Glyph drawing operations

   ========================================================================== */

static int
ns_get_glyph_string_clip_rect (struct glyph_string *s, NativeRectangle *nr)
/* --------------------------------------------------------------------------
    Wrapper utility to account for internal border width on full-width lines,
    and allow top full-width rows to hit the frame top.  nr should be pointer
    to two successive NSRects.  Number of rects actually used is returned.
   -------------------------------------------------------------------------- */
{
  int n = get_glyph_string_clip_rects (s, nr, 2);
  return n;
}

/* --------------------------------------------------------------------
   Draw a wavy line under glyph string s. The wave fills wave_height
   pixels from y.

                    x          wave_length = 2
                                 --
                y    *   *   *   *   *
                     |* * * * * * * * *
    wave_height = 3  | *   *   *   *
  --------------------------------------------------------------------- */

static void
ns_draw_underwave (struct glyph_string *s, EmacsCGFloat width, EmacsCGFloat x)
{
  int wave_height = 3, wave_length = 2;
  int y, dx, dy, odd, xmax;
  NSPoint a, b;
  NSRect waveClip;

  dx = wave_length;
  dy = wave_height - 1;
  y =  s->ybase - wave_height + 3;
  xmax = x + width;

  /* Find and set clipping rectangle */
  waveClip = NSMakeRect (x, y, width, wave_height);
  [[NSGraphicsContext currentContext] saveGraphicsState];
  NSRectClip (waveClip);

  /* Draw the waves */
  a.x = x - ((int)(x) % dx) + (EmacsCGFloat) 0.5;
  b.x = a.x + dx;
  odd = (int)(a.x/dx) % 2;
  a.y = b.y = y + 0.5;

  if (odd)
    a.y += dy;
  else
    b.y += dy;

  while (a.x <= xmax)
    {
      [NSBezierPath strokeLineFromPoint:a toPoint:b];
      a.x = b.x, a.y = b.y;
      b.x += dx, b.y = y + 0.5 + odd*dy;
      odd = !odd;
    }

  /* Restore previous clipping rectangle(s) */
  [[NSGraphicsContext currentContext] restoreGraphicsState];
}



static void
ns_draw_text_decoration (struct glyph_string *s, struct face *face,
                         NSColor *defaultCol, CGFloat width, CGFloat x)
/* --------------------------------------------------------------------------
   Draw underline, overline, and strike-through on glyph string s.
   -------------------------------------------------------------------------- */
{
  if (s->for_overlaps)
    return;

  /* Do underline.  */
  if (face->underline)
    {
      if (s->face->underline == FACE_UNDER_WAVE)
        {
          if (face->underline_defaulted_p)
            [defaultCol set];
          else
            [ns_lookup_indexed_color (face->underline_color, s->f) set];

          ns_draw_underwave (s, width, x);
        }
      else if (s->face->underline == FACE_UNDER_LINE)
        {

          NSRect r;
          unsigned long thickness, position;

          /* If the prev was underlined, match its appearance.  */
          if (s->prev
	      && s->prev->face->underline == FACE_UNDER_LINE
              && s->prev->underline_thickness > 0)
            {
              thickness = s->prev->underline_thickness;
              position = s->prev->underline_position;
            }
          else
            {
	      struct font *font = font_for_underline_metrics (s);
              unsigned long descent = s->y + s->height - s->ybase;
              unsigned long minimum_offset;
              BOOL underline_at_descent_line, use_underline_position_properties;
	      Lisp_Object val = (WINDOW_BUFFER_LOCAL_VALUE
				 (Qunderline_minimum_offset, s->w));

	      if (FIXNUMP (val))
		minimum_offset = XFIXNAT (val);
	      else
		minimum_offset = 1;

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_underline_at_descent_line, s->w));
	      underline_at_descent_line = !(NILP (val) || EQ (val, Qunbound));

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_use_underline_position_properties, s->w));
	      use_underline_position_properties
		= !(NILP (val) || EQ (val, Qunbound));

              /* Use underline thickness of font, defaulting to 1.  */
              thickness = (font && font->underline_thickness > 0)
                ? font->underline_thickness : 1;

              /* Determine the offset of underlining from the baseline.  */
              if (underline_at_descent_line)
                position = descent - thickness;
              else if (use_underline_position_properties
                       && font && font->underline_position >= 0)
                position = font->underline_position;
              else if (font)
                position = lround (font->descent / 2);
              else
                position = minimum_offset;

              position = max (position, minimum_offset);

              /* Ensure underlining is not cropped.  */
              if (descent <= position)
                {
                  position = descent - 1;
                  thickness = 1;
                }
              else if (descent < position + thickness)
                thickness = 1;
            }

          s->underline_thickness = thickness;
          s->underline_position = position;

          r = NSMakeRect (x, s->ybase + position, width, thickness);

          if (face->underline_defaulted_p)
            [defaultCol set];
          else
            [ns_lookup_indexed_color (face->underline_color, s->f) set];
          NSRectFill (r);
        }
    }
  /* Do overline. We follow other terms in using a thickness of 1
     and ignoring overline_margin.  */
  if (face->overline_p)
    {
      NSRect r;
      r = NSMakeRect (x, s->y, width, 1);

      if (face->overline_color_defaulted_p)
        [defaultCol set];
      else
        [ns_lookup_indexed_color (face->overline_color, s->f) set];
      NSRectFill (r);
    }

  /* Do strike-through.  We follow other terms for thickness and
     vertical position.  */
  if (face->strike_through_p)
    {
      NSRect r;
      /* Y-coordinate and height of the glyph string's first glyph.
	 We cannot use s->y and s->height because those could be
	 larger if there are taller display elements (e.g., characters
	 displayed with a larger font) in the same glyph row.  */
      int glyph_y = s->ybase - s->first_glyph->ascent;
      int glyph_height = s->first_glyph->ascent + s->first_glyph->descent;
      /* Strike-through width and offset from the glyph string's
	 top edge.  */
      unsigned long h = 1;
      unsigned long dy;

      dy = lrint ((glyph_height - h) / 2);
      r = NSMakeRect (x, glyph_y + dy, width, 1);

      if (face->strike_through_color_defaulted_p)
        [defaultCol set];
      else
        [ns_lookup_indexed_color (face->strike_through_color, s->f) set];
      NSRectFill (r);
    }
}

static void
ns_draw_box (NSRect r, CGFloat hthickness, CGFloat vthickness,
             NSColor *col, char left_p, char right_p)
/* --------------------------------------------------------------------------
    Draw an unfilled rect inside r, optionally leaving left and/or right open.
    Note we can't just use an NSDrawRect command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  NSRect s = r;
  [col set];

  /* top, bottom */
  s.size.height = hthickness;
  NSRectFill (s);
  s.origin.y += r.size.height - hthickness;
  NSRectFill (s);

  s.size.height = r.size.height;
  s.origin.y = r.origin.y;

  /* left, right (optional) */
  s.size.width = vthickness;
  if (left_p)
    NSRectFill (s);
  if (right_p)
    {
      s.origin.x += r.size.width - vthickness;
      NSRectFill (s);
    }
}


static void
ns_draw_relief (NSRect r, int hthickness, int vthickness, char raised_p,
               char top_p, char bottom_p, char left_p, char right_p,
               struct glyph_string *s)
/* --------------------------------------------------------------------------
    Draw a relief rect inside r, optionally leaving some sides open.
    Note we can't just use an NSDrawBezel command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  static NSColor *baseCol = nil, *lightCol = nil, *darkCol = nil;
  NSColor *newBaseCol = nil;
  NSRect sr = r;

  NSTRACE ("ns_draw_relief");

  /* set up colors */

  if (s->face->use_box_color_for_shadows_p)
    {
      newBaseCol = ns_lookup_indexed_color (s->face->box_color, s->f);
    }
/*     else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
   	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
       {
         newBaseCol = IMAGE_BACKGROUND  (s->img, s->f, 0);
       } */
  else
    {
      newBaseCol = ns_lookup_indexed_color (s->face->background, s->f);
    }

  if (newBaseCol == nil)
    newBaseCol = [NSColor grayColor];

  if (newBaseCol != baseCol)  /* TODO: better check */
    {
      [baseCol release];
      baseCol = [newBaseCol retain];
      [lightCol release];
      lightCol = [[baseCol highlightWithLevel: 0.2] retain];
      [darkCol release];
      darkCol = [[baseCol shadowWithLevel: 0.3] retain];
    }

  [(raised_p ? lightCol : darkCol) set];

  /* TODO: mitering. Using NSBezierPath doesn't work because of color switch.  */

  /* top */
  sr.size.height = hthickness;
  if (top_p) NSRectFill (sr);

  /* left */
  sr.size.height = r.size.height;
  sr.size.width = vthickness;
  if (left_p) NSRectFill (sr);

  [(raised_p ? darkCol : lightCol) set];

  /* bottom */
  sr.size.width = r.size.width;
  sr.size.height = hthickness;
  sr.origin.y += r.size.height - hthickness;
  if (bottom_p) NSRectFill (sr);

  /* right */
  sr.size.height = r.size.height;
  sr.origin.y = r.origin.y;
  sr.size.width = vthickness;
  sr.origin.x += r.size.width - vthickness;
  if (right_p) NSRectFill (sr);
}


static void
ns_dumpglyphs_box_or_relief (struct glyph_string *s)
/* --------------------------------------------------------------------------
      Function modeled after x_draw_glyph_string_box ().
      Sets up parameters for drawing.
   -------------------------------------------------------------------------- */
{
  int right_x, last_x;
  char left_p, right_p;
  struct glyph *last_glyph;
  NSRect r;
  int hthickness, vthickness;
  struct face *face;

  if (s->hl == DRAW_MOUSE_FACE)
    {
      face = FACE_FROM_ID_OR_NULL (s->f,
				   MOUSE_HL_INFO (s->f)->mouse_face_face_id);
      if (!face)
        face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
    }
  else
    face = s->face;

  vthickness = face->box_vertical_line_width;
  hthickness = face->box_horizontal_line_width;

  NSTRACE ("ns_dumpglyphs_box_or_relief");

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));
  if (s->cmp || s->img)
    last_glyph = s->first_glyph;
  else if (s->first_glyph->type == COMPOSITE_GLYPH
	   && s->first_glyph->u.cmp.automatic)
    {
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

  right_x = ((s->row->full_width_p && s->extends_to_end_of_line_p
	      ? last_x - 1 : min (last_x, s->x + s->background_width) - 1));

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL || s->next->hl != s->hl)));

  r = NSMakeRect (s->x, s->y, right_x - s->x + 1, s->height);

  /* TODO: Sometimes box_color is 0 and this seems wrong; should investigate.  */
  if (s->face->box == FACE_SIMPLE_BOX && s->face->box_color)
    {
      ns_draw_box (r, abs (hthickness), abs (vthickness),
                   ns_lookup_indexed_color (face->box_color, s->f),
                   left_p, right_p);
    }
  else
    {
      ns_draw_relief (r, abs (hthickness), abs (vthickness),
                      s->face->box == FACE_RAISED_BOX,
                      1, 1, left_p, right_p, s);
    }
}


static void
ns_maybe_dumpglyphs_background (struct glyph_string *s, char force_p)
/* --------------------------------------------------------------------------
      Modeled after x_draw_glyph_string_background, which draws BG in
      certain cases.  Others are left to the text rendering routine.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_maybe_dumpglyphs_background");

  if (!s->background_filled_p/* || s->hl == DRAW_MOUSE_FACE*/)
    {
      int box_line_width = max (s->face->box_horizontal_line_width, 0);
      if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	  /* When xdisp.c ignores FONT_HEIGHT, we cannot trust font
	     dimensions, since the actual glyphs might be much
	     smaller.  So in that case we always clear the rectangle
	     with background color.  */
	  || FONT_TOO_HIGH (s->font)
          || s->font_not_found_p || s->extends_to_end_of_line_p || force_p)
	{
          struct face *face;
          if (s->hl == DRAW_MOUSE_FACE)
            {
              face
		= FACE_FROM_ID_OR_NULL (s->f,
					MOUSE_HL_INFO (s->f)->mouse_face_face_id);
              if (!face)
                face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
            }
          else
            face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
          if (!face->stipple)
            [(NS_FACE_BACKGROUND (face) != 0
              ? ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f)
              : FRAME_BACKGROUND_COLOR (s->f)) set];
          else
            {
              struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (s->f);
              [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
            }

          if (s->hl != DRAW_CURSOR)
            {
              NSRect r = NSMakeRect (s->x, s->y + box_line_width,
                                    s->background_width,
                                    s->height-2*box_line_width);
              NSRectFill (r);
            }

	  s->background_filled_p = 1;
	}
    }
}


static void
ns_dumpglyphs_image (struct glyph_string *s, NSRect r)
/* --------------------------------------------------------------------------
      Renders an image and associated borders.
   -------------------------------------------------------------------------- */
{
  EmacsImage *img = s->img->pixmap;
  int box_line_vwidth = max (s->face->box_horizontal_line_width, 0);
  int x = s->x, y = s->ybase - image_ascent (s->img, s->face, &s->slice);
  int bg_x, bg_y, bg_height;
  int th;
  char raised_p;
  NSRect br;
  struct face *face;
  NSColor *tdCol;

  NSTRACE ("ns_dumpglyphs_image");

  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  bg_x = x;
  bg_y =  s->slice.y == 0 ? s->y : s->y + box_line_vwidth;
  bg_height = s->height;
  /* other terms have this, but was causing problems w/tabbar mode */
  /* - 2 * box_line_vwidth; */

  if (s->slice.x == 0) x += s->img->hmargin;
  if (s->slice.y == 0) y += s->img->vmargin;

  /* Draw BG: if we need larger area than image itself cleared, do that,
     otherwise, since we composite the image under NS (instead of mucking
     with its background color), we must clear just the image area.  */
  if (s->hl == DRAW_MOUSE_FACE)
    {
      face = FACE_FROM_ID_OR_NULL (s->f,
				   MOUSE_HL_INFO (s->f)->mouse_face_face_id);
      if (!face)
       face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
    }
  else
    face = FACE_FROM_ID (s->f, s->first_glyph->face_id);

  [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f) set];

  if (bg_height > s->slice.height || s->img->hmargin || s->img->vmargin
      || s->img->mask || s->img->pixmap == 0 || s->width != s->background_width)
    {
      br = NSMakeRect (bg_x, bg_y, s->background_width, bg_height);
      s->background_filled_p = 1;
    }
  else
    {
      br = NSMakeRect (x, y, s->slice.width, s->slice.height);
    }

  NSRectFill (br);

  /* Draw the image... do we need to draw placeholder if img == nil?  */
  if (img != nil)
    {
      /* The idea here is that the clipped area is set in the normal
         view coordinate system, then we transform the coordinate
         system so that when we draw the image it is rotated, resized
         or whatever as required.  This is kind of backwards, but
         there's no way to apply the transform to the image without
         creating a whole new bitmap.  */
      NSRect dr = NSMakeRect (x, y, s->slice.width, s->slice.height);
      NSRect ir = NSMakeRect (0, 0, [img size].width, [img size].height);

      NSAffineTransform *setOrigin = [NSAffineTransform transform];

      [[NSGraphicsContext currentContext] saveGraphicsState];

      /* Because of the transforms it's difficult to work out what
         portion of the original, untransformed, image will be drawn,
         so the clipping area will ensure we draw only the correct
         bit.  */
      NSRectClip (dr);

      [setOrigin translateXBy:x - s->slice.x yBy:y - s->slice.y];
      [setOrigin concat];

      NSAffineTransform *doTransform = [NSAffineTransform transform];

      /* ImageMagick images don't have transforms.  */
      if (img->transform)
        [doTransform appendTransform:img->transform];

      [doTransform concat];

      /* Smoothing is the default, so if we don't want smoothing we
         have to turn it off.  */
      if (! img->smoothing)
        [[NSGraphicsContext currentContext]
          setImageInterpolation:NSImageInterpolationNone];

      [img drawInRect:ir fromRect:ir
            operation:NSCompositingOperationSourceOver
             fraction:1.0 respectFlipped:YES hints:nil];

      /* Apparently image interpolation is not reset with
         restoreGraphicsState, so we have to manually reset it.  */
      if (! img->smoothing)
        [[NSGraphicsContext currentContext]
          setImageInterpolation:NSImageInterpolationDefault];

      [[NSGraphicsContext currentContext] restoreGraphicsState];
    }

  if (s->hl == DRAW_CURSOR)
    {
    [FRAME_CURSOR_COLOR (s->f) set];
    if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
      tdCol = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f);
    else
      /* Currently on NS img->mask is always 0.  Since
         get_window_cursor_type specifies a hollow box cursor when on
         a non-masked image we never reach this clause.  But we put it
         in, in anticipation of better support for image masks on
         NS.  */
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }
  else
    {
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }

  /* Draw underline, overline, strike-through.  */
  ns_draw_text_decoration (s, face, tdCol, br.size.width, br.origin.x);

  /* Draw relief, if requested */
  if (s->img->relief || s->hl ==DRAW_IMAGE_RAISED || s->hl ==DRAW_IMAGE_SUNKEN)
    {
      if (s->hl == DRAW_IMAGE_SUNKEN || s->hl == DRAW_IMAGE_RAISED)
        {
          th = (tool_bar_button_relief < 0
		? DEFAULT_TOOL_BAR_BUTTON_RELIEF
		: min (tool_bar_button_relief, 1000000));
          raised_p = (s->hl == DRAW_IMAGE_RAISED);
        }
      else
        {
          th = abs (s->img->relief);
          raised_p = (s->img->relief > 0);
        }

      r.origin.x = x - th;
      r.origin.y = y - th;
      r.size.width = s->slice.width + 2*th-1;
      r.size.height = s->slice.height + 2*th-1;
      ns_draw_relief (r, th, th, raised_p,
                      s->slice.y == 0,
                      s->slice.y + s->slice.height == s->img->height,
                      s->slice.x == 0,
                      s->slice.x + s->slice.width == s->img->width, s);
    }

  /* If there is no mask, the background won't be seen,
     so draw a rectangle on the image for the cursor.
     Do this for all images, getting transparency right is not reliable.  */
  if (s->hl == DRAW_CURSOR)
    {
      int thickness = abs (s->img->relief);
      if (thickness == 0) thickness = 1;
      ns_draw_box (br, thickness, thickness, FRAME_CURSOR_COLOR (s->f), 1, 1);
    }
}


static void
ns_dumpglyphs_stretch (struct glyph_string *s)
{
  NSRect r[2];
  NSRect glyphRect;
  int n;
  struct face *face;
  NSColor *fgCol, *bgCol;

  if (!s->background_filled_p)
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);

      if (s->hl == DRAW_MOUSE_FACE)
        {
          face = FACE_FROM_ID_OR_NULL (s->f,
                                       MOUSE_HL_INFO (s->f)->mouse_face_face_id);
          if (!face)
            face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
        }
      else
        face = FACE_FROM_ID (s->f, s->first_glyph->face_id);

      bgCol = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f);
      fgCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);

      glyphRect = NSMakeRect (s->x, s->y, s->background_width, s->height);

      [bgCol set];

      /* NOTE: under NS this is NOT used to draw cursors, but we must avoid
         overwriting cursor (usually when cursor on a tab) */
      if (s->hl == DRAW_CURSOR)
        {
          CGFloat x, width;

          /* FIXME: This looks like it will only work for left to
             right languages.  */
          x = NSMinX (glyphRect);
          width = s->w->phys_cursor_width;
          glyphRect.size.width -= width;
          glyphRect.origin.x += width;

          NSRectFill (glyphRect);

          /* Draw overlining, etc. on the cursor. */
          if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
            ns_draw_text_decoration (s, face, bgCol, width, x);
          else
            ns_draw_text_decoration (s, face, fgCol, width, x);
        }
      else
        {
          NSRectFill (glyphRect);
        }

      /* Draw overlining, etc. on the stretch glyph (or the part
         of the stretch glyph after the cursor). */
      ns_draw_text_decoration (s, face, fgCol, NSWidth (glyphRect),
                               NSMinX (glyphRect));

      ns_unfocus (s->f);
      s->background_filled_p = 1;
    }
}


static void
ns_draw_glyph_string_foreground (struct glyph_string *s)
{
  int x, flags;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  flags = s->hl == DRAW_CURSOR ? NS_DUMPGLYPH_CURSOR :
    (s->hl == DRAW_MOUSE_FACE ? NS_DUMPGLYPH_MOUSEFACE :
     (s->for_overlaps ? NS_DUMPGLYPH_FOREGROUND :
      NS_DUMPGLYPH_NORMAL));

  font->driver->draw
    (s, s->cmp_from, s->nchars, x, s->ybase,
     (flags == NS_DUMPGLYPH_NORMAL && !s->background_filled_p)
     || flags == NS_DUMPGLYPH_MOUSEFACE);
}


static void
ns_draw_composite_glyph_string_foreground (struct glyph_string *s)
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
        {
          NSRect r = NSMakeRect (s->x, s->y, s->width-1, s->height -1);
          ns_draw_box (r, 1, 1, FRAME_CURSOR_COLOR (s->f), 1, 1);
        }
    }
  else if (! s->first_glyph->u.cmp.automatic)
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
ns_draw_glyph_string (struct glyph_string *s)
/* --------------------------------------------------------------------------
      External (RIF): Main draw-text call.
   -------------------------------------------------------------------------- */
{
  /* TODO (optimize): focus for box and contents draw */
  NSRect r[2];
  int n;
  char box_drawn_p = 0;
  struct font *font = s->face->font;
  if (! font) font = FRAME_FONT (s->f);

  NSTRACE_WHEN (NSTRACE_GROUP_GLYPHS, "ns_draw_glyph_string");

  if (s->next && s->right_overhang && !s->for_overlaps/*&&s->hl!=DRAW_CURSOR*/)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
          {
            if (next->first_glyph->type != STRETCH_GLYPH)
              {
                n = ns_get_glyph_string_clip_rect (s->next, r);
                ns_focus (s->f, r, n);
                ns_maybe_dumpglyphs_background (s->next, 1);
                ns_unfocus (s->f);
              }
            else
              {
                ns_dumpglyphs_stretch (s->next);
              }
            next->num_clips = 0;
          }
    }

  if (!s->for_overlaps && s->face->box != FACE_NO_BOX
        && (s->first_glyph->type == CHAR_GLYPH
	    || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_maybe_dumpglyphs_background (s, 1);
      ns_dumpglyphs_box_or_relief (s);
      ns_unfocus (s->f);
      box_drawn_p = 1;
    }

  switch (s->first_glyph->type)
    {

    case IMAGE_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_dumpglyphs_image (s, r[0]);
      ns_unfocus (s->f);
      break;

    case XWIDGET_GLYPH:
      x_draw_xwidget_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      ns_dumpglyphs_stretch (s);
      break;

    case CHAR_GLYPH:
    case COMPOSITE_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);

      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
        s->background_filled_p = 1;
      else
        ns_maybe_dumpglyphs_background
          (s, s->first_glyph->type == COMPOSITE_GLYPH);

      if (s->hl == DRAW_CURSOR && s->w->phys_cursor_type == FILLED_BOX_CURSOR)
        {
          unsigned long tmp = NS_FACE_BACKGROUND (s->face);
          NS_FACE_BACKGROUND (s->face) = NS_FACE_FOREGROUND (s->face);
          NS_FACE_FOREGROUND (s->face) = tmp;
        }

      {
        BOOL isComposite = s->first_glyph->type == COMPOSITE_GLYPH;

        if (isComposite)
          ns_draw_composite_glyph_string_foreground (s);
        else
          ns_draw_glyph_string_foreground (s);
      }

      {
        NSColor *col = (NS_FACE_FOREGROUND (s->face) != 0
                        ? ns_lookup_indexed_color (NS_FACE_FOREGROUND (s->face),
                                                   s->f)
                        : FRAME_FOREGROUND_COLOR (s->f));
        [col set];

        /* Draw underline, overline, strike-through. */
        ns_draw_text_decoration (s, s->face, col, s->width, s->x);
      }

      if (s->hl == DRAW_CURSOR && s->w->phys_cursor_type == FILLED_BOX_CURSOR)
        {
          unsigned long tmp = NS_FACE_BACKGROUND (s->face);
          NS_FACE_BACKGROUND (s->face) = NS_FACE_FOREGROUND (s->face);
          NS_FACE_FOREGROUND (s->face) = tmp;
        }

      ns_unfocus (s->f);
      break;

    case GLYPHLESS_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);

      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
        s->background_filled_p = 1;
      else
        ns_maybe_dumpglyphs_background
          (s, s->first_glyph->type == COMPOSITE_GLYPH);
      /* ... */
      /* Not yet implemented.  */
      /* ... */
      ns_unfocus (s->f);
      break;

    default:
      emacs_abort ();
    }

  /* Draw box if not done already.  */
  if (!s->for_overlaps && !box_drawn_p && s->face->box != FACE_NO_BOX)
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_dumpglyphs_box_or_relief (s);
      ns_unfocus (s->f);
    }

  s->num_clips = 0;
}



/* ==========================================================================

    Event loop

   ========================================================================== */


static void
ns_send_appdefined (int value)
/* --------------------------------------------------------------------------
    Internal: post an appdefined event which EmacsApp-sendEvent will
              recognize and take as a command to halt the event loop.
   -------------------------------------------------------------------------- */
{
  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "ns_send_appdefined(%d)", value);

  // GNUstep needs postEvent to happen on the main thread.
  // Cocoa needs nextEventMatchingMask to happen on the main thread too.
  if (! [[NSThread currentThread] isMainThread])
    {
      EmacsApp *app = (EmacsApp *)NSApp;
      app->nextappdefined = value;
      [app performSelectorOnMainThread:@selector (sendFromMainThread:)
                            withObject:nil
                         waitUntilDone:NO];
      return;
    }

  /* Only post this event if we haven't already posted one.  This will end
     the [NXApp run] main loop after having processed all events queued at
     this moment.  */

#ifdef NS_IMPL_COCOA
  if (! send_appdefined)
    {
      /* OS X 10.10.1 swallows the AppDefined event we are sending ourselves
         in certain situations (rapid incoming events).
         So check if we have one, if not add one.  */
      NSEvent *appev = [NSApp nextEventMatchingMask:NSEventMaskApplicationDefined
                                          untilDate:[NSDate distantPast]
                                             inMode:NSDefaultRunLoopMode
                                            dequeue:NO];
      if (! appev) send_appdefined = YES;
    }
#endif

  if (send_appdefined)
    {
      NSEvent *nxev;

      /* We only need one NX_APPDEFINED event to stop NXApp from running.  */
      send_appdefined = NO;

      /* Don't need wakeup timer any more.  */
      if (timed_entry)
        {
          [timed_entry invalidate];
          [timed_entry release];
          timed_entry = nil;
        }

      nxev = [NSEvent otherEventWithType: NSEventTypeApplicationDefined
                                location: NSMakePoint (0, 0)
                           modifierFlags: 0
                               timestamp: 0
                            windowNumber: [[NSApp mainWindow] windowNumber]
                                 context: [NSApp context]
                                 subtype: 0
                                   data1: value
                                   data2: 0];

      /* Post an application defined event on the event queue.  When this is
         received the [NXApp run] will return, thus having processed all
         events which are currently queued.  */
      [NSApp postEvent: nxev atStart: NO];
    }
}

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
static void
check_native_fs ()
{
  Lisp_Object frame, tail;

  if (ns_last_use_native_fullscreen == ns_use_native_fullscreen)
    return;

  ns_last_use_native_fullscreen = ns_use_native_fullscreen;

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_NS_P (f))
        {
          EmacsView *view = FRAME_NS_VIEW (f);
          [view updateCollectionBehavior];
        }
    }
}
#endif


static int
ns_read_socket (struct terminal *terminal, struct input_event *hold_quit)
/* --------------------------------------------------------------------------
     External (hook): Post an event to ourself and keep reading events until
     we read it back again.  In effect process all events which were waiting.
     From 21+ we have to manage the event buffer ourselves.
   -------------------------------------------------------------------------- */
{
  struct input_event ev;
  int nevents;

  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "ns_read_socket");

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  check_native_fs ();
#endif

  if ([NSApp modalWindow] != nil)
    return -1;

  if (hold_event_q.nr > 0)
    {
      int i;
      for (i = 0; i < hold_event_q.nr; ++i)
        kbd_buffer_store_event_hold (&hold_event_q.q[i], hold_quit);
      hold_event_q.nr = 0;
      return i;
    }

  if ([NSThread isMainThread])
    {
      block_input ();
      n_emacs_events_pending = 0;
      ns_init_events (&ev);
      q_event_ptr = hold_quit;

      /* We manage autorelease pools by allocate/reallocate each time around
         the loop; strict nesting is occasionally violated but seems not to
         matter... earlier methods using full nesting caused major memory leaks.  */
      [outerpool release];
      outerpool = [[NSAutoreleasePool alloc] init];

      /* If have pending open-file requests, attend to the next one of those.  */
      if (ns_pending_files && [ns_pending_files count] != 0
          && [(EmacsApp *)NSApp openFile: [ns_pending_files objectAtIndex: 0]])
        {
          [ns_pending_files removeObjectAtIndex: 0];
        }
      /* Deal with pending service requests.  */
      else if (ns_pending_service_names && [ns_pending_service_names count] != 0
               && [(EmacsApp *)
                    NSApp fulfillService: [ns_pending_service_names objectAtIndex: 0]
                                 withArg: [ns_pending_service_args objectAtIndex: 0]])
        {
          [ns_pending_service_names removeObjectAtIndex: 0];
          [ns_pending_service_args removeObjectAtIndex: 0];
        }
      else
        {
          /* Run and wait for events.  We must always send one NX_APPDEFINED event
             to ourself, otherwise [NXApp run] will never exit.  */
          send_appdefined = YES;
          ns_send_appdefined (-1);

          [NSApp run];
        }

      nevents = n_emacs_events_pending;
      n_emacs_events_pending = 0;
      ns_finish_events ();
      q_event_ptr = NULL;
      unblock_input ();
    }
  else
    return -1;

  return nevents;
}


int
ns_select (int nfds, fd_set *readfds, fd_set *writefds,
	   fd_set *exceptfds, struct timespec *timeout,
	   sigset_t *sigmask)
/* --------------------------------------------------------------------------
     Replacement for select, checking for events
   -------------------------------------------------------------------------- */
{
  int result;
  int t, k, nr = 0;
  struct input_event event;
  char c;

  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "ns_select");

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  check_native_fs ();
#endif

  if (hold_event_q.nr > 0)
    {
      /* We already have events pending.  */
      raise (SIGIO);
      errno = EINTR;
      return -1;
    }

  eassert (nfds <= FD_SETSIZE);
  for (k = 0; k < nfds; k++)
    {
      if (readfds && FD_ISSET(k, readfds)) ++nr;
      if (writefds && FD_ISSET(k, writefds)) ++nr;
    }

  if (NSApp == nil
      || ![NSThread isMainThread]
      || (timeout && timeout->tv_sec == 0 && timeout->tv_nsec == 0))
    return thread_select(pselect, nfds, readfds, writefds,
                         exceptfds, timeout, sigmask);
  else
    {
      struct timespec t = {0, 0};
      thread_select(pselect, 0, NULL, NULL, NULL, &t, sigmask);
    }

  /* FIXME: This draining of outerpool causes a crash when a buffer
     running over tramp is displayed and the user tries to use the
     menus.  I believe some other autorelease pool's lifetime
     straddles this call causing a violation of autorelease pool
     nesting.  There's no good reason to keep these here since the
     pool will be drained some other time anyway, but removing them
     leaves the menus sometimes not opening until the user moves their
     mouse pointer, but that's better than a crash.

     There must be something about running external processes like
     tramp that interferes with the modal menu code.

     See bugs 24472, 37557, 37922.  */

  // [outerpool release];
  // outerpool = [[NSAutoreleasePool alloc] init];


  send_appdefined = YES;
  if (nr > 0)
    {
      pthread_mutex_lock (&select_mutex);
      select_nfds = nfds;
      select_valid = 0;
      if (readfds)
        {
          select_readfds = *readfds;
          select_valid += SELECT_HAVE_READ;
        }
      if (writefds)
        {
          select_writefds = *writefds;
          select_valid += SELECT_HAVE_WRITE;
        }

      if (timeout)
        {
          select_timeout = *timeout;
          select_valid += SELECT_HAVE_TMO;
        }

      pthread_mutex_unlock (&select_mutex);

      /* Inform fd_handler that select should be called.  */
      c = 'g';
      emacs_write_sig (selfds[1], &c, 1);
    }
  else if (nr == 0 && timeout)
    {
      /* No file descriptor, just a timeout, no need to wake fd_handler.  */
      double time = timespectod (*timeout);
      timed_entry = [[NSTimer scheduledTimerWithTimeInterval: time
                                                      target: NSApp
                                                    selector:
                                  @selector (timeout_handler:)
                                                    userInfo: 0
                                                     repeats: NO]
                      retain];
    }
  else /* No timeout and no file descriptors, can this happen?  */
    {
      /* Send appdefined so we exit from the loop.  */
      ns_send_appdefined (-1);
    }

  block_input ();
  ns_init_events (&event);

  [NSApp run];

  ns_finish_events ();
  if (nr > 0 && readfds)
    {
      c = 's';
      emacs_write_sig (selfds[1], &c, 1);
    }
  unblock_input ();

  t = last_appdefined_event_data;

  if (t != NO_APPDEFINED_DATA)
    {
      last_appdefined_event_data = NO_APPDEFINED_DATA;

      if (t == -2)
        {
          /* The NX_APPDEFINED event we received was a timeout.  */
          result = 0;
        }
      else if (t == -1)
        {
          /* The NX_APPDEFINED event we received was the result of
             at least one real input event arriving.  */
          errno = EINTR;
          result = -1;
        }
      else
        {
          /* Received back from select () in fd_handler; copy the results.  */
          pthread_mutex_lock (&select_mutex);
          if (readfds) *readfds = select_readfds;
          if (writefds) *writefds = select_writefds;
          pthread_mutex_unlock (&select_mutex);
          result = t;
        }
    }
  else
    {
      errno = EINTR;
      result = -1;
    }

  return result;
}

#ifdef HAVE_PTHREAD
void
ns_run_loop_break ()
/* Break out of the NS run loop in ns_select or ns_read_socket.  */
{
  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "ns_run_loop_break");

  /* If we don't have a GUI, don't send the event.  */
  if (NSApp != NULL)
    ns_send_appdefined(-1);
}
#endif


/* ==========================================================================

    Scrollbar handling

   ========================================================================== */


static void
ns_set_vertical_scroll_bar (struct window *window,
                           int portion, int whole, int position)
/* --------------------------------------------------------------------------
      External (hook): Update or add scrollbar
   -------------------------------------------------------------------------- */
{
  Lisp_Object win;
  NSRect r, v;
  struct frame *f = XFRAME (WINDOW_FRAME (window));
  EmacsView *view = FRAME_NS_VIEW (f);
  EmacsScroller *bar;
  int window_y, window_height;
  int top, left, height, width;
  BOOL update_p = YES;

  /* Optimization; display engine sends WAY too many of these.  */
  if (!NILP (window->vertical_scroll_bar))
    {
      bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
      if ([bar checkSamePosition: position portion: portion whole: whole])
        {
          if (view->scrollbarsNeedingUpdate == 0)
            {
              if (!windows_or_buffers_changed)
                  return;
            }
          else
            view->scrollbarsNeedingUpdate--;
          update_p = NO;
        }
    }

  NSTRACE ("ns_set_vertical_scroll_bar");

  /* Get dimensions.  */
  window_box (window, ANY_AREA, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;
  width = NS_SCROLL_BAR_WIDTH (f);
  left = WINDOW_SCROLL_BAR_AREA_X (window);

  r = NSMakeRect (left, top, width, height);
  /* The parent view is flipped, so we need to flip y value.  */
  v = [view frame];
  r.origin.y = (v.size.height - r.size.height - r.origin.y);

  XSETWINDOW (win, window);
  block_input ();

  /* We want at least 5 lines to display a scrollbar.  */
  if (WINDOW_TOTAL_LINES (window) < 5)
    {
      if (!NILP (window->vertical_scroll_bar))
        {
          bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
          [bar removeFromSuperview];
          wset_vertical_scroll_bar (window, Qnil);
          [bar release];
          ns_clear_frame_area (f, left, top, width, height);
        }
      unblock_input ();
      return;
    }

  if (NILP (window->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	ns_clear_frame_area (f, left, top, width, height);

      bar = [[EmacsScroller alloc] initFrame: r window: win];
      wset_vertical_scroll_bar (window, make_mint_ptr (bar));
      update_p = YES;
    }
  else
    {
      NSRect oldRect;
      bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
      oldRect = [bar frame];
      r.size.width = oldRect.size.width;
      if (FRAME_LIVE_P (f) && !NSEqualRects (oldRect, r))
        {
          if (! NSEqualRects (oldRect, r))
              ns_clear_frame_area (f, left, top, width, height);
          [bar setFrame: r];
        }
    }

  if (update_p)
    [bar setPosition: position portion: portion whole: whole];
  unblock_input ();
}


static void
ns_set_horizontal_scroll_bar (struct window *window,
			      int portion, int whole, int position)
/* --------------------------------------------------------------------------
      External (hook): Update or add scrollbar.
   -------------------------------------------------------------------------- */
{
  Lisp_Object win;
  NSRect r, v;
  struct frame *f = XFRAME (WINDOW_FRAME (window));
  EmacsView *view = FRAME_NS_VIEW (f);
  EmacsScroller *bar;
  int top, height, left, width;
  int window_x, window_width;
  BOOL update_p = YES;

  /* Optimization; display engine sends WAY too many of these.  */
  if (!NILP (window->horizontal_scroll_bar))
    {
      bar = XNS_SCROLL_BAR (window->horizontal_scroll_bar);
      if ([bar checkSamePosition: position portion: portion whole: whole])
        {
          if (view->scrollbarsNeedingUpdate == 0)
            {
              if (!windows_or_buffers_changed)
                  return;
            }
          else
            view->scrollbarsNeedingUpdate--;
          update_p = NO;
        }
    }

  NSTRACE ("ns_set_horizontal_scroll_bar");

  /* Get dimensions.  */
  window_box (window, ANY_AREA, &window_x, 0, &window_width, 0);
  left = window_x;
  width = window_width;
  height = NS_SCROLL_BAR_HEIGHT (f);
  top = WINDOW_SCROLL_BAR_AREA_Y (window);

  r = NSMakeRect (left, top, width, height);
  /* The parent view is flipped, so we need to flip y value.  */
  v = [view frame];
  r.origin.y = (v.size.height - r.size.height - r.origin.y);

  XSETWINDOW (win, window);
  block_input ();

  if (NILP (window->horizontal_scroll_bar))
    {
      if (width > 0 && height > 0)
	ns_clear_frame_area (f, left, top, width, height);

      bar = [[EmacsScroller alloc] initFrame: r window: win];
      wset_horizontal_scroll_bar (window, make_mint_ptr (bar));
      update_p = YES;
    }
  else
    {
      NSRect oldRect;
      bar = XNS_SCROLL_BAR (window->horizontal_scroll_bar);
      oldRect = [bar frame];
      if (FRAME_LIVE_P (f) && !NSEqualRects (oldRect, r))
        {
          ns_clear_frame_area (f, left, top, width, height);
          [bar setFrame: r];
          update_p = YES;
        }
    }

  /* If there are both horizontal and vertical scroll-bars they leave
     a square that belongs to neither. We need to clear it otherwise
     it fills with junk.  */
  if (!NILP (window->vertical_scroll_bar))
    ns_clear_frame_area (f, WINDOW_SCROLL_BAR_AREA_X (window), top,
                         NS_SCROLL_BAR_HEIGHT (f), height);

  if (update_p)
    [bar setPosition: position portion: portion whole: whole];
  unblock_input ();
}


static void
ns_condemn_scroll_bars (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook): arrange for all frame's scrollbars to be removed
     at next call to judge_scroll_bars, except for those redeemed.
   -------------------------------------------------------------------------- */
{
  int i;
  id view;
  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];

  NSTRACE ("ns_condemn_scroll_bars");

  for (i =[subviews count]-1; i >= 0; i--)
    {
      view = [subviews objectAtIndex: i];
      if ([view isKindOfClass: [EmacsScroller class]])
        [view condemn];
    }
}


static void
ns_redeem_scroll_bar (struct window *window)
/* --------------------------------------------------------------------------
     External (hook): arrange to spare this window's scrollbar
     at next call to judge_scroll_bars.
   -------------------------------------------------------------------------- */
{
  id bar;
  NSTRACE ("ns_redeem_scroll_bar");
  if (!NILP (window->vertical_scroll_bar)
      && WINDOW_HAS_VERTICAL_SCROLL_BAR (window))
    {
      bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
      [bar reprieve];
    }

  if (!NILP (window->horizontal_scroll_bar)
      && WINDOW_HAS_HORIZONTAL_SCROLL_BAR (window))
    {
      bar = XNS_SCROLL_BAR (window->horizontal_scroll_bar);
      [bar reprieve];
    }
}


static void
ns_judge_scroll_bars (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook): destroy all scrollbars on frame that weren't
     redeemed after call to condemn_scroll_bars.
   -------------------------------------------------------------------------- */
{
  int i;
  id view;
  EmacsView *eview = FRAME_NS_VIEW (f);
  NSArray *subviews = [[eview superview] subviews];

  NSTRACE ("ns_judge_scroll_bars");
  for (i = [subviews count]-1; i >= 0; --i)
    {
      view = [subviews objectAtIndex: i];
      if (![view isKindOfClass: [EmacsScroller class]]) continue;
      [view judge];
    }
}

/* ==========================================================================

    Image Hooks

   ========================================================================== */

static void
ns_free_pixmap (struct frame *_f, Emacs_Pixmap pixmap)
{
  ns_release_object (pixmap);
}

/* ==========================================================================

    Initialization

   ========================================================================== */

int
ns_display_pixel_height (struct ns_display_info *dpyinfo)
{
  NSArray *screens = [NSScreen screens];
  NSEnumerator *enumerator = [screens objectEnumerator];
  NSScreen *screen;
  NSRect frame;

  frame = NSZeroRect;
  while ((screen = [enumerator nextObject]) != nil)
    frame = NSUnionRect (frame, [screen frame]);

  return NSHeight (frame);
}

int
ns_display_pixel_width (struct ns_display_info *dpyinfo)
{
  NSArray *screens = [NSScreen screens];
  NSEnumerator *enumerator = [screens objectEnumerator];
  NSScreen *screen;
  NSRect frame;

  frame = NSZeroRect;
  while ((screen = [enumerator nextObject]) != nil)
    frame = NSUnionRect (frame, [screen frame]);

  return NSWidth (frame);
}


static Lisp_Object ns_string_to_lispmod (const char *s)
/* --------------------------------------------------------------------------
     Convert modifier name to lisp symbol.
   -------------------------------------------------------------------------- */
{
  if (!strncmp (SSDATA (SYMBOL_NAME (Qmeta)), s, 10))
    return Qmeta;
  else if (!strncmp (SSDATA (SYMBOL_NAME (Qsuper)), s, 10))
    return Qsuper;
  else if (!strncmp (SSDATA (SYMBOL_NAME (Qcontrol)), s, 10))
    return Qcontrol;
  else if (!strncmp (SSDATA (SYMBOL_NAME (Qalt)), s, 10))
    return Qalt;
  else if (!strncmp (SSDATA (SYMBOL_NAME (Qhyper)), s, 10))
    return Qhyper;
  else if (!strncmp (SSDATA (SYMBOL_NAME (Qnone)), s, 10))
    return Qnone;
  else
    return Qnil;
}


static void
ns_default (const char *parameter, Lisp_Object *result,
           Lisp_Object yesval, Lisp_Object noval,
           BOOL is_float, BOOL is_modstring)
/* --------------------------------------------------------------------------
      Check a parameter value in user's preferences.
   -------------------------------------------------------------------------- */
{
  const char *value = ns_get_defaults_value (parameter);

  if (value)
    {
      double f;
      char *pos;
      if (c_strcasecmp (value, "YES") == 0)
        *result = yesval;
      else if (c_strcasecmp (value, "NO") == 0)
        *result = noval;
      else if (is_float && (f = strtod (value, &pos), pos != value))
        *result = make_float (f);
      else if (is_modstring && value)
        *result = ns_string_to_lispmod (value);
      else fprintf (stderr,
                   "Bad value for default \"%s\": \"%s\"\n", parameter, value);
    }
}


static void
ns_initialize_display_info (struct ns_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Initialize global info and storage for display.
   -------------------------------------------------------------------------- */
{
    NSScreen *screen = [NSScreen mainScreen];
    NSWindowDepth depth = [screen depth];

    dpyinfo->resx = 72.27; /* used 75.0, but this makes pt == pixel, expected */
    dpyinfo->resy = 72.27;
    dpyinfo->color_p = ![NSDeviceWhiteColorSpace isEqualToString:
                                                  NSColorSpaceFromDepth (depth)]
                && ![NSCalibratedWhiteColorSpace isEqualToString:
                                                 NSColorSpaceFromDepth (depth)];
    dpyinfo->n_planes = NSBitsPerPixelFromDepth (depth);
    dpyinfo->color_table = xmalloc (sizeof *dpyinfo->color_table);
    dpyinfo->color_table->colors = NULL;
    dpyinfo->root_window = 42; /* A placeholder.  */
    dpyinfo->highlight_frame = dpyinfo->ns_focus_frame = NULL;
    dpyinfo->n_fonts = 0;
    dpyinfo->smallest_font_height = 1;
    dpyinfo->smallest_char_width = 1;

    reset_mouse_highlight (&dpyinfo->mouse_highlight);
}

/* This currently does nothing, since it's only really needed when
   changing the font-backend, but macOS currently only has one
   possible backend.  This may change if we add HarfBuzz support.  */
static void
ns_default_font_parameter (struct frame *f, Lisp_Object parms)
{
}

/* This and next define (many of the) public functions in this file.  */
/* gui_* are generic versions in xdisp.c that we, and other terms, get away
         with using despite presence in the "system dependent" redisplay
         interface.  In addition, many of the ns_ methods have code that is
         shared with all terms, indicating need for further refactoring.  */
extern frame_parm_handler ns_frame_parm_handlers[];
static struct redisplay_interface ns_redisplay_interface =
{
  ns_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  ns_scroll_run,
  ns_after_update_window_line,
  NULL, /* update_window_begin */
  NULL, /* update_window_end   */
  0, /* flush_display */
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  ns_draw_fringe_bitmap,
  0, /* define_fringe_bitmap */ /* FIXME: simplify ns_draw_fringe_bitmap */
  0, /* destroy_fringe_bitmap */
  ns_compute_glyph_string_overhangs,
  ns_draw_glyph_string,
  ns_define_frame_cursor,
  ns_clear_frame_area,
  ns_clear_under_internal_border, /* clear_under_internal_border */
  ns_draw_window_cursor,
  ns_draw_vertical_window_border,
  ns_draw_window_divider,
  ns_shift_glyphs_for_insert,
  ns_show_hourglass,
  ns_hide_hourglass,
  ns_default_font_parameter
};


static void
ns_delete_display (struct ns_display_info *dpyinfo)
{
  /* TODO...  */
}


/* This function is called when the last frame on a display is deleted.  */
static void
ns_delete_terminal (struct terminal *terminal)
{
  struct ns_display_info *dpyinfo = terminal->display_info.ns;

  NSTRACE ("ns_delete_terminal");

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  block_input ();

  image_destroy_all_bitmaps (dpyinfo);
  ns_delete_display (dpyinfo);
  unblock_input ();
}

static Lisp_Object ns_new_font (struct frame *f, Lisp_Object font_object,
                                int fontset);

static struct terminal *
ns_create_terminal (struct ns_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Set up use of NS before we make the first connection.
   -------------------------------------------------------------------------- */
{
  struct terminal *terminal;

  NSTRACE ("ns_create_terminal");

  terminal = create_terminal (output_ns, &ns_redisplay_interface);

  terminal->display_info.ns = dpyinfo;
  dpyinfo->terminal = terminal;

  terminal->clear_frame_hook = ns_clear_frame;
  terminal->ring_bell_hook = ns_ring_bell;
  terminal->update_begin_hook = ns_update_begin;
  terminal->update_end_hook = ns_update_end;
  terminal->read_socket_hook = ns_read_socket;
  terminal->frame_up_to_date_hook = ns_frame_up_to_date;
  terminal->defined_color_hook = ns_defined_color;
  terminal->query_frame_background_color = ns_query_frame_background_color;
  terminal->mouse_position_hook = ns_mouse_position;
  terminal->get_focus_frame = ns_get_focus_frame;
  terminal->focus_frame_hook = ns_focus_frame;
  terminal->frame_rehighlight_hook = ns_frame_rehighlight;
  terminal->frame_raise_lower_hook = ns_frame_raise_lower;
  terminal->frame_visible_invisible_hook = ns_make_frame_visible_invisible;
  terminal->fullscreen_hook = ns_fullscreen_hook;
  terminal->iconify_frame_hook = ns_iconify_frame;
  terminal->set_window_size_hook = ns_set_window_size;
  terminal->set_frame_offset_hook = ns_set_offset;
  terminal->set_frame_alpha_hook = ns_set_frame_alpha;
  terminal->set_new_font_hook = ns_new_font;
  terminal->implicit_set_name_hook = ns_implicitly_set_name;
  terminal->menu_show_hook = ns_menu_show;
  terminal->popup_dialog_hook = ns_popup_dialog;
  terminal->set_vertical_scroll_bar_hook = ns_set_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = ns_set_horizontal_scroll_bar;
  terminal->set_scroll_bar_default_width_hook = ns_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook = ns_set_scroll_bar_default_height;
  terminal->condemn_scroll_bars_hook = ns_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = ns_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = ns_judge_scroll_bars;
  terminal->get_string_resource_hook = ns_get_string_resource;
  terminal->free_pixmap = ns_free_pixmap;
  terminal->delete_frame_hook = ns_destroy_window;
  terminal->delete_terminal_hook = ns_delete_terminal;
  /* Other hooks are NULL by default.  */

  return terminal;
}


struct ns_display_info *
ns_term_init (Lisp_Object display_name)
/* --------------------------------------------------------------------------
     Start the Application and get things rolling.
   -------------------------------------------------------------------------- */
{
  struct terminal *terminal;
  struct ns_display_info *dpyinfo;
  static int ns_initialized = 0;
  Lisp_Object tmp;

  if (ns_initialized) return x_display_list;
  ns_initialized = 1;

  block_input ();

  NSTRACE ("ns_term_init");

  [outerpool release];
  outerpool = [[NSAutoreleasePool alloc] init];

  /* count object allocs (About, click icon); on macOS use ObjectAlloc tool */
  /*GSDebugAllocationActive (YES); */
  block_input ();

  baud_rate = 38400;
  Fset_input_interrupt_mode (Qnil);

  if (selfds[0] == -1)
    {
      if (emacs_pipe (selfds) != 0)
        {
          fprintf (stderr, "Failed to create pipe: %s\n",
                   emacs_strerror (errno));
          emacs_abort ();
        }

      fcntl (selfds[0], F_SETFL, O_NONBLOCK|fcntl (selfds[0], F_GETFL));
      FD_ZERO (&select_readfds);
      FD_ZERO (&select_writefds);
      pthread_mutex_init (&select_mutex, NULL);
    }

  ns_pending_files = [[NSMutableArray alloc] init];
  ns_pending_service_names = [[NSMutableArray alloc] init];
  ns_pending_service_args = [[NSMutableArray alloc] init];

  /* Start app and create the main menu, window, view.
     Needs to be here because ns_initialize_display_info () uses AppKit classes.
     The view will then ask the NSApp to stop and return to Emacs.  */
  [EmacsApp sharedApplication];
  if (NSApp == nil)
    return NULL;
  [NSApp setDelegate: NSApp];

  /* Start the select thread.  */
  [NSThread detachNewThreadSelector:@selector (fd_handler:)
                           toTarget:NSApp
                         withObject:nil];

  /* debugging: log all notifications */
  /*   [[NSNotificationCenter defaultCenter] addObserver: NSApp
                                         selector: @selector (logNotification:)
                                             name: nil object: nil]; */

  dpyinfo = xzalloc (sizeof *dpyinfo);

  ns_initialize_display_info (dpyinfo);
  terminal = ns_create_terminal (dpyinfo);

  terminal->kboard = allocate_kboard (Qns);
  /* Don't let the initial kboard remain current longer than necessary.
     That would cause problems if a file loaded on startup tries to
     prompt in the mini-buffer.  */
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;
  terminal->kboard->reference_count++;

  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  dpyinfo->name_list_element = Fcons (display_name, Qnil);

  terminal->name = xlispstrdup (display_name);

  unblock_input ();

  if (!inhibit_x_resources)
    {
      ns_default ("GSFontAntiAlias", &ns_antialias_text,
                 Qt, Qnil, NO, NO);
      tmp = Qnil;
      /* this is a standard variable */
      ns_default ("AppleAntiAliasingThreshold", &tmp,
                 make_float (10.0), make_float (6.0), YES, NO);
      ns_antialias_threshold = NILP (tmp) ? 10.0 : extract_float (tmp);
    }

  NSTRACE_MSG ("Colors");

  {
    NSColorList *cl = [NSColorList colorListNamed: @"Emacs"];

    /* There are 752 colors defined in rgb.txt.  */
    if ( cl == nil || [[cl allKeys] count] < 752)
      {
        Lisp_Object color_file, color_map, color, name;
        unsigned long c;

        color_file = Fexpand_file_name (build_string ("rgb.txt"),
                         Fsymbol_value (intern ("data-directory")));

        color_map = Fx_load_color_file (color_file);
        if (NILP (color_map))
          fatal ("Could not read %s.\n", SDATA (color_file));

        cl = [[NSColorList alloc] initWithName: @"Emacs"];
        for ( ; CONSP (color_map); color_map = XCDR (color_map))
          {
            color = XCAR (color_map);
            name = XCAR (color);
            c = XFIXNUM (XCDR (color));
            [cl setColor:
                  [NSColor colorForEmacsRed: RED_FROM_ULONG (c) / 255.0
                                      green: GREEN_FROM_ULONG (c) / 255.0
                                       blue: BLUE_FROM_ULONG (c) / 255.0
                                      alpha: 1.0]
                  forKey: [NSString stringWithLispString: name]];
          }

        /* FIXME: Report any errors writing the color file below.  */
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101100
        if ([cl respondsToSelector:@selector(writeToURL:error:)])
#endif
          [cl writeToURL:nil error:nil];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101100
        else
#endif
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 101100 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101100 \
  || defined (NS_IMPL_GNUSTEP)
          [cl writeToFile: nil];
#endif
      }
  }

  NSTRACE_MSG ("Versions");

  {
#ifdef NS_IMPL_GNUSTEP
    Vwindow_system_version = build_string (gnustep_base_version);
#else
    /* PSnextrelease (128, c); */
    char c[DBL_BUFSIZE_BOUND];
    int len = dtoastr (c, sizeof c, 0, 0, NSAppKitVersionNumber);
    Vwindow_system_version = make_unibyte_string (c, len);
#endif
  }

  delete_keyboard_wait_descriptor (0);

  ns_app_name = [[NSProcessInfo processInfo] processName];

  /* Set up macOS app menu */

  NSTRACE_MSG ("Menu init");

#ifdef NS_IMPL_COCOA
  {
    NSMenu *appMenu;
    NSMenuItem *item;
    /* set up the application menu */
    svcsMenu = [[EmacsMenu alloc] initWithTitle: @"Services"];
    [svcsMenu setAutoenablesItems: NO];
    appMenu = [[EmacsMenu alloc] initWithTitle: @"Emacs"];
    [appMenu setAutoenablesItems: NO];
    mainMenu = [[EmacsMenu alloc] initWithTitle: @""];
    dockMenu = [[EmacsMenu alloc] initWithTitle: @""];

    [appMenu insertItemWithTitle: @"About Emacs"
                          action: @selector (orderFrontStandardAboutPanel:)
                   keyEquivalent: @""
                         atIndex: 0];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 1];
    [appMenu insertItemWithTitle: @"Preferences..."
                          action: @selector (showPreferencesWindow:)
                   keyEquivalent: @","
                         atIndex: 2];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 3];
    item = [appMenu insertItemWithTitle: @"Services"
                                 action: @selector (menuDown:)
                          keyEquivalent: @""
                                atIndex: 4];
    [appMenu setSubmenu: svcsMenu forItem: item];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 5];
    [appMenu insertItemWithTitle: @"Hide Emacs"
                          action: @selector (hide:)
                   keyEquivalent: @"h"
                         atIndex: 6];
    item =  [appMenu insertItemWithTitle: @"Hide Others"
                          action: @selector (hideOtherApplications:)
                   keyEquivalent: @"h"
                         atIndex: 7];
    [item setKeyEquivalentModifierMask: NSEventModifierFlagCommand | NSEventModifierFlagOption];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 8];
    [appMenu insertItemWithTitle: @"Quit Emacs"
                          action: @selector (terminate:)
                   keyEquivalent: @"q"
                         atIndex: 9];

    item = [mainMenu insertItemWithTitle: ns_app_name
                                  action: @selector (menuDown:)
                           keyEquivalent: @""
                                 atIndex: 0];
    [mainMenu setSubmenu: appMenu forItem: item];
    [dockMenu insertItemWithTitle: @"New Frame"
			   action: @selector (newFrame:)
		    keyEquivalent: @""
			  atIndex: 0];

    [NSApp setMainMenu: mainMenu];
    [NSApp setAppleMenu: appMenu];
    [NSApp setServicesMenu: svcsMenu];
    /* Needed at least on Cocoa, to get dock menu to show windows */
    [NSApp setWindowsMenu: [[NSMenu alloc] init]];
  }
#endif /* macOS menu setup */

  /* Register our external input/output types, used for determining
     applicable services and also drag/drop eligibility.  */

  NSTRACE_MSG ("Input/output types");

  ns_send_types = [[NSArray arrayWithObjects: NSPasteboardTypeString, nil] retain];
  ns_return_types = [[NSArray arrayWithObjects: NSPasteboardTypeString, nil]
                      retain];
  ns_drag_types = [[NSArray arrayWithObjects:
                            NSPasteboardTypeString,
                            NSPasteboardTypeTabularText,
#if NS_USE_NSPasteboardTypeFileURL != 0
                            NSPasteboardTypeFileURL,
#else
                            NSFilenamesPboardType,
#endif
                            NSPasteboardTypeURL, nil] retain];

  /* If fullscreen is in init/default-frame-alist, focus isn't set
     right for fullscreen windows, so set this.  */
  [NSApp activateIgnoringOtherApps:YES];

  NSTRACE_MSG ("Call NSApp run");

  [NSApp run];
  ns_do_open_file = YES;

#ifdef NS_IMPL_GNUSTEP
  /* GNUstep steals SIGCHLD for use in NSTask, but we don't use NSTask.
     We must re-catch it so subprocess works.  */
  catch_child_signal ();
#endif

  NSTRACE_MSG ("ns_term_init done");

  unblock_input ();

  return dpyinfo;
}


void
ns_term_shutdown (int sig)
{
  [[NSUserDefaults standardUserDefaults] synchronize];

  /* code not reached in emacs.c after this is called by shut_down_emacs: */
  if (STRINGP (Vauto_save_list_file_name))
    unlink (SSDATA (Vauto_save_list_file_name));

  if (sig == 0 || sig == SIGTERM)
    {
      [NSApp terminate: NSApp];
    }
  else // force a stack trace to happen
    {
      emacs_abort ();
    }
}


/* ==========================================================================

    EmacsApp implementation

   ========================================================================== */


@implementation EmacsApp

- (id)init
{
  NSTRACE ("[EmacsApp init]");

  if ((self = [super init]))
    {
#ifdef NS_IMPL_COCOA
      self->isFirst = YES;
#endif
#ifdef NS_IMPL_GNUSTEP
      self->applicationDidFinishLaunchingCalled = NO;
#endif
    }

  return self;
}

#ifdef NS_IMPL_COCOA
- (void)run
{
  NSTRACE ("[EmacsApp run]");

#ifndef NSAppKitVersionNumber10_9
#define NSAppKitVersionNumber10_9 1265
#endif

    if ((int)NSAppKitVersionNumber != NSAppKitVersionNumber10_9)
      {
        [super run];
        return;
      }

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  if (isFirst) [self finishLaunching];
  isFirst = NO;

  shouldKeepRunning = YES;
  do
    {
      [pool release];
      pool = [[NSAutoreleasePool alloc] init];

      NSEvent *event =
        [self nextEventMatchingMask:NSEventMaskAny
                          untilDate:[NSDate distantFuture]
                             inMode:NSDefaultRunLoopMode
                            dequeue:YES];

      [self sendEvent:event];
      [self updateWindows];
    } while (shouldKeepRunning);

  [pool release];
}

- (void)stop: (id)sender
{
  NSTRACE ("[EmacsApp stop:]");

    shouldKeepRunning = NO;
    // Stop possible dialog also.  Noop if no dialog present.
    // The file dialog still leaks 7k - 10k on 10.9 though.
    [super stop:sender];
}
#endif /* NS_IMPL_COCOA */

- (void)logNotification: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp logNotification:]");

  const char *name = [[notification name] UTF8String];
  if (!strstr (name, "Update") && !strstr (name, "NSMenu")
      && !strstr (name, "WindowNumber"))
    NSLog (@"notification: '%@'", [notification name]);
}


- (void)sendEvent: (NSEvent *)theEvent
/* --------------------------------------------------------------------------
     Called when NSApp is running for each event received.  Used to stop
     the loop when we choose, since there's no way to just run one iteration.
   -------------------------------------------------------------------------- */
{
  int type = [theEvent type];
  NSWindow *window = [theEvent window];

  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "[EmacsApp sendEvent:]");
  NSTRACE_MSG ("Type: %d", type);

#ifdef NS_IMPL_GNUSTEP
  // Keyboard events aren't propagated to file dialogs for some reason.
  if ([NSApp modalWindow] != nil &&
      (type == NSEventTypeKeyDown || type == NSEventTypeKeyUp || type == NSEventTypeFlagsChanged))
    {
      [[NSApp modalWindow] sendEvent: theEvent];
      return;
    }
#endif

  if (type == NSEventTypeApplicationDefined)
    {
      switch ([theEvent data2])
        {
#ifdef NS_IMPL_COCOA
        case NSAPP_DATA2_RUNASSCRIPT:
          ns_run_ascript ();
          [self stop: self];
          return;
#endif
        case NSAPP_DATA2_RUNFILEDIALOG:
          ns_run_file_dialog ();
          [self stop: self];
          return;
        }
    }

  if (type == NSEventTypeCursorUpdate && window == nil)
    {
      fputs ("Dropping external cursor update event.\n", stderr);
      return;
    }

  if (type == NSEventTypeApplicationDefined)
    {
      /* Events posted by ns_send_appdefined interrupt the run loop here.
         But, if a modal window is up, an appdefined can still come through,
         (e.g., from a makeKeyWindow event) but stopping self also stops the
         modal loop. Just defer it until later.  */
      if ([NSApp modalWindow] == nil)
        {
          last_appdefined_event_data = [theEvent data1];
          [self stop: self];
        }
      else
        {
          send_appdefined = YES;
        }
    }


#ifdef NS_IMPL_COCOA
  /* If no dialog and none of our frames have focus and it is a move, skip it.
     It is a mouse move in an auxiliary menu, i.e. on the top right on macOS,
     such as Wifi, sound, date or similar.
     This prevents "spooky" highlighting in the frame under the menu.  */
  if (type == NSEventTypeMouseMoved && [NSApp modalWindow] == nil)
    {
      struct ns_display_info *di;
      BOOL has_focus = NO;
      for (di = x_display_list; ! has_focus && di; di = di->next)
        has_focus = di->ns_focus_frame != 0;
      if (! has_focus)
        return;
    }
#endif

  NSTRACE_UNSILENCE();

  [super sendEvent: theEvent];
}


- (void)showPreferencesWindow: (id)sender
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return;
  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_SHOW_PREFS;
  emacs_event->modifiers = 0;
  EV_TRAILER (theEvent);
}


- (void)newFrame: (id)sender
{
  NSTRACE ("[EmacsApp newFrame:]");

  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return;
  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_NEW_FRAME;
  emacs_event->modifiers = 0;
  EV_TRAILER (theEvent);
}


/* Open a file (used by below, after going into queue read by ns_read_socket).  */
- (BOOL) openFile: (NSString *)fileName
{
  NSTRACE ("[EmacsApp openFile:]");

  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_OPEN_FILE_LINE;
  ns_input_file = append2 (ns_input_file, [fileName lispString]);
  ns_input_line = Qnil; /* can be start or cons start,end */
  emacs_event->modifiers =0;
  EV_TRAILER (theEvent);

  return YES;
}


/* **************************************************************************

      EmacsApp delegate implementation

   ************************************************************************** */

- (void)applicationDidFinishLaunching: (NSNotification *)notification
/* --------------------------------------------------------------------------
     When application is loaded, terminate event loop in ns_term_init.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("[EmacsApp applicationDidFinishLaunching:]");

#ifdef NS_IMPL_GNUSTEP
  ((EmacsApp *)self)->applicationDidFinishLaunchingCalled = YES;
#endif
  [NSApp setServicesProvider: NSApp];

  [self antialiasThresholdDidChange:nil];
#ifdef NS_IMPL_COCOA
  [[NSNotificationCenter defaultCenter]
    addObserver:self
       selector:@selector(antialiasThresholdDidChange:)
	   name:NSAntialiasThresholdChangedNotification
	 object:nil];
#endif

#ifdef NS_IMPL_COCOA
  /* Some functions/methods in CoreFoundation/Foundation increase the
     maximum number of open files for the process in their first call.
     We make dummy calls to them and then reduce the resource limit
     here, since pselect cannot handle file descriptors that are
     greater than or equal to FD_SETSIZE.  */
  CFSocketGetTypeID ();
  CFFileDescriptorGetTypeID ();
  [[NSFileHandle alloc] init];
  struct rlimit rlim;
  if (getrlimit (RLIMIT_NOFILE, &rlim) == 0
      && rlim.rlim_cur > FD_SETSIZE)
    {
      rlim.rlim_cur = FD_SETSIZE;
      setrlimit (RLIMIT_NOFILE, &rlim);
    }
  if ([NSApp activationPolicy] == NSApplicationActivationPolicyProhibited) {
    /* Set the app's activation policy to regular when we run outside
       of a bundle.  This is already done for us by Info.plist when we
       run inside a bundle.  */
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    [NSApp setApplicationIconImage:
	     [EmacsImage
	       allocInitFromFile:
		 build_string("icons/hicolor/128x128/apps/emacs.png")]];
  }
#endif

  ns_send_appdefined (-2);
}

- (void)antialiasThresholdDidChange:(NSNotification *)notification
{
#ifdef NS_IMPL_COCOA
  macfont_update_antialias_threshold ();
#endif
}


/* Termination sequences:
    C-x C-c:
    Cmd-Q:
    MenuBar | File | Exit:
    Select Quit from App menubar:
        -terminate
	KEY_NS_POWER_OFF, (save-buffers-kill-emacs)
	ns_term_shutdown()

    Select Quit from Dock menu:
    Logout attempt:
        -appShouldTerminate
          Cancel -> Nothing else
          Accept ->

	  -terminate
	  KEY_NS_POWER_OFF, (save-buffers-kill-emacs)
	  ns_term_shutdown()

*/

- (void) terminate: (id)sender
{
  NSTRACE ("[EmacsApp terminate:]");

  struct frame *emacsframe = SELECTED_FRAME ();

  if (!emacs_event)
    return;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_POWER_OFF;
  emacs_event->arg = Qt; /* mark as non-key event */
  EV_TRAILER ((id)nil);
}

static bool
runAlertPanel(NSString *title,
              NSString *msgFormat,
              NSString *defaultButton,
              NSString *alternateButton)
{
#ifdef NS_IMPL_GNUSTEP
  return NSRunAlertPanel(title, msgFormat, defaultButton, alternateButton, nil)
    == NSAlertDefaultReturn;
#else
  NSAlert *alert = [[NSAlert alloc] init];
  [alert setAlertStyle: NSAlertStyleCritical];
  [alert setMessageText: msgFormat];
  [alert addButtonWithTitle: defaultButton];
  [alert addButtonWithTitle: alternateButton];
  NSInteger ret = [alert runModal];
  [alert release];
  return ret == NSAlertFirstButtonReturn;
#endif
}


- (NSApplicationTerminateReply)applicationShouldTerminate: (id)sender
{
  NSTRACE ("[EmacsApp applicationShouldTerminate:]");

  bool ret;

  if (NILP (ns_confirm_quit)) //   || ns_shutdown_properly  --> TO DO
    return NSTerminateNow;

  ret = runAlertPanel(ns_app_name,
		      @"Exit requested.  Would you like to Save Buffers and Exit, or Cancel the request?",
		      @"Save Buffers and Exit", @"Cancel");

  return ret ? NSTerminateNow : NSTerminateCancel;
}

static int
not_in_argv (NSString *arg)
{
  int k;
  const char *a = [arg UTF8String];
  for (k = 1; k < initial_argc; ++k)
    if (strcmp (a, initial_argv[k]) == 0) return 0;
  return 1;
}

/* Notification from the Workspace to open a file.  */
- (BOOL)application: sender openFile: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}


/* Open a file as a temporary file.  */
- (BOOL)application: sender openTempFile: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}


/* Notification from the Workspace to open a file noninteractively (?).  */
- (BOOL)application: sender openFileWithoutUI: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}

/* Notification from the Workspace to open multiple files.  */
- (void)application: sender openFiles: (NSArray *)fileList
{
  NSEnumerator *files = [fileList objectEnumerator];
  NSString *file;
  /* Don't open files from the command line unconditionally,
     Cocoa parses the command line wrong, --option value tries to open value
     if --option is the last option.  */
  while ((file = [files nextObject]) != nil)
    if (ns_do_open_file || not_in_argv (file))
      [ns_pending_files addObject: file];

  [self replyToOpenOrPrint: NSApplicationDelegateReplySuccess];

}


/* Handle dock menu requests.  */
- (NSMenu *)applicationDockMenu: (NSApplication *) sender
{
  return dockMenu;
}


/* TODO: these may help w/IO switching between terminal and NSApp.  */
- (void)applicationWillBecomeActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationWillBecomeActive:]");
  // ns_app_active=YES;
}

- (void)applicationDidBecomeActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationDidBecomeActive:]");

#ifdef NS_IMPL_GNUSTEP
  if (! applicationDidFinishLaunchingCalled)
    [self applicationDidFinishLaunching:notification];
#endif
  // ns_app_active=YES;

  ns_update_auto_hide_menu_bar ();
  // No constraining takes place when the application is not active.
  ns_constrain_all_frames ();
}
- (void)applicationDidResignActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationDidResignActive:]");

  // ns_app_active=NO;
  ns_send_appdefined (-1);
}



/* ==========================================================================

    EmacsApp aux handlers for managing event loop

   ========================================================================== */


- (void)timeout_handler: (NSTimer *)timedEntry
/* --------------------------------------------------------------------------
     The timeout specified to ns_select has passed.
   -------------------------------------------------------------------------- */
{
  /* NSTRACE ("timeout_handler"); */
  ns_send_appdefined (-2);
}

- (void)sendFromMainThread:(id)unused
{
  ns_send_appdefined (nextappdefined);
}

- (void)fd_handler:(id)unused
/* --------------------------------------------------------------------------
     Check data waiting on file descriptors and terminate if so.
   -------------------------------------------------------------------------- */
{
  int result;
  int waiting = 1, nfds;
  char c;

  fd_set readfds, writefds, *wfds;
  struct timespec timeout, *tmo;
  NSAutoreleasePool *pool = nil;

  /* NSTRACE ("fd_handler"); */

  for (;;)
    {
      [pool release];
      pool = [[NSAutoreleasePool alloc] init];

      if (waiting)
        {
          fd_set fds;
          FD_ZERO (&fds);
          FD_SET (selfds[0], &fds);
          result = select (selfds[0]+1, &fds, NULL, NULL, NULL);
          if (result > 0 && read (selfds[0], &c, 1) == 1 && c == 'g')
	    waiting = 0;
        }
      else
        {
          pthread_mutex_lock (&select_mutex);
          nfds = select_nfds;

          if (select_valid & SELECT_HAVE_READ)
            readfds = select_readfds;
          else
            FD_ZERO (&readfds);

          if (select_valid & SELECT_HAVE_WRITE)
            {
              writefds = select_writefds;
              wfds = &writefds;
            }
          else
            wfds = NULL;
          if (select_valid & SELECT_HAVE_TMO)
            {
              timeout = select_timeout;
              tmo = &timeout;
            }
          else
            tmo = NULL;

          pthread_mutex_unlock (&select_mutex);

          FD_SET (selfds[0], &readfds);
          if (selfds[0] >= nfds) nfds = selfds[0]+1;

          result = pselect (nfds, &readfds, wfds, NULL, tmo, NULL);

          if (result == 0)
            ns_send_appdefined (-2);
          else if (result > 0)
            {
              if (FD_ISSET (selfds[0], &readfds))
                {
                  if (read (selfds[0], &c, 1) == 1 && c == 's')
		    waiting = 1;
                }
              else
                {
                  pthread_mutex_lock (&select_mutex);
                  if (select_valid & SELECT_HAVE_READ)
                    select_readfds = readfds;
                  if (select_valid & SELECT_HAVE_WRITE)
                    select_writefds = writefds;
                  if (select_valid & SELECT_HAVE_TMO)
                    select_timeout = timeout;
                  pthread_mutex_unlock (&select_mutex);

                  ns_send_appdefined (result);
                }
            }
          waiting = 1;
        }
    }
}



/* ==========================================================================

    Service provision

   ========================================================================== */

/* Called from system: queue for next pass through event loop.  */
- (void)requestService: (NSPasteboard *)pboard
              userData: (NSString *)userData
                 error: (NSString **)error
{
  [ns_pending_service_names addObject: userData];
  [ns_pending_service_args addObject: [NSString stringWithLispString:ns_string_from_pasteboard (pboard)]];
}


/* Called from ns_read_socket to clear queue.  */
- (BOOL)fulfillService: (NSString *)name withArg: (NSString *)arg
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  NSTRACE ("[EmacsApp fulfillService:withArg:]");

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_SPI_SERVICE_CALL;
  ns_input_spi_name = [name lispString];
  ns_input_spi_arg = [arg lispString];
  emacs_event->modifiers = EV_MODIFIERS (theEvent);
  EV_TRAILER (theEvent);

  return YES;
}


@end  /* EmacsApp */


/* ==========================================================================

    EmacsView implementation

   ========================================================================== */


@implementation EmacsView

/* Needed to inform when window closed from lisp.  */
- (void) setWindowClosing: (BOOL)closing
{
  NSTRACE ("[EmacsView setWindowClosing:%d]", closing);

  windowClosing = closing;
}


- (void)dealloc
{
  NSTRACE ("[EmacsView dealloc]");

  /* Clear the view resize notification.  */
  [[NSNotificationCenter defaultCenter]
    removeObserver:self
              name:NSViewFrameDidChangeNotification
            object:nil];

#ifdef NS_DRAW_TO_BUFFER
  [surface release];
#endif

  [toolbar release];
  if (fs_state == FULLSCREEN_BOTH)
    [nonfs_window release];
  [super dealloc];
}


/* Called on font panel selection.  */
- (void)changeFont: (id)sender
{
  NSEvent *e = [[self window] currentEvent];
  struct face *face = FACE_FROM_ID (emacsframe, DEFAULT_FACE_ID);
  struct font *font = face->font;
  id newFont;
  CGFloat size;
  NSFont *nsfont;

  NSTRACE ("[EmacsView changeFont:]");

  if (!emacs_event)
    return;

#ifdef NS_IMPL_GNUSTEP
  nsfont = ((struct nsfont_info *)font)->nsfont;
#endif
#ifdef NS_IMPL_COCOA
  nsfont = (NSFont *) macfont_get_nsctfont (font);
#endif

  if ((newFont = [sender convertFont: nsfont]))
    {
      SET_FRAME_GARBAGED (emacsframe); /* now needed as of 2008/10 */

      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->modifiers = 0;
      emacs_event->code = KEY_NS_CHANGE_FONT;

      size = [newFont pointSize];
      ns_input_fontsize = make_fixnum (lrint (size));
      ns_input_font = [[newFont familyName] lispString];
      EV_TRAILER (e);
    }
}


- (BOOL)acceptsFirstResponder
{
  NSTRACE ("[EmacsView acceptsFirstResponder]");
  return YES;
}


- (void)resetCursorRects
{
  NSRect visible = [self visibleRect];
  NSCursor *currentCursor = FRAME_POINTER_TYPE (emacsframe);
  NSTRACE ("[EmacsView resetCursorRects]");

  if (currentCursor == nil)
    currentCursor = [NSCursor arrowCursor];

  if (!NSIsEmptyRect (visible))
    [self addCursorRect: visible cursor: currentCursor];

#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101300
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
  if ([currentCursor respondsToSelector: @selector(setOnMouseEntered)])
#endif
    [currentCursor setOnMouseEntered: YES];
#endif
}



/*****************************************************************************/
/* Keyboard handling.  */
#define NS_KEYLOG 0

- (void)keyDown: (NSEvent *)theEvent
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  int code;
  unsigned fnKeysym = 0;
  static NSMutableArray *nsEvArray;
  unsigned int flags = [theEvent modifierFlags];

  NSTRACE ("[EmacsView keyDown:]");

  /* Rhapsody and macOS give up and down events for the arrow keys.  */
  if (ns_fake_keydown == YES)
    ns_fake_keydown = NO;
  else if ([theEvent type] != NSEventTypeKeyDown)
    return;

  if (!emacs_event)
    return;

 if (![[self window] isKeyWindow]
     && [[theEvent window] isKindOfClass: [EmacsWindow class]]
     /* We must avoid an infinite loop here.  */
     && (EmacsView *)[[theEvent window] delegate] != self)
   {
     /* XXX: There is an occasional condition in which, when Emacs display
         updates a different frame from the current one, and temporarily
         selects it, then processes some interrupt-driven input
         (dispnew.c:3878), OS will send the event to the correct NSWindow, but
         for some reason that window has its first responder set to the NSView
         most recently updated (I guess), which is not the correct one.  */
     [(EmacsView *)[[theEvent window] delegate] keyDown: theEvent];
     return;
   }

  if (nsEvArray == nil)
    nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];

  [NSCursor setHiddenUntilMouseMoves:! NILP (Vmake_pointer_invisible)];

  if (hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight))
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_hidden = 1;
    }

  if (!processingCompose)
    {
      /* FIXME: What should happen for key sequences with more than
         one character?  */
      code = ([[theEvent charactersIgnoringModifiers] length] == 0) ?
        0 : [[theEvent charactersIgnoringModifiers] characterAtIndex: 0];

      /* Is it a "function key"?  */
      /* Note: Sometimes a plain key will have the NSEventModifierFlagNumericPad
         flag set (this is probably a bug in the OS).  */
      if (code < 0x00ff && (flags&NSEventModifierFlagNumericPad))
        {
          fnKeysym = ns_convert_key ([theEvent keyCode] | NSEventModifierFlagNumericPad);
        }
      if (fnKeysym == 0)
        {
          fnKeysym = ns_convert_key (code);
        }

      if (fnKeysym)
        {
          /* COUNTERHACK: map 'Delete' on upper-right main KB to 'Backspace',
             because Emacs treats Delete and KP-Delete same (in simple.el).  */
          if ((fnKeysym == 0xFFFF && [theEvent keyCode] == 0x33)
#ifdef NS_IMPL_GNUSTEP
              /*  GNUstep uses incompatible keycodes, even for those that are
                  supposed to be hardware independent.  Just check for delete.
                  Keypad delete does not have keysym 0xFFFF.
                  See https://savannah.gnu.org/bugs/?25395  */
              || (fnKeysym == 0xFFFF && code == 127)
#endif
            )
            code = 0xFF08; /* backspace */
          else
            code = fnKeysym;

          /* Function keys (such as the F-keys, arrow keys, etc.) set
             modifiers as though the fn key has been pressed when it
             hasn't.  Also some combinations of fn and a function key
             return a different key than was pressed (e.g. fn-<left>
             gives <home>).  We need to unset the fn key flag in these
             cases.  */
          flags &= ~NS_FUNCTION_KEY_MASK;
        }

      /* The ⌘ and ⌥ modifiers can be either shift-like (for alternate
         character input) or control-like (as command prefix).  If we
         have only shift-like modifiers, then we should use the
         translated characters (returned by the characters method); if
         we have only control-like modifiers, then we should use the
         untranslated characters (returned by the
         charactersIgnoringModifiers method).  An annoyance happens if
         we have both shift-like and control-like modifiers because
         the NSEvent API doesn’t let us ignore only some modifiers.
         In that case we use UCKeyTranslate (ns_get_shifted_character)
         to look up the correct character.  */

      /* EV_MODIFIERS2 uses parse_solitary_modifier on all known
         modifier keys, which returns 0 for shift-like modifiers.
         Therefore its return value is the set of control-like
         modifiers.  */
      Lisp_Object kind = fnKeysym ? QCfunction : QCordinary;
      emacs_event->modifiers = EV_MODIFIERS2 (flags, kind);

      if (NS_KEYLOG)
        fprintf (stderr, "keyDown: code =%x\tfnKey =%x\tflags = %x\tmods = %x\n",
                 code, fnKeysym, flags, emacs_event->modifiers);

      /* If it was a function key or had control-like modifiers, pass
         it directly to Emacs.  */
      if (fnKeysym || (emacs_event->modifiers
                       && (emacs_event->modifiers != shift_modifier)
                       && [[theEvent charactersIgnoringModifiers] length] > 0))
        {
          emacs_event->kind = NON_ASCII_KEYSTROKE_EVENT;
          /* FIXME: What are the next four lines supposed to do?  */
          if (code < 0x20)
            code |= (1<<28)|(3<<16);
          else if (code == 0x7f)
            code |= (1<<28)|(3<<16);
          else if (!fnKeysym)
            {
#ifdef NS_IMPL_COCOA
              /* We potentially have both shift- and control-like
                 modifiers in use, so find the correct character
                 ignoring any control-like ones.  */
              code = ns_get_shifted_character (theEvent);
#endif

              /* FIXME: This seems wrong, characters in the range
                 [0x80, 0xFF] are not ASCII characters.  Can’t we just
                 use MULTIBYTE_CHAR_KEYSTROKE_EVENT here for all kinds
                 of characters?  */
              emacs_event->kind = code > 0xFF
                ? MULTIBYTE_CHAR_KEYSTROKE_EVENT : ASCII_KEYSTROKE_EVENT;
            }

          emacs_event->code = code;
          EV_TRAILER (theEvent);
          processingCompose = NO;
          return;
        }
    }

  /* If we get here, a non-function key without control-like modifiers
     was hit.  Use interpretKeyEvents, which in turn will call
     insertText; see
     https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/EventOverview/HandlingKeyEvents/HandlingKeyEvents.html.  */

  if (NS_KEYLOG && !processingCompose)
    fputs ("keyDown: Begin compose sequence.\n", stderr);

  /* FIXME: interpretKeyEvents doesn’t seem to send insertText if ⌘ is
     used as shift-like modifier, at least on El Capitan.  Mask it
     out.  This shouldn’t be needed though; we should figure out what
     the correct way of handling ⌘ is.  */
  if ([theEvent modifierFlags] & NSEventModifierFlagCommand)
    theEvent = [NSEvent keyEventWithType:[theEvent type]
                                location:[theEvent locationInWindow]
                           modifierFlags:[theEvent modifierFlags] & ~NSEventModifierFlagCommand
                               timestamp:[theEvent timestamp]
                            windowNumber:[theEvent windowNumber]
                                 context:nil
                              characters:[theEvent characters]
                        charactersIgnoringModifiers:[theEvent charactersIgnoringModifiers]
                               isARepeat:[theEvent isARepeat]
                                 keyCode:[theEvent keyCode]];

  processingCompose = YES;
  /* FIXME: Use [NSArray arrayWithObject:theEvent]?  */
  [nsEvArray addObject: theEvent];
  [self interpretKeyEvents: nsEvArray];
  [nsEvArray removeObject: theEvent];
}


/* <NSTextInput> implementation (called through [super interpretKeyEvents:]).  */


/* <NSTextInput>: called when done composing;
   NOTE: also called when we delete over working text, followed
   immediately by doCommandBySelector: deleteBackward:  */
- (void)insertText: (id)aString
{
  NSString *s;
  NSUInteger len;

  NSTRACE ("[EmacsView insertText:]");

  if ([aString isKindOfClass:[NSAttributedString class]])
    s = [aString string];
  else
    s = aString;

  len = [s length];

  if (NS_KEYLOG)
    NSLog (@"insertText '%@'\tlen = %lu", aString, (unsigned long) len);
  processingCompose = NO;

  if (!emacs_event)
    return;

  /* First, clear any working text.  */
  if (workingText != nil)
    [self deleteWorkingText];

  /* It might be preferable to use getCharacters:range: below,
     cf. https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/CocoaPerformance/Articles/StringDrawing.html#//apple_ref/doc/uid/TP40001445-112378.
     However, we probably can't use SAFE_NALLOCA here because it might
     exit nonlocally.  */

  /* Now insert the string as keystrokes.  */
  for (NSUInteger i = 0; i < len; i++)
    {
      NSUInteger code = [s characterAtIndex:i];
      if (UTF_16_HIGH_SURROGATE_P (code) && i < len - 1)
        {
          unichar low = [s characterAtIndex:i + 1];
          if (UTF_16_LOW_SURROGATE_P (low))
            {
              code = surrogates_to_codepoint (low, code);
              ++i;
            }
        }
      /* TODO: still need this?  */
      if (code == 0x2DC)
        code = '~'; /* 0x7E */
      if (code != 32) /* Space */
        emacs_event->modifiers = 0;
      emacs_event->kind
	= code > 0xFF ? MULTIBYTE_CHAR_KEYSTROKE_EVENT : ASCII_KEYSTROKE_EVENT;
      emacs_event->code = code;
      EV_TRAILER ((id)nil);
    }
}


/* <NSTextInput>: inserts display of composing characters.  */
- (void)setMarkedText: (id)aString selectedRange: (NSRange)selRange
{
  NSString *str = [aString respondsToSelector: @selector (string)] ?
    [aString string] : aString;

  NSTRACE ("[EmacsView setMarkedText:selectedRange:]");

  if (NS_KEYLOG)
    NSLog (@"setMarkedText '%@' len =%lu range %lu from %lu",
           str, (unsigned long)[str length],
           (unsigned long)selRange.length,
           (unsigned long)selRange.location);

  if ([str length] == 0)
    {
      [self deleteWorkingText];
      return;
    }

  if (!emacs_event)
    return;

  processingCompose = YES;
  [workingText release];
  workingText = [str copy];
  ns_working_text = [workingText lispString];

  emacs_event->kind = NS_TEXT_EVENT;
  emacs_event->code = KEY_NS_PUT_WORKING_TEXT;
  EV_TRAILER ((id)nil);
}


/* Delete display of composing characters [not in <NSTextInput>].  */
- (void)deleteWorkingText
{
  NSTRACE ("[EmacsView deleteWorkingText]");

  if (workingText == nil)
    return;
  if (NS_KEYLOG)
    NSLog(@"deleteWorkingText len =%lu\n", (unsigned long)[workingText length]);
  [workingText release];
  workingText = nil;
  processingCompose = NO;

  if (!emacs_event)
    return;

  emacs_event->kind = NS_TEXT_EVENT;
  emacs_event->code = KEY_NS_UNPUT_WORKING_TEXT;
  EV_TRAILER ((id)nil);
}


- (BOOL)hasMarkedText
{
  NSTRACE ("[EmacsView hasMarkedText]");

  return workingText != nil;
}


- (NSRange)markedRange
{
  NSTRACE ("[EmacsView markedRange]");

  NSRange rng = workingText != nil
    ? NSMakeRange (0, [workingText length]) : NSMakeRange (NSNotFound, 0);
  if (NS_KEYLOG)
    NSLog (@"markedRange request");
  return rng;
}


- (void)unmarkText
{
  NSTRACE ("[EmacsView unmarkText]");

  if (NS_KEYLOG)
    NSLog (@"unmark (accept) text");
  [self deleteWorkingText];
  processingCompose = NO;
}


/* Used to position char selection windows, etc.  */
- (NSRect)firstRectForCharacterRange: (NSRange)theRange
{
  NSRect rect;
  NSPoint pt;
  struct window *win;

  NSTRACE ("[EmacsView firstRectForCharacterRange:]");

  if (NS_KEYLOG)
    NSLog (@"firstRectForCharRange request");

  if (WINDOWP (echo_area_window) && ! NILP (call0 (intern ("ns-in-echo-area"))))
    win = XWINDOW (echo_area_window);
  else
    win = XWINDOW (FRAME_SELECTED_WINDOW (emacsframe));

  rect.size.width = theRange.length * FRAME_COLUMN_WIDTH (emacsframe);
  rect.size.height = FRAME_LINE_HEIGHT (emacsframe);
  pt.x = WINDOW_TEXT_TO_FRAME_PIXEL_X (win, win->phys_cursor.x);
  pt.y = WINDOW_TO_FRAME_PIXEL_Y (win, win->phys_cursor.y
                                       +FRAME_LINE_HEIGHT (emacsframe));

  pt = [self convertPoint: pt toView: nil];

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  if ([[self window] respondsToSelector: @selector(convertRectToScreen:)])
    {
#endif
      rect.origin = pt;
      rect = [(EmacsWindow *) [self window] convertRectToScreen: rect];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
    }
  else
#endif
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 1070 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070 \
  || defined (NS_IMPL_GNUSTEP)
    {
      pt = [[self window] convertBaseToScreen: pt];
      rect.origin = pt;
    }
#endif

  return rect;
}


- (NSInteger)conversationIdentifier
{
  return (NSInteger)self;
}


- (void)doCommandBySelector: (SEL)aSelector
{
  NSTRACE ("[EmacsView doCommandBySelector:]");

  if (NS_KEYLOG)
    NSLog (@"doCommandBySelector: %@", NSStringFromSelector (aSelector));

  processingCompose = NO;
  if (aSelector == @selector (deleteBackward:))
    {
      /* Happens when user backspaces over an ongoing composition:
         throw a 'delete' into the event queue.  */
      if (!emacs_event)
        return;
      emacs_event->kind = NON_ASCII_KEYSTROKE_EVENT;
      emacs_event->code = 0xFF08;
      EV_TRAILER ((id)nil);
    }
}

- (NSArray *)validAttributesForMarkedText
{
  static NSArray *arr = nil;
  if (arr == nil) arr = [NSArray new];
 /* [[NSArray arrayWithObject: NSUnderlineStyleAttributeName] retain]; */
  return arr;
}

- (NSRange)selectedRange
{
  if (NS_KEYLOG)
    NSLog (@"selectedRange request");
  return NSMakeRange (NSNotFound, 0);
}

#if defined (NS_IMPL_COCOA) || GNUSTEP_GUI_MAJOR_VERSION > 0 || \
    GNUSTEP_GUI_MINOR_VERSION > 22
- (NSUInteger)characterIndexForPoint: (NSPoint)thePoint
#else
- (unsigned int)characterIndexForPoint: (NSPoint)thePoint
#endif
{
  if (NS_KEYLOG)
    NSLog (@"characterIndexForPoint request");
  return 0;
}

- (NSAttributedString *)attributedSubstringFromRange: (NSRange)theRange
{
  static NSAttributedString *str = nil;
  if (str == nil) str = [NSAttributedString new];
  if (NS_KEYLOG)
    NSLog (@"attributedSubstringFromRange request");
  return str;
}

/* End <NSTextInput> implementation.  */
/*****************************************************************************/


/* This is what happens when the user presses a mouse button.  */
- (void)mouseDown: (NSEvent *)theEvent
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  NSPoint p = [self convertPoint: [theEvent locationInWindow] fromView: nil];

  NSTRACE ("[EmacsView mouseDown:]");

  if (!emacs_event)
    return;

  dpyinfo->last_mouse_frame = emacsframe;
  /* Appears to be needed to prevent spurious movement events generated on
     button clicks.  */
  emacsframe->mouse_moved = 0;

  if ([theEvent type] == NSEventTypeScrollWheel)
    {
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if ([theEvent respondsToSelector:@selector(hasPreciseScrollingDeltas)])
        {
#endif
          /* If the input device is a touchpad or similar, use precise
           * scrolling deltas.  These are measured in pixels, so we
           * have to add them up until they exceed one line height,
           * then we can send a scroll wheel event.
           *
           * If the device only has coarse scrolling deltas, like a
           * real mousewheel, the deltas represent a ratio of whole
           * lines, so round up the number of lines.  This means we
           * always send one scroll event per click, but can still
           * scroll more than one line if the OS tells us to.
           */
          bool horizontal;
          int lines = 0;
          int scrollUp = NO;

          /* FIXME: At the top or bottom of the buffer we should
           * ignore momentum-phase events.  */
          if (! ns_use_mwheel_momentum
              && [theEvent momentumPhase] != NSEventPhaseNone)
            return;

          if ([theEvent hasPreciseScrollingDeltas])
            {
              static int totalDeltaX, totalDeltaY;
              int lineHeight;

              if (FIXNUMP (ns_mwheel_line_height))
                lineHeight = XFIXNUM (ns_mwheel_line_height);
              else
                {
                  /* FIXME: Use actual line height instead of the default.  */
                  lineHeight = default_line_pixel_height
                    (XWINDOW (FRAME_SELECTED_WINDOW (emacsframe)));
                }

              if ([theEvent phase] == NSEventPhaseBegan)
                {
                  totalDeltaX = 0;
                  totalDeltaY = 0;
                }

              totalDeltaX += [theEvent scrollingDeltaX];
              totalDeltaY += [theEvent scrollingDeltaY];

              /* Calculate the number of lines, if any, to scroll, and
               * reset the total delta for the direction we're NOT
               * scrolling so that small movements don't add up.  */
              if (abs (totalDeltaX) > abs (totalDeltaY)
                  && abs (totalDeltaX) > lineHeight)
                {
                  horizontal = YES;
                  scrollUp = totalDeltaX > 0;

                  lines = abs (totalDeltaX / lineHeight);
                  totalDeltaX = totalDeltaX % lineHeight;
                  totalDeltaY = 0;
                }
              else if (abs (totalDeltaY) >= abs (totalDeltaX)
                       && abs (totalDeltaY) > lineHeight)
                {
                  horizontal = NO;
                  scrollUp = totalDeltaY > 0;

                  lines = abs (totalDeltaY / lineHeight);
                  totalDeltaY = totalDeltaY % lineHeight;
                  totalDeltaX = 0;
                }

              if (lines > 1 && ! ns_use_mwheel_acceleration)
                lines = 1;
            }
          else
            {
              CGFloat delta;

              if ([theEvent scrollingDeltaY] == 0)
                {
                  horizontal = YES;
                  delta = [theEvent scrollingDeltaX];
                }
              else
                {
                  horizontal = NO;
                  delta = [theEvent scrollingDeltaY];
                }

              lines = (ns_use_mwheel_acceleration)
                ? ceil (fabs (delta)) : 1;

              scrollUp = delta > 0;
            }

          if (lines == 0)
            return;

          emacs_event->kind = horizontal ? HORIZ_WHEEL_EVENT : WHEEL_EVENT;
          emacs_event->arg = (make_fixnum (lines));

          emacs_event->code = 0;
          emacs_event->modifiers = EV_MODIFIERS (theEvent) |
            (scrollUp ? up_modifier : down_modifier);
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
        }
      else
#endif
#endif /* defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070 */
#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 1070
        {
          CGFloat delta = [theEvent deltaY];
          /* Mac notebooks send wheel events with delta equal to 0
	     when trackpad scrolling.  */
          if (delta == 0)
            {
              delta = [theEvent deltaX];
              if (delta == 0)
                {
                  NSTRACE_MSG ("deltaIsZero");
                  return;
                }
              emacs_event->kind = HORIZ_WHEEL_EVENT;
            }
          else
            emacs_event->kind = WHEEL_EVENT;

          emacs_event->code = 0;
          emacs_event->modifiers = EV_MODIFIERS (theEvent) |
            ((delta > 0) ? up_modifier : down_modifier);
        }
#endif
    }
  else
    {
      emacs_event->kind = MOUSE_CLICK_EVENT;
      emacs_event->code = EV_BUTTON (theEvent);
      emacs_event->modifiers = EV_MODIFIERS (theEvent)
                             | EV_UDMODIFIERS (theEvent);
    }

  XSETINT (emacs_event->x, lrint (p.x));
  XSETINT (emacs_event->y, lrint (p.y));
  EV_TRAILER (theEvent);
  return;
}


- (void)rightMouseDown: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView rightMouseDown:]");
  [self mouseDown: theEvent];
}


- (void)otherMouseDown: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView otherMouseDown:]");
  [self mouseDown: theEvent];
}


- (void)mouseUp: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView mouseUp:]");
  [self mouseDown: theEvent];
}


- (void)rightMouseUp: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView rightMouseUp:]");
  [self mouseDown: theEvent];
}


- (void)otherMouseUp: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView otherMouseUp:]");
  [self mouseDown: theEvent];
}


- (void) scrollWheel: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView scrollWheel:]");
  [self mouseDown: theEvent];
}


/* Tell emacs the mouse has moved.  */
- (void)mouseMoved: (NSEvent *)e
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  Lisp_Object frame;
  NSPoint pt;
  BOOL dragging;

  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "[EmacsView mouseMoved:]");

  dpyinfo->last_mouse_movement_time = EV_TIMESTAMP (e);
  pt = [self convertPoint: [e locationInWindow] fromView: nil];
  dpyinfo->last_mouse_motion_x = pt.x;
  dpyinfo->last_mouse_motion_y = pt.y;

  /* Update any mouse face.  */
  if (hlinfo->mouse_face_hidden)
    {
      hlinfo->mouse_face_hidden = 0;
      clear_mouse_face (hlinfo);
    }

  /* Tooltip handling.  */
  previous_help_echo_string = help_echo_string;
  help_echo_string = Qnil;

  if (!NILP (Vmouse_autoselect_window))
    {
      NSTRACE_MSG ("mouse_autoselect_window");
      static Lisp_Object last_mouse_window;
      Lisp_Object window
	= window_from_coordinates (emacsframe, pt.x, pt.y, 0, 0, 0);

      if (WINDOWP (window)
          && !EQ (window, last_mouse_window)
          && !EQ (window, selected_window)
          && (!NILP (focus_follows_mouse)
              || (EQ (XWINDOW (window)->frame,
                      XWINDOW (selected_window)->frame))))
        {
          NSTRACE_MSG ("in_window");
          emacs_event->kind = SELECT_WINDOW_EVENT;
          emacs_event->frame_or_window = window;
          EV_TRAILER2 (e);
        }
      /* Remember the last window where we saw the mouse.  */
      last_mouse_window = window;
    }

  dragging = (e.type == NSEventTypeLeftMouseDragged);
  if (!ns_note_mouse_movement (emacsframe, pt.x, pt.y, dragging))
    help_echo_string = previous_help_echo_string;

  XSETFRAME (frame, emacsframe);
  if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
    {
      /* NOTE: help_echo_{window,pos,object} are set in xdisp.c
         (note_mouse_highlight), which is called through the
         ns_note_mouse_movement () call above.  */
      any_help_event_p = YES;
      gen_help_event (help_echo_string, frame, help_echo_window,
                      help_echo_object, help_echo_pos);
    }

  if (emacsframe->mouse_moved && send_appdefined)
    ns_send_appdefined (-1);
}


- (void)mouseDragged: (NSEvent *)e
{
  NSTRACE ("[EmacsView mouseDragged:]");
  [self mouseMoved: e];
}


- (void)rightMouseDragged: (NSEvent *)e
{
  NSTRACE ("[EmacsView rightMouseDragged:]");
  [self mouseMoved: e];
}


- (void)otherMouseDragged: (NSEvent *)e
{
  NSTRACE ("[EmacsView otherMouseDragged:]");
  [self mouseMoved: e];
}


- (BOOL)windowShouldClose: (id)sender
{
  NSEvent *e =[[self window] currentEvent];

  NSTRACE ("[EmacsView windowShouldClose:]");
  windowClosing = YES;
  if (!emacs_event)
    return NO;
  emacs_event->kind = DELETE_WINDOW_EVENT;
  emacs_event->modifiers = 0;
  emacs_event->code = 0;
  EV_TRAILER (e);
  /* Don't close this window, let this be done from lisp code.  */
  return NO;
}


- (NSSize)windowWillResize: (NSWindow *)sender toSize: (NSSize)frameSize
/* Normalize frame to gridded text size.  */
{
  int extra = 0;
  int cols, rows;

  NSTRACE ("[EmacsView windowWillResize:toSize: " NSTRACE_FMT_SIZE "]",
           NSTRACE_ARG_SIZE (frameSize));
  NSTRACE_RECT   ("[sender frame]", [sender frame]);
  NSTRACE_FSTYPE ("fs_state", fs_state);

  if (!FRAME_LIVE_P (emacsframe))
    return frameSize;

  if (fs_state == FULLSCREEN_MAXIMIZED
      && (maximized_width != (int)frameSize.width
          || maximized_height != (int)frameSize.height))
    [self setFSValue: FULLSCREEN_NONE];
  else if (fs_state == FULLSCREEN_WIDTH
           && maximized_width != (int)frameSize.width)
    [self setFSValue: FULLSCREEN_NONE];
  else if (fs_state == FULLSCREEN_HEIGHT
           && maximized_height != (int)frameSize.height)
    [self setFSValue: FULLSCREEN_NONE];

  if (fs_state == FULLSCREEN_NONE)
    maximized_width = maximized_height = -1;

  if (! [self isFullscreen])
    {
      extra = FRAME_NS_TITLEBAR_HEIGHT (emacsframe)
        + FRAME_TOOLBAR_HEIGHT (emacsframe);
    }

  cols = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (emacsframe, frameSize.width);
  if (cols < MINWIDTH)
    cols = MINWIDTH;

  rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (emacsframe,
                                           frameSize.height - extra);
  if (rows < MINHEIGHT)
    rows = MINHEIGHT;
#ifdef NS_IMPL_COCOA
  {
    /* This sets window title to have size in it; the wm does this under GS.  */
    NSRect r = [[self window] frame];
    if (r.size.height == frameSize.height && r.size.width == frameSize.width)
      {
        if (old_title != 0)
          {
            xfree (old_title);
            old_title = 0;
          }
      }
    else if (fs_state == FULLSCREEN_NONE && ! maximizing_resize
             && [[self window] title] != NULL)
      {
        char *size_title;
        NSWindow *window = [self window];
        if (old_title == 0)
          {
            char *t = strdup ([[[self window] title] UTF8String]);
            char *pos = strstr (t, "  —  ");
            if (pos)
              *pos = '\0';
            old_title = t;
          }
        size_title = xmalloc (strlen (old_title) + 40);
	esprintf (size_title, "%s  —  (%d × %d)", old_title, cols, rows);
        [window setTitle: [NSString stringWithUTF8String: size_title]];
        [window display];
        xfree (size_title);
      }
  }
#endif /* NS_IMPL_COCOA */

  NSTRACE_MSG ("cols: %d  rows: %d", cols, rows);

  /* Restrict the new size to the text grid.

     Don't restrict the width if the user only adjusted the height, and
     vice versa.  (Without this, the frame would shrink, and move
     slightly, if the window was resized by dragging one of its
     borders.)  */
  if (!frame_resize_pixelwise)
    {
      NSRect r = [[self window] frame];

      if (r.size.width != frameSize.width)
        {
          frameSize.width =
            FRAME_TEXT_COLS_TO_PIXEL_WIDTH  (emacsframe, cols);
        }

      if (r.size.height != frameSize.height)
        {
          frameSize.height =
            FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (emacsframe, rows) + extra;
        }
    }

  NSTRACE_RETURN_SIZE (frameSize);

  return frameSize;
}


- (void)windowDidResize: (NSNotification *)notification
{
  NSTRACE ("[EmacsView windowDidResize:]");
  if (!FRAME_LIVE_P (emacsframe))
    {
      NSTRACE_MSG ("Ignored (frame dead)");
      return;
    }
  if (emacsframe->output_data.ns->in_animation)
    {
      NSTRACE_MSG ("Ignored (in animation)");
      return;
    }

  if (! [self fsIsNative])
    {
      NSWindow *theWindow = [notification object];
      /* We can get notification on the non-FS window when in
         fullscreen mode.  */
      if ([self window] != theWindow) return;
    }

  NSTRACE_RECT ("frame", [[notification object] frame]);

#ifdef NS_IMPL_GNUSTEP
  NSWindow *theWindow = [notification object];

   /* In GNUstep, at least currently, it's possible to get a didResize
      without getting a willResize, therefore we need to act as if we got
      the willResize now.  */
  NSSize sz = [theWindow frame].size;
  sz = [self windowWillResize: theWindow toSize: sz];
#endif /* NS_IMPL_GNUSTEP */

  ns_send_appdefined (-1);
}

#ifdef NS_IMPL_COCOA
- (void)viewDidEndLiveResize
{
  NSTRACE ("[EmacsView viewDidEndLiveResize]");

  [super viewDidEndLiveResize];
  if (old_title != 0)
    {
      [[self window] setTitle: [NSString stringWithUTF8String: old_title]];
      xfree (old_title);
      old_title = 0;
    }
  maximizing_resize = NO;
}
#endif /* NS_IMPL_COCOA */


- (void)viewDidResize:(NSNotification *)notification
{
  NSRect frame = [self frame];
  int neww, newh;

  if (! FRAME_LIVE_P (emacsframe))
    return;

  NSTRACE ("[EmacsView viewDidResize]");

  neww = (int)NSWidth (frame);
  newh = (int)NSHeight (frame);
  NSTRACE_SIZE ("New size", NSMakeSize (neww, newh));

#ifdef NS_DRAW_TO_BUFFER
  if ([self wantsUpdateLayer])
    {
      CGFloat scale = [[self window] backingScaleFactor];
      NSSize size = [surface getSize];
      int oldw = size.width / scale;
      int oldh = size.height / scale;

      NSTRACE_SIZE ("Original size", NSMakeSize (oldw, oldh));

      /* Don't want to do anything when the view size hasn't changed. */
      if ((oldh == newh && oldw == neww))
        {
          NSTRACE_MSG ("No change");
          return;
        }

      [surface release];
      surface = nil;

      [self setNeedsDisplay:YES];
    }
#endif

  /* I'm not sure if it's safe to call this every time the view
     changes size, as Emacs may already know about the change.
     Unfortunately there doesn't seem to be a bullet-proof method of
     determining whether we need to call it or not.  */
  change_frame_size (emacsframe,
                     FRAME_PIXEL_TO_TEXT_WIDTH (emacsframe, neww),
                     FRAME_PIXEL_TO_TEXT_HEIGHT (emacsframe, newh),
                     0, YES, 0, 1);

  SET_FRAME_GARBAGED (emacsframe);
  cancel_mouse_face (emacsframe);
}


- (void)windowDidBecomeKey: (NSNotification *)notification
/* cf. x_detect_focus_change(), x_focus_changed(), x_new_focus_frame() */
{
  [self windowDidBecomeKey];
}


- (void)windowDidBecomeKey      /* for direct calls */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  struct frame *old_focus = dpyinfo->ns_focus_frame;

  NSTRACE ("[EmacsView windowDidBecomeKey]");

  if (emacsframe != old_focus)
    dpyinfo->ns_focus_frame = emacsframe;

  ns_frame_rehighlight (emacsframe);

  if (emacs_event)
    {
      emacs_event->kind = FOCUS_IN_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowDidResignKey: (NSNotification *)notification
/* cf. x_detect_focus_change(), x_focus_changed(), x_new_focus_frame() */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  BOOL is_focus_frame = dpyinfo->ns_focus_frame == emacsframe;
  NSTRACE ("[EmacsView windowDidResignKey:]");

  if (is_focus_frame)
    dpyinfo->ns_focus_frame = 0;

  emacsframe->mouse_moved = 0;
  ns_frame_rehighlight (emacsframe);

  /* FIXME: for some reason needed on second and subsequent clicks away
            from sole-frame Emacs to get hollow box to show.  */
  if (!windowClosing && [[self window] isVisible] == YES)
    {
      gui_update_cursor (emacsframe, 1);
      ns_set_frame_alpha (emacsframe);
    }

  if (any_help_event_p)
    {
      Lisp_Object frame;
      XSETFRAME (frame, emacsframe);
      help_echo_string = Qnil;
      gen_help_event (Qnil, frame, Qnil, Qnil, 0);
    }

  if (emacs_event && is_focus_frame)
    {
      emacs_event->kind = FOCUS_OUT_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowWillMiniaturize: sender
{
  NSTRACE ("[EmacsView windowWillMiniaturize:]");
}


- (void)setFrame:(NSRect)frameRect
{
  NSTRACE ("[EmacsView setFrame:" NSTRACE_FMT_RECT "]",
           NSTRACE_ARG_RECT (frameRect));

  [super setFrame:(NSRect)frameRect];
}


- (BOOL)isFlipped
{
  return YES;
}


- (BOOL)isOpaque
{
  return NO;
}


- (void)createToolbar: (struct frame *)f
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  toolbar = [[EmacsToolbar alloc] initForView: self withIdentifier:
                   [NSString stringWithFormat: @"Emacs Frame %d",
                             ns_window_num]];
  [toolbar setVisible: NO];
  [window setToolbar: toolbar];

  /* Don't set frame garbaged until tool bar is up to date?
     This avoids an extra clear and redraw (flicker) at frame creation.  */
  if (FRAME_EXTERNAL_TOOL_BAR (f)) wait_for_tool_bar = YES;
  else wait_for_tool_bar = NO;


#ifdef NS_IMPL_COCOA
  {
    NSButton *toggleButton;
    toggleButton = [window standardWindowButton: NSWindowToolbarButton];
    [toggleButton setTarget: self];
    [toggleButton setAction: @selector (toggleToolbar: )];
  }
#endif
}


- (instancetype) initFrameFromEmacs: (struct frame *)f
{
  NSRect r, wr;
  Lisp_Object tem;
  EmacsWindow *win;
  NSColor *col;
  NSString *name;

  NSTRACE ("[EmacsView initFrameFromEmacs:]");
  NSTRACE_MSG ("cols:%d lines:%d", f->text_cols, f->text_lines);

  windowClosing = NO;
  processingCompose = NO;
  scrollbarsNeedingUpdate = 0;
  fs_state = FULLSCREEN_NONE;
  fs_before_fs = next_maximized = -1;

  fs_is_native = NO;
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  if (NSAppKitVersionNumber >= NSAppKitVersionNumber10_7)
#endif
    fs_is_native = ns_use_native_fullscreen;
#endif
  in_fullscreen_transition = NO;

  maximized_width = maximized_height = -1;
  nonfs_window = nil;

  ns_userRect = NSMakeRect (0, 0, 0, 0);
  r = NSMakeRect (0, 0, FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, f->text_cols),
                 FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, f->text_lines));
  [self initWithFrame: r];
  [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

#ifdef NS_DRAW_TO_BUFFER
  /* These settings mean AppKit will retain the contents of the frame
     on resize.  Unfortunately it also means the frame will not be
     automatically marked for display, but we can do that ourselves in
     viewDidResize.  */
  [self setLayerContentsRedrawPolicy:
          NSViewLayerContentsRedrawOnSetNeedsDisplay];
  [self setLayerContentsPlacement:NSViewLayerContentsPlacementTopLeft];
#endif

  FRAME_NS_VIEW (f) = self;
  emacsframe = f;
#ifdef NS_IMPL_COCOA
  old_title = 0;
  maximizing_resize = NO;
#endif

  win = [[EmacsWindow alloc]
            initWithContentRect: r
                      styleMask: (FRAME_UNDECORATED (f)
                                  ? FRAME_UNDECORATED_FLAGS
                                  : FRAME_DECORATED_FLAGS)
                        backing: NSBackingStoreBuffered
                          defer: YES];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  if (NSAppKitVersionNumber >= NSAppKitVersionNumber10_7)
#endif
    if (FRAME_PARENT_FRAME (f))
      [win setCollectionBehavior:NSWindowCollectionBehaviorFullScreenAuxiliary];
    else
      [win setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
#endif

  wr = [win frame];
  bwidth = f->border_width = wr.size.width - r.size.width;

  [win setAcceptsMouseMovedEvents: YES];
  [win setDelegate: self];
#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MIN_REQUIRED <= 1090
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
  if ([win respondsToSelector: @selector(useOptimizedDrawing:)])
#endif
    [win useOptimizedDrawing: YES];
#endif

  [[win contentView] addSubview: self];

  if (ns_drag_types)
    [self registerForDraggedTypes: ns_drag_types];

  tem = f->name;
  name = NILP (tem) ? @"Emacs" : [NSString stringWithLispString:tem];
  [win setTitle: name];

  /* toolbar support */
  if (! FRAME_UNDECORATED (f))
    [self createToolbar: f];


  [win setAppearance];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  if ([win respondsToSelector: @selector(titlebarAppearsTransparent)])
    win.titlebarAppearsTransparent = FRAME_NS_TRANSPARENT_TITLEBAR (f);
#endif

  tem = f->icon_name;
  if (!NILP (tem))
    [win setMiniwindowTitle:
           [NSString stringWithLispString:tem]];

  if (FRAME_PARENT_FRAME (f) != NULL)
    {
      NSWindow *parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];
      [parent addChildWindow: win
                     ordered: NSWindowAbove];
    }

  if (FRAME_Z_GROUP (f) != z_group_none)
      win.level = NSNormalWindowLevel
        + (FRAME_Z_GROUP_BELOW (f) ? -1 : 1);

  {
    NSScreen *screen = [win screen];

    if (screen != 0)
      {
        NSPoint pt = NSMakePoint
          (IN_BOUND (-SCREENMAX, f->left_pos
                     + NS_PARENT_WINDOW_LEFT_POS (f), SCREENMAX),
           IN_BOUND (-SCREENMAX,
                     NS_PARENT_WINDOW_TOP_POS (f) - f->top_pos,
                     SCREENMAX));

        [win setFrameTopLeftPoint: pt];

        NSTRACE_RECT ("new frame", [win frame]);
      }
  }

  [win makeFirstResponder: self];

  col = ns_lookup_indexed_color (NS_FACE_BACKGROUND
				 (FACE_FROM_ID (emacsframe, DEFAULT_FACE_ID)),
				 emacsframe);
  [win setBackgroundColor: col];
  if ([col alphaComponent] != (EmacsCGFloat) 1.0)
    [win setOpaque: NO];

#if !defined (NS_IMPL_COCOA) \
  || MAC_OS_X_VERSION_MIN_REQUIRED <= 1090
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
  if ([self respondsToSelector: @selector(allocateGState)])
#endif
    [self allocateGState];
#endif
  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: [NSArray array]];

  /* Set up view resize notifications.  */
  [self setPostsFrameChangedNotifications:YES];
  [[NSNotificationCenter defaultCenter]
      addObserver:self
         selector:@selector (viewDidResize:)
             name:NSViewFrameDidChangeNotification object:nil];

  /* macOS Sierra automatically enables tabbed windows.  We can't
     allow this to be enabled until it's available on a Free system.
     Currently it only happens by accident and is buggy anyway.  */
#ifdef NS_IMPL_COCOA
  if ([win respondsToSelector: @selector(setTabbingMode:)])
    [win setTabbingMode: NSWindowTabbingModeDisallowed];
#endif

  ns_window_num++;
  return self;
}


- (void)windowDidMove: sender
{
  NSWindow *win = [self window];
  NSRect r = [win frame];
  NSArray *screens = [NSScreen screens];
  NSScreen *screen = [screens objectAtIndex: 0];

  NSTRACE ("[EmacsView windowDidMove:]");

  if (!emacsframe->output_data.ns)
    return;
  if (screen != nil)
    {
      emacsframe->left_pos = NSMinX (r) - NS_PARENT_WINDOW_LEFT_POS (emacsframe);
      emacsframe->top_pos = NS_PARENT_WINDOW_TOP_POS (emacsframe) - NSMaxY (r);

      // FIXME: after event part below didExitFullScreen is not received
      // if (emacs_event)
      //   {
      //     emacs_event->kind = MOVE_FRAME_EVENT;
      //     EV_TRAILER ((id)nil);
      //   }
    }
}


/* Called AFTER method below, but before our windowWillResize call there leads
   to windowDidResize -> ns_set_window_size.  Update emacs' notion of frame
   location so set_window_size moves the frame.  */
- (BOOL)windowShouldZoom: (NSWindow *)sender toFrame: (NSRect)newFrame
{
  NSTRACE (("[EmacsView windowShouldZoom:toFrame:" NSTRACE_FMT_RECT "]"
            NSTRACE_FMT_RETURN "YES"),
           NSTRACE_ARG_RECT (newFrame));

  emacsframe->output_data.ns->zooming = 1;
  return YES;
}


/* Override to do something slightly nonstandard, but nice.  First click on
   zoom button will zoom vertically.  Second will zoom completely.  Third
   returns to original.  */
- (NSRect)windowWillUseStandardFrame:(NSWindow *)sender
                        defaultFrame:(NSRect)defaultFrame
{
  // TODO: Rename to "currentFrame" and assign "result" properly in
  // all paths.
  NSRect result = [sender frame];

  NSTRACE (("[EmacsView windowWillUseStandardFrame:defaultFrame:"
            NSTRACE_FMT_RECT "]"),
           NSTRACE_ARG_RECT (defaultFrame));
  NSTRACE_FSTYPE ("fs_state", fs_state);
  NSTRACE_FSTYPE ("fs_before_fs", fs_before_fs);
  NSTRACE_FSTYPE ("next_maximized", next_maximized);
  NSTRACE_RECT   ("ns_userRect", ns_userRect);
  NSTRACE_RECT   ("[sender frame]", [sender frame]);

  if (fs_before_fs != -1) /* Entering fullscreen */
    {
      NSTRACE_MSG ("Entering fullscreen");
      result = defaultFrame;
    }
  else
    {
      // Save the window size and position (frame) before the resize.
      if (fs_state != FULLSCREEN_MAXIMIZED
          && fs_state != FULLSCREEN_WIDTH)
        {
          ns_userRect.size.width = result.size.width;
          ns_userRect.origin.x   = result.origin.x;
        }

      if (fs_state != FULLSCREEN_MAXIMIZED
          && fs_state != FULLSCREEN_HEIGHT)
        {
          ns_userRect.size.height = result.size.height;
          ns_userRect.origin.y    = result.origin.y;
        }

      NSTRACE_RECT ("ns_userRect (2)", ns_userRect);

      if (next_maximized == FULLSCREEN_HEIGHT
          || (next_maximized == -1
              && abs ((int)(defaultFrame.size.height - result.size.height))
              > FRAME_LINE_HEIGHT (emacsframe)))
        {
          /* first click */
          NSTRACE_MSG ("FULLSCREEN_HEIGHT");
          maximized_height = result.size.height = defaultFrame.size.height;
          maximized_width = -1;
          result.origin.y = defaultFrame.origin.y;
          if (ns_userRect.size.height != 0)
            {
              result.origin.x = ns_userRect.origin.x;
              result.size.width = ns_userRect.size.width;
            }
          [self setFSValue: FULLSCREEN_HEIGHT];
#ifdef NS_IMPL_COCOA
          maximizing_resize = YES;
#endif
        }
      else if (next_maximized == FULLSCREEN_WIDTH)
        {
          NSTRACE_MSG ("FULLSCREEN_WIDTH");
          maximized_width = result.size.width = defaultFrame.size.width;
          maximized_height = -1;
          result.origin.x = defaultFrame.origin.x;
          if (ns_userRect.size.width != 0)
            {
              result.origin.y = ns_userRect.origin.y;
              result.size.height = ns_userRect.size.height;
            }
          [self setFSValue: FULLSCREEN_WIDTH];
        }
      else if (next_maximized == FULLSCREEN_MAXIMIZED
               || (next_maximized == -1
                   && abs ((int)(defaultFrame.size.width - result.size.width))
                   > FRAME_COLUMN_WIDTH (emacsframe)))
        {
          NSTRACE_MSG ("FULLSCREEN_MAXIMIZED");

          result = defaultFrame; /* second click */
          maximized_width = result.size.width;
          maximized_height = result.size.height;
          [self setFSValue: FULLSCREEN_MAXIMIZED];
#ifdef NS_IMPL_COCOA
          maximizing_resize = YES;
#endif
        }
      else
        {
          /* restore */
          NSTRACE_MSG ("Restore");
          result = ns_userRect.size.height ? ns_userRect : result;
          NSTRACE_RECT ("restore (2)", result);
          ns_userRect = NSMakeRect (0, 0, 0, 0);
#ifdef NS_IMPL_COCOA
          maximizing_resize = fs_state != FULLSCREEN_NONE;
#endif
          [self setFSValue: FULLSCREEN_NONE];
          maximized_width = maximized_height = -1;
        }
    }

  if (fs_before_fs == -1) next_maximized = -1;

  NSTRACE_RECT   ("Final ns_userRect", ns_userRect);
  NSTRACE_MSG    ("Final maximized_width: %d", maximized_width);
  NSTRACE_MSG    ("Final maximized_height: %d", maximized_height);
  NSTRACE_FSTYPE ("Final next_maximized", next_maximized);

  [self windowWillResize: sender toSize: result.size];

  NSTRACE_RETURN_RECT (result);

  return result;
}


- (void)windowDidDeminiaturize: sender
{
  NSTRACE ("[EmacsView windowDidDeminiaturize:]");
  if (!emacsframe->output_data.ns)
    return;

  SET_FRAME_ICONIFIED (emacsframe, 0);
  SET_FRAME_VISIBLE (emacsframe, 1);
  windows_or_buffers_changed = 63;

  if (emacs_event)
    {
      emacs_event->kind = DEICONIFY_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowDidExpose: sender
{
  NSTRACE ("[EmacsView windowDidExpose:]");
  if (!emacsframe->output_data.ns)
    return;

  SET_FRAME_VISIBLE (emacsframe, 1);
  SET_FRAME_GARBAGED (emacsframe);

  if (send_appdefined)
    ns_send_appdefined (-1);
}


- (void)windowDidMiniaturize: sender
{
  NSTRACE ("[EmacsView windowDidMiniaturize:]");
  if (!emacsframe->output_data.ns)
    return;

  SET_FRAME_ICONIFIED (emacsframe, 1);
  SET_FRAME_VISIBLE (emacsframe, 0);

  if (emacs_event)
    {
      emacs_event->kind = ICONIFY_EVENT;
      EV_TRAILER ((id)nil);
    }
}

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
- (NSApplicationPresentationOptions)window:(NSWindow *)window
      willUseFullScreenPresentationOptions:
  (NSApplicationPresentationOptions)proposedOptions
{
  return proposedOptions|NSApplicationPresentationAutoHideToolbar;
}
#endif

- (void)windowWillEnterFullScreen:(NSNotification *)notification
{
  NSTRACE ("[EmacsView windowWillEnterFullScreen:]");
  in_fullscreen_transition = YES;
  [self windowWillEnterFullScreen];
}
- (void)windowWillEnterFullScreen /* provided for direct calls */
{
  NSTRACE ("[EmacsView windowWillEnterFullScreen]");
  fs_before_fs = fs_state;
}

- (void)windowDidEnterFullScreen:(NSNotification *)notification
{
  NSTRACE ("[EmacsView windowDidEnterFullScreen:]");
  [self windowDidEnterFullScreen];
  in_fullscreen_transition = NO;
}

- (void)windowDidEnterFullScreen /* provided for direct calls */
{
  NSTRACE ("[EmacsView windowDidEnterFullScreen]");
  [self setFSValue: FULLSCREEN_BOTH];
  if (! [self fsIsNative])
    {
      [self windowDidBecomeKey];
      [nonfs_window orderOut:self];
    }
  else
    {
      BOOL tbar_visible = FRAME_EXTERNAL_TOOL_BAR (emacsframe) ? YES : NO;
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070 \
  && MAC_OS_X_VERSION_MIN_REQUIRED <= 1070
      unsigned val = (unsigned)[NSApp presentationOptions];

      // Mac OS X 10.7 bug fix, the menu won't appear without this.
      // val is non-zero on other macOS versions.
      if (val == 0)
        {
          NSApplicationPresentationOptions options
            = NSApplicationPresentationAutoHideDock
            | NSApplicationPresentationAutoHideMenuBar
            | NSApplicationPresentationFullScreen
            | NSApplicationPresentationAutoHideToolbar;

          [NSApp setPresentationOptions: options];
        }
#endif
      [toolbar setVisible:tbar_visible];
    }
}

- (void)windowWillExitFullScreen:(NSNotification *)notification
{
  NSTRACE ("[EmacsView windowWillExitFullScreen:]");
  in_fullscreen_transition = YES;
  [self windowWillExitFullScreen];
}

- (void)windowWillExitFullScreen /* provided for direct calls */
{
  NSTRACE ("[EmacsView windowWillExitFullScreen]");
  if (!FRAME_LIVE_P (emacsframe))
    {
      NSTRACE_MSG ("Ignored (frame dead)");
      return;
    }
  if (next_maximized != -1)
    fs_before_fs = next_maximized;
}

- (void)windowDidExitFullScreen:(NSNotification *)notification
{
  NSTRACE ("[EmacsView windowDidExitFullScreen:]");
  [self windowDidExitFullScreen];
  in_fullscreen_transition = NO;
}

- (void)windowDidExitFullScreen /* provided for direct calls */
{
  NSTRACE ("[EmacsView windowDidExitFullScreen]");
  if (!FRAME_LIVE_P (emacsframe))
    {
      NSTRACE_MSG ("Ignored (frame dead)");
      return;
    }
  [self setFSValue: fs_before_fs];
  fs_before_fs = -1;
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  [self updateCollectionBehavior];
#endif
  if (FRAME_EXTERNAL_TOOL_BAR (emacsframe))
    {
      [toolbar setVisible:YES];
      update_frame_tool_bar (emacsframe);
      [[self window] display];
    }
  else
    [toolbar setVisible:NO];

  if (next_maximized != -1)
    [[self window] performZoom:self];
}

- (BOOL)inFullScreenTransition
{
  return in_fullscreen_transition;
}

- (void)waitFullScreenTransition
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  while ([self inFullScreenTransition])
    {
      NSTRACE ("wait for fullscreen");
      wait_reading_process_output (0, 300000000, 0, 1, Qnil, NULL, 0);
    }
#endif
}

- (BOOL)fsIsNative
{
  return fs_is_native;
}

- (BOOL)isFullscreen
{
  BOOL res;

  if (! fs_is_native)
    {
      res = (nonfs_window != nil);
    }
  else
    {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
      res = (([[self window] styleMask] & NSWindowStyleMaskFullScreen) != 0);
#else
      res = NO;
#endif
    }

  NSTRACE ("[EmacsView isFullscreen] " NSTRACE_FMT_RETURN " %d",
           (int) res);

  return res;
}

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
- (void)updateCollectionBehavior
{
  NSTRACE ("[EmacsView updateCollectionBehavior]");

  if (! [self isFullscreen])
    {
      NSWindow *win = [self window];
      NSWindowCollectionBehavior b = [win collectionBehavior];
      if (ns_use_native_fullscreen)
        {
          if ([win parentWindow])
            {
              b &= ~NSWindowCollectionBehaviorFullScreenPrimary;
              b |= NSWindowCollectionBehaviorFullScreenAuxiliary;
            }
          else
            {
              b |= NSWindowCollectionBehaviorFullScreenPrimary;
              b &= ~NSWindowCollectionBehaviorFullScreenAuxiliary;
            }
        }
      else
        {
          b &= ~NSWindowCollectionBehaviorFullScreenPrimary;
        }

      [win setCollectionBehavior: b];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if (NSAppKitVersionNumber >= NSAppKitVersionNumber10_7)
#endif
        fs_is_native = ns_use_native_fullscreen;
    }
}
#endif

- (void)toggleFullScreen: (id)sender
{
  NSWindow *w, *fw;
  BOOL onFirstScreen;
  struct frame *f;
  NSRect r, wr;
  NSColor *col;

  NSTRACE ("[EmacsView toggleFullScreen:]");

  if (fs_is_native)
    {
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if ([[self window] respondsToSelector: @selector(toggleFullScreen:)])
        {
#endif
          [[self window] toggleFullScreen:sender];
          // wait for fullscreen animation complete (bug#28496)
          [self waitFullScreenTransition];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
        }
#endif
#endif
      return;
    }

  w = [self window];
  onFirstScreen = [[w screen] isEqual:[[NSScreen screens] objectAtIndex:0]];
  f = emacsframe;
  wr = [w frame];
  col = ns_lookup_indexed_color (NS_FACE_BACKGROUND
				 (FACE_FROM_ID (f, DEFAULT_FACE_ID)),
                                 f);

  if (fs_state != FULLSCREEN_BOTH)
    {
      NSScreen *screen = [w screen];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1090
      /* Hide ghost menu bar on secondary monitor?  */
      if (! onFirstScreen
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1090
          && [NSScreen respondsToSelector: @selector(screensHaveSeparateSpaces)]
#endif
          )
        onFirstScreen = [NSScreen screensHaveSeparateSpaces];
#endif
      /* Hide dock and menubar if we are on the primary screen.  */
      if (onFirstScreen)
        {
#ifdef NS_IMPL_COCOA
          NSApplicationPresentationOptions options
            = NSApplicationPresentationAutoHideDock
            | NSApplicationPresentationAutoHideMenuBar;

          [NSApp setPresentationOptions: options];
#else
          [NSMenu setMenuBarVisible:NO];
#endif
        }

      fw = [[EmacsFSWindow alloc]
                       initWithContentRect:[w contentRectForFrameRect:wr]
                                 styleMask:NSWindowStyleMaskBorderless
                                   backing:NSBackingStoreBuffered
                                     defer:YES
                                    screen:screen];

      [fw setContentView:[w contentView]];
      [fw setTitle:[w title]];
      [fw setDelegate:self];
      [fw setAcceptsMouseMovedEvents: YES];
#if !defined (NS_IMPL_COCOA) \
  || MAC_OS_X_VERSION_MIN_REQUIRED <= 1090
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
      if ([fw respondsToSelector: @selector(useOptimizedDrawing:)])
#endif
        [fw useOptimizedDrawing: YES];
#endif
      [fw setBackgroundColor: col];
      if ([col alphaComponent] != (EmacsCGFloat) 1.0)
        [fw setOpaque: NO];

      f->border_width = 0;

      nonfs_window = w;

      [self windowWillEnterFullScreen];
      [fw makeKeyAndOrderFront:NSApp];
      [fw makeFirstResponder:self];
      [w orderOut:self];
      r = [fw frameRectForContentRect:[screen frame]];
      [fw setFrame: r display:YES animate:ns_use_fullscreen_animation];
      [self windowDidEnterFullScreen];
      [fw display];
    }
  else
    {
      fw = w;
      w = nonfs_window;
      nonfs_window = nil;

      if (onFirstScreen)
        {
#ifdef NS_IMPL_COCOA
          [NSApp setPresentationOptions: NSApplicationPresentationDefault];
#else
          [NSMenu setMenuBarVisible:YES];
#endif
        }

      [w setContentView:[fw contentView]];
      [w setBackgroundColor: col];
      if ([col alphaComponent] != (EmacsCGFloat) 1.0)
        [w setOpaque: NO];

      f->border_width = bwidth;

      // To do: consider using [NSNotificationCenter postNotificationName:] to
      // send notifications.

      [self windowWillExitFullScreen];
      [fw setFrame:[[w contentView] frame]
                    display:YES animate:ns_use_fullscreen_animation];
      [fw close];
      [w makeKeyAndOrderFront:NSApp];
      [self windowDidExitFullScreen];
    }
}

- (void)handleFS
{
  NSTRACE ("[EmacsView handleFS]");

  if (fs_state != emacsframe->want_fullscreen)
    {
      if (fs_state == FULLSCREEN_BOTH)
        {
          NSTRACE_MSG ("fs_state == FULLSCREEN_BOTH");
          [self toggleFullScreen:self];
        }

      switch (emacsframe->want_fullscreen)
        {
        case FULLSCREEN_BOTH:
          NSTRACE_MSG ("FULLSCREEN_BOTH");
          [self toggleFullScreen:self];
          break;
        case FULLSCREEN_WIDTH:
          NSTRACE_MSG ("FULLSCREEN_WIDTH");
          next_maximized = FULLSCREEN_WIDTH;
          if (fs_state != FULLSCREEN_BOTH)
            [[self window] performZoom:self];
          break;
        case FULLSCREEN_HEIGHT:
          NSTRACE_MSG ("FULLSCREEN_HEIGHT");
          next_maximized = FULLSCREEN_HEIGHT;
          if (fs_state != FULLSCREEN_BOTH)
            [[self window] performZoom:self];
          break;
        case FULLSCREEN_MAXIMIZED:
          NSTRACE_MSG ("FULLSCREEN_MAXIMIZED");
          next_maximized = FULLSCREEN_MAXIMIZED;
          if (fs_state != FULLSCREEN_BOTH)
            [[self window] performZoom:self];
          break;
        case FULLSCREEN_NONE:
          NSTRACE_MSG ("FULLSCREEN_NONE");
          if (fs_state != FULLSCREEN_BOTH)
            {
              next_maximized = FULLSCREEN_NONE;
              [[self window] performZoom:self];
            }
          break;
        }

      emacsframe->want_fullscreen = FULLSCREEN_NONE;
    }

}

- (void) setFSValue: (int)value
{
  NSTRACE ("[EmacsView setFSValue:" NSTRACE_FMT_FSTYPE "]",
           NSTRACE_ARG_FSTYPE(value));

  Lisp_Object lval = Qnil;
  switch (value)
    {
    case FULLSCREEN_BOTH:
      lval = Qfullboth;
      break;
    case FULLSCREEN_WIDTH:
      lval = Qfullwidth;
      break;
    case FULLSCREEN_HEIGHT:
      lval = Qfullheight;
      break;
    case FULLSCREEN_MAXIMIZED:
      lval = Qmaximized;
      break;
    }
  store_frame_param (emacsframe, Qfullscreen, lval);
  fs_state = value;
}

- (void)mouseEntered: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsView mouseEntered:]");
  if (emacsframe)
    FRAME_DISPLAY_INFO (emacsframe)->last_mouse_movement_time
      = EV_TIMESTAMP (theEvent);
}


- (void)mouseExited: (NSEvent *)theEvent
{
  Mouse_HLInfo *hlinfo = emacsframe ? MOUSE_HL_INFO (emacsframe) : NULL;

  NSTRACE ("[EmacsView mouseExited:]");

  if (!hlinfo)
    return;

  FRAME_DISPLAY_INFO (emacsframe)->last_mouse_movement_time
    = EV_TIMESTAMP (theEvent);

  if (emacsframe == hlinfo->mouse_face_mouse_frame)
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_mouse_frame = 0;
    }
}


- (instancetype)menuDown: sender
{
  NSTRACE ("[EmacsView menuDown:]");
  if (context_menu_value == -1)
    context_menu_value = [sender tag];
  else
    {
      NSInteger tag = [sender tag];
      find_and_call_menu_selection (emacsframe, emacsframe->menu_bar_items_used,
                                    emacsframe->menu_bar_vector,
                                    (void *)tag);
    }

  ns_send_appdefined (-1);
  return self;
}


- (EmacsToolbar *)toolbar
{
  return toolbar;
}


/* This gets called on toolbar button click.  */
- (instancetype)toolbarClicked: (id)item
{
  NSEvent *theEvent;
  int idx = [item tag] * TOOL_BAR_ITEM_NSLOTS;

  NSTRACE ("[EmacsView toolbarClicked:]");

  if (!emacs_event)
    return self;

  theEvent = [[self window] currentEvent];
  emacs_event->kind = TOOL_BAR_EVENT;
  /* XSETINT (emacs_event->code, 0); */
  emacs_event->arg = AREF (emacsframe->tool_bar_items,
			   idx + TOOL_BAR_ITEM_KEY);
  emacs_event->modifiers = EV_MODIFIERS (theEvent);
  EV_TRAILER (theEvent);
  return self;
}


- (instancetype)toggleToolbar: (id)sender
{
  NSTRACE ("[EmacsView toggleToolbar:]");

  if (!emacs_event)
    return self;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_TOGGLE_TOOLBAR;
  EV_TRAILER ((id)nil);
  return self;
}


#ifdef NS_DRAW_TO_BUFFER
- (void)focusOnDrawingBuffer
{
  CGFloat scale = [[self window] backingScaleFactor];

  NSTRACE ("[EmacsView focusOnDrawingBuffer]");

  if (! surface)
    {
      NSRect frame = [self frame];
      NSSize s = NSMakeSize (NSWidth (frame) * scale, NSHeight (frame) * scale);

      surface = [[EmacsSurface alloc] initWithSize:s
                                        ColorSpace:[[[self window] colorSpace]
                                                     CGColorSpace]];

      /* Since we're using NSViewLayerContentsRedrawOnSetNeedsDisplay
         the layer's scale factor is not set automatically, so do it
         now.  */
      [[self layer] setContentsScale:[[self window] backingScaleFactor]];
    }

  CGContextRef context = [surface getContext];

  CGContextTranslateCTM(context, 0, [surface getSize].height);
  CGContextScaleCTM(context, scale, -scale);

  [NSGraphicsContext
    setCurrentContext:[NSGraphicsContext
                        graphicsContextWithCGContext:context
                                             flipped:YES]];
}


- (void)unfocusDrawingBuffer
{
  NSTRACE ("[EmacsView unfocusDrawingBuffer]");

  [NSGraphicsContext setCurrentContext:nil];
  [surface releaseContext];
  [self setNeedsDisplay:YES];
}


- (void)windowDidChangeBackingProperties:(NSNotification *)notification
  /* Update the drawing buffer when the backing properties change.  */
{
  NSTRACE ("EmacsView windowDidChangeBackingProperties:]");

  NSRect frame = [self frame];

  [surface release];
  surface = nil;

  ns_clear_frame (emacsframe);
  expose_frame (emacsframe, 0, 0, NSWidth (frame), NSHeight (frame));
}
#endif /* NS_DRAW_TO_BUFFER */


- (void)copyRect:(NSRect)srcRect to:(NSRect)dstRect
{
  NSTRACE ("[EmacsView copyRect:To:]");
  NSTRACE_RECT ("Source", srcRect);
  NSTRACE_RECT ("Destination", dstRect);

#ifdef NS_DRAW_TO_BUFFER
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  if ([self wantsUpdateLayer])
    {
#endif
      double scale = [[self window] backingScaleFactor];
      CGContextRef context = [[NSGraphicsContext currentContext] CGContext];
      int bpp = CGBitmapContextGetBitsPerPixel (context) / 8;
      void *pixels = CGBitmapContextGetData (context);
      int rowSize = CGBitmapContextGetBytesPerRow (context);
      int srcRowSize = NSWidth (srcRect) * scale * bpp;
      void *srcPixels = (char *) pixels
                        + (int) (NSMinY (srcRect) * scale * rowSize
                                 + NSMinX (srcRect) * scale * bpp);
      void *dstPixels = (char *) pixels
                        + (int) (NSMinY (dstRect) * scale * rowSize
                                 + NSMinX (dstRect) * scale * bpp);

      if (NSIntersectsRect (srcRect, dstRect)
          && NSMinY (srcRect) < NSMinY (dstRect))
        for (int y = NSHeight (srcRect) * scale - 1 ; y >= 0 ; y--)
          memmove ((char *) dstPixels + y * rowSize,
                   (char *) srcPixels + y * rowSize,
                   srcRowSize);
      else
        for (int y = 0 ; y < NSHeight (srcRect) * scale ; y++)
          memmove ((char *) dstPixels + y * rowSize,
                   (char *) srcPixels + y * rowSize,
                   srcRowSize);

#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
  else
    {
#endif
#endif /* NS_DRAW_TO_BUFFER */

#if !defined (NS_DRAW_TO_BUFFER) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      hide_bell();              // Ensure the bell image isn't scrolled.

      ns_focus (emacsframe, &dstRect, 1);
      [self scrollRect: srcRect
                    by: NSMakeSize (dstRect.origin.x - srcRect.origin.x,
                                    dstRect.origin.y - srcRect.origin.y)];
      ns_unfocus (emacsframe);
#endif
#if defined (NS_DRAW_TO_BUFFER) && MAC_OS_X_VERSION_MIN_REQUIRED < 101400
    }
#endif
}


#ifdef NS_IMPL_COCOA
/* If the frame has been garbaged but the toolkit wants to draw, for
   example when resizing the frame, we end up with a blank screen.
   Sometimes this results in an unpleasant flicker, so try to
   redisplay before drawing.  */
- (void)viewWillDraw
{
  if (FRAME_GARBAGED_P (emacsframe)
      && !redisplaying_p)
    {
      /* If there is IO going on when redisplay is run here Emacs
         crashes.  I think it's because this code will always be run
         within the run loop and for whatever reason processing input
         is dangerous.  This technique was stolen wholesale from
         nsmenu.m and seems to work.  */
      bool owfi = waiting_for_input;
      waiting_for_input = 0;
      block_input ();

      redisplay ();

      unblock_input ();
      waiting_for_input = owfi;
    }
}
#endif


#ifdef NS_DRAW_TO_BUFFER
- (BOOL)wantsUpdateLayer
{
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  if (NSAppKitVersionNumber < 1671)
    return NO;
#endif

  /* Running on macOS 10.14 or above.  */
  return YES;
}


- (void)updateLayer
{
  NSTRACE ("[EmacsView updateLayer]");

  /* We run redisplay on frames that are garbaged, but marked for
     display, before updateLayer is called so if the frame is still
     garbaged that means the last redisplay must have refused to
     update the frame.  */
  if (FRAME_GARBAGED_P (emacsframe))
    return;

  /* This can fail to update the screen if the same surface is
     provided twice in a row, even if its contents have changed.
     There's a private method, -[CALayer setContentsChanged], that we
     could use to force it, but we shouldn't often get the same
     surface twice in a row.  */
  [[self layer] setContents:(id)[surface getSurface]];
}
#endif


- (void)drawRect: (NSRect)rect
{
  NSTRACE ("[EmacsView drawRect:" NSTRACE_FMT_RECT "]",
           NSTRACE_ARG_RECT(rect));

  if (!emacsframe || !emacsframe->output_data.ns)
    return;

  int x = NSMinX (rect), y = NSMinY (rect);
  int width = NSWidth (rect), height = NSHeight (rect);

  ns_clear_frame_area (emacsframe, x, y, width, height);
  block_input ();
  expose_frame (emacsframe, x, y, width, height);
  unblock_input ();
}


/* NSDraggingDestination protocol methods.  Actually this is not really a
   protocol, but a category of Object.  O well...  */

-(NSDragOperation) draggingEntered: (id <NSDraggingInfo>) sender
{
  NSTRACE ("[EmacsView draggingEntered:]");
  return NSDragOperationGeneric;
}


-(BOOL)prepareForDragOperation: (id <NSDraggingInfo>) sender
{
  return YES;
}


-(BOOL)performDragOperation: (id <NSDraggingInfo>) sender
{
  id pb;
  int x, y;
  NSString *type;
  NSEvent *theEvent = [[self window] currentEvent];
  NSPoint position;
  NSDragOperation op = [sender draggingSourceOperationMask];
  Lisp_Object operations = Qnil;
  Lisp_Object strings = Qnil;
  Lisp_Object type_sym;

  NSTRACE ("[EmacsView performDragOperation:]");

  if (!emacs_event)
    return NO;

  position = [self convertPoint: [sender draggingLocation] fromView: nil];
  x = lrint (position.x);  y = lrint (position.y);

  pb = [sender draggingPasteboard];
  type = [pb availableTypeFromArray: ns_drag_types];

  /* We used to convert these drag operations to keyboard modifiers,
     but because they can be set by the sending program as well as the
     keyboard modifiers it was difficult to work out a sensible key
     mapping for drag and drop.  */
  if (op & NSDragOperationLink)
    operations = Fcons (Qns_drag_operation_link, operations);
  if (op & NSDragOperationCopy)
    operations = Fcons (Qns_drag_operation_copy, operations);
  if (op & NSDragOperationGeneric || NILP (operations))
    operations = Fcons (Qns_drag_operation_generic, operations);

  if (type == 0)
    {
      return NO;
    }
#if NS_USE_NSPasteboardTypeFileURL != 0
  else if ([type isEqualToString: NSPasteboardTypeFileURL])
    {
      type_sym = Qfile;

      NSArray *urls = [pb readObjectsForClasses: @[[NSURL self]]
                                        options: nil];
      NSEnumerator *uenum = [urls objectEnumerator];
      NSURL *url;
      while ((url = [uenum nextObject]))
        strings = Fcons ([[url path] lispString], strings);
    }
#else  // !NS_USE_NSPasteboardTypeFileURL
  else if ([type isEqualToString: NSFilenamesPboardType])
    {
      NSArray *files;
      NSEnumerator *fenum;
      NSString *file;

      if (!(files = [pb propertyListForType: type]))
        return NO;

      type_sym = Qfile;

      fenum = [files objectEnumerator];
      while ( (file = [fenum nextObject]) )
        strings = Fcons ([file lispString], strings);
    }
#endif   // !NS_USE_NSPasteboardTypeFileURL
  else if ([type isEqualToString: NSPasteboardTypeURL])
    {
      NSURL *url = [NSURL URLFromPasteboard: pb];
      if (url == nil) return NO;

      type_sym = Qurl;

      strings = list1 ([[url absoluteString] lispString]);
    }
  else if ([type isEqualToString: NSPasteboardTypeString]
           || [type isEqualToString: NSPasteboardTypeTabularText])
    {
      NSString *data;

      if (! (data = [pb stringForType: type]))
        return NO;

      type_sym = Qnil;

      strings = list1 ([data lispString]);
    }
  else
    {
      fputs ("Invalid data type in dragging pasteboard\n", stderr);
      return NO;
    }

  emacs_event->kind = DRAG_N_DROP_EVENT;
  XSETINT (emacs_event->x, x);
  XSETINT (emacs_event->y, y);
  emacs_event->modifiers = 0;

  emacs_event->arg = Fcons (type_sym,
                            Fcons (operations,
                                   strings));
  EV_TRAILER (theEvent);

  return YES;
}


- (id) validRequestorForSendType: (NSString *)typeSent
                      returnType: (NSString *)typeReturned
{
  NSTRACE ("[EmacsView validRequestorForSendType:returnType:]");
  if (typeSent != nil && [ns_send_types indexOfObject: typeSent] != NSNotFound
      && typeReturned == nil)
    {
      if (! NILP (ns_get_local_selection (QPRIMARY, QUTF8_STRING)))
        return self;
    }

  return [super validRequestorForSendType: typeSent
                               returnType: typeReturned];
}


/* The next two methods are part of NSServicesRequests informal protocol,
   supposedly called when a services menu item is chosen from this app.
   But this should not happen because we override the services menu with our
   own entries which call ns-perform-service.
   Nonetheless, it appeared to happen (under strange circumstances): bug#1435.
   So let's at least stub them out until further investigation can be done.  */

- (BOOL) readSelectionFromPasteboard: (NSPasteboard *)pb
{
  /* We could call ns_string_from_pasteboard(pboard) here but then it should
     be written into the buffer in place of the existing selection.
     Ordinary service calls go through functions defined in ns-win.el.  */
  return NO;
}

- (BOOL) writeSelectionToPasteboard: (NSPasteboard *)pb types: (NSArray *)types
{
  NSArray *typesDeclared;
  Lisp_Object val;

  NSTRACE ("[EmacsView writeSelectionToPasteboard:types:]");

  /* We only support NSPasteboardTypeString.  */
  if ([types containsObject:NSPasteboardTypeString] == NO) {
    return NO;
  }

  val = ns_get_local_selection (QPRIMARY, QUTF8_STRING);
  if (CONSP (val) && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
        val = XCAR (val);
    }
  if (! STRINGP (val))
    return NO;

  typesDeclared = [NSArray arrayWithObject:NSPasteboardTypeString];
  [pb declareTypes:typesDeclared owner:nil];
  ns_string_to_pasteboard (pb, val);
  return YES;
}


/* setMini = YES means set from internal (gives a finder icon), NO means set nil
   (gives a miniaturized version of the window); currently we use the latter for
   frames whose active buffer doesn't correspond to any file
   (e.g., '*scratch*').  */
- (instancetype)setMiniwindowImage: (BOOL) setMini
{
  id image = [[self window] miniwindowImage];
  NSTRACE ("[EmacsView setMiniwindowImage:%d]", setMini);

  /* NOTE: under Cocoa miniwindowImage always returns nil, documentation
     about "AppleDockIconEnabled" notwithstanding, however the set message
     below has its effect nonetheless.  */
  if (image != emacsframe->output_data.ns->miniimage)
    {
      if (image && [image isKindOfClass: [EmacsImage class]])
        [image release];
      [[self window] setMiniwindowImage:
                       setMini ? emacsframe->output_data.ns->miniimage : nil];
    }

  return self;
}


- (int) fullscreenState
{
  return fs_state;
}

@end  /* EmacsView */



/* ==========================================================================

    EmacsWindow implementation

   ========================================================================== */

@implementation EmacsWindow

/* It seems the only way to reorder child frames is by removing them
   from the parent and then reattaching them in the correct order.  */

- (void)orderFront:(id)sender
{
  NSTRACE ("[EmacsWindow orderFront:]");

  NSWindow *parent = [self parentWindow];
  if (parent)
    {
      [parent removeChildWindow:self];
      [parent addChildWindow:self ordered:NSWindowAbove];
    }
  else
    [super orderFront:sender];
}

- (void)makeKeyAndOrderFront:(id)sender
{
  NSTRACE ("[EmacsWindow makeKeyAndOrderFront:]");

  if ([self parentWindow])
    {
      [self orderFront:sender];
      [self makeKeyWindow];
    }
  else
    [super makeKeyAndOrderFront:sender];
}


/* The array returned by [NSWindow parentWindow] may already be
   sorted, but the documentation doesn't tell us whether or not it is,
   so to be safe we'll sort it.  */
static NSInteger
nswindow_orderedIndex_sort (id w1, id w2, void *c)
{
  NSInteger i1 = [w1 orderedIndex];
  NSInteger i2 = [w2 orderedIndex];

  if (i1 > i2)
    return NSOrderedAscending;
  if (i1 < i2)
    return NSOrderedDescending;

  return NSOrderedSame;
}

- (void)orderBack:(id)sender
{
  NSTRACE ("[EmacsWindow orderBack:]");

  NSWindow *parent = [self parentWindow];
  if (parent)
    {
      NSArray *children = [[parent childWindows]
                            sortedArrayUsingFunction:nswindow_orderedIndex_sort
                                              context:nil];
      [parent removeChildWindow:self];
      [parent addChildWindow:self ordered:NSWindowAbove];

      for (NSWindow *win in children)
        {
          if (win != self)
            {
              [parent removeChildWindow:win];
              [parent addChildWindow:win ordered:NSWindowAbove];
            }
        }
    }
  else
    [super orderBack:sender];
}

- (BOOL)restackWindow:(NSWindow *)win above:(BOOL)above
{
  NSTRACE ("[EmacsWindow restackWindow:above:]");

  /* If parent windows don't match we can't restack these frames
     without changing the parents.  */
  if ([self parentWindow] != [win parentWindow])
    return NO;
  else if (![self parentWindow])
    [self orderWindow:(above ? NSWindowAbove : NSWindowBelow)
           relativeTo:[win windowNumber]];
  else
    {
      NSInteger index;
      NSWindow *parent = [self parentWindow];
      NSMutableArray *children = [[[parent childWindows]
                                   sortedArrayUsingFunction:nswindow_orderedIndex_sort
                                                    context:nil]
                                   mutableCopy];
      [children removeObject:self];
      index = [children indexOfObject:win];
      [children insertObject:self atIndex:(above ? index+1 : index)];

      for (NSWindow *w in children)
        {
          [parent removeChildWindow:w];
          [parent addChildWindow:w ordered:NSWindowAbove];
        }
    }

  return YES;
}

#ifdef NS_IMPL_COCOA
- (id)accessibilityAttributeValue:(NSString *)attribute
{
  Lisp_Object str = Qnil;
  struct frame *f = SELECTED_FRAME ();
  struct buffer *curbuf = XBUFFER (XWINDOW (f->selected_window)->contents);

  NSTRACE ("[EmacsWindow accessibilityAttributeValue:]");

  if ([attribute isEqualToString:NSAccessibilityRoleAttribute])
    return NSAccessibilityTextFieldRole;

  if ([attribute isEqualToString:NSAccessibilitySelectedTextAttribute]
      && curbuf && ! NILP (BVAR (curbuf, mark_active)))
    {
      str = ns_get_local_selection (QPRIMARY, QUTF8_STRING);
    }
  else if (curbuf && [attribute isEqualToString:NSAccessibilityValueAttribute])
    {
      if (! NILP (BVAR (curbuf, mark_active)))
          str = ns_get_local_selection (QPRIMARY, QUTF8_STRING);

      if (NILP (str))
        {
          ptrdiff_t start_byte = BUF_BEGV_BYTE (curbuf);
          ptrdiff_t byte_range = BUF_ZV_BYTE (curbuf) - start_byte;
          ptrdiff_t range = BUF_ZV (curbuf) - BUF_BEGV (curbuf);

          if (! NILP (BVAR (curbuf, enable_multibyte_characters)))
            str = make_uninit_multibyte_string (range, byte_range);
          else
            str = make_uninit_string (range);
          /* To check: This returns emacs-utf-8, which is a superset of utf-8.
             Is this a problem?  */
          memcpy (SDATA (str), BYTE_POS_ADDR (start_byte), byte_range);
        }
    }


  if (! NILP (str))
    {
      if (CONSP (str) && SYMBOLP (XCAR (str)))
        {
          str = XCDR (str);
          if (CONSP (str) && NILP (XCDR (str)))
            str = XCAR (str);
        }
      if (STRINGP (str))
        {
          return [NSString stringWithLispString:str];
        }
    }

  return [super accessibilityAttributeValue:attribute];
}
#endif /* NS_IMPL_COCOA */

/* Constrain size and placement of a frame.

   By returning the original "frameRect", the frame is not
   constrained. This can lead to unwanted situations where, for
   example, the menu bar covers the frame.

   The default implementation (accessed using "super") constrains the
   frame to the visible area of SCREEN, minus the menu bar (if
   present) and the Dock.  Note that default implementation also calls
   windowWillResize, with the frame it thinks should have.  (This can
   make the frame exit maximized mode.)

   Note that this should work in situations where multiple monitors
   are present.  Common configurations are side-by-side monitors and a
   monitor on top of another (e.g. when a laptop is placed under a
   large screen).  */
- (NSRect)constrainFrameRect:(NSRect)frameRect toScreen:(NSScreen *)screen
{
  NSTRACE ("[EmacsWindow constrainFrameRect:" NSTRACE_FMT_RECT " toScreen:]",
             NSTRACE_ARG_RECT (frameRect));

#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1090
  // If separate spaces is on, it is like each screen is independent.  There is
  // no spanning of frames across screens.
  if (
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1090
      [NSScreen respondsToSelector: @selector(screensHaveSeparateSpaces)] &&
#endif
      [NSScreen screensHaveSeparateSpaces])
    {
      NSTRACE_MSG ("Screens have separate spaces");
      frameRect = [super constrainFrameRect:frameRect toScreen:screen];
      NSTRACE_RETURN_RECT (frameRect);
      return frameRect;
    }
  else
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 1090 */

    // Check that the proposed frameRect is visible in at least one
    // screen.  If it is not, ask the system to reposition it (only
    // for non-child windows).

    if (!FRAME_PARENT_FRAME (((EmacsView *)[self delegate])->emacsframe))
    {
      NSArray *screens = [NSScreen screens];
      NSUInteger nr_screens = [screens count];

      int i;
      BOOL frame_on_screen = NO;

      for (i = 0; i < nr_screens; ++i)
        {
          NSScreen *s = [screens objectAtIndex: i];
          NSRect scrRect = [s frame];

          if (NSIntersectsRect(frameRect, scrRect))
            {
              frame_on_screen = YES;
              break;
            }
        }

      if (!frame_on_screen)
        {
          NSTRACE_MSG ("Frame outside screens; constraining");
          frameRect = [super constrainFrameRect:frameRect toScreen:screen];
          NSTRACE_RETURN_RECT (frameRect);
          return frameRect;
        }
    }
#endif

  return constrain_frame_rect(frameRect,
                              [(EmacsView *)[self delegate] isFullscreen]);
}


- (void)performZoom:(id)sender
{
  NSTRACE ("[EmacsWindow performZoom:]");

  return [super performZoom:sender];
}

- (void)zoom:(id)sender
{
  NSTRACE ("[EmacsWindow zoom:]");

  ns_update_auto_hide_menu_bar();

  // Below are three zoom implementations.  In the final commit, the
  // idea is that the last should be included.

#if 0
  // Native zoom done using the standard zoom animation.  Size of the
  // resulting frame reduced to accommodate the Dock and, if present,
  // the menu-bar.
  [super zoom:sender];

#elif 0
  // Native zoom done using the standard zoom animation, plus an
  // explicit resize to cover the full screen, except the menu-bar and
  // dock, if present.
  [super zoom:sender];

  // After the native zoom, resize the resulting frame to fill the
  // entire screen, except the menu-bar.
  //
  // This works for all practical purposes.  (The only minor oddity is
  // when transiting from full-height frame to a maximized, the
  // animation reduces the height of the frame slightly (to the 4
  // pixels needed to accommodate the Doc) before it snaps back into
  // full height.  The user would need a very trained eye to spot
  // this.)
  NSScreen * screen = [self screen];
  if (screen != nil)
    {
      int fs_state = [(EmacsView *)[self delegate] fullscreenState];

      NSTRACE_FSTYPE ("fullscreenState", fs_state);

      NSRect sr = [screen frame];
      struct EmacsMargins margins
        = ns_screen_margins_ignoring_hidden_dock(screen);

      NSRect wr = [self frame];
      NSTRACE_RECT ("Rect after zoom", wr);

      NSRect newWr = wr;

      if (fs_state == FULLSCREEN_MAXIMIZED
          || fs_state == FULLSCREEN_HEIGHT)
        {
          newWr.origin.y = sr.origin.y + margins.bottom;
          newWr.size.height = sr.size.height - margins.top - margins.bottom;
        }

      if (fs_state == FULLSCREEN_MAXIMIZED
          || fs_state == FULLSCREEN_WIDTH)
        {
          newWr.origin.x = sr.origin.x + margins.left;
          newWr.size.width = sr.size.width - margins.right - margins.left;
        }

      if (newWr.size.width     != wr.size.width
          || newWr.size.height != wr.size.height
          || newWr.origin.x    != wr.origin.x
          || newWr.origin.y    != wr.origin.y)
        {
          NSTRACE_MSG ("New frame different");
          [self setFrame: newWr display: NO];
        }
    }
#else
  // Non-native zoom which is done instantaneously.  The resulting
  // frame covers the entire screen, except the menu-bar and dock, if
  // present.
  NSScreen * screen = [self screen];
  if (screen != nil)
    {
      NSRect sr = [screen frame];
      struct EmacsMargins margins
        = ns_screen_margins_ignoring_hidden_dock(screen);

      sr.size.height -= (margins.top + margins.bottom);
      sr.size.width  -= (margins.left + margins.right);
      sr.origin.x += margins.left;
      sr.origin.y += margins.bottom;

      sr = [[self delegate] windowWillUseStandardFrame:self
                                          defaultFrame:sr];
      [self setFrame: sr display: NO];
    }
#endif
}

- (void)setAppearance
{
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  struct frame *f = ((EmacsView *)[self delegate])->emacsframe;
  NSAppearance *appearance = nil;

  NSTRACE ("[EmacsWindow setAppearance]");

#ifndef NSAppKitVersionNumber10_10
#define NSAppKitVersionNumber10_10 1343
#endif

  if (NSAppKitVersionNumber < NSAppKitVersionNumber10_10)
    return;

  if (FRAME_NS_APPEARANCE (f) == ns_appearance_vibrant_dark)
    appearance =
      [NSAppearance appearanceNamed:NSAppearanceNameVibrantDark];
  else if (FRAME_NS_APPEARANCE (f) == ns_appearance_aqua)
    appearance =
      [NSAppearance appearanceNamed:NSAppearanceNameAqua];

  [self setAppearance:appearance];
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 101000 */
}

- (void)setFrame:(NSRect)windowFrame
         display:(BOOL)displayViews
{
  NSTRACE ("[EmacsWindow setFrame:" NSTRACE_FMT_RECT " display:%d]",
           NSTRACE_ARG_RECT (windowFrame), displayViews);

  [super setFrame:windowFrame display:displayViews];
}

- (void)setFrame:(NSRect)windowFrame
         display:(BOOL)displayViews
         animate:(BOOL)performAnimation
{
  NSTRACE ("[EmacsWindow setFrame:" NSTRACE_FMT_RECT
           " display:%d performAnimation:%d]",
           NSTRACE_ARG_RECT (windowFrame), displayViews, performAnimation);

  [super setFrame:windowFrame display:displayViews animate:performAnimation];
}

- (void)setFrameTopLeftPoint:(NSPoint)point
{
  NSTRACE ("[EmacsWindow setFrameTopLeftPoint:" NSTRACE_FMT_POINT "]",
           NSTRACE_ARG_POINT (point));

  [super setFrameTopLeftPoint:point];
}

- (BOOL)canBecomeKeyWindow
{
  return !FRAME_NO_ACCEPT_FOCUS (((EmacsView *)[self delegate])->emacsframe);
}
@end /* EmacsWindow */


@implementation EmacsFSWindow

- (BOOL)canBecomeKeyWindow
{
  return YES;
}

- (BOOL)canBecomeMainWindow
{
  return YES;
}

@end

/* ==========================================================================

    EmacsScroller implementation

   ========================================================================== */


@implementation EmacsScroller

/* for repeat button push */
#define SCROLL_BAR_FIRST_DELAY 0.5
#define SCROLL_BAR_CONTINUOUS_DELAY (1.0 / 15)

+ (CGFloat) scrollerWidth
{
  /* TODO: if we want to allow variable widths, this is the place to do it,
           however neither GNUstep nor Cocoa support it very well.  */
  CGFloat r;
#if defined (NS_IMPL_COCOA) \
  && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  if ([NSScroller respondsToSelector:
                    @selector(scrollerWidthForControlSize:scrollerStyle:)])
#endif
    r = [NSScroller scrollerWidthForControlSize: NSControlSizeRegular
                                  scrollerStyle: NSScrollerStyleLegacy];
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  else
#endif
#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= 1070 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070 \
  || defined (NS_IMPL_GNUSTEP)
    r = [NSScroller scrollerWidth];
#endif
  return r;
}

- (instancetype)initFrame: (NSRect )r window: (Lisp_Object)nwin
{
  NSTRACE ("[EmacsScroller initFrame: window:]");

  if (r.size.width > r.size.height)
      horizontal = YES;
  else
      horizontal = NO;

  [super initWithFrame: r/*NSMakeRect (0, 0, 0, 0)*/];
  [self setContinuous: YES];
  [self setEnabled: YES];

  /* Ensure auto resizing of scrollbars occurs within the emacs frame's view
     locked against the top and bottom edges, and right edge on macOS, where
     scrollers are on right.  */
#ifdef NS_IMPL_GNUSTEP
  [self setAutoresizingMask: NSViewMaxXMargin | NSViewHeightSizable];
#else
  [self setAutoresizingMask: NSViewMinXMargin | NSViewHeightSizable];
#endif

  window = XWINDOW (nwin);
  condemned = NO;
  if (horizontal)
    pixel_length = NSWidth (r);
  else
    pixel_length = NSHeight (r);
  if (pixel_length == 0) pixel_length = 1;
  min_portion = 20 / pixel_length;

  frame = XFRAME (window->frame);
  if (FRAME_LIVE_P (frame))
    {
      int i;
      EmacsView *view = FRAME_NS_VIEW (frame);
      NSView *sview = [[view window] contentView];
      NSArray *subs = [sview subviews];

      /* Disable optimization stopping redraw of other scrollbars.  */
      view->scrollbarsNeedingUpdate = 0;
      for (i =[subs count]-1; i >= 0; i--)
        if ([[subs objectAtIndex: i] isKindOfClass: [EmacsScroller class]])
          view->scrollbarsNeedingUpdate++;
      [sview addSubview: self];
    }

  /* [self setFrame: r]; */

  return self;
}


- (void)setFrame: (NSRect)newRect
{
  NSTRACE ("[EmacsScroller setFrame:]");

  /* block_input (); */
  if (horizontal)
    pixel_length = NSWidth (newRect);
  else
    pixel_length = NSHeight (newRect);
  if (pixel_length == 0) pixel_length = 1;
  min_portion = 20 / pixel_length;
  [super setFrame: newRect];
  /* unblock_input (); */
}


- (void)dealloc
{
  NSTRACE ("[EmacsScroller dealloc]");
  if (window)
    {
      if (horizontal)
        wset_horizontal_scroll_bar (window, Qnil);
      else
        wset_vertical_scroll_bar (window, Qnil);
    }
  window = 0;
  [super dealloc];
}


- (instancetype)condemn
{
  NSTRACE ("[EmacsScroller condemn]");
  condemned =YES;
  return self;
}


- (instancetype)reprieve
{
  NSTRACE ("[EmacsScroller reprieve]");
  condemned =NO;
  return self;
}


-(bool)judge
{
  NSTRACE ("[EmacsScroller judge]");
  bool ret = condemned;
  if (condemned)
    {
      EmacsView *view;
      block_input ();
      /* Ensure other scrollbar updates after deletion.  */
      view = (EmacsView *)FRAME_NS_VIEW (frame);
      if (view != nil)
        view->scrollbarsNeedingUpdate++;
      if (window)
        {
          if (horizontal)
            wset_horizontal_scroll_bar (window, Qnil);
          else
            wset_vertical_scroll_bar (window, Qnil);
        }
      window = 0;
      [self removeFromSuperview];
      [self release];
      unblock_input ();
    }
  return ret;
}


- (void)resetCursorRects
{
  NSRect visible = [self visibleRect];
  NSTRACE ("[EmacsScroller resetCursorRects]");

  if (!NSIsEmptyRect (visible))
    [self addCursorRect: visible cursor: [NSCursor arrowCursor]];

#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101300
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
  if ([[NSCursor arrowCursor] respondsToSelector:
                                @selector(setOnMouseEntered)])
#endif
    [[NSCursor arrowCursor] setOnMouseEntered: YES];
#endif
}


- (int) checkSamePosition: (int) position portion: (int) portion
                    whole: (int) whole
{
  return em_position ==position && em_portion ==portion && em_whole ==whole
    && portion != whole; /* Needed for resizing empty buffer.  */
}


- (instancetype)setPosition: (int)position portion: (int)portion whole: (int)whole
{
  NSTRACE ("[EmacsScroller setPosition:portion:whole:]");

  em_position = position;
  em_portion = portion;
  em_whole = whole;

  if (portion >= whole)
    {
#ifdef NS_IMPL_COCOA
      [self setKnobProportion: 1.0];
      [self setDoubleValue: 1.0];
#else
      [self setFloatValue: 0.0 knobProportion: 1.0];
#endif
    }
  else
    {
      float pos;
      CGFloat por;
      portion = max ((float)whole*min_portion/pixel_length, portion);
      pos = (float)position / (whole - portion);
      por = (CGFloat)portion/whole;
#ifdef NS_IMPL_COCOA
      [self setKnobProportion: por];
      [self setDoubleValue: pos];
#else
      [self setFloatValue: pos knobProportion: por];
#endif
    }

  return self;
}

/* Set up emacs_event.  */
- (void) sendScrollEventAtLoc: (float)loc fromEvent: (NSEvent *)e
{
  Lisp_Object win;

  NSTRACE ("[EmacsScroller sendScrollEventAtLoc:fromEvent:]");

  if (!emacs_event)
    return;

  emacs_event->part = last_hit_part;
  emacs_event->code = 0;
  emacs_event->modifiers = EV_MODIFIERS (e) | down_modifier;
  XSETWINDOW (win, window);
  emacs_event->frame_or_window = win;
  emacs_event->timestamp = EV_TIMESTAMP (e);
  emacs_event->arg = Qnil;

  if (horizontal)
    {
      emacs_event->kind = HORIZONTAL_SCROLL_BAR_CLICK_EVENT;
      XSETINT (emacs_event->x, em_whole * loc / pixel_length);
      XSETINT (emacs_event->y, em_whole);
    }
  else
    {
      emacs_event->kind = SCROLL_BAR_CLICK_EVENT;
      XSETINT (emacs_event->x, loc);
      XSETINT (emacs_event->y, pixel_length-20);
    }

  if (q_event_ptr)
    {
      n_emacs_events_pending++;
      kbd_buffer_store_event_hold (emacs_event, q_event_ptr);
    }
  else
    hold_event (emacs_event);
  EVENT_INIT (*emacs_event);
  ns_send_appdefined (-1);
}


/* Called manually through timer to implement repeated button action
   with hold-down.  */
- (instancetype)repeatScroll: (NSTimer *)scrollEntry
{
  NSEvent *e = [[self window] currentEvent];
  NSPoint p =  [[self window] mouseLocationOutsideOfEventStream];
  BOOL inKnob = [self testPart: p] == NSScrollerKnob;

  NSTRACE ("[EmacsScroller repeatScroll:]");

  /* Clear timer if need be.  */
  if (inKnob || [scroll_repeat_entry timeInterval] == SCROLL_BAR_FIRST_DELAY)
    {
        [scroll_repeat_entry invalidate];
        [scroll_repeat_entry release];
        scroll_repeat_entry = nil;

        if (inKnob)
          return self;

        scroll_repeat_entry
	  = [[NSTimer scheduledTimerWithTimeInterval:
			SCROLL_BAR_CONTINUOUS_DELAY
                                            target: self
                                          selector: @selector (repeatScroll:)
                                          userInfo: 0
                                           repeats: YES]
	      retain];
    }

  [self sendScrollEventAtLoc: 0 fromEvent: e];
  return self;
}


/* Asynchronous mouse tracking for scroller.  This allows us to dispatch
   mouseDragged events without going into a modal loop.  */
- (void)mouseDown: (NSEvent *)e
{
  NSRect sr, kr;
  /* hitPart is only updated AFTER event is passed on.  */
  NSScrollerPart part = [self testPart: [e locationInWindow]];
  CGFloat loc, kloc, pos UNINIT;
  int edge = 0;

  NSTRACE ("[EmacsScroller mouseDown:]");

  switch (part)
    {
    case NSScrollerDecrementPage:
      last_hit_part = horizontal ? scroll_bar_before_handle : scroll_bar_above_handle; break;
    case NSScrollerIncrementPage:
      last_hit_part = horizontal ? scroll_bar_after_handle : scroll_bar_below_handle; break;
#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 1070
    case NSScrollerDecrementLine:
      last_hit_part = horizontal ? scroll_bar_left_arrow : scroll_bar_up_arrow; break;
    case NSScrollerIncrementLine:
      last_hit_part = horizontal ? scroll_bar_right_arrow : scroll_bar_down_arrow; break;
#endif
    case NSScrollerKnob:
      last_hit_part = horizontal ? scroll_bar_horizontal_handle : scroll_bar_handle; break;
    case NSScrollerKnobSlot:  /* GNUstep-only */
      last_hit_part = scroll_bar_move_ratio; break;
    default:  /* NSScrollerNoPart? */
      fprintf (stderr, "EmacsScroller-mouseDown: unexpected part %ld\n",
               (long) part);
      return;
    }

  if (part == NSScrollerKnob || part == NSScrollerKnobSlot)
    {
      /* handle, or on GNUstep possibly slot */
      NSEvent *fake_event;
      int length;

      /* compute float loc in slot and mouse offset on knob */
      sr = [self convertRect: [self rectForPart: NSScrollerKnobSlot]
                      toView: nil];
      if (horizontal)
        {
          length = NSWidth (sr);
          loc = ([e locationInWindow].x - NSMinX (sr));
        }
      else
        {
          length = NSHeight (sr);
          loc = length - ([e locationInWindow].y - NSMinY (sr));
        }

      if (loc <= 0.0)
        {
          loc = 0.0;
          edge = -1;
        }
      else if (loc >= length)
        {
          loc = length;
          edge = 1;
        }

      if (edge)
        kloc = 0.5 * edge;
      else
        {
          kr = [self convertRect: [self rectForPart: NSScrollerKnob]
                          toView: nil];
          if (horizontal)
            kloc = ([e locationInWindow].x - NSMinX (kr));
          else
            kloc = NSHeight (kr) - ([e locationInWindow].y - NSMinY (kr));
        }
      last_mouse_offset = kloc;

      /* if knob, tell emacs a location offset by knob pos
         (to indicate top of handle) */
      if (part == NSScrollerKnob)
        pos = (loc - last_mouse_offset);
      else
        /* else this is a slot click on GNUstep: go straight there */
        pos = loc;

      /* If there are buttons in the scroller area, we need to
         recalculate pos as emacs expects the scroller slot to take up
         the entire available length.  */
      if (length != pixel_length)
        pos = pos * pixel_length / length;

      /* send a fake mouse-up to super to preempt modal -trackKnob: mode */
      fake_event = [NSEvent mouseEventWithType: NSEventTypeLeftMouseUp
                                      location: [e locationInWindow]
                                 modifierFlags: [e modifierFlags]
                                     timestamp: [e timestamp]
                                  windowNumber: [e windowNumber]
                                       context: nil
                                   eventNumber: [e eventNumber]
                                    clickCount: [e clickCount]
                                      pressure: [e pressure]];
      [super mouseUp: fake_event];
    }
  else
    {
      pos = 0; /* ignored */

      /* Set a timer to repeat, as we can't let superclass do this modally.  */
      scroll_repeat_entry
	= [[NSTimer scheduledTimerWithTimeInterval: SCROLL_BAR_FIRST_DELAY
                                            target: self
                                          selector: @selector (repeatScroll:)
                                          userInfo: 0
                                           repeats: YES]
	    retain];
    }

  if (part != NSScrollerKnob)
    [self sendScrollEventAtLoc: pos fromEvent: e];
}


/* Called as we manually track scroller drags, rather than superclass.  */
- (void)mouseDragged: (NSEvent *)e
{
    NSRect sr;
    double loc, pos;
    int length;

    NSTRACE ("[EmacsScroller mouseDragged:]");

      sr = [self convertRect: [self rectForPart: NSScrollerKnobSlot]
                      toView: nil];

      if (horizontal)
        {
          length = NSWidth (sr);
          loc = ([e locationInWindow].x - NSMinX (sr));
        }
      else
        {
          length = NSHeight (sr);
          loc = length - ([e locationInWindow].y - NSMinY (sr));
        }

      if (loc <= 0.0)
        {
          loc = 0.0;
        }
      else if (loc >= length + last_mouse_offset)
        {
          loc = length + last_mouse_offset;
        }

      pos = (loc - last_mouse_offset);

      /* If there are buttons in the scroller area, we need to
         recalculate pos as emacs expects the scroller slot to take up
         the entire available length.  */
      if (length != pixel_length)
        pos = pos * pixel_length / length;

      [self sendScrollEventAtLoc: pos fromEvent: e];
}


- (void)mouseUp: (NSEvent *)e
{
  NSTRACE ("[EmacsScroller mouseUp:]");

  if (scroll_repeat_entry)
    {
      [scroll_repeat_entry invalidate];
      [scroll_repeat_entry release];
      scroll_repeat_entry = nil;
    }
  last_hit_part = scroll_bar_above_handle;
}


/* Treat scrollwheel events in the bar as though they were in the main window.  */
- (void) scrollWheel: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsScroller scrollWheel:]");

  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (frame);
  [view mouseDown: theEvent];
}

@end  /* EmacsScroller */


#ifdef NS_DRAW_TO_BUFFER

/* ==========================================================================

   A class to handle the screen buffer.

   ========================================================================== */

@implementation EmacsSurface


/* An IOSurface is a pixel buffer that is efficiently copied to VRAM
   for display.  In order to use an IOSurface we must first lock it,
   write to it, then unlock it.  At this point it is transferred to
   VRAM and if we modify it during this transfer we may see corruption
   of the output.  To avoid this problem we can check if the surface
   is "in use", and if it is then avoid using it.  Unfortunately to
   avoid writing to a surface that's in use, but still maintain the
   ability to draw to the screen at any time, we need to keep a cache
   of multiple surfaces that we can use at will.

   The EmacsSurface class maintains this cache of surfaces, and
   handles the conversion to a CGGraphicsContext that AppKit can use
   to draw on.

   The cache is simple: if a free surface is found it is removed from
   the cache and set as the "current" surface.  Once Emacs is done
   with drawing to the current surface, the previous surface that was
   drawn to is added to the cache for reuse, and the current one is
   set as the last surface.  If no free surfaces are found in the
   cache then a new one is created.

   When AppKit wants to update the screen, we provide it with the last
   surface, as that has the most recent data.

   FIXME: It is possible for the cache to grow if Emacs draws faster
   than the surfaces can be drawn to the screen, so there should
   probably be some sort of pruning job that removes excess
   surfaces.  */


- (id) initWithSize: (NSSize)s
         ColorSpace: (CGColorSpaceRef)cs
{
  NSTRACE ("[EmacsSurface initWithSize:ColorSpace:]");

  [super init];

  cache = [[NSMutableArray arrayWithCapacity:3] retain];
  size = s;
  colorSpace = cs;

  return self;
}


- (void) dealloc
{
  if (context)
    CGContextRelease (context);

  if (currentSurface)
    CFRelease (currentSurface);
  if (lastSurface)
    CFRelease (lastSurface);

  for (id object in cache)
    CFRelease ((IOSurfaceRef)object);

  [cache release];

  [super dealloc];
}


/* Return the size values our cached data is using.  */
- (NSSize) getSize
{
  return size;
}


/* Return a CGContextRef that can be used for drawing to the screen.
   This must ALWAYS be paired with a call to releaseContext, and the
   calls cannot be nested.  */
- (CGContextRef) getContext
{
  IOSurfaceRef surface = NULL;

  NSTRACE ("[EmacsSurface getContextWithSize:]");
  NSTRACE_MSG (@"IOSurface count: %lu", [cache count] + (lastSurface ? 1 : 0));

  for (id object in cache)
    {
      if (!IOSurfaceIsInUse ((IOSurfaceRef)object))
      {
        surface = (IOSurfaceRef)object;
        [cache removeObject:object];
        break;
      }
    }

  if (!surface)
    {
      int bytesPerRow = IOSurfaceAlignProperty (kIOSurfaceBytesPerRow,
                                                size.width * 4);

      surface = IOSurfaceCreate
        ((CFDictionaryRef)@{(id)kIOSurfaceWidth:[NSNumber numberWithInt:size.width],
            (id)kIOSurfaceHeight:[NSNumber numberWithInt:size.height],
            (id)kIOSurfaceBytesPerRow:[NSNumber numberWithInt:bytesPerRow],
            (id)kIOSurfaceBytesPerElement:[NSNumber numberWithInt:4],
            (id)kIOSurfacePixelFormat:[NSNumber numberWithUnsignedInt:'BGRA']});
    }

  IOReturn lockStatus = IOSurfaceLock (surface, 0, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to lock surface: %x", lockStatus);

  [self copyContentsTo:surface];

  currentSurface = surface;

  context = CGBitmapContextCreate (IOSurfaceGetBaseAddress (currentSurface),
                                   IOSurfaceGetWidth (currentSurface),
                                   IOSurfaceGetHeight (currentSurface),
                                   8,
                                   IOSurfaceGetBytesPerRow (currentSurface),
                                   colorSpace,
                                   (kCGImageAlphaPremultipliedFirst
                                    | kCGBitmapByteOrder32Host));
  return context;
}


/* Releases the CGGraphicsContext and unlocks the associated
   IOSurface, so it will be sent to VRAM.  */
- (void) releaseContext
{
  NSTRACE ("[EmacsSurface releaseContextAndGetSurface]");

  CGContextRelease (context);
  context = NULL;

  IOReturn lockStatus = IOSurfaceUnlock (currentSurface, 0, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to unlock surface: %x", lockStatus);

  /* Put lastSurface back on the end of the cache.  It may not have
     been displayed on the screen yet, but we probably want the new
     data and not some stale data anyway.  */
  if (lastSurface)
    [cache addObject:(id)lastSurface];
  lastSurface = currentSurface;
  currentSurface = NULL;
}


/* Get the IOSurface that we want to draw to the screen.  */
- (IOSurfaceRef) getSurface
{
  /* lastSurface always contains the most up-to-date and complete data.  */
  return lastSurface;
}


/* Copy the contents of lastSurface to DESTINATION.  This is required
   every time we want to use an IOSurface as its contents are probably
   blanks (if it's new), or stale.  */
- (void) copyContentsTo: (IOSurfaceRef) destination
{
  IOReturn lockStatus;
  void *sourceData, *destinationData;
  int numBytes = IOSurfaceGetAllocSize (destination);

  NSTRACE ("[EmacsSurface copyContentsTo:]");

  if (! lastSurface)
    return;

  lockStatus = IOSurfaceLock (lastSurface, kIOSurfaceLockReadOnly, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to lock source surface: %x", lockStatus);

  sourceData = IOSurfaceGetBaseAddress (lastSurface);
  destinationData = IOSurfaceGetBaseAddress (destination);

  /* Since every IOSurface should have the exact same settings, a
     memcpy seems like the fastest way to copy the data from one to
     the other.  */
  memcpy (destinationData, sourceData, numBytes);

  lockStatus = IOSurfaceUnlock (lastSurface, kIOSurfaceLockReadOnly, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to unlock source surface: %x", lockStatus);
}


@end /* EmacsSurface */


#endif


#ifdef NS_IMPL_GNUSTEP
/* Dummy class to get rid of startup warnings.  */
@implementation EmacsDocument

@end
#endif


/* ==========================================================================

   Font-related functions; these used to be in nsfaces.m

   ========================================================================== */


static Lisp_Object
ns_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  /* --------------------------------------------------------------------------
     External (hook)
     -------------------------------------------------------------------------- */
  struct font *font = XFONT_OBJECT (font_object);
  EmacsView *view = FRAME_NS_VIEW (f);
  int font_ascent, font_descent;

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

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid - 1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Compute the scroll bar height in character lines.  */
  if (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0)
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f)
	= (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) + height - 1) / height;
    }
  else
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f) = (14 + height - 1) / height;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_NS_WINDOW (f) != 0 && ! [view isFullscreen])
    adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 3,
		       false, Qfont);

  return font_object;
}


/* XLFD: -foundry-family-weight-slant-swidth-adstyle-pxlsz-ptSz-resx-resy-spc-avgWidth-rgstry-encoding */
/* Note: ns_font_to_xlfd and ns_fontname_to_xlfd no longer needed, removed
         in 1.43.  */

const char *
ns_xlfd_to_fontname (const char *xlfd)
/* --------------------------------------------------------------------------
    Convert an X font name (XLFD) to an NS font name.
    Only family is used.
    The string returned is temporarily allocated.
   -------------------------------------------------------------------------- */
{
  char *name = xmalloc (180);
  int i, len;
  const char *ret;

  if (!strncmp (xlfd, "--", 2))
    sscanf (xlfd, "--%*[^-]-%179[^-]-", name);
  else
    sscanf (xlfd, "-%*[^-]-%179[^-]-", name);

  /* stopgap for malformed XLFD input */
  if (!*name)
    strcpy (name, "Monaco");

  /* undo hack in ns_fontname_to_xlfd, converting '$' to '-', '_' to ' '
     also uppercase after '-' or ' ' */
  name[0] = c_toupper (name[0]);
  for (len =strlen (name), i =0; i<len; i++)
    {
      if (name[i] == '$')
        {
          name[i] = '-';
          if (i+1<len)
            name[i+1] = c_toupper (name[i+1]);
        }
      else if (name[i] == '_')
        {
          name[i] = ' ';
          if (i+1<len)
            name[i+1] = c_toupper (name[i+1]);
        }
    }
  /* fprintf (stderr, "converted '%s' to '%s'\n",xlfd,name); */
  ret = [[NSString stringWithUTF8String: name] UTF8String];
  xfree (name);
  return ret;
}


void
syms_of_nsterm (void)
{
  NSTRACE ("syms_of_nsterm");

  ns_antialias_threshold = 10.0;
  PDUMPER_REMEMBER_SCALAR (ns_antialias_threshold);

  /* From 23+ we need to tell emacs what modifiers there are.  */
  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qcontrol, "control");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");

  DEFSYM (Qfile, "file");
  DEFSYM (Qurl, "url");

  DEFSYM (Qns_drag_operation_copy, "ns-drag-operation-copy");
  DEFSYM (Qns_drag_operation_link, "ns-drag-operation-link");
  DEFSYM (Qns_drag_operation_generic, "ns-drag-operation-generic");

  Fput (Qalt, Qmodifier_value, make_fixnum (alt_modifier));
  Fput (Qhyper, Qmodifier_value, make_fixnum (hyper_modifier));
  Fput (Qmeta, Qmodifier_value, make_fixnum (meta_modifier));
  Fput (Qsuper, Qmodifier_value, make_fixnum (super_modifier));
  Fput (Qcontrol, Qmodifier_value, make_fixnum (ctrl_modifier));

  DEFVAR_LISP ("ns-input-file", ns_input_file,
              "The file specified in the last NS event.");
  ns_input_file =Qnil;

  DEFVAR_LISP ("ns-working-text", ns_working_text,
              "String for visualizing working composition sequence.");
  ns_working_text =Qnil;

  DEFVAR_LISP ("ns-input-font", ns_input_font,
              "The font specified in the last NS event.");
  ns_input_font =Qnil;

  DEFVAR_LISP ("ns-input-fontsize", ns_input_fontsize,
              "The fontsize specified in the last NS event.");
  ns_input_fontsize =Qnil;

  DEFVAR_LISP ("ns-input-line", ns_input_line,
               "The line specified in the last NS event.");
  ns_input_line =Qnil;

  DEFVAR_LISP ("ns-input-spi-name", ns_input_spi_name,
               "The service name specified in the last NS event.");
  ns_input_spi_name =Qnil;

  DEFVAR_LISP ("ns-input-spi-arg", ns_input_spi_arg,
               "The service argument specified in the last NS event.");
  ns_input_spi_arg =Qnil;

  DEFVAR_LISP ("ns-alternate-modifier", ns_alternate_modifier,
               "This variable describes the behavior of the alternate or option key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_alternate_modifier = Qmeta;

  DEFVAR_LISP ("ns-right-alternate-modifier", ns_right_alternate_modifier,
               "This variable describes the behavior of the right alternate or option key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
It can also be `left' to use the value of `ns-alternate-modifier' instead.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_right_alternate_modifier = Qleft;

  DEFVAR_LISP ("ns-command-modifier", ns_command_modifier,
               "This variable describes the behavior of the command key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_command_modifier = Qsuper;

  DEFVAR_LISP ("ns-right-command-modifier", ns_right_command_modifier,
               "This variable describes the behavior of the right command key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
It can also be `left' to use the value of `ns-command-modifier' instead.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_right_command_modifier = Qleft;

  DEFVAR_LISP ("ns-control-modifier", ns_control_modifier,
               "This variable describes the behavior of the control key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_control_modifier = Qcontrol;

  DEFVAR_LISP ("ns-right-control-modifier", ns_right_control_modifier,
               "This variable describes the behavior of the right control key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
It can also be `left' to use the value of `ns-control-modifier' instead.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_right_control_modifier = Qleft;

  DEFVAR_LISP ("ns-function-modifier", ns_function_modifier,
               "This variable describes the behavior of the function (fn) key.\n\
Either SYMBOL, describing the behavior for any event,\n\
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior\n\
separately for ordinary keys, function keys, and mouse events.\n\
\n\
Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.\n\
If `none', the key is ignored by Emacs and retains its standard meaning.");
  ns_function_modifier = Qnone;

  DEFVAR_LISP ("ns-antialias-text", ns_antialias_text,
               "Non-nil (the default) means to render text antialiased.");
  ns_antialias_text = Qt;

  DEFVAR_LISP ("ns-use-thin-smoothing", ns_use_thin_smoothing,
               "Non-nil turns on a font smoothing method that produces thinner strokes.");
  ns_use_thin_smoothing = Qnil;

  DEFVAR_LISP ("ns-confirm-quit", ns_confirm_quit,
               "Whether to confirm application quit using dialog.");
  ns_confirm_quit = Qnil;

  DEFVAR_LISP ("ns-auto-hide-menu-bar", ns_auto_hide_menu_bar,
               doc: /* Non-nil means that the menu bar is hidden, but appears when the mouse is near.
Only works on Mac OS X.  */);
  ns_auto_hide_menu_bar = Qnil;

  DEFVAR_BOOL ("ns-use-native-fullscreen", ns_use_native_fullscreen,
     doc: /* Non-nil means to use native fullscreen on Mac OS X 10.7 and later.
Nil means use fullscreen the old (< 10.7) way.  The old way works better with
multiple monitors, but lacks tool bar.  This variable is ignored on
Mac OS X < 10.7.  Default is t.  */);
  ns_use_native_fullscreen = YES;
  ns_last_use_native_fullscreen = ns_use_native_fullscreen;

  DEFVAR_BOOL ("ns-use-fullscreen-animation", ns_use_fullscreen_animation,
     doc: /* Non-nil means use animation on non-native fullscreen.
For native fullscreen, this does nothing.
Default is nil.  */);
  ns_use_fullscreen_animation = NO;

  DEFVAR_BOOL ("ns-use-srgb-colorspace", ns_use_srgb_colorspace,
     doc: /* Non-nil means to use sRGB colorspace on Mac OS X 10.7 and later.
Note that this does not apply to images.
This variable is ignored on Mac OS X < 10.7 and GNUstep.  */);
  ns_use_srgb_colorspace = YES;

  DEFVAR_BOOL ("ns-use-mwheel-acceleration",
               ns_use_mwheel_acceleration,
     doc: /* Non-nil means use macOS's standard mouse wheel acceleration.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is t.  */);
  ns_use_mwheel_acceleration = YES;

  DEFVAR_LISP ("ns-mwheel-line-height", ns_mwheel_line_height,
               doc: /* The number of pixels touchpad scrolling considers one line.
Nil or a non-number means use the default frame line height.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is nil.  */);
  ns_mwheel_line_height = Qnil;

  DEFVAR_BOOL ("ns-use-mwheel-momentum", ns_use_mwheel_momentum,
               doc: /* Non-nil means mouse wheel scrolling uses momentum.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is t.  */);
  ns_use_mwheel_momentum = YES;

  /* TODO: Move to common code.  */
  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_toolkit_scroll_bars = Qt;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_use_underline_position_properties = 0;
  DEFSYM (Qx_use_underline_position_properties,
	  "x-use-underline-position-properties");

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_underline_at_descent_line = 0;
  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");

  /* Tell Emacs about this window system.  */
  Fprovide (Qns, Qnil);

  DEFSYM (Qcocoa, "cocoa");
  DEFSYM (Qgnustep, "gnustep");
  DEFSYM (QCordinary, ":ordinary");
  DEFSYM (QCfunction, ":function");
  DEFSYM (QCmouse, ":mouse");

#ifdef NS_IMPL_COCOA
  Fprovide (Qcocoa, Qnil);
  syms_of_macfont ();
#else
  Fprovide (Qgnustep, Qnil);
  syms_of_nsfont ();
#endif

}
