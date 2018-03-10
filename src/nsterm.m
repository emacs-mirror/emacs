/* NeXT/Open/GNUstep / macOS communication module.      -*- coding: utf-8 -*-

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2018 Free Software
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
   interpretation of even the system includes. */
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

#ifdef NS_IMPL_GNUSTEP
#include "process.h"
#endif

#ifdef NS_IMPL_COCOA
#include "macfont.h"
#include <Carbon/Carbon.h>
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
   parallel threads. */
volatile int nstrace_num = 0;
volatile int nstrace_depth = 0;

/* When 0, no trace is emitted.  This is used by NSTRACE_WHEN and
   NSTRACE_UNLESS to silence functions called.

   TODO: This should really be a thread-local variable, to avoid that
   a function with disabled trace thread silence trace output in
   another.  However, in practice this seldom is a problem. */
volatile int nstrace_enabled_global = 1;

/* Called when nstrace_enabled goes out of scope. */
void nstrace_leave(int * pointer_to_nstrace_enabled)
{
  if (*pointer_to_nstrace_enabled)
    {
      --nstrace_depth;
    }
}


/* Called when nstrace_saved_enabled_global goes out of scope. */
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
#if defined (NS_IMPL_COCOA) \
  && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  if (ns_use_srgb_colorspace
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      && [NSColor respondsToSelector:
                    @selector(colorWithSRGBRed:green:blue:alpha:)]
#endif
      )
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
  /* FIXMES: We're checking for colorWithSRGBRed here so this will
     only work in the same place as in the method above.  It should
     really be a check whether we're on macOS 10.7 or above. */
#if defined (NS_IMPL_COCOA) \
  && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  if (ns_use_srgb_colorspace
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      && [NSColor respondsToSelector:
                    @selector(colorWithSRGBRed:green:blue:alpha:)]
#endif
      )
    return [self colorUsingColorSpace: [NSColorSpace sRGBColorSpace]];
#endif
  return [self colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
}

@end

/* ==========================================================================

    Local declarations

   ========================================================================== */

/* Convert a symbol indexed with an NSxxx value to a value as defined
   in keyboard.c (lispy_function_key). I hope this is a correct way
   of doing things... */
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

  NSBackspaceCharacter,         0x08,  /* 8: Not on some KBs. */
  NSDeleteCharacter,            0xFF,  /* 127: Big 'delete' key upper right. */
  NSDeleteFunctionKey,          0x9F,  /* 63272: Del forw key off main array. */

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
   no way to control this behavior. */
float ns_antialias_threshold;

NSArray *ns_send_types = 0, *ns_return_types = 0;
static NSArray *ns_drag_types = 0;
NSString *ns_app_name = @"Emacs";  /* default changed later */

/* Display variables */
struct ns_display_info *x_display_list; /* Chain of existing displays */
long context_menu_value = 0;

/* display update */
static struct frame *ns_updating_frame;
static NSView *focus_view = NULL;
static int ns_window_num = 0;
#ifdef NS_IMPL_GNUSTEP
static NSRect uRect;            // TODO: This is dead, remove it?
#endif
static BOOL gsaved = NO;
static BOOL ns_fake_keydown = NO;
#ifdef NS_IMPL_COCOA
static BOOL ns_menu_bar_is_hidden = NO;
#endif
/*static int debug_lock = 0; */

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

static NSString *represented_filename = nil;
static struct frame *represented_frame = 0;

#ifdef NS_IMPL_COCOA
/*
 * State for pending menu activation:
 * MENU_NONE     Normal state
 * MENU_PENDING  A menu has been clicked on, but has been canceled so we can
 *               run lisp to update the menu.
 * MENU_OPENING  Menu is up to date, and the click event is redone so the menu
 *               will open.
 */
#define MENU_NONE 0
#define MENU_PENDING 1
#define MENU_OPENING 2
static int menu_will_open_state = MENU_NONE;

/* Saved position for menu click.  */
static CGPoint menu_mouse_point;
#endif

/* Convert modifiers in a NeXTstep event to emacs style modifiers.  */
#define NS_FUNCTION_KEY_MASK 0x800000
#define NSLeftControlKeyMask    (0x000001 | NSEventModifierFlagControl)
#define NSRightControlKeyMask   (0x002000 | NSEventModifierFlagControl)
#define NSLeftCommandKeyMask    (0x000008 | NSEventModifierFlagCommand)
#define NSRightCommandKeyMask   (0x000010 | NSEventModifierFlagCommand)
#define NSLeftAlternateKeyMask  (0x000020 | NSEventModifierFlagOption)
#define NSRightAlternateKeyMask (0x000040 | NSEventModifierFlagOption)

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

#define EV_MODIFIERS2(flags)                                            \
  (((flags & NSEventModifierFlagHelp) ?                                 \
    hyper_modifier : 0)                                                 \
   | ((flags & NSEventModifierFlagShift) ?                              \
      shift_modifier : 0)                                               \
   | ((flags & NS_FUNCTION_KEY_MASK) ?                                  \
      parse_solitary_modifier (ns_function_modifier) : 0)               \
   | ev_modifiers_helper (flags, NSLeftControlKeyMask,                  \
                          NSRightControlKeyMask,                        \
                          NSEventModifierFlagControl,                   \
                          ns_control_modifier,                          \
                          ns_right_control_modifier)                    \
   | ev_modifiers_helper (flags, NSLeftCommandKeyMask,                  \
                          NSRightCommandKeyMask,                        \
                          NSEventModifierFlagCommand,                   \
                          ns_command_modifier,                          \
                          ns_right_command_modifier)                    \
   | ev_modifiers_helper (flags, NSLeftAlternateKeyMask,                \
                          NSRightAlternateKeyMask,                      \
                          NSEventModifierFlagOption,                    \
                          ns_alternate_modifier,                        \
                          ns_right_alternate_modifier))

#define EV_MODIFIERS(e) EV_MODIFIERS2 ([e modifierFlags])

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

/* Convert the time field to a timestamp in milliseconds. */
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
   property depending on what we're doing. */
#define FRAME_DECORATED_FLAGS (NSWindowStyleMaskTitled              \
                               | NSWindowStyleMaskResizable         \
                               | NSWindowStyleMaskMiniaturizable    \
                               | NSWindowStyleMaskClosable)
#define FRAME_UNDECORATED_FLAGS NSWindowStyleMaskBorderless

/* TODO: get rid of need for these forward declarations */
static void ns_condemn_scroll_bars (struct frame *f);
static void ns_judge_scroll_bars (struct frame *f);


/* ==========================================================================

    Utilities

   ========================================================================== */

void
ns_set_represented_filename (struct frame *f)
{
  Lisp_Object filename, encoded_filename;
  Lisp_Object buf = XWINDOW (f->selected_window)->contents;
  NSAutoreleasePool *pool;
  NSString *fstr;

  NSTRACE ("ns_set_represented_filename");

  if (f->explicit_name || ! NILP (f->title))
    return;

  block_input ();
  pool = [[NSAutoreleasePool alloc] init];
  filename = BVAR (XBUFFER (buf), filename);

  if (! NILP (filename))
    {
      encoded_filename = ENCODE_UTF_8 (filename);

      fstr = [NSString stringWithUTF8String: SSDATA (encoded_filename)];
      if (fstr == nil) fstr = @"";
    }
  else
    fstr = @"";

  represented_filename = [fstr retain];
  represented_frame = f;

  [pool release];
  unblock_input ();
}

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
  return CALLN (Fnconc, list, list1 (item));
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
   from the GUI. Get the locale from the OS and set LANG. */
{
  NSLocale *locale = [NSLocale currentLocale];

  NSTRACE ("ns_init_locale");

  @try
    {
      /* It seems macOS should probably use UTF-8 everywhere.
         'localeIdentifier' does not specify the encoding, and I can't
         find any way to get the OS to tell us which encoding to use,
         so hard-code '.UTF-8'. */
      NSString *localeID = [NSString stringWithFormat:@"%@.UTF-8",
                                     [locale localeIdentifier]];

      /* Set LANG to locale, but not if LANG is already set. */
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
reserved for an hidden dock.  */
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
   external (RIF) call; whole frame, called before update_window_begin
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
  [view lockFocus];

  /* drawRect may have been called for say the minibuffer, and then clip path
     is for the minibuffer.  But the display engine may draw more because
     we have set the frame as garbaged.  So reset clip path to the whole
     view.  */
#ifdef NS_IMPL_COCOA
  {
    NSBezierPath *bp;
    NSRect r = [view frame];
    NSRect cr = [[view window] frame];
    /* If a large frame size is set, r may be larger than the window frame
       before constrained.  In that case don't change the clip path, as we
       will clear in to the tool bar and title bar.  */
    if (r.size.height
        + FRAME_NS_TITLEBAR_HEIGHT (f)
        + FRAME_TOOLBAR_HEIGHT (f) <= cr.size.height)
      {
        bp = [[NSBezierPath bezierPathWithRect: r] retain];
        [bp setClip];
        [bp release];
      }
  }
#endif

#ifdef NS_IMPL_GNUSTEP
  uRect = NSMakeRect (0, 0, 0, 0);
#endif
}


static void
ns_update_window_begin (struct window *w)
/* --------------------------------------------------------------------------
   Prepare for a grouped sequence of drawing calls
   external (RIF) call; for one window, called after update_begin
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_window_begin");
  w->output_cursor = w->cursor;

  block_input ();

  if (f == hlinfo->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      hlinfo->mouse_face_defer = 1;

        /* If the frame needs to be redrawn,
           simply forget about any prior mouse highlighting.  */
      if (FRAME_GARBAGED_P (f))
        hlinfo->mouse_face_window = Qnil;

      /* (further code for mouse faces ifdef'd out in other terms elided) */
    }

  unblock_input ();
}


static void
ns_update_window_end (struct window *w, bool cursor_on_p,
                      bool mouse_face_overwritten_p)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for one window called before update_end
   -------------------------------------------------------------------------- */
{
  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_window_end");

  /* note: this fn is nearly identical in all terms */
  if (!w->pseudo_window_p)
    {
      block_input ();

      if (cursor_on_p)
	display_and_set_cursor (w, 1,
				w->output_cursor.hpos, w->output_cursor.vpos,
				w->output_cursor.x, w->output_cursor.y);

      if (draw_window_fringes (w, 1))
	{
	  if (WINDOW_RIGHT_DIVIDER_WIDTH (w))
	    x_draw_right_divider (w);
	  else
	    x_draw_vertical_border (w);
	}

      unblock_input ();
    }

  /* If a row with mouse-face was overwritten, arrange for
     frame_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    reset_mouse_highlight (MOUSE_HL_INFO (XFRAME (w->frame)));
}


static void
ns_update_end (struct frame *f)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for whole frame, called after update_window_end
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_end");

/*   if (f == MOUSE_HL_INFO (f)->mouse_face_mouse_frame) */
  MOUSE_HL_INFO (f)->mouse_face_defer = 0;

  block_input ();

  [view unlockFocus];
  [[view window] flushWindow];

  unblock_input ();
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
  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "ns_focus");
  if (r != NULL)
    {
      NSTRACE_RECT ("r", *r);
    }

  if (f != ns_updating_frame)
    {
      NSView *view = FRAME_NS_VIEW (f);
      if (view != focus_view)
        {
          if (focus_view != NULL)
            {
              [focus_view unlockFocus];
              [[focus_view window] flushWindow];
/*debug_lock--; */
            }

          if (view)
            [view lockFocus];
          focus_view = view;
/*if (view) debug_lock++; */
        }
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

  if (f != ns_updating_frame)
    {
      if (focus_view != NULL)
        {
          [focus_view unlockFocus];
          [[focus_view window] flushWindow];
          focus_view = NULL;
/*debug_lock--; */
        }
    }
}


static void
ns_clip_to_row (struct window *w, struct glyph_row *row,
		enum glyph_row_area area, BOOL gc)
/* --------------------------------------------------------------------------
     Internal (but parallels other terms): Focus drawing on given row
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  NSRect clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.origin.x = window_x;
  clip_rect.origin.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.origin.y = max (clip_rect.origin.y, window_y);
  clip_rect.size.width = window_width;
  clip_rect.size.height = row->visible_height;

  ns_focus (f, &clip_rect, 1);
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
  // Number of currently active bell:s.
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


/* ==========================================================================

    Frame / window manager related functions

   ========================================================================== */


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


static void
ns_frame_rehighlight (struct frame *frame)
/* --------------------------------------------------------------------------
     External (hook): called on things like window switching within frame
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  NSTRACE ("ns_frame_rehighlight");
  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= (FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
           ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
           : dpyinfo->x_focus_frame);
      if (!FRAME_LIVE_P (dpyinfo->x_highlight_frame))
        {
          fset_focus_frame (dpyinfo->x_focus_frame, Qnil);
          dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame;
        }
    }
  else
      dpyinfo->x_highlight_frame = 0;

  if (dpyinfo->x_highlight_frame &&
         dpyinfo->x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	{
          x_update_cursor (old_highlight, 1);
	  x_set_frame_alpha (old_highlight);
	}
      if (dpyinfo->x_highlight_frame)
	{
          x_update_cursor (dpyinfo->x_highlight_frame, 1);
          x_set_frame_alpha (dpyinfo->x_highlight_frame);
	}
    }
}


void
x_make_frame_visible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Show the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSTRACE ("x_make_frame_visible");
  /* XXX: at some points in past this was not needed, as the only place that
     called this (frame.c:Fraise_frame ()) also called raise_lower;
     if this ends up the case again, comment this out again. */
  if (!FRAME_VISIBLE_P (f))
    {
      EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
      NSWindow *window = [view window];

      SET_FRAME_VISIBLE (f, 1);
      ns_raise_frame (f, ! FRAME_NO_FOCUS_ON_MAP (f));

      /* Making a new frame from a fullscreen frame will make the new frame
         fullscreen also.  So skip handleFS as this will print an error.  */
      if ([view fsIsNative] && f->want_fullscreen == FULLSCREEN_BOTH
          && [view isFullscreen])
        return;

      if (f->want_fullscreen != FULLSCREEN_NONE)
        {
          block_input ();
          [view handleFS];
          unblock_input ();
        }

      /* Making a frame invisible seems to break the parent->child
         relationship, so reinstate it. */
      if ([window parentWindow] == nil && FRAME_PARENT_FRAME (f) != NULL)
        {
          NSWindow *parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];

          block_input ();
          [parent addChildWindow: window
                         ordered: NSWindowAbove];
          unblock_input ();

          /* If the parent frame moved while the child frame was
             invisible, the child frame's position won't have been
             updated.  Make sure it's in the right place now. */
          x_set_offset(f, f->left_pos, f->top_pos, 0);
        }
    }
}


void
x_make_frame_invisible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Hide the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSView *view;
  NSTRACE ("x_make_frame_invisible");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  [[view window] orderOut: NSApp];
  SET_FRAME_VISIBLE (f, 0);
  SET_FRAME_ICONIFIED (f, 0);
}


void
x_iconify_frame (struct frame *f)
/* --------------------------------------------------------------------------
     External: Iconify window
   -------------------------------------------------------------------------- */
{
  NSView *view;
  struct ns_display_info *dpyinfo;

  NSTRACE ("x_iconify_frame");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (dpyinfo->x_highlight_frame == f)
    dpyinfo->x_highlight_frame = 0;

  if ([[view window] windowNumber] <= 0)
    {
      /* the window is still deferred.  Make it very small, bring it
         on screen and order it out. */
      NSRect s = { { 100, 100}, {0, 0} };
      NSRect t;
      t = [[view window] frame];
      [[view window] setFrame: s display: NO];
      [[view window] orderBack: NSApp];
      [[view window] orderOut: NSApp];
      [[view window] setFrame: t display: NO];
    }

  /* Processing input while Emacs is being minimized can cause a
     crash, so block it for the duration. */
  block_input();
  [[view window] miniaturize: NSApp];
  unblock_input();
}

/* Free X resources of frame F.  */

void
x_free_frame_resources (struct frame *f)
{
  NSView *view;
  struct ns_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;

  NSTRACE ("x_free_frame_resources");
  check_window_system (f);
  view = FRAME_NS_VIEW (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);
  hlinfo = MOUSE_HL_INFO (f);

  [(EmacsView *)view setWindowClosing: YES]; /* may not have been informed */

  block_input ();

  free_frame_menubar (f);
  free_frame_faces (f);

  if (f == dpyinfo->x_focus_frame)
    dpyinfo->x_focus_frame = 0;
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  if (f->output_data.ns->miniimage != nil)
    [f->output_data.ns->miniimage release];

  [[view window] close];
  [view release];

  xfree (f->output_data.ns);

  unblock_input ();
}

void
x_destroy_window (struct frame *f)
/* --------------------------------------------------------------------------
     External: Delete the window
   -------------------------------------------------------------------------- */
{
  NSTRACE ("x_destroy_window");

  /* If this frame has a parent window, detach it as not doing so can
     cause a crash in GNUStep. */
  if (FRAME_PARENT_FRAME (f) != NULL)
    {
      NSWindow *child = [FRAME_NS_VIEW (f) window];
      NSWindow *parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];

      [parent removeChildWindow: child];
    }

  check_window_system (f);
  x_free_frame_resources (f);
  ns_window_num--;
}


void
x_set_offset (struct frame *f, int xoff, int yoff, int change_grav)
/* --------------------------------------------------------------------------
     External: Position the window
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSScreen *screen = [[view window] screen];

  NSTRACE ("x_set_offset");

  block_input ();

  f->left_pos = xoff;
  f->top_pos = yoff;

  if (view != nil)
    {
      if (FRAME_PARENT_FRAME (f) == NULL && screen)
        {
          f->left_pos = f->size_hint_flags & XNegative
            ? [screen visibleFrame].size.width + f->left_pos - FRAME_PIXEL_WIDTH (f)
            : f->left_pos;
          /* We use visibleFrame here to take menu bar into account.
             Ideally we should also adjust left/top with visibleFrame.origin.  */

          f->top_pos = f->size_hint_flags & YNegative
            ? ([screen visibleFrame].size.height + f->top_pos
               - FRAME_PIXEL_HEIGHT (f) - FRAME_NS_TITLEBAR_HEIGHT (f)
               - FRAME_TOOLBAR_HEIGHT (f))
            : f->top_pos;
#ifdef NS_IMPL_GNUSTEP
	  if (f->left_pos < 100)
	    f->left_pos = 100;  /* don't overlap menu */
#endif
        }
      else if (FRAME_PARENT_FRAME (f) != NULL)
        {
          struct frame *parent = FRAME_PARENT_FRAME (f);

          /* On X negative values for child frames always result in
             positioning relative to the bottom right corner of the
             parent frame.  */
          if (f->left_pos < 0)
            f->left_pos = FRAME_PIXEL_WIDTH (parent) - FRAME_PIXEL_WIDTH (f) + f->left_pos;

          if (f->top_pos < 0)
            f->top_pos = FRAME_PIXEL_HEIGHT (parent) + FRAME_TOOLBAR_HEIGHT (parent)
              - FRAME_PIXEL_HEIGHT (f) + f->top_pos;
        }

      /* Constrain the setFrameTopLeftPoint so we don't move behind the
         menu bar.  */
      NSPoint pt = NSMakePoint (SCREENMAXBOUND (f->left_pos
                                                + NS_PARENT_WINDOW_LEFT_POS (f)),
                                SCREENMAXBOUND (NS_PARENT_WINDOW_TOP_POS (f)
                                                - f->top_pos));
      NSTRACE_POINT ("setFrameTopLeftPoint", pt);
      [[view window] setFrameTopLeftPoint: pt];
      f->size_hint_flags &= ~(XNegative|YNegative);
    }

  unblock_input ();
}


void
x_set_window_size (struct frame *f,
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

  NSTRACE ("x_set_window_size");

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
    list5 (Fcons (make_number (pixelwidth), make_number (pixelheight)),
	   Fcons (make_number (wr.size.width), make_number (wr.size.height)),
	   make_number (f->border_width),
	   make_number (FRAME_NS_TITLEBAR_HEIGHT (f)),
	   make_number (FRAME_TOOLBAR_HEIGHT (f))));

  [window setFrame: wr display: YES];

  [view updateFrameSize: NO];
  unblock_input ();
}

#ifdef NS_IMPL_COCOA
void
x_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
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

  NSTRACE ("x_set_undecorated");

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
          /* Do I need to release the toolbar here? */

          FRAME_UNDECORATED (f) = true;
          [window setStyleMask: ((window.styleMask | FRAME_UNDECORATED_FLAGS)
                                 ^ FRAME_DECORATED_FLAGS)];
        }

      /* At this point it seems we don't have an active NSResponder,
         so some key presses (TAB) are swallowed by the system. */
      [window makeFirstResponder: view];

      [view updateFrameSize: NO];
      unblock_input ();
    }
}
#endif /* NS_IMPL_COCOA */

void
x_set_parent_frame (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
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

  NSTRACE ("x_set_parent_frame");

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
      parent = [FRAME_NS_VIEW (p) window];
      child = [FRAME_NS_VIEW (f) window];

      block_input ();
      [parent addChildWindow: child
                     ordered: NSWindowAbove];
      unblock_input ();

      fset_parent_frame (f, new_value);
    }
}

void
x_set_no_focus_on_map (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `no-focus-on-map' parameter which, if non-nil, means
 * that F's window-system window does not want to receive input focus
 * when it is mapped.  (A frame's window is mapped when the frame is
 * displayed for the first time and when the frame changes its state
 * from `iconified' or `invisible' to `visible'.)
 *
 * Some window managers may not honor this parameter. */
{
  NSTRACE ("x_set_no_focus_on_map");

  if (!EQ (new_value, old_value))
    {
      FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
    }
}

void
x_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/*  Set frame F's `no-accept-focus' parameter which, if non-nil, hints
 * that F's window-system window does not want to receive input focus
 * via mouse clicks or by moving the mouse into it.
 *
 * If non-nil, this may have the unwanted side-effect that a user cannot
 * scroll a non-selected frame with the mouse.
 *
 * Some window managers may not honor this parameter. */
{
  NSTRACE ("x_set_no_accept_focus");

  if (!EQ (new_value, old_value))
    FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
}

void
x_set_z_group (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `z-group' parameter.  If `above', F's window-system
   window is displayed above all windows that do not have the `above'
   property set.  If nil, F's window is shown below all windows that
   have the `above' property set and above all windows that have the
   `below' property set.  If `below', F's window is displayed below
   all windows that do.

   Some window managers may not honor this parameter. */
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  NSTRACE ("x_set_z_group");

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
      /* Not sure what level this should be. */
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
  NSWindow *window = [view window];

  NSTRACE ("ns_set_appearance");

#ifndef NSAppKitVersionNumber10_10
#define NSAppKitVersionNumber10_10 1343
#endif

  if (NSAppKitVersionNumber < NSAppKitVersionNumber10_10)
    return;

  if (EQ (new_value, Qdark))
    {
      window.appearance = [NSAppearance
                            appearanceNamed: NSAppearanceNameVibrantDark];
      FRAME_NS_APPEARANCE (f) = ns_appearance_vibrant_dark;
    }
  else
    {
      window.appearance = [NSAppearance
                            appearanceNamed: NSAppearanceNameAqua];
      FRAME_NS_APPEARANCE (f) = ns_appearance_aqua;
    }
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
         init/default-frame alist, so use a timer (not nice...).
      */
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
/*fprintf(stderr, "color_table: allocated %d\n",idx);*/
  return idx;
}


static int
ns_get_color (const char *name, NSColor **col)
/* --------------------------------------------------------------------------
     Parse a color name
   -------------------------------------------------------------------------- */
/* On *Step, we attempt to mimic the X11 platform here, down to installing an
   X11 rgb.txt-compatible color list in Emacs.clr (see ns_term_init()).
   See: http://thread.gmane.org/gmane.emacs.devel/113050/focus=113272). */
{
  NSColor *new = nil;
  static char hex[20];
  int scaling = 0;
  float r = -1.0, g, b;
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
         selection, but text may be unreadable if we don't.
      */
      if ((new = [NSColor selectedTextColor]) != nil)
        {
          *col = [new colorUsingDefaultColorSpace];
          unblock_input ();
          return 0;
        }

      nsname = NS_SELECTION_FG_COLOR_DEFAULT;
      name = [nsname UTF8String];
    }

  /* First, check for some sort of numeric specification. */
  hex[0] = '\0';

  if (name[0] == '0' || name[0] == '1' || name[0] == '.')  /* RGB decimal */
    {
      NSScanner *scanner = [NSScanner scannerWithString: nsname];
      [scanner scanFloat: &r];
      [scanner scanFloat: &g];
      [scanner scanFloat: &b];
    }
  else if (!strncmp(name, "rgb:", 4))  /* A newer X11 format -- rgb:r/g/b */
    scaling = (snprintf (hex, sizeof hex, "%s", name + 4) - 2) / 3;
  else if (name[0] == '#')        /* An old X11 format; convert to newer */
    {
      int len = (strlen(name) - 1);
      int start = (len % 3 == 0) ? 1 : len / 4 + 1;
      int i;
      scaling = strlen(name+start) / 3;
      for (i = 0; i < 3; i++)
	sprintf (hex + i * (scaling + 1), "%.*s/", scaling,
		 name + start + i * scaling);
      hex[3 * (scaling + 1) - 1] = '\0';
    }

  if (hex[0])
    {
      unsigned int rr, gg, bb;
      float fscale = scaling == 4 ? 65535.0 : (scaling == 2 ? 255.0 : 15.0);
      if (sscanf (hex, "%x/%x/%x", &rr, &gg, &bb))
        {
          r = rr / fscale;
          g = gg / fscale;
          b = bb / fscale;
        }
    }

  if (r >= 0.0F)
    {
      *col = [NSColor colorForEmacsRed: r green: g blue: b alpha: 1.0];
      unblock_input ();
      return 0;
    }

  /* Otherwise, color is expected to be from a list */
  {
    NSEnumerator *lenum, *cenum;
    NSString *name;
    NSColorList *clist;

#ifdef NS_IMPL_GNUSTEP
    /* XXX: who is wrong, the requestor or the implementation? */
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
     Convert a Lisp string object to a NS color
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_lisp_to_color");
  if (STRINGP (color))
    return ns_get_color (SSDATA (color), col);
  else if (SYMBOLP (color))
    return ns_get_color (SSDATA (SYMBOL_NAME (color)), col);
  return 1;
}


void
ns_query_color(void *col, XColor *color_def, int setPixel)
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
      = ARGB_TO_ULONG((int)(a*255),
		      (int)(r*255), (int)(g*255), (int)(b*255));
}


bool
ns_defined_color (struct frame *f,
                  const char *name,
                  XColor *color_def,
                  bool alloc,
                  bool makeIndex)
/* --------------------------------------------------------------------------
         Return true if named color found, and set color_def rgb accordingly.
         If makeIndex and alloc are nonzero put the color in the color_table,
         and set color_def pixel to the resulting index.
         If makeIndex is zero, set color_def pixel to ARGB.
         Return false if not found
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


void
x_set_frame_alpha (struct frame *f)
/* --------------------------------------------------------------------------
     change the entire-frame transparency
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  double alpha = 1.0;
  double alpha_min = 1.0;

  NSTRACE ("x_set_frame_alpha");

  if (dpyinfo->x_highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (INTEGERP (Vframe_alpha_lower_limit))
    alpha_min = (XINT (Vframe_alpha_lower_limit)) / 100.0;

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

  /* FIXME: what about GNUstep? */
#ifdef NS_IMPL_COCOA
  CGPoint mouse_pos =
    CGPointMake(f->left_pos + pix_x,
                f->top_pos + pix_y +
                FRAME_NS_TITLEBAR_HEIGHT(f) + FRAME_TOOLBAR_HEIGHT(f));
  CGWarpMouseCursorPosition (mouse_pos);
#endif
}

static int
note_mouse_movement (struct frame *frame, CGFloat x, CGFloat y)
/*   ------------------------------------------------------------------------
     Called by EmacsView on mouseMovement events.  Passes on
     to emacs mainstream code if we moved off of a rect of interest
     known as last_mouse_glyph.
     ------------------------------------------------------------------------ */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  NSRect *r;

//  NSTRACE ("note_mouse_movement");

  dpyinfo->last_mouse_motion_frame = frame;
  r = &dpyinfo->last_mouse_glyph;

  /* Note, this doesn't get called for enter/leave, since we don't have a
     position.  Those are taken care of in the corresponding NSView methods. */

  /* has movement gone beyond last rect we were tracking? */
  if (x < r->origin.x || x >= r->origin.x + r->size.width
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
    and length of scrollbar respectively
   -------------------------------------------------------------------------- */
{
  id view;
  NSPoint position;
  Lisp_Object frame, tail;
  struct frame *f;
  struct ns_display_info *dpyinfo;

  NSTRACE ("ns_mouse_position");

  if (*fp == NULL)
    {
      fprintf (stderr, "Warning: ns_mouse_position () called with null *fp.\n");
      return;
    }

  dpyinfo = FRAME_DISPLAY_INFO (*fp);

  block_input ();

  /* Clear the mouse-moved flag for every frame on this display.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_NS_P (XFRAME (frame))
        && FRAME_NS_DISPLAY (XFRAME (frame)) == FRAME_NS_DISPLAY (*fp))
      XFRAME (frame)->mouse_moved = 0;

  dpyinfo->last_mouse_scroll_bar = nil;
  if (dpyinfo->last_mouse_frame
      && FRAME_LIVE_P (dpyinfo->last_mouse_frame))
    f = dpyinfo->last_mouse_frame;
  else
    f = dpyinfo->x_focus_frame ? dpyinfo->x_focus_frame : SELECTED_FRAME ();

  if (f && FRAME_NS_P (f))
    {
      view = FRAME_NS_VIEW (*fp);

      position = [[view window] mouseLocationOutsideOfEventStream];
      position = [view convertPoint: position fromView: nil];
      remember_mouse_glyph (f, position.x, position.y,
                            &dpyinfo->last_mouse_glyph);
      NSTRACE_POINT ("position", position);

      if (bar_window) *bar_window = Qnil;
      if (part) *part = scroll_bar_above_handle;

      if (x) XSETINT (*x, lrint (position.x));
      if (y) XSETINT (*y, lrint (position.y));
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
ns_define_frame_cursor (struct frame *f, Cursor cursor)
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
      x_update_cursor (f, 1);
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
  /* An array would be faster, but less easy to read. */
  for (keysym = 0; keysym < last_keysym; keysym += 2)
    if (code == convert_ns_to_X_keysym[keysym])
      return 0xFF00 | convert_ns_to_X_keysym[keysym+1];
  return 0;
/* if decide to use keyCode and Carbon table, use this line:
     return code > 0xff ? 0 : 0xFF00 | ns_keycode_to_xkeysym_table[code]; */
}


char *
x_get_keysym_name (int keysym)
/* --------------------------------------------------------------------------
    Called by keyboard.c.  Not sure if the return val is important, except
    that it be unique.
   -------------------------------------------------------------------------- */
{
  static char value[16];
  NSTRACE ("x_get_keysym_name");
  sprintf (value, "%d", keysym);
  return value;
}

#ifdef NS_IMPL_COCOA
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
      && (EQ (ns_right_alternate_modifier, Qnone)
          || (EQ (ns_right_alternate_modifier, Qleft)
              && EQ (ns_alternate_modifier, Qnone))))
    modifiers |= rightOptionKey;

  if ((flags & NSLeftAlternateKeyMask) == NSLeftAlternateKeyMask
      && EQ (ns_alternate_modifier, Qnone))
    modifiers |= optionKey;

  if ((flags & NSRightCommandKeyMask) == NSRightCommandKeyMask
      && (EQ (ns_right_command_modifier, Qnone)
          || (EQ (ns_right_command_modifier, Qleft)
              && EQ (ns_command_modifier, Qnone))))
    /* Carbon doesn't differentiate between left and right command
       keys.  */
    modifiers |= cmdKey;

  if ((flags & NSLeftCommandKeyMask) == NSLeftCommandKeyMask
      && EQ (ns_command_modifier, Qnone))
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
  ns_redraw_scroll_bars (f);
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
ns_copy_bits (struct frame *f, NSRect src, NSRect dest)
{
  NSTRACE ("ns_copy_bits");

  if (FRAME_NS_VIEW (f))
    {
      hide_bell();              // Ensure the bell image isn't scrolled.

      ns_focus (f, &dest, 1);
      [FRAME_NS_VIEW (f) scrollRect: src
                                 by: NSMakeSize (dest.origin.x - src.origin.x,
                                                 dest.origin.y - src.origin.y)];
      ns_unfocus (f);
    }
}

static void
ns_scroll_run (struct window *w, struct run *run)
/* --------------------------------------------------------------------------
    External (RIF):  Insert or delete n lines at line vpos
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

  x_clear_cursor (w);

  {
    NSRect srcRect = NSMakeRect (x, from_y, width, height);
    NSRect dstRect = NSMakeRect (x, to_y, width, height);

    ns_copy_bits (f, srcRect , dstRect);
  }

  unblock_input ();
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

      block_input ();
      ns_clear_frame_area (f, 0, y, width, height);
      ns_clear_frame_area (f,
                           FRAME_PIXEL_WIDTH (f) - width,
                           y, width, height);
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

  ns_copy_bits (f, srcRect, dstRect);
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
      if (EQ (font->driver->type, Qns))
        s->right_overhang = ((struct nsfont_info *)font)->ital ?
          FONT_HEIGHT (font) * 0.2 : 0;
      else
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

  /* Must clip because of partially visible lines.  */
  ns_clip_to_row (w, row, ANY_AREA, YES);

  if (!p->overlay_p)
    {
      int bx = p->bx, by = p->by, nx = p->nx, ny = p->ny;

      if (bx >= 0 && nx > 0)
        {
          NSRect r = NSMakeRect (bx, by, nx, ny);
          NSRectClip (r);
          [ns_lookup_indexed_color (face->background, f) set];
          NSRectFill (r);
        }
    }

  if (p->which)
    {
      NSRect r = NSMakeRect (p->x, p->y, p->wd, p->h);
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
                                             fg: 0 bg: 0];
          bimgs[p->which - 1] = img;
          xfree (cbits);
        }

      NSTRACE_RECT ("r", r);

      NSRectClip (r);
      /* Since we composite the bitmap instead of just blitting it, we need
         to erase the whole background. */
      [ns_lookup_indexed_color(face->background, f) set];
      NSRectFill (r);

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

#ifdef NS_IMPL_COCOA
      // Note: For periodic images, the full image height is "h + hd".
      // By using the height h, a suitable part of the image is used.
      NSRect fromRect = NSMakeRect(0, 0, p->wd, p->h);

      NSTRACE_RECT ("fromRect", fromRect);

      [img drawInRect: r
              fromRect: fromRect
             operation: NSCompositingOperationSourceOver
              fraction: 1.0
           respectFlipped: YES
                hints: nil];
#else
      {
        NSPoint pt = r.origin;
        pt.y += p->h;
        [img compositeToPoint: pt operation: NSCompositingOperationSourceOver];
      }
#endif
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
     to the glyph width; replace with CURSOR_WIDTH for (V)BAR cursors. */
  if (cursor_type == BAR_CURSOR)
    {
      if (cursor_width < 1)
	cursor_width = max (FRAME_CURSOR_WIDTH (f), 1);

      /* The bar cursor should never be wider than the glyph. */
      if (cursor_width < w->phys_cursor_width)
        w->phys_cursor_width = cursor_width;
    }
  /* If we have an HBAR, "cursor_width" MAY specify height. */
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

  /* Prevent the cursor from being drawn outside the text area. */
  ns_clip_to_row (w, glyph_row, TEXT_AREA, NO); /* do ns_focus(f, &r, 1); if remove */


  face = FACE_FROM_ID_OR_NULL (f, phys_cursor_glyph->face_id);
  if (face && NS_FACE_BACKGROUND (face)
      == ns_index_color (FRAME_CURSOR_COLOR (f), f))
    {
      [ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), f) set];
      hollow_color = FRAME_CURSOR_COLOR (f);
    }
  else
    [FRAME_CURSOR_COLOR (f) set];

#ifdef NS_IMPL_COCOA
  /* TODO: This makes drawing of cursor plus that of phys_cursor_glyph
           atomic.  Cleaner ways of doing this should be investigated.
           One way would be to set a global variable DRAWING_CURSOR
  	   when making the call to draw_phys..(), don't focus in that
  	   case, then move the ns_unfocus() here after that call. */
  NSDisableScreenUpdates ();
#endif

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

  /* draw the character under the cursor */
  if (cursor_type != NO_CURSOR)
    draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);

#ifdef NS_IMPL_COCOA
  NSEnableScreenUpdates ();
#endif

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

  /* Do underline. */
  if (face->underline_p)
    {
      if (s->face->underline_type == FACE_UNDER_WAVE)
        {
          if (face->underline_defaulted_p)
            [defaultCol set];
          else
            [ns_lookup_indexed_color (face->underline_color, s->f) set];

          ns_draw_underwave (s, width, x);
        }
      else if (s->face->underline_type == FACE_UNDER_LINE)
        {

          NSRect r;
          unsigned long thickness, position;

          /* If the prev was underlined, match its appearance. */
          if (s->prev && s->prev->face->underline_p
	      && s->prev->face->underline_type == FACE_UNDER_LINE
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
              Lisp_Object val = buffer_local_value (Qunderline_minimum_offset,
                                                    s->w->contents);
              if (INTEGERP (val))
                minimum_offset = XFASTINT (val);
              else
                minimum_offset = 1;
              val = buffer_local_value (Qx_underline_at_descent_line,
                                        s->w->contents);
              underline_at_descent_line = !(NILP (val) || EQ (val, Qunbound));
              val = buffer_local_value (Qx_use_underline_position_properties,
                                        s->w->contents);
              use_underline_position_properties =
		!(NILP (val) || EQ (val, Qunbound));

              /* Use underline thickness of font, defaulting to 1. */
              thickness = (font && font->underline_thickness > 0)
                ? font->underline_thickness : 1;

              /* Determine the offset of underlining from the baseline. */
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

              /* Ensure underlining is not cropped. */
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
     and ignoring overline_margin. */
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
     vertical position.*/
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
ns_draw_box (NSRect r, CGFloat thickness, NSColor *col,
             char left_p, char right_p)
/* --------------------------------------------------------------------------
    Draw an unfilled rect inside r, optionally leaving left and/or right open.
    Note we can't just use an NSDrawRect command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  NSRect s = r;
  [col set];

  /* top, bottom */
  s.size.height = thickness;
  NSRectFill (s);
  s.origin.y += r.size.height - thickness;
  NSRectFill (s);

  s.size.height = r.size.height;
  s.origin.y = r.origin.y;

  /* left, right (optional) */
  s.size.width = thickness;
  if (left_p)
    NSRectFill (s);
  if (right_p)
    {
      s.origin.x += r.size.width - thickness;
      NSRectFill (s);
    }
}


static void
ns_draw_relief (NSRect r, int thickness, char raised_p,
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

  /* TODO: mitering. Using NSBezierPath doesn't work because of color switch. */

  /* top */
  sr.size.height = thickness;
  if (top_p) NSRectFill (sr);

  /* left */
  sr.size.height = r.size.height;
  sr.size.width = thickness;
  if (left_p) NSRectFill (sr);

  [(raised_p ? darkCol : lightCol) set];

  /* bottom */
  sr.size.width = r.size.width;
  sr.size.height = thickness;
  sr.origin.y += r.size.height - thickness;
  if (bottom_p) NSRectFill (sr);

  /* right */
  sr.size.height = r.size.height;
  sr.origin.y = r.origin.y;
  sr.size.width = thickness;
  sr.origin.x += r.size.width - thickness;
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
  int thickness;
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

  thickness = face->box_line_width;

  NSTRACE ("ns_dumpglyphs_box_or_relief");

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));
  last_glyph = (s->cmp || s->img
                ? s->first_glyph : s->first_glyph + s->nchars-1);

  right_x = ((s->row->full_width_p && s->extends_to_end_of_line_p
	      ? last_x - 1 : min (last_x, s->x + s->background_width) - 1));

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL || s->next->hl != s->hl)));

  r = NSMakeRect (s->x, s->y, right_x - s->x + 1, s->height);

  /* TODO: Sometimes box_color is 0 and this seems wrong; should investigate. */
  if (s->face->box == FACE_SIMPLE_BOX && s->face->box_color)
    {
      ns_draw_box (r, abs (thickness),
                   ns_lookup_indexed_color (face->box_color, s->f),
                  left_p, right_p);
    }
  else
    {
      ns_draw_relief (r, abs (thickness), s->face->box == FACE_RAISED_BOX,
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
      int box_line_width = max (s->face->box_line_width, 0);
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
  int box_line_vwidth = max (s->face->box_line_width, 0);
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
    x += abs (s->face->box_line_width);

  bg_x = x;
  bg_y =  s->slice.y == 0 ? s->y : s->y + box_line_vwidth;
  bg_height = s->height;
  /* other terms have this, but was causing problems w/tabbar mode */
  /* - 2 * box_line_vwidth; */

  if (s->slice.x == 0) x += s->img->hmargin;
  if (s->slice.y == 0) y += s->img->vmargin;

  /* Draw BG: if we need larger area than image itself cleared, do that,
     otherwise, since we composite the image under NS (instead of mucking
     with its background color), we must clear just the image area. */
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

  /* Draw the image.. do we need to draw placeholder if img ==nil? */
  if (img != nil)
    {
#ifdef NS_IMPL_COCOA
      NSRect dr = NSMakeRect (x, y, s->slice.width, s->slice.height);
      NSRect ir = NSMakeRect (s->slice.x,
                              s->img->height - s->slice.y - s->slice.height,
                              s->slice.width, s->slice.height);
      [img drawInRect: dr
             fromRect: ir
             operation: NSCompositingOperationSourceOver
              fraction: 1.0
           respectFlipped: YES
                hints: nil];
#else
      [img compositeToPoint: NSMakePoint (x, y + s->slice.height)
                  operation: NSCompositingOperationSourceOver];
#endif
    }

  if (s->hl == DRAW_CURSOR)
    {
    [FRAME_CURSOR_COLOR (s->f) set];
    if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
      tdCol = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f);
    else
      /* Currently on NS img->mask is always 0. Since
         get_window_cursor_type specifies a hollow box cursor when on
         a non-masked image we never reach this clause. But we put it
         in, in anticipation of better support for image masks on
         NS. */
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }
  else
    {
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }

  /* Draw underline, overline, strike-through. */
  ns_draw_text_decoration (s, face, tdCol, br.size.width, br.origin.x);

  /* Draw relief, if requested */
  if (s->img->relief || s->hl ==DRAW_IMAGE_RAISED || s->hl ==DRAW_IMAGE_SUNKEN)
    {
      if (s->hl == DRAW_IMAGE_SUNKEN || s->hl == DRAW_IMAGE_RAISED)
        {
          th = tool_bar_button_relief >= 0 ?
            tool_bar_button_relief : DEFAULT_TOOL_BAR_BUTTON_RELIEF;
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
      ns_draw_relief (r, th, raised_p,
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
      ns_draw_box (br, thickness, FRAME_CURSOR_COLOR (s->f), 1, 1);
    }
}


static void
ns_dumpglyphs_stretch (struct glyph_string *s)
{
  NSRect r[2];
  int n, i;
  struct face *face;
  NSColor *fgCol, *bgCol;

  if (!s->background_filled_p)
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      *r = NSMakeRect (s->x, s->y, s->background_width, s->height);

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

      for (i = 0; i < n; ++i)
        {
          if (!s->row->full_width_p)
            {
	      int overrun, leftoverrun;

              /* truncate to avoid overwriting fringe and/or scrollbar */
	      overrun = max (0, (s->x + s->background_width)
			     - (WINDOW_BOX_RIGHT_EDGE_X (s->w)
				- WINDOW_RIGHT_FRINGE_WIDTH (s->w)));
              r[i].size.width -= overrun;

	      /* truncate to avoid overwriting to left of the window box */
	      leftoverrun = (WINDOW_BOX_LEFT_EDGE_X (s->w)
			     + WINDOW_LEFT_FRINGE_WIDTH (s->w)) - s->x;

	      if (leftoverrun > 0)
		{
		  r[i].origin.x += leftoverrun;
		  r[i].size.width -= leftoverrun;
		}

              /* XXX: Try to work between problem where a stretch glyph on
                 a partially-visible bottom row will clear part of the
                 modeline, and another where list-buffers headers and similar
                 rows erroneously have visible_height set to 0.  Not sure
                 where this is coming from as other terms seem not to show. */
              r[i].size.height = min (s->height, s->row->visible_height);
            }

          [bgCol set];

          /* NOTE: under NS this is NOT used to draw cursors, but we must avoid
             overwriting cursor (usually when cursor on a tab) */
          if (s->hl == DRAW_CURSOR)
            {
              CGFloat x, width;

              x = r[i].origin.x;
              width = s->w->phys_cursor_width;
              r[i].size.width -= width;
              r[i].origin.x += width;

              NSRectFill (r[i]);

              /* Draw overlining, etc. on the cursor. */
              if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
                ns_draw_text_decoration (s, face, bgCol, width, x);
              else
                ns_draw_text_decoration (s, face, fgCol, width, x);
            }
          else
            {
              NSRectFill (r[i]);
            }

          /* Draw overlining, etc. on the stretch glyph (or the part
             of the stretch glyph after the cursor). */
          ns_draw_text_decoration (s, face, fgCol, r[i].size.width,
                                   r[i].origin.x);
        }
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
    x = s->x + eabs (s->face->box_line_width);
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
    x = s->x + eabs (s->face->box_line_width);
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
          ns_draw_box (r, 1, FRAME_CURSOR_COLOR (s->f), 1, 1);
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

  /* Draw box if not done already. */
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

      /* Don't need wakeup timer any more */
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

/* GNUstep does not have cancelTracking.  */
#ifdef NS_IMPL_COCOA
/* Check if menu open should be canceled or continued as normal.  */
void
ns_check_menu_open (NSMenu *menu)
{
  /* Click in menu bar? */
  NSArray *a = [[NSApp mainMenu] itemArray];
  int i;
  BOOL found = NO;

  if (menu == nil) // Menu tracking ended.
    {
      if (menu_will_open_state == MENU_OPENING)
        menu_will_open_state = MENU_NONE;
      return;
    }

  for (i = 0; ! found && i < [a count]; i++)
    found = menu == [[a objectAtIndex:i] submenu];
  if (found)
    {
      if (menu_will_open_state == MENU_NONE && emacs_event)
        {
          NSEvent *theEvent = [NSApp currentEvent];
          struct frame *emacsframe = SELECTED_FRAME ();

          [menu cancelTracking];
          menu_will_open_state = MENU_PENDING;
          emacs_event->kind = MENU_BAR_ACTIVATE_EVENT;
          EV_TRAILER (theEvent);

          CGEventRef ourEvent = CGEventCreate (NULL);
          menu_mouse_point = CGEventGetLocation (ourEvent);
          CFRelease (ourEvent);
        }
      else if (menu_will_open_state == MENU_OPENING)
        {
          menu_will_open_state = MENU_NONE;
        }
    }
}

/* Redo saved menu click if state is MENU_PENDING.  */
void
ns_check_pending_open_menu ()
{
  if (menu_will_open_state == MENU_PENDING)
    {
      CGEventSourceRef source
        = CGEventSourceCreate (kCGEventSourceStateHIDSystemState);

      CGEventRef event = CGEventCreateMouseEvent (source,
                                                  kCGEventLeftMouseDown,
                                                  menu_mouse_point,
                                                  kCGMouseButtonLeft);
      CGEventSetType (event, kCGEventLeftMouseDown);
      CGEventPost (kCGHIDEventTap, event);
      CFRelease (event);
      CFRelease (source);

      menu_will_open_state = MENU_OPENING;
    }
}
#endif /* NS_IMPL_COCOA */

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

      /* we manage autorelease pools by allocate/reallocate each time around
         the loop; strict nesting is occasionally violated but seems not to
         matter.. earlier methods using full nesting caused major memory leaks */
      [outerpool release];
      outerpool = [[NSAutoreleasePool alloc] init];

      /* If have pending open-file requests, attend to the next one of those. */
      if (ns_pending_files && [ns_pending_files count] != 0
          && [(EmacsApp *)NSApp openFile: [ns_pending_files objectAtIndex: 0]])
        {
          [ns_pending_files removeObjectAtIndex: 0];
        }
      /* Deal with pending service requests. */
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
      /* We already have events pending. */
      raise (SIGIO);
      errno = EINTR;
      return -1;
    }

  for (k = 0; k < nfds+1; k++)
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

  [outerpool release];
  outerpool = [[NSAutoreleasePool alloc] init];


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

      /* Inform fd_handler that select should be called */
      c = 'g';
      emacs_write_sig (selfds[1], &c, 1);
    }
  else if (nr == 0 && timeout)
    {
      /* No file descriptor, just a timeout, no need to wake fd_handler  */
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
      /* Send appdefined so we exit from the loop */
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
          /* The NX_APPDEFINED event we received was a timeout. */
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
          /* Received back from select () in fd_handler; copy the results */
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
/* Break out of the NS run loop in ns_select or ns_read_socket. */
{
  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "ns_run_loop_break");

  /* If we don't have a GUI, don't send the event. */
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

  /* optimization; display engine sends WAY too many of these.. */
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
  /* the parent view is flipped, so we need to flip y value */
  v = [view frame];
  r.origin.y = (v.size.height - r.size.height - r.origin.y);

  XSETWINDOW (win, window);
  block_input ();

  /* we want at least 5 lines to display a scrollbar */
  if (WINDOW_TOTAL_LINES (window) < 5)
    {
      if (!NILP (window->vertical_scroll_bar))
        {
          bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
          [bar removeFromSuperview];
          wset_vertical_scroll_bar (window, Qnil);
          [bar release];
        }
      ns_clear_frame_area (f, left, top, width, height);
      unblock_input ();
      return;
    }

  if (NILP (window->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	ns_clear_frame_area (f, left, top, width, height);

      bar = [[EmacsScroller alloc] initFrame: r window: win];
      wset_vertical_scroll_bar (window, make_save_ptr (bar));
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
          if (oldRect.origin.x != r.origin.x)
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
      External (hook): Update or add scrollbar
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

  /* optimization; display engine sends WAY too many of these.. */
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
  /* the parent view is flipped, so we need to flip y value */
  v = [view frame];
  r.origin.y = (v.size.height - r.size.height - r.origin.y);

  XSETWINDOW (win, window);
  block_input ();

  if (NILP (window->horizontal_scroll_bar))
    {
      if (width > 0 && height > 0)
	ns_clear_frame_area (f, left, top, width, height);

      bar = [[EmacsScroller alloc] initFrame: r window: win];
      wset_horizontal_scroll_bar (window, make_save_ptr (bar));
      update_p = YES;
    }
  else
    {
      NSRect oldRect;
      bar = XNS_SCROLL_BAR (window->horizontal_scroll_bar);
      oldRect = [bar frame];
      if (FRAME_LIVE_P (f) && !NSEqualRects (oldRect, r))
        {
          if (oldRect.origin.y != r.origin.y)
            ns_clear_frame_area (f, left, top, width, height);
          [bar setFrame: r];
          update_p = YES;
        }
    }

  /* If there are both horizontal and vertical scroll-bars they leave
     a square that belongs to neither. We need to clear it otherwise
     it fills with junk. */
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
  BOOL removed = NO;

  NSTRACE ("ns_judge_scroll_bars");
  for (i = [subviews count]-1; i >= 0; --i)
    {
      view = [subviews objectAtIndex: i];
      if (![view isKindOfClass: [EmacsScroller class]]) continue;
      if ([view judge])
        removed = YES;
    }

  if (removed)
    [eview updateFrameSize: NO];
}

/* ==========================================================================

    Initialization

   ========================================================================== */

int
x_display_pixel_height (struct ns_display_info *dpyinfo)
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
x_display_pixel_width (struct ns_display_info *dpyinfo)
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
     Convert modifier name to lisp symbol
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
      Check a parameter value in user's preferences
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
    dpyinfo->root_window = 42; /* a placeholder.. */
    dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame = NULL;
    dpyinfo->n_fonts = 0;
    dpyinfo->smallest_font_height = 1;
    dpyinfo->smallest_char_width = 1;

    reset_mouse_highlight (&dpyinfo->mouse_highlight);
}


/* This and next define (many of the) public functions in this file. */
/* x_... are generic versions in xdisp.c that we, and other terms, get away
         with using despite presence in the "system dependent" redisplay
         interface.  In addition, many of the ns_ methods have code that is
         shared with all terms, indicating need for further refactoring. */
extern frame_parm_handler ns_frame_parm_handlers[];
static struct redisplay_interface ns_redisplay_interface =
{
  ns_frame_parm_handlers,
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  ns_scroll_run,
  ns_after_update_window_line,
  ns_update_window_begin,
  ns_update_window_end,
  0, /* flush_display */
  x_clear_window_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area,
  ns_draw_fringe_bitmap,
  0, /* define_fringe_bitmap */ /* FIXME: simplify ns_draw_fringe_bitmap */
  0, /* destroy_fringe_bitmap */
  ns_compute_glyph_string_overhangs,
  ns_draw_glyph_string,
  ns_define_frame_cursor,
  ns_clear_frame_area,
  ns_draw_window_cursor,
  ns_draw_vertical_window_border,
  ns_draw_window_divider,
  ns_shift_glyphs_for_insert,
  ns_show_hourglass,
  ns_hide_hourglass
};


static void
ns_delete_display (struct ns_display_info *dpyinfo)
{
  /* TODO... */
}


/* This function is called when the last frame on a display is deleted. */
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

  x_destroy_all_bitmaps (dpyinfo);
  ns_delete_display (dpyinfo);
  unblock_input ();
}


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
  terminal->mouse_position_hook = ns_mouse_position;
  terminal->frame_rehighlight_hook = ns_frame_rehighlight;
  terminal->frame_raise_lower_hook = ns_frame_raise_lower;
  terminal->fullscreen_hook = ns_fullscreen_hook;
  terminal->menu_show_hook = ns_menu_show;
  terminal->popup_dialog_hook = ns_popup_dialog;
  terminal->set_vertical_scroll_bar_hook = ns_set_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = ns_set_horizontal_scroll_bar;
  terminal->condemn_scroll_bars_hook = ns_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = ns_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = ns_judge_scroll_bars;
  terminal->delete_frame_hook = x_destroy_window;
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
     The view will then ask the NSApp to stop and return to Emacs. */
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

    if ( cl == nil )
      {
        Lisp_Object color_file, color_map, color;
        unsigned long c;
        char *name;

        color_file = Fexpand_file_name (build_string ("rgb.txt"),
                         Fsymbol_value (intern ("data-directory")));

        color_map = Fx_load_color_file (color_file);
        if (NILP (color_map))
          fatal ("Could not read %s.\n", SDATA (color_file));

        cl = [[NSColorList alloc] initWithName: @"Emacs"];
        for ( ; CONSP (color_map); color_map = XCDR (color_map))
          {
            color = XCAR (color_map);
            name = SSDATA (XCAR (color));
            c = XINT (XCDR (color));
            [cl setColor:
                  [NSColor colorForEmacsRed: RED_FROM_ULONG (c) / 255.0
                                      green: GREEN_FROM_ULONG (c) / 255.0
                                       blue: BLUE_FROM_ULONG (c) / 255.0
                                      alpha: 1.0]
                  forKey: [NSString stringWithUTF8String: name]];
          }
        [cl writeToFile: nil];
      }
  }

  NSTRACE_MSG ("Versions");

  {
#ifdef NS_IMPL_GNUSTEP
    Vwindow_system_version = build_string (gnustep_base_version);
#else
    /*PSnextrelease (128, c); */
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

    [[NSNotificationCenter defaultCenter]
      addObserver: mainMenu
         selector: @selector (trackingNotification:)
             name: NSMenuDidBeginTrackingNotification object: mainMenu];
    [[NSNotificationCenter defaultCenter]
      addObserver: mainMenu
         selector: @selector (trackingNotification:)
             name: NSMenuDidEndTrackingNotification object: mainMenu];
  }
#endif /* macOS menu setup */

  /* Register our external input/output types, used for determining
     applicable services and also drag/drop eligibility. */

  NSTRACE_MSG ("Input/output types");

  ns_send_types = [[NSArray arrayWithObjects: NSStringPboardType, nil] retain];
  ns_return_types = [[NSArray arrayWithObjects: NSStringPboardType, nil]
                      retain];
  ns_drag_types = [[NSArray arrayWithObjects:
                            NSStringPboardType,
                            NSTabularTextPboardType,
                            NSFilenamesPboardType,
                            NSURLPboardType, nil] retain];

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

  if (represented_filename != nil && represented_frame)
    {
      NSString *fstr = represented_filename;
      NSView *view = FRAME_NS_VIEW (represented_frame);
#ifdef NS_IMPL_COCOA
      /* work around a bug observed on 10.3 and later where
         setTitleWithRepresentedFilename does not clear out previous state
         if given filename does not exist */
      if (! [[NSFileManager defaultManager] fileExistsAtPath: fstr])
        [[view window] setRepresentedFilename: @""];
#endif
      [[view window] setRepresentedFilename: fstr];
      [represented_filename release];
      represented_filename = nil;
      represented_frame = NULL;
    }

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
      fprintf (stderr, "Dropping external cursor update event.\n");
      return;
    }

  if (type == NSEventTypeApplicationDefined)
    {
      /* Events posted by ns_send_appdefined interrupt the run loop here.
         But, if a modal window is up, an appdefined can still come through,
         (e.g., from a makeKeyWindow event) but stopping self also stops the
         modal loop. Just defer it until later. */
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
        has_focus = di->x_focus_frame != 0;
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


/* Open a file (used by below, after going into queue read by ns_read_socket) */
- (BOOL) openFile: (NSString *)fileName
{
  NSTRACE ("[EmacsApp openFile:]");

  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_OPEN_FILE_LINE;
  ns_input_file = append2 (ns_input_file, build_string ([fileName UTF8String]));
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
     When application is loaded, terminate event loop in ns_term_init
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
  if ([NSApp activationPolicy] == NSApplicationActivationPolicyProhibited) {
    /* Set the app's activation policy to regular when we run outside
       of a bundle.  This is already done for us by Info.plist when we
       run inside a bundle. */
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

/*   Notification from the Workspace to open a file */
- (BOOL)application: sender openFile: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}


/*   Open a file as a temporary file */
- (BOOL)application: sender openTempFile: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}


/*   Notification from the Workspace to open a file noninteractively (?) */
- (BOOL)application: sender openFileWithoutUI: (NSString *)file
{
  if (ns_do_open_file || not_in_argv (file))
    [ns_pending_files addObject: file];
  return YES;
}

/*   Notification from the Workspace to open multiple files */
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


/* TODO: these may help w/IO switching btwn terminal and NSApp */
- (void)applicationWillBecomeActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationWillBecomeActive:]");
  //ns_app_active=YES;
}

- (void)applicationDidBecomeActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationDidBecomeActive:]");

#ifdef NS_IMPL_GNUSTEP
  if (! applicationDidFinishLaunchingCalled)
    [self applicationDidFinishLaunching:notification];
#endif
  //ns_app_active=YES;

  ns_update_auto_hide_menu_bar ();
  // No constraining takes place when the application is not active.
  ns_constrain_all_frames ();
}
- (void)applicationDidResignActive: (NSNotification *)notification
{
  NSTRACE ("[EmacsApp applicationDidResignActive:]");

  //ns_app_active=NO;
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
  /*NSTRACE ("timeout_handler"); */
  ns_send_appdefined (-2);
}

- (void)sendFromMainThread:(id)unused
{
  ns_send_appdefined (nextappdefined);
}

- (void)fd_handler:(id)unused
/* --------------------------------------------------------------------------
     Check data waiting on file descriptors and terminate if so
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

/* called from system: queue for next pass through event loop */
- (void)requestService: (NSPasteboard *)pboard
              userData: (NSString *)userData
                 error: (NSString **)error
{
  [ns_pending_service_names addObject: userData];
  [ns_pending_service_args addObject: [NSString stringWithUTF8String:
      SSDATA (ns_string_from_pasteboard (pboard))]];
}


/* called from ns_read_socket to clear queue */
- (BOOL)fulfillService: (NSString *)name withArg: (NSString *)arg
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  NSTRACE ("[EmacsApp fulfillService:withArg:]");

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_SPI_SERVICE_CALL;
  ns_input_spi_name = build_string ([name UTF8String]);
  ns_input_spi_arg = build_string ([arg UTF8String]);
  emacs_event->modifiers = EV_MODIFIERS (theEvent);
  EV_TRAILER (theEvent);

  return YES;
}


@end  /* EmacsApp */


/* ==========================================================================

    EmacsView implementation

   ========================================================================== */


@implementation EmacsView

/* needed to inform when window closed from LISP */
- (void) setWindowClosing: (BOOL)closing
{
  NSTRACE ("[EmacsView setWindowClosing:%d]", closing);

  windowClosing = closing;
}


- (void)dealloc
{
  NSTRACE ("[EmacsView dealloc]");
  [toolbar release];
  if (fs_state == FULLSCREEN_BOTH)
    [nonfs_window release];
  [super dealloc];
}


/* called on font panel selection */
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
      ns_input_fontsize = make_number (lrint (size));
      ns_input_font = build_string ([[newFont familyName] UTF8String]);
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
/* Keyboard handling. */
#define NS_KEYLOG 0

- (void)keyDown: (NSEvent *)theEvent
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  int code;
  unsigned fnKeysym = 0;
  static NSMutableArray *nsEvArray;
  unsigned int flags = [theEvent modifierFlags];

  NSTRACE ("[EmacsView keyDown:]");

  /* Rhapsody and macOS give up and down events for the arrow keys */
  if (ns_fake_keydown == YES)
    ns_fake_keydown = NO;
  else if ([theEvent type] != NSEventTypeKeyDown)
    return;

  if (!emacs_event)
    return;

 if (![[self window] isKeyWindow]
     && [[theEvent window] isKindOfClass: [EmacsWindow class]]
     /* we must avoid an infinite loop here. */
     && (EmacsView *)[[theEvent window] delegate] != self)
   {
     /* XXX: There is an occasional condition in which, when Emacs display
         updates a different frame from the current one, and temporarily
         selects it, then processes some interrupt-driven input
         (dispnew.c:3878), OS will send the event to the correct NSWindow, but
         for some reason that window has its first responder set to the NSView
         most recently updated (I guess), which is not the correct one. */
     [(EmacsView *)[[theEvent window] delegate] keyDown: theEvent];
     return;
   }

  if (nsEvArray == nil)
    nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];

  [NSCursor setHiddenUntilMouseMoves: YES];

  if (hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
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

      /* is it a "function key"? */
      /* Note: Sometimes a plain key will have the NSEventModifierFlagNumericPad
         flag set (this is probably a bug in the OS).
      */
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
             because Emacs treats Delete and KP-Delete same (in simple.el). */
          if ((fnKeysym == 0xFFFF && [theEvent keyCode] == 0x33)
#ifdef NS_IMPL_GNUSTEP
              /*  GNUstep uses incompatible keycodes, even for those that are
                  supposed to be hardware independent.  Just check for delete.
                  Keypad delete does not have keysym 0xFFFF.
                  See https://savannah.gnu.org/bugs/?25395
              */
              || (fnKeysym == 0xFFFF && code == 127)
#endif
            )
            code = 0xFF08; /* backspace */
          else
            code = fnKeysym;
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
      emacs_event->modifiers = EV_MODIFIERS2 (flags);

      /* Function keys (such as the F-keys, arrow keys, etc.) set
         modifiers as though the fn key has been pressed when it
         hasn't.  Also some combinations of fn and a function key
         return a different key than was pressed (e.g. fn-<left> gives
         <home>).  We need to unset the fn modifier in these cases.
         FIXME: Can we avoid setting it in the first place.  */
      if (fnKeysym && (flags & NS_FUNCTION_KEY_MASK))
        emacs_event->modifiers ^= parse_solitary_modifier (ns_function_modifier);

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
    fprintf (stderr, "keyDown: Begin compose sequence.\n");

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


/* <NSTextInput> implementation (called through super interpretKeyEvents:]). */


/* <NSTextInput>: called when done composing;
   NOTE: also called when we delete over working text, followed immed.
         by doCommandBySelector: deleteBackward: */
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

  /* first, clear any working text */
  if (workingText != nil)
    [self deleteWorkingText];

  /* It might be preferable to use getCharacters:range: below,
     cf. https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/CocoaPerformance/Articles/StringDrawing.html#//apple_ref/doc/uid/TP40001445-112378.
     However, we probably can't use SAFE_NALLOCA here because it might
     exit nonlocally.  */

  /* now insert the string as keystrokes */
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
      /* TODO: still need this? */
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


/* <NSTextInput>: inserts display of composing characters */
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

  if (workingText != nil)
    [self deleteWorkingText];
  if ([str length] == 0)
    return;

  if (!emacs_event)
    return;

  processingCompose = YES;
  workingText = [str copy];
  ns_working_text = build_string ([workingText UTF8String]);

  emacs_event->kind = NS_TEXT_EVENT;
  emacs_event->code = KEY_NS_PUT_WORKING_TEXT;
  EV_TRAILER ((id)nil);
}


/* delete display of composing characters [not in <NSTextInput>] */
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


/* used to position char selection windows, etc. */
- (NSRect)firstRectForCharacterRange: (NSRange)theRange
{
  NSRect rect;
  NSPoint pt;
  struct window *win = XWINDOW (FRAME_SELECTED_WINDOW (emacsframe));

  NSTRACE ("[EmacsView firstRectForCharacterRange:]");

  if (NS_KEYLOG)
    NSLog (@"firstRectForCharRange request");

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
      /* happens when user backspaces over an ongoing composition:
         throw a 'delete' into the event queue */
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

/* End <NSTextInput> impl. */
/*****************************************************************************/


/* This is what happens when the user presses a mouse button.  */
- (void)mouseDown: (NSEvent *)theEvent
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  NSPoint p = [self convertPoint: [theEvent locationInWindow] fromView: nil];

  NSTRACE ("[EmacsView mouseDown:]");

  [self deleteWorkingText];

  if (!emacs_event)
    return;

  dpyinfo->last_mouse_frame = emacsframe;
  /* appears to be needed to prevent spurious movement events generated on
     button clicks */
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

              if (NUMBERP (ns_mwheel_line_height))
                lineHeight = XINT (ns_mwheel_line_height);
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
          emacs_event->arg = (make_number (lines));

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
          /* Mac notebooks send wheel events w/delta =0 when trackpad scrolling */
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


/* Tell emacs the mouse has moved. */
- (void)mouseMoved: (NSEvent *)e
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  Lisp_Object frame;
  NSPoint pt;

  NSTRACE_WHEN (NSTRACE_GROUP_EVENTS, "[EmacsView mouseMoved:]");

  dpyinfo->last_mouse_movement_time = EV_TIMESTAMP (e);
  pt = [self convertPoint: [e locationInWindow] fromView: nil];
  dpyinfo->last_mouse_motion_x = pt.x;
  dpyinfo->last_mouse_motion_y = pt.y;

  /* update any mouse face */
  if (hlinfo->mouse_face_hidden)
    {
      hlinfo->mouse_face_hidden = 0;
      clear_mouse_face (hlinfo);
    }

  /* tooltip handling */
  previous_help_echo_string = help_echo_string;
  help_echo_string = Qnil;

  if (!NILP (Vmouse_autoselect_window))
    {
      NSTRACE_MSG ("mouse_autoselect_window");
      static Lisp_Object last_mouse_window;
      Lisp_Object window
	= window_from_coordinates (emacsframe, pt.x, pt.y, 0, 0);

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

  if (!note_mouse_movement (emacsframe, pt.x, pt.y))
    help_echo_string = previous_help_echo_string;

  XSETFRAME (frame, emacsframe);
  if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
    {
      /* NOTE: help_echo_{window,pos,object} are set in xdisp.c
         (note_mouse_highlight), which is called through the
         note_mouse_movement () call above */
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

- (void) updateFrameSize: (BOOL) delay
{
  NSWindow *window = [self window];
  NSRect wr = [window frame];
  int extra = 0;
  int oldc = cols, oldr = rows;
  int oldw = FRAME_PIXEL_WIDTH (emacsframe);
  int oldh = FRAME_PIXEL_HEIGHT (emacsframe);
  int neww, newh;

  NSTRACE ("[EmacsView updateFrameSize:]");
  NSTRACE_SIZE ("Original size", NSMakeSize (oldw, oldh));
  NSTRACE_RECT ("Original frame", wr);
  NSTRACE_MSG  ("Original columns: %d", cols);
  NSTRACE_MSG  ("Original rows: %d", rows);

  if (! [self isFullscreen])
    {
      int toolbar_height;
#ifdef NS_IMPL_GNUSTEP
      // GNUstep does not always update the tool bar height.  Force it.
      if (toolbar && [toolbar isVisible])
          update_frame_tool_bar (emacsframe);
#endif

      toolbar_height = FRAME_TOOLBAR_HEIGHT (emacsframe);
      if (toolbar_height < 0)
        toolbar_height = 35;

      extra = FRAME_NS_TITLEBAR_HEIGHT (emacsframe)
        + toolbar_height;
    }

  if (wait_for_tool_bar)
    {
      /* The toolbar height is always 0 in fullscreen and undecorated
         frames, so don't wait for it to become available. */
      if (FRAME_TOOLBAR_HEIGHT (emacsframe) == 0
          && FRAME_UNDECORATED (emacsframe) == false
          && ! [self isFullscreen])
        {
          NSTRACE_MSG ("Waiting for toolbar");
          return;
        }
      wait_for_tool_bar = NO;
    }

  neww = (int)wr.size.width - emacsframe->border_width;
  newh = (int)wr.size.height - extra;

  NSTRACE_SIZE ("New size", NSMakeSize (neww, newh));
  NSTRACE_MSG ("FRAME_TOOLBAR_HEIGHT: %d", FRAME_TOOLBAR_HEIGHT (emacsframe));
  NSTRACE_MSG ("FRAME_NS_TITLEBAR_HEIGHT: %d", FRAME_NS_TITLEBAR_HEIGHT (emacsframe));

  cols = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (emacsframe, neww);
  rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (emacsframe, newh);

  if (cols < MINWIDTH)
    cols = MINWIDTH;

  if (rows < MINHEIGHT)
    rows = MINHEIGHT;

  NSTRACE_MSG ("New columns: %d", cols);
  NSTRACE_MSG ("New rows: %d", rows);

  if (oldr != rows || oldc != cols || neww != oldw || newh != oldh)
    {
      NSView *view = FRAME_NS_VIEW (emacsframe);

      change_frame_size (emacsframe,
                         FRAME_PIXEL_TO_TEXT_WIDTH (emacsframe, neww),
                         FRAME_PIXEL_TO_TEXT_HEIGHT (emacsframe, newh),
                         0, delay, 0, 1);
      SET_FRAME_GARBAGED (emacsframe);
      cancel_mouse_face (emacsframe);

      /* The next two lines set the frame to the same size as we've
         already set above.  We need to do this when we switch back
         from non-native fullscreen, in other circumstances it appears
         to be a noop.  (bug#28872) */
      wr = NSMakeRect (0, 0, neww, newh);
      [view setFrame: wr];

      // to do: consider using [NSNotificationCenter postNotificationName:].
      [self windowDidMove: // Update top/left.
	      [NSNotification notificationWithName:NSWindowDidMoveNotification
					    object:[view window]]];
    }
  else
    {
      NSTRACE_MSG ("No change");
    }
}

- (NSSize)windowWillResize: (NSWindow *)sender toSize: (NSSize)frameSize
/* normalize frame to gridded text size */
{
  int extra = 0;

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
    /* this sets window title to have size in it; the wm does this under GS */
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
	esprintf (size_title, "%s  —  (%d x %d)", old_title, cols, rows);
        [window setTitle: [NSString stringWithUTF8String: size_title]];
        [window display];
        xfree (size_title);
      }
  }
#endif /* NS_IMPL_COCOA */

  NSTRACE_MSG ("cols: %d  rows: %d", cols, rows);

  /* Restrict the new size to the text gird.

     Don't restrict the width if the user only adjusted the height, and
     vice versa.  (Without this, the frame would shrink, and move
     slightly, if the window was resized by dragging one of its
     borders.) */
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
      without getting a willResize.. therefore we need to act as if we got
      the willResize now */
  NSSize sz = [theWindow frame].size;
  sz = [self windowWillResize: theWindow toSize: sz];
#endif /* NS_IMPL_GNUSTEP */

  if (cols > 0 && rows > 0)
    {
      [self updateFrameSize: YES];
    }

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


- (void)windowDidBecomeKey: (NSNotification *)notification
/* cf. x_detect_focus_change(), x_focus_changed(), x_new_focus_frame() */
{
  [self windowDidBecomeKey];
}


- (void)windowDidBecomeKey      /* for direct calls */
{
  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (emacsframe);
  struct frame *old_focus = dpyinfo->x_focus_frame;

  NSTRACE ("[EmacsView windowDidBecomeKey]");

  if (emacsframe != old_focus)
    dpyinfo->x_focus_frame = emacsframe;

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
  BOOL is_focus_frame = dpyinfo->x_focus_frame == emacsframe;
  NSTRACE ("[EmacsView windowDidResignKey:]");

  if (is_focus_frame)
    dpyinfo->x_focus_frame = 0;

  emacsframe->mouse_moved = 0;
  ns_frame_rehighlight (emacsframe);

  /* FIXME: for some reason needed on second and subsequent clicks away
            from sole-frame Emacs to get hollow box to show */
  if (!windowClosing && [[self window] isVisible] == YES)
    {
      x_update_cursor (emacsframe, 1);
      x_set_frame_alpha (emacsframe);
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
      [self deleteWorkingText];
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
  NSWindow *win;
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

  maximized_width = maximized_height = -1;
  nonfs_window = nil;

  ns_userRect = NSMakeRect (0, 0, 0, 0);
  r = NSMakeRect (0, 0, FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, f->text_cols),
                 FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, f->text_lines));
  [self initWithFrame: r];
  [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

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
  name = [NSString stringWithUTF8String:
                   NILP (tem) ? "Emacs" : SSDATA (tem)];
  [win setTitle: name];

  /* toolbar support */
  if (! FRAME_UNDECORATED (f))
    [self createToolbar: f];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
#ifndef NSAppKitVersionNumber10_10
#define NSAppKitVersionNumber10_10 1343
#endif

  if (NSAppKitVersionNumber >= NSAppKitVersionNumber10_10
      && FRAME_NS_APPEARANCE (f) != ns_appearance_aqua)
    win.appearance = [NSAppearance
                          appearanceNamed: NSAppearanceNameVibrantDark];
#endif

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  if ([win respondsToSelector: @selector(titlebarAppearsTransparent)])
    win.titlebarAppearsTransparent = FRAME_NS_TRANSPARENT_TITLEBAR (f);
#endif

  tem = f->icon_name;
  if (!NILP (tem))
    [win setMiniwindowTitle:
           [NSString stringWithUTF8String: SSDATA (tem)]];

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

  /* macOS Sierra automatically enables tabbed windows.  We can't
     allow this to be enabled until it's available on a Free system.
     Currently it only happens by accident and is buggy anyway. */
#if defined (NS_IMPL_COCOA) \
  && MAC_OS_X_VERSION_MAX_ALLOWED >= 101200
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101200
  if ([win respondsToSelector: @selector(setTabbingMode:)])
#endif
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
      emacsframe->left_pos = r.origin.x - NS_PARENT_WINDOW_LEFT_POS (emacsframe);
      emacsframe->top_pos =
        NS_PARENT_WINDOW_TOP_POS (emacsframe) - (r.origin.y + r.size.height);

      if (emacs_event)
        {
          emacs_event->kind = MOVE_FRAME_EVENT;
          EV_TRAILER ((id)nil);
        }
    }
}


/* Called AFTER method below, but before our windowWillResize call there leads
   to windowDidResize -> x_set_window_size.  Update emacs' notion of frame
   location so set_window_size moves the frame. */
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
   returns to original. */
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

          result = defaultFrame;  /* second click */
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
      [self updateFrameSize:YES];
      [[self window] display];
    }
  else
    [toolbar setVisible:NO];

  if (next_maximized != -1)
    [[self window] performZoom:self];
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
        b |= NSWindowCollectionBehaviorFullScreenPrimary;
      else
        b &= ~NSWindowCollectionBehaviorFullScreenPrimary;

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
#endif
        [[self window] toggleFullScreen:sender];
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
      /* Hide ghost menu bar on secondary monitor? */
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

      // to do: consider using [NSNotificationCenter postNotificationName:] to send notifications.

      [self windowWillExitFullScreen];
      [fw setFrame: [w frame] display:YES animate:ns_use_fullscreen_animation];
      [fw close];
      [w makeKeyAndOrderFront:NSApp];
      [self windowDidExitFullScreen];
      [self updateFrameSize:YES];
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


/* this gets called on toolbar button click */
- (instancetype)toolbarClicked: (id)item
{
  NSEvent *theEvent;
  int idx = [item tag] * TOOL_BAR_ITEM_NSLOTS;

  NSTRACE ("[EmacsView toolbarClicked:]");

  if (!emacs_event)
    return self;

  /* send first event (for some reason two needed) */
  theEvent = [[self window] currentEvent];
  emacs_event->kind = TOOL_BAR_EVENT;
  XSETFRAME (emacs_event->arg, emacsframe);
  EV_TRAILER (theEvent);

  emacs_event->kind = TOOL_BAR_EVENT;
/*   XSETINT (emacs_event->code, 0); */
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


- (void)drawRect: (NSRect)rect
{
  int x = NSMinX (rect), y = NSMinY (rect);
  int width = NSWidth (rect), height = NSHeight (rect);

  NSTRACE ("[EmacsView drawRect:" NSTRACE_FMT_RECT "]",
           NSTRACE_ARG_RECT(rect));

  if (!emacsframe || !emacsframe->output_data.ns)
    return;

  ns_clear_frame_area (emacsframe, x, y, width, height);
  block_input ();
  expose_frame (emacsframe, x, y, width, height);
  unblock_input ();

  /*
    drawRect: may be called (at least in Mac OS X 10.5) for invisible
    views as well for some reason.  Thus, do not infer visibility
    here.

    emacsframe->async_visible = 1;
    emacsframe->async_iconified = 0;
  */
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
  int modifiers = 0;

  NSTRACE ("[EmacsView performDragOperation:]");

  if (!emacs_event)
    return NO;

  position = [self convertPoint: [sender draggingLocation] fromView: nil];
  x = lrint (position.x);  y = lrint (position.y);

  pb = [sender draggingPasteboard];
  type = [pb availableTypeFromArray: ns_drag_types];

  if (! (op & (NSDragOperationMove|NSDragOperationDelete)) &&
      // URL drags contain all operations (0xf), don't allow all to be set.
      (op & 0xf) != 0xf)
    {
      if (op & NSDragOperationLink)
        modifiers |= NSEventModifierFlagControl;
      if (op & NSDragOperationCopy)
        modifiers |= NSEventModifierFlagOption;
      if (op & NSDragOperationGeneric)
        modifiers |= NSEventModifierFlagCommand;
    }

  modifiers = EV_MODIFIERS2 (modifiers);
  if (type == 0)
    {
      return NO;
    }
  else if ([type isEqualToString: NSFilenamesPboardType])
    {
      NSArray *files;
      NSEnumerator *fenum;
      NSString *file;

      if (!(files = [pb propertyListForType: type]))
        return NO;

      fenum = [files objectEnumerator];
      while ( (file = [fenum nextObject]) )
        {
          emacs_event->kind = DRAG_N_DROP_EVENT;
          XSETINT (emacs_event->x, x);
          XSETINT (emacs_event->y, y);
          emacs_event->modifiers = modifiers;
          emacs_event->arg =  list2 (Qfile, build_string ([file UTF8String]));
          EV_TRAILER (theEvent);
        }
      return YES;
    }
  else if ([type isEqualToString: NSURLPboardType])
    {
      NSURL *url = [NSURL URLFromPasteboard: pb];
      if (url == nil) return NO;

      emacs_event->kind = DRAG_N_DROP_EVENT;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      emacs_event->modifiers = modifiers;
      emacs_event->arg =  list2 (Qurl,
                                 build_string ([[url absoluteString]
                                                 UTF8String]));
      EV_TRAILER (theEvent);

      if ([url isFileURL] != NO)
        {
          NSString *file = [url path];
          ns_input_file = append2 (ns_input_file,
                                   build_string ([file UTF8String]));
        }
      return YES;
    }
  else if ([type isEqualToString: NSStringPboardType]
           || [type isEqualToString: NSTabularTextPboardType])
    {
      NSString *data;

      if (! (data = [pb stringForType: type]))
        return NO;

      emacs_event->kind = DRAG_N_DROP_EVENT;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      emacs_event->modifiers = modifiers;
      emacs_event->arg =  list2 (Qnil, build_string ([data UTF8String]));
      EV_TRAILER (theEvent);
      return YES;
    }
  else
    {
      fprintf (stderr, "Invalid data type in dragging pasteboard");
      return NO;
    }
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
   So let's at least stub them out until further investigation can be done. */

- (BOOL) readSelectionFromPasteboard: (NSPasteboard *)pb
{
  /* we could call ns_string_from_pasteboard(pboard) here but then it should
     be written into the buffer in place of the existing selection..
     ordinary service calls go through functions defined in ns-win.el */
  return NO;
}

- (BOOL) writeSelectionToPasteboard: (NSPasteboard *)pb types: (NSArray *)types
{
  NSArray *typesDeclared;
  Lisp_Object val;

  NSTRACE ("[EmacsView writeSelectionToPasteboard:types:]");

  /* We only support NSStringPboardType */
  if ([types containsObject:NSStringPboardType] == NO) {
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

  typesDeclared = [NSArray arrayWithObject:NSStringPboardType];
  [pb declareTypes:typesDeclared owner:nil];
  ns_string_to_pasteboard (pb, val);
  return YES;
}


/* setMini =YES means set from internal (gives a finder icon), NO means set nil
   (gives a miniaturized version of the window); currently we use the latter for
   frames whose active buffer doesn't correspond to any file
   (e.g., '*scratch*') */
- (instancetype)setMiniwindowImage: (BOOL) setMini
{
  id image = [[self window] miniwindowImage];
  NSTRACE ("[EmacsView setMiniwindowImage:%d]", setMini);

  /* NOTE: under Cocoa miniwindowImage always returns nil, documentation
     about "AppleDockIconEnabled" notwithstanding, however the set message
     below has its effect nonetheless. */
  if (image != emacsframe->output_data.ns->miniimage)
    {
      if (image && [image isKindOfClass: [EmacsImage class]])
        [image release];
      [[self window] setMiniwindowImage:
                       setMini ? emacsframe->output_data.ns->miniimage : nil];
    }

  return self;
}


- (void) setRows: (int) r andColumns: (int) c
{
  NSTRACE ("[EmacsView setRows:%d andColumns:%d]", r, c);
  rows = r;
  cols = c;
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
          const char *utfStr = SSDATA (str);
          NSString *nsStr = [NSString stringWithUTF8String: utfStr];
          return nsStr;
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
   large screen). */
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
           however neither GNUstep nor Cocoa support it very well */
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
     scrollers are on right. */
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

      /* disable optimization stopping redraw of other scrollbars */
      view->scrollbarsNeedingUpdate = 0;
      for (i =[subs count]-1; i >= 0; i--)
        if ([[subs objectAtIndex: i] isKindOfClass: [EmacsScroller class]])
          view->scrollbarsNeedingUpdate++;
      [sview addSubview: self];
    }

/*  [self setFrame: r]; */

  return self;
}


- (void)setFrame: (NSRect)newRect
{
  NSTRACE ("[EmacsScroller setFrame:]");

/*  block_input (); */
  if (horizontal)
    pixel_length = NSWidth (newRect);
  else
    pixel_length = NSHeight (newRect);
  if (pixel_length == 0) pixel_length = 1;
  min_portion = 20 / pixel_length;
  [super setFrame: newRect];
/*  unblock_input (); */
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
      /* ensure other scrollbar updates after deletion */
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
    && portion != whole; /* needed for resize empty buf */
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

/* set up emacs_event */
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


/* called manually thru timer to implement repeated button action w/hold-down */
- (instancetype)repeatScroll: (NSTimer *)scrollEntry
{
  NSEvent *e = [[self window] currentEvent];
  NSPoint p =  [[self window] mouseLocationOutsideOfEventStream];
  BOOL inKnob = [self testPart: p] == NSScrollerKnob;

  NSTRACE ("[EmacsScroller repeatScroll:]");

  /* clear timer if need be */
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
   mouseDragged events without going into a modal loop. */
- (void)mouseDown: (NSEvent *)e
{
  NSRect sr, kr;
  /* hitPart is only updated AFTER event is passed on */
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
    case NSScrollerDecrementLine:
      last_hit_part = horizontal ? scroll_bar_left_arrow : scroll_bar_up_arrow; break;
    case NSScrollerIncrementLine:
      last_hit_part = horizontal ? scroll_bar_right_arrow : scroll_bar_down_arrow; break;
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
      pos = 0;      /* ignored */

      /* set a timer to repeat, as we can't let superclass do this modally */
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


/* Called as we manually track scroller drags, rather than superclass. */
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


/* treat scrollwheel events in the bar as though they were in the main window */
- (void) scrollWheel: (NSEvent *)theEvent
{
  NSTRACE ("[EmacsScroller scrollWheel:]");

  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (frame);
  [view mouseDown: theEvent];
}

@end  /* EmacsScroller */


#ifdef NS_IMPL_GNUSTEP
/* Dummy class to get rid of startup warnings.  */
@implementation EmacsDocument

@end
#endif


/* ==========================================================================

   Font-related functions; these used to be in nsfaces.m

   ========================================================================== */


Lisp_Object
x_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
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
         in 1.43. */

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
  if (strlen (name) == 0)
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
/*fprintf (stderr, "converted '%s' to '%s'\n",xlfd,name);  */
  ret = [[NSString stringWithUTF8String: name] UTF8String];
  xfree (name);
  return ret;
}


void
syms_of_nsterm (void)
{
  NSTRACE ("syms_of_nsterm");

  ns_antialias_threshold = 10.0;

  /* from 23+ we need to tell emacs what modifiers there are.. */
  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qcontrol, "control");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");

  DEFSYM (Qfile, "file");
  DEFSYM (Qurl, "url");

  Fput (Qalt, Qmodifier_value, make_number (alt_modifier));
  Fput (Qhyper, Qmodifier_value, make_number (hyper_modifier));
  Fput (Qmeta, Qmodifier_value, make_number (meta_modifier));
  Fput (Qsuper, Qmodifier_value, make_number (super_modifier));
  Fput (Qcontrol, Qmodifier_value, make_number (ctrl_modifier));

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
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.\n\
Set to none means that the alternate / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_alternate_modifier = Qmeta;

  DEFVAR_LISP ("ns-right-alternate-modifier", ns_right_alternate_modifier,
               "This variable describes the behavior of the right alternate or option key.\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.\n\
Set to left means be the same key as `ns-alternate-modifier'.\n\
Set to none means that the alternate / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_alternate_modifier = Qleft;

  DEFVAR_LISP ("ns-command-modifier", ns_command_modifier,
               "This variable describes the behavior of the command key.\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.");
  ns_command_modifier = Qsuper;

  DEFVAR_LISP ("ns-right-command-modifier", ns_right_command_modifier,
               "This variable describes the behavior of the right command key.\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.\n\
Set to left means be the same key as `ns-command-modifier'.\n\
Set to none means that the command / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_command_modifier = Qleft;

  DEFVAR_LISP ("ns-control-modifier", ns_control_modifier,
               "This variable describes the behavior of the control key.\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.");
  ns_control_modifier = Qcontrol;

  DEFVAR_LISP ("ns-right-control-modifier", ns_right_control_modifier,
               "This variable describes the behavior of the right control key.\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.\n\
Set to left means be the same key as `ns-control-modifier'.\n\
Set to none means that the control / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_control_modifier = Qleft;

  DEFVAR_LISP ("ns-function-modifier", ns_function_modifier,
               "This variable describes the behavior of the function key (on laptops).\n\
Set to the symbol control, meta, alt, super, or hyper means it is taken to be\n\
that key.\n\
Set to none means that the function key is not interpreted by Emacs at all,\n\
allowing it to be used at a lower level for accented character entry.");
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
Only works on Mac OS X 10.6 or later.  */);
  ns_auto_hide_menu_bar = Qnil;

  DEFVAR_BOOL ("ns-use-native-fullscreen", ns_use_native_fullscreen,
     doc: /*Non-nil means to use native fullscreen on Mac OS X 10.7 and later.
Nil means use fullscreen the old (< 10.7) way.  The old way works better with
multiple monitors, but lacks tool bar.  This variable is ignored on
Mac OS X < 10.7.  Default is t.  */);
  ns_use_native_fullscreen = YES;
  ns_last_use_native_fullscreen = ns_use_native_fullscreen;

  DEFVAR_BOOL ("ns-use-fullscreen-animation", ns_use_fullscreen_animation,
     doc: /*Non-nil means use animation on non-native fullscreen.
For native fullscreen, this does nothing.
Default is nil.  */);
  ns_use_fullscreen_animation = NO;

  DEFVAR_BOOL ("ns-use-srgb-colorspace", ns_use_srgb_colorspace,
     doc: /*Non-nil means to use sRGB colorspace on Mac OS X 10.7 and later.
Note that this does not apply to images.
This variable is ignored on Mac OS X < 10.7 and GNUstep.  */);
  ns_use_srgb_colorspace = YES;

  DEFVAR_BOOL ("ns-use-mwheel-acceleration",
               ns_use_mwheel_acceleration,
     doc: /*Non-nil means use macOS's standard mouse wheel acceleration.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is t.  */);
  ns_use_mwheel_acceleration = YES;

  DEFVAR_LISP ("ns-mwheel-line-height", ns_mwheel_line_height,
               doc: /*The number of pixels touchpad scrolling considers one line.
Nil or a non-number means use the default frame line height.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is nil.  */);
  ns_mwheel_line_height = Qnil;

  DEFVAR_BOOL ("ns-use-mwheel-momentum", ns_use_mwheel_momentum,
               doc: /*Non-nil means mouse wheel scrolling uses momentum.
This variable is ignored on macOS < 10.7 and GNUstep.  Default is t.  */);
  ns_use_mwheel_momentum = YES;

  /* TODO: move to common code */
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

#ifdef NS_IMPL_COCOA
  Fprovide (Qcocoa, Qnil);
  syms_of_macfont ();
#else
  Fprovide (Qgnustep, Qnil);
  syms_of_nsfont ();
#endif

}
