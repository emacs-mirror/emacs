/* NeXT/Open/GNUstep / macOS communication module.      -*- coding: utf-8 -*-

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2026 Free Software
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
#import <GNUstepGUI/GSDisplayServer.h>
#endif

#ifdef NS_IMPL_COCOA
#include "macfont.h"
#include <Carbon/Carbon.h>
#include <IOSurface/IOSurface.h>
#endif

static EmacsMenu *dockMenu;
#ifdef NS_IMPL_COCOA
static EmacsMenu *mainMenu;
#endif

/* The last known monitor attributes list.  */
static Lisp_Object last_known_monitors;

/* ==========================================================================

   NSTRACE, Trace support.

   ========================================================================== */

#if NSTRACE_ENABLED

/* The following use "volatile" since they can be accessed from
   parallel threads.  */
volatile int nstrace_num;
volatile int nstrace_depth;

/* When 0, no trace is emitted.  This is used by NSTRACE_WHEN and
   NSTRACE_UNLESS to silence functions called.

   TODO: This should really be a thread-local variable, to avoid that
   a function with disabled trace thread silence trace output in
   another.  However, in practice this seldom is a problem.  */
volatile int nstrace_enabled_global = 1;

/* Called when nstrace_enabled goes out of scope.  */
void
nstrace_leave (int *pointer_to_nstrace_enabled)
{
  if (*pointer_to_nstrace_enabled)
    --nstrace_depth;
}


/* Called when nstrace_saved_enabled_global goes out of scope.  */
void
nstrace_restore_global_trace_state (int *pointer_to_saved_enabled_global)
{
  nstrace_enabled_global = *pointer_to_saved_enabled_global;
}


const char *
nstrace_fullscreen_type_name (int fs_type)
{
  switch (fs_type)
    {
    case -1:
      return "-1";
    case FULLSCREEN_NONE:
      return "FULLSCREEN_NONE";
    case FULLSCREEN_WIDTH:
      return "FULLSCREEN_WIDTH";
    case FULLSCREEN_HEIGHT:
      return "FULLSCREEN_HEIGHT";
    case FULLSCREEN_BOTH:
      return "FULLSCREEN_BOTH";
    case FULLSCREEN_MAXIMIZED:
      return "FULLSCREEN_MAXIMIZED";
    default:
      return "FULLSCREEN_?????";
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
  return [self colorUsingColorSpace: [NSColorSpace genericRGBColorSpace]];
}

+ (NSColor *)colorWithUnsignedLong:(unsigned long)c
{
  EmacsCGFloat a = (double)((c >> 24) & 0xff) / 255.0;
  EmacsCGFloat r = (double)((c >> 16) & 0xff) / 255.0;
  EmacsCGFloat g = (double)((c >> 8) & 0xff) / 255.0;
  EmacsCGFloat b = (double)(c & 0xff) / 255.0;

  return [NSColor colorForEmacsRed:r green:g blue:b alpha:a];
}

- (unsigned long)unsignedLong
{
  EmacsCGFloat r, g, b, a;
  [self getRed:&r green:&g blue:&b alpha:&a];

  return (((unsigned long) (a * 255)) << 24)
    | (((unsigned long) (r * 255)) << 16)
    | (((unsigned long) (g * 255)) << 8)
    | ((unsigned long) (b * 255));
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
static int ns_window_num = 0;
static BOOL gsaved = NO;
#ifdef NS_IMPL_COCOA
static BOOL ns_menu_bar_is_hidden = NO;
#endif

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
      Lisp_Object val = plist_get (modifier, kind);
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
#define EV_TRAILER(e)						\
  {								\
    XSETFRAME (emacs_event->frame_or_window, emacsframe);	\
    EV_TRAILER2 (e);						\
  }

#define EV_TRAILER2(e)                                                  \
  {                                                                     \
    if (e) emacs_event->timestamp = EV_TIMESTAMP (e);			\
    if (q_event_ptr)							\
      {									\
	Lisp_Object tem = Vinhibit_quit;				\
	Vinhibit_quit = Qt;						\
	n_emacs_events_pending++;					\
	kbd_buffer_store_event_hold (emacs_event, q_event_ptr);		\
	Vinhibit_quit = tem;						\
      }									\
    else								\
      hold_event (emacs_event);						\
    EVENT_INIT (*emacs_event);						\
    ns_send_appdefined (-1);						\
  }


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
ns_relocate (const char *epath)
/* If we're running in a self-contained app bundle some hard-coded
   paths are relative to the root of the bundle, so work out the full
   path.

   FIXME: I think this should be able to handle cases where multiple
   directories are separated by colons.  */
{
#ifdef NS_SELF_CONTAINED
  NSBundle *bundle = [NSBundle mainBundle];
  NSString *root = [bundle bundlePath];
  NSString *original = [NSString stringWithUTF8String:epath];
  NSString *fixedPath = [NSString pathWithComponents:
                                    [NSArray arrayWithObjects:
                                               root, original, nil]];
  NSFileManager *fileManager = [NSFileManager defaultManager];

  if (![original isAbsolutePath]
      && [fileManager fileExistsAtPath:fixedPath isDirectory:NULL])
    return [fixedPath UTF8String];

  /* If we reach here either the path is absolute and therefore we
     don't need to complete it, or we're unable to relocate the
     file/directory.  If it's the latter it may be because the user is
     trying to use a bundled app as though it's a Unix style install
     and we have no way to guess what was intended, so return the
     original string unaltered.  */

#endif

  return epath;
}


void
ns_init_pool (void)
/* Initialize the 'outerpool' autorelease pool.  This should be called
   from main before any Objective C code is run.  */
{
  outerpool = [[NSAutoreleasePool alloc] init];
}


void
ns_init_locale (void)
/* macOS doesn't set any environment variables for the locale when run
   from the GUI. Get the locale from the OS and set LANG.  */
{
  NSTRACE ("ns_init_locale");

  /* Either use LANG, if set, or try to construct LANG from
     NSLocale.  */
  const char *lang = getenv ("LANG");
  if (lang == NULL || *lang == 0)
    {
      const NSLocale *locale = [NSLocale currentLocale];
      const NSString *localeID = [NSString stringWithFormat:@"%@.UTF-8",
					   [locale localeIdentifier]];
      lang = [localeID UTF8String];
    }

  /* Check if LANG can be used for initializing the locale.  If not,
     use a default setting.  Note that Emacs's main will undo the
     setlocale below, initializing the locale from the
     environment.  */
  if (setlocale (LC_ALL, lang) == NULL)
    {
      const char *const default_lang = "en_US.UTF-8";
      fprintf (stderr, "LANG=%s cannot be used, using %s instead.\n",
	       lang, default_lang);
      lang = default_lang;
    }

  setenv ("LANG", lang, 1);
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

#if defined (NS_IMPL_COCOA) && !defined (MAC_OS_X_VERSION_10_7)
      parentRect.origin = [[parentView window] convertBaseToScreen:parentRect.origin];
#elif defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if ([[parentView window]
             respondsToSelector:@selector(convertRectToScreen:)])
        parentRect = [[parentView window] convertRectToScreen:parentRect];
      else
        parentRect.origin = [[parentView window] convertBaseToScreen:parentRect.origin];
#else
      parentRect = [[parentView window] convertRectToScreen:parentRect];
#endif
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
#if defined (NS_IMPL_GNUSTEP) || !defined (MAC_OS_X_VERSION_10_7)
  return [[FRAME_NS_VIEW (f) window] userSpaceScaleFactor];
#elif MAC_OS_X_VERSION_MIN_REQUIRED < 1070
  if ([[FRAME_NS_VIEW (f) window]
            respondsToSelector:@selector(backingScaleFactor:)])
    return [[FRAME_NS_VIEW (f) window] backingScaleFactor];
  else
    return [[FRAME_NS_VIEW (f) window] userSpaceScaleFactor];
#else
  return [[FRAME_NS_VIEW (f) window] backingScaleFactor];
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

  ns_updating_frame = f;
  [view lockFocus];
}


static void
ns_update_end (struct frame *f)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for whole frame, called after gui_update_window_end
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);

  NSTRACE_WHEN (NSTRACE_GROUP_UPDATES, "ns_update_end");

/*   if (f == MOUSE_HL_INFO (f)->mouse_face_mouse_frame) */
  MOUSE_HL_INFO (f)->mouse_face_defer = 0;

  block_input ();

  [view unlockFocus];
#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
  [[view window] flushWindow];
#endif

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
      EmacsView *view = FRAME_NS_VIEW (f);
      [view lockFocus];
    }

  /* clipping */
  if (r)
    {
      NSGraphicsContext *ctx = [NSGraphicsContext currentContext];
      [ctx saveGraphicsState];
#ifdef NS_IMPL_COCOA
      if (n == 2)
        NSRectClipList (r, 2);
      else
        NSRectClip (*r);
#else
      GSRectClipList (ctx, r, n);
#endif
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
      EmacsView *view = FRAME_NS_VIEW (f);
      [view unlockFocus];
#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
      [[view window] flushWindow];
#endif
    }
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
#if NS_IMPL_GNUSTEP && !HAVE_DECL_NSIMAGENAMECAUTION
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

#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MIN_REQUIRED < 101400
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
      if (make_key && !f->no_accept_focus)
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
      EmacsWindow *window = (EmacsWindow *)[view window];

      SET_FRAME_VISIBLE (f, true);
      ns_raise_frame (f, ! FRAME_NO_FOCUS_ON_MAP (f));

      /* Making a new frame from a fullscreen frame will make the new frame
         fullscreen also.  So skip handleFS as this will print an error.  */
      if ([view fsIsNative] && [view isFullscreen])
        {
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
          block_input ();
          [window setParentChildRelationships];
          unblock_input ();

          /* If the parent frame moved while the child frame was
             invisible, the child frame's position won't have been
             updated.  Make sure it's in the right place now.  */
          ns_set_offset(f, f->left_pos, f->top_pos, 0);
        }
    }
}


void
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
  SET_FRAME_VISIBLE (f, false);
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
  [view removeFromSuperview];
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
  if (FRAME_PARENT_FRAME (f))
    {
      NSWindow *child = [FRAME_NS_VIEW (f) window];
      NSWindow *parent;

      /* Pacify a incorrect GCC warning about FRAME_PARENT_FRAME (f)
	 being NULL. */
      if (FRAME_PARENT_FRAME (f))
	parent = [FRAME_NS_VIEW (FRAME_PARENT_FRAME (f)) window];
      else
	emacs_abort ();

      [parent removeChildWindow: child];
    }

  [[FRAME_NS_VIEW (f) window] close];
  ns_free_frame_resources (f);
  ns_window_num--;
}

static NSPoint
compute_offset (struct frame *f, NSView *view, int xoff, int yoff)
{
  /* If there is no parent frame then just convert to screen
     coordinates, UNLESS we have negative values, in which case I
     think it's best to position from the bottom and right of the
     current screen rather than the main screen or whole display.  */

  NSRect parentRect = ns_parent_window_rect (f);
  NSRect windowFrame = [[view window] frame];
  NSPoint topLeft;

  if (f->size_hint_flags & XNegative)
    topLeft.x = NSMaxX (parentRect) - NSWidth (windowFrame) + xoff;
  else if (FRAME_PARENT_FRAME (f))
    topLeft.x = NSMinX (parentRect) + xoff;
  else
    topLeft.x = xoff;

  if (f->size_hint_flags & YNegative)
    topLeft.y = NSMinY (parentRect) + NSHeight (windowFrame) - yoff;
  else if (FRAME_PARENT_FRAME (f))
    topLeft.y = NSMaxY (parentRect) - yoff;
  else
    topLeft.y = NSMaxY ([[[NSScreen screens] objectAtIndex:0] frame]) - yoff;

#ifdef NS_IMPL_GNUSTEP
  /* Don't overlap the menu.

     FIXME: Surely there's a better way than just hardcoding 100 in
     here?  */
  if (topLeft.x < 100)
    topLeft.x = 100;
#endif

  return topLeft;
}

void
ns_set_offset (struct frame *f, int xoff, int yoff, int change_grav)
/* --------------------------------------------------------------------------
     External: Position the window
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);

  NSTRACE ("ns_set_offset");

  if (view == nil)
    return;

  block_input ();

  NSPoint topLeft = compute_offset (f, view, xoff, yoff);
  NSTRACE_POINT ("setFrameTopLeftPoint", topLeft);
  [[view window] setFrameTopLeftPoint:topLeft];
  f->size_hint_flags &= ~(XNegative|YNegative);

  unblock_input ();
}

static void
ns_set_window_size (struct frame *f, bool change_gravity,
                    int width, int height)
/* --------------------------------------------------------------------------
     Adjust window pixel size based on native sizes WIDTH and HEIGHT.
     Impl is a bit more complex than other terms, need to do some
     internal clipping.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSWindow *window = [view window];
  NSRect frameRect;

  NSTRACE ("ns_set_window_size");

  if (view == nil)
    return;

  NSTRACE_RECT ("current", [window frame]);
  NSTRACE_MSG ("Width:%d Height:%d", width, height);
  NSTRACE_MSG ("Font %d x %d", FRAME_COLUMN_WIDTH (f), FRAME_LINE_HEIGHT (f));

  block_input ();

  frameRect = [window frameRectForContentRect:NSMakeRect (0, 0, width, height)];

  /* Set the origin so the top left of the frame doesn't move.  */
  frameRect.origin = [window frame].origin;
  frameRect.origin.y += NSHeight ([view frame]) - height;

  if (f->output_data.ns->zooming)
    f->output_data.ns->zooming = 0;

  /* Usually it seems safe to delay changing the frame size, but when a
     series of actions are taken with no redisplay between them then we
     can end up using old values so don't delay here.  */
  change_frame_size (f, width, height, false, NO, false);

  [window setFrame:frameRect display:NO];

  unblock_input ();
}

static void
ns_set_window_size_and_position (struct frame *f,
				 int width, int height)
/* --------------------------------------------------------------------------
   Adjust window pixelwise size and position in one operation.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSWindow *window = [view window];

  NSTRACE ("ns_set_window_size_and_position");

  if (view == nil)
    return;

  block_input ();

  /* Both window frame origin, and the rect that setFrame accepts are
     anchored to the bottom-left corner of the window.  */
  NSPoint topLeft = compute_offset (f, view, f->left_pos, f->top_pos);
  NSPoint bottomLeft = NSMakePoint(topLeft.x,
				   /* text-area pixels + decorations. */
				   topLeft.y - (height
						+ FRAME_NS_TITLEBAR_HEIGHT(f)
						+ FRAME_TOOLBAR_HEIGHT(f)));
  NSRect frameRect = [window frameRectForContentRect:NSMakeRect (bottomLeft.x,
								 bottomLeft.y,
								 width,
								 height)];
  if (f->output_data.ns->zooming)
    f->output_data.ns->zooming = 0;
  /* Usually it seems safe to delay changing the frame size, but when a
     series of actions are taken with no redisplay between them then we
     can end up using old values so don't delay here.  */
  change_frame_size (f, width, height, false, NO, false);
  [window setFrame:frameRect display:NO];
  f->size_hint_flags &= ~(XNegative|YNegative);

  unblock_input ();
}

void
ns_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* --------------------------------------------------------------------------
     Set frame F's `undecorated' parameter.  If non-nil, F's window-system
     window is drawn without decorations, title, minimize/maximize boxes
     and external borders.  This usually means that the window cannot be
     dragged, resized, iconified, maximized or deleted with the mouse.  If
     nil, draw the frame with all the elements listed above unless these
     have been suspended via window manager settings.
   -------------------------------------------------------------------------- */
{
  NSTRACE ("ns_set_undecorated");

  if (!EQ (new_value, old_value))
    {
      EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
      NSWindow *oldWindow = [view window];
      NSWindow *newWindow;

      block_input ();

      FRAME_UNDECORATED (f) = !NILP (new_value);

      newWindow = [[EmacsWindow alloc] initWithEmacsFrame:f];

      if ([oldWindow isKeyWindow])
        [newWindow makeKeyAndOrderFront:NSApp];

      [newWindow setIsVisible:[oldWindow isVisible]];
      if ([oldWindow isMiniaturized])
        [newWindow miniaturize:NSApp];

      [oldWindow close];

      unblock_input ();
    }
}

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

  NSTRACE ("ns_set_parent_frame");

  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_NS_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      error ("Invalid specification of `parent-frame'");
    }

  fset_parent_frame (f, new_value);

  block_input ();
  [(EmacsWindow *)[FRAME_NS_VIEW (f) window] setParentChildRelationships];
  unblock_input ();
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
ns_set_appearance_1 (struct frame *f, Lisp_Object new_value)
{
  if (EQ (new_value, Qdark))
    FRAME_NS_APPEARANCE (f) = ns_appearance_vibrant_dark;
  else if (EQ (new_value, Qlight))
    FRAME_NS_APPEARANCE (f) = ns_appearance_aqua;
  else
    FRAME_NS_APPEARANCE (f) = ns_appearance_system_default;
}

void
ns_set_appearance (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);
  EmacsWindow *window = (EmacsWindow *)[view window];

  NSTRACE ("ns_set_appearance");

  if (NSAppKitVersionNumber < NSAppKitVersionNumber10_10)
    return;

  ns_set_appearance_1 (f, new_value);

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

  block_input ();
  [view handleFS];
  unblock_input ();
}

/* ==========================================================================

    Color management

   ========================================================================== */


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

static void
ns_query_color (void *col, Emacs_Color *color_def)
/* --------------------------------------------------------------------------
         Get ARGB values out of NSColor col and put them into color_def
         and set color_def pixel to the ARGB color.
   -------------------------------------------------------------------------- */
{
  EmacsCGFloat r, g, b, a;

  [((NSColor *)col) getRed: &r green: &g blue: &b alpha: &a];
  color_def->red   = r * 65535;
  color_def->green = g * 65535;
  color_def->blue  = b * 65535;

  color_def->pixel = [(NSColor *)col unsignedLong];
}

bool
ns_defined_color (struct frame *f,
                  const char *name,
                  Emacs_Color *color_def,
                  bool alloc,
                  bool _makeIndex)
/* --------------------------------------------------------------------------
         Return true if named color found, and set color_def rgb accordingly.
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
  ns_query_color (col, color_def);
  unblock_input ();
  return 1;
}

static void
ns_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
/* --------------------------------------------------------------------------
     External (hook): Store F's background color into *BGCOLOR
   -------------------------------------------------------------------------- */
{
  ns_query_color (FRAME_BACKGROUND_COLOR (f), bgcolor);
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

  {
    EmacsView *view = FRAME_NS_VIEW (f);
    [[view window] setAlphaValue: alpha];
  }
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

#ifdef NS_IMPL_COCOA
  CGPoint mouse_pos =
    CGPointMake(f->left_pos + pix_x,
                f->top_pos + pix_y +
                FRAME_NS_TITLEBAR_HEIGHT(f) + FRAME_TOOLBAR_HEIGHT(f));
  CGWarpMouseCursorPosition (mouse_pos);
#else
  GSDisplayServer *server = GSServerForWindow ([FRAME_NS_VIEW (f) window]);
  [server setMouseLocation: NSMakePoint (f->left_pos + pix_x,
					 f->top_pos + pix_y
					 + FRAME_NS_TITLEBAR_HEIGHT(f)
					 + FRAME_TOOLBAR_HEIGHT(f))
		  onScreen: [[[FRAME_NS_VIEW (f) window] screen] screenNumber]];
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
  bool return_no_frame_flag = false;
#ifdef NS_IMPL_COCOA
  NSPoint screen_position;
  NSInteger window_number;
  NSWindow *w;
#endif

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

  screen_position = [NSEvent mouseLocation];
  window_number = 0;

  do
    {
      window_number = [NSWindow windowNumberAtPoint: screen_position
                        belowWindowWithWindowNumber: window_number];
      w = [NSApp windowWithWindowNumber: window_number];

      if ((EQ (track_mouse, Qdrag_source)
	   || EQ (track_mouse, Qdropping))
	  && w && [[w delegate] isKindOfClass: [EmacsTooltip class]])
	continue;

      if (w && [[w delegate] isKindOfClass: [EmacsView class]])
        f = ((EmacsView *) [w delegate])->emacsframe;
      else if (EQ (track_mouse, Qdrag_source))
	break;

      if (f && (EQ (track_mouse, Qdrag_source)
		|| EQ (track_mouse, Qdropping))
	  && FRAME_TOOLTIP_P (f))
	continue;
    }
  while (window_number > 0 && !f);
#endif

  if (!f)
    {
      f = (dpyinfo->ns_focus_frame
	   ? dpyinfo->ns_focus_frame : SELECTED_FRAME ());
      return_no_frame_flag = EQ (track_mouse, Qdrag_source);
    }

  if (!FRAME_NS_P (f))
    f = NULL;

  if (f && FRAME_TOOLTIP_P (f))
    f = dpyinfo->last_mouse_frame;

  /* While dropping, use the last mouse frame only if there is no
     currently focused frame.  */
  if (!f && (EQ (track_mouse, Qdropping)
	     || EQ (track_mouse, Qdrag_source))
      && dpyinfo->last_mouse_frame
      && FRAME_LIVE_P (dpyinfo->last_mouse_frame))
    {
      f = dpyinfo->last_mouse_frame;
      return_no_frame_flag = EQ (track_mouse, Qdrag_source);
    }

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
      *fp = return_no_frame_flag ? NULL : f;
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
  snprintf (value, 16, "%d", keysym);
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


#ifdef NS_IMPL_GNUSTEP
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
  [[NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND
			    (FACE_FROM_ID (f, DEFAULT_FACE_ID))] set];
  NSRectFill (r);
  ns_unfocus (f);

#ifdef NS_IMPL_GNUSTEP
  ns_redraw_scroll_bars (f);
#endif
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
  [[NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)] set];

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
    NSPoint dest = NSMakePoint (x, to_y);
    EmacsView *view = FRAME_NS_VIEW (f);

    [view copyRect:srcRect to:dest];
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED < 101400
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
      int border = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      int margin = FRAME_TOP_MARGIN_HEIGHT (f);
      int bottom_margin = FRAME_BOTTOM_MARGIN_HEIGHT (f);
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
      [[NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)] set];
      NSRectFill (NSMakeRect (0, margin, width, border));
      NSRectFill (NSMakeRect (0, 0, border, height));
      NSRectFill (NSMakeRect (0, margin, width, border));
      NSRectFill (NSMakeRect (width - border, 0, border, height));
      NSRectFill (NSMakeRect (0, height - bottom_margin - border,
			      width, border));
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

          [[NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)] set];
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
  NSPoint dest = NSMakePoint (x+shift_by, y);

  NSTRACE ("ns_shift_glyphs_for_insert");

  [FRAME_NS_VIEW (f) copyRect:srcRect to:dest];
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
  if (s->cmp == NULL
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      struct font_metrics metrics;

      if (s->first_glyph->type == CHAR_GLYPH)
	{
	  struct font *font = s->font;
	  font->driver->text_extents (font, s->char2b, s->nchars, &metrics);
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);

	  composition_gstring_width (gstring, s->cmp_from, s->cmp_to, &metrics);
	}
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? - metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}



/* ==========================================================================

    Fringe and cursor drawing

   ========================================================================== */

static NSMutableDictionary *fringe_bmp;

static void
ns_define_fringe_bitmap (int which, unsigned short *bits, int h, int w)
{
  NSBezierPath *p = [NSBezierPath bezierPath];

  if (!fringe_bmp)
    fringe_bmp = [[NSMutableDictionary alloc] initWithCapacity:25];

  uint8_t *points = alloca ((h + 1) * (w + 1) * 4);
  uint8_t *cur = points;

  /* Find all the outgoing edges in a clockwise path.  That is, we only
     want to list the edges leaving a point, not the ones entering a
     point, so we don't double count them.  */
  for (int y = 0; y < h + 1; y++)
    for (int x = 0; x < w + 1; x++)
      {
        int nw = 0, ne = 0, se = 0, sw = 0;
        if (x != 0 && y != 0)
          nw = bits[y-1] & (1 << (w - x));

        if (x != 0 && y < h)
          sw = bits[y] & (1 << (w - x));

        if (x < w && y < h)
          se = bits[y] & (1 << (w - x - 1));

        if (x < w && y != 0)
          ne = bits[y-1] & (1 << (w - x - 1));

        cur[0] = !nw && ne; /* North.  */
        cur[1] = !ne && se; /* East.  */
        cur[2] = !se && sw; /* South.  */
        cur[3] = !sw && nw; /* West.  */
        cur += 4;
      }

  /* Find all the points with edges and trace them out.  */
  int v = 0;
  char last = 0;
  while (v < (h + 1) * (w + 1) * 4)
    {
      char this = 0;
      int x = (v/4) % (w+1);
      int y = (v/4) / (w+1);

      if (points[v+3])
        {
          /* West.  */
          points[v+3] = 0;
          v = v - 4;
          this = 'w';
        }
      else if (points[v+1])
        {
          /* East.  */
          points[v+1] = 0;
          v = v + 4;
          this = 'e';
        }
      else if (points[v+2])
        {
          /* South.  */
          points[v+2] = 0;
          v = ((y+1)*(w+1) + x) * 4;
          this = 's';
        }
      else if (points[v])
        {
          /* North.  */
          points[v] = 0;
          v = ((y-1)*(w+1) + x) * 4;
          this = 'n';
        }
      else
        {
          /* No edge.  */
          v = v + 4;

          if (last)
            {
              /* If we reach here we were tracing a shape but have run
                 out of edges, so we must be back to the start (or
                 something's gone wrong).  */
              [p closePath];
              last = 0;
            }
        }

      if (this)
        {
          /* If we've found an edge we now need to either move to that
             point (if it's the start of a shape) or draw a line from
             the last corner to this point, but only if it's a
             corner.  */
          if (!last)
            [p moveToPoint:NSMakePoint (x, y)];
          else if (last && last != this)
	    [p lineToPoint:NSMakePoint (x, y)];
          last = this;
        }
    }

  [fringe_bmp setObject:p forKey:[NSNumber numberWithInt:which]];
}


static void
ns_destroy_fringe_bitmap (int which)
{
  [fringe_bmp removeObjectForKey:[NSNumber numberWithInt:which]];
}


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
  NSRect bmpRect = NSZeroRect;
  NSRect rowRect = ns_row_rect (w, row, ANY_AREA);

  NSTRACE_WHEN (NSTRACE_GROUP_FRINGE, "ns_draw_fringe_bitmap");
  NSTRACE_MSG ("which:%d cursor:%d overlay:%d width:%d height:%d period:%d",
               p->which, p->cursor_p, p->overlay_p, p->wd, p->h, p->dh);

  /* Work out the rectangle we will need to clear.  */
  bmpRect = NSMakeRect (p->x, p->y, p->wd, p->h);

  if (p->bx >= 0)
    bmpRect = NSUnionRect (bmpRect, NSMakeRect (p->bx, p->by, p->nx, p->ny));

  /* Handle partially visible rows.  */
  bmpRect = NSIntersectionRect (bmpRect, rowRect);

  /* Clip to the bitmap's area.  */
  ns_focus (f, &bmpRect, 1);

  /* Clear screen unless overlay.  */
  if (!p->overlay_p && !NSIsEmptyRect (bmpRect))
    {
      NSTRACE_RECT ("clearRect", bmpRect);

      [[NSColor colorWithUnsignedLong:face->background] set];
      NSRectFill (bmpRect);
    }

  NSBezierPath *bmp = [fringe_bmp objectForKey:[NSNumber numberWithInt:p->which]];

  if (bmp == nil
      && p->which < max_used_fringe_bitmap)
    {
      gui_define_fringe_bitmap (f, p->which);
      bmp = [fringe_bmp objectForKey: [NSNumber numberWithInt: p->which]];
    }

  if (bmp)
    {
      NSAffineTransform *transform = [NSAffineTransform transform];
      NSColor *bm_color;

      /* Because the image is defined at (0, 0) we need to take a copy
         and then transform that copy to the new origin.  */
      bmp = [bmp copy];
      [transform translateXBy:p->x yBy:p->y - p->dh];
      [bmp transformUsingAffineTransform:transform];

      if (!p->cursor_p)
        bm_color = [NSColor colorWithUnsignedLong:face->foreground];
      else if (p->overlay_p)
        bm_color = [NSColor colorWithUnsignedLong:face->background];
      else
        bm_color = f->output_data.ns->cursor_color;

      [bm_color set];
      [bmp fill];

      [bmp release];
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
  NSRect r;
  int fx, fy, h, cursor_height;
  struct frame *f = WINDOW_XFRAME (w);
  struct glyph *phys_cursor_glyph;
  struct glyph *cursor_glyph;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */

  NSTRACE ("ns_draw_window_cursor (on = %d, cursor_type = %d)",
	   on_p, cursor_type);

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
      NSTRACE_MSG ("No phys cursor glyph was found!");

      if (glyph_row->exact_window_width_line_p
          && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
        {
          glyph_row->cursor_in_fringe_p = 1;
          draw_fringe_bitmap (w, glyph_row, 0);
        }
      return;
    }

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

      /* If the character under cursor is R2L, draw the bar cursor
         on the right of its glyph, rather than on the left.  */
      cursor_glyph = get_phys_cursor_glyph (w);
      if ((cursor_glyph->resolved_level & 1) != 0)
        fx += cursor_glyph->pixel_width - w->phys_cursor_width;
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

  ns_focus (f, NULL, 0);

  NSGraphicsContext *ctx = [NSGraphicsContext currentContext];
  [ctx saveGraphicsState];
#ifdef NS_IMPL_GNUSTEP
  GSRectClipList (ctx, &r, 1);
#else
  NSRectClip (r);
#endif

  [FRAME_CURSOR_COLOR (f) set];

  switch (cursor_type)
    {
    case DEFAULT_CURSOR:
    case NO_CURSOR:
      break;
    case FILLED_BOX_CURSOR:
      /* The call to draw_phys_cursor_glyph can end up undoing the
	 ns_focus, so unfocus here and regain focus later.  */
      [ctx restoreGraphicsState];
      ns_unfocus (f);
      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
      ns_focus (f, &r, 1);
      break;
    case HOLLOW_BOX_CURSOR:
      /* This works like it does in PostScript, not X Windows.  */
      [NSBezierPath strokeRect: NSInsetRect (r, 0.5, 0.5)];
      [ctx restoreGraphicsState];
      break;
    case HBAR_CURSOR:
    case BAR_CURSOR:
      NSRectFill (r);
      [ctx restoreGraphicsState];
      break;
    }

  ns_unfocus (f);
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
    [[NSColor colorWithUnsignedLong:face->foreground] set];

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
      [[NSColor colorWithUnsignedLong:color_first] set];
      NSRectFill(NSMakeRect (x0, y0, 1, y1 - y0));
      [[NSColor colorWithUnsignedLong:color] set];
      NSRectFill(NSMakeRect (x0 + 1, y0, x1 - x0 - 2, y1 - y0));
      [[NSColor colorWithUnsignedLong:color_last] set];
      NSRectFill(NSMakeRect (x1 - 1, y0, 1, y1 - y0));
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first and
       last pixels differently.  */
    {
      [[NSColor colorWithUnsignedLong:color_first] set];
      NSRectFill(NSMakeRect (x0, y0, x1 - x0, 1));
      [[NSColor colorWithUnsignedLong:color] set];
      NSRectFill(NSMakeRect (x0, y0 + 1, x1 - x0, y1 - y0 - 2));
      [[NSColor colorWithUnsignedLong:color_last] set];
      NSRectFill(NSMakeRect (x0, y1 - 1, x1 - x0, 1));
    }
  else
    {
      /* In any other case do not draw the first and last pixels
         differently.  */
      [[NSColor colorWithUnsignedLong:color] set];
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

/* Draw a dashed underline of thickness THICKNESS and width WIDTH onto
   the focused frame at a vertical offset of OFFSET from the position of
   the glyph string S, with each segment SEGMENT pixels in length.  */

static void
ns_draw_dash (struct glyph_string *s, int width, int segment,
	      int offset, int thickness)
{
  CGFloat pattern[2], y_center = s->ybase + offset + thickness / 2.0;
  NSBezierPath *path = [[NSBezierPath alloc] init];

  pattern[0] = segment;
  pattern[1] = segment;

  [path setLineDash: pattern count: 2 phase: (CGFloat) s->x];
  [path setLineWidth: thickness];
  [path moveToPoint: NSMakePoint (s->x, y_center)];
  [path lineToPoint: NSMakePoint (s->x + width, y_center)];
  [path stroke];
  [path release];
}

/* Draw an underline of STYLE onto the focused frame at an offset of
   POSITION from the baseline of the glyph string S, S->WIDTH in length,
   and THICKNESS in height.  */

static void
ns_fill_underline (struct glyph_string *s, enum face_underline_type style,
		   int position, int thickness)
{
  int segment;
  NSRect rect;

  segment = thickness * 3;

  switch (style)
    {
      /* FACE_UNDERLINE_DOUBLE_LINE is treated identically to SINGLE, as
	 the second line will be filled by another invocation of this
	 function.  */
    case FACE_UNDERLINE_SINGLE:
    case FACE_UNDERLINE_DOUBLE_LINE:
      rect = NSMakeRect (s->x, s->ybase + position, s->width, thickness);
      NSRectFill (rect);
      break;

    case FACE_UNDERLINE_DOTS:
      segment = thickness;
      FALLTHROUGH;

    case FACE_UNDERLINE_DASHES:
      ns_draw_dash (s, s->width, segment, position, thickness);
      break;

    case FACE_NO_UNDERLINE:
    case FACE_UNDERLINE_WAVE:
    default:
      emacs_abort ();
    }
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

  if (s->hl == DRAW_CURSOR)
    [FRAME_BACKGROUND_COLOR (s->f) set];
  else
    [defaultCol set];

  /* Do underline.  */
  if (face->underline)
    {
      if (s->face->underline == FACE_UNDERLINE_WAVE)
        {
          if (!face->underline_defaulted_p)
            [[NSColor colorWithUnsignedLong:face->underline_color] set];

          ns_draw_underwave (s, width, x);
        }
      else if (face->underline >= FACE_UNDERLINE_SINGLE)
        {
          unsigned long thickness, position;

          /* If the prev was underlined, match its appearance.  */
          if (s->prev
	      && (s->prev->face->underline != FACE_UNDERLINE_WAVE
		  && s->prev->face->underline >= FACE_UNDERLINE_SINGLE)
              && s->prev->underline_thickness > 0
	      && (s->prev->face->underline_at_descent_line_p
		  == s->face->underline_at_descent_line_p)
	      && (s->prev->face->underline_pixels_above_descent_line
		  == s->face->underline_pixels_above_descent_line))
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
	      underline_at_descent_line = (!(NILP (val) || EQ (val, Qunbound))
					   || s->face->underline_at_descent_line_p);

	      val = (WINDOW_BUFFER_LOCAL_VALUE
		     (Qx_use_underline_position_properties, s->w));
	      use_underline_position_properties
		= !(NILP (val) || EQ (val, Qunbound));

              /* Use underline thickness of font, defaulting to 1.  */
              thickness = (font && font->underline_thickness > 0)
                ? font->underline_thickness : 1;

              /* Determine the offset of underlining from the baseline.  */
              if (underline_at_descent_line)
                position = (descent - thickness
			    - s->face->underline_pixels_above_descent_line);
              else if (use_underline_position_properties
                       && font && font->underline_position >= 0)
                position = font->underline_position;
              else if (font)
                position = lround (font->descent / 2);
              else
                position = minimum_offset;

	      if (!s->face->underline_pixels_above_descent_line)
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

          if (!face->underline_defaulted_p)
            [[NSColor colorWithUnsignedLong:face->underline_color] set];

	  ns_fill_underline (s, s->face->underline, position,
			     thickness);

	  /* Place a second underline above the first if this was
	     requested in the face specification.  */

	  if (s->face->underline == FACE_UNDERLINE_DOUBLE_LINE)
	    {
	      /* Compute the position of the second underline.  */
	      position = position - thickness - 1;
	      ns_fill_underline (s, s->face->underline, position,
				 thickness);
	    }
        }
    }
  /* Do overline. We follow other terms in using a thickness of 1
     and ignoring overline_margin.  */
  if (face->overline_p)
    {
      NSRect r;
      r = NSMakeRect (x, s->y, width, 1);

      if (!face->overline_color_defaulted_p)
        [[NSColor colorWithUnsignedLong:face->overline_color] set];

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

      if (!face->strike_through_color_defaulted_p)
        [[NSColor colorWithUnsignedLong:face->strike_through_color] set];

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

/* Set up colors for the relief lines around glyph string S.  */

static void
ns_setup_relief_colors (struct glyph_string *s)
{
  struct ns_output *di = FRAME_OUTPUT_DATA (s->f);
  NSColor *color;

  if (s->face->use_box_color_for_shadows_p)
    color = [NSColor colorWithUnsignedLong: s->face->box_color];
  else
    color = [NSColor colorWithUnsignedLong: s->face->background];

  if (s->hl == DRAW_CURSOR)
    color = FRAME_CURSOR_COLOR (s->f);

  if (color == nil)
    color = [NSColor grayColor];

  if (color != di->relief_background_color)
    {
      [di->relief_background_color release];
      di->relief_background_color = [color retain];
      [di->light_relief_color release];
      di->light_relief_color = [[color highlightWithLevel: 0.4] retain];
      [di->dark_relief_color release];
      di->dark_relief_color = [[color shadowWithLevel: 0.4] retain];
    }
}

static void
ns_draw_relief (NSRect outer, int hthickness, int vthickness, char raised_p,
		char top_p, char bottom_p, char left_p, char right_p,
		struct glyph_string *s)
/* --------------------------------------------------------------------------
    Draw a relief rect inside r, optionally leaving some sides open.
    Note we can't just use an NSDrawBezel command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  NSRect inner;
  NSBezierPath *p = nil;

  NSTRACE ("ns_draw_relief");

  /* set up colors */
  ns_setup_relief_colors (s);

  /* Calculate the inner rectangle.  */
  inner = outer;

  if (left_p)
    {
      inner.origin.x += vthickness;
      inner.size.width -= vthickness;
    }

  if (right_p)
    inner.size.width -= vthickness;

  if (top_p)
    {
      inner.origin.y += hthickness;
      inner.size.height -= hthickness;
    }

  if (bottom_p)
    inner.size.height -= hthickness;

  struct ns_output *di = FRAME_OUTPUT_DATA (s->f);

  [(raised_p ? di->light_relief_color : di->dark_relief_color) set];

  if (top_p || left_p)
    {
      p = [NSBezierPath bezierPath];

      [p moveToPoint: NSMakePoint (NSMinX (outer), NSMinY (outer))];
      if (top_p)
        {
          [p lineToPoint: NSMakePoint (NSMaxX (outer), NSMinY (outer))];
          [p lineToPoint: NSMakePoint (NSMaxX (inner), NSMinY (inner))];
        }
      [p lineToPoint: NSMakePoint (NSMinX (inner), NSMinY (inner))];
      if (left_p)
        {
          [p lineToPoint: NSMakePoint (NSMinX (inner), NSMaxY (inner))];
          [p lineToPoint: NSMakePoint (NSMinX (outer), NSMaxY (outer))];
        }
      [p closePath];
      [p fill];
    }

  [(raised_p ? di->dark_relief_color : di->light_relief_color) set];

  if (bottom_p || right_p)
    {
      p = [NSBezierPath bezierPath];

      [p moveToPoint: NSMakePoint (NSMaxX (outer), NSMaxY (outer))];
      if (right_p)
        {
          [p lineToPoint: NSMakePoint (NSMaxX (outer), NSMinY (outer))];
          [p lineToPoint: NSMakePoint (NSMaxX (inner), NSMinY (inner))];
        }
      [p lineToPoint:NSMakePoint (NSMaxX (inner), NSMaxY (inner))];
      if (bottom_p)
        {
          [p lineToPoint: NSMakePoint (NSMinX (inner), NSMaxY (inner))];
          [p lineToPoint: NSMakePoint (NSMinX (outer), NSMaxY (outer))];
        }
      [p closePath];
      [p fill];
    }

  /* If one of h/vthickness are more than 1, draw the outermost line
     on the respective sides in the black relief color.  */

  if (p)
    [p removeAllPoints];
  else
    p = [NSBezierPath bezierPath];

  if (hthickness > 1 && top_p)
    {
      [p moveToPoint: NSMakePoint (NSMinX (outer),
				   NSMinY (outer) + 0.5)];
      [p lineToPoint: NSMakePoint (NSMaxX (outer),
				   NSMinY (outer) + 0.5)];
    }

  if (hthickness > 1 && bottom_p)
    {
      [p moveToPoint: NSMakePoint (NSMinX (outer),
				   NSMaxY (outer) - 0.5)];
      [p lineToPoint: NSMakePoint (NSMaxX (outer),
				   NSMaxY (outer) - 0.5)];
    }

  if (vthickness > 1 && left_p)
    {
      [p moveToPoint: NSMakePoint (NSMinX (outer) + 0.5,
				   NSMinY (outer) + 0.5)];
      [p lineToPoint: NSMakePoint (NSMinX (outer) + 0.5,
				   NSMaxY (outer) - 0.5)];
    }

  if (vthickness > 1 && left_p)
    {
      [p moveToPoint: NSMakePoint (NSMinX (outer) + 0.5,
				   NSMinY (outer) + 0.5)];
      [p lineToPoint: NSMakePoint (NSMinX (outer) + 0.5,
				   NSMaxY (outer) - 0.5)];
    }

  [di->dark_relief_color set];
  [p stroke];

  if (vthickness > 1 && hthickness > 1)
    {
      [FRAME_BACKGROUND_COLOR (s->f) set];

      if (left_p && top_p)
	[NSBezierPath fillRect: NSMakeRect (NSMinX (outer),
					    NSMinY (outer),
					    1, 1)];

      if (right_p && top_p)
	[NSBezierPath fillRect: NSMakeRect (NSMaxX (outer) - 1,
					    NSMinY (outer),
					    1, 1)];

      if (right_p && bottom_p)
	[NSBezierPath fillRect: NSMakeRect (NSMaxX (outer) - 1,
					    NSMaxY (outer) - 1,
					    1, 1)];

      if (left_p && bottom_p)
	[NSBezierPath fillRect: NSMakeRect (NSMinX (outer),
					    NSMaxY (outer) - 1,
					    1, 1)];
    }
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
  struct face *face = s->face;

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
                   [NSColor colorWithUnsignedLong:face->box_color],
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
  struct face *face = s->face;
  NSRect r;

  NSTRACE ("ns_maybe_dumpglyphs_background");

  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_horizontal_line_width, 0);

      if (s->stippled_p)
	{
	  struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (s->f);
#ifdef NS_IMPL_COCOA
	  /* On cocoa emacs the stipple is stored as a mask CGImage.
	     First we want to clear the background with the bg color.  */
	  [[NSColor colorWithUnsignedLong:face->background] set];
	  r = NSMakeRect (s->x, s->y + box_line_width,
			  s->background_width,
			  s->height - 2 * box_line_width);
	  NSRectFill (r);
	  s->background_filled_p = 1;
	  CGImageRef mask
	    = [dpyinfo->bitmaps[face->stipple - 1].img stippleMask];

	  /* This part could possibly be improved, the author is
	     unfamiliar with NS/CoreGraphics and isn't sure if it's
	     possible to do this with NSImage */
	  NSGraphicsContext *ctx = [NSGraphicsContext currentContext];
	  [ctx saveGraphicsState];
	  /* Checkpoint the graphics state and then focus in on the area
	     we're going to fill */
	  CGContextRef context = [ctx CGContext];
	  CGContextClipToRect (context, r);
	  CGContextScaleCTM (context, 1, -1);

	  /* Stamp the foreground color using the stipple mask */
	  [[NSColor colorWithUnsignedLong:face->foreground] set];
	  CGRect imageSize = CGRectMake (0, 0, CGImageGetWidth (mask),
					 CGImageGetHeight (mask));
	  CGContextDrawTiledImage (context, imageSize, mask);

	  [[NSGraphicsContext currentContext] restoreGraphicsState];
#else
	  [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
	  goto fill;
#endif /* NS_IMPL_COCOA */

	}
      else if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       /* When xdisp.c ignores FONT_HEIGHT, we cannot trust font
		  dimensions, since the actual glyphs might be much
		  smaller.  So in that case we always clear the
		  rectangle with background color.  */
	       || FONT_TOO_HIGH (s->font)
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  if (s->hl != DRAW_CURSOR)
	    [(NS_FACE_BACKGROUND (face) != 0
	      ? [NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)]
	      : FRAME_BACKGROUND_COLOR (s->f)) set];
	  else if (face && (NS_FACE_BACKGROUND (face)
			    == [(NSColor *) FRAME_CURSOR_COLOR (s->f)
					    unsignedLong]))
	    [[NSColor colorWithUnsignedLong:NS_FACE_FOREGROUND (face)] set];
	  else
	    [FRAME_CURSOR_COLOR (s->f) set];

#ifndef NS_IMPL_COCOA
	fill:
#endif /* !NS_IMPL_COCOA */
	  r = NSMakeRect (s->x, s->y + box_line_width,
			  s->background_width,
			  s->height - 2 * box_line_width);
	  NSRectFill (r);
	  s->background_filled_p = 1;
	}
    }
}

static void
ns_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      if (s->face->id == TAB_BAR_FACE_ID)
	thick = (tab_bar_button_relief < 0
		 ? DEFAULT_TAB_BAR_BUTTON_RELIEF
		 : min (tab_bar_button_relief, 1000000));
      else
	thick = (tool_bar_button_relief < 0
		 ? DEFAULT_TOOL_BAR_BUTTON_RELIEF
		 : min (tool_bar_button_relief, 1000000));
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x1 = x + s->slice.width - 1;
  y1 = y + s->slice.height - 1;

  extra_x = extra_y = 0;
  if (s->face->id == TAB_BAR_FACE_ID)
    {
      if (CONSP (Vtab_bar_button_margin)
	  && FIXNUMP (XCAR (Vtab_bar_button_margin))
	  && FIXNUMP (XCDR (Vtab_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtab_bar_button_margin)) - thick;
	  extra_y = XFIXNUM (XCDR (Vtab_bar_button_margin)) - thick;
	}
      else if (FIXNUMP (Vtab_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtab_bar_button_margin) - thick;
    }

  if (s->face->id == TOOL_BAR_FACE_ID)
    {
      if (CONSP (Vtool_bar_button_margin)
	  && FIXNUMP (XCAR (Vtool_bar_button_margin))
	  && FIXNUMP (XCDR (Vtool_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtool_bar_button_margin));
	  extra_y = XFIXNUM (XCDR (Vtool_bar_button_margin));
	}
      else if (FIXNUMP (Vtool_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtool_bar_button_margin);
    }

  top_p = bot_p = left_p = right_p = false;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = true;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = true;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = true;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = true;

  ns_draw_relief (NSMakeRect (x, y, x1 - x + 1, y1 - y + 1), thick,
		  thick, raised_p, top_p, bot_p, left_p, right_p, s);
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
  NSRect br;
  struct face *face = s->face;
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

  [[NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)] set];

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
      tdCol = [NSColor colorWithUnsignedLong: NS_FACE_BACKGROUND (face)];
    }
  else
    tdCol = [NSColor colorWithUnsignedLong: NS_FACE_FOREGROUND (face)];

  /* Draw underline, overline, strike-through.  */
  ns_draw_text_decoration (s, face, tdCol, br.size.width, br.origin.x);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    ns_draw_image_relief (s);

  /* If there is no mask, the background won't be seen, so draw a
     rectangle on the image for the cursor.  Do this for all images,
     getting transparency right is not reliable.  */
  if (s->hl == DRAW_CURSOR)
    {
      int thickness = abs (s->img->relief);
      if (thickness == 0) thickness = 1;
      ns_draw_box (br, thickness, thickness,
		   FRAME_CURSOR_COLOR (s->f), 1, 1);
    }
}


static void
ns_draw_stretch_glyph_string (struct glyph_string *s)
{
  struct face *face;
  NSColor *fg_color;

  if (s->hl == DRAW_CURSOR && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right (s->w, TEXT_AREA);

	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}

      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      if (s->hl == DRAW_CURSOR)
	[FRAME_CURSOR_COLOR (s->f) set];
      else
	[[NSColor colorWithUnsignedLong: s->face->foreground] set];

      NSRectFill (NSMakeRect (x, s->y, width, s->height));

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;

	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      face = FACE_FROM_ID_OR_NULL (s->f,
					   MOUSE_HL_INFO (s->f)->mouse_face_face_id);

	      if (!face)
		face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
	      prepare_face_for_display (s->f, face);

	      [[NSColor colorWithUnsignedLong: face->background] set];
	    }
	  else
	    [[NSColor colorWithUnsignedLong: s->face->background] set];
	  NSRectFill (NSMakeRect (x, y, w, h));
	}
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, text_left_x = window_box_left (s->w, TEXT_AREA);

      /* Don't draw into left fringe or scrollbar area except for
         header line and mode line.  */
      if (s->area == TEXT_AREA
	  && x < text_left_x && !s->row->mode_line_p)
	{
	  background_width -= text_left_x - x;
	  x = text_left_x;
	}

      if (!s->row->stipple_p)
	s->row->stipple_p = s->stippled_p;

      if (background_width > 0)
	{
	  struct ns_display_info *dpyinfo;

	  dpyinfo = FRAME_DISPLAY_INFO (s->f);
	  if (s->hl == DRAW_CURSOR)
	    [FRAME_CURSOR_COLOR (s->f) set];
	  else if (s->stippled_p)
	    {
#ifdef NS_IMPL_COCOA
	      /* On cocoa emacs the stipple is stored as a mask CGImage.
		 First we want to clear the background with the bg
		 color.  */
	      [[NSColor colorWithUnsignedLong:s->face->background] set];
	      NSRectFill (NSMakeRect (x, s->y, background_width, s->height));

	      /* This part could possibly be improved, the author is
		 unfamiliar with NS/CoreGraphics and isn't sure if it's
		 possible to do this with NSImage.  */
	      CGImageRef mask = [dpyinfo->bitmaps[s->face->stipple - 1].img stippleMask];
	      CGRect bounds = CGRectMake (s->x, s->y, s->background_width, s->height);

	      /* Checkpoint the graphics state and then focus in on the
		 area we're going to fill.  */
	      NSGraphicsContext *ctx = [NSGraphicsContext currentContext];
	      [ctx saveGraphicsState];
	      CGContextRef context = [ctx CGContext];
	      CGContextClipToRect (context, bounds);
	      CGContextScaleCTM (context, 1, -1);

	      /* Stamp the foreground color using the stipple mask.  */
	      [[NSColor colorWithUnsignedLong:s->face->foreground] set];
	      CGRect imageSize = CGRectMake (0, 0, CGImageGetWidth (mask),
					     CGImageGetHeight (mask));
	      CGContextDrawTiledImage (context, imageSize, mask);
	      [[NSGraphicsContext currentContext] restoreGraphicsState];
#else
	      [[dpyinfo->bitmaps[s->face->stipple - 1].img stippleMask] set];
#endif /* NS_IMPL_COCOA */
	    }
	  else
	    [[NSColor colorWithUnsignedLong: s->face->background] set];

	  NSRectFill (NSMakeRect (x, s->y, background_width, s->height));
	}
    }

  /* Draw overlining, etc. on the stretch glyph (or the part of the
     stretch glyph after the cursor).  If the glyph has a box, then
     decorations will be drawn after drawing the box in
     ns_draw_glyph_string, in order to prevent them from being
     overwritten by the box.  */
  if (s->face->box == FACE_NO_BOX)
    {
      fg_color = [NSColor colorWithUnsignedLong:
			    NS_FACE_FOREGROUND (s->face)];
      ns_draw_text_decoration (s, s->face, fg_color,
			       s->background_width, s->x);
    }
}

static void
ns_draw_glyph_string_foreground (struct glyph_string *s)
{
  int x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  font->driver->draw
    (s, s->cmp_from, s->nchars, x, s->ybase,
     !s->for_overlaps && !s->background_filled_p);
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

/* Draw the foreground of glyph string S for glyphless characters.  */
static void
ns_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  NSGlyph char2b[8];
  int x, i, j;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  s->char2b = char2b;

  for (i = 0; i < s->nchars; i++, glyph++)
    {
#ifdef GCC_LINT
      enum { PACIFY_GCC_BUG_81401 = 1 };
#else
      enum { PACIFY_GCC_BUG_81401 = 0 };
#endif
      char buf[8 + PACIFY_GCC_BUG_81401];
      char *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 0
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (CONSP (acronym))
		acronym = XCAR (acronym);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  unsigned int ch = glyph->u.glyphless.ch;
	  eassume (ch <= MAX_CHAR);
	  snprintf (buf, 8, "%0*X", ch < 0x10000 ? 4 : 6, ch);
	  str = buf;
	}

      if (str)
	{
	  int upper_len = (len + 1) / 2;

	  /* It is assured that all LEN characters in STR is ASCII.  */
	  for (j = 0; j < len; j++)
            char2b[j] = s->font->driver->encode_char (s->font, str[j]) & 0xFFFF;
	  s->font->driver->draw (s, 0, upper_len,
				 x + glyph->slice.glyphless.upper_xoff,
				 s->ybase + glyph->slice.glyphless.upper_yoff,
				 false);
	  s->font->driver->draw (s, upper_len, len,
				 x + glyph->slice.glyphless.lower_xoff,
				 s->ybase + glyph->slice.glyphless.lower_yoff,
				 false);
	}
      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
        ns_draw_box (NSMakeRect (x, s->ybase - glyph->ascent,
                                 glyph->pixel_width - 1,
                                 glyph->ascent + glyph->descent - 1),
                     1, 1,
                     [NSColor colorWithUnsignedLong:NS_FACE_FOREGROUND (s->face)],
                     YES, YES);
      x += glyph->pixel_width;
   }

  /* GCC 12 complains even though nothing ever uses s->char2b after
     this function returns.  */
  s->char2b = NULL;
}

/* Transfer glyph string parameters from S's face to S itself.
   Set S->stipple_p as appropriate, taking the draw type into
   account.  */

static void
ns_set_glyph_string_gc (struct glyph_string *s)
{
  prepare_face_for_display (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      /* s->gc = s->face->gc; */
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      /* x_set_mode_line_face_gc (s); */
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      /* x_set_cursor_gc (s); */
      s->stippled_p = false;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      /* x_set_mouse_face_gc (s); */
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      /* s->gc = s->face->gc; */
      s->stippled_p = s->face->stipple != 0;
    }
  else
    emacs_abort ();
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

  NSTRACE ("ns_draw_glyph_string (hl = %u)", s->hl);

  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
          {
	    ns_set_glyph_string_gc (next);
	    n = ns_get_glyph_string_clip_rect (s->next, r);
	    ns_focus (s->f, r, n);
            if (next->first_glyph->type != STRETCH_GLYPH)
	      ns_maybe_dumpglyphs_background (s->next, 1);
	    else
	      ns_draw_stretch_glyph_string (s->next);
	    ns_unfocus (s->f);
            next->num_clips = 0;
          }
    }

  ns_set_glyph_string_gc (s);

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

  n = ns_get_glyph_string_clip_rect (s, r);

  if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
      && !s->clip_tail
      && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	  || (s->next && s->next->hl != s->hl && s->right_overhang)))
    r[0] = NSIntersectionRect (r[0], NSMakeRect (s->x, s->y, s->width, s->height));

  ns_focus (s->f, r, n);

  switch (s->first_glyph->type)
    {

    case IMAGE_GLYPH:
      ns_dumpglyphs_image (s, r[0]);
      break;

    case XWIDGET_GLYPH:
      x_draw_xwidget_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      ns_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
    case COMPOSITE_GLYPH:
      {
	BOOL isComposite = s->first_glyph->type == COMPOSITE_GLYPH;
	if (s->for_overlaps || (isComposite
				&& (s->cmp_from > 0
				    && ! s->first_glyph->u.cmp.automatic)))
	  s->background_filled_p = 1;
	else
	  ns_maybe_dumpglyphs_background
	    (s, s->first_glyph->type == COMPOSITE_GLYPH);

	if (isComposite)
	  ns_draw_composite_glyph_string_foreground (s);
	else
	  ns_draw_glyph_string_foreground (s);

	{
	  NSColor *col = (NS_FACE_FOREGROUND (s->face) != 0
			  ? [NSColor colorWithUnsignedLong:NS_FACE_FOREGROUND (s->face)]
			  : FRAME_FOREGROUND_COLOR (s->f));

	  /* Draw underline, overline, strike-through. */
	  ns_draw_text_decoration (s, s->face, col, s->width, s->x);
	}
      }

      break;

    case GLYPHLESS_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
        s->background_filled_p = 1;
      else
        ns_maybe_dumpglyphs_background
          (s, s->first_glyph->type == COMPOSITE_GLYPH);
      ns_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  /* Draw box if not done already.  */
  if (!s->for_overlaps && !box_drawn_p && s->face->box != FACE_NO_BOX)
    ns_dumpglyphs_box_or_relief (s);

  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->type == STRETCH_GLYPH)
    {
      NSColor *fg_color;

      fg_color = [NSColor colorWithUnsignedLong: NS_FACE_FOREGROUND (s->face)];

      ns_draw_text_decoration (s, s->face, fg_color,
			       s->background_width, s->x);
    }

  ns_unfocus (s->f);

  /* Draw surrounding overhangs. */
  if (s->prev)
    {
      ns_focus (s->f, NULL, 0);
      struct glyph_string *prev;

      for (prev = s->prev; prev; prev = prev->prev)
	if (prev->hl != s->hl
	    && prev->x + prev->width + prev->right_overhang > s->x)
	  {
	    /* As prev was drawn while clipped to its own area, we
	       must draw the right_overhang part using s->hl now.  */
	    enum draw_glyphs_face save = prev->hl;

	    prev->hl = s->hl;
	    NSRect r = NSMakeRect (s->x, s->y, s->width, s->height);
	    NSRect rc;
	    get_glyph_string_clip_rect (s, &rc);
	    [[NSGraphicsContext currentContext] saveGraphicsState];
	    NSRectClip (r);
	    if (n)
	      NSRectClip (rc);
#ifdef NS_IMPL_GNUSTEP
	    DPSgsave ([NSGraphicsContext currentContext]);
	    DPSrectclip ([NSGraphicsContext currentContext], s->x, s->y,
			 s->width, s->height);
	    DPSrectclip ([NSGraphicsContext currentContext], NSMinX (rc),
			 NSMinY (rc), NSWidth (rc), NSHeight (rc));
#endif
	    if (prev->first_glyph->type == CHAR_GLYPH)
	      ns_draw_glyph_string_foreground (prev);
	    else
	      ns_draw_composite_glyph_string_foreground (prev);
#ifdef NS_IMPL_GNUSTEP
	    DPSgrestore ([NSGraphicsContext currentContext]);
#endif
	    [[NSGraphicsContext currentContext] restoreGraphicsState];
	    prev->hl = save;
	  }
      ns_unfocus (s->f);
    }

  if (s->next)
    {
      ns_focus (s->f, NULL, 0);
      struct glyph_string *next;

      for (next = s->next; next; next = next->next)
	if (next->hl != s->hl
	    && next->x - next->left_overhang < s->x + s->width)
	  {
	    /* As next will be drawn while clipped to its own area,
	       we must draw the left_overhang part using s->hl now.  */
	    enum draw_glyphs_face save = next->hl;

	    next->hl = s->hl;
	    NSRect r = NSMakeRect (s->x, s->y, s->width, s->height);
	    NSRect rc;
	    get_glyph_string_clip_rect (s, &rc);
	    [[NSGraphicsContext currentContext] saveGraphicsState];
	    NSRectClip (r);
	    NSRectClip (rc);
#ifdef NS_IMPL_GNUSTEP
	    DPSgsave ([NSGraphicsContext currentContext]);
	    DPSrectclip ([NSGraphicsContext currentContext], s->x, s->y,
			 s->width, s->height);
	    DPSrectclip ([NSGraphicsContext currentContext], NSMinX (rc),
			 NSMinY (rc), NSWidth (rc), NSHeight (rc));
#endif
	    if (next->first_glyph->type == CHAR_GLYPH)
	      ns_draw_glyph_string_foreground (next);
	    else
	      ns_draw_composite_glyph_string_foreground (next);
#ifdef NS_IMPL_GNUSTEP
	    DPSgrestore ([NSGraphicsContext currentContext]);
#endif
	    [[NSGraphicsContext currentContext] restoreGraphicsState];
	    next->hl = save;
	    next->clip_head = s->next;
	  }
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
check_native_fs (void)
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
ns_read_socket_1 (struct terminal *terminal, struct input_event *hold_quit,
		  BOOL no_release)
/* --------------------------------------------------------------------------
     External (hook): Post an event to ourself and keep reading events until
     we read it back again.  In effect process all events which were waiting.
     From 21+ we have to manage the event buffer ourselves.

     NO_RELEASE means not to touch the global autorelease pool.
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

      if (!no_release)
	{
	  /* We manage autorelease pools by allocate/reallocate each time around
	     the loop; strict nesting is occasionally violated but seems not to
	     matter... earlier methods using full nesting caused major memory leaks.  */
	  [outerpool release];
	  outerpool = [[NSAutoreleasePool alloc] init];
	}

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

static int
ns_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  return ns_read_socket_1 (terminal, hold_quit, NO);
}


static int
ns_select_1 (int nfds, fd_set *readfds, fd_set *writefds,
	     fd_set *exceptfds, struct timespec *timeout,
	     sigset_t *sigmask, BOOL run_loop_only)
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

  /* If there are input events pending, store them so that Emacs can
     recognize C-g.  (And we must make sure [NSApp run] is called in
     this function, so that C-g has a chance to land in
     hold_event_q.)  */
  if (hold_event_q.nr > 0)
    {
      for (int i = 0; i < hold_event_q.nr; ++i)
        kbd_buffer_store_event_hold (&hold_event_q.q[i], NULL);
      hold_event_q.nr = 0;
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
    return thread_select (pselect, nfds, readfds, writefds,
			  exceptfds, timeout, sigmask);
  else
    {
      struct timespec t = {0, 1};
      thread_select (pselect, 0, NULL, NULL, NULL, &t, sigmask);
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

int
ns_select (int nfds, fd_set *readfds, fd_set *writefds,
	   fd_set *exceptfds, struct timespec *timeout,
	   sigset_t *sigmask)
{
  return ns_select_1 (nfds, readfds, writefds, exceptfds,
		      timeout, sigmask, NO);
}

#ifdef HAVE_PTHREAD
void
ns_run_loop_break (void)
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
  width = WINDOW_SCROLL_BAR_AREA_WIDTH (window);
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
  height = WINDOW_SCROLL_BAR_AREA_HEIGHT (window);
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
                         WINDOW_SCROLL_BAR_AREA_WIDTH (window), height);

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

#ifdef NS_IMPL_GNUSTEP
static void
ns_update_window_end (struct window *w, bool cursor_on_p,
		      bool mouse_face_overwritten_p)
{
  NSTRACE ("ns_update_window_end (cursor_on_p = %d)", cursor_on_p);

  ns_redraw_scroll_bars (WINDOW_XFRAME (w));
}
#endif

static void
ns_flush_display (struct frame *f)
{
  struct input_event ie;

  EVENT_INIT (ie);
  ns_read_socket_1 (FRAME_TERMINAL (f), &ie, YES);
}

/* This and next define (many of the) public functions in this
   file.  */
/* gui_* are generic versions in xdisp.c that we, and other terms, get
   away with using despite presence in the "system dependent"
   redisplay interface.  In addition, many of the ns_ methods have
   code that is shared with all terms, indicating need for further
   refactoring.  */
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
#ifndef NS_IMPL_GNUSTEP
  NULL, /* update_window_end   */
#else
  ns_update_window_end,
#endif
  ns_flush_display,
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  ns_draw_fringe_bitmap,
  ns_define_fringe_bitmap,
  ns_destroy_fringe_bitmap,
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

#ifdef NS_IMPL_COCOA
static void
ns_displays_reconfigured (CGDirectDisplayID display,
			  CGDisplayChangeSummaryFlags flags,
			  void *user_info)
{
  struct input_event ie;
  union buffered_input_event *ev;
  Lisp_Object new_monitors;

  EVENT_INIT (ie);

  new_monitors = Fns_display_monitor_attributes_list (Qnil);

  if (!NILP (Fequal (new_monitors, last_known_monitors)))
    return;

  last_known_monitors = new_monitors;

  ev = (kbd_store_ptr == kbd_buffer
	? kbd_buffer + KBD_BUFFER_SIZE - 1
	: kbd_store_ptr - 1);

  if (kbd_store_ptr != kbd_fetch_ptr
      && ev->ie.kind == MONITORS_CHANGED_EVENT)
    return;

  ie.kind = MONITORS_CHANGED_EVENT;
  XSETTERMINAL (ie.arg, x_display_list->terminal);

  kbd_buffer_store_event (&ie);
}
#endif

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

#ifdef NS_IMPL_COCOA
  /* Rather than try to clean up the NS environment we can just
     disable the app and leave it waiting for any new frames.  */
  [NSApp setActivationPolicy:NSApplicationActivationPolicyProhibited];
#endif

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
  terminal->set_window_size_and_position_hook = ns_set_window_size_and_position;
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
  terminal->change_tab_bar_height_hook = ns_change_tab_bar_height;
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

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
  /* Disable problematic event processing on macOS 26 (Tahoe) to avoid
     scrolling lag and input handling issues.  These are undocumented
     options as of macOS 26.0.  */
  [NSUserDefaults.standardUserDefaults
      registerDefaults:@{@"NSEventConcurrentProcessingEnabled" : @"NO",
        @"NSApplicationUpdateCycleEnabled" : @"NO"}];
#endif

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

  gui_init_fringe (terminal->rif);

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
            c |= 0xFF000000;
            [cl setColor:
                  [NSColor colorWithUnsignedLong:c]
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

#ifdef NS_IMPL_COCOA
  /* Begin listening for display reconfiguration, so we can run the
     appropriate hooks.  FIXME: is this called when the resolution of
     a monitor changes?  */

  CGDisplayRegisterReconfigurationCallback (ns_displays_reconfigured,
					    NULL);
#endif
  last_known_monitors = Fns_display_monitor_attributes_list (Qnil);

  NSTRACE_MSG ("ns_term_init done");

  unblock_input ();

  return dpyinfo;
}


void
ns_term_shutdown (int sig)
{
  NSAutoreleasePool *pool;
  /* We also need an autorelease pool here, since this can be called
     during dumping.  */
  pool = [[NSAutoreleasePool alloc] init];
  [[NSUserDefaults standardUserDefaults] synchronize];
  [pool release];

  /* code not reached in emacs.c after this is called by shut_down_emacs: */
  if (STRINGP (Vauto_save_list_file_name))
    unlink (SSDATA (Vauto_save_list_file_name));

  if (sig == 0 || sig == SIGTERM)
    [NSApp terminate: NSApp];
  else /* Force a stack trace to happen.  */
    emacs_abort ();
}


/* ==========================================================================

    EmacsApp implementation

   ========================================================================== */


@implementation EmacsApp

- (id)init
{
#ifdef NS_IMPL_GNUSTEP
  NSNotificationCenter *notification_center;
#endif

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

#ifdef NS_IMPL_GNUSTEP
  notification_center = [NSNotificationCenter defaultCenter];
  [notification_center addObserver: self
			  selector: @selector(updateMonitors:)
			      name: NSApplicationDidChangeScreenParametersNotification
			    object: nil];
#endif

  return self;
}

#ifdef NS_IMPL_COCOA
- (void)run
{
  NSTRACE ("[EmacsApp run]");

#ifndef NSAppKitVersionNumber10_9
#define NSAppKitVersionNumber10_9 1265
#endif

  if ((int) NSAppKitVersionNumber != NSAppKitVersionNumber10_9)
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

#ifdef NS_IMPL_GNUSTEP
- (void) updateMonitors: (NSNotification *) notification
{
  struct input_event ie;
  union buffered_input_event *ev;
  Lisp_Object new_monitors;

  EVENT_INIT (ie);

  new_monitors = Fns_display_monitor_attributes_list (Qnil);

  if (!NILP (Fequal (new_monitors, last_known_monitors)))
    return;

  last_known_monitors = new_monitors;

  ev = (kbd_store_ptr == kbd_buffer
	? kbd_buffer + KBD_BUFFER_SIZE - 1
	: kbd_store_ptr - 1);

  if (kbd_store_ptr != kbd_fetch_ptr
      && ev->ie.kind == MONITORS_CHANGED_EVENT)
    return;

  ie.kind = MONITORS_CHANGED_EVENT;
  XSETTERMINAL (ie.arg, x_display_list->terminal);

  kbd_buffer_store_event (&ie);
}
#endif

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

- (BOOL) applicationSupportsSecureRestorableState: (NSApplication *)app
{
  return YES;
}

- (void) terminate: (id)sender
{
  struct input_event ie;
  struct frame *f;

  NSTRACE ("[EmacsApp terminate:]");

  f = SELECTED_FRAME ();
  EVENT_INIT (ie);

  ie.kind = NS_NONKEY_EVENT;
  ie.code = KEY_NS_POWER_OFF;
  ie.arg = Qt; /* mark as non-key event */
  XSETFRAME (ie.frame_or_window, f);

  kbd_buffer_store_event (&ie);
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
          result = pselect (selfds[0]+1, &fds, NULL, NULL, NULL, NULL);
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

static Lisp_Object
ns_font_desc_to_font_spec (NSFontDescriptor *desc, NSFont *font)
{
  NSFontSymbolicTraits traits = [desc symbolicTraits];
  NSDictionary *dict = [desc objectForKey: NSFontTraitsAttribute];
  NSString *family = [font familyName];
  Lisp_Object lwidth, lslant, lweight, lheight;
  NSNumber *tem;

  lwidth = Qnil;
  lslant = Qnil;
  lweight = Qnil;
  lheight = Qnil;

  if (traits & NSFontBoldTrait)
    lweight = Qbold;

  if (traits & NSFontItalicTrait)
    lslant = Qitalic;

  if (traits & NSFontCondensedTrait)
    lwidth = Qcondensed;
  else if (traits & NSFontExpandedTrait)
    lwidth = Qexpanded;

  if (dict != nil)
    {
      tem = [dict objectForKey: NSFontSlantTrait];

      if (tem != nil)
	lslant = ([tem floatValue] > 0
		  ? Qitalic : ([tem floatValue] < 0
			       ? Qreverse_italic
			       : Qnormal));

      tem = [dict objectForKey: NSFontWeightTrait];

#ifdef NS_IMPL_GNUSTEP
      if (tem != nil)
	lweight = ([tem floatValue] > 0
		   ? Qbold : ([tem floatValue] < -0.4f
			      ? Qlight : Qnormal));
#else
      if (tem != nil)
	{
	  if ([tem floatValue] >= 0.4)
	    lweight = Qbold;
	  else if ([tem floatValue] >= 0.24)
	    lweight = Qmedium;
	  else if ([tem floatValue] >= 0)
	    lweight = Qnormal;
	  else if ([tem floatValue] >= -0.24)
	    lweight = Qsemi_light;
	  else
	    lweight = Qlight;
	}
#endif

      tem = [dict objectForKey: NSFontWidthTrait];

      if (tem != nil)
	lwidth = ([tem floatValue] > 0
		  ? Qexpanded : ([tem floatValue] < 0
				 ? Qcondensed : Qnormal));
    }

  lheight = make_float ([font pointSize]);

  return CALLN (Ffont_spec,
		QCwidth, lwidth, QCslant, lslant,
		QCweight, lweight, QCsize, lheight,
		QCfamily, (family
			   ? [family lispString]
			   : Qnil));
}

#ifdef NS_IMPL_COCOA
static NSView *
ns_create_font_panel_buttons (id target, SEL select, SEL cancel_action)
{
  NSMatrix *matrix;
  NSButtonCell *prototype;
  NSSize cell_size;
  NSRect frame;
  NSButtonCell *cancel, *ok;

  prototype = [[NSButtonCell alloc] init];
  [prototype setBezelStyle: NSBezelStyleRounded];
  [prototype setTitle: @"Cancel"];
  cell_size = [prototype cellSize];
  frame = NSMakeRect (0, 0, cell_size.width * 2,
		      cell_size.height);
  matrix = [[NSMatrix alloc] initWithFrame: frame
				      mode: NSTrackModeMatrix
				 prototype: prototype
			      numberOfRows: 1
			   numberOfColumns: 2];
  [prototype release];

  ok = (NSButtonCell *) [matrix cellAtRow: 0 column: 0];
  cancel = (NSButtonCell *) [matrix cellAtRow: 0 column: 1];

  [ok setTitle: @"OK"];
  [ok setTarget: target];
  [ok setAction: select];
  [ok setButtonType: NSButtonTypeMomentaryPushIn];

  [cancel setTitle: @"Cancel"];
  [cancel setTarget: target];
  [cancel setAction: cancel_action];
  [cancel setButtonType: NSButtonTypeMomentaryPushIn];

  [matrix selectCell: ok];

  return matrix;
}
#endif

/* ==========================================================================

    EmacsView implementation

   ========================================================================== */


@implementation EmacsView

- (void)windowDidEndLiveResize:(NSNotification *)notification
{
  [self updateFramePosition];
}

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

  if (fs_state == FULLSCREEN_BOTH)
    [nonfs_window release];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
  /* Release layer and menu */
  EmacsLayer *layer = (EmacsLayer *)[self layer];
  [layer release];
#endif

  [[self menu] release];
  [super dealloc];
}


/* Called on font panel selection.  */
- (void) changeFont: (id) sender
{
  struct font *font = FRAME_OUTPUT_DATA (emacsframe)->font;
  NSFont *nsfont;

#ifdef NS_IMPL_GNUSTEP
  nsfont = ((struct nsfont_info *) font)->nsfont;
#else
  nsfont = (NSFont *) macfont_get_nsctfont (font);
#endif

  if (!font_panel_active)
    return;

  if (font_panel_result)
    [font_panel_result release];

  font_panel_result = (NSFont *) [sender convertFont: nsfont];

  if (font_panel_result)
    [font_panel_result retain];

#ifndef NS_IMPL_COCOA
  font_panel_active = NO;
  [NSApp stop: self];
#endif
}

#ifdef NS_IMPL_COCOA
- (void) noteUserSelectedFont
{
  font_panel_active = NO;

  /* If no font was previously selected, use the currently selected
     font.  */

  if (!font_panel_result && FRAME_FONT (emacsframe))
    {
      font_panel_result
	= macfont_get_nsctfont (FRAME_FONT (emacsframe));

      if (font_panel_result)
	[font_panel_result retain];
    }

  [NSApp stop: self];
}

- (void) noteUserCancelledSelection
{
  font_panel_active = NO;

  if (font_panel_result)
    [font_panel_result release];
  font_panel_result = nil;

  [NSApp stop: self];
}
#endif

- (Lisp_Object) showFontPanel
{
  id fm = [NSFontManager sharedFontManager];
  struct font *font = FRAME_OUTPUT_DATA (emacsframe)->font;
  NSFont *nsfont, *result;
  struct timespec timeout;
#ifdef NS_IMPL_COCOA
  NSView *buttons;
  BOOL canceled;
#endif

#ifdef NS_IMPL_GNUSTEP
  nsfont = ((struct nsfont_info *) font)->nsfont;
#else
  nsfont = (NSFont *) macfont_get_nsctfont (font);
#endif

#ifdef NS_IMPL_COCOA
  buttons
    = ns_create_font_panel_buttons (self,
				    @selector (noteUserSelectedFont),
				    @selector (noteUserCancelledSelection));
  [[fm fontPanel: YES] setAccessoryView: buttons];
  [buttons release];
#endif

  [fm setSelectedFont: nsfont isMultiple: NO];
  [fm orderFrontFontPanel: NSApp];

  font_panel_active = YES;
  timeout = make_timespec (0, 100000000);

  block_input ();
  while (font_panel_active
#ifdef NS_IMPL_COCOA
	 && (canceled = [[fm fontPanel: YES] isVisible])
#else
	 && [[fm fontPanel: YES] isVisible]
#endif
	 )
    ns_select_1 (0, NULL, NULL, NULL, &timeout, NULL, YES);
  unblock_input ();

  if (font_panel_result)
    [font_panel_result autorelease];

#ifdef NS_IMPL_COCOA
  if (!canceled)
    font_panel_result = nil;
#endif

  result = font_panel_result;
  font_panel_result = nil;

  [[fm fontPanel: YES] setIsVisible: NO];
  font_panel_active = NO;

  if (result)
    return ns_font_desc_to_font_spec ([result fontDescriptor],
				      result);

  return Qnil;
}

- (BOOL)acceptsFirstResponder
{
  NSTRACE ("[EmacsView acceptsFirstResponder]");
  return YES;
}

/* Tell NS we want to accept clicks that activate the window */
- (BOOL)acceptsFirstMouse:(NSEvent *)theEvent
{
  NSTRACE_MSG ("First mouse event: type=%ld, clickCount=%ld",
               [theEvent type], [theEvent clickCount]);
  return ns_click_through;
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
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
  if ([currentCursor respondsToSelector: @selector(setOnMouseEntered:)])
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
  if ([theEvent type] != NSEventTypeKeyDown)
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

  if (!hlinfo->mouse_face_hidden
      && FIXNUMP (Vmouse_highlight)
      && !EQ (emacsframe->tab_bar_window, hlinfo->mouse_face_window))
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_hidden = true;
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

#ifndef NS_IMPL_GNUSTEP
      if (NS_KEYLOG)
	fprintf (stderr,
		 "keyDown: code = %x\tfnKey = %x\tflags = %x\tmods = "
		 "%x\n",
		 (unsigned int) code, (unsigned int) fnKeysym,
		 (unsigned int) flags,
		 (unsigned int) emacs_event->modifiers);
#endif

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

/***********************************************************************
			   NSTextInputClient
 ***********************************************************************/

#ifdef NS_IMPL_COCOA

- (void) insertText: (id) string
   replacementRange: (NSRange) replacementRange
{
  if ([string isKindOfClass:[NSAttributedString class]])
    string = [string string];
  [self unmarkText];
  [self insertText:string];
}

- (void) setMarkedText: (id) string
	 selectedRange: (NSRange) selectedRange
      replacementRange: (NSRange) replacementRange
{
  [self setMarkedText: string selectedRange: selectedRange];
}

- (nullable NSAttributedString *)
  attributedSubstringForProposedRange: (NSRange) range
			  actualRange: (nullable NSRangePointer) actualRange
{
  return nil;
}

- (NSRect) firstRectForCharacterRange: (NSRange) range
			  actualRange: (nullable NSRangePointer) actualRange
{
  return [self firstRectForCharacterRange: range];
}

#endif /* NS_IMPL_COCOA */

/***********************************************************************
			      NSTextInput
 ***********************************************************************/

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

static Lisp_Object
ns_in_echo_area_1 (void *ptr)
{
  const specpdl_ref count = SPECPDL_INDEX ();
  specbind (Qinhibit_quit, Qt);
  const Lisp_Object in_echo_area = safe_calln (Qns_in_echo_area);
  return unbind_to (count, in_echo_area);
}

static Lisp_Object
ns_in_echo_area_2 (enum nonlocal_exit exit, Lisp_Object error)
{
  return Qnil;
}

static bool
ns_in_echo_area (void)
{
  Lisp_Object in_echo_area;

  in_echo_area
    = internal_catch_all (ns_in_echo_area_1, NULL,
			  ns_in_echo_area_2);

  return !NILP (in_echo_area);
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

  if (WINDOWP (echo_area_window) && ns_in_echo_area ())
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

  struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (emacsframe));
  struct buffer *buf = XBUFFER (w->contents);
  ptrdiff_t point = BUF_PT (buf);

  if (NILP (BVAR (buf, mark_active)))
    {
      NSUInteger selection_location = point - BUF_BEGV (buf);
      return NSMakeRange (selection_location, 0);
    }

  ptrdiff_t mark = marker_position (BVAR (buf, mark));
  ptrdiff_t region_start = min (point, mark);
  ptrdiff_t region_end = max (point, mark);
  NSUInteger selection_location = region_start - BUF_BEGV (buf);
  NSUInteger selection_length = region_end - region_start;

  return NSMakeRange (selection_location, selection_length);
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
  EmacsWindow *window;

  NSTRACE ("[EmacsView mouseDown:]");

  if (!emacs_event)
    return;

  if (FRAME_TOOLTIP_P (emacsframe))
    return;

  dpyinfo->last_mouse_frame = emacsframe;
  /* Appears to be needed to prevent spurious movement events generated on
     button clicks.  */
  emacsframe->mouse_moved = 0;

  window = (EmacsWindow *) [self window];
  [window setLastDragEvent: theEvent];

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
          int x = 0, y = 0;
          int scrollUp = NO;

	  static bool end_flag = false;

	  if (!ns_use_mwheel_momentum && !end_flag
	      && [theEvent momentumPhase] != NSEventPhaseNone)
	    {
	      emacs_event->kind = TOUCH_END_EVENT;
	      emacs_event->arg = Qnil;
	      end_flag = [theEvent momentumPhase] != NSEventPhaseNone;
	      XSETINT (emacs_event->x, lrint (p.x));
	      XSETINT (emacs_event->y, lrint (p.y));
	      EV_TRAILER (theEvent);
	      return;
	    }

	  end_flag = [theEvent momentumPhase] != NSEventPhaseNone;

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
                  && (!mwheel_coalesce_scroll_events
		      || abs (totalDeltaX) > lineHeight))
                {
                  horizontal = YES;
                  scrollUp = totalDeltaX > 0;

                  lines = abs (totalDeltaX / lineHeight);
		  x = totalDeltaX;
		  if (!mwheel_coalesce_scroll_events)
		    totalDeltaX = 0;
		  else
		    totalDeltaX = totalDeltaX % lineHeight;
                  totalDeltaY = 0;
                }
              else if (abs (totalDeltaY) >= abs (totalDeltaX)
                       && (!mwheel_coalesce_scroll_events
			   || abs (totalDeltaY) > lineHeight))
                {
                  horizontal = NO;
                  scrollUp = totalDeltaY > 0;

                  lines = abs (totalDeltaY / lineHeight);
		  y = totalDeltaY;
		  if (!mwheel_coalesce_scroll_events)
		    totalDeltaY = 0;
		  else
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
	      x = ([theEvent scrollingDeltaX]
		   * FRAME_COLUMN_WIDTH (emacsframe));
	      y = ([theEvent scrollingDeltaY]
		   * FRAME_LINE_HEIGHT (emacsframe));
            }

          if (lines == 0 && mwheel_coalesce_scroll_events)
            return;

	  if (NUMBERP (Vns_scroll_event_delta_factor))
	    {
	      x *= XFLOATINT (Vns_scroll_event_delta_factor);
	      y *= XFLOATINT (Vns_scroll_event_delta_factor);
	    }

          emacs_event->kind = horizontal ? HORIZ_WHEEL_EVENT : WHEEL_EVENT;
          emacs_event->arg = list3 (make_fixnum (lines),
				    make_float (x),
				    make_float (y));

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
      Lisp_Object tab_bar_arg = Qnil;
      bool tab_bar_p = false;

      if (WINDOWP (emacsframe->tab_bar_window)
	  && WINDOW_TOTAL_LINES (XWINDOW (emacsframe->tab_bar_window)))
	{
	  Lisp_Object window;
	  int x = lrint (p.x);
	  int y = lrint (p.y);

	  window = window_from_coordinates (emacsframe, x, y, 0, true, true, true);
	  tab_bar_p = EQ (window, emacsframe->tab_bar_window);

	  if (tab_bar_p)
	    tab_bar_arg = handle_tab_bar_click (emacsframe, x, y,
						EV_UDMODIFIERS (theEvent) & down_modifier,
						EV_MODIFIERS (theEvent) | EV_UDMODIFIERS (theEvent));
	}

      if (!(tab_bar_p && NILP (tab_bar_arg)))
	emacs_event->kind = MOUSE_CLICK_EVENT;
      emacs_event->arg = tab_bar_arg;
      emacs_event->code = EV_BUTTON (theEvent);
      emacs_event->modifiers = EV_MODIFIERS (theEvent)
                             | EV_UDMODIFIERS (theEvent);

      if (emacs_event->modifiers & down_modifier)
	FRAME_DISPLAY_INFO (emacsframe)->grabbed |= 1 << EV_BUTTON (theEvent);
      else
	FRAME_DISPLAY_INFO (emacsframe)->grabbed &= ~(1 << EV_BUTTON (theEvent));
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

  if (FRAME_TOOLTIP_P (emacsframe))
    return;

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
	= window_from_coordinates (emacsframe, pt.x, pt.y, 0, 0, 0, 0);

      if (WINDOWP (window)
          && !EQ (window, last_mouse_window)
          && !EQ (window, selected_window)
	  && !MINI_WINDOW_P (XWINDOW (selected_window))
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

#if defined NS_IMPL_COCOA && defined MAC_OS_X_VERSION_10_7
- (void) magnifyWithEvent: (NSEvent *) event
{
  NSPoint pt = [self convertPoint: [event locationInWindow] fromView: nil];
  static CGFloat last_scale;

  NSTRACE ("[EmacsView magnifyWithEvent]");
  if (emacs_event)
    {
      emacs_event->kind = PINCH_EVENT;
      emacs_event->modifiers = EV_MODIFIERS (event);
      XSETINT (emacs_event->x, lrint (pt.x));
      XSETINT (emacs_event->y, lrint (pt.y));
      XSETFRAME (emacs_event->frame_or_window, emacsframe);

      if ([event phase] == NSEventPhaseBegan)
	{
	  last_scale = 1.0 + [event magnification];
	  emacs_event->arg = list4 (make_float (0.0),
				    make_float (0.0),
				    make_float (last_scale),
				    make_float (0.0));
	}
      else
	/* Report a tiny change so that Lisp code doesn't think this
	   is the beginning of an event sequence.  This is the best we
	   can do because NS doesn't report pinch events in as much
	   detail as XInput 2 or GTK+ do.  */
	emacs_event->arg = list4 (make_float (0.01),
				  make_float (0.0),
				  make_float (last_scale += [event magnification]),
				  make_float (0.0));
      EV_TRAILER (event);
    }
}
#endif

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


- (void)updateFramePosition
{
  NSWindow *win = [self window];
  NSRect r = [win frame];
  NSArray *screens = [NSScreen screens];
  NSScreen *screen = [screens objectAtIndex: 0];

  if (!emacsframe->output_data.ns)
    return;

  if (screen != nil)
    {
      emacsframe->left_pos = (NSMinX (r)
                              - NS_PARENT_WINDOW_LEFT_POS (emacsframe));
      emacsframe->top_pos = (NS_PARENT_WINDOW_TOP_POS (emacsframe)
                             - NSMaxY (r));

      if (emacs_event)
        {
          struct input_event ie;
          EVENT_INIT (ie);
          ie.kind = MOVE_FRAME_EVENT;
          XSETFRAME (ie.frame_or_window, emacsframe);
          XSETINT (ie.x, emacsframe->left_pos);
          XSETINT (ie.y, emacsframe->top_pos);
          kbd_buffer_store_event (&ie);
        }
    }
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

  /* Trigger `move-frame-functions' (Bug#74074).  */
  [self windowDidMove:(NSNotification *)sender];

  return frameSize;
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


- (void)resizeWithOldSuperviewSize: (NSSize)oldSize
{
  NSRect frame;
  int width, height;

  NSTRACE ("[EmacsView resizeWithOldSuperviewSize:]");

  [super resizeWithOldSuperviewSize:oldSize];

  if (! FRAME_LIVE_P (emacsframe))
    return;

  frame = [[self superview] bounds];
  width = (int)NSWidth (frame);
  height = (int)NSHeight (frame);

  NSTRACE_SIZE ("New size", NSMakeSize (width, height));

  /* Reset the frame size to match the bounds of the superview (the
     NSWindow's contentView).  We need to do this as sometimes the
     view's frame isn't resized correctly, or can end up with the
     wrong origin.  */
  [self setFrame:frame];
  change_frame_size (emacsframe, width, height, false, YES, false);

  SET_FRAME_GARBAGED (emacsframe);
  cancel_mouse_face (emacsframe);
  ns_send_appdefined (-1);
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
  struct input_event event;

  EVENT_INIT (event);

  NSTRACE ("[EmacsView windowDidBecomeKey]");

  if (emacsframe != old_focus)
    dpyinfo->ns_focus_frame = emacsframe;

  ns_frame_rehighlight (emacsframe);
  [self adjustEmacsFrameRect];

  event.kind = FOCUS_IN_EVENT;
  XSETFRAME (event.frame_or_window, emacsframe);
  kbd_buffer_store_event (&event);
  ns_send_appdefined (-1);  // Kick main loop
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
      any_help_event_p = NO;
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


- (instancetype) initFrameFromEmacs: (struct frame *)f
{
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
  [self initWithFrame:
          NSMakeRect (0, 0, FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, f->text_cols),
                      FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, f->text_lines))];
  [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

  FRAME_NS_VIEW (f) = self;
  emacsframe = f;
#ifdef NS_IMPL_COCOA
  old_title = 0;
  maximizing_resize = NO;
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 140000
  /* Restore to default before macOS 14 (bug#72440).  */
  [self setClipsToBounds: YES];
#endif
#endif

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
  /* These settings mean AppKit will retain the contents of the frame
     on resize.  Unfortunately it also means the frame will not be
     automatically marked for display, but we can do that ourselves in
     resizeWithOldSuperviewSize.  */
  [self setWantsLayer:YES];
  [self setLayerContentsRedrawPolicy:
          NSViewLayerContentsRedrawOnSetNeedsDisplay];
  [self setLayerContentsPlacement:NSViewLayerContentsPlacementTopLeft];

  [[EmacsWindow alloc] initWithEmacsFrame:f];

  /* Now the NSWindow has been created, we can finish up configuring
     the layer.  */
  [(EmacsLayer *)[self layer] setColorSpace:
                   [[[self window] colorSpace] CGColorSpace]];
  [(EmacsLayer *)[self layer] setContentsScale:
                   [[self window] backingScaleFactor]];
#else
  [[EmacsWindow alloc] initWithEmacsFrame:f];
#endif

  if (ns_drag_types)
    [self registerForDraggedTypes: ns_drag_types];

#if !defined (NS_IMPL_COCOA) \
  || MAC_OS_X_VERSION_MIN_REQUIRED <= 1090
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
  if ([self respondsToSelector: @selector(allocateGState)])
#endif
    [self allocateGState];
#endif
  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: [NSArray array]];

  ns_window_num++;
  return self;
}


- (void)windowDidMove: sender
{
  NSTRACE ("[EmacsView windowDidMove:]");

  [self updateFramePosition];
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

- (void)adjustEmacsFrameRect
{
  struct frame *f = emacsframe;
  NSWindow *frame_window = [FRAME_NS_VIEW (f) window];
  NSRect r = [frame_window frame];
  f->left_pos = NSMinX (r) - NS_PARENT_WINDOW_LEFT_POS (f);
  f->top_pos = NS_PARENT_WINDOW_TOP_POS (f) - NSMaxY (r);
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
    }

  /* Do what windowDidMove does which isn't called when entering/exiting
     fullscreen mode.  */
  [self adjustEmacsFrameRect];
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

  if (next_maximized != -1)
    [[self window] performZoom:self];

  /* Do what windowDidMove does which isn't called when entering/exiting
     fullscreen mode.  */
  [self adjustEmacsFrameRect];
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
	  if (FRAME_PARENT_FRAME (emacsframe)
	      || FRAME_TOOLTIP_P (emacsframe))
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
  EmacsWindow *w, *fw;
  BOOL onFirstScreen;
  struct frame *f;
  NSRect r;
  NSColor *col;

  NSTRACE ("[EmacsView toggleFullScreen:]");

  /* Reset fs_is_native to value of ns-use-native-full-screen if not
     fullscreen already */
  if (fs_state != FULLSCREEN_BOTH)
    fs_is_native = ns_use_native_fullscreen;

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

  w = (EmacsWindow *)[self window];
  onFirstScreen = [[w screen] isEqual:[[NSScreen screens] objectAtIndex:0]];
  f = emacsframe;
  col = [NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND
				 (FACE_FROM_ID (f, DEFAULT_FACE_ID))];

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

      fw = [[EmacsWindow alloc] initWithEmacsFrame:emacsframe
                                        fullscreen:YES
                                            screen:screen];

      f->border_width = 0;

      nonfs_window = w;

      [self windowWillEnterFullScreen];
      [fw makeKeyAndOrderFront:NSApp];
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

      f->border_width = [w borderWidth];

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

- (BOOL) validateToolbarItem: (NSToolbarItem *) toolbarItem
{
  return [toolbarItem isEnabled];
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


#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
- (CALayer *)makeBackingLayer
{
  EmacsLayer *l = [[EmacsLayer alloc]
                    initWithDoubleBuffered:FRAME_DOUBLE_BUFFERED (emacsframe)];

  [l setDelegate:(id)self];

  return l;
}


- (void)lockFocus
{
  NSTRACE ("[EmacsView lockFocus]");

  CGContextRef context = [(EmacsLayer*)[self layer] getContext];

  [NSGraphicsContext
        setCurrentContext:[NSGraphicsContext
                            graphicsContextWithCGContext:context
                                                 flipped:YES]];
}


- (void)unlockFocus
{
  NSTRACE ("[EmacsView unlockFocus]");

  [NSGraphicsContext setCurrentContext:nil];
  [self setNeedsDisplay:YES];
}


- (void)windowDidChangeBackingProperties:(NSNotification *)notification
  /* Update the drawing buffer when the backing properties change.  */
{
  NSTRACE ("EmacsView windowDidChangeBackingProperties:]");

  NSRect frame = [self frame];
  EmacsLayer *layer = (EmacsLayer *)[self layer];

  [layer setContentsScale:[[notification object] backingScaleFactor]];
  [layer setColorSpace:[(id) [[notification object] colorSpace] CGColorSpace]];

  ns_clear_frame (emacsframe);
  expose_frame (emacsframe, 0, 0, NSWidth (frame), NSHeight (frame));
}
#endif


- (void)copyRect:(NSRect)srcRect to:(NSPoint)dest
{
  NSTRACE ("[EmacsView copyRect:To:]");
  NSTRACE_RECT ("Source", srcRect);
  NSTRACE_POINT ("Destination", dest);

  NSRect dstRect = NSMakeRect (dest.x, dest.y, NSWidth (srcRect),
                               NSHeight (srcRect));

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
  CGContextRef context = [(EmacsLayer *)[self layer] getContext];
  CGContextFlush (context);

  double scale = [[self window] backingScaleFactor];
  int bpp = CGBitmapContextGetBitsPerPixel (context) / 8;
  void *pixels = CGBitmapContextGetData (context);
  int rowSize = CGBitmapContextGetBytesPerRow (context);
  int srcRowSize = NSWidth (srcRect) * scale * bpp;
  void *srcPixels = (char *) pixels
    + (int) (NSMinY (srcRect) * scale * rowSize
             + NSMinX (srcRect) * scale * bpp);
  void *dstPixels = (char *) pixels
    + (int) (dest.y * scale * rowSize
             + dest.x * scale * bpp);

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

#else
  hide_bell();              // Ensure the bell image isn't scrolled.

  ns_focus (emacsframe, &dstRect, 1);
  [self scrollRect: srcRect
                by: NSMakeSize (dstRect.origin.x - srcRect.origin.x,
                                dstRect.origin.y - srcRect.origin.y)];
  ns_unfocus (emacsframe);
#endif
}


#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
/* If the frame has been garbaged but the toolkit wants to draw, for
   example when resizing the frame, we end up with a blank screen.
   Sometimes this results in an unpleasant flicker, so try to
   redisplay before drawing.

   This used to be done in viewWillDraw, but with the custom layer
   that method is not called.  We cannot call redisplay directly from
   [NSView layout], because it may trigger another round of layout by
   changing the frame size and recursive layout calls are banned.  It
   appears to be safe to call redisplay here.  */
- (void)layoutSublayersOfLayer:(CALayer *)layer
{
  if (!redisplaying_p && FRAME_GARBAGED_P (emacsframe))
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
  id source;

  NSTRACE ("[EmacsView draggingEntered:]");

  source = [sender draggingSource];

  if (source && [source respondsToSelector: @selector(mustNotDropOn:)]
      && [source mustNotDropOn: self])
    return NSDragOperationNone;

  return NSDragOperationGeneric;
}


-(BOOL) prepareForDragOperation: (id <NSDraggingInfo>) sender
{
  id source;

  source = [sender draggingSource];

  if (source && [source respondsToSelector: @selector(mustNotDropOn:)]
      && [source mustNotDropOn: self])
    return NO;

  return YES;
}

- (BOOL) wantsPeriodicDraggingUpdates
{
  return YES;
}

- (NSDragOperation) draggingUpdated: (id <NSDraggingInfo>) sender
{
#ifdef NS_IMPL_GNUSTEP
  struct input_event ie;
#else
  Lisp_Object frame;
#endif
  NSPoint position;
  int x, y;
  NSAutoreleasePool *ap;
  specpdl_ref count;

  ap = [[NSAutoreleasePool alloc] init];
  count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (ns_release_autorelease_pool, ap);

#ifdef NS_IMPL_GNUSTEP
  EVENT_INIT (ie);
  ie.kind = DRAG_N_DROP_EVENT;
#endif

  /* Get rid of mouse face.  */
  [self mouseExited: [[self window] currentEvent]];

  position = [self convertPoint: [sender draggingLocation]
		       fromView: nil];
  x = lrint (position.x);
  y = lrint (position.y);

#ifdef NS_IMPL_GNUSTEP
  XSETINT (ie.x, x);
  XSETINT (ie.y, y);
  XSETFRAME (ie.frame_or_window, emacsframe);
  ie.arg = Qlambda;
  ie.modifiers = 0;

  kbd_buffer_store_event (&ie);
#else
  /* Input events won't be processed until the drop happens on macOS,
     so call this function instead.  */
  XSETFRAME (frame, emacsframe);

  safe_calln (Vns_drag_motion_function, frame,
	      make_fixnum (x), make_fixnum (y));

  redisplay ();
#endif

  unbind_to (count, Qnil);
  return NSDragOperationGeneric;
}

- (BOOL) performDragOperation: (id <NSDraggingInfo>) sender
{
  id pb, source;
  int x, y;
  NSString *type;
  NSPoint position;
  NSDragOperation op = [sender draggingSourceOperationMask];
  Lisp_Object operations = Qnil;
  Lisp_Object strings = Qnil;
  Lisp_Object type_sym;
  struct input_event ie;

  NSTRACE ("[EmacsView performDragOperation:]");

  source = [sender draggingSource];

  if (source && [source respondsToSelector: @selector(mustNotDropOn:)]
      && [source mustNotDropOn: self])
    return NO;

  position = [self convertPoint: [sender draggingLocation] fromView: nil];
  x = lrint (position.x);
  y = lrint (position.y);

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

  if (!type)
    return NO;
#if NS_USE_NSPasteboardTypeFileURL
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
      id files;
      NSEnumerator *fenum;
      NSString *file;

      files = [pb propertyListForType: type];

      if (!files)
        return NO;

      type_sym = Qfile;

      /* On GNUstep, files might be a string.  */

      if ([files respondsToSelector: @selector (objectEnumerator:)])
	{
	  fenum = [files objectEnumerator];

	  while ((file = [fenum nextObject]))
	    strings = Fcons ([file lispString], strings);
	}
      else
	/* Then `files' is an NSString.  */
	strings = list1 ([files lispString]);
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

      data = [pb stringForType: type];

      if (!data)
        return NO;

      type_sym = Qnil;
      strings = list1 ([data lispString]);
    }
  else
    return NO;

  EVENT_INIT (ie);
  ie.kind = DRAG_N_DROP_EVENT;
  ie.arg = Fcons (type_sym, Fcons (operations,
				   strings));
  XSETINT (ie.x, x);
  XSETINT (ie.y, y);
  XSETFRAME (ie.frame_or_window, emacsframe);

  kbd_buffer_store_event (&ie);
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


- (instancetype) initWithEmacsFrame: (struct frame *) f
{
  return [self initWithEmacsFrame:f fullscreen:NO screen:nil];
}


- (instancetype) initWithEmacsFrame: (struct frame *) f
                         fullscreen: (BOOL) fullscreen
                             screen: (NSScreen *) screen
{
  NSWindowStyleMask styleMask;
  int width, height;

  NSTRACE ("[EmacsWindow initWithEmacsFrame:fullscreen:screen:]");

  if (fullscreen)
    styleMask = NSWindowStyleMaskBorderless;
  else if (FRAME_UNDECORATED (f))
    {
      styleMask = NSWindowStyleMaskBorderless;
#ifdef NS_IMPL_COCOA
      styleMask |= NSWindowStyleMaskResizable;
#endif
    }
  else if (f->tooltip)
    styleMask = 0;
  else
    styleMask = (NSWindowStyleMaskTitled
		 | NSWindowStyleMaskResizable
		 | NSWindowStyleMaskMiniaturizable
		 | NSWindowStyleMaskClosable);

  last_drag_event = nil;

  width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, f->text_cols);
  height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, f->text_lines);

  self = [super initWithContentRect: NSMakeRect (0, 0, width, height)
                          styleMask: styleMask
                            backing: NSBackingStoreBuffered
                              defer: YES
                             screen: screen];
  if (self)
    {
      NSString *name;
      NSColor *col;
      NSScreen *screen = [self screen];
      EmacsView *view = FRAME_NS_VIEW (f);

      [self setDelegate:view];
      [[self contentView] addSubview:view];
      [self makeFirstResponder:view];

#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MIN_REQUIRED <= 1090
#if MAC_OS_X_VERSION_MAX_ALLOWED > 1090
      if ([self respondsToSelector: @selector(useOptimizedDrawing:)])
#endif
        [self useOptimizedDrawing:YES];
#endif

      [self setAcceptsMouseMovedEvents:YES];

      name = NILP (f->name) ? @"Emacs" : [NSString stringWithLispString:f->name];
      [self setTitle:name];

      if (!NILP (f->icon_name))
        [self setMiniwindowTitle:
                [NSString stringWithLispString:f->icon_name]];

      [self setAppearance];

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
      if ([self respondsToSelector:@selector(titlebarAppearsTransparent)])
        [self setTitlebarAppearsTransparent:FRAME_NS_TRANSPARENT_TITLEBAR (f)];
#endif

      [self setParentChildRelationships];

      if (FRAME_Z_GROUP (f) != z_group_none)
        [self setLevel:NSNormalWindowLevel + (FRAME_Z_GROUP_BELOW (f) ? -1 : 1)];

      if (screen != 0)
        {
          NSPoint pt = NSMakePoint
            (IN_BOUND (-SCREENMAX, f->left_pos
                       + NS_PARENT_WINDOW_LEFT_POS (f), SCREENMAX),
             IN_BOUND (-SCREENMAX,
                       NS_PARENT_WINDOW_TOP_POS (f) - f->top_pos,
                       SCREENMAX));

          [self setFrameTopLeftPoint:pt];

          NSTRACE_RECT ("new frame", [self frame]);
        }

      f->border_width = [self borderWidth];

      col = [NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND
                                     (FACE_FROM_ID (f, DEFAULT_FACE_ID))];
      [self setBackgroundColor:col];
      if ([col alphaComponent] != (EmacsCGFloat) 1.0)
        [self setOpaque:NO];

      /* toolbar support */
      [self createToolbar:f];

      /* macOS Sierra automatically enables tabbed windows.  We can't
         allow this to be enabled until it's available on a Free system.
         Currently it only happens by accident and is buggy anyway.  */
#ifdef NS_IMPL_COCOA
      if ([self respondsToSelector:@selector(setTabbingMode:)])
        [self setTabbingMode:NSWindowTabbingModeDisallowed];
#endif
      /* Always show the toolbar below the window title.  This is needed
	 on Mac OS 11+ where the toolbar style is decided by the system
	 (which is unpredictable) and the newfangled "compact" toolbar
	 may be chosen (which is undesirable).  */
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
      if ([self respondsToSelector:@selector(setToolbarStyle:)])
	[self setToolbarStyle: NSWindowToolbarStyleExpanded];
#endif
    }

  return self;
}


- (void)createToolbar: (struct frame *)f
{
  if (FRAME_UNDECORATED (f)
      || [self styleMask] == NSWindowStyleMaskBorderless
      || !FRAME_EXTERNAL_TOOL_BAR (f)
      || [self toolbar] != nil)
    return;

  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (f);

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
  /* If the view's layer isn't an EmacsLayer then we can't create the
     toolbar yet.  */
  if (! [[view layer] isKindOfClass:[EmacsLayer class]])
    return;
#endif

  EmacsToolbar *toolbar = [[EmacsToolbar alloc]
                            initForView:view
                            withIdentifier:[NSString stringWithFormat:@"%p", f]];

  [self setToolbar:toolbar];
  update_frame_tool_bar_1 (f, toolbar);

#ifdef NS_IMPL_COCOA
  {
    NSButton *toggleButton;
    toggleButton = [self standardWindowButton:NSWindowToolbarButton];
    [toggleButton setTarget:view];
    [toggleButton setAction:@selector (toggleToolbar:)];
  }
#endif
}

- (void)dealloc
{
  NSTRACE ("[EmacsWindow dealloc]");

  /* We need to release the toolbar ourselves.  */
  [[self toolbar] release];
  [self setToolbar: nil];


  /* Also the last button press event .  */
  if (last_drag_event)
    [last_drag_event release];

  [super dealloc];
}

- (NSInteger) borderWidth
{
  return NSWidth ([self frame]) - NSWidth ([[self contentView] frame]);
}


- (void)setParentChildRelationships
  /* After certain operations, for example making a frame visible or
     resetting the NSWindow through modifying the undecorated status,
     the parent/child relationship may be broken.  We can also use
     this method to set them, as long as the frame struct already has
     the correct relationship set.  */
{
  NSTRACE ("[EmacsWindow setParentChildRelationships]");

  Lisp_Object frame, tail;
  EmacsView *ourView = (EmacsView *)[self delegate];
  struct frame *ourFrame = ourView->emacsframe;
  struct frame *parentFrame = FRAME_PARENT_FRAME (ourFrame);
  EmacsWindow *oldParentWindow = (EmacsWindow *)[self parentWindow];


#ifdef NS_IMPL_COCOA
  /* We have to set the accessibility subroles and/or the collection
     behaviors early otherwise child windows may not go fullscreen as
     expected later.  */

#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
  if ([self respondsToSelector:@selector(setAccessibilitySubrole:)])
#endif
    /* Set the accessibility subroles.  */
    if (parentFrame)
      [self setAccessibilitySubrole:NSAccessibilityFloatingWindowSubrole];
    else
      [self setAccessibilitySubrole:NSAccessibilityStandardWindowSubrole];

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  [ourView updateCollectionBehavior];
#endif

  /* Child frames are often used in ways that may mean they should
     "disappear" into the contents of the parent frame.  macOs's
     drop-shadows break this effect, so remove them on undecorated
     child frames.  */
  if (parentFrame && FRAME_UNDECORATED (ourFrame))
    [self setHasShadow:NO];
  else
    [self setHasShadow:YES];
#endif


  /* Check if we have an incorrectly set parent.  */
  if ((! parentFrame && oldParentWindow)
      || (parentFrame && oldParentWindow
          && ((EmacsView *)[oldParentWindow delegate])->emacsframe != parentFrame))
    {
      [[self parentWindow] removeChildWindow:self];

#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if ([ourView respondsToSelector:@selector (toggleFullScreen)])
#endif
          /* If we are the descendent of a fullscreen window and we
             have no new parent, go fullscreen.  */
          {
            NSWindow *parent = (NSWindow *)oldParentWindow;
            while (parent)
              {
                if (([parent styleMask] & NSWindowStyleMaskFullScreen) != 0)
                  {
                    [ourView toggleFullScreen:self];
                    break;
                  }
                parent = [parent parentWindow];
              }
          }
#endif
    }

  if (parentFrame)
    {
      NSWindow *parentWindow = [FRAME_NS_VIEW (parentFrame) window];

#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
      if ([ourView respondsToSelector:@selector (toggleFullScreen)])
#endif
	/* Child frames must not be fullscreen.  */
	if ([ourView fsIsNative] && [ourView isFullscreen])
	  [ourView toggleFullScreen:self];
#endif

      [parentWindow addChildWindow:self
                           ordered:NSWindowAbove];
    }

  /* Check our child windows are configured correctly.  */
  FOR_EACH_FRAME (tail, frame)
    {
      if (FRAME_PARENT_FRAME (XFRAME (frame)) == ourFrame)
        [(EmacsWindow *)[FRAME_NS_VIEW (XFRAME (frame)) window] setParentChildRelationships];
    }
}


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


#ifdef NS_IMPL_GNUSTEP
/* orderedIndex isn't yet available in GNUstep, but it seems pretty
   easy to implement.  */
- (NSInteger) orderedIndex
{
  return [[NSApp orderedWindows] indexOfObjectIdenticalTo:self];
}
#endif


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
    return NSAccessibilityWindowRole;

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

  /* Don't do anything for child frames because that leads to weird
     child frame placement in some cases involving Dock placement and
     Dock Hiding.  */
#ifdef NS_IMPL_COCOA
  struct frame *f = ((EmacsView *) [self delegate])->emacsframe;
  if (FRAME_PARENT_FRAME (f))
    return frameRect;

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

- (BOOL)canBecomeMainWindow
  /* Required for fullscreen and undecorated windows.  */
{
  return YES;
}

- (void) setLastDragEvent: (NSEvent *) event
{
  if (last_drag_event)
    [last_drag_event release];
  last_drag_event = [event copy];
}

- (NSDragOperation) draggingSourceOperationMaskForLocal: (BOOL) is_local
{
  return drag_op;
}

- (void) draggedImage: (NSImage *) image
	      endedAt: (NSPoint) screen_point
	    operation: (NSDragOperation) operation
{
  selected_op = operation;
}

- (void) draggedImage: (NSImage *) dragged_image
	      movedTo: (NSPoint) screen_point
{
  NSPoint mouse_loc;
#ifdef NS_IMPL_COCOA
  NSInteger window_number;
  NSWindow *w;
#endif

  mouse_loc = [NSEvent mouseLocation];

#ifdef NS_IMPL_COCOA
  if (dnd_mode != RETURN_FRAME_NEVER)
    {
      window_number = [NSWindow windowNumberAtPoint: mouse_loc
			belowWindowWithWindowNumber: 0];
      w = [NSApp windowWithWindowNumber: window_number];

      if (!w || w != self)
	dnd_mode = RETURN_FRAME_NOW;

      if (dnd_mode != RETURN_FRAME_NOW
	  || ![[w delegate] isKindOfClass: [EmacsView class]]
	  || ((EmacsView *) [w delegate])->emacsframe->tooltip)
	goto out;

      dnd_return_frame = ((EmacsView *) [w delegate])->emacsframe;

      /* FIXME: there must be a better way to leave the event loop.  */
      [NSException raise: @""
		  format: @"Must return DND frame"];
    }

 out:
#endif

  if (dnd_move_tooltip_with_frame)
    ns_move_tooltip_to_mouse_location (mouse_loc);
}

- (BOOL) mustNotDropOn: (NSView *) receiver
{
  return ([receiver window] == self
	  ? !dnd_allow_same_frame : NO);
}

- (NSDragOperation) beginDrag: (NSDragOperation) op
		forPasteboard: (NSPasteboard *) pasteboard
		     withMode: (enum ns_return_frame_mode) mode
		returnFrameTo: (struct frame **) frame_return
		 prohibitSame: (BOOL) prohibit_same_frame
		followTooltip: (BOOL) follow_tooltip
{
  NSImage *image;
#ifdef NS_IMPL_COCOA
  NSInteger window_number;
  NSWindow *w;
#endif
  drag_op = op;
  selected_op = NSDragOperationNone;
  image = [[NSImage alloc] initWithSize: NSMakeSize (1.0, 1.0)];
  dnd_mode = mode;
  dnd_return_frame = NULL;
  dnd_allow_same_frame = !prohibit_same_frame;
  dnd_move_tooltip_with_frame = follow_tooltip;

  /* Now draw transparency onto the image.  */
  [image lockFocus];
  [[NSColor colorWithUnsignedLong: 0] set];
  NSRectFillUsingOperation (NSMakeRect (0, 0, 1, 1),
			    NSCompositingOperationCopy);
  [image unlockFocus];

  block_input ();
#ifdef NS_IMPL_COCOA
  if (mode == RETURN_FRAME_NOW)
    {
      window_number = [NSWindow windowNumberAtPoint: [NSEvent mouseLocation]
			belowWindowWithWindowNumber: 0];
      w = [NSApp windowWithWindowNumber: window_number];

      if (w && [[w delegate] isKindOfClass: [EmacsView class]]
	  && !((EmacsView *) [w delegate])->emacsframe->tooltip)
	{
	  *frame_return = ((EmacsView *) [w delegate])->emacsframe;
	  [image release];
	  unblock_input ();

	  return NSDragOperationNone;
	}
    }

  @try
    {
#endif
      if (last_drag_event)
	[self dragImage: image
		     at: NSMakePoint (0, 0)
		 offset: NSMakeSize (0, 0)
		  event: last_drag_event
	     pasteboard: pasteboard
		 source: self
	      slideBack: NO];
#ifdef NS_IMPL_COCOA
    }
  @catch (NSException *e)
    {
      /* Ignore.  This is probably the wrong way to leave the
	 drag-and-drop run loop.  */
    }
#endif
  unblock_input ();

  /* The drop happened, so delete the tooltip.  */
  if (follow_tooltip)
    Fx_hide_tip ();

  /* Assume all buttons have been released since the drag-and-drop
     operation is now over.  */
  if (!dnd_return_frame)
    x_display_list->grabbed = 0;

  [image release];

  *frame_return = dnd_return_frame;
  return selected_op;
}

@end /* EmacsWindow */


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

- (void) mark
{
  if (window)
    {
      Lisp_Object win;
      XSETWINDOW (win, window);
      mark_object (win);
    }
}


- (void)resetCursorRects
{
  NSRect visible = [self visibleRect];
  NSTRACE ("[EmacsScroller resetCursorRects]");

  if (!NSIsEmptyRect (visible))
    [self addCursorRect: visible cursor: [NSCursor arrowCursor]];

#if defined (NS_IMPL_GNUSTEP) || MAC_OS_X_VERSION_MIN_REQUIRED < 101300
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
  if ([[NSCursor arrowCursor] respondsToSelector:
                                @selector(setOnMouseEntered:)])
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


#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400

/* ==========================================================================

   A class to handle the screen buffer.

   ========================================================================== */

@implementation EmacsLayer


/* An IOSurface is a pixel buffer that is efficiently copied to VRAM
   for display.  In order to use an IOSurface we must first lock it,
   write to it, then unlock it.  At this point it is transferred to
   VRAM and if we modify it during this transfer we may see corruption
   of the output.  To avoid this problem we can check if the surface
   is "in use", and if it is then avoid using it.  Unfortunately to
   avoid writing to a surface that's in use, but still maintain the
   ability to draw to the screen at any time, we need to keep a cache
   of multiple surfaces that we can use at will.

   The EmacsLayer class maintains this cache of surfaces, and
   handles the conversion to a CGGraphicsContext that AppKit can use
   to draw on.

   The cache is simple: if a free surface is found it is removed from
   the cache and set as the "current" surface.  Emacs draws to the
   surface and when the layer wants to update the screen we set it's
   contents to the surface and then add it back on to the end of the
   cache.  If no free surfaces are found in the cache then a new one
   is created.  */

- (id) initWithDoubleBuffered: (bool)db
{
  NSTRACE ("[EmacsLayer initWithDoubleBuffered:]");

  self = [super init];
  if (self)
    {
      [self setColorSpace:nil];
      [self setDoubleBuffered:db];
      cache = [[NSMutableArray arrayWithCapacity:(doubleBuffered ? 2 : 1)] retain];
    }
  else
    return nil;

  return self;
}


- (void) setColorSpace: (CGColorSpaceRef)cs
{
  /* We don't need to clear the cache because the new colorspace will
     be used next time we create a new context.  */
  if (cs)
    colorSpace = cs;
  else
    colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
}


- (void) setDoubleBuffered: (bool)db
{
  if (doubleBuffered != db)
    [self releaseSurfaces];

  doubleBuffered = db;
}


- (void) dealloc
{
  [self releaseSurfaces];
  [cache release];

  [super dealloc];
}


- (void) releaseSurfaces
{
  [self setContents:nil];
  [self releaseContext];

  if (currentSurface)
    {
      CFRelease (currentSurface);
      currentSurface = nil;
    }

  if (cache)
    {
      for (id object in cache)
        CFRelease ((IOSurfaceRef)object);

      [cache removeAllObjects];
    }
}


/* Check whether the current bounds match the IOSurfaces we are using.
   If they do return YES, otherwise NO.  */
- (BOOL) checkDimensions
{
  int width = NSWidth ([self bounds]) * [self contentsScale];
  int height = NSHeight ([self bounds]) * [self contentsScale];
  IOSurfaceRef s = currentSurface ? currentSurface
    : (IOSurfaceRef)[cache firstObject];

  return !s || (IOSurfaceGetWidth (s) == width
                && IOSurfaceGetHeight (s) == height);
}


/* Return a CGContextRef that can be used for drawing to the screen.  */
- (CGContextRef) getContext
{
  CGFloat scale = [self contentsScale];

  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "[EmacsLayer getContext]");
  NSTRACE_MSG ("IOSurface count: %lu", [cache count] + (currentSurface ? 1 : 0));

  if (![self checkDimensions])
    [self releaseSurfaces];

  if (!context)
    {
      IOSurfaceRef surface = NULL;
      int width = NSWidth ([self bounds]) * scale;
      int height = NSHeight ([self bounds]) * scale;

      for (id object in cache)
        {
          if (!IOSurfaceIsInUse ((IOSurfaceRef)object))
            {
              surface = (IOSurfaceRef)object;
              [cache removeObject:object];
              break;
            }
        }

      if (!surface && [cache count] >= (doubleBuffered ? 2 : 1))
        {
          /* Just grab the first one off the cache.  This may result
             in tearing effects.  The alternative is to wait for one
             of the surfaces to become free.  */
          surface = (IOSurfaceRef)[cache firstObject];
          [cache removeObject:(id)surface];
        }
      else if (!surface)
        {
          int bytesPerRow = IOSurfaceAlignProperty (kIOSurfaceBytesPerRow,
                                                    width * 4);

          surface = IOSurfaceCreate
            ((CFDictionaryRef)@{(id)kIOSurfaceWidth:[NSNumber numberWithInt:width],
                (id)kIOSurfaceHeight:[NSNumber numberWithInt:height],
                (id)kIOSurfaceBytesPerRow:[NSNumber numberWithInt:bytesPerRow],
                (id)kIOSurfaceBytesPerElement:[NSNumber numberWithInt:4],
                (id)kIOSurfacePixelFormat:[NSNumber numberWithUnsignedInt:'BGRA']});
        }

      if (!surface)
        {
          NSLog (@"Failed to create IOSurface for frame %@", [self delegate]);
          return nil;
        }

      IOReturn lockStatus = IOSurfaceLock (surface, 0, nil);
      if (lockStatus != kIOReturnSuccess)
        NSLog (@"Failed to lock surface: %x", (unsigned int)lockStatus);

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

      if (!context)
        {
          NSLog (@"Failed to create context for frame %@", [self delegate]);
          IOSurfaceUnlock (currentSurface, 0, nil);
          CFRelease (currentSurface);
          currentSurface = nil;
          return nil;
        }

      CGContextTranslateCTM(context, 0, IOSurfaceGetHeight (surface));
      CGContextScaleCTM(context, scale, -scale);
    }

  return context;
}


/* Releases the CGGraphicsContext and unlocks the associated
   IOSurface, so it will be sent to VRAM.  */
- (void) releaseContext
{
  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "[EmacsLayer releaseContext]");

  if (!context)
    return;

  CGContextFlush (context);
  CGContextRelease (context);
  context = NULL;

  IOReturn lockStatus = IOSurfaceUnlock (currentSurface, 0, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to unlock surface: %x", (unsigned int)lockStatus);
}


- (void) display
{
  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "[EmacsLayer display]");

  if (context && context != [[NSGraphicsContext currentContext] CGContext])
    {
      [self releaseContext];

      /* This forces the layer to see the surface as updated even if
         we replace it with itself.  */
      [self setContents:nil];
      [self setContents:(id)currentSurface];

      /* Put currentSurface back on the end of the cache.  */
      [cache addObject:(id)currentSurface];
      currentSurface = NULL;
    }
}


/* Copy the contents of lastSurface to DESTINATION.  This is required
   every time we want to use an IOSurface as its contents are probably
   blanks (if it's new), or stale.  */
- (void) copyContentsTo: (IOSurfaceRef) destination
{
  IOReturn lockStatus;
  IOSurfaceRef source = (IOSurfaceRef)[self contents];
  void *sourceData, *destinationData;
  int numBytes = IOSurfaceGetAllocSize (destination);

  NSTRACE_WHEN (NSTRACE_GROUP_FOCUS, "[EmacsLayer copyContentsTo:]");

  if (!source || source == destination)
    return;

  lockStatus = IOSurfaceLock (source, kIOSurfaceLockReadOnly, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to lock source surface: %x",
	   (unsigned int) lockStatus);

  sourceData = IOSurfaceGetBaseAddress (source);
  destinationData = IOSurfaceGetBaseAddress (destination);

  /* Since every IOSurface should have the exact same settings, a
     memcpy seems like the fastest way to copy the data from one to
     the other.  */
  memcpy (destinationData, sourceData, numBytes);

  lockStatus = IOSurfaceUnlock (source, kIOSurfaceLockReadOnly, nil);
  if (lockStatus != kIOReturnSuccess)
    NSLog (@"Failed to unlock source surface: %x", (unsigned int)lockStatus);
}

#undef CACHE_MAX_SIZE

@end /* EmacsLayer */


#endif /* NS_IMPL_COCOA */


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
mark_nsterm (void)
{
  NSTRACE ("mark_nsterm");
  Lisp_Object tail, frame;
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_NS_P (f))
	{
	  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];
	  for (int i = [subviews count] - 1; i >= 0; --i)
	    {
	      id scroller = [subviews objectAtIndex: i];
	      if ([scroller isKindOfClass: [EmacsScroller class]])
                  [scroller mark];
	    }
	}
    }
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
  DEFSYM (Qns_handle_drag_motion, "ns-handle-drag-motion");

  Fput (Qalt, Qmodifier_value, make_fixnum (alt_modifier));
  Fput (Qhyper, Qmodifier_value, make_fixnum (hyper_modifier));
  Fput (Qmeta, Qmodifier_value, make_fixnum (meta_modifier));
  Fput (Qsuper, Qmodifier_value, make_fixnum (super_modifier));
  Fput (Qcontrol, Qmodifier_value, make_fixnum (ctrl_modifier));

 DEFVAR_LISP ("ns-input-font", ns_input_font,
   doc: /* The font specified in the last NS event. */);
 ns_input_font = Qnil;

 DEFVAR_LISP ("ns-input-fontsize", ns_input_fontsize,
   doc: /* The fontsize specified in the last NS event. */);
 ns_input_fontsize = Qnil;

 DEFVAR_LISP ("ns-input-line", ns_input_line,
   doc: /* The line specified in the last NS event. */);
 ns_input_line = Qnil;

 DEFVAR_LISP ("ns-input-spi-name", ns_input_spi_name,
   doc: /* The service name specified in the last NS event. */);
 ns_input_spi_name = Qnil;

 DEFVAR_LISP ("ns-input-spi-arg", ns_input_spi_arg,
   doc: /* The service argument specified in the last NS event. */);
  ns_input_spi_arg = Qnil;

  DEFVAR_LISP ("ns-input-file", ns_input_file,
    doc: /* The file specified in the last NS event.  */);
  ns_input_file = Qnil;

  DEFVAR_LISP ("ns-working-text", ns_working_text,
    doc: /* String for visualizing working composition sequence.  */);
  ns_working_text = Qnil;

  DEFVAR_LISP ("ns-alternate-modifier", ns_alternate_modifier,
    doc: /* This variable describes the behavior of the alternate or option key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_alternate_modifier = Qmeta;

  DEFVAR_LISP ("ns-right-alternate-modifier", ns_right_alternate_modifier,
    doc: /* This variable describes the behavior of the right alternate or option key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.
It can also be `left' to use the value of `ns-alternate-modifier' instead.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_right_alternate_modifier = Qleft;

  DEFVAR_LISP ("ns-command-modifier", ns_command_modifier,
    doc: /* This variable describes the behavior of the command key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
#ifdef NS_IMPL_COCOA
  ns_command_modifier = Qsuper;
#else
  ns_command_modifier = Qmeta;
#endif

  DEFVAR_LISP ("ns-right-command-modifier", ns_right_command_modifier,
    doc: /* This variable describes the behavior of the right command key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.
It can also be `left' to use the value of `ns-command-modifier' instead.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_right_command_modifier = Qleft;

  DEFVAR_LISP ("ns-control-modifier", ns_control_modifier,
    doc: /* This variable describes the behavior of the control key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_control_modifier = Qcontrol;

  DEFVAR_LISP ("ns-right-control-modifier", ns_right_control_modifier,
    doc: /* This variable describes the behavior of the right control key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.
It can also be `left' to use the value of `ns-control-modifier' instead.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_right_control_modifier = Qleft;

  DEFVAR_LISP ("ns-function-modifier", ns_function_modifier,
    doc: /* This variable describes the behavior of the function (fn) key.
Either SYMBOL, describing the behavior for any event,
or (:ordinary SYMBOL :function SYMBOL :mouse SYMBOL), describing behavior
separately for ordinary keys, function keys, and mouse events.

Each SYMBOL is `control', `meta', `alt', `super', `hyper' or `none'.
If `none', the key is ignored by Emacs and retains its standard meaning.  */);
  ns_function_modifier = Qnone;

  DEFVAR_LISP ("ns-antialias-text", ns_antialias_text,
    doc: /* Non-nil (the default) means to render text antialiased.  */);
  ns_antialias_text = Qt;

  DEFVAR_LISP ("ns-use-thin-smoothing", ns_use_thin_smoothing,
    doc: /* Non-nil turns on a font smoothing method that produces thinner strokes.  */);
  ns_use_thin_smoothing = Qnil;

  DEFVAR_LISP ("ns-confirm-quit", ns_confirm_quit,
    doc: /* Whether to confirm application quit using dialog.  */);
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

  /* TODO: add an "auto" mode that passes clicks through to "utility" UI
     elements, selects windows, and so on, but doesn't pass them through
     for commands in general--as with other applications.  */

  DEFVAR_BOOL ("ns-click-through",
	       ns_click_through,
	       doc: /* Whether to pass activation clicks through to Emacs.
When nil, if Emacs is not focused, the click that focuses Emacs will not
be interpreted as a common.  If t, it will be.  For example, when nil,
if Emacs is inactive, two clicks are needed to move point: the first to
activate Emacs and the second to activate the mouse-1 binding.  When t,
only a single click is needed.  */);
  ns_click_through = YES;

  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");

  DEFVAR_LISP ("ns-scroll-event-delta-factor", Vns_scroll_event_delta_factor,
	       doc: /* A factor to apply to pixel deltas reported in scroll events.
 This is only effective for pixel deltas generated from touch pads or
 mice with smooth scrolling capability.  */);
  Vns_scroll_event_delta_factor = make_float (1.0);

  DEFVAR_LISP ("ns-drag-motion-function", Vns_drag_motion_function,
    doc: /* Function called when another program drags items over Emacs.

It is called with three arguments FRAME, X, and Y, whenever the user
moves the mouse over an Emacs frame as part of a drag-and-drop
operation.  FRAME is the frame the mouse is on top of, and X and Y are
the frame-relative positions of the mouse in the X and Y axes
respectively.  */);
  Vns_drag_motion_function = Qns_handle_drag_motion;

  /* Tell Emacs about this window system.  */
  Fprovide (Qns, Qnil);

  DEFSYM (Qcocoa, "cocoa");
  DEFSYM (Qgnustep, "gnustep");
  DEFSYM (QCordinary, ":ordinary");
  DEFSYM (QCfunction, ":function");
  DEFSYM (QCmouse, ":mouse");
  DEFSYM (Qcondensed, "condensed");
  DEFSYM (Qreverse_italic, "reverse-italic");
  DEFSYM (Qexpanded, "expanded");
  DEFSYM (Qns_in_echo_area, "ns-in-echo-area");

#ifdef NS_IMPL_COCOA
  Fprovide (Qcocoa, Qnil);
  syms_of_macfont ();
#else
  Fprovide (Qgnustep, Qnil);
  syms_of_nsfont ();
#endif

  last_known_monitors = Qnil;
  staticpro (&last_known_monitors);
}
