/* -*- objc -*- */
/* Definitions and headers for communication with NeXT/Open/GNUstep API.
   Copyright (C) 1989, 1993, 2005, 2008-2023 Free Software Foundation,
   Inc.

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


#include "dispextern.h"
#include "frame.h"
#include "character.h"
#include "font.h"
#include "sysselect.h"
#include "sysstdio.h"

#ifdef HAVE_NS
#ifdef __OBJC__

/* CGFloat on GNUstep may be 4 or 8 byte, but functions expect float* for some
   versions.
   On Cocoa >= 10.5, functions expect CGFloat *.  Make compatible type.  */
#ifdef NS_IMPL_COCOA
typedef CGFloat EmacsCGFloat;
#elif GNUSTEP_GUI_MAJOR_VERSION > 0 || GNUSTEP_GUI_MINOR_VERSION >= 22
typedef CGFloat EmacsCGFloat;
#else
typedef float EmacsCGFloat;
#endif

/* NSFilenamesPboardType is deprecated in macOS 10.14, but
   NSPasteboardTypeFileURL is only available in 10.13 (and GNUstep
   probably lacks it too). */
#if defined NS_IMPL_COCOA && MAC_OS_X_VERSION_MIN_REQUIRED >= 101300
#define NS_USE_NSPasteboardTypeFileURL 1
#else
#define NS_USE_NSPasteboardTypeFileURL 0
#endif

/* ==========================================================================

   Trace support

   ========================================================================== */

/* Uncomment the following line to enable trace.

   Uncomment suitable NSTRACE_GROUP_xxx lines to trace more.

   Hint: keep the trailing whitespace -- the version control system
   will reject accidental commits. */

/* #define NSTRACE_ENABLED 1          */


/* When non-zero, trace output is enabled for all parts, except those
   explicitly disabled. */
/* #define NSTRACE_ALL_GROUPS     1     */

/* When non-zero, trace output is enabled in the corresponding part. */
/* #define NSTRACE_GROUP_EVENTS  1     */
/* #define NSTRACE_GROUP_UPDATES 1     */
/* #define NSTRACE_GROUP_FRINGE  1     */
/* #define NSTRACE_GROUP_COLOR   1     */
/* #define NSTRACE_GROUP_GLYPHS  1     */
/* #define NSTRACE_GROUP_FOCUS   1     */


/* Print a call tree containing all annotated functions.

   The call structure of the functions is represented using
   indentation and vertical lines.  Extra information is printed using
   horizontal lines that connect to the vertical line.

   The return value is represented using the arrow "->>".  For simple
   functions, the arrow can be printed on the same line as the
   function name.  If more output is printed, it is connected to the
   vertical line of the function.

   The first column contains the file name, the second the line
   number, and the third a number increasing for each trace line.

   Note, when trace output from several threads are mixed, the output
   can become misaligned, as all threads (currently) share one state.
   This is post prominent when the EVENTS part is enabled.

   Note that the trace system, when enabled, uses the GCC/Clang
   "cleanup" extension. */

/*   For example, the following is the output of `M-x
     toggle-frame-maximized RET'.

     (Long lines manually split to reduced width):

nsterm.m  : 1608: [  354]  ns_fullscreen_hook
nsterm.m  : 7180: [  355]  | [EmacsView handleFS]
nsterm.m  : 7209: [  356]  | +--- FULLSCREEN_MAXIMIZED
nsterm.m  : 7706: [  357]  | | [EmacsWindow performZoom:]
nsterm.m  : 7715: [  358]  | | | [EmacsWindow zoom:]
nsterm.m  :  882: [  359]  | | | | ns_update_auto_hide_menu_bar
nsterm.m  : 6752: [  360]  | | | |
  [EmacsView windowWillUseStandardFrame:defaultFrame:(X:0 Y:0)/(W:1600 H:1177)]
nsterm.m  : 6753: [  361]  | | | | +--- fs_state: FULLSCREEN_NONE
nsterm.m  : 6754: [  362]  | | | | +--- fs_before_fs: -1
nsterm.m  : 6755: [  363]  | | | | +--- next_maximized: FULLSCREEN_MAXIMIZED
nsterm.m  : 6756: [  364]  | | | | +--- ns_userRect: (X:0 Y:0)/(W:0 H:0)
nsterm.m  : 6757: [  365]  | | | | +---
                                      [sender frame]: (X:0 Y:626)/(W:595 H:551)
nsterm.m  : 6781: [  366]  | | | | +---
                                     ns_userRect (2): (X:0 Y:626)/(W:595 H:551)
nsterm.m  : 6821: [  367]  | | | | +--- FULLSCREEN_MAXIMIZED
nsterm.m  : 7232: [  368]  | | | | |
                                    [EmacsView setFSValue:FULLSCREEN_MAXIMIZED]
nsterm.m  : 6848: [  369]  | | | | +---
                                   Final ns_userRect: (X:0 Y:626)/(W:595 H:551)
nsterm.m  : 6849: [  370]  | | | | +--- Final maximized_width: 1600
nsterm.m  : 6850: [  371]  | | | | +--- Final maximized_height: 1177
nsterm.m  : 6851: [  372]  | | | | +--- Final next_maximized: -1
nsterm.m  : 6322: [  373]  | | | | |
                           [EmacsView windowWillResize:toSize: (W:1600 H:1177)]
nsterm.m  : 6323: [  374]  | | | | | +---
                                      [sender frame]: (X:0 Y:626)/(W:595 H:551)
nsterm.m  : 6324: [  375]  | | | | | +--- fs_state: FULLSCREEN_MAXIMIZED
nsterm.m  : 7027: [  376]  | | | | | | [EmacsView isFullscreen]
nsterm.m  : 6387: [  377]  | | | | | +--- cols: 223  rows: 79
nsterm.m  : 6412: [  378]  | | | | | +->> (W:1596 H:1167)
nsterm.m  : 6855: [  379]  | | | | +->> (X:0 Y:0)/(W:1600 H:1177)
*/

#ifndef NSTRACE_ENABLED
#define NSTRACE_ENABLED 0
#endif

#if NSTRACE_ENABLED

#ifndef NSTRACE_ALL_GROUPS
#define NSTRACE_ALL_GROUPS 0
#endif

#ifndef NSTRACE_GROUP_EVENTS
#define NSTRACE_GROUP_EVENTS NSTRACE_ALL_GROUPS
#endif

#ifndef NSTRACE_GROUP_UPDATES
#define NSTRACE_GROUP_UPDATES NSTRACE_ALL_GROUPS
#endif

#ifndef NSTRACE_GROUP_FRINGE
#define NSTRACE_GROUP_FRINGE NSTRACE_ALL_GROUPS
#endif

#ifndef NSTRACE_GROUP_COLOR
#define NSTRACE_GROUP_COLOR NSTRACE_ALL_GROUPS
#endif

#ifndef NSTRACE_GROUP_GLYPHS
#define NSTRACE_GROUP_GLYPHS NSTRACE_ALL_GROUPS
#endif

#ifndef NSTRACE_GROUP_FOCUS
#define NSTRACE_GROUP_FOCUS NSTRACE_ALL_GROUPS
#endif

extern volatile int nstrace_num;
extern volatile int nstrace_depth;
extern volatile int nstrace_enabled_global;

void nstrace_leave(int *);
void nstrace_restore_global_trace_state(int *);
char const * nstrace_fullscreen_type_name (int);

/* printf-style trace output.  Output is aligned with contained heading.  */
#define NSTRACE_MSG_NO_DASHES(...)                                          \
  do                                                                        \
    {                                                                       \
      if (nstrace_enabled)                                                  \
        {                                                                   \
          fprintf (stderr, "%-10s:%5d: [%5d]%.*s",                          \
                   __FILE__, __LINE__, nstrace_num++,                       \
                   2*nstrace_depth, "  | | | | | | | | | | | | | | | ..");  \
          fprintf (stderr, __VA_ARGS__);                                    \
	  putc ('\n', stderr);						    \
        }                                                                   \
    }                                                                       \
  while(0)

#define NSTRACE_MSG(...) NSTRACE_MSG_NO_DASHES("+--- " __VA_ARGS__)



/* Macros for printing complex types.

   NSTRACE_FMT_what     -- Printf format string for "what".
   NSTRACE_ARG_what(x)  -- Printf argument for "what".  */

#define NSTRACE_FMT_SIZE        "(W:%.0f H:%.0f)"
#define NSTRACE_ARG_SIZE(elt)   (elt).width, (elt).height

#define NSTRACE_FMT_POINT       "(X:%.0f Y:%.0f)"
#define NSTRACE_ARG_POINT(elt)  (elt).x, (elt).y

#define NSTRACE_FMT_RECT        NSTRACE_FMT_POINT "/" NSTRACE_FMT_SIZE
#define NSTRACE_ARG_RECT(elt)   \
  NSTRACE_ARG_POINT((elt).origin), NSTRACE_ARG_SIZE((elt).size)

#define NSTRACE_FMT_FSTYPE      "%s"
#define NSTRACE_ARG_FSTYPE(elt) nstrace_fullscreen_type_name(elt)


/* Macros for printing complex types as extra information.  */

#define NSTRACE_SIZE(str,size)                                          \
  NSTRACE_MSG (str ": " NSTRACE_FMT_SIZE,                               \
               NSTRACE_ARG_SIZE (size));

#define NSTRACE_POINT(str,point)                                        \
  NSTRACE_MSG (str ": " NSTRACE_FMT_POINT,                              \
               NSTRACE_ARG_POINT (point));

#define NSTRACE_RECT(str,rect)                                          \
  NSTRACE_MSG (str ": " NSTRACE_FMT_RECT,                               \
               NSTRACE_ARG_RECT (rect));

#define NSTRACE_FSTYPE(str,fs_type)                                     \
  NSTRACE_MSG (str ": " NSTRACE_FMT_FSTYPE,                             \
               NSTRACE_ARG_FSTYPE (fs_type));


/* Return value macros.

   NSTRACE_RETURN(fmt, ...) - Print a return value, support printf-style
                              format string and arguments.

   NSTRACE_RETURN_what(obj) - Print a return value of kind WHAT.

   NSTRACE_FMT_RETURN - A string literal representing a returned
                        value.  Useful when creating a format string
                        to printf-like constructs like NSTRACE().  */

#define NSTRACE_FMT_RETURN "->>"

#define NSTRACE_RETURN(...) \
  NSTRACE_MSG_NO_DASHES ("+" NSTRACE_FMT_RETURN " " __VA_ARGS__)

#define NSTRACE_RETURN_SIZE(size) \
  NSTRACE_RETURN(NSTRACE_FMT_SIZE, NSTRACE_ARG_SIZE(size))

#define NSTRACE_RETURN_POINT(point) \
  NSTRACE_RETURN(NSTRACE_FMT_POINT, NSTRACE_ARG_POINT(point))

#define NSTRACE_RETURN_RECT(rect) \
  NSTRACE_RETURN(NSTRACE_FMT_RECT, NSTRACE_ARG_RECT(rect))


/* Function enter macros.

   NSTRACE (fmt, ...) -- Enable trace output in current block
                         (typically a function).  Accepts printf-style
                         arguments.

   NSTRACE_WHEN (cond, fmt, ...) -- Enable trace output when COND is true.

   NSTRACE_UNLESS (cond, fmt, ...) -- Enable trace output unless COND is
                                      true.  */



#define NSTRACE_WHEN(cond, ...)                                         \
  __attribute__((cleanup(nstrace_restore_global_trace_state)))          \
  int nstrace_saved_enabled_global = nstrace_enabled_global;            \
  __attribute__((cleanup(nstrace_leave)))                               \
  int nstrace_enabled = nstrace_enabled_global && (cond);               \
  if (nstrace_enabled) { ++nstrace_depth; }                             \
  else { nstrace_enabled_global = 0; }                                  \
  NSTRACE_MSG_NO_DASHES(__VA_ARGS__);

/* Unsilence called functions.

   Concretely, this us used to allow "event" functions to be silenced
   while trace output can be printed for functions they call.  */
#define NSTRACE_UNSILENCE() do { nstrace_enabled_global = 1; } while(0)

#endif /* NSTRACE_ENABLED */

#define NSTRACE(...)              NSTRACE_WHEN(1, __VA_ARGS__)
#define NSTRACE_UNLESS(cond, ...) NSTRACE_WHEN(!(cond), __VA_ARGS__)

/* Non-trace replacement versions.  */
#ifndef NSTRACE_WHEN
#define NSTRACE_WHEN(...)
#endif

#ifndef NSTRACE_MSG
#define NSTRACE_MSG(...)
#endif

#ifndef NSTRACE_SIZE
#define NSTRACE_SIZE(str,size)
#endif

#ifndef NSTRACE_POINT
#define NSTRACE_POINT(str,point)
#endif

#ifndef NSTRACE_RECT
#define NSTRACE_RECT(str,rect)
#endif

#ifndef NSTRACE_FSTYPE
#define NSTRACE_FSTYPE(str,fs_type)
#endif

#ifndef NSTRACE_RETURN_SIZE
#define NSTRACE_RETURN_SIZE(size)
#endif

#ifndef NSTRACE_RETURN_POINT
#define NSTRACE_RETURN_POINT(point)
#endif

#ifndef NSTRACE_RETURN_RECT
#define NSTRACE_RETURN_RECT(rect)
#endif

#ifndef NSTRACE_RETURN_FSTYPE
#define NSTRACE_RETURN_FSTYPE(fs_type)
#endif

#ifndef NSTRACE_UNSILENCE
#define NSTRACE_UNSILENCE()
#endif


/* If the compiler doesn't support instancetype, map it to id.  */
#ifndef NATIVE_OBJC_INSTANCETYPE
typedef id instancetype;
#endif


/* ==========================================================================

   NSColor, EmacsColor category.

   ========================================================================== */
@interface NSColor (EmacsColor)
+ (NSColor *)colorForEmacsRed:(CGFloat)red green:(CGFloat)green
                         blue:(CGFloat)blue alpha:(CGFloat)alpha;
+ (NSColor *)colorWithUnsignedLong:(unsigned long)c;
- (NSColor *)colorUsingDefaultColorSpace;
- (unsigned long)unsignedLong;
@end


@interface NSString (EmacsString)
+ (NSString *)stringWithLispString:(Lisp_Object)string;
- (Lisp_Object)lispString;
@end

/* ==========================================================================

   The Emacs application

   ========================================================================== */

/* We override sendEvent: as a means to stop/start the event loop.  */
@interface EmacsApp : NSApplication
{
#ifdef NS_IMPL_COCOA
  BOOL shouldKeepRunning;
  BOOL isFirst;
#endif
#ifdef NS_IMPL_GNUSTEP
  BOOL applicationDidFinishLaunchingCalled;
#endif
@public
  int nextappdefined;
}
- (void)logNotification: (NSNotification *)notification;
- (void)antialiasThresholdDidChange:(NSNotification *)notification;
- (void)sendEvent: (NSEvent *)theEvent;
- (void)showPreferencesWindow: (id)sender;
- (BOOL) openFile: (NSString *)fileName;
- (void)fd_handler: (id)unused;
- (void)timeout_handler: (NSTimer *)timedEntry;
- (BOOL)fulfillService: (NSString *)name withArg: (NSString *)arg;
#ifdef NS_IMPL_GNUSTEP
- (void)sendFromMainThread:(id)unused;
#endif
@end

#ifdef NS_IMPL_GNUSTEP
/* Dummy class to get rid of startup warnings.  */
@interface EmacsDocument : NSDocument
{
}
@end
#endif

enum ns_return_frame_mode
  {
    RETURN_FRAME_NEVER,
    RETURN_FRAME_EVENTUALLY,
    RETURN_FRAME_NOW,
  };

/* EmacsWindow  */
@interface EmacsWindow : NSWindow
{
  NSPoint grabOffset;
  NSEvent *last_drag_event;
  NSDragOperation drag_op;
  NSDragOperation selected_op;

  struct frame *dnd_return_frame;
  enum ns_return_frame_mode dnd_mode;
  BOOL dnd_allow_same_frame;
  BOOL dnd_move_tooltip_with_frame;
}

#ifdef NS_IMPL_GNUSTEP
- (NSInteger) orderedIndex;
#endif

- (instancetype) initWithEmacsFrame: (struct frame *) f;
- (instancetype) initWithEmacsFrame: (struct frame *) f
			 fullscreen: (BOOL) fullscreen
			     screen: (NSScreen *) screen;
- (void) createToolbar: (struct frame *) f;
- (void) setParentChildRelationships;
- (NSInteger) borderWidth;
- (BOOL) restackWindow: (NSWindow *) win above: (BOOL) above;
- (void) setAppearance;
- (void) setLastDragEvent: (NSEvent *) event;
- (NSDragOperation) beginDrag: (NSDragOperation) op
		forPasteboard: (NSPasteboard *) pasteboard
		     withMode: (enum ns_return_frame_mode) mode
		returnFrameTo: (struct frame **) frame_return
		 prohibitSame: (BOOL) prohibit_same_frame
		followTooltip: (BOOL) follow_tooltip;
- (BOOL) mustNotDropOn: (NSView *) receiver;
@end


/* ==========================================================================

   The main Emacs view

   ========================================================================== */

@class EmacsToolbar;
@class EmacsLayer;

#ifdef NS_IMPL_COCOA
@interface EmacsView : NSView <NSTextInput, NSWindowDelegate>
#else
@interface EmacsView : NSView <NSTextInput>
#endif
{
#ifdef NS_IMPL_COCOA
  char *old_title;
  BOOL maximizing_resize;
#endif
  BOOL font_panel_active;
  NSFont *font_panel_result;
  BOOL windowClosing;
  NSString *workingText;
  BOOL processingCompose;
  int fs_state, fs_before_fs, next_maximized;
  int maximized_width, maximized_height;
  EmacsWindow *nonfs_window;
  BOOL fs_is_native;
@public
  struct frame *emacsframe;
  int scrollbarsNeedingUpdate;
  NSRect ns_userRect;
}

/* AppKit-side interface */
- (instancetype)menuDown: (id)sender;
- (instancetype)toolbarClicked: (id)item;
- (instancetype)toggleToolbar: (id)sender;
- (void)keyDown: (NSEvent *)theEvent;
- (void)mouseDown: (NSEvent *)theEvent;
- (void)mouseUp: (NSEvent *)theEvent;
- (instancetype)setMiniwindowImage: (BOOL)setMini;

/* Emacs-side interface */
- (instancetype) initFrameFromEmacs: (struct frame *) f;
- (void) setWindowClosing: (BOOL)closing;
- (void) deleteWorkingText;
- (void) handleFS;
- (void) setFSValue: (int)value;
- (void) toggleFullScreen: (id) sender;
- (BOOL) fsIsNative;
- (BOOL) isFullscreen;
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
- (void) updateCollectionBehavior;
#endif

#ifdef NS_IMPL_GNUSTEP
- (void)windowDidMove: (id)sender;
#endif
- (Lisp_Object) showFontPanel;
- (int)fullscreenState;

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
- (void)lockFocus;
- (void)unlockFocus;
#endif
- (void)copyRect:(NSRect)srcRect to:(NSPoint)dest;

/* Non-notification versions of NSView methods. Used for direct calls.  */
- (void)windowWillEnterFullScreen;
- (void)windowDidEnterFullScreen;
- (void)windowWillExitFullScreen;
- (void)windowDidExitFullScreen;
- (void)windowDidBecomeKey;
@end


/* ==========================================================================

   The main menu implementation

   ========================================================================== */

@interface EmacsMenu : NSMenu  <NSMenuDelegate>
{
  BOOL needsUpdate;
}

- (void)menuNeedsUpdate: (NSMenu *)menu; /* (delegate method) */
- (NSMenuItem *)addItemWithWidgetValue: (void *)wvptr
                            attributes: (NSDictionary *)attributes;
- (void)fillWithWidgetValue: (void *)wvptr;
- (EmacsMenu *)addSubmenuWithTitle: (const char *)title;
- (void) removeAllItems;
- (Lisp_Object)runMenuAt: (NSPoint)p forFrame: (struct frame *)f
                 keymaps: (bool)keymaps;
@end


/* ==========================================================================

   Toolbar

   ========================================================================== */

@class EmacsImage;

#ifdef NS_IMPL_COCOA
@interface EmacsToolbar : NSToolbar <NSToolbarDelegate>
#else
@interface EmacsToolbar : NSToolbar
#endif
   {
     EmacsView *emacsView;
     NSMutableDictionary *identifierToItem;
     NSMutableArray *activeIdentifiers;
     NSArray *prevIdentifiers;
     unsigned long enablement, prevEnablement;
   }
- (instancetype) initForView: (EmacsView *)view withIdentifier: (NSString *)identifier;
- (void) clearActive;
- (void) clearAll;
- (BOOL) changed;
- (void) addDisplayItemWithImage: (EmacsImage *)img
                             idx: (int)idx
                             tag: (int)tag
                       labelText: (NSString *)label
                        helpText: (NSString *)help
                         enabled: (BOOL)enabled;

/* delegate methods */
- (NSToolbarItem *)toolbar: (NSToolbar *)toolbar
     itemForItemIdentifier: (NSString *)itemIdentifier
 willBeInsertedIntoToolbar: (BOOL)flag;
- (NSArray *)toolbarDefaultItemIdentifiers: (NSToolbar *)toolbar;
- (NSArray *)toolbarAllowedItemIdentifiers: (NSToolbar *)toolbar;
@end


/* ==========================================================================

   Message / question windows

   ========================================================================== */

@interface EmacsDialogPanel : NSPanel
{
  NSTextField *command;
  NSTextField *title;
  NSMatrix *matrix;
  int rows, cols;
  BOOL timer_fired, window_closed;
  Lisp_Object dialog_return;
}

- (instancetype) initWithTitle: (char *) title_str
		    isQuestion: (BOOL) is_question;
- (void) processMenuItems: (Lisp_Object) menu_items
		     used: (ptrdiff_t) menu_items_used
	  withErrorOutput: (const char **) error_name;

- (void) addButton: (char *) str
	     value: (NSInteger) tag
	       row: (int) row
	    enable: (BOOL) enable;
- (void) addString: (char *) str
	       row: (int) row;
- (void) addSplit;
- (void) resizeBoundsPriorToDisplay;

- (Lisp_Object) runDialogAt: (NSPoint) p;
- (void) timeout_handler: (NSTimer *) timedEntry;
@end

#ifdef NS_IMPL_COCOA
@interface EmacsTooltip : NSObject <NSWindowDelegate>
#else
@interface EmacsTooltip : NSObject
#endif
{
  NSWindow *win;
  NSTextField *textField;
  NSTimer *timer;
}

- (instancetype) init;
- (void) setText: (char *) text;
- (void) setBackgroundColor: (NSColor *) col;
- (void) setForegroundColor: (NSColor *) col;
- (void) showAtX: (int) x Y: (int) y for: (int) seconds;
- (void) hide;
- (BOOL) isActive;
- (NSRect) frame;
- (void) moveTo: (NSPoint) screen_point;
@end


@interface EmacsFileDelegate : NSObject
{
}
- (BOOL)panel: (id)sender isValidFilename: (NSString *)filename;
- (BOOL)panel: (id)sender shouldShowFilename: (NSString *)filename;
- (NSString *)panel: (id)sender userEnteredFilename: (NSString *)filename
          confirmed: (BOOL)okFlag;
@end


/* ==========================================================================

   Images and stippling

   ========================================================================== */

@interface EmacsImage : NSImage
{
  NSBitmapImageRep *bmRep; /* used for accessing pixel data */
  unsigned char *pixmapData[5]; /* shortcut to access pixel data */
  NSColor *stippleMask;
@public
  NSAffineTransform *transform;
  BOOL smoothing;
}
+ (instancetype)allocInitFromFile: (Lisp_Object)file;
- (void)dealloc;
- (instancetype)initFromXBM: (unsigned char *)bits width: (int)w height: (int)h
                         fg: (unsigned long)fg bg: (unsigned long)bg
               reverseBytes: (BOOL)reverse;
- (instancetype)initForXPMWithDepth: (int)depth width: (int)width height: (int)height;
- (void)setPixmapData;
- (unsigned long)getPixelAtX: (int)x Y: (int)y;
- (void)setPixelAtX: (int)x Y: (int)y toRed: (unsigned char)r
               green: (unsigned char)g blue: (unsigned char)b
              alpha:(unsigned char)a;
- (void)setAlphaAtX: (int)x Y: (int)y to: (unsigned char)a;
- (NSColor *)stippleMask;
- (Lisp_Object)getMetadata;
- (BOOL)setFrame: (unsigned int) index;
- (void)setTransform: (double[3][3]) m;
- (void)setSmoothing: (BOOL)s;
- (size_t)sizeInBytes;
@end


/* ==========================================================================

   Scrollbars

   ========================================================================== */

@interface EmacsScroller : NSScroller
  {
   struct window *window;
   struct frame *frame;
   NSResponder *prevResponder;

   /* offset to the bottom of knob of last mouse down */
   CGFloat last_mouse_offset;
   float min_portion;
   int pixel_length;
   enum scroll_bar_part last_hit_part;

   BOOL condemned;

   BOOL horizontal;

   /* optimize against excessive positioning calls generated by emacs */
   int em_position;
   int em_portion;
   int em_whole;
   }

- (void) mark;
- (instancetype) initFrame: (NSRect )r window: (Lisp_Object)win;
- (void)setFrame: (NSRect)r;

- (instancetype) setPosition: (int) position portion: (int) portion whole: (int) whole;
- (int) checkSamePosition: (int)position portion: (int)portion
                    whole: (int)whole;
- (void) sendScrollEventAtLoc: (float)loc fromEvent: (NSEvent *)e;
- (instancetype)repeatScroll: (NSTimer *)sender;
- (instancetype)condemn;
- (instancetype)reprieve;
- (bool)judge;
+ (CGFloat)scrollerWidth;
@end

#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101400
@interface EmacsLayer : CALayer
{
  NSMutableArray *cache;
  CGColorSpaceRef colorSpace;
  IOSurfaceRef currentSurface;
  CGContextRef context;
}
- (id) initWithColorSpace: (CGColorSpaceRef)cs;
- (void) setColorSpace: (CGColorSpaceRef)cs;
- (CGContextRef) getContext;
@end
#endif


/* ==========================================================================

   Rendering

   ========================================================================== */

extern NSArray *ns_send_types, *ns_return_types;
extern NSString *ns_app_name;
extern EmacsMenu *svcsMenu;

/* Apple removed the declaration, but kept the implementation.  */
#if defined (NS_IMPL_COCOA)
@interface NSApplication (EmacsApp)
- (void)setAppleMenu: (NSMenu *)menu;
@end
#endif

#endif  /* __OBJC__ */



/* ==========================================================================

   Non-OO stuff

   ========================================================================== */

/* Special keycodes that we pass down the event chain */
#define KEY_NS_POWER_OFF               ((1<<28)|(0<<16)|1)
#define KEY_NS_OPEN_FILE               ((1<<28)|(0<<16)|2)
#define KEY_NS_OPEN_TEMP_FILE          ((1<<28)|(0<<16)|3)
#define KEY_NS_CHANGE_FONT             ((1<<28)|(0<<16)|7)
#define KEY_NS_OPEN_FILE_LINE          ((1<<28)|(0<<16)|8)
#define KEY_NS_PUT_WORKING_TEXT        ((1<<28)|(0<<16)|9)
#define KEY_NS_UNPUT_WORKING_TEXT      ((1<<28)|(0<<16)|10)
#define KEY_NS_SPI_SERVICE_CALL        ((1<<28)|(0<<16)|11)
#define KEY_NS_NEW_FRAME               ((1<<28)|(0<<16)|12)
#define KEY_NS_TOGGLE_TOOLBAR          ((1<<28)|(0<<16)|13)
#define KEY_NS_SHOW_PREFS              ((1<<28)|(0<<16)|14)

/* Could use list to store these, but rest of emacs has a big infrastructure
   for managing a table of bitmap "records".  */
struct ns_bitmap_record
{
#ifdef __OBJC__
  EmacsImage *img;
#else
  void *img;
#endif
  char *file;
  int refcount;
  int height, width, depth;
};

#ifdef NS_IMPL_GNUSTEP
/* this extends font backend font */
struct nsfont_info
{
  struct font font;

  char *name;  /* PostScript name, uniquely identifies on NS systems */

  /* The following metrics are stored as float rather than int.  */

  float width;  /* Maximum advance for the font.  */
  float height;
  float underpos;
  float underwidth;
  float size;
#ifdef __OBJC__
  NSFont *nsfont;
#else /* ! OBJC */
  void *nsfont;
#endif
  char bold, ital;  /* convenience flags */
  char synthItal;
  XCharStruct max_bounds;
  /* We compute glyph codes and metrics on-demand in blocks of 256 indexed
     by hibyte, lobyte.  */
  unsigned int **glyphs; /* map Unicode index to glyph */
  struct font_metrics **metrics;
};
#endif

/* Initialized in ns_initialize_display_info ().  */
struct ns_display_info
{
  /* Chain of all ns_display_info structures.  */
  struct ns_display_info *next;

  /* The generic display parameters corresponding to this NS display.  */
  struct terminal *terminal;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).  */
  Lisp_Object name_list_element;

  /* The number of fonts loaded.  */
  int n_fonts;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  struct ns_bitmap_record *bitmaps;
  ptrdiff_t bitmaps_size;
  ptrdiff_t bitmaps_last;

  /* DPI resolution of this screen */
  double resx, resy;

  /* Mask of things that cause the mouse to be grabbed */
  int grabbed;

  int n_planes;

  int color_p;

  Window root_window;

  /* Xism */
  Lisp_Object rdb;

  /* The cursor to use for vertical scroll bars.  */
  Emacs_Cursor vertical_scroll_bar_cursor;

  /* The cursor to use for horizontal scroll bars.  */
  Emacs_Cursor horizontal_scroll_bar_cursor;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  struct frame *highlight_frame;
  struct frame *ns_focus_frame;

  /* The frame where the mouse was last time we reported a mouse event.  */
  struct frame *last_mouse_frame;

  /* The frame where the mouse was last time we reported a mouse motion.  */
  struct frame *last_mouse_motion_frame;

  /* Position where the mouse was last time we reported a motion.
     This is a position on last_mouse_motion_frame.  */
  int last_mouse_motion_x;
  int last_mouse_motion_y;

  /* Where the mouse was last time we reported a mouse position.  */
  NSRect last_mouse_glyph;

  /* Time of last mouse movement.  */
  Time last_mouse_movement_time;

  /* The scroll bar in which the last motion event occurred.  */
#ifdef __OBJC__
  EmacsScroller *last_mouse_scroll_bar;
#else
  void *last_mouse_scroll_bar;
#endif
};

/* This is a chain of structures for all the NS displays currently in use.  */
extern struct ns_display_info *x_display_list;

struct ns_output
{
#ifdef __OBJC__
  EmacsView *view;
  id miniimage;
  NSColor *cursor_color;
  NSColor *foreground_color;
  NSColor *background_color;
  NSColor *relief_background_color;
  NSColor *light_relief_color;
  NSColor *dark_relief_color;
  EmacsToolbar *toolbar;
#else
  void *view;
  void *miniimage;
  void *cursor_color;
  void *foreground_color;
  void *background_color;
  void *relief_background_color;
  void *light_relief_color;
  void *dark_relief_color;
  void *toolbar;
#endif

  /* NSCursors are initialized in initFrameFromEmacs.  */
  Emacs_Cursor text_cursor;
  Emacs_Cursor nontext_cursor;
  Emacs_Cursor modeline_cursor;
  Emacs_Cursor hand_cursor;
  Emacs_Cursor hourglass_cursor;
  Emacs_Cursor horizontal_drag_cursor;
  Emacs_Cursor vertical_drag_cursor;
  Emacs_Cursor left_edge_cursor;
  Emacs_Cursor top_left_corner_cursor;
  Emacs_Cursor top_edge_cursor;
  Emacs_Cursor top_right_corner_cursor;
  Emacs_Cursor right_edge_cursor;
  Emacs_Cursor bottom_right_corner_cursor;
  Emacs_Cursor bottom_edge_cursor;
  Emacs_Cursor bottom_left_corner_cursor;

  /* NS-specific */
  Emacs_Cursor current_pointer;

  /* lord knows why Emacs needs to know about our Window ids.. */
  Window window_desc, parent_desc;
  char explicit_parent;

  struct font *font;
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset; /* only used with font_backend */

  int icon_top;
  int icon_left;

  /* The size of the extra width currently allotted for vertical
     scroll bars, in pixels.  */
  int vertical_scroll_bar_extra;

  /* The height of the titlebar decoration (included in NSWindow's frame).  */
  int titlebar_height;

  /* The height of the toolbar if displayed, else 0.  */
  int toolbar_height;

  /* This is the Emacs structure for the NS display this frame is on.  */
  struct ns_display_info *display_info;

  /* Non-zero if we are zooming (maximizing) the frame.  */
  int zooming;

  /* Non-zero if we are doing an animation, e.g. toggling the tool bar.  */
  int in_animation;

#ifdef NS_IMPL_GNUSTEP
  /* Zero if this is the first time a toolbar has been updated on this
     frame. */
  int tool_bar_adjusted;
#endif
};

/* This dummy declaration needed to support TTYs.  */
struct x_output
{
  int unused;
};


/* This gives the ns_display_info structure for the display F is on.  */
#define FRAME_DISPLAY_INFO(f) ((f)->output_data.ns->display_info)
#define FRAME_OUTPUT_DATA(f) ((f)->output_data.ns)
#define FRAME_NS_WINDOW(f) ((f)->output_data.ns->window_desc)
#define FRAME_NATIVE_WINDOW(f) FRAME_NS_WINDOW (f)

#define FRAME_FOREGROUND_COLOR(f) ((f)->output_data.ns->foreground_color)
#define FRAME_BACKGROUND_COLOR(f) ((f)->output_data.ns->background_color)

#define NS_FACE_FOREGROUND(f) ((f)->foreground)
#define NS_FACE_BACKGROUND(f) ((f)->background)

#define FRAME_DEFAULT_FACE(f) FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID)

#define FRAME_NS_VIEW(f) ((f)->output_data.ns->view)
#define FRAME_CURSOR_COLOR(f) ((f)->output_data.ns->cursor_color)
#define FRAME_POINTER_TYPE(f) ((f)->output_data.ns->current_pointer)

#define FRAME_FONT(f) ((f)->output_data.ns->font)

#ifdef __OBJC__
#define XNS_SCROLL_BAR(vec) ((id) xmint_pointer (vec))
#else
#define XNS_SCROLL_BAR(vec) xmint_pointer (vec)
#endif

/* Compute pixel height of the frame's titlebar.  */
#define FRAME_NS_TITLEBAR_HEIGHT(f)                                     \
  (NSHeight([FRAME_NS_VIEW (f) frame]) == 0 ?                           \
   0                                                                    \
   : (int)(NSHeight([FRAME_NS_VIEW (f) window].frame)                   \
           - NSHeight([NSWindow contentRectForFrameRect:                \
                       [[FRAME_NS_VIEW (f) window] frame]               \
                       styleMask:[[FRAME_NS_VIEW (f) window] styleMask]])))

/* Compute pixel height of the toolbar.  */
#define FRAME_TOOLBAR_HEIGHT(f)                                         \
  (([[FRAME_NS_VIEW (f) window] toolbar] == nil                         \
    || ! [[FRAME_NS_VIEW (f) window] toolbar].isVisible) ?		\
   0                                                                    \
   : (int)(NSHeight([NSWindow contentRectForFrameRect:                  \
                     [[FRAME_NS_VIEW (f) window] frame]                 \
                     styleMask:[[FRAME_NS_VIEW (f) window] styleMask]]) \
           - NSHeight([[[FRAME_NS_VIEW (f) window] contentView] frame])))

/* Compute pixel size for vertical scroll bars.  */
#define NS_SCROLL_BAR_WIDTH(f)						\
  (FRAME_HAS_VERTICAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_WIDTH (f)				\
	   : (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)))	\
   : 0)

/* Compute pixel size for horizontal scroll bars.  */
#define NS_SCROLL_BAR_HEIGHT(f)						\
  (FRAME_HAS_HORIZONTAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_HEIGHT (f)				\
	   : (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)))	\
   : 0)

/* Difference between char-column-calculated and actual SB widths.
   This is only a concern for rendering when SB on left.  */
#define NS_SCROLL_BAR_ADJUST(w, f)				\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) ?			\
   (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)		\
    - NS_SCROLL_BAR_WIDTH (f)) : 0)

/* Difference between char-line-calculated and actual SB heights.
   This is only a concern for rendering when SB on top.  */
#define NS_SCROLL_BAR_ADJUST_HORIZONTALLY(w, f)		\
  (WINDOW_HAS_HORIZONTAL_SCROLL_BARS (w) ?		\
   (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)	\
    - NS_SCROLL_BAR_HEIGHT (f)) : 0)

#define FRAME_NS_FONT_TABLE(f) (FRAME_DISPLAY_INFO (f)->font_table)

#define FRAME_FONTSET(f) ((f)->output_data.ns->fontset)

#define FRAME_BASELINE_OFFSET(f) ((f)->output_data.ns->baseline_offset)
#define BLACK_PIX_DEFAULT(f) 0x000000
#define WHITE_PIX_DEFAULT(f) 0xFFFFFF

/* First position where characters can be shown (instead of scrollbar, if
   it is on left.  */
#define FIRST_CHAR_POSITION(f)				\
  (! (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)) ? 0	\
   : FRAME_SCROLL_BAR_COLS (f))

extern struct ns_display_info *ns_term_init (Lisp_Object display_name);
extern void ns_term_shutdown (int sig);

/* constants for text rendering */
#define NS_DUMPGLYPH_NORMAL             0
#define NS_DUMPGLYPH_CURSOR             1
#define NS_DUMPGLYPH_FOREGROUND         2
#define NS_DUMPGLYPH_MOUSEFACE          3


#ifdef NS_IMPL_GNUSTEP
/* In nsfont, called from fontset.c */
extern void nsfont_make_fontset_for_font (Lisp_Object name,
                                         Lisp_Object font_object);

/* In nsfont, for debugging */
struct glyph_string;
void ns_dump_glyphstring (struct glyph_string *s) EXTERNALLY_VISIBLE;
#endif

/* Implemented in nsterm, published in or needed from nsfns.  */
extern Lisp_Object ns_list_fonts (struct frame *f, Lisp_Object pattern,
                                  int size, int maxnames);
extern void ns_clear_frame (struct frame *f);

extern void ns_set_offset (struct frame *f, int xoff, int yoff,
                           int change_grav);

extern const char *ns_xlfd_to_fontname (const char *xlfd);

extern Lisp_Object ns_map_event_to_object (void);
#ifdef __OBJC__
extern Lisp_Object ns_string_from_pasteboard (id pb);
extern void ns_string_to_pasteboard (id pb, Lisp_Object str);
#endif
extern Lisp_Object ns_get_local_selection (Lisp_Object selection_name,
                                           Lisp_Object target_type);
extern void nxatoms_of_nsselect (void);
extern void ns_set_doc_edited (void);

extern bool
ns_defined_color (struct frame *f,
                  const char *name,
                  Emacs_Color *color_def, bool alloc,
                  bool makeIndex);

#ifdef __OBJC__
extern int ns_lisp_to_color (Lisp_Object color, NSColor **col);
extern const char *ns_get_pending_menu_title (void);
#endif

/* Implemented in nsfns, published in nsterm.  */
#ifdef __OBJC__
extern void ns_move_tooltip_to_mouse_location (NSPoint);
#endif
extern void ns_implicitly_set_name (struct frame *f, Lisp_Object arg,
                                    Lisp_Object oldval);
extern void ns_set_scroll_bar_default_width (struct frame *f);
extern void ns_set_scroll_bar_default_height (struct frame *f);
extern void ns_change_tab_bar_height (struct frame *f, int height);
extern const char *ns_get_string_resource (void *_rdb,
                                           const char *name,
                                           const char *class);

/* C access to ObjC functionality */
extern void  ns_release_object (void *obj);
extern void  ns_retain_object (void *obj);
extern void *ns_alloc_autorelease_pool (void);
extern void ns_release_autorelease_pool (void *);
extern const char *ns_get_defaults_value (const char *key);
extern void ns_init_locale (void);

/* in nsmenu */
extern void update_frame_tool_bar (struct frame *f);
#ifdef __OBJC__
extern void update_frame_tool_bar_1 (struct frame *f, EmacsToolbar *toolbar);
#endif

extern void free_frame_tool_bar (struct frame *f);
extern Lisp_Object find_and_return_menu_selection (struct frame *f,
                                                   bool keymaps,
                                                   void *client_data);
extern Lisp_Object ns_popup_dialog (struct frame *, Lisp_Object header,
                                    Lisp_Object contents);

extern void ns_free_frame_resources (struct frame *);

#define NSAPP_DATA2_RUNASSCRIPT 10
extern void ns_run_ascript (void);

#define NSAPP_DATA2_RUNFILEDIALOG 11
extern void ns_run_file_dialog (void);

extern const char *ns_relocate (const char *epath);
extern void syms_of_nsterm (void);
extern void syms_of_nsfns (void);
extern void syms_of_nsmenu (void);
extern void syms_of_nsselect (void);

/* From nsimage.m, needed in image.c */
struct image;
extern bool ns_can_use_native_image_api (Lisp_Object type);
extern void *ns_image_from_XBM (char *bits, int width, int height,
                                unsigned long fg, unsigned long bg);
extern void *ns_image_for_XPM (int width, int height, int depth);
extern void *ns_image_from_file (Lisp_Object file);
extern bool ns_load_image (struct frame *f, struct image *img,
			   Lisp_Object spec_file, Lisp_Object spec_data);
extern int ns_image_width (void *img);
extern int ns_image_height (void *img);
extern void ns_image_set_size (void *img, int width, int height);
extern void ns_image_set_transform (void *img, double m[3][3]);
extern void ns_image_set_smoothing (void *img, bool smooth);
extern unsigned long ns_get_pixel (void *img, int x, int y);
extern void ns_put_pixel (void *img, int x, int y, unsigned long argb);
extern void ns_set_alpha (void *img, int x, int y, unsigned char a);

extern int ns_display_pixel_height (struct ns_display_info *);
extern int ns_display_pixel_width (struct ns_display_info *);
extern size_t ns_image_size_in_bytes (void *img);

/* This in nsterm.m */
extern float ns_antialias_threshold;
extern void ns_make_frame_visible (struct frame *f);
extern void ns_make_frame_invisible (struct frame *f);
extern void ns_iconify_frame (struct frame *f);
extern void ns_set_undecorated (struct frame *f, Lisp_Object new_value,
                                Lisp_Object old_value);
extern void ns_set_parent_frame (struct frame *f, Lisp_Object new_value,
                                 Lisp_Object old_value);
extern void ns_set_no_focus_on_map (struct frame *f, Lisp_Object new_value,
                                    Lisp_Object old_value);
extern void ns_set_no_accept_focus (struct frame *f, Lisp_Object new_value,
                                    Lisp_Object old_value);
extern void ns_set_z_group (struct frame *f, Lisp_Object new_value,
                            Lisp_Object old_value);
#ifdef NS_IMPL_COCOA
extern void ns_set_appearance (struct frame *f, Lisp_Object new_value,
                               Lisp_Object old_value);
extern void ns_set_transparent_titlebar (struct frame *f,
                                         Lisp_Object new_value,
                                         Lisp_Object old_value);
#endif
extern int ns_select (int nfds, fd_set *readfds, fd_set *writefds,
		      fd_set *exceptfds, struct timespec *timeout,
		      sigset_t *sigmask);
#ifdef HAVE_PTHREAD
extern void ns_run_loop_break (void);
#endif
extern unsigned long ns_get_rgb_color (struct frame *f,
                                       float r, float g, float b, float a);

struct input_event;
extern void ns_init_events (struct input_event *);
extern void ns_finish_events (void);

extern double ns_frame_scale_factor (struct frame *);

#ifdef NS_IMPL_GNUSTEP
extern char gnustep_base_version[];  /* version tracking */
#endif

#define MINWIDTH 10
#define MINHEIGHT 10

/* Screen max coordinate -- using larger coordinates causes
   movewindow/placewindow to abort.  */
#define SCREENMAX 16000

#define NS_SCROLL_BAR_WIDTH_DEFAULT     [EmacsScroller scrollerWidth]
#define NS_SCROLL_BAR_HEIGHT_DEFAULT    [EmacsScroller scrollerHeight]
/* This is to match emacs on other platforms, ugly though it is.  */
#define NS_SELECTION_BG_COLOR_DEFAULT	@"LightGoldenrod2";
#define NS_SELECTION_FG_COLOR_DEFAULT	@"Black";
#define RESIZE_HANDLE_SIZE 12

/* Little utility macros */
#define IN_BOUND(min, x, max) (((x) < (min)) \
                                ? (min) : (((x)>(max)) ? (max) : (x)))
#define SCREENMAXBOUND(x) (IN_BOUND (-SCREENMAX, x, SCREENMAX))


#ifdef NS_IMPL_COCOA
/* Add some required AppKit version numbers if they're not defined.  */
#ifndef NSAppKitVersionNumber10_7
#define NSAppKitVersionNumber10_7 1138
#endif

#ifndef NSAppKitVersionNumber10_10
#define NSAppKitVersionNumber10_10 1343
#endif
#endif /* NS_IMPL_COCOA */


/* macOS 10.7 introduces some new constants.  */
#if !defined (NS_IMPL_COCOA) || !defined (MAC_OS_X_VERSION_10_7)
#define NSFullScreenWindowMask                      (1 << 14)
#define NSWindowCollectionBehaviorFullScreenPrimary (1 << 7)
#define NSWindowCollectionBehaviorFullScreenAuxiliary (1 << 8)
#define NSApplicationPresentationFullScreen         (1 << 10)
#define NSApplicationPresentationAutoHideToolbar    (1 << 11)
#define NSAppKitVersionNumber10_7                   1138
#endif /* !defined (MAC_OS_X_VERSION_10_7) */

/* macOS 10.12 deprecates a bunch of constants.  */
#if !defined (NS_IMPL_COCOA) || !defined (MAC_OS_X_VERSION_10_12)
#define NSEventModifierFlagCommand         NSCommandKeyMask
#define NSEventModifierFlagControl         NSControlKeyMask
#define NSEventModifierFlagHelp            NSHelpKeyMask
#define NSEventModifierFlagNumericPad      NSNumericPadKeyMask
#define NSEventModifierFlagOption          NSAlternateKeyMask
#define NSEventModifierFlagShift           NSShiftKeyMask
#define NSCompositingOperationSourceOver   NSCompositeSourceOver
#define NSEventMaskApplicationDefined      NSApplicationDefinedMask
#define NSEventTypeApplicationDefined      NSApplicationDefined
#define NSEventTypeCursorUpdate            NSCursorUpdate
#define NSEventTypeMouseMoved              NSMouseMoved
#define NSEventTypeLeftMouseDown           NSLeftMouseDown
#define NSEventTypeRightMouseDown          NSRightMouseDown
#define NSEventTypeOtherMouseDown          NSOtherMouseDown
#define NSEventTypeLeftMouseUp             NSLeftMouseUp
#define NSEventTypeRightMouseUp            NSRightMouseUp
#define NSEventTypeOtherMouseUp            NSOtherMouseUp
#define NSEventTypeLeftMouseDragged        NSLeftMouseDragged
#define NSEventTypeRightMouseDragged       NSRightMouseDragged
#define NSEventTypeOtherMouseDragged       NSOtherMouseDragged
#define NSEventTypeScrollWheel             NSScrollWheel
#define NSEventTypeKeyDown                 NSKeyDown
#define NSEventTypeKeyUp                   NSKeyUp
#define NSEventTypeFlagsChanged            NSFlagsChanged
#define NSEventMaskAny                     NSAnyEventMask
#define NSEventTypeSystemDefined           NSSystemDefined
#define NSWindowStyleMaskBorderless        NSBorderlessWindowMask
#define NSWindowStyleMaskClosable          NSClosableWindowMask
#define NSWindowStyleMaskFullScreen        NSFullScreenWindowMask
#define NSWindowStyleMaskMiniaturizable    NSMiniaturizableWindowMask
#define NSWindowStyleMaskResizable         NSResizableWindowMask
#define NSWindowStyleMaskTitled            NSTitledWindowMask
#define NSWindowStyleMaskUtilityWindow     NSUtilityWindowMask
#define NSAlertStyleCritical               NSCriticalAlertStyle
#define NSControlSizeRegular               NSRegularControlSize
#define NSCompositingOperationCopy         NSCompositeCopy
#define NSTextAlignmentRight               NSRightTextAlignment

/* And adds NSWindowStyleMask.  */
#ifdef __OBJC__
typedef NSUInteger NSWindowStyleMask;
#endif

/* Window tabbing mode enums are new too.  */
enum NSWindowTabbingMode
  {
    NSWindowTabbingModeAutomatic,
    NSWindowTabbingModePreferred,
    NSWindowTabbingModeDisallowed
  };
#endif /* !defined (NS_IMPL_COCOA) || !defined (MAC_OS_X_VERSION_10_12)  */

#if !defined (NS_IMPL_COCOA) || !defined (MAC_OS_X_VERSION_10_13)
/* Deprecated in macOS 10.13.  */
#define NSPasteboardNameGeneral NSGeneralPboard
#define NSPasteboardNameDrag NSDragPboard
#endif

#if !defined (NS_IMPL_COCOA) || !defined (MAC_OS_X_VERSION_10_14)
/* Deprecated in macOS 10.14.  */
/* FIXME: Some of these new names, if not all, are actually available
   in some recent version of GNUstep.  */
#define NSPasteboardTypeString NSStringPboardType
#define NSPasteboardTypeTabularText NSTabularTextPboardType
#define NSPasteboardTypeURL NSURLPboardType
#define NSPasteboardTypeHTML NSHTMLPboardType
#define NSPasteboardTypePDF NSPDFPboardType
#define NSPasteboardTypeRTF NSRTFPboardType
#define NSPasteboardTypeRTFD NSRTFDPboardType
#define NSPasteboardTypeTIFF NSTIFFPboardType
#define NSControlStateValueOn NSOnState
#define NSControlStateValueOff NSOffState
#define NSBezelStyleRounded NSRoundedBezelStyle
#define NSButtonTypeMomentaryPushIn NSMomentaryPushInButton
#endif

extern void mark_nsterm (void);

#endif	/* HAVE_NS */
