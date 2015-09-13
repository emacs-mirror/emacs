/* Definitions and headers for communication with NeXT/Open/GNUstep API.
   Copyright (C) 1989, 1993, 2005, 2008-2015 Free Software Foundation,
   Inc.

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


#include "dispextern.h"
#include "frame.h"
#include "character.h"
#include "font.h"
#include "sysselect.h"

#ifdef HAVE_NS

#ifdef NS_IMPL_COCOA
#ifndef MAC_OS_X_VERSION_10_6
#define MAC_OS_X_VERSION_10_6 1060
#endif
#ifndef MAC_OS_X_VERSION_10_7
#define MAC_OS_X_VERSION_10_7 1070
#endif
#ifndef MAC_OS_X_VERSION_10_8
#define MAC_OS_X_VERSION_10_8 1080
#endif
#ifndef MAC_OS_X_VERSION_10_9
#define MAC_OS_X_VERSION_10_9 1090
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7
#define HAVE_NATIVE_FS
#endif

#endif /* NS_IMPL_COCOA */

#ifdef __OBJC__

/* CGFloat on GNUstep may be 4 or 8 byte, but functions expect float* for some
   versions.
   On Cocoa >= 10.5, functions expect CGFloat*. Make compatible type.  */
#ifdef NS_IMPL_COCOA
typedef CGFloat EmacsCGFloat;
#elif GNUSTEP_GUI_MAJOR_VERSION > 0 || GNUSTEP_GUI_MINOR_VERSION >= 22
typedef CGFloat EmacsCGFloat;
#else
typedef float EmacsCGFloat;
#endif

/* ==========================================================================

   NSColor, EmacsColor category.

   ========================================================================== */
@interface NSColor (EmacsColor)
+ (NSColor *)colorForEmacsRed:(CGFloat)red green:(CGFloat)green
                         blue:(CGFloat)blue alpha:(CGFloat)alpha;
- (NSColor *)colorUsingDefaultColorSpace;

@end

/* ==========================================================================

   The Emacs application

   ========================================================================== */

/* We override sendEvent: as a means to stop/start the event loop */
@interface EmacsApp : NSApplication
{
#ifdef NS_IMPL_COCOA
  BOOL shouldKeepRunning;
  BOOL isFirst;
#endif
#ifdef NS_IMPL_GNUSTEP
  BOOL applicationDidFinishLaunchingCalled;
@public
  int nextappdefined;
#endif
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

/* ==========================================================================

   The main Emacs view

   ========================================================================== */

@class EmacsToolbar;

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
   BOOL windowClosing;
   NSString *workingText;
   BOOL processingCompose;
   int fs_state, fs_before_fs, next_maximized;
   int tibar_height, tobar_height, bwidth;
   int maximized_width, maximized_height;
   NSWindow *nonfs_window;
   BOOL fs_is_native;
@public
   struct frame *emacsframe;
   int rows, cols;
   int scrollbarsNeedingUpdate;
   EmacsToolbar *toolbar;
   NSRect ns_userRect;
   BOOL wait_for_tool_bar;
   }

/* AppKit-side interface */
- menuDown: (id)sender;
- toolbarClicked: (id)item;
- toggleToolbar: (id)sender;
- (void)keyDown: (NSEvent *)theEvent;
- (void)mouseDown: (NSEvent *)theEvent;
- (void)mouseUp: (NSEvent *)theEvent;
- setMiniwindowImage: (BOOL)setMini;

/* Emacs-side interface */
- initFrameFromEmacs: (struct frame *) f;
- (void) setRows: (int) r andColumns: (int) c;
- (void) setWindowClosing: (BOOL)closing;
- (EmacsToolbar *) toolbar;
- (void) deleteWorkingText;
- (void) updateFrameSize: (BOOL) delay;
- (void) handleFS;
- (void) setFSValue: (int)value;
- (void) toggleFullScreen: (id) sender;
- (BOOL) fsIsNative;
- (BOOL) isFullscreen;
#ifdef HAVE_NATIVE_FS
- (void) updateCollectionBehavior;
#endif

#ifdef NS_IMPL_GNUSTEP
- (void)windowDidMove: (id)sender;
#endif
@end


/* Small utility used for processing resize events under Cocoa. */
@interface EmacsWindow : NSWindow
{
  NSPoint grabOffset;
}
@end


/* Fullscreen version of the above.  */
@interface EmacsFSWindow : EmacsWindow
{
}
@end

/* ==========================================================================

   The main menu implementation

   ========================================================================== */

#ifdef NS_IMPL_COCOA
@interface EmacsMenu : NSMenu  <NSMenuDelegate>
#else
@interface EmacsMenu : NSMenu
#endif
{
  struct frame *frame;
  unsigned long keyEquivModMask;
}

- initWithTitle: (NSString *)title frame: (struct frame *)f;
- (void)setFrame: (struct frame *)f;
- (void)menuNeedsUpdate: (NSMenu *)menu; /* (delegate method) */
- (NSString *)parseKeyEquiv: (const char *)key;
- (NSMenuItem *)addItemWithWidgetValue: (void *)wvptr;
- (void)fillWithWidgetValue: (void *)wvptr;
- (void)fillWithWidgetValue: (void *)wvptr frame: (struct frame *)f;
- (EmacsMenu *)addSubmenuWithTitle: (const char *)title forFrame: (struct frame *)f;
- (void) clear;
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
- initForView: (EmacsView *)view withIdentifier: (NSString *)identifier;
- (void) clearActive;
- (void) clearAll;
- (BOOL) changed;
- (void) addDisplayItemWithImage: (EmacsImage *)img
                             idx: (int)idx
                             tag: (int)tag
                        helpText: (const char *)help
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
   Lisp_Object *button_values;
   }
- initFromContents: (Lisp_Object)menu isQuestion: (BOOL)isQ;
- (void)process_dialog: (Lisp_Object)list;
- (void)addButton: (char *)str value: (int)tag row: (int)row;
- (void)addString: (char *)str row: (int)row;
- (void)addSplit;
- (Lisp_Object)runDialogAt: (NSPoint)p;
- (void)timeout_handler: (NSTimer *)timedEntry;
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
- init;
- (void) setText: (char *)text;
- (void) showAtX: (int)x Y: (int)y for: (int)seconds;
- (void) hide;
- (BOOL) isActive;
- (NSRect) frame;
@end


/* ==========================================================================

   File open/save panels
   This and next override methods to handle keyboard input in panels.

   ========================================================================== */

@interface EmacsSavePanel : NSSavePanel
{
}
@end
@interface EmacsOpenPanel : NSOpenPanel
{
}
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
  unsigned long xbm_fg;
}
+ allocInitFromFile: (Lisp_Object)file;
- (void)dealloc;
- initFromXBM: (unsigned char *)bits width: (int)w height: (int)h
                  fg: (unsigned long)fg bg: (unsigned long)bg;
- setXBMColor: (NSColor *)color;
- initForXPMWithDepth: (int)depth width: (int)width height: (int)height;
- (void)setPixmapData;
- (unsigned long)getPixelAtX: (int)x Y: (int)y;
- (void)setPixelAtX: (int)x Y: (int)y toRed: (unsigned char)r
               green: (unsigned char)g blue: (unsigned char)b
              alpha:(unsigned char)a;
- (void)setAlphaAtX: (int)x Y: (int)y to: (unsigned char)a;
- (NSColor *)stippleMask;
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
   int pixel_height;
   enum scroll_bar_part last_hit_part;

   BOOL condemned;

   /* optimize against excessive positioning calls generated by emacs */
   int em_position;
   int em_portion;
   int em_whole;
   }

- initFrame: (NSRect )r window: (Lisp_Object)win;
- (void)setFrame: (NSRect)r;

- setPosition: (int) position portion: (int) portion whole: (int) whole;
- (int) checkSamePosition: (int)position portion: (int)portion
                    whole: (int)whole;
- (void) sendScrollEventAtLoc: (float)loc fromEvent: (NSEvent *)e;
- repeatScroll: (NSTimer *)sender;
- condemn;
- reprieve;
- (bool)judge;
+ (CGFloat)scrollerWidth;
@end


/* ==========================================================================

   Rendering

   ========================================================================== */

#ifdef NS_IMPL_COCOA
/* rendering util */
@interface EmacsGlyphStorage : NSObject <NSGlyphStorage>
{
@public
  NSAttributedString *attrStr;
  NSMutableDictionary *dict;
  CGGlyph *cglyphs;
  unsigned long maxChar, maxGlyph;
  long i, len;
}
- initWithCapacity: (unsigned long) c;
- (void) setString: (NSString *)str font: (NSFont *)font;
@end
#endif	/* NS_IMPL_COCOA */

extern NSArray *ns_send_types, *ns_return_types;
extern NSString *ns_app_name;
extern EmacsMenu *mainMenu, *svcsMenu, *dockMenu;

/* Apple removed the declaration, but kept the implementation */
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

/* could use list to store these, but rest of emacs has a big infrastructure
   for managing a table of bitmap "records" */
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

/* this to map between emacs color indices and NSColor objects */
struct ns_color_table
{
  ptrdiff_t size;
  ptrdiff_t avail;
#ifdef __OBJC__
  NSColor **colors;
  NSMutableSet *empty_indices;
#else
  void **items;
  void *availIndices;
#endif
};
#define NS_COLOR_CAPACITY 256

#define RGB_TO_ULONG(r, g, b) (((r) << 16) | ((g) << 8) | (b))
#define ARGB_TO_ULONG(a, r, g, b) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b))

#define ALPHA_FROM_ULONG(color) ((color) >> 24)
#define RED_FROM_ULONG(color) (((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color) (((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color) ((color) & 0xff)

/* Do not change `* 0x101' in the following lines to `<< 8'.  If
   changed, image masks in 1-bit depth will not work. */
#define RED16_FROM_ULONG(color) (RED_FROM_ULONG(color) * 0x101)
#define GREEN16_FROM_ULONG(color) (GREEN_FROM_ULONG(color) * 0x101)
#define BLUE16_FROM_ULONG(color) (BLUE_FROM_ULONG(color) * 0x101)

/* this extends font backend font */
struct nsfont_info
{
  struct font font;

  char *name;  /* PostScript name, uniquely identifies on NS systems */

  /* The following metrics are stored as float rather than int. */

  float width;  /* Maximum advance for the font.  */
  float height;
  float underpos;
  float underwidth;
  float size;
#ifdef __OBJC__
  NSFont *nsfont;
#if defined (NS_IMPL_COCOA)
  CGFontRef cgfont;
#else /* GNUstep */
  void *cgfont;
#endif
#else /* ! OBJC */
  void *nsfont;
  void *cgfont;
#endif
  char bold, ital;  /* convenience flags */
  char synthItal;
  XCharStruct max_bounds;
  /* we compute glyph codes and metrics on-demand in blocks of 256 indexed
     by hibyte, lobyte */
  unsigned short **glyphs; /* map Unicode index to glyph */
  struct font_metrics **metrics;
};


/* init'd in ns_initialize_display_info () */
struct ns_display_info
{
  /* Chain of all ns_display_info structures.  */
  struct ns_display_info *next;

  /* The generic display parameters corresponding to this NS display. */
  struct terminal *terminal;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).  */
  Lisp_Object name_list_element;

  /* The number of fonts loaded. */
  int n_fonts;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  struct ns_bitmap_record *bitmaps;
  ptrdiff_t bitmaps_size;
  ptrdiff_t bitmaps_last;

  struct ns_color_table *color_table;

  /* DPI resolution of this screen */
  double resx, resy;

  /* Mask of things that cause the mouse to be grabbed */
  int grabbed;

  int n_planes;

  int color_p;

  Window root_window;

  /* Xism */
  XrmDatabase xrdb;

  /* The cursor to use for vertical scroll bars. */
  Cursor vertical_scroll_bar_cursor;

  /* The cursor to use for horizontal scroll bars. */
  Cursor horizontal_scroll_bar_cursor;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  struct frame *x_highlight_frame;
  struct frame *x_focus_frame;

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

extern struct ns_display_info *ns_display_info_for_name (Lisp_Object name);

struct ns_output
{
#ifdef __OBJC__
  EmacsView *view;
  id miniimage;
  NSColor *cursor_color;
  NSColor *foreground_color;
  NSColor *background_color;
  EmacsToolbar *toolbar;
#else
  void *view;
  void *miniimage;
  void *cursor_color;
  void *foreground_color;
  void *background_color;
  void *toolbar;
#endif

  /* NSCursors init'ed in initFrameFromEmacs */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor hand_cursor;
  Cursor hourglass_cursor;
  Cursor horizontal_drag_cursor;
  Cursor vertical_drag_cursor;

  /* NS-specific */
  Cursor current_pointer;

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

  /* The height of the titlebar decoration (included in NSWindow's frame). */
  int titlebar_height;

  /* The height of the toolbar if displayed, else 0. */
  int toolbar_height;

  /* This is the Emacs structure for the NS display this frame is on.  */
  struct ns_display_info *display_info;

  /* Non-zero if we are zooming (maximizing) the frame.  */
  int zooming;
};

/* this dummy decl needed to support TTYs */
struct x_output
{
  int unused;
};


/* This gives the ns_display_info structure for the display F is on.  */
#define FRAME_DISPLAY_INFO(f) ((f)->output_data.ns->display_info)
#define FRAME_X_OUTPUT(f) ((f)->output_data.ns)
#define FRAME_NS_WINDOW(f) ((f)->output_data.ns->window_desc)
#define FRAME_X_WINDOW(f) ((f)->output_data.ns->window_desc)

/* This is the `Display *' which frame F is on.  */
#define FRAME_NS_DISPLAY(f) (0)
#define FRAME_X_DISPLAY(f) (0)
#define FRAME_X_SCREEN(f) (0)
#define FRAME_X_VISUAL(f) FRAME_DISPLAY_INFO(f)->visual

#define FRAME_FOREGROUND_COLOR(f) ((f)->output_data.ns->foreground_color)
#define FRAME_BACKGROUND_COLOR(f) ((f)->output_data.ns->background_color)

#define NS_FACE_FOREGROUND(f) ((f)->foreground)
#define NS_FACE_BACKGROUND(f) ((f)->background)
#define FRAME_NS_TITLEBAR_HEIGHT(f) ((f)->output_data.ns->titlebar_height)
#define FRAME_TOOLBAR_HEIGHT(f) ((f)->output_data.ns->toolbar_height)

#define FRAME_DEFAULT_FACE(f) FACE_FROM_ID (f, DEFAULT_FACE_ID)

#define FRAME_NS_VIEW(f) ((f)->output_data.ns->view)
#define FRAME_CURSOR_COLOR(f) ((f)->output_data.ns->cursor_color)
#define FRAME_POINTER_TYPE(f) ((f)->output_data.ns->current_pointer)

#define FRAME_FONT(f) ((f)->output_data.ns->font)

#ifdef __OBJC__
#define XNS_SCROLL_BAR(vec) ((id) XSAVE_POINTER (vec, 0))
#else
#define XNS_SCROLL_BAR(vec) XSAVE_POINTER (vec, 0)
#endif

/* Compute pixel size for vertical scroll bars */
#define NS_SCROLL_BAR_WIDTH(f)						\
  (FRAME_HAS_VERTICAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_WIDTH (f)				\
	   : (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)))	\
   : 0)

/* Compute pixel size for horizontal scroll bars */
#define NS_SCROLL_BAR_HEIGHT(f)						\
  (FRAME_HAS_HORIZONTAL_SCROLL_BARS (f)					\
   ? rint (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0			\
	   ? FRAME_CONFIG_SCROLL_BAR_HEIGHT (f)				\
	   : (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)))	\
   : 0)

/* Difference btwn char-column-calculated and actual SB widths.
   This is only a concern for rendering when SB on left. */
#define NS_SCROLL_BAR_ADJUST(w, f)		\
(WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) ?	\
    (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)	\
        - NS_SCROLL_BAR_WIDTH (f)) : 0)

/* Difference btwn char-line-calculated and actual SB heights.
   This is only a concern for rendering when SB on top. */
#define NS_SCROLL_BAR_ADJUST_HORIZONTALLY(w, f)		\
  (WINDOW_HAS_HORIZONTAL_SCROLL_BARS (w) ?		\
   (FRAME_SCROLL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)	\
    - NS_SCROLL_BAR_HEIGHT (f)) : 0)

/* XXX: fix for GNUstep inconsistent accounting for titlebar */
#ifdef NS_IMPL_GNUSTEP
#define NS_TOP_POS(f) ((f)->top_pos + 18)
#else
#define NS_TOP_POS(f) ((f)->top_pos)
#endif

#define FRAME_NS_FONT_TABLE(f) (FRAME_DISPLAY_INFO (f)->font_table)

#define FRAME_FONTSET(f) ((f)->output_data.ns->fontset)

#define FRAME_BASELINE_OFFSET(f) ((f)->output_data.ns->baseline_offset)
#define BLACK_PIX_DEFAULT(f) 0x000000
#define WHITE_PIX_DEFAULT(f) 0xFFFFFF

/* First position where characters can be shown (instead of scrollbar, if
   it is on left. */
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



/* In nsfont, called from fontset.c */
extern void nsfont_make_fontset_for_font (Lisp_Object name,
                                         Lisp_Object font_object);

/* In nsfont, for debugging */
struct glyph_string;
void ns_dump_glyphstring (struct glyph_string *s);

/* Implemented in nsterm, published in or needed from nsfns. */
extern Lisp_Object ns_list_fonts (struct frame *f, Lisp_Object pattern,
                                  int size, int maxnames);
extern void ns_clear_frame (struct frame *f);

extern const char *ns_xlfd_to_fontname (const char *xlfd);

extern Lisp_Object ns_map_event_to_object (void);
#ifdef __OBJC__
extern Lisp_Object ns_string_from_pasteboard (id pb);
extern void ns_string_to_pasteboard (id pb, Lisp_Object str);
#endif
extern Lisp_Object ns_get_local_selection (Lisp_Object selection_name,
                                           Lisp_Object target_type);
extern void nxatoms_of_nsselect (void);
extern int ns_lisp_to_cursor_type (Lisp_Object arg);
extern Lisp_Object ns_cursor_type_to_lisp (int arg);
extern void ns_set_name_as_filename (struct frame *f);
extern void ns_set_doc_edited (void);

extern bool
ns_defined_color (struct frame *f,
                  const char *name,
                  XColor *color_def, bool alloc,
                  bool makeIndex);
extern void
ns_query_color (void *col, XColor *color_def, int setPixel);

#ifdef __OBJC__
extern Lisp_Object ns_color_to_lisp (NSColor *col);
extern int ns_lisp_to_color (Lisp_Object color, NSColor **col);
extern NSColor *ns_lookup_indexed_color (unsigned long idx, struct frame *f);
extern unsigned long ns_index_color (NSColor *color, struct frame *f);
extern void ns_free_indexed_color (unsigned long idx, struct frame *f);
extern const char *ns_get_pending_menu_title (void);
extern void ns_check_menu_open (NSMenu *menu);
extern void ns_check_pending_open_menu (void);
#endif

/* C access to ObjC functionality */
extern void  ns_release_object (void *obj);
extern void  ns_retain_object (void *obj);
extern void *ns_alloc_autorelease_pool (void);
extern void ns_release_autorelease_pool (void *);
extern const char *ns_get_defaults_value (const char *key);

/* in nsmenu */
extern void update_frame_tool_bar (struct frame *f);
extern void free_frame_tool_bar (struct frame *f);
extern void find_and_call_menu_selection (struct frame *f,
    int menu_bar_items_used, Lisp_Object vector, void *client_data);
extern Lisp_Object find_and_return_menu_selection (struct frame *f,
                                                   bool keymaps,
                                                   void *client_data);
extern Lisp_Object ns_popup_dialog (struct frame *, Lisp_Object header,
                                    Lisp_Object contents);

#define NSAPP_DATA2_RUNASSCRIPT 10
extern void ns_run_ascript (void);

#define NSAPP_DATA2_RUNFILEDIALOG 11
extern void ns_run_file_dialog (void);

extern const char *ns_etc_directory (void);
extern const char *ns_exec_path (void);
extern const char *ns_load_path (void);
extern void syms_of_nsterm (void);
extern void syms_of_nsfns (void);
extern void syms_of_nsmenu (void);
extern void syms_of_nsselect (void);

/* From nsimage.m, needed in image.c */
struct image;
extern void *ns_image_from_XBM (unsigned char *bits, int width, int height,
                                unsigned long fg, unsigned long bg);
extern void *ns_image_for_XPM (int width, int height, int depth);
extern void *ns_image_from_file (Lisp_Object file);
extern bool ns_load_image (struct frame *f, struct image *img,
			   Lisp_Object spec_file, Lisp_Object spec_data);
extern int ns_image_width (void *img);
extern int ns_image_height (void *img);
extern unsigned long ns_get_pixel (void *img, int x, int y);
extern void ns_put_pixel (void *img, int x, int y, unsigned long argb);
extern void ns_set_alpha (void *img, int x, int y, unsigned char a);

extern int x_display_pixel_height (struct ns_display_info *);
extern int x_display_pixel_width (struct ns_display_info *);

/* This in nsterm.m */
extern void x_destroy_window (struct frame *f);
extern int ns_select (int nfds, fd_set *readfds, fd_set *writefds,
		      fd_set *exceptfds, struct timespec const *timeout,
		      sigset_t const *sigmask);
extern unsigned long ns_get_rgb_color (struct frame *f,
                                       float r, float g, float b, float a);

extern void ns_init_events ();
extern void ns_finish_events ();

#ifdef __OBJC__
/* From nsterm.m, needed in nsfont.m. */
extern void
ns_draw_text_decoration (struct glyph_string *s, struct face *face,
                         NSColor *defaultCol, CGFloat width, CGFloat x);
/* Needed in nsfns.m.  */
extern void
ns_set_represented_filename (NSString* fstr, struct frame *f);

#endif

#ifdef NS_IMPL_GNUSTEP
extern char gnustep_base_version[];  /* version tracking */
#endif

#define MINWIDTH 10
#define MINHEIGHT 10

/* Screen max coordinate
 Using larger coordinates causes movewindow/placewindow to abort */
#define SCREENMAX 16000

#define NS_SCROLL_BAR_WIDTH_DEFAULT     [EmacsScroller scrollerWidth]
#define NS_SCROLL_BAR_HEIGHT_DEFAULT    [EmacsScroller scrollerHeight]
/* This is to match emacs on other platforms, ugly though it is. */
#define NS_SELECTION_BG_COLOR_DEFAULT	@"LightGoldenrod2";
#define NS_SELECTION_FG_COLOR_DEFAULT	@"Black";
#define RESIZE_HANDLE_SIZE 12

/* Little utility macros */
#define IN_BOUND(min, x, max) (((x) < (min)) \
                                ? (min) : (((x)>(max)) ? (max) : (x)))
#define SCREENMAXBOUND(x) (IN_BOUND (-SCREENMAX, x, SCREENMAX))

#endif	/* HAVE_NS */
