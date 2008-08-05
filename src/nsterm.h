/* Definitions and headers for communication with NeXT/Open/GNUstep API.
   Copyright (C) 1989, 1993, 2005, 2008 Free Software Foundation, Inc.

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

#ifdef HAVE_NS

#ifdef __OBJC__

/* ==========================================================================

   The Emacs application

   ========================================================================== */

/* We override sendEvent: as a means to stop/start the event loop */
@interface EmacsApp : NSApplication
{
}
- (void)logNotification: (NSNotification *)notification;
- (void)sendEvent: (NSEvent *)theEvent;
- (void)showPreferencesWindow: (id)sender;
- (BOOL) openFile: (NSString *)fileName;
- (void)fd_handler: (NSTimer *) fdEntry;
- (void)cursor_blink_handler: (NSTimer *)cursorEntry;
- (void)timeout_handler: (NSTimer *)timedEntry;
- (BOOL)fulfillService: (NSString *)name withArg: (NSString *)arg;
@end


/* ==========================================================================

   The main Emacs view

   ========================================================================== */

@class EmacsToolbar;

@interface EmacsView : NSView <NSTextInput>
   {
   char *old_title;
   BOOL windowClosing;
   NSString *workingText;
   BOOL processingCompose;
@public
   struct frame *emacsframe;
   int rows, cols;
   int scrollbarsNeedingUpdate;
   EmacsToolbar *toolbar;
   }

/* AppKit-side interface */
- menuDown: sender;
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
@end


/* Small utility used for processing resize events under Cocoa. */
@interface EmacsWindow : NSWindow
{
  NSPoint grabOffset;
}
@end


/* ==========================================================================

   The main menu implementation

   ========================================================================== */

@interface EmacsMenu : NSMenu
{
  struct frame *frame;
  unsigned long keyEquivModMask;
}

- initWithTitle: (NSString *)title frame: (struct frame *)f;
- (void)setFrame: (struct frame *)f;
- (void)menuNeedsUpdate: (NSMenu *)menu; /* (delegate method) */
- (NSString *)parseKeyEquiv: (char *)key;
- (NSMenuItem *)addItemWithWidgetValue: (void *)wvptr;
- (void)fillWithWidgetValue: (void *)wvptr;
- (EmacsMenu *)addSubmenuWithTitle: (char *)title forFrame: (struct frame *)f;
- (void) clear;
- (Lisp_Object)runMenuAt: (NSPoint)p forFrame: (struct frame *)f
                 keymaps: (int)keymaps;
@end


/* ==========================================================================

   Toolbar

   ========================================================================== */

@class EmacsImage;

@interface EmacsToolbar : NSToolbar
   {
     EmacsView *emacsView;
     NSMutableDictionary *identifierToItem;
     NSMutableArray *activeIdentifiers;
     NSArray *prevIdentifiers;
     unsigned long enablement, prevEnablement;
   }
- initForView: (EmacsView *)view withIdentifier: (NSString *)identifier;
- (void) clearActive;
- (BOOL) changed;
- (void) addDisplayItemWithImage: (EmacsImage *)img idx: (int)idx
                        helpText: (char *)help
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
   }
- initFromContents: (Lisp_Object)menu isQuestion: (BOOL)isQ;
- addButton: (char *)str value: (Lisp_Object)val row: (int)row;
- addString: (char *)str row: (int)row;
- addSplit;
- (Lisp_Object)runDialogAt: (NSPoint)p;
@end

@interface EmacsTooltip : NSObject
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
   This and next override methods to work around OS X behavior of
   restarting application loop when user dismisses panel.

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
  id imageListNext;
  int refCount;
  NSBitmapImageRep *bmRep; /* used for accessing pixel data */
  unsigned char *pixmapData[5]; /* shortcut to access pixel data */
  BOOL onTiger;
  NSColor *stippleMask;
}
+ allocInitFromFile: (Lisp_Object)file;
- reference;
- imageListSetNext: (id)arg;
- imageListNext;
- (void)dealloc;
- initFromXBM: (unsigned char *)bits width: (int)w height: (int)h
         flip: (BOOL)flip;
- initFromSkipXBM: (unsigned char *)bits width: (int)w height: (int)h
             flip: (BOOL)flip length: (int)length;
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
   Lisp_Object win;
   struct frame *frame;
   NSResponder *prevResponder;

   /* offset to the bottom of knob of last mouse down */
   float last_mouse_offset;
   float min_portion;
   int pixel_height;
   int last_hit_part;

   BOOL condemned;

   /* optimize against excessive positioning calls generated by emacs */
   int em_position;
   int em_portion;
   int em_whole;
   }

- initFrame: (NSRect )r window: (Lisp_Object)win;
- (void)setFrame: (NSRect)r;
- (void)dealloc;

- setPosition: (int) position portion: (int) portion whole: (int) whole;
- (int) checkSamePosition: (int)position portion: (int)portion
                    whole: (int)whole;
- (void) getMouseMotionPart: (int *)part window: (Lisp_Object *)window
                          x: (Lisp_Object *)x y: ( Lisp_Object *)y;
- (void) sendScrollEventAtLoc: (float)loc fromEvent: (NSEvent *)e;
- repeatScroll: (NSTimer *)sender;
- condemn;
- reprieve;
- judge;
@end


/* ==========================================================================

   Rendering on Panther and above

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


/* ==========================================================================

   Running the preferences window

   ========================================================================== */

@interface EmacsPrefsController : NSObject
{
    struct frame *frame;
    IBOutlet NSWindow *prefsWindow;
    IBOutlet NSPopUpButton *alternateModMenu;
    IBOutlet NSPopUpButton *commandModMenu;
#ifdef NS_IMPL_COCOA
    IBOutlet NSPopUpButton *controlModMenu;
    IBOutlet NSPopUpButton *functionModMenu;
#endif
    IBOutlet NSMatrix *cursorTypeMatrix;
    IBOutlet NSSlider *cursorBlinkSlider;
    IBOutlet NSSlider *expandSpaceSlider;
#ifdef NS_IMPL_COCOA
    IBOutlet NSButton *smoothFontsCheck;
    IBOutlet NSButton *useQuickdrawCheck;
    IBOutlet NSButton *useSysHiliteCheck;
    Lisp_Object prevUseHighlightColor;
#endif
    float prevExpandSpace;
    float prevBlinkRate;
}
- (IBAction)cancel: (id)sender;
- (IBAction)ok: (id)sender;
- (IBAction)resetToDefaults: (id)sender;
- (IBAction)runHelp: (id)sender;
- (IBAction)setColors: (id)sender;
- (IBAction)setDefaultFont: (id)sender;

- (void) showForFrame: (struct frame *)f;
- (void) setPanelFromValues;
- (void) setValuesFromPanel;
@end

extern NSArray *ns_send_types, *ns_return_types;
extern EmacsMenu *mainMenu, *svcsMenu, *dockMenu;

/* Apple removed the declaration, but kept the implementation */
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_4
@interface NSApplication (EmacsApp)
- (void)setAppleMenu: (NSMenu *)menu;
@end
#endif

#ifndef NS_HAVE_INTEGER
typedef long NSInteger;
typedef unsigned long NSUInteger;
#endif /* not NS_HAVE_INTEGER */

#endif  /* __OBJC__ */



/* ==========================================================================

   Non-OO stuff

   ========================================================================== */

enum ns_cursor_types
{
   no_highlight =0,
   filled_box,
   hollow_box,
   underscore,
   bar
};


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
  unsigned int size;
  unsigned int avail;
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

  char *name;  /* postscript name, uniquely identifies on NS systems */
  float width;  /* this and following metrics stored as float rather than int */
  float height;
  float underpos;
  float underwidth;
  float size;
#ifdef __OBJC__
  NSFont *nsfont;
  /* cgfont and synthItal are used only on OS X 10.3+ */
#if defined (NS_IMPL_COCOA) && (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3)
  CGFontRef cgfont;
#else /* GNUstep or OS X < 10.3 */
  void *cgfont;
#endif
#else /* ! OBJC */
  void *nsfont;
  void *cgfont;
#endif
  char bold, ital;  /* convenience flags */
  char synthItal;
  float voffset;  /* mean of ascender/descender offsets */
  XCharStruct max_bounds; /* 23 */
  /* we compute glyph codes and metrics on-demand in blocks of 256 indexed
     by hibyte, lobyte */
  unsigned short **glyphs; /* map unicode index to glyph */
  struct font_metrics **metrics;
};


/* init'd in ns_initialize_display_info () */
struct ns_display_info
{
  /* Chain of all ns_display_info structures.  */
  struct ns_display_info *next;

  /* The generic display parameters corresponding to this NS display. */
  struct terminal *terminal;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).
     The same cons cell also appears in ns_display_name_list.  */
  Lisp_Object name_list_element;

  /* The number of fonts loaded. */
  int n_fonts;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /*/23 */
  struct ns_bitmap_record *bitmaps;
  int bitmaps_size;
  int bitmaps_last;

  /* 23 */
  struct image_cache *image_cache;

  struct ns_color_table *color_table;

  /* 23: Dimensions and DPI resolution of this screen */
  int height, width;
  double resx, resy;

  /* 23: Mask of things that cause the mouse to be grabbed */
  int grabbed;

  /* 23 */
  int n_planes;

  /* 23 */
  int color_p;

  /* 23 */
  Window root_window;

  /* 23: Xism */
  XrmDatabase xrdb;

  /* 23: The cursor to use for vertical scroll bars. */
  Cursor vertical_scroll_bar_cursor;

  /* 23: most mouse face stuff moved in here (and reasonably so) */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_beg_x, mouse_face_beg_y;
  int mouse_face_end_x, mouse_face_end_y;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;
  int mouse_face_deferred_gc;
  Lisp_Object mouse_face_overlay;
  FRAME_PTR mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;
  int mouse_face_defer;
  int mouse_face_hidden;
  int mouse_face_image_state;

  struct frame *x_highlight_frame;
  struct frame *x_focus_frame;
};

/* This is a chain of structures for all the NS displays currently in use.  */
extern struct ns_display_info *x_display_list;

extern Lisp_Object ns_display_name_list;
extern struct ns_display_info *ns_display_info_for_name ();

/* 23: FIXME: these functions (we defined in nsfns) are used in various
       places, but no prototypes are provided */
struct ns_display_info *check_x_display_info (Lisp_Object frame);
FRAME_PTR check_x_frame (Lisp_Object frame);


struct ns_output
{
#ifdef __OBJC__
  EmacsView *view;
  id miniimage;
  NSColor *current_cursor_color;
  NSColor *desired_cursor_color;
  NSColor *foreground_color;
  NSColor *background_color;
  EmacsToolbar *toolbar;
#else
  void *view;
  void *miniimage;
  void *current_cursor_color;
  void *desired_cursor_color;
  void *foreground_color;
  void *background_color;
  void *toolbar;
#endif

  /* 23: NSCursors init'ed in initFrameFromEmacs */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor hand_cursor;
  Cursor hourglass_cursor;
  Cursor horizontal_drag_cursor;

  /* 23: NS-specific */
  Cursor current_pointer;

  /* 23: lord knows why Emacs needs to know about our Window ids.. */
  Window window_desc, parent_desc;
  char explicit_parent;

  struct font *font;
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset; /* only used with font_backend */

  Lisp_Object icon_top;
  Lisp_Object icon_left;
  enum ns_cursor_types current_cursor, desired_cursor;
  unsigned char last_inactive;

  /* The size of the extra width currently allotted for vertical
     scroll bars, in pixels.  */
  int vertical_scroll_bar_extra;

  /* The height of the titlebar decoration (included in NSWindow's frame). */
  int titlebar_height;

  /* The height of the toolbar if displayed, else 0. */
  int toolbar_height;

  /* This is the Emacs structure for the NS display this frame is on.  */
  struct ns_display_info *display_info;
};

/* 23: this dummy decl now needed to support TTYs */
struct x_output
{
  unsigned long background_pixel;
  unsigned long foreground_pixel;
};


/* This gives the ns_display_info structure for the display F is on.  */
#define FRAME_NS_DISPLAY_INFO(f) ((f)->output_data.ns->display_info)
/* the primacy of X must be constantly worked with... */
#define FRAME_X_DISPLAY_INFO(f) ((f)->output_data.ns->display_info)
#define FRAME_X_OUTPUT(f) ((f)->output_data.ns)
#define FRAME_NS_WINDOW(f) ((f)->output_data.ns->window_desc)
#define FRAME_X_WINDOW(f) ((f)->output_data.ns->window_desc)

/* This is the `Display *' which frame F is on.  */
#define FRAME_NS_DISPLAY(f) (0)
#define FRAME_X_DISPLAY(f) (0)

#define FRAME_FOREGROUND_COLOR(f) ((f)->output_data.ns->foreground_color)
#define FRAME_BACKGROUND_COLOR(f) ((f)->output_data.ns->background_color)

#define FRAME_X_IMAGE_CACHE(F) FRAME_NS_DISPLAY_INFO ((F))->image_cache

#define NS_FACE_FOREGROUND(f) ((f)->foreground)
#define NS_FACE_BACKGROUND(f) ((f)->background)
#define FRAME_NS_TITLEBAR_HEIGHT(f) ((f)->output_data.ns->titlebar_height)
#define FRAME_NS_TOOLBAR_HEIGHT(f) ((f)->output_data.ns->toolbar_height)

#define FONT_WIDTH(f)	((f)->max_width)
#define FONT_HEIGHT(f)	((f)->height)
/*#define FONT_BASE(f)    ((f)->ascent) */
#define FONT_BASE(f)    (((struct nsfont_info *)f)->max_bounds.ascent)
/*#define FONT_DESCENT(f) ((f)->descent) */
#define FONT_DESCENT(f) (((struct nsfont_info *)f)->max_bounds.descent)

#define FRAME_DEFAULT_FACE(f) FACE_FROM_ID (f, DEFAULT_FACE_ID)

#define FRAME_NS_VIEW(f) ((f)->output_data.ns->view)
#define FRAME_CURSOR(f)  ((f)->output_data.ns->current_cursor)
#define FRAME_CURSOR_COLOR(f) ((f)->output_data.ns->current_cursor_color)
#define FRAME_NEW_CURSOR_COLOR(f) ((f)->output_data.ns->desired_cursor_color)
#define FRAME_NEW_CURSOR(f)  ((f)->output_data.ns->desired_cursor)
#define FRAME_POINTER_TYPE(f) ((f)->output_data.ns->current_pointer)
#define FRAME_LAST_INACTIVE(f) ((f)->output_data.ns->last_inactive)

#define FRAME_FONT(f) ((f)->output_data.ns->font)

#ifdef __OBJC__
#define XNS_SCROLL_BAR(vec) ((id) XSAVE_VALUE (vec)->pointer)
#else
#define XNS_SCROLL_BAR(vec) XSAVE_VALUE (vec)->pointer
#endif

/* Compute pixel size for vertical scroll bars */
#define NS_SCROLL_BAR_WIDTH(f)                              \
(FRAME_HAS_VERTICAL_SCROLL_BARS (f)                          \
 ? rint (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0               \
        ? FRAME_CONFIG_SCROLL_BAR_WIDTH (f)                 \
        : (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)))   \
 : 0)

/* Difference btwn char-column-calculated and actual SB widths.
   This is only a concern for rendering when SB on left. */
#define NS_SCROLL_BAR_ADJUST(w, f)		\
(WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) ?	\
    (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)	\
        - NS_SCROLL_BAR_WIDTH (f)) : 0)

/* XXX: fix for GNUstep inconsistent accounting for titlebar */
#ifdef NS_IMPL_GNUSTEP
#define NS_TOP_POS(f) ((f)->top_pos + 18)
#else
#define NS_TOP_POS(f) ((f)->top_pos)
#endif

#define FRAME_NS_FONT_TABLE(f) (FRAME_NS_DISPLAY_INFO (f)->font_table)

#define FRAME_FONTSET(f) ((f)->output_data.ns->fontset)

/* 23 */
#define FRAME_SMALLEST_CHAR_WIDTH(f)  \
  (FRAME_NS_DISPLAY_INFO (f)->smallest_char_width)
#define FRAME_SMALLEST_FONT_HEIGHT(f) \
  (FRAME_NS_DISPLAY_INFO (f)->smallest_font_height)
#define FONT_TYPE_FOR_UNIBYTE(font, ch)   0
#define FONT_TYPE_FOR_MULTIBYTE(font, ch) 0
#define FRAME_BASELINE_OFFSET(f) ((f)->output_data.ns->baseline_offset)
#define BLACK_PIX_DEFAULT(f) 0x000000
#define WHITE_PIX_DEFAULT(f) 0xFFFFFF

/* First position where characters can be shown (instead of scrollbar, if
   it is on left. */
#define FIRST_CHAR_POSITION(f) \
  (! (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)) ? 0 \
   : FRAME_SCROLL_BAR_COLS (f))

extern struct ns_display_info *ns_term_init ();
extern void ns_term_shutdown (int sig);

/* constants for text rendering */
#define NS_DUMPGLYPH_NORMAL             0
#define NS_DUMPGLYPH_CURSOR             1
#define NS_DUMPGLYPH_FOREGROUND         2
#define NS_DUMPGLYPH_MOUSEFACE          3


EXFUN (Fx_display_grayscale_p, 1);
EXFUN (Fx_display_planes, 1);

/* In nsfont, called from fontset.c */
extern void nsfont_make_fontset_for_font (Lisp_Object name,
                                         Lisp_Object font_object);

/* In nsfont, for debugging */
struct glyph_string;
void dump_glyphstring (struct glyph_string *s);

/* Implemented in nsterm, published in or needed from nsfns. */
extern Lisp_Object Qfontsize;
extern Lisp_Object ns_list_fonts (FRAME_PTR f, Lisp_Object pattern,
                                  int size, int maxnames);
extern void ns_clear_frame (struct frame *f);

extern const char *ns_xlfd_to_fontname (const char *xlfd);

extern void check_ns (void);
extern Lisp_Object ns_map_event_to_object ();
extern Lisp_Object ns_string_from_pasteboard ();
extern void ns_string_to_pasteboard ();
extern void nxatoms_of_nsselect ();
extern int ns_lisp_to_cursor_type ();
extern Lisp_Object ns_cursor_type_to_lisp (int arg);
extern Lisp_Object Qnone;

/* XColor defined in dispextern.h (we use color_def->pixel = NSColor id), but
   this causes an #include snafu, so we can't declare it.  */
extern int
ns_defined_color (struct frame *f, char *name, XColor *color_def, int alloc,
                  char makeIndex);

#ifdef __OBJC__
extern Lisp_Object ns_color_to_lisp (NSColor *col);
extern int ns_lisp_to_color (Lisp_Object color, NSColor **col);
extern NSColor *ns_lookup_indexed_color (unsigned long idx, struct frame *f);
extern unsigned long ns_index_color (NSColor *color, struct frame *f);
extern void ns_free_indexed_color (unsigned long idx, struct frame *f);
#endif

/* C access to ObjC functionality */
extern void  ns_release_object (void *obj);
extern void  ns_retain_object (void *obj);
extern void *ns_alloc_autorelease_pool ();
extern void ns_release_autorelease_pool ();

/* in nsmenu */
extern void update_frame_tool_bar (FRAME_PTR f);
extern void free_frame_tool_bar (FRAME_PTR f);
extern void find_and_call_menu_selection (FRAME_PTR f,
    int menu_bar_items_used, Lisp_Object vector, void *client_data);
extern Lisp_Object find_and_return_menu_selection (FRAME_PTR f,
                                                   int keymaps,
                                                   void *client_data);
extern Lisp_Object ns_popup_dialog (Lisp_Object position, Lisp_Object contents,
                                    Lisp_Object header);

/* two more prototypes that should be moved to a more general include file */
extern void set_frame_menubar (struct frame *f, int first_time, int deep_p);
extern void x_set_window_size (struct frame *f, int change_grav,
                              int cols, int rows);

/* From nsimage.m, needed in image.c */
struct image;
extern void *ns_image_from_XBM (unsigned char *bits, int width, int height);
extern void *ns_image_for_XPM (int width, int height, int depth);
extern void *ns_image_from_file (Lisp_Object file);
extern int ns_load_image (struct frame *f, struct image *img,
                          Lisp_Object spec_file, Lisp_Object spec_data);
extern int ns_image_width (void *img);
extern int ns_image_height (void *img);
extern unsigned long ns_get_pixel (void *img, int x, int y);
extern void ns_put_pixel (void *img, int x, int y, unsigned long argb);
extern void ns_set_alpha (void *img, int x, int y, unsigned char a);

/* This in nsterm.m */
extern unsigned long ns_get_rgb_color (struct frame *f,
                                       float r, float g, float b, float a);
extern NSPoint last_mouse_motion_position;

#ifdef NS_IMPL_GNUSTEP
extern char gnustep_base_version[];  /* version tracking */
#endif

#define MINWIDTH 10
#define MINHEIGHT 10

/* Screen max coordinate
 Using larger coordinates causes movewindow/placewindow to abort */
#define SCREENMAX 16000

#define NS_SCROLL_BAR_WIDTH_DEFAULT     [EmacsScroller scrollerWidth]
/* This is to match emacs on other platforms, ugly though it is. */
#define NS_SELECTION_COLOR_DEFAULT	@"LightGoldenrod2";
#define RESIZE_HANDLE_SIZE 12

/* Little utility macros */
#define IN_BOUND(min, x, max) (((x) < (min)) \
                                ? (min) : (((x)>(max)) ? (max) : (x)))
#define SCREENMAXBOUND(x) (IN_BOUND (-SCREENMAX, x, SCREENMAX))

/* 23: needed somewhere... */
#define VERTICAL_SCROLL_BAR_WIDTH_TRIM (0)


#endif	/* HAVE_NS */

/* arch-tag: 0a28b142-4ac1-4a81-a243-abcd82d9c4e5
   (do not change this comment) */
