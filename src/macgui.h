/* Definitions and headers for communication on the Mac OS.
   Copyright (C) 2000, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef EMACS_MACGUI_H
#define EMACS_MACGUI_H

typedef struct _XDisplay Display; /* opaque */

typedef Lisp_Object XrmDatabase;

typedef unsigned long Time;

#ifdef HAVE_CARBON
#undef Z
#ifdef MAC_OSX
#if ! HAVE_MKTIME || BROKEN_MKTIME
#undef mktime
#endif
#undef DEBUG
#undef free
#undef malloc
#undef realloc
/* Macros max and min defined in lisp.h conflict with those in
   precompiled header Carbon.h.  */
#undef max
#undef min
#undef init_process
#include <Carbon/Carbon.h>
#if ! HAVE_MKTIME || BROKEN_MKTIME
#undef mktime
#define mktime emacs_mktime
#endif
#undef free
#define free unexec_free
#undef malloc
#define malloc unexec_malloc
#undef realloc
#define realloc unexec_realloc
#undef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#undef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#undef init_process
#define init_process emacs_init_process
#undef INFINITY
#else  /* not MAC_OSX */
#undef SIGHUP
#define OLDP2C 1
#include <Carbon.h>
#endif  /* not MAC_OSX */
#undef Z
#define Z (current_buffer->text->z)
#else /* not HAVE_CARBON */
#include <QuickDraw.h>		/* for WindowRef */
#include <QDOffscreen.h>	/* for GWorldPtr */
#include <Appearance.h>		/* for ThemeCursor */
#include <Windows.h>
#include <Controls.h>
#include <Gestalt.h>
#endif /* not HAVE_CARBON */

/* Whether to use ATSUI (Apple Type Services for Unicode Imaging) for
   text drawing.  */
#if 0 /* Don't enable by default on the emacs-unicode-2 branch.  */
#ifndef USE_ATSUI
#ifdef MAC_OSX
#define USE_ATSUI 1
#endif
#endif
#endif

/* Whether to use low-level Quartz 2D (aka Core Graphics) text drawing
   in preference to ATSUI for ASCII and Latin-1 characters.  */
#ifndef USE_CG_TEXT_DRAWING
#if USE_ATSUI && MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
#define USE_CG_TEXT_DRAWING 1
#endif
#endif

/* Whether to use Quartz 2D routines for drawing operations other than
   texts.  */
#ifndef USE_CG_DRAWING
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
#define USE_CG_DRAWING 1
#endif
#endif

/* Whether to use the standard Font Panel floating dialog.  */
#ifndef USE_MAC_FONT_PANEL
#if USE_ATSUI && MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
#define USE_MAC_FONT_PANEL 1
#endif
#endif

/* Whether to use Text Services Manager.  */
#ifndef USE_MAC_TSM
#if TARGET_API_MAC_CARBON
#define USE_MAC_TSM 1
#endif
#endif

/* Whether to use HIToolbar.  */
#ifndef USE_MAC_TOOLBAR
#if USE_CG_DRAWING && MAC_OS_X_VERSION_MAX_ALLOWED >= 1030 && MAC_OS_X_VERSION_MIN_REQUIRED != 1020
#define USE_MAC_TOOLBAR 1
#endif
#endif

typedef WindowRef Window;
typedef GWorldPtr Pixmap;

#define Cursor ThemeCursor
#define No_Cursor (-1)

#define FACE_DEFAULT (~0)

#if !TARGET_API_MAC_CARBON
#define GetPixDepth(pmh) ((*(pmh))->pixelSize)
#endif


/* Emulate XCharStruct.  */
/* If the sum of ascent and descent is negative, that means some
   special status specified by enum pcm_status.  */
typedef struct _XCharStruct
{
  short	lbearing;		/* origin to left edge of raster */
  short	rbearing;		/* origin to right edge of raster */
  short	width;			/* advance to next char's origin */
  short	ascent;			/* baseline to top edge of raster */
  short	descent;		/* baseline to bottom edge of raster */
#if 0
  unsigned short attributes;	/* per char flags (not predefined) */
#endif
} XCharStruct;

enum pcm_status
  {
    PCM_VALID = 0,		/* pcm data is valid */
    PCM_INVALID = -1,		/* pcm data is invalid */
  };

#define STORE_XCHARSTRUCT(xcs, w, bds)			\
  ((xcs).width = (w),					\
   (xcs).lbearing = (bds).left,				\
   (xcs).rbearing = (bds).right,			\
   (xcs).ascent = -(bds).top,				\
   (xcs).descent = (bds).bottom)

struct MacFontStruct {
  char *full_name;

  short mac_fontnum;  /* font number of font used in this window */
  int mac_fontsize;  /* size of font */
  short mac_fontface;  /* plain, bold, italics, etc. */
#if TARGET_API_MAC_CARBON
  int mac_scriptcode;  /* Mac OS script code for font used */
#else
  short mac_scriptcode;  /* Mac OS script code for font used */
#endif
#if USE_ATSUI
  ATSUStyle mac_style;		/* NULL if QuickDraw Text is used */
#if USE_CG_TEXT_DRAWING
  CGFontRef cg_font;		/* NULL if ATSUI text drawing is used */
  CGGlyph *cg_glyphs;		/* Likewise  */
#endif
#endif

/* from Xlib.h */
#if 0
  XExtData *ext_data;      /* hook for extension to hang data */
  Font fid;                /* Font id for this font */
  unsigned direction;      /* hint about the direction font is painted */
#endif /* 0 */
  unsigned min_char_or_byte2;/* first character */
  unsigned max_char_or_byte2;/* last character */
  unsigned min_byte1;      /* first row that exists */
  unsigned max_byte1;      /* last row that exists */
#if 0
  Bool all_chars_exist;    /* flag if all characters have nonzero size */
  unsigned default_char;   /* char to print for undefined character */
  int n_properties;        /* how many properties there are */
  XFontProp *properties;   /* pointer to array of additional properties */
#endif /* 0 */
  XCharStruct min_bounds;  /* minimum bounds over all existing char */
  XCharStruct max_bounds;  /* maximum bounds over all existing char */
  union {
    XCharStruct *per_char; /* first_char to last_char information */
    XCharStruct **rows;    /* first row to last row information */
  } bounds;
  int ascent;              /* logical extent above baseline for spacing */
  int descent;             /* logical decent below baseline for spacing */
};

typedef struct MacFontStruct MacFontStruct;
typedef struct MacFontStruct XFontStruct;

/* Structure borrowed from Xlib.h to represent two-byte characters.  */

typedef struct {
  unsigned char byte1;
  unsigned char byte2;
} XChar2b;

#define STORE_XCHAR2B(chp, b1, b2) \
  ((chp)->byte1 = (b1), (chp)->byte2 = (b2))

#define XCHAR2B_BYTE1(chp) \
  ((chp)->byte1)

#define XCHAR2B_BYTE2(chp) \
  ((chp)->byte2)


/* Emulate X GC's by keeping color and font info in a structure.  */
typedef struct _XGCValues
{
  unsigned long foreground;
  unsigned long background;
  XFontStruct *font;
} XGCValues;

typedef struct _XGC
{
  /* Original value.  */
  XGCValues xgcv;

  /* Cached data members follow.  */

  /* QuickDraw foreground color.  */
  RGBColor fore_color;

  /* QuickDraw background color.  */
  RGBColor back_color;

#if USE_CG_DRAWING && MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
  /* Quartz 2D foreground color.  */
  CGColorRef cg_fore_color;

  /* Quartz 2D background color.  */
  CGColorRef cg_back_color;
#endif

#define MAX_CLIP_RECTS 2
  /* Number of clipping rectangles.  */
  int n_clip_rects;

  /* QuickDraw clipping region.  Ignored if n_clip_rects == 0.  */
  RgnHandle clip_region;

#if defined (MAC_OSX) && (USE_ATSUI || USE_CG_DRAWING)
  /* Clipping rectangles used in Quartz 2D drawing.  The y-coordinate
     is in QuickDraw's.  */
  CGRect clip_rects[MAX_CLIP_RECTS];
#endif
} *GC;

#define GCForeground            (1L<<2)
#define GCBackground            (1L<<3)
#define GCFont 			(1L<<14)
#define GCGraphicsExposures	0

/* Bit Gravity */

#define ForgetGravity		0
#define NorthWestGravity	1
#define NorthGravity		2
#define NorthEastGravity	3
#define WestGravity		4
#define CenterGravity		5
#define EastGravity		6
#define SouthWestGravity	7
#define SouthGravity		8
#define SouthEastGravity	9
#define StaticGravity		10

#define NoValue		0x0000
#define XValue  	0x0001
#define YValue		0x0002
#define WidthValue  	0x0004
#define HeightValue  	0x0008
#define AllValues 	0x000F
#define XNegative 	0x0010
#define YNegative 	0x0020

typedef struct {
    	long flags;	/* marks which fields in this structure are defined */
#if 0
	int x, y;		/* obsolete for new window mgrs, but clients */
	int width, height;	/* should set so old wm's don't mess up */
#endif
	int min_width, min_height;
#if 0
	int max_width, max_height;
#endif
    	int width_inc, height_inc;
#if 0
	struct {
		int x;	/* numerator */
		int y;	/* denominator */
	} min_aspect, max_aspect;
#endif
	int base_width, base_height;		/* added by ICCCM version 1 */
#if 0
	int win_gravity;			/* added by ICCCM version 1 */
#endif
} XSizeHints;

#define USPosition	(1L << 0) /* user specified x, y */
#define USSize		(1L << 1) /* user specified width, height */

#define PPosition	(1L << 2) /* program specified position */
#define PSize		(1L << 3) /* program specified size */
#define PMinSize	(1L << 4) /* program specified minimum size */
#define PMaxSize	(1L << 5) /* program specified maximum size */
#define PResizeInc	(1L << 6) /* program specified resize increments */
#define PAspect		(1L << 7) /* program specified min and max aspect ratios */
#define PBaseSize	(1L << 8) /* program specified base for incrementing */
#define PWinGravity	(1L << 9) /* program specified window gravity */

typedef struct {
    int x, y;
    unsigned width, height;
} XRectangle;

#define NativeRectangle Rect

#define CONVERT_TO_XRECT(xr,nr)			\
  ((xr).x = (nr).left,				\
   (xr).y = (nr).top,				\
   (xr).width = ((nr).right - (nr).left),	\
   (xr).height = ((nr).bottom - (nr).top))

#define CONVERT_FROM_XRECT(xr,nr)		\
  ((nr).left = (xr).x,				\
   (nr).top = (xr).y,				\
   (nr).right = ((xr).x + (xr).width),		\
   (nr).bottom = ((xr).y + (xr).height))

#define STORE_NATIVE_RECT(nr,x,y,width,height)	\
  ((nr).left = (x),				\
   (nr).top = (y),				\
   (nr).right = ((nr).left + (width)),		\
   (nr).bottom = ((nr).top + (height)))

#endif /* EMACS_MACGUI_H */

/* arch-tag: 5a0da49a-35e2-418b-a58c-8a55778ae849
   (do not change this comment) */
