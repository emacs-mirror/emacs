/* Functions for image support on window system.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
                 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
                 Free Software Foundation, Inc.

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

#include <config.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This makes the fields of a Display accessible, in Xlib header files.  */

#define XLIB_ILLEGAL_ACCESS

#include "lisp.h"
#include "frame.h"
#include "window.h"
#include "dispextern.h"
#include "blockinput.h"
#include "systime.h"
#include <epaths.h>
#include "charset.h"
#include "coding.h"
#include "termhooks.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#include <sys/types.h>
#include <sys/stat.h>

#define COLOR_TABLE_SUPPORT 1

typedef struct x_bitmap_record Bitmap_Record;
#define GET_PIXEL(ximg, x, y) XGetPixel(ximg, x, y)
#define NO_PIXMAP None

#define RGB_PIXEL_COLOR unsigned long

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1
#endif /* HAVE_X_WINDOWS */


#ifdef HAVE_NTGUI
#include "w32term.h"

/* W32_TODO : Color tables on W32.  */
#undef COLOR_TABLE_SUPPORT

typedef struct w32_bitmap_record Bitmap_Record;
#define GET_PIXEL(ximg, x, y) GetPixel(ximg, x, y)
#define NO_PIXMAP 0

#define RGB_PIXEL_COLOR COLORREF

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1

#define FRAME_X_VISUAL(f) FRAME_X_DISPLAY_INFO (f)->visual
#define x_defined_color w32_defined_color
#define DefaultDepthOfScreen(screen) (one_w32_display_info.n_cbits)
#endif /* HAVE_NTGUI */


#ifdef MAC_OS
#include "macterm.h"
#include <sys/stat.h>
#ifndef MAC_OSX
#include <alloca.h>
#include <sys/param.h>
#endif
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
#include <QuickTime/QuickTime.h>
#else  /* not MAC_OSX */
#include <QuickTime.h>
#endif	/* not MAC_OSX */
#else  /* not TARGET_API_MAC_CARBON */
#include <Windows.h>
#include <Gestalt.h>
#include <TextUtils.h>
#include <ImageCompression.h>
#include <QuickTimeComponents.h>
#endif	/* not TARGET_API_MAC_CARBON */

/* MAC_TODO : Color tables on Mac.  */
#undef COLOR_TABLE_SUPPORT

#define ZPixmap 0 		/* arbitrary */
typedef struct mac_bitmap_record Bitmap_Record;

#define GET_PIXEL(ximg, x, y) XGetPixel(ximg, x, y)
#define NO_PIXMAP 0

#define RGB_PIXEL_COLOR unsigned long

/* A black pixel in a mask bitmap/pixmap means ``draw a source
   pixel''.  A white pixel means ``retain the current pixel''. */
#define PIX_MASK_DRAW	RGB_TO_ULONG(0,0,0)
#define PIX_MASK_RETAIN	RGB_TO_ULONG(255,255,255)

#define FRAME_X_VISUAL(f) FRAME_X_DISPLAY_INFO (f)->visual
#define x_defined_color mac_defined_color
#define DefaultDepthOfScreen(screen) (one_mac_display_info.n_planes)

#endif /* MAC_OS */


/* Search path for bitmap files.  */

Lisp_Object Vx_bitmap_file_path;


static void x_disable_image P_ ((struct frame *, struct image *));
static void x_edge_detection P_ ((struct frame *, struct image *, Lisp_Object,
				  Lisp_Object));

static void init_color_table P_ ((void));
static unsigned long lookup_rgb_color P_ ((struct frame *f, int r, int g, int b));
#ifdef COLOR_TABLE_SUPPORT
static void free_color_table P_ ((void));
static unsigned long *colors_in_color_table P_ ((int *n));
static unsigned long lookup_pixel_color P_ ((struct frame *f, unsigned long p));
#endif

/* Code to deal with bitmaps.  Bitmaps are referenced by their bitmap
   id, which is just an int that this section returns.  Bitmaps are
   reference counted so they can be shared among frames.

   Bitmap indices are guaranteed to be > 0, so a negative number can
   be used to indicate no bitmap.

   If you use x_create_bitmap_from_data, then you must keep track of
   the bitmaps yourself.  That is, creating a bitmap from the same
   data more than once will not be caught.  */

#ifdef MAC_OS

static XImagePtr
XGetImage (display, pixmap, x, y, width, height, plane_mask, format)
     Display *display;		/* not used */
     Pixmap pixmap;
     int x, y;			/* not used */
     unsigned int width, height; /* not used */
     unsigned long plane_mask; 	/* not used */
     int format;		/* not used */
{
#if GLYPH_DEBUG
  xassert (x == 0 && y == 0);
  {
    Rect ri, rp;
    SetRect (&ri, 0, 0, width, height);
    xassert (EqualRect (&ri, GetPixBounds (GetGWorldPixMap (pixmap), &rp)));
  }
  xassert (! (pixelsLocked & GetPixelsState (GetGWorldPixMap (pixmap))));
#endif

  LockPixels (GetGWorldPixMap (pixmap));

  return pixmap;
}

static void
XPutPixel (ximage, x, y, pixel)
     XImagePtr ximage;
     int x, y;
     unsigned long pixel;
{
  PixMapHandle pixmap = GetGWorldPixMap (ximage);
  short depth = GetPixDepth (pixmap);

#if defined (WORDS_BIG_ENDIAN) || !USE_CG_DRAWING
  if (depth == 32)
    {
      char *base_addr = GetPixBaseAddr (pixmap);
      short row_bytes = GetPixRowBytes (pixmap);

      ((unsigned long *) (base_addr + y * row_bytes))[x] = 0xff000000 | pixel;
    }
  else
#endif
  if (depth == 1)
    {
      char *base_addr = GetPixBaseAddr (pixmap);
      short row_bytes = GetPixRowBytes (pixmap);

      if (pixel == PIX_MASK_DRAW)
	base_addr[y * row_bytes + x / 8] |= (1 << 7) >> (x & 7);
      else
	base_addr[y * row_bytes + x / 8] &= ~((1 << 7) >> (x & 7));
    }
  else
    {
      CGrafPtr old_port;
      GDHandle old_gdh;
      RGBColor color;

      GetGWorld (&old_port, &old_gdh);
      SetGWorld (ximage, NULL);

      color.red = RED16_FROM_ULONG (pixel);
      color.green = GREEN16_FROM_ULONG (pixel);
      color.blue = BLUE16_FROM_ULONG (pixel);

      SetCPixel (x, y, &color);

      SetGWorld (old_port, old_gdh);
    }
}

static unsigned long
XGetPixel (ximage, x, y)
     XImagePtr ximage;
     int x, y;
{
  PixMapHandle pixmap = GetGWorldPixMap (ximage);
  short depth = GetPixDepth (pixmap);

#if defined (WORDS_BIG_ENDIAN) || !USE_CG_DRAWING
  if (depth == 32)
    {
      char *base_addr = GetPixBaseAddr (pixmap);
      short row_bytes = GetPixRowBytes (pixmap);

      return ((unsigned long *) (base_addr + y * row_bytes))[x] & 0x00ffffff;
    }
  else
#endif
  if (depth == 1)
    {
      char *base_addr = GetPixBaseAddr (pixmap);
      short row_bytes = GetPixRowBytes (pixmap);

      if (base_addr[y * row_bytes + x / 8] & (1 << (~x & 7)))
	return PIX_MASK_DRAW;
      else
	return PIX_MASK_RETAIN;
    }
  else
    {
      CGrafPtr old_port;
      GDHandle old_gdh;
      RGBColor color;

      GetGWorld (&old_port, &old_gdh);
      SetGWorld (ximage, NULL);

      GetCPixel (x, y, &color);

      SetGWorld (old_port, old_gdh);
      return RGB_TO_ULONG (color.red >> 8, color.green >> 8, color.blue >> 8);
    }
}

static void
XDestroyImage (ximg)
     XImagePtr ximg;
{
  UnlockPixels (GetGWorldPixMap (ximg));
}

#if USE_CG_DRAWING
static CGImageRef
mac_create_cg_image_from_image (f, img)
     struct frame *f;
     struct image *img;
{
  Pixmap mask;
  CGImageRef result = NULL;

  BLOCK_INPUT;
  if (img->mask)
    mask = img->mask;
  else
    {
      mask = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    img->width, img->height, 1);
      if (mask)
	{
	  CGrafPtr old_port;
	  GDHandle old_gdh;
	  Rect r;

	  GetGWorld (&old_port, &old_gdh);
	  SetGWorld (mask, NULL);
	  BackColor (blackColor); /* Don't mask.  */
	  SetRect (&r, 0, 0, img->width, img->height);
	  EraseRect (&r);
	  SetGWorld (old_port, old_gdh);
	}
    }
  if (mask)
    {
      CreateCGImageFromPixMaps (GetGWorldPixMap (img->pixmap),
				GetGWorldPixMap (mask), &result);
      if (mask != img->mask)
	XFreePixmap (FRAME_X_DISPLAY (f), mask);
    }
  UNBLOCK_INPUT;

  return result;
}
#endif /* USE_CG_DRAWING */
#endif /* MAC_OS */


/* Functions to access the contents of a bitmap, given an id.  */

int
x_bitmap_height (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

int
x_bitmap_width (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

#if defined (HAVE_X_WINDOWS) || defined (HAVE_NTGUI)
int
x_bitmap_pixmap (f, id)
     FRAME_PTR f;
     int id;
{
  return (int) FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}
#endif

#ifdef HAVE_X_WINDOWS
int
x_bitmap_mask (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].mask;
}
#endif

/* Allocate a new bitmap record.  Returns index of new record.  */

static int
x_allocate_bitmap_record (f)
     FRAME_PTR f;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->bitmaps == NULL)
    {
      dpyinfo->bitmaps_size = 10;
      dpyinfo->bitmaps
	= (Bitmap_Record *) xmalloc (dpyinfo->bitmaps_size * sizeof (Bitmap_Record));
      dpyinfo->bitmaps_last = 1;
      return 1;
    }

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps_size *= 2;
  dpyinfo->bitmaps
    = (Bitmap_Record *) xrealloc (dpyinfo->bitmaps,
				  dpyinfo->bitmaps_size * sizeof (Bitmap_Record));
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
x_reference_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  ++FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */

int
x_create_bitmap_from_data (f, bits, width, height)
     struct frame *f;
     char *bits;
     unsigned int width, height;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int id;

#ifdef HAVE_X_WINDOWS
  Pixmap bitmap;
  bitmap = XCreateBitmapFromData (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				  bits, width, height);
  if (! bitmap)
    return -1;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  Pixmap bitmap;
  bitmap = CreateBitmap (width, height,
			 FRAME_X_DISPLAY_INFO (XFRAME (frame))->n_planes,
			 FRAME_X_DISPLAY_INFO (XFRAME (frame))->n_cbits,
			 bits);
  if (! bitmap)
    return -1;
#endif /* HAVE_NTGUI */

#ifdef MAC_OS
  /* MAC_TODO: for now fail if width is not mod 16 (toolbox requires it) */
  if (width % 16 != 0)
    return -1;
#endif

  id = x_allocate_bitmap_record (f);
#ifdef MAC_OS
  dpyinfo->bitmaps[id - 1].bitmap_data = (char *) xmalloc (height * width);
  bcopy (bits, dpyinfo->bitmaps[id - 1].bitmap_data, height * width);
#endif  /* MAC_OS */

  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  dpyinfo->bitmaps[id - 1].refcount = 1;

#ifdef HAVE_X_WINDOWS
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = 0;
  dpyinfo->bitmaps[id - 1].depth = 1;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].hinst = NULL;
  dpyinfo->bitmaps[id - 1].depth = 1;
#endif /* HAVE_NTGUI */

  return id;
}

/* Create bitmap from file FILE for frame F.  */

int
x_create_bitmap_from_file (f, file)
     struct frame *f;
     Lisp_Object file;
{
#ifdef MAC_OS
  return -1;  /* MAC_TODO : bitmap support */
#endif  /* MAC_OS */

#ifdef HAVE_NTGUI
  return -1;  /* W32_TODO : bitmap support */
#endif /* HAVE_NTGUI */

#ifdef HAVE_X_WINDOWS
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  unsigned int width, height;
  Pixmap bitmap;
  int xhot, yhot, result, id;
  Lisp_Object found;
  int fd;
  char *filename;

  /* Look for an existing bitmap with the same name.  */
  for (id = 0; id < dpyinfo->bitmaps_last; ++id)
    {
      if (dpyinfo->bitmaps[id].refcount
	  && dpyinfo->bitmaps[id].file
	  && !strcmp (dpyinfo->bitmaps[id].file, (char *) SDATA (file)))
	{
	  ++dpyinfo->bitmaps[id].refcount;
	  return id + 1;
	}
    }

  /* Search bitmap-file-path for the file, if appropriate.  */
  fd = openp (Vx_bitmap_file_path, file, Qnil, &found, Qnil);
  if (fd < 0)
    return -1;
  emacs_close (fd);

  filename = (char *) SDATA (found);

  result = XReadBitmapFile (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    filename, &width, &height, &bitmap, &xhot, &yhot);
  if (result != BitmapSuccess)
    return -1;

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = 0;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = (char *) xmalloc (SBYTES (file) + 1);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  strcpy (dpyinfo->bitmaps[id - 1].file, SDATA (file));

  return id;
#endif /* HAVE_X_WINDOWS */
}

/* Free bitmap B.  */

static void
free_bitmap_record (dpyinfo, bm)
     Display_Info *dpyinfo;
     Bitmap_Record *bm;
{
#ifdef HAVE_X_WINDOWS
  XFreePixmap (dpyinfo->display, bm->pixmap);
  if (bm->have_mask)
    XFreePixmap (dpyinfo->display, bm->mask);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  DeleteObject (bm->pixmap);
#endif /* HAVE_NTGUI */

#ifdef MAC_OS
  xfree (bm->bitmap_data);  /* Added ++kfs */
  bm->bitmap_data = NULL;
#endif  /* MAC_OS */

  if (bm->file)
    {
      xfree (bm->file);
      bm->file = NULL;
    }
}

/* Remove reference to bitmap with id number ID.  */

void
x_destroy_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (id > 0)
    {
      Bitmap_Record *bm = &dpyinfo->bitmaps[id - 1];

      if (--bm->refcount == 0)
	{
	  BLOCK_INPUT;
	  free_bitmap_record (dpyinfo, bm);
	  UNBLOCK_INPUT;
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

void
x_destroy_all_bitmaps (dpyinfo)
     Display_Info *dpyinfo;
{
  int i;
  Bitmap_Record *bm = dpyinfo->bitmaps;

  for (i = 0; i < dpyinfo->bitmaps_last; i++, bm++)
    if (bm->refcount > 0)
      free_bitmap_record (dpyinfo, bm);

  dpyinfo->bitmaps_last = 0;
}


#ifdef HAVE_X_WINDOWS

/* Useful functions defined in the section
   `Image type independent image structures' below. */

static unsigned long four_corners_best P_ ((XImagePtr ximg,
					    int *corners,
					    unsigned long width,
					    unsigned long height));

static int x_create_x_image_and_pixmap P_ ((struct frame *f, int width, int height,
					    int depth, XImagePtr *ximg,
					    Pixmap *pixmap));

static void x_destroy_x_image P_ ((XImagePtr ximg));


/* Create a mask of a bitmap. Note is this not a perfect mask.
   It's nicer with some borders in this context */

int
x_create_bitmap_mask (f, id)
     struct frame *f;
     int id;
{
  Pixmap pixmap, mask;
  XImagePtr ximg, mask_img;
  unsigned long width, height;
  int result;
  unsigned long bg;
  unsigned long x, y, xp, xm, yp, ym;
  GC gc;

  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (!(id > 0))
    return -1;

  pixmap = x_bitmap_pixmap (f, id);
  width = x_bitmap_width (f, id);
  height = x_bitmap_height (f, id);

  BLOCK_INPUT;
  ximg = XGetImage (FRAME_X_DISPLAY (f), pixmap, 0, 0, width, height,
		    ~0, ZPixmap);

  if (!ximg)
    {
      UNBLOCK_INPUT;
      return -1;
    }

  result = x_create_x_image_and_pixmap (f, width, height, 1, &mask_img, &mask);

  UNBLOCK_INPUT;
  if (!result)
    {
      XDestroyImage (ximg);
      return -1;
    }

  bg = four_corners_best (ximg, NULL, width, height);

  for (y = 0; y < ximg->height; ++y)
    {
      for (x = 0; x < ximg->width; ++x)
	{
	  xp = x != ximg->width - 1 ? x + 1 : 0;
	  xm = x != 0 ? x - 1 : ximg->width - 1;
	  yp = y != ximg->height - 1 ? y + 1 : 0;
	  ym = y != 0 ? y - 1 : ximg->height - 1;
	  if (XGetPixel (ximg, x, y) == bg
	      && XGetPixel (ximg, x, yp) == bg
	      && XGetPixel (ximg, x, ym) == bg
	      && XGetPixel (ximg, xp, y) == bg
	      && XGetPixel (ximg, xp, yp) == bg
	      && XGetPixel (ximg, xp, ym) == bg
	      && XGetPixel (ximg, xm, y) == bg
	      && XGetPixel (ximg, xm, yp) == bg
	      && XGetPixel (ximg, xm, ym) == bg)
	    XPutPixel (mask_img, x, y, 0);
	  else
	    XPutPixel (mask_img, x, y, 1);
	}
    }

  xassert (interrupt_input_blocked);
  gc = XCreateGC (FRAME_X_DISPLAY (f), mask, 0, NULL);
  XPutImage (FRAME_X_DISPLAY (f), mask, gc, mask_img, 0, 0, 0, 0,
	     width, height);
  XFreeGC (FRAME_X_DISPLAY (f), gc);

  dpyinfo->bitmaps[id - 1].have_mask = 1;
  dpyinfo->bitmaps[id - 1].mask = mask;

  XDestroyImage (ximg);
  x_destroy_x_image (mask_img);

  return 0;
}

#endif /* HAVE_X_WINDOWS */


/***********************************************************************
			    Image types
 ***********************************************************************/

/* Value is the number of elements of vector VECTOR.  */

#define DIM(VECTOR)	(sizeof (VECTOR) / sizeof *(VECTOR))

/* List of supported image types.  Use define_image_type to add new
   types.  Use lookup_image_type to find a type for a given symbol.  */

static struct image_type *image_types;

/* A list of symbols, one for each supported image type.  */

Lisp_Object Vimage_types;

/* An alist of image types and libraries that implement the type.  */

Lisp_Object Vimage_library_alist;

/* Cache for delayed-loading image types.  */

static Lisp_Object Vimage_type_cache;

/* The symbol `xbm' which is used as the type symbol for XBM images.  */

Lisp_Object Qxbm;

/* Keywords.  */

extern Lisp_Object QCwidth, QCheight, QCforeground, QCbackground, QCfile;
extern Lisp_Object QCdata, QCtype;
extern Lisp_Object Qcenter;
Lisp_Object QCascent, QCmargin, QCrelief, Qcount;
Lisp_Object QCconversion, QCcolor_symbols, QCheuristic_mask;
Lisp_Object QCindex, QCmatrix, QCcolor_adjustment, QCmask;

/* Other symbols.  */

Lisp_Object Qlaplace, Qemboss, Qedge_detection, Qheuristic;

/* Time in seconds after which images should be removed from the cache
   if not displayed.  */

Lisp_Object Vimage_cache_eviction_delay;

/* Function prototypes.  */

static Lisp_Object define_image_type P_ ((struct image_type *type, int loaded));
static struct image_type *lookup_image_type P_ ((Lisp_Object symbol));
static void image_error P_ ((char *format, Lisp_Object, Lisp_Object));
static void x_laplace P_ ((struct frame *, struct image *));
static void x_emboss P_ ((struct frame *, struct image *));
static int x_build_heuristic_mask P_ ((struct frame *, struct image *,
				       Lisp_Object));

#define CACHE_IMAGE_TYPE(type, status) \
  do { Vimage_type_cache = Fcons (Fcons (type, status), Vimage_type_cache); } while (0)

#define ADD_IMAGE_TYPE(type) \
  do { Vimage_types = Fcons (type, Vimage_types); } while (0)

/* Define a new image type from TYPE.  This adds a copy of TYPE to
   image_types and caches the loading status of TYPE.  */

static Lisp_Object
define_image_type (type, loaded)
     struct image_type *type;
     int loaded;
{
  Lisp_Object success;

  if (!loaded)
    success = Qnil;
  else
    {
      /* Make a copy of TYPE to avoid a bus error in a dumped Emacs.
         The initialized data segment is read-only.  */
      struct image_type *p = (struct image_type *) xmalloc (sizeof *p);
      bcopy (type, p, sizeof *p);
      p->next = image_types;
      image_types = p;
      success = Qt;
    }

  CACHE_IMAGE_TYPE (*type->type, success);
  return success;
}


/* Look up image type SYMBOL, and return a pointer to its image_type
   structure.  Value is null if SYMBOL is not a known image type.  */

static INLINE struct image_type *
lookup_image_type (symbol)
     Lisp_Object symbol;
{
  struct image_type *type;

  /* We must initialize the image-type if it hasn't been already.  */
  if (NILP (Finit_image_library (symbol, Vimage_library_alist)))
    return 0;			/* unimplemented */

  for (type = image_types; type; type = type->next)
    if (EQ (symbol, *type->type))
      break;

  return type;
}


/* Value is non-zero if OBJECT is a valid Lisp image specification.  A
   valid image specification is a list whose car is the symbol
   `image', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported image type.  The rest of the property list depends on the
   image type.  */

int
valid_image_p (object)
     Lisp_Object object;
{
  int valid_p = 0;

  if (IMAGEP (object))
    {
      Lisp_Object tem;

      for (tem = XCDR (object); CONSP (tem); tem = XCDR (tem))
	if (EQ (XCAR (tem), QCtype))
	  {
	    tem = XCDR (tem);
	    if (CONSP (tem) && SYMBOLP (XCAR (tem)))
	      {
		struct image_type *type;
		type = lookup_image_type (XCAR (tem));
		if (type)
		  valid_p = type->valid_p (object);
	      }

	    break;
	  }
    }

  return valid_p;
}


/* Log error message with format string FORMAT and argument ARG.
   Signaling an error, e.g. when an image cannot be loaded, is not a
   good idea because this would interrupt redisplay, and the error
   message display would lead to another redisplay.  This function
   therefore simply displays a message.  */

static void
image_error (format, arg1, arg2)
     char *format;
     Lisp_Object arg1, arg2;
{
  add_to_log (format, arg1, arg2);
}



/***********************************************************************
			 Image specifications
 ***********************************************************************/

enum image_value_type
{
  IMAGE_DONT_CHECK_VALUE_TYPE,
  IMAGE_STRING_VALUE,
  IMAGE_STRING_OR_NIL_VALUE,
  IMAGE_SYMBOL_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE,
  IMAGE_ASCENT_VALUE,
  IMAGE_INTEGER_VALUE,
  IMAGE_FUNCTION_VALUE,
  IMAGE_NUMBER_VALUE,
  IMAGE_BOOL_VALUE
};

/* Structure used when parsing image specifications.  */

struct image_keyword
{
  /* Name of keyword.  */
  char *name;

  /* The type of value allowed.  */
  enum image_value_type type;

  /* Non-zero means key must be present.  */
  int mandatory_p;

  /* Used to recognize duplicate keywords in a property list.  */
  int count;

  /* The value that was found.  */
  Lisp_Object value;
};


static int parse_image_spec P_ ((Lisp_Object, struct image_keyword *,
				 int, Lisp_Object));
static Lisp_Object image_spec_value P_ ((Lisp_Object, Lisp_Object, int *));


/* Parse image spec SPEC according to KEYWORDS.  A valid image spec
   has the format (image KEYWORD VALUE ...).  One of the keyword/
   value pairs must be `:type TYPE'.  KEYWORDS is a vector of
   image_keywords structures of size NKEYWORDS describing other
   allowed keyword/value pairs.  Value is non-zero if SPEC is valid.  */

static int
parse_image_spec (spec, keywords, nkeywords, type)
     Lisp_Object spec;
     struct image_keyword *keywords;
     int nkeywords;
     Lisp_Object type;
{
  int i;
  Lisp_Object plist;

  if (!IMAGEP (spec))
    return 0;

  plist = XCDR (spec);
  while (CONSP (plist))
    {
      Lisp_Object key, value;

      /* First element of a pair must be a symbol.  */
      key = XCAR (plist);
      plist = XCDR (plist);
      if (!SYMBOLP (key))
	return 0;

      /* There must follow a value.  */
      if (!CONSP (plist))
	return 0;
      value = XCAR (plist);
      plist = XCDR (plist);

      /* Find key in KEYWORDS.  Error if not found.  */
      for (i = 0; i < nkeywords; ++i)
	if (strcmp (keywords[i].name, SDATA (SYMBOL_NAME (key))) == 0)
	  break;

      if (i == nkeywords)
	continue;

      /* Record that we recognized the keyword.  If a keywords
	 was found more than once, it's an error.  */
      keywords[i].value = value;
      ++keywords[i].count;

      if (keywords[i].count > 1)
	return 0;

      /* Check type of value against allowed type.  */
      switch (keywords[i].type)
	{
	case IMAGE_STRING_VALUE:
	  if (!STRINGP (value))
	    return 0;
	  break;

	case IMAGE_STRING_OR_NIL_VALUE:
	  if (!STRINGP (value) && !NILP (value))
	    return 0;
	  break;

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) <= 0)
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR:
	  if (INTEGERP (value) && XINT (value) >= 0)
	    break;
	  if (CONSP (value)
	      && INTEGERP (XCAR (value)) && INTEGERP (XCDR (value))
	      && XINT (XCAR (value)) >= 0 && XINT (XCDR (value)) >= 0)
	    break;
	  return 0;

	case IMAGE_ASCENT_VALUE:
	  if (SYMBOLP (value) && EQ (value, Qcenter))
	    break;
	  else if (INTEGERP (value)
		   && XINT (value) >= 0
		   && XINT (value) <= 100)
	    break;
	  return 0;

	case IMAGE_NON_NEGATIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) < 0)
	    return 0;
	  break;

	case IMAGE_DONT_CHECK_VALUE_TYPE:
	  break;

	case IMAGE_FUNCTION_VALUE:
	  value = indirect_function (value);
	  if (SUBRP (value)
	      || COMPILEDP (value)
	      || (CONSP (value) && EQ (XCAR (value), Qlambda)))
	    break;
	  return 0;

	case IMAGE_NUMBER_VALUE:
	  if (!INTEGERP (value) && !FLOATP (value))
	    return 0;
	  break;

	case IMAGE_INTEGER_VALUE:
	  if (!INTEGERP (value))
	    return 0;
	  break;

	case IMAGE_BOOL_VALUE:
	  if (!NILP (value) && !EQ (value, Qt))
	    return 0;
	  break;

	default:
	  abort ();
	  break;
	}

      if (EQ (key, QCtype) && !EQ (type, value))
	return 0;
    }

  /* Check that all mandatory fields are present.  */
  for (i = 0; i < nkeywords; ++i)
    if (keywords[i].mandatory_p && keywords[i].count == 0)
      return 0;

  return NILP (plist);
}


/* Return the value of KEY in image specification SPEC.  Value is nil
   if KEY is not present in SPEC.  if FOUND is not null, set *FOUND
   to 1 if KEY was found in SPEC, set it to 0 otherwise.  */

static Lisp_Object
image_spec_value (spec, key, found)
     Lisp_Object spec, key;
     int *found;
{
  Lisp_Object tail;

  xassert (valid_image_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
	{
	  if (found)
	    *found = 1;
	  return XCAR (XCDR (tail));
	}
    }

  if (found)
    *found = 0;
  return Qnil;
}


DEFUN ("image-size", Fimage_size, Simage_size, 1, 3, 0,
       doc: /* Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
     (spec, pixels, frame)
     Lisp_Object spec, pixels, frame;
{
  Lisp_Object size;

  size = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_x_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      int width = img->width + 2 * img->hmargin;
      int height = img->height + 2 * img->vmargin;

      if (NILP (pixels))
	size = Fcons (make_float ((double) width / FRAME_COLUMN_WIDTH (f)),
		      make_float ((double) height / FRAME_LINE_HEIGHT (f)));
      else
	size = Fcons (make_number (width), make_number (height));
    }
  else
    error ("Invalid image specification");

  return size;
}


DEFUN ("image-mask-p", Fimage_mask_p, Simage_mask_p, 1, 2, 0,
       doc: /* Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
     (spec, frame)
     Lisp_Object spec, frame;
{
  Lisp_Object mask;

  mask = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_x_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      if (img->mask)
	mask = Qt;
    }
  else
    error ("Invalid image specification");

  return mask;
}

DEFUN ("image-extension-data", Fimage_extension_data, Simage_extension_data, 1, 2, 0,
       doc: /* Return extension data for image SPEC.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
     (spec, frame)
     Lisp_Object spec, frame;
{
  Lisp_Object ext;

  ext = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_x_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      ext = img->data.lisp_val;
    }

  return ext;
}


/***********************************************************************
		 Image type independent image structures
 ***********************************************************************/

static struct image *make_image P_ ((Lisp_Object spec, unsigned hash));
static void free_image P_ ((struct frame *f, struct image *img));
static int check_image_size P_ ((struct frame *f, int width, int height));

#define MAX_IMAGE_SIZE 6.0
Lisp_Object Vmax_image_size;

/* Allocate and return a new image structure for image specification
   SPEC.  SPEC has a hash value of HASH.  */

static struct image *
make_image (spec, hash)
     Lisp_Object spec;
     unsigned hash;
{
  struct image *img = (struct image *) xmalloc (sizeof *img);
  Lisp_Object file = image_spec_value (spec, QCfile, NULL);

  xassert (valid_image_p (spec));
  bzero (img, sizeof *img);
  img->dependencies = NILP (file) ? Qnil : list1 (file);
  img->type = lookup_image_type (image_spec_value (spec, QCtype, NULL));
  xassert (img->type != NULL);
  img->spec = spec;
  img->data.lisp_val = Qnil;
  img->ascent = DEFAULT_IMAGE_ASCENT;
  img->hash = hash;
  img->corners[BOT_CORNER] = -1;  /* Full image */
  return img;
}


/* Free image IMG which was used on frame F, including its resources.  */

static void
free_image (f, img)
     struct frame *f;
     struct image *img;
{
  if (img)
    {
      struct image_cache *c = FRAME_IMAGE_CACHE (f);

      /* Remove IMG from the hash table of its cache.  */
      if (img->prev)
	img->prev->next = img->next;
      else
	c->buckets[img->hash % IMAGE_CACHE_BUCKETS_SIZE] = img->next;

      if (img->next)
	img->next->prev = img->prev;

      c->images[img->id] = NULL;

      /* Free resources, then free IMG.  */
      img->type->free (f, img);
      xfree (img);
    }
}

/* Return 1 if the given widths and heights are valid for display;
   otherwise, return 0. */

int
check_image_size (f, width, height)
     struct frame *f;
     int width;
     int height;
{
  int w, h;

  if (width <= 0 || height <= 0)
    return 0;

  if (INTEGERP (Vmax_image_size))
    w = h = XINT (Vmax_image_size);
  else if (FLOATP (Vmax_image_size))
    {
      if (f != NULL)
	{
	  w = FRAME_PIXEL_WIDTH (f);
	  h = FRAME_PIXEL_HEIGHT (f);
	}
      else
	w = h = 1024;  /* Arbitrary size for unknown frame. */
      w = (int) (XFLOAT_DATA (Vmax_image_size) * w);
      h = (int) (XFLOAT_DATA (Vmax_image_size) * h);
    }
  else
    return 1;

  return (width <= w && height <= h);
}

/* Prepare image IMG for display on frame F.  Must be called before
   drawing an image.  */

void
prepare_image_for_display (f, img)
     struct frame *f;
     struct image *img;
{
  EMACS_TIME t;

  /* We're about to display IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (t);
  img->timestamp = EMACS_SECS (t);

  /* If IMG doesn't have a pixmap yet, load it now, using the image
     type dependent loader function.  */
  if (img->pixmap == NO_PIXMAP && !img->load_failed_p)
    img->load_failed_p = img->type->load (f, img) == 0;

#if defined (MAC_OS) && USE_CG_DRAWING
  if (!img->load_failed_p && img->data.ptr_val == NULL)
    {
      img->data.ptr_val = mac_create_cg_image_from_image (f, img);
      if (img->data.ptr_val == NULL)
	{
	  img->load_failed_p = 1;
	  img->type->free (f, img);
	}
    }
#endif
}


/* Value is the number of pixels for the ascent of image IMG when
   drawn in face FACE.  */

int
image_ascent (img, face, slice)
     struct image *img;
     struct face *face;
     struct glyph_slice *slice;
{
  int height;
  int ascent;

  if (slice->height == img->height)
    height = img->height + img->vmargin;
  else if (slice->y == 0)
    height = slice->height + img->vmargin;
  else
    height = slice->height;

  if (img->ascent == CENTERED_IMAGE_ASCENT)
    {
      if (face->font)
	{
#ifdef HAVE_NTGUI
	  /* W32 specific version.  Why?. ++kfs  */
	  ascent = height / 2 - (FONT_DESCENT (face->font)
				 - FONT_BASE (face->font)) / 2;
#else
	  /* This expression is arranged so that if the image can't be
	     exactly centered, it will be moved slightly up.  This is
	     because a typical font is `top-heavy' (due to the presence
	     uppercase letters), so the image placement should err towards
	     being top-heavy too.  It also just generally looks better.  */
	  ascent = (height + face->font->ascent - face->font->descent + 1) / 2;
#endif /* HAVE_NTGUI */
	}
      else
	ascent = height / 2;
    }
  else
    ascent = (int) (height * img->ascent / 100.0);

  return ascent;
}


/* Image background colors.  */

/* Find the "best" corner color of a bitmap.
   On W32, XIMG is assumed to a device context with the bitmap selected.  */

static RGB_PIXEL_COLOR
four_corners_best (ximg, corners, width, height)
     XImagePtr_or_DC ximg;
     int *corners;
     unsigned long width, height;
{
  RGB_PIXEL_COLOR corner_pixels[4], best;
  int i, best_count;

  if (corners && corners[BOT_CORNER] >= 0)
    {
      /* Get the colors at the corner_pixels of ximg.  */
      corner_pixels[0] = GET_PIXEL (ximg, corners[LEFT_CORNER], corners[TOP_CORNER]);
      corner_pixels[1] = GET_PIXEL (ximg, corners[RIGHT_CORNER] - 1, corners[TOP_CORNER]);
      corner_pixels[2] = GET_PIXEL (ximg, corners[RIGHT_CORNER] - 1, corners[BOT_CORNER] - 1);
      corner_pixels[3] = GET_PIXEL (ximg, corners[LEFT_CORNER], corners[BOT_CORNER] - 1);
    }
  else
    {
      /* Get the colors at the corner_pixels of ximg.  */
      corner_pixels[0] = GET_PIXEL (ximg, 0, 0);
      corner_pixels[1] = GET_PIXEL (ximg, width - 1, 0);
      corner_pixels[2] = GET_PIXEL (ximg, width - 1, height - 1);
      corner_pixels[3] = GET_PIXEL (ximg, 0, height - 1);
    }
  /* Choose the most frequently found color as background.  */
  for (i = best_count = 0; i < 4; ++i)
    {
      int j, n;

      for (j = n = 0; j < 4; ++j)
	if (corner_pixels[i] == corner_pixels[j])
	  ++n;

      if (n > best_count)
	best = corner_pixels[i], best_count = n;
    }

  return best;
}

/* Portability macros */

#ifdef HAVE_NTGUI

#define Destroy_Image(img_dc, prev) \
  do { SelectObject (img_dc, prev); DeleteDC (img_dc); } while (0)

#define Free_Pixmap(display, pixmap) \
  DeleteObject (pixmap)

#else

#define Destroy_Image(ximg, dummy) \
  XDestroyImage (ximg)

#define Free_Pixmap(display, pixmap) \
  XFreePixmap (display, pixmap)

#endif /* HAVE_NTGUI */


/* Return the `background' field of IMG.  If IMG doesn't have one yet,
   it is guessed heuristically.  If non-zero, XIMG is an existing
   XImage object (or device context with the image selected on W32) to
   use for the heuristic.  */

RGB_PIXEL_COLOR
image_background (img, f, ximg)
     struct image *img;
     struct frame *f;
     XImagePtr_or_DC ximg;
{
  if (! img->background_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      int free_ximg = !ximg;
#ifdef HAVE_NTGUI
      HGDIOBJ prev;
#endif /* HAVE_NTGUI */

      if (free_ximg)
	{
#ifndef HAVE_NTGUI
	  ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			    0, 0, img->width, img->height, ~0, ZPixmap);
#else
	  HDC frame_dc = get_frame_dc (f);
	  ximg = CreateCompatibleDC (frame_dc);
	  release_frame_dc (f, frame_dc);
	  prev = SelectObject (ximg, img->pixmap);
#endif /* !HAVE_NTGUI */
	}

      img->background = four_corners_best (ximg, img->corners, img->width, img->height);

      if (free_ximg)
	Destroy_Image (ximg, prev);

      img->background_valid = 1;
    }

  return img->background;
}

/* Return the `background_transparent' field of IMG.  If IMG doesn't
   have one yet, it is guessed heuristically.  If non-zero, MASK is an
   existing XImage object to use for the heuristic.  */

int
image_background_transparent (img, f, mask)
     struct image *img;
     struct frame *f;
     XImagePtr_or_DC mask;
{
  if (! img->background_transparent_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      if (img->mask)
	{
	  int free_mask = !mask;
#ifdef HAVE_NTGUI
	  HGDIOBJ prev;
#endif /* HAVE_NTGUI */

	  if (free_mask)
	    {
#ifndef HAVE_NTGUI
	      mask = XGetImage (FRAME_X_DISPLAY (f), img->mask,
				0, 0, img->width, img->height, ~0, ZPixmap);
#else
	      HDC frame_dc = get_frame_dc (f);
	      mask = CreateCompatibleDC (frame_dc);
	      release_frame_dc (f, frame_dc);
	      prev = SelectObject (mask, img->mask);
#endif /* HAVE_NTGUI */
	    }

	  img->background_transparent
	    = (four_corners_best (mask, img->corners, img->width, img->height) == PIX_MASK_RETAIN);

	  if (free_mask)
	    Destroy_Image (mask, prev);
	}
      else
	img->background_transparent = 0;

      img->background_transparent_valid = 1;
    }

  return img->background_transparent;
}


/***********************************************************************
		  Helper functions for X image types
 ***********************************************************************/

static void x_clear_image_1 P_ ((struct frame *, struct image *, int,
				 int, int));
static void x_clear_image P_ ((struct frame *f, struct image *img));
static unsigned long x_alloc_image_color P_ ((struct frame *f,
					      struct image *img,
					      Lisp_Object color_name,
					      unsigned long dflt));


/* Clear X resources of image IMG on frame F.  PIXMAP_P non-zero means
   free the pixmap if any.  MASK_P non-zero means clear the mask
   pixmap if any.  COLORS_P non-zero means free colors allocated for
   the image, if any.  */

static void
x_clear_image_1 (f, img, pixmap_p, mask_p, colors_p)
     struct frame *f;
     struct image *img;
     int pixmap_p, mask_p, colors_p;
{
  if (pixmap_p && img->pixmap)
    {
      Free_Pixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = NO_PIXMAP;
      img->background_valid = 0;
    }

  if (mask_p && img->mask)
    {
      Free_Pixmap (FRAME_X_DISPLAY (f), img->mask);
      img->mask = NO_PIXMAP;
      img->background_transparent_valid = 0;
    }

  if (colors_p && img->ncolors)
    {
      /* MAC_TODO: color table support.  */
      /* W32_TODO: color table support.  */
#ifdef HAVE_X_WINDOWS
      x_free_colors (f, img->colors, img->ncolors);
#endif /* HAVE_X_WINDOWS */
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }

#if defined (MAC_OS) && USE_CG_DRAWING
  if (img->data.ptr_val)
    {
      CGImageRelease (img->data.ptr_val);
      img->data.ptr_val = NULL;
    }
#endif
}

/* Free X resources of image IMG which is used on frame F.  */

static void
x_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  BLOCK_INPUT;
  x_clear_image_1 (f, img, 1, 1, 1);
  UNBLOCK_INPUT;
}


/* Allocate color COLOR_NAME for image IMG on frame F.  If color
   cannot be allocated, use DFLT.  Add a newly allocated color to
   IMG->colors, so that it can be freed again.  Value is the pixel
   color.  */

static unsigned long
x_alloc_image_color (f, img, color_name, dflt)
     struct frame *f;
     struct image *img;
     Lisp_Object color_name;
     unsigned long dflt;
{
  XColor color;
  unsigned long result;

  xassert (STRINGP (color_name));

  if (x_defined_color (f, SDATA (color_name), &color, 1))
    {
      /* This isn't called frequently so we get away with simply
	 reallocating the color vector to the needed size, here.  */
      ++img->ncolors;
      img->colors =
	(unsigned long *) xrealloc (img->colors,
				    img->ncolors * sizeof *img->colors);
      img->colors[img->ncolors - 1] = color.pixel;
      result = color.pixel;
    }
  else
    result = dflt;

  return result;
}



/***********************************************************************
			     Image Cache
 ***********************************************************************/

static struct image *search_image_cache P_ ((struct frame *, Lisp_Object, unsigned));
static void cache_image P_ ((struct frame *f, struct image *img));
static void postprocess_image P_ ((struct frame *, struct image *));

/* Return a new, initialized image cache that is allocated from the
   heap.  Call free_image_cache to free an image cache.  */

struct image_cache *
make_image_cache ()
{
  struct image_cache *c = (struct image_cache *) xmalloc (sizeof *c);
  int size;

  bzero (c, sizeof *c);
  c->size = 50;
  c->images = (struct image **) xmalloc (c->size * sizeof *c->images);
  size = IMAGE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
  c->buckets = (struct image **) xmalloc (size);
  bzero (c->buckets, size);
  return c;
}


/* Find an image matching SPEC in the cache, and return it.  If no
   image is found, return NULL.  */
static struct image *
search_image_cache (f, spec, hash)
     struct frame *f;
     Lisp_Object spec;
     unsigned hash;
{
  struct image *img;
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  int i = hash % IMAGE_CACHE_BUCKETS_SIZE;

  if (!c) return NULL;

  /* If the image spec does not specify a background color, the cached
     image must have the same background color as the current frame.
     The foreground color must also match, for the sake of monochrome
     images.

     In fact, we could ignore the foreground color matching condition
     for color images, or if the image spec specifies :foreground;
     similarly we could ignore the background color matching condition
     for formats that don't use transparency (such as jpeg), or if the
     image spec specifies :background.  However, the extra memory
     usage is probably negligible in practice, so we don't bother.  */

  for (img = c->buckets[i]; img; img = img->next)
    if (img->hash == hash
	&& !NILP (Fequal (img->spec, spec))
	&& img->frame_foreground == FRAME_FOREGROUND_PIXEL (f)
	&& img->frame_background == FRAME_BACKGROUND_PIXEL (f))
      break;
  return img;
}


/* Search frame F for an image with spec SPEC, and free it.  */

static void
uncache_image (f, spec)
     struct frame *f;
     Lisp_Object spec;
{
  struct image *img = search_image_cache (f, spec, sxhash (spec, 0));
  if (img)
    free_image (f, img);
}


/* Free image cache of frame F.  Be aware that X frames share images
   caches.  */

void
free_image_cache (f)
     struct frame *f;
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  if (c)
    {
      int i;

      /* Cache should not be referenced by any frame when freed.  */
      xassert (c->refcount == 0);

      for (i = 0; i < c->used; ++i)
	free_image (f, c->images[i]);
      xfree (c->images);
      xfree (c->buckets);
      xfree (c);
      FRAME_IMAGE_CACHE (f) = NULL;
    }
}


/* Clear image cache of frame F.  FILTER=t means free all images.
   FILTER=nil means clear only images that haven't been
   displayed for some time.
   Else, only free the images which have FILTER in their `dependencies'.
   Should be called from time to time to reduce the number of loaded images.
   If image-cache-eviction-delay is non-nil, this frees images in the cache
   which weren't displayed for at least that many seconds.  */

void
clear_image_cache (struct frame *f, Lisp_Object filter)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);

  if (c && (!NILP (filter) || INTEGERP (Vimage_cache_eviction_delay)))
    {
      EMACS_TIME t;
      unsigned long old;
      int i, nfreed;

      EMACS_GET_TIME (t);
      old = EMACS_SECS (t) - XFASTINT (Vimage_cache_eviction_delay);

      /* Block input so that we won't be interrupted by a SIGIO
	 while being in an inconsistent state.  */
      BLOCK_INPUT;

      for (i = nfreed = 0; i < c->used; ++i)
	{
	  struct image *img = c->images[i];
	  if (img != NULL
	      && (NILP (filter) ? img->timestamp < old
		  : (EQ (Qt, filter)
		     || !NILP (Fmember (filter, img->dependencies)))))
	    {
	      free_image (f, img);
	      ++nfreed;
	    }
	}

      /* We may be clearing the image cache because, for example,
	 Emacs was iconified for a longer period of time.  In that
	 case, current matrices may still contain references to
	 images freed above.  So, clear these matrices.  */
      if (nfreed)
	{
	  Lisp_Object tail, frame;

	  FOR_EACH_FRAME (tail, frame)
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_IMAGE_CACHE (f) == c)
		clear_current_matrices (f);
	    }

	  ++windows_or_buffers_changed;
	}

      UNBLOCK_INPUT;
    }
}

void
clear_image_caches (Lisp_Object filter)
{
  /* FIXME: We want to do
   * struct terminal *t;
   * for (t = terminal_list; t; t = t->next_terminal)
   *   clear_image_cache (t, filter); */
  Lisp_Object tail, frame;
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_WINDOW_P (XFRAME (frame)))
      clear_image_cache (XFRAME (frame), filter);
}

DEFUN ("clear-image-cache", Fclear_image_cache, Sclear_image_cache,
       0, 1, 0,
       doc: /* Clear the image cache.
FILTER nil or a frame means clear all images in the selected frame.
FILTER t means clear the image caches of all frames.
Anything else, means only clear those images which refer to FILTER,
which is then usually a filename.  */)
     (filter)
     Lisp_Object filter;
{
  if (!(EQ (filter, Qnil) || FRAMEP (filter)))
    clear_image_caches (filter);
  else
    clear_image_cache (check_x_frame (filter), Qt);

  return Qnil;
}


DEFUN ("image-refresh", Fimage_refresh, Simage_refresh,
       1, 2, 0,
       doc: /* Refresh the image with specification SPEC on frame FRAME.
If SPEC specifies an image file, the displayed image is updated with
the current contents of that file.
FRAME nil or omitted means use the selected frame.
FRAME t means refresh the image on all frames.  */)
     (spec, frame)
     Lisp_Object spec, frame;
{
  if (!valid_image_p (spec))
    error ("Invalid image specification");

  if (EQ (frame, Qt))
    {
      Lisp_Object tail;
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f))
	    uncache_image (f, spec);
	}
    }
  else
    uncache_image (check_x_frame (frame), spec);

  return Qnil;
}


/* Compute masks and transform image IMG on frame F, as specified
   by the image's specification,  */

static void
postprocess_image (f, img)
     struct frame *f;
     struct image *img;
{
  /* Manipulation of the image's mask.  */
  if (img->pixmap)
    {
      Lisp_Object conversion, spec;
      Lisp_Object mask;

      spec = img->spec;

      /* `:heuristic-mask t'
	 `:mask heuristic'
	 means build a mask heuristically.
	 `:heuristic-mask (R G B)'
	 `:mask (heuristic (R G B))'
	 means build a mask from color (R G B) in the
	 image.
	 `:mask nil'
	 means remove a mask, if any.  */

      mask = image_spec_value (spec, QCheuristic_mask, NULL);
      if (!NILP (mask))
	x_build_heuristic_mask (f, img, mask);
      else
	{
	  int found_p;

	  mask = image_spec_value (spec, QCmask, &found_p);

	  if (EQ (mask, Qheuristic))
	    x_build_heuristic_mask (f, img, Qt);
	  else if (CONSP (mask)
		   && EQ (XCAR (mask), Qheuristic))
	    {
	      if (CONSP (XCDR (mask)))
		x_build_heuristic_mask (f, img, XCAR (XCDR (mask)));
	      else
		x_build_heuristic_mask (f, img, XCDR (mask));
	    }
	  else if (NILP (mask) && found_p && img->mask)
	    {
	      Free_Pixmap (FRAME_X_DISPLAY (f), img->mask);
	      img->mask = NO_PIXMAP;
	    }
	}


      /* Should we apply an image transformation algorithm?  */
      conversion = image_spec_value (spec, QCconversion, NULL);
      if (EQ (conversion, Qdisabled))
	x_disable_image (f, img);
      else if (EQ (conversion, Qlaplace))
	x_laplace (f, img);
      else if (EQ (conversion, Qemboss))
	x_emboss (f, img);
      else if (CONSP (conversion)
	       && EQ (XCAR (conversion), Qedge_detection))
	{
	  Lisp_Object tem;
	  tem = XCDR (conversion);
	  if (CONSP (tem))
	    x_edge_detection (f, img,
			      Fplist_get (tem, QCmatrix),
			      Fplist_get (tem, QCcolor_adjustment));
	}
    }
}


/* Return the id of image with Lisp specification SPEC on frame F.
   SPEC must be a valid Lisp image specification (see valid_image_p).  */

int
lookup_image (f, spec)
     struct frame *f;
     Lisp_Object spec;
{
  struct image_cache *c;
  struct image *img;
  unsigned hash;
  struct gcpro gcpro1;
  EMACS_TIME now;

  /* F must be a window-system frame, and SPEC must be a valid image
     specification.  */
  xassert (FRAME_WINDOW_P (f));
  xassert (valid_image_p (spec));

  c = FRAME_IMAGE_CACHE (f);

  GCPRO1 (spec);

  /* Look up SPEC in the hash table of the image cache.  */
  hash = sxhash (spec, 0);
  img = search_image_cache (f, spec, hash);
  if (img && img->load_failed_p)
    {
      free_image (f, img);
      img = NULL;
    }

  /* If not found, create a new image and cache it.  */
  if (img == NULL)
    {
      extern Lisp_Object Qpostscript;

      BLOCK_INPUT;
      img = make_image (spec, hash);
      cache_image (f, img);
      img->load_failed_p = img->type->load (f, img) == 0;
      img->frame_foreground = FRAME_FOREGROUND_PIXEL (f);
      img->frame_background = FRAME_BACKGROUND_PIXEL (f);

      /* If we can't load the image, and we don't have a width and
	 height, use some arbitrary width and height so that we can
	 draw a rectangle for it.  */
      if (img->load_failed_p)
	{
	  Lisp_Object value;

	  value = image_spec_value (spec, QCwidth, NULL);
	  img->width = (INTEGERP (value)
			? XFASTINT (value) : DEFAULT_IMAGE_WIDTH);
	  value = image_spec_value (spec, QCheight, NULL);
	  img->height = (INTEGERP (value)
			 ? XFASTINT (value) : DEFAULT_IMAGE_HEIGHT);
	}
      else
	{
	  /* Handle image type independent image attributes
	     `:ascent ASCENT', `:margin MARGIN', `:relief RELIEF',
	     `:background COLOR'.  */
	  Lisp_Object ascent, margin, relief, bg;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (INTEGERP (ascent))
	    img->ascent = XFASTINT (ascent);
	  else if (EQ (ascent, Qcenter))
	    img->ascent = CENTERED_IMAGE_ASCENT;

	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (INTEGERP (margin) && XINT (margin) >= 0)
	    img->vmargin = img->hmargin = XFASTINT (margin);
	  else if (CONSP (margin) && INTEGERP (XCAR (margin))
		   && INTEGERP (XCDR (margin)))
	    {
	      if (XINT (XCAR (margin)) > 0)
		img->hmargin = XFASTINT (XCAR (margin));
	      if (XINT (XCDR (margin)) > 0)
		img->vmargin = XFASTINT (XCDR (margin));
	    }

	  relief = image_spec_value (spec, QCrelief, NULL);
	  if (INTEGERP (relief))
	    {
	      img->relief = XINT (relief);
	      img->hmargin += eabs (img->relief);
	      img->vmargin += eabs (img->relief);
	    }

	  if (! img->background_valid)
	    {
	      bg = image_spec_value (img->spec, QCbackground, NULL);
	      if (!NILP (bg))
		{
		  img->background
		    = x_alloc_image_color (f, img, bg,
					   FRAME_BACKGROUND_PIXEL (f));
		  img->background_valid = 1;
		}
	    }

	  /* Do image transformations and compute masks, unless we
	     don't have the image yet.  */
	  if (!EQ (*img->type->type, Qpostscript))
	    postprocess_image (f, img);
	}

      UNBLOCK_INPUT;
    }

  /* We're using IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (now);
  img->timestamp = EMACS_SECS (now);

  UNGCPRO;

  /* Value is the image id.  */
  return img->id;
}


/* Cache image IMG in the image cache of frame F.  */

static void
cache_image (f, img)
     struct frame *f;
     struct image *img;
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  int i;

  /* Find a free slot in c->images.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i] == NULL)
      break;

  /* If no free slot found, maybe enlarge c->images.  */
  if (i == c->used && c->used == c->size)
    {
      c->size *= 2;
      c->images = (struct image **) xrealloc (c->images,
					      c->size * sizeof *c->images);
    }

  /* Add IMG to c->images, and assign IMG an id.  */
  c->images[i] = img;
  img->id = i;
  if (i == c->used)
    ++c->used;

  /* Add IMG to the cache's hash table.  */
  i = img->hash % IMAGE_CACHE_BUCKETS_SIZE;
  img->next = c->buckets[i];
  if (img->next)
    img->next->prev = img;
  img->prev = NULL;
  c->buckets[i] = img;
}


/* Call FN on every image in the image cache of frame F.  Used to mark
   Lisp Objects in the image cache.  */

/* Mark Lisp objects in image IMG.  */

static void
mark_image (img)
     struct image *img;
{
  mark_object (img->spec);
  mark_object (img->dependencies);

  if (!NILP (img->data.lisp_val))
    mark_object (img->data.lisp_val);
}


void
mark_image_cache (struct image_cache *c)
{
  if (c)
    {
      int i;
      for (i = 0; i < c->used; ++i)
	if (c->images[i])
	  mark_image (c->images[i]);
    }
}



/***********************************************************************
			  X / MAC / W32 support code
 ***********************************************************************/

#ifdef HAVE_NTGUI

/* Macro for defining functions that will be loaded from image DLLs.  */
#define DEF_IMGLIB_FN(func) int (FAR CDECL *fn_##func)()

/* Macro for loading those image functions from the library.  */
#define LOAD_IMGLIB_FN(lib,func) {					\
    fn_##func = (void *) GetProcAddress (lib, #func);			\
    if (!fn_##func) return 0;						\
  }

/* Load a DLL implementing an image type.
   The `image-library-alist' variable associates a symbol,
   identifying an image type, to a list of possible filenames.
   The function returns NULL if no library could be loaded for
   the given image type, or if the library was previously loaded;
   else the handle of the DLL.  */
static HMODULE
w32_delayed_load (Lisp_Object libraries, Lisp_Object type)
{
  HMODULE library = NULL;

  if (CONSP (libraries) && NILP (Fassq (type, Vimage_type_cache)))
    {
      Lisp_Object dlls = Fassq (type, libraries);

      if (CONSP (dlls))
        for (dlls = XCDR (dlls); CONSP (dlls); dlls = XCDR (dlls))
          {
            CHECK_STRING_CAR (dlls);
            if (library = LoadLibrary (SDATA (XCAR (dlls))))
              break;
          }
    }

  return library;
}

#endif /* HAVE_NTGUI */

static int x_create_x_image_and_pixmap P_ ((struct frame *, int, int, int,
					    XImagePtr *, Pixmap *));
static void x_destroy_x_image P_ ((XImagePtr));
static void x_put_x_image P_ ((struct frame *, XImagePtr, Pixmap, int, int));


/* Create an XImage and a pixmap of size WIDTH x HEIGHT for use on
   frame F.  Set *XIMG and *PIXMAP to the XImage and Pixmap created.
   Set (*XIMG)->data to a raster of WIDTH x HEIGHT pixels allocated
   via xmalloc.  Print error messages via image_error if an error
   occurs.  Value is non-zero if successful.

   On W32, a DEPTH of zero signifies a 24 bit image, otherwise DEPTH
   should indicate the bit depth of the image.  */

static int
x_create_x_image_and_pixmap (f, width, height, depth, ximg, pixmap)
     struct frame *f;
     int width, height, depth;
     XImagePtr *ximg;
     Pixmap *pixmap;
{
#ifdef HAVE_X_WINDOWS
  Display *display = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);
  Screen *screen = FRAME_X_SCREEN (f);

  xassert (interrupt_input_blocked);

  if (depth <= 0)
    depth = DefaultDepthOfScreen (screen);
  *ximg = XCreateImage (display, DefaultVisualOfScreen (screen),
			depth, ZPixmap, 0, NULL, width, height,
			depth > 16 ? 32 : depth > 8 ? 16 : 8, 0);
  if (*ximg == NULL)
    {
      image_error ("Unable to allocate X image", Qnil, Qnil);
      return 0;
    }

  /* Allocate image raster.  */
  (*ximg)->data = (char *) xmalloc ((*ximg)->bytes_per_line * height);

  /* Allocate a pixmap of the same size.  */
  *pixmap = XCreatePixmap (display, window, width, height, depth);
  if (*pixmap == NO_PIXMAP)
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Unable to create X pixmap", Qnil, Qnil);
      return 0;
    }

  return 1;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI

  BITMAPINFOHEADER *header;
  HDC hdc;
  int scanline_width_bits;
  int remainder;
  int palette_colors = 0;

  if (depth == 0)
    depth = 24;

  if (depth != 1 && depth != 4 && depth != 8
      && depth != 16 && depth != 24 && depth != 32)
    {
      image_error ("Invalid image bit depth specified", Qnil, Qnil);
      return 0;
    }

  scanline_width_bits = width * depth;
  remainder = scanline_width_bits % 32;

  if (remainder)
    scanline_width_bits += 32 - remainder;

  /* Bitmaps with a depth less than 16 need a palette.  */
  /* BITMAPINFO structure already contains the first RGBQUAD.  */
  if (depth < 16)
    palette_colors = 1 << depth - 1;

  *ximg = xmalloc (sizeof (XImage) + palette_colors * sizeof (RGBQUAD));
  if (*ximg == NULL)
    {
      image_error ("Unable to allocate memory for XImage", Qnil, Qnil);
      return 0;
    }

  header = &((*ximg)->info.bmiHeader);
  bzero (&((*ximg)->info), sizeof (BITMAPINFO));
  header->biSize = sizeof (*header);
  header->biWidth = width;
  header->biHeight = -height;  /* negative indicates a top-down bitmap.  */
  header->biPlanes = 1;
  header->biBitCount = depth;
  header->biCompression = BI_RGB;
  header->biClrUsed = palette_colors;

  /* TODO: fill in palette.  */
  if (depth == 1)
    {
      (*ximg)->info.bmiColors[0].rgbBlue = 0;
      (*ximg)->info.bmiColors[0].rgbGreen = 0;
      (*ximg)->info.bmiColors[0].rgbRed = 0;
      (*ximg)->info.bmiColors[0].rgbReserved = 0;
      (*ximg)->info.bmiColors[1].rgbBlue = 255;
      (*ximg)->info.bmiColors[1].rgbGreen = 255;
      (*ximg)->info.bmiColors[1].rgbRed = 255;
      (*ximg)->info.bmiColors[1].rgbReserved = 0;
    }

  hdc = get_frame_dc (f);

  /* Create a DIBSection and raster array for the bitmap,
     and store its handle in *pixmap.  */
  *pixmap = CreateDIBSection (hdc, &((*ximg)->info),
			      (depth < 16) ? DIB_PAL_COLORS : DIB_RGB_COLORS,
			      /* casting avoids a GCC warning */
			      (void **)&((*ximg)->data), NULL, 0);

  /* Realize display palette and garbage all frames. */
  release_frame_dc (f, hdc);

  if (*pixmap == NULL)
    {
      DWORD err = GetLastError ();
      Lisp_Object errcode;
      /* All system errors are < 10000, so the following is safe.  */
      XSETINT (errcode, (int) err);
      image_error ("Unable to create bitmap, error code %d", errcode, Qnil);
      x_destroy_x_image (*ximg);
      return 0;
    }

  return 1;

#endif /* HAVE_NTGUI */

#ifdef MAC_OS
  Display *display = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);

  xassert (interrupt_input_blocked);

  /* Allocate a pixmap of the same size.  */
  *pixmap = XCreatePixmap (display, window, width, height, depth);
  if (*pixmap == NO_PIXMAP)
    {
      *ximg = NULL;
      image_error ("Unable to create X pixmap", Qnil, Qnil);
      return 0;
    }

  LockPixels (GetGWorldPixMap (*pixmap));
  *ximg = *pixmap;
  return 1;

#endif  /* MAC_OS */
}


/* Destroy XImage XIMG.  Free XIMG->data.  */

static void
x_destroy_x_image (ximg)
     XImagePtr ximg;
{
  xassert (interrupt_input_blocked);
  if (ximg)
    {
#ifdef HAVE_X_WINDOWS
      xfree (ximg->data);
      ximg->data = NULL;
      XDestroyImage (ximg);
#endif /* HAVE_X_WINDOWS */
#ifdef HAVE_NTGUI
      /* Data will be freed by DestroyObject.  */
      ximg->data = NULL;
      xfree (ximg);
#endif /* HAVE_NTGUI */
#ifdef MAC_OS
      XDestroyImage (ximg);
#endif /* MAC_OS */
    }
}


/* Put XImage XIMG into pixmap PIXMAP on frame F.  WIDTH and HEIGHT
   are width and height of both the image and pixmap.  */

static void
x_put_x_image (f, ximg, pixmap, width, height)
     struct frame *f;
     XImagePtr ximg;
     Pixmap pixmap;
     int width, height;
{
#ifdef HAVE_X_WINDOWS
  GC gc;

  xassert (interrupt_input_blocked);
  gc = XCreateGC (FRAME_X_DISPLAY (f), pixmap, 0, NULL);
  XPutImage (FRAME_X_DISPLAY (f), pixmap, gc, ximg, 0, 0, 0, 0, width, height);
  XFreeGC (FRAME_X_DISPLAY (f), gc);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
#if 0  /* I don't think this is necessary looking at where it is used.  */
  HDC hdc = get_frame_dc (f);
  SetDIBits (hdc, pixmap, 0, height, ximg->data, &(ximg->info), DIB_RGB_COLORS);
  release_frame_dc (f, hdc);
#endif
#endif /* HAVE_NTGUI */

#ifdef MAC_OS
  xassert (ximg == pixmap);
#endif  /* MAC_OS */
}


/***********************************************************************
			      File Handling
 ***********************************************************************/

static unsigned char *slurp_file P_ ((char *, int *));


/* Find image file FILE.  Look in data-directory/images, then
   x-bitmap-file-path.  Value is the encoded full name of the file
   found, or nil if not found.  */

Lisp_Object
x_find_image_file (file)
     Lisp_Object file;
{
  Lisp_Object file_found, search_path;
  struct gcpro gcpro1, gcpro2;
  int fd;

  file_found = Qnil;
  /* TODO I think this should use something like image-load-path
     instead.  Unfortunately, that can contain non-string elements.  */
  search_path = Fcons (Fexpand_file_name (build_string ("images"),
					  Vdata_directory),
		       Vx_bitmap_file_path);
  GCPRO2 (file_found, search_path);

  /* Try to find FILE in data-directory/images, then x-bitmap-file-path.  */
  fd = openp (search_path, file, Qnil, &file_found, Qnil);

  if (fd == -1)
    file_found = Qnil;
  else
    {
      file_found = ENCODE_FILE (file_found);
      close (fd);
    }

  UNGCPRO;
  return file_found;
}


/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  *SIZE is set to the size of the file.  */

static unsigned char *
slurp_file (file, size)
     char *file;
     int *size;
{
  FILE *fp = NULL;
  unsigned char *buf = NULL;
  struct stat st;

  if (stat (file, &st) == 0
      && (fp = fopen (file, "rb")) != NULL
      && (buf = (unsigned char *) xmalloc (st.st_size),
	  fread (buf, 1, st.st_size, fp) == st.st_size))
    {
      *size = st.st_size;
      fclose (fp);
    }
  else
    {
      if (fp)
	fclose (fp);
      if (buf)
	{
	  xfree (buf);
	  buf = NULL;
	}
    }

  return buf;
}



#ifdef MAC_OS

/***********************************************************************
			MAC Image Load Functions
 ***********************************************************************/

static int image_load_quicktime P_ ((struct frame *, struct image *img,
				     OSType));
#ifdef MAC_OSX
static int image_load_quartz2d P_ ((struct frame *, struct image *img, int));
#endif

static OSErr
find_image_fsspec (specified_file, file, fss)
     Lisp_Object specified_file, *file;
     FSSpec *fss;
{
  OSErr err;
  AEDesc desc;

  *file = x_find_image_file (specified_file);
  if (!STRINGP (*file))
    return fnfErr;		/* file or directory not found;
				   incomplete pathname */
  /* Try to open the image file.  */
  err = AECoercePtr (TYPE_FILE_NAME, SDATA (*file),
		     SBYTES (*file), typeFSS, &desc);
  if (err == noErr)
    {
#if TARGET_API_MAC_CARBON
      err = AEGetDescData (&desc, fss, sizeof (FSSpec));
#else
      *fss = *(FSSpec *)(*(desc.dataHandle));
#endif
      AEDisposeDesc (&desc);
    }
  return err;
}

static int
image_load_qt_1 (f, img, type, fss, dh)
     struct frame *f;
     struct image *img;
     OSType type;
     const FSSpec *fss;
     Handle dh;
{
  ComponentResult err;
  GraphicsImportComponent gi;
  Rect rect;
  int width, height;
  ImageDescriptionHandle desc_handle;
  short draw_all_pixels;
  Lisp_Object specified_bg;
  XColor color;
  XImagePtr ximg;
  RGBColor bg_color;

  err = OpenADefaultComponent (GraphicsImporterComponentType, type, &gi);
  if (err != noErr)
    {
      image_error ("Cannot get importer component for `%s'", img->spec, Qnil);
      return 0;
    }
  if (dh == NULL)
    {
      /* read from file system spec */
      err = GraphicsImportSetDataFile (gi, fss);
      if (err != noErr)
	{
	  image_error ("Cannot set fsspec to graphics importer for '%s'",
		       img->spec, Qnil);
	  goto error;
	}
    }
  else
    {
      /* read from data handle */
      err = GraphicsImportSetDataHandle (gi, dh);
      if (err != noErr)
	{
	  image_error ("Cannot set data handle to graphics importer for `%s'",
		       img->spec, Qnil);
	  goto error;
	}
    }
  err = GraphicsImportGetImageDescription (gi, &desc_handle);
  if (err != noErr || desc_handle == NULL)
    {
      image_error ("Error reading `%s'", img->spec, Qnil);
      goto error;
    }
  width = img->width = (*desc_handle)->width;
  height = img->height = (*desc_handle)->height;
  DisposeHandle ((Handle)desc_handle);

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      goto error;
    }

  err = GraphicsImportDoesDrawAllPixels (gi, &draw_all_pixels);
#if 0
  /* Don't check the error code here.  It may have an undocumented
     value -32766. */
  if (err != noErr)
    {
      image_error ("Error reading `%s'", img->spec, Qnil);
      goto error;
    }
#endif
  if (draw_all_pixels != graphicsImporterDrawsAllPixels)
    {
      specified_bg = image_spec_value (img->spec, QCbackground, NULL);
      if (!STRINGP (specified_bg) ||
	  !mac_defined_color (f, SDATA (specified_bg), &color, 0))
	{
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  color.red = RED16_FROM_ULONG (color.pixel);
	  color.green = GREEN16_FROM_ULONG (color.pixel);
	  color.blue = BLUE16_FROM_ULONG (color.pixel);
	}
    }

  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    goto error;
  if (draw_all_pixels != graphicsImporterDrawsAllPixels)
    {
      CGrafPtr old_port;
      GDHandle old_gdh;

      GetGWorld (&old_port, &old_gdh);
      SetGWorld (ximg, NULL);
      bg_color.red = color.red;
      bg_color.green = color.green;
      bg_color.blue = color.blue;
      RGBBackColor (&bg_color);
#if TARGET_API_MAC_CARBON
      GetPortBounds (ximg, &rect);
      EraseRect (&rect);
#else
      EraseRect (&(ximg->portRect));
#endif
      SetGWorld (old_port, old_gdh);
    }
  GraphicsImportSetGWorld (gi, ximg, NULL);
  GraphicsImportDraw (gi);
  CloseComponent (gi);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  return 1;

 error:
  CloseComponent (gi);
  return 0;
}


/* Load an image using the QuickTime Graphics Importer.
   Note: The alpha channel does not work for PNG images. */
static int
image_load_quicktime (f, img, type)
     struct frame *f;
     struct image *img;
     OSType type;
{
  Lisp_Object specified_file;
  Lisp_Object specified_data;
  OSErr err;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  if (NILP (specified_data))
    {
      /* Read from a file */
      Lisp_Object file;
      FSSpec fss;

      err = find_image_fsspec (specified_file, &file, &fss);
      if (err != noErr)
	{
	  if (err == fnfErr)
	    image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  else
	    image_error ("Cannot open `%s'", file, Qnil);
	  return 0;
	}
      return image_load_qt_1 (f, img, type, &fss, NULL);
    }
  else
    {
      /* Memory source! */
      int success_p;
      Handle dh;

      err = PtrToHand (SDATA (specified_data), &dh, SBYTES (specified_data));
      if (err != noErr)
	{
	  image_error ("Cannot allocate data handle for `%s'",
		       img->spec, Qnil);
	  return 0;
	}
      success_p = image_load_qt_1 (f, img, type, NULL, dh);
      DisposeHandle (dh);
      return success_p;
    }
}


#ifdef MAC_OSX
/* Load a PNG/JPEG image using Quartz 2D decoding routines.
   CGImageCreateWithPNGDataProvider is provided after Mac OS X 10.2.
   So don't use this function directly but determine at runtime
   whether it exists. */
typedef CGImageRef (*CGImageCreateWithPNGDataProviderProcType)
  (CGDataProviderRef, const float [], bool, CGColorRenderingIntent);
static CGImageCreateWithPNGDataProviderProcType MyCGImageCreateWithPNGDataProvider;


static void
init_image_func_pointer ()
{
  if (NSIsSymbolNameDefined ("_CGImageCreateWithPNGDataProvider"))
    {
      MyCGImageCreateWithPNGDataProvider
	= (CGImageCreateWithPNGDataProviderProcType)
	NSAddressOfSymbol (NSLookupAndBindSymbol
			   ("_CGImageCreateWithPNGDataProvider"));
    }
  else
    MyCGImageCreateWithPNGDataProvider = NULL;
}


static int
image_load_quartz2d (f, img, png_p)
     struct frame *f;
     struct image *img;
     int png_p;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data, specified_bg;
  struct gcpro gcpro1;
  CGDataProviderRef source;
  CGImageRef image;
  int width, height;
  XColor color;
  XImagePtr ximg = NULL;
  CGContextRef context;
  CGRect rectangle;

  /* Open the file.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      CFStringRef path;
      CFURLRef url;

      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}
      path = cfstring_create_with_utf8_cstring (SDATA (file));
      url = CFURLCreateWithFileSystemPath (NULL, path,
					   kCFURLPOSIXPathStyle, 0);
      CFRelease (path);
      source = CGDataProviderCreateWithURL (url);
      CFRelease (url);
    }
  else
    source = CGDataProviderCreateWithData (NULL, SDATA (specified_data),
					   SBYTES (specified_data), NULL);

  if (png_p)
    image = (*MyCGImageCreateWithPNGDataProvider) (source, NULL, FALSE,
						   kCGRenderingIntentDefault);
  else
    image = CGImageCreateWithJPEGDataProvider (source, NULL, FALSE,
					       kCGRenderingIntentDefault);

  CGDataProviderRelease (source);
  if (image == NULL)
    {
      UNGCPRO;
      image_error ("Error reading image `%s'", img->spec, Qnil);
      return 0;
    }
  width = img->width = CGImageGetWidth (image);
  height = img->height = CGImageGetHeight (image);

  if (!check_image_size (f, width, height))
    {
      CGImageRelease (image);
      UNGCPRO;
      image_error ("Invalid image size", Qnil, Qnil);
      return 0;
    }

  if (png_p)
    {
      specified_bg = image_spec_value (img->spec, QCbackground, NULL);
      if (!STRINGP (specified_bg) ||
	  !mac_defined_color (f, SDATA (specified_bg), &color, 0))
	{
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  color.red = RED16_FROM_ULONG (color.pixel);
	  color.green = GREEN16_FROM_ULONG (color.pixel);
	  color.blue = BLUE16_FROM_ULONG (color.pixel);
	}
    }

  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      CGImageRelease (image);
      UNGCPRO;
      return 0;
    }
  rectangle = CGRectMake (0, 0, width, height);
  QDBeginCGContext (ximg, &context);
  if (png_p)
    {
      CGContextSetRGBFillColor (context, color.red / 65535.0,
				color.green / 65535.0,
				color.blue / 65535.0, 1.0);
      CGContextFillRect (context, rectangle);
    }
  CGContextDrawImage (context, rectangle, image);
  QDEndCGContext (ximg, &context);
  CGImageRelease (image);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNGCPRO;
  return 1;
}
#endif

#endif  /* MAC_OS */


/***********************************************************************
			      XBM images
 ***********************************************************************/

static int xbm_scan P_ ((unsigned char **, unsigned char *, char *, int *));
static int xbm_load P_ ((struct frame *f, struct image *img));
static int xbm_load_image P_ ((struct frame *f, struct image *img,
			       unsigned char *, unsigned char *));
static int xbm_image_p P_ ((Lisp_Object object));
static int xbm_read_bitmap_data P_ ((struct frame *f,
				     unsigned char *, unsigned char *,
				     int *, int *, unsigned char **));
static int xbm_file_p P_ ((Lisp_Object));


/* Indices of image specification fields in xbm_format, below.  */

enum xbm_keyword_index
{
  XBM_TYPE,
  XBM_FILE,
  XBM_WIDTH,
  XBM_HEIGHT,
  XBM_DATA,
  XBM_FOREGROUND,
  XBM_BACKGROUND,
  XBM_ASCENT,
  XBM_MARGIN,
  XBM_RELIEF,
  XBM_ALGORITHM,
  XBM_HEURISTIC_MASK,
  XBM_MASK,
  XBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XBM image specifications.  */

static struct image_keyword xbm_format[XBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":width",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":height",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0}
};

/* Structure describing the image type XBM.  */

static struct image_type xbm_type =
{
  &Qxbm,
  xbm_image_p,
  xbm_load,
  x_clear_image,
  NULL
};

/* Tokens returned from xbm_scan.  */

enum xbm_token
{
  XBM_TK_IDENT = 256,
  XBM_TK_NUMBER
};


/* Return non-zero if OBJECT is a valid XBM-type image specification.
   A valid specification is a list starting with the symbol `image'
   The rest of the list is a property list which must contain an
   entry `:type xbm..

   If the specification specifies a file to load, it must contain
   an entry `:file FILENAME' where FILENAME is a string.

   If the specification is for a bitmap loaded from memory it must
   contain `:width WIDTH', `:height HEIGHT', and `:data DATA', where
   WIDTH and HEIGHT are integers > 0.  DATA may be:

   1. a string large enough to hold the bitmap data, i.e. it must
   have a size >= (WIDTH + 7) / 8 * HEIGHT

   2. a bool-vector of size >= WIDTH * HEIGHT

   3. a vector of strings or bool-vectors, one for each line of the
   bitmap.

   4. a string containing an in-memory XBM file.  WIDTH and HEIGHT
   may not be specified in this case because they are defined in the
   XBM file.

   Both the file and data forms may contain the additional entries
   `:background COLOR' and `:foreground COLOR'.  If not present,
   foreground and background of the frame on which the image is
   displayed is used.  */

static int
xbm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword kw[XBM_LAST];

  bcopy (xbm_format, kw, sizeof kw);
  if (!parse_image_spec (object, kw, XBM_LAST, Qxbm))
    return 0;

  xassert (EQ (kw[XBM_TYPE].value, Qxbm));

  if (kw[XBM_FILE].count)
    {
      if (kw[XBM_WIDTH].count || kw[XBM_HEIGHT].count || kw[XBM_DATA].count)
	return 0;
    }
  else if (kw[XBM_DATA].count && xbm_file_p (kw[XBM_DATA].value))
    {
      /* In-memory XBM file.  */
      if (kw[XBM_WIDTH].count || kw[XBM_HEIGHT].count || kw[XBM_FILE].count)
	return 0;
    }
  else
    {
      Lisp_Object data;
      int width, height;

      /* Entries for `:width', `:height' and `:data' must be present.  */
      if (!kw[XBM_WIDTH].count
	  || !kw[XBM_HEIGHT].count
	  || !kw[XBM_DATA].count)
	return 0;

      data = kw[XBM_DATA].value;
      width = XFASTINT (kw[XBM_WIDTH].value);
      height = XFASTINT (kw[XBM_HEIGHT].value);

      /* Check type of data, and width and height against contents of
	 data.  */
      if (VECTORP (data))
	{
	  int i;

	  /* Number of elements of the vector must be >= height.  */
	  if (XVECTOR (data)->size < height)
	    return 0;

	  /* Each string or bool-vector in data must be large enough
	     for one line of the image.  */
	  for (i = 0; i < height; ++i)
	    {
	      Lisp_Object elt = XVECTOR (data)->contents[i];

	      if (STRINGP (elt))
		{
		  if (SCHARS (elt)
		      < (width + BITS_PER_CHAR - 1) / BITS_PER_CHAR)
		    return 0;
		}
	      else if (BOOL_VECTOR_P (elt))
		{
		  if (XBOOL_VECTOR (elt)->size < width)
		    return 0;
		}
	      else
		return 0;
	    }
	}
      else if (STRINGP (data))
	{
	  if (SCHARS (data)
	      < (width + BITS_PER_CHAR - 1) / BITS_PER_CHAR * height)
	    return 0;
	}
      else if (BOOL_VECTOR_P (data))
	{
	  if (XBOOL_VECTOR (data)->size < width * height)
	    return 0;
	}
      else
	return 0;
    }

  return 1;
}


/* Scan a bitmap file.  FP is the stream to read from.  Value is
   either an enumerator from enum xbm_token, or a character for a
   single-character token, or 0 at end of file.  If scanning an
   identifier, store the lexeme of the identifier in SVAL.  If
   scanning a number, store its value in *IVAL.  */

static int
xbm_scan (s, end, sval, ival)
     unsigned char **s, *end;
     char *sval;
     int *ival;
{
  unsigned int c;

 loop:

  /* Skip white space.  */
  while (*s < end && (c = *(*s)++, isspace (c)))
    ;

  if (*s >= end)
    c = 0;
  else if (isdigit (c))
    {
      int value = 0, digit;

      if (c == '0' && *s < end)
	{
	  c = *(*s)++;
	  if (c == 'x' || c == 'X')
	    {
	      while (*s < end)
		{
		  c = *(*s)++;
		  if (isdigit (c))
		    digit = c - '0';
		  else if (c >= 'a' && c <= 'f')
		    digit = c - 'a' + 10;
		  else if (c >= 'A' && c <= 'F')
		    digit = c - 'A' + 10;
		  else
		    break;
		  value = 16 * value + digit;
		}
	    }
	  else if (isdigit (c))
	    {
	      value = c - '0';
	      while (*s < end
		     && (c = *(*s)++, isdigit (c)))
		value = 8 * value + c - '0';
	    }
	}
      else
	{
	  value = c - '0';
	  while (*s < end
		 && (c = *(*s)++, isdigit (c)))
	    value = 10 * value + c - '0';
	}

      if (*s < end)
	*s = *s - 1;
      *ival = value;
      c = XBM_TK_NUMBER;
    }
  else if (isalpha (c) || c == '_')
    {
      *sval++ = c;
      while (*s < end
	     && (c = *(*s)++, (isalnum (c) || c == '_')))
	*sval++ = c;
      *sval = 0;
      if (*s < end)
	*s = *s - 1;
      c = XBM_TK_IDENT;
    }
  else if (c == '/' && **s == '*')
    {
      /* C-style comment.  */
      ++*s;
      while (**s && (**s != '*' || *(*s + 1) != '/'))
	++*s;
      if (**s)
	{
	  *s += 2;
	  goto loop;
	}
    }

  return c;
}

#ifdef HAVE_NTGUI

/* Create a Windows bitmap from X bitmap data.  */
static HBITMAP
w32_create_pixmap_from_bitmap_data (int width, int height, char *data)
{
  static unsigned char swap_nibble[16]
    = { 0x0, 0x8, 0x4, 0xc,    /* 0000 1000 0100 1100 */
	0x2, 0xa, 0x6, 0xe,    /* 0010 1010 0110 1110 */
	0x1, 0x9, 0x5, 0xd,    /* 0001 1001 0101 1101 */
	0x3, 0xb, 0x7, 0xf };  /* 0011 1011 0111 1111 */
  int i, j, w1, w2;
  unsigned char *bits, *p;
  HBITMAP bmp;

  w1 = (width + 7) / 8;         /* nb of 8bits elt in X bitmap */
  w2 = ((width + 15) / 16) * 2; /* nb of 16bits elt in W32 bitmap */
  bits = (unsigned char *) alloca (height * w2);
  bzero (bits, height * w2);
  for (i = 0; i < height; i++)
    {
      p = bits + i*w2;
      for (j = 0; j < w1; j++)
	{
	  /* Bitswap XBM bytes to match how Windows does things.  */
	  unsigned char c = *data++;
	  *p++ = (unsigned char)((swap_nibble[c & 0xf] << 4)
				 | (swap_nibble[(c>>4) & 0xf]));
	}
    }
  bmp = CreateBitmap (width, height, 1, 1, (char *) bits);

  return bmp;
}

static void
convert_mono_to_color_image (f, img, foreground, background)
     struct frame *f;
     struct image *img;
     COLORREF foreground, background;
{
  HDC hdc, old_img_dc, new_img_dc;
  HGDIOBJ old_prev, new_prev;
  HBITMAP new_pixmap;

  hdc = get_frame_dc (f);
  old_img_dc = CreateCompatibleDC (hdc);
  new_img_dc = CreateCompatibleDC (hdc);
  new_pixmap = CreateCompatibleBitmap (hdc, img->width, img->height);
  release_frame_dc (f, hdc);
  old_prev = SelectObject (old_img_dc, img->pixmap);
  new_prev = SelectObject (new_img_dc, new_pixmap);
  /* Windows convention for mono bitmaps is black = background,
     white = foreground.  */
  SetTextColor (new_img_dc, background);
  SetBkColor (new_img_dc, foreground);

  BitBlt (new_img_dc, 0, 0, img->width, img->height, old_img_dc,
	  0, 0, SRCCOPY);

  SelectObject (old_img_dc, old_prev);
  SelectObject (new_img_dc, new_prev);
  DeleteDC (old_img_dc);
  DeleteDC (new_img_dc);
  DeleteObject (img->pixmap);
  if (new_pixmap == 0)
    fprintf (stderr, "Failed to convert image to color.\n");
  else
    img->pixmap = new_pixmap;
}

#define XBM_BIT_SHUFFLE(b) (~(b))

#else

#define XBM_BIT_SHUFFLE(b) (b)

#endif /* HAVE_NTGUI */


static void
Create_Pixmap_From_Bitmap_Data (f, img, data, fg, bg, non_default_colors)
     struct frame *f;
     struct image *img;
     char *data;
     RGB_PIXEL_COLOR fg, bg;
     int non_default_colors;
{
#ifdef HAVE_NTGUI
  img->pixmap
    = w32_create_pixmap_from_bitmap_data (img->width, img->height, data);

  /* If colors were specified, transfer the bitmap to a color one.  */
  if (non_default_colors)
    convert_mono_to_color_image (f, img, fg, bg);
#else
  img->pixmap
    = XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
				   FRAME_X_WINDOW (f),
				   data,
				   img->width, img->height,
				   fg, bg,
				   DefaultDepthOfScreen (FRAME_X_SCREEN (f)));
#endif /* HAVE_NTGUI */
}



/* Replacement for XReadBitmapFileData which isn't available under old
   X versions.  CONTENTS is a pointer to a buffer to parse; END is the
   buffer's end.  Set *WIDTH and *HEIGHT to the width and height of
   the image.  Return in *DATA the bitmap data allocated with xmalloc.
   Value is non-zero if successful.  DATA null means just test if
   CONTENTS looks like an in-memory XBM file.  */

static int
xbm_read_bitmap_data (f, contents, end, width, height, data)
     struct frame *f;
     unsigned char *contents, *end;
     int *width, *height;
     unsigned char **data;
{
  unsigned char *s = contents;
  char buffer[BUFSIZ];
  int padding_p = 0;
  int v10 = 0;
  int bytes_per_line, i, nbytes;
  unsigned char *p;
  int value;
  int LA1;

#define match() \
     LA1 = xbm_scan (&s, end, buffer, &value)

#define expect(TOKEN)		\
     if (LA1 != (TOKEN)) 	\
       goto failure;		\
     else			\
       match ()

#define expect_ident(IDENT)					\
     if (LA1 == XBM_TK_IDENT && strcmp (buffer, (IDENT)) == 0)	\
       match ();						\
     else							\
       goto failure

  *width = *height = -1;
  if (data)
    *data = NULL;
  LA1 = xbm_scan (&s, end, buffer, &value);

  /* Parse defines for width, height and hot-spots.  */
  while (LA1 == '#')
    {
      match ();
      expect_ident ("define");
      expect (XBM_TK_IDENT);

      if (LA1 == XBM_TK_NUMBER)
	{
          char *p = strrchr (buffer, '_');
	  p = p ? p + 1 : buffer;
          if (strcmp (p, "width") == 0)
	    *width = value;
          else if (strcmp (p, "height") == 0)
	    *height = value;
	}
      expect (XBM_TK_NUMBER);
    }

  if (!check_image_size (f, *width, *height))
    goto failure;
  else if (data == NULL)
    goto success;

  /* Parse bits.  Must start with `static'.  */
  expect_ident ("static");
  if (LA1 == XBM_TK_IDENT)
    {
      if (strcmp (buffer, "unsigned") == 0)
	{
	  match ();
	  expect_ident ("char");
	}
      else if (strcmp (buffer, "short") == 0)
	{
	  match ();
	  v10 = 1;
	  if (*width % 16 && *width % 16 < 9)
	    padding_p = 1;
	}
      else if (strcmp (buffer, "char") == 0)
	match ();
      else
	goto failure;
    }
  else
    goto failure;

  expect (XBM_TK_IDENT);
  expect ('[');
  expect (']');
  expect ('=');
  expect ('{');

  bytes_per_line = (*width + 7) / 8 + padding_p;
  nbytes = bytes_per_line * *height;
  p = *data = (unsigned char *) xmalloc (nbytes);

  if (v10)
    {
      for (i = 0; i < nbytes; i += 2)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = XBM_BIT_SHUFFLE (val);
	  if (!padding_p || ((i + 2) % bytes_per_line))
	    *p++ = XBM_BIT_SHUFFLE (value >> 8);

	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }
  else
    {
      for (i = 0; i < nbytes; ++i)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = XBM_BIT_SHUFFLE (val);

	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }

 success:
  return 1;

 failure:

  if (data && *data)
    {
      xfree (*data);
      *data = NULL;
    }
  return 0;

#undef match
#undef expect
#undef expect_ident
}


/* Load XBM image IMG which will be displayed on frame F from buffer
   CONTENTS.  END is the end of the buffer.  Value is non-zero if
   successful.  */

static int
xbm_load_image (f, img, contents, end)
     struct frame *f;
     struct image *img;
     unsigned char *contents, *end;
{
  int rc;
  unsigned char *data;
  int success_p = 0;

  rc = xbm_read_bitmap_data (f, contents, end, &img->width, &img->height, &data);
  if (rc)
    {
      unsigned long foreground = FRAME_FOREGROUND_PIXEL (f);
      unsigned long background = FRAME_BACKGROUND_PIXEL (f);
      int non_default_colors = 0;
      Lisp_Object value;

      xassert (img->width > 0 && img->height > 0);

      /* Get foreground and background colors, maybe allocate colors.  */
      value = image_spec_value (img->spec, QCforeground, NULL);
      if (!NILP (value))
	{
	  foreground = x_alloc_image_color (f, img, value, foreground);
	  non_default_colors = 1;
	}
      value = image_spec_value (img->spec, QCbackground, NULL);
      if (!NILP (value))
	{
	  background = x_alloc_image_color (f, img, value, background);
	  img->background = background;
	  img->background_valid = 1;
	  non_default_colors = 1;
	}

      Create_Pixmap_From_Bitmap_Data (f, img, data,
				      foreground, background,
				      non_default_colors);
      xfree (data);

      if (img->pixmap == NO_PIXMAP)
	{
	  x_clear_image (f, img);
	  image_error ("Unable to create X pixmap for `%s'", img->spec, Qnil);
	}
      else
	success_p = 1;
    }
  else
    image_error ("Error loading XBM image `%s'", img->spec, Qnil);

  return success_p;
}


/* Value is non-zero if DATA looks like an in-memory XBM file.  */

static int
xbm_file_p (data)
     Lisp_Object data;
{
  int w, h;
  return (STRINGP (data)
	  && xbm_read_bitmap_data (NULL, SDATA (data),
				   (SDATA (data)
				    + SBYTES (data)),
				   &w, &h, NULL));
}


/* Fill image IMG which is used on frame F with pixmap data.  Value is
   non-zero if successful.  */

static int
xbm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int success_p = 0;
  Lisp_Object file_name;

  xassert (xbm_image_p (img->spec));

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      unsigned char *contents;
      int size;
      struct gcpro gcpro1;

      file = x_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (SDATA (file), &size);
      if (contents == NULL)
	{
	  image_error ("Error loading XBM image `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      success_p = xbm_load_image (f, img, contents, contents + size);
      UNGCPRO;
    }
  else
    {
      struct image_keyword fmt[XBM_LAST];
      Lisp_Object data;
      unsigned long foreground = FRAME_FOREGROUND_PIXEL (f);
      unsigned long background = FRAME_BACKGROUND_PIXEL (f);
      int non_default_colors = 0;
      char *bits;
      int parsed_p;
      int in_memory_file_p = 0;

      /* See if data looks like an in-memory XBM file.  */
      data = image_spec_value (img->spec, QCdata, NULL);
      in_memory_file_p = xbm_file_p (data);

      /* Parse the image specification.  */
      bcopy (xbm_format, fmt, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, XBM_LAST, Qxbm);
      xassert (parsed_p);

      /* Get specified width, and height.  */
      if (!in_memory_file_p)
	{
	  img->width = XFASTINT (fmt[XBM_WIDTH].value);
	  img->height = XFASTINT (fmt[XBM_HEIGHT].value);
	  xassert (img->width > 0 && img->height > 0);
	}

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[XBM_FOREGROUND].count
	  && STRINGP (fmt[XBM_FOREGROUND].value))
	{
	  foreground = x_alloc_image_color (f, img, fmt[XBM_FOREGROUND].value,
					    foreground);
	  non_default_colors = 1;
	}

      if (fmt[XBM_BACKGROUND].count
	  && STRINGP (fmt[XBM_BACKGROUND].value))
	{
	  background = x_alloc_image_color (f, img, fmt[XBM_BACKGROUND].value,
					    background);
	  non_default_colors = 1;
	}

      if (in_memory_file_p)
	success_p = xbm_load_image (f, img, SDATA (data),
				    (SDATA (data)
				     + SBYTES (data)));
      else
	{
	  if (VECTORP (data))
	    {
	      int i;
	      char *p;
	      int nbytes = (img->width + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

	      p = bits = (char *) alloca (nbytes * img->height);
	      for (i = 0; i < img->height; ++i, p += nbytes)
		{
		  Lisp_Object line = XVECTOR (data)->contents[i];
		  if (STRINGP (line))
		    bcopy (SDATA (line), p, nbytes);
		  else
		    bcopy (XBOOL_VECTOR (line)->data, p, nbytes);
		}
	    }
	  else if (STRINGP (data))
	    bits = SDATA (data);
	  else
	    bits = XBOOL_VECTOR (data)->data;

#ifdef WINDOWSNT
          {
            char *invertedBits;
            int nbytes, i;
            /* Windows mono bitmaps are reversed compared with X.  */
            invertedBits = bits;
            nbytes = (img->width + BITS_PER_CHAR - 1) / BITS_PER_CHAR 
              * img->height;
            bits = (char *) alloca(nbytes);
            for (i = 0; i < nbytes; i++)
              bits[i] = XBM_BIT_SHUFFLE (invertedBits[i]);
          }
#endif
	  /* Create the pixmap.  */

	  Create_Pixmap_From_Bitmap_Data (f, img, bits,
					  foreground, background,
					  non_default_colors);
	  if (img->pixmap)
	    success_p = 1;
	  else
	    {
	      image_error ("Unable to create pixmap for XBM image `%s'",
			   img->spec, Qnil);
	      x_clear_image (f, img);
	    }
	}
    }

  return success_p;
}



/***********************************************************************
			      XPM images
 ***********************************************************************/

#if defined (HAVE_XPM) || defined (MAC_OS)

static int xpm_image_p P_ ((Lisp_Object object));
static int xpm_load P_ ((struct frame *f, struct image *img));
static int xpm_valid_color_symbols_p P_ ((Lisp_Object));

#endif /* HAVE_XPM || MAC_OS */

#ifdef HAVE_XPM
#ifdef HAVE_NTGUI
/* Indicate to xpm.h that we don't have Xlib.  */
#define FOR_MSW
/* simx.h in xpm defines XColor and XImage differently than Emacs.  */
/* It also defines Display the same way as Emacs, but gcc 3.3 still barfs.  */
#define XColor xpm_XColor
#define XImage xpm_XImage
#define Display xpm_Display
#define PIXEL_ALREADY_TYPEDEFED
#include "X11/xpm.h"
#undef FOR_MSW
#undef XColor
#undef XImage
#undef Display
#undef PIXEL_ALREADY_TYPEDEFED
#else
#include "X11/xpm.h"
#endif /* HAVE_NTGUI */
#endif /* HAVE_XPM */

#if defined (HAVE_XPM) || defined (MAC_OS)
/* The symbol `xpm' identifying XPM-format images.  */

Lisp_Object Qxpm;

/* Indices of image specification fields in xpm_format, below.  */

enum xpm_keyword_index
{
  XPM_TYPE,
  XPM_FILE,
  XPM_DATA,
  XPM_ASCENT,
  XPM_MARGIN,
  XPM_RELIEF,
  XPM_ALGORITHM,
  XPM_HEURISTIC_MASK,
  XPM_MASK,
  XPM_COLOR_SYMBOLS,
  XPM_BACKGROUND,
  XPM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XPM image specifications.  */

static struct image_keyword xpm_format[XPM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":color-symbols",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type XPM.  */

static struct image_type xpm_type =
{
  &Qxpm,
  xpm_image_p,
  xpm_load,
  x_clear_image,
  NULL
};

#ifdef HAVE_X_WINDOWS

/* Define ALLOC_XPM_COLORS if we can use Emacs' own color allocation
   functions for allocating image colors.  Our own functions handle
   color allocation failures more gracefully than the ones on the XPM
   lib.  */

#if defined XpmAllocColor && defined XpmFreeColors && defined XpmColorClosure
#define ALLOC_XPM_COLORS
#endif
#endif /* HAVE_X_WINDOWS */

#ifdef ALLOC_XPM_COLORS

static void xpm_init_color_cache P_ ((struct frame *, XpmAttributes *));
static void xpm_free_color_cache P_ ((void));
static int xpm_lookup_color P_ ((struct frame *, char *, XColor *));
static int xpm_color_bucket P_ ((char *));
static struct xpm_cached_color *xpm_cache_color P_ ((struct frame *, char *,
						     XColor *, int));

/* An entry in a hash table used to cache color definitions of named
   colors.  This cache is necessary to speed up XPM image loading in
   case we do color allocations ourselves.  Without it, we would need
   a call to XParseColor per pixel in the image.  */

struct xpm_cached_color
{
  /* Next in collision chain.  */
  struct xpm_cached_color *next;

  /* Color definition (RGB and pixel color).  */
  XColor color;

  /* Color name.  */
  char name[1];
};

/* The hash table used for the color cache, and its bucket vector
   size.  */

#define XPM_COLOR_CACHE_BUCKETS	1001
struct xpm_cached_color **xpm_color_cache;

/* Initialize the color cache.  */

static void
xpm_init_color_cache (f, attrs)
     struct frame *f;
     XpmAttributes *attrs;
{
  size_t nbytes = XPM_COLOR_CACHE_BUCKETS * sizeof *xpm_color_cache;
  xpm_color_cache = (struct xpm_cached_color **) xmalloc (nbytes);
  memset (xpm_color_cache, 0, nbytes);
  init_color_table ();

  if (attrs->valuemask & XpmColorSymbols)
    {
      int i;
      XColor color;

      for (i = 0; i < attrs->numsymbols; ++i)
	if (XParseColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
			 attrs->colorsymbols[i].value, &color))
	  {
	    color.pixel = lookup_rgb_color (f, color.red, color.green,
					    color.blue);
	    xpm_cache_color (f, attrs->colorsymbols[i].name, &color, -1);
	  }
    }
}

/* Free the color cache.  */

static void
xpm_free_color_cache ()
{
  struct xpm_cached_color *p, *next;
  int i;

  for (i = 0; i < XPM_COLOR_CACHE_BUCKETS; ++i)
    for (p = xpm_color_cache[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (xpm_color_cache);
  xpm_color_cache = NULL;
  free_color_table ();
}

/* Return the bucket index for color named COLOR_NAME in the color
   cache.  */

static int
xpm_color_bucket (color_name)
     char *color_name;
{
  unsigned h = 0;
  char *s;

  for (s = color_name; *s; ++s)
    h = (h << 2) ^ *s;
  return h %= XPM_COLOR_CACHE_BUCKETS;
}


/* On frame F, cache values COLOR for color with name COLOR_NAME.
   BUCKET, if >= 0, is a precomputed bucket index.  Value is the cache
   entry added.  */

static struct xpm_cached_color *
xpm_cache_color (f, color_name, color, bucket)
     struct frame *f;
     char *color_name;
     XColor *color;
     int bucket;
{
  size_t nbytes;
  struct xpm_cached_color *p;

  if (bucket < 0)
    bucket = xpm_color_bucket (color_name);

  nbytes = sizeof *p + strlen (color_name);
  p = (struct xpm_cached_color *) xmalloc (nbytes);
  strcpy (p->name, color_name);
  p->color = *color;
  p->next = xpm_color_cache[bucket];
  xpm_color_cache[bucket] = p;
  return p;
}

/* Look up color COLOR_NAME for frame F in the color cache.  If found,
   return the cached definition in *COLOR.  Otherwise, make a new
   entry in the cache and allocate the color.  Value is zero if color
   allocation failed.  */

static int
xpm_lookup_color (f, color_name, color)
     struct frame *f;
     char *color_name;
     XColor *color;
{
  struct xpm_cached_color *p;
  int h = xpm_color_bucket (color_name);

  for (p = xpm_color_cache[h]; p; p = p->next)
    if (strcmp (p->name, color_name) == 0)
      break;

  if (p != NULL)
    *color = p->color;
  else if (XParseColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
			color_name, color))
    {
      color->pixel = lookup_rgb_color (f, color->red, color->green,
				       color->blue);
      p = xpm_cache_color (f, color_name, color, h);
    }
  /* You get `opaque' at least from ImageMagick converting pbm to xpm
     with transparency, and it's useful.  */
  else if (strcmp ("opaque", color_name) == 0)
    {
      bzero (color, sizeof (XColor));  /* Is this necessary/correct?  */
      color->pixel = FRAME_FOREGROUND_PIXEL (f);
      p = xpm_cache_color (f, color_name, color, h);
    }

  return p != NULL;
}


/* Callback for allocating color COLOR_NAME.  Called from the XPM lib.
   CLOSURE is a pointer to the frame on which we allocate the
   color.  Return in *COLOR the allocated color.  Value is non-zero
   if successful.  */

static int
xpm_alloc_color (dpy, cmap, color_name, color, closure)
     Display *dpy;
     Colormap cmap;
     char *color_name;
     XColor *color;
     void *closure;
{
  return xpm_lookup_color ((struct frame *) closure, color_name, color);
}


/* Callback for freeing NPIXELS colors contained in PIXELS.  CLOSURE
   is a pointer to the frame on which we allocate the color.  Value is
   non-zero if successful.  */

static int
xpm_free_colors (dpy, cmap, pixels, npixels, closure)
     Display *dpy;
     Colormap cmap;
     Pixel *pixels;
     int npixels;
     void *closure;
{
  return 1;
}

#endif /* ALLOC_XPM_COLORS */


#ifdef HAVE_NTGUI

/* XPM library details.  */

DEF_IMGLIB_FN (XpmFreeAttributes);
DEF_IMGLIB_FN (XpmCreateImageFromBuffer);
DEF_IMGLIB_FN (XpmReadFileToImage);
DEF_IMGLIB_FN (XImageFree);

static int
init_xpm_functions (Lisp_Object libraries)
{
  HMODULE library;

  if (!(library = w32_delayed_load (libraries, Qxpm)))
    return 0;

  LOAD_IMGLIB_FN (library, XpmFreeAttributes);
  LOAD_IMGLIB_FN (library, XpmCreateImageFromBuffer);
  LOAD_IMGLIB_FN (library, XpmReadFileToImage);
  LOAD_IMGLIB_FN (library, XImageFree);
  return 1;
}

#endif /* HAVE_NTGUI */


/* Value is non-zero if COLOR_SYMBOLS is a valid color symbols list
   for XPM images.  Such a list must consist of conses whose car and
   cdr are strings.  */

static int
xpm_valid_color_symbols_p (color_symbols)
     Lisp_Object color_symbols;
{
  while (CONSP (color_symbols))
    {
      Lisp_Object sym = XCAR (color_symbols);
      if (!CONSP (sym)
	  || !STRINGP (XCAR (sym))
	  || !STRINGP (XCDR (sym)))
	break;
      color_symbols = XCDR (color_symbols);
    }

  return NILP (color_symbols);
}


/* Value is non-zero if OBJECT is a valid XPM image specification.  */

static int
xpm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[XPM_LAST];
  bcopy (xpm_format, fmt, sizeof fmt);
  return (parse_image_spec (object, fmt, XPM_LAST, Qxpm)
	  /* Either `:file' or `:data' must be present.  */
	  && fmt[XPM_FILE].count + fmt[XPM_DATA].count == 1
	  /* Either no `:color-symbols' or it's a list of conses
	     whose car and cdr are strings.  */
	  && (fmt[XPM_COLOR_SYMBOLS].count == 0
	      || xpm_valid_color_symbols_p (fmt[XPM_COLOR_SYMBOLS].value)));
}

#endif /* HAVE_XPM || MAC_OS */

#if defined (HAVE_XPM) && defined (HAVE_X_WINDOWS)
int
x_create_bitmap_from_xpm_data (f, bits)
     struct frame *f;
     char **bits;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int id, rc;
  XpmAttributes attrs;
  Pixmap bitmap, mask;

  bzero (&attrs, sizeof attrs);

  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;

  rc = XpmCreatePixmapFromData (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				bits, &bitmap, &mask, &attrs);
  if (rc != XpmSuccess)
    {
      XpmFreeAttributes (&attrs);
      return -1;
    }

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = 1;
  dpyinfo->bitmaps[id - 1].mask = mask;
  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].height = attrs.height;
  dpyinfo->bitmaps[id - 1].width = attrs.width;
  dpyinfo->bitmaps[id - 1].depth = attrs.depth;
  dpyinfo->bitmaps[id - 1].refcount = 1;

  XpmFreeAttributes (&attrs);
  return id;
}
#endif /* defined (HAVE_XPM) && defined (HAVE_X_WINDOWS) */

/* Load image IMG which will be displayed on frame F.  Value is
   non-zero if successful.  */

#ifdef HAVE_XPM

static int
xpm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int rc;
  XpmAttributes attrs;
  Lisp_Object specified_file, color_symbols;
#ifdef HAVE_NTGUI
  HDC hdc;
  xpm_XImage * xpm_image = NULL, * xpm_mask = NULL;
#endif /* HAVE_NTGUI */

  /* Configure the XPM lib.  Use the visual of frame F.  Allocate
     close colors.  Return colors allocated.  */
  bzero (&attrs, sizeof attrs);

#ifndef HAVE_NTGUI
  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;
#endif /* HAVE_NTGUI */

#ifdef ALLOC_XPM_COLORS
  /* Allocate colors with our own functions which handle
     failing color allocation more gracefully.  */
  attrs.color_closure = f;
  attrs.alloc_color = xpm_alloc_color;
  attrs.free_colors = xpm_free_colors;
  attrs.valuemask |= XpmAllocColor | XpmFreeColors | XpmColorClosure;
#else /* not ALLOC_XPM_COLORS */
  /* Let the XPM lib allocate colors.  */
  attrs.valuemask |= XpmReturnAllocPixels;
#ifdef XpmAllocCloseColors
  attrs.alloc_close_colors = 1;
  attrs.valuemask |= XpmAllocCloseColors;
#else /* not XpmAllocCloseColors */
  attrs.closeness = 600;
  attrs.valuemask |= XpmCloseness;
#endif /* not XpmAllocCloseColors */
#endif /* ALLOC_XPM_COLORS */

  /* If image specification contains symbolic color definitions, add
     these to `attrs'.  */
  color_symbols = image_spec_value (img->spec, QCcolor_symbols, NULL);
  if (CONSP (color_symbols))
    {
      Lisp_Object tail;
      XpmColorSymbol *xpm_syms;
      int i, size;

      attrs.valuemask |= XpmColorSymbols;

      /* Count number of symbols.  */
      attrs.numsymbols = 0;
      for (tail = color_symbols; CONSP (tail); tail = XCDR (tail))
	++attrs.numsymbols;

      /* Allocate an XpmColorSymbol array.  */
      size = attrs.numsymbols * sizeof *xpm_syms;
      xpm_syms = (XpmColorSymbol *) alloca (size);
      bzero (xpm_syms, size);
      attrs.colorsymbols = xpm_syms;

      /* Fill the color symbol array.  */
      for (tail = color_symbols, i = 0;
	   CONSP (tail);
	   ++i, tail = XCDR (tail))
	{
	  Lisp_Object name = XCAR (XCAR (tail));
	  Lisp_Object color = XCDR (XCAR (tail));
	  xpm_syms[i].name = (char *) alloca (SCHARS (name) + 1);
	  strcpy (xpm_syms[i].name, SDATA (name));
	  xpm_syms[i].value = (char *) alloca (SCHARS (color) + 1);
	  strcpy (xpm_syms[i].value, SDATA (color));
	}
    }

  /* Create a pixmap for the image, either from a file, or from a
     string buffer containing data in the same format as an XPM file.  */
#ifdef ALLOC_XPM_COLORS
  xpm_init_color_cache (f, &attrs);
#endif

  specified_file = image_spec_value (img->spec, QCfile, NULL);

#ifdef HAVE_NTGUI
  {
    HDC frame_dc = get_frame_dc (f);
    hdc = CreateCompatibleDC (frame_dc);
    release_frame_dc (f, frame_dc);
  }
#endif /* HAVE_NTGUI */

  if (STRINGP (specified_file))
    {
      Lisp_Object file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  return 0;
	}

#ifdef HAVE_NTGUI
      /* XpmReadFileToPixmap is not available in the Windows port of
	 libxpm.  But XpmReadFileToImage almost does what we want.  */
      rc = fn_XpmReadFileToImage (&hdc, SDATA (file),
				  &xpm_image, &xpm_mask,
				  &attrs);
#else
      rc = XpmReadFileToPixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				SDATA (file), &img->pixmap, &img->mask,
				&attrs);
#endif /* HAVE_NTGUI */
    }
  else
    {
      Lisp_Object buffer = image_spec_value (img->spec, QCdata, NULL);
#ifdef HAVE_NTGUI
      /* XpmCreatePixmapFromBuffer is not available in the Windows port
	 of libxpm.  But XpmCreateImageFromBuffer almost does what we want.  */
      rc = fn_XpmCreateImageFromBuffer (&hdc, SDATA (buffer),
					&xpm_image, &xpm_mask,
					&attrs);
#else
      rc = XpmCreatePixmapFromBuffer (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				      SDATA (buffer),
				      &img->pixmap, &img->mask,
				      &attrs);
#endif /* HAVE_NTGUI */
    }

  if (rc == XpmSuccess)
    {
#if defined (COLOR_TABLE_SUPPORT) && defined (ALLOC_XPM_COLORS)
      img->colors = colors_in_color_table (&img->ncolors);
#else /* not ALLOC_XPM_COLORS */
      int i;

#ifdef HAVE_NTGUI
      /* W32 XPM uses XImage to wrap what W32 Emacs calls a Pixmap,
	 plus some duplicate attributes.  */
      if (xpm_image && xpm_image->bitmap)
	{
	  img->pixmap = xpm_image->bitmap;
	  /* XImageFree in libXpm frees XImage struct without destroying
	     the bitmap, which is what we want.  */
	  fn_XImageFree (xpm_image);
	}
      if (xpm_mask && xpm_mask->bitmap)
	{
	  /* The mask appears to be inverted compared with what we expect.
	     TODO: invert our expectations.  See other places where we
	     have to invert bits because our idea of masks is backwards.  */
	  HGDIOBJ old_obj;
	  old_obj = SelectObject (hdc, xpm_mask->bitmap);

	  PatBlt (hdc, 0, 0, xpm_mask->width, xpm_mask->height, DSTINVERT);
	  SelectObject (hdc, old_obj);

	  img->mask = xpm_mask->bitmap;
	  fn_XImageFree (xpm_mask);
	  DeleteDC (hdc);
	}

      DeleteDC (hdc);
#endif /* HAVE_NTGUI */

      /* Remember allocated colors.  */
      img->ncolors = attrs.nalloc_pixels;
      img->colors = (unsigned long *) xmalloc (img->ncolors
					       * sizeof *img->colors);
      for (i = 0; i < attrs.nalloc_pixels; ++i)
	{
	  img->colors[i] = attrs.alloc_pixels[i];
#ifdef DEBUG_X_COLORS
	  register_color (img->colors[i]);
#endif
	}
#endif /* not ALLOC_XPM_COLORS */

      img->width = attrs.width;
      img->height = attrs.height;
      xassert (img->width > 0 && img->height > 0);

      /* The call to XpmFreeAttributes below frees attrs.alloc_pixels.  */
#ifdef HAVE_NTGUI
      fn_XpmFreeAttributes (&attrs);
#else
      XpmFreeAttributes (&attrs);
#endif /* HAVE_NTGUI */
    }
  else
    {
#ifdef HAVE_NTGUI
      DeleteDC (hdc);
#endif /* HAVE_NTGUI */

      switch (rc)
	{
	case XpmOpenFailed:
	  image_error ("Error opening XPM file (%s)", img->spec, Qnil);
	  break;

	case XpmFileInvalid:
	  image_error ("Invalid XPM file (%s)", img->spec, Qnil);
	  break;

	case XpmNoMemory:
	  image_error ("Out of memory (%s)", img->spec, Qnil);
	  break;

	case XpmColorFailed:
	  image_error ("Color allocation error (%s)", img->spec, Qnil);
	  break;

	default:
	  image_error ("Unknown error (%s)", img->spec, Qnil);
	  break;
	}
    }

#ifdef ALLOC_XPM_COLORS
  xpm_free_color_cache ();
#endif
  return rc == XpmSuccess;
}

#endif /* HAVE_XPM */

#ifdef MAC_OS

/* XPM support functions for Mac OS where libxpm is not available.
   Only XPM version 3 (without any extensions) is supported.  */

static int xpm_scan P_ ((const unsigned char **, const unsigned char *,
			 const unsigned char **, int *));
static Lisp_Object xpm_make_color_table_v
  P_ ((void (**) (Lisp_Object, const unsigned char *, int, Lisp_Object),
       Lisp_Object (**) (Lisp_Object, const unsigned char *, int)));
static void xpm_put_color_table_v P_ ((Lisp_Object, const unsigned char *,
				       int, Lisp_Object));
static Lisp_Object xpm_get_color_table_v P_ ((Lisp_Object,
					      const unsigned char *, int));
static Lisp_Object xpm_make_color_table_h
  P_ ((void (**) (Lisp_Object, const unsigned char *, int, Lisp_Object),
       Lisp_Object (**) (Lisp_Object, const unsigned char *, int)));
static void xpm_put_color_table_h P_ ((Lisp_Object, const unsigned char *,
				       int, Lisp_Object));
static Lisp_Object xpm_get_color_table_h P_ ((Lisp_Object,
					      const unsigned char *, int));
static int xpm_str_to_color_key P_ ((const char *));
static int xpm_load_image P_ ((struct frame *, struct image *,
			       const unsigned char *, const unsigned char *));

/* Tokens returned from xpm_scan.  */

enum xpm_token
{
  XPM_TK_IDENT = 256,
  XPM_TK_STRING,
  XPM_TK_EOF
};

/* Scan an XPM data and return a character (< 256) or a token defined
   by enum xpm_token above.  *S and END are the start (inclusive) and
   the end (exclusive) addresses of the data, respectively.  Advance
   *S while scanning.  If token is either XPM_TK_IDENT or
   XPM_TK_STRING, *BEG and *LEN are set to the start address and the
   length of the corresponding token, respectively.  */

static int
xpm_scan (s, end, beg, len)
     const unsigned char **s, *end, **beg;
     int *len;
{
  int c;

  while (*s < end)
    {
      /* Skip white-space.  */
      while (*s < end && (c = *(*s)++, isspace (c)))
	;

      /* gnus-pointer.xpm uses '-' in its identifier.
	 sb-dir-plus.xpm uses '+' in its identifier.  */
      if (isalpha (c) || c == '_' || c == '-' || c == '+')
	{
	  *beg = *s - 1;
	  while (*s < end &&
		 (c = **s, isalnum (c) || c == '_' || c == '-' || c == '+'))
	      ++*s;
	  *len = *s - *beg;
	  return XPM_TK_IDENT;
	}
      else if (c == '"')
	{
	  *beg = *s;
	  while (*s < end && **s != '"')
	    ++*s;
	  *len = *s - *beg;
	  if (*s < end)
	    ++*s;
	  return XPM_TK_STRING;
	}
      else if (c == '/')
	{
	  if (*s < end && **s == '*')
	    {
	      /* C-style comment.  */
	      ++*s;
	      do
		{
		  while (*s < end && *(*s)++ != '*')
		    ;
		}
	      while (*s < end && **s != '/');
	      if (*s < end)
		++*s;
	    }
	  else
	    return c;
	}
      else
	return c;
    }

  return XPM_TK_EOF;
}

/* Functions for color table lookup in XPM data.  A key is a string
   specifying the color of each pixel in XPM data.  A value is either
   an integer that specifies a pixel color, Qt that specifies
   transparency, or Qnil for the unspecified color.  If the length of
   the key string is one, a vector is used as a table.  Otherwise, a
   hash table is used.  */

static Lisp_Object
xpm_make_color_table_v (put_func, get_func)
     void (**put_func) (Lisp_Object, const unsigned char *, int, Lisp_Object);
     Lisp_Object (**get_func) (Lisp_Object, const unsigned char *, int);
{
  *put_func = xpm_put_color_table_v;
  *get_func = xpm_get_color_table_v;
  return Fmake_vector (make_number (256), Qnil);
}

static void
xpm_put_color_table_v (color_table, chars_start, chars_len, color)
     Lisp_Object color_table;
     const unsigned char *chars_start;
     int chars_len;
     Lisp_Object color;
{
  XVECTOR (color_table)->contents[*chars_start] = color;
}

static Lisp_Object
xpm_get_color_table_v (color_table, chars_start, chars_len)
     Lisp_Object color_table;
     const unsigned char *chars_start;
     int chars_len;
{
  return XVECTOR (color_table)->contents[*chars_start];
}

static Lisp_Object
xpm_make_color_table_h (put_func, get_func)
     void (**put_func) (Lisp_Object, const unsigned char *, int, Lisp_Object);
     Lisp_Object (**get_func) (Lisp_Object, const unsigned char *, int);
{
  *put_func = xpm_put_color_table_h;
  *get_func = xpm_get_color_table_h;
  return make_hash_table (Qequal, make_number (DEFAULT_HASH_SIZE),
			  make_float (DEFAULT_REHASH_SIZE),
			  make_float (DEFAULT_REHASH_THRESHOLD),
			  Qnil, Qnil, Qnil);
}

static void
xpm_put_color_table_h (color_table, chars_start, chars_len, color)
     Lisp_Object color_table;
     const unsigned char *chars_start;
     int chars_len;
     Lisp_Object color;
{
  struct Lisp_Hash_Table *table = XHASH_TABLE (color_table);
  unsigned hash_code;
  Lisp_Object chars = make_unibyte_string (chars_start, chars_len);

  hash_lookup (table, chars, &hash_code);
  hash_put (table, chars, color, hash_code);
}

static Lisp_Object
xpm_get_color_table_h (color_table, chars_start, chars_len)
     Lisp_Object color_table;
     const unsigned char *chars_start;
     int chars_len;
{
  struct Lisp_Hash_Table *table = XHASH_TABLE (color_table);
  int i = hash_lookup (table, make_unibyte_string (chars_start, chars_len),
		       NULL);

  return i >= 0 ? HASH_VALUE (table, i) : Qnil;
}

enum xpm_color_key {
  XPM_COLOR_KEY_S,
  XPM_COLOR_KEY_M,
  XPM_COLOR_KEY_G4,
  XPM_COLOR_KEY_G,
  XPM_COLOR_KEY_C
};

static const char xpm_color_key_strings[][4] = {"s", "m", "g4", "g", "c"};

static int
xpm_str_to_color_key (s)
     const char *s;
{
  int i;

  for (i = 0;
       i < sizeof xpm_color_key_strings / sizeof xpm_color_key_strings[0];
       i++)
    if (strcmp (xpm_color_key_strings[i], s) == 0)
      return i;
  return -1;
}

static int
xpm_load_image (f, img, contents, end)
     struct frame *f;
     struct image *img;
     const unsigned char *contents, *end;
{
  const unsigned char *s = contents, *beg, *str;
  unsigned char buffer[BUFSIZ];
  int width, height, x, y;
  int num_colors, chars_per_pixel;
  int len, LA1;
  void (*put_color_table) (Lisp_Object, const unsigned char *, int, Lisp_Object);
  Lisp_Object (*get_color_table) (Lisp_Object, const unsigned char *, int);
  Lisp_Object frame, color_symbols, color_table;
  int best_key, have_mask = 0;
  XImagePtr ximg = NULL, mask_img = NULL;

#define match() \
     LA1 = xpm_scan (&s, end, &beg, &len)

#define expect(TOKEN)		\
     if (LA1 != (TOKEN)) 	\
       goto failure;		\
     else			\
       match ()

#define expect_ident(IDENT)					\
     if (LA1 == XPM_TK_IDENT \
         && strlen ((IDENT)) == len && memcmp ((IDENT), beg, len) == 0)	\
       match ();						\
     else							\
       goto failure

  if (!(end - s >= 9 && memcmp (s, "/* XPM */", 9) == 0))
    goto failure;
  s += 9;
  match();
  expect_ident ("static");
  expect_ident ("char");
  expect ('*');
  expect (XPM_TK_IDENT);
  expect ('[');
  expect (']');
  expect ('=');
  expect ('{');
  expect (XPM_TK_STRING);
  if (len >= BUFSIZ)
    goto failure;
  memcpy (buffer, beg, len);
  buffer[len] = '\0';
  if (sscanf (buffer, "%d %d %d %d", &width, &height,
	      &num_colors, &chars_per_pixel) != 4
      || width <= 0 || height <= 0
      || num_colors <= 0 || chars_per_pixel <= 0)
    goto failure;

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      goto failure;
    }

  expect (',');

  XSETFRAME (frame, f);
  if (!NILP (Fxw_display_color_p (frame)))
    best_key = XPM_COLOR_KEY_C;
  else if (!NILP (Fx_display_grayscale_p (frame)))
    best_key = (XFASTINT (Fx_display_planes (frame)) > 2
		? XPM_COLOR_KEY_G : XPM_COLOR_KEY_G4);
  else
    best_key = XPM_COLOR_KEY_M;

  color_symbols = image_spec_value (img->spec, QCcolor_symbols, NULL);
  if (chars_per_pixel == 1)
    color_table = xpm_make_color_table_v (&put_color_table,
					  &get_color_table);
  else
    color_table = xpm_make_color_table_h (&put_color_table,
					  &get_color_table);

  while (num_colors-- > 0)
    {
      unsigned char *color, *max_color;
      int key, next_key, max_key = 0;
      Lisp_Object symbol_color = Qnil, color_val;
      XColor cdef;

      expect (XPM_TK_STRING);
      if (len <= chars_per_pixel || len >= BUFSIZ + chars_per_pixel)
	goto failure;
      memcpy (buffer, beg + chars_per_pixel, len - chars_per_pixel);
      buffer[len - chars_per_pixel] = '\0';

      str = strtok (buffer, " \t");
      if (str == NULL)
	goto failure;
      key = xpm_str_to_color_key (str);
      if (key < 0)
	goto failure;
      do
	{
	  color = strtok (NULL, " \t");
	  if (color == NULL)
	    goto failure;

	  while ((str = strtok (NULL, " \t")) != NULL)
	    {
	      next_key = xpm_str_to_color_key (str);
	      if (next_key >= 0)
		break;
	      color[strlen (color)] = ' ';
	    }

	  if (key == XPM_COLOR_KEY_S)
	    {
	      if (NILP (symbol_color))
		symbol_color = build_string (color);
	    }
	  else if (max_key < key && key <= best_key)
	    {
	      max_key = key;
	      max_color = color;
	    }
	  key = next_key;
	}
      while (str);

      color_val = Qnil;
      if (!NILP (color_symbols) && !NILP (symbol_color))
	{
	  Lisp_Object specified_color = Fassoc (symbol_color, color_symbols);

	  if (CONSP (specified_color) && STRINGP (XCDR (specified_color)))
	    {
	      if (xstricmp (SDATA (XCDR (specified_color)), "None") == 0)
		color_val = Qt;
	      else if (x_defined_color (f, SDATA (XCDR (specified_color)),
					&cdef, 0))
		color_val = make_number (cdef.pixel);
	    }
	}
      if (NILP (color_val) && max_key > 0)
	{
	  if (xstricmp (max_color, "None") == 0)
	    color_val = Qt;
	  else if (x_defined_color (f, max_color, &cdef, 0))
	    color_val = make_number (cdef.pixel);
	}
      if (!NILP (color_val))
	(*put_color_table) (color_table, beg, chars_per_pixel, color_val);

      expect (',');
    }

  if (!x_create_x_image_and_pixmap (f, width, height, 0,
				    &ximg, &img->pixmap)
      || !x_create_x_image_and_pixmap (f, width, height, 1,
				       &mask_img, &img->mask))
    {
      image_error ("Out of memory (%s)", img->spec, Qnil);
      goto error;
    }

  for (y = 0; y < height; y++)
    {
      expect (XPM_TK_STRING);
      str = beg;
      if (len < width * chars_per_pixel)
	goto failure;
      for (x = 0; x < width; x++, str += chars_per_pixel)
	{
	  Lisp_Object color_val =
	    (*get_color_table) (color_table, str, chars_per_pixel);

	  XPutPixel (ximg, x, y,
		     (INTEGERP (color_val) ? XINT (color_val)
		      : FRAME_FOREGROUND_PIXEL (f)));
	  XPutPixel (mask_img, x, y,
		     (!EQ (color_val, Qt) ? PIX_MASK_DRAW
		      : (have_mask = 1, PIX_MASK_RETAIN)));
	}
      if (y + 1 < height)
	expect (',');
    }

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  if (have_mask)
    {
      /* Fill in the background_transparent field while we have the
	 mask handy.  */
      image_background_transparent (img, f, mask_img);

      x_put_x_image (f, mask_img, img->mask, width, height);
      x_destroy_x_image (mask_img);
    }
  else
    {
      x_destroy_x_image (mask_img);
      Free_Pixmap (FRAME_X_DISPLAY (f), img->mask);
      img->mask = NO_PIXMAP;
    }

  return 1;

 failure:
  image_error ("Invalid XPM file (%s)", img->spec, Qnil);
 error:
  x_destroy_x_image (ximg);
  x_destroy_x_image (mask_img);
  x_clear_image (f, img);
  return 0;

#undef match
#undef expect
#undef expect_ident
}

static int
xpm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int success_p = 0;
  Lisp_Object file_name;

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      unsigned char *contents;
      int size;
      struct gcpro gcpro1;

      file = x_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (SDATA (file), &size);
      if (contents == NULL)
	{
	  image_error ("Error loading XPM image `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      success_p = xpm_load_image (f, img, contents, contents + size);
      xfree (contents);
      UNGCPRO;
    }
  else
    {
      Lisp_Object data;

      data = image_spec_value (img->spec, QCdata, NULL);
      success_p = xpm_load_image (f, img, SDATA (data),
				  SDATA (data) + SBYTES (data));
    }

  return success_p;
}

#endif /* MAC_OS */



/***********************************************************************
			     Color table
 ***********************************************************************/

#ifdef COLOR_TABLE_SUPPORT

/* An entry in the color table mapping an RGB color to a pixel color.  */

struct ct_color
{
  int r, g, b;
  unsigned long pixel;

  /* Next in color table collision list.  */
  struct ct_color *next;
};

/* The bucket vector size to use.  Must be prime.  */

#define CT_SIZE 101

/* Value is a hash of the RGB color given by R, G, and B.  */

#define CT_HASH_RGB(R, G, B) (((R) << 16) ^ ((G) << 8) ^ (B))

/* The color hash table.  */

struct ct_color **ct_table;

/* Number of entries in the color table.  */

int ct_colors_allocated;

/* Initialize the color table.  */

static void
init_color_table ()
{
  int size = CT_SIZE * sizeof (*ct_table);
  ct_table = (struct ct_color **) xmalloc (size);
  bzero (ct_table, size);
  ct_colors_allocated = 0;
}


/* Free memory associated with the color table.  */

static void
free_color_table ()
{
  int i;
  struct ct_color *p, *next;

  for (i = 0; i < CT_SIZE; ++i)
    for (p = ct_table[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (ct_table);
  ct_table = NULL;
}


/* Value is a pixel color for RGB color R, G, B on frame F.  If an
   entry for that color already is in the color table, return the
   pixel color of that entry.  Otherwise, allocate a new color for R,
   G, B, and make an entry in the color table.  */

static unsigned long
lookup_rgb_color (f, r, g, b)
     struct frame *f;
     int r, g, b;
{
  unsigned hash = CT_HASH_RGB (r, g, b);
  int i = hash % CT_SIZE;
  struct ct_color *p;
  Display_Info *dpyinfo;

  /* Handle TrueColor visuals specially, which improves performance by
     two orders of magnitude.  Freeing colors on TrueColor visuals is
     a nop, and pixel colors specify RGB values directly.  See also
     the Xlib spec, chapter 3.1.  */
  dpyinfo = FRAME_X_DISPLAY_INFO (f);
  if (dpyinfo->red_bits > 0)
    {
      unsigned long pr, pg, pb;

      /* Apply gamma-correction like normal color allocation does.  */
      if (f->gamma)
	{
	  XColor color;
	  color.red = r, color.green = g, color.blue = b;
	  gamma_correct (f, &color);
	  r = color.red, g = color.green, b = color.blue;
	}

      /* Scale down RGB values to the visual's bits per RGB, and shift
	 them to the right position in the pixel color.  Note that the
	 original RGB values are 16-bit values, as usual in X.  */
      pr = (r >> (16 - dpyinfo->red_bits))   << dpyinfo->red_offset;
      pg = (g >> (16 - dpyinfo->green_bits)) << dpyinfo->green_offset;
      pb = (b >> (16 - dpyinfo->blue_bits))  << dpyinfo->blue_offset;

      /* Assemble the pixel color.  */
      return pr | pg | pb;
    }

  for (p = ct_table[i]; p; p = p->next)
    if (p->r == r && p->g == g && p->b == b)
      break;

  if (p == NULL)
    {

#ifdef HAVE_X_WINDOWS
      XColor color;
      Colormap cmap;
      int rc;

      color.red = r;
      color.green = g;
      color.blue = b;

      cmap = FRAME_X_COLORMAP (f);
      rc = x_alloc_nearest_color (f, cmap, &color);
      if (rc)
	{
	  ++ct_colors_allocated;
	  p = (struct ct_color *) xmalloc (sizeof *p);
	  p->r = r;
	  p->g = g;
	  p->b = b;
	  p->pixel = color.pixel;
	  p->next = ct_table[i];
	  ct_table[i] = p;
	}
      else
	return FRAME_FOREGROUND_PIXEL (f);

#else
      COLORREF color;
#ifdef HAVE_NTGUI
      color = PALETTERGB (r, g, b);
#else
      color = RGB_TO_ULONG (r, g, b);
#endif /* HAVE_NTGUI */
      ++ct_colors_allocated;
      p = (struct ct_color *) xmalloc (sizeof *p);
      p->r = r;
      p->g = g;
      p->b = b;
      p->pixel = color;
      p->next = ct_table[i];
      ct_table[i] = p;
#endif /* HAVE_X_WINDOWS */

    }

  return p->pixel;
}


/* Look up pixel color PIXEL which is used on frame F in the color
   table.  If not already present, allocate it.  Value is PIXEL.  */

static unsigned long
lookup_pixel_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
  int i = pixel % CT_SIZE;
  struct ct_color *p;

  for (p = ct_table[i]; p; p = p->next)
    if (p->pixel == pixel)
      break;

  if (p == NULL)
    {
      XColor color;
      Colormap cmap;
      int rc;

#ifdef HAVE_X_WINDOWS
      cmap = FRAME_X_COLORMAP (f);
      color.pixel = pixel;
      x_query_color (f, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);
#else
      BLOCK_INPUT;
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      color.pixel = pixel;
      XQueryColor (NULL, cmap, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);
      UNBLOCK_INPUT;
#endif /* HAVE_X_WINDOWS */

      if (rc)
	{
	  ++ct_colors_allocated;

	  p = (struct ct_color *) xmalloc (sizeof *p);
	  p->r = color.red;
	  p->g = color.green;
	  p->b = color.blue;
	  p->pixel = pixel;
	  p->next = ct_table[i];
	  ct_table[i] = p;
	}
      else
	return FRAME_FOREGROUND_PIXEL (f);
    }
  return p->pixel;
}


/* Value is a vector of all pixel colors contained in the color table,
   allocated via xmalloc.  Set *N to the number of colors.  */

static unsigned long *
colors_in_color_table (n)
     int *n;
{
  int i, j;
  struct ct_color *p;
  unsigned long *colors;

  if (ct_colors_allocated == 0)
    {
      *n = 0;
      colors = NULL;
    }
  else
    {
      colors = (unsigned long *) xmalloc (ct_colors_allocated
					  * sizeof *colors);
      *n = ct_colors_allocated;

      for (i = j = 0; i < CT_SIZE; ++i)
	for (p = ct_table[i]; p; p = p->next)
	  colors[j++] = p->pixel;
    }

  return colors;
}

#else /* COLOR_TABLE_SUPPORT */

static unsigned long
lookup_rgb_color (f, r, g, b)
     struct frame *f;
     int r, g, b;
{
  unsigned long pixel;

#ifdef MAC_OS
  pixel = RGB_TO_ULONG (r >> 8, g >> 8, b >> 8);
  gamma_correct (f, &pixel);
#endif /* MAC_OS */

#ifdef HAVE_NTGUI
  pixel = PALETTERGB (r >> 8, g >> 8, b >> 8);
#endif /* HAVE_NTGUI */

  return pixel;
}

static void
init_color_table ()
{
}
#endif /* COLOR_TABLE_SUPPORT */


/***********************************************************************
			      Algorithms
 ***********************************************************************/

static XColor *x_to_xcolors P_ ((struct frame *, struct image *, int));
static void x_from_xcolors P_ ((struct frame *, struct image *, XColor *));
static void x_detect_edges P_ ((struct frame *, struct image *, int[9], int));

#ifdef HAVE_NTGUI
static void XPutPixel (XImagePtr , int, int, COLORREF);
#endif /* HAVE_NTGUI */

/* Non-zero means draw a cross on images having `:conversion
   disabled'.  */

int cross_disabled_images;

/* Edge detection matrices for different edge-detection
   strategies.  */

static int emboss_matrix[9] = {
   /* x - 1	x	x + 1  */
        2,     -1,  	  0,		/* y - 1 */
       -1,      0,        1,		/* y     */
        0,      1,       -2		/* y + 1 */
};

static int laplace_matrix[9] = {
   /* x - 1	x	x + 1  */
        1,      0,  	  0,		/* y - 1 */
        0,      0,        0,		/* y     */
        0,      0,       -1		/* y + 1 */
};

/* Value is the intensity of the color whose red/green/blue values
   are R, G, and B.  */

#define COLOR_INTENSITY(R, G, B) ((2 * (R) + 3 * (G) + (B)) / 6)


/* On frame F, return an array of XColor structures describing image
   IMG->pixmap.  Each XColor structure has its pixel color set.  RGB_P
   non-zero means also fill the red/green/blue members of the XColor
   structures.  Value is a pointer to the array of XColors structures,
   allocated with xmalloc; it must be freed by the caller.  */

static XColor *
x_to_xcolors (f, img, rgb_p)
     struct frame *f;
     struct image *img;
     int rgb_p;
{
  int x, y;
  XColor *colors, *p;
  XImagePtr_or_DC ximg;
#ifdef HAVE_NTGUI
  HDC hdc;
  HGDIOBJ prev;
#endif /* HAVE_NTGUI */

  colors = (XColor *) xmalloc (img->width * img->height * sizeof *colors);

#ifndef HAVE_NTGUI
  /* Get the X image IMG->pixmap.  */
  ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
		    0, 0, img->width, img->height, ~0, ZPixmap);
#else
  /* Load the image into a memory device context.  */
  hdc = get_frame_dc (f);
  ximg = CreateCompatibleDC (hdc);
  release_frame_dc (f, hdc);
  prev = SelectObject (ximg, img->pixmap);
#endif /* HAVE_NTGUI */

  /* Fill the `pixel' members of the XColor array.  I wished there
     were an easy and portable way to circumvent XGetPixel.  */
  p = colors;
  for (y = 0; y < img->height; ++y)
    {
      XColor *row = p;

#ifdef HAVE_X_WINDOWS
      for (x = 0; x < img->width; ++x, ++p)
	p->pixel = XGetPixel (ximg, x, y);
      if (rgb_p)
	x_query_colors (f, row, img->width);

#else

      for (x = 0; x < img->width; ++x, ++p)
	{
	  /* W32_TODO: palette support needed here?  */
	  p->pixel = GET_PIXEL (ximg, x, y);
	  if (rgb_p)
	    {
#ifdef MAC_OS
	      p->red = RED16_FROM_ULONG (p->pixel);
	      p->green = GREEN16_FROM_ULONG (p->pixel);
	      p->blue = BLUE16_FROM_ULONG (p->pixel);
#endif  /* MAC_OS */
#ifdef HAVE_NTGUI
	      p->red = 256 * GetRValue (p->pixel);
	      p->green = 256 * GetGValue (p->pixel);
	      p->blue = 256 * GetBValue (p->pixel);
#endif /* HAVE_NTGUI */
	    }
	}
#endif /* HAVE_X_WINDOWS */
    }

  Destroy_Image (ximg, prev);

  return colors;
}

#ifdef HAVE_NTGUI

/* Put a pixel of COLOR at position X, Y in XIMG.  XIMG must have been
   created with CreateDIBSection, with the pointer to the bit values
   stored in ximg->data.  */

static void
XPutPixel (ximg, x, y, color)
     XImagePtr  ximg;
     int x, y;
     COLORREF color;
{
  int width = ximg->info.bmiHeader.biWidth;
  int height = ximg->info.bmiHeader.biHeight;
  unsigned char * pixel;

  /* True color images.  */
  if (ximg->info.bmiHeader.biBitCount == 24)
    {
      int rowbytes = width * 3;
      /* Ensure scanlines are aligned on 4 byte boundaries.  */
      if (rowbytes % 4)
	rowbytes += 4 - (rowbytes % 4);

      pixel = ximg->data + y * rowbytes + x * 3;
      /* Windows bitmaps are in BGR order.  */
      *pixel = GetBValue (color);
      *(pixel + 1) = GetGValue (color);
      *(pixel + 2) = GetRValue (color);
    }
  /* Monochrome images.  */
  else if (ximg->info.bmiHeader.biBitCount == 1)
    {
      int rowbytes = width / 8;
      /* Ensure scanlines are aligned on 4 byte boundaries.  */
      if (rowbytes % 4)
	rowbytes += 4 - (rowbytes % 4);
      pixel = ximg->data + y * rowbytes + x / 8;
      /* Filter out palette info.  */
      if (color & 0x00ffffff)
	*pixel = *pixel | (1 << x % 8);
      else
	*pixel = *pixel & ~(1 << x % 8);
    }
  else
    image_error ("XPutPixel: palette image not supported", Qnil, Qnil);
}

#endif /* HAVE_NTGUI */

/* Create IMG->pixmap from an array COLORS of XColor structures, whose
   RGB members are set.  F is the frame on which this all happens.
   COLORS will be freed; an existing IMG->pixmap will be freed, too.  */

static void
x_from_xcolors (f, img, colors)
     struct frame *f;
     struct image *img;
     XColor *colors;
{
  int x, y;
  XImagePtr oimg;
  Pixmap pixmap;
  XColor *p;

  init_color_table ();

  x_create_x_image_and_pixmap (f, img->width, img->height, 0,
			       &oimg, &pixmap);
  p = colors;
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x, ++p)
      {
	unsigned long pixel;
	pixel = lookup_rgb_color (f, p->red, p->green, p->blue);
	XPutPixel (oimg, x, y, pixel);
      }

  xfree (colors);
  x_clear_image_1 (f, img, 1, 0, 1);

  x_put_x_image (f, oimg, pixmap, img->width, img->height);
  x_destroy_x_image (oimg);
  img->pixmap = pixmap;
#ifdef COLOR_TABLE_SUPPORT
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */
}


/* On frame F, perform edge-detection on image IMG.

   MATRIX is a nine-element array specifying the transformation
   matrix.  See emboss_matrix for an example.

   COLOR_ADJUST is a color adjustment added to each pixel of the
   outgoing image.  */

static void
x_detect_edges (f, img, matrix, color_adjust)
     struct frame *f;
     struct image *img;
     int matrix[9], color_adjust;
{
  XColor *colors = x_to_xcolors (f, img, 1);
  XColor *new, *p;
  int x, y, i, sum;

  for (i = sum = 0; i < 9; ++i)
    sum += eabs (matrix[i]);

#define COLOR(A, X, Y) ((A) + (Y) * img->width + (X))

  new = (XColor *) xmalloc (img->width * img->height * sizeof *new);

  for (y = 0; y < img->height; ++y)
    {
      p = COLOR (new, 0, y);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, img->width - 1, y);
      p->red = p->green = p->blue = 0xffff/2;
    }

  for (x = 1; x < img->width - 1; ++x)
    {
      p = COLOR (new, x, 0);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, x, img->height - 1);
      p->red = p->green = p->blue = 0xffff/2;
    }

  for (y = 1; y < img->height - 1; ++y)
    {
      p = COLOR (new, 1, y);

      for (x = 1; x < img->width - 1; ++x, ++p)
	{
	  int r, g, b, y1, x1;

	  r = g = b = i = 0;
	  for (y1 = y - 1; y1 < y + 2; ++y1)
	    for (x1 = x - 1; x1 < x + 2; ++x1, ++i)
	      if (matrix[i])
	        {
	          XColor *t = COLOR (colors, x1, y1);
		  r += matrix[i] * t->red;
		  g += matrix[i] * t->green;
		  b += matrix[i] * t->blue;
		}

	  r = (r / sum + color_adjust) & 0xffff;
	  g = (g / sum + color_adjust) & 0xffff;
	  b = (b / sum + color_adjust) & 0xffff;
	  p->red = p->green = p->blue = COLOR_INTENSITY (r, g, b);
	}
    }

  xfree (colors);
  x_from_xcolors (f, img, new);

#undef COLOR
}


/* Perform the pre-defined `emboss' edge-detection on image IMG
   on frame F.  */

static void
x_emboss (f, img)
     struct frame *f;
     struct image *img;
{
  x_detect_edges (f, img, emboss_matrix, 0xffff / 2);
}


/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

static void
x_laplace (f, img)
     struct frame *f;
     struct image *img;
{
  x_detect_edges (f, img, laplace_matrix, 45000);
}


/* Perform edge-detection on image IMG on frame F, with specified
   transformation matrix MATRIX and color-adjustment COLOR_ADJUST.

   MATRIX must be either

   - a list of at least 9 numbers in row-major form
   - a vector of at least 9 numbers

   COLOR_ADJUST nil means use a default; otherwise it must be a
   number.  */

static void
x_edge_detection (f, img, matrix, color_adjust)
     struct frame *f;
     struct image *img;
     Lisp_Object matrix, color_adjust;
{
  int i = 0;
  int trans[9];

  if (CONSP (matrix))
    {
      for (i = 0;
	   i < 9 && CONSP (matrix) && NUMBERP (XCAR (matrix));
	   ++i, matrix = XCDR (matrix))
	trans[i] = XFLOATINT (XCAR (matrix));
    }
  else if (VECTORP (matrix) && ASIZE (matrix) >= 9)
    {
      for (i = 0; i < 9 && NUMBERP (AREF (matrix, i)); ++i)
	trans[i] = XFLOATINT (AREF (matrix, i));
    }

  if (NILP (color_adjust))
    color_adjust = make_number (0xffff / 2);

  if (i == 9 && NUMBERP (color_adjust))
    x_detect_edges (f, img, trans, (int) XFLOATINT (color_adjust));
}


/* Transform image IMG on frame F so that it looks disabled.  */

static void
x_disable_image (f, img)
     struct frame *f;
     struct image *img;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
#ifdef HAVE_NTGUI
  int n_planes = dpyinfo->n_planes * dpyinfo->n_cbits;
#else
  int n_planes = dpyinfo->n_planes;
#endif /* HAVE_NTGUI */

  if (n_planes >= 2)
    {
      /* Color (or grayscale).  Convert to gray, and equalize.  Just
	 drawing such images with a stipple can look very odd, so
	 we're using this method instead.  */
      XColor *colors = x_to_xcolors (f, img, 1);
      XColor *p, *end;
      const int h = 15000;
      const int l = 30000;

      for (p = colors, end = colors + img->width * img->height;
	   p < end;
	   ++p)
	{
	  int i = COLOR_INTENSITY (p->red, p->green, p->blue);
	  int i2 = (0xffff - h - l) * i / 0xffff + l;
	  p->red = p->green = p->blue = i2;
	}

      x_from_xcolors (f, img, colors);
    }

  /* Draw a cross over the disabled image, if we must or if we
     should.  */
  if (n_planes < 2 || cross_disabled_images)
    {
#ifndef HAVE_NTGUI
      Display *dpy = FRAME_X_DISPLAY (f);
      GC gc;

#ifdef MAC_OS
#define MaskForeground(f)  PIX_MASK_DRAW
#else
#define MaskForeground(f)  WHITE_PIX_DEFAULT (f)
#endif

      gc = XCreateGC (dpy, img->pixmap, 0, NULL);
      XSetForeground (dpy, gc, BLACK_PIX_DEFAULT (f));
      XDrawLine (dpy, img->pixmap, gc, 0, 0,
		 img->width - 1, img->height - 1);
      XDrawLine (dpy, img->pixmap, gc, 0, img->height - 1,
		 img->width - 1, 0);
      XFreeGC (dpy, gc);

      if (img->mask)
	{
	  gc = XCreateGC (dpy, img->mask, 0, NULL);
	  XSetForeground (dpy, gc, MaskForeground (f));
	  XDrawLine (dpy, img->mask, gc, 0, 0,
		     img->width - 1, img->height - 1);
	  XDrawLine (dpy, img->mask, gc, 0, img->height - 1,
		     img->width - 1, 0);
	  XFreeGC (dpy, gc);
	}
#else
      HDC hdc, bmpdc;
      HGDIOBJ prev;

      hdc = get_frame_dc (f);
      bmpdc = CreateCompatibleDC (hdc);
      release_frame_dc (f, hdc);

      prev = SelectObject (bmpdc, img->pixmap);

      SetTextColor (bmpdc, BLACK_PIX_DEFAULT (f));
      MoveToEx (bmpdc, 0, 0, NULL);
      LineTo (bmpdc, img->width - 1, img->height - 1);
      MoveToEx (bmpdc, 0, img->height - 1, NULL);
      LineTo (bmpdc, img->width - 1, 0);

      if (img->mask)
	{
	  SelectObject (bmpdc, img->mask);
	  SetTextColor (bmpdc, WHITE_PIX_DEFAULT (f));
	  MoveToEx (bmpdc, 0, 0, NULL);
	  LineTo (bmpdc, img->width - 1, img->height - 1);
	  MoveToEx (bmpdc, 0, img->height - 1, NULL);
	  LineTo (bmpdc, img->width - 1, 0);
	}
      SelectObject (bmpdc, prev);
      DeleteDC (bmpdc);
#endif /* HAVE_NTGUI */
    }
}


/* Build a mask for image IMG which is used on frame F.  FILE is the
   name of an image file, for error messages.  HOW determines how to
   determine the background color of IMG.  If it is a list '(R G B)',
   with R, G, and B being integers >= 0, take that as the color of the
   background.  Otherwise, determine the background color of IMG
   heuristically.  Value is non-zero if successful. */

static int
x_build_heuristic_mask (f, img, how)
     struct frame *f;
     struct image *img;
     Lisp_Object how;
{
  XImagePtr_or_DC ximg;
#ifndef HAVE_NTGUI
  XImagePtr mask_img;
#else
  HDC frame_dc;
  HGDIOBJ prev;
  char *mask_img;
  int row_width;
#endif /* HAVE_NTGUI */
  int x, y, rc, use_img_background;
  unsigned long bg = 0;

  if (img->mask)
    {
      Free_Pixmap (FRAME_X_DISPLAY (f), img->mask);
      img->mask = NO_PIXMAP;
      img->background_transparent_valid = 0;
    }

#ifndef HAVE_NTGUI
  /* Create an image and pixmap serving as mask.  */
  rc = x_create_x_image_and_pixmap (f, img->width, img->height, 1,
				    &mask_img, &img->mask);
  if (!rc)
    return 0;

  /* Get the X image of IMG->pixmap.  */
  ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap, 0, 0,
		    img->width, img->height,
		    ~0, ZPixmap);
#else
  /* Create the bit array serving as mask.  */
  row_width = (img->width + 7) / 8;
  mask_img = xmalloc (row_width * img->height);
  bzero (mask_img, row_width * img->height);

  /* Create a memory device context for IMG->pixmap.  */
  frame_dc = get_frame_dc (f);
  ximg = CreateCompatibleDC (frame_dc);
  release_frame_dc (f, frame_dc);
  prev = SelectObject (ximg, img->pixmap);
#endif /* HAVE_NTGUI */

  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, use the image's background color. */
  use_img_background = 1;

  if (CONSP (how))
    {
      int rgb[3], i;

      for (i = 0; i < 3 && CONSP (how) && NATNUMP (XCAR (how)); ++i)
	{
	  rgb[i] = XFASTINT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
	  char color_name[30];
	  sprintf (color_name, "#%04x%04x%04x", rgb[0], rgb[1], rgb[2]);
	  bg = (
#ifdef HAVE_NTGUI
		0x00ffffff & /* Filter out palette info.  */
#endif /* HAVE_NTGUI */
		x_alloc_image_color (f, img, build_string (color_name), 0));
	  use_img_background = 0;
	}
    }

  if (use_img_background)
    bg = four_corners_best (ximg, img->corners, img->width, img->height);

  /* Set all bits in mask_img to 1 whose color in ximg is different
     from the background color bg.  */
#ifndef HAVE_NTGUI
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x)
      XPutPixel (mask_img, x, y, (XGetPixel (ximg, x, y) != bg
				  ? PIX_MASK_DRAW : PIX_MASK_RETAIN));

  /* Fill in the background_transparent field while we have the mask handy. */
  image_background_transparent (img, f, mask_img);

  /* Put mask_img into img->mask.  */
  x_put_x_image (f, mask_img, img->mask, img->width, img->height);
  x_destroy_x_image (mask_img);

#else
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x)
      {
	COLORREF p = GetPixel (ximg, x, y);
	if (p != bg)
	  mask_img[y * row_width + x / 8] |= 1 << (x % 8);
      }

  /* Create the mask image.  */
  img->mask = w32_create_pixmap_from_bitmap_data (img->width, img->height,
						  mask_img);
  /* Fill in the background_transparent field while we have the mask handy. */
  SelectObject (ximg, img->mask);
  image_background_transparent (img, f, ximg);

  /* Was: x_destroy_x_image ((XImagePtr )mask_img); which seems bogus ++kfs */
  xfree (mask_img);
#endif /* HAVE_NTGUI */

  Destroy_Image (ximg, prev);

  return 1;
}


/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/

static int pbm_image_p P_ ((Lisp_Object object));
static int pbm_load P_ ((struct frame *f, struct image *img));
static int pbm_scan_number P_ ((unsigned char **, unsigned char *));

/* The symbol `pbm' identifying images of this type.  */

Lisp_Object Qpbm;

/* Indices of image specification fields in gs_format, below.  */

enum pbm_keyword_index
{
  PBM_TYPE,
  PBM_FILE,
  PBM_DATA,
  PBM_ASCENT,
  PBM_MARGIN,
  PBM_RELIEF,
  PBM_ALGORITHM,
  PBM_HEURISTIC_MASK,
  PBM_MASK,
  PBM_FOREGROUND,
  PBM_BACKGROUND,
  PBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword pbm_format[PBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `pbm'.  */

static struct image_type pbm_type =
{
  &Qpbm,
  pbm_image_p,
  pbm_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid PBM image specification.  */

static int
pbm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[PBM_LAST];

  bcopy (pbm_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm))
    return 0;

  /* Must specify either :data or :file.  */
  return fmt[PBM_DATA].count + fmt[PBM_FILE].count == 1;
}


/* Scan a decimal number from *S and return it.  Advance *S while
   reading the number.  END is the end of the string.  Value is -1 at
   end of input.  */

static int
pbm_scan_number (s, end)
     unsigned char **s, *end;
{
  int c = 0, val = -1;

  while (*s < end)
    {
      /* Skip white-space.  */
      while (*s < end && (c = *(*s)++, isspace (c)))
	;

      if (c == '#')
	{
	  /* Skip comment to end of line.  */
	  while (*s < end && (c = *(*s)++, c != '\n'))
	    ;
	}
      else if (isdigit (c))
	{
	  /* Read decimal number.  */
	  val = c - '0';
	  while (*s < end && (c = *(*s)++, isdigit (c)))
	    val = 10 * val + c - '0';
	  break;
	}
      else
	break;
    }

  return val;
}


#ifdef HAVE_NTGUI
#if 0  /* Unused. ++kfs  */

/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  *SIZE is set to the size of the file.  */

static char *
pbm_read_file (file, size)
     Lisp_Object file;
     int *size;
{
  FILE *fp = NULL;
  char *buf = NULL;
  struct stat st;

  if (stat (SDATA (file), &st) == 0
      && (fp = fopen (SDATA (file), "rb")) != NULL
      && (buf = (char *) xmalloc (st.st_size),
	  fread (buf, 1, st.st_size, fp) == st.st_size))
    {
      *size = st.st_size;
      fclose (fp);
    }
  else
    {
      if (fp)
	fclose (fp);
      if (buf)
	{
	  xfree (buf);
	  buf = NULL;
	}
    }

  return buf;
}
#endif
#endif /* HAVE_NTGUI */

/* Load PBM image IMG for use on frame F.  */

static int
pbm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int raw_p, x, y;
  int width, height, max_color_idx = 0;
  XImagePtr ximg;
  Lisp_Object file, specified_file;
  enum {PBM_MONO, PBM_GRAY, PBM_COLOR} type;
  struct gcpro gcpro1;
  unsigned char *contents = NULL;
  unsigned char *end, *p;
  int size;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (STRINGP (specified_file))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (SDATA (file), &size);
      if (contents == NULL)
	{
	  image_error ("Error reading `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      p = contents;
      end = contents + size;
    }
  else
    {
      Lisp_Object data;
      data = image_spec_value (img->spec, QCdata, NULL);
      p = SDATA (data);
      end = p + SBYTES (data);
    }

  /* Check magic number.  */
  if (end - p < 2 || *p++ != 'P')
    {
      image_error ("Not a PBM image: `%s'", img->spec, Qnil);
    error:
      xfree (contents);
      UNGCPRO;
      return 0;
    }

  switch (*p++)
    {
    case '1':
      raw_p = 0, type = PBM_MONO;
      break;

    case '2':
      raw_p = 0, type = PBM_GRAY;
      break;

    case '3':
      raw_p = 0, type = PBM_COLOR;
      break;

    case '4':
      raw_p = 1, type = PBM_MONO;
      break;

    case '5':
      raw_p = 1, type = PBM_GRAY;
      break;

    case '6':
      raw_p = 1, type = PBM_COLOR;
      break;

    default:
      image_error ("Not a PBM image: `%s'", img->spec, Qnil);
      goto error;
    }

  /* Read width, height, maximum color-component.  Characters
     starting with `#' up to the end of a line are ignored.  */
  width = pbm_scan_number (&p, end);
  height = pbm_scan_number (&p, end);

  if (type != PBM_MONO)
    {
      max_color_idx = pbm_scan_number (&p, end);
      if (max_color_idx > 65535 || max_color_idx < 0)
	{
	  image_error ("Unsupported maximum PBM color value", Qnil, Qnil);
	  goto error;
	}
    }

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      goto error;
    }

  if (!x_create_x_image_and_pixmap (f, width, height, 0,
				    &ximg, &img->pixmap))
    goto error;

  /* Initialize the color hash table.  */
  init_color_table ();

  if (type == PBM_MONO)
    {
      int c = 0, g;
      struct image_keyword fmt[PBM_LAST];
      unsigned long fg = FRAME_FOREGROUND_PIXEL (f);
      unsigned long bg = FRAME_BACKGROUND_PIXEL (f);

      /* Parse the image specification.  */
      bcopy (pbm_format, fmt, sizeof fmt);
      parse_image_spec (img->spec, fmt, PBM_LAST, Qpbm);

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[PBM_FOREGROUND].count
	  && STRINGP (fmt[PBM_FOREGROUND].value))
	fg = x_alloc_image_color (f, img, fmt[PBM_FOREGROUND].value, fg);
      if (fmt[PBM_BACKGROUND].count
	  && STRINGP (fmt[PBM_BACKGROUND].value))
	{
	  bg = x_alloc_image_color (f, img, fmt[PBM_BACKGROUND].value, bg);
	  img->background = bg;
	  img->background_valid = 1;
	}

      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    if (raw_p)
	      {
		if ((x & 7) == 0)
		  {
		    if (p >= end)
		      {
			x_destroy_x_image (ximg);
			x_clear_image (f, img);
			image_error ("Invalid image size in image `%s'",
				     img->spec, Qnil);
			goto error;
		      }
		    c = *p++;
		  }
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      g = pbm_scan_number (&p, end);

	    XPutPixel (ximg, x, y, g ? fg : bg);
	  }
    }
  else
    {
      int expected_size = height * width;
      if (max_color_idx > 255)
	expected_size *= 2;
      if (type == PBM_COLOR)
	expected_size *= 3;

      if (raw_p && p + expected_size > end)
	{
	  x_destroy_x_image (ximg);
	  x_clear_image (f, img);
	  image_error ("Invalid image size in image `%s'",
		       img->spec, Qnil);
	  goto error;
	}

      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    int r, g, b;

	    if (type == PBM_GRAY && raw_p)
	      {
		r = g = b = *p++;
		if (max_color_idx > 255)
		  r = g = b = r * 256 + *p++;
	      }
	    else if (type == PBM_GRAY)
	      r = g = b = pbm_scan_number (&p, end);
	    else if (raw_p)
	      {
		r = *p++;
		if (max_color_idx > 255)
		  r = r * 256 + *p++;
		g = *p++;
		if (max_color_idx > 255)
		  g = g * 256 + *p++;
		b = *p++;
		if (max_color_idx > 255)
		  b = b * 256 + *p++;
	      }
	    else
	      {
		r = pbm_scan_number (&p, end);
		g = pbm_scan_number (&p, end);
		b = pbm_scan_number (&p, end);
	      }

	    if (r < 0 || g < 0 || b < 0)
	      {
		x_destroy_x_image (ximg);
		image_error ("Invalid pixel value in image `%s'",
			     img->spec, Qnil);
		goto error;
	      }

	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xffff supported by X.  */
	    r = (double) r * 65535 / max_color_idx;
	    g = (double) g * 65535 / max_color_idx;
	    b = (double) b * 65535 / max_color_idx;
	    XPutPixel (ximg, x, y, lookup_rgb_color (f, r, g, b));
	  }
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Store in IMG->colors the colors allocated for the image, and
     free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy.  */

  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into a pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  /* X and W32 versions did it here, MAC version above.  ++kfs
     img->width = width;
     img->height = height; */

  UNGCPRO;
  xfree (contents);
  return 1;
}


/***********************************************************************
				 PNG
 ***********************************************************************/

#if defined (HAVE_PNG) || defined (MAC_OS)

/* Function prototypes.  */

static int png_image_p P_ ((Lisp_Object object));
static int png_load P_ ((struct frame *f, struct image *img));

/* The symbol `png' identifying images of this type.  */

Lisp_Object Qpng;

/* Indices of image specification fields in png_format, below.  */

enum png_keyword_index
{
  PNG_TYPE,
  PNG_DATA,
  PNG_FILE,
  PNG_ASCENT,
  PNG_MARGIN,
  PNG_RELIEF,
  PNG_ALGORITHM,
  PNG_HEURISTIC_MASK,
  PNG_MASK,
  PNG_BACKGROUND,
  PNG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword png_format[PNG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `png'.  */

static struct image_type png_type =
{
  &Qpng,
  png_image_p,
  png_load,
  x_clear_image,
  NULL
};

/* Return non-zero if OBJECT is a valid PNG image specification.  */

static int
png_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[PNG_LAST];
  bcopy (png_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, PNG_LAST, Qpng))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[PNG_FILE].count + fmt[PNG_DATA].count == 1;
}

#endif /* HAVE_PNG || MAC_OS */


#ifdef HAVE_PNG

#if defined HAVE_LIBPNG_PNG_H
# include <libpng/png.h>
#else
# include <png.h>
#endif

#ifdef HAVE_NTGUI
/* PNG library details.  */

DEF_IMGLIB_FN (png_get_io_ptr);
DEF_IMGLIB_FN (png_check_sig);
DEF_IMGLIB_FN (png_create_read_struct);
DEF_IMGLIB_FN (png_create_info_struct);
DEF_IMGLIB_FN (png_destroy_read_struct);
DEF_IMGLIB_FN (png_set_read_fn);
DEF_IMGLIB_FN (png_set_sig_bytes);
DEF_IMGLIB_FN (png_read_info);
DEF_IMGLIB_FN (png_get_IHDR);
DEF_IMGLIB_FN (png_get_valid);
DEF_IMGLIB_FN (png_set_strip_16);
DEF_IMGLIB_FN (png_set_expand);
DEF_IMGLIB_FN (png_set_gray_to_rgb);
DEF_IMGLIB_FN (png_set_background);
DEF_IMGLIB_FN (png_get_bKGD);
DEF_IMGLIB_FN (png_read_update_info);
DEF_IMGLIB_FN (png_get_channels);
DEF_IMGLIB_FN (png_get_rowbytes);
DEF_IMGLIB_FN (png_read_image);
DEF_IMGLIB_FN (png_read_end);
DEF_IMGLIB_FN (png_error);

static int
init_png_functions (Lisp_Object libraries)
{
  HMODULE library;

  /* Try loading libpng under probable names.  */
  if (!(library = w32_delayed_load (libraries, Qpng)))
    return 0;

  LOAD_IMGLIB_FN (library, png_get_io_ptr);
  LOAD_IMGLIB_FN (library, png_check_sig);
  LOAD_IMGLIB_FN (library, png_create_read_struct);
  LOAD_IMGLIB_FN (library, png_create_info_struct);
  LOAD_IMGLIB_FN (library, png_destroy_read_struct);
  LOAD_IMGLIB_FN (library, png_set_read_fn);
  LOAD_IMGLIB_FN (library, png_set_sig_bytes);
  LOAD_IMGLIB_FN (library, png_read_info);
  LOAD_IMGLIB_FN (library, png_get_IHDR);
  LOAD_IMGLIB_FN (library, png_get_valid);
  LOAD_IMGLIB_FN (library, png_set_strip_16);
  LOAD_IMGLIB_FN (library, png_set_expand);
  LOAD_IMGLIB_FN (library, png_set_gray_to_rgb);
  LOAD_IMGLIB_FN (library, png_set_background);
  LOAD_IMGLIB_FN (library, png_get_bKGD);
  LOAD_IMGLIB_FN (library, png_read_update_info);
  LOAD_IMGLIB_FN (library, png_get_channels);
  LOAD_IMGLIB_FN (library, png_get_rowbytes);
  LOAD_IMGLIB_FN (library, png_read_image);
  LOAD_IMGLIB_FN (library, png_read_end);
  LOAD_IMGLIB_FN (library, png_error);
  return 1;
}
#else

#define fn_png_get_io_ptr		png_get_io_ptr
#define fn_png_check_sig		png_check_sig
#define fn_png_create_read_struct	png_create_read_struct
#define fn_png_create_info_struct	png_create_info_struct
#define fn_png_destroy_read_struct	png_destroy_read_struct
#define fn_png_set_read_fn		png_set_read_fn
#define fn_png_set_sig_bytes		png_set_sig_bytes
#define fn_png_read_info		png_read_info
#define fn_png_get_IHDR			png_get_IHDR
#define fn_png_get_valid		png_get_valid
#define fn_png_set_strip_16		png_set_strip_16
#define fn_png_set_expand		png_set_expand
#define fn_png_set_gray_to_rgb		png_set_gray_to_rgb
#define fn_png_set_background		png_set_background
#define fn_png_get_bKGD			png_get_bKGD
#define fn_png_read_update_info		png_read_update_info
#define fn_png_get_channels		png_get_channels
#define fn_png_get_rowbytes		png_get_rowbytes
#define fn_png_read_image		png_read_image
#define fn_png_read_end			png_read_end
#define fn_png_error			png_error

#endif /* HAVE_NTGUI */

/* Error and warning handlers installed when the PNG library
   is initialized.  */

static void
my_png_error (png_ptr, msg)
     png_struct *png_ptr;
     char *msg;
{
  xassert (png_ptr != NULL);
  image_error ("PNG error: %s", build_string (msg), Qnil);
  longjmp (png_ptr->jmpbuf, 1);
}


static void
my_png_warning (png_ptr, msg)
     png_struct *png_ptr;
     char *msg;
{
  xassert (png_ptr != NULL);
  image_error ("PNG warning: %s", build_string (msg), Qnil);
}

/* Memory source for PNG decoding.  */

struct png_memory_storage
{
  unsigned char *bytes;		/* The data       */
  size_t len;			/* How big is it? */
  int index;			/* Where are we?  */
};


/* Function set as reader function when reading PNG image from memory.
   PNG_PTR is a pointer to the PNG control structure.  Copy LENGTH
   bytes from the input to DATA.  */

static void
png_read_from_memory (png_ptr, data, length)
     png_structp png_ptr;
     png_bytep data;
     png_size_t length;
{
  struct png_memory_storage *tbr
    = (struct png_memory_storage *) fn_png_get_io_ptr (png_ptr);

  if (length > tbr->len - tbr->index)
    fn_png_error (png_ptr, "Read error");

  bcopy (tbr->bytes + tbr->index, data, length);
  tbr->index = tbr->index + length;
}


/* Function set as reader function when reading PNG image from a file.
   PNG_PTR is a pointer to the PNG control structure.  Copy LENGTH
   bytes from the input to DATA.  */

static void
png_read_from_file (png_ptr, data, length)
     png_structp png_ptr;
     png_bytep data;
     png_size_t length;
{
  FILE *fp = (FILE *) fn_png_get_io_ptr (png_ptr);

  if (fread (data, 1, length, fp) < length)
    fn_png_error (png_ptr, "Read error");
}


/* Load PNG image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
png_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  int x, y, i;
  XImagePtr ximg, mask_img = NULL;
  struct gcpro gcpro1;
  png_struct *png_ptr = NULL;
  png_info *info_ptr = NULL, *end_info = NULL;
  FILE *volatile fp = NULL;
  png_byte sig[8];
  png_byte * volatile pixels = NULL;
  png_byte ** volatile rows = NULL;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  png_byte channels;
  png_uint_32 row_bytes;
  int transparent_p;
  double screen_gamma;
  struct png_memory_storage tbr;  /* Data to be read */

  /* Find out what file to load.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Open the image file.  */
      fp = fopen (SDATA (file), "rb");
      if (!fp)
	{
	  image_error ("Cannot open image file `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Check PNG signature.  */
      if (fread (sig, 1, sizeof sig, fp) != sizeof sig
	  || !fn_png_check_sig (sig, sizeof sig))
	{
	  image_error ("Not a PNG file: `%s'", file, Qnil);
	  UNGCPRO;
	  fclose (fp);
	  return 0;
	}
    }
  else
    {
      /* Read from memory.  */
      tbr.bytes = SDATA (specified_data);
      tbr.len = SBYTES (specified_data);
      tbr.index = 0;

      /* Check PNG signature.  */
      if (tbr.len < sizeof sig
	  || !fn_png_check_sig (tbr.bytes, sizeof sig))
	{
	  image_error ("Not a PNG image: `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Need to skip past the signature.  */
      tbr.bytes += sizeof (sig);
    }

  /* Initialize read and info structs for PNG lib.  Casting return
     value avoids a GCC warning on W32.  */
  png_ptr = (png_structp)fn_png_create_read_struct (PNG_LIBPNG_VER_STRING,
						    NULL, my_png_error,
						    my_png_warning);
  if (!png_ptr)
    {
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Casting return value avoids a GCC warning on W32.  */
  info_ptr = (png_infop)fn_png_create_info_struct (png_ptr);
  if (!info_ptr)
    {
      fn_png_destroy_read_struct (&png_ptr, NULL, NULL);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Casting return value avoids a GCC warning on W32.  */
  end_info = (png_infop)fn_png_create_info_struct (png_ptr);
  if (!end_info)
    {
      fn_png_destroy_read_struct (&png_ptr, &info_ptr, NULL);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Set error jump-back.  We come back here when the PNG library
     detects an error.  */
  if (setjmp (png_ptr->jmpbuf))
    {
    error:
      if (png_ptr)
        fn_png_destroy_read_struct (&png_ptr, &info_ptr, &end_info);
      xfree (pixels);
      xfree (rows);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Read image info.  */
  if (!NILP (specified_data))
    fn_png_set_read_fn (png_ptr, (void *) &tbr, png_read_from_memory);
  else
    fn_png_set_read_fn (png_ptr, (void *) fp, png_read_from_file);

  fn_png_set_sig_bytes (png_ptr, sizeof sig);
  fn_png_read_info (png_ptr, info_ptr);
  fn_png_get_IHDR (png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
		   &interlace_type, NULL, NULL);

  if (!check_image_size (f, width, height))
    goto error;

  /* If image contains simply transparency data, we prefer to
     construct a clipping mask.  */
  if (fn_png_get_valid (png_ptr, info_ptr, PNG_INFO_tRNS))
    transparent_p = 1;
  else
    transparent_p = 0;

  /* This function is easier to write if we only have to handle
     one data format: RGB or RGBA with 8 bits per channel.  Let's
     transform other formats into that format.  */

  /* Strip more than 8 bits per channel.  */
  if (bit_depth == 16)
    fn_png_set_strip_16 (png_ptr);

  /* Expand data to 24 bit RGB, or 8 bit grayscale, with alpha channel
     if available.  */
  fn_png_set_expand (png_ptr);

  /* Convert grayscale images to RGB.  */
  if (color_type == PNG_COLOR_TYPE_GRAY
      || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    fn_png_set_gray_to_rgb (png_ptr);

  screen_gamma = (f->gamma ? 1 / f->gamma / 0.45455 : 2.2);

#if 0 /* Avoid double gamma correction for PNG images. */
  { /* Tell the PNG lib to handle gamma correction for us.  */
    int intent;
    double image_gamma;
#if defined(PNG_READ_sRGB_SUPPORTED) || defined(PNG_WRITE_sRGB_SUPPORTED)
    if (png_get_sRGB (png_ptr, info_ptr, &intent))
      /* The libpng documentation says this is right in this case.  */
      png_set_gamma (png_ptr, screen_gamma, 0.45455);
    else
#endif
      if (png_get_gAMA (png_ptr, info_ptr, &image_gamma))
	/* Image contains gamma information.  */
	png_set_gamma (png_ptr, screen_gamma, image_gamma);
      else
	/* Use the standard default for the image gamma.  */
	png_set_gamma (png_ptr, screen_gamma, 0.45455);
  }
#endif /* if 0 */

  /* Handle alpha channel by combining the image with a background
     color.  Do this only if a real alpha channel is supplied.  For
     simple transparency, we prefer a clipping mask.  */
  if (!transparent_p)
    {
      /* png_color_16 *image_bg; */
      Lisp_Object specified_bg
	= image_spec_value (img->spec, QCbackground, NULL);

      if (STRINGP (specified_bg))
	/* The user specified `:background', use that.  */
	{
	  /* W32 version incorrectly used COLORREF here!!  ++kfs */
	  XColor color;
	  if (x_defined_color (f, SDATA (specified_bg), &color, 0))
	    {
	      png_color_16 user_bg;

	      bzero (&user_bg, sizeof user_bg);
	      user_bg.red = color.red >> 8;
	      user_bg.green = color.green >> 8;
	      user_bg.blue = color.blue >> 8;

	      fn_png_set_background (png_ptr, &user_bg,
				     PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
	    }
	}
      /* The commented-out code checked if the png specifies a default
	 background color, and uses that.  Since we use the current
	 frame background, it is OK for us to ignore this.

      else if (fn_png_get_bKGD (png_ptr, info_ptr, &image_bg))
	fn_png_set_background (png_ptr, image_bg,
			       PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
	*/
      else
	{
	  /* Image does not contain a background color with which
	     to combine the image data via an alpha channel.  Use
	     the frame's background instead.  */
#ifdef HAVE_X_WINDOWS
	  XColor color;
	  png_color_16 frame_background;

	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  x_query_color (f, &color);

	  bzero (&frame_background, sizeof frame_background);
	  frame_background.red = color.red >> 8;
	  frame_background.green = color.green >> 8;
	  frame_background.blue = color.blue >> 8;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
	  COLORREF color;
	  png_color_16 frame_background;
	  color = FRAME_BACKGROUND_PIXEL (f);
#if 0 /* W32 TODO : Colormap support.  */
	  x_query_color (f, &color);
#endif
	  bzero (&frame_background, sizeof frame_background);
	  frame_background.red = GetRValue (color);
	  frame_background.green = GetGValue (color);
	  frame_background.blue = GetBValue (color);
#endif /* HAVE_NTGUI */

#ifdef MAC_OS
	  unsigned long color;
	  png_color_16 frame_background;
	  color = FRAME_BACKGROUND_PIXEL (f);
#if 0 /* MAC/W32 TODO : Colormap support.  */
	  x_query_color (f, &color);
#endif
	  bzero (&frame_background, sizeof frame_background);
	  frame_background.red = RED_FROM_ULONG (color);
	  frame_background.green = GREEN_FROM_ULONG (color);
	  frame_background.blue = BLUE_FROM_ULONG (color);
#endif /* MAC_OS */

	  fn_png_set_background (png_ptr, &frame_background,
				 PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
	}
    }

  /* Update info structure.  */
  fn_png_read_update_info (png_ptr, info_ptr);

  /* Get number of channels.  Valid values are 1 for grayscale images
     and images with a palette, 2 for grayscale images with transparency
     information (alpha channel), 3 for RGB images, and 4 for RGB
     images with alpha channel, i.e. RGBA.  If conversions above were
     sufficient we should only have 3 or 4 channels here.  */
  channels = fn_png_get_channels (png_ptr, info_ptr);
  xassert (channels == 3 || channels == 4);

  /* Number of bytes needed for one row of the image.  */
  row_bytes = fn_png_get_rowbytes (png_ptr, info_ptr);

  /* Allocate memory for the image.  */
  pixels = (png_byte *) xmalloc (row_bytes * height * sizeof *pixels);
  rows = (png_byte **) xmalloc (height * sizeof *rows);
  for (i = 0; i < height; ++i)
    rows[i] = pixels + i * row_bytes;

  /* Read the entire image.  */
  fn_png_read_image (png_ptr, rows);
  fn_png_read_end (png_ptr, info_ptr);
  if (fp)
    {
      fclose (fp);
      fp = NULL;
    }

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg,
				    &img->pixmap))
    goto error;

  /* Create an image and pixmap serving as mask if the PNG image
     contains an alpha channel.  */
  if (channels == 4
      && !transparent_p
      && !x_create_x_image_and_pixmap (f, width, height, 1,
				       &mask_img, &img->mask))
    {
      x_destroy_x_image (ximg);
      Free_Pixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = NO_PIXMAP;
      goto error;
    }

  /* Fill the X image and mask from PNG data.  */
  init_color_table ();

  for (y = 0; y < height; ++y)
    {
      png_byte *p = rows[y];

      for (x = 0; x < width; ++x)
	{
	  unsigned r, g, b;

	  r = *p++ << 8;
	  g = *p++ << 8;
	  b = *p++ << 8;
	  XPutPixel (ximg, x, y, lookup_rgb_color (f, r, g, b));
	  /* An alpha channel, aka mask channel, associates variable
	     transparency with an image.  Where other image formats
	     support binary transparency---fully transparent or fully
	     opaque---PNG allows up to 254 levels of partial transparency.
	     The PNG library implements partial transparency by combining
	     the image with a specified background color.

	     I'm not sure how to handle this here nicely: because the
	     background on which the image is displayed may change, for
	     real alpha channel support, it would be necessary to create
	     a new image for each possible background.

	     What I'm doing now is that a mask is created if we have
	     boolean transparency information.  Otherwise I'm using
	     the frame's background color to combine the image with.  */

	  if (channels == 4)
	    {
	      if (mask_img)
		XPutPixel (mask_img, x, y, *p > 0 ? PIX_MASK_DRAW : PIX_MASK_RETAIN);
	      ++p;
	    }
	}
    }

  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Set IMG's background color from the PNG image, unless the user
       overrode it.  */
    {
      png_color_16 *bg;
      if (fn_png_get_bKGD (png_ptr, info_ptr, &bg))
	{
	  img->background = lookup_rgb_color (f, bg->red, bg->green, bg->blue);
	  img->background_valid = 1;
	}
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  /* Clean up.  */
  fn_png_destroy_read_struct (&png_ptr, &info_ptr, &end_info);
  xfree (rows);
  xfree (pixels);

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy.
     Casting avoids a GCC warning.  */
  IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  /* Same for the mask.  */
  if (mask_img)
    {
      /* Fill in the background_transparent field while we have the
	 mask handy.  Casting avoids a GCC warning.  */
      image_background_transparent (img, f, (XImagePtr_or_DC)mask_img);

      x_put_x_image (f, mask_img, img->mask, img->width, img->height);
      x_destroy_x_image (mask_img);
    }

  UNGCPRO;
  return 1;
}

#else /* HAVE_PNG */

#ifdef MAC_OS
static int
png_load (f, img)
     struct frame *f;
     struct image *img;
{
#ifdef MAC_OSX
  if (MyCGImageCreateWithPNGDataProvider)
    return image_load_quartz2d (f, img, 1);
  else
#endif
    return image_load_quicktime (f, img, kQTFileTypePNG);
}
#endif  /* MAC_OS */

#endif /* !HAVE_PNG */



/***********************************************************************
				 JPEG
 ***********************************************************************/

#if defined (HAVE_JPEG) || defined (MAC_OS)

static int jpeg_image_p P_ ((Lisp_Object object));
static int jpeg_load P_ ((struct frame *f, struct image *img));

/* The symbol `jpeg' identifying images of this type.  */

Lisp_Object Qjpeg;

/* Indices of image specification fields in gs_format, below.  */

enum jpeg_keyword_index
{
  JPEG_TYPE,
  JPEG_DATA,
  JPEG_FILE,
  JPEG_ASCENT,
  JPEG_MARGIN,
  JPEG_RELIEF,
  JPEG_ALGORITHM,
  JPEG_HEURISTIC_MASK,
  JPEG_MASK,
  JPEG_BACKGROUND,
  JPEG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword jpeg_format[JPEG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `jpeg'.  */

static struct image_type jpeg_type =
{
  &Qjpeg,
  jpeg_image_p,
  jpeg_load,
  x_clear_image,
  NULL
};

/* Return non-zero if OBJECT is a valid JPEG image specification.  */

static int
jpeg_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[JPEG_LAST];

  bcopy (jpeg_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[JPEG_FILE].count + fmt[JPEG_DATA].count == 1;
}

#endif /* HAVE_JPEG || MAC_OS */

#ifdef HAVE_JPEG

/* Work around a warning about HAVE_STDLIB_H being redefined in
   jconfig.h.  */
#ifdef HAVE_STDLIB_H
#define HAVE_STDLIB_H_1
#undef HAVE_STDLIB_H
#endif /* HAVE_STLIB_H */

#if defined (HAVE_NTGUI) && !defined (__WIN32__)
/* In older releases of the jpeg library, jpeglib.h will define boolean
   differently depending on __WIN32__, so make sure it is defined.  */
#define __WIN32__ 1
#endif

#include <jpeglib.h>
#include <jerror.h>
#include <setjmp.h>

#ifdef HAVE_STLIB_H_1
#define HAVE_STDLIB_H 1
#endif

#ifdef HAVE_NTGUI

/* JPEG library details.  */
DEF_IMGLIB_FN (jpeg_CreateDecompress);
DEF_IMGLIB_FN (jpeg_start_decompress);
DEF_IMGLIB_FN (jpeg_finish_decompress);
DEF_IMGLIB_FN (jpeg_destroy_decompress);
DEF_IMGLIB_FN (jpeg_read_header);
DEF_IMGLIB_FN (jpeg_read_scanlines);
DEF_IMGLIB_FN (jpeg_std_error);
DEF_IMGLIB_FN (jpeg_resync_to_restart);

static int
init_jpeg_functions (Lisp_Object libraries)
{
  HMODULE library;

  if (!(library = w32_delayed_load (libraries, Qjpeg)))
    return 0;

  LOAD_IMGLIB_FN (library, jpeg_finish_decompress);
  LOAD_IMGLIB_FN (library, jpeg_read_scanlines);
  LOAD_IMGLIB_FN (library, jpeg_start_decompress);
  LOAD_IMGLIB_FN (library, jpeg_read_header);
  LOAD_IMGLIB_FN (library, jpeg_CreateDecompress);
  LOAD_IMGLIB_FN (library, jpeg_destroy_decompress);
  LOAD_IMGLIB_FN (library, jpeg_std_error);
  LOAD_IMGLIB_FN (library, jpeg_resync_to_restart);
  return 1;
}

/* Wrapper since we can't directly assign the function pointer
   to another function pointer that was declared more completely easily.  */
static boolean
jpeg_resync_to_restart_wrapper (cinfo, desired)
     j_decompress_ptr cinfo;
     int desired;
{
  return fn_jpeg_resync_to_restart (cinfo, desired);
}

#else

#define fn_jpeg_CreateDecompress(a,b,c)	jpeg_create_decompress(a)
#define fn_jpeg_start_decompress	jpeg_start_decompress
#define fn_jpeg_finish_decompress	jpeg_finish_decompress
#define fn_jpeg_destroy_decompress	jpeg_destroy_decompress
#define fn_jpeg_read_header		jpeg_read_header
#define fn_jpeg_read_scanlines		jpeg_read_scanlines
#define fn_jpeg_std_error		jpeg_std_error
#define jpeg_resync_to_restart_wrapper	jpeg_resync_to_restart

#endif /* HAVE_NTGUI */

struct my_jpeg_error_mgr
{
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
};


static void
my_error_exit (cinfo)
     j_common_ptr cinfo;
{
  struct my_jpeg_error_mgr *mgr = (struct my_jpeg_error_mgr *) cinfo->err;
  longjmp (mgr->setjmp_buffer, 1);
}


/* Init source method for JPEG data source manager.  Called by
   jpeg_read_header() before any data is actually read.  See
   libjpeg.doc from the JPEG lib distribution.  */

static void
our_common_init_source (cinfo)
     j_decompress_ptr cinfo;
{
}


/* Method to terminate data source.  Called by
   jpeg_finish_decompress() after all data has been processed.  */

static void
our_common_term_source (cinfo)
     j_decompress_ptr cinfo;
{
}


/* Fill input buffer method for JPEG data source manager.  Called
   whenever more data is needed.  We read the whole image in one step,
   so this only adds a fake end of input marker at the end.  */

static JOCTET our_memory_buffer[2];

static boolean
our_memory_fill_input_buffer (cinfo)
     j_decompress_ptr cinfo;
{
  /* Insert a fake EOI marker.  */
  struct jpeg_source_mgr *src = cinfo->src;

  our_memory_buffer[0] = (JOCTET) 0xFF;
  our_memory_buffer[1] = (JOCTET) JPEG_EOI;

  src->next_input_byte = our_memory_buffer;
  src->bytes_in_buffer = 2;
  return 1;
}


/* Method to skip over NUM_BYTES bytes in the image data.  CINFO->src
   is the JPEG data source manager.  */

static void
our_memory_skip_input_data (cinfo, num_bytes)
     j_decompress_ptr cinfo;
     long num_bytes;
{
  struct jpeg_source_mgr *src = (struct jpeg_source_mgr *) cinfo->src;

  if (src)
    {
      if (num_bytes > src->bytes_in_buffer)
	ERREXIT (cinfo, JERR_INPUT_EOF);

      src->bytes_in_buffer -= num_bytes;
      src->next_input_byte += num_bytes;
    }
}


/* Set up the JPEG lib for reading an image from DATA which contains
   LEN bytes.  CINFO is the decompression info structure created for
   reading the image.  */

static void
jpeg_memory_src (cinfo, data, len)
     j_decompress_ptr cinfo;
     JOCTET *data;
     unsigned int len;
{
  struct jpeg_source_mgr *src;

  if (cinfo->src == NULL)
    {
      /* First time for this JPEG object?  */
      cinfo->src = (struct jpeg_source_mgr *)
	(*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				    sizeof (struct jpeg_source_mgr));
      src = (struct jpeg_source_mgr *) cinfo->src;
      src->next_input_byte = data;
    }

  src = (struct jpeg_source_mgr *) cinfo->src;
  src->init_source = our_common_init_source;
  src->fill_input_buffer = our_memory_fill_input_buffer;
  src->skip_input_data = our_memory_skip_input_data;
  src->resync_to_restart = jpeg_resync_to_restart_wrapper; /* Use default method.  */
  src->term_source = our_common_term_source;
  src->bytes_in_buffer = len;
  src->next_input_byte = data;
}


struct jpeg_stdio_mgr
{
  struct jpeg_source_mgr mgr;
  boolean finished;
  FILE *file;
  JOCTET *buffer;
};


/* Size of buffer to read JPEG from file.
   Not too big, as we want to use alloc_small.  */
#define JPEG_STDIO_BUFFER_SIZE 8192


/* Fill input buffer method for JPEG data source manager.  Called
   whenever more data is needed.  The data is read from a FILE *.  */

static boolean
our_stdio_fill_input_buffer (cinfo)
     j_decompress_ptr cinfo;
{
  struct jpeg_stdio_mgr *src;

  src = (struct jpeg_stdio_mgr *) cinfo->src;
  if (!src->finished)
    {
      size_t bytes;

      bytes = fread (src->buffer, 1, JPEG_STDIO_BUFFER_SIZE, src->file);
      if (bytes > 0)
        src->mgr.bytes_in_buffer = bytes;
      else
        {
          WARNMS (cinfo, JWRN_JPEG_EOF);
          src->finished = 1;
          src->buffer[0] = (JOCTET) 0xFF;
          src->buffer[1] = (JOCTET) JPEG_EOI;
          src->mgr.bytes_in_buffer = 2;
        }
      src->mgr.next_input_byte = src->buffer;
    }

  return 1;
}


/* Method to skip over NUM_BYTES bytes in the image data.  CINFO->src
   is the JPEG data source manager.  */

static void
our_stdio_skip_input_data (cinfo, num_bytes)
     j_decompress_ptr cinfo;
     long num_bytes;
{
  struct jpeg_stdio_mgr *src;
  src = (struct jpeg_stdio_mgr *) cinfo->src;

  while (num_bytes > 0 && !src->finished)
    {
      if (num_bytes <= src->mgr.bytes_in_buffer)
        {
          src->mgr.bytes_in_buffer -= num_bytes;
          src->mgr.next_input_byte += num_bytes;
          break;
        }
      else
        {
          num_bytes -= src->mgr.bytes_in_buffer;
          src->mgr.bytes_in_buffer = 0;
          src->mgr.next_input_byte = NULL;

          our_stdio_fill_input_buffer (cinfo);
        }
    }
}


/* Set up the JPEG lib for reading an image from a FILE *.
   CINFO is the decompression info structure created for
   reading the image.  */

static void
jpeg_file_src (cinfo, fp)
     j_decompress_ptr cinfo;
     FILE *fp;
{
  struct jpeg_stdio_mgr *src;

  if (cinfo->src != NULL)
      src = (struct jpeg_stdio_mgr *) cinfo->src;
  else
    {
      /* First time for this JPEG object?  */
      cinfo->src = (struct jpeg_source_mgr *)
        (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
                                    sizeof (struct jpeg_stdio_mgr));
      src = (struct jpeg_stdio_mgr *) cinfo->src;
      src->buffer = (JOCTET *)
          (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
                                      JPEG_STDIO_BUFFER_SIZE);
    }

  src->file = fp;
  src->finished = 0;
  src->mgr.init_source = our_common_init_source;
  src->mgr.fill_input_buffer = our_stdio_fill_input_buffer;
  src->mgr.skip_input_data = our_stdio_skip_input_data;
  src->mgr.resync_to_restart = jpeg_resync_to_restart_wrapper; /* Use default method.  */
  src->mgr.term_source = our_common_term_source;
  src->mgr.bytes_in_buffer = 0;
  src->mgr.next_input_byte = NULL;
}


/* Load image IMG for use on frame F.  Patterned after example.c
   from the JPEG lib.  */

static int
jpeg_load (f, img)
     struct frame *f;
     struct image *img;
{
  struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr mgr;
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  FILE * volatile fp = NULL;
  JSAMPARRAY buffer;
  int row_stride, x, y;
  XImagePtr ximg = NULL;
  int rc;
  unsigned long *colors;
  int width, height;
  struct gcpro gcpro1;

  /* Open the JPEG file.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      fp = fopen (SDATA (file), "rb");
      if (fp == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Customize libjpeg's error handling to call my_error_exit when an
     error is detected.  This function will perform a longjmp.
     Casting return value avoids a GCC warning on W32.  */
  cinfo.err = (struct jpeg_error_mgr *)fn_jpeg_std_error (&mgr.pub);
  mgr.pub.error_exit = my_error_exit;

  if ((rc = setjmp (mgr.setjmp_buffer)) != 0)
    {
      if (rc == 1)
	{
	  /* Called from my_error_exit.  Display a JPEG error.  */
	  char buffer[JMSG_LENGTH_MAX];
	  cinfo.err->format_message ((j_common_ptr) &cinfo, buffer);
	  image_error ("Error reading JPEG image `%s': %s", img->spec,
		       build_string (buffer));
	}

      /* Close the input file and destroy the JPEG object.  */
      if (fp)
	fclose ((FILE *) fp);
      fn_jpeg_destroy_decompress (&cinfo);

      /* If we already have an XImage, free that.  */
      x_destroy_x_image (ximg);

      /* Free pixmap and colors.  */
      x_clear_image (f, img);

      UNGCPRO;
      return 0;
    }

  /* Create the JPEG decompression object.  Let it read from fp.
	 Read the JPEG image header.  */
  fn_jpeg_CreateDecompress (&cinfo, JPEG_LIB_VERSION, sizeof (cinfo));

  if (NILP (specified_data))
    jpeg_file_src (&cinfo, (FILE *) fp);
  else
    jpeg_memory_src (&cinfo, SDATA (specified_data),
		     SBYTES (specified_data));

  fn_jpeg_read_header (&cinfo, 1);

  /* Customize decompression so that color quantization will be used.
	 Start decompression.  */
  cinfo.quantize_colors = 1;
  fn_jpeg_start_decompress (&cinfo);
  width = img->width = cinfo.output_width;
  height = img->height = cinfo.output_height;

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      longjmp (mgr.setjmp_buffer, 2);
    }

  /* Create X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    longjmp (mgr.setjmp_buffer, 2);

  /* Allocate colors.  When color quantization is used,
     cinfo.actual_number_of_colors has been set with the number of
     colors generated, and cinfo.colormap is a two-dimensional array
     of color indices in the range 0..cinfo.actual_number_of_colors.
     No more than 255 colors will be generated.  */
  {
    int i, ir, ig, ib;

    if (cinfo.out_color_components > 2)
      ir = 0, ig = 1, ib = 2;
    else if (cinfo.out_color_components > 1)
      ir = 0, ig = 1, ib = 0;
    else
      ir = 0, ig = 0, ib = 0;

    /* Use the color table mechanism because it handles colors that
       cannot be allocated nicely.  Such colors will be replaced with
       a default color, and we don't have to care about which colors
       can be freed safely, and which can't.  */
    init_color_table ();
    colors = (unsigned long *) alloca (cinfo.actual_number_of_colors
				       * sizeof *colors);

    for (i = 0; i < cinfo.actual_number_of_colors; ++i)
      {
	/* Multiply RGB values with 255 because X expects RGB values
	   in the range 0..0xffff.  */
	int r = cinfo.colormap[ir][i] << 8;
	int g = cinfo.colormap[ig][i] << 8;
	int b = cinfo.colormap[ib][i] << 8;
	colors[i] = lookup_rgb_color (f, r, g, b);
      }

#ifdef COLOR_TABLE_SUPPORT
    /* Remember those colors actually allocated.  */
    img->colors = colors_in_color_table (&img->ncolors);
    free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */
  }

  /* Read pixels.  */
  row_stride = width * cinfo.output_components;
  buffer = cinfo.mem->alloc_sarray ((j_common_ptr) &cinfo, JPOOL_IMAGE,
				    row_stride, 1);
  for (y = 0; y < height; ++y)
    {
      fn_jpeg_read_scanlines (&cinfo, buffer, 1);
      for (x = 0; x < cinfo.output_width; ++x)
	XPutPixel (ximg, x, y, colors[buffer[0][x]]);
    }

  /* Clean up.  */
  fn_jpeg_finish_decompress (&cinfo);
  fn_jpeg_destroy_decompress (&cinfo);
  if (fp)
    fclose ((FILE *) fp);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNGCPRO;
  return 1;
}

#else /* HAVE_JPEG */

#ifdef MAC_OS
static int
jpeg_load (f, img)
     struct frame *f;
     struct image *img;
{
#ifdef MAC_OSX
  return image_load_quartz2d (f, img, 0);
#else
  return image_load_quicktime (f, img, kQTFileTypeJPEG);
#endif
}
#endif  /* MAC_OS */

#endif /* !HAVE_JPEG */



/***********************************************************************
				 TIFF
 ***********************************************************************/

#if defined (HAVE_TIFF) || defined (MAC_OS)

static int tiff_image_p P_ ((Lisp_Object object));
static int tiff_load P_ ((struct frame *f, struct image *img));

/* The symbol `tiff' identifying images of this type.  */

Lisp_Object Qtiff;

/* Indices of image specification fields in tiff_format, below.  */

enum tiff_keyword_index
{
  TIFF_TYPE,
  TIFF_DATA,
  TIFF_FILE,
  TIFF_ASCENT,
  TIFF_MARGIN,
  TIFF_RELIEF,
  TIFF_ALGORITHM,
  TIFF_HEURISTIC_MASK,
  TIFF_MASK,
  TIFF_BACKGROUND,
  TIFF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword tiff_format[TIFF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `tiff'.  */

static struct image_type tiff_type =
{
  &Qtiff,
  tiff_image_p,
  tiff_load,
  x_clear_image,
  NULL
};

/* Return non-zero if OBJECT is a valid TIFF image specification.  */

static int
tiff_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[TIFF_LAST];
  bcopy (tiff_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[TIFF_FILE].count + fmt[TIFF_DATA].count == 1;
}

#endif /* HAVE_TIFF || MAC_OS */

#ifdef HAVE_TIFF

#include <tiffio.h>

#ifdef HAVE_NTGUI

/* TIFF library details.  */
DEF_IMGLIB_FN (TIFFSetErrorHandler);
DEF_IMGLIB_FN (TIFFSetWarningHandler);
DEF_IMGLIB_FN (TIFFOpen);
DEF_IMGLIB_FN (TIFFClientOpen);
DEF_IMGLIB_FN (TIFFGetField);
DEF_IMGLIB_FN (TIFFReadRGBAImage);
DEF_IMGLIB_FN (TIFFClose);

static int
init_tiff_functions (Lisp_Object libraries)
{
  HMODULE library;

  if (!(library = w32_delayed_load (libraries, Qtiff)))
    return 0;

  LOAD_IMGLIB_FN (library, TIFFSetErrorHandler);
  LOAD_IMGLIB_FN (library, TIFFSetWarningHandler);
  LOAD_IMGLIB_FN (library, TIFFOpen);
  LOAD_IMGLIB_FN (library, TIFFClientOpen);
  LOAD_IMGLIB_FN (library, TIFFGetField);
  LOAD_IMGLIB_FN (library, TIFFReadRGBAImage);
  LOAD_IMGLIB_FN (library, TIFFClose);
  return 1;
}

#else

#define fn_TIFFSetErrorHandler		TIFFSetErrorHandler
#define fn_TIFFSetWarningHandler	TIFFSetWarningHandler
#define fn_TIFFOpen			TIFFOpen
#define fn_TIFFClientOpen		TIFFClientOpen
#define fn_TIFFGetField			TIFFGetField
#define fn_TIFFReadRGBAImage		TIFFReadRGBAImage
#define fn_TIFFClose			TIFFClose

#endif /* HAVE_NTGUI */


/* Reading from a memory buffer for TIFF images Based on the PNG
   memory source, but we have to provide a lot of extra functions.
   Blah.

   We really only need to implement read and seek, but I am not
   convinced that the TIFF library is smart enough not to destroy
   itself if we only hand it the function pointers we need to
   override.  */

typedef struct
{
  unsigned char *bytes;
  size_t len;
  int index;
}
tiff_memory_source;

static size_t
tiff_read_from_memory (data, buf, size)
     thandle_t data;
     tdata_t buf;
     tsize_t size;
{
  tiff_memory_source *src = (tiff_memory_source *) data;

  if (size > src->len - src->index)
    return (size_t) -1;
  bcopy (src->bytes + src->index, buf, size);
  src->index += size;
  return size;
}

static size_t
tiff_write_from_memory (data, buf, size)
     thandle_t data;
     tdata_t buf;
     tsize_t size;
{
  return (size_t) -1;
}

static toff_t
tiff_seek_in_memory (data, off, whence)
     thandle_t data;
     toff_t off;
     int whence;
{
  tiff_memory_source *src = (tiff_memory_source *) data;
  int idx;

  switch (whence)
    {
    case SEEK_SET:		/* Go from beginning of source.  */
      idx = off;
      break;

    case SEEK_END:		/* Go from end of source.  */
      idx = src->len + off;
      break;

    case SEEK_CUR:		/* Go from current position.  */
      idx = src->index + off;
      break;

    default:			/* Invalid `whence'.   */
      return -1;
    }

  if (idx > src->len || idx < 0)
    return -1;

  src->index = idx;
  return src->index;
}

static int
tiff_close_memory (data)
     thandle_t data;
{
  /* NOOP */
  return 0;
}

static int
tiff_mmap_memory (data, pbase, psize)
     thandle_t data;
     tdata_t *pbase;
     toff_t *psize;
{
  /* It is already _IN_ memory. */
  return 0;
}

static void
tiff_unmap_memory (data, base, size)
     thandle_t data;
     tdata_t base;
     toff_t size;
{
  /* We don't need to do this. */
}

static toff_t
tiff_size_of_memory (data)
     thandle_t data;
{
  return ((tiff_memory_source *) data)->len;
}


static void
tiff_error_handler (title, format, ap)
     const char *title, *format;
     va_list ap;
{
  char buf[512];
  int len;

  len = sprintf (buf, "TIFF error: %s ", title);
  vsprintf (buf + len, format, ap);
  add_to_log (buf, Qnil, Qnil);
}


static void
tiff_warning_handler (title, format, ap)
     const char *title, *format;
     va_list ap;
{
  char buf[512];
  int len;

  len = sprintf (buf, "TIFF warning: %s ", title);
  vsprintf (buf + len, format, ap);
  add_to_log (buf, Qnil, Qnil);
}


/* Load TIFF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
tiff_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  TIFF *tiff;
  int width, height, x, y;
  uint32 *buf;
  int rc;
  XImagePtr ximg;
  struct gcpro gcpro1;
  tiff_memory_source memsrc;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  fn_TIFFSetErrorHandler (tiff_error_handler);
  fn_TIFFSetWarningHandler (tiff_warning_handler);

  if (NILP (specified_data))
    {
      /* Read from a file */
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Try to open the image file.  Casting return value avoids a
	 GCC warning on W32.  */
      tiff = (TIFF *)fn_TIFFOpen (SDATA (file), "r");
      if (tiff == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }
  else
    {
      /* Memory source! */
      memsrc.bytes = SDATA (specified_data);
      memsrc.len = SBYTES (specified_data);
      memsrc.index = 0;

      /* Casting return value avoids a GCC warning on W32.  */
      tiff = (TIFF *)fn_TIFFClientOpen ("memory_source", "r", &memsrc,
					(TIFFReadWriteProc) tiff_read_from_memory,
					(TIFFReadWriteProc) tiff_write_from_memory,
					tiff_seek_in_memory,
					tiff_close_memory,
					tiff_size_of_memory,
					tiff_mmap_memory,
					tiff_unmap_memory);

      if (!tiff)
	{
	  image_error ("Cannot open memory source for `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Get width and height of the image, and allocate a raster buffer
     of width x height 32-bit values.  */
  fn_TIFFGetField (tiff, TIFFTAG_IMAGEWIDTH, &width);
  fn_TIFFGetField (tiff, TIFFTAG_IMAGELENGTH, &height);

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      UNGCPRO;
      return 0;
    }

  buf = (uint32 *) xmalloc (width * height * sizeof *buf);

  rc = fn_TIFFReadRGBAImage (tiff, width, height, buf, 0);
  fn_TIFFClose (tiff);
  if (!rc)
    {
      image_error ("Error reading TIFF image `%s'", img->spec, Qnil);
      xfree (buf);
      UNGCPRO;
      return 0;
    }

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      xfree (buf);
      UNGCPRO;
      return 0;
    }

  /* Initialize the color table.  */
  init_color_table ();

  /* Process the pixel raster.  Origin is in the lower-left corner.  */
  for (y = 0; y < height; ++y)
    {
      uint32 *row = buf + y * width;

      for (x = 0; x < width; ++x)
	{
	  uint32 abgr = row[x];
	  int r = TIFFGetR (abgr) << 8;
	  int g = TIFFGetG (abgr) << 8;
	  int b = TIFFGetB (abgr) << 8;
	  XPutPixel (ximg, x, height - 1 - y, lookup_rgb_color (f, r, g, b));
	}
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Remember the colors allocated for the image.  Free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning on W32.  */
    IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  xfree (buf);

  UNGCPRO;
  return 1;
}

#else /* HAVE_TIFF */

#ifdef MAC_OS
static int
tiff_load (f, img)
     struct frame *f;
     struct image *img;
{
  return image_load_quicktime (f, img, kQTFileTypeTIFF);
}
#endif /* MAC_OS */

#endif /* !HAVE_TIFF */



/***********************************************************************
				 GIF
 ***********************************************************************/

#if defined (HAVE_GIF) || defined (MAC_OS)

static int gif_image_p P_ ((Lisp_Object object));
static int gif_load P_ ((struct frame *f, struct image *img));
static void gif_clear_image P_ ((struct frame *f, struct image *img));

/* The symbol `gif' identifying images of this type.  */

Lisp_Object Qgif;

/* Indices of image specification fields in gif_format, below.  */

enum gif_keyword_index
{
  GIF_TYPE,
  GIF_DATA,
  GIF_FILE,
  GIF_ASCENT,
  GIF_MARGIN,
  GIF_RELIEF,
  GIF_ALGORITHM,
  GIF_HEURISTIC_MASK,
  GIF_MASK,
  GIF_IMAGE,
  GIF_BACKGROUND,
  GIF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword gif_format[GIF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":image",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `gif'.  */

static struct image_type gif_type =
{
  &Qgif,
  gif_image_p,
  gif_load,
  gif_clear_image,
  NULL
};

/* Free X resources of GIF image IMG which is used on frame F.  */

static void
gif_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  /* IMG->data.ptr_val may contain extension data.  */
  img->data.lisp_val = Qnil;
  x_clear_image (f, img);
}

/* Return non-zero if OBJECT is a valid GIF image specification.  */

static int
gif_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[GIF_LAST];
  bcopy (gif_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[GIF_FILE].count + fmt[GIF_DATA].count == 1;
}

#endif /* HAVE_GIF || MAC_OS */

#ifdef HAVE_GIF

#if defined (HAVE_NTGUI) || defined (MAC_OS)
/* winuser.h might define DrawText to DrawTextA or DrawTextW.
   Undefine before redefining to avoid a preprocessor warning.  */
#ifdef DrawText
#undef DrawText
#endif
/* avoid conflict with QuickdrawText.h */
#define DrawText gif_DrawText
#include <gif_lib.h>
#undef DrawText

#else /* HAVE_NTGUI || MAC_OS */

#include <gif_lib.h>

#endif /* HAVE_NTGUI || MAC_OS */


#ifdef HAVE_NTGUI

/* GIF library details.  */
DEF_IMGLIB_FN (DGifCloseFile);
DEF_IMGLIB_FN (DGifSlurp);
DEF_IMGLIB_FN (DGifOpen);
DEF_IMGLIB_FN (DGifOpenFileName);

static int
init_gif_functions (Lisp_Object libraries)
{
  HMODULE library;

  if (!(library = w32_delayed_load (libraries, Qgif)))
    return 0;

  LOAD_IMGLIB_FN (library, DGifCloseFile);
  LOAD_IMGLIB_FN (library, DGifSlurp);
  LOAD_IMGLIB_FN (library, DGifOpen);
  LOAD_IMGLIB_FN (library, DGifOpenFileName);
  return 1;
}

#else

#define fn_DGifCloseFile	DGifCloseFile
#define fn_DGifSlurp		DGifSlurp
#define fn_DGifOpen		DGifOpen
#define fn_DGifOpenFileName	DGifOpenFileName

#endif /* HAVE_NTGUI */

/* Reading a GIF image from memory
   Based on the PNG memory stuff to a certain extent. */

typedef struct
{
  unsigned char *bytes;
  size_t len;
  int index;
}
gif_memory_source;

/* Make the current memory source available to gif_read_from_memory.
   It's done this way because not all versions of libungif support
   a UserData field in the GifFileType structure.  */
static gif_memory_source *current_gif_memory_src;

static int
gif_read_from_memory (file, buf, len)
     GifFileType *file;
     GifByteType *buf;
     int len;
{
  gif_memory_source *src = current_gif_memory_src;

  if (len > src->len - src->index)
    return -1;

  bcopy (src->bytes + src->index, buf, len);
  src->index += len;
  return len;
}


/* Load GIF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int interlace_start[] = {0, 4, 2, 1};
static int interlace_increment[] = {8, 8, 4, 2};

static int
gif_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  int rc, width, height, x, y, i;
  XImagePtr ximg;
  ColorMapObject *gif_color_map;
  unsigned long pixel_colors[256];
  GifFileType *gif;
  struct gcpro gcpro1;
  Lisp_Object image;
  int ino, image_height, image_width;
  gif_memory_source memsrc;
  unsigned char *raster;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Open the GIF file.  Casting return value avoids a GCC warning
	 on W32.  */
      gif = (GifFileType *)fn_DGifOpenFileName (SDATA (file));
      if (gif == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }
  else
    {
      /* Read from memory! */
      current_gif_memory_src = &memsrc;
      memsrc.bytes = SDATA (specified_data);
      memsrc.len = SBYTES (specified_data);
      memsrc.index = 0;

      /* Casting return value avoids a GCC warning on W32.  */
      gif = (GifFileType *) fn_DGifOpen (&memsrc, gif_read_from_memory);
      if (!gif)
	{
	  image_error ("Cannot open memory source `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Before reading entire contents, check the declared image size. */
  if (!check_image_size (f, gif->SWidth, gif->SHeight))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      fn_DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  /* Read entire contents.  */
  rc = fn_DGifSlurp (gif);
  if (rc == GIF_ERROR)
    {
      image_error ("Error reading `%s'", img->spec, Qnil);
      fn_DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  image = image_spec_value (img->spec, QCindex, NULL);
  ino = INTEGERP (image) ? XFASTINT (image) : 0;
  if (ino >= gif->ImageCount)
    {
      image_error ("Invalid image number `%s' in image `%s'",
		   image, img->spec);
      fn_DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  img->corners[TOP_CORNER] = gif->SavedImages[ino].ImageDesc.Top;
  img->corners[LEFT_CORNER] = gif->SavedImages[ino].ImageDesc.Left;
  image_height = gif->SavedImages[ino].ImageDesc.Height;
  img->corners[BOT_CORNER] = img->corners[TOP_CORNER] + image_height;
  image_width = gif->SavedImages[ino].ImageDesc.Width;
  img->corners[RIGHT_CORNER] = img->corners[LEFT_CORNER] + image_width;

  width = img->width = max (gif->SWidth,
			    max (gif->Image.Left + gif->Image.Width,
				 img->corners[RIGHT_CORNER]));
  height = img->height = max (gif->SHeight,
			      max (gif->Image.Top + gif->Image.Height,
				   img->corners[BOT_CORNER]));

  if (!check_image_size (f, width, height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      fn_DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      fn_DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  /* Allocate colors.  */
  gif_color_map = gif->SavedImages[ino].ImageDesc.ColorMap;
  if (!gif_color_map)
    gif_color_map = gif->SColorMap;
  init_color_table ();
  bzero (pixel_colors, sizeof pixel_colors);

  if (gif_color_map)
    for (i = 0; i < gif_color_map->ColorCount; ++i)
      {
        int r = gif_color_map->Colors[i].Red << 8;
        int g = gif_color_map->Colors[i].Green << 8;
        int b = gif_color_map->Colors[i].Blue << 8;
        pixel_colors[i] = lookup_rgb_color (f, r, g, b);
      }

#ifdef COLOR_TABLE_SUPPORT
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  /* Clear the part of the screen image that are not covered by
     the image from the GIF file.  Full animated GIF support
     requires more than can be done here (see the gif89 spec,
     disposal methods).  Let's simply assume that the part
     not covered by a sub-image is in the frame's background color.  */
  for (y = 0; y < img->corners[TOP_CORNER]; ++y)
    for (x = 0; x < width; ++x)
      XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));

  for (y = img->corners[BOT_CORNER]; y < height; ++y)
    for (x = 0; x < width; ++x)
      XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));

  for (y = img->corners[TOP_CORNER]; y < img->corners[BOT_CORNER]; ++y)
    {
      for (x = 0; x < img->corners[LEFT_CORNER]; ++x)
	XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));
      for (x = img->corners[RIGHT_CORNER]; x < width; ++x)
	XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));
    }

  /* Read the GIF image into the X image.  We use a local variable
     `raster' here because RasterBits below is a char *, and invites
     problems with bytes >= 0x80.  */
  raster = (unsigned char *) gif->SavedImages[ino].RasterBits;

  if (gif->SavedImages[ino].ImageDesc.Interlace)
    {
      int pass;
      int row = interlace_start[0];

      pass = 0;

      for (y = 0; y < image_height; y++)
	{
	  if (row >= image_height)
	    {
	      row = interlace_start[++pass];
	      while (row >= image_height)
		row = interlace_start[++pass];
	    }

	  for (x = 0; x < image_width; x++)
	    {
	      int i = raster[(y * image_width) + x];
	      XPutPixel (ximg, x + img->corners[LEFT_CORNER],
			 row + img->corners[TOP_CORNER], pixel_colors[i]);
	    }

	  row += interlace_increment[pass];
	}
    }
  else
    {
      for (y = 0; y < image_height; ++y)
	for (x = 0; x < image_width; ++x)
	  {
	    int i = raster[y * image_width + x];
	    XPutPixel (ximg, x + img->corners[LEFT_CORNER],
		       y + img->corners[TOP_CORNER], pixel_colors[i]);
	  }
    }

  /* Save GIF image extension data for `image-extension-data'.
     Format is (count IMAGES FUNCTION "BYTES" ...).  */
  img->data.lisp_val = Qnil;
  if (gif->SavedImages[ino].ExtensionBlockCount > 0)
    {
      ExtensionBlock *ext = gif->SavedImages[ino].ExtensionBlocks;
      for (i = 0; i < gif->SavedImages[ino].ExtensionBlockCount; i++, ext++)
	/* Append (... FUNCTION "BYTES") */
	img->data.lisp_val = Fcons (make_unibyte_string (ext->Bytes, ext->ByteCount),
				    Fcons (make_number (ext->Function),
					   img->data.lisp_val));
      img->data.lisp_val = Fnreverse (img->data.lisp_val);
    }
  if (gif->ImageCount > 1)
    img->data.lisp_val = Fcons (Qcount,
				Fcons (make_number (gif->ImageCount),
				       img->data.lisp_val));

  fn_DGifCloseFile (gif);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  UNGCPRO;
  return 1;
}

#else  /* !HAVE_GIF */

#ifdef MAC_OS
static int
gif_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object specified_file, file;
  Lisp_Object specified_data;
  OSErr err;
  Boolean graphic_p, movie_p, prefer_graphic_p;
  Handle dh = NULL;
  Movie movie = NULL;
  Lisp_Object image;
  Track track = NULL;
  Media media = NULL;
  long nsamples;
  Rect rect;
  Lisp_Object specified_bg;
  XColor color;
  RGBColor bg_color;
  int width, height;
  XImagePtr ximg;
  TimeScale time_scale;
  TimeValue time, duration;
  int ino;
  CGrafPtr old_port;
  GDHandle old_gdh;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  /* Animated gifs use QuickTime Movie Toolbox.  So initialize it here. */
  EnterMovies ();

  if (NILP (specified_data))
    {
      /* Read from a file */
      FSSpec fss;
      short refnum;

      err = find_image_fsspec (specified_file, &file, &fss);
      if (err != noErr)
	{
	  if (err == fnfErr)
	    image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  else
	    goto open_error;
	}

      err = CanQuickTimeOpenFile (&fss, kQTFileTypeGIF, 0,
				  &graphic_p, &movie_p, &prefer_graphic_p, 0);
      if (err != noErr)
	goto open_error;

      if (!graphic_p && !movie_p)
	goto open_error;
      if (prefer_graphic_p)
	return image_load_qt_1 (f, img, kQTFileTypeGIF, &fss, NULL);
      err = OpenMovieFile (&fss, &refnum, fsRdPerm);
      if (err != noErr)
	goto open_error;
      err = NewMovieFromFile (&movie, refnum, NULL, NULL, 0, NULL);
      CloseMovieFile (refnum);
      if (err != noErr)
	{
	  image_error ("Error reading `%s'", file, Qnil);
	  return 0;
	}
    }
  else
    {
      /* Memory source! */
      Handle dref = NULL;
      long file_type_atom[3];

      err = PtrToHand (SDATA (specified_data), &dh, SBYTES (specified_data));
      if (err != noErr)
	{
	  image_error ("Cannot allocate data handle for `%s'",
		       img->spec, Qnil);
	  goto error;
	}

      file_type_atom[0] = EndianU32_NtoB (sizeof (long) * 3);
      file_type_atom[1] = EndianU32_NtoB (kDataRefExtensionMacOSFileType);
      file_type_atom[2] = EndianU32_NtoB (kQTFileTypeGIF);
      err = PtrToHand (&dh, &dref, sizeof (Handle));
      if (err == noErr)
	/* no file name */
	err = PtrAndHand ("\p", dref, 1);
      if (err == noErr)
	err = PtrAndHand (file_type_atom, dref, sizeof (long) * 3);
      if (err != noErr)
	{
	  image_error ("Cannot allocate handle data ref for `%s'", img->spec, Qnil);
	  goto error;
	}
      err = CanQuickTimeOpenDataRef (dref, HandleDataHandlerSubType, &graphic_p,
				     &movie_p, &prefer_graphic_p, 0);
      if (err != noErr)
	goto open_error;

      if (!graphic_p && !movie_p)
	goto open_error;
      if (prefer_graphic_p)
	{
	  int success_p;

	  DisposeHandle (dref);
	  success_p = image_load_qt_1 (f, img, kQTFileTypeGIF, NULL, dh);
	  DisposeHandle (dh);
	  return success_p;
	}
      err = NewMovieFromDataRef (&movie, 0, NULL, dref,
				 HandleDataHandlerSubType);
      DisposeHandle (dref);
      if (err != noErr)
	goto open_error;
    }

  image = image_spec_value (img->spec, QCindex, NULL);
  ino = INTEGERP (image) ? XFASTINT (image) : 0;
  track = GetMovieIndTrack (movie, 1);
  media = GetTrackMedia (track);
  nsamples = GetMediaSampleCount (media);
  if (ino >= nsamples)
    {
      image_error ("Invalid image number `%s' in image `%s'",
		   image, img->spec);
      goto error;
    }
  time_scale = GetMediaTimeScale (media);

  specified_bg = image_spec_value (img->spec, QCbackground, NULL);
  if (!STRINGP (specified_bg) ||
      !mac_defined_color (f, SDATA (specified_bg), &color, 0))
    {
      color.pixel = FRAME_BACKGROUND_PIXEL (f);
      color.red = RED16_FROM_ULONG (color.pixel);
      color.green = GREEN16_FROM_ULONG (color.pixel);
      color.blue = BLUE16_FROM_ULONG (color.pixel);
    }
  GetMovieBox (movie, &rect);
  width = img->width = rect.right - rect.left;
  height = img->height = rect.bottom - rect.top;
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    goto error;

  GetGWorld (&old_port, &old_gdh);
  SetGWorld (ximg, NULL);
  bg_color.red = color.red;
  bg_color.green = color.green;
  bg_color.blue = color.blue;
  RGBBackColor (&bg_color);
  SetGWorld (old_port, old_gdh);
  SetMovieActive (movie, 1);
  SetMovieGWorld (movie, ximg, NULL);
  SampleNumToMediaTime (media, ino + 1, &time, &duration);
  SetMovieTimeValue (movie, time);
  MoviesTask (movie, 0L);
  DisposeTrackMedia (media);
  DisposeMovieTrack (track);
  DisposeMovie (movie);
  if (dh)
    DisposeHandle (dh);

  /* Save GIF image extension data for `image-extension-data'.
     Format is (count IMAGES 0xf9 GRAPHIC_CONTROL_EXTENSION_BLOCK).  */
  {
    Lisp_Object gce = make_uninit_string (4);
    int centisec = ((float)duration / time_scale) * 100.0f + 0.5f;

    /* Fill the delay time field.  */
    SSET (gce, 1, centisec & 0xff);
    SSET (gce, 2, (centisec >> 8) & 0xff);
    /* We don't know about other fields.  */
    SSET (gce, 0, 0);
    SSET (gce, 3, 0);

    img->data.lisp_val = list4 (Qcount, make_number (nsamples),
				make_number (0xf9), gce);
  }

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  return 1;

 open_error:
  image_error ("Cannot open `%s'", file, Qnil);
 error:
  if (media)
    DisposeTrackMedia (media);
  if (track)
    DisposeMovieTrack (track);
  if (movie)
    DisposeMovie (movie);
  if (dh)
    DisposeHandle (dh);
  return 0;
}
#endif /* MAC_OS */

#endif /* HAVE_GIF */



/***********************************************************************
				 SVG
 ***********************************************************************/

#if defined (HAVE_RSVG)

/* Function prototypes.  */

static int svg_image_p P_ ((Lisp_Object object));
static int svg_load P_ ((struct frame *f, struct image *img));

static int svg_load_image P_ ((struct frame *, struct image *,
			       unsigned char *, unsigned int));

/* The symbol `svg' identifying images of this type. */

Lisp_Object Qsvg;

/* Indices of image specification fields in svg_format, below.  */

enum svg_keyword_index
{
  SVG_TYPE,
  SVG_DATA,
  SVG_FILE,
  SVG_ASCENT,
  SVG_MARGIN,
  SVG_RELIEF,
  SVG_ALGORITHM,
  SVG_HEURISTIC_MASK,
  SVG_MASK,
  SVG_BACKGROUND,
  SVG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword svg_format[SVG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `svg'.  Its the same type of
   structure defined for all image formats, handled by emacs image
   functions.  See struct image_type in dispextern.h.  */

static struct image_type svg_type =
{
  /* An identifier showing that this is an image structure for the SVG format.  */
  &Qsvg,
  /* Handle to a function that can be used to identify a SVG file.  */
  svg_image_p,
  /* Handle to function used to load a SVG file.  */
  svg_load,
  /* Handle to function to free sresources for SVG.  */
  x_clear_image,
  /* An internal field to link to the next image type in a list of
     image types, will be filled in when registering the format.  */
  NULL
};


/* Return non-zero if OBJECT is a valid SVG image specification.  Do
   this by calling parse_image_spec and supplying the keywords that
   identify the SVG format.   */

static int
svg_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[SVG_LAST];
  bcopy (svg_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, SVG_LAST, Qsvg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[SVG_FILE].count + fmt[SVG_DATA].count == 1;
}

#include <librsvg/rsvg.h>

#ifdef HAVE_NTGUI

/* SVG library functions.  */
DEF_IMGLIB_FN (rsvg_handle_new);
DEF_IMGLIB_FN (rsvg_handle_set_size_callback);
DEF_IMGLIB_FN (rsvg_handle_write);
DEF_IMGLIB_FN (rsvg_handle_close);
DEF_IMGLIB_FN (rsvg_handle_get_pixbuf);
DEF_IMGLIB_FN (rsvg_handle_free);

DEF_IMGLIB_FN (gdk_pixbuf_get_width);
DEF_IMGLIB_FN (gdk_pixbuf_get_height);
DEF_IMGLIB_FN (gdk_pixbuf_get_pixels);
DEF_IMGLIB_FN (gdk_pixbuf_get_rowstride);
DEF_IMGLIB_FN (gdk_pixbuf_get_colorspace);
DEF_IMGLIB_FN (gdk_pixbuf_get_n_channels);
DEF_IMGLIB_FN (gdk_pixbuf_get_has_alpha);
DEF_IMGLIB_FN (gdk_pixbuf_get_bits_per_sample);

DEF_IMGLIB_FN (g_type_init);
DEF_IMGLIB_FN (g_object_unref);
DEF_IMGLIB_FN (g_error_free);

Lisp_Object Qgdk_pixbuf, Qglib;

static int
init_svg_functions (Lisp_Object libraries)
{
  HMODULE library, gdklib, glib;

  if (!(glib = w32_delayed_load (libraries, Qglib))
      || !(gdklib = w32_delayed_load (libraries, Qgdk_pixbuf))
      || !(library = w32_delayed_load (libraries, Qsvg)))
    return 0;

  LOAD_IMGLIB_FN (library, rsvg_handle_new);
  LOAD_IMGLIB_FN (library, rsvg_handle_set_size_callback);
  LOAD_IMGLIB_FN (library, rsvg_handle_write);
  LOAD_IMGLIB_FN (library, rsvg_handle_close);
  LOAD_IMGLIB_FN (library, rsvg_handle_get_pixbuf);
  LOAD_IMGLIB_FN (library, rsvg_handle_free);

  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_width);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_height);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_pixels);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_rowstride);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_colorspace);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_n_channels);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_has_alpha);
  LOAD_IMGLIB_FN (gdklib, gdk_pixbuf_get_bits_per_sample);

  LOAD_IMGLIB_FN (glib, g_type_init);
  LOAD_IMGLIB_FN (glib, g_object_unref);
  LOAD_IMGLIB_FN (glib, g_error_free);
  return 1;
}

#else
/* The following aliases for library functions allow dynamic loading
   to be used on some platforms.  */
#define fn_rsvg_handle_new		rsvg_handle_new
#define fn_rsvg_handle_set_size_callback rsvg_handle_set_size_callback
#define fn_rsvg_handle_write		rsvg_handle_write
#define fn_rsvg_handle_close		rsvg_handle_close
#define fn_rsvg_handle_get_pixbuf	rsvg_handle_get_pixbuf
#define fn_rsvg_handle_free		rsvg_handle_free

#define fn_gdk_pixbuf_get_width		  gdk_pixbuf_get_width
#define fn_gdk_pixbuf_get_height	  gdk_pixbuf_get_height
#define fn_gdk_pixbuf_get_pixels	  gdk_pixbuf_get_pixels
#define fn_gdk_pixbuf_get_rowstride	  gdk_pixbuf_get_rowstride
#define fn_gdk_pixbuf_get_colorspace	  gdk_pixbuf_get_colorspace
#define fn_gdk_pixbuf_get_n_channels	  gdk_pixbuf_get_n_channels
#define fn_gdk_pixbuf_get_has_alpha	  gdk_pixbuf_get_has_alpha
#define fn_gdk_pixbuf_get_bits_per_sample gdk_pixbuf_get_bits_per_sample

#define fn_g_type_init                    g_type_init
#define fn_g_object_unref                 g_object_unref
#define fn_g_error_free                   g_error_free
#endif /* !HAVE_NTGUI  */

/* Load SVG image IMG for use on frame F.  Value is non-zero if
   successful. this function will go into the svg_type structure, and
   the prototype thus needs to be compatible with that structure.  */

static int
svg_load (f, img)
     struct frame *f;
     struct image *img;
{
  int success_p = 0;
  Lisp_Object file_name;

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      unsigned char *contents;
      int size;
      struct gcpro gcpro1;

      file = x_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Read the entire file into memory.  */
      contents = slurp_file (SDATA (file), &size);
      if (contents == NULL)
	{
	  image_error ("Error loading SVG image `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}
      /* If the file was slurped into memory properly, parse it.  */
      success_p = svg_load_image (f, img, contents, size);
      xfree (contents);
      UNGCPRO;
    }
  /* Else its not a file, its a lisp object.  Load the image from a
     lisp object rather than a file.  */
  else
    {
      Lisp_Object data;

      data = image_spec_value (img->spec, QCdata, NULL);
      success_p = svg_load_image (f, img, SDATA (data), SBYTES (data));
    }

  return success_p;
}

/* svg_load_image is a helper function for svg_load, which does the
   actual loading given contents and size, apart from frame and image
   structures, passed from svg_load.

   Uses librsvg to do most of the image processing.

   Returns non-zero when successful.  */
static int
svg_load_image (f, img, contents, size)
    /* Pointer to emacs frame structure.  */
     struct frame *f;
     /* Pointer to emacs image structure.  */
     struct image *img;
     /* String containing the SVG XML data to be parsed.  */
     unsigned char *contents;
     /* Size of data in bytes.  */
     unsigned int size;
{
  RsvgHandle *rsvg_handle;
  GError *error = NULL;
  GdkPixbuf *pixbuf;
  int width;
  int height;
  const guint8 *pixels;
  int rowstride;
  XImagePtr ximg;
  Lisp_Object specified_bg;
  XColor background;
  int x;
  int y;

  /* g_type_init is a glib function that must be called prior to using
     gnome type library functions.  */
  fn_g_type_init ();
  /* Make a handle to a new rsvg object.  */
  rsvg_handle = fn_rsvg_handle_new ();

  /* Parse the contents argument and fill in the rsvg_handle.  */
  fn_rsvg_handle_write (rsvg_handle, contents, size, &error);
  if (error)
    goto rsvg_error;

  /* The parsing is complete, rsvg_handle is ready to used, close it
     for further writes.  */
  fn_rsvg_handle_close (rsvg_handle, &error);
  if (error)
    goto rsvg_error;
  /* We can now get a valid pixel buffer from the svg file, if all
     went ok.  */
  pixbuf = fn_rsvg_handle_get_pixbuf (rsvg_handle);
  eassert (pixbuf);

  /* Extract some meta data from the svg handle.  */
  width     = fn_gdk_pixbuf_get_width (pixbuf);
  height    = fn_gdk_pixbuf_get_height (pixbuf);
  pixels    = fn_gdk_pixbuf_get_pixels (pixbuf);
  rowstride = fn_gdk_pixbuf_get_rowstride (pixbuf);

  /* Validate the svg meta data.  */
  eassert (fn_gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
  eassert (fn_gdk_pixbuf_get_n_channels (pixbuf) == 4);
  eassert (fn_gdk_pixbuf_get_has_alpha (pixbuf));
  eassert (fn_gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);

  /* Try to create a x pixmap to hold the svg pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      fn_g_object_unref (pixbuf);
      return 0;
    }

  init_color_table ();

  /* Handle alpha channel by combining the image with a background
     color.  */
  specified_bg = image_spec_value (img->spec, QCbackground, NULL);
  if (STRINGP (specified_bg)
      && x_defined_color (f, SDATA (specified_bg), &background, 0))
    {
      background.red   >>= 8;
      background.green >>= 8;
      background.blue  >>= 8;
    }
  else
    {
#ifdef HAVE_X_WINDOWS
      background.pixel = FRAME_BACKGROUND_PIXEL (f);
      x_query_color (f, &background);

      /* SVG pixmaps specify transparency in the last byte, so right
	 shift 8 bits to get rid of it, since emacs doesn't support
	 transparency.  */
      background.red   >>= 8;
      background.green >>= 8;
      background.blue  >>= 8;
#elif defined (MAC_OS)
      background.pixel = FRAME_BACKGROUND_PIXEL (f);
      background.red   = RED_FROM_ULONG (background.pixel);
      background.green = GREEN_FROM_ULONG (background.pixel);
      background.blue  = BLUE_FROM_ULONG (background.pixel);
#elif defined (HAVE_NTGUI)
      background.pixel = FRAME_BACKGROUND_PIXEL (f);
#if 0 /* W32 TODO : Colormap support.  */
      x_query_color (f, &background);
#endif

      /* SVG pixmaps specify transparency in the last byte, so right
	 shift 8 bits to get rid of it, since emacs doesn't support
	 transparency.  */
      background.red   >>= 8;
      background.green >>= 8;
      background.blue  >>= 8;
#else /* not HAVE_X_WINDOWS && not MAC_OS*/
#error FIXME
#endif
    }

  /* This loop handles opacity values, since Emacs assumes
     non-transparent images.  Each pixel must be "flattened" by
     calculating the resulting color, given the transparency of the
     pixel, and the image background color.  */
  for (y = 0; y < height; ++y)
    {
      for (x = 0; x < width; ++x)
	{
	  unsigned red;
	  unsigned green;
	  unsigned blue;
	  unsigned opacity;

	  red     = *pixels++;
	  green   = *pixels++;
	  blue    = *pixels++;
	  opacity = *pixels++;

	  red   = ((red * opacity)
		   + (background.red * ((1 << 8) - opacity)));
	  green = ((green * opacity)
		   + (background.green * ((1 << 8) - opacity)));
	  blue  = ((blue * opacity)
		   + (background.blue * ((1 << 8) - opacity)));

	  XPutPixel (ximg, x, y, lookup_rgb_color (f, red, green, blue));
	}

      pixels += rowstride - 4 * width;
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  fn_g_object_unref (pixbuf);

  img->width  = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy.
     Casting avoids a GCC warning.  */
  IMAGE_BACKGROUND (img, f, (XImagePtr_or_DC)ximg);

  /* Put the image into the pixmap, then free the X image and its
     buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  return 1;

 rsvg_error:
  /* FIXME: Use error->message so the user knows what is the actual
     problem with the image.  */
  image_error ("Error parsing SVG image `%s'", img->spec, Qnil);
  fn_g_error_free (error);
  return 0;
}

#endif	/* defined (HAVE_RSVG) */




/***********************************************************************
				Ghostscript
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS
#define HAVE_GHOSTSCRIPT 1
#endif /* HAVE_X_WINDOWS */

/* The symbol `postscript' identifying images of this type.  */

Lisp_Object Qpostscript;

#ifdef HAVE_GHOSTSCRIPT

static int gs_image_p P_ ((Lisp_Object object));
static int gs_load P_ ((struct frame *f, struct image *img));
static void gs_clear_image P_ ((struct frame *f, struct image *img));

/* Keyword symbols.  */

Lisp_Object QCloader, QCbounding_box, QCpt_width, QCpt_height;

/* Indices of image specification fields in gs_format, below.  */

enum gs_keyword_index
{
  GS_TYPE,
  GS_PT_WIDTH,
  GS_PT_HEIGHT,
  GS_FILE,
  GS_LOADER,
  GS_BOUNDING_BOX,
  GS_ASCENT,
  GS_MARGIN,
  GS_RELIEF,
  GS_ALGORITHM,
  GS_HEURISTIC_MASK,
  GS_MASK,
  GS_BACKGROUND,
  GS_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword gs_format[GS_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":pt-width",		IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":pt-height",	IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":loader",		IMAGE_FUNCTION_VALUE,			0},
  {":bounding-box",	IMAGE_DONT_CHECK_VALUE_TYPE,		1},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `ghostscript'.  */

static struct image_type gs_type =
{
  &Qpostscript,
  gs_image_p,
  gs_load,
  gs_clear_image,
  NULL
};


/* Free X resources of Ghostscript image IMG which is used on frame F.  */

static void
gs_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  /* IMG->data.ptr_val may contain a recorded colormap.  */
  xfree (img->data.ptr_val);
  x_clear_image (f, img);
}


/* Return non-zero if OBJECT is a valid Ghostscript image
   specification.  */

static int
gs_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[GS_LAST];
  Lisp_Object tem;
  int i;

  bcopy (gs_format, fmt, sizeof fmt);

  if (!parse_image_spec (object, fmt, GS_LAST, Qpostscript))
    return 0;

  /* Bounding box must be a list or vector containing 4 integers.  */
  tem = fmt[GS_BOUNDING_BOX].value;
  if (CONSP (tem))
    {
      for (i = 0; i < 4; ++i, tem = XCDR (tem))
	if (!CONSP (tem) || !INTEGERP (XCAR (tem)))
	  return 0;
      if (!NILP (tem))
	return 0;
    }
  else if (VECTORP (tem))
    {
      if (XVECTOR (tem)->size != 4)
	return 0;
      for (i = 0; i < 4; ++i)
	if (!INTEGERP (XVECTOR (tem)->contents[i]))
	  return 0;
    }
  else
    return 0;

  return 1;
}


/* Load Ghostscript image IMG for use on frame F.  Value is non-zero
   if successful.  */

static int
gs_load (f, img)
     struct frame *f;
     struct image *img;
{
  char buffer[100];
  Lisp_Object window_and_pixmap_id = Qnil, loader, pt_height, pt_width;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object frame;
  double in_width, in_height;
  Lisp_Object pixel_colors = Qnil;

  /* Compute pixel size of pixmap needed from the given size in the
     image specification.  Sizes in the specification are in pt.  1 pt
     = 1/72 in, xdpi and ydpi are stored in the frame's X display
     info.  */
  pt_width = image_spec_value (img->spec, QCpt_width, NULL);
  in_width = XFASTINT (pt_width) / 72.0;
  img->width = in_width * FRAME_X_DISPLAY_INFO (f)->resx;
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = XFASTINT (pt_height) / 72.0;
  img->height = in_height * FRAME_X_DISPLAY_INFO (f)->resy;

  if (!check_image_size (f, img->width, img->height))
    {
      image_error ("Invalid image size", Qnil, Qnil);
      return 0;
    }

  /* Create the pixmap.  */
  xassert (img->pixmap == NO_PIXMAP);

  /* Only W32 version did BLOCK_INPUT here.  ++kfs */
  BLOCK_INPUT;
  img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			       img->width, img->height,
			       DefaultDepthOfScreen (FRAME_X_SCREEN (f)));
  UNBLOCK_INPUT;

  if (!img->pixmap)
    {
      image_error ("Unable to create pixmap for `%s'", img->spec, Qnil);
      return 0;
    }

  /* Call the loader to fill the pixmap.  It returns a process object
     if successful.  We do not record_unwind_protect here because
     other places in redisplay like calling window scroll functions
     don't either.  Let the Lisp loader use `unwind-protect' instead.  */
  GCPRO2 (window_and_pixmap_id, pixel_colors);

  sprintf (buffer, "%lu %lu",
	   (unsigned long) FRAME_X_WINDOW (f),
	   (unsigned long) img->pixmap);
  window_and_pixmap_id = build_string (buffer);

  sprintf (buffer, "%lu %lu",
	   FRAME_FOREGROUND_PIXEL (f),
	   FRAME_BACKGROUND_PIXEL (f));
  pixel_colors = build_string (buffer);

  XSETFRAME (frame, f);
  loader = image_spec_value (img->spec, QCloader, NULL);
  if (NILP (loader))
    loader = intern ("gs-load-image");

  img->data.lisp_val = call6 (loader, frame, img->spec,
			      make_number (img->width),
			      make_number (img->height),
			      window_and_pixmap_id,
			      pixel_colors);
  UNGCPRO;
  return PROCESSP (img->data.lisp_val);
}


/* Kill the Ghostscript process that was started to fill PIXMAP on
   frame F.  Called from XTread_socket when receiving an event
   telling Emacs that Ghostscript has finished drawing.  */

void
x_kill_gs_process (pixmap, f)
     Pixmap pixmap;
     struct frame *f;
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  int class, i;
  struct image *img;

  /* Find the image containing PIXMAP.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i]->pixmap == pixmap)
      break;

  /* Should someone in between have cleared the image cache, for
     instance, give up.  */
  if (i == c->used)
    return;

  /* Kill the GS process.  We should have found PIXMAP in the image
     cache and its image should contain a process object.  */
  img = c->images[i];
  xassert (PROCESSP (img->data.lisp_val));
  Fkill_process (img->data.lisp_val, Qnil);
  img->data.lisp_val = Qnil;

#if defined (HAVE_X_WINDOWS)

  /* On displays with a mutable colormap, figure out the colors
     allocated for the image by looking at the pixels of an XImage for
     img->pixmap.  */
  class = FRAME_X_VISUAL (f)->class;
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
      XImagePtr ximg;

      BLOCK_INPUT;

      /* Try to get an XImage for img->pixmep.  */
      ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			0, 0, img->width, img->height, ~0, ZPixmap);
      if (ximg)
	{
	  int x, y;

	  /* Initialize the color table.  */
	  init_color_table ();

	  /* For each pixel of the image, look its color up in the
	     color table.  After having done so, the color table will
	     contain an entry for each color used by the image.  */
	  for (y = 0; y < img->height; ++y)
	    for (x = 0; x < img->width; ++x)
	      {
		unsigned long pixel = XGetPixel (ximg, x, y);
		lookup_pixel_color (f, pixel);
	      }

	  /* Record colors in the image.  Free color table and XImage.  */
#ifdef COLOR_TABLE_SUPPORT
	  img->colors = colors_in_color_table (&img->ncolors);
	  free_color_table ();
#endif
	  XDestroyImage (ximg);

#if 0 /* This doesn't seem to be the case.  If we free the colors
	 here, we get a BadAccess later in x_clear_image when
	 freeing the colors.  */
	  /* We have allocated colors once, but Ghostscript has also
	     allocated colors on behalf of us.  So, to get the
	     reference counts right, free them once.  */
	  if (img->ncolors)
	    x_free_colors (f, img->colors, img->ncolors);
#endif
	}
      else
	image_error ("Cannot get X image of `%s'; colors will not be freed",
		     img->spec, Qnil);

      UNBLOCK_INPUT;
    }
#endif /* HAVE_X_WINDOWS */

  /* Now that we have the pixmap, compute mask and transform the
     image if requested.  */
  BLOCK_INPUT;
  postprocess_image (f, img);
  UNBLOCK_INPUT;
}

#endif /* HAVE_GHOSTSCRIPT */


/***********************************************************************
				Tests
 ***********************************************************************/

#if GLYPH_DEBUG

DEFUN ("imagep", Fimagep, Simagep, 1, 1, 0,
       doc: /* Value is non-nil if SPEC is a valid image specification.  */)
  (spec)
     Lisp_Object spec;
{
  return valid_image_p (spec) ? Qt : Qnil;
}


DEFUN ("lookup-image", Flookup_image, Slookup_image, 1, 1, 0, "")
  (spec)
     Lisp_Object spec;
{
  int id = -1;

  if (valid_image_p (spec))
    id = lookup_image (SELECTED_FRAME (), spec);

  debug_print (spec);
  return make_number (id);
}

#endif /* GLYPH_DEBUG != 0 */


/***********************************************************************
			    Initialization
 ***********************************************************************/

#ifdef HAVE_NTGUI
/* Image types that rely on external libraries are loaded dynamically
   if the library is available.  */
#define CHECK_LIB_AVAILABLE(image_type, init_lib_fn, libraries) \
  define_image_type (image_type, init_lib_fn (libraries))
#else
#define CHECK_LIB_AVAILABLE(image_type, init_lib_fn, libraries) \
  define_image_type (image_type, 1)
#endif /* HAVE_NTGUI */

DEFUN ("init-image-library", Finit_image_library, Sinit_image_library, 2, 2, 0,
       doc: /* Initialize image library implementing image type TYPE.
Return non-nil if TYPE is a supported image type.

Image types pbm and xbm are prebuilt; other types are loaded here.
Libraries to load are specified in alist LIBRARIES (usually, the value
of `image-library-alist', which see).  */)
  (type, libraries)
  Lisp_Object type, libraries;
{
  Lisp_Object tested;

  /* Don't try to reload the library.  */
  tested = Fassq (type, Vimage_type_cache);
  if (CONSP (tested))
    return XCDR (tested);

#if defined (HAVE_XPM) || defined (MAC_OS)
  if (EQ (type, Qxpm))
    return CHECK_LIB_AVAILABLE (&xpm_type, init_xpm_functions, libraries);
#endif

#if defined (HAVE_JPEG) || defined (MAC_OS)
  if (EQ (type, Qjpeg))
    return CHECK_LIB_AVAILABLE (&jpeg_type, init_jpeg_functions, libraries);
#endif

#if defined (HAVE_TIFF) || defined (MAC_OS)
  if (EQ (type, Qtiff))
    return CHECK_LIB_AVAILABLE (&tiff_type, init_tiff_functions, libraries);
#endif

#if defined (HAVE_GIF) || defined (MAC_OS)
  if (EQ (type, Qgif))
    return CHECK_LIB_AVAILABLE (&gif_type, init_gif_functions, libraries);
#endif

#if defined (HAVE_PNG) || defined (MAC_OS)
  if (EQ (type, Qpng))
    return CHECK_LIB_AVAILABLE (&png_type, init_png_functions, libraries);
#endif

#if defined (HAVE_RSVG)
  if (EQ (type, Qsvg))
    return CHECK_LIB_AVAILABLE (&svg_type, init_svg_functions, libraries);
#endif

#ifdef HAVE_GHOSTSCRIPT
  if (EQ (type, Qpostscript))
    return CHECK_LIB_AVAILABLE (&gs_type, init_gs_functions, libraries);
#endif

  /* If the type is not recognized, avoid testing it ever again.  */
  CACHE_IMAGE_TYPE (type, Qnil);
  return Qnil;
}

void
syms_of_image ()
{
  extern Lisp_Object Qrisky_local_variable;   /* Syms_of_xdisp has already run.  */

  /* Initialize this only once, since that's what we do with Vimage_types
     and they are supposed to be in sync.  Initializing here gives correct
     operation on GNU/Linux of calling dump-emacs after loading some images.  */
  image_types = NULL;

  /* Must be defined now becase we're going to update it below, while
     defining the supported image types.  */
  DEFVAR_LISP ("image-types", &Vimage_types,
    doc: /* List of potentially supported image types.
Each element of the list is a symbol for an image type, like 'jpeg or 'png.
To check whether it is really supported, use `image-type-available-p'.  */);
  Vimage_types = Qnil;

  DEFVAR_LISP ("image-library-alist", &Vimage_library_alist,
    doc: /* Alist of image types vs external libraries needed to display them.

Each element is a list (IMAGE-TYPE LIBRARY...), where the car is a symbol
representing a supported image type, and the rest are strings giving
alternate filenames for the corresponding external libraries.

Emacs tries to load the libraries in the order they appear on the
list; if none is loaded, the running session of Emacs won't
support the image type.  Types 'pbm and 'xbm don't need to be
listed; they are always supported.  */);
  Vimage_library_alist = Qnil;
  Fput (intern ("image-library-alist"), Qrisky_local_variable, Qt);

  DEFVAR_LISP ("max-image-size", &Vmax_image_size,
    doc: /* Maximum size of images.
Emacs will not load an image into memory if its pixel width or
pixel height exceeds this limit.

If the value is an integer, it directly specifies the maximum
image height and width, measured in pixels.  If it is a floating
point number, it specifies the maximum image height and width
as a ratio to the frame height and width.  If the value is
non-numeric, there is no explicit limit on the size of images.  */);
  Vmax_image_size = make_float (MAX_IMAGE_SIZE);

  Vimage_type_cache = Qnil;
  staticpro (&Vimage_type_cache);

  Qpbm = intern ("pbm");
  staticpro (&Qpbm);
  ADD_IMAGE_TYPE (Qpbm);

  Qxbm = intern ("xbm");
  staticpro (&Qxbm);
  ADD_IMAGE_TYPE (Qxbm);

  define_image_type (&xbm_type, 1);
  define_image_type (&pbm_type, 1);

  Qcount = intern ("count");
  staticpro (&Qcount);

  QCascent = intern (":ascent");
  staticpro (&QCascent);
  QCmargin = intern (":margin");
  staticpro (&QCmargin);
  QCrelief = intern (":relief");
  staticpro (&QCrelief);
  QCconversion = intern (":conversion");
  staticpro (&QCconversion);
  QCcolor_symbols = intern (":color-symbols");
  staticpro (&QCcolor_symbols);
  QCheuristic_mask = intern (":heuristic-mask");
  staticpro (&QCheuristic_mask);
  QCindex = intern (":index");
  staticpro (&QCindex);
  QCmatrix = intern (":matrix");
  staticpro (&QCmatrix);
  QCcolor_adjustment = intern (":color-adjustment");
  staticpro (&QCcolor_adjustment);
  QCmask = intern (":mask");
  staticpro (&QCmask);

  Qlaplace = intern ("laplace");
  staticpro (&Qlaplace);
  Qemboss = intern ("emboss");
  staticpro (&Qemboss);
  Qedge_detection = intern ("edge-detection");
  staticpro (&Qedge_detection);
  Qheuristic = intern ("heuristic");
  staticpro (&Qheuristic);

  Qpostscript = intern ("postscript");
  staticpro (&Qpostscript);
#ifdef HAVE_GHOSTSCRIPT
  ADD_IMAGE_TYPE (Qpostscript);
  QCloader = intern (":loader");
  staticpro (&QCloader);
  QCbounding_box = intern (":bounding-box");
  staticpro (&QCbounding_box);
  QCpt_width = intern (":pt-width");
  staticpro (&QCpt_width);
  QCpt_height = intern (":pt-height");
  staticpro (&QCpt_height);
#endif /* HAVE_GHOSTSCRIPT */

#if defined (HAVE_XPM) || defined (MAC_OS)
  Qxpm = intern ("xpm");
  staticpro (&Qxpm);
  ADD_IMAGE_TYPE (Qxpm);
#endif

#if defined (HAVE_JPEG) || defined (MAC_OS)
  Qjpeg = intern ("jpeg");
  staticpro (&Qjpeg);
  ADD_IMAGE_TYPE (Qjpeg);
#endif

#if defined (HAVE_TIFF) || defined (MAC_OS)
  Qtiff = intern ("tiff");
  staticpro (&Qtiff);
  ADD_IMAGE_TYPE (Qtiff);
#endif

#if defined (HAVE_GIF) || defined (MAC_OS)
  Qgif = intern ("gif");
  staticpro (&Qgif);
  ADD_IMAGE_TYPE (Qgif);
#endif

#if defined (HAVE_PNG) || defined (MAC_OS)
  Qpng = intern ("png");
  staticpro (&Qpng);
  ADD_IMAGE_TYPE (Qpng);
#endif

#if defined (HAVE_RSVG)
  Qsvg = intern ("svg");
  staticpro (&Qsvg);
  ADD_IMAGE_TYPE (Qsvg);
#ifdef HAVE_NTGUI
  Qgdk_pixbuf = intern ("gdk-pixbuf");
  staticpro (&Qgdk_pixbuf);
  Qglib = intern ("glib");
  staticpro (&Qglib);
#endif /* HAVE_NTGUI  */
#endif /* HAVE_RSVG  */

  defsubr (&Sinit_image_library);
  defsubr (&Sclear_image_cache);
  defsubr (&Simage_refresh);
  defsubr (&Simage_size);
  defsubr (&Simage_mask_p);
  defsubr (&Simage_extension_data);

#if GLYPH_DEBUG
  defsubr (&Simagep);
  defsubr (&Slookup_image);
#endif

  DEFVAR_BOOL ("cross-disabled-images", &cross_disabled_images,
    doc: /* Non-nil means always draw a cross over disabled images.
Disabled images are those having a `:conversion disabled' property.
A cross is always drawn on black & white displays.  */);
  cross_disabled_images = 0;

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
    doc: /* List of directories to search for window system bitmap files.  */);
  Vx_bitmap_file_path = decode_env_path ((char *) 0, PATH_BITMAPS);

  DEFVAR_LISP ("image-cache-eviction-delay", &Vimage_cache_eviction_delay,
    doc: /* Time after which cached images are removed from the cache.
When an image has not been displayed this many seconds, remove it
from the image cache.  Value must be an integer or nil with nil
meaning don't clear the cache.  */);
  Vimage_cache_eviction_delay = make_number (30 * 60);
}

void
init_image ()
{
#if defined (MAC_OSX) && TARGET_API_MAC_CARBON
  init_image_func_pointer ();
#endif
}

/* arch-tag: 123c2a5e-14a8-4c53-ab95-af47d7db49b9
   (do not change this comment) */
