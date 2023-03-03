/* Functions for image support on window system.

Copyright (C) 1989-2023 Free Software Foundation, Inc.

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

#include <config.h>

#include <fcntl.h>
#include <math.h>
#include <unistd.h>

/* Include this before including <setjmp.h> to work around bugs with
   older libpng; see Bug#17429.  */
#if defined HAVE_PNG
# include <png.h>
#endif

#include <setjmp.h>

#include <math.h>
#include <stdint.h>
#include <c-ctype.h>
#include <flexmember.h>

#include "lisp.h"
#include "frame.h"
#include "process.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "blockinput.h"
#include "sysstdio.h"
#include "systime.h"
#include <epaths.h>
#include "coding.h"
#include "termhooks.h"
#include "font.h"
#include "pdumper.h"

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

/* Work around GCC bug 54561.  */
#if GNUC_PREREQ (4, 3, 0)
# pragma GCC diagnostic ignored "-Wclobbered"
#endif

#ifdef HAVE_X_WINDOWS
typedef struct x_bitmap_record Bitmap_Record;
#ifndef USE_CAIRO
#define GET_PIXEL(ximg, x, y) XGetPixel (ximg, x, y)
#define PUT_PIXEL XPutPixel
#define NO_PIXMAP None

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1
#endif	/* !USE_CAIRO */
#endif /* HAVE_X_WINDOWS */

#if defined(USE_CAIRO) || defined(HAVE_NS)
#define RGB_TO_ULONG(r, g, b) (((r) << 16) | ((g) << 8) | (b))
#ifndef HAVE_NS
#define ARGB_TO_ULONG(a, r, g, b) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b))
#endif
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)
#define RED16_FROM_ULONG(color)		(RED_FROM_ULONG (color) * 0x101)
#define GREEN16_FROM_ULONG(color)	(GREEN_FROM_ULONG (color) * 0x101)
#define BLUE16_FROM_ULONG(color)	(BLUE_FROM_ULONG (color) * 0x101)
#endif

#ifdef USE_CAIRO
#define GET_PIXEL image_pix_context_get_pixel
#define PUT_PIXEL image_pix_container_put_pixel
#define NO_PIXMAP 0

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	255

static unsigned long image_alloc_image_color (struct frame *, struct image *,
					      Lisp_Object, unsigned long);
#endif	/* USE_CAIRO */

#if defined HAVE_PGTK && defined HAVE_IMAGEMAGICK
/* In pgtk, we don't want to create scaled image.  If we create scaled
 * image on scale=2.0 environment, the created image is half size and
 * Gdk scales it back, and the result is blurry.  To avoid this, we
 * hold original size image as far as we can, and let Gdk to scale it
 * when it is shown.  */
# define DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE
#endif

#ifdef HAVE_NTGUI

/* We need (or want) w32.h only when we're _not_ compiling for Cygwin.  */
#ifdef WINDOWSNT
# include "w32common.h"
# include "w32.h"
#endif

typedef struct w32_bitmap_record Bitmap_Record;
#define GET_PIXEL(ximg, x, y) GetPixel (ximg, x, y)
#define PUT_PIXEL XPutPixel
#define NO_PIXMAP 0

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1

#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
typedef struct ns_bitmap_record Bitmap_Record;

#define GET_PIXEL(ximg, x, y) XGetPixel (ximg, x, y)
#define PUT_PIXEL XPutPixel
#define NO_PIXMAP 0

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1

#endif /* HAVE_NS */

#ifdef HAVE_PGTK
typedef struct pgtk_bitmap_record Bitmap_Record;
#endif /* HAVE_PGTK */

#if (defined HAVE_X_WINDOWS \
     && ! (defined HAVE_NTGUI || defined USE_CAIRO || defined HAVE_NS))
/* W32_TODO : Color tables on W32.  */
# define COLOR_TABLE_SUPPORT 1
#endif

#ifdef HAVE_HAIKU
#include "haiku_support.h"
typedef struct haiku_bitmap_record Bitmap_Record;

#define GET_PIXEL(ximg, x, y) haiku_get_pixel (ximg, x, y)
#define PUT_PIXEL haiku_put_pixel
#define NO_PIXMAP 0

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	1

#define RGB_TO_ULONG(r, g, b) (((r) << 16) | ((g) << 8) | (b))
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)
#define RED16_FROM_ULONG(color)		(RED_FROM_ULONG (color) * 0x101)
#define GREEN16_FROM_ULONG(color)	(GREEN_FROM_ULONG (color) * 0x101)
#define BLUE16_FROM_ULONG(color)	(BLUE_FROM_ULONG (color) * 0x101)

#endif

static void image_disable_image (struct frame *, struct image *);
static void image_edge_detection (struct frame *, struct image *, Lisp_Object,
                                  Lisp_Object);

static void init_color_table (void);
static unsigned long lookup_rgb_color (struct frame *f, int r, int g, int b);
#ifdef COLOR_TABLE_SUPPORT
static void free_color_table (void);
static unsigned long *colors_in_color_table (int *n);
#endif

#if defined (HAVE_WEBP) || defined (HAVE_GIF)
static void anim_prune_animation_cache (Lisp_Object);
#endif

#ifdef USE_CAIRO

static Emacs_Pix_Container
image_create_pix_container (unsigned int width, unsigned int height,
			    unsigned int depth)
{
  Emacs_Pix_Container pimg;

  pimg = xmalloc (sizeof (*pimg));
  pimg->width = width;
  pimg->height = height;
  pimg->bits_per_pixel = depth == 1 ? 8 : 32;
  pimg->bytes_per_line = cairo_format_stride_for_width ((depth == 1
							 ? CAIRO_FORMAT_A8
							 : CAIRO_FORMAT_RGB24),
							width);
  pimg->data = xmalloc (pimg->bytes_per_line * height);

  return pimg;
}

static void
image_pix_container_put_pixel (Emacs_Pix_Container image,
			       int x, int y, unsigned long pixel)
{
  if (image->bits_per_pixel == 32)
    ((uint32_t *)(image->data + y * image->bytes_per_line))[x] = pixel;
  else
    ((uint8_t *)(image->data + y * image->bytes_per_line))[x] = pixel;
}

static unsigned long
image_pix_context_get_pixel (Emacs_Pix_Context image, int x, int y)
{
  if (image->bits_per_pixel == 32)
    return ((uint32_t *)(image->data + y * image->bytes_per_line))[x];
  else
    return ((uint8_t *)(image->data + y * image->bytes_per_line))[x];
}

static Emacs_Pix_Container
image_pix_container_create_from_bitmap_data (struct frame *f,
					     char *data, unsigned int width,
					     unsigned int height,
					     unsigned long fg,
					     unsigned long bg)
{
  Emacs_Pix_Container pimg = image_create_pix_container (width, height, 0);
  int bytes_per_line = (width + (CHAR_BIT - 1)) / CHAR_BIT;

  for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
	PUT_PIXEL (pimg, x, y,
		   (data[x / CHAR_BIT] >> (x % CHAR_BIT)) & 1 ? fg : bg);
      data += bytes_per_line;
    }

  return pimg;
}

static cairo_surface_t *
cr_create_surface_from_pix_containers (Emacs_Pix_Container pimg,
				       Emacs_Pix_Container mask)
{
  cairo_surface_t *surface;

  if (mask)
    {
      int x, y;

      for (y = 0; y < pimg->height; y++)
	for (x = 0; x < pimg->width; x++)
	  {
	    unsigned long color, alpha;
	    int r, g, b;

	    color = GET_PIXEL (pimg, x, y);
	    alpha = GET_PIXEL (mask, x, y);
	    r = (RED_FROM_ULONG (color) * alpha + 0x7f) / 0xff;
	    g = (GREEN_FROM_ULONG (color) * alpha + 0x7f) / 0xff;
	    b = (BLUE_FROM_ULONG (color) * alpha + 0x7f) / 0xff;
	    PUT_PIXEL (pimg, x, y, ARGB_TO_ULONG (alpha, r, g, b));
	  }
      xfree (mask->data);
      mask->data = NULL;
    }
  surface = cairo_image_surface_create_for_data ((unsigned char *) pimg->data,
						 (mask ? CAIRO_FORMAT_ARGB32
						  : CAIRO_FORMAT_RGB24),
						 pimg->width, pimg->height,
						 pimg->bytes_per_line);
  static const cairo_user_data_key_t key;
  cairo_surface_set_user_data (surface, &key, pimg->data, xfree);
  pimg->data = NULL;

  return surface;
}

static void
cr_put_image_to_cr_data (struct image *img)
{
  cairo_pattern_t *pattern = NULL;
  cairo_surface_t *surface = cr_create_surface_from_pix_containers (img->pixmap,
								    img->mask);
  if (surface)
    {
      pattern = cairo_pattern_create_for_surface (surface);
      if (img->cr_data)
	{
	  cairo_matrix_t matrix;
	  cairo_pattern_get_matrix (img->cr_data, &matrix);
	  cairo_pattern_set_matrix (pattern, &matrix);
          cairo_pattern_set_filter
            (pattern, cairo_pattern_get_filter (img->cr_data));
	  cairo_pattern_destroy (img->cr_data);
	}
      cairo_surface_destroy (surface);
    }

  img->cr_data = pattern;
}

#endif	/* USE_CAIRO */

#ifdef HAVE_NS
/* Use with images created by ns_image_for_XPM.  */
static unsigned long
XGetPixel (Emacs_Pix_Container image, int x, int y)
{
  return ns_get_pixel (image, x, y);
}

/* Use with images created by ns_image_for_XPM; alpha set to 1;
   pixel is assumed to be in RGB form.  */
static void
XPutPixel (Emacs_Pix_Container image, int x, int y, unsigned long pixel)
{
  ns_put_pixel (image, x, y, pixel);
}
#endif /* HAVE_NS */

/* Code to deal with bitmaps.  Bitmaps are referenced by their bitmap
   id, which is just an int that this section returns.  Bitmaps are
   reference counted so they can be shared among frames.

   Bitmap indices are guaranteed to be > 0, so a negative number can
   be used to indicate no bitmap.

   If you use image_create_bitmap_from_data, then you must keep track
   of the bitmaps yourself.  That is, creating a bitmap from the same
   data more than once will not be caught.  */

/* Functions to access the contents of a bitmap, given an id.  */

#ifdef HAVE_X_WINDOWS
static int
x_bitmap_height (struct frame *f, ptrdiff_t id)
{
  return FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

static int
x_bitmap_width (struct frame *f, ptrdiff_t id)
{
  return FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

#ifdef USE_CAIRO
cairo_pattern_t *
x_bitmap_stipple (struct frame *f, Pixmap pixmap)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);

  for (ptrdiff_t i = 0; i < dpyinfo->bitmaps_last; i++)
    {
      struct x_bitmap_record *bm = dpyinfo->bitmaps + i;

      if (bm->refcount && bm->pixmap == pixmap && bm->depth == 1)
	{
	  if (bm->stipple == NULL)
	    {
	      cairo_surface_t *surface
		= cairo_xlib_surface_create_for_bitmap (FRAME_X_DISPLAY (f),
							pixmap,
							FRAME_X_SCREEN (f),
							bm->width, bm->height);
	      cairo_pattern_t *pattern
		= cairo_pattern_create_for_surface (surface);
	      cairo_surface_destroy (surface);
	      cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);
	      bm->stipple = pattern;
	    }

	  return bm->stipple;
	}
    }

  return NULL;
}

#endif	/* USE_CAIRO */
#endif

#if defined (HAVE_X_WINDOWS) || defined (HAVE_NTGUI)
ptrdiff_t
image_bitmap_pixmap (struct frame *f, ptrdiff_t id)
{
  /* HAVE_NTGUI needs the explicit cast here.  */
  return (ptrdiff_t) FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}
#endif

#ifdef HAVE_X_WINDOWS
int
x_bitmap_mask (struct frame *f, ptrdiff_t id)
{
  return FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].mask;
}
#endif

/* Allocate a new bitmap record.  Returns index of new record.  */

static ptrdiff_t
image_allocate_bitmap_record (struct frame *f)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  ptrdiff_t i;

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps =
    xpalloc (dpyinfo->bitmaps, &dpyinfo->bitmaps_size,
	     10, -1, sizeof *dpyinfo->bitmaps);
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
image_reference_bitmap (struct frame *f, ptrdiff_t id)
{
  ++FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

#ifdef HAVE_PGTK
static cairo_pattern_t *
image_create_pattern_from_pixbuf (struct frame *f, GdkPixbuf * pixbuf)
{
  GdkPixbuf *pb = gdk_pixbuf_add_alpha (pixbuf, TRUE, 255, 255, 255);
  cairo_surface_t *surface =
    cairo_surface_create_similar_image (cairo_get_target
					(f->output_data.pgtk->cr_context),
					CAIRO_FORMAT_A1,
					gdk_pixbuf_get_width (pb),
					gdk_pixbuf_get_height (pb));

  cairo_t *cr = cairo_create (surface);
  gdk_cairo_set_source_pixbuf (cr, pb, 0, 0);
  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
  cairo_paint (cr);
  cairo_destroy (cr);

  cairo_pattern_t *pat = cairo_pattern_create_for_surface (surface);
  cairo_pattern_set_extend (pat, CAIRO_EXTEND_REPEAT);

  cairo_surface_destroy (surface);
  g_object_unref (pb);

  return pat;
}
#endif

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */

ptrdiff_t
image_create_bitmap_from_data (struct frame *f, char *bits,
                               unsigned int width, unsigned int height)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  ptrdiff_t id;

#ifdef HAVE_X_WINDOWS
  Pixmap bitmap;
  bitmap = XCreateBitmapFromData (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				  bits, width, height);
  if (! bitmap)
    return -1;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  Lisp_Object frame UNINIT;	/* The value is not used.  */
  Emacs_Pixmap bitmap;
  bitmap = CreateBitmap (width, height,
			 FRAME_DISPLAY_INFO (XFRAME (frame))->n_planes,
			 FRAME_DISPLAY_INFO (XFRAME (frame))->n_cbits,
			 bits);
  if (! bitmap)
    return -1;
#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
  void *bitmap = ns_image_from_XBM (bits, width, height, 0, 0);
  if (!bitmap)
      return -1;
#endif

#ifdef HAVE_PGTK
  GdkPixbuf *pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB,
				      FALSE,
				      8,
				      width,
				      height);
  {
    char *sp = bits;
    int mask = 0x01;
    unsigned char *buf = gdk_pixbuf_get_pixels (pixbuf);
    int rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    for (int y = 0; y < height; y++)
      {
	unsigned char *dp = buf + rowstride * y;
	for (int x = 0; x < width; x++)
	  {
	    if (*sp & mask)
	      {
		*dp++ = 0xff;
		*dp++ = 0xff;
		*dp++ = 0xff;
	      }
	    else
	      {
		*dp++ = 0x00;
		*dp++ = 0x00;
		*dp++ = 0x00;
	      }
	    if ((mask <<= 1) >= 0x100)
	      {
		mask = 0x01;
		sp++;
	      }
	  }
	if (mask != 0x01)
	  {
	    mask = 0x01;
	    sp++;
	  }
      }
  }
#endif /* HAVE_PGTK */

#ifdef HAVE_HAIKU
  void *bitmap, *stipple;
  int bytes_per_line, x, y;

  bitmap = BBitmap_new (width, height, false);

  if (!bitmap)
    return -1;

  bytes_per_line = (width + 7) / 8;
  stipple = xmalloc (height * bytes_per_line);
  memcpy (stipple, bits, height * bytes_per_line);

  for (y = 0; y < height; y++)
    {
      for (x = 0; x < width; x++)
	PUT_PIXEL (bitmap, x, y, ((bits[8] >> (x % 8)) & 1
				  ? f->foreground_pixel
				  : f->background_pixel));
      bits += bytes_per_line;
    }
#endif

  id = image_allocate_bitmap_record (f);

#ifdef HAVE_NS
  dpyinfo->bitmaps[id - 1].img = bitmap;
  dpyinfo->bitmaps[id - 1].depth = 1;
#endif

#ifdef HAVE_PGTK
  dpyinfo->bitmaps[id - 1].img = pixbuf;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].pattern =
    image_create_pattern_from_pixbuf (f, pixbuf);
#endif

#ifdef HAVE_HAIKU
  dpyinfo->bitmaps[id - 1].img = bitmap;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].stipple_bits = stipple;
  dpyinfo->bitmaps[id - 1].stipple_foreground
    = f->foreground_pixel & 0xffffffff;
  dpyinfo->bitmaps[id - 1].stipple_background
    = f->background_pixel & 0xffffffff;
#endif

  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  dpyinfo->bitmaps[id - 1].refcount = 1;

#ifdef HAVE_X_WINDOWS
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = false;
  dpyinfo->bitmaps[id - 1].depth = 1;
#ifdef USE_CAIRO
  dpyinfo->bitmaps[id - 1].stipple = NULL;
#endif	/* USE_CAIRO */
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].hinst = NULL;
  dpyinfo->bitmaps[id - 1].depth = 1;
#endif /* HAVE_NTGUI */

  return id;
}

#if defined HAVE_HAIKU || defined HAVE_NS
static char *slurp_file (int, ptrdiff_t *);
static Lisp_Object image_find_image_fd (Lisp_Object, int *);
static bool xbm_read_bitmap_data (struct frame *, char *, char *,
				  int *, int *, char **, bool);
#endif

/* Create bitmap from file FILE for frame F.  */

ptrdiff_t
image_create_bitmap_from_file (struct frame *f, Lisp_Object file)
{
#if defined (HAVE_NTGUI)
  return -1;  /* W32_TODO : bitmap support */
#else
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

#ifdef HAVE_NS
  ptrdiff_t id, size;
  int fd, width, height, rc;
  char *contents, *data;
  void *bitmap;

  if (!STRINGP (image_find_image_fd (file, &fd)))
    return -1;

  contents = slurp_file (fd, &size);

  if (!contents)
    return -1;

  rc = xbm_read_bitmap_data (f, contents, contents + size,
			     &width, &height, &data, 0);

  if (!rc)
    {
      xfree (contents);
      return -1;
    }

  bitmap = ns_image_from_XBM (data, width, height, 0, 0);

  if (!bitmap)
    {
      xfree (contents);
      xfree (data);
      return -1;
    }

  id = image_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].img = bitmap;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = xlispstrdup (file);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = ns_image_width (bitmap);
  dpyinfo->bitmaps[id - 1].width = ns_image_height (bitmap);

  xfree (contents);
  xfree (data);
  return id;
#endif

#ifdef HAVE_PGTK
  GError *err = NULL;
  ptrdiff_t id;
  void * bitmap = gdk_pixbuf_new_from_file (SSDATA (file), &err);

  if (!bitmap)
    {
      g_error_free (err);
      return -1;
    }

  id = image_allocate_bitmap_record (f);

  dpyinfo->bitmaps[id - 1].img = bitmap;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = xlispstrdup (file);
  dpyinfo->bitmaps[id - 1].height = gdk_pixbuf_get_width (bitmap);
  dpyinfo->bitmaps[id - 1].width = gdk_pixbuf_get_height (bitmap);
  dpyinfo->bitmaps[id - 1].pattern
    = image_create_pattern_from_pixbuf (f, bitmap);
  return id;
#endif

#ifdef HAVE_X_WINDOWS
  unsigned int width, height;
  Pixmap bitmap;
  int xhot, yhot, result;
  ptrdiff_t id;
  Lisp_Object found;
  char *filename;

  /* Look for an existing bitmap with the same name.  */
  for (id = 0; id < dpyinfo->bitmaps_last; ++id)
    {
      if (dpyinfo->bitmaps[id].refcount
	  && dpyinfo->bitmaps[id].file
	  && !strcmp (dpyinfo->bitmaps[id].file, SSDATA (file)))
	{
	  ++dpyinfo->bitmaps[id].refcount;
	  return id + 1;
	}
    }

  /* Search bitmap-file-path for the file, if appropriate.  */
  if (openp (Vx_bitmap_file_path, file, Qnil, &found,
	     make_fixnum (R_OK), false, false)
      < 0)
    return -1;

  filename = SSDATA (found);

  result = XReadBitmapFile (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
			    filename, &width, &height, &bitmap, &xhot, &yhot);
  if (result != BitmapSuccess)
    return -1;

  id = image_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = false;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = xlispstrdup (file);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
#ifdef USE_CAIRO
  dpyinfo->bitmaps[id - 1].stipple = NULL;
#endif	/* USE_CAIRO */

  return id;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_HAIKU
  ptrdiff_t id, size;
  int fd, width, height, rc, bytes_per_line, x, y;
  char *contents, *data, *tmp;
  void *bitmap;
  Lisp_Object found;

  /* Look for an existing bitmap with the same name.  */
  for (id = 0; id < dpyinfo->bitmaps_last; ++id)
    {
      if (dpyinfo->bitmaps[id].refcount
	  && dpyinfo->bitmaps[id].file
	  && !strcmp (dpyinfo->bitmaps[id].file, SSDATA (file)))
	{
	  ++dpyinfo->bitmaps[id].refcount;
	  return id + 1;
	}
    }

  /* Search bitmap-file-path for the file, if appropriate.  */
  if (openp (Vx_bitmap_file_path, file, Qnil, &found,
	     make_fixnum (R_OK), false, false)
      < 0)
    return -1;

  if (!STRINGP (image_find_image_fd (file, &fd))
      && !STRINGP (image_find_image_fd (found, &fd)))
    return -1;

  contents = slurp_file (fd, &size);

  if (!contents)
    return -1;

  rc = xbm_read_bitmap_data (f, contents, contents + size,
			     &width, &height, &data, 0);

  if (!rc)
    {
      xfree (contents);
      return -1;
    }

  bitmap = BBitmap_new (width, height, false);

  if (!bitmap)
    {
      xfree (contents);
      xfree (data);
      return -1;
    }

  id = image_allocate_bitmap_record (f);

  dpyinfo->bitmaps[id - 1].img = bitmap;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].file = xlispstrdup (file);
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].stipple_foreground
    = f->foreground_pixel & 0xffffffff;
  dpyinfo->bitmaps[id - 1].stipple_background
    = f->background_pixel & 0xffffffff;
  dpyinfo->bitmaps[id - 1].stipple_bits = data;

  bytes_per_line = (width + 7) / 8;
  tmp = data;

  for (y = 0; y < height; y++)
    {
      for (x = 0; x < width; x++)
	PUT_PIXEL (bitmap, x, y, ((tmp[x / 8] >> (x % 8)) & 1
				  ? f->foreground_pixel
				  : f->background_pixel));

      tmp += bytes_per_line;
    }

  xfree (contents);
  return id;
#endif
}

/* Free bitmap B.  */

static void
free_bitmap_record (Display_Info *dpyinfo, Bitmap_Record *bm)
{
#ifdef HAVE_X_WINDOWS
  XFreePixmap (dpyinfo->display, bm->pixmap);
  if (bm->have_mask)
    XFreePixmap (dpyinfo->display, bm->mask);
#ifdef USE_CAIRO
  if (bm->stipple)
    cairo_pattern_destroy (bm->stipple);
#endif	/* USE_CAIRO */
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
  DeleteObject (bm->pixmap);
#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
  ns_release_object (bm->img);
#endif

#ifdef HAVE_PGTK
  if (bm->pattern != NULL)
    cairo_pattern_destroy (bm->pattern);
#endif

#ifdef HAVE_HAIKU
  BBitmap_free (bm->img);

  if (bm->stipple_bits)
    xfree (bm->stipple_bits);
#endif

  if (bm->file)
    {
      xfree (bm->file);
      bm->file = NULL;
    }
}

/* Remove reference to bitmap with id number ID.  */

void
image_destroy_bitmap (struct frame *f, ptrdiff_t id)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (id > 0)
    {
      Bitmap_Record *bm = &dpyinfo->bitmaps[id - 1];

      if (--bm->refcount == 0)
	{
	  block_input ();
	  free_bitmap_record (dpyinfo, bm);
	  unblock_input ();
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

void
image_destroy_all_bitmaps (Display_Info *dpyinfo)
{
  ptrdiff_t i;
  Bitmap_Record *bm = dpyinfo->bitmaps;

  for (i = 0; i < dpyinfo->bitmaps_last; i++, bm++)
    if (bm->refcount > 0)
      free_bitmap_record (dpyinfo, bm);

  dpyinfo->bitmaps_last = 0;
}

#ifndef HAVE_XRENDER
/* Required for the definition of image_create_x_image_and_pixmap_1 below.  */
typedef void Picture;
#endif

static bool image_create_x_image_and_pixmap_1 (struct frame *, int, int, int,
                                               Emacs_Pix_Container *,
                                               Emacs_Pixmap *, Picture *);
static void image_destroy_x_image (Emacs_Pix_Container);

#ifdef HAVE_NTGUI
static HDC image_get_x_image_or_dc (struct frame *, struct image *,
                                    bool, HGDIOBJ *);
static void image_unget_x_image_or_dc (struct image *, bool,
                                       HDC, HGDIOBJ);
#else
static Emacs_Pix_Container image_get_x_image (struct frame *, struct image *,
                                              bool);
static void image_unget_x_image (struct image *, bool, Emacs_Pix_Container);
#define image_get_x_image_or_dc(f, img, mask_p, dummy)	\
  image_get_x_image (f, img, mask_p)
#define image_unget_x_image_or_dc(img, mask_p, ximg, dummy)	\
  image_unget_x_image (img, mask_p, ximg)
#endif

#ifdef HAVE_X_WINDOWS

#ifndef USE_CAIRO
static void image_sync_to_pixmaps (struct frame *, struct image *);
#endif	/* !USE_CAIRO */

/* We are working on X-specific data structures here even with cairo.
   So we use X-specific versions of image construction/destruction
   functions and inline the specific case of four_corners_best.  */

static bool x_create_x_image_and_pixmap (struct frame *, int, int, int,
					 XImage **, Pixmap *);
static void x_destroy_x_image (XImage *);

/* Create a mask of a bitmap. Note is this not a perfect mask.
   It's nicer with some borders in this context */

void
x_create_bitmap_mask (struct frame *f, ptrdiff_t id)
{
  Pixmap pixmap, mask;
  XImage *ximg, *mask_img;
  unsigned long width, height;
  bool result;
  unsigned long bg UNINIT;
  unsigned long x, y, xp, xm, yp, ym;
  GC gc;

  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (!(id > 0))
    return;

  pixmap = image_bitmap_pixmap (f, id);
  width = x_bitmap_width (f, id);
  height = x_bitmap_height (f, id);

  block_input ();
  ximg = XGetImage (FRAME_X_DISPLAY (f), pixmap, 0, 0, width, height,
		    ~0, ZPixmap);

  if (!ximg)
    {
      unblock_input ();
      return;
    }

  result = x_create_x_image_and_pixmap (f, width, height, 1, &mask_img, &mask);

  unblock_input ();
  if (!result)
    {
      XDestroyImage (ximg);
      return;
    }

  unsigned long corner_pixels[4];
  corner_pixels[0] = XGetPixel (ximg, 0, 0);
  corner_pixels[1] = XGetPixel (ximg, width - 1, 0);
  corner_pixels[2] = XGetPixel (ximg, width - 1, height - 1);
  corner_pixels[3] = XGetPixel (ximg, 0, height - 1);
  int i, best_count;
  for (i = best_count = 0; i < 4; ++i)
    {
      int j, n;

      for (j = n = 0; j < 4; ++j)
	if (corner_pixels[i] == corner_pixels[j])
	  ++n;

      if (n > best_count)
	bg = corner_pixels[i], best_count = n;
    }

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

  eassert (input_blocked_p ());
  gc = XCreateGC (FRAME_X_DISPLAY (f), mask, 0, NULL);
  XPutImage (FRAME_X_DISPLAY (f), mask, gc, mask_img, 0, 0, 0, 0,
	     width, height);
  XFreeGC (FRAME_X_DISPLAY (f), gc);

  dpyinfo->bitmaps[id - 1].have_mask = true;
  dpyinfo->bitmaps[id - 1].mask = mask;

  XDestroyImage (ximg);
  x_destroy_x_image (mask_img);
}

#endif /* HAVE_X_WINDOWS */

/***********************************************************************
			    Image types
 ***********************************************************************/

/* Each image format (JPEG, TIFF, ...) supported is described by
   a structure of the type below.  */

struct image_type
{
  /* Index of a symbol uniquely identifying the image type, e.g., 'jpeg'.  */
  int type;

  /* Check that SPEC is a valid image specification for the given
     image type.  Value is true if SPEC is valid.  */
  bool (*valid_p) (Lisp_Object spec);

  /* Load IMG which is used on frame F from information contained in
     IMG->spec.  Value is true if successful.  */
  bool (*load_img) (struct frame *f, struct image *img);

  /* Free resources of image IMG which is used on frame F.  */
  void (*free_img) (struct frame *f, struct image *img);

#ifdef WINDOWSNT
  /* Initialization function (used for dynamic loading of image
     libraries on Windows), or NULL if none.  */
  bool (*init) (void);
  /* An initializer for the init field.  */
#endif
#if defined HAVE_RSVG || defined HAVE_PNG || defined HAVE_GIF || \
  defined HAVE_TIFF || defined HAVE_JPEG || defined HAVE_XPM || \
  defined HAVE_NS || defined HAVE_HAIKU || defined HAVE_PGTK || \
  defined HAVE_WEBP
# ifdef WINDOWSNT
#  define IMAGE_TYPE_INIT(f) f
# else
#  define IMAGE_TYPE_INIT(f)
# endif
#endif
};

/* Forward function prototypes.  */

static struct image_type const *lookup_image_type (Lisp_Object);
static void image_laplace (struct frame *, struct image *);
static void image_emboss (struct frame *, struct image *);
static void image_build_heuristic_mask (struct frame *, struct image *,
                                    Lisp_Object);

static void
add_image_type (Lisp_Object type)
{
  Vimage_types = Fcons (type, Vimage_types);
}


/* Value is true if OBJECT is a valid Lisp image specification.  A
   valid image specification is a list whose car is the symbol
   `image', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported image type.  The rest of the property list depends on the
   image type.  */

bool
valid_image_p (Lisp_Object object)
{
  if (IMAGEP (object))
    {
      Lisp_Object tail = XCDR (object);
      FOR_EACH_TAIL_SAFE (tail)
	{
	  if (EQ (XCAR (tail), QCtype))
	    {
	      tail = XCDR (tail);
	      if (CONSP (tail))
		{
		  struct image_type const *type =
		    lookup_image_type (XCAR (tail));
		  if (type)
		    return type->valid_p (object);
		}
	      break;
	    }
	  tail = XCDR (tail);
	  if (! CONSP (tail))
	    return false;
	}
    }

  return false;
}

/* Log error message with format string FORMAT and trailing arguments.
   Signaling an error, e.g. when an image cannot be loaded, is not a
   good idea because this would interrupt redisplay, and the error
   message display would lead to another redisplay.  This function
   therefore simply displays a message.  */

static void
image_error (const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  vadd_to_log (format, ap);
  va_end (ap);
}

static void
image_size_error (void)
{
  image_error ("Invalid image size (see `max-image-size')");
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
  IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR,
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
  const char *name;

  /* The type of value allowed.  */
  enum image_value_type type;

  /* True means key must be present.  */
  bool mandatory_p;

  /* Used to recognize duplicate keywords in a property list.  */
  bool count;

  /* The value that was found.  */
  Lisp_Object value;
};


/* Parse image spec SPEC according to KEYWORDS.  A valid image spec
   has the format (image KEYWORD VALUE ...).  One of the keyword/
   value pairs must be `:type TYPE'.  KEYWORDS is a vector of
   image_keywords structures of size NKEYWORDS describing other
   allowed keyword/value pairs.  Value is true if SPEC is valid.  */

static bool
parse_image_spec (Lisp_Object spec, struct image_keyword *keywords,
		  int nkeywords, Lisp_Object type)
{
  int i;
  Lisp_Object plist;

  if (!IMAGEP (spec))
    return false;

  plist = XCDR (spec);
  FOR_EACH_TAIL_SAFE (plist)
    {
      Lisp_Object key, value;

      /* First element of a pair must be a symbol.  */
      key = XCAR (plist);
      plist = XCDR (plist);
      if (!SYMBOLP (key))
	return false;

      /* There must follow a value.  */
      if (!CONSP (plist))
	return false;
      value = XCAR (plist);

      /* Find key in KEYWORDS.  Error if not found.  */
      for (i = 0; i < nkeywords; ++i)
	if (strcmp (keywords[i].name, SSDATA (SYMBOL_NAME (key))) == 0)
	  break;

      if (i == nkeywords)
	goto maybe_done;

      /* Record that we recognized the keyword.  If a keyword
	 was found more than once, it's an error.  */
      keywords[i].value = value;
      if (keywords[i].count)
	return false;
      keywords[i].count = true;

      /* Check type of value against allowed type.  */
      switch (keywords[i].type)
	{
	case IMAGE_STRING_VALUE:
	  if (!STRINGP (value))
	    return false;
	  break;

	case IMAGE_STRING_OR_NIL_VALUE:
	  if (!STRINGP (value) && !NILP (value))
	    return false;
	  break;

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return false;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (! RANGED_FIXNUMP (1, value, INT_MAX))
	    return false;
	  break;

	case IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR:
	  if (RANGED_FIXNUMP (0, value, INT_MAX))
	    break;
	  if (CONSP (value)
	      && RANGED_FIXNUMP (0, XCAR (value), INT_MAX)
	      && RANGED_FIXNUMP (0, XCDR (value), INT_MAX))
	    break;
	  return false;

	case IMAGE_ASCENT_VALUE:
	  if (SYMBOLP (value) && EQ (value, Qcenter))
	    break;
	  else if (RANGED_FIXNUMP (0, value, 100))
	    break;
	  return false;

	case IMAGE_NON_NEGATIVE_INTEGER_VALUE:
	  /* Unlike the other integer-related cases, this one does not
	     verify that VALUE fits in 'int'.  This is because callers
	     want EMACS_INT.  */
	  if (!FIXNUMP (value) || XFIXNUM (value) < 0)
	    return false;
	  break;

	case IMAGE_DONT_CHECK_VALUE_TYPE:
	  break;

	case IMAGE_FUNCTION_VALUE:
	  value = indirect_function (value);
	  if (FUNCTIONP (value))
	    break;
	  return false;

	case IMAGE_NUMBER_VALUE:
	  if (! NUMBERP (value))
	    return false;
	  break;

	case IMAGE_INTEGER_VALUE:
	  if (! TYPE_RANGED_FIXNUMP (int, value))
	    return false;
	  break;

	case IMAGE_BOOL_VALUE:
	  if (!NILP (value) && !EQ (value, Qt))
	    return false;
	  break;

	default:
	  emacs_abort ();
	  break;
	}

      if (EQ (key, QCtype)
	  && !(EQ (type, value) || EQ (type, Qnative_image)))
	return false;

    maybe_done:
      if (NILP (XCDR (plist)))
	{
	  /* Check that all mandatory fields are present.  */
	  for (i = 0; i < nkeywords; ++i)
	    if (keywords[i].mandatory_p && keywords[i].count == 0)
	      return false;

	  return true;
	}
    }

  return false;
}


/* Return the value of KEY in image specification SPEC.  Value is nil
   if KEY is not present in SPEC.  Set *FOUND depending on whether KEY
   was found in SPEC.  */

static Lisp_Object
image_spec_value (Lisp_Object spec, Lisp_Object key, bool *found)
{
  Lisp_Object tail;

  eassert (valid_image_p (spec));

  tail = XCDR (spec);
  FOR_EACH_TAIL_SAFE (tail)
    {
      if (EQ (XCAR (tail), key))
	{
	  if (found)
	    *found = 1;
	  return XCAR (XCDR (tail));
	}
      tail = XCDR (tail);
      if (! CONSP (tail))
	break;
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
or omitted means use the selected frame.

Calling this function will result in the image being stored in the
image cache.  If this is not desirable, call `image-flush' after
calling this function.  */)
  (Lisp_Object spec, Lisp_Object pixels, Lisp_Object frame)
{
  Lisp_Object size;

  size = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      int width = img->width + 2 * img->hmargin;
      int height = img->height + 2 * img->vmargin;

      if (NILP (pixels))
	size = Fcons (make_float ((double) width / FRAME_COLUMN_WIDTH (f)),
		      make_float ((double) height / FRAME_LINE_HEIGHT (f)));
      else
	size = Fcons (make_fixnum (width), make_fixnum (height));
    }
  else
    error ("Invalid image specification");

  return size;
}


DEFUN ("image-mask-p", Fimage_mask_p, Simage_mask_p, 1, 2, 0,
       doc: /* Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
  (Lisp_Object spec, Lisp_Object frame)
{
  Lisp_Object mask;

  mask = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      if (img->mask)
	mask = Qt;
    }
  else
    error ("Invalid image specification");

  return mask;
}

DEFUN ("image-metadata", Fimage_metadata, Simage_metadata, 1, 2, 0,
       doc: /* Return metadata for image SPEC.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
  (Lisp_Object spec, Lisp_Object frame)
{
  Lisp_Object ext;

  ext = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      ext = img->lisp_data;
    }

  return ext;
}


/***********************************************************************
		 Image type independent image structures
 ***********************************************************************/

#define MAX_IMAGE_SIZE 10.0
/* Allocate and return a new image structure for image specification
   SPEC.  SPEC has a hash value of HASH.  */

static struct image *
make_image (Lisp_Object spec, EMACS_UINT hash)
{
  struct image *img = xzalloc (sizeof *img);
  Lisp_Object file = image_spec_value (spec, QCfile, NULL);

  eassert (valid_image_p (spec));
  img->dependencies = NILP (file) ? Qnil : list1 (file);
  img->type = lookup_image_type (image_spec_value (spec, QCtype, NULL));
  eassert (img->type != NULL);
  img->spec = spec;
  img->lisp_data = Qnil;
  img->ascent = DEFAULT_IMAGE_ASCENT;
  img->hash = hash;
  img->corners[BOT_CORNER] = -1;  /* Full image */
  return img;
}


/* Free image IMG which was used on frame F, including its resources.  */

static void
free_image (struct frame *f, struct image *img)
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

#if !defined USE_CAIRO && defined HAVE_XRENDER
      if (img->picture)
        XRenderFreePicture (FRAME_X_DISPLAY (f), img->picture);
      if (img->mask_picture)
        XRenderFreePicture (FRAME_X_DISPLAY (f), img->mask_picture);
#endif

      /* Free resources, then free IMG.  */
      img->type->free_img (f, img);
      xfree (img->face_font_family);
      xfree (img);
    }
}

/* Return true if the given widths and heights are valid for display.  */

static bool
check_image_size (struct frame *f, int width, int height)
{
  int w, h;

  if (width <= 0 || height <= 0)
    return 0;

  if (FIXNUMP (Vmax_image_size))
    return (width <= XFIXNUM (Vmax_image_size)
	    && height <= XFIXNUM (Vmax_image_size));
  else if (FLOATP (Vmax_image_size))
    {
      if (f != NULL)
	{
	  w = FRAME_PIXEL_WIDTH (f);
	  h = FRAME_PIXEL_HEIGHT (f);
	}
      else
	w = h = 1024;  /* Arbitrary size for unknown frame. */
      return (width <= XFLOAT_DATA (Vmax_image_size) * w
	      && height <= XFLOAT_DATA (Vmax_image_size) * h);
    }
  else
    return 1;
}

/* Prepare image IMG for display on frame F.  Must be called before
   drawing an image.  */

void
prepare_image_for_display (struct frame *f, struct image *img)
{
  /* We're about to display IMG, so set its timestamp to `now'.  */
  img->timestamp = current_timespec ();

  /* If IMG doesn't have a pixmap yet, load it now, using the image
     type dependent loader function.  */
  if (img->pixmap == NO_PIXMAP && !img->load_failed_p)
    img->load_failed_p = ! img->type->load_img (f, img);

#ifdef USE_CAIRO
  if (!img->load_failed_p)
    {
      block_input ();
      if (img->cr_data == NULL || (cairo_pattern_get_type (img->cr_data)
				   != CAIRO_PATTERN_TYPE_SURFACE))
	{
	  /* Fill in the background/background_transparent field while
	     we have img->pixmap->data/img->mask->data.  */
	  IMAGE_BACKGROUND (img, f, img->pixmap);
	  IMAGE_BACKGROUND_TRANSPARENT (img, f, img->mask);
	  cr_put_image_to_cr_data (img);
	  if (img->cr_data == NULL)
	    {
	      img->load_failed_p = 1;
	      img->type->free_img (f, img);
	    }
	}
      unblock_input ();
    }
#elif defined HAVE_X_WINDOWS
  if (!img->load_failed_p)
    {
      block_input ();
      image_sync_to_pixmaps (f, img);
      unblock_input ();
    }
#endif
}


/* Value is the number of pixels for the ascent of image IMG when
   drawn in face FACE.  */

int
image_ascent (struct image *img, struct face *face, struct glyph_slice *slice)
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
	  ascent = (height + FONT_BASE (face->font)
                    - FONT_DESCENT (face->font) + 1) / 2;
#endif /* HAVE_NTGUI */
	}
      else
	ascent = height / 2;
    }
  else
    ascent = height * (img->ascent / 100.0);

  return ascent;
}


/* Image background colors.  */

/* Find the "best" corner color of a bitmap.
   On W32, PIMG is assumed to a device context with the bitmap selected.  */

static RGB_PIXEL_COLOR
four_corners_best (Emacs_Pix_Context pimg, int *corners,
		   unsigned long width, unsigned long height)
{
  RGB_PIXEL_COLOR corner_pixels[4];
  RGB_PIXEL_COLOR best UNINIT;
  int i, best_count;

  if (corners && corners[BOT_CORNER] >= 0)
    {
      /* Get the colors at the corner_pixels of pimg.  */
      corner_pixels[0] = GET_PIXEL (pimg, corners[LEFT_CORNER], corners[TOP_CORNER]);
      corner_pixels[1] = GET_PIXEL (pimg, corners[RIGHT_CORNER] - 1, corners[TOP_CORNER]);
      corner_pixels[2] = GET_PIXEL (pimg, corners[RIGHT_CORNER] - 1, corners[BOT_CORNER] - 1);
      corner_pixels[3] = GET_PIXEL (pimg, corners[LEFT_CORNER], corners[BOT_CORNER] - 1);
    }
  else

    {
      /* Get the colors at the corner_pixels of pimg.  */
      corner_pixels[0] = GET_PIXEL (pimg, 0, 0);
      corner_pixels[1] = GET_PIXEL (pimg, width - 1, 0);
      corner_pixels[2] = GET_PIXEL (pimg, width - 1, height - 1);
      corner_pixels[3] = GET_PIXEL (pimg, 0, height - 1);
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

/* Return the `background' field of IMG.  If IMG doesn't have one yet,
   it is guessed heuristically.  If non-zero, XIMG is an existing
   Emacs_Pix_Context object (device context with the image selected on
   W32) to use for the heuristic.  */

RGB_PIXEL_COLOR
image_background (struct image *img, struct frame *f, Emacs_Pix_Context pimg)
{
  if (! img->background_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      bool free_pimg = !pimg;
#ifdef HAVE_NTGUI
      HGDIOBJ prev;
#endif /* HAVE_NTGUI */

      if (free_pimg)
	pimg = image_get_x_image_or_dc (f, img, 0, &prev);

      RGB_PIXEL_COLOR bg
	= four_corners_best (pimg, img->corners, img->width, img->height);
#ifdef USE_CAIRO
      {
	char color_name[30];
	sprintf (color_name, "#%04x%04x%04x",
		 (unsigned int) RED16_FROM_ULONG (bg),
		 (unsigned int) GREEN16_FROM_ULONG (bg),
		 (unsigned int) BLUE16_FROM_ULONG (bg));
	bg = image_alloc_image_color (f, img, build_string (color_name), 0);
      }
#endif
      img->background = bg;

      if (free_pimg)
	image_unget_x_image_or_dc (img, 0, pimg, prev);

      img->background_valid = 1;
    }

  return img->background;
}

/* Return the `background_transparent' field of IMG.  If IMG doesn't
   have one yet, it is guessed heuristically.  If non-zero, MASK is an
   existing Emacs_Pix_Context (XImage* on X) object to use for the
   heuristic.  */

int
image_background_transparent (struct image *img, struct frame *f,
                              Emacs_Pix_Context mask)
{
  if (! img->background_transparent_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      if (img->mask)
	{
	  bool free_mask = !mask;
#ifdef HAVE_NTGUI
	  HGDIOBJ prev;
#endif /* HAVE_NTGUI */

	  if (free_mask)
	    mask = image_get_x_image_or_dc (f, img, 1, &prev);

	  img->background_transparent
	    = (four_corners_best (mask, img->corners, img->width, img->height) == PIX_MASK_RETAIN);

	  if (free_mask)
	    image_unget_x_image_or_dc (img, 1, mask, prev);
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

/* Clear X resources of image IMG on frame F according to FLAGS.
   FLAGS is bitwise-or of the following masks:
   CLEAR_IMAGE_PIXMAP free the pixmap if any.
   CLEAR_IMAGE_MASK means clear the mask pixmap if any.
   CLEAR_IMAGE_COLORS means free colors allocated for the image, if
     any.  */

#define CLEAR_IMAGE_PIXMAP	(1 << 0)
#define CLEAR_IMAGE_MASK	(1 << 1)
#define CLEAR_IMAGE_COLORS	(1 << 2)

static void
image_clear_image_1 (struct frame *f, struct image *img, int flags)
{
  if (flags & CLEAR_IMAGE_PIXMAP)
    {
      if (img->pixmap)
	{
	  FRAME_TERMINAL (f)->free_pixmap (f, img->pixmap);
	  img->pixmap = NO_PIXMAP;
	  /* NOTE (HAVE_NS): background color is NOT an indexed color! */
	  img->background_valid = 0;
	}
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
      if (img->ximg)
	{
	  image_destroy_x_image (img->ximg);
	  img->ximg = NULL;
	  img->background_valid = 0;
	}
#endif
    }

  if (flags & CLEAR_IMAGE_MASK)
    {
      if (img->mask)
	{
	  FRAME_TERMINAL (f)->free_pixmap (f, img->mask);
	  img->mask = NO_PIXMAP;
	  img->background_transparent_valid = 0;
	}
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
      if (img->mask_img)
	{
	  image_destroy_x_image (img->mask_img);
	  img->mask_img = NULL;
	  img->background_transparent_valid = 0;
	}
#endif
    }

  if ((flags & CLEAR_IMAGE_COLORS) && img->ncolors)
    {
      /* W32_TODO: color table support.  */
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
      x_free_colors (f, img->colors, img->ncolors);
#endif /* HAVE_X_WINDOWS && !USE_CAIRO */
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }

#ifdef USE_CAIRO
  if (img->cr_data)
    {
      cairo_pattern_destroy (img->cr_data);
      img->cr_data = NULL;
    }
#endif	/* USE_CAIRO */
}

/* Free X resources of image IMG which is used on frame F.  */

static void
image_clear_image (struct frame *f, struct image *img)
{
  block_input ();
  image_clear_image_1 (f, img,
		       (CLEAR_IMAGE_PIXMAP
			| CLEAR_IMAGE_MASK
			| CLEAR_IMAGE_COLORS));
  unblock_input ();
}


/* Allocate color COLOR_NAME for image IMG on frame F.  If color
   cannot be allocated, use DFLT.  Add a newly allocated color to
   IMG->colors, so that it can be freed again.  Value is the pixel
   color.  */

static unsigned long
image_alloc_image_color (struct frame *f, struct image *img,
                         Lisp_Object color_name, unsigned long dflt)
{
  Emacs_Color color;
  unsigned long result;

  eassert (STRINGP (color_name));

  if (FRAME_TERMINAL (f)->defined_color_hook (f,
                                              SSDATA (color_name),
                                              &color,
                                              true,
                                              false)
      && img->ncolors < min (min (PTRDIFF_MAX, SIZE_MAX) / sizeof *img->colors,
			     INT_MAX))
    {
      /* This isn't called frequently so we get away with simply
	 reallocating the color vector to the needed size, here.  */
      ptrdiff_t ncolors = img->ncolors + 1;
      img->colors = xrealloc (img->colors, ncolors * sizeof *img->colors);
      img->colors[ncolors - 1] = color.pixel;
      img->ncolors = ncolors;
      result = color.pixel;
    }
  else
    result = dflt;

  return result;
}



/***********************************************************************
			     Image Cache
 ***********************************************************************/

static void cache_image (struct frame *f, struct image *img);

/* Return a new, initialized image cache that is allocated from the
   heap.  Call free_image_cache to free an image cache.  */

struct image_cache *
make_image_cache (void)
{
  struct image_cache *c = xmalloc (sizeof *c);

  c->size = 50;
  c->used = c->refcount = 0;
  c->images = xmalloc (c->size * sizeof *c->images);
  c->buckets = xzalloc (IMAGE_CACHE_BUCKETS_SIZE * sizeof *c->buckets);
  return c;
}

/* Find an image matching SPEC in the cache, and return it.  If no
   image is found, return NULL.  */
static struct image *
search_image_cache (struct frame *f, Lisp_Object spec, EMACS_UINT hash,
                    unsigned long foreground, unsigned long background,
                    int font_size, char *font_family, bool ignore_colors)
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
	&& (ignore_colors || (img->face_foreground == foreground
                              && img->face_background == background
			      && img->face_font_size == font_size
			      && (font_family
				  &&!strcmp (font_family, img->face_font_family)))))
      break;
  return img;
}


/* Filter out image elements that don't affect display, but will
   disrupt finding the image in the cache.  This should perhaps be
   user-configurable, but for now it's hard-coded (but new elements
   can be added at will).  */
static Lisp_Object
filter_image_spec (Lisp_Object spec)
{
  Lisp_Object out = Qnil;

  /* Skip past the `image' element.  */
  if (CONSP (spec))
    spec = XCDR (spec);

  while (CONSP (spec))
    {
      Lisp_Object key = XCAR (spec);
      spec = XCDR (spec);
      if (CONSP (spec))
	{
	  Lisp_Object value = XCAR (spec);
	  spec = XCDR (spec);

	  /* Some animation-related data doesn't affect display, but
	     breaks the image cache.  Filter those out.  */
	  if (!(EQ (key, QCanimate_buffer)
		|| EQ (key, QCanimate_tardiness)
		|| EQ (key, QCanimate_position)
		|| EQ (key, QCanimate_multi_frame_data)))
	    {
	      out = Fcons (value, out);
	      out = Fcons (key, out);
	    }
	}
    }
  return out;
}

/* Search frame F for an image with spec SPEC, and free it.  */

static void
uncache_image (struct frame *f, Lisp_Object spec)
{
  struct image *img;
  EMACS_UINT hash = sxhash (filter_image_spec (spec));

  /* Because the background colors are based on the current face, we
     can have multiple copies of an image with the same spec. We want
     to remove them all to ensure the user doesn't see an old version
     of the image when the face changes.  */
  while ((img = search_image_cache (f, spec, hash, 0, 0, 0, NULL, true)))
    {
      free_image (f, img);
      /* As display glyphs may still be referring to the image ID, we
	 must garbage the frame (Bug#6426).  */
      SET_FRAME_GARBAGED (f);
    }
}


/* Free image cache of frame F.  Be aware that X frames share images
   caches.  */

void
free_image_cache (struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  if (c)
    {
      ptrdiff_t i;

      /* Cache should not be referenced by any frame when freed.  */
      eassert (c->refcount == 0);

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

static void
clear_image_cache (struct frame *f, Lisp_Object filter)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);

  if (c && !f->inhibit_clear_image_cache)
    {
      ptrdiff_t i, nfreed = 0;

      /* Block input so that we won't be interrupted by a SIGIO
	 while being in an inconsistent state.  */
      block_input ();

      if (!NILP (filter))
	{
	  /* Filter image cache.  */
	  for (i = 0; i < c->used; ++i)
	    {
	      struct image *img = c->images[i];
	      if (img && (EQ (Qt, filter)
			  || !NILP (Fmember (filter, img->dependencies))))
		{
		  free_image (f, img);
		  ++nfreed;
		}
	    }
	}
      else if (FIXNUMP (Vimage_cache_eviction_delay))
	{
	  /* Free cache based on timestamp.  */
	  struct timespec old, t;
	  double delay;
	  ptrdiff_t nimages = 0;

	  for (i = 0; i < c->used; ++i)
	    if (c->images[i])
	      nimages++;

	  /* If the number of cached images has grown unusually large,
	     decrease the cache eviction delay (Bug#6230).  */
	  delay = XFIXNUM (Vimage_cache_eviction_delay);
	  if (nimages > 40)
	    delay = 1600 * delay / nimages / nimages;
	  delay = max (delay, 1);

	  t = current_timespec ();
	  old = timespec_sub (t, dtotimespec (delay));

	  for (i = 0; i < c->used; ++i)
	    {
	      struct image *img = c->images[i];
	      if (img && timespec_cmp (img->timestamp, old) < 0)
		{
		  free_image (f, img);
		  ++nfreed;
		}
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
	      struct frame *fr = XFRAME (frame);
	      if (FRAME_IMAGE_CACHE (fr) == c)
		clear_current_matrices (fr);
	    }

	  windows_or_buffers_changed = 19;
	}

      unblock_input ();
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
       0, 2, 0,
       doc: /* Clear the image cache.
FILTER nil or a frame means clear all images in the selected frame.
FILTER t means clear the image caches of all frames.
Anything else means clear only those images that refer to FILTER,
which is then usually a filename.

This function also clears the image animation cache.  If
ANIMATION-CACHE is non-nil, only the image spec `eq' with
ANIMATION-CACHE is removed, and other image cache entries are not
evicted.  */)
  (Lisp_Object filter, Lisp_Object animation_cache)
{
  if (!NILP (animation_cache))
    {
#if defined (HAVE_WEBP) || defined (HAVE_GIF)
      anim_prune_animation_cache (XCDR (animation_cache));
#endif
      return Qnil;
    }

  if (! (NILP (filter) || FRAMEP (filter)))
    clear_image_caches (filter);
  else
    clear_image_cache (decode_window_system_frame (filter), Qt);

  /* Also clear the animation caches.  */
  image_prune_animation_caches (true);

  return Qnil;
}

static size_t
image_size_in_bytes (struct image *img)
{
  size_t size = 0;

#if defined USE_CAIRO
  Emacs_Pixmap pm = img->pixmap;
  if (pm)
    size += pm->height * pm->bytes_per_line;
  Emacs_Pixmap msk = img->mask;
  if (msk)
    size += msk->height * msk->bytes_per_line;

#elif defined HAVE_X_WINDOWS
  /* Use a nominal depth of 24 bpp for pixmap and 1 bpp for mask,
     to avoid having to query the server. */
  if (img->pixmap != NO_PIXMAP)
    size += img->width * img->height * 3;
  if (img->mask != NO_PIXMAP)
    size += img->width * img->height / 8;

  if (img->ximg && img->ximg->data)
    size += img->ximg->bytes_per_line * img->ximg->height;
  if (img->mask_img && img->mask_img->data)
    size += img->mask_img->bytes_per_line * img->mask_img->height;

#elif defined HAVE_NS
  if (img->pixmap)
    size += ns_image_size_in_bytes (img->pixmap);
  if (img->mask)
    size += ns_image_size_in_bytes (img->mask);

#elif defined HAVE_NTGUI
  if (img->pixmap)
    size += w32_image_size (img->pixmap);
  if (img->mask)
    size += w32_image_size (img->mask);

#elif defined HAVE_HAIKU
  if (img->pixmap)
    size += BBitmap_bytes_length (img->pixmap);
  if (img->mask)
    size += BBitmap_bytes_length (img->mask);
#endif

  return size;
}

static size_t
image_frame_cache_size (struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  if (!c)
    return 0;

  size_t total = 0;
  for (ptrdiff_t i = 0; i < c->used; ++i)
    {
      struct image *img = c->images[i];
      total += img ? image_size_in_bytes (img) : 0;
    }
  return total;
}

DEFUN ("image-flush", Fimage_flush, Simage_flush,
       1, 2, 0,
       doc: /* Flush the image with specification SPEC on frame FRAME.
This removes the image from the Emacs image cache.  If SPEC specifies
an image file, the next redisplay of this image will read from the
current contents of that file.

FRAME nil or omitted means use the selected frame.
FRAME t means refresh the image on all frames.  */)
  (Lisp_Object spec, Lisp_Object frame)
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
    uncache_image (decode_window_system_frame (frame), spec);

  return Qnil;
}


/* Compute masks and transform image IMG on frame F, as specified
   by the image's specification,  */

static void
postprocess_image (struct frame *f, struct image *img)
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
	image_build_heuristic_mask (f, img, mask);
      else
	{
	  bool found_p;

	  mask = image_spec_value (spec, QCmask, &found_p);

	  if (EQ (mask, Qheuristic))
	    image_build_heuristic_mask (f, img, Qt);
	  else if (CONSP (mask)
		   && EQ (XCAR (mask), Qheuristic))
	    {
	      if (CONSP (XCDR (mask)))
		image_build_heuristic_mask (f, img, XCAR (XCDR (mask)));
	      else
		image_build_heuristic_mask (f, img, XCDR (mask));
	    }
	  else if (NILP (mask) && found_p && img->mask)
	    image_clear_image_1 (f, img, CLEAR_IMAGE_MASK);
	}


      /* Should we apply an image transformation algorithm?  */
      conversion = image_spec_value (spec, QCconversion, NULL);
      if (EQ (conversion, Qdisabled))
	image_disable_image (f, img);
      else if (EQ (conversion, Qlaplace))
	image_laplace (f, img);
      else if (EQ (conversion, Qemboss))
	image_emboss (f, img);
      else if (CONSP (conversion)
	       && EQ (XCAR (conversion), Qedge_detection))
	{
	  Lisp_Object tem;
	  tem = XCDR (conversion);
	  if (CONSP (tem))
	    image_edge_detection (f, img,
                                  plist_get (tem, QCmatrix),
                                  plist_get (tem, QCcolor_adjustment));
	}
    }
}

#if defined (HAVE_IMAGEMAGICK) || defined (HAVE_NATIVE_TRANSFORMS)
/* Scale an image size by returning SIZE / DIVISOR * MULTIPLIER,
   safely rounded and clipped to int range.  */

static int
scale_image_size (int size, double divisor, double multiplier)
{
  if (divisor != 0)
    {
      double scaled = size * multiplier / divisor;
      if (scaled < INT_MAX)
	{
	  /* Use ceil, as rounding can discard fractional SVG pixels.  */
	  return ceil (scaled);
	}
    }
  return INT_MAX;
}

/* Return a size, in pixels, from the value specified by SYMBOL, which
   may be an integer or a pair of the form (VALUE . 'em) where VALUE
   is a float that is multiplied by the font size to get the final
   dimension.

   If the value doesn't exist in the image spec, or is invalid, return
   -1.
*/
static int
image_get_dimension (struct image *img, Lisp_Object symbol)
{
  Lisp_Object value = image_spec_value (img->spec, symbol, NULL);

  if (FIXNATP (value))
    return min (XFIXNAT (value), INT_MAX);
  if (CONSP (value) && NUMBERP (CAR (value)) && EQ (Qem, CDR (value)))
    return scale_image_size (img->face_font_size, 1, XFLOATINT (CAR (value)));

  return -1;
}

/* Compute the desired size of an image with native size WIDTH x HEIGHT.
   Use IMG to deduce the size.  Store the desired size into
   *D_WIDTH x *D_HEIGHT.  Store -1 x -1 if the native size is OK.  */
static void
compute_image_size (double width, double height,
		    struct image *img,
		    int *d_width, int *d_height)
{
  double scale = 1;
  Lisp_Object value = image_spec_value (img->spec, QCscale, NULL);
  if (NUMBERP (value))
    {
      double dval = XFLOATINT (value);
      if (0 <= dval)
	scale = dval;
    }

  /* If width and/or height is set in the display spec assume we want
     to scale to those values.  If either h or w is unspecified, the
     unspecified should be calculated from the specified to preserve
     aspect ratio.  */
  int desired_width = image_get_dimension (img, QCwidth), max_width;
  if (desired_width < 0)
    max_width = image_get_dimension (img, QCmax_width);
  else
    {
      desired_width = scale_image_size (desired_width, 1, scale);
      /* :width overrides :max-width. */
      max_width = -1;
    }

  int desired_height = image_get_dimension (img, QCheight), max_height;
  if (desired_height < 0)
    max_height = image_get_dimension (img, QCmax_height);
  else
    {
      desired_height = scale_image_size (desired_height, 1, scale);
      /* :height overrides :max-height. */
      max_height = -1;
    }

  /* If we have both width/height set explicitly, we skip past all the
     aspect ratio-preserving computations below. */
  if (0 <= desired_width && 0 <= desired_height)
    goto out;

  if (0 <= desired_width)
    /* Width known, calculate height. */
    desired_height = scale_image_size (desired_width, width, height);
  else if (0 <= desired_height)
    /* Height known, calculate width. */
    desired_width = scale_image_size (desired_height, height, width);
  else
    {
      desired_width = scale_image_size (width, 1, scale);
      desired_height = scale_image_size (height, 1, scale);
    }

  if (0 <= max_width && max_width < desired_width)
    {
      /* The image is wider than :max-width. */
      desired_width = max_width;
      desired_height = scale_image_size (desired_width, width, height);
    }

  if (0 <= max_height && max_height < desired_height)
    {
      /* The image is higher than :max-height. */
      desired_height = max_height;
      desired_width = scale_image_size (desired_height, height, width);
    }

 out:
  *d_width = desired_width;
  *d_height = desired_height;
}

/* image_set_rotation and image_set_transform use affine
   transformation matrices to perform various transforms on the image.
   The matrix is a 2D array of doubles.  It is laid out like this:

   m[0][0] = m11 | m[1][0] = m12 | m[2][0] = tx
   --------------+---------------+-------------
   m[0][1] = m21 | m[1][1] = m22 | m[2][1] = ty
   --------------+---------------+-------------
   m[0][2] = 0   | m[1][2] = 0   | m[2][2] = 1

   tx and ty represent translations, m11 and m22 represent scaling
   transforms and m21 and m12 represent shear transforms.  Most
   graphics toolkits don't require the third row, however it is
   necessary for multiplication.

   Transforms are done by creating a matrix for each action we wish to
   take, then multiplying the transformation matrix by each of those
   matrices in order (matrix multiplication is not commutative).
   After we've done that we can use our modified transformation matrix
   to transform points.  We take the x and y coordinates and convert
   them into a 3x1 matrix and multiply that by the transformation
   matrix and it gives us a new, transformed, set of coordinates:

       [m11 m12 tx]   [x]   [m11*x+m12*y+tx*1]   [x']
       [m21 m22 ty] X [y] = [m21*x+m22*y+ty*1] = [y']
       [  0   0  1]   [1]   [     0*x+0*y+1*1]   [ 1]

   We don't have to worry about the last step as the graphics toolkit
   will do it for us.

   The three transforms we are concerned with are translation, scaling
   and rotation.  The translation matrix looks like this:

       [1 0 tx]
       [0 1 ty]
       [0 0  1]

   Where tx and ty are the amount to translate the origin in the x and
   y coordinates, respectively.  Since we are translating the origin
   and not the image data itself, it can appear backwards in use, for
   example to move the image 10 pixels to the right, you would set tx
   to -10.

   To scale we use:

       [x 0 0]
       [0 y 0]
       [0 0 1]

   Where x and y are the amounts to scale in the x and y dimensions.
   Values smaller than 1 make the image larger, values larger than 1
   make it smaller.  Negative values flip the image.  For example to
   double the image size set x and y to 0.5.

   To rotate we use:

       [ cos(r) sin(r) 0]
       [-sin(r) cos(r) 0]
       [      0      0 1]

   Where r is the angle of rotation required.  Rotation occurs around
   the origin, not the center of the image.  Note that this is
   normally considered a counter-clockwise rotation, however because
   our y axis is reversed, (0, 0) at the top left, it works as a
   clockwise rotation.

   The full process of rotating an image is to move the origin to the
   center of the image (width/2, height/2), perform the rotation, and
   finally move the origin back to the top left of the image, which
   may now be a different corner.

   Note that different GUI backends (X, Cairo, w32, NS, Haiku) want
   the transform matrix defined as transform from the original image
   to the transformed image, while others want the matrix to describe
   the transform of the space, which boils down to inverting the
   matrix.

   It's possible to pre-calculate the matrix multiplications and just
   generate one transform matrix that will do everything we need in a
   single step, but the maths for each element is much more complex
   and performing the steps separately makes for more readable code.  */

typedef double matrix3x3[3][3];

static void
matrix3x3_mult (matrix3x3 a, matrix3x3 b, matrix3x3 result)
{
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      {
	double sum = 0;
	for (int k = 0; k < 3; k++)
	  sum += a[i][k] * b[k][j];
	result[i][j] = sum;
      }
}

static void
compute_image_rotation (struct image *img, double *rotation)
{
  bool foundp = false;
  Lisp_Object value = image_spec_value (img->spec, QCrotation, &foundp);
  if (!foundp)
    return;
  if (! NUMBERP (value))
    {
      image_error ("Invalid image `:rotation' parameter");
      return;
    }

  Lisp_Object reduced_angle = Fmod (value, make_fixnum (360));
  if (FLOATP (reduced_angle))
    *rotation = XFLOAT_DATA (reduced_angle);
  else
    *rotation = XFIXNUM (reduced_angle);
}

static void
image_set_transform (struct frame *f, struct image *img)
{
  bool flip;

#if defined HAVE_HAIKU
  matrix3x3 identity = {
    { 1, 0, 0 },
    { 0, 1, 0 },
    { 0, 0, 1 },
  };

  img->original_width = img->width;
  img->original_height = img->height;
  img->use_bilinear_filtering = false;

  memcpy (&img->transform, identity, sizeof identity);
#endif

# if (defined HAVE_IMAGEMAGICK \
      && !defined DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE)
  /* ImageMagick images already have the correct transform.  */
  if (EQ (image_spec_value (img->spec, QCtype, NULL), Qimagemagick))
    return;
# endif

# if !defined USE_CAIRO && defined HAVE_XRENDER
  if (!img->picture)
    return;

  /* Store the original dimensions as we'll overwrite them later.  */
  img->original_width = img->width;
  img->original_height = img->height;
# endif

  /* Determine size.  */
  int width, height;

#ifdef HAVE_RSVG
  /* SVGs are pre-scaled to the correct size.  */
  if (EQ (image_spec_value (img->spec, QCtype, NULL), Qsvg))
    {
      width = img->width / FRAME_SCALE_FACTOR (f);
      height = img->height / FRAME_SCALE_FACTOR (f);
    }
  else
#endif
    compute_image_size (img->width, img->height, img, &width, &height);

  /* Determine rotation.  */
  double rotation = 0.0;
  compute_image_rotation (img, &rotation);

  /* Determine flipping.  */
  flip = !NILP (image_spec_value (img->spec, QCflip, NULL));

# if defined USE_CAIRO || defined HAVE_XRENDER || defined HAVE_NS || defined HAVE_HAIKU
  /* We want scale up operations to use a nearest neighbor filter to
     show real pixels instead of munging them, but scale down
     operations to use a blended filter, to avoid aliasing and the like.

     TODO: implement for Windows.  */
  bool smoothing;
  Lisp_Object s = image_spec_value (img->spec, QCtransform_smoothing, NULL);
  if (NILP (s))
    smoothing = (width < img->width) || (height < img->height);
  else
    smoothing = !NILP (s);
# endif

#ifdef HAVE_HAIKU
  img->use_bilinear_filtering = smoothing;
#endif

  /* Perform scale transformation.  */

  matrix3x3 matrix
    = {
# if defined USE_CAIRO || defined HAVE_XRENDER
	[0][0] = (!IEEE_FLOATING_POINT && width == 0 ? DBL_MAX
		  : img->width / (double) width),
	[1][1] = (!IEEE_FLOATING_POINT && height == 0 ? DBL_MAX
		  : img->height / (double) height),
# elif defined HAVE_NTGUI || defined HAVE_NS || defined HAVE_HAIKU
	[0][0] = (!IEEE_FLOATING_POINT && img->width == 0 ? DBL_MAX
		  : width / (double) img->width),
	[1][1] = (!IEEE_FLOATING_POINT && img->height == 0 ? DBL_MAX
		  : height / (double) img->height),
# else
	[0][0] = 1, [1][1] = 1,
# endif
	[2][2] = 1 };
  img->width = width;
  img->height = height;

  /* Perform rotation transformation.  */

  int rotate_flag = -1;

  /* Haiku needs this, since the transformation is done on the basis
     of the view, and not the image.  */
#ifdef HAVE_HAIKU
  int extra_tx, extra_ty;

  extra_tx = 0;
  extra_ty = 0;
#endif

  if (rotation == 0 && !flip)
    rotate_flag = 0;
  else
    {
# if (defined USE_CAIRO || defined HAVE_XRENDER \
      || defined HAVE_NTGUI || defined HAVE_NS \
      || defined HAVE_HAIKU)
      int cos_r, sin_r;
      if (rotation == 0)
	{
	  /* FLIP is always true here.  As this will rotate by 0
	     degrees, it has no visible effect.  Applying only
	     translation matrix to the image would be sufficient for
	     horizontal flipping, but writing special handling for
	     this case would increase code complexity somewhat.  */
	  cos_r = 1;
	  sin_r = 0;
	  rotate_flag = 1;

#ifdef HAVE_HAIKU
	  extra_tx = width;
	  extra_ty = 0;
#endif
	}
      else if (rotation == 90)
	{
	  width = img->height;
	  height = img->width;
	  cos_r = 0;
	  sin_r = 1;
	  rotate_flag = 1;

#ifdef HAVE_HAIKU
	  if (!flip)
	    extra_ty = height;
	  extra_tx = 0;
#endif
	}
      else if (rotation == 180)
	{
	  cos_r = -1;
	  sin_r = 0;
	  rotate_flag = 1;

#ifdef HAVE_HAIKU
	  if (!flip)
	    extra_tx = width;
	  extra_ty = height;
#endif
	}
      else if (rotation == 270)
	{
	  width = img->height;
	  height = img->width;
	  cos_r = 0;
	  sin_r = -1;
	  rotate_flag = 1;

#ifdef HAVE_HAIKU
	  extra_tx = width;

	  if (flip)
	    extra_ty = height;
#endif
	}

      if (0 < rotate_flag)
	{
#  if defined USE_CAIRO || defined HAVE_XRENDER
	  /* 1. Translate so (0, 0) is in the center of the image.  */
	  matrix3x3 t
	    = { [0][0] = 1,
					[1][1] = 1,
		[2][0] = img->width*.5, [2][1] = img->height*.5, [2][2] = 1 };
	  matrix3x3 u;
	  matrix3x3_mult (t, matrix, u);

	  /* 2. Rotate.  */
	  matrix3x3 rot = { [0][0] = cos_r, [0][1] = -sin_r,
			    [1][0] = sin_r, [1][1] = cos_r,
							     [2][2] = 1 };
	  matrix3x3 v;
	  matrix3x3_mult (rot, u, v);

	  /* 3. Translate back.  Flip horizontally if requested.  */
	  t[2][0] = width * -.5;
	  t[2][1] = height * -.5;
	  if (flip)
	    {
	      t[0][0] = -t[0][0];
	      t[2][0] = -t[2][0];
	    }
	  matrix3x3_mult (t, v, matrix);
#  else
	  /* 1. Translate so (0, 0) is in the center of the image.  */
	  matrix3x3 t
	    = { [0][0] = 1,
					 [1][1] = 1,
		[2][0] = img->width*-.5, [2][1] = img->height*-.5, [2][2] = 1 };
	  matrix3x3 u;
	  matrix3x3_mult (matrix, t, u);

	  /* 2. Rotate.  */
	  matrix3x3 rot = { [0][0] = cos_r,  [0][1] = sin_r,
			    [1][0] = -sin_r, [1][1] = cos_r,
							     [2][2] = 1 };
	  matrix3x3 v;
	  matrix3x3_mult (u, rot, v);

	  /* 3. Translate back.  Flip horizontally if requested.  */
	  t[2][0] = width * .5;
	  t[2][1] = height * .5;
	  if (flip) t[0][0] = -t[0][0];
	  matrix3x3_mult (v, t, matrix);
#  endif
	  img->width = width;
	  img->height = height;
	}
# endif
    }

  if (rotate_flag < 0)
    image_error ("No native support for rotation by %g degrees",
		 make_float (rotation));

# if defined (HAVE_NS)
  /* Under NS the transform is applied to the drawing surface at
     drawing time, so store it for later.  */
  ns_image_set_transform (img->pixmap, matrix);
  ns_image_set_smoothing (img->pixmap, smoothing);
# elif defined USE_CAIRO
  cairo_matrix_t cr_matrix = {matrix[0][0], matrix[0][1], matrix[1][0],
			      matrix[1][1], matrix[2][0], matrix[2][1]};
  cairo_pattern_t *pattern = cairo_pattern_create_rgb (0, 0, 0);
  cairo_pattern_set_matrix (pattern, &cr_matrix);
  cairo_pattern_set_filter (pattern, smoothing
                            ? CAIRO_FILTER_BEST : CAIRO_FILTER_NEAREST);
  /* Dummy solid color pattern just to record pattern matrix.  */
  img->cr_data = pattern;
# elif defined (HAVE_XRENDER)
  if (img->picture)
    {
      XTransform tmat
	= {{{XDoubleToFixed (matrix[0][0]),
             XDoubleToFixed (matrix[1][0]),
             XDoubleToFixed (matrix[2][0])},
	    {XDoubleToFixed (matrix[0][1]),
             XDoubleToFixed (matrix[1][1]),
             XDoubleToFixed (matrix[2][1])},
	    {XDoubleToFixed (matrix[0][2]),
             XDoubleToFixed (matrix[1][2]),
             XDoubleToFixed (matrix[2][2])}}};

      XRenderSetPictureFilter (FRAME_X_DISPLAY (f), img->picture,
                               smoothing ? FilterBest : FilterNearest, 0, 0);
      XRenderSetPictureTransform (FRAME_X_DISPLAY (f), img->picture, &tmat);

      if (img->mask_picture)
        {
          XRenderSetPictureFilter (FRAME_X_DISPLAY (f), img->mask_picture,
                                   smoothing ? FilterBest : FilterNearest, 0, 0);
          XRenderSetPictureTransform (FRAME_X_DISPLAY (f), img->mask_picture,
                                      &tmat);
        }
    }
# elif defined HAVE_NTGUI
  /* Store the transform matrix for application at draw time.  */
  img->xform.eM11 = matrix[0][0];
  img->xform.eM12 = matrix[0][1];
  img->xform.eM21 = matrix[1][0];
  img->xform.eM22 = matrix[1][1];
  img->xform.eDx  = matrix[2][0];
  img->xform.eDy  = matrix[2][1];
# elif defined HAVE_HAIKU
  /* Store the transform in the struct image for later.  */
  memcpy (&img->transform, &matrix, sizeof matrix);

  /* Also add the extra translations.   */
  if (rotate_flag)
    {
      img->transform[0][2] = extra_tx;
      img->transform[1][2] = extra_ty;
    }
#endif
}

#endif /* HAVE_IMAGEMAGICK || HAVE_NATIVE_TRANSFORMS */

/* Return the id of image with Lisp specification SPEC on frame F.
   SPEC must be a valid Lisp image specification (see valid_image_p).  */

ptrdiff_t
lookup_image (struct frame *f, Lisp_Object spec, int face_id)
{
  struct image *img;
  EMACS_UINT hash;

  if (FRAME_FACE_CACHE (f) == NULL)
    init_frame_faces (f);
  if (FRAME_FACE_CACHE (f)->used == 0)
    recompute_basic_faces (f);
  if (face_id < 0 || face_id >= FRAME_FACE_CACHE (f)->used)
    face_id = DEFAULT_FACE_ID;

  struct face *face = FACE_FROM_ID (f, face_id);
  unsigned long foreground = face->foreground;
  unsigned long background = face->background;
  int font_size = face->font->pixel_size;
  char *font_family = SSDATA (face->lface[LFACE_FAMILY_INDEX]);

  /* F must be a window-system frame, and SPEC must be a valid image
     specification.  */
  eassert (FRAME_WINDOW_P (f));
  eassert (valid_image_p (spec));

  /* Look up SPEC in the hash table of the image cache.  */
  hash = sxhash (filter_image_spec (spec));
  img = search_image_cache (f, spec, hash, foreground, background,
			    font_size, font_family, false);
  if (img && img->load_failed_p)
    {
      free_image (f, img);
      img = NULL;
    }

  /* If not found, create a new image and cache it.  */
  if (img == NULL)
    {
      block_input ();
      img = make_image (spec, hash);
      cache_image (f, img);
      img->face_foreground = foreground;
      img->face_background = background;
      img->face_font_size = font_size;
      img->face_font_family = xmalloc (strlen (font_family) + 1);
      strcpy (img->face_font_family, font_family);
      img->load_failed_p = ! img->type->load_img (f, img);

      /* If we can't load the image, and we don't have a width and
	 height, use some arbitrary width and height so that we can
	 draw a rectangle for it.  */
      if (img->load_failed_p)
	{
	  Lisp_Object value;

	  value = image_spec_value (spec, QCwidth, NULL);
	  img->width = (FIXNUMP (value)
			? XFIXNAT (value) : DEFAULT_IMAGE_WIDTH);
	  value = image_spec_value (spec, QCheight, NULL);
	  img->height = (FIXNUMP (value)
			 ? XFIXNAT (value) : DEFAULT_IMAGE_HEIGHT);
	}
      else
	{
	  /* Handle image type independent image attributes
	     `:ascent ASCENT', `:margin MARGIN', `:relief RELIEF',
	     `:background COLOR'.  */
	  Lisp_Object ascent, margin, relief, bg;
	  int relief_bound;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (FIXNUMP (ascent))
	    img->ascent = XFIXNUM (ascent);
	  else if (EQ (ascent, Qcenter))
	    img->ascent = CENTERED_IMAGE_ASCENT;

	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (FIXNUMP (margin))
	    img->vmargin = img->hmargin = XFIXNUM (margin);
	  else if (CONSP (margin))
	    {
	      img->hmargin = XFIXNUM (XCAR (margin));
	      img->vmargin = XFIXNUM (XCDR (margin));
	    }

	  relief = image_spec_value (spec, QCrelief, NULL);
	  relief_bound = INT_MAX - max (img->hmargin, img->vmargin);
	  if (RANGED_FIXNUMP (- relief_bound, relief, relief_bound))
	    {
	      img->relief = XFIXNUM (relief);
	      img->hmargin += eabs (img->relief);
	      img->vmargin += eabs (img->relief);
	    }

	  if (! img->background_valid)
	    {
	      bg = image_spec_value (img->spec, QCbackground, NULL);
	      if (!NILP (bg))
		{
		  img->background
		    = image_alloc_image_color (f, img, bg, background);
		  img->background_valid = 1;
		}
	    }

	  /* Do image transformations and compute masks, unless we
	     don't have the image yet.  */
	  if (!EQ (builtin_lisp_symbol (img->type->type), Qpostscript))
	    postprocess_image (f, img);

          /* postprocess_image above may modify the image or the mask,
             relying on the image's real width and height, so
             image_set_transform must be called after it.  */
#ifdef HAVE_NATIVE_TRANSFORMS
          image_set_transform (f, img);
#endif
	}

      unblock_input ();
    }

  /* IMG is now being used, so set its timestamp to the current
     time.  */
  img->timestamp = current_timespec ();

  /* Value is the image id.  */
  return img->id;
}


/* Cache image IMG in the image cache of frame F.  */

static void
cache_image (struct frame *f, struct image *img)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  ptrdiff_t i;

  if (!c)
    c = FRAME_IMAGE_CACHE (f) = make_image_cache ();

  /* Find a free slot in c->images.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i] == NULL)
      break;

  /* If no free slot found, maybe enlarge c->images.  */
  if (i == c->used && c->used == c->size)
    c->images = xpalloc (c->images, &c->size, 1, -1, sizeof *c->images);

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


#if defined (HAVE_WEBP) || defined (HAVE_GIF)

/* To speed animations up, we keep a cache (based on EQ-ness of the
   image spec/object) where we put the animator iterator.  */

struct anim_cache
{
  Lisp_Object spec;
  /* For webp, this will be an iterator, and for libgif, a gif handle.  */
  void *handle;
  /* If we need to maintain temporary data of some sort.  */
  void *temp;
  /* A function to call to free the handle.  */
  void (*destructor) (void *);
  int index, width, height, frames;
  /* This is used to be able to say something about the cache size.
     We don't actually know how much memory the different libraries
     actually use here (since these cache structures are opaque), so
     this is mostly just the size of the original image file.  */
  int byte_size;
  struct timespec update_time;
  struct anim_cache *next;
};

static struct anim_cache *anim_cache = NULL;

static struct anim_cache *
anim_create_cache (Lisp_Object spec)
{
  struct anim_cache *cache = xmalloc (sizeof (struct anim_cache));
  cache->handle = NULL;
  cache->temp = NULL;

  cache->index = -1;
  cache->next = NULL;
  cache->spec = spec;
  cache->byte_size = 0;
  return cache;
}

/* Discard cached images that haven't been used for a minute.  If
   CLEAR is t, remove all animation cache entries.  If CLEAR is
   anything other than nil or t, only remove the entries that have a
   spec `eq' to CLEAR.  */
static void
anim_prune_animation_cache (Lisp_Object clear)
{
  struct anim_cache **pcache = &anim_cache;
  struct timespec old = timespec_sub (current_timespec (),
				      make_timespec (60, 0));

  while (*pcache)
    {
      struct anim_cache *cache = *pcache;
      if (EQ (clear, Qt)
	  || (EQ (clear, Qnil) && timespec_cmp (old, cache->update_time) > 0)
	  || EQ (clear, cache->spec))
	{
	  if (cache->handle)
	    cache->destructor (cache);
	  if (cache->temp)
	    xfree (cache->temp);
	  *pcache = cache->next;
	  xfree (cache);
	}
      else
	pcache = &cache->next;
    }
}

static struct anim_cache *
anim_get_animation_cache (Lisp_Object spec)
{
  struct anim_cache *cache;
  struct anim_cache **pcache = &anim_cache;

  anim_prune_animation_cache (Qnil);

  while (1)
    {
      cache = *pcache;
      if (! cache)
	{
          *pcache = cache = anim_create_cache (spec);
          break;
        }
      if (EQ (spec, cache->spec))
	break;
      pcache = &cache->next;
    }

  cache->update_time = current_timespec ();
  return cache;
}

#endif  /* HAVE_WEBP || HAVE_GIF */

/* Call FN on every image in the image cache of frame F.  Used to mark
   Lisp Objects in the image cache.  */

/* Mark Lisp objects in image IMG.  */

static void
mark_image (struct image *img)
{
  mark_object (img->spec);
  mark_object (img->dependencies);

  if (!NILP (img->lisp_data))
    mark_object (img->lisp_data);
}


void
mark_image_cache (struct image_cache *c)
{
  if (c)
    {
      ptrdiff_t i;
      for (i = 0; i < c->used; ++i)
	if (c->images[i])
	  mark_image (c->images[i]);
    }

#if defined HAVE_WEBP || defined HAVE_GIF
  for (struct anim_cache *cache = anim_cache; cache; cache = cache->next)
    mark_object (cache->spec);
#endif
}



/***********************************************************************
			  X / NS / W32 support code
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS
static bool
x_check_image_size (XImage *ximg, int width, int height)
{
  /* Respect Xlib's limits: it cannot deal with images that have more
     than INT_MAX (and/or UINT_MAX) bytes.  And respect Emacs's limits
     of PTRDIFF_MAX (and/or SIZE_MAX) bytes for any object.  */
  enum
  {
    XLIB_BYTES_MAX = min (INT_MAX, UINT_MAX),
    X_IMAGE_BYTES_MAX = min (XLIB_BYTES_MAX, min (PTRDIFF_MAX, SIZE_MAX))
  };

  int bitmap_pad, depth, bytes_per_line;
  if (ximg)
    {
      bitmap_pad = ximg->bitmap_pad;
      depth = ximg->depth;
      bytes_per_line = ximg->bytes_per_line;
    }
  else
    {
      bitmap_pad = 8;
      depth = 1;
      bytes_per_line = (width >> 3) + ((width & 7) != 0);
    }
  return (width <= (INT_MAX - (bitmap_pad - 1)) / depth
	  && height <= X_IMAGE_BYTES_MAX / bytes_per_line);
}

static bool
x_create_x_image_and_pixmap (struct frame *f, int width, int height, int depth,
			     XImage **ximg, Pixmap *pixmap)
{
  Display *display = FRAME_X_DISPLAY (f);
  Drawable drawable = FRAME_X_DRAWABLE (f);

  eassert (input_blocked_p ());

  if (depth <= 0)
    depth = FRAME_DISPLAY_INFO (f)->n_planes;
  *ximg = XCreateImage (display, FRAME_X_VISUAL (f),
			depth, ZPixmap, 0, NULL, width, height,
			depth > 16 ? 32 : depth > 8 ? 16 : 8, 0);
  if (*ximg == NULL)
    {
      image_error ("Unable to allocate X image");
      return 0;
    }

  if (! x_check_image_size (*ximg, width, height))
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Image too large (%dx%d)",
		   make_fixnum (width), make_fixnum (height));
      return 0;
    }

  /* Allocate image raster.  */
  (*ximg)->data = xmalloc ((*ximg)->bytes_per_line * height);

  /* Allocate a pixmap of the same size.  */
  *pixmap = XCreatePixmap (display, drawable, width, height, depth);
  if (*pixmap == NO_PIXMAP)
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Unable to create X pixmap");
      return 0;
    }

  return 1;
}

static void
x_destroy_x_image (XImage *ximg)
{
  if (ximg)
    {
      xfree (ximg->data);
      ximg->data = NULL;
    }

  XDestroyImage (ximg);
}

# if !defined USE_CAIRO && defined HAVE_XRENDER
/* Create and return an XRender Picture for XRender transforms.  */
static Picture
x_create_xrender_picture (struct frame *f, Emacs_Pixmap pixmap, int depth)
{
  Picture p;
  Display *display = FRAME_X_DISPLAY (f);

  if (FRAME_DISPLAY_INFO (f)->xrender_supported_p)
    {
      if (depth <= 0)
	depth = FRAME_DISPLAY_INFO (f)->n_planes;
      if (depth == 32 || depth == 24 || depth == 8 || depth == 4 || depth == 1)
        {
          /* FIXME: Do we need to handle all possible bit depths?
             XRenderFindStandardFormat supports PictStandardARGB32,
             PictStandardRGB24, PictStandardA8, PictStandardA4,
             PictStandardA1, and PictStandardNUM (what is this?!).

             XRenderFindFormat may support more, but I don't
             understand the documentation.  */
          XRenderPictFormat *format;
          format = XRenderFindStandardFormat (display,
                                              depth == 32 ? PictStandardARGB32
                                              : depth == 24 ? PictStandardRGB24
                                              : depth == 8 ? PictStandardA8
                                              : depth == 4 ? PictStandardA4
                                              : PictStandardA1);

          /* Set the Picture repeat to "pad".  This means when
             operations look at pixels outside the image area they
             will use the value of the nearest real pixel instead of
             using a transparent black pixel.  */
          XRenderPictureAttributes attr;
          unsigned long attr_mask = CPRepeat;
          attr.repeat = RepeatPad;

          p = XRenderCreatePicture (display, pixmap, format, attr_mask, &attr);
        }
      else
        {
          image_error ("Specified image bit depth is not supported by XRender");
          return 0;
        }
    }
  else
    {
      /* XRender not supported on this display.  */
      return 0;
    }

  return p;
}
# endif /* !defined USE_CAIRO && defined HAVE_XRENDER */
#endif	/* HAVE_X_WINDOWS */

/* Return true if XIMG's size WIDTH x HEIGHT doesn't break the
   windowing system.
   WIDTH and HEIGHT must both be positive.
   If XIMG is null, assume it is a bitmap.  */

static bool
image_check_image_size (Emacs_Pix_Container ximg, int width, int height)
{
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
  return x_check_image_size (ximg, width, height);
#else
  /* FIXME: Implement this check for the HAVE_NS and HAVE_NTGUI cases.
     For now, assume that every image size is allowed on these systems.  */
  return 1;
#endif
}

/* Create an Emacs_Pix_Container and a pixmap of size WIDTH x
   HEIGHT for use on frame F.  Set *PIMG and *PIXMAP to the
   Emacs_Pix_Container and Emacs_Pixmap created.  Set (*PIMG)->data
   to a raster of WIDTH x HEIGHT pixels allocated via xmalloc.  Print
   error messages via image_error if an error occurs.  Value is true
   if successful.

   On W32, a DEPTH of zero signifies a 24 bit image, otherwise DEPTH
   should indicate the bit depth of the image.  */

static bool
image_create_x_image_and_pixmap_1 (struct frame *f, int width, int height, int depth,
                                   Emacs_Pix_Container *pimg,
                                   Emacs_Pixmap *pixmap, Picture *picture)
{
#ifdef USE_CAIRO
  eassert (input_blocked_p ());

  /* Allocate a pixmap of the same size.  */
  *pixmap = image_create_pix_container (width, height, depth);
  if (*pixmap == NO_PIXMAP)
    {
      *pimg = NULL;
      image_error ("Unable to create X pixmap", Qnil, Qnil);
      return 0;
    }

  *pimg = *pixmap;
  return 1;
#elif defined HAVE_X_WINDOWS
  if (!x_create_x_image_and_pixmap (f, width, height, depth, pimg, pixmap))
    return 0;
# ifdef HAVE_XRENDER
  if (picture)
    *picture = x_create_xrender_picture (f, *pixmap, depth);
# endif

  return 1;
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_HAIKU
  if (depth == 0)
    depth = 24;

  if (depth != 24 && depth != 1)
    {
      *pimg = NULL;
      image_error ("Invalid image bit depth specified");
      return 0;
    }

  *pixmap = BBitmap_new (width, height, depth == 1);

  if (*pixmap == NO_PIXMAP)
    {
      *pimg = NULL;
      image_error ("Unable to create pixmap", Qnil, Qnil);
      return 0;
    }

  *pimg = *pixmap;
  return 1;
#endif

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
      image_error ("Invalid image bit depth specified");
      return 0;
    }

  scanline_width_bits = width * depth;
  remainder = scanline_width_bits % 32;

  if (remainder)
    scanline_width_bits += 32 - remainder;

  /* Bitmaps with a depth less than 16 need a palette.  */
  /* BITMAPINFO structure already contains the first RGBQUAD.  */
  if (depth < 16)
    palette_colors = 1 << (depth - 1);

  *pimg = xmalloc (sizeof (XImage) + palette_colors * sizeof (RGBQUAD));

  header = &(*pimg)->info.bmiHeader;
  memset (&(*pimg)->info, 0, sizeof (BITMAPINFO));
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
      (*pimg)->info.bmiColors[0].rgbBlue = 0;
      (*pimg)->info.bmiColors[0].rgbGreen = 0;
      (*pimg)->info.bmiColors[0].rgbRed = 0;
      (*pimg)->info.bmiColors[0].rgbReserved = 0;
      /* bmiColors is a variable-length array declared by w32api
	 headers as bmiColors[1], which triggers a warning under
	 -Warray-bounds; shut that up.  */
#     if GNUC_PREREQ (4, 4, 0)
#      pragma GCC push_options
#      pragma GCC diagnostic ignored "-Warray-bounds"
#     endif
      (*pimg)->info.bmiColors[1].rgbBlue = 255;
      (*pimg)->info.bmiColors[1].rgbGreen = 255;
      (*pimg)->info.bmiColors[1].rgbRed = 255;
      (*pimg)->info.bmiColors[1].rgbReserved = 0;
#     if GNUC_PREREQ (4, 4, 0)
#      pragma GCC pop_options
#     endif
    }

  hdc = get_frame_dc (f);

  /* Create a DIBSection and raster array for the bitmap,
     and store its handle in *pixmap.  */
  *pixmap = CreateDIBSection (hdc, &(*pimg)->info,
			      (depth < 16) ? DIB_PAL_COLORS : DIB_RGB_COLORS,
			      /* casting avoids a GCC warning */
			      (void **) &(*pimg)->data, NULL, 0);

  /* Realize display palette and garbage all frames. */
  release_frame_dc (f, hdc);

  if (*pixmap == NULL)
    {
      DWORD err = GetLastError ();
      Lisp_Object errcode;
      /* All system errors are < 10000, so the following is safe.  */
      XSETINT (errcode, err);
      image_error ("Unable to create bitmap, error code %d", errcode);
      image_destroy_x_image (*pimg);
      *pimg = NULL;
      return 0;
    }

  return 1;

#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
  *pixmap = ns_image_for_XPM (width, height, depth);
  if (*pixmap == 0)
    {
      *pimg = NULL;
      image_error ("Unable to allocate NSImage for XPM pixmap");
      return 0;
    }
  *pimg = *pixmap;
  return 1;
#endif
}


/* Destroy Emacs_Pix_Container PIMG.  Free data associated with PIMG.  */

static void
image_destroy_x_image (Emacs_Pix_Container pimg)
{
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
  x_destroy_x_image (pimg);
#else
  eassert (input_blocked_p ());
  if (pimg)
    {
#ifdef USE_CAIRO
#endif	/* USE_CAIRO */
#ifdef HAVE_NTGUI
      /* Data will be freed by DestroyObject.  */
      pimg->data = NULL;
      xfree (pimg);
#endif /* HAVE_NTGUI */
#ifdef HAVE_NS
      ns_release_object (pimg);
#endif /* HAVE_NS */
    }
#endif
}


/* Put Emacs_Pix_Container PIMG into pixmap PIXMAP on frame F.
   WIDTH and HEIGHT are width and height of both the image and
   pixmap.  */

static void
gui_put_x_image (struct frame *f, Emacs_Pix_Container pimg,
                 Emacs_Pixmap pixmap, int width, int height)
{
#if defined USE_CAIRO || defined HAVE_HAIKU
  eassert (pimg == pixmap);
#elif defined HAVE_X_WINDOWS
  GC gc;

  eassert (input_blocked_p ());
  gc = XCreateGC (FRAME_X_DISPLAY (f), pixmap, 0, NULL);
  XPutImage (FRAME_X_DISPLAY (f), pixmap, gc, pimg, 0, 0, 0, 0,
             pimg->width, pimg->height);
  XFreeGC (FRAME_X_DISPLAY (f), gc);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NS
  eassert (pimg == pixmap);
  ns_retain_object (pimg);
#endif
}

/* Thin wrapper for image_create_x_image_and_pixmap_1, so that it matches
   with image_put_x_image.  */

static bool
image_create_x_image_and_pixmap (struct frame *f, struct image *img,
				 int width, int height, int depth,
				 Emacs_Pix_Container *ximg, bool mask_p)
{
  eassert ((!mask_p ? img->pixmap : img->mask) == NO_PIXMAP);

  Picture *picture = NULL;
#if !defined USE_CAIRO && defined HAVE_XRENDER
  picture = !mask_p ? &img->picture : &img->mask_picture;
#endif
  return image_create_x_image_and_pixmap_1 (f, width, height, depth, ximg,
                                            !mask_p ? &img->pixmap : &img->mask,
                                            picture);
}

/* Put pixel image PIMG into image IMG on frame F, as a mask if and only
   if MASK_P.  On X, this simply records PIMG on a member of IMG, so
   it can be put into the pixmap afterwards via image_sync_to_pixmaps.
   On the other platforms, it puts PIMG into the pixmap, then frees
   the pixel image and its buffer.  */

static void
image_put_x_image (struct frame *f, struct image *img, Emacs_Pix_Container ximg,
		   bool mask_p)
{
#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
  if (!mask_p)
    {
      eassert (img->ximg == NULL);
      img->ximg = ximg;
    }
  else
    {
      eassert (img->mask_img == NULL);
      img->mask_img = ximg;
    }
#else
  gui_put_x_image (f, ximg, !mask_p ? img->pixmap : img->mask,
                   img->width, img->height);
  image_destroy_x_image (ximg);
#endif
}

#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
/* Put the X images recorded in IMG on frame F into pixmaps, then free
   the X images and their buffers.  */

static void
image_sync_to_pixmaps (struct frame *f, struct image *img)
{
  if (img->ximg)
    {
      gui_put_x_image (f, img->ximg, img->pixmap, img->width, img->height);
      image_destroy_x_image (img->ximg);
      img->ximg = NULL;
    }
  if (img->mask_img)
    {
      gui_put_x_image (f, img->mask_img, img->mask, img->width, img->height);
      image_destroy_x_image (img->mask_img);
      img->mask_img = NULL;
    }
}
#endif

#ifdef HAVE_NTGUI
/* Create a memory device context for IMG on frame F.  It stores the
   currently selected GDI object into *PREV for future restoration by
   image_unget_x_image_or_dc.  */

static HDC
image_get_x_image_or_dc (struct frame *f, struct image *img, bool mask_p,
			 HGDIOBJ *prev)
{
  HDC frame_dc = get_frame_dc (f);
  HDC ximg = CreateCompatibleDC (frame_dc);

  release_frame_dc (f, frame_dc);
  *prev = SelectObject (ximg, !mask_p ? img->pixmap : img->mask);

  return ximg;
}

static void
image_unget_x_image_or_dc (struct image *img, bool mask_p,
			   HDC ximg, HGDIOBJ prev)
{
  SelectObject (ximg, prev);
  DeleteDC (ximg);
}
#else  /* !HAVE_NTGUI */
/* Get the X image for IMG on frame F.  The resulting X image data
   should be treated as read-only at least on X.  */

static Emacs_Pix_Container
image_get_x_image (struct frame *f, struct image *img, bool mask_p)
{
#if defined USE_CAIRO || defined (HAVE_HAIKU)
  return !mask_p ? img->pixmap : img->mask;
#elif defined HAVE_X_WINDOWS
  XImage *ximg_in_img = !mask_p ? img->ximg : img->mask_img;

  if (ximg_in_img)
    return ximg_in_img;
#ifdef HAVE_XRENDER
  else if (img->picture)
    return XGetImage (FRAME_X_DISPLAY (f), !mask_p ? img->pixmap : img->mask,
		      0, 0, img->original_width, img->original_height, ~0, ZPixmap);
#endif
  else
    return XGetImage (FRAME_X_DISPLAY (f), !mask_p ? img->pixmap : img->mask,
		      0, 0, img->width, img->height, ~0, ZPixmap);
#elif defined (HAVE_NS)
  Emacs_Pix_Container pixmap = !mask_p ? img->pixmap : img->mask;

  ns_retain_object (pixmap);
  return pixmap;
#endif
}

static void
image_unget_x_image (struct image *img, bool mask_p, Emacs_Pix_Container ximg)
{
#ifdef USE_CAIRO
#elif defined HAVE_X_WINDOWS
  XImage *ximg_in_img = !mask_p ? img->ximg : img->mask_img;

  if (ximg_in_img)
    eassert (ximg == ximg_in_img);
  else
    XDestroyImage (ximg);
#elif defined (HAVE_NS)
  ns_release_object (ximg);
#endif
}
#endif	/* !HAVE_NTGUI */


/***********************************************************************
			      File Handling
 ***********************************************************************/

/* Find image file FILE.  Look in data-directory/images, then
   x-bitmap-file-path.  Value is the full name of the file
   found, or nil if not found.  If PFD is nonnull store into *PFD a
   readable file descriptor for the file, opened in binary mode.  If
   PFD is null, do not open the file.  */

static Lisp_Object
image_find_image_fd (Lisp_Object file, int *pfd)
{
  Lisp_Object file_found, search_path;
  int fd;

  /* TODO I think this should use something like image-load-path
     instead.  Unfortunately, that can contain non-string elements.  */
  search_path = Fcons (Fexpand_file_name (build_string ("images"),
					  Vdata_directory),
		       Vx_bitmap_file_path);

  /* Try to find FILE in data-directory/images, then x-bitmap-file-path.  */
  fd = openp (search_path, file, Qnil, &file_found,
	      pfd ? Qt : make_fixnum (R_OK), false, false);
  if (fd == -2)
    {
      /* The file exists locally, but has a file name handler.
	 (This happens, e.g., under Auto Image File Mode.)
	 'openp' didn't open the file, so we should, because the
	 caller expects that.  */
      Lisp_Object encoded_name = ENCODE_FILE (file_found);
      fd = emacs_open (SSDATA (encoded_name), O_RDONLY, 0);
    }
  else if (fd < 0)
    return Qnil;
  if (pfd)
    *pfd = fd;
  return file_found;
}

/* Find image file FILE.  Look in data-directory/images, then
   x-bitmap-file-path.  Value is the full name of the file found, or
   nil if not found.  */

Lisp_Object
image_find_image_file (Lisp_Object file)
{
  return image_find_image_fd (file, 0);
}

/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  FD is a file descriptor open for reading FILE.  Set
   *SIZE to the size of the file.  */

static char *
slurp_file (int fd, ptrdiff_t *size)
{
  FILE *fp = fdopen (fd, "rb");

  char *buf = NULL;
  struct stat st;

  if (fp)
    {
      specpdl_ref count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (fclose_unwind, fp);

      if (fstat (fileno (fp), &st) == 0
	  && 0 <= st.st_size && st.st_size < min (PTRDIFF_MAX, SIZE_MAX))
	{
	  /* Report an error if we read past the purported EOF.
	     This can happen if the file grows as we read it.  */
	  ptrdiff_t buflen = st.st_size;
	  buf = xmalloc (buflen + 1);
	  if (fread (buf, 1, buflen + 1, fp) == buflen)
	    *size = buflen;
	  else
	    {
	      xfree (buf);
	      buf = NULL;
	    }
	}

      unbind_to (count, Qnil);
    }

  return buf;
}



/***********************************************************************
			      XBM images
 ***********************************************************************/

static bool xbm_file_p (Lisp_Object);


/* Indices of image specification fields in xbm_format, below.  */

enum xbm_keyword_index
{
  XBM_TYPE,
  XBM_FILE,
  XBM_WIDTH,
  XBM_HEIGHT,
  XBM_STRIDE,
  XBM_DATA,
  XBM_FOREGROUND,
  XBM_BACKGROUND,
  XBM_ASCENT,
  XBM_MARGIN,
  XBM_RELIEF,
  XBM_ALGORITHM,
  XBM_HEURISTIC_MASK,
  XBM_MASK,
  XBM_DATA_WIDTH,
  XBM_DATA_HEIGHT,
  XBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XBM image specifications.  */

static const struct image_keyword xbm_format[XBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":width",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":height",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":stride",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":data-width",	IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data-height",	IMAGE_POSITIVE_INTEGER_VALUE,		0}
};

/* Tokens returned from xbm_scan.  */

enum xbm_token
{
  XBM_TK_IDENT = 256,
  XBM_TK_NUMBER,
  XBM_TK_OVERFLOW
};


/* Return true if OBJECT is a valid XBM-type image specification.
   A valid specification is a list starting with the symbol `image'
   The rest of the list is a property list which must contain an
   entry `:type xbm'.

   If the specification specifies a file to load, it must contain
   an entry `:file FILENAME' where FILENAME is a string.

   If the specification is for a bitmap loaded from memory it must
   contain `:data-width WIDTH', `:data-height HEIGHT', and `:data DATA',
   where WIDTH and HEIGHT are integers > 0.  DATA may be:

   1. a string large enough to hold the bitmap data, i.e. it must
   have a size >= (WIDTH + 7) / 8 * HEIGHT

   2. a bool-vector of size >= WIDTH * HEIGHT

   3. a vector of strings or bool-vectors, one for each line of the
   bitmap.

   4. a string containing an in-memory XBM file.

   Both the file and data forms may contain the additional entries
   `:background COLOR' and `:foreground COLOR'.  If not present,
   foreground and background of the frame on which the image is
   displayed is used.  */

static bool
xbm_image_p (Lisp_Object object)
{
  struct image_keyword kw[XBM_LAST];

  memcpy (kw, xbm_format, sizeof kw);
  if (!parse_image_spec (object, kw, XBM_LAST, Qxbm))
    return 0;

  eassert (EQ (kw[XBM_TYPE].value, Qxbm));

  if (kw[XBM_FILE].count)
    {
      if (kw[XBM_DATA].count)
	return 0;
    }
  else if (kw[XBM_DATA].count && xbm_file_p (kw[XBM_DATA].value))
    {
      /* In-memory XBM file.  */
      if (kw[XBM_FILE].count)
	return 0;
    }
  else
    {
      Lisp_Object data;
      int width, height, stride;

      /* Entries for `:width', `:height' and `:data' must be present.  */
      if (!kw[XBM_DATA_WIDTH].count
	  || !kw[XBM_DATA_HEIGHT].count
	  || !kw[XBM_DATA].count)
	return 0;

      data = kw[XBM_DATA].value;
      width = XFIXNAT (kw[XBM_DATA_WIDTH].value);
      height = XFIXNAT (kw[XBM_DATA_HEIGHT].value);

      if (!kw[XBM_STRIDE].count)
	stride = width;
      else
	stride = XFIXNAT (kw[XBM_STRIDE].value);

      /* Check type of data, and width and height against contents of
	 data.  */
      if (VECTORP (data))
	{
	  EMACS_INT i;

	  /* Number of elements of the vector must be >= height.  */
	  if (ASIZE (data) < height)
	    return 0;

	  /* Each string or bool-vector in data must be large enough
	     for one line of the image.  */
	  for (i = 0; i < height; ++i)
	    {
	      Lisp_Object elt = AREF (data, i);

	      if (STRINGP (elt))
		{
		  if (SCHARS (elt) < stride / CHAR_BIT)
		    return 0;
		}
	      else if (BOOL_VECTOR_P (elt))
		{
		  if (bool_vector_size (elt) < width)
		    return 0;
		}
	      else
		return 0;
	    }
	}
      else if (STRINGP (data))
	{
	  if (SCHARS (data) < stride / CHAR_BIT * height)
	    return 0;
	}
      else if (BOOL_VECTOR_P (data))
	{
	  if (height > 1 && stride != (width + CHAR_BIT - 1)
	      / CHAR_BIT * CHAR_BIT)
	    return 0;

	  if (bool_vector_size (data) / height < stride)
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
xbm_scan (char **s, char *end, char *sval, int *ival)
{
  unsigned char c UNINIT;
  char *sval_end = sval + BUFSIZ;

 loop:

  /* Skip white space.  */
  while (*s < end && (c = *(*s)++, c_isspace (c)))
    ;

  if (*s >= end)
    c = 0;
  else if (c_isdigit (c))
    {
      int value = 0, digit;
      bool overflow = false;

      if (c == '0' && *s < end)
	{
	  c = *(*s)++;
	  if (c == 'x' || c == 'X')
	    {
	      while (*s < end)
		{
		  c = *(*s)++;
		  digit = char_hexdigit (c);
		  if (digit < 0)
		    break;
		  overflow |= INT_MULTIPLY_WRAPV (value, 16, &value);
		  value += digit;
		}
	    }
	  else if ('0' <= c && c <= '7')
	    {
	      value = c - '0';
	      while (*s < end
		     && (c = *(*s)++, '0' <= c && c <= '7'))
		{
		  overflow |= INT_MULTIPLY_WRAPV (value, 8, &value);
		  value += c - '0';
		}
	    }
	}
      else
	{
	  value = c - '0';
	  while (*s < end
		 && (c = *(*s)++, c_isdigit (c)))
	    {
	      overflow |= INT_MULTIPLY_WRAPV (value, 10, &value);
	      overflow |= INT_ADD_WRAPV (value, c - '0', &value);
	    }
	}

      if (*s < end)
	*s = *s - 1;
      *ival = value;
      return overflow ? XBM_TK_OVERFLOW : XBM_TK_NUMBER;
    }
  /* Character literal.  XBM images typically contain hex escape
     sequences and not actual characters, so we only try to handle
     that here.  */
  else if (c == '\'')
    {
      int value = 0, digit;
      bool overflow = false;

      if (*s == end)
	return 0;

      c = *(*s)++;

      if (c != '\\' || *s == end)
	return 0;

      c = *(*s)++;

      if (c == 'x')
	{
	  while (*s < end)
	    {
	      c = *(*s)++;

	      if (c == '\'')
		{
		  *ival = value;
		  return overflow ? XBM_TK_OVERFLOW : XBM_TK_NUMBER;
		}

	      digit = char_hexdigit (c);

	      if (digit < 0)
		return 0;

	      overflow |= INT_MULTIPLY_WRAPV (value, 16, &value);
	      value += digit;
	    }
	}

      return 0;
    }
  else if (c_isalpha (c) || c == '_')
    {
      *sval++ = c;
      while (*s < end && sval < sval_end
	     && (c = *(*s)++, (c_isalnum (c) || c == '_')))
	*sval++ = c;
      *sval = 0;
      if (*s < end)
	*s = *s - 1;
      return XBM_TK_IDENT;
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
  bits = alloca (height * w2);
  memset (bits, 0, height * w2);
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
convert_mono_to_color_image (struct frame *f, struct image *img,
			     COLORREF foreground, COLORREF background)
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
  SetTextColor (new_img_dc, foreground);
  SetBkColor (new_img_dc, background);

  BitBlt (new_img_dc, 0, 0, img->width, img->height, old_img_dc,
	  0, 0, SRCCOPY);

  SelectObject (old_img_dc, old_prev);
  SelectObject (new_img_dc, new_prev);
  DeleteDC (old_img_dc);
  DeleteDC (new_img_dc);
  DeleteObject (img->pixmap);
  if (new_pixmap == 0)
    fputs ("Failed to convert image to color.\n", stderr);
  else
    img->pixmap = new_pixmap;
}

#define XBM_BIT_SHUFFLE(b) (~(b))

#else

#define XBM_BIT_SHUFFLE(b) (b)

#endif /* HAVE_NTGUI */


static void
Create_Pixmap_From_Bitmap_Data (struct frame *f, struct image *img, char *data,
				RGB_PIXEL_COLOR fg, RGB_PIXEL_COLOR bg,
				bool non_default_colors)
{
#ifdef USE_CAIRO
  Emacs_Color fgbg[] = {{.pixel = fg}, {.pixel = bg}};
  FRAME_TERMINAL (f)->query_colors (f, fgbg, ARRAYELTS (fgbg));
  fg = lookup_rgb_color (f, fgbg[0].red, fgbg[0].green, fgbg[0].blue);
  bg = lookup_rgb_color (f, fgbg[1].red, fgbg[1].green, fgbg[1].blue);
  img->pixmap
    = image_pix_container_create_from_bitmap_data (f, data, img->width,
						   img->height, fg, bg);
#elif defined HAVE_X_WINDOWS
  img->pixmap
    = XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
				   FRAME_X_DRAWABLE (f),
				   data,
				   img->width, img->height,
				   fg, bg,
				   FRAME_DISPLAY_INFO (f)->n_planes);
# if !defined USE_CAIRO && defined HAVE_XRENDER
  if (img->pixmap)
    img->picture = x_create_xrender_picture (f, img->pixmap, 0);
# endif

#elif defined HAVE_NTGUI
  img->pixmap
    = w32_create_pixmap_from_bitmap_data (img->width, img->height, data);

  /* If colors were specified, transfer the bitmap to a color one.  */
  if (non_default_colors)
    convert_mono_to_color_image (f, img, fg, bg);
#elif defined HAVE_NS
  img->pixmap = ns_image_from_XBM (data, img->width, img->height, fg, bg);
#elif defined HAVE_HAIKU
  img->pixmap = BBitmap_new (img->width, img->height, 0);

  if (img->pixmap)
    {
      int bytes_per_line = (img->width + 7) / 8;

      for (int y = 0; y < img->height; y++)
	{
	  for (int x = 0; x < img->width; x++)
	    PUT_PIXEL (img->pixmap, x, y,
		       (data[x / 8] >> (x % 8)) & 1 ? fg : bg);
	  data += bytes_per_line;
	}
    }
#endif
}



/* Replacement for XReadBitmapFileData which isn't available under old
   X versions.  CONTENTS is a pointer to a buffer to parse; END is the
   buffer's end.  Set *WIDTH and *HEIGHT to the width and height of
   the image.  Return in *DATA the bitmap data allocated with xmalloc.
   Value is true if successful.  DATA null means just test if
   CONTENTS looks like an in-memory XBM file.  If INHIBIT_IMAGE_ERROR,
   inhibit the call to image_error when the image size is invalid (the
   bitmap remains unread).  */

static bool
xbm_read_bitmap_data (struct frame *f, char *contents, char *end,
		      int *width, int *height, char **data,
		      bool inhibit_image_error)
{
  char *s = contents;
  char buffer[BUFSIZ];
  bool padding_p = 0;
  bool v10 = 0;
  int bytes_per_line, i, nbytes;
  char *p;
  int value;
  int LA1;

#define match() \
     LA1 = xbm_scan (&s, end, buffer, &value)

#define expect(TOKEN)		\
  do				\
    {				\
      if (LA1 != (TOKEN)) 	\
	goto failure;		\
      match ();			\
    }				\
  while (0)

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
	  char *q = strrchr (buffer, '_');
	  q = q ? q + 1 : buffer;
	  if (strcmp (q, "width") == 0)
	    *width = value;
	  else if (strcmp (q, "height") == 0)
	    *height = value;
	}
      expect (XBM_TK_NUMBER);
    }

  if (!check_image_size (f, *width, *height))
    {
      if (!inhibit_image_error)
	image_size_error ();
      goto failure;
    }
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

  if (! image_check_image_size (0, *width, *height))
    {
      if (!inhibit_image_error)
	image_error ("Image too large (%dx%d)",
		     make_fixnum (*width), make_fixnum (*height));
      goto failure;
    }
  bytes_per_line = (*width + 7) / 8 + padding_p;
  nbytes = bytes_per_line * *height;
  p = *data = xmalloc (nbytes);

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
   CONTENTS.  END is the end of the buffer.  Value is true if
   successful.  */

static bool
xbm_load_image (struct frame *f, struct image *img, char *contents, char *end)
{
  bool rc;
  char *data;
  bool success_p = 0;

  rc = xbm_read_bitmap_data (f, contents, end, &img->width, &img->height,
			     &data, 0);

  if (rc)
    {
      unsigned long foreground = img->face_foreground;
      unsigned long background = img->face_background;
      bool non_default_colors = 0;
      Lisp_Object value;

      eassert (img->width > 0 && img->height > 0);

      /* Get foreground and background colors, maybe allocate colors.  */
      value = image_spec_value (img->spec, QCforeground, NULL);
      if (!NILP (value))
	{
	  foreground = image_alloc_image_color (f, img, value, foreground);
	  non_default_colors = 1;
	}
      value = image_spec_value (img->spec, QCbackground, NULL);
      if (!NILP (value))
	{
	  background = image_alloc_image_color (f, img, value, background);
	  img->background = background;
	  img->background_valid = 1;
	  non_default_colors = 1;
	}

      if (image_check_image_size (0, img->width, img->height))
	Create_Pixmap_From_Bitmap_Data (f, img, data,
					foreground, background,
					non_default_colors);
      else
	img->pixmap = NO_PIXMAP;
      xfree (data);

      if (img->pixmap == NO_PIXMAP)
	{
	  image_clear_image (f, img);
	  image_error ("Unable to create X pixmap for `%s'", img->spec);
	}
      else
	success_p = 1;
    }
  else
    image_error ("Error loading XBM image `%s'", img->spec);

  return success_p;
}


/* Value is true if DATA looks like an in-memory XBM file.  */

static bool
xbm_file_p (Lisp_Object data)
{
  int w, h;
  return (STRINGP (data)
	  && xbm_read_bitmap_data (NULL, SSDATA (data),
				   SSDATA (data) + SBYTES (data),
				   &w, &h, NULL, 1));
}


/* Fill image IMG which is used on frame F with pixmap data.  Value is
   true if successful.  */

static bool
xbm_load (struct frame *f, struct image *img)
{
  bool success_p = 0;
  Lisp_Object file_name;

  eassert (xbm_image_p (img->spec));

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (file_name, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name);
	  return 0;
	}

      ptrdiff_t size;
      char *contents = slurp_file (fd, &size);
      if (contents == NULL)
	{
	  image_error ("Error loading XBM image `%s'", file);
	  return 0;
	}

      success_p = xbm_load_image (f, img, contents, contents + size);
      xfree (contents);
    }
  else
    {
      struct image_keyword fmt[XBM_LAST];
      Lisp_Object data;
      unsigned long foreground = img->face_foreground;
      unsigned long background = img->face_background;
      bool non_default_colors = 0;
      char *bits;
      bool parsed_p;
      bool in_memory_file_p = 0;

      /* See if data looks like an in-memory XBM file.  */
      data = image_spec_value (img->spec, QCdata, NULL);
      in_memory_file_p = xbm_file_p (data);

      /* Parse the image specification.  */
      memcpy (fmt, xbm_format, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, XBM_LAST, Qxbm);
      eassert (parsed_p);

      /* Get specified width, and height.  */
      if (!in_memory_file_p)
	{
	  img->width = XFIXNAT (fmt[XBM_DATA_WIDTH].value);
	  img->height = XFIXNAT (fmt[XBM_DATA_HEIGHT].value);
	  eassert (img->width > 0 && img->height > 0);
	  if (!check_image_size (f, img->width, img->height))
	    {
	      image_size_error ();
	      return 0;
	    }
	}

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[XBM_FOREGROUND].count
	  && STRINGP (fmt[XBM_FOREGROUND].value))
	{
	  foreground = image_alloc_image_color (f,
                                                img,
                                                fmt[XBM_FOREGROUND].value,
                                                foreground);
	  non_default_colors = 1;
	}

      if (fmt[XBM_BACKGROUND].count
	  && STRINGP (fmt[XBM_BACKGROUND].value))
	{
	  background = image_alloc_image_color (f,
                                                img,
                                                fmt[XBM_BACKGROUND].value,
                                                background);
	  non_default_colors = 1;
	}

      if (in_memory_file_p)
	success_p = xbm_load_image (f, img, SSDATA (data),
				    SSDATA (data) + SBYTES (data));
      else
	{
	  USE_SAFE_ALLOCA;

	  if (VECTORP (data))
	    {
	      int i;
	      char *p;
	      int nbytes = (img->width + CHAR_BIT - 1) / CHAR_BIT;

	      SAFE_NALLOCA (bits, nbytes, img->height);
	      p = bits;
	      for (i = 0; i < img->height; ++i, p += nbytes)
		{
		  Lisp_Object line = AREF (data, i);
		  if (STRINGP (line))
		    memcpy (p, SDATA (line), nbytes);
		  else
		    memcpy (p, bool_vector_data (line), nbytes);
		}
	    }
	  else if (STRINGP (data))
	    bits = SSDATA (data);
	  else
	    bits = (char *) bool_vector_data (data);

#ifdef HAVE_NTGUI
          {
            char *invertedBits;
            int nbytes, i;
            /* Windows mono bitmaps are reversed compared with X.  */
            invertedBits = bits;
            nbytes = (img->width + CHAR_BIT - 1) / CHAR_BIT * img->height;
            SAFE_NALLOCA (bits, 1, nbytes);
            for (i = 0; i < nbytes; i++)
              bits[i] = XBM_BIT_SHUFFLE (invertedBits[i]);
          }
#endif
	  /* Create the pixmap.  */

	  if (image_check_image_size (0, img->width, img->height))
	    Create_Pixmap_From_Bitmap_Data (f, img, bits,
					    foreground, background,
					    non_default_colors);
	  else
	    img->pixmap = NO_PIXMAP;

	  if (img->pixmap)
	    success_p = 1;
	  else
	    {
	      image_error ("Unable to create pixmap for XBM image `%s'",
			   img->spec);
	      image_clear_image (f, img);
	    }

	  SAFE_FREE ();
	}
    }

  return success_p;
}



/***********************************************************************
			      XPM images
 ***********************************************************************/

#if defined (HAVE_XPM) || defined (HAVE_NS) || defined (HAVE_PGTK)

static bool xpm_image_p (Lisp_Object object);
static bool xpm_load (struct frame *f, struct image *img);

#endif /* HAVE_XPM || HAVE_NS */

#ifdef HAVE_XPM
#ifdef HAVE_NTGUI
/* Indicate to xpm.h that we don't have Xlib.  */
#define FOR_MSW
/* simx.h in xpm defines XColor and XImage differently than Emacs.  */
/* It also defines Display the same way as Emacs, but gcc 3.3 still barfs.  */
#define XColor xpm_XColor
#define XImage xpm_XImage
#define Display xpm_Display
#ifdef CYGWIN
#include "noX/xpm.h"
#else  /* not CYGWIN */
#include "X11/xpm.h"
#endif	/* not CYGWIN */
#undef FOR_MSW
#undef XColor
#undef XImage
#undef Display
#else  /* not HAVE_NTGUI */
#include "X11/xpm.h"
#endif /* not HAVE_NTGUI */
#endif /* HAVE_XPM */

#if defined HAVE_XPM || defined USE_CAIRO || defined HAVE_NS || defined HAVE_HAIKU

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

#if defined HAVE_XPM || defined HAVE_NS || defined HAVE_HAIKU || defined HAVE_PGTK
/* Vector of image_keyword structures describing the format
   of valid XPM image specifications.  */

static const struct image_keyword xpm_format[XPM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":color-symbols",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};
#endif	/* HAVE_XPM || HAVE_NS || HAVE_HAIKU || HAVE_PGTK */

#if defined HAVE_X_WINDOWS && !defined USE_CAIRO

/* Define ALLOC_XPM_COLORS if we can use Emacs' own color allocation
   functions for allocating image colors.  Our own functions handle
   color allocation failures more gracefully than the ones on the XPM
   lib.  */

#if defined XpmAllocColor && defined XpmFreeColors && defined XpmColorClosure
#define ALLOC_XPM_COLORS
#endif
#endif /* HAVE_X_WINDOWS && !USE_CAIRO */

#ifdef ALLOC_XPM_COLORS

static struct xpm_cached_color *xpm_cache_color (struct frame *, char *,
                                                 XColor *, int);

/* An entry in a hash table used to cache color definitions of named
   colors.  This cache is necessary to speed up XPM image loading in
   case we do color allocations ourselves.  Without it, we would need
   a call to XParseColor per pixel in the image.

   FIXME Now that we're using x_parse_color and its cache, reevaluate
   the need for this caching layer.  */

struct xpm_cached_color
{
  /* Next in collision chain.  */
  struct xpm_cached_color *next;

  /* Color definition (RGB and pixel color).  */
  XColor color;

  /* Color name.  */
  char name[FLEXIBLE_ARRAY_MEMBER];
};

/* The hash table used for the color cache, and its bucket vector
   size (which should be prime).  */

#define XPM_COLOR_CACHE_BUCKETS 1009
static struct xpm_cached_color **xpm_color_cache;

/* Initialize the color cache.  */

static void
xpm_init_color_cache (struct frame *f, XpmAttributes *attrs)
{
  size_t nbytes = XPM_COLOR_CACHE_BUCKETS * sizeof *xpm_color_cache;
  xpm_color_cache = xzalloc (nbytes);
  init_color_table ();

  if (attrs->valuemask & XpmColorSymbols)
    {
      int i;
      XColor color;

      for (i = 0; i < attrs->numsymbols; ++i)
	if (x_parse_color (f, attrs->colorsymbols[i].value, &color))
	  {
	    color.pixel = lookup_rgb_color (f, color.red, color.green,
					    color.blue);
	    xpm_cache_color (f, attrs->colorsymbols[i].name, &color, -1);
	  }
    }
}

/* Free the color cache.  */

static void
xpm_free_color_cache (void)
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
xpm_color_bucket (char *color_name)
{
  EMACS_UINT hash = hash_string (color_name, strlen (color_name));
  return hash % XPM_COLOR_CACHE_BUCKETS;
}


/* On frame F, cache values COLOR for color with name COLOR_NAME.
   BUCKET, if >= 0, is a precomputed bucket index.  Value is the cache
   entry added.  */

static struct xpm_cached_color *
xpm_cache_color (struct frame *f, char *color_name, XColor *color, int bucket)
{
  size_t nbytes;
  struct xpm_cached_color *p;

  if (bucket < 0)
    bucket = xpm_color_bucket (color_name);

  nbytes = FLEXSIZEOF (struct xpm_cached_color, name, strlen (color_name) + 1);
  p = xmalloc (nbytes);
  strcpy (p->name, color_name);
  p->color = *color;
  p->next = xpm_color_cache[bucket];
  xpm_color_cache[bucket] = p;
  return p;
}

/* Look up color COLOR_NAME for frame F in the color cache.  If found,
   return the cached definition in *COLOR.  Otherwise, make a new
   entry in the cache and allocate the color.  Value is false if color
   allocation failed.  */

static bool
xpm_lookup_color (struct frame *f, char *color_name, XColor *color)
{
  struct xpm_cached_color *p;
  int h = xpm_color_bucket (color_name);

  for (p = xpm_color_cache[h]; p; p = p->next)
    if (strcmp (p->name, color_name) == 0)
      break;

  if (p != NULL)
    *color = p->color;
  else if (x_parse_color (f, color_name, color))
    {
      color->pixel = lookup_rgb_color (f, color->red, color->green,
				       color->blue);
      p = xpm_cache_color (f, color_name, color, h);
    }
  /* You get `opaque' at least from ImageMagick converting pbm to xpm
     with transparency, and it's useful.  */
  else if (strcmp ("opaque", color_name) == 0)
    {
      memset (color, 0, sizeof (XColor));  /* Is this necessary/correct?  */
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
xpm_alloc_color (Display *dpy, Colormap cmap, char *color_name, XColor *color,
		 void *closure)
{
  return xpm_lookup_color (closure, color_name, color);
}


/* Callback for freeing NPIXELS colors contained in PIXELS.  CLOSURE
   is a pointer to the frame on which we allocate the color.  Value is
   non-zero if successful.  */

static int
xpm_free_colors (Display *dpy, Colormap cmap, Pixel *pixels, int npixels, void *closure)
{
  return 1;
}

#endif /* ALLOC_XPM_COLORS */


#ifdef WINDOWSNT

/* XPM library details.  */

DEF_DLL_FN (void, XpmFreeAttributes, (XpmAttributes *));
DEF_DLL_FN (int, XpmCreateImageFromBuffer,
	    (Display *, char *, xpm_XImage **,
	     xpm_XImage **, XpmAttributes *));
DEF_DLL_FN (int, XpmReadFileToImage,
	    (Display *, char *, xpm_XImage **,
	     xpm_XImage **, XpmAttributes *));
DEF_DLL_FN (void, XImageFree, (xpm_XImage *));

static bool
init_xpm_functions (void)
{
  HMODULE library;

  if (!(library = w32_delayed_load (Qxpm)))
    return 0;

  LOAD_DLL_FN (library, XpmFreeAttributes);
  LOAD_DLL_FN (library, XpmCreateImageFromBuffer);
  LOAD_DLL_FN (library, XpmReadFileToImage);
  LOAD_DLL_FN (library, XImageFree);
  return 1;
}

# undef XImageFree
# undef XpmCreateImageFromBuffer
# undef XpmFreeAttributes
# undef XpmReadFileToImage

# define XImageFree fn_XImageFree
# define XpmCreateImageFromBuffer fn_XpmCreateImageFromBuffer
# define XpmFreeAttributes fn_XpmFreeAttributes
# define XpmReadFileToImage fn_XpmReadFileToImage

#endif /* WINDOWSNT */

#if defined HAVE_XPM || defined HAVE_NS || defined HAVE_HAIKU || defined HAVE_PGTK
/* Value is true if COLOR_SYMBOLS is a valid color symbols list
   for XPM images.  Such a list must consist of conses whose car and
   cdr are strings.  */

static bool
xpm_valid_color_symbols_p (Lisp_Object color_symbols)
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

/* Value is true if OBJECT is a valid XPM image specification.  */

static bool
xpm_image_p (Lisp_Object object)
{
  struct image_keyword fmt[XPM_LAST];
  memcpy (fmt, xpm_format, sizeof fmt);
  return (parse_image_spec (object, fmt, XPM_LAST, Qxpm)
	  /* Either `:file' or `:data' must be present.  */
	  && fmt[XPM_FILE].count + fmt[XPM_DATA].count == 1
	  /* Either no `:color-symbols' or it's a list of conses
	     whose car and cdr are strings.  */
	  && (! fmt[XPM_COLOR_SYMBOLS].count
	      || xpm_valid_color_symbols_p (fmt[XPM_COLOR_SYMBOLS].value)));
}
#endif	/* HAVE_XPM || HAVE_NS || HAVE_HAIKU || HAVE_PGTK */

#endif /* HAVE_XPM || USE_CAIRO || HAVE_NS || HAVE_HAIKU */

#if defined HAVE_XPM && defined HAVE_X_WINDOWS && !defined USE_GTK
ptrdiff_t
x_create_bitmap_from_xpm_data (struct frame *f, const char **bits)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  ptrdiff_t id;
  int rc;
  XpmAttributes attrs;
  Pixmap bitmap, mask;

  memset (&attrs, 0, sizeof attrs);

  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;

#ifdef ALLOC_XPM_COLORS
  attrs.color_closure = f;
  attrs.alloc_color = xpm_alloc_color;
  attrs.free_colors = xpm_free_colors;
  attrs.valuemask |= XpmAllocColor | XpmFreeColors | XpmColorClosure;
  xpm_init_color_cache (f, &attrs);
#endif

  rc = XpmCreatePixmapFromData (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				(char **) bits, &bitmap, &mask, &attrs);
  if (rc != XpmSuccess)
    {
      XpmFreeAttributes (&attrs);
      return -1;
    }

  id = image_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].have_mask = true;
  dpyinfo->bitmaps[id - 1].mask = mask;
  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].height = attrs.height;
  dpyinfo->bitmaps[id - 1].width = attrs.width;
  dpyinfo->bitmaps[id - 1].depth = attrs.depth;
  dpyinfo->bitmaps[id - 1].refcount = 1;
#ifdef USE_CAIRO
  dpyinfo->bitmaps[id - 1].stipple = NULL;
#endif	/* USE_CAIRO */

#ifdef ALLOC_XPM_COLORS
  xpm_free_color_cache ();
#endif
  XpmFreeAttributes (&attrs);
  return id;
}
#endif /* defined (HAVE_XPM) && defined (HAVE_X_WINDOWS) */

/* Load image IMG which will be displayed on frame F.  Value is
   true if successful.  */

#if defined HAVE_XPM && !defined USE_CAIRO

static bool
xpm_load (struct frame *f, struct image *img)
{
  int rc;
  XpmAttributes attrs;
  Lisp_Object specified_file, color_symbols;
  USE_SAFE_ALLOCA;

#ifdef HAVE_NTGUI
  HDC hdc;
  xpm_XImage * xpm_image = NULL, * xpm_mask = NULL;
#endif /* HAVE_NTGUI */

  /* Configure the XPM lib.  Use the visual of frame F.  Allocate
     close colors.  Return colors allocated.  */
  memset (&attrs, 0, sizeof attrs);

#ifndef HAVE_NTGUI
  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.depth = FRAME_DISPLAY_INFO (f)->n_planes;
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;
  attrs.valuemask |= XpmDepth;
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
      ptrdiff_t i, size;

      attrs.valuemask |= XpmColorSymbols;

      /* Count number of symbols.  */
      attrs.numsymbols = 0;
      for (tail = color_symbols; CONSP (tail); tail = XCDR (tail))
	++attrs.numsymbols;

      /* Allocate an XpmColorSymbol array.  */
      SAFE_NALLOCA (xpm_syms, 1, attrs.numsymbols);
      size = attrs.numsymbols * sizeof *xpm_syms;
      memset (xpm_syms, 0, size);
      attrs.colorsymbols = xpm_syms;

      /* Fill the color symbol array.  */
      for (tail = color_symbols, i = 0;
	   CONSP (tail);
	   ++i, tail = XCDR (tail))
	{
	  Lisp_Object name;
	  Lisp_Object color;
	  char *empty_string = (char *) "";

	  if (!CONSP (XCAR (tail)))
	    {
	      xpm_syms[i].name = empty_string;
	      xpm_syms[i].value = empty_string;
	      continue;
	    }
	  name = XCAR (XCAR (tail));
	  color = XCDR (XCAR (tail));
	  if (STRINGP (name))
	    SAFE_ALLOCA_STRING (xpm_syms[i].name, name);
	  else
	    xpm_syms[i].name = empty_string;
	  if (STRINGP (color))
	    SAFE_ALLOCA_STRING (xpm_syms[i].value, color);
	  else
	    xpm_syms[i].value = empty_string;
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
      Lisp_Object file = image_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
#ifdef ALLOC_XPM_COLORS
	  xpm_free_color_cache ();
#endif
	  SAFE_FREE ();
	  return 0;
	}

      file = ENCODE_FILE (file);
#ifdef HAVE_NTGUI
#ifdef WINDOWSNT
      /* FILE is encoded in UTF-8, but image libraries on Windows
	 support neither UTF-8 nor UTF-16 encoded file names.  So we
	 need to re-encode it in ANSI.  */
      file = ansi_encode_filename (file);
#endif
      /* XpmReadFileToPixmap is not available in the Windows port of
	 libxpm.  But XpmReadFileToImage almost does what we want.  */
      rc = XpmReadFileToImage (&hdc, SSDATA (file),
			       &xpm_image, &xpm_mask,
			       &attrs);
#else
      rc = XpmReadFileToImage (FRAME_X_DISPLAY (f), SSDATA (file),
			       &img->ximg, &img->mask_img,
			       &attrs);
#endif /* HAVE_NTGUI */
    }
  else
    {
      Lisp_Object buffer = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (buffer))
	{
	  image_error ("Invalid image data `%s'", buffer);
#ifdef ALLOC_XPM_COLORS
	  xpm_free_color_cache ();
#endif
	  SAFE_FREE ();
	  return 0;
	}
#ifdef HAVE_NTGUI
      /* XpmCreatePixmapFromBuffer is not available in the Windows port
	 of libxpm.  But XpmCreateImageFromBuffer almost does what we want.  */
      rc = XpmCreateImageFromBuffer (&hdc, SSDATA (buffer),
				     &xpm_image, &xpm_mask,
				     &attrs);
#else
      rc = XpmCreateImageFromBuffer (FRAME_X_DISPLAY (f), SSDATA (buffer),
				     &img->ximg, &img->mask_img,
				     &attrs);
#endif /* HAVE_NTGUI */
    }

#ifdef HAVE_X_WINDOWS
  if (rc == XpmSuccess)
    {
      img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				   img->ximg->width, img->ximg->height,
				   img->ximg->depth);
      if (img->pixmap == NO_PIXMAP)
	{
	  image_clear_image (f, img);
	  rc = XpmNoMemory;
	}
      else
        {
# if !defined USE_CAIRO && defined HAVE_XRENDER
          img->picture = x_create_xrender_picture (f, img->pixmap,
                                                   img->ximg->depth);
# endif
          if (img->mask_img)
            {
              img->mask = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
                                         img->mask_img->width,
                                         img->mask_img->height,
                                         img->mask_img->depth);
              if (img->mask == NO_PIXMAP)
                {
                  image_clear_image (f, img);
                  rc = XpmNoMemory;
                }
# if !defined USE_CAIRO && defined HAVE_XRENDER
              else
                img->mask_picture = x_create_xrender_picture
                  (f, img->mask, img->mask_img->depth);
# endif
            }
        }
    }
#endif

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
	  XImageFree (xpm_image);
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
	  XImageFree (xpm_mask);
	  DeleteDC (hdc);
	}

      DeleteDC (hdc);
#endif /* HAVE_NTGUI */

      /* Remember allocated colors.  */
      img->colors = xnmalloc (attrs.nalloc_pixels, sizeof *img->colors);
      img->ncolors = attrs.nalloc_pixels;
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
      eassert (img->width > 0 && img->height > 0);

      /* The call to XpmFreeAttributes below frees attrs.alloc_pixels.  */
      XpmFreeAttributes (&attrs);

#ifdef HAVE_X_WINDOWS
      /* Maybe fill in the background field while we have ximg handy.  */
      IMAGE_BACKGROUND (img, f, img->ximg);
      if (img->mask_img)
	/* Fill in the background_transparent field while we have the
	   mask handy.  */
	image_background_transparent (img, f, img->mask_img);
#endif
    }
  else
    {
#ifdef HAVE_NTGUI
      DeleteDC (hdc);
#endif /* HAVE_NTGUI */

      switch (rc)
	{
	case XpmOpenFailed:
	  image_error ("Error opening XPM file (%s)", img->spec);
	  break;

	case XpmFileInvalid:
	  image_error ("Invalid XPM file (%s)", img->spec);
	  break;

	case XpmNoMemory:
	  image_error ("Out of memory (%s)", img->spec);
	  break;

	case XpmColorFailed:
	  image_error ("Color allocation error (%s)", img->spec);
	  break;

	default:
	  image_error ("Unknown error (%s)", img->spec);
	  break;
	}
    }

#ifdef ALLOC_XPM_COLORS
  xpm_free_color_cache ();
#endif
  SAFE_FREE ();
  return rc == XpmSuccess;
}

#endif /* HAVE_XPM && !USE_CAIRO */

#if (defined USE_CAIRO && defined HAVE_XPM)	\
  || (defined HAVE_NS && !defined HAVE_XPM)	\
  || (defined HAVE_HAIKU && !defined HAVE_XPM)  \
  || (defined HAVE_PGTK && !defined HAVE_XPM)

/* XPM support functions for NS and Haiku where libxpm is not available, and for
   Cairo.  Only XPM version 3 (without any extensions) is supported.  */

static void xpm_put_color_table_v (Lisp_Object, const char *,
                                   int, Lisp_Object);
static Lisp_Object xpm_get_color_table_v (Lisp_Object, const char *, int);
static void xpm_put_color_table_h (Lisp_Object, const char *,
                                   int, Lisp_Object);
static Lisp_Object xpm_get_color_table_h (Lisp_Object, const char *, int);

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
xpm_scan (const char **s, const char *end, const char **beg, ptrdiff_t *len)
{
  unsigned char c;

  while (*s < end)
    {
      /* Skip white-space.  */
      do
	c = *(*s)++;
      while (c_isspace (c) && *s < end);

      /* gnus-pointer.xpm uses '-' in its identifier.
	 sb-dir-plus.xpm uses '+' in its identifier.  */
      if (c_isalpha (c) || c == '_' || c == '-' || c == '+')
	{
	  *beg = *s - 1;
	  while (*s < end
		 && (c = **s, c_isalnum (c)
		     || c == '_' || c == '-' || c == '+'))
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
xpm_make_color_table_v (void (**put_func) (Lisp_Object, const char *, int,
                                           Lisp_Object),
                        Lisp_Object (**get_func) (Lisp_Object, const char *,
                                                  int))
{
  *put_func = xpm_put_color_table_v;
  *get_func = xpm_get_color_table_v;
  return make_nil_vector (256);
}

static void
xpm_put_color_table_v (Lisp_Object color_table,
                       const char *chars_start,
                       int chars_len,
                       Lisp_Object color)
{
  unsigned char uc = *chars_start;
  ASET (color_table, uc, color);
}

static Lisp_Object
xpm_get_color_table_v (Lisp_Object color_table,
                       const char *chars_start,
                       int chars_len)
{
  unsigned char uc = *chars_start;
  return AREF (color_table, uc);
}

static Lisp_Object
xpm_make_color_table_h (void (**put_func) (Lisp_Object, const char *, int,
                                           Lisp_Object),
                        Lisp_Object (**get_func) (Lisp_Object, const char *,
                                                  int))
{
  *put_func = xpm_put_color_table_h;
  *get_func = xpm_get_color_table_h;
  return make_hash_table (hashtest_equal, DEFAULT_HASH_SIZE,
			  DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
			  Qnil, false);
}

static void
xpm_put_color_table_h (Lisp_Object color_table,
                       const char *chars_start,
                       int chars_len,
                       Lisp_Object color)
{
  struct Lisp_Hash_Table *table = XHASH_TABLE (color_table);
  Lisp_Object chars = make_unibyte_string (chars_start, chars_len), hash_code;

  hash_lookup (table, chars, &hash_code);
  hash_put (table, chars, color, hash_code);
}

static Lisp_Object
xpm_get_color_table_h (Lisp_Object color_table,
                       const char *chars_start,
                       int chars_len)
{
  struct Lisp_Hash_Table *table = XHASH_TABLE (color_table);
  ptrdiff_t i =
    hash_lookup (table, make_unibyte_string (chars_start, chars_len), NULL);

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
xpm_str_to_color_key (const char *s)
{
  int i;

  for (i = 0; i < ARRAYELTS (xpm_color_key_strings); i++)
    if (strcmp (xpm_color_key_strings[i], s) == 0)
      return i;
  return -1;
}

static bool
xpm_load_image (struct frame *f,
                struct image *img,
                const char *contents,
                const char *end)
{
  const char *s = contents, *beg, *str;
  char buffer[BUFSIZ];
  int width, height, x, y;
  int num_colors, chars_per_pixel;
  ptrdiff_t len;
  int LA1;
  void (*put_color_table) (Lisp_Object, const char *, int, Lisp_Object);
  Lisp_Object (*get_color_table) (Lisp_Object, const char *, int);
  Lisp_Object frame, color_symbols, color_table;
  int best_key;
#if !defined (HAVE_NS)
  bool have_mask = false;
#endif
  Emacs_Pix_Container ximg = NULL, mask_img = NULL;

#define match() \
     LA1 = xpm_scan (&s, end, &beg, &len)

#define expect(TOKEN)		\
  do				\
    {				\
      if (LA1 != (TOKEN)) 	\
	goto failure;		\
      match ();			\
    }				\
  while (0)

#define expect_ident(IDENT)					\
     if (LA1 == XPM_TK_IDENT \
         && strlen ((IDENT)) == len && memcmp ((IDENT), beg, len) == 0)	\
       match ();						\
     else							\
       goto failure

  if (!(end - s >= 9 && memcmp (s, "/* XPM */", 9) == 0))
    goto failure;
  s += 9;
  match ();
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
      image_size_error ();
      goto failure;
    }

  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0)
#ifndef HAVE_NS
      || !image_create_x_image_and_pixmap (f, img, width, height, 1,
					   &mask_img, 1)
#endif
      )
    {
      image_error ("Image too large");
      goto failure;
    }

  expect (',');

  XSETFRAME (frame, f);
  if (!NILP (Fxw_display_color_p (frame)))
    best_key = XPM_COLOR_KEY_C;
  else if (!NILP (Fx_display_grayscale_p (frame)))
    best_key = (XFIXNAT (Fx_display_planes (frame)) > 2
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
      char *color, *max_color = NULL;
      int key, next_key, max_key = 0;
      Lisp_Object symbol_color = Qnil, color_val;
      Emacs_Color cdef;

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
	  Lisp_Object specified_color = Fassoc (symbol_color, color_symbols, Qnil);

	  if (CONSP (specified_color) && STRINGP (XCDR (specified_color)))
	    {
	      if (xstrcasecmp (SSDATA (XCDR (specified_color)), "None") == 0)
		color_val = Qt;
	      else if (FRAME_TERMINAL (f)->defined_color_hook
                       (f, SSDATA (XCDR (specified_color)), &cdef, false, false))
		color_val
		  = make_fixnum (lookup_rgb_color (f, cdef.red, cdef.green,
						   cdef.blue));
	    }
	}
      if (NILP (color_val) && max_color)
	{
	  if (xstrcasecmp (max_color, "None") == 0)
	    color_val = Qt;
	  else if (FRAME_TERMINAL (f)->defined_color_hook
                   (f, max_color, &cdef, false, false))
	    color_val = make_fixnum (lookup_rgb_color (f, cdef.red, cdef.green,
						       cdef.blue));
	}
      if (!NILP (color_val))
	(*put_color_table) (color_table, beg, chars_per_pixel, color_val);

      expect (',');
    }

  unsigned long frame_fg = FRAME_FOREGROUND_PIXEL (f);
#ifdef USE_CAIRO
  {
    Emacs_Color color = {.pixel = frame_fg};
    FRAME_TERMINAL (f)->query_colors (f, &color, 1);
    frame_fg = lookup_rgb_color (f, color.red, color.green, color.blue);
  }
#endif
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

	  PUT_PIXEL (ximg, x, y,
		     FIXNUMP (color_val) ? XFIXNUM (color_val) : frame_fg);
#ifndef HAVE_NS
	  PUT_PIXEL (mask_img, x, y,
		     (!EQ (color_val, Qt) ? PIX_MASK_DRAW
		      : (have_mask = true, PIX_MASK_RETAIN)));
#else
          if (EQ (color_val, Qt))
            ns_set_alpha (ximg, x, y, 0);
#endif
	}
      if (y + 1 < height)
	expect (',');
    }

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  image_put_x_image (f, img, ximg, 0);
#ifndef HAVE_NS
  if (have_mask)
    {
      /* Fill in the background_transparent field while we have the
	 mask handy.  */
      image_background_transparent (img, f, mask_img);

      image_put_x_image (f, img, mask_img, 1);
    }
  else
    {
      image_destroy_x_image (mask_img);
      image_clear_image_1 (f, img, CLEAR_IMAGE_MASK);
    }
#endif
  return 1;

 failure:
  image_error ("Invalid XPM3 file (%s)", img->spec);
  image_destroy_x_image (ximg);
  image_destroy_x_image (mask_img);
  image_clear_image (f, img);
  return 0;

#undef match
#undef expect
#undef expect_ident
}

static bool
xpm_load (struct frame *f,
          struct image *img)
{
  bool success_p = 0;
  Lisp_Object file_name;

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (file_name, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name);
	  return 0;
	}

      ptrdiff_t size;
      char *contents = slurp_file (fd, &size);
      if (contents == NULL)
	{
	  image_error ("Error loading XPM image `%s'", file);
	  return 0;
	}

      success_p = xpm_load_image (f, img, contents, contents + size);
      xfree (contents);
    }
  else
    {
      Lisp_Object data;

      data = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (data))
	{
	  image_error ("Invalid image data `%s'", data);
	  return 0;
	}
      success_p = xpm_load_image (f, img, SSDATA (data),
				  SSDATA (data) + SBYTES (data));
    }

  return success_p;
}

#endif /* HAVE_NS && !HAVE_XPM */



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

static unsigned
ct_hash_rgb (unsigned r, unsigned g, unsigned b)
{
  return (r << 16) ^ (g << 8) ^ b;
}

/* The color hash table.  */

static struct ct_color **ct_table;

/* Number of entries in the color table.  */

static int ct_colors_allocated;
enum
{
  ct_colors_allocated_max =
    min (INT_MAX,
	 min (PTRDIFF_MAX, SIZE_MAX) / sizeof (unsigned long))
};

/* Initialize the color table.  */

static void
init_color_table (void)
{
  int size = CT_SIZE * sizeof (*ct_table);
  ct_table = xzalloc (size);
  ct_colors_allocated = 0;
}


/* Free memory associated with the color table.  */

static void
free_color_table (void)
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
lookup_rgb_color (struct frame *f, int r, int g, int b)
{
  unsigned hash = ct_hash_rgb (r, g, b);
  int i = hash % CT_SIZE;
  struct ct_color *p;
  Display_Info *dpyinfo;

  /* Handle TrueColor visuals specially, which improves performance by
     two orders of magnitude.  Freeing colors on TrueColor visuals is
     a nop, and pixel colors specify RGB values directly.  See also
     the Xlib spec, chapter 3.1.  */
  dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->red_bits > 0)
    {
      /* Apply gamma-correction like normal color allocation does.  */
      if (f->gamma)
	{
	  XColor color;
	  color.red = r, color.green = g, color.blue = b;
	  gamma_correct (f, &color);
	  r = color.red, g = color.green, b = color.blue;
	}

      return x_make_truecolor_pixel (dpyinfo, r, g, b);
    }

  for (p = ct_table[i]; p; p = p->next)
    if (p->r == r && p->g == g && p->b == b)
      break;

  if (p == NULL)
    {

#ifdef HAVE_X_WINDOWS
      XColor color;
      Colormap cmap;
      bool rc;
#else
      COLORREF color;
#endif

      if (ct_colors_allocated_max <= ct_colors_allocated)
	return FRAME_FOREGROUND_PIXEL (f);

#ifdef HAVE_X_WINDOWS
      color.red = r;
      color.green = g;
      color.blue = b;

      cmap = FRAME_X_COLORMAP (f);
      rc = x_alloc_nearest_color (f, cmap, &color);
      if (rc)
	{
	  ++ct_colors_allocated;
	  p = xmalloc (sizeof *p);
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
#ifdef HAVE_NTGUI
      color = PALETTERGB (r, g, b);
#else
      color = RGB_TO_ULONG (r, g, b);
#endif /* HAVE_NTGUI */
      ++ct_colors_allocated;
      p = xmalloc (sizeof *p);
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
lookup_pixel_color (struct frame *f, unsigned long pixel)
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
      bool rc;

      if (ct_colors_allocated >= ct_colors_allocated_max)
	return FRAME_FOREGROUND_PIXEL (f);

#ifdef HAVE_X_WINDOWS
      cmap = FRAME_X_COLORMAP (f);
      color.pixel = pixel;
      x_query_colors (f, &color, 1);
      rc = x_alloc_nearest_color (f, cmap, &color);
#else
      block_input ();
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      color.pixel = pixel;
      XQueryColor (NULL, cmap, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);
      unblock_input ();
#endif /* HAVE_X_WINDOWS */

      if (rc)
	{
	  ++ct_colors_allocated;

	  p = xmalloc (sizeof *p);
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
colors_in_color_table (int *n)
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
      colors = xmalloc (ct_colors_allocated * sizeof *colors);
      *n = ct_colors_allocated;

      for (i = j = 0; i < CT_SIZE; ++i)
	for (p = ct_table[i]; p; p = p->next)
	  colors[j++] = p->pixel;
    }

  return colors;
}

#else /* COLOR_TABLE_SUPPORT */

static unsigned long
lookup_rgb_color (struct frame *f, int r, int g, int b)
{
#ifdef HAVE_NTGUI
  return PALETTERGB (r >> 8, g >> 8, b >> 8);
#elif defined USE_CAIRO || defined HAVE_NS || defined HAVE_HAIKU
  return RGB_TO_ULONG (r >> 8, g >> 8, b >> 8);
#else
  xsignal1 (Qfile_error,
	    build_string ("This Emacs mishandles this image file type"));
#endif
}

static void
init_color_table (void)
{
}
#endif /* COLOR_TABLE_SUPPORT */


/***********************************************************************
			      Algorithms
 ***********************************************************************/

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


/* On frame F, return an array of Emacs_Color structures describing image
   IMG->pixmap.  Each Emacs_Color structure has its pixel color set.  RGB_P
   means also fill the red/green/blue members of the Emacs_Color
   structures.  Value is a pointer to the array of Emacs_Color structures,
   allocated with xmalloc; it must be freed by the caller.  */

static Emacs_Color *
image_to_emacs_colors (struct frame *f, struct image *img, bool rgb_p)
{
  int x, y;
  Emacs_Color *colors, *p;
  Emacs_Pix_Context ximg;
  ptrdiff_t nbytes;
#ifdef HAVE_NTGUI
  HGDIOBJ prev;
#endif /* HAVE_NTGUI */

  if (INT_MULTIPLY_WRAPV (sizeof *colors, img->width, &nbytes)
      || INT_MULTIPLY_WRAPV (img->height, nbytes, &nbytes)
      || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  colors = xmalloc (nbytes);

  /* Get the X image or create a memory device context for IMG. */
  ximg = image_get_x_image_or_dc (f, img, 0, &prev);

  /* Fill the `pixel' members of the Emacs_Color array.  I wished there
     were an easy and portable way to circumvent XGetPixel.  */
  p = colors;
  for (y = 0; y < img->height; ++y)
    {
#if !defined USE_CAIRO && !defined HAVE_NS && !defined HAVE_HAIKU
      Emacs_Color *row = p;
      for (x = 0; x < img->width; ++x, ++p)
	p->pixel = GET_PIXEL (ximg, x, y);
      if (rgb_p)
        {
          FRAME_TERMINAL (f)->query_colors (f, row, img->width);
        }
#else  /* USE_CAIRO || HAVE_NS || HAVE_HAIKU */
      for (x = 0; x < img->width; ++x, ++p)
	{
	  p->pixel = GET_PIXEL (ximg, x, y);
	  if (rgb_p)
	    {
	      p->red = RED16_FROM_ULONG (p->pixel);
	      p->green = GREEN16_FROM_ULONG (p->pixel);
	      p->blue = BLUE16_FROM_ULONG (p->pixel);
	    }
	}
#endif	/* USE_CAIRO || HAVE_NS */
    }

  image_unget_x_image_or_dc (img, 0, ximg, prev);

  return colors;
}

#ifdef HAVE_NTGUI

/* Put a pixel of COLOR at position X, Y in XIMG.  XIMG must have been
   created with CreateDIBSection, with the pointer to the bit values
   stored in ximg->data.  */

static void
XPutPixel (XImage *ximg, int x, int y, COLORREF color)
{
  int width = ximg->info.bmiHeader.biWidth;
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
    image_error ("XPutPixel: palette image not supported");
}

#endif /* HAVE_NTGUI */

/* Create IMG->pixmap from an array COLORS of Emacs_Color structures, whose
   RGB members are set.  F is the frame on which this all happens.
   COLORS will be freed; an existing IMG->pixmap will be freed, too.  */

static void
image_from_emacs_colors (struct frame *f, struct image *img, Emacs_Color *colors)
{
  int x, y;
  Emacs_Pix_Container ximage;
  Emacs_Color *p;

  ximage = NULL;

  init_color_table ();

  image_clear_image_1 (f, img, CLEAR_IMAGE_PIXMAP | CLEAR_IMAGE_COLORS);
  image_create_x_image_and_pixmap (f, img, img->width, img->height, 0,
				   &ximage, 0);
  p = colors;
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x, ++p)
      {
	unsigned long pixel;
	pixel = lookup_rgb_color (f, p->red, p->green, p->blue);
	PUT_PIXEL (ximage, x, y, pixel);
      }

  xfree (colors);

  image_put_x_image (f, img, ximage, false);
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
image_detect_edges (struct frame *f, struct image *img,
                    int *matrix, int color_adjust)
{
  Emacs_Color *colors = image_to_emacs_colors (f, img, 1);
  Emacs_Color *new, *p;
  int x, y, i, sum;
  ptrdiff_t nbytes;

  for (i = sum = 0; i < 9; ++i)
    sum += eabs (matrix[i]);

#define COLOR(A, X, Y) ((A) + (Y) * img->width + (X))

  if (INT_MULTIPLY_WRAPV (sizeof *new, img->width, &nbytes)
      || INT_MULTIPLY_WRAPV (img->height, nbytes, &nbytes))
    memory_full (SIZE_MAX);
  new = xmalloc (nbytes);

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
	  int r, g, b, yy, xx;

	  r = g = b = i = 0;
	  for (yy = y - 1; yy < y + 2; ++yy)
	    for (xx = x - 1; xx < x + 2; ++xx, ++i)
	      if (matrix[i])
	        {
	          Emacs_Color *t = COLOR (colors, xx, yy);
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
  image_from_emacs_colors (f, img, new);

#undef COLOR
}


/* Perform the pre-defined `emboss' edge-detection on image IMG
   on frame F.  */

static void
image_emboss (struct frame *f, struct image *img)
{
  image_detect_edges (f, img, emboss_matrix, 0xffff / 2);
}


/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

static void
image_laplace (struct frame *f, struct image *img)
{
  image_detect_edges (f, img, laplace_matrix, 45000);
}


/* Perform edge-detection on image IMG on frame F, with specified
   transformation matrix MATRIX and color-adjustment COLOR_ADJUST.

   MATRIX must be either

   - a list of at least 9 numbers in row-major form
   - a vector of at least 9 numbers

   COLOR_ADJUST nil means use a default; otherwise it must be a
   number.  */

static void
image_edge_detection (struct frame *f, struct image *img,
                      Lisp_Object matrix, Lisp_Object color_adjust)
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
    color_adjust = make_fixnum (0xffff / 2);

  if (i == 9 && NUMBERP (color_adjust))
    image_detect_edges (f, img, trans, XFLOATINT (color_adjust));
}


#if defined HAVE_X_WINDOWS || defined USE_CAIRO || defined HAVE_HAIKU
static void
image_pixmap_draw_cross (struct frame *f, Emacs_Pixmap pixmap,
			 int x, int y, unsigned int width, unsigned int height,
			 unsigned long color)
{
#ifdef USE_CAIRO
  cairo_surface_t *surface
    = cairo_image_surface_create_for_data ((unsigned char *) pixmap->data,
					   (pixmap->bits_per_pixel == 32
					    ? CAIRO_FORMAT_RGB24
					    : CAIRO_FORMAT_A8),
					   pixmap->width, pixmap->height,
					   pixmap->bytes_per_line);
  cairo_t *cr = cairo_create (surface);
  cairo_surface_destroy (surface);
  cairo_set_source_rgb (cr, RED_FROM_ULONG (color) / 255.0,
			GREEN_FROM_ULONG (color) / 255.0,
			BLUE_FROM_ULONG (color) / 255.0);
  cairo_move_to (cr, x + 0.5, y + 0.5);
  cairo_rel_line_to (cr, width - 1, height - 1);
  cairo_rel_move_to (cr, 0, - (height - 1));
  cairo_rel_line_to (cr, - (width - 1), height - 1);
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  cairo_destroy (cr);
#elif HAVE_X_WINDOWS
  Display *dpy = FRAME_X_DISPLAY (f);
  GC gc = XCreateGC (dpy, pixmap, 0, NULL);

  XSetForeground (dpy, gc, color);
  XDrawLine (dpy, pixmap, gc, x, y, x + width - 1, y + height - 1);
  XDrawLine (dpy, pixmap, gc, x, y + height - 1, x + width - 1, y);
  XFreeGC (dpy, gc);
#elif HAVE_HAIKU
  be_draw_cross_on_pixmap (pixmap, x, y, width, height, color);
#endif
}
#endif	/* HAVE_X_WINDOWS || USE_CAIRO || HAVE_HAIKU */

/* Transform image IMG on frame F so that it looks disabled.  */

static void
image_disable_image (struct frame *f, struct image *img)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
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
      Emacs_Color *colors = image_to_emacs_colors (f, img, 1);
      Emacs_Color *p, *end;
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

      image_from_emacs_colors (f, img, colors);
    }

  /* Draw a cross over the disabled image, if we must or if we
     should.  */
  if (n_planes < 2 || cross_disabled_images)
    {
#ifndef HAVE_NTGUI
#ifndef HAVE_NS  /* TODO: NS support, however this not needed for toolbars */

#if !defined USE_CAIRO && !defined HAVE_HAIKU
#define CrossForeground(f) BLACK_PIX_DEFAULT (f)
#define MaskForeground(f)  WHITE_PIX_DEFAULT (f)
#else  /* USE_CAIRO || HAVE_HAIKU */
#define CrossForeground(f) 0
#define MaskForeground(f)  PIX_MASK_DRAW
#endif	/* USE_CAIRO || HAVE_HAIKU */

#if !defined USE_CAIRO && !defined HAVE_HAIKU
      image_sync_to_pixmaps (f, img);
#endif	/* !USE_CAIRO && !HAVE_HAIKU */
      image_pixmap_draw_cross (f, img->pixmap, 0, 0, img->width, img->height,
			       CrossForeground (f));
      if (img->mask)
	image_pixmap_draw_cross (f, img->mask, 0, 0, img->width, img->height,
				 MaskForeground (f));
#endif /* !HAVE_NS */
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
   heuristically.  */

static void
image_build_heuristic_mask (struct frame *f, struct image *img,
                            Lisp_Object how)
{
  Emacs_Pix_Context ximg;
#ifdef HAVE_NTGUI
  HGDIOBJ prev;
  char *mask_img;
  int row_width;
#elif !defined HAVE_NS
  Emacs_Pix_Container mask_img;
#endif
  int x, y;
  bool use_img_background;
  unsigned long bg = 0;

  if (img->mask)
    image_clear_image_1 (f, img, CLEAR_IMAGE_MASK);

#ifndef HAVE_NTGUI
#ifndef HAVE_NS
  /* Create an image and pixmap serving as mask.  */
  if (! image_create_x_image_and_pixmap (f, img, img->width, img->height, 1,
					 &mask_img, 1))
    return;
#endif /* !HAVE_NS */
#else
  /* Create the bit array serving as mask.  */
  row_width = (img->width + 7) / 8;
  mask_img = xzalloc (row_width * img->height);
#endif /* HAVE_NTGUI */

  /* Get the X image or create a memory device context for IMG.  */
  ximg = image_get_x_image_or_dc (f, img, 0, &prev);

  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, use the image's background color. */
  use_img_background = 1;

  if (CONSP (how))
    {
      int rgb[3], i;

      for (i = 0; i < 3 && CONSP (how) && FIXNATP (XCAR (how)); ++i)
	{
	  rgb[i] = XFIXNAT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
#ifndef USE_CAIRO
	  char color_name[30];
	  sprintf (color_name, "#%04x%04x%04x",
		   rgb[0] + 0u, rgb[1] + 0u, rgb[2] + 0u);
	  bg = (
#ifdef HAVE_NTGUI
		0x00ffffff & /* Filter out palette info.  */
#endif /* HAVE_NTGUI */
		image_alloc_image_color (f, img, build_string (color_name), 0));
#else  /* USE_CAIRO */
	  bg = lookup_rgb_color (f, rgb[0], rgb[1], rgb[2]);
#endif	/* USE_CAIRO */
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
#ifndef HAVE_NS
      PUT_PIXEL (mask_img, x, y, (GET_PIXEL (ximg, x, y) != bg
				  ? PIX_MASK_DRAW : PIX_MASK_RETAIN));
#else
      if (XGetPixel (ximg, x, y) == bg)
        ns_set_alpha (ximg, x, y, 0);
#endif /* HAVE_NS */
#ifndef HAVE_NS
  /* Fill in the background_transparent field while we have the mask handy. */
  image_background_transparent (img, f, mask_img);

  /* Put mask_img into the image.  */
  image_put_x_image (f, img, mask_img, 1);
#endif /* !HAVE_NS */
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

  /* Was: image_destroy_x_image ((XImagePtr )mask_img); which seems bogus ++kfs */
  xfree (mask_img);
#endif /* HAVE_NTGUI */

  image_unget_x_image_or_dc (img, 0, ximg, prev);
}


/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/

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

static const struct image_keyword pbm_format[PBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid PBM image specification.  */

static bool
pbm_image_p (Lisp_Object object)
{
  struct image_keyword fmt[PBM_LAST];

  memcpy (fmt, pbm_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm))
    return 0;

  /* Must specify either :data or :file.  */
  return fmt[PBM_DATA].count + fmt[PBM_FILE].count == 1;
}


/* Get next char skipping comments in Netpbm header.  Returns -1 at
   end of input.  */

static int
pbm_next_char (char **s, char *end)
{
  while (*s < end)
    {
      unsigned char c = *(*s)++;
      if (c != '#')
	return c;
      while (*s < end)
	{
	  c = *(*s)++;
	  if (c == '\n' || c == '\r')
	    break;
	}
    }

  return -1;
}


/* Scan a decimal number from *S and return it.  Advance *S while
   reading the number.  END is the end of the string.  Value is -1 at
   end of input.  */

static int
pbm_scan_number (char **s, char *end)
{
  int c = 0, val = -1;

  /* Skip white-space.  */
  while ((c = pbm_next_char (s, end)) != -1 && c_isspace (c))
    ;

  if (c_isdigit (c))
    {
      /* Read decimal number.  */
      val = c - '0';
      while ((c = pbm_next_char (s, end)) != -1 && c_isdigit (c))
        val = 10 * val + c - '0';
    }

  return val;
}

/* Scan an index from *S and return it.  It is a one-byte unsigned
   index if !TWO_BYTE, and a two-byte big-endian unsigned index if
   TWO_BYTE.  */

static int
pbm_scan_index (char **s, bool two_byte)
{
  char *p = *s;
  unsigned char c0 = *p++;
  int n = c0;
  if (two_byte)
    {
      unsigned char c1 = *p++;
      n = (n << 8) + c1;
    }
  *s = p;
  return n;
}


/* Load PBM image IMG for use on frame F.  */

static bool
pbm_load (struct frame *f, struct image *img)
{
  bool raw_p;
  int x, y;
  int width, height, max_color_idx = 0;
  Lisp_Object specified_file;
  enum {PBM_MONO, PBM_GRAY, PBM_COLOR} type;
  char *contents = NULL;
  char *end, *p;
  Emacs_Pix_Container ximg;

  specified_file = image_spec_value (img->spec, QCfile, NULL);

  if (STRINGP (specified_file))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (specified_file, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
	  return 0;
	}

      ptrdiff_t size;
      contents = slurp_file (fd, &size);
      if (contents == NULL)
	{
	  image_error ("Error reading `%s'", file);
	  return 0;
	}

      p = contents;
      end = contents + size;
    }
  else
    {
      Lisp_Object data;
      data = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (data))
	{
	  image_error ("Invalid image data `%s'", data);
	  return 0;
	}
      p = SSDATA (data);
      end = p + SBYTES (data);
    }

  /* Check magic number.  */
  if (end - p < 2 || *p++ != 'P')
    {
      image_error ("Not a PBM image: `%s'", img->spec);
    error:
      xfree (contents);
      img->pixmap = NO_PIXMAP;
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
      image_error ("Not a PBM image: `%s'", img->spec);
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
	  image_error ("Unsupported maximum PBM color value");
	  goto error;
	}
    }

  if (!check_image_size (f, width, height))
    {
      image_size_error ();
      goto error;
    }

  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
    goto error;

  /* Initialize the color hash table.  */
  init_color_table ();

  if (type == PBM_MONO)
    {
      unsigned char c = 0;
      int g;
      struct image_keyword fmt[PBM_LAST];
      unsigned long fg = img->face_foreground;
      unsigned long bg = img->face_background;
      /* Parse the image specification.  */
      memcpy (fmt, pbm_format, sizeof fmt);
      parse_image_spec (img->spec, fmt, PBM_LAST, Qpbm);

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[PBM_FOREGROUND].count
	  && STRINGP (fmt[PBM_FOREGROUND].value))
	fg = image_alloc_image_color (f, img, fmt[PBM_FOREGROUND].value, fg);
      if (fmt[PBM_BACKGROUND].count
	  && STRINGP (fmt[PBM_BACKGROUND].value))
	{
	  bg = image_alloc_image_color (f, img, fmt[PBM_BACKGROUND].value, bg);
	  img->background = bg;
	  img->background_valid = 1;
	}

#ifdef USE_CAIRO
      {
	Emacs_Color fgbg[] = {{.pixel = fg}, {.pixel = bg}};
	FRAME_TERMINAL (f)->query_colors (f, fgbg, ARRAYELTS (fgbg));
	fg = lookup_rgb_color (f, fgbg[0].red, fgbg[0].green, fgbg[0].blue);
	bg = lookup_rgb_color (f, fgbg[1].red, fgbg[1].green, fgbg[1].blue);
      }
#endif
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    if (raw_p)
	      {
		if ((x & 7) == 0)
		  {
		    if (p >= end)
		      {
			image_destroy_x_image (ximg);
			image_clear_image (f, img);
			image_error ("Invalid image size in image `%s'",
				     img->spec);
			goto error;
		      }
		    c = *p++;
		  }
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      {
		int c = 0;
		/* Skip white-space and comments.  */
		while ((c = pbm_next_char (&p, end)) != -1 && c_isspace (c))
		  ;

		if (c == '0' || c == '1')
		  g = c - '0';
		else
		  g = 0;
	      }

	    PUT_PIXEL (ximg, x, y, g ? fg : bg);
	  }
    }
  else
    {
      int expected_size = height * width;
      bool two_byte = 255 < max_color_idx;
      if (two_byte)
	expected_size *= 2;
      if (type == PBM_COLOR)
	expected_size *= 3;

      if (raw_p && p + expected_size > end)
	{
	  image_destroy_x_image (ximg);
	  image_clear_image (f, img);
	  image_error ("Invalid image size in image `%s'", img->spec);
	  goto error;
	}

      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    int r, g, b;

	    if (type == PBM_GRAY && raw_p)
	      r = g = b = pbm_scan_index (&p, two_byte);
	    else if (type == PBM_GRAY)
	      r = g = b = pbm_scan_number (&p, end);
	    else if (raw_p)
	      {
		r = pbm_scan_index (&p, two_byte);
		g = pbm_scan_index (&p, two_byte);
		b = pbm_scan_index (&p, two_byte);
	      }
	    else
	      {
		r = pbm_scan_number (&p, end);
		g = pbm_scan_number (&p, end);
		b = pbm_scan_number (&p, end);
	      }

	    if (r < 0 || g < 0 || b < 0)
	      {
		image_destroy_x_image (ximg);
		image_error ("Invalid pixel value in image `%s'", img->spec);
		goto error;
	      }

	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xffff supported by X.  */
	    r = (double) r * 65535 / max_color_idx;
	    g = (double) g * 65535 / max_color_idx;
	    b = (double) b * 65535 / max_color_idx;
	    PUT_PIXEL (ximg, x, y, lookup_rgb_color (f, r, g, b));
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
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  /* X and W32 versions did it here, MAC version above.  ++kfs
     img->width = width;
     img->height = height; */

  xfree (contents);
  return 1;
}


/***********************************************************************
			    NATIVE IMAGE HANDLING
 ***********************************************************************/

#if HAVE_NATIVE_IMAGE_API
static bool
image_can_use_native_api (Lisp_Object type)
{
# ifdef HAVE_NTGUI
  return w32_can_use_native_image_api (type);
# elif defined HAVE_NS
  return ns_can_use_native_image_api (type);
# elif defined HAVE_HAIKU
  return haiku_can_use_native_image_api (type);
# else
  return false;
# endif
}

/*
 * These functions are actually defined in the OS-native implementation file.
 * Currently, for Windows GDI+ interface, w32image.c, and nsimage.m for macOS.
 */

/* Indices of image specification fields in native format, below.  */
enum native_image_keyword_index
{
  NATIVE_IMAGE_TYPE,
  NATIVE_IMAGE_DATA,
  NATIVE_IMAGE_FILE,
  NATIVE_IMAGE_ASCENT,
  NATIVE_IMAGE_MARGIN,
  NATIVE_IMAGE_RELIEF,
  NATIVE_IMAGE_ALGORITHM,
  NATIVE_IMAGE_HEURISTIC_MASK,
  NATIVE_IMAGE_MASK,
  NATIVE_IMAGE_BACKGROUND,
  NATIVE_IMAGE_INDEX,
  NATIVE_IMAGE_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */
static const struct image_keyword native_image_format[] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0}
};

/* Return true if OBJECT is a valid native API image specification.  */

static bool
native_image_p (Lisp_Object object)
{
  struct image_keyword fmt[NATIVE_IMAGE_LAST];
  memcpy (fmt, native_image_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, 10, Qnative_image))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[NATIVE_IMAGE_FILE].count + fmt[NATIVE_IMAGE_DATA].count == 1;
}

static bool
native_image_load (struct frame *f, struct image *img)
{
  Lisp_Object image_file = image_spec_value (img->spec, QCfile, NULL);

  if (STRINGP (image_file))
    image_file = image_find_image_file (image_file);

# ifdef HAVE_NTGUI
  return w32_load_image (f, img, image_file,
                         image_spec_value (img->spec, QCdata, NULL));
# elif defined HAVE_NS
  return ns_load_image (f, img, image_file,
                        image_spec_value (img->spec, QCdata, NULL));
# elif defined HAVE_HAIKU
  return haiku_load_image (f, img, image_file,
			   image_spec_value (img->spec, QCdata, NULL));
# else
  return 0;
# endif
}

#endif	/* HAVE_NATIVE_IMAGE_API */


/***********************************************************************
				 PNG
 ***********************************************************************/

#if defined (HAVE_PNG)

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

static const struct image_keyword png_format[PNG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid PNG image specification.  */

static bool
png_image_p (Lisp_Object object)
{
  struct image_keyword fmt[PNG_LAST];
  memcpy (fmt, png_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, PNG_LAST, Qpng))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[PNG_FILE].count + fmt[PNG_DATA].count == 1;
}

#endif /* HAVE_PNG */


#ifdef HAVE_PNG

# ifdef WINDOWSNT
/* PNG library details.  */

DEF_DLL_FN (png_voidp, png_get_io_ptr, (png_structp));
DEF_DLL_FN (int, png_sig_cmp, (png_bytep, png_size_t, png_size_t));
DEF_DLL_FN (png_structp, png_create_read_struct,
	    (png_const_charp, png_voidp, png_error_ptr, png_error_ptr));
DEF_DLL_FN (png_infop, png_create_info_struct, (png_structp));
DEF_DLL_FN (void, png_destroy_read_struct,
	    (png_structpp, png_infopp, png_infopp));
DEF_DLL_FN (void, png_set_read_fn, (png_structp, png_voidp, png_rw_ptr));
DEF_DLL_FN (void, png_set_sig_bytes, (png_structp, int));
DEF_DLL_FN (void, png_read_info, (png_structp, png_infop));
DEF_DLL_FN (png_uint_32, png_get_IHDR,
	    (png_structp, png_infop, png_uint_32 *, png_uint_32 *,
	     int *, int *, int *, int *, int *));
#  ifdef PNG_tRNS_SUPPORTED
DEF_DLL_FN (png_uint_32, png_get_tRNS, (png_structp, png_infop, png_bytep *,
					int *, png_color_16p *));
#  endif
DEF_DLL_FN (void, png_set_strip_16, (png_structp));
DEF_DLL_FN (void, png_set_expand, (png_structp));
DEF_DLL_FN (void, png_set_gray_to_rgb, (png_structp));
DEF_DLL_FN (int, png_set_interlace_handling, (png_structp));
DEF_DLL_FN (void, png_set_background,
	    (png_structp, png_color_16p, int, int, double));
DEF_DLL_FN (png_uint_32, png_get_bKGD,
	    (png_structp, png_infop, png_color_16p *));
DEF_DLL_FN (void, png_read_update_info, (png_structp, png_infop));
DEF_DLL_FN (png_byte, png_get_channels, (png_structp, png_infop));
DEF_DLL_FN (png_size_t, png_get_rowbytes, (png_structp, png_infop));
DEF_DLL_FN (void, png_read_image, (png_structp, png_bytepp));
DEF_DLL_FN (void, png_read_end, (png_structp, png_infop));
DEF_DLL_FN (void, png_error, (png_structp, png_const_charp));

#  if (PNG_LIBPNG_VER >= 10500)
DEF_DLL_FN (void, png_longjmp, (png_structp, int) PNG_NORETURN);
DEF_DLL_FN (jmp_buf *, png_set_longjmp_fn,
	    (png_structp, png_longjmp_ptr, size_t));
#  endif /* libpng version >= 1.5 */

static bool
init_png_functions (void)
{
  HMODULE library;

  if (!(library = w32_delayed_load (Qpng)))
    return 0;

  LOAD_DLL_FN (library, png_get_io_ptr);
  LOAD_DLL_FN (library, png_sig_cmp);
  LOAD_DLL_FN (library, png_create_read_struct);
  LOAD_DLL_FN (library, png_create_info_struct);
  LOAD_DLL_FN (library, png_destroy_read_struct);
  LOAD_DLL_FN (library, png_set_read_fn);
  LOAD_DLL_FN (library, png_set_sig_bytes);
  LOAD_DLL_FN (library, png_read_info);
  LOAD_DLL_FN (library, png_get_IHDR);
#  ifdef PNG_tRNS_SUPPORTED
  LOAD_DLL_FN (library, png_get_tRNS);
#  endif
  LOAD_DLL_FN (library, png_set_strip_16);
  LOAD_DLL_FN (library, png_set_expand);
  LOAD_DLL_FN (library, png_set_gray_to_rgb);
  LOAD_DLL_FN (library, png_set_interlace_handling);
  LOAD_DLL_FN (library, png_set_background);
  LOAD_DLL_FN (library, png_get_bKGD);
  LOAD_DLL_FN (library, png_read_update_info);
  LOAD_DLL_FN (library, png_get_channels);
  LOAD_DLL_FN (library, png_get_rowbytes);
  LOAD_DLL_FN (library, png_read_image);
  LOAD_DLL_FN (library, png_read_end);
  LOAD_DLL_FN (library, png_error);

#  if (PNG_LIBPNG_VER >= 10500)
  LOAD_DLL_FN (library, png_longjmp);
  LOAD_DLL_FN (library, png_set_longjmp_fn);
#  endif /* libpng version >= 1.5 */

  return 1;
}

#  undef png_create_info_struct
#  undef png_create_read_struct
#  undef png_destroy_read_struct
#  undef png_error
#  undef png_get_bKGD
#  undef png_get_channels
#  undef png_get_IHDR
#  undef png_get_io_ptr
#  undef png_get_rowbytes
#  undef png_get_tRNS
#  undef png_longjmp
#  undef png_read_end
#  undef png_read_image
#  undef png_read_info
#  undef png_read_update_info
#  undef png_set_background
#  undef png_set_expand
#  undef png_set_gray_to_rgb
#  undef png_set_interlace_handling
#  undef png_set_longjmp_fn
#  undef png_set_read_fn
#  undef png_set_sig_bytes
#  undef png_set_strip_16
#  undef png_sig_cmp

#  define png_create_info_struct fn_png_create_info_struct
#  define png_create_read_struct fn_png_create_read_struct
#  define png_destroy_read_struct fn_png_destroy_read_struct
#  define png_error fn_png_error
#  define png_get_bKGD fn_png_get_bKGD
#  define png_get_channels fn_png_get_channels
#  define png_get_IHDR fn_png_get_IHDR
#  define png_get_io_ptr fn_png_get_io_ptr
#  define png_get_rowbytes fn_png_get_rowbytes
#  define png_get_tRNS fn_png_get_tRNS
#  define png_longjmp fn_png_longjmp
#  define png_read_end fn_png_read_end
#  define png_read_image fn_png_read_image
#  define png_read_info fn_png_read_info
#  define png_read_update_info fn_png_read_update_info
#  define png_set_background fn_png_set_background
#  define png_set_expand fn_png_set_expand
#  define png_set_gray_to_rgb fn_png_set_gray_to_rgb
#  define png_set_interlace_handling fn_png_set_interlace_handling
#  define png_set_longjmp_fn fn_png_set_longjmp_fn
#  define png_set_read_fn fn_png_set_read_fn
#  define png_set_sig_bytes fn_png_set_sig_bytes
#  define png_set_strip_16 fn_png_set_strip_16
#  define png_sig_cmp fn_png_sig_cmp

# endif /* WINDOWSNT */

/* Fast implementations of setjmp and longjmp.  Although setjmp and longjmp
   will do, POSIX _setjmp and _longjmp (if available) are often faster.
   Do not use sys_setjmp, as PNG supports only jmp_buf.
   It's OK if the longjmp substitute restores the signal mask.  */
# ifdef HAVE__SETJMP
#  define FAST_SETJMP(j) _setjmp (j)
#  define FAST_LONGJMP _longjmp
# else
#  define FAST_SETJMP(j) setjmp (j)
#  define FAST_LONGJMP longjmp
# endif

# if PNG_LIBPNG_VER < 10500
#  define PNG_LONGJMP(ptr) FAST_LONGJMP ((ptr)->jmpbuf, 1)
#  define PNG_JMPBUF(ptr) ((ptr)->jmpbuf)
# else
/* In libpng version 1.5, the jmpbuf member is hidden. (Bug#7908)  */
#  define PNG_LONGJMP(ptr) png_longjmp (ptr, 1)
#  define PNG_JMPBUF(ptr) \
     (*png_set_longjmp_fn (ptr, FAST_LONGJMP, sizeof (jmp_buf)))
# endif

/* Error and warning handlers installed when the PNG library
   is initialized.  */

static AVOID
my_png_error (png_struct *png_ptr, const char *msg)
{
  eassert (png_ptr != NULL);
  /* Avoid compiler warning about deprecated direct access to
     png_ptr's fields in libpng versions 1.4.x.  */
  image_error ("PNG error: %s", build_string (msg));
  PNG_LONGJMP (png_ptr);
}


static void
my_png_warning (png_struct *png_ptr, const char *msg)
{
  eassert (png_ptr != NULL);
  image_error ("PNG warning: %s", build_string (msg));
}

/* Memory source for PNG decoding.  */

struct png_memory_storage
{
  unsigned char *bytes;		/* The data       */
  ptrdiff_t len;		/* How big is it? */
  ptrdiff_t index;		/* Where are we?  */
};


/* Function set as reader function when reading PNG image from memory.
   PNG_PTR is a pointer to the PNG control structure.  Copy LENGTH
   bytes from the input to DATA.  */

static void
png_read_from_memory (png_structp png_ptr, png_bytep data, png_size_t length)
{
  struct png_memory_storage *tbr = png_get_io_ptr (png_ptr);

  if (length > tbr->len - tbr->index)
    png_error (png_ptr, "Read error");

  memcpy (data, tbr->bytes + tbr->index, length);
  tbr->index = tbr->index + length;
}


/* Function set as reader function when reading PNG image from a file.
   PNG_PTR is a pointer to the PNG control structure.  Copy LENGTH
   bytes from the input to DATA.  */

static void
png_read_from_file (png_structp png_ptr, png_bytep data, png_size_t length)
{
  FILE *fp = png_get_io_ptr (png_ptr);

  if (fread (data, 1, length, fp) < length)
    png_error (png_ptr, "Read error");
}


/* Load PNG image IMG for use on frame F.  Value is true if
   successful.  */

struct png_load_context
{
  /* These are members so that longjmp doesn't munge local variables.  */
  png_struct *png_ptr;
  png_info *info_ptr;
  png_info *end_info;
  FILE *fp;
  png_byte *pixels;
  png_byte **rows;
};

static bool
png_load_body (struct frame *f, struct image *img, struct png_load_context *c)
{
  Lisp_Object specified_file, specified_data;
  FILE *fp = NULL;
  int x, y;
  ptrdiff_t i;
  png_struct *png_ptr;
  png_info *info_ptr = NULL, *end_info = NULL;
  png_byte sig[8];
  png_byte *pixels = NULL;
  png_byte **rows = NULL;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  png_byte channels;
  png_uint_32 row_bytes;
  bool transparent_p;
  struct png_memory_storage tbr;  /* Data to be read */
  ptrdiff_t nbytes;
  Emacs_Pix_Container ximg, mask_img = NULL;

  /* Find out what file to load.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  if (NILP (specified_data))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (specified_file, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
	  return 0;
	}

      /* Open the image file.  */
      fp = fdopen (fd, "rb");
      if (!fp)
	{
	  image_error ("Cannot open image file `%s'", file);
	  return 0;
	}

      /* Check PNG signature.  */
      if (fread (sig, 1, sizeof sig, fp) != sizeof sig
	  || png_sig_cmp (sig, 0, sizeof sig))
	{
	  fclose (fp);
	  image_error ("Not a PNG file: `%s'", file);
	  return 0;
	}
    }
  else
    {
      if (!STRINGP (specified_data))
	{
	  image_error ("Invalid image data `%s'", specified_data);
	  return 0;
	}

      /* Read from memory.  */
      tbr.bytes = SDATA (specified_data);
      tbr.len = SBYTES (specified_data);
      tbr.index = 0;

      /* Check PNG signature.  */
      if (tbr.len < sizeof sig
	  || png_sig_cmp (tbr.bytes, 0, sizeof sig))
	{
	  image_error ("Not a PNG image: `%s'", img->spec);
	  return 0;
	}

      /* Need to skip past the signature.  */
      tbr.bytes += sizeof (sig);
    }

  /* Initialize read and info structs for PNG lib.  */
  png_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING,
				       NULL, my_png_error,
				       my_png_warning);
  if (png_ptr)
    {
      info_ptr = png_create_info_struct (png_ptr);
      end_info = png_create_info_struct (png_ptr);
    }

  c->png_ptr = png_ptr;
  c->info_ptr = info_ptr;
  c->end_info = end_info;
  c->fp = fp;
  c->pixels = pixels;
  c->rows = rows;

  if (! (info_ptr && end_info))
    {
      png_destroy_read_struct (&c->png_ptr, &c->info_ptr, &c->end_info);
      png_ptr = 0;
    }
  if (! png_ptr)
    {
      if (fp) fclose (fp);
      return 0;
    }

  /* Set error jump-back.  We come back here when the PNG library
     detects an error.  */
  if (FAST_SETJMP (PNG_JMPBUF (png_ptr)))
    {
    error:
      if (c->png_ptr)
	png_destroy_read_struct (&c->png_ptr, &c->info_ptr, &c->end_info);
      xfree (c->pixels);
      xfree (c->rows);
      if (c->fp)
	fclose (c->fp);
      return 0;
    }

  /* Read image info.  */
  if (!NILP (specified_data))
    png_set_read_fn (png_ptr, &tbr, png_read_from_memory);
  else
    png_set_read_fn (png_ptr, fp, png_read_from_file);

  png_set_sig_bytes (png_ptr, sizeof sig);
  png_read_info (png_ptr, info_ptr);
  png_get_IHDR (png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
		&interlace_type, NULL, NULL);

  if (! (width <= INT_MAX && height <= INT_MAX
	 && check_image_size (f, width, height)))
    {
      image_size_error ();
      goto error;
    }

  /* Create the X image and pixmap now, so that the work below can be
     omitted if the image is too large for X.  */
  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
    goto error;

  /* If image contains simply transparency data, we prefer to
     construct a clipping mask.  */
  transparent_p = false;
# ifdef PNG_tRNS_SUPPORTED
  png_bytep trans_alpha;
  int num_trans;
  if (png_get_tRNS (png_ptr, info_ptr, &trans_alpha, &num_trans, NULL))
    {
      transparent_p = true;
      if (trans_alpha)
	for (int i = 0; i < num_trans; i++)
	  if (0 < trans_alpha[i] && trans_alpha[i] < 255)
	    {
	      transparent_p = false;
	      break;
	    }
    }
# endif

  /* This function is easier to write if we only have to handle
     one data format: RGB or RGBA with 8 bits per channel.  Let's
     transform other formats into that format.  */

  /* Strip more than 8 bits per channel.  */
  if (bit_depth == 16)
    png_set_strip_16 (png_ptr);

  /* Expand data to 24 bit RGB, or 8 bit grayscale, with alpha channel
     if available.  */
  png_set_expand (png_ptr);

  /* Convert grayscale images to RGB.  */
  if (color_type == PNG_COLOR_TYPE_GRAY
      || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    png_set_gray_to_rgb (png_ptr);

  /* Handle alpha channel by combining the image with a background
     color.  Do this only if a real alpha channel is supplied.  For
     simple transparency, we prefer a clipping mask.  */
  if (!transparent_p)
    {
      /* png_color_16 *image_bg; */
      Lisp_Object specified_bg
	= image_spec_value (img->spec, QCbackground, NULL);
      Emacs_Color color;

      /* If the user specified a color, try to use it; if not, use the
	 current frame background, ignoring any default background
	 color set by the image.  */
      if (STRINGP (specified_bg)
	  ? FRAME_TERMINAL (f)->defined_color_hook (f,
                                                    SSDATA (specified_bg),
                                                    &color,
                                                    false,
                                                    false)
	  : (FRAME_TERMINAL (f)->query_frame_background_color (f, &color),
             true))
	/* The user specified `:background', use that.  */
	{
	  int shift = bit_depth == 16 ? 0 : 8;
	  png_color_16 bg = { 0 };
	  bg.red = color.red >> shift;
	  bg.green = color.green >> shift;
	  bg.blue = color.blue >> shift;

	  png_set_background (png_ptr, &bg,
			      PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
	}
    }

  png_set_interlace_handling (png_ptr);
  png_read_update_info (png_ptr, info_ptr);

  /* Get number of channels.  Valid values are 1 for grayscale images
     and images with a palette, 2 for grayscale images with transparency
     information (alpha channel), 3 for RGB images, and 4 for RGB
     images with alpha channel, i.e. RGBA.  If conversions above were
     sufficient we should only have 3 or 4 channels here.  */
  channels = png_get_channels (png_ptr, info_ptr);
  eassert (channels == 3 || channels == 4);

  /* Number of bytes needed for one row of the image.  */
  row_bytes = png_get_rowbytes (png_ptr, info_ptr);

  /* Allocate memory for the image.  */
  if (INT_MULTIPLY_WRAPV (row_bytes, sizeof *pixels, &nbytes)
      || INT_MULTIPLY_WRAPV (nbytes, height, &nbytes))
    memory_full (SIZE_MAX);
  c->pixels = pixels = xmalloc (nbytes);
  c->rows = rows = xmalloc (height * sizeof *rows);
  for (i = 0; i < height; ++i)
    rows[i] = pixels + i * row_bytes;

  /* Read the entire image.  */
  png_read_image (png_ptr, rows);
  png_read_end (png_ptr, info_ptr);
  if (fp)
    {
      fclose (fp);
      c->fp = NULL;
    }

  /* Create an image and pixmap serving as mask if the PNG image
     contains an alpha channel.  */
  if (channels == 4
      && transparent_p
      && !image_create_x_image_and_pixmap (f, img, width, height, 1,
					   &mask_img, 1))
    {
      image_destroy_x_image (ximg);
      image_clear_image_1 (f, img, CLEAR_IMAGE_PIXMAP);
      goto error;
    }

  /* Fill the X image and mask from PNG data.  */
  init_color_table ();

  for (y = 0; y < height; ++y)
    {
      png_byte *p = rows[y];

      for (x = 0; x < width; ++x)
	{
	  int r, g, b;

	  r = *p++ << 8;
	  g = *p++ << 8;
	  b = *p++ << 8;
	  PUT_PIXEL (ximg, x, y, lookup_rgb_color (f, r, g, b));
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
		PUT_PIXEL (mask_img, x, y, *p > 0 ? PIX_MASK_DRAW : PIX_MASK_RETAIN);
	      ++p;
	    }
	}
    }

  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Set IMG's background color from the PNG image, unless the user
       overrode it.  */
    {
      png_color_16 *bg;
      if (png_get_bKGD (png_ptr, info_ptr, &bg))
	{
#ifndef USE_CAIRO
	  img->background = lookup_rgb_color (f, bg->red, bg->green, bg->blue);
#else  /* USE_CAIRO */
	  char color_name[30];
	  sprintf (color_name, "#%04x%04x%04x", bg->red, bg->green, bg->blue);
	  img->background
	    = image_alloc_image_color (f, img, build_string (color_name), 0);
#endif /* USE_CAIRO */
	  img->background_valid = 1;
	}
    }

# ifdef COLOR_TABLE_SUPPORT
  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
# endif /* COLOR_TABLE_SUPPORT */

  /* Clean up.  */
  png_destroy_read_struct (&c->png_ptr, &c->info_ptr, &c->end_info);
  xfree (rows);
  xfree (pixels);

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy.
     Casting avoids a GCC warning.  */
  IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  /* Same for the mask.  */
  if (mask_img)
    {
      /* Fill in the background_transparent field while we have the
	 mask handy.  Casting avoids a GCC warning.  */
      image_background_transparent (img, f, (Emacs_Pix_Context)mask_img);

      image_put_x_image (f, img, mask_img, 1);
    }

  return 1;
}

static bool
png_load (struct frame *f, struct image *img)
{
  struct png_load_context c;
  return png_load_body (f, img, &c);
}

#endif /* HAVE_PNG */



/***********************************************************************
				 JPEG
 ***********************************************************************/

#if defined (HAVE_JPEG)

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

static const struct image_keyword jpeg_format[JPEG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid JPEG image specification.  */

static bool
jpeg_image_p (Lisp_Object object)
{
  struct image_keyword fmt[JPEG_LAST];

  memcpy (fmt, jpeg_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[JPEG_FILE].count + fmt[JPEG_DATA].count == 1;
}

#endif /* HAVE_JPEG */

#ifdef HAVE_JPEG

/* Work around a warning about HAVE_STDLIB_H being redefined in
   jconfig.h.  */
# ifdef HAVE_STDLIB_H
#  undef HAVE_STDLIB_H
# endif

# if defined (HAVE_NTGUI) && !defined (__WIN32__)
/* In older releases of the jpeg library, jpeglib.h will define boolean
   differently depending on __WIN32__, so make sure it is defined.  */
#  define __WIN32__ 1
# endif

/* rpcndr.h (via windows.h) and jpeglib.h both define boolean types.
   Some versions of jpeglib try to detect whether rpcndr.h is loaded,
   using the Windows boolean type instead of the jpeglib boolean type
   if so.  Cygwin jpeglib, however, doesn't try to detect whether its
   headers are included along with windows.h, so under Cygwin, jpeglib
   attempts to define a conflicting boolean type.  Worse, forcing
   Cygwin jpeglib headers to use the Windows boolean type doesn't work
   because it created an ABI incompatibility between the
   already-compiled jpeg library and the header interface definition.

   The best we can do is to define jpeglib's boolean type to a
   different name.  This name, jpeg_boolean, remains in effect through
   the rest of image.c.
*/
# if defined CYGWIN && defined HAVE_NTGUI
#  define boolean jpeg_boolean
# endif
# include <jpeglib.h>
# include <jerror.h>

# ifdef WINDOWSNT

/* JPEG library details.  */
DEF_DLL_FN (void, jpeg_CreateDecompress, (j_decompress_ptr, int, size_t));
DEF_DLL_FN (boolean, jpeg_start_decompress, (j_decompress_ptr));
DEF_DLL_FN (boolean, jpeg_finish_decompress, (j_decompress_ptr));
DEF_DLL_FN (void, jpeg_destroy_decompress, (j_decompress_ptr));
DEF_DLL_FN (int, jpeg_read_header, (j_decompress_ptr, boolean));
DEF_DLL_FN (JDIMENSION, jpeg_read_scanlines,
	    (j_decompress_ptr, JSAMPARRAY, JDIMENSION));
DEF_DLL_FN (struct jpeg_error_mgr *, jpeg_std_error,
	    (struct jpeg_error_mgr *));
DEF_DLL_FN (boolean, jpeg_resync_to_restart, (j_decompress_ptr, int));

static bool
init_jpeg_functions (void)
{
  HMODULE library;

  if (!(library = w32_delayed_load (Qjpeg)))
    return 0;

  LOAD_DLL_FN (library, jpeg_finish_decompress);
  LOAD_DLL_FN (library, jpeg_read_scanlines);
  LOAD_DLL_FN (library, jpeg_start_decompress);
  LOAD_DLL_FN (library, jpeg_read_header);
  LOAD_DLL_FN (library, jpeg_CreateDecompress);
  LOAD_DLL_FN (library, jpeg_destroy_decompress);
  LOAD_DLL_FN (library, jpeg_std_error);
  LOAD_DLL_FN (library, jpeg_resync_to_restart);
  return 1;
}

#  undef jpeg_CreateDecompress
#  undef jpeg_destroy_decompress
#  undef jpeg_finish_decompress
#  undef jpeg_read_header
#  undef jpeg_read_scanlines
#  undef jpeg_resync_to_restart
#  undef jpeg_start_decompress
#  undef jpeg_std_error

#  define jpeg_CreateDecompress fn_jpeg_CreateDecompress
#  define jpeg_destroy_decompress fn_jpeg_destroy_decompress
#  define jpeg_finish_decompress fn_jpeg_finish_decompress
#  define jpeg_read_header fn_jpeg_read_header
#  define jpeg_read_scanlines fn_jpeg_read_scanlines
#  define jpeg_resync_to_restart fn_jpeg_resync_to_restart
#  define jpeg_start_decompress fn_jpeg_start_decompress
#  define jpeg_std_error fn_jpeg_std_error

/* Wrapper since we can't directly assign the function pointer
   to another function pointer that was declared more completely easily.  */
static boolean
jpeg_resync_to_restart_wrapper (j_decompress_ptr cinfo, int desired)
{
  return jpeg_resync_to_restart (cinfo, desired);
}
#  undef jpeg_resync_to_restart
#  define jpeg_resync_to_restart jpeg_resync_to_restart_wrapper

# endif /* WINDOWSNT */

struct my_jpeg_error_mgr
{
  struct jpeg_error_mgr pub;
  sys_jmp_buf setjmp_buffer;

  /* The remaining members are so that longjmp doesn't munge local
     variables.  */
  struct jpeg_decompress_struct cinfo;
  enum
    {
      MY_JPEG_ERROR_EXIT,
      MY_JPEG_INVALID_IMAGE_SIZE,
      MY_JPEG_CANNOT_CREATE_X
    } failure_code;
};


static AVOID
my_error_exit (j_common_ptr cinfo)
{
  struct my_jpeg_error_mgr *mgr = (struct my_jpeg_error_mgr *) cinfo->err;
  mgr->failure_code = MY_JPEG_ERROR_EXIT;
  sys_longjmp (mgr->setjmp_buffer, 1);
}


/* Init source method for JPEG data source manager.  Called by
   jpeg_read_header before any data is actually read.  See
   libjpeg.doc from the JPEG lib distribution.  */

static void
our_common_init_source (j_decompress_ptr cinfo)
{
}


/* Method to terminate data source.  Called by
   jpeg_finish_decompress after all data has been processed.  */

static void
our_common_term_source (j_decompress_ptr cinfo)
{
}


/* Fill input buffer method for JPEG data source manager.  Called
   whenever more data is needed.  We read the whole image in one step,
   so this only adds a fake end of input marker at the end.  */

static JOCTET our_memory_buffer[2];

static boolean
our_memory_fill_input_buffer (j_decompress_ptr cinfo)
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
our_memory_skip_input_data (j_decompress_ptr cinfo, long int num_bytes)
{
  struct jpeg_source_mgr *src = cinfo->src;

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
jpeg_memory_src (j_decompress_ptr cinfo, JOCTET *data, ptrdiff_t len)
{
  struct jpeg_source_mgr *src = cinfo->src;

  if (! src)
    {
      /* First time for this JPEG object?  */
      src = cinfo->mem->alloc_small ((j_common_ptr) cinfo,
				     JPOOL_PERMANENT, sizeof *src);
      cinfo->src = src;
      src->next_input_byte = data;
    }

  src->init_source = our_common_init_source;
  src->fill_input_buffer = our_memory_fill_input_buffer;
  src->skip_input_data = our_memory_skip_input_data;
  src->resync_to_restart = jpeg_resync_to_restart; /* Use default method.  */
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
our_stdio_fill_input_buffer (j_decompress_ptr cinfo)
{
  struct jpeg_stdio_mgr *src;

  src = (struct jpeg_stdio_mgr *) cinfo->src;
  if (!src->finished)
    {
      ptrdiff_t bytes;

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
our_stdio_skip_input_data (j_decompress_ptr cinfo, long int num_bytes)
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
jpeg_file_src (j_decompress_ptr cinfo, FILE *fp)
{
  struct jpeg_stdio_mgr *src = (struct jpeg_stdio_mgr *) cinfo->src;

  if (! src)
    {
      /* First time for this JPEG object?  */
      src = cinfo->mem->alloc_small ((j_common_ptr) cinfo,
				     JPOOL_PERMANENT, sizeof *src);
      cinfo->src = (struct jpeg_source_mgr *) src;
      src->buffer = cinfo->mem->alloc_small ((j_common_ptr) cinfo,
					     JPOOL_PERMANENT,
					     JPEG_STDIO_BUFFER_SIZE);
    }

  src->file = fp;
  src->finished = 0;
  src->mgr.init_source = our_common_init_source;
  src->mgr.fill_input_buffer = our_stdio_fill_input_buffer;
  src->mgr.skip_input_data = our_stdio_skip_input_data;
  src->mgr.resync_to_restart = jpeg_resync_to_restart; /* Use default.  */
  src->mgr.term_source = our_common_term_source;
  src->mgr.bytes_in_buffer = 0;
  src->mgr.next_input_byte = NULL;
}

/* Load image IMG for use on frame F.  Patterned after example.c
   from the JPEG lib.  */

static bool
jpeg_load_body (struct frame *f, struct image *img,
		struct my_jpeg_error_mgr *mgr)
{
  Lisp_Object specified_file, specified_data;
  FILE *volatile fp = NULL;
  JSAMPARRAY buffer;
  int row_stride, x, y;
  int width, height;
  int i, ir, ig, ib;
  unsigned long *colors;
  Emacs_Pix_Container ximg = NULL;

  /* Open the JPEG file.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  if (NILP (specified_data))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (specified_file, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
	  return 0;
	}

      fp = fdopen (fd, "rb");
      if (fp == NULL)
	{
	  image_error ("Cannot open `%s'", file);
	  return 0;
	}
    }
  else if (!STRINGP (specified_data))
    {
      image_error ("Invalid image data `%s'", specified_data);
      return 0;
    }

  /* Customize libjpeg's error handling to call my_error_exit when an
     error is detected.  This function will perform a longjmp.  */
  mgr->cinfo.err = jpeg_std_error (&mgr->pub);
  mgr->pub.error_exit = my_error_exit;
  if (sys_setjmp (mgr->setjmp_buffer))
    {
      switch (mgr->failure_code)
	{
	case MY_JPEG_ERROR_EXIT:
	  {
	    char buf[JMSG_LENGTH_MAX];
	    mgr->cinfo.err->format_message ((j_common_ptr) &mgr->cinfo, buf);
	    image_error ("Error reading JPEG image `%s': %s",
			 img->spec, build_string (buf));
	    break;
	  }

	case MY_JPEG_INVALID_IMAGE_SIZE:
	  image_size_error ();
	  break;

	case MY_JPEG_CANNOT_CREATE_X:
	  break;
	}

      /* Close the input file and destroy the JPEG object.  */
      if (fp)
	fclose (fp);
      jpeg_destroy_decompress (&mgr->cinfo);

      /* If we already have an XImage, free that.  */
      image_destroy_x_image (ximg);
      /* Free pixmap and colors.  */
      image_clear_image (f, img);
      return 0;
    }

  /* Create the JPEG decompression object.  Let it read from fp.
	 Read the JPEG image header.  */
  jpeg_CreateDecompress (&mgr->cinfo, JPEG_LIB_VERSION, sizeof *&mgr->cinfo);

  if (NILP (specified_data))
    jpeg_file_src (&mgr->cinfo, fp);
  else
    jpeg_memory_src (&mgr->cinfo, SDATA (specified_data),
		     SBYTES (specified_data));

  jpeg_read_header (&mgr->cinfo, 1);

  /* Customize decompression so that color quantization will be used.
	 Start decompression.  */
  mgr->cinfo.quantize_colors = 1;
  jpeg_start_decompress (&mgr->cinfo);
  width = img->width = mgr->cinfo.output_width;
  height = img->height = mgr->cinfo.output_height;

  if (!check_image_size (f, width, height))
    {
      mgr->failure_code = MY_JPEG_INVALID_IMAGE_SIZE;
      sys_longjmp (mgr->setjmp_buffer, 1);
    }

  /* Create X image and pixmap.  */
  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
    {
      mgr->failure_code = MY_JPEG_CANNOT_CREATE_X;
      sys_longjmp (mgr->setjmp_buffer, 1);
    }

  /* Allocate colors.  When color quantization is used,
     mgr->cinfo.actual_number_of_colors has been set with the number of
     colors generated, and mgr->cinfo.colormap is a two-dimensional array
     of color indices in the range 0..mgr->cinfo.actual_number_of_colors.
     No more than 255 colors will be generated.  */
  USE_SAFE_ALLOCA;
  {
    if (mgr->cinfo.out_color_components > 2)
      ir = 0, ig = 1, ib = 2;
    else if (mgr->cinfo.out_color_components > 1)
      ir = 0, ig = 1, ib = 0;
    else
      ir = 0, ig = 0, ib = 0;

    /* Use the color table mechanism because it handles colors that
       cannot be allocated nicely.  Such colors will be replaced with
       a default color, and we don't have to care about which colors
       can be freed safely, and which can't.  */
    init_color_table ();
    SAFE_NALLOCA (colors, 1, mgr->cinfo.actual_number_of_colors);

    for (i = 0; i < mgr->cinfo.actual_number_of_colors; ++i)
      {
	/* Multiply RGB values with 255 because X expects RGB values
	   in the range 0..0xffff.  */
	int r = mgr->cinfo.colormap[ir][i] << 8;
	int g = mgr->cinfo.colormap[ig][i] << 8;
	int b = mgr->cinfo.colormap[ib][i] << 8;
	colors[i] = lookup_rgb_color (f, r, g, b);
      }

#ifdef COLOR_TABLE_SUPPORT
    /* Remember those colors actually allocated.  */
    img->colors = colors_in_color_table (&img->ncolors);
    free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */
  }

  /* Read pixels.  */
  row_stride = width * mgr->cinfo.output_components;
  buffer = mgr->cinfo.mem->alloc_sarray ((j_common_ptr) &mgr->cinfo,
					 JPOOL_IMAGE, row_stride, 1);
  for (y = 0; y < height; ++y)
    {
      jpeg_read_scanlines (&mgr->cinfo, buffer, 1);
      for (x = 0; x < mgr->cinfo.output_width; ++x)
	PUT_PIXEL (ximg, x, y, colors[buffer[0][x]]);
    }

  /* Clean up.  */
  jpeg_finish_decompress (&mgr->cinfo);
  jpeg_destroy_decompress (&mgr->cinfo);
  if (fp)
    fclose (fp);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);
  SAFE_FREE ();
  return 1;
}

static bool
jpeg_load (struct frame *f, struct image *img)
{
  struct my_jpeg_error_mgr mgr;
  return jpeg_load_body (f, img, &mgr);
}

#endif /* !HAVE_JPEG */



/***********************************************************************
				 TIFF
 ***********************************************************************/

#if defined (HAVE_TIFF)

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
  TIFF_INDEX,
  TIFF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword tiff_format[TIFF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0}
};

/* Return true if OBJECT is a valid TIFF image specification.  */

static bool
tiff_image_p (Lisp_Object object)
{
  struct image_keyword fmt[TIFF_LAST];
  memcpy (fmt, tiff_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[TIFF_FILE].count + fmt[TIFF_DATA].count == 1;
}

#endif /* HAVE_TIFF */

#ifdef HAVE_TIFF

# include <tiffio.h>

/* libtiff version 4.3.0 deprecated uint32 typedef.  */
#if TIFFLIB_VERSION >= 20210416
# define UINT32 uint32_t
#else
# define UINT32 uint32
#endif

# ifdef WINDOWSNT

/* TIFF library details.  */
DEF_DLL_FN (TIFFErrorHandler, TIFFSetErrorHandler, (TIFFErrorHandler));
DEF_DLL_FN (TIFFErrorHandler, TIFFSetWarningHandler, (TIFFErrorHandler));
DEF_DLL_FN (TIFF *, TIFFOpen, (const char *, const char *));
DEF_DLL_FN (TIFF *, TIFFClientOpen,
	    (const char *, const char *, thandle_t, TIFFReadWriteProc,
	     TIFFReadWriteProc, TIFFSeekProc, TIFFCloseProc, TIFFSizeProc,
	     TIFFMapFileProc, TIFFUnmapFileProc));
DEF_DLL_FN (int, TIFFGetField, (TIFF *, ttag_t, ...));
DEF_DLL_FN (int, TIFFReadRGBAImage, (TIFF *, UINT32, UINT32, UINT32 *, int));
DEF_DLL_FN (void, TIFFClose, (TIFF *));
DEF_DLL_FN (int, TIFFSetDirectory, (TIFF *, tdir_t));

static bool
init_tiff_functions (void)
{
  HMODULE library;

  if (!(library = w32_delayed_load (Qtiff)))
    return 0;

  LOAD_DLL_FN (library, TIFFSetErrorHandler);
  LOAD_DLL_FN (library, TIFFSetWarningHandler);
  LOAD_DLL_FN (library, TIFFOpen);
  LOAD_DLL_FN (library, TIFFClientOpen);
  LOAD_DLL_FN (library, TIFFGetField);
  LOAD_DLL_FN (library, TIFFReadRGBAImage);
  LOAD_DLL_FN (library, TIFFClose);
  LOAD_DLL_FN (library, TIFFSetDirectory);
  return 1;
}

#  undef TIFFClientOpen
#  undef TIFFClose
#  undef TIFFGetField
#  undef TIFFOpen
#  undef TIFFReadRGBAImage
#  undef TIFFSetDirectory
#  undef TIFFSetErrorHandler
#  undef TIFFSetWarningHandler

#  define TIFFClientOpen fn_TIFFClientOpen
#  define TIFFClose fn_TIFFClose
#  define TIFFGetField fn_TIFFGetField
#  define TIFFOpen fn_TIFFOpen
#  define TIFFReadRGBAImage fn_TIFFReadRGBAImage
#  define TIFFSetDirectory fn_TIFFSetDirectory
#  define TIFFSetErrorHandler fn_TIFFSetErrorHandler
#  define TIFFSetWarningHandler fn_TIFFSetWarningHandler

# endif /* WINDOWSNT */


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
  ptrdiff_t len;
  ptrdiff_t index;
}
tiff_memory_source;

static tsize_t
tiff_read_from_memory (thandle_t data, tdata_t buf, tsize_t size)
{
  tiff_memory_source *src = (tiff_memory_source *) data;

  size = min (size, src->len - src->index);
  memcpy (buf, src->bytes + src->index, size);
  src->index += size;
  return size;
}

static tsize_t
tiff_write_from_memory (thandle_t data, tdata_t buf, tsize_t size)
{
  return -1;
}

static toff_t
tiff_seek_in_memory (thandle_t data, toff_t off, int whence)
{
  tiff_memory_source *src = (tiff_memory_source *) data;
  ptrdiff_t idx;

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
tiff_close_memory (thandle_t data)
{
  /* NOOP */
  return 0;
}

static int
tiff_mmap_memory (thandle_t data, tdata_t *pbase, toff_t *psize)
{
  /* It is already _IN_ memory. */
  return 0;
}

static void
tiff_unmap_memory (thandle_t data, tdata_t base, toff_t size)
{
  /* We don't need to do this. */
}

static toff_t
tiff_size_of_memory (thandle_t data)
{
  return ((tiff_memory_source *) data)->len;
}

/* GCC 3.x on x86 Windows targets has a bug that triggers an internal
   compiler error compiling tiff_handler, see Bugzilla bug #17406
   (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=17406).  Declaring
   this function as external works around that problem.  */
# if defined (__MINGW32__) && __GNUC__ == 3
#  define MINGW_STATIC
# else
#  define MINGW_STATIC static
# endif

MINGW_STATIC void
tiff_handler (const char *, const char *, const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (3, 0);
MINGW_STATIC void
tiff_handler (const char *log_format, const char *title,
	      const char *format, va_list ap)
{
  /* doprnt is not suitable here, as TIFF handlers are called from
     libtiff and are passed arbitrary printf directives.  Instead, use
     vsnprintf, taking care to be portable to nonstandard environments
     where vsnprintf returns -1 on buffer overflow.  Since it's just a
     log entry, it's OK to truncate it.  */
  char buf[4000];
  int len = vsnprintf (buf, sizeof buf, format, ap);
  add_to_log (log_format, build_string (title),
	      make_string (buf, max (0, min (len, sizeof buf - 1))));
}
# undef MINGW_STATIC

static void tiff_error_handler (const char *, const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (2, 0);
static void
tiff_error_handler (const char *title, const char *format, va_list ap)
{
  tiff_handler ("TIFF error: %s %s", title, format, ap);
}


static void tiff_warning_handler (const char *, const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (2, 0);
static void
tiff_warning_handler (const char *title, const char *format, va_list ap)
{
  tiff_handler ("TIFF warning: %s %s", title, format, ap);
}


/* Load TIFF image IMG for use on frame F.  Value is true if
   successful.  */

static bool
tiff_load (struct frame *f, struct image *img)
{
  Lisp_Object specified_file;
  Lisp_Object specified_data;
  TIFF *tiff;
  int width, height, x, y, count;
  UINT32 *buf;
  int rc;
  Emacs_Pix_Container ximg;
  tiff_memory_source memsrc;
  Lisp_Object image;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);

  TIFFSetErrorHandler ((TIFFErrorHandler) tiff_error_handler);
  TIFFSetWarningHandler ((TIFFErrorHandler) tiff_warning_handler);

  if (NILP (specified_data))
    {
      /* Read from a file */
      Lisp_Object file = image_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
	  return 0;
	}

      Lisp_Object encoded_file = ENCODE_FILE (file);
# ifdef WINDOWSNT
      encoded_file = ansi_encode_filename (encoded_file);
# endif

      /* Try to open the image file.  */
      tiff = TIFFOpen (SSDATA (encoded_file), "r");
      if (tiff == NULL)
	{
	  image_error ("Cannot open `%s'", file);
	  return 0;
	}
    }
  else
    {
      if (!STRINGP (specified_data))
	{
	  image_error ("Invalid image data `%s'", specified_data);
	  return 0;
	}

      /* Memory source! */
      memsrc.bytes = SDATA (specified_data);
      memsrc.len = SBYTES (specified_data);
      memsrc.index = 0;

      tiff = TIFFClientOpen ("memory_source", "r", (thandle_t)&memsrc,
			     tiff_read_from_memory,
			     tiff_write_from_memory,
			     tiff_seek_in_memory,
			     tiff_close_memory,
			     tiff_size_of_memory,
			     tiff_mmap_memory,
			     tiff_unmap_memory);

      if (!tiff)
	{
	  image_error ("Cannot open memory source for `%s'", img->spec);
	  return 0;
	}
    }

  image = image_spec_value (img->spec, QCindex, NULL);
  if (FIXNUMP (image))
    {
      EMACS_INT ino = XFIXNAT (image);
      if (! (TYPE_MINIMUM (tdir_t) <= ino && ino <= TYPE_MAXIMUM (tdir_t)
	     && TIFFSetDirectory (tiff, ino)))
	{
	  image_error ("Invalid image number `%s' in image `%s'",
		       image, img->spec);
	  TIFFClose (tiff);
	  return 0;
	}
    }

  /* Get width and height of the image, and allocate a raster buffer
     of width x height 32-bit values.  */
  TIFFGetField (tiff, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField (tiff, TIFFTAG_IMAGELENGTH, &height);

  if (!check_image_size (f, width, height))
    {
      image_size_error ();
      TIFFClose (tiff);
      return 0;
    }

  /* Create the X image and pixmap.  */
  if (! (height <= min (PTRDIFF_MAX, SIZE_MAX) / sizeof *buf / width
	 && image_create_x_image_and_pixmap (f, img, width, height, 0,
					     &ximg, 0)))
    {
      TIFFClose (tiff);
      return 0;
    }

  buf = xmalloc (sizeof *buf * width * height);

  rc = TIFFReadRGBAImage (tiff, width, height, buf, 0);

  /* Count the number of images in the file.  */
  for (count = 1; TIFFSetDirectory (tiff, count); count++)
    continue;

  if (count > 1)
    img->lisp_data = Fcons (Qcount,
			    Fcons (make_fixnum (count),
				   img->lisp_data));

  TIFFClose (tiff);
  if (!rc)
    {
      image_error ("Error reading TIFF image `%s'", img->spec);
      xfree (buf);
      return 0;
    }

  /* Initialize the color table.  */
  init_color_table ();

  /* Process the pixel raster.  Origin is in the lower-left corner.  */
  for (y = 0; y < height; ++y)
    {
      UINT32 *row = buf + y * width;

      for (x = 0; x < width; ++x)
	{
	  UINT32 abgr = row[x];
	  int r = TIFFGetR (abgr) << 8;
	  int g = TIFFGetG (abgr) << 8;
	  int b = TIFFGetB (abgr) << 8;
	  PUT_PIXEL (ximg, x, height - 1 - y, lookup_rgb_color (f, r, g, b));
	}
    }

# ifdef COLOR_TABLE_SUPPORT
  /* Remember the colors allocated for the image.  Free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
# endif /* COLOR_TABLE_SUPPORT */

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning on W32.  */
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  xfree (buf);
  return 1;
}

#endif



/***********************************************************************
				 GIF
 ***********************************************************************/

#if defined (HAVE_GIF)

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

static const struct image_keyword gif_format[GIF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Free X resources of GIF image IMG which is used on frame F.  */

static void
gif_clear_image (struct frame *f, struct image *img)
{
  img->lisp_data = Qnil;
  image_clear_image (f, img);
}

/* Return true if OBJECT is a valid GIF image specification.  */

static bool
gif_image_p (Lisp_Object object)
{
  struct image_keyword fmt[GIF_LAST];
  memcpy (fmt, gif_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[GIF_FILE].count + fmt[GIF_DATA].count == 1;
}

#endif /* HAVE_GIF */

#ifdef HAVE_GIF

# ifdef HAVE_NTGUI

/* winuser.h might define DrawText to DrawTextA or DrawTextW.
   Undefine before redefining to avoid a preprocessor warning.  */
#  ifdef DrawText
#   undef DrawText
#  endif
/* avoid conflict with QuickdrawText.h */
#  define DrawText gif_DrawText
#  include <gif_lib.h>
/* The bogus ifdef below, which is always true, is to avoid a compiler
   warning about DrawText being unused.  */
#  ifdef DrawText
#   undef DrawText
#  endif

# else /* HAVE_NTGUI */

#  include <gif_lib.h>

# endif /* HAVE_NTGUI */

/* Giflib before 4.1.6 didn't define these macros.  */
# ifndef GIFLIB_MAJOR
#  define GIFLIB_MAJOR 4
# endif
# ifndef GIFLIB_MINOR
#  define GIFLIB_MINOR 0
# endif
# ifndef GIFLIB_RELEASE
#  define GIFLIB_RELEASE 0
# endif
/* Giflib before 5.0 didn't define these macros.  */
# if GIFLIB_MAJOR < 5
#  define DISPOSAL_UNSPECIFIED    0    /* No disposal specified.  */
#  define DISPOSE_DO_NOT          1    /* Leave image in place.  */
#  define DISPOSE_BACKGROUND      2    /* Set area too background color.  */
#  define DISPOSE_PREVIOUS        3    /* Restore to previous content.  */
#  define NO_TRANSPARENT_COLOR    -1
# endif

/* GifErrorString is declared to return char const * when GIFLIB_MAJOR
   and GIFLIB_MINOR indicate 5.1 or later.  Do not bother using it in
   earlier releases, where either it returns char * or GIFLIB_MINOR
   may be incorrect.  */
# define HAVE_GIFERRORSTRING (5 < GIFLIB_MAJOR + (1 <= GIFLIB_MINOR))

# ifdef WINDOWSNT

/* GIF library details.  */
#  if GIFLIB_MAJOR + (GIFLIB_MINOR >= 1) > 5
DEF_DLL_FN (int, DGifCloseFile, (GifFileType *, int *));
#   else
DEF_DLL_FN (int, DGifCloseFile, (GifFileType *));
#  endif
DEF_DLL_FN (int, DGifSlurp, (GifFileType *));
#  if GIFLIB_MAJOR < 5
DEF_DLL_FN (GifFileType *, DGifOpen, (void *, InputFunc));
DEF_DLL_FN (GifFileType *, DGifOpenFileName, (const char *));
#  else
DEF_DLL_FN (GifFileType *, DGifOpen, (void *, InputFunc, int *));
DEF_DLL_FN (GifFileType *, DGifOpenFileName, (const char *, int *));
DEF_DLL_FN (int, DGifSavedExtensionToGCB,
	    (GifFileType *, int, GraphicsControlBlock *));
#  endif
#  if HAVE_GIFERRORSTRING
DEF_DLL_FN (char const *, GifErrorString, (int));
#  endif

static bool
init_gif_functions (void)
{
  HMODULE library;

  if (!(library = w32_delayed_load (Qgif)))
    return 0;

  LOAD_DLL_FN (library, DGifCloseFile);
  LOAD_DLL_FN (library, DGifSlurp);
  LOAD_DLL_FN (library, DGifOpen);
  LOAD_DLL_FN (library, DGifOpenFileName);
#  if GIFLIB_MAJOR >= 5
  LOAD_DLL_FN (library, DGifSavedExtensionToGCB);
#  endif
#  if HAVE_GIFERRORSTRING
  LOAD_DLL_FN (library, GifErrorString);
#  endif
  return 1;
}

#  undef DGifCloseFile
#  undef DGifOpen
#  undef DGifOpenFileName
#  undef DGifSlurp
#  if GIFLIB_MAJOR >= 5
#   undef DGifSavedExtensionToGCB
#  endif
#  undef GifErrorString

#  define DGifCloseFile fn_DGifCloseFile
#  define DGifOpen fn_DGifOpen
#  define DGifOpenFileName fn_DGifOpenFileName
#  define DGifSlurp fn_DGifSlurp
#  if GIFLIB_MAJOR >= 5
#   define DGifSavedExtensionToGCB fn_DGifSavedExtensionToGCB
#  endif
#  define GifErrorString fn_GifErrorString

# endif /* WINDOWSNT */

/* Reading a GIF image from memory
   Based on the PNG memory stuff to a certain extent. */

typedef struct
{
  unsigned char *bytes;
  ptrdiff_t len;
  ptrdiff_t index;
}
gif_memory_source;

/* Make the current memory source available to gif_read_from_memory.
   It's done this way because not all versions of libungif support
   a UserData field in the GifFileType structure.  */
static gif_memory_source *current_gif_memory_src;

static int
gif_read_from_memory (GifFileType *file, GifByteType *buf, int len)
{
  gif_memory_source *src = current_gif_memory_src;

  if (len > src->len - src->index)
    return -1;

  memcpy (buf, src->bytes + src->index, len);
  src->index += len;
  return len;
}

static int
gif_close (GifFileType *gif, int *err)
{
  int retval;

#if GIFLIB_MAJOR + (GIFLIB_MINOR >= 1) > 5
  retval = DGifCloseFile (gif, err);
#else
  retval = DGifCloseFile (gif);
#if GIFLIB_MAJOR >= 5
  if (err)
    *err = gif->Error;
#endif
#endif
  return retval;
}

/* Load GIF image IMG for use on frame F.  Value is true if
   successful.  */

static const int interlace_start[] = {0, 4, 2, 1};
static const int interlace_increment[] = {8, 8, 4, 2};

#define GIF_LOCAL_DESCRIPTOR_EXTENSION 249

static void
gif_destroy (struct anim_cache* cache)
{
  int gif_err;
  gif_close (cache->handle, &gif_err);
}

static bool
gif_load (struct frame *f, struct image *img)
{
  int rc, width, height, x, y, i, j;
  ColorMapObject *gif_color_map;
  GifFileType *gif = NULL;
  gif_memory_source memsrc;
  Lisp_Object specified_bg = image_spec_value (img->spec, QCbackground, NULL);
  Lisp_Object specified_file = image_spec_value (img->spec, QCfile, NULL);
  Lisp_Object specified_data = image_spec_value (img->spec, QCdata, NULL);
  unsigned long *pixmap = NULL;
  EMACS_INT idx = -1;
  int gif_err;
  struct anim_cache* cache = NULL;
  /* Which sub-image are we to display?  */
  Lisp_Object image_number = image_spec_value (img->spec, QCindex, NULL);
  int byte_size = 0;

  idx = FIXNUMP (image_number) ? XFIXNAT (image_number) : 0;

  if (!NILP (image_number))
    {
      /* If this is an animated image, create a cache for it.  */
      cache = anim_get_animation_cache (XCDR (img->spec));
      /* We have an old cache entry, so use it.  */
      if (cache->handle)
	{
	  gif = cache->handle;
	  pixmap = cache->temp;
	  /* We're out of sync, so start from the beginning.  */
	  if (cache->index != idx - 1)
	    cache->index = -1;
	}
    }

  /* If we don't have a cached entry, read the image.  */
  if (! gif)
    {
      if (NILP (specified_data))
	{
	  Lisp_Object file = image_find_image_file (specified_file);
	  if (!STRINGP (file))
	    {
	      image_error ("Cannot find image file `%s'", specified_file);
	      return false;
	    }

	  Lisp_Object encoded_file = ENCODE_FILE (file);
#ifdef WINDOWSNT
	  encoded_file = ansi_encode_filename (encoded_file);
#endif

	  /* Open the GIF file.  */
#if GIFLIB_MAJOR < 5
	  gif = DGifOpenFileName (SSDATA (encoded_file));
#else
	  gif = DGifOpenFileName (SSDATA (encoded_file), &gif_err);
#endif
	  if (gif == NULL)
	    {
#if HAVE_GIFERRORSTRING
	      const char *errstr = GifErrorString (gif_err);
	      if (errstr)
		image_error ("Cannot open `%s': %s", file,
			     build_string (errstr));
	      else
#endif
		image_error ("Cannot open `%s'", file);
	      return false;
	    }

	  /* Get the file size so that we can report it in
	     `image-cache-size'.  */
	  struct stat st;
	  FILE *fp = fopen (SSDATA (encoded_file), "rb");
	  if (fstat (fileno (fp), &st) == 0)
	    byte_size = st.st_size;
	  fclose (fp);
	}
      else
	{
	  if (!STRINGP (specified_data))
	    {
	      image_error ("Invalid image data `%s'", specified_data);
	      return false;
	    }

	  /* Read from memory! */
	  current_gif_memory_src = &memsrc;
	  memsrc.bytes = SDATA (specified_data);
	  memsrc.len = SBYTES (specified_data);
	  memsrc.index = 0;
	  byte_size = memsrc.len;

#if GIFLIB_MAJOR < 5
	  gif = DGifOpen (&memsrc, gif_read_from_memory);
#else
	  gif = DGifOpen (&memsrc, gif_read_from_memory, &gif_err);
#endif
	  if (!gif)
	    {
#if HAVE_GIFERRORSTRING
	      const char *errstr = GifErrorString (gif_err);
	      if (errstr)
		image_error ("Cannot open memory source `%s': %s",
			     img->spec, build_string (errstr));
	      else
#endif
		image_error ("Cannot open memory source `%s'", img->spec);
	      return false;
	    }
	}

      /* Before reading entire contents, check the declared image size. */
      if (!check_image_size (f, gif->SWidth, gif->SHeight))
	{
	  image_size_error ();
	  goto gif_error;
	}

      /* Read entire contents.  */
      rc = DGifSlurp (gif);
      if (rc == GIF_ERROR || gif->ImageCount <= 0)
	{
#if HAVE_GIFERRORSTRING
	  const char *errstr = GifErrorString (gif->Error);
	  if (errstr)
	    if (NILP (specified_data))
	      image_error ("Error reading `%s' (%s)", img->spec,
			   build_string (errstr));
	    else
	      image_error ("Error reading GIF data: %s",
			   build_string (errstr));
	  else
#endif
	    if (NILP (specified_data))
	      image_error ("Error reading `%s'", img->spec);
	    else
	      image_error ("Error reading GIF data");
	  goto gif_error;
	}

      width = img->width = gif->SWidth;
      height = img->height = gif->SHeight;

      /* Check that the selected subimages fit.  It's not clear whether
	 the GIF spec requires this, but Emacs can crash if they don't fit.  */
      for (j = 0; j < gif->ImageCount; ++j)
	{
	  struct SavedImage *subimage = gif->SavedImages + j;
	  int subimg_width = subimage->ImageDesc.Width;
	  int subimg_height = subimage->ImageDesc.Height;
	  int subimg_top = subimage->ImageDesc.Top;
	  int subimg_left = subimage->ImageDesc.Left;
	  if (subimg_width < 0
	      || subimg_height < 0
	      || subimg_top < 0
	      || subimg_left < 0
	      || subimg_top + subimg_height > height
	      || subimg_left + subimg_width > width)
	    {
	      image_error ("Subimage does not fit in image");
	      goto gif_error;
	    }
	}
    }
  else
    {
      /* Cached image; set data.  */
      width = img->width = gif->SWidth;
      height = img->height = gif->SHeight;
    }

  if (idx < 0 || idx >= gif->ImageCount)
    {
      image_error ("Invalid image number `%s' in image `%s'",
		   make_fixnum (idx), img->spec);
      goto gif_error;
    }

  /* It's an animated image, so initialize the cache.  */
  if (cache && !cache->handle)
    {
      cache->handle = gif;
      cache->destructor = (void (*)(void *)) &gif_destroy;
      cache->width = width;
      cache->height = height;
      cache->byte_size = byte_size;
    }

  img->corners[TOP_CORNER] = gif->SavedImages[0].ImageDesc.Top;
  img->corners[LEFT_CORNER] = gif->SavedImages[0].ImageDesc.Left;
  img->corners[BOT_CORNER]
    = img->corners[TOP_CORNER] + gif->SavedImages[0].ImageDesc.Height;
  img->corners[RIGHT_CORNER]
    = img->corners[LEFT_CORNER] + gif->SavedImages[0].ImageDesc.Width;

  if (!check_image_size (f, width, height))
    {
      image_size_error ();
      goto gif_error;
    }

  /* Create the X image and pixmap.  */
  Emacs_Pix_Container ximg;
  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
    goto gif_error;

  /* We construct the (possibly composited animated) image in this
     buffer.  */
  if (!pixmap)
    {
      pixmap = xmalloc (width * height * sizeof (unsigned long));
      if (cache)
	cache->temp = pixmap;
    }

  /* Clear the part of the screen image not covered by the image.
     Full animated GIF support requires more here (see the gif89 spec,
     disposal methods).  Let's simply assume that the part not covered
     by a sub-image is in the frame's background color.  */
  unsigned long frame_bg;
#ifndef USE_CAIRO
  frame_bg = FRAME_BACKGROUND_PIXEL (f);
#else  /* USE_CAIRO */
  {
    Emacs_Color color;
    FRAME_TERMINAL (f)->query_frame_background_color (f, &color);
    frame_bg = lookup_rgb_color (f, color.red, color.green, color.blue);
  }
#endif	/* USE_CAIRO */

  for (y = 0; y < img->corners[TOP_CORNER]; ++y)
    for (x = 0; x < width; ++x)
      *(pixmap + x + y * width) = frame_bg;

  for (y = img->corners[BOT_CORNER]; y < height; ++y)
    for (x = 0; x < width; ++x)
      *(pixmap + x + y * width) = frame_bg;

  for (y = img->corners[TOP_CORNER]; y < img->corners[BOT_CORNER]; ++y)
    {
      for (x = 0; x < img->corners[LEFT_CORNER]; ++x)
	*(pixmap + x + y * width) = frame_bg;
      for (x = img->corners[RIGHT_CORNER]; x < width; ++x)
	*(pixmap + x + y * width) = frame_bg;
    }

  /* Read the GIF image into the X image.   */

  init_color_table ();

  unsigned long bgcolor UNINIT;
  if (STRINGP (specified_bg))
    {
      bgcolor = image_alloc_image_color (f, img, specified_bg,
					 FRAME_BACKGROUND_PIXEL (f));
#ifdef USE_CAIRO
      Emacs_Color color = {.pixel = bgcolor};
      FRAME_TERMINAL (f)->query_colors (f, &color, 1);
      bgcolor = lookup_rgb_color (f, color.red, color.green, color.blue);
#endif
    }

  int start_frame = 0;

  /* We have animation data in the cache.  */
  if (cache && cache->temp)
    {
      start_frame = cache->index + 1;
      if (start_frame > idx)
	start_frame = 0;
      cache->index = idx;
    }

  for (j = start_frame; j <= idx; ++j)
    {
      /* We use a local variable `raster' here because RasterBits is a
	 char *, which invites problems with bytes >= 0x80.  */
      struct SavedImage *subimage = gif->SavedImages + j;
      unsigned char *raster = (unsigned char *) subimage->RasterBits;
      int subimg_width = subimage->ImageDesc.Width;
      int subimg_height = subimage->ImageDesc.Height;
      int subimg_top = subimage->ImageDesc.Top;
      int subimg_left = subimage->ImageDesc.Left;

      /* From gif89a spec: 1 = "keep in place", 2 = "restore
	 to background".  Treat any other value like 2.  */
      int disposal = DISPOSAL_UNSPECIFIED;
      int transparency_color_index = NO_TRANSPARENT_COLOR;

#if GIFLIB_MAJOR < 5
      /* Find the Graphic Control Extension block for this sub-image.
	 Extract the disposal method and transparency color.  */
      for (i = 0; i < subimage->ExtensionBlockCount; i++)
	{
	  ExtensionBlock *extblock = subimage->ExtensionBlocks + i;

	  if ((extblock->Function == GIF_LOCAL_DESCRIPTOR_EXTENSION)
	      && extblock->ByteCount == 4
	      && extblock->Bytes[0] & 1)
	    {
	      disposal = (extblock->Bytes[0] >> 2) & 7;
	      transparency_color_index = (unsigned char) extblock->Bytes[3];
	      break;
	    }
	}
#else
      GraphicsControlBlock gcb;
      DGifSavedExtensionToGCB (gif, j, &gcb);
      disposal = gcb.DisposalMode;
      transparency_color_index = gcb.TransparentColor;
#endif

      /* We can't "keep in place" the first subimage.  */
      if (j == 0)
	disposal = DISPOSE_BACKGROUND;

      /* For disposal == 0 (DISPOSAL_UNSPECIFIED), the spec says
	 "No disposal specified.  The decoder is not required to take
	 any action."  In practice, it seems we need to treat this
	 like "keep in place" (DISPOSE_DO_NOT), see e.g.
	 https://upload.wikimedia.org/wikipedia/commons/3/37/Clock.gif */
      if (disposal == DISPOSAL_UNSPECIFIED)
	disposal = DISPOSE_DO_NOT;

      /* This is not quite correct -- the specification is unclear,
	 but I think we're supposed to restore to the frame before the
	 previous frame?  And we don't have that data at this point.
	 But DISPOSE_DO_NOT is less wrong than substituting the
	 background, so do that for now.  */
      if (disposal == DISPOSE_PREVIOUS)
	disposal = DISPOSE_DO_NOT;

      gif_color_map = subimage->ImageDesc.ColorMap;
      if (!gif_color_map)
	gif_color_map = gif->SColorMap;

      /* Allocate subimage colors.  */
      unsigned long pixel_colors[256] = { 0, };

      if (gif_color_map)
	for (i = 0; i < gif_color_map->ColorCount; ++i)
	  {
	    if (transparency_color_index == i)
	      pixel_colors[i] = STRINGP (specified_bg)
		? bgcolor : frame_bg;
	    else
	      {
		int r = gif_color_map->Colors[i].Red << 8;
		int g = gif_color_map->Colors[i].Green << 8;
		int b = gif_color_map->Colors[i].Blue << 8;
		pixel_colors[i] = lookup_rgb_color (f, r, g, b);
	      }
	  }

      /* Apply the pixel values.  */
      if (GIFLIB_MAJOR < 5 && gif->SavedImages[j].ImageDesc.Interlace)
	{
	  int row, pass;

	  for (y = 0, row = interlace_start[0], pass = 0;
	       y < subimg_height;
	       y++, row += interlace_increment[pass])
	    {
	      while (subimg_height <= row)
		row = interlace_start[++pass];

	      for (x = 0; x < subimg_width; x++)
		{
		  int c = raster[y * subimg_width + x];
		  if (transparency_color_index != c || disposal != DISPOSE_DO_NOT)
                    {
		      *(pixmap + x + subimg_left + (y + subimg_top) * width) =
			pixel_colors[c];
		    }
		}
	    }
	}
      else
	{
          for (y = 0; y < subimg_height; ++y)
	    for (x = 0; x < subimg_width; ++x)
	      {
		int c = raster[y * subimg_width + x];
		if (transparency_color_index != c || disposal != DISPOSE_DO_NOT)
                  {
		    *(pixmap + x + subimg_left + (y + subimg_top) * width) =
		      pixel_colors[c];
                  }
	      }
	}
    }

  /* We now have the complete image (possibly composed from a series
     of animated frames) in pixmap.  Put it into ximg.  */
  for (y = 0; y < height; ++y)
    for (x = 0; x < width; ++x)
      PUT_PIXEL (ximg, x, y, *(pixmap + x + y * width));

#ifdef COLOR_TABLE_SUPPORT
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  /* Save GIF image extension data for `image-metadata'.
     Format is (count IMAGES extension-data (FUNCTION "BYTES" ...)).  */
  img->lisp_data = Qnil;
  if (gif->SavedImages[idx].ExtensionBlockCount > 0)
    {
      int delay = 0;
      ExtensionBlock *ext = gif->SavedImages[idx].ExtensionBlocks;
      for (i = 0; i < gif->SavedImages[idx].ExtensionBlockCount; i++, ext++)
	/* Append (... FUNCTION "BYTES") */
	{
	  img->lisp_data
	    = Fcons (make_fixnum (ext->Function),
		     Fcons (make_unibyte_string ((char *) ext->Bytes,
						 ext->ByteCount),
			    img->lisp_data));
	  if (ext->Function == GIF_LOCAL_DESCRIPTOR_EXTENSION
	      && ext->ByteCount == 4)
	    {
	      delay = ext->Bytes[2] << CHAR_BIT;
	      delay |= ext->Bytes[1];
	    }
	}
      img->lisp_data = list2 (Qextension_data, img->lisp_data);
      img->lisp_data
	= Fcons (Qdelay,
		 /* Default GIF delay is 1/15th of a second.  */
		 Fcons (make_float (delay? delay / 100.0: 1.0 / 15),
			img->lisp_data));
    }

  if (gif->ImageCount > 1)
    img->lisp_data = Fcons (Qcount,
			    Fcons (make_fixnum (gif->ImageCount),
				   img->lisp_data));

  if (!cache)
    {
      if (pixmap)
	xfree (pixmap);
      if (gif_close (gif, &gif_err) == GIF_ERROR)
	{
#if HAVE_GIFERRORSTRING
	  char const *error_text = GifErrorString (gif_err);

	  if (error_text)
	    image_error ("Error closing `%s': %s",
			 img->spec, build_string (error_text));
	  else
#endif
	    image_error ("Error closing `%s'", img->spec);
	}
    }

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  return true;

 gif_error:
  if (pixmap)
    xfree (pixmap);
  gif_close (gif, NULL);
  if (cache)
    {
      cache->handle = NULL;
      cache->temp = NULL;
    }
  return false;
}

#endif /* HAVE_GIF */


#ifdef HAVE_WEBP


/***********************************************************************
				 WebP
 ***********************************************************************/

#include "webp/decode.h"
#include "webp/demux.h"

/* Indices of image specification fields in webp_format, below.  */

enum webp_keyword_index
{
  WEBP_TYPE,
  WEBP_DATA,
  WEBP_FILE,
  WEBP_ASCENT,
  WEBP_MARGIN,
  WEBP_RELIEF,
  WEBP_ALGORITHM,
  WEBP_HEURISTIC_MASK,
  WEBP_MASK,
  WEBP_INDEX,
  WEBP_BACKGROUND,
  WEBP_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword webp_format[WEBP_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid WebP image specification.  */

static bool
webp_image_p (Lisp_Object object)
{
  struct image_keyword fmt[WEBP_LAST];
  memcpy (fmt, webp_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, WEBP_LAST, Qwebp))
    return false;

  /* Must specify either the :data or :file keyword.  */
  return fmt[WEBP_FILE].count + fmt[WEBP_DATA].count == 1;
}

#ifdef WINDOWSNT

/* WebP library details.  */

DEF_DLL_FN (int, WebPGetInfo, (const uint8_t *, size_t, int *, int *));
/* WebPGetFeatures is a static inline function defined in WebP's
   decode.h.  Since we cannot use that with dynamically-loaded libwebp
   DLL, we instead load the internal function it calls and redirect to
   that through a macro.  */
DEF_DLL_FN (VP8StatusCode, WebPGetFeaturesInternal,
	    (const uint8_t *, size_t, WebPBitstreamFeatures *, int));
DEF_DLL_FN (uint8_t *, WebPDecodeRGBA, (const uint8_t *, size_t, int *, int *));
DEF_DLL_FN (uint8_t *, WebPDecodeRGB, (const uint8_t *, size_t, int *, int *));
DEF_DLL_FN (void, WebPFree, (void *));
DEF_DLL_FN (uint32_t, WebPDemuxGetI, (const WebPDemuxer *, WebPFormatFeature));
DEF_DLL_FN (WebPDemuxer *, WebPDemuxInternal,
	    (const WebPData *, int, WebPDemuxState *, int));
DEF_DLL_FN (void, WebPDemuxDelete, (WebPDemuxer *));
DEF_DLL_FN (int, WebPAnimDecoderGetNext,
	    (WebPAnimDecoder *, uint8_t **, int *));
DEF_DLL_FN (WebPAnimDecoder *, WebPAnimDecoderNewInternal,
	    (const WebPData *, const WebPAnimDecoderOptions *, int));
DEF_DLL_FN (int, WebPAnimDecoderOptionsInitInternal,
	    (WebPAnimDecoderOptions *, int));
DEF_DLL_FN (int, WebPAnimDecoderHasMoreFrames, (const WebPAnimDecoder *));
DEF_DLL_FN (void, WebPAnimDecoderDelete, (WebPAnimDecoder *));

static bool
init_webp_functions (void)
{
  HMODULE library1, library2;

  if (!((library1 = w32_delayed_load (Qwebp))
	&& (library2 = w32_delayed_load (Qwebpdemux))))
    return false;

  LOAD_DLL_FN (library1, WebPGetInfo);
  LOAD_DLL_FN (library1, WebPGetFeaturesInternal);
  LOAD_DLL_FN (library1, WebPDecodeRGBA);
  LOAD_DLL_FN (library1, WebPDecodeRGB);
  LOAD_DLL_FN (library1, WebPFree);
  LOAD_DLL_FN (library2, WebPDemuxGetI);
  LOAD_DLL_FN (library2, WebPDemuxInternal);
  LOAD_DLL_FN (library2, WebPDemuxDelete);
  LOAD_DLL_FN (library2, WebPAnimDecoderGetNext);
  LOAD_DLL_FN (library2, WebPAnimDecoderNewInternal);
  LOAD_DLL_FN (library2, WebPAnimDecoderOptionsInitInternal);
  LOAD_DLL_FN (library2, WebPAnimDecoderHasMoreFrames);
  LOAD_DLL_FN (library2, WebPAnimDecoderDelete);
  return true;
}

#undef WebPGetInfo
#undef WebPGetFeatures
#undef WebPDecodeRGBA
#undef WebPDecodeRGB
#undef WebPFree
#undef WebPDemuxGetI
#undef WebPDemux
#undef WebPDemuxDelete
#undef WebPAnimDecoderGetNext
#undef WebPAnimDecoderNew
#undef WebPAnimDecoderOptionsInit
#undef WebPAnimDecoderHasMoreFrames
#undef WebPAnimDecoderDelete

#define WebPGetInfo fn_WebPGetInfo
#define WebPGetFeatures(d,s,f)					\
  fn_WebPGetFeaturesInternal(d,s,f,WEBP_DECODER_ABI_VERSION)
#define WebPDecodeRGBA fn_WebPDecodeRGBA
#define WebPDecodeRGB fn_WebPDecodeRGB
#define WebPFree fn_WebPFree
#define WebPDemuxGetI fn_WebPDemuxGetI
#define WebPDemux(d)						\
  fn_WebPDemuxInternal(d,0,NULL,WEBP_DEMUX_ABI_VERSION)
#define WebPDemuxDelete fn_WebPDemuxDelete
#define WebPAnimDecoderGetNext fn_WebPAnimDecoderGetNext
#define WebPAnimDecoderNew(d,o)					\
  fn_WebPAnimDecoderNewInternal(d,o,WEBP_DEMUX_ABI_VERSION)
#define WebPAnimDecoderOptionsInit(o)				\
  fn_WebPAnimDecoderOptionsInitInternal(o,WEBP_DEMUX_ABI_VERSION)
#define WebPAnimDecoderHasMoreFrames fn_WebPAnimDecoderHasMoreFrames
#define WebPAnimDecoderDelete fn_WebPAnimDecoderDelete

#endif /* WINDOWSNT */

static void
webp_destroy (struct anim_cache* cache)
{
  WebPAnimDecoderDelete (cache->handle);
}

/* Load WebP image IMG for use on frame F.  Value is true if
   successful.  */

static bool
webp_load (struct frame *f, struct image *img)
{
  ptrdiff_t size = 0;
  uint8_t *contents;
  Lisp_Object file = Qnil;
  int frames = 0;
  double delay = 0;
  WebPAnimDecoder* anim = NULL;

  /* Open the WebP file.  */
  Lisp_Object specified_file = image_spec_value (img->spec, QCfile, NULL);
  Lisp_Object specified_data = image_spec_value (img->spec, QCdata, NULL);

  if (NILP (specified_data))
    {
      int fd;
      file = image_find_image_fd (specified_file, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file);
	  return false;
	}

      contents = (uint8_t *) slurp_file (fd, &size);
      if (contents == NULL)
	{
	  image_error ("Error loading WebP image `%s'", file);
	  return false;
	}
    }
  else
    {
      if (!STRINGP (specified_data))
	{
	  image_error ("Invalid image data `%s'", specified_data);
	  return false;
	}
      contents = SDATA (specified_data);
      size = SBYTES (specified_data);
    }

  /* Validate the WebP image header.  */
  if (!WebPGetInfo (contents, size, NULL, NULL))
    {
      if (!NILP (file))
	image_error ("Not a WebP file: `%s'", file);
      else
	image_error ("Invalid header in WebP image data");
      goto webp_error1;
    }

  Lisp_Object image_number = image_spec_value (img->spec, QCindex, NULL);
  ptrdiff_t idx = FIXNUMP (image_number) ? XFIXNAT (image_number) : 0;

  /* Get WebP features.  */
  WebPBitstreamFeatures features;
  VP8StatusCode result = WebPGetFeatures (contents, size, &features);
  switch (result)
    {
    case VP8_STATUS_OK:
      break;
    case VP8_STATUS_NOT_ENOUGH_DATA:
    case VP8_STATUS_OUT_OF_MEMORY:
    case VP8_STATUS_INVALID_PARAM:
    case VP8_STATUS_BITSTREAM_ERROR:
    case VP8_STATUS_UNSUPPORTED_FEATURE:
    case VP8_STATUS_SUSPENDED:
    case VP8_STATUS_USER_ABORT:
    default:
      /* Error out in all other cases.  */
      if (!NILP (file))
	image_error ("Error when interpreting WebP image data: `%s'", file);
      else
	image_error ("Error when interpreting WebP image data");
      goto webp_error1;
    }

  uint8_t *decoded = NULL;
  int width, height;

  if (features.has_animation)
    {
      /* Animated image.  */
      int timestamp;

      struct anim_cache* cache = anim_get_animation_cache (XCDR (img->spec));
      /* Get the next frame from the animation cache.  */
      if (cache->handle && cache->index == idx - 1)
	{
	  WebPAnimDecoderGetNext (cache->handle, &decoded, &timestamp);
	  delay = timestamp;
	  cache->index++;
	  anim = cache->handle;
	  width = cache->width;
	  height = cache->height;
	  frames = cache->frames;
	}
      else
	{
	  /* Start a new cache entry.  */
	  if (cache->handle)
	    WebPAnimDecoderDelete (cache->handle);

	  WebPData webp_data;
	  if (NILP (specified_data))
	    /* If we got the data from a file, then we don't need to
	       copy the data. */
	    webp_data.bytes = cache->temp = contents;
	  else
	    /* We got the data from a string, so copy it over so that
	       it doesn't get garbage-collected.  */
	    {
	      webp_data.bytes = xmalloc (size);
	      memcpy ((void*) webp_data.bytes, contents, size);
	    }
	  /* In any case, we release the allocated memory when we
	     purge the anim cache.  */
	  webp_data.size = size;

	  /* This is used just for reporting by `image-cache-size'.  */
	  cache->byte_size = size;

	  /* Get the width/height of the total image.  */
	  WebPDemuxer* demux = WebPDemux (&webp_data);
	  cache->width = width = WebPDemuxGetI (demux, WEBP_FF_CANVAS_WIDTH);
	  cache->height = height = WebPDemuxGetI (demux,
						  WEBP_FF_CANVAS_HEIGHT);
	  cache->frames = frames = WebPDemuxGetI (demux, WEBP_FF_FRAME_COUNT);
	  cache->destructor = (void (*)(void *)) webp_destroy;
	  WebPDemuxDelete (demux);

	  WebPAnimDecoderOptions dec_options;
	  WebPAnimDecoderOptionsInit (&dec_options);
	  anim = WebPAnimDecoderNew (&webp_data, &dec_options);

	  cache->handle = anim;
	  cache->index = idx;

	  while (WebPAnimDecoderHasMoreFrames (anim)) {
	    WebPAnimDecoderGetNext (anim, &decoded, &timestamp);
	    /* Each frame has its own delay, but we don't really support
	       that.  So just use the delay from the first frame.  */
	    if (delay == 0)
	      delay = timestamp;
	    /* Stop when we get to the desired index.  */
	    if (idx-- == 0)
	      break;
	  }
	}
    }
  else
    {
      /* Non-animated image.  */
      if (features.has_alpha)
	/* Linear [r0, g0, b0, a0, r1, g1, b1, a1, ...] order.  */
	decoded = WebPDecodeRGBA (contents, size, &width, &height);
      else
	/* Linear [r0, g0, b0, r1, g1, b1, ...] order.  */
	decoded = WebPDecodeRGB (contents, size, &width, &height);
    }

  if (!decoded)
    {
      image_error ("Error when decoding WebP image data");
      goto webp_error1;
    }

  if (!(width <= INT_MAX && height <= INT_MAX
	&& check_image_size (f, width, height)))
    {
      image_size_error ();
      goto webp_error2;
    }

  /* Create the x image and pixmap.  */
  Emacs_Pix_Container ximg, mask_img = NULL;
  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, false))
    goto webp_error2;

  /* Create an image and pixmap serving as mask if the WebP image
     contains an alpha channel.  */
  if (features.has_alpha
      && !image_create_x_image_and_pixmap (f, img, width, height, 1,
					   &mask_img, true))
    {
      image_destroy_x_image (ximg);
      image_clear_image_1 (f, img, CLEAR_IMAGE_PIXMAP);
      goto webp_error2;
    }

  /* Fill the X image and mask from WebP data.  */
  init_color_table ();

  img->corners[TOP_CORNER] = 0;
  img->corners[LEFT_CORNER] = 0;
  img->corners[BOT_CORNER]
    = img->corners[TOP_CORNER] + height;
  img->corners[RIGHT_CORNER]
    = img->corners[LEFT_CORNER] + width;

  uint8_t *p = decoded;
  for (int y = 0; y < height; ++y)
    {
      for (int x = 0; x < width; ++x)
	{
	  int r = *p++ << 8;
	  int g = *p++ << 8;
	  int b = *p++ << 8;
	  PUT_PIXEL (ximg, x, y, lookup_rgb_color (f, r, g, b));

	  /* An alpha channel associates variable transparency with an
	     image.  WebP allows up to 256 levels of partial transparency.
	     We handle this like with PNG (which see), using the frame's
	     background color to combine the image with.  */
	  if (features.has_alpha || anim)
	    {
	      if (mask_img)
		PUT_PIXEL (mask_img, x, y, *p > 0 ? PIX_MASK_DRAW : PIX_MASK_RETAIN);
	      ++p;
	    }
	}
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  /* Same for the mask.  */
  if (mask_img)
    {
      /* Fill in the background_transparent field while we have the
	 mask handy.  Casting avoids a GCC warning.  */
      image_background_transparent (img, f, (Emacs_Pix_Context)mask_img);

      image_put_x_image (f, img, mask_img, 1);
    }

  img->width = width;
  img->height = height;

  /* Return animation data.  */
  img->lisp_data = Fcons (Qcount,
			  Fcons (make_fixnum (frames),
				 img->lisp_data));
  img->lisp_data = Fcons (Qdelay,
			  Fcons (make_float (delay / 1000),
				 img->lisp_data));

  /* Clean up.  */
  if (!anim)
    WebPFree (decoded);
  if (NILP (specified_data) && !anim)
    xfree (contents);
  return true;

 webp_error2:
  if (!anim)
    WebPFree (decoded);

 webp_error1:
  if (NILP (specified_data))
    xfree (contents);
  return false;
}

#endif /* HAVE_WEBP */


#ifdef HAVE_IMAGEMAGICK


/***********************************************************************
				 ImageMagick
***********************************************************************/

/* Indices of image specification fields in imagemagick_format.  */

enum imagemagick_keyword_index
  {
    IMAGEMAGICK_TYPE,
    IMAGEMAGICK_DATA,
    IMAGEMAGICK_FILE,
    IMAGEMAGICK_ASCENT,
    IMAGEMAGICK_MARGIN,
    IMAGEMAGICK_RELIEF,
    IMAGEMAGICK_ALGORITHM,
    IMAGEMAGICK_HEURISTIC_MASK,
    IMAGEMAGICK_MASK,
    IMAGEMAGICK_BACKGROUND,
    IMAGEMAGICK_HEIGHT,
    IMAGEMAGICK_WIDTH,
    IMAGEMAGICK_MAX_HEIGHT,
    IMAGEMAGICK_MAX_WIDTH,
    IMAGEMAGICK_FORMAT,
    IMAGEMAGICK_ROTATION,
    IMAGEMAGICK_CROP,
    IMAGEMAGICK_LAST
  };

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword imagemagick_format[IMAGEMAGICK_LAST] =
  {
    {":type",		IMAGE_SYMBOL_VALUE,			1},
    {":data",		IMAGE_STRING_VALUE,			0},
    {":file",		IMAGE_STRING_VALUE,			0},
    {":ascent",		IMAGE_ASCENT_VALUE,			0},
    {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
    {":relief",		IMAGE_INTEGER_VALUE,			0},
    {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
    {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
    {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
    {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
    {":height",		IMAGE_INTEGER_VALUE,			0},
    {":width",		IMAGE_INTEGER_VALUE,			0},
    {":max-height",	IMAGE_INTEGER_VALUE,			0},
    {":max-width",	IMAGE_INTEGER_VALUE,			0},
    {":format",		IMAGE_SYMBOL_VALUE,			0},
    {":rotation",	IMAGE_NUMBER_VALUE,     		0},
    {":crop",		IMAGE_DONT_CHECK_VALUE_TYPE,		0}
  };

/* Free X resources of imagemagick image IMG which is used on frame F.  */

static void
imagemagick_clear_image (struct frame *f,
                         struct image *img)
{
  image_clear_image (f, img);
}

/* Return true if OBJECT is a valid IMAGEMAGICK image specification.  Do
   this by calling parse_image_spec and supplying the keywords that
   identify the IMAGEMAGICK format.   */

static bool
imagemagick_image_p (Lisp_Object object)
{
  struct image_keyword fmt[IMAGEMAGICK_LAST];
  memcpy (fmt, imagemagick_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, IMAGEMAGICK_LAST, Qimagemagick))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[IMAGEMAGICK_FILE].count + fmt[IMAGEMAGICK_DATA].count == 1;
}

/* The GIF library also defines DrawRectangle, but its never used in Emacs.
   Therefore rename the function so it doesn't collide with ImageMagick.  */
#define DrawRectangle DrawRectangleGif

#ifdef HAVE_IMAGEMAGICK7
# include <MagickWand/MagickWand.h>
# include <MagickCore/version.h>
/* ImageMagick 7 compatibility definitions.  */
# define PixelSetMagickColor PixelSetPixelColor
typedef PixelInfo MagickPixelPacket;
#else
# include <wand/MagickWand.h>
# include <magick/version.h>
#endif

/* ImageMagick 6.5.3 through 6.6.5 hid PixelGetMagickColor for some reason.
   Emacs seems to work fine with the hidden version, so unhide it.  */
#if 0x653 <= MagickLibVersion && MagickLibVersion <= 0x665
extern WandExport void PixelGetMagickColor (const PixelWand *,
					    MagickPixelPacket *);
#endif

static void
imagemagick_initialize (void)
{
  static bool imagemagick_initialized;
  if (!imagemagick_initialized)
    {
      imagemagick_initialized = true;
      MagickWandGenesis ();
    }
}

/* Log ImageMagick error message.
   Useful when an ImageMagick function returns the status `MagickFalse'.  */

static void
imagemagick_error (MagickWand *wand)
{
  char *description;
  ExceptionType severity;

  description = MagickGetException (wand, &severity);
  image_error ("ImageMagick error: %s", build_string (description));
  MagickRelinquishMemory (description);
}

/* Possibly give ImageMagick some extra help to determine the image
   type by supplying a "dummy" filename based on the Content-Type.  */

static char *
imagemagick_filename_hint (Lisp_Object spec, char hint_buffer[MaxTextExtent])
{
  Lisp_Object symbol = intern ("image-format-suffixes");
  Lisp_Object val = find_symbol_value (symbol);
  Lisp_Object format;

  if (! CONSP (val))
    return NULL;

  format = image_spec_value (spec, intern (":format"), NULL);
  val = Fcar_safe (Fcdr_safe (Fassq (format, val)));
  if (! STRINGP (val))
    return NULL;

  /* It's OK to truncate the hint if it has MaxTextExtent or more bytes,
     as ImageMagick would ignore the extra bytes anyway.  */
  snprintf (hint_buffer, MaxTextExtent, "/tmp/foo.%s", SSDATA (val));
  return hint_buffer;
}

/* Animated images (e.g., GIF89a) are composed from one "master image"
   (which is the first one, and then there's a number of images that
   follow.  If following images have non-transparent colors, these are
   composed "on top" of the master image.  So, in general, one has to
   compute all the preceding images to be able to display a particular
   sub-image.

   Computing all the preceding images is too slow, so we maintain a
   cache of previously computed images.  We have to maintain a cache
   separate from the image cache, because the images may be scaled
   before display. */

struct animation_cache
{
  MagickWand *wand;
  int index;
  struct timespec update_time;
  struct animation_cache *next;
  char signature[FLEXIBLE_ARRAY_MEMBER];
};

static struct animation_cache *animation_cache = NULL;

static struct animation_cache *
imagemagick_create_cache (char *signature)
{
  struct animation_cache *cache
    = xmalloc (FLEXSIZEOF (struct animation_cache, signature,
			   strlen (signature) + 1));
  cache->wand = 0;
  cache->index = 0;
  cache->next = 0;
  strcpy (cache->signature, signature);
  return cache;
}

/* Discard cached images that haven't been used for a minute.  If
   CLEAR, discard all cached animated images.  */
static void
imagemagick_prune_animation_cache (bool clear)
{
  struct animation_cache **pcache = &animation_cache;
  struct timespec old = timespec_sub (current_timespec (),
				      make_timespec (60, 0));

  while (*pcache)
    {
      struct animation_cache *cache = *pcache;
      if (clear || timespec_cmp (old, cache->update_time) > 0)
	{
	  if (cache->wand)
	    DestroyMagickWand (cache->wand);
	  *pcache = cache->next;
	  xfree (cache);
	}
      else
	pcache = &cache->next;
    }
}

static struct animation_cache *
imagemagick_get_animation_cache (MagickWand *wand)
{
  char *signature = MagickGetImageSignature (wand);
  struct animation_cache *cache;
  struct animation_cache **pcache = &animation_cache;

  imagemagick_prune_animation_cache (false);

  while (1)
    {
      cache = *pcache;
      if (! cache)
	{
          *pcache = cache = imagemagick_create_cache (signature);
          break;
        }
      if (strcmp (signature, cache->signature) == 0)
	break;
      pcache = &cache->next;
    }

  DestroyString (signature);
  cache->update_time = current_timespec ();
  return cache;
}

static MagickWand *
imagemagick_compute_animated_image (MagickWand *super_wand, int ino)
{
  int i;
  MagickWand *composite_wand;
  size_t dest_width, dest_height;
  struct animation_cache *cache = imagemagick_get_animation_cache (super_wand);

  MagickSetIteratorIndex (super_wand, 0);

  if (ino == 0 || cache->wand == NULL || cache->index > ino)
    {
      composite_wand = MagickGetImage (super_wand);
      if (cache->wand)
	DestroyMagickWand (cache->wand);
    }
  else
    composite_wand = cache->wand;

  dest_height = MagickGetImageHeight (composite_wand);

  for (i = max (1, cache->index + 1); i <= ino; i++)
    {
      MagickWand *sub_wand;
      PixelIterator *source_iterator, *dest_iterator;
      PixelWand **source, **dest;
      size_t source_width, source_height;
      ssize_t source_left, source_top;
      MagickPixelPacket pixel;
      DisposeType dispose;
      ptrdiff_t lines = 0;

      MagickSetIteratorIndex (super_wand, i);
      sub_wand = MagickGetImage (super_wand);

      MagickGetImagePage (sub_wand, &source_width, &source_height,
			  &source_left, &source_top);

      /* This flag says how to handle transparent pixels.  */
      dispose = MagickGetImageDispose (sub_wand);

      source_iterator = NewPixelIterator (sub_wand);
      if (! source_iterator)
	{
	  DestroyMagickWand (composite_wand);
	  DestroyMagickWand (sub_wand);
	  cache->wand = NULL;
	  image_error ("Imagemagick pixel iterator creation failed");
	  return NULL;
	}

      dest_iterator = NewPixelIterator (composite_wand);
      if (! dest_iterator)
	{
	  DestroyMagickWand (composite_wand);
	  DestroyMagickWand (sub_wand);
	  DestroyPixelIterator (source_iterator);
	  cache->wand = NULL;
	  image_error ("Imagemagick pixel iterator creation failed");
	  return NULL;
	}

      /* The sub-image may not start at origin, so move the destination
	 iterator to where the sub-image should start. */
      if (source_top > 0)
	{
	  PixelSetIteratorRow (dest_iterator, source_top);
	  lines = source_top;
	}

      while ((source = PixelGetNextIteratorRow (source_iterator, &source_width))
	     != NULL)
	{
	  ptrdiff_t x;

	  /* Sanity check.  This shouldn't happen, but apparently
	     does in some pictures.  */
	  if (++lines >= dest_height)
	    break;

	  dest = PixelGetNextIteratorRow (dest_iterator, &dest_width);
	  for (x = 0; x < source_width; x++)
	    {
	      /* Sanity check.  This shouldn't happen, but apparently
		 also does in some pictures.  */
	      if (x + source_left >= dest_width)
		break;
	      /* Normally we only copy over non-transparent pixels,
		 but if the disposal method is "Background", then we
		 copy over all pixels.  */
	      if (dispose == BackgroundDispose || PixelGetAlpha (source[x]))
		{
		  PixelGetMagickColor (source[x], &pixel);
		  PixelSetMagickColor (dest[x + source_left], &pixel);
		}
	    }
	  PixelSyncIterator (dest_iterator);
	}

      DestroyPixelIterator (source_iterator);
      DestroyPixelIterator (dest_iterator);
      DestroyMagickWand (sub_wand);
    }

  /* Cache a copy for the next iteration.  The current wand will be
     destroyed by the caller. */
  cache->wand = CloneMagickWand (composite_wand);
  cache->index = ino;

  return composite_wand;
}


/* Helper function for imagemagick_load, which does the actual loading
   given contents and size, apart from frame and image structures,
   passed from imagemagick_load.  Uses librimagemagick to do most of
   the image processing.

   F is a pointer to the Emacs frame; IMG to the image structure to
   prepare; CONTENTS is the string containing the IMAGEMAGICK data to
   be parsed; SIZE is the number of bytes of data; and FILENAME is
   either the file name or the image data.

   Return true if successful.  */

static bool
imagemagick_load_image (struct frame *f, struct image *img,
			unsigned char *contents, unsigned int size,
			char *filename)
{
  int width, height;
  size_t image_width, image_height;
  MagickBooleanType status;
  Emacs_Pix_Container ximg;
  int x, y;
  MagickWand *image_wand;
  PixelIterator *iterator;
  PixelWand **pixels, *bg_wand = NULL;
  MagickPixelPacket  pixel;
  Lisp_Object image;
#ifndef DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE
  Lisp_Object value;
#endif
  Lisp_Object crop;
  EMACS_INT ino;
  int desired_width, desired_height;
#ifndef DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE
  double rotation;
#endif
  char hint_buffer[MaxTextExtent];
  char *filename_hint = NULL;
  imagemagick_initialize ();

  /* Handle image index for image types who can contain more than one image.
     Interface :index is same as for GIF.  First we "ping" the image to see how
     many sub-images it contains.  Pinging is faster than loading the image to
     find out things about it.  */

  image = image_spec_value (img->spec, QCindex, NULL);
  ino = FIXNUMP (image) ? XFIXNAT (image) : 0;
  image_wand = NewMagickWand ();

  if (filename)
    status = MagickReadImage (image_wand, filename);
  else
    {
      Lisp_Object lwidth = image_spec_value (img->spec, QCwidth, NULL);
      Lisp_Object lheight = image_spec_value (img->spec, QCheight, NULL);

      if (FIXNATP (lwidth) && FIXNATP (lheight))
	{
	  MagickSetSize (image_wand, XFIXNAT (lwidth), XFIXNAT (lheight));
	  MagickSetDepth (image_wand, 8);
	}
      filename_hint = imagemagick_filename_hint (img->spec, hint_buffer);
      MagickSetFilename (image_wand, filename_hint);
      status = MagickReadImageBlob (image_wand, contents, size);
    }

  if (status == MagickFalse)
    {
      imagemagick_error (image_wand);
      DestroyMagickWand (image_wand);
      return 0;
    }

#ifdef HAVE_MAGICKAUTOORIENTIMAGE
  /* If no :rotation is explicitly specified, apply the automatic
     rotation from EXIF. */
  if (NILP (image_spec_value (img->spec, QCrotation, NULL)))
    if (MagickAutoOrientImage (image_wand) == MagickFalse)
      {
        image_error ("Error applying automatic orientation in image `%s'", img->spec);
        DestroyMagickWand (image_wand);
        return 0;
      }
#endif

  if (ino < 0 || ino >= MagickGetNumberImages (image_wand))
    {
      image_error ("Invalid image number `%s' in image `%s'", image, img->spec);
      DestroyMagickWand (image_wand);
      return 0;
    }

  if (MagickGetImageDelay (image_wand) > 0)
    img->lisp_data =
      Fcons (Qdelay,
             Fcons (make_float (MagickGetImageDelay (image_wand) / 100.0),
                    img->lisp_data));

  if (MagickGetNumberImages (image_wand) > 1)
    img->lisp_data =
      Fcons (Qcount,
             Fcons (make_fixnum (MagickGetNumberImages (image_wand)),
                    img->lisp_data));

  /* If we have an animated image, get the new wand based on the
     "super-wand". */
  if (MagickGetNumberImages (image_wand) > 1)
    {
      /* This is an animated image (it has a delay), so compute the
	 composite image etc. */
      if (MagickGetImageDelay (image_wand) > 0)
	{
	  MagickWand *super_wand = image_wand;
	  image_wand = imagemagick_compute_animated_image (super_wand, ino);
	  if (! image_wand)
	    image_wand = super_wand;
	  else
	    DestroyMagickWand (super_wand);
	}
      else
	/* This is not an animated image: It's just a multi-image file
	   (like an .ico file).  Just return the correct
	   sub-image.  */
	{
	  MagickWand *super_wand = image_wand;

	  MagickSetIteratorIndex (super_wand, ino);
	  image_wand = MagickGetImage (super_wand);
	  DestroyMagickWand (super_wand);
	}
    }

  /* Retrieve the frame's background color, for use later.  */
  {
    Emacs_Color bgcolor;
    Lisp_Object specified_bg;

    specified_bg = image_spec_value (img->spec, QCbackground, NULL);
    if (!STRINGP (specified_bg)
	|| !FRAME_TERMINAL (f)->defined_color_hook (f,
                                                    SSDATA (specified_bg),
                                                    &bgcolor,
                                                    false,
                                                    false))
      FRAME_TERMINAL (f)->query_frame_background_color (f, &bgcolor);

    bg_wand = NewPixelWand ();
    PixelSetRed   (bg_wand, (double) bgcolor.red   / 65535);
    PixelSetGreen (bg_wand, (double) bgcolor.green / 65535);
    PixelSetBlue  (bg_wand, (double) bgcolor.blue  / 65535);
  }

#ifndef DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE
  compute_image_size (MagickGetImageWidth (image_wand),
		      MagickGetImageHeight (image_wand),
		      img, &desired_width, &desired_height);
#else
  desired_width = desired_height = -1;
#endif

  if (desired_width != -1 && desired_height != -1)
    {
      status = MagickScaleImage (image_wand, desired_width, desired_height);
      if (status == MagickFalse)
	{
	  image_error ("Imagemagick scale failed");
	  imagemagick_error (image_wand);
	  goto imagemagick_error;
	}
    }

  /* crop behaves similar to image slicing in Emacs but is more memory
     efficient.  */
  crop = image_spec_value (img->spec, QCcrop, NULL);

  if (CONSP (crop) && TYPE_RANGED_FIXNUMP (size_t, XCAR (crop)))
    {
      /* After some testing, it seems MagickCropImage is the fastest crop
         function in ImageMagick.  This crop function seems to do less copying
         than the alternatives, but it still reads the entire image into memory
         before cropping, which is apparently difficult to avoid when using
         imagemagick.  */
      size_t crop_width = XFIXNUM (XCAR (crop));
      crop = XCDR (crop);
      if (CONSP (crop) && TYPE_RANGED_FIXNUMP (size_t, XCAR (crop)))
	{
	  size_t crop_height = XFIXNUM (XCAR (crop));
	  crop = XCDR (crop);
	  if (CONSP (crop) && TYPE_RANGED_FIXNUMP (ssize_t, XCAR (crop)))
	    {
	      ssize_t crop_x = XFIXNUM (XCAR (crop));
	      crop = XCDR (crop);
	      if (CONSP (crop) && TYPE_RANGED_FIXNUMP (ssize_t, XCAR (crop)))
		{
		  ssize_t crop_y = XFIXNUM (XCAR (crop));
		  MagickCropImage (image_wand, crop_width, crop_height,
				   crop_x, crop_y);
		}
	    }
	}
    }

#ifndef DONT_CREATE_TRANSFORMED_IMAGEMAGICK_IMAGE
  /* Furthermore :rotation. we need background color and angle for
     rotation.  */
  /*
    TODO background handling for rotation specified_bg =
    image_spec_value (img->spec, QCbackground, NULL); if (!STRINGP
    (specified_bg).  */
  value = image_spec_value (img->spec, QCrotation, NULL);
  if (FLOATP (value))
    {
      rotation = XFLOAT_DATA (value);
      status = MagickRotateImage (image_wand, bg_wand, rotation);
      if (status == MagickFalse)
        {
          image_error ("Imagemagick image rotate failed");
	  imagemagick_error (image_wand);
          goto imagemagick_error;
        }
    }
#endif

  /* Set the canvas background color to the frame or specified
     background, and flatten the image.  Note: as of ImageMagick
     6.6.0, SVG image transparency is not handled properly
     (e.g. etc/images/splash.svg shows a white background always).  */
  {
    MagickWand *new_wand;
    MagickSetImageBackgroundColor (image_wand, bg_wand);
#ifdef HAVE_MAGICKMERGEIMAGELAYERS
    new_wand = MagickMergeImageLayers (image_wand, MergeLayer);
#else
    new_wand = MagickFlattenImages (image_wand);
#endif
    DestroyMagickWand (image_wand);
    image_wand = new_wand;
  }

  /* Finally we are done manipulating the image.  Figure out the
     resulting width/height and transfer ownership to Emacs.  */
  image_height = MagickGetImageHeight (image_wand);
  image_width = MagickGetImageWidth (image_wand);

  if (! (image_width <= INT_MAX && image_height <= INT_MAX
	 && check_image_size (f, image_width, image_height)))
    {
      image_size_error ();
      goto imagemagick_error;
    }

  width = image_width;
  height = image_height;

  /* We can now get a valid pixel buffer from the imagemagick file, if all
     went ok.  */

  init_color_table ();

#if defined (HAVE_MAGICKEXPORTIMAGEPIXELS) && \
  ! defined (HAVE_NS) && ! defined (HAVE_HAIKU)
  if (imagemagick_render_type != 0)
    {
      /* Magicexportimage is normally faster than pixelpushing.  This
         method is also well tested.  Some aspects of this method are
         ad-hoc and needs to be more researched. */
      void *dataptr;
      int imagedepth = 24; /*MagickGetImageDepth(image_wand);*/
      const char *exportdepth = imagedepth <= 8 ? "I" : "BGRP"; /*"RGBP";*/
      /* Try to create a x pixmap to hold the imagemagick pixmap.  */
      if (!image_create_x_image_and_pixmap (f, img, width, height, imagedepth,
					    &ximg, 0))
	{
#ifdef COLOR_TABLE_SUPPORT
	  free_color_table ();
#endif
	  image_error ("Imagemagick X bitmap allocation failure");
	  goto imagemagick_error;
	}
      dataptr = ximg->data;

      /* Oddly, the below code doesn't seem to work:*/
      /* switch(ximg->bitmap_unit){ */
      /* case 8: */
      /*   pixelwidth=CharPixel; */
      /*   break; */
      /* case   16: */
      /*   pixelwidth=ShortPixel; */
      /*   break; */
      /* case   32: */
      /*   pixelwidth=LongPixel; */
      /*   break; */
      /* } */
      /*
        Here im just guessing the format of the bitmap.
        happens to work fine for:
        - bw djvu images
        on rgb display.
        seems about 3 times as fast as pixel pushing(not carefully measured)
      */
      int pixelwidth = CharPixel; /*??? TODO figure out*/
      MagickExportImagePixels (image_wand, 0, 0, width, height,
			       exportdepth, pixelwidth, dataptr);
    }
  else
#endif /* HAVE_MAGICKEXPORTIMAGEPIXELS */
    {
      size_t image_height;
      double quantum_range = QuantumRange;
      MagickRealType color_scale = 65535.0 / quantum_range;
      /* Try to create a x pixmap to hold the imagemagick pixmap.  */
      if (!image_create_x_image_and_pixmap (f, img, width, height, 0,
					    &ximg, 0))
        {
#ifdef COLOR_TABLE_SUPPORT
	  free_color_table ();
#endif
          image_error ("Imagemagick X bitmap allocation failure");
          goto imagemagick_error;
        }

      /* Copy imagemagick image to x with primitive yet robust pixel
         pusher loop.  This has been tested a lot with many different
         images.  */

      /* Copy pixels from the imagemagick image structure to the x image map. */
      iterator = NewPixelIterator (image_wand);
      if (! iterator)
        {
#ifdef COLOR_TABLE_SUPPORT
	  free_color_table ();
#endif
	  image_destroy_x_image (ximg);
          image_error ("Imagemagick pixel iterator creation failed");
          goto imagemagick_error;
        }

      image_height = MagickGetImageHeight (image_wand);
      for (y = 0; y < image_height; y++)
        {
	  size_t row_width;
	  pixels = PixelGetNextIteratorRow (iterator, &row_width);
          if (! pixels)
            break;
	  int xlim = min (row_width, width);
	  for (x = 0; x < xlim; x++)
            {
              PixelGetMagickColor (pixels[x], &pixel);
              PUT_PIXEL (ximg, x, y,
                         lookup_rgb_color (f,
					   color_scale * pixel.red,
					   color_scale * pixel.green,
					   color_scale * pixel.blue));
	    }
	}
      DestroyPixelIterator (iterator);
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  img->width  = width;
  img->height = height;

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  /* Final cleanup. image_wand should be the only resource left. */
  DestroyMagickWand (image_wand);
  if (bg_wand) DestroyPixelWand (bg_wand);

  /* Do not call MagickWandTerminus, to work around ImageMagick bug 825.  See:
     https://github.com/ImageMagick/ImageMagick/issues/825
     Although this bug was introduced in ImageMagick 6.9.9-14 and
     fixed in 6.9.9-18, it's simpler to work around it in all versions.  */

  return 1;

 imagemagick_error:
  DestroyMagickWand (image_wand);
  if (bg_wand) DestroyPixelWand (bg_wand);

  /* TODO more cleanup.  */
  image_error ("Error parsing IMAGEMAGICK image `%s'", img->spec);
  return 0;
}


/* Load IMAGEMAGICK image IMG for use on frame F.  Value is true if
   successful. this function will go into the imagemagick_type structure, and
   the prototype thus needs to be compatible with that structure.  */

static bool
imagemagick_load (struct frame *f, struct image *img)
{
  bool success_p = 0;
  Lisp_Object file_name;

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file = image_find_image_file (file_name);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name);
	  return 0;
	}
      file = ENCODE_FILE (file);
#ifdef WINDOWSNT
      file = ansi_encode_filename (file);
#endif
      success_p = imagemagick_load_image (f, img, 0, 0, SSDATA (file));
    }
  /* Else it's not a file, it's a Lisp object.  Load the image from a
     Lisp object rather than a file.  */
  else
    {
      Lisp_Object data;

      data = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (data))
	{
	  image_error ("Invalid image data `%s'", data);
	  return 0;
	}
      success_p = imagemagick_load_image (f, img, SDATA (data),
                                          SBYTES (data), NULL);
    }

  return success_p;
}

DEFUN ("imagemagick-types", Fimagemagick_types, Simagemagick_types, 0, 0, 0,
       doc: /* Return a list of image types supported by ImageMagick.
Each entry in this list is a symbol named after an ImageMagick format
tag.  See the ImageMagick manual for a list of ImageMagick formats and
their descriptions (https://www.imagemagick.org/script/formats.php).
You can also try the shell command: `identify -list format'.

Note that ImageMagick recognizes many file-types that Emacs does not
recognize as images, such as C.  See `imagemagick-enabled-types'
and `imagemagick-types-inhibit'.  */)
  (void)
{
  Lisp_Object typelist = Qnil;
  size_t numf = 0;
  ExceptionInfo *ex;
  char **imtypes;
  size_t i;

  imagemagick_initialize ();
  ex = AcquireExceptionInfo ();
  imtypes = GetMagickList ("*", &numf, ex);
  DestroyExceptionInfo (ex);

  for (i = 0; i < numf; i++)
    {
      Lisp_Object imagemagicktype = intern (imtypes[i]);
      typelist = Fcons (imagemagicktype, typelist);
      imtypes[i] = MagickRelinquishMemory (imtypes[i]);
    }

  MagickRelinquishMemory (imtypes);
  return Fnreverse (typelist);
}

#endif	/* defined (HAVE_IMAGEMAGICK) */



/***********************************************************************
				 SVG
 ***********************************************************************/

#ifdef HAVE_RSVG

/* Function prototypes.  */

static bool svg_load_image (struct frame *, struct image *,
			    char *, ptrdiff_t, char *);

/* Indices of image specification fields in svg_format, below.  */

enum svg_keyword_index
{
  SVG_TYPE,
  SVG_DATA,
  SVG_FILE,
  SVG_BASE_URI,
  SVG_CSS,
  SVG_ASCENT,
  SVG_MARGIN,
  SVG_RELIEF,
  SVG_ALGORITHM,
  SVG_HEURISTIC_MASK,
  SVG_MASK,
  SVG_FOREGROUND,
  SVG_BACKGROUND,
  SVG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword svg_format[SVG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":base-uri",		IMAGE_STRING_VALUE,			0},
  {":css",		IMAGE_STRING_VALUE,                     0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid SVG image specification.  Do
   this by calling parse_image_spec and supplying the keywords that
   identify the SVG format.   */

static bool
svg_image_p (Lisp_Object object)
{
  struct image_keyword fmt[SVG_LAST];
  memcpy (fmt, svg_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, SVG_LAST, Qsvg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[SVG_FILE].count + fmt[SVG_DATA].count == 1;
}

/* Some versions of glib's gatomic.h define MemoryBarrier, but MinGW
   w32api 3.18 and later has its own definition.  The following gross
   hack avoids the clash.  */
# ifdef WINDOWSNT
#  if (__W32API_MAJOR_VERSION + (__W32API_MINOR_VERSION >= 18)) >= 4
#   define W32_SAVE_MINGW_VERSION __MINGW_MAJOR_VERSION
#   undef __MINGW_MAJOR_VERSION
#   define __MINGW_MAJOR_VERSION 4
#  endif
# endif

# include <librsvg/rsvg.h>

/* librsvg is too old for us if it doesn't define this macro.  */
# ifndef LIBRSVG_CHECK_VERSION
#  define LIBRSVG_CHECK_VERSION(v, w, x) false
# endif

# ifdef WINDOWSNT

/* Restore the original definition of __MINGW_MAJOR_VERSION.  */
#  if defined W32_SAVE_MINGW_VERSION && defined __MINGW_MAJOR_VERSION
#   undef __MINGW_MAJOR_VERSION
#   define __MINGW_MAJOR_VERSION W32_SAVE_MINGW_VERSION
#   ifdef __MINGW_MAJOR_VERSION
#    undef W32_SAVE_MINGW_VERSION
#   endif
#  endif

/* SVG library functions.  */
#  if LIBRSVG_CHECK_VERSION (2, 32, 0)
DEF_DLL_FN (GFile *, g_file_new_for_path, (char const *));
DEF_DLL_FN (GInputStream *, g_memory_input_stream_new_from_data,
	    (void const *, gssize, GDestroyNotify));
DEF_DLL_FN (RsvgHandle *, rsvg_handle_new_from_stream_sync,
	    (GInputStream *, GFile *, RsvgHandleFlags, GCancellable *,
	     GError **error));
#  else
DEF_DLL_FN (RsvgHandle *, rsvg_handle_new, (void));
DEF_DLL_FN (void, rsvg_handle_set_base_uri, (RsvgHandle *, const char *));
DEF_DLL_FN (gboolean, rsvg_handle_write,
	    (RsvgHandle *, const guchar *, gsize, GError **));
DEF_DLL_FN (gboolean, rsvg_handle_close, (RsvgHandle *, GError **));
#  endif

DEF_DLL_FN (void, rsvg_handle_set_dpi_x_y,
	    (RsvgHandle * handle, double dpi_x, double dpi_y));

#  if LIBRSVG_CHECK_VERSION (2, 52, 1)
DEF_DLL_FN (gboolean, rsvg_handle_get_intrinsic_size_in_pixels,
            (RsvgHandle *, gdouble *, gdouble *));
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 46, 0)
DEF_DLL_FN (void, rsvg_handle_get_intrinsic_dimensions,
            (RsvgHandle *, gboolean *, RsvgLength *, gboolean *,
            RsvgLength *, gboolean *, RsvgRectangle *));
DEF_DLL_FN (gboolean, rsvg_handle_get_geometry_for_layer,
	    (RsvgHandle *, const char *, const RsvgRectangle *,
	     RsvgRectangle *, RsvgRectangle *, GError **));
#  else
DEF_DLL_FN (void, rsvg_handle_get_dimensions,
	    (RsvgHandle *, RsvgDimensionData *));
#  endif

#  if LIBRSVG_CHECK_VERSION (2, 48, 0)
DEF_DLL_FN (gboolean, rsvg_handle_set_stylesheet,
	    (RsvgHandle *, const guint8 *, gsize, GError **));
#  endif
DEF_DLL_FN (GdkPixbuf *, rsvg_handle_get_pixbuf, (RsvgHandle *));
DEF_DLL_FN (int, gdk_pixbuf_get_width, (const GdkPixbuf *));
DEF_DLL_FN (int, gdk_pixbuf_get_height, (const GdkPixbuf *));
DEF_DLL_FN (guchar *, gdk_pixbuf_get_pixels, (const GdkPixbuf *));
DEF_DLL_FN (int, gdk_pixbuf_get_rowstride, (const GdkPixbuf *));
DEF_DLL_FN (GdkColorspace, gdk_pixbuf_get_colorspace, (const GdkPixbuf *));
DEF_DLL_FN (int, gdk_pixbuf_get_n_channels, (const GdkPixbuf *));
DEF_DLL_FN (gboolean, gdk_pixbuf_get_has_alpha, (const GdkPixbuf *));
DEF_DLL_FN (int, gdk_pixbuf_get_bits_per_sample, (const GdkPixbuf *));

#  if ! GLIB_CHECK_VERSION (2, 36, 0)
DEF_DLL_FN (void, g_type_init, (void));
#  endif
DEF_DLL_FN (void, g_object_unref, (gpointer));
DEF_DLL_FN (void, g_error_free, (GError *));

static bool
init_svg_functions (void)
{
  HMODULE library, gdklib = NULL, glib = NULL, gobject = NULL, giolib = NULL;

  if (!(glib = w32_delayed_load (Qglib))
      || !(gobject = w32_delayed_load (Qgobject))
#  if LIBRSVG_CHECK_VERSION (2, 32, 0)
      || !(giolib = w32_delayed_load (Qgio))
#  endif
      || !(gdklib = w32_delayed_load (Qgdk_pixbuf))
      || !(library = w32_delayed_load (Qsvg)))
    {
      if (gdklib)  FreeLibrary (gdklib);
      if (giolib)  FreeLibrary (giolib);
      if (gobject) FreeLibrary (gobject);
      if (glib)    FreeLibrary (glib);
      return 0;
    }

#if LIBRSVG_CHECK_VERSION (2, 32, 0)
  LOAD_DLL_FN (giolib, g_file_new_for_path);
  LOAD_DLL_FN (giolib, g_memory_input_stream_new_from_data);
  LOAD_DLL_FN (library, rsvg_handle_new_from_stream_sync);
#else
  LOAD_DLL_FN (library, rsvg_handle_new);
  LOAD_DLL_FN (library, rsvg_handle_set_base_uri);
  LOAD_DLL_FN (library, rsvg_handle_write);
  LOAD_DLL_FN (library, rsvg_handle_close);
#endif
  LOAD_DLL_FN (library, rsvg_handle_set_dpi_x_y);
#if LIBRSVG_CHECK_VERSION (2, 52, 1)
  LOAD_DLL_FN (library, rsvg_handle_get_intrinsic_size_in_pixels);
#endif
#if LIBRSVG_CHECK_VERSION (2, 46, 0)
  LOAD_DLL_FN (library, rsvg_handle_get_intrinsic_dimensions);
  LOAD_DLL_FN (library, rsvg_handle_get_geometry_for_layer);
#else
  LOAD_DLL_FN (library, rsvg_handle_get_dimensions);
#endif
#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  LOAD_DLL_FN (library, rsvg_handle_set_stylesheet);
#endif
  LOAD_DLL_FN (library, rsvg_handle_get_pixbuf);

  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_width);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_height);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_pixels);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_rowstride);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_colorspace);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_n_channels);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_has_alpha);
  LOAD_DLL_FN (gdklib, gdk_pixbuf_get_bits_per_sample);

#  if ! GLIB_CHECK_VERSION (2, 36, 0)
  LOAD_DLL_FN (gobject, g_type_init);
#  endif
  LOAD_DLL_FN (gobject, g_object_unref);
  LOAD_DLL_FN (glib, g_error_free);

  return 1;
}

/* The following aliases for library functions allow dynamic loading
   to be used on some platforms.  */

#  undef gdk_pixbuf_get_bits_per_sample
#  undef gdk_pixbuf_get_colorspace
#  undef gdk_pixbuf_get_has_alpha
#  undef gdk_pixbuf_get_height
#  undef gdk_pixbuf_get_n_channels
#  undef gdk_pixbuf_get_pixels
#  undef gdk_pixbuf_get_rowstride
#  undef gdk_pixbuf_get_width
#  undef g_error_free
#  undef g_object_unref
#  undef g_type_init
#  if LIBRSVG_CHECK_VERSION (2, 52, 1)
#   undef rsvg_handle_get_intrinsic_size_in_pixels
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 46, 0)
#   undef rsvg_handle_get_intrinsic_dimensions
#   undef rsvg_handle_get_geometry_for_layer
#  else
#   undef rsvg_handle_get_dimensions
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 48, 0)
#   undef rsvg_handle_set_stylesheet
#  endif
#  undef rsvg_handle_get_pixbuf
#  if LIBRSVG_CHECK_VERSION (2, 32, 0)
#   undef g_file_new_for_path
#   undef g_memory_input_stream_new_from_data
#   undef rsvg_handle_new_from_stream_sync
#  else
#   undef rsvg_handle_close
#   undef rsvg_handle_new
#   undef rsvg_handle_set_base_uri
#   undef rsvg_handle_write
#  endif
#  undef rsvg_handle_set_dpi_x_y

#  define gdk_pixbuf_get_bits_per_sample fn_gdk_pixbuf_get_bits_per_sample
#  define gdk_pixbuf_get_colorspace fn_gdk_pixbuf_get_colorspace
#  define gdk_pixbuf_get_has_alpha fn_gdk_pixbuf_get_has_alpha
#  define gdk_pixbuf_get_height fn_gdk_pixbuf_get_height
#  define gdk_pixbuf_get_n_channels fn_gdk_pixbuf_get_n_channels
#  define gdk_pixbuf_get_pixels fn_gdk_pixbuf_get_pixels
#  define gdk_pixbuf_get_rowstride fn_gdk_pixbuf_get_rowstride
#  define gdk_pixbuf_get_width fn_gdk_pixbuf_get_width
#  define g_error_free fn_g_error_free
#  define g_object_unref fn_g_object_unref
#  if ! GLIB_CHECK_VERSION (2, 36, 0)
#   define g_type_init fn_g_type_init
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 52, 1)
#   define rsvg_handle_get_intrinsic_size_in_pixels \
	fn_rsvg_handle_get_intrinsic_size_in_pixels
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 46, 0)
#   define rsvg_handle_get_intrinsic_dimensions \
	fn_rsvg_handle_get_intrinsic_dimensions
#   define rsvg_handle_get_geometry_for_layer	\
	fn_rsvg_handle_get_geometry_for_layer
#  else
#   define rsvg_handle_get_dimensions fn_rsvg_handle_get_dimensions
#  endif
#  if LIBRSVG_CHECK_VERSION (2, 48, 0)
#   define rsvg_handle_set_stylesheet fn_rsvg_handle_set_stylesheet
#  endif
#  define rsvg_handle_get_pixbuf fn_rsvg_handle_get_pixbuf
#  if LIBRSVG_CHECK_VERSION (2, 32, 0)
#   define g_file_new_for_path fn_g_file_new_for_path
#   define g_memory_input_stream_new_from_data \
	fn_g_memory_input_stream_new_from_data
#   define rsvg_handle_new_from_stream_sync fn_rsvg_handle_new_from_stream_sync
#  else
#   define rsvg_handle_close fn_rsvg_handle_close
#   define rsvg_handle_new fn_rsvg_handle_new
#   define rsvg_handle_set_base_uri fn_rsvg_handle_set_base_uri
#   define rsvg_handle_write fn_rsvg_handle_write
#  endif
#  define rsvg_handle_set_dpi_x_y fn_rsvg_handle_set_dpi_x_y

# endif /* !WINDOWSNT  */

/* Load SVG image IMG for use on frame F.  Value is true if
   successful.  */

static bool
svg_load (struct frame *f, struct image *img)
{
  bool success_p = 0;
  Lisp_Object file_name, base_uri;

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  base_uri = image_spec_value (img->spec, QCbase_uri, NULL);
  if (STRINGP (file_name))
    {
      int fd;
      Lisp_Object file = image_find_image_fd (file_name, &fd);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name);
	  return 0;
	}

      /* Read the entire file into memory.  */
      ptrdiff_t size;
      char *contents = slurp_file (fd, &size);
      if (contents == NULL)
	{
	  image_error ("Error loading SVG image `%s'", file);
	  return 0;
	}
      /* If the file was slurped into memory properly, parse it.  */
      if (!STRINGP (base_uri))
        base_uri = file;
      success_p = svg_load_image (f, img, contents, size,
                                  SSDATA (ENCODE_FILE (base_uri)));
      xfree (contents);
    }
  /* Else it's not a file, it's a Lisp object.  Load the image from a
     Lisp object rather than a file.  */
  else
    {
      Lisp_Object data;

      data = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (data))
	{
	  image_error ("Invalid image data `%s'", data);
	  return 0;
	}
      if (!STRINGP (base_uri))
        base_uri = BVAR (current_buffer, filename);
      success_p = svg_load_image (f, img, SSDATA (data), SBYTES (data),
                                  (STRINGP (base_uri) ?
                                   SSDATA (ENCODE_FILE (base_uri)) : NULL));
    }

  return success_p;
}

#if LIBRSVG_CHECK_VERSION (2, 46, 0)
static double
svg_css_length_to_pixels (RsvgLength length, double dpi, int font_size)
{
  double value = length.length;

  switch (length.unit)
    {
    case RSVG_UNIT_PX:
      /* Already a pixel value.  */
      break;
    case RSVG_UNIT_CM:
      /* 2.54 cm in an inch.  */
      value = dpi * value / 2.54;
      break;
    case RSVG_UNIT_MM:
      /* 25.4 mm in an inch.  */
      value = dpi * value / 25.4;
      break;
    case RSVG_UNIT_PT:
      /* 72 points in an inch.  */
      value = dpi * value / 72;
      break;
    case RSVG_UNIT_PC:
      /* 6 picas in an inch.  */
      value = dpi * value / 6;
      break;
    case RSVG_UNIT_IN:
      value *= dpi;
      break;
#if LIBRSVG_CHECK_VERSION (2, 48, 0)
      /* We don't know exactly what font size is used on older librsvg
	 versions.  */
    case RSVG_UNIT_EM:
      value *= font_size;
      break;
#endif
    default:
      /* Probably ex or %.  We can't know what the pixel value is
         without more information.  */
      value = 0;
    }

  return value;
}
#endif

/* Load frame F and image IMG.  CONTENTS contains the SVG XML data to
   be parsed, SIZE is its size, and FILENAME is the name of the SVG
   file being loaded.

   Use librsvg to do most of the image processing.

   Return true when successful.  */
static bool
svg_load_image (struct frame *f, struct image *img, char *contents,
		ptrdiff_t size, char *filename)
{
  RsvgHandle *rsvg_handle;
  double viewbox_width, viewbox_height;
  GError *err = NULL;
  GdkPixbuf *pixbuf;
  int width;
  int height;
  const guint8 *pixels;
  int rowstride;
  char *wrapped_contents = NULL;
  ptrdiff_t wrapped_size;

  bool empty_errmsg = true;
  const char *errmsg = "";
  ptrdiff_t errlen = 0;

#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  char *css = NULL;
#endif

#if ! GLIB_CHECK_VERSION (2, 36, 0)
  /* g_type_init is a glib function that must be called prior to
     using gnome type library functions (obsolete since 2.36.0).  */
  g_type_init ();
#endif

  /* Parse the unmodified SVG data so we can get its initial size.  */

#if LIBRSVG_CHECK_VERSION (2, 32, 0)
  GInputStream *input_stream
    = g_memory_input_stream_new_from_data (contents, size, NULL);
  GFile *base_file = filename ? g_file_new_for_path (filename) : NULL;
  rsvg_handle = rsvg_handle_new_from_stream_sync (input_stream, base_file,
						  RSVG_HANDLE_FLAGS_NONE,
						  NULL, &err);

  if (base_file)
    g_object_unref (base_file);
  g_object_unref (input_stream);

  /* Check rsvg_handle too, to avoid librsvg 2.40.13 bug (Bug#36773#26).  */
  if (!rsvg_handle || err) goto rsvg_error;

  rsvg_handle_set_dpi_x_y (rsvg_handle, FRAME_DISPLAY_INFO (f)->resx,
                           FRAME_DISPLAY_INFO (f)->resy);

#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  Lisp_Object lcss = image_spec_value (img->spec, QCcss, NULL);
  if (!STRINGP (lcss))
    {
      /* Generate the CSS for the SVG image.  */
      /* FIXME: The below calculations leave enough space for a font
	 size up to 9999, if it overflows we just throw an error but
	 should probably increase the buffer size.  */
      const char *css_spec = "svg{font-family:\"%s\";font-size:%dpx}";
      int css_len = strlen (css_spec) + strlen (img->face_font_family) + 1;
      css = xmalloc (css_len);
      if (css_len <= snprintf (css, css_len, css_spec,
			       img->face_font_family, img->face_font_size))
	goto rsvg_error;

      rsvg_handle_set_stylesheet (rsvg_handle, (guint8 *)css, strlen (css), NULL);
    }
  else
    {
      css = xmalloc (SBYTES (lcss) + 1);
      strncpy (css, SSDATA (lcss), SBYTES (lcss));
      *(css + SBYTES (lcss) + 1) = 0;
    }
#endif

#else
  /* Make a handle to a new rsvg object.  */
  rsvg_handle = rsvg_handle_new ();
  eassume (rsvg_handle);

  rsvg_handle_set_dpi_x_y (rsvg_handle, FRAME_DISPLAY_INFO (f)->resx,
                           FRAME_DISPLAY_INFO (f)->resy);

  /* Set base_uri for properly handling referenced images (via 'href').
     Can be explicitly specified using `:base_uri' image property.
     See rsvg bug 596114 - "image refs are relative to curdir, not .svg file"
     <https://gitlab.gnome.org/GNOME/librsvg/issues/33>. */
  if (filename)
    rsvg_handle_set_base_uri (rsvg_handle, filename);

  /* Parse the contents argument and fill in the rsvg_handle.  */
  rsvg_handle_write (rsvg_handle, (unsigned char *) contents, size, &err);
  if (err) goto rsvg_error;

  /* The parsing is complete, rsvg_handle is ready to be used, close
     it for further writes.  */
  rsvg_handle_close (rsvg_handle, &err);
  if (err) goto rsvg_error;
#endif

  /* Get the image dimensions.  */
#if LIBRSVG_CHECK_VERSION (2, 46, 0)
  gdouble gviewbox_width = 0, gviewbox_height = 0;
  gboolean has_viewbox = FALSE;
# if LIBRSVG_CHECK_VERSION (2, 52, 1)
  has_viewbox = rsvg_handle_get_intrinsic_size_in_pixels (rsvg_handle,
							  &gviewbox_width,
							  &gviewbox_height);
# endif

  if (has_viewbox)
    {
      viewbox_width = gviewbox_width;
      viewbox_height = gviewbox_height;
    }
  else
    {
      RsvgRectangle zero_rect, viewbox, out_logical_rect;

      /* Try the intrinsic dimensions first.  */
      gboolean has_width, has_height;
      RsvgLength iwidth, iheight;
      double dpi = FRAME_DISPLAY_INFO (f)->resx;

      rsvg_handle_get_intrinsic_dimensions (rsvg_handle,
					    &has_width, &iwidth,
					    &has_height, &iheight,
					    &has_viewbox, &viewbox);

      if (has_width && has_height)
	{
	  /* Success!  We can use these values directly.  */
	  viewbox_width = svg_css_length_to_pixels (iwidth, dpi,
						    img->face_font_size);
	  viewbox_height = svg_css_length_to_pixels (iheight, dpi,
						     img->face_font_size);

	  /* Here one dimension could be zero because in percent unit.
	     So calculate this dimension with the other.  */
	  if (! (0 < viewbox_width) && (iwidth.unit == RSVG_UNIT_PERCENT))
	    viewbox_width = (viewbox_height * viewbox.width / viewbox.height)
	      * iwidth.length;
	  else if (! (0 < viewbox_height) && (iheight.unit == RSVG_UNIT_PERCENT))
	    viewbox_height = (viewbox_width * viewbox.height / viewbox.width)
	      * iheight.length;
	}
      else if (has_width && has_viewbox)
	{
	  viewbox_width = svg_css_length_to_pixels (iwidth, dpi,
						    img->face_font_size);
	  viewbox_height = viewbox_width * viewbox.height / viewbox.width;
	}
      else if (has_height && has_viewbox)
	{
	  viewbox_height = svg_css_length_to_pixels (iheight, dpi,
						     img->face_font_size);
	  viewbox_width = viewbox_height * viewbox.width / viewbox.height;
	}
      else if (has_viewbox)
	{
	  viewbox_width = viewbox.width;
	  viewbox_height = viewbox.height;
	}
      else
	viewbox_width = viewbox_height = 0;

      if (! (0 < viewbox_width && 0 < viewbox_height))
	{
	  /* We haven't found a usable set of sizes, so try working out
	     the visible area.  */
	  rsvg_handle_get_geometry_for_layer (rsvg_handle, NULL,
					      &zero_rect, &viewbox,
					      &out_logical_rect, NULL);
	  viewbox_width = viewbox.x + viewbox.width;
	  viewbox_height = viewbox.y + viewbox.height;
	}
    }
#else
  /* In librsvg before 2.46.0, guess the viewbox from the image dimensions.  */
  RsvgDimensionData dimension_data;
  rsvg_handle_get_dimensions (rsvg_handle, &dimension_data);
  viewbox_width = dimension_data.width;
  viewbox_height = dimension_data.height;
#endif

#ifdef HAVE_NATIVE_TRANSFORMS
  compute_image_size (viewbox_width, viewbox_height, img,
                      &width, &height);

  width = scale_image_size (width, 1, FRAME_SCALE_FACTOR (f));
  height = scale_image_size (height, 1, FRAME_SCALE_FACTOR (f));
#else
  width = viewbox_width;
  height = viewbox_height;
#endif

  if (! check_image_size (f, width, height))
    {
      image_size_error ();
      goto done_error;
    }

  /* We are now done with the unmodified data.  */
  g_object_unref (rsvg_handle);

  /* Wrap the SVG data in another SVG.  This allows us to set the
     width and height, as well as modify the foreground and background
     colors.  */
  {
    Lisp_Object value;
    unsigned long foreground = img->face_foreground;
    unsigned long background = img->face_background;

    Lisp_Object encoded_contents
      = Fbase64_encode_string (make_unibyte_string (contents, size), Qt);

    /* The wrapper sets the foreground color, width and height, and
       viewBox must contain the dimensions of the original image.  It
       also draws a rectangle over the whole space, set to the
       background color, before including the original image.  This
       acts to set the background color, instead of leaving it
       transparent.  */
    const char *wrapper =
      "<svg xmlns:xlink=\"http://www.w3.org/1999/xlink\" "
      "xmlns:xi=\"http://www.w3.org/2001/XInclude\" "
      "style=\"color: #%06X; fill: currentColor;\" "
      "width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\" "
      "viewBox=\"0 0 %f %f\">"
      "<rect width=\"100%%\" height=\"100%%\" fill=\"#%06X\"/>"
      "<xi:include href=\"data:image/svg+xml;base64,%s\"></xi:include>"
      "</svg>";

    /* FIXME: I've added 64 in the hope it will cover the size of the
       width and height strings and things.  */
    int buffer_size = SBYTES (encoded_contents) + strlen (wrapper) + 64;

    value = image_spec_value (img->spec, QCforeground, NULL);
    if (!NILP (value))
      foreground = image_alloc_image_color (f, img, value, img->face_foreground);
    value = image_spec_value (img->spec, QCbackground, NULL);
    if (!NILP (value))
      {
        background = image_alloc_image_color (f, img, value, img->face_background);
        img->background = background;
        img->background_valid = 1;
      }

    wrapped_contents = xmalloc (buffer_size);

    if (buffer_size <= snprintf (wrapped_contents, buffer_size, wrapper,
				 foreground & 0xFFFFFF, width, height,
				 viewbox_width, viewbox_height,
				 background & 0xFFFFFF,
				 SSDATA (encoded_contents)))
      goto rsvg_error;

    wrapped_size = strlen (wrapped_contents);
  }

  /* Now we parse the wrapped version.  */

#if LIBRSVG_CHECK_VERSION (2, 32, 0)
  input_stream = g_memory_input_stream_new_from_data (wrapped_contents, wrapped_size, NULL);
  base_file = filename ? g_file_new_for_path (filename) : NULL;
  rsvg_handle = rsvg_handle_new_from_stream_sync (input_stream, base_file,
						  RSVG_HANDLE_FLAGS_NONE,
						  NULL, &err);

  if (base_file)
    g_object_unref (base_file);
  g_object_unref (input_stream);

  /* Check rsvg_handle too, to avoid librsvg 2.40.13 bug (Bug#36773#26).  */
  if (!rsvg_handle || err) goto rsvg_error;

  rsvg_handle_set_dpi_x_y (rsvg_handle, FRAME_DISPLAY_INFO (f)->resx,
                           FRAME_DISPLAY_INFO (f)->resy);

#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  rsvg_handle_set_stylesheet (rsvg_handle, (guint8 *)css, strlen (css), NULL);
#endif
#else
  /* Make a handle to a new rsvg object.  */
  rsvg_handle = rsvg_handle_new ();
  eassume (rsvg_handle);

  rsvg_handle_set_dpi_x_y (rsvg_handle, FRAME_DISPLAY_INFO (f)->resx,
                           FRAME_DISPLAY_INFO (f)->resy);

  /* Set base_uri for properly handling referenced images (via 'href').
     Can be explicitly specified using `:base_uri' image property.
     See rsvg bug 596114 - "image refs are relative to curdir, not .svg file"
     <https://gitlab.gnome.org/GNOME/librsvg/issues/33>. */
  if (filename)
    rsvg_handle_set_base_uri (rsvg_handle, filename);

  /* Parse the contents argument and fill in the rsvg_handle.  */
  rsvg_handle_write (rsvg_handle, (unsigned char *) wrapped_contents, wrapped_size, &err);
  if (err) goto rsvg_error;

  /* The parsing is complete, rsvg_handle is ready to used, close it
     for further writes.  */
  rsvg_handle_close (rsvg_handle, &err);
  if (err) goto rsvg_error;
#endif


  /* We can now get a valid pixel buffer from the svg file, if all
     went ok.  */
  pixbuf = rsvg_handle_get_pixbuf (rsvg_handle);
  if (!pixbuf) goto rsvg_error;
  g_object_unref (rsvg_handle);
  xfree (wrapped_contents);

#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  if (!STRINGP (lcss))
    xfree (css);
#endif

  /* Extract some meta data from the svg handle.  */
  width     = gdk_pixbuf_get_width (pixbuf);
  height    = gdk_pixbuf_get_height (pixbuf);
  pixels    = gdk_pixbuf_get_pixels (pixbuf);
  rowstride = gdk_pixbuf_get_rowstride (pixbuf);

  /* Validate the svg meta data.  */
  eassert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
  eassert (gdk_pixbuf_get_n_channels (pixbuf) == 4);
  eassert (gdk_pixbuf_get_has_alpha (pixbuf));
  eassert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);

  {
    /* Try to create a x pixmap to hold the svg pixmap.  */
    Emacs_Pix_Container ximg;
    if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
      {
	g_object_unref (pixbuf);
	return false;
      }

    init_color_table ();

    /* This loop handles opacity values, since Emacs assumes
       non-transparent images.  Each pixel must be "flattened" by
       calculating the resulting color, given the transparency of the
       pixel, and the image background color.  */
    for (int y = 0; y < height; ++y)
      {
	for (int x = 0; x < width; ++x)
	  {
	    int red     = *pixels++;
	    int green   = *pixels++;
	    int blue    = *pixels++;

            /* Skip opacity.  */
	    pixels++;

	    PUT_PIXEL (ximg, x, y, lookup_rgb_color (f, red << 8, green << 8, blue << 8));
	  }

	pixels += rowstride - 4 * width;
      }

#ifdef COLOR_TABLE_SUPPORT
    /* Remember colors allocated for this image.  */
    img->colors = colors_in_color_table (&img->ncolors);
    free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

    g_object_unref (pixbuf);

    img->width  = width;
    img->height = height;

    /* Maybe fill in the background field while we have ximg handy.
       Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

    /* Put ximg into the image.  */
    image_put_x_image (f, img, ximg, 0);
  }

  eassume (err == NULL);
  return true;

 rsvg_error:
  if (err && err->message[0])
    {
      errmsg = err->message;
      errlen = strlen (errmsg);
      /* Remove trailing whitespace from the error message text.  It
	 has a newline at the end, and perhaps more whitespace.  */
      while (errlen && c_isspace (errmsg[errlen - 1]))
	errlen--;
      empty_errmsg = errlen == 0;
    }

  if (empty_errmsg)
    image_error ("Error parsing SVG image");
  else
    image_error ("Error parsing SVG image: %s", make_string (errmsg, errlen));

  if (err)
    g_error_free (err);

 done_error:
  if (rsvg_handle)
    g_object_unref (rsvg_handle);
  if (wrapped_contents)
    xfree (wrapped_contents);
#if LIBRSVG_CHECK_VERSION (2, 48, 0)
  if (css && !STRINGP (lcss))
    xfree (css);
#endif
  return false;
}

#endif	/* defined (HAVE_RSVG) */




/***********************************************************************
				Ghostscript
 ***********************************************************************/

#if defined HAVE_X_WINDOWS && !defined USE_CAIRO
#define HAVE_GHOSTSCRIPT 1
#endif /* HAVE_X_WINDOWS && !USE_CAIRO */

#ifdef HAVE_GHOSTSCRIPT

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

static const struct image_keyword gs_format[GS_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":pt-width",		IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":pt-height",	IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":loader",		IMAGE_FUNCTION_VALUE,			0},
  {":bounding-box",	IMAGE_DONT_CHECK_VALUE_TYPE,		1},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid Ghostscript image
   specification.  */

static bool
gs_image_p (Lisp_Object object)
{
  struct image_keyword fmt[GS_LAST];
  Lisp_Object tem;
  int i;

  memcpy (fmt, gs_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, GS_LAST, Qpostscript))
    return 0;

  /* Bounding box must be a list or vector containing 4 integers.  */
  tem = fmt[GS_BOUNDING_BOX].value;
  if (CONSP (tem))
    {
      for (i = 0; i < 4; ++i, tem = XCDR (tem))
	if (!CONSP (tem) || !FIXNUMP (XCAR (tem)))
	  return 0;
      if (!NILP (tem))
	return 0;
    }
  else if (VECTORP (tem))
    {
      if (ASIZE (tem) != 4)
	return 0;
      for (i = 0; i < 4; ++i)
	if (!FIXNUMP (AREF (tem, i)))
	  return 0;
    }
  else
    return 0;

  return 1;
}


/* Load Ghostscript image IMG for use on frame F.  Value is true
   if successful.  */

static bool
gs_load (struct frame *f, struct image *img)
{
  uintmax_t printnum1, printnum2;
  char buffer[sizeof " " + 2 * INT_STRLEN_BOUND (intmax_t)];
  Lisp_Object window_and_pixmap_id = Qnil, loader, pt_height, pt_width;
  Lisp_Object frame;
  double in_width, in_height;
  Lisp_Object pixel_colors = Qnil;

  /* Compute pixel size of pixmap needed from the given size in the
     image specification.  Sizes in the specification are in pt.  1 pt
     = 1/72 in, xdpi and ydpi are stored in the frame's X display
     info.  */
  pt_width = image_spec_value (img->spec, QCpt_width, NULL);
  in_width = FIXNUMP (pt_width) ? XFIXNAT (pt_width) / 72.0 : 0;
  in_width *= FRAME_RES_X (f);
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = FIXNUMP (pt_height) ? XFIXNAT (pt_height) / 72.0 : 0;
  in_height *= FRAME_RES_Y (f);

  if (! (in_width <= INT_MAX && in_height <= INT_MAX
	 && check_image_size (f, in_width, in_height)))
    {
      image_size_error ();
      return 0;
    }
  img->width = in_width;
  img->height = in_height;

  /* Create the pixmap.  */
  eassert (img->pixmap == NO_PIXMAP);

  if (image_check_image_size (0, img->width, img->height))
    {
      /* Only W32 version did BLOCK_INPUT here.  ++kfs */
      block_input ();
      img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				   img->width, img->height,
				   FRAME_DISPLAY_INFO (f)->n_planes);
      unblock_input ();
    }

  if (!img->pixmap)
    {
      image_error ("Unable to create pixmap for `%s'" , img->spec);
      return 0;
    }

  /* Call the loader to fill the pixmap.  It returns a process object
     if successful.  We do not record_unwind_protect here because
     other places in redisplay like calling window scroll functions
     don't either.  Let the Lisp loader use `unwind-protect' instead.  */
  printnum1 = FRAME_X_DRAWABLE (f);
  printnum2 = img->pixmap;
  window_and_pixmap_id
    = make_formatted_string (buffer, "%"PRIuMAX" %"PRIuMAX,
			     printnum1, printnum2);

  printnum1 = FRAME_FOREGROUND_PIXEL (f);
  printnum2 = FRAME_BACKGROUND_PIXEL (f);
  pixel_colors
    = make_formatted_string (buffer, "%"PRIuMAX" %"PRIuMAX,
			     printnum1, printnum2);

  XSETFRAME (frame, f);
  loader = image_spec_value (img->spec, QCloader, NULL);
  if (NILP (loader))
    loader = intern ("gs-load-image");

  img->lisp_data = call6 (loader, frame, img->spec,
			  make_fixnum (img->width),
			  make_fixnum (img->height),
			  window_and_pixmap_id,
			  pixel_colors);
  return PROCESSP (img->lisp_data);
}


/* Kill the Ghostscript process that was started to fill PIXMAP on
   frame F.  Called from XTread_socket when receiving an event
   telling Emacs that Ghostscript has finished drawing.  */

void
x_kill_gs_process (Pixmap pixmap, struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  ptrdiff_t i;
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
  eassert (PROCESSP (img->lisp_data));
  Fkill_process (img->lisp_data, Qnil);
  img->lisp_data = Qnil;

#if defined (HAVE_X_WINDOWS)

  /* On displays with a mutable colormap, figure out the colors
     allocated for the image by looking at the pixels of an XImage for
     img->pixmap.  */
  if (x_mutable_colormap (FRAME_X_VISUAL_INFO (f)))
    {
      XImage *ximg;

      block_input ();

      /* Try to get an XImage for img->pixmep.  */
      ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			0, 0, img->width, img->height, ~0, ZPixmap);
      if (ximg)
	{
	  /* Initialize the color table.  */
	  init_color_table ();

	  /* For each pixel of the image, look its color up in the
	     color table.  After having done so, the color table will
	     contain an entry for each color used by the image.  */
#ifdef COLOR_TABLE_SUPPORT
	  for (int y = 0; y < img->height; ++y)
	    for (int x = 0; x < img->width; ++x)
	      {
		unsigned long pixel = XGetPixel (ximg, x, y);

		lookup_pixel_color (f, pixel);
	      }

	  /* Record colors in the image.  Free color table and XImage.  */
	  img->colors = colors_in_color_table (&img->ncolors);
	  free_color_table ();
#endif
	  XDestroyImage (ximg);
	}
      else
	image_error ("Cannot get X image of `%s'; colors will not be freed",
		     img->spec);

      unblock_input ();
    }
#endif /* HAVE_X_WINDOWS */

  /* Now that we have the pixmap, compute mask and transform the
     image if requested.  */
  block_input ();
  postprocess_image (f, img);
  unblock_input ();
}

#endif /* HAVE_GHOSTSCRIPT */


/***********************************************************************
				Tests
 ***********************************************************************/
DEFUN ("imagep", Fimagep, Simagep, 1, 1, 0,
       doc: /* Value is non-nil if SPEC is a valid image specification.  */)
  (Lisp_Object spec)
{
  return valid_image_p (spec) ? Qt : Qnil;
}

#ifdef GLYPH_DEBUG

DEFUN ("lookup-image", Flookup_image, Slookup_image, 1, 1, 0,
       doc: /* */)
  (Lisp_Object spec)
{
  ptrdiff_t id = -1;

  if (valid_image_p (spec))
    id = lookup_image (SELECTED_FRAME (), spec, -1);

  debug_print (spec);
  return make_fixnum (id);
}

#endif /* GLYPH_DEBUG */


/***********************************************************************
			    Initialization
 ***********************************************************************/

DEFUN ("image-transforms-p", Fimage_transforms_p, Simage_transforms_p, 0, 1, 0,
       doc: /* Test whether FRAME supports image transformation.
Return list of capabilities if FRAME supports native transforms, nil otherwise.
FRAME defaults to the selected frame.
The list of capabilities can include one or more of the following:

 - the symbol `scale' if FRAME supports image scaling
 - the symbol `rotate90' if FRAME supports image rotation only by angles
    that are integral multiples of 90 degrees.  */)
     (Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);
  if (FRAME_WINDOW_P (f))
    {
#ifdef HAVE_NATIVE_TRANSFORMS
# if defined HAVE_IMAGEMAGICK || defined (USE_CAIRO) || defined (HAVE_NS) \
  || defined (HAVE_HAIKU)
      return list2 (Qscale, Qrotate90);
# elif defined (HAVE_X_WINDOWS) && defined (HAVE_XRENDER)
      if (FRAME_DISPLAY_INFO (f)->xrender_supported_p)
	return list2 (Qscale, Qrotate90);
# elif defined (HAVE_NTGUI)
      return (w32_image_rotations_p ()
	      ? list2 (Qscale, Qrotate90)
	      : list1 (Qscale));
# endif
#endif
    }

  return Qnil;
}

DEFUN ("image-cache-size", Fimage_cache_size, Simage_cache_size, 0, 0, 0,
       doc: /* Return the size of the image cache.  */)
  (void)
{
  Lisp_Object tail, frame;
  size_t total = 0;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_WINDOW_P (XFRAME (frame)))
      total += image_frame_cache_size (XFRAME (frame));

#if defined (HAVE_WEBP) || defined (HAVE_GIF)
  struct anim_cache *pcache = anim_cache;
  while (pcache)
    {
      total += pcache->byte_size;
      pcache = pcache->next;
    }
#endif

  return make_int (total);
}


DEFUN ("init-image-library", Finit_image_library, Sinit_image_library, 1, 1, 0,
       doc: /* Initialize image library implementing image type TYPE.
Return t if TYPE is a supported image type.

If image libraries are loaded dynamically (currently the case only on
MS-Windows), load the library for TYPE if it is not yet loaded, using
the library file(s) specified by `dynamic-library-alist'.  */)
  (Lisp_Object type)
{
  return lookup_image_type (type) ? Qt : Qnil;
}

static bool
initialize_image_type (struct image_type const *type)
{
#ifdef WINDOWSNT
  Lisp_Object typesym = builtin_lisp_symbol (type->type);

# if HAVE_NATIVE_IMAGE_API
  if (image_can_use_native_api (typesym))
    return true;
# endif

  Lisp_Object tested = Fassq (typesym, Vlibrary_cache);
  /* If we failed to load the library before, don't try again.  */
  if (CONSP (tested))
    return !NILP (XCDR (tested)) ? true : false;

  bool (*init) (void) = type->init;
  if (init)
    {
      bool type_valid = init ();
      Vlibrary_cache = Fcons (Fcons (typesym, type_valid ? Qt : Qnil),
			      Vlibrary_cache);
      return type_valid;
    }
#endif
  return true;
}

/* Array of supported image types.  */

static struct image_type const image_types[] =
{
#ifdef HAVE_GHOSTSCRIPT
 { SYMBOL_INDEX (Qpostscript), gs_image_p, gs_load, image_clear_image },
#endif
#ifdef HAVE_IMAGEMAGICK
 { SYMBOL_INDEX (Qimagemagick), imagemagick_image_p, imagemagick_load,
   imagemagick_clear_image },
#endif
#ifdef HAVE_RSVG
 { SYMBOL_INDEX (Qsvg), svg_image_p, svg_load, image_clear_image,
   IMAGE_TYPE_INIT (init_svg_functions) },
#endif
#if defined HAVE_PNG
 { SYMBOL_INDEX (Qpng), png_image_p, png_load, image_clear_image,
   IMAGE_TYPE_INIT (init_png_functions) },
#endif
#if defined HAVE_GIF
 { SYMBOL_INDEX (Qgif), gif_image_p, gif_load, gif_clear_image,
   IMAGE_TYPE_INIT (init_gif_functions) },
#endif
#if defined HAVE_TIFF
 { SYMBOL_INDEX (Qtiff), tiff_image_p, tiff_load, image_clear_image,
   IMAGE_TYPE_INIT (init_tiff_functions) },
#endif
#if defined HAVE_JPEG
 { SYMBOL_INDEX (Qjpeg), jpeg_image_p, jpeg_load, image_clear_image,
   IMAGE_TYPE_INIT (init_jpeg_functions) },
#endif
#if defined HAVE_XPM || defined HAVE_NS || defined HAVE_HAIKU || defined HAVE_PGTK
 { SYMBOL_INDEX (Qxpm), xpm_image_p, xpm_load, image_clear_image,
   IMAGE_TYPE_INIT (init_xpm_functions) },
#endif
#if defined HAVE_WEBP
 { SYMBOL_INDEX (Qwebp), webp_image_p, webp_load, image_clear_image,
   IMAGE_TYPE_INIT (init_webp_functions) },
#endif
 { SYMBOL_INDEX (Qxbm), xbm_image_p, xbm_load, image_clear_image },
 { SYMBOL_INDEX (Qpbm), pbm_image_p, pbm_load, image_clear_image },
};

#if HAVE_NATIVE_IMAGE_API
struct image_type native_image_type =
  { SYMBOL_INDEX (Qnative_image), native_image_p, native_image_load,
    image_clear_image };
#endif

/* Look up image type TYPE, and return a pointer to its image_type
   structure.  Return 0 if TYPE is not a known image type.  */

static struct image_type const *
lookup_image_type (Lisp_Object type)
{
#if HAVE_NATIVE_IMAGE_API
  if (image_can_use_native_api (type))
    return &native_image_type;
#endif

  for (int i = 0; i < ARRAYELTS (image_types); i++)
    {
      struct image_type const *r = &image_types[i];
      if (EQ (type, builtin_lisp_symbol (r->type)))
	return initialize_image_type (r) ? r : NULL;
    }
  return NULL;
}

/* Prune the animation caches.  If CLEAR, remove all animation cache
   entries.  */
void
image_prune_animation_caches (bool clear)
{
#if defined (HAVE_WEBP) || defined (HAVE_GIF)
  anim_prune_animation_cache (clear? Qt: Qnil);
#endif
#ifdef HAVE_IMAGEMAGICK
  imagemagick_prune_animation_cache (clear);
#endif
}

void
syms_of_image (void)
{
  /* Must be defined now because we're going to update it below, while
     defining the supported image types.  */
  DEFVAR_LISP ("image-types", Vimage_types,
    doc: /* List of potentially supported image types.
Each element of the list is a symbol for an image type, like `jpeg' or `png'.
To check whether it is really supported, use `image-type-available-p'.  */);
  Vimage_types = Qnil;

  DEFVAR_LISP ("max-image-size", Vmax_image_size,
    doc: /* Maximum size of images.
Emacs will not load an image into memory if its pixel width or
pixel height exceeds this limit.

If the value is an integer, it directly specifies the maximum
image height and width, measured in pixels.  If it is a floating
point number, it specifies the maximum image height and width
as a ratio to the frame height and width.  If the value is
non-numeric, there is no explicit limit on the size of images.  */);
  Vmax_image_size = make_float (MAX_IMAGE_SIZE);

  /* Other symbols.  */
  DEFSYM (Qcount, "count");
  DEFSYM (Qextension_data, "extension-data");
  DEFSYM (Qdelay, "delay");

  /* Keywords.  */
  DEFSYM (QCascent, ":ascent");
  DEFSYM (QCmargin, ":margin");
  DEFSYM (QCrelief, ":relief");
  DEFSYM (QCconversion, ":conversion");
  DEFSYM (QCcolor_symbols, ":color-symbols");
  DEFSYM (QCheuristic_mask, ":heuristic-mask");
  DEFSYM (QCindex, ":index");
  DEFSYM (QCcrop, ":crop");
  DEFSYM (QCrotation, ":rotation");
  DEFSYM (QCmatrix, ":matrix");
  DEFSYM (QCscale, ":scale");
  DEFSYM (QCtransform_smoothing, ":transform-smoothing");
  DEFSYM (QCcolor_adjustment, ":color-adjustment");
  DEFSYM (QCmask, ":mask");
  DEFSYM (QCflip, ":flip");

  /* Other symbols.  */
  DEFSYM (Qlaplace, "laplace");
  DEFSYM (Qemboss, "emboss");
  DEFSYM (Qedge_detection, "edge-detection");
  DEFSYM (Qheuristic, "heuristic");

  DEFSYM (Qpostscript, "postscript");
  DEFSYM (QCmax_width, ":max-width");
  DEFSYM (QCmax_height, ":max-height");

  DEFSYM (Qem, "em");

#ifdef HAVE_NATIVE_TRANSFORMS
  DEFSYM (Qscale, "scale");
  DEFSYM (Qrotate, "rotate");
  DEFSYM (Qrotate90, "rotate90");
  DEFSYM (Qcrop, "crop");
#endif

#ifdef HAVE_GHOSTSCRIPT
  add_image_type (Qpostscript);
  DEFSYM (QCloader, ":loader");
  DEFSYM (QCpt_width, ":pt-width");
  DEFSYM (QCpt_height, ":pt-height");
#endif /* HAVE_GHOSTSCRIPT */

#ifdef HAVE_NTGUI
  /* Versions of libpng, libgif, and libjpeg that we were compiled with,
     or -1 if no PNG/GIF support was compiled in.  This is tested by
     w32-win.el to correctly set up the alist used to search for the
     respective image libraries.  */
  DEFSYM (Qlibpng_version, "libpng-version");
  Fset (Qlibpng_version,
#if HAVE_PNG
	make_fixnum (PNG_LIBPNG_VER)
#else
	make_fixnum (-1)
#endif
	);
  DEFSYM (Qlibgif_version, "libgif-version");
  Fset (Qlibgif_version,
#ifdef HAVE_GIF
	make_fixnum (GIFLIB_MAJOR * 10000
		     + GIFLIB_MINOR * 100
		     + GIFLIB_RELEASE)
#else
	make_fixnum (-1)
#endif
        );
  DEFSYM (Qlibjpeg_version, "libjpeg-version");
  Fset (Qlibjpeg_version,
#if HAVE_JPEG
	make_fixnum (JPEG_LIB_VERSION)
#else
	make_fixnum (-1)
#endif
	);
#endif

  DEFSYM (Qpbm, "pbm");
  add_image_type (Qpbm);

  DEFSYM (Qxbm, "xbm");
  add_image_type (Qxbm);

#if defined (HAVE_XPM) || defined (HAVE_NS) \
  || defined (HAVE_HAIKU) || defined (HAVE_PGTK)
  DEFSYM (Qxpm, "xpm");
  add_image_type (Qxpm);
#endif

#if defined (HAVE_JPEG) || defined (HAVE_NATIVE_IMAGE_API)
  DEFSYM (Qjpeg, "jpeg");
  add_image_type (Qjpeg);
#endif

#if defined (HAVE_TIFF) || defined (HAVE_NATIVE_IMAGE_API)
  DEFSYM (Qtiff, "tiff");
  add_image_type (Qtiff);
#endif

#if defined (HAVE_GIF) || defined (HAVE_NATIVE_IMAGE_API)
  DEFSYM (Qgif, "gif");
  add_image_type (Qgif);
#endif

#if defined (HAVE_PNG) || defined (HAVE_NATIVE_IMAGE_API)
  DEFSYM (Qpng, "png");
  add_image_type (Qpng);
#endif

#if defined (HAVE_WEBP) || (defined (HAVE_NATIVE_IMAGE_API) \
			    && defined (HAVE_HAIKU))
  DEFSYM (Qwebp, "webp");
  DEFSYM (Qwebpdemux, "webpdemux");
  add_image_type (Qwebp);
#endif

#if defined (HAVE_IMAGEMAGICK)
  DEFSYM (Qimagemagick, "imagemagick");
  add_image_type (Qimagemagick);
#endif

#if defined (HAVE_RSVG)
  DEFSYM (Qsvg, "svg");
  DEFSYM (QCbase_uri, ":base-uri");
  DEFSYM (QCcss, ":css");
  add_image_type (Qsvg);
#ifdef HAVE_NTGUI
  /* Other libraries used directly by svg code.  */
  DEFSYM (Qgdk_pixbuf, "gdk-pixbuf");
  DEFSYM (Qglib, "glib");
# if LIBRSVG_CHECK_VERSION (2, 32, 0)
  DEFSYM (Qgio,  "gio");
# endif
  DEFSYM (Qgobject, "gobject");
#endif /* HAVE_NTGUI  */
#elif defined HAVE_NATIVE_IMAGE_API			\
  && ((defined HAVE_NS && defined NS_IMPL_COCOA)	\
      || defined HAVE_HAIKU)
  DEFSYM (Qsvg, "svg");

  /* On Haiku, the SVG translator may not be installed.  */
  if (image_can_use_native_api (Qsvg))
    add_image_type (Qsvg);
#endif

#ifdef HAVE_NS
  DEFSYM (Qheic, "heic");
  add_image_type (Qheic);
#endif

#if HAVE_NATIVE_IMAGE_API
  DEFSYM (Qnative_image, "native-image");

# if defined HAVE_NTGUI || defined HAVE_HAIKU
  DEFSYM (Qbmp, "bmp");
  add_image_type (Qbmp);
# endif

# ifdef HAVE_NTGUI
  DEFSYM (Qgdiplus, "gdiplus");
  DEFSYM (Qshlwapi, "shlwapi");
# endif
#endif

  defsubr (&Sinit_image_library);
#ifdef HAVE_IMAGEMAGICK
  defsubr (&Simagemagick_types);
#endif
  defsubr (&Sclear_image_cache);
  defsubr (&Simage_flush);
  defsubr (&Simage_size);
  defsubr (&Simage_mask_p);
  defsubr (&Simage_metadata);
  defsubr (&Simage_cache_size);
  defsubr (&Simagep);

#ifdef GLYPH_DEBUG
  defsubr (&Slookup_image);
#endif

  DEFSYM (QCanimate_buffer, ":animate-buffer");
  DEFSYM (QCanimate_tardiness, ":animate-tardiness");
  DEFSYM (QCanimate_position, ":animate-position");
  DEFSYM (QCanimate_multi_frame_data, ":animate-multi-frame-data");

  defsubr (&Simage_transforms_p);

  DEFVAR_BOOL ("cross-disabled-images", cross_disabled_images,
    doc: /* Non-nil means always draw a cross over disabled images.
Disabled images are those having a `:conversion disabled' property.
A cross is always drawn on black & white displays.  */);
  cross_disabled_images = 0;

  DEFVAR_LISP ("x-bitmap-file-path", Vx_bitmap_file_path,
    doc: /* List of directories to search for window system bitmap files.  */);
  Vx_bitmap_file_path = decode_env_path (0, PATH_BITMAPS, 0);

  DEFVAR_LISP ("image-cache-eviction-delay", Vimage_cache_eviction_delay,
    doc: /* Maximum time after which images are removed from the cache.
When an image has not been displayed this many seconds, Emacs
automatically removes it from the image cache.  If the cache contains
a large number of images, the actual eviction time may be shorter.
The value can also be nil, meaning the cache is never cleared.

The function `clear-image-cache' disregards this variable.  */);
  Vimage_cache_eviction_delay = make_fixnum (300);
#ifdef HAVE_IMAGEMAGICK
  DEFVAR_INT ("imagemagick-render-type", imagemagick_render_type,
    doc: /* Integer indicating which ImageMagick rendering method to use.
The options are:
  0 -- the default method (pixel pushing)
  1 -- a newer method ("MagickExportImagePixels") that may perform
       better (speed etc) in some cases, but has not been as thoroughly
       tested with Emacs as the default method.  This method requires
       ImageMagick version 6.4.6 (approximately) or later.
*/);
  /* MagickExportImagePixels is in 6.4.6-9, but not 6.4.4-10.  */
  imagemagick_render_type = 0;
#endif
}
