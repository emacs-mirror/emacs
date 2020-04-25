/* Implementation of MS-Windows native image API via the GDI+ library.

Copyright (C) 2020 Free Software Foundation, Inc.

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

/* Written by Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>.  */

#include <config.h>
#include "lisp.h"
#include "dispextern.h"
#define COBJMACROS
#ifdef MINGW_W64
/* FIXME: Do we need to include objidl.h?  */
#include <objidl.h>
#endif
#include <wtypes.h>
#include <gdiplus.h>
#include <shlwapi.h>
#include "w32common.h"
#include "w32term.h"
#ifdef WINDOWSNT
#include "w32.h"	/* for map_w32_filename, filename_to_utf16 */
#endif
#include "frame.h"
#include "coding.h"

#ifdef WINDOWSNT

typedef GpStatus (WINGDIPAPI *GdiplusStartup_Proc)
  (ULONG_PTR *, GdiplusStartupInput *, GdiplusStartupOutput *);
typedef VOID (WINGDIPAPI *GdiplusShutdown_Proc) (ULONG_PTR);
typedef GpStatus (WINGDIPAPI *GdipGetPropertyItemSize_Proc)
  (GpImage *, PROPID, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetPropertyItem_Proc)
  (GpImage *, PROPID, UINT, PropertyItem *);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameDimensionsCount_Proc)
  (GpImage *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameDimensionsList_Proc)
  (GpImage *, GUID *, UINT);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameCount_Proc)
  (GpImage *, GDIPCONST GUID *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipImageSelectActiveFrame_Proc)
  (GpImage*, GDIPCONST GUID *, UINT);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromFile_Proc)
  (WCHAR *, GpBitmap **);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromStream_Proc)
  (IStream *, GpBitmap **);
typedef IStream * (WINAPI *SHCreateMemStream_Proc) (const BYTE *, UINT);
typedef GpStatus (WINGDIPAPI *GdipCreateHBITMAPFromBitmap_Proc)
  (GpBitmap *, HBITMAP *, ARGB);
typedef GpStatus (WINGDIPAPI *GdipDisposeImage_Proc) (GpImage *);
typedef GpStatus (WINGDIPAPI *GdipGetImageHeight_Proc) (GpImage *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetImageWidth_Proc) (GpImage *, UINT *);

GdiplusStartup_Proc fn_GdiplusStartup;
GdiplusShutdown_Proc fn_GdiplusShutdown;
GdipGetPropertyItemSize_Proc fn_GdipGetPropertyItemSize;
GdipGetPropertyItem_Proc fn_GdipGetPropertyItem;
GdipImageGetFrameDimensionsCount_Proc fn_GdipImageGetFrameDimensionsCount;
GdipImageGetFrameDimensionsList_Proc fn_GdipImageGetFrameDimensionsList;
GdipImageGetFrameCount_Proc fn_GdipImageGetFrameCount;
GdipImageSelectActiveFrame_Proc fn_GdipImageSelectActiveFrame;
GdipCreateBitmapFromFile_Proc fn_GdipCreateBitmapFromFile;
GdipCreateBitmapFromStream_Proc fn_GdipCreateBitmapFromStream;
SHCreateMemStream_Proc fn_SHCreateMemStream;
GdipCreateHBITMAPFromBitmap_Proc fn_GdipCreateHBITMAPFromBitmap;
GdipDisposeImage_Proc fn_GdipDisposeImage;
GdipGetImageHeight_Proc fn_GdipGetImageHeight;
GdipGetImageWidth_Proc fn_GdipGetImageWidth;

static bool
gdiplus_init (void)
{
  HANDLE gdiplus_lib, shlwapi_lib;

  if (!((gdiplus_lib = w32_delayed_load (Qgdiplus))
	&& (shlwapi_lib = w32_delayed_load (Qshlwapi))))
    return false;

  fn_GdiplusStartup = (GdiplusStartup_Proc)
    get_proc_addr (gdiplus_lib, "GdiplusStartup");
  if (!fn_GdiplusStartup)
    return false;
  fn_GdiplusShutdown = (GdiplusShutdown_Proc)
    get_proc_addr (gdiplus_lib, "GdiplusShutdown");
  if (!fn_GdiplusShutdown)
    return false;
  fn_GdipGetPropertyItemSize = (GdipGetPropertyItemSize_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetPropertyItemSize");
  if (!fn_GdipGetPropertyItemSize)
    return false;
  fn_GdipGetPropertyItem = (GdipGetPropertyItem_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetPropertyItem");
  if (!fn_GdipGetPropertyItem)
    return false;
  fn_GdipImageGetFrameDimensionsCount = (GdipImageGetFrameDimensionsCount_Proc)
    get_proc_addr (gdiplus_lib, "GdipImageGetFrameDimensionsCount");
  if (!fn_GdipImageGetFrameDimensionsCount)
    return false;
  fn_GdipImageGetFrameDimensionsList = (GdipImageGetFrameDimensionsList_Proc)
    get_proc_addr (gdiplus_lib, "GdipImageGetFrameDimensionsList");
  if (!fn_GdipImageGetFrameDimensionsList)
    return false;
  fn_GdipImageGetFrameCount = (GdipImageGetFrameCount_Proc)
    get_proc_addr (gdiplus_lib, "GdipImageGetFrameCount");
  if (!fn_GdipImageGetFrameCount)
    return false;
  fn_GdipImageSelectActiveFrame = (GdipImageSelectActiveFrame_Proc)
    get_proc_addr (gdiplus_lib, "GdipImageSelectActiveFrame");
  if (!fn_GdipImageSelectActiveFrame)
    return false;
  fn_GdipCreateBitmapFromFile = (GdipCreateBitmapFromFile_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateBitmapFromFile");
  if (!fn_GdipCreateBitmapFromFile)
    return false;
  fn_GdipCreateBitmapFromStream = (GdipCreateBitmapFromStream_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateBitmapFromStream");
  if (!fn_GdipCreateBitmapFromStream)
    return false;
  fn_GdipCreateHBITMAPFromBitmap = (GdipCreateHBITMAPFromBitmap_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateHBITMAPFromBitmap");
  if (!fn_GdipCreateHBITMAPFromBitmap)
    return false;
  fn_GdipDisposeImage = (GdipDisposeImage_Proc)
    get_proc_addr (gdiplus_lib, "GdipDisposeImage");
  if (!fn_GdipDisposeImage)
    return false;
  fn_GdipGetImageHeight = (GdipGetImageHeight_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetImageHeight");
  if (!fn_GdipGetImageHeight)
    return false;
  fn_GdipGetImageWidth = (GdipGetImageWidth_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetImageWidth");
  if (!fn_GdipGetImageWidth)
    return false;
  /* LOAD_DLL_FN (shlwapi_lib, SHCreateMemStream); */

  /* The following terrible kludge is required to use native image API
     on Windows before Vista, because SHCreateMemStream was not
     exported by name in those versions, only by ordinal number.  */
  fn_SHCreateMemStream = (SHCreateMemStream_Proc)
    get_proc_addr (shlwapi_lib, "SHCreateMemStream");
  if (!fn_SHCreateMemStream)
    {
      fn_SHCreateMemStream = (SHCreateMemStream_Proc)
	get_proc_addr (shlwapi_lib, MAKEINTRESOURCEA (12));
      if (!fn_SHCreateMemStream)
	return false;
    }

  return true;
}

# undef GdiplusStartup
# undef GdiplusShutdown
# undef GdipGetPropertyItemSize
# undef GdipGetPropertyItem
# undef GdipImageGetFrameDimensionsCount
# undef GdipImageGetFrameDimensionsList
# undef GdipImageGetFrameCount
# undef GdipImageSelectActiveFrame
# undef GdipCreateBitmapFromFile
# undef GdipCreateBitmapFromStream
# undef SHCreateMemStream
# undef GdipCreateHBITMAPFromBitmap
# undef GdipDisposeImage
# undef GdipGetImageHeight
# undef GdipGetImageWidth

# define GdiplusStartup fn_GdiplusStartup
# define GdiplusShutdown fn_GdiplusShutdown
# define GdipGetPropertyItemSize fn_GdipGetPropertyItemSize
# define GdipGetPropertyItem fn_GdipGetPropertyItem
# define GdipImageGetFrameDimensionsCount fn_GdipImageGetFrameDimensionsCount
# define GdipImageGetFrameDimensionsList fn_GdipImageGetFrameDimensionsList
# define GdipImageGetFrameCount fn_GdipImageGetFrameCount
# define GdipImageSelectActiveFrame fn_GdipImageSelectActiveFrame
# define GdipCreateBitmapFromFile fn_GdipCreateBitmapFromFile
# define GdipCreateBitmapFromStream fn_GdipCreateBitmapFromStream
# define SHCreateMemStream fn_SHCreateMemStream
# define GdipCreateHBITMAPFromBitmap fn_GdipCreateHBITMAPFromBitmap
# define GdipDisposeImage fn_GdipDisposeImage
# define GdipGetImageHeight fn_GdipGetImageHeight
# define GdipGetImageWidth fn_GdipGetImageWidth

#endif	/* WINDOWSNT */

static int gdip_initialized;
static bool gdiplus_started;
static ULONG_PTR token;
static GdiplusStartupInput input;
static GdiplusStartupOutput output;


/* Initialize GDI+, return true if successful.  */
static bool
gdiplus_startup (void)
{
  GpStatus status;

  if (gdiplus_started)
    return true;
#ifdef WINDOWSNT
  if (!gdip_initialized)
    gdip_initialized = gdiplus_init () ? 1 : -1;
#else
  gdip_initialized = 1;
#endif
  if (gdip_initialized > 0)
    {
      input.GdiplusVersion = 1;
      input.DebugEventCallback = NULL;
      input.SuppressBackgroundThread = FALSE;
      input.SuppressExternalCodecs = FALSE;

      status = GdiplusStartup (&token, &input, &output);
      if (status == Ok)
	gdiplus_started = true;
      return (status == Ok);
    }
  return false;
}

/* This is called from term_ntproc.  */
void
w32_gdiplus_shutdown (void)
{
  if (gdiplus_started)
    GdiplusShutdown (token);
  gdiplus_started = false;
}

bool
w32_can_use_native_image_api (Lisp_Object type)
{
  if (!w32_use_native_image_api)
    return false;
  if (!(EQ (type, Qjpeg)
	|| EQ (type, Qpng)
	|| EQ (type, Qgif)
	|| EQ (type, Qtiff)
	|| EQ (type, Qnative_image)))
    {
      /* GDI+ can also display BMP, Exif, ICON, WMF, and EMF images.
	 But we don't yet support these in image.c.  */
      return false;
    }
  return gdiplus_startup ();
}

enum PropertyItem_type {
  PI_BYTE = 1,
  PI_ASCIIZ = 2,
  PI_USHORT = 3,
  PI_ULONG = 4,
  PI_ULONG_PAIR = 5,
  PI_BYTE_ANY = 6,
  PI_LONG = 7,
  PI_LONG_PAIR = 10
};

static double
decode_delay (PropertyItem *propertyItem, int frame)
{
  enum PropertyItem_type type = propertyItem[0].type;
  unsigned long udelay;
  double retval;

  switch (type)
    {
    case PI_BYTE:
    case PI_BYTE_ANY:
      udelay = ((unsigned char *)propertyItem[0].value)[frame];
      retval = udelay;
      break;
    case PI_USHORT:
      udelay = ((unsigned short *)propertyItem[0].value)[frame];
      retval = udelay;
      break;
    case PI_ULONG:
    case PI_LONG:	/* delay should always be positive */
      udelay = ((unsigned long *)propertyItem[0].value)[frame];
      retval = udelay;
      break;
    default:
      /* This negative value will cause the caller to disregard the
	 delay if we cannot determine it reliably.  */
      add_to_log ("Invalid or unknown propertyItem type in w32image.c");
      retval = -1.0;
    }

  return retval;
}

static double
w32_frame_delay (GpBitmap *pBitmap, int frame)
{
  UINT size;
  PropertyItem *propertyItem;
  double delay = -1.0;

  /* Assume that the image has a property item of type PropertyItemEquipMake.
     Get the size of that property item.  This can fail for multi-frame TIFF
     images.  */
  GpStatus status = GdipGetPropertyItemSize (pBitmap, PropertyTagFrameDelay,
					     &size);

  if (status == Ok)
    {
      /* Allocate a buffer to receive the property item.  */
      propertyItem = malloc (size);
      if (propertyItem != NULL)
	{
	  /* Get the property item.  */
	  GdipGetPropertyItem (pBitmap, PropertyTagFrameDelay, size,
			       propertyItem);
	  delay = decode_delay (propertyItem, frame);
	  if (delay <= 0)
	    {
	      /* In GIF files, unfortunately, delay is only specified
		 for the first frame.  */
	      delay = decode_delay (propertyItem, 0);
	    }
	  delay /= 100.0;
	  free (propertyItem);
	}
    }
  return delay;
}

static GpStatus
w32_select_active_frame (GpBitmap *pBitmap, int frame, int *nframes,
			 double *delay)
{
  UINT count, frameCount;
  GUID pDimensionIDs[1];
  GpStatus status = Ok;

  status = GdipImageGetFrameDimensionsCount (pBitmap, &count);
  frameCount = *nframes = 0;
  *delay = -1.0;
  if (count)
    {
      /* The following call will fill pDimensionIDs[0] with the
	 FrameDimensionTime GUID for GIF images, and
	 FrameDimensionPage GUID for other image types.  Multi-page
	 GIF and TIFF images expect these values in the
	 GdipImageSelectActiveFrame call below.  */
      status = GdipImageGetFrameDimensionsList (pBitmap, pDimensionIDs, 1);
      status = GdipImageGetFrameCount (pBitmap, &pDimensionIDs[0], &frameCount);
      if (status == Ok && frameCount > 1)
        {
          if (frame < 0 || frame >= frameCount)
	    status = GenericError;
          else
            {
              status = GdipImageSelectActiveFrame (pBitmap, &pDimensionIDs[0],
						   frame);
              *delay = w32_frame_delay (pBitmap, frame);
              *nframes = frameCount;
            }
        }
    }
  return status;
}

static ARGB
w32_image_bg_color (struct frame *f, struct image *img)
{
  Lisp_Object specified_bg = Fplist_get (XCDR (img->spec), QCbackground);
  Emacs_Color color;

  /* If the user specified a color, try to use it; if not, use the
     current frame background, ignoring any default background
     color set by the image.  */
  if (STRINGP (specified_bg)
      ? w32_defined_color (f, SSDATA (specified_bg), &color, false, false)
      : (w32_query_frame_background_color (f, &color), true))
    /* The user specified ':background', use that.  */
    {
      DWORD red = (((DWORD) color.red) & 0xff00) << 8;
      DWORD green = ((DWORD) color.green) & 0xff00;
      DWORD blue = ((DWORD) color.blue) >> 8;
      return (ARGB) (red | green | blue);
    }
  return (ARGB) 0xff000000;
}

int
w32_load_image (struct frame *f, struct image *img,
                Lisp_Object spec_file, Lisp_Object spec_data)
{
  GpStatus status = GenericError;
  GpBitmap *pBitmap;
  Lisp_Object metadata;

  eassert (valid_image_p (img->spec));

  /* This function only gets called if w32_gdiplus_startup was invoked
     and succeeded.  We have a valid token and GDI+ is active.  */
  if (STRINGP (spec_file))
    {
      const char *fn = map_w32_filename (SSDATA (spec_file), NULL);
      wchar_t filename_w[MAX_PATH];
      filename_to_utf16 (fn, filename_w);
      status = GdipCreateBitmapFromFile (filename_w, &pBitmap);
    }
  else if (STRINGP (spec_data))
    {
      IStream *pStream = SHCreateMemStream ((BYTE *) SDATA (spec_data),
                                            SBYTES (spec_data));
      if (pStream != NULL)
        {
          status = GdipCreateBitmapFromStream (pStream, &pBitmap);
          IStream_Release (pStream);
        }
    }

  metadata = Qnil;
  if (status == Ok)
    {
      /* In multiframe pictures, select the first frame.  */
      Lisp_Object lisp_index = Fplist_get (XCDR (img->spec), QCindex);
      int index = FIXNATP (lisp_index) ? XFIXNAT (lisp_index) : 0;
      int nframes;
      double delay;
      status = w32_select_active_frame (pBitmap, index, &nframes, &delay);
      if (status == Ok)
        {
          if (nframes > 1)
            metadata = Fcons (Qcount, Fcons (make_fixnum (nframes), metadata));
          if (delay >= 0)
            metadata = Fcons (Qdelay, Fcons (make_float (delay), metadata));
        }
    }

  if (status == Ok)
    {
      ARGB bg_color = w32_image_bg_color (f, img);
      Emacs_Pixmap pixmap;

      status = GdipCreateHBITMAPFromBitmap (pBitmap, &pixmap, bg_color);
      if (status == Ok)
        {
          UINT width, height;
          GdipGetImageWidth (pBitmap, &width);
          GdipGetImageHeight (pBitmap, &height);
          img->width = width;
          img->height = height;
          img->pixmap = pixmap;
          img->lisp_data = metadata;
        }

      GdipDisposeImage (pBitmap);
    }

  if (status != Ok)
    {
      add_to_log ("Unable to load image %s", img->spec);
      return 0;
    }
  return 1;
}
