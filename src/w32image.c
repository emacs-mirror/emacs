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

DEF_DLL_FN (GpStatus, GdiplusStartup,
	    (ULONG_PTR *, GdiplusStartupInput *, GdiplusStartupOutput *));
DEF_DLL_FN (VOID, GdiplusShutdown, (ULONG_PTR));
DEF_DLL_FN (GpStatus, GdipGetPropertyItemSize,
	    (GpImage *, PROPID, UINT *));
DEF_DLL_FN (GpStatus, GdipGetPropertyItem,
	    (GpImage *, PROPID, UINT, PropertyItem *));
DEF_DLL_FN (GpStatus, GdipImageGetFrameDimensionsCount, (GpImage *, UINT *));
DEF_DLL_FN (GpStatus, GdipImageGetFrameDimensionsList,
	    (GpImage *, GUID *, UINT));
DEF_DLL_FN (GpStatus, GdipImageGetFrameCount,
	    (GpImage *, GDIPCONST GUID *, UINT *));
DEF_DLL_FN (GpStatus, GdipImageSelectActiveFrame,
	    (GpImage*, GDIPCONST GUID *, UINT));
DEF_DLL_FN (GpStatus, GdipCreateBitmapFromFile, (WCHAR *, GpBitmap **));
DEF_DLL_FN (GpStatus, GdipCreateBitmapFromStream, (IStream *, GpBitmap **));
DEF_DLL_FN (IStream *, SHCreateMemStream, (const BYTE *pInit, UINT cbInit));
DEF_DLL_FN (GpStatus, GdipCreateHBITMAPFromBitmap,
	    (GpBitmap *, HBITMAP *, ARGB));
DEF_DLL_FN (GpStatus, GdipDisposeImage, (GpImage *));
DEF_DLL_FN (GpStatus, GdipGetImageHeight, (GpImage *, UINT *));
DEF_DLL_FN (GpStatus, GdipGetImageWidth, (GpImage *, UINT *));

static bool
gdiplus_init (void)
{
  HANDLE gdiplus_lib, shlwapi_lib;

  if (!((gdiplus_lib = w32_delayed_load (Qgdiplus))
	&& (shlwapi_lib = w32_delayed_load (Qshlwapi))))
    return false;

  LOAD_DLL_FN (gdiplus_lib, GdiplusStartup);
  LOAD_DLL_FN (gdiplus_lib, GdiplusShutdown);
  LOAD_DLL_FN (gdiplus_lib, GdipGetPropertyItemSize);
  LOAD_DLL_FN (gdiplus_lib, GdipGetPropertyItem);
  LOAD_DLL_FN (gdiplus_lib, GdipImageGetFrameDimensionsCount);
  LOAD_DLL_FN (gdiplus_lib, GdipImageGetFrameDimensionsList);
  LOAD_DLL_FN (gdiplus_lib, GdipImageGetFrameCount);
  LOAD_DLL_FN (gdiplus_lib, GdipImageSelectActiveFrame);
  LOAD_DLL_FN (gdiplus_lib, GdipCreateBitmapFromFile);
  LOAD_DLL_FN (gdiplus_lib, GdipCreateBitmapFromStream);
  LOAD_DLL_FN (gdiplus_lib, GdipCreateHBITMAPFromBitmap);
  LOAD_DLL_FN (gdiplus_lib, GdipDisposeImage);
  LOAD_DLL_FN (gdiplus_lib, GdipGetImageHeight);
  LOAD_DLL_FN (gdiplus_lib, GdipGetImageWidth);
  /* LOAD_DLL_FN (shlwapi_lib, SHCreateMemStream); */

  /* The following terrible kludge is required to use native image API
     on Windows before Vista, because SHCreateMemStream was not
     exported by name in those versions, only by ordinal number.  */
  fn_SHCreateMemStream =
    (W32_PFN_SHCreateMemStream) get_proc_addr (shlwapi_lib,
					       "SHCreateMemStream");
  if (!fn_SHCreateMemStream)
    {
      fn_SHCreateMemStream =
	(W32_PFN_SHCreateMemStream) get_proc_addr (shlwapi_lib,
						   MAKEINTRESOURCEA (12));
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

static unsigned long
decode_delay (PropertyItem *propertyItem, int frame)
{
  enum PropertyItem_type type = propertyItem[0].type;
  unsigned long delay;

  switch (type)
    {
    case PI_BYTE:
    case PI_BYTE_ANY:
      delay = ((unsigned char *)propertyItem[0].value)[frame];
      break;
    case PI_USHORT:
      delay = ((unsigned short *)propertyItem[0].value)[frame];
      break;
    case PI_ULONG:
    case PI_LONG:	/* delay should always be positive */
      delay = ((unsigned long *)propertyItem[0].value)[frame];
      break;
    default:
      emacs_abort ();
    }

  return delay;
}

static double
w32_frame_delay (GpBitmap *pBitmap, int frame)
{
  UINT size;
  PropertyItem *propertyItem;
  double delay = 0.0;

  /* Assume that the image has a property item of type PropertyItemEquipMake.
     Get the size of that property item.  */
  GdipGetPropertyItemSize (pBitmap, PropertyTagFrameDelay, &size);

  /* Allocate a buffer to receive the property item.  */
  propertyItem = malloc (size);
  if (propertyItem != NULL)
    {
      /* Get the property item.  */
      GdipGetPropertyItem (pBitmap, PropertyTagFrameDelay, size, propertyItem);
      delay = decode_delay (propertyItem, frame);
      if (delay <= 0)
        {
          /* In GIF files, unfortunately, delay is only specified for the first
             frame.  */
          delay = decode_delay (propertyItem, 0);
        }
      delay /= 100.0;
      free (propertyItem);
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
  *delay = 0.0;
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
      spec_file = ENCODE_FILE (spec_file);
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
          if (delay)
            metadata = Fcons (Qdelay, Fcons (make_float (delay), metadata));
        }
      else if (status == Win32Error) /* FIXME! */
	status = Ok;
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
