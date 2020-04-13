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
#include <objidl.h>
#include <wtypes.h>
#include <gdiplus.h>
#include <shlwapi.h>
#include "w32common.h"
#include "w32term.h"
#include "frame.h"
#include "coding.h"

/*#define LINK_GDIPLUS_STATICALLY 1*/

#ifndef LINK_GDIPLUS_STATICALLY
DEF_DLL_FN (GpStatus, GdiplusStartup, (ULONG_PTR *, GdiplusStartupInput *, GdiplusStartupOutput *));
DEF_DLL_FN (VOID, GdiplusShutdown, (ULONG_PTR));
DEF_DLL_FN (GpStatus, GdipGetPropertyItemSize, (GpImage *, PROPID, UINT *));
DEF_DLL_FN (GpStatus, GdipGetPropertyItem, (GpImage *, PROPID, UINT, PropertyItem *));
DEF_DLL_FN (GpStatus, GdipImageGetFrameDimensionsCount, (GpImage *, UINT *));
DEF_DLL_FN (GpStatus, GdipImageGetFrameDimensionsList, (GpImage *, GUID *, UINT));
DEF_DLL_FN (GpStatus, GdipImageGetFrameCount, (GpImage *, GDIPCONST GUID *, UINT *));
DEF_DLL_FN (GpStatus, GdipImageSelectActiveFrame, (GpImage*, GDIPCONST GUID *, UINT));
DEF_DLL_FN (GpStatus, GdipCreateBitmapFromFile, (WCHAR *, GpBitmap **));
DEF_DLL_FN (GpStatus, GdipCreateBitmapFromStream, (IStream *, GpBitmap **));
DEF_DLL_FN (IStream *, SHCreateMemStream, (const BYTE *pInit, UINT cbInit));
DEF_DLL_FN (GpStatus, GdipCreateHBITMAPFromBitmap, (GpBitmap *, HBITMAP *, ARGB));
DEF_DLL_FN (GpStatus, GdipDisposeImage, (GpImage *));
DEF_DLL_FN (GpStatus, GdipGetImageHeight, (GpImage *, UINT *));
DEF_DLL_FN (GpStatus, GdipGetImageWidth, (GpImage *, UINT *));
#endif

static int gdip_initialized = 0;
static ULONG_PTR token;
static GdiplusStartupInput input;
static GdiplusStartupOutput output;

bool
w32_gdiplus_startup (void)
{
  HANDLE gdiplus_lib, shlwapi_lib;
  GpStatus status;

  if (gdip_initialized < 0)
      return 0;
  else if (gdip_initialized)
      return 1;

#ifndef LINK_GDIPLUS_STATICALLY
  DEFSYM (Qgdiplus, "gdiplus");
  DEFSYM (Qshlwapi, "shlwapi");
  if (!(gdiplus_lib = w32_delayed_load (Qgdiplus))) {
    gdip_initialized = -1;
    return 0;
  }
  if (!(shlwapi_lib = w32_delayed_load (Qshlwapi))) {
    gdip_initialized = -1;
    return 0;
  }

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
  LOAD_DLL_FN (shlwapi_lib, SHCreateMemStream);

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
#endif

  input.GdiplusVersion = 1;
  input.DebugEventCallback = NULL;
  input.SuppressBackgroundThread = FALSE;
  input.SuppressExternalCodecs = FALSE;

  status = GdiplusStartup (&token, &input, &output);
  if (status == Ok)
    {
      gdip_initialized = 1;
      return 1;
    }
  else
    {
      gdip_initialized = -1;
      return 0;
    }
}

void
w32_gdiplus_shutdown (void)
{
  GdiplusShutdown (token);
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
  propertyItem = (PropertyItem*)malloc (size);
  if (propertyItem != NULL)
    {
      /* Get the property item.  */
      GdipGetPropertyItem (pBitmap, PropertyTagFrameDelay, size, propertyItem);
      delay = ((double)propertyItem[frame].length) / 100;
      if (delay == 0)
        {
          /* In GIF files, unfortunately, delay is only specified for the first
             frame.  */
          delay = ((double)propertyItem[0].length) / 100;
        }
      free (propertyItem);
    }
  return delay;
}

static UINT
w32_select_active_frame (GpBitmap *pBitmap, int frame, int *nframes, double *delay)
{
  UINT count, frameCount;
  GUID pDimensionIDs[1];
  GpStatus status = Ok;

  status = GdipImageGetFrameDimensionsCount (pBitmap, &count);
  frameCount = *nframes = 0;
  *delay = 0.0;
  if (count)
    {
      status = GdipImageGetFrameDimensionsList (pBitmap, pDimensionIDs, 1);
      status = GdipImageGetFrameCount (pBitmap, &pDimensionIDs[0], &frameCount);
      if ((status == Ok) && (frameCount > 1))
        {
          if (frame < 0 || frame >= frameCount)
            {
              status = GenericError;
            }
          else
            {
              status = GdipImageSelectActiveFrame (pBitmap, &pDimensionIDs[0], frame);
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
  /* png_color_16 *image_bg; */
  Lisp_Object specified_bg
    = Fplist_get (XCDR (img->spec), QCbackground);
  Emacs_Color color;

  /* If the user specified a color, try to use it; if not, use the
     current frame background, ignoring any default background
     color set by the image.  */
  if (STRINGP (specified_bg)
      ? w32_defined_color (f, SSDATA (specified_bg), &color, false, false)
      : (w32_query_frame_background_color (f, &color), true))
    /* The user specified `:background', use that.  */
    {
      DWORD red = (((DWORD) color.red) & 0xff00) << 8;
      DWORD green = ((DWORD) color.green) & 0xff00;
      DWORD blue = ((DWORD) color.blue) >> 8;
      return red | green | blue;
    }
  return ((DWORD) 0xff000000);
}

int
w32_load_image (struct frame *f, struct image *img,
                Lisp_Object spec_file, Lisp_Object spec_data)
{
  Emacs_Pixmap pixmap;
  GpStatus status = GenericError;
  GpBitmap *pBitmap;
  wchar_t filename[MAX_PATH];
  ARGB bg_color;
  Lisp_Object lisp_index, metadata;
  unsigned int index, nframes;
  double delay;

  eassert (valid_image_p (img->spec));

  /* This function only gets called if init_w32_gdiplus () was invoked. We have
     a valid token and GDI+ is active.  */
  if (STRINGP (spec_file))
    {
      if (w32_unicode_filenames)
        {
          filename_to_utf16 (SSDATA (spec_file) , filename);
          status = GdipCreateBitmapFromFile (filename, &pBitmap);
        }
      else
        {
          add_to_log ("GDI+ requires w32-unicode-filenames to be T");
          status = GenericError;
        }
    }
  else if (STRINGP (spec_data))
    {
      IStream *pStream = SHCreateMemStream ((BYTE *) SSDATA (spec_data),
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
      /* In multiframe pictures, select the first one */
      lisp_index = Fplist_get (XCDR (img->spec), QCindex);
      index = FIXNUMP (lisp_index) ? XFIXNAT (lisp_index) : 0;
      status = w32_select_active_frame (pBitmap, index, &nframes, &delay);
      if ((status == Ok))
        {
          if (nframes > 1)
            metadata = Fcons (Qcount, Fcons (make_fixnum (nframes), metadata));
          if (delay)
            metadata = Fcons (Qdelay, Fcons (make_float (delay), metadata));
        }
    }

  if (status == Ok)
    {
      bg_color = w32_image_bg_color (f, img);
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
