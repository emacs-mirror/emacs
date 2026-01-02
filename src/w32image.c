/* Implementation of MS-Windows native image API via the GDI+ library.

Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "w32gdiplus.h"
#ifdef WINDOWSNT
GdiplusStartup_Proc fn_GdiplusStartup;
GdiplusShutdown_Proc fn_GdiplusShutdown;
GdipCreateFromHDC_Proc fn_GdipCreateFromHDC;
GdipDeleteGraphics_Proc fn_GdipDeleteGraphics;
GdipGetPropertyItemSize_Proc fn_GdipGetPropertyItemSize;
GdipGetPropertyItem_Proc fn_GdipGetPropertyItem;
GdipImageGetFrameDimensionsCount_Proc fn_GdipImageGetFrameDimensionsCount;
GdipImageGetFrameDimensionsList_Proc fn_GdipImageGetFrameDimensionsList;
GdipImageGetFrameCount_Proc fn_GdipImageGetFrameCount;
GdipImageSelectActiveFrame_Proc fn_GdipImageSelectActiveFrame;
GdipCreateBitmapFromFile_Proc fn_GdipCreateBitmapFromFile;
GdipCreateBitmapFromStream_Proc fn_GdipCreateBitmapFromStream;
GdipCreateBitmapFromScan0_Proc fn_GdipCreateBitmapFromScan0;
SHCreateMemStream_Proc fn_SHCreateMemStream;
GdipCreateHBITMAPFromBitmap_Proc fn_GdipCreateHBITMAPFromBitmap;
GdipCreateBitmapFromHBITMAP_Proc fn_GdipCreateBitmapFromHBITMAP;
GdipDrawImageRectRectI_Proc fn_GdipDrawImageRectRectI;
GdipSetInterpolationMode_Proc fn_GdipSetInterpolationMode;
GdipDisposeImage_Proc fn_GdipDisposeImage;
GdipGetImageHeight_Proc fn_GdipGetImageHeight;
GdipGetImageWidth_Proc fn_GdipGetImageWidth;
GdipGetImageEncodersSize_Proc fn_GdipGetImageEncodersSize;
GdipGetImageEncoders_Proc fn_GdipGetImageEncoders;
GdipLoadImageFromFile_Proc fn_GdipLoadImageFromFile;
GdipGetImageThumbnail_Proc fn_GdipGetImageThumbnail;
GdipSaveImageToFile_Proc fn_GdipSaveImageToFile;
GdipImageRotateFlip_Proc fn_GdipImageRotateFlip;

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
  fn_GdipCreateFromHDC = (GdipCreateFromHDC_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateFromHDC");
  if (!fn_GdipCreateFromHDC)
    return false;
  fn_GdipDeleteGraphics = (GdipDeleteGraphics_Proc)
    get_proc_addr (gdiplus_lib, "GdipDeleteGraphics");
  if (!fn_GdipDeleteGraphics)
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
  fn_GdipCreateBitmapFromScan0 = (GdipCreateBitmapFromScan0_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateBitmapFromScan0");
  if (!fn_GdipCreateBitmapFromScan0)
    return false;
  fn_GdipCreateHBITMAPFromBitmap = (GdipCreateHBITMAPFromBitmap_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateHBITMAPFromBitmap");
  if (!fn_GdipCreateHBITMAPFromBitmap)
    return false;
  fn_GdipCreateBitmapFromHBITMAP = (GdipCreateBitmapFromHBITMAP_Proc)
    get_proc_addr (gdiplus_lib, "GdipCreateBitmapFromHBITMAP");
  if (!fn_GdipCreateBitmapFromHBITMAP)
    return false;
  fn_GdipDrawImageRectRectI = (GdipDrawImageRectRectI_Proc)
    get_proc_addr (gdiplus_lib, "GdipDrawImageRectRectI");
  if (!fn_GdipDrawImageRectRectI)
    return false;
  fn_GdipSetInterpolationMode = (GdipSetInterpolationMode_Proc)
    get_proc_addr (gdiplus_lib, "GdipSetInterpolationMode");
  if (!fn_GdipSetInterpolationMode)
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
  fn_GdipGetImageEncodersSize = (GdipGetImageEncodersSize_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetImageEncodersSize");
  if (!fn_GdipGetImageEncodersSize)
    return false;
  fn_GdipGetImageEncoders = (GdipGetImageEncoders_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetImageEncoders");
  if (!fn_GdipGetImageEncoders)
    return false;
  fn_GdipLoadImageFromFile = (GdipLoadImageFromFile_Proc)
    get_proc_addr (gdiplus_lib, "GdipLoadImageFromFile");
  if (!fn_GdipLoadImageFromFile)
    return false;
  fn_GdipGetImageThumbnail = (GdipGetImageThumbnail_Proc)
    get_proc_addr (gdiplus_lib, "GdipGetImageThumbnail");
  if (!fn_GdipGetImageThumbnail)
    return false;
  fn_GdipSaveImageToFile = (GdipSaveImageToFile_Proc)
    get_proc_addr (gdiplus_lib, "GdipSaveImageToFile");
  if (!fn_GdipSaveImageToFile)
    return false;
  fn_GdipImageRotateFlip = (GdipImageRotateFlip_Proc)
    get_proc_addr (gdiplus_lib, "GdipImageRotateFlip");
  if (!fn_GdipImageRotateFlip)
    return false;

  return true;
}

#endif	/* WINDOWSNT */

static int gdip_initialized;
static bool gdiplus_started;
static ULONG_PTR token;
static GdiplusStartupInput input;
static GdiplusStartupOutput output;


/* Initialize GDI+, return true if successful.  */
bool
w32_gdiplus_startup (void)
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
	|| EQ (type, Qbmp)
	|| EQ (type, Qnative_image)))
    {
      /* GDI+ can also display Exif, ICON, WMF, and EMF images.
	 But we don't yet support these in image.c.  */
      return false;
    }
  return w32_gdiplus_startup ();
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
  Lisp_Object specified_bg = plist_get (XCDR (img->spec), QCbackground);
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
      Lisp_Object lisp_index = plist_get (XCDR (img->spec), QCindex);
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

struct cached_encoder {
  int num;
  char *type;
  CLSID clsid;
};

static struct cached_encoder last_encoder;

struct thumb_type_data {
  const char *ext;
  const wchar_t *mime;
};

static struct thumb_type_data thumb_types [] =
  {
    /* jpg and png are at the front because 'image-dired-thumb-name'
       uses them in most cases. */
    {"jpg", L"image/jpeg"},
    {"png", L"image/png"},
    {"bmp", L"image/bmp"},
    {"jpeg", L"image/jpeg"},
    {"gif", L"image/gif"},
    {"tiff", L"image/tiff"},
    {NULL, NULL}
  };


int
w32_gdip_get_encoder_clsid (const char *type, CLSID *clsid)
{
  /* A simple cache based on the assumptions that many thumbnails will
     be generated using the same TYPE.  */
  if (last_encoder.type && stricmp (type, last_encoder.type) == 0)
    {
      *clsid = last_encoder.clsid;
      return last_encoder.num;
    }

  const wchar_t *format = NULL;
  struct thumb_type_data *tp = thumb_types;
  for ( ; tp->ext; tp++)
    {
      if (stricmp (type, tp->ext) == 0)
	{
	  format = tp->mime;
	  break;
	}
    }
  if (!format)
    return -1;

  unsigned num = 0;
  unsigned size = 0;
  ImageCodecInfo *image_codec_info = NULL;

  GdipGetImageEncodersSize (&num, &size);
  if(size == 0)
    return -1;

  image_codec_info = xmalloc (size);
  GdipGetImageEncoders (num, size, image_codec_info);

  for (int j = 0; j < num; ++j)
    {
      if (wcscmp (image_codec_info[j].MimeType, format) == 0 )
	{
	  if (last_encoder.type)
	    xfree (last_encoder.type);
	  last_encoder.type = xstrdup (tp->ext);
	  last_encoder.clsid = image_codec_info[j].Clsid;
	  last_encoder.num = j;
          *clsid = image_codec_info[j].Clsid;
          xfree (image_codec_info);
          return j;
	}
    }

  xfree (image_codec_info);
  return -1;
}

DEFUN ("w32image-create-thumbnail", Fw32image_create_thumbnail,
       Sw32image_create_thumbnail, 5, 5, 0,
       doc: /* Create a HEIGHT by WIDTH thumbnail file THUMB-FILE for image INPUT-FILE.
TYPE is the image type to use for the thumbnail file, a string.  It is
usually identical to the file-name extension of THUMB-FILE, but without
the leading period, and both "jpeg" and "jpg" can be used for JPEG.
TYPE is matched case-insensitively against supported types.  Currently,
the supported TYPEs are BMP, JPEG, GIF, TIFF, and PNG; any other type
will cause the function to fail.
Return non-nil if thumbnail creation succeeds, nil otherwise.  */)
  (Lisp_Object input_file, Lisp_Object thumb_file, Lisp_Object type,
   Lisp_Object height, Lisp_Object width)
{
  /* Sanity checks.  */
  CHECK_STRING (input_file);
  CHECK_STRING (thumb_file);
  CHECK_STRING (type);
  CHECK_FIXNAT (height);
  CHECK_FIXNAT (width);

  if (!gdiplus_started)
    {
      if (!w32_gdiplus_startup ())
	return Qnil;
    }

  /* Create an image by reading from INPUT_FILE.  */
  wchar_t input_file_w[MAX_PATH];
  input_file
    = ENCODE_FILE (Fexpand_file_name (Fcopy_sequence (input_file), Qnil));
  unixtodos_filename (SSDATA (input_file));
  filename_to_utf16 (SSDATA (input_file), input_file_w);
  GpImage *file_image;
  GpStatus status = GdipLoadImageFromFile (input_file_w, &file_image);

  if (status == Ok)
    {
      /* Create a thumbnail for the image.  */
      GpImage *thumb_image;
      status = GdipGetImageThumbnail (file_image,
				      XFIXNAT (width), XFIXNAT (height),
				      &thumb_image, NULL, NULL);
      GdipDisposeImage (file_image);
      CLSID thumb_clsid;
      if (status == Ok
	  /* Get the GUID of the TYPE's encoder. */
	  && w32_gdip_get_encoder_clsid (SSDATA (type), &thumb_clsid) >= 0)
	{
	  /* Save the thumbnail image to a file of specified TYPE.  */
	  wchar_t thumb_file_w[MAX_PATH];
	  thumb_file
	    = ENCODE_FILE (Fexpand_file_name (Fcopy_sequence (thumb_file),
					      Qnil));
	  unixtodos_filename (SSDATA (thumb_file));
	  filename_to_utf16 (SSDATA (thumb_file), thumb_file_w);
	  status = GdipSaveImageToFile (thumb_image, thumb_file_w,
					&thumb_clsid, NULL);
	  GdipDisposeImage (thumb_image);
	}
      else if (status == Ok)	/* no valid encoder */
	status = InvalidParameter;
    }
  return (status == Ok) ? Qt : Qnil;
}

void
syms_of_w32image (void)
{
  defsubr (&Sw32image_create_thumbnail);
}

void
globals_of_w32image (void)
{
}
