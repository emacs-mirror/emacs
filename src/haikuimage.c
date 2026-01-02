/* Haiku window system support.
   Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "dispextern.h"
#include "haikuterm.h"
#include "coding.h"

#include "haiku_support.h"

bool
haiku_can_use_native_image_api (Lisp_Object type)
{
  const char *mime_type = NULL;

  if (EQ (type, Qnative_image))
    return 1;

#ifdef HAVE_RSVG
  if (EQ (type, Qsvg))
    return 0;
#endif

  if (EQ (type, Qjpeg))
    mime_type = "image/jpeg";
  else if (EQ (type, Qpng))
    mime_type = "image/png";
#ifndef HAVE_GIF
  else if (EQ (type, Qgif))
    mime_type = "image/gif";
#endif
  else if (EQ (type, Qtiff))
    mime_type = "image/tiff";
  else if (EQ (type, Qbmp))
    mime_type = "image/bmp";
  else if (EQ (type, Qsvg))
    mime_type = "image/svg";
  else if (EQ (type, Qpbm))
    mime_type = "image/pbm";
  /* Don't use native image APIs for image types that have animations,
     since those aren't supported by the Translation Kit.  */
#ifndef HAVE_WEBP
  else if (EQ (type, Qwebp))
    mime_type = "image/webp";
#endif

  if (!mime_type)
    return 0;

  return be_can_translate_type_to_bitmap_p (mime_type);
}

extern int
haiku_load_image (struct frame *f, struct image *img,
		  Lisp_Object spec_file, Lisp_Object spec_data)
{
  eassert (valid_image_p (img->spec));

  void *pixmap = NULL;

  if (STRINGP (spec_file))
    {
      pixmap = be_translate_bitmap_from_file_name
	(SSDATA (ENCODE_UTF_8 (spec_file)));
    }
  else if (STRINGP (spec_data))
    {
      pixmap = be_translate_bitmap_from_memory
	(SSDATA (spec_data), SBYTES (spec_data));
    }

  void *conv = NULL;

  if (!pixmap || !BBitmap_convert (pixmap, &conv))
    {
      add_to_log ("Unable to load image %s", img->spec);
      return 0;
    }

  if (conv)
    {
      BBitmap_free (pixmap);
      pixmap = conv;
    }

  int left, top, right, bottom, stride, mono_p;
  BBitmap_dimensions (pixmap, &left, &top, &right, &bottom, &stride, &mono_p);

  img->width = (1 + right - left);
  img->height = (1 + bottom - top);
  img->pixmap = pixmap;

  return 1;
}

void
syms_of_haikuimage (void)
{
}
