/* ftfont.h -- Interface definition for Freetype font backend.
   Copyright (C) 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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


#ifndef EMACS_FTFONT_H
#define EMACS_FTFONT_H

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SIZES_H
#ifdef FT_BDF_H
# include FT_BDF_H
#endif

#ifdef HAVE_LIBOTF
# include <otf.h>
#ifdef HAVE_M17N_FLT
# include <m17n-flt.h>
#endif	/* HAVE_M17N_FLT */
#endif	/* HAVE_LIBOTF */

extern FcCharSet *ftfont_get_fc_charset (Lisp_Object);
extern Lisp_Object ftfont_open2 (struct frame *f,
                                 Lisp_Object entity,
                                 int pixel_size,
                                 Lisp_Object font_object);

/* This struct is shared by the XFT, Freetype, and Cairo font
   backends.  Members up to and including 'matrix' are common, the
   rest depend on which backend is in use.  */
struct font_info
{
  struct font font;
#ifdef HAVE_LIBOTF
  bool maybe_otf;	/* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
  FT_Size ft_size;
  int index;
  FT_Matrix matrix;

#ifdef USE_CAIRO
  cairo_font_face_t *cr_font_face;
  /* To prevent cairo from cluttering the activated FT_Size maintained
     in ftfont.c, we activate this special FT_Size before drawing.  */
  FT_Size ft_size_draw;
  /* Font metrics cache.  */
  struct font_metrics **metrics;
  short metrics_nrows;
#else
  /* These are used by the XFT backend.  */
  Display *display;
  XftFont *xftfont;
  unsigned x_display_id;
#endif
};

#endif	/* EMACS_FTFONT_H */
