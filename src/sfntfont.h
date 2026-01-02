/* sfnt format font driver for GNU Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#ifndef _SFNTFONT_H_
#define _SFNTFONT_H_

#include "lisp.h"
#include "frame.h"
#include "font.h"
#include "sfnt.h"

extern int sfnt_enum_font (const char *);


/* Font driver callbacks.  */

extern Lisp_Object sfntfont_list (struct frame *, Lisp_Object);
extern Lisp_Object sfntfont_match (struct frame *, Lisp_Object);
extern Lisp_Object sfntfont_open (struct frame *, Lisp_Object, int);

extern unsigned int sfntfont_encode_char (struct font *, int);
extern void sfntfont_text_extents (struct font *, const unsigned int *,
				   int, struct font_metrics *);
extern void sfntfont_close (struct font *);
extern int sfntfont_draw (struct glyph_string *, int, int,
			  int, int, bool);
extern Lisp_Object sfntfont_list_family (struct frame *);
extern int sfntfont_get_variation_glyphs (struct font *, int, unsigned[256]);


/* Initialization functions.  */

typedef void (*sfntfont_put_glyph_proc) (struct glyph_string *, int, int,
					 int, int, bool, struct sfnt_raster **,
					 int *);

extern void syms_of_sfntfont (void);
extern void init_sfntfont (void);
extern void mark_sfntfont (void);
extern void init_sfntfont_vendor (Lisp_Object, const struct font_driver *,
				  sfntfont_put_glyph_proc);


/* mmap specific functions.  */

#ifdef HAVE_MMAP

extern bool sfntfont_detect_sigbus (void *);

#endif /* HAVE_MMAP */



/* HarfBuzz specific functions.  */

#ifdef HAVE_HARFBUZZ

extern hb_font_t *sfntfont_begin_hb_font (struct font *, double *);

#endif /* HAVE_HARFBUZZ */

#endif /* _SFNTFONT_H_ */
