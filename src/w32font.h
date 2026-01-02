/* Shared GDI and Uniscribe Font backend declarations for the Windows API.
   Copyright (C) 2007-2026 Free Software Foundation, Inc.

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

#ifndef EMACS_W32FONT_H
#define EMACS_W32FONT_H

#include "font.h"

/* Bit 17 of ntmFlags in NEWTEXTMETRIC is set for PostScript OpenType fonts,
   bit 18 for TrueType OpenType fonts, bit 20 for Type1 fonts.  */
#ifndef NTM_PS_OPENTYPE
#define NTM_PS_OPENTYPE 0x00020000
#endif
#ifndef NTM_TT_OPENTYPE
#define NTM_TT_OPENTYPE 0x00040000
#endif
#ifndef NTM_TYPE1
#define NTM_TYPE1 0x00100000
#endif

#define NTMFLAGS_OPENTYPE (NTM_PS_OPENTYPE | NTM_TT_OPENTYPE)

struct w32_metric_cache
{
  short lbearing, rbearing, width, ascent, descent;
  unsigned char status;
};

#define W32METRIC_NO_ATTEMPT 0
#define W32METRIC_SUCCESS 1
#define W32METRIC_FAIL 2

/* The actual structure for a w32 font, that can be cast to struct font.
   The Uniscribe backend extends this.  */
struct w32font_info
{
  struct font font;
  TEXTMETRICW metrics;
  unsigned int glyph_idx;
  struct w32_metric_cache **cached_metrics;
  int n_cache_blocks;
  HFONT hfont;
};

/* Extension of w32font_info used by Uniscribe and HarfBuzz backends.  */
struct uniscribe_font_info
{
  struct w32font_info w32_font;
  /* This is used by the Uniscribe backend as a pointer to the script
     cache, and by the HarfBuzz backend as a pointer to a hb_font_t
     object.  */
  void *cache;
  /* This is used by the HarfBuzz backend to store the font scale.  */
  double scale;
  /* This is used by DirectWrite to store the FontFace object.
     DirectWrite works on top of the HarfBuzz backend, modifying some
     calls.  If there are problems manipulating this font,
     dwrite_skip_font is set to true.  Future operations will not use
     DirectWrite and fall back to the HarfBuzz backend.  */
  void *dwrite_cache;
  float dwrite_font_size;
  bool dwrite_skip_font;
};

/* Macros for getting OS specific information from a font struct.  */
#define FONT_HANDLE(f) (((struct w32font_info *)(f))->hfont)
#define FONT_TEXTMETRIC(f) (((struct w32font_info *)(f))->metrics)

#define CACHE_BLOCKSIZE 128

Lisp_Object w32font_get_cache (struct frame *fe);
Lisp_Object w32font_list_internal (struct frame *f,
                                   Lisp_Object font_spec,
                                   bool opentype_only);
Lisp_Object w32font_match_internal (struct frame *f,
                                    Lisp_Object font_spec,
                                    bool opentype_only);
int w32font_open_internal (struct frame *f, Lisp_Object font_entity,
                           int pixel_size, Lisp_Object font_object);
void w32font_close (struct font *font);
int w32font_has_char (Lisp_Object entity, int c);
void w32font_text_extents (struct font *font, const unsigned *code, int nglyphs,
			   struct font_metrics *metrics);
int w32font_draw (struct glyph_string *s, int from, int to,
                  int x, int y, bool with_background);


int uniscribe_check_otf (LOGFONT *font, Lisp_Object otf_spec);

Lisp_Object intern_font_name (char *);

/* Function prototypes for DirectWrite.  */
void w32_initialize_direct_write (void);
bool w32_use_direct_write (struct w32font_info *w32font);
bool w32_dwrite_draw (HDC hdc, int x, int y, unsigned *glyphs, int len,
		      COLORREF color, struct font *font );
bool w32_dwrite_text_extents (struct font *font, const unsigned *code,
			      int nglyphs, struct font_metrics *metrics);
unsigned w32_dwrite_encode_char (struct font *font, int c);
void w32_dwrite_free_cached_face (void *cache);
void syms_of_w32dwrite (void);

extern void globals_of_w32font (void);

#endif
