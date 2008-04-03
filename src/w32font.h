/* Shared GDI and Uniscribe Font backend declarations for the W32 API.
   Copyright (C) 2007, 2008 Free Software Foundation, Inc.

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

#ifndef EMACS_W32FONT_H
#define EMACS_W32FONT_H


/* Bit 17 of ntmFlags in NEWTEXTMETRIC is set for Postscript OpenType fonts,
   bit 18 for Truetype OpenType fonts, bit 20 for Type1 fonts.  */
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

/* The actual structure for a w32 font, that can be cast to struct font.
   The Uniscribe backend extends this.  */
struct w32font_info
{
  struct font font;
  TEXTMETRIC metrics;
  unsigned int glyph_idx;
  struct font_metrics ascii_metrics[128];
};

Lisp_Object w32font_get_cache P_ ((FRAME_PTR fe));
Lisp_Object w32font_list_internal P_ ((Lisp_Object frame,
                                       Lisp_Object font_spec,
                                       int opentype_only));
Lisp_Object w32font_match_internal P_ ((Lisp_Object frame,
                                        Lisp_Object font_spec,
                                        int opentype_only));
int w32font_open_internal P_ ((FRAME_PTR f, Lisp_Object font_entity,
                               int pixel_size, struct w32font_info *w32_font));
void w32font_close P_ ((FRAME_PTR f, struct font *font));
int w32font_has_char P_ ((Lisp_Object entity, int c));
int w32font_text_extents P_ ((struct font *font, unsigned *code, int nglyphs,
                              struct font_metrics *metrics));
int w32font_draw P_ ((struct glyph_string *s, int from, int to,
                      int x, int y, int with_background));


int uniscribe_check_otf P_ ((LOGFONT *font, Lisp_Object otf_spec));

#endif

/* arch-tag: ef9d9675-a2a5-4d01-9526-815e9a3da7cb
   (do not change this comment) */
