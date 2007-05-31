/* Font backend for the Microsoft W32 API.
   Copyright (C) 2007 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>
#include <windows.h>

#include "lisp.h"
#include "w32term.h"
#include "frame.h"
#include "dispextern.h"
#include "character.h"
#include "charset.h"
#include "fontset.h"
#include "font.h"

/* The actual structure for a w32 font, that can be cast to struct font.  */
struct w32font_info
{
  struct font font;
  TEXTMETRIC metrics;
  /* Unicode subset bitfield. See MSDN documentation for FONTSIGNATURE.  */
  DWORD *subranges;
};

extern struct font_driver w32font_driver;

Lisp_Object Qw32;
static Lisp_Object Qmodern, Qswiss, Qroman, Qdecorative, Qscript, Qunknown;

static void fill_in_logfont P_ ((FRAME_PTR f, LOGFONT *logfont,
                                 Lisp_Object font_spec));

static void set_fonts_frame P_ ((Lisp_Object fontlist, Lisp_Object frame));

static int unicode_range_for_char (unsigned c);

static void list_all_matching_fonts P_ ((Lisp_Object frame,
                                         LOGFONT *font_match_pattern,
                                         Lisp_Object* list));

/* From old font code in w32fns.c */
char * w32_to_x_charset P_ ((int charset, char * matching));

static Lisp_Object w32_registry P_ ((LONG w32_charset));

/* EnumFontFamiliesEx callbacks.  */
static int CALLBACK add_font_entity_to_list P_ ((ENUMLOGFONTEX *,
                                                 NEWTEXTMETRICEX *,
                                                 DWORD, LPARAM));
static int CALLBACK add_one_font_entity_to_list P_ ((ENUMLOGFONTEX *,
                                                     NEWTEXTMETRICEX *,
                                                     DWORD, LPARAM));
static int CALLBACK add_font_name_to_list P_ ((ENUMLOGFONTEX *,
                                               NEWTEXTMETRICEX *,
                                               DWORD, LPARAM));

/* W32 API functions only available on some versions of Windows  */
typedef DWORD (*GETGLYPHINDICES) (HDC, wchar_t *, int, LPWORD, DWORD);
typedef BOOL (*GETTEXTEXTENTPTI) (HDC, LPWORD, int, LPSIZE);
static GETGLYPHINDICES get_glyph_indices_fn = NULL;
static GETTEXTEXTENTPTI get_text_extent_pointi_fn = NULL;
/* MingW headers only define this when _WIN32_WINNT >= 0x0500, but we
   target older versions.  */
#define GGI_MARK_NONEXISTING_GLYPHS 1

static int
memq_no_quit (elt, list)
     Lisp_Object elt, list;
{
  while (CONSP (list) && ! EQ (XCAR (list), elt))
    list = XCDR (list);
  return (CONSP (list));
}

/* w32 implementation of get_cache for font backend.
   Return a cache of font-entities on FRAME.  The cache must be a
   cons whose cdr part is the actual cache area.  */
static Lisp_Object w32font_get_cache (Lisp_Object frame)
{
  struct w32_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (XFRAME (frame));

  return (dpyinfo->name_list_element);
}

/* w32 implementation of list for font backend.
   List fonts exactly matching with FONT_SPEC on FRAME.  The value
   is a vector of font-entities.  This is the sole API that
   allocates font-entities.  */
static Lisp_Object w32font_list (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object list = Qnil;
  LOGFONT font_match_pattern;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  bzero (&font_match_pattern, sizeof (font_match_pattern));
  fill_in_logfont (f, &font_match_pattern, font_spec);


  if (font_match_pattern.lfFaceName[0] == '\0')
    {
      /* EnumFontFamiliesEx does not take other fields into account if
         font name is blank, so need to use two passes.  */
      list_all_matching_fonts (frame, &font_match_pattern, &list);
    }
  else
    {
      dc = get_frame_dc (f);

      EnumFontFamiliesEx (dc, &font_match_pattern,
                          (FONTENUMPROC) add_font_entity_to_list,
                          (LPARAM) &list, 0);
      release_frame_dc (f, dc);
    }

  set_fonts_frame (list, frame);

  return list;
}

/* w32 implementation of match for font backend.
   Return a font entity most closely matching with FONT_SPEC on
   FRAME.  The closeness is detemined by the font backend, thus
   `face-font-selection-order' is ignored here.  */
static Lisp_Object w32font_match (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object list = Qnil;
  LOGFONT font_match_pattern;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  bzero (&font_match_pattern, sizeof (font_match_pattern));
  fill_in_logfont (f, &font_match_pattern, font_spec);

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &font_match_pattern,
                      (FONTENUMPROC) add_one_font_entity_to_list,
                      (LPARAM) &list, 0);
  release_frame_dc (f, dc);

  set_fonts_frame (list, frame);

  return NILP (list) ? Qnil : XCAR (list);
}


/* w32 implementation of list_family for font backend.
   List available families.  The value is a list of family names
   (symbols).  */
static Lisp_Object w32font_list_family (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  LOGFONT font_match_pattern;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  bzero (&font_match_pattern, sizeof (font_match_pattern));

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &font_match_pattern,
                      (FONTENUMPROC) add_font_name_to_list,
                      (LPARAM) &list, 0);
  release_frame_dc (f, dc);

  return list;
}

/* w32 implementation of open for font backend.
   Open a font specified by FONT_ENTITY on frame F.
   If the font is scalable, open it with PIXEL_SIZE.  */
static struct font* w32font_open (FRAME_PTR f, Lisp_Object font_entity,
                                  int pixel_size)
{
  int len, size;
  LOGFONT logfont;
  HDC dc;
  HFONT hfont, old_font;
  Lisp_Object val;
  /* For backwards compatibility.  */
  W32FontStruct *compat_w32_font;

  struct w32font_info *w32_font = xmalloc (sizeof (struct w32font_info));

  struct font * font = (struct font *) w32_font;
  if (!font)
    return NULL;

  bzero (&logfont, sizeof (logfont));
  fill_in_logfont (f, &logfont, font_entity);

  size = XINT (AREF (font_entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;

  logfont.lfHeight = size;
  hfont = CreateFontIndirect (&logfont);

  if (hfont == NULL)
    {
      xfree (w32_font);
      return NULL;
    }

  /* Get the metrics for this font.  */
  dc = get_frame_dc (f);
  old_font = SelectObject (dc, hfont);

  GetTextMetrics (dc, &w32_font->metrics);

  SelectObject (dc, old_font);
  release_frame_dc (f, dc);
  /* W32FontStruct - we should get rid of this, and use the w32font_info
     struct for any W32 specific fields. font->font.font can then be hfont.  */
  font->font.font = xmalloc (sizeof (W32FontStruct));
  compat_w32_font = (W32FontStruct *) font->font.font;
  bzero (compat_w32_font, sizeof (W32FontStruct));
  compat_w32_font->font_type = UNICODE_FONT;
  /* Duplicate the text metrics.  */
  bcopy (&w32_font->metrics,  &compat_w32_font->tm, sizeof (TEXTMETRIC));
  compat_w32_font->hfont = hfont;

  font->font.font_idx = 0;
  len = strlen (logfont.lfFaceName);
  font->font.name = (char *) xmalloc (len + 1);
  bcopy (logfont.lfFaceName, font->font.name, len);
  font->font.name[len] = '\0';
  font->font.full_name = font->font.name;
  font->font.charset = 0;
  font->font.codepage = 0;
  font->font.size = w32_font->metrics.tmMaxCharWidth;
  font->font.height = w32_font->metrics.tmHeight
    + w32_font->metrics.tmExternalLeading;
  font->font.space_width = font->font.average_width
    = w32_font->metrics.tmAveCharWidth;

  font->font.vertical_centering = 0;
  font->font.encoding_type = 0;
  font->font.baseline_offset = 0;
  font->font.relative_compose = 0;
  font->font.default_ascent = w32_font->metrics.tmAscent;
  font->font.font_encoder = NULL;
  font->entity = font_entity;
  font->pixel_size = size;
  font->driver = &w32font_driver;
  font->format = Qw32;
  font->file_name = NULL;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  font->min_width = 0;
  font->ascent = w32_font->metrics.tmAscent;
  font->descent = w32_font->metrics.tmDescent;
  font->scalable = w32_font->metrics.tmPitchAndFamily & TMPF_VECTOR;

  /* Truetype fonts will have extra information about the characters
     covered by the font.  */
  val = AREF (font_entity, FONT_EXTRA_INDEX);
  if (XTYPE (val) == Lisp_Misc && XMISCTYPE (val) == Lisp_Misc_Save_Value)
    ((struct w32font_info *)(font))->subranges = XSAVE_VALUE (val)->pointer;
  else
    ((struct w32font_info *)(font))->subranges = NULL;

  return font;
}

/* w32 implementation of close for font_backend.
   Close FONT on frame F.  */
static void w32font_close (FRAME_PTR f, struct font *font)
{
  if (font->font.font)
    {
      W32FontStruct *old_w32_font = (W32FontStruct *)font->font.font;
      DeleteObject (font->font.font);
      xfree (old_w32_font);
      font->font.font = 0;
    }

  if (font->font.name)
    xfree (font->font.name);
  xfree (font);
}

/* w32 implementation of has_char for font backend.
   Optional.
   If FONT_ENTITY has a glyph for character C (Unicode code point),
   return 1.  If not, return 0.  If a font must be opened to check
   it, return -1.  */
static int w32font_has_char (Lisp_Object entity, int c)
{
  Lisp_Object val;
  DWORD *ranges;
  int index;
  DWORD mask;

  val = AREF (entity, FONT_EXTRA_INDEX);
  if (XTYPE (val) != Lisp_Misc || XMISCTYPE (val) != Lisp_Misc_Save_Value)
    return -1;

  ranges = XSAVE_VALUE (val)->pointer;

  index = unicode_range_for_char (c);
  mask = 1 << (index % 32);
  index = index / 32;

  if (ranges[index] & mask)
    return 1;
  else
    return 0;
}

/* w32 implementation of encode_char for font backend.
   Return a glyph code of FONT for characer C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
static unsigned w32font_encode_char (struct font *font, int c)
{
  if (get_glyph_indices_fn)
    {
      HFONT old_font;
      WORD glyph[2];
      int converted;
      /* FIXME: Be nice if we had a frame here, rather than getting
         the desktop's device context to measure against... */
      HDC dc = GetDC (NULL);
      wchar_t string[2];
      string[0] = c;
      string[1] = 0x0000;

      if (get_glyph_indices_fn)
        converted = (*get_glyph_indices_fn) (dc, string, 1, glyph,
                                             GGI_MARK_NONEXISTING_GLYPHS);

      /* Restore state and release DC.  */
      SelectObject (dc, old_font);
      ReleaseDC (NULL, dc);
      if (converted > 0 && glyph[0] != 0xFFFF)
        return glyph[0];
      else if (converted != GDI_ERROR)
        return FONT_INVALID_CODE;
    }
  /* On older platforms, or if the above fails, return the unicode
     code point.  */
  return c;
}

/* w32 implementation of text_extents for font backend.
   Perform the size computation of glyphs of FONT and fillin members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS).  Apparently medtrics can be NULL, in this
   case just return the overall width.  */
static int w32font_text_extents (struct font *font,
                                 unsigned *code, int nglyphs,
                                 struct font_metrics *metrics)
{
  int i;
  HFONT old_font;
  /* FIXME: Be nice if we had a frame here, rather than getting the desktop's
     device context to measure against... */
  HDC dc = GetDC (NULL);
  int total_width = 0;

  /* TODO: Allow some extra room for surrogates. */
  WORD *wcode = alloca(nglyphs * sizeof (WORD));

  old_font = SelectObject (dc, ((W32FontStruct *)(font->font.font))->hfont);

  if (metrics)
    {
      GLYPHMETRICS gm;
      int i;
      UINT format = GGO_METRICS;
      if (get_text_extent_pointi_fn)
        format |= GGO_GLYPH_INDEX;

      for (i = 0; i < nglyphs; i++)
        {
          if (GetGlyphOutline (dc, *(code + i), format, &gm, 0, NULL, NULL)
              != GDI_ERROR)
            {
              metrics[i].lbearing = gm.gmptGlyphOrigin.x;
              metrics[i].rbearing = gm.gmptGlyphOrigin.x + gm.gmBlackBoxX;
              metrics[i].width = gm.gmCellIncX;
              metrics[i].ascent = -gm.gmptGlyphOrigin.y;
              metrics[i].descent = gm.gmBlackBoxY + gm.gmptGlyphOrigin.y;
            }
          else
            {
              metrics[i].lbearing = 0;
              metrics[i].rbearing = font->font.size
                + ((struct w32font_info *) font)->metrics.tmOverhang;
              metrics[i].width = font->font.size;
              metrics[i].ascent = font->ascent;
              metrics[i].descent = font->descent;
            }
        }
    }

  for (i = 0; i < nglyphs; i++)
    {
      if (code[i] < 0x10000)
        wcode[i] = code[i];
      else
        {
          /* TODO: Convert to surrogate, reallocating array if needed */
          wcode[i] = 0xffff;
        }
    }

  if (get_text_extent_pointi_fn)
    {
      SIZE size;
      if ((*get_text_extent_pointi_fn) (dc, wcode, nglyphs, &size))
        {
          total_width = size.cx;
        }
    }

  if (total_width == 0)
    {
      RECT rect;
      rect.top = 0; rect.bottom = font->font.height; rect.left = 0; rect.right = 1;
      DrawTextW (dc, wcode, nglyphs, &rect,
                 DT_CALCRECT | DT_NOPREFIX | DT_SINGLELINE);
      total_width = rect.right;
    }
  /* Restore state and release DC.  */
  SelectObject (dc, old_font);
  ReleaseDC (NULL, dc);

  return total_width;
}

/* w32 implementation of draw for font backend.
   Optional.
   Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
   position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
   is nonzero, fill the background in advance.  It is assured that
   WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars).  */
static int w32font_draw (struct glyph_string *s, int from, int to,
                         int x, int y, int with_background)
{
  /* TODO: Do we need to specify ETO_GLYPH_INDEX or is char2b always utf-16?  */
  UINT options = 0;

  if (with_background)
    {
      options = ETO_OPAQUE;
      SetBkColor (s->hdc, s->gc->background);
    }
  else
    SetBkMode (s->hdc, TRANSPARENT);

  ExtTextOutW (s->hdc, x, y, options, NULL, s->char2b + from, to - from, NULL);
}

/* w32 implementation of free_entity for font backend.
   Optional (if FONT_EXTRA_INDEX is not Lisp_Save_Value).
   Free FONT_EXTRA_INDEX field of FONT_ENTITY.
static void w32font_free_entity (Lisp_Object entity);
  */

/* w32 implementation of prepare_face for font backend.
   Optional (if FACE->extra is not used).
   Prepare FACE for displaying characters by FONT on frame F by
   storing some data in FACE->extra.  If successful, return 0.
   Otherwise, return -1.
static int w32font_prepare_face (FRAME_PTR f, struct face *face);
  */
/* w32 implementation of done_face for font backend.
   Optional.
   Done FACE for displaying characters by FACE->font on frame F.
static void w32font_done_face (FRAME_PTR f, struct face *face);  */

/* w32 implementation of get_bitmap for font backend.
   Optional.
   Store bitmap data for glyph-code CODE of FONT in BITMAP.  It is
   intended that this method is callled from the other font-driver
   for actual drawing.
static int w32font_get_bitmap (struct font *font, unsigned code,
                               struct font_bitmap *bitmap,
                               int bits_per_pixel);
  */
/* w32 implementation of free_bitmap for font backend.
   Optional.
   Free bitmap data in BITMAP.
static void w32font_free_bitmap (struct font *font, struct font_bitmap *bitmap);
  */
/* w32 implementation of get_outline for font backend.
   Optional.
   Return an outline data for glyph-code CODE of FONT.  The format
   of the outline data depends on the font-driver.
static void* w32font_get_outline (struct font *font, unsigned code);
  */
/* w32 implementation of free_outline for font backend.
   Optional.
   Free OUTLINE (that is obtained by the above method).
static void w32font_free_outline (struct font *font, void *outline);
  */
/* w32 implementation of anchor_point for font backend.
   Optional.
   Get coordinates of the INDEXth anchor point of the glyph whose
   code is CODE.  Store the coordinates in *X and *Y.  Return 0 if
   the operations was successfull.  Otherwise return -1.
static int w32font_anchor_point (struct font *font, unsigned code,
                                 int index, int *x, int *y);
  */
/* w32 implementation of otf_capability for font backend.
   Optional.
   Return a list describing which scripts/languages FONT
   supports by which GSUB/GPOS features of OpenType tables.
static Lisp_Object w32font_otf_capability (struct font *font);
  */
/* w32 implementation of otf_drive for font backend.
   Optional.
   Apply FONT's OTF-FEATURES to the glyph string.

   FEATURES specifies which OTF features to apply in this format:
      (SCRIPT LANGSYS GSUB-FEATURE GPOS-FEATURE)
   See the documentation of `font-drive-otf' for the detail.

   This method applies the specified features to the codes in the
   elements of GSTRING-IN (between FROMth and TOth).  The output
   codes are stored in GSTRING-OUT at the IDXth element and the
   following elements.

   Return the number of output codes.  If none of the features are
   applicable to the input data, return 0.  If GSTRING-OUT is too
   short, return -1.
static int w32font_otf_drive (struct font *font, Lisp_Object features,
                              Lisp_Object gstring_in, int from, int to,
                              Lisp_Object gstring_out, int idx,
                              int alternate_subst);
  */

/* Callback function for EnumFontFamiliesEx.
 * Adds the name of a font to a Lisp list (passed in as the lParam arg).  */
static int CALLBACK add_font_name_to_list (ENUMLOGFONTEX *logical_font,
                                           NEWTEXTMETRICEX *physical_font,
                                           DWORD font_type,
                                  LPARAM list_object)
{
  Lisp_Object* list = (Lisp_Object *) list_object;
  Lisp_Object family = intern_downcase (logical_font->elfLogFont.lfFaceName,
                                        strlen (logical_font->elfLogFont.lfFaceName));
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}

/* Convert an enumerated Windows font to an Emacs font entity.  */
Lisp_Object w32_enumfont_pattern_entity (ENUMLOGFONTEX *logical_font,
                                         NEWTEXTMETRICEX *physical_font,
                                         DWORD font_type)
{
  Lisp_Object entity, tem;
  LOGFONT *lf = (LOGFONT*) logical_font;
  BYTE generic_type;

  entity = Fmake_vector (make_number (FONT_ENTITY_MAX), null_string);

  ASET (entity, FONT_TYPE_INDEX, Qw32);
  ASET (entity, FONT_REGISTRY_INDEX, w32_registry (lf->lfCharSet));
  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

  /* Foundry is difficult to get in readable form on Windows.
     But Emacs crashes if it is not set, so set it to the generic type.  */
  generic_type = physical_font->ntmTm.tmPitchAndFamily & 0xF0;
  if (generic_type == FF_DECORATIVE)
    tem = Qdecorative;
  else if (generic_type == FF_MODERN)
    tem = Qmodern;
  else if (generic_type == FF_ROMAN)
    tem = Qroman;
  else if (generic_type == FF_SCRIPT)
    tem = Qscript;
  else if (generic_type == FF_SWISS)
    tem = Qswiss;
  else
    tem = Qunknown;

  ASET (entity, FONT_FOUNDRY_INDEX, tem);

  ASET (entity, FONT_FAMILY_INDEX,
        intern_downcase (lf->lfFaceName, strlen (lf->lfFaceName)));

  ASET (entity, FONT_WEIGHT_INDEX, make_number (lf->lfWeight));
  ASET (entity, FONT_SLANT_INDEX, make_number (lf->lfItalic ? 200 : 100));
  ASET (entity, FONT_WIDTH_INDEX,
        make_number (physical_font->ntmTm.tmAveCharWidth));

  if (font_type & RASTER_FONTTYPE)
    ASET (entity, FONT_SIZE_INDEX, make_number (physical_font->ntmTm.tmHeight));
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));

  /* Cache unicode codepoints covered by this font, as there is no other way
     of getting this information easily.  */
  if (font_type & TRUETYPE_FONTTYPE)
    {
      DWORD *subranges = xmalloc(16);
      memcpy (subranges, physical_font->ntmFontSig.fsUsb, 16);
      ASET (entity, FONT_EXTRA_INDEX, make_save_value (subranges, 0));
    }
  return entity;
}

/* Callback function for EnumFontFamiliesEx.
 * Adds the name of a font to a Lisp list (passed in as the lParam arg).  */
static int CALLBACK add_font_entity_to_list (ENUMLOGFONTEX *logical_font,
                                             NEWTEXTMETRICEX *physical_font,
                                             DWORD font_type,
                                             LPARAM list_object)
{
  Lisp_Object *list = (Lisp_Object *) list_object;
  Lisp_Object entity = w32_enumfont_pattern_entity (logical_font,
                                                    physical_font, font_type);
  if (!NILP (entity))
    *list = Fcons (entity, *list);

  return 1;
}

/* Callback function for EnumFontFamiliesEx.
 * Adds the name of a font to a Lisp list (passed in as the lParam arg),
 * then terminate the search.  */
static int CALLBACK add_one_font_entity_to_list (ENUMLOGFONTEX *logical_font,
                                                 NEWTEXTMETRICEX *physical_font,
                                                 DWORD font_type,
                                                 LPARAM list_object)
{
  add_font_entity_to_list (logical_font, physical_font, font_type, list_object);
  return 0;
}

/* Convert a Lisp font registry (symbol) to a windows charset.  */
static LONG registry_to_w32_charset (Lisp_Object charset)
{
  if (EQ (charset, Qiso10646_1) || EQ (charset, Qunicode_bmp)
      || EQ (charset, Qunicode_sip))
    return DEFAULT_CHARSET; /* UNICODE_CHARSET not defined in MingW32 */
  else if (EQ (charset, Qiso8859_1))
    return ANSI_CHARSET;
  else if (SYMBOLP (charset))
    return x_to_w32_charset (SDATA (SYMBOL_NAME (charset)));
  else if (STRINGP (charset))
    return x_to_w32_charset (SDATA (charset));
  else
    return DEFAULT_CHARSET;
}

static Lisp_Object w32_registry (LONG w32_charset)
{
  if (w32_charset == ANSI_CHARSET)
    return Qiso8859_1;
  else
    {
      char * charset = w32_to_x_charset (w32_charset, NULL);
      return intern_downcase (charset, strlen(charset));
    }
}

static void set_fonts_frame (Lisp_Object fontlist, Lisp_Object frame)
{
  if (VECTORP (fontlist))
    ASET (fontlist, FONT_FRAME_INDEX, frame);
  else
    {
      for ( ; CONSP (fontlist); fontlist = XCDR (fontlist))
        {
          Lisp_Object entity = XCAR (fontlist);
          if (VECTORP (entity))
            ASET (entity, FONT_FRAME_INDEX, frame);
        }
    }
}

/* Fill in all the available details of LOGFONT from FONT_SPEC.  */
static void fill_in_logfont (FRAME_PTR f, LOGFONT *logfont, Lisp_Object font_spec)
{
  Lisp_Object val, tmp, extra;
  int dpi = FRAME_W32_DISPLAY_INFO (f)->resy;

  /* TODO: Allow user to override dpi settings.  */

  /* Height  */
  tmp = AREF (font_spec, FONT_SIZE_INDEX);
  if (INTEGERP (tmp))
    logfont->lfHeight = -1 * XINT (tmp);
  else if (FLOATP (tmp))
    logfont->lfHeight = (int) (-1.0 *  dpi * XFLOAT_DATA (tmp) / 72.0);

  /* Width  TODO: makes fonts look distorted.
  tmp = AREF (font_spec, FONT_WIDTH_INDEX);
  if (INTEGERP (tmp))
    logfont->lfWidth = XINT (tmp);
 */

  /* Escapement  */

  /* Orientation  */

  /* Weight  */
  tmp = AREF (font_spec, FONT_WEIGHT_INDEX);
  if (INTEGERP (tmp))
    logfont->lfWeight = XINT (tmp);

  /* Italic  */
  tmp = AREF (font_spec, FONT_SLANT_INDEX);
  if (INTEGERP (tmp))
    {
      int slant = XINT (tmp);
      logfont->lfItalic = slant > 150 ? 1 : 0;
    }

  /* Underline  */

  /* Strikeout  */

  /* Charset  */
  tmp = AREF (font_spec, FONT_REGISTRY_INDEX);
  if (! NILP (tmp))
    {
      if (STRINGP (tmp))
        logfont->lfCharSet = x_to_w32_charset (SDATA (tmp));
      else
        logfont->lfCharSet = registry_to_w32_charset (tmp);
    }

  /* Out Precision  */
  /* Clip Precision  */
  /* Quality  TODO: Allow different quality to be specified, so user
     can enable/disable anti-aliasing for individual fonts.  */
  logfont->lfQuality = DEFAULT_QUALITY;

  /* Pitch and Family  */
  /* Facename  TODO: handle generic names  */
  tmp = AREF (font_spec, FONT_FAMILY_INDEX);
  /* Font families are interned  */
  if (SYMBOLP (tmp))
    strncpy (logfont->lfFaceName, SDATA (SYMBOL_NAME (tmp)), LF_FACESIZE);
  else if (STRINGP (tmp))
    strncpy (logfont->lfFaceName, SDATA (tmp), LF_FACESIZE);

}

static void list_all_matching_fonts (Lisp_Object frame,
                                     LOGFONT *font_match_pattern,
                                     Lisp_Object* list)
{
  HDC dc;
  Lisp_Object families = w32font_list_family (frame);
  struct frame *f = XFRAME (frame);

  dc = get_frame_dc (f);

  while (!NILP (families))
    {
      Lisp_Object family = CAR (families);
      families = CDR (families);
      if (STRINGP (family))
        {
          /* TODO: Use the Unicode versions of the W32 APIs, so we can
             handle non-ASCII font names.  */
          char * name = SDATA (family);
          strncpy (font_match_pattern->lfFaceName, name, LF_FACESIZE);
          font_match_pattern->lfFaceName[LF_FACESIZE - 1] = '\0';

          EnumFontFamiliesEx (dc, font_match_pattern,
                              (FONTENUMPROC) add_font_entity_to_list,
                              (LPARAM)&list, 0);
        }
    }

  release_frame_dc (f, dc);
}

static int unicode_range_for_char (unsigned c)
{
  /* Is there really no Windows API function for this?!!! */
  if (c < 0x80)
    return 0;              // Basic Latin
  else if (c < 0x100)
    return 1;              // Latin-1 supplement
  else if (c < 0x180)
    return 2;              // Latin Extended-A
  else if (c < 0x250)
    return 3;              // Latin Extended-B
  else if (c < 0x2B0)
    return 4;              // IPA Extensions
  else if (c < 0x300)
    return 5;              // Spacing modifiers
  else if (c < 0x370)
    return 6;              // Combining diacritical marks
  else if (c < 0x400)
    return 7;              // Greek and Coptic
  else if (c < 0x530)
    return 9;              // Cyrillic, Cyrillic supplementary
  else if (c < 0x590)
    return 10;             // Armenian
  else if (c < 0x600)
    return 11;             // Hebrew
  else if (c < 0x700)
    return 13;             // Arabic
  else if (c < 0x750)
    return 71;             // Syriac
  else if (c < 0x780)
    return 13;             // Arabic supplement
  else if (c < 0x7c0)
    return 72;             // Thaana
  else if (c < 0x800)
    return 14;             // N'Ko
  else if (c < 0x900)
    return -1;             // Unsupported range
  else if (c < 0x980)
    return 15;             // Devanagari
  else if (c < 0xA00)
    return 16;             // Bengali
  else if (c < 0xA80)
    return 17;             // Gurmukhi
  else if (c < 0xB00)
    return 18;             // Gujarati
  else if (c < 0xB80)
    return 19;             // Oriya
  else if (c < 0xC00)
    return 20;             // Tamil
  else if (c < 0xC80)
    return 21;             // Telugu
  else if (c < 0xD00)
    return 22;             // Kannada
  else if (c < 0xD80)
    return 23;             // Malayalam
  else if (c < 0xE00)
    return 73;             // Sinhala
  else if (c < 0xE80)
    return 24;             // Thai
  else if (c < 0xF00)
    return 25;             // Lao
  else if (c < 0x1000)
    return 70;             // Tibetan
  else if (c < 0x10A0)
    return 74;             // Myanmar
  else if (c < 0x1100)
    return 26;             // Georgian
  else if (c < 0x1200)
    return 28;             // Hangul Jamo
  else if (c < 0x13A0)
    return 75;             // Ethiopic, Ethiopic Supplement
  else if (c < 0x1400)
    return 76;             // Cherokee
  else if (c < 0x1680)
    return 77;             // Unified Canadian Aboriginal Syllabics
  else if (c < 0x16A0)
    return 78;             // Ogham
  else if (c < 0x1700)
    return 79;             // Runic
  else if (c < 0x1780)
    return 84;             // Tagalog, Hanunoo, Buhid, Tagbanwa
  else if (c < 0x1800)
    return 80;             // Khmer
  else if (c < 0x18B0)
    return 81;             // Mongolian
  else if (c < 0x1900)
    return -1;             // Unsupported range
  else if (c < 0x1950)
    return 93;             // Limbu
  else if (c < 0x1980)
    return 94;             // Tai Le
  else if (c < 0x19E0)
    return 95;             // New Tai Le
  else if (c < 0x1A00)
    return 80;             // Khmer Symbols
  else if (c < 0x1A20)
    return 96;             // Buginese
  else if (c < 0x1B00)
    return -1;             // Unsupported range
  else if (c < 0x1B80)
    return 27;             // Balinese
  else if (c < 0x1D00)
    return -1;             // Unsupported range
  else if (c < 0x1DC0)
    return 4;              // Phonetic extensions + supplement
  else if (c < 0x1E00)
    return 6;              // Combining diacritical marks supplement
  else if (c < 0x1F00)
    return 29;             // Latin Extended additional
  else if (c < 0x2000)
    return 30;             // Greek Extended
  else if (c < 0x2070)
    return 31;             // General Punctuation
  else if (c < 0x20A0)
    return 32;             // Subscripts and Superscripts
  else if (c < 0x20D0)
    return 33;             // Currency symbols
  else if (c < 0x2100)
    return 34;             // Combining marks for diacriticals
  else if (c < 0x2150)
    return 35;             // Letterlike symbols
  else if (c < 0x2190)
    return 36;             // Number forms
  else if (c < 0x2200)
    return 37;             // Arrows
  else if (c < 0x2300)
    return 38;             // Mathematical operators
  else if (c < 0x2400)
    return 39;             // Miscellaneous technical
  else if (c < 0x2440)
    return 40;             // Control pictures
  else if (c < 0x2460)
    return 41;             // Optical character recognition
  else if (c < 0x2500)
    return 42;             // Enclosed alphanumerics
  else if (c < 0x2580)
    return 43;             // Box drawing
  else if (c < 0x25A0)
    return 44;             // Block elements
  else if (c < 0x2600)
    return 45;             // Geometric shapes
  else if (c < 0x2700)
    return 46;             // Miscellaneous symbols
  else if (c < 0x27C0)
    return 47;             // Dingbats
  else if (c < 0x27F0)
    return 38;             // Misc Math symbols-A
  else if (c < 0x2800)
    return 37;             // Supplemental arrows-A
  else if (c < 0x2900)
    return 82;             // Braille patterns
  else if (c < 0x2980)
    return 37;             // Supplemental arrows-B
  else if (c < 0x2B00)
    return 38;             // Misc Math symbols-B, Supplemental Math operators
  else if (c < 0x2C00)
    return 37;             // Misc Symbols and Arrows
  else if (c < 0x2C60)
    return 97;             // Galgolitic
  else if (c < 0x2C80)
    return 29;             // Latin Extended-C
  else if (c < 0x2D00)
    return 8;              // Coptic
  else if (c < 0x2D30)
    return 26;             // Georgian supplement
  else if (c < 0x2D80)
    return 98;             // Tifinagh
  else if (c < 0x2DE0)
    return 75;             // Ethiopic extended
  else if (c < 0x2E00)
    return -1;             // Unsupported range
  else if (c < 0x2E80)
    return 31;             // Supplemental punctuation
  else if (c < 0x2FE0)
    return 59;             // CJK radicals supplement, Kangxi radicals
  else if (c < 0x2FF0)
    return -1;             // Unsupported range
  else if (c < 0x3000)
    return 59;             // Ideographic description characters
  else if (c < 0x3040)
    return 48;             // CJK symbols and punctuation
  else if (c < 0x30A0)
    return 49;             // Hiragana
  else if (c < 0x3100)
    return 50;             // Katakana
  else if (c < 0x3130)
    return 51;             // Bopomofo
  else if (c < 0x3190)
    return 52;             // Hangul compatibility Jamo
  else if (c < 0x31A0)
    return 59;             // Kanbun
  else if (c < 0x31C0)
    return 51;             // Bopomofo extended
  else if (c < 0x31F0)
    return 61;             // CJK strokes
  else if (c < 0x3200)
    return 50;             // Katakana phonetic extensions
  else if (c < 0x3300)
    return 54;             // CJK enclosed letters and months
  else if (c < 0x3400)
    return 55;             // CJK compatibility
  else if (c < 0x4DC0)
    return 59;             // CJK unified ideographs extension-A
  else if (c < 0x4E00)
    return 99;             // Yijing Hexagram Symbols
  else if (c < 0xA000)
    return 59;             // CJK unified ideographs
  else if (c < 0xA4D0)
    return 83;             // Yi syllables, Yi radicals
  else if (c < 0xA700)
    return -1;             // Unsupported range
  else if (c < 0xA720)
    return 5;              // Modifier tone letters
  else if (c < 0xA800)
    return 29;             // Latin Extended-D
  else if (c < 0xA830)
    return 100;            // Syloti Nagri
  else if (c < 0xA840)
    return -1;             // Unsupported range
  else if (c < 0xA880)
    return 53;             // Phags-pa
  else if (c < 0xAC00)
    return -1;             // Unsupported range
  else if (c < 0xD7A4)
    return 56;             // Hangul syllables
  else if (c < 0xD800)
    return -1;             // Unsupported range
  else if (c < 0xE000)
    return 57;             // Surrogates
  else if (c < 0xF900)
    return 60;             // Private use (plane 0)
  else if (c < 0xFB00)
    return 61;             // CJK Compatibility ideographs
  else if (c < 0xFB50)
    return 62;             // Alphabetic Presentation Forms
  else if (c < 0xFE00)
    return 63;             // Arabic Presentation Forms-A
  else if (c < 0xFE10)
    return 91;             // Variation selectors
  else if (c < 0xFE20)
    return 65;             // Vertical forms
  else if (c < 0xFE30)
    return 64;             // Combining half marks
  else if (c < 0xFE50)
    return 65;             // CJK compatibility forms
  else if (c < 0xFE70)
    return 66;             // Small form variants
  else if (c < 0xFEFF)
    return 67;             // Arabic Presentation Forms-B
  else if (c == 0xFEFF)
    return -1;             // Unsupported range
  else if (c < 0xFFF0)
    return 68;             // Halfwidth and fullwidth forms
  else if (c <= 0xFFFF)
    return 69;             // Specials

  // If int is 64 bit, it could represent characters from 10000 up, but 
  // any font that handles them should have the surrogate bit set (57).
  return 57;
}


struct font_driver w32font_driver =
  {
    0, /* Qw32 */
    w32font_get_cache,
    w32font_list,
    w32font_match,
    w32font_list_family,
    NULL, /* free_entity */
    w32font_open,
    w32font_close,
    NULL, /* prepare_face */
    NULL, /* done_face */
    w32font_has_char,
    w32font_encode_char,
    w32font_text_extents,
    w32font_draw,
    NULL, /* get_bitmap */
    NULL, /* free_bitmap */
    NULL, /* get_outline */
    NULL, /* free_outline */
    NULL, /* anchor_point */
    NULL, /* otf_capability */
    NULL /* otf_drive */
  };

/* Initialize the font subsystem for the environment on which
   Emacs is running.   */
void w32font_initialize ()
{
  /* Load functions that might not exist on older versions of Windows.  */
  HANDLE gdi = LoadLibrary ("gdi32.dll");

  get_glyph_indices_fn
    = (GETGLYPHINDICES) GetProcAddress (gdi, "GetGlyphIndicesW");
  get_text_extent_pointi_fn
    = (GETTEXTEXTENTPTI) GetProcAddress (gdi, "GetTextExtentPoint32W");
}

/* Initialize state that does not change between invocations. This is only
   called when Emacs is dumped.  */
void syms_of_w32font ()
{
  DEFSYM (Qw32, "w32");
  DEFSYM (Qdecorative, "decorative");
  DEFSYM (Qmodern, "modern");
  DEFSYM (Qroman, "roman");
  DEFSYM (Qscript, "script");
  DEFSYM (Qswiss, "swiss");
  DEFSYM (Qunknown, "unknown");

  w32font_driver.type = Qw32;
  register_font_driver (&w32font_driver, NULL);
}
