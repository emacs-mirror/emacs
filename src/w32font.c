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
};

extern struct font_driver w32font_driver;

Lisp_Object Qgdi, QCfamily;
static Lisp_Object Qmonospace, Qsans_serif, Qserif, Qmono, Qsans, Qsans__serif;
static Lisp_Object Qscript, Qdecorative, Qraster, Qoutline, Qunknown;

/* scripts */
static Lisp_Object Qlatin, Qgreek, Qcoptic, Qcyrillic, Qarmenian, Qhebrew;
static Lisp_Object Qarabic, Qsyriac, Qnko, Qthaana, Qdevanagari, Qbengali;
static Lisp_Object Qgurmukhi, Qgujarati, Qoriya, Qtamil, Qtelugu;
static Lisp_Object Qkannada, Qmalayalam, Qsinhala, Qthai, Qlao;
static Lisp_Object Qtibetan, Qmyanmar, Qgeorgian, Qhangul, Qethiopic;
static Lisp_Object Qcherokee, Qcanadian_aboriginal, Qogham, Qrunic;
static Lisp_Object Qkhmer, Qmongolian, Qsymbol, Qbraille, Qhan;
static Lisp_Object Qideographic_description, Qcjk_misc, Qkana, Qbopomofo;
static Lisp_Object Qkanbun, Qyi, Qbyzantine_musical_symbol;
static Lisp_Object Qmusical_symbol, Qmathematical;

/* Font spacing symbols - defined in font.c.  */
extern Lisp_Object Qc, Qp, Qm;

static void fill_in_logfont P_ ((FRAME_PTR f, LOGFONT *logfont,
                                 Lisp_Object font_spec));

static Lisp_Object font_supported_scripts P_ ((FONTSIGNATURE * sig));

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

/* struct passed in as LPARAM arg to EnumFontFamiliesEx, for keeping track
   of what we really want.  */
struct font_callback_data
{
  /* The logfont we are matching against. EnumFontFamiliesEx only matches
     face name and charset, so we need to manually match everything else
     in the callback function.  */
  LOGFONT pattern;
  /* The original font spec or entity.  */
  Lisp_Object orig_font_spec;
  /* The frame the font is being loaded on.  */
  Lisp_Object frame;
  /* The list to add matches to.  */
  Lisp_Object list;
};

/* Handles the problem that EnumFontFamiliesEx will not return all
   style variations if the font name is not specified.  */
static void list_all_matching_fonts P_ ((struct font_callback_data *match_data));


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
static Lisp_Object
w32font_get_cache (frame)
     Lisp_Object frame;
{
  struct w32_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (XFRAME (frame));

  return (dpyinfo->name_list_element);
}

/* w32 implementation of list for font backend.
   List fonts exactly matching with FONT_SPEC on FRAME.  The value
   is a vector of font-entities.  This is the sole API that
   allocates font-entities.  */
static Lisp_Object
w32font_list (frame, font_spec)
     Lisp_Object frame, font_spec;
{
  Lisp_Object tem;
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.list = Qnil;
  match_data.frame = frame;
  bzero (&match_data.pattern, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  if (match_data.pattern.lfFaceName[0] == '\0')
    {
      /* EnumFontFamiliesEx does not take other fields into account if
         font name is blank, so need to use two passes.  */
      list_all_matching_fonts (&match_data);
    }
  else
    {
      dc = get_frame_dc (f);

      EnumFontFamiliesEx (dc, &match_data.pattern,
                          (FONTENUMPROC) add_font_entity_to_list,
                          (LPARAM) &match_data, 0);
      release_frame_dc (f, dc);
    }

  return NILP (match_data.list) ? null_vector : Fvconcat (1, &match_data.list);
}

/* w32 implementation of match for font backend.
   Return a font entity most closely matching with FONT_SPEC on
   FRAME.  The closeness is detemined by the font backend, thus
   `face-font-selection-order' is ignored here.  */
static Lisp_Object
w32font_match (frame, font_spec)
     Lisp_Object frame, font_spec;
{
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.frame = frame;
  match_data.list = Qnil;
  bzero (&match_data.pattern, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &match_data.pattern,
                      (FONTENUMPROC) add_one_font_entity_to_list,
                      (LPARAM) &match_data, 0);
  release_frame_dc (f, dc);

  return NILP (match_data.list) ? Qnil : XCAR (match_data.list);
}


/* w32 implementation of list_family for font backend.
   List available families.  The value is a list of family names
   (symbols).  */
static Lisp_Object
w32font_list_family (frame)
     Lisp_Object frame;
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
static struct font *
w32font_open (f, font_entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object font_entity;
     int pixel_size;
{
  int len, size;
  LOGFONT logfont;
  HDC dc;
  HFONT hfont, old_font;
  Lisp_Object val, extra;
  /* For backwards compatibility.  */
  W32FontStruct *compat_w32_font;

  struct w32font_info *w32_font = xmalloc (sizeof (struct w32font_info));

  struct font * font = (struct font *) w32_font;
  if (!font)
    return NULL;

  bzero (&logfont, sizeof (logfont));
  fill_in_logfont (f, &logfont, font_entity);

  size = XINT (AREF (font_entity, FONT_SIZE_INDEX));
  if (!size)
    size = pixel_size;

  logfont.lfHeight = -size;
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
  font->format = Qgdi;
  font->file_name = NULL;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  font->min_width = 0;
  font->ascent = w32_font->metrics.tmAscent;
  font->descent = w32_font->metrics.tmDescent;
  font->scalable = w32_font->metrics.tmPitchAndFamily & TMPF_VECTOR;

  return font;
}

/* w32 implementation of close for font_backend.
   Close FONT on frame F.  */
static void
w32font_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  if (font->font.font)
    {
      W32FontStruct *old_w32_font = (W32FontStruct *)font->font.font;
      DeleteObject (old_w32_font->hfont);
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
static int
w32font_has_char (entity, c)
     Lisp_Object entity;
     int c;
{
  Lisp_Object supported_scripts, extra, script;
  DWORD mask;

  extra = AREF (entity, FONT_EXTRA_INDEX);
  if (!CONSP (extra))
    return -1;

  supported_scripts = assq_no_quit (QCscript, extra);
  if (!CONSP (supported_scripts))
    return -1;

  supported_scripts = XCDR (supported_scripts);

  script = CHAR_TABLE_REF (Vchar_script_table, c);

  return (memq_no_quit (script, supported_scripts)) ? 1 : 0;
}

/* w32 implementation of encode_char for font backend.
   Return a glyph code of FONT for characer C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
static unsigned
w32font_encode_char (font, c)
     struct font *font;
     int c;
{
  /* Avoid unneccesary conversion - all the Win32 APIs will take a unicode
     character.  */
  return c;
}

/* w32 implementation of text_extents for font backend.
   Perform the size computation of glyphs of FONT and fillin members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS).  Apparently medtrics can be NULL, in this
   case just return the overall width.  */
static int
w32font_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  int i;
  HFONT old_font;
  /* FIXME: Be nice if we had a frame here, rather than getting the desktop's
     device context to measure against... */
  HDC dc = GetDC (NULL);
  int total_width = 0;

  /* TODO: Allow some extra room for surrogates. */
  WORD *wcode = alloca(nglyphs * sizeof (WORD));
  SIZE size;

  old_font = SelectObject (dc, ((W32FontStruct *)(font->font.font))->hfont);

  if (metrics)
    {
      GLYPHMETRICS gm;
      MAT2 transform;
      int i, width;
      UINT format = GGO_METRICS;

      /* Set transform to the identity matrix.  */
      bzero (&transform, sizeof (transform));
      transform.eM11.value = 1;
      transform.eM22.value = 1;

      for (i = 0; i < nglyphs; i++)
        {
	  if (code[i] < 0x10000)
	    wcode[i] = code[i];
	  else
	    {
	      /* TODO: Convert to surrogate, reallocating array if needed */
	      wcode[i] = 0xffff;
	    }

          if (GetGlyphOutlineW (dc, *(code + i), format, &gm, 0,
                                NULL, &transform) != GDI_ERROR)
            {
              metrics[i].lbearing = gm.gmptGlyphOrigin.x;
              metrics[i].rbearing = gm.gmptGlyphOrigin.x + gm.gmBlackBoxX;
              metrics[i].width = gm.gmCellIncX;
              metrics[i].ascent = -gm.gmptGlyphOrigin.y;
              metrics[i].descent = gm.gmBlackBoxY + gm.gmptGlyphOrigin.y;
            }
          else if (GetTextExtentPoint32W (dc, wcode + i, 1, &size)
                   != GDI_ERROR)
            {
              metrics[i].lbearing = 0;
              metrics[i].rbearing = size.cx
                + ((struct w32font_info *) font)->metrics.tmOverhang;
              metrics[i].width = size.cx;
              metrics[i].ascent = font->ascent;
              metrics[i].descent = font->descent;
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
  else
    {
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
    }

  if (GetTextExtentPoint32W (dc, wcode, nglyphs, &size))
    {
      total_width = size.cx;
    }

  if (!total_width)
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
static int
w32font_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  UINT options = 0;

  if (with_background)
    {
      HBRUSH brush;
      RECT rect;

      brush = CreateSolidBrush (s->gc->background);
      rect.left = x;
      rect.top = y - ((struct font *) (s->font_info->font))->ascent;
      rect.right = x + s->width;
      rect.bottom = y + ((struct font *) (s->font_info->font))->descent;
      FillRect (s->hdc, &rect, brush);
      DeleteObject (brush);
    }
  else
    SetBkMode (s->hdc, TRANSPARENT);

  ExtTextOutW (s->hdc, x, y, options, NULL, s->char2b + from, to - from, NULL);
}

/* w32 implementation of free_entity for font backend.
   Optional (if FONT_EXTRA_INDEX is not Lisp_Save_Value).
   Free FONT_EXTRA_INDEX field of FONT_ENTITY.
static void
w32font_free_entity (Lisp_Object entity);
  */

/* w32 implementation of prepare_face for font backend.
   Optional (if FACE->extra is not used).
   Prepare FACE for displaying characters by FONT on frame F by
   storing some data in FACE->extra.  If successful, return 0.
   Otherwise, return -1.
static int
w32font_prepare_face (FRAME_PTR f, struct face *face);
  */
/* w32 implementation of done_face for font backend.
   Optional.
   Done FACE for displaying characters by FACE->font on frame F.
static void
w32font_done_face (FRAME_PTR f, struct face *face);  */

/* w32 implementation of get_bitmap for font backend.
   Optional.
   Store bitmap data for glyph-code CODE of FONT in BITMAP.  It is
   intended that this method is called from the other font-driver
   for actual drawing.
static int
w32font_get_bitmap (struct font *font, unsigned code,
                    struct font_bitmap *bitmap, int bits_per_pixel);
  */
/* w32 implementation of free_bitmap for font backend.
   Optional.
   Free bitmap data in BITMAP.
static void
w32font_free_bitmap (struct font *font, struct font_bitmap *bitmap);
  */
/* w32 implementation of get_outline for font backend.
   Optional.
   Return an outline data for glyph-code CODE of FONT.  The format
   of the outline data depends on the font-driver.
static void *
w32font_get_outline (struct font *font, unsigned code);
  */
/* w32 implementation of free_outline for font backend.
   Optional.
   Free OUTLINE (that is obtained by the above method).
static void
w32font_free_outline (struct font *font, void *outline);
  */
/* w32 implementation of anchor_point for font backend.
   Optional.
   Get coordinates of the INDEXth anchor point of the glyph whose
   code is CODE.  Store the coordinates in *X and *Y.  Return 0 if
   the operations was successfull.  Otherwise return -1.
static int
w32font_anchor_point (struct font *font, unsigned code,
                                 int index, int *x, int *y);
  */
/* w32 implementation of otf_capability for font backend.
   Optional.
   Return a list describing which scripts/languages FONT
   supports by which GSUB/GPOS features of OpenType tables.
static Lisp_Object
w32font_otf_capability (struct font *font);
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
static int
w32font_otf_drive (struct font *font, Lisp_Object features,
                   Lisp_Object gstring_in, int from, int to,
                   Lisp_Object gstring_out, int idx,
                   int alternate_subst);
  */

/* Callback function for EnumFontFamiliesEx.
 * Adds the name of a font to a Lisp list (passed in as the lParam arg).  */
static int CALLBACK
add_font_name_to_list (logical_font, physical_font, font_type, list_object)
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
     LPARAM list_object;
{
  Lisp_Object* list = (Lisp_Object *) list_object;
  Lisp_Object family = intern_downcase (logical_font->elfLogFont.lfFaceName,
                                        strlen (logical_font->elfLogFont.lfFaceName));
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}

/* Convert an enumerated Windows font to an Emacs font entity.  */
static Lisp_Object
w32_enumfont_pattern_entity (frame, logical_font, physical_font, font_type)
     Lisp_Object frame;
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
{
  Lisp_Object entity, tem;
  LOGFONT *lf = (LOGFONT*) logical_font;
  BYTE generic_type;

  entity = Fmake_vector (make_number (FONT_ENTITY_MAX), Qnil);

  ASET (entity, FONT_TYPE_INDEX, Qgdi);
  ASET (entity, FONT_FRAME_INDEX, frame);
  ASET (entity, FONT_REGISTRY_INDEX, w32_registry (lf->lfCharSet));
  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

  /* Foundry is difficult to get in readable form on Windows.
     But Emacs crashes if it is not set, so set it to something more
     generic.  Thes values make xflds compatible with Emacs 22. */
  if (lf->lfOutPrecision == OUT_STRING_PRECIS)
    tem = Qraster;
  else if (lf->lfOutPrecision == OUT_STROKE_PRECIS)
    tem = Qoutline;
  else
    tem = Qunknown;

  ASET (entity, FONT_FOUNDRY_INDEX, tem);

  /* Save the generic family in the extra info, as it is likely to be
     useful to users looking for a close match.  */
  generic_type = physical_font->ntmTm.tmPitchAndFamily & 0xF0;
  if (generic_type == FF_DECORATIVE)
    tem = Qdecorative;
  else if (generic_type == FF_MODERN)
    tem = Qmonospace;
  else if (generic_type == FF_ROMAN)
    tem = Qserif;
  else if (generic_type == FF_SCRIPT)
    tem = Qscript;
  else if (generic_type == FF_SWISS)
    tem = Qsans_serif;
  else
    tem = Qnil;
    
  if (! NILP (tem))
    font_put_extra (entity, QCfamily, tem);


  if (physical_font->ntmTm.tmPitchAndFamily & 0x01)
    font_put_extra (entity, QCspacing, make_number (FONT_SPACING_PROPORTIONAL));
  else
    font_put_extra (entity, QCspacing, make_number (FONT_SPACING_MONO));

  ASET (entity, FONT_FAMILY_INDEX,
        intern_downcase (lf->lfFaceName, strlen (lf->lfFaceName)));

  ASET (entity, FONT_WEIGHT_INDEX, make_number (lf->lfWeight));
  ASET (entity, FONT_SLANT_INDEX, make_number (lf->lfItalic ? 200 : 100));
  /* TODO: PANOSE struct has this info, but need to call GetOutlineTextMetrics
     to get it.  */
  ASET (entity, FONT_WIDTH_INDEX, make_number (100));

  if (font_type & RASTER_FONTTYPE)
    ASET (entity, FONT_SIZE_INDEX, make_number (physical_font->ntmTm.tmHeight));
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));

  /* Cache unicode codepoints covered by this font, as there is no other way
     of getting this information easily.  */
  if (font_type & TRUETYPE_FONTTYPE)
    {
      font_put_extra (entity, QCscript,
                      font_supported_scripts (&physical_font->ntmFontSig));
    }

  return entity;
}


/* Convert generic families to the family portion of lfPitchAndFamily.  */
BYTE
w32_generic_family (Lisp_Object name)
{
  /* Generic families.  */
  if (EQ (name, Qmonospace) || EQ (name, Qmono))
    return FF_MODERN;
  else if (EQ (name, Qsans_serif) || EQ (name, Qsans__serif)
           || EQ (name, Qsans))
    return FF_SWISS;
  else if (EQ (name, Qserif))
    return FF_ROMAN;
  else if (EQ (name, Qdecorative))
    return FF_DECORATIVE;
  else if (EQ (name, Qscript))
    return FF_SCRIPT;
  else
    return FF_DONTCARE;
}

static int
logfonts_match (font, pattern)
     LOGFONT *font, *pattern;
{
  /* Only check height for raster fonts.  */
  if (pattern->lfHeight && font->lfOutPrecision == OUT_STRING_PRECIS
      && font->lfHeight != pattern->lfHeight)
    return 0;

  /* Have some flexibility with weights.  */
  if (pattern->lfWeight
      && ((font->lfWeight < (pattern->lfWeight - 150))
          || font->lfWeight > (pattern->lfWeight + 150)))
      return 0;

  /* Charset and face should be OK.  Italic has to be checked
     against the original spec, in case we don't have any preference.  */
  return 1;
}

static int
font_matches_spec (type, font, spec)
     DWORD type;
     NEWTEXTMETRICEX *font;
     Lisp_Object spec;
{
  Lisp_Object extra, val;

  /* Check italic. Can't check logfonts, since it is a boolean field,
     so there is no difference between "non-italic" and "don't care".  */
  val = AREF (spec, FONT_SLANT_INDEX);
  if (INTEGERP (val))
    {
      int slant = XINT (val);
      if ((slant > 150 && !font->ntmTm.tmItalic)
          || (slant <= 150 && font->ntmTm.tmItalic))
        {
          OutputDebugString ("italic mismatch");
        return 0;
        }
    }

  /* Check extra parameters.  */
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object extra_entry;
      extra_entry = XCAR (extra);
      if (CONSP (extra_entry))
        {
          Lisp_Object key = XCAR (extra_entry);
          val = XCDR (extra_entry);
          if (EQ (key, QCfamily))
            {
              /* Generic family. Most useful when there is no font name
                 specified. eg, if a script does not exist in the default
                 font, we could look for a font with the same generic family
                 that does support the script. Full PANOSE support would
                 be better, but we need to open the font to get that.  */
              BYTE w32_family = w32_generic_family (val);

              /* Reject if FF_DONTCARE is returned, as it means the
                 font spec is bad.  */
              if (w32_family == FF_DONTCARE
                  || w32_family != (font->ntmTm.tmPitchAndFamily & 0xF0))
                return 0;
            }
          else if (EQ (key, QCspacing))
            {
              int proportional;
              if (INTEGERP (val))
                {
                  int spacing = XINT (val);
                  proportional = (spacing < FONT_SPACING_MONO);
                }
              else if (EQ (val, Qp))
                proportional = 1;
              else if (EQ (val, Qc) || EQ (val, Qm))
                proportional = 0;
              else
                return 0; /* Bad font spec.  */

              if ((proportional && !(font->ntmTm.tmPitchAndFamily & 0x01))
                  || (!proportional && (font->ntmTm.tmPitchAndFamily & 0x01)))
                return 0;
            }
          else if (EQ (key, QCscript) && SYMBOLP (val))
            {
              /* Only truetype fonts will have information about what
                 scripts they support.  This probably means the user
                 will have to force Emacs to use raster, postscript
                 or atm fonts for non-ASCII text.  */
              if (type & TRUETYPE_FONTTYPE)
                {
                  Lisp_Object support
                    = font_supported_scripts (&font->ntmFontSig);
                  if (! memq_no_quit (val, support))
                    return 0;
                }
              else
                {
                  /* Return specific matches, but play it safe. Fonts
                     that cover more than their charset would suggest
                     are likely to be truetype or opentype fonts,
                     covered above.  */
                  if (EQ (val, Qlatin))
                    {
                      /* Although every charset but symbol, thai and
                         arabic contains the basic ASCII set of latin
                         characters, Emacs expects much more.  */
                      if (font->ntmTm.tmCharSet != ANSI_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qsymbol))
                    {
                      if (font->ntmTm.tmCharSet != SYMBOL_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qcyrillic))
                    {
                      if (font->ntmTm.tmCharSet != RUSSIAN_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qgreek))
                    {
                      if (font->ntmTm.tmCharSet != GREEK_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qarabic))
                    {
                      if (font->ntmTm.tmCharSet != ARABIC_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhebrew))
                    {
                      if (font->ntmTm.tmCharSet != HEBREW_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qthai))
                    {
                      if (font->ntmTm.tmCharSet != THAI_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qkana))
                    {
                      if (font->ntmTm.tmCharSet != SHIFTJIS_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qbopomofo))
                    {
                      if (font->ntmTm.tmCharSet != CHINESEBIG5_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhangul))
                    {
                      if (font->ntmTm.tmCharSet != HANGUL_CHARSET
                          && font->ntmTm.tmCharSet != JOHAB_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhan))
                    {
                      if (font->ntmTm.tmCharSet != CHINESEBIG5_CHARSET
                          && font->ntmTm.tmCharSet != GB2312_CHARSET
                          && font->ntmTm.tmCharSet != HANGUL_CHARSET
                          && font->ntmTm.tmCharSet != JOHAB_CHARSET
                          && font->ntmTm.tmCharSet != SHIFTJIS_CHARSET)
                        return 0;
                    }
                  else
                    /* Other scripts unlikely to be handled.  */
                    return 0;
                }
            }
        }
    }
  return 1;
}

/* Callback function for EnumFontFamiliesEx.
 * Checks if a font matches everything we are trying to check agaist,
 * and if so, adds it to a list. Both the data we are checking against
 * and the list to which the fonts are added are passed in via the
 * lparam argument, in the form of a font_callback_data struct. */
static int CALLBACK
add_font_entity_to_list (logical_font, physical_font, font_type, lParam)
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
     LPARAM lParam;
{
  struct font_callback_data *match_data
    = (struct font_callback_data *) lParam;

  if (logfonts_match (&logical_font->elfLogFont, &match_data->pattern)
      && font_matches_spec (font_type, physical_font,
                            match_data->orig_font_spec))
    {
      Lisp_Object entity
        = w32_enumfont_pattern_entity (match_data->frame, logical_font,
                                       physical_font, font_type);
      if (!NILP (entity))
        match_data->list = Fcons (entity, match_data->list);
    }
  return 1;
}

/* Callback function for EnumFontFamiliesEx.
 * Terminates the search once we have a match. */
static int CALLBACK
add_one_font_entity_to_list (logical_font, physical_font, font_type, lParam)
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
     LPARAM lParam;
{
  struct font_callback_data *match_data
    = (struct font_callback_data *) lParam;
  add_font_entity_to_list (logical_font, physical_font, font_type, lParam);

  /* If we have a font in the list, terminate the search.  */
  return !NILP (match_data->list);
}

/* Convert a Lisp font registry (symbol) to a windows charset.  */
static LONG
registry_to_w32_charset (charset)
     Lisp_Object charset;
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

static Lisp_Object
w32_registry (w32_charset)
     LONG w32_charset;
{
  if (w32_charset == ANSI_CHARSET)
    return Qiso10646_1;
  else
    {
      char * charset = w32_to_x_charset (w32_charset, NULL);
      return intern_downcase (charset, strlen(charset));
    }
}

/* Fill in all the available details of LOGFONT from FONT_SPEC.  */
static void
fill_in_logfont (f, logfont, font_spec)
     FRAME_PTR f;
     LOGFONT *logfont;
     Lisp_Object font_spec;
{
  Lisp_Object tmp, extra;
  int dpi = FRAME_W32_DISPLAY_INFO (f)->resy;

  extra = AREF (font_spec, FONT_EXTRA_INDEX);
  /* Allow user to override dpi settings.  */
  if (CONSP (extra))
    {
      tmp = assq_no_quit (QCdpi, extra);
      if (CONSP (tmp) && INTEGERP (XCDR (tmp)))
        {
          dpi = XINT (XCDR (tmp));
        }
      else if (CONSP (tmp) && FLOATP (XCDR (tmp)))
        {
          dpi = (int) (XFLOAT_DATA (XCDR (tmp)) + 0.5);
        }
    }

  /* Height  */
  tmp = AREF (font_spec, FONT_SIZE_INDEX);
  if (INTEGERP (tmp))
    logfont->lfHeight = -1 * XINT (tmp);
  else if (FLOATP (tmp))
    logfont->lfHeight = (int) (-1.0 *  dpi * XFLOAT_DATA (tmp) / 72.27 + 0.5);

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
    logfont->lfCharSet = registry_to_w32_charset (tmp);

  /* Out Precision  */
  /* Clip Precision  */
  /* Quality  TODO: Allow different quality to be specified, so user
     can enable/disable anti-aliasing for individual fonts.  */
  logfont->lfQuality = DEFAULT_QUALITY;

  /* Generic Family and Face Name  */
  logfont->lfPitchAndFamily = FF_DONTCARE | DEFAULT_PITCH;

  tmp = AREF (font_spec, FONT_FAMILY_INDEX);
  if (! NILP (tmp))
    {
      logfont->lfPitchAndFamily = w32_generic_family (tmp) | DEFAULT_PITCH;
      if ((logfont->lfPitchAndFamily & 0xF0) != FF_DONTCARE)
        ; /* Font name was generic, don't fill in font name.  */
        /* Font families are interned, but allow for strings also in case of
           user input.  */
      else if (SYMBOLP (tmp))
        strncpy (logfont->lfFaceName, SDATA (SYMBOL_NAME (tmp)), LF_FACESIZE);
      else if (STRINGP (tmp))
        strncpy (logfont->lfFaceName, SDATA (tmp), LF_FACESIZE);
    }

  /* Process EXTRA info.  */
  for ( ; CONSP (extra); extra = XCDR (extra))
    {
      tmp = XCAR (extra);
      if (CONSP (tmp))
        {
          Lisp_Object key, val;
          key = XCAR (tmp), val = XCDR (tmp);
          if (EQ (key, QCfamily))
            {
              /* Override generic family.  */
              BYTE family = w32_generic_family (val);
              if (family != FF_DONTCARE)
                logfont->lfPitchAndFamily
                  = logfont->lfPitchAndFamily & 0x0F | family;
            }
          else if (EQ (key, QCspacing))
            {
              /* Set pitch based on the spacing property.  */
              if (INTEGERP (val))
                {
                  int spacing = XINT (val);
                  if (spacing < FONT_SPACING_MONO)
                    logfont->lfPitchAndFamily
                      = logfont->lfPitchAndFamily & 0xF0 | VARIABLE_PITCH;
                  else
                    logfont->lfPitchAndFamily
                      = logfont->lfPitchAndFamily & 0xF0 | FIXED_PITCH;
                }
              else if (EQ (val, Qp))
                logfont->lfPitchAndFamily
                  = logfont->lfPitchAndFamily & 0xF0 | VARIABLE_PITCH;
              else if (EQ (val, Qc) || EQ (val, Qm))
                logfont->lfPitchAndFamily
                  = logfont->lfPitchAndFamily & 0xF0 | FIXED_PITCH;
            }
          /* Only use QCscript if charset is not provided, or is unicode
             and a single script is specified.  This is rather crude,
             and is only used to narrow down the fonts returned where
             there is a definite match.  Some scripts, such as latin, han,
             cjk-misc match multiple lfCharSet values, so we can't pre-filter
             them.  */
          else if (EQ (key, QCscript)
                   && logfont->lfCharSet == DEFAULT_CHARSET
                   && SYMBOLP (val))
            {
              if (EQ (val, Qgreek))
                logfont->lfCharSet = GREEK_CHARSET;
              else if (EQ (val, Qhangul))
                logfont->lfCharSet = HANGUL_CHARSET;
              else if (EQ (val, Qkana) || EQ (val, Qkanbun))
                logfont->lfCharSet = SHIFTJIS_CHARSET;
              else if (EQ (val, Qbopomofo))
                logfont->lfCharSet = CHINESEBIG5_CHARSET;
              /* GB 18030 supports tibetan, yi, mongolian,
                 fonts that support it should show up if we ask for
                 GB2312 fonts. */
              else if (EQ (val, Qtibetan) || EQ (val, Qyi)
                       || EQ (val, Qmongolian))
                logfont->lfCharSet = GB2312_CHARSET;
              else if (EQ (val, Qhebrew))
                logfont->lfCharSet = HEBREW_CHARSET;
              else if (EQ (val, Qarabic))
                logfont->lfCharSet = ARABIC_CHARSET;
              else if (EQ (val, Qthai))
                logfont->lfCharSet = THAI_CHARSET;
              else if (EQ (val, Qsymbol))
                logfont->lfCharSet = SYMBOL_CHARSET;
            }
        }
    }
}

static void
list_all_matching_fonts (match_data)
     struct font_callback_data *match_data;
{
  HDC dc;
  Lisp_Object families = w32font_list_family (match_data->frame);
  struct frame *f = XFRAME (match_data->frame);

  dc = get_frame_dc (f);

  while (!NILP (families))
    {
      /* TODO: Use the Unicode versions of the W32 APIs, so we can
         handle non-ASCII font names.  */
      char *name;
      Lisp_Object family = CAR (families);
      families = CDR (families);
      if (NILP (family))
        continue;
      else if (STRINGP (family))
        name = SDATA (family);
      else
        name = SDATA (SYMBOL_NAME (family)); 

      strncpy (match_data->pattern.lfFaceName, name, LF_FACESIZE);
      match_data->pattern.lfFaceName[LF_FACESIZE - 1] = '\0';

      EnumFontFamiliesEx (dc, &match_data->pattern,
                          (FONTENUMPROC) add_font_entity_to_list,
                          (LPARAM) match_data, 0);
    }

  release_frame_dc (f, dc);
}

/* Return a list of all the scripts that the font supports.  */
static Lisp_Object
font_supported_scripts (FONTSIGNATURE * sig)
{
  DWORD * subranges = sig->fsUsb;
  Lisp_Object supported = Qnil;

  /* Match a single subrange. SYM is set if bit N is set in subranges.  */
#define SUBRANGE(n,sym) \
  if (subranges[(n) / 32] & (1 << ((n) % 32))) \
    supported = Fcons ((sym), supported)

  /* Match multiple subranges. SYM is set if any MASK bit is set in
     subranges[0 - 3].  */
#define MASK_ANY(mask0,mask1,mask2,mask3,sym)      \
  if ((subranges[0] & (mask0)) || (subranges[1] & (mask1))     \
      || (subranges[2] & (mask2)) || (subranges[3] & (mask3))) \
    supported = Fcons ((sym), supported)

  SUBRANGE (0, Qlatin); /* There are many others... */

  SUBRANGE (7, Qgreek);
  SUBRANGE (8, Qcoptic);
  SUBRANGE (9, Qcyrillic);
  SUBRANGE (10, Qarmenian);
  SUBRANGE (11, Qhebrew);
  SUBRANGE (13, Qarabic);
  SUBRANGE (14, Qnko);
  SUBRANGE (15, Qdevanagari);
  SUBRANGE (16, Qbengali);
  SUBRANGE (17, Qgurmukhi);
  SUBRANGE (18, Qgujarati);
  SUBRANGE (19, Qoriya);
  SUBRANGE (20, Qtamil);
  SUBRANGE (21, Qtelugu);
  SUBRANGE (22, Qkannada);
  SUBRANGE (23, Qmalayalam);
  SUBRANGE (24, Qthai);
  SUBRANGE (25, Qlao);
  SUBRANGE (26, Qgeorgian);

  SUBRANGE (48, Qcjk_misc);
  SUBRANGE (51, Qbopomofo);
  SUBRANGE (54, Qkanbun); /* Is this right?  */
  SUBRANGE (56, Qhangul);

  SUBRANGE (59, Qhan); /* There are others, but this is the main one.  */
  SUBRANGE (59, Qideographic_description); /* Windows lumps this in  */

  SUBRANGE (70, Qtibetan);
  SUBRANGE (71, Qsyriac);
  SUBRANGE (72, Qthaana);
  SUBRANGE (73, Qsinhala);
  SUBRANGE (74, Qmyanmar);
  SUBRANGE (75, Qethiopic);
  SUBRANGE (76, Qcherokee);
  SUBRANGE (77, Qcanadian_aboriginal);
  SUBRANGE (78, Qogham);
  SUBRANGE (79, Qrunic);
  SUBRANGE (80, Qkhmer);
  SUBRANGE (81, Qmongolian);
  SUBRANGE (82, Qbraille);
  SUBRANGE (83, Qyi);

  SUBRANGE (88, Qbyzantine_musical_symbol);
  SUBRANGE (88, Qmusical_symbol); /* Windows doesn't distinguish these.  */

  SUBRANGE (89, Qmathematical);

  /* Match either katakana or hiragana for kana.  */
  MASK_ANY (0, 0x00060000, 0, 0, Qkana);

  /* There isn't really a main symbol range, so include symbol if any
     relevant range is set.  */
  MASK_ANY (0x8000000, 0x0000FFFF, 0, 0, Qsymbol);

#undef SUBRANGE
#undef MASK_ANY

  return supported;
}


struct font_driver w32font_driver =
  {
    0, /* Qgdi */
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


/* Initialize state that does not change between invocations. This is only
   called when Emacs is dumped.  */
void
syms_of_w32font ()
{
  DEFSYM (Qgdi, "gdi");

  /* Generic font families.  */
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qserif, "serif");
  DEFSYM (Qsans_serif, "sans-serif");
  DEFSYM (Qscript, "script");
  DEFSYM (Qdecorative, "decorative");
  /* Aliases.  */
  DEFSYM (Qsans__serif, "sans_serif");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qmono, "mono");

  /* Fake foundries.  */
  DEFSYM (Qraster, "raster");
  DEFSYM (Qoutline, "outline");
  DEFSYM (Qunknown, "unknown");

  /* Indexes for extra info.  */
  DEFSYM (QCfamily, ":family");

  /* Scripts  */
  DEFSYM (Qlatin, "latin");
  DEFSYM (Qgreek, "greek");
  DEFSYM (Qcoptic, "coptic");
  DEFSYM (Qcyrillic, "cyrillic");
  DEFSYM (Qarmenian, "armenian");
  DEFSYM (Qhebrew, "hebrew");
  DEFSYM (Qarabic, "arabic");
  DEFSYM (Qsyriac, "syriac");
  DEFSYM (Qnko, "nko");
  DEFSYM (Qthaana, "thaana");
  DEFSYM (Qdevanagari, "devanagari");
  DEFSYM (Qbengali, "bengali");
  DEFSYM (Qgurmukhi, "gurmukhi");
  DEFSYM (Qgujarati, "gujarati");
  DEFSYM (Qoriya, "oriya");
  DEFSYM (Qtamil, "tamil");
  DEFSYM (Qtelugu, "telugu");
  DEFSYM (Qkannada, "kannada");
  DEFSYM (Qmalayalam, "malayalam");
  DEFSYM (Qsinhala, "sinhala");
  DEFSYM (Qthai, "thai");
  DEFSYM (Qlao, "lao");
  DEFSYM (Qtibetan, "tibetan");
  DEFSYM (Qmyanmar, "myanmar");
  DEFSYM (Qgeorgian, "georgian");
  DEFSYM (Qhangul, "hangul");
  DEFSYM (Qethiopic, "ethiopic");
  DEFSYM (Qcherokee, "cherokee");
  DEFSYM (Qcanadian_aboriginal, "canadian-aboriginal");
  DEFSYM (Qogham, "ogham");
  DEFSYM (Qrunic, "runic");
  DEFSYM (Qkhmer, "khmer");
  DEFSYM (Qmongolian, "mongolian");
  DEFSYM (Qsymbol, "symbol");
  DEFSYM (Qbraille, "braille");
  DEFSYM (Qhan, "han");
  DEFSYM (Qideographic_description, "ideographic-description");
  DEFSYM (Qcjk_misc, "cjk-misc");
  DEFSYM (Qkana, "kana");
  DEFSYM (Qbopomofo, "bopomofo");
  DEFSYM (Qkanbun, "kanbun");
  DEFSYM (Qyi, "yi");
  DEFSYM (Qbyzantine_musical_symbol, "byzantine-musical-symbol");
  DEFSYM (Qmusical_symbol, "musical-symbol");
  DEFSYM (Qmathematical, "mathematical");

  w32font_driver.type = Qgdi;
  register_font_driver (&w32font_driver, NULL);
}

/* arch-tag: 65b8a3cd-46aa-4c0d-a1f3-99e75b9c07ee
   (do not change this comment) */
