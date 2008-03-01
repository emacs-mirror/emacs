/* Font backend for the Microsoft W32 API.
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
#include "w32font.h"

/* Cleartype available on Windows XP, cleartype_natural from XP SP1.
   The latter does not try to fit cleartype smoothed fonts into the
   same bounding box as the non-antialiased version of the font.
 */
#ifndef CLEARTYPE_QUALITY
#define CLEARTYPE_QUALITY 5
#endif
#ifndef CLEARTYPE_NATURAL_QUALITY
#define CLEARTYPE_NATURAL_QUALITY 6
#endif

extern struct font_driver w32font_driver;

Lisp_Object Qgdi;
static Lisp_Object Qmonospace, Qsansserif, Qmono, Qsans, Qsans_serif;
static Lisp_Object Qserif, Qscript, Qdecorative;
static Lisp_Object Qraster, Qoutline, Qunknown;

/* antialiasing  */
extern Lisp_Object QCantialias; /* defined in font.c  */
extern Lisp_Object Qnone; /* reuse from w32fns.c  */
static Lisp_Object Qstandard, Qsubpixel, Qnatural;

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

static BYTE w32_antialias_type P_ ((Lisp_Object type));
static Lisp_Object lispy_antialias_type P_ ((BYTE type));

static Lisp_Object font_supported_scripts P_ ((FONTSIGNATURE * sig));
static int w32font_full_name P_ ((LOGFONT * font, Lisp_Object font_obj,
                                  int pixel_size, char *name, int nbytes));

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
  /* Whether to match only opentype fonts.  */
  int opentype_only;
};

/* Handles the problem that EnumFontFamiliesEx will not return all
   style variations if the font name is not specified.  */
static void list_all_matching_fonts P_ ((struct font_callback_data *match));


/* MingW headers only define this when _WIN32_WINNT >= 0x0500, but we
   target older versions.  */
#ifndef GGI_MARK_NONEXISTING_GLYPHS
#define GGI_MARK_NONEXISTING_GLYPHS 1
#endif

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
Lisp_Object
w32font_get_cache (f)
     FRAME_PTR f;
{
  struct w32_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

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
  return w32font_list_internal (frame, font_spec, 0);
}

/* w32 implementation of match for font backend.
   Return a font entity most closely matching with FONT_SPEC on
   FRAME.  The closeness is detemined by the font backend, thus
   `face-font-selection-order' is ignored here.  */
static Lisp_Object
w32font_match (frame, font_spec)
     Lisp_Object frame, font_spec;
{
  return w32font_match_internal (frame, font_spec, 0);
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
  struct w32font_info *w32_font = xmalloc (sizeof (struct w32font_info));

  if (w32_font == NULL)
    return NULL;

  if (!w32font_open_internal (f, font_entity, pixel_size, w32_font))
    {
      xfree (w32_font);
      return NULL;
    }

  return (struct font *) w32_font;
}

/* w32 implementation of close for font_backend.
   Close FONT on frame F.  */
void
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

  if (font->font.full_name && font->font.full_name != font->font.name)
    xfree (font->font.full_name);

  if (font->font.name)
    xfree (font->font.name);

  xfree (font);
}

/* w32 implementation of has_char for font backend.
   Optional.
   If FONT_ENTITY has a glyph for character C (Unicode code point),
   return 1.  If not, return 0.  If a font must be opened to check
   it, return -1.  */
int
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
unsigned
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
   CODE (length NGLYPHS).  Apparently metrics can be NULL, in this
   case just return the overall width.  */
int
w32font_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  int i;
  HFONT old_font = NULL;
  HDC dc = NULL;
  struct frame * f;
  int total_width = 0;
  WORD *wcode = alloca(nglyphs * sizeof (WORD));
  SIZE size;

  /* TODO: Frames can come and go, and their fonts outlive them. So we
     can't cache the frame in the font structure.  Use selected_frame
     until the API is updated to pass in a frame.  */
  f = XFRAME (selected_frame);

  if (metrics)
    {
      GLYPHMETRICS gm;
      MAT2 transform;

      /* Set transform to the identity matrix.  */
      bzero (&transform, sizeof (transform));
      transform.eM11.value = 1;
      transform.eM22.value = 1;
      metrics->width = 0;
      metrics->ascent = 0;
      metrics->descent = 0;
      metrics->lbearing = 0;

      for (i = 0; i < nglyphs; i++)
        {
          if (*(code + i) < 128 && *(code + i) > 32)
            {
              /* Use cached metrics for ASCII.  */
              struct font_metrics *char_metric
                = &((struct w32font_info *)font)->ascii_metrics[*(code+i)-32];

              /* If we couldn't get metrics when caching, use fallback.  */
              if (char_metric->width == 0)
                break;

              metrics->lbearing = max (metrics->lbearing,
                                       char_metric->lbearing - metrics->width);
              metrics->rbearing = max (metrics->rbearing,
                                       metrics->width + char_metric->rbearing);
              metrics->width += char_metric->width;
              metrics->ascent = max (metrics->ascent, char_metric->ascent);
              metrics->descent = max (metrics->descent, char_metric->descent);
            }
          else
            {
              if (dc == NULL)
                {
                  dc = get_frame_dc (f);
                  old_font = SelectObject (dc, ((W32FontStruct *)
                                                (font->font.font))->hfont);
                }
              if (GetGlyphOutlineW (dc, *(code + i), GGO_METRICS, &gm, 0,
                                    NULL, &transform) != GDI_ERROR)
                {
                  int new_val = metrics->width + gm.gmBlackBoxX
                    + gm.gmptGlyphOrigin.x;
                  metrics->rbearing = max (metrics->rbearing, new_val);
                  new_val = -gm.gmptGlyphOrigin.x - metrics->width;
                  metrics->lbearing = max (metrics->lbearing, new_val);
                  metrics->width += gm.gmCellIncX;
                  new_val = -gm.gmptGlyphOrigin.y;
                  metrics->ascent = max (metrics->ascent, new_val);
                  new_val = gm.gmBlackBoxY + gm.gmptGlyphOrigin.y;
                  metrics->descent = max (metrics->descent, new_val);
                }
              else
                {
                  /* Rely on an estimate based on the overall font metrics.  */
                  break;
                }
            }
        }

      /* If we got through everything, return.  */
      if (i == nglyphs)
        {
          if (dc != NULL)
            {
              /* Restore state and release DC.  */
              SelectObject (dc, old_font);
              release_frame_dc (f, dc);
            }

          return metrics->width;
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

  if (dc == NULL)
    {
      dc = get_frame_dc (f);
      old_font = SelectObject (dc, ((W32FontStruct *)
                                    (font->font.font))->hfont);
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

  if (metrics)
    {
      metrics->width = total_width;
      metrics->ascent = font->ascent;
      metrics->descent = font->descent;
      metrics->lbearing = 0;
      metrics->rbearing = total_width
        + ((struct w32font_info *) font)->metrics.tmOverhang;
    }

  /* Restore state and release DC.  */
  SelectObject (dc, old_font);
  release_frame_dc (f, dc);

  return total_width;
}

/* w32 implementation of draw for font backend.
   Optional.
   Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
   position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
   is nonzero, fill the background in advance.  It is assured that
   WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars).

   TODO: Currently this assumes that the colors and fonts are already
   set in the DC. This seems to be true now, but maybe only due to
   the old font code setting it up. It may be safer to resolve faces
   and fonts in here and set them explicitly
*/

int
w32font_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  UINT options = 0;
  HRGN orig_clip;

  /* Save clip region for later restoration.  */
  GetClipRgn(s->hdc, orig_clip);

  if (s->num_clips > 0)
    {
      HRGN new_clip = CreateRectRgnIndirect (s->clip);

      if (s->num_clips > 1)
        {
          HRGN clip2 = CreateRectRgnIndirect (s->clip + 1);

          CombineRgn (new_clip, new_clip, clip2, RGN_OR);
          DeleteObject (clip2);
        }

      SelectClipRgn (s->hdc, new_clip);
      DeleteObject (new_clip);
    }

  /* Using OPAQUE background mode can clear more background than expected
     when Cleartype is used.  Draw the background manually to avoid this.  */
  SetBkMode (s->hdc, TRANSPARENT);
  if (with_background)
    {
      HBRUSH brush;
      RECT rect;
      struct font *font = (struct font *) s->face->font_info;

      brush = CreateSolidBrush (s->gc->background);
      rect.left = x;
      rect.top = y - font->ascent;
      rect.right = x + s->width;
      rect.bottom = y + font->descent;
      FillRect (s->hdc, &rect, brush);
      DeleteObject (brush);
    }

  if (s->padding_p)
    {
      int len = to - from, i;

      for (i = 0; i < len; i++)
	ExtTextOutW (s->hdc, x + i, y, options, NULL,
		     s->char2b + from + i, 1, NULL);
    }
  else
    ExtTextOutW (s->hdc, x, y, options, NULL, s->char2b + from, to - from, NULL);

  /* Restore clip region.  */
  if (s->num_clips > 0)
    {
      SelectClipRgn (s->hdc, orig_clip);
    }
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

/* Internal implementation of w32font_list.
   Additional parameter opentype_only restricts the returned fonts to
   opentype fonts, which can be used with the Uniscribe backend.  */
Lisp_Object
w32font_list_internal (frame, font_spec, opentype_only)
     Lisp_Object frame, font_spec;
     int opentype_only;
{
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.list = Qnil;
  match_data.frame = frame;

  bzero (&match_data.pattern, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  match_data.opentype_only = opentype_only;
  if (opentype_only)
    match_data.pattern.lfOutPrecision = OUT_OUTLINE_PRECIS;

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

/* Internal implementation of w32font_match.
   Additional parameter opentype_only restricts the returned fonts to
   opentype fonts, which can be used with the Uniscribe backend.  */
Lisp_Object
w32font_match_internal (frame, font_spec, opentype_only)
     Lisp_Object frame, font_spec;
     int opentype_only;
{
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.frame = frame;
  match_data.list = Qnil;

  bzero (&match_data.pattern, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  match_data.opentype_only = opentype_only;
  if (opentype_only)
    match_data.pattern.lfOutPrecision = OUT_OUTLINE_PRECIS;

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &match_data.pattern,
                      (FONTENUMPROC) add_one_font_entity_to_list,
                      (LPARAM) &match_data, 0);
  release_frame_dc (f, dc);

  return NILP (match_data.list) ? Qnil : XCAR (match_data.list);
}

int
w32font_open_internal (f, font_entity, pixel_size, w32_font)
     FRAME_PTR f;
     Lisp_Object font_entity;
     int pixel_size;
     struct w32font_info *w32_font;
{
  int len, size;
  LOGFONT logfont;
  HDC dc;
  HFONT hfont, old_font;
  Lisp_Object val, extra;
  /* For backwards compatibility.  */
  W32FontStruct *compat_w32_font;

  struct font * font = (struct font *) w32_font;
  if (!font)
    return 0;

  bzero (&logfont, sizeof (logfont));
  fill_in_logfont (f, &logfont, font_entity);

  size = XINT (AREF (font_entity, FONT_SIZE_INDEX));
  if (!size)
    size = pixel_size;

  logfont.lfHeight = -size;
  hfont = CreateFontIndirect (&logfont);

  if (hfont == NULL)
    return 0;

  /* Get the metrics for this font.  */
  dc = get_frame_dc (f);
  old_font = SelectObject (dc, hfont);

  GetTextMetrics (dc, &w32_font->metrics);

  /* Cache ASCII metrics.  */
  {
    GLYPHMETRICS gm;
    MAT2 transform;
    int i;

    bzero (&transform, sizeof (transform));
    transform.eM11.value = 1;
    transform.eM22.value = 1;

    for (i = 0; i < 96; i++)
      {
        struct font_metrics* char_metric = &w32_font->ascii_metrics[i];

        if (GetGlyphOutlineW (dc, i + 32, GGO_METRICS, &gm, 0,
                              NULL, &transform) != GDI_ERROR)
          {
            char_metric->lbearing = -gm.gmptGlyphOrigin.x;
            char_metric->rbearing = gm.gmBlackBoxX + gm.gmptGlyphOrigin.x;
            char_metric->width = gm.gmCellIncX;
            char_metric->ascent = -gm.gmptGlyphOrigin.y;
            char_metric->descent = gm.gmBlackBoxY + gm.gmptGlyphOrigin.y;
          }
        else
          char_metric->width = 0;
      }
  }
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

  {
    char *name;

    /* We don't know how much space we need for the full name, so start with
       96 bytes and go up in steps of 32.  */
    len = 96;
    name = xmalloc (len);
    while (name && w32font_full_name (&logfont, font_entity, pixel_size,
                                      name, len) < 0)
      {
        char *new = xrealloc (name, len += 32);

        if (! new)
          xfree (name);
        name = new;
      }
    if (name)
      font->font.full_name = name;
    else
      font->font.full_name = font->font.name;
  }
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
  /* TODO: do we really want the minimum width here, which could be negative? */
  font->min_width = font->font.space_width;
  font->ascent = w32_font->metrics.tmAscent;
  font->descent = w32_font->metrics.tmDescent;
  font->scalable = w32_font->metrics.tmPitchAndFamily & TMPF_VECTOR;

  /* Set global flag fonts_changed_p to non-zero if the font loaded
     has a character with a smaller width than any other character
     before, or if the font loaded has a smaller height than any other
     font loaded before.  If this happens, it will make a glyph matrix
     reallocation necessary.  */
  {
    struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
    dpyinfo->n_fonts++;

    if (dpyinfo->n_fonts == 1)
      {
        dpyinfo->smallest_font_height = font->font.height;
        dpyinfo->smallest_char_width = font->min_width;
      }
    else
      {
        if (dpyinfo->smallest_font_height > font->font.height)
          {
            dpyinfo->smallest_font_height = font->font.height;
            fonts_changed_p |= 1;
          }
        if (dpyinfo->smallest_char_width > font->min_width)
          {
            dpyinfo->smallest_char_width = font->min_width;
            fonts_changed_p |= 1;
          }
      }
  }

  return 1;
}

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
  Lisp_Object family;

  /* Skip vertical fonts (intended only for printing)  */
  if (logical_font->elfLogFont.lfFaceName[0] == '@')
    return 1;

  family = intern_downcase (logical_font->elfLogFont.lfFaceName,
                            strlen (logical_font->elfLogFont.lfFaceName));
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}

/* Convert an enumerated Windows font to an Emacs font entity.  */
static Lisp_Object
w32_enumfont_pattern_entity (frame, logical_font, physical_font,
                             font_type, requested_font)
     Lisp_Object frame;
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
     LOGFONT *requested_font;
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
    tem = Qmono;
  else if (generic_type == FF_ROMAN)
    tem = Qserif;
  else if (generic_type == FF_SCRIPT)
    tem = Qscript;
  else if (generic_type == FF_SWISS)
    tem = Qsans;
  else
    tem = null_string;

  ASET (entity, FONT_ADSTYLE_INDEX, tem);

  if (physical_font->ntmTm.tmPitchAndFamily & 0x01)
    font_put_extra (entity, QCspacing, make_number (FONT_SPACING_PROPORTIONAL));
  else
    font_put_extra (entity, QCspacing, make_number (FONT_SPACING_MONO));

  if (requested_font->lfQuality != DEFAULT_QUALITY)
    {
      font_put_extra (entity, QCantialias,
                      lispy_antialias_type (requested_font->lfQuality));
    }
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
  else if (EQ (name, Qsans) || EQ (name, Qsans_serif) || EQ (name, Qsansserif))
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
        return 0;
    }

  /* Check adstyle against generic family.  */
  val = AREF (spec, FONT_ADSTYLE_INDEX);
  if (!NILP (val))
    {
      BYTE family = w32_generic_family (val);
      if (family != FF_DONTCARE
          && family != (font->ntmTm.tmPitchAndFamily & 0xF0))
        return 0;
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
          if (EQ (key, QCspacing))
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

  if ((!match_data->opentype_only
       || (physical_font->ntmTm.ntmFlags & NTMFLAGS_OPENTYPE))
      && logfonts_match (&logical_font->elfLogFont, &match_data->pattern)
      && font_matches_spec (font_type, physical_font,
                            match_data->orig_font_spec)
      /* Avoid substitutions involving raster fonts (eg Helv -> MS Sans Serif)
         We limit this to raster fonts, because the test can catch some
         genuine fonts (eg the full name of DejaVu Sans Mono Light is actually
         DejaVu Sans Mono ExtraLight). Helvetica -> Arial substitution will
         therefore get through this test.  Since full names can be prefixed
         by a foundry, we accept raster fonts if the font name is found
         anywhere within the full name.  */
      && (logical_font->elfLogFont.lfOutPrecision != OUT_STRING_PRECIS
          || strstr (logical_font->elfFullName,
                     logical_font->elfLogFont.lfFaceName)))
    {
      Lisp_Object entity
        = w32_enumfont_pattern_entity (match_data->frame, logical_font,
                                       physical_font, font_type,
                                       &match_data->pattern);
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

  /* Quality */
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

  tmp = AREF (font_spec, FONT_ADSTYLE_INDEX);
  if (!NILP (tmp))
    {
      /* Override generic family.  */
      BYTE family = w32_generic_family (tmp);
      if (family != FF_DONTCARE)
        logfont->lfPitchAndFamily = family | DEFAULT_PITCH;
    }

  /* Process EXTRA info.  */
  for ( ; CONSP (extra); extra = XCDR (extra))
    {
      tmp = XCAR (extra);
      if (CONSP (tmp))
        {
          Lisp_Object key, val;
          key = XCAR (tmp), val = XCDR (tmp);
          if (EQ (key, QCspacing))
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
          else if (EQ (key, QCantialias) && SYMBOLP (val))
            {
              logfont->lfQuality = w32_antialias_type (val);
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

static Lisp_Object
lispy_antialias_type (type)
     BYTE type;
{
  Lisp_Object lispy;

  switch (type)
    {
    case NONANTIALIASED_QUALITY:
      lispy = Qnone;
      break;
    case ANTIALIASED_QUALITY:
      lispy = Qstandard;
      break;
    case CLEARTYPE_QUALITY:
      lispy = Qsubpixel;
      break;
    case CLEARTYPE_NATURAL_QUALITY:
      lispy = Qnatural;
      break;
    default:
      lispy = Qnil;
      break;
    }
  return lispy;
}

/* Convert antialiasing symbols to lfQuality  */
static BYTE
w32_antialias_type (type)
     Lisp_Object type;
{
  if (EQ (type, Qnone))
    return NONANTIALIASED_QUALITY;
  else if (EQ (type, Qstandard))
    return ANTIALIASED_QUALITY;
  else if (EQ (type, Qsubpixel))
    return CLEARTYPE_QUALITY;
  else if (EQ (type, Qnatural))
    return CLEARTYPE_NATURAL_QUALITY;
  else
    return DEFAULT_QUALITY;
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

/* Generate a full name for a Windows font.
   The full name is in fcname format, with weight, slant and antialiasing
   specified if they are not "normal".  */
static int
w32font_full_name (font, font_obj, pixel_size, name, nbytes)
  LOGFONT * font;
  Lisp_Object font_obj;
  int pixel_size;
  char *name;
  int nbytes;
{
  int len;
  char *p;
  Lisp_Object antialiasing, weight = Qnil;

  len = strlen (font->lfFaceName) + 21; /* :pixelsize=SIZE */

  if (font->lfItalic)
    len += 7; /* :italic */

  if (font->lfWeight && font->lfWeight != FW_NORMAL)
    {
      weight = font_symbolic_weight (font_obj);
      len += 8 + SBYTES (SYMBOL_NAME (weight)); /* :weight=NAME */
    }

  antialiasing = lispy_antialias_type (font->lfQuality);
  if (! NILP (antialiasing))
    len += 11 + SBYTES (SYMBOL_NAME (antialiasing)); /* :antialias=NAME */

  /* Check that the buffer is big enough  */
  if (len > nbytes)
    return -1;

  p = name;
  p += sprintf (p, "%s", font->lfFaceName);

  if (font->lfHeight)
    p += sprintf (p, ":pixelsize=%d", eabs (font->lfHeight));
  else if (pixel_size > 0)
    p += sprintf (p, ":pixelsize=%d", pixel_size);

  if (font->lfItalic)
    p += sprintf (p, ":italic");

  if (SYMBOLP (weight) && ! NILP (weight))
    p += sprintf (p, ":weight=%s", SDATA (SYMBOL_NAME (weight)));

  if (SYMBOLP (antialiasing) && ! NILP (antialiasing))
    p += sprintf (p, ":antialias=%s", SDATA (SYMBOL_NAME (antialiasing)));

  return (p - name);
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
    NULL, /* otf_drive */
    NULL, /* start_for_frame */
    NULL, /* end_for_frame */
    NULL  /* shape */
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
  DEFSYM (Qsansserif, "sansserif");
  DEFSYM (Qscript, "script");
  DEFSYM (Qdecorative, "decorative");
  /* Aliases.  */
  DEFSYM (Qsans_serif, "sans_serif");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qmono, "mono");

  /* Fake foundries.  */
  DEFSYM (Qraster, "raster");
  DEFSYM (Qoutline, "outline");
  DEFSYM (Qunknown, "unknown");

  /* Antialiasing.  */
  DEFSYM (Qstandard, "standard");
  DEFSYM (Qsubpixel, "subpixel");
  DEFSYM (Qnatural, "natural");

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
