/* Font backend for the Microsoft W32 API.
   Copyright (C) 2007, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <windows.h>
#include <math.h>

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
Lisp_Object Quniscribe;
static Lisp_Object QCformat;
static Lisp_Object Qmonospace, Qsansserif, Qmono, Qsans, Qsans_serif;
static Lisp_Object Qserif, Qscript, Qdecorative;
static Lisp_Object Qraster, Qoutline, Qunknown;

/* antialiasing  */
extern Lisp_Object QCantialias, QCotf, QClang; /* defined in font.c  */
extern Lisp_Object Qnone; /* reuse from w32fns.c  */
static Lisp_Object Qstandard, Qsubpixel, Qnatural;

/* languages */
static Lisp_Object Qja, Qko, Qzh;

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
/* Not defined in characters.el, but referenced in fontset.el.  */
static Lisp_Object Qbalinese, Qbuginese, Qbuhid, Qcuneiform, Qcypriot;
static Lisp_Object Qdeseret, Qglagolitic, Qgothic, Qhanunoo, Qkharoshthi;
static Lisp_Object Qlimbu, Qlinear_b, Qold_italic, Qold_persian, Qosmanya;
static Lisp_Object Qphags_pa, Qphoenician, Qshavian, Qsyloti_nagri;
static Lisp_Object Qtagalog, Qtagbanwa, Qtai_le, Qtifinagh, Qugaritic;
/* Only defined here, but useful for distinguishing IPA capable fonts.  */
static Lisp_Object Qphonetic;

/* Font spacing symbols - defined in font.c.  */
extern Lisp_Object Qc, Qp, Qm;

static void fill_in_logfont P_ ((FRAME_PTR, LOGFONT *, Lisp_Object));

static BYTE w32_antialias_type P_ ((Lisp_Object));
static Lisp_Object lispy_antialias_type P_ ((BYTE));

static Lisp_Object font_supported_scripts P_ ((FONTSIGNATURE *));
static int w32font_full_name P_ ((LOGFONT *, Lisp_Object, int, char *, int));
static void compute_metrics P_ ((HDC, struct w32font_info *, unsigned int,
				 struct w32_metric_cache *));
static void clear_cached_metrics P_ ((struct w32font_info *));

static Lisp_Object w32_registry P_ ((LONG, DWORD));

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
static void list_all_matching_fonts P_ ((struct font_callback_data *));

/* From old font code in w32fns.c */
char * w32_to_x_charset P_ ((int, char *));


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
  font_match_pattern.lfCharSet = DEFAULT_CHARSET;

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
static Lisp_Object
w32font_open (f, font_entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object font_entity;
     int pixel_size;
{
  Lisp_Object font_object;

  font_object = font_make_object (VECSIZE (struct w32font_info));

  if (!w32font_open_internal (f, font_entity, pixel_size, font_object))
    {
      return Qnil;
    }

  return font_object;
}

/* w32 implementation of close for font_backend.
   Close FONT on frame F.  */
void
w32font_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct w32font_info *w32_font = (struct w32font_info *) font;

  if (w32_font->compat_w32_font)
    {
      W32FontStruct *old_w32_font = w32_font->compat_w32_font;
      DeleteObject (old_w32_font->hfont);
      xfree (old_w32_font);
      w32_font->compat_w32_font = 0;
    }
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

  return (memq_no_quit (script, supported_scripts)) ? -1 : 0;
}

/* w32 implementation of encode_char for font backend.
   Return a glyph code of FONT for characer C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
static unsigned
w32font_encode_char (font, c)
     struct font *font;
     int c;
{
  struct frame *f;
  HDC dc;
  HFONT old_font;
  DWORD retval;
  GCP_RESULTSW result;
  wchar_t in[2];
  wchar_t out[2];
  int len;
  struct w32font_info *w32_font = (struct w32font_info *) font;

  /* If glyph indexing is not working for this font, just return the
     unicode code-point.  */
  if (!w32_font->glyph_idx)
    return c;

  if (c > 0xFFFF)
    {
      /* TODO: Encode as surrogate pair and lookup the glyph.  */
      return FONT_INVALID_CODE;
    }
  else
    {
      in[0] = (wchar_t) c;
      len = 1;
    }

  bzero (&result, sizeof (result));
  result.lStructSize = sizeof (result);
  result.lpGlyphs = out;
  result.nGlyphs = 2;

  f = XFRAME (selected_frame);

  dc = get_frame_dc (f);
  old_font = SelectObject (dc, w32_font->compat_w32_font->hfont);

  retval = GetCharacterPlacementW (dc, in, len, 0, &result, 0);

  SelectObject (dc, old_font);
  release_frame_dc (f, dc);

  if (retval)
    {
      if (result.nGlyphs != 1 || !result.lpGlyphs[0])
        return FONT_INVALID_CODE;
      return result.lpGlyphs[0];
    }
  else
    {
      int i;
      /* Mark this font as not supporting glyph indices. This can happen
         on Windows9x, and maybe with non-Truetype fonts on NT etc.  */
      w32_font->glyph_idx = 0;
      /* Clear metrics cache.  */
      clear_cached_metrics (w32_font);

      return c;
    }
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
  WORD *wcode = NULL;
  SIZE size;

  if (metrics)
    {
      struct w32font_info *w32_font = (struct w32font_info *) font;

      metrics->width = 0;
      metrics->ascent = font->ascent;
      metrics->descent = font->descent;
      metrics->lbearing = 0;

      for (i = 0; i < nglyphs; i++)
        {
	  struct w32_metric_cache *char_metric;
	  int block = *(code + i) / CACHE_BLOCKSIZE;
	  int pos_in_block = *(code + i) % CACHE_BLOCKSIZE;

	  if (block >= w32_font->n_cache_blocks)
	    {
	      if (!w32_font->cached_metrics)
		w32_font->cached_metrics
		  = xmalloc ((block + 1)
			     * sizeof (struct w32_cached_metric *));
	      else
		w32_font->cached_metrics
		  = xrealloc (w32_font->cached_metrics,
			      (block + 1)
			      * sizeof (struct w32_cached_metric *));
	      bzero (w32_font->cached_metrics + w32_font->n_cache_blocks,
		     ((block + 1 - w32_font->n_cache_blocks)
		      * sizeof (struct w32_cached_metric *)));
	      w32_font->n_cache_blocks = block + 1;
	    }

	  if (!w32_font->cached_metrics[block])
	    {
	      w32_font->cached_metrics[block]
		= xmalloc (CACHE_BLOCKSIZE * sizeof (struct font_metrics));
	      bzero (w32_font->cached_metrics[block],
		     CACHE_BLOCKSIZE * sizeof (struct font_metrics));
	    }

	  char_metric = w32_font->cached_metrics[block] + pos_in_block;

	  if (char_metric->status == W32METRIC_NO_ATTEMPT)
	    {
	      if (dc == NULL)
		{
		  /* TODO: Frames can come and go, and their fonts
		     outlive them. So we can't cache the frame in the
		     font structure.  Use selected_frame until the API
		     is updated to pass in a frame.  */
		  f = XFRAME (selected_frame);
  
                  dc = get_frame_dc (f);
                  old_font = SelectObject (dc, FONT_COMPAT (font)->hfont);
		}
	      compute_metrics (dc, w32_font, *(code + i), char_metric);
	    }

	  if (char_metric->status == W32METRIC_SUCCESS)
	    {
	      metrics->lbearing = min (metrics->lbearing,
				       metrics->width + char_metric->lbearing);
	      metrics->rbearing = max (metrics->rbearing,
				       metrics->width + char_metric->rbearing);
	      metrics->width += char_metric->width;
	    }
	  else
	    /* If we couldn't get metrics for a char,
	       use alternative method.  */
	    break;
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

  /* For non-truetype fonts, GetGlyphOutlineW is not supported, so
     fallback on other methods that will at least give some of the metric
     information.  */
  if (!wcode) {
    wcode = alloca (nglyphs * sizeof (WORD));
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
  if (dc == NULL)
    {
      /* TODO: Frames can come and go, and their fonts outlive
	 them. So we can't cache the frame in the font structure.  Use
	 selected_frame until the API is updated to pass in a
	 frame.  */
      f = XFRAME (selected_frame);

      dc = get_frame_dc (f);
      old_font = SelectObject (dc, FONT_COMPAT (font)->hfont);
    }

  if (GetTextExtentPoint32W (dc, wcode, nglyphs, &size))
    {
      total_width = size.cx;
    }

  /* On 95/98/ME, only some unicode functions are available, so fallback
     on doing a dummy draw to find the total width.  */
  if (!total_width)
    {
      RECT rect;
      rect.top = 0; rect.bottom = font->height; rect.left = 0; rect.right = 1;
      DrawTextW (dc, wcode, nglyphs, &rect,
                 DT_CALCRECT | DT_NOPREFIX | DT_SINGLELINE);
      total_width = rect.right;
    }

  /* Give our best estimate of the metrics, based on what we know.  */
  if (metrics)
    {
      metrics->width = total_width;
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
  UINT options;
  HRGN orig_clip;
  struct w32font_info *w32font = (struct w32font_info *) s->font;

  options = w32font->glyph_idx;

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
      struct font *font = s->font;

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

  return NILP (match_data.list) ? Qnil : match_data.list;
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
w32font_open_internal (f, font_entity, pixel_size, font_object)
     FRAME_PTR f;
     Lisp_Object font_entity;
     int pixel_size;
     Lisp_Object font_object;
{
  int len, size, i;
  LOGFONT logfont;
  HDC dc;
  HFONT hfont, old_font;
  Lisp_Object val, extra;
  /* For backwards compatibility.  */
  W32FontStruct *compat_w32_font;
  struct w32font_info *w32_font;
  struct font * font;
  OUTLINETEXTMETRIC* metrics = NULL;

  w32_font = (struct w32font_info *) XFONT_OBJECT (font_object);
  font = (struct font *) w32_font;

  if (!font)
    return 0;

  /* Copy from font entity.  */
  for (i = 0; i < FONT_ENTITY_MAX; i++)
    ASET (font_object, i, AREF (font_entity, i));
  ASET (font_object, FONT_SIZE_INDEX, make_number (pixel_size));

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

  /* Try getting the outline metrics (only works for truetype fonts).  */
  len = GetOutlineTextMetrics (dc, 0, NULL);
  if (len)
    {
      metrics = (OUTLINETEXTMETRIC *) alloca (len);
      if (GetOutlineTextMetrics (dc, len, metrics))
        bcopy (&metrics->otmTextMetrics, &w32_font->metrics,
               sizeof (TEXTMETRIC));
      else
        metrics = NULL;
    }
  if (!metrics)
    GetTextMetrics (dc, &w32_font->metrics);

  w32_font->glyph_idx = ETO_GLYPH_INDEX;

  w32_font->cached_metrics = NULL;
  w32_font->n_cache_blocks = 0;

  SelectObject (dc, old_font);
  release_frame_dc (f, dc);

  /* W32FontStruct - we should get rid of this, and use the w32font_info
     struct for any W32 specific fields. font->font.font can then be hfont.  */
  w32_font->compat_w32_font = xmalloc (sizeof (W32FontStruct));
  compat_w32_font = w32_font->compat_w32_font;
  bzero (compat_w32_font, sizeof (W32FontStruct));
  compat_w32_font->font_type = UNICODE_FONT;
  /* Duplicate the text metrics.  */
  bcopy (&w32_font->metrics,  &compat_w32_font->tm, sizeof (TEXTMETRIC));
  compat_w32_font->hfont = hfont;

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
      font->props[FONT_FULLNAME_INDEX]
        = make_unibyte_string (name, strlen (name));
    else
      font->props[FONT_FULLNAME_INDEX] =
        make_unibyte_string (logfont.lfFaceName, len);
  }

  font->max_width = w32_font->metrics.tmMaxCharWidth;
  font->height = w32_font->metrics.tmHeight
    + w32_font->metrics.tmExternalLeading;
  font->space_width = font->average_width = w32_font->metrics.tmAveCharWidth;

  font->vertical_centering = 0;
  font->encoding_type = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = w32_font->metrics.tmAscent;
  font->font_encoder = NULL;
  font->pixel_size = size;
  font->driver = &w32font_driver;
  /* Use format cached during list, as the information we have access to
     here is incomplete.  */
  extra = AREF (font_entity, FONT_EXTRA_INDEX);
  if (CONSP (extra))
    {
      val = assq_no_quit (QCformat, extra);
      if (CONSP (val))
        font->props[FONT_FORMAT_INDEX] = XCDR (val);
      else
        font->props[FONT_FORMAT_INDEX] = Qunknown;
    }
  else
    font->props[FONT_FORMAT_INDEX] = Qunknown;

  font->props[FONT_FILE_INDEX] = Qnil;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  /* TODO: do we really want the minimum width here, which could be negative? */
  font->min_width = font->space_width;
  font->ascent = w32_font->metrics.tmAscent;
  font->descent = w32_font->metrics.tmDescent;

  if (metrics)
    {
      font->underline_thickness = metrics->otmsUnderscoreSize;
      font->underline_position = -metrics->otmsUnderscorePosition;
    }
  else
    {
      font->underline_thickness = 0;
      font->underline_position = -1;
    }

  /* max_descent is used for underlining in w32term.c.  Hopefully this
     is temporary, as we'll want to get rid of the old compatibility
     stuff later.  */
  compat_w32_font->max_bounds.descent = font->descent;

  /* For temporary compatibility with legacy code that expects the
     name to be usable in x-list-fonts. Eventually we expect to change
     x-list-fonts and other places that use fonts so that this can be
     an fcname or similar.  */
  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);

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

  family = font_intern_prop (logical_font->elfLogFont.lfFaceName,
			     strlen (logical_font->elfLogFont.lfFaceName));
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}

static int w32_decode_weight P_ ((int));
static int w32_encode_weight P_ ((int));

/* Convert an enumerated Windows font to an Emacs font entity.  */
static Lisp_Object
w32_enumfont_pattern_entity (frame, logical_font, physical_font,
                             font_type, requested_font, backend)
     Lisp_Object frame;
     ENUMLOGFONTEX *logical_font;
     NEWTEXTMETRICEX *physical_font;
     DWORD font_type;
     LOGFONT *requested_font;
     Lisp_Object backend;
{
  Lisp_Object entity, tem;
  LOGFONT *lf = (LOGFONT*) logical_font;
  BYTE generic_type;
  DWORD full_type = physical_font->ntmTm.ntmFlags;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, backend);
  ASET (entity, FONT_REGISTRY_INDEX, w32_registry (lf->lfCharSet, font_type));
  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

  /* Foundry is difficult to get in readable form on Windows.
     But Emacs crashes if it is not set, so set it to something more
     generic.  These values make xflds compatible with Emacs 22. */
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
    tem = Qnil;

  ASET (entity, FONT_ADSTYLE_INDEX, tem);

  if (physical_font->ntmTm.tmPitchAndFamily & 0x01)
    ASET (entity, FONT_SPACING_INDEX, make_number (FONT_SPACING_PROPORTIONAL));
  else
    ASET (entity, FONT_SPACING_INDEX, make_number (FONT_SPACING_MONO));

  if (requested_font->lfQuality != DEFAULT_QUALITY)
    {
      font_put_extra (entity, QCantialias,
                      lispy_antialias_type (requested_font->lfQuality));
    }
  ASET (entity, FONT_FAMILY_INDEX,
        font_intern_prop (lf->lfFaceName, strlen (lf->lfFaceName)));

  FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		  make_number (w32_decode_weight (lf->lfWeight)));
  FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		  make_number (lf->lfItalic ? 200 : 100));
  /* TODO: PANOSE struct has this info, but need to call GetOutlineTextMetrics
     to get it.  */
  FONT_SET_STYLE (entity, FONT_WIDTH_INDEX, make_number (100));

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

  /* This information is not fully available when opening fonts, so
     save it here.  Only Windows 2000 and later return information
     about opentype and type1 fonts, so need a fallback for detecting
     truetype so that this information is not any worse than we could
     have obtained later.  */
  if (EQ (backend, Quniscribe) && (full_type & NTMFLAGS_OPENTYPE))
    tem = intern ("opentype");
  else if (font_type & TRUETYPE_FONTTYPE)
    tem = intern ("truetype");
  else if (full_type & NTM_PS_OPENTYPE)
    tem = intern ("postscript");
  else if (full_type & NTM_TYPE1)
    tem = intern ("type1");
  else if (font_type & RASTER_FONTTYPE)
    tem = intern ("w32bitmap");
  else
    tem = intern ("w32vector");

  font_put_extra (entity, QCformat, tem);

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
font_matches_spec (type, font, spec, backend, logfont)
     DWORD type;
     NEWTEXTMETRICEX *font;
     Lisp_Object spec;
     Lisp_Object backend;
     LOGFONT *logfont;
{
  Lisp_Object extra, val;

  /* Check italic. Can't check logfonts, since it is a boolean field,
     so there is no difference between "non-italic" and "don't care".  */
  {
    int slant = FONT_SLANT_NUMERIC (spec);

    if (slant >= 0
	&& ((slant > 150 && !font->ntmTm.tmItalic)
	    || (slant <= 150 && font->ntmTm.tmItalic)))
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

  /* Check spacing */
  val = AREF (spec, FONT_SPACING_INDEX);
  if (INTEGERP (val))
    {
      int spacing = XINT (val);
      int proportional = (spacing < FONT_SPACING_MONO);

      if ((proportional && !(font->ntmTm.tmPitchAndFamily & 0x01))
	  || (!proportional && (font->ntmTm.tmPitchAndFamily & 0x01)))
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
          if (EQ (key, QCscript) && SYMBOLP (val))
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
                    /* Other scripts unlikely to be handled by non-truetype
		       fonts.  */
                    return 0;
                }
            }
	  else if (EQ (key, QClang) && SYMBOLP (val))
	    {
	      /* Just handle the CJK languages here, as the language
		 parameter is used to select a font with appropriate
		 glyphs in the cjk unified ideographs block. Other fonts
	         support for a language can be solely determined by
	         its character coverage.  */
	      if (EQ (val, Qja))
		{
		  if (font->ntmTm.tmCharSet != SHIFTJIS_CHARSET)
		    return 0;
		}
	      else if (EQ (val, Qko))
		{
		  if (font->ntmTm.tmCharSet != HANGUL_CHARSET
		      && font->ntmTm.tmCharSet != JOHAB_CHARSET)
		    return 0;
		}
	      else if (EQ (val, Qzh))
		{
		  if (font->ntmTm.tmCharSet != GB2312_CHARSET
		      && font->ntmTm.tmCharSet != CHINESEBIG5_CHARSET)
		return 0;
		}
	      else
		/* Any other language, we don't recognize it. Fontset
		   spec should have a fallback, as some backends do
		   not recognize language at all.  */
		return 0;
	    }
          else if (EQ (key, QCotf) && CONSP (val))
	    {
	      /* OTF features only supported by the uniscribe backend.  */
	      if (EQ (backend, Quniscribe))
		{
		  if (!uniscribe_check_otf (logfont, val))
		    return 0;
		}
	      else
		return 0;
	    }
        }
    }
  return 1;
}

static int
w32font_coverage_ok (coverage, charset)
     FONTSIGNATURE * coverage;
     BYTE charset;
{
  DWORD subrange1 = coverage->fsUsb[1];

#define SUBRANGE1_HAN_MASK 0x08000000
#define SUBRANGE1_HANGEUL_MASK 0x01000000
#define SUBRANGE1_JAPANESE_MASK (0x00060000 | SUBRANGE1_HAN_MASK)

  if (charset == GB2312_CHARSET || charset == CHINESEBIG5_CHARSET)
    {
      return (subrange1 & SUBRANGE1_HAN_MASK) == SUBRANGE1_HAN_MASK;
    }
  else if (charset == SHIFTJIS_CHARSET)
    {
      return (subrange1 & SUBRANGE1_JAPANESE_MASK) == SUBRANGE1_JAPANESE_MASK;
    }
  else if (charset == HANGEUL_CHARSET)
    {
      return (subrange1 & SUBRANGE1_HANGEUL_MASK) == SUBRANGE1_HANGEUL_MASK;
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
  Lisp_Object backend = match_data->opentype_only ? Quniscribe : Qgdi;

  if ((!match_data->opentype_only
       || (physical_font->ntmTm.ntmFlags & NTMFLAGS_OPENTYPE))
      && logfonts_match (&logical_font->elfLogFont, &match_data->pattern)
      && font_matches_spec (font_type, physical_font,
                            match_data->orig_font_spec, backend,
			    &logical_font->elfLogFont)
      && w32font_coverage_ok (&physical_font->ntmFontSig,
                              match_data->pattern.lfCharSet)
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
                                       &match_data->pattern,
                                       backend);
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
  else
    return DEFAULT_CHARSET;
}

static Lisp_Object
w32_registry (w32_charset, font_type)
     LONG w32_charset;
     DWORD font_type;
{
  /* If charset is defaulted, use ANSI (unicode for truetype fonts).  */
  if (w32_charset == DEFAULT_CHARSET)
    w32_charset = ANSI_CHARSET;

  if (font_type == TRUETYPE_FONTTYPE && w32_charset == ANSI_CHARSET)
    return Qiso10646_1;
  else
    {
      char * charset = w32_to_x_charset (w32_charset, NULL);
      return font_intern_prop (charset, strlen(charset));
    }
}

static struct
{
  unsigned w32_numeric;
  unsigned numeric;
} w32_weight_table[] =
  { { FW_THIN, 0 },
    { FW_EXTRALIGHT, 40 },
    { FW_LIGHT, 50},
    { FW_NORMAL, 100},
    { FW_MEDIUM, 100},
    { FW_SEMIBOLD, 180},
    { FW_BOLD, 200},
    { FW_EXTRABOLD, 205},
    { FW_HEAVY, 210} };

static int
w32_decode_weight (fnweight)
     int fnweight;
{
  if (fnweight >= FW_HEAVY)      return 210;
  if (fnweight >= FW_EXTRABOLD)  return 205;
  if (fnweight >= FW_BOLD)       return 200;
  if (fnweight >= FW_SEMIBOLD)   return 180;
  if (fnweight >= FW_NORMAL)     return 100;
  if (fnweight >= FW_LIGHT)      return 50;
  if (fnweight >= FW_EXTRALIGHT) return 40;
  if (fnweight > FW_THIN)        return 20;
  return 0;
}

static int
w32_encode_weight (n)
     int n;
{
  if (n >= 210) return FW_HEAVY;
  if (n >= 205) return FW_EXTRABOLD;
  if (n >= 200) return FW_BOLD;
  if (n >= 180) return FW_SEMIBOLD;
  if (n >= 100) return FW_NORMAL;
  if (n >= 50)  return FW_LIGHT;
  if (n >= 40)  return FW_EXTRALIGHT;
  if (n >= 20)  return  FW_THIN;
  return 0;
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

  tmp = AREF (font_spec, FONT_DPI_INDEX);
  if (INTEGERP (tmp))
    {
      dpi = XINT (tmp);
    }
  else if (FLOATP (tmp))
    {
      dpi = (int) (XFLOAT_DATA (tmp) + 0.5);
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
    logfont->lfWeight = w32_encode_weight (FONT_WEIGHT_NUMERIC (font_spec));

  /* Italic  */
  tmp = AREF (font_spec, FONT_SLANT_INDEX);
  if (INTEGERP (tmp))
    {
      int slant = FONT_SLANT_NUMERIC (font_spec);
      logfont->lfItalic = slant > 150 ? 1 : 0;
    }

  /* Underline  */

  /* Strikeout  */

  /* Charset  */
  tmp = AREF (font_spec, FONT_REGISTRY_INDEX);
  if (! NILP (tmp))
    logfont->lfCharSet = registry_to_w32_charset (tmp);
  else
    logfont->lfCharSet = DEFAULT_CHARSET;

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
    }

  tmp = AREF (font_spec, FONT_ADSTYLE_INDEX);
  if (!NILP (tmp))
    {
      /* Override generic family.  */
      BYTE family = w32_generic_family (tmp);
      if (family != FF_DONTCARE)
        logfont->lfPitchAndFamily = family | DEFAULT_PITCH;
    }

					   
  /* Set pitch based on the spacing property.  */
  tmp = AREF (font_spec, FONT_SPACING_INDEX);
  if (INTEGERP (tmp))
    {
      int spacing = XINT (tmp);
      if (spacing < FONT_SPACING_MONO)
	logfont->lfPitchAndFamily
	  = logfont->lfPitchAndFamily & 0xF0 | VARIABLE_PITCH;
      else
	logfont->lfPitchAndFamily
	  = logfont->lfPitchAndFamily & 0xF0 | FIXED_PITCH;
    }

  /* Process EXTRA info.  */
  for (extra = AREF (font_spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      tmp = XCAR (extra);
      if (CONSP (tmp))
        {
          Lisp_Object key, val;
          key = XCAR (tmp), val = XCDR (tmp);
          /* Only use QCscript if charset is not provided, or is unicode
             and a single script is specified.  This is rather crude,
             and is only used to narrow down the fonts returned where
             there is a definite match.  Some scripts, such as latin, han,
             cjk-misc match multiple lfCharSet values, so we can't pre-filter
             them.  */
	  if (EQ (key, QCscript)
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
      else if (SYMBOLP (family))
        name = SDATA (SYMBOL_NAME (family)); 
      else
	continue;

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

  SUBRANGE (0, Qlatin);
  /* The following count as latin too, ASCII should be present in these fonts,
     so don't need to mark them separately.  */
  /* 1: Latin-1 supplement, 2: Latin Extended A, 3: Latin Extended B.  */
  SUBRANGE (4, Qphonetic);
  /* 5: Spacing and tone modifiers, 6: Combining Diacriticals.  */
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
  SUBRANGE (27, Qbalinese);
  /* 28: Hangul Jamo.  */
  /* 29: Latin Extended, 30: Greek Extended, 31: Punctuation.  */
  /* 32-47: Symbols (defined below).  */
  SUBRANGE (48, Qcjk_misc);
  /* Match either 49: katakana or 50: hiragana for kana.  */
  MASK_ANY (0, 0x00060000, 0, 0, Qkana);
  SUBRANGE (51, Qbopomofo);
  /* 52: Compatibility Jamo */
  SUBRANGE (53, Qphags_pa);
  /* 54: Enclosed CJK letters and months, 55: CJK Compatibility.  */
  SUBRANGE (56, Qhangul);
  /* 57: Surrogates.  */
  SUBRANGE (58, Qphoenician);
  SUBRANGE (59, Qhan); /* There are others, but this is the main one.  */
  SUBRANGE (59, Qideographic_description); /* Windows lumps this in.  */
  SUBRANGE (59, Qkanbun); /* And this.  */
  /* 60: Private use, 61: CJK strokes and compatibility.  */
  /* 62: Alphabetic Presentation, 63: Arabic Presentation A.  */
  /* 64: Combining half marks, 65: Vertical and CJK compatibility.  */
  /* 66: Small forms, 67: Arabic Presentation B, 68: Half and Full width.  */
  /* 69: Specials.  */
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
  SUBRANGE (84, Qbuhid);
  SUBRANGE (84, Qhanunoo);
  SUBRANGE (84, Qtagalog);
  SUBRANGE (84, Qtagbanwa);
  SUBRANGE (85, Qold_italic);
  SUBRANGE (86, Qgothic);
  SUBRANGE (87, Qdeseret);
  SUBRANGE (88, Qbyzantine_musical_symbol);
  SUBRANGE (88, Qmusical_symbol); /* Windows doesn't distinguish these.  */
  SUBRANGE (89, Qmathematical);
  /* 90: Private use, 91: Variation selectors, 92: Tags.  */
  SUBRANGE (93, Qlimbu);
  SUBRANGE (94, Qtai_le);
  /* 95: New Tai Le */
  SUBRANGE (90, Qbuginese);
  SUBRANGE (97, Qglagolitic);
  SUBRANGE (98, Qtifinagh);
  /* 99: Yijing Hexagrams.  */
  SUBRANGE (100, Qsyloti_nagri);
  SUBRANGE (101, Qlinear_b);
  /* 102: Ancient Greek Numbers.  */
  SUBRANGE (103, Qugaritic);
  SUBRANGE (104, Qold_persian);
  SUBRANGE (105, Qshavian);
  SUBRANGE (106, Qosmanya);
  SUBRANGE (107, Qcypriot);
  SUBRANGE (108, Qkharoshthi);
  /* 109: Tai Xuan Jing.  */
  SUBRANGE (110, Qcuneiform);
  /* 111: Counting Rods.  */

  /* There isn't really a main symbol range, so include symbol if any
     relevant range is set.  */
  MASK_ANY (0x8000000, 0x0000FFFF, 0, 0, Qsymbol);

  /* Missing: Tai Viet (U+AA80) and Cham (U+AA00) .  */
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
  int len, height, outline;
  char *p;
  Lisp_Object antialiasing, weight = Qnil;

  len = strlen (font->lfFaceName);

  outline = EQ (AREF (font_obj, FONT_FOUNDRY_INDEX), Qoutline);

  /* Represent size of scalable fonts by point size. But use pixelsize for
     raster fonts to indicate that they are exactly that size.  */
  if (outline)
    len += 11; /* -SIZE */
  else
    len = strlen (font->lfFaceName) + 21;

  if (font->lfItalic)
    len += 7; /* :italic */

  if (font->lfWeight && font->lfWeight != FW_NORMAL)
    {
      weight = FONT_WEIGHT_SYMBOLIC (font_obj);
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

  height = font->lfHeight ? eabs (font->lfHeight) : pixel_size;

  if (height > 0)
    {
      if (outline)
        {
          float pointsize = height * 72.0 / one_w32_display_info.resy;
          /* Round to nearest half point.  floor is used, since round is not
	     supported in MS library.  */
          pointsize = floor (pointsize * 2 + 0.5) / 2;
          p += sprintf (p, "-%1.1f", pointsize);
        }
      else
        p += sprintf (p, ":pixelsize=%d", height);
    }

  if (font->lfItalic)
    p += sprintf (p, ":italic");

  if (SYMBOLP (weight) && ! NILP (weight))
    p += sprintf (p, ":weight=%s", SDATA (SYMBOL_NAME (weight)));

  if (SYMBOLP (antialiasing) && ! NILP (antialiasing))
    p += sprintf (p, ":antialias=%s", SDATA (SYMBOL_NAME (antialiasing)));

  return (p - name);
}


static void compute_metrics (dc, w32_font, code, metrics)
     HDC dc;
     struct w32font_info *w32_font;
     unsigned int code;
     struct w32_metric_cache *metrics;
{
  GLYPHMETRICS gm;
  MAT2 transform;
  unsigned int options = GGO_METRICS;

  if (w32_font->glyph_idx)
    options |= GGO_GLYPH_INDEX;

  bzero (&transform, sizeof (transform));
  transform.eM11.value = 1;
  transform.eM22.value = 1;

  if (GetGlyphOutlineW (dc, code, options, &gm, 0, NULL, &transform)
      != GDI_ERROR)
    {
      metrics->lbearing = gm.gmptGlyphOrigin.x;
      metrics->rbearing = gm.gmptGlyphOrigin.x + gm.gmBlackBoxX;
      metrics->width = gm.gmCellIncX;
      metrics->status = W32METRIC_SUCCESS;
    }
  else
    {
      if (w32_font->glyph_idx)
	{
	  /* Can't use glyph indexes after all.
	     Avoid it in future, and clear any metrics that were based on
	     glyph indexes.  */
	  w32_font->glyph_idx = 0;
	  clear_cached_metrics (w32_font);
	}
      metrics->status = W32METRIC_FAIL;
    }
}

static void
clear_cached_metrics (w32_font)
     struct w32font_info *w32_font;
{
  int i;
  for (i = 0; i < w32_font->n_cache_blocks; i++)
    {
      if (w32_font->cached_metrics[i])
        bzero (w32_font->cached_metrics[i],
               CACHE_BLOCKSIZE * sizeof (struct font_metrics));
    }
}

struct font_driver w32font_driver =
  {
    0, /* Qgdi */
    0, /* case insensitive */
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
  DEFSYM (Quniscribe, "uniscribe");
  DEFSYM (QCformat, ":format");

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

  /* Languages  */
  DEFSYM (Qja, "ja");
  DEFSYM (Qko, "ko");
  DEFSYM (Qzh, "zh");

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
  DEFSYM (Qphonetic, "phonetic");
  DEFSYM (Qbalinese, "balinese");
  DEFSYM (Qbuginese, "buginese");
  DEFSYM (Qbuhid, "buhid");
  DEFSYM (Qcuneiform, "cuneiform");
  DEFSYM (Qcypriot, "cypriot");
  DEFSYM (Qdeseret, "deseret");
  DEFSYM (Qglagolitic, "glagolitic");
  DEFSYM (Qgothic, "gothic");
  DEFSYM (Qhanunoo, "hanunoo");
  DEFSYM (Qkharoshthi, "kharoshthi");
  DEFSYM (Qlimbu, "limbu");
  DEFSYM (Qlinear_b, "linear_b");
  DEFSYM (Qold_italic, "old_italic");
  DEFSYM (Qold_persian, "old_persian");
  DEFSYM (Qosmanya, "osmanya");
  DEFSYM (Qphags_pa, "phags-pa");
  DEFSYM (Qphoenician, "phoenician");
  DEFSYM (Qshavian, "shavian");
  DEFSYM (Qsyloti_nagri, "syloti_nagri");
  DEFSYM (Qtagalog, "tagalog");
  DEFSYM (Qtagbanwa, "tagbanwa");
  DEFSYM (Qtai_le, "tai_le");
  DEFSYM (Qtifinagh, "tifinagh");
  DEFSYM (Qugaritic, "ugaritic");

  w32font_driver.type = Qgdi;
  register_font_driver (&w32font_driver, NULL);
}

/* arch-tag: 65b8a3cd-46aa-4c0d-a1f3-99e75b9c07ee
   (do not change this comment) */
