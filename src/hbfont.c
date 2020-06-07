/* hbfont.c -- Platform-independent support for HarfBuzz font driver.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
#include <math.h>
#include <hb.h>
#include <hb-ot.h>

#include "lisp.h"
#include "frame.h"
#include "composite.h"
#include "font.h"
#include "dispextern.h"
#include "buffer.h"

#ifdef HAVE_NTGUI

#include "w32common.h"

/* The w32 implementation calls HarfBuzz functions via function
   pointers.  We use the below to declare the function pointers and
   redirect function names to those pointers.  */
DEF_DLL_FN (hb_unicode_funcs_t *, hb_unicode_funcs_create,
	    (hb_unicode_funcs_t *));
DEF_DLL_FN (hb_unicode_funcs_t *, hb_unicode_funcs_get_default, (void));
DEF_DLL_FN (void, hb_unicode_funcs_set_combining_class_func,
	    (hb_unicode_funcs_t *, hb_unicode_combining_class_func_t,
	     void *, hb_destroy_func_t));
DEF_DLL_FN (void, hb_unicode_funcs_set_general_category_func,
	    (hb_unicode_funcs_t *, hb_unicode_general_category_func_t,
	     void *, hb_destroy_func_t));
DEF_DLL_FN (void, hb_unicode_funcs_set_mirroring_func,
	    (hb_unicode_funcs_t *, hb_unicode_mirroring_func_t,
	     void *, hb_destroy_func_t));
DEF_DLL_FN (hb_buffer_t *, hb_buffer_create, (void));
DEF_DLL_FN (void, hb_buffer_set_unicode_funcs,
	    (hb_buffer_t *, hb_unicode_funcs_t *));
DEF_DLL_FN (void, hb_buffer_clear_contents, (hb_buffer_t *));
DEF_DLL_FN (hb_bool_t, hb_buffer_pre_allocate, (hb_buffer_t *, unsigned int));
DEF_DLL_FN (void, hb_buffer_add, (hb_buffer_t *, hb_codepoint_t, unsigned int));
DEF_DLL_FN (void, hb_buffer_set_content_type,
	    (hb_buffer_t *, hb_buffer_content_type_t));
DEF_DLL_FN (void, hb_buffer_set_cluster_level,
	    (hb_buffer_t *, hb_buffer_cluster_level_t));
DEF_DLL_FN (void, hb_buffer_set_direction, (hb_buffer_t *, hb_direction_t));
DEF_DLL_FN (void, hb_buffer_set_language, (hb_buffer_t *, hb_language_t));
DEF_DLL_FN (hb_language_t, hb_language_from_string, (const char *, int));
DEF_DLL_FN (void, hb_buffer_guess_segment_properties, (hb_buffer_t *));
DEF_DLL_FN (hb_bool_t, hb_shape_full,
	    (hb_font_t *, hb_buffer_t *, const hb_feature_t *,
	     unsigned int, const char * const *));
DEF_DLL_FN (unsigned int, hb_buffer_get_length, (hb_buffer_t *));
DEF_DLL_FN (hb_direction_t, hb_buffer_get_direction, (hb_buffer_t *));
DEF_DLL_FN (void, hb_buffer_reverse_clusters, (hb_buffer_t *));
DEF_DLL_FN (hb_glyph_info_t *, hb_buffer_get_glyph_infos,
	    (hb_buffer_t *, unsigned int *));
DEF_DLL_FN (hb_glyph_position_t *, hb_buffer_get_glyph_positions,
	    (hb_buffer_t *, unsigned int *));
DEF_DLL_FN (void, hb_tag_to_string, (hb_tag_t, char *));
DEF_DLL_FN (hb_face_t *, hb_font_get_face, (hb_font_t *font));
DEF_DLL_FN (unsigned int, hb_ot_layout_table_get_script_tags,
	    (hb_face_t *, hb_tag_t, unsigned int, unsigned int *, hb_tag_t *));
DEF_DLL_FN (unsigned int, hb_ot_layout_table_get_feature_tags,
	    (hb_face_t *, hb_tag_t, unsigned int, unsigned int *, hb_tag_t *));
DEF_DLL_FN (unsigned int, hb_ot_layout_script_get_language_tags,
	    (hb_face_t *, hb_tag_t, unsigned int, unsigned int, unsigned int *,
	     hb_tag_t *));
DEF_DLL_FN (unsigned int, hb_ot_layout_language_get_feature_tags,
	    (hb_face_t *, hb_tag_t, unsigned int, unsigned int, unsigned int,
	     unsigned int *, hb_tag_t *));

#define hb_unicode_funcs_create fn_hb_unicode_funcs_create
#define hb_unicode_funcs_get_default fn_hb_unicode_funcs_get_default
#define hb_unicode_funcs_set_combining_class_func fn_hb_unicode_funcs_set_combining_class_func
#define hb_unicode_funcs_set_general_category_func fn_hb_unicode_funcs_set_general_category_func
#define hb_unicode_funcs_set_mirroring_func fn_hb_unicode_funcs_set_mirroring_func
#define hb_buffer_create fn_hb_buffer_create
#define hb_buffer_set_unicode_funcs fn_hb_buffer_set_unicode_funcs
#define hb_buffer_clear_contents fn_hb_buffer_clear_contents
#define hb_buffer_pre_allocate fn_hb_buffer_pre_allocate
#define hb_buffer_add fn_hb_buffer_add
#define hb_buffer_set_content_type fn_hb_buffer_set_content_type
#define hb_buffer_set_cluster_level fn_hb_buffer_set_cluster_level
#define hb_buffer_set_direction fn_hb_buffer_set_direction
#define hb_buffer_set_language fn_hb_buffer_set_language
#define hb_language_from_string fn_hb_language_from_string
#define hb_buffer_guess_segment_properties fn_hb_buffer_guess_segment_properties
#define hb_shape_full fn_hb_shape_full
#define hb_buffer_get_length fn_hb_buffer_get_length
#define hb_buffer_get_direction fn_hb_buffer_get_direction
#define hb_buffer_reverse_clusters fn_hb_buffer_reverse_clusters
#define hb_buffer_get_glyph_infos fn_hb_buffer_get_glyph_infos
#define hb_buffer_get_glyph_positions fn_hb_buffer_get_glyph_positions
#define hb_tag_to_string fn_hb_tag_to_string
#define hb_font_get_face fn_hb_font_get_face
#define hb_ot_layout_table_get_script_tags fn_hb_ot_layout_table_get_script_tags
#define hb_ot_layout_table_get_feature_tags fn_hb_ot_layout_table_get_feature_tags
#define hb_ot_layout_script_get_language_tags fn_hb_ot_layout_script_get_language_tags
#define hb_ot_layout_language_get_feature_tags fn_hb_ot_layout_language_get_feature_tags

/* This function is called from syms_of_w32uniscribe_for_pdumper to
   initialize the above function pointers.  */
bool
hbfont_init_w32_funcs (HMODULE library)
{
  LOAD_DLL_FN (library, hb_unicode_funcs_create);
  LOAD_DLL_FN (library, hb_unicode_funcs_get_default);
  LOAD_DLL_FN (library, hb_unicode_funcs_set_combining_class_func);
  LOAD_DLL_FN (library, hb_unicode_funcs_set_general_category_func);
  LOAD_DLL_FN (library, hb_unicode_funcs_set_mirroring_func);
  LOAD_DLL_FN (library, hb_buffer_create);
  LOAD_DLL_FN (library, hb_buffer_set_unicode_funcs);
  LOAD_DLL_FN (library, hb_buffer_clear_contents);
  LOAD_DLL_FN (library, hb_buffer_pre_allocate);
  LOAD_DLL_FN (library, hb_buffer_add);
  LOAD_DLL_FN (library, hb_buffer_set_content_type);
  LOAD_DLL_FN (library, hb_buffer_set_cluster_level);
  LOAD_DLL_FN (library, hb_buffer_set_direction);
  LOAD_DLL_FN (library, hb_buffer_set_language);
  LOAD_DLL_FN (library, hb_language_from_string);
  LOAD_DLL_FN (library, hb_buffer_guess_segment_properties);
  LOAD_DLL_FN (library, hb_shape_full);
  LOAD_DLL_FN (library, hb_buffer_get_length);
  LOAD_DLL_FN (library, hb_buffer_get_direction);
  LOAD_DLL_FN (library, hb_buffer_reverse_clusters);
  LOAD_DLL_FN (library, hb_buffer_get_glyph_infos);
  LOAD_DLL_FN (library, hb_buffer_get_glyph_positions);
  LOAD_DLL_FN (library, hb_tag_to_string);
  LOAD_DLL_FN (library, hb_font_get_face);
  LOAD_DLL_FN (library, hb_ot_layout_table_get_script_tags);
  LOAD_DLL_FN (library, hb_ot_layout_table_get_feature_tags);
  LOAD_DLL_FN (library, hb_ot_layout_script_get_language_tags);
  LOAD_DLL_FN (library, hb_ot_layout_language_get_feature_tags);
  return true;
}
#endif	/* HAVE_NTGUI */

static Lisp_Object
hbfont_otf_features (hb_face_t *face, hb_tag_t table_tag)
{
  hb_tag_t *language_tags = NULL, *feature_tags = NULL;
  char buf[4];
  unsigned int script_count
    = hb_ot_layout_table_get_script_tags (face, table_tag, 0, NULL, NULL);
  hb_tag_t *script_tags = xnmalloc (script_count, sizeof *script_tags);
  hb_ot_layout_table_get_script_tags (face, table_tag, 0, &script_count,
				      script_tags);
  Lisp_Object scripts = Qnil;
  for (int i = script_count - 1; i >= 0; i--)
    {
      unsigned int language_count
	= hb_ot_layout_script_get_language_tags (face, table_tag, i, 0,
						 NULL, NULL);
      language_tags = xnrealloc (language_tags, language_count,
				 sizeof *language_tags);
      hb_ot_layout_script_get_language_tags (face, table_tag, i, 0,
					     &language_count, language_tags);
      Lisp_Object langsyses = Qnil;
      for (int j = language_count - 1; j >= -1; j--)
	{
	  unsigned int language_index
	    = j >= 0 ? j : HB_OT_LAYOUT_DEFAULT_LANGUAGE_INDEX;
	  unsigned int feature_count
	    = hb_ot_layout_language_get_feature_tags (face, table_tag,
						      i, language_index, 0,
						      NULL, NULL);
	  if (feature_count == 0)
	    continue;
	  feature_tags = xnrealloc (feature_tags, feature_count,
				    sizeof *feature_tags);
	  hb_ot_layout_language_get_feature_tags (face, table_tag,
						  i, language_index, 0,
						  &feature_count, feature_tags);
	  Lisp_Object features = Qnil;
	  for (int k = feature_count - 1; k >= 0; k--)
	    {
	      hb_tag_to_string (feature_tags[k], buf);
	      features = Fcons (font_intern_prop (buf, 4, 1), features);
	    }

	  Lisp_Object sym = Qnil;
	  if (j >= 0)
	    {
	      hb_tag_to_string (language_tags[j], buf);
	      sym = font_intern_prop (buf, 4, 1);
	    }
	  langsyses = Fcons (Fcons (sym, features), langsyses);
	}

      hb_tag_to_string (script_tags[i], buf);
      scripts = Fcons (Fcons (font_intern_prop (buf, 4, 1), langsyses),
		       scripts);
    }
  xfree (feature_tags);
  xfree (language_tags);
  xfree (script_tags);

  return scripts;
}

Lisp_Object
hbfont_otf_capability (struct font *font)
{
  double position_unit;
  hb_font_t *hb_font
    = font->driver->begin_hb_font
    ? font->driver->begin_hb_font (font, &position_unit)
    : NULL;
  if (!hb_font)
    return Qnil;

  Lisp_Object gsub_gpos = Fcons (Qnil, Qnil);
  hb_face_t *face = hb_font_get_face (hb_font);
  if (hb_ot_layout_table_get_feature_tags (face, HB_OT_TAG_GSUB, 0, NULL, NULL))
    XSETCAR (gsub_gpos, hbfont_otf_features (face, HB_OT_TAG_GSUB));
  if (hb_ot_layout_table_get_feature_tags (face, HB_OT_TAG_GPOS, 0, NULL, NULL))
    XSETCDR (gsub_gpos, hbfont_otf_features (face, HB_OT_TAG_GPOS));

  if (font->driver->end_hb_font)
    font->driver->end_hb_font (font, hb_font);

  return gsub_gpos;
}

/* Support functions for HarfBuzz shaper.  */

static bool combining_class_loaded = false;
static Lisp_Object canonical_combining_class_table;

static hb_unicode_combining_class_t
uni_combining (hb_unicode_funcs_t *funcs, hb_codepoint_t ch, void *user_data)
{
  /* Load the Unicode table first time it is needed.  */
  if (!combining_class_loaded)
    {
      canonical_combining_class_table =
	uniprop_table (intern ("canonical-combining-class"));
      if (NILP (canonical_combining_class_table))
	emacs_abort ();
      staticpro (&canonical_combining_class_table);
      combining_class_loaded = true;
    }

  Lisp_Object combining =
    get_unicode_property (canonical_combining_class_table, ch);
  if (FIXNUMP (combining))
    return (hb_unicode_combining_class_t) XFIXNUM (combining);

  return HB_UNICODE_COMBINING_CLASS_NOT_REORDERED;
}

static hb_unicode_general_category_t
uni_general (hb_unicode_funcs_t *funcs, hb_codepoint_t ch, void *user_data)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, ch);

  if (INTEGERP (category))
    {
    switch (XFIXNUM (category))
      {
      case UNICODE_CATEGORY_Cc:
        return HB_UNICODE_GENERAL_CATEGORY_CONTROL;
      case UNICODE_CATEGORY_Cf:
        return HB_UNICODE_GENERAL_CATEGORY_FORMAT;
      case UNICODE_CATEGORY_Cn:
        return HB_UNICODE_GENERAL_CATEGORY_UNASSIGNED;
      case UNICODE_CATEGORY_Co:
        return HB_UNICODE_GENERAL_CATEGORY_PRIVATE_USE;
      case UNICODE_CATEGORY_Cs:
        return HB_UNICODE_GENERAL_CATEGORY_SURROGATE;
      case UNICODE_CATEGORY_Ll:
        return HB_UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER;
      case UNICODE_CATEGORY_Lm:
        return HB_UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER;
      case UNICODE_CATEGORY_Lo:
        return HB_UNICODE_GENERAL_CATEGORY_OTHER_LETTER;
      case UNICODE_CATEGORY_Lt:
        return HB_UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER;
      case UNICODE_CATEGORY_Lu:
        return HB_UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER;
      case UNICODE_CATEGORY_Mc:
        return HB_UNICODE_GENERAL_CATEGORY_SPACING_MARK;
      case UNICODE_CATEGORY_Me:
        return HB_UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK;
      case UNICODE_CATEGORY_Mn:
        return HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK;
      case UNICODE_CATEGORY_Nd:
        return HB_UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER;
      case UNICODE_CATEGORY_Nl:
        return HB_UNICODE_GENERAL_CATEGORY_LETTER_NUMBER;
      case UNICODE_CATEGORY_No:
        return HB_UNICODE_GENERAL_CATEGORY_OTHER_NUMBER;
      case UNICODE_CATEGORY_Pc:
        return HB_UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION;
      case UNICODE_CATEGORY_Pd:
        return HB_UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION;
      case UNICODE_CATEGORY_Pe:
        return HB_UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION;
      case UNICODE_CATEGORY_Pf:
        return HB_UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION;
      case UNICODE_CATEGORY_Pi:
        return HB_UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION;
      case UNICODE_CATEGORY_Po:
        return HB_UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION;
      case UNICODE_CATEGORY_Ps:
        return HB_UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION;
      case UNICODE_CATEGORY_Sc:
        return HB_UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL;
      case UNICODE_CATEGORY_Sk:
        return HB_UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL;
      case UNICODE_CATEGORY_Sm:
        return HB_UNICODE_GENERAL_CATEGORY_MATH_SYMBOL;
      case UNICODE_CATEGORY_So:
        return HB_UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL;
      case UNICODE_CATEGORY_Zl:
        return HB_UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR;
      case UNICODE_CATEGORY_Zp:
        return HB_UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR;
      case UNICODE_CATEGORY_Zs:
        return HB_UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR;
      case UNICODE_CATEGORY_UNKNOWN:
        return HB_UNICODE_GENERAL_CATEGORY_UNASSIGNED;
      }
    }

  return HB_UNICODE_GENERAL_CATEGORY_UNASSIGNED;
}

static hb_codepoint_t
uni_mirroring (hb_unicode_funcs_t *funcs, hb_codepoint_t ch, void *user_data)
{
  return bidi_mirror_char (ch);
}

static hb_unicode_funcs_t *
get_hb_unicode_funcs (void)
{
  /* Subclass HarfBuzz's default Unicode functions and override functions that
   * use data Emacs can provide. This way changing Emacs data is reflected in
   * the shaped output. */
  hb_unicode_funcs_t *funcs = hb_unicode_funcs_create (hb_unicode_funcs_get_default ());

  hb_unicode_funcs_set_combining_class_func (funcs, uni_combining, NULL, NULL);
  hb_unicode_funcs_set_general_category_func (funcs, uni_general, NULL, NULL);
  hb_unicode_funcs_set_mirroring_func (funcs, uni_mirroring, NULL, NULL);

  /* Use default implementation for Unicode composition/decomposition.
     We might want to revisit this later.
  hb_unicode_funcs_set_compose_func (funcs, uni_compose, NULL, NULL);
  hb_unicode_funcs_set_decompose_func (funcs, uni_decompose, NULL, NULL);
  */

  /* Emacs own script mapping for characters differs from Unicode, so we want
   * to keep the default HarfBuzz's implementation here.
  hb_unicode_funcs_set_script_func (funcs, uni_script, NULL, NULL);
  */

  return funcs;
}

/* HarfBuzz implementation of shape for font backend.

   Shape text in LGSTRING.  See the docstring of
   'composition-get-gstring' for the format of LGSTRING.  If the
   (N+1)th element of LGSTRING is nil, input of shaping is from the
   1st to (N)th elements.  In each input glyph, FROM, TO, CHAR, and
   CODE are already set, but FROM and TO need adjustments according
   to the glyphs produced by the shaping function.
   DIRECTION is either L2R or R2L, or nil if unknown.  During
   redisplay, this comes from applying the UBA, is passed from
   composition_reseat_it, and is used by the HarfBuzz shaper.

   This function updates all fields of the input glyphs.  If the
   output glyphs (M) are more than the input glyphs (N), (N+1)th
   through (M)th elements of LGSTRING are updated possibly by making
   a new glyph object and storing it in LGSTRING.  If (M) is greater
   than the length of LGSTRING, nil should be returned.  In that case,
   this function is called again with a larger LGSTRING.  */
Lisp_Object
hbfont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  ptrdiff_t glyph_len = 0, text_len = LGSTRING_GLYPH_LEN (lgstring);
  ptrdiff_t i;

  hb_glyph_info_t *info;
  hb_glyph_position_t *pos;

  /* Cache the HarfBuzz buffer for better performance and less allocations.
   * We intentionally never destroy the buffer. */
  static hb_buffer_t *hb_buffer = NULL;
  if (! hb_buffer)
    {
      hb_buffer = hb_buffer_create ();
      hb_unicode_funcs_t* ufuncs = get_hb_unicode_funcs();
      hb_buffer_set_unicode_funcs(hb_buffer, ufuncs);
    }

  hb_buffer_clear_contents (hb_buffer);
  hb_buffer_pre_allocate (hb_buffer, text_len);

  /* Copy the characters in their original logical order, so we can
     assign them to glyphs correctly after shaping.  */
  int *chars = alloca (text_len * sizeof (int));
  for (i = 0; i < text_len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (lgstring, i);
      int c;

      if (NILP (g))
	break;
      c = LGLYPH_CHAR (g);
      hb_buffer_add (hb_buffer, c, i);
      chars[i] = c;
    }

  text_len = i;
  if (!text_len)
    return Qnil;

  hb_buffer_set_content_type (hb_buffer, HB_BUFFER_CONTENT_TYPE_UNICODE);
  hb_buffer_set_cluster_level (hb_buffer,
			       HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES);

  /* If the caller didn't provide a meaningful DIRECTION, let HarfBuzz
     guess it. */
  if (!NILP (direction)
      /* If they bind bidi-display-reordering to nil, the DIRECTION
	 they provide is meaningless, and we should let HarfBuzz guess
	 the real direction.  */
      && !NILP (BVAR (current_buffer, bidi_display_reordering)))
    {
      hb_direction_t dir = HB_DIRECTION_LTR;
      if (EQ (direction, QL2R))
	dir = HB_DIRECTION_LTR;
      else if (EQ (direction, QR2L))
	dir = HB_DIRECTION_RTL;
      hb_buffer_set_direction (hb_buffer, dir);
    }

  /* Leave the script determination to HarfBuzz, until Emacs has a
     better idea of the script of LGSTRING.  FIXME. */
#if 0
  hb_buffer_set_script (hb_buffer, XXX);
#endif

  /* FIXME: This can only handle the single global language, which
     normally comes from the locale.  In addition, if
     current-iso639-language is a list, we arbitrarily use the first
     one.  We should instead have a notion of the language of the text
     being shaped.  */
  Lisp_Object lang = Vcurrent_iso639_language;
  if (CONSP (Vcurrent_iso639_language))
    lang = XCAR (Vcurrent_iso639_language);
  if (SYMBOLP (lang))
    {
      Lisp_Object lang_str = SYMBOL_NAME (lang);
      hb_buffer_set_language (hb_buffer,
			      hb_language_from_string (SSDATA (lang_str),
						       SBYTES (lang_str)));
    }

  /* Guess the default properties for when they cannot be determined above.

     FIXME: maybe drop this guessing once script and language handling
     is fixed above; but then will need to guess the direction by
     ourselves, perhaps by looking at the characters using
     bidi_get_type or somesuch.  */
  hb_buffer_guess_segment_properties (hb_buffer);

  double position_unit;
  hb_font_t *hb_font
    = font->driver->begin_hb_font
    ? font->driver->begin_hb_font (font, &position_unit)
    : NULL;
  if (!hb_font)
    return make_fixnum (0);

  hb_bool_t success = hb_shape_full (hb_font, hb_buffer, NULL, 0, NULL);
  if (font->driver->end_hb_font)
    font->driver->end_hb_font (font, hb_font);
  if (!success)
    return Qnil;

  glyph_len = hb_buffer_get_length (hb_buffer);
  if (glyph_len > LGSTRING_GLYPH_LEN (lgstring))
    return Qnil;

  /* We need the clusters in logical order.  */
  bool buf_reversed = false;
  if (HB_DIRECTION_IS_BACKWARD (hb_buffer_get_direction (hb_buffer)))
    {
      buf_reversed = true;
      hb_buffer_reverse_clusters (hb_buffer);
    }
  info = hb_buffer_get_glyph_infos (hb_buffer, NULL);
  pos = hb_buffer_get_glyph_positions (hb_buffer, NULL);
  ptrdiff_t from = -1, to UNINIT, cluster_offset UNINIT;
  int incr = buf_reversed ? -1 : 1;
  for (i = 0; i < glyph_len; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      struct font_metrics metrics = {.width = 0};
      int xoff, yoff, wadjust;
      bool new_lglyph = false;

      if (NILP (lglyph))
	{
	  new_lglyph = true;
	  lglyph = LGLYPH_NEW ();
	  LGSTRING_SET_GLYPH (lgstring, i, lglyph);
	}

      if (info[i].cluster != from)
	{
	  int j;
	  /* Found a new cluster.  Determine its FROM and TO, and the
	     offset to the first character of the cluster.  */
	  /* FROM is the index of the first character that contributed
	     to this cluster.  */
	  from = info[i].cluster;
	  /* TO is the index of the last character that contributed to
	     this cluster.  */
	  for (j = i; j < glyph_len && info[j].cluster == from; j++)
	    ;
	  to = (j == glyph_len) ? text_len - 1 : info[j].cluster - 1;
	  cluster_offset = 0;
	  /* For RTL buffers, HarfBuzz produces glyphs in a cluster in
	     reverse order, so we need to account for that to record
	     the correct character in each glyph.

	     Implementation note: the character codepoint recorded in
	     each glyph is not really used, except when we display the
	     glyphs in descr-text.el.  So this is just an aesthetic
	     issue.  */
	  if (buf_reversed)
	    cluster_offset = to - from;
	}

      /* All the glyphs in a cluster have the same values of FROM and TO.  */
      LGLYPH_SET_FROM (lglyph, from);
      /* This heuristic is for when the Lisp shape-gstring function
	 substitutes known precomposed characters for decomposed
	 sequences.  E.g., hebrew.el does that.  This makes TEXT_LEN
	 be smaller than the original length of the composed character
	 sequence.  In that case, we must not alter the largest TO,
	 because the display engine must know that all the characters
	 in the original sequence were processed by the composition.
	 If we don't do this, some of the composed characters will be
	 displayed again as separate glyphs.  */
      if (!(!new_lglyph
	    && to == text_len - 1
	    && LGLYPH_TO (lglyph) > to))
	LGLYPH_SET_TO (lglyph, to);

      /* Not every glyph in a cluster maps directly to a single
	 character; in general, N characters can yield M glyphs, where
	 M could be smaller or greater than N.  However, in many cases
	 there is a one-to-one correspondence, and it would be a pity
	 to lose that information, even if it's sometimes inaccurate.  */
      ptrdiff_t char_idx = from + cluster_offset;
      cluster_offset += incr;
      if (char_idx > to)
	char_idx = to;
      if (char_idx < from)
	char_idx = from;
      LGLYPH_SET_CHAR (lglyph, chars[char_idx]);
      LGLYPH_SET_CODE (lglyph, info[i].codepoint);

      unsigned code = info[i].codepoint;
      font->driver->text_extents (font, &code, 1, &metrics);
      LGLYPH_SET_WIDTH (lglyph, metrics.width);
      LGLYPH_SET_LBEARING (lglyph, metrics.lbearing);
      LGLYPH_SET_RBEARING (lglyph, metrics.rbearing);
      LGLYPH_SET_ASCENT (lglyph, metrics.ascent);
      LGLYPH_SET_DESCENT (lglyph, metrics.descent);

      xoff = lround (pos[i].x_offset * position_unit);
      yoff = - lround (pos[i].y_offset * position_unit);
      wadjust = lround (pos[i].x_advance * position_unit);
      if (xoff || yoff || wadjust != metrics.width)
	{
	  Lisp_Object vec = make_uninit_vector (3);
	  ASET (vec, 0, make_fixnum (xoff));
	  ASET (vec, 1, make_fixnum (yoff));
	  ASET (vec, 2, make_fixnum (wadjust));
	  LGLYPH_SET_ADJUSTMENT (lglyph, vec);
	}
    }

  return make_fixnum (glyph_len);
}

Lisp_Object
hbfont_combining_capability (struct font *font)
{
  return Qt;
}
