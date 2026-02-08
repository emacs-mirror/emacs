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

#include <config.h>

#include <fcntl.h>
#include <c-ctype.h>

#include "lisp.h"

#include "blockinput.h"
#include "charset.h"
#include "coding.h"
#include "font.h"
#include "frame.h"
#include "math.h"
#include "sfnt.h"
#include "sfntfont.h"

#ifdef HAVE_HARFBUZZ
#include <hb.h>
#include <hb-ot.h>
#endif /* HAVE_HARFBUZZ */

/* For FRAME_FONT.  */
#include TERM_HEADER

/* Generic font driver for sfnt-based fonts (currently TrueType, but
   it would be easy to add CFF support in the future with a PostScript
   renderer.)

   This is not a complete font driver.  Hooks must be supplied by the
   platform implementer to draw glyphs.  */



/* Tables associated with each font, be it distortable or not.  This
   allows different font objects sharing the same underlying font file
   to share tables.  */

struct sfnt_font_tables
{
  /* Various tables required to use the font.  */
  struct sfnt_cmap_table *cmap;
  struct sfnt_hhea_table *hhea;
  struct sfnt_maxp_table *maxp;
  struct sfnt_head_table *head;
  struct sfnt_hmtx_table *hmtx;
  struct sfnt_glyf_table *glyf;
  struct sfnt_loca_table_short *loca_short;
  struct sfnt_loca_table_long *loca_long;
  struct sfnt_prep_table *prep;
  struct sfnt_fpgm_table *fpgm;
  struct sfnt_cvt_table *cvt;
  struct sfnt_fvar_table *fvar;
  struct sfnt_avar_table *avar;
  struct sfnt_gvar_table *gvar;
  struct sfnt_cvar_table *cvar;

  /* The selected character map.  */
  struct sfnt_cmap_encoding_subtable_data *cmap_data;

  /* Data identifying that character map.  */
  struct sfnt_cmap_encoding_subtable cmap_subtable;

  /* The UVS context.  */
  struct sfnt_uvs_context *uvs;

#ifdef HAVE_MMAP
  /* Whether or not the glyph table has been mmapped.  */
  bool glyf_table_mapped;
#endif /* HAVE_MMAP */

#ifdef HAVE_HARFBUZZ
  /* File descriptor associated with this font.  */
  int fd;

  /* The table directory of the font file.  */
  struct sfnt_offset_subtable *directory;
#endif /* HAVE_HARFBUZZ */
};

/* Description of a font that hasn't been opened.  */

struct sfnt_font_desc
{
  /* Next font in this list.  */
  struct sfnt_font_desc *next;

  /* Family name of the font.  */
  Lisp_Object family;

  /* Style name of the font.  */
  Lisp_Object style;

  /* The font foundry name, or `misc' if not present.  */
  Lisp_Object designer;

  /* Style tokens that could not be parsed.  */
  Lisp_Object adstyle;

  /* List of design languages.  */
  Lisp_Object languages;

  /* Font registry that this font supports.  */
  Lisp_Object registry;

  /* Vector of instances.  Each element is another of the instance's
     `style', `adstyle', and numeric width, weight, and slant.  May be
     nil.  */
  Lisp_Object instances;

  /* Numeric width, weight, slant and spacing.  */
  int width, weight, slant, spacing;

  /* Path to the font file.  */
  char *path;

  /* char table consisting of characters already known to be
     present in the font.  */
  Lisp_Object char_cache;

  /* The header of the cmap being used.  May be invalid, in which case
     platform_id will be 500.  */
  struct sfnt_cmap_encoding_subtable subtable;

  /* The offset of the table directory within PATH.  */
  off_t offset;

  /* List of font tables.  */
  struct sfnt_font_tables *tables;

  /* The number of glyphs in this font.  Used to catch invalid cmap
     tables.  This is actually the number of glyphs - 1.  */
  int num_glyphs;

  /* The number of references to the font tables below.  */
  int refcount;

  /* The underline position and thickness if a post table supplies
     this information.  */
  sfnt_fword underline_position, underline_thickness;

  /* Whether an underline position is available.  */
  bool_bf underline_position_set : 1;

  /* Whether or not the character map can't be used by Emacs.  */
  bool cmap_invalid : 1;
};

/* List of fonts.  */

static struct sfnt_font_desc *system_fonts;

/* Font enumeration and matching.  The sfnt driver assumes it can read
   data from each font at startup.  It then reads the head, meta and
   name tables to determine font data, and records the font in a list
   of system fonts that is then matched against.  */

/* Set up the coding system CODING to decode string data from the
   given platform id ID and platform specific id PLATFORM_SPECIFIC_ID.
   Value is 0 upon success, 1 upon failure.  */

static int
sfnt_setup_coding_system (enum sfnt_platform_id id, int platform_specific_id,
			  struct coding_system *coding)
{
  Lisp_Object system;

  system = Qnil;

  /* Figure out what coding system to use.  */

  switch (id)
    {
    case SFNT_PLATFORM_UNICODE:
      system = Qutf_16be;
      break;

    case SFNT_PLATFORM_MACINTOSH:

      if (platform_specific_id == SFNT_MACINTOSH_ROMAN)
	system = Qmac_roman;
      else
	/* MULE doesn't support the rest... */
	system = Qnil;

      break;

    case SFNT_PLATFORM_MICROSOFT:
      system = Qutf_16be;

      /* Not sure if this is right.  */
      if (platform_specific_id == SFNT_MICROSOFT_BIG_FIVE)
	system = Qchinese_big5;

      break;

    default:
      system = Qnil;
    }

  if (NILP (system))
    return 1;

  setup_coding_system (system, coding);
  return 0;
}

/* Globals used to communicate inside the condition-case wrapper.  */
static struct coding_system *sfnt_font_coding;

/* The src_object being encoded from.  This should be on the stack as
   well, or it will get garbage collected.  */
static Lisp_Object sfnt_font_src_object;

/* From-position.  */
static ptrdiff_t sfnt_font_from, sfnt_font_from_byte;

/* To-position.  */
static ptrdiff_t sfnt_font_to, sfnt_font_to_byte;

/* Destination object.  Once again, this should also be on the
   stack.  */
static Lisp_Object sfnt_font_dst_object;

/* Error flag.  Set to true if a signal was caught.  */
static bool sfnt_font_signal;

static Lisp_Object
sfnt_safe_decode_coding_object_1 (void)
{
  decode_coding_object (sfnt_font_coding,
			sfnt_font_src_object,
			sfnt_font_from,
			sfnt_font_from_byte,
			sfnt_font_to,
			sfnt_font_to_byte,
			sfnt_font_dst_object);
  return Qnil;
}

static Lisp_Object
sfnt_safe_decode_coding_object_2 (Lisp_Object error)
{
  sfnt_font_signal = true;

  return Qnil;
}

/* Like decode_coding_object, but return 1 if a signal happens.  Value
   is otherwise 0.  */

static int
sfnt_safe_decode_coding_object (struct coding_system *coding,
				Lisp_Object src_object,
				ptrdiff_t from, ptrdiff_t from_byte,
				ptrdiff_t to, ptrdiff_t to_byte,
				Lisp_Object dst_object)
{
  sfnt_font_coding = coding;
  sfnt_font_src_object = src_object;
  sfnt_font_from = from;
  sfnt_font_from_byte = from_byte;
  sfnt_font_to = to;
  sfnt_font_to_byte = to_byte;
  sfnt_font_dst_object = dst_object;
  sfnt_font_signal = false;

  internal_condition_case (sfnt_safe_decode_coding_object_1,
			   Qt,
			   sfnt_safe_decode_coding_object_2);

  return (int) sfnt_font_signal;
}

/* Decode the specified string DATA.  The encoding is determined based
   on PLATFORM_ID, PLATFORM_SPECIFIC_ID and LANGUAGE_ID.  Consult
   sfnt.h and the TrueType Reference Manual for more details.  LENGTH
   is the length of DATA in bytes.

   Value is nil upon failure, else the decoded string.  */

static Lisp_Object
sfnt_decode_font_string (unsigned char *data, enum sfnt_platform_id id,
			 int platform_specific_id, int language_id,
			 size_t length)
{
  struct coding_system coding;

  memset (&coding, 0, sizeof coding);
  sfnt_setup_coding_system (id, platform_specific_id, &coding);
  coding.mode |= CODING_MODE_SAFE_ENCODING;
  coding.mode |= CODING_MODE_LAST_BLOCK;
  /* Suppress producing escape sequences for composition.  */
  coding.common_flags &= ~CODING_ANNOTATION_MASK;
  coding.source = data;

  if (sfnt_safe_decode_coding_object (&coding, Qnil, 0, 0,
				      length, length, Qt))
    return Qnil;

  return coding.dst_object;
}

/* Decode the family and style names from the name table NAME.  Return
   0 and the family and style names upon success, else 1.  */

static int
sfnt_decode_family_style (struct sfnt_name_table *name,
			  Lisp_Object *family, Lisp_Object *style)
{
  struct sfnt_name_record family_rec, style_rec;
  unsigned char *family_data, *style_data;

  /* Because MS-Windows is incapable of treating font families
     comprising more than four styles correctly, the TrueType
     specification incorporates additional PREFERRED_FAMILY and
     PREFERRED_SUBFAMILY name resources that are meant to be consulted
     over the traditional family and subfamily resources.  When
     present within fonts supplying unusual styles, these names hold
     the ``actual'' typographic family and style of the font, in lieu
     of the font family with the style affixed to the front and
     Regular.  */

  family_data = sfnt_find_name (name, SFNT_NAME_PREFERRED_FAMILY,
				&family_rec);

  if (!family_data)
    family_data = sfnt_find_name (name, SFNT_NAME_FONT_FAMILY,
				  &family_rec);

  style_data = sfnt_find_name (name, SFNT_NAME_PREFERRED_SUBFAMILY,
			       &style_rec);

  if (!style_data)
    style_data = sfnt_find_name (name, SFNT_NAME_FONT_SUBFAMILY,
				 &style_rec);

  if (!family_data || !style_data)
    return 1;

  /* Now decode the data.  */
  *family = sfnt_decode_font_string (family_data,
				     family_rec.platform_id,
				     family_rec.platform_specific_id,
				     family_rec.language_id,
				     family_rec.length);
  *style = sfnt_decode_font_string (style_data,
				    style_rec.platform_id,
				    style_rec.platform_specific_id,
				    style_rec.language_id,
				    style_rec.length);

  /* Return whether or not it was successful.  */
  return (!NILP (*family) && !NILP (*style)) ? 0 : 1;
}

/* Decode the name of the specified font INSTANCE using the given NAME
   table.  Return the name of that instance, or nil upon failure.  */

static Lisp_Object
sfnt_decode_instance_name (struct sfnt_instance *instance,
			   struct sfnt_name_table *name)
{
  struct sfnt_name_record name_rec;
  unsigned char *name_data;

  name_data = sfnt_find_name (name, instance->name_id,
			      &name_rec);

  if (!name_data)
    return Qnil;

  return sfnt_decode_font_string (name_data,
				  name_rec.platform_id,
				  name_rec.platform_specific_id,
				  name_rec.language_id,
				  name_rec.length);
}

struct sfnt_style_desc
{
  /* The C string to match against.  */
  const char *c_string;

  /* The value of the style field.  */
  int value;
};

/* Array of style descriptions describing weight.  */
static struct sfnt_style_desc sfnt_weight_descriptions[] =
  {
    { "thin", 0,		},
    { "extralight", 40,		},
    { "ultralight", 40,		},
    { "light", 50,		},
    { "demilight", 55,		},
    { "semilight", 55,		},
    { "book", 75,		},
    { "medium", 100,		},
    { "demibold", 180,		},
    { "semibold", 180,		},
    { "bold", 200,		},
    { "extrabold", 205,		},
    { "ultrabold", 205,		},
    { "black", 210,		},
    { "heavy", 210,		},
    { "extrablack", 215,	},
    { "ultrablack", 215,	},
  };

/* Array of style descriptions describing slant.  */
static struct sfnt_style_desc sfnt_slant_descriptions[] =
  {
    { "italic", 200,	},
    { "oblique", 210,	},
  };

/* Array of style descriptions describing width.  */
static struct sfnt_style_desc sfnt_width_descriptions[] =
  {
    { "ultracondensed", 50,	},
    { "extracondensed", 63,	},
    { "condensed", 75,		},
    { "semicondensed", 87,	},
    { "semiexpanded", 113,	},
    { "expanded", 125,		},
    { "extraexpanded", 150,	},
    { "ultraexpanded", 200,	},
  };

/* Figure out DESC->width, DESC->weight, DESC->slant and DESC->spacing
   based on the style name passed as STYLE_NAME.

   Also append any unknown tokens to DESC->adstyle.  */

static void
sfnt_parse_style (Lisp_Object style_name, struct sfnt_font_desc *desc)
{
  char *style, *single, *saveptr, c;
  int i;
  ptrdiff_t x;
  USE_SAFE_ALLOCA;

  /* Fill in default values.  slant seems to not be consistent with
     Fontconfig.  */
  desc->weight = 80;
  desc->slant = 100;
  desc->width = 100;

  /* Split the style into tokens delimited by spaces.  Attempt to find
     a token specifying each of the weight, slant, or width attributes
     using their respective descriptions arrays as a reference.  */

  SAFE_ALLOCA_STRING (style, Fdowncase (style_name));
  saveptr = NULL;

  while ((single = strtok_r (style, " ", &saveptr)))
    {
      style = NULL;

      if (!strcmp (single, "regular"))
	/* ``Regular'' within a font family can represent either the
	   weight, slant or width of the font.  Leave each value as
	   its default, but never append it to the adstyle.  */
	goto next;

      if (desc->weight == 80)
	{
	  /* Weight hasn't been found yet.  Scan through the weight
	     table.  */
	  for (i = 0; i < ARRAYELTS (sfnt_weight_descriptions); ++i)
	    {
	      if (!strcmp (sfnt_weight_descriptions[i].c_string,
			   single))
		{
		  /* Weight found.  Continue on reading the slant and
		     width.  */
		  desc->weight = sfnt_weight_descriptions[i].value;
		  goto next;
		}
	    }
	}

      if (desc->slant == 100)
	{
	  /* Slant hasn't been found yet.  Scan through the slant
	     table.  */
	  for (i = 0; i < ARRAYELTS (sfnt_slant_descriptions); ++i)
	    {
	      if (!strcmp (sfnt_slant_descriptions[i].c_string,
			   single))
		{
		  /* Slant found.  Continue on reading the weight and
		     width.  */
		  desc->slant = sfnt_slant_descriptions[i].value;
		  goto next;
		}
	    }
	}

      if (desc->width == 100)
	{
	  /* Width hasn't been found yet.  Scan through the width
	     table.  */
	  for (i = 0; i < ARRAYELTS (sfnt_width_descriptions); ++i)
	    {
	      if (!strcmp (sfnt_width_descriptions[i].c_string,
			   single))
		{
		  /* Width found.  Continue on reading the slant and
		     weight.  */
		  desc->width = sfnt_width_descriptions[i].value;
		  goto next;
		}
	    }
	}

      /* This token is extraneous or was not recognized.  Capitalize
	 the first letter if it's ASCII lowercase, then set the token as
	 the adstyle.  */

      if (strlen (single))
	{
	  single[0] = c_toupper (single[0]);

	  if (NILP (desc->adstyle))
	    desc->adstyle = build_string (single);
	  else
	    desc->adstyle = CALLN (Fconcat, desc->adstyle,
				   build_string (" "),
				   build_string (single));
	}

    next:
      continue;
    }

  /* The adstyle must be a symbol, so intern it if it is set.  */

  if (!NILP (desc->adstyle))
    {
      /* Characters that can't be represented in an XLFD must be
	 replaced.  */

      for (x = 0; x < SBYTES (desc->adstyle); ++x)
	{
	  c = SREF (desc->adstyle, x);
	  if (c == '-' || c == '*' || c == '?' || c == '"')
	    SSET (desc->adstyle, x, ' ');
	}

      desc->adstyle = Fintern (desc->adstyle, Qnil);
    }

  SAFE_FREE ();
}

/* Parse the list of design languages in META, a font metadata table,
   and place the results in DESC->languages.  Do nothing if there is
   no such metadata.  */

static void
sfnt_parse_languages (struct sfnt_meta_table *meta,
		      struct sfnt_font_desc *desc)
{
  char *data, *metadata, *tag;
  struct sfnt_meta_data_map map;
  char *saveptr;

  /* Look up the ``design languages'' metadata.  This is a comma (and
     possibly space) separated list of scripts that the font was
     designed for.  Here is an example of one such tag:

       zh-Hans,Jpan,Kore

     for a font that covers Simplified Chinese, along with Japanese
     and Korean text.  */

  saveptr = NULL;
  data = sfnt_find_metadata (meta, SFNT_META_DATA_TAG_DLNG,
			     &map);

  if (!data)
    {
      /* Fall back to the supported languages metadata.  */
      data = sfnt_find_metadata (meta, SFNT_META_DATA_TAG_SLNG,
				 &map);

      if (!data)
	return;
    }

  USE_SAFE_ALLOCA;

  /* Now copy metadata and add a trailing NULL byte.  */

  if (map.data_length >= SIZE_MAX)
    memory_full (SIZE_MAX);

  metadata = SAFE_ALLOCA ((size_t) map.data_length + 1);
  memcpy (metadata, data, map.data_length);
  metadata[map.data_length] = '\0';

  /* Loop through each script-language tag.  Note that there may be
     extra leading spaces.  */
  while ((tag = strtok_r (metadata, ",", &saveptr)))
    {
      metadata = NULL;

      if (strstr (tag, "Hans") || strstr (tag, "Hant"))
	desc->languages = Fcons (Qzh, desc->languages);

      if (strstr (tag, "Japn"))
	desc->languages = Fcons (Qja, desc->languages);

      if (strstr (tag, "Kore"))
	desc->languages = Fcons (Qko, desc->languages);
    }

  SAFE_FREE ();
}

/* Return the font registry corresponding to the encoding subtable
   SUBTABLE.

   Under X, the font registry is an atom registered with the Open
   Group uniquely identifying the organization which defines the
   font's character set.

   In practice, the registry overlaps with the character set itself.
   So Emacs just uses the ``registry'' field of each font object and
   entity to represent both instead.  */

static Lisp_Object
sfnt_registry_for_subtable (struct sfnt_cmap_encoding_subtable *subtable)
{
  switch (subtable->platform_id)
    {
    case SFNT_PLATFORM_UNICODE:
      /* Reject variation selector and last resort tables.  */
      if ((subtable->platform_specific_id
	   == SFNT_UNICODE_VARIATION_SEQUENCES)
	  || (subtable->platform_specific_id
	      == SFNT_UNICODE_LAST_RESORT))
	return Qnil;

      return Qiso10646_1;

    case SFNT_PLATFORM_MACINTOSH:

      switch (subtable->platform_specific_id)
	{
	case SFNT_MACINTOSH_ROMAN:
	  /* X calls mac-roman ``apple-roman''.  */
	  return Qapple_roman;

	default:
	  /* Some other Macintosh charset not supported by Emacs.  */
	  return Qnil;
	}

    case SFNT_PLATFORM_MICROSOFT:

      /* Microsoft specific encodings.  */

      switch (subtable->platform_specific_id)
	{
	case SFNT_MICROSOFT_SYMBOL:
	case SFNT_MICROSOFT_UNICODE_BMP:
	  /* Symbols in the Unicode PUA are still Unicode.  */
	  return Qiso10646_1;

	case SFNT_MICROSOFT_SHIFT_JIS:
	  return Qjisx0208_1983_0;

	case SFNT_MICROSOFT_PRC:
	  return Qgbk;

	case SFNT_MICROSOFT_JOHAB:
	  return Qksc5601_1987_0;

	case SFNT_MICROSOFT_UNICODE_UCS_4:
	  return Qiso10646_1;
	}

    default:
      return Qnil;
    }
}

/* Return the type of characters that the cmap subtable SUBTABLE maps
   from.  Value is:

   2 if SUBTABLE maps from Unicode characters, including those outside
   the Unicode Basic Multilingual Plane (BMP).

   1 if SUBTABLE maps from Unicode characters within the BMP.

   0 if SUBTABLE maps from some other character set that Emacs knows
   about.

   3 if SUBTABLE cannot be used by Emacs.  */

static int
sfntfont_identify_cmap (struct sfnt_cmap_encoding_subtable subtable)
{
  switch (subtable.platform_id)
    {
    case SFNT_PLATFORM_UNICODE:

      /* Reject variation selector and last resort tables.  */
      if ((subtable.platform_specific_id
	   == SFNT_UNICODE_VARIATION_SEQUENCES)
	  || (subtable.platform_specific_id
	      == SFNT_UNICODE_LAST_RESORT))
	return 3;

      /* 1.0, 1.1, ISO-10646-1993, and 2.0_BMP tables are all within
	 the BMP.  */
      if (subtable.platform_specific_id < SFNT_UNICODE_2_0)
	return 1;

      return 2;

    case SFNT_PLATFORM_MACINTOSH:

      switch (subtable.platform_specific_id)
	{
	case SFNT_MACINTOSH_ROMAN:
	  /* mac-roman */
	  return 0;

	default:
	  /* Some other Macintosh charset not supported by Emacs.  */
	  return 3;
	}

    case SFNT_PLATFORM_MICROSOFT:

      /* Microsoft specific encodings.  */

      switch (subtable.platform_specific_id)
	{
	case SFNT_MICROSOFT_SYMBOL:
	  /* Symbols in the Unicode PUA are still Unicode.  */
	  return 1;

	case SFNT_MICROSOFT_UNICODE_BMP:
	  return 1;

	case SFNT_MICROSOFT_SHIFT_JIS:
	  /* PCK aka japanese-jisx0208.  */
	  return 0;

	case SFNT_MICROSOFT_PRC:
	  /* GBK, GB2312 or GB18030.  */
	  return 0;

	case SFNT_MICROSOFT_JOHAB:
	  /* KS C 5601-1992, aka korean-ksc5601.  */
	  return 0;

	case SFNT_MICROSOFT_UNICODE_UCS_4:
	  /* Unicode past the BMP.  */
	  return 2;
	}

    default:
      return 3;
    }
}

/* Figure out which registry DESC, backed by FD, whose table directory
   is SUBTABLE, is likely to support.

   Read the header of each subtable in the character map and compute
   the registry to use; then, set DESC->registry to that value.  */

static void
sfnt_grok_registry (int fd, struct sfnt_font_desc *desc,
		    struct sfnt_offset_subtable *subtable)
{
  struct sfnt_cmap_table *cmap;
  struct sfnt_cmap_encoding_subtable *subtables;
  int i;

  cmap = sfnt_read_cmap_table (fd, subtable, &subtables, NULL);

  if (!cmap)
    return;

  /* Now pick the ``best'' character map the same way as sfntfont_open
     does.  The caveat is that since the subtable data has not been
     read, Emacs cannot determine whether or not the encoding subtable
     is valid.

     Once platform_id is set, that value becomes much more
     reliable.  */

  /* First look for a non-BMP Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (sfntfont_identify_cmap (subtables[i]) == 2)
	{
	  desc->registry
	    = sfnt_registry_for_subtable (&subtables[i]);
	  goto done;
	}
    }

  /* Next, look for a BMP only Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (sfntfont_identify_cmap (subtables[i]) == 1)
        {
	  desc->registry
	    = sfnt_registry_for_subtable (&subtables[i]);
	  goto done;
	}
    }

  /* Finally, use the first cmap that appears and can be
     identified.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (sfntfont_identify_cmap (subtables[i]) == 0)
        {
	  desc->registry
	    = sfnt_registry_for_subtable (&subtables[i]);
	  goto done;
	}
    }

  /* There are no cmaps available to Emacs.  */
 done:
  xfree (cmap);
  xfree (subtables);
}

/* Return whether or not the font description PREV conflicts with the
   newer font description DESC, and should be removed from the list of
   system fonts.

   If both PREV and DESC are variable fonts, remove styles within PREV
   that overlap with DESC and return false.

   If PREV is a variable font, potentially adjust its list of
   instances.  */

static bool
sfnt_replace_fonts_p (struct sfnt_font_desc *prev,
		      struct sfnt_font_desc *desc)
{
  int i, j, width, weight, slant, count_instance;
  Lisp_Object tem, tem1;
  bool family_equal_p;

  family_equal_p = !NILP (Fstring_equal (prev->family,
					 desc->family));

  if ((!NILP (desc->instances)
       || !NILP (Fstring_equal (prev->style, desc->style)))
      && family_equal_p)
    {
      /* If both inputs are GX fonts...  */
      if (!NILP (desc->instances) && !NILP (prev->instances))
	{
	  /* ...iterate over each of the styles provided by PREV.  If
	     they match any styles within DESC, remove the old style
	     from PREV.  */

	  count_instance = 0;
	  for (i = 0; i < ASIZE (prev->instances); ++i)
	    {
	      tem = AREF (prev->instances, i);

	      if (NILP (tem))
		continue;

	      for (j = 0; j < ASIZE (desc->instances); ++j)
		{
		  tem1 = AREF (desc->instances, j);

		  if (NILP (tem1))
		    continue;

		  if (!NILP (Fequal (tem1, tem)))
		    {
		      /* tem1 is identical to tem, so opt for it over
			 tem.  */
		      ASET (prev->instances, i, Qnil);
		      goto next;
		    }
		}

	      /* Increment the number of instances remaining within
		 PREV.  */
	      count_instance++;

	    next:
	      ;
	    }

	  /* Return true if no instances remain inside
	     PREV->instances, so that the now purposeless desc may be
	     removed.  */
	  return !count_instance;
	}

      return true;
    }

  if (NILP (prev->instances) || !family_equal_p)
    return false;

  /* Look through instances in PREV to see if DESC provides the same
     thing.  */

  count_instance = 0;
  for (i = 0; i < ASIZE (prev->instances); ++i)
    {
      tem = AREF (prev->instances, i);

      if (NILP (tem))
	continue;

      width = XFIXNUM (AREF (tem, 2));
      weight = XFIXNUM (AREF (tem, 3));
      slant = XFIXNUM (AREF (tem, 4));

      if (desc->width == width
	  && desc->weight == weight
	  && desc->slant == slant)
	{
	  /* Remove this instance.  */
	  ASET (prev->instances, i, Qnil);
	  continue;
	}

      count_instance++;
    }

  /* Remove this desc if there are no more instances.  */
  return count_instance < 1;
}

/* Enumerate the offset subtable SUBTABLES in the file FD, whose file
   name is FILE.  OFFSET should be the offset of the subtable within
   the font file, and is recorded for future use.  Value is 1 upon
   failure, else 0.  */

static int
sfnt_enum_font_1 (int fd, const char *file,
		  struct sfnt_offset_subtable *subtables,
		  off_t offset)
{
  struct sfnt_font_desc *desc, **next, *prev;
  struct sfnt_head_table *head;
  struct sfnt_name_table *name;
  struct sfnt_meta_table *meta;
  struct sfnt_maxp_table *maxp;
  struct sfnt_fvar_table *fvar;
  struct sfnt_OS_2_table *OS_2;
  struct sfnt_post_table *post;
  struct sfnt_font_desc temp;
  Lisp_Object family, style, instance, style1;
  int i;
  char buffer[5];

  /* Create the font desc and copy in the file name.  */
  desc = xzalloc (sizeof *desc + strlen (file) + 1);
  desc->path = (char *) (desc + 1);
  memcpy (desc->path, file, strlen (file) + 1);
  desc->offset = offset;

  /* Check that this is a TrueType font.  */
  if (subtables->scaler_type != SFNT_SCALER_TRUE
      && subtables->scaler_type != SFNT_SCALER_VER1)
    goto bail1;

  /* Read required tables.  */
  head = sfnt_read_head_table (fd, subtables);
  if (!head)
    goto bail1;

  name = sfnt_read_name_table (fd, subtables);
  if (!name)
    goto bail2;

  maxp = sfnt_read_maxp_table (fd, subtables);
  if (!maxp)
    goto bail3;

  /* meta is not required, nor present on many non-Apple fonts.  */
  meta = sfnt_read_meta_table (fd, subtables);

  /* Decode the family and style from the name table.  */
  if (sfnt_decode_family_style (name, &family, &style))
    goto bail4;

  /* See if this is a distortable/variable/multiple master font (all
     three terms mean the same time.)  */
  fvar = sfnt_read_fvar_table (fd, subtables);

  /* Set the family.  */
  desc->family = family;
  desc->char_cache = Qnil;
  desc->subtable.platform_id = 500;

  /* Now set the font foundry name.  This information is located
     within the OS/2 table's `ach_vendor_id' field, but use `misc' as
     a recourse if it is not present.  */

  OS_2 = sfnt_read_OS_2_table (fd, subtables);

  if (OS_2)
    {
      memcpy (buffer, OS_2->ach_vendor_id,
	      sizeof OS_2->ach_vendor_id);
      buffer[sizeof OS_2->ach_vendor_id] = '\0';

      /* If the foundry name is empty, use `misc' instead.  */

      if (!buffer[0])
	desc->designer = Qmisc;
      else
	desc->designer = intern (buffer);

      xfree (OS_2);
    }
  else
    desc->designer = Qmisc;

  /* Set the largest glyph identifier.  */
  desc->num_glyphs = maxp->num_glyphs;

  /* Parse the style.  */
  sfnt_parse_style (style, desc);

  /* If the meta table exists, parse the list of design languages.  */
  if (meta)
    sfnt_parse_languages (meta, desc);

  /* Check whether the font claims to be a fixed pitch font and forgo
     the rudimentary detection below if so.  */

  post = sfnt_read_post_table (fd, subtables);

  if (post)
    {
      desc->spacing = (post->is_fixed_pitch ? 100 : 0);
      desc->underline_position = post->underline_position;
      desc->underline_thickness = post->underline_thickness;
      desc->underline_position_set = true;
      xfree (post);
    }
  else
    {
      /* Figure out the spacing.  Some fancy test like what Fontconfig
	 does is probably in order but not really necessary.  */
      if (!NILP (Fstring_search (Fdowncase (family),
				 build_string ("mono"),
				 Qnil)))
	desc->spacing = 100; /* FC_MONO */
    }

  /* Finally add mac-style flags.  Allow them to override styles that
     have not been found.  */

  if (head->mac_style & 01 && desc->weight == 80) /* Bold */
    desc->weight = 200;

  if (head->mac_style & 02 && desc->slant == 0) /* Italic */
    desc->slant = 100;

  /* Figure out what registry this font is likely to support.  */
  sfnt_grok_registry (fd, desc, subtables);

  if (fvar && fvar->instance_count)
    {
      /* If there is an fvar table with instances, then this is a font
	 which defines different axes along which the points in each
	 glyph can be changed.

         Instead of enumerating the font itself, enumerate each
         instance within, which specifies how to configure each axis
         to achieve a specified style.  */

      desc->instances = make_vector (fvar->instance_count, Qnil);

      for (i = 0; i < fvar->instance_count; ++i)
	{
	  style1 = sfnt_decode_instance_name (&fvar->instance[i],
					      name);

	  if (NILP (style1))
	    continue;

	  /* Now parse the style.  */
	  temp.adstyle = Qnil;
	  sfnt_parse_style (style1, &temp);

	  /* Set each field of the vector.  */
	  instance = make_vector (5, Qnil);
	  ASET (instance, 0, style1);
	  ASET (instance, 1, temp.adstyle);
	  ASET (instance, 2, make_fixnum (temp.width));
	  ASET (instance, 3, make_fixnum (temp.weight));
	  ASET (instance, 4, make_fixnum (temp.slant));

	  /* Place the vector in desc->instances.  */
	  ASET (desc->instances, i, instance);
	}
    }

  /* Set the style, link the desc onto system_fonts and return.  */
  desc->style = style;
  desc->next = system_fonts;
  system_fonts = desc;

  /* Remove any fonts which have the same style as this one.  For
     distortable fonts, only remove overlapping styles, unless this is
     also a distortable font.  */

  next = &system_fonts->next;
  prev = *next;
  for (; *next; prev = *next)
    {
      if (sfnt_replace_fonts_p (prev, desc))
	{
	  *next = prev->next;
	  xfree (prev);
	}
      else
	next = &prev->next;
    }

  xfree (fvar);
  xfree (meta);
  xfree (maxp);
  xfree (name);
  xfree (head);
  return 0;

 bail4:
  xfree (meta);
  xfree (maxp);
 bail3:
  xfree (name);
 bail2:
  xfree (head);
 bail1:
  xfree (desc);
  return 1;
}

/* Enumerate the font FILE into the list of system fonts.  Return 1 if
   it could not be enumerated, 0 otherwise.

   Remove any font whose family and style is a duplicate of this one.

   FILE can either be a TrueType collection file containing TrueType
   fonts, or a TrueType font itself.  */

int
sfnt_enum_font (const char *file)
{
  int fd;
  int rc;
  off_t seek;
  struct sfnt_offset_subtable *subtables;
  struct sfnt_ttc_header *ttc;
  size_t i;

  /* Now open the font for reading.  */
  fd = emacs_open (file, O_RDONLY, 0);

  if (fd == -1)
    goto bail;

  /* Read the table directory.  */
  subtables = sfnt_read_table_directory (fd);

  if (subtables == (struct sfnt_offset_subtable *) -1)
    {
      /* This is actually a TrueType container file.  Go back to the
	 beginning and read the TTC header.  */

      if (lseek (fd, 0, SEEK_SET))
	goto bail0;

      ttc = sfnt_read_ttc_header (fd);

      if (!ttc)
	goto bail0;

      /* Enumerate each of the fonts in the collection.  */

      for (i = 0; i < ttc->num_fonts; ++i)
	{
	  seek = lseek (fd, ttc->offset_table[i], SEEK_SET);

	  if (seek == -1 || seek != ttc->offset_table[i])
	    continue;

	  subtables = sfnt_read_table_directory (fd);

	  if (!subtables
	      /* This value means that FD was pointing at a TTC
		 header.  Since FD should already have been moved to
		 the beginning of the TrueType header above, it
		 follows that the font format is invalid.  */
	      || (subtables == (struct sfnt_offset_subtable *) -1))
	    continue;

	  sfnt_enum_font_1 (fd, file, subtables,
			    ttc->offset_table[i]);
	  xfree (subtables);
	}

      /* Always treat reading containers as having been
	 successful.  */

      emacs_close (fd);
      xfree (ttc);
      return 0;
    }

  if (!subtables)
    goto bail0;

  /* Now actually enumerate this font.  */
  rc = sfnt_enum_font_1 (fd, file, subtables, 0);
  xfree (subtables);
  emacs_close (fd);
  return rc;

 bail0:
  emacs_close (fd);
 bail:
  return 1;
}



/* Font discovery and matching.  */

static struct charset *
sfntfont_charset_for_name (Lisp_Object symbol)
{
  ptrdiff_t idx;
  int id;

  idx = CHARSET_SYMBOL_HASH_INDEX (symbol);

  if (idx == -1)
    return NULL;

  /* Vcharset_hash_table is not a real variable, so Lisp programs
     can't clobber it.  */
  id = XFIXNUM (AREF (HASH_VALUE (XHASH_TABLE (Vcharset_hash_table),
				  idx),
		      charset_id));

  return CHARSET_FROM_ID (id);
}

/* Return the character set corresponding to a cmap subtable SUBTABLE.
   Value is NULL if the subtable is not supported.  */

static struct charset *
sfntfont_charset_for_cmap (struct sfnt_cmap_encoding_subtable subtable)
{
  switch (subtable.platform_id)
    {
    case SFNT_PLATFORM_UNICODE:
      /* Reject variation selector and last resort tables.  */
      if ((subtable.platform_specific_id
	   == SFNT_UNICODE_VARIATION_SEQUENCES)
	  || (subtable.platform_specific_id
	      == SFNT_UNICODE_LAST_RESORT))
	return NULL;

      /* 1.0, 1.1, ISO-10646-1993, and 2.0_BMP tables are all within
	 the BMP.  */
      if (subtable.platform_specific_id < SFNT_UNICODE_2_0)
	return sfntfont_charset_for_name (Qunicode_bmp);

      return sfntfont_charset_for_name (Qunicode);

    case SFNT_PLATFORM_MACINTOSH:

      switch (subtable.platform_specific_id)
	{
	case SFNT_MACINTOSH_ROMAN:
	  return sfntfont_charset_for_name (Qmac_roman);

	default:
	  /* Some other Macintosh charset not supported by Emacs.  */
	  return NULL;
	}

    case SFNT_PLATFORM_MICROSOFT:

      /* Microsoft specific encodings.  */

      switch (subtable.platform_specific_id)
	{
	case SFNT_MICROSOFT_SYMBOL:
	  /* Symbols in the Unicode PUA are still Unicode.  */
	  return sfntfont_charset_for_name (Qunicode);

	case SFNT_MICROSOFT_UNICODE_BMP:
	  return sfntfont_charset_for_name (Qunicode_bmp);

	case SFNT_MICROSOFT_SHIFT_JIS:
	  /* PCK aka japanese-jisx0208.  */
	  return sfntfont_charset_for_name (Qjapanese_jisx0208);

	case SFNT_MICROSOFT_PRC:
	  /* GBK, GB2312 or GB18030.  */
	  return sfntfont_charset_for_name (Qgbk);

	case SFNT_MICROSOFT_JOHAB:
	  /* KS C 5601-1992, aka korean-ksc5601.  */
	  return sfntfont_charset_for_name (Qkorean_ksc5601);

	case SFNT_MICROSOFT_UNICODE_UCS_4:
	  /* Unicode past the BMP.  */
	  return sfntfont_charset_for_name (Qucs);
	}

    default:
      return NULL;
    }
}

/* Pick the best character map in the cmap table CMAP.  Use the
   subtables in SUBTABLES and DATA.  Return the subtable data and the
   subtable in *SUBTABLE upon success, NULL otherwise.

   If FORMAT14 is non-NULL, return any associated format 14 variation
   selection context in *FORMAT14 should the selected character map be
   a Unicode character map.  */

static struct sfnt_cmap_encoding_subtable_data *
sfntfont_select_cmap (struct sfnt_cmap_table *cmap,
		      struct sfnt_cmap_encoding_subtable *subtables,
		      struct sfnt_cmap_encoding_subtable_data **data,
		      struct sfnt_cmap_encoding_subtable *subtable,
		      struct sfnt_cmap_format_14 **format14)
{
  int i, j;

  /* First look for a non-BMP Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (data[i] && sfntfont_identify_cmap (subtables[i]) == 2)
	{
	  *subtable = subtables[i];

	  if (!format14)
	    return data[i];

	  /* Search for a corresponding format 14 character map.
	     This is used in conjunction with the selected character
	     map to map variation sequences.  */

	  for (j = 0; j < cmap->num_subtables; ++j)
	    {
	      if (data[j]
		  && subtables[j].platform_id == SFNT_PLATFORM_UNICODE
		  && (subtables[j].platform_specific_id
		      == SFNT_UNICODE_VARIATION_SEQUENCES)
		  && data[j]->format == 14)
		*format14 = (struct sfnt_cmap_format_14 *) data[j];
	    }

	  return data[i];
	}
    }

  /* Next, look for a BMP only Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (data[i] && sfntfont_identify_cmap (subtables[i]) == 1)
	{
	  *subtable = subtables[i];

	  if (!format14)
	    return data[i];

	  /* Search for a corresponding format 14 character map.
	     This is used in conjunction with the selected character
	     map to map variation sequences.  */

	  for (j = 0; j < cmap->num_subtables; ++j)
	    {
	      if (data[j]
		  && subtables[j].platform_id == SFNT_PLATFORM_UNICODE
		  && (subtables[j].platform_specific_id
		      == SFNT_UNICODE_VARIATION_SEQUENCES)
		  && data[j]->format == 14)
		*format14 = (struct sfnt_cmap_format_14 *) data[j];
	    }

	  return data[i];
	}
    }

  /* Finally, use the first cmap that appears and can be
     identified.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (data[i] && sfntfont_identify_cmap (subtables[i]) == 0)
	{
	  *subtable = subtables[i];
	  return data[i];
	}
    }

  /* There are no cmaps available to Emacs.  */
  return NULL;
}

/* Read the cmap from the font descriptor DESC, and place it in CMAP.
   Keep *CMAP untouched if opening the cmap fails.  Set SUBTABLE to
   the cmap's header upon success.  */

static void
sfntfont_read_cmap (struct sfnt_font_desc *desc,
		    struct sfnt_cmap_encoding_subtable_data **cmap,
		    struct sfnt_cmap_encoding_subtable *subtable)
{
  struct sfnt_offset_subtable *font;
  struct sfnt_cmap_encoding_subtable *subtables;
  struct sfnt_cmap_encoding_subtable_data **data;
  struct sfnt_cmap_table *table;
  int fd, i;

  /* Pick a character map and place it in *CMAP.  */
  fd = emacs_open (desc->path, O_RDONLY, 0);

  if (fd < 0)
    return;

  /* Seek to the start of the font itself within its collection.  */

  if (desc->offset
      && lseek (fd, desc->offset, SEEK_SET) != desc->offset)
    {
      emacs_close (fd);
      return;
    }

  font = sfnt_read_table_directory (fd);

  /* Return if FONT is a TrueType collection: the file pointer should
     already have been moved to the start of the table directory if
     so.  */

  if (!font || (font == (struct sfnt_offset_subtable *) -1))
    {
      emacs_close (fd);
      return;
    }

  table = sfnt_read_cmap_table (fd, font, &subtables,
				&data);
  xfree (font);

  if (!table)
    {
      emacs_close (fd);
      return;
    }

  /* Now pick the best character map.  */

  *cmap = sfntfont_select_cmap (table, subtables, data,
				subtable, NULL);

  /* Free the cmap data.  */

  for (i = 0; i < table->num_subtables; ++i)
    {
      if (data[i] != *cmap)
	xfree (data[i]);
    }

  xfree (data);
  xfree (subtables);
  xfree (table);
  emacs_close (fd);
}

/* Return whether or not CHARACTER has an associated mapping in CMAP,
   and the mapping points to a valid glyph.  DESC is the font
   descriptor associated with the font.  */

static bool
sfntfont_glyph_valid (struct sfnt_font_desc *desc,
		      sfnt_char font_character,
		      struct sfnt_cmap_encoding_subtable_data *cmap)
{
  sfnt_glyph glyph;

  glyph = sfnt_lookup_glyph (font_character, cmap);

  if (!glyph)
    return false;

  return glyph <= desc->num_glyphs;
}

/* Look up a character CHARACTER in the font description DESC.  Cache
   the results.  Return true if the character exists, false otherwise.

   If *CMAP is NULL, select a character map for the font and save it
   there.  Otherwise, use the character map in *CMAP.  Save data
   associated with the character map in *SUBTABLE.  */

static bool
sfntfont_lookup_char (struct sfnt_font_desc *desc, Lisp_Object character,
		      struct sfnt_cmap_encoding_subtable_data **cmap,
		      struct sfnt_cmap_encoding_subtable *subtable)
{
  Lisp_Object cached;
  sfnt_char font_character;
  struct charset *charset;
  bool present;

  /* Return false for characters that don't fit in a char table.  */
  if (XFIXNUM (character) > INT_MAX || XFIXNUM (character) < 0)
    return false;

  if (!NILP (desc->char_cache))
    {
      cached = char_table_ref (desc->char_cache,
			       XFIXNUM (character));
      if (!NILP (cached))
	return (EQ (cached, Qlambda) ? false : true);
    }

  if (!*cmap && !desc->cmap_invalid)
    sfntfont_read_cmap (desc, cmap, subtable);

  /* Check that a cmap is now present.  */
  if (!*cmap)
    {
      /* Opening the cmap failed.  Set desc->cmap_invalid to avoid
	 opening it again.  */
      desc->cmap_invalid = true;
      return false;
    }

  /* Otherwise, encode the character.  */

  charset = sfntfont_charset_for_cmap (*subtable);
  if (!charset)
    /* Emacs missing charsets? */
    return false;

  font_character = ENCODE_CHAR (charset, (int) XFIXNUM (character));

  if (font_character == CHARSET_INVALID_CODE (charset))
    return false;

  /* Now return whether or not the glyph is present.  Noto Sans
     Georgian comes with a corrupt format 4 cmap table that somehow
     tries to express glyphs greater than 65565.  */
  present = sfntfont_glyph_valid (desc, font_character, *cmap);

  /* Cache the result.  Store Qlambda when not present, Qt
     otherwise.  */

  if (NILP (desc->char_cache))
    desc->char_cache = Fmake_char_table (Qfont_lookup_cache,
					 Qnil);

  Fset_char_table_range (desc->char_cache, character,
			 present ? Qt : Qlambda);
  return present;
}

/* Return whether or not the specified registry A is ``compatible''
   with registry B.

   Compatibility does not refer to whether or not the font registries
   have an identical character set or repertory of characters.

   Instead, it refers to whether or not Emacs expects looking for A to
   result in fonts used with B.  */

static bool
sfntfont_registries_compatible_p (Lisp_Object a, Lisp_Object b)
{
  if (EQ (a, Qiso8859_1) && EQ (b, Qiso10646_1))
    return true;

  return EQ (a, b);
}

/* Return whether or not the font description DESC satisfactorily
   matches the font specification FONT_SPEC.

   Value is 0 if there is no match, -1 if there is a match against
   DESC itself, and the number of matching instances if the style
   matches one or more instances defined in DESC.  Return the index
   of each matching instance in INSTANCES; it should be SIZE big.  */

static int
sfntfont_list_1 (struct sfnt_font_desc *desc, Lisp_Object spec,
		 int *instances, int size)
{
  Lisp_Object tem, extra, tail;
  struct sfnt_cmap_encoding_subtable_data *cmap;
  size_t i;
  struct sfnt_cmap_encoding_subtable subtable;
  int instance, num_instance;
  Lisp_Object item;
  bool matching;

  /* cmap and subtable are caches for sfntfont_lookup_char.  */

  /* Check that the family name in SPEC matches DESC->family if it is
     specified.  */

  tem = AREF (spec, FONT_FAMILY_INDEX);

  /* If TEM is a family listed in Vsfnt_default_family_alist,
     then use that instead.  */

  if (SYMBOLP (tem) && CONSP (Vsfnt_default_family_alist))
    {
      tail = Vsfnt_default_family_alist;
      FOR_EACH_TAIL_SAFE (tail)
	{
	  if (!CONSP (XCAR (tail)))
	    continue;

	  if (STRINGP (XCAR (XCAR (tail)))
	      && STRINGP (XCDR (XCAR (tail)))
	      && !NILP (Fstring_equal (SYMBOL_NAME (tem),
				       XCAR (XCAR (tail)))))
	    {
	      /* Special family found.  */
	      tem = Fintern (XCDR (XCAR (tail)), Qnil);
	      break;
	    }
	}
    }

  if (!NILP (tem) && NILP (Fstring_equal (SYMBOL_NAME (tem),
					  desc->family)))
    return 0;

  instance = -1;

  /* If a registry is set and wrong, then reject the font desc
     immediately.  This detects 50% of mismatches from fontset.c.

     If DESC->registry is nil, then the registry couldn't be
     determined beforehand.  */

  tem = AREF (spec, FONT_REGISTRY_INDEX);
  if (!NILP (tem) && !NILP (desc->registry)
      && !sfntfont_registries_compatible_p (tem, desc->registry))
    return 0;

  /* If the font spacings disagree, reject this font also.  */

  tem = AREF (spec, FONT_SPACING_INDEX);
  if (FIXNUMP (tem) && (XFIXNUM (tem) != desc->spacing))
    return 0;

  /* Check the style.  If DESC is a fixed font, just check once.
     Otherwise, check each instance.  */

  if (NILP (desc->instances))
    {
      tem = AREF (spec, FONT_ADSTYLE_INDEX);
      if (!NILP (tem) && !EQ (tem, desc->adstyle))
	return 0;

      if (FONT_WIDTH_NUMERIC (spec) != -1
	  && FONT_WIDTH_NUMERIC (spec) != desc->width)
	return 0;

      if (FONT_WEIGHT_NUMERIC (spec) != -1
	  && FONT_WEIGHT_NUMERIC (spec) != desc->weight)
	return 0;

      if (FONT_SLANT_NUMERIC (spec) != -1
	  && FONT_SLANT_NUMERIC (spec) != desc->slant)
	return 0;
    }
  else
    {
      num_instance = 0;

      /* Find the indices of instances in this distortable font which
	 match the given font spec.  */

      for (i = 0; i < ASIZE (desc->instances); ++i)
	{
	  item = AREF (desc->instances, i);

	  if (NILP (item))
	    continue;

	  /* Check that the adstyle specified matches.  */

	  tem = AREF (spec, FONT_ADSTYLE_INDEX);
	  if (!NILP (tem) && NILP (Fequal (tem, AREF (item, 1))))
	    continue;

	  /* Check the style.  */

	  if (FONT_WIDTH_NUMERIC (spec) != -1
	      && (FONT_WIDTH_NUMERIC (spec)
		  != XFIXNUM (AREF (item, 2))))
	    continue;

	  if (FONT_WEIGHT_NUMERIC (spec) != -1
	      && (FONT_WEIGHT_NUMERIC (spec)
		  != XFIXNUM (AREF (item, 3))))
	    continue;

	  if (FONT_SLANT_NUMERIC (spec) != -1
	      && (FONT_SLANT_NUMERIC (spec)
		  != XFIXNUM (AREF (item, 4))))
	    continue;

	  if (num_instance == size)
	    break;

	  /* A matching instance has been found.  Set its index, then
	     go back to the rest of the font matching.  */
	  instances[num_instance++] = i;
	}

      instance = num_instance;
    }

  /* Handle extras.  */
  extra = AREF (spec, FONT_EXTRA_INDEX);

  if (NILP (extra))
    return instance;

  tem = assq_no_quit (QCscript, extra);
  cmap = NULL;

  if (!NILP (tem))
    {
      /* If a script has been specified, look up its representative
	 characters and see if they are present in the font.  This
	 requires reading the cmap.  */
      tem = assq_no_quit (XCDR (tem), Vscript_representative_chars);

      if (CONSP (tem) && VECTORP (XCDR (tem)))
	{
	  tem = XCDR (tem);

	  /* The vector contains characters, of which one must be
	     present in the font.  */
	  matching = false;
	  for (i = 0; i < ASIZE (tem); ++i)
	    {
	      if (FIXNUMP (AREF (tem, i)))
		{
		  if (sfntfont_lookup_char (desc, AREF (tem, i),
					    &cmap, &subtable))
		    {
		      matching = true;
		      break;
		    }
		}
	    }
	  if (!matching)
	    goto fail;
	}
      else if (CONSP (tem) && CONSP (XCDR (tem)))
	{
	  tem = XCDR (tem);

	  /* tem is a list of each characters, all of which must be
	     present in the font.  */
	  FOR_EACH_TAIL_SAFE (tem)
	    {
	      if (FIXNUMP (XCAR (tem))
		  && !sfntfont_lookup_char (desc, XCAR (tem), &cmap,
					    &subtable))
		goto fail;
	    }

	  /* One or more characters are missing.  */
	  if (!NILP (tem))
	    goto fail;
	}
      /* Fail if there are no matching fonts at all.  */
      else if (NILP (tem))
	goto fail;
    }

  /* Now check that the language is supported.  */
  tem = assq_no_quit (QClang, extra);
  if (!NILP (tem) && NILP (Fmemq (tem, desc->languages)))
    goto fail;

  /* Set desc->subtable if cmap was specified.  */
  if (cmap)
    desc->subtable = subtable;

  xfree (cmap);
  return instance;

 fail:
  /* The cmap might've been read in and require deallocation.  */
  xfree (cmap);
  return 0;
}

/* Type of font entities and font objects created.  */
static Lisp_Object sfnt_vendor_name;

/* Font driver used in font objects created.  */
static const struct font_driver *sfnt_font_driver;

/* Return the font registry corresponding to the font descriptor DESC.
   Under X, the font registry is an atom registered with the Open
   Group uniquely identifying the organization which defines the
   font's character set.

   In practice, the registry overlaps with the character set itself.
   So Emacs just uses the ``registry'' field to represent both
   instead.  */

static Lisp_Object
sfntfont_registry_for_desc (struct sfnt_font_desc *desc)
{
  struct sfnt_cmap_encoding_subtable_data *cmap;

  cmap = NULL;

  if (desc->cmap_invalid)
    return Qnil;

  if (desc->subtable.platform_id == 500)
    {
      /* Read in the cmap to determine the registry.  */
      sfntfont_read_cmap (desc, &cmap, &desc->subtable);

      if (!cmap)
	{
	  desc->cmap_invalid = true;
	  return Qnil;
	}
    }

  xfree (cmap);

  if (desc->subtable.platform_id != 500)
    /* desc->subtable.platform_id is now set.  CMAP is already free,
       because it is not actually used.  */
    return sfnt_registry_for_subtable (&desc->subtable);

  return Qnil;
}

/* Return a font-entity that represents the font descriptor (unopened
   font) DESC.  If INSTANCE is more than or equal to 1, then it is the
   index of the instance in DESC that should be opened plus 1; in that
   case, DESC must be a distortable font.  */

static Lisp_Object
sfntfont_desc_to_entity (struct sfnt_font_desc *desc, int instance)
{
  Lisp_Object entity, vector;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, sfnt_vendor_name);
  ASET (entity, FONT_FOUNDRY_INDEX, desc->designer);
  ASET (entity, FONT_FAMILY_INDEX, Fintern (desc->family, Qnil));
  ASET (entity, FONT_ADSTYLE_INDEX, Qnil);
  ASET (entity, FONT_REGISTRY_INDEX,
	sfntfont_registry_for_desc (desc));

  /* Size of 0 means the font is scalable.  */
  ASET (entity, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (entity, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (entity, FONT_SPACING_INDEX, make_fixnum (desc->spacing));

  if (instance >= 1)
    {
      if (NILP (desc->instances)
	  || instance > ASIZE (desc->instances))
	emacs_abort ();

      vector = AREF (desc->instances, instance - 1);
      FONT_SET_STYLE (entity, FONT_WIDTH_INDEX,
		      AREF (vector, 2));
      FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		      AREF (vector, 3));
      FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		      AREF (vector, 4));
      ASET (entity, FONT_ADSTYLE_INDEX, AREF (vector, 1));
    }
  else
    {
      FONT_SET_STYLE (entity, FONT_WIDTH_INDEX,
		      make_fixnum (desc->width));
      FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		      make_fixnum (desc->weight));
      FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		      make_fixnum (desc->slant));
      ASET (entity, FONT_ADSTYLE_INDEX, desc->adstyle);
    }

  /* Set FONT_EXTRA_INDEX to a pointer to the font description.  Font
     descriptions are never supposed to be freed.  */

  ASET (entity, FONT_EXTRA_INDEX,
	(instance >= 1
	 ? list2 (Fcons (Qfont_entity, make_mint_ptr (desc)),
		  Fcons (Qfont_instance, make_fixnum (instance - 1)))
	 : list1 (Fcons (Qfont_entity, make_mint_ptr (desc)))));

  return entity;
}

/* Return whether fewer fields inside the font entity A are set than
   there are set inside the font entity B.  */

static Lisp_Object
sfntfont_compare_font_entities (Lisp_Object a, Lisp_Object b)
{
  ptrdiff_t count_a, count_b, i;

  count_a = 0;
  count_b = 0;

  for (i = 0; i < FONT_ENTITY_MAX; ++i)
    {
      if (!NILP (AREF (a, i)))
	count_a++;
    }

  for (i = 0; i < FONT_ENTITY_MAX; ++i)
    {
      if (!NILP (AREF (b, i)))
	count_b++;
    }

  return count_a < count_b ? Qt : Qnil;
}

/* Function that compares two font entities to return whether fewer
   fields are set within the first than in the second.  */

static union Aligned_Lisp_Subr Scompare_font_entities =
  {
    {
      { PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS), },
      { .a2 = sfntfont_compare_font_entities, },
      2, 2, "sfntfont_compare_font_entities", {0}, lisp_h_Qnil,
    },
  };

/* Return a list of font-entities matching the specified
   FONT_SPEC.  */

Lisp_Object
sfntfont_list (struct frame *f, Lisp_Object font_spec)
{
  Lisp_Object matching, tem, compare_font_entities;
  struct sfnt_font_desc *desc;
  int i, rc, instances[100];

  matching = Qnil;

  block_input ();
  /* Returning irrelevant results on receiving an OTF form will cause
     fontset.c to loop over and over, making displaying some
     characters very slow.  */
  tem = assq_no_quit (QCotf, AREF (font_spec, FONT_EXTRA_INDEX));
  if (CONSP (tem) && !NILP (XCDR (tem)))
    {
      unblock_input ();
      return Qnil;
    }

  /* Loop through known system fonts and add them one-by-one.  */

  for (desc = system_fonts; desc; desc = desc->next)
    {
      rc = sfntfont_list_1 (desc, font_spec, instances,
			    ARRAYELTS (instances));

      if (rc < 0)
	matching = Fcons (sfntfont_desc_to_entity (desc, 0),
			  matching);
      else if (rc)
	{
	  /* Add each matching instance.  */

	  for (i = 0; i < rc; ++i)
	    matching = Fcons (sfntfont_desc_to_entity (desc,
						       instances[i] + 1),
			      matching);
	}
    }
  unblock_input ();

  /* Sort matching by the number of fields set inside each element, so
     that values of FONT_SPECs that leave a number of fields
     unspecified will yield a list with the closest matches (that is
     to say, those whose fields are precisely as specified by the
     caller) ordered first.  */

  XSETSUBR (compare_font_entities, &Scompare_font_entities.s);
  matching = CALLN (Fsort, matching, compare_font_entities);
  return matching;
}

/* Return the first font-entity matching the specified FONT_SPEC.  */

Lisp_Object
sfntfont_match (struct frame *f, Lisp_Object font_spec)
{
  Lisp_Object matches;

  matches = sfntfont_list (f, font_spec);

  if (!NILP (matches))
    return XCAR (matches);

  return Qnil;
}



enum
  {
    SFNT_OUTLINE_CACHE_SIZE = 256,
    SFNT_RASTER_CACHE_SIZE  = 128,
  };

/* Caching subsystem.  Generating outlines from glyphs is expensive,
   and so is rasterizing them, so two caches are maintained for both
   glyph outlines and rasters.

   Computing metrics also requires some expensive processing if the
   glyph has instructions or distortions.  */

struct sfnt_outline_cache
{
  /* Next and last cache buckets.  */
  struct sfnt_outline_cache *next, *last;

  /* Pointer to outline.  */
  struct sfnt_glyph_outline *outline;

  /* Reference to glyph metrics.  */
  struct sfnt_glyph_metrics metrics;

  /* What glyph this caches.  */
  sfnt_glyph glyph;
};

struct sfnt_raster_cache
{
  /* Next and last cache buckets.  */
  struct sfnt_raster_cache *next, *last;

  /* Pointer to raster.  */
  struct sfnt_raster *raster;

  /* What glyph this caches.  */
  sfnt_glyph glyph;
};

struct sfntfont_get_glyph_outline_dcontext
{
  /* Long and short loca tables.  */
  struct sfnt_loca_table_long *loca_long;
  struct sfnt_loca_table_short *loca_short;

  /* glyf table.  */
  struct sfnt_glyf_table *glyf;

  /* hmtx, hhea and maxp tables utilized to acquire glyph metrics.  */
  struct sfnt_hmtx_table *hmtx;
  struct sfnt_hhea_table *hhea;
  struct sfnt_maxp_table *maxp;

  /* Variation settings, or NULL.  */
  struct sfnt_blend *blend;
};

/* Return the glyph identified by GLYPH_ID from the glyf and loca
   table specified in DCONTEXT.  Set *NEED_FREE to true.  */

static struct sfnt_glyph *
sfntfont_get_glyph (sfnt_glyph glyph_id, void *dcontext,
		    bool *need_free)
{
  struct sfntfont_get_glyph_outline_dcontext *tables;
  struct sfnt_glyph *glyph;
  struct sfnt_metrics_distortion distortion;

  tables = dcontext;
  *need_free = true;

  glyph = sfnt_read_glyph (glyph_id, tables->glyf,
			   tables->loca_short,
			   tables->loca_long);

  if (tables->blend && glyph)
    {
      if (glyph->simple)
	sfnt_vary_simple_glyph (tables->blend, glyph_id, glyph,
				&distortion);
      else
	sfnt_vary_compound_glyph (tables->blend, glyph_id, glyph,
				  &distortion);
    }

  /* Note that the distortion is not relevant for compound glyphs.  */
  return glyph;
}

/* Free the glyph identified by GLYPH.  */

static void
sfntfont_free_glyph (struct sfnt_glyph *glyph, void *dcontext)
{
  sfnt_free_glyph (glyph);
}

/* Return unscaled glyph metrics for the glyph designated by the ID
   GLYPH within *METRICS, utilizing tables within DCONTEXT.

   Value is 1 upon failure, 0 otherwise.  */

static int
sfntfont_get_metrics (sfnt_glyph glyph, struct sfnt_glyph_metrics *metrics,
		      void *dcontext)
{
  struct sfntfont_get_glyph_outline_dcontext *tables;

  tables = dcontext;
  return sfnt_lookup_glyph_metrics (glyph, metrics, tables->hmtx,
				    tables->hhea, tables->maxp);
}

/* Dereference the outline OUTLINE.  Free it once refcount reaches
   0.  */

static void
sfntfont_dereference_outline (struct sfnt_glyph_outline *outline)
{
  eassert (outline->refcount > 0);

  if (--outline->refcount)
    return;

  xfree (outline);
}

/* Get the outline corresponding to the specified GLYPH_CODE in CACHE.
   Use the scale factor SCALE, the glyf table GLYF, and the head table
   HEAD.  Keep *CACHE_SIZE updated with the number of elements in the
   cache.

   Distort the glyph using BLEND if INDEX is not -1.

   Use the offset information in the long or short loca tables
   LOCA_LONG and LOCA_SHORT, whichever is set.

   Use the specified HMTX, HEAD, HHEA and MAXP tables when instructing
   compound glyphs.

   If INTERPRETER is non-NULL, then possibly use it and the
   interpreter graphics STATE to instruct the glyph.

   If METRICS is non-NULL, return the scaled glyph metrics after
   variation and instructing.

   Return the outline with an incremented reference count and enter
   the generated outline into CACHE upon success, possibly discarding
   any older outlines, or NULL on failure.  */

static struct sfnt_glyph_outline *
sfntfont_get_glyph_outline (sfnt_glyph glyph_code,
			    struct sfnt_outline_cache *cache,
			    sfnt_fixed scale, int *cache_size,
			    struct sfnt_blend *blend,
			    int index,
			    struct sfnt_glyf_table *glyf,
			    struct sfnt_head_table *head,
			    struct sfnt_hmtx_table *hmtx,
			    struct sfnt_hhea_table *hhea,
			    struct sfnt_maxp_table *maxp,
			    struct sfnt_loca_table_short *loca_short,
			    struct sfnt_loca_table_long *loca_long,
			    struct sfnt_interpreter *interpreter,
			    struct sfnt_glyph_metrics *metrics,
			    struct sfnt_graphics_state *state)
{
  struct sfnt_outline_cache *start;
  struct sfnt_glyph_outline *outline;
  struct sfnt_glyph *glyph;
  struct sfntfont_get_glyph_outline_dcontext dcontext;
  struct sfnt_instructed_outline *value;
  const char *error;
  struct sfnt_glyph_metrics temp;
  struct sfnt_metrics_distortion distortion;
  sfnt_fixed advance;

  start = cache->next;
  distortion.advance = 0;

  /* See if the outline is already cached.  */
  for (; start != cache; start = start->next)
    {
      if (start->glyph == glyph_code)
	{
	  /* Move start to the start of the ring.  Then increase
	     start->outline->refcount and return it.  */

	  start->last->next = start->next;
	  start->next->last = start->last;

	  start->next = cache->next;
	  start->last = cache;
	  start->next->last = start;
	  start->last->next = start;
	  start->outline->refcount++;

	  if (metrics)
	    *metrics = start->metrics;

	  return start->outline;
	}
    }

  /* Not already cached.  Get the glyph.  */
  glyph = sfnt_read_glyph (glyph_code, glyf,
			   loca_short, loca_long);

  if (!glyph)
    return NULL;

  /* Distort the glyph if necessary.  */

  if (index != -1)
    {
      if (glyph->simple)
	{
	  if (sfnt_vary_simple_glyph (blend, glyph_code,
				      glyph, &distortion))
	    {
	      sfnt_free_glyph (glyph);
	      return NULL;
	    }
	}
      else if (sfnt_vary_compound_glyph (blend, glyph_code,
					 glyph, &distortion))
	{
	  sfnt_free_glyph (glyph);
	  return NULL;
	}
    }

  /* Try to instruct the glyph if INTERPRETER is specified.  */

  outline = NULL;

  dcontext.loca_long = loca_long;
  dcontext.loca_short = loca_short;
  dcontext.glyf = glyf;
  dcontext.hhea = hhea;
  dcontext.hmtx = hmtx;
  dcontext.maxp = maxp;
  dcontext.blend = (index != -1 ? blend : NULL);

  /* Now load the glyph's unscaled metrics into TEMP.  */

  if (sfnt_lookup_glyph_metrics (glyph_code, &temp, hmtx, hhea, maxp))
    goto fail;

  if (interpreter)
    {
      if (glyph->simple)
	{
	  /* Restore the interpreter state from the snapshot taken
	     after loading the preprogram.  */
	  interpreter->state = *state;

	  error = sfnt_interpret_simple_glyph (glyph, interpreter,
					       &temp, &value);
	}
      else
	/* Restoring the interpreter state is done by
	   sfnt_interpret_compound_glyph; all that must be done here
	   is to give the graphics state to that function.  */
	error = sfnt_interpret_compound_glyph (glyph, interpreter,
					       state,
					       sfntfont_get_glyph,
					       sfntfont_free_glyph,
					       hmtx, hhea, maxp,
					       &temp, &dcontext,
					       &value);

      if (!error)
	{
	  /* Now record the advance with that measured from the
	     phantom points within the instructed glyph outline, and
	     subsequently replace it once metrics are scaled.  */

	  outline = sfnt_build_instructed_outline (value,
						   &advance);
	  xfree (value);

	  if (outline)
	    {
	      /* Save the new advance width.  This advance width is
		 rounded again, as the instruction code executed might
		 have moved both phantom points such that they no
		 longer measure a fractional distance.  */
	      temp.advance = SFNT_ROUND_FIXED (advance);

	      /* Finally, adjust the left side bearing of the glyph
		 metrics by the origin point of the outline, should a
		 transformation have been applied by either
		 instruction code or glyph variation.  The left side
		 bearing is the distance from the origin point to the
		 left most point on the X axis.  */
	      temp.lbearing
		= SFNT_FLOOR_FIXED (outline->xmin - outline->origin);
	    }
	}
    }

  if (!outline)
    {
      /* Build the outline.  This will apply GX offsets within *GLYPH
	 to TEMP.  */
      outline = sfnt_build_glyph_outline (glyph, scale,
					  &temp,
					  sfntfont_get_glyph,
					  sfntfont_free_glyph,
					  sfntfont_get_metrics,
					  &dcontext);

      /* At this point, the glyph metrics are unscaled.  Scale them
	 up.  If INTERPRETER is set, use the scale placed within.  */
      sfnt_scale_metrics (&temp, scale);
    }

 fail:

  xfree (glyph);

  if (!outline)
    return NULL;

  start = xmalloc (sizeof *start);
  start->glyph = glyph_code;
  start->outline = outline;
  start->metrics = temp;

  /* One reference goes to the cache.  The second reference goes to
     the caller.  */
  outline->refcount = 2;

  /* Link start onto the cache.  */
  start->next = cache->next;
  start->last = cache;
  start->next->last = start;
  start->last->next = start;

  /* Update the cache size.  */
  (*cache_size)++;

  /* Figure out if the least recently used element has to be
     evicted.  */
  if (*cache_size > SFNT_OUTLINE_CACHE_SIZE)
    {
      start = cache->last;
      eassert (start != cache);

      /* Free the least recently used entry in the cache.  */
      start->last->next = start->next;
      start->next->last = start->last;
      sfntfont_dereference_outline (start->outline);
      xfree (start);

      (*cache_size)--;
    }

  /* Return the cached outline and metrics.  */

  if (metrics)
    *metrics = temp;

  return outline;
}

/* Free the outline cache referred to by CACHE.  Dereference each
   outline contained therein.  */

static void
sfntfont_free_outline_cache (struct sfnt_outline_cache *cache)
{
  struct sfnt_outline_cache *next, *last;

  /* Handle partly initialized fonts.  */
  if (!cache->next)
    return;

  for (next = cache->next; next != cache;)
    {
      last = next;
      next = next->next;

      sfntfont_dereference_outline (last->outline);
      xfree (last);
    }

  cache->next = cache;
  cache->last = cache;
}

/* Dereference the raster RASTER.  Free it once refcount reaches
   0.  */

static void
sfntfont_dereference_raster (struct sfnt_raster *raster)
{
  eassert (raster->refcount > 0);

  if (--raster->refcount)
    return;

  xfree (raster);
}

/* Get the raster corresponding to the specified GLYPH_CODE in CACHE.
   Use the outline named OUTLINE.  Keep *CACHE_SIZE updated with the
   number of elements in the cache.  */

static struct sfnt_raster *
sfntfont_get_glyph_raster (sfnt_glyph glyph_code,
			   struct sfnt_raster_cache *cache,
			   struct sfnt_glyph_outline *outline,
			   int *cache_size)
{
  struct sfnt_raster_cache *start;
  struct sfnt_raster *raster;

  /* See if the raster is already cached.  */
  start = cache->next;

  for (; start != cache; start = start->next)
    {
      if (start->glyph == glyph_code)
	{
	  /* Move start to the start of the ring.  Them, increase
	     start->raster->refcount and return it.  */

	  start->last->next = start->next;
	  start->next->last = start->last;

	  start->next = cache->next;
	  start->last = cache;
	  start->next->last = start;
	  start->last->next = start;
	  start->raster->refcount++;

	  return start->raster;
	}
    }

  /* Not already cached.  Raster the outline.  */

  if (!sfnt_raster_glyphs_exactly)
    raster = sfnt_raster_glyph_outline (outline);
  else
    raster = sfnt_raster_glyph_outline_exact (outline);

  if (!raster)
    return NULL;

  start = xmalloc (sizeof *start);
  start->glyph = glyph_code;
  start->raster = raster;

  /* One reference goes to the cache.  The second reference goes to
     the caller.  */
  raster->refcount = 2;

  /* Link start onto the cache.  */
  start->next = cache->next;
  start->last = cache;
  start->next->last = start;
  start->last->next = start;

  /* Update the cache size.  */
  (*cache_size)++;

  /* Figure out if the least recently used element has to be
     evicted.  */
  if (*cache_size > SFNT_OUTLINE_CACHE_SIZE)
    {
      start = cache->last;
      eassert (start != cache);

      /* Free the least recently used entry in the cache.  */
      start->last->next = start->next;
      start->next->last = start->last;
      sfntfont_dereference_raster (start->raster);
      xfree (start);

      (*cache_size)--;
    }

  /* Return the cached raster.  */
  return raster;
}

/* Free the raster cache referred to by CACHE.  Dereference each
   raster contained therein.  */

static void
sfntfont_free_raster_cache (struct sfnt_raster_cache *cache)
{
  struct sfnt_raster_cache *next, *last;

  /* Handle partly initialized fonts.  */
  if (!cache->next)
    return;

  for (next = cache->next; next != cache;)
    {
      last = next;
      next = next->next;

      sfntfont_dereference_raster (last->raster);
      xfree (last);
    }

  cache->next = cache;
  cache->last = cache;
}



/* Opening fonts.  */

struct sfnt_font_info
{
  /* Parent font structure.  */
  struct font font;

#ifdef HAVE_MMAP
  /* The next font in this chain.  */
  struct sfnt_font_info *next;
#endif /* HAVE_MMAP */

  /* The font description used to create this font.  Used to
     dereference tables associated with this font.  */
  struct sfnt_font_desc *desc;

  /* Various tables required to use the font.  */
  struct sfnt_cmap_table *cmap;
  struct sfnt_hhea_table *hhea;
  struct sfnt_maxp_table *maxp;
  struct sfnt_head_table *head;
  struct sfnt_hmtx_table *hmtx;
  struct sfnt_glyf_table *glyf;
  struct sfnt_loca_table_short *loca_short;
  struct sfnt_loca_table_long *loca_long;
  struct sfnt_prep_table *prep;
  struct sfnt_fpgm_table *fpgm;
  struct sfnt_cvt_table *cvt;

  /* The selected character map.  */
  struct sfnt_cmap_encoding_subtable_data *cmap_data;

  /* Data identifying that character map.  */
  struct sfnt_cmap_encoding_subtable cmap_subtable;

  /* The UVS context.  */
  struct sfnt_uvs_context *uvs;

  /* Outline cache.  */
  struct sfnt_outline_cache outline_cache;

  /* Number of elements in the outline cache.  */
  int outline_cache_size;

  /* Raster cache.  */
  struct sfnt_raster_cache raster_cache;

  /* Number of elements in the raster cache.  */
  int raster_cache_size;

  /* Interpreter for grid fitting (if enabled).  */
  struct sfnt_interpreter *interpreter;

  /* Graphics state after the execution of the font and control value
     programs.  */
  struct sfnt_graphics_state state;

  /* Factor used to convert from em space to pixel space.  */
  sfnt_fixed scale;

  /* The blend (configuration of this multiple master font).  */
  struct sfnt_blend blend;

  /* The index of the named instance used to initialize BLEND.
     -1 if BLEND is not initialized.  */
  int instance;

#ifdef HAVE_MMAP
  /* Whether or not the glyph table has been mmapped.  */
  bool glyf_table_mapped;
#endif /* HAVE_MMAP */

#ifdef HAVE_HARFBUZZ
  /* HarfBuzz font object.  */
  hb_font_t *hb_font;

  /* File descriptor associated with this font.  */
  int fd;

  /* The table directory of the font file.  */
  struct sfnt_offset_subtable *directory;
#endif /* HAVE_HARFBUZZ */
};

#ifdef HAVE_MMAP

/* List of all open fonts.  */

static struct sfnt_font_info *open_fonts;

#endif /* HAVE_MMAP */

/* Look up the glyph corresponding to the character C in FONT.  Return
   0 upon failure, and the glyph otherwise.  */

static sfnt_glyph
sfntfont_lookup_glyph (struct sfnt_font_info *font_info, int c)
{
  struct charset *charset;
  sfnt_char character;
  sfnt_glyph glyph;

  charset = CHARSET_FROM_ID (font_info->font.encoding_charset);

  if (!charset)
    return 0;

  character = ENCODE_CHAR (charset, c);

  if (character == CHARSET_INVALID_CODE (charset))
    return 0;

  /* Do the actual lookup with the encoded character.  */
  glyph = sfnt_lookup_glyph (character, font_info->cmap_data);

  return glyph;
}

static int sfntfont_measure_pcm (struct sfnt_font_info *, sfnt_glyph,
				 struct font_metrics *);

/* Probe and set FONT_INFO->font.average_width,
   FONT_INFO->font.space_width, and FONT_INFO->font.min_width
   according to the tables contained therein.

   As this function generates outlines for all glyphs, outlines for
   all ASCII characters will be entered into the outline cache as
   well.  */

static void
sfntfont_probe_widths (struct sfnt_font_info *font_info)
{
  int i, num_characters, total_width;
  sfnt_glyph glyph;
  struct font_metrics pcm;

  num_characters = 0;
  total_width = 0;

  /* First set some reasonable default values.  */
  font_info->font.average_width = font_info->font.pixel_size;
  font_info->font.space_width = font_info->font.pixel_size;
  font_info->font.min_width = 1;

  /* Next, loop through the common ASCII characters.  Tally up their
     advance widths and set space_width if necessary.  */
  for (i = 32; i < 127; ++i)
    {
      glyph = sfntfont_lookup_glyph (font_info, i);

      if (!glyph)
	continue;

      /* Now look up the metrics of this glyph.  Data from the metrics
	 table doesn't fit the bill, since variations and instruction
	 code is not applied to it.  */
      if (sfntfont_measure_pcm (font_info, glyph, &pcm))
	continue;

      /* Increase the number of characters.  */
      num_characters++;

      /* Add the advance to total_width.  */
      total_width += pcm.width;

      /* Update min_width if it hasn't been set yet or is wider.  */
      if (font_info->font.min_width == 1
	  || font_info->font.min_width > pcm.width)
	font_info->font.min_width = pcm.width;

      /* If i is the space character, set the space width.  Make sure
	 to round this up.  */
      if (i == 32)
	font_info->font.space_width = pcm.width;
    }

  /* Now, if characters were found, set average_width.  */
  if (num_characters)
    font_info->font.average_width = total_width / num_characters;
}

/* Initialize the instruction interpreter for INFO.  Load the font and
   preprogram for the pixel size in INFO and its corresponding point
   size POINT_SIZE.  Use the FVAR table in DESC.

   The font tables in INFO must already have been initialized.

   Set INFO->interpreter upon success, and leave that field intact
   otherwise.  */

static void
sfntfont_setup_interpreter (struct sfnt_font_info *info,
			    struct sfnt_font_desc *desc,
			    int point_size)
{
  struct sfnt_cvt_table *cvt;
  struct sfnt_fpgm_table *fpgm;
  struct sfnt_prep_table *prep;
  struct sfnt_interpreter *interpreter;
  const char *error;
  struct sfnt_graphics_state state;
  Lisp_Object regexp;

  /* If Vsfnt_uninstructable_family_regexp matches this font, then
     return.  */

  regexp = Vsfnt_uninstructable_family_regexp;

  if (STRINGP (regexp)
      && (fast_string_match_ignore_case (regexp,
					 desc->family)
	  >= 0))
    return;

  /* Load the cvt, fpgm and prep already read.  */

  cvt  = info->cvt ;
  fpgm = info->fpgm;
  prep = info->prep;

  /* If both fpgm and prep are NULL, this font likely has no
     instructions, so don't bother setting up the interpreter.  */

  if (!fpgm && !prep)
    goto bail;

  /* If the interpreter does not use the operand stack at all, it is
     useless.  In addition, some broken fonts specify some unnecessary
     instructions in prep and set head->max_stack_elements to 0.

     Don't create the interpreter in that case.  */

  if (!info->maxp->max_stack_elements)
    goto bail;

  /* Now, create the interpreter using the limits in info->maxp and
     info->head.  CVT can be NULL.  */

  interpreter = sfnt_make_interpreter (info->maxp, cvt, info->head,
				       desc->tables->fvar,
				       info->font.pixel_size,
				       point_size);

  /* Bail if the interpreter couldn't be created.  */
  if (!interpreter)
    goto bail;

  if (fpgm)
    {
      /* Otherwise, evaluate the font and cvt programs.

	 FIXME: make sure infinite loops inside these programs
	 cannot lock up Emacs.  */

      error = sfnt_interpret_font_program (interpreter, fpgm);

      if (error)
	{
	  /* If an error occurs, log it to the *Messages* buffer.  */
	  message_with_string ("While interpreting font program: %s",
			       build_string (error), true);
	  goto bail1;
	}

      /* Save the graphics state.  */
      state = interpreter->state;
    }

  if (prep)
    {
      /* This will overwrite state if the instruction control is set
	 appropriately.  */
      error = sfnt_interpret_control_value_program (interpreter, prep,
						    &state);

      if (error)
	{
	  /* If an error occurs, log it to the *Messages* buffer.  */
	  message_with_string ("While interpreting preprogram: %s",
			       build_string (error), true);
	  goto bail1;
	}
    }

  /* The interpreter has been properly set up.  */
  info->fpgm = fpgm;
  info->prep = prep;
  info->cvt = cvt;
  info->state = state;
  info->interpreter = interpreter;

  return;

 bail1:
  xfree (interpreter);
 bail:
  return;
}

/* Free each of the tables opened by `sfnt_open_tables', and possibly
   file descriptors as well.  Then, free TABLES itself.  */

static void
sfnt_close_tables (struct sfnt_font_tables *tables)
{
#ifdef HAVE_MMAP
  int rc;
#endif /* HAVE_MMAP */

  xfree (tables->cmap);
  xfree (tables->hhea);
  xfree (tables->maxp);
  xfree (tables->head);
  xfree (tables->hmtx);
#ifdef HAVE_MMAP
  if (tables->glyf_table_mapped)
    {
      rc = sfnt_unmap_glyf_table (tables->glyf);

      if (rc)
	emacs_abort ();
    }
  else
#endif /* HAVE_MMAP */
    xfree (tables->glyf);
  xfree (tables->loca_short);
  xfree (tables->loca_long);
  xfree (tables->prep);
  xfree (tables->fpgm);
  xfree (tables->cvt);
  xfree (tables->fvar);
  xfree (tables->avar);
  xfree (tables->gvar);
  xfree (tables->cvar);
  xfree (tables->cmap_data);

  if (tables->uvs)
    sfnt_free_uvs_context (tables->uvs);

#ifdef HAVE_HARFBUZZ
  /* Close the font file.  */

  if (tables->fd != -1)
    {
      emacs_close (tables->fd);
      tables->fd = -1;
    }

  /* Free its table directory.  */
  xfree (tables->directory);
  tables->directory = NULL;
#endif
}

/* Open font tables associated with the specified font description
   DESC.  Return the font tables, or NULL upon failure.  */

static struct sfnt_font_tables *
sfnt_open_tables (struct sfnt_font_desc *desc)
{
  struct sfnt_font_tables *tables;
  struct sfnt_offset_subtable *subtable;
  int fd, i;
#ifdef HAVE_MMAP
  int rc;
#endif /* HAVE_MMAP */
  struct sfnt_cmap_encoding_subtable *subtables;
  struct sfnt_cmap_encoding_subtable_data **data;
  struct sfnt_cmap_format_14 *format14;

  tables = xzalloc (sizeof *tables);

  /* Open the font.  */
  fd = emacs_open (desc->path, O_RDONLY, 0);

  if (fd == -1)
    goto bail;

  /* Seek to the offset specified to the table directory.  */

  if (desc->offset
      && lseek (fd, desc->offset, SEEK_SET) != desc->offset)
    goto bail;

  /* Read the offset subtable.  */
  subtable = sfnt_read_table_directory (fd);

  if (!subtable || (subtable == (struct sfnt_offset_subtable *) -1))
    goto bail1;

  /* Read required tables.  This font backend is supposed to be used
     mostly on devices with flash memory, so the order in which they
     are read is insignificant.  */

  tables->cmap = sfnt_read_cmap_table (fd, subtable, &subtables,
				       &data);
  if (!tables->cmap)
    goto bail2;

  format14 = NULL;
  tables->cmap_data
    = sfntfont_select_cmap (tables->cmap,
			    subtables, data,
			    &tables->cmap_subtable,
			    &format14);

  if (format14)
    {
      /* Build a UVS context from this format 14 mapping table.  A UVS
         context contains each variation selector supported by the
         font, and a list of ``non-default'' mappings between base
         characters and variation glyph IDs.  */

      tables->uvs = sfnt_create_uvs_context (format14, fd);
      xfree (format14);
    }

  for (i = 0; i < tables->cmap->num_subtables; ++i)
    {
      if (data[i] != tables->cmap_data
	  /* format14 has already been freed.  */
	  && data[i] != (struct sfnt_cmap_encoding_subtable_data *) format14)
	xfree (data[i]);
    }

  xfree (subtables);
  xfree (data);

  if (!tables->cmap_data)
    goto bail3;

  /* Read the hhea, maxp, glyf, and head tables.  */
  tables->hhea = sfnt_read_hhea_table (fd, subtable);
  tables->maxp = sfnt_read_maxp_table (fd, subtable);

#ifdef HAVE_MMAP

  /* First try to map the glyf table.  If that fails, then read the
     glyf table.  */

  tables->glyf = sfnt_map_glyf_table (fd, subtable);

  /* Next, if this fails, read the glyf table.  */

  if (!tables->glyf)
#endif /* HAVE_MMAP */
    tables->glyf = sfnt_read_glyf_table (fd, subtable);
#ifdef HAVE_MMAP
  else
    tables->glyf_table_mapped = true;
#endif /* HAVE_MMAP */

  tables->head = sfnt_read_head_table (fd, subtable);

  /* If any of those tables couldn't be read, bail.  */
  if (!tables->hhea || !tables->maxp || !tables->glyf
      || !tables->head)
    goto bail4;

  /* Now figure out which kind of loca table must be read based on
     head->index_to_loc_format.  */

  if (tables->head->index_to_loc_format)
    {
      tables->loca_long
	= sfnt_read_loca_table_long (fd, subtable);

      if (!tables->loca_long)
	goto bail4;
    }
  else
    {
      tables->loca_short
	= sfnt_read_loca_table_short (fd, subtable);

      if (!tables->loca_short)
	goto bail4;
    }

  /* Read the horizontal metrics table.  */
  tables->hmtx = sfnt_read_hmtx_table (fd, subtable,
				       tables->hhea,
				       tables->maxp);
  if (!tables->hmtx)
    goto bail5;

  /* Read instruction related font tables.  These might not be
     present, which is OK, since instructing fonts is optional.  */
  tables->prep = sfnt_read_prep_table (fd, subtable);
  tables->fpgm = sfnt_read_fpgm_table (fd, subtable);
  tables->cvt  = sfnt_read_cvt_table (fd, subtable);

  /* Read distortion related tables.  These might not be present.  */
  tables->fvar = sfnt_read_fvar_table (fd, subtable);
  tables->avar = sfnt_read_avar_table (fd, subtable);
  tables->gvar = sfnt_read_gvar_table (fd, subtable);

  if (tables->cvt && tables->fvar)
    tables->cvar = sfnt_read_cvar_table (fd, subtable, tables->fvar,
					 tables->cvt);

#ifdef HAVE_HARFBUZZ
  /* Now copy over the subtable if necessary, as it is needed to read
     extra font tables required by HarfBuzz.  */
  tables->directory = subtable;
  tables->fd = fd;
#else /* !HAVE_HARFBUZZ */
  /* Otherwise, close the fd and free the table directory.  */
  xfree (subtable);
  emacs_close (fd);
#endif /* HAVE_HARFBUZZ */

  return tables;

 bail5:
  xfree (tables->loca_long);
  xfree (tables->loca_short);
 bail4:
  xfree (tables->hhea);
  xfree (tables->maxp);

#ifdef HAVE_MMAP
  if (tables->glyf_table_mapped)
    {
      rc = sfnt_unmap_glyf_table (tables->glyf);

      if (rc)
	emacs_abort ();
    }
  else
#endif /* HAVE_MMAP */
    xfree (tables->glyf);

  xfree (tables->head);

  /* This comes under bail4 due to a peculiarity of how the four
     tables above are validated.  */
  xfree (tables->cmap_data);
 bail3:
  if (tables->uvs)
    sfnt_free_uvs_context (tables->uvs);

  xfree (tables->cmap);
 bail2:
  xfree (subtable);
 bail1:
  emacs_close (fd);
 bail:
  xfree (tables);
  return NULL;
}

/* Open or reference font tables corresponding to the specified font
   DESC.  Return NULL upon failure.  */

static struct sfnt_font_tables *
sfnt_reference_font_tables (struct sfnt_font_desc *desc)
{
  if (desc->refcount)
    {
      desc->refcount++;
      return desc->tables;
    }

  desc->tables = sfnt_open_tables (desc);

  if (!desc->tables)
    return NULL;

  desc->refcount++;
  return desc->tables;
}

/* Dereference font tables corresponding to the specified font
   DESC.  */

static void
sfnt_dereference_font_tables (struct sfnt_font_desc *desc)
{
  if (!desc->refcount)
    emacs_abort ();

  if (--desc->refcount)
    return;

  sfnt_close_tables (desc->tables);
  desc->tables = NULL;
  return;
}

/* Open the font corresponding to the font-entity FONT_ENTITY.  Return
   nil upon failure, else the opened font-object.  */

Lisp_Object
sfntfont_open (struct frame *f, Lisp_Object font_entity,
	       int pixel_size)
{
  struct sfnt_font_info *font_info;
  struct font *font;
  struct sfnt_font_desc *desc;
  Lisp_Object font_object;
  struct charset *charset;
  int point_size, instance, i;
  Display_Info *dpyinfo;
  struct sfnt_font_tables *tables;
  Lisp_Object tem;

  if (XFIXNUM (AREF (font_entity, FONT_SIZE_INDEX)) != 0)
    pixel_size = XFIXNUM (AREF (font_entity, FONT_SIZE_INDEX));
  else if (pixel_size == 0)
    {
      /* This bit was copied from xfont.c.  The values might need
	 adjustment.  */

      if (FRAME_FONT (f))
	pixel_size = FRAME_FONT (f)->pixel_size;
      else
	pixel_size = 12;
    }

  /* Now find the font description corresponding to FONT_ENTITY.  */

  tem = AREF (font_entity, FONT_EXTRA_INDEX);
  if (NILP (tem))
    return Qnil;

  desc = xmint_pointer (XCDR (XCAR (tem)));

  /* Finally, see if a specific instance is associated with
     FONT_ENTITY.  */

  instance = -1;
  if (!NILP (XCDR (tem)))
    instance = XFIXNUM (XCDR (XCAR (XCDR (tem))));

  /* Build the font object.  */
  font_object = font_make_object (VECSIZE (struct sfnt_font_info),
				  font_entity, pixel_size);
  font_info = (struct sfnt_font_info *) XFONT_OBJECT (font_object);

  block_input ();

  /* Initialize all the font driver specific data.  */

  font_info->cmap = NULL;
  font_info->hhea = NULL;
  font_info->maxp = NULL;
  font_info->head = NULL;
  font_info->glyf = NULL;
  font_info->hmtx = NULL;
  font_info->loca_short = NULL;
  font_info->loca_long = NULL;
  font_info->cmap_data = NULL;
  font_info->prep = NULL;
  font_info->fpgm = NULL;
  font_info->cvt = NULL;
  font_info->uvs = NULL;

  font_info->outline_cache.next = &font_info->outline_cache;
  font_info->outline_cache.last = &font_info->outline_cache;
  font_info->outline_cache_size = 0;
  font_info->raster_cache.next = &font_info->raster_cache;
  font_info->raster_cache.last = &font_info->raster_cache;
  font_info->raster_cache_size = 0;
  font_info->interpreter = NULL;
  font_info->scale = 0;
  font_info->instance = -1;
  font_info->blend.coords = NULL;
#ifdef HAVE_MMAP
  font_info->glyf_table_mapped = false;
#endif /* HAVE_MMAP */
#ifdef HAVE_HARFBUZZ
  font_info->hb_font = NULL;
  font_info->fd = -1;
  font_info->directory = NULL;
#endif /* HAVE_HARFBUZZ */

  /* Read required tables.  This font backend is supposed to be used
     mostly on devices with flash memory, so the order in which they
     are read is insignificant.  */

  tables = sfnt_reference_font_tables (desc);

  if (!tables)
    goto bail;

  /* Copy fields from the table structure to the font for fast
     access.  */
  font_info->cmap = tables->cmap;
  font_info->hhea = tables->hhea;
  font_info->maxp = tables->maxp;
  font_info->head = tables->head;
  font_info->hmtx = tables->hmtx;
  font_info->glyf = tables->glyf;
  font_info->loca_short = tables->loca_short;
  font_info->loca_long = tables->loca_long;
  font_info->prep = tables->prep;
  font_info->fpgm = tables->fpgm;
  font_info->cvt  = tables->cvt ;
  font_info->cmap_data = tables->cmap_data;
  font_info->cmap_subtable = tables->cmap_subtable;
  font_info->uvs = tables->uvs;

  /* Calculate the font's scaling factor.  */
  font_info->scale = sfnt_get_scale (font_info->head, pixel_size);

  /* Fill in font data.  */
  font = &font_info->font;
  font->pixel_size = pixel_size;
  font->driver = sfnt_font_driver;
  font->encoding_charset = font->repertory_charset = -1;

  /* Figure out which character set to use.  */
  charset = sfntfont_charset_for_cmap (font_info->cmap_subtable);

  if (!charset)
    goto bail6;

  /* Set the character set IDs.  */
  font->encoding_charset = charset->id;
  font->repertory_charset = charset->id;

  /* Figure out the font ascent and descent.  */
  font->ascent
    = ceil (font_info->hhea->ascent
	    * pixel_size
	    * (1.0 / font_info->head->units_per_em));
  font->descent
    = ceil ((-font_info->hhea->descent)
	    * pixel_size
	    * (1.0 / font_info->head->units_per_em));
  font->height = font->ascent + font->descent;

  /* Set font->max_width to the maximum advance width.  */
  font->max_width = (font_info->hhea->advance_width_max
		     * pixel_size * 1.0 / font_info->head->units_per_em);

  /* Set generic attributes such as type and style.  */
  ASET (font_object, FONT_TYPE_INDEX, sfnt_vendor_name);
  ASET (font_object, FONT_FOUNDRY_INDEX, desc->designer);
  ASET (font_object, FONT_FAMILY_INDEX, Fintern (desc->family, Qnil));
  ASET (font_object, FONT_ADSTYLE_INDEX, desc->adstyle);
  ASET (font_object, FONT_REGISTRY_INDEX,
	sfntfont_registry_for_desc (desc));

  /* Size of 0 means the font is scalable.  */
  ASET (font_object, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (font_object, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (font_object, FONT_SPACING_INDEX, make_fixnum (desc->spacing));

  /* Set the font style.  */

  FONT_SET_STYLE (font_object, FONT_WIDTH_INDEX,
		  make_fixnum (desc->width));
  FONT_SET_STYLE (font_object, FONT_WEIGHT_INDEX,
		  make_fixnum (desc->weight));
  FONT_SET_STYLE (font_object, FONT_SLANT_INDEX,
		  make_fixnum (desc->slant));

  /* Clear various offsets.  */
  font_info->font.baseline_offset = 0;
  font_info->font.relative_compose = 0;
  font_info->font.default_ascent = 0;
  font_info->font.vertical_centering = 0;

  if (!desc->underline_position_set)
    {
      font_info->font.underline_position = -1;
      font_info->font.underline_thickness = 0;
    }
  else
    {
      font_info->font.underline_position
	= sfnt_coerce_fixed (-desc->underline_position
			     * font_info->scale) + 0.5;
      font_info->font.underline_thickness
	= sfnt_coerce_fixed (desc->underline_thickness
			     * font_info->scale) + 0.5;
    }

  /* Now try to set up grid fitting for this font.  */
  dpyinfo = FRAME_DISPLAY_INFO (f);
  point_size = PIXEL_TO_POINT (pixel_size, (dpyinfo->resx
					    * dpyinfo->resy
					    / 2));
  sfntfont_setup_interpreter (font_info, desc, point_size);

  /* If an instance was specified and the font is distortable, set up
     the blend.  */

  if (instance != -1
      && desc->tables->fvar && desc->tables->gvar
      /* Make sure the instance is within range.  */
      && instance < desc->tables->fvar->instance_count)
    {
      tem = AREF (desc->instances, instance);

      if (!NILP (tem))
	{
	  sfnt_init_blend (&font_info->blend, desc->tables->fvar,
			   desc->tables->gvar, desc->tables->avar,
			   desc->tables->cvar);

	  /* Copy over the coordinates.  */
	  for (i = 0; i < desc->tables->fvar->axis_count; ++i)
	    font_info->blend.coords[i]
	      = desc->tables->fvar->instance[instance].coords[i];

	  sfnt_normalize_blend (&font_info->blend);

	  /* Test whether or not the instance is actually redundant,
	     as all of its axis are at their default values.  If so,
	     free the instance.  */

	  for (i = 0; i < desc->tables->fvar->axis_count; ++i)
	    {
	      if (font_info->blend.norm_coords[i])
		break;
	    }

	  if (i == desc->tables->fvar->axis_count)
	    {
	      sfnt_free_blend (&font_info->blend);
	      goto cancel_blend;
	    }

	  /* If an interpreter was specified, distort it now.  */

	  if (font_info->interpreter)
	    sfnt_vary_interpreter (font_info->interpreter,
				   &font_info->blend);

	  font_info->instance = instance;

	  /* Replace the style information with that of the
	     instance.  */

	  FONT_SET_STYLE (font_object, FONT_WIDTH_INDEX,
			  AREF (tem, 2));
	  FONT_SET_STYLE (font_object, FONT_WEIGHT_INDEX,
			  AREF (tem, 3));
	  FONT_SET_STYLE (font_object, FONT_SLANT_INDEX,
			  AREF (tem, 4));
	  ASET (font_object, FONT_ADSTYLE_INDEX, AREF (tem, 1));
	}
    }

 cancel_blend:

  /* Find out the minimum, maximum and average widths.  */
  sfntfont_probe_widths (font_info);

  /* Calculate the xfld name.  */
  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil, Qt);

#ifdef HAVE_HARFBUZZ
  /* HarfBuzz will potentially read font tables after the font has
     been opened by Emacs.  Keep the font open, and record its offset
     subtable.  */
  font_info->fd = tables->fd;
  font_info->directory = tables->directory;
#endif /* HAVE_HARFBUZZ */

  /* Set font->desc so that font tables can be dereferenced if
     anything goes wrong.  */
  font_info->desc = desc;

#ifdef HAVE_MMAP
  /* Link the font onto the font table.  */
  font_info->next = open_fonts;
  open_fonts = font_info;
#endif /* HAVE_MMAP */

  /* Now ascertain if vertical centering is desired by matching the
     font XLFD against vertical-centering-font-regexp.  */

  if (!NILP (font->props[FONT_NAME_INDEX]))
    font->vertical_centering
      = (STRINGP (Vvertical_centering_font_regexp)
	 && (fast_string_match_ignore_case
	     (Vvertical_centering_font_regexp,
	      font->props[FONT_NAME_INDEX]) >= 0));

  /* Set the name of the font file.  */
  font->props[FONT_FILE_INDEX]
    = DECODE_FILE (build_unibyte_string (desc->path));

  /* Encapsulate some information on the font useful while debugging
     (along with being informative in general) in the font name.  */

  AUTO_STRING (format, "%s %s interpreted: %s upem: %s charset: %s"
	       " instance: %s");
  font->props[FONT_FULLNAME_INDEX]
    = CALLN (Fformat, format, desc->family, desc->style,
	     font_info->interpreter ? Qt : Qnil,
	     make_fixnum (font_info->head->units_per_em),
	     CHARSET_NAME (charset),
	     make_fixnum (instance));

  /* All done.  */
  unblock_input ();
  return font_object;

 bail6:
  sfnt_dereference_font_tables (desc);
  font_info->desc = NULL;
 bail:
  unblock_input ();
  return Qnil;
}



/* Metrics computation and other similar font backend functions.  */

/* Return the glyph code corresponding to C inside the font-object
   FONT.  Value is the glyph code upon success, else
   FONT_INVALID_CODE.  */

unsigned int
sfntfont_encode_char (struct font *font, int c)
{
  sfnt_glyph glyph;

  /* Now look up the glyph.  */
  glyph = sfntfont_lookup_glyph ((struct sfnt_font_info *) font, c);

  if (!glyph)
    return FONT_INVALID_CODE;

  return glyph;
}

/* Measure the single glyph GLYPH in the font FONT and return its
   metrics in *PCM.

   Instruct the glyph if possible.

   Value is 0 upon success, 1 otherwise.  */

static int
sfntfont_measure_pcm (struct sfnt_font_info *font, sfnt_glyph glyph,
		      struct font_metrics *pcm)
{
  struct sfnt_glyph_metrics metrics;
  struct sfnt_glyph_outline *outline;

  /* Now get the glyph outline, which is required to obtain the rsb,
     ascent and descent.  */
  outline = sfntfont_get_glyph_outline (glyph, &font->outline_cache,
					font->scale,
					&font->outline_cache_size,
					&font->blend,
					font->instance,
					font->glyf, font->head,
					font->hmtx, font->hhea,
					font->maxp,
					font->loca_short,
					font->loca_long,
					font->interpreter, &metrics,
					&font->state);

  if (!outline)
    return 1;

  /* The left side bearing has already been floored.  */
  pcm->lbearing = metrics.lbearing / 65536;
  pcm->rbearing = SFNT_CEIL_FIXED (outline->xmax) / 65536;

  /* The advance is already rounded; ceil the ascent and descent.  */
  pcm->width = metrics.advance / 65536;
  pcm->ascent = SFNT_CEIL_FIXED (outline->ymax) / 65536;
  pcm->descent = SFNT_CEIL_FIXED (-outline->ymin) / 65536;

  sfntfont_dereference_outline (outline);
  return 0;
}

/* Return the total text extents of NGLYPHS glyphs given as CODE in
   the single font metrics array METRICS.  */

void
sfntfont_text_extents (struct font *font, const unsigned int *code,
		       int nglyphs, struct font_metrics *metrics)
{
  int i, total_width;
  struct font_metrics pcm;

  total_width = 0;

  /* First clear the metrics array.  */
  memset (metrics, 0, sizeof *metrics);

  /* Get the metrcs one by one, then sum them up.  */
  for (i = 0; i < nglyphs; ++i)
    {
      if (!sfntfont_measure_pcm ((struct sfnt_font_info *) font,
				 code[i], &pcm))
	{
	  /* Add the per-char metric (PCM) to the metrics in
	     METRICS.  */

	  if (total_width + pcm.lbearing < metrics->lbearing)
	    metrics->lbearing = total_width + pcm.lbearing;

	  if (total_width + pcm.rbearing > metrics->rbearing)
	    metrics->rbearing = total_width + pcm.rbearing;

	  if (pcm.ascent > metrics->ascent)
	    metrics->ascent = pcm.ascent;

	  if (pcm.descent > metrics->descent)
	    metrics->descent = pcm.descent;

	  total_width += pcm.width;
	}
    }

  metrics->width = total_width;
}

/* Close the font FONT, discarding all tables inside it and
   dereferencing all cached outlines and rasters.  */

void
sfntfont_close (struct font *font)
{
  struct sfnt_font_info *info;
#ifdef HAVE_MMAP
  struct sfnt_font_info **next;
#endif /* HAVE_MMAP */

  info = (struct sfnt_font_info *) font;

  /* If info->desc is still set, dereference the font tables.  */
  if (info->desc)
    sfnt_dereference_font_tables (info->desc);
  info->desc = NULL;

  /* Free the interpreter, which is created on a per font basis.  */
  xfree (info->interpreter);

  /* Clear these fields.  It seems that close can be called twice,
     once during font driver destruction, and once during GC.  */

  info->cmap = NULL;
  info->hhea = NULL;
  info->maxp = NULL;
  info->head = NULL;
  info->hhea = NULL;
  info->glyf = NULL;
  info->loca_short = NULL;
  info->loca_long = NULL;
  info->cmap_data = NULL;
  info->prep = NULL;
  info->fpgm = NULL;
  info->cvt = NULL;
  info->interpreter = NULL;
  info->uvs = NULL;

  /* Deinitialize the blend.  */
  if (info->instance != -1 && info->blend.coords)
    sfnt_free_blend (&info->blend);
  info->instance = -1;

#ifdef HAVE_MMAP

  /* Unlink INFO.  */

  next = &open_fonts;
  while (*next && (*next) != info)
    next = &(*next)->next;

  if (*next)
    *next = info->next;
  info->next = NULL;

#endif /* HAVE_MMAP */

#ifdef HAVE_HARFBUZZ
  /* These fields will be freed or closed by
     sfnt_dereference_font_tables, but clear them here for good
     measure.  */
  info->directory = NULL;
  info->fd = -1;

  /* Free any hb_font created.  */

  if (info->hb_font)
    {
      hb_font_destroy (info->hb_font);
      info->hb_font = NULL;
    }
#endif

  sfntfont_free_outline_cache (&info->outline_cache);
  sfntfont_free_raster_cache (&info->raster_cache);
}



/* Glyph display.  */

/* Function called to actually draw rasters to the glass.  */
static sfntfont_put_glyph_proc sfnt_put_glyphs;

/* Draw glyphs in S->char2b starting from FROM to TO, with the origin
   at X and baseline at Y.  Fill the background from X, Y +
   FONT_DESCENT to X + S->background_width, Y - FONT_ASCENT with the
   background color if necessary.  Use the foreground and background
   colors in S->gc.  */

int
sfntfont_draw (struct glyph_string *s, int from, int to,
	       int x, int y, bool with_background)
{
  int length;
  struct sfnt_raster **rasters;
  int *x_coords, current_x, i;
  struct sfnt_glyph_outline *outline;
  struct font *font;
  struct sfnt_font_info *info;
  struct sfnt_glyph_metrics metrics;

  length = to - from;
  font = s->font;
  info = (struct sfnt_font_info *) font;

  rasters = alloca (length * sizeof *rasters);
  x_coords = alloca (length * sizeof *x_coords);
  current_x = x;

  /* Get rasters and outlines for them.  */
  for (i = from; i < to; ++i)
    {
      /* Look up the outline.  */
      outline = sfntfont_get_glyph_outline (s->char2b[i],
					    &info->outline_cache,
					    info->scale,
					    &info->outline_cache_size,
					    &info->blend,
					    info->instance,
					    info->glyf, info->head,
					    info->hmtx, info->hhea,
					    info->maxp,
					    info->loca_short,
					    info->loca_long,
					    info->interpreter,
					    &metrics,
					    &info->state);
      x_coords[i - from] = 0;

      if (!outline)
	{
	  rasters[i - from] = NULL;
	  continue;
	}

      /* Rasterize the outline.  */
      rasters[i - from] = sfntfont_get_glyph_raster (s->char2b[i],
						     &info->raster_cache,
						     outline,
						     &info->raster_cache_size);
      sfntfont_dereference_outline (outline);

      if (!rasters[i - from])
	continue;

      /* Now work out where to put the outline.  */
      x_coords[i - from] = current_x;

      if (s->padding_p)
	current_x += 1;
      else
	current_x += metrics.advance / 65536;
    }

  /* Call the window system function to put the glyphs to the
     frame.  */
  sfnt_put_glyphs (s, from, to, x, y, with_background,
		   rasters, x_coords);

  /* Dereference all the rasters.  */
  for (i = 0; i < from - to; ++i)
    {
      if (rasters[i])
	sfntfont_dereference_raster (rasters[i]);
    }

  return 1;
}



/* Other callbacks.  */

/* Return a list of each font family known to Emacs.  F is supposed to
   be a frame but is ignored.  */

Lisp_Object
sfntfont_list_family (struct frame *f)
{
  Lisp_Object families, tem, next;
  struct sfnt_font_desc *desc;

  families = Qnil;

  for (desc = system_fonts; desc; desc = desc->next)
    /* Add desc->family to the list.  */
    families = Fcons (desc->family, families);

  /* Sort families in preparation for removing duplicates.  */
  families = CALLN (Fsort, families, Qstring_lessp);

  /* Remove each duplicate within families.  */

  tem = families;
  while (!NILP (tem) && !NILP ((next = XCDR (tem))))
    {
      /* If the two strings are equal.  */
      if (!NILP (Fstring_equal (XCAR (tem), XCAR (next))))
	/* Set tem's cdr to the cons after the next item.  */
	XSETCDR (tem, XCDR (next));
      else
	/* Otherwise, start considering the next item.  */
	tem = next;
    }

  /* Intern each font family.  */

  tem = families;

  FOR_EACH_TAIL (tem)
    XSETCAR (tem, Fintern (XCAR (tem), Qnil));

  return families;
}



/* Unicode Variation Selector (UVS) support.  This is typically
   required for Harfbuzz.  */

/* Given a FONT object, a character C, and VARIATIONS, return the
   number of non-default variation glyphs, and their glyph ids in
   VARIATIONS.

   For each variation selector character K with a non-default glyph in
   the variation selector range 0xFE00 to 0xFE0F, set variations[K -
   0xFE0] to its ID.

   For each variation selector character K with a non-default glyph in
   the variation selector range 0xE0100 to 0xE01EF, set variations[K -
   0xE0100 + 16] to its ID.

   If value is more than 0, set all other members of VARIATIONS to 0.
   Else, the contents of VARIATIONS are undefined.  */

int
sfntfont_get_variation_glyphs (struct font *font, int c,
			       unsigned variations[256])
{
  struct sfnt_font_info *info;
  size_t i, index;
  int n;
  struct sfnt_mapped_variation_selector_record *record;
  sfnt_glyph default_glyph;

  info = (struct sfnt_font_info *) font;
  n = 0;

  /* Return 0 if there is no UVS mapping table.  */

  if (!info->uvs)
    return 0;

  /* Clear the variations array.  */

  memset (variations, 0, sizeof *variations * 256);

  /* Find the first 0xFExx selector.  */

  i = 0;
  while (i < info->uvs->num_records
	 && info->uvs->records[i].selector < 0xfe00)
    ++i;

  /* Get the glyph represented by C, used when C is present within a
     default value table.  */

  default_glyph = sfntfont_lookup_glyph (info, c);

  /* Fill in selectors 0 to 15.  */

  while (i < info->uvs->num_records
	 && info->uvs->records[i].selector <= 0xfe0f)
    {
      record = &info->uvs->records[i];
      index = info->uvs->records[i].selector - 0xfe00 + 16;

      /* Handle invalid unsorted tables.  */

      if (record->selector < 0xfe00)
	return 0;

      /* If there are default mappings in this record, ascertain if
	 this glyph matches one of them.  */

      if (record->default_uvs
	  && sfnt_is_character_default (record->default_uvs, c))
	{
	  variations[index] = default_glyph;

	  if (default_glyph)
	    ++n;

	  goto next_selector;
	}

      /* If record has no non-default mappings, continue on to the
	 next selector.  */

      if (!record->nondefault_uvs)
	goto next_selector;

      /* Find the glyph ID associated with C and put it in
	 VARIATIONS.  */

      variations[index]
	= sfnt_variation_glyph_for_char (record->nondefault_uvs, c);

      if (variations[index])
	++n;

    next_selector:
      ++i;
    }

  /* Find the first 0xE0100 selector.  */

  i = 0;
  while (i < info->uvs->num_records
	 && info->uvs->records[i].selector < 0xe0100)
    ++i;

  /* Fill in selectors 16 to 255.  */

  while (i < info->uvs->num_records
	 && info->uvs->records[i].selector <= 0xe01ef)
    {
      record = &info->uvs->records[i];
      index = info->uvs->records[i].selector - 0xe0100 + 16;

      /* Handle invalid unsorted tables.  */

      if (record->selector < 0xe0100)
	return 0;

      /* If there are default mappings in this record, ascertain if
	 this glyph matches one of them.  */

      if (record->default_uvs
	  && sfnt_is_character_default (record->default_uvs, c))
	{
	  variations[index] = default_glyph;

	  if (default_glyph)
	    ++n;

	  goto next_selector_1;
	}

      /* If record has no non-default mappings, continue on to the
	 next selector.  */

      if (!record->nondefault_uvs)
	goto next_selector_1;

      /* Find the glyph ID associated with C and put it in
	 VARIATIONS.  */

      variations[index]
	= sfnt_variation_glyph_for_char (record->nondefault_uvs, c);

      if (variations[index])
	++n;

    next_selector_1:
      ++i;
    }

  return n;
}



/* mmap specific stuff.  */

#ifdef HAVE_MMAP

/* Return whether or not ADDR lies in a mapped glyph, and bus faults
   should be ignored.  */

bool
sfntfont_detect_sigbus (void *addr)
{
  struct sfnt_font_info *info;

  for (info = open_fonts; info; info = info->next)
    {
      if (info->glyf_table_mapped
	  && (unsigned char *) addr >= info->glyf->glyphs
	  && (unsigned char *) addr < (info->glyf->glyphs
				       + info->glyf->size))
	return true;
    }

  return false;
}

#endif /* HAVE_MMAP */



/* Harfbuzz font support.  */

#ifdef HAVE_HARFBUZZ

#ifdef HAVE_MMAP

/* Unmap the specified table.  */

static void
sfntfont_unmap_blob (void *ptr)
{
  if (sfnt_unmap_table (ptr))
    emacs_abort ();

  xfree (ptr);
}

#endif /* HAVE_MMAP */

/* Given a font DATA and a tag TAG, return the data of the
   corresponding font table as a HarfBuzz blob.  */

static hb_blob_t *
sfntfont_get_font_table (hb_face_t *face, hb_tag_t tag, void *data)
{
  size_t size;
  struct sfnt_font_info *info;
#ifdef HAVE_MMAP
  struct sfnt_mapped_table *table;
  hb_blob_t *blob;

  info = data;
  table = xmalloc (sizeof *table);

  if (!sfnt_map_table (info->fd, info->directory, tag,
		       table))
    {
      /* Create an hb_blob_t and return it.
         TODO: record this mapping properly so that SIGBUS can
	 be handled.  */

      blob = hb_blob_create (table->data, table->length,
			     HB_MEMORY_MODE_READONLY,
			     table, sfntfont_unmap_blob);

      /* Note that sfntfont_unmap_blob will be called if the empty
	 blob is returned.  */
      return blob;
    }

  xfree (table);
#else /* !HAVE_MMAP */

  /* Try to read the table conventionally.  */
  info = data;
#endif /* HAVE_MMAP */

  data = sfnt_read_table (info->fd, info->directory, tag,
			  &size);

  if (!data)
    return NULL;

  return hb_blob_create (data, size, HB_MEMORY_MODE_WRITABLE,
			 data, xfree);
}

/* Create or return a HarfBuzz font object corresponding to the
   specified FONT.  Return the scale to convert between fwords and
   pixels in POSITION_UNIT.  */

hb_font_t *
sfntfont_begin_hb_font (struct font *font, double *position_unit)
{
  struct sfnt_font_info *info;
  hb_face_t *face;
  int factor;

  info = (struct sfnt_font_info *) font;

  if (info->hb_font)
    {
      /* Calculate the scale factor.  */
      *position_unit = 1.0 / 64.0;
      return info->hb_font;
    }

  /* Create a face and then a font.  */
  face = hb_face_create_for_tables (sfntfont_get_font_table, font,
				    NULL);

  if (hb_face_get_glyph_count (face) > 0)
    {
      info->hb_font = hb_font_create (face);
      if (!info->hb_font)
	goto bail;

      factor = font->pixel_size;

      /* Set the scale and PPEM values.  */
      hb_font_set_scale (info->hb_font, factor * 64, factor * 64);
      hb_font_set_ppem (info->hb_font, factor, factor);

#ifdef HAVE_HB_FONT_SET_VAR_NAMED_INSTANCE
      /* Set the instance if this is a distortable font.  */
      if (info->instance != -1)
	hb_font_set_var_named_instance (info->hb_font,
					info->instance);
#endif /* HAVE_HB_FONT_SET_VAR_NAMED_INSTANCE */

      /* This is needed for HarfBuzz before 2.0.0; it is the default
	 in later versions.  */
      hb_ot_font_set_funcs (info->hb_font);
    }

 bail:
  hb_face_destroy (face);

  /* Calculate the scale factor.  */
  *position_unit = 1.0 / 64.0;
  return info->hb_font;
}

#endif /* HAVE_HARFBUZZ */



void
syms_of_sfntfont (void)
{
  DEFSYM (Qutf_16be, "utf-16be");
  DEFSYM (Qmac_roman, "mac-roman");
  DEFSYM (Qchinese_big5, "chinese-big5");
  DEFSYM (Qunicode_bmp, "unicode-bmp");
  DEFSYM (Qucs, "ucs");
  DEFSYM (Qjapanese_jisx0208, "japanese-jisx0208");
  DEFSYM (Qgbk, "gbk");
  DEFSYM (Qkorean_ksc5601, "korean-ksc5601");
  DEFSYM (Qapple_roman, "apple-roman");
  DEFSYM (Qjisx0208_1983_0, "jisx0208.1983-0");
  DEFSYM (Qksc5601_1987_0, "ksc5601.1987-0");
  DEFSYM (Qzh, "zh");
  DEFSYM (Qja, "ja");
  DEFSYM (Qko, "ko");
  DEFSYM (Qfont_instance, "font-instance");

  /* Char-table purpose.  */
  DEFSYM (Qfont_lookup_cache, "font-lookup-cache");

  /* Default foundry name.  */
  DEFSYM (Qmisc, "misc");

  /* Predicated employed for sorting font family lists.  */
  DEFSYM (Qstring_lessp, "string-lessp");

  /* Set up staticpros.  */
  sfnt_vendor_name = Qnil;
  staticpro (&sfnt_vendor_name);

  /* This variable is supposed to be set by the platform specific part
     of the font backend.  */
  DEFVAR_LISP ("sfnt-default-family-alist", Vsfnt_default_family_alist,
    doc: /* Alist between "emulated" and actual font family names.
Much Emacs code assumes that font families named "Monospace" and "Sans
Serif" exist, and map to the default monospace and Sans Serif fonts on
a system.  When the `sfnt' font driver is asked to look for a font
with one of the families in this alist, it uses its value instead.  */);
  Vsfnt_default_family_alist = Qnil;

  DEFVAR_LISP ("sfnt-uninstructable-family-regexp",
	       Vsfnt_uninstructable_family_regexp,
    doc: /* Regexp matching font families whose glyphs must not be instructed.
If nil, instruction code supplied by all fonts will be executed.  This
variable takes effect when a font entity is opened, not after, and
therefore won't affect the scaling of realized faces until their
frames' font caches are cleared (see `clear-font-cache').

TrueType fonts incorporate instruction code executed to fit each glyph
to a pixel grid, so as to improve the visual fidelity of each glyph by
eliminating artifacts and chance effects consequent upon the direct
upscaling of glyph outline data.  Instruction code is occasionally
incompatible with Emacs and must be disregarded.  */);
  Vsfnt_uninstructable_family_regexp = Qnil;

  DEFVAR_BOOL ("sfnt-raster-glyphs-exactly", sfnt_raster_glyphs_exactly,
    doc: /* How font glyph outlines should be converted to graphics.
If non-nil, glyphs will be displayed in a more precise manner, at the
cost of performance on devices where floating-point math operations
are slow.  */);
  sfnt_raster_glyphs_exactly = true;
}

void
mark_sfntfont (void)
{
  struct sfnt_font_desc *desc;

  /* Mark each font desc.  */
  for (desc = system_fonts; desc; desc = desc->next)
    {
      mark_object (desc->family);
      mark_object (desc->style);
      mark_object (desc->adstyle);
      mark_object (desc->instances);
      mark_object (desc->languages);
      mark_object (desc->registry);
      mark_object (desc->char_cache);
      mark_object (desc->designer);
    }
}

void
init_sfntfont (void)
{

}



/* Initialize the sfntfont font driver.  VENDOR_TYPE is the type of
   all font entities created.  DRIVER is the font driver that is saved
   in font objects.  PUT_GLYPHS is a function that is called with 8
   arguments, S, FROM, TO, X, Y, WITH_BACKGROUND, RASTERS, and
   X_COORDS, and should draw all the rasters in RASTERS to S->f,
   originating at X_COORDS[i], Y, along with filling the background if
   WITH_BACKGROUND is specified.  */

void
init_sfntfont_vendor (Lisp_Object vendor_name,
		      const struct font_driver *driver,
		      sfntfont_put_glyph_proc put_glyphs)
{
  sfnt_vendor_name = vendor_name;
  sfnt_font_driver = driver;
  sfnt_put_glyphs = put_glyphs;
}
