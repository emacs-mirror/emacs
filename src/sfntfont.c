/* sfnt format font driver for GNU Emacs.

Copyright (C) 2023 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <config.h>

#include <fcntl.h>
#include <ctype.h>

#include "lisp.h"

#include "blockinput.h"
#include "charset.h"
#include "coding.h"
#include "font.h"
#include "frame.h"
#include "math.h"
#include "sfnt.h"
#include "sfntfont.h"

/* For FRAME_FONT.  */
#include TERM_HEADER

/* Generic font driver for sfnt-based fonts (currently TrueType, but
   it would be easy to add CFF support in the future with a PostScript
   renderer.)

   This is not a complete font driver.  Hooks must be supplied by the
   platform implementer to draw glyphs.  */



/* Description of a font that hasn't been opened.  */

struct sfnt_font_desc
{
  /* Next font in this list.  */
  struct sfnt_font_desc *next;

  /* Family name of the font.  */
  Lisp_Object family;

  /* Style name of the font.  */
  Lisp_Object style;

  /* Designer (foundry) of the font.  */
  Lisp_Object designer;

  /* Style tokens that could not be parsed.  */
  Lisp_Object adstyle;

  /* List of design languages.  */
  Lisp_Object languages;

  /* Numeric width, weight, slant and spacing.  */
  int width, weight, slant, spacing;

  /* Path to the font file.  */
  char *path;

  /* char table consisting of characters already known to be
     present in the font.  */
  Lisp_Object char_cache;

  /* Whether or not the character map can't be used by Emacs.  */
  bool cmap_invalid;

  /* The header of the cmap being used.  May be invalid, in which case
     platform_id will be 500.  */
  struct sfnt_cmap_encoding_subtable subtable;

  /* The offset of the table directory within PATH.  */
  off_t offset;
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

  family_data = sfnt_find_name (name, SFNT_NAME_FONT_FAMILY,
				&family_rec);
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

/* Decode the foundry names from the name table NAME.  Return the
   foundry name, or nil upon failure.  */

static Lisp_Object
sfnt_decode_foundry_name (struct sfnt_name_table *name)
{
  struct sfnt_name_record designer_rec;
  unsigned char *designer_data;

  designer_data = sfnt_find_name (name, SFNT_NAME_DESIGNER,
				  &designer_rec);

  if (!designer_data)
    return Qnil;

  return sfnt_decode_font_string (designer_data,
				  designer_rec.platform_id,
				  designer_rec.platform_specific_id,
				  designer_rec.language_id,
				  designer_rec.length);
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
  char *style, *single, *saveptr;
  int i;

  /* Fill in default values.  slant seems to not be consistent with
     Fontconfig.  */
  desc->weight = 80;
  desc->slant = 100;
  desc->width = 100;

  /* Split the style into spaces.  As long as no weight, slant, or
     width is encountered, look in the corresponding descriptions
     array.  GC must not happen inside this block.  */
  style = SSDATA (Fdowncase (style_name));
  saveptr = NULL;

  while ((single = strtok_r (style, " ", &saveptr)))
    {
      style = NULL;

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
	 the first letter and set it as the adstyle.  */

      if (strlen (single))
	{
	  if (islower (single[0]))
	    single[0] = toupper (single[0]);

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
    return;

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

/* Enumerate the offset subtable SUBTABLES in the file FD, whose file
   name is FILE.  OFFSET should be the offset of the subtable within
   the font file, and is recorded for future use.  Value is 1 upon
   failure, else 0.  */

static int
sfnt_enum_font_1 (int fd, const char *file,
		  struct sfnt_offset_subtable *subtables,
		  off_t offset)
{
  struct sfnt_font_desc *desc;
  struct sfnt_head_table *head;
  struct sfnt_name_table *name;
  struct sfnt_meta_table *meta;
  Lisp_Object family, style;

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

  /* meta is not required, nor present on many non-Apple fonts.  */
  meta = sfnt_read_meta_table (fd, subtables);

  /* Decode the family and style from the name table.  */
  if (sfnt_decode_family_style (name, &family, &style))
    goto bail3;

  /* Set the family.  */
  desc->family = family;
  desc->designer = sfnt_decode_foundry_name (name);
  desc->char_cache = Qnil;
  desc->subtable.platform_id = 500;

  /* Parse the style.  */
  sfnt_parse_style (style, desc);

  /* If the meta table exists, parse the list of design languages.  */
  if (meta)
    sfnt_parse_languages (meta, desc);

  /* Figure out the spacing.  Some fancy test like what Fontconfig
     does is probably in order but not really necessary.  */
  if (!NILP (Fstring_search (Fdowncase (family),
			     build_string ("mono"),
			     Qnil)))
    desc->spacing = 100; /* FC_MONO */

  /* Finally add mac-style flags.  Allow them to override styles that
     have not been found.  */

  if (head->mac_style & 01 && desc->weight == 80) /* Bold */
    desc->weight = 200;

  if (head->mac_style & 02 && desc->slant == 0) /* Italic */
    desc->slant = 100;

  /* Set the style, link the desc onto system_fonts and return.  */
  desc->style = style;
  desc->next = system_fonts;
  system_fonts = desc;

  xfree (meta);
  xfree (name);
  xfree (head);
  return 0;

 bail3:
  xfree (meta);
  xfree (name);
 bail2:
  xfree (head);
 bail1:
  xfree (desc);
  return 1;
}

/* Enumerate the font FILE into the list of system fonts.  Return 1 if
   it could not be enumerated, 0 otherwise.

   FILE can either be a TrueType collection file containing TrueType
   fonts, or a TrueType font itself.  */

int
sfnt_enum_font (const char *file)
{
  int fd, rc;
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
	  if (lseek (fd, ttc->offset_table[i], SEEK_SET)
	      != ttc->offset_table[i])
	    continue;

	  subtables = sfnt_read_table_directory (fd);

	  if (!subtables)
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

/* Pick the best character map in the cmap table CMAP.  Use the
   subtables in SUBTABLES and DATA.  Return the subtable data and the
   subtable in *SUBTABLE upon success, NULL otherwise.  */

static struct sfnt_cmap_encoding_subtable_data *
sfntfont_select_cmap (struct sfnt_cmap_table *cmap,
		      struct sfnt_cmap_encoding_subtable *subtables,
		      struct sfnt_cmap_encoding_subtable_data **data,
		      struct sfnt_cmap_encoding_subtable *subtable)
{
  int i;

  /* First look for a non-BMP Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (data[i] && sfntfont_identify_cmap (subtables[i]) == 2)
	{
	  *subtable = subtables[i];
	  return data[i];
	}
    }

  /* Next, look for a BMP only Unicode cmap.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      if (data[i] && sfntfont_identify_cmap (subtables[i]) == 1)
	{
	  *subtable = subtables[i];
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

  if (fd < 1)
    return;

  font = sfnt_read_table_directory (fd);

  if (!font)
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
				subtable);

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

  /* Now return whether or not the glyph is present.  */
  present = sfnt_lookup_glyph (font_character, *cmap) != 0;

  /* Cache the result.  Store Qlambda when not present, Qt
     otherwise.  */

  if (NILP (desc->char_cache))
    desc->char_cache = Fmake_char_table (Qfont_lookup_cache,
					 Qnil);

  Fset_char_table_range (desc->char_cache, character,
			 present ? Qt : Qlambda);
  return present;
}

/* Return whether or not the font description DESC satisfactorily
   matches the font specification FONT_SPEC.  */

static bool
sfntfont_list_1 (struct sfnt_font_desc *desc, Lisp_Object spec)
{
  Lisp_Object tem, extra, tail;
  struct sfnt_cmap_encoding_subtable_data *cmap;
  size_t i;
  struct sfnt_cmap_encoding_subtable subtable;

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
	      && Fstring_equal (SYMBOL_NAME (tem),
			        XCAR (XCAR (tail))))
	    {
	      /* Special family found.  */
	      tem = Fintern (XCDR (XCAR (tail)), Qnil);
	      break;
	    }
	}
    }

  if (!NILP (tem) && NILP (Fstring_equal (SYMBOL_NAME (tem),
					  desc->family)))
    return false;

  /* Check that the adstyle specified matches.  */

  tem = AREF (spec, FONT_ADSTYLE_INDEX);
  if (!NILP (tem) && NILP (Fequal (tem, desc->adstyle)))
    return false;

  /* Check the style.  */

  if (FONT_WIDTH_NUMERIC (spec) != -1
      && FONT_WIDTH_NUMERIC (spec) != desc->width)
    return false;

  if (FONT_WEIGHT_NUMERIC (spec) != -1
      && FONT_WEIGHT_NUMERIC (spec) != desc->weight)
    return false;

  if (FONT_SLANT_NUMERIC (spec) != -1
      && FONT_SLANT_NUMERIC (spec) != desc->slant)
    return false;

  /* Handle extras.  */
  extra = AREF (spec, FONT_EXTRA_INDEX);

  if (NILP (extra))
    return true;

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
	  for (i = 0; i < ASIZE (tem); ++i)
	    {
	      if (FIXNUMP (AREF (tem, i)))
		{
		  if (!sfntfont_lookup_char (desc, AREF (tem, i),
					     &cmap, &subtable))
		    goto fail;

		  /* One character is enough to pass a font.  Don't
		     look at too many.  */
		  break;
		}
	    }
	}
      else if (CONSP (tem) && CONSP (XCDR (tem)))
	{
	  tem = XCDR (tem);

	  /* tem is a list of each characters, one of which must be
	     present in the font.  */
	  FOR_EACH_TAIL_SAFE (tem)
	    {
	      if (FIXNUMP (XCAR (tem)))
		{
		  if (!sfntfont_lookup_char (desc, XCAR (tem), &cmap,
					     &subtable))
		    goto fail;

		  /* One character is enough to pass a font.  Don't
		     look at too many.  */
		  break;
		}
	    }
	}
    }

  /* Now check that the language is supported.  */
  tem = assq_no_quit (QClang, extra);
  if (!NILP (tem) && NILP (Fmemq (tem, desc->languages)))
    goto fail;

  /* Set desc->subtable if cmap was specified.  */
  if (cmap)
    desc->subtable = subtable;

  xfree (cmap);
  return true;

 fail:
  /* The cmap might've been read in and require deallocation.  */
  xfree (cmap);
  return false;
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
    {
      /* desc->subtable.platform_id is now set.  CMAP is already free,
	 because it is not actually used.  */

      switch (desc->subtable.platform_id)
	{
	case SFNT_PLATFORM_UNICODE:
	  /* Reject variation selector and last resort tables.  */
	  if ((desc->subtable.platform_specific_id
	       == SFNT_UNICODE_VARIATION_SEQUENCES)
	      || (desc->subtable.platform_specific_id
		  == SFNT_UNICODE_LAST_RESORT))
	    return Qnil;

	  return Qiso10646_1;

	case SFNT_PLATFORM_MACINTOSH:

	  switch (desc->subtable.platform_specific_id)
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

	  switch (desc->subtable.platform_specific_id)
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

  return Qnil;
}

/* Return a font-entity that represents the font descriptor (unopened
   font) DESC.  */

static Lisp_Object
sfntfont_desc_to_entity (struct sfnt_font_desc *desc)
{
  Lisp_Object entity;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, sfnt_vendor_name);

  if (!NILP (desc->designer))
    ASET (entity, FONT_FOUNDRY_INDEX,
	  Fintern (desc->designer, Qnil));

  ASET (entity, FONT_FAMILY_INDEX, Fintern (desc->family, Qnil));
  ASET (entity, FONT_ADSTYLE_INDEX, Qnil);
  ASET (entity, FONT_REGISTRY_INDEX,
	sfntfont_registry_for_desc (desc));

  /* Size of 0 means the font is scalable.  */
  ASET (entity, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (entity, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (entity, FONT_SPACING_INDEX,
	make_fixnum (desc->spacing));

  FONT_SET_STYLE (entity, FONT_WIDTH_INDEX,
		  make_fixnum (desc->width));
  FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		  make_fixnum (desc->weight));
  FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		  make_fixnum (desc->slant));

  ASET (entity, FONT_ADSTYLE_INDEX, Qnil);

  /* Set FONT_EXTRA_INDEX to a pointer to the font description.  Font
     descriptions are never supposed to be freed.  */
  ASET (entity, FONT_EXTRA_INDEX,
	list1 (Fcons (Qfont_entity, make_mint_ptr (desc))));

  return entity;
}

/* Return a list of font-entities matching the specified
   FONT_SPEC.  */

Lisp_Object
sfntfont_list (struct frame *f, Lisp_Object font_spec)
{
  Lisp_Object matching, tem;
  struct sfnt_font_desc *desc;

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
      if (sfntfont_list_1 (desc, font_spec))
	matching = Fcons (sfntfont_desc_to_entity (desc), matching);
    }

  unblock_input ();

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
    SFNT_OUTLINE_CACHE_SIZE = 128,
    SFNT_RASTER_CACHE_SIZE  = 100,
  };

/* Caching subsystem.  Generating outlines from glyphs is expensive,
   and so is rasterizing them, so two caches are maintained for both
   glyph outlines and rasters.  */

struct sfnt_outline_cache
{
  /* Next and last cache buckets.  */
  struct sfnt_outline_cache *next, *last;

  /* Pointer to outline.  */
  struct sfnt_glyph_outline *outline;

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
};

/* Return the glyph identified by GLYPH from the glyf and loca table
   specified in DCONTEXT.  Set *NEED_FREE to true.  */

static struct sfnt_glyph *
sfntfont_get_glyph (sfnt_glyph glyph, void *dcontext,
		    bool *need_free)
{
  struct sfntfont_get_glyph_outline_dcontext *tables;

  tables = dcontext;
  *need_free = true;

  return sfnt_read_glyph (glyph, tables->glyf,
			  tables->loca_short,
			  tables->loca_long);
}

/* Free the glyph identified by GLYPH.  */

static void
sfntfont_free_glyph (struct sfnt_glyph *glyph, void *dcontext)
{
  sfnt_free_glyph (glyph);
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
   Use the pixel size PIXEL_SIZE, the glyf table GLYF, and the head
   table HEAD.  Keep *CACHE_SIZE updated with the number of elements
   in the cache.

   Use the offset information in the long or short loca tables
   LOCA_LONG and LOCA_SHORT, whichever is set.

   Use the specified HMTX, HHEA and MAXP tables when instructing
   compound glyphs.

   If INTERPRETER is non-NULL, then possibly use the unscaled glyph
   metrics in METRICS and the interpreter STATE to instruct the glyph.

   Return the outline with an incremented reference count and enter
   the generated outline into CACHE upon success, possibly discarding
   any older outlines, or NULL on failure.  */

static struct sfnt_glyph_outline *
sfntfont_get_glyph_outline (sfnt_glyph glyph_code,
			    struct sfnt_outline_cache *cache,
			    int pixel_size, int *cache_size,
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

  start = cache->next;

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

	  return start->outline;
	}
    }

  /* Not already cached.  Get the glyph.  */
  glyph = sfnt_read_glyph (glyph_code, glyf,
			   loca_short, loca_long);

  if (!glyph)
    return NULL;

  /* Try to instruct the glyph if INTERPRETER is specified.
     TODO: support compound glyphs.  */

  outline = NULL;

  dcontext.loca_long = loca_long;
  dcontext.loca_short = loca_short;
  dcontext.glyf = glyf;

  if (interpreter)
    {
      if (glyph->simple)
	{
	  /* Restore the interpreter state from the snapshot taken
	     after loading the preprogram.  */
	  interpreter->state = *state;

	  error = sfnt_interpret_simple_glyph (glyph, interpreter,
					       metrics, &value);
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
					       metrics, &dcontext,
					       &value);

      if (!error)
	{
	  outline = sfnt_build_instructed_outline (value);
	  xfree (value);
	}
    }

  if (!outline)
    outline = sfnt_build_glyph_outline (glyph, head, pixel_size,
					sfntfont_get_glyph,
					sfntfont_free_glyph,
					&dcontext);
  xfree (glyph);

  if (!outline)
    return NULL;

  start = xmalloc (sizeof *start);
  start->glyph = glyph_code;
  start->outline = outline;

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

  /* Return the cached outline.  */
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
  raster = sfnt_raster_glyph_outline (outline);

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
}



/* Opening fonts.  */

struct sfnt_font_info
{
  /* Parent font structure.  */
  struct font font;

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
};

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

/* Probe and set FONT_INFO->font.average_width,
   FONT_INFO->font.space_width, and FONT_INFO->font.min_width
   according to the tables contained therein.  */

static void
sfntfont_probe_widths (struct sfnt_font_info *font_info)
{
  int i, num_characters, total_width;
  sfnt_glyph glyph;
  struct sfnt_glyph_metrics metrics;

  num_characters = 0;
  total_width = 0;

  /* First set some reasonable default values.  */
  font_info->font.average_width = font_info->font.pixel_size;
  font_info->font.space_width = font_info->font.pixel_size;
  font_info->font.min_width = 1;

  /* Next, loop through the common ASCII characters.  Tally up their
     advance widths and set space_width if necessary.  */
  for (i = 0; i < 127; ++i)
    {
      glyph = sfntfont_lookup_glyph (font_info, i);

      if (!glyph)
	continue;

      /* Now look up the metrics of this glyph.  */
      if (sfnt_lookup_glyph_metrics (glyph, font_info->font.pixel_size,
				     &metrics, font_info->hmtx,
				     font_info->hhea, font_info->head,
				     font_info->maxp))
	continue;

      /* Increase the number of characters.  */
      num_characters++;

      /* Add the advance to total_width.  */
      total_width += metrics.advance >> 16;

      /* Update min_width if it hasn't been set yet or is wider.  */
      if (font_info->font.min_width == 1
	  || font_info->font.min_width > metrics.advance >> 16)
	font_info->font.min_width = metrics.advance >> 16;

      /* If i is the space character, set the space width.  Make sure
	 to round this up.  */
      if (i == 32)
	font_info->font.space_width
	  = SFNT_CEIL_FIXED (metrics.advance) >> 16;
    }

  /* Now, if characters were found, set average_width.  */
  if (num_characters)
    font_info->font.average_width = total_width / num_characters;
}

/* Initialize the instruction interpreter for INFO, whose file and
   offset subtable should be respectively FD and SUBTABLE.  Load the
   font and preprogram for the pixel size in INFO and its
   corresponding point size POINT_SIZE.

   The font tables in INFO must already have been initialized.

   Set INFO->interpreter, INFO->cvt, INFO->prep, INFO->fpgm and
   INFO->state upon success, and leave those fields intact
   otherwise.  */

static void
sfntfont_setup_interpreter (int fd, struct sfnt_font_info *info,
			    struct sfnt_offset_subtable *subtable,
			    int point_size)
{
  struct sfnt_cvt_table *cvt;
  struct sfnt_fpgm_table *fpgm;
  struct sfnt_prep_table *prep;
  struct sfnt_interpreter *interpreter;
  const char *error;
  struct sfnt_graphics_state state;

  /* Try to read the control value program, cvt, and font program
     tables.  */

  cvt = sfnt_read_cvt_table (fd, subtable);
  fpgm = sfnt_read_fpgm_table (fd, subtable);
  prep = sfnt_read_prep_table (fd, subtable);

  /* If both fpgm and prep are NULL, this font likely has no
     instructions, so don't bother setting up the interpreter.  */

  if (!fpgm && !prep)
    goto bail;

  /* Now, create the interpreter using the limits in info->maxp and
     info->head.  CVT can be NULL.  */
  interpreter = sfnt_make_interpreter (info->maxp, cvt, info->head,
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
  xfree (cvt);
  xfree (fpgm);
  xfree (prep);
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
  int fd, i;
  struct sfnt_offset_subtable *subtable;
  struct sfnt_cmap_encoding_subtable *subtables;
  struct sfnt_cmap_encoding_subtable_data **data;
  struct charset *charset;
  int point_size;
  Display_Info *dpyinfo;

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

  if (NILP (AREF (font_entity, FONT_EXTRA_INDEX)))
    return Qnil;

  desc = xmint_pointer (XCDR (XCAR (AREF (font_entity, FONT_EXTRA_INDEX))));

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

  font_info->outline_cache.next = &font_info->outline_cache;
  font_info->outline_cache.last = &font_info->outline_cache;
  font_info->outline_cache_size = 0;
  font_info->raster_cache.next = &font_info->raster_cache;
  font_info->raster_cache.last = &font_info->raster_cache;
  font_info->raster_cache_size = 0;
  font_info->interpreter = NULL;

  /* Open the font.  */
  fd = emacs_open (desc->path, O_RDONLY, 0);

  if (fd == -1)
    goto bail;

  /* Seek to the offset specified.  */

  if (desc->offset
      && lseek (fd, desc->offset, SEEK_SET) != desc->offset)
    goto bail;

  /* Read the offset subtable.  */
  subtable = sfnt_read_table_directory (fd);

  if (!subtable)
    goto bail1;

  /* Read required tables.  This font backend is supposed to be used
     mostly on devices with flash memory, so the order in which they
     are read is insignificant.  */

  /* Select a character map table.  */
  font_info->cmap = sfnt_read_cmap_table (fd, subtable, &subtables,
					  &data);
  if (!font_info->cmap)
    goto bail2;

  font_info->cmap_data
    = sfntfont_select_cmap (font_info->cmap,
			    subtables, data,
			    &font_info->cmap_subtable);

  for (i = 0; i < font_info->cmap->num_subtables; ++i)
    {
      if (data[i] != font_info->cmap_data)
	xfree (data[i]);
    }

  xfree (subtables);
  xfree (data);

  if (!font_info->cmap_data)
    goto bail3;

  /* Read the hhea, maxp, glyf, and head tables.  */
  font_info->hhea = sfnt_read_hhea_table (fd, subtable);
  font_info->maxp = sfnt_read_maxp_table (fd, subtable);
  font_info->glyf = sfnt_read_glyf_table (fd, subtable);
  font_info->head = sfnt_read_head_table (fd, subtable);

  /* If any of those tables couldn't be read, bail.  */
  if (!font_info->hhea || !font_info->maxp || !font_info->glyf
      || !font_info->head)
    goto bail4;

  /* Now figure out which kind of loca table must be read based on
     head->index_to_loc_format.  */
  font_info->loca_short = NULL;
  font_info->loca_long = NULL;

  if (font_info->head->index_to_loc_format)
    {
      font_info->loca_long
	= sfnt_read_loca_table_long (fd, subtable);

      if (!font_info->loca_long)
	goto bail4;
    }
  else
    {
      font_info->loca_short
	= sfnt_read_loca_table_short (fd, subtable);

      if (!font_info->loca_short)
	goto bail4;
    }

  /* Read the horizontal metrics table.  */
  font_info->hmtx = sfnt_read_hmtx_table (fd, subtable,
					  font_info->hhea,
					  font_info->maxp);
  if (!font_info->hmtx)
    goto bail5;

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
	    * pixel_size * 1.0 / font_info->head->units_per_em);
  font->descent
    = -floor (font_info->hhea->descent
	      * pixel_size * 1.0 / font_info->head->units_per_em);
  font->height = font->ascent + font->descent;

  /* Set generic attributes such as type and style.  */
  ASET (font_object, FONT_TYPE_INDEX, sfnt_vendor_name);

  if (!NILP (desc->designer))
    ASET (font_object, FONT_FOUNDRY_INDEX,
	  Fintern (desc->designer, Qnil));

  ASET (font_object, FONT_FAMILY_INDEX, Fintern (desc->family, Qnil));
  ASET (font_object, FONT_ADSTYLE_INDEX, Qnil);
  ASET (font_object, FONT_REGISTRY_INDEX,
	sfntfont_registry_for_desc (desc));

  /* Size of 0 means the font is scalable.  */
  ASET (font_object, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (font_object, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (font_object, FONT_SPACING_INDEX,
	make_fixnum (desc->spacing));

  FONT_SET_STYLE (font_object, FONT_WIDTH_INDEX,
		  make_fixnum (desc->width));
  FONT_SET_STYLE (font_object, FONT_WEIGHT_INDEX,
		  make_fixnum (desc->weight));
  FONT_SET_STYLE (font_object, FONT_SLANT_INDEX,
		  make_fixnum (desc->slant));

  ASET (font_object, FONT_ADSTYLE_INDEX, Qnil);

  /* Find out the minimum, maximum and average widths.  */
  sfntfont_probe_widths (font_info);

  /* Clear various offsets.  */
  font_info->font.baseline_offset = 0;
  font_info->font.relative_compose = 0;
  font_info->font.default_ascent = 0;
  font_info->font.vertical_centering = 0;
  font_info->font.underline_position = -1;
  font_info->font.underline_thickness = 0;

  /* Calculate the xfld name.  */
  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);

  /* Now try to set up grid fitting for this font.  */
  dpyinfo = FRAME_DISPLAY_INFO (f);
  point_size = PIXEL_TO_POINT (pixel_size, (dpyinfo->resx
					    * dpyinfo->resy
					    / 2));
  sfntfont_setup_interpreter (fd, font_info, subtable,
			      point_size);

  /* Close the font file descriptor.  */
  emacs_close (fd);

  /* Free the offset subtable.  */
  xfree (subtable);

  /* All done.  */
  unblock_input ();
  return font_object;

 bail6:
  xfree (font_info->hmtx);
  font_info->hmtx = NULL;
 bail5:
  xfree (font_info->loca_long);
  xfree (font_info->loca_short);
  font_info->loca_long = NULL;
  font_info->loca_short = NULL;
 bail4:
  xfree (font_info->hhea);
  xfree (font_info->maxp);
  xfree (font_info->glyf);
  xfree (font_info->head);
  font_info->hhea = NULL;
  font_info->maxp = NULL;
  font_info->glyf = NULL;
  font_info->head = NULL;

  /* This comes in bail4 due to a peculiarity of how the four tables
     above are validated.  */
  xfree (font_info->cmap_data);
  font_info->cmap_data = NULL;
 bail3:
  xfree (font_info->cmap);
  font_info->cmap = NULL;
 bail2:
  xfree (subtable);
 bail1:
  emacs_close (fd);
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
sfntfont_measure_instructed_pcm (struct sfnt_font_info *font, sfnt_glyph glyph,
				 struct font_metrics *pcm)
{
  struct sfnt_glyph_metrics metrics;
  struct sfnt_glyph_outline *outline;

  /* Ask for unscaled metrics.  */
  if (sfnt_lookup_glyph_metrics (glyph, -1, &metrics, font->hmtx,
				 font->hhea, font->head, font->maxp))
    return 1;

  /* Now get the glyph outline, which is required to obtain the rsb,
     ascent and descent.  */
  outline = sfntfont_get_glyph_outline (glyph, &font->outline_cache,
					font->font.pixel_size,
					&font->outline_cache_size,
					font->glyf, font->head,
					font->hmtx, font->hhea,
					font->maxp,
					font->loca_short,
					font->loca_long,
					font->interpreter, &metrics,
					&font->state);

  if (!outline)
    return 1;

  /* Scale the metrics by the interpreter's scale.  */
  sfnt_scale_metrics (&metrics, font->interpreter->scale);

  pcm->lbearing = metrics.lbearing >> 16;
  pcm->rbearing = SFNT_CEIL_FIXED (outline->xmax) >> 16;

  /* Round the advance, ascent and descent upwards.  */
  pcm->width = SFNT_CEIL_FIXED (metrics.advance) >> 16;
  pcm->ascent = SFNT_CEIL_FIXED (outline->ymax) >> 16;
  pcm->descent = SFNT_CEIL_FIXED (-outline->ymin) >> 16;

  sfntfont_dereference_outline (outline);
  return 0;
}

/* Measure the single glyph GLYPH in the font FONT and return its
   metrics in *PCM.  Value is 0 upon success, 1 otherwise.  */

static int
sfntfont_measure_pcm (struct sfnt_font_info *font, sfnt_glyph glyph,
		      struct font_metrics *pcm)
{
  struct sfnt_glyph_metrics metrics;
  struct sfnt_glyph_outline *outline;

  if (font->interpreter)
    /* Use a function which instructs the glyph.  */
    return sfntfont_measure_instructed_pcm (font, glyph, pcm);

  /* Get the glyph metrics first.  */
  if (sfnt_lookup_glyph_metrics (glyph, font->font.pixel_size,
				 &metrics, font->hmtx, font->hhea,
				 font->head, font->maxp))
    return 1;

  /* Now get the glyph outline, which is required to obtain the rsb,
     ascent and descent.  */
  outline = sfntfont_get_glyph_outline (glyph, &font->outline_cache,
					font->font.pixel_size,
					&font->outline_cache_size,
					font->glyf, font->head,
					font->hmtx, font->hhea,
					font->maxp,
					font->loca_short,
					font->loca_long, NULL, NULL,
					NULL);

  if (!outline)
    return 1;

  /* How to round lbearing and rbearing? */
  pcm->lbearing = metrics.lbearing >> 16;
  pcm->rbearing = SFNT_CEIL_FIXED (outline->xmax) >> 16;

  /* Round the advance, ascent and descent upwards.  */
  pcm->width = SFNT_CEIL_FIXED (metrics.advance) >> 16;
  pcm->ascent = SFNT_CEIL_FIXED (outline->ymax) >> 16;
  pcm->descent = SFNT_CEIL_FIXED (-outline->ymin) >> 16;

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

  info = (struct sfnt_font_info *) font;
  xfree (info->cmap);
  xfree (info->hhea);
  xfree (info->maxp);
  xfree (info->head);
  xfree (info->hmtx);
  xfree (info->glyf);
  xfree (info->loca_short);
  xfree (info->loca_long);
  xfree (info->cmap_data);
  xfree (info->prep);
  xfree (info->fpgm);
  xfree (info->cvt);
  xfree (info->interpreter);

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
  int pixel_size;

  length = to - from;
  font = s->font;
  info = (struct sfnt_font_info *) font;
  pixel_size = font->pixel_size;

  if (info->interpreter)
    pixel_size = -1;

  rasters = alloca (length * sizeof *rasters);
  x_coords = alloca (length * sizeof *x_coords);
  current_x = x;

  /* Get rasters and outlines for them.  */
  for (i = from; i < to; ++i)
    {
      /* Look up the metrics for this glyph.  The metrics are unscaled
	 if INFO->interpreter is set.  */
      if (sfnt_lookup_glyph_metrics (s->char2b[i], pixel_size,
				     &metrics, info->hmtx, info->hhea,
				     info->head, info->maxp))
	{
	  rasters[i - from] = NULL;
	  x_coords[i - from] = 0;
	  continue;
	}

      /* Look up the outline.  */
      outline = sfntfont_get_glyph_outline (s->char2b[i],
					    &info->outline_cache,
					    font->pixel_size,
					    &info->outline_cache_size,
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

      /* Scale the metrics if info->interpreter is set.  */
      if (info->interpreter)
	sfnt_scale_metrics (&metrics, info->interpreter->scale);

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
      current_x += SFNT_CEIL_FIXED (metrics.advance) >> 16;
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
  Lisp_Object families;
  struct sfnt_font_desc *desc;

  families = Qnil;

  for (desc = system_fonts; desc; desc = desc->next)
    /* Add desc->family to the list.  */
    families = Fcons (desc->family, families);

  /* Not sure if deleting duplicates is worth it.  Is this ever
     called? */
  return families;
}



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

  /* Char-table purpose.  */
  DEFSYM (Qfont_lookup_cache, "font-lookup-cache");

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
      mark_object (desc->languages);
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
