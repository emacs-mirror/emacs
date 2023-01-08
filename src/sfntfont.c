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

#include "lisp.h"
#include "sfnt.h"
#include "coding.h"

/* Generic font driver for sfnt-based fonts (currently TrueType, but
   it would be easy to add CFF support in the future.)

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

  /* Numeric width, weight, slant and spacing.  */
  int width, weight, slant, spacing;

  /* Path to the font file.  */
  char *path;
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

      /* FIXME will someone look at the MS spec and see if this
	 right.  */
      if (platform_specific_id
	  == SFNT_MICROSOFT_BIG_FIVE)
	system = Qchinese_big5;

      break;
    }

  if (NILP (system))
    return 1;

  setup_coding_system (system, coding);
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
sfnt_safe_encode_coding_object_1 (void)
{
  encode_coding_object (sfnt_font_coding,
			sfnt_font_src_object,
			sfnt_font_from,
			sfnt_font_from_byte,
			sfnt_font_to,
			sfnt_font_to_byte,
			sfnt_font_dst_object);
  return Qnil;
}

static Lisp_Object
sfnt_safe_encode_coding_object_2 (void)
{
  sfnt_font_signal = true;

  return Qnil;
}

/* Like encode_coding_object, but return 1 if a signal happens.  Value
   is otherwise 0.  */

static int
sfnt_safe_encode_coding_object (struct coding_system *coding,
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

  internal_condition_case (sfnt_safe_encode_coding_object_1,
			   Qt,
			   sfnt_safe_encode_coding_object_2);

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

  if (sfnt_safe_encode_coding_object (&coding, Qnil, 0, 0,
				      0, 0, length, length,
				      Qnil))
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

struct sfnt_style_desc
{
  /* The C string to match against.  */
  const char *c_string;

  /* The value of the style field.  */
  int value;
};

/* Array of style descriptions describing weight.  */
static struct sfnt_style_desc sfnt_weight_descriptions =
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
static struct sfnt_style_desc sfnt_slant_descriptions =
  {
    { "italic", 100,	},
    { "oblique", 110,	},
  };

/* Array of style descriptions describing width.  */
static struct sfnt_width_desc sfnt_width_descriptions =
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
   based on the style name passed as STYLE.  */

static void
sfnt_parse_style (Lisp_Object style, struct sfnt_font_desc *desc)
{
  char *style, single, *saveptr;
  int i;

  /* Fill in default values.  */
  desc->weight = 80;
  desc->slant = 0;
  desc->width = 100;

  /* Split the style into spaces.  As long as no weight, slant, or
     width is encountered, look in the corresponding descriptions
     array.  GC must not happen inside this block.  */
  style = SSDATA (Fdowncase (style));
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

      if (!desc->slant)
	{
	  /* Slant hasn't been found yet.  Scan through the slant
	     table.  */
	  for (i = 0; i < ARRAYELTS (sfnt_slant_descriptions); ++i)
	    {
	      if (!strcmp (sfnt_weight_descriptions[i].c_string,
			   single))
		{
		  /* Slant found.  Continue on reading the weight and
		     width.  */
		  desc->slant = sfnt_weight_descriptions[i].value;
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
		  desc->slant = sfnt_width_descriptions[i].value;
		  goto next;
		}
	    }
	}

    next:

      /* Break early if everything has been found.  */
      if (desc->slant && desc->width != 100 && desc->weight != 80)
	break;

      continue;
    }
}

/* Enumerate the font FILE into the list of system fonts.  Return 1 if
   it could not be enumerated, 0 otherwise.  */

int
sfnt_enum_font (const char *file)
{
  struct sfnt_font_desc *desc;
  int fd;
  struct sfnt_offset_subtable *subtables;
  struct sfnt_head_table *head;
  struct sfnt_name_table *name;
  struct sfnt_meta_table *meta;
  Lisp_Object family, style;

  /* Create the font desc and copy in the file name.  */
  desc = xzalloc (sizeof *desc + strlen (file) + 1);
  desc->path = (char *) (desc + 1);
  memcpy (desc->path, file, strlen (file) + 1);

  /* Now open the font for reading.  */
  fd = emacs_open (file, O_RDWR);

  /* Read the table directory.  */
  subtables = sfnt_read_table_directory (fd);

  if (!subtables)
    goto bail0;

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

  /* Parse the style.  */
  sfnt_parse_style (style, desc);

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
  xfree (subtables);
  emacs_close (fd);
  return 0;

 bail3:
  xfree (meta);
  xfree (name);
 bail2:
  xfree (head);
 bail1:
  xfree (subtables);
 bail0:
  emacs_close (fd);
  return 1;
}



void
syms_of_sfntfont (void)
{
  DEFSYM (Qutf_16be, "utf-16be");
  DEFSYM (Qmac_roman, "mac-roman");
}

void
mark_sfntfont (void)
{
  struct sfnt_font_desc *desc;

  /* Mark each font desc.  */
  for (desc = system_fonts; ++desc; desc = desc->next)
    {
      mark_object (desc->family);
      mark_object (desc->style);
    }
}
