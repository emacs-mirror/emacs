/* sfnt format font support for GNU Emacs.

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

#include "sfnt.h"

#include <assert.h>
#include <attribute.h>
#include <byteswap.h>
#include <fcntl.h>
#include <intprops.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined __GNUC__ && !defined __clang__
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#endif

#ifdef TEST

#include <time.h>
#include <timespec.h>

static void *
xmalloc (size_t size)
{
  return malloc (size);
}

static void *
xrealloc (void *ptr, size_t size)
{
  return realloc (ptr, size);
}

static void
xfree (void *ptr)
{
  return free (ptr);
}

/* Use this for functions that are static while building in test mode,
   but are used outside as well.  */
#define TEST_STATIC static

#else
#define TEST_STATIC
#include "lisp.h"
#endif

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

/* This file provides generic support for reading most TrueType fonts,
   and some OpenType fonts with TrueType outlines, along with glyph
   lookup, outline decomposition, and alpha mask generation from those
   glyphs.  It is intended to be used on any platform where proper
   libraries such as FreeType are not easily available, and the native
   font library is too limited for Emacs to support properly.

   Unlike most popular libraries for handling fonts, no ``font'' or
   ``face'' type is provided.  Instead, routines and structure
   definitions for accessing and making use of individual tables in a
   font file are exported, which allows for flexibility in the rest of
   Emacs.

   Try not to keep this file too dependent on Emacs.  Everything Lisp
   related goes in sfntfont.c.  The author wants to keep using it for
   some other (free) software.  */



/* Mapping between sfnt table names and their identifiers.  */

static uint32_t sfnt_table_names[] =
  {
    [SFNT_TABLE_CMAP] = 0x636d6170,
    [SFNT_TABLE_GLYF] = 0x676c7966,
    [SFNT_TABLE_HEAD] = 0x68656164,
    [SFNT_TABLE_HHEA] = 0x68686561,
    [SFNT_TABLE_HMTX] = 0x686d7478,
    [SFNT_TABLE_LOCA] = 0x6c6f6361,
    [SFNT_TABLE_MAXP] = 0x6d617870,
    [SFNT_TABLE_NAME] = 0x6e616d65,
    [SFNT_TABLE_META] = 0x6d657461,
  };

/* Swap values from TrueType to system byte order.  */

static void
_sfnt_swap16 (uint16_t *value)
{
#ifndef WORDS_BIGENDIAN
  *value = bswap_16 (*value);
#endif
}

static void
_sfnt_swap32 (uint32_t *value)
{
#ifndef WORDS_BIGENDIAN
  *value = bswap_32 (*value);
#endif
}

#define sfnt_swap16(what) (_sfnt_swap16 ((uint16_t *) (what)))
#define sfnt_swap32(what) (_sfnt_swap32 ((uint32_t *) (what)))

/* Read the table directory from the file FD.  FD must currently be at
   the start of the file, and must be seekable.  Return the table
   directory upon success, else NULL.  */

TEST_STATIC struct sfnt_offset_subtable *
sfnt_read_table_directory (int fd)
{
  struct sfnt_offset_subtable *subtable;
  ssize_t rc;
  size_t offset, subtable_size;
  int i;

  subtable = xmalloc (sizeof *subtable);
  offset = SFNT_ENDOF (struct sfnt_offset_subtable,
		       range_shift, uint16_t);
  rc = read (fd, subtable, offset);

  if (rc < offset)
    {
      xfree (subtable);
      return NULL;
    }

  sfnt_swap32 (&subtable->scaler_type);
  sfnt_swap16 (&subtable->num_tables);
  sfnt_swap16 (&subtable->search_range);
  sfnt_swap16 (&subtable->entry_selector);
  sfnt_swap16 (&subtable->range_shift);

  /* Figure out how many more tables have to be read, and read each
     one of them.  */
  subtable_size = (subtable->num_tables
		   * sizeof (struct sfnt_table_directory));
  subtable = xrealloc (subtable, sizeof *subtable + subtable_size);
  subtable->subtables
    = (struct sfnt_table_directory *) (subtable + 1);

  rc = read (fd, subtable->subtables, subtable_size);

  if (rc < offset)
    {
      xfree (subtable);
      return NULL;
    }

  /* Swap each of the subtables.  */

  for (i = 0; i < subtable->num_tables; ++i)
    {
      sfnt_swap32 (&subtable->subtables[i].tag);
      sfnt_swap32 (&subtable->subtables[i].checksum);
      sfnt_swap32 (&subtable->subtables[i].offset);
      sfnt_swap32 (&subtable->subtables[i].length);
    }

  return subtable;
}

/* Return a pointer to the table directory entry for TABLE in
   SUBTABLE, or NULL if it was not found.  */

static struct sfnt_table_directory *
sfnt_find_table (struct sfnt_offset_subtable *subtable,
		 enum sfnt_table table)
{
  int i;

  for (i = 0; i < subtable->num_tables; ++i)
    {
      if (subtable->subtables[i].tag == sfnt_table_names[table])
	return &subtable->subtables[i];
    }

  return NULL;
}



/* Character mapping routines.  */

/* Read a format 0 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_0 *
sfnt_read_cmap_format_0 (int fd,
			 struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_0 *format0;
  ssize_t rc;
  size_t wanted_size;

  format0 = xmalloc (sizeof *format0);

  /* Fill in fields that have already been read.  */
  format0->format = header->format;
  format0->length = header->length;

  /* Read the rest.  */
  wanted_size = (sizeof *format0
		 - offsetof (struct sfnt_cmap_format_0,
			     language));
  rc = read (fd, &format0->language, wanted_size);

  if (rc < wanted_size)
    {
      xfree (format0);
      return (struct sfnt_cmap_format_0 *) -1;
    }

  /* Swap fields and return.  */
  sfnt_swap16 (&format0->language);
  return format0;
}

/* Read a format 2 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_2 *
sfnt_read_cmap_format_2 (int fd,
			 struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_2 *format2;
  ssize_t rc;
  size_t min_bytes;
  int i, nsub;

  /* Reject contents that are too small.  */
  min_bytes = SFNT_ENDOF (struct sfnt_cmap_format_2,
			  sub_header_keys, uint16_t[256]);
  if (header->length < min_bytes)
    return NULL;

  /* Add enough bytes at the end to fit the two variable length
     pointers.  */
  format2 = xmalloc (header->length + sizeof *format2);
  format2->format = header->format;
  format2->length = header->length;

  /* Read the part before the variable length data.  */
  min_bytes -= offsetof (struct sfnt_cmap_format_2, language);
  rc = read (fd, &format2->language, min_bytes);
  if (rc < min_bytes)
    {
      xfree (format2);
      return (struct sfnt_cmap_format_2 *) -1;
    }

  /* Swap the fields now.  */

  sfnt_swap16 (&format2->language);

  /* At the same time, look for the largest value in sub_header_keys.
     That will be the number of subheaders and elements in the glyph
     index array.  */

  nsub = 0;

  for (i = 0; i < 256; ++i)
    {
      sfnt_swap16 (&format2->sub_header_keys[i]);

      if (format2->sub_header_keys[i] > nsub)
	nsub = format2->sub_header_keys[i];
    }

  if (!nsub)
    /* If there are no subheaders, then things are finished.  */
    return format2;

  /* Otherwise, read the rest of the variable length data to the end
     of format2.  */
  min_bytes = (format2->length
	       - SFNT_ENDOF (struct sfnt_cmap_format_2,
			     sub_header_keys, uint16_t[256]));
  rc = read (fd, format2 + 1, min_bytes);
  if (rc < min_bytes)
    {
      xfree (format2);
      return (struct sfnt_cmap_format_2 *) -1;
    }

  /* Check whether or not the data is of the correct size.  */
  if (min_bytes < nsub * sizeof *format2->subheaders)
    {
      xfree (format2);
      return (struct sfnt_cmap_format_2 *) -1;
    }

  /* Point the data pointers to the right location, swap everything,
     and return.  */

  format2->subheaders
    = (struct sfnt_cmap_format_2_subheader *) (format2 + 1);
  format2->glyph_index_array
    = (uint16_t *) (format2->subheaders + nsub);

  for (i = 0; i < nsub; ++i)
    {
      sfnt_swap16 (&format2->subheaders[i].first_code);
      sfnt_swap16 (&format2->subheaders[i].entry_count);
      sfnt_swap16 (&format2->subheaders[i].id_delta);
      sfnt_swap16 (&format2->subheaders[i].id_range_offset);
    }

  /* Figure out how big the glyph index array is, and swap everything
     there.  */
  format2->num_glyphs
    = (min_bytes - nsub * sizeof *format2->subheaders) / 2;

  for (i = 0; i < format2->num_glyphs; ++i)
    sfnt_swap16 (&format2->glyph_index_array[i]);

  return format2;
}

/* Read a format 4 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_4 *
sfnt_read_cmap_format_4 (int fd,
			 struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_4 *format4;
  size_t min_bytes, variable_size;
  ssize_t rc;
  size_t bytes_minus_format4;
  int seg_count, i;

  min_bytes = SFNT_ENDOF (struct sfnt_cmap_format_4,
			  entry_selector, uint16_t);

  /* Check that the length is at least min_bytes.  */
  if (header->length < min_bytes)
    return NULL;

  /* Allocate the format4 buffer, making it the size of the buffer
     itself plus that of the data.  */
  format4 = xmalloc (header->length + sizeof *format4);

  /* Copy over fields that have already been read.  */
  format4->format = header->format;
  format4->length = header->length;

  /* Read the initial data.  */
  min_bytes -= offsetof (struct sfnt_cmap_format_4, language);
  rc = read (fd, &format4->language, min_bytes);
  if (rc < min_bytes)
    {
      xfree (format4);
      return (struct sfnt_cmap_format_4 *) -1;
    }

  /* Swap fields that have been read.  */
  sfnt_swap16 (&format4->language);
  sfnt_swap16 (&format4->seg_count_x2);
  sfnt_swap16 (&format4->search_range);
  sfnt_swap16 (&format4->entry_selector);

  /* Get the number of segments to read.  */
  seg_count = format4->seg_count_x2 / 2;

  /* Now calculate whether or not the size is sufficiently large.  */
  bytes_minus_format4
    = format4->length - SFNT_ENDOF (struct sfnt_cmap_format_4,
				    entry_selector, uint16_t);
  variable_size = (seg_count * sizeof *format4->end_code
		   + sizeof *format4->reserved_pad
		   + seg_count * sizeof *format4->start_code
		   + seg_count * sizeof *format4->id_delta
		   + seg_count * sizeof *format4->id_range_offset);

  if (bytes_minus_format4 < variable_size)
    {
      /* Not enough bytes to fit the entire implied table
	 contents.  */
      xfree (format4);
      return NULL;
    }

  /* Read the rest of the bytes to the end of format4.  */
  rc = read (fd, format4 + 1, bytes_minus_format4);
  if (rc < bytes_minus_format4)
    {
      xfree (format4);
      return (struct sfnt_cmap_format_4 *) -1;
    }

  /* Set data pointers to the right locations.  */
  format4->end_code = (uint16_t *) (format4 + 1);
  format4->reserved_pad = format4->end_code + seg_count;
  format4->start_code = format4->reserved_pad + 1;
  format4->id_delta = (int16_t *) (format4->start_code + seg_count);
  format4->id_range_offset = format4->id_delta + seg_count;
  format4->glyph_index_array = (uint16_t *) (format4->id_range_offset
					     + seg_count);

  /* N.B. that the number of elements in glyph_index_array is
     (bytes_minus_format4 - variable_size) / 2.  Swap all the
     data.  */

  sfnt_swap16 (format4->reserved_pad);

  for (i = 0; i < seg_count; ++i)
    {
      sfnt_swap16 (&format4->end_code[i]);
      sfnt_swap16 (&format4->start_code[i]);
      sfnt_swap16 (&format4->id_delta[i]);
      sfnt_swap16 (&format4->id_range_offset[i]);
    }

  format4->glyph_index_size
    = (bytes_minus_format4 - variable_size) / 2;

  for (i = 0; i < format4->glyph_index_size; ++i)
    sfnt_swap16 (&format4->glyph_index_array[i]);

  /* Done.  Return the format 4 character map.  */
  return format4;
}

/* Read a format 6 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_6 *
sfnt_read_cmap_format_6 (int fd,
			 struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_6 *format6;
  size_t min_size;
  ssize_t rc;
  uint16_t i;

  min_size = SFNT_ENDOF (struct sfnt_cmap_format_6, entry_count,
			 uint16_t);

  /* See if header->length is big enough.  */
  if (header->length < min_size)
    return NULL;

  /* Allocate the buffer to hold header->size and enough for at least
     the glyph index array pointer.  */
  format6 = xmalloc (header->length + sizeof *format6);

  /* Fill in data that has already been read.  */
  format6->format = header->format;
  format6->length = header->length;

  /* Read the fixed size data.  */
  min_size -= offsetof (struct sfnt_cmap_format_6, language);
  rc = read (fd, &format6->language, min_size);
  if (rc < min_size)
    {
      xfree (format6);
      return (struct sfnt_cmap_format_6 *) -1;
    }

  /* Swap what was read.  */
  sfnt_swap16 (&format6->language);
  sfnt_swap16 (&format6->first_code);
  sfnt_swap16 (&format6->entry_count);

  /* Figure out whether or not header->length is sufficient to hold
     the variable length data.  */
  if (header->length
      < format6->entry_count * sizeof *format6->glyph_index_array)
    {
      xfree (format6);
      return NULL;
    }

  /* Read the variable length data.  */
  rc = read (fd, format6 + 1,
	     (format6->entry_count
	      * sizeof *format6->glyph_index_array));
  if (rc < format6->entry_count * sizeof *format6->glyph_index_array)
    {
      xfree (format6);
      return (struct sfnt_cmap_format_6 *) -1;
    }

  /* Set the data pointer and swap everything.  */
  format6->glyph_index_array = (uint16_t *) (format6 + 1);
  for (i = 0; i < format6->entry_count; ++i)
    sfnt_swap16 (&format6->glyph_index_array[i]);

  /* All done! */
  return format6;
}

/* Read a format 8 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_8 *
sfnt_read_cmap_format_8 (int fd,
			 struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_8 *format8;
  size_t min_size, temp;
  ssize_t rc;
  uint32_t length, i;

  /* Read the 32-bit lenth field.  */
  if (read (fd, &length, sizeof (length)) < sizeof (length))
    return (struct sfnt_cmap_format_8 *) -1;

  /* Swap the 32-bit length field.  */
  sfnt_swap32 (&length);

  min_size = SFNT_ENDOF (struct sfnt_cmap_format_8, num_groups,
			 uint32_t);

  /* Make sure the header is at least as large as min_size.  */
  if (length < min_size)
    return NULL;

  /* Allocate a buffer of sufficient size.  */
  format8 = xmalloc (length + sizeof *format8);
  format8->format = header->format;
  format8->reserved = header->length;
  format8->length = length;

  /* Read the fixed length data.  */
  min_size -= offsetof (struct sfnt_cmap_format_8, language);
  rc = read (fd, &format8->language, min_size);
  if (rc < min_size)
    {
      xfree (format8);
      return (struct sfnt_cmap_format_8 *) -1;
    }

  /* Swap what was read.  */
  sfnt_swap32 (&format8->language);
  sfnt_swap32 (&format8->num_groups);

  /* See if the size is sufficient to read the variable length
     data.  */
  min_size = SFNT_ENDOF (struct sfnt_cmap_format_8, num_groups,
			 uint32_t);

  if (INT_MULTIPLY_WRAPV (format8->num_groups, sizeof *format8->groups,
			  &temp))
    {
      xfree (format8);
      return NULL;
    }

  if (INT_ADD_WRAPV (min_size, temp, &min_size))
    {
      xfree (format8);
      return NULL;
    }

  if (length < min_size)
    {
      xfree (format8);
      return NULL;
    }

  /* Now read the variable length data.  */
  rc = read (fd, format8 + 1, temp);
  if (rc < temp)
    {
      xfree (format8);
      return (struct sfnt_cmap_format_8 *) -1;
    }

  /* Set the pointer to the variable length data.  */
  format8->groups
    = (struct sfnt_cmap_format_8_or_12_group *) (format8 + 1);

  for (i = 0; i < format8->num_groups; ++i)
    {
      sfnt_swap32 (&format8->groups[i].start_char_code);
      sfnt_swap32 (&format8->groups[i].end_char_code);
      sfnt_swap32 (&format8->groups[i].start_glyph_code);
    }

  /* All done.  */
  return format8;
}

/* Read a format 12 cmap subtable from FD.  HEADER has already been
   read.  */

static struct sfnt_cmap_format_12 *
sfnt_read_cmap_format_12 (int fd,
			  struct sfnt_cmap_encoding_subtable_data *header)
{
  struct sfnt_cmap_format_12 *format12;
  size_t min_size, temp;
  ssize_t rc;
  uint32_t length, i;

  /* Read the 32-bit lenth field.  */
  if (read (fd, &length, sizeof (length)) < sizeof (length))
    return (struct sfnt_cmap_format_12 *) -1;

  /* Swap the 32-bit length field.  */
  sfnt_swap32 (&length);

  min_size = SFNT_ENDOF (struct sfnt_cmap_format_12, num_groups,
			 uint32_t);

  /* Make sure the header is at least as large as min_size.  */
  if (length < min_size)
    return NULL;

  /* Allocate a buffer of sufficient size.  */
  format12 = xmalloc (length + sizeof *format12);
  format12->format = header->format;
  format12->reserved = header->length;
  format12->length = length;

  /* Read the fixed length data.  */
  min_size -= offsetof (struct sfnt_cmap_format_12, language);
  rc = read (fd, &format12->language, min_size);
  if (rc < min_size)
    {
      xfree (format12);
      return (struct sfnt_cmap_format_12 *) -1;
    }

  /* Swap what was read.  */
  sfnt_swap32 (&format12->language);
  sfnt_swap32 (&format12->num_groups);

  /* See if the size is sufficient to read the variable length
     data.  */
  min_size = SFNT_ENDOF (struct sfnt_cmap_format_12, num_groups,
			 uint32_t);

  if (INT_MULTIPLY_WRAPV (format12->num_groups, sizeof *format12->groups,
			  &temp))
    {
      xfree (format12);
      return NULL;
    }

  if (INT_ADD_WRAPV (min_size, temp, &min_size))
    {
      xfree (format12);
      return NULL;
    }

  if (length < min_size)
    {
      xfree (format12);
      return NULL;
    }

  /* Now read the variable length data.  */
  rc = read (fd, format12 + 1, temp);
  if (rc < temp)
    {
      xfree (format12);
      return (struct sfnt_cmap_format_12 *) -1;
    }

  /* Set the pointer to the variable length data.  */
  format12->groups
    = (struct sfnt_cmap_format_8_or_12_group *) (format12 + 1);

  for (i = 0; i < format12->num_groups; ++i)
    {
      sfnt_swap32 (&format12->groups[i].start_char_code);
      sfnt_swap32 (&format12->groups[i].end_char_code);
      sfnt_swap32 (&format12->groups[i].start_glyph_code);
    }

  /* All done.  */
  return format12;
}

/* Read the CMAP subtable data from a given file FD at TABLE_OFFSET
   bytes from DIRECTORY_OFFSET.  Return the subtable data if it is
   supported.  Else, value is NULL if the format is unsupported, or -1
   upon an IO error.  */

static struct sfnt_cmap_encoding_subtable_data *
sfnt_read_cmap_table_1 (int fd, uint32_t directory_offset,
			uint32_t table_offset)
{
  off_t offset;
  struct sfnt_cmap_encoding_subtable_data header;

  if (INT_ADD_WRAPV (directory_offset, table_offset, &offset))
    return (struct sfnt_cmap_encoding_subtable_data *) -1;

  if (lseek (fd, offset, SEEK_SET) == (off_t) -1)
    return (struct sfnt_cmap_encoding_subtable_data *) -1;

  if (read (fd, &header, sizeof header) < sizeof header)
    return (struct sfnt_cmap_encoding_subtable_data *) -1;

  sfnt_swap16 (&header.format);
  sfnt_swap16 (&header.length);

  switch (header.format)
    {
    case 0:
      /* If the length changes, then something has changed to the
	 format.  */
      if (header.length != 262)
	return NULL;

      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_0 (fd, &header));

    case 2:
      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_2 (fd, &header));

    case 4:
      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_4 (fd, &header));

    case 6:
      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_6 (fd, &header));

    case 8:
      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_8 (fd, &header));

    case 12:
      return ((struct sfnt_cmap_encoding_subtable_data *)
	      sfnt_read_cmap_format_12 (fd, &header));

    default:
      return NULL;
    }
}

/* Read the CMAP table of a given font from the file FD.  Use the
   table directory specified in SUBTABLE.

   Return the CMAP table and a list of encoding subtables in
   *SUBTABLES and *DATA upon success, else NULL.  */

TEST_STATIC struct sfnt_cmap_table *
sfnt_read_cmap_table (int fd, struct sfnt_offset_subtable *subtable,
		      struct sfnt_cmap_encoding_subtable **subtables,
		      struct sfnt_cmap_encoding_subtable_data ***data)
{
  struct sfnt_table_directory *directory;
  struct sfnt_cmap_table *cmap;
  ssize_t rc;
  int i, j;

  /* Find the CMAP table in the table directory.  */
  directory = sfnt_find_table (subtable, SFNT_TABLE_CMAP);

  if (!directory)
    return NULL;

  /* Seek to the start of the CMAP table.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Read the table header.  */
  cmap = xmalloc (sizeof *cmap);
  rc = read (fd, cmap, sizeof *cmap);

  if (rc < sizeof *cmap)
    {
      xfree (cmap);
      return NULL;
    }

  /* Swap the header data.  */
  sfnt_swap16 (&cmap->version);
  sfnt_swap16 (&cmap->num_subtables);

  if (cmap->version != 0)
    {
      xfree (cmap);
      return NULL;
    }

  *subtables = xmalloc (cmap->num_subtables
			* sizeof **subtables);


  /* First, read the common parts of each encoding subtable.  */

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      /* Read the common part of the new subtable.  */
      rc = read (fd, &(*subtables)[i], sizeof (*subtables)[i]);

      if (rc < sizeof (*subtables))
	{
	  xfree (cmap);
	  xfree (*subtables);
	  return NULL;
	}

      sfnt_swap16 (&(*subtables)[i].platform_id);
      sfnt_swap16 (&(*subtables)[i].platform_specific_id);
      sfnt_swap32 (&(*subtables)[i].offset);
    }

  /* Second, read each encoding subtable itself.  */
  *data = xmalloc (cmap->num_subtables
		   * sizeof **subtables);

  for (i = 0; i < cmap->num_subtables; ++i)
    {
      (*data)[i] = sfnt_read_cmap_table_1 (fd, directory->offset,
					   (*subtables)[i].offset);

      if ((*data)[i] == (void *) -1)
	{
	  /* An IO error occurred (as opposed to the subtable format
	     being unsupported.)  Return now.  */

	  for (j = 0; j < i; ++j)
	    xfree (data[i]);
	  xfree (*data);
	  xfree (*subtables);
	  xfree (cmap);
	  return NULL;
	}
    }

  return cmap;
}

/* Look up the glyph corresponding to CHARACTER in the format 0 cmap
   FORMAT0.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_0 (sfnt_char character,
		     struct sfnt_cmap_format_0 *format0)
{
  if (character >= 256)
    return 0;

  return format0->glyph_index_array[character];
}

/* Look up the glyph corresponding to CHARACTER in the format 2 cmap
   FORMAT2.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_2 (sfnt_char character,
		     struct sfnt_cmap_format_2 *format2)
{
  unsigned char i, k, j;
  struct sfnt_cmap_format_2_subheader *subheader;
  unsigned char *slice;
  uint16_t glyph;

  if (character > 65335)
    return 0;

  i = character >> 16;
  j = character & 0xff;
  k = format2->sub_header_keys[i] / 8;

  if (k)
    {
      subheader = &format2->subheaders[k];

      if (subheader->first_code <= j
	  && j <= ((int) subheader->first_code
		   + (int) subheader->entry_count))
	{
	  /* id_range_offset is actually the number of bytes past
	     itself containing the uint16_t ``slice''.  It is possibly
	     unaligned.  */
	  slice = (unsigned char *) &subheader->id_range_offset;
	  slice += subheader->id_range_offset;
	  slice += (j - subheader->first_code) * sizeof (uint16_t);

	  if (slice < (unsigned char *) format2->glyph_index_array
	      || (slice + 1
		  > (unsigned char *) (format2->glyph_index_array
				       + format2->num_glyphs)))
	    /* The character is out of bounds.  */
	    return 0;

	  memcpy (&glyph, slice, sizeof glyph);
	  return (glyph + subheader->id_delta) % 65536;
	}
      else
	return 0;
    }

  /* k is 0, so glyph_index_array[i] is the glyph.  */
  return (i < format2->num_glyphs
	  ? format2->glyph_index_array[i]
	  : 0);
}

/* Like `bsearch'.  However, return the highest element above KEY if
   it could not be found.  */

static void *
sfnt_bsearch_above (const void *key, const void *base,
		    size_t nmemb, size_t size,
		    int (*compar) (const void *,
				   const void *))
{
  const unsigned char *bytes, *sample;
  size_t low, high, mid;

  bytes = base;
  low = 0;
  high = nmemb - 1;

  if (!nmemb)
    return NULL;

  while (low != high)
    {
      mid = low + (high - low) / 2;
      sample = bytes + mid * size;

      if (compar (key, sample) > 0)
	low = mid + 1;
      else
	high = mid;
    }

  return (unsigned char *) bytes + low * size;
}

/* Compare two uint16_t's.  Used to bisect through a format 4
   table.  */

static int
sfnt_compare_uint16 (const void *a, const void *b)
{
  return ((int) *((uint16_t *) a)) - ((int) *((uint16_t *) b));
}

/* Look up the glyph corresponding to CODE in the format 4 cmap
   FORMAT4, using the table segment SEGMENT.  Value is 0 if no glyph
   was found.  */

static sfnt_glyph
sfnt_lookup_glyph_4_1 (uint16_t code, uint16_t segment,
		       struct sfnt_cmap_format_4 *format4)
{
  uint16_t *index;

  if (format4->id_range_offset[segment])
    {
      /* id_range_offset is not 0, so the glyph mapping depends on
	 it.  */
      index = (uint16_t *) (&format4->id_range_offset[segment]
			    + format4->id_range_offset[segment] / 2
			    + (code - format4->start_code[segment]));

      /* Check that index is not out of bounds.  */
      if (index >= (format4->glyph_index_array
		    + format4->glyph_index_size)
	  || index < format4->glyph_index_array)
	return 0;

      /* Return what is in index.  */
      return (*index ? (format4->id_delta[segment]
			+ *index) % 65536 : 0);
    }

  /* Otherwise, just add id_delta.  */
  return (format4->id_delta[segment] + code) % 65536;
}

/* Look up the glyph corresponding to CHARACTER in the format 4 cmap
   FORMAT4.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_4 (sfnt_char character,
		     struct sfnt_cmap_format_4 *format4)
{
  uint16_t *segment_address;
  uint16_t code, segment;
  sfnt_glyph glyph;

  if (character > 65535)
    return 0;

  code = character;

  /* Find the segment ending above or at CHARACTER.  */
  segment_address = sfnt_bsearch_above (&code, format4->end_code,
					format4->seg_count_x2 / 2,
					sizeof code,
					sfnt_compare_uint16);
  segment = segment_address - format4->end_code;

  /* If the segment starts too late, return 0.  */
  if (!segment_address || format4->start_code[segment] > character)
    return 0;

  glyph = sfnt_lookup_glyph_4_1 (character, segment, format4);

  if (glyph)
    return glyph;

  /* Droid Sans Mono has overlapping segments in its format 4 cmap
     subtable where the first segment's end code is 32, while the
     second segment's start code is also 32.  The TrueType Reference
     Manual says that mapping should begin by searching for the first
     segment whose end code is greater than or equal to the character
     being indexed, but that results in the first subtable being
     found, which doesn't work, while the second table does.  Try to
     detect this situation and use the second table if possible.  */

  if (!glyph
      /* The character being looked up is the current segment's end
	 code.  */
      && code == format4->end_code[segment]
      /* There is an additional segment.  */
      && segment + 1 < format4->seg_count_x2 / 2
      /* That segment's start code is the same as this segment's end
	 code.  */
      && format4->start_code[segment + 1] == format4->end_code[segment])
    /* Try the second segment.  */
    return sfnt_lookup_glyph_4_1 (character, segment + 1, format4);

  /* Fail.  */
  return 0;
}

/* Look up the glyph corresponding to CHARACTER in the format 6 cmap
   FORMAT6.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_6 (sfnt_char character,
		     struct sfnt_cmap_format_6 *format6)
{
  if (character < format6->first_code
      || character >= (format6->first_code
		       + (int) format6->entry_count))
    return 0;

  return format6->glyph_index_array[character - format6->first_code];
}

/* Look up the glyph corresponding to CHARACTER in the format 8 cmap
   FORMAT8.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_8 (sfnt_char character,
		     struct sfnt_cmap_format_8 *format8)
{
  uint32_t i;

  if (character > 0xffffffff)
    return 0;

  for (i = 0; i < format8->num_groups; ++i)
    {
      if (format8->groups[i].start_char_code <= character
	  && format8->groups[i].end_char_code >= character)
	return (format8->groups[i].start_glyph_code
		+ (character
		   - format8->groups[i].start_char_code));
    }

  return 0;
}

/* Look up the glyph corresponding to CHARACTER in the format 12 cmap
   FORMAT12.  Return 0 if no glyph was found.  */

static sfnt_glyph
sfnt_lookup_glyph_12 (sfnt_char character,
		      struct sfnt_cmap_format_12 *format12)
{
  uint32_t i;

  if (character > 0xffffffff)
    return 0;

  for (i = 0; i < format12->num_groups; ++i)
    {
      if (format12->groups[i].start_char_code <= character
	  && format12->groups[i].end_char_code >= character)
	return (format12->groups[i].start_glyph_code
		+ (character
		   - format12->groups[i].start_char_code));
    }

  return 0;
}

/* Look up the glyph index corresponding to the character CHARACTER,
   which must be in the correct encoding for the cmap table pointed to
   by DATA.  */

TEST_STATIC sfnt_glyph
sfnt_lookup_glyph (sfnt_char character,
		   struct sfnt_cmap_encoding_subtable_data *data)
{
  switch (data->format)
    {
    case 0:
      return sfnt_lookup_glyph_0 (character,
				  (struct sfnt_cmap_format_0 *) data);

    case 2:
      return sfnt_lookup_glyph_2 (character,
				  (struct sfnt_cmap_format_2 *) data);

    case 4:
      return sfnt_lookup_glyph_4 (character,
				  (struct sfnt_cmap_format_4 *) data);

    case 6:
      return sfnt_lookup_glyph_6 (character,
				  (struct sfnt_cmap_format_6 *) data);

    case 8:
      return sfnt_lookup_glyph_8 (character,
				  (struct sfnt_cmap_format_8 *) data);

    case 12:
      return sfnt_lookup_glyph_12 (character,
				   (struct sfnt_cmap_format_12 *) data);
    }

  return 0;
}



/* Header reading routines.  */

/* Read the head table of a given font FD.  Use the table directory
   specified in SUBTABLE.

   Return the head table upon success, else NULL.  */

TEST_STATIC struct sfnt_head_table *
sfnt_read_head_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_head_table *head;
  ssize_t rc;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_HEAD);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Read the entire table.  */
  head = xmalloc (sizeof *head);
  rc = read (fd, head, sizeof *head);

  if (rc < sizeof *head)
    {
      xfree (head);
      return NULL;
    }

  /* Swap the header data.  */
  sfnt_swap32 (&head->version);
  sfnt_swap32 (&head->revision);

  if (head->version != 0x00010000)
    {
      xfree (head);
      return NULL;
    }

  /* Swap the rest of the data.  */
  sfnt_swap32 (&head->checksum_adjustment);
  sfnt_swap32 (&head->magic);

  if (head->magic != 0x5f0f3cf5)
    {
      xfree (head);
      return NULL;
    }

  sfnt_swap16 (&head->flags);
  sfnt_swap16 (&head->units_per_em);
  sfnt_swap32 (&head->created_high);
  sfnt_swap32 (&head->created_low);
  sfnt_swap32 (&head->modified_high);
  sfnt_swap32 (&head->modified_low);
  sfnt_swap16 (&head->xmin);
  sfnt_swap16 (&head->xmax);
  sfnt_swap16 (&head->ymin);
  sfnt_swap16 (&head->ymax);
  sfnt_swap16 (&head->mac_style);
  sfnt_swap16 (&head->lowest_rec_ppem);
  sfnt_swap16 (&head->font_direction_hint);
  sfnt_swap16 (&head->index_to_loc_format);
  sfnt_swap16 (&head->glyph_data_format);

  return head;
}

/* Read the hhea table of a given font FD.  Use the table directory
   specified in SUBTABLE.

   Return the head table upon success, else NULL.  */

TEST_STATIC struct sfnt_hhea_table *
sfnt_read_hhea_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_hhea_table *hhea;
  ssize_t rc;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_HHEA);

  if (!directory)
    return NULL;

  /* Check the length is right.  */
  if (directory->length != sizeof *hhea)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Read the entire table.  */
  hhea = xmalloc (sizeof *hhea);
  rc = read (fd, hhea, sizeof *hhea);

  if (rc < sizeof *hhea)
    {
      xfree (hhea);
      return NULL;
    }

  /* Swap the header data.  */
  sfnt_swap32 (&hhea->version);

  if (hhea->version != 0x00010000)
    {
      xfree (hhea);
      return NULL;
    }

  /* Swap the rest of the data.  */
  sfnt_swap16 (&hhea->ascent);
  sfnt_swap16 (&hhea->descent);
  sfnt_swap16 (&hhea->line_gap);
  sfnt_swap16 (&hhea->advance_width_max);
  sfnt_swap16 (&hhea->min_left_side_bearing);
  sfnt_swap16 (&hhea->min_right_side_bearing);
  sfnt_swap16 (&hhea->x_max_extent);
  sfnt_swap16 (&hhea->caret_slope_rise);
  sfnt_swap16 (&hhea->caret_slope_run);
  sfnt_swap16 (&hhea->reserved1);
  sfnt_swap16 (&hhea->reserved2);
  sfnt_swap16 (&hhea->reserved3);
  sfnt_swap16 (&hhea->reserved4);
  sfnt_swap16 (&hhea->metric_data_format);
  sfnt_swap16 (&hhea->num_of_long_hor_metrics);

  return hhea;
}

/* Read a short loca table from the given font FD.  Use the table
   directory specified in SUBTABLE.

   Return the short table upon success, else NULL.  */

TEST_STATIC struct sfnt_loca_table_short *
sfnt_read_loca_table_short (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_loca_table_short *loca;
  ssize_t rc;
  int i;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_LOCA);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out how many glyphs there are based on the length.  */
  loca = xmalloc (sizeof *loca + directory->length);
  loca->offsets = (uint16_t *) (loca + 1);
  loca->num_offsets = directory->length / 2;

  /* Read the variable-length table data.  */
  rc = read (fd, loca->offsets, directory->length);
  if (rc < directory->length)
    {
      xfree (loca);
      return NULL;
    }

  /* Swap each of the offsets.  */
  for (i = 0; i < loca->num_offsets; ++i)
    sfnt_swap16 (&loca->offsets[i]);

  /* Return the table.  */
  return loca;
}

/* Read a long loca table from the given font FD.  Use the table
   directory specified in SUBTABLE.

   Return the long table upon success, else NULL.  */

TEST_STATIC struct sfnt_loca_table_long *
sfnt_read_loca_table_long (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_loca_table_long *loca;
  ssize_t rc;
  int i;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_LOCA);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out how many glyphs there are based on the length.  */
  loca = xmalloc (sizeof *loca + directory->length);
  loca->offsets = (uint32_t *) (loca + 1);
  loca->num_offsets = directory->length / 4;

  /* Read the variable-length table data.  */
  rc = read (fd, loca->offsets, directory->length);
  if (rc < directory->length)
    {
      xfree (loca);
      return NULL;
    }

  /* Swap each of the offsets.  */
  for (i = 0; i < loca->num_offsets; ++i)
    sfnt_swap32 (&loca->offsets[i]);

  /* Return the table.  */
  return loca;
}

/* Read the maxp table from the given font FD.  Use the table
   directory specified in SUBTABLE.

   Return the maxp table upon success, else NULL.  If the version is
   0.5, fields past num_glyphs will not be populated.  */

TEST_STATIC struct sfnt_maxp_table *
sfnt_read_maxp_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_maxp_table *maxp;
  size_t size;
  ssize_t rc;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_MAXP);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* If directory->length is not big enough for version 0.5, punt.  */
  if (directory->length < SFNT_ENDOF (struct sfnt_maxp_table,
				      num_glyphs, uint16_t))
    return NULL;

  /* Allocate the buffer to hold the data.  Then, read
     directory->length or sizeof *maxp bytes into it, whichever is
     smaller.  */

  maxp = malloc (sizeof *maxp);
  size = MIN (directory->length, sizeof *maxp);
  rc = read (fd, maxp, size);

  if (rc < size)
    {
      xfree (maxp);
      return NULL;
    }

  /* Now, swap version and num_glyphs.  */
  sfnt_swap32 (&maxp->version);
  sfnt_swap16 (&maxp->num_glyphs);

  /* Reject version 1.0 tables that are too small.  */
  if (maxp->version > 0x00005000 && size < sizeof *maxp)
    {
      xfree (maxp);
      return NULL;
    }

  /* If the table is version 0.5, then this function is done.  */
  if (maxp->version == 0x00005000)
    return maxp;
  else if (maxp->version != 0x00010000)
    {
      /* Reject invalid versions.  */
      xfree (maxp);
      return NULL;
    }

  /* Otherwise, swap the rest of the fields.  */
  sfnt_swap16 (&maxp->max_points);
  sfnt_swap16 (&maxp->max_contours);
  sfnt_swap16 (&maxp->max_composite_points);
  sfnt_swap16 (&maxp->max_composite_contours);
  sfnt_swap16 (&maxp->max_zones);
  sfnt_swap16 (&maxp->max_twilight_points);
  sfnt_swap16 (&maxp->max_storage);
  sfnt_swap16 (&maxp->max_function_defs);
  sfnt_swap16 (&maxp->max_instruction_defs);
  sfnt_swap16 (&maxp->max_stack_elements);
  sfnt_swap16 (&maxp->max_size_of_instructions);
  sfnt_swap16 (&maxp->max_component_elements);
  sfnt_swap16 (&maxp->max_component_depth);

  /* All done.  */
  return maxp;
}



/* Glyph outlining generation.  */

/* Read a glyf table from the given font FD.  Use the table directory
   specified in SUBTABLE.  The glyph data is not swapped.

   Return the glyf table upon success, else NULL.  */

TEST_STATIC struct sfnt_glyf_table *
sfnt_read_glyf_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_glyf_table *glyf;
  ssize_t rc;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_GLYF);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Allocate enough to hold everything.  */
  glyf = xmalloc (sizeof *glyf + directory->length);
  glyf->size = directory->length;
  glyf->glyphs = (unsigned char *) (glyf + 1);

  /* Read the glyph data.  */
  rc = read (fd, glyf->glyphs, glyf->size);
  if (rc < glyf->size)
    {
      xfree (glyf);
      return NULL;
    }

  /* Return the table.  */
  return glyf;
}

/* Read the simple glyph outline from the glyph GLYPH from the
   specified glyf table at the given offset.  Set GLYPH->simple to a
   non-NULL value upon success, else set it to NULL.  */

static void
sfnt_read_simple_glyph (struct sfnt_glyph *glyph,
			struct sfnt_glyf_table *glyf,
			ptrdiff_t offset)
{
  struct sfnt_simple_glyph *simple;
  ssize_t min_size, min_size_2;
  int i, number_of_points, repeat_count;
  unsigned char *instructions_start;
  unsigned char *flags_start, *flags_end;
  unsigned char *vec_start;
  int16_t delta, x, y;

  /* Calculate the minimum size of the glyph data.  This is the size
     of the instruction length field followed by
     glyph->number_of_contours * sizeof (uint16_t).  */

  min_size = (glyph->number_of_contours * sizeof (uint16_t)
	      + sizeof (uint16_t));

  /* Check that the size is big enough.  */
  if (glyf->size < offset + min_size)
    {
      glyph->simple = NULL;
      return;
    }

  /* Allocate enough to read at least that.  */
  simple = xmalloc (sizeof *simple + min_size);
  simple->end_pts_of_contours = (uint16_t *) (simple + 1);
  memcpy (simple->end_pts_of_contours, glyf->glyphs + offset,
	  min_size);

  /* This is not really an index into simple->end_pts_of_contours.
     Rather, it is reading the first word past it.  */
  simple->instruction_length
    = simple->end_pts_of_contours[glyph->number_of_contours];

  /* Swap the contour end point indices and the instruction
     length.  */

  for (i = 0; i < glyph->number_of_contours; ++i)
    sfnt_swap16 (&simple->end_pts_of_contours[i]);

  sfnt_swap16 (&simple->instruction_length);

  /* Based on those values, calculate the maximum size of the
     following data.  This is the instruction length + the last
     contour point + the last contour point * uint16_t * 2.  */

  if (glyph->number_of_contours)
    number_of_points
      = simple->end_pts_of_contours[glyph->number_of_contours - 1] + 1;
  else
    number_of_points = 0;

  min_size_2 = (simple->instruction_length
		+ number_of_points
		+ (number_of_points
		   * sizeof (uint16_t) * 2));

  /* Set simple->number_of_points.  */
  simple->number_of_points = number_of_points;

  /* Make simple big enough.  */
  simple = xrealloc (simple, sizeof *simple + min_size + min_size_2);
  simple->end_pts_of_contours = (uint16_t *) (simple + 1);

  /* Set the instruction data pointer and other pointers.
     simple->instructions comes one word past number_of_contours,
     because end_pts_of_contours also contains the instruction
     length.  */
  simple->instructions = (uint8_t *) (simple->end_pts_of_contours
				      + glyph->number_of_contours + 1);
  simple->flags = simple->instructions + simple->instruction_length;

  /* Read instructions into the glyph.  */
  instructions_start = glyf->glyphs + offset + min_size;

  if (instructions_start >= glyf->glyphs + glyf->size
      || (instructions_start + simple->instruction_length
	  >= glyf->glyphs + glyf->size))
    {
      glyph->simple = NULL;
      xfree (simple);
      return;
    }

  memcpy (simple->instructions, instructions_start,
	  simple->instruction_length);

  /* Start reading flags.  */
  flags_start = (glyf->glyphs + offset
		 + min_size + simple->instruction_length);
  flags_end = flags_start + number_of_points;

  if (flags_start >= glyf->glyphs + glyf->size)
    {
      glyph->simple = NULL;
      xfree (simple);
      return;
    }

  i = 0;

  while (flags_start < flags_end)
    {
      if (i == number_of_points)
	break;

      if (flags_start >= glyf->glyphs + glyf->size)
	break;

      simple->flags[i++] = *flags_start;

      if (*flags_start & 010) /* REPEAT_FLAG */
	{
	  /* The next byte specifies how many times this byte is to be
	     repeated.  Check that it is in range.  */

	  if (flags_start + 1 >= glyf->glyphs + glyf->size)
	    {
	      glyph->simple = NULL;
	      xfree (simple);
	    }

	  /* Repeat the current flag until
	     glyph->number_of_points.  */

	  repeat_count = *(flags_start + 1);

	  while (i < number_of_points && repeat_count)
	    {
	      simple->flags[i++] = *flags_start;
	      repeat_count--;
	    }

	  /* Skip one byte in flags_start.  */
	  flags_start++;
	}

      flags_start++;
    }

  /* If an insufficient number of flags have been read, then the
     outline is invalid.  */

  if (i != number_of_points)
    {
      glyph->simple = NULL;
      xfree (simple);
      return;
    }

  /* Now that the flags have been decoded, start decoding the
     vectors.  */
  simple->x_coordinates = (int16_t *) (simple->flags + number_of_points);
  vec_start = flags_start;
  i = 0;
  x = 0;

  /* flags_start is now repurposed to act as a pointer to the flags
     for the current vector! */
  flags_start = simple->flags;

  while (i < number_of_points)
    {
      delta = 0;

      if ((*flags_start) & 02) /* X_SHORT_VECTOR */
	{
	  /* The next byte is a delta to apply to the previous
	     value.  Make sure it is in bounds.  */

	  if (vec_start + 1 >= glyf->glyphs + glyf->size)
	    {
	      glyph->simple = NULL;
	      xfree (simple);
	      return;
	    }

	  delta = *vec_start++;

	  if (!(*flags_start & 020)) /* SAME_X */
	    delta = -delta;
	}
      else if (!(*flags_start & 020)) /* SAME_X */
	{
	  /* The next word is a delta to apply to the previous value.
	     Make sure it is in bounds.  */

	  if (vec_start + 2 >= glyf->glyphs + glyf->size)
	    {
	      glyph->simple = NULL;
	      xfree (simple);
	      return;
	    }

	  /* Read the unaligned word and swap it.  */
	  memcpy (&delta, vec_start, sizeof delta);
	  sfnt_swap16 (&delta);
	  vec_start += 2;
	}

      /* Apply the delta and set the X value.  */
      x += delta;
      simple->x_coordinates[i++] = x;
      flags_start++;
    }

  /* Decode the Y vector.  flags_start is again repurposed to act as a
     pointer to the flags for the current vector.  */
  flags_start = simple->flags;
  y = 0;
  simple->y_coordinates = simple->x_coordinates + i;
  i = 0;

  while (i < number_of_points)
    {
      delta = 0;

      if (*flags_start & 04) /* Y_SHORT_VECTOR */
	{
	  /* The next byte is a delta to apply to the previous
	     value.  Make sure it is in bounds.  */

	  if (vec_start + 1 >= glyf->glyphs + glyf->size)
	    {
	      glyph->simple = NULL;
	      xfree (simple);
	      return;
	    }

	  delta = *vec_start++;

	  if (!(*flags_start & 040)) /* SAME_Y */
	    delta = -delta;
	}
      else if (!(*flags_start & 040)) /* SAME_Y */
	{
	  /* The next word is a delta to apply to the previous value.
	     Make sure it is in bounds.  */

	  if (vec_start + 2 >= glyf->glyphs + glyf->size)
	    {
	      glyph->simple = NULL;
	      xfree (simple);
	      return;
	    }

	  /* Read the unaligned word and swap it.  */
	  memcpy (&delta, vec_start, sizeof delta);
	  sfnt_swap16 (&delta);
	  vec_start += 2;
	}

      /* Apply the delta and set the X value.  */
      y += delta;
      simple->y_coordinates[i++] = y;
      flags_start++;
    }

  /* All done.  */
  simple->y_coordinates_end = simple->y_coordinates + i;
  glyph->simple = simple;
  return;
}

/* Read the compound glyph outline from the glyph GLYPH from the
   specified glyf table at the given offset.  Set GLYPH->compound to a
   non-NULL value upon success, else set it to NULL.  */

static void
sfnt_read_compound_glyph (struct sfnt_glyph *glyph,
			  struct sfnt_glyf_table *glyf,
			  ptrdiff_t offset)
{
  uint16_t flags, instruction_length, words[2], words4[4];
  size_t required_bytes, num_components, i;
  unsigned char *data, *instruction_base;

  /* Assume failure for now.  Figure out how many bytes have to be
     allocated by reading the compound data.  */
  glyph->compound = NULL;
  required_bytes = 0;
  num_components = 0;
  data = glyf->glyphs + offset;

  /* Offset could be unaligned.  */
  do
    {
      if (data + 2 > glyf->glyphs + glyf->size)
	return;

      memcpy (&flags, data, sizeof flags);
      sfnt_swap16 (&flags);
      data += sizeof flags;

      /* Require at least one structure to hold this data.  */
      required_bytes += sizeof (struct sfnt_compound_glyph_component);
      num_components++;

      /* Skip past unused data.  */
      data += 2;

      if (flags & 01) /* ARG_1_AND_2_ARE_WORDS */
	data += sizeof (int16_t) * 2;
      else
	data += sizeof (int8_t) * 2;

      if (flags & 010) /* WE_HAVE_A_SCALE */
	data += sizeof (uint16_t);
      else if (flags & 0100) /* WE_HAVE_AN_X_AND_Y_SCALE */
	data += sizeof (uint16_t) * 2;
      else if (flags & 0200) /* WE_HAVE_A_TWO_BY_TWO */
	data += sizeof (uint16_t) * 4;
    }
  while (flags & 040); /* MORE_COMPONENTS */

  if (flags & 0400) /* WE_HAVE_INSTRUCTIONS */
    {
      /* Figure out the instruction length.  */
      if (data + 2 > glyf->glyphs + glyf->size)
	return;

      /* Now see how much is required to hold the instruction
	 data.  */
      memcpy (&instruction_length, data,
	      sizeof instruction_length);
      sfnt_swap16 (&instruction_length);
      required_bytes += instruction_length;
      data += sizeof data + instruction_length;
    }

  /* Now allocate the buffer to hold all the glyph data.  */
  glyph->compound = xmalloc (sizeof *glyph->compound
			     + required_bytes);
  glyph->compound->components
    = (struct sfnt_compound_glyph_component *) (glyph->compound + 1);
  glyph->compound->num_components = num_components;

  /* Figure out where instruction data starts.  It comes after
     glyph->compound->components ends.  */
  instruction_base
    = (unsigned char *) (glyph->compound->components
			 + glyph->compound->num_components);

  /* Start reading.  */
  i = 0;
  data = glyf->glyphs + offset;
  do
    {
      if (data + 4 > glyf->glyphs + glyf->size)
	{
	  xfree (glyph->compound);
	  glyph->compound = NULL;
	  return;
	}

      memcpy (&flags, data, sizeof flags);
      sfnt_swap16 (&flags);
      data += sizeof flags;
      glyph->compound->components[i].flags = flags;

      memcpy (&glyph->compound->components[i].glyph_index,
	      data, sizeof glyph->compound->components[i].glyph_index);
      sfnt_swap16 (&glyph->compound->components[i].glyph_index);
      data += sizeof glyph->compound->components[i].glyph_index;

      if (flags & 01) /* ARG_1_AND_2_ARE_WORDS.  */
	{
	  if (data + 4 > glyf->glyphs + glyf->size)
	    {
	      xfree (glyph->compound);
	      glyph->compound = NULL;
	      return;
	    }

	  /* Read two words into arg1 and arg2.  */
	  memcpy (words, data, sizeof words);
	  sfnt_swap16 (&words[0]);
	  sfnt_swap16 (&words[1]);

	  glyph->compound->components[i].argument1.c = words[0];
	  glyph->compound->components[i].argument2.c = words[1];
	  data += sizeof words;
	}
      else
	{
	  if (data + 2 > glyf->glyphs + glyf->size)
	    {
	      xfree (glyph->compound);
	      glyph->compound = NULL;
	      return;
	    }

	  /* Read two bytes into arg1 and arg2.  */
	  glyph->compound->components[i].argument1.a = data[0];
	  glyph->compound->components[i].argument2.a = data[1];
	  data += 2;
	}

      if (flags & 010) /* WE_HAVE_A_SCALE */
	{
	  if (data + 2 > glyf->glyphs + glyf->size)
	    {
	      xfree (glyph->compound);
	      glyph->compound = NULL;
	      return;
	    }

	  /* Read one word into scale.  */
	  memcpy (&glyph->compound->components[i].u.scale, data,
		  sizeof glyph->compound->components[i].u.scale);
	  sfnt_swap16 (&glyph->compound->components[i].u.scale);
	  data += sizeof glyph->compound->components[i].u.scale;
	}
      else if (flags & 0100) /* WE_HAVE_AN_X_AND_Y_SCALE.  */
	{
	  if (data + 4 > glyf->glyphs + glyf->size)
	    {
	      xfree (glyph->compound);
	      glyph->compound = NULL;
	      return;
	    }

	  /* Read two words into xscale and yscale.  */
	  memcpy (words, data, sizeof words);
	  sfnt_swap16 (&words[0]);
	  sfnt_swap16 (&words[1]);

	  glyph->compound->components[i].u.a.xscale = words[0];
	  glyph->compound->components[i].u.a.yscale = words[1];
	  data += sizeof words;
	}
      else if (flags & 0200) /* WE_HAVE_A_TWO_BY_TWO */
	{
	  if (data + 8 > glyf->glyphs + glyf->size)
	    {
	      xfree (glyph->compound);
	      glyph->compound = NULL;
	      return;
	    }

	  /* Read 4 words into the transformation matrix.  */
	  memcpy (words4, data, sizeof words4);
	  sfnt_swap16 (&words4[0]);
	  sfnt_swap16 (&words4[1]);
	  sfnt_swap16 (&words4[2]);
	  sfnt_swap16 (&words4[3]);

	  glyph->compound->components[i].u.b.xscale = words4[0];
	  glyph->compound->components[i].u.b.scale01 = words4[1];
	  glyph->compound->components[i].u.b.scale10 = words4[2];
	  glyph->compound->components[i].u.b.yscale = words4[3];
	  data += sizeof words4;
	}

      /* Record the component flags.  */
      glyph->compound->components[i].flags = flags;

      i++;
    }
  while (flags & 040); /* MORE_COMPONENTS */

  if (flags & 0400) /* WE_HAVE_INSTR */
    {
      /* Figure out the instruction length.  */
      if (data + 2 > glyf->glyphs + glyf->size)
	{
	  xfree (glyph->compound);
	  glyph->compound = NULL;
	  return;
	}

      /* Now see how much is required to hold the instruction
	 data.  */
      memcpy (&glyph->compound->instruction_length,
	      data,
	      sizeof glyph->compound->instruction_length);
      sfnt_swap16 (&glyph->compound->instruction_length);
      data += 2;

      /* Read the instructions.  */
      glyph->compound->instructions = instruction_base;

      if (data + glyph->compound->instruction_length
	  > glyf->glyphs + glyf->size)
	{
	  xfree (glyph->compound);
	  glyph->compound = NULL;
	  return;
	}

      memcpy (instruction_base, data,
	      glyph->compound->instruction_length);
    }
  else
    {
      glyph->compound->instructions = NULL;
      glyph->compound->instruction_length = 0;
    }

  /* Data read successfully.  */
  return;
}

/* Read the description of the glyph GLYPH_CODE from the specified
   glyf table, using the offsets of LOCA_SHORT or LOCA_LONG, depending
   on which is non-NULL.  */

TEST_STATIC struct sfnt_glyph *
sfnt_read_glyph (sfnt_glyph glyph_code,
		 struct sfnt_glyf_table *glyf,
		 struct sfnt_loca_table_short *loca_short,
		 struct sfnt_loca_table_long *loca_long)
{
  struct sfnt_glyph glyph, *memory;
  ptrdiff_t offset, next_offset;

  /* Check the glyph code is within bounds.  */
  if (glyph_code > 65535)
    return NULL;

  if (loca_short)
    {
      /* Check that the glyph is within bounds.  glyph_code + 1 is the
	 entry in the table which defines the length of the glyph.  */
      if (glyph_code + 1 >= loca_short->num_offsets)
	return NULL;

      offset = loca_short->offsets[glyph_code] * 2;
      next_offset = loca_short->offsets[glyph_code + 1] * 2;
    }
  else if (loca_long)
    {
      if (glyph_code + 1 >= loca_long->num_offsets)
	return NULL;

      offset = loca_long->offsets[glyph_code];
      next_offset = loca_long->offsets[glyph_code + 1];
    }
  else
    abort ();

  /* If offset - next_offset is 0, then the glyph is empty.  Its
     horizontal advance may still be provided by the hmtx table.  */

  if (offset == next_offset)
    {
      glyph.number_of_contours = 0;
      glyph.xmin = 0;
      glyph.ymin = 0;
      glyph.xmax = 0;
      glyph.ymax = 0;
      glyph.simple = xmalloc (sizeof *glyph.simple);
      glyph.compound = NULL;
      memset (glyph.simple, 0, sizeof *glyph.simple);
      memory = xmalloc (sizeof *memory);
      *memory = glyph;
      return memory;
    }

  /* Verify that GLYF is big enough to hold a glyph at OFFSET.  */
  if (glyf->size < offset + SFNT_ENDOF (struct sfnt_glyph,
					ymax, sfnt_fword))
    return NULL;

  /* Copy over the glyph data.  */
  memcpy (&glyph, glyf->glyphs + offset,
	  SFNT_ENDOF (struct sfnt_glyph,
		      ymax, sfnt_fword));

  /* Swap the glyph data.  */
  sfnt_swap16 (&glyph.number_of_contours);
  sfnt_swap16 (&glyph.xmin);
  sfnt_swap16 (&glyph.ymin);
  sfnt_swap16 (&glyph.xmax);
  sfnt_swap16 (&glyph.ymax);

  /* Figure out what needs to be read based on
     glyph.number_of_contours.  */
  if (glyph.number_of_contours >= 0)
    {
      /* Read the simple glyph.  */

      glyph.compound = NULL;
      sfnt_read_simple_glyph (&glyph, glyf,
			      offset + SFNT_ENDOF (struct sfnt_glyph,
						   ymax, sfnt_fword));

      if (glyph.simple)
	{
	  memory = xmalloc (sizeof glyph);
	  *memory = glyph;

	  return memory;
	}
    }
  else
    {
      /* Read the compound glyph.  */

      glyph.simple = NULL;
      sfnt_read_compound_glyph (&glyph, glyf,
				offset + SFNT_ENDOF (struct sfnt_glyph,
						     ymax, sfnt_fword));

      if (glyph.compound)
	{
	  memory = xmalloc (sizeof glyph);
	  *memory = glyph;

	  return memory;
	}
    }

  return NULL;
}

/* Free a glyph returned from sfnt_read_glyph.  GLYPH may be NULL.  */

TEST_STATIC void
sfnt_free_glyph (struct sfnt_glyph *glyph)
{
  if (!glyph)
    return;

  xfree (glyph->simple);
  xfree (glyph->compound);
  xfree (glyph);
}



/* Glyph outline decomposition.  */

/* Apply the transform in the compound glyph component COMPONENT to
   the array of points of length NUM_COORDINATES given as X and Y.  */

static void
sfnt_transform_coordinates (struct sfnt_compound_glyph_component *component,
			    sfnt_fixed *restrict x, sfnt_fixed *restrict y,
			    size_t num_coordinates)
{
  double m1, m2, m3;
  double m4, m5, m6;
  size_t i;

  if (component->flags & 010) /* WE_HAVE_A_SCALE */
    {
      for (i = 0; i < num_coordinates; ++i)
	{
	  x[i] *= component->u.scale / 16384.0;
	  y[i] *= component->u.scale / 16384.0;
	}
    }
  else if (component->flags & 0100) /* WE_HAVE_AN_X_AND_Y_SCALE */
    {
      for (i = 0; i < num_coordinates; ++i)
	{
	  x[i] *= component->u.a.xscale / 16384.0;
	  y[i] *= component->u.a.yscale / 16384.0;
	}
    }
  else if (component->flags & 0200) /* WE_HAVE_A_TWO_BY_TWO */
    {
      /* Apply the specified affine transformation.
	 A transform looks like:

	   M1 M2 M3     X
	   M4 M5 M6   * Y

	   =

	   M1*X + M2*Y + M3*1 = X1
	   M4*X + M5*Y + M6*1 = Y1

         (In most transforms, there is another row at the bottom for
          mathematical reasons.  Since Z1 is always 1.0, the row is
          simply implied to be 0 0 1, because 0 * x + 0 * y + 1 * 1 =
          1.0.  See the definition of matrix3x3 in image.c for some
          more explanations about this.) */
      m1 = component->u.b.xscale / 16384.0;
      m2 = component->u.b.scale01 / 16384.0;
      m3 = 0;
      m4 = component->u.b.scale10 / 16384.0;
      m5 = component->u.b.yscale / 16384.0;
      m6 = 0;

      for (i = 0; i < num_coordinates; ++i)
	{
	  x[i] = m1 * x[i] + m2 * y[i] + m3 * 1;
	  y[i] = m4 * x[i] + m5 * y[i] + m6 * 1;
	}
    }
}

struct sfnt_compound_glyph_context
{
  /* Array of points.  */
  sfnt_fixed *x_coordinates, *y_coordinates;

  /* Array of flags for the points.  */
  unsigned char *flags;

  /* Number of points in that array, and the size of that array.  */
  size_t num_points, points_size;

  /* Array of contour end points.  */
  ptrdiff_t *contour_end_points;

  /* Number of elements in and the size of that array.  */
  size_t num_end_points, end_points_size;
};

/* Extend the arrays inside the compound glyph decomposition context
   CONTEXT.  NUMBER_OF_CONTOURS is the number of contours to add.
   NUMBER_OF_POINTS is the number of points to add.

   Return pointers to the beginning of the extension in *X_BASE,
   *Y_BASE, *FLAGS_BASE and *CONTOUR_BASE.  Value zero upon success,
   and something else on failure.  */

static int
sfnt_expand_compound_glyph_context (struct sfnt_compound_glyph_context *context,
				    size_t number_of_contours,
				    size_t number_of_points,
				    sfnt_fixed **x_base, sfnt_fixed **y_base,
				    unsigned char **flags_base,
				    ptrdiff_t **contour_base)
{
  size_t size_bytes;

  /* Add each field while checking for overflow.  */
  if (INT_ADD_WRAPV (number_of_contours, context->num_end_points,
		     &context->num_end_points))
    return 1;

  if (INT_ADD_WRAPV (number_of_points, context->num_points,
		     &context->num_points))
    return 1;

  /* Reallocate each array to the new size if necessary.  */
  if (context->points_size < context->num_points)
    {
      if (INT_MULTIPLY_WRAPV (context->num_points, 2,
			      &context->points_size))
	context->points_size = context->num_points;

      if (INT_MULTIPLY_WRAPV (context->points_size,
			      sizeof *context->x_coordinates,
			      &size_bytes))
	return 1;

      context->x_coordinates = xrealloc (context->x_coordinates,
					 size_bytes);
      context->y_coordinates = xrealloc (context->y_coordinates,
					 size_bytes);
      context->flags = xrealloc (context->flags,
				 context->points_size);
    }

  /* Set x_base and y_base.  */
  *x_base = (context->x_coordinates
	     + context->num_points
	     - number_of_points);
  *y_base = (context->y_coordinates
	     + context->num_points
	     - number_of_points);
  *flags_base = (context->flags
		 + context->num_points
		 - number_of_points);

  if (context->end_points_size < context->num_end_points)
    {
      if (INT_MULTIPLY_WRAPV (context->num_end_points, 2,
			      &context->end_points_size))
	context->end_points_size = context->num_end_points;

      if (INT_MULTIPLY_WRAPV (context->end_points_size,
			      sizeof *context->contour_end_points,
			      &size_bytes))
	return 1;

      context->contour_end_points
	= xrealloc (context->contour_end_points,
		    size_bytes);
    }

  /* Set contour_base.  */
  *contour_base = (context->contour_end_points
		   + context->num_end_points
		   - number_of_contours);
  return 0;
}

/* Round the 16.16 fixed point number NUMBER to the nearest integral
   value.  */

static int32_t
sfnt_round_fixed (int32_t number)
{
  /* Add 0.5... */
  number += (1 << 15);

  /* Remove the fractional.  */
  return number & ~0xffff;
}

/* Decompose GLYPH, a compound glyph, into an array of points and
   contours.  CONTEXT should be zeroed and put on the stack.  OFF_X
   and OFF_Y should be zero, as should RECURSION_COUNT.  GET_GLYPH and
   FREE_GLYPH, along with DCONTEXT, mean the same as in
   sfnt_decompose_glyph.  */

static int
sfnt_decompose_compound_glyph (struct sfnt_glyph *glyph,
			       struct sfnt_compound_glyph_context *context,
			       sfnt_get_glyph_proc get_glyph,
			       sfnt_free_glyph_proc free_glyph,
			       sfnt_fixed off_x, sfnt_fixed off_y,
			       int recursion_count,
			       void *dcontext)
{
  struct sfnt_glyph *subglyph;
  int i, j, rc;
  bool need_free;
  struct sfnt_compound_glyph_component *component;
  sfnt_fixed x, y, xtemp, ytemp;
  ptrdiff_t point, point2, index;
  uint16_t last_point, number_of_contours;
  sfnt_fixed *x_base, *y_base;
  ptrdiff_t *contour_base;
  unsigned char *flags_base;
  ptrdiff_t base_index, contour_start;

  /* Prevent infinite loops.  */
  if (recursion_count > 12)
    return 1;

  /* Set up the base index.  */
  base_index = context->num_points;

  for (j = 0; j < glyph->compound->num_components; ++j)
    {
      /* Look up the associated subglyph.  */
      component = &glyph->compound->components[j];
      subglyph = get_glyph (component->glyph_index,
			    dcontext, &need_free);

      if (!subglyph)
	return -1;

      /* Compute the offset for the component.  */
      if (component->flags & 02) /* ARGS_ARE_XY_VALUES */
	{
	  /* Component offsets are X/Y values as opposed to points
	     GLYPH.  */

	  if (!(component->flags & 01)) /* ARG_1_AND_2_ARE_WORDS */
	    {
	      /* X and Y are signed bytes.  */
	      x = component->argument1.b << 16;
	      y = component->argument2.b << 16;
	    }
	  else
	    {
	      /* X and Y are signed words.  */
	      x = component->argument1.d << 16;
	      y = component->argument1.d << 16;
	    }

	  /* If there is some kind of scale and component offsets are
	     scaled, then apply the transform to the offset.  */
	  if (component->flags & 04000) /* SCALED_COMPONENT_OFFSET */
	    sfnt_transform_coordinates (component, &x, &y, 1);

	  if (component->flags & 04) /* ROUND_XY_TO_GRID */
	    {
	      x = sfnt_round_fixed (x);
	      y = sfnt_round_fixed (y);
	    }
	}
      else
	{
	  /* The offset is determined by matching a point location in
	     a preceeding component with a point location in the
	     current component.  The index of the point in the
	     previous component can be determined by adding
	     component->argument1.a or component->argument1.c to
	     point.  argument2 contains the index of the point in the
	     current component.  */

	  if (!(component->flags & 01)) /* ARG_1_AND_2_ARE_WORDS */
	    {
	      point = base_index + component->argument1.a;
	      point2 = component->argument2.a;
	    }
	  else
	    {
	      point = base_index + component->argument1.c;
	      point2 = component->argument2.c;
	    }

	  /* If subglyph is itself a compound glyph, how is Emacs
	     supposed to compute the offset of its children correctly,
	     when said offset depends on itself? */

	  if (subglyph->compound)
	    {
	      if (need_free)
		free_glyph (subglyph, dcontext);

	      return 1;
	    }

	  /* Check that POINT and POINT2 are valid.  */
	  if (point >= context->num_points
	      || (subglyph->simple->y_coordinates + point2
		  >= subglyph->simple->y_coordinates_end))
	    {
	      if (need_free)
		free_glyph (subglyph, dcontext);

	      return 1;
	    }

	  /* Get the points and use them to compute the offsets.  */
	  xtemp = context->x_coordinates[point];
	  ytemp = context->y_coordinates[point];
	  x = (xtemp - subglyph->simple->x_coordinates[point2]) << 16;
	  y = (ytemp - subglyph->simple->y_coordinates[point2]) << 16;
	}

      /* Record the size of the point array before expansion.  This
	 will be the base to apply to all points coming from this
	 subglyph.  */
      contour_start = context->num_points;

      if (subglyph->simple)
	{
	  /* Simple subglyph.  Copy over the points and contours, and
	     transform them.  */
	  if (subglyph->number_of_contours)
	    {
	      index = subglyph->number_of_contours - 1;
	      last_point
		= subglyph->simple->end_pts_of_contours[index];
	      number_of_contours = subglyph->number_of_contours;


	      /* Grow various arrays.  */
	      rc = sfnt_expand_compound_glyph_context (context,
						       /* Number of
							  new contours
							  required.  */
						       number_of_contours,
						       /* Number of new
							  points
							  required.  */
						       last_point + 1,
						       &x_base,
						       &y_base,
						       &flags_base,
						       &contour_base);
	      if (rc)
		{
		  if (need_free)
		    free_glyph (subglyph, dcontext);

		  return 1;
		}

	      for (i = 0; i <= last_point; ++i)
		{
		  x_base[i] = ((subglyph->simple->x_coordinates[i] << 16)
			       + off_x + x);
		  y_base[i] = ((subglyph->simple->y_coordinates[i] << 16)
			       + off_y + y);
		  flags_base[i] = subglyph->simple->flags[i];
		}

	      /* Apply the transform to the points.  */
	      sfnt_transform_coordinates (component, x_base, y_base,
					  last_point + 1);

	      /* Copy over the contours.  */
	      for (i = 0; i < number_of_contours; ++i)
		contour_base[i] = (contour_start
				   + subglyph->simple->end_pts_of_contours[i]);
	    }
	}
      else
	{
	  /* Compound subglyph.  Decompose the glyph recursively, and
	     then apply the transform.  */
	  rc = sfnt_decompose_compound_glyph (subglyph,
					      context,
					      get_glyph,
					      free_glyph,
					      off_x + x,
					      off_y + y,
					      recursion_count + 1,
					      dcontext);

	  if (rc)
	    {
	      if (need_free)
		free_glyph (subglyph, dcontext);

	      return 1;
	    }

	  sfnt_transform_coordinates (component,
				      context->x_coordinates + contour_start,
				      context->y_coordinates + contour_start,
				      contour_start - context->num_points);
	}

      if (need_free)
	free_glyph (subglyph, dcontext);
    }

  /* Decomposition is complete.  CONTEXT now contains the adjusted
     outlines of the entire compound glyph.  */
  return 0;
}

/* Linear-interpolate to a point halfway between the points specified
   by CONTROL1 and CONTROL2.  Put the result in RESULT.  */

static void
sfnt_lerp_half (struct sfnt_point *control1, struct sfnt_point *control2,
		struct sfnt_point *result)
{
  result->x = control1->x + ((control2->x - control1->x) >> 1);
  result->y = control1->y + ((control2->y - control1->y) >> 1);
}

/* Decompose GLYPH into its individual components.  Call MOVE_TO to
   move to a specific location.  For each line encountered, call
   LINE_TO to draw a line to that location.  For each spline
   encountered, call CURVE_TO to draw the curves comprising the
   spline.

   If GLYPH is compound, use GET_GLYPH to obtain subglyphs.  PROC must
   return whether or not FREE_PROC will be called with the glyph after
   sfnt_decompose_glyph is done with it.

   Both functions will be called with DCONTEXT as an argument.

   The winding rule used to fill the resulting lines is described in
   chapter 2 of the TrueType reference manual, under the heading
   "distinguishing the inside from the outside of a glyph."

   Value is 0 upon success, or some non-zero value upon failure, which
   can happen if the glyph is invalid.  */

static int
sfnt_decompose_glyph (struct sfnt_glyph *glyph,
		      sfnt_move_to_proc move_to,
		      sfnt_line_to_proc line_to,
		      sfnt_curve_to_proc curve_to,
		      sfnt_get_glyph_proc get_glyph,
		      sfnt_free_glyph_proc free_glyph,
		      void *dcontext)
{
  size_t here, start, last;
  struct sfnt_point pen, control1, control2;
  struct sfnt_compound_glyph_context context;
  size_t n;

  if (glyph->simple)
    {
      if (!glyph->number_of_contours)
	/* No contours.  Nothing needs to be decomposed.  */
	return 0;

      here = 0;

      for (n = 0; n < glyph->number_of_contours; ++n)
	{
	  /* here is the first index into the glyph's point arrays
	     belonging to the contour in question.  last is the index
	     of the last point in the contour.  */
	  last = glyph->simple->end_pts_of_contours[n];

	  /* Move to the start.  */
	  pen.x = glyph->simple->x_coordinates[here] << 16U;
	  pen.y = glyph->simple->y_coordinates[here] << 16U;
	  move_to (pen, dcontext);

	  /* Record start so the contour can be closed.  */
	  start = here;

	  /* If there is only one point in a contour, draw a one pixel
	     wide line.  */
	  if (last == here)
	    {
	      line_to (pen, dcontext);
	      here++;

	      continue;
	    }

	  if (here > last)
	    /* Indices moved backwards.  */
	    return 1;

	  /* Now start reading points.  If the next point is on the
	     curve, then it is actually a line.  */
	  for (++here; here <= last; ++here)
	    {
	      /* Make sure here is within bounds.  */
	      if (here >= glyph->simple->number_of_points)
		return 1;

	      if (glyph->simple->flags[here] & 01) /* On Curve */
		{
		  pen.x = glyph->simple->x_coordinates[here] << 16U;
		  pen.y = glyph->simple->y_coordinates[here] << 16U;

		  /* See if the last point was on the curve.  If it
		     wasn't, then curve from there to here.  */
		  if (!(glyph->simple->flags[here - 1] & 01))
		    {
		      control1.x
			= glyph->simple->x_coordinates[here - 1] << 16U;
		      control1.y
			= glyph->simple->y_coordinates[here - 1] << 16U;
		      curve_to (control1, pen, dcontext);
		    }
		  else
		    /* Otherwise, this is an ordinary line from there
		       to here.  */
		    line_to (pen, dcontext);

		  continue;
		}

	      /* If the last point was on the curve, then there's
		 nothing extraordinary to do yet.  */
	      if (glyph->simple->flags[here - 1] & 01)
		;
	      else
		{
		  /* Otherwise, interpolate the point halfway between
		     the last and current points and make that point
		     the pen.  */
		  control1.x = glyph->simple->x_coordinates[here - 1] << 16U;
		  control1.y = glyph->simple->y_coordinates[here - 1] << 16U;
		  control2.x = glyph->simple->x_coordinates[here] << 16U;
		  control2.y = glyph->simple->y_coordinates[here] << 16U;
		  sfnt_lerp_half (&control1, &control2, &pen);
		  curve_to (control1, pen, dcontext);
		}
	    }

	  /* Now close the contour if there is more than one point
	     inside it.  */
	  if (start != here - 1)
	    {
	      /* Restore here after the for loop increased it.  */
	      here --;

	      if (glyph->simple->flags[start] & 01) /* On Curve */
		{
		  pen.x = glyph->simple->x_coordinates[start] << 16U;
		  pen.y = glyph->simple->y_coordinates[start] << 16U;

		  /* See if the last point (in this case, `here') was
		     on the curve.  If it wasn't, then curve from
		     there to here.  */
		  if (!(glyph->simple->flags[here] & 01))
		    {
		      control1.x
			= glyph->simple->x_coordinates[here] << 16U;
		      control1.y
			= glyph->simple->y_coordinates[here] << 16U;
		      curve_to (control1, pen, dcontext);
		    }
		  else
		    /* Otherwise, this is an ordinary line from there
		       to here.  */
		    line_to (pen, dcontext);
		}

	      /* Restore here to where it was earlier.  */
	      here++;
	    }
	}

      return 0;
    }

  /* Decompose the specified compound glyph.  */
  memset (&context, 0, sizeof context);

  if (sfnt_decompose_compound_glyph (glyph, &context,
				     get_glyph, free_glyph,
				     0, 0, 0, dcontext))
    {
      xfree (context.x_coordinates);
      xfree (context.y_coordinates);
      xfree (context.flags);
      xfree (context.contour_end_points);

      return 1;
    }

  /* Now, generate the outlines.  */

  if (!context.num_end_points)
    /* No contours.  */
    goto early;

  here = 0;

  for (n = 0; n < context.num_end_points; ++n)
    {
      /* here is the first index into the glyph's point arrays
	 belonging to the contour in question.  last is the index
	 of the last point in the contour.  */
      last = context.contour_end_points[n];

      /* Move to the start.  */
      pen.x = context.x_coordinates[here];
      pen.y = context.y_coordinates[here];
      move_to (pen, dcontext);

      /* Record start so the contour can be closed.  */
      start = here;

      /* If there is only one point in a contour, draw a one pixel
	 wide line.  */
      if (last == here)
	{
	  line_to (pen, dcontext);
	  here++;

	  continue;
	}

      if (here > last)
	/* Indices moved backwards.  */
	goto fail;

      /* Now start reading points.  If the next point is on the
	 curve, then it is actually a line.  */
      for (++here; here <= last; ++here)
	{
	  /* Make sure here is within bounds.  */
	  if (here >= context.num_points)
	    return 1;

	  if (context.flags[here] & 01) /* On Curve */
	    {
	      pen.x = context.x_coordinates[here];
	      pen.y = context.y_coordinates[here];

	      /* See if the last point was on the curve.  If it
		 wasn't, then curve from there to here.  */
	      if (!(context.flags[here - 1] & 01))
		{
		  control1.x = context.x_coordinates[here - 1];
		  control1.y = context.y_coordinates[here - 1];
		  curve_to (control1, pen, dcontext);
		}
	      else
		/* Otherwise, this is an ordinary line from there
		   to here.  */
		line_to (pen, dcontext);

	      continue;
	    }

	  /* If the last point was on the curve, then there's
	     nothing extraordinary to do yet.  */
	  if (context.flags[here - 1] & 01)
	    ;
	  else
	    {
	      /* Otherwise, interpolate the point halfway between
		 the last and current points and make that point
		 the pen.  */
	      control1.x = context.x_coordinates[here - 1];
	      control1.y = context.y_coordinates[here - 1];
	      control2.x = context.x_coordinates[here];
	      control2.y = context.y_coordinates[here];
	      sfnt_lerp_half (&control1, &control2, &pen);
	      curve_to (control1, pen, dcontext);
	    }
	}

      /* Now close the contour if there is more than one point
	 inside it.  */
      if (start != here - 1)
	{
	  /* Restore here after the for loop increased it.  */
	  here --;

	  if (context.flags[start] & 01) /* On Curve */
	    {
	      pen.x = context.x_coordinates[start];
	      pen.y = context.y_coordinates[start];

	      /* See if the last point (in this case, `here') was
		 on the curve.  If it wasn't, then curve from
		 there to here.  */
	      if (!(context.flags[here] & 01))
		{
		  control1.x = context.x_coordinates[here];
		  control1.y = context.y_coordinates[here];
		  curve_to (control1, pen, dcontext);
		}
	      else
		/* Otherwise, this is an ordinary line from there
		   to here.  */
		line_to (pen, dcontext);
	    }

	  /* Restore here to where it was earlier.  */
	  here++;
	}
    }

 early:
  xfree (context.x_coordinates);
  xfree (context.y_coordinates);
  xfree (context.flags);
  xfree (context.contour_end_points);
  return 0;

 fail:
  xfree (context.x_coordinates);
  xfree (context.y_coordinates);
  xfree (context.flags);
  xfree (context.contour_end_points);
  return 1;
}

struct sfnt_build_glyph_outline_context
{
  /* The outline being built.  */
  struct sfnt_glyph_outline *outline;

  /* The head table.  */
  struct sfnt_head_table *head;

  /* The pixel size being used, and any extra flags to apply to the
     outline at this point.  */
  int pixel_size;

  /* Factor to multiply positions by to get the pixel width.  */
  sfnt_fixed factor;

  /* The position of the pen in 16.16 fixed point format.  */
  sfnt_fixed x, y;
};

/* Global state for sfnt_build_glyph_outline and related
   functions.  */
static struct sfnt_build_glyph_outline_context build_outline_context;

/* Append the given three words FLAGS, X, and Y to the outline
   currently being built.  Value is the new pointer to outline
   memory.  */

static struct sfnt_glyph_outline *
sfnt_build_append (int flags, sfnt_fixed x, sfnt_fixed y)
{
  struct sfnt_glyph_outline *outline;

  if (x == build_outline_context.x
      && y == build_outline_context.y)
    /* Ignore redundant motion.  */
    return build_outline_context.outline;

  outline = build_outline_context.outline;
  outline->outline_used++;

  /* See if the outline has to be extended.  Checking for overflow
     should not be necessary.  */

  if (outline->outline_used > outline->outline_size)
    {
      outline->outline_size = outline->outline_used * 2;

      /* Extend the outline to some size past the new size.  */
      outline = xrealloc (outline, (sizeof *outline
				    + (outline->outline_size
				       * sizeof *outline->outline)));
      outline->outline
	= (struct sfnt_glyph_outline_command *) (outline + 1);
    }

  /* Write the outline data.  */
  outline->outline[outline->outline_used - 1].flags = flags;
  outline->outline[outline->outline_used - 1].x = x;
  outline->outline[outline->outline_used - 1].y = y;

  /* Extend outline bounding box.  */

  if (outline->outline_used == 1)
    {
      /* These are the first points in the outline.  */
      outline->xmin = outline->xmax = x;
      outline->ymin = outline->ymax = y;
    }
  else
    {
      outline->xmin = MIN ((sfnt_fixed) x, outline->xmin);
      outline->ymin = MIN ((sfnt_fixed) y, outline->ymin);
      outline->xmax = MAX ((sfnt_fixed) x, outline->xmax);
      outline->ymax = MAX ((sfnt_fixed) y, outline->ymax);
    }

  return outline;
}

/* Multiply the two 16.16 fixed point numbers X and Y.  Return the
   result regardless of overflow.  */

static sfnt_fixed
sfnt_mul_fixed (sfnt_fixed x, sfnt_fixed y)
{
#ifdef INT64_MAX
  int64_t product;

  product = (int64_t) x * (int64_t) y;

  /* This can be done quickly with int64_t.  */
  return product >> 16;
#else
  int a, b, c, d, product_high;
  unsigned int carry, product_low;

  a = (x >> 16);
  c = (y >> 16);
  b = (x & 0xffff);
  d = (y & 0xffff);

  product_high = a * c + ((a * d + c * b) >> 16);
  carry = (a * d + c * b) << 16;
  product_low = b * d + carry;

  if (product_low < b * d)
    product_high++;

  return (product_high << 16) | (product_low >> 16);
#endif
}

/* Set the pen size to the specified point and return.  POINT will be
   scaled up to the pixel size.  */

static void
sfnt_move_to_and_build (struct sfnt_point point, void *dcontext)
{
  sfnt_fixed x, y;

  x = sfnt_mul_fixed (build_outline_context.factor, point.x);
  y = sfnt_mul_fixed (build_outline_context.factor, point.y);

  build_outline_context.outline = sfnt_build_append (0, x, y);
  build_outline_context.x = x;
  build_outline_context.y = y;
}

/* Record a line to the specified point and return.  POINT will be
   scaled up to the pixel size.  */

static void
sfnt_line_to_and_build (struct sfnt_point point, void *dcontext)
{
  sfnt_fixed x, y;

  x = sfnt_mul_fixed (build_outline_context.factor, point.x);
  y = sfnt_mul_fixed (build_outline_context.factor, point.y);

  build_outline_context.outline
    = sfnt_build_append (SFNT_GLYPH_OUTLINE_LINETO,
			 x, y);
  build_outline_context.x = x;
  build_outline_context.y = y;
}

/* Divide the two 16.16 fixed point numbers X and Y.  Return the
   result regardless of overflow.  */

static sfnt_fixed
sfnt_div_fixed (sfnt_fixed x, sfnt_fixed y)
{
#ifdef INT64_MAX
  int64_t result;

  result = ((int64_t) x << 16) / y;

  return result;
#else
  unsigned int reciprocal;
  int product;

  reciprocal = 1U << 31;
  reciprocal = reciprocal / y;

  product = x * reciprocal;

  /* This loses one bit at the end.  Now to see if anyone runs across
     this...  */
  return product << 1;
#endif
}

/* Return the ceiling value of the specified fixed point number X.  */

static sfnt_fixed
sfnt_ceil_fixed (sfnt_fixed x)
{
  if (!(x & 0177777))
    return x;

  return (x + 0200000) & 037777600000;
}

/* Return the floor value of the specified fixed point number X.  */

static sfnt_fixed
sfnt_floor_fixed (sfnt_fixed x)
{
  return x & 037777600000;
}

/* Given a curve consisting of three points CONTROL0, CONTROL1 and
   ENDPOINT, return whether or not the curve is sufficiently small to
   be approximated by a line between CONTROL0 and ENDPOINT.  */

static bool
sfnt_curve_is_flat (struct sfnt_point control0,
		    struct sfnt_point control1,
		    struct sfnt_point endpoint)
{
  struct sfnt_point g, h;

  g.x = control1.x - control0.x;
  g.y = control1.y - control0.y;
  h.x = endpoint.x - control0.x;
  h.y = endpoint.y - control0.y;

  /* 2.0 is a constant describing the area covered at which point the
     curve is considered "flat".  */
  return (abs (sfnt_mul_fixed (g.x, h.y)
	       - sfnt_mul_fixed (g.y, h.x))
	  <= 0400000);
}

/* Recursively split the splines in the bezier curve formed from
   CONTROL0, CONTROL1 and ENDPOINT until the area between the curve's
   two ends is small enough to be considered ``flat''.  Then, turn
   those ``flat'' curves into lines.  */

static void
sfnt_curve_to_and_build_1 (struct sfnt_point control0,
			   struct sfnt_point control1,
			   struct sfnt_point endpoint)
{
  struct sfnt_point ab, bc, abbc;

  /* control0, control and endpoint make up the spline.  Figure out
     its distance from a line.  */
  if (sfnt_curve_is_flat (control0, control1, endpoint))
    {
      /* Draw a line to endpoint.  */
      build_outline_context.outline
	= sfnt_build_append (SFNT_GLYPH_OUTLINE_LINETO,
			     endpoint.x, endpoint.y);
      build_outline_context.x = endpoint.x;
      build_outline_context.y = endpoint.y;
    }
  else
    {
      /* Calculate new control points.
         Maybe apply a recursion limit here? */
      sfnt_lerp_half (&control0, &control1, &ab);
      sfnt_lerp_half (&control1, &endpoint, &bc);
      sfnt_lerp_half (&ab, &bc, &abbc);

      /* Keep splitting until a flat enough spline results.  */
      sfnt_curve_to_and_build_1 (control0, ab, abbc);

      /* Then go on with the spline between control1 and endpoint.  */
      sfnt_curve_to_and_build_1 (abbc, bc, endpoint);
    }
}

/* Scale and decompose the specified bezier curve into individual
   lines.  Then, record each of those lines into the outline being
   built.  */

static void
sfnt_curve_to_and_build (struct sfnt_point control,
			 struct sfnt_point endpoint,
			 void *dcontext)
{
  struct sfnt_point control0;

  control0.x = build_outline_context.x;
  control0.y = build_outline_context.y;
  control.x = sfnt_mul_fixed (control.x,
			      build_outline_context.factor);
  control.y = sfnt_mul_fixed (control.y,
			      build_outline_context.factor);
  endpoint.x = sfnt_mul_fixed (endpoint.x,
			       build_outline_context.factor);
  endpoint.y = sfnt_mul_fixed (endpoint.y,
			       build_outline_context.factor);

  sfnt_curve_to_and_build_1 (control0, control, endpoint);
}

/* Non-reentrantly build the outline for the specified GLYPH at the
   given pixel size.  Return the outline data with a refcount of 0
   upon success, or NULL upon failure.

   Call GET_GLYPH and FREE_GLYPH with the specified DCONTEXT to obtain
   glyphs for compound glyph subcomponents.

   HEAD should be the `head' table of the font.  */

TEST_STATIC struct sfnt_glyph_outline *
sfnt_build_glyph_outline (struct sfnt_glyph *glyph,
			  struct sfnt_head_table *head,
			  int pixel_size,
			  sfnt_get_glyph_proc get_glyph,
			  sfnt_free_glyph_proc free_glyph,
			  void *dcontext)
{
  struct sfnt_glyph_outline *outline;
  int rc;

  /* Allocate the outline now with enough for 44 words at the end.  */
  outline = xmalloc (sizeof *outline + 40 * sizeof (*outline->outline));
  outline->outline_size = 40;
  outline->outline_used = 0;
  outline->refcount = 0;
  outline->outline
    = (struct sfnt_glyph_outline_command *) (outline + 1);

  /* DCONTEXT will be passed to GET_GLYPH and FREE_GLYPH, so global
     variables must be used to communicate with the decomposition
     functions.  */
  build_outline_context.outline = outline;
  build_outline_context.head = head;
  build_outline_context.pixel_size = pixel_size;

  /* Clear outline bounding box.  */
  outline->xmin = 0;
  outline->ymin = 0;
  outline->xmax = 0;
  outline->ymax = 0;

  /* Figure out how to convert from font unit-space to pixel space.
     To turn one unit to its corresponding pixel size given a ppem of
     1, the unit must be divided by head->units_per_em.  Then, it must
     be multipled by the ppem.  So,

       PIXEL = UNIT / UPEM * PPEM

     which means:

       PIXEL = UNIT * PPEM / UPEM

     It would be nice to get rid of this floating point arithmetic at
     some point.  */
  build_outline_context.factor
    = sfnt_div_fixed (pixel_size << 16,
		      head->units_per_em << 16);

  /* Decompose the outline.  */
  rc = sfnt_decompose_glyph (glyph, sfnt_move_to_and_build,
			     sfnt_line_to_and_build,
			     sfnt_curve_to_and_build,
			     get_glyph, free_glyph, dcontext);

  /* Synchronize the outline object with what might have changed
     inside sfnt_decompose_glyph.  */
  outline = build_outline_context.outline;

  if (rc)
    {
      xfree (outline);
      return NULL;
    }

  return outline;
}



/* Glyph rasterization.  The algorithm used here is fairly simple.
   Each contour is decomposed into lines, which turn into a polygon.
   Then, a bog standard edge filler is used to turn them into
   spans.  */

/* Coverage table.  This is a four dimensional array indiced by the Y,
   then X axis fractional, shifted down to 2 bits.  */

static unsigned char sfnt_poly_coverage[4][4] =
  {
    { 0x10, 0x10, 0x10, 0x10 },
    { 0x10, 0x10, 0x10, 0x10 },
    { 0x0f, 0x10, 0x10, 0x10 },
    { 0x10, 0x10, 0x10, 0x10 },
  };

/* Return the nearest coordinate on the sample grid no less than
   F.  */

static sfnt_fixed
sfnt_poly_grid_ceil (sfnt_fixed f)
{
  return (((f + (SFNT_POLY_START - 1))
	   & ~(SFNT_POLY_STEP - 1)) + SFNT_POLY_START);
}

enum
  {
    SFNT_POLY_ALIGNMENT = 4,
  };

/* Initialize the specified RASTER in preparation for displaying spans
   for OUTLINE, and set RASTER->refcount to 0.  The caller must then
   set RASTER->cells to a zeroed array of size RASTER->stride *
   RASTER->height, aligned to RASTER.  */

TEST_STATIC void
sfnt_prepare_raster (struct sfnt_raster *raster,
		     struct sfnt_glyph_outline *outline)
{
  raster->width
    = (sfnt_ceil_fixed (outline->xmax)
       - sfnt_floor_fixed (outline->xmin)) >> 16;
  raster->height
    = (sfnt_ceil_fixed (outline->ymax)
       - sfnt_floor_fixed (outline->ymin)) >> 16;
  raster->refcount = 0;

  /* Align the raster to a SFNT_POLY_ALIGNMENT byte boundary.  */
  raster->stride = ((raster->width
		     + (SFNT_POLY_ALIGNMENT - 1))
		    & ~(SFNT_POLY_ALIGNMENT - 1));

  raster->offx
    = sfnt_floor_fixed (outline->xmin) >> 16;
  raster->offy
    = sfnt_floor_fixed (outline->ymin) >> 16;
}

typedef void (*sfnt_edge_proc) (struct sfnt_edge *, size_t,
				void *);
typedef void (*sfnt_span_proc) (struct sfnt_edge *, sfnt_fixed, void *);

/* Move EDGE->x forward, assuming that the scanline has moved upwards
   by SFNT_POLY_STEP.  */

static void
sfnt_step_edge (struct sfnt_edge *edge)
{
  /* Step edge.  */
  edge->x += edge->step_x;
}

/* Build a list of edges for each contour in OUTLINE, applying
   OUTLINE->xmin and floor (OUTLINE->ymin) as the offset to each edge.
   Call EDGE_PROC with DCONTEXT and the resulting edges as arguments.
   It is OK to modify the edges given to EDGE_PROC.  Align all edges
   to the sub-pixel grid.  */

static void
sfnt_build_outline_edges (struct sfnt_glyph_outline *outline,
			  sfnt_edge_proc edge_proc, void *dcontext)
{
  struct sfnt_edge *edges;
  size_t i, edge, next_vertex;
  sfnt_fixed dx, dy, bot, step_x, ymin, xmin;
  int inc_x;
  size_t top, bottom, y;

  edges = alloca (outline->outline_used * sizeof *edges);
  edge = 0;

  /* ymin and xmin must be the same as the offset used to set offy and
     offx in rasters.  */
  ymin = sfnt_floor_fixed (outline->ymin);
  xmin = sfnt_floor_fixed (outline->xmin);

  for (i = 0; i < outline->outline_used; ++i)
    {
      /* Set NEXT_VERTEX to the next point (vertex) in this contour.

	 If i is past the end of the contour, then don't build edges
	 for this point.  */
      next_vertex = i + 1;

      if (next_vertex == outline->outline_used
	  || !(outline->outline[next_vertex].flags
	       & SFNT_GLYPH_OUTLINE_LINETO))
	continue;

      /* Skip past horizontal vertices.  */
      if (outline->outline[next_vertex].y == outline->outline[i].y)
	continue;

      /* Figure out the winding direction.  */
      if (outline->outline[next_vertex].y < outline->outline[i].y)
	/* Vector will cross imaginary ray from its bottom from the
	   left of the ray.  Winding is thus 1.  */
	edges[edge].winding = 1;
      else
	/* Moving clockwise.  Winding is thus -1.  */
        edges[edge].winding = -1;

      /* Figure out the top and bottom values of this edge.  If the
	 next edge is below, top is here and bot is the edge below.
	 If the next edge is above, then top is there and this is the
	 bottom.  */

      if (outline->outline[next_vertex].y < outline->outline[i].y)
	{
	  /* End of edge is below this one (keep in mind this is a
	     cartesian coordinate system, so smaller values are below
	     larger ones.) */
	  top = i;
	  bottom = next_vertex;
	}
      else
	{
	  /* End of edge is above this one.  */
	  bottom = i;
	  top = next_vertex;
	}

      bot = (outline->outline[bottom].y - ymin);
      edges[edge].top = (outline->outline[top].y - ymin);

      /* Record the edge.  Rasterization happens from bottom to
	 up, so record the X at the bottom.  */
      edges[edge].x = (outline->outline[bottom].x - xmin);
      dx = (outline->outline[top].x - outline->outline[bottom].x);
      dy = abs (outline->outline[top].y
		- outline->outline[bottom].y);

#ifdef TEST
      edges[edge].source_x = edges[edge].x;
#endif

      /* Compute the increment.  This is which direction X moves in
	 for each increase in Y.  */

      if (dx >= 0)
	inc_x = 1;
      else
	{
	  inc_x = -1;
	  dx = -dx;
	}

      /* Compute the step X.  This is how much X changes for each
	 increase in Y.  */
      step_x = inc_x * sfnt_div_fixed (dx, dy);

      /* Step to first grid point.  */
      y = sfnt_poly_grid_ceil (bot);

      /* If rounding would make the edge not cover any area, skip this
	 edge.  */
      if (y > edges[edge].top)
	continue;

      edges[edge].x += sfnt_mul_fixed (step_x, bot - y);
      edges[edge].bottom = y;
      edges[edge].next = NULL;

      /* Compute the step X scaled to the poly step.  */
      edges[edge].step_x
	= sfnt_mul_fixed (step_x, SFNT_POLY_STEP);

      edge++;
    }

  if (edge)
    edge_proc (edges, edge, dcontext);
}

static int
sfnt_compare_edges (const void *a, const void *b)
{
  const struct sfnt_edge *first, *second;

  first = a;
  second = b;

  return (int) (first->bottom - second->bottom);
}

/* Draw EDGES, an unsorted array of polygon edges of size SIZE.  For
   each scanline, call SPAN_FUNC with a list of active edges and
   coverage information, and DCONTEXT.

   Sort each edge in ascending order by the bottommost Y coordinate to
   which it applies.  Start a loop on the Y coordinate, which starts
   out at that of the bottommost edge.  For each iteration, add edges
   that now overlap with Y, keeping them sorted by X.  Poly those
   edges through SPAN_FUNC.  Then, move upwards by SFNT_POLY_STEP,
   remove edges that no longer apply, and interpolate the remaining
   edge's X coordinates.  Repeat until all the edges have been polyed.

   Or alternatively, think of this as such: each edge is actually a
   vector from its bottom position towards its top most position.
   Every time Y moves upwards, the position of each edge intersecting
   with Y is interpolated and added to a list of spans along with
   winding information that is then given to EDGE_FUNC.

   Anti-aliasing is performed using a coverage map for fractional
   coordinates, and incrementing the Y axis by SFNT_POLY_STEP instead
   of 1.  SFNT_POLY_STEP is chosen to always keep Y aligned to a grid
   placed such that there are always 1 << SFNT_POLY_SHIFT positions
   available for each integral pixel coordinate.  */

static void
sfnt_poly_edges (struct sfnt_edge *edges, size_t size,
		 sfnt_span_proc span_func, void *dcontext)
{
  sfnt_fixed y;
  size_t e;
  struct sfnt_edge *active, **prev, *a, *n;

  if (!size)
    return;

  /* Sort edges to ascend by Y-order.  Once again, remember: cartesian
     coordinates.  */
  qsort (edges, size, sizeof *edges, sfnt_compare_edges);

  /* Step down line by line.  Find active edges.  */

  y = edges[0].bottom;
  active = 0;
  active = NULL;
  e = 0;

  for (;;)
    {
      /* Add in new edges keeping them sorted.  */
      for (; e < size && edges[e].bottom <= y; ++e)
	{
	  /* Find where to place this edge.  */
	  for (prev = &active; (a = *prev); prev = &(a->next))
	    {
	      if (a->x > edges[e].x)
		break;
	    }

	  edges[e].next = *prev;
	  *prev = &edges[e];
	}

      /* Draw this span at the current position.  Y axis antialiasing
	 is expected to be handled by SPAN_FUNC.  */
      span_func (active, y, dcontext);

      /* Compute the next Y position.  */
      y += SFNT_POLY_STEP;

      /* Strip out edges that no longer have effect.  */

      for (prev = &active; (a = *prev);)
	{
	  if (a->top <= y)
	    *prev = a->next;
	  else
	    prev = &a->next;
	}

      /* Break if all is done.  */
      if (!active && e == size)
	break;

      /* Step all edges.  */
      for (a = active; a; a = a->next)
	sfnt_step_edge (a);

      /* Resort on X axis.  */
      for (prev = &active; (a = *prev) && (n = a->next);)
	{
	  if (a->x > n->x)
	    {
	      a->next = n->next;
	      n->next = a;
	      *prev = n;
	      prev = &active;
	    }
	  else
	    prev = &a->next;
	}
    }
}

/* Saturate-convert the given unsigned short value X to an unsigned
   char.  */

static unsigned char
sfnt_saturate_short (unsigned short x)
{
  return (unsigned char) ((x) | (0 - ((x) >> 8)));
}

/* Fill a single span of pixels between X0 and X1 at Y, a raster
   coordinate, onto RASTER.  */

static void
sfnt_fill_span (struct sfnt_raster *raster, sfnt_fixed y,
		sfnt_fixed x0, sfnt_fixed x1)
{
  unsigned char *start;
  unsigned char *coverage;
  sfnt_fixed left, right, end;
  unsigned short w, a;
  int row, col;

  /* Clip bounds to pixmap.  */
  if (x0 < 0)
    x0 = 0;

  if (x1 >= raster->width << 16)
    x1 = (raster->width - 1) << 16;

  /* Check for empty bounds.  */
  if (x1 <= x0)
    return;

  /* Figure out coverage based on Y axis fractional.  */
  coverage = sfnt_poly_coverage[(y >> (16 - SFNT_POLY_SHIFT))
				& SFNT_POLY_MASK];
  row = y >> 16;

  /* Don't fill out of bounds rows.  */
  if (row < 0 || row >= raster->height)
    return;

  /* Set start, then start filling according to coverage.  left and
     right are now 16.2.  */
  left = sfnt_poly_grid_ceil (x0) >> (16 - SFNT_POLY_SHIFT);
  right = sfnt_poly_grid_ceil (x1) >> (16 - SFNT_POLY_SHIFT);
#if 7 > __GNUC__
  start = raster->cells + row * raster->stride;
#else
  start = __builtin_assume_aligned ((raster->cells
				     + row * raster->stride),
				    SFNT_POLY_ALIGNMENT);
#endif
  start += left >> SFNT_POLY_SHIFT;

  /* Compute coverage for first pixel, then poly.  */
  if (left & SFNT_POLY_MASK)
    {
      w = 0;

      /* Note that col is an index into the columns of the coverage
	 map, unlike row which indexes the raster.  */
      col = 0;

      /* Precompute this to allow for better optimizations.  */
      end = ((left + SFNT_POLY_SAMPLE - 1) & ~SFNT_POLY_MASK);

      while (left < right && left < end)
	left++, w += coverage[col++];

      a = *start + w;
      *start++ = sfnt_saturate_short (a);
    }

  /* Clear coverage info for first pixel.  Compute coverage for center
     pixels.  */
  w = 0;
  for (col = 0; col < SFNT_POLY_SAMPLE; ++col)
    w += coverage[col];

  /* Fill pixels between left and right.  */
  while (left + SFNT_POLY_MASK < right)
    {
      a = *start + w;
      *start++ = sfnt_saturate_short (a);
      left += SFNT_POLY_SAMPLE;
    }

  /* Fill right pixel if necessary (because it has a fractional
     part.)  */
  if (right & SFNT_POLY_MASK)
    {
      w = 0;
      col = 0;
      while (left < right)
	left++, w += coverage[col++];
      a = *start + w;
      *start = sfnt_saturate_short (a);
    }

  /* All done.  */
}

/* Poly each span starting from START onto RASTER, at position Y.  Y
   here is still a cartesian coordinate, where the bottom of the
   raster is 0.  But that is no longer true by the time sfnt_span_fill
   is called.  */

static void
sfnt_poly_span (struct sfnt_edge *start, sfnt_fixed y,
		struct sfnt_raster *raster)
{
  struct sfnt_edge *edge;
  int winding;
  sfnt_fixed x0;

  /* Generate the X axis coverage map.  Then poly it onto RASTER.
     winding on each edge determines the winding direction: when it is
     positive, winding is 1.  When it is negative, winding is -1.  */

  winding = 0;

  for (edge = start; edge; edge = edge->next)
    {
      if (!winding)
	x0 = edge->x;
      else
	sfnt_fill_span (raster, (raster->height << 16) - y,
			x0, edge->x);

      winding += edge->winding;
    }
}



/* Main entry point for outline rasterization.  */

/* Raster the spans between START and its end to the raster specified
   as DCONTEXT.  The span's position is Y.  */

static void
sfnt_raster_span (struct sfnt_edge *start, sfnt_fixed y,
		  void *dcontext)
{
  sfnt_poly_span (start, y, dcontext);
}

/* Generate and poly each span in EDGES onto the raster specified as
   DCONTEXT.  */

static void
sfnt_raster_edge (struct sfnt_edge *edges, size_t num_edges,
		  void *dcontext)
{
  sfnt_poly_edges (edges, num_edges, sfnt_raster_span,
		   dcontext);
}

/* Generate an alpha mask for the glyph outline OUTLINE.  Value is the
   alpha mask upon success, NULL upon failure.  */

TEST_STATIC struct sfnt_raster *
sfnt_raster_glyph_outline (struct sfnt_glyph_outline *outline)
{
  struct sfnt_raster raster, *data;

  /* Get the raster parameters.  */
  sfnt_prepare_raster (&raster, outline);

  /* Allocate the raster data.  */
  data = xmalloc (sizeof *data + raster.stride * raster.height);
  *data = raster;
  data->cells = (unsigned char *) (data + 1);
  memset (data->cells, 0, raster.stride * raster.height);

  /* Generate edges for the outline, polying each array of edges to
     the raster.  */
  sfnt_build_outline_edges (outline, sfnt_raster_edge, data);

  /* All done.  */
  return data;
}



/* Glyph metrics computation.  */

/* Read an hmtx table from the font FD, using the table directory
   specified as SUBTABLE, the maxp table MAXP, and the hhea table
   HHEA.

   Return NULL upon failure, and the hmtx table otherwise.
   HHEA->num_of_long_hor_metrics determines the number of horizontal
   metrics present, and MAXP->num_glyphs -
   HHEA->num_of_long_hor_metrics determines the number of left-side
   bearings present.  */

TEST_STATIC struct sfnt_hmtx_table *
sfnt_read_hmtx_table (int fd, struct sfnt_offset_subtable *subtable,
		      struct sfnt_hhea_table *hhea,
		      struct sfnt_maxp_table *maxp)
{
  struct sfnt_table_directory *directory;
  struct sfnt_hmtx_table *hmtx;
  size_t size;
  ssize_t rc;
  int i;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_HMTX);

  if (!directory)
    return NULL;

  /* Figure out how many bytes are required.  */
  size = ((hhea->num_of_long_hor_metrics
	   * sizeof (struct sfnt_long_hor_metric))
	  + (MAX (0, ((int) maxp->num_glyphs
		      - hhea->num_of_long_hor_metrics))
	     * sizeof (int16_t)));

  /* Check the length matches exactly.  */
  if (directory->length != size)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Now allocate enough to hold all of that along with the table
     directory structure.  */

  hmtx = xmalloc (sizeof *hmtx + size);

  /* Read into hmtx + 1.  */
  rc = read (fd, hmtx + 1, size);
  if (rc < size)
    {
      xfree (hmtx);
      return NULL;
    }

  /* Set pointers to data.  */
  hmtx->h_metrics = (struct sfnt_long_hor_metric *) (hmtx + 1);
  hmtx->left_side_bearing
    = (int16_t *) (hmtx->h_metrics
		   + hhea->num_of_long_hor_metrics);

  /* Swap what was read.  */

  for (i = 0; i < hhea->num_of_long_hor_metrics; ++i)
    {
      sfnt_swap16 (&hmtx->h_metrics[i].advance_width);
      sfnt_swap16 (&hmtx->h_metrics[i].left_side_bearing);
    }

  for (; i < maxp->num_glyphs; ++i)
    sfnt_swap16 (&hmtx->left_side_bearing[i - hhea->num_of_long_hor_metrics]);

  /* All done.  */
  return hmtx;
}

/* Obtain glyph metrics for the glyph indiced by GLYPH at the
   specified PIXEL_SIZE.  Return 0 and the metrics in *METRICS if
   metrics could be found, else 1.

   HMTX, HHEA, HEAD and MAXP should be the hmtx, hhea, head, and maxp
   tables of the font respectively.  */

TEST_STATIC int
sfnt_lookup_glyph_metrics (sfnt_glyph glyph, int pixel_size,
			   struct sfnt_glyph_metrics *metrics,
			   struct sfnt_hmtx_table *hmtx,
			   struct sfnt_hhea_table *hhea,
			   struct sfnt_head_table *head,
			   struct sfnt_maxp_table *maxp)
{
  short lbearing;
  unsigned short advance;
  sfnt_fixed factor;

  if (glyph < hhea->num_of_long_hor_metrics)
    {
      /* There is a long entry in the hmtx table.  */
      lbearing = hmtx->h_metrics[glyph].left_side_bearing;
      advance = hmtx->h_metrics[glyph].advance_width;
    }
  else if (hhea->num_of_long_hor_metrics
	   && glyph < maxp->num_glyphs)
    {
      /* There is a short entry in the hmtx table.  */
      lbearing
	= hmtx->left_side_bearing[glyph
				  - hhea->num_of_long_hor_metrics];
      advance
	= hmtx->h_metrics[hhea->num_of_long_hor_metrics - 1].advance_width;
    }
  else
    /* No entry corresponds to the glyph.  */
    return 1;

  /* Now scale lbearing and advance up to the pixel size.  */
  factor = sfnt_div_fixed (pixel_size << 16,
			   head->units_per_em << 16);

  /* Save them.  */
  metrics->lbearing = sfnt_mul_fixed (lbearing << 16, factor);
  metrics->advance = sfnt_mul_fixed (advance << 16, factor);

  /* All done.  */
  return 0;
}



/* Font style parsing.  */

/* Read the name table from the given font FD, using the table
   directory specified as SUBTABLE.  Perform validation on the offsets
   in the name records.  Return NULL upon failure, else the name
   table.  */

TEST_STATIC struct sfnt_name_table *
sfnt_read_name_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_name_table *name;
  size_t required;
  ssize_t rc;
  int i;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_NAME);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out the minimum that has to be read.  */
  required = SFNT_ENDOF (struct sfnt_name_table,
			 string_offset, uint16_t);

  if (directory->length < required)
    return NULL;

  /* Allocate enough to hold the name table and variable length
     data.  */
  name = xmalloc (sizeof *name + directory->length);

  /* Read the fixed length data.  */
  rc = read (fd, name, required);
  if (rc < required)
    {
      xfree (name);
      return NULL;
    }

  /* Swap what was read.  */
  sfnt_swap16 (&name->format);
  sfnt_swap16 (&name->count);
  sfnt_swap16 (&name->string_offset);

  /* Reject unsupported formats.  */
  if (name->format)
    {
      xfree (name);
      return NULL;
    }

  /* Set the pointer to the start of the variable length data.  */
  name->name_records
    = (struct sfnt_name_record *) (name + 1);

  /* Check there is enough for the name records.  */
  required = directory->length - required;
  if (required < name->count * sizeof *name->name_records)
    {
      xfree (name);
      return NULL;
    }

  /* Read the variable length data.  First, read the name records.  */
  rc = read (fd, name->name_records,
	     (name->count
	      * sizeof *name->name_records));
  if (rc < (name->count
	    * sizeof *name->name_records))
    {
      xfree (name);
      return NULL;
    }

  /* Swap each of the name records.  */
  for (i = 0; i < name->count; ++i)
    {
      sfnt_swap16 (&name->name_records[i].platform_id);
      sfnt_swap16 (&name->name_records[i].platform_specific_id);
      sfnt_swap16 (&name->name_records[i].language_id);
      sfnt_swap16 (&name->name_records[i].name_id);
      sfnt_swap16 (&name->name_records[i].length);
      sfnt_swap16 (&name->name_records[i].offset);
    }

  /* Now, read the name data.  */

  if (name->string_offset > directory->length)
    {
      xfree (name);
      return NULL;
    }

  required = directory->length - name->string_offset;

  /* It can happen that the string offset comes before the name
     records, and as a result exceeds the number of bytes
     previously allocated.  Extend name if that is the case.  */

  if (required > (directory->length
		  - (name->count
		     * sizeof *name->name_records)))
    {
      name = xrealloc (name, (sizeof *name
			      + (name->count
				 * sizeof *name->name_records)
			      + required));
      name->name_records = (struct sfnt_name_record *) (name + 1);
    }

  /* There is enough space past name->name_records to hold REQUIRED
     bytes.  Seek to the right offset.  */

  if (lseek (fd, directory->offset + name->string_offset,
	     SEEK_SET) == (off_t) -1)
    {
      xfree (name);
      return NULL;
    }

  /* Read REQURIED bytes into the string data.  */
  name->data = (unsigned char *) (name->name_records
				  + name->count);
  rc = read (fd, name->data, required);
  if (rc < required)
    {
      xfree (name);
      return NULL;
    }

  /* Now validate each of the name records.  */
  for (i = 0; i < name->count; ++i)
    {
      if (((int) name->name_records[i].offset
	   + name->name_records[i].length) > required)
	{
	  /* The name is out of bounds! */
	  xfree (name);
	  return NULL;
	}
    }

  /* Return the name table.  */
  return name;
}

/* Return a pointer to the name data corresponding with CODE under the
   name table NAME.  Return the start of the data and the name record
   under *RECORD upon success, and NULL otherwise.  */

TEST_STATIC unsigned char *
sfnt_find_name (struct sfnt_name_table *name,
		enum sfnt_name_identifier_code code,
		struct sfnt_name_record *record)
{
  int i;

  for (i = 0; i < name->count; ++i)
    {
      if (name->name_records[i].name_id == code)
	{
	  /* The offsets within have already been validated.  */
	  *record = name->name_records[i];
	  return name->data + record->offset;
	}
    }

  return NULL;
}

/* Read the meta table from the give font FD, using the table
   directory specified as SUBTABLE.  Perform validation on the offsets
   in each metadata record.  Return NULL upon failure, else the meta
   table.  */

TEST_STATIC struct sfnt_meta_table *
sfnt_read_meta_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  struct sfnt_meta_table *meta;
  size_t required, i, data_size, map_size, offset;
  ssize_t rc;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_META);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out the minimum that has to be read.  */
  required = SFNT_ENDOF (struct sfnt_meta_table,
			 num_data_maps, uint32_t);

  if (directory->length < required)
    return NULL;

  /* Allocate enough to hold it.  */
  meta = xmalloc (sizeof *meta);

  /* Read the header.  */
  rc = read (fd, meta, required);
  if (rc < required)
    {
      xfree (meta);
      return NULL;
    }

  /* Swap what has been read so far.  */
  sfnt_swap32 (&meta->version);
  sfnt_swap32 (&meta->flags);
  sfnt_swap32 (&meta->data_offset);
  sfnt_swap32 (&meta->num_data_maps);

  /* Make sure the meta is supported.  */
  if (meta->version != 1)
    {
      xfree (meta);
      return NULL;
    }

  /* Reallocate the table to hold sizeof *meta + meta->num_data_maps
     times sizeof meta->data_maps + directory->length bytes.  This is
     because it is ok for metadata to point into the data map itself,
     so an unswapped copy of the whole meta contents must be
     retained.  */

  if (INT_MULTIPLY_WRAPV (sizeof *meta->data_maps, meta->num_data_maps,
			  &map_size)
      /* Do so while checking for overflow from bad sfnt files.  */
      || INT_ADD_WRAPV (map_size, sizeof *meta, &data_size)
      || INT_ADD_WRAPV (data_size, directory->length, &data_size))
    {
      xfree (meta);
      return NULL;
    }

  /* Do the reallocation.  */
  meta = xrealloc (meta, data_size);

  /* Check that the remaining data is big enough to hold the data
     maps.  */
  if (directory->length - required < map_size)
    {
      xfree (meta);
      return NULL;
    }

  /* Set pointers to data_maps and data.  */
  meta->data_maps = (struct sfnt_meta_data_map *) (meta + 1);
  meta->data = (unsigned char *) (meta->data_maps
				  + meta->num_data_maps);

  /* Now, seek back.  Read the entire table into meta->data.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    {
      xfree (meta);
      return NULL;
    }

  rc = read (fd, meta->data, directory->length);
  if (rc < directory->length)
    {
      xfree (meta);
      return NULL;
    }

  /* Copy the data maps into meta->data_maps and swap them one by
     one.  */
  memcpy (meta->data_maps, meta->data + required,
	  map_size);

  for (i = 0; i < meta->num_data_maps; ++i)
    {
      sfnt_swap32 (&meta->data_maps[i].tag);
      sfnt_swap32 (&meta->data_maps[i].data_offset);
      sfnt_swap32 (&meta->data_maps[i].data_length);

      /* Verify the data offsets.  Overflow checking is particularly
	 important here.  */

      if (INT_ADD_WRAPV (meta->data_maps[i].data_offset,
			 meta->data_maps[i].data_length,
			 &offset))
	{
	  xfree (meta);
	  return NULL;
	}

      if (offset > directory->length)
	{
	  xfree (meta);
	  return NULL;
	}
    }

  /* All done.  */
  return meta;
}

/* Return a pointer to the metadata corresponding to TAG under the
   meta table META.  Return the start of the data and the metadata map
   under *MAP upon success, and NULL otherwise.  */

MAYBE_UNUSED TEST_STATIC char *
sfnt_find_metadata (struct sfnt_meta_table *meta,
		    enum sfnt_meta_data_tag tag,
		    struct sfnt_meta_data_map *map)
{
  int i;

  for (i = 0; i < meta->num_data_maps; ++i)
    {
      if (meta->data_maps[i].tag == tag)
	{
	  *map = meta->data_maps[i];
	  return (char *) meta->data + map->data_offset;
	}
    }

  return NULL;
}



#ifdef TEST

struct sfnt_test_dcontext
{
  /* Context for sfnt_test_get_glyph.  */
  struct sfnt_glyf_table *glyf;
  struct sfnt_loca_table_short *loca_short;
  struct sfnt_loca_table_long *loca_long;
};

/* Global context for test functions.  Height of glyph.  */
static sfnt_fixed sfnt_test_max;

static void
sfnt_test_move_to (struct sfnt_point point, void *dcontext)
{
  printf ("move_to: %g, %g\n", sfnt_coerce_fixed (point.x),
	  sfnt_coerce_fixed (point.y));
}

static void
sfnt_test_line_to (struct sfnt_point point, void *dcontext)
{
  printf ("line_to: %g, %g\n", sfnt_coerce_fixed (point.x),
	  sfnt_coerce_fixed (point.y));
}

static void
sfnt_test_curve_to (struct sfnt_point control,
		    struct sfnt_point endpoint,
		    void *dcontext)
{
  printf ("curve_to: %g, %g - %g, %g\n",
	  sfnt_coerce_fixed (control.x),
	  sfnt_coerce_fixed (control.y),
	  sfnt_coerce_fixed (endpoint.x),
	  sfnt_coerce_fixed (endpoint.y));
}

static struct sfnt_glyph *
sfnt_test_get_glyph (sfnt_glyph glyph, void *dcontext,
		     bool *need_free)
{
  struct sfnt_test_dcontext *tables;

  tables = dcontext;
  *need_free = true;

  return sfnt_read_glyph (glyph, tables->glyf,
			  tables->loca_short,
			  tables->loca_long);
}

static void
sfnt_test_free_glyph (struct sfnt_glyph *glyph, void *dcontext)
{
  sfnt_free_glyph (glyph);
}

static void
sfnt_test_span (struct sfnt_edge *edge, sfnt_fixed y,
		void *dcontext)
{
#if 0
  printf ("/* span at %g */\n", sfnt_coerce_fixed (y));
  for (; edge; edge = edge->next)
    {
      if (y >= edge->bottom && y < edge->top)
	printf ("ctx.fillRect (%g, %g, 1, 1); "
		"/* %g top: %g bot: %g stepx: %g */\n",
		sfnt_coerce_fixed (edge->x),
		sfnt_coerce_fixed (sfnt_test_max - y),
		sfnt_coerce_fixed (y),
		sfnt_coerce_fixed (edge->bottom),
		sfnt_coerce_fixed (edge->top),
		sfnt_coerce_fixed (edge->step_x));
    }
#elif 0
  int winding;
  short x, dx;

  winding = 0;
  x = 0;

  for (; edge; edge = edge->next)
    {
      dx = (edge->x >> 16) - x;
      x = edge->x >> 16;

      for (; dx > 0; --dx)
	putc (winding ? '.' : ' ', stdout);

      winding = !winding;
    }

  putc ('\n', stdout);
#elif 0
  for (; edge; edge = edge->next)
    printf ("%g-", sfnt_coerce_fixed (edge->x));
  puts ("");
#endif
}

static void
sfnt_test_edge_ignore (struct sfnt_edge *edges, size_t num_edges,
		       void *dcontext)
{

}

static void
sfnt_test_edge (struct sfnt_edge *edges, size_t num_edges,
		void *dcontext)
{
  size_t i;

  printf ("built %zu edges\n", num_edges);

  for (i = 0; i < num_edges; ++i)
    {
      printf ("/* edge x, top, bot: %g, %g - %g.  winding: %d */\n"
	      "/* edge step_x: %g, source_x: %g (%d) */\n",
	      sfnt_coerce_fixed (edges[i].x),
	      sfnt_coerce_fixed (edges[i].top),
	      sfnt_coerce_fixed (edges[i].bottom),
	      edges[i].winding,
	      sfnt_coerce_fixed (edges[i].step_x),
	      sfnt_coerce_fixed (edges[i].source_x),
	      edges[i].source_x);
#ifdef TEST_VERTEX
      printf ("ctx.fillRect (%g, %g, 1, 1);\n",
	      sfnt_coerce_fixed (edges[i].x),
	      sfnt_coerce_fixed (sfnt_test_max
				 - edges[i].y));
#else
      printf ("ctx.fillRect (%g, %g, 1, 1);\n",
	      sfnt_coerce_fixed (edges[i].x),
	      sfnt_coerce_fixed (sfnt_test_max
				 - edges[i].bottom));
#endif
    }

  printf ("==end of edges==\n");

  sfnt_poly_edges (edges, num_edges, sfnt_test_span, NULL);
}

static void
sfnt_test_raster (struct sfnt_raster *raster)
{
  int x, y;

  for (y = 0; y < raster->height; ++y)
    {
      for (x = 0; x < raster->width; ++x)
	printf ("%3d ", (int) raster->cells[y * raster->stride + x]);
      puts ("");
    }
}

/* Simple tests that were used while developing this file.  By the
   time you are reading this, they probably no longer work.

   Compile like so in this directory:

    gcc -Demacs -I. -I. -I../lib -I../lib -MMD -MF deps/.d -MP
    -fno-common -Wall -Warith-conversion -Wdate-time
    -Wdisabled-optimization -Wdouble-promotion -Wduplicated-cond
    -Wextra -Wformat-signedness -Winit-self -Winvalid-pch -Wlogical-op
    -Wmissing-declarations -Wmissing-include-dirs -Wmissing-prototypes
    -Wnested-externs -Wnull-dereference -Wold-style-definition
    -Wopenmp-simd -Wpacked -Wpointer-arith -Wstrict-prototypes
    -Wsuggest-attribute=format -Wsuggest-final-methods
    -Wsuggest-final-types -Wtrampolines -Wuninitialized
    -Wunknown-pragmas -Wunused-macros -Wvariadic-macros
    -Wvector-operation-performance -Wwrite-strings -Warray-bounds=2
    -Wattribute-alias=2 -Wformat=2 -Wformat-truncation=2
    -Wimplicit-fallthrough=5 -Wshift-overflow=2 -Wuse-after-free=3
    -Wvla-larger-than=4031 -Wredundant-decls
    -Wno-missing-field-initializers -Wno-override-init
    -Wno-sign-compare -Wno-type-limits -Wno-unused-parameter
    -Wno-format-nonliteral -Wno-bidi-chars -g3 -O0 -DTEST sfnt.c -o
    sfnt ../lib/libgnu.a

   after gnulib has been built.  Then, run ./sfnt
   /path/to/font.ttf.  */

int
main (int argc, char **argv)
{
  struct sfnt_offset_subtable *font;
  struct sfnt_cmap_encoding_subtable *subtables;
  struct sfnt_cmap_encoding_subtable_data **data;
  struct sfnt_cmap_table *table;
  int fd, i;
  sfnt_char character;
  struct sfnt_head_table *head;
  struct sfnt_hhea_table *hhea;
  struct sfnt_loca_table_short *loca_short;
  struct sfnt_loca_table_long *loca_long;
  struct sfnt_glyf_table *glyf;
  struct sfnt_glyph *glyph;
  sfnt_glyph code;
  struct sfnt_test_dcontext dcontext;
  struct sfnt_glyph_outline *outline;
  struct timespec start, end, sub, sub1, sub2;
  static struct sfnt_maxp_table *maxp;
  struct sfnt_raster *raster;
  struct sfnt_hmtx_table *hmtx;
  struct sfnt_glyph_metrics metrics;
  struct sfnt_name_table *name;
  unsigned char *string;
  struct sfnt_name_record record;
  struct sfnt_meta_table *meta;

  if (argc != 2)
    return 1;

  fd = open (argv[1], O_RDONLY);

  if (fd < 1)
    return 1;

  font = sfnt_read_table_directory (fd);

  if (!font)
    {
      close (fd);
      return 1;
    }

  for (i = 0; i < font->num_tables; ++i)
    fprintf (stderr, "Found new subtable with tag %"PRIx32
	     " at offset %"PRIu32"\n",
	     font->subtables[i].tag,
	     font->subtables[i].offset);

  table = sfnt_read_cmap_table (fd, font, &subtables, &data);

  if (!table)
    {
      close (fd);
      xfree (font);
      return 1;
    }

  for (i = 0; i < table->num_subtables; ++i)
    {
      fprintf (stderr, "Found cmap table %"PRIu32": %p\n",
	       subtables[i].offset, data);

      if (data)
	fprintf (stderr, "  format: %"PRIu16"\n",
		 data[i]->format);
    }

  head = sfnt_read_head_table (fd, font);
  hhea = sfnt_read_hhea_table (fd, font);
  glyf = sfnt_read_glyf_table (fd, font);
  maxp = sfnt_read_maxp_table (fd, font);
  name = sfnt_read_name_table (fd, font);
  meta = sfnt_read_meta_table (fd, font);
  hmtx = NULL;

  if (hhea && maxp)
    hmtx = sfnt_read_hmtx_table (fd, font, hhea, maxp);

  if (maxp)
    fprintf (stderr, "maxp says num glyphs is %"PRIu16"\n",
	     maxp->num_glyphs);

  if (name)
    {
      fprintf (stderr, "name table of format: %"PRIu16" count: %"
	       PRIu16"\n", name->format, name->count);

      string = sfnt_find_name (name, SFNT_NAME_FONT_FAMILY,
			       &record);

      if (string)
	fprintf (stderr, "FONT_FAMILY: %"PRIu16", %"PRIu16"\n",
		 record.platform_id, record.length);
    }

  if (meta)
    fprintf (stderr, "meta table with count: %"PRIu32"\n",
	     meta->num_data_maps);

  loca_long = NULL;
  loca_short = NULL;

  if (head)
    {
      fprintf (stderr, "HEAD table:\n"
	       "version: \t\t\t%g\n"
	       "revision: \t\t\t%g\n"
	       "checksum_adjustment: \t\t%"PRIu32"\n"
	       "magic: \t\t\t\t%"PRIx32"\n"
	       "flags: \t\t\t\t%"PRIx16"\n"
	       "units_per_em: \t\t\t%"PRIx16"\n"
	       "xmin, ymin, xmax, ymax: \t%d, %d, %d, %d\n"
	       "mac_style: \t\t\t%"PRIx16"\n"
	       "lowest_rec_ppem: \t\t%"PRIu16"\n"
	       "font_direction_hint: \t\t%"PRIi16"\n"
	       "index_to_loc_format: \t\t%"PRIi16"\n"
	       "glyph_data_format: \t\t%"PRIi16"\n",
	       sfnt_coerce_fixed (head->version),
	       sfnt_coerce_fixed (head->revision),
	       head->checksum_adjustment,
	       head->magic,
	       head->flags,
	       head->units_per_em,
	       (int) head->xmin,
	       (int) head->ymin,
	       (int) head->xmax,
	       (int) head->ymax,
	       head->mac_style,
	       head->lowest_rec_ppem,
	       head->font_direction_hint,
	       head->index_to_loc_format,
	       head->glyph_data_format);

      if (head->index_to_loc_format)
	{
	  loca_long = sfnt_read_loca_table_long (fd, font);
	  if (!loca_long)
	    return 1;

	  fprintf (stderr, "long loca table has %zu glyphs\n",
		   loca_long->num_offsets);
	}
      else
	{
	  loca_short = sfnt_read_loca_table_short (fd, font);
	  if (!loca_short)
	    return 1;

	  fprintf (stderr, "short loca table has %zu glyphs\n",
		   loca_short->num_offsets);
	}
    }

  if (hhea)
    fprintf (stderr, "HHEA table:\n"
	     "version: \t\t\t%g\n"
	     "ascent, descent: \t\t%d %d\n"
	     "line_gap: \t\t\t%d\n"
	     "advance_width_max: \t\t%u\n"
	     "min_lsb: \t\t\t%d\n"
	     "min_rsb: \t\t\t%d\n"
	     "caret_srise: \t\t\t%d\n"
	     "caret_srun: \t\t\t%d\n",
	     sfnt_coerce_fixed (hhea->version),
	     (int) hhea->ascent,
	     (int) hhea->descent,
	     (int) hhea->line_gap,
	     (unsigned int) hhea->advance_width_max,
	     (int) hhea->min_left_side_bearing,
	     (int) hhea->min_right_side_bearing,
	     (int) hhea->caret_slope_rise,
	     (int) hhea->caret_slope_run);

  while (true)
    {
      printf ("table, character? ");

      if (scanf ("%d %"SCNu32"", &i, &character) == EOF)
	break;

      if (i >= table->num_subtables)
	{
	  printf ("table out of range\n");
	  continue;
	}

      if (!data[i])
	{
	  printf ("table not present\n");
	  continue;
	}

      code = sfnt_lookup_glyph (character, data[i]);
      printf ("glyph is %"PRIu32"\n", code);

      if ((loca_long || loca_short) && glyf)
	{
	  glyph = sfnt_read_glyph (code, glyf, loca_short,
				   loca_long);

	  if (glyph)
	    {
	      printf ("glyph is: %s\n",
		      glyph->simple ? "simple" : "compound");

	      dcontext.glyf = glyf;
	      dcontext.loca_short = loca_short;
	      dcontext.loca_long = loca_long;

	      if (sfnt_decompose_glyph (glyph, sfnt_test_move_to,
					sfnt_test_line_to,
					sfnt_test_curve_to,
				        sfnt_test_get_glyph,
					sfnt_test_free_glyph,
					&dcontext))
		printf ("decomposition failure\n");

	      /* Time this important bit.  */
	      clock_gettime (CLOCK_THREAD_CPUTIME_ID, &start);
	      outline = sfnt_build_glyph_outline (glyph, head,
						  12,
						  sfnt_test_get_glyph,
						  sfnt_test_free_glyph,
						  &dcontext);

	      clock_gettime (CLOCK_THREAD_CPUTIME_ID, &end);
	      sub = timespec_sub (end, start);
	      memset (&sub1, 0, sizeof sub1);

	      if (outline)
		{
		  sfnt_test_max = outline->ymax - outline->ymin;

		  for (i = 0; i < outline->outline_used; i++)
		    {
		      printf ("ctx.%s (%g, %g) /* %g, %g */\n",
			      (outline->outline[i].flags & SFNT_GLYPH_OUTLINE_LINETO
			       ? "lineTo" : "moveTo"),
			      sfnt_coerce_fixed (outline->outline[i].x
						 - outline->xmin),
			      sfnt_coerce_fixed (sfnt_test_max
						 - (outline->outline[i].y
						    - outline->ymin)),
			      sfnt_coerce_fixed (outline->outline[i].x
						 - outline->xmin),
			      sfnt_coerce_fixed (outline->outline[i].y
						 - outline->ymin));
		    }

		  clock_gettime (CLOCK_THREAD_CPUTIME_ID, &start);
		  sfnt_build_outline_edges (outline, sfnt_test_edge_ignore,
					    NULL);
		  clock_gettime (CLOCK_THREAD_CPUTIME_ID, &end);
		  sub1 = timespec_sub (end, start);

		  sfnt_build_outline_edges (outline, sfnt_test_edge,
					    NULL);

		  raster = NULL;

		  clock_gettime (CLOCK_THREAD_CPUTIME_ID, &start);

		  for (i = 0; i < 400; ++i)
		    {
		      xfree (raster);
		      raster = sfnt_raster_glyph_outline (outline);
		    }

		  clock_gettime (CLOCK_THREAD_CPUTIME_ID, &end);
		  sub2 = timespec_sub (end, start);

		  /* Print out the raster.  */
		  sfnt_test_raster (raster);
		  printf ("raster offsets: %d, %d\n",
			  raster->offx, raster->offy);

		  xfree (raster);

		  printf ("outline bounds: %g %g, %g %g\n",
			  sfnt_coerce_fixed (outline->xmin),
			  sfnt_coerce_fixed (outline->ymin),
			  sfnt_coerce_fixed (outline->xmax),
			  sfnt_coerce_fixed (outline->ymax));
		}

	      if (hmtx && head)
		{
		  if (!sfnt_lookup_glyph_metrics (code, 12,
						  &metrics,
						  hmtx, hhea,
						  head, maxp))
		    printf ("lbearing, advance: %g, %g\n",
			    sfnt_coerce_fixed (metrics.lbearing),
			    sfnt_coerce_fixed (metrics.advance));
		}

	      printf ("time spent outlining: %lld sec %ld nsec\n",
		      (long long) sub.tv_sec, sub.tv_nsec);
	      printf ("time spent building edges: %lld sec %ld nsec\n",
		      (long long) sub1.tv_sec, sub1.tv_nsec);
	      printf ("time spent rasterizing: %lld sec %ld nsec\n",
		      (long long) sub2.tv_sec / 400, sub2.tv_nsec / 400);

	      xfree (outline);
	    }

	  sfnt_free_glyph (glyph);
	}
    }

  xfree (font);

  for (i = 0; i < table->num_subtables; ++i)
    xfree (data[i]);

  xfree (table);
  xfree (data);
  xfree (subtables);
  xfree (head);
  xfree (hhea);
  xfree (loca_long);
  xfree (loca_short);
  xfree (glyf);
  xfree (maxp);
  xfree (hmtx);
  xfree (name);
  xfree (meta);

  return 0;
}

#endif
