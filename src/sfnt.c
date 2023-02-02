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
#include <setjmp.h>

#if defined __GNUC__ && !defined __clang__
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#endif

#ifdef TEST

#include <time.h>
#include <timespec.h>

static void *
xmalloc (size_t size)
{
  void *ptr;

  ptr = malloc (size);

  if (!ptr)
    abort ();

  return ptr;
}

static void *
xrealloc (void *ptr, size_t size)
{
  void *new_ptr;

  new_ptr = realloc (ptr, size);

  if (!new_ptr)
    abort ();

  return new_ptr;
}

static void
xfree (void *ptr)
{
  return free (ptr);
}

/* Use this for functions that are static while building in test mode,
   but are used outside as well.  */
#define TEST_STATIC static

/* Needed for tests.  */
#define ARRAYELTS(arr) (sizeof (arr) / sizeof (arr)[0])

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
    [SFNT_TABLE_CVT ] = 0x63767420,
    [SFNT_TABLE_FPGM] = 0x6670676d,
    [SFNT_TABLE_PREP] = 0x70726570,
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
   the start of the file (or an offset defined in the TTC header, if
   applicable), and must be seekable.  Return the table directory upon
   success, else NULL.

   Value is NULL upon failure, and the offset subtable upon success.
   If FD is actually a TrueType collection file, value is -1.  */

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
      if (rc >= sizeof (uint32_t))
	{
	  /* Detect a TTC file.  In that case, the first long will be
	     ``ttcf''.  */
	  sfnt_swap32 (&subtable->scaler_type);

	  if (subtable->scaler_type == SFNT_TTC_TTCF)
	    {
	      xfree (subtable);
	      return (struct sfnt_offset_subtable *) -1;
	    }
	}

      xfree (subtable);
      return NULL;
    }

  sfnt_swap32 (&subtable->scaler_type);

  /* Bail out early if this font is actually a TrueType collection
     file.  */

  if (subtable->scaler_type == SFNT_TTC_TTCF)
    {
      xfree (subtable);
      return (struct sfnt_offset_subtable *) -1;
    }

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
   the array of points of length NUM_COORDINATES given as X and Y.

   Also, apply the fixed point offsets X_OFF and Y_OFF to each X and Y
   coordinate.

   See sfnt_decompose_compound_glyph for an explanation of why offsets
   might be applied here, and not while reading the subglyph
   itself.  */

static void
sfnt_transform_coordinates (struct sfnt_compound_glyph_component *component,
			    sfnt_fixed *restrict x, sfnt_fixed *restrict y,
			    size_t num_coordinates,
			    sfnt_fixed x_off, sfnt_fixed y_off)
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
	  x[i] += x_off;
	  y[i] += y_off;
	}
    }
  else if (component->flags & 0100) /* WE_HAVE_AN_X_AND_Y_SCALE */
    {
      for (i = 0; i < num_coordinates; ++i)
	{
	  x[i] *= component->u.a.xscale / 16384.0;
	  y[i] *= component->u.a.yscale / 16384.0;
	  x[i] += x_off;
	  y[i] += y_off;
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
	  x[i] += x_off;
	  y[i] += y_off;
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
   contours.

   CONTEXT should be zeroed and put on the stack.  OFF_X and OFF_Y
   should be zero, as should RECURSION_COUNT.  GET_GLYPH and
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
  bool defer_offsets;

  /* Set up the base index.  This is the index from where on point
     renumbering starts.

     In other words, point 0 in this glyph will be 0 + base_index,
     point 1 will be 1 + base_index, and so on.  */
  base_index = context->num_points;

  /* Prevent infinite loops.  */
  if (recursion_count > 12)
    return 1;

  /* Don't defer offsets.  */
  defer_offsets = false;

  for (j = 0; j < glyph->compound->num_components; ++j)
    {
      /* Look up the associated subglyph.  */
      component = &glyph->compound->components[j];
      subglyph = get_glyph (component->glyph_index,
			    dcontext, &need_free);

      if (!subglyph)
	return -1;

      /* Record the size of the point array before expansion.  This
	 will be the base to apply to all points coming from this
	 subglyph.  */
      contour_start = context->num_points;

      /* Compute the offset for the component.  */
      if (component->flags & 02) /* ARGS_ARE_XY_VALUES */
	{
	  /* Component offsets are X/Y values as opposed to points
	     GLYPH.  */

	  if (!(component->flags & 01)) /* ARG_1_AND_2_ARE_WORDS */
	    {
	      /* X and Y are signed bytes.  */
	      x = component->argument1.b * 65536;
	      y = component->argument2.b * 65536;
	    }
	  else
	    {
	      /* X and Y are signed words.  */
	      x = component->argument1.d * 65536;
	      y = component->argument2.d * 65536;
	    }

	  /* If there is some kind of scale and component offsets are
	     scaled, then apply the transform to the offset.  */
	  if (component->flags & 04000) /* SCALED_COMPONENT_OFFSET */
	    sfnt_transform_coordinates (component, &x, &y, 1,
					0, 0);

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

	  /* Now, check that the anchor point specified lies inside
	     the glyph.  */

	  if (point >= contour_start)
	    {
	      if (need_free)
		free_glyph (subglyph, dcontext);

	      return 1;
	    }

	  if (!subglyph->compound)
	    {
	      if (point2 >= subglyph->simple->number_of_points)
		{
		  if (need_free)
		    free_glyph (subglyph, dcontext);

		  return 1;
		}

	      /* Get the points and use them to compute the offsets.  */
	      xtemp = context->x_coordinates[point];
	      ytemp = context->y_coordinates[point];
	      x = (xtemp - subglyph->simple->x_coordinates[point2]) * 65536;
	      y = (ytemp - subglyph->simple->y_coordinates[point2]) * 65536;
	    }
	  else
	    {
	      /* First, set offsets to 0, because it is not yet
		 possible to determine the position of the anchor
		 point in the child.  */
	      x = 0;
	      y = 0;

	      /* Set a flag which indicates that offsets must be
		 resolved from the child glyph after it is loaded, but
		 before it is incorporated into the parent glyph.  */
	      defer_offsets = true;
	    }
	}

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
		  x_base[i] = ((subglyph->simple->x_coordinates[i] * 65536)
			       + off_x + x);
		  y_base[i] = ((subglyph->simple->y_coordinates[i] * 65536)
			       + off_y + y);
		  flags_base[i] = subglyph->simple->flags[i];
		}

	      /* Apply the transform to the points.  */
	      sfnt_transform_coordinates (component, x_base, y_base,
					  last_point + 1, 0, 0);

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

	  /* When an anchor point is being used to translate the
	     glyph, and the subglyph in question is actually a
	     compound glyph, it is impossible to know which offset to
	     use until the compound subglyph has actually been
	     loaded.

	     As a result, the offset is calculated here, using the
	     points in the loaded child compound glyph.  But first, X
	     and Y must be reset to 0, as otherwise the translation
	     might be applied twice if defer_offsets is not set.  */

	  x = 0;
	  y = 0;

	  if (defer_offsets)
	    {
	      /* Renumber the non renumbered point2 to point into the
		 decomposed component.  */
	      point2 += contour_start;

	      /* Next, check that the non-renumbered point being
		 anchored lies inside the glyph data that was
		 decomposed.  */

	      if (point2 >= context->num_points)
		{
		  if (need_free)
		    free_glyph (subglyph, dcontext);

		  return 1;
		}

	      /* Get the points and use them to compute the
		 offsets.  */

	      xtemp = context->x_coordinates[point];
	      ytemp = context->y_coordinates[point];
	      x = (xtemp - context->x_coordinates[point2]);
	      y = (ytemp - context->y_coordinates[point2]);
	    }

	  sfnt_transform_coordinates (component,
				      context->x_coordinates + contour_start,
				      context->y_coordinates + contour_start,
				      contour_start - context->num_points,
				      x, y);
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
	  pen.x = glyph->simple->x_coordinates[here] * 65536;
	  pen.y = glyph->simple->y_coordinates[here] * 65536;
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
		  pen.x = glyph->simple->x_coordinates[here] * 65536;
		  pen.y = glyph->simple->y_coordinates[here] * 65536;

		  /* See if the last point was on the curve.  If it
		     wasn't, then curve from there to here.  */
		  if (!(glyph->simple->flags[here - 1] & 01))
		    {
		      control1.x
			= glyph->simple->x_coordinates[here - 1] * 65536;
		      control1.y
			= glyph->simple->y_coordinates[here - 1] * 65536;
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
		  control1.x = glyph->simple->x_coordinates[here - 1] * 65536;
		  control1.y = glyph->simple->y_coordinates[here - 1] * 65536;
		  control2.x = glyph->simple->x_coordinates[here] * 65536;
		  control2.y = glyph->simple->y_coordinates[here] * 65536;
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
		  pen.x = glyph->simple->x_coordinates[start] * 65536;
		  pen.y = glyph->simple->y_coordinates[start] * 65536;

		  /* See if the last point (in this case, `here') was
		     on the curve.  If it wasn't, then curve from
		     there to here.  */
		  if (!(glyph->simple->flags[here] & 01))
		    {
		      control1.x
			= glyph->simple->x_coordinates[here] * 65536;
		      control1.y
			= glyph->simple->y_coordinates[here] * 65536;
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

#ifndef INT64_MAX

/* 64 bit integer type.  */

struct sfnt_large_integer
{
  unsigned int high, low;
};

/* Calculate (A * B), placing the result in *VALUE.  */

static void
sfnt_multiply_divide_1 (unsigned int a, unsigned int b,
			struct sfnt_large_integer *value)
{
  unsigned int lo1, hi1, lo2, hi2, lo, hi, i1, i2;

  lo1 = a & 0x0000ffffu;
  hi1 = a >> 16;
  lo2 = b & 0x0000ffffu;
  hi2 = b >> 16;

  lo = lo1 * lo2;
  i1 = lo1 * hi2;
  i2 = lo2 * hi1;
  hi = hi1 * hi2;

  /* Check carry overflow of i1 + i2.  */
  i1 += i2;
  hi += (unsigned int) (i1 < i2) << 16;

  hi += i1 >> 16;
  i1  = i1 << 16;

  /* Check carry overflow of i1 + lo.  */
  lo += i1;
  hi += (lo < i1);

  value->low = lo;
  value->high = hi;
}

/* Count the number of most significant zero bits in N.  */

static unsigned int
sfnt_count_leading_zero_bits (unsigned int n)
{
  int shift;

  shift = 0;

  if (n & 0xffff0000ul)
    {
      n >>= 16;
      shift += 16;
    }

  if (n & 0x0000ff00ul)
    {
      n >>= 8;
      shift += 8;
    }

  if (n & 0x000000f0ul)
    {
      n >>= 4;
      shift += 4;
    }

  if (n & 0x0000000cul)
    {
      n >>= 2;
      shift += 2;
    }

  if (n & 0x00000002ul)
    shift += 1;

  return shift;
}

/* Calculate AB / C.  Value is a 32 bit unsigned integer.  */

static unsigned int
sfnt_multiply_divide_2 (struct sfnt_large_integer *ab,
			unsigned int c)
{
  unsigned int hi, lo;
  int i;
  unsigned int r, q; /* Remainder and quotient.  */

  hi = ab->high;
  lo = ab->low;

  i = 31 - sfnt_count_leading_zero_bits (hi);
  r = (hi << i) | (lo >> (32 - i));
  lo <<= i;
  q = r / c;
  r -= q * c;
  i = 32 - i;

  do
    {
      q <<= 1;
      r = (r << 1) | (lo >> 31);
      lo <<= 1;

      if (r >= c)
	{
	  r -= c;
	  q |= 1;
	}
    }
  while (--i);

  return q;
}

#endif

/* Calculate (A * B) / C with no rounding and return the result, using
   a 64 bit integer if necessary.  */

static unsigned int
sfnt_multiply_divide (unsigned int a, unsigned int b, unsigned int c)
{
#ifndef INT64_MAX
  struct sfnt_large_integer temp;

  sfnt_multiply_divide_1 (a, b, &temp);
  return sfnt_multiply_divide_2 (&temp, c);
#else
  uint64_t temp;

  temp = (uint64_t) a * (uint64_t) b;
  return temp / c;
#endif
}

/* The same as sfnt_multiply_divide, but handle signed values
   instead.  */

static MAYBE_UNUSED int
sfnt_multiply_divide_signed (int a, int b, int c)
{
  int sign;

  sign = 1;

  if (a < 0)
    sign = -sign;

  if (b < 0)
    sign = -sign;

  if (c < 0)
    sign = -sign;

  return (sfnt_multiply_divide (abs (a), abs (b), abs (c))
	  * sign);
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
  return product / (int64_t) 65536;
#else
  int sign;

  sign = 1;

  if (x < 0)
    sign = -sign;

  if (y < 0)
    sign = -sign;

  return sfnt_multiply_divide (abs (x), abs (y),
			       65536) * sign;
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

  result = ((int64_t) x * 65536) / y;

  return result;
#else
  int sign;
  unsigned int a, b;

  sign = 1;

  if (x < 0)
    sign = -sign;

  if (y < 0)
    sign = -sign;

  a = abs (x);
  b = abs (y);

  return sfnt_multiply_divide (a, 65536, b) * sign;
#endif
}

/* Return the ceiling value of the specified fixed point number X.  */

static sfnt_fixed
sfnt_ceil_fixed (sfnt_fixed x)
{
  return (x + 0177777) & 037777600000;
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

       PIXEL = UNIT * PPEM / UPEM  */

  build_outline_context.factor
    = sfnt_div_fixed (pixel_size * 65536,
		      head->units_per_em * 65536);

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
   edges' X coordinates.  Repeat until all the edges have been polyed.

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

   If PIXEL_SIZE is -1, do not perform any scaling on the glyph
   metrics.

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

  if (pixel_size == -1)
    {
      /* Return unscaled metrics in this case.  */
      metrics->lbearing = lbearing;
      metrics->advance = advance;
      return 1;
    }

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

/* Read the meta table from the given font FD, using the table
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



/* TrueType collection format support.  */

/* Read a TrueType collection header from the font file FD.
   FD must currently at the start of the file.

   Value is the header upon success, else NULL.  */

TEST_STATIC struct sfnt_ttc_header *
sfnt_read_ttc_header (int fd)
{
  struct sfnt_ttc_header *ttc;
  size_t size, i;
  ssize_t rc;

  /* First, allocate only as much as required.  */

  ttc = xmalloc (sizeof *ttc);

  /* Read the version 1.0 data.  */

  size = SFNT_ENDOF (struct sfnt_ttc_header, num_fonts,
		     uint32_t);
  rc = read (fd, ttc, size);
  if (rc < size)
    {
      xfree (ttc);
      return NULL;
    }

  /* Now swap what was read.  */
  sfnt_swap32 (&ttc->ttctag);
  sfnt_swap32 (&ttc->version);
  sfnt_swap32 (&ttc->num_fonts);

  /* Verify that the tag is as expected.  */
  if (ttc->ttctag != SFNT_TTC_TTCF)
    {
      xfree (ttc);
      return NULL;
    }

  /* Now, read the variable length data.  Make sure to check for
     overflow.  */

  if (INT_MULTIPLY_WRAPV (ttc->num_fonts,
			  sizeof *ttc->offset_table,
			  &size))
    {
      xfree (ttc);
      return NULL;
    }

  ttc = xrealloc (ttc, sizeof *ttc + size);
  ttc->offset_table = (uint32_t *) (ttc + 1);
  rc = read (fd, ttc->offset_table, size);
  if (rc < size)
    {
      xfree (ttc);
      return NULL;
    }

  /* Swap each of the offsets read.  */
  for (i = 0; i < ttc->num_fonts; ++i)
    sfnt_swap32 (&ttc->offset_table[i]);

  /* Now, look at the version.  If it is earlier than 2.0, then
     reading is finished.  */

  if (ttc->version < 0x00020000)
    return ttc;

  /* If it is 2.0 or later, then continue to read ul_dsig_tag to
     ul_dsig_offset.  */

  size = (SFNT_ENDOF (struct sfnt_ttc_header, ul_dsig_offset,
		      uint32_t)
	  - offsetof (struct sfnt_ttc_header, ul_dsig_tag));
  rc = read (fd, &ttc->ul_dsig_offset, size);
  if (rc < size)
    {
      xfree (ttc);
      return NULL;
    }

  /* Swap what was read.  */
  sfnt_swap32 (&ttc->ul_dsig_tag);
  sfnt_swap32 (&ttc->ul_dsig_length);
  sfnt_swap32 (&ttc->ul_dsig_offset);

  /* All done.  */
  return ttc;
}



#ifdef TEST
#define SFNT_ENABLE_HINTING
#endif

#ifdef SFNT_ENABLE_HINTING

/* TrueType hinting support.

   If you do not read the code in this section in conjunction with
   Apple's TrueType Reference Manual, you will get very confused!

   TrueType fonts don't provide simple hinting meta data, unlike Type
   2 or CFF fonts.

   Instead, they come with a ``font program'', a bytecode program
   which is executed upon loading the font, a ``control value
   program'', executed upon font metrics changing, and then a ``glyph
   program'' for each glyph, which is run to fit its glyph after
   scaling.

   The virtual machine which runs this bytecode is arranged as such:

   Firstly, there is a set of registers known as the ``graphics
   state''.  Each time the point size of a font changes, the ``control
   value program'' is run to establish the default values of the
   ``graphics state''.  Then, before each glyph program is run, the
   ``graphics state'' is set back to the default values.

   Secondly, there is an address space which contains all instructions
   being run for the current program, which is addressed by the
   interpreter through its program counter and also by the
   instructions which push data on to the stack.

   Thirdly, there is a single stack, from which most instructions take
   their operands and store data.

   Then, there is some memory set aside for each font, the ``storage
   area'', which is addressed through the RS[] and WS[] instructions,
   and a ``control value table'', which is the `cvt ' table of the
   font.

   And finally, there is a ``glyph zone'' which holds points from a
   scaled glyph outline, and a ``twilight zone'', which holds points
   used by the font program itself.  Both are addressed indirectly
   through one of three ``zone pointer'' registers, and are accessible
   only when a program is being run on behalf of a glyph.  */

/* Structure definitions for tables used by the TrueType
   interpreter.  */

struct sfnt_cvt_table
{
  /* Number of elements in the control value table.  */
  size_t num_elements;

  /* Pointer to elements in the control value table.  */
  sfnt_fword *values;
};

struct sfnt_fpgm_table
{
  /* Number of instructions in the font program table.  */
  size_t num_instructions;

  /* Pointer to elements in the font program table.  */
  unsigned char *instructions;
};

struct sfnt_prep_table
{
  /* Number of instructions in the control value program (pre-program)
     table.  */
  size_t num_instructions;

  /* Pointer to elements in the preprogram table.  */
  unsigned char *instructions;
};



/* Functions for reading tables used by the TrueType interpreter.  */

/* Read the cvt table (control value table) from the given font FD,
   using the table directory specified as SUBTABLE.  Swap all values
   in the control value table.  Return NULL upon failure, else the cvt
   table.  */

static struct sfnt_cvt_table *
sfnt_read_cvt_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  size_t required, i;
  ssize_t rc;
  struct sfnt_cvt_table *cvt;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_CVT );

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out the minimum amount that has to be read.  */
  if (INT_ADD_WRAPV (sizeof *cvt, directory->length, &required))
    return NULL;

  /* Allocate enough for that much data.  */
  cvt = xmalloc (required);

  /* Now set cvt->num_elements as appropriate, and make cvt->values
     point into the values.  */
  cvt->num_elements = directory->length / 2;
  cvt->values = (sfnt_fword *) (cvt + 1);

  /* Read into cvt.  */
  rc = read (fd, cvt->values, directory->length);
  if (rc != directory->length)
    {
      xfree (cvt);
      return NULL;
    }

  /* Swap each element in the control value table.  */
  for (i = 0; i < cvt->num_elements; ++i)
    sfnt_swap16 (&cvt->values[i]);

  /* All done.  */
  return cvt;
}

/* Read the fpgm table from the given font FD, using the table
   directory specified as SUBTABLE.  Value is NULL upon failure, else
   the fpgm table.  */

static struct sfnt_fpgm_table *
sfnt_read_fpgm_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  size_t required;
  ssize_t rc;
  struct sfnt_fpgm_table *fpgm;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_FPGM);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out the minimum amount that has to be read.  */
  if (INT_ADD_WRAPV (sizeof *fpgm, directory->length, &required))
    return NULL;

  /* Allocate enough for that much data.  */
  fpgm = xmalloc (sizeof *fpgm + directory->length);

  /* Now set fpgm->num_instructions as appropriate, and make
     fpgm->instructions point to the right place.  */

  fpgm->num_instructions = directory->length;
  fpgm->instructions = (unsigned char *) (fpgm + 1);

  /* Read into fpgm.  */
  rc = read (fd, fpgm->instructions, directory->length);
  if (rc != directory->length)
    {
      xfree (fpgm);
      return NULL;
    }

  /* All done.  */
  return fpgm;
}

/* Read the prep table from the given font FD, using the table
   directory specified as SUBTABLE.  Value is NULL upon failure, else
   the prep table.  */

static struct sfnt_prep_table *
sfnt_read_prep_table (int fd, struct sfnt_offset_subtable *subtable)
{
  struct sfnt_table_directory *directory;
  size_t required;
  ssize_t rc;
  struct sfnt_prep_table *prep;

  /* Find the table in the directory.  */

  directory = sfnt_find_table (subtable, SFNT_TABLE_PREP);

  if (!directory)
    return NULL;

  /* Seek to the location given in the directory.  */
  if (lseek (fd, directory->offset, SEEK_SET) == (off_t) -1)
    return NULL;

  /* Figure out the minimum amount that has to be read.  */
  if (INT_ADD_WRAPV (sizeof *prep, directory->length, &required))
    return NULL;

  /* Allocate enough for that much data.  */
  prep = xmalloc (sizeof *prep + directory->length);

  /* Now set prep->num_instructions as appropriate, and make
     prep->instructions point to the right place.  */

  prep->num_instructions = directory->length;
  prep->instructions = (unsigned char *) (prep + 1);

  /* Read into prep.  */
  rc = read (fd, prep->instructions, directory->length);
  if (rc != directory->length)
    {
      xfree (prep);
      return NULL;
    }

  /* All done.  */
  return prep;
}



/* Interpreter execution environment.  */

/* 26.6 fixed point type used within the interpreter.  */
typedef int32_t sfnt_f26dot6;

/* 2.14 fixed point type used to represent versors of unit
   vectors.  */
typedef int16_t sfnt_f2dot14;

/* 18.14 fixed point type used to calculate rounding details.  */
typedef int32_t sfnt_f18dot14;

struct sfnt_unit_vector
{
  /* X and Y versors of the 2d unit vector.  */
  sfnt_f2dot14 x, y;
};

struct sfnt_interpreter_definition
{
  /* The opcode of this instruction or function.  */
  uint16_t opcode;

  /* The number of instructions.  */
  uint16_t instruction_count;

  /* Pointer to instructions belonging to the definition.  This
     pointer points directly into the control value or font program.
     Make sure both programs are kept around as long as the
     interpreter continues to exist.  */
  unsigned char *instructions;
};

/* This structure represents a ``struct sfnt_glyph'' that has been
   scaled to a given pixel size.

   It can either contain a simple glyph, or a decomposed compound
   glyph; instructions are interpreted for both simple glyphs, simple
   glyph components inside a compound glyph, and compound glyphs as a
   whole.

   In addition to the glyph data itself, it also records various
   information for the instruction interpretation process:

     - ``current'' point coordinates, which have been modified
       by the instructing process.

     - two phantom points at the origin and the advance of the
       glyph.  */

struct sfnt_interpreter_zone
{
  /* The number of points in this zone, including the two phantom
     points at the end.  */
  size_t num_points;

  /* The number of contours in this zone.  */
  size_t num_contours;

  /* The end points of each contour.  */
  size_t *contour_end_points;

  /* Pointer to the X axis point data.  */
  sfnt_f26dot6 *restrict x_points;

  /* Pointer to the Y axis point data.  */
  sfnt_f26dot6 *restrict y_points;

  /* Pointer to the X axis current point data.  */
  sfnt_f26dot6 *restrict x_current;

  /* Pointer to the Y axis current point data.  */
  sfnt_f26dot6 *restrict y_current;

  /* Pointer to the flags associated with this data.  */
  unsigned char *flags;
};

enum
  {
    /* Bits 7 and 6 of a glyph point's flags is reserved.  This scaler
       uses it to mean that the point has been touched in one axis or
       another.  */
    SFNT_POINT_TOUCHED_X = (1 << 7),
    SFNT_POINT_TOUCHED_Y = (1 << 6),
    SFNT_POINT_TOUCHED_BOTH = (SFNT_POINT_TOUCHED_X
			       | SFNT_POINT_TOUCHED_Y),
  };

/* This is needed because `round' below needs an interpreter
   argument.  */
struct sfnt_interpreter;

struct sfnt_graphics_state
{
  /* Pointer to the function used for rounding.  This function is
     asymmetric, so -0.5 rounds up to 0, not -1.  It is up to the
     caller to handle negative values.

     Value is undefined unless sfnt_validate_gs has been called, and
     the second argument may be used to provide detailed rounding
     information (``super rounding state''.)  */
  sfnt_f26dot6 (*round) (sfnt_f26dot6, struct sfnt_interpreter *);

  /* Pointer to the function used to project euclidean vectors onto
     the projection vector.  Value is the magnitude of the projected
     vector.  */
  sfnt_f26dot6 (*project) (sfnt_f26dot6, sfnt_f26dot6,
			   struct sfnt_interpreter *);

  /* Pointer to the function used to move a specified point
     along the freedom vector by a distance specified in terms
     of the projection vector.  */
  void (*move) (sfnt_f26dot6 *, sfnt_f26dot6 *,
		struct sfnt_interpreter *,
		sfnt_f26dot6, unsigned char *);

  /* Dot product between the freedom and the projection vectors.  */
  sfnt_f2dot14 vector_dot_product;

  /* Controls whether the sign of control value table entries will be
     changed to match the sign of the actual distance measurement with
     which it is compared.  Setting auto flip to TRUE makes it
     possible to control distances measured with or against the
     projection vector with a single control value table entry. When
     auto flip is set to FALSE, distances must be measured with the
     projection vector.  */
  bool auto_flip;

  /* Limits the regularizing effects of control value table entries to
     cases where the difference between the table value and the
     measurement taken from the original outline is sufficiently
     small.  */
  sfnt_f26dot6 cvt_cut_in;

  /* Establishes the base value used to calculate the range of point
     sizes to which a given DELTAC[] or DELTAP[] instruction will
     apply.  The formulas given below are used to calculate the range
     of the various DELTA instructions.

     DELTAC1 DELTAP1 (delta_base) through (delta_base + 15)
     DELTAC2 DELTAP2 (delta_base + 16) through (delta_base + 31)
     DELTAC3 DELTAP3 (delta_base + 32) through (delta_base + 47)

     Please keep this documentation in sync with the TrueType
     reference manual.  */
  unsigned short delta_base;

  /* Determines the range of movement and smallest magnitude of
     movement (the step) in a DELTAC[] or DELTAP[] instruction.
     Changing the value of the delta shift makes it possible to trade
     off fine control of point movement for range of movement.  A low
     delta shift favors range of movement over fine control.  A high
     delta shift favors fine control over range of movement.  The step
     has the value 1/2 to the power delta shift.  The range of
     movement is calculated by taking the number of steps allowed (16)
     and multiplying it by the step.

     The legal range for delta shift is zero through six.  Negative
     values are illegal.  */
  unsigned short delta_shift;

  /* A second projection vector set to a line defined by the original
     outline location of two points.  The dual projection vector is
     used when it is necessary to measure distances from the scaled
     outline before any instructions were executed.  */
  struct sfnt_unit_vector dual_projection_vector;

  /* A unit vector that establishes an axis along which points can
     move.  */
  struct sfnt_unit_vector freedom_vector;

  /* Makes it possible to turn off instructions under some
     circumstances.  When flag 1 is set, changes to the graphics state
     made in the control value program will be ignored.  When flag is
     1, grid fitting instructions will be ignored.  */
  unsigned char instruct_control;

  /* Makes it possible to repeat certain instructions a designated
     number of times.  The default value of one assures that unless
     the value of loop is altered, these instructions will execute one
     time.  */
  unsigned short loop;

  /* Establishes the smallest possible value to which a distance will
     be rounded.  */
  sfnt_f26dot6 minimum_distance;

  /* A unit vector whose direction establishes an axis along which
     distances are measured.  */
  struct sfnt_unit_vector projection_vector;

  /* Determines the manner in which values are rounded. Can be set to
     a number of predefined states or to a customized state with the
     SROUND or S45ROUND instructions.  */
  int round_state;

  /* Reference points.  These reference point numbers, which together
     with a zone designation, specify a point in either the glyph zone
     or the twilight zone.  */
  uint16_t rp0, rp1, rp2;

  /* Flags which determine whether the interpreter will activate
     dropout control for the current glyph.  */
  int scan_control;

  /* The distance difference below which the interpreter will replace
     a CVT distance or an actual distance in favor of the single width
     value.  */
  sfnt_f26dot6 sw_cut_in;

  /* The value used in place of the control value table distance or
     the actual distance value when the difference between that
     distance and the single width value is less than the single width
     cut-in.  */
  sfnt_f26dot6 single_width_value;

  /* Zone pointers, which reference a zone.  */
  int zp0, zp1, zp2;
};

struct sfnt_interpreter
{
  /* The number of elements in the stack.  */
  uint16_t max_stack_elements;

  /* The number of instructions in INSTRUCTIONS.  */
  uint16_t num_instructions;

  /* Size of the storage area.  */
  uint16_t storage_size;

  /* Size of the function definition area.  */
  uint16_t function_defs_size;

  /* Size of the instruction definition area.  */
  uint16_t instruction_defs_size;

  /* Size of the twilight zone.  */
  uint16_t twilight_zone_size;

  /* The instruction pointer.  This points to the instruction
     currently being executed.  */
  int IP;

  /* The current scale.  */
  sfnt_fixed scale;

  /* The current ppem and point size.  */
  int ppem, point_size;

  /* The execution stack.  This has at most max_stack_elements
     elements.  */
  uint32_t *stack;

  /* Pointer past the top of the stack.  */
  uint32_t *SP;

  /* The size of the control value table.  */
  size_t cvt_size;

  /* Pointer to instructions currently being executed.  */
  unsigned char *instructions;

  /* The twilight zone.  May not be NULL.  */
  sfnt_f26dot6 *twilight_x, *twilight_y;

  /* The scaled outlines being manipulated.  May be NULL.  */
  struct sfnt_interpreter_zone *glyph_zone;

  /* The glyph advance width.  Value is undefined unless GLYPH_ZONE is
     set.  */
  sfnt_f26dot6 advance_width;

  /* The storage area.  */
  uint32_t *storage;

  /* Control value table values.  */
  sfnt_f26dot6 *cvt;

  /* Function definitions.  */
  struct sfnt_interpreter_definition *function_defs;

  /* Instruction definitions.  */
  struct sfnt_interpreter_definition *instruction_defs;

  /* Interpreter registers.  */
  struct sfnt_graphics_state state;

  /* Detailed rounding state used when state.round_state indicates
     that fine grained rounding should be used.

     PERIOD says how often a round value occurs, for numbers
     increasing from PHASE to infinity.

     THRESHOLD says when to round a value between two increasing
     periods towards the larger period.  */
  sfnt_f26dot6 period, phase, threshold;

  /* The depth of any ongoing calls.  */
  int call_depth;

  /* Jump buffer for traps.  */
  jmp_buf trap;

  /* What was the trap.  */
  const char *trap_reason;
};

/* Divide the specified two 26.6 fixed point numbers X and Y.
   Return the result.  */

static sfnt_f26dot6
sfnt_div_f26dot6 (sfnt_f26dot6 x, sfnt_f26dot6 y)
{
#ifdef INT64_MAX
  int64_t result;

  result = ((int64_t) x * 64) / y;

  return result;
#else
  int sign;
  unsigned int a, b;

  sign = 1;

  if (x < 0)
    sign = -sign;

  if (y < 0)
    sign = -sign;

  a = abs (x);
  b = abs (y);

  return sfnt_multiply_divide (a, 64, b) * sign;
#endif
}

/* Multiply-round the specified two 26.6 fixed point numbers A and B.
   Return the result, or an undefined value upon overflow.  */

static sfnt_f26dot6
sfnt_mul_f26dot6 (sfnt_f26dot6 a, sfnt_f26dot6 b)
{
#ifdef INT64_MAX
  int64_t product;

  product = (int64_t) a * (int64_t) b;

  /* This can be done quickly with int64_t.  */
  return product / (int64_t) 64;
#else
  int sign;

  sign = 1;

  if (a < 0)
    sign = -sign;

  if (b < 0)
    sign = -sign;

  return sfnt_multiply_divide (abs (a), abs (b),
			       64) * sign;
#endif
}

/* Multiply the specified 26.6 fixed point number X by the specified
   16.16 fixed point number Y.

   The 26.6 fixed point number must fit inside -32768 to 32767.ffff.
   Value is otherwise undefined.  */

static sfnt_f26dot6
sfnt_mul_f26dot6_fixed (sfnt_f26dot6 x, sfnt_fixed y)
{
  sfnt_fixed result;

  result = sfnt_mul_fixed (y, x);
  return result;
}

/* Return the floor of the specified 26.6 fixed point value X.  */

static sfnt_f26dot6
sfnt_floor_f26dot6 (sfnt_f26dot6 x)
{
  return x & 037777777700;
}

/* Return the ceiling of the specified 26.6 fixed point value X.  */

static sfnt_f26dot6
sfnt_ceil_f26dot6 (sfnt_f26dot6 x)
{
  return (x + 077) & ~077;
}

/* Return the 26.6 fixed point value X rounded to the nearest integer
   value.  */

static sfnt_f26dot6
sfnt_round_f26dot6 (sfnt_f26dot6 x)
{
  /* Add 0.5.  */
  x += 040;

  /* Remove the fractional.  */
  return x & ~077;
}

/* Needed by sfnt_init_graphics_state.  */

static void sfnt_validate_gs (struct sfnt_graphics_state *);

/* Set up default values for the interpreter graphics state.  Return
   them in STATE.  */

static void
sfnt_init_graphics_state (struct sfnt_graphics_state *state)
{
  state->auto_flip = true;
  state->cvt_cut_in = 0104;
  state->delta_base = 9;
  state->delta_shift = 3;
  state->dual_projection_vector.x = 040000; /* 1.0 */
  state->dual_projection_vector.y = 0;
  state->freedom_vector.x = 040000; /* 1.0 */
  state->freedom_vector.y = 0;
  state->instruct_control = 0;
  state->loop = 1;
  state->minimum_distance = 0100;
  state->projection_vector.x = 040000; /* 1.0 */
  state->projection_vector.y = 0;
  state->round_state = 1;
  state->rp0 = 0;
  state->rp1 = 0;
  state->rp2 = 0;
  state->scan_control = 0;
  state->sw_cut_in = 0;
  state->single_width_value = 0;
  state->zp0 = 1;
  state->zp1 = 1;
  state->zp2 = 1;

  /* Validate the graphics state.  */
  sfnt_validate_gs (state);
}

/* Set up an interpreter to be used with a font.  Use the resource
   limits specified in the MAXP table, the values specified in the CVT
   and HEAD tables, the pixel size PIXEL_SIZE, and the point size
   POINT_SIZE.  CVT may be NULL, in which case the interpreter will
   not have access to a control value table.

   POINT_SIZE should be PIXEL_SIZE, converted to 1/72ths of an inch.

   Value is the interpreter, with all state initialized to default
   values, or NULL upon failure.  */

static struct sfnt_interpreter *
sfnt_make_interpreter (struct sfnt_maxp_table *maxp,
		       struct sfnt_cvt_table *cvt,
		       struct sfnt_head_table *head,
		       int pixel_size, int point_size)
{
  size_t size, temp, i, storage_size, pad;
  struct sfnt_interpreter *interpreter;

  /* Detect CFF maxp tables.  */
  if (maxp->version != 0x00010000)
    return NULL;

  /* Use the contents of the MAXP table to determine the size of the
     interpreter structure.  */
  size = sizeof (*interpreter);

  /* Add program stack.  */
  if (INT_ADD_WRAPV ((maxp->max_stack_elements
		      * sizeof *interpreter->stack),
		     size, &size))
    return NULL;

  /* Add twilight zone.  */

  if (INT_ADD_WRAPV ((maxp->max_twilight_points
		      * sizeof *interpreter->twilight_x),
		     size, &size))
    return NULL;

  if (INT_ADD_WRAPV ((maxp->max_twilight_points
		      * sizeof *interpreter->twilight_y),
		     size, &size))
    return NULL;

  /* Add the storage area.  */
  storage_size = maxp->max_storage * sizeof *interpreter->storage;
  if (INT_ADD_WRAPV (storage_size, size, &size))
    return NULL;

  /* Add padding for the storage area.  */
  pad = alignof (struct sfnt_interpreter_definition);
  pad -= size & (pad - 1);
  if (INT_ADD_WRAPV (pad, size, &size))
    return NULL;

  /* Add function and instruction definitions.  */
  if (INT_ADD_WRAPV ((((int) maxp->max_instruction_defs
		       + maxp->max_function_defs)
		      * sizeof *interpreter->function_defs),
		     size, &size))
    return NULL;

  /* Add control value table.  */

  if (cvt)
    {
      if (INT_MULTIPLY_WRAPV (cvt->num_elements,
			      sizeof *interpreter->cvt,
			      &temp)
	  || INT_ADD_WRAPV (temp, size, &size))
	return NULL;
    }

  /* Allocate the interpreter.  */
  interpreter = xmalloc (size);

  /* Fill in pointers and default values.  */
  interpreter->max_stack_elements = maxp->max_stack_elements;
  interpreter->num_instructions = 0;
  interpreter->IP = 0;
  interpreter->storage_size = maxp->max_storage;
  interpreter->function_defs_size = maxp->max_function_defs;
  interpreter->instruction_defs_size = maxp->max_instruction_defs;
  interpreter->twilight_zone_size = maxp->max_twilight_points;
  interpreter->scale = 0; /* This should be set later.  */

  interpreter->stack = (uint32_t *) (interpreter + 1);
  interpreter->SP = interpreter->stack;
  interpreter->instructions = NULL;
  interpreter->twilight_x
    = (sfnt_f26dot6 *) (interpreter->stack
			+ maxp->max_stack_elements);
  interpreter->twilight_y = (interpreter->twilight_x
			     + maxp->max_twilight_points);
  interpreter->glyph_zone = NULL;
  interpreter->advance_width = 0;
  interpreter->storage
    = (uint32_t *) (interpreter->twilight_y
		    + maxp->max_twilight_points);
  interpreter->function_defs
    = (struct sfnt_interpreter_definition *) (interpreter->storage
					      + maxp->max_storage);
  interpreter->function_defs
    = ((struct sfnt_interpreter_definition *)
       ((unsigned char *) interpreter->function_defs + pad));
  interpreter->instruction_defs = (interpreter->function_defs
				   + maxp->max_function_defs);
  interpreter->cvt
    = (sfnt_f26dot6 *) (interpreter->instruction_defs
			+ maxp->max_instruction_defs);

  if (cvt)
    interpreter->cvt_size = cvt->num_elements;
  else
    interpreter->cvt_size = 0;

  /* Now compute the scale.  Then, scale up the control value table
     values.  */
  interpreter->scale
    = sfnt_div_fixed (pixel_size * 64,
		      head->units_per_em * 64);

  /* Set the PPEM.  */
  interpreter->ppem = pixel_size;
  interpreter->point_size = point_size;

  /* Zero out the interpreter state from the stack to the end of the
     instruction definitions.  */
  memset (interpreter->stack, 0, size - sizeof *interpreter);

  /* Initialize the interpreter graphics state.  */
  sfnt_init_graphics_state (&interpreter->state);

  /* Load the control value table.  */
  for (i = 0; i < interpreter->cvt_size; ++i)
    interpreter->cvt[i]
      = sfnt_mul_f26dot6_fixed (cvt->values[i] * 64,
				interpreter->scale);

  /* Fill in the default values for phase, period and threshold.  */
  interpreter->period = 64;
  interpreter->phase = 0;
  interpreter->threshold = 0;

  /* Fill in the current call depth.  */
  interpreter->call_depth = 0;

  /* Return the interpreter.  */
  return interpreter;
}

/* These enums are used to determine why the interpreter is being
   run.  They have the following meanings:

     - The interpreter is being run to interpret a font program.
     - The interpreter is being run to interpret the control
       value program.
     - The interpreter is being run to fit a glyph.
     - The interpreter is being run as part of an automated test.  */

enum sfnt_interpreter_run_context
  {
    SFNT_RUN_CONTEXT_FONT_PROGRAM,
    SFNT_RUN_CONTEXT_CONTROL_VALUE_PROGRAM,
    SFNT_RUN_CONTEXT_GLYPH_PROGRAM,
#ifdef TEST
    SFNT_RUN_CONTEXT_TEST,
#endif
  };

/* Cancel execution of the program in INTERPRETER with the specified
   error REASON.

   After this is called, it is probably okay to reuse INTERPRETER.
   However, instructions must always be reloaded.  */

static void
sfnt_interpret_trap (struct sfnt_interpreter *interpreter,
		     const char *reason)
{
  interpreter->trap_reason = reason;
  interpreter->call_depth = 0;
  longjmp (interpreter->trap, 1);
}

#define STACKSIZE()				\
  (interpreter->SP - interpreter->stack)

#define TRAP(why)				\
  sfnt_interpret_trap (interpreter, (why))

#define MOVE(a, b, n)				\
  memmove (a, b, (n) * sizeof (uint32_t))

#define CHECK_PREP()				\
  if (!is_prep)					\
    TRAP ("instruction executed not valid"	\
	  " outside control value program")	\

#define sfnt_add(a, b)				\
  ((int) ((unsigned int) (a) + (unsigned int) (b)))

#define sfnt_sub(a, b)				\
  ((int) ((unsigned int) (a) - (unsigned int) (b)))

#define sfnt_mul(a, b)				\
  ((int) ((unsigned int) (a) * (unsigned int) (b)))



/* Register, alu and logic instructions.  */

#define POP()					\
  (interpreter->SP == interpreter->stack	\
   ? (TRAP ("stack underflow"), 0)		\
   : (*(--interpreter->SP)))

#define LOOK()					\
  (interpreter->SP == interpreter->stack	\
   ? (TRAP ("stack underflow"), 0)		\
   : *(interpreter->SP - 1))

#define PUSH(value)				\
  {						\
    if ((char *) (interpreter->SP + 1)		\
	> (char *) interpreter->twilight_x)	\
      TRAP ("stack overflow");			\
						\
    *interpreter->SP = value;			\
    interpreter->SP++;				\
  }

#define PUSH2(high, low)			\
  {						\
    PUSH ((int16_t) ((int8_t) high) << 8	\
	  | low);				\
  }						\

#define SRP0()					\
  {						\
    uint32_t p;					\
						\
    p = POP ();					\
    interpreter->state.rp0 = p;			\
  }

#define SRP1()					\
  {						\
    uint32_t p;					\
						\
    p = POP ();					\
    interpreter->state.rp1 = p;			\
  }

#define SRP2()					\
  {						\
    uint32_t p;					\
						\
    p = POP ();					\
    interpreter->state.rp2 = p;			\
  }

#define SZP0()					\
  {						\
    uint32_t zone;				\
						\
    zone = POP ();				\
						\
    if (zone > 1)				\
      TRAP ("invalid zone");			\
						\
    interpreter->state.zp0 = zone;		\
  }

#define SZP1()					\
  {						\
    uint32_t zone;				\
						\
    zone = POP ();				\
						\
    if (zone > 1)				\
      TRAP ("invalid zone");			\
						\
    interpreter->state.zp1 = zone;		\
  }

#define SZP2()					\
  {						\
    uint32_t zone;				\
						\
    zone = POP ();				\
						\
    if (zone > 1)				\
      TRAP ("invalid zone");			\
						\
    interpreter->state.zp2 = zone;		\
  }

#define SZPS()					\
  {						\
    uint32_t zone;				\
						\
    zone = POP ();				\
						\
    if (zone > 1)				\
      TRAP ("invalid zone");			\
						\
    interpreter->state.zp0 = zone;		\
    interpreter->state.zp1 = zone;		\
    interpreter->state.zp2 = zone;		\
  }

#define SLOOP()					\
  {						\
    uint32_t loop;				\
						\
    loop = POP ();				\
						\
    if (!loop)					\
      TRAP ("loop set to 0");			\
						\
    interpreter->state.loop = loop;		\
  }

#define SMD()					\
  {						\
    sfnt_f26dot6 md;				\
						\
    md = POP ();				\
						\
    interpreter->state.minimum_distance = md;	\
  }

#define ELSE()					\
  {						\
    sfnt_interpret_else (interpreter);		\
    goto skip_step;				\
  }

#define JMPR()					\
  {						\
    int32_t offset;				\
						\
    offset = POP ();				\
						\
    if (interpreter->IP + offset < 0		\
	|| (interpreter->IP + offset		\
	    > interpreter->num_instructions))	\
      TRAP ("JMPR out of bounds");		\
						\
    interpreter->IP += offset;			\
    goto skip_step;				\
  }

#define SCVTCI()				\
  {						\
    sfnt_f26dot6 cutin;				\
						\
    cutin = POP ();				\
						\
    interpreter->state.cvt_cut_in = cutin;	\
  }

#define SSWCI()					\
  {						\
    sfnt_f26dot6 cutin;				\
						\
    cutin = POP ();				\
						\
    interpreter->state.sw_cut_in = cutin;	\
  }

#define SSW()					\
  {						\
    int32_t single_width;			\
						\
    single_width = POP ();			\
						\
    interpreter->state.single_width_value	\
      = (interpreter->scale * single_width	\
	 / 1024);				\
  }

#define DUP()					\
  {						\
    uint32_t value;				\
						\
    value = LOOK ();				\
    PUSH (value);				\
  }

#define CLEAR()					\
  {						\
    interpreter->SP = interpreter->stack;	\
  }

#define SWAP()					\
  {						\
    uint32_t a, b;				\
						\
    a = POP ();					\
    b = POP ();					\
						\
    PUSH (a);					\
    PUSH (b);					\
  }

#define DEPTH()					\
  {						\
    ptrdiff_t diff;				\
						\
    diff = (interpreter->SP			\
	    - interpreter->stack);		\
    PUSH (diff);				\
  }

#define CINDEX()				\
  {						\
    int32_t index;				\
						\
    index = POP ();				\
						\
    if (index <= 0 || index > STACKSIZE ())	\
      TRAP ("stack overflow");			\
						\
    PUSH (*(interpreter->SP - index));		\
  }

#define MINDEX()				\
  {						\
    int32_t index, what;			\
						\
    index = POP ();				\
						\
    if (index <= 0 || index > STACKSIZE ())	\
      TRAP ("stack overflow");			\
						\
    what = *(interpreter->SP - index);		\
    MOVE (interpreter->SP - index,		\
	  interpreter->SP - index + 1,		\
	  index - 1);				\
    *(interpreter->SP - 1) = what;		\
  }

#define RAW()					\
  {						\
    if (why != SFNT_RUN_CONTEXT_GLYPH_PROGRAM)	\
      TRAP ("Read Advance Width without loaded"	\
	    " glyph");				\
    PUSH (interpreter->advance_width);		\
  }

#define CALL()					\
  {						\
    uint32_t id, i;				\
    struct sfnt_interpreter_definition *def;	\
						\
    id = POP ();				\
						\
    for (i = 0;					\
	 i < interpreter->function_defs_size;	\
	 ++i)					\
      {						\
	def = &interpreter->function_defs[i];	\
						\
	if (!def->instructions)			\
	  TRAP ("invalid function");		\
						\
	if (def->opcode == id)			\
	  sfnt_interpret_call (def,		\
			       interpreter,	\
			       why);		\
	if (def->opcode == id)			\
	  break;				\
      }						\
						\
    if (i == interpreter->function_defs_size)	\
      TRAP ("invalid function");		\
  }

#define LOOPCALL()				\
  {						\
    uint32_t id, i;				\
    int32_t n;					\
    struct sfnt_interpreter_definition *def;	\
						\
    id = POP ();				\
    n = POP ();					\
						\
    if (n > 65535)				\
      TRAP ("invalid LOOPCALL count");		\
						\
    for (i = 0;					\
	 i < interpreter->function_defs_size;	\
	 ++i)					\
      {						\
	def = &interpreter->function_defs[i];	\
						\
	if (!def->instructions)			\
	  TRAP ("invalid function");		\
						\
	if (def->opcode == id)			\
	  break;				\
      }						\
						\
    if (i == interpreter->function_defs_size)	\
      TRAP ("invalid function");		\
						\
  loopcall_begin:				\
    if (n-- <= 0)				\
      break;					\
						\
    sfnt_interpret_call (def, interpreter,	\
			 why);			\
    goto loopcall_begin;			\
  }

#define FDEF()					\
  {						\
    if (why == SFNT_RUN_CONTEXT_GLYPH_PROGRAM)	\
      TRAP ("FDEF inside glyph program");	\
						\
    sfnt_interpret_fdef (interpreter, POP ());	\
    goto skip_step;				\
  }

#define ENDF()					\
  {						\
    TRAP ("stray ENDF");			\
  }

#define NPUSHB()				\
  {						\
    int b, nbytes, IP;				\
						\
    if ((IP = interpreter->IP + 1)		\
	>= interpreter->num_instructions)	\
      TRAP ("Missing arg to NPUSHB");		\
						\
    nbytes					\
      = interpreter->instructions[IP];		\
						\
    if (IP + 1 + nbytes				\
	> interpreter->num_instructions)	\
      TRAP ("args to NPUSHB lie outside IS");	\
						\
    for (b = IP + 1; b < IP + 1 + nbytes; ++b)	\
      PUSH (interpreter->instructions[b]);	\
						\
    interpreter->IP += nbytes + 1;		\
  }

#define NPUSHW()				\
  {						\
    int b, nbytes, IP;				\
						\
    if ((IP = interpreter->IP + 1)		\
	>= interpreter->num_instructions)	\
      TRAP ("Missing arg to NPUSHW");		\
						\
    nbytes					\
      = interpreter->instructions[IP] * 2;	\
						\
    if (IP + 1 + nbytes				\
	> interpreter->num_instructions)	\
      TRAP ("args to NPUSHW lie outside IS");	\
						\
    for (b = IP + 1; b < IP + 1 + nbytes;	\
	 b += 2)				\
      PUSH2 (interpreter->instructions[b],	\
	     interpreter->instructions[b + 1]);	\
						\
    interpreter->IP += nbytes + 1;		\
  }

#define WS()					\
  {						\
    uint32_t address, value;			\
						\
    value = POP ();				\
    address = POP ();				\
						\
    if (address >= interpreter->storage_size)	\
      TRAP ("invalid WS");			\
						\
    interpreter->storage[address] = value;	\
  }

#define RS()					\
  {						\
    uint32_t address;				\
						\
    address = POP ();				\
						\
    if (address >= interpreter->storage_size)	\
      TRAP ("invalid RS");			\
						\
    PUSH (interpreter->storage[address]);	\
  }

#define WCVTP()					\
  {						\
    sfnt_f26dot6 value;				\
    uint32_t location;				\
						\
    value = POP ();				\
    location = POP ();				\
						\
    if (location >= interpreter->cvt_size)	\
      TRAP ("WCVTP out of bounds");		\
						\
    interpreter->cvt[location] = value;		\
  }

#define RCVT()					\
  {						\
    sfnt_f26dot6 value;				\
    uint32_t location;				\
						\
    location = POP ();				\
						\
    if (location >= interpreter->cvt_size)	\
      TRAP ("out of bounds RCVT");		\
						\
    value = interpreter->cvt[location];		\
    PUSH (value);				\
  }

#define MPPEM()					\
  {						\
    PUSH (interpreter->ppem);			\
  }

#define MPS()					\
  {						\
    PUSH (interpreter->point_size);		\
  }

#define FLIPON()				\
  {						\
    interpreter->state.auto_flip = true;	\
  }

#define FLIPOFF()				\
  {						\
    interpreter->state.auto_flip = false;	\
  }

#define DEBUG()					\
  {						\
    POP (); /* Value is ignored.  */		\
  }

#define LT()					\
  {						\
    int32_t e1, e2;				\
						\
    e2 = POP ();				\
    e1 = POP ();				\
						\
    PUSH (e1 < e2 ? 1 : 0);			\
  }

#define LTEQ()					\
  {						\
    int32_t e1, e2;				\
						\
    e2 = POP ();				\
    e1 = POP ();				\
						\
    PUSH (e1 <= e2 ? 1 : 0);			\
  }

#define GT()					\
  {						\
    int32_t e1, e2;				\
						\
    e2 = POP ();				\
    e1 = POP ();				\
						\
    PUSH (e1 > e2 ? 1 : 0);			\
  }

#define GTEQ()					\
  {						\
    int32_t e1, e2;				\
						\
    e2 = POP ();				\
    e1 = POP ();				\
						\
    PUSH (e1 >= e2 ? 1 : 0);			\
  }

#define EQ()					\
  {						\
    uint32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (e1 == e2 ? 1 : 0);			\
  }

#define NEQ()					\
  {						\
    uint32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (e1 != e2 ? 1 : 0);			\
  }

#define ODD()					\
  {						\
    sfnt_f26dot6 e1, result;			\
						\
    e1 = POP ();				\
    result = abs (e1);				\
						\
    result					\
      = interpreter->state.round (result,	\
				  interpreter);	\
    PUSH (((result & 127) == 64) ? 1 : 0);	\
  }

#define EVEN()					\
  {						\
    sfnt_f26dot6 e1, result;			\
						\
    e1 = POP ();				\
    result = abs (e1);				\
						\
    result					\
      = interpreter->state.round (result,	\
				  interpreter);	\
    PUSH (((result & 127) == 64) ? 0 : 1);	\
  }

#define IF()					\
  {						\
    uint32_t condition;				\
						\
    condition = POP ();				\
    sfnt_interpret_if (interpreter, condition);	\
    goto skip_step;				\
  }

#define EIF()					\
  {						\
						\
  }

#define AND()					\
  {						\
    uint32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (e1 && e2 ? 1 : 0);			\
  }

#define OR()					\
  {						\
    uint32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (e1 || e2 ? 1 : 0);			\
  }

#define NOT()					\
  {						\
    uint32_t e1;				\
						\
    e1 = POP ();				\
						\
    PUSH (!e1 ? 1 : 0);				\
  }

#define SDB()					\
  {						\
    uint32_t base;				\
						\
    base = POP ();				\
						\
    interpreter->state.delta_base = base;	\
  }

#define SDS()					\
  {						\
    uint32_t shift;				\
						\
    shift = POP ();				\
						\
    if (shift > 6)				\
      TRAP ("invalid delta shift");		\
						\
    interpreter->state.delta_shift = shift;	\
  }

#define ADD()					\
  {						\
    sfnt_f26dot6 n1, n2;			\
						\
    n1 = POP ();				\
    n2 = POP ();				\
						\
    PUSH (sfnt_add (n1, n2));			\
  }

#define SUB()					\
  {						\
    sfnt_f26dot6 n2, n1;			\
						\
    n2 = POP ();				\
    n1 = POP ();				\
						\
    PUSH (sfnt_sub (n1, n2));			\
  }

#define DIV()					\
  {						\
    sfnt_f26dot6 n2, n1;			\
						\
    n2 = POP ();				\
    n1 = POP ();				\
						\
    if (!n2)					\
      TRAP ("DIV by 0");			\
						\
    PUSH (sfnt_div_f26dot6 (n1, n2));		\
  }

#define MUL()					\
  {						\
    sfnt_f26dot6 n2, n1;			\
						\
    n2 = POP ();				\
    n1 = POP ();				\
						\
    PUSH (sfnt_mul_f26dot6 (n2, n1));		\
  }

#define ABS()					\
  {						\
    sfnt_f26dot6 n;				\
						\
    n = POP ();					\
						\
    if (n == INT_MIN)				\
      PUSH (0)					\
    else					\
      PUSH (n < 0 ? -n : n)			\
  }

#define NEG()					\
  {						\
    sfnt_f26dot6 n;				\
						\
    n = POP ();					\
						\
    if (n == INT_MIN)				\
      PUSH (0)					\
    else					\
      PUSH (-n)					\
  }

#define FLOOR()					\
  {						\
    sfnt_f26dot6 n;				\
						\
    n = POP ();					\
    PUSH (sfnt_floor_f26dot6 (n));		\
  }

#define CEILING()				\
  {						\
    sfnt_f26dot6 n;				\
						\
    n = POP ();					\
    PUSH (sfnt_ceil_f26dot6 (n));		\
  }

#define WCVTF()					\
  {						\
    int32_t value;				\
    uint32_t location;				\
						\
    value = POP ();				\
    location = POP ();				\
						\
    if (location >= interpreter->cvt_size)	\
      TRAP ("WCVTF out of bounds");		\
						\
    interpreter->cvt[location]			\
      = (interpreter->scale * value		\
	 / 1024);				\
  }

#define SCANCTRL()				\
  {						\
    uint32_t value;				\
						\
    value = POP ();				\
    interpreter->state.scan_control = value;	\
  }

#define GETINFO()				\
  {						\
    uint32_t selector;				\
						\
    selector = POP ();				\
						\
    if (selector & 1)				\
      PUSH (2)					\
    else					\
      PUSH (0)					\
  }

#define IDEF()					\
  {						\
    if (why == SFNT_RUN_CONTEXT_GLYPH_PROGRAM)	\
      TRAP ("IDEF inside glyph program");	\
						\
    sfnt_interpret_idef (interpreter, POP ());	\
    goto skip_step;				\
  }

#define ROLL()					\
  {						\
    uint32_t a, b, c;				\
						\
    a = POP ();					\
    b = POP ();					\
    c = POP ();					\
						\
    PUSH (b);					\
    PUSH (a);					\
    PUSH (c);					\
  }

#define _MAX()					\
  {						\
    int32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (MAX (e1, e2));			\
  }

#define _MIN()					\
  {						\
    int32_t e1, e2;				\
						\
    e1 = POP ();				\
    e2 = POP ();				\
						\
    PUSH (MIN (e1, e2));			\
  }

#define SCANTYPE()				\
  {						\
    POP ();					\
  }

#define INSTCTRL()				\
  {						\
    uint32_t s, v;				\
						\
    CHECK_PREP ();				\
    s = POP ();					\
    v = POP ();					\
						\
    if (!s || s > 2)				\
      break;					\
						\
    interpreter->state.instruct_control		\
      &= ~(1 << s);				\
						\
    if (v)					\
      interpreter->state.instruct_control	\
	|= (1 << s);				\
  }

#define PUSHB()					\
  {						\
    int b, nbytes, IP;				\
						\
    IP = interpreter->IP;			\
    nbytes = opcode - 0xb0 + 1;			\
						\
    if (IP + nbytes + 1				\
	> interpreter->num_instructions)	\
      TRAP ("args to PUSHB lie outside IS");	\
						\
    for (b = IP + 1; b < IP + nbytes + 1; ++b)	\
      PUSH (interpreter->instructions[b]);	\
						\
    interpreter->IP += nbytes;			\
  }

#define PUSHW()					\
  {						\
    int b, nbytes, IP;				\
						\
    IP = interpreter->IP;			\
    nbytes = (opcode - 0xb8 + 1) * 2;		\
						\
    if (IP + 1 + nbytes				\
	> interpreter->num_instructions)	\
      TRAP ("args to PUSHW lie outside IS");	\
						\
    for (b = IP + 1; b < IP + nbytes + 1;	\
	 b += 2)				\
      PUSH2 (interpreter->instructions[b],	\
	     interpreter->instructions[b + 1]);	\
						\
    interpreter->IP += nbytes;			\
  }



/* Rounding instructions.  */

#define ROUND()					\
  {						\
    sfnt_f26dot6 n, result;			\
						\
    n = POP ();					\
    result = abs (n);				\
						\
    result					\
      = interpreter->state.round (result,	\
				  interpreter);	\
    PUSH (n < 0 ? -result : result);		\
  }

#define NROUND()				\
  {						\
    sfnt_f26dot6 n;				\
						\
    n = POP ();					\
    PUSH (n);					\
  }

#define ROFF()					\
  {						\
    interpreter->state.round_state = 5;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define RUTG()					\
  {						\
    interpreter->state.round_state = 4;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define RDTG()					\
  {						\
    interpreter->state.round_state = 3;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define RTG()					\
  {						\
    interpreter->state.round_state = 1;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define RTHG()					\
  {						\
    interpreter->state.round_state = 0;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define RTDG()					\
  {						\
    interpreter->state.round_state = 2;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define SROUND()				\
  {						\
    uint32_t operand;				\
						\
    operand = POP ();				\
    sfnt_set_srounding_state (interpreter,	\
			      operand,		\
			      0x4000);		\
    interpreter->state.round_state = 6;		\
    sfnt_validate_gs (&interpreter->state);	\
  }

#define S45ROUND()				\
  {						\
    uint32_t operand;				\
						\
    operand = POP ();				\
    sfnt_set_srounding_state (interpreter,	\
			      operand,		\
			      0x5a82);		\
    interpreter->state.round_state = 7;		\
    sfnt_validate_gs (&interpreter->state);	\
  }



/* CVT delta exception instructions.

   ``Exceptions'' can be placed directly inside the control value
   table, as it is reloaded every time the point size changes.  */

#define DELTAC1()				\
  {						\
    uint32_t operand1, operand2, n;		\
						\
    n = POP ();					\
						\
  deltac1_start:				\
    if (!n)					\
      break;					\
						\
    operand1 = POP ();				\
    operand2 = POP ();				\
    sfnt_deltac (1, interpreter, operand1,	\
		 operand2);			\
    n--;					\
    goto deltac1_start;				\
  }

#define DELTAC2()				\
  {						\
    uint32_t operand1, operand2, n;		\
						\
    n = POP ();					\
						\
  deltac2_start:				\
    if (!n)					\
      break;					\
						\
    operand1 = POP ();				\
    operand2 = POP ();				\
    sfnt_deltac (2, interpreter, operand1,	\
		 operand2);			\
    n--;					\
    goto deltac2_start;				\
  }

#define DELTAC3()				\
  {						\
    uint32_t operand1, operand2, n;		\
						\
    n = POP ();					\
						\
  deltac3_start:				\
    if (!n)					\
      break;					\
						\
    operand1 = POP ();				\
    operand2 = POP ();				\
    sfnt_deltac (3, interpreter, operand1,	\
		 operand2);			\
    n--;					\
    goto deltac3_start;				\
  }



/* Anachronistic angle instructions.  */

#define AA()					\
  {						\
    POP ();					\
  }

#define SANGW()					\
  {						\
    POP ();					\
  }



/* Projection and freedom vector operations.  */

#define SVTCAy()				\
  {						\
    sfnt_set_freedom_vector (interpreter,	\
			     040000, 0);	\
    sfnt_set_projection_vector (interpreter,	\
				040000, 0);	\
  }

#define SVTCAx()				\
  {						\
    sfnt_set_freedom_vector (interpreter, 0,	\
			     040000);		\
    sfnt_set_projection_vector (interpreter, 0,	\
				040000);	\
  }

#define SPvTCAy()				\
  {						\
    sfnt_set_projection_vector (interpreter,	\
				040000, 0);	\
  }

#define SPvTCAx()				\
  {						\
    sfnt_set_projection_vector (interpreter, 0,	\
				040000);	\
  }

#define SFvTCAy()				\
  {						\
    sfnt_set_freedom_vector (interpreter,	\
			     040000, 0);	\
  }

#define SFvTCAx()				\
  {						\
    sfnt_set_freedom_vector (interpreter, 0,	\
			     040000);		\
  }

#define SPVTL()					\
  {						\
    struct sfnt_unit_vector vector;		\
    uint32_t p2, p1;				\
						\
    p2 = POP ();				\
    p1 = POP ();				\
						\
    sfnt_line_to_vector (interpreter,		\
			 p2, p1, &vector,	\
			 opcode == 0x07);	\
						\
    sfnt_save_projection_vector (interpreter,	\
				 &vector);	\
  }

#define SFVTL()					\
  {						\
    struct sfnt_unit_vector vector;		\
    uint32_t p2, p1;				\
						\
    p2 = POP ();				\
    p1 = POP ();				\
						\
    sfnt_line_to_vector (interpreter,		\
			 p2, p1, &vector,	\
			 opcode == 0x09);	\
						\
    sfnt_save_freedom_vector (interpreter,	\
			      &vector);		\
  }

#define SPVFS()					\
  {						\
    uint32_t y, x;				\
						\
    y = POP ();					\
    x = POP ();					\
						\
    sfnt_set_projection_vector (interpreter, x,	\
				y);		\
  }

#define SFVFS()					\
  {						\
    uint16_t y, x;				\
						\
    y = POP ();					\
    x = POP ();					\
						\
    sfnt_set_freedom_vector (interpreter, x,	\
			     y);		\
  }

#define GPV()					\
  {						\
    struct sfnt_unit_vector vector;		\
						\
    vector					\
      = interpreter->state.projection_vector;	\
						\
    PUSH ((uint16_t) vector.x);			\
    PUSH ((uint16_t) vector.y);			\
  }

#define GFV()					\
  {						\
    struct sfnt_unit_vector vector;		\
						\
    vector					\
      = interpreter->state.freedom_vector;	\
						\
    PUSH ((uint16_t) vector.x);			\
    PUSH ((uint16_t) vector.y);			\
  }

#define SFVTPV()				\
  {						\
    interpreter->state.freedom_vector		\
      = interpreter->state.projection_vector;	\
  }

#define ISECT()					\
  {						\
    uint32_t a0, a1, b0, b1, p;			\
						\
    a0 = POP ();				\
    a1 = POP ();				\
    b0 = POP ();				\
    b1 = POP ();				\
    p = POP ();					\
						\
    sfnt_interpret_isect (interpreter,		\
			  a0, a1, b0, b1, p);	\
  }

#define ALIGNPTS()				\
  {						\
    uint32_t p1, p2;				\
						\
    p1 = POP ();				\
    p2 = POP ();				\
						\
    sfnt_interpret_alignpts (interpreter, p1,	\
			     p2);		\
  }



#define NOT_IMPLEMENTED()			\
  sfnt_interpret_unimplemented (interpreter,	\
				opcode, why)



/* Save the specified unit VECTOR into INTERPRETER's graphics
   state.  */

static void
sfnt_save_projection_vector (struct sfnt_interpreter *interpreter,
			     struct sfnt_unit_vector *vector)
{
  interpreter->state.projection_vector = *vector;

  sfnt_validate_gs (&interpreter->state);
}

/* Save the specified unit VECTOR into INTERPRETER's graphics
   state.  */

static void
sfnt_save_freedom_vector (struct sfnt_interpreter *interpreter,
			  struct sfnt_unit_vector *vector)
{
  interpreter->state.freedom_vector = *vector;

  sfnt_validate_gs (&interpreter->state);
}

/* Return the values of the point NUMBER in the zone pointed to by
   INTERPRETER's ZP2 register.

   Trap if NUMBER is out of bounds or the zone is inaccessible.  */

static void
sfnt_address_zp2 (struct sfnt_interpreter *interpreter,
		  uint32_t number,
		  sfnt_f26dot6 *x, sfnt_f26dot6 *y)
{
  if (!interpreter->state.zp2)
    {
      /* Address the twilight zone.  */
      if (number >= interpreter->twilight_zone_size)
	TRAP ("address to ZP2 (twilight zone) out of bounds");

      *x = interpreter->twilight_x[number];
      *y = interpreter->twilight_y[number];
      return;
    }

  /* Address the glyph zone.  */
  if (!interpreter->glyph_zone)
    TRAP ("address to ZP2 (glyph zone) points into unset"
	  " zone");

  if (number >= interpreter->glyph_zone->num_points)
    TRAP ("address to ZP2 (glyph zone) out of bounds");

  *x = interpreter->glyph_zone->x_current[number];
  *y = interpreter->glyph_zone->y_current[number];
}

/* Return the values of the point NUMBER in the zone pointed to by
   INTERPRETER's ZP1 register.

   Trap if NUMBER is out of bounds or the zone is inaccessible.  */

static void
sfnt_address_zp1 (struct sfnt_interpreter *interpreter,
		  uint32_t number,
		  sfnt_f26dot6 *x, sfnt_f26dot6 *y)
{
  if (!interpreter->state.zp1)
    {
      /* Address the twilight zone.  */
      if (number >= interpreter->twilight_zone_size)
	TRAP ("address to ZP1 (twilight zone) out of bounds");

      *x = interpreter->twilight_x[number];
      *y = interpreter->twilight_y[number];
      return;
    }

  /* Address the glyph zone.  */
  if (!interpreter->glyph_zone)
    TRAP ("address to ZP1 (glyph zone) points into unset"
	  " zone");

  if (number >= interpreter->glyph_zone->num_points)
    TRAP ("address to ZP1 (glyph zone) out of bounds");

  *x = interpreter->glyph_zone->x_current[number];
  *y = interpreter->glyph_zone->y_current[number];
}

/* Return the values of the point NUMBER in the zone pointed to by
   INTERPRETER's ZP0 register.

   Trap if NUMBER is out of bounds or the zone is inaccessible.  */

static void
sfnt_address_zp0 (struct sfnt_interpreter *interpreter,
		  uint32_t number,
		  sfnt_f26dot6 *x, sfnt_f26dot6 *y)
{
  if (!interpreter->state.zp0)
    {
      /* Address the twilight zone.  */
      if (number >= interpreter->twilight_zone_size)
	TRAP ("address to ZP0 (twilight zone) out of bounds");

      *x = interpreter->twilight_x[number];
      *y = interpreter->twilight_y[number];
      return;
    }

  /* Address the glyph zone.  */
  if (!interpreter->glyph_zone)
    TRAP ("address to ZP0 (glyph zone) points into unset"
	  " zone");

  if (number >= interpreter->glyph_zone->num_points)
    TRAP ("address to ZP0 (glyph zone) out of bounds");

  *x = interpreter->glyph_zone->x_current[number];
  *y = interpreter->glyph_zone->y_current[number];
}

/* Set the point NUMBER in the zone referenced by INTERPRETER's ZP2
   register to the specified X and Y.

   Apply FLAGS to NUMBER's flags in that zone.  Trap if NUMBER is out
   of bounds.  */

static void
sfnt_store_zp2 (struct sfnt_interpreter *interpreter,
		uint32_t number, sfnt_f26dot6 x, sfnt_f26dot6 y,
		int flags)
{
  if (!interpreter->state.zp2)
    {
      /* Address the twilight zone.  */
      if (number >= interpreter->twilight_zone_size)
	TRAP ("address to ZP2 (twilight zone) out of bounds");

      interpreter->twilight_x[number] = x;
      interpreter->twilight_y[number] = y;
      return;
    }

  /* Address the glyph zone.  */
  if (!interpreter->glyph_zone)
    TRAP ("address to ZP0 (glyph zone) points into unset"
	  " zone");

  if (number >= interpreter->glyph_zone->num_points)
    TRAP ("address to ZP0 (glyph zone) out of bounds");

  interpreter->glyph_zone->x_current[number] = x;
  interpreter->glyph_zone->y_current[number] = y;
  interpreter->glyph_zone->flags[number] |= flags;
}

#if 0

/* Convert the line between the points X1, Y1 and X2, Y2 to standard
   form.

   Return the two coefficients in *A0 and *B0, and the constant in
   *C.  */

static void
sfnt_line_to_standard_form (sfnt_f26dot6 x1, sfnt_f26dot6 y1,
			    sfnt_f26dot6 x2, sfnt_f26dot6 y2,
			    sfnt_f26dot6 *a, sfnt_f26dot6 *b,
			    sfnt_f26dot6 *c)
{
  sfnt_f26dot6 a_temp, b_temp, c_temp;

  a_temp = sfnt_sub (y2, y1);
  b_temp = sfnt_sub (x1, x2);
  c_temp = sfnt_sub (sfnt_mul_f26dot6 (x1, y2),
		     sfnt_mul_f26dot6 (x2, y1));

  *a = a_temp;
  *b = b_temp;
  *c = c_temp;
}

#endif

/* Move the specified POINT in the zone addressed by INTERPRETER's ZP0
   register by the given DISTANCE along the freedom vector.

   No checking is done to ensure that POINT lies inside the zone, or
   even that the zone exists at all.  */

static void
sfnt_move_zp0 (struct sfnt_interpreter *interpreter, uint32_t point,
	       sfnt_f26dot6 distance)
{
  if (!interpreter->state.zp0)
    interpreter->state.move (&interpreter->twilight_x[point],
			     &interpreter->twilight_y[point],
			     interpreter, distance, NULL);
  else
    interpreter->state.move (&interpreter->glyph_zone->x_current[point],
			     &interpreter->glyph_zone->y_current[point],
			     interpreter, distance,
			     &interpreter->glyph_zone->flags[point]);
}

/* Move the specified POINT in the zone addressed by INTERPRETER's ZP1
   register by the given DISTANCE along the freedom vector.

   No checking is done to ensure that POINT lies inside the zone, or
   even that the zone exists at all.  */

static void
sfnt_move_zp1 (struct sfnt_interpreter *interpreter, uint32_t point,
	       sfnt_f26dot6 distance)
{
  if (!interpreter->state.zp1)
    interpreter->state.move (&interpreter->twilight_x[point],
			     &interpreter->twilight_y[point],
			     interpreter, distance, NULL);
  else
    interpreter->state.move (&interpreter->glyph_zone->x_current[point],
			     &interpreter->glyph_zone->y_current[point],
			     interpreter, distance,
			     &interpreter->glyph_zone->flags[point]);
}

/* Project the vector VX, VY onto INTERPRETER's projection vector.
   Return the magnitude of the projection.  */

static sfnt_f26dot6
sfnt_project_vector (struct sfnt_interpreter *interpreter,
		     sfnt_f26dot6 vx, sfnt_f26dot6 vy)
{
  return interpreter->state.project (vx, vy, interpreter);
}

/* Align the two points P1 and P2 relative to the projection vector.
   P1 is addressed relative to ZP0, and P2 is addressed relative to
   ZP1.

   Move both points along the freedom vector by half the magnitude of
   the the projection of a vector formed by P1.x - P2.x, P1.y - P2.y,
   upon the projection vector.  */

static void
sfnt_interpret_alignpts (struct sfnt_interpreter *interpreter,
			 uint32_t p1, uint32_t p2)
{
  sfnt_f26dot6 p1x, p1y, p2x, p2y;
  sfnt_f26dot6 magnitude;

  sfnt_address_zp0 (interpreter, p1, &p1x, &p1y);
  sfnt_address_zp1 (interpreter, p2, &p2x, &p2y);

  magnitude = sfnt_project_vector (interpreter,
				   sfnt_sub (p1x, p2x),
				   sfnt_sub (p1y, p2y));
  magnitude = magnitude / 2;

  /* Now move both points along the freedom vector.  */
  sfnt_move_zp0 (interpreter, p1, magnitude);
  sfnt_move_zp1 (interpreter, p2, -magnitude);
}

/* Set the point P in the zone referenced in INTERPRETER's ZP2
   register to the intersection between the line formed by the points
   POINT_A0 to POINT_A1 in ZP0 and another line formed by POINT_B0 to
   POINT_B1 in ZP1.

   Touch the point P.  */

static void
sfnt_interpret_isect (struct sfnt_interpreter *interpreter,
		      uint32_t point_a0, uint32_t point_a1,
		      uint32_t point_b0, uint32_t point_b1,
		      uint32_t p)
{
  sfnt_f26dot6 a0x, a0y, a1x, a1y;
  sfnt_f26dot6 b0x, b0y, b1x, b1y;
#if 0
  sfnt_f26dot6 determinant, dx, dy;
  sfnt_f26dot6 a0, b0, a1, b1;
  sfnt_f26dot6 c0, c1, px, py;
#else
  sfnt_f26dot6 dx, dy, dax, day, dbx, dby;
  sfnt_f26dot6 discriminant, val, dot_product;
  sfnt_f26dot6 px, py;
#endif

  /* Load points.  */
  sfnt_address_zp0 (interpreter, point_a0, &a0x, &a0y);
  sfnt_address_zp0 (interpreter, point_a1, &a1x, &a1y);
  sfnt_address_zp1 (interpreter, point_b0, &b0x, &b0y);
  sfnt_address_zp1 (interpreter, point_b1, &b1x, &b1y);

#if 0
  /* The system is determined from the standard form (look this up) of
     both lines.

     (the variables below have no relation to C identifiers
      unless otherwise specified.)

       a0*x + b0*y = c0
       a1*x + b1*y = c1

     The coefficient matrix is thus

       [ a0 b0
         a1 b1 ]

     the vector of constants (also just dubbed the ``column vector''
     by some people)

       [ c0
         c1 ]

     and the solution vector becomes

       [ x
         y ]

     Since there are exactly two equations and two unknowns, Cramer's
     rule applies, and there is no need for any Gaussian elimination.

     The determinant for the coefficient matrix is:

       D = a0*b1 - b0*a1

     the first and second determinants are:

       Dx = c0*b1 - a0*c1
       Dy = a1*c1 - c0*b1

     and x = Dx / D, y = Dy / D.

     If the system is indeterminate, D will be 0.  */

  sfnt_line_to_standard_form (a0x, a0y, a1x, a1y,
			      &a0, &b0, &c0);
  sfnt_line_to_standard_form (b0x, b0y, b1x, b1y,
			      &a1, &b1, &c1);


  /* Compute determinants.  */
  determinant = sfnt_sub (sfnt_mul_fixed (a0, b1),
			  sfnt_mul_fixed (b0, a1));
  dx = sfnt_sub (sfnt_mul_fixed (c0, b1),
		 sfnt_mul_fixed (a1, c1));
  dy = sfnt_sub (sfnt_mul_fixed (a0, c1),
		 sfnt_mul_fixed (c0, b0));

  /* Detect degenerate cases.  */

  if (determinant == 0)
    goto degenerate_case;
#else
  /* The algorithm above would work with floating point, but overflows
     too easily with fixed point numbers.

     Instead, use the modified vector projection algorithm found in
     FreeType.  */

  dbx = sfnt_sub (b1x, b0x);
  dby = sfnt_sub (b1y, b0y);
  dax = sfnt_sub (a1x, a0x);
  day = sfnt_sub (a1y, a0y);

  /* Compute vector cross product.  */
  discriminant = sfnt_add (sfnt_mul_f26dot6 (dax, -dby),
			   sfnt_mul_f26dot6 (day, dbx));
  dot_product = sfnt_add (sfnt_mul_f26dot6 (dax, dbx),
			  sfnt_mul_f26dot6 (day, dby));

  /* Reject any non-intersections and grazing intersections.  */
  if (!(sfnt_mul (19, abs (discriminant)) > abs (dot_product)))
    return;

  /* Reject any non-intersections.  */
  if (!discriminant)
    goto degenerate_case;

  dx = sfnt_sub (b0x, a0x);
  dy = sfnt_sub (b0y, a0y);
  val = sfnt_add (sfnt_mul_f26dot6 (dx, -dby),
		  sfnt_mul_f26dot6 (dy, dbx));

  /* Project according to these values.  */
  dx = sfnt_add (a0x, sfnt_multiply_divide_signed (val, dax,
						   discriminant));
  dy = sfnt_add (a0y, sfnt_multiply_divide_signed (val, day,
						   discriminant));
#endif

  sfnt_store_zp2 (interpreter, p,
#if 0
		  sfnt_div_fixed (dx, determinant),
		  sfnt_div_fixed (dy, determinant),
#else
		  dx, dy,
#endif
		  SFNT_POINT_TOUCHED_BOTH);
  return;

 degenerate_case:

  /* Apple says that in this case:

     Px = (a0x + a1x) / 2 + (b0x + b1x) / 2
          ---------------------------------
	                  2
     Py = (a0y + a1y) / 2 + (b0y + b1y) / 2
          ---------------------------------
	                  2  */

  px = (sfnt_add (a0x, a1x) / 2 + sfnt_add (b0x, b1x) / 2) / 2;
  py = (sfnt_add (a0y, a1y) / 2 + sfnt_add (b0y, b1y) / 2) / 2;
  sfnt_store_zp2 (interpreter, p, px, py,
		  SFNT_POINT_TOUCHED_BOTH);
}

/* Compute the square root of the 16.16 fixed point number N.  */

static sfnt_fixed
sfnt_sqrt_fixed (sfnt_fixed n)
{
  int count;
  unsigned int root, rem_hi, rem_lo, possible;

  root = 0;

  if (n > 0)
    {
      rem_hi = 0;
      rem_lo = n;
      count = 24;

      do
	{
	  rem_hi = (rem_hi << 2) | (rem_lo >> 30);
	  rem_lo <<= 2;
	  root <<= 1;
	  possible = (root << 1) + 1;

	  if (rem_hi >= possible)
	    {
	      rem_hi -= possible;
	      root += 1;
	    }
	}
      while (--count);
    }

  return root;
}

/* Compute a unit vector describing a vector VX, VY.  Return the value
   in *VECTOR.  */

static void
sfnt_normalize_vector (sfnt_f26dot6 vx, sfnt_f26dot6 vy,
		       struct sfnt_unit_vector *vector)
{
  sfnt_f26dot6 x_squared, y_squared;
  sfnt_fixed n, magnitude;

  if (!vx && !vy)
    {
      /* The MS scaler seems to do this.  */
      vector->x = 04000;
      vector->y = 0;
      return;
    }

  /* Compute the magnitude of this vector.  */
  x_squared = sfnt_mul_f26dot6 (vx, vx);
  y_squared = sfnt_mul_f26dot6 (vy, vy);

  /* Convert to 16.16 for greater precision.  */
  n = sfnt_add (x_squared, y_squared) * 1024;

  /* Get hypotenuse of the triangle from vx, 0, to 0, vy.  */
  magnitude = sfnt_sqrt_fixed (n);

  /* Long division.. eek! */
  vector->x = (sfnt_div_fixed (vx * 1024, magnitude) >> 2);
  vector->y = (sfnt_div_fixed (vy * 1024, magnitude) >> 2);
}

/* Compute a unit vector describing the direction of a line from the
   point P2 to the point P1.  Save the result in *VECTOR.

   P2 is the address of a point in the zone specified in the ZP2
   register.  P1 is the address of a point in the zone specified in
   the ZP1 register.  Take the values of both registers from the
   specified INTERPRETER's graphics state.

   If PERPENDICULAR, then *VECTOR will be rotated 90 degrees
   counter-clockwise.  Else, *VECTOR will be parallel to the line.  */

static void
sfnt_line_to_vector (struct sfnt_interpreter *interpreter,
		     uint32_t p2, uint32_t p1,
		     struct sfnt_unit_vector *vector,
		     bool perpendicular)
{
  sfnt_f26dot6 x2, y2;
  sfnt_f26dot6 x1, y1;
  sfnt_f26dot6 a, b, temp;

  sfnt_address_zp2 (interpreter, p2, &x2, &y2);
  sfnt_address_zp1 (interpreter, p1, &x1, &y1);

  /* Calculate the vector between X2, Y2, and X1, Y1.  */
  a = sfnt_sub (x1, x2);
  b = sfnt_sub (y1, y2);

  /* Rotate counterclockwise if necessary.  */

  if (perpendicular)
    {
      temp = b;
      b = a;
      a = -temp;
    }

  /* Normalize this vector, turning it into a unit vector.  */
  sfnt_normalize_vector (a, b, vector);
}

/* Apply the delta specified by OPERAND to the control value table
   entry at INDEX currently loaded inside INTERPRETER.

   Trap if INDEX is out of bounds.

   NUMBER is the number of the specific DELTAC instruction this
   instruction is being applied on behalf of.  It must be between 1
   and 3.  */

static void
sfnt_deltac (int number, struct sfnt_interpreter *interpreter,
	     unsigned char operand, unsigned int index)
{
  int ppem, delta;

  /* Make sure INDEX is a valid cvt entry.  */

  if (index >= interpreter->cvt_size)
    TRAP ("DELTACn instruction out of bounds");

  /* operand is an 8 bit number.  The most significant 4 bits
     represent a specific PPEM size at which to apply the delta
     specified in the low 4 bits, summed with an instruction specific
     delta, and the current delta base.  */

  ppem = (operand >> 4) + interpreter->state.delta_base;

  switch (number)
    {
    case 1:
      break;

    case 2:
      ppem += 16;
      break;

    case 3:
      ppem += 32;
      break;
    }

  /* Don't apply the delta if the ppem size doesn't match.  */

  if (interpreter->ppem != ppem)
    return;

  /* Now, determine the delta using the low 4 bits.  The low 4 bits
     actually specify a ``magnitude'' to apply to the delta, and do
     not have an encoding for the delta 0.  */

  switch (operand & 0xf)
    {
    case 0:
      delta = -8;
      break;

    case 1:
      delta = -7;
      break;

    case 2:
      delta = -6;
      break;

    case 3:
      delta = -5;
      break;

    case 4:
      delta = -4;
      break;

    case 5:
      delta = -3;
      break;

    case 6:
      delta = -2;
      break;

    case 7:
      delta = -1;
      break;

    case 8:
      delta = 1;
      break;

    case 9:
      delta = 2;
      break;

    case 10:
      delta = 3;
      break;

    case 11:
      delta = 4;
      break;

    case 12:
      delta = 5;
      break;

    case 13:
      delta = 6;
      break;

    case 14:
      delta = 7;
      break;

    case 15:
      delta = 8;
      break;
    }

  /* Now, scale up the delta by the step size, which is determined by
     the delta shift.  */
  delta *= 1l << (6 - interpreter->state.delta_shift);

  /* Finally, apply the delta to the CVT entry.  */
  interpreter->cvt[index] = sfnt_add (interpreter->cvt[index],
				      delta);
}

/* Needed by sfnt_interpret_call.  */
static void sfnt_interpret_run (struct sfnt_interpreter *,
				enum sfnt_interpreter_run_context);

/* Call DEFINITION inside INTERPRETER.

   Save INTERPRETER->IP, INTERPRETER->instructions, and
   INTERPRETER->num_instructions onto the C stack.

   Then, load the instructions in DEFINITION, and run the interpreter
   again with the context CONTEXT.

   Finally, restore all values.  */

static void
sfnt_interpret_call (struct sfnt_interpreter_definition *definition,
		     struct sfnt_interpreter *interpreter,
		     enum sfnt_interpreter_run_context context)
{
  uint16_t num_instructions;
  int IP;
  unsigned char *instructions;

  /* Check that no recursion is going on.  */
  if (interpreter->call_depth++ >= 64)
    TRAP ("CALL called CALL more than 63 times");

  /* Save the old IP, instructions and number of instructions.  */
  num_instructions = interpreter->num_instructions;
  IP = interpreter->IP;
  instructions = interpreter->instructions;

  /* Load and run the definition.  */
  interpreter->num_instructions = definition->instruction_count;
  interpreter->instructions = definition->instructions;
  interpreter->IP = 0;
  sfnt_interpret_run (interpreter, context);

  /* Restore the old values.  */
  interpreter->num_instructions = num_instructions;
  interpreter->IP = IP;
  interpreter->instructions = instructions;
}

/* Set the detailed rounding state in interpreter, on behalf of either
   an SROUND or S45ROUND instruction that has been given the operand
   OPERAND.

   Use the specified GRID_PERIOD to determine the period.  It is is a
   18.14 fixed point number, but the rounding state set will be a 26.6
   fixed point number.  */

static void
sfnt_set_srounding_state (struct sfnt_interpreter *interpreter,
			  uint32_t operand, sfnt_f18dot14 grid_period)
{
  sfnt_f18dot14 period, phase, threshold;

  /* The most significant 2 bits in the 8 bit OPERAND determine the
     period.  */

  switch ((operand & 0xc0) >> 6)
    {
    case 0:
      period = grid_period / 2;
      break;

    case 1:
      period = grid_period;
      break;

    case 2:
      period = grid_period * 2;
      break;

    case 3:
    default:
      TRAP ("reserved period given to SROUND");
    }

  /* The next two bits determine the phase.  */

  switch ((operand & 0x30) >> 4)
    {
    case 0:
      phase = 0;
      break;

    case 1:
      phase = period / 4;
      break;

    case 2:
      phase = period / 2;
      break;

    case 3:
    default:
      phase = period * 3 / 2;
      break;
    }

  /* And the least significant 4 bits determine the threshold.  */

  if (operand & 0x0f)
    threshold = (((int) (operand & 0x0f) - 4)
		 * period / 8);
  else
    threshold = period - 1;

  /* Now extend these values to 26.6 format and set them.  */
  interpreter->period = period >> 8;
  interpreter->phase = phase >> 8;
  interpreter->threshold = threshold >> 8;
}

/* Move to the next opcode in INTERPRETER's instruction stream.
   Value is the opcode originally at INTERPRETER->IP.  */

static unsigned char
sfnt_skip_code (struct sfnt_interpreter *interpreter)
{
  unsigned char opcode;
  int nbytes;

  if (interpreter->IP == interpreter->num_instructions)
    TRAP ("IP at end of instruction stream");

  /* Load opcode at IP.  */
  opcode = interpreter->instructions[interpreter->IP];

  if (opcode == 0x40 || opcode == 0x41)
    {
      if (interpreter->IP + 1 >= interpreter->num_instructions)
	TRAP ("Missing arg to NPUSHB or NPUSHW");

      /* Figure out how many bytes or words to push.  */

      nbytes = interpreter->instructions[interpreter->IP + 1];

      if (opcode == 0x41)
	nbytes *= 2;

      if (interpreter->IP + 2 + nbytes > interpreter->num_instructions)
	TRAP ("args to NPUSH instruction lie outside IS");

      /* Increment IP by so much.  */
      interpreter->IP += 2 + nbytes;
    }
  else if (opcode >= 0xb0 && opcode <= 0xb7)
    {
      nbytes = opcode - 0xb0 + 1;

      if (interpreter->IP + 1 + nbytes > interpreter->num_instructions)
	TRAP ("args to PUSHB instruction lie outide IS");

      interpreter->IP += 1 + nbytes;
    }
  else if (opcode >= 0xb8 && opcode <= 0xbf)
    {
      nbytes = (opcode - 0xb8 + 1) * 2;

      if (interpreter->IP + 1 + nbytes > interpreter->num_instructions)
	TRAP ("args to PUSHW instruction lie outide IS");

      interpreter->IP += 1 + nbytes;
    }
  else
    interpreter->IP++;

  return opcode;
}

/* Interpret the unimplemented operation OPCODE using INTERPRETER, and
   the context WHY.  If there is no instruction definition named
   OPCODE, trap.  */

static void
sfnt_interpret_unimplemented (struct sfnt_interpreter *interpreter,
			      unsigned char opcode,
			      enum sfnt_interpreter_run_context why)
{
  uint32_t i;
  struct sfnt_interpreter_definition *def;

  for (i = 0; i < interpreter->instruction_defs_size; ++i)
    {
      def = &interpreter->instruction_defs[i];

      if (def->opcode == opcode)
	{
	  if (!def->instructions)
	    TRAP ("** ERROR ** malformed internal instruction"
		  " definition");

	  sfnt_interpret_call (def, interpreter, why);
	  return;
	}
    }

  TRAP ("invalid instruction");
}

/* Start a function definition in INTERPRETER, with the function
   opcode OPCODE.  */

static void
sfnt_interpret_fdef (struct sfnt_interpreter *interpreter,
		     uint32_t opcode)
{
  size_t i, num_fdefs;
  int IP;
  unsigned char instruction;

  IP = interpreter->IP + 1;
  num_fdefs = 0;

  /* Now find an ENDF.  */

  while ((instruction = sfnt_skip_code (interpreter)) != 0x2d)
    {
      if (interpreter->IP >= interpreter->num_instructions)
	TRAP ("missing ENDF");

      /* If this is an FDEF or IDEF instruction, increment num_fdefs.
	 Prohibit nested FDEFs or IDEFS.  */
      if (instruction == 0x2c || instruction == 0x89)
	++num_fdefs;

      if (num_fdefs > 1)
	TRAP ("IDEF or FDEF before ENDF");
    }

  /* ENDF has been found.  Now save the function definition.  Try to
     find an existing function definition with this opcode.  If that
     fails, make i the first available function definition.  */

  for (i = 0; i < interpreter->function_defs_size; ++i)
    {
      if (interpreter->function_defs[i].opcode == opcode
	  || !interpreter->function_defs[i].instructions)
	break;
    }

  if (i == interpreter->function_defs_size)
    TRAP ("number of fdefs exceeded maxp->max_function_defs");

  /* Save the opcode of this function definition.  */
  interpreter->function_defs[i].opcode = opcode;

  /* Make sure to ignore the trailing ENDF instruction.  */
  interpreter->function_defs[i].instruction_count
    = interpreter->IP - IP - 1;

  /* Now save a pointer to the instructions.  */
  interpreter->function_defs[i].instructions = interpreter->instructions + IP;
}

/* Start an instruction definition in INTERPRETER, with the
   instruction opcode OPCODE.  */

static void
sfnt_interpret_idef (struct sfnt_interpreter *interpreter,
		     uint32_t opcode)
{
  size_t i, num_fdefs;
  int IP;
  unsigned char instruction;

  IP = interpreter->IP + 1;
  num_fdefs = 0;

  /* Now find an ENDF.  */

  while ((instruction = sfnt_skip_code (interpreter)) != 0x2d)
    {
      if (interpreter->IP >= interpreter->num_instructions)
	TRAP ("missing ENDF");

      /* If this is an FDEF or IDEF instruction, increment num_fdefs.
	 Prohibit nested FDEFs or IDEFS.  */
      if (instruction == 0x2c || instruction == 0x89)
	++num_fdefs;

      if (num_fdefs > 1)
	TRAP ("IDEF or FDEF before ENDF");
    }

  /* ENDF has been found.  Now save the instruction definition.  Try to
     find an existing instruction definition with this opcode.  If that
     fails, make i the first available instruction definition.  */

  for (i = 0; i < interpreter->instruction_defs_size; ++i)
    {
      if (interpreter->instruction_defs[i].opcode == opcode
	  || !interpreter->instruction_defs[i].instructions)
	break;
    }

  if (i == interpreter->instruction_defs_size)
    TRAP ("number of defs exceeded maxp->max_instruction_defs");

  /* Save the opcode of this instruction definition.  */
  interpreter->instruction_defs[i].opcode = opcode;

  /* Make sure to ignore the trailing ENDF instruction.  */
  interpreter->instruction_defs[i].instruction_count
    = interpreter->IP - IP - 1;

  /* Now save a pointer to the instructions.  */
  interpreter->instruction_defs[i].instructions
    = interpreter->instructions + IP;
}

/* Interpret the specified conditional at INTERPRETER->IP.
   If CONDITION, evaluate this branch up until the next ELSE or ENDIF.
   Else, evaluate the branch from a matching ELSE condition, if
   one exists.  */

static void
sfnt_interpret_if (struct sfnt_interpreter *interpreter,
		   bool condition)
{
  int nifs;
  bool need_break;
  unsigned char opcode;

  if (condition)
    {
      interpreter->IP++;
      return;
    }

  /* Number of ifs.  */
  nifs = 0;
  need_break = false;

  /* Break past the matching else condition.  */
  do
    {
      /* Load the current opcode, then increase IP.  */
      opcode = sfnt_skip_code (interpreter);

      if (interpreter->IP >= interpreter->num_instructions)
	break;

      switch (opcode)
	{
	case 0x58: /* IF */
	  nifs++;
	  break;

	case 0x1B: /* ELSE */
	  if (nifs == 1)
	    need_break = true;

	  break;

	case 0x59: /* EIF */
	  nifs--;
	  if (nifs == 0)
	    need_break = true;

	  break;
	}
    }
  while (!need_break);
}

/* Interpret the specified ELSE branch at INTERPRETER->IP.
   Evaluate starting from a matching ENDIF instruction.

   If IF has set INTERPRETER->IP to a code within an ELSE branch, this
   will not be called.  */

static void
sfnt_interpret_else (struct sfnt_interpreter *interpreter)
{
  int nifs;
  unsigned char opcode;

  /* Number of ifs.  */
  nifs = 1;

  /* Break past the matching ENDIF condition.  */
  do
    {
      /* Load the current opcode, then increase IP.  */
      opcode = sfnt_skip_code (interpreter);

      if (interpreter->IP >= interpreter->num_instructions)
	break;

      switch (opcode)
	{
	case 0x58: /* IF */
	  nifs++;
	  break;

	case 0x59: /* EIF */
	  nifs--;

	  break;
	}
    }
  while (nifs > 0);
}

/* ``Add engine compensation to X''.  Since engine compensation is not
   implemented here, this simply returns X.  INTERPRETER is
   unused.  */

static sfnt_f26dot6
sfnt_round_none (sfnt_f26dot6 x, struct sfnt_interpreter *interpreter)
{
  return x;
}

/* Round X to the grid after adding engine compensation.  Return the
   result.  INTERPRETER is unused.  */

static sfnt_f26dot6
sfnt_round_to_grid (sfnt_f26dot6 x, struct sfnt_interpreter *interpreter)
{
  return sfnt_round_f26dot6 (x);
}

/* Round X to the nearest half integer or integer and return the
   result.  INTERPRETER is unused.  */

static sfnt_f26dot6
sfnt_round_to_double_grid (sfnt_f26dot6 x,
			   struct sfnt_interpreter *interpreter)
{
  return (x + 020) & ~037;
}

/* Take the floor of X and return the result.  INTERPRETER is
   unused.  */

static sfnt_f26dot6
sfnt_round_down_to_grid (sfnt_f26dot6 x,
			 struct sfnt_interpreter *interpreter)
{
  return sfnt_floor_f26dot6 (x);
}

/* Take the ceiling of X and return the result.  INTERPRETER is
   unused.  */

static sfnt_f26dot6
sfnt_round_up_to_grid (sfnt_f26dot6 x,
		       struct sfnt_interpreter *interpreter)
{
  return sfnt_ceil_f26dot6 (x);
}

/* Round X to only the nearest half integer and return the result.
   INTERPRETER is unused.  */

static sfnt_f26dot6
sfnt_round_to_half_grid (sfnt_f26dot6 x,
			 struct sfnt_interpreter *interpreter)
{
  return sfnt_floor_f26dot6 (x) + 32;
}

/* Round X using the detailed rounding information ``super rounding
   state'' in INTERPRETER.  Value is the result.  */

static sfnt_f26dot6
sfnt_round_super (sfnt_f26dot6 x,
		  struct sfnt_interpreter *interpreter)
{
  sfnt_f26dot6 value;

  /* Compute the rounded value.  */
  value = sfnt_add ((interpreter->threshold
		     - interpreter->phase), x);
  value = sfnt_add (value & -interpreter->period,
		    interpreter->phase);

  /* Remember that since the phase is specified by font instructions,
     it is possible for the sign to be changed.  In that case, return
     the phase itself.  */

  return value < 0 ? interpreter->phase : value;
}

/* Round X using the detailed rounding information ``super rounding
   state'' in INTERPRETER, but suitably for values that are multiples
   of the sqrt of 2.  Value is the result.  */

static sfnt_f26dot6
sfnt_round_super45 (sfnt_f26dot6 x,
		    struct sfnt_interpreter *interpreter)
{
  sfnt_f26dot6 value;

  /* Compute the rounded value.  */

  value = ((sfnt_add (x, (interpreter->threshold
			  - interpreter->phase))
	    / interpreter->period)
	   * interpreter->period);
  value = sfnt_add (value, interpreter->phase);

  /* Remember that since the phase is specified by font instructions,
     it is possible for the sign to be changed.  In that case, return
     the phase itself.  */

  return value < 0 ? interpreter->phase : value;
}

/* Project the specified vector VX and VY onto the unit vector that is
   INTERPRETER's projection vector, assuming that INTERPRETER's
   projection vector is on the X axis.

   Value is the magnitude of the projected vector.  */

static sfnt_f26dot6
sfnt_project_onto_x_axis_vector (sfnt_f26dot6 vx, sfnt_f26dot6 vy,
				 struct sfnt_interpreter *interpreter)
{
  return vx;
}

/* Project the specified vector VX and VY onto the unit vector that is
   INTERPRETER's projection vector, assuming that INTERPRETER's
   projection vector is on the Y axis.

   Value is the magnitude of the projected vector.  */

static sfnt_f26dot6
sfnt_project_onto_y_axis_vector (sfnt_f26dot6 vx, sfnt_f26dot6 vy,
				 struct sfnt_interpreter *interpreter)
{
  return vy;
}

/* Calculate AX * BX + AY * BY divided by 16384.  */

static int32_t
sfnt_dot_fix_14 (int32_t ax, int32_t ay, int bx, int by)
{
  int32_t m, s, hi1, hi2, hi;
  uint32_t l, lo1, lo2, lo;


  /* Compute ax*bx as 64-bit value.  */
  l = (uint32_t) ((ax & 0xffffu) * bx);
  m = (ax >> 16) * bx;

  lo1 = l + ((uint32_t) m << 16);
  hi1 = (m >> 16) + ((int32_t) l >> 31) + (lo1 < l);

  /* Compute ay*by as 64-bit value.  */
  l = (uint32_t) ((ay & 0xffffu) * by);
  m = (ay >> 16) * by;

  lo2 = l + ((uint32_t) m << 16);
  hi2 = (m >> 16) + ((int32_t) l >> 31) + (lo2 < l);

  /* add them */
  lo = lo1 + lo2;
  hi = hi1 + hi2 + (lo < lo1);

  /* divide the result by 2^14 with rounding */
  s   = hi >> 31;
  l   = lo + (uint32_t) s;
  hi += s + (l < lo);
  lo  = l;

  l   = lo + 0x2000u;
  hi += (l < lo);

  return (int32_t) (((uint32_t) hi << 18) | (l >> 14));
}

/* Project the specified vector VX and VY onto the unit vector that is
   INTERPRETER's projection vector, making only the assumption that the
   projection vector is a valid unit vector.

   Value is the magnitude of the projected vector.  */

static sfnt_f26dot6
sfnt_project_onto_any_vector (sfnt_f26dot6 vx, sfnt_f26dot6 vy,
			      struct sfnt_interpreter *interpreter)
{
  return sfnt_dot_fix_14 (vx, vy,
			  interpreter->state.projection_vector.x,
			  interpreter->state.projection_vector.y);
}

/* Move the point at *X, *Y by DISTANCE along INTERPRETER's freedom
   vector.  Set *FLAGS where appropriate and when non-NULL.

   Assume both vectors are aligned to the X axis.  */

static void
sfnt_move_x (sfnt_f26dot6 *x, sfnt_f26dot6 *y,
	     struct sfnt_interpreter *interpreter,
	     sfnt_f26dot6 distance, unsigned char *flags)
{
  *x = sfnt_add (*x, distance);

  if (flags)
    *flags |= SFNT_POINT_TOUCHED_X;
}

/* Move the point at *X, *Y by DISTANCE along INTERPRETER's freedom
   vector.  Set *FLAGS where appropriate and when non-NULL.

   Assume both vectors are aligned to the Y axis.  */

static void
sfnt_move_y (sfnt_f26dot6 *x, sfnt_f26dot6 *y,
	     struct sfnt_interpreter *interpreter,
	     sfnt_f26dot6 distance, unsigned char *flags)
{
  *y = sfnt_add (*y, distance);

  if (flags)
    *flags |= SFNT_POINT_TOUCHED_Y;
}

/* Move the point at *X, *Y by DISTANCE along INTERPRETER's freedom
   vector.  Set *FLAGS where appropriate and when non-NULL.  */

static void
sfnt_move (sfnt_f26dot6 *x, sfnt_f26dot6 *y,
	   struct sfnt_interpreter *interpreter,
	   sfnt_f26dot6 distance, unsigned char *flags)
{
  sfnt_f26dot6 versor;
  sfnt_f2dot14 dot_product;

  dot_product = interpreter->state.vector_dot_product;

  /* Not actually 26.6, but the multiply-divisions below cancel each
     other out, so the result is 26.6.  */
  versor = interpreter->state.freedom_vector.x;

  if (versor)
    {
      /* Move along X axis, converting the distance to the freedom
	 vector.  */
      *x = sfnt_add (*x, sfnt_multiply_divide_signed (distance,
						      versor,
						      dot_product));

      if (flags)
	*flags |= SFNT_POINT_TOUCHED_X;
    }

  versor = interpreter->state.freedom_vector.y;

  if (versor)
    {
      /* Move along X axis, converting the distance to the freedom
	 vector.  */
      *y = sfnt_add (*y, sfnt_multiply_divide_signed (distance,
						      versor,
						      dot_product));

      if (flags)
	*flags |= SFNT_POINT_TOUCHED_Y;
    }
}

/* Validate the graphics state GS.
   Establish function pointers for rounding and projection.
   Establish dot product used to convert vector distances between
   each other.  */

static void
sfnt_validate_gs (struct sfnt_graphics_state *gs)
{
  /* Establish the function used for rounding based on the round
     state.  */

  switch (gs->round_state)
    {
    case 5: /* Rounding off.  */
      gs->round = sfnt_round_none;
      break;

    case 0: /* Round to half grid.  */
      gs->round = sfnt_round_to_half_grid;
      break;

    case 1: /* Round to grid.  */
      gs->round = sfnt_round_to_grid;
      break;

    case 2: /* Round to double grid.  */
      gs->round = sfnt_round_to_double_grid;
      break;

    case 4: /* Round up to grid.  */
      gs->round = sfnt_round_up_to_grid;
      break;

    case 3: /* Round down to grid.  */
      gs->round = sfnt_round_down_to_grid;
      break;

    case 6: /* Fine grained rounding.  */
      gs->round = sfnt_round_super;
      break;

    case 7: /* Fine grained rounding 45 degree variant.  */
      gs->round = sfnt_round_super45;
      break;
    }

  /* Establish the function used for vector projection.
     When the projection vector is an axis vector, a fast
     version can be used.  */

  if (gs->projection_vector.x == 040000)
    gs->project = sfnt_project_onto_x_axis_vector;
  else if (gs->projection_vector.y == 0x40000)
    gs->project = sfnt_project_onto_y_axis_vector;
  else
    gs->project = sfnt_project_onto_any_vector;

  /* Compute dot product of the freedom and projection vectors.
     Handle the common case where the freedom vector is aligned
     to an axis.  */

  if (gs->freedom_vector.x == 040000)
    gs->vector_dot_product = gs->projection_vector.x;
  else if (gs->freedom_vector.y == 040000)
    gs->vector_dot_product = gs->projection_vector.y;
  else
    /* Actually calculate the dot product.  */
    gs->vector_dot_product = ((((long) gs->projection_vector.x
				* gs->freedom_vector.x)
			       + ((long) gs->projection_vector.y
				  * gs->freedom_vector.y))
			      / 16384);

  /* Now figure out which function to use to move distances.  Handle
     the common case where both the freedom and projection vectors are
     aligned to an axis.  */

  if (gs->freedom_vector.x == 040000
      && gs->projection_vector.y == 040000)
    gs->move = sfnt_move_x;
  else if (gs->freedom_vector.y == 040000
	   && gs->projection_vector.y == 040000)
    gs->move = sfnt_move_y;
  else
    gs->move = sfnt_move;
}

/* Set the X and Y versors of the freedom vector of INTERPRETER's
   graphics state to the specified X and Y, in 2.14 fixed point
   format.  */

static void
sfnt_set_freedom_vector (struct sfnt_interpreter *interpreter,
			 sfnt_f2dot14 x, sfnt_f2dot14 y)
{
  interpreter->state.freedom_vector.x = x;
  interpreter->state.freedom_vector.y = y;

  sfnt_validate_gs (&interpreter->state);
}

/* Set the X and Y versors of the projection vector of INTERPRETER's
   graphics state to the specified X and Y, in 2.14 fixed point
   format.  */

static void
sfnt_set_projection_vector (struct sfnt_interpreter *interpreter,
			    sfnt_f2dot14 x, sfnt_f2dot14 y)
{
  interpreter->state.projection_vector.x = x;
  interpreter->state.projection_vector.y = y;

  sfnt_validate_gs (&interpreter->state);
}

/* Execute the program now loaded into INTERPRETER.
   WHY specifies why the interpreter is being run, and is used to
   control the behavior of instructions such IDEF[] and FDEF[].

   Control may be transferred to INTERPRETER->trap if interpretation
   fails.  */

static void
sfnt_interpret_run (struct sfnt_interpreter *interpreter,
		    enum sfnt_interpreter_run_context why)
{
  unsigned char opcode;
  bool is_prep;

  /* Determine whether or not this is the control value program.  */
  is_prep = (why == SFNT_RUN_CONTEXT_CONTROL_VALUE_PROGRAM);

#ifdef TEST
  /* Allow testing control value program instructions as well.  */
  if (why == SFNT_RUN_CONTEXT_TEST)
    is_prep = true;
#endif

  while (interpreter->IP < interpreter->num_instructions)
    {
      opcode = interpreter->instructions[interpreter->IP];

      switch (opcode)
	{
	case 0x00:  /* SVTCA y  */
	  SVTCAy ();
	  break;

	case 0x01:  /* SVTCA x  */
	  SVTCAx ();
	  break;

	case 0x02:  /* SPvTCA y */
	  SPvTCAy ();
	  break;

	case 0x03:  /* SPvTCA x */
	  SPvTCAx ();
	  break;

	case 0x04:  /* SFvTCA y */
	  SFvTCAy ();
	  break;

	case 0x05:  /* SFvTCA x */
	  SFvTCAx ();
	  break;

	case 0x06: /* SPvTL // */
	case 0x07: /* SPvTL +  */
	  SPVTL ();
	  break;

	case 0x08:  /* SFvTL // */
	case 0x09:  /* SFvTL +  */
	  SFVTL ();
	  break;

	case 0x0A:  /* SPvFS */
	  SPVFS ();
	  break;

	case 0x0B:  /* SFvFS */
	  SFVFS ();
	  break;

	case 0x0C:  /* GPv */
	  GPV ();
	  break;

	case 0x0D:  /* GFv */
	  GFV ();
	  break;

	case 0x0E:  /* SFvTPv */
	  SFVTPV ();
	  break;

	case 0x0F:  /* ISECT  */
	  ISECT ();
	  break;

	case 0x10:  /* SRP0 */
	  SRP0 ();
	  break;

	case 0x11:  /* SRP1 */
	  SRP1 ();
	  break;

	case 0x12:  /* SRP2 */
	  SRP2 ();
	  break;

	case 0x13:  /* SZP0 */
	  SZP0 ();
	  break;

	case 0x14:  /* SZP1 */
	  SZP1 ();
	  break;

	case 0x15:  /* SZP2 */
	  SZP2 ();
	  break;

	case 0x16:  /* SZPS */
	  SZPS ();
	  break;

	case 0x17:  /* SLOOP */
	  SLOOP ();
	  break;

	case 0x18:  /* RTG */
	  RTG ();
	  break;

	case 0x19:  /* RTHG */
	  RTHG ();
	  break;

	case 0x1A:  /* SMD */
	  SMD ();
	  break;

	case 0x1B:  /* ELSE */
	  ELSE ();
	  break;

	case 0x1C:  /* JMPR */
	  JMPR ();
	  break;

	case 0x1D:  /* SCVTCI */
	  SCVTCI ();
	  break;

	case 0x1E:  /* SSWCI */
	  SSWCI ();
	  break;

	case 0x1F:  /* SSW */
	  SSW ();
	  break;

	case 0x20:  /* DUP */
	  DUP ();
	  break;

	case 0x21:  /* POP */
	  POP ();
	  break;

	case 0x22:  /* CLEAR */
	  CLEAR ();
	  break;

	case 0x23:  /* SWAP */
	  SWAP ();
	  break;

	case 0x24:  /* DEPTH */
	  DEPTH ();
	  break;

	case 0x25:  /* CINDEX */
	  CINDEX ();
	  break;

	case 0x26:  /* MINDEX */
	  MINDEX ();
	  break;

	case 0x27:  /* ALIGNPTS */
	  ALIGNPTS ();
	  break;

	case 0x28:  /* RAW */
	  RAW ();
	  break;

	case 0x29:  /* UTP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x2A:  /* LOOPCALL */
	  LOOPCALL ();
	  break;

	case 0x2B:  /* CALL */
	  CALL ();
	  break;

	case 0x2C:  /* FDEF */
	  FDEF ();
	  break;

	case 0x2D:  /* ENDF */
	  ENDF ();
	  break;

	case 0x2E:  /* MDAP */
	case 0x2F:  /* MDAP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x30:  /* IUP */
	case 0x31:  /* IUP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x32:  /* SHP */
	case 0x33:  /* SHP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x34:  /* SHC */
	case 0x35:  /* SHC */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x36:  /* SHZ */
	case 0x37:  /* SHZ */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x38:  /* SHPIX */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x39:  /* IP    */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x3A:  /* MSIRP */
	case 0x3B:  /* MSIRP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x3C:  /* AlignRP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x3D:  /* RTDG */
	  RTDG ();
	  break;

	case 0x3E:  /* MIAP */
	case 0x3F:  /* MIAP */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x40:  /* NPUSHB */
	  NPUSHB ();
	  break;

	case 0x41:  /* NPUSHW */
	  NPUSHW ();
	  break;

	case 0x42:  /* WS */
	  WS ();
	  break;

	case 0x43:  /* RS */
	  RS ();
	  break;

	case 0x44:  /* WCVTP */
	  WCVTP ();
	  break;

	case 0x45:  /* RCVT */
	  RCVT ();
	  break;

	case 0x46:  /* GC */
	case 0x47:  /* GC */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x48:  /* SCFS */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x49:  /* MD */
	case 0x4A:  /* MD */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x4B:  /* MPPEM */
	  MPPEM ();
	  break;

	case 0x4C:  /* MPS */
	  MPS ();
	  break;

	case 0x4D:  /* FLIPON */
	  FLIPON ();
	  break;

	case 0x4E:  /* FLIPOFF */
	  FLIPOFF ();
	  break;

	case 0x4F:  /* DEBUG */
	  DEBUG ();
	  break;

	case 0x50:  /* LT */
	  LT ();
	  break;

	case 0x51:  /* LTEQ */
	  LTEQ ();
	  break;

	case 0x52:  /* GT */
	  GT ();
	  break;

	case 0x53:  /* GTEQ */
	  GTEQ ();
	  break;

	case 0x54:  /* EQ */
	  EQ ();
	  break;

	case 0x55:  /* NEQ */
	  NEQ ();
	  break;

	case 0x56:  /* ODD */
	  ODD ();
	  break;

	case 0x57:  /* EVEN */
	  EVEN ();
	  break;

	case 0x58:  /* IF */
	  IF ();
	  break;

	case 0x59:  /* EIF */
	  EIF ();
	  break;

	case 0x5A:  /* AND */
	  AND ();
	  break;

	case 0x5B:  /* OR */
	  OR ();
	  break;

	case 0x5C:  /* NOT */
	  NOT ();
	  break;

	case 0x5D:  /* DELTAP1 */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x5E:  /* SDB */
	  SDB ();
	  break;

	case 0x5F:  /* SDS */
	  SDS ();
	  break;

	case 0x60:  /* ADD */
	  ADD ();
	  break;

	case 0x61:  /* SUB */
	  SUB ();
	  break;

	case 0x62:  /* DIV */
	  DIV ();
	  break;

	case 0x63:  /* MUL */
	  MUL ();
	  break;

	case 0x64:  /* ABS */
	  ABS ();
	  break;

	case 0x65:  /* NEG */
	  NEG ();
	  break;

	case 0x66:  /* FLOOR */
	  FLOOR ();
	  break;

	case 0x67:  /* CEILING */
	  CEILING ();
	  break;

	case 0x68:  /* ROUND */
	case 0x69:  /* ROUND */
	case 0x6A:  /* ROUND */
	case 0x6B:  /* ROUND */
	  ROUND ();
	  break;

	case 0x6C:  /* NROUND */
	case 0x6D:  /* NROUND */
	case 0x6E:  /* NRRUND */
	case 0x6F:  /* NROUND */
	  NROUND ();
	  break;

	case 0x70:  /* WCVTF */
	  WCVTF ();
	  break;

	case 0x71:  /* DELTAP2 */
	case 0x72:  /* DELTAP3 */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x73:  /* DELTAC1 */
	  DELTAC1 ();
	  break;

	case 0x74:  /* DELTAC2 */
	  DELTAC2 ();
	  break;

	case 0x75:  /* DELTAC3 */
	  DELTAC3 ();
	  break;

	case 0x76:  /* SROUND */
	  SROUND ();
	  break;

	case 0x77:  /* S45Round */
	  S45ROUND ();
	  break;

	case 0x78:  /* JROT */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x79:  /* JROF */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x7A:  /* ROFF */
	  ROFF ();
	  break;

	case 0x7B:  /* ???? */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x7C:  /* RUTG */
	  RUTG ();
	  break;

	case 0x7D:  /* RDTG */
	  RDTG ();
	  break;

	case 0x7E:  /* SANGW */
	  SANGW ();
	  break;

	case 0x7F:  /* AA */
	  AA ();
	  break;

	case 0x80:  /* FLIPPT */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x81:  /* FLIPRGON */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x82:  /* FLIPRGOFF */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x83:  /* UNKNOWN */
	case 0x84:  /* UNKNOWN */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x85:  /* SCANCTRL */
	  SCANCTRL ();
	  break;

	case 0x86:  /* SDPvTL */
	case 0x87:  /* SDPvTL */
	  NOT_IMPLEMENTED ();
	  break;

	case 0x88:  /* GETINFO */
	  GETINFO ();
	  break;

	case 0x89:  /* IDEF */
	  IDEF ();
	  break;

	case 0x8A:  /* ROLL */
	  ROLL ();
	  break;

	case 0x8B:  /* MAX */
	  _MAX ();
	  break;

	case 0x8C:  /* MIN */
	  _MIN ();
	  break;

	  /* Scan or dropout control is not implemented.  Instead, 256
	     grays are used to display pixels which are partially
	     turned on.  */
	case 0x8D:  /* SCANTYPE */
	  SCANTYPE ();
	  break;

	case 0x8E:  /* INSTCTRL */
	  INSTCTRL ();
	  break;

	case 0x8F:  /* ADJUST */
	case 0x90:  /* ADJUST */
	  NOT_IMPLEMENTED ();
	  break;

	default:
	  if (opcode >= 0xE0) /* MIRP */
	    NOT_IMPLEMENTED ();
	  else if (opcode >= 0xC0) /* MDRP */
	    NOT_IMPLEMENTED ();
	  else if (opcode >= 0xB8) /* PUSHW */
	    {
	      PUSHW ();
	    }
	  else if (opcode >= 0xB0) /* PUSHB */
	    {
	      PUSHB ();
	    }
	  else
	    NOT_IMPLEMENTED ();
	}

      /* In the case of an NPUSHB or NPUSHW instruction,
	 interpreter->IP has only been increased to skip over the
	 extra bytes, and not the byte containing the instruction
	 itself.  */
      interpreter->IP++;

      /* This label is used by instructions to continue without
	 incrementing IP.  It is used by instructions which set IP
	 themselves, such as ELSE, IF, FDEF, IDEF and JMPR.  */
    skip_step:
      continue;
    }
}

/* Execute the font program FPGM using INTERPRETER.
   This must only be called once per interpreter, else behavior is
   undefined.

   Value is NULL upon success, else it is a string describing the
   reason for failure.  */

static const char *
sfnt_interpret_font_program (struct sfnt_interpreter *interpreter,
			     struct sfnt_fpgm_table *fpgm)
{
  if (setjmp (interpreter->trap))
    return interpreter->trap_reason;

  /* Set up the interpreter to evaluate the font program.  */
  interpreter->IP = 0;
  interpreter->SP = interpreter->stack;
  interpreter->instructions = fpgm->instructions;
  interpreter->num_instructions = fpgm->num_instructions;

  sfnt_interpret_run (interpreter, SFNT_RUN_CONTEXT_FONT_PROGRAM);
  return NULL;
}

/* Execute the control value program PREP using INTERPRETER.

   Return NULL and the graphics state after the execution of the
   program in *STATE, or a string describing the reason for a failure
   to interpret the program.  */

static const char *
sfnt_interpret_control_value_program (struct sfnt_interpreter *interpreter,
				      struct sfnt_prep_table *prep,
				      struct sfnt_graphics_state *state)
{
  if (setjmp (interpreter->trap))
    return interpreter->trap_reason;

  /* Set up the interpreter to evaluate the control value program.  */
  interpreter->IP = 0;
  interpreter->SP = interpreter->stack;
  interpreter->instructions = prep->instructions;
  interpreter->num_instructions = prep->num_instructions;

  sfnt_interpret_run (interpreter,
		      SFNT_RUN_CONTEXT_CONTROL_VALUE_PROGRAM);

  /* If instruct_control & 4, then changes to the graphics state made
     in this program should be reverted.  */

  if (interpreter->state.instruct_control & 4)
    sfnt_init_graphics_state (&interpreter->state);

  /* Save the graphics state upon success.  */
  memcpy (state, &interpreter->state, sizeof *state);
  return NULL;
}



/* Glyph hinting.  The routines here perform hinting on simple and
   compound glyphs.

   In order to keep the hinting mechanism separate from the rest of
   the code, the routines here perform outline decomposition and
   scaling separately.  It might be nice to fix that in the
   future.  */

/* Structure describing a single scaled and fitted outline.  */

struct sfnt_scaled_outline
{
  /* The number of points in this contour, including the two phantom
     points at the end.  */
  size_t num_points;

  /* The number of contours in this outline.  */
  size_t num_contours;

  /* The end points of each contour.  */
  size_t *contour_end_points;

  /* The points of each contour, with two additional phantom points at
     the end.  */
  sfnt_f26dot6 *restrict x_points, *restrict y_points;
};



/* Compute phantom points for the specified glyph GLYPH.  Use the
   unscaled metrics specified in METRICS, and the 16.16 fixed point
   scale SCALE.

   Place the X and Y coordinates of the first phantom point in *X1 and
   *Y1, and those of the second phantom point in *X2 and *Y2.  */

static void
sfnt_compute_phantom_points (struct sfnt_glyph *glyph,
			     struct sfnt_glyph_metrics *metrics,
			     sfnt_fixed scale,
			     sfnt_f26dot6 *x1, sfnt_f26dot6 *y1,
			     sfnt_f26dot6 *x2, sfnt_f26dot6 *y2)
{
  sfnt_fword f1, f2;

  /* Two ``phantom points'' are appended to each outline by the scaler
     prior to instruction interpretation.  One of these points
     represents the left-side bearing distance from xmin, while the
     other represents the advance width.  Both are then used after the
     hinting process to ensure that the reported glyph metrics are
     consistent with the hinted outline.  */

  /* First compute both values in fwords.  */
  f1 = glyph->xmin - metrics->lbearing;
  f2 = f1 + metrics->advance;

  /* Next, scale both up.  */
  *x1 = sfnt_mul_f26dot6_fixed (f1 * 64, scale);
  *x2 = sfnt_mul_f26dot6_fixed (f2 * 64, scale);

  /* Clear y1 and y2.  */
  *y1 = 0;
  *y2 = 0;
}

/* Load the simple glyph GLYPH into the specified INTERPRETER, scaling
   it up by INTERPRETER's scale, and run its glyph program if
   present.  Use the unscaled metrics specified in METRICS.

   Upon success, return NULL and the resulting points and contours in
   *VALUE.  Else, value is the reason interpretation failed.  */

static const char *
sfnt_interpret_simple_glyph (struct sfnt_glyph *glyph,
			     struct sfnt_interpreter *interpreter,
			     struct sfnt_glyph_metrics *metrics,
			     struct sfnt_scaled_outline **value)
{
  size_t zone_size, temp, outline_size, i;
  struct sfnt_interpreter_zone *zone;
  sfnt_f26dot6 phantom_point_1_x;
  sfnt_f26dot6 phantom_point_1_y;
  sfnt_f26dot6 phantom_point_2_x;
  sfnt_f26dot6 phantom_point_2_y;
  sfnt_f26dot6 tem;
  bool zone_was_allocated;
  struct sfnt_scaled_outline *outline;

  zone_size = 0;
  zone_was_allocated = false;

  /* Calculate the size of the zone structure.

     This should include four longs for each point (including two
     phantom points at the end), and then one size_t and one char for
     each point, once again including the phantom points, and finally
     the size of the zone structure.  */

  if (INT_MULTIPLY_WRAPV (glyph->simple->number_of_points + 2,
			  sizeof *zone->x_points * 4,
			  &temp)
      || INT_ADD_WRAPV (temp, zone_size, &zone_size)
      || INT_MULTIPLY_WRAPV (glyph->simple->number_of_points + 2,
			     sizeof *zone->contour_end_points,
			     &temp)
      || INT_ADD_WRAPV (temp, zone_size, &zone_size)
      || INT_MULTIPLY_WRAPV (glyph->simple->number_of_points + 2,
			     sizeof *zone->flags,
			     &temp)
      || INT_ADD_WRAPV (temp, zone_size, &zone_size)
      || INT_ADD_WRAPV (sizeof (*zone), zone_size, &zone_size))
    return "Glyph exceeded maximum permissible size";

  /* Don't use malloc if possible.  */

  if (zone_size <= 1024 * 16)
    zone = alloca (zone_size);
  else
    {
      zone = xmalloc (zone_size);
      zone_was_allocated = true;
    }

  /* Now load the zone with data.  */
  zone->num_points = glyph->simple->number_of_points + 2;
  zone->num_contours = glyph->number_of_contours;
  zone->contour_end_points = (size_t *) (glyph + 1);
  zone->x_points = (sfnt_f26dot6 *) (zone->contour_end_points
				     + zone->num_points);
  zone->y_points = zone->x_points + zone->num_points;
  zone->x_current = zone->y_points + zone->num_points;
  zone->y_current = zone->x_current + zone->num_points;
  zone->flags = (unsigned char *) (zone->y_current
				   + zone->num_points);

  /* Load x_points and x_current.  */
  for (i = 0; i < glyph->simple->number_of_points; ++i)
    {
      /* Load the fword.  */
      tem = glyph->simple->x_coordinates[i];

      /* Scale that fword.  */
      tem = sfnt_mul_f26dot6_fixed (tem * 64, interpreter->scale);

      /* Set x_points and x_current.  */
      zone->x_points[i] = tem;
      zone->x_current[i] = tem;
    }

  /* Compute phantom points.  */
  sfnt_compute_phantom_points (glyph, metrics, interpreter->scale,
			       &phantom_point_1_x, &phantom_point_1_y,
			       &phantom_point_2_x, &phantom_point_2_y);

  /* Load phantom points.  */
  zone->x_points[i] = phantom_point_1_x;
  zone->x_points[i + 1] = phantom_point_2_x;

  /* Load y_points and y_current, along with flags.  */
  for (i = 0; i < glyph->simple->number_of_points; ++i)
    {
      /* Load the fword.  */
      tem = glyph->simple->y_coordinates[i];

      /* Scale that fword.  */
      tem = sfnt_mul_f26dot6_fixed (tem * 64, interpreter->scale);

      /* Set y_points and y_current.  */
      zone->y_points[i] = tem;
      zone->y_current[i] = tem;

      /* Set flags.  */
      zone->flags[i] = (glyph->simple->flags[i]
			& ~SFNT_POINT_TOUCHED_BOTH);
    }

  /* Load phantom points.  */
  zone->y_points[i] = phantom_point_1_y;
  zone->y_points[i + 1] = phantom_point_2_y;

  /* Load contour end points.  */
  for (i = 0; i < zone->num_contours; ++i)
    zone->contour_end_points[i]
      = glyph->simple->end_pts_of_contours[i];

  /* Load the glyph program.  */
  interpreter->IP = 0;
  interpreter->SP = interpreter->stack;
  interpreter->instructions = glyph->simple->instructions;
  interpreter->num_instructions = glyph->simple->instruction_length;
  interpreter->glyph_zone = zone;

  if (setjmp (interpreter->trap))
    {
      if (zone_was_allocated)
	xfree (zone);

      interpreter->glyph_zone = NULL;
      return interpreter->trap_reason;
    }

  sfnt_interpret_run (interpreter, SFNT_RUN_CONTEXT_GLYPH_PROGRAM);

  /* Now that the program has been run, build the scaled outline.  */

  outline_size = sizeof (*outline);
  outline_size += (zone->num_contours
		   * sizeof *outline->contour_end_points);
  outline_size += (zone->num_points
		   * sizeof *outline->x_points * 2);

  /* Allocate the outline.  */
  outline = xmalloc (outline_size);
  outline->num_points = zone->num_points;
  outline->num_contours = zone->num_contours;
  outline->contour_end_points = (size_t *) (outline + 1);
  outline->x_points = (sfnt_f26dot6 *) (outline->contour_end_points
					+ outline->num_contours);
  outline->y_points = outline->x_points + outline->num_points;

  /* Copy over the contour endpoints and points.  */
  memcpy (outline->contour_end_points, zone->contour_end_points,
	  zone->num_contours * sizeof *outline->contour_end_points);
  memcpy (outline->x_points, zone->x_current,
	  zone->num_points * sizeof *outline->x_points);
  memcpy (outline->y_points, zone->y_current,
	  zone->num_points * sizeof *outline->y_points);

  /* Free the zone if necessary.  */
  if (zone_was_allocated)
    xfree (zone);

  /* Return the outline and NULL.  */
  *value = outline;
  return NULL;
}

#endif /* SFNT_ENABLE_HINTING */



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



/* Instruction execution tests.  */

static struct sfnt_maxp_table test_interpreter_profile =
  {
    0x00010000,
    650,
    100,
    100,
    100,
    100,
    2,
    100,
    255,
    12,
    12,
    100,
    5000,
    100,
    1,
  };

static sfnt_fword test_cvt_values[] =
  {
    100, 100, -100, -100, 50, 50, 50, 50, 0, 0,
  };

static struct sfnt_cvt_table test_interpreter_cvt =
  {
    10,
    test_cvt_values,
  };

static struct sfnt_head_table test_interpreter_head =
  {
    0x00010000,
    0x00010000,
    0,
    0x5f0f3cf5,
    0,
    800,
    0,
    0,
    0,
    0,
    -312,
    -555,
    1315,
    2163,
    0,
    12,
    0,
    0,
    0,
  };

static struct sfnt_interpreter *
sfnt_make_test_interpreter (void)
{
  return sfnt_make_interpreter (&test_interpreter_profile,
				&test_interpreter_cvt,
				&test_interpreter_head,
				17, 17);
}

struct sfnt_interpreter_test
{
  const char *name;
  unsigned char *instructions;
  int num_instructions;
  void *arg;
  void (*check) (struct sfnt_interpreter *, void *, bool);
};

static void
sfnt_run_interpreter_test (struct sfnt_interpreter_test *test,
			   struct sfnt_interpreter *interpreter)
{
  fprintf (stderr, "Testing %s: ", test->name);

  if (setjmp (interpreter->trap))
    test->check (interpreter, test->arg, true);
  else
    {
      interpreter->IP = 0;
      interpreter->SP = interpreter->stack;
      interpreter->instructions = test->instructions;
      interpreter->num_instructions = test->num_instructions;

      sfnt_interpret_run (interpreter, SFNT_RUN_CONTEXT_TEST);
      test->check (interpreter, test->arg, false);
    }
}

struct sfnt_generic_test_args
{
  uint32_t *expected_stack;
  int expected_stack_elements;
  bool expected_trap;
  int expected_IP;
};

static void
sfnt_generic_check (struct sfnt_interpreter *interpreter,
		    void *arg, bool trap)
{
  struct sfnt_generic_test_args *args;
  int i;

  args = arg;

  if (((interpreter->SP - interpreter->stack)
       != args->expected_stack_elements))
    {
      fprintf (stderr,
	       "failed at IP %d:%d (expected %d stack elements,"
	       " got %td); last trap string: %s\n",
	       interpreter->call_depth, interpreter->IP,
	       args->expected_stack_elements,
	       interpreter->SP - interpreter->stack,
	       ((trap && interpreter->trap_reason)
		? interpreter->trap_reason
		: "NULL"));

      for (i = 0; i < interpreter->SP - interpreter->stack; ++i)
	fprintf (stderr, "%8d ", (int) interpreter->stack[i]);
      fprintf (stderr, "\n");
      return;
    }

  if (memcmp (interpreter->stack, args->expected_stack,
	      ((char *) interpreter->SP
	       - (char *) interpreter->stack)))
    {
      fprintf (stderr, "failed (inconsistent stack elements)\n"
	       "machine stack ------------------------->\n");

      for (i = 0; i < args->expected_stack_elements; ++i)
	fprintf (stderr, "%8d ", (int) interpreter->stack[i]);

      fprintf (stderr,
	       "\nexpected stack ------------------------>\n");

      for (i = 0; i < args->expected_stack_elements; ++i)
	fprintf (stderr, "%8d ", (int) args->expected_stack[i]);

      fprintf (stderr, "\n");
      return;
    }

  if (args->expected_IP != -1
      && interpreter->IP != args->expected_IP)
    {
      fprintf (stderr, "failed (IP is %d, not %d)\n",
	       interpreter->IP, args->expected_IP);
      return;
    }

  if (trap)
    {
      if (args->expected_trap)
	fprintf (stderr, "passed (with trap %s)\n",
		 interpreter->trap_reason);
      else
	fprintf (stderr, "failed (unexpected trap %s)\n",
		 interpreter->trap_reason);

      return;
    }

  if (args->expected_trap)
    fprintf (stderr, "failed, trap not encountered\n");
  else
    fprintf (stderr, "passed\n");

  return;
}

static void
sfnt_check_srp0 (struct sfnt_interpreter *interpreter,
		 void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed (unexpected trap %s)\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->state.rp0 != 0)
    {
      fprintf (stderr, "failed, rp0 is not 0, but %d\n",
	       interpreter->state.rp0);
      return;
    }

  if (interpreter->state.rp1 != 1)
    {
      fprintf (stderr, "failed, rp1 is not 1, but %d\n",
	       interpreter->state.rp1);
      return;
    }

  if (interpreter->state.rp2 != 2)
    {
      fprintf (stderr, "failed, rp2 is not 2, but %d\n",
	       interpreter->state.rp2);
      return;
    }

  if (interpreter->SP != interpreter->stack)
    {
      fprintf (stderr, "failed, stack not empty\n");
      return;
    }

  fprintf (stderr, "passed\n");
  return;
}

static void
sfnt_check_szp0 (struct sfnt_interpreter *interpreter,
		 void *arg, bool trap)
{
  if (!trap)
    {
      fprintf (stderr, "failed, expected trap\n");
      return;
    }

  if (interpreter->state.zp0 != 1
      || interpreter->state.zp1 != 1
      || interpreter->state.zp2 != 0)
    {
      fprintf (stderr,
	       "failed, unexpected values of zone pointers: %d %d %d\n",
	       interpreter->state.zp0, interpreter->state.zp1,
	       interpreter->state.zp2);
      return;
    }

  if (interpreter->SP != interpreter->stack)
    {
      fprintf (stderr, "failed, stack not empty\n");
      return;
    }

  fprintf (stderr, "passed with expected trap %s\n",
	   interpreter->trap_reason);
  return;
}

static void
sfnt_check_sloop (struct sfnt_interpreter *interpreter,
		  void *arg, bool trap)
{
  if (interpreter->state.loop != 2)
    {
      fprintf (stderr, "failed, GS->loop should be 2, not %d\n",
	       interpreter->state.loop);
      return;
    }

  if (!trap)
    {
      fprintf (stderr, "failed, expected trap\n");
      return;
    }

  if (interpreter->SP != interpreter->stack)
    {
      fprintf (stderr, "failed, stack not empty\n");
      return;
    }

  fprintf (stderr, "passed with expected trap %s\n",
	   interpreter->trap_reason);
  return;
}

struct sfnt_rounding_test_args
{
  sfnt_f26dot6 value;
};

static void
sfnt_check_rounding (struct sfnt_interpreter *interpreter,
		     void *arg, bool trap)
{
  sfnt_f26dot6 value;
  struct sfnt_rounding_test_args *args;

  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap: %s\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->SP == interpreter->stack)
    {
      fprintf (stderr, "failed, empty stack\n");
      return;
    }

  value = *(interpreter->SP - 1);
  args = arg;

  if (value != args->value)
    {
      fprintf (stderr, "failed.  value is: %d %d, but wanted: %d %d\n",
	       value >> 6, value & 63, args->value >> 6,
	       args->value & 63);
      return;
    }

  fprintf (stderr, "passed, expected value %d\n", value);
  return;
}

static void
sfnt_check_smd (struct sfnt_interpreter *interpreter,
		void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (interpreter->state.minimum_distance != 32)
    {
      fprintf (stderr, "failed, expected minimum distance"
	       " of 32, got %d\n",
	       interpreter->state.minimum_distance);
      return;
    }

  fprintf (stderr, "passed\n");
  return;
}

static void
sfnt_check_scvtci (struct sfnt_interpreter *interpreter,
		   void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (interpreter->state.cvt_cut_in != 128)
    {
      fprintf (stderr, "failed, expected 128, got %d\n",
	       interpreter->state.cvt_cut_in);
      return;
    }

  fprintf (stderr, "passed\n");
  return;
}

static void
sfnt_check_sswci (struct sfnt_interpreter *interpreter,
		  void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (interpreter->state.sw_cut_in != 512)
    {
      fprintf (stderr, "failed, expected 512, got %d\n",
	       interpreter->state.sw_cut_in);
      return;
    }

  fprintf (stderr, "passed\n");
  return;
}

static void
sfnt_check_ssw (struct sfnt_interpreter *interpreter,
		void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (interpreter->state.single_width_value
      != sfnt_mul_f26dot6_fixed (-64, interpreter->scale))
    {
      fprintf (stderr, "failed, got %d at scale %d,"
	       " expected %d\n",
	       interpreter->state.single_width_value,
	       interpreter->scale,
	       sfnt_mul_f26dot6_fixed (-64, interpreter->scale));
      return;
    }

  fprintf (stderr, "passed\n");
  return;
}

static void
sfnt_check_flipon (struct sfnt_interpreter *interpreter,
		   void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (!interpreter->state.auto_flip)
    fprintf (stderr, "failed, auto flip not enabled\n");
  else
    fprintf (stderr, "pass\n");

  return;
}

static void
sfnt_check_flipoff (struct sfnt_interpreter *interpreter,
		    void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap\n");
      return;
    }

  if (interpreter->state.auto_flip)
    fprintf (stderr, "failed, auto flip not disabled\n");
  else
    fprintf (stderr, "pass\n");

  return;
}

static void
sfnt_check_sdb (struct sfnt_interpreter *interpreter,
		void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap %s\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->state.delta_base != 8)
    fprintf (stderr, "failed, delta base is %d, not 8\n",
	     interpreter->state.delta_base);
  else
    fprintf (stderr, "pass\n");

  return;
}

static void
sfnt_check_sds (struct sfnt_interpreter *interpreter,
		void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap %s\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->state.delta_shift != 1)
    fprintf (stderr, "failed, delta shift is %d, not 1\n",
	     interpreter->state.delta_shift);
  else
    fprintf (stderr, "pass\n");

  return;
}

static void
sfnt_check_scanctrl (struct sfnt_interpreter *interpreter,
		     void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap %s\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->SP != interpreter->stack)
    {
      fprintf (stderr, "failed, expected empty stack\n");
      return;
    }

  if (interpreter->state.scan_control != 1)
    fprintf (stderr, "failed, scan control is %d, not 1\n",
	     interpreter->state.scan_control);
  else
    fprintf (stderr, "pass\n");

  return;
}

static void
sfnt_check_instctrl (struct sfnt_interpreter *interpreter,
		     void *arg, bool trap)
{
  if (trap)
    {
      fprintf (stderr, "failed, unexpected trap %s\n",
	       interpreter->trap_reason);
      return;
    }

  if (interpreter->SP != interpreter->stack)
    {
      fprintf (stderr, "failed, expected empty stack\n");
      return;
    }

  if (interpreter->state.instruct_control != 2)
    fprintf (stderr, "failed, inst control is %d, not 2\n",
	     interpreter->state.instruct_control);
  else
    fprintf (stderr, "pass\n");

  return;
}

static struct sfnt_generic_test_args npushb_test_args =
  {
    (uint32_t []) { 1U, 2U, 3U, 4U, },
    4,
    true,
    6,
  };

static struct sfnt_generic_test_args npushw_test_args =
  {
    (uint32_t []) { 0x101U, 0x202U, 0x303U, 0x404U, },
    4,
    true,
    10,
  };

static struct sfnt_generic_test_args pushb_test_args =
  {
    (uint32_t []) { 1U, 2U, 3U, 4U, 5U, 6U, 7U, 8U,
		   1U, },
    9,
    true,
    11,
  };

static struct sfnt_generic_test_args pushw_test_args =
  {
    (uint32_t []) { 0x203U, 0x204U, 0x205U, 0x206U, 0x207U, 0x208U,
		   0x909U, 0x909U, (uint32_t) -1, },
    9,
    true,
    20,
  };

static struct sfnt_generic_test_args stack_overflow_test_args =
  {
    (uint32_t[100]) { },
    100,
    true,
    0,
  };

static struct sfnt_generic_test_args stack_underflow_test_args =
  {
    /* GCC BUG, this should be []! */
    (uint32_t []) { },
    0,
    true,
    4,
  };

static struct sfnt_rounding_test_args rtg_test_args =
  {
    64,
  };

static struct sfnt_rounding_test_args rtg_symmetric_test_args =
  {
    -64,
  };

static struct sfnt_rounding_test_args rtg_1_test_args =
  {
    0,
  };

static struct sfnt_rounding_test_args rtg_1_symmetric_test_args =
  {
    0,
  };

static struct sfnt_rounding_test_args rthg_test_args =
  {
    32,
  };

static struct sfnt_rounding_test_args rthg_1_test_args =
  {
    96,
  };

static struct sfnt_rounding_test_args rtdg_test_args =
  {
    32,
  };

static struct sfnt_rounding_test_args rtdg_1_test_args =
  {
    0,
  };

static struct sfnt_rounding_test_args rtdg_2_test_args =
  {
    32,
  };

static struct sfnt_rounding_test_args rtdg_3_test_args =
  {
    64,
  };

static struct sfnt_generic_test_args else_test_args =
  {
    (uint32_t []) { 77U, 90U, 83U, },
    3,
    false,
    40,
  };

static struct sfnt_generic_test_args jmpr_test_args =
  {
    /* What ends up on the stack?

       First, there are the three words that the first PUSHW[2]
       instruction has pushed:

	 0, 0xb2, -3

       After those three words are pushed, JMPR[] is called, and pops an
       offset:

	 -3

       so now the stack is:

	 0, 0xb2

       as a result of the relative jump, IP is now at the least
       significant byte of the word inside what was originally a
       PUSHW[2] instruction, 0xb2, which itself is PUSHB[2]!

       As a result of that instruction, three more bytes, including
       JMPR[] itself are pushed onto the stack, making it:

	 0, 0xb2, 255, 253, 0x1c

       Then, execution continues as usual.  4 is pushed on to the
       stack, making it:

	 0, 0xb2, 255, 253, 0x1c, 4

       Another JMPR[] pops:

	 4

       making the stack:

	 0, 0xb2, 255, 253, 0x1c

       And skips the next three padding bytes, finally reaching a
       PUSHW[0] instruction which pushes -30 onto the stack:

	 0, 0xb2, 255, 253, 0x1c, -30

       and a JMPR[] instruction, which pops:

	 -30

       making:

	 0, 0xb2, 255, 253,

       and subsequently traps, as -30 would underflow the instruction
       stream.  */
    (uint32_t []) { 0, 0xb2, 255, 253, 0x1c, },
    5,
    true,
    17,
  };

static struct sfnt_generic_test_args dup_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    5,
  };

static struct sfnt_generic_test_args pop_test_args =
  {
    (uint32_t []) { 70, 70, },
    2,
    false,
    5,
  };

static struct sfnt_generic_test_args clear_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    10,
  };

static struct sfnt_generic_test_args swap_test_args =
  {
    (uint32_t []) { 2, 1, },
    2,
    false,
    4,
  };

static struct sfnt_generic_test_args depth_test_args =
  {
    (uint32_t []) { 3, 3, 3, 3, },
    4,
    false,
    5,
  };

static struct sfnt_generic_test_args cindex_test_args =
  {
    (uint32_t []) { 0, 3, 3, 4, 0, },
    5,
    true,
    10,
  };

static struct sfnt_generic_test_args mindex_test_args =
  {
    (uint32_t []) { 0, 3, 7, 4, 4, },
    5,
    false,
    10,
  };

static struct sfnt_generic_test_args raw_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    0,
  };

static struct sfnt_generic_test_args loopcall_test_args =
  {
    (uint32_t []) { 10, },
    1,
    false,
    12,
  };

static struct sfnt_generic_test_args call_test_args =
  {
    (uint32_t []) { 11, },
    1,
    true,
    2,
  };

static struct sfnt_generic_test_args fdef_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    4,
  };

static struct sfnt_generic_test_args fdef_1_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    9,
  };

static struct sfnt_generic_test_args endf_test_args =
  {
    (uint32_t []) {  },
    0,
    true,
    0,
  };

static struct sfnt_generic_test_args ws_test_args =
  {
    (uint32_t []) { 40, },
    1,
    true,
    10,
  };

static struct sfnt_generic_test_args rs_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    2,
  };

static struct sfnt_generic_test_args wcvtp_test_args =
  {
    (uint32_t []) { 32, },
    1,
    true,
    10,
  };

static struct sfnt_generic_test_args rcvt_test_args =
  {
    (uint32_t []) { 135, },
    1,
    true,
    5,
  };

static struct sfnt_generic_test_args mppem_test_args =
  {
    (uint32_t []) { 17, },
    1,
    false,
    1,
  };

static struct sfnt_generic_test_args mps_test_args =
  {
    (uint32_t []) { 17, },
    1,
    false,
    1,
  };

static struct sfnt_generic_test_args debug_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    3,
  };

static struct sfnt_generic_test_args lt_test_args =
  {
    (uint32_t []) { 1, 0, 0, },
    3,
    false,
    12,
  };

static struct sfnt_generic_test_args lteq_test_args =
  {
    (uint32_t []) { 1, 0, 1, },
    3,
    false,
    12,
  };

static struct sfnt_generic_test_args gt_test_args =
  {
    (uint32_t []) { 0, 1, 0, },
    3,
    false,
    12,
  };

static struct sfnt_generic_test_args gteq_test_args =
  {
    (uint32_t []) { 0, 1, 1, },
    3,
    false,
    12,
  };

static struct sfnt_generic_test_args eq_test_args =
  {
    (uint32_t []) { 0, 1, 0, },
    3,
    false,
    18,
  };

static struct sfnt_generic_test_args neq_test_args =
  {
    (uint32_t []) { 1, 0, 1, },
    3,
    false,
    18,
  };

static struct sfnt_generic_test_args odd_test_args =
  {
    (uint32_t []) { 1, 0, },
    2,
    false,
    9,
  };

static struct sfnt_generic_test_args even_test_args =
  {
    (uint32_t []) { 0, 1, },
    2,
    false,
    9,
  };

static struct sfnt_generic_test_args if_test_args =
  {
    (uint32_t []) { 17, 24, 1, 2, 3, 4, 5, -1, -1,
		    88, 1, 3, },
    12,
    false,
    185,
  };

static struct sfnt_generic_test_args eif_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    3,
  };

static struct sfnt_generic_test_args and_test_args =
  {
    (uint32_t []) { 0, 0, 1, 0, },
    4,
    false,
    16,
  };

static struct sfnt_generic_test_args or_test_args =
  {
    (uint32_t []) { 1, 1, 1, 0, },
    4,
    false,
    16,
  };

static struct sfnt_generic_test_args not_test_args =
  {
    (uint32_t []) { 0, 1, },
    2,
    false,
    6,
  };

static struct sfnt_generic_test_args sds_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    5,
  };

static struct sfnt_generic_test_args add_test_args =
  {
    (uint32_t []) { 96, -1, },
    2,
    false,
    10,
  };

static struct sfnt_generic_test_args sub_test_args =
  {
    (uint32_t []) { 64, -64, 431, },
    3,
    false,
    14,
  };

static struct sfnt_generic_test_args div_test_args =
  {
    (uint32_t []) { 32, -64, },
    2,
    true,
    15,
  };

static struct sfnt_generic_test_args mul_test_args =
  {
    (uint32_t []) { 255, -255, 255, },
    3,
    false,
    16,
  };

static struct sfnt_generic_test_args abs_test_args =
  {
    (uint32_t []) { 1, 1, },
    2,
    false,
    7,
  };

static struct sfnt_generic_test_args neg_test_args =
  {
    (uint32_t []) { 1, -1, },
    2,
    false,
    7,
  };

static struct sfnt_generic_test_args floor_test_args =
  {
    (uint32_t []) { -128, -64, 0, 64, 128, },
    5,
    false,
    17,
  };

static struct sfnt_generic_test_args ceiling_test_args =
  {
    (uint32_t []) { -128, -128, -64, 0, 64, 128, 128, },
    7,
    false,
    25,
  };

static struct sfnt_generic_test_args round_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    0,
  };

static struct sfnt_generic_test_args nround_test_args =
  {
    (uint32_t []) { 63, },
    1,
    false,
    3,
  };

static struct sfnt_generic_test_args wcvtf_test_args =
  {
    (uint32_t []) { (63 * 17 * 65535 / 800) >> 10, },
    1,
    false,
    7,
  };

static struct sfnt_generic_test_args deltac1_test_args =
  {
    (uint32_t []) { ((50 * 17 * 65535 / 800) >> 10) + 8,
		    ((50 * 17 * 65535 / 800) >> 10) + 8, },
    2,
    false,
    22,
  };

static struct sfnt_generic_test_args deltac2_test_args =
  {
    (uint32_t []) { ((50 * 17 * 65535 / 800) >> 10) + 8,
		    ((50 * 17 * 65535 / 800) >> 10) + 8, },
    2,
    false,
    22,
  };

static struct sfnt_generic_test_args deltac3_test_args =
  {
    (uint32_t []) { ((50 * 17 * 65535 / 800) >> 10) + 8,
		    ((50 * 17 * 65535 / 800) >> 10) + 8, },
    2,
    false,
    22,
  };

/* Macros and instructions for detailed rounding tests.  */

/* PUSHB[0] period:phase:threshold
   SROUND[] */
#define SFNT_ROUNDING_OPERAND(period, phase, threshold)	\
  0xb0, (((unsigned char) period << 6)			\
	 | ((unsigned char) phase & 3) << 4		\
	 | ((unsigned char) threshold & 15)), 0x76

/* PUSHB[0] period:phase:threshold
   S45ROUND[] */
#define SFNT_ROUNDING_OPERAND_45(period, phase, threshold)	\
  0xb0, (((unsigned char) period << 6)				\
	 | ((unsigned char) phase & 3) << 4			\
	 | ((unsigned char) threshold & 15)), 0x77

/* PUSHB[0] value
   ROUND[] */
#define SFNT_ROUND_VALUE(value) 0xb0, value, 0x68

static unsigned char sfnt_sround_instructions[] =
  {
    SFNT_ROUNDING_OPERAND (0, 0, 8),
    SFNT_ROUND_VALUE (15),
    SFNT_ROUND_VALUE (17),
    SFNT_ROUNDING_OPERAND (1, 0, 8),
    SFNT_ROUND_VALUE (32),
    SFNT_ROUND_VALUE (16),
    SFNT_ROUNDING_OPERAND (2, 0, 8),
    SFNT_ROUND_VALUE (64),
    SFNT_ROUND_VALUE (63),
    SFNT_ROUNDING_OPERAND (0, 1, 8),
    SFNT_ROUND_VALUE (16),
    SFNT_ROUND_VALUE (24),
    SFNT_ROUNDING_OPERAND (0, 2, 8),
    SFNT_ROUND_VALUE (20),
    SFNT_ROUND_VALUE (48),
    SFNT_ROUNDING_OPERAND (0, 3, 8),
    SFNT_ROUND_VALUE (7),
    SFNT_ROUND_VALUE (70),
  };

static uint32_t sfnt_sround_values[] =
  {
    /* 0, 0, 8 = RTDG; 15 rounded to the double grid and becomes 0, 17
       is 32.  */
    0, 32,
    /* 1, 0, 8 = RTG; 32 rounded to the grid is 64, 16 is 0.  */
    64, 0,
    /* 2, 0, 8 = round to a grid separated by 128s.  64 is 128, 63 is
       0.  */
    128, 0,
    /* 0, 1, 8 = round to a double grid with a phase of 8.  16 rounds
       down to 8, 24 rounds up to 40.  */
    8, 40,
    /* 0, 2, 8 = round to a double grid with a phase of 16.  20 rounds
       down to 16, 40 rounds up to 48.  */
    16, 48,
    /* 0, 3, 8 = round to a double grid with a phase of 48.  7 rounds
       up to 16, 70 rounds up to 80.  */
    16, 80,
  };

static struct sfnt_generic_test_args sround_test_args =
  {
    sfnt_sround_values,
    ARRAYELTS (sfnt_sround_values),
    false,
    ARRAYELTS (sfnt_sround_instructions),
  };

static unsigned char sfnt_s45round_instructions[] =
  {
    SFNT_ROUNDING_OPERAND_45 (0, 0, 0),
    SFNT_ROUND_VALUE (1),
    SFNT_ROUND_VALUE (45),
  };

static uint32_t sfnt_s45round_values[] =
  {
    /* 0, 0, 0: 1 rounded to the double cubic grid becomes 45, and 46
       rounded to the double cubic grid becomes 90.  */
    45, 90,
  };

static struct sfnt_generic_test_args s45round_test_args =
  {
    sfnt_s45round_values,
    ARRAYELTS (sfnt_s45round_values),
    false,
    ARRAYELTS (sfnt_s45round_instructions),
  };

static struct sfnt_generic_test_args rutg_test_args =
  {
    (uint32_t []) { 64, 64, 0, },
    3,
    false,
    10,
  };

static struct sfnt_generic_test_args rdtg_test_args =
  {
    (uint32_t []) { 0, 0, 64, },
    3,
    false,
    10,
  };

static struct sfnt_generic_test_args sangw_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    3,
  };

static struct sfnt_generic_test_args aa_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    3,
  };

static struct sfnt_generic_test_args getinfo_test_args =
  {
    /* Pretend to be the Macintosh System 7 scaler.

       This lets the interpreter get away with only two phantom
       points, as specified in Apple's TrueType reference manual.  */
    (uint32_t []) { 2, 0, },
    2,
    false,
    6,
  };

static struct sfnt_generic_test_args idef_test_args =
  {
    (uint32_t []) { 1, 2, 3, },
    3,
    false,
    11,
  };

static struct sfnt_generic_test_args roll_test_args =
  {
    (uint32_t []) { 1, 2, 4, 5, 3, },
    5,
    false,
    7,
  };

static struct sfnt_generic_test_args roll_1_test_args =
  {
    (uint32_t []) { },
    0,
    true,
    3,
  };

static struct sfnt_generic_test_args max_test_args =
  {
    (uint32_t []) { 70, },
    1,
    false,
    6,
  };

static struct sfnt_generic_test_args min_test_args =
  {
    (uint32_t []) { -70, },
    1,
    false,
    6,
  };

static struct sfnt_generic_test_args scantype_test_args =
  {
    (uint32_t []) { },
    0,
    false,
    3,
  };

static struct sfnt_interpreter_test all_tests[] =
  {
    {
      "NPUSHB",
      /* NPUSHB[] 4 1 2 3 4
	 NPUSHB[] 5 1 2 3 4 */
      (unsigned char []) { 0x40, 4, 1, 2, 3, 4,
			   0x40, 5, 1, 2, 3, 4, },
      10,
      &npushb_test_args,
      sfnt_generic_check,
    },
    {
      "NPUSHW",
      /* NPUSHW[] 4 0x101 0x202 0x303 0x404
	 NPUSHW[] 4 0x101 0x202 0x303 0x4?? */
      (unsigned char []) { 0x41, 4, 1, 1, 2, 2, 3, 3, 4, 4,
			   0x41, 4, 1, 1, 2, 2, 3, 3, 4, },
      19,
      &npushw_test_args,
      sfnt_generic_check,
    },
    {
      "PUSHB",
      /* PUSHB[7] 1 2 3 4 5 6 7 8
	 PUSHB[0] 1
	 PUSHB[5] 1 2 3 4 5 ? */
      (unsigned char []) { 0xb7, 1, 2, 3, 4, 5, 6, 7, 8,
			   0xb0, 1,
			   0xb5, 1, 2, 3, 4, 5, },
      17,
      &pushb_test_args,
      sfnt_generic_check,
    },
    {
      "PUSHW",
      /* PUSHW[7] 2 3 2 4 2 5 2 6 2 7 2 8 9 9 9 9
	 PUSHW[0] 255 255 -- this should get sign-extended
	 PUSHW[5] 1 1 2 2 3 3 4 4 5 5 6 ? */
      (unsigned char []) { 0xbf, 2, 3, 2, 4, 2, 5, 2, 6, 2, 7, 2, 8, 9, 9, 9, 9,
			   0xb8, 255, 255,
			   0xbc, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, },
      28,
      &pushw_test_args,
      sfnt_generic_check,
    },
    {
      "that stack overflow is handled correctly",
      /* NPUSHB[] 101 0... */
      (unsigned char [103]) { 0x40, 101, },
      103,
      &stack_overflow_test_args,
      sfnt_generic_check,
    },
    {
      "that stack underflow is handled correctly",
      /* PUSHW[0] 100 100
	 POP[]
	 POP[] */
      (unsigned char []) { 0xb8, 100, 100,
			   0x21,
			   0x21, },
      5,
      &stack_underflow_test_args,
      sfnt_generic_check,
    },
    {
      "SRP0, SRP1, SRP2",
      /* PUSHB[0] 0
	 SRP0[]
	 PUSHB[0] 1
	 SRP1[]
	 PUSHB[0] 2
	 SRP2[] */
      (unsigned char []) { 0xb0, 0,
			   0x10,
			   0xb0, 1,
			   0x11,
			   0xb0, 2,
			   0x12, },
      9,
      NULL,
      sfnt_check_srp0,
    },
    {
      "SZP0, SZP1, SZP2, SZPS",
      /* PUSHB[0] 1
	 SZP0[]
	 PUSHB[0] 1
	 SZP1[]
	 PUSHB[0] 0
	 SZP2[]
	 PUSHB[0] 5
	 SZPS[]  */
      (unsigned char []) { 0xb0, 1,
			   0x13,
			   0xb0, 1,
			   0x14,
			   0xb0, 0,
			   0x15,
			   0xb0, 5,
			   0x16, },
      12,
      NULL,
      sfnt_check_szp0,
    },
    {
      "SLOOP",
      /* PUSHB[0] 2
	 SLOOP[]
	 PUSHB[0] 0
	 SLOOP[] */
      (unsigned char []) { 0xb0, 2,
			   0x17,
			   0xb0, 0,
			   0x17, },
      6,
      NULL,
      sfnt_check_sloop,
    },
    {
      "RTG",
      /* RTG[]
	 PUSHB[0] 32
	 ROUND[] */
      (unsigned char []) { 0x18,
			   0xb0, 32,
			   0x68, },
      4,
      &rtg_test_args,
      sfnt_check_rounding,
    },
    {
      "rounding symmetry",
      /* RTG[]
	 PUSHW[0] 255 -32
	 ROUND[] */
      (unsigned char []) { 0x18,
			   0xb8, 255, - (signed char) 32,
			   0x68, },
      5,
      &rtg_symmetric_test_args,
      sfnt_check_rounding,
    },
    {
      "RTG to 0",
      /* RTG[]
	 PUSHB[0] 31
	 ROUND[] */
      (unsigned char []) { 0x18,
			   0xb0, 31,
			   0x68, },
      4,
      &rtg_1_test_args,
      sfnt_check_rounding,
    },
    {
      "rounding symmetry to 0",
      /* RTG[]
	 PUSHB[0] 255 -31
	 ROUND[] */
      (unsigned char []) { 0x18,
			   0xb8, 255, - (signed char) 31,
			   0x68, },
      5,
      &rtg_1_symmetric_test_args,
      sfnt_check_rounding,
    },
    {
      "RTHG",
      /* RTHG[]
	 PUSHB[0] 0
	 ROUND[] */
      (unsigned char []) { 0x19,
			   0xb0, 0,
			   0x68, },
      4,
      &rthg_test_args,
      sfnt_check_rounding,
    },
    {
      "RTHG to 96",
      /* RTHG[]
	 PUSHB[0] 64
	 ROUND[] */
      (unsigned char []) { 0x19,
			   0xb0, 64,
			   0x68, },
      4,
      &rthg_1_test_args,
      sfnt_check_rounding,
    },
    {
      "SMD",
      /* PUSHB[0] 32
	 SMD[] */
      (unsigned char []) { 0xb0, 32,
			   0x1a, },
      3,
      NULL,
      sfnt_check_smd,
    },
    {
      "ELSE",
      /* ELSE[]
	 ;; Lots of variable length instructions
	 ;; which will not be executed, like:
	 NPUSHW[] 3 11 22 33 44 55 66
	 NPUSHB[] 1 3
	 PUSHW[2] 1 1 2 2 3 3
	 PUSHB[2] 1 2 3
	 ;; Also test nested ifs.
	 PUSHW[0] 1 1
	 IF[]
	 PUSHW[0] 1 1
	 ELSE[]
	 PUSHW[0] 1 1
	 EIF[]
	 EIF[]
	 PUSHW[0] 1 1
	 ;; the actual contents of the stack.
	 PUSHB[2] 77 90 83 */
      (unsigned char []) { 0x1b,
			   0x41, 3, 11, 22, 33, 44, 55, 66,
			   0x40, 1, 3,
			   0xba, 1, 1, 2, 2, 3, 3,
			   0xb2, 1, 2, 3,
			   0xb8, 1, 1,
			   0x58,
			   0xb8, 1, 1,
			   0x1b,
			   0xb8, 1, 1,
			   0x59,
			   0x59,
			   0xb2, 77, 90, 83, },
      40,
      &else_test_args,
      sfnt_generic_check,
    },
    {
      "JMPR",
      /* PUSHW[2] 00 00 00 PUSHB[2] 255 253 JMPR[]
	 PUSHB[0] 4
	 JMPR[]
	 255 255 255
	 PUSHW[0] 255 -30
	 JMPR[] */
      (unsigned char []) { 0xba, 00, 00, 00, 0xb2, 255, 253, 0x1c,
			   0xb0, 4,
			   0x1c,
			   255, 255, 255,
			   0xb8, 255, -30,
			   0x1c, },
      18,
      &jmpr_test_args,
      sfnt_generic_check,
    },
    {
      "SCVTCI",
      /* PUSHB[0] 128
	 SCVTCI[] */
      (unsigned char []) { 0xb0, 128,
			   0x1d, },
      3,
      NULL,
      sfnt_check_scvtci,
    },
    {
      "SSWCI",
      /* PUSHW[0] 2 0 ;; 512
	 SSWCI[] */
      (unsigned char []) { 0xb8, 2, 0,
			   0x1e, },
      4,
      NULL,
      sfnt_check_sswci,
    },
    {
      "SSW",
      /* PUSHW[0] 255 255 ; -1
	 SSW[] ; this should be converted to device-space */
      (unsigned char []) { 0xb8, 255, 255,
			   0x1f, },
      4,
      NULL,
      sfnt_check_ssw,
    },
    {
      "DUP",
      /* PUSHB[0] 70
	 DUP[]
	 POP[]
	 POP[]
	 DUP[] */
      (unsigned char []) { 0xb0, 70,
			   0x20,
			   0x21,
			   0x21,
			   0x70, },
      6,
      &dup_test_args,
      sfnt_generic_check,
    },
    {
      "POP",
      /* PUSHB[0] 70
	 DUP[]
	 DUP[]
	 POP[] */
      (unsigned char []) { 0xb0, 70,
			   0x20,
			   0x20,
			   0x21, },
      5,
      &pop_test_args,
      sfnt_generic_check,
    },
    {
      "CLEAR",
      /* PUSHB[7] 1 2 3 4 5 6 7 8
	 CLEAR[] */
      (unsigned char []) { 0xb7, 1, 2, 3, 4, 5, 6, 7, 8,
			   0x22, },
      10,
      &clear_test_args,
      sfnt_generic_check,
    },
    {
      "SWAP",
      /* PUSHB[1] 1 2
	 SWAP[] */
      (unsigned char []) { 0xb1, 1, 2,
			   0x23, },
      4,
      &swap_test_args,
      sfnt_generic_check,
    },
    {
      "DEPTH",
      /* PUSHB[2] 3 3 3
	 DEPTH[] */
      (unsigned char []) { 0xb2, 3, 3, 3,
			   0x24, },
      5,
      &depth_test_args,
      sfnt_generic_check,
    },
    {
      "CINDEX",
      /* PUSHB[4] 0 3 3 4 1
	 CINDEX[] ; pops 1, indices 4
	 CINDEX[] ; pops 4, indices 0
	 PUSHB[0] 6
	 CINDEX[] ; pops 6, trap */
      (unsigned char []) { 0xb4, 0, 3, 3, 4, 1,
			   0x25,
			   0x25,
			   0xb0, 6,
			   0x25, },
      11,
      &cindex_test_args,
      sfnt_generic_check,
    },
    {
      "MINDEX",
      /* PUSHB[6] 0 3 4 7 3 4 2
	 MINDEX[] ; pops 2, array becomes 0 3 4 7 4 3
	 MINDEX[] ; pops 3, array becomes 0 3 7 4 4 */
      (unsigned char []) { 0xb6, 0, 3, 4, 7, 3, 4, 2,
			   0x26,
			   0x26, },
      10,
      &mindex_test_args,
      sfnt_generic_check,
    },
    {
      "RAW",
      /* RAW[] */
      (unsigned char []) { 0x28, },
      1,
      &raw_test_args,
      sfnt_generic_check,
    },
    {
      "LOOPCALL",
      /* PUSHB[1] 0 2
	 FDEF[]
	 PUSHB[0] 1
	 ADD[]
	 ENDF[]
	 PUSHB[1] 10 2
	 LOOPCALL[]  */
      (unsigned char []) { 0xb1, 0, 2,
			   0x2c,
			   0xb0, 1,
			   0x60,
			   0x2d,
			   0xb1, 10, 2,
			   0x2a, },
      12,
      &loopcall_test_args,
      sfnt_generic_check,
    },
    {
      "CALL",
      /* PUSHB[1] 7 2
	 FDEF[]
	 PUSHB[0] 1
	 ADD[]
	 ENDF[]
	 PUSHB[0] 2
	 CALL[]
	 PUSHB[0] 3
	 ADD[]
	 ;; Test that infinite recursion fails.
	 PUSHB[0] 3
	 FDEF[]
	 PUSHB[0] 3
	 CALL[]
	 ENDF[]
	 PUSHB[0] 3
	 CALL[] */
      (unsigned char []) { 0xb1, 7, 2,
			   0x2c,
			   0xb0, 1,
			   0x60,
			   0x2d,
			   0xb0, 2,
			   0x2b,
			   0xb0, 3,
			   0x60,
			   0xb0, 3,
			   0x2c,
			   0xb0, 3,
			   0x2b,
			   0x2d,
			   0xb0, 3,
			   0x2b, },
      24,
      &call_test_args,
      sfnt_generic_check,
    },
    {
      "that FDEF traps inside nested definitions",
      /* PUSHB[0] 1
	 FDEF[]
	 FDEF[]
	 ENDF[]
	 ENDF[] */
      (unsigned char []) { 0xb0, 1,
			   0x2c,
			   0x2c,
			   0x2d,
			   0x2d, },
      6,
      &fdef_test_args,
      sfnt_generic_check,
    },
    {
      "that FDEF traps upon missing ENDF",
      /* PUSHB[0] 1
	 FDEF[]
	 PUSHB[3] 1 2 3 4
	 POP[]  */
      (unsigned char []) { 0xb0, 1,
			   0x2c,
			   0xb3, 1, 2, 3, 4,
			   0x21, },
      9,
      &fdef_1_test_args,
      sfnt_generic_check,
    },
    {
      "ENDF",
      /* ENDF[] */
      (unsigned char []) { 0x2d, },
      1,
      &endf_test_args,
      sfnt_generic_check,
    },
    {
      "RTDG",
      /* RTDG[]
	 PUSHB[0] 16
	 ROUND[] */
      (unsigned char []) { 0x3d,
			   0xb0, 16,
			   0x68, },
      4,
      &rtdg_test_args,
      sfnt_check_rounding,
    },
    {
      "RTDG down to 0",
      /* RTDG[]
	 PUSHB[0] 15
	 ROUND[] */
      (unsigned char []) { 0x3d,
			   0xb0, 15,
			   0x68, },
      4,
      &rtdg_1_test_args,
      sfnt_check_rounding,
    },
    {
      "RTDG down to 32",
      /* RTDG[]
	 PUSHB[0] 47
	 ROUND[] */
      (unsigned char []) { 0x3d,
			   0xb0, 47,
			   0x68, },
      4,
      &rtdg_2_test_args,
      sfnt_check_rounding,
    },
    {
      "RTDG up to 64",
      /* RTDG[]
	 PUSHB[0] 48
	 ROUND[] */
      (unsigned char []) { 0x3d,
			   0xb0, 48,
			   0x68, },
      4,
      &rtdg_3_test_args,
      sfnt_check_rounding,
    },
    {
      "WS",
      /* PUSHB[1] 240 40
	 WS[]
	 PUSHB[0] 240
	 RS[]
	 PUSHB[1] 255 40
	 WS[] */
      (unsigned char []) { 0xb1, 240, 40,
			   0x42,
			   0xb0, 240,
			   0x43,
			   0xb1, 255, 40,
			   0x42, },
      11,
      &ws_test_args,
      sfnt_generic_check,
    },
    {
      "RS",
      /* PUSHB[0] 255
	 RS[] */
      (unsigned char []) { 0xb0, 255,
			   0x43, },
      3,
      &rs_test_args,
      sfnt_generic_check,
    },
    {
      "WCVTP",
      /* PUSHB[1] 9 32
	 WCVTP[]
	 PUSHB[0] 9
	 RCVT[]
	 PUSHB[1] 10 10
	 WCVTP[] */
      (unsigned char []) { 0xb1, 9, 32,
			   0x44,
			   0xb0, 9,
			   0x45,
			   0xb1, 10, 10,
			   0x44, },
      11,
      &wcvtp_test_args,
      sfnt_generic_check,
    },
    {
      "RCVT",
      /* PUSHB[0] 1
	 RCVT[]
	 PUSHB[0] 10
	 RCVT[] */
      (unsigned char []) { 0xb0, 1,
			   0x45,
			   0xb0, 10,
			   0x45, },
      6,
      &rcvt_test_args,
      sfnt_generic_check,
    },
    {
      "MPPEM",
      /* MPPEM[] */
      (unsigned char []) { 0x4b, },
      1,
      &mppem_test_args,
      sfnt_generic_check,
    },
    {
      "MPS",
      /* MPS[] */
      (unsigned char []) { 0x4c, },
      1,
      &mps_test_args,
      sfnt_generic_check,
    },
    {
      "FLIPON",
      /* FLIPON[] */
      (unsigned char []) { 0x4d, },
      1,
      NULL,
      sfnt_check_flipon,
    },
    {
      "FLIPOFF",
      /* FLIPOFF[] */
      (unsigned char []) { 0x4e, },
      1,
      NULL,
      sfnt_check_flipoff,
    },
    {
      "DEBUG",
      /* PUSHB[0] 1
	 DEBUG[] */
      (unsigned char []) { 0xb0, 1,
			   0x4f, },
      3,
      &debug_test_args,
      sfnt_generic_check,
    },
    {
      "LT",
      /* PUSHB[1] 47 48
	 LT[]
	 PUSHB[1] 48 47
	 LT[]
	 PUSHB[1] 47 47
	 LT[] */
      (unsigned char []) { 0xb1, 47, 48,
			   0x50,
			   0xb1, 48, 47,
			   0x50,
			   0xb1, 47, 47,
			   0x50, },
      12,
      &lt_test_args,
      sfnt_generic_check,
    },
    {
      "LTEQ",
      /* PUSHB[1] 47 48
	 LTEQ[]
	 PUSHB[1] 48 47
	 LTEQ[]
	 PUSHB[1] 47 47
	 LTEQ[] */
      (unsigned char []) { 0xb1, 47, 48,
			   0x51,
			   0xb1, 48, 47,
			   0x51,
			   0xb1, 47, 47,
			   0x51, },
      12,
      &lteq_test_args,
      sfnt_generic_check,
    },
    {
      "GT",
      /* PUSHB[1] 47 48
	 GT[]
	 PUSHB[1] 48 47
	 GT[]
	 GT[1] 47 47
	 LTEQ[] */
      (unsigned char []) { 0xb1, 47, 48,
			   0x52,
			   0xb1, 48, 47,
			   0x52,
			   0xb1, 47, 47,
			   0x52, },
      12,
      &gt_test_args,
      sfnt_generic_check,
    },
    {
      "GTEQ",
      /* PUSHB[1] 47 48
	 GTEQ[]
	 PUSHB[1] 48 47
	 GTEQ[]
	 GTEQ[1] 47 47
	 LTEQ[] */
      (unsigned char []) { 0xb1, 47, 48,
			   0x53,
			   0xb1, 48, 47,
			   0x53,
			   0xb1, 47, 47,
			   0x53, },
      12,
      &gteq_test_args,
      sfnt_generic_check,
    },
    {
      "EQ",
      /* PUSHW[1] 255 253 255 255
	 EQ[]
	 PUSHW[1] 27 27 27 27
	 EQ[]
	 PUSHB[0] 3
	 PUSHW[0] 255 254
	 EQ[] */
      (unsigned char []) { 0xb9, 255, 253, 255, 255,
			   0x54,
			   0xb9, 27, 27, 27, 27,
			   0x54,
			   0xb0, 3,
			   0xb8, 255, 254,
			   0x54, },
      18,
      &eq_test_args,
      sfnt_generic_check,
    },
    {
      "NEQ",
      /* PUSHW[1] 255 253 255 255
	 NEQ[]
	 PUSHW[1] 27 27 27 27
	 NEQ[]
	 PUSHB[0] 3
	 PUSHW[0] 255 254
	 NEQ[] */
      (unsigned char []) { 0xb9, 255, 253, 255, 255,
			   0x55,
			   0xb9, 27, 27, 27, 27,
			   0x55,
			   0xb0, 3,
			   0xb8, 255, 254,
			   0x55, },
      18,
      &neq_test_args,
      sfnt_generic_check,
    },
    {
      "ODD",
      /* RTG[]
	 PUSHW[1] 255 224 ;; -32
	 ODD[] ;; Rounds symmetrically to -64, which is odd.
	 PUSHW[1] 255 159 ;; -96
	 ODD[] ;; Rounds symmetrically to -128, which is even.  */
      (unsigned char []) { 0x18,
			   0xb8, 255, 224,
			   0x56,
			   0xb8, 255, 159,
			   0x56, },
      9,
      &odd_test_args,
      sfnt_generic_check,
    },
    {
      "EVEN",
      /* RTG[]
	 PUSHW[1] 255 224 ;; -32
	 EVEN[] ;; Rounds symmetrically to -64, which is odd.
	 PUSHW[1] 255 159 ;; -96
	 EVEN[] ;; Rounds symmetrically to -128, which is even.  */
      (unsigned char []) { 0x18,
			   0xb8, 255, 224,
			   0x57,
			   0xb8, 255, 159,
			   0x57, },
      9,
      &even_test_args,
      sfnt_generic_check,
    },
    {
      "IF",
      /* NPUSHB[] 1 0
	 IF[]
	 PUSHW[0] 1 1
	 PUSHW[1] 1 1 2 2
	 PUSHW[2] 1 1 2 2 3 3
	 PUSHW[3] 1 1 2 2 3 3 4 4
	 PUSHW[4] 1 1 2 2 3 3 4 4 5 5
	 PUSHW[5] 1 1 2 2 3 3 4 4 5 5 6 6
	 PUSHW[6] 1 1 2 2 3 3 4 4 5 5 6 6 7 7
	 PUSHW[7] 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8
	 PUSHB[0] 1
	 PUSHB[1] 2 1
	 PUSHB[2] 3 2 1
	 PUSHB[3] 4 3 2 1
	 PUSHB[4] 5 4 3 2 1
	 PUSHB[5] 6 5 4 3 2 1
	 PUSHB[6] 7 6 5 4 3 2 1
	 PUSHB[7] 8 7 6 5 4 3 2 1
	 DEBUG[]
	 IF[]
	 PUSHB[7] 12 12 12 12 12 12 12 12
	 ELSE[]
	 EIF[]
	 ELSE[]
	 PUSHB[1] 17 24
	 NPUSHB[] 5 1 2 3 4 5
	 NPUSHW[] 2 255 255 255 255
	 EIF[]

	 PUSHB[0] 1
	 IF[]
	 NPUSHB[] 2 43 43
	 IF[]
	 PUSHB[0] 45
	 ELSE[]
	 PUSHB[0] 14
	 EIF[]
	 ADD[]
	 ELSE[]
	 NPUSHB[] 4 3 2 1 0
	 EIF[]
	 PUSHB[1] 1 3 */
      (unsigned char []) { 0x40, 1, 0,
			   0x58,
			   0xb8, 1, 1,
			   0xb9, 1, 1, 2, 2,
			   0xba, 1, 1, 2, 2, 3, 3,
			   0xbb, 1, 1, 2, 2, 3, 3, 4, 4,
			   0xbc, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
			   0xbd, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
			   0xbe, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7,
			   0xbf, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
			   0xb0, 1,
			   0xb1, 2, 1,
			   0xb2, 3, 2, 1,
			   0xb3, 4, 3, 2, 1,
			   0xb4, 5, 4, 3, 2, 1,
			   0xb5, 6, 5, 4, 3, 2, 1,
			   0xb6, 7, 6, 5, 4, 3, 2, 1,
			   0xb7, 8, 7, 6, 5, 4, 3, 2, 1,
			   0x4f,
			   0x58,
			   0xb7, 12, 12, 12, 12, 12, 12, 12, 12,
			   0x1b,
			   0x59,
			   0x1b,
			   0xb1, 17, 24,
			   0x40, 5, 1, 2, 3, 4, 5,
			   0x41, 2, 255, 255, 255, 255,
			   0x59,
			   0xb0, 1,
			   0x58,
			   0x40, 2, 43, 43,
			   0x58,
			   0xb0, 45,
			   0x1b,
			   0xb0, 14,
			   0x59,
			   0x60,
			   0x1b,
			   0x40, 4, 3, 2, 1, 0,
			   0x59,
			   0xb1, 1, 3, },
      185,
      &if_test_args,
      sfnt_generic_check,
    },
    {
      "EIF",
      /* PUSHB[0] 1
	 IF[]
	 EIF[] */
      (unsigned char []) { 0xb0, 1,
			   0x58,
			   0x59, },
      3,
      &eif_test_args,
      sfnt_generic_check,
    },
    {
      "AND",
      /* PUSHB[1] 0 1
	 AND[]
	 PUSHB[1] 37 0
	 AND[]
	 PUSHB[1] 40 1
	 AND[]
	 PUSHB[1] 0 0
	 AND[] */
      (unsigned char []) { 0xb1, 0, 1,
			   0x5a,
			   0xb1, 37, 0,
			   0x5a,
			   0xb1, 40, 1,
			   0x5a,
			   0xb1, 0, 0,
			   0x5a, },
      16,
      &and_test_args,
      sfnt_generic_check,
    },
    {
      "OR",
      /* PUSHB[1] 0 1
	 OR[]
	 PUSHB[1] 37 0
	 OR[]
	 PUSHB[1] 40 1
	 OR[]
	 PUSHB[1] 0 0
	 OR[] */
      (unsigned char []) { 0xb1, 0, 1,
			   0x5b,
			   0xb1, 37, 0,
			   0x5b,
			   0xb1, 40, 1,
			   0x5b,
			   0xb1, 0, 0,
			   0x5b, },
      16,
      &or_test_args,
      sfnt_generic_check,
    },
    {
      "NOT",
      /* PUSHB[0] 1
	 NOT[]
	 PUSHB[0] 0
	 NOT[] */
      (unsigned char []) { 0xb0, 1,
			   0x5c,
			   0xb0, 0,
			   0x5c, },
      6,
      &not_test_args,
      sfnt_generic_check,
    },
    {
      "SDB",
      /* PUSHB[0] 8
	 SDB[] */
      (unsigned char []) { 0xb0, 8,
			   0x5e, },
      3,
      NULL,
      sfnt_check_sdb,
    },
    {
      "SDS",
      /* PUSHB[0] 1
	 SDS[] */
      (unsigned char []) { 0xb0, 1,
			   0x5f, },
      3,
      NULL,
      sfnt_check_sds,
    },
    {
      "that SDS rejects invalid values",
      /* PUSHB[0] 1,
	 SDS[]
	 PUSHB[0] 7
	 SDS[] */
      (unsigned char []) { 0xb0, 1,
			   0x5f,
			   0xb0, 7,
			   0x5f, },
      6,
      &sds_test_args,
      sfnt_generic_check,
    },
    {
      "ADD",
      /* PUSHB[1] 64 32
	 ADD[]
	 PUSHW[1] 255 40 0 215 ;; -216 + 215
	 ADD[] */
      (unsigned char []) { 0xb1, 64, 32,
			   0x60,
			   0xb9, 255, 40, 0, 215,
			   0x60, },
      10,
      &add_test_args,
      sfnt_generic_check,
    },
    {
      "SUB",
      /* PUSHB[1] 96 32
	 SUB[]
	 PUSHB[1] 32 96
	 SUB[]
	 PUSHW[1] 0 215 255 40 ;; 215 - -216
	 SUB[] */
      (unsigned char []) { 0xb1, 96, 32,
			   0x61,
			   0xb1, 32, 96,
			   0x61,
			   0xb9, 0, 215, 255, 40,
			   0x61, },
      14,
      &sub_test_args,
      sfnt_generic_check,
    },
    {
      "DIV",
      /* PUSHB[1] 64 128
	 DIV[] ; 1 / 2 = 0.5
	 PUSHW[1] 0 32 255 224
	 DIV[] ; 0.5 / -0.5 = -1.0
	 PUSHW[1] 255 255 0 0
	 DIV[] ; -1 / 0 = trap */
      (unsigned char []) { 0xb1, 64, 128,
			   0x62,
			   0xb9, 0, 32, 255, 224,
			   0x62,
			   0xb9, 255, 255, 0, 0,
			   0x62, },
      16,
      &div_test_args,
      sfnt_generic_check,
    },
    {
      "MUL",
      /* PUSHB[1] 255 64
	 MUL[] ; 255 * 1 = 255
	 PUSHW[1] 0 255 255 192
	 MUL[] ; 255 * -1 = -255
	 PUSHW[1] 255 1 255 192
	 MUL[] ; -255 * -1 = 255 */
      (unsigned char []) { 0xb1, 255, 64,
			   0x63,
			   0xb9, 0, 255, 255, 192,
			   0x63,
			   0xb9, 255, 1, 255, 192,
			   0x63, },
      16,
      &mul_test_args,
      sfnt_generic_check,
    },
    {
      "ABS",
      /* PUSHW[0] 255 255
	 ABS[] ;; abs (-1) == 1
	 PUSHB[0] 1
	 ABS[] ;; abs (1) == 1 */
      (unsigned char []) { 0xb8, 255, 255,
			   0x64,
			   0xb0, 1,
			   0x64, },
      7,
      &abs_test_args,
      sfnt_generic_check,
    },
    {
      "NEG",
      /* PUSHW[0] 255 255
	 NEG[] ;; neg (-1) == 1
	 PUSHB[0] 1
	 NEG[] ;; neg (1) == -1 */
      (unsigned char []) { 0xb8, 255, 255,
			   0x65,
			   0xb0, 1,
			   0x65, },
      7,
      &neg_test_args,
      sfnt_generic_check,
    },
    {
      "FLOOR",
      /* PUSHW[0] 255 129 ; -127
	 FLOOR[] ; floor (-127) == -128
	 PUSHW[0] 255 193 ; -63
	 FLOOR[] ; floor (-63) == -64
	 PUSHB[0] 63
	 FLOOR[] ; floor (63) == 0
	 PUSHB[0] 127
	 FLOOR[] ; floor (127) == 64
	 PUSHB[0] 191
	 FLOOR[] ; floor (191) == 128 */
      (unsigned char []) { 0xb8, 255, 129,
			   0x66,
			   0xb8, 255, 193,
			   0x66,
			   0xb0, 63,
			   0x66,
			   0xb0, 127,
			   0x66,
			   0xb0, 191,
			   0x66, },
      17,
      &floor_test_args,
      sfnt_generic_check,
    },
    {
      "CEILING",
      /* PUSHW[0] 255 128 ; -128
	 CEILING[] ; ceiling (-128) == -128
	 PUSHW[0] 255 127 ; -129
	 CEILING[] ; ceiling (-129) == -128
	 PUSHW[0] 255 191 ; -65
	 CEILING[] ; ceiling (-65) == -64
	 PUSHW[0] 255 255 ; -1
	 CEILING[] ; ceiling (-1) == 0
	 PUSHB[0] 63
	 CEILING[] ; ceiling (63) == 64
	 PUSHB[0] 65
	 CEILING[] ; ceiling (65) == 128
	 PUSHB[0] 128
	 CEILING[] ; ceiling (128) == 128 */
      (unsigned char []) { 0xb8, 255, 128,
			   0x67,
			   0xb8, 255, 127,
			   0x67,
			   0xb8, 255, 191,
			   0x67,
			   0xb8, 255, 255,
			   0x67,
			   0xb0, 63,
			   0x67,
			   0xb0, 65,
			   0x67,
			   0xb0, 128,
			   0x67, },
      25,
      &ceiling_test_args,
      sfnt_generic_check,
    },
    {
      "ROUND",
      /* ROUND[] */
      (unsigned char []) { 0x68, },
      1,
      &round_test_args,
      sfnt_generic_check,
    },
    {
      "NROUND",
      /* PUSHB[0] 63
	 NROUND[] */
      (unsigned char []) { 0xb0, 63,
			   0x6c, },
      3,
      &nround_test_args,
      sfnt_generic_check,
    },
    {
      "WCVTF",
      /* PUSHB[1] 1 63
         WCVTF[]
         PUSHB[0] 1
         RCVT[] */
      (unsigned char []) { 0xb1, 1, 63,
			   0x70,
			   0xb0, 1,
			   0x45, },
      7,
      &wcvtf_test_args,
      sfnt_generic_check,
    },
    {
      "DELTAC1",
      /* PUSHB[0] 2
	 SDB[] ; delta base now 2
	 PUSHB[0] 6
	 SDS[] ; delta shift now 6
	 PUSHB[2] 1 0xff 1 ; CVT index 5, ppem 15 + 2, magnitude 15
         DELTAC1[]
	 PUSHB[0] 1
         RCVT[] ; CVT index 5 should now be greater by 8 / 64

         PUSHB[2] 1 0xef 1 ; CVT index 5, ppem 14 + 2, magnitude 15
         DELTAC1[]
         PUSHB[0] 1
         RCVT[] ; CVT index 5 should be unchanged */
      (unsigned char []) { 0xb0, 2,
			   0x5e,
			   0xb0, 6,
			   0x5f,
			   0xb2, 5, 255, 1,
			   0x73,
			   0xb0, 5,
			   0x45,
			   0xb2, 5, 239, 1,
			   0x73,
			   0xb0, 5,
			   0x45, },
      22,
      &deltac1_test_args,
      sfnt_generic_check,
    },
    {
      "DELTAC2",
      /* PUSHB[0] 2
	 SDB[] ; delta base now 2
	 PUSHB[0] 6
	 SDS[] ; delta shift now 6
	 PUSHB[2] 1 0xff 1 ; CVT index 5, ppem 15 + 2 + 16, magnitude 15
         DELTAC2[]
	 PUSHB[0] 1
         RCVT[] ; CVT index 5 should be unchanged

         PUSHB[2] 1 0xef 1 ; CVT index 5, ppem 14 + 2 + 16, magnitude 15
         DELTAC2[]
         PUSHB[0] 1
         RCVT[] ; CVT index 5 should be unchanged */
      (unsigned char []) { 0xb0, 2,
			   0x5e,
			   0xb0, 6,
			   0x5f,
			   0xb2, 5, 255, 1,
			   0x74,
			   0xb0, 5,
			   0x45,
			   0xb2, 5, 239, 1,
			   0x74,
			   0xb0, 5,
			   0x45, },
      22,
      &deltac2_test_args,
      sfnt_generic_check,
    },
    {
      "DELTAC3",
      /* PUSHB[0] 2
	 SDB[] ; delta base now 2
	 PUSHB[0] 6
	 SDS[] ; delta shift now 6
	 PUSHB[2] 1 0xff 1 ; CVT index 5, ppem 15 + 2 + 32, magnitude 15
         DELTAC3[]
	 PUSHB[0] 1
         RCVT[] ; CVT index 5 should be unchanged

         PUSHB[2] 1 0xef 1 ; CVT index 5, ppem 14 + 2 + 32, magnitude 15
         DELTAC3[]
         PUSHB[0] 1
         RCVT[] ; CVT index 5 should be unchanged */
      (unsigned char []) { 0xb0, 2,
			   0x5e,
			   0xb0, 6,
			   0x5f,
			   0xb2, 5, 255, 1,
			   0x75,
			   0xb0, 5,
			   0x45,
			   0xb2, 5, 239, 1,
			   0x75,
			   0xb0, 5,
			   0x45, },
      22,
      &deltac3_test_args,
      sfnt_generic_check,
    },
    {
      "SROUND",
      sfnt_sround_instructions,
      ARRAYELTS (sfnt_sround_instructions),
      &sround_test_args,
      sfnt_generic_check,
    },
    {
      "S45ROUND",
      sfnt_s45round_instructions,
      ARRAYELTS (sfnt_s45round_instructions),
      &s45round_test_args,
      sfnt_generic_check,
    },
    {
      "RUTG",
      /* RUTG[]
	 PUSHB[0] 1
	 ROUND[]
	 PUSHB[0] 64
	 ROUND[]
	 PUSHB[0] 0
	 ROUND[] */
      (unsigned char []) { 0x7c,
			   0xb0, 1,
			   0x68,
			   0xb0, 64,
			   0x68,
			   0xb0, 0,
			   0x68, },
      10,
      &rutg_test_args,
      sfnt_generic_check,
    },
    {
      "RDTG",
      /* RUTG[]
	 PUSHB[0] 1
	 ROUND[]
	 PUSHB[0] 63
	 ROUND[]
	 PUSHB[0] 64
	 ROUND[] */
      (unsigned char []) { 0x7d,
			   0xb0, 1,
			   0x68,
			   0xb0, 63,
			   0x68,
			   0xb0, 64,
			   0x68, },
      10,
      &rdtg_test_args,
      sfnt_generic_check,
    },
    {
      "SANGW",
      /* PUSHB[0] 3
	 SANGW[] */
      (unsigned char []) { 0xb0, 3,
			   0x7e, },
      3,
      &sangw_test_args,
      sfnt_generic_check,
    },
    {
      "AA",
      /* PUSHB[0] 3
	 AA[] */
      (unsigned char []) { 0xb0, 3,
			   0x7f, },
      3,
      &aa_test_args,
      sfnt_generic_check,
    },
    {
      "SCANCTRL",
      /* PUSHB[0] 1
	 SCANCTRL[] */
      (unsigned char []) { 0xb0, 1,
			   0x85, },
      3,
      NULL,
      sfnt_check_scanctrl,
    },
    {
      "GETINFO",
      /* PUSHB[0] 1
         GETINFO[]
         PUSHB[0] 6
         GETINFO[] */
      (unsigned char []) { 0xb0, 1,
			   0x88,
			   0xb0, 6,
			   0x88, },
      6,
      &getinfo_test_args,
      sfnt_generic_check,
    },
    {
      "IDEF",
      /* PUSHB[0] 247
	 IDEF[]
	 PUSHB[3] 1 2 3 4
	 POP[]
	 ENDF[]
	 247 */
      (unsigned char []) { 0xb0, 247,
			   0x89,
			   0xb3, 1, 2, 3, 4,
			   0x21,
			   0x2d,
			   247, },
      11,
      &idef_test_args,
      sfnt_generic_check,
    },
    {
      "ROLL",
      /* PUSHB[4] 1 2 3 4 5
         ROLL[] ; this should become 1 2 4 5 3 */
      (unsigned char []) { 0xb4, 1, 2, 3, 4, 5,
			   0x8a, },
      7,
      &roll_test_args,
      sfnt_generic_check,
    },
    {
      "that ROLL correctly handles underflow",
      /* PUSHB[1] 1 2
         ROLL[] */
      (unsigned char []) { 0xb1, 1, 2,
			   0x8a, },
      4,
      &roll_1_test_args,
      sfnt_generic_check,
    },
    {
      "MAX",
      /* PUSHW[1] 0 70 255 186 ; 70, -70
         MAX[] */
      (unsigned char []) { 0xb9, 0, 70, 255, 186,
			   0x8b, },
      6,
      &max_test_args,
      sfnt_generic_check,
    },
    {
      "MIN",
      /* PUSHW[1] 0 70 255 186 ; 70, -70
         MIN[] */
      (unsigned char []) { 0xb9, 0, 70, 255, 186,
			   0x8c, },
      6,
      &min_test_args,
      sfnt_generic_check,
    },
    {
      "SCANTYPE",
      /* PUSHB[0] 0
         SCANTYPE[] */
      (unsigned char []) { 0xb0, 0,
			   0x8d, },
      3,
      &scantype_test_args,
      sfnt_generic_check,
    },
    {
      "INSTCTRL",
      /* PUSHB[1] 1 1
	 INSTCTRL[] ; (1 << 1) should now be set
         PUSHB[1] 2 1
	 INSTCTRL[] ; (1 << 2) should now be set
         PUSHB[1] 2 0
	 INSTCTRL[] ; (1 << 2) should no longer be set */
      (unsigned char []) { 0xb1, 1, 1,
			   0x8e,
			   0xb1, 2, 1,
			   0x8e,
			   0xb1, 2, 0,
			   0x8e, },
      12,
      NULL,
      sfnt_check_instctrl,
    },
  };



/* Main entry point.  */

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
  struct sfnt_ttc_header *ttc;
  struct sfnt_interpreter *interpreter;
  struct sfnt_cvt_table *cvt;
  struct sfnt_fpgm_table *fpgm;
  const char *trap;
  struct sfnt_prep_table *prep;
  struct sfnt_graphics_state state;

  if (argc != 2)
    return 1;

  if (!strcmp (argv[1], "--check-interpreter"))
    {
      interpreter = sfnt_make_test_interpreter ();

      for (i = 0; i < ARRAYELTS (all_tests); ++i)
	sfnt_run_interpreter_test (&all_tests[i], interpreter);

      exit (0);
    }

  fd = open (argv[1], O_RDONLY);

  if (fd < 1)
    return 1;

  ttc = NULL;

  font = sfnt_read_table_directory (fd);

  if (font == (struct sfnt_offset_subtable *) -1)
    {
      if (lseek (fd, 0, SEEK_SET) != 0)
	return 1;

      ttc = sfnt_read_ttc_header (fd);

      if (!ttc)
	return 1;

      fprintf (stderr, "TrueType collection: %"PRIu32" fonts installed\n",
	       ttc->num_fonts);
      fflush (stderr);

      printf ("Which font? ");
      if (scanf ("%d", &i) == EOF)
	return 1;

      if (i >= ttc->num_fonts || i < 0)
	{
	  printf ("out of range\n");
	  return 1;
	}

      if (lseek (fd, ttc->offset_table[i], SEEK_SET)
	  != ttc->offset_table[i])
	return 1;

      font = sfnt_read_table_directory (fd);
    }

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
	       subtables[i].offset, data[i]);

      if (data[i])
	fprintf (stderr, "  format: %"PRIu16"\n",
		 data[i]->format);
    }

  interpreter = NULL;
  head = sfnt_read_head_table (fd, font);
  hhea = sfnt_read_hhea_table (fd, font);
  glyf = sfnt_read_glyf_table (fd, font);
  maxp = sfnt_read_maxp_table (fd, font);
  name = sfnt_read_name_table (fd, font);
  meta = sfnt_read_meta_table (fd, font);
  cvt  = sfnt_read_cvt_table (fd, font);
  fpgm = sfnt_read_fpgm_table (fd, font);
  prep = sfnt_read_prep_table (fd, font);
  hmtx = NULL;

  if (head && maxp && maxp->version >= 0x00010000)
    {
      fprintf (stderr, "creating interpreter\n"
	       "the size of the stack is %"PRIu16"\n"
	       "the size of the twilight zone is %"PRIu16"\n"
	       "the size of the storage area is %"PRIu16"\n"
	       "there are at most %"PRIu16" idefs\n"
	       "there are at most %"PRIu16" fdefs\n"
	       "the cvt is %zu fwords in length\n",
	       maxp->max_stack_elements,
	       maxp->max_twilight_points,
	       maxp->max_storage,
	       maxp->max_instruction_defs,
	       maxp->max_function_defs,
	       cvt ? cvt->num_elements : 0ul);

      interpreter = sfnt_make_interpreter (maxp, cvt, head,
					   17, 17);

      if (fpgm)
	{
	  fprintf (stderr, "interpreting the font program, with"
		   " %zu instructions\n", fpgm->num_instructions);
	  trap = sfnt_interpret_font_program (interpreter, fpgm);

	  if (trap)
	    fprintf (stderr, "**TRAP**: %s\n", trap);
	}

      if (prep)
	{
	  fprintf (stderr, "interpreting the control value program, with"
		   " %zu instructions\n", prep->num_instructions);
	  trap = sfnt_interpret_control_value_program (interpreter, prep,
						       &state);

	  if (trap)
	    fprintf (stderr, "**TRAP**: %s\n", trap);
	}
    }

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
    {
      fprintf (stderr, "meta table with count: %"PRIu32"\n",
	       meta->num_data_maps);

      for (i = 0; i < meta->num_data_maps; ++i)
	fprintf (stderr, "  meta tag: %"PRIx32"\n",
		 meta->data_maps[i].tag);
    }

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

      if (i < 0 || i >= table->num_subtables)
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
						  50,
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
		  if (!sfnt_lookup_glyph_metrics (code, 50,
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
  xfree (ttc);
  xfree (cvt);
  xfree (fpgm);
  xfree (interpreter);
  xfree (prep);

  return 0;
}

#endif
