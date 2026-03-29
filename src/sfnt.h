/* sfnt format font support for GNU Emacs.

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

#ifndef _SFNT_H_
#define _SFNT_H_

#include <stdint.h>
#include <stddef.h>
#include <setjmp.h>

#include <sys/types.h>



/* Container structure and enumerator definitions.  */

/* The sfnt container format is organized into different tables, such
   as ``cmap'' or ``glyf''.  Each of these tables has a specific
   format and use.  These are all the tables known to Emacs.  */

enum sfnt_table
  {
    SFNT_TABLE_CMAP,
    SFNT_TABLE_GLYF,
    SFNT_TABLE_HEAD,
    SFNT_TABLE_HHEA,
    SFNT_TABLE_HMTX,
    SFNT_TABLE_LOCA,
    SFNT_TABLE_MAXP,
    SFNT_TABLE_NAME,
    SFNT_TABLE_META,
    SFNT_TABLE_CVT ,
    SFNT_TABLE_FPGM,
    SFNT_TABLE_PREP,
    SFNT_TABLE_FVAR,
    SFNT_TABLE_GVAR,
    SFNT_TABLE_CVAR,
    SFNT_TABLE_AVAR,
    SFNT_TABLE_OS_2,
    SFNT_TABLE_POST,
  };

#define SFNT_ENDOF(type, field, type1)			\
  ((size_t) offsetof (type, field) + sizeof (type1))

/* Each of these structures must be aligned so that no compiler will
   ever generate padding bytes on platforms where the alignment
   requirements for uint32_t and uint16_t are no larger than 4 and 2
   bytes respectively.

   Pointer types are assumed to impose an alignmnent requirement no
   less than that of uint32_t.

   If a table has more than one kind of variable-length subtable array
   at the end, make sure to pad subsequent subtables
   appropriately.  */

struct sfnt_offset_subtable
{
  /* The scaler type.  */
  uint32_t scaler_type;

  /* The number of tables.  */
  uint16_t num_tables;

  /* (Maximum power of 2 <= numTables) * 16.  */
  uint16_t search_range;

  /* log2 (maximum power of 2 <= numTables) */
  uint16_t entry_selector;

  /* numTables * 16 - searchRange.  */
  uint16_t range_shift;

  /* Variable length data.  */
  struct sfnt_table_directory *subtables;
};

/* The table directory.  Follows the offset subtable, with one for
   each table.  */

struct sfnt_table_directory
{
  /* 4-byte identifier for each table.  See sfnt_table_names.  */
  uint32_t tag;

  /* Table checksum.  */
  uint32_t checksum;

  /* Offset from the start of the file.  */
  uint32_t offset;

  /* Length of the table in bytes, not subject to padding.  */
  uint32_t length;
};

enum sfnt_scaler_type
  {
    SFNT_SCALER_TRUE = 0x74727565,
    SFNT_SCALER_VER1 = 0x00010000,
    SFNT_SCALER_TYP1 = 0x74797031,
    SFNT_SCALER_OTTO = 0x4F54544F,
  };

typedef int32_t sfnt_fixed;
typedef int16_t sfnt_fword;
typedef uint16_t sfnt_ufword;

#define sfnt_coerce_fixed(fixed) ((sfnt_fixed) (fixed) / 65535.0)
#define sfnt_fixed_float(fixed)  ((sfnt_fixed) (fixed) / 65535.0f)

typedef unsigned int sfnt_glyph;
typedef unsigned int sfnt_char;

struct sfnt_head_table
{
  /* The version.  This is a 16.16 fixed point number.  */
  sfnt_fixed version;

  /* The revision.  */
  sfnt_fixed revision;

  /* Checksum adjustment.  */
  uint32_t checksum_adjustment;

  /* Magic number, should be 0x5F0F3CF5.  */
  uint32_t magic;

  /* Flags for the font.  */
  uint16_t flags;

  /* Units per em.  */
  uint16_t units_per_em;

  /* Time of creation.  */
  uint32_t created_high, created_low;

  /* Time of modification.  */
  uint32_t modified_high, modified_low;

  /* Minimum bounds.  */
  sfnt_fword xmin, ymin, xmax, ymax;

  /* Mac specific stuff.  */
  uint16_t mac_style;

  /* Smallest readable size in pixels.  */
  uint16_t lowest_rec_ppem;

  /* Font direction hint.  */
  int16_t font_direction_hint;

  /* Index to loc format.  0 for short offsets, 1 for long.  */
  int16_t index_to_loc_format;

  /* Unused.  */
  int16_t glyph_data_format;
};

struct sfnt_hhea_table
{
  /* The version.  This is a 16.16 fixed point number.  */
  sfnt_fixed version;

  /* The maximum ascent and descent values for this font.  */
  sfnt_fword ascent, descent;

  /* The typographic line gap.  */
  sfnt_fword line_gap;

  /* The maximum advance width.  */
  sfnt_ufword advance_width_max;

  /* The minimum bearings on either side.  */
  sfnt_fword min_left_side_bearing, min_right_side_bearing;

  /* The maximum extent.  */
  sfnt_fword x_max_extent;

  /* Caret slope.  */
  int16_t caret_slope_rise, caret_slope_run;

  /* Caret offset for non slanted fonts.  */
  sfnt_fword caret_offset;

  /* Reserved values.  */
  int16_t reserved1, reserved2, reserved3, reserved4;

  /* Should always be zero.  */
  int16_t metric_data_format;

  /* Number of advanced widths in metrics table.  */
  uint16_t num_of_long_hor_metrics;
};

struct sfnt_cmap_table
{
  /* Should be zero.  */
  uint16_t version;

  /* Number of subtables.  */
  uint16_t num_subtables;
};

enum sfnt_platform_id
  {
    SFNT_PLATFORM_UNICODE   = 0,
    SFNT_PLATFORM_MACINTOSH = 1,
    SFNT_PLATFORM_RESERVED  = 2,
    SFNT_PLATFORM_MICROSOFT = 3,
  };

enum sfnt_unicode_platform_specific_id
  {
    SFNT_UNICODE_1_0		     = 0,
    SFNT_UNICODE_1_1		     = 1,
    SFNT_UNICODE_ISO_10646_1993	     = 2,
    SFNT_UNICODE_2_0_BMP	     = 3,
    SFNT_UNICODE_2_0		     = 4,
    SFNT_UNICODE_VARIATION_SEQUENCES = 5,
    SFNT_UNICODE_LAST_RESORT	     = 6,
  };

enum sfnt_macintosh_platform_specific_id
  {
    SFNT_MACINTOSH_ROMAN	       = 0,
    SFNT_MACINTOSH_JAPANESE	       = 1,
    SFNT_MACINTOSH_TRADITIONAL_CHINESE = 2,
    SFNT_MACINTOSH_KOREAN	       = 3,
    SFNT_MACINTOSH_ARABIC	       = 4,
    SFNT_MACINTOSH_HEBREW	       = 5,
    SFNT_MACINTOSH_GREEK	       = 6,
    SFNT_MACINTOSH_RUSSIAN	       = 7,
    SFNT_MACINTOSH_RSYMBOL	       = 8,
    SFNT_MACINTOSH_DEVANAGARI	       = 9,
    SFNT_MACINTOSH_GURMUKHI	       = 10,
    SFNT_MACINTOSH_GUJARATI	       = 11,
    SFNT_MACINTOSH_ORIYA	       = 12,
    SFNT_MACINTOSH_BENGALI	       = 13,
    SFNT_MACINTOSH_TAMIL	       = 14,
    SFNT_MACINTOSH_TELUGU	       = 15,
    SFNT_MACINTOSH_KANNADA	       = 16,
    SFNT_MACINTOSH_MALAYALAM	       = 17,
    SFNT_MACINTOSH_SINHALESE	       = 18,
    SFNT_MACINTOSH_BURMESE	       = 19,
    SFNT_MACINTOSH_KHMER	       = 20,
    SFNT_MACINTOSH_THAI		       = 21,
    SFNT_MACINTOSH_LAOTIAN	       = 22,
    SFNT_MACINTOSH_GEORGIAN	       = 23,
    SFNT_MACINTOSH_ARMENIAN	       = 24,
    SFNT_MACINTOSH_SIMPLIFIED_CHINESE  = 25,
    SFNT_MACINTOSH_TIBETIAN	       = 26,
    SFNT_MACINTOSH_MONGOLIAN	       = 27,
    SFNT_MACINTOSH_GEEZ		       = 28,
    SFNT_MACINTOSH_SLAVIC	       = 29,
    SFNT_MACINTOSH_VIETNAMESE	       = 30,
    SFNT_MACINTOSH_SINDHI	       = 31,
    SFNT_MACINTOSH_UNINTERPRETED       = 32,
  };

enum sfnt_microsoft_platform_specific_id
  {
    SFNT_MICROSOFT_SYMBOL	 = 0,
    SFNT_MICROSOFT_UNICODE_BMP	 = 1,
    SFNT_MICROSOFT_SHIFT_JIS	 = 2,
    SFNT_MICROSOFT_PRC		 = 3,
    SFNT_MICROSOFT_BIG_FIVE	 = 4,
    SFNT_MICROSOFT_WANSUNG	 = 5,
    SFNT_MICROSOFT_JOHAB	 = 6,
    SFNT_MICROSOFT_UNICODE_UCS_4 = 10,
  };

struct sfnt_cmap_encoding_subtable
{
  /* The platform ID.  */
  uint16_t platform_id;

  /* Platform specific ID.  */
  uint16_t platform_specific_id;

  /* Mapping table offset.  */
  uint32_t offset;
};

struct sfnt_cmap_encoding_subtable_data
{
  /* Format and possibly the length in bytes.  */
  uint16_t format, length;
};

struct sfnt_cmap_format_0
{
  /* Format, set to 0.  */
  uint16_t format;

  /* Length in bytes.  Should be 262.  */
  uint16_t length;

  /* Language code.  */
  uint16_t language;

  /* Character code to glyph index map.  */
  uint8_t glyph_index_array[256];
};

struct sfnt_cmap_format_2_subheader
{
  uint16_t first_code;
  uint16_t entry_count;
  int16_t id_delta;
  uint16_t id_range_offset;
};

struct sfnt_cmap_format_2
{
  /* Format, set to 2.  */
  uint16_t format;

  /* Length in bytes.  */
  uint16_t length;

  /* Language code.  */
  uint16_t language;

  /* Array mapping high bytes to subheaders.  */
  uint16_t sub_header_keys[256];

  /* Variable length data.  */
  struct sfnt_cmap_format_2_subheader *subheaders;
  uint16_t *glyph_index_array;
  uint16_t num_glyphs;
};

struct sfnt_cmap_format_4
{
  /* Format, set to 4.  */
  uint16_t format;

  /* Length in bytes.  */
  uint16_t length;

  /* Language code.  */
  uint16_t language;

  /* 2 * seg_count.  */
  uint16_t seg_count_x2;

  /* 2 * (2**FLOOR(log2(segCount))) */
  uint16_t search_range;

  /* log2(searchRange/2) */
  uint16_t entry_selector;

  /* (2 * segCount) - searchRange */
  uint16_t range_shift;

  /* Variable-length data.  */
  uint16_t *end_code;
  uint16_t *reserved_pad;
  uint16_t *start_code;
  int16_t *id_delta;
  int16_t *id_range_offset;
  uint16_t *glyph_index_array;

  /* The number of elements in glyph_index_array.  */
  size_t glyph_index_size;
};

struct sfnt_cmap_format_6
{
  /* Format, set to 6.  */
  uint16_t format;

  /* Length in bytes.  */
  uint16_t length;

  /* Language code.  */
  uint16_t language;

  /* First character code in subrange.  */
  uint16_t first_code;

  /* Number of character codes.  */
  uint16_t entry_count;

  /* Variable-length data.  */
  uint16_t *glyph_index_array;
};

struct sfnt_cmap_format_8_or_12_group
{
  uint32_t start_char_code;
  uint32_t end_char_code;
  uint32_t start_glyph_code;
};

struct sfnt_cmap_format_8
{
  /* Format, set to 8.  */
  uint16_t format;

  /* Reserved.  */
  uint16_t reserved;

  /* Length in bytes.  */
  uint32_t length;

  /* Language code.  */
  uint32_t language;

  /* Tightly packed array of bits (8K bytes total) indicating whether
     the particular 16-bit (index) value is the start of a 32-bit
     character code.  */
  uint8_t is32[65536];

  /* Number of groups.  */
  uint32_t num_groups;

  /* Variable length data.  */
  struct sfnt_cmap_format_8_or_12_group *groups;
};

/* cmap formats 10, 13 unsupported.  */

struct sfnt_cmap_format_12
{
  /* Format, set to 12.  */
  uint16_t format;

  /* Reserved.  */
  uint16_t reserved;

  /* Length in bytes.  */
  uint32_t length;

  /* Language code.  */
  uint32_t language;

  /* Number of groups.  */
  uint32_t num_groups;

  /* Variable length data.  */
  struct sfnt_cmap_format_8_or_12_group *groups;
};

struct sfnt_cmap_format_14
{
  /* Format, set to 14.  */
  uint16_t format;

  /* The length of the table in bytes.  */
  uint32_t length;

  /* Number of variation selector records.  */
  uint16_t num_var_selector_records;

  /* The offset of this table in the font file.  */
  off_t offset;

  /* Variable length data.  */
  struct sfnt_variation_selector_record *records;
};

struct sfnt_variation_selector_record
{
  /* 24-bit unsigned variation selector.  */
  unsigned int var_selector;

  /* Offset to default UVS table.  */
  uint32_t default_uvs_offset;

  /* Offset to non-default UVS table.  */
  uint32_t nondefault_uvs_offset;
};

struct sfnt_maxp_table
{
  /* Table version.  */
  sfnt_fixed version;

  /* The number of glyphs in this font - 1.  Set at version 0.5 or
     later.  */
  uint16_t num_glyphs;

  /* These fields are only set in version 1.0 or later.  Maximum
     points in a non-composite glyph.  */
  uint16_t max_points;

  /* Maximum contours in a non-composite glyph.  */
  uint16_t max_contours;

  /* Maximum points in a composite glyph.  */
  uint16_t max_composite_points;

  /* Maximum contours in a composite glyph.  */
  uint16_t max_composite_contours;

  /* 1 if instructions do not use the twilight zone (Z0), or 2 if
     instructions do use Z0; should be set to 2 in most cases.  */
  uint16_t max_zones;

  /* Maximum points used in Z0.  */
  uint16_t max_twilight_points;

  /* Number of Storage Area locations.  */
  uint16_t max_storage;

  /* Number of FDEFs, equal to the highest function number + 1.  */
  uint16_t max_function_defs;

  /* Number of IDEFs.  */
  uint16_t max_instruction_defs;

  /* Maximum stack depth across Font Program ('fpgm' table), CVT
     Program ('prep' table) and all glyph instructions (in the 'glyf'
     table).  */
  uint16_t max_stack_elements;

  /* Maximum byte count for glyph instructions.  */
  uint16_t max_size_of_instructions;

  /* Maximum number of components referenced at ``top level'' for any
     composite glyph.  */
  uint16_t max_component_elements;

  /* Maximum levels of recursion; 1 for simple components.  */
  uint16_t max_component_depth;
};

struct sfnt_loca_table_short
{
  /* Offsets to glyph data divided by two.  */
  uint16_t *offsets;

  /* Size of the offsets list.  */
  size_t num_offsets;
};

struct sfnt_loca_table_long
{
  /* Offsets to glyph data.  */
  uint32_t *offsets;

  /* Size of the offsets list.  */
  size_t num_offsets;
};

struct sfnt_glyf_table
{
  /* Size of the glyph data.  */
  size_t size;

  /* Pointer to possibly unaligned glyph data.  */
  unsigned char *glyphs;

  /* Pointer to the start of the mapping.
     Only initialized if this table was mmapped.  */
  unsigned char *start;
};

struct sfnt_simple_glyph
{
  /* The total number of points in this glyph.  */
  size_t number_of_points;

  /* Array containing the last points of each contour.  */
  uint16_t *restrict end_pts_of_contours;

  /* Total number of bytes needed for instructions.  */
  uint16_t instruction_length;

  /* Instruction data.  */
  uint8_t *restrict instructions;

  /* Array of flags.  */
  uint8_t *restrict flags;

  /* Array of X coordinates.  */
  int16_t *restrict x_coordinates;

  /* Array of Y coordinates.  */
  int16_t *restrict y_coordinates;

  /* Pointer to the end of that array.  */
  int16_t *restrict y_coordinates_end;
};

struct sfnt_compound_glyph_component
{
  /* Compound glyph flags.  */
  uint16_t flags;

  /* Component glyph index.  */
  uint16_t glyph_index;

  /* X-offset for component or point number; type depends on bits 0
     and 1 in component flags.  */
  union {
    uint8_t a;
    int8_t b;
    uint16_t c;
    int16_t d;
  } argument1;

  /* Y-offset for component or point number; type depends on bits 0
     and 1 in component flags.  */
  union {
    uint8_t a;
    int8_t b;
    uint16_t c;
    int16_t d;
  } argument2;

  /* Various scale formats.  */
  union {
    int16_t scale;
    struct {
      int16_t xscale;
      int16_t yscale;
    } a;
    struct {
      int16_t xscale;
      int16_t scale01;
      int16_t scale10;
      int16_t yscale;
    } b;
  } u;
};

struct sfnt_compound_glyph
{
  /* Pointer to array of components.  */
  struct sfnt_compound_glyph_component *components;

  /* Number of elements in that array.  */
  size_t num_components;

  /* Instruction data.  */
  uint8_t *instructions;

  /* Length of instructions.  */
  uint16_t instruction_length;
};

struct sfnt_glyph
{
  /* Number of contours in this glyph.  */
  int16_t number_of_contours;

  /* Coordinate bounds.  */
  sfnt_fword xmin, ymin, xmax, ymax;

  /* Distortion applied to the right side phantom point.  */
  sfnt_fword advance_distortion;

  /* Distortion applied to the origin point.  */
  sfnt_fword origin_distortion;

  /* Either a simple glyph or a compound glyph, depending on which is
     set.  */
  struct sfnt_simple_glyph *simple;
  struct sfnt_compound_glyph *compound;
};



/* Glyph outline decomposition.  */

struct sfnt_point
{
  /* X and Y in em space.  */
  sfnt_fixed x, y;
};

typedef void (*sfnt_move_to_proc) (struct sfnt_point, void *);
typedef void (*sfnt_line_to_proc) (struct sfnt_point, void *);
typedef void (*sfnt_curve_to_proc) (struct sfnt_point,
				    struct sfnt_point,
				    void *);

/* Forward declaration for use in sfnt_get_metrics_proc.  */
struct sfnt_glyph_metrics;

typedef struct sfnt_glyph *(*sfnt_get_glyph_proc) (sfnt_glyph, void *,
						   bool *);
typedef void (*sfnt_free_glyph_proc) (struct sfnt_glyph *, void *);
typedef int (*sfnt_get_metrics_proc) (sfnt_glyph,
				      struct sfnt_glyph_metrics *,
				      void *);



/* Decomposed glyph outline.  */

struct sfnt_glyph_outline_command
{
  /* Flags for this outline command.  */
  int flags;

  /* X and Y position of this command.  */
  sfnt_fixed x, y;
};

/* Structure describing a single recorded outline in fixed pixel
   space.  */

struct sfnt_glyph_outline
{
  /* Array of outlines elements.  */
  struct sfnt_glyph_outline_command *outline;

  /* Size of the outline data, and how much is full.  */
  size_t outline_size, outline_used;

  /* Rectangle defining bounds of the outline.  Namely, the minimum
     and maximum X and Y positions.  */
  sfnt_fixed xmin, ymin, xmax, ymax;

  /* The origin point of the outline on the X axis.  Value defaults to
     0.  */
  sfnt_fixed origin;

  /* Reference count.  Initially zero.  */
  short refcount;
};

enum sfnt_glyph_outline_flags
  {
    SFNT_GLYPH_OUTLINE_LINETO	  = (1 << 1),
  };



/* Glyph rasterization.  */

struct sfnt_raster
{
  /* Pointer to coverage data.  */
  unsigned char *cells;

  /* Basic dimensions of the raster.  */
  unsigned short width, height;

  /* Integer offset to apply to positions in the raster so that they
     start from the origin point of the glyph.  */
  short offx, offy;

  /* The raster stride.  */
  unsigned short stride;

  /* Reference count.  Initially zero.  */
  unsigned short refcount;
};

struct sfnt_edge
{
  /* Next edge in this chain.  */
  struct sfnt_edge *next;

  /* Winding direction.  1 if clockwise, -1 if counterclockwise.  */
  int winding;

  /* X position, top and bottom of edges.  */
  sfnt_fixed x, top, bottom;

  /* Amount to move X by upon each change of Y, and vice versa.  */
  sfnt_fixed step_x;
};



/* Polygon rasterization constants.  */

enum
  {
    SFNT_POLY_SHIFT  = 3,
    SFNT_POLY_SAMPLE = (1 << SFNT_POLY_SHIFT),
    SFNT_POLY_MASK   = (SFNT_POLY_SAMPLE - 1),
    SFNT_POLY_STEP   = (0x10000 >> SFNT_POLY_SHIFT),
    SFNT_POLY_START  = (SFNT_POLY_STEP >> 1),
  };



/* Glyph metrics computation.  */

struct sfnt_long_hor_metric
{
  uint16_t advance_width;
  int16_t left_side_bearing;
};

struct sfnt_hmtx_table
{
  /* Array of horizontal metrics for each glyph.  */
  struct sfnt_long_hor_metric *h_metrics;

  /* Lbearing for remaining glyphs.  */
  int16_t *left_side_bearing;
};

/* Structure describing the metrics of a single glyph.  The fields
   mean the same as in XCharStruct, except they are 16.16 fixed point
   values, and are missing significant information.  */

struct sfnt_glyph_metrics
{
  /* Distance between origin and left edge of raster.  Positive
     changes move rightwards.

     If sfnt_lookup_glyph_metrics is given a pixel size of -1,
     this is actually a sign extended fword.  */
  sfnt_fixed lbearing;

  /* Advance to next glyph's origin.

     If sfnt_lookup_glyph_metrics is given a pixel size of -1, this is
     actually a sign extended fword.  */
  sfnt_fixed advance;
};



/* Font style parsing.  */

struct sfnt_name_record
{
  /* Platform identifier code.  */
  uint16_t platform_id;

  /* Platform specific ID.  */
  uint16_t platform_specific_id;

  /* Language identifier.  */
  uint16_t language_id;

  /* Name identifier.  */
  uint16_t name_id;

  /* String length in bytes.  */
  uint16_t length;

  /* Offset from start of storage area.  */
  uint16_t offset;
};

struct sfnt_name_table
{
  /* Format selector of name table.  */
  uint16_t format;

  /* Number of name records.  */
  uint16_t count;

  /* Offset to start of string data.  */
  uint16_t string_offset;

  /* Variable length data.  */
  struct sfnt_name_record *name_records;

  /* Start of string data.  */
  unsigned char *data;
};

/* Name identifier codes.  These are Apple's codes, not
   Microsoft's.  */

enum sfnt_name_identifier_code
  {
    SFNT_NAME_COPYRIGHT_NOTICE			= 0,
    SFNT_NAME_FONT_FAMILY			= 1,
    SFNT_NAME_FONT_SUBFAMILY			= 2,
    SFNT_NAME_UNIQUE_SUBFAMILY_IDENTIFICATION	= 3,
    SFNT_NAME_FULL_NAME				= 4,
    SFNT_NAME_NAME_TABLE_VERSION		= 5,
    SFNT_NAME_POSTSCRIPT_NAME			= 6,
    SFNT_NAME_TRADEMARK_NOTICE			= 7,
    SFNT_NAME_MANUFACTURER_NAME			= 8,
    SFNT_NAME_DESIGNER				= 9,
    SFNT_NAME_DESCRIPTION			= 10,
    SFNT_NAME_FONT_VENDOR_URL			= 11,
    SFNT_NAME_FONT_DESIGNER_URL			= 12,
    SFNT_NAME_LICENSE_DESCRIPTION		= 13,
    SFNT_NAME_LICENSE_INFORMATION_URL		= 14,
    SFNT_NAME_PREFERRED_FAMILY			= 16,
    SFNT_NAME_PREFERRED_SUBFAMILY		= 17,
    SFNT_NAME_COMPATIBLE_FULL			= 18,
    SFNT_NAME_SAMPLE_TEXT			= 19,
    SFNT_NAME_VARIATIONS_POSTSCRIPT_NAME_PREFIX = 25,
  };

struct sfnt_meta_data_map
{
  /* Identifier for the tag.  */
  uint32_t tag;

  /* Offset from start of table to data.  */
  uint32_t data_offset;

  /* Length of the data.  */
  uint32_t data_length;
};

struct sfnt_meta_table
{
  /* Version of the table.  Currently set to 1.  */
  uint32_t version;

  /* Flags.  Currently 0.  */
  uint32_t flags;

  /* Offset from start of table to beginning of variable length
     data.  */
  uint32_t data_offset;

  /* Number of data maps in the table.  */
  uint32_t num_data_maps;

  /* Beginning of variable length data.  */
  struct sfnt_meta_data_map *data_maps;

  /* The whole table contents.  */
  unsigned char *data;
};

enum sfnt_meta_data_tag
  {
    SFNT_META_DATA_TAG_DLNG = 0x646c6e67,
    SFNT_META_DATA_TAG_SLNG = 0x736c6e67,
  };



/* TrueType collection format support.  */

struct sfnt_ttc_header
{
  /* TrueType collection ID tag.  */
  uint32_t ttctag;

  /* Version of the TTC header.  */
  uint32_t version;

  /* Number of fonts in the TTC header.  */
  uint32_t num_fonts;

  /* Array of offsets to the offset table for each font in the
     file.  */
  uint32_t *offset_table;

  /* Tag indicating that a DSIG table exists, or 0.  Fields from here
     on are only set on version 2.0 headers or later.  */
  uint32_t ul_dsig_tag;

  /* Length in bytes of the signature table, or 0 if there is no
     signature.  */
  uint32_t ul_dsig_length;

  /* Offset in bytes of the dsig table from the beginning of the TTC
     file.  */
  uint32_t ul_dsig_offset;
};

enum sfnt_ttc_tag
  {
    SFNT_TTC_TTCF = 0x74746366,
    SFNT_TTC_DSIG = 0x44534947,
  };



/* Unicode Variation Sequence (UVS) support.  */

struct sfnt_default_uvs_table
{
  /* Number of ranges that follow.  */
  uint32_t num_unicode_value_ranges;

  /* Variable length data.  */
  struct sfnt_unicode_value_range *ranges;
};

struct sfnt_unicode_value_range
{
  /* First value in this range.  */
  unsigned int start_unicode_value;

  /* Number of additional values in this range.  */
  unsigned char additional_count;
};

struct sfnt_nondefault_uvs_table
{
  /* Number of UVS mappings which follow.  */
  uint32_t num_uvs_mappings;

  /* Variable length data.  */
  struct sfnt_uvs_mapping *mappings;
};

struct sfnt_uvs_mapping
{
  /* Base character value.  */
  unsigned int unicode_value;

  /* Glyph ID of the base character value.  */
  uint16_t base_character_value;
};

struct sfnt_mapped_variation_selector_record
{
  /* The variation selector.  */
  unsigned int selector;

  /* Its default UVS table.  */
  struct sfnt_default_uvs_table *default_uvs;

  /* Its nondefault UVS table.  */
  struct sfnt_nondefault_uvs_table *nondefault_uvs;
};

/* Structure describing a single offset to load into a variation
   selection context.  */

struct sfnt_table_offset_rec
{
  /* The offset from the start of the font file.  */
  off_t offset;

  /* Whether or not the offset points to a non-default UVS table.  */
  bool is_nondefault_table;

  /* Pointer to the UVS table.  */
  void *table;
};

struct sfnt_uvs_context
{
  /* Number of records and tables.  */
  size_t num_records, nmemb;

  /* Array of UVS tables.  */
  struct sfnt_table_offset_rec *tables;

  /* Array of variation selector records mapped to
     their corresponding tables.  */
  struct sfnt_mapped_variation_selector_record *records;
};



#if defined HAVE_MMAP && !defined TEST

/* Memory mapping support.  */

struct sfnt_mapped_table
{
  /* Pointer to table data.  */
  void *data;

  /* Pointer to table mapping.  */
  void *mapping;

  /* Size of mapped data and size of mapping.  */
  size_t length, size;
};

#endif /* HAVE_MMAP && !TEST */



/* Glyph variation support.  */

/* 2.14 fixed point type used to represent versors of unit
   vectors.  */
typedef int16_t sfnt_f2dot14;

/* Forward declaration used only for the distortable font stuff.  */
struct sfnt_cvt_table;

struct sfnt_variation_axis
{
  /* The axis tag.  */
  uint32_t axis_tag;

  /* The minimum style coordinate for the axis.  */
  sfnt_fixed min_value;

  /* The default style coordinate for the axis.  */
  sfnt_fixed default_value;

  /* The maximum style coordinate for the axis.  */
  sfnt_fixed max_value;

  /* Set to zero.  */
  uint16_t flags;

  /* Identifier under which this axis's name will be found in the
     `name' table.  */
  uint16_t name_id;
};

struct sfnt_instance
{
  /* The instance name ID.  */
  uint16_t name_id;

  /* Flags.  */
  uint16_t flags;

  /* Optional PostScript name.  */
  uint16_t ps_name_id;

  /* Coordinates of each defined instance.  */
  sfnt_fixed *coords;
};

struct sfnt_fvar_table
{
  /* Major version; should be 1.  */
  uint16_t major_version;

  /* Minor version; should be 0.  */
  uint16_t minor_version;

  /* Offset in bytes from the beginning of the table to the beginning
     of the first axis data.  */
  uint16_t offset_to_data;

  /* Reserved field; always 2.  */
  uint16_t count_size_pairs;

  /* Number of style axes in this font.  */
  uint16_t axis_count;

  /* The number of bytes in each variation axis record.  Currently 20
     bytes.  */
  uint16_t axis_size;

  /* The number of named instances for the font found in the
     instance array.  */
  uint16_t instance_count;

  /* The size of each instance record.  */
  uint16_t instance_size;

  /* Variable length data.  */
  struct sfnt_variation_axis *axis;
  struct sfnt_instance *instance;
};

struct sfnt_short_frac_correspondence
{
  /* Value in normalized user space.  */
  sfnt_f2dot14 from_coord;

  /* Value in normalized axis space.  */
  sfnt_f2dot14 to_coord;
};

struct sfnt_short_frac_segment
{
  /* The number of pairs for this axis.  */
  uint16_t pair_count;

  /* Variable length data.  */
  struct sfnt_short_frac_correspondence *correspondence;
};

struct sfnt_avar_table
{
  /* The version of the table.  Should be 1.0.  */
  sfnt_fixed version;

  /* Number of variation axes defined in this table.
     XXX: why is this signed? */
  int32_t axis_count;

  /* Variable length data.  */
  struct sfnt_short_frac_segment *segments;
};

struct sfnt_tuple_variation
{
  /* Tuple point numbers.  */
  uint16_t *points;

  /* Deltas.  */
  sfnt_fword *deltas;

  /* Tuple coordinates.  One for each axis specified in the [gaf]var
     tables.  */
  sfnt_f2dot14 *coordinates;

  /* Intermediate start and end coordinates.  */
  sfnt_f2dot14 *restrict intermediate_start;

  /* Intermediate start and end coordinates.  */
  sfnt_f2dot14 *restrict intermediate_end;

  /* The number of points and deltas present.

     UINT16_MAX and POINTS set to NULL means there are deltas for each
     CVT entry.  */
  uint16_t num_points;
};

struct sfnt_cvar_table
{
  /* The version of this CVT variations table.  */
  sfnt_fixed version;

  /* Flags.  */
  uint16_t tuple_count;

  /* Offset from the beginning of the table to the tuple data.  */
  uint16_t data_offset;

  /* Variable length data.  */
  struct sfnt_tuple_variation *variation;
};

struct sfnt_gvar_table
{
  /* Version of the glyph variations table.  */
  uint16_t version;

  /* Reserved, currently 0.  */
  uint16_t reserved;

  /* The number of style axes for this font.  This must be the same
     number as axisCount in the 'fvar' table.  */
  uint16_t axis_count;

  /* The number of shared coordinates.  */
  uint16_t shared_coord_count;

  /* Byte offset from the beginning of this table to the list of
     shared style coordinates.  */
  uint32_t offset_to_coord;

  /* The number of glyphs in this font; this should match the number
     of the glyphs store elsewhere in the font.  */
  uint16_t glyph_count;

  /* Bit-field that gives the format of the offset array that
     follows. If the flag is 0, the type is uint16. If the flag is 1,
     the type is unit 32.  */
  uint16_t flags;

  /* Byte offset from the beginning of this table to the first glyph
     glyphVariationData.  */
  uint32_t offset_to_data;

  /* Number of bytes in the glyph variation data.  */
  size_t data_size;

  /* Byte offsets from the beginning of the glyphVariationData array
     to the glyphVariationData for each glyph in the font.  The format
     of this field is set by the flags field.  */
  union {
    uint16_t *offset_word;
    uint32_t *offset_long;
  } u;

  /* Other variable length data.  */
  sfnt_f2dot14 *global_coords;
  unsigned char *glyph_variation_data;
};

/* Structure representing a set of axis coordinates and their
   normalized equivalents.

   To use this structure, call

     sfnt_init_blend (&blend, fvar, gvar)

   on a `struct sfnt_blend *', with an appropriate fvar and gvar
   table.

   Then, fill in blend.coords with the un-normalized coordinates,
   and call

     sfnt_normalize_blend (&blend)

   finally, call sfnt_vary_simple_glyph and related functions.  */

struct sfnt_blend
{
  /* The fvar table.  This determines the number of elements in each
     of the arrays below.  */
  struct sfnt_fvar_table *fvar;

  /* The gvar table.  This provides the glyph variation data.  */
  struct sfnt_gvar_table *gvar;

  /* The avar table.  This provides adjustments to normalized axis
     values, and may be NULL.  */
  struct sfnt_avar_table *avar;

  /* The cvar table.  This provides adjustments to CVT values, and may
     be NULL.  */
  struct sfnt_cvar_table *cvar;

  /* Un-normalized coordinates.  */
  sfnt_fixed *coords;

  /* Normalized coordinates.  */
  sfnt_fixed *norm_coords;
};

struct sfnt_metrics_distortion
{
  /* Distortion applied to the origin point.  */
  sfnt_fword origin;

  /* Distortion applied to the advance point.  */
  sfnt_fword advance;
};



/* OS/2 font metadata.  */

struct sfnt_OS_2_table
{
  /* Table version number.  */
  uint16_t version;

  /* Average weighted advance width of lower case letters and
     space.  */
  int16_t x_avg_char_width;

  /* Wisual weight (degree of blackness or thickness) of stroke in
     glyphs.  */
  uint16_t us_weight_class;

  /* Relative change from the normal aspect ratio (width to height
     ratio) as specified by a font designer for the glyphs in the
     font.  */
  uint16_t us_width_class;

  /* Miscellaneous font attributes.  */
  int16_t fs_type;

  /* Recommended horizontal size in pixels for subscripts.  */
  int16_t y_subscript_x_size;

  /* Recommended vertical subscript size.  */
  int16_t y_subscript_y_size;

  /* Recommended horizontal offset for subscripts.  */
  int16_t y_subscript_x_offset;

  /* Recommended vertical offset from the baseline for subscripts.  */
  int16_t y_subscript_y_offset;

  /* Recommended horizontal size in pixels for superscripts.  */
  int16_t y_superscript_x_size;

  /* Recommended vertical superscript size.  */
  int16_t y_superscript_y_size;

  /* Recommended horizontal offset for superscripts.  */
  int16_t y_superscript_x_offset;

  /* Recommended vertical offset from the baseline for superscripts.  */
  int16_t y_superscript_y_offset;

  /* Width of the strikeout stroke.  */
  int16_t y_strikeout_size;

  /* Position of the strikeout stroke relative to the baseline.  */
  int16_t y_strikeout_position;

  /* Font family classification.  */
  int16_t s_family_class;

  /* Microsoft ``panose'' classification.  */
  unsigned char panose[10];

  /* Alignment boundary! */

  /* Unicode range specification.  */
  uint32_t ul_unicode_range[4];

  /* Font foundry name.  */
  char ach_vendor_id[4];

  /* Two byte bitfield providing the nature of font patterns.  */
  uint16_t fs_selection;

  /* The minimum Unicode codepoint covered.  */
  uint16_t fs_first_char_index;

  /* The maximum Unicode codepoint covered.  */
  uint16_t fs_last_char_index;
};



/* PostScript metadata.  */

struct sfnt_post_table
{
  /* Format of this table.  This is a fixed point number rather than
     an integer.  */
  sfnt_fixed format;

  /* Italic angle in degrees.  */
  sfnt_fixed italic_angle;

  /* Underline position.  */
  sfnt_fword underline_position;

  /* Underline thickness.  */
  sfnt_fword underline_thickness;

  /* Whether the font is monospaced.  */
  uint32_t is_fixed_pitch;

  /* Minimum memory usage (on a PostScript printer) when a TrueType
     font is downloaded as a Type 42 font.  */
  uint32_t min_mem_type_42;

  /* Maximum memory usage (on a PostScript printer) when a TrueType
     font is downloaded as a Type 42 font.  */
  uint32_t max_mem_type_42;

  /* Minimum memory usage (on a PostScript printer) when a TrueType
     font is downloaded as a Type 42 font.  */
  uint32_t min_mem_type_1;

  /* Maximum memory usage (on a PostScript printer) when a TrueType
     font is downloaded as a Type 42 font.  */
  uint32_t max_mem_type_1;
};



#define SFNT_CEIL_FIXED(fixed)	(((fixed) + 0177777) & 037777600000)
#define SFNT_ROUND_FIXED(fixed) (((fixed) + 0100000) & 037777600000)
#define SFNT_FLOOR_FIXED(fixed) ((fixed) & 037777600000)



/* Function declarations.  Keep these sorted by the order in which
   they appear in sfnt.c.  Keep each line no longer than 80
   columns.  */

#ifndef TEST

extern struct sfnt_offset_subtable *sfnt_read_table_directory (int);

#define PROTOTYPE				\
  int, struct sfnt_offset_subtable *,		\
  struct sfnt_cmap_encoding_subtable **,	\
  struct sfnt_cmap_encoding_subtable_data ***
extern struct sfnt_cmap_table *sfnt_read_cmap_table (PROTOTYPE);
#undef PROTOTYPE

extern sfnt_glyph sfnt_lookup_glyph (sfnt_char,
				     struct sfnt_cmap_encoding_subtable_data *);

#define PROTOTYPE int, struct sfnt_offset_subtable *
extern struct sfnt_head_table *sfnt_read_head_table (PROTOTYPE);
extern struct sfnt_hhea_table *sfnt_read_hhea_table (PROTOTYPE);
extern struct sfnt_loca_table_short *sfnt_read_loca_table_short (PROTOTYPE);
extern struct sfnt_loca_table_long *sfnt_read_loca_table_long (PROTOTYPE);
extern struct sfnt_maxp_table *sfnt_read_maxp_table (PROTOTYPE);
extern struct sfnt_glyf_table *sfnt_read_glyf_table (PROTOTYPE);

#ifdef HAVE_MMAP
extern struct sfnt_glyf_table *sfnt_map_glyf_table (PROTOTYPE);
extern int sfnt_unmap_glyf_table (struct sfnt_glyf_table *);
#endif /* HAVE_MMAP */
#undef PROTOTYPE

extern struct sfnt_glyph *sfnt_read_glyph (sfnt_glyph, struct sfnt_glyf_table *,
					   struct sfnt_loca_table_short *,
					   struct sfnt_loca_table_long *);
extern void sfnt_free_glyph (struct sfnt_glyph *);

#define PROTOTYPE		\
  struct sfnt_glyph *,		\
  sfnt_fixed,			\
  struct sfnt_glyph_metrics *,	\
  sfnt_get_glyph_proc,		\
  sfnt_free_glyph_proc,		\
  sfnt_get_metrics_proc,	\
  void *
extern struct sfnt_glyph_outline *sfnt_build_glyph_outline (PROTOTYPE);
#undef PROTOTYPE

extern void sfnt_prepare_raster (struct sfnt_raster *,
				 struct sfnt_glyph_outline *);

#define PROTOTYPE struct sfnt_glyph_outline *
extern struct sfnt_raster *sfnt_raster_glyph_outline (PROTOTYPE);
extern struct sfnt_raster *sfnt_raster_glyph_outline_exact (PROTOTYPE);
#undef PROTOTYPE

#define PROTOTYPE			\
  int,					\
  struct sfnt_offset_subtable *,	\
  struct sfnt_hhea_table *,		\
  struct sfnt_maxp_table *
extern struct sfnt_hmtx_table *sfnt_read_hmtx_table (PROTOTYPE);
#undef PROTOTYPE

extern int sfnt_lookup_glyph_metrics (sfnt_glyph,
				      struct sfnt_glyph_metrics *,
				      struct sfnt_hmtx_table *,
				      struct sfnt_hhea_table *,
				      struct sfnt_maxp_table *);

extern void sfnt_scale_metrics (struct sfnt_glyph_metrics *,
				sfnt_fixed);
extern sfnt_fixed sfnt_get_scale (struct sfnt_head_table *, int);

#define PROTOTYPE int, struct sfnt_offset_subtable *
extern struct sfnt_name_table *sfnt_read_name_table (PROTOTYPE);
#undef PROTOTYPE

extern unsigned char *sfnt_find_name (struct sfnt_name_table *,
				      enum sfnt_name_identifier_code,
				      struct sfnt_name_record *);

#define PROTOTYPE int, struct sfnt_offset_subtable *
extern struct sfnt_meta_table *sfnt_read_meta_table (PROTOTYPE);
#undef PROTOTYPE

extern char *sfnt_find_metadata (struct sfnt_meta_table *,
				 enum sfnt_meta_data_tag,
				 struct sfnt_meta_data_map *);

extern struct sfnt_ttc_header *sfnt_read_ttc_header (int);



#define PROTOTYPE struct sfnt_cmap_format_14 *, int

extern struct sfnt_uvs_context *sfnt_create_uvs_context (PROTOTYPE);

#undef PROTOTYPE

extern void sfnt_free_uvs_context (struct sfnt_uvs_context *);

#define PROTOTYPE struct sfnt_nondefault_uvs_table *, sfnt_char

extern sfnt_glyph sfnt_variation_glyph_for_char (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE struct sfnt_default_uvs_table *, sfnt_char

extern bool sfnt_is_character_default (PROTOTYPE);

#undef PROTOTYPE



#ifdef HAVE_MMAP

extern int sfnt_map_table (int, struct sfnt_offset_subtable *,
			   uint32_t, struct sfnt_mapped_table *);
extern int sfnt_unmap_table (struct sfnt_mapped_table *);

#endif /* HAVE_MMAP */



extern void *sfnt_read_table (int, struct sfnt_offset_subtable *,
			      uint32_t, size_t *);



#define PROTOTYPE int, struct sfnt_offset_subtable *

extern struct sfnt_fvar_table *sfnt_read_fvar_table (PROTOTYPE);
extern struct sfnt_gvar_table *sfnt_read_gvar_table (PROTOTYPE);
extern struct sfnt_avar_table *sfnt_read_avar_table (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  int,						\
  struct sfnt_offset_subtable *,		\
  struct sfnt_fvar_table *,			\
  struct sfnt_cvt_table *

extern struct sfnt_cvar_table *sfnt_read_cvar_table (PROTOTYPE);

#undef PROTOTYPE



extern void sfnt_init_blend (struct sfnt_blend *,
			     struct sfnt_fvar_table *,
			     struct sfnt_gvar_table *,
			     struct sfnt_avar_table *,
			     struct sfnt_cvar_table *);
extern void sfnt_free_blend (struct sfnt_blend *);
extern void sfnt_normalize_blend (struct sfnt_blend *);



extern int sfnt_vary_simple_glyph (struct sfnt_blend *, sfnt_glyph,
				   struct sfnt_glyph *,
				   struct sfnt_metrics_distortion *);
extern int sfnt_vary_compound_glyph (struct sfnt_blend *, sfnt_glyph,
				     struct sfnt_glyph *,
				     struct sfnt_metrics_distortion *);



#define PROTOTYPE int, struct sfnt_offset_subtable *

extern struct sfnt_OS_2_table *sfnt_read_OS_2_table (PROTOTYPE);

#undef PROTOTYPE



#define PROTOTYPE int, struct sfnt_offset_subtable *

extern struct sfnt_post_table *sfnt_read_post_table (PROTOTYPE);

#undef PROTOTYPE

#endif /* TEST */



/* TrueType hinting support.  */

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



/* Fixed point types used by the TrueType interpreter.  */

/* 26.6 fixed point type used within the interpreter.  */
typedef int32_t sfnt_f26dot6;

/* 18.14 fixed point type used to calculate rounding details.  */
typedef int32_t sfnt_f18dot14;



/* Interpreter execution environment.  */

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

  /* Pointer to the X axis current point data.  */
  sfnt_f26dot6 *restrict x_current;

  /* Pointer to the Y axis point data.  */
  sfnt_f26dot6 *restrict y_points;

  /* Pointer to the Y axis current point data.  */
  sfnt_f26dot6 *restrict y_current;

  /* Pointer to the flags associated with this data.  */
  unsigned char *flags;

  /* If this structure was produced from a simple glyph, pointer to
     the simple glyph itself.  NULL otherwise.  */
  struct sfnt_simple_glyph *simple;
};

enum
  {
    /* Bits 1 stands for X_SHORT_VECTOR on disk and in the tables, but
       this representation is not useful in memory.  Inside an
       instructed glyph, this bit is repurposed to mean that the
       corresponding point is a phantom point.  */
    SFNT_POINT_PHANTOM	= (1 << 1),
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

  /* Pointer to the function used to project euclidean vectors onto
     the dual projection vector.  Value is the magnitude of the
     projected vector.  */
  sfnt_f26dot6 (*dual_project) (sfnt_f26dot6, sfnt_f26dot6,
				struct sfnt_interpreter *);

  /* Pointer to the function used to move specified points
     along the freedom vector by a distance specified in terms
     of the projection vector.  */
  void (*move) (sfnt_f26dot6 *restrict,
		sfnt_f26dot6 *restrict, size_t,
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
  unsigned char *restrict instructions;

  /* The twilight zone.  May not be NULL.  */
  sfnt_f26dot6 *restrict twilight_x, *restrict twilight_y;

  /* The original X positions of points in the twilight zone.  */
  sfnt_f26dot6 *restrict twilight_original_x;

  /* The original Y positions of points in the twilight zone.

     Apple does not directly say whether or not points in the twilight
     zone can have their original positions changed.  But this is
     implied by ``create points in the twilight zone''.  */
  sfnt_f26dot6 *restrict twilight_original_y;

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

  /* Number of variation axes provided by this distortable font.  */
  int n_axis;

  /* Normalized axis coordinates set for this distortable font.  */
  sfnt_fixed *norm_coords;

#ifdef TEST
  /* If non-NULL, function called before each instruction is
     executed.  */
  void (*run_hook) (struct sfnt_interpreter *);

  /* If non-NULL, function called before each stack element is
     pushed.  */
  void (*push_hook) (struct sfnt_interpreter *, uint32_t);

  /* If non-NULL, function called before each stack element is
     popped.  */
  void (*pop_hook) (struct sfnt_interpreter *, uint32_t);
#endif
};



/* Glyph hinting.  */

/* Structure describing a single scaled and fitted outline.  */

struct sfnt_instructed_outline
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

  /* The flags of each point.  */
  unsigned char *flags;
};



/* Functions used to read tables used by the TrueType interpreter.  */

#ifndef TEST

#define PROTOTYPE int, struct sfnt_offset_subtable *

extern struct sfnt_cvt_table *sfnt_read_cvt_table (PROTOTYPE);
extern struct sfnt_fpgm_table *sfnt_read_fpgm_table (PROTOTYPE);
extern struct sfnt_prep_table *sfnt_read_prep_table (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  struct sfnt_maxp_table *,			\
  struct sfnt_cvt_table *,			\
  struct sfnt_head_table *,			\
  struct sfnt_fvar_table *,			\
  int, int

extern struct sfnt_interpreter *sfnt_make_interpreter (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  struct sfnt_interpreter *,			\
  struct sfnt_fpgm_table *

extern const char *sfnt_interpret_font_program (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  struct sfnt_interpreter *,			\
  struct sfnt_prep_table *,			\
  struct sfnt_graphics_state *

extern const char *sfnt_interpret_control_value_program (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE struct sfnt_instructed_outline *, sfnt_fixed *

extern struct sfnt_glyph_outline *sfnt_build_instructed_outline (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  struct sfnt_glyph *,				\
  struct sfnt_interpreter *,			\
  struct sfnt_glyph_metrics *,	       		\
  struct sfnt_instructed_outline **

extern const char *sfnt_interpret_simple_glyph (PROTOTYPE);

#undef PROTOTYPE

#define PROTOTYPE				\
  struct sfnt_glyph *,				\
  struct sfnt_interpreter *,			\
  struct sfnt_graphics_state *,			\
  sfnt_get_glyph_proc,				\
  sfnt_free_glyph_proc,				\
  struct sfnt_hmtx_table *,			\
  struct sfnt_hhea_table *,			\
  struct sfnt_maxp_table *,			\
  struct sfnt_glyph_metrics *,			\
  void *,					\
  struct sfnt_instructed_outline **

extern const char *sfnt_interpret_compound_glyph (PROTOTYPE);

#undef PROTOTYPE



extern void sfnt_vary_interpreter (struct sfnt_interpreter *,
				   struct sfnt_blend *);

#endif /* TEST */



#endif /* _SFNT_H_ */
