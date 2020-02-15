/* Header for coding system handler.
   Copyright (C) 2001-2020 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

#ifndef EMACS_CODING_H
#define EMACS_CODING_H

#include "lisp.h"

INLINE_HEADER_BEGIN

/* Index to arguments of Fdefine_coding_system_internal.  */

enum define_coding_system_arg_index
  {
    coding_arg_name,
    coding_arg_mnemonic,
    coding_arg_coding_type,
    coding_arg_charset_list,
    coding_arg_ascii_compatible_p,
    coding_arg_decode_translation_table,
    coding_arg_encode_translation_table,
    coding_arg_post_read_conversion,
    coding_arg_pre_write_conversion,
    coding_arg_default_char,
    coding_arg_for_unibyte,
    coding_arg_plist,
    coding_arg_eol_type,
    coding_arg_max
  };

enum define_coding_iso2022_arg_index
  {
    coding_arg_iso2022_initial = coding_arg_max,
    coding_arg_iso2022_reg_usage,
    coding_arg_iso2022_request,
    coding_arg_iso2022_flags,
    coding_arg_iso2022_max
  };

enum define_coding_utf8_arg_index
  {
    coding_arg_utf8_bom = coding_arg_max,
    coding_arg_utf8_max
  };

enum define_coding_utf16_arg_index
  {
    coding_arg_utf16_bom = coding_arg_max,
    coding_arg_utf16_endian,
    coding_arg_utf16_max
  };

enum define_coding_ccl_arg_index
  {
    coding_arg_ccl_decoder = coding_arg_max,
    coding_arg_ccl_encoder,
    coding_arg_ccl_valids,
    coding_arg_ccl_max
  };

enum define_coding_undecided_arg_index
  {
    coding_arg_undecided_inhibit_nul_byte_detection = coding_arg_max,
    coding_arg_undecided_inhibit_iso_escape_detection,
    coding_arg_undecided_prefer_utf_8,
    coding_arg_undecided_max
  };

/* Hash table for all coding systems.  Keys are coding system symbols
   and values are spec vectors of the corresponding coding system.  A
   spec vector has the form [ ATTRS ALIASES EOL-TYPE ].  ATTRS is a
   vector of attribute of the coding system.  ALIASES is a list of
   aliases (symbols) of the coding system.  EOL-TYPE is `unix', `dos',
   `mac' or a vector of coding systems (symbols).  */

extern Lisp_Object Vcoding_system_hash_table;

/* Name (or base name) of work buffer for code conversion.  */
extern Lisp_Object Vcode_conversion_workbuf_name;

/* Enumeration of index to an attribute vector of a coding system.  */

enum coding_attr_index
  {
    coding_attr_base_name,
    coding_attr_docstring,
    coding_attr_mnemonic,
    coding_attr_type,
    coding_attr_charset_list,
    coding_attr_ascii_compat,
    coding_attr_decode_tbl,
    coding_attr_encode_tbl,
    coding_attr_trans_tbl,
    coding_attr_post_read,
    coding_attr_pre_write,
    coding_attr_default_char,
    coding_attr_for_unibyte,
    coding_attr_plist,

    coding_attr_category,
    coding_attr_safe_charsets,

    /* The followings are extra attributes for each type.  */
    coding_attr_charset_valids,

    coding_attr_ccl_decoder,
    coding_attr_ccl_encoder,
    coding_attr_ccl_valids,

    coding_attr_iso_initial,
    coding_attr_iso_usage,
    coding_attr_iso_request,
    coding_attr_iso_flags,

    coding_attr_utf_bom,
    coding_attr_utf_16_endian,

    coding_attr_emacs_mule_full,

    coding_attr_undecided_inhibit_nul_byte_detection,
    coding_attr_undecided_inhibit_iso_escape_detection,
    coding_attr_undecided_prefer_utf_8,

    coding_attr_last_index
  };


/* Macros to access an element of an attribute vector.  */

#define CODING_ATTR_BASE_NAME(attrs)	AREF (attrs, coding_attr_base_name)
#define CODING_ATTR_TYPE(attrs)		AREF (attrs, coding_attr_type)
#define CODING_ATTR_CHARSET_LIST(attrs)	AREF (attrs, coding_attr_charset_list)
#define CODING_ATTR_MNEMONIC(attrs)	AREF (attrs, coding_attr_mnemonic)
#define CODING_ATTR_DOCSTRING(attrs)	AREF (attrs, coding_attr_docstring)
#define CODING_ATTR_ASCII_COMPAT(attrs)	AREF (attrs, coding_attr_ascii_compat)
#define CODING_ATTR_DECODE_TBL(attrs)	AREF (attrs, coding_attr_decode_tbl)
#define CODING_ATTR_ENCODE_TBL(attrs)	AREF (attrs, coding_attr_encode_tbl)
#define CODING_ATTR_TRANS_TBL(attrs)	AREF (attrs, coding_attr_trans_tbl)
#define CODING_ATTR_POST_READ(attrs)	AREF (attrs, coding_attr_post_read)
#define CODING_ATTR_PRE_WRITE(attrs)	AREF (attrs, coding_attr_pre_write)
#define CODING_ATTR_DEFAULT_CHAR(attrs)	AREF (attrs, coding_attr_default_char)
#define CODING_ATTR_FOR_UNIBYTE(attrs)	AREF (attrs, coding_attr_for_unibyte)
#define CODING_ATTR_PLIST(attrs)	AREF (attrs, coding_attr_plist)
#define CODING_ATTR_CATEGORY(attrs)	AREF (attrs, coding_attr_category)
#define CODING_ATTR_SAFE_CHARSETS(attrs)AREF (attrs, coding_attr_safe_charsets)


/* Return the name of a coding system specified by ID.  */
#define CODING_ID_NAME(id) \
  (HASH_KEY (XHASH_TABLE (Vcoding_system_hash_table), id))

/* Return the attribute vector of a coding system specified by ID.  */

#define CODING_ID_ATTRS(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 0))

/* Return the list of aliases of a coding system specified by ID.  */

#define CODING_ID_ALIASES(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 1))

/* Return the eol-type of a coding system specified by ID.  */

#define CODING_ID_EOL_TYPE(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 2))


/* Return the spec vector of CODING_SYSTEM_SYMBOL.  */

#define CODING_SYSTEM_SPEC(coding_system_symbol)	\
  (Fgethash (coding_system_symbol, Vcoding_system_hash_table, Qnil))


/* Return the ID of CODING_SYSTEM_SYMBOL.  */

#define CODING_SYSTEM_ID(coding_system_symbol)			\
  hash_lookup (XHASH_TABLE (Vcoding_system_hash_table),		\
	       coding_system_symbol, NULL)

/* Return true if CODING_SYSTEM_SYMBOL is a coding system.  */

#define CODING_SYSTEM_P(coding_system_symbol)		\
  (CODING_SYSTEM_ID (coding_system_symbol) >= 0		\
   || (! NILP (coding_system_symbol)			\
       && ! NILP (Fcoding_system_p (coding_system_symbol))))

/* Check if X is a coding system or not.  */

#define CHECK_CODING_SYSTEM(x)				\
  do {							\
    if (CODING_SYSTEM_ID (x) < 0			\
	&& NILP (Fcheck_coding_system (x)))		\
      wrong_type_argument (Qcoding_system_p, (x));	\
  } while (false)


/* Check if X is a coding system or not.  If it is, set SEPC to the
   spec vector of the coding system.  */

#define CHECK_CODING_SYSTEM_GET_SPEC(x, spec)		\
  do {							\
    spec = CODING_SYSTEM_SPEC (x);			\
    if (NILP (spec))					\
      {							\
	Fcheck_coding_system (x);			\
	spec = CODING_SYSTEM_SPEC (x);			\
      }							\
    if (NILP (spec))					\
      wrong_type_argument (Qcoding_system_p, (x));	\
  } while (false)


/* Check if X is a coding system or not.  If it is, set ID to the
   ID of the coding system.  */

#define CHECK_CODING_SYSTEM_GET_ID(x, id)			\
  do								\
    {								\
      id = CODING_SYSTEM_ID (x);				\
      if (id < 0)						\
	{							\
	  Fcheck_coding_system (x);				\
	  id = CODING_SYSTEM_ID (x);				\
	}							\
      if (id < 0)						\
	wrong_type_argument (Qcoding_system_p, (x));	\
    } while (false)


/*** GENERAL section ***/

/* Enumeration of result code of code conversion.  */
enum coding_result_code
  {
    CODING_RESULT_SUCCESS,
    CODING_RESULT_INSUFFICIENT_SRC,
    CODING_RESULT_INSUFFICIENT_DST,
    CODING_RESULT_INVALID_SRC,
    CODING_RESULT_INTERRUPT
  };


/* Macros used for the member `mode' of the struct coding_system.  */

/* If set, the decoding/encoding routines treat the current data as
   the last block of the whole text to be converted, and do the
   appropriate finishing job.  */
#define CODING_MODE_LAST_BLOCK			0x01

/* If set, it means that the current source text is in a buffer which
   enables selective display.  */
#define CODING_MODE_SELECTIVE_DISPLAY		0x02

/* This flag is used by the decoding/encoding routines on the fly.  If
   set, it means that right-to-left text is being processed.  */
#define CODING_MODE_DIRECTION			0x04

#define CODING_MODE_FIXED_DESTINATION		0x08

/* If set, it means that the encoding routines produces some safe
   ASCII characters (usually '?') for unsupported characters.  */
#define CODING_MODE_SAFE_ENCODING		0x10

  /* For handling composition sequence.  */
#include "composite.h"

enum composition_state
  {
    COMPOSING_NO,
    COMPOSING_CHAR,
    COMPOSING_RULE,
    COMPOSING_COMPONENT_CHAR,
    COMPOSING_COMPONENT_RULE
  };

/* Structure for the current composition status.  */
struct composition_status
{
  enum composition_state state;
  enum composition_method method;
  bool old_form;	  /* true if pre-21 form */
  int length;		  /* number of elements produced in charbuf */
  int nchars;		  /* number of characters composed */
  int ncomps;		  /* number of composition components */
  /* Maximum carryover is for the case of COMPOSITION_WITH_RULE_ALTCHARS.
     See the comment in coding.c.  */
  int carryover[4 		/* annotation header */
		+ MAX_COMPOSITION_COMPONENTS * 3 - 2 /* ALTs and RULEs */
		+ 2				     /* intermediate -1 -1 */
		+ MAX_COMPOSITION_COMPONENTS	     /* CHARs */
		];
};


/* Structure of the field `spec.iso_2022' in the structure
   `coding_system'.  */
struct iso_2022_spec
{
  /* Bit-wise-or of CODING_ISO_FLAG_XXX.  */
  unsigned flags;

  /* The current graphic register invoked to each graphic plane.  */
  int current_invocation[2];

  /* The current charset designated to each graphic register.  The
     value -1 means that not charset is designated, -2 means that
     there was an invalid designation previously.  */
  int current_designation[4];

  /* If positive, we are now scanning CTEXT extended segment.  */
  int ctext_extended_segment_len;

  /* True temporarily only when graphic register 2 or 3 is invoked by
     single-shift while encoding.  */
  bool_bf single_shifting : 1;

  /* True temporarily only when processing at beginning of line.  */
  bool_bf bol : 1;

  /* If true, we are now scanning embedded UTF-8 sequence.  */
  bool_bf embedded_utf_8 : 1;

  /* The current composition.  */
  struct composition_status cmp_status;
};

struct emacs_mule_spec
{
  struct composition_status cmp_status;
};

struct undecided_spec
{
  /* Inhibit NUL byte detection.  1 means always inhibit,
     -1 means do not inhibit, 0 means rely on user variable.  */
  int inhibit_nbd;

  /* Inhibit ISO escape detection.  -1, 0, 1 as above.  */
  int inhibit_ied;

  /* Prefer UTF-8 when the input could be other encodings.  */
  bool prefer_utf_8;
};

enum utf_bom_type
  {
    utf_detect_bom,
    utf_without_bom,
    utf_with_bom
  };

enum utf_16_endian_type
  {
    utf_16_big_endian,
    utf_16_little_endian
  };

struct utf_16_spec
{
  enum utf_bom_type bom;
  enum utf_16_endian_type endian;
  int surrogate;
};

struct coding_detection_info
{
  /* Values of these members are bitwise-OR of CATEGORY_MASK_XXXs.  */
  /* Which categories are already checked.  */
  int checked;
  /* Which categories are strongly found.  */
  int found;
  /* Which categories are rejected.  */
  int rejected;
};


struct coding_system
{
  /* ID number of the coding system.  This is an index to
     Vcoding_system_hash_table.  This value is set by
     setup_coding_system.  At the early stage of building time, this
     value is -1 in the array coding_categories to indicate that no
     coding-system of that category is yet defined.  */
  ptrdiff_t id;

  /* Flag bits of the coding system.  The meaning of each bit is common
     to all types of coding systems.  */
  unsigned common_flags : 14;

  /* Mode bits of the coding system.  See the comments of the macros
     CODING_MODE_XXX.  */
  unsigned mode : 5;

  /* The following two members specify how binary 8-bit code 128..255
     are represented in source and destination text respectively.  True
     means they are represented by 2-byte sequence, false means they are
     represented by 1-byte as is (see the comment in character.h).  */
  bool_bf src_multibyte : 1;
  bool_bf dst_multibyte : 1;

  /* True if the source of conversion is not in the member
     `charbuf', but at `src_object'.  */
  bool_bf chars_at_source : 1;

  /* Nonzero if the result of conversion is in `destination'
     buffer rather than in `dst_object'.  */
  bool_bf raw_destination : 1;

  /* Set to true if charbuf contains an annotation.  */
  bool_bf annotated : 1;

  /* Used internally in coding.c.  See the comment of detect_ascii.  */
  unsigned eol_seen : 3;

  /* Finish status of code conversion.  */
  ENUM_BF (coding_result_code) result : 3;

  int max_charset_id;

  /* Detailed information specific to each type of coding system.  */
  union
    {
      struct iso_2022_spec iso_2022;
      struct ccl_spec *ccl;	/* Defined in ccl.h.  */
      struct utf_16_spec utf_16;
      enum utf_bom_type utf_8_bom;
      struct emacs_mule_spec emacs_mule;
      struct undecided_spec undecided;
    } spec;

  unsigned char *safe_charsets;

  /* How many heading bytes we can skip for decoding.  This is set to
     -1 in setup_coding_system, and updated by detect_coding.  So,
     when this is equal to the byte length of the text being
     converted, we can skip the actual conversion process except for
     the eol format.  */
  ptrdiff_t head_ascii;

  /* How many bytes/chars at the source are detected as valid utf-8
     sequence.  Set by detect_coding_utf_8.  */
  ptrdiff_t detected_utf8_bytes, detected_utf8_chars;

  /* The following members are set by encoding/decoding routine.  */
  ptrdiff_t produced, produced_char, consumed, consumed_char;

  ptrdiff_t src_pos, src_pos_byte, src_chars, src_bytes;
  Lisp_Object src_object;
  const unsigned char *source;

  ptrdiff_t dst_pos, dst_pos_byte, dst_bytes;
  Lisp_Object dst_object;
  unsigned char *destination;

  /* If an element is non-negative, it is a character code.

     If it is in the range -128..-1, it is a 8-bit character code
     minus 256.

     If it is less than -128, it specifies the start of an annotation
     chunk.  The length of the chunk is -128 minus the value of the
     element.  The following elements are OFFSET, ANNOTATION-TYPE, and
     a sequence of actual data for the annotation.  OFFSET is a
     character position offset from dst_pos or src_pos,
     ANNOTATION-TYPE specifies the meaning of the annotation and how to
     handle the following data..  */
  int *charbuf;
  int charbuf_size, charbuf_used;

  unsigned char carryover[64];
  int carryover_bytes;

  int default_char;

  bool (*detector) (struct coding_system *, struct coding_detection_info *);
  void (*decoder) (struct coding_system *);
  bool (*encoder) (struct coding_system *);
};

/* Meanings of bits in the member `common_flags' of the structure
   coding_system.  The lowest 8 bits are reserved for various kind of
   annotations (currently two of them are used).  */
#define CODING_ANNOTATION_MASK			0x00FF
#define CODING_ANNOTATE_COMPOSITION_MASK	0x0001
#define CODING_ANNOTATE_DIRECTION_MASK		0x0002
#define CODING_ANNOTATE_CHARSET_MASK		0x0003
#define CODING_FOR_UNIBYTE_MASK			0x0100
#define CODING_REQUIRE_FLUSHING_MASK		0x0200
#define CODING_REQUIRE_DECODING_MASK		0x0400
#define CODING_REQUIRE_ENCODING_MASK		0x0800
#define CODING_REQUIRE_DETECTION_MASK		0x1000
#define CODING_RESET_AT_BOL_MASK		0x2000

/* Return nonzero if the coding context CODING requires annotation
   handling.  */
#define CODING_REQUIRE_ANNOTATION(coding) \
  ((coding)->common_flags & CODING_ANNOTATION_MASK)

/* Return nonzero if the coding context CODING prefers decoding into
   unibyte.  */
#define CODING_FOR_UNIBYTE(coding) \
  ((coding)->common_flags & CODING_FOR_UNIBYTE_MASK)

/* Return nonzero if the coding context CODING requires specific code to be
   attached at the tail of converted text.  */
#define CODING_REQUIRE_FLUSHING(coding) \
  ((coding)->common_flags & CODING_REQUIRE_FLUSHING_MASK)

/* Return nonzero if the coding context CODING requires code conversion on
   decoding.  */
#define CODING_REQUIRE_DECODING(coding)	\
  ((coding)->dst_multibyte		\
   || (coding)->common_flags & CODING_REQUIRE_DECODING_MASK)


/* Return nonzero if the coding context CODING requires code conversion on
   encoding.
   The non-multibyte part of the condition is to support encoding of
   unibyte strings/buffers generated by string-as-unibyte or
   (set-buffer-multibyte nil) from multibyte strings/buffers.  */
#define CODING_REQUIRE_ENCODING(coding)				\
  ((coding)->src_multibyte					\
   || (coding)->common_flags & CODING_REQUIRE_ENCODING_MASK	\
   || (coding)->mode & CODING_MODE_SELECTIVE_DISPLAY)


/* Return nonzero if the coding context CODING requires some kind of code
   detection.  */
#define CODING_REQUIRE_DETECTION(coding) \
  ((coding)->common_flags & CODING_REQUIRE_DETECTION_MASK)

/* Return nonzero if the coding context CODING requires code conversion on
   decoding or some kind of code detection.  */
#define CODING_MAY_REQUIRE_DECODING(coding)	\
  (CODING_REQUIRE_DECODING (coding)		\
   || CODING_REQUIRE_DETECTION (coding))

/* Macros to decode or encode a character of JISX0208 in SJIS.  S1 and
   S2 are the 1st and 2nd position-codes of JISX0208 in SJIS coding
   system.  C1 and C2 are the 1st and 2nd position codes of Emacs'
   internal format.  */

#define SJIS_TO_JIS(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    s1 = (code) >> 8, s2 = (code) & 0xFF;		\
							\
    if (s2 >= 0x9F)					\
      (j1 = s1 * 2 - (s1 >= 0xE0 ? 0x160 : 0xE0),	\
       j2 = s2 - 0x7E);					\
    else						\
      (j1 = s1 * 2 - ((s1 >= 0xE0) ? 0x161 : 0xE1),	\
       j2 = s2 - ((s2 >= 0x7F) ? 0x20 : 0x1F));		\
    (code) = (j1 << 8) | j2;				\
  } while (false)

#define SJIS_TO_JIS2(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    s1 = (code) >> 8, s2 = (code) & 0xFF;		\
							\
    if (s2 >= 0x9F)					\
      {							\
	j1 = (s1 == 0xF0 ? 0x28				\
	      : s1 == 0xF1 ? 0x24			\
	      : s1 == 0xF2 ? 0x2C			\
	      : s1 == 0xF3 ? 0x2E			\
	      : 0x6E + (s1 - 0xF4) * 2);		\
	j2 = s2 - 0x7E;					\
      }							\
    else						\
      {							\
	j1 = (s1 <= 0xF2 ? 0x21 + (s1 - 0xF0) * 2	\
	      : s1 <= 0xF4 ? 0x2D + (s1 - 0xF3) * 2	\
	      : 0x6F + (s1 - 0xF5) * 2);		\
	j2 = s2 - ((s2 >= 0x7F ? 0x20 : 0x1F));		\
      }							\
    (code) = (j1 << 8) | j2;				\
  } while (false)


#define JIS_TO_SJIS(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    j1 = (code) >> 8, j2 = (code) & 0xFF;		\
    if (j1 & 1)						\
      (s1 = j1 / 2 + ((j1 < 0x5F) ? 0x71 : 0xB1),	\
       s2 = j2 + ((j2 >= 0x60) ? 0x20 : 0x1F));		\
    else						\
      (s1 = j1 / 2 + ((j1 < 0x5F) ? 0x70 : 0xB0),	\
       s2 = j2 + 0x7E);					\
    (code) = (s1 << 8) | s2;				\
  } while (false)

#define JIS_TO_SJIS2(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    j1 = (code) >> 8, j2 = (code) & 0xFF;		\
    if (j1 & 1)						\
      {							\
	s1 = (j1 <= 0x25 ? 0xF0 + (j1 - 0x21) / 2	\
	      : j1 <= 0x2F ? 0xF3 + (j1 - 0x2D) / 2	\
	      : 0xF5 + (j1 - 0x6F) / 2);		\
	s2 = j2 + ((j2 >= 0x60) ? 0x20 : 0x1F);		\
      }							\
    else						\
      {							\
	s1 = (j1 == 0x28 ? 0xF0				\
	      : j1 == 0x24 ? 0xF1			\
	      : j1 == 0x2C ? 0xF2			\
	      : j1 == 0x2E ? 0xF3			\
	      : 0xF4 + (j1 - 0x6E) / 2);		\
	s2 = j2 + 0x7E;					\
      }							\
    (code) = (s1 << 8) | s2;				\
  } while (false)

/* Encode the file name NAME using the specified coding system
   for file names, if any.  */
#define ENCODE_FILE(NAME)  encode_file_name (NAME)

/* Decode the file name NAME using the specified coding system
   for file names, if any.  */
#define DECODE_FILE(NAME)  decode_file_name (NAME)

/* Encode the string STR using the specified coding system
   for system functions, if any.  */
#define ENCODE_SYSTEM(str)						   \
  (! NILP (Vlocale_coding_system)					   \
   ? code_convert_string_norecord (str, Vlocale_coding_system, true)	   \
   : str)

/* Decode the string STR using the specified coding system
   for system functions, if any.  */
#define DECODE_SYSTEM(str)						   \
  (! NILP (Vlocale_coding_system)					   \
   ? code_convert_string_norecord (str, Vlocale_coding_system, false)	   \
   : str)

/* Note that this encodes utf-8, not utf-8-emacs, so it's not a no-op.  */
#define ENCODE_UTF_8(str) code_convert_string_norecord (str, Qutf_8, true)

/* Return true if VAL is a high surrogate.  VAL must be a 16-bit code
   unit.  */

#define UTF_16_HIGH_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xD800)

/* Return true if VAL is a low surrogate.  VAL must be a 16-bit code
   unit.  */

#define UTF_16_LOW_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xDC00)

/* Extern declarations.  */
extern Lisp_Object code_conversion_save (bool, bool);
extern bool encode_coding_utf_8 (struct coding_system *);
extern bool utf8_string_p (Lisp_Object);
extern void setup_coding_system (Lisp_Object, struct coding_system *);
extern Lisp_Object coding_charset_list (struct coding_system *);
extern Lisp_Object coding_system_charset_list (Lisp_Object);
extern Lisp_Object code_convert_string (Lisp_Object, Lisp_Object,
                                        Lisp_Object, bool, bool, bool);
extern Lisp_Object code_convert_string_norecord (Lisp_Object, Lisp_Object,
                                                 bool);
extern Lisp_Object encode_string_utf_8 (Lisp_Object, Lisp_Object, bool,
					Lisp_Object, Lisp_Object);
extern Lisp_Object decode_string_utf_8 (Lisp_Object, const char *, ptrdiff_t,
					Lisp_Object, bool,
					Lisp_Object, Lisp_Object);
extern Lisp_Object encode_file_name (Lisp_Object);
extern Lisp_Object decode_file_name (Lisp_Object);
extern Lisp_Object raw_text_coding_system (Lisp_Object);
extern bool raw_text_coding_system_p (struct coding_system *);
extern Lisp_Object coding_inherit_eol_type (Lisp_Object, Lisp_Object);
extern Lisp_Object complement_process_encoding_system (Lisp_Object);
extern Lisp_Object make_string_from_utf8 (const char *, ptrdiff_t);

extern void decode_coding_gap (struct coding_system *, ptrdiff_t);
extern void decode_coding_object (struct coding_system *,
                                  Lisp_Object, ptrdiff_t, ptrdiff_t,
                                  ptrdiff_t, ptrdiff_t, Lisp_Object);
extern void encode_coding_object (struct coding_system *,
                                  Lisp_Object, ptrdiff_t, ptrdiff_t,
                                  ptrdiff_t, ptrdiff_t, Lisp_Object);
/* Defined in this file.  */
INLINE int surrogates_to_codepoint (int, int);

#if defined (WINDOWSNT) || defined (CYGWIN)

/* These functions use Lisp string objects to store the UTF-16LE
   strings that modern versions of Windows expect.  These strings are
   not particularly useful to Lisp, and all Lisp strings should be
   native Emacs multibyte.  */

/* Access the wide-character string stored in a Lisp string object.  */
#define WCSDATA(x) ((wchar_t *) SDATA (x))

/* Convert the multi-byte string in STR to UTF-16LE encoded unibyte
   string, and store it in *BUF.  BUF may safely point to STR on entry.  */
extern wchar_t *to_unicode (Lisp_Object str, Lisp_Object *buf);

/* Convert STR, a UTF-16LE encoded string embedded in a unibyte string
   object, to a multi-byte Emacs string and return it.  This function
   calls code_convert_string_norecord internally and has all its
   failure modes.  STR itself is not modified.  */
extern Lisp_Object from_unicode (Lisp_Object str);

/* Convert WSTR to an Emacs string.  */
extern Lisp_Object from_unicode_buffer (const wchar_t *wstr);

#endif /* WINDOWSNT || CYGWIN */

/* Macros for backward compatibility.  */

#define encode_coding_string(coding, string, nocopy)			\
  (STRING_MULTIBYTE(string) ?						\
    (encode_coding_object (coding, string, 0, 0, SCHARS (string),	\
			   SBYTES (string), Qt),			\
     (coding)->dst_object) : (string))


#define decode_coding_c_string(coding, src, bytes, dst_object)		\
  do {									\
    (coding)->source = (src);						\
    (coding)->src_chars = (coding)->src_bytes = (bytes);		\
    decode_coding_object ((coding), Qnil, 0, 0, (bytes), (bytes),	\
			  (dst_object));				\
  } while (false)


/* Return the Unicode code point for the given UTF-16 surrogates.  */

INLINE int
surrogates_to_codepoint (int low, int high)
{
  eassert (0 <= low && low <= 0xFFFF);
  eassert (0 <= high && high <= 0xFFFF);
  eassert (UTF_16_LOW_SURROGATE_P (low));
  eassert (UTF_16_HIGH_SURROGATE_P (high));
  return 0x10000 + (low - 0xDC00) + ((high - 0xD800) * 0x400);
}

/* Like build_string, but always returns a multibyte string, and is
   optimized for speed when STR is a UTF-8 encoded text string.  */

INLINE Lisp_Object
build_string_from_utf8 (const char *str)
{
  return make_string_from_utf8 (str, strlen (str));
}


extern Lisp_Object preferred_coding_system (void);

/* Coding system to be used to encode text for terminal display when
   terminal coding system is nil.  */
extern struct coding_system safe_terminal_coding;

extern char emacs_mule_bytes[256];

INLINE_HEADER_END

#endif /* EMACS_CODING_H */
