/* Header for charset handler.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef EMACS_CHARSET_H
#define EMACS_CHARSET_H

/* Index to arguments of Fdefine_charset_internal.  */

enum define_charset_arg_index
  {
    charset_arg_name,
    charset_arg_dimension,
    charset_arg_code_space,
    charset_arg_min_code,
    charset_arg_max_code,
    charset_arg_iso_final,
    charset_arg_iso_revision,
    charset_arg_emacs_mule_id,
    charset_arg_ascii_compatible_p,
    charset_arg_supplementary_p,
    charset_arg_invalid_code,
    charset_arg_code_offset,
    charset_arg_map,
    charset_arg_parents,
    charset_arg_unify_map,
    charset_arg_plist,
    charset_arg_max
  };


/* Indices to charset attributes vector.  */

enum charset_attr_index
  {
    /* ID number of the charset.  */
    charset_id,

    /* Name of the charset (symbol).  */
    charset_name,

    /* Property list of the charset.  */
    charset_plist,

    /* If the method of the charset is `MAP_DEFERRED', the value is a
       mapping vector or a file name that contains mapping vector.
       Otherwise, nil.  */
    charset_map,

    /* If the method of the charset is `MAP', the value is a vector
       that maps code points of the charset to characters.  The vector
       is indexed by a character index.  A character index is
       calculated from a code point and the code-space table of the
       charset.  */
    charset_decoder,

    /* If the method of the charset is `MAP', the value is a
       char-table that maps characters of the charset to code
       points.  */
    charset_encoder,

    /* If the method of the charset is `INHERIT', the value is a list
       of the form (PARENT-CHARSET-ID .  CODE-OFFSET).  */
    charset_parents,

    /* The value is a mapping vector or a file name that contains
       mapping vector.  This provide how characters in the charset
       should be unified with Unicode.  The value of the member
       `charset_deunifier' is created from this information.  */
    charset_unify_map,

    /* If characters in the charset must be unified Unicode, the value
       is a char table that maps a character code in the charset to
       the corresponding Unicode character.  */
    charset_deunifier,

    /* The length of charset attribute vector.  */
    charset_attr_max
  };

/* Methods for converting code points and characters of charsets.  */

enum charset_method
  {
    /* For a charset of this method, a character code is calculated
       from a character index (which is calculated from a code point)
       simply by adding an offset value.  */
    CHARSET_METHOD_OFFSET,

    /* For a charset of this method, a decoder vector and an encoder
       char-table is used for code point <-> character code
       conversion.  */
    CHARSET_METHOD_MAP,

    /* Same as above but decoder and encoder are loaded from a file on
       demand.  Once loaded, the method is changed to
       CHARSET_METHOD_MAP.  */
    CHARSET_METHOD_MAP_DEFERRED,

    /* A charset of this method inherits characters from the other
       charsets.  */
    CHARSET_METHOD_INHERIT
  };

struct charset
{
  /* Index to charset_table.  */
  int id;

  /* Index to Vcharset_hash_table.  */
  int hash_index;

  /* Dimension of the charset: 1, 2, 3, or 4.  */
  int dimension;

  /* Byte code range of each dimension.  <code_space>[4N] is a mininum
     byte code of the (N+1)th dimension, <code_space>[4N+1] is a
     maximum byte code of the (N+1)th dimension, <code_space>[4N+2] is
     (<code_space>[4N+1] - <code_space>[4N] + 1), <code_space>[4N+3]
     is a number of characters containd in the first to (N+1)th
     dismesions.  We get `char-index' of a `code-point' from this
     information.  */
  int code_space[16];

  /* If B is a byte of Nth dimension of a code-point, the (N-1)th bit
     of code_space_mask[B] is set.  This array is used to quickly
     check if a code-point is in a valid range.  */
  unsigned char *code_space_mask;

  /* 1 if there's no gap in code-points.  */
  int code_linear_p;

  /* If the charset is treated as 94-chars in ISO-2022, the value is 0.
     If the charset is treated as 96-chars in ISO-2022, the value is 1.  */
  int iso_chars_96;

  /* ISO final byte of the charset: 48..127.  It may be -1 if the
     charset doesn't conform to ISO-2022.  */
  int iso_final;

  /* ISO revision number of the charset.  */
  int iso_revision;

  /* If the charset is identical to what supported by Emacs 21 and the
     priors, the identification number of the charset used in those
     version.  Otherwise, -1.  */
  int emacs_mule_id;

  /* Nonzero iff the charset is compatible with ASCII.  */
  int ascii_compatible_p;

  /* Nonzero iff the charset is supplementary.  */
  int supplementary_p;

  /* Nonzero iff all the code points are representable by Lisp_Int.  */
  int compact_codes_p;

  /* The method for encoding/decoding characters of the charset.  */
  enum charset_method method;

  /* Mininum and Maximum code points of the charset.  */
  unsigned min_code, max_code;

  /* Offset value used by macros CODE_POINT_TO_INDEX and
      INDEX_TO_CODE_POINT. .  */
  unsigned char_index_offset;

  /* Mininum and Maximum character codes of the charset.  If the
     charset is compatible with ASCII, min_char is a minimum non-ASCII
     character of the charset.  */
  int min_char, max_char;

  /* The code returned by ENCODE_CHAR if a character is not encodable
     by the charset.  */
  unsigned invalid_code;

  /* If the method of the charset is CHARSET_METHOD_MAP, this is a
     table of bits used to quickly and roughly guess if a character
     belongs to the charset.

     The first 64 elements are 512 bits for characters less than
     0x10000.  Each bit corresponds to 128-character block.  The last
     126 elements are 1008 bits for the greater characters
     (0x10000..0x3FFFFF).  Each bit corresponds to 4096-character
     block.

     If a bit is 1, at least one character in the corresponds block is
     in this charset.  */
  unsigned char fast_map[190];

  /* Offset value to calculate a character code from code-point, and
     visa versa.  */
  int code_offset;

  int unified_p;
};

/* Hash table of charset symbols vs. the correponding attribute
   vectors.  */
extern Lisp_Object Vcharset_hash_table;

/* Table of struct charset.  */
extern struct charset *charset_table;
extern int charset_table_used;

#define CHARSET_FROM_ID(id) (charset_table + (id))

extern Lisp_Object Vcharset_list;
extern Lisp_Object Viso_2022_charset_list;
extern Lisp_Object Vemacs_mule_charset_list;

extern struct charset *emacs_mule_charset[256];


/* Macros to access information about charset.  */

/* Return the attribute vector of charset whose symbol is SYMBOL.  */
#define CHARSET_SYMBOL_ATTRIBUTES(symbol)	\
  Fgethash ((symbol), Vcharset_hash_table, Qnil)

#define CHARSET_ATTR_ID(attrs)		AREF ((attrs), charset_id)
#define CHARSET_ATTR_NAME(attrs)	AREF ((attrs), charset_name)
#define CHARSET_ATTR_PLIST(attrs)	AREF ((attrs), charset_plist)
#define CHARSET_ATTR_MAP(attrs)		AREF ((attrs), charset_map)
#define CHARSET_ATTR_DECODER(attrs)	AREF ((attrs), charset_decoder)
#define CHARSET_ATTR_ENCODER(attrs)	AREF ((attrs), charset_encoder)
#define CHARSET_ATTR_PARENTS(attrs)	AREF ((attrs), charset_parents)
#define CHARSET_ATTR_UNIFY_MAP(attrs)	AREF ((attrs), charset_unify_map)
#define CHARSET_ATTR_DEUNIFIER(attrs)	AREF ((attrs), charset_deunifier)

#define CHARSET_SYMBOL_ID(symbol)	\
  CHARSET_ATTR_ID (CHARSET_SYMBOL_ATTRIBUTES (symbol))

/* Return an index to Vcharset_hash_table of the charset whose symbol
   is SYMBOL.  */
#define CHARSET_SYMBOL_HASH_INDEX(symbol)	\
  hash_lookup (XHASH_TABLE (Vcharset_hash_table), symbol, NULL)

/* Return the attribute vector of CHARSET.  */
#define CHARSET_ATTRIBUTES(charset)	\
  (HASH_VALUE (XHASH_TABLE (Vcharset_hash_table), (charset)->hash_index))

#define CHARSET_ID(charset)		((charset)->id)
#define CHARSET_HASH_INDEX(charset)	((charset)->hash_index)
#define CHARSET_DIMENSION(charset)	((charset)->dimension)
#define CHARSET_CODE_SPACE(charset)	((charset)->code_space)
#define CHARSET_CODE_LINEAR_P(charset)	((charset)->code_linear_p)
#define CHARSET_ISO_CHARS_96(charset)	((charset)->iso_chars_96)
#define CHARSET_ISO_FINAL(charset)	((charset)->iso_final)
#define CHARSET_ISO_PLANE(charset)	((charset)->iso_plane)
#define CHARSET_ISO_REVISION(charset)	((charset)->iso_revision)
#define CHARSET_EMACS_MULE_ID(charset)	((charset)->emacs_mule_id)
#define CHARSET_ASCII_COMPATIBLE_P(charset) ((charset)->ascii_compatible_p)
#define CHARSET_COMPACT_CODES_P(charset) ((charset)->compact_codes_p)
#define CHARSET_METHOD(charset)		((charset)->method)
#define CHARSET_MIN_CODE(charset)	((charset)->min_code)
#define CHARSET_MAX_CODE(charset)	((charset)->max_code)
#define CHARSET_INVALID_CODE(charset)	((charset)->invalid_code)
#define CHARSET_MIN_CHAR(charset)	((charset)->min_char)
#define CHARSET_MAX_CHAR(charset)	((charset)->max_char)
#define CHARSET_CODE_OFFSET(charset)	((charset)->code_offset)
#define CHARSET_UNIFIED_P(charset)	((charset)->unified_p)

#define CHARSET_NAME(charset)		\
  (CHARSET_ATTR_NAME (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_MAP(charset)	\
  (CHARSET_ATTR_MAP (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_DECODER(charset)	\
  (CHARSET_ATTR_DECODER (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_ENCODER(charset)	\
  (CHARSET_ATTR_ENCODER (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_PARENTS(charset)	\
  (CHARSET_ATTR_PARENTS (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_UNIFY_MAP(charset)	\
  (CHARSET_ATTR_UNIFY_MAP (CHARSET_ATTRIBUTES (charset)))
#define CHARSET_DEUNIFIER(charset)	\
  (CHARSET_ATTR_DEUNIFIER (CHARSET_ATTRIBUTES (charset)))


/* Nonzero iff OBJ is a valid charset symbol.  */
#define CHARSETP(obj) (CHARSET_SYMBOL_HASH_INDEX (obj) >= 0)

/* Check if X is a valid charset symbol.  If not, signal an error.  */
#define CHECK_CHARSET(x)					\
  do {								\
    if (! SYMBOLP (x) || CHARSET_SYMBOL_HASH_INDEX (x) < 0)	\
      x = wrong_type_argument (Qcharsetp, (x));			\
  } while (0)


/* Check if X is a valid charset symbol.  If valid, set ID to the id
   number of the charset.  Otherwise, signal an error. */
#define CHECK_CHARSET_GET_ID(x, id)					\
  do {									\
    int idx;								\
									\
    if (! SYMBOLP (x) || (idx = CHARSET_SYMBOL_HASH_INDEX (x)) < 0)	\
      x = wrong_type_argument (Qcharsetp, (x));				\
    id = AREF (HASH_VALUE (XHASH_TABLE (Vcharset_hash_table), idx),	\
	       charset_id);						\
  } while (0)


/* Check if X is a valid charset symbol.  If valid, set ATTR to the
   attr vector of the charset.  Otherwise, signal an error. */
#define CHECK_CHARSET_GET_ATTR(x, attr)				\
  do {									\
    if (!SYMBOLP (x) || NILP (attr = CHARSET_SYMBOL_ATTRIBUTES (x)))	\
      x = wrong_type_argument (Qcharsetp, (x));				\
  } while (0)


#define CHECK_CHARSET_GET_CHARSET(x, charset)	\
  do {						\
    int id;					\
    CHECK_CHARSET_GET_ID (x, id);		\
    charset = CHARSET_FROM_ID (id);		\
  } while (0)


/* Lookup Vcharset_order_list and return the first charset that
   contains the character C.  */
#define CHAR_CHARSET(c) \
  char_charset ((c), Qnil, NULL)

#if 0
/* Char-table of charset-sets.  Each element is a bool vector indexed
   by a charset ID.  */
extern Lisp_Object Vchar_charset_set;

/* Charset-bag of character C.  */
#define CHAR_CHARSET_SET(c) \
  CHAR_TABLE_REF (Vchar_charset_set, c)

/* Check if two characters C1 and C2 belong to the same charset.  */
#define SAME_CHARSET_P(c1, c2)	\
  intersection_p (CHAR_CHARSET_SET (c1), CHAR_CHARSET_SET (c2))

#endif


/* Return a character correponding to the code-point CODE of CHARSET.
   Try some optimization before calling decode_char.  */

#define DECODE_CHAR(charset, code)					\
  ((ASCII_BYTE_P (code) && (charset)->ascii_compatible_p)		\
   ? (code)								\
   : ((code) < (charset)->min_code || (code) > (charset)->max_code)	\
   ? -1									\
   : (charset)->unified_p						\
   ? decode_char ((charset), (code))					\
   : (charset)->method == CHARSET_METHOD_OFFSET				\
   ? ((charset)->code_linear_p						\
      ? (code) - (charset)->min_code + (charset)->code_offset		\
      : decode_char ((charset), (code)))				\
   : (charset)->method == CHARSET_METHOD_MAP				\
   ? ((charset)->code_linear_p						\
      ? XINT (AREF (CHARSET_DECODER (charset),				\
			(code) - (charset)->min_code))			\
      : decode_char ((charset), (code)))				\
   : decode_char ((charset), (code)))


/* Return a code point of CHAR in CHARSET.
   Try some optimization before calling encode_char.  */

#define ENCODE_CHAR(charset, c)						\
  ((ASCII_CHAR_P (c) && (charset)->ascii_compatible_p)			\
   ? (c)								\
   : (charset)->unified_p						\
   ? encode_char ((charset), (c))					\
   : ((c) < (charset)->min_char || (c) > (charset)->max_char)		\
   ? (charset)->invalid_code						\
   : (charset)->method == CHARSET_METHOD_OFFSET				\
   ? ((charset)->code_linear_p						\
      ? (c) - (charset)->code_offset + (charset)->min_code		\
      : encode_char ((charset), (c)))					\
   : (charset)->method == CHARSET_METHOD_MAP				\
   ? ((charset)->compact_codes_p					\
      ? XFASTINT (CHAR_TABLE_REF (CHARSET_ENCODER (charset), (c)))	\
      : encode_char ((charset), (c)))					\
   : encode_char ((charset), (c)))


/* Set to 1 when a charset map is loaded to warn that a buffer text
   and a string data may be relocated.  */
extern int charset_map_loaded;


/* Set CHARSET to the charset highest priority of C, CODE to the
   code-point of C in CHARSET.  */
#define SPLIT_CHAR(c, charset, code)	\
  ((charset) = char_charset ((c), Qnil, &(code)))


#define ISO_MAX_DIMENSION 3
#define ISO_MAX_CHARS 2
#define ISO_MAX_FINAL 0x80	/* only 0x30..0xFF are used */

/* Mapping table from ISO2022's charset (specified by DIMENSION,
   CHARS, and FINAL_CHAR) to Emacs' charset ID.  Should be accessed by
   macro ISO_CHARSET_TABLE (DIMENSION, CHARS, FINAL_CHAR).  */
extern int iso_charset_table[ISO_MAX_DIMENSION][ISO_MAX_CHARS][ISO_MAX_FINAL];

/* A charset of type iso2022 who has DIMENSION, CHARS, and FINAL
   (final character).  */
#define ISO_CHARSET_TABLE(dimension, chars_96, final)	\
  iso_charset_table[(dimension) - 1][(chars_96)][(final)]

/* Nonzero iff the charset who has FAST_MAP may contain C.  */
#define CHARSET_FAST_MAP_REF(c, fast_map)		\
  ((c) < 0x10000					\
   ? fast_map[(c) >> 10] & (1 << (((c) >> 7) & 7))	\
   : fast_map[((c) >> 15) + 62] & (1 << (((c) >> 12) & 7)))

#define CHARSET_FAST_MAP_SET(c, fast_map)			\
  do {								\
    if ((c) < 0x10000)						\
      (fast_map)[(c) >> 10] |= 1 << (((c) >> 7) & 7);		\
    else							\
      (fast_map)[((c) >> 15) + 62] |= 1 << (((c) >> 12) & 7);	\
  } while (0)



/* 1 iff CHARSET may contain the character C.  */
#define CHAR_CHARSET_P(c, charset)					    \
  ((ASCII_CHAR_P (c) && (charset)->ascii_compatible_p)			    \
   || (CHARSET_UNIFIED_P (charset)					    \
       ? encode_char ((charset), (c)) != (charset)->invalid_code	    \
       : (CHARSET_FAST_MAP_REF ((c), (charset)->fast_map)		    \
	  && ((charset)->method == CHARSET_METHOD_OFFSET		    \
	      ? (c) >= (charset)->min_char && (c) <= (charset)->max_char    \
	      : ((charset)->method == CHARSET_METHOD_MAP		    \
		 && (charset)->compact_codes_p)				    \
	      ? (XFASTINT (CHAR_TABLE_REF (CHARSET_ENCODER (charset), (c))) \
		 != (charset)->invalid_code)				    \
	      : encode_char ((charset), (c)) != (charset)->invalid_code))))


extern Lisp_Object Qcharsetp;

extern Lisp_Object Qascii, Qunicode;
extern int charset_ascii, charset_8_bit_control, charset_8_bit_graphic;
extern int charset_iso_8859_1;
extern int charset_primary;
extern int charset_jisx0201_roman;
extern int charset_jisx0208_1978;
extern int charset_jisx0208;

extern struct charset *char_charset P_ ((int, Lisp_Object, unsigned *));
extern Lisp_Object charset_attributes P_ ((int));

extern int decode_char P_ ((struct charset *, unsigned));
extern unsigned encode_char P_ ((struct charset *, int));
extern int string_xstring_p P_ ((Lisp_Object));

EXFUN (Funify_charset, 2);

#endif /* EMACS_CHARSET_H */
