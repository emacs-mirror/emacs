/* Basic multilingual character support.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

/* At first, see the document in `charset.h' to understand the code in
   this file.  */

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>

#ifdef emacs

#include <sys/types.h>
#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "composite.h"
#include "coding.h"
#include "disptab.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* emacs */

Lisp_Object Qcharset, Qascii, Qeight_bit_control, Qeight_bit_graphic;
Lisp_Object Qunknown;

/* Declaration of special leading-codes.  */
EMACS_INT leading_code_private_11; /* for private DIMENSION1 of 1-column */
EMACS_INT leading_code_private_12; /* for private DIMENSION1 of 2-column */
EMACS_INT leading_code_private_21; /* for private DIMENSION2 of 1-column */
EMACS_INT leading_code_private_22; /* for private DIMENSION2 of 2-column */

/* Declaration of special charsets.  The values are set by
   Fsetup_special_charsets.  */
int charset_latin_iso8859_1;	/* ISO8859-1 (Latin-1) */
int charset_jisx0208_1978;	/* JISX0208.1978 (Japanese Kanji old set) */
int charset_jisx0208;		/* JISX0208.1983 (Japanese Kanji) */
int charset_katakana_jisx0201;	/* JISX0201.Kana (Japanese Katakana) */
int charset_latin_jisx0201;	/* JISX0201.Roman (Japanese Roman) */
int charset_big5_1;		/* Big5 Level 1 (Chinese Traditional) */
int charset_big5_2;		/* Big5 Level 2 (Chinese Traditional) */

Lisp_Object Qcharset_table;

/* A char-table containing information of each character set.  */
Lisp_Object Vcharset_table;

/* A vector of charset symbol indexed by charset-id.  This is used
   only for returning charset symbol from C functions.  */
Lisp_Object Vcharset_symbol_table;

/* A list of charset symbols ever defined.  */
Lisp_Object Vcharset_list;

/* Vector of translation table ever defined.
   ID of a translation table is used to index this vector.  */
Lisp_Object Vtranslation_table_vector;

/* A char-table for characters which may invoke auto-filling.  */
Lisp_Object Vauto_fill_chars;

Lisp_Object Qauto_fill_chars;

/* Tables used by macros BYTES_BY_CHAR_HEAD and WIDTH_BY_CHAR_HEAD.  */
int bytes_by_char_head[256];
int width_by_char_head[256];

/* Mapping table from ISO2022's charset (specified by DIMENSION,
   CHARS, and FINAL-CHAR) to Emacs' charset.  */
int iso_charset_table[2][2][128];

/* Variables used locally in the macro FETCH_MULTIBYTE_CHAR.  */
unsigned char *_fetch_multibyte_char_p;
int _fetch_multibyte_char_len;

/* Offset to add to a non-ASCII value when inserting it.  */
EMACS_INT nonascii_insert_offset;

/* Translation table for converting non-ASCII unibyte characters
   to multibyte codes, or nil.  */
Lisp_Object Vnonascii_translation_table;

/* List of all possible generic characters.  */
Lisp_Object Vgeneric_character_list;


void
invalid_character (c)
     int c;
{
  error ("Invalid character: 0%o, %d, 0x%x", c, c, c);
}

/* Parse string STR of length LENGTH and fetch information of a
   character at STR.  Set BYTES to the byte length the character
   occupies, CHARSET, C1, C2 to proper values of the character. */

#define SPLIT_MULTIBYTE_SEQ(str, length, bytes, charset, c1, c2)	     \
  do {									     \
    (c1) = *(str);							     \
    (bytes) = BYTES_BY_CHAR_HEAD (c1);					     \
    if ((bytes) == 1)							     \
      (charset) = ASCII_BYTE_P (c1) ? CHARSET_ASCII : CHARSET_8_BIT_GRAPHIC; \
    else if ((bytes) == 2)						     \
      {									     \
	if ((c1) == LEADING_CODE_8_BIT_CONTROL)				     \
	  (charset) = CHARSET_8_BIT_CONTROL, (c1) = (str)[1] - 0x20;	     \
	else								     \
	  (charset) = (c1), (c1) = (str)[1] & 0x7F;			     \
      }									     \
    else if ((bytes) == 3)						     \
      {									     \
	if ((c1) < LEADING_CODE_PRIVATE_11)				     \
	  (charset) = (c1), (c1) = (str)[1] & 0x7F, (c2) = (str)[2] & 0x7F;  \
	else								     \
	  (charset) = (str)[1], (c1) = (str)[2] & 0x7F;			     \
      }									     \
    else								     \
      (charset) = (str)[1], (c1) = (str)[2] & 0x7F, (c2) = (str)[3] & 0x7F;  \
  } while (0)

/* 1 if CHARSET, C1, and C2 compose a valid character, else 0.
   Note that this intentionally allows invalid components, such
   as 0xA0 0xA0, because there exist many files that contain
   such invalid byte sequences, especially in EUC-GB. */
#define CHAR_COMPONENTS_VALID_P(charset, c1, c2)	\
  ((charset) == CHARSET_ASCII				\
   ? ((c1) >= 0 && (c1) <= 0x7F)			\
   : ((charset) == CHARSET_8_BIT_CONTROL		\
      ? ((c1) >= 0x80 && (c1) <= 0x9F)			\
      : ((charset) == CHARSET_8_BIT_GRAPHIC		\
	 ? ((c1) >= 0x80 && (c1) <= 0xFF)		\
	 : (CHARSET_DIMENSION (charset) == 1		\
	    ? ((c1) >= 0x20 && (c1) <= 0x7F)		\
	    : ((c1) >= 0x20 && (c1) <= 0x7F		\
	       && (c2) >= 0x20 && (c2) <= 0x7F)))))

/* Store multi-byte form of the character C in STR.  The caller should
   allocate at least 4-byte area at STR in advance.  Returns the
   length of the multi-byte form.  If C is an invalid character code,
   return -1.  */

int
char_to_string_1 (c, str)
     int c;
     unsigned char *str;
{
  unsigned char *p = str;

  if (c & CHAR_MODIFIER_MASK)	/* This includes the case C is negative.  */
    {
      /* Multibyte character can't have a modifier bit.  */
      if (! SINGLE_BYTE_CHAR_P ((c & ~CHAR_MODIFIER_MASK)))
	return -1;

      /* For Meta, Shift, and Control modifiers, we need special care.  */
      if (c & CHAR_META)
	{
	  /* Move the meta bit to the right place for a string.  */
	  c = (c & ~CHAR_META) | 0x80;
	}
      if (c & CHAR_SHIFT)
	{
	  /* Shift modifier is valid only with [A-Za-z].  */
	  if ((c & 0377) >= 'A' && (c & 0377) <= 'Z')
	    c &= ~CHAR_SHIFT;
	  else if ((c & 0377) >= 'a' && (c & 0377) <= 'z')
	    c = (c & ~CHAR_SHIFT) - ('a' - 'A');
	}
      if (c & CHAR_CTL)
	{
	  /* Simulate the code in lread.c.  */
	  /* Allow `\C- ' and `\C-?'.  */
	  if (c == (CHAR_CTL | ' '))
	    c = 0;
	  else if (c == (CHAR_CTL | '?'))
	    c = 127;
	  /* ASCII control chars are made from letters (both cases),
	     as well as the non-letters within 0100...0137.  */
	  else if ((c & 0137) >= 0101 && (c & 0137) <= 0132)
	    c &= (037 | (~0177 & ~CHAR_CTL));
	  else if ((c & 0177) >= 0100 && (c & 0177) <= 0137)
	    c &= (037 | (~0177 & ~CHAR_CTL));
	}

      /* If C still has any modifier bits, just ignore it.  */
      c &= ~CHAR_MODIFIER_MASK;
    }
  
  if (SINGLE_BYTE_CHAR_P (c))
    {
      if (ASCII_BYTE_P (c) || c >= 0xA0)
	*p++ = c;
      else
	{
	  *p++ = LEADING_CODE_8_BIT_CONTROL;
	  *p++ = c + 0x20;
	}
    }
  else if (CHAR_VALID_P (c, 0))
    {
      int charset, c1, c2;

      SPLIT_CHAR (c, charset, c1, c2);

      if (charset >= LEADING_CODE_EXT_11)
	*p++ = (charset < LEADING_CODE_EXT_12
		? LEADING_CODE_PRIVATE_11
		: (charset < LEADING_CODE_EXT_21
		   ? LEADING_CODE_PRIVATE_12
		   : (charset < LEADING_CODE_EXT_22
		      ? LEADING_CODE_PRIVATE_21
		      : LEADING_CODE_PRIVATE_22)));
      *p++ = charset;
      if ((c1 > 0 && c1 < 32) || (c2 > 0 && c2 < 32))
	return -1;
      if (c1)
	{
	  *p++ = c1 | 0x80;
	  if (c2 > 0)
	    *p++ = c2 | 0x80;
	}
    }
  else
    return -1;

  return (p - str);
}


/* Store multi-byte form of the character C in STR.  The caller should
   allocate at least 4-byte area at STR in advance.  Returns the
   length of the multi-byte form.  If C is an invalid character code,
   signal an error.

   Use macro `CHAR_STRING (C, STR)' instead of calling this function
   directly if C can be an ASCII character.  */

int
char_to_string (c, str)
     int c;
     unsigned char *str;
{
  int len;
  len = char_to_string_1 (c, str);
  if (len == -1)
    invalid_character (c);
  return len;
}


/* Return the non-ASCII character corresponding to multi-byte form at
   STR of length LEN.  If ACTUAL_LEN is not NULL, store the byte
   length of the multibyte form in *ACTUAL_LEN.

   Use macros STRING_CHAR or STRING_CHAR_AND_LENGTH instead of calling
   this function directly if you want ot handle ASCII characters as
   well.  */

int
string_to_char (str, len, actual_len)
     const unsigned char *str;
     int len, *actual_len;
{
  int c, bytes, charset, c1, c2;

  SPLIT_MULTIBYTE_SEQ (str, len, bytes, charset, c1, c2);
  c = MAKE_CHAR (charset, c1, c2);
  if (actual_len)
    *actual_len = bytes;
  return c;
}

/* Return the length of the multi-byte form at string STR of length LEN.
   Use the macro MULTIBYTE_FORM_LENGTH instead.  */
int
multibyte_form_length (str, len)
     const unsigned char *str;
     int len;
{
  int bytes;

  PARSE_MULTIBYTE_SEQ (str, len, bytes);
  return bytes;
}

/* Check multibyte form at string STR of length LEN and set variables
   pointed by CHARSET, C1, and C2 to charset and position codes of the
   character at STR, and return 0.  If there's no multibyte character,
   return -1.  This should be used only in the macro SPLIT_STRING
   which checks range of STR in advance.  */

int
split_string (str, len, charset, c1, c2)
     const unsigned char *str;
     unsigned char *c1, *c2;
     int len, *charset;
{
  register int bytes, cs, code1, code2 = -1;

  SPLIT_MULTIBYTE_SEQ (str, len, bytes, cs, code1, code2);
  if (cs == CHARSET_ASCII)
    return -1;
  *charset = cs;
  *c1 = code1;
  *c2 = code2;
  return 0;
}

/* Return 1 iff character C has valid printable glyph.
   Use the macro CHAR_PRINTABLE_P instead.  */
int
char_printable_p (c)
     int c;
{
  int charset, c1, c2;

  if (ASCII_BYTE_P (c))
    return 1;
  else if (SINGLE_BYTE_CHAR_P (c))
    return 0;
  else if (c >= MAX_CHAR)
    return 0;
  
  SPLIT_CHAR (c, charset, c1, c2);
  if (! CHARSET_DEFINED_P (charset))
    return 0;
  if (CHARSET_CHARS (charset) == 94
      ? c1 <= 32 || c1 >= 127
      : c1 < 32)
    return 0;
  if (CHARSET_DIMENSION (charset) == 2
      && (CHARSET_CHARS (charset) == 94
	  ? c2 <= 32 || c2 >= 127
	  : c2 < 32))
    return 0;
  return 1;
}

/* Translate character C by translation table TABLE.  If C
   is negative, translate a character specified by CHARSET, C1, and C2
   (C1 and C2 are code points of the character).  If no translation is
   found in TABLE, return C.  */
int
translate_char (table, c, charset, c1, c2)
     Lisp_Object table;
     int c, charset, c1, c2;
{
  Lisp_Object ch;
  int alt_charset, alt_c1, alt_c2, dimension;

  if (c < 0) c = MAKE_CHAR (charset, (c1 & 0x7F) , (c2 & 0x7F));
  if (!CHAR_TABLE_P (table)
      || (ch = Faref (table, make_number (c)), !NATNUMP (ch)))
    return c;

  SPLIT_CHAR (XFASTINT (ch), alt_charset, alt_c1, alt_c2);
  dimension = CHARSET_DIMENSION (alt_charset);
  if ((dimension == 1 && alt_c1 > 0) || (dimension == 2 && alt_c2 > 0))
    /* CH is not a generic character, just return it.  */
    return XFASTINT (ch);

  /* Since CH is a generic character, we must return a specific
     charater which has the same position codes as C from CH.  */
  if (charset < 0)
    SPLIT_CHAR (c, charset, c1, c2);
  if (dimension != CHARSET_DIMENSION (charset))
    /* We can't make such a character because of dimension mismatch.  */
    return c;
  return MAKE_CHAR (alt_charset, c1, c2);
}

/* Convert the unibyte character C to multibyte based on
   Vnonascii_translation_table or nonascii_insert_offset.  If they can't
   convert C to a valid multibyte character, convert it based on
   DEFAULT_NONASCII_INSERT_OFFSET which makes C a Latin-1 character.  */

int
unibyte_char_to_multibyte (c)
     int c;
{
  if (c < 0400 && c >= 0200)
    {
      int c_save = c;

      if (! NILP (Vnonascii_translation_table))
	{
	  c = XINT (Faref (Vnonascii_translation_table, make_number (c)));
	  if (c >= 0400 && ! char_valid_p (c, 0))
	    c = c_save + DEFAULT_NONASCII_INSERT_OFFSET;
	}
      else if (c >= 0240 && nonascii_insert_offset > 0)
	{
	  c += nonascii_insert_offset;
	  if (c < 0400 || ! char_valid_p (c, 0))
	    c = c_save + DEFAULT_NONASCII_INSERT_OFFSET;
	}
      else if (c >= 0240)
	c = c_save + DEFAULT_NONASCII_INSERT_OFFSET;
    }
  return c;
}


/* Convert the multibyte character C to unibyte 8-bit character based
   on Vnonascii_translation_table or nonascii_insert_offset.  If
   REV_TBL is non-nil, it should be a reverse table of
   Vnonascii_translation_table, i.e. what given by:
     Fchar_table_extra_slot (Vnonascii_translation_table, make_number (0))  */

int
multibyte_char_to_unibyte (c, rev_tbl)
     int c;
     Lisp_Object rev_tbl;
{
  if (!SINGLE_BYTE_CHAR_P (c))
    {
      int c_save = c;

      if (! CHAR_TABLE_P (rev_tbl)
	  && CHAR_TABLE_P (Vnonascii_translation_table))
	rev_tbl = Fchar_table_extra_slot (Vnonascii_translation_table,
					  make_number (0));
      if (CHAR_TABLE_P (rev_tbl))
	{
	  Lisp_Object temp;
	  temp = Faref (rev_tbl, make_number (c));
	  if (INTEGERP (temp))
	    c = XINT (temp);
	  if (c >= 256)
	    c = (c_save & 0177) + 0200;
	}
      else
	{
	  if (nonascii_insert_offset > 0)
	    c -= nonascii_insert_offset;
	  if (c < 128 || c >= 256)
	    c = (c_save & 0177) + 0200;
	}
    }

  return c;
}


/* Update the table Vcharset_table with the given arguments (see the
   document of `define-charset' for the meaning of each argument).
   Several other table contents are also updated.  The caller should
   check the validity of CHARSET-ID and the remaining arguments in
   advance.  */

void
update_charset_table (charset_id, dimension, chars, width, direction,
		      iso_final_char, iso_graphic_plane,
		      short_name, long_name, description)
     Lisp_Object charset_id, dimension, chars, width, direction;
     Lisp_Object iso_final_char, iso_graphic_plane;
     Lisp_Object short_name, long_name, description;
{
  int charset = XINT (charset_id);
  int bytes;
  unsigned char leading_code_base, leading_code_ext;

  if (NILP (CHARSET_TABLE_ENTRY (charset)))
    CHARSET_TABLE_ENTRY (charset)
      = Fmake_vector (make_number (CHARSET_MAX_IDX), Qnil);

  if (NILP (long_name))
    long_name = short_name;
  if (NILP (description))
    description = long_name;

  /* Get byte length of multibyte form, base leading-code, and
     extended leading-code of the charset.  See the comment under the
     title "GENERAL NOTE on CHARACTER SET (CHARSET)" in charset.h.  */
  bytes = XINT (dimension);
  if (charset < MIN_CHARSET_PRIVATE_DIMENSION1)
    {
      /* Official charset, it doesn't have an extended leading-code.  */
      if (charset != CHARSET_ASCII && charset != CHARSET_8_BIT_GRAPHIC)
	bytes += 1; /* For a base leading-code.  */
      leading_code_base = charset;
      leading_code_ext = 0;
    }
  else
    {
      /* Private charset.  */
      bytes += 2; /* For base and extended leading-codes.  */
      leading_code_base
	= (charset < LEADING_CODE_EXT_12
	   ? LEADING_CODE_PRIVATE_11
	   : (charset < LEADING_CODE_EXT_21
	      ? LEADING_CODE_PRIVATE_12
	      : (charset < LEADING_CODE_EXT_22
		 ? LEADING_CODE_PRIVATE_21
		 : LEADING_CODE_PRIVATE_22)));
      leading_code_ext = charset;
      if (BYTES_BY_CHAR_HEAD (leading_code_base) != bytes)
	error ("Invalid dimension for the charset-ID %d", charset);
    }

  CHARSET_TABLE_INFO (charset, CHARSET_ID_IDX) = charset_id;
  CHARSET_TABLE_INFO (charset, CHARSET_BYTES_IDX) = make_number (bytes);
  CHARSET_TABLE_INFO (charset, CHARSET_DIMENSION_IDX) = dimension;
  CHARSET_TABLE_INFO (charset, CHARSET_CHARS_IDX) = chars;
  CHARSET_TABLE_INFO (charset, CHARSET_WIDTH_IDX) = width;
  CHARSET_TABLE_INFO (charset, CHARSET_DIRECTION_IDX) = direction;
  CHARSET_TABLE_INFO (charset, CHARSET_LEADING_CODE_BASE_IDX)
    = make_number (leading_code_base);
  CHARSET_TABLE_INFO (charset, CHARSET_LEADING_CODE_EXT_IDX)
    = make_number (leading_code_ext);
  CHARSET_TABLE_INFO (charset, CHARSET_ISO_FINAL_CHAR_IDX) = iso_final_char;
  CHARSET_TABLE_INFO (charset, CHARSET_ISO_GRAPHIC_PLANE_IDX)
    = iso_graphic_plane;
  CHARSET_TABLE_INFO (charset, CHARSET_SHORT_NAME_IDX) = short_name;
  CHARSET_TABLE_INFO (charset, CHARSET_LONG_NAME_IDX) = long_name;
  CHARSET_TABLE_INFO (charset, CHARSET_DESCRIPTION_IDX) = description;
  CHARSET_TABLE_INFO (charset, CHARSET_PLIST_IDX) = Qnil;

  {
    /* If we have already defined a charset which has the same
       DIMENSION, CHARS and ISO-FINAL-CHAR but the different
       DIRECTION, we must update the entry REVERSE-CHARSET of both
       charsets.  If there's no such charset, the value of the entry
       is set to nil.  */
    int i;

    for (i = 0; i <= MAX_CHARSET; i++)
      if (!NILP (CHARSET_TABLE_ENTRY (i)))
	{
	  if (CHARSET_DIMENSION (i) == XINT (dimension)
	      && CHARSET_CHARS (i) == XINT (chars)
	      && CHARSET_ISO_FINAL_CHAR (i) == XINT (iso_final_char)
	      && CHARSET_DIRECTION (i) != XINT (direction))
	    {
	      CHARSET_TABLE_INFO (charset, CHARSET_REVERSE_CHARSET_IDX)
		= make_number (i);
	      CHARSET_TABLE_INFO (i, CHARSET_REVERSE_CHARSET_IDX) = charset_id;
	      break;
	    }
	}
    if (i > MAX_CHARSET)
      /* No such a charset.  */
      CHARSET_TABLE_INFO (charset, CHARSET_REVERSE_CHARSET_IDX)
	= make_number (-1);
  }

  if (charset != CHARSET_ASCII && charset != CHARSET_8_BIT_GRAPHIC
      && charset < MIN_CHARSET_PRIVATE_DIMENSION1)
    {
      bytes_by_char_head[leading_code_base] = bytes;
      width_by_char_head[leading_code_base] = XINT (width);

      /* Update table emacs_code_class.  */
      emacs_code_class[charset] = (bytes == 2
				   ? EMACS_leading_code_2
				   : (bytes == 3
				      ? EMACS_leading_code_3
				      : EMACS_leading_code_4));
    }

  /* Update table iso_charset_table.  */
  if (XINT (iso_final_char) >= 0
      && ISO_CHARSET_TABLE (dimension, chars, iso_final_char) < 0)
    ISO_CHARSET_TABLE (dimension, chars, iso_final_char) = charset;
}

#ifdef emacs

/* Return charset id of CHARSET_SYMBOL, or return -1 if CHARSET_SYMBOL
   is invalid.  */
int
get_charset_id (charset_symbol)
     Lisp_Object charset_symbol;
{
  Lisp_Object val;
  int charset;

  /* This originally used a ?: operator, but reportedly the HP-UX
     compiler version HP92453-01 A.10.32.22 miscompiles that.  */
  if (SYMBOLP (charset_symbol)
      && VECTORP (val = Fget (charset_symbol, Qcharset))
      && CHARSET_VALID_P (charset =
			  XINT (XVECTOR (val)->contents[CHARSET_ID_IDX])))
    return charset;
  else
    return -1;
}

/* Return an identification number for a new private charset of
   DIMENSION and WIDTH.  If there's no more room for the new charset,
   return 0.  */
Lisp_Object
get_new_private_charset_id (dimension, width)
     int dimension, width;
{
  int charset, from, to;

  if (dimension == 1)
    {
      from = LEADING_CODE_EXT_11;
      to = LEADING_CODE_EXT_21;
    }
  else
    {
      from = LEADING_CODE_EXT_21;
      to = LEADING_CODE_EXT_MAX + 1;
    }

  for (charset = from; charset < to; charset++)
    if (!CHARSET_DEFINED_P (charset)) break;

  return make_number (charset < to ? charset : 0);
}

DEFUN ("define-charset", Fdefine_charset, Sdefine_charset, 3, 3, 0,
       doc: /* Define CHARSET-ID as the identification number of CHARSET with INFO-VECTOR.
If CHARSET-ID is nil, it is decided automatically, which means CHARSET is
 treated as a private charset.
INFO-VECTOR is a vector of the format:
   [DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
    SHORT-NAME LONG-NAME DESCRIPTION]
The meanings of each elements is as follows:
DIMENSION (integer) is the number of bytes to represent a character: 1 or 2.
CHARS (integer) is the number of characters in a dimension: 94 or 96.
WIDTH (integer) is the number of columns a character in the charset
occupies on the screen: one of 0, 1, and 2.

DIRECTION (integer) is the rendering direction of characters in the
charset when rendering.  If 0, render from left to right, else
render from right to left.

ISO-FINAL-CHAR (character) is the final character of the
corresponding ISO 2022 charset.
It may be -1 if the charset is internal use only.

ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked
while encoding to variants of ISO 2022 coding system, one of the
following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).
It may be -1 if the charset is internal use only.

SHORT-NAME (string) is the short name to refer to the charset.

LONG-NAME (string) is the long name to refer to the charset.

DESCRIPTION (string) is the description string of the charset.  */)
       (charset_id, charset_symbol, info_vector)
     Lisp_Object charset_id, charset_symbol, info_vector;
{
  Lisp_Object *vec;

  if (!NILP (charset_id))
    CHECK_NUMBER (charset_id);
  CHECK_SYMBOL (charset_symbol);
  CHECK_VECTOR (info_vector);

  if (! NILP (charset_id))
    {
      if (! CHARSET_VALID_P (XINT (charset_id)))
	error ("Invalid CHARSET: %d", XINT (charset_id));
      else if (CHARSET_DEFINED_P (XINT (charset_id)))
	error ("Already defined charset: %d", XINT (charset_id));
    }

  vec = XVECTOR (info_vector)->contents;
  if (XVECTOR (info_vector)->size != 9
      || !INTEGERP (vec[0]) || !(XINT (vec[0]) == 1 || XINT (vec[0]) == 2)
      || !INTEGERP (vec[1]) || !(XINT (vec[1]) == 94 || XINT (vec[1]) == 96)
      || !INTEGERP (vec[2]) || !(XINT (vec[2]) == 1 || XINT (vec[2]) == 2)
      || !INTEGERP (vec[3]) || !(XINT (vec[3]) == 0 || XINT (vec[3]) == 1)
      || !INTEGERP (vec[4])
      || !(XINT (vec[4]) == -1 || (XINT (vec[4]) >= '0' && XINT (vec[4]) <= '~'))
      || !INTEGERP (vec[5])
      || !(XINT (vec[5]) == -1 || XINT (vec[5]) == 0 || XINT (vec[5]) == 1)
      || !STRINGP (vec[6])
      || !STRINGP (vec[7])
      || !STRINGP (vec[8]))
    error ("Invalid info-vector argument for defining charset %s",
	   XSTRING (SYMBOL_NAME (charset_symbol))->data);

  if (NILP (charset_id))
    {
      charset_id = get_new_private_charset_id (XINT (vec[0]), XINT (vec[2]));
      if (XINT (charset_id) == 0)
	error ("There's no room for a new private charset %s",
	       XSTRING (SYMBOL_NAME (charset_symbol))->data);
    }

  update_charset_table (charset_id, vec[0], vec[1], vec[2], vec[3],
			vec[4], vec[5], vec[6], vec[7], vec[8]);
  Fput (charset_symbol, Qcharset, CHARSET_TABLE_ENTRY (XINT (charset_id)));
  CHARSET_SYMBOL (XINT (charset_id)) = charset_symbol;
  Vcharset_list = Fcons (charset_symbol, Vcharset_list);
  Fupdate_coding_systems_internal ();
  return Qnil;
}

DEFUN ("generic-character-list", Fgeneric_character_list,
       Sgeneric_character_list, 0, 0, 0,
       doc: /* Return a list of all possible generic characters.
It includes a generic character for a charset not yet defined.  */)
     ()
{
  return Vgeneric_character_list;
}

DEFUN ("get-unused-iso-final-char", Fget_unused_iso_final_char,
       Sget_unused_iso_final_char, 2, 2, 0,
       doc: /* Return an unsed ISO's final char for a charset of DIMENISION and CHARS.
DIMENSION is the number of bytes to represent a character: 1 or 2.
CHARS is the number of characters in a dimension: 94 or 96.

This final char is for private use, thus the range is `0' (48) .. `?' (63).
If there's no unused final char for the specified kind of charset,
return nil.  */)
     (dimension, chars)
     Lisp_Object dimension, chars;
{
  int final_char;

  CHECK_NUMBER (dimension);
  CHECK_NUMBER (chars);
  if (XINT (dimension) != 1 && XINT (dimension) != 2)
    error ("Invalid charset dimension %d, it should be 1 or 2",
	   XINT (dimension));
  if (XINT (chars) != 94 && XINT (chars) != 96)
    error ("Invalid charset chars %d, it should be 94 or 96",
	   XINT (chars));
  for (final_char = '0'; final_char <= '?'; final_char++)
    {
      if (ISO_CHARSET_TABLE (dimension, chars, make_number (final_char)) < 0)
	break;
    }
  return (final_char <= '?' ? make_number (final_char) : Qnil);
}

DEFUN ("declare-equiv-charset", Fdeclare_equiv_charset, Sdeclare_equiv_charset,
       4, 4, 0,
       doc: /* Declare a charset of DIMENSION, CHARS, FINAL-CHAR is the same as CHARSET.
CHARSET should be defined by `defined-charset' in advance.  */)
     (dimension, chars, final_char, charset_symbol)
     Lisp_Object dimension, chars, final_char, charset_symbol;
{
  int charset;

  CHECK_NUMBER (dimension);
  CHECK_NUMBER (chars);
  CHECK_NUMBER (final_char);
  CHECK_SYMBOL (charset_symbol);

  if (XINT (dimension) != 1 && XINT (dimension) != 2)
    error ("Invalid DIMENSION %d, it should be 1 or 2", XINT (dimension));
  if (XINT (chars) != 94 && XINT (chars) != 96)
    error ("Invalid CHARS %d, it should be 94 or 96", XINT (chars));
  if (XINT (final_char) < '0' || XFASTINT (final_char) > '~')
    error ("Invalid FINAL-CHAR %c, it should be `0'..`~'", XINT (chars));
  if ((charset = get_charset_id (charset_symbol)) < 0)
    error ("Invalid charset %s", XSTRING (SYMBOL_NAME (charset_symbol))->data);

  ISO_CHARSET_TABLE (dimension, chars, final_char) = charset;
  return Qnil;
}

/* Return information about charsets in the text at PTR of NBYTES
   bytes, which are NCHARS characters.  The value is:

	0: Each character is represented by one byte.  This is always
	   true for unibyte text.
	1: No charsets other than ascii eight-bit-control,
	   eight-bit-graphic, and latin-1 are found.
	2: Otherwise.

   In addition, if CHARSETS is nonzero, for each found charset N, set
   CHARSETS[N] to 1.  For that, callers should allocate CHARSETS
   (MAX_CHARSET + 1 elements) in advance.  It may lookup a translation
   table TABLE if supplied.  For invalid charsets, set CHARSETS[1] to
   1 (note that there's no charset whose ID is 1).  */

int
find_charset_in_text (ptr, nchars, nbytes, charsets, table)
     unsigned char *ptr;
     int nchars, nbytes, *charsets;
     Lisp_Object table;
{
  if (nchars == nbytes)
    {
      if (charsets && nbytes > 0)
	{
	  unsigned char *endp = ptr + nbytes;
	  int maskbits = 0;

	  while (ptr < endp && maskbits != 7)
	    {
	      maskbits |= (*ptr < 0x80 ? 1 : *ptr < 0xA0 ? 2 : 4);
	      ptr++;
	    }	      

	  if (maskbits & 1)
	    charsets[CHARSET_ASCII] = 1;
	  if (maskbits & 2)
	    charsets[CHARSET_8_BIT_CONTROL] = 1;
	  if (maskbits & 4)
	    charsets[CHARSET_8_BIT_GRAPHIC] = 1;
	}
      return 0;
    }
  else
    {
      int return_val = 1;
      int bytes, charset, c1, c2;

      if (! CHAR_TABLE_P (table))
	table = Qnil;

      while (nchars-- > 0)
	{
	  SPLIT_MULTIBYTE_SEQ (ptr, len, bytes, charset, c1, c2);
	  ptr += bytes;

	  if (!CHARSET_DEFINED_P (charset))
	    charset = 1;
	  else if (! NILP (table))
	    {
	      int c = translate_char (table, -1, charset, c1, c2);
	      if (c >= 0)
		charset = CHAR_CHARSET (c);
	    }

	  if (return_val == 1
	      && charset != CHARSET_ASCII
	      && charset != CHARSET_8_BIT_CONTROL
	      && charset != CHARSET_8_BIT_GRAPHIC
	      && charset != charset_latin_iso8859_1)
	    return_val = 2;

	  if (charsets)
	    charsets[charset] = 1;
	  else if (return_val == 2)
	    break;
	}
      return return_val;
    }
}

DEFUN ("find-charset-region", Ffind_charset_region, Sfind_charset_region,
       2, 3, 0,
       doc: /* Return a list of charsets in the region between BEG and END.
BEG and END are buffer positions.
Optional arg TABLE if non-nil is a translation table to look up.

If the region contains invalid multibyte characters,
`unknown' is included in the returned list.

If the current buffer is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'.  */)
     (beg, end, table)
     Lisp_Object beg, end, table;
{
  int charsets[MAX_CHARSET + 1];
  int from, from_byte, to, stop, stop_byte, i;
  Lisp_Object val;

  validate_region (&beg, &end);
  from = XFASTINT (beg);
  stop = to = XFASTINT (end);

  if (from < GPT && GPT < to)
    {
      stop = GPT;
      stop_byte = GPT_BYTE;
    }
  else
    stop_byte = CHAR_TO_BYTE (stop);

  from_byte = CHAR_TO_BYTE (from);

  bzero (charsets, (MAX_CHARSET + 1) * sizeof (int));
  while (1)
    {
      find_charset_in_text (BYTE_POS_ADDR (from_byte), stop - from,
			    stop_byte - from_byte, charsets, table);
      if (stop < to)
	{
	  from = stop, from_byte = stop_byte;
	  stop = to, stop_byte = CHAR_TO_BYTE (stop);
	}
      else
	break;
    }

  val = Qnil;
  if (charsets[1])
    val = Fcons (Qunknown, val);
  for (i = MAX_CHARSET; i >= MIN_CHARSET_OFFICIAL_DIMENSION1; i--)
    if (charsets[i])
      val = Fcons (CHARSET_SYMBOL (i), val);
  if (charsets[0])
    val = Fcons (Qascii, val);
  return val;
}

DEFUN ("find-charset-string", Ffind_charset_string, Sfind_charset_string,
       1, 2, 0,
       doc: /* Return a list of charsets in STR.
Optional arg TABLE if non-nil is a translation table to look up.

If the string contains invalid multibyte characters,
`unknown' is included in the returned list.

If STR is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'.  */)
     (str, table)
     Lisp_Object str, table;
{
  int charsets[MAX_CHARSET + 1];
  int i;
  Lisp_Object val;

  CHECK_STRING (str);

  bzero (charsets, (MAX_CHARSET + 1) * sizeof (int));
  find_charset_in_text (XSTRING (str)->data, XSTRING (str)->size,
			STRING_BYTES (XSTRING (str)), charsets, table);

  val = Qnil;
  if (charsets[1])
    val = Fcons (Qunknown, val);
  for (i = MAX_CHARSET; i >= MIN_CHARSET_OFFICIAL_DIMENSION1; i--)
    if (charsets[i])
      val = Fcons (CHARSET_SYMBOL (i), val);
  if (charsets[0])
    val = Fcons (Qascii, val);
  return val;
}


DEFUN ("make-char-internal", Fmake_char_internal, Smake_char_internal, 1, 3, 0,
       doc: /* Return a character made from arguments.
Internal use only.  */)
     (charset, code1, code2)
     Lisp_Object charset, code1, code2;
{
  int charset_id, c1, c2;

  CHECK_NUMBER (charset);
  charset_id = XINT (charset);
  if (!CHARSET_DEFINED_P (charset_id))
    error ("Invalid charset ID: %d", XINT (charset));

  if (NILP (code1))
    c1 = 0;
  else
    {
      CHECK_NUMBER (code1);
      c1 = XINT (code1);
    }
  if (NILP (code2))
    c2 = 0;
  else
    {
      CHECK_NUMBER (code2);
      c2 = XINT (code2);
    }

  if (charset_id == CHARSET_ASCII)
    {
      if (c1 < 0 || c1 > 0x7F)
	goto invalid_code_posints;
      return make_number (c1);
    }
  else if (charset_id == CHARSET_8_BIT_CONTROL)
    {
      if (NILP (code1))
	c1 = 0x80;
      else if (c1 < 0x80 || c1 > 0x9F)
	goto invalid_code_posints;
      return make_number (c1);
    }
  else if (charset_id == CHARSET_8_BIT_GRAPHIC)
    {
      if (NILP (code1))
	c1 = 0xA0;
      else if (c1 < 0xA0 || c1 > 0xFF)
	goto invalid_code_posints;
      return make_number (c1);
    }
  else if (c1 < 0 || c1 > 0xFF || c2 < 0 || c2 > 0xFF)
    goto invalid_code_posints;
  c1 &= 0x7F;
  c2 &= 0x7F;
  if (c1 == 0
      ? c2 != 0
      : (c2 == 0
	 ? !CHAR_COMPONENTS_VALID_P (charset_id, c1, 0x20)
	 : !CHAR_COMPONENTS_VALID_P (charset_id, c1, c2)))
    goto invalid_code_posints;
  return make_number (MAKE_CHAR (charset_id, c1, c2));

 invalid_code_posints:
  error ("Invalid code points for charset ID %d: %d %d", charset_id, c1, c2);
}

DEFUN ("split-char", Fsplit_char, Ssplit_char, 1, 1, 0,
       doc: /* Return list of charset and one or two position-codes of CHAR.
If CHAR is invalid as a character code,
return a list of symbol `unknown' and CHAR.  */)
     (ch)
     Lisp_Object ch;
{
  int c, charset, c1, c2;

  CHECK_NUMBER (ch);
  c = XFASTINT (ch);
  if (!CHAR_VALID_P (c, 1))
    return Fcons (Qunknown, Fcons (ch, Qnil));
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  return (c2 >= 0
	  ? Fcons (CHARSET_SYMBOL (charset),
		   Fcons (make_number (c1), Fcons (make_number (c2), Qnil)))
	  : Fcons (CHARSET_SYMBOL (charset), Fcons (make_number (c1), Qnil)));
}

DEFUN ("char-charset", Fchar_charset, Schar_charset, 1, 1, 0,
       doc: /* Return charset of CHAR.  */)
     (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch);

  return CHARSET_SYMBOL (CHAR_CHARSET (XINT (ch)));
}

DEFUN ("charset-after", Fcharset_after, Scharset_after, 0, 1, 0,
       doc: /* Return charset of a character in the current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil.  */)
     (pos)
     Lisp_Object pos;
{
  Lisp_Object ch;
  int charset;

  ch = Fchar_after (pos);
  if (! INTEGERP (ch))
    return ch;
  charset = CHAR_CHARSET (XINT (ch));
  return CHARSET_SYMBOL (charset);
}

DEFUN ("iso-charset", Fiso_charset, Siso_charset, 3, 3, 0,
       doc: /* Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.

ISO 2022's designation sequence (escape sequence) distinguishes charsets
by their DIMENSION, CHARS, and FINAL-CHAR,
where as Emacs distinguishes them by charset symbol.
See the documentation of the function `charset-info' for the meanings of
DIMENSION, CHARS, and FINAL-CHAR.  */)
     (dimension, chars, final_char)
     Lisp_Object dimension, chars, final_char;
{
  int charset;

  CHECK_NUMBER (dimension);
  CHECK_NUMBER (chars);
  CHECK_NUMBER (final_char);

  if ((charset = ISO_CHARSET_TABLE (dimension, chars, final_char)) < 0)
    return Qnil;
  return CHARSET_SYMBOL (charset);
}

/* If GENERICP is nonzero, return nonzero iff C is a valid normal or
   generic character.  If GENERICP is zero, return nonzero iff C is a
   valid normal character.  Do not call this function directly,
   instead use macro CHAR_VALID_P.  */
int
char_valid_p (c, genericp)
     int c, genericp;
{
  int charset, c1, c2;

  if (c < 0 || c >= MAX_CHAR)
    return 0;
  if (SINGLE_BYTE_CHAR_P (c))
    return 1;
  SPLIT_CHAR (c, charset, c1, c2);
  if (genericp)
    {
      if (c1)
	{
	  if (c2 <= 0) c2 = 0x20;
	}
      else
	{
	  if (c2 <= 0) c1 = c2 = 0x20;
	}
    }
  return (CHARSET_DEFINED_P (charset)
	  && CHAR_COMPONENTS_VALID_P (charset, c1, c2));
}

DEFUN ("char-valid-p", Fchar_valid_p, Schar_valid_p, 1, 2, 0,
       doc: /* Return t if OBJECT is a valid normal character.
If optional arg GENERICP is non-nil, also return t if OBJECT is
a valid generic character.  */)
     (object, genericp)
     Lisp_Object object, genericp;
{
  if (! NATNUMP (object))
    return Qnil;
  return (CHAR_VALID_P (XFASTINT (object), !NILP (genericp)) ? Qt : Qnil);
}

DEFUN ("unibyte-char-to-multibyte", Funibyte_char_to_multibyte,
       Sunibyte_char_to_multibyte, 1, 1, 0,
       doc: /* Convert the unibyte character CH to multibyte character.
The conversion is done based on `nonascii-translation-table' (which see)
 or `nonascii-insert-offset' (which see).  */)
     (ch)
     Lisp_Object ch;
{
  int c;

  CHECK_NUMBER (ch);
  c = XINT (ch);
  if (c < 0 || c >= 0400)
    error ("Invalid unibyte character: %d", c);
  c = unibyte_char_to_multibyte (c);
  if (c < 0)
    error ("Can't convert to multibyte character: %d", XINT (ch));
  return make_number (c);
}

DEFUN ("multibyte-char-to-unibyte", Fmultibyte_char_to_unibyte,
       Smultibyte_char_to_unibyte, 1, 1, 0,
       doc: /* Convert the multibyte character CH to unibyte character.
The conversion is done based on `nonascii-translation-table' (which see)
 or `nonascii-insert-offset' (which see).  */)
     (ch)
     Lisp_Object ch;
{
  int c;

  CHECK_NUMBER (ch);
  c = XINT (ch);
  if (! CHAR_VALID_P (c, 0))
    error ("Invalid multibyte character: %d", c);
  c = multibyte_char_to_unibyte (c, Qnil);
  if (c < 0)
    error ("Can't convert to unibyte character: %d", XINT (ch));
  return make_number (c);
}

DEFUN ("char-bytes", Fchar_bytes, Schar_bytes, 1, 1, 0,
       doc: /* Return 1 regardless of the argument CHAR.
This is now an obsolete function.  We keep it just for backward compatibility.  */)
     (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch);
  return make_number (1);
}

/* Return how many bytes C will occupy in a multibyte buffer.
   Don't call this function directly, instead use macro CHAR_BYTES.  */
int
char_bytes (c)
     int c;
{
  int charset;

  if (ASCII_BYTE_P (c) || (c & ~((1 << CHARACTERBITS) -1)))
    return 1;
  if (SINGLE_BYTE_CHAR_P (c) && c >= 0xA0)
    return 1;

  charset = CHAR_CHARSET (c);
  return (CHARSET_DEFINED_P (charset) ? CHARSET_BYTES (charset) : 1);
}

/* Return the width of character of which multi-byte form starts with
   C.  The width is measured by how many columns occupied on the
   screen when displayed in the current buffer.  */

#define ONE_BYTE_CHAR_WIDTH(c)					     	\
  (c < 0x20							     	\
   ? (c == '\t'							     	\
      ? XFASTINT (current_buffer->tab_width)			     	\
      : (c == '\n' ? 0 : (NILP (current_buffer->ctl_arrow) ? 4 : 2)))	\
   : (c < 0x7f							     	\
      ? 1							     	\
      : (c == 0x7F						     	\
	 ? (NILP (current_buffer->ctl_arrow) ? 4 : 2)		     	\
	 : ((! NILP (current_buffer->enable_multibyte_characters)    	\
	     && BASE_LEADING_CODE_P (c))			     	\
	    ? WIDTH_BY_CHAR_HEAD (c)				     	\
	    : 4))))

DEFUN ("char-width", Fchar_width, Schar_width, 1, 1, 0,
       doc: /* Return width of CHAR when displayed in the current buffer.
The width is measured by how many columns it occupies on the screen.
Tab is taken to occupy `tab-width' columns.  */)
     (ch)
     Lisp_Object ch;
{
  Lisp_Object val, disp;
  int c;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  CHECK_NUMBER (ch);

  c = XINT (ch);

  /* Get the way the display table would display it.  */
  disp = dp ? DISP_CHAR_VECTOR (dp, c) : Qnil;

  if (VECTORP (disp))
    XSETINT (val, XVECTOR (disp)->size);
  else if (SINGLE_BYTE_CHAR_P (c))
    XSETINT (val, ONE_BYTE_CHAR_WIDTH (c));
  else
    {
      int charset = CHAR_CHARSET (c);

      XSETFASTINT (val, CHARSET_WIDTH (charset));
    }
  return val;
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  */

int
strwidth (str, len)
     unsigned char *str;
     int len;
{
  return c_string_width (str, len, -1, NULL, NULL);
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  If PRECISION > 0, return the width of
   longest substring that doesn't exceed PRECISION, and set number of
   characters and bytes of the substring in *NCHARS and *NBYTES
   respectively.  */

int
c_string_width (str, len, precision, nchars, nbytes)
     unsigned char *str;
     int precision, *nchars, *nbytes;
{
  int i = 0, i_byte = 0;
  int width = 0;
  int chars;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i_byte < len)
    {
      int bytes, thiswidth;
      Lisp_Object val;

      if (dp)
	{
	  int c = STRING_CHAR_AND_LENGTH (str + i_byte, len - i_byte, bytes);

	  chars = 1;
	  val = DISP_CHAR_VECTOR (dp, c);
	  if (VECTORP (val))
	    thiswidth = XVECTOR (val)->size;
	  else
	    thiswidth = ONE_BYTE_CHAR_WIDTH (str[i_byte]);
	}
      else
	{
	  chars = 1;
	  PARSE_MULTIBYTE_SEQ (str + i_byte, len - i_byte, bytes);
	  thiswidth = ONE_BYTE_CHAR_WIDTH (str[i_byte]);
	}

      if (precision > 0
	  && (width + thiswidth > precision))
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      i++;
      i_byte += bytes;
      width += thiswidth;
  }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

/* Return width of Lisp string STRING when displayed in the current
   buffer.  The width is measured by how many columns it occupies on
   the screen while paying attention to compositions.  If PRECISION >
   0, return the width of longest substring that doesn't exceed
   PRECISION, and set number of characters and bytes of the substring
   in *NCHARS and *NBYTES respectively.  */

int
lisp_string_width (string, precision, nchars, nbytes)
     Lisp_Object string;
     int precision, *nchars, *nbytes;
{
  int len = XSTRING (string)->size;
  int len_byte = STRING_BYTES (XSTRING (string));
  unsigned char *str = XSTRING (string)->data;
  int i = 0, i_byte = 0;
  int width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i < len)
    {
      int chars, bytes, thiswidth;
      Lisp_Object val;
      int cmp_id;
      int ignore, end;

      if (find_composition (i, -1, &ignore, &end, &val, string)
	  && ((cmp_id = get_composition_id (i, i_byte, end - i, val, string))
	      >= 0))
	{
	  thiswidth = composition_table[cmp_id]->width;
	  chars = end - i;
	  bytes = string_char_to_byte (string, end) - i_byte;
	}
      else if (dp)
	{
	  int c = STRING_CHAR_AND_LENGTH (str + i_byte, len - i_byte, bytes);

	  chars = 1;
	  val = DISP_CHAR_VECTOR (dp, c);
	  if (VECTORP (val))
	    thiswidth = XVECTOR (val)->size;
	  else
	    thiswidth = ONE_BYTE_CHAR_WIDTH (str[i_byte]);
	}
      else
	{
	  chars = 1;
	  PARSE_MULTIBYTE_SEQ (str + i_byte, len_byte - i_byte, bytes);
	  thiswidth = ONE_BYTE_CHAR_WIDTH (str[i_byte]);
	}

      if (precision > 0
	  && (width + thiswidth > precision))
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      i += chars;
      i_byte += bytes;
      width += thiswidth;
  }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

DEFUN ("string-width", Fstring_width, Sstring_width, 1, 1, 0,
       doc: /* Return width of STRING when displayed in the current buffer.
Width is measured by how many columns it occupies on the screen.
When calculating width of a multibyte character in STRING,
only the base leading-code is considered; the validity of
the following bytes is not checked.  Tabs in STRING are always
taken to occupy `tab-width' columns.  */)
     (str)
     Lisp_Object str;
{
  Lisp_Object val;

  CHECK_STRING (str);
  XSETFASTINT (val, lisp_string_width (str, -1, NULL, NULL));
  return val;
}

DEFUN ("char-direction", Fchar_direction, Schar_direction, 1, 1, 0,
       doc: /* Return the direction of CHAR.
The returned value is 0 for left-to-right and 1 for right-to-left.  */)
     (ch)
     Lisp_Object ch;
{
  int charset;

  CHECK_NUMBER (ch);
  charset = CHAR_CHARSET (XFASTINT (ch));
  if (!CHARSET_DEFINED_P (charset))
    invalid_character (XINT (ch));
  return CHARSET_TABLE_INFO (charset, CHARSET_DIRECTION_IDX);
}

DEFUN ("chars-in-region", Fchars_in_region, Schars_in_region, 2, 2, 0,
       doc: /* Return number of characters between BEG and END.  */)
     (beg, end)
     Lisp_Object beg, end;
{
  int from, to;

  CHECK_NUMBER_COERCE_MARKER (beg);
  CHECK_NUMBER_COERCE_MARKER (end);

  from = min (XFASTINT (beg), XFASTINT (end));
  to = max (XFASTINT (beg), XFASTINT (end));

  return make_number (to - from);
}

/* Return the number of characters in the NBYTES bytes at PTR.
   This works by looking at the contents and checking for multibyte sequences.
   However, if the current buffer has enable-multibyte-characters = nil,
   we treat each byte as a character.  */

int
chars_in_text (ptr, nbytes)
     unsigned char *ptr;
     int nbytes;
{
  /* current_buffer is null at early stages of Emacs initialization.  */
  if (current_buffer == 0
      || NILP (current_buffer->enable_multibyte_characters))
    return nbytes;

  return multibyte_chars_in_text (ptr, nbytes);
}

/* Return the number of characters in the NBYTES bytes at PTR.
   This works by looking at the contents and checking for multibyte sequences.
   It ignores enable-multibyte-characters.  */

int
multibyte_chars_in_text (ptr, nbytes)
     unsigned char *ptr;
     int nbytes;
{
  unsigned char *endp;
  int chars, bytes;

  endp = ptr + nbytes;
  chars = 0;

  while (ptr < endp)
    {
      PARSE_MULTIBYTE_SEQ (ptr, endp - ptr, bytes);
      ptr += bytes;
      chars++;
    }

  return chars;
}

/* Parse unibyte text at STR of LEN bytes as multibyte text, and
   count the numbers of characters and bytes in it.  On counting
   bytes, pay attention to the fact that 8-bit characters in the range
   0x80..0x9F are represented by 2 bytes in multibyte text.  */
void
parse_str_as_multibyte (str, len, nchars, nbytes)
     unsigned char *str;
     int len, *nchars, *nbytes;
{
  unsigned char *endp = str + len;
  int n, chars = 0, bytes = 0;

  while (str < endp)
    {
      if (UNIBYTE_STR_AS_MULTIBYTE_P (str, endp - str, n))
	str += n, bytes += n;
      else
	str++, bytes += 2;
      chars++;
    }
  *nchars = chars;
  *nbytes = bytes;
  return;
}

/* Arrange unibyte text at STR of NBYTES bytes as multibyte text.
   It actually converts only 8-bit characters in the range 0x80..0x9F
   that don't contruct multibyte characters to multibyte forms.  If
   NCHARS is nonzero, set *NCHARS to the number of characters in the
   text.  It is assured that we can use LEN bytes at STR as a work
   area and that is enough.  Return the number of bytes of the
   resulting text.  */

int
str_as_multibyte (str, len, nbytes, nchars)
     unsigned char *str;
     int len, nbytes, *nchars;
{
  unsigned char *p = str, *endp = str + nbytes;
  unsigned char *to;
  int chars = 0;
  int n;

  while (p < endp && UNIBYTE_STR_AS_MULTIBYTE_P (p, endp - p, n))
    p += n, chars++;
  if (nchars)
    *nchars = chars;
  if (p == endp)
    return nbytes;

  to = p;
  nbytes = endp - p;
  endp = str + len;
  safe_bcopy (p, endp - nbytes, nbytes);
  p = endp - nbytes;
  while (p < endp)
    {
      if (UNIBYTE_STR_AS_MULTIBYTE_P (p, endp - p, n))
	{
	  while (n--)
	    *to++ = *p++;
	}	  
      else
	{
	  *to++ = LEADING_CODE_8_BIT_CONTROL;
	  *to++ = *p++ + 0x20;
	}
      chars++;
    }
  if (nchars)
    *nchars = chars;
  return (to - str);
}

/* Parse unibyte string at STR of LEN bytes, and return the number of
   bytes it may ocupy when converted to multibyte string by
   `str_to_multibyte'.  */

int
parse_str_to_multibyte (str, len)
     unsigned char *str;
     int len;
{
  unsigned char *endp = str + len;
  int bytes;

  for (bytes = 0; str < endp; str++)
    bytes += (*str < 0x80 || *str >= 0xA0) ? 1 : 2;
  return bytes;
}

/* Convert unibyte text at STR of NBYTES bytes to multibyte text
   that contains the same single-byte characters.  It actually
   converts all 8-bit characters to multibyte forms.  It is assured
   that we can use LEN bytes at STR as a work area and that is
   enough.  */

int
str_to_multibyte (str, len, bytes)
     unsigned char *str;
     int len, bytes;
{
  unsigned char *p = str, *endp = str + bytes;
  unsigned char *to;

  while (p < endp && (*p < 0x80 || *p >= 0xA0)) p++;
  if (p == endp)
    return bytes;
  to = p;
  bytes = endp - p;
  endp = str + len;
  safe_bcopy (p, endp - bytes, bytes);
  p = endp - bytes;
  while (p < endp)      
    {
      if (*p < 0x80 || *p >= 0xA0)
	*to++ = *p++;
      else
	*to++ = LEADING_CODE_8_BIT_CONTROL, *to++ = *p++ + 0x20;
    }
  return (to - str);
}

/* Arrange multibyte text at STR of LEN bytes as a unibyte text.  It
   actually converts only 8-bit characters in the range 0x80..0x9F to
   unibyte forms.  */

int
str_as_unibyte (str, bytes)
     unsigned char *str;
     int bytes;
{
  unsigned char *p = str, *endp = str + bytes;
  unsigned char *to = str;

  while (p < endp && *p != LEADING_CODE_8_BIT_CONTROL) p++;
  to = p;
  while (p < endp)      
    {
      if (*p == LEADING_CODE_8_BIT_CONTROL)
	*to++ = *(p + 1) - 0x20, p += 2;
      else
	*to++ = *p++;
    }
  return (to - str);
}


DEFUN ("string", Fstring, Sstring, 0, MANY, 0,
  doc: /* Concatenate all the argument characters and make the result a string.
usage: (string &rest CHARACTERS)  */)
     (n, args)
     int n;
     Lisp_Object *args;
{
  int i;
  unsigned char *buf = (unsigned char *) alloca (MAX_MULTIBYTE_LENGTH * n);
  unsigned char *p = buf;
  int c;
  int multibyte = 0;

  for (i = 0; i < n; i++)
    {
      CHECK_NUMBER (args[i]);
      if (!multibyte && !SINGLE_BYTE_CHAR_P (XFASTINT (args[i])))
	multibyte = 1;
    }

  for (i = 0; i < n; i++)
    {
      c = XINT (args[i]);
      if (multibyte)
	p += CHAR_STRING (c, p);
      else
	*p++ = c;
    }

  return make_string_from_bytes (buf, n, p - buf);
}

#endif /* emacs */

int
charset_id_internal (charset_name)
     char *charset_name;
{
  Lisp_Object val;

  val= Fget (intern (charset_name), Qcharset);
  if (!VECTORP (val))
    error ("Charset %s is not defined", charset_name);

  return (XINT (XVECTOR (val)->contents[0]));
}

DEFUN ("setup-special-charsets", Fsetup_special_charsets,
       Ssetup_special_charsets, 0, 0, 0, doc: /* Internal use only.  */)
     ()
{
  charset_latin_iso8859_1 = charset_id_internal ("latin-iso8859-1");
  charset_jisx0208_1978 = charset_id_internal ("japanese-jisx0208-1978");
  charset_jisx0208 = charset_id_internal ("japanese-jisx0208");
  charset_katakana_jisx0201 = charset_id_internal ("katakana-jisx0201");
  charset_latin_jisx0201 = charset_id_internal ("latin-jisx0201");
  charset_big5_1 = charset_id_internal ("chinese-big5-1");
  charset_big5_2 = charset_id_internal ("chinese-big5-2");
  return Qnil;
}

void
init_charset_once ()
{
  int i, j, k;

  staticpro (&Vcharset_table);
  staticpro (&Vcharset_symbol_table);
  staticpro (&Vgeneric_character_list);

  /* This has to be done here, before we call Fmake_char_table.  */
  Qcharset_table = intern ("charset-table");
  staticpro (&Qcharset_table);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  /* Now we are ready to set up this property, so we can
     create the charset table.  */
  Fput (Qcharset_table, Qchar_table_extra_slots, make_number (0));
  Vcharset_table = Fmake_char_table (Qcharset_table, Qnil);

  Qunknown = intern ("unknown");
  staticpro (&Qunknown);
  Vcharset_symbol_table = Fmake_vector (make_number (MAX_CHARSET + 1),
					Qunknown);

  /* Setup tables.  */
  for (i = 0; i < 2; i++)
    for (j = 0; j < 2; j++)
      for (k = 0; k < 128; k++)
	iso_charset_table [i][j][k] = -1;

  for (i = 0; i < 256; i++)
    bytes_by_char_head[i] = 1;
  bytes_by_char_head[LEADING_CODE_PRIVATE_11] = 3;
  bytes_by_char_head[LEADING_CODE_PRIVATE_12] = 3;
  bytes_by_char_head[LEADING_CODE_PRIVATE_21] = 4;
  bytes_by_char_head[LEADING_CODE_PRIVATE_22] = 4;

  for (i = 0; i < 128; i++)
    width_by_char_head[i] = 1;
  for (; i < 256; i++)
    width_by_char_head[i] = 4;
  width_by_char_head[LEADING_CODE_PRIVATE_11] = 1;
  width_by_char_head[LEADING_CODE_PRIVATE_12] = 2;
  width_by_char_head[LEADING_CODE_PRIVATE_21] = 1;
  width_by_char_head[LEADING_CODE_PRIVATE_22] = 2;

  {
    Lisp_Object val;

    val = Qnil;
    for (i = 0x81; i < 0x90; i++)
      val = Fcons (make_number ((i - 0x70) << 7), val);
    for (; i < 0x9A; i++)
      val = Fcons (make_number ((i - 0x8F) << 14), val);
    for (i = 0xA0; i < 0xF0; i++)
      val = Fcons (make_number ((i - 0x70) << 7), val);
    for (; i < 0xFF; i++)
      val = Fcons (make_number ((i - 0xE0) << 14), val);
    Vgeneric_character_list = Fnreverse (val);
  }

  nonascii_insert_offset = 0;
  Vnonascii_translation_table = Qnil;
}

#ifdef emacs

void
syms_of_charset ()
{
  Qcharset = intern ("charset");
  staticpro (&Qcharset);

  Qascii = intern ("ascii");
  staticpro (&Qascii);

  Qeight_bit_control = intern ("eight-bit-control");
  staticpro (&Qeight_bit_control);

  Qeight_bit_graphic = intern ("eight-bit-graphic");
  staticpro (&Qeight_bit_graphic);

  /* Define special charsets ascii, eight-bit-control, and
     eight-bit-graphic.  */
  update_charset_table (make_number (CHARSET_ASCII),
			make_number (1), make_number (94),
			make_number (1),
			make_number (0),
			make_number ('B'),
			make_number (0),
			build_string ("ASCII"),
			Qnil,	/* same as above */
			build_string ("ASCII (ISO646 IRV)"));
  CHARSET_SYMBOL (CHARSET_ASCII) = Qascii;
  Fput (Qascii, Qcharset, CHARSET_TABLE_ENTRY (CHARSET_ASCII));

  update_charset_table (make_number (CHARSET_8_BIT_CONTROL),
			make_number (1), make_number (96),
			make_number (4),
			make_number (0),
			make_number (-1),
			make_number (-1),
			build_string ("8-bit control code (0x80..0x9F)"),
			Qnil,	/* same as above */
			Qnil);	/* same as above */
  CHARSET_SYMBOL (CHARSET_8_BIT_CONTROL) = Qeight_bit_control;
  Fput (Qeight_bit_control, Qcharset,
	CHARSET_TABLE_ENTRY (CHARSET_8_BIT_CONTROL));

  update_charset_table (make_number (CHARSET_8_BIT_GRAPHIC),
			make_number (1), make_number (96),
			make_number (4),
			make_number (0),
			make_number (-1),
			make_number (-1),
			build_string ("8-bit graphic char (0xA0..0xFF)"),
			Qnil,	/* same as above */
			Qnil);	/* same as above */
  CHARSET_SYMBOL (CHARSET_8_BIT_GRAPHIC) = Qeight_bit_graphic;
  Fput (Qeight_bit_graphic, Qcharset,
	CHARSET_TABLE_ENTRY (CHARSET_8_BIT_GRAPHIC));

  Qauto_fill_chars = intern ("auto-fill-chars");
  staticpro (&Qauto_fill_chars);
  Fput (Qauto_fill_chars, Qchar_table_extra_slots, make_number (0));

  defsubr (&Sdefine_charset);
  defsubr (&Sgeneric_character_list);
  defsubr (&Sget_unused_iso_final_char);
  defsubr (&Sdeclare_equiv_charset);
  defsubr (&Sfind_charset_region);
  defsubr (&Sfind_charset_string);
  defsubr (&Smake_char_internal);
  defsubr (&Ssplit_char);
  defsubr (&Schar_charset);
  defsubr (&Scharset_after);
  defsubr (&Siso_charset);
  defsubr (&Schar_valid_p);
  defsubr (&Sunibyte_char_to_multibyte);
  defsubr (&Smultibyte_char_to_unibyte);
  defsubr (&Schar_bytes);
  defsubr (&Schar_width);
  defsubr (&Sstring_width);
  defsubr (&Schar_direction);
  defsubr (&Schars_in_region);
  defsubr (&Sstring);
  defsubr (&Ssetup_special_charsets);

  DEFVAR_LISP ("charset-list", &Vcharset_list,
	       doc: /* List of charsets ever defined.  */);
  Vcharset_list = Fcons (Qascii, Fcons (Qeight_bit_control,
					Fcons (Qeight_bit_graphic, Qnil)));

  DEFVAR_LISP ("translation-table-vector",  &Vtranslation_table_vector,
	       doc: /* Vector of cons cell of a symbol and translation table ever defined.
An ID of a translation table is an index of this vector.  */);
  Vtranslation_table_vector = Fmake_vector (make_number (16), Qnil);

  DEFVAR_INT ("leading-code-private-11", &leading_code_private_11,
	      doc: /* Leading-code of private TYPE9N charset of column-width 1.  */);
  leading_code_private_11 = LEADING_CODE_PRIVATE_11;

  DEFVAR_INT ("leading-code-private-12", &leading_code_private_12,
	      doc: /* Leading-code of private TYPE9N charset of column-width 2.  */);
  leading_code_private_12 = LEADING_CODE_PRIVATE_12;

  DEFVAR_INT ("leading-code-private-21", &leading_code_private_21,
	      doc: /* Leading-code of private TYPE9Nx9N charset of column-width 1.  */);
  leading_code_private_21 = LEADING_CODE_PRIVATE_21;

  DEFVAR_INT ("leading-code-private-22", &leading_code_private_22,
	      doc: /* Leading-code of private TYPE9Nx9N charset of column-width 2.  */);
  leading_code_private_22 = LEADING_CODE_PRIVATE_22;

  DEFVAR_INT ("nonascii-insert-offset", &nonascii_insert_offset,
	      doc: /* Offset for converting non-ASCII unibyte codes 0240...0377 to multibyte.
This is used for converting unibyte text to multibyte,
and for inserting character codes specified by number.

This serves to convert a Latin-1 or similar 8-bit character code
to the corresponding Emacs multibyte character code.
Typically the value should be (- (make-char CHARSET 0) 128),
for your choice of character set.
If `nonascii-translation-table' is non-nil, it overrides this variable.  */);
  nonascii_insert_offset = 0;

  DEFVAR_LISP ("nonascii-translation-table", &Vnonascii_translation_table,
	       doc: /* Translation table to convert non-ASCII unibyte codes to multibyte.
This is used for converting unibyte text to multibyte,
and for inserting character codes specified by number.

Conversion is performed only when multibyte characters are enabled,
and it serves to convert a Latin-1 or similar 8-bit character code
to the corresponding Emacs character code.

If this is nil, `nonascii-insert-offset' is used instead.
See also the docstring of `make-translation-table'.  */);
  Vnonascii_translation_table = Qnil;

  DEFVAR_LISP ("auto-fill-chars", &Vauto_fill_chars,
	       doc: /* A char-table for characters which invoke auto-filling.
Such characters have value t in this table.  */);
  Vauto_fill_chars = Fmake_char_table (Qauto_fill_chars, Qnil);
  CHAR_TABLE_SET (Vauto_fill_chars, make_number (' '), Qt);
  CHAR_TABLE_SET (Vauto_fill_chars, make_number ('\n'), Qt);
}

#endif /* emacs */
