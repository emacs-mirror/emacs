/* Basic multilingual character support.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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
#include "coding.h"
#include "disptab.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* emacs */

Lisp_Object Qcharset, Qascii;
Lisp_Object Qunknown;

/* Declaration of special leading-codes.  */
int leading_code_private_11;	/* for private DIMENSION1 of 1-column */
int leading_code_private_12;	/* for private DIMENSION1 of 2-column */
int leading_code_private_21;	/* for private DIMENSION2 of 1-column */
int leading_code_private_22;	/* for private DIMENSION2 of 2-column */

/* Declaration of special charsets.  */
int charset_ascii;		/* ASCII */
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
int nonascii_insert_offset;

/* Translation table for converting non-ASCII unibyte characters
   to multibyte codes, or nil.  */
Lisp_Object Vnonascii_translation_table;

/* List of all possible generic characters.  */
Lisp_Object Vgeneric_character_list;

#define min(X, Y) ((X) < (Y) ? (X) : (Y))
#define max(X, Y) ((X) > (Y) ? (X) : (Y))

void
invalid_character (c)
     int c;
{
  error ("Invalid character: 0%o, %d, 0x%x", c, c, c);
}

/* Parse a multibyte character string STR of length LENGTH (>= 2) set
   BYTES to the length of actual multibyte sequence, CHARSET, C1, and
   C2 to such values that MAKE_CHAR can make the multibyte character
   from them.

   It is assumed that *STR is one of base leading codes and the
   following (LENGTH - 1) bytes satisfy !CHAR_HEAD_P.

   This macro should be called only from SPLIT_MULTIBYTE_SEQ.  */

#define SPLIT_CHARACTER_SEQ(str, length, bytes, charset, c1, c2)	\
  do {									\
    (bytes) = 1;							\
    (charset) = (str)[0];						\
    if ((charset) >= LEADING_CODE_PRIVATE_11				\
	&& (charset) <= LEADING_CODE_PRIVATE_22)			\
      (charset) = (str)[(bytes)++];					\
    if ((bytes) < (length))						\
      {									\
	(c1) = (str)[(bytes)++] & 0x7F;					\
	if ((bytes) < (length))						\
	  (c2) = (str)[(bytes)++] & 0x7F;				\
	else								\
	  (c2) = -1;							\
      }									\
    else								\
      (c1) = (c2) = -1;							\
  } while (0)

/* Parse string STR of length LENGTH and check if a multibyte
   characters is at STR.  Set BYTES to the actual length, CHARSET, C1,
   C2 to proper values for that character.  */

#define SPLIT_MULTIBYTE_SEQ(str, length, bytes, charset, c1, c2)	\
  do {									\
    int i;								\
    if (ASCII_BYTE_P ((str)[0]))					\
      i = 1;								\
    else								\
      for (i = 1; i < (length) && ! CHAR_HEAD_P ((str)[i]); i++);	\
    if (i == 1)								\
      (bytes) = 1, (charset) = CHARSET_ASCII, (c1) = (str)[0] ;		\
    else								\
      {									\
	if (i > BYTES_BY_CHAR_HEAD ((str)[0]))				\
	  i = BYTES_BY_CHAR_HEAD ((str)[0]);				\
	SPLIT_CHARACTER_SEQ (str, i, bytes, charset, c1, c2);		\
      }									\
  } while (0)

/* 1 if CHARSET, C1, and C2 compose a valid character, else 0.  */
#define CHAR_COMPONENTS_VALID_P(charset, c1, c2)	\
  (charset == CHARSET_ASCII				\
   ? ((c1) >= 0 && (c1) <= 0x7F)			\
   : (CHARSET_DIMENSION (charset) == 1			\
      ? ((c1) >= 0x20 && (c1) <= 0x7F)			\
      : ((c1) >= 0x20 && (c1) <= 0x7F && (c2) >= 0x20 && (c2) <= 0x7F)))

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
  unsigned char *p = str;

  if (c & CHAR_MODIFIER_MASK)	/* This includes the case C is negative.  */
    {
      /* Multibyte character can't have a modifier bit.  */
      if (! SINGLE_BYTE_CHAR_P ((c & ~CHAR_MODIFIER_MASK)))
	invalid_character (c);

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

      /* If C still has any modifier bits, it is an invalid character.  */
      if (c & CHAR_MODIFIER_MASK)
	invalid_character (c);

      *p++ = c;
    }
  else if (c < MAX_CHAR)
    {
      int charset, c1, c2;

      SPLIT_NON_ASCII_CHAR (c, charset, c1, c2);

      if (charset >= LEADING_CODE_EXT_11)
	*p++ = (charset < LEADING_CODE_EXT_12
		? LEADING_CODE_PRIVATE_11
		: (charset < LEADING_CODE_EXT_21
		   ? LEADING_CODE_PRIVATE_12
		   : (charset < LEADING_CODE_EXT_22
		      ? LEADING_CODE_PRIVATE_21
		      : LEADING_CODE_PRIVATE_22)));
      *p++ = charset;
      if (c1 > 0 && c1 < 32 || c2 > 0 && c2 < 32)
	invalid_character (c);
      if (c1)
	{
	  *p++ = c1 | 0x80;
	  if (c2 > 0)
	    *p++ = c2 | 0x80;
	}
    }

  return (p -str);
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
  int charset, c1, c2, chars;

  if (SINGLE_BYTE_CHAR_P (c))
    return 1;
  if (c >= MAX_CHAR)
    return 0;
  
  SPLIT_NON_ASCII_CHAR (c, charset, c1, c2);
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
  if (dimension == 1 && alt_c1 > 0 || dimension == 2 && alt_c2 > 0)
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

  /* Get byte length of multibyte form, base leading-code, and
     extended leading-code of the charset.  See the comment under the
     title "GENERAL NOTE on CHARACTER SET (CHARSET)" in charset.h.  */
  bytes = XINT (dimension);
  if (charset < MIN_CHARSET_PRIVATE_DIMENSION1)
    {
      /* Official charset, it doesn't have an extended leading-code.  */
      if (charset != CHARSET_ASCII)
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
    } 

  if (BYTES_BY_CHAR_HEAD (leading_code_base) != bytes)
    error ("Invalid dimension for the charset-ID %d", charset);

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

  if (charset != CHARSET_ASCII
      && charset < MIN_CHARSET_PRIVATE_DIMENSION1)
    {
      width_by_char_head[leading_code_base] = XINT (width);

      /* Update table emacs_code_class.  */
      emacs_code_class[charset] = (bytes == 2
				   ? EMACS_leading_code_2
				   : (bytes == 3
				      ? EMACS_leading_code_3
				      : EMACS_leading_code_4));
    }

  /* Update table iso_charset_table.  */
  if (ISO_CHARSET_TABLE (dimension, chars, iso_final_char) < 0)
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

  return ((SYMBOLP (charset_symbol)
	   && (val = Fget (charset_symbol, Qcharset), VECTORP (val))
	   && (charset = XINT (XVECTOR (val)->contents[CHARSET_ID_IDX]),
	       CHARSET_VALID_P (charset)))
	  ? charset : -1);
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
      if (width == 1)
	from = LEADING_CODE_EXT_11, to = LEADING_CODE_EXT_12;
      else
	from = LEADING_CODE_EXT_12, to = LEADING_CODE_EXT_21;
    }
  else
    {
      if (width == 1)
	from = LEADING_CODE_EXT_21, to = LEADING_CODE_EXT_22;
      else
	from = LEADING_CODE_EXT_22, to = LEADING_CODE_EXT_MAX + 1;
    }

  for (charset = from; charset < to; charset++)
    if (!CHARSET_DEFINED_P (charset)) break;

  return make_number (charset < to ? charset : 0);
}

DEFUN ("define-charset", Fdefine_charset, Sdefine_charset, 3, 3, 0,
  "Define CHARSET-ID as the identification number of CHARSET with INFO-VECTOR.\n\
If CHARSET-ID is nil, it is decided automatically, which means CHARSET is\n\
 treated as a private charset.\n\
INFO-VECTOR is a vector of the format:\n\
   [DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE\n\
    SHORT-NAME LONG-NAME DESCRIPTION]\n\
The meanings of each elements is as follows:\n\
DIMENSION (integer) is the number of bytes to represent a character: 1 or 2.\n\
CHARS (integer) is the number of characters in a dimension: 94 or 96.\n\
WIDTH (integer) is the number of columns a character in the charset\n\
occupies on the screen: one of 0, 1, and 2.\n\
\n\
DIRECTION (integer) is the rendering direction of characters in the\n\
charset when rendering.  If 0, render from left to right, else\n\
render from right to left.\n\
\n\
ISO-FINAL-CHAR (character) is the final character of the\n\
corresponding ISO 2022 charset.\n\
\n\
ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked\n\
while encoding to variants of ISO 2022 coding system, one of the\n\
following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).\n\
\n\
SHORT-NAME (string) is the short name to refer to the charset.\n\
\n\
LONG-NAME (string) is the long name to refer to the charset.\n\
\n\
DESCRIPTION (string) is the description string of the charset.")
  (charset_id, charset_symbol, info_vector)
     Lisp_Object charset_id, charset_symbol, info_vector;
{
  Lisp_Object *vec;

  if (!NILP (charset_id))
    CHECK_NUMBER (charset_id, 0);
  CHECK_SYMBOL (charset_symbol, 1);
  CHECK_VECTOR (info_vector, 2);

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
      || !INTEGERP (vec[4]) || !(XINT (vec[4]) >= '0' && XINT (vec[4]) <= '~')
      || !INTEGERP (vec[5]) || !(XINT (vec[5]) == 0 || XINT (vec[5]) == 1)
      || !STRINGP (vec[6])
      || !STRINGP (vec[7])
      || !STRINGP (vec[8]))
    error ("Invalid info-vector argument for defining charset %s",
	   XSYMBOL (charset_symbol)->name->data);

  if (NILP (charset_id))
    {
      charset_id = get_new_private_charset_id (XINT (vec[0]), XINT (vec[2]));
      if (XINT (charset_id) == 0)
	error ("There's no room for a new private charset %s",
	       XSYMBOL (charset_symbol)->name->data);
    }

  update_charset_table (charset_id, vec[0], vec[1], vec[2], vec[3],
			vec[4], vec[5], vec[6], vec[7], vec[8]);
  Fput (charset_symbol, Qcharset, CHARSET_TABLE_ENTRY (XINT (charset_id)));
  CHARSET_SYMBOL (XINT (charset_id)) = charset_symbol;
  Vcharset_list = Fcons (charset_symbol, Vcharset_list);
  return Qnil;
}

DEFUN ("generic-character-list", Fgeneric_character_list,
       Sgeneric_character_list, 0, 0, 0,
  "Return a list of all possible generic characters.\n\
It includes a generic character for a charset not yet defined.")
  ()
{
  return Vgeneric_character_list;
}

DEFUN ("get-unused-iso-final-char", Fget_unused_iso_final_char,
       Sget_unused_iso_final_char, 2, 2, 0,
  "Return an unsed ISO's final char for a charset of DIMENISION and CHARS.\n\
DIMENSION is the number of bytes to represent a character: 1 or 2.\n\
CHARS is the number of characters in a dimension: 94 or 96.\n\
\n\
This final char is for private use, thus the range is `0' (48) .. `?' (63).\n\
If there's no unused final char for the specified kind of charset,\n\
return nil.")
  (dimension, chars)
     Lisp_Object dimension, chars;
{
  int final_char;

  CHECK_NUMBER (dimension, 0);
  CHECK_NUMBER (chars, 1);
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
  "Declare a charset of DIMENSION, CHARS, FINAL-CHAR is the same as CHARSET.\n\
CHARSET should be defined by `defined-charset' in advance.")
  (dimension, chars, final_char, charset_symbol)
     Lisp_Object dimension, chars, final_char, charset_symbol;
{
  int charset;

  CHECK_NUMBER (dimension, 0);
  CHECK_NUMBER (chars, 1);
  CHECK_NUMBER (final_char, 2);
  CHECK_SYMBOL (charset_symbol, 3);

  if (XINT (dimension) != 1 && XINT (dimension) != 2)
    error ("Invalid DIMENSION %d, it should be 1 or 2", XINT (dimension));
  if (XINT (chars) != 94 && XINT (chars) != 96)
    error ("Invalid CHARS %d, it should be 94 or 96", XINT (chars));
  if (XINT (final_char) < '0' || XFASTINT (final_char) > '~')
    error ("Invalid FINAL-CHAR %c, it should be `0'..`~'", XINT (chars));
  if ((charset = get_charset_id (charset_symbol)) < 0)
    error ("Invalid charset %s", XSYMBOL (charset_symbol)->name->data);

  ISO_CHARSET_TABLE (dimension, chars, final_char) = charset;
  return Qnil;
}

/* Return number of different charsets in STR of length LEN.  In
   addition, for each found charset N, CHARSETS[N] is set 1.  The
   caller should allocate CHARSETS (MAX_CHARSET + 1 elements) in advance.
   It may lookup a translation table TABLE if supplied.

   If MULTIBYTE is zero, do not check multibyte characters, i.e. if
   any ASCII codes (7-bit) are found, CHARSET[0] is set to 1, if any
   8-bit codes are found CHARSET[1] is set to 1.  */

int
find_charset_in_str (str, len, charsets, table, multibyte)
     unsigned char *str;
     int len, *charsets;
     Lisp_Object table;
     int multibyte;
{
  register int num = 0, c;

  if (! multibyte)
    {
      unsigned char *endp = str + len;
      int maskbits = 0;
	
      while (str < endp && maskbits != 3)
	maskbits |=  (*str++ < 0x80 ? 1 : 2);
      if (maskbits & 1)
	{
	  charsets[0] = 1;
	  num++;
	}
      if (maskbits & 2)
	{
	  charsets[1] = 1;
	  num++;
	}
      return num;
    }

  if (! CHAR_TABLE_P (table))
    table = Qnil;

  while (len > 0)
    {
      int bytes, charset, c1, c2;

      SPLIT_MULTIBYTE_SEQ (str, len, bytes, charset, c1, c2);

      if (! NILP (table))
	{
	  int c1 = translate_char (table, -1, charset, c1, c2);
	  if (c1 >= 0)
	    charset = CHAR_CHARSET (c);
	}

      if (!charsets[charset])
	{
	  charsets[charset] = 1;
	  num += 1;
	}
      str += bytes;
      len -= bytes;
    }
  return num;
}

DEFUN ("find-charset-region", Ffind_charset_region, Sfind_charset_region,
       2, 3, 0,
  "Return a list of charsets in the region between BEG and END.\n\
BEG and END are buffer positions.\n\
Optional arg TABLE if non-nil is a translation table to look up.\n\
\n\
If the region contains invalid multiybte characters,\n\
`unknown' is included in the returned list.\n\
\n\
If the current buffer is unibyte, the returned list contains\n\
`ascii' if any 7-bit characters are found,\n\
and `unknown' if any 8-bit characters are found.")
  (beg, end, table)
     Lisp_Object beg, end, table;
{
  int charsets[MAX_CHARSET + 1];
  int from, from_byte, to, stop, stop_byte, i;
  Lisp_Object val;
  int undefined;
  int multibyte = !NILP (current_buffer->enable_multibyte_characters);

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
      find_charset_in_str (BYTE_POS_ADDR (from_byte), stop_byte - from_byte,
			   charsets, table, multibyte);
      if (stop < to)
	{
	  from = stop, from_byte = stop_byte;
	  stop = to, stop_byte = CHAR_TO_BYTE (stop);
	}
      else
	break;
    }

  val = Qnil;
  undefined = 0;
  for (i = (multibyte ? MAX_CHARSET : 1); i >= 0; i--)
    if (charsets[i])
      {
	if (CHARSET_DEFINED_P (i))
	  val = Fcons (CHARSET_SYMBOL (i), val);
	else
	  undefined = 1;
      }
  if (undefined)
    val = Fcons (Qunknown, val);
  return val;
}

DEFUN ("find-charset-string", Ffind_charset_string, Sfind_charset_string,
       1, 2, 0,
  "Return a list of charsets in STR.\n\
Optional arg TABLE if non-nil is a translation table to look up.\n\
\n\
If the region contains invalid multiybte characters,\n\
`unknown' is included in the returned list.\n\
\n\
If STR is unibyte, the returned list contains\n\
`ascii' if any 7-bit characters are found,\n\
and `unknown' if any 8-bit characters are found.")
  (str, table)
     Lisp_Object str, table;
{
  int charsets[MAX_CHARSET + 1];
  int i;
  Lisp_Object val;
  int undefined;
  int multibyte;

  CHECK_STRING (str, 0);
  multibyte = STRING_MULTIBYTE (str);

  bzero (charsets, (MAX_CHARSET + 1) * sizeof (int));
  find_charset_in_str (XSTRING (str)->data, STRING_BYTES (XSTRING (str)),
		       charsets, table, multibyte);
  val = Qnil;
  undefined = 0;
  for (i = (multibyte ? MAX_CHARSET : 1); i >= 0; i--)
    if (charsets[i])
      {
	if (CHARSET_DEFINED_P (i))
	  val = Fcons (CHARSET_SYMBOL (i), val);
	else
	  undefined = 1;
      }
  if (undefined)
    val = Fcons (Qunknown, val);
  return val;
}

DEFUN ("make-char-internal", Fmake_char_internal, Smake_char_internal, 1, 3, 0,
  "")
  (charset, code1, code2)
     Lisp_Object charset, code1, code2;
{
  int charset_id, c1, c2;

  CHECK_NUMBER (charset, 0);
  charset_id = XINT (charset);
  if (!CHARSET_DEFINED_P (charset_id))
    error ("Invalid charset ID: %d", XINT (charset));

  if (NILP (code1))
    c1 = 0;
  else
    {
      CHECK_NUMBER (code1, 1);
      c1 = XINT (code1);
    }
  if (NILP (code2))
    c2 = 0;
  else
    {
      CHECK_NUMBER (code2, 2);
      c2 = XINT (code2);
    }

  if (c1 < 0 || c1 > 0xFF || c2 < 0 || c2 > 0xFF)
    error ("Invalid code points for charset ID %d: %d %d", charset_id, c1, c2);
  c1 &= 0x7F;
  c2 &= 0x7F;
  if (c1 == 0
      ? c2 != 0
      : (c2 == 0
	 ? !CHAR_COMPONENTS_VALID_P (charset, c1, 0x20)
	 : !CHAR_COMPONENTS_VALID_P (charset, c1, c2)))
    error ("Invalid code points for charset ID %d: %d %d", charset_id, c1, c2);

  return make_number (MAKE_CHAR (charset_id, c1, c2));
}

DEFUN ("split-char", Fsplit_char, Ssplit_char, 1, 1, 0,
  "Return list of charset and one or two position-codes of CHAR.\n\
If CHAR is invalid as a character code,\n\
return a list of symbol `unknown' and CHAR.")
  (ch)
     Lisp_Object ch;
{
  Lisp_Object val;
  int c, charset, c1, c2;

  CHECK_NUMBER (ch, 0);
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
  "Return charset of CHAR.")
  (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch, 0);

  return CHARSET_SYMBOL (CHAR_CHARSET (XINT (ch)));
}

DEFUN ("charset-after", Fcharset_after, Scharset_after, 0, 1, 0,
  "Return charset of a character in the current buffer at position POS.\n\
If POS is nil, it defauls to the current point.\n\
If POS is out of range, the value is nil.")
  (pos)
     Lisp_Object pos;
{
  register int pos_byte, bytes, charset, c1, c2;
  register unsigned char *p;

  if (NILP (pos))
    pos_byte = PT_BYTE;
  else if (MARKERP (pos))
    {
      pos_byte = marker_byte_position (pos);
      if (pos_byte < BEGV_BYTE || pos_byte >= ZV_BYTE)
	return Qnil;
    }
  else
    {
      CHECK_NUMBER (pos, 0);
      if (XINT (pos) < BEGV || XINT (pos) >= ZV)
	return Qnil;
      pos_byte = CHAR_TO_BYTE (XINT (pos));
    }
  p = BYTE_POS_ADDR (pos_byte);
  if (BASE_LEADING_CODE_P (*p))
    {
      SPLIT_MULTIBYTE_SEQ (p, Z_BYTE - pos_byte, bytes, charset, c1, c2);
      if (charset < 0)
	charset = 1;
    }
  else
    charset = CHARSET_ASCII;

  return CHARSET_SYMBOL (charset);
}

DEFUN ("iso-charset", Fiso_charset, Siso_charset, 3, 3, 0,
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.\n\
\n\
ISO 2022's designation sequence (escape sequence) distinguishes charsets\n\
by their DIMENSION, CHARS, and FINAL-CHAR,\n\
where as Emacs distinguishes them by charset symbol.\n\
See the documentation of the function `charset-info' for the meanings of\n\
DIMENSION, CHARS, and FINAL-CHAR.")
  (dimension, chars, final_char)
     Lisp_Object dimension, chars, final_char;
{
  int charset;

  CHECK_NUMBER (dimension, 0);
  CHECK_NUMBER (chars, 1);
  CHECK_NUMBER (final_char, 2);

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

  if (c < 0)
    return 0;
  if (SINGLE_BYTE_CHAR_P (c))
    return 1;
  SPLIT_NON_ASCII_CHAR (c, charset, c1, c2);
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
  "Return t if OBJECT is a valid normal character.\n\
If optional arg GENERICP is non-nil, also return t if OBJECT is\n\
a valid generic character.")
  (object, genericp)
     Lisp_Object object, genericp;
{
  if (! NATNUMP (object))
    return Qnil;
  return (CHAR_VALID_P (XFASTINT (object), !NILP (genericp)) ? Qt : Qnil);
}

DEFUN ("unibyte-char-to-multibyte", Funibyte_char_to_multibyte,
       Sunibyte_char_to_multibyte, 1, 1, 0,
  "Convert the unibyte character CH to multibyte character.\n\
The conversion is done based on `nonascii-translation-table' (which see)\n\
 or `nonascii-insert-offset' (which see).")
  (ch)
     Lisp_Object ch;
{
  int c;

  CHECK_NUMBER (ch, 0);
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
  "Convert the multibyte character CH to unibyte character.\n\
The conversion is done based on `nonascii-translation-table' (which see)\n\
 or `nonascii-insert-offset' (which see).")
  (ch)
     Lisp_Object ch;
{
  int c;

  CHECK_NUMBER (ch, 0);
  c = XINT (ch);
  if (! CHAR_VALID_P (c, 0))
    error ("Invalid multibyte character: %d", c);
  c = multibyte_char_to_unibyte (c, Qnil);
  if (c < 0)
    error ("Can't convert to unibyte character: %d", XINT (ch));
  return make_number (c);
}

DEFUN ("char-bytes", Fchar_bytes, Schar_bytes, 1, 1, 0,
  "Return 1 regardless of the argument CHAR.\n\
This is now an obsolete function.  We keep it just for backward compatibility.")
  (ch)
     Lisp_Object ch;
{
  Lisp_Object val;

  CHECK_NUMBER (ch, 0);
  return make_number (1);
}

/* Return how many bytes C will occupy in a multibyte buffer.
   Don't call this function directly, instead use macro CHAR_BYTES.  */
int
char_bytes (c)
     int c;
{
  int charset;

  if (SINGLE_BYTE_CHAR_P (c) || (c & ~GLYPH_MASK_CHAR))
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
  "Return width of CHAR when displayed in the current buffer.\n\
The width is measured by how many columns it occupies on the screen.")
  (ch)
       Lisp_Object ch;
{
  Lisp_Object val, disp;
  int c;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  CHECK_NUMBER (ch, 0);

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
  unsigned char *endp = str + len;
  int width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (str < endp)
    {
      Lisp_Object disp;
      int thislen;
      int c = STRING_CHAR_AND_LENGTH (str, endp - str, thislen);

      /* Get the way the display table would display it.  */
      if (dp)
	disp = DISP_CHAR_VECTOR (dp, c);
      else
	disp = Qnil;

      if (VECTORP (disp))
	width += XVECTOR (disp)->size;
      else
	width += ONE_BYTE_CHAR_WIDTH (*str);

      str += thislen;
    }
  return width;
}

DEFUN ("string-width", Fstring_width, Sstring_width, 1, 1, 0,
  "Return width of STRING when displayed in the current buffer.\n\
Width is measured by how many columns it occupies on the screen.\n\
When calculating width of a multibyte character in STRING,\n\
only the base leading-code is considered; the validity of\n\
the following bytes is not checked.")
  (str)
     Lisp_Object str;
{
  Lisp_Object val;

  CHECK_STRING (str, 0);
  XSETFASTINT (val, strwidth (XSTRING (str)->data,
			      STRING_BYTES (XSTRING (str))));
  return val;
}

DEFUN ("char-direction", Fchar_direction, Schar_direction, 1, 1, 0,
  "Return the direction of CHAR.\n\
The returned value is 0 for left-to-right and 1 for right-to-left.")
  (ch)
     Lisp_Object ch;
{
  int charset;

  CHECK_NUMBER (ch, 0);
  charset = CHAR_CHARSET (XFASTINT (ch));
  if (!CHARSET_DEFINED_P (charset))
    invalid_character (XINT (ch));
  return CHARSET_TABLE_INFO (charset, CHARSET_DIRECTION_IDX);
}

DEFUN ("chars-in-region", Fchars_in_region, Schars_in_region, 2, 2, 0,
  "Return number of characters between BEG and END.")
  (beg, end)
     Lisp_Object beg, end;
{
  int from, to;

  CHECK_NUMBER_COERCE_MARKER (beg, 0);
  CHECK_NUMBER_COERCE_MARKER (end, 1);

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
      if (BASE_LEADING_CODE_P (*ptr))
	{
	  PARSE_MULTIBYTE_SEQ (ptr, nbytes, bytes);
	  ptr += bytes;
	  nbytes -= bytes;
	}
      else
	{
	  ptr++;
	  nbytes--;
	}
      chars++;
    }

  return chars;
}

DEFUN ("string", Fstring, Sstring, 1, MANY, 0,
  "Concatenate all the argument characters and make the result a string.")
  (n, args)
     int n;
     Lisp_Object *args;
{
  int i;
  unsigned char *buf = (unsigned char *) alloca (MAX_MULTIBYTE_LENGTH * n);
  unsigned char *p = buf;
  Lisp_Object val;
  int c;

  for (i = 0; i < n; i++)
    {
      if (!INTEGERP (args[i]))
	CHECK_NUMBER (args[i], 0);
      c = XINT (args[i]);
      p += CHAR_STRING (c, p);
    }

  /* Here, we can't use make_string_from_bytes because of byte
     combining problem.  */
  val = make_string (buf, p - buf);
  return val;
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
       Ssetup_special_charsets, 0, 0, 0, "Internal use only.")
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
    BYTES_BY_CHAR_HEAD (i) = 1;
  for (i = MIN_CHARSET_OFFICIAL_DIMENSION1;
       i <= MAX_CHARSET_OFFICIAL_DIMENSION1; i++)
    BYTES_BY_CHAR_HEAD (i) = 2;
  for (i = MIN_CHARSET_OFFICIAL_DIMENSION2;
       i <= MAX_CHARSET_OFFICIAL_DIMENSION2; i++)
    BYTES_BY_CHAR_HEAD (i) = 3;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_11) = 3;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_12) = 3;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_21) = 4;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_22) = 4;
  /* The followings don't reflect the actual bytes, but just to tell
     that it is a start of a multibyte character.  */
  BYTES_BY_CHAR_HEAD (0x80) = 2;
  BYTES_BY_CHAR_HEAD (0x9E) = 2;
  BYTES_BY_CHAR_HEAD (0x9F) = 2;

  for (i = 0; i < 128; i++)
    WIDTH_BY_CHAR_HEAD (i) = 1;
  for (; i < 256; i++)
    WIDTH_BY_CHAR_HEAD (i) = 4;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_11) = 1;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_12) = 2;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_21) = 1;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_22) = 2;

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
  Qascii = intern ("ascii");
  staticpro (&Qascii);

  Qcharset = intern ("charset");
  staticpro (&Qcharset);

  /* Define ASCII charset now.  */
  update_charset_table (make_number (CHARSET_ASCII),
			make_number (1), make_number (94),
			make_number (1),
			make_number (0),
			make_number ('B'),
			make_number (0),
			build_string ("ASCII"),
			build_string ("ASCII"),
			build_string ("ASCII (ISO646 IRV)"));
  CHARSET_SYMBOL (CHARSET_ASCII) = Qascii;
  Fput (Qascii, Qcharset, CHARSET_TABLE_ENTRY (CHARSET_ASCII));

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
    "List of charsets ever defined.");
  Vcharset_list = Fcons (Qascii, Qnil);

  DEFVAR_LISP ("translation-table-vector",  &Vtranslation_table_vector,
    "Vector of cons cell of a symbol and translation table ever defined.\n\
An ID of a translation table is an index of this vector.");
  Vtranslation_table_vector = Fmake_vector (make_number (16), Qnil);

  DEFVAR_INT ("leading-code-private-11", &leading_code_private_11,
    "Leading-code of private TYPE9N charset of column-width 1.");
  leading_code_private_11 = LEADING_CODE_PRIVATE_11;

  DEFVAR_INT ("leading-code-private-12", &leading_code_private_12,
    "Leading-code of private TYPE9N charset of column-width 2.");
  leading_code_private_12 = LEADING_CODE_PRIVATE_12;

  DEFVAR_INT ("leading-code-private-21", &leading_code_private_21,
    "Leading-code of private TYPE9Nx9N charset of column-width 1.");
  leading_code_private_21 = LEADING_CODE_PRIVATE_21;

  DEFVAR_INT ("leading-code-private-22", &leading_code_private_22,
    "Leading-code of private TYPE9Nx9N charset of column-width 2.");
  leading_code_private_22 = LEADING_CODE_PRIVATE_22;

  DEFVAR_INT ("nonascii-insert-offset", &nonascii_insert_offset,
    "Offset for converting non-ASCII unibyte codes 0240...0377 to multibyte.\n\
This is used for converting unibyte text to multibyte,\n\
and for inserting character codes specified by number.\n\n\
This serves to convert a Latin-1 or similar 8-bit character code\n\
to the corresponding Emacs multibyte character code.\n\
Typically the value should be (- (make-char CHARSET 0) 128),\n\
for your choice of character set.\n\
If `nonascii-translation-table' is non-nil, it overrides this variable.");
  nonascii_insert_offset = 0;

  DEFVAR_LISP ("nonascii-translation-table", &Vnonascii_translation_table,
    "Translation table to convert non-ASCII unibyte codes to multibyte.\n\
This is used for converting unibyte text to multibyte,\n\
and for inserting character codes specified by number.\n\n\
Conversion is performed only when multibyte characters are enabled,\n\
and it serves to convert a Latin-1 or similar 8-bit character code\n\
to the corresponding Emacs character code.\n\n\
If this is nil, `nonascii-insert-offset' is used instead.\n\
See also the docstring of `make-translation-table'.");
  Vnonascii_translation_table = Qnil;

  DEFVAR_LISP ("auto-fill-chars", &Vauto_fill_chars,
    "A char-table for characters which invoke auto-filling.\n\
Such characters has value t in this table.");
  Vauto_fill_chars = Fmake_char_table (Qauto_fill_chars, Qnil);
  CHAR_TABLE_SET (Vauto_fill_chars, make_number (' '), Qt);
  CHAR_TABLE_SET (Vauto_fill_chars, make_number ('\n'), Qt);
}

#endif /* emacs */
