/* Header for multibyte character handler.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
     Licensed to the Free Software Foundation.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

#ifndef EMACS_CHARACTER_H
#define EMACS_CHARACTER_H

#include <verify.h>
#include "lisp.h"

INLINE_HEADER_BEGIN

/* character code	1st byte   byte sequence
   --------------	--------   -------------
        0-7F		00..7F	   0xxxxxxx
       80-7FF		C2..DF	   110xxxxx 10xxxxxx
      800-FFFF		E0..EF	   1110xxxx 10xxxxxx 10xxxxxx
    10000-1FFFFF	F0..F7	   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
   200000-3FFF7F	F8	   11111000 1000xxxx 10xxxxxx 10xxxxxx 10xxxxxx
   3FFF80-3FFFFF	C0..C1	   1100000x 10xxxxxx (for eight-bit-char)
   400000-...		invalid

   invalid 1st byte	80..BF	   10xxxxxx
			F9..FF	   11111xxx (xxx != 000)
*/

/* Maximum character code ((1 << CHARACTERBITS) - 1).  */
#define MAX_CHAR  0x3FFFFF

/* Maximum Unicode character code.  */
#define MAX_UNICODE_CHAR 0x10FFFF

/* Maximum N-byte character codes.  */
#define MAX_1_BYTE_CHAR 0x7F
#define MAX_2_BYTE_CHAR 0x7FF
#define MAX_3_BYTE_CHAR 0xFFFF
#define MAX_4_BYTE_CHAR 0x1FFFFF
#define MAX_5_BYTE_CHAR 0x3FFF7F

/* Minimum leading code of multibyte characters.  */
#define MIN_MULTIBYTE_LEADING_CODE 0xC0
/* Maximum leading code of multibyte characters.  */
#define MAX_MULTIBYTE_LEADING_CODE 0xF8

/* Unicode character values.  */
enum
{
  NO_BREAK_SPACE = 0x00A0,
  SOFT_HYPHEN = 0x00AD,
  ZERO_WIDTH_NON_JOINER = 0x200C,
  ZERO_WIDTH_JOINER = 0x200D,
  HYPHEN = 0x2010,
  NON_BREAKING_HYPHEN = 0x2011,
  LEFT_SINGLE_QUOTATION_MARK = 0x2018,
  RIGHT_SINGLE_QUOTATION_MARK = 0x2019,
  PARAGRAPH_SEPARATOR = 0x2029,
  LEFT_POINTING_ANGLE_BRACKET = 0x2329,
  RIGHT_POINTING_ANGLE_BRACKET = 0x232A,
  LEFT_ANGLE_BRACKET = 0x3008,
  RIGHT_ANGLE_BRACKET = 0x3009,
  OBJECT_REPLACEMENT_CHARACTER = 0xFFFC,
};

/* UTF-8 encodings.  Use \x escapes, so they are portable to pre-C11
   compilers and can be concatenated with ordinary string literals.  */
#define uLSQM "\xE2\x80\x98" /* U+2018 LEFT SINGLE QUOTATION MARK */
#define uRSQM "\xE2\x80\x99" /* U+2019 RIGHT SINGLE QUOTATION MARK */

/* Nonzero iff C is a character that corresponds to a raw 8-bit
   byte.  */
#define CHAR_BYTE8_P(c) ((c) > MAX_5_BYTE_CHAR)

/* Return the character code for raw 8-bit byte BYTE.  */
#define BYTE8_TO_CHAR(byte) ((byte) + 0x3FFF00)

#define UNIBYTE_TO_CHAR(byte) \
  (ASCII_CHAR_P (byte) ? (byte) : BYTE8_TO_CHAR (byte))

/* Return the raw 8-bit byte for character C.  */
#define CHAR_TO_BYTE8(c) (CHAR_BYTE8_P (c) ? (c) - 0x3FFF00 : (c & 0xFF))

/* Return the raw 8-bit byte for character C,
   or -1 if C doesn't correspond to a byte.  */
#define CHAR_TO_BYTE_SAFE(c)						\
  (ASCII_CHAR_P (c) ? c : (CHAR_BYTE8_P (c) ? (c) - 0x3FFF00 : -1))

/* Nonzero iff BYTE is the 1st byte of a multibyte form of a character
   that corresponds to a raw 8-bit byte.  */
#define CHAR_BYTE8_HEAD_P(byte) ((byte) == 0xC0 || (byte) == 0xC1)

/* If C is not ASCII, make it unibyte. */
#define MAKE_CHAR_UNIBYTE(c)	\
  do {				\
    if (! ASCII_CHAR_P (c))	\
      c = CHAR_TO_BYTE8 (c);	\
  } while (false)


/* If C is not ASCII, make it multibyte.  Assumes C < 256.  */
#define MAKE_CHAR_MULTIBYTE(c) \
  (eassert ((c) >= 0 && (c) < 256), (c) = UNIBYTE_TO_CHAR (c))

/* This is the maximum byte length of multibyte form.  */
#define MAX_MULTIBYTE_LENGTH 5

/* Nonzero iff X is a character.  */
#define CHARACTERP(x) (NATNUMP (x) && XFASTINT (x) <= MAX_CHAR)

/* Nonzero iff C is valid as a character code.  */
#define CHAR_VALID_P(c) UNSIGNED_CMP (c, <=, MAX_CHAR)

/* Check if Lisp object X is a character or not.  */
#define CHECK_CHARACTER(x) \
  CHECK_TYPE (CHARACTERP (x), Qcharacterp, x)

#define CHECK_CHARACTER_CAR(x) \
  do {					\
    Lisp_Object tmp = XCAR (x);		\
    CHECK_CHARACTER (tmp);		\
  } while (false)

#define CHECK_CHARACTER_CDR(x) \
  do {					\
    Lisp_Object tmp = XCDR (x);		\
    CHECK_CHARACTER (tmp);		\
  } while (false)

/* Nonzero iff C is a character of code less than 0x100.  */
#define SINGLE_BYTE_CHAR_P(c) UNSIGNED_CMP (c, <, 0x100)

/* Nonzero if character C has a printable glyph.  */
#define CHAR_PRINTABLE_P(c)	\
  (((c) >= 32 && (c) < 127)	\
   || ! NILP (CHAR_TABLE_REF (Vprintable_chars, (c))))

/* Return byte length of multibyte form for character C.  */
#define CHAR_BYTES(c)			\
  ( (c) <= MAX_1_BYTE_CHAR ? 1		\
    : (c) <= MAX_2_BYTE_CHAR ? 2	\
    : (c) <= MAX_3_BYTE_CHAR ? 3	\
    : (c) <= MAX_4_BYTE_CHAR ? 4	\
    : (c) <= MAX_5_BYTE_CHAR ? 5	\
    : 2)


/* Return the leading code of multibyte form of C.  */
#define CHAR_LEADING_CODE(c)				\
  ((c) <= MAX_1_BYTE_CHAR ? c				\
   : (c) <= MAX_2_BYTE_CHAR ? (0xC0 | ((c) >> 6))	\
   : (c) <= MAX_3_BYTE_CHAR ? (0xE0 | ((c) >> 12))	\
   : (c) <= MAX_4_BYTE_CHAR ? (0xF0 | ((c) >> 18))	\
   : (c) <= MAX_5_BYTE_CHAR ? 0xF8			\
   : (0xC0 | (((c) >> 6) & 0x01)))


/* Store multibyte form of the character C in P.  The caller should
   allocate at least MAX_MULTIBYTE_LENGTH bytes area at P in advance.
   Returns the length of the multibyte form.  */

#define CHAR_STRING(c, p)			\
  (UNSIGNED_CMP (c, <=, MAX_1_BYTE_CHAR)	\
   ? ((p)[0] = (c),				\
      1)					\
   : UNSIGNED_CMP (c, <=, MAX_2_BYTE_CHAR)	\
   ? ((p)[0] = (0xC0 | ((c) >> 6)),		\
      (p)[1] = (0x80 | ((c) & 0x3F)),		\
      2)					\
   : UNSIGNED_CMP (c, <=, MAX_3_BYTE_CHAR)	\
   ? ((p)[0] = (0xE0 | ((c) >> 12)),		\
      (p)[1] = (0x80 | (((c) >> 6) & 0x3F)),	\
      (p)[2] = (0x80 | ((c) & 0x3F)),		\
      3)					\
   : verify_expr (sizeof (c) <= sizeof (unsigned), char_string (c, p)))

/* Store multibyte form of byte B in P.  The caller should allocate at
   least MAX_MULTIBYTE_LENGTH bytes area at P in advance.  Returns the
   length of the multibyte form.  */

#define BYTE8_STRING(b, p)			\
  ((p)[0] = (0xC0 | (((b) >> 6) & 0x01)),	\
   (p)[1] = (0x80 | ((b) & 0x3F)),		\
   2)


/* Store multibyte form of the character C in P and advance P to the
   end of the multibyte form.  The caller should allocate at least
   MAX_MULTIBYTE_LENGTH bytes area at P in advance.  */

#define CHAR_STRING_ADVANCE(c, p)		\
  do {						\
    if ((c) <= MAX_1_BYTE_CHAR)			\
      *(p)++ = (c);				\
    else if ((c) <= MAX_2_BYTE_CHAR)		\
      *(p)++ = (0xC0 | ((c) >> 6)),		\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else if ((c) <= MAX_3_BYTE_CHAR)		\
      *(p)++ = (0xE0 | ((c) >> 12)),		\
	*(p)++ = (0x80 | (((c) >> 6) & 0x3F)),	\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else					\
      {						\
	verify (sizeof (c) <= sizeof (unsigned));	\
	(p) += char_string (c, p);		\
      }						\
  } while (false)


/* Nonzero iff BYTE starts a non-ASCII character in a multibyte
   form.  */
#define LEADING_CODE_P(byte) (((byte) & 0xC0) == 0xC0)

/* Nonzero iff BYTE is a trailing code of a non-ASCII character in a
   multibyte form.  */
#define TRAILING_CODE_P(byte) (((byte) & 0xC0) == 0x80)

/* Nonzero iff BYTE starts a character in a multibyte form.
   This is equivalent to:
	(ASCII_CHAR_P (byte) || LEADING_CODE_P (byte))  */
#define CHAR_HEAD_P(byte) (((byte) & 0xC0) != 0x80)

/* How many bytes a character that starts with BYTE occupies in a
   multibyte form.  Unlike MULTIBYTE_LENGTH below, this macro does not
   validate the multibyte form, but looks only at its first byte.  */
#define BYTES_BY_CHAR_HEAD(byte)	\
  (!((byte) & 0x80) ? 1			\
   : !((byte) & 0x20) ? 2		\
   : !((byte) & 0x10) ? 3		\
   : !((byte) & 0x08) ? 4		\
   : 5)


/* The byte length of multibyte form at unibyte string P ending at
   PEND.  If the string doesn't point to a valid multibyte form,
   return 0.  Unlike BYTES_BY_CHAR_HEAD, this macro validates the
   multibyte form.  */

#define MULTIBYTE_LENGTH(p, pend)				\
  (p >= pend ? 0						\
   : !((p)[0] & 0x80) ? 1					\
   : ((p + 1 >= pend) || (((p)[1] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xE0) == 0xC0 ? 2				\
   : ((p + 2 >= pend) || (((p)[2] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xF0) == 0xE0 ? 3				\
   : ((p + 3 >= pend) || (((p)[3] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xF8) == 0xF0 ? 4				\
   : ((p + 4 >= pend) || (((p)[4] & 0xC0) != 0x80)) ? 0		\
   : (p)[0] == 0xF8 && ((p)[1] & 0xF0) == 0x80 ? 5		\
   : 0)


/* Like MULTIBYTE_LENGTH, but don't check the ending address.  The
   multibyte form is still validated, unlike BYTES_BY_CHAR_HEAD.  */

#define MULTIBYTE_LENGTH_NO_CHECK(p)			\
  (!((p)[0] & 0x80) ? 1					\
   : ((p)[1] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xE0) == 0xC0 ? 2			\
   : ((p)[2] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xF0) == 0xE0 ? 3			\
   : ((p)[3] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xF8) == 0xF0 ? 4			\
   : ((p)[4] & 0xC0) != 0x80 ? 0			\
   : (p)[0] == 0xF8 && ((p)[1] & 0xF0) == 0x80 ? 5	\
   : 0)

/* If P is before LIMIT, advance P to the next character boundary.
   Assumes that P is already at a character boundary of the same
   multibyte form whose end address is LIMIT.  */

#define NEXT_CHAR_BOUNDARY(p, limit)	\
  do {					\
    if ((p) < (limit))			\
      (p) += BYTES_BY_CHAR_HEAD (*(p));	\
  } while (false)


/* If P is after LIMIT, advance P to the previous character boundary.
   Assumes that P is already at a character boundary of the same
   multibyte form whose beginning address is LIMIT.  */

#define PREV_CHAR_BOUNDARY(p, limit)					\
  do {									\
    if ((p) > (limit))							\
      {									\
	const unsigned char *chp = (p);					\
	do {								\
	  chp--;							\
	} while (chp >= limit && ! CHAR_HEAD_P (*chp));			\
	(p) = (BYTES_BY_CHAR_HEAD (*chp) == (p) - chp) ? chp : (p) - 1;	\
      }									\
  } while (false)

/* Return the character code of character whose multibyte form is at P.  */

#define STRING_CHAR(p)						\
  (!((p)[0] & 0x80)						\
   ? (p)[0]							\
   : ! ((p)[0] & 0x20)						\
   ? (((((p)[0] & 0x1F) << 6)					\
       | ((p)[1] & 0x3F))					\
      + (((unsigned char) (p)[0]) < 0xC2 ? 0x3FFF80 : 0))	\
   : ! ((p)[0] & 0x10)						\
   ? ((((p)[0] & 0x0F) << 12)					\
      | (((p)[1] & 0x3F) << 6)					\
      | ((p)[2] & 0x3F))					\
   : string_char ((p), NULL, NULL))


/* Like STRING_CHAR, but set ACTUAL_LEN to the length of multibyte
   form.  */

#define STRING_CHAR_AND_LENGTH(p, actual_len)			\
  (!((p)[0] & 0x80)						\
   ? ((actual_len) = 1, (p)[0])					\
   : ! ((p)[0] & 0x20)						\
   ? ((actual_len) = 2,						\
      (((((p)[0] & 0x1F) << 6)					\
	| ((p)[1] & 0x3F))					\
       + (((unsigned char) (p)[0]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((actual_len) = 3,						\
      ((((p)[0] & 0x0F) << 12)					\
       | (((p)[1] & 0x3F) << 6)					\
       | ((p)[2] & 0x3F)))					\
   : string_char ((p), NULL, &actual_len))


/* Like STRING_CHAR, but advance P to the end of multibyte form.  */

#define STRING_CHAR_ADVANCE(p)					\
  (!((p)[0] & 0x80)						\
   ? *(p)++							\
   : ! ((p)[0] & 0x20)						\
   ? ((p) += 2,							\
      ((((p)[-2] & 0x1F) << 6)					\
       | ((p)[-1] & 0x3F)					\
       | ((unsigned char) ((p)[-2]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((p) += 3,							\
      ((((p)[-3] & 0x0F) << 12)					\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F)))					\
   : string_char ((p), &(p), NULL))


/* Fetch the "next" character from Lisp string STRING at byte position
   BYTEIDX, character position CHARIDX.  Store it into OUTPUT.

   All the args must be side-effect-free.
   BYTEIDX and CHARIDX must be lvalues;
   we increment them past the character fetched.  */

#define FETCH_STRING_CHAR_ADVANCE(OUTPUT, STRING, CHARIDX, BYTEIDX)	\
  do                                                                    \
    {									\
      CHARIDX++;							\
      if (STRING_MULTIBYTE (STRING))					\
	{								\
	  unsigned char *chp = &SDATA (STRING)[BYTEIDX];		\
	  int chlen;							\
									\
	  OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);			\
	  BYTEIDX += chlen;						\
	}								\
      else								\
	{								\
	  OUTPUT = SREF (STRING, BYTEIDX);				\
	  BYTEIDX++;							\
	}								\
    }									\
  while (false)

/* Like FETCH_STRING_CHAR_ADVANCE, but return a multibyte character
   even if STRING is unibyte.  */

#define FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE(OUTPUT, STRING, CHARIDX, BYTEIDX) \
  do                                                                          \
    {									      \
      CHARIDX++;							      \
      if (STRING_MULTIBYTE (STRING))					      \
	{								      \
	  unsigned char *chp = &SDATA (STRING)[BYTEIDX];		      \
	  int chlen;							      \
									      \
	  OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);			      \
	  BYTEIDX += chlen;						      \
	}								      \
      else								      \
	{								      \
	  OUTPUT = SREF (STRING, BYTEIDX);				      \
	  BYTEIDX++;							      \
	  MAKE_CHAR_MULTIBYTE (OUTPUT);					      \
	}								      \
    }									      \
  while (false)


/* Like FETCH_STRING_CHAR_ADVANCE, but assumes STRING is multibyte.  */

#define FETCH_STRING_CHAR_ADVANCE_NO_CHECK(OUTPUT, STRING, CHARIDX, BYTEIDX) \
  do    								     \
    {									     \
      unsigned char *fetch_ptr = &SDATA (STRING)[BYTEIDX];		     \
      int fetch_len;							     \
									     \
      OUTPUT = STRING_CHAR_AND_LENGTH (fetch_ptr, fetch_len);		     \
      BYTEIDX += fetch_len;						     \
      CHARIDX++;							     \
    }									     \
  while (false)


/* Like FETCH_STRING_CHAR_ADVANCE, but fetch character from the current
   buffer.  */

#define FETCH_CHAR_ADVANCE(OUTPUT, CHARIDX, BYTEIDX)		\
  do    							\
    {								\
      CHARIDX++;						\
      if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	{							\
	  unsigned char *chp = BYTE_POS_ADDR (BYTEIDX);		\
	  int chlen;						\
								\
	  OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);		\
	  BYTEIDX += chlen;					\
	}							\
      else							\
	{							\
	  OUTPUT = *(BYTE_POS_ADDR (BYTEIDX));			\
	  BYTEIDX++;						\
	}							\
    }								\
  while (false)


/* Like FETCH_CHAR_ADVANCE, but assumes the current buffer is multibyte.  */

#define FETCH_CHAR_ADVANCE_NO_CHECK(OUTPUT, CHARIDX, BYTEIDX)	\
  do    							\
    {								\
      unsigned char *chp = BYTE_POS_ADDR (BYTEIDX);		\
      int chlen;							\
								\
      OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);		\
      BYTEIDX += chlen;						\
      CHARIDX++;						\
    }								\
  while (false)


/* Increment the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  No range checking of POS.  */

#define INC_POS(pos_byte)				\
  do {							\
    unsigned char *chp = BYTE_POS_ADDR (pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*chp);		\
  } while (false)


/* Decrement the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS.  */

#define DEC_POS(pos_byte)			\
  do {						\
    unsigned char *chp;				\
    						\
    pos_byte--;					\
    if (pos_byte < GPT_BYTE)			\
      chp = BEG_ADDR + pos_byte - BEG_BYTE;	\
    else					\
      chp = BEG_ADDR + GAP_SIZE + pos_byte - BEG_BYTE; \
    while (!CHAR_HEAD_P (*chp))			\
      {						\
	chp--;					\
	pos_byte--;				\
      }						\
  } while (false)

/* Increment both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define INC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)++;						\
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	(bytepos)++;						\
      else							\
	INC_POS ((bytepos));					\
    }								\
  while (false)


/* Decrement both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define DEC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)--;						\
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	(bytepos)--;						\
      else							\
	DEC_POS ((bytepos));					\
    }								\
  while (false)


/* Increment the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  This macro relies on the fact that
   *GPT_ADDR and *Z_ADDR are always accessible and the values are
   '\0'.  No range checking of POS_BYTE.  */

#define BUF_INC_POS(buf, pos_byte)				\
  do {								\
    unsigned char *chp = BUF_BYTE_ADDRESS (buf, pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*chp);			\
  } while (false)


/* Decrement the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS_BYTE.  */

#define BUF_DEC_POS(buf, pos_byte)					\
  do {									\
    unsigned char *chp;							\
    pos_byte--;								\
    if (pos_byte < BUF_GPT_BYTE (buf))					\
      chp = BUF_BEG_ADDR (buf) + pos_byte - BEG_BYTE;			\
    else								\
      chp = BUF_BEG_ADDR (buf) + BUF_GAP_SIZE (buf) + pos_byte - BEG_BYTE;\
    while (!CHAR_HEAD_P (*chp))						\
      {									\
	chp--;								\
	pos_byte--;							\
      }									\
  } while (false)


/* Return a non-outlandish value for the tab width.  */

#define SANE_TAB_WIDTH(buf) \
  sanitize_tab_width (XFASTINT (BVAR (buf, tab_width)))
INLINE int
sanitize_tab_width (EMACS_INT width)
{
  return 0 < width && width <= 1000 ? width : 8;
}

/* Return the width of ASCII character C.  The width is measured by
   how many columns C will occupy on the screen when displayed in the
   current buffer.  */

#define ASCII_CHAR_WIDTH(c)						\
  (c < 0x20								\
   ? (c == '\t'								\
      ? SANE_TAB_WIDTH (current_buffer)					\
      : (c == '\n' ? 0 : (NILP (BVAR (current_buffer, ctl_arrow)) ? 4 : 2)))	\
   : (c < 0x7f								\
      ? 1								\
      : ((NILP (BVAR (current_buffer, ctl_arrow)) ? 4 : 2))))

/* Return a non-outlandish value for a character width.  */

INLINE int
sanitize_char_width (EMACS_INT width)
{
  return 0 <= width && width <= 1000 ? width : 1000;
}

/* Return the width of character C.  The width is measured by how many
   columns C will occupy on the screen when displayed in the current
   buffer.  The name CHARACTER_WIDTH avoids a collision with <limits.h>
   CHAR_WIDTH when enabled; see ISO/IEC TS 18661-1:2014.  */

#define CHARACTER_WIDTH(c)	\
  (ASCII_CHAR_P (c)		\
   ? ASCII_CHAR_WIDTH (c)	\
   : sanitize_char_width (XINT (CHAR_TABLE_REF (Vchar_width_table, c))))

/* If C is a variation selector, return the index of the
   variation selector (1..256).  Otherwise, return 0.  */

#define CHAR_VARIATION_SELECTOR_P(c)		\
  ((c) < 0xFE00 ? 0				\
   : (c) <= 0xFE0F ? (c) - 0xFE00 + 1		\
   : (c) < 0xE0100 ? 0				\
   : (c) <= 0xE01EF ? (c) - 0xE0100 + 17	\
   : 0)

/* Return true if C is a surrogate.  */

INLINE bool
char_surrogate_p (int c)
{
  return 0xD800 <= c && c <= 0xDFFF;
}

/* Data type for Unicode general category.

   The order of members must be in sync with the 8th element of the
   member of unidata-prop-alist (in admin/unidata/unidata-gen.el) for
   Unicode character property `general-category'.  */

typedef enum {
  UNICODE_CATEGORY_UNKNOWN = 0,
  UNICODE_CATEGORY_Lu,
  UNICODE_CATEGORY_Ll,
  UNICODE_CATEGORY_Lt,
  UNICODE_CATEGORY_Lm,
  UNICODE_CATEGORY_Lo,
  UNICODE_CATEGORY_Mn,
  UNICODE_CATEGORY_Mc,
  UNICODE_CATEGORY_Me,
  UNICODE_CATEGORY_Nd,
  UNICODE_CATEGORY_Nl,
  UNICODE_CATEGORY_No,
  UNICODE_CATEGORY_Pc,
  UNICODE_CATEGORY_Pd,
  UNICODE_CATEGORY_Ps,
  UNICODE_CATEGORY_Pe,
  UNICODE_CATEGORY_Pi,
  UNICODE_CATEGORY_Pf,
  UNICODE_CATEGORY_Po,
  UNICODE_CATEGORY_Sm,
  UNICODE_CATEGORY_Sc,
  UNICODE_CATEGORY_Sk,
  UNICODE_CATEGORY_So,
  UNICODE_CATEGORY_Zs,
  UNICODE_CATEGORY_Zl,
  UNICODE_CATEGORY_Zp,
  UNICODE_CATEGORY_Cc,
  UNICODE_CATEGORY_Cf,
  UNICODE_CATEGORY_Cs,
  UNICODE_CATEGORY_Co,
  UNICODE_CATEGORY_Cn
} unicode_category_t;

extern EMACS_INT char_resolve_modifier_mask (EMACS_INT) ATTRIBUTE_CONST;
extern int char_string (unsigned, unsigned char *);
extern int string_char (const unsigned char *,
                        const unsigned char **, int *);

extern int translate_char (Lisp_Object, int c);
extern ptrdiff_t count_size_as_multibyte (const unsigned char *, ptrdiff_t);
extern ptrdiff_t str_as_multibyte (unsigned char *, ptrdiff_t, ptrdiff_t,
				   ptrdiff_t *);
extern ptrdiff_t str_to_multibyte (unsigned char *, ptrdiff_t, ptrdiff_t);
extern ptrdiff_t str_as_unibyte (unsigned char *, ptrdiff_t);
extern ptrdiff_t str_to_unibyte (const unsigned char *, unsigned char *,
                                 ptrdiff_t);
extern ptrdiff_t strwidth (const char *, ptrdiff_t);
extern ptrdiff_t c_string_width (const unsigned char *, ptrdiff_t, int,
				 ptrdiff_t *, ptrdiff_t *);
extern ptrdiff_t lisp_string_width (Lisp_Object, ptrdiff_t,
				    ptrdiff_t *, ptrdiff_t *);

extern Lisp_Object Vchar_unify_table;
extern Lisp_Object string_escape_byte8 (Lisp_Object);

extern bool alphabeticp (int);
extern bool alphanumericp (int);
extern bool graphicp (int);
extern bool printablep (int);
extern bool blankp (int);

extern bool confusable_symbol_character_p (int ch);

/* Return a translation table of id number ID.  */
#define GET_TRANSLATION_TABLE(id) \
  (XCDR (XVECTOR (Vtranslation_table_vector)->contents[(id)]))

/* Look up the element in char table OBJ at index CH, and return it as
   an integer.  If the element is not a character, return CH itself.  */

INLINE int
char_table_translate (Lisp_Object obj, int ch)
{
  /* This internal function is expected to be called with valid arguments,
     so there is a eassert instead of CHECK_xxx for the sake of speed.  */
  eassert (CHAR_VALID_P (ch));
  eassert (CHAR_TABLE_P (obj));
  obj = CHAR_TABLE_REF (obj, ch);
  return CHARACTERP (obj) ? XINT (obj) : ch;
}

#if defined __GNUC__ && !defined __STRICT_ANSI__
# define HEXDIGIT_CONST const
# define HEXDIGIT_IS_CONST true
#else
# define HEXDIGIT_CONST
# define HEXDIGIT_IS_CONST false
#endif
extern signed char HEXDIGIT_CONST hexdigit[];

/* If C is a hexadecimal digit ('0'-'9', 'a'-'f', 'A'-'F'), return its
   value (0-15).  Otherwise return -1.  */

INLINE int
char_hexdigit (int c)
{
  return 0 <= c && c <= UCHAR_MAX ? hexdigit[c] : -1;
}

INLINE_HEADER_END

#endif /* EMACS_CHARACTER_H */
