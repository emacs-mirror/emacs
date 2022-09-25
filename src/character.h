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
       80-7FF		C2..DF	   110yyyyx 10xxxxxx
      800-FFFF		E0..EF	   1110yyyy 10yxxxxx 10xxxxxx
    10000-1FFFFF	F0..F7	   11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
   200000-3FFF7F	F8	   11111000 1000yxxx 10xxxxxx 10xxxxxx 10xxxxxx
   3FFF80-3FFFFF	C0..C1	   1100000x 10xxxxxx (for eight-bit-char)
   400000-...		invalid

   invalid 1st byte	80..BF	   10xxxxxx
			F9..FF	   11111yyy

   In each bit pattern, 'x' and 'y' each represent a single bit of the
   character code payload, and at least one 'y' must be a 1 bit.
   In the 5-byte sequence, the 22-bit payload cannot exceed 3FFF7F.
*/

/* Maximum character code ((1 << CHARACTERBITS) - 1).  */
enum { MAX_CHAR = 0x3FFFFF };

/* Maximum Unicode character code.  */
enum { MAX_UNICODE_CHAR = 0x10FFFF };

/* Maximum N-byte character codes.  */
enum { MAX_1_BYTE_CHAR = 0x7F };
enum { MAX_2_BYTE_CHAR = 0x7FF };
enum { MAX_3_BYTE_CHAR = 0xFFFF };
enum { MAX_4_BYTE_CHAR = 0x1FFFFF };
enum { MAX_5_BYTE_CHAR = 0x3FFF7F };

/* Minimum leading code of multibyte characters.  */
enum { MIN_MULTIBYTE_LEADING_CODE = 0xC0 };
/* Maximum leading code of multibyte characters.  Note: this must be
   updated if we ever increase MAX_CHAR above.  */
enum { MAX_MULTIBYTE_LEADING_CODE = 0xF8 };

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
  TAG_SPACE = 0xE0020,
  CANCEL_TAG = 0xE007F,
};

extern int char_string (unsigned, unsigned char *);

/* UTF-8 encodings.  Use \x escapes, so they are portable to pre-C11
   compilers and can be concatenated with ordinary string literals.  */
#define uLSQM "\xE2\x80\x98" /* U+2018 LEFT SINGLE QUOTATION MARK */
#define uRSQM "\xE2\x80\x99" /* U+2019 RIGHT SINGLE QUOTATION MARK */

/* True iff C is a character of code less than 0x100.  */
INLINE bool
SINGLE_BYTE_CHAR_P (intmax_t c)
{
  return 0 <= c && c < 0x100;
}

/* True iff C is a character that corresponds to a raw 8-bit
   byte.  */
INLINE bool
CHAR_BYTE8_P (int c)
{
  return MAX_5_BYTE_CHAR < c;
}

/* Return the character code for raw 8-bit byte BYTE.  */
INLINE int
BYTE8_TO_CHAR (int byte)
{
  return byte + 0x3FFF00;
}

INLINE int
UNIBYTE_TO_CHAR (int byte)
{
  return ASCII_CHAR_P (byte) ? byte : BYTE8_TO_CHAR (byte);
}

/* Return the raw 8-bit byte for character C.  */
INLINE int
CHAR_TO_BYTE8 (int c)
{
  return CHAR_BYTE8_P (c) ? c - 0x3FFF00 : c & 0xFF;
}

/* Return the raw 8-bit byte for character C,
   or -1 if C doesn't correspond to a byte.  */
INLINE int
CHAR_TO_BYTE_SAFE (int c)
{
  return ASCII_CHAR_P (c) ? c : CHAR_BYTE8_P (c) ? c - 0x3FFF00 : -1;
}

/* True iff BYTE is the 1st byte of a multibyte form of a character
   that corresponds to a raw 8-bit byte.  */
INLINE bool
CHAR_BYTE8_HEAD_P (int byte)
{
  return byte == 0xC0 || byte == 0xC1;
}

/* If C is not ASCII, make it multibyte.  Assumes C < 256.  */
INLINE int
make_char_multibyte (int c)
{
  eassert (SINGLE_BYTE_CHAR_P (c));
  return UNIBYTE_TO_CHAR (c);
}

/* This is the maximum byte length of multibyte form.  */
enum { MAX_MULTIBYTE_LENGTH = 5 };

/* Nonzero iff C is valid as a character code.  */
INLINE bool
CHAR_VALID_P (intmax_t c)
{
  return 0 <= c && c <= MAX_CHAR;
}

/* Nonzero iff X is a character.  */
INLINE bool
CHARACTERP (Lisp_Object x)
{
  return FIXNUMP (x) && CHAR_VALID_P (XFIXNUM (x));
}

/* Check if Lisp object X is a character or not.  */
INLINE void
CHECK_CHARACTER (Lisp_Object x)
{
  CHECK_TYPE (CHARACTERP (x), Qcharacterp, x);
}

INLINE void
CHECK_CHARACTER_CAR (Lisp_Object x)
{
  CHECK_CHARACTER (XCAR (x));
}

INLINE void
CHECK_CHARACTER_CDR (Lisp_Object x)
{
  CHECK_CHARACTER (XCDR (x));
}

/* True if character C has a printable glyph.  */
INLINE bool
CHAR_PRINTABLE_P (int c)
{
  return ((32 <= c && c < 127)
	  || ! NILP (CHAR_TABLE_REF (Vprintable_chars, c)));
}

/* Return byte length of multibyte form for character C.  */
INLINE int
CHAR_BYTES (int c)
{
  return ((MAX_5_BYTE_CHAR < c ? -2 : 1)
	  + (MAX_1_BYTE_CHAR < c)
	  + (MAX_2_BYTE_CHAR < c)
	  + (MAX_3_BYTE_CHAR < c)
	  + (MAX_4_BYTE_CHAR < c));
}

/* Return the leading code of multibyte form of C.  */
INLINE int
CHAR_LEADING_CODE (int c)
{
  return (c <= MAX_1_BYTE_CHAR ? c
	  : c <= MAX_2_BYTE_CHAR ? 0xC0 | (c >> 6)
	  : c <= MAX_3_BYTE_CHAR ? 0xE0 | (c >> 12)
	  : c <= MAX_4_BYTE_CHAR ? 0xF0 | (c >> 18)
	  : c <= MAX_5_BYTE_CHAR ? 0xF8
	  : 0xC0 | ((c >> 6) & 0x01));
}


/* Store multibyte form of the character C in P.  The caller should
   allocate at least MAX_MULTIBYTE_LENGTH bytes area at P in advance.
   Returns the length of the multibyte form.  */

INLINE int
CHAR_STRING (int c, unsigned char *p)
{
  eassume (0 <= c);
  if (c <= MAX_1_BYTE_CHAR)
    {
      p[0] = c;
      return 1;
    }
  if (c <= MAX_2_BYTE_CHAR)
    {
      p[0] = 0xC0 | (c >> 6);
      p[1] = 0x80 | (c & 0x3F);
      return 2;
    }
  if (c <= MAX_3_BYTE_CHAR)
    {
      p[0] = 0xE0 | (c >> 12);
      p[1] = 0x80 | ((c >> 6) & 0x3F);
      p[2] = 0x80 | (c & 0x3F);
      return 3;
    }
  int len = char_string (c, p);
  eassume (0 < len && len <= MAX_MULTIBYTE_LENGTH);
  return len;
}

/* Store multibyte form of byte B in P.  The caller should allocate at
   least MAX_MULTIBYTE_LENGTH bytes area at P in advance.  Returns the
   length of the multibyte form.  */

INLINE int
BYTE8_STRING (int b, unsigned char *p)
{
  p[0] = 0xC0 | ((b >> 6) & 0x01);
  p[1] = 0x80 | (b & 0x3F);
  return 2;
}


/* True iff BYTE starts a non-ASCII character in a multibyte form.  */
INLINE bool
LEADING_CODE_P (int byte)
{
  return (byte & 0xC0) == 0xC0;
}

/* True iff BYTE is a trailing code of a non-ASCII character in a
   multibyte form.  */
INLINE bool
TRAILING_CODE_P (int byte)
{
  return (byte & 0xC0) == 0x80;
}

/* True iff BYTE starts a character in a multibyte form.
   This is equivalent to:
	(ASCII_CHAR_P (byte) || LEADING_CODE_P (byte))  */
INLINE bool
CHAR_HEAD_P (int byte)
{
  return (byte & 0xC0) != 0x80;
}

/* How many bytes a character that starts with BYTE occupies in a
   multibyte form.  Unlike multibyte_length, this function does not
   validate the multibyte form, but looks only at its first byte.  */
INLINE int
BYTES_BY_CHAR_HEAD (int byte)
{
  return (!(byte & 0x80) ? 1
	  : !(byte & 0x20) ? 2
	  : !(byte & 0x10) ? 3
	  : !(byte & 0x08) ? 4
	  : 5);
}


/* The byte length of the multibyte form at the unibyte string P,
   ending at PEND if CHECK, and without a length check if !CHECK.
   If ALLOW_8BIT, allow multibyte forms of eight-bit characters.
   If the string doesn't point to a valid multibyte form, return 0.
   Unlike BYTES_BY_CHAR_HEAD, this function validates the multibyte form.  */

INLINE int
multibyte_length (unsigned char const *p, unsigned char const *pend,
		  bool check, bool allow_8bit)
{
  if (!check || p < pend)
    {
      unsigned char c = p[0];
      if (c < 0x80)
	return 1;
      if (!check || p + 1 < pend)
	{
	  unsigned char d = p[1];
	  int w = ((d & 0xC0) << 2) + c;
	  if ((allow_8bit ? 0x2C0 : 0x2C2) <= w && w <= 0x2DF)
	    return 2;
	  if (!check || p + 2 < pend)
	    {
	      unsigned char e = p[2];
	      w += (e & 0xC0) << 4;
	      int w1 = w | ((d & 0x20) >> 2);
	      if (0xAE1 <= w1 && w1 <= 0xAEF)
		return 3;
	      if (!check || p + 3 < pend)
		{
		  unsigned char f = p[3];
		  w += (f & 0xC0) << 6;
		  int w2 = w | ((d & 0x30) >> 3);
		  if (0x2AF1 <= w2 && w2 <= 0x2AF7)
		    return 4;
		  if (!check || p + 4 < pend)
		    {
		      int_fast64_t lw = w + ((p[4] & 0xC0) << 8),
			w3 = (lw << 24) + (d << 16) + (e << 8) + f;
		      if (0xAAF8888080 <= w3 && w3 <= 0xAAF88FBFBD)
			return 5;
		    }
		}
	    }
	}
    }

  return 0;
}


/* Return number of bytes in the multibyte character just before P.
   Assumes that P is already at a character boundary of the same
   multibyte form, and is not at the start of that form.  */

INLINE int
raw_prev_char_len (unsigned char const *p)
{
  for (int len = 1; ; len++)
    if (CHAR_HEAD_P (p[-len]))
      return len;
}


/* Return the character code of character whose multibyte form is at P,
   and set *LENGTH to its length.  */

INLINE int
string_char_and_length (unsigned char const *p, int *length)
{
  int c = p[0];
  if (! (c & 0x80))
    {
      *length = 1;
      return c;
    }
  eassume (0xC0 <= c);

  int d = (c << 6) + p[1] - ((0xC0 << 6) + 0x80);
  if (! (c & 0x20))
    {
      *length = 2;
      return d + (c < 0xC2 ? 0x3FFF80 : 0);
    }

  d = (d << 6) + p[2] - ((0x20 << 12) + 0x80);
  if (! (c & 0x10))
    {
      *length = 3;
      eassume (MAX_2_BYTE_CHAR < d && d <= MAX_3_BYTE_CHAR);
      return d;
    }

  d = (d << 6) + p[3] - ((0x10 << 18) + 0x80);
  if (! (c & 0x08))
    {
      *length = 4;
      eassume (MAX_3_BYTE_CHAR < d && d <= MAX_4_BYTE_CHAR);
      return d;
    }

  d = (d << 6) + p[4] - ((0x08 << 24) + 0x80);
  *length = 5;
  eassume (MAX_4_BYTE_CHAR < d && d <= MAX_5_BYTE_CHAR);
  return d;
}

/* Return the character code of character whose multibyte form is at P.  */

INLINE int
STRING_CHAR (unsigned char const *p)
{
  int len;
  return string_char_and_length (p, &len);
}


/* Like STRING_CHAR (*PP), but advance *PP to the end of multibyte form.  */

INLINE int
string_char_advance (unsigned char const **pp)
{
  unsigned char const *p = *pp;
  int len, c = string_char_and_length (p, &len);
  *pp = p + len;
  return c;
}


/* Return the next character from Lisp string STRING at byte position
   *BYTEIDX, character position *CHARIDX.  Update *BYTEIDX and
   *CHARIDX past the character fetched.  */

INLINE int
fetch_string_char_advance (Lisp_Object string,
			   ptrdiff_t *charidx, ptrdiff_t *byteidx)
{
  int output;
  ptrdiff_t b = *byteidx;
  unsigned char *chp = SDATA (string) + b;
  if (STRING_MULTIBYTE (string))
    {
      int chlen;
      output = string_char_and_length (chp, &chlen);
      b += chlen;
    }
  else
    {
      output = *chp;
      b++;
    }
  (*charidx)++;
  *byteidx = b;
  return output;
}

/* Like fetch_string_char_advance, but return a multibyte character
   even if STRING is unibyte.  */

INLINE int
fetch_string_char_as_multibyte_advance (Lisp_Object string,
					ptrdiff_t *charidx, ptrdiff_t *byteidx)
{
  int output;
  ptrdiff_t b = *byteidx;
  unsigned char *chp = SDATA (string) + b;
  if (STRING_MULTIBYTE (string))
    {
      int chlen;
      output = string_char_and_length (chp, &chlen);
      b += chlen;
    }
  else
    {
      output = make_char_multibyte (*chp);
      b++;
    }
  (*charidx)++;
  *byteidx = b;
  return output;
}


/* Like fetch_string_char_advance, but assumes STRING is multibyte.  */

INLINE int
fetch_string_char_advance_no_check (Lisp_Object string,
				    ptrdiff_t *charidx, ptrdiff_t *byteidx)
{
  ptrdiff_t b = *byteidx;
  unsigned char *chp = SDATA (string) + b;
  int chlen, output = string_char_and_length (chp, &chlen);
  (*charidx)++;
  *byteidx = b + chlen;
  return output;
}


/* If C is a variation selector, return the index of the
   variation selector (1..256).  Otherwise, return 0.  */

INLINE int
CHAR_VARIATION_SELECTOR_P (int c)
{
  return (c < 0xFE00 ? 0
	  : c <= 0xFE0F ? c - 0xFE00 + 1
	  : c < 0xE0100 ? 0
	  : c <= 0xE01EF ? c - 0xE0100 + 17
	  : 0);
}

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

extern int translate_char (Lisp_Object, int c);
extern ptrdiff_t count_size_as_multibyte (const unsigned char *, ptrdiff_t);
extern ptrdiff_t str_as_multibyte (unsigned char *, ptrdiff_t, ptrdiff_t,
				   ptrdiff_t *);
extern ptrdiff_t str_to_multibyte (unsigned char *dst, const unsigned char *src,
				   ptrdiff_t nchars);
extern ptrdiff_t str_as_unibyte (unsigned char *, ptrdiff_t);
extern ptrdiff_t strwidth (const char *, ptrdiff_t);
extern ptrdiff_t c_string_width (const unsigned char *, ptrdiff_t, int,
				 ptrdiff_t *, ptrdiff_t *);
extern ptrdiff_t lisp_string_width (Lisp_Object, ptrdiff_t, ptrdiff_t,
				    ptrdiff_t, ptrdiff_t *, ptrdiff_t *, bool);

extern Lisp_Object Vchar_unify_table;
extern Lisp_Object string_escape_byte8 (Lisp_Object);

extern bool alphabeticp (int);
extern bool alphanumericp (int);
extern bool graphicp (int);
extern bool printablep (int);
extern bool blankp (int);
extern bool graphic_base_p (int);

/* Look up the element in char table OBJ at index CH, and return it as
   an integer.  If the element is not a character, return CH itself.  */

INLINE int
char_table_translate (Lisp_Object obj, int ch)
{
  /* This internal function is expected to be called with valid arguments,
     so there is an eassert instead of CHECK_xxx for the sake of speed.  */
  eassert (CHAR_VALID_P (ch));
  eassert (CHAR_TABLE_P (obj));
  obj = CHAR_TABLE_REF (obj, ch);
  return CHARACTERP (obj) ? XFIXNUM (obj) : ch;
}

extern signed char const hexdigit[];

/* If C is a hexadecimal digit ('0'-'9', 'a'-'f', 'A'-'F'), return its
   value (0-15).  Otherwise return -1.  */

INLINE int
char_hexdigit (int c)
{
  return 0 <= c && c <= UCHAR_MAX ? hexdigit[c] - 1 : -1;
}

INLINE_HEADER_END

#endif /* EMACS_CHARACTER_H */
