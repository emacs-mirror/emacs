/* Header for multibyte character handler.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
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

#ifndef EMACS_CHARACTER_H
#define EMACS_CHARACTER_H

/* character code	1st byte   byte sequence
   --------------	--------   -------------
        0-7F		00..7F	   0xxxxxxx
       80-7FF		C2..DF	   110xxxxx 10xxxxxx
      800-FFFF		E0..EF	   1110xxxx 10xxxxxx 10xxxxxx
    10000-1FFFFF	F0..F7	   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
   200000-3FFF7F	F8	   11111000 1000xxxx 10xxxxxx 10xxxxxx 10xxxxxx
      invalid		F9..FF

   raw-8-bit
   3FFF80-3FFFFF	C0..C1	   1100000x 10xxxxxx
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

/* Return the character code for raw 8-bit byte BYTE.  */
#define BYTE8_TO_CHAR(byte) ((byte) + 0x3FFF00)

/* Return the raw 8-bit byte for character C.  */
#define CHAR_TO_BYTE8(c) ((c) - 0x3FFF00)

/* Nonzero iff C is a character that corresponds to a raw 8-bit
   byte.  */
#define CHAR_BYTE8_P(c) ((c) > MAX_5_BYTE_CHAR)

/* Nonzero iff BYTE is the 1st byte of a multibyte form of a character
   that corresponds to a raw 8-bit byte.  */
#define CHAR_BYTE8_HEAD_P(byte) ((byte) == 0xC0 || (byte) == 0xC1)

/* This is the maximum byte length of multibyte form.  */
#define MAX_MULTIBYTE_LENGTH 5

/* Return a Lisp character whose character code is C. */
#define make_char(c) make_number (c)

/* Nonzero iff C is an ASCII byte.  */
#define ASCII_BYTE_P(c) ((unsigned) (c) < 0x80)

/* Nonzero iff X is a character.  */
#define CHARACTERP(x) (NATNUMP (x) && XFASTINT (x) <= MAX_CHAR)

/* Nozero iff C is valid as a charater code.  GENERICP is not used
   now.  */
#define CHAR_VALID_P(c, genericp) ((unsigned) (c) <= MAX_CHAR)

/* Check if Lisp object X is a character or not.  */
#define CHECK_CHARACTER(x)						\
  do {									\
    if (! CHARACTERP(x)) x = wrong_type_argument (Qcharacterp, (x));	\
  } while (0)

/* Nonzero iff C is an ASCII character.  */
#define ASCII_CHAR_P(c) ((unsigned) (c) < 0x80)

/* Nonzero iff C is a character of code less than 0x100.  */
#define SINGLE_BYTE_CHAR_P(c) ((unsigned) (c) < 0x100)

/* Nonzero if character C has a printable glyph.  */
#define CHAR_PRINTABLE_P(c)	\
  (((c) >= 32 && ((c) < 127)	\
    || ! NILP (CHAR_TABLE_REF (Vprintable_chars, (c)))))

/* Return byte length of multibyte form for character C.  */
#define CHAR_BYTES(c)			\
  ( (c) <= MAX_1_BYTE_CHAR ? 1		\
    : (c) <= MAX_2_BYTE_CHAR ? 2	\
    : (c) <= MAX_3_BYTE_CHAR ? 3	\
    : (c) <= MAX_4_BYTE_CHAR ? 4	\
    : (c) <= MAX_5_BYTE_CHAR ? 5	\
    : 2)

/* Store multibyte form of the character C in P.  The caller should
   allocate at least MAX_MULTIBYTE_LENGTH bytes area at P in advance.
   Returns the length of the multibyte form.  */

#define CHAR_STRING(c, p)			\
  ((unsigned) (c) <= MAX_1_BYTE_CHAR		\
   ? ((p)[0] = (c),				\
      1)					\
   : (unsigned) (c) <= MAX_2_BYTE_CHAR		\
   ? ((p)[0] = (0xC0 | ((c) >> 6)),		\
      (p)[1] = (0x80 | ((c) & 0x3F)),		\
      2)					\
   : (unsigned) (c) <= MAX_3_BYTE_CHAR		\
   ? ((p)[0] = (0xE0 | ((c) >> 12)),		\
      (p)[1] = (0x80 | (((c) >> 6) & 0x3F)),	\
      (p)[2] = (0x80 | ((c) & 0x3F)),		\
      3)					\
   : (unsigned) (c) <= MAX_5_BYTE_CHAR		\
   ? char_string_with_unification (c, p)	\
   : ((p)[0] = (0xC0 | (((c) >> 6) & 0x01)),	\
      (p)[1] = (0x80 | ((c) & 0x3F)),		\
      2))


/* Store multibyte form of the character C in P.  The caller should
   allocate at least MAX_MULTIBYTE_LENGTH bytes area at P in advance.
   And, advance P to the end of the multibyte form.  */

#define CHAR_STRING_ADVANCE(c, p)			\
  do {							\
    if ((c) <= MAX_1_BYTE_CHAR)				\
      *(p)++ = (c);					\
    else if ((c) <= MAX_2_BYTE_CHAR)			\
      *(p)++ = (0xC0 | ((c) >> 6)),			\
	*(p)++ = (0x80 | ((c) & 0x3F));			\
    else if ((c) <= MAX_3_BYTE_CHAR)			\
      *(p)++ = (0xE0 | ((c) >> 12)),			\
	*(p)++ = (0x80 | (((c) >> 6) & 0x3F)),		\
	*(p)++ = (0x80 | ((c) & 0x3F));			\
    else if ((c) <= MAX_5_BYTE_CHAR)			\
      (p) += char_string_with_unification ((c), (p));	\
    else						\
      *(p)++ = (0xC0 | (((c) >> 6) & 0x01)),		\
	*(p)++ = (0x80 | ((c) & 0x3F));			\
  } while (0)

/* Nonzero iff BYTE starts a non-ASCII character in a multibyte
   form.  */
#define LEADING_CODE_P(byte) (((byte) & 0xC0) == 0xC0)

/* Nonzero iff BYTE starts a character in a multibyte form.
   This is equivalent to:
	(ASCII_BYTE_P (byte) || LEADING_CODE_P (byte))  */
#define CHAR_HEAD_P(byte) (((byte) & 0xC0) != 0x80)

/* Just kept for backward compatibility.  This macro will be removed
   in the future.  */
#define BASE_LEADING_CODE_P LEADING_CODE_P

/* How many bytes a character that starts with BYTE occupies in a
   multibyte form.  */
#define BYTES_BY_CHAR_HEAD(byte)	\
  (!((byte) & 0x80) ? 1			\
   : !((byte) & 0x20) ? 2		\
   : !((byte) & 0x10) ? 3		\
   : !((byte) & 0x08) ? 4		\
   : 5)


/* Return the length of the multi-byte form at string STR of length
   LEN while assuming that STR points a valid multi-byte form.  As
   this macro isn't necessary anymore, all callers will be changed to
   use BYTES_BY_CHAR_HEAD directly in the future.  */

#define MULTIBYTE_FORM_LENGTH(str, len)		\
  BYTES_BY_CHAR_HEAD (*(str))

/* Parse multibyte string STR of length LENGTH and set BYTES to the
   byte length of a character at STR while assuming that STR points a
   valid multibyte form.  As this macro isn't necessary anymore, all
   callers will be changed to use BYTES_BY_CHAR_HEAD directly in the
   future.  */

#define PARSE_MULTIBYTE_SEQ(str, length, bytes)	\
  (bytes) = BYTES_BY_CHAR_HEAD (*(str))

/* The byte length of multibyte form at unibyte string P ending at
   PEND.  If STR doesn't point a valid multibyte form, return 0.  */

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


/* Like MULTIBYTE_LENGTH but don't check the ending address.  */

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


/* Return the character code of character whose multibyte form is at
   P.  The argument LEN is ignored.  It will be removed in the
   future.  */

#define STRING_CHAR(p, len)					\
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
   : string_char_with_unification ((p), NULL, NULL))


/* Like STRING_CHAR but set ACTUAL_LEN to the length of multibyte
   form.  The argument LEN is ignored.  It will be removed in the
   future.  */

#define STRING_CHAR_AND_LENGTH(p, len, actual_len)		\
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
   : string_char_with_unification ((p), NULL, &actual_len))


/* Like STRING_CHAR but advacen P to the end of multibyte form.  */

#define STRING_CHAR_ADVANCE(p)					\
  (!((p)[0] & 0x80)						\
   ? *(p)++							\
   : ! ((p)[0] & 0x20)						\
   ? ((p) += 2,							\
      ((((p)[-2] & 0x1F) << 6)					\
       | ((p)[-1] & 0x3F)					\
       | (((unsigned char) (p)[-2]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((p) += 3,							\
      ((((p)[-3] & 0x0F) << 12)					\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F)))					\
   : string_char_with_unification ((p), &(p), NULL))


/* Fetch the "next" character from Lisp string STRING at byte position
   BYTEIDX, character position CHARIDX.  Store it into OUTPUT.

   All the args must be side-effect-free.
   BYTEIDX and CHARIDX must be lvalues;
   we increment them past the character fetched.  */

#define FETCH_STRING_CHAR_ADVANCE(OUTPUT, STRING, CHARIDX, BYTEIDX)	\
  if (1)								\
    {									\
      CHARIDX++;							\
      if (STRING_MULTIBYTE (STRING))					\
	{								\
	  unsigned char *ptr = &XSTRING (STRING)->data[BYTEIDX];	\
	  int len;							\
									\
	  OUTPUT = STRING_CHAR_AND_LENGTH (ptr, 0, len);		\
	  BYTEIDX += len;						\
	}								\
      else								\
	OUTPUT = XSTRING (STRING)->data[BYTEIDX++];			\
    }									\
  else


/* Like FETCH_STRING_CHAR_ADVANCE but assumes STRING is multibyte.  */

#define FETCH_STRING_CHAR_ADVANCE_NO_CHECK(OUTPUT, STRING, CHARIDX, BYTEIDX) \
  if (1)								     \
    {									     \
      unsigned char *ptr = &XSTRING (STRING)->data[BYTEIDX];		     \
      int len;								     \
									     \
      OUTPUT = STRING_CHAR_AND_LENGTH (ptr, 0, len);			     \
      BYTEIDX += len;							     \
      CHARIDX++;							     \
    }									     \
  else


/* Like FETCH_STRING_CHAR_ADVANCE but fetch character from the current
   buffer.  */

#define FETCH_CHAR_ADVANCE(OUTPUT, CHARIDX, BYTEIDX)		\
  if (1)							\
    {								\
      CHARIDX++;						\
      if (!NILP (current_buffer->enable_multibyte_characters))	\
	{							\
	  unsigned char *ptr = BYTE_POS_ADDR (BYTEIDX);		\
	  int len;						\
								\
	  OUTPUT= STRING_CHAR_AND_LENGTH (ptr, 0, len);		\
	  BYTEIDX += len;					\
	}							\
      else							\
	{							\
	  OUTPUT = *(BYTE_POS_ADDR (BYTEIDX));			\
	  BYTEIDX++;						\
	}							\
    }								\
  else


/* Like FETCH_CHAR_ADVANCE but assumes STRING is multibyte.  */

#define FETCH_CHAR_ADVANCE_NO_CHECK(OUTPUT, CHARIDX, BYTEIDX)	\
  if (1)							\
    {								\
      unsigned char *ptr = BYTE_POS_ADDR (BYTEIDX);		\
      int len;							\
								\
      OUTPUT= STRING_CHAR_AND_LENGTH (ptr, 0, len);		\
      BYTEIDX += len;						\
      CHARIDX++;						\
    }								\
  else


/* Increase the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  No range checking of POS.  */

#define INC_POS(pos_byte)				\
  do {							\
    unsigned char *p = BYTE_POS_ADDR (pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*p);		\
  } while (0)


/* Decrease the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS.  */

#define DEC_POS(pos_byte)			\
  do {						\
    unsigned char *p;				\
    						\
    pos_byte--;					\
    if (pos_byte < GPT_BYTE)			\
      p = BEG_ADDR + pos_byte - 1;		\
    else					\
      p = BEG_ADDR + GAP_SIZE + pos_byte - 1;	\
    while (!CHAR_HEAD_P (*p))			\
      {						\
	p--;					\
	pos_byte--;				\
      }						\
  } while (0)

/* Increment both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define INC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)++;						\
      if (NILP (current_buffer->enable_multibyte_characters))	\
	(bytepos)++;						\
      else							\
	INC_POS ((bytepos));					\
    }								\
  while (0)


/* Decrement both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define DEC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)--;						\
      if (NILP (current_buffer->enable_multibyte_characters))	\
	(bytepos)--;						\
      else							\
	DEC_POS ((bytepos));					\
    }								\
  while (0)


/* Increase the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  This macro relies on the fact that
   *GPT_ADDR and *Z_ADDR are always accessible and the values are
   '\0'.  No range checking of POS_BYTE.  */

#define BUF_INC_POS(buf, pos_byte)				\
  do {								\
    unsigned char *p = BUF_BYTE_ADDRESS (buf, pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*p);			\
  } while (0)


/* Decrease the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS_BYTE.  */

#define BUF_DEC_POS(buf, pos_byte)					\
  do {									\
    unsigned char *p;							\
    pos_byte--;								\
    if (pos_byte < BUF_GPT_BYTE (buf))					\
      p = BUF_BEG_ADDR (buf) + pos_byte - 1;				\
    else								\
      p = BUF_BEG_ADDR (buf) + BUF_GAP_SIZE (buf) + pos_byte - 1;	\
    while (!CHAR_HEAD_P (*p))						\
      {									\
	p--;								\
	pos_byte--;							\
      }									\
  } while (0)


#define MAYBE_UNIFY_CHAR(c)				\
  if (CHAR_TABLE_P (Vchar_unify_table))			\
    {							\
      Lisp_Object val;					\
      int unified;					\
							\
      val = CHAR_TABLE_REF (Vchar_unify_table, c);	\
      if (SYMBOLP (val))				\
	{						\
	  Funify_charset (val, Qnil);			\
	  val = CHAR_TABLE_REF (Vchar_unify_table, c);	\
	}						\
      if ((unified = XINT (val)) >= 0)			\
	c = unified;					\
    }							\
  else

/* Return the width of ASCII character C.  The width is measured by
   how many columns occupied on the screen when displayed in the
   current buffer.  */

#define ASCII_CHAR_WIDTH(c)						\
  (c < 0x20								\
   ? (c == '\t'								\
      ? XFASTINT (current_buffer->tab_width)				\
      : (c == '\n' ? 0 : (NILP (current_buffer->ctl_arrow) ? 4 : 2)))	\
   : (c < 0x7f								\
      ? 1								\
      : ((NILP (current_buffer->ctl_arrow) ? 4 : 2))))

/* Return the width of character C.  The width is measured by how many
   columns occupied on the screen when displayed in the current
   buffer.  */

#define CHAR_WIDTH(c)		\
  (ASCII_CHAR_P (c)		\
   ? ASCII_CHAR_WIDTH (c)	\
   : XINT (CHAR_TABLE_REF (Vchar_width_table, c)))

extern int char_string_with_unification P_ ((int, unsigned char *));
extern int string_char_with_unification P_ ((unsigned char *,
					     unsigned char **, int *));

extern int translate_char P_ ((Lisp_Object, int c));
extern int char_printable_p P_ ((int c));
extern void parse_str_as_multibyte P_ ((unsigned char *, int, int *, int *));
extern int parse_str_to_multibyte P_ ((unsigned char *, int));
extern int str_as_multibyte P_ ((unsigned char *, int, int, int *));
extern int str_to_multibyte P_ ((unsigned char *, int, int));
extern int str_as_unibyte P_ ((unsigned char *, int));
extern int strwidth P_ ((unsigned char *, int));
extern int c_string_width P_ ((unsigned char *, int, int, int *, int *));
extern int lisp_string_width P_ ((Lisp_Object, int, int *, int *));

extern Lisp_Object Vprintable_chars;

extern Lisp_Object Qcharacterp, Qauto_fill_chars;
extern Lisp_Object Vtranslation_table_vector;
extern Lisp_Object Vchar_width_table;
extern Lisp_Object Vchar_direction_table;
extern Lisp_Object Vchar_unify_table;

/* Return a translation table of id number ID.  */
#define GET_TRANSLATION_TABLE(id) \
  (XCDR(XVECTOR(Vtranslation_table_vector)->contents[(id)]))

/* A char-table for characters which may invoke auto-filling.  */
extern Lisp_Object Vauto_fill_chars;

/* Copy LEN bytes from FROM to TO.  This macro should be used only
   when a caller knows that LEN is short and the obvious copy loop is
   faster than calling bcopy which has some overhead.  Copying a
   multibyte sequence of a character is the typical case.  */

#define BCOPY_SHORT(from, to, len)		\
  do {						\
    int i = len;				\
    unsigned char *from_p = from, *to_p = to;	\
    while (i--) *to_p++ = *from_p++;		\
  } while (0)

#define DEFSYM(sym, name)	\
  do { (sym) = intern ((name)); staticpro (&(sym)); } while (0)

#endif /* EMACS_CHARACTER_H */
