/* Multilingual characters handler.
   Ver.1.0
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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

#include <stdio.h>

#ifdef emacs

#include <sys/types.h>
#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* emacs */

Lisp_Object Qcharset, Qascii, Qcomposition;

/* Declaration of special leading-codes.  */
int leading_code_composition;	/* for composite characters */
int leading_code_private_11;	/* for private DIMENSION1 of 1-column */
int leading_code_private_12;	/* for private DIMENSION1 of 2-column */
int leading_code_private_21;	/* for private DIMENSION2 of 1-column */
int leading_code_private_22;	/* for private DIMENSION2 of 2-column */

/* Declaration of special charsets.  */
int charset_ascii;		/* ASCII */
int charset_composition;	/* for a composite character */
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

/* Tables used by macros BYTES_BY_CHAR_HEAD and WIDTH_BY_CHAR_HEAD.  */
int bytes_by_char_head[256];
int width_by_char_head[256];

/* Mapping table from ISO2022's charset (specified by DIMENSION,
   CHARS, and FINAL-CHAR) to Emacs' charset.  */
int iso_charset_table[2][2][128];

/* Table of pointers to the structure `cmpchar_info' indexed by
   CMPCHAR-ID.  */
struct cmpchar_info **cmpchar_table;
/* The current size of `cmpchar_table'.  */
static int cmpchar_table_size;
/* Number of the current composite characters.  */
int n_cmpchars;

/* Variables used locally in the macro FETCH_MULTIBYTE_CHAR.  */
unsigned char *_fetch_multibyte_char_p;
int _fetch_multibyte_char_len;

/* Set STR a pointer to the multi-byte form of the character C.  If C
   is not a composite character, the multi-byte form is set in WORKBUF
   and STR points WORKBUF.  The caller should allocate at least 4-byte
   area at WORKBUF in advance.  Returns the length of the multi-byte
   form.

   Use macro `CHAR_STRING (C, WORKBUF, STR)' instead of calling this
   function directly if C can be an ASCII character.  */

int
non_ascii_char_to_string (c, workbuf, str)
     int c;
     unsigned char *workbuf, **str;
{
  int charset;
  unsigned char c1, c2;

  if (COMPOSITE_CHAR_P (c))
    {
      int cmpchar_id = COMPOSITE_CHAR_ID (c);

      if (cmpchar_id < n_cmpchars)
	{
	  *str = cmpchar_table[cmpchar_id]->data;
	  return cmpchar_table[cmpchar_id]->len;
	}
      else
	{
	  *str = workbuf;
	  return 0;
	}
    }

  SPLIT_NON_ASCII_CHAR (c, charset, c1, c2);

  *str = workbuf;
  *workbuf++ = CHARSET_LEADING_CODE_BASE (charset);
  if (*workbuf = CHARSET_LEADING_CODE_EXT (charset))
    workbuf++;
  *workbuf++ = c1 | 0x80;
  if (c2)
    *workbuf++ = c2 | 0x80;

  return (workbuf - *str);
}

/* Return a non-ASCII character of which multi-byte form is at STR of
   length LEN.  If ACTUAL_LEN is not NULL, the actual length of the
   character is set to the address ACTUAL_LEN.

   Use macro `STRING_CHAR (STR, LEN)' instead of calling this function
   directly if STR can hold an ASCII character.  */

string_to_non_ascii_char (str, len, actual_len)
     unsigned char *str;
     int len, *actual_len;
{
  int charset;
  unsigned char c1, c2;
  register int c;

  if (SPLIT_STRING (str, len, charset, c1, c2) == CHARSET_ASCII)
    {
      if (actual_len)
	*actual_len = 1;
      return (int) *str;
    }

  c = MAKE_NON_ASCII_CHAR (charset, c1, c2);

  if (actual_len)
    *actual_len = (charset == CHARSET_COMPOSITION
		   ? cmpchar_table[COMPOSITE_CHAR_ID (c)]->len
		   : BYTES_BY_CHAR_HEAD (*str));
  return c;
}

/* Return the length of the multi-byte form at string STR of length LEN.  */
int
multibyte_form_length (str, len)
     unsigned char *str;
     int len;
{
  int charset;
  unsigned char c1, c2;
  register int c;

  if (SPLIT_STRING (str, len, charset, c1, c2) == CHARSET_ASCII)
    return 1;

  return (charset == CHARSET_COMPOSITION
	  ? cmpchar_table[(c1 << 7) | c2]->len
	  : BYTES_BY_CHAR_HEAD (*str));
}

/* Check if string STR of length LEN contains valid multi-byte form of
   a character.  If valid, charset and position codes of the character
   is set at *CHARSET, *C1, and *C2, and return 0.  If not valid,
   return -1.  This should be used only in the macro SPLIT_STRING
   which checks range of STR in advance.  */

split_non_ascii_string (str, len, charset, c1, c2)
     register unsigned char *str, *c1, *c2;
     register int len, *charset;
{
  register unsigned int cs = *str++;

  if (cs == LEADING_CODE_COMPOSITION)
    {
      int cmpchar_id = str_cmpchar_id (str - 1, len);

      if (cmpchar_id < 0)
	return -1;
      *charset = cs, *c1 = cmpchar_id >> 7, *c2 = cmpchar_id & 0x7F;
    }
  else if ((cs < LEADING_CODE_PRIVATE_11 || (cs = *str++) >= 0xA0)
	   && CHARSET_DEFINED_P (cs))
    {
      *charset = cs;
      if (*str < 0xA0)
	return -1;
      *c1 = (*str++) & 0x7F;
      if (CHARSET_DIMENSION (cs) == 2)
	{
	  if (*str < 0xA0)
	    return -1;
	  *c2 = (*str++) & 0x7F;
	}
    }
  else
    return -1;
  return 0;
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

  if (NILP (Faref (Vcharset_table, charset_id)))
    Faset (Vcharset_table, charset_id,
	   Fmake_vector (make_number (CHARSET_MAX_IDX), Qnil));

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
      /* Update tables bytes_by_char_head and width_by_char_head.  */
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
	from = LEADING_CODE_EXT_22, to = LEADING_CODE_EXT_MAX - 1;
    }

  for (charset = from; charset < to; charset++)
    if (!CHARSET_DEFINED_P (charset)) break;

  return make_number (charset < to ? charset : 0);
}

DEFUN ("define-charset", Fdefine_charset, Sdefine_charset, 3, 3, 0,
  "Define CHARSET-ID as the identification number of CHARSET with INFO-VECTOR.\n\
If CHARSET-ID is nil, it is set automatically, which means CHARSET is\n\
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
charset when rendering.  If 0, render from right to left, else\n\
render from left to right.\n\
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
  Fput (charset_symbol, Qcharset, Faref (Vcharset_table, charset_id));
  CHARSET_SYMBOL (XINT (charset_id)) = charset_symbol;
  Vcharset_list = Fcons (charset_symbol, Vcharset_list);
  return Qnil;
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
   caller should allocate CHARSETS (MAX_CHARSET + 1 bytes) in advance.  */

int
find_charset_in_str (str, len, charsets)
     unsigned char *str, *charsets;
     int len;
{
  int num = 0;

  while (len > 0)
    {
      int bytes = BYTES_BY_CHAR_HEAD (*str);
      int charset = CHARSET_AT (str);

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
       2, 2, 0,
  "Return a list of charsets in the region between BEG and END.\n\
BEG and END are buffer positions.")
  (beg, end)
     Lisp_Object beg, end;
{
  char charsets[MAX_CHARSET + 1];
  int from, to, stop, i;
  Lisp_Object val;

  validate_region (&beg, &end);
  from = XFASTINT (beg);
  stop = to = XFASTINT (end);
  if (from < GPT && GPT < to)
    stop = GPT;
  bzero (charsets, MAX_CHARSET + 1);
  while (1)
    {
      find_charset_in_str (POS_ADDR (from), stop - from, charsets);
      if (stop < to)
	from = stop, stop = to;
      else
	break;
    }
  val = Qnil;
  for (i = MAX_CHARSET; i >= 0; i--)
    if (charsets[i])
      val = Fcons (CHARSET_SYMBOL (i), val);
  return val;
}

DEFUN ("find-charset-string", Ffind_charset_string, Sfind_charset_string,
       1, 1, 0,
  "Return a list of charsets in STR.")
  (str)
     Lisp_Object str;
{
  char charsets[MAX_CHARSET + 1];
  int i;
  Lisp_Object val;

  CHECK_STRING (str, 0);
  bzero (charsets, MAX_CHARSET + 1);
  find_charset_in_str (XSTRING (str)->data, XSTRING (str)->size, charsets);
  val = Qnil;
  for (i = MAX_CHARSET; i >= 0; i--)
    if (charsets[i])
      val = Fcons (CHARSET_SYMBOL (i), val);
  return val;
}

DEFUN ("make-char-internal", Fmake_char_internal, Smake_char_internal, 1, 3, 0,
  "")
  (charset, code1, code2)
     Lisp_Object charset, code1, code2;
{
  CHECK_NUMBER (charset, 0);

  if (NILP (code1))
    XSETFASTINT (code1, 0);
  else
    CHECK_NUMBER (code1, 1);
  if (NILP (code2))
    XSETFASTINT (code2, 0);
  else
    CHECK_NUMBER (code2, 2);

  if (!CHARSET_DEFINED_P (XINT (charset)))
    error ("Invalid charset: %d", XINT (charset));

  return make_number (MAKE_CHAR (XINT (charset), XINT (code1), XINT (code2)));
}

DEFUN ("split-char", Fsplit_char, Ssplit_char, 1, 1, 0,
  "Return list of charset and one or two position-codes of CHAR.")
  (ch)
     Lisp_Object ch;
{
  Lisp_Object val;
  int charset;
  unsigned char c1, c2;

  CHECK_NUMBER (ch, 0);
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  return ((charset == CHARSET_COMPOSITION || CHARSET_DIMENSION (charset) == 2)
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

DEFUN ("iso-charset", Fiso_charset, Siso_charset, 3, 3, 0,
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.")
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

DEFUN ("char-bytes", Fchar_bytes, Schar_bytes, 1, 1, 0,
  "Return byte length of multi-byte form of CHAR.")
  (ch)
     Lisp_Object ch;
{
  Lisp_Object val;
  int bytes;

  CHECK_NUMBER (ch, 0);
  if (COMPOSITE_CHAR_P (XFASTINT (ch)))
    {
      unsigned int id = COMPOSITE_CHAR_ID (XFASTINT (ch));

      bytes = (id < n_cmpchars ? cmpchar_table[id]->len : 1);
    }
  else
    {
      int charset = CHAR_CHARSET (XFASTINT (ch));

      bytes = CHARSET_DEFINED_P (charset) ? CHARSET_BYTES (charset) : 1;
    }

  XSETFASTINT (val, bytes);
  return val;
}

/* Return the width of character of which multi-byte form starts with
   C.  The width is measured by how many columns occupied on the
   screen when displayed in the current buffer.  */

#define ONE_BYTE_CHAR_WIDTH(c)					     	\
  (c < 0x20							     	\
   ? (c == '\t'							     	\
      ? current_buffer->tab_width				     	\
      : (c == '\n' ? 0 : (NILP (current_buffer->ctl_arrow) ? 4 : 2)))	\
   : (c < 0x7f							     	\
      ? 1							     	\
      : (c == 0x7F						     	\
	 ? (NILP (current_buffer->ctl_arrow) ? 4 : 2)		     	\
	 : ((! NILP (current_buffer->enable_multibyte_characters)    	\
	     && BASE_LEADING_CODE_P (c))			     	\
	    ? WIDTH_BY_CHAR_HEAD (c)				     	\
	    : 4))))						     	\


DEFUN ("char-width", Fchar_width, Schar_width, 1, 1, 0,
  "Return width of CHAR when displayed in the current buffer.\n\
The width is measured by how many columns it occupies on the screen.")
  (ch)
       Lisp_Object ch;
{
  Lisp_Object val;
  int c;

  CHECK_NUMBER (ch, 0);

  c = XFASTINT (ch);
  if (SINGLE_BYTE_CHAR_P (c))
    XSETFASTINT (val, ONE_BYTE_CHAR_WIDTH (c));
  else if (COMPOSITE_CHAR_P (c))
    {
      int id = COMPOSITE_CHAR_ID (XFASTINT (ch));
      XSETFASTINT (val, (id < n_cmpchars ? cmpchar_table[id]->width : 0));
    }
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

  while (str < endp) {
    if (*str == LEADING_CODE_COMPOSITION)
      {
	int id = str_cmpchar_id (str, endp - str);

	if (id < 0)
	  {
	    width += 4;
	    str++;
	  }
	else
	  {
	    width += cmpchar_table[id]->width;
	    str += cmpchar_table[id]->len;
	  }
      }
    else
      {
	width += ONE_BYTE_CHAR_WIDTH (*str);
	str += BYTES_BY_CHAR_HEAD (*str);
      }
  }
  return width;
}

DEFUN ("string-width", Fstring_width, Sstring_width, 1, 1, 0,
  "Return width of STRING when displayed in the current buffer.\n\
Width is measured by how many columns it occupies on the screen.\n\
When calculating width of a multi-byte character in STRING,\n\
 only the base leading-code is considered and the validity of\n\
 the following bytes are not checked.")
  (str)
     Lisp_Object str;
{
  Lisp_Object val;

  CHECK_STRING (str, 0);
  XSETFASTINT (val, strwidth (XSTRING (str)->data, XSTRING (str)->size));
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
    error ("Invalid character: %d", XINT (ch));
  return CHARSET_TABLE_INFO (charset, CHARSET_DIRECTION_IDX);
}

DEFUN ("chars-in-string", Fchars_in_string, Schars_in_string, 1, 1, 0,
  "Return number of characters in STRING.")
  (str)
     Lisp_Object str;
{
  Lisp_Object val;
  unsigned char *p, *endp;
  int chars;

  CHECK_STRING (str, 0);

  p = XSTRING (str)->data; endp = p + XSTRING (str)->size;
  chars = 0;
  while (p < endp)
    {
      if (*p == LEADING_CODE_COMPOSITION)
	{
	  p++;
	  while (p < endp && ! CHAR_HEAD_P (p)) p++;
	}
      else
	p += BYTES_BY_CHAR_HEAD (*p);
      chars++;
    }

  XSETFASTINT (val, chars);
  return val;
}

DEFUN ("char-boundary-p", Fchar_boundary_p, Schar_boundary_p, 1, 1, 0,
  "Return non-nil value if POS is at character boundary of multibyte form.\n\
The return value is:\n\
 0 if POS is at an ASCII character or at the end of range,\n\
 1 if POS is at a head of 2-byte length multi-byte form,\n\
 2 if POS is at a head of 3-byte length multi-byte form,\n\
 3 if POS is at a head of 4-byte length multi-byte form,\n\
 4 if POS is at a head of multi-byte form of a composite character.\n\
If POS is out of range or not at character boundary, return NIL.")
  (pos)
     Lisp_Object pos;
{
  Lisp_Object val;
  int n;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  n = XINT (pos);
  if (n < BEGV || n > ZV)
    return Qnil;

  if (n == ZV || NILP (current_buffer->enable_multibyte_characters))
    XSETFASTINT (val, 0);
  else
    {
      unsigned char *p = POS_ADDR (n);

      if (SINGLE_BYTE_CHAR_P (*p))
	XSETFASTINT (val, 0);
      else if (*p == LEADING_CODE_COMPOSITION)
	XSETFASTINT (val, 4);
      else if (BYTES_BY_CHAR_HEAD (*p) > 1)
	XSETFASTINT (val, BYTES_BY_CHAR_HEAD (*p) - 1);
      else
	val = Qnil;
    }
  return val;
}

DEFUN ("concat-chars", Fconcat_chars, Sconcat_chars, 1, MANY, 0,
  "Concatenate all the argument characters and make the result a string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  int i, n = XINT (nargs);
  unsigned char *buf
    = (unsigned char *) malloc (MAX_LENGTH_OF_MULTI_BYTE_FORM * n);
  unsigned char *p = buf;
  Lisp_Object val;

  for (i = 0; i < n; i++)
    {
      int c, len;
      unsigned char *str;

      if (!INTEGERP (args[i]))
	{
	  free (buf);
	  CHECK_NUMBER (args[i], 0);
	}
      c = XINT (args[i]);
      len = CHAR_STRING (c, p, str);
      if (p != str)
	/* C is a composite character.  */
	bcopy (str, p, len);
      p += len;
    }

  val = make_string (buf, p - buf);
  free (buf);
  return val;
}

#endif /* emacs */

/*** Composite characters staffs ***/

/* Each composite character is identified by CMPCHAR-ID which is
   assigned when Emacs needs the character code of the composite
   character (e.g. when displaying it on the screen).  See the
   document "GENERAL NOTE on COMPOSITE CHARACTER" in `charset.h' how a
   composite character is represented in Emacs.  */

/* If `static' is defined, it means that it is defined to null string. */
#ifndef static
/* The following function is copied from lread.c.  */
static int
hash_string (ptr, len)
     unsigned char *ptr;
     int len;
{
  register unsigned char *p = ptr;
  register unsigned char *end = p + len;
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140) c -= 40;
      hash = ((hash<<3) + (hash>>28) + c);
    }
  return hash & 07777777777;
}
#endif

#define CMPCHAR_HASH_TABLE_SIZE 0xFFF

static int *cmpchar_hash_table[CMPCHAR_HASH_TABLE_SIZE];

/* Each element of `cmpchar_hash_table' is a pointer to an array of
   integer, where the 1st element is the size of the array, the 2nd
   element is how many elements are actually used in the array, and
   the remaining elements are CMPCHAR-IDs of composite characters of
   the same hash value.  */
#define CMPCHAR_HASH_SIZE(table) table[0]
#define CMPCHAR_HASH_USED(table) table[1]
#define CMPCHAR_HASH_CMPCHAR_ID(table, i) table[i]

/* Return CMPCHAR-ID of the composite character in STR of the length
   LEN.  If the composite character has not yet been registered,
   register it in `cmpchar_table' and assign new CMPCHAR-ID.  This
   is the sole function for assigning CMPCHAR-ID.  */
int
str_cmpchar_id (str, len)
     unsigned char *str;
     int len;
{
  int hash_idx, *hashp;
  unsigned char *buf;
  int embedded_rule;		/* 1 if composition rule is embedded.  */
  int chars;			/* number of components.  */
  int i;
  struct cmpchar_info *cmpcharp;

  if (len < 5)
    /* Any composite char have at least 3-byte length.  */
    return -1;

  /* The second byte 0xFF means compostion rule is embedded.  */
  embedded_rule = (str[1] == 0xFF);

  /* At first, get the actual length of the composite character.  */
  {
    unsigned char *p, *endp = str + 1, *lastp = str + len;
    int bytes;

    while (endp < lastp && ! CHAR_HEAD_P (endp)) endp++;
    chars = 0;
    p = str + 1 + embedded_rule;
    while (p < endp)
      {
	/* No need of checking if *P is 0xA0 because
	 BYTES_BY_CHAR_HEAD (0x80) surely returns 2.  */
	p += (bytes = BYTES_BY_CHAR_HEAD (*p - 0x20) + embedded_rule);
	chars++;
      }
    len = (p -= embedded_rule) - str;
    if (p > endp)
      len -= - bytes, chars--;

    if (chars < 2 || chars > MAX_COMPONENT_COUNT)
      /* Invalid number of components.  */
      return -1;
  }
  hash_idx = hash_string (str, len) % CMPCHAR_HASH_TABLE_SIZE;
  hashp = cmpchar_hash_table[hash_idx];

  /* Then, look into the hash table.  */
  if (hashp != NULL)
    /* Find the correct one among composite characters of the same
       hash value.  */
    for (i = 2; i < CMPCHAR_HASH_USED (hashp); i++)
      {
	cmpcharp = cmpchar_table[CMPCHAR_HASH_CMPCHAR_ID (hashp, i)];
	if (len == cmpcharp->len
	    && ! bcmp (str, cmpcharp->data, len))
	  return CMPCHAR_HASH_CMPCHAR_ID (hashp, i);
      }

  /* We have to register the composite character in cmpchar_table.  */
  if (n_cmpchars > (CHAR_FIELD2_MASK | CHAR_FIELD3_MASK))
    /* No, we have no more room for a new composite character.  */
    return -1;

  /* Make the entry in hash table.  */
  if (hashp == NULL)
    {
      /* Make a table for 8 composite characters initially.  */
      hashp = (cmpchar_hash_table[hash_idx]
	       = (int *) xmalloc (sizeof (int) * (2 + 8)));
      CMPCHAR_HASH_SIZE (hashp) = 10;
      CMPCHAR_HASH_USED (hashp) = 2;
    }
  else if (CMPCHAR_HASH_USED (hashp) >= CMPCHAR_HASH_SIZE (hashp))
    {
      CMPCHAR_HASH_SIZE (hashp) += 8;
      hashp = (cmpchar_hash_table[hash_idx]
	       = (int *) xrealloc (hashp, 
				   sizeof (int) * CMPCHAR_HASH_SIZE (hashp)));
    }
  CMPCHAR_HASH_CMPCHAR_ID (hashp, CMPCHAR_HASH_USED (hashp)) = n_cmpchars;
  CMPCHAR_HASH_USED (hashp)++;

  /* Set information of the composite character in cmpchar_table.  */
  if (cmpchar_table_size == 0)
    {
      /* This is the first composite character to be registered.  */
      cmpchar_table_size = 256;
      cmpchar_table
	= (struct cmpchar_info **) xmalloc (sizeof (cmpchar_table[0])
					    * cmpchar_table_size);
    }
  else if (cmpchar_table_size <= n_cmpchars)
    {
      cmpchar_table_size += 256;
      cmpchar_table
	= (struct cmpchar_info **) xrealloc (cmpchar_table,
					     sizeof (cmpchar_table[0])
					     * cmpchar_table_size);
    }

  cmpcharp = (struct cmpchar_info *) xmalloc (sizeof (struct cmpchar_info));

  cmpcharp->len = len;
  cmpcharp->data = (unsigned char *) xmalloc (len + 1);
  bcopy (str, cmpcharp->data, len);
  cmpcharp->data[len] = 0;
  cmpcharp->glyph_len = chars;
  cmpcharp->glyph = (GLYPH *) xmalloc (sizeof (GLYPH) * chars);
  if (embedded_rule)
    {
      cmpcharp->cmp_rule = (unsigned char *) xmalloc (chars);
      cmpcharp->col_offset = (float *) xmalloc (sizeof (float) * chars);
    }
  else
    {
      cmpcharp->cmp_rule = NULL;
      cmpcharp->col_offset = NULL;
    }

  /* Setup GLYPH data and composition rules (if any) so as not to make
     them every time on displaying.  */
  {
    unsigned char *bufp;
    int width;
    float leftmost = 0.0, rightmost = 1.0;

    if (embedded_rule)
      /* At first, col_offset[N] is set to relative to col_offset[0].  */
      cmpcharp->col_offset[0] = 0;

    for (i = 0, bufp = cmpcharp->data + 1; i < chars; i++)
      {
	if (embedded_rule)
	  cmpcharp->cmp_rule[i] = *bufp++;

	if (*bufp == 0xA0)	/* This is an ASCII character.  */
	  {
	    cmpcharp->glyph[i] = FAST_MAKE_GLYPH ((*++bufp & 0x7F), 0);
	    width = 1;
	    bufp++;
	  }
	else			/* Multibyte character.  */
	  {
	    /* Make `bufp' point normal multi-byte form temporally.  */
	    *bufp -= 0x20;
	    cmpcharp->glyph[i]
	      = FAST_MAKE_GLYPH (string_to_non_ascii_char (bufp, 4, 0), 0);
	    width = WIDTH_BY_CHAR_HEAD (*bufp);
	    *bufp += 0x20;
	    bufp += BYTES_BY_CHAR_HEAD (*bufp - 0x20);
	  }

	if (embedded_rule && i > 0)
	  {
	    /* Reference points (global_ref and new_ref) are
	       encoded as below:
	       
	       0--1--2 -- ascent
	       |     |
	       |     |
	       |  4 -+--- center
	    -- 3     5 -- baseline
	       |     |
	       6--7--8 -- descent

	       Now, we calculate the column offset of the new glyph
	       from the left edge of the first glyph.  This can avoid
	       the same calculation everytime displaying this
	       composite character.  */

	    /* Reference points of global glyph and new glyph.  */
	    int global_ref = (cmpcharp->cmp_rule[i] - 0xA0) / 9;
	    int new_ref = (cmpcharp->cmp_rule[i] - 0xA0) % 9;
	    /* Column offset relative to the first glyph.  */
	    float left = (leftmost
			  + (global_ref % 3) * (rightmost - leftmost) / 2.0
			  - (new_ref % 3) * width / 2.0);

	    cmpcharp->col_offset[i] = left;
	    if (left < leftmost)
	      leftmost = left;
	    if (left + width > rightmost)
	      rightmost = left + width;
	  }
	else
	  {
	    if (width > rightmost)
	      rightmost = width;
	  }
      }
    if (embedded_rule)
      {
	/* Now col_offset[N] are relative to the left edge of the
           first component.  Make them relative to the left edge of
           overall glyph.  */
	for (i = 0; i < chars; i++)
	  cmpcharp->col_offset[i] -= leftmost;
	/* Make rightmost holds width of overall glyph.  */
	rightmost -= leftmost;
      }

    cmpcharp->width = rightmost;
    if (cmpcharp->width < rightmost)
      /* To get a ceiling integer value.  */
      cmpcharp->width++;
  }

  cmpchar_table[n_cmpchars] = cmpcharp;

  return n_cmpchars++;
}

/* Return the Nth element of the composite character C.  */
int
cmpchar_component (c, n)
     unsigned int c, n;
{
  int id = COMPOSITE_CHAR_ID (c);

  if (id >= n_cmpchars		/* C is not a valid composite character.  */
      || n >= cmpchar_table[id]->glyph_len) /* No such component.  */
    return -1;
  /* No face data is stored in glyph code.  */
  return ((int) (cmpchar_table[id]->glyph[n]));
}

DEFUN ("cmpcharp", Fcmpcharp, Scmpcharp, 1, 1, 0,
  "T if CHAR is a composite character.")
  (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch, 0);
  return (COMPOSITE_CHAR_P (XINT (ch)) ? Qt : Qnil);
}

DEFUN ("composite-char-component", Fcmpchar_component, Scmpchar_component,
       2, 2, 0,
  "Return the IDXth component character of composite character CHARACTER.")
  (character, idx)
     Lisp_Object character, idx;
{
  int c;

  CHECK_NUMBER (character, 0);
  CHECK_NUMBER (idx, 1);

  if ((c = cmpchar_component (XINT (character), XINT (idx))) < 0)
    args_out_of_range (character, idx);

  return make_number (c);
}

DEFUN ("composite-char-composition-rule", Fcmpchar_cmp_rule, Scmpchar_cmp_rule,
       2, 2, 0,
  "Return the Nth composition rule embedded in composite character CHARACTER.\n\
The returned rule is for composing the Nth component\n\
on the (N-1)th component.  If N is 0, the returned value is always 255.")
  (character, n)
     Lisp_Object character, n;
{
  int id, i;

  CHECK_NUMBER (character, 0);
  CHECK_NUMBER (n, 1);

  id = COMPOSITE_CHAR_ID (XINT (character));
  if (id < 0 || id >= n_cmpchars)
    error ("Invalid composite character: %d", XINT (character));
  i = XINT (n);
  if (i > cmpchar_table[id]->glyph_len)
    args_out_of_range (character, n);

  return make_number (cmpchar_table[id]->cmp_rule[i]);
}

DEFUN ("composite-char-composition-rule-p", Fcmpchar_cmp_rule_p,
       Scmpchar_cmp_rule_p, 1, 1, 0,
  "Return non-nil if composite character CHARACTER contains a embedded rule.")
  (character)
     Lisp_Object character;
{
  int id;

  CHECK_NUMBER (character, 0);
  id = COMPOSITE_CHAR_ID (XINT (character));
  if (id < 0 || id >= n_cmpchars)
    error ("Invalid composite character: %d", XINT (character));

  return (cmpchar_table[id]->cmp_rule ? Qt : Qnil);
}

DEFUN ("composite-char-component-count", Fcmpchar_cmp_count,
       Scmpchar_cmp_count, 1, 1, 0,
  "Return number of compoents of composite character CHARACTER.")
  (character)
     Lisp_Object character;
{
  int id;

  CHECK_NUMBER (character, 0);
  id = COMPOSITE_CHAR_ID (XINT (character));
  if (id < 0 || id >= n_cmpchars)
    error ("Invalid composite character: %d", XINT (character));

  return (make_number (cmpchar_table[id]->glyph_len));
}

DEFUN ("compose-string", Fcompose_string, Scompose_string,
       1, 1, 0,
  "Return one char string composed from all characters in STRING.")
  (str)
     Lisp_Object str;
{
  unsigned char buf[MAX_LENGTH_OF_MULTI_BYTE_FORM], *p, *pend, *ptemp;
  int len, i;

  CHECK_STRING (str, 0);

  buf[0] = LEADING_CODE_COMPOSITION;
  p = XSTRING (str)->data;
  pend = p + XSTRING (str)->size;
  i = 1;
  while (p < pend)
    {
      if (*p < 0x20 || *p == 127) /* control code */
	error ("Invalid component character: %d", *p);
      else if (*p < 0x80)	/* ASCII */
	{
	  if (i + 2 >= MAX_LENGTH_OF_MULTI_BYTE_FORM)
	    error ("Too long string to be composed: %s", XSTRING (str)->data);
	  /* Prepend an ASCII charset indicator 0xA0, set MSB of the
             code itself.  */
	  buf[i++] = 0xA0;
	  buf[i++] = *p++ + 0x80;
	}
      else if (*p == LEADING_CODE_COMPOSITION) /* composite char */
	{
	  /* Already composed.  Eliminate the heading
             LEADING_CODE_COMPOSITION, keep the remaining bytes
             unchanged.  */
	  p++;
	  ptemp = p;
	  while (! CHAR_HEAD_P (p)) p++;
	  if (i + (p - ptemp) >= MAX_LENGTH_OF_MULTI_BYTE_FORM)
	    error ("Too long string to be composed: %s", XSTRING (str)->data);
	  bcopy (ptemp, buf + i, p - ptemp);
	  i += p - ptemp;
	}
      else			/* multibyte char */
	{
	  /* Add 0x20 to the base leading-code, keep the remaining
             bytes unchanged.  */
	  len = BYTES_BY_CHAR_HEAD (*p);
	  if (i + len >= MAX_LENGTH_OF_MULTI_BYTE_FORM)
	    error ("Too long string to be composed: %s", XSTRING (str)->data);
	  bcopy (p, buf + i, len);
	  buf[i] += 0x20;
	  p += len, i += len;
	}
    }

  if (i < 5)
    /* STR contains only one character, which can't be composed.  */
    error ("Too short string to be composed: %s", XSTRING (str)->data);

  return make_string (buf, i);
}


charset_id_internal (charset_name)
     char *charset_name;
{
  Lisp_Object val = Fget (intern (charset_name), Qcharset);

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

init_charset_once ()
{
  int i, j, k;

  staticpro (&Vcharset_table);
  staticpro (&Vcharset_symbol_table);

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

  Vcharset_symbol_table = Fmake_vector (make_number (MAX_CHARSET + 1), Qnil);

  /* Setup tables.  */
  for (i = 0; i < 2; i++)
    for (j = 0; j < 2; j++)
      for (k = 0; k < 128; k++)
	iso_charset_table [i][j][k] = -1;

  bzero (cmpchar_hash_table, sizeof cmpchar_hash_table);
  cmpchar_table_size = n_cmpchars = 0;

  for (i = 0; i < 256; i++)
    BYTES_BY_CHAR_HEAD (i) = 1;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_11) = 3;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_12) = 3;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_21) = 4;
  BYTES_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_22) = 4;
  /* The following doesn't reflect the actual bytes, but just to tell
     that it is a start of a multibyte character.  */
  BYTES_BY_CHAR_HEAD (LEADING_CODE_COMPOSITION) = 2;

  for (i = 0; i < 128; i++)
    WIDTH_BY_CHAR_HEAD (i) = 1;
  for (; i < 256; i++)
    WIDTH_BY_CHAR_HEAD (i) = 4;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_11) = 1;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_12) = 2;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_21) = 1;
  WIDTH_BY_CHAR_HEAD (LEADING_CODE_PRIVATE_22) = 2;
}

#ifdef emacs

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

  Qcomposition = intern ("composition");
  staticpro (&Qcomposition);
  CHARSET_SYMBOL (CHARSET_COMPOSITION) = Qcomposition;

  defsubr (&Sdefine_charset);
  defsubr (&Sdeclare_equiv_charset);
  defsubr (&Sfind_charset_region);
  defsubr (&Sfind_charset_string);
  defsubr (&Smake_char_internal);
  defsubr (&Ssplit_char);
  defsubr (&Schar_charset);
  defsubr (&Siso_charset);
  defsubr (&Schar_bytes);
  defsubr (&Schar_width);
  defsubr (&Sstring_width);
  defsubr (&Schar_direction);
  defsubr (&Schars_in_string);
  defsubr (&Schar_boundary_p);
  defsubr (&Sconcat_chars);
  defsubr (&Scmpcharp);
  defsubr (&Scmpchar_component);
  defsubr (&Scmpchar_cmp_rule);
  defsubr (&Scmpchar_cmp_rule_p);
  defsubr (&Scmpchar_cmp_count);
  defsubr (&Scompose_string);
  defsubr (&Ssetup_special_charsets);

  DEFVAR_LISP ("charset-list", &Vcharset_list,
    "List of charsets ever defined.");
  Vcharset_list = Fcons (Qascii, Qnil);

  DEFVAR_INT ("leading-code-composition", &leading_code_composition,
    "Leading-code of composite characters.");
  leading_code_composition = LEADING_CODE_COMPOSITION;

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
}

#endif /* emacs */
