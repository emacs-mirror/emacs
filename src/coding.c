/* Coding system handler (conversion, detection, and etc).
   Copyright (C) 1995, 1997, 1998, 2002 Electrotechnical Laboratory, JAPAN.
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

/*** TABLE OF CONTENTS ***

  0. General comments
  1. Preamble
  2. Emacs' internal format (emacs-mule) handlers
  3. ISO2022 handlers
  4. Shift-JIS and BIG5 handlers
  5. CCL handlers
  6. End-of-line handlers
  7. C library functions
  8. Emacs Lisp library functions
  9. Post-amble

*/

/*** 0. General comments ***/


/*** GENERAL NOTE on CODING SYSTEMS ***

  A coding system is an encoding mechanism for one or more character
  sets.  Here's a list of coding systems which Emacs can handle.  When
  we say "decode", it means converting some other coding system to
  Emacs' internal format (emacs-mule), and when we say "encode",
  it means converting the coding system emacs-mule to some other
  coding system.

  0. Emacs' internal format (emacs-mule)

  Emacs itself holds a multi-lingual character in buffers and strings
  in a special format.  Details are described in section 2.

  1. ISO2022

  The most famous coding system for multiple character sets.  X's
  Compound Text, various EUCs (Extended Unix Code), and coding
  systems used in Internet communication such as ISO-2022-JP are
  all variants of ISO2022.  Details are described in section 3.

  2. SJIS (or Shift-JIS or MS-Kanji-Code)

  A coding system to encode character sets: ASCII, JISX0201, and
  JISX0208.  Widely used for PC's in Japan.  Details are described in
  section 4.

  3. BIG5

  A coding system to encode the character sets ASCII and Big5.  Widely
  used for Chinese (mainly in Taiwan and Hong Kong).  Details are
  described in section 4.  In this file, when we write "BIG5"
  (all uppercase), we mean the coding system, and when we write
  "Big5" (capitalized), we mean the character set.

  4. Raw text

  A coding system for text containing random 8-bit code.  Emacs does
  no code conversion on such text except for end-of-line format.

  5. Other

  If a user wants to read/write text encoded in a coding system not
  listed above, he can supply a decoder and an encoder for it as CCL
  (Code Conversion Language) programs.  Emacs executes the CCL program
  while reading/writing.

  Emacs represents a coding system by a Lisp symbol that has a property
  `coding-system'.  But, before actually using the coding system, the
  information about it is set in a structure of type `struct
  coding_system' for rapid processing.  See section 6 for more details.

*/

/*** GENERAL NOTES on END-OF-LINE FORMAT ***

  How end-of-line of text is encoded depends on the operating system.
  For instance, Unix's format is just one byte of `line-feed' code,
  whereas DOS's format is two-byte sequence of `carriage-return' and
  `line-feed' codes.  MacOS's format is usually one byte of
  `carriage-return'.

  Since text character encoding and end-of-line encoding are
  independent, any coding system described above can have any
  end-of-line format.  So Emacs has information about end-of-line
  format in each coding-system.  See section 6 for more details.

*/

/*** GENERAL NOTES on `detect_coding_XXX ()' functions ***

  These functions check if a text between SRC and SRC_END is encoded
  in the coding system category XXX.  Each returns an integer value in
  which appropriate flag bits for the category XXX are set.  The flag
  bits are defined in macros CODING_CATEGORY_MASK_XXX.  Below is the
  template for these functions.  If MULTIBYTEP is nonzero, 8-bit codes
  of the range 0x80..0x9F are in multibyte form.  */
#if 0
int
detect_coding_emacs_mule (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  ...
}
#endif

/*** GENERAL NOTES on `decode_coding_XXX ()' functions ***

  These functions decode SRC_BYTES length of unibyte text at SOURCE
  encoded in CODING to Emacs' internal format.  The resulting
  multibyte text goes to a place pointed to by DESTINATION, the length
  of which should not exceed DST_BYTES.

  These functions set the information about original and decoded texts
  in the members `produced', `produced_char', `consumed', and
  `consumed_char' of the structure *CODING.  They also set the member
  `result' to one of CODING_FINISH_XXX indicating how the decoding
  finished.

  DST_BYTES zero means that the source area and destination area are
  overlapped, which means that we can produce a decoded text until it
  reaches the head of the not-yet-decoded source text.

  Below is a template for these functions.  */
#if 0
static void
decode_coding_XXX (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  ...
}
#endif

/*** GENERAL NOTES on `encode_coding_XXX ()' functions ***

  These functions encode SRC_BYTES length text at SOURCE from Emacs'
  internal multibyte format to CODING.  The resulting unibyte text
  goes to a place pointed to by DESTINATION, the length of which
  should not exceed DST_BYTES.

  These functions set the information about original and encoded texts
  in the members `produced', `produced_char', `consumed', and
  `consumed_char' of the structure *CODING.  They also set the member
  `result' to one of CODING_FINISH_XXX indicating how the encoding
  finished.

  DST_BYTES zero means that the source area and destination area are
  overlapped, which means that we can produce encoded text until it
  reaches at the head of the not-yet-encoded source text.

  Below is a template for these functions.  */
#if 0
static void
encode_coding_XXX (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  ...
}
#endif

/*** COMMONLY USED MACROS ***/

/* The following two macros ONE_MORE_BYTE and TWO_MORE_BYTES safely
   get one, two, and three bytes from the source text respectively.
   If there are not enough bytes in the source, they jump to
   `label_end_of_loop'.  The caller should set variables `coding',
   `src' and `src_end' to appropriate pointer in advance.  These
   macros are called from decoding routines `decode_coding_XXX', thus
   it is assumed that the source text is unibyte.  */

#define ONE_MORE_BYTE(c1)					\
  do {								\
    if (src >= src_end)						\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_SRC;	\
	goto label_end_of_loop;					\
      }								\
    c1 = *src++;						\
  } while (0)

#define TWO_MORE_BYTES(c1, c2)					\
  do {								\
    if (src + 1 >= src_end)					\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_SRC;	\
	goto label_end_of_loop;					\
      }								\
    c1 = *src++;						\
    c2 = *src++;						\
  } while (0)


/* Like ONE_MORE_BYTE, but 8-bit bytes of data at SRC are in multibyte
   form if MULTIBYTEP is nonzero.  */

#define ONE_MORE_BYTE_CHECK_MULTIBYTE(c1, multibytep)		\
  do {								\
    if (src >= src_end)						\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_SRC;	\
	goto label_end_of_loop;					\
      }								\
    c1 = *src++;						\
    if (multibytep && c1 == LEADING_CODE_8_BIT_CONTROL)		\
      c1 = *src++ - 0x20;					\
  } while (0)

/* Set C to the next character at the source text pointed by `src'.
   If there are not enough characters in the source, jump to
   `label_end_of_loop'.  The caller should set variables `coding'
   `src', `src_end', and `translation_table' to appropriate pointers
   in advance.  This macro is used in encoding routines
   `encode_coding_XXX', thus it assumes that the source text is in
   multibyte form except for 8-bit characters.  8-bit characters are
   in multibyte form if coding->src_multibyte is nonzero, else they
   are represented by a single byte.  */

#define ONE_MORE_CHAR(c)					\
  do {								\
    int len = src_end - src;					\
    int bytes;							\
    if (len <= 0)						\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_SRC;	\
	goto label_end_of_loop;					\
      }								\
    if (coding->src_multibyte					\
	|| UNIBYTE_STR_AS_MULTIBYTE_P (src, len, bytes))	\
      c = STRING_CHAR_AND_LENGTH (src, len, bytes);		\
    else							\
      c = *src, bytes = 1;					\
    if (!NILP (translation_table))				\
      c = translate_char (translation_table, c, -1, 0, 0);	\
    src += bytes;						\
  } while (0)


/* Produce a multibyte form of character C to `dst'.  Jump to
   `label_end_of_loop' if there's not enough space at `dst'.

   If we are now in the middle of a composition sequence, the decoded
   character may be ALTCHAR (for the current composition).  In that
   case, the character goes to coding->cmp_data->data instead of
   `dst'.

   This macro is used in decoding routines.  */

#define EMIT_CHAR(c)							\
  do {									\
    if (! COMPOSING_P (coding)						\
	|| coding->composing == COMPOSITION_RELATIVE			\
	|| coding->composing == COMPOSITION_WITH_RULE)			\
      {									\
	int bytes = CHAR_BYTES (c);					\
	if ((dst + bytes) > (dst_bytes ? dst_end : src))		\
	  {								\
	    coding->result = CODING_FINISH_INSUFFICIENT_DST;		\
	    goto label_end_of_loop;					\
	  }								\
	dst += CHAR_STRING (c, dst);					\
	coding->produced_char++;					\
      }									\
    									\
    if (COMPOSING_P (coding)						\
	&& coding->composing != COMPOSITION_RELATIVE)			\
      {									\
	CODING_ADD_COMPOSITION_COMPONENT (coding, c);			\
	coding->composition_rule_follows				\
	  = coding->composing != COMPOSITION_WITH_ALTCHARS;		\
      }									\
  } while (0)


#define EMIT_ONE_BYTE(c)					\
  do {								\
    if (dst >= (dst_bytes ? dst_end : src))			\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_DST;	\
	goto label_end_of_loop;					\
      }								\
    *dst++ = c;							\
  } while (0)

#define EMIT_TWO_BYTES(c1, c2)					\
  do {								\
    if (dst + 2 > (dst_bytes ? dst_end : src))			\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_DST;	\
	goto label_end_of_loop;					\
      }								\
    *dst++ = c1, *dst++ = c2;					\
  } while (0)

#define EMIT_BYTES(from, to)					\
  do {								\
    if (dst + (to - from) > (dst_bytes ? dst_end : src))	\
      {								\
	coding->result = CODING_FINISH_INSUFFICIENT_DST;	\
	goto label_end_of_loop;					\
      }								\
    while (from < to)						\
      *dst++ = *from++;						\
  } while (0)


/*** 1. Preamble ***/

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>

#ifdef emacs

#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "composite.h"
#include "ccl.h"
#include "coding.h"
#include "window.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* not emacs */

Lisp_Object Qcoding_system, Qeol_type;
Lisp_Object Qbuffer_file_coding_system;
Lisp_Object Qpost_read_conversion, Qpre_write_conversion;
Lisp_Object Qno_conversion, Qundecided;
Lisp_Object Qcoding_system_history;
Lisp_Object Qsafe_chars;
Lisp_Object Qvalid_codes;

extern Lisp_Object Qinsert_file_contents, Qwrite_region;
Lisp_Object Qcall_process, Qcall_process_region, Qprocess_argument;
Lisp_Object Qstart_process, Qopen_network_stream;
Lisp_Object Qtarget_idx;

Lisp_Object Vselect_safe_coding_system_function;

/* Mnemonic string for each format of end-of-line.  */
Lisp_Object eol_mnemonic_unix, eol_mnemonic_dos, eol_mnemonic_mac;
/* Mnemonic string to indicate format of end-of-line is not yet
   decided.  */
Lisp_Object eol_mnemonic_undecided;

/* Format of end-of-line decided by system.  This is CODING_EOL_LF on
   Unix, CODING_EOL_CRLF on DOS/Windows, and CODING_EOL_CR on Mac.  */
int system_eol_type;

#ifdef emacs

Lisp_Object Vcoding_system_list, Vcoding_system_alist;

Lisp_Object Qcoding_system_p, Qcoding_system_error;

/* Coding system emacs-mule and raw-text are for converting only
   end-of-line format.  */
Lisp_Object Qemacs_mule, Qraw_text;

/* Coding-systems are handed between Emacs Lisp programs and C internal
   routines by the following three variables.  */
/* Coding-system for reading files and receiving data from process.  */
Lisp_Object Vcoding_system_for_read;
/* Coding-system for writing files and sending data to process.  */
Lisp_Object Vcoding_system_for_write;
/* Coding-system actually used in the latest I/O.  */
Lisp_Object Vlast_coding_system_used;

/* A vector of length 256 which contains information about special
   Latin codes (especially for dealing with Microsoft codes).  */
Lisp_Object Vlatin_extra_code_table;

/* Flag to inhibit code conversion of end-of-line format.  */
int inhibit_eol_conversion;

/* Flag to inhibit ISO2022 escape sequence detection.  */
int inhibit_iso_escape_detection;

/* Flag to make buffer-file-coding-system inherit from process-coding.  */
int inherit_process_coding_system;

/* Coding system to be used to encode text for terminal display.  */
struct coding_system terminal_coding;

/* Coding system to be used to encode text for terminal display when
   terminal coding system is nil.  */
struct coding_system safe_terminal_coding;

/* Coding system of what is sent from terminal keyboard.  */
struct coding_system keyboard_coding;

/* Default coding system to be used to write a file.  */
struct coding_system default_buffer_file_coding;

Lisp_Object Vfile_coding_system_alist;
Lisp_Object Vprocess_coding_system_alist;
Lisp_Object Vnetwork_coding_system_alist;

Lisp_Object Vlocale_coding_system;

#endif /* emacs */

Lisp_Object Qcoding_category, Qcoding_category_index;

/* List of symbols `coding-category-xxx' ordered by priority.  */
Lisp_Object Vcoding_category_list;

/* Table of coding categories (Lisp symbols).  */
Lisp_Object Vcoding_category_table;

/* Table of names of symbol for each coding-category.  */
char *coding_category_name[CODING_CATEGORY_IDX_MAX] = {
  "coding-category-emacs-mule",
  "coding-category-sjis",
  "coding-category-iso-7",
  "coding-category-iso-7-tight",
  "coding-category-iso-8-1",
  "coding-category-iso-8-2",
  "coding-category-iso-7-else",
  "coding-category-iso-8-else",
  "coding-category-ccl",
  "coding-category-big5",
  "coding-category-utf-8",
  "coding-category-utf-16-be",
  "coding-category-utf-16-le",
  "coding-category-raw-text",
  "coding-category-binary"
};

/* Table of pointers to coding systems corresponding to each coding
   categories.  */
struct coding_system *coding_system_table[CODING_CATEGORY_IDX_MAX];

/* Table of coding category masks.  Nth element is a mask for a coding
   category of which priority is Nth.  */
static
int coding_priorities[CODING_CATEGORY_IDX_MAX];

/* Flag to tell if we look up translation table on character code
   conversion.  */
Lisp_Object Venable_character_translation;
/* Standard translation table to look up on decoding (reading).  */
Lisp_Object Vstandard_translation_table_for_decode;
/* Standard translation table to look up on encoding (writing).  */
Lisp_Object Vstandard_translation_table_for_encode;

Lisp_Object Qtranslation_table;
Lisp_Object Qtranslation_table_id;
Lisp_Object Qtranslation_table_for_decode;
Lisp_Object Qtranslation_table_for_encode;

/* Alist of charsets vs revision number.  */
Lisp_Object Vcharset_revision_alist;

/* Default coding systems used for process I/O.  */
Lisp_Object Vdefault_process_coding_system;

/* Global flag to tell that we can't call post-read-conversion and
   pre-write-conversion functions.  Usually the value is zero, but it
   is set to 1 temporarily while such functions are running.  This is
   to avoid infinite recursive call.  */
static int inhibit_pre_post_conversion;

/* Char-table containing safe coding systems of each character.  */
Lisp_Object Vchar_coding_system_table;
Lisp_Object Qchar_coding_system;

/* Return `safe-chars' property of coding system CODING.  Don't check
   validity of CODING.  */

Lisp_Object
coding_safe_chars (coding)
     struct coding_system *coding;
{
  Lisp_Object coding_spec, plist, safe_chars;

  coding_spec = Fget (coding->symbol, Qcoding_system);
  plist = XVECTOR (coding_spec)->contents[3];
  safe_chars = Fplist_get (XVECTOR (coding_spec)->contents[3], Qsafe_chars);
  return (CHAR_TABLE_P (safe_chars) ? safe_chars : Qt);
}

#define CODING_SAFE_CHAR_P(safe_chars, c) \
  (EQ (safe_chars, Qt) || !NILP (CHAR_TABLE_REF (safe_chars, c)))


/*** 2. Emacs internal format (emacs-mule) handlers ***/

/* Emacs' internal format for representation of multiple character
   sets is a kind of multi-byte encoding, i.e. characters are
   represented by variable-length sequences of one-byte codes.

   ASCII characters and control characters (e.g. `tab', `newline') are
   represented by one-byte sequences which are their ASCII codes, in
   the range 0x00 through 0x7F.

   8-bit characters of the range 0x80..0x9F are represented by
   two-byte sequences of LEADING_CODE_8_BIT_CONTROL and (their 8-bit
   code + 0x20).

   8-bit characters of the range 0xA0..0xFF are represented by
   one-byte sequences which are their 8-bit code.

   The other characters are represented by a sequence of `base
   leading-code', optional `extended leading-code', and one or two
   `position-code's.  The length of the sequence is determined by the
   base leading-code.  Leading-code takes the range 0x81 through 0x9D,
   whereas extended leading-code and position-code take the range 0xA0
   through 0xFF.  See `charset.h' for more details about leading-code
   and position-code.

   --- CODE RANGE of Emacs' internal format ---
   character set	range
   -------------	-----
   ascii		0x00..0x7F
   eight-bit-control	LEADING_CODE_8_BIT_CONTROL + 0xA0..0xBF
   eight-bit-graphic	0xA0..0xBF
   ELSE			0x81..0x9D + [0xA0..0xFF]+
   ---------------------------------------------

   As this is the internal character representation, the format is
   usually not used externally (i.e. in a file or in a data sent to a
   process).  But, it is possible to have a text externally in this
   format (i.e. by encoding by the coding system `emacs-mule').

   In that case, a sequence of one-byte codes has a slightly different
   form.

   Firstly, all characters in eight-bit-control are represented by
   one-byte sequences which are their 8-bit code.

   Next, character composition data are represented by the byte
   sequence of the form: 0x80 METHOD BYTES CHARS COMPONENT ...,
   where,
	METHOD is 0xF0 plus one of composition method (enum
	composition_method),

	BYTES is 0xA0 plus the byte length of these composition data,

	CHARS is 0xA0 plus the number of characters composed by these
	data,

	COMPONENTs are characters of multibyte form or composition
	rules encoded by two-byte of ASCII codes.

   In addition, for backward compatibility, the following formats are
   also recognized as composition data on decoding.

   0x80 MSEQ ...
   0x80 0xFF MSEQ RULE MSEQ RULE ... MSEQ

   Here,
	MSEQ is a multibyte form but in these special format:
	  ASCII: 0xA0 ASCII_CODE+0x80,
	  other: LEADING_CODE+0x20 FOLLOWING-BYTE ...,
	RULE is a one byte code of the range 0xA0..0xF0 that
	represents a composition rule.
  */

enum emacs_code_class_type emacs_code_class[256];

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in Emacs' internal format.  If it is,
   return CODING_CATEGORY_MASK_EMACS_MULE, else return 0.  */

static int
detect_coding_emacs_mule (src, src_end, multibytep)
      unsigned char *src, *src_end;
      int multibytep;
{
  unsigned char c;
  int composing = 0;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  while (1)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);

      if (composing)
	{
	  if (c < 0xA0)
	    composing = 0;
	  else if (c == 0xA0)
	    {
	      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
	      c &= 0x7F;
	    }
	  else
	    c -= 0x20;
	}

      if (c < 0x20)
	{
	  if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	    return 0;
	}
      else if (c >= 0x80 && c < 0xA0)
	{
	  if (c == 0x80)
	    /* Old leading code for a composite character.  */
	    composing = 1;
	  else
	    {
	      unsigned char *src_base = src - 1;
	      int bytes;

	      if (!UNIBYTE_STR_AS_MULTIBYTE_P (src_base, src_end - src_base,
					       bytes))
		return 0;
	      src = src_base + bytes;
	    }
	}
    }
 label_end_of_loop:
  return CODING_CATEGORY_MASK_EMACS_MULE;
}


/* Record the starting position START and METHOD of one composition.  */

#define CODING_ADD_COMPOSITION_START(coding, start, method)	\
  do {								\
    struct composition_data *cmp_data = coding->cmp_data;	\
    int *data = cmp_data->data + cmp_data->used;		\
    coding->cmp_data_start = cmp_data->used;			\
    data[0] = -1;						\
    data[1] = cmp_data->char_offset + start;			\
    data[3] = (int) method;					\
    cmp_data->used += 4;					\
  } while (0)

/* Record the ending position END of the current composition.  */

#define CODING_ADD_COMPOSITION_END(coding, end)			\
  do {								\
    struct composition_data *cmp_data = coding->cmp_data;	\
    int *data = cmp_data->data + coding->cmp_data_start;	\
    data[0] = cmp_data->used - coding->cmp_data_start;		\
    data[2] = cmp_data->char_offset + end;			\
  } while (0)

/* Record one COMPONENT (alternate character or composition rule).  */

#define CODING_ADD_COMPOSITION_COMPONENT(coding, component)	\
  (coding->cmp_data->data[coding->cmp_data->used++] = component)


/* Get one byte from a data pointed by SRC and increment SRC.  If SRC
   is not less than SRC_END, return -1 without incrementing Src.  */

#define SAFE_ONE_MORE_BYTE() (src >= src_end ? -1 : *src++)


/* Decode a character represented as a component of composition
   sequence of Emacs 20 style at SRC.  Set C to that character, store
   its multibyte form sequence at P, and set P to the end of that
   sequence.  If no valid character is found, set C to -1.  */

#define DECODE_EMACS_MULE_COMPOSITION_CHAR(c, p)		\
  do {								\
    int bytes;							\
    								\
    c = SAFE_ONE_MORE_BYTE ();					\
    if (c < 0)							\
      break;							\
    if (CHAR_HEAD_P (c))					\
      c = -1;							\
    else if (c == 0xA0)						\
      {								\
	c = SAFE_ONE_MORE_BYTE ();				\
	if (c < 0xA0)						\
	  c = -1;						\
	else							\
	  {							\
	    c -= 0xA0;						\
	    *p++ = c;						\
	  }							\
      }								\
    else if (BASE_LEADING_CODE_P (c - 0x20))			\
      {								\
	unsigned char *p0 = p;					\
								\
	c -= 0x20;						\
	*p++ = c;						\
	bytes = BYTES_BY_CHAR_HEAD (c);				\
	while (--bytes)						\
	  {							\
	    c = SAFE_ONE_MORE_BYTE ();				\
	    if (c < 0)						\
	      break;						\
	    *p++ = c;						\
	  }							\
	if (UNIBYTE_STR_AS_MULTIBYTE_P (p0, p - p0, bytes))	\
	  c = STRING_CHAR (p0, bytes);				\
	else							\
	  c = -1;						\
      }								\
    else							\
      c = -1;							\
  } while (0)


/* Decode a composition rule represented as a component of composition
   sequence of Emacs 20 style at SRC.  Set C to the rule.  If not
   valid rule is found, set C to -1.  */

#define DECODE_EMACS_MULE_COMPOSITION_RULE(c)		\
  do {							\
    c = SAFE_ONE_MORE_BYTE ();				\
    c -= 0xA0;						\
    if (c < 0 || c >= 81)				\
      c = -1;						\
    else						\
      {							\
	gref = c / 9, nref = c % 9;			\
	c = COMPOSITION_ENCODE_RULE (gref, nref);	\
      }							\
  } while (0)


/* Decode composition sequence encoded by `emacs-mule' at the source
   pointed by SRC.  SRC_END is the end of source.  Store information
   of the composition in CODING->cmp_data.

   For backward compatibility, decode also a composition sequence of
   Emacs 20 style.  In that case, the composition sequence contains
   characters that should be extracted into a buffer or string.  Store
   those characters at *DESTINATION in multibyte form.

   If we encounter an invalid byte sequence, return 0.
   If we encounter an insufficient source or destination, or
   insufficient space in CODING->cmp_data, return 1.
   Otherwise, return consumed bytes in the source.

*/
static INLINE int
decode_composition_emacs_mule (coding, src, src_end,
			       destination, dst_end, dst_bytes)
     struct coding_system *coding;
     unsigned char *src, *src_end, **destination, *dst_end;
     int dst_bytes;
{
  unsigned char *dst = *destination;
  int method, data_len, nchars;
  unsigned char *src_base = src++;
  /* Store components of composition.  */
  int component[COMPOSITION_DATA_MAX_BUNCH_LENGTH];
  int ncomponent;
  /* Store multibyte form of characters to be composed.  This is for
     Emacs 20 style composition sequence.  */
  unsigned char buf[MAX_COMPOSITION_COMPONENTS * MAX_MULTIBYTE_LENGTH];
  unsigned char *bufp = buf;
  int c, i, gref, nref;

  if (coding->cmp_data->used + COMPOSITION_DATA_MAX_BUNCH_LENGTH
      >= COMPOSITION_DATA_SIZE)
    {
      coding->result = CODING_FINISH_INSUFFICIENT_CMP;
      return -1;
    }

  ONE_MORE_BYTE (c);
  if (c - 0xF0 >= COMPOSITION_RELATIVE
	   && c - 0xF0 <= COMPOSITION_WITH_RULE_ALTCHARS)
    {
      int with_rule;

      method = c - 0xF0;
      with_rule = (method == COMPOSITION_WITH_RULE
		   || method == COMPOSITION_WITH_RULE_ALTCHARS);
      ONE_MORE_BYTE (c);
      data_len = c - 0xA0;
      if (data_len < 4
	  || src_base + data_len > src_end)
	return 0;
      ONE_MORE_BYTE (c);
      nchars = c - 0xA0;
      if (c < 1)
	return 0;
      for (ncomponent = 0; src < src_base + data_len; ncomponent++)
	{
	  /* If it is longer than this, it can't be valid.  */
	  if (ncomponent >= COMPOSITION_DATA_MAX_BUNCH_LENGTH)
	    return 0;

	  if (ncomponent % 2 && with_rule)
	    {
	      ONE_MORE_BYTE (gref);
	      gref -= 32;
	      ONE_MORE_BYTE (nref);
	      nref -= 32;
	      c = COMPOSITION_ENCODE_RULE (gref, nref);
	    }
	  else
	    {
	      int bytes;
	      if (UNIBYTE_STR_AS_MULTIBYTE_P (src, src_end - src, bytes))
		c = STRING_CHAR (src, bytes);
	      else
		c = *src, bytes = 1;
	      src += bytes;
	    }
	  component[ncomponent] = c;
	}
    }
  else
    {
      /* This may be an old Emacs 20 style format.  See the comment at
	 the section 2 of this file.  */
      while (src < src_end && !CHAR_HEAD_P (*src)) src++;
      if (src == src_end
	  && !(coding->mode & CODING_MODE_LAST_BLOCK))
	goto label_end_of_loop;

      src_end = src;
      src = src_base + 1;
      if (c < 0xC0)
	{
	  method = COMPOSITION_RELATIVE;
	  for (ncomponent = 0; ncomponent < MAX_COMPOSITION_COMPONENTS;)
	    {
	      DECODE_EMACS_MULE_COMPOSITION_CHAR (c, bufp);
	      if (c < 0)
		break;
	      component[ncomponent++] = c;
	    }
	  if (ncomponent < 2)
	    return 0;
	  nchars = ncomponent;
	}
      else if (c == 0xFF)
	{
	  method = COMPOSITION_WITH_RULE;
	  src++;
	  DECODE_EMACS_MULE_COMPOSITION_CHAR (c, bufp);
	  if (c < 0)
	    return 0;
	  component[0] = c;
	  for (ncomponent = 1;
	       ncomponent < MAX_COMPOSITION_COMPONENTS * 2 - 1;)
	    {
	      DECODE_EMACS_MULE_COMPOSITION_RULE (c);
	      if (c < 0)
		break;
	      component[ncomponent++] = c;
	      DECODE_EMACS_MULE_COMPOSITION_CHAR (c, bufp);
	      if (c < 0)
		break;
	      component[ncomponent++] = c;
	    }
	  if (ncomponent < 3)
	    return 0;
	  nchars = (ncomponent + 1) / 2;
	}
      else
	return 0;
    }

  if (buf == bufp || dst + (bufp - buf) <= (dst_bytes ? dst_end : src))
    {
      CODING_ADD_COMPOSITION_START (coding, coding->produced_char, method);
      for (i = 0; i < ncomponent; i++)
	CODING_ADD_COMPOSITION_COMPONENT (coding, component[i]);
      CODING_ADD_COMPOSITION_END (coding, coding->produced_char + nchars);
      if (buf < bufp)
	{
	  unsigned char *p = buf;
	  EMIT_BYTES (p, bufp);
	  *destination += bufp - buf;
	  coding->produced_char += nchars;
	}
      return (src - src_base);
    }
 label_end_of_loop:
  return -1;
}

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_emacs_mule (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code, or
     when there's not enough destination area to produce a
     character.  */
  unsigned char *src_base;

  coding->produced_char = 0;
  while ((src_base = src) < src_end)
    {
      unsigned char tmp[MAX_MULTIBYTE_LENGTH], *p;
      int bytes;

      if (*src == '\r')
	{
	  int c = *src++;

	  if (coding->eol_type == CODING_EOL_CR)
	    c = '\n';
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    {
	      ONE_MORE_BYTE (c);
	      if (c != '\n')
		{
		  if (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		    {
		      coding->result = CODING_FINISH_INCONSISTENT_EOL;
		      goto label_end_of_loop;
		    }
		  src--;
		  c = '\r';
		}
	    }
	  *dst++ = c;
	  coding->produced_char++;
	  continue;
	}
      else if (*src == '\n')
	{
	  if ((coding->eol_type == CODING_EOL_CR
	       || coding->eol_type == CODING_EOL_CRLF)
	      && coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
	    {
	      coding->result = CODING_FINISH_INCONSISTENT_EOL;
	      goto label_end_of_loop;
	    }
	  *dst++ = *src++;
	  coding->produced_char++;
	  continue;
	}
      else if (*src == 0x80)
	{
	  /* Start of composition data.  */
	  int consumed  = decode_composition_emacs_mule (coding, src, src_end,
							 &dst, dst_end,
							 dst_bytes);
	  if (consumed < 0)
	    goto label_end_of_loop;
	  else if (consumed > 0)
	    {
	      src += consumed;
	      continue;
	    }
	  bytes = CHAR_STRING (*src, tmp);
	  p = tmp;
	  src++;
	}
      else if (UNIBYTE_STR_AS_MULTIBYTE_P (src, src_end - src, bytes))
	{
	  p = src;
	  src += bytes;
	}
      else
	{
	  bytes = CHAR_STRING (*src, tmp);
	  p = tmp;
	  src++;
	}
      if (dst + bytes >= (dst_bytes ? dst_end : src))
	{
	  coding->result = CODING_FINISH_INSUFFICIENT_DST;
	  break;
	}
      while (bytes--) *dst++ = *p++;
      coding->produced_char++;
    }
 label_end_of_loop:
  coding->consumed = coding->consumed_char = src_base - source;
  coding->produced = dst - destination;
}


/* Encode composition data stored at DATA into a special byte sequence
   starting by 0x80.  Update CODING->cmp_data_start and maybe
   CODING->cmp_data for the next call.  */

#define ENCODE_COMPOSITION_EMACS_MULE(coding, data)			\
  do {									\
    unsigned char buf[1024], *p0 = buf, *p;				\
    int len = data[0];							\
    int i;								\
    									\
    buf[0] = 0x80;							\
    buf[1] = 0xF0 + data[3];	/* METHOD */				\
    buf[3] = 0xA0 + (data[2] - data[1]); /* COMPOSED-CHARS */		\
    p = buf + 4;							\
    if (data[3] == COMPOSITION_WITH_RULE				\
	|| data[3] == COMPOSITION_WITH_RULE_ALTCHARS)			\
      {									\
	p += CHAR_STRING (data[4], p);					\
	for (i = 5; i < len; i += 2)					\
	  {								\
	    int gref, nref;						\
	     COMPOSITION_DECODE_RULE (data[i], gref, nref);		\
	    *p++ = 0x20 + gref;						\
	    *p++ = 0x20 + nref;						\
	    p += CHAR_STRING (data[i + 1], p);				\
	  }								\
      }									\
    else								\
      {									\
	for (i = 4; i < len; i++)					\
	  p += CHAR_STRING (data[i], p);				\
      }									\
    buf[2] = 0xA0 + (p - buf);	/* COMPONENTS-BYTES */			\
    									\
    if (dst + (p - buf) + 4 > (dst_bytes ? dst_end : src))		\
      {									\
	coding->result = CODING_FINISH_INSUFFICIENT_DST;		\
	goto label_end_of_loop;						\
      }									\
    while (p0 < p)							\
      *dst++ = *p0++;							\
    coding->cmp_data_start += data[0];					\
    if (coding->cmp_data_start == coding->cmp_data->used		\
	&& coding->cmp_data->next)					\
      {									\
	coding->cmp_data = coding->cmp_data->next;			\
	coding->cmp_data_start = 0;					\
      }									\
  } while (0)


static void encode_eol P_ ((struct coding_system *, unsigned char *,
			    unsigned char *, int, int));

static void
encode_coding_emacs_mule (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  unsigned char *src_base;
  int c;
  int char_offset;
  int *data;

  Lisp_Object translation_table;

  translation_table = Qnil;

  /* Optimization for the case that there's no composition.  */
  if (!coding->cmp_data || coding->cmp_data->used == 0)
    {
      encode_eol (coding, source, destination, src_bytes, dst_bytes);
      return;
    }

  char_offset = coding->cmp_data->char_offset;
  data = coding->cmp_data->data + coding->cmp_data_start;
  while (1)
    {
      src_base = src;

      /* If SRC starts a composition, encode the information about the
	 composition in advance.  */
      if (coding->cmp_data_start < coding->cmp_data->used
	  && char_offset + coding->consumed_char == data[1])
	{
	  ENCODE_COMPOSITION_EMACS_MULE (coding, data);
	  char_offset = coding->cmp_data->char_offset;
	  data = coding->cmp_data->data + coding->cmp_data_start;
	}

      ONE_MORE_CHAR (c);
      if (c == '\n' && (coding->eol_type == CODING_EOL_CRLF
			|| coding->eol_type == CODING_EOL_CR))
	{
	  if (coding->eol_type == CODING_EOL_CRLF)
	    EMIT_TWO_BYTES ('\r', c);
	  else
	    EMIT_ONE_BYTE ('\r');
	}
      else if (SINGLE_BYTE_CHAR_P (c))
	EMIT_ONE_BYTE (c);
      else
	EMIT_BYTES (src_base, src);
      coding->consumed_char++;
    }
 label_end_of_loop:
  coding->consumed = src_base - source;
  coding->produced = coding->produced_char = dst - destination;
  return;
}


/*** 3. ISO2022 handlers ***/

/* The following note describes the coding system ISO2022 briefly.
   Since the intention of this note is to help understand the
   functions in this file, some parts are NOT ACCURATE or are OVERLY
   SIMPLIFIED.  For thorough understanding, please refer to the
   original document of ISO2022.  This is equivalent to the standard
   ECMA-35, obtainable from <URL:http://www.ecma.ch/> (*).

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environments.  For 7-bit environments, all text
   is encoded using bytes less than 128.  This may make the encoded
   text a little bit longer, but the text passes more easily through
   several types of gateway, some of which strip off the MSB (Most
   Significant Bit).

   There are two kinds of character sets: control character sets and
   graphic character sets.  The former contain control characters such
   as `newline' and `escape' to provide control functions (control
   functions are also provided by escape sequences).  The latter
   contain graphic characters such as 'A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, according to the number of bytes (DIMENSION) and
   number of characters in one dimension (CHARS) of the set:
   - DIMENSION1_CHARS94
   - DIMENSION1_CHARS96
   - DIMENSION2_CHARS94
   - DIMENSION2_CHARS96

   In addition, each character set is assigned an identification tag,
   unique for each set, called the "final character" (denoted as <F>
   hereafter).  The <F> of each character set is decided by ECMA(*)
   when it is registered in ISO.  The code range of <F> is 0x30..0x7F
   (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character sets [NAME(<F>)]:
	o DIMENSION1_CHARS94 -- ASCII('B'), right-half-of-JISX0201('I'), ...
	o DIMENSION1_CHARS96 -- right-half-of-ISO8859-1('A'), ...
	o DIMENSION2_CHARS94 -- GB2312('A'), JISX0208('B'), ...
	o DIMENSION2_CHARS96 -- none for the moment

   A code area (1 byte=8 bits) is divided into 4 areas, C0, GL, C1, and GR.
	C0 [0x00..0x1F] -- control character plane 0
	GL [0x20..0x7F] -- graphic character plane 0
	C1 [0x80..0x9F] -- control character plane 1
	GR [0xA0..0xFF] -- graphic character plane 1

   A control character set is directly designated and invoked to C0 or
   C1 by an escape sequence.  The most common case is that:
   - ISO646's  control character set is designated/invoked to C0, and
   - ISO6429's control character set is designated/invoked to C1,
   and usually these designations/invocations are omitted in encoded
   text.  In a 7-bit environment, only C0 can be used, and a control
   character for C1 is encoded by an appropriate escape sequence to
   fit into the environment.  All control characters for C1 are
   defined to have corresponding escape sequences.

   A graphic character set is at first designated to one of four
   graphic registers (G0 through G3), then these graphic registers are
   invoked to GL or GR.  These designations and invocations can be
   done independently.  The most common case is that G0 is invoked to
   GL, G1 is invoked to GR, and ASCII is designated to G0.  Usually
   these invocations and designations are omitted in encoded text.
   In a 7-bit environment, only GL can be used.

   When a graphic character set of CHARS94 is invoked to GL, codes
   0x20 and 0x7F of the GL area work as control characters SPACE and
   DEL respectively, and codes 0xA0 and 0xFF of the GR area should not
   be used.

   There are two ways of invocation: locking-shift and single-shift.
   With locking-shift, the invocation lasts until the next different
   invocation, whereas with single-shift, the invocation affects the
   following character only and doesn't affect the locking-shift
   state.  Invocations are done by the following control characters or
   escape sequences:

   ----------------------------------------------------------------------
   abbrev  function	             cntrl escape seq	description
   ----------------------------------------------------------------------
   SI/LS0  (shift-in)		     0x0F  none		invoke G0 into GL
   SO/LS1  (shift-out)		     0x0E  none		invoke G1 into GL
   LS2     (locking-shift-2)	     none  ESC 'n'	invoke G2 into GL
   LS3     (locking-shift-3)	     none  ESC 'o'	invoke G3 into GL
   LS1R    (locking-shift-1 right)   none  ESC '~'      invoke G1 into GR (*)
   LS2R    (locking-shift-2 right)   none  ESC '}'      invoke G2 into GR (*)
   LS3R    (locking-shift 3 right)   none  ESC '|'      invoke G3 into GR (*)
   SS2     (single-shift-2)	     0x8E  ESC 'N'	invoke G2 for one char
   SS3     (single-shift-3)	     0x8F  ESC 'O'	invoke G3 for one char
   ----------------------------------------------------------------------
   (*) These are not used by any known coding system.

   Control characters for these functions are defined by macros
   ISO_CODE_XXX in `coding.h'.

   Designations are done by the following escape sequences:
   ----------------------------------------------------------------------
   escape sequence	description
   ----------------------------------------------------------------------
   ESC '(' <F>		designate DIMENSION1_CHARS94<F> to G0
   ESC ')' <F>		designate DIMENSION1_CHARS94<F> to G1
   ESC '*' <F>		designate DIMENSION1_CHARS94<F> to G2
   ESC '+' <F>		designate DIMENSION1_CHARS94<F> to G3
   ESC ',' <F>		designate DIMENSION1_CHARS96<F> to G0 (*)
   ESC '-' <F>		designate DIMENSION1_CHARS96<F> to G1
   ESC '.' <F>		designate DIMENSION1_CHARS96<F> to G2
   ESC '/' <F>		designate DIMENSION1_CHARS96<F> to G3
   ESC '$' '(' <F>	designate DIMENSION2_CHARS94<F> to G0 (**)
   ESC '$' ')' <F>	designate DIMENSION2_CHARS94<F> to G1
   ESC '$' '*' <F>	designate DIMENSION2_CHARS94<F> to G2
   ESC '$' '+' <F>	designate DIMENSION2_CHARS94<F> to G3
   ESC '$' ',' <F>	designate DIMENSION2_CHARS96<F> to G0 (*)
   ESC '$' '-' <F>	designate DIMENSION2_CHARS96<F> to G1
   ESC '$' '.' <F>	designate DIMENSION2_CHARS96<F> to G2
   ESC '$' '/' <F>	designate DIMENSION2_CHARS96<F> to G3
   ----------------------------------------------------------------------

   In this list, "DIMENSION1_CHARS94<F>" means a graphic character set
   of dimension 1, chars 94, and final character <F>, etc...

   Note (*): Although these designations are not allowed in ISO2022,
   Emacs accepts them on decoding, and produces them on encoding
   CHARS96 character sets in a coding system which is characterized as
   7-bit environment, non-locking-shift, and non-single-shift.

   Note (**): If <F> is '@', 'A', or 'B', the intermediate character
   '(' can be omitted.  We refer to this as "short-form" hereafter.

   Now you may notice that there are a lot of ways of encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X11's inter client
   communication, ISO-2022-JP (used in Japanese Internet), ISO-2022-KR
   (used in Korean Internet), EUC (Extended UNIX Code, used in Asian
   localized platforms), and all of these are variants of ISO2022.

   In addition to the above, Emacs handles two more kinds of escape
   sequences: ISO6429's direction specification and Emacs' private
   sequence for specifying character composition.

   ISO6429's direction specification takes the following form:
	o CSI ']'      -- end of the current direction
	o CSI '0' ']'  -- end of the current direction
	o CSI '1' ']'  -- start of left-to-right text
	o CSI '2' ']'  -- start of right-to-left text
   The control character CSI (0x9B: control sequence introducer) is
   abbreviated to the escape sequence ESC '[' in a 7-bit environment.

   Character composition specification takes the following form:
	o ESC '0' -- start relative composition
	o ESC '1' -- end composition
	o ESC '2' -- start rule-base composition (*)
	o ESC '3' -- start relative composition with alternate chars  (**)
	o ESC '4' -- start rule-base composition with alternate chars  (**)
  Since these are not standard escape sequences of any ISO standard,
  the use of them with these meanings is restricted to Emacs only.

  (*) This form is used only in Emacs 20.5 and older versions,
  but the newer versions can safely decode it.
  (**) This form is used only in Emacs 21.1 and newer versions,
  and the older versions can't decode it.

  Here's a list of example usages of these composition escape
  sequences (categorized by `enum composition_method').

  COMPOSITION_RELATIVE:
	ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE:
	ESC 2 CHAR [ RULE CHAR ] ESC 1
  COMPOSITION_WITH_ALTCHARS:
	ESC 3 ALTCHAR [ ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE_ALTCHARS:
	ESC 4 ALTCHAR [ RULE ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1 */

enum iso_code_class_type iso_code_class[256];

#define CHARSET_OK(idx, charset, c)					\
  (coding_system_table[idx]						\
   && (charset == CHARSET_ASCII						\
       || (safe_chars = coding_safe_chars (coding_system_table[idx]),	\
	   CODING_SAFE_CHAR_P (safe_chars, c)))				\
   && (CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding_system_table[idx],	\
					      charset)			\
       != CODING_SPEC_ISO_NO_REQUESTED_DESIGNATION))

#define SHIFT_OUT_OK(idx) \
  (CODING_SPEC_ISO_INITIAL_DESIGNATION (coding_system_table[idx], 1) >= 0)

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in ISO2022.  If it is, return an
   integer in which appropriate flag bits any of:
	CODING_CATEGORY_MASK_ISO_7
	CODING_CATEGORY_MASK_ISO_7_TIGHT
	CODING_CATEGORY_MASK_ISO_8_1
	CODING_CATEGORY_MASK_ISO_8_2
	CODING_CATEGORY_MASK_ISO_7_ELSE
	CODING_CATEGORY_MASK_ISO_8_ELSE
   are set.  If a code which should never appear in ISO2022 is found,
   returns 0.  */

static int
detect_coding_iso2022 (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  int mask = CODING_CATEGORY_MASK_ISO;
  int mask_found = 0;
  int reg[4], shift_out = 0, single_shifting = 0;
  int c, c1, charset;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;
  Lisp_Object safe_chars;

  reg[0] = CHARSET_ASCII, reg[1] = reg[2] = reg[3] = -1;
  while (mask && src < src_end)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      switch (c)
	{
	case ISO_CODE_ESC:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
	  if (c >= '(' && c <= '/')
	    {
	      /* Designation sequence for a charset of dimension 1.  */
	      ONE_MORE_BYTE_CHECK_MULTIBYTE (c1, multibytep);
	      if (c1 < ' ' || c1 >= 0x80
		  || (charset = iso_charset_table[0][c >= ','][c1]) < 0)
		/* Invalid designation sequence.  Just ignore.  */
		break;
	      reg[(c - '(') % 4] = charset;
	    }
	  else if (c == '$')
	    {
	      /* Designation sequence for a charset of dimension 2.  */
	      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
	      if (c >= '@' && c <= 'B')
		/* Designation for JISX0208.1978, GB2312, or JISX0208.  */
		reg[0] = charset = iso_charset_table[1][0][c];
	      else if (c >= '(' && c <= '/')
		{
		  ONE_MORE_BYTE_CHECK_MULTIBYTE (c1, multibytep);
		  if (c1 < ' ' || c1 >= 0x80
		      || (charset = iso_charset_table[1][c >= ','][c1]) < 0)
		    /* Invalid designation sequence.  Just ignore.  */
		    break;
		  reg[(c - '(') % 4] = charset;
		}
	      else
		/* Invalid designation sequence.  Just ignore.  */
		break;
	    }
	  else if (c == 'N' || c == 'O')
	    {
	      /* ESC <Fe> for SS2 or SS3.  */
	      mask &= CODING_CATEGORY_MASK_ISO_7_ELSE;
	      break;
	    }
	  else if (c >= '0' && c <= '4')
	    {
	      /* ESC <Fp> for start/end composition.  */
	      mask_found |= CODING_CATEGORY_MASK_ISO;
	      break;
	    }
	  else
	    /* Invalid escape sequence.  Just ignore.  */
	    break;

	  /* We found a valid designation sequence for CHARSET.  */
	  mask &= ~CODING_CATEGORY_MASK_ISO_8BIT;
	  c = MAKE_CHAR (charset, 0, 0);
	  if (CHARSET_OK (CODING_CATEGORY_IDX_ISO_7, charset, c))
	    mask_found |= CODING_CATEGORY_MASK_ISO_7;
	  else
	    mask &= ~CODING_CATEGORY_MASK_ISO_7;
	  if (CHARSET_OK (CODING_CATEGORY_IDX_ISO_7_TIGHT, charset, c))
	    mask_found |= CODING_CATEGORY_MASK_ISO_7_TIGHT;
	  else
	    mask &= ~CODING_CATEGORY_MASK_ISO_7_TIGHT;
	  if (CHARSET_OK (CODING_CATEGORY_IDX_ISO_7_ELSE, charset, c))
	    mask_found |= CODING_CATEGORY_MASK_ISO_7_ELSE;
	  else
	    mask &= ~CODING_CATEGORY_MASK_ISO_7_ELSE;
	  if (CHARSET_OK (CODING_CATEGORY_IDX_ISO_8_ELSE, charset, c))
	    mask_found |= CODING_CATEGORY_MASK_ISO_8_ELSE;
	  else
	    mask &= ~CODING_CATEGORY_MASK_ISO_8_ELSE;
	  break;

	case ISO_CODE_SO:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  if (shift_out == 0
	      && (reg[1] >= 0
		  || SHIFT_OUT_OK (CODING_CATEGORY_IDX_ISO_7_ELSE)
		  || SHIFT_OUT_OK (CODING_CATEGORY_IDX_ISO_8_ELSE)))
	    {
	      /* Locking shift out.  */
	      mask &= ~CODING_CATEGORY_MASK_ISO_7BIT;
	      mask_found |= CODING_CATEGORY_MASK_ISO_SHIFT;
	    }
	  break;

	case ISO_CODE_SI:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  if (shift_out == 1)
	    {
	      /* Locking shift in.  */
	      mask &= ~CODING_CATEGORY_MASK_ISO_7BIT;
	      mask_found |= CODING_CATEGORY_MASK_ISO_SHIFT;
	    }
	  break;

	case ISO_CODE_CSI:
	  single_shifting = 0;
	case ISO_CODE_SS2:
	case ISO_CODE_SS3:
	  {
	    int newmask = CODING_CATEGORY_MASK_ISO_8_ELSE;

	    if (inhibit_iso_escape_detection)
	      break;
	    if (c != ISO_CODE_CSI)
	      {
		if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_1]->flags
		    & CODING_FLAG_ISO_SINGLE_SHIFT)
		  newmask |= CODING_CATEGORY_MASK_ISO_8_1;
		if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_2]->flags
		    & CODING_FLAG_ISO_SINGLE_SHIFT)
		  newmask |= CODING_CATEGORY_MASK_ISO_8_2;
		single_shifting = 1;
	      }
	    if (VECTORP (Vlatin_extra_code_table)
		&& !NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
	      {
		if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_1]->flags
		    & CODING_FLAG_ISO_LATIN_EXTRA)
		  newmask |= CODING_CATEGORY_MASK_ISO_8_1;
		if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_2]->flags
		    & CODING_FLAG_ISO_LATIN_EXTRA)
		  newmask |= CODING_CATEGORY_MASK_ISO_8_2;
	      }
	    mask &= newmask;
	    mask_found |= newmask;
	  }
	  break;

	default:
	  if (c < 0x80)
	    {
	      single_shifting = 0;
	      break;
	    }
	  else if (c < 0xA0)
	    {
	      single_shifting = 0;
	      if (VECTORP (Vlatin_extra_code_table)
		  && !NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
		{
		  int newmask = 0;

		  if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_1]->flags
		      & CODING_FLAG_ISO_LATIN_EXTRA)
		    newmask |= CODING_CATEGORY_MASK_ISO_8_1;
		  if (coding_system_table[CODING_CATEGORY_IDX_ISO_8_2]->flags
		      & CODING_FLAG_ISO_LATIN_EXTRA)
		    newmask |= CODING_CATEGORY_MASK_ISO_8_2;
		  mask &= newmask;
		  mask_found |= newmask;
		}
	      else
		return 0;
	    }
	  else
	    {
	      mask &= ~(CODING_CATEGORY_MASK_ISO_7BIT
			| CODING_CATEGORY_MASK_ISO_7_ELSE);
	      mask_found |= CODING_CATEGORY_MASK_ISO_8_1;
	      /* Check the length of succeeding codes of the range
                 0xA0..0FF.  If the byte length is odd, we exclude
                 CODING_CATEGORY_MASK_ISO_8_2.  We can check this only
                 when we are not single shifting.  */
	      if (!single_shifting
		  && mask & CODING_CATEGORY_MASK_ISO_8_2)
		{
		  int i = 1;
		  while (src < src_end)
		    {
		      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
		      if (c < 0xA0)
			break;
		      i++;
		    }

		  if (i & 1 && src < src_end)
		    mask &= ~CODING_CATEGORY_MASK_ISO_8_2;
		  else
		    mask_found |= CODING_CATEGORY_MASK_ISO_8_2;
		}
	    }
	  break;
	}
    }
 label_end_of_loop:
  return (mask & mask_found);
}

/* Decode a character of which charset is CHARSET, the 1st position
   code is C1, the 2nd position code is C2, and return the decoded
   character code.  If the variable `translation_table' is non-nil,
   returned the translated code.  */

#define DECODE_ISO_CHARACTER(charset, c1, c2)	\
  (NILP (translation_table)			\
   ? MAKE_CHAR (charset, c1, c2)		\
   : translate_char (translation_table, -1, charset, c1, c2))

/* Set designation state into CODING.  */
#define DECODE_DESIGNATION(reg, dimension, chars, final_char)		   \
  do {									   \
    int charset, c;							   \
    									   \
    if (final_char < '0' || final_char >= 128)				   \
      goto label_invalid_code;						   \
    charset = ISO_CHARSET_TABLE (make_number (dimension),		   \
				 make_number (chars),			   \
				 make_number (final_char));		   \
    c = MAKE_CHAR (charset, 0, 0);					   \
    if (charset >= 0							   \
	&& (CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) == reg \
	    || CODING_SAFE_CHAR_P (safe_chars, c)))			   \
      {									   \
	if (coding->spec.iso2022.last_invalid_designation_register == 0	   \
	    && reg == 0							   \
	    && charset == CHARSET_ASCII)				   \
	  {								   \
	    /* We should insert this designation sequence as is so	   \
               that it is surely written back to a file.  */		   \
	    coding->spec.iso2022.last_invalid_designation_register = -1;   \
	    goto label_invalid_code;					   \
	  }								   \
	coding->spec.iso2022.last_invalid_designation_register = -1;	   \
        if ((coding->mode & CODING_MODE_DIRECTION)			   \
	    && CHARSET_REVERSE_CHARSET (charset) >= 0)			   \
          charset = CHARSET_REVERSE_CHARSET (charset);			   \
        CODING_SPEC_ISO_DESIGNATION (coding, reg) = charset;		   \
      }									   \
    else								   \
      {									   \
	coding->spec.iso2022.last_invalid_designation_register = reg;	   \
	goto label_invalid_code;					   \
      }									   \
  } while (0)

/* Allocate a memory block for storing information about compositions.
   The block is chained to the already allocated blocks.  */

void
coding_allocate_composition_data (coding, char_offset)
     struct coding_system *coding;
     int char_offset;
{
  struct composition_data *cmp_data
    = (struct composition_data *) xmalloc (sizeof *cmp_data);

  cmp_data->char_offset = char_offset;
  cmp_data->used = 0;
  cmp_data->prev = coding->cmp_data;
  cmp_data->next = NULL;
  if (coding->cmp_data)
    coding->cmp_data->next = cmp_data;
  coding->cmp_data = cmp_data;
  coding->cmp_data_start = 0;
}

/* Handle composition start sequence ESC 0, ESC 2, ESC 3, or ESC 4.
   ESC 0 : relative composition : ESC 0 CHAR ... ESC 1
   ESC 2 : rulebase composition : ESC 2 CHAR RULE CHAR RULE ... CHAR ESC 1
   ESC 3 : altchar composition :  ESC 3 ALT ... ESC 0 CHAR ... ESC 1
   ESC 4 : alt&rule composition : ESC 4 ALT RULE .. ALT ESC 0 CHAR ... ESC 1
  */

#define DECODE_COMPOSITION_START(c1)					   \
  do {									   \
    if (coding->composing == COMPOSITION_DISABLED)			   \
      {									   \
      	*dst++ = ISO_CODE_ESC;						   \
	*dst++ = c1 & 0x7f;						   \
	coding->produced_char += 2;					   \
      }									   \
    else if (!COMPOSING_P (coding))					   \
      {									   \
	/* This is surely the start of a composition.  We must be sure	   \
           that coding->cmp_data has enough space to store the		   \
           information about the composition.  If not, terminate the	   \
           current decoding loop, allocate one more memory block for	   \
           coding->cmp_data in the caller, then start the decoding	   \
           loop again.  We can't allocate memory here directly because	   \
           it may cause buffer/string relocation.  */			   \
	if (!coding->cmp_data						   \
	    || (coding->cmp_data->used + COMPOSITION_DATA_MAX_BUNCH_LENGTH \
		>= COMPOSITION_DATA_SIZE))				   \
	  {								   \
	    coding->result = CODING_FINISH_INSUFFICIENT_CMP;		   \
	    goto label_end_of_loop;					   \
	  }								   \
	coding->composing = (c1 == '0' ? COMPOSITION_RELATIVE		   \
			     : c1 == '2' ? COMPOSITION_WITH_RULE	   \
			     : c1 == '3' ? COMPOSITION_WITH_ALTCHARS	   \
			     : COMPOSITION_WITH_RULE_ALTCHARS);		   \
	CODING_ADD_COMPOSITION_START (coding, coding->produced_char,	   \
				      coding->composing);		   \
	coding->composition_rule_follows = 0;				   \
      }									   \
    else								   \
      {									   \
	/* We are already handling a composition.  If the method is	   \
           the following two, the codes following the current escape	   \
           sequence are actual characters stored in a buffer.  */	   \
	if (coding->composing == COMPOSITION_WITH_ALTCHARS		   \
	    || coding->composing == COMPOSITION_WITH_RULE_ALTCHARS)	   \
	  {								   \
	    coding->composing = COMPOSITION_RELATIVE;			   \
	    coding->composition_rule_follows = 0;			   \
	  }								   \
      }									   \
  } while (0)

/* Handle composition end sequence ESC 1.  */

#define DECODE_COMPOSITION_END(c1)					\
  do {									\
    if (! COMPOSING_P (coding))						\
      {									\
	*dst++ = ISO_CODE_ESC;						\
	*dst++ = c1;							\
	coding->produced_char += 2;					\
      }									\
    else								\
      {									\
	CODING_ADD_COMPOSITION_END (coding, coding->produced_char);	\
	coding->composing = COMPOSITION_NO;				\
      }									\
  } while (0)

/* Decode a composition rule from the byte C1 (and maybe one more byte
   from SRC) and store one encoded composition rule in
   coding->cmp_data.  */

#define DECODE_COMPOSITION_RULE(c1)					\
  do {									\
    int rule = 0;							\
    (c1) -= 32;								\
    if (c1 < 81)		/* old format (before ver.21) */	\
      {									\
	int gref = (c1) / 9;						\
	int nref = (c1) % 9;						\
	if (gref == 4) gref = 10;					\
	if (nref == 4) nref = 10;					\
	rule = COMPOSITION_ENCODE_RULE (gref, nref);			\
      }									\
    else if (c1 < 93)		/* new format (after ver.21) */		\
      {									\
	ONE_MORE_BYTE (c2);						\
	rule = COMPOSITION_ENCODE_RULE (c1 - 81, c2 - 32);		\
      }									\
    CODING_ADD_COMPOSITION_COMPONENT (coding, rule);			\
    coding->composition_rule_follows = 0;				\
  } while (0)


/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_iso2022 (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Charsets invoked to graphic plane 0 and 1 respectively.  */
  int charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
  int charset1 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 1);
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code
     (within macro ONE_MORE_BYTE), or when there's not enough
     destination area to produce a character (within macro
     EMIT_CHAR).  */
  unsigned char *src_base;
  int c, charset;
  Lisp_Object translation_table;
  Lisp_Object safe_chars;

  safe_chars = coding_safe_chars (coding);

  if (NILP (Venable_character_translation))
    translation_table = Qnil;
  else
    {
      translation_table = coding->translation_table_for_decode;
      if (NILP (translation_table))
	translation_table = Vstandard_translation_table_for_decode;
    }

  coding->result = CODING_FINISH_NORMAL;

  while (1)
    {
      int c1, c2;

      src_base = src;
      ONE_MORE_BYTE (c1);

      /* We produce no character or one character.  */
      switch (iso_code_class [c1])
	{
	case ISO_0x20_or_0x7F:
	  if (COMPOSING_P (coding) && coding->composition_rule_follows)
	    {
	      DECODE_COMPOSITION_RULE (c1);
	      continue;
	    }
	  if (charset0 < 0 || CHARSET_CHARS (charset0) == 94)
	    {
	      /* This is SPACE or DEL.  */
	      charset = CHARSET_ASCII;
	      break;
	    }
	  /* This is a graphic character, we fall down ...  */

	case ISO_graphic_plane_0:
	  if (COMPOSING_P (coding) && coding->composition_rule_follows)
	    {
	      DECODE_COMPOSITION_RULE (c1);
	      continue;
	    }
	  charset = charset0;
	  break;

	case ISO_0xA0_or_0xFF:
	  if (charset1 < 0 || CHARSET_CHARS (charset1) == 94
	      || coding->flags & CODING_FLAG_ISO_SEVEN_BITS)
	    goto label_invalid_code;
	  /* This is a graphic character, we fall down ... */

	case ISO_graphic_plane_1:
	  if (charset1 < 0)
	    goto label_invalid_code;
	  charset = charset1;
	  break;

	case ISO_control_0:
	  if (COMPOSING_P (coding))
	    DECODE_COMPOSITION_END ('1');

	  /* All ISO2022 control characters in this class have the
             same representation in Emacs internal format.  */
	  if (c1 == '\n'
	      && (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
	      && (coding->eol_type == CODING_EOL_CR
		  || coding->eol_type == CODING_EOL_CRLF))
	    {
	      coding->result = CODING_FINISH_INCONSISTENT_EOL;
	      goto label_end_of_loop;
	    }
	  charset = CHARSET_ASCII;
	  break;

	case ISO_control_1:
	  if (COMPOSING_P (coding))
	    DECODE_COMPOSITION_END ('1');
	  goto label_invalid_code;

	case ISO_carriage_return:
	  if (COMPOSING_P (coding))
	    DECODE_COMPOSITION_END ('1');

	  if (coding->eol_type == CODING_EOL_CR)
	    c1 = '\n';
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    {
	      ONE_MORE_BYTE (c1);
	      if (c1 != ISO_CODE_LF)
		{
		  if (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		    {
		      coding->result = CODING_FINISH_INCONSISTENT_EOL;
		      goto label_end_of_loop;
		    }
		  src--;
		  c1 = '\r';
		}
	    }
	  charset = CHARSET_ASCII;
	  break;

	case ISO_shift_out:
	  if (! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT)
	      || CODING_SPEC_ISO_DESIGNATION (coding, 1) < 0)
	    goto label_invalid_code;
	  CODING_SPEC_ISO_INVOCATION (coding, 0) = 1;
	  charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	  continue;

	case ISO_shift_in:
	  if (! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT))
	    goto label_invalid_code;
	  CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;
	  charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	  continue;

	case ISO_single_shift_2_7:
	case ISO_single_shift_2:
	  if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT))
	    goto label_invalid_code;
	  /* SS2 is handled as an escape sequence of ESC 'N' */
	  c1 = 'N';
	  goto label_escape_sequence;

	case ISO_single_shift_3:
	  if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT))
	    goto label_invalid_code;
	  /* SS2 is handled as an escape sequence of ESC 'O' */
	  c1 = 'O';
	  goto label_escape_sequence;

	case ISO_control_sequence_introducer:
	  /* CSI is handled as an escape sequence of ESC '[' ...  */
	  c1 = '[';
	  goto label_escape_sequence;

	case ISO_escape:
	  ONE_MORE_BYTE (c1);
	label_escape_sequence:
	  /* Escape sequences handled by Emacs are invocation,
	     designation, direction specification, and character
	     composition specification.  */
	  switch (c1)
	    {
	    case '&':		/* revision of following character set */
	      ONE_MORE_BYTE (c1);
	      if (!(c1 >= '@' && c1 <= '~'))
		goto label_invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 != ISO_CODE_ESC)
		goto label_invalid_code;
	      ONE_MORE_BYTE (c1);
	      goto label_escape_sequence;

	    case '$':		/* designation of 2-byte character set */
	      if (! (coding->flags & CODING_FLAG_ISO_DESIGNATION))
		goto label_invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 >= '@' && c1 <= 'B')
		{	/* designation of JISX0208.1978, GB2312.1980,
			   or JISX0208.1980 */
		  DECODE_DESIGNATION (0, 2, 94, c1);
		}
	      else if (c1 >= 0x28 && c1 <= 0x2B)
		{	/* designation of DIMENSION2_CHARS94 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x28, 2, 94, c2);
		}
	      else if (c1 >= 0x2C && c1 <= 0x2F)
		{	/* designation of DIMENSION2_CHARS96 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x2C, 2, 96, c2);
		}
	      else
		goto label_invalid_code;
	      /* We must update these variables now.  */
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      charset1 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 1);
	      continue;

	    case 'n':		/* invocation of locking-shift-2 */
	      if (! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT)
		  || CODING_SPEC_ISO_DESIGNATION (coding, 2) < 0)
		goto label_invalid_code;
	      CODING_SPEC_ISO_INVOCATION (coding, 0) = 2;
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      continue;

	    case 'o':		/* invocation of locking-shift-3 */
	      if (! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT)
		  || CODING_SPEC_ISO_DESIGNATION (coding, 3) < 0)
		goto label_invalid_code;
	      CODING_SPEC_ISO_INVOCATION (coding, 0) = 3;
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      continue;

	    case 'N':		/* invocation of single-shift-2 */
	      if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT)
		  || CODING_SPEC_ISO_DESIGNATION (coding, 2) < 0)
		goto label_invalid_code;
	      charset = CODING_SPEC_ISO_DESIGNATION (coding, 2);
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto label_invalid_code;
	      break;

	    case 'O':		/* invocation of single-shift-3 */
	      if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT)
		  || CODING_SPEC_ISO_DESIGNATION (coding, 3) < 0)
		goto label_invalid_code;
	      charset = CODING_SPEC_ISO_DESIGNATION (coding, 3);
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto label_invalid_code;
	      break;

	    case '0': case '2':	case '3': case '4': /* start composition */
	      DECODE_COMPOSITION_START (c1);
	      continue;

	    case '1':		/* end composition */
	      DECODE_COMPOSITION_END (c1);
	      continue;

	    case '[':		/* specification of direction */
	      if (coding->flags & CODING_FLAG_ISO_NO_DIRECTION)
		goto label_invalid_code;
	      /* For the moment, nested direction is not supported.
		 So, `coding->mode & CODING_MODE_DIRECTION' zero means
		 left-to-right, and nonzero means right-to-left.  */
	      ONE_MORE_BYTE (c1);
	      switch (c1)
		{
		case ']':	/* end of the current direction */
		  coding->mode &= ~CODING_MODE_DIRECTION;

		case '0':	/* end of the current direction */
		case '1':	/* start of left-to-right direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->mode &= ~CODING_MODE_DIRECTION;
		  else
		    goto label_invalid_code;
		  break;

		case '2':	/* start of right-to-left direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->mode |= CODING_MODE_DIRECTION;
		  else
		    goto label_invalid_code;
		  break;

		default:
		  goto label_invalid_code;
		}
	      continue;

	    default:
	      if (! (coding->flags & CODING_FLAG_ISO_DESIGNATION))
		goto label_invalid_code;
	      if (c1 >= 0x28 && c1 <= 0x2B)
		{	/* designation of DIMENSION1_CHARS94 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x28, 1, 94, c2);
		}
	      else if (c1 >= 0x2C && c1 <= 0x2F)
		{	/* designation of DIMENSION1_CHARS96 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x2C, 1, 96, c2);
		}
	      else
		goto label_invalid_code;
	      /* We must update these variables now.  */
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      charset1 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 1);
	      continue;
	    }
	}

      /* Now we know CHARSET and 1st position code C1 of a character.
         Produce a multibyte sequence for that character while getting
         2nd position code C2 if necessary.  */
      if (CHARSET_DIMENSION (charset) == 2)
	{
	  ONE_MORE_BYTE (c2);
	  if (c1 < 0x80 ? c2 < 0x20 || c2 >= 0x80 : c2 < 0xA0)
	    /* C2 is not in a valid range.  */
	    goto label_invalid_code;
	}
      c = DECODE_ISO_CHARACTER (charset, c1, c2);
      EMIT_CHAR (c);
      continue;

    label_invalid_code:
      coding->errors++;
      if (COMPOSING_P (coding))
	DECODE_COMPOSITION_END ('1');
      src = src_base;
      c = *src++;
      EMIT_CHAR (c);
    }

 label_end_of_loop:
  coding->consumed = coding->consumed_char = src_base - source;
  coding->produced = dst - destination;
  return;
}


/* ISO2022 encoding stuff.  */

/*
   It is not enough to say just "ISO2022" on encoding, we have to
   specify more details.  In Emacs, each ISO2022 coding system
   variant has the following specifications:
	1. Initial designation to G0 through G3.
	2. Allows short-form designation?
	3. ASCII should be designated to G0 before control characters?
	4. ASCII should be designated to G0 at end of line?
	5. 7-bit environment or 8-bit environment?
	6. Use locking-shift?
	7. Use Single-shift?
   And the following two are only for Japanese:
	8. Use ASCII in place of JIS0201-1976-Roman?
	9. Use JISX0208-1983 in place of JISX0208-1978?
   These specifications are encoded in `coding->flags' as flag bits
   defined by macros CODING_FLAG_ISO_XXX.  See `coding.h' for more
   details.
*/

/* Produce codes (escape sequence) for designating CHARSET to graphic
   register REG at DST, and increment DST.  If <final-char> of CHARSET is
   '@', 'A', or 'B' and the coding system CODING allows, produce
   designation sequence of short-form.  */

#define ENCODE_DESIGNATION(charset, reg, coding)			\
  do {									\
    unsigned char final_char = CHARSET_ISO_FINAL_CHAR (charset);	\
    char *intermediate_char_94 = "()*+";				\
    char *intermediate_char_96 = ",-./";				\
    int revision = CODING_SPEC_ISO_REVISION_NUMBER(coding, charset);	\
    									\
    if (revision < 255)							\
      {									\
	*dst++ = ISO_CODE_ESC;						\
	*dst++ = '&';							\
	*dst++ = '@' + revision;					\
      }									\
    *dst++ = ISO_CODE_ESC;						\
    if (CHARSET_DIMENSION (charset) == 1)				\
      {									\
	if (CHARSET_CHARS (charset) == 94)				\
	  *dst++ = (unsigned char) (intermediate_char_94[reg]);		\
	else								\
	  *dst++ = (unsigned char) (intermediate_char_96[reg]);		\
      }									\
    else								\
      {									\
	*dst++ = '$';							\
	if (CHARSET_CHARS (charset) == 94)				\
	  {								\
	    if (! (coding->flags & CODING_FLAG_ISO_SHORT_FORM)		\
		|| reg != 0						\
		|| final_char < '@' || final_char > 'B')		\
	      *dst++ = (unsigned char) (intermediate_char_94[reg]);	\
	  }								\
	else								\
	  *dst++ = (unsigned char) (intermediate_char_96[reg]);		\
      }									\
    *dst++ = final_char;						\
    CODING_SPEC_ISO_DESIGNATION (coding, reg) = charset;		\
  } while (0)

/* The following two macros produce codes (control character or escape
   sequence) for ISO2022 single-shift functions (single-shift-2 and
   single-shift-3).  */

#define ENCODE_SINGLE_SHIFT_2				\
  do {							\
    if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)	\
      *dst++ = ISO_CODE_ESC, *dst++ = 'N';		\
    else						\
      *dst++ = ISO_CODE_SS2;				\
    CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 1;	\
  } while (0)

#define ENCODE_SINGLE_SHIFT_3				\
  do {							\
    if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)	\
      *dst++ = ISO_CODE_ESC, *dst++ = 'O';		\
    else						\
      *dst++ = ISO_CODE_SS3;				\
    CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 1;	\
  } while (0)

/* The following four macros produce codes (control character or
   escape sequence) for ISO2022 locking-shift functions (shift-in,
   shift-out, locking-shift-2, and locking-shift-3).  */

#define ENCODE_SHIFT_IN				\
  do {						\
    *dst++ = ISO_CODE_SI;			\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;	\
  } while (0)

#define ENCODE_SHIFT_OUT			\
  do {						\
    *dst++ = ISO_CODE_SO;			\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 1;	\
  } while (0)

#define ENCODE_LOCKING_SHIFT_2			\
  do {						\
    *dst++ = ISO_CODE_ESC, *dst++ = 'n';	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 2;	\
  } while (0)

#define ENCODE_LOCKING_SHIFT_3			\
  do {						\
    *dst++ = ISO_CODE_ESC, *dst++ = 'o';	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 3;	\
  } while (0)

/* Produce codes for a DIMENSION1 character whose character set is
   CHARSET and whose position-code is C1.  Designation and invocation
   sequences are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION1(charset, c1)			\
  do {									\
    if (CODING_SPEC_ISO_SINGLE_SHIFTING (coding))			\
      {									\
	if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)			\
	  *dst++ = c1 & 0x7F;						\
	else								\
	  *dst++ = c1 | 0x80;						\
	CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (charset == CODING_SPEC_ISO_PLANE_CHARSET (coding, 0))	\
      {									\
	*dst++ = c1 & 0x7F;						\
	break;								\
      }									\
    else if (charset == CODING_SPEC_ISO_PLANE_CHARSET (coding, 1))	\
      {									\
	*dst++ = c1 | 0x80;						\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst);	\
  } while (1)

/* Produce codes for a DIMENSION2 character whose character set is
   CHARSET and whose position-codes are C1 and C2.  Designation and
   invocation codes are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION2(charset, c1, c2)		\
  do {									\
    if (CODING_SPEC_ISO_SINGLE_SHIFTING (coding))			\
      {									\
	if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)			\
	  *dst++ = c1 & 0x7F, *dst++ = c2 & 0x7F;			\
	else								\
	  *dst++ = c1 | 0x80, *dst++ = c2 | 0x80;			\
	CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (charset == CODING_SPEC_ISO_PLANE_CHARSET (coding, 0))	\
      {									\
	*dst++ = c1 & 0x7F, *dst++= c2 & 0x7F;				\
	break;								\
      }									\
    else if (charset == CODING_SPEC_ISO_PLANE_CHARSET (coding, 1))	\
      {									\
	*dst++ = c1 | 0x80, *dst++= c2 | 0x80;				\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst);	\
  } while (1)

#define ENCODE_ISO_CHARACTER(c)					\
  do {								\
    int charset, c1, c2;					\
    								\
    SPLIT_CHAR (c, charset, c1, c2);				\
    if (CHARSET_DEFINED_P (charset))				\
      {								\
	if (CHARSET_DIMENSION (charset) == 1)			\
	  {							\
	    if (charset == CHARSET_ASCII			\
		&& coding->flags & CODING_FLAG_ISO_USE_ROMAN)	\
	      charset = charset_latin_jisx0201;			\
	    ENCODE_ISO_CHARACTER_DIMENSION1 (charset, c1);	\
	  }							\
	else							\
	  {							\
	    if (charset == charset_jisx0208			\
		&& coding->flags & CODING_FLAG_ISO_USE_OLDJIS)	\
	      charset = charset_jisx0208_1978;			\
	    ENCODE_ISO_CHARACTER_DIMENSION2 (charset, c1, c2);	\
	  }							\
      }								\
    else							\
      {								\
	*dst++ = c1;						\
	if (c2 >= 0)						\
	  *dst++ = c2;						\
      }								\
  } while (0)


/* Instead of encoding character C, produce one or two `?'s.  */

#define ENCODE_UNSAFE_CHARACTER(c)					\
  do {									\
    ENCODE_ISO_CHARACTER (CODING_INHIBIT_CHARACTER_SUBSTITUTION);	\
    if (CHARSET_WIDTH (CHAR_CHARSET (c)) > 1)				\
      ENCODE_ISO_CHARACTER (CODING_INHIBIT_CHARACTER_SUBSTITUTION);	\
  } while (0)


/* Produce designation and invocation codes at a place pointed by DST
   to use CHARSET.  The element `spec.iso2022' of *CODING is updated.
   Return new DST.  */

unsigned char *
encode_invocation_designation (charset, coding, dst)
     int charset;
     struct coding_system *coding;
     unsigned char *dst;
{
  int reg;			/* graphic register number */

  /* At first, check designations.  */
  for (reg = 0; reg < 4; reg++)
    if (charset == CODING_SPEC_ISO_DESIGNATION (coding, reg))
      break;

  if (reg >= 4)
    {
      /* CHARSET is not yet designated to any graphic registers.  */
      /* At first check the requested designation.  */
      reg = CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset);
      if (reg == CODING_SPEC_ISO_NO_REQUESTED_DESIGNATION)
	/* Since CHARSET requests no special designation, designate it
	   to graphic register 0.  */
	reg = 0;

      ENCODE_DESIGNATION (charset, reg, coding);
    }

  if (CODING_SPEC_ISO_INVOCATION (coding, 0) != reg
      && CODING_SPEC_ISO_INVOCATION (coding, 1) != reg)
    {
      /* Since the graphic register REG is not invoked to any graphic
	 planes, invoke it to graphic plane 0.  */
      switch (reg)
	{
	case 0:			/* graphic register 0 */
	  ENCODE_SHIFT_IN;
	  break;

	case 1:			/* graphic register 1 */
	  ENCODE_SHIFT_OUT;
	  break;

	case 2:			/* graphic register 2 */
	  if (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_2;
	  else
	    ENCODE_LOCKING_SHIFT_2;
	  break;

	case 3:			/* graphic register 3 */
	  if (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_3;
	  else
	    ENCODE_LOCKING_SHIFT_3;
	  break;
	}
    }

  return dst;
}

/* Produce 2-byte codes for encoded composition rule RULE.  */

#define ENCODE_COMPOSITION_RULE(rule)		\
  do {						\
    int gref, nref;				\
    COMPOSITION_DECODE_RULE (rule, gref, nref);	\
    *dst++ = 32 + 81 + gref;			\
    *dst++ = 32 + nref;				\
  } while (0)

/* Produce codes for indicating the start of a composition sequence
   (ESC 0, ESC 3, or ESC 4).  DATA points to an array of integers
   which specify information about the composition.  See the comment
   in coding.h for the format of DATA.  */

#define ENCODE_COMPOSITION_START(coding, data)				\
  do {									\
    coding->composing = data[3];					\
    *dst++ = ISO_CODE_ESC;						\
    if (coding->composing == COMPOSITION_RELATIVE)			\
      *dst++ = '0';							\
    else								\
      {									\
	*dst++ = (coding->composing == COMPOSITION_WITH_ALTCHARS	\
		  ? '3' : '4');						\
	coding->cmp_data_index = coding->cmp_data_start + 4;		\
	coding->composition_rule_follows = 0;				\
      }									\
  } while (0)

/* Produce codes for indicating the end of the current composition.  */

#define ENCODE_COMPOSITION_END(coding, data)			\
  do {								\
    *dst++ = ISO_CODE_ESC;					\
    *dst++ = '1';						\
    coding->cmp_data_start += data[0];				\
    coding->composing = COMPOSITION_NO;				\
    if (coding->cmp_data_start == coding->cmp_data->used	\
	&& coding->cmp_data->next)				\
      {								\
	coding->cmp_data = coding->cmp_data->next;		\
	coding->cmp_data_start = 0;				\
      }								\
  } while (0)

/* Produce composition start sequence ESC 0.  Here, this sequence
   doesn't mean the start of a new composition but means that we have
   just produced components (alternate chars and composition rules) of
   the composition and the actual text follows in SRC.  */

#define ENCODE_COMPOSITION_FAKE_START(coding)	\
  do {						\
    *dst++ = ISO_CODE_ESC;			\
    *dst++ = '0';				\
    coding->composing = COMPOSITION_RELATIVE;	\
  } while (0)

/* The following three macros produce codes for indicating direction
   of text.  */
#define ENCODE_CONTROL_SEQUENCE_INTRODUCER		\
  do {							\
    if (coding->flags == CODING_FLAG_ISO_SEVEN_BITS)	\
      *dst++ = ISO_CODE_ESC, *dst++ = '[';		\
    else						\
      *dst++ = ISO_CODE_CSI;				\
  } while (0)

#define ENCODE_DIRECTION_R2L	\
  ENCODE_CONTROL_SEQUENCE_INTRODUCER (dst), *dst++ = '2', *dst++ = ']'

#define ENCODE_DIRECTION_L2R	\
  ENCODE_CONTROL_SEQUENCE_INTRODUCER (dst), *dst++ = '0', *dst++ = ']'

/* Produce codes for designation and invocation to reset the graphic
   planes and registers to initial state.  */
#define ENCODE_RESET_PLANE_AND_REGISTER					    \
  do {									    \
    int reg;								    \
    if (CODING_SPEC_ISO_INVOCATION (coding, 0) != 0)			    \
      ENCODE_SHIFT_IN;							    \
    for (reg = 0; reg < 4; reg++)					    \
      if (CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, reg) >= 0	    \
	  && (CODING_SPEC_ISO_DESIGNATION (coding, reg)			    \
	      != CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, reg)))	    \
	ENCODE_DESIGNATION						    \
	  (CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, reg), reg, coding); \
  } while (0)

/* Produce designation sequences of charsets in the line started from
   SRC to a place pointed by DST, and return updated DST.

   If the current block ends before any end-of-line, we may fail to
   find all the necessary designations.  */

static unsigned char *
encode_designation_at_bol (coding, translation_table, src, src_end, dst)
     struct coding_system *coding;
     Lisp_Object translation_table;
     unsigned char *src, *src_end, *dst;
{
  int charset, c, found = 0, reg;
  /* Table of charsets to be designated to each graphic register.  */
  int r[4];

  for (reg = 0; reg < 4; reg++)
    r[reg] = -1;

  while (found < 4)
    {
      ONE_MORE_CHAR (c);
      if (c == '\n')
	break;

      charset = CHAR_CHARSET (c);
      reg = CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset);
      if (reg != CODING_SPEC_ISO_NO_REQUESTED_DESIGNATION && r[reg] < 0)
	{
	  found++;
	  r[reg] = charset;
	}
    }

 label_end_of_loop:
  if (found)
    {
      for (reg = 0; reg < 4; reg++)
	if (r[reg] >= 0
	    && CODING_SPEC_ISO_DESIGNATION (coding, reg) != r[reg])
	  ENCODE_DESIGNATION (r[reg], reg, coding);
    }

  return dst;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".  */

static void
encode_coding_iso2022 (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Since the maximum bytes produced by each loop is 20, we subtract 19
     from DST_END to assure overflow checking is necessary only at the
     head of loop.  */
  unsigned char *adjusted_dst_end = dst_end - 19;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source text to
     analyze multi-byte codes (within macro ONE_MORE_CHAR), or when
     there's not enough destination area to produce encoded codes
     (within macro EMIT_BYTES).  */
  unsigned char *src_base;
  int c;
  Lisp_Object translation_table;
  Lisp_Object safe_chars;

  safe_chars = coding_safe_chars (coding);

  if (NILP (Venable_character_translation))
    translation_table = Qnil;
  else
    {
      translation_table = coding->translation_table_for_encode;
      if (NILP (translation_table))
	translation_table = Vstandard_translation_table_for_encode;
    }

  coding->consumed_char = 0;
  coding->errors = 0;
  while (1)
    {
      src_base = src;

      if (dst >= (dst_bytes ? adjusted_dst_end : (src - 19)))
	{
	  coding->result = CODING_FINISH_INSUFFICIENT_DST;
	  break;
	}

      if (coding->flags & CODING_FLAG_ISO_DESIGNATE_AT_BOL
	  && CODING_SPEC_ISO_BOL (coding))
	{
	  /* We have to produce designation sequences if any now.  */
	  dst = encode_designation_at_bol (coding, translation_table,
					   src, src_end, dst);
	  CODING_SPEC_ISO_BOL (coding) = 0;
	}

      /* Check composition start and end.  */
      if (coding->composing != COMPOSITION_DISABLED
	  && coding->cmp_data_start < coding->cmp_data->used)
	{
	  struct composition_data *cmp_data = coding->cmp_data;
	  int *data = cmp_data->data + coding->cmp_data_start;
	  int this_pos = cmp_data->char_offset + coding->consumed_char;

	  if (coding->composing == COMPOSITION_RELATIVE)
	    {
	      if (this_pos == data[2])
		{
		  ENCODE_COMPOSITION_END (coding, data);
		  cmp_data = coding->cmp_data;
		  data = cmp_data->data + coding->cmp_data_start;
		}
	    }
	  else if (COMPOSING_P (coding))
	    {
	      /* COMPOSITION_WITH_ALTCHARS or COMPOSITION_WITH_RULE_ALTCHAR  */
	      if (coding->cmp_data_index == coding->cmp_data_start + data[0])
		/* We have consumed components of the composition.
                   What follows in SRC is the composition's base
                   text.  */
		ENCODE_COMPOSITION_FAKE_START (coding);
	      else
		{
		  int c = cmp_data->data[coding->cmp_data_index++];
		  if (coding->composition_rule_follows)
		    {
		      ENCODE_COMPOSITION_RULE (c);
		      coding->composition_rule_follows = 0;
		    }
		  else
		    {
		      if (coding->flags & CODING_FLAG_ISO_SAFE
			  && ! CODING_SAFE_CHAR_P (safe_chars, c))
			ENCODE_UNSAFE_CHARACTER (c);
		      else
			ENCODE_ISO_CHARACTER (c);
		      if (coding->composing == COMPOSITION_WITH_RULE_ALTCHARS)
			coding->composition_rule_follows = 1;
		    }
		  continue;
		}
	    }
	  if (!COMPOSING_P (coding))
	    {
	      if (this_pos == data[1])
		{
		  ENCODE_COMPOSITION_START (coding, data);
		  continue;
		}
	    }
	}

      ONE_MORE_CHAR (c);

      /* Now encode the character C.  */
      if (c < 0x20 || c == 0x7F)
	{
	  if (c == '\r')
	    {
	      if (! (coding->mode & CODING_MODE_SELECTIVE_DISPLAY))
		{
		  if (coding->flags & CODING_FLAG_ISO_RESET_AT_CNTL)
		    ENCODE_RESET_PLANE_AND_REGISTER;
		  *dst++ = c;
		  continue;
		}
	      /* fall down to treat '\r' as '\n' ...  */
	      c = '\n';
	    }
	  if (c == '\n')
	    {
	      if (coding->flags & CODING_FLAG_ISO_RESET_AT_EOL)
		ENCODE_RESET_PLANE_AND_REGISTER;
	      if (coding->flags & CODING_FLAG_ISO_INIT_AT_BOL)
		bcopy (coding->spec.iso2022.initial_designation,
		       coding->spec.iso2022.current_designation,
		       sizeof coding->spec.iso2022.initial_designation);
	      if (coding->eol_type == CODING_EOL_LF
		  || coding->eol_type == CODING_EOL_UNDECIDED)
		*dst++ = ISO_CODE_LF;
	      else if (coding->eol_type == CODING_EOL_CRLF)
		*dst++ = ISO_CODE_CR, *dst++ = ISO_CODE_LF;
	      else
		*dst++ = ISO_CODE_CR;
	      CODING_SPEC_ISO_BOL (coding) = 1;
	    }
	  else
	    {
	      if (coding->flags & CODING_FLAG_ISO_RESET_AT_CNTL)
		ENCODE_RESET_PLANE_AND_REGISTER;
	      *dst++ = c;
	    }
	}
      else if (ASCII_BYTE_P (c))
	ENCODE_ISO_CHARACTER (c);
      else if (SINGLE_BYTE_CHAR_P (c))
	{
	  *dst++ = c;
	  coding->errors++;
	}
      else if (coding->flags & CODING_FLAG_ISO_SAFE
	       && ! CODING_SAFE_CHAR_P (safe_chars, c))
	ENCODE_UNSAFE_CHARACTER (c);
      else
	ENCODE_ISO_CHARACTER (c);

      coding->consumed_char++;
    }

 label_end_of_loop:
  coding->consumed = src_base - source;
  coding->produced = coding->produced_char = dst - destination;
}


/*** 4. SJIS and BIG5 handlers ***/

/* Although SJIS and BIG5 are not ISO coding systems, they are used
   quite widely.  So, for the moment, Emacs supports them in the bare
   C code.  But, in the future, they may be supported only by CCL.  */

/* SJIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of charset katakana-jisx0201 is encoded by
   "position-code + 0x80".  A character of charset japanese-jisx0208
   is encoded in 2-byte but two position-codes are divided and shifted
   so that it fits in the range below.

   --- CODE RANGE of SJIS ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   KATAKANA-JISX0201	0xA1 .. 0xDF
   JISX0208 (1st byte)	0x81 .. 0x9F and 0xE0 .. 0xEF
	    (2nd byte)	0x40 .. 0x7E and 0x80 .. 0xFC
   -------------------------------

*/

/* BIG5 is a coding system encoding two character sets: ASCII and
   Big5.  An ASCII character is encoded as is.  Big5 is a two-byte
   character set and is encoded in two bytes.

   --- CODE RANGE of BIG5 ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   Big5 (1st byte)	0xA1 .. 0xFE
	(2nd byte)	0x40 .. 0x7E and 0xA1 .. 0xFE
   --------------------------

   Since the number of characters in Big5 is larger than maximum
   characters in Emacs' charset (96x96), it can't be handled as one
   charset.  So, in Emacs, Big5 is divided into two: `charset-big5-1'
   and `charset-big5-2'.  Both are DIMENSION2 and CHARS94.  The former
   contains frequently used characters and the latter contains less
   frequently used characters.  */

/* Macros to decode or encode a character of Big5 in BIG5.  B1 and B2
   are the 1st and 2nd position-codes of Big5 in BIG5 coding system.
   C1 and C2 are the 1st and 2nd position-codes of of Emacs' internal
   format.  CHARSET is `charset_big5_1' or `charset_big5_2'.  */

/* Number of Big5 characters which have the same code in 1st byte.  */
#define BIG5_SAME_ROW (0xFF - 0xA1 + 0x7F - 0x40)

#define DECODE_BIG5(b1, b2, charset, c1, c2)			     	\
  do {								     	\
    unsigned int temp						     	\
      = (b1 - 0xA1) * BIG5_SAME_ROW + b2 - (b2 < 0x7F ? 0x40 : 0x62);	\
    if (b1 < 0xC9)						     	\
      charset = charset_big5_1;					     	\
    else							     	\
      {								     	\
	charset = charset_big5_2;				     	\
	temp -= (0xC9 - 0xA1) * BIG5_SAME_ROW;			     	\
      }								     	\
    c1 = temp / (0xFF - 0xA1) + 0x21;				     	\
    c2 = temp % (0xFF - 0xA1) + 0x21;				     	\
  } while (0)

#define ENCODE_BIG5(charset, c1, c2, b1, b2)			  	\
  do {								  	\
    unsigned int temp = (c1 - 0x21) * (0xFF - 0xA1) + (c2 - 0x21);	\
    if (charset == charset_big5_2)				  	\
      temp += BIG5_SAME_ROW * (0xC9 - 0xA1);			  	\
    b1 = temp / BIG5_SAME_ROW + 0xA1;				  	\
    b2 = temp % BIG5_SAME_ROW;					  	\
    b2 += b2 < 0x3F ? 0x40 : 0x62;				  	\
  } while (0)

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in SJIS.  If it is, return
   CODING_CATEGORY_MASK_SJIS, else return 0.  */

static int
detect_coding_sjis (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  int c;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  while (1)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      if (c < 0x80)
	continue;
      if (c == 0x80 || c == 0xA0 || c > 0xEF)
	return 0;
      if (c <= 0x9F || c >= 0xE0)
	{
	  ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
	  if (c < 0x40 || c == 0x7F || c > 0xFC)
	    return 0;
	}
    }
 label_end_of_loop:
  return CODING_CATEGORY_MASK_SJIS;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in BIG5.  If it is, return
   CODING_CATEGORY_MASK_BIG5, else return 0.  */

static int
detect_coding_big5 (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  int c;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  while (1)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      if (c < 0x80)
	continue;
      if (c < 0xA1 || c > 0xFE)
	return 0;
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      if (c < 0x40 || (c > 0x7F && c < 0xA1) || c > 0xFE)
	return 0;
    }
 label_end_of_loop:
  return CODING_CATEGORY_MASK_BIG5;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in UTF-8.  If it is, return
   CODING_CATEGORY_MASK_UTF_8, else return 0.  */

#define UTF_8_1_OCTET_P(c)         ((c) < 0x80)
#define UTF_8_EXTRA_OCTET_P(c)     (((c) & 0xC0) == 0x80)
#define UTF_8_2_OCTET_LEADING_P(c) (((c) & 0xE0) == 0xC0)
#define UTF_8_3_OCTET_LEADING_P(c) (((c) & 0xF0) == 0xE0)
#define UTF_8_4_OCTET_LEADING_P(c) (((c) & 0xF8) == 0xF0)
#define UTF_8_5_OCTET_LEADING_P(c) (((c) & 0xFC) == 0xF8)
#define UTF_8_6_OCTET_LEADING_P(c) (((c) & 0xFE) == 0xFC)

static int
detect_coding_utf_8 (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  unsigned char c;
  int seq_maybe_bytes;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  while (1)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      if (UTF_8_1_OCTET_P (c))
	continue;
      else if (UTF_8_2_OCTET_LEADING_P (c))
	seq_maybe_bytes = 1;
      else if (UTF_8_3_OCTET_LEADING_P (c))
	seq_maybe_bytes = 2;
      else if (UTF_8_4_OCTET_LEADING_P (c))
	seq_maybe_bytes = 3;
      else if (UTF_8_5_OCTET_LEADING_P (c))
	seq_maybe_bytes = 4;
      else if (UTF_8_6_OCTET_LEADING_P (c))
	seq_maybe_bytes = 5;
      else
	return 0;

      do
	{
	  ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
	  if (!UTF_8_EXTRA_OCTET_P (c))
	    return 0;
	  seq_maybe_bytes--;
	}
      while (seq_maybe_bytes > 0);
    }

 label_end_of_loop:
  return CODING_CATEGORY_MASK_UTF_8;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in UTF-16 Big Endian (endian == 1) or
   Little Endian (otherwise).  If it is, return
   CODING_CATEGORY_MASK_UTF_16_BE or CODING_CATEGORY_MASK_UTF_16_LE,
   else return 0.  */

#define UTF_16_INVALID_P(val)	\
  (((val) == 0xFFFE)		\
   || ((val) == 0xFFFF))

#define UTF_16_HIGH_SURROGATE_P(val) \
  (((val) & 0xD800) == 0xD800)

#define UTF_16_LOW_SURROGATE_P(val) \
  (((val) & 0xDC00) == 0xDC00)

static int
detect_coding_utf_16 (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  unsigned char c1, c2;
  /* Dummy for TWO_MORE_BYTES.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  ONE_MORE_BYTE_CHECK_MULTIBYTE (c1, multibytep);
  ONE_MORE_BYTE_CHECK_MULTIBYTE (c2, multibytep);

  if ((c1 == 0xFF) && (c2 == 0xFE))
    return CODING_CATEGORY_MASK_UTF_16_LE;
  else if ((c1 == 0xFE) && (c2 == 0xFF))
    return CODING_CATEGORY_MASK_UTF_16_BE;

 label_end_of_loop:
  return 0;
}

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".
   If SJIS_P is 1, decode SJIS text, else decode BIG5 test.  */

static void
decode_coding_sjis_big5 (coding, source, destination,
			 src_bytes, dst_bytes, sjis_p)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int sjis_p;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code
     (within macro ONE_MORE_BYTE), or when there's not enough
     destination area to produce a character (within macro
     EMIT_CHAR).  */
  unsigned char *src_base;
  Lisp_Object translation_table;

  if (NILP (Venable_character_translation))
    translation_table = Qnil;
  else
    {
      translation_table = coding->translation_table_for_decode;
      if (NILP (translation_table))
	translation_table = Vstandard_translation_table_for_decode;
    }

  coding->produced_char = 0;
  while (1)
    {
      int c, charset, c1, c2;

      src_base = src;
      ONE_MORE_BYTE (c1);

      if (c1 < 0x80)
	{
	  charset = CHARSET_ASCII;
	  if (c1 < 0x20)
	    {
	      if (c1 == '\r')
		{
		  if (coding->eol_type == CODING_EOL_CRLF)
		    {
		      ONE_MORE_BYTE (c2);
		      if (c2 == '\n')
			c1 = c2;
		      else if (coding->mode
			       & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
			{
			  coding->result = CODING_FINISH_INCONSISTENT_EOL;
			  goto label_end_of_loop;
			}
		      else
			/* To process C2 again, SRC is subtracted by 1.  */
			src--;
		    }
		  else if (coding->eol_type == CODING_EOL_CR)
		    c1 = '\n';
		}
	      else if (c1 == '\n'
		       && (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		       && (coding->eol_type == CODING_EOL_CR
			   || coding->eol_type == CODING_EOL_CRLF))
		{
		  coding->result = CODING_FINISH_INCONSISTENT_EOL;
		  goto label_end_of_loop;
		}
	    }
	}
      else
        {
	  if (sjis_p)
	    {
	      if (c1 == 0x80 || c1 == 0xA0 || c1 > 0xEF)
		goto label_invalid_code;
	      if (c1 <= 0x9F || c1 >= 0xE0)
		{
		  /* SJIS -> JISX0208 */
		  ONE_MORE_BYTE (c2);
		  if (c2 < 0x40 || c2 == 0x7F || c2 > 0xFC)
		    goto label_invalid_code;
		  DECODE_SJIS (c1, c2, c1, c2);
		  charset = charset_jisx0208;
		}
	      else
		/* SJIS -> JISX0201-Kana */
		charset = charset_katakana_jisx0201;
	    }
	  else
	    {
	      /* BIG5 -> Big5 */
	      if (c1 < 0xA0 || c1 > 0xFE)
		goto label_invalid_code;
	      ONE_MORE_BYTE (c2);
	      if (c2 < 0x40 || (c2 > 0x7E && c2 < 0xA1) || c2 > 0xFE)
		goto label_invalid_code;
	      DECODE_BIG5 (c1, c2, charset, c1, c2);
	    }
	}

      c = DECODE_ISO_CHARACTER (charset, c1, c2);
      EMIT_CHAR (c);
      continue;

    label_invalid_code:
      coding->errors++;
      src = src_base;
      c = *src++;
      EMIT_CHAR (c);
    }

 label_end_of_loop:
  coding->consumed = coding->consumed_char = src_base - source;
  coding->produced = dst - destination;
  return;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".
   This function can encode charsets `ascii', `katakana-jisx0201',
   `japanese-jisx0208', `chinese-big5-1', and `chinese-big5-2'.  We
   are sure that all these charsets are registered as official charset
   (i.e. do not have extended leading-codes).  Characters of other
   charsets are produced without any encoding.  If SJIS_P is 1, encode
   SJIS text, else encode BIG5 text.  */

static void
encode_coding_sjis_big5 (coding, source, destination,
			 src_bytes, dst_bytes, sjis_p)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int sjis_p;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source text to
     analyze multi-byte codes (within macro ONE_MORE_CHAR), or when
     there's not enough destination area to produce encoded codes
     (within macro EMIT_BYTES).  */
  unsigned char *src_base;
  Lisp_Object translation_table;

  if (NILP (Venable_character_translation))
    translation_table = Qnil;
  else
    {
      translation_table = coding->translation_table_for_encode;
      if (NILP (translation_table))
	translation_table = Vstandard_translation_table_for_encode;
    }

  while (1)
    {
      int c, charset, c1, c2;

      src_base = src;
      ONE_MORE_CHAR (c);

      /* Now encode the character C.  */
      if (SINGLE_BYTE_CHAR_P (c))
	{
	  switch (c)
	    {
	    case '\r':
	      if (!(coding->mode & CODING_MODE_SELECTIVE_DISPLAY))
		{
		  EMIT_ONE_BYTE (c);
		  break;
		}
	      c = '\n';
	    case '\n':
	      if (coding->eol_type == CODING_EOL_CRLF)
		{
		  EMIT_TWO_BYTES ('\r', c);
		  break;
		}
	      else if (coding->eol_type == CODING_EOL_CR)
		c = '\r';
	    default:
	      EMIT_ONE_BYTE (c);
	    }
	}
      else
	{
	  SPLIT_CHAR (c, charset, c1, c2);
	  if (sjis_p)
	    {
	      if (charset == charset_jisx0208
		  || charset == charset_jisx0208_1978)
		{
		  ENCODE_SJIS (c1, c2, c1, c2);
		  EMIT_TWO_BYTES (c1, c2);
		}
	      else if (charset == charset_katakana_jisx0201)
		EMIT_ONE_BYTE (c1 | 0x80);
	      else if (charset == charset_latin_jisx0201)
		EMIT_ONE_BYTE (c1);
	      else
		/* There's no way other than producing the internal
		   codes as is.  */
		EMIT_BYTES (src_base, src);
	    }
	  else
	    {
	      if (charset == charset_big5_1 || charset == charset_big5_2)
		{
		  ENCODE_BIG5 (charset, c1, c2, c1, c2);
		  EMIT_TWO_BYTES (c1, c2);
		}
	      else
		/* There's no way other than producing the internal
		   codes as is.  */
		EMIT_BYTES (src_base, src);
	    }
	}
      coding->consumed_char++;
    }

 label_end_of_loop:
  coding->consumed = src_base - source;
  coding->produced = coding->produced_char = dst - destination;
}


/*** 5. CCL handlers ***/

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in a coding system of which
   encoder/decoder are written in CCL program.  If it is, return
   CODING_CATEGORY_MASK_CCL, else return 0.  */

static int
detect_coding_ccl (src, src_end, multibytep)
     unsigned char *src, *src_end;
     int multibytep;
{
  unsigned char *valid;
  int c;
  /* Dummy for ONE_MORE_BYTE.  */
  struct coding_system dummy_coding;
  struct coding_system *coding = &dummy_coding;

  /* No coding system is assigned to coding-category-ccl.  */
  if (!coding_system_table[CODING_CATEGORY_IDX_CCL])
    return 0;

  valid = coding_system_table[CODING_CATEGORY_IDX_CCL]->spec.ccl.valid_codes;
  while (1)
    {
      ONE_MORE_BYTE_CHECK_MULTIBYTE (c, multibytep);
      if (! valid[c])
	return 0;
    }
 label_end_of_loop:
  return CODING_CATEGORY_MASK_CCL;
}


/*** 6. End-of-line handlers ***/

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_eol (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *dst = destination;
  unsigned char *src_end = src + src_bytes;
  unsigned char *dst_end = dst + dst_bytes;
  Lisp_Object translation_table;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code
     (within macro ONE_MORE_BYTE), or when there's not enough
     destination area to produce a character (within macro
     EMIT_CHAR).  */
  unsigned char *src_base;
  int c;

  translation_table = Qnil;
  switch (coding->eol_type)
    {
    case CODING_EOL_CRLF:
      while (1)
	{
	  src_base = src;
	  ONE_MORE_BYTE (c);
	  if (c == '\r')
	    {
	      ONE_MORE_BYTE (c);
	      if (c != '\n')
		{
		  if (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		    {
		      coding->result = CODING_FINISH_INCONSISTENT_EOL;
		      goto label_end_of_loop;
		    }
		  src--;
		  c = '\r';
		}
	    }
	  else if (c == '\n'
		   && (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL))
	    {
	      coding->result = CODING_FINISH_INCONSISTENT_EOL;
	      goto label_end_of_loop;
	    }
	  EMIT_CHAR (c);
	}
      break;

    case CODING_EOL_CR:
      while (1)
	{
	  src_base = src;
	  ONE_MORE_BYTE (c);
	  if (c == '\n')
	    {
	      if (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		{
		  coding->result = CODING_FINISH_INCONSISTENT_EOL;
		  goto label_end_of_loop;
		}
	    }
	  else if (c == '\r')
	    c = '\n';
	  EMIT_CHAR (c);
	}
      break;

    default:			/* no need for EOL handling */
      while (1)
	{
	  src_base = src;
	  ONE_MORE_BYTE (c);
	  EMIT_CHAR (c);
	}
    }

 label_end_of_loop:
  coding->consumed = coding->consumed_char = src_base - source;
  coding->produced = dst - destination;
  return;
}

/* See "GENERAL NOTES about `encode_coding_XXX ()' functions".  Encode
   format of end-of-line according to `coding->eol_type'.  It also
   convert multibyte form 8-bit characters to unibyte if
   CODING->src_multibyte is nonzero.  If `coding->mode &
   CODING_MODE_SELECTIVE_DISPLAY' is nonzero, code '\r' in source text
   also means end-of-line.  */

static void
encode_eol (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  unsigned char *src = source;
  unsigned char *dst = destination;
  unsigned char *src_end = src + src_bytes;
  unsigned char *dst_end = dst + dst_bytes;
  Lisp_Object translation_table;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source text to
     analyze multi-byte codes (within macro ONE_MORE_CHAR), or when
     there's not enough destination area to produce encoded codes
     (within macro EMIT_BYTES).  */
  unsigned char *src_base;
  int c;
  int selective_display = coding->mode & CODING_MODE_SELECTIVE_DISPLAY;

  translation_table = Qnil;
  if (coding->src_multibyte
      && *(src_end - 1) == LEADING_CODE_8_BIT_CONTROL)
    {
      src_end--;
      src_bytes--;
      coding->result = CODING_FINISH_INSUFFICIENT_SRC;
    }

  if (coding->eol_type == CODING_EOL_CRLF)
    {
      while (src < src_end)
	{
	  src_base = src;
	  c = *src++;
	  if (c >= 0x20)
	    EMIT_ONE_BYTE (c);
	  else if (c == '\n' || (c == '\r' && selective_display))
	    EMIT_TWO_BYTES ('\r', '\n');
	  else
	    EMIT_ONE_BYTE (c);
	}
      src_base = src;
    label_end_of_loop:
      ;
    }
  else
    {
      if (!dst_bytes || src_bytes <= dst_bytes)
	{
	  safe_bcopy (src, dst, src_bytes);
	  src_base = src_end;
	  dst += src_bytes;
	}
      else
	{
	  if (coding->src_multibyte
	      && *(src + dst_bytes - 1) == LEADING_CODE_8_BIT_CONTROL)
	    dst_bytes--;
	  safe_bcopy (src, dst, dst_bytes);
	  src_base = src + dst_bytes;
	  dst = destination + dst_bytes;
	  coding->result = CODING_FINISH_INSUFFICIENT_DST;
	}
      if (coding->eol_type == CODING_EOL_CR)
	{
	  for (src = destination; src < dst; src++)
	    if (*src == '\n') *src = '\r';
	}
      else if (selective_display)
	{
	  for (src = destination; src < dst; src++)
	    if (*src == '\r') *src = '\n';
	}
    }
  if (coding->src_multibyte)
    dst = destination + str_as_unibyte (destination, dst - destination);

  coding->consumed = src_base - source;
  coding->produced = dst - destination;
  coding->produced_char = coding->produced;
}


/*** 7. C library functions ***/

/* In Emacs Lisp, a coding system is represented by a Lisp symbol which
   has a property `coding-system'.  The value of this property is a
   vector of length 5 (called the coding-vector).  Among elements of
   this vector, the first (element[0]) and the fifth (element[4])
   carry important information for decoding/encoding.  Before
   decoding/encoding, this information should be set in fields of a
   structure of type `coding_system'.

   The value of the property `coding-system' can be a symbol of another
   subsidiary coding-system.  In that case, Emacs gets coding-vector
   from that symbol.

   `element[0]' contains information to be set in `coding->type'.  The
   value and its meaning is as follows:

   0 -- coding_type_emacs_mule
   1 -- coding_type_sjis
   2 -- coding_type_iso2022
   3 -- coding_type_big5
   4 -- coding_type_ccl encoder/decoder written in CCL
   nil -- coding_type_no_conversion
   t -- coding_type_undecided (automatic conversion on decoding,
   			       no-conversion on encoding)

   `element[4]' contains information to be set in `coding->flags' and
   `coding->spec'.  The meaning varies by `coding->type'.

   If `coding->type' is `coding_type_iso2022', element[4] is a vector
   of length 32 (of which the first 13 sub-elements are used now).
   Meanings of these sub-elements are:

   sub-element[N] where N is 0 through 3: to be set in `coding->spec.iso2022'
   	If the value is an integer of valid charset, the charset is
	assumed to be designated to graphic register N initially.

	If the value is minus, it is a minus value of charset which
	reserves graphic register N, which means that the charset is
	not designated initially but should be designated to graphic
	register N just before encoding a character in that charset.

	If the value is nil, graphic register N is never used on
	encoding.

   sub-element[N] where N is 4 through 11: to be set in `coding->flags'
   	Each value takes t or nil.  See the section ISO2022 of
	`coding.h' for more information.

   If `coding->type' is `coding_type_big5', element[4] is t to denote
   BIG5-ETen or nil to denote BIG5-HKU.

   If `coding->type' takes the other value, element[4] is ignored.

   Emacs Lisp's coding systems also carry information about format of
   end-of-line in a value of property `eol-type'.  If the value is
   integer, 0 means CODING_EOL_LF, 1 means CODING_EOL_CRLF, and 2
   means CODING_EOL_CR.  If it is not integer, it should be a vector
   of subsidiary coding systems of which property `eol-type' has one
   of the above values.

*/

/* Extract information for decoding/encoding from CODING_SYSTEM_SYMBOL
   and set it in CODING.  If CODING_SYSTEM_SYMBOL is invalid, CODING
   is setup so that no conversion is necessary and return -1, else
   return 0.  */

int
setup_coding_system (coding_system, coding)
     Lisp_Object coding_system;
     struct coding_system *coding;
{
  Lisp_Object coding_spec, coding_type, eol_type, plist;
  Lisp_Object val;

  /* At first, zero clear all members.  */
  bzero (coding, sizeof (struct coding_system));

  /* Initialize some fields required for all kinds of coding systems.  */
  coding->symbol = coding_system;
  coding->heading_ascii = -1;
  coding->post_read_conversion = coding->pre_write_conversion = Qnil;
  coding->composing = COMPOSITION_DISABLED;
  coding->cmp_data = NULL;

  if (NILP (coding_system))
    goto label_invalid_coding_system;

  coding_spec = Fget (coding_system, Qcoding_system);

  if (!VECTORP (coding_spec)
      || XVECTOR (coding_spec)->size != 5
      || !CONSP (XVECTOR (coding_spec)->contents[3]))
    goto label_invalid_coding_system;

  eol_type = inhibit_eol_conversion ? Qnil : Fget (coding_system, Qeol_type);
  if (VECTORP (eol_type))
    {
      coding->eol_type = CODING_EOL_UNDECIDED;
      coding->common_flags = CODING_REQUIRE_DETECTION_MASK;
    }
  else if (XFASTINT (eol_type) == 1)
    {
      coding->eol_type = CODING_EOL_CRLF;
      coding->common_flags
	= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
    }
  else if (XFASTINT (eol_type) == 2)
    {
      coding->eol_type = CODING_EOL_CR;
      coding->common_flags
	= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
    }
  else
    coding->eol_type = CODING_EOL_LF;

  coding_type = XVECTOR (coding_spec)->contents[0];
  /* Try short cut.  */
  if (SYMBOLP (coding_type))
    {
      if (EQ (coding_type, Qt))
	{
	  coding->type = coding_type_undecided;
	  coding->common_flags |= CODING_REQUIRE_DETECTION_MASK;
	}
      else
	coding->type = coding_type_no_conversion;
      /* Initialize this member.  Any thing other than
	 CODING_CATEGORY_IDX_UTF_16_BE and
	 CODING_CATEGORY_IDX_UTF_16_LE are ok because they have
	 special treatment in detect_eol.  */
      coding->category_idx = CODING_CATEGORY_IDX_EMACS_MULE;

      return 0;
    }

  /* Get values of coding system properties:
     `post-read-conversion', `pre-write-conversion',
     `translation-table-for-decode', `translation-table-for-encode'.  */
  plist = XVECTOR (coding_spec)->contents[3];
  /* Pre & post conversion functions should be disabled if
     inhibit_eol_conversion is nonzero.  This is the case that a code
     conversion function is called while those functions are running.  */
  if (! inhibit_pre_post_conversion)
    {
      coding->post_read_conversion = Fplist_get (plist, Qpost_read_conversion);
      coding->pre_write_conversion = Fplist_get (plist, Qpre_write_conversion);
    }
  val = Fplist_get (plist, Qtranslation_table_for_decode);
  if (SYMBOLP (val))
    val = Fget (val, Qtranslation_table_for_decode);
  coding->translation_table_for_decode = CHAR_TABLE_P (val) ? val : Qnil;
  val = Fplist_get (plist, Qtranslation_table_for_encode);
  if (SYMBOLP (val))
    val = Fget (val, Qtranslation_table_for_encode);
  coding->translation_table_for_encode = CHAR_TABLE_P (val) ? val : Qnil;
  val = Fplist_get (plist, Qcoding_category);
  if (!NILP (val))
    {
      val = Fget (val, Qcoding_category_index);
      if (INTEGERP (val))
	coding->category_idx = XINT (val);
      else
	goto label_invalid_coding_system;
    }
  else
    goto label_invalid_coding_system;

  /* If the coding system has non-nil `composition' property, enable
     composition handling.  */
  val = Fplist_get (plist, Qcomposition);
  if (!NILP (val))
    coding->composing = COMPOSITION_NO;

  switch (XFASTINT (coding_type))
    {
    case 0:
      coding->type = coding_type_emacs_mule;
      coding->common_flags
	|= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
      coding->composing = COMPOSITION_NO;
      if (!NILP (coding->post_read_conversion))
	coding->common_flags |= CODING_REQUIRE_DECODING_MASK;
      if (!NILP (coding->pre_write_conversion))
	coding->common_flags |= CODING_REQUIRE_ENCODING_MASK;
      break;

    case 1:
      coding->type = coding_type_sjis;
      coding->common_flags
	|= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
      break;

    case 2:
      coding->type = coding_type_iso2022;
      coding->common_flags
	|= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
      {
	Lisp_Object val, temp;
	Lisp_Object *flags;
	int i, charset, reg_bits = 0;

	val = XVECTOR (coding_spec)->contents[4];

	if (!VECTORP (val) || XVECTOR (val)->size != 32)
	  goto label_invalid_coding_system;

	flags = XVECTOR (val)->contents;
	coding->flags
	  = ((NILP (flags[4]) ? 0 : CODING_FLAG_ISO_SHORT_FORM)
	     | (NILP (flags[5]) ? 0 : CODING_FLAG_ISO_RESET_AT_EOL)
	     | (NILP (flags[6]) ? 0 : CODING_FLAG_ISO_RESET_AT_CNTL)
	     | (NILP (flags[7]) ? 0 : CODING_FLAG_ISO_SEVEN_BITS)
	     | (NILP (flags[8]) ? 0 : CODING_FLAG_ISO_LOCKING_SHIFT)
	     | (NILP (flags[9]) ? 0 : CODING_FLAG_ISO_SINGLE_SHIFT)
	     | (NILP (flags[10]) ? 0 : CODING_FLAG_ISO_USE_ROMAN)
	     | (NILP (flags[11]) ? 0 : CODING_FLAG_ISO_USE_OLDJIS)
	     | (NILP (flags[12]) ? 0 : CODING_FLAG_ISO_NO_DIRECTION)
	     | (NILP (flags[13]) ? 0 : CODING_FLAG_ISO_INIT_AT_BOL)
	     | (NILP (flags[14]) ? 0 : CODING_FLAG_ISO_DESIGNATE_AT_BOL)
	     | (NILP (flags[15]) ? 0 : CODING_FLAG_ISO_SAFE)
	     | (NILP (flags[16]) ? 0 : CODING_FLAG_ISO_LATIN_EXTRA)
	     );

	/* Invoke graphic register 0 to plane 0.  */
	CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;
	/* Invoke graphic register 1 to plane 1 if we can use full 8-bit.  */
	CODING_SPEC_ISO_INVOCATION (coding, 1)
	  = (coding->flags & CODING_FLAG_ISO_SEVEN_BITS ? -1 : 1);
	/* Not single shifting at first.  */
	CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 0;
	/* Beginning of buffer should also be regarded as bol. */
	CODING_SPEC_ISO_BOL (coding) = 1;

	for (charset = 0; charset <= MAX_CHARSET; charset++)
	  CODING_SPEC_ISO_REVISION_NUMBER (coding, charset) = 255;
	val = Vcharset_revision_alist;
	while (CONSP (val))
	  {
	    charset = get_charset_id (Fcar_safe (XCAR (val)));
	    if (charset >= 0
		&& (temp = Fcdr_safe (XCAR (val)), INTEGERP (temp))
		&& (i = XINT (temp), (i >= 0 && (i + '@') < 128)))
	      CODING_SPEC_ISO_REVISION_NUMBER (coding, charset) = i;
	    val = XCDR (val);
	  }

	/* Checks FLAGS[REG] (REG = 0, 1, 2 3) and decide designations.
	   FLAGS[REG] can be one of below:
		integer CHARSET: CHARSET occupies register I,
		t: designate nothing to REG initially, but can be used
		  by any charsets,
		list of integer, nil, or t: designate the first
		  element (if integer) to REG initially, the remaining
		  elements (if integer) is designated to REG on request,
		  if an element is t, REG can be used by any charsets,
		nil: REG is never used.  */
	for (charset = 0; charset <= MAX_CHARSET; charset++)
	  CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
	    = CODING_SPEC_ISO_NO_REQUESTED_DESIGNATION;
	for (i = 0; i < 4; i++)
	  {
	    if ((INTEGERP (flags[i])
		 && (charset = XINT (flags[i]), CHARSET_VALID_P (charset)))
		|| (charset = get_charset_id (flags[i])) >= 0)
	      {
		CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = charset;
		CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) = i;
	      }
	    else if (EQ (flags[i], Qt))
	      {
		CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;
		reg_bits |= 1 << i;
		coding->flags |= CODING_FLAG_ISO_DESIGNATION;
	      }
	    else if (CONSP (flags[i]))
	      {
		Lisp_Object tail;
		tail = flags[i];

		coding->flags |= CODING_FLAG_ISO_DESIGNATION;
		if ((INTEGERP (XCAR (tail))
		     && (charset = XINT (XCAR (tail)),
			 CHARSET_VALID_P (charset)))
		    || (charset = get_charset_id (XCAR (tail))) >= 0)
		  {
		    CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = charset;
		    CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) =i;
		  }
		else
		  CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;
		tail = XCDR (tail);
		while (CONSP (tail))
		  {
		    if ((INTEGERP (XCAR (tail))
			 && (charset = XINT (XCAR (tail)),
			     CHARSET_VALID_P (charset)))
			|| (charset = get_charset_id (XCAR (tail))) >= 0)
		      CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
			= i;
		    else if (EQ (XCAR (tail), Qt))
		      reg_bits |= 1 << i;
		    tail = XCDR (tail);
		  }
	      }
	    else
	      CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;

	    CODING_SPEC_ISO_DESIGNATION (coding, i)
	      = CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i);
	  }

	if (reg_bits && ! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT))
	  {
	    /* REG 1 can be used only by locking shift in 7-bit env.  */
	    if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)
	      reg_bits &= ~2;
	    if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT))
	      /* Without any shifting, only REG 0 and 1 can be used.  */
	      reg_bits &= 3;
	  }

	if (reg_bits)
	  for (charset = 0; charset <= MAX_CHARSET; charset++)
	    {
	      if (CHARSET_DEFINED_P (charset)
		  && (CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
		      == CODING_SPEC_ISO_NO_REQUESTED_DESIGNATION))
		{
		  /* There exist some default graphic registers to be
		     used by CHARSET.  */

		  /* We had better avoid designating a charset of
		     CHARS96 to REG 0 as far as possible.  */
		  if (CHARSET_CHARS (charset) == 96)
		    CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
		      = (reg_bits & 2
			 ? 1 : (reg_bits & 4 ? 2 : (reg_bits & 8 ? 3 : 0)));
		  else
		    CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
		      = (reg_bits & 1
			 ? 0 : (reg_bits & 2 ? 1 : (reg_bits & 4 ? 2 : 3)));
		}
	    }
      }
      coding->common_flags |= CODING_REQUIRE_FLUSHING_MASK;
      coding->spec.iso2022.last_invalid_designation_register = -1;
      break;

    case 3:
      coding->type = coding_type_big5;
      coding->common_flags
	|= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
      coding->flags
	= (NILP (XVECTOR (coding_spec)->contents[4])
	   ? CODING_FLAG_BIG5_HKU
	   : CODING_FLAG_BIG5_ETEN);
      break;

    case 4:
      coding->type = coding_type_ccl;
      coding->common_flags
	|= CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK;
      {
	val = XVECTOR (coding_spec)->contents[4];
	if (! CONSP (val)
	    || setup_ccl_program (&(coding->spec.ccl.decoder),
				  XCAR (val)) < 0
	    || setup_ccl_program (&(coding->spec.ccl.encoder),
				  XCDR (val)) < 0)
	  goto label_invalid_coding_system;

	bzero (coding->spec.ccl.valid_codes, 256);
	val = Fplist_get (plist, Qvalid_codes);
	if (CONSP (val))
	  {
	    Lisp_Object this;

	    for (; CONSP (val); val = XCDR (val))
	      {
		this = XCAR (val);
		if (INTEGERP (this)
		    && XINT (this) >= 0 && XINT (this) < 256)
		  coding->spec.ccl.valid_codes[XINT (this)] = 1;
		else if (CONSP (this)
			 && INTEGERP (XCAR (this))
			 && INTEGERP (XCDR (this)))
		  {
		    int start = XINT (XCAR (this));
		    int end = XINT (XCDR (this));

		    if (start >= 0 && start <= end && end < 256)
		      while (start <= end)
			coding->spec.ccl.valid_codes[start++] = 1;
		  }
	      }
	  }
      }
      coding->common_flags |= CODING_REQUIRE_FLUSHING_MASK;
      coding->spec.ccl.cr_carryover = 0;
      coding->spec.ccl.eight_bit_carryover[0] = 0;
      break;

    case 5:
      coding->type = coding_type_raw_text;
      break;

    default:
      goto label_invalid_coding_system;
    }
  return 0;

 label_invalid_coding_system:
  coding->type = coding_type_no_conversion;
  coding->category_idx = CODING_CATEGORY_IDX_BINARY;
  coding->common_flags = 0;
  coding->eol_type = CODING_EOL_LF;
  coding->pre_write_conversion = coding->post_read_conversion = Qnil;
  return -1;
}

/* Free memory blocks allocated for storing composition information.  */

void
coding_free_composition_data (coding)
     struct coding_system *coding;
{
  struct composition_data *cmp_data = coding->cmp_data, *next;

  if (!cmp_data)
    return;
  /* Memory blocks are chained.  At first, rewind to the first, then,
     free blocks one by one.  */
  while (cmp_data->prev)
    cmp_data = cmp_data->prev;
  while (cmp_data)
    {
      next = cmp_data->next;
      xfree (cmp_data);
      cmp_data = next;
    }
  coding->cmp_data = NULL;
}

/* Set `char_offset' member of all memory blocks pointed by
   coding->cmp_data to POS.  */

void
coding_adjust_composition_offset (coding, pos)
     struct coding_system *coding;
     int pos;
{
  struct composition_data *cmp_data;

  for (cmp_data = coding->cmp_data; cmp_data; cmp_data = cmp_data->next)
    cmp_data->char_offset = pos;
}

/* Setup raw-text or one of its subsidiaries in the structure
   coding_system CODING according to the already setup value eol_type
   in CODING.  CODING should be setup for some coding system in
   advance.  */

void
setup_raw_text_coding_system (coding)
     struct coding_system *coding;
{
  if (coding->type != coding_type_raw_text)
    {
      coding->symbol = Qraw_text;
      coding->type = coding_type_raw_text;
      if (coding->eol_type != CODING_EOL_UNDECIDED)
	{
	  Lisp_Object subsidiaries;
	  subsidiaries = Fget (Qraw_text, Qeol_type);

	  if (VECTORP (subsidiaries)
	      && XVECTOR (subsidiaries)->size == 3)
	    coding->symbol
	      = XVECTOR (subsidiaries)->contents[coding->eol_type];
	}
      setup_coding_system (coding->symbol, coding);
    }
  return;
}

/* Emacs has a mechanism to automatically detect a coding system if it
   is one of Emacs' internal format, ISO2022, SJIS, and BIG5.  But,
   it's impossible to distinguish some coding systems accurately
   because they use the same range of codes.  So, at first, coding
   systems are categorized into 7, those are:

   o coding-category-emacs-mule

   	The category for a coding system which has the same code range
	as Emacs' internal format.  Assigned the coding-system (Lisp
	symbol) `emacs-mule' by default.

   o coding-category-sjis

	The category for a coding system which has the same code range
	as SJIS.  Assigned the coding-system (Lisp
	symbol) `japanese-shift-jis' by default.

   o coding-category-iso-7

   	The category for a coding system which has the same code range
	as ISO2022 of 7-bit environment.  This doesn't use any locking
	shift and single shift functions.  This can encode/decode all
	charsets.  Assigned the coding-system (Lisp symbol)
	`iso-2022-7bit' by default.

   o coding-category-iso-7-tight

	Same as coding-category-iso-7 except that this can
	encode/decode only the specified charsets.

   o coding-category-iso-8-1

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION1 charset.  This doesn't use any locking shift
	and single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-latin-1' by default.

   o coding-category-iso-8-2

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION2 charset.  This doesn't use any locking shift
	and single shift functions.  Assigned the coding-system (Lisp
	symbol) `japanese-iso-8bit' by default.

   o coding-category-iso-7-else

   	The category for a coding system which has the same code range
	as ISO2022 of 7-bit environment but uses locking shift or
	single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-2022-7bit-lock' by default.

   o coding-category-iso-8-else

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment but uses locking shift or
	single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-2022-8bit-ss2' by default.

   o coding-category-big5

   	The category for a coding system which has the same code range
	as BIG5.  Assigned the coding-system (Lisp symbol)
	`cn-big5' by default.

   o coding-category-utf-8

	The category for a coding system which has the same code range
	as UTF-8 (cf. RFC2279).  Assigned the coding-system (Lisp
	symbol) `utf-8' by default.

   o coding-category-utf-16-be

	The category for a coding system in which a text has an
	Unicode signature (cf. Unicode Standard) in the order of BIG
	endian at the head.  Assigned the coding-system (Lisp symbol)
	`utf-16-be' by default.

   o coding-category-utf-16-le

	The category for a coding system in which a text has an
	Unicode signature (cf. Unicode Standard) in the order of
	LITTLE endian at the head.  Assigned the coding-system (Lisp
	symbol) `utf-16-le' by default.

   o coding-category-ccl

	The category for a coding system of which encoder/decoder is
	written in CCL programs.  The default value is nil, i.e., no
	coding system is assigned.

   o coding-category-binary

   	The category for a coding system not categorized in any of the
	above.  Assigned the coding-system (Lisp symbol)
	`no-conversion' by default.

   Each of them is a Lisp symbol and the value is an actual
   `coding-system' (this is also a Lisp symbol) assigned by a user.
   What Emacs does actually is to detect a category of coding system.
   Then, it uses a `coding-system' assigned to it.  If Emacs can't
   decide a single possible category, it selects a category of the
   highest priority.  Priorities of categories are also specified by a
   user in a Lisp variable `coding-category-list'.

*/

static
int ascii_skip_code[256];

/* Detect how a text of length SRC_BYTES pointed by SOURCE is encoded.
   If it detects possible coding systems, return an integer in which
   appropriate flag bits are set.  Flag bits are defined by macros
   CODING_CATEGORY_MASK_XXX in `coding.h'.  If PRIORITIES is non-NULL,
   it should point the table `coding_priorities'.  In that case, only
   the flag bit for a coding system of the highest priority is set in
   the returned value.  If MULTIBYTEP is nonzero, 8-bit codes of the
   range 0x80..0x9F are in multibyte form.

   How many ASCII characters are at the head is returned as *SKIP.  */

static int
detect_coding_mask (source, src_bytes, priorities, skip, multibytep)
     unsigned char *source;
     int src_bytes, *priorities, *skip;
     int multibytep;
{
  register unsigned char c;
  unsigned char *src = source, *src_end = source + src_bytes;
  unsigned int mask, utf16_examined_p, iso2022_examined_p;
  int i;

  /* At first, skip all ASCII characters and control characters except
     for three ISO2022 specific control characters.  */
  ascii_skip_code[ISO_CODE_SO] = 0;
  ascii_skip_code[ISO_CODE_SI] = 0;
  ascii_skip_code[ISO_CODE_ESC] = 0;

 label_loop_detect_coding:
  while (src < src_end && ascii_skip_code[*src]) src++;
  *skip = src - source;

  if (src >= src_end)
    /* We found nothing other than ASCII.  There's nothing to do.  */
    return 0;

  c = *src;
  /* The text seems to be encoded in some multilingual coding system.
     Now, try to find in which coding system the text is encoded.  */
  if (c < 0x80)
    {
      /* i.e. (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO) */
      /* C is an ISO2022 specific control code of C0.  */
      mask = detect_coding_iso2022 (src, src_end, multibytep);
      if (mask == 0)
	{
	  /* No valid ISO2022 code follows C.  Try again.  */
	  src++;
	  if (c == ISO_CODE_ESC)
	    ascii_skip_code[ISO_CODE_ESC] = 1;
	  else
	    ascii_skip_code[ISO_CODE_SO] = ascii_skip_code[ISO_CODE_SI] = 1;
	  goto label_loop_detect_coding;
	}
      if (priorities)
	{
	  for (i = 0; i < CODING_CATEGORY_IDX_MAX; i++)
	    {
	      if (mask & priorities[i])
		return priorities[i];
	    }
	  return CODING_CATEGORY_MASK_RAW_TEXT;
	}
    }
  else
    {
      int try;

      if (multibytep && c == LEADING_CODE_8_BIT_CONTROL)
	c = src[1] - 0x20;

      if (c < 0xA0)
	{
	  /* C is the first byte of SJIS character code,
	     or a leading-code of Emacs' internal format (emacs-mule),
	     or the first byte of UTF-16.  */
	  try = (CODING_CATEGORY_MASK_SJIS
		  | CODING_CATEGORY_MASK_EMACS_MULE
		  | CODING_CATEGORY_MASK_UTF_16_BE
		  | CODING_CATEGORY_MASK_UTF_16_LE);

	  /* Or, if C is a special latin extra code,
	     or is an ISO2022 specific control code of C1 (SS2 or SS3),
	     or is an ISO2022 control-sequence-introducer (CSI),
	     we should also consider the possibility of ISO2022 codings.  */
	  if ((VECTORP (Vlatin_extra_code_table)
	       && !NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
	      || (c == ISO_CODE_SS2 || c == ISO_CODE_SS3)
	      || (c == ISO_CODE_CSI
		  && (src < src_end
		      && (*src == ']'
			  || ((*src == '0' || *src == '1' || *src == '2')
			      && src + 1 < src_end
			      && src[1] == ']')))))
	    try |= (CODING_CATEGORY_MASK_ISO_8_ELSE
		     | CODING_CATEGORY_MASK_ISO_8BIT);
	}
      else
	/* C is a character of ISO2022 in graphic plane right,
	   or a SJIS's 1-byte character code (i.e. JISX0201),
	   or the first byte of BIG5's 2-byte code,
	   or the first byte of UTF-8/16.  */
	try = (CODING_CATEGORY_MASK_ISO_8_ELSE
		| CODING_CATEGORY_MASK_ISO_8BIT
		| CODING_CATEGORY_MASK_SJIS
		| CODING_CATEGORY_MASK_BIG5
	        | CODING_CATEGORY_MASK_UTF_8
	        | CODING_CATEGORY_MASK_UTF_16_BE
	        | CODING_CATEGORY_MASK_UTF_16_LE);

      /* Or, we may have to consider the possibility of CCL.  */
      if (coding_system_table[CODING_CATEGORY_IDX_CCL]
	  && (coding_system_table[CODING_CATEGORY_IDX_CCL]
	      ->spec.ccl.valid_codes)[c])
	try |= CODING_CATEGORY_MASK_CCL;

      mask = 0;
      utf16_examined_p = iso2022_examined_p = 0;
      if (priorities)
	{
	  for (i = 0; i < CODING_CATEGORY_IDX_MAX; i++)
	    {
	      if (!iso2022_examined_p
		  && (priorities[i] & try & CODING_CATEGORY_MASK_ISO))
		{
		  mask |= detect_coding_iso2022 (src, src_end, multibytep);
		  iso2022_examined_p = 1;
		}
	      else if (priorities[i] & try & CODING_CATEGORY_MASK_SJIS)
		mask |= detect_coding_sjis (src, src_end, multibytep);
	      else if (priorities[i] & try & CODING_CATEGORY_MASK_UTF_8)
		mask |= detect_coding_utf_8 (src, src_end, multibytep);
	      else if (!utf16_examined_p
		       && (priorities[i] & try &
			   CODING_CATEGORY_MASK_UTF_16_BE_LE))
		{
		  mask |= detect_coding_utf_16 (src, src_end, multibytep);
		  utf16_examined_p = 1;
		}
	      else if (priorities[i] & try & CODING_CATEGORY_MASK_BIG5)
		mask |= detect_coding_big5 (src, src_end, multibytep);
	      else if (priorities[i] & try & CODING_CATEGORY_MASK_EMACS_MULE)
		mask |= detect_coding_emacs_mule (src, src_end, multibytep);
	      else if (priorities[i] & try & CODING_CATEGORY_MASK_CCL)
		mask |= detect_coding_ccl (src, src_end, multibytep);
	      else if (priorities[i] & CODING_CATEGORY_MASK_RAW_TEXT)
		mask |= CODING_CATEGORY_MASK_RAW_TEXT;
	      else if (priorities[i] & CODING_CATEGORY_MASK_BINARY)
		mask |= CODING_CATEGORY_MASK_BINARY;
	      if (mask & priorities[i])
		return priorities[i];
	    }
	  return CODING_CATEGORY_MASK_RAW_TEXT;
	}
      if (try & CODING_CATEGORY_MASK_ISO)
	mask |= detect_coding_iso2022 (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_SJIS)
	mask |= detect_coding_sjis (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_BIG5)
	mask |= detect_coding_big5 (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_UTF_8)
	mask |= detect_coding_utf_8 (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_UTF_16_BE_LE)
	mask |= detect_coding_utf_16 (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_EMACS_MULE)
	mask |= detect_coding_emacs_mule (src, src_end, multibytep);
      if (try & CODING_CATEGORY_MASK_CCL)
	mask |= detect_coding_ccl (src, src_end, multibytep);
    }
  return (mask | CODING_CATEGORY_MASK_RAW_TEXT | CODING_CATEGORY_MASK_BINARY);
}

/* Detect how a text of length SRC_BYTES pointed by SRC is encoded.
   The information of the detected coding system is set in CODING.  */

void
detect_coding (coding, src, src_bytes)
     struct coding_system *coding;
     unsigned char *src;
     int src_bytes;
{
  unsigned int idx;
  int skip, mask;
  Lisp_Object val;

  val = Vcoding_category_list;
  mask = detect_coding_mask (src, src_bytes, coding_priorities, &skip,
			     coding->src_multibyte);
  coding->heading_ascii = skip;

  if (!mask) return;

  /* We found a single coding system of the highest priority in MASK.  */
  idx = 0;
  while (mask && ! (mask & 1)) mask >>= 1, idx++;
  if (! mask)
    idx = CODING_CATEGORY_IDX_RAW_TEXT;

  val = SYMBOL_VALUE (XVECTOR (Vcoding_category_table)->contents[idx]);

  if (coding->eol_type != CODING_EOL_UNDECIDED)
    {
      Lisp_Object tmp;

      tmp = Fget (val, Qeol_type);
      if (VECTORP (tmp))
	val = XVECTOR (tmp)->contents[coding->eol_type];
    }

  /* Setup this new coding system while preserving some slots.  */
  {
    int src_multibyte = coding->src_multibyte;
    int dst_multibyte = coding->dst_multibyte;

    setup_coding_system (val, coding);
    coding->src_multibyte = src_multibyte;
    coding->dst_multibyte = dst_multibyte;
    coding->heading_ascii = skip;
  }
}

/* Detect how end-of-line of a text of length SRC_BYTES pointed by
   SOURCE is encoded.  Return one of CODING_EOL_LF, CODING_EOL_CRLF,
   CODING_EOL_CR, and CODING_EOL_UNDECIDED.

   How many non-eol characters are at the head is returned as *SKIP.  */

#define MAX_EOL_CHECK_COUNT 3

static int
detect_eol_type (source, src_bytes, skip)
     unsigned char *source;
     int src_bytes, *skip;
{
  unsigned char *src = source, *src_end = src + src_bytes;
  unsigned char c;
  int total = 0;		/* How many end-of-lines are found so far.  */
  int eol_type = CODING_EOL_UNDECIDED;
  int this_eol_type;

  *skip = 0;

  while (src < src_end && total < MAX_EOL_CHECK_COUNT)
    {
      c = *src++;
      if (c == '\n' || c == '\r')
	{
	  if (*skip == 0)
	    *skip = src - 1 - source;
	  total++;
	  if (c == '\n')
	    this_eol_type = CODING_EOL_LF;
	  else if (src >= src_end || *src != '\n')
	    this_eol_type = CODING_EOL_CR;
	  else
	    this_eol_type = CODING_EOL_CRLF, src++;

	  if (eol_type == CODING_EOL_UNDECIDED)
	    /* This is the first end-of-line.  */
	    eol_type = this_eol_type;
	  else if (eol_type != this_eol_type)
	    {
	      /* The found type is different from what found before.  */
	      eol_type = CODING_EOL_INCONSISTENT;
	      break;
	    }
	}
    }

  if (*skip == 0)
    *skip = src_end - source;
  return eol_type;
}

/* Like detect_eol_type, but detect EOL type in 2-octet
   big-endian/little-endian format for coding systems utf-16-be and
   utf-16-le.  */

static int
detect_eol_type_in_2_octet_form (source, src_bytes, skip, big_endian_p)
     unsigned char *source;
     int src_bytes, *skip, big_endian_p;
{
  unsigned char *src = source, *src_end = src + src_bytes;
  unsigned int c1, c2;
  int total = 0;		/* How many end-of-lines are found so far.  */
  int eol_type = CODING_EOL_UNDECIDED;
  int this_eol_type;
  int msb, lsb;

  if (big_endian_p)
    msb = 0, lsb = 1;
  else
    msb = 1, lsb = 0;

  *skip = 0;

  while ((src + 1) < src_end && total < MAX_EOL_CHECK_COUNT)
    {
      c1 = (src[msb] << 8) | (src[lsb]);
      src += 2;

      if (c1 == '\n' || c1 == '\r')
	{
	  if (*skip == 0)
	    *skip = src - 2 - source;
	  total++;
	  if (c1 == '\n')
	    {
	      this_eol_type = CODING_EOL_LF;
	    }
	  else
	    {
	      if ((src + 1) >= src_end)
		{
		  this_eol_type = CODING_EOL_CR;
		}
	      else
		{
		  c2 = (src[msb] << 8) | (src[lsb]);
		  if (c2 == '\n')
		    this_eol_type = CODING_EOL_CRLF, src += 2;
		  else
		    this_eol_type = CODING_EOL_CR;
		}
	    }

	  if (eol_type == CODING_EOL_UNDECIDED)
	    /* This is the first end-of-line.  */
	    eol_type = this_eol_type;
	  else if (eol_type != this_eol_type)
	    {
	      /* The found type is different from what found before.  */
	      eol_type = CODING_EOL_INCONSISTENT;
	      break;
	    }
	}
    }

  if (*skip == 0)
    *skip = src_end - source;
  return eol_type;
}

/* Detect how end-of-line of a text of length SRC_BYTES pointed by SRC
   is encoded.  If it detects an appropriate format of end-of-line, it
   sets the information in *CODING.  */

void
detect_eol (coding, src, src_bytes)
     struct coding_system *coding;
     unsigned char *src;
     int src_bytes;
{
  Lisp_Object val;
  int skip;
  int eol_type;

  switch (coding->category_idx)
    {
    case CODING_CATEGORY_IDX_UTF_16_BE:
      eol_type = detect_eol_type_in_2_octet_form (src, src_bytes, &skip, 1);
      break;
    case CODING_CATEGORY_IDX_UTF_16_LE:
      eol_type = detect_eol_type_in_2_octet_form (src, src_bytes, &skip, 0);
      break;
    default:
      eol_type = detect_eol_type (src, src_bytes, &skip);
      break;
    }

  if (coding->heading_ascii > skip)
    coding->heading_ascii = skip;
  else
    skip = coding->heading_ascii;

  if (eol_type == CODING_EOL_UNDECIDED)
    return;
  if (eol_type == CODING_EOL_INCONSISTENT)
    {
#if 0
      /* This code is suppressed until we find a better way to
	 distinguish raw text file and binary file.  */

      /* If we have already detected that the coding is raw-text, the
	 coding should actually be no-conversion.  */
      if (coding->type == coding_type_raw_text)
	{
	  setup_coding_system (Qno_conversion, coding);
	  return;
	}
      /* Else, let's decode only text code anyway.  */
#endif /* 0 */
      eol_type = CODING_EOL_LF;
    }

  val = Fget (coding->symbol, Qeol_type);
  if (VECTORP (val) && XVECTOR (val)->size == 3)
    {
      int src_multibyte = coding->src_multibyte;
      int dst_multibyte = coding->dst_multibyte;

      setup_coding_system (XVECTOR (val)->contents[eol_type], coding);
      coding->src_multibyte = src_multibyte;
      coding->dst_multibyte = dst_multibyte;
      coding->heading_ascii = skip;
    }
}

#define CONVERSION_BUFFER_EXTRA_ROOM 256

#define DECODING_BUFFER_MAG(coding)			\
  (coding->type == coding_type_iso2022			\
   ? 3							\
   : (coding->type == coding_type_ccl			\
      ? coding->spec.ccl.decoder.buf_magnification	\
      : 2))

/* Return maximum size (bytes) of a buffer enough for decoding
   SRC_BYTES of text encoded in CODING.  */

int
decoding_buffer_size (coding, src_bytes)
     struct coding_system *coding;
     int src_bytes;
{
  return (src_bytes * DECODING_BUFFER_MAG (coding)
	  + CONVERSION_BUFFER_EXTRA_ROOM);
}

/* Return maximum size (bytes) of a buffer enough for encoding
   SRC_BYTES of text to CODING.  */

int
encoding_buffer_size (coding, src_bytes)
     struct coding_system *coding;
     int src_bytes;
{
  int magnification;

  if (coding->type == coding_type_ccl)
    magnification = coding->spec.ccl.encoder.buf_magnification;
  else if (CODING_REQUIRE_ENCODING (coding))
    magnification = 3;
  else
    magnification = 1;

  return (src_bytes * magnification + CONVERSION_BUFFER_EXTRA_ROOM);
}

/* Working buffer for code conversion.  */
struct conversion_buffer
{
  int size;			/* size of data.  */
  int on_stack;			/* 1 if allocated by alloca.  */
  unsigned char *data;
};

/* Don't use alloca for allocating memory space larger than this, lest
   we overflow their stack.  */
#define MAX_ALLOCA 16*1024

/* Allocate LEN bytes of memory for BUF (struct conversion_buffer).  */
#define allocate_conversion_buffer(buf, len)		\
  do {							\
    if (len < MAX_ALLOCA)				\
      {							\
	buf.data = (unsigned char *) alloca (len);	\
	buf.on_stack = 1;				\
      }							\
    else						\
      {							\
	buf.data = (unsigned char *) xmalloc (len);	\
	buf.on_stack = 0;				\
      }							\
    buf.size = len;					\
  } while (0)

/* Double the allocated memory for *BUF.  */
static void
extend_conversion_buffer (buf)
     struct conversion_buffer *buf;
{
  if (buf->on_stack)
    {
      unsigned char *save = buf->data;
      buf->data = (unsigned char *) xmalloc (buf->size * 2);
      bcopy (save, buf->data, buf->size);
      buf->on_stack = 0;
    }
  else
    {
      buf->data = (unsigned char *) xrealloc (buf->data, buf->size * 2);
    }
  buf->size *= 2;
}

/* Free the allocated memory for BUF if it is not on stack.  */
static void
free_conversion_buffer (buf)
     struct conversion_buffer *buf;
{
  if (!buf->on_stack)
    xfree (buf->data);
}

int
ccl_coding_driver (coding, source, destination, src_bytes, dst_bytes, encodep)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes, encodep;
{
  struct ccl_program *ccl
    = encodep ? &coding->spec.ccl.encoder : &coding->spec.ccl.decoder;
  unsigned char *dst = destination;

  ccl->suppress_error = coding->suppress_error;
  ccl->last_block = coding->mode & CODING_MODE_LAST_BLOCK;
  if (encodep)
    {
      /* On encoding, EOL format is converted within ccl_driver.  For
	 that, setup proper information in the structure CCL.  */
      ccl->eol_type = coding->eol_type;
      if (ccl->eol_type ==CODING_EOL_UNDECIDED)
	ccl->eol_type = CODING_EOL_LF;
      ccl->cr_consumed = coding->spec.ccl.cr_carryover;
    }
  ccl->multibyte = coding->src_multibyte;
  if (coding->spec.ccl.eight_bit_carryover[0] != 0)
    {
      /* Move carryover bytes to DESTINATION.  */
      unsigned char *p = coding->spec.ccl.eight_bit_carryover;
      while (*p)
	*dst++ = *p++;
      coding->spec.ccl.eight_bit_carryover[0] = 0;
      if (dst_bytes)
	dst_bytes -= dst - destination;
    }

  coding->produced = (ccl_driver (ccl, source, dst, src_bytes, dst_bytes,
				  &(coding->consumed))
		      + dst - destination);

  if (encodep)
    {
      coding->produced_char = coding->produced;
      coding->spec.ccl.cr_carryover = ccl->cr_consumed;
    }
  else if (!ccl->eight_bit_control)
    {
      /* The produced bytes forms a valid multibyte sequence. */
      coding->produced_char
	= multibyte_chars_in_text (destination, coding->produced);
      coding->spec.ccl.eight_bit_carryover[0] = 0;
    }
  else
    {
      /* On decoding, the destination should always multibyte.  But,
	 CCL program might have been generated an invalid multibyte
	 sequence.  Here we make such a sequence valid as
	 multibyte.  */
      int bytes
	= dst_bytes ? dst_bytes : source + coding->consumed - destination;

      if ((coding->consumed < src_bytes
	   || !ccl->last_block)
	  && coding->produced >= 1
	  && destination[coding->produced - 1] >= 0x80)
	{
	  /* We should not convert the tailing 8-bit codes to
	     multibyte form even if they doesn't form a valid
	     multibyte sequence.  They may form a valid sequence in
	     the next call.  */
	  int carryover = 0;

	  if (destination[coding->produced - 1] < 0xA0)
	    carryover = 1;
	  else if (coding->produced >= 2)
	    {
	      if (destination[coding->produced - 2] >= 0x80)
		{
		  if (destination[coding->produced - 2] < 0xA0)
		    carryover = 2;
		  else if (coding->produced >= 3
			   && destination[coding->produced - 3] >= 0x80
			   && destination[coding->produced - 3] < 0xA0)
		    carryover = 3;
		}
	    }
	  if (carryover > 0)
	    {
	      BCOPY_SHORT (destination + coding->produced - carryover,
			   coding->spec.ccl.eight_bit_carryover,
			   carryover);
	      coding->spec.ccl.eight_bit_carryover[carryover] = 0;
	      coding->produced -= carryover;
	    }
	}
      coding->produced = str_as_multibyte (destination, bytes,
					   coding->produced,
					   &(coding->produced_char));
    }

  switch (ccl->status)
    {
    case CCL_STAT_SUSPEND_BY_SRC:
      coding->result = CODING_FINISH_INSUFFICIENT_SRC;
      break;
    case CCL_STAT_SUSPEND_BY_DST:
      coding->result = CODING_FINISH_INSUFFICIENT_DST;
      break;
    case CCL_STAT_QUIT:
    case CCL_STAT_INVALID_CMD:
      coding->result = CODING_FINISH_INTERRUPT;
      break;
    default:
      coding->result = CODING_FINISH_NORMAL;
      break;
    }
  return coding->result;
}

/* Decode EOL format of the text at PTR of BYTES length destructively
   according to CODING->eol_type.  This is called after the CCL
   program produced a decoded text at PTR.  If we do CRLF->LF
   conversion, update CODING->produced and CODING->produced_char.  */

static void
decode_eol_post_ccl (coding, ptr, bytes)
     struct coding_system *coding;
     unsigned char *ptr;
     int bytes;
{
  Lisp_Object val, saved_coding_symbol;
  unsigned char *pend = ptr + bytes;
  int dummy;

  /* Remember the current coding system symbol.  We set it back when
     an inconsistent EOL is found so that `last-coding-system-used' is
     set to the coding system that doesn't specify EOL conversion.  */
  saved_coding_symbol = coding->symbol;

  coding->spec.ccl.cr_carryover = 0;
  if (coding->eol_type == CODING_EOL_UNDECIDED)
    {
      /* Here, to avoid the call of setup_coding_system, we directly
	 call detect_eol_type.  */
      coding->eol_type = detect_eol_type (ptr, bytes, &dummy);
      if (coding->eol_type == CODING_EOL_INCONSISTENT)
	coding->eol_type = CODING_EOL_LF;
      if (coding->eol_type != CODING_EOL_UNDECIDED)
	{
	  val = Fget (coding->symbol, Qeol_type);
	  if (VECTORP (val) && XVECTOR (val)->size == 3)
	    coding->symbol = XVECTOR (val)->contents[coding->eol_type];
	}
      coding->mode |= CODING_MODE_INHIBIT_INCONSISTENT_EOL;
    }

  if (coding->eol_type == CODING_EOL_LF
      || coding->eol_type == CODING_EOL_UNDECIDED)
    {
      /* We have nothing to do.  */
      ptr = pend;
    }
  else if (coding->eol_type == CODING_EOL_CRLF)
    {
      unsigned char *pstart = ptr, *p = ptr;

      if (! (coding->mode & CODING_MODE_LAST_BLOCK)
	  && *(pend - 1) == '\r')
	{
	  /* If the last character is CR, we can't handle it here
	     because LF will be in the not-yet-decoded source text.
	     Record that the CR is not yet processed.  */
	  coding->spec.ccl.cr_carryover = 1;
	  coding->produced--;
	  coding->produced_char--;
	  pend--;
	}
      while (ptr < pend)
	{
	  if (*ptr == '\r')
	    {
	      if (ptr + 1 < pend && *(ptr + 1) == '\n')
		{
		  *p++ = '\n';
		  ptr += 2;
		}
	      else
		{
		  if (coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
		    goto undo_eol_conversion;
		  *p++ = *ptr++;
		}
	    }
	  else if (*ptr == '\n'
		   && coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
	    goto undo_eol_conversion;
	  else
	    *p++ = *ptr++;
	  continue;

	undo_eol_conversion:
	  /* We have faced with inconsistent EOL format at PTR.
	     Convert all LFs before PTR back to CRLFs.  */
	  for (p--, ptr--; p >= pstart; p--)
	    {
	      if (*p == '\n')
		*ptr-- = '\n', *ptr-- = '\r';
	      else
		*ptr-- = *p;
	    }
	  /*  If carryover is recorded, cancel it because we don't
	      convert CRLF anymore.  */
	  if (coding->spec.ccl.cr_carryover)
	    {
	      coding->spec.ccl.cr_carryover = 0;
	      coding->produced++;
	      coding->produced_char++;
	      pend++;
	    }
	  p = ptr = pend;
	  coding->eol_type = CODING_EOL_LF;
	  coding->symbol = saved_coding_symbol;
	}
      if (p < pend)
	{
	  /* As each two-byte sequence CRLF was converted to LF, (PEND
	     - P) is the number of deleted characters.  */
	  coding->produced -= pend - p;
	  coding->produced_char -= pend - p;
	}
    }
  else			/* i.e. coding->eol_type == CODING_EOL_CR */
    {
      unsigned char *p = ptr;

      for (; ptr < pend; ptr++)
	{
	  if (*ptr == '\r')
	    *ptr = '\n';
	  else if (*ptr == '\n'
		   && coding->mode & CODING_MODE_INHIBIT_INCONSISTENT_EOL)
	    {
	      for (; p < ptr; p++)
		{
		  if (*p == '\n')
		    *p = '\r';
		}
	      ptr = pend;
	      coding->eol_type = CODING_EOL_LF;
	      coding->symbol = saved_coding_symbol;
	    }
	}
    }
}

/* See "GENERAL NOTES about `decode_coding_XXX ()' functions".  Before
   decoding, it may detect coding system and format of end-of-line if
   those are not yet decided.  The source should be unibyte, the
   result is multibyte if CODING->dst_multibyte is nonzero, else
   unibyte.  */

int
decode_coding (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  int extra = 0;

  if (coding->type == coding_type_undecided)
    detect_coding (coding, source, src_bytes);

  if (coding->eol_type == CODING_EOL_UNDECIDED
      && coding->type != coding_type_ccl)
    {
      detect_eol (coding, source, src_bytes);
      /* We had better recover the original eol format if we
	 encounter an inconsistent eol format while decoding.  */
      coding->mode |= CODING_MODE_INHIBIT_INCONSISTENT_EOL;
    }

  coding->produced = coding->produced_char = 0;
  coding->consumed = coding->consumed_char = 0;
  coding->errors = 0;
  coding->result = CODING_FINISH_NORMAL;

  switch (coding->type)
    {
    case coding_type_sjis:
      decode_coding_sjis_big5 (coding, source, destination,
			       src_bytes, dst_bytes, 1);
      break;

    case coding_type_iso2022:
      decode_coding_iso2022 (coding, source, destination,
			     src_bytes, dst_bytes);
      break;

    case coding_type_big5:
      decode_coding_sjis_big5 (coding, source, destination,
			       src_bytes, dst_bytes, 0);
      break;

    case coding_type_emacs_mule:
      decode_coding_emacs_mule (coding, source, destination,
				src_bytes, dst_bytes);
      break;

    case coding_type_ccl:
      if (coding->spec.ccl.cr_carryover)
	{
	  /* Put the CR which was not processed by the previous call
	     of decode_eol_post_ccl in DESTINATION.  It will be
	     decoded together with the following LF by the call to
	     decode_eol_post_ccl below.  */
	  *destination = '\r';
	  coding->produced++;
	  coding->produced_char++;
	  dst_bytes--;
	  extra = coding->spec.ccl.cr_carryover;
	}
      ccl_coding_driver (coding, source, destination + extra,
			 src_bytes, dst_bytes, 0);
      if (coding->eol_type != CODING_EOL_LF)
	{
	  coding->produced += extra;
	  coding->produced_char += extra;
	  decode_eol_post_ccl (coding, destination, coding->produced);
	}
      break;

    default:
      decode_eol (coding, source, destination, src_bytes, dst_bytes);
    }

  if (coding->result == CODING_FINISH_INSUFFICIENT_SRC
      && coding->mode & CODING_MODE_LAST_BLOCK
      && coding->consumed == src_bytes)
    coding->result = CODING_FINISH_NORMAL;

  if (coding->mode & CODING_MODE_LAST_BLOCK
      && coding->result == CODING_FINISH_INSUFFICIENT_SRC)
    {
      unsigned char *src = source + coding->consumed;
      unsigned char *dst = destination + coding->produced;

      src_bytes -= coding->consumed;
      coding->errors++;
      if (COMPOSING_P (coding))
	DECODE_COMPOSITION_END ('1');
      while (src_bytes--)
	{
	  int c = *src++;
	  dst += CHAR_STRING (c, dst);
	  coding->produced_char++;
	}
      coding->consumed = coding->consumed_char = src - source;
      coding->produced = dst - destination;
      coding->result = CODING_FINISH_NORMAL;
    }

  if (!coding->dst_multibyte)
    {
      coding->produced = str_as_unibyte (destination, coding->produced);
      coding->produced_char = coding->produced;
    }

  return coding->result;
}

/* See "GENERAL NOTES about `encode_coding_XXX ()' functions".  The
   multibyteness of the source is CODING->src_multibyte, the
   multibyteness of the result is always unibyte.  */

int
encode_coding (coding, source, destination, src_bytes, dst_bytes)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
{
  coding->produced = coding->produced_char = 0;
  coding->consumed = coding->consumed_char = 0;
  coding->errors = 0;
  coding->result = CODING_FINISH_NORMAL;

  switch (coding->type)
    {
    case coding_type_sjis:
      encode_coding_sjis_big5 (coding, source, destination,
			       src_bytes, dst_bytes, 1);
      break;

    case coding_type_iso2022:
      encode_coding_iso2022 (coding, source, destination,
			     src_bytes, dst_bytes);
      break;

    case coding_type_big5:
      encode_coding_sjis_big5 (coding, source, destination,
			       src_bytes, dst_bytes, 0);
      break;

    case coding_type_emacs_mule:
      encode_coding_emacs_mule (coding, source, destination,
				src_bytes, dst_bytes);
      break;

    case coding_type_ccl:
      ccl_coding_driver (coding, source, destination,
			 src_bytes, dst_bytes, 1);
      break;

    default:
      encode_eol (coding, source, destination, src_bytes, dst_bytes);
    }

  if (coding->mode & CODING_MODE_LAST_BLOCK
      && coding->result == CODING_FINISH_INSUFFICIENT_SRC)
    {
      unsigned char *src = source + coding->consumed;
      unsigned char *dst = destination + coding->produced;

      if (coding->type == coding_type_iso2022)
	ENCODE_RESET_PLANE_AND_REGISTER;
      if (COMPOSING_P (coding))
	*dst++ = ISO_CODE_ESC, *dst++ = '1';
      if (coding->consumed < src_bytes)
	{
	  int len = src_bytes - coding->consumed;

	  BCOPY_SHORT (src, dst, len);
	  if (coding->src_multibyte)
	    len = str_as_unibyte (dst, len);
	  dst += len;
	  coding->consumed = src_bytes;
	}
      coding->produced = coding->produced_char = dst - destination;
      coding->result = CODING_FINISH_NORMAL;
    }

  if (coding->result == CODING_FINISH_INSUFFICIENT_SRC
      && coding->consumed == src_bytes)
    coding->result = CODING_FINISH_NORMAL;

  return coding->result;
}

/* Scan text in the region between *BEG and *END (byte positions),
   skip characters which we don't have to decode by coding system
   CODING at the head and tail, then set *BEG and *END to the region
   of the text we actually have to convert.  The caller should move
   the gap out of the region in advance if the region is from a
   buffer.

   If STR is not NULL, *BEG and *END are indices into STR.  */

static void
shrink_decoding_region (beg, end, coding, str)
     int *beg, *end;
     struct coding_system *coding;
     unsigned char *str;
{
  unsigned char *begp_orig, *begp, *endp_orig, *endp, c;
  int eol_conversion;
  Lisp_Object translation_table;

  if (coding->type == coding_type_ccl
      || coding->type == coding_type_undecided
      || coding->eol_type != CODING_EOL_LF
      || !NILP (coding->post_read_conversion)
      || coding->composing != COMPOSITION_DISABLED)
    {
      /* We can't skip any data.  */
      return;
    }
  if (coding->type == coding_type_no_conversion
      || coding->type == coding_type_raw_text
      || coding->type == coding_type_emacs_mule)
    {
      /* We need no conversion, but don't have to skip any data here.
         Decoding routine handles them effectively anyway.  */
      return;
    }

  translation_table = coding->translation_table_for_decode;
  if (NILP (translation_table) && !NILP (Venable_character_translation))
    translation_table = Vstandard_translation_table_for_decode;
  if (CHAR_TABLE_P (translation_table))
    {
      int i;
      for (i = 0; i < 128; i++)
	if (!NILP (CHAR_TABLE_REF (translation_table, i)))
	  break;
      if (i < 128)
	/* Some ASCII character should be translated.  We give up
	   shrinking.  */
	return;
    }

  if (coding->heading_ascii >= 0)
    /* Detection routine has already found how much we can skip at the
       head.  */
    *beg += coding->heading_ascii;

  if (str)
    {
      begp_orig = begp = str + *beg;
      endp_orig = endp = str + *end;
    }
  else
    {
      begp_orig = begp = BYTE_POS_ADDR (*beg);
      endp_orig = endp = begp + *end - *beg;
    }

  eol_conversion = (coding->eol_type == CODING_EOL_CR
		    || coding->eol_type == CODING_EOL_CRLF);

  switch (coding->type)
    {
    case coding_type_sjis:
    case coding_type_big5:
      /* We can skip all ASCII characters at the head.  */
      if (coding->heading_ascii < 0)
	{
	  if (eol_conversion)
	    while (begp < endp && *begp < 0x80 && *begp != '\r') begp++;
	  else
	    while (begp < endp && *begp < 0x80) begp++;
	}
      /* We can skip all ASCII characters at the tail except for the
	 second byte of SJIS or BIG5 code.  */
      if (eol_conversion)
	while (begp < endp && endp[-1] < 0x80 && endp[-1] != '\r') endp--;
      else
	while (begp < endp && endp[-1] < 0x80) endp--;
      /* Do not consider LF as ascii if preceded by CR, since that
	 confuses eol decoding. */
      if (begp < endp && endp < endp_orig && endp[-1] == '\r' && endp[0] == '\n')
	endp++;
      if (begp < endp && endp < endp_orig && endp[-1] >= 0x80)
	endp++;
      break;

    case coding_type_iso2022:
      if (CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, 0) != CHARSET_ASCII)
	/* We can't skip any data.  */
	break;
      if (coding->heading_ascii < 0)
	{
	  /* We can skip all ASCII characters at the head except for a
	     few control codes.  */
	  while (begp < endp && (c = *begp) < 0x80
		 && c != ISO_CODE_CR && c != ISO_CODE_SO
		 && c != ISO_CODE_SI && c != ISO_CODE_ESC
		 && (!eol_conversion || c != ISO_CODE_LF))
	    begp++;
	}
      switch (coding->category_idx)
	{
	case CODING_CATEGORY_IDX_ISO_8_1:
	case CODING_CATEGORY_IDX_ISO_8_2:
	  /* We can skip all ASCII characters at the tail.  */
	  if (eol_conversion)
	    while (begp < endp && (c = endp[-1]) < 0x80 && c != '\r') endp--;
	  else
	    while (begp < endp && endp[-1] < 0x80) endp--;
	  /* Do not consider LF as ascii if preceded by CR, since that
             confuses eol decoding. */
	  if (begp < endp && endp < endp_orig && endp[-1] == '\r' && endp[0] == '\n')
	    endp++;
	  break;

	case CODING_CATEGORY_IDX_ISO_7:
	case CODING_CATEGORY_IDX_ISO_7_TIGHT:
	  {
	    /* We can skip all characters at the tail except for 8-bit
	       codes and ESC and the following 2-byte at the tail.  */
	    unsigned char *eight_bit = NULL;

	    if (eol_conversion)
	      while (begp < endp
		     && (c = endp[-1]) != ISO_CODE_ESC && c != '\r')
		{
		  if (!eight_bit && c & 0x80) eight_bit = endp;
		  endp--;
		}
	    else
	      while (begp < endp
		     && (c = endp[-1]) != ISO_CODE_ESC)
		{
		  if (!eight_bit && c & 0x80) eight_bit = endp;
		  endp--;
		}
	    /* Do not consider LF as ascii if preceded by CR, since that
	       confuses eol decoding. */
	    if (begp < endp && endp < endp_orig
		&& endp[-1] == '\r' && endp[0] == '\n')
	      endp++;
	    if (begp < endp && endp[-1] == ISO_CODE_ESC)
	      {
		if (endp + 1 < endp_orig && end[0] == '(' && end[1] == 'B')
		  /* This is an ASCII designation sequence.  We can
		     surely skip the tail.  But, if we have
		     encountered an 8-bit code, skip only the codes
		     after that.  */
		  endp = eight_bit ? eight_bit : endp + 2;
		else
		  /* Hmmm, we can't skip the tail.  */
		  endp = endp_orig;
	      }
	    else if (eight_bit)
	      endp = eight_bit;
	  }
	}
      break;

    default:
      abort ();
    }
  *beg += begp - begp_orig;
  *end += endp - endp_orig;
  return;
}

/* Like shrink_decoding_region but for encoding.  */

static void
shrink_encoding_region (beg, end, coding, str)
     int *beg, *end;
     struct coding_system *coding;
     unsigned char *str;
{
  unsigned char *begp_orig, *begp, *endp_orig, *endp;
  int eol_conversion;
  Lisp_Object translation_table;

  if (coding->type == coding_type_ccl
      || coding->eol_type == CODING_EOL_CRLF
      || coding->eol_type == CODING_EOL_CR
      || (coding->cmp_data && coding->cmp_data->used > 0))
    {
      /* We can't skip any data.  */
      return;
    }
  if (coding->type == coding_type_no_conversion
      || coding->type == coding_type_raw_text
      || coding->type == coding_type_emacs_mule
      || coding->type == coding_type_undecided)
    {
      /* We need no conversion, but don't have to skip any data here.
         Encoding routine handles them effectively anyway.  */
      return;
    }

  translation_table = coding->translation_table_for_encode;
  if (NILP (translation_table) && !NILP (Venable_character_translation))
    translation_table = Vstandard_translation_table_for_encode;
  if (CHAR_TABLE_P (translation_table))
    {
      int i;
      for (i = 0; i < 128; i++)
	if (!NILP (CHAR_TABLE_REF (translation_table, i)))
	  break;
      if (i < 128)
	/* Some ASCII character should be translated.  We give up
	   shrinking.  */
	return;
    }

  if (str)
    {
      begp_orig = begp = str + *beg;
      endp_orig = endp = str + *end;
    }
  else
    {
      begp_orig = begp = BYTE_POS_ADDR (*beg);
      endp_orig = endp = begp + *end - *beg;
    }

  eol_conversion = (coding->eol_type == CODING_EOL_CR
		    || coding->eol_type == CODING_EOL_CRLF);

  /* Here, we don't have to check coding->pre_write_conversion because
     the caller is expected to have handled it already.  */
  switch (coding->type)
    {
    case coding_type_iso2022:
      if (CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, 0) != CHARSET_ASCII)
	/* We can't skip any data.  */
	break;
      if (coding->flags & CODING_FLAG_ISO_DESIGNATE_AT_BOL)
	{
	  unsigned char *bol = begp;
	  while (begp < endp && *begp < 0x80)
	    {
	      begp++;
	      if (begp[-1] == '\n')
		bol = begp;
	    }
	  begp = bol;
	  goto label_skip_tail;
	}
      /* fall down ... */

    case coding_type_sjis:
    case coding_type_big5:
      /* We can skip all ASCII characters at the head and tail.  */
      if (eol_conversion)
	while (begp < endp && *begp < 0x80 && *begp != '\n') begp++;
      else
	while (begp < endp && *begp < 0x80) begp++;
    label_skip_tail:
      if (eol_conversion)
	while (begp < endp && endp[-1] < 0x80 && endp[-1] != '\n') endp--;
      else
	while (begp < endp && *(endp - 1) < 0x80) endp--;
      break;

    default:
      abort ();
    }

  *beg += begp - begp_orig;
  *end += endp - endp_orig;
  return;
}

/* As shrinking conversion region requires some overhead, we don't try
   shrinking if the length of conversion region is less than this
   value.  */
static int shrink_conversion_region_threshhold = 1024;

#define SHRINK_CONVERSION_REGION(beg, end, coding, str, encodep)	\
  do {									\
    if (*(end) - *(beg) > shrink_conversion_region_threshhold)		\
      {									\
        if (encodep) shrink_encoding_region (beg, end, coding, str);	\
        else shrink_decoding_region (beg, end, coding, str);		\
      }									\
  } while (0)

static Lisp_Object
code_convert_region_unwind (dummy)
     Lisp_Object dummy;
{
  inhibit_pre_post_conversion = 0;
  return Qnil;
}

/* Store information about all compositions in the range FROM and TO
   of OBJ in memory blocks pointed by CODING->cmp_data.  OBJ is a
   buffer or a string, defaults to the current buffer.  */

void
coding_save_composition (coding, from, to, obj)
     struct coding_system *coding;
     int from, to;
     Lisp_Object obj;
{
  Lisp_Object prop;
  int start, end;

  if (coding->composing == COMPOSITION_DISABLED)
    return;
  if (!coding->cmp_data)
    coding_allocate_composition_data (coding, from);
  if (!find_composition (from, to, &start, &end, &prop, obj)
      || end > to)
    return;
  if (start < from
      && (!find_composition (end, to, &start, &end, &prop, obj)
	  || end > to))
    return;
  coding->composing = COMPOSITION_NO;
  do
    {
      if (COMPOSITION_VALID_P (start, end, prop))
	{
	  enum composition_method method = COMPOSITION_METHOD (prop);
	  if (coding->cmp_data->used + COMPOSITION_DATA_MAX_BUNCH_LENGTH
	      >= COMPOSITION_DATA_SIZE)
	    coding_allocate_composition_data (coding, from);
	  /* For relative composition, we remember start and end
             positions, for the other compositions, we also remember
             components.  */
	  CODING_ADD_COMPOSITION_START (coding, start - from, method);
	  if (method != COMPOSITION_RELATIVE)
	    {
	      /* We must store a*/
	      Lisp_Object val, ch;

	      val = COMPOSITION_COMPONENTS (prop);
	      if (CONSP (val))
		while (CONSP (val))
		  {
		    ch = XCAR (val), val = XCDR (val);
		    CODING_ADD_COMPOSITION_COMPONENT (coding, XINT (ch));
		  }
	      else if (VECTORP (val) || STRINGP (val))
		{
		  int len = (VECTORP (val)
			     ? XVECTOR (val)->size : XSTRING (val)->size);
		  int i;
		  for (i = 0; i < len; i++)
		    {
		      ch = (STRINGP (val)
			    ? Faref (val, make_number (i))
			    : XVECTOR (val)->contents[i]);
		      CODING_ADD_COMPOSITION_COMPONENT (coding, XINT (ch));
		    }
		}
	      else		/* INTEGERP (val) */
		CODING_ADD_COMPOSITION_COMPONENT (coding, XINT (val));
	    }
	  CODING_ADD_COMPOSITION_END (coding, end - from);
	}
      start = end;
    }
  while (start < to
	 && find_composition (start, to, &start, &end, &prop, obj)
	 && end <= to);

  /* Make coding->cmp_data point to the first memory block.  */
  while (coding->cmp_data->prev)
    coding->cmp_data = coding->cmp_data->prev;
  coding->cmp_data_start = 0;
}

/* Reflect the saved information about compositions to OBJ.
   CODING->cmp_data points to a memory block for the information.  OBJ
   is a buffer or a string, defaults to the current buffer.  */

void
coding_restore_composition (coding, obj)
     struct coding_system *coding;
     Lisp_Object obj;
{
  struct composition_data *cmp_data = coding->cmp_data;

  if (!cmp_data)
    return;

  while (cmp_data->prev)
    cmp_data = cmp_data->prev;

  while (cmp_data)
    {
      int i;

      for (i = 0; i < cmp_data->used && cmp_data->data[i] > 0;
	   i += cmp_data->data[i])
	{
	  int *data = cmp_data->data + i;
	  enum composition_method method = (enum composition_method) data[3];
	  Lisp_Object components;

	  if (method == COMPOSITION_RELATIVE)
	    components = Qnil;
	  else
	    {
	      int len = data[0] - 4, j;
	      Lisp_Object args[MAX_COMPOSITION_COMPONENTS * 2 - 1];

	      for (j = 0; j < len; j++)
		args[j] = make_number (data[4 + j]);
	      components = (method == COMPOSITION_WITH_ALTCHARS
			    ? Fstring (len, args) : Fvector (len, args));
	    }
	  compose_text (data[1], data[2], components, Qnil, obj);
	}
      cmp_data = cmp_data->next;
    }
}

/* Decode (if ENCODEP is zero) or encode (if ENCODEP is nonzero) the
   text from FROM to TO (byte positions are FROM_BYTE and TO_BYTE) by
   coding system CODING, and return the status code of code conversion
   (currently, this value has no meaning).

   How many characters (and bytes) are converted to how many
   characters (and bytes) are recorded in members of the structure
   CODING.

   If REPLACE is nonzero, we do various things as if the original text
   is deleted and a new text is inserted.  See the comments in
   replace_range (insdel.c) to know what we are doing.

   If REPLACE is zero, it is assumed that the source text is unibyte.
   Otherwise, it is assumed that the source text is multibyte.  */

int
code_convert_region (from, from_byte, to, to_byte, coding, encodep, replace)
     int from, from_byte, to, to_byte, encodep, replace;
     struct coding_system *coding;
{
  int len = to - from, len_byte = to_byte - from_byte;
  int nchars_del = 0, nbytes_del = 0;
  int require, inserted, inserted_byte;
  int head_skip, tail_skip, total_skip = 0;
  Lisp_Object saved_coding_symbol;
  int first = 1;
  unsigned char *src, *dst;
  Lisp_Object deletion;
  int orig_point = PT, orig_len = len;
  int prev_Z;
  int multibyte_p = !NILP (current_buffer->enable_multibyte_characters);

  deletion = Qnil;
  saved_coding_symbol = coding->symbol;

  if (from < PT && PT < to)
    {
      TEMP_SET_PT_BOTH (from, from_byte);
      orig_point = from;
    }

  if (replace)
    {
      int saved_from = from;
      int saved_inhibit_modification_hooks;

      prepare_to_modify_buffer (from, to, &from);
      if (saved_from != from)
	{
	  to = from + len;
	  from_byte = CHAR_TO_BYTE (from), to_byte = CHAR_TO_BYTE (to);
	  len_byte = to_byte - from_byte;
	}

      /* The code conversion routine can not preserve text properties
	 for now.  So, we must remove all text properties in the
	 region.  Here, we must suppress all modification hooks.  */
      saved_inhibit_modification_hooks = inhibit_modification_hooks;
      inhibit_modification_hooks = 1;
      Fset_text_properties (make_number (from), make_number (to), Qnil, Qnil);
      inhibit_modification_hooks = saved_inhibit_modification_hooks;
    }

  if (! encodep && CODING_REQUIRE_DETECTION (coding))
    {
      /* We must detect encoding of text and eol format.  */

      if (from < GPT && to > GPT)
	move_gap_both (from, from_byte);
      if (coding->type == coding_type_undecided)
	{
	  detect_coding (coding, BYTE_POS_ADDR (from_byte), len_byte);
	  if (coding->type == coding_type_undecided)
	    {
	      /* It seems that the text contains only ASCII, but we
		 should not leave it undecided because the deeper
		 decoding routine (decode_coding) tries to detect the
		 encodings again in vain.  */
	      coding->type = coding_type_emacs_mule;
	      coding->category_idx = CODING_CATEGORY_IDX_EMACS_MULE;
	      /* As emacs-mule decoder will handle composition, we
		 need this setting to allocate coding->cmp_data
		 later.  */
	      coding->composing = COMPOSITION_NO;
	    }
	}
      if (coding->eol_type == CODING_EOL_UNDECIDED
	  && coding->type != coding_type_ccl)
	{
	  detect_eol (coding, BYTE_POS_ADDR (from_byte), len_byte);
	  if (coding->eol_type == CODING_EOL_UNDECIDED)
	    coding->eol_type = CODING_EOL_LF;
	  /* We had better recover the original eol format if we
	     encounter an inconsistent eol format while decoding.  */
	  coding->mode |= CODING_MODE_INHIBIT_INCONSISTENT_EOL;
	}
    }

  /* Now we convert the text.  */

  /* For encoding, we must process pre-write-conversion in advance.  */
  if (! inhibit_pre_post_conversion
      && encodep
      && SYMBOLP (coding->pre_write_conversion)
      && ! NILP (Ffboundp (coding->pre_write_conversion)))
    {
      /* The function in pre-write-conversion may put a new text in a
         new buffer.  */
      struct buffer *prev = current_buffer;
      Lisp_Object new;

      record_unwind_protect (code_convert_region_unwind, Qnil);
      /* We should not call any more pre-write/post-read-conversion
         functions while this pre-write-conversion is running.  */
      inhibit_pre_post_conversion = 1;
      call2 (coding->pre_write_conversion,
	     make_number (from), make_number (to));
      inhibit_pre_post_conversion = 0;
      /* Discard the unwind protect.  */
      specpdl_ptr--;

      if (current_buffer != prev)
	{
	  len = ZV - BEGV;
	  new = Fcurrent_buffer ();
	  set_buffer_internal_1 (prev);
	  del_range_2 (from, from_byte, to, to_byte, 0);
	  TEMP_SET_PT_BOTH (from, from_byte);
	  insert_from_buffer (XBUFFER (new), 1, len, 0);
	  Fkill_buffer (new);
	  if (orig_point >= to)
	    orig_point += len - orig_len;
	  else if (orig_point > from)
	    orig_point = from;
	  orig_len = len;
	  to = from + len;
	  from_byte = CHAR_TO_BYTE (from);
	  to_byte = CHAR_TO_BYTE (to);
	  len_byte = to_byte - from_byte;
	  TEMP_SET_PT_BOTH (from, from_byte);
	}
    }

  if (replace)
    {
      if (! EQ (current_buffer->undo_list, Qt))
	deletion = make_buffer_string_both (from, from_byte, to, to_byte, 1);
      else
	{
	  nchars_del = to - from;
	  nbytes_del = to_byte - from_byte;
	}
    }

  if (coding->composing != COMPOSITION_DISABLED)
    {
      if (encodep)
	coding_save_composition (coding, from, to, Fcurrent_buffer ());
      else
	coding_allocate_composition_data (coding, from);
    }

  /* Try to skip the heading and tailing ASCIIs.  */
  if (coding->type != coding_type_ccl)
    {
      int from_byte_orig = from_byte, to_byte_orig = to_byte;

      if (from < GPT && GPT < to)
	move_gap_both (from, from_byte);
      SHRINK_CONVERSION_REGION (&from_byte, &to_byte, coding, NULL, encodep);
      if (from_byte == to_byte
	  && (encodep || NILP (coding->post_read_conversion))
	  && ! CODING_REQUIRE_FLUSHING (coding))
	{
	  coding->produced = len_byte;
	  coding->produced_char = len;
	  if (!replace)
	    /* We must record and adjust for this new text now.  */
	    adjust_after_insert (from, from_byte_orig, to, to_byte_orig, len);
	  return 0;
	}

      head_skip = from_byte - from_byte_orig;
      tail_skip = to_byte_orig - to_byte;
      total_skip = head_skip + tail_skip;
      from += head_skip;
      to -= tail_skip;
      len -= total_skip; len_byte -= total_skip;
    }

  /* For conversion, we must put the gap before the text in addition to
     making the gap larger for efficient decoding.  The required gap
     size starts from 2000 which is the magic number used in make_gap.
     But, after one batch of conversion, it will be incremented if we
     find that it is not enough .  */
  require = 2000;

  if (GAP_SIZE  < require)
    make_gap (require - GAP_SIZE);
  move_gap_both (from, from_byte);

  inserted = inserted_byte = 0;

  GAP_SIZE += len_byte;
  ZV -= len;
  Z -= len;
  ZV_BYTE -= len_byte;
  Z_BYTE -= len_byte;

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (!encodep && coding->src_multibyte)
    {
      /* Decoding routines expects that the source text is unibyte.
	 We must convert 8-bit characters of multibyte form to
	 unibyte.  */
      int len_byte_orig = len_byte;
      len_byte = str_as_unibyte (GAP_END_ADDR - len_byte, len_byte);
      if (len_byte < len_byte_orig)
	safe_bcopy (GAP_END_ADDR - len_byte_orig, GAP_END_ADDR - len_byte,
		    len_byte);
      coding->src_multibyte = 0;
    }

  for (;;)
    {
      int result;

      /* The buffer memory is now:
	 +--------+converted-text+---------+-------original-text-------+---+
	 |<-from->|<--inserted-->|---------|<--------len_byte--------->|---|
		  |<---------------------- GAP ----------------------->|  */
      src = GAP_END_ADDR - len_byte;
      dst = GPT_ADDR + inserted_byte;

      if (encodep)
	result = encode_coding (coding, src, dst, len_byte, 0);
      else
	{
	  if (coding->composing != COMPOSITION_DISABLED)
	    coding->cmp_data->char_offset = from + inserted;
	  result = decode_coding (coding, src, dst, len_byte, 0);
	}

      /* The buffer memory is now:
	 +--------+-------converted-text----+--+------original-text----+---+
	 |<-from->|<-inserted->|<-produced->|--|<-(len_byte-consumed)->|---|
		  |<---------------------- GAP ----------------------->|  */

      inserted += coding->produced_char;
      inserted_byte += coding->produced;
      len_byte -= coding->consumed;

      if (result == CODING_FINISH_INSUFFICIENT_CMP)
	{
	  coding_allocate_composition_data (coding, from + inserted);
	  continue;
	}

      src += coding->consumed;
      dst += coding->produced;

      if (result == CODING_FINISH_NORMAL)
	{
	  src += len_byte;
	  break;
	}
      if (! encodep && result == CODING_FINISH_INCONSISTENT_EOL)
	{
	  unsigned char *pend = dst, *p = pend - inserted_byte;
	  Lisp_Object eol_type;

	  /* Encode LFs back to the original eol format (CR or CRLF).  */
	  if (coding->eol_type == CODING_EOL_CR)
	    {
	      while (p < pend) if (*p++ == '\n') p[-1] = '\r';
	    }
	  else
	    {
	      int count = 0;

	      while (p < pend) if (*p++ == '\n') count++;
	      if (src - dst < count)
		{
		  /* We don't have sufficient room for encoding LFs
		     back to CRLF.  We must record converted and
		     not-yet-converted text back to the buffer
		     content, enlarge the gap, then record them out of
		     the buffer contents again.  */
		  int add = len_byte + inserted_byte;

		  GAP_SIZE -= add;
		  ZV += add; Z += add; ZV_BYTE += add; Z_BYTE += add;
		  GPT += inserted_byte; GPT_BYTE += inserted_byte;
		  make_gap (count - GAP_SIZE);
		  GAP_SIZE += add;
		  ZV -= add; Z -= add; ZV_BYTE -= add; Z_BYTE -= add;
		  GPT -= inserted_byte; GPT_BYTE -= inserted_byte;
		  /* Don't forget to update SRC, DST, and PEND.  */
		  src = GAP_END_ADDR - len_byte;
		  dst = GPT_ADDR + inserted_byte;
		  pend = dst;
		}
	      inserted += count;
	      inserted_byte += count;
	      coding->produced += count;
	      p = dst = pend + count;
	      while (count)
		{
		  *--p = *--pend;
		  if (*p == '\n') count--, *--p = '\r';
		}
	    }

	  /* Suppress eol-format conversion in the further conversion.  */
	  coding->eol_type = CODING_EOL_LF;

	  /* Set the coding system symbol to that for Unix-like EOL.  */
	  eol_type = Fget (saved_coding_symbol, Qeol_type);
	  if (VECTORP (eol_type)
	      && XVECTOR (eol_type)->size == 3
	      && SYMBOLP (XVECTOR (eol_type)->contents[CODING_EOL_LF]))
	    coding->symbol = XVECTOR (eol_type)->contents[CODING_EOL_LF];
	  else
	    coding->symbol = saved_coding_symbol;

	  continue;
	}
      if (len_byte <= 0)
	{
	  if (coding->type != coding_type_ccl
	      || coding->mode & CODING_MODE_LAST_BLOCK)
	    break;
	  coding->mode |= CODING_MODE_LAST_BLOCK;
	  continue;
	}
      if (result == CODING_FINISH_INSUFFICIENT_SRC)
	{
	  /* The source text ends in invalid codes.  Let's just
	     make them valid buffer contents, and finish conversion.  */
	  if (multibyte_p)
	    {
	      unsigned char *start = dst;

	      inserted += len_byte;
	      while (len_byte--)
		{
		  int c = *src++;
		  dst += CHAR_STRING (c, dst);
		}

	      inserted_byte += dst - start;
	    }
	  else
	    {
	      inserted += len_byte;
	      inserted_byte += len_byte;
	      while (len_byte--)
		*dst++ = *src++;
	    }
	  break;
	}
      if (result == CODING_FINISH_INTERRUPT)
	{
	  /* The conversion procedure was interrupted by a user.  */
	  break;
	}
      /* Now RESULT == CODING_FINISH_INSUFFICIENT_DST  */
      if (coding->consumed < 1)
	{
	  /* It's quite strange to require more memory without
	     consuming any bytes.  Perhaps CCL program bug.  */
	  break;
	}
      if (first)
	{
	  /* We have just done the first batch of conversion which was
	     stopped because of insufficient gap.  Let's reconsider the
	     required gap size (i.e. SRT - DST) now.

	     We have converted ORIG bytes (== coding->consumed) into
	     NEW bytes (coding->produced).  To convert the remaining
	     LEN bytes, we may need REQUIRE bytes of gap, where:
		REQUIRE + LEN_BYTE = LEN_BYTE * (NEW / ORIG)
		REQUIRE = LEN_BYTE * (NEW - ORIG) / ORIG
	     Here, we are sure that NEW >= ORIG.  */
	  float ratio = coding->produced - coding->consumed;
	  ratio /= coding->consumed;
	  require = len_byte * ratio;
	  first = 0;
	}
      if ((src - dst) < (require + 2000))
	{
	  /* See the comment above the previous call of make_gap.  */
	  int add = len_byte + inserted_byte;

	  GAP_SIZE -= add;
	  ZV += add; Z += add; ZV_BYTE += add; Z_BYTE += add;
	  GPT += inserted_byte; GPT_BYTE += inserted_byte;
	  make_gap (require + 2000);
	  GAP_SIZE += add;
	  ZV -= add; Z -= add; ZV_BYTE -= add; Z_BYTE -= add;
	  GPT -= inserted_byte; GPT_BYTE -= inserted_byte;
	}
    }
  if (src - dst > 0) *dst = 0; /* Put an anchor.  */

  if (encodep && coding->dst_multibyte)
    {
      /* The output is unibyte.  We must convert 8-bit characters to
	 multibyte form.  */
      if (inserted_byte * 2 > GAP_SIZE)
	{
	  GAP_SIZE -= inserted_byte;
	  ZV += inserted_byte; Z += inserted_byte;
	  ZV_BYTE += inserted_byte; Z_BYTE += inserted_byte;
	  GPT += inserted_byte; GPT_BYTE += inserted_byte;
	  make_gap (inserted_byte - GAP_SIZE);
	  GAP_SIZE += inserted_byte;
	  ZV -= inserted_byte; Z -= inserted_byte;
	  ZV_BYTE -= inserted_byte; Z_BYTE -= inserted_byte;
	  GPT -= inserted_byte; GPT_BYTE -= inserted_byte;
	}
      inserted_byte = str_to_multibyte (GPT_ADDR, GAP_SIZE, inserted_byte);
    }

  /* If we shrank the conversion area, adjust it now.  */
  if (total_skip > 0)
    {
      if (tail_skip > 0)
	safe_bcopy (GAP_END_ADDR, GPT_ADDR + inserted_byte, tail_skip);
      inserted += total_skip; inserted_byte += total_skip;
      GAP_SIZE += total_skip;
      GPT -= head_skip; GPT_BYTE -= head_skip;
      ZV -= total_skip; ZV_BYTE -= total_skip;
      Z -= total_skip; Z_BYTE -= total_skip;
      from -= head_skip; from_byte -= head_skip;
      to += tail_skip; to_byte += tail_skip;
    }

  prev_Z = Z;
  if (! EQ (current_buffer->undo_list, Qt))
    adjust_after_replace (from, from_byte, deletion, inserted, inserted_byte);
  else
    adjust_after_replace_noundo (from, from_byte, nchars_del, nbytes_del,
				 inserted, inserted_byte);
  inserted = Z - prev_Z;

  if (!encodep && coding->cmp_data && coding->cmp_data->used)
    coding_restore_composition (coding, Fcurrent_buffer ());
  coding_free_composition_data (coding);

  if (! inhibit_pre_post_conversion
      && ! encodep && ! NILP (coding->post_read_conversion))
    {
      Lisp_Object val;

      if (from != PT)
	TEMP_SET_PT_BOTH (from, from_byte);
      prev_Z = Z;
      record_unwind_protect (code_convert_region_unwind, Qnil);
      /* We should not call any more pre-write/post-read-conversion
         functions while this post-read-conversion is running.  */
      inhibit_pre_post_conversion = 1;
      val = call1 (coding->post_read_conversion, make_number (inserted));
      inhibit_pre_post_conversion = 0;
      /* Discard the unwind protect.  */
      specpdl_ptr--;
      CHECK_NUMBER (val);
      inserted += Z - prev_Z;
    }

  if (orig_point >= from)
    {
      if (orig_point >= from + orig_len)
	orig_point += inserted - orig_len;
      else
	orig_point = from;
      TEMP_SET_PT (orig_point);
    }

  if (replace)
    {
      signal_after_change (from, to - from, inserted);
      update_compositions (from, from + inserted, CHECK_BORDER);
    }

  {
    coding->consumed = to_byte - from_byte;
    coding->consumed_char = to - from;
    coding->produced = inserted_byte;
    coding->produced_char = inserted;
  }

  return 0;
}

Lisp_Object
run_pre_post_conversion_on_str (str, coding, encodep)
     Lisp_Object str;
     struct coding_system *coding;
     int encodep;
{
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;
  int multibyte = STRING_MULTIBYTE (str);
  Lisp_Object buffer;
  struct buffer *buf;

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
  record_unwind_protect (code_convert_region_unwind, Qnil);
  GCPRO1 (str);

  buffer = Fget_buffer_create (build_string (" *code-converting-work*"));
  buf = XBUFFER (buffer);

  buf->directory = current_buffer->directory;
  buf->read_only = Qnil;
  buf->filename = Qnil;
  buf->undo_list = Qt;
  buf->overlays_before = Qnil;
  buf->overlays_after = Qnil;

  set_buffer_internal (buf);
  /* We must insert the contents of STR as is without
     unibyte<->multibyte conversion.  For that, we adjust the
     multibyteness of the working buffer to that of STR.  */
  Ferase_buffer ();
  buf->enable_multibyte_characters = multibyte ? Qt : Qnil;

  insert_from_string (str, 0, 0,
		      XSTRING (str)->size, STRING_BYTES (XSTRING (str)), 0);
  UNGCPRO;
  inhibit_pre_post_conversion = 1;
  if (encodep)
    call2 (coding->pre_write_conversion, make_number (BEG), make_number (Z));
  else
    {
      TEMP_SET_PT_BOTH (BEG, BEG_BYTE);
      call1 (coding->post_read_conversion, make_number (Z - BEG));
    }
  inhibit_pre_post_conversion = 0;
  str = make_buffer_string (BEG, Z, 1);
  return unbind_to (count, str);
}

Lisp_Object
decode_coding_string (str, coding, nocopy)
     Lisp_Object str;
     struct coding_system *coding;
     int nocopy;
{
  int len;
  struct conversion_buffer buf;
  int from, to_byte;
  Lisp_Object saved_coding_symbol;
  int result;
  int require_decoding;
  int shrinked_bytes = 0;
  Lisp_Object newstr;
  int consumed, consumed_char, produced, produced_char;

  from = 0;
  to_byte = STRING_BYTES (XSTRING (str));

  saved_coding_symbol = coding->symbol;
  coding->src_multibyte = STRING_MULTIBYTE (str);
  coding->dst_multibyte = 1;
  if (CODING_REQUIRE_DETECTION (coding))
    {
      /* See the comments in code_convert_region.  */
      if (coding->type == coding_type_undecided)
	{
	  detect_coding (coding, XSTRING (str)->data, to_byte);
	  if (coding->type == coding_type_undecided)
	    {
	      coding->type = coding_type_emacs_mule;
	      coding->category_idx = CODING_CATEGORY_IDX_EMACS_MULE;
	      /* As emacs-mule decoder will handle composition, we
		 need this setting to allocate coding->cmp_data
		 later.  */
	      coding->composing = COMPOSITION_NO;
	    }
	}
      if (coding->eol_type == CODING_EOL_UNDECIDED
	  && coding->type != coding_type_ccl)
	{
	  saved_coding_symbol = coding->symbol;
	  detect_eol (coding, XSTRING (str)->data, to_byte);
	  if (coding->eol_type == CODING_EOL_UNDECIDED)
	    coding->eol_type = CODING_EOL_LF;
	  /* We had better recover the original eol format if we
	     encounter an inconsistent eol format while decoding.  */
	  coding->mode |= CODING_MODE_INHIBIT_INCONSISTENT_EOL;
	}
    }

  if (coding->type == coding_type_no_conversion
      || coding->type == coding_type_raw_text)
    coding->dst_multibyte = 0;

  require_decoding = CODING_REQUIRE_DECODING (coding);

  if (STRING_MULTIBYTE (str))
    {
      /* Decoding routines expect the source text to be unibyte.  */
      str = Fstring_as_unibyte (str);
      to_byte = STRING_BYTES (XSTRING (str));
      nocopy = 1;
      coding->src_multibyte = 0;
    }

  /* Try to skip the heading and tailing ASCIIs.  */
  if (require_decoding && coding->type != coding_type_ccl)
    {
      SHRINK_CONVERSION_REGION (&from, &to_byte, coding, XSTRING (str)->data,
				0);
      if (from == to_byte)
	require_decoding = 0;
      shrinked_bytes = from + (STRING_BYTES (XSTRING (str)) - to_byte);
    }

  if (!require_decoding)
    {
      coding->consumed = STRING_BYTES (XSTRING (str));
      coding->consumed_char = XSTRING (str)->size;
      if (coding->dst_multibyte)
	{
	  str = Fstring_as_multibyte (str);
	  nocopy = 1;
	}
      coding->produced = STRING_BYTES (XSTRING (str));
      coding->produced_char = XSTRING (str)->size;
      return (nocopy ? str : Fcopy_sequence (str));
    }

  if (coding->composing != COMPOSITION_DISABLED)
    coding_allocate_composition_data (coding, from);
  len = decoding_buffer_size (coding, to_byte - from);
  allocate_conversion_buffer (buf, len);

  consumed = consumed_char = produced = produced_char = 0;
  while (1)
    {
      result = decode_coding (coding, XSTRING (str)->data + from + consumed,
			      buf.data + produced, to_byte - from - consumed,
			      buf.size - produced);
      consumed += coding->consumed;
      consumed_char += coding->consumed_char;
      produced += coding->produced;
      produced_char += coding->produced_char;
      if (result == CODING_FINISH_NORMAL
	  || (result == CODING_FINISH_INSUFFICIENT_SRC
	      && coding->consumed == 0))
	break;
      if (result == CODING_FINISH_INSUFFICIENT_CMP)
	coding_allocate_composition_data (coding, from + produced_char);
      else if (result == CODING_FINISH_INSUFFICIENT_DST)
	extend_conversion_buffer (&buf);
      else if (result == CODING_FINISH_INCONSISTENT_EOL)
	{
	  Lisp_Object eol_type;

	  /* Recover the original EOL format.  */
	  if (coding->eol_type == CODING_EOL_CR)
	    {
	      unsigned char *p;
	      for (p = buf.data; p < buf.data + produced; p++)
		if (*p == '\n') *p = '\r';
	    }
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    {
	      int num_eol = 0;
	      unsigned char *p0, *p1;
	      for (p0 = buf.data, p1 = p0 + produced; p0 < p1; p0++)
		if (*p0 == '\n') num_eol++;
	      if (produced + num_eol >= buf.size)
		extend_conversion_buffer (&buf);
	      for (p0 = buf.data + produced, p1 = p0 + num_eol; p0 > buf.data;)
		{
		  *--p1 = *--p0;
		  if (*p0 == '\n') *--p1 = '\r';
		}
	      produced += num_eol;
	      produced_char += num_eol;
	    }
	  /* Suppress eol-format conversion in the further conversion.  */
	  coding->eol_type = CODING_EOL_LF;

	  /* Set the coding system symbol to that for Unix-like EOL.  */
	  eol_type = Fget (saved_coding_symbol, Qeol_type);
	  if (VECTORP (eol_type)
	      && XVECTOR (eol_type)->size == 3
	      && SYMBOLP (XVECTOR (eol_type)->contents[CODING_EOL_LF]))
	    coding->symbol = XVECTOR (eol_type)->contents[CODING_EOL_LF];
	  else
	    coding->symbol = saved_coding_symbol;


	}
    }

  coding->consumed = consumed;
  coding->consumed_char = consumed_char;
  coding->produced = produced;
  coding->produced_char = produced_char;

  if (coding->dst_multibyte)
    newstr = make_uninit_multibyte_string (produced_char + shrinked_bytes,
					   produced + shrinked_bytes);
  else
    newstr = make_uninit_string (produced + shrinked_bytes);
  if (from > 0)
    bcopy (XSTRING (str)->data, XSTRING (newstr)->data, from);
  bcopy (buf.data, XSTRING (newstr)->data + from, produced);
  if (shrinked_bytes > from)
    bcopy (XSTRING (str)->data + to_byte,
	   XSTRING (newstr)->data + from + produced,
	   shrinked_bytes - from);
  free_conversion_buffer (&buf);

  if (coding->cmp_data && coding->cmp_data->used)
    coding_restore_composition (coding, newstr);
  coding_free_composition_data (coding);

  if (SYMBOLP (coding->post_read_conversion)
      && !NILP (Ffboundp (coding->post_read_conversion)))
    newstr = run_pre_post_conversion_on_str (newstr, coding, 0);

  return newstr;
}

Lisp_Object
encode_coding_string (str, coding, nocopy)
     Lisp_Object str;
     struct coding_system *coding;
     int nocopy;
{
  int len;
  struct conversion_buffer buf;
  int from, to, to_byte;
  int result;
  int shrinked_bytes = 0;
  Lisp_Object newstr;
  int consumed, consumed_char, produced, produced_char;

  if (SYMBOLP (coding->pre_write_conversion)
      && !NILP (Ffboundp (coding->pre_write_conversion)))
    str = run_pre_post_conversion_on_str (str, coding, 1);

  from = 0;
  to = XSTRING (str)->size;
  to_byte = STRING_BYTES (XSTRING (str));

  /* Encoding routines determine the multibyteness of the source text
     by coding->src_multibyte.  */
  coding->src_multibyte = STRING_MULTIBYTE (str);
  coding->dst_multibyte = 0;
  if (! CODING_REQUIRE_ENCODING (coding))
    {
      coding->consumed = STRING_BYTES (XSTRING (str));
      coding->consumed_char = XSTRING (str)->size;
      if (STRING_MULTIBYTE (str))
	{
	  str = Fstring_as_unibyte (str);
	  nocopy = 1;
	}
      coding->produced = STRING_BYTES (XSTRING (str));
      coding->produced_char = XSTRING (str)->size;
      return (nocopy ? str : Fcopy_sequence (str));
    }

  if (coding->composing != COMPOSITION_DISABLED)
    coding_save_composition (coding, from, to, str);

  /* Try to skip the heading and tailing ASCIIs.  */
  if (coding->type != coding_type_ccl)
    {
      SHRINK_CONVERSION_REGION (&from, &to_byte, coding, XSTRING (str)->data,
				1);
      if (from == to_byte)
	return (nocopy ? str : Fcopy_sequence (str));
      shrinked_bytes = from + (STRING_BYTES (XSTRING (str)) - to_byte);
    }

  len = encoding_buffer_size (coding, to_byte - from);
  allocate_conversion_buffer (buf, len);

  consumed = consumed_char = produced = produced_char = 0;
  while (1)
    {
      result = encode_coding (coding, XSTRING (str)->data + from + consumed,
			      buf.data + produced, to_byte - from - consumed,
			      buf.size - produced);
      consumed += coding->consumed;
      consumed_char += coding->consumed_char;
      produced += coding->produced;
      produced_char += coding->produced_char;
      if (result == CODING_FINISH_NORMAL
	  || (result == CODING_FINISH_INSUFFICIENT_SRC
	      && coding->consumed == 0))
	break;
      /* Now result should be CODING_FINISH_INSUFFICIENT_DST.  */
      extend_conversion_buffer (&buf);
    }

  coding->consumed = consumed;
  coding->consumed_char = consumed_char;
  coding->produced = produced;
  coding->produced_char = produced_char;

  newstr = make_uninit_string (produced + shrinked_bytes);
  if (from > 0)
    bcopy (XSTRING (str)->data, XSTRING (newstr)->data, from);
  bcopy (buf.data, XSTRING (newstr)->data + from, produced);
  if (shrinked_bytes > from)
    bcopy (XSTRING (str)->data + to_byte,
	   XSTRING (newstr)->data + from + produced,
	   shrinked_bytes - from);

  free_conversion_buffer (&buf);
  coding_free_composition_data (coding);

  return newstr;
}


#ifdef emacs
/*** 8. Emacs Lisp library functions ***/

DEFUN ("coding-system-p", Fcoding_system_p, Scoding_system_p, 1, 1, 0,
       doc: /* Return t if OBJECT is nil or a coding-system.
See the documentation of `make-coding-system' for information
about coding-system objects.  */)
     (obj)
     Lisp_Object obj;
{
  if (NILP (obj))
    return Qt;
  if (!SYMBOLP (obj))
    return Qnil;
  /* Get coding-spec vector for OBJ.  */
  obj = Fget (obj, Qcoding_system);
  return ((VECTORP (obj) && XVECTOR (obj)->size == 5)
	  ? Qt : Qnil);
}

DEFUN ("read-non-nil-coding-system", Fread_non_nil_coding_system,
       Sread_non_nil_coding_system, 1, 1, 0,
       doc: /* Read a coding system from the minibuffer, prompting with string PROMPT.  */)
     (prompt)
     Lisp_Object prompt;
{
  Lisp_Object val;
  do
    {
      val = Fcompleting_read (prompt, Vcoding_system_alist, Qnil,
			      Qt, Qnil, Qcoding_system_history, Qnil, Qnil);
    }
  while (XSTRING (val)->size == 0);
  return (Fintern (val, Qnil));
}

DEFUN ("read-coding-system", Fread_coding_system, Sread_coding_system, 1, 2, 0,
       doc: /* Read a coding system from the minibuffer, prompting with string PROMPT.
If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.  */)
     (prompt, default_coding_system)
     Lisp_Object prompt, default_coding_system;
{
  Lisp_Object val;
  if (SYMBOLP (default_coding_system))
    XSETSTRING (default_coding_system, XSYMBOL (default_coding_system)->name);
  val = Fcompleting_read (prompt, Vcoding_system_alist, Qnil,
			  Qt, Qnil, Qcoding_system_history,
			  default_coding_system, Qnil);
  return (XSTRING (val)->size == 0 ? Qnil : Fintern (val, Qnil));
}

DEFUN ("check-coding-system", Fcheck_coding_system, Scheck_coding_system,
       1, 1, 0,
       doc: /* Check validity of CODING-SYSTEM.
If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
It is valid if it is a symbol with a non-nil `coding-system' property.
The value of property should be a vector of length 5.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  if (!NILP (Fcoding_system_p (coding_system)))
    return coding_system;
  while (1)
    Fsignal (Qcoding_system_error, Fcons (coding_system, Qnil));
}

Lisp_Object
detect_coding_system (src, src_bytes, highest, multibytep)
     unsigned char *src;
     int src_bytes, highest;
     int multibytep;
{
  int coding_mask, eol_type;
  Lisp_Object val, tmp;
  int dummy;

  coding_mask = detect_coding_mask (src, src_bytes, NULL, &dummy, multibytep);
  eol_type  = detect_eol_type (src, src_bytes, &dummy);
  if (eol_type == CODING_EOL_INCONSISTENT)
    eol_type = CODING_EOL_UNDECIDED;

  if (!coding_mask)
    {
      val = Qundecided;
      if (eol_type != CODING_EOL_UNDECIDED)
	{
	  Lisp_Object val2;
	  val2 = Fget (Qundecided, Qeol_type);
	  if (VECTORP (val2))
	    val = XVECTOR (val2)->contents[eol_type];
	}
      return (highest ? val : Fcons (val, Qnil));
    }

  /* At first, gather possible coding systems in VAL.  */
  val = Qnil;
  for (tmp = Vcoding_category_list; CONSP (tmp); tmp = XCDR (tmp))
    {
      Lisp_Object category_val, category_index;

      category_index = Fget (XCAR (tmp), Qcoding_category_index);
      category_val = Fsymbol_value (XCAR (tmp));
      if (!NILP (category_val)
	  && NATNUMP (category_index)
	  && (coding_mask & (1 << XFASTINT (category_index))))
	{
	  val = Fcons (category_val, val);
	  if (highest)
	    break;
	}
    }
  if (!highest)
    val = Fnreverse (val);

  /* Then, replace the elements with subsidiary coding systems.  */
  for (tmp = val; CONSP (tmp); tmp = XCDR (tmp))
    {
      if (eol_type != CODING_EOL_UNDECIDED
	  && eol_type != CODING_EOL_INCONSISTENT)
	{
	  Lisp_Object eol;
	  eol = Fget (XCAR (tmp), Qeol_type);
	  if (VECTORP (eol))
	    XSETCAR (tmp, XVECTOR (eol)->contents[eol_type]);
	}
    }
  return (highest ? XCAR (val) : val);
}

DEFUN ("detect-coding-region", Fdetect_coding_region, Sdetect_coding_region,
       2, 3, 0,
       doc: /* Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority.

If only ASCII characters are found, it returns a list of single element
`undecided' or its subsidiary coding system according to a detected
end-of-line format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.  */)
     (start, end, highest)
     Lisp_Object start, end, highest;
{
  int from, to;
  int from_byte, to_byte;
  int include_anchor_byte = 0;

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);

  validate_region (&start, &end);
  from = XINT (start), to = XINT (end);
  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  if (from < GPT && to >= GPT)
    move_gap_both (to, to_byte);
  /* If we an anchor byte `\0' follows the region, we include it in
     the detecting source.  Then code detectors can handle the tailing
     byte sequence more accurately.

     Fix me: This is not an perfect solution.  It is better that we
     add one more argument, say LAST_BLOCK, to all detect_coding_XXX.
  */
  if (to == Z || (to == GPT && GAP_SIZE > 0))
    include_anchor_byte = 1;
  return detect_coding_system (BYTE_POS_ADDR (from_byte),
			       to_byte - from_byte + include_anchor_byte,
			       !NILP (highest),
			       !NILP (current_buffer
				      ->enable_multibyte_characters));
}

DEFUN ("detect-coding-string", Fdetect_coding_string, Sdetect_coding_string,
       1, 2, 0,
       doc: /* Detect coding system of the text in STRING.
Return a list of possible coding systems ordered by priority.

If only ASCII characters are found, it returns a list of single element
`undecided' or its subsidiary coding system according to a detected
end-of-line format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.  */)
     (string, highest)
     Lisp_Object string, highest;
{
  CHECK_STRING (string);

  return detect_coding_system (XSTRING (string)->data,
			       /* "+ 1" is to include the anchor byte
				  `\0'.  With this, code detectors can
				  handle the tailing bytes more
				  accurately.  */
			       STRING_BYTES (XSTRING (string)) + 1,
			       !NILP (highest),
			       STRING_MULTIBYTE (string));
}

/* Return an intersection of lists L1 and L2.  */

static Lisp_Object
intersection (l1, l2)
     Lisp_Object l1, l2;
{
  Lisp_Object val = Fcons (Qnil, Qnil), tail;

  for (tail = val; CONSP (l1); l1 = XCDR (l1))
    {
      if (!NILP (Fmemq (XCAR (l1), l2)))
	{
	  XSETCDR (tail, Fcons (XCAR (l1), Qnil));
	  tail = XCDR (tail);
	}
    }
  return XCDR (val);
}


/*  Subroutine for Fsafe_coding_systems_region_internal.

    Return a list of coding systems that safely encode the multibyte
    text between P and PEND.  SAFE_CODINGS, if non-nil, is a list of
    possible coding systems.  If it is nil, it means that we have not
    yet found any coding systems.

    WORK_TABLE is a copy of the char-table Vchar_coding_system_table.  An
    element of WORK_TABLE is set to t once the element is looked up.

    If a non-ASCII single byte char is found, set
    *single_byte_char_found to 1.  */

static Lisp_Object
find_safe_codings (p, pend, safe_codings, work_table, single_byte_char_found)
     unsigned char *p, *pend;
     Lisp_Object safe_codings, work_table;
     int *single_byte_char_found;
{
  int c, len, idx;
  Lisp_Object val;

  while (p < pend)
    {
      c = STRING_CHAR_AND_LENGTH (p, pend - p, len);
      p += len;
      if (ASCII_BYTE_P (c))
	/* We can ignore ASCII characters here.  */
	continue;
      if (SINGLE_BYTE_CHAR_P (c))
	*single_byte_char_found = 1;
      if (NILP (safe_codings))
	continue;
      /* Check the safe coding systems for C.  */
      val = char_table_ref_and_index (work_table, c, &idx);
      if (EQ (val, Qt))
	/* This element was already checked.  Ignore it.  */
	continue;
      /* Remember that we checked this element.  */
      CHAR_TABLE_SET (work_table, make_number (idx), Qt);

      /* If there are some safe coding systems for C and we have
	 already found the other set of coding systems for the
	 different characters, get the intersection of them.  */
      if (!EQ (safe_codings, Qt) && !NILP (val))
	val = intersection (safe_codings, val);
      safe_codings = val;
    }
  return safe_codings;
}


/* Return a list of coding systems that safely encode the text between
   START and END.  If the text contains only ASCII or is unibyte,
   return t.  */

DEFUN ("find-coding-systems-region-internal",
       Ffind_coding_systems_region_internal,
       Sfind_coding_systems_region_internal, 2, 2, 0,
       doc: /* Internal use only.  */)
     (start, end)
     Lisp_Object start, end;
{
  Lisp_Object work_table, safe_codings;
  int non_ascii_p = 0;
  int single_byte_char_found = 0;
  unsigned char *p1, *p1end, *p2, *p2end, *p;

  if (STRINGP (start))
    {
      if (!STRING_MULTIBYTE (start))
	return Qt;
      p1 = XSTRING (start)->data, p1end = p1 + STRING_BYTES (XSTRING (start));
      p2 = p2end = p1end;
      if (XSTRING (start)->size != STRING_BYTES (XSTRING (start)))
	non_ascii_p = 1;
    }
  else
    {
      int from, to, stop;

      CHECK_NUMBER_COERCE_MARKER (start);
      CHECK_NUMBER_COERCE_MARKER (end);
      if (XINT (start) < BEG || XINT (end) > Z || XINT (start) > XINT (end))
	args_out_of_range (start, end);
      if (NILP (current_buffer->enable_multibyte_characters))
	return Qt;
      from = CHAR_TO_BYTE (XINT (start));
      to = CHAR_TO_BYTE (XINT (end));
      stop = from < GPT_BYTE && GPT_BYTE < to ? GPT_BYTE : to;
      p1 = BYTE_POS_ADDR (from), p1end = p1 + (stop - from);
      if (stop == to)
	p2 = p2end = p1end;
      else
	p2 = BYTE_POS_ADDR (stop), p2end = p2 + (to - stop);
      if (XINT (end) - XINT (start) != to - from)
	non_ascii_p = 1;
    }

  if (!non_ascii_p)
    {
      /* We are sure that the text contains no multibyte character.
	 Check if it contains eight-bit-graphic.  */
      p = p1;
      for (p = p1; p < p1end && ASCII_BYTE_P (*p); p++);
      if (p == p1end)
	{
	  for (p = p2; p < p2end && ASCII_BYTE_P (*p); p++);
	  if (p == p2end)
	    return Qt;
	}
    }

  /* The text contains non-ASCII characters.  */
  work_table = Fcopy_sequence (Vchar_coding_system_table);
  safe_codings = find_safe_codings (p1, p1end, Qt, work_table,
				    &single_byte_char_found);
  if (p2 < p2end)
    safe_codings = find_safe_codings (p2, p2end, safe_codings, work_table,
				      &single_byte_char_found);

  if (EQ (safe_codings, Qt))
    ; /* Nothing to be done.  */
  else if (!single_byte_char_found)
    {
      /* Append generic coding systems.  */
      Lisp_Object args[2];
      args[0] = safe_codings;
      args[1] = Fchar_table_extra_slot (Vchar_coding_system_table,
					make_number (0));
      safe_codings = Fappend (2, args);
    }
  else
    safe_codings = Fcons (Qraw_text,
			  Fcons (Qemacs_mule,
				 Fcons (Qno_conversion, safe_codings)));
  return safe_codings;
}


Lisp_Object
code_convert_region1 (start, end, coding_system, encodep)
     Lisp_Object start, end, coding_system;
     int encodep;
{
  struct coding_system coding;
  int from, to;

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);
  CHECK_SYMBOL (coding_system);

  validate_region (&start, &end);
  from = XFASTINT (start);
  to = XFASTINT (end);

  if (NILP (coding_system))
    return make_number (to - from);

  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding system: %s", XSYMBOL (coding_system)->name->data);

  coding.mode |= CODING_MODE_LAST_BLOCK;
  coding.src_multibyte = coding.dst_multibyte
    = !NILP (current_buffer->enable_multibyte_characters);
  code_convert_region (from, CHAR_TO_BYTE (from), to, CHAR_TO_BYTE (to),
		       &coding, encodep, 1);
  Vlast_coding_system_used = coding.symbol;
  return make_number (coding.produced_char);
}

DEFUN ("decode-coding-region", Fdecode_coding_region, Sdecode_coding_region,
       3, 3, "r\nzCoding system: ",
       doc: /* Decode the current region from the specified coding system.
When called from a program, takes three arguments:
START, END, and CODING-SYSTEM.  START and END are buffer positions.
This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)
It returns the length of the decoded text.  */)
     (start, end, coding_system)
     Lisp_Object start, end, coding_system;
{
  return code_convert_region1 (start, end, coding_system, 0);
}

DEFUN ("encode-coding-region", Fencode_coding_region, Sencode_coding_region,
       3, 3, "r\nzCoding system: ",
       doc: /* Encode the current region into the specified coding system.
When called from a program, takes three arguments:
START, END, and CODING-SYSTEM.  START and END are buffer positions.
This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)
It returns the length of the encoded text.  */)
     (start, end, coding_system)
     Lisp_Object start, end, coding_system;
{
  return code_convert_region1 (start, end, coding_system, 1);
}

Lisp_Object
code_convert_string1 (string, coding_system, nocopy, encodep)
     Lisp_Object string, coding_system, nocopy;
     int encodep;
{
  struct coding_system coding;

  CHECK_STRING (string);
  CHECK_SYMBOL (coding_system);

  if (NILP (coding_system))
    return (NILP (nocopy) ? Fcopy_sequence (string) : string);

  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding system: %s", XSYMBOL (coding_system)->name->data);

  coding.mode |= CODING_MODE_LAST_BLOCK;
  string = (encodep
	    ? encode_coding_string (string, &coding, !NILP (nocopy))
	    : decode_coding_string (string, &coding, !NILP (nocopy)));
  Vlast_coding_system_used = coding.symbol;

  return string;
}

DEFUN ("decode-coding-string", Fdecode_coding_string, Sdecode_coding_string,
       2, 3, 0,
       doc: /* Decode STRING which is encoded in CODING-SYSTEM, and return the result.
Optional arg NOCOPY non-nil means it is OK to return STRING itself
if the decoding operation is trivial.
This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
     (string, coding_system, nocopy)
     Lisp_Object string, coding_system, nocopy;
{
  return code_convert_string1 (string, coding_system, nocopy, 0);
}

DEFUN ("encode-coding-string", Fencode_coding_string, Sencode_coding_string,
       2, 3, 0,
       doc: /* Encode STRING to CODING-SYSTEM, and return the result.
Optional arg NOCOPY non-nil means it is OK to return STRING itself
if the encoding operation is trivial.
This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
     (string, coding_system, nocopy)
     Lisp_Object string, coding_system, nocopy;
{
  return code_convert_string1 (string, coding_system, nocopy, 1);
}

/* Encode or decode STRING according to CODING_SYSTEM.
   Do not set Vlast_coding_system_used.

   This function is called only from macros DECODE_FILE and
   ENCODE_FILE, thus we ignore character composition.  */

Lisp_Object
code_convert_string_norecord (string, coding_system, encodep)
     Lisp_Object string, coding_system;
     int encodep;
{
  struct coding_system coding;

  CHECK_STRING (string);
  CHECK_SYMBOL (coding_system);

  if (NILP (coding_system))
    return string;

  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding system: %s", XSYMBOL (coding_system)->name->data);

  coding.composing = COMPOSITION_DISABLED;
  coding.mode |= CODING_MODE_LAST_BLOCK;
  return (encodep
	  ? encode_coding_string (string, &coding, 1)
	  : decode_coding_string (string, &coding, 1));
}

DEFUN ("decode-sjis-char", Fdecode_sjis_char, Sdecode_sjis_char, 1, 1, 0,
       doc: /* Decode a Japanese character which has CODE in shift_jis encoding.
Return the corresponding character.  */)
     (code)
     Lisp_Object code;
{
  unsigned char c1, c2, s1, s2;
  Lisp_Object val;

  CHECK_NUMBER (code);
  s1 = (XFASTINT (code)) >> 8, s2 = (XFASTINT (code)) & 0xFF;
  if (s1 == 0)
    {
      if (s2 < 0x80)
	XSETFASTINT (val, s2);
      else if (s2 >= 0xA0 || s2 <= 0xDF)
	XSETFASTINT (val, MAKE_CHAR (charset_katakana_jisx0201, s2, 0));
      else
	error ("Invalid Shift JIS code: %x", XFASTINT (code));
    }
  else
    {
      if ((s1 < 0x80 || (s1 > 0x9F && s1 < 0xE0) || s1 > 0xEF)
	  || (s2 < 0x40 || s2 == 0x7F || s2 > 0xFC))
	error ("Invalid Shift JIS code: %x", XFASTINT (code));
      DECODE_SJIS (s1, s2, c1, c2);
      XSETFASTINT (val, MAKE_CHAR (charset_jisx0208, c1, c2));
    }
  return val;
}

DEFUN ("encode-sjis-char", Fencode_sjis_char, Sencode_sjis_char, 1, 1, 0,
       doc: /* Encode a Japanese character CHAR to shift_jis encoding.
Return the corresponding code in SJIS.  */)
     (ch)
     Lisp_Object ch;
{
  int charset, c1, c2, s1, s2;
  Lisp_Object val;

  CHECK_NUMBER (ch);
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  if (charset == CHARSET_ASCII)
    {
      val = ch;
    }
  else if (charset == charset_jisx0208
	   && c1 > 0x20 && c1 < 0x7F && c2 > 0x20 && c2 < 0x7F)
    {
      ENCODE_SJIS (c1, c2, s1, s2);
      XSETFASTINT (val, (s1 << 8) | s2);
    }
  else if (charset == charset_katakana_jisx0201
	   && c1 > 0x20 && c2 < 0xE0)
    {
      XSETFASTINT (val, c1 | 0x80);
    }
  else
    error ("Can't encode to shift_jis: %d", XFASTINT (ch));
  return val;
}

DEFUN ("decode-big5-char", Fdecode_big5_char, Sdecode_big5_char, 1, 1, 0,
       doc: /* Decode a Big5 character which has CODE in BIG5 coding system.
Return the corresponding character.  */)
     (code)
     Lisp_Object code;
{
  int charset;
  unsigned char b1, b2, c1, c2;
  Lisp_Object val;

  CHECK_NUMBER (code);
  b1 = (XFASTINT (code)) >> 8, b2 = (XFASTINT (code)) & 0xFF;
  if (b1 == 0)
    {
      if (b2 >= 0x80)
	error ("Invalid BIG5 code: %x", XFASTINT (code));
      val = code;
    }
  else
    {
      if ((b1 < 0xA1 || b1 > 0xFE)
	  || (b2 < 0x40 || (b2 > 0x7E && b2 < 0xA1) || b2 > 0xFE))
	error ("Invalid BIG5 code: %x", XFASTINT (code));
      DECODE_BIG5 (b1, b2, charset, c1, c2);
      XSETFASTINT (val, MAKE_CHAR (charset, c1, c2));
    }
  return val;
}

DEFUN ("encode-big5-char", Fencode_big5_char, Sencode_big5_char, 1, 1, 0,
       doc: /* Encode the Big5 character CHAR to BIG5 coding system.
Return the corresponding character code in Big5.  */)
     (ch)
     Lisp_Object ch;
{
  int charset, c1, c2, b1, b2;
  Lisp_Object val;

  CHECK_NUMBER (ch);
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  if (charset == CHARSET_ASCII)
    {
      val = ch;
    }
  else if ((charset == charset_big5_1
	    && (XFASTINT (ch) >= 0x250a1 && XFASTINT (ch) <= 0x271ec))
	   || (charset == charset_big5_2
	       && XFASTINT (ch) >= 0x290a1 && XFASTINT (ch) <= 0x2bdb2))
    {
      ENCODE_BIG5 (charset, c1, c2, b1, b2);
      XSETFASTINT (val, (b1 << 8) | b2);
    }
  else
    error ("Can't encode to Big5: %d", XFASTINT (ch));
  return val;
}

DEFUN ("set-terminal-coding-system-internal",
       Fset_terminal_coding_system_internal,
       Sset_terminal_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system), &terminal_coding);
  /* We had better not send unsafe characters to terminal.  */
  terminal_coding.flags |= CODING_FLAG_ISO_SAFE;
  /* Character composition should be disabled.  */
  terminal_coding.composing = COMPOSITION_DISABLED;
  /* Error notification should be suppressed.  */
  terminal_coding.suppress_error = 1;
  terminal_coding.src_multibyte = 1;
  terminal_coding.dst_multibyte = 0;
  return Qnil;
}

DEFUN ("set-safe-terminal-coding-system-internal",
       Fset_safe_terminal_coding_system_internal,
       Sset_safe_terminal_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system),
		       &safe_terminal_coding);
  /* Character composition should be disabled.  */
  safe_terminal_coding.composing = COMPOSITION_DISABLED;
  /* Error notification should be suppressed.  */
  terminal_coding.suppress_error = 1;
  safe_terminal_coding.src_multibyte = 1;
  safe_terminal_coding.dst_multibyte = 0;
  return Qnil;
}

DEFUN ("terminal-coding-system",
       Fterminal_coding_system, Sterminal_coding_system, 0, 0, 0,
       doc: /* Return coding system specified for terminal output.  */)
     ()
{
  return terminal_coding.symbol;
}

DEFUN ("set-keyboard-coding-system-internal",
       Fset_keyboard_coding_system_internal,
       Sset_keyboard_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system), &keyboard_coding);
  /* Character composition should be disabled.  */
  keyboard_coding.composing = COMPOSITION_DISABLED;
  return Qnil;
}

DEFUN ("keyboard-coding-system",
       Fkeyboard_coding_system, Skeyboard_coding_system, 0, 0, 0,
       doc: /* Return coding system specified for decoding keyboard input.  */)
     ()
{
  return keyboard_coding.symbol;
}


DEFUN ("find-operation-coding-system", Ffind_operation_coding_system,
       Sfind_operation_coding_system,  1, MANY, 0,
       doc: /* Choose a coding system for an operation based on the target name.
The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
DECODING-SYSTEM is the coding system to use for decoding
\(in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
for encoding (in case OPERATION does encoding).

The first argument OPERATION specifies an I/O primitive:
  For file I/O, `insert-file-contents' or `write-region'.
  For process I/O, `call-process', `call-process-region', or `start-process'.
  For network I/O, `open-network-stream'.

The remaining arguments should be the same arguments that were passed
to the primitive.  Depending on which primitive, one of those arguments
is selected as the TARGET.  For example, if OPERATION does file I/O,
whichever argument specifies the file name is TARGET.

TARGET has a meaning which depends on OPERATION:
  For file I/O, TARGET is a file name.
  For process I/O, TARGET is a process name.
  For network I/O, TARGET is a service name or a port number

This function looks up what specified for TARGET in,
`file-coding-system-alist', `process-coding-system-alist',
or `network-coding-system-alist' depending on OPERATION.
They may specify a coding system, a cons of coding systems,
or a function symbol to call.
In the last case, we call the function with one argument,
which is a list of all the arguments given to this function.

usage: (find-operation-coding-system OPERATION ARGUMENTS ...)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object operation, target_idx, target, val;
  register Lisp_Object chain;

  if (nargs < 2)
    error ("Too few arguments");
  operation = args[0];
  if (!SYMBOLP (operation)
      || !INTEGERP (target_idx = Fget (operation, Qtarget_idx)))
    error ("Invalid first argument");
  if (nargs < 1 + XINT (target_idx))
    error ("Too few arguments for operation: %s",
	   XSYMBOL (operation)->name->data);
  target = args[XINT (target_idx) + 1];
  if (!(STRINGP (target)
	|| (EQ (operation, Qopen_network_stream) && INTEGERP (target))))
    error ("Invalid argument %d", XINT (target_idx) + 1);

  chain = ((EQ (operation, Qinsert_file_contents)
	    || EQ (operation, Qwrite_region))
	   ? Vfile_coding_system_alist
	   : (EQ (operation, Qopen_network_stream)
	      ? Vnetwork_coding_system_alist
	      : Vprocess_coding_system_alist));
  if (NILP (chain))
    return Qnil;

  for (; CONSP (chain); chain = XCDR (chain))
    {
      Lisp_Object elt;
      elt = XCAR (chain);

      if (CONSP (elt)
	  && ((STRINGP (target)
	       && STRINGP (XCAR (elt))
	       && fast_string_match (XCAR (elt), target) >= 0)
	      || (INTEGERP (target) && EQ (target, XCAR (elt)))))
	{
	  val = XCDR (elt);
	  /* Here, if VAL is both a valid coding system and a valid
             function symbol, we return VAL as a coding system.  */
	  if (CONSP (val))
	    return val;
	  if (! SYMBOLP (val))
	    return Qnil;
	  if (! NILP (Fcoding_system_p (val)))
	    return Fcons (val, val);
	  if (! NILP (Ffboundp (val)))
	    {
	      val = call1 (val, Flist (nargs, args));
	      if (CONSP (val))
		return val;
	      if (SYMBOLP (val) && ! NILP (Fcoding_system_p (val)))
		return Fcons (val, val);
	    }
	  return Qnil;
	}
    }
  return Qnil;
}

DEFUN ("update-coding-systems-internal",  Fupdate_coding_systems_internal,
       Supdate_coding_systems_internal, 0, 0, 0,
       doc: /* Update internal database for ISO2022 and CCL based coding systems.
When values of any coding categories are changed, you must
call this function.  */)
     ()
{
  int i;

  for (i = CODING_CATEGORY_IDX_EMACS_MULE; i < CODING_CATEGORY_IDX_MAX; i++)
    {
      Lisp_Object val;

      val = SYMBOL_VALUE (XVECTOR (Vcoding_category_table)->contents[i]);
      if (!NILP (val))
	{
	  if (! coding_system_table[i])
	    coding_system_table[i] = ((struct coding_system *)
				      xmalloc (sizeof (struct coding_system)));
	  setup_coding_system (val, coding_system_table[i]);
	}
      else if (coding_system_table[i])
	{
	  xfree (coding_system_table[i]);
	  coding_system_table[i] = NULL;
	}
    }

  return Qnil;
}

DEFUN ("set-coding-priority-internal", Fset_coding_priority_internal,
       Sset_coding_priority_internal, 0, 0, 0,
       doc: /* Update internal database for the current value of `coding-category-list'.
This function is internal use only.  */)
     ()
{
  int i = 0, idx;
  Lisp_Object val;

  val = Vcoding_category_list;

  while (CONSP (val) && i < CODING_CATEGORY_IDX_MAX)
    {
      if (! SYMBOLP (XCAR (val)))
	break;
      idx = XFASTINT (Fget (XCAR (val), Qcoding_category_index));
      if (idx >= CODING_CATEGORY_IDX_MAX)
	break;
      coding_priorities[i++] = (1 << idx);
      val = XCDR (val);
    }
  /* If coding-category-list is valid and contains all coding
     categories, `i' should be CODING_CATEGORY_IDX_MAX now.  If not,
     the following code saves Emacs from crashing.  */
  while (i < CODING_CATEGORY_IDX_MAX)
    coding_priorities[i++] = CODING_CATEGORY_MASK_RAW_TEXT;

  return Qnil;
}

#endif /* emacs */


/*** 9. Post-amble ***/

void
init_coding_once ()
{
  int i;

  /* Emacs' internal format specific initialize routine.  */
  for (i = 0; i <= 0x20; i++)
    emacs_code_class[i] = EMACS_control_code;
  emacs_code_class[0x0A] = EMACS_linefeed_code;
  emacs_code_class[0x0D] = EMACS_carriage_return_code;
  for (i = 0x21 ; i < 0x7F; i++)
    emacs_code_class[i] = EMACS_ascii_code;
  emacs_code_class[0x7F] = EMACS_control_code;
  for (i = 0x80; i < 0xFF; i++)
    emacs_code_class[i] = EMACS_invalid_code;
  emacs_code_class[LEADING_CODE_PRIVATE_11] = EMACS_leading_code_3;
  emacs_code_class[LEADING_CODE_PRIVATE_12] = EMACS_leading_code_3;
  emacs_code_class[LEADING_CODE_PRIVATE_21] = EMACS_leading_code_4;
  emacs_code_class[LEADING_CODE_PRIVATE_22] = EMACS_leading_code_4;

  /* ISO2022 specific initialize routine.  */
  for (i = 0; i < 0x20; i++)
    iso_code_class[i] = ISO_control_0;
  for (i = 0x21; i < 0x7F; i++)
    iso_code_class[i] = ISO_graphic_plane_0;
  for (i = 0x80; i < 0xA0; i++)
    iso_code_class[i] = ISO_control_1;
  for (i = 0xA1; i < 0xFF; i++)
    iso_code_class[i] = ISO_graphic_plane_1;
  iso_code_class[0x20] = iso_code_class[0x7F] = ISO_0x20_or_0x7F;
  iso_code_class[0xA0] = iso_code_class[0xFF] = ISO_0xA0_or_0xFF;
  iso_code_class[ISO_CODE_CR] = ISO_carriage_return;
  iso_code_class[ISO_CODE_SO] = ISO_shift_out;
  iso_code_class[ISO_CODE_SI] = ISO_shift_in;
  iso_code_class[ISO_CODE_SS2_7] = ISO_single_shift_2_7;
  iso_code_class[ISO_CODE_ESC] = ISO_escape;
  iso_code_class[ISO_CODE_SS2] = ISO_single_shift_2;
  iso_code_class[ISO_CODE_SS3] = ISO_single_shift_3;
  iso_code_class[ISO_CODE_CSI] = ISO_control_sequence_introducer;

  setup_coding_system (Qnil, &keyboard_coding);
  setup_coding_system (Qnil, &terminal_coding);
  setup_coding_system (Qnil, &safe_terminal_coding);
  setup_coding_system (Qnil, &default_buffer_file_coding);

  bzero (coding_system_table, sizeof coding_system_table);

  bzero (ascii_skip_code, sizeof ascii_skip_code);
  for (i = 0; i < 128; i++)
    ascii_skip_code[i] = 1;

#if defined (MSDOS) || defined (WINDOWSNT)
  system_eol_type = CODING_EOL_CRLF;
#else
  system_eol_type = CODING_EOL_LF;
#endif

  inhibit_pre_post_conversion = 0;
}

#ifdef emacs

void
syms_of_coding ()
{
  Qtarget_idx = intern ("target-idx");
  staticpro (&Qtarget_idx);

  Qcoding_system_history = intern ("coding-system-history");
  staticpro (&Qcoding_system_history);
  Fset (Qcoding_system_history, Qnil);

  /* Target FILENAME is the first argument.  */
  Fput (Qinsert_file_contents, Qtarget_idx, make_number (0));
  /* Target FILENAME is the third argument.  */
  Fput (Qwrite_region, Qtarget_idx, make_number (2));

  Qcall_process = intern ("call-process");
  staticpro (&Qcall_process);
  /* Target PROGRAM is the first argument.  */
  Fput (Qcall_process, Qtarget_idx, make_number (0));

  Qcall_process_region = intern ("call-process-region");
  staticpro (&Qcall_process_region);
  /* Target PROGRAM is the third argument.  */
  Fput (Qcall_process_region, Qtarget_idx, make_number (2));

  Qstart_process = intern ("start-process");
  staticpro (&Qstart_process);
  /* Target PROGRAM is the third argument.  */
  Fput (Qstart_process, Qtarget_idx, make_number (2));

  Qopen_network_stream = intern ("open-network-stream");
  staticpro (&Qopen_network_stream);
  /* Target SERVICE is the fourth argument.  */
  Fput (Qopen_network_stream, Qtarget_idx, make_number (3));

  Qcoding_system = intern ("coding-system");
  staticpro (&Qcoding_system);

  Qeol_type = intern ("eol-type");
  staticpro (&Qeol_type);

  Qbuffer_file_coding_system = intern ("buffer-file-coding-system");
  staticpro (&Qbuffer_file_coding_system);

  Qpost_read_conversion = intern ("post-read-conversion");
  staticpro (&Qpost_read_conversion);

  Qpre_write_conversion = intern ("pre-write-conversion");
  staticpro (&Qpre_write_conversion);

  Qno_conversion = intern ("no-conversion");
  staticpro (&Qno_conversion);

  Qundecided = intern ("undecided");
  staticpro (&Qundecided);

  Qcoding_system_p = intern ("coding-system-p");
  staticpro (&Qcoding_system_p);

  Qcoding_system_error = intern ("coding-system-error");
  staticpro (&Qcoding_system_error);

  Fput (Qcoding_system_error, Qerror_conditions,
	Fcons (Qcoding_system_error, Fcons (Qerror, Qnil)));
  Fput (Qcoding_system_error, Qerror_message,
	build_string ("Invalid coding system"));

  Qcoding_category = intern ("coding-category");
  staticpro (&Qcoding_category);
  Qcoding_category_index = intern ("coding-category-index");
  staticpro (&Qcoding_category_index);

  Vcoding_category_table
    = Fmake_vector (make_number (CODING_CATEGORY_IDX_MAX), Qnil);
  staticpro (&Vcoding_category_table);
  {
    int i;
    for (i = 0; i < CODING_CATEGORY_IDX_MAX; i++)
      {
	XVECTOR (Vcoding_category_table)->contents[i]
	  = intern (coding_category_name[i]);
	Fput (XVECTOR (Vcoding_category_table)->contents[i],
	      Qcoding_category_index, make_number (i));
      }
  }

  Qtranslation_table = intern ("translation-table");
  staticpro (&Qtranslation_table);
  Fput (Qtranslation_table, Qchar_table_extra_slots, make_number (1));

  Qtranslation_table_id = intern ("translation-table-id");
  staticpro (&Qtranslation_table_id);

  Qtranslation_table_for_decode = intern ("translation-table-for-decode");
  staticpro (&Qtranslation_table_for_decode);

  Qtranslation_table_for_encode = intern ("translation-table-for-encode");
  staticpro (&Qtranslation_table_for_encode);

  Qsafe_chars = intern ("safe-chars");
  staticpro (&Qsafe_chars);

  Qchar_coding_system = intern ("char-coding-system");
  staticpro (&Qchar_coding_system);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");
  Fput (Qsafe_chars, Qchar_table_extra_slots, make_number (0));
  Fput (Qchar_coding_system, Qchar_table_extra_slots, make_number (2));

  Qvalid_codes = intern ("valid-codes");
  staticpro (&Qvalid_codes);

  Qemacs_mule = intern ("emacs-mule");
  staticpro (&Qemacs_mule);

  Qraw_text = intern ("raw-text");
  staticpro (&Qraw_text);

  defsubr (&Scoding_system_p);
  defsubr (&Sread_coding_system);
  defsubr (&Sread_non_nil_coding_system);
  defsubr (&Scheck_coding_system);
  defsubr (&Sdetect_coding_region);
  defsubr (&Sdetect_coding_string);
  defsubr (&Sfind_coding_systems_region_internal);
  defsubr (&Sdecode_coding_region);
  defsubr (&Sencode_coding_region);
  defsubr (&Sdecode_coding_string);
  defsubr (&Sencode_coding_string);
  defsubr (&Sdecode_sjis_char);
  defsubr (&Sencode_sjis_char);
  defsubr (&Sdecode_big5_char);
  defsubr (&Sencode_big5_char);
  defsubr (&Sset_terminal_coding_system_internal);
  defsubr (&Sset_safe_terminal_coding_system_internal);
  defsubr (&Sterminal_coding_system);
  defsubr (&Sset_keyboard_coding_system_internal);
  defsubr (&Skeyboard_coding_system);
  defsubr (&Sfind_operation_coding_system);
  defsubr (&Supdate_coding_systems_internal);
  defsubr (&Sset_coding_priority_internal);

  DEFVAR_LISP ("coding-system-list", &Vcoding_system_list,
	       doc: /* List of coding systems.

Do not alter the value of this variable manually.  This variable should be
updated by the functions `make-coding-system' and
`define-coding-system-alias'.  */);
  Vcoding_system_list = Qnil;

  DEFVAR_LISP ("coding-system-alist", &Vcoding_system_alist,
	       doc: /* Alist of coding system names.
Each element is one element list of coding system name.
This variable is given to `completing-read' as TABLE argument.

Do not alter the value of this variable manually.  This variable should be
updated by the functions `make-coding-system' and
`define-coding-system-alias'.  */);
  Vcoding_system_alist = Qnil;

  DEFVAR_LISP ("coding-category-list", &Vcoding_category_list,
	       doc: /* List of coding-categories (symbols) ordered by priority.

On detecting a coding system, Emacs tries code detection algorithms
associated with each coding-category one by one in this order.  When
one algorithm agrees with a byte sequence of source text, the coding
system bound to the corresponding coding-category is selected.  */);
  {
    int i;

    Vcoding_category_list = Qnil;
    for (i = CODING_CATEGORY_IDX_MAX - 1; i >= 0; i--)
      Vcoding_category_list
	= Fcons (XVECTOR (Vcoding_category_table)->contents[i],
		 Vcoding_category_list);
  }

  DEFVAR_LISP ("coding-system-for-read", &Vcoding_system_for_read,
	       doc: /* Specify the coding system for read operations.
It is useful to bind this variable with `let', but do not set it globally.
If the value is a coding system, it is used for decoding on read operation.
If not, an appropriate element is used from one of the coding system alists:
There are three such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.  */);
  Vcoding_system_for_read = Qnil;

  DEFVAR_LISP ("coding-system-for-write", &Vcoding_system_for_write,
	       doc: /* Specify the coding system for write operations.
Programs bind this variable with `let', but you should not set it globally.
If the value is a coding system, it is used for encoding of output,
when writing it to a file and when sending it to a file or subprocess.

If this does not specify a coding system, an appropriate element
is used from one of the coding system alists:
There are three such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.
For output to files, if the above procedure does not specify a coding system,
the value of `buffer-file-coding-system' is used.  */);
  Vcoding_system_for_write = Qnil;

  DEFVAR_LISP ("last-coding-system-used", &Vlast_coding_system_used,
	       doc: /* Coding system used in the latest file or process I/O.  */);
  Vlast_coding_system_used = Qnil;

  DEFVAR_BOOL ("inhibit-eol-conversion", &inhibit_eol_conversion,
	       doc: /* *Non-nil means always inhibit code conversion of end-of-line format.
See info node `Coding Systems' and info node `Text and Binary' concerning
such conversion.  */);
  inhibit_eol_conversion = 0;

  DEFVAR_BOOL ("inherit-process-coding-system", &inherit_process_coding_system,
	       doc: /* Non-nil means process buffer inherits coding system of process output.
Bind it to t if the process output is to be treated as if it were a file
read from some filesystem.  */);
  inherit_process_coding_system = 0;

  DEFVAR_LISP ("file-coding-system-alist", &Vfile_coding_system_alist,
	       doc: /* Alist to decide a coding system to use for a file I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a file name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding and encoding
the file contents.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.  The function gets
the arguments with which `find-operation-coding-system' was called.

See also the function `find-operation-coding-system'
and the variable `auto-coding-alist'.  */);
  Vfile_coding_system_alist = Qnil;

  DEFVAR_LISP ("process-coding-system-alist", &Vprocess_coding_system_alist,
    doc: /* Alist to decide a coding system to use for a process I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a program name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the program and encoding what sent to the program.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.  */);
  Vprocess_coding_system_alist = Qnil;

  DEFVAR_LISP ("network-coding-system-alist", &Vnetwork_coding_system_alist,
    doc: /* Alist to decide a coding system to use for a network I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a network service name
or is a port number to connect to,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the network stream and encoding what sent to the network stream.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.  */);
  Vnetwork_coding_system_alist = Qnil;

  DEFVAR_LISP ("locale-coding-system", &Vlocale_coding_system,
	       doc: /* Coding system to use with system messages.
Also used for decoding keyboard input on X Window system.  */);
  Vlocale_coding_system = Qnil;

  /* The eol mnemonics are reset in startup.el system-dependently.  */
  DEFVAR_LISP ("eol-mnemonic-unix", &eol_mnemonic_unix,
	       doc: /* *String displayed in mode line for UNIX-like (LF) end-of-line format.  */);
  eol_mnemonic_unix = build_string (":");

  DEFVAR_LISP ("eol-mnemonic-dos", &eol_mnemonic_dos,
	       doc: /* *String displayed in mode line for DOS-like (CRLF) end-of-line format.  */);
  eol_mnemonic_dos = build_string ("\\");

  DEFVAR_LISP ("eol-mnemonic-mac", &eol_mnemonic_mac,
	       doc: /* *String displayed in mode line for MAC-like (CR) end-of-line format.  */);
  eol_mnemonic_mac = build_string ("/");

  DEFVAR_LISP ("eol-mnemonic-undecided", &eol_mnemonic_undecided,
	       doc: /* *String displayed in mode line when end-of-line format is not yet determined.  */);
  eol_mnemonic_undecided = build_string (":");

  DEFVAR_LISP ("enable-character-translation", &Venable_character_translation,
	       doc: /* *Non-nil enables character translation while encoding and decoding.  */);
  Venable_character_translation = Qt;

  DEFVAR_LISP ("standard-translation-table-for-decode",
	       &Vstandard_translation_table_for_decode,
	       doc: /* Table for translating characters while decoding.  */);
  Vstandard_translation_table_for_decode = Qnil;

  DEFVAR_LISP ("standard-translation-table-for-encode",
	       &Vstandard_translation_table_for_encode,
	       doc: /* Table for translating characters while encoding.  */);
  Vstandard_translation_table_for_encode = Qnil;

  DEFVAR_LISP ("charset-revision-table", &Vcharset_revision_alist,
	       doc: /* Alist of charsets vs revision numbers.
While encoding, if a charset (car part of an element) is found,
designate it with the escape sequence identifying revision (cdr part of the element).  */);
  Vcharset_revision_alist = Qnil;

  DEFVAR_LISP ("default-process-coding-system",
	       &Vdefault_process_coding_system,
	       doc: /* Cons of coding systems used for process I/O by default.
The car part is used for decoding a process output,
the cdr part is used for encoding a text to be sent to a process.  */);
  Vdefault_process_coding_system = Qnil;

  DEFVAR_LISP ("latin-extra-code-table", &Vlatin_extra_code_table,
	       doc: /* Table of extra Latin codes in the range 128..159 (inclusive).
This is a vector of length 256.
If Nth element is non-nil, the existence of code N in a file
\(or output of subprocess) doesn't prevent it to be detected as
a coding system of ISO 2022 variant which has a flag
`accept-latin-extra-code' t (e.g. iso-latin-1) on reading a file
or reading output of a subprocess.
Only 128th through 159th elements has a meaning.  */);
  Vlatin_extra_code_table = Fmake_vector (make_number (256), Qnil);

  DEFVAR_LISP ("select-safe-coding-system-function",
	       &Vselect_safe_coding_system_function,
	       doc: /* Function to call to select safe coding system for encoding a text.

If set, this function is called to force a user to select a proper
coding system which can encode the text in the case that a default
coding system used in each operation can't encode the text.

The default value is `select-safe-coding-system' (which see).  */);
  Vselect_safe_coding_system_function = Qnil;

  DEFVAR_LISP ("char-coding-system-table", &Vchar_coding_system_table,
	       doc: /* Char-table containing safe coding systems of each characters.
Each element doesn't include such generic coding systems that can
encode any characters.   They are in the first extra slot.  */);
  Vchar_coding_system_table = Fmake_char_table (Qchar_coding_system, Qnil);

  DEFVAR_BOOL ("inhibit-iso-escape-detection",
	       &inhibit_iso_escape_detection,
	       doc: /* If non-nil, Emacs ignores ISO2022's escape sequence on code detection.

By default, on reading a file, Emacs tries to detect how the text is
encoded.  This code detection is sensitive to escape sequences.  If
the sequence is valid as ISO2022, the code is determined as one of
the ISO2022 encodings, and the file is decoded by the corresponding
coding system (e.g. `iso-2022-7bit').

However, there may be a case that you want to read escape sequences in
a file as is.  In such a case, you can set this variable to non-nil.
Then, as the code detection ignores any escape sequences, no file is
detected as encoded in some ISO2022 encoding.  The result is that all
escape sequences become visible in a buffer.

The default value is nil, and it is strongly recommended not to change
it.  That is because many Emacs Lisp source files that contain
non-ASCII characters are encoded by the coding system `iso-2022-7bit'
in Emacs's distribution, and they won't be decoded correctly on
reading if you suppress escape sequence detection.

The other way to read escape sequences in a file without decoding is
to explicitly specify some coding system that doesn't use ISO2022's
escape sequence (e.g `latin-1') on reading by \\[universal-coding-system-argument].  */);
  inhibit_iso_escape_detection = 0;
}

char *
emacs_strerror (error_number)
     int error_number;
{
  char *str;

  synchronize_system_messages_locale ();
  str = strerror (error_number);

  if (! NILP (Vlocale_coding_system))
    {
      Lisp_Object dec = code_convert_string_norecord (build_string (str),
						      Vlocale_coding_system,
						      0);
      str = (char *) XSTRING (dec)->data;
    }

  return str;
}

#endif /* emacs */

