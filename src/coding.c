/* Coding system handler (conversion, detection, and etc).
   Ver.1.0.
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

/*** TABLE OF CONTENTS ***

  1. Preamble
  2. Emacs' internal format handlers
  3. ISO2022 handlers
  4. Shift-JIS and BIG5 handlers
  5. End-of-line handlers
  6. C library functions
  7. Emacs Lisp library functions
  8. Post-amble

*/

/*** GENERAL NOTE on CODING SYSTEM ***

  Coding system is an encoding mechanism of one or more character
  sets.  Here's a list of coding systems which Emacs can handle.  When
  we say "decode", it means converting some other coding system to
  Emacs' internal format, and when we say "encode", it means
  converting Emacs' internal format to some other coding system.

  0. Emacs' internal format

  Emacs itself holds a multi-lingual character in a buffer and a string
  in a special format.  Details are described in the section 2.

  1. ISO2022

  The most famous coding system for multiple character sets.  X's
  Compound Text, various EUCs (Extended Unix Code), and such coding
  systems used in Internet communication as ISO-2022-JP are all
  variants of ISO2022.  Details are described in the section 3.

  2. SJIS (or Shift-JIS or MS-Kanji-Code)
   
  A coding system to encode character sets: ASCII, JISX0201, and
  JISX0208.  Widely used for PC's in Japan.  Details are described in
  the section 4.

  3. BIG5

  A coding system to encode character sets: ASCII and Big5.  Widely
  used by Chinese (mainly in Taiwan and Hong Kong).  Details are
  described in the section 4.  In this file, when written as "BIG5"
  (all uppercase), it means the coding system, and when written as
  "Big5" (capitalized), it means the character set.

  4. Else

  If a user want to read/write a text encoded in a coding system not
  listed above, he can supply a decoder and an encoder for it in CCL
  (Code Conversion Language) programs.  Emacs executes the CCL program
  while reading/writing.

  Emacs represent a coding-system by a Lisp symbol that has a property
  `coding-system'.  But, before actually using the coding-system, the
  information about it is set in a structure of type `struct
  coding_system' for rapid processing.  See the section 6 for more
  detail.

*/

/*** GENERAL NOTES on END-OF-LINE FORMAT ***

  How end-of-line of a text is encoded depends on a system.  For
  instance, Unix's format is just one byte of `line-feed' code,
  whereas DOS's format is two bytes sequence of `carriage-return' and
  `line-feed' codes.  MacOS's format is one byte of `carriage-return'.

  Since how characters in a text is encoded and how end-of-line is
  encoded is independent, any coding system described above can take
  any format of end-of-line.  So, Emacs has information of format of
  end-of-line in each coding-system.  See the section 6 for more
  detail.

*/

/*** GENERAL NOTES on `detect_coding_XXX ()' functions ***

  These functions check if a text between SRC and SRC_END is encoded
  in the coding system category XXX.  Each returns an integer value in
  which appropriate flag bits for the category XXX is set.  The flag
  bits are defined in macros CODING_CATEGORY_MASK_XXX.  Below is the
  template of these functions.  */
#if 0
int
detect_coding_internal (src, src_end)
     unsigned char *src, *src_end;
{
  ...
}
#endif

/*** GENERAL NOTES on `decode_coding_XXX ()' functions ***

  These functions decode SRC_BYTES length text at SOURCE encoded in
  CODING to Emacs' internal format.  The resulting text goes to a
  place pointed by DESTINATION, the length of which should not exceed
  DST_BYTES.  The bytes actually processed is returned as *CONSUMED.
  The return value is the length of the decoded text.  Below is a
  template of these functions.  */
#if 0
decode_coding_XXX (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  ...
}
#endif

/*** GENERAL NOTES on `encode_coding_XXX ()' functions ***

  These functions encode SRC_BYTES length text at SOURCE of Emacs
  internal format to CODING.  The resulting text goes to a place
  pointed by DESTINATION, the length of which should not exceed
  DST_BYTES.  The bytes actually processed is returned as *CONSUMED.
  The return value is the length of the encoded text.  Below is a
  template of these functions.  */
#if 0
encode_coding_XXX (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  ...
}
#endif

/*** COMMONLY USED MACROS ***/

/* The following three macros ONE_MORE_BYTE, TWO_MORE_BYTES, and
   THREE_MORE_BYTES safely get one, two, and three bytes from the
   source text respectively.  If there are not enough bytes in the
   source, they jump to `label_end_of_loop'.  The caller should set
   variables `src' and `src_end' to appropriate areas in advance.  */

#define ONE_MORE_BYTE(c1)   	\
  do {			     	\
    if (src < src_end)	     	\
      c1 = *src++;	     	\
    else		     	\
      goto label_end_of_loop;	\
  } while (0)

#define TWO_MORE_BYTES(c1, c2) 	\
  do {			       	\
    if (src + 1 < src_end)     	\
      c1 = *src++, c2 = *src++;	\
    else		       	\
      goto label_end_of_loop;  	\
  } while (0)

#define THREE_MORE_BYTES(c1, c2, c3)	    	\
  do {					    	\
    if (src + 2 < src_end)		    	\
      c1 = *src++, c2 = *src++, c3 = *src++;	\
    else				    	\
      goto label_end_of_loop;		    	\
  } while (0)

/* The following three macros DECODE_CHARACTER_ASCII,
   DECODE_CHARACTER_DIMENSION1, and DECODE_CHARACTER_DIMENSION2 put
   the multi-byte form of a character of each class at the place
   pointed by `dst'.  The caller should set the variable `dst' to
   point to an appropriate area and the variable `coding' to point to
   the coding-system of the currently decoding text in advance.  */

/* Decode one ASCII character C.  */

#define DECODE_CHARACTER_ASCII(c)				\
  do {								\
    if (COMPOSING_P (coding->composing))			\
      *dst++ = 0xA0, *dst++ = (c) | 0x80;			\
    else							\
      *dst++ = (c);						\
  } while (0)

/* Decode one DIMENSION1 character of which charset is CHARSET and
   position-code is C.  */

#define DECODE_CHARACTER_DIMENSION1(charset, c)				\
  do {									\
    unsigned char leading_code = CHARSET_LEADING_CODE_BASE (charset);	\
    if (COMPOSING_P (coding->composing))				\
      *dst++ = leading_code + 0x20;					\
    else								\
      *dst++ = leading_code;						\
    if (leading_code = CHARSET_LEADING_CODE_EXT (charset))		\
      *dst++ = leading_code;						\
    *dst++ = (c) | 0x80;						\
  } while (0)

/* Decode one DIMENSION2 character of which charset is CHARSET and
   position-codes are C1 and C2.  */

#define DECODE_CHARACTER_DIMENSION2(charset, c1, c2)	\
  do {							\
    DECODE_CHARACTER_DIMENSION1 (charset, c1);		\
    *dst++ = (c2) | 0x80;				\
  } while (0)


/*** 1. Preamble ***/

#include <stdio.h>

#ifdef emacs

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "ccl.h"
#include "coding.h"
#include "window.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* not emacs */

Lisp_Object Qcoding_system, Qeol_type;
Lisp_Object Qbuffer_file_coding_system;
Lisp_Object Qpost_read_conversion, Qpre_write_conversion;

extern Lisp_Object Qinsert_file_contents, Qwrite_region;
Lisp_Object Qcall_process, Qcall_process_region, Qprocess_argument;
Lisp_Object Qstart_process, Qopen_network_stream;
Lisp_Object Qtarget_idx;

/* Mnemonic character of each format of end-of-line.  */
int eol_mnemonic_unix, eol_mnemonic_dos, eol_mnemonic_mac;
/* Mnemonic character to indicate format of end-of-line is not yet
   decided.  */
int eol_mnemonic_undecided;

#ifdef emacs

Lisp_Object Qcoding_system_vector, Qcoding_system_p, Qcoding_system_error;

/* Coding-systems are handed between Emacs Lisp programs and C internal
   routines by the following three variables.  */
/* Coding-system for reading files and receiving data from process.  */
Lisp_Object Vcoding_system_for_read;
/* Coding-system for writing files and sending data to process.  */
Lisp_Object Vcoding_system_for_write;
/* Coding-system actually used in the latest I/O.  */
Lisp_Object Vlast_coding_system_used;

/* Coding-system of what terminal accept for displaying.  */
struct coding_system terminal_coding;

/* Coding-system of what is sent from terminal keyboard.  */
struct coding_system keyboard_coding;

Lisp_Object Vcoding_system_alist;

#endif /* emacs */

Lisp_Object Qcoding_category_index;

/* List of symbols `coding-category-xxx' ordered by priority.  */
Lisp_Object Vcoding_category_list;

/* Table of coding-systems currently assigned to each coding-category.  */
Lisp_Object coding_category_table[CODING_CATEGORY_IDX_MAX];

/* Table of names of symbol for each coding-category.  */
char *coding_category_name[CODING_CATEGORY_IDX_MAX] = {
  "coding-category-internal",
  "coding-category-sjis",
  "coding-category-iso-7",
  "coding-category-iso-8-1",
  "coding-category-iso-8-2",
  "coding-category-iso-else",
  "coding-category-big5",
  "coding-category-binary"
};

/* Alist of charsets vs the alternate charsets.  */
Lisp_Object Valternate_charset_table;

/* Alist of charsets vs revision number.  */
Lisp_Object Vcharset_revision_alist;


/*** 2. Emacs internal format handlers ***/

/* Emacs' internal format for encoding multiple character sets is a
   kind of multi-byte encoding, i.e. encoding a character by a sequence
   of one-byte codes of variable length.  ASCII characters and control
   characters (e.g. `tab', `newline') are represented by one-byte as
   is.  It takes the range 0x00 through 0x7F.  The other characters
   are represented by a sequence of `base leading-code', optional
   `extended leading-code', and one or two `position-code's.  Length
   of the sequence is decided by the base leading-code.  Leading-code
   takes the range 0x80 through 0x9F, whereas extended leading-code
   and position-code take the range 0xA0 through 0xFF.  See the
   document of `charset.h' for more detail about leading-code and
   position-code.

   There's one exception in this rule.  Special leading-code
   `leading-code-composition' denotes that the following several
   characters should be composed into one character.  Leading-codes of
   components (except for ASCII) are added 0x20.  An ASCII character
   component is represented by a 2-byte sequence of `0xA0' and
   `ASCII-code + 0x80'.  See also the document in `charset.h' for the
   detail of composite character.  Hence, we can summarize the code
   range as follows:

   --- CODE RANGE of Emacs' internal format ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   ELSE (1st byte)	0x80 .. 0x9F
	(rest bytes)	0xA0 .. 0xFF
   ---------------------------------------------

  */

enum emacs_code_class_type emacs_code_class[256];

/* Go to the next statement only if *SRC is accessible and the code is
   greater than 0xA0.  */
#define CHECK_CODE_RANGE_A0_FF 	\
  do {			       	\
    if (src >= src_end)	       	\
      goto label_end_of_switch;	\
    else if (*src++ < 0xA0)    	\
      return 0;		       	\
  } while (0)

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in Emacs' internal format.  If it is,
   return CODING_CATEGORY_MASK_INTERNAL, else return 0.  */

int
detect_coding_internal (src, src_end)
     unsigned char *src, *src_end;
{
  unsigned char c;
  int composing = 0;

  while (src < src_end)
    {
      c = *src++;

      if (composing)
	{
	  if (c < 0xA0)
	    composing = 0;
	  else
	    c -= 0x20;
	}

      switch (emacs_code_class[c])
	{
	case EMACS_ascii_code:
	case EMACS_linefeed_code:
	  break;

	case EMACS_control_code:
	  if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	    return 0;
	  break;

	case EMACS_invalid_code:
	  return 0;

	case EMACS_leading_code_composition: /* c == 0x80 */
	  if (composing)
	    CHECK_CODE_RANGE_A0_FF;
	  else
	    composing = 1;
	  break;

	case EMACS_leading_code_4:
	  CHECK_CODE_RANGE_A0_FF;
	  /* fall down to check it two more times ...  */

	case EMACS_leading_code_3:
	  CHECK_CODE_RANGE_A0_FF;
	  /* fall down to check it one more time ...  */

	case EMACS_leading_code_2:
	  CHECK_CODE_RANGE_A0_FF;
	  break;

	default:
	label_end_of_switch:
	  break;
	}
    }
  return CODING_CATEGORY_MASK_INTERNAL;
}


/*** 3. ISO2022 handlers ***/

/* The following note describes the coding system ISO2022 briefly.
   Since the intension of this note is to help understanding of the
   programs in this file, some parts are NOT ACCURATE or OVERLY
   SIMPLIFIED.  For the thorough understanding, please refer to the
   original document of ISO2022.

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environment.  If one choose 7-bite environment,
   all text is encoded by codes of less than 128.  This may make the
   encoded text a little bit longer, but the text get more stability
   to pass through several gateways (some of them split MSB off).

   There are two kind of character set: control character set and
   graphic character set.  The former contains control characters such
   as `newline' and `escape' to provide control functions (control
   functions are provided also by escape sequence).  The latter
   contains graphic characters such as ' A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, DIMENSION1_CHARS94, DIMENSION1_CHARS96,
   DIMENSION2_CHARS94, DIMENSION2_CHARS96 according to the number of
   bytes (DIMENSION) and the number of characters in one dimension
   (CHARS) of the set.  In addition, each character set is assigned an
   identification tag (called "final character" and denoted as <F>
   here after) which is unique in each class.  <F> of each character
   set is decided by ECMA(*) when it is registered in ISO.  Code range
   of <F> is 0x30..0x7F (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character set [NAME(<F>)]:
	o DIMENSION1_CHARS94 -- ASCII('B'), right-half-of-JISX0201('I'), ...
	o DIMENSION1_CHARS96 -- right-half-of-ISO8859-1('A'), ...
	o DIMENSION2_CHARS94 -- GB2312('A'), JISX0208('B'), ...
	o DIMENSION2_CHARS96 -- none for the moment

   A code area (1byte=8bits) is divided into 4 areas, C0, GL, C1, and GR.
	C0 [0x00..0x1F] -- control character plane 0
	GL [0x20..0x7F] -- graphic character plane 0
	C1 [0x80..0x9F] -- control character plane 1
	GR [0xA0..0xFF] -- graphic character plane 1

   A control character set is directly designated and invoked to C0 or
   C1 by an escape sequence.  The most common case is that ISO646's
   control character set is designated/invoked to C0 and ISO6429's
   control character set is designated/invoked to C1, and usually
   these designations/invocations are omitted in a coded text.  With
   7-bit environment, only C0 can be used, and a control character for
   C1 is encoded by an appropriate escape sequence to fit in the
   environment.  All control characters for C1 are defined the
   corresponding escape sequences.

   A graphic character set is at first designated to one of four
   graphic registers (G0 through G3), then these graphic registers are
   invoked to GL or GR.  These designations and invocations can be
   done independently.  The most common case is that G0 is invoked to
   GL, G1 is invoked to GR, and ASCII is designated to G0, and usually
   these invocations and designations are omitted in a coded text.
   With 7-bit environment, only GL can be used.

   When a graphic character set of CHARS94 is invoked to GL, code 0x20
   and 0x7F of GL area work as control characters SPACE and DEL
   respectively, and code 0xA0 and 0xFF of GR area should not be used.

   There are two ways of invocation: locking-shift and single-shift.
   With locking-shift, the invocation lasts until the next different
   invocation, whereas with single-shift, the invocation works only
   for the following character and doesn't affect locking-shift.
   Invocations are done by the following control characters or escape
   sequences.

   ----------------------------------------------------------------------
   function		control char	escape sequence	description
   ----------------------------------------------------------------------
   SI  (shift-in)		0x0F	none		invoke G0 to GL
   SI  (shift-out)		0x0E	none		invoke G1 to GL
   LS2 (locking-shift-2)	none	ESC 'n'		invoke G2 into GL
   LS3 (locking-shift-3)	none	ESC 'o'		invoke G3 into GL
   SS2 (single-shift-2)		0x8E	ESC 'N'		invoke G2 into GL
   SS3 (single-shift-3)		0x8F	ESC 'O'		invoke G3 into GL
   ----------------------------------------------------------------------
   The first four are for locking-shift.  Control characters for these
   functions are defined by macros ISO_CODE_XXX in `coding.h'.

   Designations are done by the following escape sequences.
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
   of dimension 1, chars 94, and final character <F>, and etc.

   Note (*): Although these designations are not allowed in ISO2022,
   Emacs accepts them on decoding, and produces them on encoding
   CHARS96 character set in a coding system which is characterized as
   7-bit environment, non-locking-shift, and non-single-shift.

   Note (**): If <F> is '@', 'A', or 'B', the intermediate character
   '(' can be omitted.  We call this as "short-form" here after.

   Now you may notice that there are a lot of ways for encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X's inter client
   communication, ISO-2022-JP (used in Japanese Internet), ISO-2022-KR
   (used in Korean Internet), EUC (Extended UNIX Code, used in Asian
   localized platforms), and all of these are variants of ISO2022.

   In addition to the above, Emacs handles two more kinds of escape
   sequences: ISO6429's direction specification and Emacs' private
   sequence for specifying character composition.

   ISO6429's direction specification takes the following format:
	o CSI ']'      -- end of the current direction
	o CSI '0' ']'  -- end of the current direction
	o CSI '1' ']'  -- start of left-to-right text
	o CSI '2' ']'  -- start of right-to-left text
   The control character CSI (0x9B: control sequence introducer) is
   abbreviated to the escape sequence ESC '[' in 7-bit environment.
   
   Character composition specification takes the following format:
	o ESC '0' -- start character composition
	o ESC '1' -- end character composition
   Since these are not standard escape sequences of any ISO, the use
   of them for these meaning is restricted to Emacs only.  */

enum iso_code_class_type iso_code_class[256];

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in ISO2022.  If it is, returns an
   integer in which appropriate flag bits any of:
	CODING_CATEGORY_MASK_ISO_7
	CODING_CATEGORY_MASK_ISO_8_1
	CODING_CATEGORY_MASK_ISO_8_2
	CODING_CATEGORY_MASK_ISO_ELSE
   are set.  If a code which should never appear in ISO2022 is found,
   returns 0.  */

int
detect_coding_iso2022 (src, src_end)
     unsigned char *src, *src_end;
{
  unsigned char c, g1 = 0;
  int mask = (CODING_CATEGORY_MASK_ISO_7
	      | CODING_CATEGORY_MASK_ISO_8_1
	      | CODING_CATEGORY_MASK_ISO_8_2);
  /* We may look ahead at most 4 bytes.  */
  unsigned char *adjusted_src_end = src_end - 4;
  int i;

  while (src < src_end)
    {
      c = *src++;
      switch (c)
	{
	case ISO_CODE_ESC:
	  if (src >= src_end)
	    break;
	  c = *src++;
	  if (src + 2 >= src_end
	      && ((c >= '(' && c <= '/')
		  || c == '$' && ((*src >= '(' && *src <= '/')
				  || (*src >= '@' && *src <= 'B'))))
	    {
	      /* Valid designation sequence.  */
	      if (c == ')' || (c == '$' && *src == ')'))
		g1 = 1;
	      src++;
	      break;
	    }
	  else if (c == 'N' || c == 'O' || c == 'n' || c == 'o')
	    return CODING_CATEGORY_MASK_ISO_ELSE;
	  break;

	case ISO_CODE_SO:
	  if (g1)
	    return CODING_CATEGORY_MASK_ISO_ELSE;
	  break;
	  
	case ISO_CODE_CSI:
	case ISO_CODE_SS2:
	case ISO_CODE_SS3:
	  mask &= ~CODING_CATEGORY_MASK_ISO_7;
	  break;

	default:
	  if (c < 0x80)
	    break;
	  else if (c < 0xA0)
	    return 0;
	  else
	    {
	      int count = 1;

	      mask &= ~CODING_CATEGORY_MASK_ISO_7;
	      while (src < src_end && *src >= 0xA0)
		count++, src++;
	      if (count & 1 && src < src_end)
		mask &= ~CODING_CATEGORY_MASK_ISO_8_2;
	    }
	  break;
	}
    }

  return mask;
}

/* Decode a character of which charset is CHARSET and the 1st position
   code is C1.  If dimension of CHARSET 2, the 2nd position code is
   fetched from SRC and set to C2.  If CHARSET is negative, it means
   that we are decoding ill formed text, and what we can do is just to
   read C1 as is.  */

#define DECODE_ISO_CHARACTER(charset, c1)			\
  do {								\
    if ((charset) >= 0 && CHARSET_DIMENSION (charset) == 2)	\
      ONE_MORE_BYTE (c2);					\
    if (COMPOSING_HEAD_P (coding->composing))			\
      {								\
	*dst++ = LEADING_CODE_COMPOSITION;			\
	if (COMPOSING_WITH_RULE_P (coding->composing))		\
	  /* To tell composition rules are embeded.  */		\
	  *dst++ = 0xFF;					\
	coding->composing += 2;					\
      }								\
    if ((charset) < 0)						\
      *dst++ = c1;						\
    else if ((charset) == CHARSET_ASCII)			\
      DECODE_CHARACTER_ASCII (c1);				\
    else if (CHARSET_DIMENSION (charset) == 1)			\
      DECODE_CHARACTER_DIMENSION1 (charset, c1);		\
    else							\
      DECODE_CHARACTER_DIMENSION2 (charset, c1, c2);		\
    if (COMPOSING_WITH_RULE_P (coding->composing))		\
      /* To tell a composition rule follows.  */		\
      coding->composing = COMPOSING_WITH_RULE_RULE;		\
  } while (0)

/* Set designation state into CODING.  */
#define DECODE_DESIGNATION(reg, dimension, chars, final_char)		\
  do {							      		\
    int charset = ISO_CHARSET_TABLE (dimension, chars, final_char);	\
    Lisp_Object temp							\
      = Fassq (CHARSET_SYMBOL (charset), Valternate_charset_table);	\
    if (! NILP (temp))							\
      charset = get_charset_id (XCONS (temp)->cdr);			\
    if (charset >= 0)					      		\
      {					      				\
        if (coding->direction == 1					\
	    && CHARSET_REVERSE_CHARSET (charset) >= 0)      		\
          charset = CHARSET_REVERSE_CHARSET (charset);      		\
        CODING_SPEC_ISO_DESIGNATION (coding, reg) = charset;		\
      }						      			\
  } while (0)

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

int
decode_coding_iso2022 (coding, source, destination,
		       src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Since the maximum bytes produced by each loop is 7, we subtract 6
     from DST_END to assure that overflow checking is necessary only
     at the head of loop.  */
  unsigned char *adjusted_dst_end = dst_end - 6;
  int charset;
  /* Charsets invoked to graphic plane 0 and 1 respectively.  */
  int charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
  int charset1 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 1);

  while (src < src_end && dst < adjusted_dst_end)
    {
      /* SRC_BASE remembers the start position in source in each loop.
	 The loop will be exited when there's not enough source text
	 to analyze long escape sequence or 2-byte code (within macros
	 ONE_MORE_BYTE or TWO_MORE_BYTES).  In that case, SRC is reset
	 to SRC_BASE before exiting.  */
      unsigned char *src_base = src;
      unsigned char c1 = *src++, c2, cmprule;

      switch (iso_code_class [c1])
	{
	case ISO_0x20_or_0x7F:
	  if (!coding->composing
	      && (charset0 < 0 || CHARSET_CHARS (charset0) == 94))
	    {
	      /* This is SPACE or DEL.  */
	      *dst++ = c1;
	      break;
	    }
	  /* This is a graphic character, we fall down ...  */

	case ISO_graphic_plane_0:
	  if (coding->composing == COMPOSING_WITH_RULE_RULE)
	    {
	      /* This is a composition rule.  */
	      *dst++ = c1 | 0x80;
	      coding->composing = COMPOSING_WITH_RULE_TAIL;
	    }
	  else
	    DECODE_ISO_CHARACTER (charset0, c1);
	  break;

	case ISO_0xA0_or_0xFF:
	  if (charset1 < 0 || CHARSET_CHARS (charset1) == 94)
	    {
	      /* Invalid code.  */
	      *dst++ = c1;
	      break;
	    }
	  /* This is a graphic character, we fall down ... */

	case ISO_graphic_plane_1:
	  DECODE_ISO_CHARACTER (charset1, c1);
	  break;

	case ISO_control_code:
	  /* All ISO2022 control characters in this class have the
             same representation in Emacs internal format.  */
	  *dst++ = c1;
	  break;

	case ISO_carriage_return:
	  if (coding->eol_type == CODING_EOL_CR)
	    {
	      *dst++ = '\n';
	    }
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    {
	      ONE_MORE_BYTE (c1);
	      if (c1 == ISO_CODE_LF)
		*dst++ = '\n';
	      else
		{
		  src--;
		  *dst++ = c1;
		}
	    }
	  else
	    {
	      *dst++ = c1;
	    }
	  break;

	case ISO_shift_out:
	  if (CODING_SPEC_ISO_DESIGNATION (coding, 1) < 0)
	    goto label_invalid_escape_sequence;
	  CODING_SPEC_ISO_INVOCATION (coding, 0) = 1;
	  charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	  break;

	case ISO_shift_in:
	  CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;
	  charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	  break;

	case ISO_single_shift_2_7:
	case ISO_single_shift_2:
	  /* SS2 is handled as an escape sequence of ESC 'N' */
	  c1 = 'N';
	  goto label_escape_sequence;

	case ISO_single_shift_3:
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
		goto label_invalid_escape_sequence;
	      ONE_MORE_BYTE (c1);
	      if (c1 != ISO_CODE_ESC)
		goto label_invalid_escape_sequence;
	      ONE_MORE_BYTE (c1);
	      goto label_escape_sequence;

	    case '$':		/* designation of 2-byte character set */
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
		goto label_invalid_escape_sequence;
	      break;

	    case 'n':		/* invocation of locking-shift-2 */
	      if (CODING_SPEC_ISO_DESIGNATION (coding, 2) < 0)
		goto label_invalid_escape_sequence;
	      CODING_SPEC_ISO_INVOCATION (coding, 0) = 2;
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      break;

	    case 'o':		/* invocation of locking-shift-3 */
	      if (CODING_SPEC_ISO_DESIGNATION (coding, 3) < 0)
		goto label_invalid_escape_sequence;
	      CODING_SPEC_ISO_INVOCATION (coding, 0) = 3;
	      charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	      break;

	    case 'N':		/* invocation of single-shift-2 */
	      if (CODING_SPEC_ISO_DESIGNATION (coding, 2) < 0)
		goto label_invalid_escape_sequence;
	      ONE_MORE_BYTE (c1);
	      charset = CODING_SPEC_ISO_DESIGNATION (coding, 2);
	      DECODE_ISO_CHARACTER (charset, c1);
	      break;

	    case 'O':		/* invocation of single-shift-3 */
	      if (CODING_SPEC_ISO_DESIGNATION (coding, 3) < 0)
		goto label_invalid_escape_sequence;
	      ONE_MORE_BYTE (c1);
	      charset = CODING_SPEC_ISO_DESIGNATION (coding, 3);
	      DECODE_ISO_CHARACTER (charset, c1);
	      break;

	    case '0':		/* start composing without embeded rules */
	      coding->composing = COMPOSING_NO_RULE_HEAD;
	      break;

	    case '1':		/* end composing */
	      coding->composing = COMPOSING_NO;
	      break;

	    case '2':		/* start composing with embeded rules */
	      coding->composing = COMPOSING_WITH_RULE_HEAD;
	      break;

	    case '[':		/* specification of direction */
	      /* For the moment, nested direction is not supported.
		 So, the value of `coding->direction' is 0 or 1: 0
		 means left-to-right, 1 means right-to-left.  */
	      ONE_MORE_BYTE (c1);
	      switch (c1)
		{
		case ']':	/* end of the current direction */
		  coding->direction = 0;

		case '0':	/* end of the current direction */
		case '1':	/* start of left-to-right direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->direction = 0;
		  else
		    goto label_invalid_escape_sequence;
		  break;

		case '2':	/* start of right-to-left direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->direction= 1;
		  else
		    goto label_invalid_escape_sequence;
		  break;

		default:
		  goto label_invalid_escape_sequence;
		}
	      break;

	    default:
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
		{
		  goto label_invalid_escape_sequence;
		}
	    }
	  /* We must update these variables now.  */
	  charset0 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 0);
	  charset1 = CODING_SPEC_ISO_PLANE_CHARSET (coding, 1);
	  break;

	label_invalid_escape_sequence:
	  {
	    int length = src - src_base;

	    bcopy (src_base, dst, length);
	    dst += length;
	  }
	}
      continue;

    label_end_of_loop:
      coding->carryover_size = src - src_base;
      bcopy (src_base, coding->carryover, coding->carryover_size);
      src = src_base;
      break;
    }

  /* If this is the last block of the text to be decoded, we had
     better just flush out all remaining codes in the text although
     they are not valid characters.  */
  if (coding->last_block)
    {
      bcopy (src, dst, src_end - src);
      dst += (src_end - src);
      src = src_end;
    }
  *consumed = src - source;
  return dst - destination;
}

/* ISO2022 encoding staffs.  */

/*
   It is not enough to say just "ISO2022" on encoding, but we have to
   specify more details.  In Emacs, each coding-system of ISO2022
   variant has the following specifications:
	1. Initial designation to G0 thru G3.
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
   detail.
*/

/* Produce codes (escape sequence) for designating CHARSET to graphic
   register REG.  If <final-char> of CHARSET is '@', 'A', or 'B' and
   the coding system CODING allows, produce designation sequence of
   short-form.  */

#define ENCODE_DESIGNATION(charset, reg, coding)			\
  do {									\
    unsigned char final_char = CHARSET_ISO_FINAL_CHAR (charset);	\
    char *intermediate_char_94 = "()*+";				\
    char *intermediate_char_96 = ",-./";				\
    Lisp_Object temp							\
      = Fassq (make_number (charset), Vcharset_revision_alist);		\
    if (! NILP (temp))							\
	{								\
	*dst++ = ISO_CODE_ESC;						\
	*dst++ = '&';							\
	*dst++ = XINT (XCONS (temp)->cdr) + '@';			\
      }									\
    *dst++ = ISO_CODE_ESC;				       		\
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
	    if (! (coding->flags & CODING_FLAG_ISO_SHORT_FORM)     	\
		|| reg != 0					       	\
		|| final_char < '@' || final_char > 'B')	       	\
	      *dst++ = (unsigned char) (intermediate_char_94[reg]);	\
	  }								\
	else								\
	  *dst++ = (unsigned char) (intermediate_char_96[reg]);  	\
      }									\
    *dst++ = final_char;				       		\
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

#define ENCODE_SINGLE_SHIFT_3			   	\
  do {						   	\
    if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)	\
      *dst++ = ISO_CODE_ESC, *dst++ = 'O';	   	\
    else					   	\
      *dst++ = ISO_CODE_SS3;			   	\
    CODING_SPEC_ISO_SINGLE_SHIFTING (coding) = 1;	\
  } while (0)

/* The following four macros produce codes (control character or
   escape sequence) for ISO2022 locking-shift functions (shift-in,
   shift-out, locking-shift-2, and locking-shift-3).  */

#define ENCODE_SHIFT_IN			  	\
  do {					  	\
    *dst++ = ISO_CODE_SI;		  	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;	\
  } while (0)

#define ENCODE_SHIFT_OUT		  	\
  do {					  	\
    *dst++ = ISO_CODE_SO;		  	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 1;	\
  } while (0)

#define ENCODE_LOCKING_SHIFT_2			\
  do {						\
    *dst++ = ISO_CODE_ESC, *dst++ = 'n';	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 2;	\
  } while (0)

#define ENCODE_LOCKING_SHIFT_3		  	\
  do {					  	\
    *dst++ = ISO_CODE_ESC, *dst++ = 'o';  	\
    CODING_SPEC_ISO_INVOCATION (coding, 0) = 3;	\
  } while (0)

/* Produce codes for a DIMENSION1 character of which character set is
   CHARSET and position-code is C1.  Designation and invocation
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

/* Produce codes for a DIMENSION2 character of which character set is
   CHARSET and position-codes are C1 and C2.  Designation and
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
      if (reg < 0)
	/* Since CHARSET requests no special designation, designate to
	   graphic register 0.  */
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

/* The following two macros produce codes for indicating composition.  */
#define ENCODE_COMPOSITION_NO_RULE_START  *dst++ = ISO_CODE_ESC, *dst++ = '0'
#define ENCODE_COMPOSITION_WITH_RULE_START  *dst++ = ISO_CODE_ESC, *dst++ = '2'
#define ENCODE_COMPOSITION_END    *dst++ = ISO_CODE_ESC, *dst++ = '1'

/* The following three macros produce codes for indicating direction
   of text.  */
#define ENCODE_CONTROL_SEQUENCE_INTRODUCER	    	\
  do {						    	\
    if (coding->flags == CODING_FLAG_ISO_SEVEN_BITS)	\
      *dst++ = ISO_CODE_ESC, *dst++ = '[';	    	\
    else					    	\
      *dst++ = ISO_CODE_CSI;			    	\
  } while (0)

#define ENCODE_DIRECTION_R2L	\
  ENCODE_CONTROL_SEQUENCE_INTRODUCER, *dst++ = '2', *dst++ = ']'

#define ENCODE_DIRECTION_L2R	\
  ENCODE_CONTROL_SEQUENCE_INTRODUCER, *dst++ = '0', *dst++ = ']'

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

int
encode_designation_at_bol (coding, src, src_end, dstp)
     struct coding_system *coding;
     unsigned char *src, *src_end, **dstp;
{
  int charset, reg, r[4];
  unsigned char *dst = *dstp, c;
  for (reg = 0; reg < 4; reg++) r[reg] = -1;
  while (src < src_end && (c = *src++) != '\n')
    {
      switch (emacs_code_class[c])
	{
	case EMACS_ascii_code:
	  charset = CHARSET_ASCII;
	  break;
	case EMACS_leading_code_2:
	  if (++src >= src_end) continue;
	  charset = c;
	  break;
	case EMACS_leading_code_3:
	  if ((src += 2) >= src_end) continue;
	  charset =  (c < LEADING_CODE_PRIVATE_11 ? c : *(src - 2));
	  break;
	case EMACS_leading_code_4:
	  if ((src += 3) >= src_end) continue;
	  charset = *(src - 3);
	  break;
	default:
	  continue;
	}
      reg = CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset);
      if (r[reg] < 0
	  && CODING_SPEC_ISO_DESIGNATION (coding, reg) != charset)
	r[reg] = charset;
    }
  if (c != '\n' && !coding->last_block)
    return -1;
  for (reg = 0; reg < 4; reg++)
    if (r[reg] >= 0)
      ENCODE_DESIGNATION (r[reg], reg, coding);
  *dstp = dst;
  return 0;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".  */

int
encode_coding_iso2022 (coding, source, destination,
		       src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Since the maximum bytes produced by each loop is 20, we subtract 19
     from DST_END to assure overflow checking is necessary only at the
     head of loop.  */
  unsigned char *adjusted_dst_end = dst_end - 19;

  while (src < src_end && dst < adjusted_dst_end)
    {
      /* SRC_BASE remembers the start position in source in each loop.
	 The loop will be exited when there's not enough source text
	 to analyze multi-byte codes (within macros ONE_MORE_BYTE,
	 TWO_MORE_BYTES, and THREE_MORE_BYTES).  In that case, SRC is
	 reset to SRC_BASE before exiting.  */
      unsigned char *src_base = src;
      unsigned char c1, c2, c3, c4;
      int charset;

      if (coding->flags & CODING_FLAG_ISO_DESIGNATE_AT_BOL
	  && CODING_SPEC_ISO_BOL (coding))
	{
	  /* We have to produce destination sequences now.  */
	  if (encode_designation_at_bol (coding, src, src_end, &dst) < 0)
	    /* We can't find end of line in the current block.  Let's
	     repeat encoding starting from the current position
	     pointed by SRC.  */
	    break;
	  CODING_SPEC_ISO_BOL (coding) = 0;
	}

      c1 = *src++;
      /* If we are seeing a component of a composite character, we are
	 seeing a leading-code specially encoded for composition, or a
	 composition rule if composing with rule.  We must set C1
	 to a normal leading-code or an ASCII code.  If we are not at
	 a composed character, we must reset the composition state.  */
      if (COMPOSING_P (coding->composing))
	{
	  if (c1 < 0xA0)
	    {
	      /* We are not in a composite character any longer.  */
	      coding->composing = COMPOSING_NO;
	      ENCODE_COMPOSITION_END;
	    }
	  else
	    {
	      if (coding->composing == COMPOSING_WITH_RULE_RULE)
		{
		  *dst++ = c1 & 0x7F;
		  coding->composing = COMPOSING_WITH_RULE_HEAD;
		  continue;
		}
	      else if (coding->composing == COMPOSING_WITH_RULE_HEAD)
		coding->composing = COMPOSING_WITH_RULE_RULE;
	      if (c1 == 0xA0)
		{
		  /* This is an ASCII component.  */
		  ONE_MORE_BYTE (c1);
		  c1 &= 0x7F;
		}
	      else
		/* This is a leading-code of non ASCII component.  */
		c1 -= 0x20;
	    }
	}
	
      /* Now encode one character.  C1 is a control character, an
         ASCII character, or a leading-code of multi-byte character.  */
      switch (emacs_code_class[c1])
	{
	case EMACS_ascii_code:
	  ENCODE_ISO_CHARACTER_DIMENSION1 (CHARSET_ASCII, c1);
	  break;

	case EMACS_control_code:
	  if (coding->flags & CODING_FLAG_ISO_RESET_AT_CNTL)
	    ENCODE_RESET_PLANE_AND_REGISTER;
	  *dst++ = c1;
	  break;

	case EMACS_carriage_return_code:
	  if (!coding->selective)
	    {
	      if (coding->flags & CODING_FLAG_ISO_RESET_AT_CNTL)
		ENCODE_RESET_PLANE_AND_REGISTER;
	      *dst++ = c1;
	      break;
	    }
	  /* fall down to treat '\r' as '\n' ...  */

	case EMACS_linefeed_code:
	  if (coding->flags & CODING_FLAG_ISO_RESET_AT_EOL)
	    ENCODE_RESET_PLANE_AND_REGISTER;
	  if (coding->flags & CODING_FLAG_ISO_INIT_AT_BOL)
	    bcopy (coding->spec.iso2022.initial_designation,
		   coding->spec.iso2022.current_designation,
		   sizeof coding->spec.iso2022.initial_designation);
	  if (coding->eol_type == CODING_EOL_LF
	      || coding->eol_type == CODING_EOL_AUTOMATIC)
	    *dst++ = ISO_CODE_LF;
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    *dst++ = ISO_CODE_CR, *dst++ = ISO_CODE_LF;
	  else
	    *dst++ = ISO_CODE_CR;
	  CODING_SPEC_ISO_BOL (coding) = 1;
	  break;

	case EMACS_leading_code_2:
	  ONE_MORE_BYTE (c2);
	  ENCODE_ISO_CHARACTER_DIMENSION1 (c1, c2);
	  break;

	case EMACS_leading_code_3:
	  TWO_MORE_BYTES (c2, c3);
	  if (c1 < LEADING_CODE_PRIVATE_11)
	    ENCODE_ISO_CHARACTER_DIMENSION2 (c1, c2, c3);
	  else
	    ENCODE_ISO_CHARACTER_DIMENSION1 (c2, c3);
	  break;

	case EMACS_leading_code_4:
	  THREE_MORE_BYTES (c2, c3, c4);
	  ENCODE_ISO_CHARACTER_DIMENSION2 (c2, c3, c4);
	  break;

	case EMACS_leading_code_composition:
	  ONE_MORE_BYTE (c1);
	  if (c1 == 0xFF)
	    {
	      coding->composing = COMPOSING_WITH_RULE_HEAD;
	      ENCODE_COMPOSITION_WITH_RULE_START;
	    }
	  else
	    {
	      /* Rewind one byte because it is a character code of
                 composition elements.  */
	      src--;
	      coding->composing = COMPOSING_NO_RULE_HEAD;
	      ENCODE_COMPOSITION_NO_RULE_START;
	    }
	  break;

	case EMACS_invalid_code:
	  *dst++ = c1;
	  break;
	}
      continue;
    label_end_of_loop:
      coding->carryover_size = src - src_base;
      bcopy (src_base, coding->carryover, coding->carryover_size);
      src = src_base;
      break;
    }

  /* If this is the last block of the text to be encoded, we must
     reset the state of graphic planes and registers to initial one.
     In addition, we had better just flush out all remaining codes in
     the text although they are not valid characters.  */
  if (coding->last_block)
    {
      ENCODE_RESET_PLANE_AND_REGISTER;
      bcopy(src, dst, src_end - src);
      dst += (src_end - src);
      src = src_end;
    }
  *consumed = src - source;
  return dst - destination;
}


/*** 4. SJIS and BIG5 handlers ***/

/* Although SJIS and BIG5 are not ISO's coding system, They are used
   quite widely.  So, for the moment, Emacs supports them in the bare
   C code.  But, in the future, they may be supported only by CCL.  */

/* SJIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of charset katakana-jisx0201 is encoded by
   "position-code + 0x80".  A character of charset japanese-jisx0208
   is encoded in 2-byte but two position-codes are divided and shifted
   so that it fit in the range below.

   --- CODE RANGE of SJIS ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   KATAKANA-JISX0201	0xA0 .. 0xDF
   JISX0208 (1st byte)	0x80 .. 0x9F and 0xE0 .. 0xFF
	    (2nd byte)	0x40 .. 0xFF
   -------------------------------

*/

/* BIG5 is a coding system encoding two character sets: ASCII and
   Big5.  An ASCII character is encoded as is.  Big5 is a two-byte
   character set and is encoded in two-byte.

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

int
detect_coding_sjis (src, src_end)
     unsigned char *src, *src_end;
{
  unsigned char c;

  while (src < src_end)
    {
      c = *src++;
      if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	return 0;
      if ((c >= 0x80 && c < 0xA0) || c >= 0xE0)
	{
	  if (src < src_end && *src++ < 0x40)
	    return 0;
	}
    }
  return CODING_CATEGORY_MASK_SJIS;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in BIG5.  If it is, return
   CODING_CATEGORY_MASK_BIG5, else return 0.  */

int
detect_coding_big5 (src, src_end)
     unsigned char *src, *src_end;
{
  unsigned char c;

  while (src < src_end)
    {
      c = *src++;
      if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	return 0;
      if (c >= 0xA1)
	{
	  if (src >= src_end)
	    break;
	  c = *src++;
	  if (c < 0x40 || (c >= 0x7F && c <= 0xA0))
	    return 0;
	}
    }
  return CODING_CATEGORY_MASK_BIG5;
}

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".
   If SJIS_P is 1, decode SJIS text, else decode BIG5 test.  */

int
decode_coding_sjis_big5 (coding, source, destination,
			 src_bytes, dst_bytes, consumed, sjis_p)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
     int sjis_p;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Since the maximum bytes produced by each loop is 4, we subtract 3
     from DST_END to assure overflow checking is necessary only at the
     head of loop.  */
  unsigned char *adjusted_dst_end = dst_end - 3;

  while (src < src_end && dst < adjusted_dst_end)
    {
      /* SRC_BASE remembers the start position in source in each loop.
	 The loop will be exited when there's not enough source text
	 to analyze two-byte character (within macro ONE_MORE_BYTE).
	 In that case, SRC is reset to SRC_BASE before exiting.  */
      unsigned char *src_base = src;
      unsigned char c1 = *src++, c2, c3, c4;

      if (c1 == '\r')
	{
	  if (coding->eol_type == CODING_EOL_CRLF)
	    {
	      ONE_MORE_BYTE (c2);
	      if (c2 == '\n')
		*dst++ = c2;
	      else
		/* To process C2 again, SRC is subtracted by 1.  */
		*dst++ = c1, src--;
	    }
	  else
	    *dst++ = c1;
	}
      else if (c1 < 0x80)
	*dst++ = c1;
      else if (c1 < 0xA0 || c1 >= 0xE0)
	{
	  /* SJIS -> JISX0208, BIG5 -> Big5 (only if 0xE0 <= c1 < 0xFF) */
	  if (sjis_p)
	    {
	      ONE_MORE_BYTE (c2);
	      DECODE_SJIS (c1, c2, c3, c4);
	      DECODE_CHARACTER_DIMENSION2 (charset_jisx0208, c3, c4);
	    }
	  else if (c1 >= 0xE0 && c1 < 0xFF)
	    {
	      int charset;

	      ONE_MORE_BYTE (c2);
	      DECODE_BIG5 (c1, c2, charset, c3, c4);
	      DECODE_CHARACTER_DIMENSION2 (charset, c3, c4);
	    }
	  else			/* Invalid code */
	    *dst++ = c1;
	}
      else
	{
	  /* SJIS -> JISX0201-Kana, BIG5 -> Big5 */
	  if (sjis_p)
	    DECODE_CHARACTER_DIMENSION1 (charset_katakana_jisx0201, c1);
	  else
	    {
	      int charset;

	      ONE_MORE_BYTE (c2);
	      DECODE_BIG5 (c1, c2, charset, c3, c4);
	      DECODE_CHARACTER_DIMENSION2 (charset, c3, c4);
	    }
	}
      continue;

    label_end_of_loop:
      coding->carryover_size = src - src_base;
      bcopy (src_base, coding->carryover, coding->carryover_size);
      src = src_base;
      break;
    }

  *consumed = src - source;
  return dst - destination;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".
   This function can encode `charset_ascii', `charset_katakana_jisx0201',
   `charset_jisx0208', `charset_big5_1', and `charset_big5-2'.  We are
   sure that all these charsets are registered as official charset
   (i.e. do not have extended leading-codes).  Characters of other
   charsets are produced without any encoding.  If SJIS_P is 1, encode
   SJIS text, else encode BIG5 text.  */

int
encode_coding_sjis_big5 (coding, source, destination,
			 src_bytes, dst_bytes, consumed, sjis_p)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
     int sjis_p;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  /* Since the maximum bytes produced by each loop is 2, we subtract 1
     from DST_END to assure overflow checking is necessary only at the
     head of loop.  */
  unsigned char *adjusted_dst_end = dst_end - 1;

  while (src < src_end && dst < adjusted_dst_end)
    {
      /* SRC_BASE remembers the start position in source in each loop.
	 The loop will be exited when there's not enough source text
	 to analyze multi-byte codes (within macros ONE_MORE_BYTE and
	 TWO_MORE_BYTES).  In that case, SRC is reset to SRC_BASE
	 before exiting.  */
      unsigned char *src_base = src;
      unsigned char c1 = *src++, c2, c3, c4;

      if (coding->composing)
	{
	  if (c1 == 0xA0)
	    {
	      ONE_MORE_BYTE (c1);
	      c1 &= 0x7F;
	    }
	  else if (c1 >= 0xA0)
	    c1 -= 0x20;
	  else
	    coding->composing = 0;
	}

      switch (emacs_code_class[c1])
	{
	case EMACS_ascii_code:
	case EMACS_control_code:
	  *dst++ = c1;
	  break;

	case EMACS_carriage_return_code:
	  if (!coding->selective)
	    {
	      *dst++ = c1;
	      break;
	    }
	  /* fall down to treat '\r' as '\n' ...  */

	case EMACS_linefeed_code:
	  if (coding->eol_type == CODING_EOL_LF
	      || coding->eol_type == CODING_EOL_AUTOMATIC)
	    *dst++ = '\n';
	  else if (coding->eol_type == CODING_EOL_CRLF)
	    *dst++ = '\r', *dst++ = '\n';
	  else
	    *dst++ = '\r';
	  break;

	case EMACS_leading_code_2:
	  ONE_MORE_BYTE (c2);
	  if (sjis_p && c1 == charset_katakana_jisx0201)
	    *dst++ = c2;
	  else
	    *dst++ = c1, *dst++ = c2;
	  break;

	case EMACS_leading_code_3:
	  TWO_MORE_BYTES (c2, c3);
	  c2 &= 0x7F, c3 &= 0x7F;
	  if (sjis_p && c1 == charset_jisx0208)
	    {
	      unsigned char s1, s2;

	      ENCODE_SJIS (c2, c3, s1, s2);
	      *dst++ = s1, *dst++ = s2;
	    }
	  else if (!sjis_p && (c1 == charset_big5_1 || c1 == charset_big5_2))
	    {
	      unsigned char b1, b2;

	      ENCODE_BIG5 (c1, c2, c3, b1, b2);
	      *dst++ = b1, *dst++ = b2;
	    }
	  else
	    *dst++ = c1, *dst++ = c2, *dst++ = c3;
	  break;

	case EMACS_leading_code_4:
	  THREE_MORE_BYTES (c2, c3, c4);
	  *dst++ = c1, *dst++ = c2, *dst++ = c3, *dst++ = c4;
	  break;

	case EMACS_leading_code_composition:
	  coding->composing = 1;
	  break;

	default:		/* i.e. case EMACS_invalid_code: */
	  *dst++ = c1;
	}
      continue;

    label_end_of_loop:
      coding->carryover_size = src - src_base;
      bcopy (src_base, coding->carryover, coding->carryover_size);
      src = src_base;
      break;
    }

  *consumed = src - source;
  return dst - destination;
}


/*** 5. End-of-line handlers ***/

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".
   This function is called only when `coding->eol_type' is
   CODING_EOL_CRLF or CODING_EOL_CR.  */

decode_eol (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  unsigned char *src = source;
  unsigned char *src_end = source + src_bytes;
  unsigned char *dst = destination;
  unsigned char *dst_end = destination + dst_bytes;
  int produced;

  switch (coding->eol_type)
    {
    case CODING_EOL_CRLF:
      {
	/* Since the maximum bytes produced by each loop is 2, we
	   subtract 1 from DST_END to assure overflow checking is
	   necessary only at the head of loop.  */
	unsigned char *adjusted_dst_end = dst_end - 1;

	while (src < src_end && dst < adjusted_dst_end)
	  {
	    unsigned char *src_base = src;
	    unsigned char c = *src++;
	    if (c == '\r')
	      {
		ONE_MORE_BYTE (c);
		if (c != '\n')
		  *dst++ = '\r';
		*dst++ = c;
	      }
	    else
	      *dst++ = c;
	    continue;

	  label_end_of_loop:
	    coding->carryover_size = src - src_base;
	    bcopy (src_base, coding->carryover, coding->carryover_size);
	    src = src_base;
	    break;
	  }
	*consumed = src - source;
	produced = dst - destination;
	break;
      }

    case CODING_EOL_CR:
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      bcopy (source, destination, produced);
      dst_end = destination + produced;
      while (dst < dst_end)
	if (*dst++ == '\r') dst[-1] = '\n';
      *consumed = produced;
      break;

    default:			/* i.e. case: CODING_EOL_LF */
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      bcopy (source, destination, produced);
      *consumed = produced;
      break;
    }

  return produced;
}

/* See "GENERAL NOTES about `encode_coding_XXX ()' functions".  Encode
   format of end-of-line according to `coding->eol_type'.  If
   `coding->selective' is 1, code '\r' in source text also means
   end-of-line.  */

encode_eol (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  unsigned char *src = source;
  unsigned char *dst = destination;
  int produced;

  if (src_bytes <= 0)
    return 0;

  switch (coding->eol_type)
    {
    case CODING_EOL_LF:
    case CODING_EOL_AUTOMATIC:
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      bcopy (source, destination, produced);
      if (coding->selective)
	{
	  int i = produced;
	  while (i--)
	    if (*dst++ == '\r') dst[-1] = '\n';
	}
      *consumed = produced;
      
    case CODING_EOL_CRLF:
      {
	unsigned char c;
	unsigned char *src_end = source + src_bytes;
	unsigned char *dst_end = destination + dst_bytes;
	/* Since the maximum bytes produced by each loop is 2, we
	   subtract 1 from DST_END to assure overflow checking is
	   necessary only at the head of loop.  */
	unsigned char *adjusted_dst_end = dst_end - 1;

	while (src < src_end && dst < adjusted_dst_end)
	  {
	    c = *src++;
	    if (c == '\n' || (c == '\r' && coding->selective))
	      *dst++ = '\r', *dst++ = '\n';
	    else
	      *dst++ = c;
	  }
	produced = dst - destination;
	*consumed = src - source;
	break;
      }

    default:			/* i.e. case CODING_EOL_CR: */
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      bcopy (source, destination, produced);
      {
	int i = produced;
	while (i--)
	  if (*dst++ == '\n') dst[-1] = '\r';
      }
      *consumed = produced;
    }

  return produced;
}


/*** 6. C library functions ***/

/* In Emacs Lisp, coding system is represented by a Lisp symbol which
   has a property `coding-system'.  The value of this property is a
   vector of length 5 (called as coding-vector).  Among elements of
   this vector, the first (element[0]) and the fifth (element[4])
   carry important information for decoding/encoding.  Before
   decoding/encoding, this information should be set in fields of a
   structure of type `coding_system'.

   A value of property `coding-system' can be a symbol of another
   subsidiary coding-system.  In that case, Emacs gets coding-vector
   from that symbol.

   `element[0]' contains information to be set in `coding->type'.  The
   value and its meaning is as follows:

   0 -- coding_system_internal
   1 -- coding_system_sjis
   2 -- coding_system_iso2022
   3 -- coding_system_big5
   4 -- coding_system_ccl
   nil -- coding_system_no_conversion
   t -- coding_system_automatic

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

   Emacs Lisp's coding system also carries information about format of
   end-of-line in a value of property `eol-type'.  If the value is
   integer, 0 means CODING_EOL_LF, 1 means CODING_EOL_CRLF, and 2
   means CODING_EOL_CR.  If it is not integer, it should be a vector
   of subsidiary coding systems of which property `eol-type' has one
   of above values.

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
  Lisp_Object type, eol_type;

  /* At first, set several fields default values.  */
  coding->require_flushing = 0;
  coding->last_block = 0;
  coding->selective = 0;
  coding->composing = 0;
  coding->direction = 0;
  coding->carryover_size = 0;
  coding->post_read_conversion = coding->pre_write_conversion = Qnil;

  Vlast_coding_system_used = coding->symbol = coding_system;
  eol_type = Qnil;
  /* Get value of property `coding-system' until we get a vector.
     While doing that, also get values of properties
     `post-read-conversion', `pre-write-conversion', and `eol-type'.  */
  while (!NILP (coding_system) && SYMBOLP (coding_system))
    {
      if (NILP (coding->post_read_conversion))
	coding->post_read_conversion = Fget (coding_system,
					     Qpost_read_conversion);
      if (NILP (coding->pre_write_conversion))	
	coding->pre_write_conversion = Fget (coding_system,
					     Qpre_write_conversion);
      if (NILP (eol_type))
	eol_type = Fget (coding_system, Qeol_type);
      coding_system = Fget (coding_system, Qcoding_system);
    }
  if (!VECTORP (coding_system)
      || XVECTOR (coding_system)->size != 5)
    goto label_invalid_coding_system;

  if (VECTORP (eol_type))
    coding->eol_type = CODING_EOL_AUTOMATIC;
  else if (XFASTINT (eol_type) == 1)
    coding->eol_type = CODING_EOL_CRLF;
  else if (XFASTINT (eol_type) == 2)
    coding->eol_type = CODING_EOL_CR;
  else
    coding->eol_type = CODING_EOL_LF;

  type = XVECTOR (coding_system)->contents[0];
  switch (XFASTINT (type))
    {
    case 0:
      coding->type = coding_type_internal;
      break;

    case 1:
      coding->type = coding_type_sjis;
      break;

    case 2:
      coding->type = coding_type_iso2022;
      {
	Lisp_Object val = XVECTOR (coding_system)->contents[4];
	Lisp_Object *flags;
	int i, charset, default_reg_bits = 0;

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
	     | (NILP (flags[14]) ? 0 : CODING_FLAG_ISO_DESIGNATE_AT_BOL));

	/* Invoke graphic register 0 to plane 0.  */
	CODING_SPEC_ISO_INVOCATION (coding, 0) = 0;
	/* Invoke graphic register 1 to plane 1 if we can use full 8-bit.  */
	CODING_SPEC_ISO_INVOCATION (coding, 1)
	  = (coding->flags & CODING_FLAG_ISO_SEVEN_BITS ? -1 : 1);
	/* Not single shifting at first.  */
	CODING_SPEC_ISO_SINGLE_SHIFTING(coding) = 0;
	/* Beginning of buffer should also be regarded as bol. */
	CODING_SPEC_ISO_BOL(coding) = 1;

	/* Checks FLAGS[REG] (REG = 0, 1, 2 3) and decide designations.
	   FLAGS[REG] can be one of below:
		integer CHARSET: CHARSET occupies register I,
		t: designate nothing to REG initially, but can be used
		  by any charsets,
		list of integer, nil, or t: designate the first
		  element (if integer) to REG initially, the remaining
		  elements (if integer) is designated to REG on request,
		  if an element is t, REG can be used by any charset,
		nil: REG is never used.  */
	for (charset = 0; charset < MAX_CHARSET; charset++)
	  CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) = -1;
	for (i = 0; i < 4; i++)
	  {
	    if (INTEGERP (flags[i])
		&& (charset = XINT (flags[i]), CHARSET_VALID_P (charset))
		|| (charset = get_charset_id (flags[i])) >= 0)
	      {
		CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = charset;
		CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) = i;
	      }
	    else if (EQ (flags[i], Qt))
	      {
		CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;
		default_reg_bits |= 1 << i;
	      }
	    else if (CONSP (flags[i]))
	      {
		Lisp_Object tail = flags[i];

		if (INTEGERP (XCONS (tail)->car)
		    && (charset = XINT (XCONS (tail)->car),
			CHARSET_VALID_P (charset))
		    || (charset = get_charset_id (XCONS (tail)->car)) >= 0)
		  {
		    CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = charset;
		    CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) =i;
		  }
		else
		  CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;
		tail = XCONS (tail)->cdr;
		while (CONSP (tail))
		  {
		    if (INTEGERP (XCONS (tail)->car)
			&& (charset = XINT (XCONS (tail)->car),
			    CHARSET_VALID_P (charset))
			|| (charset = get_charset_id (XCONS (tail)->car)) >= 0)
		      CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
			= i;
		    else if (EQ (XCONS (tail)->car, Qt))
		      default_reg_bits |= 1 << i;
		    tail = XCONS (tail)->cdr;
		  }
	      }
	    else
	      CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i) = -1;
	    
	    CODING_SPEC_ISO_DESIGNATION (coding, i)
	      = CODING_SPEC_ISO_INITIAL_DESIGNATION (coding, i);
	  }

	if (! (coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT))
	  {
	    /* REG 1 can be used only by locking shift in 7-bit env.  */
	    if (coding->flags & CODING_FLAG_ISO_SEVEN_BITS)
	      default_reg_bits &= ~2;
	    if (! (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT))
	      /* Without any shifting, only REG 0 and 1 can be used.  */
	      default_reg_bits &= 3;
	  }

	for (charset = 0; charset < MAX_CHARSET; charset++)
	  if (CHARSET_VALID_P (charset)
	      && CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset) < 0)
	    {
	      /* We have not yet decided where to designate CHARSET.  */
	      int reg_bits = default_reg_bits;

	      if (CHARSET_CHARS (charset) == 96)
		/* A charset of CHARS96 can't be designated to REG 0.  */
		reg_bits &= ~1;

	      if (reg_bits)
		/* There exist some default graphic register.  */
		CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
		  = (reg_bits & 1
		     ? 0 : (reg_bits & 2 ? 1 : (reg_bits & 4 ? 2 : 3)));
	      else
		/* We anyway have to designate CHARSET to somewhere.  */
		CODING_SPEC_ISO_REQUESTED_DESIGNATION (coding, charset)
		  = (CHARSET_CHARS (charset) == 94
		     ? 0
		     : ((coding->flags & CODING_FLAG_ISO_LOCKING_SHIFT
			 || ! coding->flags & CODING_FLAG_ISO_SEVEN_BITS)
			? 1
			: (coding->flags & CODING_FLAG_ISO_SINGLE_SHIFT
			   ? 2 : 0)));
	    }
      }
      coding->require_flushing = 1;
      break;

    case 3:
      coding->type = coding_type_big5;
      coding->flags
	= (NILP (XVECTOR (coding_system)->contents[4])
	   ? CODING_FLAG_BIG5_HKU
	   : CODING_FLAG_BIG5_ETEN);
      break;

    case 4:
      coding->type = coding_type_ccl;
      {
	Lisp_Object val = XVECTOR (coding_system)->contents[4];
	if (CONSP  (val)
	    && VECTORP (XCONS (val)->car)
	    && VECTORP (XCONS (val)->cdr))
	  {
	    setup_ccl_program (&(coding->spec.ccl.decoder), XCONS (val)->car);
	    setup_ccl_program (&(coding->spec.ccl.encoder), XCONS (val)->cdr);
	  }
	else
	  goto label_invalid_coding_system;
      }
      coding->require_flushing = 1;
      break;

    default:
      if (EQ (type, Qt))
	coding->type = coding_type_automatic;
      else
	coding->type = coding_type_no_conversion;
      break;
    }
  return 0;

 label_invalid_coding_system:
  coding->type = coding_type_no_conversion;
  coding->symbol = coding->pre_write_conversion = coding->post_read_conversion
    = Qnil;
  return -1;
}

/* Emacs has a mechanism to automatically detect a coding system if it
   is one of Emacs' internal format, ISO2022, SJIS, and BIG5.  But,
   it's impossible to distinguish some coding systems accurately
   because they use the same range of codes.  So, at first, coding
   systems are categorized into 7, those are:

   o coding-category-internal

   	The category for a coding system which has the same code range
	as Emacs' internal format.  Assigned the coding-system (Lisp
	symbol) `internal' by default.

   o coding-category-sjis

	The category for a coding system which has the same code range
	as SJIS.  Assigned the coding-system (Lisp
	symbol) `shift-jis' by default.

   o coding-category-iso-7

   	The category for a coding system which has the same code range
	as ISO2022 of 7-bit environment.  Assigned the coding-system
	(Lisp symbol) `iso-2022-7' by default.

   o coding-category-iso-8-1

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION1 charset.  Assigned the coding-system (Lisp
	symbol) `iso-8859-1' by default.

   o coding-category-iso-8-2

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION2 charset.  Assigned the coding-system (Lisp
	symbol) `euc-japan' by default.

   o coding-category-iso-else

   	The category for a coding system which has the same code range
	as ISO2022 but not belongs to any of the above three
	categories.  Assigned the coding-system (Lisp symbol)
	`iso-2022-ss2-7' by default.

   o coding-category-big5

   	The category for a coding system which has the same code range
	as BIG5.  Assigned the coding-system (Lisp symbol)
	`cn-big5' by default.

   o coding-category-binary

   	The category for a coding system not categorized in any of the
	above.  Assigned the coding-system (Lisp symbol)
	`no-conversion' by default.

   Each of them is a Lisp symbol and the value is an actual
   `coding-system's (this is also a Lisp symbol) assigned by a user.
   What Emacs does actually is to detect a category of coding system.
   Then, it uses a `coding-system' assigned to it.  If Emacs can't
   decide only one possible category, it selects a category of the
   highest priority.  Priorities of categories are also specified by a
   user in a Lisp variable `coding-category-list'.

*/

/* Detect how a text of length SRC_BYTES pointed by SRC is encoded.
   If it detects possible coding systems, return an integer in which
   appropriate flag bits are set.  Flag bits are defined by macros
   CODING_CATEGORY_MASK_XXX in `coding.h'.  */

int
detect_coding_mask (src, src_bytes)
     unsigned char *src;
     int src_bytes;
{
  register unsigned char c;
  unsigned char *src_end = src + src_bytes;
  int mask;

  /* At first, skip all ASCII characters and control characters except
     for three ISO2022 specific control characters.  */
  while (src < src_end)
    {
      c = *src;
      if (c >= 0x80
	  || (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO))
	break;
      src++;
    }

  if (src >= src_end)
    /* We found nothing other than ASCII.  There's nothing to do.  */
    return CODING_CATEGORY_MASK_ANY;

  /* The text seems to be encoded in some multilingual coding system.
     Now, try to find in which coding system the text is encoded.  */
  if (c < 0x80)
    /* i.e. (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO) */
    /* C is an ISO2022 specific control code of C0.  */
    mask = detect_coding_iso2022 (src, src_end);

  else if (c == ISO_CODE_SS2 || c == ISO_CODE_SS3 || c == ISO_CODE_CSI)
    /* C is an ISO2022 specific control code of C1,
       or the first byte of SJIS's 2-byte character code,
       or a leading code of Emacs.  */
    mask = (detect_coding_iso2022 (src, src_end)
	    | detect_coding_sjis (src, src_end)
	    | detect_coding_internal (src, src_end));

  else if (c < 0xA0)
    /* C is the first byte of SJIS character code,
       or a leading-code of Emacs.  */
    mask = (detect_coding_sjis (src, src_end)
	    | detect_coding_internal (src, src_end));

  else
    /* C is a character of ISO2022 in graphic plane right,
       or a SJIS's 1-byte character code (i.e. JISX0201),
       or the first byte of BIG5's 2-byte code.  */
    mask = (detect_coding_iso2022 (src, src_end)
	    | detect_coding_sjis (src, src_end)
	    | detect_coding_big5 (src, src_end));

  return mask;
}

/* Detect how a text of length SRC_BYTES pointed by SRC is encoded.
   The information of the detected coding system is set in CODING.  */

void
detect_coding (coding, src, src_bytes)
     struct coding_system *coding;
     unsigned char *src;
     int src_bytes;
{
  int mask = detect_coding_mask (src, src_bytes);
  int idx;

  if (mask == CODING_CATEGORY_MASK_ANY)
    /* We found nothing other than ASCII.  There's nothing to do.  */
    return;

  if (!mask)
    /* The source text seems to be encoded in unknown coding system.
       Emacs regards the category of such a kind of coding system as
       `coding-category-binary'.  We assume that a user has assigned
       an appropriate coding system for a `coding-category-binary'.  */
    idx = CODING_CATEGORY_IDX_BINARY;
  else
    {
      /* We found some plausible coding systems.  Let's use a coding
	 system of the highest priority.  */
      Lisp_Object val = Vcoding_category_list;

      if (CONSP (val))
	while (!NILP (val))
	  {
	    idx = XFASTINT (Fget (XCONS (val)->car, Qcoding_category_index));
	    if ((idx < CODING_CATEGORY_IDX_MAX) && (mask & (1 << idx)))
	      break;
	    val = XCONS (val)->cdr;
	  }
      else
	val = Qnil;

      if (NILP (val))
	{
	  /* For unknown reason, `Vcoding_category_list' contains none
	     of found categories.  Let's use any of them.  */
	  for (idx = 0; idx < CODING_CATEGORY_IDX_MAX; idx++)
	    if (mask & (1 << idx))
	      break;
	}
    }
  setup_coding_system (XSYMBOL (coding_category_table[idx])->value, coding);
}

/* Detect how end-of-line of a text of length SRC_BYTES pointed by SRC
   is encoded.  Return one of CODING_EOL_LF, CODING_EOL_CRLF,
   CODING_EOL_CR, and CODING_EOL_AUTOMATIC.  */

int
detect_eol_type (src, src_bytes)
     unsigned char *src;
     int src_bytes;
{
  unsigned char *src_end = src + src_bytes;
  unsigned char c;

  while (src < src_end)
    {
      c = *src++;
      if (c == '\n')
	return CODING_EOL_LF;
      else if (c == '\r')
	{
	  if (src < src_end && *src == '\n')
	    return CODING_EOL_CRLF;
	  else
	    return CODING_EOL_CR;
	}
    }
  return CODING_EOL_AUTOMATIC;
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
  int eol_type = detect_eol_type (src, src_bytes);

  if (eol_type == CODING_EOL_AUTOMATIC)
    /*  We found no end-of-line in the source text.  */
    return;

  val = Fget (coding->symbol, Qeol_type);
  if (VECTORP (val) && XVECTOR (val)->size == 3)
    setup_coding_system (XVECTOR (val)->contents[eol_type], coding);
}

/* See "GENERAL NOTES about `decode_coding_XXX ()' functions".  Before
   decoding, it may detect coding system and format of end-of-line if
   those are not yet decided.  */

int
decode_coding (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  int produced;

  if (src_bytes <= 0)
    {
      *consumed = 0;
      return 0;
    }

  if (coding->type == coding_type_automatic)
    detect_coding (coding, source, src_bytes);

  if (coding->eol_type == CODING_EOL_AUTOMATIC)
    detect_eol (coding, source, src_bytes);

  coding->carryover_size = 0;
  switch (coding->type)
    {
    case coding_type_no_conversion:
    label_no_conversion:
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      bcopy (source, destination, produced);
      *consumed = produced;
      break;

    case coding_type_internal:
    case coding_type_automatic:
      if (coding->eol_type == CODING_EOL_LF
	  ||  coding->eol_type == CODING_EOL_AUTOMATIC)
	goto label_no_conversion;
      produced = decode_eol (coding, source, destination,
			     src_bytes, dst_bytes, consumed);
      break;

    case coding_type_sjis:
      produced = decode_coding_sjis_big5 (coding, source, destination,
					  src_bytes, dst_bytes, consumed,
					  1);
      break;

    case coding_type_iso2022:
      produced = decode_coding_iso2022 (coding, source, destination,
					src_bytes, dst_bytes, consumed);
      break;

    case coding_type_big5:
      produced = decode_coding_sjis_big5 (coding, source, destination,
					  src_bytes, dst_bytes, consumed,
					  0);
      break;

    case coding_type_ccl:
      produced = ccl_driver (&coding->spec.ccl.decoder, source, destination,
			     src_bytes, dst_bytes, consumed);
      break;
    }

  return produced;
}

/* See "GENERAL NOTES about `encode_coding_XXX ()' functions".  */

int
encode_coding (coding, source, destination, src_bytes, dst_bytes, consumed)
     struct coding_system *coding;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  int produced;

  coding->carryover_size = 0;
  switch (coding->type)
    {
    case coding_type_no_conversion:
    label_no_conversion:
      produced = (src_bytes > dst_bytes) ? dst_bytes : src_bytes;
      if (produced > 0)
	{
	  bcopy (source, destination, produced);
	  if (coding->selective)
	    {
	      unsigned char *p = destination, *pend = destination + produced;
	      while (p < pend)
		if (*p++ == '\015') p[-1] = '\n';
	    }
	}
      *consumed = produced;
      break;

    case coding_type_internal:
    case coding_type_automatic:
      if (coding->eol_type == CODING_EOL_LF
	  ||  coding->eol_type == CODING_EOL_AUTOMATIC)
	goto label_no_conversion;
      produced = encode_eol (coding, source, destination,
			     src_bytes, dst_bytes, consumed);
      break;

    case coding_type_sjis:
      produced = encode_coding_sjis_big5 (coding, source, destination,
					  src_bytes, dst_bytes, consumed,
					  1);
      break;

    case coding_type_iso2022:
      produced = encode_coding_iso2022 (coding, source, destination,
					src_bytes, dst_bytes, consumed);
      break;

    case coding_type_big5:
      produced = encode_coding_sjis_big5 (coding, source, destination,
					  src_bytes, dst_bytes, consumed,
					  0);
      break;

    case coding_type_ccl:
      produced = ccl_driver (&coding->spec.ccl.encoder, source, destination,
			     src_bytes, dst_bytes, consumed);
      break;
    }

  return produced;
}

#define CONVERSION_BUFFER_EXTRA_ROOM 256

/* Return maximum size (bytes) of a buffer enough for decoding
   SRC_BYTES of text encoded in CODING.  */

int
decoding_buffer_size (coding, src_bytes)
     struct coding_system *coding;
     int src_bytes;
{
  int magnification;

  if (coding->type == coding_type_iso2022)
    magnification = 3;
  else if (coding->type == coding_type_ccl)
    magnification = coding->spec.ccl.decoder.buf_magnification;
  else
    magnification = 2;

  return (src_bytes * magnification + CONVERSION_BUFFER_EXTRA_ROOM);
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
  else
    magnification = 3;

  return (src_bytes * magnification + CONVERSION_BUFFER_EXTRA_ROOM);
}

#ifndef MINIMUM_CONVERSION_BUFFER_SIZE
#define MINIMUM_CONVERSION_BUFFER_SIZE 1024
#endif

char *conversion_buffer;
int conversion_buffer_size;

/* Return a pointer to a SIZE bytes of buffer to be used for encoding
   or decoding.  Sufficient memory is allocated automatically.  If we
   run out of memory, return NULL.  */

char *
get_conversion_buffer (size)
     int size;
{
  if (size > conversion_buffer_size)
    {
      char *buf;
      int real_size = conversion_buffer_size * 2;

      while (real_size < size) real_size *= 2;
      buf = (char *) xmalloc (real_size);
      xfree (conversion_buffer);
      conversion_buffer = buf;
      conversion_buffer_size = real_size;
    }
  return conversion_buffer;
}


#ifdef emacs
/*** 7. Emacs Lisp library functions ***/

DEFUN ("coding-system-vector", Fcoding_system_vector, Scoding_system_vector,
       1, 1, 0,
  "Return coding-vector of CODING-SYSTEM.\n\
If CODING-SYSTEM is not a valid coding-system, return nil.")
  (obj)
     Lisp_Object obj;
{
  while (SYMBOLP (obj) && !NILP (obj))
    obj = Fget (obj, Qcoding_system);
  return ((NILP (obj) || !VECTORP (obj) || XVECTOR (obj)->size != 5)
	  ? Qnil : obj);
}

DEFUN ("coding-system-p", Fcoding_system_p, Scoding_system_p, 1, 1, 0,
  "Return t if OBJECT is nil or a coding-system.\n\
See document of make-coding-system for coding-system object.")
  (obj)
     Lisp_Object obj;
{
  return ((NILP (obj) || !NILP (Fcoding_system_vector (obj))) ? Qt : Qnil);
}

DEFUN ("read-non-nil-coding-system",
       Fread_non_nil_coding_system, Sread_non_nil_coding_system, 1, 1, 0,
  "Read a coding system from the minibuffer, prompting with string PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  Lisp_Object val;
  do {
    val = Fcompleting_read (prompt, Vobarray, Qcoding_system_vector,
			    Qt, Qnil, Qnil);
  } while (XSTRING (val)->size == 0);
  return (Fintern (val, Qnil));
}

DEFUN ("read-coding-system", Fread_coding_system, Sread_coding_system, 1, 1, 0,
  "Read a coding system or nil from the minibuffer, prompting with string PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  Lisp_Object val = Fcompleting_read (prompt, Vobarray, Qcoding_system_p,
				      Qt, Qnil, Qnil);
  return (XSTRING (val)->size == 0 ? Qnil : Fintern (val, Qnil));
}

DEFUN ("check-coding-system", Fcheck_coding_system, Scheck_coding_system,
       1, 1, 0,
  "Check validity of CODING-SYSTEM.\n\
If valid, return CODING-SYSTEM, else `coding-system-error' is signaled.\n\
CODING-SYSTEM is valid if it is a symbol and has \"coding-system\" property.\n\
The value of property should be a vector of length 5.")
  (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system, 0);
  if (!NILP (Fcoding_system_p (coding_system)))
    return coding_system;
  while (1)
    Fsignal (Qcoding_system_error, coding_system);
}

DEFUN ("detect-coding-region", Fdetect_coding_region, Sdetect_coding_region,
       2, 2, 0,
  "Detect coding-system of the text in the region between START and END.\n\
Return a list of possible coding-systems ordered by priority.\n\
If only ASCII characters are found, it returns `automatic-conversion'\n\
 or its subsidiary coding-system according to a detected end-of-line format.")
  (b, e)
     Lisp_Object b, e;
{
  int coding_mask, eol_type;
  Lisp_Object val;
  int beg, end;

  validate_region (&b, &e);
  beg = XINT (b), end = XINT (e);
  if (beg < GPT && end >= GPT) move_gap (end);

  coding_mask = detect_coding_mask (POS_ADDR (beg), end - beg);
  eol_type  = detect_eol_type (POS_ADDR (beg), end - beg);

  if (coding_mask == CODING_CATEGORY_MASK_ANY)
    {
      val = intern ("automatic-conversion");
      if (eol_type != CODING_EOL_AUTOMATIC)
	{
	  Lisp_Object val2 = Fget (val, Qeol_type);
	  if (VECTORP (val2))
	    val = XVECTOR (val2)->contents[eol_type];
	}
    }
  else
    {
      Lisp_Object val2;

      /* At first, gather possible coding-systems in VAL in a reverse
	 order.  */
      val = Qnil;
      for (val2 = Vcoding_category_list;
	   !NILP (val2);
	   val2 = XCONS (val2)->cdr)
	{
	  int idx
	    = XFASTINT (Fget (XCONS (val2)->car, Qcoding_category_index));
	  if (coding_mask & (1 << idx))
	    val = Fcons (Fsymbol_value (XCONS (val2)->car), val);
	}

      /* Then, change the order of the list, while getting subsidiary
	 coding-systems.  */
      val2 = val;
      val = Qnil;
      for (; !NILP (val2); val2 = XCONS (val2)->cdr)
	{
	  if (eol_type == CODING_EOL_AUTOMATIC)
	    val = Fcons (XCONS (val2)->car, val);
	  else
	    {
	      Lisp_Object val3 = Fget (XCONS (val2)->car, Qeol_type);
	      if (VECTORP (val3))
		val = Fcons (XVECTOR (val3)->contents[eol_type], val);
	      else
		val = Fcons (XCONS (val2)->car, val);
	    }
	}
    }

  return val;
}

/* Scan text in the region between *BEGP and *ENDP, skip characters
   which we never have to encode to (iff ENCODEP is 1) or decode from
   coding system CODING at the head and tail, then set BEGP and ENDP
   to the addresses of start and end of the text we actually convert.  */

void
shrink_conversion_area (begp, endp, coding, encodep)
     unsigned char **begp, **endp;
     struct coding_system *coding;
     int encodep;
{
  register unsigned char *beg_addr = *begp, *end_addr = *endp;

  if (coding->eol_type != CODING_EOL_LF
      && coding->eol_type != CODING_EOL_AUTOMATIC)
    /* Since we anyway have to convert end-of-line format, it is not
       worth skipping at most 100 bytes or so.  */
    return;

  if (encodep)			/* for encoding */
    {
      switch (coding->type)
	{
	case coding_type_no_conversion:
	case coding_type_internal:
	case coding_type_automatic:
	  /* We need no conversion.  */
	  *begp = *endp;
	  return;
	case coding_type_ccl:
	  /* We can't skip any data.  */
	  return;
	case coding_type_iso2022:
	  if (coding->flags & CODING_FLAG_ISO_DESIGNATE_AT_BOL)
	    {
	      unsigned char *bol = beg_addr; 
	      while (beg_addr < end_addr && *beg_addr < 0x80)
		{
		  beg_addr++;
		  if (*(beg_addr - 1) == '\n')
		    bol = beg_addr;
		}
	      beg_addr = bol;
	      goto label_skip_tail;
	    }
	  /* fall down ... */
	default:
	  /* We can skip all ASCII characters at the head and tail.  */
	  while (beg_addr < end_addr && *beg_addr < 0x80) beg_addr++;
	label_skip_tail:
	  while (beg_addr < end_addr && *(end_addr - 1) < 0x80) end_addr--;
	  break;
	}
    }
  else				/* for decoding */
    {
      switch (coding->type)
	{
	case coding_type_no_conversion:
	  /* We need no conversion.  */
	  *begp = *endp;
	  return;
	case coding_type_internal:
	  if (coding->eol_type == CODING_EOL_LF)
	    {
	      /* We need no conversion.  */
	      *begp = *endp;
	      return;
	    }
	  /* We can skip all but carriage-return.  */
	  while (beg_addr < end_addr && *beg_addr != '\r') beg_addr++;
	  while (beg_addr < end_addr && *(end_addr - 1) != '\r') end_addr--;
	  break;
	case coding_type_sjis:
	case coding_type_big5:
	  /* We can skip all ASCII characters at the head.  */
	  while (beg_addr < end_addr && *beg_addr < 0x80) beg_addr++;
	  /* We can skip all ASCII characters at the tail except for
	     the second byte of SJIS or BIG5 code.  */
	  while (beg_addr < end_addr && *(end_addr - 1) < 0x80) end_addr--;
	  if (end_addr != *endp)
	    end_addr++;
	  break;
	case coding_type_ccl:
	  /* We can't skip any data.  */
	  return;
	default:		/* i.e. case coding_type_iso2022: */
	  {
	    unsigned char c;

	    /* We can skip all ASCII characters except for a few
	       control codes at the head.  */
	    while (beg_addr < end_addr && (c = *beg_addr) < 0x80
		   && c != ISO_CODE_CR && c != ISO_CODE_SO
		   && c != ISO_CODE_SI && c != ISO_CODE_ESC)
	      beg_addr++;
	  }
	  break;
	}
    }
  *begp = beg_addr;
  *endp = end_addr;
  return;
}

/* Encode to (iff ENCODEP is 1) or decode form coding system CODING a
   text between B and E.  B and E are buffer position.  */

Lisp_Object
code_convert_region (b, e, coding, encodep)
     Lisp_Object b, e;
     struct coding_system *coding;
     int encodep;
{
  int beg, end, len, consumed, produced;
  char *buf;
  unsigned char *begp, *endp;
  int pos = PT;

  validate_region (&b, &e);
  beg = XINT (b), end = XINT (e);
  if (beg < GPT && end >= GPT)
    move_gap (end);

  if (encodep && !NILP (coding->pre_write_conversion))
    {
      /* We must call a pre-conversion function which may put a new
	 text to be converted in a new buffer.  */
      struct buffer *old = current_buffer, *new;

      TEMP_SET_PT (beg);
      call2 (coding->pre_write_conversion, b, e);
      if (old != current_buffer)
	{
	  /* Replace the original text by the text just generated.  */
	  len = ZV - BEGV;
	  new = current_buffer;
	  set_buffer_internal (old);
	  del_range (beg, end);
	  insert_from_buffer (new, 1, len, 0);
	  end = beg + len;
	}
    }

  /* We may be able to shrink the conversion region.  */
  begp = POS_ADDR (beg); endp = begp + (end - beg);
  shrink_conversion_area (&begp, &endp, coding, encodep);

  if (begp == endp)
    /* We need no conversion.  */
    len = end - beg;
  else
    {
      beg += begp - POS_ADDR (beg);
      end =  beg + (endp - begp);

      if (encodep)
	len = encoding_buffer_size (coding, end - beg);
      else
	len = decoding_buffer_size (coding, end - beg);
      buf = get_conversion_buffer (len);

      coding->last_block = 1;
      produced = (encodep
		  ? encode_coding (coding, POS_ADDR (beg), buf, end - beg, len,
				   &consumed)
		  : decode_coding (coding, POS_ADDR (beg), buf, end - beg, len,
				   &consumed));

      len = produced + (beg - XINT (b)) + (XINT (e) - end);

      TEMP_SET_PT (beg);
      insert (buf, produced);
      del_range (PT, PT + end - beg);
      if (pos >= end)
	pos = PT + (pos - end);
      else if (pos > beg)
	pos = beg;
      TEMP_SET_PT (pos);
  }

  if (!encodep && !NILP (coding->post_read_conversion))
    {
      /* We must call a post-conversion function which may alter
	 the text just converted.  */
      Lisp_Object insval;

      beg = XINT (b);
      TEMP_SET_PT (beg);
      insval = call1 (coding->post_read_conversion, make_number (len));
      CHECK_NUMBER (insval, 0);
      len = XINT (insval);
    }

  return make_number (len);
}

Lisp_Object
code_convert_string (str, coding, encodep, nocopy)
     Lisp_Object str, nocopy;
     struct coding_system *coding;
     int encodep;
{
  int len, consumed, produced;
  char *buf;
  unsigned char *begp, *endp;
  int head_skip, tail_skip;
  struct gcpro gcpro1;

  if (encodep && !NILP (coding->pre_write_conversion)
      || !encodep && !NILP (coding->post_read_conversion))
    {
      /* Since we have to call Lisp functions which assume target text
         is in a buffer, after setting a temporary buffer, call
         code_convert_region.  */
      int count = specpdl_ptr - specpdl;
      int len = XSTRING (str)->size;
      Lisp_Object result;
      struct buffer *old = current_buffer;

      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      temp_output_buffer_setup (" *code-converting-work*");
      set_buffer_internal (XBUFFER (Vstandard_output));
      insert_from_string (str, 0, len, 0);
      code_convert_region (make_number (BEGV), make_number (ZV),
			   coding, encodep);
      result = make_buffer_string (BEGV, ZV, 0);
      set_buffer_internal (old);
      return unbind_to (count, result);
    }

  /* We may be able to shrink the conversion region.  */
  begp = XSTRING (str)->data;
  endp = begp + XSTRING (str)->size;
  shrink_conversion_area (&begp, &endp, coding, encodep);

  if (begp == endp)
    /* We need no conversion.  */
    return (NILP (nocopy) ? Fcopy_sequence (str) : str);

  head_skip = begp - XSTRING (str)->data;
  tail_skip = XSTRING (str)->size - head_skip - (endp - begp);

  GCPRO1 (str);

  if (encodep)
    len = encoding_buffer_size (coding, endp - begp);
  else
    len = decoding_buffer_size (coding, endp - begp);
  buf = get_conversion_buffer (len + head_skip + tail_skip);

  bcopy (XSTRING (str)->data, buf, head_skip);
  coding->last_block = 1;
  produced = (encodep
	      ? encode_coding (coding, XSTRING (str)->data + head_skip,
			       buf + head_skip, endp - begp, len, &consumed)
	      : decode_coding (coding, XSTRING (str)->data + head_skip,
			       buf + head_skip, endp - begp, len, &consumed));
  bcopy (XSTRING (str)->data + head_skip + (endp - begp),
	 buf + head_skip + produced,
	 tail_skip);

  UNGCPRO;

  return make_string (buf, head_skip + produced + tail_skip);
}

DEFUN ("decode-coding-region", Fdecode_coding_region, Sdecode_coding_region,
       3, 3, "r\nzCoding system: ",
  "Decode current region by specified coding system.\n\
When called from a program, takes three arguments:\n\
START, END, and CODING-SYSTEM.  START END are buffer positions.\n\
Return length of decoded text.")
  (b, e, coding_system)
     Lisp_Object b, e, coding_system;
{
  struct coding_system coding;

  CHECK_NUMBER_COERCE_MARKER (b, 0);
  CHECK_NUMBER_COERCE_MARKER (e, 1);
  CHECK_SYMBOL (coding_system, 2);

  if (NILP (coding_system))
    return make_number (XFASTINT (e) - XFASTINT (b));
  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding-system: %s", XSYMBOL (coding_system)->name->data);

  return code_convert_region (b, e, &coding, 0);
}

DEFUN ("encode-coding-region", Fencode_coding_region, Sencode_coding_region,
       3, 3, "r\nzCoding system: ",
  "Encode current region by specified coding system.\n\
When called from a program, takes three arguments:\n\
START, END, and CODING-SYSTEM.  START END are buffer positions.\n\
Return length of encoded text.")
  (b, e, coding_system)
     Lisp_Object b, e, coding_system;
{
  struct coding_system coding;

  CHECK_NUMBER_COERCE_MARKER (b, 0);
  CHECK_NUMBER_COERCE_MARKER (e, 1);
  CHECK_SYMBOL (coding_system, 2);

  if (NILP (coding_system))
    return make_number (XFASTINT (e) - XFASTINT (b));
  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding-system: %s", XSYMBOL (coding_system)->name->data);

  return code_convert_region (b, e, &coding, 1);
}

DEFUN ("decode-coding-string", Fdecode_coding_string, Sdecode_coding_string,
       2, 3, 0,
  "Decode STRING which is encoded in CODING-SYSTEM, and return the result.\n\
Optional arg NOCOPY non-nil means return STRING itself if there's no need\n\
of decoding.")
  (string, coding_system, nocopy)
     Lisp_Object string, coding_system, nocopy;
{
  struct coding_system coding;

  CHECK_STRING (string, 0);
  CHECK_SYMBOL (coding_system, 1);

  if (NILP (coding_system))
    return (NILP (nocopy) ? Fcopy_sequence (string) : string);
  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding-system: %s", XSYMBOL (coding_system)->name->data);

  return code_convert_string (string, &coding, 0, nocopy);
}

DEFUN ("encode-coding-string", Fencode_coding_string, Sencode_coding_string,
       2, 3, 0,
  "Encode STRING to CODING-SYSTEM, and return the result.\n\
Optional arg NOCOPY non-nil means return STRING itself if there's no need\n\
of encoding.")
  (string, coding_system, nocopy)
     Lisp_Object string, coding_system, nocopy;
{
  struct coding_system coding;

  CHECK_STRING (string, 0);
  CHECK_SYMBOL (coding_system, 1);

  if (NILP (coding_system))
    return (NILP (nocopy) ? Fcopy_sequence (string) : string);
  if (setup_coding_system (Fcheck_coding_system (coding_system), &coding) < 0)
    error ("Invalid coding-system: %s", XSYMBOL (coding_system)->name->data);

  return code_convert_string (string, &coding, 1, nocopy);
}

DEFUN ("decode-sjis-char", Fdecode_sjis_char, Sdecode_sjis_char, 1, 1, 0,
  "Decode a JISX0208 character of shift-jis encoding.\n\
CODE is the character code in SJIS.\n\
Return the corresponding character.")
  (code)
     Lisp_Object code;
{
  unsigned char c1, c2, s1, s2;
  Lisp_Object val;

  CHECK_NUMBER (code, 0);
  s1 = (XFASTINT (code)) >> 8, s2 = (XFASTINT (code)) & 0xFF;
  DECODE_SJIS (s1, s2, c1, c2);
  XSETFASTINT (val, MAKE_NON_ASCII_CHAR (charset_jisx0208, c1, c2));
  return val;
}

DEFUN ("encode-sjis-char", Fencode_sjis_char, Sencode_sjis_char, 1, 1, 0,
  "Encode a JISX0208 character CHAR to SJIS coding-system.\n\
Return the corresponding character code in SJIS.")
  (ch)
     Lisp_Object ch;
{
  int charset;
  unsigned char c1, c2, s1, s2;
  Lisp_Object val;

  CHECK_NUMBER (ch, 0);
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  if (charset == charset_jisx0208)
    {
      ENCODE_SJIS (c1, c2, s1, s2);
      XSETFASTINT (val, ((int)s1 << 8) | s2);
    }
  else
    XSETFASTINT (val, 0);
  return val;
}

DEFUN ("decode-big5-char", Fdecode_big5_char, Sdecode_big5_char, 1, 1, 0,
  "Decode a Big5 character CODE of BIG5 coding-system.\n\
CODE is the character code in BIG5.\n\
Return the corresponding character.")
  (code)
     Lisp_Object code;
{
  int charset;
  unsigned char b1, b2, c1, c2;
  Lisp_Object val;

  CHECK_NUMBER (code, 0);
  b1 = (XFASTINT (code)) >> 8, b2 = (XFASTINT (code)) & 0xFF;
  DECODE_BIG5 (b1, b2, charset, c1, c2);
  XSETFASTINT (val, MAKE_NON_ASCII_CHAR (charset, c1, c2));
  return val;
}

DEFUN ("encode-big5-char", Fencode_big5_char, Sencode_big5_char, 1, 1, 0,
  "Encode the Big5 character CHAR to BIG5 coding-system.\n\
Return the corresponding character code in Big5.")
  (ch)
     Lisp_Object ch;
{
  int charset;
  unsigned char c1, c2, b1, b2;
  Lisp_Object val;

  CHECK_NUMBER (ch, 0);
  SPLIT_CHAR (XFASTINT (ch), charset, c1, c2);
  if (charset == charset_big5_1 || charset == charset_big5_2)
    {
      ENCODE_BIG5 (charset, c1, c2, b1, b2);
      XSETFASTINT (val, ((int)b1 << 8) | b2);
    }
  else
    XSETFASTINT (val, 0);
  return val;
}

DEFUN ("set-terminal-coding-system",
       Fset_terminal_coding_system, Sset_terminal_coding_system, 1, 1,
       "zCoding-system for terminal display: ",
  "Set coding-system of your terminal to CODING-SYSTEM.\n\
All outputs to terminal are encoded to this coding-system.")
  (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system, 0);
  setup_coding_system (Fcheck_coding_system (coding_system), &terminal_coding);
  update_mode_lines++;
  if (!NILP (Finteractive_p ()))
    Fredraw_display ();
  return Qnil;
}

DEFUN ("terminal-coding-system",
       Fterminal_coding_system, Sterminal_coding_system, 0, 0, 0,
  "Return coding-system of your terminal.")
  ()
{
  return terminal_coding.symbol;
}

DEFUN ("set-keyboard-coding-system",
       Fset_keyboard_coding_system, Sset_keyboard_coding_system, 1, 1,
       "zCoding-system for keyboard input: ",
  "Set coding-system of what is sent from terminal keyboard to CODING-SYSTEM.\n\
All inputs from terminal are decoded from this coding-system.")
  (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system, 0);
  setup_coding_system (Fcheck_coding_system (coding_system), &keyboard_coding);
  return Qnil;
}

DEFUN ("keyboard-coding-system",
       Fkeyboard_coding_system, Skeyboard_coding_system, 0, 0, 0,
  "Return coding-system of what is sent from terminal keyboard.")
  ()
{
  return keyboard_coding.symbol;
}


DEFUN ("find-coding-system", Ffind_coding_system, Sfind_coding_system,
       1, MANY, 0,
  "Return a cons of coding systems for I/O primitive OPERATION.\n\
Remaining arguments are for OPERATION.\n\
OPERATION is one of the following Emacs I/O primitives:\n\
  For file I/O, insert-file-contents or write-region.\n\
  For process I/O, call-process, call-process-region, or start-process.\n\
  For network I/O, open-network-stream.\n\
For each OPERATION, TARGET is selected from the arguments as below:\n\
  For file I/O, TARGET is a file name.\n\
  For process I/O, TARGET is a process name.\n\
  For network I/O, TARGET is a service name or a port number\n\
\n\
The return value is a cons of coding systems for decoding and encoding\n\
registered in nested alist `coding-system-alist' (which see) at a slot\n\
corresponding to OPERATION and TARGET.\n\
If a function symbol is at the slot, return a result of the function call.\n\
The function is called with one argument, a list of all the arguments.")
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
    error ("Invalid first arguement");
  if (nargs < 1 + XINT (target_idx))
    error ("Too few arguments for operation: %s",
	   XSYMBOL (operation)->name->data);
  target = args[XINT (target_idx) + 1];
  if (!(STRINGP (target)
	|| (EQ (operation, Qopen_network_stream) && INTEGERP (target))))
    error ("Invalid %dth argument", XINT (target_idx) + 1);

  chain = Fassq (operation, Vcoding_system_alist);
  if (NILP (chain))
    return Qnil;

  for (chain = XCONS (chain)->cdr; CONSP (chain); chain = XCONS (chain)->cdr)
    {
      Lisp_Object elt = XCONS (chain)->car;

      if (CONSP (elt)
	  && ((STRINGP (target)
	       && STRINGP (XCONS (elt)->car)
	       && fast_string_match (XCONS (elt)->car, target) >= 0)
	      || (INTEGERP (target) && EQ (target, XCONS (elt)->car))))
	return (CONSP (val = XCONS (elt)->cdr)
		? val
		: ((SYMBOLP (val) && Fboundp (val)
		    ? call2 (val, Flist (nargs, args))
		    : Qnil)));
    }
  return Qnil;
}

#endif /* emacs */


/*** 8. Post-amble ***/

init_coding_once ()
{
  int i;

  /* Emacs internal format specific initialize routine.  */ 
  for (i = 0; i <= 0x20; i++)
    emacs_code_class[i] = EMACS_control_code;
  emacs_code_class[0x0A] = EMACS_linefeed_code;
  emacs_code_class[0x0D] = EMACS_carriage_return_code;
  for (i = 0x21 ; i < 0x7F; i++)
    emacs_code_class[i] = EMACS_ascii_code;
  emacs_code_class[0x7F] = EMACS_control_code;
  emacs_code_class[0x80] = EMACS_leading_code_composition;
  for (i = 0x81; i < 0xFF; i++)
    emacs_code_class[i] = EMACS_invalid_code;
  emacs_code_class[LEADING_CODE_PRIVATE_11] = EMACS_leading_code_3;
  emacs_code_class[LEADING_CODE_PRIVATE_12] = EMACS_leading_code_3;
  emacs_code_class[LEADING_CODE_PRIVATE_21] = EMACS_leading_code_4;
  emacs_code_class[LEADING_CODE_PRIVATE_22] = EMACS_leading_code_4;

  /* ISO2022 specific initialize routine.  */
  for (i = 0; i < 0x20; i++)
    iso_code_class[i] = ISO_control_code;
  for (i = 0x21; i < 0x7F; i++)
    iso_code_class[i] = ISO_graphic_plane_0;
  for (i = 0x80; i < 0xA0; i++)
    iso_code_class[i] = ISO_control_code;
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

  conversion_buffer_size = MINIMUM_CONVERSION_BUFFER_SIZE;
  conversion_buffer = (char *) xmalloc (MINIMUM_CONVERSION_BUFFER_SIZE);

  setup_coding_system (Qnil, &keyboard_coding);
  setup_coding_system (Qnil, &terminal_coding);
}

#ifdef emacs

syms_of_coding ()
{
  Qtarget_idx = intern ("target-idx");
  staticpro (&Qtarget_idx);

  Fput (Qinsert_file_contents, Qtarget_idx, make_number (0));
  Fput (Qwrite_region, Qtarget_idx, make_number (2));

  Qcall_process = intern ("call-process");
  staticpro (&Qcall_process);
  Fput (Qcall_process, Qtarget_idx, make_number (0));

  Qcall_process_region = intern ("call-process-region");
  staticpro (&Qcall_process_region);
  Fput (Qcall_process_region, Qtarget_idx, make_number (2));

  Qstart_process = intern ("start-process");
  staticpro (&Qstart_process);
  Fput (Qstart_process, Qtarget_idx, make_number (2));

  Qopen_network_stream = intern ("open-network-stream");
  staticpro (&Qopen_network_stream);
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

  Qcoding_system_vector = intern ("coding-system-vector");
  staticpro (&Qcoding_system_vector);

  Qcoding_system_p = intern ("coding-system-p");
  staticpro (&Qcoding_system_p);

  Qcoding_system_error = intern ("coding-system-error");
  staticpro (&Qcoding_system_error);

  Fput (Qcoding_system_error, Qerror_conditions,
	Fcons (Qcoding_system_error, Fcons (Qerror, Qnil)));
  Fput (Qcoding_system_error, Qerror_message,
	build_string ("Coding-system error"));

  Qcoding_category_index = intern ("coding-category-index");
  staticpro (&Qcoding_category_index);

  {
    int i;
    for (i = 0; i < CODING_CATEGORY_IDX_MAX; i++)
      {
	coding_category_table[i] = intern (coding_category_name[i]);
	staticpro (&coding_category_table[i]);
	Fput (coding_category_table[i], Qcoding_category_index,
	      make_number (i));
      }
  }

  defsubr (&Scoding_system_vector);
  defsubr (&Scoding_system_p);
  defsubr (&Sread_coding_system);
  defsubr (&Sread_non_nil_coding_system);
  defsubr (&Scheck_coding_system);
  defsubr (&Sdetect_coding_region);
  defsubr (&Sdecode_coding_region);
  defsubr (&Sencode_coding_region);
  defsubr (&Sdecode_coding_string);
  defsubr (&Sencode_coding_string);
  defsubr (&Sdecode_sjis_char);
  defsubr (&Sencode_sjis_char);
  defsubr (&Sdecode_big5_char);
  defsubr (&Sencode_big5_char);
  defsubr (&Sset_terminal_coding_system);
  defsubr (&Sterminal_coding_system);
  defsubr (&Sset_keyboard_coding_system);
  defsubr (&Skeyboard_coding_system);
  defsubr (&Sfind_coding_system);

  DEFVAR_LISP ("coding-category-list", &Vcoding_category_list,
    "List of coding-categories (symbols) ordered by priority.");
  {
    int i;

    Vcoding_category_list = Qnil;
    for (i = CODING_CATEGORY_IDX_MAX - 1; i >= 0; i--)
      Vcoding_category_list
	= Fcons (coding_category_table[i], Vcoding_category_list);
  }

  DEFVAR_LISP ("coding-system-for-read", &Vcoding_system_for_read,
    "A variable of internal use only.\n\
If the value is a coding system, it is used for decoding on read operation.\n\
If not, an appropriate element in `coding-system-alist' (which see) is used.");
  Vcoding_system_for_read = Qnil;

  DEFVAR_LISP ("coding-system-for-write", &Vcoding_system_for_write,
    "A variable of internal use only.\n\
If the value is a coding system, it is used for encoding on write operation.\n\
If not, an appropriate element in `coding-system-alist' (which see) is used.");
  Vcoding_system_for_write = Qnil;

  DEFVAR_LISP ("last-coding-system-used", &Vlast_coding_system_used,
    "Coding-system used in the latest file or process I/O.");
  Vlast_coding_system_used = Qnil;

  DEFVAR_LISP ("coding-system-alist", &Vcoding_system_alist,
    "Nested alist to decide a coding system for a specific I/O operation.\n\
The format is ((OPERATION . ((REGEXP . CODING-SYSTEMS) ...)) ...).\n\
\n\
OPERATION is one of the following Emacs I/O primitives:\n\
  For file I/O, insert-file-contents and write-region.\n\
  For process I/O, call-process, call-process-region, and start-process.\n\
  For network I/O, open-network-stream.\n\
In addition, for process I/O, `process-argument' can be specified for\n\
encoding arguments of the process.\n\
\n\
REGEXP is a regular expression matching a target of OPERATION, where\n\
target is a file name for file I/O operations, a process name for\n\
process I/O operations, or a service name for network I/O\n\
operations.  REGEXP might be a port number for network I/O operation.\n\
\n\
CODING-SYSTEMS is a cons of coding systems to encode and decode\n\
character code on OPERATION, or a function symbol returning the cons.\n\
See the documentation of `find-coding-system' for more detail.");
  Vcoding_system_alist = Qnil;

  DEFVAR_INT ("eol-mnemonic-unix", &eol_mnemonic_unix,
    "Mnemonic character indicating UNIX-like end-of-line format (i.e. LF) .");
  eol_mnemonic_unix = '.';

  DEFVAR_INT ("eol-mnemonic-dos", &eol_mnemonic_dos,
    "Mnemonic character indicating DOS-like end-of-line format (i.e. CRLF).");
  eol_mnemonic_dos = ':';

  DEFVAR_INT ("eol-mnemonic-mac", &eol_mnemonic_mac,
    "Mnemonic character indicating MAC-like end-of-line format (i.e. CR).");
  eol_mnemonic_mac = '\'';

  DEFVAR_INT ("eol-mnemonic-undecided", &eol_mnemonic_undecided,
    "Mnemonic character indicating end-of-line format is not yet decided.");
  eol_mnemonic_undecided = '-';

  DEFVAR_LISP ("alternate-charset-table", &Valternate_charset_table,
    "Alist of charsets vs the alternate charsets.\n\
While decoding, if a charset (car part of an element) is found,\n\
decode it as the alternate charset (cdr part of the element).");
  Valternate_charset_table = Qnil;

  DEFVAR_LISP ("charset-revision-table", &Vcharset_revision_alist,
    "Alist of charsets vs revision numbers.\n\
While encoding, if a charset (car part of an element) is found,\n\
designate it with the escape sequence identifing revision (cdr part of the element).");
  Vcharset_revision_alist = Qnil;
}

#endif /* emacs */
