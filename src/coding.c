/* Coding system handler (conversion, detection, and etc).
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

/*** TABLE OF CONTENTS ***

  0. General comments
  1. Preamble
  2. Emacs' internal format (emacs-utf-8) handlers
  3. UTF-8 handlers
  4. UTF-16 handlers
  5. Charset-base coding systems handlers
  6. emacs-mule (old Emacs' internal format) handlers
  7. ISO2022 handlers
  8. Shift-JIS and BIG5 handlers
  9. CCL handlers
  10. C library functions
  11. Emacs Lisp library functions
  12. Postamble

*/

/*** 0. General comments ***


CODING SYSTEM

  Coding system is an object for a encoding mechanism that contains
  information about how to convert byte sequence to character
  sequences and vice versa.  When we say "decode", it means converting
  a byte sequence of a specific coding system into a character
  sequence that is represented by Emacs' internal coding system
  `emacs-utf-8', and when we say "encode", it means converting a
  character sequence of emacs-utf-8 to a byte sequence of a specific
  coding system.

  In Emacs Lisp, a coding system is represented by a Lisp symbol.  In
  C level, a coding system is represented by a vector of attributes
  stored in the hash table Vcharset_hash_table.  The conversion from a
  coding system symbol to attributes vector is done by looking up
  Vcharset_hash_table by the symbol.

  Coding systems are classified into the following types depending on
  the mechanism of encoding.  Here's a brief descrition about type.

  o UTF-8

  o UTF-16

  o Charset-base coding system

  A coding system defined by one or more (coded) character sets.
  Decoding and encoding are done by code converter defined for each
  character set.

  o Old Emacs' internal format (emacs-mule)

  The coding system adopted by an old versions of Emacs (20 and 21).

  o ISO2022-base coding system

  The most famous coding system for multiple character sets.  X's
  Compound Text, various EUCs (Extended Unix Code), and coding systems
  used in the Internet communication such as ISO-2022-JP are all
  variants of ISO2022.

  o SJIS (or Shift-JIS or MS-Kanji-Code)
   
  A coding system to encode character sets: ASCII, JISX0201, and
  JISX0208.  Widely used for PC's in Japan.  Details are described in
  section 8.

  o BIG5

  A coding system to encode character sets: ASCII and Big5.  Widely
  used by Chinese (mainly in Taiwan and Hong Kong).  Details are
  described in section 8.  In this file, when we write "big5" (all
  lowercase), we mean the coding system, and when we write "Big5"
  (capitalized), we mean the character set.

  o CCL

  If a user wants to decode/encode a text encoded in a coding system
  not listed above, he can supply a decoder and an encoder for it in
  CCL (Code Conversion Language) programs.  Emacs executes the CCL
  program while decoding/encoding.

  o Raw-text

  A coding system for a text containing raw eight-bit data.  Emacs
  treat each byte of source text as a character (except for
  end-of-line conversion).

  o No-conversion

  Like raw text, but don't do end-of-line conversion.


END-OF-LINE FORMAT

  How end-of-line of a text is encoded depends on a system.  For
  instance, Unix's format is just one byte of LF (line-feed) code,
  whereas DOS's format is two-byte sequence of `carriage-return' and
  `line-feed' codes.  MacOS's format is usually one byte of
  `carriage-return'.

  Since text characters encoding and end-of-line encoding are
  independent, any coding system described above can take any format
  of end-of-line (except for no-conversion).

STRUCT CODING_SYSTEM

  Before using a coding system for code conversion (i.e. decoding and
  encoding), we setup a structure of type `struct coding_system'.
  This structure keeps various information about a specific code
  conversion (e.g.  the location of source and destination data).

*/

/* COMMON MACROS */


/*** GENERAL NOTES on `detect_coding_XXX ()' functions ***

  These functions check if a byte sequence specified as a source in
  CODING conforms to the format of XXX.  Return 1 if the data contains
  a byte sequence which can be decoded into non-ASCII characters by
  the coding system.  Otherwize (i.e. the data contains only ASCII
  characters or invalid sequence) return 0.

  It also resets some bits of an integer pointed by MASK.  The macros
  CATEGORY_MASK_XXX specifies each bit of this integer.

  Below is the template of these functions.  */

#if 0
static int
detect_coding_XXX (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int c;
  int found = 0;
  ...;

  while (1)
    {
      /* Get one byte from the source.  If the souce is exausted, jump
	 to no_more_source:.  */
      ONE_MORE_BYTE (c);
      /* Check if it conforms to XXX.  If not, break the loop.  */
    }
  /* As the data is invalid for XXX, reset a proper bits.  */
  *mask &= ~CODING_CATEGORY_XXX;
  return 0;
 no_more_source:
  /* The source exausted.  */
  if (!found)
    /* ASCII characters only. */
    return 0;
  /* Some data should be decoded into non-ASCII characters.  */
  *mask &= CODING_CATEGORY_XXX;
  return 1;
}
#endif

/*** GENERAL NOTES on `decode_coding_XXX ()' functions ***

  These functions decode a byte sequence specified as a source by
  CODING.  The resulting multibyte text goes to a place pointed to by
  CODING->charbuf, the length of which should not exceed
  CODING->charbuf_size;

  These functions set the information of original and decoded texts in
  CODING->consumed, CODING->consumed_char, and CODING->charbuf_used.
  They also set CODING->result to one of CODING_RESULT_XXX indicating
  how the decoding is finished.

  Below is the template of these functions.  */

#if 0
static void
decode_coding_XXXX (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code, or
     when there's no room in CHARBUF for a decoded character.  */
  unsigned char *src_base;
  /* A buffer to produce decoded characters.  */
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int multibytep = coding->src_multibyte;

  while (1)
    {
      src_base = src;
      if (charbuf < charbuf_end)
	/* No more room to produce a decoded character.  */
	break;
      ONE_MORE_BYTE (c);
      /* Decode it. */
    }

 no_more_source:
  if (src_base < src_end
      && coding->mode & CODING_MODE_LAST_BLOCK)
    /* If the source ends by partial bytes to construct a character,
       treat them as eight-bit raw data.  */
    while (src_base < src_end && charbuf < charbuf_end)
      *charbuf++ = *src_base++;
  /* Remember how many bytes and characters we consumed.  If the
     source is multibyte, the bytes and chars are not identical.  */
  coding->consumed = coding->consumed_char = src_base - coding->source;
  /* Remember how many characters we produced.  */
  coding->charbuf_used = charbuf - coding->charbuf;
}
#endif

/*** GENERAL NOTES on `encode_coding_XXX ()' functions ***

  These functions encode SRC_BYTES length text at SOURCE of Emacs'
  internal multibyte format by CODING.  The resulting byte sequence
  goes to a place pointed to by DESTINATION, the length of which
  should not exceed DST_BYTES.

  These functions set the information of original and encoded texts in
  the members produced, produced_char, consumed, and consumed_char of
  the structure *CODING.  They also set the member result to one of
  CODING_RESULT_XXX indicating how the encoding finished.

  DST_BYTES zero means that source area and destination area are
  overlapped, which means that we can produce a encoded text until it
  reaches at the head of not-yet-encoded source text.

  Below is a template of these functions.  */
#if 0
static void
encode_coding_XXX (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf->charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  unsigned char *adjusted_dst_end = dst_end - _MAX_BYTES_PRODUCED_IN_LOOP_;
  int produced_chars = 0;

  for (; charbuf < charbuf_end && dst < adjusted_dst_end; charbuf++)
    {
      int c = *charbuf;
      /* Encode C into DST, and increment DST.  */
    }
 label_no_more_destination:
  /* How many chars and bytes we produced.  */
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
}
#endif


/*** 1. Preamble ***/

#include <config.h>
#include <stdio.h>

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "ccl.h"
#include "composite.h"
#include "coding.h"
#include "window.h"

Lisp_Object Vcoding_system_hash_table;

Lisp_Object Qcoding_system, Qcoding_aliases, Qeol_type;
Lisp_Object Qunix, Qdos, Qmac;
Lisp_Object Qbuffer_file_coding_system;
Lisp_Object Qpost_read_conversion, Qpre_write_conversion;
Lisp_Object Qdefault_char;
Lisp_Object Qno_conversion, Qundecided;
Lisp_Object Qcharset, Qiso_2022, Qutf_8, Qutf_16, Qshift_jis, Qbig5;
Lisp_Object Qutf_16_be_nosig, Qutf_16_be, Qutf_16_le_nosig, Qutf_16_le;
Lisp_Object Qsignature, Qendian, Qbig, Qlittle;
Lisp_Object Qcoding_system_history;
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

Lisp_Object Vfile_coding_system_alist;
Lisp_Object Vprocess_coding_system_alist;
Lisp_Object Vnetwork_coding_system_alist;

Lisp_Object Vlocale_coding_system;

#endif /* emacs */

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
static Lisp_Object Vcharset_revision_table;

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

/* Two special coding systems.  */
Lisp_Object Vsjis_coding_system;
Lisp_Object Vbig5_coding_system;


static int detect_coding_utf_8 P_ ((struct coding_system *, int *));
static void decode_coding_utf_8 P_ ((struct coding_system *));
static int encode_coding_utf_8 P_ ((struct coding_system *));

static int detect_coding_utf_16 P_ ((struct coding_system *, int *));
static void decode_coding_utf_16 P_ ((struct coding_system *));
static int encode_coding_utf_16 P_ ((struct coding_system *));

static int detect_coding_iso_2022 P_ ((struct coding_system *, int *));
static void decode_coding_iso_2022 P_ ((struct coding_system *));
static int encode_coding_iso_2022 P_ ((struct coding_system *));

static int detect_coding_emacs_mule P_ ((struct coding_system *, int *));
static void decode_coding_emacs_mule P_ ((struct coding_system *));
static int encode_coding_emacs_mule P_ ((struct coding_system *));

static int detect_coding_sjis P_ ((struct coding_system *, int *));
static void decode_coding_sjis P_ ((struct coding_system *));
static int encode_coding_sjis P_ ((struct coding_system *));

static int detect_coding_big5 P_ ((struct coding_system *, int *));
static void decode_coding_big5 P_ ((struct coding_system *));
static int encode_coding_big5 P_ ((struct coding_system *));

static int detect_coding_ccl P_ ((struct coding_system *, int *));
static void decode_coding_ccl P_ ((struct coding_system *));
static int encode_coding_ccl P_ ((struct coding_system *));

static void decode_coding_raw_text P_ ((struct coding_system *));
static int encode_coding_raw_text P_ ((struct coding_system *));


/* ISO2022 section */

#define CODING_ISO_INITIAL(coding, reg)			\
  (XINT (AREF (AREF (CODING_ID_ATTRS ((coding)->id),	\
		     coding_attr_iso_initial),		\
	       reg)))


#define CODING_ISO_REQUEST(coding, charset_id)	\
  ((charset_id <= (coding)->max_charset_id	\
    ? (coding)->safe_charsets[charset_id]	\
    : -1))


#define CODING_ISO_FLAGS(coding)	\
  ((coding)->spec.iso_2022.flags)
#define CODING_ISO_DESIGNATION(coding, reg)	\
  ((coding)->spec.iso_2022.current_designation[reg])
#define CODING_ISO_INVOCATION(coding, plane)	\
  ((coding)->spec.iso_2022.current_invocation[plane])
#define CODING_ISO_SINGLE_SHIFTING(coding)	\
  ((coding)->spec.iso_2022.single_shifting)
#define CODING_ISO_BOL(coding)	\
  ((coding)->spec.iso_2022.bol)
#define CODING_ISO_INVOKED_CHARSET(coding, plane)	\
  CODING_ISO_DESIGNATION ((coding), CODING_ISO_INVOCATION ((coding), (plane)))

/* Control characters of ISO2022.  */
			/* code */	/* function */
#define ISO_CODE_LF	0x0A		/* line-feed */
#define ISO_CODE_CR	0x0D		/* carriage-return */
#define ISO_CODE_SO	0x0E		/* shift-out */
#define ISO_CODE_SI	0x0F		/* shift-in */
#define ISO_CODE_SS2_7	0x19		/* single-shift-2 for 7-bit code */
#define ISO_CODE_ESC	0x1B		/* escape */
#define ISO_CODE_SS2	0x8E		/* single-shift-2 */
#define ISO_CODE_SS3	0x8F		/* single-shift-3 */
#define ISO_CODE_CSI	0x9B		/* control-sequence-introducer */

/* All code (1-byte) of ISO2022 is classified into one of the
   followings.  */
enum iso_code_class_type
  {
    ISO_control_0,		/* Control codes in the range
				   0x00..0x1F and 0x7F, except for the
				   following 5 codes.  */
    ISO_carriage_return,	/* ISO_CODE_CR (0x0D) */
    ISO_shift_out,		/* ISO_CODE_SO (0x0E) */
    ISO_shift_in,		/* ISO_CODE_SI (0x0F) */
    ISO_single_shift_2_7,	/* ISO_CODE_SS2_7 (0x19) */
    ISO_escape,			/* ISO_CODE_SO (0x1B) */
    ISO_control_1,		/* Control codes in the range
				   0x80..0x9F, except for the
				   following 3 codes.  */
    ISO_single_shift_2,		/* ISO_CODE_SS2 (0x8E) */
    ISO_single_shift_3,		/* ISO_CODE_SS3 (0x8F) */
    ISO_control_sequence_introducer, /* ISO_CODE_CSI (0x9B) */
    ISO_0x20_or_0x7F,		/* Codes of the values 0x20 or 0x7F.  */
    ISO_graphic_plane_0,	/* Graphic codes in the range 0x21..0x7E.  */
    ISO_0xA0_or_0xFF,		/* Codes of the values 0xA0 or 0xFF.  */
    ISO_graphic_plane_1		/* Graphic codes in the range 0xA1..0xFE.  */
  };

/** The macros CODING_ISO_FLAG_XXX defines a flag bit of the
    `iso-flags' attribute of an iso2022 coding system.  */

/* If set, produce long-form designation sequence (e.g. ESC $ ( A)
   instead of the correct short-form sequence (e.g. ESC $ A).  */
#define CODING_ISO_FLAG_LONG_FORM	0x0001

/* If set, reset graphic planes and registers at end-of-line to the
   initial state.  */
#define CODING_ISO_FLAG_RESET_AT_EOL	0x0002

/* If set, reset graphic planes and registers before any control
   characters to the initial state.  */
#define CODING_ISO_FLAG_RESET_AT_CNTL	0x0004

/* If set, encode by 7-bit environment.  */
#define CODING_ISO_FLAG_SEVEN_BITS	0x0008

/* If set, use locking-shift function.  */
#define CODING_ISO_FLAG_LOCKING_SHIFT	0x0010

/* If set, use single-shift function.  Overwrite
   CODING_ISO_FLAG_LOCKING_SHIFT.  */
#define CODING_ISO_FLAG_SINGLE_SHIFT	0x0020

/* If set, use designation escape sequence.  */
#define CODING_ISO_FLAG_DESIGNATION	0x0040

/* If set, produce revision number sequence.  */
#define CODING_ISO_FLAG_REVISION	0x0080

/* If set, produce ISO6429's direction specifying sequence.  */
#define CODING_ISO_FLAG_DIRECTION	0x0100

/* If set, assume designation states are reset at beginning of line on
   output.  */
#define CODING_ISO_FLAG_INIT_AT_BOL	0x0200

/* If set, designation sequence should be placed at beginning of line
   on output.  */
#define CODING_ISO_FLAG_DESIGNATE_AT_BOL 0x0400

/* If set, do not encode unsafe charactes on output.  */
#define CODING_ISO_FLAG_SAFE		0x0800

/* If set, extra latin codes (128..159) are accepted as a valid code
   on input.  */
#define CODING_ISO_FLAG_LATIN_EXTRA	0x1000

#define CODING_ISO_FLAG_COMPOSITION	0x2000

#define CODING_ISO_FLAG_EUC_TW_SHIFT	0x4000

#define CODING_ISO_FLAG_FULL_SUPPORT	0x8000

/* A character to be produced on output if encoding of the original
   character is prohibited by CODING_ISO_FLAG_SAFE.  */
#define CODING_INHIBIT_CHARACTER_SUBSTITUTION  '?'


/* UTF-16 section */
#define CODING_UTF_16_BOM(coding)	\
  ((coding)->spec.utf_16.bom)

#define CODING_UTF_16_ENDIAN(coding)	\
  ((coding)->spec.utf_16.endian)

#define CODING_UTF_16_SURROGATE(coding)	\
  ((coding)->spec.utf_16.surrogate)


/* CCL section */
#define CODING_CCL_DECODER(coding)	\
  AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_decoder)
#define CODING_CCL_ENCODER(coding)	\
  AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_encoder)
#define CODING_CCL_VALIDS(coding)					   \
  (XSTRING (AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_valids)) \
   ->data)

/* Index for each coding category in `coding_category_table' */

enum coding_category
  {
    coding_category_iso_7,
    coding_category_iso_7_tight,
    coding_category_iso_8_1,
    coding_category_iso_8_2,
    coding_category_iso_7_else,
    coding_category_iso_8_else,
    coding_category_utf_8,
    coding_category_utf_16_auto,
    coding_category_utf_16_be,
    coding_category_utf_16_le,
    coding_category_utf_16_be_nosig,
    coding_category_utf_16_le_nosig,
    coding_category_charset,
    coding_category_sjis,
    coding_category_big5,
    coding_category_ccl,
    coding_category_emacs_mule,
    /* All above are targets of code detection.  */
    coding_category_raw_text,
    coding_category_undecided,
    coding_category_max
  };

/* Definitions of flag bits used in detect_coding_XXXX.  */
#define CATEGORY_MASK_ISO_7		(1 << coding_category_iso_7)
#define CATEGORY_MASK_ISO_7_TIGHT	(1 << coding_category_iso_7_tight)
#define CATEGORY_MASK_ISO_8_1		(1 << coding_category_iso_8_1)
#define CATEGORY_MASK_ISO_8_2		(1 << coding_category_iso_8_2)
#define CATEGORY_MASK_ISO_7_ELSE	(1 << coding_category_iso_7_else)
#define CATEGORY_MASK_ISO_8_ELSE	(1 << coding_category_iso_8_else)
#define CATEGORY_MASK_UTF_8		(1 << coding_category_utf_8)
#define CATEGORY_MASK_UTF_16_BE		(1 << coding_category_utf_16_be)
#define CATEGORY_MASK_UTF_16_LE		(1 << coding_category_utf_16_le)
#define CATEGORY_MASK_UTF_16_BE_NOSIG	(1 << coding_category_utf_16_be_nosig)
#define CATEGORY_MASK_UTF_16_LE_NOSIG	(1 << coding_category_utf_16_le_nosig)
#define CATEGORY_MASK_CHARSET		(1 << coding_category_charset)
#define CATEGORY_MASK_SJIS		(1 << coding_category_sjis)
#define CATEGORY_MASK_BIG5		(1 << coding_category_big5)
#define CATEGORY_MASK_CCL		(1 << coding_category_ccl)
#define CATEGORY_MASK_EMACS_MULE	(1 << coding_category_emacs_mule)

/* This value is returned if detect_coding_mask () find nothing other
   than ASCII characters.  */
#define CATEGORY_MASK_ANY		\
  (CATEGORY_MASK_ISO_7			\
   | CATEGORY_MASK_ISO_7_TIGHT		\
   | CATEGORY_MASK_ISO_8_1		\
   | CATEGORY_MASK_ISO_8_2		\
   | CATEGORY_MASK_ISO_7_ELSE		\
   | CATEGORY_MASK_ISO_8_ELSE		\
   | CATEGORY_MASK_UTF_8		\
   | CATEGORY_MASK_UTF_16_BE		\
   | CATEGORY_MASK_UTF_16_LE		\
   | CATEGORY_MASK_UTF_16_BE_NOSIG	\
   | CATEGORY_MASK_UTF_16_LE_NOSIG	\
   | CATEGORY_MASK_CHARSET		\
   | CATEGORY_MASK_SJIS			\
   | CATEGORY_MASK_BIG5			\
   | CATEGORY_MASK_CCL			\
   | CATEGORY_MASK_EMACS_MULE)


#define CATEGORY_MASK_ISO_7BIT \
  (CATEGORY_MASK_ISO_7 | CATEGORY_MASK_ISO_7_TIGHT)

#define CATEGORY_MASK_ISO_8BIT \
  (CATEGORY_MASK_ISO_8_1 | CATEGORY_MASK_ISO_8_2)

#define CATEGORY_MASK_ISO_ELSE \
  (CATEGORY_MASK_ISO_7_ELSE | CATEGORY_MASK_ISO_8_ELSE)

#define CATEGORY_MASK_ISO_ESCAPE	\
  (CATEGORY_MASK_ISO_7			\
   | CATEGORY_MASK_ISO_7_TIGHT		\
   | CATEGORY_MASK_ISO_7_ELSE		\
   | CATEGORY_MASK_ISO_8_ELSE)

#define CATEGORY_MASK_ISO	\
  (  CATEGORY_MASK_ISO_7BIT	\
     | CATEGORY_MASK_ISO_8BIT	\
     | CATEGORY_MASK_ISO_ELSE)

#define CATEGORY_MASK_UTF_16		\
  (CATEGORY_MASK_UTF_16_BE		\
   | CATEGORY_MASK_UTF_16_LE		\
   | CATEGORY_MASK_UTF_16_BE_NOSIG	\
   | CATEGORY_MASK_UTF_16_LE_NOSIG)


/* List of symbols `coding-category-xxx' ordered by priority.  This
   variable is exposed to Emacs Lisp.  */
static Lisp_Object Vcoding_category_list;

/* Table of coding categories (Lisp symbols).  This variable is for
   internal use oly.  */
static Lisp_Object Vcoding_category_table;

/* Table of coding-categories ordered by priority.  */
static enum coding_category coding_priorities[coding_category_max];

/* Nth element is a coding context for the coding system bound to the
   Nth coding category.  */
static struct coding_system coding_categories[coding_category_max];

static int detected_mask[coding_category_raw_text] =
  { CATEGORY_MASK_ISO,
    CATEGORY_MASK_ISO,
    CATEGORY_MASK_ISO,
    CATEGORY_MASK_ISO,
    CATEGORY_MASK_ISO,
    CATEGORY_MASK_ISO,
    CATEGORY_MASK_UTF_8,
    CATEGORY_MASK_UTF_16,
    CATEGORY_MASK_UTF_16,
    CATEGORY_MASK_UTF_16,
    CATEGORY_MASK_UTF_16,
    CATEGORY_MASK_UTF_16,
    CATEGORY_MASK_CHARSET,
    CATEGORY_MASK_SJIS,
    CATEGORY_MASK_BIG5,
    CATEGORY_MASK_CCL,
    CATEGORY_MASK_EMACS_MULE
  };

/*** Commonly used macros and functions ***/

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

#define CODING_GET_INFO(coding, attrs, eol_type, charset_list)	\
  do {								\
    attrs = CODING_ID_ATTRS (coding->id);			\
    eol_type = CODING_ID_EOL_TYPE (coding->id);			\
    if (VECTORP (eol_type))					\
      eol_type = Qunix;						\
    charset_list = CODING_ATTR_CHARSET_LIST (attrs);		\
  } while (0)


/* Safely get one byte from the source text pointed by SRC which ends
   at SRC_END, and set C to that byte.  If there are not enough bytes
   in the source, it jumps to `no_more_source'.  The caller
   should declare and set these variables appropriately in advance:
	src, src_end, multibytep
*/

#define ONE_MORE_BYTE(c)					\
  do {								\
    if (src == src_end)						\
      {								\
	if (src_base < src)					\
	  coding->result = CODING_RESULT_INSUFFICIENT_SRC;	\
	goto no_more_source;					\
      }								\
    c = *src++;							\
    if (multibytep && (c & 0x80))				\
      {								\
	if ((c & 0xFE) != 0xC0)					\
	  error ("Undecodable char found");			\
	c = ((c & 1) << 6) | *src++;				\
      }								\
    consumed_chars++;						\
  } while (0)


#define ONE_MORE_BYTE_NO_CHECK(c)		\
  do {						\
    c = *src++;					\
    if (multibytep && (c & 0x80))		\
      {						\
	if ((c & 0xFE) != 0xC0)			\
	  error ("Undecodable char found");	\
	c = ((c & 1) << 6) | *src++;		\
      }						\
  } while (0)


/* Store a byte C in the place pointed by DST and increment DST to the
   next free point, and increment PRODUCED_CHARS.  The caller should
   assure that C is 0..127, and declare and set the variable `dst'
   appropriately in advance.
*/


#define EMIT_ONE_ASCII_BYTE(c)	\
  do {				\
    produced_chars++;		\
    *dst++ = (c);		\
  } while (0)


/* Like EMIT_ONE_ASCII_BYTE byt store two bytes; C1 and C2.  */

#define EMIT_TWO_ASCII_BYTES(c1, c2)	\
  do {					\
    produced_chars += 2;		\
    *dst++ = (c1), *dst++ = (c2);	\
  } while (0)


/* Store a byte C in the place pointed by DST and increment DST to the
   next free point, and increment PRODUCED_CHARS.  If MULTIBYTEP is
   nonzero, store in an appropriate multibyte from.  The caller should
   declare and set the variables `dst' and `multibytep' appropriately
   in advance.  */

#define EMIT_ONE_BYTE(c)		\
  do {					\
    produced_chars++;			\
    if (multibytep)			\
      {					\
	int ch = (c);			\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
      }					\
    else				\
      *dst++ = (c);			\
  } while (0)


/* Like EMIT_ONE_BYTE, but emit two bytes; C1 and C2.  */

#define EMIT_TWO_BYTES(c1, c2)		\
  do {					\
    produced_chars += 2;		\
    if (multibytep)			\
      {					\
	int ch;				\
					\
	ch = (c1);			\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
	ch = (c2);			\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
      }					\
    else				\
      {					\
	*dst++ = (c1);			\
	*dst++ = (c2);			\
      }					\
  } while (0)


#define EMIT_THREE_BYTES(c1, c2, c3)	\
  do {					\
    EMIT_ONE_BYTE (c1);			\
    EMIT_TWO_BYTES (c2, c3);		\
  } while (0)


#define EMIT_FOUR_BYTES(c1, c2, c3, c4)		\
  do {						\
    EMIT_TWO_BYTES (c1, c2);			\
    EMIT_TWO_BYTES (c3, c4);			\
  } while (0)


#define CODING_DECODE_CHAR(coding, src, src_base, src_end, charset, code, c) \
  do {									     \
    charset_map_loaded = 0;						     \
    c = DECODE_CHAR (charset, code);					     \
    if (charset_map_loaded)						     \
      {									     \
	unsigned char *orig = coding->source;				     \
	EMACS_INT offset;						     \
									     \
	coding_set_source (coding);					     \
	offset = coding->source - orig;					     \
	src += offset;							     \
	src_base += offset;						     \
	src_end += offset;						     \
      }									     \
  } while (0)


#define ASSURE_DESTINATION(bytes)				\
  do {								\
    if (dst + (bytes) >= dst_end)				\
      {								\
	int more_bytes = charbuf_end - charbuf + (bytes);	\
								\
	dst = alloc_destination (coding, more_bytes, dst);	\
	dst_end = coding->destination + coding->dst_bytes;	\
      }								\
  } while (0)



static void
coding_set_source (coding)
     struct coding_system *coding;
{
  if (BUFFERP (coding->src_object))
    {
      if (coding->src_pos < 0)
	coding->source = GAP_END_ADDR + coding->src_pos_byte;
      else
	{
	  struct buffer *buf = XBUFFER (coding->src_object);
	  EMACS_INT beg_byte = BUF_BEG_BYTE (buf);
	  EMACS_INT gpt_byte = BUF_GPT_BYTE (buf);
	  unsigned char *beg_addr = BUF_BEG_ADDR (buf);

	  coding->source = beg_addr + coding->src_pos_byte - 1;
	  if (coding->src_pos_byte >= gpt_byte)
	    coding->source += BUF_GAP_SIZE (buf);
	}
    }
  else if (STRINGP (coding->src_object))
    {
      coding->source = (XSTRING (coding->src_object)->data
			+ coding->src_pos_byte);
    }
  else
    /* Otherwise, the source is C string and is never relocated
       automatically.  Thus we don't have to update anything.  */
    ;
}

static void
coding_set_destination (coding)
     struct coding_system *coding;
{
  if (BUFFERP (coding->dst_object))
    {
      /* We are sure that coding->dst_pos_byte is before the gap of the
	 buffer. */
      coding->destination = (BUF_BEG_ADDR (XBUFFER (coding->dst_object))
			     + coding->dst_pos_byte - 1);
      if (coding->src_pos < 0)
	coding->dst_bytes = (GAP_END_ADDR
			     - (coding->src_bytes - coding->consumed)
			     - coding->destination);
      else
	coding->dst_bytes = (BUF_GAP_END_ADDR (XBUFFER (coding->dst_object))
			     - coding->destination);
    }
  else
    /* Otherwise, the destination is C string and is never relocated
       automatically.  Thus we don't have to update anything.  */
    ;
}


static void
coding_alloc_by_realloc (coding, bytes)
     struct coding_system *coding;
     EMACS_INT bytes;
{
  coding->destination = (unsigned char *) xrealloc (coding->destination,
						    coding->dst_bytes + bytes);
  coding->dst_bytes += bytes;
}

static void
coding_alloc_by_making_gap (coding, bytes)
     struct coding_system *coding;
     EMACS_INT bytes;
{
  if (BUFFERP (coding->dst_object)
      && EQ (coding->src_object, coding->dst_object))
    {
      EMACS_INT add = coding->src_bytes - coding->consumed;

      GAP_SIZE -= add; ZV += add; Z += add; ZV_BYTE += add; Z_BYTE += add;
      make_gap (bytes);
      GAP_SIZE += add; ZV -= add; Z -= add; ZV_BYTE -= add; Z_BYTE -= add;
    }
  else
    {
      Lisp_Object this_buffer;

      this_buffer = Fcurrent_buffer ();
      set_buffer_internal (XBUFFER (coding->dst_object));
      make_gap (bytes);
      set_buffer_internal (XBUFFER (this_buffer));
    }
}
     

static unsigned char *
alloc_destination (coding, nbytes, dst)
     struct coding_system *coding;
     int nbytes;
     unsigned char *dst;
{
  EMACS_INT offset = dst - coding->destination;

  if (BUFFERP (coding->dst_object))
    coding_alloc_by_making_gap (coding, nbytes);
  else
    coding_alloc_by_realloc (coding, nbytes);
  coding->result = CODING_RESULT_SUCCESS;
  coding_set_destination (coding);
  dst = coding->destination + offset;
  return dst;
}


/*** 2. Emacs' internal format (emacs-utf-8) ***/




/*** 3. UTF-8 ***/

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in UTF-8.  If it is, return
   CATEGORY_MASK_UTF_8, else return 0.  */

#define UTF_8_1_OCTET_P(c)         ((c) < 0x80)
#define UTF_8_EXTRA_OCTET_P(c)     (((c) & 0xC0) == 0x80)
#define UTF_8_2_OCTET_LEADING_P(c) (((c) & 0xE0) == 0xC0)
#define UTF_8_3_OCTET_LEADING_P(c) (((c) & 0xF0) == 0xE0)
#define UTF_8_4_OCTET_LEADING_P(c) (((c) & 0xF8) == 0xF0)
#define UTF_8_5_OCTET_LEADING_P(c) (((c) & 0xFC) == 0xF8)

static int
detect_coding_utf_8 (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int found = 0;

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      int c, c1, c2, c3, c4;

      ONE_MORE_BYTE (c);
      if (UTF_8_1_OCTET_P (c))
	continue;
      ONE_MORE_BYTE (c1);
      if (! UTF_8_EXTRA_OCTET_P (c1))
	break;
      if (UTF_8_2_OCTET_LEADING_P (c))
	{
	  found++;
	  continue;
	}
      ONE_MORE_BYTE (c2);
      if (! UTF_8_EXTRA_OCTET_P (c2))
	break;
      if (UTF_8_3_OCTET_LEADING_P (c))
	{
	  found++;
	  continue;
	}
      ONE_MORE_BYTE (c3);
      if (! UTF_8_EXTRA_OCTET_P (c3))
	break;
      if (UTF_8_4_OCTET_LEADING_P (c))
	{
	  found++;
	  continue;
	}
      ONE_MORE_BYTE (c4);
      if (! UTF_8_EXTRA_OCTET_P (c4))
	break;
      if (UTF_8_5_OCTET_LEADING_P (c))
	{
	  found++;
	  continue;
	}
      break;
    }
  *mask &= ~CATEGORY_MASK_UTF_8;
  return 0;

 no_more_source:
  if (! found)
    return 0;
  *mask &= CATEGORY_MASK_UTF_8;
  return 1;
}


static void
decode_coding_utf_8 (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  Lisp_Object attr, eol_type, charset_list;

  CODING_GET_INFO (coding, attr, eol_type, charset_list);

  while (1)
    {
      int c, c1, c2, c3, c4, c5;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c1);
      if (UTF_8_1_OCTET_P(c1))
	{
	  c = c1;
	  if (c == '\r')
	    {
	      if (EQ (eol_type, Qdos))
		{
		  if (src == src_end)
		    goto no_more_source;
		  if (*src == '\n')
		    ONE_MORE_BYTE (c);
		}
	      else if (EQ (eol_type, Qmac))
		c = '\n';
	    }
	}
      else
	{
	  ONE_MORE_BYTE (c2);
	  if (! UTF_8_EXTRA_OCTET_P (c2))
	    goto invalid_code;
	  if (UTF_8_2_OCTET_LEADING_P (c1))
	    c = ((c1 & 0x1F) << 6) | (c2 & 0x3F);
	  else
	    {
	      ONE_MORE_BYTE (c3);
	      if (! UTF_8_EXTRA_OCTET_P (c3))
		goto invalid_code;
	      if (UTF_8_3_OCTET_LEADING_P (c1))
		c = (((c1 & 0xF) << 12)
		     | ((c2 & 0x3F) << 6) | (c3 & 0x3F));
	      else
		{
		  ONE_MORE_BYTE (c4);
		  if (! UTF_8_EXTRA_OCTET_P (c4))
		    goto invalid_code;
		  if (UTF_8_4_OCTET_LEADING_P (c1))
		    c = (((c1 & 0x7) << 18) | ((c2 & 0x3F) << 12)
			 | ((c3 & 0x3F) << 6) | (c4 & 0x3F));
		  else
		    {
		      ONE_MORE_BYTE (c5);
		      if (! UTF_8_EXTRA_OCTET_P (c5))
			goto invalid_code;
		      if (UTF_8_5_OCTET_LEADING_P (c1))
			{
			  c = (((c1 & 0x3) << 24) | ((c2 & 0x3F) << 18)
			       | ((c3 & 0x3F) << 12) | ((c4 & 0x3F) << 6)
			       | (c5 & 0x3F));
			  if (c > MAX_CHAR)
			    goto invalid_code;
			}
		      else
			goto invalid_code;
		    }
		}
	    }
	}

      *charbuf++ = c;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


static int
encode_coding_utf_8 (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int produced_chars = 0;
  int c;

  if (multibytep)
    {
      int safe_room = MAX_MULTIBYTE_LENGTH * 2;

      while (charbuf < charbuf_end)
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH], *p, *pend = str;
	  
	  ASSURE_DESTINATION (safe_room);
	  c = *charbuf++;
	  CHAR_STRING_ADVANCE (c, pend);
	  for (p = str; p < pend; p++)
	    EMIT_ONE_BYTE (*p);
	}
    }
  else
    {
      int safe_room = MAX_MULTIBYTE_LENGTH;

      while (charbuf < charbuf_end)
	{
	  ASSURE_DESTINATION (safe_room);
	  c = *charbuf++;
	  dst += CHAR_STRING (c, dst);
	  produced_chars++;
	}
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in UTF-16 Big Endian (endian == 1) or
   Little Endian (otherwise).  If it is, return
   CATEGORY_MASK_UTF_16_BE or CATEGORY_MASK_UTF_16_LE,
   else return 0.  */

#define UTF_16_HIGH_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xD800)

#define UTF_16_LOW_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xDC00)

#define UTF_16_INVALID_P(val)	\
  (((val) == 0xFFFE)		\
   || ((val) == 0xFFFF)		\
   || UTF_16_LOW_SURROGATE_P (val))


static int
detect_coding_utf_16 (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int c1, c2;

  ONE_MORE_BYTE (c1);
  ONE_MORE_BYTE (c2);

  if ((c1 == 0xFF) && (c2 == 0xFE))
    {
      *mask &= CATEGORY_MASK_UTF_16_LE;
      return 1;
    }
  else if ((c1 == 0xFE) && (c2 == 0xFF))
    {
      *mask &= CATEGORY_MASK_UTF_16_BE;
      return 1;
    }
 no_more_source:
  return 0;
}

static void
decode_coding_utf_16 (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  enum utf_16_bom_type bom = CODING_UTF_16_BOM (coding);
  enum utf_16_endian_type endian = CODING_UTF_16_ENDIAN (coding);
  int surrogate = CODING_UTF_16_SURROGATE (coding);
  Lisp_Object attr, eol_type, charset_list;

  CODING_GET_INFO (coding, attr, eol_type, charset_list);

  if (bom != utf_16_without_bom)
    {
      int c, c1, c2;

      src_base = src;
      ONE_MORE_BYTE (c1);
      ONE_MORE_BYTE (c2);
      c = (c1 << 8) | c2;
      if (bom == utf_16_with_bom)
	{
	  if (endian == utf_16_big_endian
	      ? c != 0xFFFE : c != 0xFEFF)
	    {
	      /* We are sure that there's enouph room at CHARBUF.  */
	      *charbuf++ = c1;
	      *charbuf++ = c2;
	      coding->errors++;
	    }
	}
      else
	{
	  if (c == 0xFFFE)
	    CODING_UTF_16_ENDIAN (coding)
	      = endian = utf_16_big_endian;
	  else if (c == 0xFEFF)
	    CODING_UTF_16_ENDIAN (coding)
	      = endian = utf_16_little_endian;
	  else
	    {
	      CODING_UTF_16_ENDIAN (coding)
		= endian = utf_16_big_endian;
	      src = src_base;
	    }
	}
      CODING_UTF_16_BOM (coding) = utf_16_with_bom;
    }

  while (1)
    {
      int c, c1, c2;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf + 2 >= charbuf_end)
	break;

      ONE_MORE_BYTE (c1);
      ONE_MORE_BYTE (c2);
      c = (endian == utf_16_big_endian
	   ? ((c1 << 8) | c2) : ((c2 << 8) | c1));
      if (surrogate)
	{
	  if (! UTF_16_LOW_SURROGATE_P (c))
	    {
	      if (endian == utf_16_big_endian)
		c1 = surrogate >> 8, c2 = surrogate & 0xFF;
	      else
		c1 = surrogate & 0xFF, c2 = surrogate >> 8;
	      *charbuf++ = c1;
	      *charbuf++ = c2;
	      coding->errors++;
	      if (UTF_16_HIGH_SURROGATE_P (c))
		CODING_UTF_16_SURROGATE (coding) = surrogate = c;
	      else
		*charbuf++ = c;
	    }
	  else
	    {
	      c = ((surrogate - 0xD800) << 10) | (c - 0xDC00);
	      CODING_UTF_16_SURROGATE (coding) = surrogate = 0;
	      *charbuf++ = c;
	    }
	}
      else
	{
	  if (UTF_16_HIGH_SURROGATE_P (c))
	    CODING_UTF_16_SURROGATE (coding) = surrogate = c;
	  else
	    *charbuf++ = c;
	}	  
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_utf_16 (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 8;
  enum utf_16_bom_type bom = CODING_UTF_16_BOM (coding);
  int big_endian = CODING_UTF_16_ENDIAN (coding) == utf_16_big_endian;
  int produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);

  if (bom == utf_16_with_bom)
    {
      ASSURE_DESTINATION (safe_room);
      if (big_endian)
	EMIT_TWO_BYTES (0xFF, 0xFE);
      else
	EMIT_TWO_BYTES (0xFE, 0xFF);
      CODING_UTF_16_BOM (coding) = utf_16_without_bom;
    }

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      if (c >= MAX_UNICODE_CHAR)
	c = coding->default_char;

      if (c < 0x10000)
	{
	  if (big_endian)
	    EMIT_TWO_BYTES (c >> 8, c & 0xFF);
	  else
	    EMIT_TWO_BYTES (c & 0xFF, c >> 8);
	}
      else
	{
	  int c1, c2;

	  c -= 0x10000;
	  c1 = (c >> 10) + 0xD800;
	  c2 = (c & 0x3FF) + 0xDC00;
	  if (big_endian)
	    EMIT_FOUR_BYTES (c1 >> 8, c1 & 0xFF, c2 >> 8, c2 & 0xFF);
	  else
	    EMIT_FOUR_BYTES (c1 & 0xFF, c1 >> 8, c2 & 0xFF, c2 >> 8);
	}
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced = dst - coding->destination;
  coding->produced_char += produced_chars;
  return 0;
}


/*** 6. Old Emacs' internal format (emacs-mule) ***/

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

   At first, all characters in eight-bit-control are represented by
   one-byte sequences which are their 8-bit code.

   Next, character composition data are represented by the byte
   sequence of the form: 0x80 METHOD BYTES CHARS COMPONENT ...,
   where,
	METHOD is 0xF0 plus one of composition method (enum
	composition_method),

	BYTES is 0xA0 plus a byte length of this composition data,

	CHARS is 0x20 plus a number of characters composed by this
	data,

	COMPONENTs are characters of multibye form or composition
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

char emacs_mule_bytes[256];

/* Leading-code followed by extended leading-code.  */
#define LEADING_CODE_PRIVATE_11	0x9A /* for private DIMENSION1 of 1-column */
#define LEADING_CODE_PRIVATE_12	0x9B /* for private DIMENSION1 of 2-column */
#define LEADING_CODE_PRIVATE_21	0x9C /* for private DIMENSION2 of 1-column */
#define LEADING_CODE_PRIVATE_22	0x9D /* for private DIMENSION2 of 2-column */


int
emacs_mule_char (coding, composition, nbytes, nchars)
     struct coding_system *coding;
     int composition;
     int *nbytes, *nchars;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  unsigned char *src_base = src;
  struct charset *charset;
  unsigned code;
  int c;
  int consumed_chars = 0;

  ONE_MORE_BYTE (c);
  if (composition)
    {
      c -= 0x20;
      if (c == 0x80)
	{
	  ONE_MORE_BYTE (c);
	  if (c < 0xA0)
	    goto invalid_code;
	  *nbytes = src - src_base;
	  *nchars = consumed_chars;
	  return (c - 0x80);
	}
    }

  switch (emacs_mule_bytes[c])
    {
    case 2:
      if (! (charset = emacs_mule_charset[c]))
	goto invalid_code;
      ONE_MORE_BYTE (c);
      code = c & 0x7F;
      break;

    case 3:
      if (c == LEADING_CODE_PRIVATE_11
	  || c == LEADING_CODE_PRIVATE_12)
	{
	  ONE_MORE_BYTE (c);
	  if (! (charset = emacs_mule_charset[c]))
	    goto invalid_code;
	  ONE_MORE_BYTE (c);
	  code = c & 0x7F;
	}
      else
	{
	  if (! (charset = emacs_mule_charset[c]))
	    goto invalid_code;
	  ONE_MORE_BYTE (c);
	  code = (c & 0x7F) << 7;
	  ONE_MORE_BYTE (c);
	  code |= c & 0x7F;
	}
      break;

    case 4:
      if (! (charset = emacs_mule_charset[c]))
	goto invalid_code;
      ONE_MORE_BYTE (c);
      code = (c & 0x7F) << 7;
      ONE_MORE_BYTE (c);
      code |= c & 0x7F;
      break;

    case 1:
      code = c;
      charset = CHARSET_FROM_ID (ASCII_BYTE_P (code) ? charset_ascii
				 : code < 0xA0 ? charset_8_bit_control
				 : charset_8_bit_graphic);
      break;

    default:
      abort ();
    }
  c = DECODE_CHAR (charset, code);
  if (c < 0)
    goto invalid_code;
  *nbytes = src - src_base;
  *nchars = consumed_chars;
  return c;

 no_more_source:
  return -2;

 invalid_code:
  return -1;
}


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in `emacs-mule'.  */

static int
detect_coding_emacs_mule (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int c;
  int found = 0;

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      ONE_MORE_BYTE (c);

      if (c == 0x80)
	{
	  /* Perhaps the start of composite character.  We simple skip
	     it because analyzing it is too heavy for detecting.  But,
	     at least, we check that the composite character
	     constitues of more than 4 bytes.  */
	  unsigned char *src_base;

	repeat:
	  src_base = src;
	  do
	    {
	      ONE_MORE_BYTE (c);
	    }
	  while (c >= 0xA0);

	  if (src - src_base <= 4)
	    break;
	  found = 1;
	  if (c == 0x80)
	    goto repeat;
	}

      if (c < 0x80)
	{
	  if (c < 0x20
	      && (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO))
	    break;
	}
      else
	{
	  unsigned char *src_base = src - 1;

	  do
	    {
	      ONE_MORE_BYTE (c);
	    }
	  while (c >= 0xA0);
	  if (src - src_base != emacs_mule_bytes[*src_base])
	    break;
	  found = 1;
	}
    }
  *mask &= ~CATEGORY_MASK_EMACS_MULE;
  return 0;

 no_more_source:
  if (!found)
    return 0;
  *mask &= CATEGORY_MASK_EMACS_MULE;
  return 1;
}


/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

/* Decode a character represented as a component of composition
   sequence of Emacs 20/21 style at SRC.  Set C to that character and
   update SRC to the head of next character (or an encoded composition
   rule).  If SRC doesn't points a composition component, set C to -1.
   If SRC points an invalid byte sequence, global exit by a return
   value 0.  */

#define DECODE_EMACS_MULE_COMPOSITION_CHAR(buf)			\
  if (1)							\
    {								\
      int c;							\
      int nbytes, nchars;					\
								\
      if (src == src_end)					\
	break;							\
      c = emacs_mule_char (coding, 1, &nbytes, &nchars);	\
      if (c < 0)						\
	{							\
	  if (c == -2)						\
	    break;						\
	  goto invalid_code;					\
	}							\
      *buf++ = c;						\
      src += nbytes;						\
      consumed_chars += nchars;					\
    }								\
  else


/* Decode a composition rule represented as a component of composition
   sequence of Emacs 20 style at SRC.  Set C to the rule.  If SRC
   points an invalid byte sequence, set C to -1.  */

#define DECODE_EMACS_MULE_COMPOSITION_RULE(buf)		\
  do {							\
    int c, gref, nref;					\
							\
    if (src < src_end)					\
      goto invalid_code;				\
    ONE_MORE_BYTE_NO_CHECK (c);				\
    c -= 0xA0;						\
    if (c < 0 || c >= 81)				\
      goto invalid_code;				\
							\
    gref = c / 9, nref = c % 9;				\
    *buf++ = COMPOSITION_ENCODE_RULE (gref, nref);	\
  } while (0)


#define ADD_COMPOSITION_DATA(buf, method, nchars)	\
  do {							\
    *buf++ = -5;					\
    *buf++ = coding->produced_char + char_offset;	\
    *buf++ = CODING_ANNOTATE_COMPOSITION_MASK;		\
    *buf++ = method;					\
    *buf++ = nchars;					\
  } while (0)


#define DECODE_EMACS_MULE_21_COMPOSITION(c)				\
  do {									\
    /* Emacs 21 style format.  The first three bytes at SRC are		\
       (METHOD - 0xF0), (BYTES - 0xA0), (CHARS - 0xA0), where BYTES is	\
       the byte length of this composition information, CHARS is the	\
       number of characters composed by this composition.  */		\
    enum composition_method method = c - 0xF0;				\
    int consumed_chars_limit;						\
    int nbytes, nchars;							\
									\
    ONE_MORE_BYTE (c);							\
    nbytes = c - 0xA0;							\
    if (nbytes < 3)							\
      goto invalid_code;						\
    ONE_MORE_BYTE (c);							\
    nchars = c - 0xA0;							\
    ADD_COMPOSITION_DATA (charbuf, method, nchars);			\
    consumed_chars_limit = consumed_chars_base + nbytes;		\
    if (method != COMPOSITION_RELATIVE)					\
      {									\
	int i = 0;							\
	while (consumed_chars < consumed_chars_limit)			\
	  {								\
	    if (i % 2 && method != COMPOSITION_WITH_ALTCHARS)		\
	      DECODE_EMACS_MULE_COMPOSITION_RULE (charbuf);		\
	    else							\
	      DECODE_EMACS_MULE_COMPOSITION_CHAR (charbuf);		\
	  }								\
	if (consumed_chars < consumed_chars_limit)			\
	  goto invalid_code;						\
      }									\
  } while (0)


#define DECODE_EMACS_MULE_20_RELATIVE_COMPOSITION(c)		\
  do {								\
    /* Emacs 20 style format for relative composition.  */	\
    /* Store multibyte form of characters to be composed.  */	\
    int components[MAX_COMPOSITION_COMPONENTS * 2 - 1];		\
    int *buf = components;					\
    int i, j;							\
								\
    src = src_base;						\
    ONE_MORE_BYTE (c);		/* skip 0x80 */			\
    for (i = 0; i < MAX_COMPOSITION_COMPONENTS; i++)		\
      DECODE_EMACS_MULE_COMPOSITION_CHAR (buf);			\
    if (i < 2)							\
      goto invalid_code;					\
    ADD_COMPOSITION_DATA (charbuf, COMPOSITION_RELATIVE, i);	\
    for (j = 0; j < i; j++)					\
      *charbuf++ = components[j];				\
  } while (0)


#define DECODE_EMACS_MULE_20_RULEBASE_COMPOSITION(c)		\
  do {								\
    /* Emacs 20 style format for rule-base composition.  */	\
    /* Store multibyte form of characters to be composed.  */	\
    int components[MAX_COMPOSITION_COMPONENTS * 2 - 1];		\
    int *buf = components;					\
    int i, j;							\
								\
    DECODE_EMACS_MULE_COMPOSITION_CHAR (buf);			\
    for (i = 0; i < MAX_COMPOSITION_COMPONENTS; i++)		\
      {								\
	DECODE_EMACS_MULE_COMPOSITION_RULE (buf);		\
	DECODE_EMACS_MULE_COMPOSITION_CHAR (buf);		\
      }								\
    if (i < 1 || (buf - components) % 2 == 0)			\
      goto invalid_code;					\
    if (charbuf + i + (i / 2) + 1 < charbuf_end)		\
      goto no_more_source;					\
    ADD_COMPOSITION_DATA (buf, COMPOSITION_WITH_RULE, i);	\
    for (j = 0; j < i; j++)					\
      *charbuf++ = components[j];				\
    for (j = 0; j < i; j += 2)					\
      *charbuf++ = components[j];				\
  } while (0)


static void
decode_coding_emacs_mule (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int char_offset = 0;
  int multibytep = coding->src_multibyte;
  Lisp_Object attrs, eol_type, charset_list;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);

  while (1)
    {
      int c;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c);

      if (c < 0x80)
	{
	  if (c == '\r')
	    {
	      if (EQ (eol_type, Qdos))
		{
		  if (src == src_end)
		    goto no_more_source;
		  if (*src == '\n')
		    ONE_MORE_BYTE (c);
		}
	      else if (EQ (eol_type, Qmac))
		c = '\n';
	    }
	  *charbuf++ = c;
	  char_offset++;
	}
      else if (c == 0x80)
	{
	  if (charbuf + 5 + (MAX_COMPOSITION_COMPONENTS * 2) - 1 > charbuf_end)
	    break;
	  ONE_MORE_BYTE (c);
	  if (c - 0xF0 >= COMPOSITION_RELATIVE
	      && c - 0xF0 <= COMPOSITION_WITH_RULE_ALTCHARS)
	    DECODE_EMACS_MULE_21_COMPOSITION (c);
	  else if (c < 0xC0)
	    DECODE_EMACS_MULE_20_RELATIVE_COMPOSITION (c);
	  else if (c == 0xFF)
	    DECODE_EMACS_MULE_20_RULEBASE_COMPOSITION (c);
	  else
	    goto invalid_code;
	}
      else if (c < 0xA0 && emacs_mule_bytes[c] > 1)
	{
	  int nbytes, nchars;
	  src--;
	  c = emacs_mule_char (coding, 0, &nbytes, &nchars);
	  if (c < 0)
	    {
	      if (c == -2)
		break;
	      goto invalid_code;
	    }
	  *charbuf++ = c;
	  char_offset++;
	}
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


#define EMACS_MULE_LEADING_CODES(id, codes)	\
  do {						\
    if (id < 0xA0)				\
      codes[0] = id, codes[1] = 0;		\
    else if (id < 0xE0)				\
      codes[0] = 0x9A, codes[1] = id;		\
    else if (id < 0xF0)				\
      codes[0] = 0x9B, codes[1] = id;		\
    else if (id < 0xF5)				\
      codes[0] = 0x9C, codes[1] = id;		\
    else					\
      codes[0] = 0x9D, codes[1] = id;		\
  } while (0);


static int
encode_coding_emacs_mule (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 8;
  int produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      if (ASCII_CHAR_P (c))
	EMIT_ONE_ASCII_BYTE (c);
      else
	{
	  struct charset *charset;
	  unsigned code;
	  int dimension;
	  int emacs_mule_id;
	  unsigned char leading_codes[2];

	  charset = char_charset (c, charset_list, &code);
	  if (! charset)
	    {
	      c = coding->default_char;
	      if (ASCII_CHAR_P (c))
		{
		  EMIT_ONE_ASCII_BYTE (c);
		  continue;
		}
	      charset = char_charset (c, charset_list, &code);
	    }
	  dimension = CHARSET_DIMENSION (charset);
	  emacs_mule_id = CHARSET_EMACS_MULE_ID (charset);
	  EMACS_MULE_LEADING_CODES (emacs_mule_id, leading_codes);
	  EMIT_ONE_BYTE (leading_codes[0]);
	  if (leading_codes[1])
	    EMIT_ONE_BYTE (leading_codes[1]);
	  if (dimension == 1)
	    EMIT_ONE_BYTE (code);
	  else
	    {
	      EMIT_ONE_BYTE (code >> 8);
	      EMIT_ONE_BYTE (code & 0xFF);
	    }
	}
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 7. ISO2022 handlers ***/

/* The following note describes the coding system ISO2022 briefly.
   Since the intention of this note is to help understand the
   functions in this file, some parts are NOT ACCURATE or OVERLY
   SIMPLIFIED.  For thorough understanding, please refer to the
   original document of ISO2022.

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environments.  For 7-bite environments, all text
   is encoded using bytes less than 128.  This may make the encoded
   text a little bit longer, but the text passes more easily through
   several gateways, some of which strip off MSB (Most Signigant Bit).

   There are two kinds of character sets: control character set and
   graphic character set.  The former contains control characters such
   as `newline' and `escape' to provide control functions (control
   functions are also provided by escape sequences).  The latter
   contains graphic characters such as 'A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, according to the number of bytes (DIMENSION) and
   number of characters in one dimension (CHARS) of the set:
   - DIMENSION1_CHARS94
   - DIMENSION1_CHARS96
   - DIMENSION2_CHARS94
   - DIMENSION2_CHARS96

   In addition, each character set is assigned an identification tag,
   unique for each set, called "final character" (denoted as <F>
   hereafter).  The <F> of each character set is decided by ECMA(*)
   when it is registered in ISO.  The code range of <F> is 0x30..0x7F
   (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character set [NAME(<F>)]:
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
   '(' must be omitted.  We refer to this as "short-form" hereafter.

   Now you may notice that there are a lot of ways for encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X11's inter client
   communication, ISO-2022-JP (used in Japanese internet), ISO-2022-KR
   (used in Korean internet), EUC (Extended UNIX Code, used in Asian
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
  the use of them for these meaning is restricted to Emacs only.

  (*) This form is used only in Emacs 20.5 and the older versions,
  but the newer versions can safely decode it.
  (**) This form is used only in Emacs 21.1 and the newer versions,
  and the older versions can't decode it.

  Here's a list of examples usages of these composition escape
  sequences (categorized by `enum composition_method').

  COMPOSITION_RELATIVE:
	ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITOIN_WITH_RULE:
	ESC 2 CHAR [ RULE CHAR ] ESC 1
  COMPOSITION_WITH_ALTCHARS:
	ESC 3 ALTCHAR [ ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE_ALTCHARS:
	ESC 4 ALTCHAR [ RULE ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1 */

enum iso_code_class_type iso_code_class[256];

#define SAFE_CHARSET_P(coding, id)	\
  ((id) <= (coding)->max_charset_id	\
   && (coding)->safe_charsets[id] >= 0)


#define SHIFT_OUT_OK(category)	\
  (CODING_ISO_INITIAL (&coding_categories[category], 1) >= 0)

static void
setup_iso_safe_charsets (Lisp_Object attrs)
{
  Lisp_Object charset_list, safe_charsets;
  Lisp_Object request;
  Lisp_Object reg_usage;
  Lisp_Object tail;
  int reg94, reg96;
  int flags = XINT (AREF (attrs, coding_attr_iso_flags));
  int max_charset_id;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  if ((flags & CODING_ISO_FLAG_FULL_SUPPORT)
      && ! EQ (charset_list, Viso_2022_charset_list))
    {
      CODING_ATTR_CHARSET_LIST (attrs)
	= charset_list = Viso_2022_charset_list;
      ASET (attrs, coding_attr_safe_charsets, Qnil);
    }

  if (STRINGP (AREF (attrs, coding_attr_safe_charsets)))
    return;

  max_charset_id = 0;
  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    {
      int id = XINT (XCAR (tail));
      if (max_charset_id < id)
	max_charset_id = id;
    }

  safe_charsets = Fmake_string (make_number (max_charset_id + 1),
				make_number (255));
  request = AREF (attrs, coding_attr_iso_request);
  reg_usage = AREF (attrs, coding_attr_iso_usage);
  reg94 = XINT (XCAR (reg_usage));
  reg96 = XINT (XCDR (reg_usage));

  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object id;
      Lisp_Object reg;
      struct charset *charset;

      id = XCAR (tail);
      charset = CHARSET_FROM_ID (XINT (id));
      reg = Fcdr (Fassq (request, id));
      if (! NILP (reg))
	XSTRING (safe_charsets)->data[XINT (id)] = XINT (reg);
      else if (charset->iso_chars_96)
	{
	  if (reg96 < 4)
	    XSTRING (safe_charsets)->data[XINT (id)] = reg96;
	}
      else
	{
	  if (reg94 < 4)
	    XSTRING (safe_charsets)->data[XINT (id)] = reg94;
	}
    }
  ASET (attrs, coding_attr_safe_charsets, safe_charsets);
}


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in ISO2022.  If it is, returns an
   integer in which appropriate flag bits any of:
	CATEGORY_MASK_ISO_7
	CATEGORY_MASK_ISO_7_TIGHT
	CATEGORY_MASK_ISO_8_1
	CATEGORY_MASK_ISO_8_2
	CATEGORY_MASK_ISO_7_ELSE
	CATEGORY_MASK_ISO_8_ELSE
   are set.  If a code which should never appear in ISO2022 is found,
   returns 0.  */

static int
detect_coding_iso_2022 (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int mask_iso = CATEGORY_MASK_ISO;
  int mask_found = 0, mask_8bit_found = 0;
  int reg[4], shift_out = 0, single_shifting = 0;
  int id;
  int c, c1;
  int consumed_chars = 0;
  int i;

  for (i = coding_category_iso_7; i <= coding_category_iso_8_else; i++)
    {
      struct coding_system *this = &(coding_categories[i]);
      Lisp_Object attrs, val;

      attrs = CODING_ID_ATTRS (this->id);
      if (CODING_ISO_FLAGS (this) & CODING_ISO_FLAG_FULL_SUPPORT
	  && ! EQ (CODING_ATTR_SAFE_CHARSETS (attrs), Viso_2022_charset_list))
	setup_iso_safe_charsets (attrs);
      val = CODING_ATTR_SAFE_CHARSETS (attrs);
      this->max_charset_id = XSTRING (val)->size - 1;
      this->safe_charsets = (char *) XSTRING (val)->data;
    }

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  reg[0] = charset_ascii, reg[1] = reg[2] = reg[3] = -1;
  while (mask_iso && src < src_end)
    {
      ONE_MORE_BYTE (c);
      switch (c)
	{
	case ISO_CODE_ESC:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  ONE_MORE_BYTE (c);
	  if (c >= '(' && c <= '/')
	    {
	      /* Designation sequence for a charset of dimension 1.  */
	      ONE_MORE_BYTE (c1);
	      if (c1 < ' ' || c1 >= 0x80
		  || (id = iso_charset_table[0][c >= ','][c1]) < 0)
		/* Invalid designation sequence.  Just ignore.  */
		break;
	      reg[(c - '(') % 4] = id;
	    }
	  else if (c == '$')
	    {
	      /* Designation sequence for a charset of dimension 2.  */
	      ONE_MORE_BYTE (c);
	      if (c >= '@' && c <= 'B')
		/* Designation for JISX0208.1978, GB2312, or JISX0208.  */
		reg[0] = id = iso_charset_table[1][0][c];
	      else if (c >= '(' && c <= '/')
		{
		  ONE_MORE_BYTE (c1);
		  if (c1 < ' ' || c1 >= 0x80
		      || (id = iso_charset_table[1][c >= ','][c1]) < 0)
		    /* Invalid designation sequence.  Just ignore.  */
		    break;
		  reg[(c - '(') % 4] = id;
		}
	      else
		/* Invalid designation sequence.  Just ignore.  */
		break;
	    }
	  else if (c == 'N' || c == 'O')
	    {
	      /* ESC <Fe> for SS2 or SS3.  */
	      mask_iso &= CATEGORY_MASK_ISO_7_ELSE;
	      break;
	    }
	  else if (c >= '0' && c <= '4')
	    {
	      /* ESC <Fp> for start/end composition.  */
	      mask_found |= CATEGORY_MASK_ISO;
	      break;
	    }
	  else
	    {
	      /* Invalid escape sequence.  */
	      mask_iso &= ~CATEGORY_MASK_ISO_ESCAPE;
	      break;
	    }

	  /* We found a valid designation sequence for CHARSET.  */
	  mask_iso &= ~CATEGORY_MASK_ISO_8BIT;
	  if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7],
			      id))
	    mask_found |= CATEGORY_MASK_ISO_7;
	  else
	    mask_iso &= ~CATEGORY_MASK_ISO_7;
	  if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7_tight],
			      id))
	    mask_found |= CATEGORY_MASK_ISO_7_TIGHT;
	  else
	    mask_iso &= ~CATEGORY_MASK_ISO_7_TIGHT;
	  if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7_else],
			      id))
	    mask_found |= CATEGORY_MASK_ISO_7_ELSE;
	  else
	    mask_iso &= ~CATEGORY_MASK_ISO_7_ELSE;
	  if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_8_else],
			      id))
	    mask_found |= CATEGORY_MASK_ISO_8_ELSE;
	  else
	    mask_iso &= ~CATEGORY_MASK_ISO_8_ELSE;
	  break;

	case ISO_CODE_SO:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  if (shift_out == 0
	      && (reg[1] >= 0
		  || SHIFT_OUT_OK (coding_category_iso_7_else)
		  || SHIFT_OUT_OK (coding_category_iso_8_else)))
	    {
	      /* Locking shift out.  */
	      mask_iso &= ~CATEGORY_MASK_ISO_7BIT;
	      mask_found |= CATEGORY_MASK_ISO_ELSE;
	    }
	  break;
	  
	case ISO_CODE_SI:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  if (shift_out == 1)
	    {
	      /* Locking shift in.  */
	      mask_iso &= ~CATEGORY_MASK_ISO_7BIT;
	      mask_found |= CATEGORY_MASK_ISO_ELSE;
	    }
	  break;

	case ISO_CODE_CSI:
	  single_shifting = 0;
	case ISO_CODE_SS2:
	case ISO_CODE_SS3:
	  {
	    int newmask = CATEGORY_MASK_ISO_8_ELSE;

	    if (inhibit_iso_escape_detection)
	      break;
	    if (c != ISO_CODE_CSI)
	      {
		if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_1])
		    & CODING_ISO_FLAG_SINGLE_SHIFT)
		  newmask |= CATEGORY_MASK_ISO_8_1;
		if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_2])
		    & CODING_ISO_FLAG_SINGLE_SHIFT)
		  newmask |= CATEGORY_MASK_ISO_8_2;
		single_shifting = 1;
	      }
	    if (VECTORP (Vlatin_extra_code_table)
		&& !NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
	      {
		if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_1])
		    & CODING_ISO_FLAG_LATIN_EXTRA)
		  newmask |= CATEGORY_MASK_ISO_8_1;
		if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_2])
		    & CODING_ISO_FLAG_LATIN_EXTRA)
		  newmask |= CATEGORY_MASK_ISO_8_2;
	      }
	    mask_iso &= newmask;
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
	      mask_8bit_found = 1;
	      if (VECTORP (Vlatin_extra_code_table)
		  && !NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
		{
		  int newmask = 0;

		  if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_1])
		      & CODING_ISO_FLAG_LATIN_EXTRA)
		    newmask |= CATEGORY_MASK_ISO_8_1;
		  if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_2])
		      & CODING_ISO_FLAG_LATIN_EXTRA)
		    newmask |= CATEGORY_MASK_ISO_8_2;
		  mask_iso &= newmask;
		  mask_found |= newmask;
		}
	      else
		return 0;
	    }
	  else
	    {
	      mask_iso &= ~(CATEGORY_MASK_ISO_7BIT
			    | CATEGORY_MASK_ISO_7_ELSE);
	      mask_found |= CATEGORY_MASK_ISO_8_1;
	      mask_8bit_found = 1;
	      /* Check the length of succeeding codes of the range
                 0xA0..0FF.  If the byte length is odd, we exclude
                 CATEGORY_MASK_ISO_8_2.  We can check this only
                 when we are not single shifting.  */
	      if (!single_shifting
		  && mask_iso & CATEGORY_MASK_ISO_8_2)
		{
		  int i = 1;
		  while (src < src_end)
		    {
		      ONE_MORE_BYTE (c);
		      if (c < 0xA0)
			break;
		      i++;
		    }

		  if (i & 1 && src < src_end)
		    mask_iso &= ~CATEGORY_MASK_ISO_8_2;
		  else
		    mask_found |= CATEGORY_MASK_ISO_8_2;
		}
	    }
	  break;
	}
    }
 no_more_source:
  if (!mask_iso)
    {
      *mask &= ~CATEGORY_MASK_ISO;
      return 0;
    }
  if (!mask_found)
    return 0;
  *mask &= mask_iso & mask_found; 
  if (! mask_8bit_found)
    *mask &= ~(CATEGORY_MASK_ISO_8BIT | CATEGORY_MASK_ISO_8_ELSE);
  return 1;
}


/* Set designation state into CODING.  */
#define DECODE_DESIGNATION(reg, dim, chars_96, final)			\
  do {									\
    int id, prev;							\
									\
    if (final < '0' || final >= 128					\
	|| ((id = ISO_CHARSET_TABLE (dim, chars_96, final)) < 0)	\
	|| !SAFE_CHARSET_P (coding, id))				\
      {									\
	CODING_ISO_DESIGNATION (coding, reg) = -2;			\
	goto invalid_code;						\
      }									\
    prev = CODING_ISO_DESIGNATION (coding, reg);			\
    CODING_ISO_DESIGNATION (coding, reg) = id;				\
    /* If there was an invalid designation to REG previously, and this	\
       designation is ASCII to REG, we should keep this designation	\
       sequence.  */							\
    if (prev == -2 && id == charset_ascii)				\
      goto invalid_code;						\
  } while (0)


#define MAYBE_FINISH_COMPOSITION()				\
  do {								\
    int i;							\
    if (composition_state == COMPOSING_NO)			\
      break;							\
    /* It is assured that we have enough room for producing	\
       characters stored in the table `components'.  */		\
    if (charbuf + component_idx > charbuf_end)			\
      goto no_more_source;					\
    composition_state = COMPOSING_NO;				\
    if (method == COMPOSITION_RELATIVE				\
	|| method == COMPOSITION_WITH_ALTCHARS)			\
      {								\
	for (i = 0; i < component_idx; i++)			\
	  *charbuf++ = components[i];				\
	char_offset += component_idx;				\
      }								\
    else							\
      {								\
	for (i = 0; i < component_idx; i += 2)			\
	  *charbuf++ = components[i];				\
	char_offset += (component_idx / 2) + 1;			\
      }								\
  } while (0)


/* Handle composition start sequence ESC 0, ESC 2, ESC 3, or ESC 4.
   ESC 0 : relative composition : ESC 0 CHAR ... ESC 1
   ESC 2 : rulebase composition : ESC 2 CHAR RULE CHAR RULE ... CHAR ESC 1
   ESC 3 : altchar composition :  ESC 3 CHAR ... ESC 0 CHAR ... ESC 1
   ESC 4 : alt&rule composition : ESC 4 CHAR RULE ... CHAR ESC 0 CHAR ... ESC 1
  */

#define DECODE_COMPOSITION_START(c1)					\
  do {									\
    if (c1 == '0'							\
	&& composition_state == COMPOSING_COMPONENT_CHAR)		\
      {									\
	component_len = component_idx;					\
	composition_state = COMPOSING_CHAR;				\
      }									\
    else								\
      {									\
	unsigned char *p;						\
									\
	MAYBE_FINISH_COMPOSITION ();					\
	if (charbuf + MAX_COMPOSITION_COMPONENTS > charbuf_end)		\
	  goto no_more_source;						\
	for (p = src; p < src_end - 1; p++)				\
	  if (*p == ISO_CODE_ESC && p[1] == '1')			\
	    break;							\
	if (p == src_end - 1)						\
	  {								\
	    if (coding->mode & CODING_MODE_LAST_BLOCK)			\
	      goto invalid_code;					\
	    goto no_more_source;					\
	  }								\
									\
	/* This is surely the start of a composition.  */		\
	method = (c1 == '0' ? COMPOSITION_RELATIVE			\
		  : c1 == '2' ? COMPOSITION_WITH_RULE			\
		  : c1 == '3' ? COMPOSITION_WITH_ALTCHARS		\
		  : COMPOSITION_WITH_RULE_ALTCHARS);			\
	composition_state = (c1 <= '2' ? COMPOSING_CHAR			\
			     : COMPOSING_COMPONENT_CHAR);		\
	component_idx = component_len = 0;				\
      }									\
  } while (0)


/* Handle compositoin end sequence ESC 1.  */

#define DECODE_COMPOSITION_END()					\
  do {									\
    int nchars = (component_len > 0 ? component_idx - component_len	\
		  : method == COMPOSITION_RELATIVE ? component_idx	\
		  : (component_idx + 1) / 2);				\
    int i;								\
    int *saved_charbuf = charbuf;					\
									\
    ADD_COMPOSITION_DATA (charbuf, method, nchars);			\
    if (method != COMPOSITION_RELATIVE)					\
      {									\
	if (component_len == 0)						\
	  for (i = 0; i < component_idx; i++)				\
	    *charbuf++ = components[i];					\
	else								\
	  for (i = 0; i < component_len; i++)				\
	    *charbuf++ = components[i];					\
	*saved_charbuf = saved_charbuf - charbuf;			\
      }									\
    if (method == COMPOSITION_WITH_RULE)				\
      for (i = 0; i < component_idx; i += 2, char_offset++)		\
	*charbuf++ = components[i];					\
    else								\
      for (i = component_len; i < component_idx; i++, char_offset++)	\
	*charbuf++ = components[i];					\
    coding->annotated = 1;						\
    composition_state = COMPOSING_NO;					\
  } while (0)


/* Decode a composition rule from the byte C1 (and maybe one more byte
   from SRC) and store one encoded composition rule in
   coding->cmp_data.  */

#define DECODE_COMPOSITION_RULE(c1)					\
  do {									\
    (c1) -= 32;								\
    if (c1 < 81)		/* old format (before ver.21) */	\
      {									\
	int gref = (c1) / 9;						\
	int nref = (c1) % 9;						\
	if (gref == 4) gref = 10;					\
	if (nref == 4) nref = 10;					\
	c1 = COMPOSITION_ENCODE_RULE (gref, nref);			\
      }									\
    else if (c1 < 93)		/* new format (after ver.21) */		\
      {									\
	ONE_MORE_BYTE (c2);						\
	c1 = COMPOSITION_ENCODE_RULE (c1 - 81, c2 - 32);		\
      }									\
    else								\
      c1 = 0;								\
  } while (0)


/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_iso_2022 (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size - 4;
  int consumed_chars = 0, consumed_chars_base;
  int char_offset = 0;
  int multibytep = coding->src_multibyte;
  /* Charsets invoked to graphic plane 0 and 1 respectively.  */
  int charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
  int charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
  struct charset *charset;
  int c;
  /* For handling composition sequence.  */
#define COMPOSING_NO			0
#define COMPOSING_CHAR			1
#define COMPOSING_RULE			2
#define COMPOSING_COMPONENT_CHAR	3
#define COMPOSING_COMPONENT_RULE	4

  int composition_state = COMPOSING_NO;
  enum composition_method method;
  int components[MAX_COMPOSITION_COMPONENTS * 2 + 1];
  int component_idx;
  int component_len;
  Lisp_Object attrs, eol_type, charset_list;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  setup_iso_safe_charsets (attrs);

  while (1)
    {
      int c1, c2;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c1);

      /* We produce no character or one character.  */
      switch (iso_code_class [c1])
	{
	case ISO_0x20_or_0x7F:
	  if (composition_state != COMPOSING_NO)
	    {
	      if (composition_state == COMPOSING_RULE
		  || composition_state == COMPOSING_COMPONENT_RULE)
		{
		  DECODE_COMPOSITION_RULE (c1);
		  components[component_idx++] = c1;
		  composition_state--;
		  continue;
		}
	      else if (method == COMPOSITION_WITH_RULE)
		composition_state = COMPOSING_RULE;
	      else if (method == COMPOSITION_WITH_RULE_ALTCHARS
		       && composition_state == COMPOSING_COMPONENT_CHAR)
		composition_state = COMPOSING_COMPONENT_CHAR;
	    }
	  if (charset_id_0 < 0
	      || ! CHARSET_ISO_CHARS_96 (CHARSET_FROM_ID (charset_id_0)))
	    {
	      /* This is SPACE or DEL.  */
	      charset = CHARSET_FROM_ID (charset_ascii);
	      break;
	    }
	  /* This is a graphic character, we fall down ...  */

	case ISO_graphic_plane_0:
	  if (composition_state == COMPOSING_RULE)
	    {
	      DECODE_COMPOSITION_RULE (c1);
	      components[component_idx++] = c1;
	      composition_state = COMPOSING_CHAR;
	    }
	  charset = CHARSET_FROM_ID (charset_id_0);
	  break;

	case ISO_0xA0_or_0xFF:
	  if (charset_id_1 < 0
	      || ! CHARSET_ISO_CHARS_96 (CHARSET_FROM_ID (charset_id_1))
	      || CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)
	    goto invalid_code;
	  /* This is a graphic character, we fall down ... */

	case ISO_graphic_plane_1:
	  if (charset_id_1 < 0)
	    goto invalid_code;
	  charset = CHARSET_FROM_ID (charset_id_1);
	  break;

	case ISO_carriage_return:
	  if (c1 == '\r')
	    {
	      if (EQ (eol_type, Qdos))
		{
		  if (src == src_end)
		    goto no_more_source;
		  if (*src == '\n')
		    ONE_MORE_BYTE (c1);
		}
	      else if (EQ (eol_type, Qmac))
		c1 = '\n';
	    }
	  /* fall through */

	case ISO_control_0:
	  MAYBE_FINISH_COMPOSITION ();
	  charset = CHARSET_FROM_ID (charset_ascii);
	  break;

	case ISO_control_1:
	  MAYBE_FINISH_COMPOSITION ();
	  goto invalid_code;

	case ISO_shift_out:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
	      || CODING_ISO_DESIGNATION (coding, 1) < 0)
	    goto invalid_code;
	  CODING_ISO_INVOCATION (coding, 0) = 1;
	  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	  continue;

	case ISO_shift_in:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT))
	    goto invalid_code;
	  CODING_ISO_INVOCATION (coding, 0) = 0;
	  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	  continue;

	case ISO_single_shift_2_7:
	case ISO_single_shift_2:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT))
	    goto invalid_code;
	  /* SS2 is handled as an escape sequence of ESC 'N' */
	  c1 = 'N';
	  goto label_escape_sequence;

	case ISO_single_shift_3:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT))
	    goto invalid_code;
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
	  /* Escape sequences handled here are invocation,
	     designation, direction specification, and character
	     composition specification.  */
	  switch (c1)
	    {
	    case '&':		/* revision of following character set */
	      ONE_MORE_BYTE (c1);
	      if (!(c1 >= '@' && c1 <= '~'))
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 != ISO_CODE_ESC)
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      goto label_escape_sequence;

	    case '$':		/* designation of 2-byte character set */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATION))
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 >= '@' && c1 <= 'B')
		{	/* designation of JISX0208.1978, GB2312.1980,
			   or JISX0208.1980 */
		  DECODE_DESIGNATION (0, 2, 0, c1);
		}
	      else if (c1 >= 0x28 && c1 <= 0x2B)
		{	/* designation of DIMENSION2_CHARS94 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x28, 2, 0, c2);
		}
	      else if (c1 >= 0x2C && c1 <= 0x2F)
		{	/* designation of DIMENSION2_CHARS96 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x2C, 2, 1, c2);
		}
	      else
		goto invalid_code;
	      /* We must update these variables now.  */
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
	      continue;

	    case 'n':		/* invocation of locking-shift-2 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 2) < 0)
		goto invalid_code;
	      CODING_ISO_INVOCATION (coding, 0) = 2;
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      continue;

	    case 'o':		/* invocation of locking-shift-3 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 3) < 0)
		goto invalid_code;
	      CODING_ISO_INVOCATION (coding, 0) = 3;
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      continue;

	    case 'N':		/* invocation of single-shift-2 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 2) < 0)
		goto invalid_code;
	      charset = CHARSET_FROM_ID (CODING_ISO_DESIGNATION (coding, 2));
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto invalid_code;
	      break;

	    case 'O':		/* invocation of single-shift-3 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 3) < 0)
		goto invalid_code;
	      charset = CHARSET_FROM_ID (CODING_ISO_DESIGNATION (coding, 3));
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto invalid_code;
	      break;

	    case '0': case '2':	case '3': case '4': /* start composition */
	      if (! (coding->common_flags & CODING_ANNOTATE_COMPOSITION_MASK))
		goto invalid_code;
	      DECODE_COMPOSITION_START (c1);
	      continue;

	    case '1':		/* end composition */
	      if (composition_state == COMPOSING_NO)
		goto invalid_code;
	      DECODE_COMPOSITION_END ();
	      continue;

	    case '[':		/* specification of direction */
	      if (! CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DIRECTION)
		goto invalid_code;
	      /* For the moment, nested direction is not supported.
		 So, `coding->mode & CODING_MODE_DIRECTION' zero means
		 left-to-right, and nozero means right-to-left.  */
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
		    goto invalid_code;
		  break;

		case '2':	/* start of right-to-left direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->mode |= CODING_MODE_DIRECTION;
		  else
		    goto invalid_code;
		  break;

		default:
		  goto invalid_code;
		}
	      continue;

	    default:
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATION))
		goto invalid_code;
	      if (c1 >= 0x28 && c1 <= 0x2B)
		{	/* designation of DIMENSION1_CHARS94 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x28, 1, 0, c2);
		}
	      else if (c1 >= 0x2C && c1 <= 0x2F)
		{	/* designation of DIMENSION1_CHARS96 character set */
		  ONE_MORE_BYTE (c2);
		  DECODE_DESIGNATION (c1 - 0x2C, 1, 1, c2);
		}
	      else
		goto invalid_code;
	      /* We must update these variables now.  */
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
	      continue;
	    }
	}

      /* Now we know CHARSET and 1st position code C1 of a character.
         Produce a decoded character while getting 2nd position code
         C2 if necessary.  */
      c1 &= 0x7F;
      if (CHARSET_DIMENSION (charset) > 1)
	{
	  ONE_MORE_BYTE (c2);
	  if (c2 < 0x20 || (c2 >= 0x80 && c2 < 0xA0))
	    /* C2 is not in a valid range.  */
	    goto invalid_code;
	  c1 = (c1 << 8) | (c2 & 0x7F);
	  if (CHARSET_DIMENSION (charset) > 2)
	    {
	      ONE_MORE_BYTE (c2);
	      if (c2 < 0x20 || (c2 >= 0x80 && c2 < 0xA0))
		/* C2 is not in a valid range.  */
		goto invalid_code;
	      c1 = (c1 << 8) | (c2 & 0x7F);
	    }
	}

      CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c1, c);
      if (c < 0)
	{
	  MAYBE_FINISH_COMPOSITION ();
	  for (; src_base < src; src_base++, char_offset++)
	    {
	      if (ASCII_BYTE_P (*src_base))
		*charbuf++ = *src_base;
	      else
		*charbuf++ = BYTE8_TO_CHAR (*src_base);
	    }
	}
      else if (composition_state == COMPOSING_NO)
	{
	  *charbuf++ = c;
	  char_offset++;
	}
      else
	components[component_idx++] = c;
      continue;

    invalid_code:
      MAYBE_FINISH_COMPOSITION ();
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


/* ISO2022 encoding stuff.  */

/*
   It is not enough to say just "ISO2022" on encoding, we have to
   specify more details.  In Emacs, each coding system of ISO2022
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
   These specifications are encoded in CODING_ISO_FLAGS (coding) as flag bits
   defined by macros CODING_ISO_FLAG_XXX.  See `coding.h' for more
   details.
*/

/* Produce codes (escape sequence) for designating CHARSET to graphic
   register REG at DST, and increment DST.  If <final-char> of CHARSET is
   '@', 'A', or 'B' and the coding system CODING allows, produce
   designation sequence of short-form.  */

#define ENCODE_DESIGNATION(charset, reg, coding)			\
  do {									\
    unsigned char final_char = CHARSET_ISO_FINAL (charset);		\
    char *intermediate_char_94 = "()*+";				\
    char *intermediate_char_96 = ",-./";				\
    int revision = -1;							\
    int c;								\
									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_REVISION)		\
      revision = XINT (CHARSET_ISO_REVISION (charset));			\
									\
    if (revision >= 0)							\
      {									\
	EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, '&');			\
	EMIT_ONE_BYTE ('@' + revision);					\
      }									\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_ESC);					\
    if (CHARSET_DIMENSION (charset) == 1)				\
      {									\
	if (! CHARSET_ISO_CHARS_96 (charset))				\
	  c = intermediate_char_94[reg];				\
	else								\
	  c = intermediate_char_96[reg];				\
	EMIT_ONE_ASCII_BYTE (c);					\
      }									\
    else								\
      {									\
	EMIT_ONE_ASCII_BYTE ('$');					\
	if (! CHARSET_ISO_CHARS_96 (charset))				\
	  {								\
	    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LONG_FORM	\
		|| reg != 0						\
		|| final_char < '@' || final_char > 'B')		\
	      EMIT_ONE_ASCII_BYTE (intermediate_char_94[reg]);		\
	  }								\
	else								\
	  EMIT_ONE_ASCII_BYTE (intermediate_char_96[reg]);		\
      }									\
    EMIT_ONE_ASCII_BYTE (final_char);					\
									\
    CODING_ISO_DESIGNATION (coding, reg) = CHARSET_ID (charset);	\
  } while (0)


/* The following two macros produce codes (control character or escape
   sequence) for ISO2022 single-shift functions (single-shift-2 and
   single-shift-3).  */

#define ENCODE_SINGLE_SHIFT_2						\
  do {									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)		\
      EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'N');				\
    else								\
      EMIT_ONE_BYTE (ISO_CODE_SS2);					\
    CODING_ISO_SINGLE_SHIFTING (coding) = 1;				\
  } while (0)


#define ENCODE_SINGLE_SHIFT_3						\
  do {									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)		\
      EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'O');				\
    else								\
      EMIT_ONE_BYTE (ISO_CODE_SS3);					\
    CODING_ISO_SINGLE_SHIFTING (coding) = 1;				\
  } while (0)


/* The following four macros produce codes (control character or
   escape sequence) for ISO2022 locking-shift functions (shift-in,
   shift-out, locking-shift-2, and locking-shift-3).  */

#define ENCODE_SHIFT_IN					\
  do {							\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_SI);			\
    CODING_ISO_INVOCATION (coding, 0) = 0;		\
  } while (0)


#define ENCODE_SHIFT_OUT				\
  do {							\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_SO);			\
    CODING_ISO_INVOCATION (coding, 0) = 1;		\
  } while (0)


#define ENCODE_LOCKING_SHIFT_2				\
  do {							\
    EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'n');		\
    CODING_ISO_INVOCATION (coding, 0) = 2;		\
  } while (0)


#define ENCODE_LOCKING_SHIFT_3				\
  do {							\
    EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'n');		\
    CODING_ISO_INVOCATION (coding, 0) = 3;		\
  } while (0)


/* Produce codes for a DIMENSION1 character whose character set is
   CHARSET and whose position-code is C1.  Designation and invocation
   sequences are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION1(charset, c1)			\
  do {									\
    int id = CHARSET_ID (charset);					\
    if (CODING_ISO_SINGLE_SHIFTING (coding))				\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)	\
	  EMIT_ONE_ASCII_BYTE (c1 & 0x7F);				\
	else								\
	  EMIT_ONE_BYTE (c1 | 0x80);					\
	CODING_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 0))		\
      {									\
	EMIT_ONE_ASCII_BYTE (c1 & 0x7F);				\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 1))		\
      {									\
	EMIT_ONE_BYTE (c1 | 0x80);					\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst,	\
					   &produced_chars);		\
  } while (1)


/* Produce codes for a DIMENSION2 character whose character set is
   CHARSET and whose position-codes are C1 and C2.  Designation and
   invocation codes are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION2(charset, c1, c2)		\
  do {									\
    int id = CHARSET_ID (charset);					\
    if (CODING_ISO_SINGLE_SHIFTING (coding))				\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)	\
	  EMIT_TWO_ASCII_BYTES ((c1) & 0x7F, (c2) & 0x7F);		\
	else								\
	  EMIT_TWO_BYTES ((c1) | 0x80, (c2) | 0x80);			\
	CODING_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 0))		\
      {									\
	EMIT_TWO_ASCII_BYTES ((c1) & 0x7F, (c2) & 0x7F);		\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 1))		\
      {									\
	EMIT_TWO_BYTES ((c1) | 0x80, (c2) | 0x80);			\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst,	\
					   &produced_chars);		\
  } while (1)


#define ENCODE_ISO_CHARACTER(charset, c)				   \
  do {									   \
    int code = ENCODE_CHAR ((charset),(c));				   \
									   \
    if (CHARSET_DIMENSION (charset) == 1)				   \
      ENCODE_ISO_CHARACTER_DIMENSION1 ((charset), code);		   \
    else								   \
      ENCODE_ISO_CHARACTER_DIMENSION2 ((charset), code >> 8, code & 0xFF); \
  } while (0)


/* Produce designation and invocation codes at a place pointed by DST
   to use CHARSET.  The element `spec.iso_2022' of *CODING is updated.
   Return new DST.  */

unsigned char *
encode_invocation_designation (charset, coding, dst, p_nchars)
     struct charset *charset;
     struct coding_system *coding;
     unsigned char *dst;
     int *p_nchars;
{
  int multibytep = coding->dst_multibyte;
  int produced_chars = *p_nchars;
  int reg;			/* graphic register number */
  int id = CHARSET_ID (charset);

  /* At first, check designations.  */
  for (reg = 0; reg < 4; reg++)
    if (id == CODING_ISO_DESIGNATION (coding, reg))
      break;

  if (reg >= 4)
    {
      /* CHARSET is not yet designated to any graphic registers.  */
      /* At first check the requested designation.  */
      reg = CODING_ISO_REQUEST (coding, id);
      if (reg < 0)
	/* Since CHARSET requests no special designation, designate it
	   to graphic register 0.  */
	reg = 0;

      ENCODE_DESIGNATION (charset, reg, coding);
    }

  if (CODING_ISO_INVOCATION (coding, 0) != reg
      && CODING_ISO_INVOCATION (coding, 1) != reg)
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
	  if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_2;
	  else
	    ENCODE_LOCKING_SHIFT_2;
	  break;

	case 3:			/* graphic register 3 */
	  if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_3;
	  else
	    ENCODE_LOCKING_SHIFT_3;
	  break;
	}
    }

  *p_nchars = produced_chars;
  return dst;
}

/* The following three macros produce codes for indicating direction
   of text.  */
#define ENCODE_CONTROL_SEQUENCE_INTRODUCER				\
  do {									\
    if (CODING_ISO_FLAGS (coding) == CODING_ISO_FLAG_SEVEN_BITS)	\
      EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, '[');				\
    else								\
      EMIT_ONE_BYTE (ISO_CODE_CSI);					\
  } while (0)


#define ENCODE_DIRECTION_R2L()			\
  do {						\
    ENCODE_CONTROL_SEQUENCE_INTRODUCER (dst);	\
    EMIT_TWO_ASCII_BYTES ('2', ']');		\
  } while (0)


#define ENCODE_DIRECTION_L2R()			\
  do {						\
    ENCODE_CONTROL_SEQUENCE_INTRODUCER (dst);	\
    EMIT_TWO_ASCII_BYTES ('0', ']');		\
  } while (0)


/* Produce codes for designation and invocation to reset the graphic
   planes and registers to initial state.  */
#define ENCODE_RESET_PLANE_AND_REGISTER()				\
  do {									\
    int reg;								\
    struct charset *charset;						\
									\
    if (CODING_ISO_INVOCATION (coding, 0) != 0)				\
      ENCODE_SHIFT_IN;							\
    for (reg = 0; reg < 4; reg++)					\
      if (CODING_ISO_INITIAL (coding, reg) >= 0				\
	  && (CODING_ISO_DESIGNATION (coding, reg)			\
	      != CODING_ISO_INITIAL (coding, reg)))			\
	{								\
	  charset = CHARSET_FROM_ID (CODING_ISO_INITIAL (coding, reg));	\
	  ENCODE_DESIGNATION (charset, reg, coding);			\
	}								\
  } while (0)


/* Produce designation sequences of charsets in the line started from
   SRC to a place pointed by DST, and return updated DST.

   If the current block ends before any end-of-line, we may fail to
   find all the necessary designations.  */

static unsigned char *
encode_designation_at_bol (coding, charbuf, charbuf_end, dst)
     struct coding_system *coding;
     int *charbuf, *charbuf_end;
     unsigned char *dst;
{
  struct charset *charset;
  /* Table of charsets to be designated to each graphic register.  */
  int r[4];
  int c, found = 0, reg;
  int produced_chars = 0;
  int multibytep = coding->dst_multibyte;
  Lisp_Object attrs;
  Lisp_Object charset_list;

  attrs = CODING_ID_ATTRS (coding->id);
  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  if (EQ (charset_list, Qiso_2022))
    charset_list = Viso_2022_charset_list;

  for (reg = 0; reg < 4; reg++)
    r[reg] = -1;

  while (found < 4)
    {
      int id;

      c = *charbuf++;
      if (c == '\n')
	break;
      charset = char_charset (c, charset_list, NULL);
      id = CHARSET_ID (charset);
      reg = CODING_ISO_REQUEST (coding, id);
      if (reg >= 0 && r[reg] < 0)
	{
	  found++;
	  r[reg] = id;
	}
    }

  if (found)
    {
      for (reg = 0; reg < 4; reg++)
	if (r[reg] >= 0
	    && CODING_ISO_DESIGNATION (coding, reg) != r[reg])
	  ENCODE_DESIGNATION (CHARSET_FROM_ID (r[reg]), reg, coding);
    }

  return dst;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".  */

static int
encode_coding_iso_2022 (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 16;
  int bol_designation
    = (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATE_AT_BOL
       && CODING_ISO_BOL (coding));
  int produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list;
  int ascii_compatible;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);

  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);

      if (bol_designation)
	{
	  unsigned char *dst_prev = dst;

	  /* We have to produce designation sequences if any now.  */
	  dst = encode_designation_at_bol (coding, charbuf, charbuf_end, dst);
	  bol_designation = 0;
	  /* We are sure that designation sequences are all ASCII bytes.  */
	  produced_chars += dst - dst_prev;
	}

      c = *charbuf++;

      /* Now encode the character C.  */
      if (c < 0x20 || c == 0x7F)
	{
	  if (c == '\n'
	      || (c == '\r' && EQ (eol_type, Qmac)))
	    {
	      if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_EOL)
		ENCODE_RESET_PLANE_AND_REGISTER ();
	      if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_INIT_AT_BOL)
		{
		  int i;

		  for (i = 0; i < 4; i++)
		    CODING_ISO_DESIGNATION (coding, i)
		      = CODING_ISO_INITIAL (coding, i);
		}
	      bol_designation
		= CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATE_AT_BOL;
	    }
	  else if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_CNTL)
	    ENCODE_RESET_PLANE_AND_REGISTER ();
	  EMIT_ONE_ASCII_BYTE (c);
	}
      else if (ASCII_CHAR_P (c))
	{
	  if (ascii_compatible)
	    EMIT_ONE_ASCII_BYTE (c);
	  else
	    ENCODE_ISO_CHARACTER (CHARSET_FROM_ID (charset_ascii), c);
	}
      else
	{
	  struct charset *charset = char_charset (c, charset_list, NULL);

	  if (!charset)
	    {
	      c = coding->default_char;
	      charset = char_charset (c, charset_list, NULL);
	    }
	  ENCODE_ISO_CHARACTER (charset, c);
	}
    }

  if (coding->mode & CODING_MODE_LAST_BLOCK
      && CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_EOL)
    {
      ASSURE_DESTINATION (safe_room);
      ENCODE_RESET_PLANE_AND_REGISTER ();
    }
  coding->result = CODING_RESULT_SUCCESS;
  CODING_ISO_BOL (coding) = bol_designation;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 8,9. SJIS and BIG5 handlers ***/

/* Although SJIS and BIG5 are not ISO's coding system, they are used
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
   JISX0208 (1st byte)	0x81 .. 0x9F and 0xE0 .. 0xEF
	    (2nd byte)	0x40 .. 0x7E and 0x80 .. 0xFC
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

  */

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in SJIS.  If it is, return
   CATEGORY_MASK_SJIS, else return 0.  */

static int
detect_coding_sjis (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int found = 0;
  int c;

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      ONE_MORE_BYTE (c);
      if (c < 0x80)
	continue;
      if ((c >= 0x81 && c <= 0x9F) || (c >= 0xE0 && c <= 0xEF))
	{
	  ONE_MORE_BYTE (c);
	  if (c < 0x40 || c == 0x7F || c > 0xFC)
	    break;
	  found = 1;
	}
      else if (c >= 0xA0 && c < 0xE0)
	found = 1;
      else
	break;
    }
  *mask &= ~CATEGORY_MASK_SJIS;
  return 0;

 no_more_source:
  if (!found)
    return 0;
  *mask &= CATEGORY_MASK_SJIS;
  return 1;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in BIG5.  If it is, return
   CATEGORY_MASK_BIG5, else return 0.  */

static int
detect_coding_big5 (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int found = 0;
  int c;

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      ONE_MORE_BYTE (c);
      if (c < 0x80)
	continue;
      if (c >= 0xA1)
	{
	  ONE_MORE_BYTE (c);
	  if (c < 0x40 || (c >= 0x7F && c <= 0xA0))
	    return 0;
	  found = 1;
	}
      else
	break;
    }
  *mask &= ~CATEGORY_MASK_BIG5;
  return 0;

 no_more_source:
  if (!found)
    return 0;
  *mask &= CATEGORY_MASK_BIG5;
  return 1;
}

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".
   If SJIS_P is 1, decode SJIS text, else decode BIG5 test.  */

static void
decode_coding_sjis (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  struct charset *charset_roman, *charset_kanji, *charset_kana;
  Lisp_Object attrs, eol_type, charset_list, val;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);

  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val)));

  while (1)
    {
      int c, c1;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c);

      if (c == '\r')
	{
	  if (EQ (eol_type, Qdos))
	    {
	      if (src == src_end)
		goto no_more_source;
	      if (*src == '\n')
		ONE_MORE_BYTE (c);
	    }
	  else if (EQ (eol_type, Qmac))
	    c = '\n';
	}
      else
	{
	  struct charset *charset;

	  if (c < 0x80)
	    charset = charset_roman;
	  else
	    {
	      if (c >= 0xF0)
		goto invalid_code;
	      if (c < 0xA0 || c >= 0xE0)
		{
		  /* SJIS -> JISX0208 */
		  ONE_MORE_BYTE (c1);
		  if (c1 < 0x40 || c1 == 0x7F || c1 > 0xFC)
		    goto invalid_code;
		  c = (c << 8) | c1;
		  SJIS_TO_JIS (c);
		  charset = charset_kanji;
		}
	      else
		/* SJIS -> JISX0201-Kana */
		charset = charset_kana;
	    }
	  CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c, c);
	}
      *charbuf++ = c;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static void
decode_coding_big5 (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  struct charset *charset_roman, *charset_big5;
  Lisp_Object attrs, eol_type, charset_list, val;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));

  while (1)
    {
      int c, c1;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c);

      if (c == '\r')
	{
	  if (EQ (eol_type, Qdos))
	    {
	      if (src == src_end)
		goto no_more_source;
	      if (*src == '\n')
		ONE_MORE_BYTE (c);
	    }
	  else if (EQ (eol_type, Qmac))
	    c = '\n';
	}
      else
	{
	  struct charset *charset;
	  if (c < 0x80)
	    charset = charset_roman;
	  else
	    {
	      /* BIG5 -> Big5 */
	      if (c < 0xA1 || c > 0xFE)
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x40 || (c1 > 0x7E && c1 < 0xA1) || c1 > 0xFE)
		goto invalid_code;
	      c = c << 8 | c1;
	      charset = charset_big5;
	    }
	  CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c, c);
	}

      *charbuf++ = c;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".
   This function can encode charsets `ascii', `katakana-jisx0201',
   `japanese-jisx0208', `chinese-big5-1', and `chinese-big5-2'.  We
   are sure that all these charsets are registered as official charset
   (i.e. do not have extended leading-codes).  Characters of other
   charsets are produced without any encoding.  If SJIS_P is 1, encode
   SJIS text, else encode BIG5 text.  */

static int
encode_coding_sjis (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 4;
  int produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list, val;
  int ascii_compatible;
  struct charset *charset_roman, *charset_kanji, *charset_kana;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val)));

  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      /* Now encode the character C.  */
      if (ASCII_CHAR_P (c) && ascii_compatible)
	EMIT_ONE_ASCII_BYTE (c);
      else
	{
	  unsigned code;
	  struct charset *charset = char_charset (c, charset_list, &code);

	  if (!charset)
	    {
	      c = coding->default_char;
	      charset = char_charset (c, charset_list, &code);
	    }
	  if (code == CHARSET_INVALID_CODE (charset))
	    abort ();
	  if (charset == charset_kanji)
	    {
	      int c1, c2;
	      JIS_TO_SJIS (code);
	      c1 = code >> 8, c2 = code & 0xFF;
	      EMIT_TWO_BYTES (c1, c2);
	    }
	  else if (charset == charset_kana)
	    EMIT_ONE_BYTE (code | 0x80);
	  else
	    EMIT_ONE_ASCII_BYTE (code & 0x7F);
	}
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}

static int
encode_coding_big5 (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 4;
  int produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list, val;
  int ascii_compatible;
  struct charset *charset_roman, *charset_big5;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));
  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      /* Now encode the character C.  */
      if (ASCII_CHAR_P (c) && ascii_compatible)
	EMIT_ONE_ASCII_BYTE (c);
      else
	{
	  unsigned code;
	  struct charset *charset = char_charset (c, charset_list, &code);

	  if (! charset)
	    {
	      c = coding->default_char;
	      charset = char_charset (c, charset_list, &code);
	    }
	  if (code == CHARSET_INVALID_CODE (charset))
	    abort ();
	  if (charset == charset_big5)
	    {
	      int c1, c2;

	      c1 = code >> 8, c2 = code & 0xFF;
	      EMIT_TWO_BYTES (c1, c2);
	    }
	  else
	    EMIT_ONE_ASCII_BYTE (code & 0x7F);
	}
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 10. CCL handlers ***/

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in a coding system of which
   encoder/decoder are written in CCL program.  If it is, return
   CATEGORY_MASK_CCL, else return 0.  */

static int
detect_coding_ccl (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  int found = 0;
  unsigned char *valids = CODING_CCL_VALIDS (coding);
  int head_ascii = coding->head_ascii;
  Lisp_Object attrs;

  coding = &coding_categories[coding_category_ccl];
  attrs = CODING_ID_ATTRS (coding->id);
  if (! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    src += head_ascii;

  while (1)
    {
      int c;
      ONE_MORE_BYTE (c);
      if (! valids[c])
	break;
      if (!found && valids[c] > 1)
	found = 1;
    }
  *mask &= ~CATEGORY_MASK_CCL;
  return 0;

 no_more_source:
  if (!found)
    return 0;
  *mask &= CATEGORY_MASK_CCL;
  return 1;
}

static void
decode_coding_ccl (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0;
  int multibytep = coding->src_multibyte;
  struct ccl_program ccl;
  int source_charbuf[1024];
  int source_byteidx[1024];

  setup_ccl_program (&ccl, CODING_CCL_DECODER (coding));

  while (src < src_end)
    {
      unsigned char *p = src;
      int *source, *source_end;
      int i = 0;

      if (multibytep)
	while (i < 1024 && p < src_end)
	  {
	    source_byteidx[i] = p - src;
	    source_charbuf[i++] = STRING_CHAR_ADVANCE (p);
	  }
      else
	while (i < 1024 && p < src_end)
	  source_charbuf[i++] = *p++;
      
      if (p == src_end && coding->mode & CODING_MODE_LAST_BLOCK)
	ccl.last_block = 1;

      source = source_charbuf;
      source_end = source + i;
      while (source < source_end)
	{
	  ccl_driver (&ccl, source, charbuf,
		      source_end - source, charbuf_end - charbuf);
	  source += ccl.consumed;
	  charbuf += ccl.produced;
	  if (ccl.status != CCL_STAT_SUSPEND_BY_DST)
	    break;
	}
      if (source < source_end)
	src += source_byteidx[source - source_charbuf];
      else
	src = p;
      consumed_chars += source - source_charbuf;

      if (ccl.status != CCL_STAT_SUSPEND_BY_SRC
	  && ccl.status != CODING_RESULT_INSUFFICIENT_SRC)
	break;
    }

  switch (ccl.status)
    {
    case CCL_STAT_SUSPEND_BY_SRC:
      coding->result = CODING_RESULT_INSUFFICIENT_SRC;
      break;
    case CCL_STAT_SUSPEND_BY_DST:
      break;
    case CCL_STAT_QUIT:
    case CCL_STAT_INVALID_CMD:
      coding->result = CODING_RESULT_INTERRUPT;
      break;
    default:
      coding->result = CODING_RESULT_SUCCESS;
      break;
    }
  coding->consumed_char += consumed_chars;
  coding->consumed = src - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_ccl (coding)
     struct coding_system *coding;
{
  struct ccl_program ccl;
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  unsigned char *adjusted_dst_end = dst_end - 1;
  int destination_charbuf[1024];
  int i, produced_chars = 0;

  setup_ccl_program (&ccl, CODING_CCL_ENCODER (coding));

  ccl.last_block = coding->mode & CODING_MODE_LAST_BLOCK;
  ccl.dst_multibyte = coding->dst_multibyte;

  while (charbuf < charbuf_end && dst < adjusted_dst_end)
    {
      int dst_bytes = dst_end - dst;
      if (dst_bytes > 1024)
	dst_bytes = 1024;

      ccl_driver (&ccl, charbuf, destination_charbuf,
		  charbuf_end - charbuf, dst_bytes);
      charbuf += ccl.consumed;
      if (multibytep)
	for (i = 0; i < ccl.produced; i++)
	  EMIT_ONE_BYTE (destination_charbuf[i] & 0xFF);
      else
	{
	  for (i = 0; i < ccl.produced; i++)	
	    *dst++ = destination_charbuf[i] & 0xFF;
	  produced_chars += ccl.produced;
	}
    }

  switch (ccl.status)
    {
    case CCL_STAT_SUSPEND_BY_SRC:
      coding->result = CODING_RESULT_INSUFFICIENT_SRC;
      break;
    case CCL_STAT_SUSPEND_BY_DST:
      coding->result = CODING_RESULT_INSUFFICIENT_DST;
      break;
    case CCL_STAT_QUIT:
    case CCL_STAT_INVALID_CMD:
      coding->result = CODING_RESULT_INTERRUPT;
      break;
    default:
      coding->result = CODING_RESULT_SUCCESS;
      break;
    }

  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}



/*** 10, 11. no-conversion handlers ***/

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_raw_text (coding)
     struct coding_system *coding;
{
  coding->chars_at_source = 1;
  coding->consumed_char = 0;
  coding->consumed = 0;
  coding->result = CODING_RESULT_SUCCESS;
}

static int
encode_coding_raw_text (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = coding->charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int produced_chars = 0;
  int c;

  if (multibytep)
    {
      int safe_room = MAX_MULTIBYTE_LENGTH * 2;

      if (coding->src_multibyte)
	while (charbuf < charbuf_end)
	  {
	    ASSURE_DESTINATION (safe_room);
	    c = *charbuf++;
	    if (ASCII_CHAR_P (c))
	      EMIT_ONE_ASCII_BYTE (c);
	    else if (CHAR_BYTE8_P (c))
	      {
		c = CHAR_TO_BYTE8 (c);
		EMIT_ONE_BYTE (c);
	      }
	    else
	      {
		unsigned char str[MAX_MULTIBYTE_LENGTH], *p0 = str, *p1 = str;

		CHAR_STRING_ADVANCE (c, p1);
		while (p0 < p1)
		  EMIT_ONE_BYTE (*p0);
	      }
	  }
      else
	while (charbuf < charbuf_end)
	  {
	    ASSURE_DESTINATION (safe_room);
	    c = *charbuf++;
	    EMIT_ONE_BYTE (c);
	  }
    }
  else
    {
      if (coding->src_multibyte)
	{
	  int safe_room = MAX_MULTIBYTE_LENGTH;

	  while (charbuf < charbuf_end)
	    {
	      ASSURE_DESTINATION (safe_room);
	      c = *charbuf++;
	      if (ASCII_CHAR_P (c))
		*dst++ = c;
	      else if (CHAR_BYTE8_P (c))
		*dst++ = CHAR_TO_BYTE8 (c);
	      else
		CHAR_STRING_ADVANCE (c, dst);
	      produced_chars++;
	    }
	}
      else
	{
	  ASSURE_DESTINATION (charbuf_end - charbuf);
	  while (charbuf < charbuf_end && dst < dst_end)
	    *dst++ = *charbuf++;
	  produced_chars = dst - (coding->destination + coding->dst_bytes);
	} 
    }
  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}

static int
detect_coding_charset (coding, mask)
     struct coding_system *coding;
     int *mask;
{
  unsigned char *src = coding->source, *src_base = src;
  unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int consumed_chars = 0;
  Lisp_Object attrs, valids;

  coding = &coding_categories[coding_category_charset];
  attrs = CODING_ID_ATTRS (coding->id);
  valids = AREF (attrs, coding_attr_charset_valids);

  if (! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    src += coding->head_ascii;

  while (1)
    {
      int c;

      ONE_MORE_BYTE (c);
      if (NILP (AREF (valids, c)))
	break;
    }
  *mask &= ~CATEGORY_MASK_CHARSET;
  return 0;

 no_more_source:
  *mask &= CATEGORY_MASK_CHARSET;
  return 1;
}

static void
decode_coding_charset (coding)
     struct coding_system *coding;
{
  unsigned char *src = coding->source + coding->consumed;
  unsigned char *src_end = coding->source + coding->src_bytes;
  unsigned char *src_base;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_size;
  int consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  Lisp_Object attrs, eol_type, charset_list, valids;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  valids = AREF (attrs, coding_attr_charset_valids);

  while (1)
    {
      int c;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	break;

      ONE_MORE_BYTE (c);
      if (c == '\r')
	{
	  if (EQ (eol_type, Qdos))
	    {
	      if (src < src_end
		  && *src == '\n')
		ONE_MORE_BYTE (c);
	    }
	  else if (EQ (eol_type, Qmac))
	    c = '\n';
	}
      else
	{
	  Lisp_Object val;
	  struct charset *charset;
	  int c1;

	  val = AREF (valids, c);
	  if (NILP (val))
	    goto invalid_code;
	  charset = CHARSET_FROM_ID (XFASTINT (val));
	  if (CHARSET_DIMENSION (charset) > 1)
	    {
	      ONE_MORE_BYTE (c1);
	      c = (c << 8) | c1;
	      if (CHARSET_DIMENSION (charset) > 2)
		{
		  ONE_MORE_BYTE (c1);
		  c = (c << 8) | c1;
		  if (CHARSET_DIMENSION (charset) > 3)
		    {
		      ONE_MORE_BYTE (c1);
		      c = (c << 8) | c1;
		    }
		}
	    }
	  CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c, c);
	  if (c < 0)
	    goto invalid_code;
	}
      *charbuf++ = c;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_charset (coding)
     struct coding_system *coding;
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = MAX_MULTIBYTE_LENGTH;
  int produced_chars = 0;
  struct charset *charset;
  Lisp_Object attrs, eol_type, charset_list;
  int ascii_compatible;
  int c;

  CODING_GET_INFO (coding, attrs, eol_type, charset_list);
  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      struct charset *charset;
      unsigned code;
      
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      if (ascii_compatible && ASCII_CHAR_P (c))
	EMIT_ONE_ASCII_BYTE (c);
      else
	{
	  charset = char_charset (c, charset_list, &code);
	  if (charset)
	    {
	      if (CHARSET_DIMENSION (charset) == 1)
		EMIT_ONE_BYTE (code);
	      else if (CHARSET_DIMENSION (charset) == 2)
		EMIT_TWO_BYTES (code >> 8, code & 0xFF);
	      else if (CHARSET_DIMENSION (charset) == 3)
		EMIT_THREE_BYTES (code >> 16, (code >> 8) & 0xFF, code & 0xFF);
	      else
		EMIT_FOUR_BYTES (code >> 24, (code >> 16) & 0xFF,
				 (code >> 8) & 0xFF, code & 0xFF);
	    }
	  else
	    EMIT_ONE_BYTE (coding->default_char);
	}
    }

  coding->result = CODING_RESULT_SUCCESS;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 7. C library functions ***/

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

   0 -- coding_type_emacs_mule
   1 -- coding_type_sjis
   2 -- coding_type_iso_2022
   3 -- coding_type_big5
   4 -- coding_type_ccl encoder/decoder written in CCL
   nil -- coding_type_no_conversion
   t -- coding_type_undecided (automatic conversion on decoding,
   			       no-conversion on encoding)

   `element[4]' contains information to be set in `coding->flags' and
   `coding->spec'.  The meaning varies by `coding->type'.

   If `coding->type' is `coding_type_iso_2022', element[4] is a vector
   of length 32 (of which the first 13 sub-elements are used now).
   Meanings of these sub-elements are:

   sub-element[N] where N is 0 through 3: to be set in `coding->spec.iso_2022'
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
   integer, 0 means eol_lf, 1 means eol_crlf, and 2 means eol_cr.  If
   it is not integer, it should be a vector of subsidiary coding
   systems of which property `eol-type' has one of above values.

*/

/* Setup coding context CODING from information about CODING_SYSTEM.
   If CODING_SYSTEM is nil, `no-conversion' is assumed.  If
   CODING_SYSTEM is invalid, signal an error.  */

void
setup_coding_system (coding_system, coding)
     Lisp_Object coding_system;
     struct coding_system *coding;
{
  Lisp_Object attrs;
  Lisp_Object eol_type;
  Lisp_Object coding_type;
  Lisp_Object val;

  if (NILP (coding_system))
    coding_system = Qno_conversion;

  CHECK_CODING_SYSTEM_GET_ID (coding_system, coding->id);

  attrs = CODING_ID_ATTRS (coding->id);
  eol_type = CODING_ID_EOL_TYPE (coding->id);

  coding->mode = 0;
  coding->head_ascii = -1;
  coding->common_flags
    = (VECTORP (eol_type) ? CODING_REQUIRE_DETECTION_MASK : 0);

  val = CODING_ATTR_SAFE_CHARSETS (attrs);
  coding->max_charset_id = XSTRING (val)->size - 1;
  coding->safe_charsets = (char *) XSTRING (val)->data;
  coding->default_char = XINT (CODING_ATTR_DEFAULT_CHAR (attrs));

  coding_type = CODING_ATTR_TYPE (attrs);
  if (EQ (coding_type, Qundecided))
    {
      coding->detector = NULL;
      coding->decoder = decode_coding_raw_text;
      coding->encoder = encode_coding_raw_text;
      coding->common_flags |= CODING_REQUIRE_DETECTION_MASK;
    }
  else if (EQ (coding_type, Qiso_2022))
    {
      int i;
      int flags = XINT (AREF (attrs, coding_attr_iso_flags));

      /* Invoke graphic register 0 to plane 0.  */
      CODING_ISO_INVOCATION (coding, 0) = 0;
      /* Invoke graphic register 1 to plane 1 if we can use 8-bit.  */
      CODING_ISO_INVOCATION (coding, 1)
	= (flags & CODING_ISO_FLAG_SEVEN_BITS ? -1 : 1);
      /* Setup the initial status of designation.  */
      for (i = 0; i < 4; i++)
	CODING_ISO_DESIGNATION (coding, i) = CODING_ISO_INITIAL (coding, i);
      /* Not single shifting initially.  */
      CODING_ISO_SINGLE_SHIFTING (coding) = 0;
      /* Beginning of buffer should also be regarded as bol. */
      CODING_ISO_BOL (coding) = 1;
      coding->detector = detect_coding_iso_2022;
      coding->decoder = decode_coding_iso_2022;
      coding->encoder = encode_coding_iso_2022;
      if (flags & CODING_ISO_FLAG_SAFE)
	coding->mode |= CODING_MODE_SAFE_ENCODING;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK
	    | CODING_REQUIRE_FLUSHING_MASK);
      if (flags & CODING_ISO_FLAG_COMPOSITION)
	coding->common_flags |= CODING_ANNOTATE_COMPOSITION_MASK;
      if (flags & CODING_ISO_FLAG_FULL_SUPPORT)
	{
	  setup_iso_safe_charsets (attrs);
	  val = CODING_ATTR_SAFE_CHARSETS (attrs);
	  coding->max_charset_id = XSTRING (val)->size - 1;
	  coding->safe_charsets = (char *) XSTRING (val)->data;
	}
      CODING_ISO_FLAGS (coding) = flags;
    }
  else if (EQ (coding_type, Qcharset))
    {
      coding->detector = detect_coding_charset;
      coding->decoder = decode_coding_charset;
      coding->encoder = encode_coding_charset;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qutf_8))
    {
      coding->detector = detect_coding_utf_8;
      coding->decoder = decode_coding_utf_8;
      coding->encoder = encode_coding_utf_8;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qutf_16))
    {
      val = AREF (attrs, coding_attr_utf_16_bom);
      CODING_UTF_16_BOM (coding) = (CONSP (val) ? utf_16_detect_bom
				    : EQ (val, Qt) ? utf_16_with_bom
				    : utf_16_without_bom);
      val = AREF (attrs, coding_attr_utf_16_endian);
      CODING_UTF_16_ENDIAN (coding) = (NILP (val) ? utf_16_big_endian
				       : utf_16_little_endian);
      CODING_UTF_16_SURROGATE (coding) = 0;
      coding->detector = detect_coding_utf_16;
      coding->decoder = decode_coding_utf_16;
      coding->encoder = encode_coding_utf_16;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qccl))
    {
      coding->detector = detect_coding_ccl;
      coding->decoder = decode_coding_ccl;
      coding->encoder = encode_coding_ccl;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK
	    | CODING_REQUIRE_FLUSHING_MASK);
    }
  else if (EQ (coding_type, Qemacs_mule))
    {
      coding->detector = detect_coding_emacs_mule;
      coding->decoder = decode_coding_emacs_mule;
      coding->encoder = encode_coding_emacs_mule;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
      if (! NILP (AREF (attrs, coding_attr_emacs_mule_full))
	  && ! EQ (CODING_ATTR_CHARSET_LIST (attrs), Vemacs_mule_charset_list))
	{
	  Lisp_Object tail, safe_charsets;
	  int max_charset_id = 0;

	  for (tail = Vemacs_mule_charset_list; CONSP (tail);
	       tail = XCDR (tail))
	    if (max_charset_id < XFASTINT (XCAR (tail)))
	      max_charset_id = XFASTINT (XCAR (tail));
	  safe_charsets = Fmake_string (make_number (max_charset_id + 1),
					make_number (255));
	  for (tail = Vemacs_mule_charset_list; CONSP (tail);
	       tail = XCDR (tail))
	    XSTRING (safe_charsets)->data[XFASTINT (XCAR (tail))] = 0;
	  coding->max_charset_id = max_charset_id;
	  coding->safe_charsets = (char *) XSTRING (safe_charsets)->data;
	}
    }
  else if (EQ (coding_type, Qshift_jis))
    {
      coding->detector = detect_coding_sjis;
      coding->decoder = decode_coding_sjis;
      coding->encoder = encode_coding_sjis;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qbig5))
    {
      coding->detector = detect_coding_big5;
      coding->decoder = decode_coding_big5;
      coding->encoder = encode_coding_big5;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else				/* EQ (coding_type, Qraw_text) */
    {
      coding->detector = NULL;
      coding->decoder = decode_coding_raw_text;
      coding->encoder = encode_coding_raw_text;
      coding->common_flags |= CODING_FOR_UNIBYTE_MASK;
    }

  return;
}

/* Return raw-text or one of its subsidiaries that has the same
   eol_type as CODING-SYSTEM.  */

Lisp_Object
raw_text_coding_system (coding_system)
     Lisp_Object coding_system;
{
  Lisp_Object spec, attrs;
  Lisp_Object eol_type, raw_text_eol_type;

  spec = CODING_SYSTEM_SPEC (coding_system);
  attrs = AREF (spec, 0);
  
  if (EQ (CODING_ATTR_TYPE (attrs), Qraw_text))
    return coding_system;

  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    return Qraw_text;
  spec = CODING_SYSTEM_SPEC (Qraw_text);
  raw_text_eol_type = AREF (spec, 2);
  return (EQ (eol_type, Qunix) ? AREF (raw_text_eol_type, 0)
	  : EQ (eol_type, Qdos) ? AREF (raw_text_eol_type, 1)
	  : AREF (raw_text_eol_type, 2));
}


/* If CODING_SYSTEM doesn't specify end-of-line format but PARENT
   does, return one of the subsidiary that has the same eol-spec as
   PARENT.  Otherwise, return CODING_SYSTEM.  */

Lisp_Object
coding_inherit_eol_type (coding_system, parent)
     Lisp_Object coding_system, parent;
{
  Lisp_Object spec, attrs, eol_type;

  spec = CODING_SYSTEM_SPEC (coding_system);
  attrs = AREF (spec, 0);
  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    {
      Lisp_Object parent_spec;
      Lisp_Object parent_eol_type;

      parent_spec
	= CODING_SYSTEM_SPEC (buffer_defaults.buffer_file_coding_system);
      parent_eol_type = AREF (parent_spec, 2);
      if (EQ (parent_eol_type, Qunix))
	coding_system = AREF (eol_type, 0);
      else if (EQ (parent_eol_type, Qdos))
	coding_system = AREF (eol_type, 1);
      else if (EQ (parent_eol_type, Qmac))
	coding_system = AREF (eol_type, 2);
    }
  return coding_system;
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
	as ISO2022 of 7-bit environemnt but uses locking shift or
	single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-2022-7bit-lock' by default.

   o coding-category-iso-8-else

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environemnt but uses locking shift or
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
   `coding-system's (this is also a Lisp symbol) assigned by a user.
   What Emacs does actually is to detect a category of coding system.
   Then, it uses a `coding-system' assigned to it.  If Emacs can't
   decide only one possible category, it selects a category of the
   highest priority.  Priorities of categories are also specified by a
   user in a Lisp variable `coding-category-list'.

*/

#define EOL_SEEN_NONE	0
#define EOL_SEEN_LF	1
#define EOL_SEEN_CR	2
#define EOL_SEEN_CRLF	4

/* Detect how end-of-line of a text of length CODING->src_bytes
   pointed by CODING->source is encoded.  Return one of
   EOL_SEEN_XXX.  */

#define MAX_EOL_CHECK_COUNT 3

static int
detect_eol (coding, source, src_bytes)
     struct coding_system *coding;
     unsigned char *source;
     EMACS_INT src_bytes;
{
  Lisp_Object attrs, coding_type;
  unsigned char *src = source, *src_end = src + src_bytes;
  unsigned char c;
  int total  = 0;
  int eol_seen = EOL_SEEN_NONE;

  attrs = CODING_ID_ATTRS (coding->id);
  coding_type = CODING_ATTR_TYPE (attrs);

  if (EQ (coding_type, Qccl))
    {
      int msb, lsb;

      msb = coding->spec.utf_16.endian == utf_16_little_endian;
      lsb = 1 - msb;

      while (src + 1 < src_end)
	{
	  c = src[lsb];
	  if (src[msb] == 0 && (c == '\n' || c == '\r'))
	    {
	      int this_eol;

	      if (c == '\n')
		this_eol = EOL_SEEN_LF;
	      else if (src + 3 >= src_end
		       || src[msb + 2] != 0
		       || src[lsb + 2] != '\n')
		this_eol = EOL_SEEN_CR;
	      else
		this_eol = EOL_SEEN_CRLF; 

	      if (eol_seen == EOL_SEEN_NONE)
		/* This is the first end-of-line.  */
		eol_seen = this_eol;
	      else if (eol_seen != this_eol)
		{
		  /* The found type is different from what found before.  */
		  eol_seen = EOL_SEEN_LF;
		  break;
		}
	      if (++total == MAX_EOL_CHECK_COUNT)
		break;
	    }
	  src += 2;
	}
    }   
  else
    {
      while (src < src_end)
	{
	  c = *src++;
	  if (c == '\n' || c == '\r')
	    {
	      int this_eol;

	      if (c == '\n')
		this_eol = EOL_SEEN_LF;
	      else if (src >= src_end || *src != '\n')
		this_eol = EOL_SEEN_CR;
	      else
		this_eol = EOL_SEEN_CRLF, src++;

	      if (eol_seen == EOL_SEEN_NONE)
		/* This is the first end-of-line.  */
		eol_seen = this_eol;
	      else if (eol_seen != this_eol)
		{
		  /* The found type is different from what found before.  */
		  eol_seen = EOL_SEEN_LF;
		  break;
		}
	      if (++total == MAX_EOL_CHECK_COUNT)
		break;
	    }
	}
    }
  return eol_seen;
}


static void
adjust_coding_eol_type (coding, eol_seen)
     struct coding_system *coding;
     int eol_seen;
{
  Lisp_Object eol_type;
  
  eol_type = CODING_ID_EOL_TYPE (coding->id);
  if (eol_seen & EOL_SEEN_LF)
    coding->id = CODING_SYSTEM_ID (AREF (eol_type, 0));
  else if (eol_type & EOL_SEEN_CRLF)
    coding->id = CODING_SYSTEM_ID (AREF (eol_type, 1));
  else if (eol_type & EOL_SEEN_CR)
    coding->id = CODING_SYSTEM_ID (AREF (eol_type, 2));
}

/* Detect how a text specified in CODING is encoded.  If a coding
   system is detected, update fields of CODING by the detected coding
   system.  */

void
detect_coding (coding)
     struct coding_system *coding;
{
  unsigned char *src, *src_end;
  Lisp_Object attrs, coding_type;

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  coding_set_source (coding);

  src_end = coding->source + coding->src_bytes;

  /* If we have not yet decided the text encoding type, detect it
     now.  */
  if (EQ (CODING_ATTR_TYPE (CODING_ID_ATTRS (coding->id)), Qundecided))
    {
      int mask = CATEGORY_MASK_ANY;
      int c, i;

      for (src = coding->source; src < src_end; src++)
	{
	  c = *src;
	  if (c & 0x80 || (c < 0x20 && (c == ISO_CODE_ESC
					|| c == ISO_CODE_SI
					|| c == ISO_CODE_SO)))
	    break;
	}
      coding->head_ascii = src - (coding->source + coding->consumed);

      if (coding->head_ascii < coding->src_bytes)
	{
	  int detected = 0;

	  for (i = 0; i < coding_category_raw_text; i++)
	    {
	      enum coding_category category = coding_priorities[i];
	      struct coding_system *this = coding_categories + category;

	      if (category >= coding_category_raw_text
		  || detected & (1 << category))
		continue;

	      if (this->id < 0)
		{
		  /* No coding system of this category is defined.  */
		  mask &= ~(1 << category);
		}
	      else
		{
		  detected |= detected_mask[category];
		  if ((*(this->detector)) (coding, &mask))
		    break;
		}
	    }
	  if (! mask)
	    setup_coding_system (Qraw_text, coding);
	  else if (mask != CATEGORY_MASK_ANY)
	    for (i = 0; i < coding_category_raw_text; i++)
	      {
		enum coding_category category = coding_priorities[i];
		struct coding_system *this = coding_categories + category;

		if (mask & (1 << category))
		  {
		    setup_coding_system (CODING_ID_NAME (this->id), coding);
		    break;
		  }
	      }
	}
    }

  attrs = CODING_ID_ATTRS (coding->id);
  coding_type = CODING_ATTR_TYPE (attrs);

  /* If we have not yet decided the EOL type, detect it now.  But, the
     detection is impossible for a CCL based coding system, in which
     case, we detct the EOL type after decoding.  */
  if (VECTORP (CODING_ID_EOL_TYPE (coding->id))
      && ! EQ (coding_type, Qccl))
    {
      int eol_seen = detect_eol (coding, coding->source, coding->src_bytes);

      if (eol_seen != EOL_SEEN_NONE)
	adjust_coding_eol_type (coding, eol_seen);
    }
}


static void
decode_eol (coding)
     struct coding_system *coding;
{
  if (VECTORP (CODING_ID_EOL_TYPE (coding->id)))
    {
      unsigned char *p = CHAR_POS_ADDR (coding->dst_pos);
      unsigned char *pend = p + coding->produced;
      int eol_seen = EOL_SEEN_NONE;

      for (; p < pend; p++)
	{
	  if (*p == '\n')
	    eol_seen |= EOL_SEEN_LF;
	  else if (*p == '\r')
	    {
	      if (p + 1 < pend && *(p + 1) == '\n')
		{
		  eol_seen |= EOL_SEEN_CRLF;
		  p++;
		}
	      else
		eol_seen |= EOL_SEEN_CR;
	    }
	}
      if (eol_seen != EOL_SEEN_NONE)
	adjust_coding_eol_type (coding, eol_seen);
    }

  if (EQ (CODING_ID_EOL_TYPE (coding->id), Qmac))
    {
      unsigned char *p = CHAR_POS_ADDR (coding->dst_pos);
      unsigned char *pend = p + coding->produced;
      
      for (; p < pend; p++)
	if (*p == '\r')
	  *p = '\n';
    }
  else if (EQ (CODING_ID_EOL_TYPE (coding->id), Qdos))
    {
      unsigned char *p, *pbeg, *pend;
      Lisp_Object undo_list;

      move_gap_both (coding->dst_pos + coding->produced_char,
		     coding->dst_pos_byte + coding->produced);
      undo_list = current_buffer->undo_list;
      current_buffer->undo_list = Qt;
      del_range_2 (coding->dst_pos, coding->dst_pos_byte, GPT, GPT_BYTE, Qnil);
      current_buffer->undo_list = undo_list;
      pbeg = GPT_ADDR;
      pend = pbeg + coding->produced;

      for (p = pend - 1; p >= pbeg; p--)
	if (*p == '\r')
	  {
	    safe_bcopy ((char *) (p + 1), (char *) p, pend - p - 1);
	    pend--;
	  }
      coding->produced_char -= coding->produced - (pend - pbeg);
      coding->produced = pend - pbeg;
      insert_from_gap (coding->produced_char, coding->produced);
    }
}

static void
translate_chars (coding, table)
     struct coding_system *coding;
     Lisp_Object table;
{
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  int c;

  if (coding->chars_at_source)
    return;

  while (charbuf < charbuf_end)
    {
      c = *charbuf;
      if (c < 0)
	charbuf += c;
      else
	*charbuf++ = translate_char (table, c);
    }
}

static int
produce_chars (coding)
     struct coding_system *coding;
{
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int produced;
  int produced_chars = 0;

  if (! coding->chars_at_source)
    {
      /* Characters are in coding->charbuf.  */
      int *buf = coding->charbuf;
      int *buf_end = buf + coding->charbuf_used;
      unsigned char *adjusted_dst_end;

      if (BUFFERP (coding->src_object)
	  && EQ (coding->src_object, coding->dst_object))
	dst_end = coding->source + coding->consumed;
      adjusted_dst_end = dst_end - MAX_MULTIBYTE_LENGTH;

      while (buf < buf_end)
	{
	  int c = *buf++;
	  
	  if (dst >= adjusted_dst_end)
	    {
	      dst = alloc_destination (coding,
				       buf_end - buf + MAX_MULTIBYTE_LENGTH,
				       dst);
	      dst_end = coding->destination + coding->dst_bytes;
	      adjusted_dst_end = dst_end - MAX_MULTIBYTE_LENGTH;
	    }
	  if (c >= 0)
	    {
	      if (coding->dst_multibyte
		  || ! CHAR_BYTE8_P (c))
		CHAR_STRING_ADVANCE (c, dst);
	      else
		*dst++ = CHAR_TO_BYTE8 (c);
	      produced_chars++;
	    }
	  else
	    /* This is an annotation data.  */
	    buf -= c + 1;
	}
    }
  else
    {
      unsigned char *src = coding->source;
      unsigned char *src_end = src + coding->src_bytes;
      Lisp_Object eol_type;

      eol_type = CODING_ID_EOL_TYPE (coding->id);

      if (coding->src_multibyte != coding->dst_multibyte)
	{
	  if (coding->src_multibyte)
	    {
	      int multibytep = 1;
	      int consumed_chars;

	      while (1)
		{
		  unsigned char *src_base = src;
		  int c;

		  ONE_MORE_BYTE (c);
		  if (c == '\r')
		    {
		      if (EQ (eol_type, Qdos))
			{
			  if (src < src_end
			      && *src == '\n')
			    c = *src++;
			}
		      else if (EQ (eol_type, Qmac))
			c = '\n';
		    }
		  if (dst == dst_end)
		    {
		      coding->consumed = src - coding->source;

		    if (EQ (coding->src_object, coding->dst_object))
		      dst_end = src;
		    if (dst == dst_end)
		      {
			dst = alloc_destination (coding, src_end - src + 1,
						 dst);
			dst_end = coding->destination + coding->dst_bytes;
			coding_set_source (coding);
			src = coding->source + coding->consumed;
			src_end = coding->source + coding->src_bytes;
		      }
		    }
		  *dst++ = c;
		  produced_chars++;
		}
	    no_more_source:
	      ;
	    }
	  else
	    while (src < src_end)
	      {
		int multibytep = 1;
		int c = *src++;

		if (c == '\r')
		  {
		    if (EQ (eol_type, Qdos))
		      {
			if (src < src_end
			    && *src == '\n')
			  c = *src++;
		      }
		    else if (EQ (eol_type, Qmac))
		      c = '\n';
		  }
		if (dst >= dst_end - 1)
		  {
		    coding->consumed = src - coding->source;

		    if (EQ (coding->src_object, coding->dst_object))
		      dst_end = src;
		    if (dst >= dst_end - 1)
		      {
			dst = alloc_destination (coding, src_end - src + 2,
						 dst);
			dst_end = coding->destination + coding->dst_bytes;
			coding_set_source (coding);
			src = coding->source + coding->consumed;
			src_end = coding->source + coding->src_bytes;
		      }
		  }
		EMIT_ONE_BYTE (c);
	      }
	}
      else
	{
	  if (!EQ (coding->src_object, coding->dst_object))
	    {
	      int require = coding->src_bytes - coding->dst_bytes;

	      if (require > 0)
		{
		  EMACS_INT offset = src - coding->source;

		  dst = alloc_destination (coding, require, dst);
		  coding_set_source (coding);
		  src = coding->source + offset;
		  src_end = coding->source + coding->src_bytes;
		}
	    }
	  produced_chars = coding->src_chars;
	  while (src < src_end)
	    {
	      int c = *src++;

	      if (c == '\r')
		{
		  if (EQ (eol_type, Qdos))
		    {
		      if (src < src_end
			  && *src == '\n')
			c = *src++;
		      produced_chars--;
		    }
		  else if (EQ (eol_type, Qmac))
		    c = '\n';
		}
	      *dst++ = c;
	    }
	}
      coding->consumed = coding->src_bytes;
      coding->consumed_char = coding->src_chars;
    }

  produced = dst - (coding->destination + coding->produced);
  if (BUFFERP (coding->dst_object))
    insert_from_gap (produced_chars, produced);
  coding->produced += produced;
  coding->produced_char += produced_chars;
  return produced_chars;
}

/* [ -LENGTH CHAR_POS_OFFSET MASK METHOD COMP_LEN ]
	or
   [ -LENGTH CHAR_POS_OFFSET MASK METHOD COMP_LEN COMPONENTS... ]
 */

static INLINE void
produce_composition (coding, charbuf)
     struct coding_system *coding;
     int *charbuf;
{
  Lisp_Object buffer;
  int len;
  EMACS_INT pos;
  enum composition_method method;
  int cmp_len;
  Lisp_Object components;

  buffer = coding->dst_object;
  len = -charbuf[0];
  pos = coding->dst_pos + charbuf[1];
  method = (enum composition_method) (charbuf[3]);
  cmp_len = charbuf[4];

  if (method == COMPOSITION_RELATIVE)
    components = Qnil;
  else
    {
      Lisp_Object args[MAX_COMPOSITION_COMPONENTS * 2 - 1];
      int i;

      len -= 5;
      charbuf += 5;
      for (i = 0; i < len; i++)
	args[i] = make_number (charbuf[i]);
      components = (method == COMPOSITION_WITH_ALTCHARS
		    ? Fstring (len, args) : Fvector (len, args));
    }
  compose_text (pos, pos + cmp_len, components, Qnil, Qnil);
}

static int *
save_composition_data (buf, buf_end, prop)
     int *buf, *buf_end;
     Lisp_Object prop;
{
  enum composition_method method = COMPOSITION_METHOD (prop);
  int cmp_len = COMPOSITION_LENGTH (prop);

  if (buf + 4 + (MAX_COMPOSITION_COMPONENTS * 2 - 1) > buf_end)
    return NULL;

  buf[1] = CODING_ANNOTATE_COMPOSITION_MASK;
  buf[2] = method;
  buf[3] = cmp_len;

  if (method == COMPOSITION_RELATIVE)
    buf[0] = 4;
  else
    {
      Lisp_Object components;
      int len, i;

      components = COMPOSITION_COMPONENTS (prop);
      if (VECTORP (components))
	{
	  len = XVECTOR (components)->size;
	  for (i = 0; i < len; i++)
	    buf[4 + i] = XINT (AREF (components, i));
	}
      else if (STRINGP (components))
	{
	  int i_byte;

	  len = XSTRING (components)->size;
	  i = i_byte = 0;
	  while (i < len)
	    FETCH_STRING_CHAR_ADVANCE (buf[4 + i], components, i, i_byte);
	}
      else if (INTEGERP (components))
	{
	  len = 1;
	  buf[4] = XINT (components);
	}
      else if (CONSP (components))
	{
	  for (len = 0; CONSP (components);
	       len++, components = XCDR (components))
	    buf[4 + len] = XINT (XCAR (components));
	}
      else
	abort ();
      buf[0] = 4 + len;
    }
  return (buf + buf[0]);
}

#define CHARBUF_SIZE 0x4000

#define ALLOC_CONVERSION_WORK_AREA(coding)				\
  do {									\
    int size = CHARBUF_SIZE;;						\
    									\
    coding->charbuf = NULL;						\
    while (size > 1024)							\
      {									\
	coding->charbuf = (int *) alloca (sizeof (int) * size);		\
	if (coding->charbuf)						\
	  break;							\
	size >>= 1;							\
      }									\
    if (! coding->charbuf)						\
      {									\
	coding->result = CODING_RESULT_INSUFFICIENT_MEM;		\
	return coding->result;						\
      }									\
    coding->charbuf_size = size;					\
  } while (0)


static void
produce_annotation (coding)
     struct coding_system *coding;
{
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;

  while (charbuf < charbuf_end)
    {
      if (*charbuf >= 0)
	charbuf++;
      else
	{
	  int len = -*charbuf;
	  switch (charbuf[2])
	    {
	    case CODING_ANNOTATE_COMPOSITION_MASK:
	      produce_composition (coding, charbuf);
	      break;
	    default:
	      abort ();
	    }
	  charbuf += len;
	}
    }
}

/* Decode the data at CODING->src_object into CODING->dst_object.
   CODING->src_object is a buffer, a string, or nil.
   CODING->dst_object is a buffer.

   If CODING->src_object is a buffer, it must be the current buffer.
   In this case, if CODING->src_pos is positive, it is a position of
   the source text in the buffer, otherwise, the source text is in the
   gap area of the buffer, and CODING->src_pos specifies the offset of
   the text from GPT (which must be the same as PT).  If this is the
   same buffer as CODING->dst_object, CODING->src_pos must be
   negative.

   If CODING->src_object is a string, CODING->src_pos in an index to
   that string.

   If CODING->src_object is nil, CODING->source must already point to
   the non-relocatable memory area.  In this case, CODING->src_pos is
   an offset from CODING->source.

   The decoded data is inserted at the current point of the buffer
   CODING->dst_object.
*/

static int
decode_coding (coding)
     struct coding_system *coding;
{
  Lisp_Object attrs;

  if (BUFFERP (coding->src_object)
      && coding->src_pos > 0
      && coding->src_pos < GPT
      && coding->src_pos + coding->src_chars > GPT)
    move_gap_both (coding->src_pos, coding->src_pos_byte);

  if (BUFFERP (coding->dst_object))
    {
      if (current_buffer != XBUFFER (coding->dst_object))
	set_buffer_internal (XBUFFER (coding->dst_object));
      if (GPT != PT)
	move_gap_both (PT, PT_BYTE);
    }

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  coding->chars_at_source = 0;
  coding->result = CODING_RESULT_SUCCESS;
  coding->errors = 0;

  ALLOC_CONVERSION_WORK_AREA (coding);

  attrs = CODING_ID_ATTRS (coding->id);

  do
    {
      coding_set_source (coding);
      coding->annotated = 0;
      (*(coding->decoder)) (coding);
      if (!NILP (CODING_ATTR_DECODE_TBL (attrs)))
	translate_chars (CODING_ATTR_DECODE_TBL (attrs), coding);
      coding_set_destination (coding);
      produce_chars (coding);
      if (coding->annotated)
	produce_annotation (coding);
    }
  while (coding->consumed < coding->src_bytes
	 && ! coding->result);

  if (EQ (CODING_ATTR_TYPE (CODING_ID_ATTRS (coding->id)), Qccl)
      && SYMBOLP (CODING_ID_EOL_TYPE (coding->id))
      && ! EQ (CODING_ID_EOL_TYPE (coding->id), Qunix))
    decode_eol (coding);

  coding->carryover_bytes = 0;
  if (coding->consumed < coding->src_bytes)
    {
      int nbytes = coding->src_bytes - coding->consumed;
      unsigned char *src;

      coding_set_source (coding);
      coding_set_destination (coding);
      src = coding->source + coding->consumed;

      if (coding->mode & CODING_MODE_LAST_BLOCK)
	{
	  /* Flush out unprocessed data as binary chars.  We are sure
	     that the number of data is less than the size of
	     coding->charbuf.  */
	  int *charbuf = coding->charbuf;

	  while (nbytes-- > 0)
	    {
	      int c = *src++;
	      *charbuf++ =  (c & 0x80 ? - c : c);
	    }
	  produce_chars (coding);
	}
      else
	{
	  /* Record unprocessed bytes in coding->carryover.  We are
	     sure that the number of data is less than the size of
	     coding->carryover.  */
	  unsigned char *p = coding->carryover;

	  coding->carryover_bytes = nbytes;
	  while (nbytes-- > 0)
	    *p++ = *src++;
	}
      coding->consumed = coding->src_bytes;
    }

  return coding->result;
}

static void
consume_chars (coding)
     struct coding_system *coding;
{
  int *buf = coding->charbuf;
  /* -1 is to compensate for CRLF.  */
  int *buf_end = coding->charbuf + coding->charbuf_size - 1;
  unsigned char *src = coding->source + coding->consumed;
  int pos = coding->src_pos + coding->consumed_char;
  int end_pos = coding->src_pos + coding->src_chars;
  int multibytep = coding->src_multibyte;
  Lisp_Object eol_type;
  int c;
  int start, end, stop;
  Lisp_Object object, prop;

  eol_type = CODING_ID_EOL_TYPE (coding->id);
  if (VECTORP (eol_type))
    eol_type = Qunix;

  object = coding->src_object;

  /* Note: composition handling is not yet implemented.  */
  coding->common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;

  if (coding->common_flags & CODING_ANNOTATE_COMPOSITION_MASK
      && find_composition (pos, end_pos, &start, &end, &prop, object)
      && end <= end_pos
      && (start >= pos
	  || (find_composition (end, end_pos, &start, &end, &prop, object)
	      && end <= end_pos)))
    stop = start;
  else
    stop = end_pos;

  while (buf < buf_end)
    {
      if (pos == stop)
	{
	  int *p;

	  if (pos == end_pos)
	    break;
	  p = save_composition_data (buf, buf_end, prop);
	  if (p == NULL)
	    break;
	  buf = p;
	  if (find_composition (end, end_pos, &start, &end, &prop, object)
	      && end <= end_pos)
	    stop = start;
	  else
	    stop = end_pos;
	}

      if (! multibytep)
	c = *src++;
      else
	c = STRING_CHAR_ADVANCE (src);
      if ((c == '\r') && (coding->mode & CODING_MODE_SELECTIVE_DISPLAY))
	c = '\n';
      if (! EQ (eol_type, Qunix))
	{
	  if (c == '\n')
	    {
	      if (EQ (eol_type, Qdos))
		*buf++ = '\r';
	      else
		c = '\r';
	    }
	}
      *buf++ = c;
      pos++;
    }

  coding->consumed = src - coding->source;
  coding->consumed_char = pos - coding->src_pos;
  coding->charbuf_used = buf - coding->charbuf;
  coding->chars_at_source = 0;
}


/* Encode the text at CODING->src_object into CODING->dst_object.
   CODING->src_object is a buffer or a string.
   CODING->dst_object is a buffer or nil.

   If CODING->src_object is a buffer, it must be the current buffer.
   In this case, if CODING->src_pos is positive, it is a position of
   the source text in the buffer, otherwise. the source text is in the
   gap area of the buffer, and coding->src_pos specifies the offset of
   the text from GPT (which must be the same as PT).  If this is the
   same buffer as CODING->dst_object, CODING->src_pos must be
   negative and CODING should not have `pre-write-conversion'.

   If CODING->src_object is a string, CODING should not have
   `pre-write-conversion'.

   If CODING->dst_object is a buffer, the encoded data is inserted at
   the current point of that buffer.

   If CODING->dst_object is nil, the encoded data is placed at the
   memory area specified by CODING->destination.  */

static int
encode_coding (coding)
     struct coding_system *coding;
{
  Lisp_Object attrs;

  attrs = CODING_ID_ATTRS (coding->id);

  if (BUFFERP (coding->dst_object))
    {
      set_buffer_internal (XBUFFER (coding->dst_object));
      coding->dst_multibyte
	= ! NILP (current_buffer->enable_multibyte_characters);
    }

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  coding->result = CODING_RESULT_SUCCESS;
  coding->errors = 0;

  ALLOC_CONVERSION_WORK_AREA (coding);

  do {
    coding_set_source (coding);
    consume_chars (coding);

    if (!NILP (CODING_ATTR_ENCODE_TBL (attrs)))
      translate_chars (CODING_ATTR_ENCODE_TBL (attrs), coding);

    coding_set_destination (coding);
    (*(coding->encoder)) (coding);
  } while (coding->consumed_char < coding->src_chars);

  if (BUFFERP (coding->dst_object))
    insert_from_gap (coding->produced_char, coding->produced);

  return (coding->result);
}

/* Work buffer */

/* List of currently used working buffer.  */
Lisp_Object Vcode_conversion_work_buf_list;

/* A working buffer used by the top level conversion.  */
Lisp_Object Vcode_conversion_reused_work_buf;


/* Return a working buffer that can be freely used by the following
   code conversion.  MULTIBYTEP specifies the multibyteness of the
   buffer.  */

Lisp_Object
make_conversion_work_buffer (multibytep)
     int multibytep;
{
  struct buffer *current = current_buffer;
  Lisp_Object buf;

  if (NILP (Vcode_conversion_work_buf_list))
    {
      if (NILP (Vcode_conversion_reused_work_buf))
	Vcode_conversion_reused_work_buf
	  = Fget_buffer_create (build_string (" *code-conversion-work*"));
      Vcode_conversion_work_buf_list
	= Fcons (Vcode_conversion_reused_work_buf, Qnil);
    }
  else
    {
      int depth = Flength (Vcode_conversion_work_buf_list);
      char str[128];

      sprintf (str, " *code-conversion-work*<%d>", depth);
      Vcode_conversion_work_buf_list
	= Fcons (Fget_buffer_create (build_string (str)),
		 Vcode_conversion_work_buf_list);
    }

  buf = XCAR (Vcode_conversion_work_buf_list);
  set_buffer_internal (XBUFFER (buf));
  current_buffer->undo_list = Qt;
  Ferase_buffer ();
  Fset_buffer_multibyte (multibytep ? Qt : Qnil);
  set_buffer_internal (current);
  return buf;
}

static struct coding_system *saved_coding;

Lisp_Object
code_conversion_restore (info)
     Lisp_Object info;
{
  int depth = Flength (Vcode_conversion_work_buf_list);
  Lisp_Object buf;

  if (depth > 0)
    {
      buf = XCAR (Vcode_conversion_work_buf_list);
      Vcode_conversion_work_buf_list = XCDR (Vcode_conversion_work_buf_list);
      if (depth > 1 && !NILP (Fbuffer_live_p (buf)))
	Fkill_buffer (buf);
    }

  if (saved_coding->dst_object == Qt
      && saved_coding->destination)
    xfree (saved_coding->destination);

  return save_excursion_restore (info);
}


int
decode_coding_gap (coding, chars, bytes)
     struct coding_system *coding;
     EMACS_INT chars, bytes;
{
  int count = specpdl_ptr - specpdl;

  saved_coding = coding;
  record_unwind_protect (code_conversion_restore, save_excursion_save ());

  coding->src_object = Fcurrent_buffer ();
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_pos = -chars;
  coding->src_pos_byte = -bytes;
  coding->src_multibyte = chars < bytes;
  coding->dst_object = coding->src_object;
  coding->dst_pos = PT;
  coding->dst_pos_byte = PT_BYTE;
  coding->dst_multibyte = ! NILP (current_buffer->enable_multibyte_characters);

  if (CODING_REQUIRE_DETECTION (coding))
    detect_coding (coding);
    
  decode_coding (coding);

  unbind_to (count, Qnil);
  return coding->result;
}

int
encode_coding_gap (coding, chars, bytes)
     struct coding_system *coding;
     EMACS_INT chars, bytes;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object buffer;

  saved_coding = coding;
  record_unwind_protect (code_conversion_restore, save_excursion_save ());

  buffer = Fcurrent_buffer ();
  coding->src_object = buffer;
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_pos = -chars;
  coding->src_pos_byte = -bytes;
  coding->src_multibyte = chars < bytes;
  coding->dst_object = coding->src_object;
  coding->dst_pos = PT;
  coding->dst_pos_byte = PT_BYTE;

  encode_coding (coding);

  unbind_to (count, Qnil);
  return coding->result;
}


/* Decode the text in the range FROM/FROM_BYTE and TO/TO_BYTE in
   SRC_OBJECT into DST_OBJECT by coding context CODING.

   SRC_OBJECT is a buffer, a string, or Qnil.

   If it is a buffer, the text is at point of the buffer.  FROM and TO
   are positions in the buffer.

   If it is a string, the text is at the beginning of the string.
   FROM and TO are indices to the string.

   If it is nil, the text is at coding->source.  FROM and TO are
   indices to coding->source.

   DST_OBJECT is a buffer, Qt, or Qnil.

   If it is a buffer, the decoded text is inserted at point of the
   buffer.  If the buffer is the same as SRC_OBJECT, the source text
   is deleted.

   If it is Qt, a string is made from the decoded text, and
   set in CODING->dst_object.

   If it is Qnil, the decoded text is stored at CODING->destination.
   The called must allocate CODING->dst_bytes bytes at
   CODING->destination by xmalloc.  If the decoded text is longer than
   CODING->dst_bytes, CODING->destination is relocated by xrealloc.
 */

void
decode_coding_object (coding, src_object, from, from_byte, to, to_byte,
		      dst_object)
     struct coding_system *coding;
     Lisp_Object src_object;
     EMACS_INT from, from_byte, to, to_byte;
     Lisp_Object dst_object;
{
  int count = specpdl_ptr - specpdl;
  unsigned char *destination;
  EMACS_INT dst_bytes;
  EMACS_INT chars = to - from;
  EMACS_INT bytes = to_byte - from_byte;
  Lisp_Object attrs;

  saved_coding = coding;
  record_unwind_protect (code_conversion_restore, save_excursion_save ());

  if (NILP (dst_object))
    {
      destination = coding->destination;
      dst_bytes = coding->dst_bytes;
    }

  coding->src_object = src_object;
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_multibyte = chars < bytes;

  if (STRINGP (src_object))
    {
      coding->src_pos = from;
      coding->src_pos_byte = from_byte;
    }
  else if (BUFFERP (src_object))
    {
      set_buffer_internal (XBUFFER (src_object));
      if (from != GPT)
	move_gap_both (from, from_byte);
      if (EQ (src_object, dst_object))
	{
	  TEMP_SET_PT_BOTH (from, from_byte);
	  del_range_both (from, from_byte, to, to_byte, 1);
	  coding->src_pos = -chars;
	  coding->src_pos_byte = -bytes;
	}
      else
	{
	  coding->src_pos = from;
	  coding->src_pos_byte = from_byte;
	}
    }

  if (CODING_REQUIRE_DETECTION (coding))
    detect_coding (coding);
  attrs = CODING_ID_ATTRS (coding->id);

  if (! NILP (CODING_ATTR_POST_READ (attrs))
      || EQ (dst_object, Qt))
    {
      coding->dst_object = make_conversion_work_buffer (1);
      coding->dst_pos = BEG;
      coding->dst_pos_byte = BEG_BYTE;
      coding->dst_multibyte = 1;
    }
  else if (BUFFERP (dst_object))
    {
      coding->dst_object = dst_object;
      coding->dst_pos = BUF_PT (XBUFFER (dst_object));
      coding->dst_pos_byte = BUF_PT_BYTE (XBUFFER (dst_object));
      coding->dst_multibyte
	= ! NILP (XBUFFER (dst_object)->enable_multibyte_characters);
    }
  else
    {
      coding->dst_object = Qnil;
      coding->dst_multibyte = 1;
    }

  decode_coding (coding);

  if (BUFFERP (coding->dst_object))
    set_buffer_internal (XBUFFER (coding->dst_object));

  if (! NILP (CODING_ATTR_POST_READ (attrs)))
    {
      struct gcpro gcpro1, gcpro2;
      EMACS_INT prev_Z = Z, prev_Z_BYTE = Z_BYTE;
      Lisp_Object val;

      GCPRO2 (coding->src_object, coding->dst_object);
      val = call1 (CODING_ATTR_POST_READ (attrs),
		   make_number (coding->produced_char));
      UNGCPRO;
      CHECK_NATNUM (val);
      coding->produced_char += Z - prev_Z;
      coding->produced += Z_BYTE - prev_Z_BYTE;
    }

  if (EQ (dst_object, Qt))
    {
      coding->dst_object = Fbuffer_string ();
    }
  else if (NILP (dst_object) && BUFFERP (coding->dst_object))
    {
      set_buffer_internal (XBUFFER (coding->dst_object));
      if (dst_bytes < coding->produced)
	{
	  destination
	    = (unsigned char *) xrealloc (destination, coding->produced);
	  if (! destination)
	    {
	      coding->result = CODING_RESULT_INSUFFICIENT_DST;
	      unbind_to (count, Qnil);
	      return;
	    }
	  if (BEGV < GPT && GPT < BEGV + coding->produced_char)
	    move_gap_both (BEGV, BEGV_BYTE);
	  bcopy (BEGV_ADDR, destination, coding->produced);
	  coding->destination = destination;
	}
    }

  unbind_to (count, Qnil);
}


void
encode_coding_object (coding, src_object, from, from_byte, to, to_byte,
		      dst_object)
     struct coding_system *coding;
     Lisp_Object src_object;
     EMACS_INT from, from_byte, to, to_byte;
     Lisp_Object dst_object;
{
  int count = specpdl_ptr - specpdl;
  EMACS_INT chars = to - from;
  EMACS_INT bytes = to_byte - from_byte;
  Lisp_Object attrs;

  saved_coding = coding;
  record_unwind_protect (code_conversion_restore, save_excursion_save ());

  coding->src_object = src_object;
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_multibyte = chars < bytes;

  attrs = CODING_ID_ATTRS (coding->id);

  if (! NILP (CODING_ATTR_PRE_WRITE (attrs)))
    {
      Lisp_Object val;

      coding->src_object = make_conversion_work_buffer (coding->src_multibyte);
      set_buffer_internal (XBUFFER (coding->src_object));
      if (STRINGP (src_object))
	insert_from_string (src_object, from, from_byte, chars, bytes, 0);
      else if (BUFFERP (src_object))
	insert_from_buffer (XBUFFER (src_object), from, chars, 0);
      else
	insert_1_both (coding->source + from, chars, bytes, 0, 0, 0);

      if (EQ (src_object, dst_object))
	{
	  set_buffer_internal (XBUFFER (src_object));
	  del_range_both (from, from_byte, to, to_byte, 1);
	  set_buffer_internal (XBUFFER (coding->src_object));
	}

      val = call2 (CODING_ATTR_PRE_WRITE (attrs),
		   make_number (1), make_number (chars));
      CHECK_NATNUM (val);
      if (BEG != GPT)
	move_gap_both (BEG, BEG_BYTE);
      coding->src_chars = Z - BEG;
      coding->src_bytes = Z_BYTE - BEG_BYTE;
      coding->src_pos = BEG;
      coding->src_pos_byte = BEG_BYTE;
      coding->src_multibyte = Z < Z_BYTE;
    }
  else if (STRINGP (src_object))
    {
      coding->src_pos = from;
      coding->src_pos_byte = from_byte;
    }
  else if (BUFFERP (src_object))
    {
      set_buffer_internal (XBUFFER (src_object));
      if (from != GPT)
	move_gap_both (from, from_byte);
      if (EQ (src_object, dst_object))
	{
	  del_range_both (from, from_byte, to, to_byte, 1);
	  coding->src_pos = -chars;
	  coding->src_pos_byte = -bytes;
	}
      else
	{
	  coding->src_pos = from;
	  coding->src_pos_byte = from_byte;
	}
    }

  if (BUFFERP (dst_object))
    {
      coding->dst_object = dst_object;
      coding->dst_pos = BUF_PT (XBUFFER (dst_object));
      coding->dst_pos_byte = BUF_PT_BYTE (XBUFFER (dst_object));
      coding->dst_multibyte
	= ! NILP (XBUFFER (dst_object)->enable_multibyte_characters);
    }
  else if (EQ (dst_object, Qt))
    {
      coding->dst_object = Qnil;
      coding->destination = (unsigned char *) xmalloc (coding->src_chars);
      coding->dst_bytes = coding->src_chars;
      coding->dst_multibyte = 0;
    }
  else
    {
      coding->dst_object = Qnil;
      coding->dst_multibyte = 0;
    }

  encode_coding (coding);

  if (EQ (dst_object, Qt))
    {
      if (BUFFERP (coding->dst_object))
	coding->dst_object = Fbuffer_string ();
      else
	{
	  coding->dst_object
	    = make_unibyte_string ((char *) coding->destination,
				   coding->produced);
	  xfree (coding->destination);
	}
    }

  unbind_to (count, Qnil);
}


Lisp_Object
preferred_coding_system ()
{
  int id = coding_categories[coding_priorities[0]].id;

  return CODING_ID_NAME (id);
}


#ifdef emacs
/*** 8. Emacs Lisp library functions ***/

DEFUN ("coding-system-p", Fcoding_system_p, Scoding_system_p, 1, 1, 0,
       doc: /* Return t if OBJECT is nil or a coding-system.
See the documentation of `define-coding-system' for information
about coding-system objects.  */)
     (obj)
     Lisp_Object obj;
{
  return ((NILP (obj) || CODING_SYSTEM_P (obj)) ? Qt : Qnil);
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
detect_coding_system (src, src_bytes, highest, multibytep, coding_system)
     unsigned char *src;
     int src_bytes, highest;
     int multibytep;
     Lisp_Object coding_system;
{
  unsigned char *src_end = src + src_bytes;
  int mask = CATEGORY_MASK_ANY;
  int detected = 0;
  int c, i;
  Lisp_Object attrs, eol_type;
  Lisp_Object val;
  struct coding_system coding;

  if (NILP (coding_system))
    coding_system = Qundecided;
  setup_coding_system (coding_system, &coding);
  attrs = CODING_ID_ATTRS (coding.id);
  eol_type = CODING_ID_EOL_TYPE (coding.id);

  coding.source = src;
  coding.src_bytes = src_bytes;
  coding.src_multibyte = multibytep;
  coding.consumed = 0;

  if (XINT (CODING_ATTR_CATEGORY (attrs)) != coding_category_undecided)
    {
      mask = 1 << XINT (CODING_ATTR_CATEGORY (attrs));
    }
  else
    {
      coding_system = Qnil;
      for (; src < src_end; src++)
	{
	  c = *src;
	  if (c & 0x80 || (c < 0x20 && (c == ISO_CODE_ESC
					|| c == ISO_CODE_SI
					|| c == ISO_CODE_SO)))
	    break;
	}
      coding.head_ascii = src - coding.source;

      if (src < src_end)
	for (i = 0; i < coding_category_raw_text; i++)
	  {
	    enum coding_category category = coding_priorities[i];
	    struct coding_system *this = coding_categories + category;

	    if (category >= coding_category_raw_text
		|| detected & (1 << category))
	      continue;
	
	    if (this->id < 0)
	      {
		/* No coding system of this category is defined.  */
		mask &= ~(1 << category);
	      }
	    else
	      {
		detected |= detected_mask[category];
		if ((*(coding_categories[category].detector)) (&coding, &mask)
		    && highest)
		  {
		    mask &= detected_mask[category];
		    break;
		  }
	      }
	  }
    }

  if (!mask)
    val = Fcons (make_number (coding_category_raw_text), Qnil);
  else if (mask == CATEGORY_MASK_ANY)
    val = Fcons (make_number (coding_category_undecided), Qnil);
  else if (highest)
    {
      for (i = 0; i < coding_category_raw_text; i++)
	if (mask & (1 << coding_priorities[i]))
	  {
	    val = Fcons (make_number (coding_priorities[i]), Qnil);
	    break;
	  }
    }	
  else
    {
      val = Qnil;
      for (i = coding_category_raw_text - 1; i >= 0; i--)
	if (mask & (1 << coding_priorities[i]))
	  val = Fcons (make_number (coding_priorities[i]), val);
    }

  {
    int one_byte_eol = -1, two_byte_eol = -1;
    Lisp_Object tail;

    for (tail = val; CONSP (tail); tail = XCDR (tail))
      {
	struct coding_system *this
	  = (NILP (coding_system) ? coding_categories + XINT (XCAR (tail))
	     : &coding);
	int this_eol;
	
	attrs = CODING_ID_ATTRS (this->id);
	eol_type = CODING_ID_EOL_TYPE (this->id);
	XSETCAR (tail, CODING_ID_NAME (this->id));
	if (VECTORP (eol_type))
	  {
	    if (EQ (CODING_ATTR_TYPE (attrs), Qutf_16))
	      {
		if (two_byte_eol < 0)
		  two_byte_eol = detect_eol (this, coding.source, src_bytes);
		this_eol = two_byte_eol;
	      }
	    else
	      {
		if (one_byte_eol < 0)
		  one_byte_eol =detect_eol (this, coding.source, src_bytes);
		this_eol = one_byte_eol;
	      }
	    if (this_eol == EOL_SEEN_LF)
	      XSETCAR (tail, AREF (eol_type, 0));
	    else if (this_eol == EOL_SEEN_CRLF)
	      XSETCAR (tail, AREF (eol_type, 1));
	    else if (this_eol == EOL_SEEN_CR)
	      XSETCAR (tail, AREF (eol_type, 2));
	  }
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

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);

  validate_region (&start, &end);
  from = XINT (start), to = XINT (end);
  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  if (from < GPT && to >= GPT)
    move_gap_both (to, to_byte);

  return detect_coding_system (BYTE_POS_ADDR (from_byte),
			       to_byte - from_byte,
			       !NILP (highest),
			       !NILP (current_buffer
				      ->enable_multibyte_characters),
			       Qnil);
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
			       STRING_BYTES (XSTRING (string)),
			       !NILP (highest),
			       STRING_MULTIBYTE (string),
			       Qnil);
}


static INLINE int
char_encodable_p (c, attrs)
     int c;
     Lisp_Object attrs;
{
  Lisp_Object tail;
  struct charset *charset;

  for (tail = CODING_ATTR_CHARSET_LIST (attrs);
       CONSP (tail); tail = XCDR (tail))
    {
      charset = CHARSET_FROM_ID (XINT (XCAR (tail)));
      if (CHAR_CHARSET_P (c, charset))
	break;
    }
  return (! NILP (tail));
}


/* Return a list of coding systems that safely encode the text between
   START and END.  If EXCLUDE is non-nil, it is a list of coding
   systems not to check.  The returned list doesn't contain any such
   coding systems.  In any case, If the text contains only ASCII or is
   unibyte, return t.  */

DEFUN ("find-coding-systems-region-internal",
       Ffind_coding_systems_region_internal,
       Sfind_coding_systems_region_internal, 2, 3, 0,
       doc: /* Internal use only.  */)
     (start, end, exclude)
     Lisp_Object start, end, exclude;
{
  Lisp_Object coding_attrs_list, safe_codings;
  EMACS_INT start_byte, end_byte;
  unsigned char *p, *pbeg, *pend;
  int c;
  Lisp_Object tail, elt;

  if (STRINGP (start))
    {
      if (!STRING_MULTIBYTE (start)
	  && XSTRING (start)->size != STRING_BYTES (XSTRING (start)))
	return Qt;
      start_byte = 0;
      end_byte = STRING_BYTES (XSTRING (start));
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start);
      CHECK_NUMBER_COERCE_MARKER (end);
      if (XINT (start) < BEG || XINT (end) > Z || XINT (start) > XINT (end))
	args_out_of_range (start, end);
      if (NILP (current_buffer->enable_multibyte_characters))
	return Qt;
      start_byte = CHAR_TO_BYTE (XINT (start));
      end_byte = CHAR_TO_BYTE (XINT (end));
      if (XINT (end) - XINT (start) == end_byte - start_byte)
	return Qt;

      if (start < GPT && end > GPT)
	{
	  if ((GPT - start) < (end - GPT))
	    move_gap_both (start, start_byte);
	  else
	    move_gap_both (end, end_byte);
	}
    }

  coding_attrs_list = Qnil;
  for (tail = Vcoding_system_list; CONSP (tail); tail = XCDR (tail))
    if (NILP (exclude)
	|| NILP (Fmemq (XCAR (tail), exclude)))
      {
	Lisp_Object attrs;

	attrs = AREF (CODING_SYSTEM_SPEC (XCAR (tail)), 0);
	if (EQ (XCAR (tail), CODING_ATTR_BASE_NAME (attrs))
	    && ! EQ (CODING_ATTR_TYPE (attrs), Qundecided))
	  coding_attrs_list = Fcons (attrs, coding_attrs_list);
      }

  if (STRINGP (start))
    p = pbeg = XSTRING (start)->data;
  else
    p = pbeg = BYTE_POS_ADDR (start_byte);
  pend = p + (end_byte - start_byte);

  while (p < pend && ASCII_BYTE_P (*p)) p++;
  while (p < pend && ASCII_BYTE_P (*(pend - 1))) pend--;

  while (p < pend)
    {
      if (ASCII_BYTE_P (*p))
	p++;
      else
	{
	  c = STRING_CHAR_ADVANCE (p);

	  charset_map_loaded = 0;
	  for (tail = coding_attrs_list; CONSP (tail);)
	    {
	      elt = XCAR (tail);
	      if (NILP (elt))
		tail = XCDR (tail);
	      else if (char_encodable_p (c, elt))
		tail = XCDR (tail);
	      else if (CONSP (XCDR (tail)))
		{
		  XSETCAR (tail, XCAR (XCDR (tail)));
		  XSETCDR (tail, XCDR (XCDR (tail)));
		}
	      else
		{
		  XSETCAR (tail, Qnil);
		  tail = XCDR (tail);
		}
	    }
	  if (charset_map_loaded)
	    {
	      EMACS_INT p_offset = p - pbeg, pend_offset = pend - pbeg;

	      if (STRINGP (start))
		pbeg = XSTRING (start)->data;
	      else
		pbeg = BYTE_POS_ADDR (start_byte);
	      p = pbeg + p_offset;
	      pend = pbeg + pend_offset;
	    }
	}
    }

  safe_codings = Qnil;
  for (tail = coding_attrs_list; CONSP (tail); tail = XCDR (tail))
    if (! NILP (XCAR (tail)))
      safe_codings = Fcons (CODING_ATTR_BASE_NAME (XCAR (tail)), safe_codings);

  return safe_codings;
}


DEFUN ("check-coding-systems-region", Fcheck_coding_systems_region,
       Scheck_coding_systems_region, 3, 3, 0,
       doc: /* Check if the region is encodable by coding systems.

START and END are buffer positions specifying the region.
CODING-SYSTEM-LIST is a list of coding systems to check.

The value is an alist ((CODING-SYSTEM POS0 POS1 ...) ...), where
CODING-SYSTEM is a member of CODING-SYSTEM-LIst and can't encode the
whole region, POS0, POS1, ... are buffer positions where non-encodable
characters are found.

If all coding systems in CODING-SYSTEM-LIST can encode the region, the
value is nil.

START may be a string.  In that case, check if the string is
encodable, and the value contains indices to the string instead of
buffer positions.  END is ignored.  */)
     (start, end, coding_system_list)
     Lisp_Object start, end, coding_system_list;
{
  Lisp_Object list;
  EMACS_INT start_byte, end_byte;
  int pos;
  unsigned char *p, *pbeg, *pend;
  int c;
  Lisp_Object tail, elt;

  if (STRINGP (start))
    {
      if (!STRING_MULTIBYTE (start)
	  && XSTRING (start)->size != STRING_BYTES (XSTRING (start)))
	return Qnil;
      start_byte = 0;
      end_byte = STRING_BYTES (XSTRING (start));
      pos = 0;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start);
      CHECK_NUMBER_COERCE_MARKER (end);
      if (XINT (start) < BEG || XINT (end) > Z || XINT (start) > XINT (end))
	args_out_of_range (start, end);
      if (NILP (current_buffer->enable_multibyte_characters))
	return Qnil;
      start_byte = CHAR_TO_BYTE (XINT (start));
      end_byte = CHAR_TO_BYTE (XINT (end));
      if (XINT (end) - XINT (start) == end_byte - start_byte)
	return Qt;

      if (start < GPT && end > GPT)
	{
	  if ((GPT - start) < (end - GPT))
	    move_gap_both (start, start_byte);
	  else
	    move_gap_both (end, end_byte);
	}
      pos = start;
    }

  list = Qnil;
  for (tail = coding_system_list; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      list = Fcons (Fcons (elt, Fcons (AREF (CODING_SYSTEM_SPEC (elt), 0),
				       Qnil)),
		    list);
    }

  if (STRINGP (start))
    p = pbeg = XSTRING (start)->data;
  else
    p = pbeg = BYTE_POS_ADDR (start_byte);
  pend = p + (end_byte - start_byte);

  while (p < pend && ASCII_BYTE_P (*p)) p++, pos++;
  while (p < pend && ASCII_BYTE_P (*(pend - 1))) pend--;

  while (p < pend)
    {
      if (ASCII_BYTE_P (*p))
	p++;
      else
	{
	  c = STRING_CHAR_ADVANCE (p);

	  charset_map_loaded = 0;
	  for (tail = list; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCDR (XCAR (tail));
	      if (! char_encodable_p (c, XCAR (elt)))
		XSETCDR (elt, Fcons (make_number (pos), XCDR (elt)));
	    }
	  if (charset_map_loaded)
	    {
	      EMACS_INT p_offset = p - pbeg, pend_offset = pend - pbeg;

	      if (STRINGP (start))
		pbeg = XSTRING (start)->data;
	      else
		pbeg = BYTE_POS_ADDR (start_byte);
	      p = pbeg + p_offset;
	      pend = pbeg + pend_offset;
	    }
	}
      pos++;
    }

  tail = list;
  list = Qnil;
  for (; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (XCDR (XCDR (elt))))
	list = Fcons (Fcons (XCAR (elt), Fnreverse (XCDR (XCDR (elt)))),
		      list);
    }

  return list;
}



Lisp_Object
code_convert_region (start, end, coding_system, dst_object, encodep, norecord)
     Lisp_Object start, end, coding_system, dst_object;
     int encodep, norecord;
{
  struct coding_system coding;
  EMACS_INT from, from_byte, to, to_byte;
  Lisp_Object src_object;

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);
  if (NILP (coding_system))
    coding_system = Qno_conversion;
  else
    CHECK_CODING_SYSTEM (coding_system);
  src_object = Fcurrent_buffer ();
  if (NILP (dst_object))
    dst_object = src_object;
  else if (! EQ (dst_object, Qt))
    CHECK_BUFFER (dst_object);

  validate_region (&start, &end);
  from = XFASTINT (start);
  from_byte = CHAR_TO_BYTE (from);
  to = XFASTINT (end);
  to_byte = CHAR_TO_BYTE (to);

  setup_coding_system (coding_system, &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;

  if (encodep)
    encode_coding_object (&coding, src_object, from, from_byte, to, to_byte,
			  dst_object);
  else
    decode_coding_object (&coding, src_object, from, from_byte, to, to_byte,
			  dst_object);
  if (! norecord)
    Vlast_coding_system_used = CODING_ID_NAME (coding.id);

  if (coding.result != CODING_RESULT_SUCCESS)
    error ("Code conversion error: %d", coding.result);

  return (BUFFERP (dst_object)
	  ? make_number (coding.produced_char)
	  : coding.dst_object);
}


DEFUN ("decode-coding-region", Fdecode_coding_region, Sdecode_coding_region,
       3, 4, "r\nzCoding system: ",
       doc: /* Decode the current region from the specified coding system.
When called from a program, takes four arguments:
	START, END, CODING-SYSTEM, and DESTINATION.
START and END are buffer positions.

Optional 4th arguments DESTINATION specifies where the decoded text goes.
If nil, the region between START and END is replace by the decoded text.
If buffer, the decoded text is inserted in the buffer.
If t, the decoded text is returned.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)
It returns the length of the decoded text.  */)
     (start, end, coding_system, destination)
     Lisp_Object start, end, coding_system, destination;
{
  return code_convert_region (start, end, coding_system, destination, 0, 0);
}

DEFUN ("encode-coding-region", Fencode_coding_region, Sencode_coding_region,
       3, 4, "r\nzCoding system: ",
       doc: /* Encode the current region by specified coding system.
When called from a program, takes three arguments:
START, END, and CODING-SYSTEM.  START and END are buffer positions.

Optional 4th arguments DESTINATION specifies where the encoded text goes.
If nil, the region between START and END is replace by the encoded text.
If buffer, the encoded text is inserted in the buffer.
If t, the encoded text is returned.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)
It returns the length of the encoded text.  */)
  (start, end, coding_system, destination)
     Lisp_Object start, end, coding_system, destination;
{
  return code_convert_region (start, end, coding_system, destination, 1, 0);
}

Lisp_Object
code_convert_string (string, coding_system, dst_object,
		     encodep, nocopy, norecord)
     Lisp_Object string, coding_system, dst_object;
     int encodep, nocopy, norecord;
{
  struct coding_system coding;
  EMACS_INT chars, bytes;

  CHECK_STRING (string);
  if (NILP (coding_system))
    {
      if (! norecord)
	Vlast_coding_system_used = Qno_conversion;
      if (NILP (dst_object))
	return (nocopy ? Fcopy_sequence (string) : string);
    }

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  else
    CHECK_CODING_SYSTEM (coding_system);
  if (NILP (dst_object))
    dst_object = Qt;
  else if (! EQ (dst_object, Qt))
    CHECK_BUFFER (dst_object);

  setup_coding_system (coding_system, &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;
  chars = XSTRING (string)->size;
  bytes = STRING_BYTES (XSTRING (string));
  if (encodep)
    encode_coding_object (&coding, string, 0, 0, chars, bytes, dst_object);
  else
    decode_coding_object (&coding, string, 0, 0, chars, bytes, dst_object);
  if (! norecord)
    Vlast_coding_system_used = CODING_ID_NAME (coding.id);

  if (coding.result != CODING_RESULT_SUCCESS)
    error ("Code conversion error: %d", coding.result);

  return (BUFFERP (dst_object)
	  ? make_number (coding.produced_char)
	  : coding.dst_object);
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
  return code_convert_string (string, coding_system, Qt, encodep, 0, 1);
}


DEFUN ("decode-coding-string", Fdecode_coding_string, Sdecode_coding_string,
       2, 4, 0,
       doc: /* Decode STRING which is encoded in CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING itself
if the decoding operation is trivial.

Optional fourth arg BUFFER non-nil meant that the decoded text is
inserted in BUFFER instead of returned as a astring.  In this case,
the return value is BUFFER.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.  */)
  (string, coding_system, nocopy, buffer)
     Lisp_Object string, coding_system, nocopy, buffer;
{
  return code_convert_string (string, coding_system, buffer,
			      0, ! NILP (nocopy), 0);
}

DEFUN ("encode-coding-string", Fencode_coding_string, Sencode_coding_string,
       2, 4, 0,
       doc: /* Encode STRING to CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING
itself if the encoding operation is trivial.

Optional fourth arg BUFFER non-nil meant that the encoded text is
inserted in BUFFER instead of returned as a astring.  In this case,
the return value is BUFFER.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
     (string, coding_system, nocopy, buffer)
     Lisp_Object string, coding_system, nocopy, buffer;
{
  return code_convert_string (string, coding_system, buffer,
			      nocopy, ! NILP (nocopy), 1);
}


DEFUN ("decode-sjis-char", Fdecode_sjis_char, Sdecode_sjis_char, 1, 1, 0,
       doc: /* Decode a Japanese character which has CODE in shift_jis encoding.
Return the corresponding character.  */)
     (code)
     Lisp_Object code;
{
  Lisp_Object spec, attrs, val;
  struct charset *charset_roman, *charset_kanji, *charset_kana, *charset;
  int c;

  CHECK_NATNUM (code);
  c = XFASTINT (code);
  CHECK_CODING_SYSTEM_GET_SPEC (Vsjis_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_BYTE_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return code;

  val = CODING_ATTR_CHARSET_LIST (attrs);
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val)));

  if (c <= 0x7F)
    charset = charset_roman;
  else if (c >= 0xA0 && c < 0xDF)
    {
      charset = charset_kana;
      c -= 0x80;
    }
  else
    {
      int s1 = c >> 8, s2 = c & 0x7F;

      if (s1 < 0x81 || (s1 > 0x9F && s1 < 0xE0) || s1 > 0xEF
	  || s2 < 0x40 || s2 == 0x7F || s2 > 0xFC)
	error ("Invalid code: %d", code);
      SJIS_TO_JIS (c);
      charset = charset_kanji;
    }
  c = DECODE_CHAR (charset, c);
  if (c < 0)
    error ("Invalid code: %d", code);
  return make_number (c);
}


DEFUN ("encode-sjis-char", Fencode_sjis_char, Sencode_sjis_char, 1, 1, 0,
       doc: /* Encode a Japanese character CHAR to shift_jis encoding.
Return the corresponding code in SJIS.  */)
     (ch)
    Lisp_Object ch;
{
  Lisp_Object spec, attrs, charset_list;
  int c;
  struct charset *charset;
  unsigned code;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  CHECK_CODING_SYSTEM_GET_SPEC (Vsjis_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_CHAR_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return ch;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  charset = char_charset (c, charset_list, &code);
  if (code == CHARSET_INVALID_CODE (charset))
    error ("Can't encode by shift_jis encoding: %d", c);
  JIS_TO_SJIS (code);

  return make_number (code);
}

DEFUN ("decode-big5-char", Fdecode_big5_char, Sdecode_big5_char, 1, 1, 0,
       doc: /* Decode a Big5 character which has CODE in BIG5 coding system.
Return the corresponding character.  */)
     (code)
     Lisp_Object code;
{
  Lisp_Object spec, attrs, val;
  struct charset *charset_roman, *charset_big5, *charset;
  int c;

  CHECK_NATNUM (code);
  c = XFASTINT (code);
  CHECK_CODING_SYSTEM_GET_SPEC (Vbig5_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_BYTE_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return code;

  val = CODING_ATTR_CHARSET_LIST (attrs);
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));

  if (c <= 0x7F)
    charset = charset_roman;
  else
    {
      int b1 = c >> 8, b2 = c & 0x7F;
      if (b1 < 0xA1 || b1 > 0xFE
	  || b2 < 0x40 || (b2 > 0x7E && b2 < 0xA1) || b2 > 0xFE)
	error ("Invalid code: %d", code);
      charset = charset_big5;
    }
  c = DECODE_CHAR (charset, (unsigned )c);
  if (c < 0)
    error ("Invalid code: %d", code);
  return make_number (c);
}

DEFUN ("encode-big5-char", Fencode_big5_char, Sencode_big5_char, 1, 1, 0,
       doc: /* Encode the Big5 character CHAR to BIG5 coding system.
Return the corresponding character code in Big5.  */)
     (ch)
     Lisp_Object ch;
{
  Lisp_Object spec, attrs, charset_list;
  struct charset *charset;
  int c;
  unsigned code;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  CHECK_CODING_SYSTEM_GET_SPEC (Vbig5_coding_system, spec);
  attrs = AREF (spec, 0);
  if (ASCII_CHAR_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return ch;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  charset = char_charset (c, charset_list, &code);
  if (code == CHARSET_INVALID_CODE (charset))
    error ("Can't encode by Big5 encoding: %d", c);

  return make_number (code);
}


DEFUN ("set-terminal-coding-system-internal",
       Fset_terminal_coding_system_internal,
       Sset_terminal_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system),
			&terminal_coding);
  
  /* We had better not send unsafe characters to terminal.  */
  terminal_coding.mode |= CODING_MODE_SAFE_ENCODING;
  /* Characer composition should be disabled.  */
  terminal_coding.common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;
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
  /* Characer composition should be disabled.  */
  safe_terminal_coding.common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;
  safe_terminal_coding.src_multibyte = 1;
  safe_terminal_coding.dst_multibyte = 0;
  return Qnil;
}

DEFUN ("terminal-coding-system",
       Fterminal_coding_system, Sterminal_coding_system, 0, 0, 0,
       doc: /* Return coding system specified for terminal output.  */)
     ()
{
  return CODING_ID_NAME (terminal_coding.id);
}

DEFUN ("set-keyboard-coding-system-internal",
       Fset_keyboard_coding_system_internal,
       Sset_keyboard_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system),
		       &keyboard_coding);
  /* Characer composition should be disabled.  */
  keyboard_coding.common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;
  return Qnil;
}

DEFUN ("keyboard-coding-system",
       Fkeyboard_coding_system, Skeyboard_coding_system, 0, 0, 0,
       doc: /* Return coding system specified for decoding keyboard input.  */)
     ()
{
  return CODING_ID_NAME (keyboard_coding.id);
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
    error ("Invalid first arguement");
  if (nargs < 1 + XINT (target_idx))
    error ("Too few arguments for operation: %s",
	   XSYMBOL (operation)->name->data);
  target = args[XINT (target_idx) + 1];
  if (!(STRINGP (target)
	|| (EQ (operation, Qopen_network_stream) && INTEGERP (target))))
    error ("Invalid %dth argument", XINT (target_idx) + 1);

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

DEFUN ("set-coding-system-priority", Fset_coding_system_priority,
       Sset_coding_system_priority, 1, MANY, 0,
       doc: /* Put higher priority to coding systems of the arguments.  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  int i, j;
  int changed[coding_category_max];
  enum coding_category priorities[coding_category_max];

  bzero (changed, sizeof changed);

  for (i = j = 0; i < nargs; i++)
    {
      enum coding_category category;
      Lisp_Object spec, attrs;

      CHECK_CODING_SYSTEM_GET_SPEC (args[i], spec);
      attrs = AREF (spec, 0);
      category = XINT (CODING_ATTR_CATEGORY (attrs));
      if (changed[category])
	/* Ignore this coding system because a coding system of the
	   same category already had a higher priority.  */
	continue;
      changed[category] = 1;
      priorities[j++] = category;
      if (coding_categories[category].id >= 0
	  && ! EQ (args[i], CODING_ID_NAME (coding_categories[category].id)))
	setup_coding_system (args[i], &coding_categories[category]);
    }

  /* Now we have decided top J priorities.  Reflect the order of the
     original priorities to the remaining priorities.  */

  for (i = j, j = 0; i < coding_category_max; i++, j++)
    {
      while (j < coding_category_max
	     && changed[coding_priorities[j]])
	j++;
      if (j == coding_category_max)
	abort ();
      priorities[i] = coding_priorities[j];
    }

  bcopy (priorities, coding_priorities, sizeof priorities);
  return Qnil;
}

DEFUN ("coding-system-priority-list", Fcoding_system_priority_list,
       Scoding_system_priority_list, 0, 1, 0,
       doc: /* Return a list of coding systems ordered by their priorities.  */)
     (highestp)
     Lisp_Object highestp;
{
  int i;
  Lisp_Object val;

  for (i = 0, val = Qnil; i < coding_category_max; i++)
    {
      enum coding_category category = coding_priorities[i];
      int id = coding_categories[category].id;
      Lisp_Object attrs;

      if (id < 0)
	continue;
      attrs = CODING_ID_ATTRS (id);
      if (! NILP (highestp))
	return CODING_ATTR_BASE_NAME (attrs);
      val = Fcons (CODING_ATTR_BASE_NAME (attrs), val);
    }
  return Fnreverse (val);
}

static Lisp_Object
make_subsidiaries (base)
     Lisp_Object base;
{
  Lisp_Object subsidiaries;
  char *suffixes[] = { "-unix", "-dos", "-mac" };
  int base_name_len = STRING_BYTES (XSYMBOL (base)->name);
  char *buf = (char *) alloca (base_name_len + 6);
  int i;
      
  bcopy (XSYMBOL (base)->name->data, buf, base_name_len);
  subsidiaries = Fmake_vector (make_number (3), Qnil);
  for (i = 0; i < 3; i++)
    {
      bcopy (suffixes[i], buf + base_name_len, strlen (suffixes[i]) + 1);
      ASET (subsidiaries, i, intern (buf));
    }
  return subsidiaries;
}


DEFUN ("define-coding-system-internal", Fdefine_coding_system_internal,
       Sdefine_coding_system_internal, coding_arg_max, MANY, 0,
       doc: /* For internal use only.  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object name;
  Lisp_Object spec_vec;		/* [ ATTRS ALIASE EOL_TYPE ] */
  Lisp_Object attrs;		/* Vector of attributes.  */
  Lisp_Object eol_type;
  Lisp_Object aliases;
  Lisp_Object coding_type, charset_list, safe_charsets;
  enum coding_category category;
  Lisp_Object tail, val;
  int max_charset_id = 0;
  int i;

  if (nargs < coding_arg_max)
    goto short_args;

  attrs = Fmake_vector (make_number (coding_attr_last_index), Qnil);

  name = args[coding_arg_name];
  CHECK_SYMBOL (name);
  CODING_ATTR_BASE_NAME (attrs) = name;

  val = args[coding_arg_mnemonic];
  if (! STRINGP (val))
    CHECK_CHARACTER (val);
  CODING_ATTR_MNEMONIC (attrs) = val;

  coding_type = args[coding_arg_coding_type];
  CHECK_SYMBOL (coding_type);
  CODING_ATTR_TYPE (attrs) = coding_type;

  charset_list = args[coding_arg_charset_list];
  if (SYMBOLP (charset_list))
    {
      if (EQ (charset_list, Qiso_2022))
	{
	  if (! EQ (coding_type, Qiso_2022))
	    error ("Invalid charset-list");
	  charset_list = Viso_2022_charset_list;
	}
      else if (EQ (charset_list, Qemacs_mule))
	{
	  if (! EQ (coding_type, Qemacs_mule))
	    error ("Invalid charset-list");
	  charset_list = Vemacs_mule_charset_list;
	}
      for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
	if (max_charset_id < XFASTINT (XCAR (tail)))
	  max_charset_id = XFASTINT (XCAR (tail));
    }
  else
    {
      charset_list = Fcopy_sequence (charset_list);
      for (tail = charset_list; !NILP (tail); tail = Fcdr (tail))
	{
	  struct charset *charset;

	  val = Fcar (tail);
	  CHECK_CHARSET_GET_CHARSET (val, charset);
	  if (EQ (coding_type, Qiso_2022)
	      ? CHARSET_ISO_FINAL (charset) < 0
	      : EQ (coding_type, Qemacs_mule)
	      ? CHARSET_EMACS_MULE_ID (charset) < 0
	      : 0)
	    error ("Can't handle charset `%s'",
		   XSYMBOL (CHARSET_NAME (charset))->name->data);

	  XCAR (tail) = make_number (charset->id);
	  if (max_charset_id < charset->id)
	    max_charset_id = charset->id;
	}
    }
  CODING_ATTR_CHARSET_LIST (attrs) = charset_list;

  safe_charsets = Fmake_string (make_number (max_charset_id + 1),
				make_number (255));
  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    XSTRING (safe_charsets)->data[XFASTINT (XCAR (tail))] = 0;
  CODING_ATTR_SAFE_CHARSETS (attrs) = safe_charsets;

  val = args[coding_arg_decode_translation_table];
  if (! NILP (val))
    CHECK_CHAR_TABLE (val);
  CODING_ATTR_DECODE_TBL (attrs) = val;

  val = args[coding_arg_encode_translation_table];
  if (! NILP (val))
    CHECK_CHAR_TABLE (val);
  CODING_ATTR_ENCODE_TBL (attrs) = val;

  val = args[coding_arg_post_read_conversion];
  CHECK_SYMBOL (val);
  CODING_ATTR_POST_READ (attrs) = val;

  val = args[coding_arg_pre_write_conversion];
  CHECK_SYMBOL (val);
  CODING_ATTR_PRE_WRITE (attrs) = val;

  val = args[coding_arg_default_char];
  if (NILP (val))
    CODING_ATTR_DEFAULT_CHAR (attrs) = make_number (' ');
  else
    {
      CHECK_CHARACTER (val); 
      CODING_ATTR_DEFAULT_CHAR (attrs) = val;
    }

  val = args[coding_arg_plist];
  CHECK_LIST (val);
  CODING_ATTR_PLIST (attrs) = val;

  if (EQ (coding_type, Qcharset))
    {
      val = Fmake_vector (make_number (256), Qnil);

      for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
	{
	  struct charset *charset = CHARSET_FROM_ID (XINT (XCAR (tail)));

	  for (i = charset->code_space[0]; i <= charset->code_space[1]; i++)
	    if (NILP (AREF (val, i)))
	      ASET (val, i, XCAR (tail));
	}
      ASET (attrs, coding_attr_charset_valids, val);
      category = coding_category_charset;
    }
  else if (EQ (coding_type, Qccl))
    {
      Lisp_Object valids;
      
      if (nargs < coding_arg_ccl_max)
	goto short_args;

      val = args[coding_arg_ccl_decoder];
      CHECK_CCL_PROGRAM (val);
      if (VECTORP (val))
	val = Fcopy_sequence (val);
      ASET (attrs, coding_attr_ccl_decoder, val);

      val = args[coding_arg_ccl_encoder];
      CHECK_CCL_PROGRAM (val);
      if (VECTORP (val))
	val = Fcopy_sequence (val);
      ASET (attrs, coding_attr_ccl_encoder, val);

      val = args[coding_arg_ccl_valids];
      valids = Fmake_string (make_number (256), make_number (0));
      for (tail = val; !NILP (tail); tail = Fcdr (tail))
	{
	  val = Fcar (tail);
	  if (INTEGERP (val))
	    ASET (valids, XINT (val), 1);
	  else
	    {
	      int from, to;

	      CHECK_CONS (val);
	      CHECK_NUMBER (XCAR (val));
	      CHECK_NUMBER (XCDR (val));
	      from = XINT (XCAR (val));
	      to = XINT (XCDR (val));
	      for (i = from; i <= to; i++)
		ASET (valids, i, 1);
	    }
	}
      ASET (attrs, coding_attr_ccl_valids, valids);
      
      category = coding_category_ccl;
    }
  else if (EQ (coding_type, Qutf_16))
    {
      Lisp_Object bom, endian;

      if (nargs < coding_arg_utf16_max)
	goto short_args;

      bom = args[coding_arg_utf16_bom];
      if (! NILP (bom) && ! EQ (bom, Qt))
	{
	  CHECK_CONS (bom);
	  CHECK_CODING_SYSTEM (XCAR (bom));
	  CHECK_CODING_SYSTEM (XCDR (bom));
	}
      ASET (attrs, coding_attr_utf_16_bom, bom);

      endian = args[coding_arg_utf16_endian];
      ASET (attrs, coding_attr_utf_16_endian, endian);

      category = (CONSP (bom)
		  ? coding_category_utf_16_auto
		  : NILP (bom)
		  ? (NILP (endian)
		     ? coding_category_utf_16_be_nosig
		     : coding_category_utf_16_le_nosig)
		  : (NILP (endian)
		     ? coding_category_utf_16_be
		     : coding_category_utf_16_le));
    }
  else if (EQ (coding_type, Qiso_2022))
    {
      Lisp_Object initial, reg_usage, request, flags;
      struct charset *charset;
      int i, id;

      if (nargs < coding_arg_iso2022_max)
	goto short_args;

      initial = Fcopy_sequence (args[coding_arg_iso2022_initial]);
      CHECK_VECTOR (initial);
      for (i = 0; i < 4; i++)
	{
	  val = Faref (initial, make_number (i));
	  if (! NILP (val))
	    {
	      CHECK_CHARSET_GET_ID (val, id);
	      ASET (initial, i, make_number (id));
	    }
	  else
	    ASET (initial, i, make_number (-1));
	}

      reg_usage = args[coding_arg_iso2022_reg_usage];
      CHECK_CONS (reg_usage);
      CHECK_NATNUM (XCAR (reg_usage));
      CHECK_NATNUM (XCDR (reg_usage));

      request = Fcopy_sequence (args[coding_arg_iso2022_request]);
      for (tail = request; ! NILP (tail); tail = Fcdr (tail))
	{
	  int id;

	  val = Fcar (tail);
	  CHECK_CONS (val);
	  CHECK_CHARSET_GET_ID (XCAR (val), id);
	  CHECK_NATNUM (XCDR (val));
	  if (XINT (XCDR (val)) >= 4)
	    error ("Invalid graphic register number: %d", XINT (XCDR (val)));
	  XCAR (val) = make_number (id);
	}

      flags = args[coding_arg_iso2022_flags];
      CHECK_NATNUM (flags);
      i = XINT (flags);
      if (EQ (args[coding_arg_charset_list], Qiso_2022))
	flags = make_number (i | CODING_ISO_FLAG_FULL_SUPPORT);

      ASET (attrs, coding_attr_iso_initial, initial);
      ASET (attrs, coding_attr_iso_usage, reg_usage);
      ASET (attrs, coding_attr_iso_request, request);
      ASET (attrs, coding_attr_iso_flags, flags);
      setup_iso_safe_charsets (attrs);

      if (i & CODING_ISO_FLAG_SEVEN_BITS)
	category = ((i & (CODING_ISO_FLAG_LOCKING_SHIFT
			  | CODING_ISO_FLAG_SINGLE_SHIFT))
		    ? coding_category_iso_7_else
		    : EQ (args[coding_arg_charset_list], Qiso_2022)
		    ? coding_category_iso_7
		    : coding_category_iso_7_tight);
      else
	{
	  int id = XINT (AREF (initial, 1));

	  category = (((i & (CODING_ISO_FLAG_LOCKING_SHIFT
			     | CODING_ISO_FLAG_SINGLE_SHIFT))
		       || EQ (args[coding_arg_charset_list], Qiso_2022)
		       || id < 0)
		      ? coding_category_iso_8_else
		      : (CHARSET_DIMENSION (CHARSET_FROM_ID (id)) == 1)
		      ? coding_category_iso_8_1
		      : coding_category_iso_8_2);
	}
    }
  else if (EQ (coding_type, Qemacs_mule))
    {
      if (EQ (args[coding_arg_charset_list], Qemacs_mule))
	ASET (attrs, coding_attr_emacs_mule_full, Qt);

      category = coding_category_emacs_mule;
    }
  else if (EQ (coding_type, Qshift_jis))
    {

      struct charset *charset;

      if (XINT (Flength (charset_list)) != 3)
	error ("There should be just three charsets");

      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       XSYMBOL (CHARSET_NAME (charset))->name->data);

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       XSYMBOL (CHARSET_NAME (charset))->name->data);

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 2)
	error ("Dimension of charset %s is not two",
	       XSYMBOL (CHARSET_NAME (charset))->name->data);

      category = coding_category_sjis;
      Vsjis_coding_system = name;
    }
  else if (EQ (coding_type, Qbig5))
    {
      struct charset *charset;

      if (XINT (Flength (charset_list)) != 2)
	error ("There should be just two charsets");

      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       XSYMBOL (CHARSET_NAME (charset))->name->data);

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 2)
	error ("Dimension of charset %s is not two",
	       XSYMBOL (CHARSET_NAME (charset))->name->data);

      category = coding_category_big5;
      Vbig5_coding_system = name;
    }
  else if (EQ (coding_type, Qraw_text))
    category = coding_category_raw_text;
  else if (EQ (coding_type, Qutf_8))
    category = coding_category_utf_8;
  else if (EQ (coding_type, Qundecided))
    category = coding_category_undecided;
  else
    error ("Invalid coding system type: %s",
	   XSYMBOL (coding_type)->name->data);

  CODING_ATTR_CATEGORY (attrs) = make_number (category);

  eol_type = args[coding_arg_eol_type];
  if (! NILP (eol_type)
      && ! EQ (eol_type, Qunix)
      && ! EQ (eol_type, Qdos)
      && ! EQ (eol_type, Qmac))
    error ("Invalid eol-type");

  aliases = Fcons (name, Qnil);

  if (NILP (eol_type))
    {
      eol_type = make_subsidiaries (name);
      for (i = 0; i < 3; i++)
	{
	  Lisp_Object this_spec, this_name, this_aliases, this_eol_type;

	  this_name = AREF (eol_type, i);
	  this_aliases = Fcons (this_name, Qnil);
	  this_eol_type = (i == 0 ? Qunix : i == 1 ? Qdos : Qmac);
	  this_spec = Fmake_vector (make_number (3), attrs);
	  ASET (this_spec, 1, this_aliases);
	  ASET (this_spec, 2, this_eol_type);
	  Fputhash (this_name, this_spec, Vcoding_system_hash_table);
	  Vcoding_system_list = Fcons (this_name, Vcoding_system_list);
	  Vcoding_system_alist = Fcons (Fcons (Fsymbol_name (this_name), Qnil),
					Vcoding_system_alist);
	}
    }

  spec_vec = Fmake_vector (make_number (3), attrs);
  ASET (spec_vec, 1, aliases);
  ASET (spec_vec, 2, eol_type);

  Fputhash (name, spec_vec, Vcoding_system_hash_table);
  Vcoding_system_list = Fcons (name, Vcoding_system_list);
  Vcoding_system_alist = Fcons (Fcons (Fsymbol_name (name), Qnil),
				Vcoding_system_alist);

  {
    int id = coding_categories[category].id;

    if (id < 0 || EQ (name, CODING_ID_NAME (id)))
      setup_coding_system (name, &coding_categories[category]);
  }

  return Qnil;

 short_args:
  return Fsignal (Qwrong_number_of_arguments,
		  Fcons (intern ("define-coding-system-internal"),
			 make_number (nargs)));
}

DEFUN ("define-coding-system-alias", Fdefine_coding_system_alias,
       Sdefine_coding_system_alias, 2, 2, 0,
       doc: /* Define ALIAS as an alias for CODING-SYSTEM.  */)
     (alias, coding_system)
     Lisp_Object alias, coding_system;
{
  Lisp_Object spec, aliases, eol_type;

  CHECK_SYMBOL (alias);
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  aliases = AREF (spec, 1);
  while (!NILP (XCDR (aliases)))
    aliases = XCDR (aliases);
  XCDR (aliases) = Fcons (alias, Qnil);

  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    {
      Lisp_Object subsidiaries;
      int i;

      subsidiaries = make_subsidiaries (alias);
      for (i = 0; i < 3; i++)
	Fdefine_coding_system_alias (AREF (subsidiaries, i),
				     AREF (eol_type, i));

      ASET (spec, 2, subsidiaries);
    }

  Fputhash (alias, spec, Vcoding_system_hash_table);
  Vcoding_system_alist = Fcons (Fcons (alias, Qnil), Vcoding_system_alist);

  return Qnil;
}

DEFUN ("coding-system-base", Fcoding_system_base, Scoding_system_base,
       1, 1, 0,
       doc: /* Return the base of CODING-SYSTEM.
Any alias or subsidiary coding systems are not base coding system.  */)
  (coding_system)
     Lisp_Object coding_system;
{
  Lisp_Object spec, attrs;

  if (NILP (coding_system))
    return (Qno_conversion);
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  attrs = AREF (spec, 0);
  return CODING_ATTR_BASE_NAME (attrs);
}

DEFUN ("coding-system-plist", Fcoding_system_plist, Scoding_system_plist,
       1, 1, 0,
       doc: "Return the property list of CODING-SYSTEM.")
     (coding_system)
     Lisp_Object coding_system;
{
  Lisp_Object spec, attrs;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  attrs = AREF (spec, 0);
  return CODING_ATTR_PLIST (attrs);
}


DEFUN ("coding-system-aliases", Fcoding_system_aliases, Scoding_system_aliases,
       1, 1, 0,
       doc: /* Return the list of aliases of CODING-SYSTEM.
A base coding system is what made by `define-coding-system'.
Any alias nor subsidiary coding systems are not base coding system.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  Lisp_Object spec;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  return AREF (spec, 2);
}

DEFUN ("coding-system-eol-type", Fcoding_system_eol_type,
       Scoding_system_eol_type, 1, 1, 0,
       doc: /* Return eol-type of CODING-SYSTEM.
An eol-type is integer 0, 1, 2, or a vector of coding systems.

Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
and CR respectively.

A vector value indicates that a format of end-of-line should be
detected automatically.  Nth element of the vector is the subsidiary
coding system whose eol-type is N.  */)
     (coding_system)
     Lisp_Object coding_system;
{
  Lisp_Object spec, eol_type;
  int n;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  if (! CODING_SYSTEM_P (coding_system))
    return Qnil;
  spec = CODING_SYSTEM_SPEC (coding_system);
  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    return Fcopy_sequence (eol_type);
  n = EQ (eol_type, Qunix) ? 0 : EQ (eol_type, Qdos) ? 1 : 2;
  return make_number (n);
}

#endif /* emacs */


/*** 9. Post-amble ***/

void
init_coding_once ()
{
  int i;

  for (i = 0; i < coding_category_max; i++)
    {
      coding_categories[i].id = -1;
      coding_priorities[i] = i;
    }

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

  inhibit_pre_post_conversion = 0;

  for (i = 0; i < 256; i++)
    {
      emacs_mule_bytes[i] = 1;
    }
}

#ifdef emacs

void
syms_of_coding ()
{
  staticpro (&Vcoding_system_hash_table);
  Vcoding_system_hash_table = Fmakehash (Qeq);

  staticpro (&Vsjis_coding_system);
  Vsjis_coding_system = Qnil;

  staticpro (&Vbig5_coding_system);
  Vbig5_coding_system = Qnil;

  staticpro (&Vcode_conversion_work_buf_list);
  Vcode_conversion_work_buf_list = Qnil;

  staticpro (&Vcode_conversion_reused_work_buf);
  Vcode_conversion_reused_work_buf = Qnil;

  DEFSYM (Qcharset, "charset");
  DEFSYM (Qtarget_idx, "target-idx");
  DEFSYM (Qcoding_system_history, "coding-system-history");
  Fset (Qcoding_system_history, Qnil);

  /* Target FILENAME is the first argument.  */
  Fput (Qinsert_file_contents, Qtarget_idx, make_number (0));
  /* Target FILENAME is the third argument.  */
  Fput (Qwrite_region, Qtarget_idx, make_number (2));

  DEFSYM (Qcall_process, "call-process");
  /* Target PROGRAM is the first argument.  */
  Fput (Qcall_process, Qtarget_idx, make_number (0));

  DEFSYM (Qcall_process_region, "call-process-region");
  /* Target PROGRAM is the third argument.  */
  Fput (Qcall_process_region, Qtarget_idx, make_number (2));

  DEFSYM (Qstart_process, "start-process");
  /* Target PROGRAM is the third argument.  */
  Fput (Qstart_process, Qtarget_idx, make_number (2));

  DEFSYM (Qopen_network_stream, "open-network-stream");
  /* Target SERVICE is the fourth argument.  */
  Fput (Qopen_network_stream, Qtarget_idx, make_number (3));

  DEFSYM (Qcoding_system, "coding-system");
  DEFSYM (Qcoding_aliases, "coding-aliases");

  DEFSYM (Qeol_type, "eol-type");
  DEFSYM (Qunix, "unix");
  DEFSYM (Qdos, "dos");
  DEFSYM (Qmac, "mac");

  DEFSYM (Qbuffer_file_coding_system, "buffer-file-coding-system");
  DEFSYM (Qpost_read_conversion, "post-read-conversion");
  DEFSYM (Qpre_write_conversion, "pre-write-conversion");
  DEFSYM (Qdefault_char, "default-char");
  DEFSYM (Qundecided, "undecided");
  DEFSYM (Qno_conversion, "no-conversion");
  DEFSYM (Qraw_text, "raw-text");

  DEFSYM (Qiso_2022, "iso-2022");

  DEFSYM (Qutf_8, "utf-8");

  DEFSYM (Qutf_16, "utf-16");
  DEFSYM (Qutf_16_be, "utf-16-be");
  DEFSYM (Qutf_16_be_nosig, "utf-16-be-nosig");
  DEFSYM (Qutf_16_le, "utf-16-l3");
  DEFSYM (Qutf_16_le_nosig, "utf-16-le-nosig");
  DEFSYM (Qsignature, "signature");
  DEFSYM (Qendian, "endian");
  DEFSYM (Qbig, "big");
  DEFSYM (Qlittle, "little");

  DEFSYM (Qshift_jis, "shift-jis");
  DEFSYM (Qbig5, "big5");

  DEFSYM (Qcoding_system_p, "coding-system-p");

  DEFSYM (Qcoding_system_error, "coding-system-error");
  Fput (Qcoding_system_error, Qerror_conditions,
	Fcons (Qcoding_system_error, Fcons (Qerror, Qnil)));
  Fput (Qcoding_system_error, Qerror_message,
	build_string ("Invalid coding system"));

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  DEFSYM (Qtranslation_table, "translation-table");
  Fput (Qtranslation_table, Qchar_table_extra_slots, make_number (1));
  DEFSYM (Qtranslation_table_id, "translation-table-id");
  DEFSYM (Qtranslation_table_for_decode, "translation-table-for-decode");
  DEFSYM (Qtranslation_table_for_encode, "translation-table-for-encode");

  DEFSYM (Qchar_coding_system, "char-coding-system");

  Fput (Qchar_coding_system, Qchar_table_extra_slots, make_number (2));

  DEFSYM (Qvalid_codes, "valid-codes");

  DEFSYM (Qemacs_mule, "emacs-mule");

  Vcoding_category_table
    = Fmake_vector (make_number (coding_category_max), Qnil);
  staticpro (&Vcoding_category_table);
  /* Followings are target of code detection.  */
  ASET (Vcoding_category_table, coding_category_iso_7,
	intern ("coding-category-iso-7"));
  ASET (Vcoding_category_table, coding_category_iso_7_tight,
	intern ("coding-category-iso-7-tight"));
  ASET (Vcoding_category_table, coding_category_iso_8_1,
	intern ("coding-category-iso-8-1"));
  ASET (Vcoding_category_table, coding_category_iso_8_2,
	intern ("coding-category-iso-8-2"));
  ASET (Vcoding_category_table, coding_category_iso_7_else,
	intern ("coding-category-iso-7-else"));
  ASET (Vcoding_category_table, coding_category_iso_8_else,
	intern ("coding-category-iso-8-else"));
  ASET (Vcoding_category_table, coding_category_utf_8,
	intern ("coding-category-utf-8"));
  ASET (Vcoding_category_table, coding_category_utf_16_be,
	intern ("coding-category-utf-16-be"));
  ASET (Vcoding_category_table, coding_category_utf_16_le,
	intern ("coding-category-utf-16-le"));
  ASET (Vcoding_category_table, coding_category_utf_16_be_nosig,
	intern ("coding-category-utf-16-be-nosig"));
  ASET (Vcoding_category_table, coding_category_utf_16_le_nosig,
	intern ("coding-category-utf-16-le-nosig"));
  ASET (Vcoding_category_table, coding_category_charset,
	intern ("coding-category-charset"));
  ASET (Vcoding_category_table, coding_category_sjis,
	intern ("coding-category-sjis"));
  ASET (Vcoding_category_table, coding_category_big5,
	intern ("coding-category-big5"));
  ASET (Vcoding_category_table, coding_category_ccl,
	intern ("coding-category-ccl"));
  ASET (Vcoding_category_table, coding_category_emacs_mule,
	intern ("coding-category-emacs-mule"));
  /* Followings are NOT target of code detection.  */
  ASET (Vcoding_category_table, coding_category_raw_text,
	intern ("coding-category-raw-text"));
  ASET (Vcoding_category_table, coding_category_undecided,
	intern ("coding-category-undecided"));

  defsubr (&Scoding_system_p);
  defsubr (&Sread_coding_system);
  defsubr (&Sread_non_nil_coding_system);
  defsubr (&Scheck_coding_system);
  defsubr (&Sdetect_coding_region);
  defsubr (&Sdetect_coding_string);
  defsubr (&Sfind_coding_systems_region_internal);
  defsubr (&Scheck_coding_systems_region);
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
  defsubr (&Sset_coding_system_priority);
  defsubr (&Sdefine_coding_system_internal);
  defsubr (&Sdefine_coding_system_alias);
  defsubr (&Scoding_system_base);
  defsubr (&Scoding_system_plist);
  defsubr (&Scoding_system_aliases);
  defsubr (&Scoding_system_eol_type);
  defsubr (&Scoding_system_priority_list);

  DEFVAR_LISP ("coding-system-list", &Vcoding_system_list,
	       doc: /* List of coding systems.

Do not alter the value of this variable manually.  This variable should be
updated by the functions `define-coding-system' and
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
    for (i = coding_category_max - 1; i >= 0; i--)
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
	       doc: /*
Coding system used in the latest file or process I/O.  */);
  Vlast_coding_system_used = Qnil;

  DEFVAR_BOOL ("inhibit-eol-conversion", &inhibit_eol_conversion,
	       doc: /*
*Non-nil means always inhibit code conversion of end-of-line format.
See info node `Coding Systems' and info node `Text and Binary' concerning
such conversion.  */);
  inhibit_eol_conversion = 0;

  DEFVAR_BOOL ("inherit-process-coding-system", &inherit_process_coding_system,
	       doc: /*
Non-nil means process buffer inherits coding system of process output.
Bind it to t if the process output is to be treated as if it were a file
read from some filesystem.  */);
  inherit_process_coding_system = 0;

  DEFVAR_LISP ("file-coding-system-alist", &Vfile_coding_system_alist,
	       doc: /*
Alist to decide a coding system to use for a file I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a file name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding and encoding
the file contents.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.  The function gets
the arguments with which `find-operation-coding-systems' was called.

See also the function `find-operation-coding-system'
and the variable `auto-coding-alist'.  */);
  Vfile_coding_system_alist = Qnil;

  DEFVAR_LISP ("process-coding-system-alist", &Vprocess_coding_system_alist,
	       doc: /*
Alist to decide a coding system to use for a process I/O operation.
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
	       doc: /*
Alist to decide a coding system to use for a network I/O operation.
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
	       doc: /*
*String displayed in mode line for UNIX-like (LF) end-of-line format.  */);
  eol_mnemonic_unix = build_string (":");

  DEFVAR_LISP ("eol-mnemonic-dos", &eol_mnemonic_dos,
	       doc: /*
*String displayed in mode line for DOS-like (CRLF) end-of-line format.  */);
  eol_mnemonic_dos = build_string ("\\");

  DEFVAR_LISP ("eol-mnemonic-mac", &eol_mnemonic_mac,
	       doc: /*
*String displayed in mode line for MAC-like (CR) end-of-line format.  */);
  eol_mnemonic_mac = build_string ("/");

  DEFVAR_LISP ("eol-mnemonic-undecided", &eol_mnemonic_undecided,
	       doc: /*
*String displayed in mode line when end-of-line format is not yet determined.  */);
  eol_mnemonic_undecided = build_string (":");

  DEFVAR_LISP ("enable-character-translation", &Venable_character_translation,
	       doc: /*
*Non-nil enables character translation while encoding and decoding.  */);
  Venable_character_translation = Qt;

  DEFVAR_LISP ("standard-translation-table-for-decode",
	       &Vstandard_translation_table_for_decode,
	       doc: /* Table for translating characters while decoding.  */);
  Vstandard_translation_table_for_decode = Qnil;

  DEFVAR_LISP ("standard-translation-table-for-encode",
	       &Vstandard_translation_table_for_encode,
	       doc: /* Table for translating characters while encoding.  */);
  Vstandard_translation_table_for_encode = Qnil;

  DEFVAR_LISP ("charset-revision-table", &Vcharset_revision_table,
	       doc: /* Alist of charsets vs revision numbers.
While encoding, if a charset (car part of an element) is found,
designate it with the escape sequence identifying revision (cdr part
of the element).  */);
  Vcharset_revision_table = Qnil;

  DEFVAR_LISP ("default-process-coding-system",
	       &Vdefault_process_coding_system,
	       doc: /* Cons of coding systems used for process I/O by default.
The car part is used for decoding a process output,
the cdr part is used for encoding a text to be sent to a process.  */);
  Vdefault_process_coding_system = Qnil;

  DEFVAR_LISP ("latin-extra-code-table", &Vlatin_extra_code_table,
	       doc: /*
Table of extra Latin codes in the range 128..159 (inclusive).
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
	       doc: /*
Function to call to select safe coding system for encoding a text.

If set, this function is called to force a user to select a proper
coding system which can encode the text in the case that a default
coding system used in each operation can't encode the text.

The default value is `select-safe-coding-system' (which see).  */);
  Vselect_safe_coding_system_function = Qnil;

  DEFVAR_LISP ("char-coding-system-table", &Vchar_coding_system_table,
	       doc: /*
Char-table containing safe coding systems of each characters.
Each element doesn't include such generic coding systems that can
encode any characters.   They are in the first extra slot.  */);
  Vchar_coding_system_table = Fmake_char_table (Qchar_coding_system, Qnil);

  DEFVAR_BOOL ("inhibit-iso-escape-detection",
	       &inhibit_iso_escape_detection,
	       doc: /*
If non-nil, Emacs ignores ISO2022's escape sequence on code detection.

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

  {
    Lisp_Object args[coding_arg_max];
    Lisp_Object plist[14];
    int i;

    for (i = 0; i < coding_arg_max; i++)
      args[i] = Qnil;

    plist[0] = intern (":name");
    plist[1] = args[coding_arg_name] = Qno_conversion;
    plist[2] = intern (":mnemonic");
    plist[3] = args[coding_arg_mnemonic] = make_number ('=');
    plist[4] = intern (":coding-type");
    plist[5] = args[coding_arg_coding_type] = Qraw_text;
    plist[6] = intern (":ascii-compatible-p");
    plist[7] = args[coding_arg_ascii_compatible_p] = Qt;
    plist[8] = intern (":default-char");
    plist[9] = args[coding_arg_default_char] = make_number (0);
    plist[10] = intern (":docstring");
    plist[11] = build_string ("Do no conversion.\n\
\n\
When you visit a file with this coding, the file is read into a\n\
unibyte buffer as is, thus each byte of a file is treated as a\n\
character.");
    plist[12] = intern (":eol-type");
    plist[13] = args[coding_arg_eol_type] = Qunix;
    args[coding_arg_plist] = Flist (14, plist);
    Fdefine_coding_system_internal (coding_arg_max, args);
  }

  setup_coding_system (Qno_conversion, &keyboard_coding);
  setup_coding_system (Qno_conversion, &terminal_coding);
  setup_coding_system (Qno_conversion, &safe_terminal_coding);
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
