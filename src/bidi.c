/* Low-level bidirectional buffer-scanning functions for GNU Emacs.
   Copyright (C) 2000, 2001, 2004, 2005, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Eli Zaretskii <eliz@gnu.org>.

   A sequential implementation of the Unicode Bidirectional algorithm,
   as per UAX#9, a part of the Unicode Standard.

   Unlike the reference and most other implementations, this one is
   designed to be called once for every character in the buffer.

   The main entry point is bidi_get_next_char_visually.  Each time it
   is called, it finds the next character in the visual order, and
   returns its information in a special structure.  The caller is then
   expected to process this character for display or any other
   purposes, and call bidi_get_next_char_visually for the next
   character.  See the comments in bidi_get_next_char_visually for
   more details about its algorithm that finds the next visual-order
   character by resolving their levels on the fly.

   If you want to understand the code, you will have to read it
   together with the relevant portions of UAX#9.  The comments include
   references to UAX#9 rules, for that very reason.

   A note about references to UAX#9 rules: if the reference says
   something like "X9/Retaining", it means that you need to refer to
   rule X9 and to its modifications decribed in the "Implementation
   Notes" section of UAX#9, under "Retaining Format Codes".  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <setjmp.h>

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "dispextern.h"

static int bidi_initialized = 0;

static Lisp_Object bidi_type_table;

/* FIXME: Remove these when bidi_explicit_dir_char uses a lookup table.  */
#define LRM_CHAR   0x200E
#define RLM_CHAR   0x200F
#define LRE_CHAR   0x202A
#define RLE_CHAR   0x202B
#define PDF_CHAR   0x202C
#define LRO_CHAR   0x202D
#define RLO_CHAR   0x202E

#define BIDI_EOB   -1
#define BIDI_BOB   -2		/* FIXME: Is this needed? */

/* Local data structures.  (Look in dispextern.h for the rest.)  */

/* What we need to know about the current paragraph.  */
struct bidi_paragraph_info {
  int start_bytepos;	/* byte position where it begins */
  int end_bytepos;	/* byte position where it ends */
  int embedding_level;	/* its basic embedding level */
  bidi_dir_t base_dir;	/* its base direction */
};

/* Data type for describing the bidirectional character categories.  */
typedef enum {
  UNKNOWN_BC,
  NEUTRAL,
  WEAK,
  STRONG
} bidi_category_t;

int bidi_ignore_explicit_marks_for_paragraph_level = 1;

static Lisp_Object fallback_paragraph_start_re, fallback_paragraph_separate_re;
static Lisp_Object Qparagraph_start, Qparagraph_separate;

static void
bidi_initialize ()
{
  /* FIXME: This should come from the Unicode Database.  */
  struct {
    int from, to;
    bidi_type_t type;
  } bidi_type[] =
      { { 0x0000, 0x0008, WEAK_BN },
	{ 0x0009, 0x0000, NEUTRAL_S },
	{ 0x000A, 0x0000, NEUTRAL_B },
	{ 0x000B, 0x0000, NEUTRAL_S },
	{ 0x000C, 0x0000, NEUTRAL_WS },
	{ 0x000D, 0x0000, NEUTRAL_B },
	{ 0x000E, 0x001B, WEAK_BN },
	{ 0x001C, 0x001E, NEUTRAL_B },
	{ 0x001F, 0x0000, NEUTRAL_S },
	{ 0x0020, 0x0000, NEUTRAL_WS },
	{ 0x0021, 0x0022, NEUTRAL_ON },
	{ 0x0023, 0x0025, WEAK_ET },
	{ 0x0026, 0x002A, NEUTRAL_ON },
	{ 0x002B, 0x0000, WEAK_ES },
	{ 0x002C, 0x0000, WEAK_CS },
	{ 0x002D, 0x0000, WEAK_ES },
	{ 0x002E, 0x002F, WEAK_CS },
	{ 0x0030, 0x0039, WEAK_EN },
	{ 0x003A, 0x0000, WEAK_CS },
	{ 0x003B, 0x0040, NEUTRAL_ON },
	{ 0x005B, 0x0060, NEUTRAL_ON },
	{ 0x007B, 0x007E, NEUTRAL_ON },
	{ 0x007F, 0x0084, WEAK_BN },
	{ 0x0085, 0x0000, NEUTRAL_B },
	{ 0x0086, 0x009F, WEAK_BN },
	{ 0x00A0, 0x0000, WEAK_CS },
	{ 0x00A1, 0x0000, NEUTRAL_ON },
	{ 0x00A2, 0x00A5, WEAK_ET },
	{ 0x00A6, 0x00A9, NEUTRAL_ON },
	{ 0x00AB, 0x00AC, NEUTRAL_ON },
	{ 0x00AD, 0x0000, WEAK_BN },
	{ 0x00AE, 0x00Af, NEUTRAL_ON },
	{ 0x00B0, 0x00B1, WEAK_ET },
	{ 0x00B2, 0x00B3, WEAK_EN },
	{ 0x00B4, 0x0000, NEUTRAL_ON },
	{ 0x00B6, 0x00B8, NEUTRAL_ON },
	{ 0x00B9, 0x0000, WEAK_EN },
	{ 0x00BB, 0x00BF, NEUTRAL_ON },
	{ 0x00D7, 0x0000, NEUTRAL_ON },
	{ 0x00F7, 0x0000, NEUTRAL_ON },
	{ 0x02B9, 0x02BA, NEUTRAL_ON },
	{ 0x02C2, 0x02CF, NEUTRAL_ON },
	{ 0x02D2, 0x02DF, NEUTRAL_ON },
	{ 0x02E5, 0x02ED, NEUTRAL_ON },
	{ 0x0300, 0x036F, WEAK_NSM },
	{ 0x0374, 0x0375, NEUTRAL_ON },
	{ 0x037E, 0x0385, NEUTRAL_ON },
	{ 0x0387, 0x0000, NEUTRAL_ON },
	{ 0x03F6, 0x0000, NEUTRAL_ON },
	{ 0x0483, 0x0489, WEAK_NSM },
	{ 0x058A, 0x0000, NEUTRAL_ON },
	{ 0x0591, 0x05BD, WEAK_NSM },
	{ 0x05BE, 0x0000, STRONG_R },
	{ 0x05BF, 0x0000, WEAK_NSM },
	{ 0x05C0, 0x0000, STRONG_R },
	{ 0x05C1, 0x05C2, WEAK_NSM },
	{ 0x05C3, 0x0000, STRONG_R },
	{ 0x05C4, 0x05C5, WEAK_NSM },
	{ 0x05C6, 0x0000, STRONG_R },
	{ 0x05C7, 0x0000, WEAK_NSM },
	{ 0x05D0, 0x05F4, STRONG_R },
	{ 0x060C, 0x0000, WEAK_CS },
	{ 0x061B, 0x064A, STRONG_AL },
	{ 0x064B, 0x0655, WEAK_NSM },
	{ 0x0660, 0x0669, WEAK_AN },
	{ 0x066A, 0x0000, WEAK_ET },
	{ 0x066B, 0x066C, WEAK_AN },
	{ 0x066D, 0x066F, STRONG_AL },
	{ 0x0670, 0x0000, WEAK_NSM },
	{ 0x0671, 0x06D5, STRONG_AL },
	{ 0x06D6, 0x06DC, WEAK_NSM },
	{ 0x06DD, 0x0000, STRONG_AL },
	{ 0x06DE, 0x06E4, WEAK_NSM },
	{ 0x06E5, 0x06E6, STRONG_AL },
	{ 0x06E7, 0x06E8, WEAK_NSM },
	{ 0x06E9, 0x0000, NEUTRAL_ON },
	{ 0x06EA, 0x06ED, WEAK_NSM },
	{ 0x06F0, 0x06F9, WEAK_EN },
	{ 0x06FA, 0x070D, STRONG_AL },
	{ 0x070F, 0x0000, WEAK_BN },
	{ 0x0710, 0x0000, STRONG_AL },
	{ 0x0711, 0x0000, WEAK_NSM },
	{ 0x0712, 0x072C, STRONG_AL },
	{ 0x0730, 0x074A, WEAK_NSM },
	{ 0x0780, 0x07A5, STRONG_AL },
	{ 0x07A6, 0x07B0, WEAK_NSM },
	{ 0x07B1, 0x0000, STRONG_AL },
	{ 0x0901, 0x0902, WEAK_NSM },
	{ 0x093C, 0x0000, WEAK_NSM },
	{ 0x0941, 0x0948, WEAK_NSM },
	{ 0x094D, 0x0000, WEAK_NSM },
	{ 0x0951, 0x0954, WEAK_NSM },
	{ 0x0962, 0x0963, WEAK_NSM },
	{ 0x0981, 0x0000, WEAK_NSM },
	{ 0x09BC, 0x0000, WEAK_NSM },
	{ 0x09C1, 0x09C4, WEAK_NSM },
	{ 0x09CD, 0x0000, WEAK_NSM },
	{ 0x09E2, 0x09E3, WEAK_NSM },
	{ 0x09F2, 0x09F3, WEAK_ET },
	{ 0x0A02, 0x0000, WEAK_NSM },
	{ 0x0A3C, 0x0000, WEAK_NSM },
	{ 0x0A41, 0x0A4D, WEAK_NSM },
	{ 0x0A70, 0x0A71, WEAK_NSM },
	{ 0x0A81, 0x0A82, WEAK_NSM },
	{ 0x0ABC, 0x0000, WEAK_NSM },
	{ 0x0AC1, 0x0AC8, WEAK_NSM },
	{ 0x0ACD, 0x0000, WEAK_NSM },
	{ 0x0B01, 0x0000, WEAK_NSM },
	{ 0x0B3C, 0x0000, WEAK_NSM },
	{ 0x0B3F, 0x0000, WEAK_NSM },
	{ 0x0B41, 0x0B43, WEAK_NSM },
	{ 0x0B4D, 0x0B56, WEAK_NSM },
	{ 0x0B82, 0x0000, WEAK_NSM },
	{ 0x0BC0, 0x0000, WEAK_NSM },
	{ 0x0BCD, 0x0000, WEAK_NSM },
	{ 0x0C3E, 0x0C40, WEAK_NSM },
	{ 0x0C46, 0x0C56, WEAK_NSM },
	{ 0x0CBF, 0x0000, WEAK_NSM },
	{ 0x0CC6, 0x0000, WEAK_NSM },
	{ 0x0CCC, 0x0CCD, WEAK_NSM },
	{ 0x0D41, 0x0D43, WEAK_NSM },
	{ 0x0D4D, 0x0000, WEAK_NSM },
	{ 0x0DCA, 0x0000, WEAK_NSM },
	{ 0x0DD2, 0x0DD6, WEAK_NSM },
	{ 0x0E31, 0x0000, WEAK_NSM },
	{ 0x0E34, 0x0E3A, WEAK_NSM },
	{ 0x0E3F, 0x0000, WEAK_ET },
	{ 0x0E47, 0x0E4E, WEAK_NSM },
	{ 0x0EB1, 0x0000, WEAK_NSM },
	{ 0x0EB4, 0x0EBC, WEAK_NSM },
	{ 0x0EC8, 0x0ECD, WEAK_NSM },
	{ 0x0F18, 0x0F19, WEAK_NSM },
	{ 0x0F35, 0x0000, WEAK_NSM },
	{ 0x0F37, 0x0000, WEAK_NSM },
	{ 0x0F39, 0x0000, WEAK_NSM },
	{ 0x0F3A, 0x0F3D, NEUTRAL_ON },
	{ 0x0F71, 0x0F7E, WEAK_NSM },
	{ 0x0F80, 0x0F84, WEAK_NSM },
	{ 0x0F86, 0x0F87, WEAK_NSM },
	{ 0x0F90, 0x0FBC, WEAK_NSM },
	{ 0x0FC6, 0x0000, WEAK_NSM },
	{ 0x102D, 0x1030, WEAK_NSM },
	{ 0x1032, 0x1037, WEAK_NSM },
	{ 0x1039, 0x0000, WEAK_NSM },
	{ 0x1058, 0x1059, WEAK_NSM },
	{ 0x1680, 0x0000, NEUTRAL_WS },
	{ 0x169B, 0x169C, NEUTRAL_ON },
	{ 0x1712, 0x1714, WEAK_NSM },
	{ 0x1732, 0x1734, WEAK_NSM },
	{ 0x1752, 0x1753, WEAK_NSM },
	{ 0x1772, 0x1773, WEAK_NSM },
	{ 0x17B7, 0x17BD, WEAK_NSM },
	{ 0x17C6, 0x0000, WEAK_NSM },
	{ 0x17C9, 0x17D3, WEAK_NSM },
	{ 0x17DB, 0x0000, WEAK_ET },
	{ 0x1800, 0x180A, NEUTRAL_ON },
	{ 0x180B, 0x180D, WEAK_NSM },
	{ 0x180E, 0x0000, WEAK_BN },
	{ 0x18A9, 0x0000, WEAK_NSM },
	{ 0x1FBD, 0x0000, NEUTRAL_ON },
	{ 0x1FBF, 0x1FC1, NEUTRAL_ON },
	{ 0x1FCD, 0x1FCF, NEUTRAL_ON },
	{ 0x1FDD, 0x1FDF, NEUTRAL_ON },
	{ 0x1FED, 0x1FEF, NEUTRAL_ON },
	{ 0x1FFD, 0x1FFE, NEUTRAL_ON },
	{ 0x2000, 0x200A, NEUTRAL_WS },
	{ 0x200B, 0x200D, WEAK_BN },
	{ 0x200F, 0x0000, STRONG_R },
	{ 0x2010, 0x2027, NEUTRAL_ON },
	{ 0x2028, 0x0000, NEUTRAL_WS },
	{ 0x2029, 0x0000, NEUTRAL_B },
	{ 0x202A, 0x0000, LRE },
	{ 0x202B, 0x0000, RLE },
	{ 0x202C, 0x0000, PDF },
	{ 0x202D, 0x0000, LRO },
	{ 0x202E, 0x0000, RLO },
	{ 0x202F, 0x0000, NEUTRAL_WS },
	{ 0x2030, 0x2034, WEAK_ET },
	{ 0x2035, 0x2057, NEUTRAL_ON },
	{ 0x205F, 0x0000, NEUTRAL_WS },
	{ 0x2060, 0x206F, WEAK_BN },
	{ 0x2070, 0x0000, WEAK_EN },
	{ 0x2074, 0x2079, WEAK_EN },
	{ 0x207A, 0x207B, WEAK_ET },
	{ 0x207C, 0x207E, NEUTRAL_ON },
	{ 0x2080, 0x2089, WEAK_EN },
	{ 0x208A, 0x208B, WEAK_ET },
	{ 0x208C, 0x208E, NEUTRAL_ON },
	{ 0x20A0, 0x20B1, WEAK_ET },
	{ 0x20D0, 0x20EA, WEAK_NSM },
	{ 0x2100, 0x2101, NEUTRAL_ON },
	{ 0x2103, 0x2106, NEUTRAL_ON },
	{ 0x2108, 0x2109, NEUTRAL_ON },
	{ 0x2114, 0x0000, NEUTRAL_ON },
	{ 0x2116, 0x2118, NEUTRAL_ON },
	{ 0x211E, 0x2123, NEUTRAL_ON },
	{ 0x2125, 0x0000, NEUTRAL_ON },
	{ 0x2127, 0x0000, NEUTRAL_ON },
	{ 0x2129, 0x0000, NEUTRAL_ON },
	{ 0x212E, 0x0000, WEAK_ET },
	{ 0x2132, 0x0000, NEUTRAL_ON },
	{ 0x213A, 0x0000, NEUTRAL_ON },
	{ 0x2140, 0x2144, NEUTRAL_ON },
	{ 0x214A, 0x215F, NEUTRAL_ON },
	{ 0x2190, 0x2211, NEUTRAL_ON },
	{ 0x2212, 0x2213, WEAK_ET },
	{ 0x2214, 0x2335, NEUTRAL_ON },
	{ 0x237B, 0x2394, NEUTRAL_ON },
	{ 0x2396, 0x244A, NEUTRAL_ON },
	{ 0x2460, 0x249B, WEAK_EN },
	{ 0x24EA, 0x0000, WEAK_EN },
	{ 0x24EB, 0x2FFB, NEUTRAL_ON },
	{ 0x3000, 0x0000, NEUTRAL_WS },
	{ 0x3001, 0x3004, NEUTRAL_ON },
	{ 0x3008, 0x3020, NEUTRAL_ON },
	{ 0x302A, 0x302F, WEAK_NSM },
	{ 0x3030, 0x0000, NEUTRAL_ON },
	{ 0x3036, 0x3037, NEUTRAL_ON },
	{ 0x303D, 0x303F, NEUTRAL_ON },
	{ 0x3099, 0x309A, WEAK_NSM },
	{ 0x309B, 0x309C, NEUTRAL_ON },
	{ 0x30A0, 0x0000, NEUTRAL_ON },
	{ 0x30FB, 0x0000, NEUTRAL_ON },
	{ 0x3251, 0x325F, NEUTRAL_ON },
	{ 0x32B1, 0x32BF, NEUTRAL_ON },
	{ 0xA490, 0xA4C6, NEUTRAL_ON },
	{ 0xFB1D, 0x0000, STRONG_R },
	{ 0xFB1E, 0x0000, WEAK_NSM },
	{ 0xFB1F, 0xFB28, STRONG_R },
	{ 0xFB29, 0x0000, WEAK_ET },
	{ 0xFB2A, 0xFB4F, STRONG_R },
	{ 0xFB50, 0xFD3D, STRONG_AL },
	{ 0xFD3E, 0xFD3F, NEUTRAL_ON },
	{ 0xFD50, 0xFDFC, STRONG_AL },
	{ 0xFE00, 0xFE23, WEAK_NSM },
	{ 0xFE30, 0xFE4F, NEUTRAL_ON },
	{ 0xFE50, 0x0000, WEAK_CS },
	{ 0xFE51, 0x0000, NEUTRAL_ON },
	{ 0xFE52, 0x0000, WEAK_CS },
	{ 0xFE54, 0x0000, NEUTRAL_ON },
	{ 0xFE55, 0x0000, WEAK_CS },
	{ 0xFE56, 0xFE5E, NEUTRAL_ON },
	{ 0xFE5F, 0x0000, WEAK_ET },
	{ 0xFE60, 0xFE61, NEUTRAL_ON },
	{ 0xFE62, 0xFE63, WEAK_ET },
	{ 0xFE64, 0xFE68, NEUTRAL_ON },
	{ 0xFE69, 0xFE6A, WEAK_ET },
	{ 0xFE6B, 0x0000, NEUTRAL_ON },
	{ 0xFE70, 0xFEFC, STRONG_AL },
	{ 0xFEFF, 0x0000, WEAK_BN },
	{ 0xFF01, 0xFF02, NEUTRAL_ON },
	{ 0xFF03, 0xFF05, WEAK_ET },
	{ 0xFF06, 0xFF0A, NEUTRAL_ON },
	{ 0xFF0B, 0x0000, WEAK_ET },
	{ 0xFF0C, 0x0000, WEAK_CS },
	{ 0xFF0D, 0x0000, WEAK_ET },
	{ 0xFF0E, 0x0000, WEAK_CS },
	{ 0xFF0F, 0x0000, WEAK_ES },
	{ 0xFF10, 0xFF19, WEAK_EN },
	{ 0xFF1A, 0x0000, WEAK_CS },
	{ 0xFF1B, 0xFF20, NEUTRAL_ON },
	{ 0xFF3B, 0xFF40, NEUTRAL_ON },
	{ 0xFF5B, 0xFF65, NEUTRAL_ON },
	{ 0xFFE0, 0xFFE1, WEAK_ET },
	{ 0xFFE2, 0xFFE4, NEUTRAL_ON },
	{ 0xFFE5, 0xFFE6, WEAK_ET },
	{ 0xFFE8, 0xFFEE, NEUTRAL_ON },
	{ 0xFFF9, 0xFFFB, WEAK_BN },
	{ 0xFFFC, 0xFFFD, NEUTRAL_ON },
	{ 0x1D167, 0x1D169, WEAK_NSM },
	{ 0x1D173, 0x1D17A, WEAK_BN },
	{ 0x1D17B, 0x1D182, WEAK_NSM },
	{ 0x1D185, 0x1D18B, WEAK_NSM },
	{ 0x1D1AA, 0x1D1AD, WEAK_NSM },
	{ 0x1D7CE, 0x1D7FF, WEAK_EN },
	{ 0xE0001, 0xE007F, WEAK_BN } };
  int i;

  bidi_type_table = Fmake_char_table (Qnil, make_number (STRONG_L));
  staticpro (&bidi_type_table);

  for (i = 0; i < sizeof bidi_type / sizeof bidi_type[0]; i++)
    char_table_set_range (bidi_type_table, bidi_type[i].from,
			  bidi_type[i].to ? bidi_type[i].to : bidi_type[i].from,
			  make_number (bidi_type[i].type));

  fallback_paragraph_start_re =
    XSYMBOL (Fintern_soft (build_string ("paragraph-start"), Qnil))->value;
  if (!STRINGP (fallback_paragraph_start_re))
    fallback_paragraph_start_re = build_string ("\f\\|[ \t]*$");
  staticpro (&fallback_paragraph_start_re);
  Qparagraph_start = intern ("paragraph-start");
  staticpro (&Qparagraph_start);
  fallback_paragraph_separate_re =
    XSYMBOL (Fintern_soft (build_string ("paragraph-separate"), Qnil))->value;
  if (!STRINGP (fallback_paragraph_separate_re))
    fallback_paragraph_separate_re = build_string ("[ \t\f]*$");
  staticpro (&fallback_paragraph_separate_re);
  Qparagraph_separate = intern ("paragraph-separate");
  staticpro (&Qparagraph_separate);
  bidi_initialized = 1;
}

/* Return the bidi type of a character CH, subject to the current
   directional OVERRIDE.  */
bidi_type_t
bidi_get_type (int ch, bidi_dir_t override)
{
  bidi_type_t default_type;

  if (ch == BIDI_EOB)
    return NEUTRAL_B;
  if (ch < 0 || ch > MAX_CHAR)
    abort ();

  default_type = (bidi_type_t) XINT (CHAR_TABLE_REF (bidi_type_table, ch));

  if (override == NEUTRAL_DIR)
    return default_type;

  switch (default_type)
    {
      /* Although UAX#9 does not tell, it doesn't make sense to
	 override NEUTRAL_B and LRM/RLM characters.  */
      case NEUTRAL_B:
      case LRE:
      case LRO:
      case RLE:
      case RLO:
      case PDF:
	return default_type;
      default:
	switch (ch)
	  {
	    case LRM_CHAR:
	    case RLM_CHAR:
	      return default_type;
	    default:
	      if (override == L2R) /* X6 */
		return STRONG_L;
	      else if (override == R2L)
		return STRONG_R;
	      else
		abort ();	/* can't happen: handled above */
	  }
    }
}

void
bidi_check_type (bidi_type_t type)
{
  if (type < UNKNOWN_BT || type > NEUTRAL_ON)
    abort ();
}

/* Given a bidi TYPE of a character, return its category.  */
bidi_category_t
bidi_get_category (bidi_type_t type)
{
  switch (type)
    {
      case UNKNOWN_BT:
	return UNKNOWN_BC;
      case STRONG_L:
      case STRONG_R:
      case STRONG_AL:
      case LRE:
      case LRO:
      case RLE:
      case RLO:
	return STRONG;
      case PDF:		/* ??? really?? */
      case WEAK_EN:
      case WEAK_ES:
      case WEAK_ET:
      case WEAK_AN:
      case WEAK_CS:
      case WEAK_NSM:
      case WEAK_BN:
	return WEAK;
      case NEUTRAL_B:
      case NEUTRAL_S:
      case NEUTRAL_WS:
      case NEUTRAL_ON:
	return NEUTRAL;
      default:
	abort ();
    }
}

/* Return the mirrored character of C, if any.

   Note: The conditions in UAX#9 clause L4 must be tested by the
   caller.  */
/* FIXME: exceedingly temporary!  Should consult the Unicode database
   of character properties.  */
int
bidi_mirror_char (int c)
{
  static const char mirrored_pairs[] = "()<>[]{}";
  const char *p = c > 0 && c < 128 ? strchr (mirrored_pairs, c) : NULL;

  if (p)
    {
      size_t i = p - mirrored_pairs;

      return mirrored_pairs [(i ^ 1)];
    }
  return c;
}

/* Copy the bidi iterator from FROM to TO.  To save cycles, this only
   copies the part of the level stack that is actually in use.  */
static inline void
bidi_copy_it (struct bidi_it *to, struct bidi_it *from)
{
  int i;

  /* Copy everything except the level stack and beyond.  */
  memcpy (to, from, ((size_t)&((struct bidi_it *)0)->level_stack[0]));

  /* Copy the active part of the level stack.  */
  to->level_stack[0] = from->level_stack[0]; /* level zero is always in use */
  for (i = 1; i <= from->stack_idx; i++)
    to->level_stack[i] = from->level_stack[i];
}

/* Caching the bidi iterator states.  */

static struct bidi_it bidi_cache[1000]; /* FIXME: make this dynamically allocated! */
static int bidi_cache_idx;
static int bidi_cache_last_idx;

static inline void
bidi_cache_reset (void)
{
  bidi_cache_idx = 0;
  bidi_cache_last_idx = -1;
}

static inline void
bidi_cache_fetch_state (int idx, struct bidi_it *bidi_it)
{
  int current_scan_dir = bidi_it->scan_dir;

  if (idx < 0 || idx >= bidi_cache_idx)
    abort ();

  bidi_copy_it (bidi_it, &bidi_cache[idx]);
  bidi_it->scan_dir = current_scan_dir;
  bidi_cache_last_idx = idx;
}

/* Find a cached state with a given CHARPOS and resolved embedding
   level less or equal to LEVEL.  if LEVEL is -1, disregard the
   resolved levels in cached states.  DIR, if non-zero, means search
   in that direction from the last cache hit.  */
static inline int
bidi_cache_search (int charpos, int level, int dir)
{
  int i, i_start;

  if (bidi_cache_idx)
    {
      if (charpos < bidi_cache[bidi_cache_last_idx].charpos)
	dir = -1;
      else if (charpos > bidi_cache[bidi_cache_last_idx].charpos)
	dir = 1;
      if (dir)
	i_start = bidi_cache_last_idx;
      else
	{
	  dir = -1;
	  i_start = bidi_cache_idx - 1;
	}

      if (dir < 0)
	{
	  /* Linear search for now; FIXME!  */
	  for (i = i_start; i >= 0; i--)
	    if (bidi_cache[i].charpos == charpos
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
      else
	{
	  for (i = i_start; i < bidi_cache_idx; i++)
	    if (bidi_cache[i].charpos == charpos
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
    }

  return -1;
}

/* Find a cached state where the resolved level changes to a value
   that is lower than LEVEL, and return its cache slot index.  DIR is
   the direction to search, starting with the last used cache slot.
   BEFORE, if non-zero, means return the index of the slot that is
   ``before'' the level change in the search direction.  That is,
   given the cached levels like this:

	 1122333442211
	  AB        C

   and assuming we are at the position cached at the slot marked with
   C, searching backwards (DIR = -1) for LEVEL = 2 will return the
   index of slot B or A, depending whether BEFORE is, respectively,
   non-zero or zero.  */
static int
bidi_cache_find_level_change (int level, int dir, int before)
{
  if (bidi_cache_idx)
    {
      int i = dir ? bidi_cache_last_idx : bidi_cache_idx - 1;
      int incr = before ? 1 : 0;

      if (!dir)
	dir = -1;
      else if (!incr)
	i += dir;

      if (dir < 0)
	{
	  while (i >= incr)
	    {
	      if (bidi_cache[i - incr].resolved_level >= 0
		  && bidi_cache[i - incr].resolved_level < level)
		return i;
	      i--;
	    }
	}
      else
	{
	  while (i < bidi_cache_idx - incr)
	    {
	      if (bidi_cache[i + incr].resolved_level >= 0
		  && bidi_cache[i + incr].resolved_level < level)
		return i;
	      i++;
	    }
	}
    }

  return -1;
}

static inline void
bidi_cache_iterator_state (struct bidi_it *bidi_it, int resolved)
{
  int idx;

  /* We should never cache on backward scans.  */
  if (bidi_it->scan_dir == -1)
    abort ();
  idx = bidi_cache_search (bidi_it->charpos, -1, 1);

  if (idx < 0)
    {
      idx = bidi_cache_idx;
      /* Don't overrun the cache limit.  */
      if (idx > sizeof (bidi_cache) / sizeof (bidi_cache[0]) - 1)
	abort ();
      /* Character positions should correspond to cache positions 1:1.
	 If we are outside the range of cached positions, the cache is
	 useless and must be reset.  */
      if (idx > 0 &&
	  (bidi_it->charpos > bidi_cache[idx - 1].charpos + 1
	   || bidi_it->charpos < bidi_cache[0].charpos))
	{
	  bidi_cache_reset ();
	  idx = 0;
	}
      bidi_copy_it (&bidi_cache[idx], bidi_it);
      if (!resolved)
	bidi_cache[idx].resolved_level = -1;
      bidi_cache[idx].new_paragraph = 0;
    }
  else
    {
      /* Copy only the members which could have changed, to avoid
	 costly copying of the entire struct.  */
      bidi_cache[idx].type = bidi_it->type;
      bidi_check_type (bidi_it->type);
      bidi_cache[idx].type_after_w1 = bidi_it->type_after_w1;
      bidi_check_type (bidi_it->type_after_w1);
      if (resolved)
	bidi_cache[idx].resolved_level = bidi_it->resolved_level;
      else
	bidi_cache[idx].resolved_level = -1;
      bidi_cache[idx].invalid_levels = bidi_it->invalid_levels;
      bidi_cache[idx].invalid_rl_levels = bidi_it->invalid_rl_levels;
      bidi_cache[idx].next_for_neutral = bidi_it->next_for_neutral;
      bidi_cache[idx].next_for_ws = bidi_it->next_for_ws;
      bidi_cache[idx].ignore_bn_limit = bidi_it->ignore_bn_limit;
    }

  bidi_cache_last_idx = idx;
  if (idx >= bidi_cache_idx)
    bidi_cache_idx = idx + 1;
}

static inline bidi_type_t
bidi_cache_find (int charpos, int level, struct bidi_it *bidi_it)
{
  int i = bidi_cache_search (charpos, level, bidi_it->scan_dir);

  if (i >= 0)
    {
      bidi_dir_t current_scan_dir = bidi_it->scan_dir;

      bidi_copy_it (bidi_it, &bidi_cache[i]);
      bidi_cache_last_idx = i;
      /* Don't let scan direction from from the cached state override
	 the current scan direction.  */
      bidi_it->scan_dir = current_scan_dir;
      return bidi_it->type;
    }

  return UNKNOWN_BT;
}

static inline int
bidi_peek_at_next_level (struct bidi_it *bidi_it)
{
  if (bidi_cache_idx == 0 || bidi_cache_last_idx == -1)
    abort ();
  return bidi_cache[bidi_cache_last_idx + bidi_it->scan_dir].resolved_level;
}

/* Check if buffer position CHARPOS/BYTEPOS is the end of a paragraph.
   Value is the non-negative length of the paragraph separator
   following the buffer position, -1 if position is at the beginning
   of a new paragraph, or -2 if position is neither at beginning nor
   at end of a paragraph.  */
EMACS_INT
bidi_at_paragraph_end (EMACS_INT charpos, EMACS_INT bytepos)
{
  Lisp_Object sep_re = Fbuffer_local_value (Qparagraph_separate,
					    Fcurrent_buffer ());
  Lisp_Object start_re = Fbuffer_local_value (Qparagraph_start,
					      Fcurrent_buffer ());
  EMACS_INT val;

  if (!STRINGP (sep_re))
    sep_re = fallback_paragraph_separate_re;
  if (!STRINGP (start_re))
    start_re = fallback_paragraph_start_re;

  val = fast_looking_at (sep_re, charpos, bytepos, ZV, ZV_BYTE, Qnil);
  if (val < 0)
    {
      if (fast_looking_at (start_re, charpos, bytepos, ZV, ZV_BYTE, Qnil) >= 0)
	val = -1;
      else
	val = -2;
    }

  return val;
}

/* Determine the start-of-run (sor) directional type given the two
   embedding levels on either side of the run boundary.  Also, update
   the saved info about previously seen characters, since that info is
   generally valid for a single level run.  */
static inline void
bidi_set_sor_type (struct bidi_it *bidi_it, int level_before, int level_after)
{
  int higher_level = level_before > level_after ? level_before : level_after;

  /* The prev_was_pdf gork is required for when we have several PDFs
     in a row.  In that case, we want to compute the sor type for the
     next level run only once: when we see the first PDF.  That's
     because the sor type depends only on the higher of the two levels
     that we find on the two sides of the level boundary (see UAX#9,
     clause X10), and so we don't need to know the final embedding
     level to which we descend after processing all the PDFs.  */
  if (!bidi_it->prev_was_pdf || level_before < level_after)
    /* FIXME: should the default sor direction be user selectable?  */
    bidi_it->sor = (higher_level & 1) != 0 ? R2L : L2R;
  if (level_before > level_after)
    bidi_it->prev_was_pdf = 1;

  bidi_it->prev.type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.type_after_w1 =
    bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.type = bidi_it->sor == R2L ? STRONG_R : STRONG_L;
  bidi_it->prev_for_neutral.charpos = bidi_it->charpos;
  bidi_it->prev_for_neutral.bytepos = bidi_it->bytepos;
  bidi_it->next_for_neutral.type = bidi_it->next_for_neutral.type_after_w1 =
    bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->ignore_bn_limit = 0; /* meaning it's unknown */
}

static void
bidi_line_init (struct bidi_it *bidi_it)
{
  bidi_it->scan_dir = 1; /* FIXME: do we need to have control on this? */
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
  bidi_it->level_stack[0].override = NEUTRAL_DIR; /* X1 */
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  bidi_it->next_en_pos = -1;
  bidi_it->next_for_ws.type = UNKNOWN_BT;
  bidi_set_sor_type (bidi_it,
		     bidi_it->paragraph_dir == R2L ? 1 : 0,
		     bidi_it->level_stack[0].level); /* X10 */

  bidi_cache_reset ();
}

/* Find the beginning of this paragraph by looking back in the buffer.
   Value is the byte position of the paragraph's beginning.  */
static EMACS_INT
bidi_find_paragraph_start (EMACS_INT pos, EMACS_INT pos_byte)
{
  Lisp_Object re = Fbuffer_local_value (Qparagraph_start, Fcurrent_buffer ());
  EMACS_INT limit = ZV, limit_byte = ZV_BYTE;

  if (!STRINGP (re))
    re = fallback_paragraph_start_re;
  while (pos_byte > BEGV_BYTE
	 && fast_looking_at (re, pos, pos_byte, limit, limit_byte, Qnil) < 0)
    {
      pos = find_next_newline_no_quit (pos - 1, -1);
      pos_byte = CHAR_TO_BYTE (pos);
    }
  return pos_byte;
}

/* Determine the direction, a.k.a. base embedding level, of the
   paragraph we are about to iterate through.  If DIR is either L2R or
   R2L, just use that.  Otherwise, determine the paragraph direction
   from the first strong character of the paragraph.

   Note that this gives the paragraph separator the same direction as
   the preceding paragraph, even though Emacs generally views the
   separartor as not belonging to any paragraph.  */
void
bidi_paragraph_init (bidi_dir_t dir, struct bidi_it *bidi_it)
{
  EMACS_INT bytepos = bidi_it->bytepos;

  /* Special case for an empty buffer. */
  if (bytepos == BEGV_BYTE && bytepos == ZV_BYTE)
    dir = L2R;
  /* We should never be called at EOB or before BEGV.  */
  else if (bytepos >= ZV_BYTE || bytepos < BEGV_BYTE)
    abort ();

  if (dir == L2R)
    {
      bidi_it->paragraph_dir = L2R;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == R2L)
    {
      bidi_it->paragraph_dir = R2L;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == NEUTRAL_DIR)	/* P2 */
    {
      int ch, ch_len;
      EMACS_INT pos;
      bidi_type_t type;
      EMACS_INT sep_len;

      /* If we are inside a paragraph separator, we are just waiting
	 for the separator to be exhausted; use the previous paragraph
	 direction.  But don't do that if we have been just reseated,
	 because we need to reinitialize below in that case.  */
      if (!bidi_it->first_elt
	  && bidi_it->charpos < bidi_it->separator_limit)
	return;

      /* If we are on a newline, get past it to where the next
	 paragraph might start.  But don't do that at BEGV since then
	 we are potentially in a new paragraph that doesn't yet
	 exist.  */
      pos = bidi_it->charpos;
      if (bytepos > BEGV_BYTE && FETCH_CHAR (bytepos) == '\n')
	{
	  bytepos++;
	  pos++;
	}

      /* We are either at the beginning of a paragraph or in the
	 middle of it.  Find where this paragraph starts.  */
      bytepos = bidi_find_paragraph_start (pos, bytepos);

      /* We should always be at the beginning of a new line at this
	 point.  */
      if (!(bytepos == BEGV_BYTE || FETCH_CHAR (bytepos - 1) == '\n'))
	abort ();

      bidi_it->separator_limit = -1;
      bidi_it->new_paragraph = 0;
      ch = FETCH_CHAR (bytepos);
      ch_len = CHAR_BYTES (ch);
      pos = BYTE_TO_CHAR (bytepos);
      type = bidi_get_type (ch, NEUTRAL_DIR);

      for (pos++, bytepos += ch_len;
	   /* NOTE: UAX#9 says to search only for L, AL, or R types of
	      characters, and ignore RLE, RLO, LRE, and LRO.  However,
	      I'm not sure it makes sense to omit those 4; should try
	      with and without that to see the effect.  */
	   (bidi_get_category (type) != STRONG)
	     || (bidi_ignore_explicit_marks_for_paragraph_level
		 && (type == RLE || type == RLO
		     || type == LRE || type == LRO));
	   type = bidi_get_type (ch, NEUTRAL_DIR))
	{
	  if (type == NEUTRAL_B && bidi_at_paragraph_end (pos, bytepos) >= -1)
	    break;
	  if (bytepos >= ZV_BYTE)
	    {
	      /* Pretend there's a paragraph separator at end of buffer.  */
	      type = NEUTRAL_B;
	      break;
	    }
	  FETCH_CHAR_ADVANCE (ch, pos, bytepos);
	}
      if (type == STRONG_R || type == STRONG_AL) /* P3 */
	bidi_it->paragraph_dir = R2L;
      else if (type == STRONG_L)
	bidi_it->paragraph_dir = L2R;
    }
  else
    abort ();

  /* Contrary to UAX#9 clause P3, we only default the paragraph
     direction to L2R if we have no previous usable paragraph
     direction.  */
  if (bidi_it->paragraph_dir == NEUTRAL_DIR)
    bidi_it->paragraph_dir = L2R; /* P3 and ``higher protocols'' */
  if (bidi_it->paragraph_dir == R2L)
    bidi_it->level_stack[0].level = 1;
  else
    bidi_it->level_stack[0].level = 0;

  bidi_line_init (bidi_it);
}

/* Do whatever UAX#9 clause X8 says should be done at paragraph's
   end.  */
static inline void
bidi_set_paragraph_end (struct bidi_it *bidi_it)
{
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  bidi_it->stack_idx = 0;
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
}

/* Initialize the bidi iterator from buffer position CHARPOS.  */
void
bidi_init_it (EMACS_INT charpos, EMACS_INT bytepos, struct bidi_it *bidi_it)
{
  if (! bidi_initialized)
    bidi_initialize ();
  bidi_it->charpos = charpos;
  bidi_it->bytepos = bytepos;
  bidi_it->first_elt = 1;
  bidi_set_paragraph_end (bidi_it);
  bidi_it->new_paragraph = 1;
  bidi_it->separator_limit = -1;
  bidi_it->type = NEUTRAL_B;
  bidi_it->type_after_w1 = UNKNOWN_BT;
  bidi_it->orig_type = UNKNOWN_BT;
  bidi_it->prev_was_pdf = 0;
  bidi_it->prev.type = bidi_it->prev.type_after_w1 = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.type_after_w1 =
    bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->next_for_neutral.charpos = -1;
  bidi_it->next_for_neutral.type =
    bidi_it->next_for_neutral.type_after_w1 =
    bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.charpos = -1;
  bidi_it->prev_for_neutral.type =
    bidi_it->prev_for_neutral.type_after_w1 =
    bidi_it->prev_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->sor = L2R;	 /* FIXME: should it be user-selectable? */
}

/* Push the current embedding level and override status; reset the
   current level to LEVEL and the current override status to OVERRIDE.  */
static inline void
bidi_push_embedding_level (struct bidi_it *bidi_it,
			   int level, bidi_dir_t override)
{
  bidi_it->stack_idx++;
  if (bidi_it->stack_idx >= BIDI_MAXLEVEL)
    abort ();
  bidi_it->level_stack[bidi_it->stack_idx].level = level;
  bidi_it->level_stack[bidi_it->stack_idx].override = override;
}

/* Pop the embedding level and directional override status from the
   stack, and return the new level.  */
static inline int
bidi_pop_embedding_level (struct bidi_it *bidi_it)
{
  /* UAX#9 says to ignore invalid PDFs.  */
  if (bidi_it->stack_idx > 0)
    bidi_it->stack_idx--;
  return bidi_it->level_stack[bidi_it->stack_idx].level;
}

/* Record in SAVED_INFO the information about the current character.  */
static inline void
bidi_remember_char (struct bidi_saved_info *saved_info,
		    struct bidi_it *bidi_it)
{
  saved_info->charpos = bidi_it->charpos;
  saved_info->bytepos = bidi_it->bytepos;
  saved_info->type = bidi_it->type;
  bidi_check_type (bidi_it->type);
  saved_info->type_after_w1 = bidi_it->type_after_w1;
  bidi_check_type (bidi_it->type_after_w1);
  saved_info->orig_type = bidi_it->orig_type;
  bidi_check_type (bidi_it->orig_type);
}

/* Resolve the type of a neutral character according to the type of
   surrounding strong text and the current embedding level.  */
static inline bidi_type_t
bidi_resolve_neutral_1 (bidi_type_t prev_type, bidi_type_t next_type, int lev)
{
  /* N1: European and Arabic numbers are treated as though they were R.  */
  if (next_type == WEAK_EN || next_type == WEAK_AN)
    next_type = STRONG_R;
  if (prev_type == WEAK_EN || prev_type == WEAK_AN)
    prev_type = STRONG_R;

  if (next_type == prev_type)	/* N1 */
    return next_type;
  else if ((lev & 1) == 0)	/* N2 */
    return STRONG_L;
  else
    return STRONG_R;
}

static inline int
bidi_explicit_dir_char (int c)
{
  /* FIXME: this should be replaced with a lookup table with suitable
     bits set, like standard C ctype macros do.  */
  return (c == LRE_CHAR || c == LRO_CHAR
	  || c == RLE_CHAR || c == RLO_CHAR || c == PDF_CHAR);
}

/* A helper function for bidi_resolve_explicit.  It advances to the
   next character in logical order and determines the new embedding
   level and directional override, but does not take into account
   empty embeddings.  */
static int
bidi_resolve_explicit_1 (struct bidi_it *bidi_it)
{
  int curchar;
  bidi_type_t type;
  int current_level;
  int new_level;
  bidi_dir_t override;

  if (bidi_it->bytepos < BEGV_BYTE	/* after reseat to BEGV? */
      || bidi_it->first_elt)
    {
      bidi_it->first_elt = 0;
      if (bidi_it->charpos < BEGV)
	bidi_it->charpos = BEGV;
      bidi_it->bytepos = CHAR_TO_BYTE (bidi_it->charpos);
    }
  else if (bidi_it->bytepos < ZV_BYTE)	/* don't move at ZV */
    {
      bidi_it->charpos++;
      if (bidi_it->ch_len == 0)
	abort ();
      bidi_it->bytepos += bidi_it->ch_len;
    }

  current_level = bidi_it->level_stack[bidi_it->stack_idx].level; /* X1 */
  override = bidi_it->level_stack[bidi_it->stack_idx].override;
  new_level = current_level;

  /* in case it is a unibyte character (not yet implemented) */
  /* _fetch_multibyte_char_len = 1; */
  if (bidi_it->bytepos >= ZV_BYTE)
    {
      curchar = BIDI_EOB;
      bidi_it->ch_len = 1;
    }
  else
    {
      curchar = FETCH_CHAR (bidi_it->bytepos);
      bidi_it->ch_len = CHAR_BYTES (curchar);
    }
  bidi_it->ch = curchar;

  /* Don't apply directional override here, as all the types we handle
     below will not be affected by the override anyway, and we need
     the original type unaltered.  The override will be applied in
     bidi_resolve_weak.  */
  type = bidi_get_type (curchar, NEUTRAL_DIR);
  bidi_it->orig_type = type;
  bidi_check_type (bidi_it->orig_type);

  if (type != PDF)
    bidi_it->prev_was_pdf = 0;

  bidi_it->type_after_w1 = UNKNOWN_BT;

  switch (type)
    {
      case RLE:	/* X2 */
      case RLO:	/* X4 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= 0)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 4)
	      {
		/* Compute the least odd embedding level greater than
		   the current level.  */
		new_level = ((current_level + 1) & ~1) + 1;
		if (bidi_it->type_after_w1 == RLE)
		  override = NEUTRAL_DIR;
		else
		  override = R2L;
		if (current_level == BIDI_MAXLEVEL - 4)
		  bidi_it->invalid_rl_levels = 0;
		bidi_push_embedding_level (bidi_it, new_level, override);
	      }
	    else
	      {
		bidi_it->invalid_levels++;
		/* See the commentary about invalid_rl_levels below.  */
		if (bidi_it->invalid_rl_levels < 0)
		  bidi_it->invalid_rl_levels = 0;
		bidi_it->invalid_rl_levels++;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      case LRE:	/* X3 */
      case LRO:	/* X5 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= 0)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 5)
	      {
		/* Compute the least even embedding level greater than
		   the current level.  */
		new_level = ((current_level + 2) & ~1);
		if (bidi_it->type_after_w1 == LRE)
		  override = NEUTRAL_DIR;
		else
		  override = L2R;
		bidi_push_embedding_level (bidi_it, new_level, override);
	      }
	    else
	      {
		bidi_it->invalid_levels++;
		/* invalid_rl_levels counts invalid levels encountered
		   while the embedding level was already too high for
		   LRE/LRO, but not for RLE/RLO.  That is because
		   there may be exactly one PDF which we should not
		   ignore even though invalid_levels is non-zero.
		   invalid_rl_levels helps to know what PDF is
		   that.  */
		if (bidi_it->invalid_rl_levels >= 0)
		  bidi_it->invalid_rl_levels++;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      case PDF:	/* X7 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= 0)
	  {
	    if (!bidi_it->invalid_rl_levels)
	      {
		new_level = bidi_pop_embedding_level (bidi_it);
		bidi_it->invalid_rl_levels = -1;
		if (bidi_it->invalid_levels)
		  bidi_it->invalid_levels--;
		/* else nothing: UAX#9 says to ignore invalid PDFs */
	      }
	    if (!bidi_it->invalid_levels)
	      new_level = bidi_pop_embedding_level (bidi_it);
	    else
	      {
		bidi_it->invalid_levels--;
		bidi_it->invalid_rl_levels--;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      default:
	/* Nothing.  */
	break;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  return new_level;
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer to the next character (in the logical order), resolve
   any explicit embeddings and directional overrides, and return the
   embedding level of the character after resolving explicit
   directives and ignoring empty embeddings.  */
static int
bidi_resolve_explicit (struct bidi_it *bidi_it)
{
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  int new_level  = bidi_resolve_explicit_1 (bidi_it);

  if (prev_level < new_level
      && bidi_it->type == WEAK_BN
      && bidi_it->ignore_bn_limit == 0 /* only if not already known */
      && bidi_it->ch != BIDI_EOB       /* not already at EOB */
      && bidi_explicit_dir_char (FETCH_CHAR (bidi_it->bytepos
					     + bidi_it->ch_len)))
    {
      /* Avoid pushing and popping embedding levels if the level run
	 is empty, as this breaks level runs where it shouldn't.
	 UAX#9 removes all the explicit embedding and override codes,
	 so empty embeddings disappear without a trace.  We need to
	 behave as if we did the same.  */
      struct bidi_it saved_it;
      int level = prev_level;

      bidi_copy_it (&saved_it, bidi_it);

      while (bidi_explicit_dir_char (FETCH_CHAR (bidi_it->bytepos
						 + bidi_it->ch_len)))
	{
	  level = bidi_resolve_explicit_1 (bidi_it);
	}

      if (level == prev_level)	/* empty embedding */
	saved_it.ignore_bn_limit = bidi_it->charpos + 1;
      else			/* this embedding is non-empty */
	saved_it.ignore_bn_limit = -1;

      bidi_copy_it (bidi_it, &saved_it);
      if (bidi_it->ignore_bn_limit > 0)
	{
	  /* We pushed a level, but we shouldn't have.  Undo that. */
	  if (!bidi_it->invalid_rl_levels)
	    {
	      new_level = bidi_pop_embedding_level (bidi_it);
	      bidi_it->invalid_rl_levels = -1;
	      if (bidi_it->invalid_levels)
		bidi_it->invalid_levels--;
	    }
	  if (!bidi_it->invalid_levels)
	    new_level = bidi_pop_embedding_level (bidi_it);
	  else
	    {
	      bidi_it->invalid_levels--;
	      bidi_it->invalid_rl_levels--;
	    }
	}
    }

  if (bidi_it->type == NEUTRAL_B)	/* X8 */
    {
      bidi_set_paragraph_end (bidi_it);
      /* This is needed by bidi_resolve_weak below, and in L1.  */
      bidi_it->type_after_w1 = bidi_it->type;
      bidi_check_type (bidi_it->type_after_w1);
    }

  return new_level;
}

/* Advance in the buffer, resolve weak types and return the type of
   the next character after weak type resolution.  */
bidi_type_t
bidi_resolve_weak (struct bidi_it *bidi_it)
{
  bidi_type_t type;
  bidi_dir_t override;
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  int new_level  = bidi_resolve_explicit (bidi_it);
  int next_char;
  bidi_type_t type_of_next;
  struct bidi_it saved_it;

  type = bidi_it->type;
  override = bidi_it->level_stack[bidi_it->stack_idx].override;

  if (type == UNKNOWN_BT
      || type == LRE
      || type == LRO
      || type == RLE
      || type == RLO
      || type == PDF)
    abort ();

  if (new_level != prev_level
      || bidi_it->type == NEUTRAL_B)
    {
      /* We've got a new embedding level run, compute the directional
         type of sor and initialize per-run variables (UAX#9, clause
         X10).  */
      bidi_set_sor_type (bidi_it, prev_level, new_level);
    }
  else if (type == NEUTRAL_S || type == NEUTRAL_WS
	   || type == WEAK_BN || type == STRONG_AL)
    bidi_it->type_after_w1 = type;	/* needed in L1 */
  bidi_check_type (bidi_it->type_after_w1);

  /* Level and directional override status are already recorded in
     bidi_it, and do not need any change; see X6.  */
  if (override == R2L)		/* X6 */
    type = STRONG_R;
  else if (override == L2R)
    type = STRONG_L;
  else
    {
      if (type == WEAK_NSM)	/* W1 */
	{
	  /* Note that we don't need to consider the case where the
	     prev character has its type overridden by an RLO or LRO:
	     such characters are outside the current level run, and
	     thus not relevant to this NSM.  Thus, NSM gets the
	     orig_type of the previous character.  */
	  if (bidi_it->prev.type != UNKNOWN_BT)
	    type = bidi_it->prev.orig_type;
	  else if (bidi_it->sor == R2L)
	    type = STRONG_R;
	  else if (bidi_it->sor == L2R)
	    type = STRONG_L;
	  else /* shouldn't happen! */
	    abort ();
	}
      if (type == WEAK_EN	/* W2 */
	  && bidi_it->last_strong.type_after_w1 == STRONG_AL)
	type = WEAK_AN;
      else if (type == STRONG_AL) /* W3 */
	type = STRONG_R;
      else if ((type == WEAK_ES	/* W4 */
		&& bidi_it->prev.type_after_w1 == WEAK_EN
		&& bidi_it->prev.orig_type == WEAK_EN)
	       || (type == WEAK_CS
		   && ((bidi_it->prev.type_after_w1 == WEAK_EN
			&& bidi_it->prev.orig_type == WEAK_EN)
		       || bidi_it->prev.type_after_w1 == WEAK_AN)))
	{
	  next_char =
	    bidi_it->bytepos + bidi_it->ch_len >= ZV_BYTE
	    ? BIDI_EOB : FETCH_CHAR (bidi_it->bytepos + bidi_it->ch_len);
	  type_of_next = bidi_get_type (next_char, override);

	  if (type_of_next == WEAK_BN
	      || bidi_explicit_dir_char (next_char))
	    {
	      bidi_copy_it (&saved_it, bidi_it);
	      while (bidi_resolve_explicit (bidi_it) == new_level
		     && bidi_it->type == WEAK_BN)
		;
	      type_of_next = bidi_it->type;
	      bidi_copy_it (bidi_it, &saved_it);
	    }

	  /* If the next character is EN, but the last strong-type
	     character is AL, that next EN will be changed to AN when
	     we process it in W2 above.  So in that case, this ES
	     should not be changed into EN.  */
	  if (type == WEAK_ES
	      && type_of_next == WEAK_EN
	      && bidi_it->last_strong.type_after_w1 != STRONG_AL)
	    type = WEAK_EN;
	  else if (type == WEAK_CS)
	    {
	      if (bidi_it->prev.type_after_w1 == WEAK_AN
		  && (type_of_next == WEAK_AN
		      /* If the next character is EN, but the last
			 strong-type character is AL, EN will be later
			 changed to AN when we process it in W2 above.
			 So in that case, this ES should not be
			 changed into EN.  */
		      || (type_of_next == WEAK_EN
			  && bidi_it->last_strong.type_after_w1 == STRONG_AL)))
		type = WEAK_AN;
	      else if (bidi_it->prev.type_after_w1 == WEAK_EN
		       && type_of_next == WEAK_EN
		       && bidi_it->last_strong.type_after_w1 != STRONG_AL)
		type = WEAK_EN;
	    }
	}
      else if (type == WEAK_ET	/* W5: ET with EN before or after it */
	       || type == WEAK_BN)	/* W5/Retaining */
	{
	  if (bidi_it->prev.type_after_w1 == WEAK_EN /* ET/BN w/EN before it */
	      || bidi_it->next_en_pos > bidi_it->charpos)
	    type = WEAK_EN;
	  else			/* W5: ET/BN with EN after it.  */
	    {
	      EMACS_INT en_pos = bidi_it->charpos + 1;

	      next_char =
		bidi_it->bytepos + bidi_it->ch_len >= ZV_BYTE
		? BIDI_EOB : FETCH_CHAR (bidi_it->bytepos + bidi_it->ch_len);
	      type_of_next = bidi_get_type (next_char, override);

	      if (type_of_next == WEAK_ET
		  || type_of_next == WEAK_BN
		  || bidi_explicit_dir_char (next_char))
		{
		  bidi_copy_it (&saved_it, bidi_it);
		  while (bidi_resolve_explicit (bidi_it) == new_level
			 && (bidi_it->type == WEAK_BN
			     || bidi_it->type == WEAK_ET))
		    ;
		  type_of_next = bidi_it->type;
		  en_pos = bidi_it->charpos;
		  bidi_copy_it (bidi_it, &saved_it);
		}
	      if (type_of_next == WEAK_EN)
		{
		  /* If the last strong character is AL, the EN we've
		     found will become AN when we get to it (W2). */
		  if (bidi_it->last_strong.type_after_w1 != STRONG_AL)
		    {
		      type = WEAK_EN;
		      /* Remember this EN position, to speed up processing
			 of the next ETs.  */
		      bidi_it->next_en_pos = en_pos;
		    }
		  else if (type == WEAK_BN)
		    type = NEUTRAL_ON; /* W6/Retaining */
		}
	    }
	}
    }

  if (type == WEAK_ES || type == WEAK_ET || type == WEAK_CS /* W6 */
      || (type == WEAK_BN
	  && (bidi_it->prev.type_after_w1 == WEAK_CS	    /* W6/Retaining */
	      || bidi_it->prev.type_after_w1 == WEAK_ES
	      || bidi_it->prev.type_after_w1 == WEAK_ET)))
    type = NEUTRAL_ON;

  /* Store the type we've got so far, before we clobber it with strong
     types in W7 and while resolving neutral types.  But leave alone
     the original types that were recorded above, because we will need
     them for the L1 clause.  */
  if (bidi_it->type_after_w1 == UNKNOWN_BT)
    bidi_it->type_after_w1 = type;
  bidi_check_type (bidi_it->type_after_w1);

  if (type == WEAK_EN)	/* W7 */
    {
      if ((bidi_it->last_strong.type_after_w1 == STRONG_L)
	  || (bidi_it->last_strong.type == UNKNOWN_BT && bidi_it->sor == L2R))
	type = STRONG_L;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);
  return type;
}

bidi_type_t
bidi_resolve_neutral (struct bidi_it *bidi_it)
{
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  bidi_type_t type = bidi_resolve_weak (bidi_it);
  int current_level = bidi_it->level_stack[bidi_it->stack_idx].level;

  if (!(type == STRONG_R
	|| type == STRONG_L
	|| type == WEAK_BN
	|| type == WEAK_EN
	|| type == WEAK_AN
	|| type == NEUTRAL_B
	|| type == NEUTRAL_S
	|| type == NEUTRAL_WS
	|| type == NEUTRAL_ON))
    abort ();

  if (bidi_get_category (type) == NEUTRAL
      || (type == WEAK_BN && prev_level == current_level))
    {
      if (bidi_it->next_for_neutral.type != UNKNOWN_BT)
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       bidi_it->next_for_neutral.type,
				       current_level);
      else
	{
	  /* Arrrgh!!  The UAX#9 algorithm is too deeply entrenched in
	     the assumption of batch-style processing; see clauses W4,
	     W5, and especially N1, which require to look far forward
	     (as well as back) in the buffer.  May the fleas of a
	     thousand camels infest the armpits of those who design
	     supposedly general-purpose algorithms by looking at their
	     own implementations, and fail to consider other possible
	     implementations!  */
	  struct bidi_it saved_it;
	  bidi_type_t next_type;

	  if (bidi_it->scan_dir == -1)
	    abort ();

	  bidi_copy_it (&saved_it, bidi_it);
	  /* Scan the text forward until we find the first non-neutral
	     character, and then use that to resolve the neutral we
	     are dealing with now.  We also cache the scanned iterator
	     states, to salvage some of the effort later.  */
	  bidi_cache_iterator_state (bidi_it, 0);
	  do {
	    /* Record the info about the previous character, so that
	       it will be cached below with this state.  */
	    if (bidi_it->type_after_w1 != WEAK_BN /* W1/Retaining */
		&& bidi_it->type != WEAK_BN)
	      bidi_remember_char (&bidi_it->prev, bidi_it);
	    type = bidi_resolve_weak (bidi_it);
	    /* Paragraph separators have their levels fully resolved
	       at this point, so cache them as resolved.  */
	    bidi_cache_iterator_state (bidi_it, type == NEUTRAL_B);
	    /* FIXME: implement L1 here, by testing for a newline and
	       resetting the level for any sequence of whitespace
	       characters adjacent to it.  */
	  } while (!(type == NEUTRAL_B
		     || (type != WEAK_BN
			 && bidi_get_category (type) != NEUTRAL)
		     /* This is all per level run, so stop when we
			reach the end of this level run.  */
		     || bidi_it->level_stack[bidi_it->stack_idx].level !=
		     current_level));

	  bidi_remember_char (&saved_it.next_for_neutral, bidi_it);

	  switch (type)
	    {
	      case STRONG_L:
	      case STRONG_R:
	      case STRONG_AL:
		next_type = type;
		break;
	      case WEAK_EN:
	      case WEAK_AN:
		/* N1: ``European and Arabic numbers are treated as
		   though they were R.''  */
		next_type = STRONG_R;
		saved_it.next_for_neutral.type = STRONG_R;
		break;
	      case WEAK_BN:
		if (!bidi_explicit_dir_char (bidi_it->ch))
		  abort ();		/* can't happen: BNs are skipped */
		/* FALLTHROUGH */
	      case NEUTRAL_B:
		/* Marched all the way to the end of this level run.
		   We need to use the eor type, whose information is
		   stored by bidi_set_sor_type in the prev_for_neutral
		   member.  */
		if (saved_it.type != WEAK_BN
		    || bidi_get_category (bidi_it->prev.type_after_w1) == NEUTRAL)
		  {
		    next_type = bidi_it->prev_for_neutral.type;
		    saved_it.next_for_neutral.type = next_type;
		    bidi_check_type (next_type);
		  }
		else
		  {
		    /* This is a BN which does not adjoin neutrals.
		       Leave its type alone.  */
		    bidi_copy_it (bidi_it, &saved_it);
		    return bidi_it->type;
		  }
		break;
	      default:
		abort ();
	    }
	  type = bidi_resolve_neutral_1 (saved_it.prev_for_neutral.type,
					 next_type, current_level);
	  saved_it.type = type;
	  bidi_check_type (type);
	  bidi_copy_it (bidi_it, &saved_it);
	}
    }
  return type;
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer to the next character (in the logical order), resolve
   the bidi type of that next character, and return that type.  */
bidi_type_t
bidi_type_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type;

  /* This should always be called during a forward scan.  */
  if (bidi_it->scan_dir != 1)
    abort ();

  /* Reset the limit until which to ignore BNs if we step out of the
     area where we found only empty levels.  */
  if ((bidi_it->ignore_bn_limit > 0
       && bidi_it->ignore_bn_limit <= bidi_it->charpos)
      || (bidi_it->ignore_bn_limit == -1
	  && !bidi_explicit_dir_char (bidi_it->ch)))
    bidi_it->ignore_bn_limit = 0;

  type = bidi_resolve_neutral (bidi_it);

  return type;
}

/* Given an iterator state BIDI_IT, advance one character position in
   the buffer to the next character (in the logical order), resolve
   the embedding and implicit levels of that next character, and
   return the resulting level.  */
int
bidi_level_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type;
  int level, prev_level = -1;
  struct bidi_saved_info next_for_neutral;

  if (bidi_it->scan_dir == 1)
    {
      /* There's no sense in trying to advance if we hit end of text.  */
      if (bidi_it->ch == BIDI_EOB)
	return bidi_it->resolved_level;

      /* Record the info about the previous character.  */
      if (bidi_it->type_after_w1 != WEAK_BN /* W1/Retaining */
	  && bidi_it->type != WEAK_BN)
	bidi_remember_char (&bidi_it->prev, bidi_it);
      if (bidi_it->type_after_w1 == STRONG_R
	  || bidi_it->type_after_w1 == STRONG_L
	  || bidi_it->type_after_w1 == STRONG_AL)
	bidi_remember_char (&bidi_it->last_strong, bidi_it);
      /* FIXME: it sounds like we don't need both prev and
	 prev_for_neutral members, but I'm leaving them both for now.  */
      if (bidi_it->type == STRONG_R || bidi_it->type == STRONG_L
	  || bidi_it->type == WEAK_EN || bidi_it->type == WEAK_AN)
	bidi_remember_char (&bidi_it->prev_for_neutral, bidi_it);

      /* If we overstepped the characters used for resolving neutrals
	 and whitespace, invalidate their info in the iterator.  */
      if (bidi_it->charpos >= bidi_it->next_for_neutral.charpos)
	bidi_it->next_for_neutral.type = UNKNOWN_BT;
      if (bidi_it->next_en_pos >= 0
	  && bidi_it->charpos >= bidi_it->next_en_pos)
	bidi_it->next_en_pos = -1;
      if (bidi_it->next_for_ws.type != UNKNOWN_BT
	  && bidi_it->charpos >= bidi_it->next_for_ws.charpos)
	bidi_it->next_for_ws.type = UNKNOWN_BT;

      /* This must be taken before we fill the iterator with the info
	 about the next char.  If we scan backwards, the iterator
	 state must be already cached, so there's no need to know the
	 embedding level of the previous character, since we will be
	 returning to our caller shortly.  */
      prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
    }
  next_for_neutral = bidi_it->next_for_neutral;

  /* Perhaps it is already cached.  */
  type = bidi_cache_find (bidi_it->charpos + bidi_it->scan_dir, -1, bidi_it);
  if (type != UNKNOWN_BT)
    {
      /* Don't lose the information for resolving neutrals!  The
	 cached states could have been cached before their
	 next_for_neutral member was computed.  If we are on our way
	 forward, we can simply take the info from the previous
	 state.  */
      if (bidi_it->scan_dir == 1
	  && bidi_it->next_for_neutral.type == UNKNOWN_BT)
	bidi_it->next_for_neutral = next_for_neutral;

      /* If resolved_level is -1, it means this state was cached
	 before it was completely resolved, so we cannot return
	 it.  */
      if (bidi_it->resolved_level != -1)
	return bidi_it->resolved_level;
    }
  if (bidi_it->scan_dir == -1)
    /* If we are going backwards, the iterator state is already cached
       from previous scans, and should be fully resolved.  */
    abort ();

  if (type == UNKNOWN_BT)
    type = bidi_type_of_next_char (bidi_it);

  if (type == NEUTRAL_B)
    return bidi_it->resolved_level;

  level = bidi_it->level_stack[bidi_it->stack_idx].level;
  if ((bidi_get_category (type) == NEUTRAL /* && type != NEUTRAL_B */)
      || (type == WEAK_BN && prev_level == level))
    {
      if (bidi_it->next_for_neutral.type == UNKNOWN_BT)
	abort ();

      /* If the cached state shows a neutral character, it was not
	 resolved by bidi_resolve_neutral, so do it now.  */
      type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				     bidi_it->next_for_neutral.type,
				     level);
    }

  if (!(type == STRONG_R
	|| type == STRONG_L
	|| type == WEAK_BN
	|| type == WEAK_EN
	|| type == WEAK_AN))
    abort ();
  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  /* For L1 below, we need to know, for each WS character, whether
     it belongs to a sequence of WS characters preceeding a newline
     or a TAB or a paragraph separator.  */
  if (bidi_it->orig_type == NEUTRAL_WS
      && bidi_it->next_for_ws.type == UNKNOWN_BT)
    {
      int ch;
      int clen = bidi_it->ch_len;
      EMACS_INT bpos = bidi_it->bytepos;
      EMACS_INT cpos = bidi_it->charpos;
      bidi_type_t chtype;

      do {
	/*_fetch_multibyte_char_len = 1;*/
	ch = bpos + clen >= ZV_BYTE ? BIDI_EOB : FETCH_CHAR (bpos + clen);
	bpos += clen;
	cpos++;
	clen = (ch == BIDI_EOB ? 1 : CHAR_BYTES (ch));
	if (ch == '\n' || ch == BIDI_EOB /* || ch == LINESEP_CHAR */)
	  chtype = NEUTRAL_B;
	else
	  chtype = bidi_get_type (ch, NEUTRAL_DIR);
      } while (chtype == NEUTRAL_WS || chtype == WEAK_BN
	       || bidi_explicit_dir_char (ch)); /* L1/Retaining */
      bidi_it->next_for_ws.type = chtype;
      bidi_check_type (bidi_it->next_for_ws.type);
      bidi_it->next_for_ws.charpos = cpos;
      bidi_it->next_for_ws.bytepos = bpos;
    }

  /* Resolve implicit levels, with a twist: PDFs get the embedding
     level of the enbedding they terminate.  See below for the
     reason.  */
  if (bidi_it->orig_type == PDF
      /* Don't do this if this formatting code didn't change the
	 embedding level due to invalid or empty embeddings.  */
      && prev_level != level)
    {
      /* Don't look in UAX#9 for the reason for this: it's our own
	 private quirk.  The reason is that we want the formatting
	 codes to be delivered so that they bracket the text of their
	 embedding.  For example, given the text

	     {RLO}teST{PDF}

	 we want it to be displayed as

	     {RLO}STet{PDF}

	 not as

	     STet{RLO}{PDF}

	 which will result because we bump up the embedding level as
	 soon as we see the RLO and pop it as soon as we see the PDF,
	 so RLO itself has the same embedding level as "teST", and
	 thus would be normally delivered last, just before the PDF.
	 The switch below fiddles with the level of PDF so that this
	 ugly side effect does not happen.

	 (This is, of course, only important if the formatting codes
	 are actually displayed, but Emacs does need to display them
	 if the user wants to.)  */
      level = prev_level;
    }
  else if (bidi_it->orig_type == NEUTRAL_B /* L1 */
	   || bidi_it->orig_type == NEUTRAL_S
	   || bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB
	   /* || bidi_it->ch == LINESEP_CHAR */
	   || (bidi_it->orig_type == NEUTRAL_WS
	       && (bidi_it->next_for_ws.type == NEUTRAL_B
		   || bidi_it->next_for_ws.type == NEUTRAL_S)))
    level = bidi_it->level_stack[0].level;
  else if ((level & 1) == 0) /* I1 */
    {
      if (type == STRONG_R)
	level++;
      else if (type == WEAK_EN || type == WEAK_AN)
	level += 2;
    }
  else			/* I2 */
    {
      if (type == STRONG_L || type == WEAK_EN || type == WEAK_AN)
	level++;
    }

  bidi_it->resolved_level = level;
  return level;
}

/* Move to the other edge of a level given by LEVEL.  If END_FLAG is
   non-zero, we are at the end of a level, and we need to prepare to
   resume the scan of the lower level.

   If this level's other edge is cached, we simply jump to it, filling
   the iterator structure with the iterator state on the other edge.
   Otherwise, we walk the buffer until we come back to the same level
   as LEVEL.

   Note: we are not talking here about a ``level run'' in the UAX#9
   sense of the term, but rather about a ``level'' which includes
   all the levels higher than it.  In other words, given the levels
   like this:

         11111112222222333333334443343222222111111112223322111
                A      B                    C

   and assuming we are at point A scanning left to right, this
   function moves to point C, whereas the UAX#9 ``level 2 run'' ends
   at point B.  */
static void
bidi_find_other_level_edge (struct bidi_it *bidi_it, int level, int end_flag)
{
  int dir = end_flag ? -bidi_it->scan_dir : bidi_it->scan_dir;
  int idx;

  /* Try the cache first.  */
  if ((idx = bidi_cache_find_level_change (level, dir, end_flag)) >= 0)
    bidi_cache_fetch_state (idx, bidi_it);
  else
    {
      int new_level;

      if (end_flag)
	abort (); /* if we are at end of level, its edges must be cached */

      bidi_cache_iterator_state (bidi_it, 1);
      do {
	new_level = bidi_level_of_next_char (bidi_it);
	bidi_cache_iterator_state (bidi_it, 1);
      } while (new_level >= level);
    }
}

void
bidi_get_next_char_visually (struct bidi_it *bidi_it)
{
  int old_level, new_level, next_level;
  struct bidi_it sentinel;

  if (bidi_it->scan_dir == 0)
    {
      bidi_it->scan_dir = 1;	/* default to logical order */
    }

  /* If we just passed a newline, initialize for the next line.  */
  if (!bidi_it->first_elt && bidi_it->orig_type == NEUTRAL_B)
    bidi_line_init (bidi_it);

  /* Prepare the sentinel iterator state.  */
  if (bidi_cache_idx == 0)
    {
      bidi_copy_it (&sentinel, bidi_it);
      if (bidi_it->first_elt)
	{
	  sentinel.charpos--;	/* cached charpos needs to be monotonic */
	  sentinel.bytepos--;
	  sentinel.ch = '\n';	/* doesn't matter, but why not? */
	  sentinel.ch_len = 1;
	}
    }

  old_level = bidi_it->resolved_level;
  new_level = bidi_level_of_next_char (bidi_it);

  /* Reordering of resolved levels (clause L2) is implemented by
     jumping to the other edge of the level and flipping direction of
     scanning the text whenever we find a level change.  */
  if (new_level != old_level)
    {
      int ascending = new_level > old_level;
      int level_to_search = ascending ? old_level + 1 : old_level;
      int incr = ascending ? 1 : -1;
      int expected_next_level = old_level + incr;

      /* If we don't have anything cached yet, we need to cache the
	 sentinel state, since we'll need it to record where to jump
	 when the last non-base level is exhausted.  */
      if (bidi_cache_idx == 0)
	bidi_cache_iterator_state (&sentinel, 1);
      /* Jump (or walk) to the other edge of this level.  */
      bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
      /* Switch scan direction and peek at the next character in the
	 new direction.  */
      bidi_it->scan_dir = -bidi_it->scan_dir;

      /* The following loop handles the case where the resolved level
	 jumps by more than one.  This is typical for numbers inside a
	 run of text with left-to-right embedding direction, but can
	 also happen in other situations.  In those cases the decision
	 where to continue after a level change, and in what direction,
	 is tricky.  For example, given a text like below:

	          abcdefgh
	          11336622

	 (where the numbers below the text show the resolved levels),
	 the result of reordering according to UAX#9 should be this:

		  efdcghba

	 This is implemented by the loop below which flips direction
	 and jumps to the other edge of the level each time it finds
	 the new level not to be the expected one.  The expected level
	 is always one more or one less than the previous one.  */
      next_level = bidi_peek_at_next_level (bidi_it);
      while (next_level != expected_next_level)
	{
	  expected_next_level += incr;
	  level_to_search += incr;
	  bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
	  bidi_it->scan_dir = -bidi_it->scan_dir;
	  next_level = bidi_peek_at_next_level (bidi_it);
	}

      /* Finally, deliver the next character in the new direction.  */
      next_level = bidi_level_of_next_char (bidi_it);
    }

  /* Take note when we have just processed the newline that precedes
     the end of the paragraph.  The next time we are about to be
     called, set_iterator_to_next will automatically reinit the
     paragraph direction, if needed.  We do this at the newline before
     the paragraph separator, because the next character might not be
     the first character of the next paragraph, due to the bidi
     reordering, whereas we _must_ know the paragraph base direction
     _before_ we process the paragraph's text, since the base
     direction affects the reordering.  */
  if (bidi_it->scan_dir == 1
      && bidi_it->orig_type == NEUTRAL_B
      && bidi_it->bytepos < ZV_BYTE)
    {
      EMACS_INT sep_len =
	bidi_at_paragraph_end (bidi_it->charpos + 1,
			       bidi_it->bytepos + bidi_it->ch_len);
      if (sep_len >= 0)
	{
	  bidi_it->new_paragraph = 1;
	  /* Record the buffer position of the last character of the
	     paragraph separator.  */
	  bidi_it->separator_limit = bidi_it->charpos + 1 + sep_len;
	}
    }

  if (bidi_it->scan_dir == 1 && bidi_cache_idx)
    {
      /* If we are at paragraph's base embedding level and beyond the
	 last cached position, the cache's job is done and we can
	 discard it.  */
      if (bidi_it->resolved_level == bidi_it->level_stack[0].level
	  && bidi_it->charpos > bidi_cache[bidi_cache_idx - 1].charpos)
	bidi_cache_reset ();
	/* But as long as we are caching during forward scan, we must
	   cache each state, or else the cache integrity will be
	   compromised: it assumes cached states correspond to buffer
	   positions 1:1.  */
      else
	bidi_cache_iterator_state (bidi_it, 1);
    }
}

/* This is meant to be called from within the debugger, whenever you
   wish to examine the cache contents.  */
void
bidi_dump_cached_states (void)
{
  int i;
  int ndigits = 1;

  if (bidi_cache_idx == 0)
    {
      fprintf (stderr, "The cache is empty.\n");
      return;
    }
  fprintf (stderr, "Total of %d state%s in cache:\n",
	   bidi_cache_idx, bidi_cache_idx == 1 ? "" : "s");

  for (i = bidi_cache[bidi_cache_idx - 1].charpos; i > 0; i /= 10)
    ndigits++;
  fputs ("ch  ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*c", ndigits, bidi_cache[i].ch);
  fputs ("\n", stderr);
  fputs ("lvl ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*d", ndigits, bidi_cache[i].resolved_level);
  fputs ("\n", stderr);
  fputs ("pos ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*d", ndigits, bidi_cache[i].charpos);
  fputs ("\n", stderr);
}
