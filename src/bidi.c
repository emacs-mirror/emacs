/* A sequential implementation of the Unicode Bidirectional algorithm,
   as per UAX#9.

   Unlike the reference and most other implementations, this one is
   designed to be called once for every character in the buffer.

   The main entry point is bidi_get_next_char_visually.  Each time it
   is called, it finds the next character in the visual order, and
   returns its information in a special structure.  The caller is then
   expected to process this character for display or any other
   purposes, and call bidi_get_next_char_visually for the next
   character.

   A note about references to UAX#9 rules: if the reference says
   something like "X9/Retaining", it means that you need to refer to
   rule X9 and to its modifications decribed in the "Implementation
   Notes" section of UAX#9, under "Retaining Format Codes".  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "dispextern.h"

static int bidi_initialized = 0;

static Lisp_Object bidi_type_table;

#define LRM_CHAR   0x200E
#define RLM_CHAR   0x200F
#define LRE_CHAR   0x202A
#define RLE_CHAR   0x202B
#define PDF_CHAR   0x202C
#define LRO_CHAR   0x202D
#define RLO_CHAR   0x202E

#define CHARSET_HEBREW   0x88
#define CHARSET_ARABIC	 0x87
#define CHARSET_SYRIAC	 -1	/* these are undefined yet, -1 is invalid */
#define CHARSET_THAANA	 -1

/* FIXME: need to define wrappers for FETCH_CHAR etc. that return
   BIDI_EOB when they hit ZV.  */
#define BIDI_EOB   -1
#define BIDI_BOB   -2

#ifdef TEST_STANDALONE
/* Testing.  */

#include <stdio.h>

static unsigned char *input_buf;
static size_t input_buf_size;

int _fetch_multibyte_char_len, _c_c_;

#undef FETCH_CHAR_ADVANCE
#define FETCH_CHAR_ADVANCE(ch, cp, bp) \
  do {				       \
    ch = input_buf[cp];		       \
    (cp)++;			       \
    (bp)++;			       \
    if (ch == '\0')		       \
      ch = BIDI_EOB;			       \
  } while (0)

#undef FETCH_CHAR
#define FETCH_CHAR(n)		((_c_c_ = input_buf[n]) ? _c_c_ : BIDI_EOB)

#undef CHAR_CHARSET
#define CHAR_CHARSET(c) 					 \
  (((c) >= 128 || ((c) < 8 && (c)) || ((c) >= 'A' && (c) < 'X')) \
   ? CHARSET_HEBREW						 \
   : ((((c) >= 'X' && (c) <= 'Z') || ((c) >= '6' && (c) <= '9')) \
      ? CHARSET_ARABIC						 \
      : CHARSET_ASCII))

#undef CHAR_TO_BYTE
#define CHAR_TO_BYTE(pos) (pos)

#define char_bytes(ch) 1

#undef LRE_CHAR
#undef LRO_CHAR
#undef RLE_CHAR
#undef RLO_CHAR
#undef PDF_CHAR
#undef RLM_CHAR
#undef LRM_CHAR

#define LRE_CHAR   1
#define LRO_CHAR   2
#define RLE_CHAR   3
#define RLO_CHAR   4
#define PDF_CHAR   5
#define RLM_CHAR   6
#define LRM_CHAR   7

static const char *bidi_name[] =
  {
    "[???]", "[LRE]", "[LRO]", "[RLE]", "[RLO]", "[PDF]", "[RLM]", "[LRM]"
  };

#endif	/* TEST_STANDALONE */

/* Data structures.  */

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

bidi_dir_t bidi_overriding_paragraph_direction = NEUTRAL_DIR;

#define ASCII_BIDI_TYPE_SET(STR, TYPE)			\
  do {							\
    unsigned char *p;					\
    for (p = (STR); *p; p++)				\
      CHAR_TABLE_SET (bidi_type_table, *p, (TYPE));	\
  } while (0)

static void
bidi_initialize ()
{
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
	{ 0x002B, 0x0000, WEAK_ET },
	{ 0x002C, 0x0000, WEAK_CS },
	{ 0x002D, 0x0000, WEAK_ET },
	{ 0x002E, 0x0000, WEAK_CS },
	{ 0x002F, 0x0000, WEAK_ES },
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
	{ 0x00AB, 0x00AF, NEUTRAL_ON },
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
	{ 0x05C4, 0x0000, WEAK_NSM },
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

  for (i = 0; i < sizeof bidi_type / sizeof bidi_type[0]; i++)
    char_table_set_range (bidi_type_table, bidi_type[i].from, bidi_type[i].to,
			  make_number (bidi_type[i].type));
  bidi_initialized = 1;
}

static int
bidi_is_arabic_number (int ch)
{
#ifdef TEST_STANDALONE
  return ch >= '6' && ch <= '9';
#else
  return 0;	/* FIXME! */
#endif
}

/* Return the bidi type of a character CH.  */
bidi_type_t
bidi_get_type (int ch)
{
  return (bidi_type_t) XINT (CHAR_TABLE_REF (bidi_type_table, ch));
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

/* FIXME: exceedingly temporary!  Should consult some TBD data base of
   character properties.  */
int
bidi_mirror_char (int c)
{
  static const char mirrored_pairs[] = "()<>[]{}";
  const char *p = strchr (mirrored_pairs, c);

  if (p)
    {
      size_t i = p - mirrored_pairs;

      if ((i & 1) == 0)
	return mirrored_pairs[i + 1];
      else
	return mirrored_pairs[i - 1];
    }
  return c;
}

/* Copy the bidi iterator from FROM to TO.  To save cycles, this only
   copies the part of the level stack that is actually in use.  */
static inline void
bidi_copy_it (struct bidi_it *to, struct bidi_it *from)
{
  int i;

  /* Copy everything except the level stack.  */
  memcpy (to, from, ((int)&((struct bidi_it *)0)->level_stack[0]));

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
      if (idx > sizeof (bidi_cache) / sizeof (bidi_cache[0]) - 1)
	abort ();
      bidi_copy_it (&bidi_cache[idx], bidi_it);
      if (!resolved)
	bidi_cache[idx].resolved_level = -1;
    }
  else
    {
      /* Copy only the members which could have changed, to avoid
	 costly copying of the entire struct.  */
      bidi_cache[idx].type = bidi_it->type;
      bidi_cache[idx].orig_type = bidi_it->orig_type;
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

      *bidi_it = bidi_cache[i];
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

/* Return non-zero if buffer's byte position POS is the last character
   of a paragraph.  THIS_CH is the character preceding the one at POS in
   the buffer.  */
int
bidi_at_paragraph_end (int this_ch, int pos)
{
  int next_ch = FETCH_CHAR (pos);

  return (this_ch == '\n' && next_ch == '\n') || this_ch == BIDI_EOB;
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
  if (level_before < level_after || !bidi_it->prev_was_pdf)
    /* FIXME: should the default sor direction be user selectable?  */
    bidi_it->sor = (higher_level & 1) != 0 ? R2L : L2R;
  if (level_before > level_after)
    bidi_it->prev_was_pdf = 1;

  bidi_it->prev.type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.orig_type =
    bidi_it->last_strong.pristine_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.type = bidi_it->sor == R2L ? STRONG_R : STRONG_L;
  bidi_it->prev_for_neutral.charpos = bidi_it->charpos;
  bidi_it->prev_for_neutral.bytepos = bidi_it->bytepos;
  bidi_it->next_for_neutral.type = bidi_it->next_for_neutral.orig_type =
    bidi_it->next_for_neutral.pristine_type = UNKNOWN_BT;
  bidi_it->ignore_bn_limit = 0; /* meaning it's unknown */
}

void
bidi_paragraph_init (bidi_dir_t dir, struct bidi_it *bidi_it)
{
  bidi_it->level_stack[0].level = 0;
  if (dir == R2L)
    bidi_it->level_stack[0].level = 1;
  else if (dir == NEUTRAL_DIR)	/* P2 */
    {
      bidi_type_t type;
      int pos = bidi_it->charpos, bytepos = bidi_it->bytepos;
      int ch;

      if (pos < 0)
	pos = bytepos = 0;
      else if (bidi_it->ch != BIDI_EOB)
	{
	  pos++;
	  bytepos += bidi_it->ch_len;
	}

      ch = FETCH_CHAR (bytepos);
      pos++;
      bytepos += CHAR_BYTES (ch);

      /* FIXME: should actually go to where the paragraph begins and
	 start the loop below from there, since UAX#9 says to find the
	 first strong directional character in the paragraph.  */

      for (type = bidi_get_type (ch);
	   /* NOTE: UAX#9 says to search only for L, AL, or R types of
	      characters, and ignore RLE, RLO, LRE, and LRO.  However,
	      I'm not sure it makes sense to omit those 4; should try
	      with and without that to see the effect.  */
	   (bidi_get_category (type) != STRONG)
	     || (bidi_ignore_explicit_marks_for_paragraph_level
		 && (type == RLE || type == RLO
		     || type == LRE || type == LRO));
	   type = bidi_get_type (ch))
	{
	  if (type == NEUTRAL_B || bidi_at_paragraph_end (ch, bytepos))
	    break;
	  FETCH_CHAR_ADVANCE (ch, pos, bytepos);
	}
      if (type == STRONG_R || type == STRONG_AL) /* P3 */
	bidi_it->level_stack[0].level = 1;
    }
  bidi_it->scan_dir = 1; /* FIXME: do we need to have control on this? */
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
  bidi_it->level_stack[0].override = NEUTRAL_DIR; /* X1 */
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  bidi_it->new_paragraph = 0;
  bidi_it->next_en_pos = -1;
  bidi_it->next_for_ws.type = UNKNOWN_BT;
  bidi_set_sor_type (bidi_it, bidi_it->level_stack[0].level, 0); /* X10 */

  bidi_cache_reset ();
}

/* Do whatever UAX#9 clause X8 says should be done at paragraph's end,
   and set the new paragraph flag in the iterator.  */
static inline void
bidi_set_paragraph_end (struct bidi_it *bidi_it)
{
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  bidi_it->stack_idx = 0;
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
  bidi_it->new_paragraph = 1;
}

/* Initialize the bidi iterator from buffer position POS for paragraph
   direction DIR.  Return the embedding level at POS.  */
void
bidi_init_it (int pos, bidi_dir_t dir, struct bidi_it *bidi_it)
{
  if (! bidi_initialized)
    bidi_initialize ();
  bidi_set_paragraph_end (bidi_it);
  bidi_it->charpos = pos;
  if (pos <= 0)
    {
      bidi_it->bytepos = bidi_it->charpos;
      bidi_it->ch_len = 1;	/* so that incrementing bytepos works */
    }
  else
    {
      bidi_it->bytepos = CHAR_TO_BYTE (pos);
      bidi_it->ch_len
	= MULTIBYTE_FORM_LENGTH (BYTE_POS_ADDR (bidi_it->bytepos),
				 MAX_MULTIBYTE_LENGTH);
    }
  bidi_it->ch = '\x1d';	 /* FIXME: should be U+2029 */
  bidi_it->type = NEUTRAL_B;
  bidi_it->orig_type = UNKNOWN_BT;
  bidi_it->pristine_type = UNKNOWN_BT;
  bidi_it->prev_was_pdf = 0;
  bidi_it->prev.type = bidi_it->prev.orig_type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.orig_type =
    bidi_it->last_strong.pristine_type = UNKNOWN_BT;
  bidi_it->next_for_neutral.charpos = -1;
  bidi_it->next_for_neutral.type =
    bidi_it->next_for_neutral.orig_type =
    bidi_it->next_for_neutral.pristine_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.charpos = -1;
  bidi_it->prev_for_neutral.type =
    bidi_it->prev_for_neutral.orig_type =
    bidi_it->prev_for_neutral.pristine_type = UNKNOWN_BT;
  bidi_it->sor = L2R;	 /* FIXME: should it be user-selectable? */
  bidi_paragraph_init (dir, bidi_it);
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
  saved_info->orig_type = bidi_it->orig_type;
  saved_info->pristine_type = bidi_it->pristine_type;
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

  if (bidi_it->charpos < 0)
    bidi_it->charpos = bidi_it->bytepos = 0;
  else
    {
      bidi_it->charpos++;
      bidi_it->bytepos += bidi_it->ch_len;
    }

  current_level = bidi_it->level_stack[bidi_it->stack_idx].level; /* X1 */
  override = bidi_it->level_stack[bidi_it->stack_idx].override;
  new_level = current_level;

  /* in case it is a unibyte character (not yet implemented) */
  /* _fetch_multibyte_char_len = 1; */
  curchar = FETCH_CHAR (bidi_it->bytepos);
  bidi_it->ch = curchar;
  bidi_it->ch_len = CHAR_BYTES (curchar);

  type = bidi_get_type (curchar);
  bidi_it->pristine_type = type;

  if (type != PDF)
    bidi_it->prev_was_pdf = 0;

  bidi_it->orig_type = UNKNOWN_BT;

  switch (type)
    {
      case RLE:	/* X2 */
      case RLO:	/* X4 */
	bidi_it->orig_type = type;
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= 0)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 4)
	      {
		/* Compute the least odd embedding level greater than
		   the current level.  */
		new_level = ((current_level + 1) & ~1) + 1;
		if (bidi_it->orig_type == RLE)
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
	else if (bidi_it->prev.orig_type == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      case LRE:	/* X3 */
      case LRO:	/* X5 */
	bidi_it->orig_type = type;
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= 0)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 5)
	      {
		/* Compute the least even embedding level greater than
		   the current level.  */
		new_level = ((current_level + 2) & ~1);
		if (bidi_it->orig_type == LRE)
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
	else if (bidi_it->prev.orig_type == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      case PDF:	/* X7 */
	bidi_it->orig_type = type;
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
	else if (bidi_it->prev.orig_type == WEAK_EN /* W5/Retaining */
		 || bidi_it->next_en_pos > bidi_it->charpos)
	  type = WEAK_EN;
	break;
      default:
	/* Nothing.  */
	break;
    }

  bidi_it->type = type;

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

  /* For when the paragraph end is defined by anything other than a
     special Unicode character (a.k.a. ``higher protocols'').  */
  if (bidi_it->type != NEUTRAL_B)
    if (bidi_at_paragraph_end (bidi_it->ch,
			       bidi_it->bytepos + bidi_it->ch_len))
      bidi_it->type = NEUTRAL_B;

  if (bidi_it->type == NEUTRAL_B)	/* X8 */
    {
      bidi_set_paragraph_end (bidi_it);
      bidi_it->orig_type = bidi_it->type; /* needed below and in L1 */
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
    bidi_it->orig_type = type;	/* needed in L1 */

  /* Level and directional override status are already recorded in
     bidi_it, and do not need any change; see X6.  */
  if (override == R2L)		/* X6 */
    type = STRONG_R;
  else if (override == L2R)
    type = STRONG_L;
  else if (type == STRONG_AL)
    type = STRONG_R;		/* W3 */
  else if (type == WEAK_NSM)	/* W1 */
    {
      /* Note that we don't need to consider the case where the prev
	 character has its type overridden by an RLO or LRO: such
	 characters are outside the current level run, and thus not
	 relevant to this NSM.  Thus, NSM gets the pristine_type of
	 the previous character.  */
      if (bidi_it->prev.type != UNKNOWN_BT)
	type = bidi_it->prev.pristine_type;
      else if (bidi_it->sor == R2L)
	type = STRONG_R;
      else if (bidi_it->sor == L2R)
	type = STRONG_L;
      else /* shouldn't happen! */
	abort ();
      if (type == WEAK_EN	/* W2 after W1 */
	  && bidi_it->last_strong.orig_type == STRONG_AL)
	type = WEAK_AN;
    }
  else if (type == WEAK_EN	/* W2 */
	   && bidi_it->last_strong.orig_type == STRONG_AL)
    type = WEAK_AN;
  else if ((type == WEAK_ES
	    && (bidi_it->prev.orig_type == WEAK_EN 		/* W4 */
		&& (bidi_it->prev.pristine_type == WEAK_EN
		    || bidi_it->prev.pristine_type == WEAK_NSM))) /* aft W1 */
	   || (type == WEAK_CS
	       && ((bidi_it->prev.orig_type == WEAK_EN
		    && (bidi_it->prev.pristine_type == WEAK_EN  /* W4 */
			|| bidi_it->prev.pristine_type == WEAK_NSM)) /* a/W1 */
		   || bidi_it->prev.orig_type == WEAK_AN))) 	/* W4 */
    {
      next_char = FETCH_CHAR (bidi_it->bytepos + bidi_it->ch_len);
      type_of_next = bidi_get_type (next_char);

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
	 character is AL, that next EN will be changed to AN when we
	 process it in W2 above.  So in that case, this ES should not
	 be changed into EN.  */
      if (type == WEAK_ES
	  && type_of_next == WEAK_EN
	  && bidi_it->last_strong.orig_type != STRONG_AL)
	type = WEAK_EN;
      else if (type == WEAK_CS)
	{
	  if (bidi_it->prev.orig_type == WEAK_AN
	      && (type_of_next == WEAK_AN
		  /* If the next character is EN, but the last
		     strong-type character is AL, EN will be later
		     changed to AN when we process it in W2 above.  So
		     in that case, this ES should not be changed into
		     EN.  */
		  || (type_of_next == WEAK_EN
		      && bidi_it->last_strong.orig_type == STRONG_AL)))
	    type = WEAK_AN;
	  else if (bidi_it->prev.orig_type == WEAK_EN
		   && type_of_next == WEAK_EN
		   && bidi_it->last_strong.orig_type != STRONG_AL)
	    type = WEAK_EN;
	}
    }
  else if (type == WEAK_ET	/* W5: ET with EN before or after it */
	   || type == WEAK_BN)	/* W5/Retaining */
    {
      if (bidi_it->prev.orig_type == WEAK_EN /* ET/BN with EN before it */
	  || bidi_it->next_en_pos > bidi_it->charpos)
	type = WEAK_EN;
      /* W5: ET with EN after it.  */
      else
	{
	  int en_pos = bidi_it->charpos + 1;

	  next_char = FETCH_CHAR (bidi_it->bytepos + bidi_it->ch_len);
	  type_of_next = bidi_get_type (next_char);

	  if (type_of_next == WEAK_ET
	      || type_of_next == WEAK_BN
	      || bidi_explicit_dir_char (next_char))
	    {
	      bidi_copy_it (&saved_it, bidi_it);
	      while (bidi_resolve_explicit (bidi_it) == new_level
		     && (bidi_it->type == WEAK_BN || bidi_it->type == WEAK_ET))
		;
	      type_of_next = bidi_it->type;
	      en_pos = bidi_it->charpos;
	      bidi_copy_it (bidi_it, &saved_it);
	    }
	  if (type_of_next == WEAK_EN)
	    {
	      /* If the last strong character is AL, the EN we've
		 found will become AN when we get to it (W2). */
	      if (bidi_it->last_strong.orig_type != STRONG_AL)
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

  if (type == WEAK_ES || type == WEAK_ET || type == WEAK_CS /* W6 */
      || (type == WEAK_BN && (bidi_it->prev.orig_type == WEAK_CS /* W6/Ret. */
			      || bidi_it->prev.orig_type == WEAK_ES
			      || bidi_it->prev.orig_type == WEAK_ET)))
    type = NEUTRAL_ON;

  /* Store the type we've got so far, before we clobber it with strong
     types in W7 and while resolving neutral types.  But leave alone
     the original types that were recorded above, because we will need
     them for the L1 clause.  */
  if (bidi_it->orig_type == UNKNOWN_BT)
    bidi_it->orig_type = type;

  if (type == WEAK_EN)	/* W7 */
    {
      if ((bidi_it->last_strong.orig_type == STRONG_L)
	  || (bidi_it->last_strong.type == UNKNOWN_BT && bidi_it->sor == L2R))
	type = STRONG_L;
    }

  bidi_it->type = type;
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
	    if (bidi_it->orig_type != WEAK_BN /* W1/Retaining */
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
		    || bidi_get_category (bidi_it->prev.orig_type) == NEUTRAL)
		  {
		    next_type = bidi_it->prev_for_neutral.type;
		    saved_it.next_for_neutral.type = next_type;
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
      if (bidi_it->orig_type != WEAK_BN /* W1/Retaining */
	  && bidi_it->type != WEAK_BN)
	bidi_remember_char (&bidi_it->prev, bidi_it);
      if (bidi_it->orig_type == STRONG_R || bidi_it->orig_type == STRONG_L
	  || bidi_it->orig_type == STRONG_AL)
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

  /* For L1 below, we need to know, for each WS character, whether
     it belongs to a sequence of WS characters preceeding a newline
     or a TAB or a paragraph separator.  */
  if (bidi_it->pristine_type == NEUTRAL_WS
      && bidi_it->next_for_ws.type == UNKNOWN_BT)
    {
      int ch;
      int clen = bidi_it->ch_len;
      int bpos = bidi_it->bytepos;
      int cpos = bidi_it->charpos;
      bidi_type_t chtype;

      do {
	/*_fetch_multibyte_char_len = 1;*/
	ch = FETCH_CHAR (bpos + clen);
	bpos += clen;
	cpos++;
	clen = CHAR_BYTES (ch);
	if (ch == '\n' /* || ch == LINESEP_CHAR */)
	  chtype = NEUTRAL_B;
	else
	  chtype = bidi_get_type (ch);
      } while (chtype == NEUTRAL_WS || chtype == WEAK_BN
	       || bidi_explicit_dir_char (ch)); /* L1/Retaining */
      bidi_it->next_for_ws.type = chtype;
      bidi_it->next_for_ws.charpos = cpos;
      bidi_it->next_for_ws.bytepos = bpos;
    }

  /* Resolve implicit levels, with a twist: PDFs get the embedding
     level of the enbedding they terminate.  See below for the
     reason.  */
  if (bidi_it->pristine_type == PDF
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

	 which will result because we buump up the embedding level as
	 soon as we see the RLO and pop it as soon as we see the PDF,
	 so RLO itself has the same embedding level as "teST", and
	 thus would be normally delivered last, just before the PDF.
	 The switch below fiddles with the level of PDF so that this
	 ugly side effect does not happen.

	 (This is, of course, only important if the formatting codes
	 are actually displayed, but Emacs does display them if the
	 user wants to.)     */
      level = prev_level;
    }
  else if (bidi_it->pristine_type == NEUTRAL_B /* L1 */
	   || bidi_it->pristine_type == NEUTRAL_S
	   || bidi_it->ch == '\n' /* || bidi_it->ch == LINESEP_CHAR */
	   || (bidi_it->pristine_type == NEUTRAL_WS
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
  struct bidi_it prev_bidi_it;

  if (bidi_it->scan_dir == 0)
    {
      bidi_it->scan_dir = 1;	/* default to logical order */
    }

  if (bidi_it->new_paragraph)
    bidi_paragraph_init (bidi_overriding_paragraph_direction, bidi_it);
  if (bidi_cache_idx == 0)
    bidi_copy_it (&prev_bidi_it, bidi_it);

  old_level = bidi_it->resolved_level;
  new_level = bidi_level_of_next_char (bidi_it);
  if (bidi_it->ch == BIDI_EOB)
    return;

  /* Reordering of resolved levels (clause L2) is implemented by
     jumping to the other edge of the level and flipping direction of
     scanning the buffer whenever we find a level change.  */
  if (new_level != old_level)
    {
      int ascending = new_level > old_level;
      int level_to_search = ascending ? old_level + 1 : old_level;
      int incr = ascending ? 1 : -1;
      int expected_next_level = old_level + incr;

      /* If we don't have anything cached yet, we need to cache the
	 previous character we've seen, since we'll need it to record
	 where to jump when the last non-base level is exhausted.  */
      if (bidi_cache_idx == 0)
	bidi_cache_iterator_state (&prev_bidi_it, 1);
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

#ifdef TEST_STANDALONE

#include <stdio.h>
#include <sys/stat.h>
#include <signal.h>

static char display_line[80];
static int simulate_display;
static int incr = 1;

void
signal_catcher (int sig)
{
  if (simulate_display)
    puts (display_line);
  else
    {
      puts ("<");
      fflush (stdout);
    }
  signal (sig, SIG_DFL);
  raise (sig);
}

void
put_line (char *p)
{
  if (simulate_display)
    {
      if (incr == -1)
	{
	  if (p >= display_line)
	    memset (display_line, ' ', p - display_line + 1);
	}
      else
	*p = '\0';
      fputs (display_line, stdout);
    }
  fflush (stdout);
}

char *
init_display_direction (bidi_dir_t default_dir, int base_level)
{
  char *p;

  /* To which display margin should we flush the lines?  */
  switch (default_dir)
    {
      case NEUTRAL_DIR:
	if ((base_level & 1) == 0)
	  {
	    p = display_line;
	    incr = 1;
	  }
	else
	  {
	    p = display_line + sizeof (display_line) - 1;
	    *p-- = '\0';
	    incr = -1;
	  }
	break;
      case L2R:
	p = display_line;
	incr = 1;
	break;
      case R2L:
	p = display_line + sizeof (display_line) - 1;
	*p-- = '\0';
	incr = -1;
	break;
      default:
	abort ();
    }

  return p;
}

static char *
continuation_line (char *p, int need)
{
  if (incr == -1)
    {
      if (p < display_line + need)
	{
	  *p-- = '/';
	  put_line (p);
	  putc ('\n', stdout);
	  memset (display_line, '>', sizeof(display_line) - 1);
	  p = display_line + sizeof (display_line) - 1;
	  *p-- = '\0';
	}
    }
  else
    {
      if (p > display_line + sizeof(display_line) - need - 2)
	{
	  *p++ = '\\';
	  put_line (p);
	  putc ('\n', stdout);
	  memset (display_line, '<', sizeof(display_line) - 1);
	  p = display_line;
	}
    }

  return p;
}

int main (int argc, char *argv[])
{
  bidi_dir_t default_dir = NEUTRAL_DIR;
  char lots_of_equals[] = "\n===============================================================================\n";


  if (argc > 1 && argv[1][0] == '-')
    {
      switch (argv[1][1])
	{
	  case 'R':
	    default_dir = R2L;
	    simulate_display = 1;
	    ++argv;
	    break;
	  case 'L':
	    default_dir = L2R;
	    simulate_display = 1;
	    ++argv;
	    break;
	  case 'N':
	    simulate_display = 1;
	    ++argv;
	    break;
	  default:
	    break;
	}
      bidi_overriding_paragraph_direction = default_dir;
    }

  for (argv++; *argv; argv++)
    {
      FILE *in = fopen (*argv, "rb");
      struct stat stat_buf;
      struct bidi_it iterator;
      size_t i;
      char *p = display_line;
      int base_level;
      unsigned char *s, *d, *s_end;

      if (!in || stat (*argv, &stat_buf))
	{
	  perror (*argv);
	  continue;
	}

      if (stat_buf.st_size > input_buf_size)
	{
	  input_buf = realloc (input_buf, stat_buf.st_size + 1);
	  if (!input_buf)
	    {
	      perror ("realloc input buffer");
	      continue;
	    }
	  input_buf_size = stat_buf.st_size;
	}
      if (fread (input_buf, 1, stat_buf.st_size, in) != stat_buf.st_size)
	{
	  perror ("reading input");
	  continue;
	}
      input_buf[stat_buf.st_size] = '\0';
      for (d = s = input_buf, s_end = s + stat_buf.st_size - 1; *s; s++)
	{
	  if (*s != '\r' || s >= s_end || s[1] != '\n')
	    *d++ = *s;
	}
      stat_buf.st_size = d - input_buf;
      input_buf[stat_buf.st_size] = '\0';

      /* Done with administrivia, now for some real work...  */
      signal (SIGABRT, signal_catcher);
      signal (SIGINT, signal_catcher);
      bidi_init_it (-1, default_dir, &iterator);
      if (simulate_display)
	{
	  p = init_display_direction (default_dir,
				      iterator.level_stack[0].level);
	}

      memset (display_line, incr == -1 ? '>' : '<', sizeof (display_line) - 1);
      display_line[sizeof (display_line) - 1] = '\0';
      base_level = iterator.level_stack[0].level;

      for (i = 0; i <= stat_buf.st_size; i++)
	{
	  int c;

	  bidi_get_next_char_visually (&iterator);
	  c = iterator.ch;

	  if (c == '\n' || c == BIDI_EOB)
	    {
	      if (simulate_display)
		{
		  put_line (p);
		  /* FIXME: if -R or -L, need to init paragraph here.  */
		}
	      if (c == BIDI_EOB)
		break;
	      putc (c, stdout);
	    }
	  else if (c >= LRE_CHAR && c <= LRM_CHAR)
	    {
	      if (simulate_display)
		{
		  p = continuation_line (p, 5);
		  if (incr == -1)
		    {
		      memcpy (p - 4, bidi_name[c], 5);
		      p -= 5;
		    }
		  else
		    {
		      memcpy (p, bidi_name[c], 5);
		      p += 5;
		    }
		}
	      else
		fputs (bidi_name[c], stdout);
	    }
	  else if (c < ' ')
	    {
	      if (simulate_display)
		{
		  p = continuation_line (p, 2);
		  if (incr == -1)
		    {
		      *p-- = '@' + c;
		      *p-- = '^';
		    }
		  else
		    {
		      *p++ = '^';
		      *p++ = '@' + c;
		    }
		}
	      else
		printf ("^%c", (c | 0x40));
	    }
	  else
	    {
	      int c1 = (iterator.type == STRONG_R) ? bidi_mirror_char (c) : c;

	      if (simulate_display)
		{
		  p = continuation_line (p, 1);
		  *p = c1;
		  p += incr;
		}
	      else
		putc (c1, stdout);
	    }

	  if (iterator.ch == '\n')
	    {
	      if (base_level != iterator.level_stack[0].level)
		base_level = iterator.level_stack[0].level;
	      p = init_display_direction (default_dir, base_level);
	      memset (display_line, incr == -1 ? '>' : '<',
		      sizeof (display_line) - 1);
	    }
	}
      fputs (lots_of_equals, stdout);
      fclose (in);
    }
  return 0;
}
#endif
