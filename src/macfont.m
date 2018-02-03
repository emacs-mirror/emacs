/* Font driver on macOS Core text.
   Copyright (C) 2009-2018 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

Original author: YAMAMOTO Mitsuharu
*/

#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"
#include "termchar.h"
#include "nsgui.h"
#include "nsterm.h"
#include "macfont.h"
#include "macuvs.h"

#include <libkern/OSByteOrder.h>

static double mac_font_get_advance_width_for_glyph (CTFontRef, CGGlyph);
static CGRect mac_font_get_bounding_rect_for_glyph (CTFontRef, CGGlyph);
static CFArrayRef mac_font_create_available_families (void);
static Boolean mac_font_equal_in_postscript_name (CTFontRef, CTFontRef);
static CTLineRef mac_font_create_line_with_string_and_font (CFStringRef,
							    CTFontRef);
static Boolean mac_font_descriptor_supports_languages (CTFontDescriptorRef,
						       CFArrayRef);
static CFStringRef mac_font_create_preferred_family_for_attributes (CFDictionaryRef);
static CFIndex mac_font_shape (CTFontRef, CFStringRef,
			       struct mac_glyph_layout *, CFIndex);
static CFArrayRef mac_font_copy_default_descriptors_for_language (CFStringRef);
static CFStringRef mac_font_copy_default_name_for_charset_and_languages (CFCharacterSetRef, CFArrayRef);
#if USE_CT_GLYPH_INFO
static CGGlyph mac_ctfont_get_glyph_for_cid (CTFontRef, CTCharacterCollection,
                                             CGFontIndex);
#endif

struct macfont_metrics;

/* The actual structure for Mac font that can be cast to struct font.  */

struct macfont_info
{
  struct font font;
  CTFontRef macfont;
  CGFontRef cgfont;
  ScreenFontRef screen_font;
  struct macfont_cache *cache;
  struct macfont_metrics **metrics;
  short metrics_nrows;
  bool_bf synthetic_italic_p : 1;
  bool_bf synthetic_bold_p : 1;
  unsigned spacing : 2;
  unsigned antialias : 2;
  bool_bf color_bitmap_p : 1;
};

/* Values for the `spacing' member in `struct macfont_info'.  */

enum
  {
    MACFONT_SPACING_PROPORTIONAL,
    MACFONT_SPACING_MONO,
    MACFONT_SPACING_SYNTHETIC_MONO,
  };

/* Values for the `antialias' member in `struct macfont_info'.  */

enum
  {
    MACFONT_ANTIALIAS_DEFAULT,
    MACFONT_ANTIALIAS_OFF,
    MACFONT_ANTIALIAS_ON,
  };

enum {FONT_SLANT_SYNTHETIC_ITALIC = 200}; /* FC_SLANT_ITALIC + 100 */
enum {FONT_WEIGHT_SYNTHETIC_BOLD = 200};  /* FC_WEIGHT_BOLD */
enum {FONT_SPACING_SYNTHETIC_MONO = FONT_SPACING_MONO};

static const CGAffineTransform synthetic_italic_atfm = {1, 0, 0.25, 1, 0, 0};
static const CGFloat synthetic_bold_factor = 0.024;

static Boolean cfnumber_get_font_symbolic_traits_value (CFNumberRef,
                                                        CTFontSymbolicTraits *);
static void macfont_store_descriptor_attributes (CTFontDescriptorRef,
                                                 Lisp_Object);
static Lisp_Object macfont_descriptor_entity (CTFontDescriptorRef, Lisp_Object,
                                              CTFontSymbolicTraits);
static CFStringRef macfont_create_family_with_symbol (Lisp_Object);
static int macfont_glyph_extents (struct font *, CGGlyph,
                                  struct font_metrics *, CGFloat *, int);
static CFMutableDictionaryRef macfont_create_attributes_with_spec (Lisp_Object);
static Boolean macfont_supports_charset_and_languages_p (CTFontDescriptorRef,
                                                         CFCharacterSetRef,
                                                         Lisp_Object,
                                                         CFArrayRef);
static Boolean macfont_closest_traits_index_p (CFArrayRef, CTFontSymbolicTraits,
                                               CFIndex);
static CFDataRef mac_font_copy_uvs_table (CTFontRef);
static void mac_font_get_glyphs_for_variants (CFDataRef, UTF32Char,
                                              const UTF32Char [],
                                              CGGlyph [], CFIndex);

/* From CFData to a lisp string.  Always returns a unibyte string.  */

static Lisp_Object
cfdata_to_lisp (CFDataRef data)
{
  CFIndex len = CFDataGetLength (data);
  Lisp_Object result = make_uninit_string (len);

  CFDataGetBytes (data, CFRangeMake (0, len), SDATA (result));

  return result;
}



/* From CFString to a lisp string.  Returns a unibyte string
   containing a UTF-8 byte sequence.  */

static Lisp_Object
cfstring_to_lisp_nodecode (CFStringRef string)
{
  Lisp_Object result = Qnil;
  CFDataRef data;
  const char *s = CFStringGetCStringPtr (string, kCFStringEncodingUTF8);

  if (s)
    {
      CFIndex i, length = CFStringGetLength (string);

      for (i = 0; i < length; i++)
        if (CFStringGetCharacterAtIndex (string, i) == 0)
          break;

      if (i == length)
        return make_unibyte_string (s, strlen (s));
    }

  data = CFStringCreateExternalRepresentation (NULL, string,
                                               kCFStringEncodingUTF8, '?');
  if (data)
    {
      result = cfdata_to_lisp (data);
      CFRelease (data);
    }

  return result;
}

/* Lisp string containing a UTF-8 byte sequence to CFString.  Unlike
   cfstring_create_with_utf8_cstring, this function preserves NUL
   characters.  */

static CFStringRef
cfstring_create_with_string_noencode (Lisp_Object s)
{
  CFStringRef string = CFStringCreateWithBytes (NULL, SDATA (s), SBYTES (s),
                                                kCFStringEncodingUTF8, false);

  if (string == NULL)
    /* Failed to interpret as UTF 8.  Fall back on Mac Roman.  */
    string = CFStringCreateWithBytes (NULL, SDATA (s), SBYTES (s),
                                      kCFStringEncodingMacRoman, false);

  return string;
}

static CFIndex
mac_font_get_weight (CTFontRef font)
{
  NSFont *nsFont = (NSFont *) font;

  return [[NSFontManager sharedFontManager] weightOfFont:nsFont];
}

static CGFloat
mac_screen_font_get_advance_width_for_glyph (ScreenFontRef font, CGGlyph glyph)
{
  NSSize advancement = [(NSFont *)font advancementForGlyph:glyph];

  return advancement.width;
}

#if !USE_CT_GLYPH_INFO
static CGGlyph
mac_font_get_glyph_for_cid (CTFontRef font, NSCharacterCollection collection,
                            CGFontIndex cid)
{
  CGGlyph result = kCGFontIndexInvalid;
  NSFont *nsFont = (NSFont *) font;
  unichar characters[] = {0xfffd};
  NSString *string =
    [NSString stringWithCharacters:characters
			    length:ARRAYELTS (characters)];
  NSGlyphInfo *glyphInfo =
    [NSGlyphInfo glyphInfoWithCharacterIdentifier:cid
				       collection:collection
				       baseString:string];
  NSDictionary *attributes =
    [NSDictionary dictionaryWithObjectsAndKeys:nsFont,NSFontAttributeName,
		  glyphInfo,NSGlyphInfoAttributeName,nil];
  NSTextStorage *textStorage =
    [[NSTextStorage alloc] initWithString:string
			       attributes:attributes];
  NSLayoutManager *layoutManager = [[NSLayoutManager alloc] init];
  NSTextContainer *textContainer = [[NSTextContainer alloc] init];
  NSFont *fontInTextStorage;

  [layoutManager addTextContainer:textContainer];
  [textContainer release];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager release];

  /* Force layout.  */
  (void) [layoutManager glyphRangeForTextContainer:textContainer];

  fontInTextStorage = [textStorage attribute:NSFontAttributeName atIndex:0
			      effectiveRange:NULL];
  if (fontInTextStorage == nsFont
      || [[fontInTextStorage fontName] isEqualToString:[nsFont fontName]])
    {
      NSGlyph glyph = [layoutManager glyphAtIndex:0];

      if (glyph < [nsFont numberOfGlyphs])
	result = glyph;
    }

  [textStorage release];

  return result;
}
#endif

static ScreenFontRef
mac_screen_font_create_with_name (CFStringRef name, CGFloat size)
{
  NSFont *result, *font;

  font = [NSFont fontWithName:((NSString *) name) size:size];
  result = [font screenFont];

  return (ScreenFontRef)[result retain];
}


static Boolean
mac_screen_font_get_metrics (ScreenFontRef font, CGFloat *ascent,
                             CGFloat *descent, CGFloat *leading)
{
  NSFont *nsFont = [(NSFont *)font printerFont];
  NSTextStorage *textStorage;
  NSLayoutManager *layoutManager;
  NSTextContainer *textContainer;
  NSRect usedRect;
  NSPoint spaceLocation;
  CGFloat descender;

  textStorage = [[NSTextStorage alloc] initWithString:@" "];
  layoutManager = [[NSLayoutManager alloc] init];
  textContainer = [[NSTextContainer alloc] init];

  [textStorage setFont:nsFont];
  [textContainer setLineFragmentPadding:0];

  [layoutManager addTextContainer:textContainer];
  [textContainer release];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager release];

  if (!(textStorage && layoutManager && textContainer))
    {
      [textStorage release];

      return false;
    }

  usedRect = [layoutManager lineFragmentUsedRectForGlyphAtIndex:0
                                                 effectiveRange:NULL];
  spaceLocation = [layoutManager locationForGlyphAtIndex:0];
  [textStorage release];

  *ascent = spaceLocation.y;
  *descent = NSHeight (usedRect) - spaceLocation.y;
  *leading = 0;
  descender = [nsFont descender];
  if (- descender < *descent)
    {
      *leading = *descent + descender;
      *descent = - descender;
    }

  return true;
}

static CFIndex
mac_font_shape_1 (NSFont *font, NSString *string,
                  struct mac_glyph_layout *glyph_layouts, CFIndex glyph_len)
{
  NSUInteger i;
  CFIndex result = 0;
  NSTextStorage *textStorage;
  NSLayoutManager *layoutManager;
  NSTextContainer *textContainer;
  NSUInteger stringLength;
  NSPoint spaceLocation;
  NSUInteger used, numberOfGlyphs;

  textStorage = [[NSTextStorage alloc] initWithString:string];
  layoutManager = [[NSLayoutManager alloc] init];
  textContainer = [[NSTextContainer alloc] init];

  /* Append a trailing space to measure baseline position.  */
  [textStorage appendAttributedString:([[[NSAttributedString alloc]
                                          initWithString:@" "] autorelease])];
  [textStorage setFont:font];
  [textContainer setLineFragmentPadding:0];

  [layoutManager addTextContainer:textContainer];
  [textContainer release];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager release];

  if (!(textStorage && layoutManager && textContainer))
    {
      [textStorage release];

      return 0;
    }

  stringLength = [string length];

  /* Force layout.  */
  (void) [layoutManager glyphRangeForTextContainer:textContainer];

  spaceLocation = [layoutManager locationForGlyphAtIndex:stringLength];

  /* Remove the appended trailing space because otherwise it may
     generate a wrong result for a right-to-left text.  */
  [textStorage beginEditing];
  [textStorage deleteCharactersInRange:(NSMakeRange (stringLength, 1))];
  [textStorage endEditing];
  (void) [layoutManager glyphRangeForTextContainer:textContainer];

  i = 0;
  while (i < stringLength)
    {
      NSRange range;
      NSFont *fontInTextStorage =
        [textStorage attribute:NSFontAttributeName atIndex:i
                     longestEffectiveRange:&range
                       inRange:(NSMakeRange (0, stringLength))];

      if (!(fontInTextStorage == font
            || [[fontInTextStorage fontName] isEqualToString:[font fontName]]))
        break;
      i = NSMaxRange (range);
    }
  if (i < stringLength)
    /* Make the test `used <= glyph_len' below fail if textStorage
       contained some fonts other than the specified one.  */
    used = glyph_len + 1;
  else
    {
      NSRange range = NSMakeRange (0, stringLength);

      range = [layoutManager glyphRangeForCharacterRange:range
                                    actualCharacterRange:NULL];
      numberOfGlyphs = NSMaxRange (range);
      used = numberOfGlyphs;
      for (i = 0; i < numberOfGlyphs; i++)
        if ([layoutManager notShownAttributeForGlyphAtIndex:i])
          used--;
    }

  if (0 < used && used <= glyph_len)
    {
      NSUInteger glyphIndex, prevGlyphIndex;
      unsigned char bidiLevel;
      NSUInteger *permutation;
      NSRange compRange, range;
      CGFloat totalAdvance;

      glyphIndex = 0;
      while ([layoutManager notShownAttributeForGlyphAtIndex:glyphIndex])
        glyphIndex++;

      /* For now we assume the direction is not changed within the
         string.  */
      [layoutManager getGlyphsInRange:(NSMakeRange (glyphIndex, 1))
                               glyphs:NULL
                           properties:NULL
                     characterIndexes:NULL
                           bidiLevels:&bidiLevel];
      if (bidiLevel & 1)
        permutation = xmalloc (sizeof (NSUInteger) * used);
      else
        permutation = NULL;

#define RIGHT_TO_LEFT_P permutation

      /* Fill the `comp_range' member of struct mac_glyph_layout, and
         setup a permutation for right-to-left text.  */
      compRange = NSMakeRange (0, 0);
      for (range = NSMakeRange (0, 0); NSMaxRange (range) < used;
           range.length++)
        {
          struct mac_glyph_layout *gl = glyph_layouts + NSMaxRange (range);
          NSUInteger characterIndex =
            [layoutManager characterIndexForGlyphAtIndex:glyphIndex];

          gl->string_index = characterIndex;

          if (characterIndex >= NSMaxRange (compRange))
            {
              compRange.location = NSMaxRange (compRange);
              do
                {
                  NSRange characterRange =
                    [string
                      rangeOfComposedCharacterSequenceAtIndex:characterIndex];

                  compRange.length =
                    NSMaxRange (characterRange) - compRange.location;
                  [layoutManager glyphRangeForCharacterRange:compRange
                                        actualCharacterRange:&characterRange];
                  characterIndex = NSMaxRange (characterRange) - 1;
                }
              while (characterIndex >= NSMaxRange (compRange));

              if (RIGHT_TO_LEFT_P)
                for (i = 0; i < range.length; i++)
                  permutation[range.location + i] = NSMaxRange (range) - i - 1;

              range = NSMakeRange (NSMaxRange (range), 0);
            }

          gl->comp_range.location = compRange.location;
          gl->comp_range.length = compRange.length;

          while (++glyphIndex < numberOfGlyphs)
            if (![layoutManager notShownAttributeForGlyphAtIndex:glyphIndex])
              break;
        }
      if (RIGHT_TO_LEFT_P)
        for (i = 0; i < range.length; i++)
          permutation[range.location + i] = NSMaxRange (range) - i - 1;

      /* Then fill the remaining members.  */
      glyphIndex = prevGlyphIndex = 0;
      while ([layoutManager notShownAttributeForGlyphAtIndex:glyphIndex])
        glyphIndex++;

      if (!RIGHT_TO_LEFT_P)
        totalAdvance = 0;
      else
        {
          NSUInteger nrects;
          NSRect *glyphRects =
            [layoutManager
              rectArrayForGlyphRange:(NSMakeRange (0, numberOfGlyphs))
              withinSelectedGlyphRange:(NSMakeRange (NSNotFound, 0))
                     inTextContainer:textContainer rectCount:&nrects];

          totalAdvance = NSMaxX (glyphRects[0]);
        }

      for (i = 0; i < used; i++)
        {
          struct mac_glyph_layout *gl;
          NSPoint location;
          NSUInteger nextGlyphIndex;
          NSRange glyphRange;
          NSRect *glyphRects;
          NSUInteger nrects;

          if (!RIGHT_TO_LEFT_P)
            gl = glyph_layouts + i;
          else
            {
              NSUInteger dest = permutation[i];

              gl = glyph_layouts + dest;
              if (i < dest)
                {
                  CFIndex tmp = gl->string_index;

                  gl->string_index = glyph_layouts[i].string_index;
                  glyph_layouts[i].string_index = tmp;
                }
            }
          gl->glyph_id = [layoutManager glyphAtIndex:glyphIndex];

          location = [layoutManager locationForGlyphAtIndex:glyphIndex];
          gl->baseline_delta = spaceLocation.y - location.y;

          for (nextGlyphIndex = glyphIndex + 1; nextGlyphIndex < numberOfGlyphs;
               nextGlyphIndex++)
            if (![layoutManager
                   notShownAttributeForGlyphAtIndex:nextGlyphIndex])
              break;

          if (!RIGHT_TO_LEFT_P)
            {
              CGFloat maxX;

              if (prevGlyphIndex == 0)
                glyphRange = NSMakeRange (0, nextGlyphIndex);
              else
                glyphRange = NSMakeRange (glyphIndex,
                                          nextGlyphIndex - glyphIndex);
              glyphRects =
                [layoutManager
                  rectArrayForGlyphRange:glyphRange
                  withinSelectedGlyphRange:(NSMakeRange (NSNotFound, 0))
                         inTextContainer:textContainer rectCount:&nrects];
              maxX = max (NSMaxX (glyphRects[0]), totalAdvance);
              gl->advance_delta = location.x - totalAdvance;
              gl->advance = maxX - totalAdvance;
              totalAdvance = maxX;
            }
          else
            {
              CGFloat minX;

              if (nextGlyphIndex == numberOfGlyphs)
                glyphRange = NSMakeRange (prevGlyphIndex,
                                          numberOfGlyphs - prevGlyphIndex);
              else
                glyphRange = NSMakeRange (prevGlyphIndex,
                                          glyphIndex + 1 - prevGlyphIndex);
              glyphRects =
                [layoutManager
                  rectArrayForGlyphRange:glyphRange
                  withinSelectedGlyphRange:(NSMakeRange (NSNotFound, 0))
                         inTextContainer:textContainer rectCount:&nrects];
              minX = min (NSMinX (glyphRects[0]), totalAdvance);
              gl->advance = totalAdvance - minX;
              totalAdvance = minX;
              gl->advance_delta = location.x - totalAdvance;
            }

          prevGlyphIndex = glyphIndex + 1;
          glyphIndex = nextGlyphIndex;
        }

      if (RIGHT_TO_LEFT_P)
        xfree (permutation);

#undef RIGHT_TO_LEFT_P

      result = used;
    }
  [textStorage release];

  return result;
}

static CFIndex
mac_screen_font_shape (ScreenFontRef font, CFStringRef string,
                       struct mac_glyph_layout *glyph_layouts,
                       CFIndex glyph_len)
{
  return mac_font_shape_1 ([(NSFont *)font printerFont],
                           (NSString *) string,
                           glyph_layouts, glyph_len);
}

static CGColorRef
get_cgcolor(unsigned long idx, struct frame *f)
{
  NSColor *nsColor = ns_lookup_indexed_color (idx, f);
  [nsColor set];
  CGColorSpaceRef colorSpace = [[nsColor colorSpace] CGColorSpace];
  NSInteger noc = [nsColor numberOfComponents];
  CGFloat *components = xmalloc (sizeof(CGFloat)*(1+noc));
  CGColorRef cgColor;

  [nsColor getComponents: components];
  cgColor = CGColorCreate (colorSpace, components);
  xfree (components);
  return cgColor;
}

#define CG_SET_FILL_COLOR_WITH_FACE_FOREGROUND(context, face, f)        \
  do {                                                                  \
    CGColorRef refcol_ = get_cgcolor (NS_FACE_FOREGROUND (face), f);    \
    CGContextSetFillColorWithColor (context, refcol_) ;                 \
    CGColorRelease (refcol_);                                           \
  } while (0)
#define CG_SET_FILL_COLOR_WITH_FACE_BACKGROUND(context, face, f)        \
  do {                                                                  \
    CGColorRef refcol_ = get_cgcolor (NS_FACE_BACKGROUND (face), f);    \
    CGContextSetFillColorWithColor (context, refcol_);                  \
    CGColorRelease (refcol_);                                           \
  } while (0)
#define CG_SET_STROKE_COLOR_WITH_FACE_FOREGROUND(context, face, f)      \
  do {                                                                  \
    CGColorRef refcol_ = get_cgcolor (NS_FACE_FOREGROUND (face), f);    \
    CGContextSetStrokeColorWithColor (context, refcol_);                \
    CGColorRelease (refcol_);                                           \
  } while (0)



/* Mac font driver.  */

static struct
{
  /* registry name */
  const char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* additional constraint by language */
  CFStringRef lang;
  /* set on demand */
  CFCharacterSetRef cf_charset;
  CFStringRef cf_charset_string;
} cf_charset_table[] =
  { { "iso8859-1", { 0x00A0, 0x00A1, 0x00B4, 0x00BC, 0x00D0 } },
    { "iso8859-2", { 0x00A0, 0x010E }},
    { "iso8859-3", { 0x00A0, 0x0108 }},
    { "iso8859-4", { 0x00A0, 0x00AF, 0x0128, 0x0156, 0x02C7 }},
    { "iso8859-5", { 0x00A0, 0x0401 }},
    { "iso8859-6", { 0x00A0, 0x060C }},
    { "iso8859-7", { 0x00A0, 0x0384 }},
    { "iso8859-8", { 0x00A0, 0x05D0 }},
    { "iso8859-9", { 0x00A0, 0x00A1, 0x00BC, 0x011E }},
    { "iso8859-10", { 0x00A0, 0x00D0, 0x0128, 0x2015 }},
    { "iso8859-11", { 0x00A0, 0x0E01 }},
    { "iso8859-13", { 0x00A0, 0x201C }},
    { "iso8859-14", { 0x00A0, 0x0174 }},
    { "iso8859-15", { 0x00A0, 0x00A1, 0x00D0, 0x0152 }},
    { "iso8859-16", { 0x00A0, 0x0218}},
    { "gb2312.1980-0", { 0x4E13 }, CFSTR ("zh-Hans")},
    { "big5-0", { /* 0xF6B1 in ftfont.c */ 0x4EDC }, CFSTR ("zh-Hant") },
    { "jisx0208.1983-0", { 0x4E55 }, CFSTR ("ja")},
    { "ksc5601.1987-0", { 0xAC00 }, CFSTR ("ko")},
    { "cns11643.1992-1", { 0xFE32 }, CFSTR ("zh-Hant")},
    { "cns11643.1992-2", { 0x4E33, 0x7934 }},
    { "cns11643.1992-3", { 0x201A9 }},
    { "cns11643.1992-4", { 0x20057 }},
    { "cns11643.1992-5", { 0x20000 }},
    { "cns11643.1992-6", { 0x20003 }},
    { "cns11643.1992-7", { 0x20055 }},
    { "gbk-0", { 0x4E06 }, CFSTR ("zh-Hans")},
    { "jisx0212.1990-0", { 0x4E44 }},
    { "jisx0213.2000-1", { 0xFA10 }, CFSTR ("ja")},
    { "jisx0213.2000-2", { 0xFA49 }},
    { "jisx0213.2004-1", { 0x20B9F }},
    { "viscii1.1-1", { 0x1EA0, 0x1EAE, 0x1ED2 }, CFSTR ("vi")},
    { "tis620.2529-1", { 0x0E01 }, CFSTR ("th")},
    { "windows-1251", { 0x0401, 0x0490 }, CFSTR ("ru")},
    { "koi8-r", { 0x0401, 0x2219 }, CFSTR ("ru")},
    { "mulelao-1", { 0x0E81 }, CFSTR ("lo")},
    { "unicode-sip", { 0x20000 }},
    { NULL }
  };

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1080
static const struct
{
  CFStringRef language;
  CFStringRef font_names[3];
} macfont_language_default_font_names[] = {
  { CFSTR ("ja"), { CFSTR ("HiraKakuProN-W3"), /* 10.5 - 10.9 */
                    CFSTR ("HiraKakuPro-W3"),  /* 10.4 */
                    NULL }},
  { CFSTR ("ko"), { CFSTR ("AppleSDGothicNeo-Regular"), /* 10.8 - 10.9 */
                    CFSTR ("AppleGothic"), /* 10.4 - 10.7 */
                    NULL }},
  { CFSTR ("zh-Hans"), { CFSTR ("STHeitiSC-Light"), /* 10.6 - 10.9 */
                         CFSTR ("STXihei"),	    /* 10.4 - 10.5 */
                         NULL }},
  { CFSTR ("zh-Hant"), { CFSTR ("STHeitiTC-Light"), /* 10.6 - 10.9 */
                         CFSTR ("LiHeiPro"),	    /* 10.4 - 10.5 */
                         NULL }},
  { NULL }
};
#endif

static CGFloat macfont_antialias_threshold;

void
macfont_update_antialias_threshold (void)
{
  int threshold;
  Boolean valid_p;

  threshold =
    CFPreferencesGetAppIntegerValue (CFSTR ("AppleAntiAliasingThreshold"),
                                     kCFPreferencesCurrentApplication,
                                     &valid_p);
  if (valid_p)
    macfont_antialias_threshold = threshold;
}

static inline Lisp_Object
macfont_intern_prop_cfstring (CFStringRef cfstring)
{
  Lisp_Object string = cfstring_to_lisp_nodecode (cfstring);

  return font_intern_prop (SSDATA (string), SBYTES (string), 1);
}

static inline CFIndex
macfont_store_utf32char_to_unichars (UTF32Char c, UniChar *unichars)
{
  if (c < 0x10000)
    {
      unichars[0] = c;

      return 1;
    }
  else
    {
      c -= 0x10000;
      unichars[0] = (c >> 10) + 0xD800;
      unichars[1] = (c & 0x3FF) + 0xDC00;

      return 2;
    }
}

static Boolean
cfnumber_get_font_symbolic_traits_value (CFNumberRef number,
                                         CTFontSymbolicTraits *sym_traits)
{
  SInt64 sint64_value;

  /* Getting symbolic traits with kCFNumberSInt32Type is lossy on Mac
     OS X 10.6 when the value is greater than or equal to 1 << 31.  */
  if (CFNumberGetValue (number, kCFNumberSInt64Type, &sint64_value))
    {
      *sym_traits = (CTFontSymbolicTraits) sint64_value;

      return true;
    }

  return false;
}

static CGFloat
mac_font_descriptor_get_adjusted_weight (CTFontDescriptorRef desc, CGFloat val)
{
  long percent_val = lround (val * 100);

  if (percent_val == -40)
    {
      CTFontRef font = NULL;
      CFStringRef name =
	CTFontDescriptorCopyAttribute (desc, kCTFontNameAttribute);

      if (name)
	{
	  font = CTFontCreateWithName (name, 0, NULL);
	  CFRelease (name);
	}
      if (font)
	{
	  CFIndex weight = mac_font_get_weight (font);

	  /* Workaround for crash when displaying Oriya characters
	     with Arial Unicode MS on OS X 10.11.  */
	  if (weight == 5)
	    val = 0;
	  CFRelease (font);
	}
    }

  return val;
}

static void
macfont_store_descriptor_attributes (CTFontDescriptorRef desc,
                                     Lisp_Object spec_or_entity)
{
  CFStringRef str;
  CFDictionaryRef dict;
  CFNumberRef num;
  CGFloat floatval;

  str = CTFontDescriptorCopyAttribute (desc, kCTFontFamilyNameAttribute);
  if (str)
    {
      ASET (spec_or_entity, FONT_FAMILY_INDEX,
            macfont_intern_prop_cfstring (str));
      CFRelease (str);
    }
  dict = CTFontDescriptorCopyAttribute (desc, kCTFontTraitsAttribute);
  if (dict)
    {
      struct {
        enum font_property_index index;
        CFStringRef trait;
        CGPoint points[6];
	CGFloat (*adjust_func) (CTFontDescriptorRef, CGFloat);
      } numeric_traits[] =
          {{FONT_WEIGHT_INDEX, kCTFontWeightTrait,
            {{-0.4, 50},	/* light */
             {-0.24, 87.5},	/* (semi-light + normal) / 2 */
             {0, 100},		/* normal */
             {0.24, 140},	/* (semi-bold + normal) / 2 */
             {0.4, 200},	/* bold */
             {CGFLOAT_MAX, CGFLOAT_MAX}},
	    mac_font_descriptor_get_adjusted_weight},
           {FONT_SLANT_INDEX, kCTFontSlantTrait,
            {{0, 100}, {0.1, 200}, {CGFLOAT_MAX, CGFLOAT_MAX}}, NULL},
           {FONT_WIDTH_INDEX, kCTFontWidthTrait,
            {{0, 100}, {1, 200}, {CGFLOAT_MAX, CGFLOAT_MAX}}, NULL}};
      int i;

      for (i = 0; i < ARRAYELTS (numeric_traits); i++)
        {
          num = CFDictionaryGetValue (dict, numeric_traits[i].trait);
          if (num && CFNumberGetValue (num, kCFNumberCGFloatType, &floatval))
            {
              CGPoint *point = numeric_traits[i].points;

	      if (numeric_traits[i].adjust_func)
		floatval = (*numeric_traits[i].adjust_func) (desc, floatval);
              while (point->x < floatval)
                point++;
              if (point == numeric_traits[i].points)
                point++;
              else if (point->x == CGFLOAT_MAX)
                point--;
              floatval = (point - 1)->y + ((floatval - (point - 1)->x)
                                           * ((point->y - (point - 1)->y)
                                              / (point->x - (point - 1)->x)));
              FONT_SET_STYLE (spec_or_entity, numeric_traits[i].index,
                              make_number (lround (floatval)));
            }
        }

      num = CFDictionaryGetValue (dict, kCTFontSymbolicTrait);
      if (num)
        {
          CTFontSymbolicTraits sym_traits;
          int spacing;

          cfnumber_get_font_symbolic_traits_value (num, &sym_traits);
          spacing = (sym_traits & kCTFontTraitMonoSpace
                     ? FONT_SPACING_MONO : FONT_SPACING_PROPORTIONAL);
          ASET (spec_or_entity, FONT_SPACING_INDEX, make_number (spacing));
        }

      CFRelease (dict);
    }
  num = CTFontDescriptorCopyAttribute (desc, kCTFontSizeAttribute);
  if (num && CFNumberGetValue (num, kCFNumberCGFloatType, &floatval))
    ASET (spec_or_entity, FONT_SIZE_INDEX, make_number (floatval));
  else
    ASET (spec_or_entity, FONT_SIZE_INDEX, make_number (0));
  if (num)
    CFRelease (num);
}

static Lisp_Object
macfont_descriptor_entity (CTFontDescriptorRef desc, Lisp_Object extra,
                           CTFontSymbolicTraits synth_sym_traits)
{
  Lisp_Object entity;
  CFDictionaryRef dict;
  CTFontSymbolicTraits sym_traits = 0;
  CFStringRef name;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, Qmac_ct);
  ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  macfont_store_descriptor_attributes (desc, entity);

  dict = CTFontDescriptorCopyAttribute (desc, kCTFontTraitsAttribute);
  if (dict)
    {
      CFNumberRef num = CFDictionaryGetValue (dict, kCTFontSymbolicTrait);

      if (num)
        cfnumber_get_font_symbolic_traits_value (num, &sym_traits);
      CFRelease (dict);
    }
  if (EQ (AREF (entity, FONT_SIZE_INDEX), make_number (0)))
    ASET (entity, FONT_AVGWIDTH_INDEX, make_number (0));
  ASET (entity, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
  name = CTFontDescriptorCopyAttribute (desc, kCTFontNameAttribute);
  font_put_extra (entity, QCfont_entity,
                  make_save_ptr_int ((void *) name, sym_traits));
  if (synth_sym_traits & kCTFontTraitItalic)
    FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
                    make_number (FONT_SLANT_SYNTHETIC_ITALIC));
  if (synth_sym_traits & kCTFontTraitBold)
    FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
                    make_number (FONT_WEIGHT_SYNTHETIC_BOLD));
  if (synth_sym_traits & kCTFontTraitMonoSpace)
    ASET (entity, FONT_SPACING_INDEX,
          make_number (FONT_SPACING_SYNTHETIC_MONO));

  return entity;
}

/* Cache for font family name symbols vs CFStrings.  A value of nil
means the cache has been invalidated.  Otherwise the value is a Lisp
hash table whose keys are symbols and the value for a key is either
nil (no corresponding family name) or a Lisp save value wrapping the
corresponding family name in CFString.  */

static Lisp_Object macfont_family_cache;

static void
macfont_invalidate_family_cache (void)
{
  if (HASH_TABLE_P (macfont_family_cache))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (macfont_family_cache);
      ptrdiff_t i, size = HASH_TABLE_SIZE (h);

      for (i = 0; i < size; ++i)
	if (!NILP (HASH_HASH (h, i)))
	  {
	    Lisp_Object value = HASH_VALUE (h, i);

	    if (SAVE_VALUEP (value))
	      CFRelease (XSAVE_POINTER (value, 0));
	  }
      macfont_family_cache = Qnil;
    }
}

static bool
macfont_get_family_cache_if_present (Lisp_Object symbol, CFStringRef *string)
{
  if (HASH_TABLE_P (macfont_family_cache))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (macfont_family_cache);
      ptrdiff_t i = hash_lookup (h, symbol, NULL);

      if (i >= 0)
	{
	  Lisp_Object value = HASH_VALUE (h, i);

	  *string = SAVE_VALUEP (value) ? XSAVE_POINTER (value, 0) : NULL;

	  return true;
	}
    }

  return false;
}

static void
macfont_set_family_cache (Lisp_Object symbol, CFStringRef string)
{
  struct Lisp_Hash_Table *h;
  ptrdiff_t i;
  EMACS_UINT hash;
  Lisp_Object value;

  if (!HASH_TABLE_P (macfont_family_cache))
    macfont_family_cache = CALLN (Fmake_hash_table, QCtest, Qeq);

  h = XHASH_TABLE (macfont_family_cache);
  i = hash_lookup (h, symbol, &hash);
  value = string ? make_save_ptr ((void *) CFRetain (string)) : Qnil;
  if (i >= 0)
    {
      Lisp_Object old_value = HASH_VALUE (h, i);

      if (SAVE_VALUEP (old_value))
	CFRelease (XSAVE_POINTER (old_value, 0));
      set_hash_value_slot (h, i, value);
    }
  else
    hash_put (h, symbol, value, hash);
}

/* Cache of all the available font family names except "LastResort"
and those start with ".".  NULL means the cache has been invalidated.
Otherwise, the value is CFArray of CFStrings and the elements are
sorted in the canonical order (CTFontManagerCompareFontFamilyNames on
Mac OS X 10.6 and later).  */

static CFArrayRef macfont_available_families_cache = NULL;

static void
macfont_invalidate_available_families_cache (void)
{
  if (macfont_available_families_cache)
    {
      CFRelease (macfont_available_families_cache);
      macfont_available_families_cache = NULL;
    }
}

static void
macfont_handle_font_change_notification (CFNotificationCenterRef center,
					 void *observer,
					 CFStringRef name, const void *object,
					 CFDictionaryRef userInfo)
{
  macfont_invalidate_family_cache ();
  macfont_invalidate_available_families_cache ();
}

static void
macfont_init_font_change_handler (void)
{
  static bool initialized = false;

  if (initialized)
    return;

  initialized = true;
  CFNotificationCenterAddObserver
    (CFNotificationCenterGetLocalCenter (), NULL,
     macfont_handle_font_change_notification,
     kCTFontManagerRegisteredFontsChangedNotification,
     NULL, CFNotificationSuspensionBehaviorCoalesce);
}

static CFArrayRef
macfont_copy_available_families_cache (void)
{
  macfont_init_font_change_handler ();

  if (macfont_available_families_cache == NULL)
    macfont_available_families_cache = mac_font_create_available_families ();

  return (macfont_available_families_cache
	  ? CFRetain (macfont_available_families_cache) : NULL);
}

static CFStringRef
macfont_create_family_with_symbol (Lisp_Object symbol)
{
  CFStringRef result = NULL, family_name;
  CFDictionaryRef attributes = NULL;
  CTFontDescriptorRef pat_desc = NULL;

  if (macfont_get_family_cache_if_present (symbol, &result))
    return result ? CFRetain (result) : NULL;

  family_name = cfstring_create_with_string_noencode (SYMBOL_NAME (symbol));
  if (family_name)
    {
      attributes =
	CFDictionaryCreate (NULL,
			    (const void **) &kCTFontFamilyNameAttribute,
			    (const void **) &family_name, 1,
			    &kCFTypeDictionaryKeyCallBacks,
			    &kCFTypeDictionaryValueCallBacks);
      CFRelease (family_name);
    }
  if (attributes)
    {
      pat_desc = CTFontDescriptorCreateWithAttributes (attributes);
      CFRelease (attributes);
    }
  if (pat_desc)
    {
      CTFontDescriptorRef desc =
	CTFontDescriptorCreateMatchingFontDescriptor (pat_desc, NULL);

      if (desc)
	{
	  result =
	    CTFontDescriptorCopyAttribute (desc, kCTFontFamilyNameAttribute);
	  CFRelease (desc);
	}
      macfont_set_family_cache (symbol, result);
      CFRelease (pat_desc);
    }

  return result;
}

#define WIDTH_FRAC_BITS		(4)
#define WIDTH_FRAC_SCALE	(2 * ((1 << (WIDTH_FRAC_BITS - 1)) - 1))

struct macfont_metrics
{
  unsigned char lbearing_low, rbearing_low;
  signed lbearing_high : 4, rbearing_high : 4;
  unsigned char ascent_low, descent_low;
  signed ascent_high : 4, descent_high : 4;

  /* These two members are used for fixed-point representation of
     glyph width.  The `width_int' member is an integer that is
     closest to the width.  The `width_frac' member is the fractional
     adjustment representing a value in [-.5, .5], multiplied by
     WIDTH_FRAC_SCALE.  For synthetic monospace fonts, they represent
     the advance delta for centering instead of the glyph width.  */
  signed width_frac : WIDTH_FRAC_BITS, width_int : 16 - WIDTH_FRAC_BITS;
};

#define METRICS_VALUE(metrics, member)                          \
  (((metrics)->member##_high << 8) | (metrics)->member##_low)
#define METRICS_SET_VALUE(metrics, member, value)                   \
  do {short tmp = (value); (metrics)->member##_low = tmp & 0xff;    \
    (metrics)->member##_high = tmp >> 8;} while (0)

enum metrics_status
{
  METRICS_INVALID = -1,    /* metrics entry is invalid */
  METRICS_WIDTH_VALID = -2 /* width is valid but others are invalid */
};

#define METRICS_STATUS(metrics)                                         \
  (METRICS_VALUE (metrics, ascent) + METRICS_VALUE (metrics, descent))
#define METRICS_SET_STATUS(metrics, status)                     \
  do {METRICS_SET_VALUE (metrics, ascent, 0);                   \
    METRICS_SET_VALUE (metrics, descent, status);} while (0)

#define METRICS_NCOLS_PER_ROW	(128)
#define LCD_FONT_SMOOTHING_LEFT_MARGIN	(0.396f)
#define LCD_FONT_SMOOTHING_RIGHT_MARGIN	(0.396f)

static int
macfont_glyph_extents (struct font *font, CGGlyph glyph,
                       struct font_metrics *metrics, CGFloat *advance_delta,
                       int force_integral_p)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;
  int row, col;
  struct macfont_metrics *cache;
  int width;

  row = glyph / METRICS_NCOLS_PER_ROW;
  col = glyph % METRICS_NCOLS_PER_ROW;
  if (row >= macfont_info->metrics_nrows)
    {
      macfont_info->metrics =
        xrealloc (macfont_info->metrics,
                  sizeof (struct macfont_metrics *) * (row + 1));
      memset (macfont_info->metrics + macfont_info->metrics_nrows, 0,
              (sizeof (struct macfont_metrics *)
               * (row + 1 - macfont_info->metrics_nrows)));
      macfont_info->metrics_nrows = row + 1;
    }
  if (macfont_info->metrics[row] == NULL)
    {
      struct macfont_metrics *new;
      int i;

      new = xmalloc (sizeof (struct macfont_metrics) * METRICS_NCOLS_PER_ROW);
      for (i = 0; i < METRICS_NCOLS_PER_ROW; i++)
        METRICS_SET_STATUS (new + i, METRICS_INVALID);
      macfont_info->metrics[row] = new;
    }
  cache = macfont_info->metrics[row] + col;

  if (METRICS_STATUS (cache) == METRICS_INVALID)
    {
      CGFloat fwidth;

      if (macfont_info->screen_font)
        fwidth = mac_screen_font_get_advance_width_for_glyph (macfont_info->screen_font, glyph);
      else
        fwidth = mac_font_get_advance_width_for_glyph (macfont, glyph);

      /* For synthetic mono fonts, cache->width_{int,frac} holds the
         advance delta value.  */
      if (macfont_info->spacing == MACFONT_SPACING_SYNTHETIC_MONO)
        fwidth = (font->pixel_size - fwidth) / 2;
      cache->width_int = lround (fwidth);
      cache->width_frac = lround ((fwidth - cache->width_int)
                                  * WIDTH_FRAC_SCALE);
      METRICS_SET_STATUS (cache, METRICS_WIDTH_VALID);
    }
  if (macfont_info->spacing == MACFONT_SPACING_SYNTHETIC_MONO)
    width = font->pixel_size;
  else
    width = cache->width_int;

  if (metrics)
    {
      if (METRICS_STATUS (cache) == METRICS_WIDTH_VALID)
        {
          CGRect bounds = mac_font_get_bounding_rect_for_glyph (macfont, glyph);

          if (macfont_info->synthetic_italic_p)
            {
              /* We assume the members a, b, c, and d in
                 synthetic_italic_atfm are non-negative.  */
              bounds.origin =
                CGPointApplyAffineTransform (bounds.origin,
                                             synthetic_italic_atfm);
              bounds.size =
                CGSizeApplyAffineTransform (bounds.size, synthetic_italic_atfm);
            }
          if (macfont_info->synthetic_bold_p && ! force_integral_p)
            {
              CGFloat d = - synthetic_bold_factor * CTFontGetSize (macfont) / 2;

	      bounds = CGRectInset (bounds, d, d);
            }
          switch (macfont_info->spacing)
            {
            case MACFONT_SPACING_PROPORTIONAL:
              bounds.origin.x += - (cache->width_frac
                                    / (CGFloat) (WIDTH_FRAC_SCALE * 2));
              break;
            case MACFONT_SPACING_MONO:
              break;
            case MACFONT_SPACING_SYNTHETIC_MONO:
              bounds.origin.x += (cache->width_int
                                  + (cache->width_frac
                                     / (CGFloat) WIDTH_FRAC_SCALE));
              break;
            }
          if (bounds.size.width > 0)
            {
              bounds.origin.x -= LCD_FONT_SMOOTHING_LEFT_MARGIN;
              bounds.size.width += (LCD_FONT_SMOOTHING_LEFT_MARGIN
                                    + LCD_FONT_SMOOTHING_RIGHT_MARGIN);
            }
          bounds = CGRectIntegral (bounds);
          METRICS_SET_VALUE (cache, lbearing, CGRectGetMinX (bounds));
          METRICS_SET_VALUE (cache, rbearing, CGRectGetMaxX (bounds));
          METRICS_SET_VALUE (cache, ascent, CGRectGetMaxY (bounds));
          METRICS_SET_VALUE (cache, descent, -CGRectGetMinY (bounds));
        }
      metrics->lbearing = METRICS_VALUE (cache, lbearing);
      metrics->rbearing = METRICS_VALUE (cache, rbearing);
      metrics->width = width;
      metrics->ascent = METRICS_VALUE (cache, ascent);
      metrics->descent = METRICS_VALUE (cache, descent);
    }

  if (advance_delta)
    {
      switch (macfont_info->spacing)
        {
        case MACFONT_SPACING_PROPORTIONAL:
          *advance_delta = (force_integral_p ? 0
                            : - (cache->width_frac
                                 / (CGFloat) (WIDTH_FRAC_SCALE * 2)));
          break;
        case MACFONT_SPACING_MONO:
          *advance_delta = 0;
          break;
        case MACFONT_SPACING_SYNTHETIC_MONO:
          *advance_delta = (force_integral_p ? cache->width_int
                            : (cache->width_int
                               + (cache->width_frac
                                  / (CGFloat) WIDTH_FRAC_SCALE)));
          break;
        }
    }

  return width;
}

static CFMutableDictionaryRef macfont_cache_dictionary;

/* Threshold used in row_nkeys_or_perm.  This must be less than or
   equal to the number of rows that are invalid as BMP (i.e., from
   U+D800 to U+DFFF).  */
#define ROW_PERM_OFFSET	(8)

/* The number of glyphs that can be stored in a value for a single
   entry of CFDictionary.  */
#define NGLYPHS_IN_VALUE (sizeof (void *) / sizeof (CGGlyph))

struct macfont_cache
{
  int reference_count;
  CFCharacterSetRef cf_charset;
  struct {
    /* The cached glyph for a BMP character c is stored in
       matrix[row_nkeys_or_perm[c / 256] - ROW_PERM_OFFSET][c % 256]
       if row_nkeys_or_perm[c / 256] >= ROW_PERM_OFFSET.  */
    unsigned char row_nkeys_or_perm[256];
    CGGlyph **matrix;

    /* Number of rows for which the BMP cache is allocated so far.
       I.e., matrix[0] ... matrix[nrows - 1] are non-NULL.  */
    int nrows;

    /* The cached glyph for a character c is stored as the (c %
       NGLYPHS_IN_VALUE)-th CGGlyph block of a value for the key (c /
       NGLYPHS_IN_VALUE).  However, the glyph for a BMP character c is
       not stored here if row_nkeys_or_perm[c / 256] >=
       ROW_PERM_OFFSET.  */
    CFMutableDictionaryRef dictionary;
  } glyph;

  struct {
    /* UVS (Unicode Variation Sequence) subtable data, which is of
       type CFDataRef if available.  NULL means it is not initialized
       yet.  kCFNull means the subtable is not found and there is no
       suitable fallback table for this font.  */
    CFTypeRef table;

    /* Character collection specifying the destination of the mapping
       provided by `table' above.  If `table' is obtained from the UVS
       subtable in the font cmap table, then the value of this member
       should be NSIdentityMappingCharacterCollection.  */
    NSCharacterCollection collection;
  } uvs;
};

static struct macfont_cache *macfont_lookup_cache (CFStringRef);
static struct macfont_cache *macfont_retain_cache (struct macfont_cache *);
static void macfont_release_cache (struct macfont_cache *);
static CFCharacterSetRef macfont_get_cf_charset (struct font *);
static CFCharacterSetRef macfont_get_cf_charset_for_name (CFStringRef);
static CGGlyph macfont_get_glyph_for_character (struct font *, UTF32Char);
static CGGlyph macfont_get_glyph_for_cid (struct font *font,
                                          NSCharacterCollection, CGFontIndex);
static CFDataRef macfont_get_uvs_table (struct font *, NSCharacterCollection *);

static struct macfont_cache *
macfont_lookup_cache (CFStringRef key)
{
  struct macfont_cache *cache;

  if (macfont_cache_dictionary == NULL)
    {
      macfont_cache_dictionary =
        CFDictionaryCreateMutable (NULL, 0,
                                   &kCFTypeDictionaryKeyCallBacks, NULL);
      cache = NULL;
    }
  else
    cache = ((struct macfont_cache *)
             CFDictionaryGetValue (macfont_cache_dictionary, key));

  if (cache == NULL)
    {
      CTFontRef macfont = CTFontCreateWithName (key, 0, NULL);

      if (macfont)
        {
          cache = xzalloc (sizeof (struct macfont_cache));
          /* Treat the LastResort font as if it contained glyphs for
             all characters.  This may look too rough, but neither
             CTFontCopyCharacterSet nor -[NSFont coveredCharacterSet]
             for this font is correct for non-BMP characters on Mac OS
             X 10.5, anyway.  */
          if (CFEqual (key, CFSTR ("LastResort")))
            {
              CFRange range = CFRangeMake (0, MAX_UNICODE_CHAR + 1);

              cache->cf_charset =
                CFCharacterSetCreateWithCharactersInRange (NULL, range);
            }
          if (cache->cf_charset == NULL)
            cache->cf_charset = CTFontCopyCharacterSet (macfont);
          CFDictionaryAddValue (macfont_cache_dictionary, key,
                                (const void *) cache);
          CFRelease (macfont);
        }
    }

  return cache;
}

static struct macfont_cache *
macfont_retain_cache (struct macfont_cache *cache)
{
  cache->reference_count++;

  return cache;
}

static void
macfont_release_cache (struct macfont_cache *cache)
{
  if (--cache->reference_count == 0)
    {
      int i;

      for (i = 0; i < cache->glyph.nrows; i++)
        xfree (cache->glyph.matrix[i]);
      xfree (cache->glyph.matrix);
      if (cache->glyph.dictionary)
        CFRelease (cache->glyph.dictionary);
      memset (&cache->glyph, 0, sizeof (cache->glyph));
      if (cache->uvs.table)
        CFRelease (cache->uvs.table);
      memset (&cache->uvs, 0, sizeof (cache->uvs));
    }
}

static CFCharacterSetRef
macfont_get_cf_charset (struct font *font)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;

  return macfont_info->cache->cf_charset;
}

static CFCharacterSetRef
macfont_get_cf_charset_for_name (CFStringRef name)
{
  struct macfont_cache *cache = macfont_lookup_cache (name);

  return cache->cf_charset;
}

static CGGlyph
macfont_get_glyph_for_character (struct font *font, UTF32Char c)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;
  struct macfont_cache *cache = macfont_info->cache;

  if (c < 0xD800 || (c > 0xDFFF && c < 0x10000))
    {
      int row = c / 256;
      int nkeys_or_perm = cache->glyph.row_nkeys_or_perm[row];

      if (nkeys_or_perm < ROW_PERM_OFFSET)
        {
          UniChar unichars[256], ch;
          CGGlyph *glyphs;
          int i, len;
          int nrows;
          int nkeys;

          if (row != 0)
            {
              CFMutableDictionaryRef dictionary;
              uintptr_t key, value;
              int nshifts;
              CGGlyph glyph;

              if (cache->glyph.dictionary == NULL)
                cache->glyph.dictionary =
                  CFDictionaryCreateMutable (NULL, 0, NULL, NULL);
              dictionary = cache->glyph.dictionary;
              key = c / NGLYPHS_IN_VALUE;
              nshifts = ((c % NGLYPHS_IN_VALUE) * sizeof (CGGlyph) * 8);
              value = ((uintptr_t)
                       CFDictionaryGetValue (dictionary, (const void *) key));
              glyph = (value >> nshifts);
              if (glyph)
                return glyph;

              if (nkeys_or_perm + 1 != ROW_PERM_OFFSET)
                {
                  ch = c;
                  if (!CTFontGetGlyphsForCharacters (macfont, &ch, &glyph, 1)
                      || glyph == 0)
                    glyph = kCGFontIndexInvalid;

                  if (value == 0)
                    cache->glyph.row_nkeys_or_perm[row] = nkeys_or_perm + 1;
                  value |= ((uintptr_t) glyph << nshifts);
                  CFDictionarySetValue (dictionary, (const void *) key,
                                        (const void *) value);

                  return glyph;
                }

              nkeys = nkeys_or_perm;
              for (key = row * (256 / NGLYPHS_IN_VALUE); ; key++)
                if (CFDictionaryContainsKey (dictionary,
                                             (const void *) key))
                  {
                    CFDictionaryRemoveValue (dictionary,
                                             (const void *) key);
                    if (--nkeys == 0)
                      break;
                  }
            }

          len = 0;
          for (i = 0; i < 256; i++)
            {
              ch = row * 256 + i;
              if (CFCharacterSetIsLongCharacterMember (cache->cf_charset, ch))
                unichars[len++] = ch;
            }

          glyphs = xmalloc (sizeof (CGGlyph) * 256);
          if (len > 0)
            {
              CTFontGetGlyphsForCharacters (macfont, unichars, glyphs, len);
              while (i > len)
                {
                  int next = unichars[len - 1] % 256;

                  while (--i > next)
                    glyphs[i] = kCGFontIndexInvalid;

                  len--;
                  glyphs[i] = glyphs[len];
                  if (len == 0)
                    break;
                }
            }
          if (i > len)
            while (i-- > 0)
              glyphs[i] = kCGFontIndexInvalid;

          nrows = cache->glyph.nrows;
          nkeys_or_perm = nrows + ROW_PERM_OFFSET;
          cache->glyph.row_nkeys_or_perm[row] = nkeys_or_perm;
          nrows++;
          cache->glyph.matrix = xrealloc (cache->glyph.matrix,
                                          sizeof (CGGlyph *) * nrows);
          cache->glyph.matrix[nrows - 1] = glyphs;
          cache->glyph.nrows = nrows;
        }

      return cache->glyph.matrix[nkeys_or_perm - ROW_PERM_OFFSET][c % 256];
    }
  else
    {
      uintptr_t key, value;
      int nshifts;
      CGGlyph glyph;

      if (cache->glyph.dictionary == NULL)
        cache->glyph.dictionary =
          CFDictionaryCreateMutable (NULL, 0, NULL, NULL);
      key = c / NGLYPHS_IN_VALUE;
      nshifts = ((c % NGLYPHS_IN_VALUE) * sizeof (CGGlyph) * 8);
      value = (uintptr_t) CFDictionaryGetValue (cache->glyph.dictionary,
                                                (const void *) key);
      glyph = (value >> nshifts);
      if (glyph == 0)
        {
          UniChar unichars[2];
          CGGlyph glyphs[2];
          CFIndex count = macfont_store_utf32char_to_unichars (c, unichars);

          if (CTFontGetGlyphsForCharacters (macfont, unichars, glyphs, count))
            glyph = glyphs[0];
          if (glyph == 0)
            glyph = kCGFontIndexInvalid;

          value |= ((uintptr_t) glyph << nshifts);
          CFDictionarySetValue (cache->glyph.dictionary,
                                (const void *) key, (const void *) value);
        }

      return glyph;
    }
}

static CGGlyph
macfont_get_glyph_for_cid (struct font *font, NSCharacterCollection collection,
                           CGFontIndex cid)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;

  /* Cache it? */
  return mac_font_get_glyph_for_cid (macfont, collection, cid);
}

static CFDataRef
macfont_get_uvs_table (struct font *font, NSCharacterCollection *collection)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;
  struct macfont_cache *cache = macfont_info->cache;
  CFDataRef result = NULL;

  if (cache->uvs.table == NULL)
    {
      CFDataRef uvs_table = mac_font_copy_uvs_table (macfont);
      NSCharacterCollection uvs_collection =
        NSIdentityMappingCharacterCollection;

      if (uvs_table == NULL
          && mac_font_get_glyph_for_cid (macfont,
                                         NSAdobeJapan1CharacterCollection,
                                         6480) != kCGFontIndexInvalid)
        {
          /* If the glyph for U+4E55 is accessible via its CID 6480,
             then we use the Adobe-Japan1 UVS table, which maps a
             variation sequence to a CID, as a fallback.  */
          static CFDataRef mac_uvs_table_adobe_japan1 = NULL;

          if (mac_uvs_table_adobe_japan1 == NULL)
            mac_uvs_table_adobe_japan1 =
              CFDataCreateWithBytesNoCopy (NULL,
                                           mac_uvs_table_adobe_japan1_bytes,
                                           sizeof (mac_uvs_table_adobe_japan1_bytes),
                                           kCFAllocatorNull);
          if (mac_uvs_table_adobe_japan1)
            {
              uvs_table = CFRetain (mac_uvs_table_adobe_japan1);
              uvs_collection = NSAdobeJapan1CharacterCollection;
            }
        }
      if (uvs_table == NULL)
        cache->uvs.table = kCFNull;
      else
        cache->uvs.table = uvs_table;
      cache->uvs.collection = uvs_collection;
    }

  if (cache->uvs.table != kCFNull)
    {
      result = cache->uvs.table;
      *collection = cache->uvs.collection;
    }

  return result;
}

static Lisp_Object macfont_get_cache (struct frame *);
static Lisp_Object macfont_list (struct frame *, Lisp_Object);
static Lisp_Object macfont_match (struct frame *, Lisp_Object);
static Lisp_Object macfont_list_family (struct frame *);
static void macfont_free_entity (Lisp_Object);
static Lisp_Object macfont_open (struct frame *, Lisp_Object, int);
static void macfont_close (struct font *);
static int macfont_has_char (Lisp_Object, int);
static unsigned macfont_encode_char (struct font *, int);
static void macfont_text_extents (struct font *, unsigned int *, int,
                                  struct font_metrics *);
static int macfont_draw (struct glyph_string *, int, int, int, int, bool);
static Lisp_Object macfont_shape (Lisp_Object);
static int macfont_variation_glyphs (struct font *, int c,
                                     unsigned variations[256]);
static void macfont_filter_properties (Lisp_Object, Lisp_Object);

static struct font_driver const macfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qmac_ct),
  .get_cache = macfont_get_cache,
  .list = macfont_list,
  .match = macfont_match,
  .list_family = macfont_list_family,
  .free_entity = macfont_free_entity,
  .open = macfont_open,
  .close = macfont_close,
  .has_char = macfont_has_char,
  .encode_char = macfont_encode_char,
  .text_extents = macfont_text_extents,
  .draw = macfont_draw,
  .shape = macfont_shape,
  .get_variation_glyphs = macfont_variation_glyphs,
  .filter_properties = macfont_filter_properties,
  };

static Lisp_Object
macfont_get_cache (struct frame * f)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);

  return (dpyinfo->name_list_element);
}

static int
macfont_get_charset (Lisp_Object registry)
{
  char *str = SSDATA (SYMBOL_NAME (registry));
  char *re = alloca (SBYTES (SYMBOL_NAME (registry)) * 2 + 1);
  Lisp_Object regexp;
  int i, j;

  for (i = j = 0; i < SBYTES (SYMBOL_NAME (registry)); i++, j++)
    {
      if (str[i] == '.')
        re[j++] = '\\';
      else if (str[i] == '*')
        re[j++] = '.';
      re[j] = str[i];
      if (re[j] == '?')
        re[j] = '.';
    }
  re[j] = '\0';
  regexp = make_unibyte_string (re, j);
  for (i = 0; cf_charset_table[i].name; i++)
    if (fast_c_string_match_ignore_case
        (regexp, cf_charset_table[i].name,
         strlen (cf_charset_table[i].name)) >= 0)
      break;
  if (! cf_charset_table[i].name)
    return -1;
  if (! cf_charset_table[i].cf_charset)
    {
      int *uniquifier = cf_charset_table[i].uniquifier;
      UniChar *unichars = alloca (sizeof (cf_charset_table[i].uniquifier));
      CFIndex count = 0;
      CFStringRef string;
      CFMutableCharacterSetRef charset = CFCharacterSetCreateMutable (NULL);

      if (! charset)
        return -1;
      for (j = 0; uniquifier[j]; j++)
        {
          count += macfont_store_utf32char_to_unichars (uniquifier[j],
                                                        unichars + count);
          CFCharacterSetAddCharactersInRange (charset,
                                              CFRangeMake (uniquifier[j], 1));
        }

      string = CFStringCreateWithCharacters (NULL, unichars, count);
      if (! string)
        {
          CFRelease (charset);
          return -1;
        }
      cf_charset_table[i].cf_charset = CFCharacterSetCreateCopy (NULL,
                                                                 charset);
      CFRelease (charset);
      /* CFCharacterSetCreateWithCharactersInString does not handle
         surrogate pairs properly as of Mac OS X 10.5.  */
      cf_charset_table[i].cf_charset_string = string;
    }
  return i;
}

struct OpenTypeSpec
{
  Lisp_Object script;
  unsigned int script_tag, langsys_tag;
  int nfeatures[2];
  unsigned int *features[2];
};

#define OTF_SYM_TAG(SYM, TAG)                               \
  do {                                                      \
    unsigned char *p = SDATA (SYMBOL_NAME (SYM));           \
    TAG = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];	\
  } while (0)

#define OTF_TAG_STR(TAG, P)                     \
  do {                                          \
    (P)[0] = (char) (TAG >> 24);                \
    (P)[1] = (char) ((TAG >> 16) & 0xFF);       \
    (P)[2] = (char) ((TAG >> 8) & 0xFF);        \
    (P)[3] = (char) (TAG & 0xFF);               \
    (P)[4] = '\0';                              \
  } while (0)

static struct OpenTypeSpec *
macfont_get_open_type_spec (Lisp_Object otf_spec)
{
  struct OpenTypeSpec *spec = xmalloc (sizeof *spec);
  Lisp_Object val;
  int i, j;
  bool negative;

  if (! spec)
    return NULL;
  spec->script = XCAR (otf_spec);
  if (! NILP (spec->script))
    {
      OTF_SYM_TAG (spec->script, spec->script_tag);
      val = assq_no_quit (spec->script, Votf_script_alist);
      if (CONSP (val) && SYMBOLP (XCDR (val)))
        spec->script = XCDR (val);
      else
        spec->script = Qnil;
    }
  else
    spec->script_tag = 0x44464C54; 	/* "DFLT" */
  otf_spec = XCDR (otf_spec);
  spec->langsys_tag = 0;
  if (! NILP (otf_spec))
    {
      val = XCAR (otf_spec);
      if (! NILP (val))
        OTF_SYM_TAG (val, spec->langsys_tag);
      otf_spec = XCDR (otf_spec);
    }
  spec->nfeatures[0] = spec->nfeatures[1] = 0;
  for (i = 0; i < 2 && ! NILP (otf_spec); i++, otf_spec = XCDR (otf_spec))
    {
      Lisp_Object len;

      val = XCAR (otf_spec);
      if (NILP (val))
        continue;
      len = Flength (val);
      spec->features[i] =
        (min (PTRDIFF_MAX, SIZE_MAX) / sizeof (int) < XINT (len)
         ? 0
         : malloc (XINT (len) * sizeof *spec->features[i]));
      if (! spec->features[i])
        {
          if (i > 0 && spec->features[0])
            free (spec->features[0]);
          free (spec);
          return NULL;
        }
      for (j = 0, negative = 0; CONSP (val); val = XCDR (val))
        {
          if (NILP (XCAR (val)))
            negative = 1;
          else
            {
              unsigned int tag;

              OTF_SYM_TAG (XCAR (val), tag);
              spec->features[i][j++] = negative ? tag & 0x80000000 : tag;
            }
        }
      spec->nfeatures[i] = j;
    }
  return spec;
}

static CFMutableDictionaryRef
macfont_create_attributes_with_spec (Lisp_Object spec)
{
  Lisp_Object tmp, extra;
  CFMutableArrayRef langarray = NULL;
  CFCharacterSetRef charset = NULL;
  CFStringRef charset_string = NULL;
  CFMutableDictionaryRef attributes = NULL, traits = NULL;
  Lisp_Object script = Qnil;
  Lisp_Object registry;
  int cf_charset_idx, i;
  struct OpenTypeSpec *otspec = NULL;
  struct {
    enum font_property_index index;
    CFStringRef trait;
    CGPoint points[6];
  } numeric_traits[] =
      {{FONT_WEIGHT_INDEX, kCTFontWeightTrait,
        {{-0.4, 50},		/* light */
         {-0.24, 87.5},		/* (semi-light + normal) / 2 */
         {0, 100},		/* normal */
         {0.24, 140},		/* (semi-bold + normal) / 2 */
         {0.4, 200},		/* bold */
         {CGFLOAT_MAX, CGFLOAT_MAX}}},
       {FONT_SLANT_INDEX, kCTFontSlantTrait,
        {{0, 100}, {0.1, 200}, {CGFLOAT_MAX, CGFLOAT_MAX}}},
       {FONT_WIDTH_INDEX, kCTFontWidthTrait,
        {{0, 100}, {1, 200}, {CGFLOAT_MAX, CGFLOAT_MAX}}}};

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry)
      || EQ (registry, Qascii_0)
      || EQ (registry, Qiso10646_1)
      || EQ (registry, Qunicode_bmp))
    cf_charset_idx = -1;
  else
    {
      CFStringRef lang;

      cf_charset_idx = macfont_get_charset (registry);
      if (cf_charset_idx < 0)
        goto err;
      charset = cf_charset_table[cf_charset_idx].cf_charset;
      charset_string = cf_charset_table[cf_charset_idx].cf_charset_string;
      lang = cf_charset_table[cf_charset_idx].lang;
      if (lang)
        {
          langarray = CFArrayCreateMutable (NULL, 0, &kCFTypeArrayCallBacks);
          if (! langarray)
            goto err;
          CFArrayAppendValue (langarray, lang);
        }
    }

  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      tmp = XCAR (extra);
      key = XCAR (tmp), val = XCDR (tmp);
      if (EQ (key, QClang))
        {
          if (! langarray)
            langarray = CFArrayCreateMutable (NULL, 0, &kCFTypeArrayCallBacks);
          if (! langarray)
            goto err;
          if (SYMBOLP (val))
            val = list1 (val);
          for (; CONSP (val); val = XCDR (val))
            if (SYMBOLP (XCAR (val)))
              {
                CFStringRef lang =
                  cfstring_create_with_string_noencode (SYMBOL_NAME
                                                        (XCAR (val)));

                if (lang == NULL)
                  goto err;
                CFArrayAppendValue (langarray, lang);
                CFRelease (lang);
              }
        }
      else if (EQ (key, QCotf))
        {
          otspec = macfont_get_open_type_spec (val);
          if (! otspec)
            goto err;
          script = otspec->script;
        }
      else if (EQ (key, QCscript))
        script = val;
    }

  if (! NILP (script) && ! charset)
    {
      Lisp_Object chars = assq_no_quit (script, Vscript_representative_chars);

      if (CONSP (chars) && CONSP (CDR (chars)))
        {
          CFMutableStringRef string = CFStringCreateMutable (NULL, 0);
          CFMutableCharacterSetRef cs = CFCharacterSetCreateMutable (NULL);

          if (! string || !cs)
            {
              if (string)
                CFRelease (string);
              else if (cs)
                CFRelease (cs);
              goto err;
            }
          for (chars = XCDR (chars); CONSP (chars); chars = XCDR (chars))
            if (CHARACTERP (XCAR (chars)))
              {
                UniChar unichars[2];
                CFIndex count =
                  macfont_store_utf32char_to_unichars (XFASTINT (XCAR (chars)),
                                                       unichars);
                CFRange range = CFRangeMake (XFASTINT (XCAR (chars)), 1);

                CFStringAppendCharacters (string, unichars, count);
                CFCharacterSetAddCharactersInRange (cs, range);
              }
          charset = cs;
          /* CFCharacterSetCreateWithCharactersInString does not
             handle surrogate pairs properly as of Mac OS X 10.5.  */
          charset_string = string;
        }
    }

  attributes = CFDictionaryCreateMutable (NULL, 0,
                                          &kCFTypeDictionaryKeyCallBacks,
                                          &kCFTypeDictionaryValueCallBacks);
  if (! attributes)
    goto err;

  tmp = AREF (spec, FONT_FAMILY_INDEX);
  if (SYMBOLP (tmp) && ! NILP (tmp))
    {
      CFStringRef family = macfont_create_family_with_symbol (tmp);

      if (! family)
        goto err;
      CFDictionaryAddValue (attributes, kCTFontFamilyNameAttribute,
                            family);
      CFRelease (family);
    }

  traits = CFDictionaryCreateMutable (NULL, 4,
                                      &kCFTypeDictionaryKeyCallBacks,
                                      &kCFTypeDictionaryValueCallBacks);
  if (! traits)
    goto err;

  for (i = 0; i < ARRAYELTS (numeric_traits); i++)
    {
      tmp = AREF (spec, numeric_traits[i].index);
      if (INTEGERP (tmp))
        {
          CGPoint *point = numeric_traits[i].points;
          CGFloat floatval = (XINT (tmp) >> 8); // XXX
          CFNumberRef num;

          while (point->y < floatval)
            point++;
          if (point == numeric_traits[i].points)
            point++;
          else if (point->y == CGFLOAT_MAX)
            point--;
          floatval = (point - 1)->x + ((floatval - (point - 1)->y)
                                       * ((point->x - (point - 1)->x)
                                          / (point->y - (point - 1)->y)));
          if (floatval > 1.0)
            floatval = 1.0;
          else if (floatval < -1.0)
            floatval = -1.0;
          num = CFNumberCreate (NULL, kCFNumberCGFloatType, &floatval);
          if (! num)
            goto err;
          CFDictionaryAddValue (traits, numeric_traits[i].trait, num);
          CFRelease (num);
        }
    }
  if (CFDictionaryGetCount (traits))
    CFDictionaryAddValue (attributes, kCTFontTraitsAttribute, traits);

  if (charset)
    CFDictionaryAddValue (attributes, kCTFontCharacterSetAttribute,
                          charset);
  if (charset_string)
    CFDictionaryAddValue (attributes, MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE,
                          charset_string);
  if (langarray)
    CFDictionaryAddValue (attributes, kCTFontLanguagesAttribute, langarray);

  goto finish;

 err:
  if (attributes)
    {
      CFRelease (attributes);
      attributes = NULL;
    }

 finish:
  if (langarray) CFRelease (langarray);
  if (charset && cf_charset_idx < 0) CFRelease (charset);
  if (charset_string && cf_charset_idx < 0) CFRelease (charset_string);
  if (traits) CFRelease (traits);
  if (otspec)
    {
      if (otspec->nfeatures[0] > 0)
        free (otspec->features[0]);
      if (otspec->nfeatures[1] > 0)
        free (otspec->features[1]);
      free (otspec);
    }

  return attributes;
}

static Boolean
macfont_supports_charset_and_languages_p (CTFontDescriptorRef desc,
                                          CFCharacterSetRef charset,
                                          Lisp_Object chars,
                                          CFArrayRef languages)
{
  Boolean result = true;

  if (charset || VECTORP (chars))
    {
      CFCharacterSetRef desc_charset =
        CTFontDescriptorCopyAttribute (desc, kCTFontCharacterSetAttribute);

      if (desc_charset == NULL)
        result = false;
      else
        {
          if (charset)
            result = CFCharacterSetIsSupersetOfSet (desc_charset, charset);
          else 			/* VECTORP (chars) */
            {
              ptrdiff_t j;

              for (j = 0; j < ASIZE (chars); j++)
                if (TYPE_RANGED_INTEGERP (UTF32Char, AREF (chars, j))
                    && CFCharacterSetIsLongCharacterMember (desc_charset,
                                                            XFASTINT (AREF (chars, j))))
                  break;
              if (j == ASIZE (chars))
                result = false;
            }
          CFRelease (desc_charset);
        }
    }
  if (result && languages)
    result = mac_font_descriptor_supports_languages (desc, languages);

  return result;
}

static int
macfont_traits_distance (CTFontSymbolicTraits sym_traits1,
                         CTFontSymbolicTraits sym_traits2)
{
  CTFontSymbolicTraits diff = (sym_traits1 ^ sym_traits2);
  int distance = 0;

  /* We prefer synthetic bold of italic to synthetic italic of bold
     when both bold and italic are available but bold-italic is not
     available.  */
  if (diff & kCTFontTraitBold)
    distance |= (1 << 0);
  if (diff & kCTFontTraitItalic)
    distance |= (1 << 1);
  if (diff & kCTFontTraitMonoSpace)
    distance |= (1 << 2);

  return distance;
}

static Boolean
macfont_closest_traits_index_p (CFArrayRef traits_array,
                                CTFontSymbolicTraits target,
                                CFIndex index)
{
  CFIndex i, count = CFArrayGetCount (traits_array);
  CTFontSymbolicTraits traits;
  int my_distance;

  traits = ((CTFontSymbolicTraits) (uintptr_t)
            CFArrayGetValueAtIndex (traits_array, index));
  my_distance = macfont_traits_distance (target, traits);

  for (i = 0; i < count; i++)
    if (i != index)
      {
        traits = ((CTFontSymbolicTraits) (uintptr_t)
                  CFArrayGetValueAtIndex (traits_array, i));
        if (macfont_traits_distance (target, traits) < my_distance)
          return false;
      }

  return true;
}

static Lisp_Object
macfont_list (struct frame *f, Lisp_Object spec)
{
  Lisp_Object val = Qnil, family, extra;
  int i, n;
  CFStringRef family_name = NULL;
  CFMutableDictionaryRef attributes = NULL, traits;
  Lisp_Object chars = Qnil;
  int spacing = -1;
  CTFontSymbolicTraits synth_sym_traits = 0;
  CFArrayRef families;
  CFIndex families_count;
  CFCharacterSetRef charset = NULL;
  CFArrayRef languages = NULL;

  block_input ();

  family = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      family_name = macfont_create_family_with_symbol (family);
      if (family_name == NULL)
        goto finish;
    }

  attributes = macfont_create_attributes_with_spec (spec);
  if (! attributes)
    goto finish;

  languages = CFDictionaryGetValue (attributes, kCTFontLanguagesAttribute);

  if (INTEGERP (AREF (spec, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (spec, FONT_SPACING_INDEX));

  traits = ((CFMutableDictionaryRef)
            CFDictionaryGetValue (attributes, kCTFontTraitsAttribute));

  n = FONT_SLANT_NUMERIC (spec);
  if (n < 0 || n == FONT_SLANT_SYNTHETIC_ITALIC)
    {
      synth_sym_traits |= kCTFontTraitItalic;
      if (traits)
        CFDictionaryRemoveValue (traits, kCTFontSlantTrait);
    }

  n = FONT_WEIGHT_NUMERIC (spec);
  if (n < 0 || n == FONT_WEIGHT_SYNTHETIC_BOLD)
    {
      synth_sym_traits |= kCTFontTraitBold;
      if (traits)
        CFDictionaryRemoveValue (traits, kCTFontWeightTrait);
    }

  if (languages
      && (spacing < 0 || spacing == FONT_SPACING_SYNTHETIC_MONO))
    {
      CFStringRef language = CFArrayGetValueAtIndex (languages, 0);

      if (CFStringHasPrefix (language, CFSTR ("ja"))
          || CFStringHasPrefix (language, CFSTR ("ko"))
          || CFStringHasPrefix (language, CFSTR ("zh")))
        synth_sym_traits |= kCTFontTraitMonoSpace;
    }

  /* Create array of families.  */
  if (family_name)
    families = CFArrayCreate (NULL, (const void **) &family_name,
                              1, &kCFTypeArrayCallBacks);
  else
    {
      CFStringRef pref_family;
      CFIndex families_count, pref_family_index = -1;

      families = macfont_copy_available_families_cache ();
      if (families == NULL)
        goto err;

      families_count = CFArrayGetCount (families);

      /* Move preferred family to the front if exists.  */
      pref_family =
        mac_font_create_preferred_family_for_attributes (attributes);
      if (pref_family)
        {
          pref_family_index =
            CFArrayGetFirstIndexOfValue (families,
                                         CFRangeMake (0, families_count),
                                         pref_family);
          CFRelease (pref_family);
        }
      if (pref_family_index > 0)
        {
          CFMutableArrayRef mutable_families =
            CFArrayCreateMutable (NULL, families_count, &kCFTypeArrayCallBacks);

          if (mutable_families)
            {
              CFArrayAppendValue (mutable_families,
                                  CFArrayGetValueAtIndex (families,
                                                          pref_family_index));
              CFArrayAppendArray (mutable_families, families,
                                  CFRangeMake (0, pref_family_index));
              if (pref_family_index + 1 < families_count)
                CFArrayAppendArray (mutable_families, families,
                                    CFRangeMake (pref_family_index + 1,
                                                 families_count
                                                 - (pref_family_index + 1)));
              CFRelease (families);
              families = mutable_families;
            }
        }
    }

  charset = CFDictionaryGetValue (attributes, kCTFontCharacterSetAttribute);
  if (charset)
    {
      CFRetain (charset);
      CFDictionaryRemoveValue (attributes, kCTFontCharacterSetAttribute);
    }
  else
    {
      val = assq_no_quit (QCscript, AREF (spec, FONT_EXTRA_INDEX));
      if (! NILP (val))
        {
          val = assq_no_quit (XCDR (val), Vscript_representative_chars);
          if (CONSP (val) && VECTORP (XCDR (val)))
            chars = XCDR (val);
        }
      val = Qnil;
    }

  if (languages)
    {
      CFRetain (languages);
      CFDictionaryRemoveValue (attributes, kCTFontLanguagesAttribute);
    }

  val = Qnil;
  extra = AREF (spec, FONT_EXTRA_INDEX);
  families_count = CFArrayGetCount (families);
  for (i = 0; i < families_count; i++)
    {
      CFStringRef family_name = CFArrayGetValueAtIndex (families, i);
      CTFontDescriptorRef pat_desc;
      CFArrayRef descs;
      CFIndex descs_count;
      CFMutableArrayRef filtered_descs, traits_array;
      Lisp_Object entity;
      int j;

      CFDictionarySetValue (attributes, kCTFontFamilyNameAttribute,
                            family_name);
      pat_desc = CTFontDescriptorCreateWithAttributes (attributes);
      if (! pat_desc)
        goto err;

      /* CTFontDescriptorCreateMatchingFontDescriptors on Mac OS X
         10.7 returns NULL if pat_desc represents the LastResort font.
         So we use CTFontDescriptorCreateMatchingFontDescriptor (no
         trailing "s") for such a font.  */
      if (!CFEqual (family_name, CFSTR ("LastResort")))
        descs = CTFontDescriptorCreateMatchingFontDescriptors (pat_desc, NULL);
      else
        {
          CTFontDescriptorRef lr_desc =
            CTFontDescriptorCreateMatchingFontDescriptor (pat_desc, NULL);
          if (lr_desc)
            {
              descs = CFArrayCreate (NULL, (const void **) &lr_desc, 1,
                                     &kCFTypeArrayCallBacks);
              CFRelease (lr_desc);
            }
          else
            descs = NULL;
        }
      CFRelease (pat_desc);
      if (! descs)
        continue;

      descs_count = CFArrayGetCount (descs);
      if (descs_count == 0
          || !macfont_supports_charset_and_languages_p (CFArrayGetValueAtIndex (descs, 0),
                                                        charset, chars,
                                                        languages))
        {
          CFRelease (descs);
          continue;
        }

      filtered_descs =
        CFArrayCreateMutable (NULL, descs_count, &kCFTypeArrayCallBacks);
      traits_array = CFArrayCreateMutable (NULL, descs_count, NULL);
      for (j = 0; j < descs_count; j++)
        {
          CTFontDescriptorRef desc = CFArrayGetValueAtIndex (descs, j);
          CFDictionaryRef dict;
          CFNumberRef num;
          CTFontSymbolicTraits sym_traits;

          dict = CTFontDescriptorCopyAttribute (desc, kCTFontTraitsAttribute);
          if (dict == NULL)
            continue;

          num = CFDictionaryGetValue (dict, kCTFontSymbolicTrait);
          CFRelease (dict);
          if (num == NULL
              || !cfnumber_get_font_symbolic_traits_value (num, &sym_traits))
            continue;

          if (spacing >= 0
              && !(synth_sym_traits & kCTFontTraitMonoSpace)
              && (((sym_traits & kCTFontTraitMonoSpace) != 0)
                  != (spacing >= FONT_SPACING_MONO)))
            continue;

          /* Don't use a color bitmap font until it is supported on
	     free platforms.  */
          if (sym_traits & kCTFontTraitColorGlyphs)
            continue;

          if (j > 0
              && !macfont_supports_charset_and_languages_p (desc, charset,
                                                            chars, languages))
            continue;

          CFArrayAppendValue (filtered_descs, desc);
          CFArrayAppendValue (traits_array,
                              (const void *) (uintptr_t) sym_traits);
        }

      CFRelease (descs);
      descs = filtered_descs;
      descs_count = CFArrayGetCount (descs);

      for (j = 0; j < descs_count; j++)
        {
          CTFontDescriptorRef desc = CFArrayGetValueAtIndex (descs, j);
          CTFontSymbolicTraits sym_traits =
            ((CTFontSymbolicTraits) (uintptr_t)
             CFArrayGetValueAtIndex (traits_array, j));
          CTFontSymbolicTraits mask_min, mask_max, imask, bmask, mmask;

          mask_min = ((synth_sym_traits ^ sym_traits)
                      & (kCTFontTraitItalic | kCTFontTraitBold));
          if (FONT_SLANT_NUMERIC (spec) < 0)
            mask_min &= ~kCTFontTraitItalic;
          if (FONT_WEIGHT_NUMERIC (spec) < 0)
            mask_min &= ~kCTFontTraitBold;

          mask_max = (synth_sym_traits & ~sym_traits);
          /* Synthetic bold does not work for bitmap-only fonts on Mac
             OS X 10.6.  */
          if ((mask_min ^ mask_max) & kCTFontTraitBold)
            {
              CFNumberRef format =
                CTFontDescriptorCopyAttribute (desc, kCTFontFormatAttribute);

              if (format)
                {
                  uint32_t format_val;

                  if (CFNumberGetValue (format, kCFNumberSInt32Type,
                                        &format_val)
                      && format_val == kCTFontFormatBitmap)
                    mask_max &= ~kCTFontTraitBold;
                }
            }
          if (spacing >= 0)
            mask_min |= (mask_max & kCTFontTraitMonoSpace);

          for (mmask = (mask_min & kCTFontTraitMonoSpace);
               mmask <= (mask_max & kCTFontTraitMonoSpace);
               mmask += kCTFontTraitMonoSpace)
            for (bmask = (mask_min & kCTFontTraitBold);
                 bmask <= (mask_max & kCTFontTraitBold);
                 bmask += kCTFontTraitBold)
              for (imask = (mask_min & kCTFontTraitItalic);
                   imask <= (mask_max & kCTFontTraitItalic);
                   imask += kCTFontTraitItalic)
                {
                  CTFontSymbolicTraits synth = (imask | bmask | mmask);

                  if (synth == 0
                      || macfont_closest_traits_index_p (traits_array,
                                                         (sym_traits | synth),
                                                         j))
                    {
                      entity = macfont_descriptor_entity (desc, extra, synth);
                      if (! NILP (entity))
                        val = Fcons (entity, val);
                    }
                }
        }

      CFRelease (traits_array);
      CFRelease (descs);
    }

  CFRelease (families);
  val = Fnreverse (val);
  goto finish;
 err:
  val = Qnil;

 finish:
  FONT_ADD_LOG ("macfont-list", spec, val);
  if (charset) CFRelease (charset);
  if (languages) CFRelease (languages);
  if (attributes) CFRelease (attributes);
  if (family_name) CFRelease (family_name);

  unblock_input ();

  return val;
}

static Lisp_Object
macfont_match (struct frame * frame, Lisp_Object spec)
{
  Lisp_Object entity = Qnil;
  CFMutableDictionaryRef attributes;
  CTFontDescriptorRef pat_desc = NULL, desc = NULL;

  block_input ();

  attributes = macfont_create_attributes_with_spec (spec);
  if (attributes)
    {
      pat_desc = CTFontDescriptorCreateWithAttributes (attributes);
      CFRelease (attributes);
    }
  if (pat_desc)
    {
      desc = CTFontDescriptorCreateMatchingFontDescriptor (pat_desc, NULL);
      CFRelease (pat_desc);
    }
  if (desc)
    {
      entity = macfont_descriptor_entity (desc, AREF (spec, FONT_EXTRA_INDEX),
                                          0);
      CFRelease (desc);
    }
  unblock_input ();

  FONT_ADD_LOG ("macfont-match", spec, entity);
  return entity;
}

static Lisp_Object
macfont_list_family (struct frame *frame)
{
  Lisp_Object list = Qnil;
  CFArrayRef families;

  block_input ();

  families = macfont_copy_available_families_cache ();
  if (families)
    {
      CFIndex i, count = CFArrayGetCount (families);

      for (i = 0; i < count; i++)
        list = Fcons (macfont_intern_prop_cfstring (CFArrayGetValueAtIndex (families, i)), list);
      CFRelease (families);
    }

  unblock_input ();

  return list;
}

static void
macfont_free_entity (Lisp_Object entity)
{
  Lisp_Object val = assq_no_quit (QCfont_entity,
                                  AREF (entity, FONT_EXTRA_INDEX));
  CFStringRef name = XSAVE_POINTER (XCDR (val), 0);

  block_input ();
  CFRelease (name);
  unblock_input ();
}

static Lisp_Object
macfont_open (struct frame * f, Lisp_Object entity, int pixel_size)
{
  Lisp_Object val, font_object;
  CFStringRef font_name;
  struct macfont_info *macfont_info = NULL;
  struct font *font;
  int size;
  CTFontRef macfont;
  CTFontSymbolicTraits sym_traits;
  int i, total_width;
  CGGlyph glyph;
  CGFloat ascent, descent, leading;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val)
      || XTYPE (XCDR (val)) != Lisp_Misc
      || XMISCTYPE (XCDR (val)) != Lisp_Misc_Save_Value)
    return Qnil;
  font_name = XSAVE_POINTER (XCDR (val), 0);
  sym_traits = XSAVE_INTEGER (XCDR (val), 1);

  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;

  block_input ();
  macfont = CTFontCreateWithName (font_name, size, NULL);
  if (macfont)
    {
      int fontsize = (int) [((NSFont *) macfont) pointSize];
      if (fontsize != size) size = fontsize;
    }
  unblock_input ();
  if (! macfont)
    return Qnil;

  font_object = font_build_object (VECSIZE (struct macfont_info),
                                   Qmac_ct, entity, size);
  font = XFONT_OBJECT (font_object);
  font->pixel_size = size;
  font->driver = &macfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  block_input ();

  macfont_info = (struct macfont_info *) font;
  macfont_info->macfont = macfont;
  macfont_info->cgfont = CTFontCopyGraphicsFont (macfont, NULL);

  val = assq_no_quit (QCdestination, AREF (entity, FONT_EXTRA_INDEX));
  if (CONSP (val) && EQ (XCDR (val), make_number (1)))
    macfont_info->screen_font = mac_screen_font_create_with_name (font_name,
                                                                  size);
  else
    macfont_info->screen_font = NULL;
  macfont_info->cache = macfont_lookup_cache (font_name);
  macfont_retain_cache (macfont_info->cache);
  macfont_info->metrics = NULL;
  macfont_info->metrics_nrows = 0;
  macfont_info->synthetic_italic_p = 0;
  macfont_info->synthetic_bold_p = 0;
  macfont_info->spacing = MACFONT_SPACING_PROPORTIONAL;
  macfont_info->antialias = MACFONT_ANTIALIAS_DEFAULT;
  if (!(sym_traits & kCTFontTraitItalic)
      && FONT_SLANT_NUMERIC (entity) == FONT_SLANT_SYNTHETIC_ITALIC)
    macfont_info->synthetic_italic_p = 1;
  if (!(sym_traits & kCTFontTraitBold)
      && FONT_WEIGHT_NUMERIC (entity) == FONT_WEIGHT_SYNTHETIC_BOLD)
    macfont_info->synthetic_bold_p = 1;
  if (sym_traits & kCTFontTraitMonoSpace)
    macfont_info->spacing = MACFONT_SPACING_MONO;
  else if (INTEGERP (AREF (entity, FONT_SPACING_INDEX))
           && (XINT (AREF (entity, FONT_SPACING_INDEX))
               == FONT_SPACING_SYNTHETIC_MONO))
    macfont_info->spacing = MACFONT_SPACING_SYNTHETIC_MONO;
  if (macfont_info->synthetic_italic_p || macfont_info->synthetic_bold_p)
    macfont_info->antialias = MACFONT_ANTIALIAS_ON;
  else
    {
      val = assq_no_quit (QCantialias, AREF (entity, FONT_EXTRA_INDEX));
      if (CONSP (val))
        macfont_info->antialias =
          NILP (XCDR (val)) ? MACFONT_ANTIALIAS_OFF : MACFONT_ANTIALIAS_ON;
    }
  macfont_info->color_bitmap_p = 0;
  if (sym_traits & kCTFontTraitColorGlyphs)
    macfont_info->color_bitmap_p = 1;

  glyph = macfont_get_glyph_for_character (font, ' ');
  if (glyph != kCGFontIndexInvalid)
    font->space_width = macfont_glyph_extents (font, glyph, NULL, NULL, 0);
  else
    /* dirty workaround */
    font->space_width = pixel_size;

  total_width = font->space_width;
  for (i = 1; i < 95; i++)
    {
      glyph = macfont_get_glyph_for_character (font, ' ' + i);
      if (glyph == kCGFontIndexInvalid)
        break;
      total_width += macfont_glyph_extents (font, glyph, NULL, NULL, 0);
    }
  if (i == 95)
    font->average_width = total_width / 95;
  else
    font->average_width = font->space_width; /* XXX */

  if (!(macfont_info->screen_font
        && mac_screen_font_get_metrics (macfont_info->screen_font,
                                        &ascent, &descent, &leading)))
    {
      CFStringRef family_name;

      ascent = CTFontGetAscent (macfont);
      descent = CTFontGetDescent (macfont);
      leading = CTFontGetLeading (macfont);
      /* AppKit and WebKit do some adjustment to the heights of
         Courier, Helvetica, and Times.  */
      family_name = CTFontCopyFamilyName (macfont);
      if (family_name)
        {
          if (CFEqual (family_name, CFSTR ("Courier"))
              || CFEqual (family_name, CFSTR ("Helvetica"))
              || CFEqual (family_name, CFSTR ("Times")))
            ascent += (ascent + descent) * .15f;
          else if (CFStringHasPrefix (family_name, CFSTR ("Hiragino")))
            {
              leading *= .25f;
              ascent += leading;
            }
          CFRelease (family_name);
        }
    }
  font->ascent = ascent + 0.5f;
  val = assq_no_quit (QCminspace, AREF (entity, FONT_EXTRA_INDEX));
  if (CONSP (val) && !NILP (XCDR (val)))
    font->descent = descent + 0.5f;
  else
    font->descent = descent + leading + 0.5f;
  font->height = font->ascent + font->descent;

  font->underline_position = - CTFontGetUnderlinePosition (macfont) + 0.5f;
  font->underline_thickness = CTFontGetUnderlineThickness (macfont) + 0.5f;

  unblock_input ();

  /* Unfortunately Xft doesn't provide a way to get minimum char
     width.  So, we use space_width instead.  */
  font->min_width = font->max_width = font->space_width; /* XXX */

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;

  return font_object;
}

static void
macfont_close (struct font *font)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;

  if (macfont_info->cache)
    {
      int i;

      block_input ();
      CFRelease (macfont_info->macfont);
      CGFontRelease (macfont_info->cgfont);
      if (macfont_info->screen_font)
        CFRelease (macfont_info->screen_font);
      macfont_release_cache (macfont_info->cache);
      for (i = 0; i < macfont_info->metrics_nrows; i++)
        if (macfont_info->metrics[i])
          xfree (macfont_info->metrics[i]);
      if (macfont_info->metrics)
        xfree (macfont_info->metrics);
      macfont_info->cache = NULL;
      unblock_input ();
    }
}

static int
macfont_has_char (Lisp_Object font, int c)
{
  int result;
  CFCharacterSetRef charset;

  block_input ();
  if (FONT_ENTITY_P (font))
    {
      Lisp_Object val;
      CFStringRef name;

      val = assq_no_quit (QCfont_entity, AREF (font, FONT_EXTRA_INDEX));
      val = XCDR (val);
      name = XSAVE_POINTER (val, 0);
      charset = macfont_get_cf_charset_for_name (name);
    }
  else
    charset = macfont_get_cf_charset (XFONT_OBJECT (font));

  result = CFCharacterSetIsLongCharacterMember (charset, c);
  unblock_input ();

  return result;
}

static unsigned
macfont_encode_char (struct font *font, int c)
{
  CGGlyph glyph;

  block_input ();
  glyph = macfont_get_glyph_for_character (font, c);
  unblock_input ();

  return glyph != kCGFontIndexInvalid ? glyph : FONT_INVALID_CODE;
}

static void
macfont_text_extents (struct font *font, unsigned int *code, int nglyphs,
                      struct font_metrics *metrics)
{
  int width, i;

  block_input ();
  width = macfont_glyph_extents (font, code[0], metrics, NULL, 0);
  for (i = 1; i < nglyphs; i++)
    {
      struct font_metrics m;
      int w = macfont_glyph_extents (font, code[i], metrics ? &m : NULL,
                                     NULL, 0);

      if (metrics)
        {
          if (width + m.lbearing < metrics->lbearing)
            metrics->lbearing = width + m.lbearing;
          if (width + m.rbearing > metrics->rbearing)
            metrics->rbearing = width + m.rbearing;
          if (m.ascent > metrics->ascent)
            metrics->ascent = m.ascent;
          if (m.descent > metrics->descent)
            metrics->descent = m.descent;
        }
      width += w;
    }
  unblock_input ();

  if (metrics)
    metrics->width = width;
}

static int
macfont_draw (struct glyph_string *s, int from, int to, int x, int y,
              bool with_background)
{
  struct frame * f = s->f;
  struct macfont_info *macfont_info = (struct macfont_info *) s->font;
  CGRect background_rect;
  CGPoint text_position;
  CGGlyph *glyphs;
  CGPoint *positions;
  CGFloat font_size = CTFontGetSize (macfont_info->macfont);
  bool no_antialias_p =
    (NILP (ns_antialias_text)
     || macfont_info->antialias == MACFONT_ANTIALIAS_OFF
     || (macfont_info->antialias == MACFONT_ANTIALIAS_DEFAULT
         && font_size <= macfont_antialias_threshold));
  int len = to - from;
  struct face *face = s->face;
  CGContextRef context;

  block_input ();

  if (with_background)
    background_rect = CGRectMake (x, y - FONT_BASE (s->font),
                                  s->width, FONT_HEIGHT (s->font));
  else
    background_rect = CGRectNull;

  text_position = CGPointMake (x, -y);
  glyphs = xmalloc (sizeof (CGGlyph) * len);
  {
    CGFloat advance_delta = 0;
    int i;
    CGFloat total_width = 0;

    positions = xmalloc (sizeof (CGPoint) * len);
    for (i = 0; i < len; i++)
      {
        int width;

        glyphs[i] = s->char2b[from + i];
        width = (s->padding_p ? 1
                 : macfont_glyph_extents (s->font, glyphs[i],
                                          NULL, &advance_delta,
                                          no_antialias_p));
        positions[i].x = total_width + advance_delta;
        positions[i].y = 0;
        total_width += width;
      }
  }

  context = [[NSGraphicsContext currentContext] graphicsPort];
  CGContextSaveGState (context);

  if (!CGRectIsNull (background_rect))
    {
      if (s->hl == DRAW_MOUSE_FACE)
        {
          face = FACE_FROM_ID_OR_NULL (s->f,
				       MOUSE_HL_INFO (s->f)->mouse_face_face_id);
          if (!face)
            face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
        }
      CG_SET_FILL_COLOR_WITH_FACE_BACKGROUND (context, face, f);
      CGContextFillRects (context, &background_rect, 1);
    }

  if (macfont_info->cgfont)
    {
      CGAffineTransform atfm;

      CGContextScaleCTM (context, 1, -1);
      CG_SET_FILL_COLOR_WITH_FACE_FOREGROUND (context, face, s->f);
      if (macfont_info->synthetic_italic_p)
        atfm = synthetic_italic_atfm;
      else
        atfm = CGAffineTransformIdentity;
      if (macfont_info->synthetic_bold_p && ! no_antialias_p)
        {
          CGContextSetTextDrawingMode (context, kCGTextFillStroke);

          /* Stroke line width for text drawing is not correctly
             scaled on Retina display/HiDPI mode when drawn to screen
             (whereas it is correctly scaled when drawn to bitmaps),
             and synthetic bold looks thinner on such environments.
             Apple says there are no plans to address this issue
             (rdar://11644870) currently.  So we add a workaround.  */
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
          if ([[FRAME_NS_VIEW(f) window] respondsToSelector:
                                           @selector(backingScaleFactor)])
#endif
            CGContextSetLineWidth (context, synthetic_bold_factor * font_size
                                   * [[FRAME_NS_VIEW(f) window] backingScaleFactor]);
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
          else
#endif
#endif
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
            CGContextSetLineWidth (context, synthetic_bold_factor * font_size);
#endif
          CG_SET_STROKE_COLOR_WITH_FACE_FOREGROUND (context, face, f);
        }
      if (no_antialias_p)
        CGContextSetShouldAntialias (context, false);

      if (!NILP (ns_use_thin_smoothing))
        {
          CGContextSetShouldSmoothFonts(context, YES);
          CGContextSetFontSmoothingStyle(context, 16);
        }

      CGContextSetTextMatrix (context, atfm);
      CGContextSetTextPosition (context, text_position.x, text_position.y);

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
      if (macfont_info->color_bitmap_p
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1070
          && CTFontDrawGlyphs != NULL
#endif
          )
        {
          if (len > 0)
            {
              CTFontDrawGlyphs (macfont_info->macfont, glyphs, positions, len,
                                context);
            }
        }
      else
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1070 */
        {
          CGContextSetFont (context, macfont_info->cgfont);
          CGContextSetFontSize (context, font_size);
          CGContextShowGlyphsAtPositions (context, glyphs, positions, len);
        }
    }


  xfree (glyphs);
  xfree (positions);
  CGContextRestoreGState (context);

  unblock_input ();

  return len;
}

static Lisp_Object
macfont_shape (Lisp_Object lgstring)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;
  ptrdiff_t glyph_len, len, i, j;
  CFIndex nonbmp_len;
  UniChar *unichars;
  CFIndex *nonbmp_indices;
  CFStringRef string;
  CFIndex used = 0;
  struct mac_glyph_layout *glyph_layouts;

  glyph_len = LGSTRING_GLYPH_LEN (lgstring);
  nonbmp_len = 0;
  for (i = 0; i < glyph_len; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);

      if (NILP (lglyph))
        break;
      if (LGLYPH_CHAR (lglyph) >= 0x10000)
        nonbmp_len++;
    }

  len = i;

  if (INT_MAX / 2 < len)
    memory_full (SIZE_MAX);

  unichars = alloca (sizeof (UniChar) * (len + nonbmp_len));
  nonbmp_indices = alloca (sizeof (CFIndex) * (nonbmp_len + 1));
  for (i = j = 0; i < len; i++)
    {
      UTF32Char c = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));

      if (macfont_store_utf32char_to_unichars (c, unichars + i + j) > 1)
        {
          nonbmp_indices[j] = i + j;
          j++;
        }
    }
  nonbmp_indices[j] = len + j;	/* sentinel */

  block_input ();

  string = CFStringCreateWithCharactersNoCopy (NULL, unichars, len + nonbmp_len,
                                               kCFAllocatorNull);
  if (string)
    {
      glyph_layouts = alloca (sizeof (struct mac_glyph_layout) * glyph_len);
      if (macfont_info->screen_font)
        used = mac_screen_font_shape (macfont_info->screen_font, string,
                                      glyph_layouts, glyph_len);
      else
        used = mac_font_shape (macfont, string, glyph_layouts, glyph_len);
      CFRelease (string);
    }

  unblock_input ();

  if (used == 0)
    return Qnil;

  block_input ();

  for (i = 0; i < used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      struct mac_glyph_layout *gl = glyph_layouts + i;
      EMACS_INT from, to;
      struct font_metrics metrics;
      int xoff, yoff, wadjust;

      if (NILP (lglyph))
        {
          lglyph = Fmake_vector (make_number (LGLYPH_SIZE), Qnil);
          LGSTRING_SET_GLYPH (lgstring, i, lglyph);
        }

      from = gl->comp_range.location;
      /* Convert UTF-16 index to UTF-32.  */
      j = 0;
      while (nonbmp_indices[j] < from)
        j++;
      from -= j;
      LGLYPH_SET_FROM (lglyph, from);

      to = gl->comp_range.location + gl->comp_range.length;
      /* Convert UTF-16 index to UTF-32.  */
      while (nonbmp_indices[j] < to)
        j++;
      to -= j;
      LGLYPH_SET_TO (lglyph, to - 1);

      /* LGLYPH_CHAR is used in `describe-char' for checking whether
         the composition is trivial.  */
      {
        UTF32Char c;

        if (unichars[gl->string_index] >= 0xD800
            && unichars[gl->string_index] < 0xDC00)
          c = (((unichars[gl->string_index] - 0xD800) << 10)
               + (unichars[gl->string_index + 1] - 0xDC00) + 0x10000);
        else
          c = unichars[gl->string_index];
        if (macfont_get_glyph_for_character (font, c) != gl->glyph_id)
          c = 0;
        LGLYPH_SET_CHAR (lglyph, c);
      }

      {
        unsigned long cc = gl->glyph_id;
        LGLYPH_SET_CODE (lglyph, cc);
      }

      macfont_glyph_extents (font, gl->glyph_id, &metrics, NULL, 0);
      LGLYPH_SET_WIDTH (lglyph, metrics.width);
      LGLYPH_SET_LBEARING (lglyph, metrics.lbearing);
      LGLYPH_SET_RBEARING (lglyph, metrics.rbearing);
      LGLYPH_SET_ASCENT (lglyph, metrics.ascent);
      LGLYPH_SET_DESCENT (lglyph, metrics.descent);

      xoff = lround (gl->advance_delta);
      yoff = lround (- gl->baseline_delta);
      wadjust = lround (gl->advance);
      if (xoff != 0 || yoff != 0 || wadjust != metrics.width)
        {
          Lisp_Object vec;

          vec = Fmake_vector (make_number (3), Qnil);
          ASET (vec, 0, make_number (xoff));
          ASET (vec, 1, make_number (yoff));
          ASET (vec, 2, make_number (wadjust));
          LGLYPH_SET_ADJUSTMENT (lglyph, vec);
        }
    }

  unblock_input ();

  return make_number (used);
}

/* Structures for the UVS subtable (format 14) in the cmap table.  */
typedef UInt8 UINT24[3];

#pragma pack(push, 1)
struct variation_selector_record
{
  UINT24 var_selector;
  UInt32 default_uvs_offset, non_default_uvs_offset;
};
struct uvs_table
{
  UInt16 format;
  UInt32 length, num_var_selector_records;
  struct variation_selector_record variation_selector_records[1];
};
#define SIZEOF_UVS_TABLE_HEADER                                         \
  (sizeof (struct uvs_table) - sizeof (struct variation_selector_record))

struct unicode_value_range
{
  UINT24 start_unicode_value;
  UInt8 additional_count;
};
struct default_uvs_table {
  UInt32 num_unicode_value_ranges;
  struct unicode_value_range unicode_value_ranges[1];
};
#define SIZEOF_DEFAULT_UVS_TABLE_HEADER                                 \
  (sizeof (struct default_uvs_table) - sizeof (struct unicode_value_range))

struct uvs_mapping
{
  UINT24 unicode_value;
  UInt16 glyph_id;
};
struct non_default_uvs_table
{
  UInt32 num_uvs_mappings;
  struct uvs_mapping uvs_mappings[1];
};
#define SIZEOF_NON_DEFAULT_UVS_TABLE_HEADER                             \
  (sizeof (struct non_default_uvs_table) - sizeof (struct uvs_mapping))
#pragma pack(pop)

/* Read big endian values.  The argument LVAL must be an lvalue.  */
/* I suppose OSReadBigInt* takes care of unaligned data.  At least, we
   can find "... = OSReadBigInt32(cdb, 2);" followed by "... =
   OSReadBigInt16(cdb, 7);" in a sample code by Apple.  */
#define BUINT8_VALUE(lval)	(*((UInt8 *) &(lval)))
#define BUINT16_VALUE(lval)	OSReadBigInt16 (&(lval), 0)
/* Succeeding one byte should also be accessible.  */
#define BUINT24_VALUE(lval)	(OSReadBigInt32 (&(lval), 0) >> 8)
#define BUINT32_VALUE(lval)	OSReadBigInt32 (&(lval), 0)

/* Return UVS subtable for the specified FONT.  If the subtable is not
   found or ill-formatted, then return NULL.  */

static CFDataRef
mac_font_copy_uvs_table (CTFontRef font)
{
  CFDataRef cmap_table, uvs_table = NULL;

  cmap_table = CTFontCopyTable (font, cmapFontTableTag,
				kCTFontTableOptionNoOptions);
  if (cmap_table)
    {
      sfntCMapHeader *cmap = (sfntCMapHeader *) CFDataGetBytePtr (cmap_table);
      struct uvs_table *uvs;
      struct variation_selector_record *records;
      UInt32 cmap_len, ntables, i, uvs_offset, uvs_len, nrecords;

#if __LP64__
      if (CFDataGetLength (cmap_table) > UINT32_MAX)
        goto finish;
#endif

      cmap_len = CFDataGetLength (cmap_table);
      if (sizeof_sfntCMapHeader > cmap_len)
        goto finish;

      ntables = BUINT16_VALUE (cmap->numTables);
      if (ntables > ((cmap_len - sizeof_sfntCMapHeader)
                     / sizeof_sfntCMapEncoding))
        goto finish;

      for (i = 0; i < ntables; i++)
        if ((BUINT16_VALUE (cmap->encoding[i].platformID)
             == kFontUnicodePlatform)
            && (BUINT16_VALUE (cmap->encoding[i].scriptID)
                == 5)) /* kFontUnicodeV4_0VariationSequenceSemantics */
          {
            uvs_offset = BUINT32_VALUE (cmap->encoding[i].offset);
            break;
          }
      if (i == ntables
          || uvs_offset > cmap_len
          || SIZEOF_UVS_TABLE_HEADER > cmap_len - uvs_offset)
        goto finish;

      uvs = (struct uvs_table *) ((UInt8 *) cmap + uvs_offset);
      uvs_len = BUINT32_VALUE (uvs->length);
      if (uvs_len > cmap_len - uvs_offset
          || SIZEOF_UVS_TABLE_HEADER > uvs_len)
        goto finish;

      if (BUINT16_VALUE (uvs->format) != 14)
        goto finish;

      nrecords = BUINT32_VALUE (uvs->num_var_selector_records);
      if (nrecords > ((uvs_len - SIZEOF_UVS_TABLE_HEADER)
                      / sizeof (struct variation_selector_record)))
        goto finish;

      records = uvs->variation_selector_records;
      for (i = 0; i < nrecords; i++)
        {
          UInt32 default_uvs_offset, non_default_uvs_offset;

          default_uvs_offset = BUINT32_VALUE (records[i].default_uvs_offset);
          if (default_uvs_offset)
            {
              struct default_uvs_table *default_uvs;
              UInt32 nranges;

              if (default_uvs_offset > uvs_len
                  || (SIZEOF_DEFAULT_UVS_TABLE_HEADER
                      > uvs_len - default_uvs_offset))
                goto finish;

              default_uvs = ((struct default_uvs_table *)
                             ((UInt8 *) uvs + default_uvs_offset));
              nranges = BUINT32_VALUE (default_uvs->num_unicode_value_ranges);
              if (nranges > ((uvs_len - default_uvs_offset
                              - SIZEOF_DEFAULT_UVS_TABLE_HEADER)
                             / sizeof (struct unicode_value_range)))
                goto finish;
              /* Now 2 * nranges can't overflow, so we can safely use
                 `(lo + hi) / 2' instead of `lo + (hi - lo) / 2' in
                 mac_font_get_glyphs_for_variants.  */
            }

          non_default_uvs_offset =
            BUINT32_VALUE (records[i].non_default_uvs_offset);
          if (non_default_uvs_offset)
            {
              struct non_default_uvs_table *non_default_uvs;
              UInt32 nmappings;

              if (non_default_uvs_offset > uvs_len
                  || (SIZEOF_NON_DEFAULT_UVS_TABLE_HEADER
                      > uvs_len - non_default_uvs_offset))
                goto finish;

              non_default_uvs = ((struct non_default_uvs_table *)
                                 ((UInt8 *) uvs + non_default_uvs_offset));
              nmappings = BUINT32_VALUE (non_default_uvs->num_uvs_mappings);
              if (nmappings > ((uvs_len - non_default_uvs_offset
                                - SIZEOF_NON_DEFAULT_UVS_TABLE_HEADER)
                               / sizeof (struct uvs_mapping)))
                goto finish;
              /* Now 2 * nmappings can't overflow, so we can safely
                 use `(lo + hi) / 2' instead of `lo + (hi - lo) / 2'
                 in mac_font_get_glyphs_for_variants.  */
            }
        }

      uvs_table = CFDataCreate (NULL, (UInt8 *) uvs, uvs_len);

    finish:
      CFRelease (cmap_table);
    }

  return uvs_table;
}

/* Find an entry in the given UVS subtable UVS_TABLE for a variation
   sequence consisting of the given base character C and each
   variation selector SELECTORS[i] for 0 <= i < COUNT, and store the
   result (explained below) into the corresponding GLYPHS[i].  If the
   entry is found in the Default UVS Table, then the result is 0.  If
   the entry is found in the Non-Default UVS Table, then the result is
   the associated glyph ID.  Otherwise, kCGFontIndexInvalid.  The
   elements in SELECTORS must be sorted in strictly increasing
   order.  */

static void
mac_font_get_glyphs_for_variants (CFDataRef uvs_table, UTF32Char c,
                                  const UTF32Char selectors[], CGGlyph glyphs[],
                                  CFIndex count)
{
  struct uvs_table *uvs = (struct uvs_table *) CFDataGetBytePtr (uvs_table);
  struct variation_selector_record *records = uvs->variation_selector_records;
  CFIndex i;
  UInt32 ir, nrecords;

  nrecords = BUINT32_VALUE (uvs->num_var_selector_records);
  i = 0;
  ir = 0;
  while (i < count && ir < nrecords)
    {
      UInt32 default_uvs_offset, non_default_uvs_offset;

      if (selectors[i] < BUINT24_VALUE (records[ir].var_selector))
        {
          glyphs[i++] = kCGFontIndexInvalid;
          continue;
        }
      else if (selectors[i] > BUINT24_VALUE (records[ir].var_selector))
        {
          ir++;
          continue;
        }

      /* selectors[i] == BUINT24_VALUE (records[ir].var_selector) */
      default_uvs_offset = BUINT32_VALUE (records[ir].default_uvs_offset);
      non_default_uvs_offset =
        BUINT32_VALUE (records[ir].non_default_uvs_offset);

      glyphs[i] = kCGFontIndexInvalid;

      if (default_uvs_offset)
        {
          struct default_uvs_table *default_uvs =
            (struct default_uvs_table *) ((UInt8 *) uvs
                                          + default_uvs_offset);
          struct unicode_value_range *ranges =
            default_uvs->unicode_value_ranges;
          UInt32 lo, hi;

          lo = 0;
          hi = BUINT32_VALUE (default_uvs->num_unicode_value_ranges);
          while (lo < hi)
            {
              UInt32 mid = (lo + hi) / 2;

              if (c < BUINT24_VALUE (ranges[mid].start_unicode_value))
                hi = mid;
              else
                lo = mid + 1;
            }
          if (hi > 0
              && (c <= (BUINT24_VALUE (ranges[hi - 1].start_unicode_value)
                        + BUINT8_VALUE (ranges[hi - 1].additional_count))))
            glyphs[i] = 0;
        }

      if (glyphs[i] == kCGFontIndexInvalid && non_default_uvs_offset)
        {
          struct non_default_uvs_table *non_default_uvs =
            (struct non_default_uvs_table *) ((UInt8 *) uvs
                                              + non_default_uvs_offset);
          struct uvs_mapping *mappings = non_default_uvs->uvs_mappings;
          UInt32 lo, hi;

          lo = 0;
          hi = BUINT32_VALUE (non_default_uvs->num_uvs_mappings);
          while (lo < hi)
            {
              UInt32 mid = (lo + hi) / 2;

              if (c < BUINT24_VALUE (mappings[mid].unicode_value))
                hi = mid;
              else
                lo = mid + 1;
            }
          if (hi > 0 &&
              BUINT24_VALUE (mappings[hi - 1].unicode_value) == c)
            glyphs[i] = BUINT16_VALUE (mappings[hi - 1].glyph_id);
        }
      i++;
      ir++;
    }
  while (i < count)
    glyphs[i++] = kCGFontIndexInvalid;
}

static int
macfont_variation_glyphs (struct font *font, int c, unsigned variations[256])
{
  CFDataRef uvs_table;
  NSCharacterCollection uvs_collection;
  int i, n = 0;

  block_input ();
  uvs_table = macfont_get_uvs_table (font, &uvs_collection);

  if (uvs_table)
    {
      UTF32Char selectors[256];
      CGGlyph glyphs[256];

      for (i = 0; i < 16; i++)
        selectors[i] = 0xFE00 + i;
      for (; i < 256; i++)
        selectors[i] = 0xE0100 + (i - 16);
      mac_font_get_glyphs_for_variants (uvs_table, c, selectors, glyphs, 256);
      for (i = 0; i < 256; i++)
        {
          CGGlyph glyph = glyphs[i];

          if (uvs_collection != NSIdentityMappingCharacterCollection
              && glyph != kCGFontIndexInvalid)
            glyph = macfont_get_glyph_for_cid (font, uvs_collection, glyph);
          if (glyph == kCGFontIndexInvalid)
            variations[i] = 0;
          else
            {
              variations[i] = (glyph ? glyph
                               : macfont_get_glyph_for_character (font, c));
              n++;
            }
        }
    }
  unblock_input ();

  return n;
}

static const char *const macfont_booleans[] = {
  ":antialias",
  ":minspace",
  NULL,
};

static const char *const macfont_non_booleans[] = {
  ":lang",
  ":script",
  ":destination",
  NULL,
};

static void
macfont_filter_properties (Lisp_Object font, Lisp_Object alist)
{
  font_filter_properties (font, alist, macfont_booleans, macfont_non_booleans);
}

static Boolean
mac_font_descriptor_supports_languages (CTFontDescriptorRef descriptor,
					CFArrayRef languages)
{
  Boolean result = true;
  CFArrayRef desc_languages =
    CTFontDescriptorCopyAttribute (descriptor, kCTFontLanguagesAttribute);

  if (desc_languages == NULL)
    result = false;
  else
    {
      CFRange range = CFRangeMake (0, CFArrayGetCount (desc_languages));
      CFIndex i, languages_count = CFArrayGetCount (languages);

      for (i = 0; i < languages_count; i++)
	{
	  CFStringRef language = CFArrayGetValueAtIndex (languages, i);

	  if (!CFArrayContainsValue (desc_languages, range, language)
	      /* PingFang SC contains "zh" and "zh-Hant" as covered
		 languages, but does not contain "zh-Hans".  */
	      && !(CFEqual (language, CFSTR ("zh-Hans"))
		   && CFArrayContainsValue (desc_languages, range,
					    CFSTR ("zh"))))
	    {
	      result = false;
	      break;
	    }
	}
      CFRelease (desc_languages);
    }

  return result;
}

static CFStringRef
mac_font_create_preferred_family_for_attributes (CFDictionaryRef attributes)
{
  CFStringRef result = NULL;
  CFStringRef charset_string =
    CFDictionaryGetValue (attributes, MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE);

  if (charset_string && CFStringGetLength (charset_string) > 0)
    {
      CFStringRef keys[] = {
#if MAC_OS_X_VERSION_MIN_REQUIRED >= 1090
        kCTLanguageAttributeName
#else
        CFSTR ("NSLanguage")
#endif
      };
      CFTypeRef values[] = {NULL};
      CFIndex num_values = 0;
      CFArrayRef languages
        = CFDictionaryGetValue (attributes, kCTFontLanguagesAttribute);

      if (languages && CFArrayGetCount (languages) > 0)
        {
          if (CTGetCoreTextVersion () >= kCTVersionNumber10_9)
            values[num_values++] = CFArrayGetValueAtIndex (languages, 0);
          else
            {
              CFCharacterSetRef charset =
                CFDictionaryGetValue (attributes, kCTFontCharacterSetAttribute);

              result = mac_font_copy_default_name_for_charset_and_languages (charset, languages);
            }
        }
      if (result == NULL)
        {
          CFAttributedStringRef attr_string = NULL;
          CTLineRef ctline = NULL;
          CFDictionaryRef attrs
            = CFDictionaryCreate (NULL, (const void **) keys,
                                  (const void **) values, num_values,
                                  &kCFTypeDictionaryKeyCallBacks,
                                  &kCFTypeDictionaryValueCallBacks);

          if (attrs)
            {
              attr_string = CFAttributedStringCreate (NULL, charset_string,
                                                      attrs);
              CFRelease (attrs);
            }
          if (attr_string)
            {
              ctline = CTLineCreateWithAttributedString (attr_string);
              CFRelease (attr_string);
            }
          if (ctline)
            {
              CFArrayRef runs = CTLineGetGlyphRuns (ctline);
              CFIndex i, nruns = CFArrayGetCount (runs);
              CTFontRef font;

              for (i = 0; i < nruns; i++)
                {
                  CTRunRef run = CFArrayGetValueAtIndex (runs, i);
                  CFDictionaryRef attributes = CTRunGetAttributes (run);
                  CTFontRef font_in_run;

                  if (attributes == NULL)
                    break;
                  font_in_run =
                    CFDictionaryGetValue (attributes, kCTFontAttributeName);
                  if (font_in_run == NULL)
                    break;
                  if (i == 0)
                    font = font_in_run;
                  else if (!mac_font_equal_in_postscript_name (font,
							       font_in_run))
                    break;
                }
              if (nruns > 0 && i == nruns)
                result = CTFontCopyAttribute (font, kCTFontFamilyNameAttribute);
              CFRelease (ctline);
            }
        }
    }

  return result;
}

static inline double
mac_font_get_advance_width_for_glyph (CTFontRef font, CGGlyph glyph)
{
  return CTFontGetAdvancesForGlyphs (font, kCTFontOrientationDefault,
				     &glyph, NULL, 1);
}

static inline CGRect
mac_font_get_bounding_rect_for_glyph (CTFontRef font, CGGlyph glyph)
{
  return CTFontGetBoundingRectsForGlyphs (font, kCTFontOrientationDefault,
					  &glyph, NULL, 1);
}

static CFArrayRef
mac_font_create_available_families (void)
{
  CFMutableArrayRef families = NULL;
  CFArrayRef orig_families = CTFontManagerCopyAvailableFontFamilyNames ();

  if (orig_families)
    {
      CFIndex i, count = CFArrayGetCount (orig_families);

      families = CFArrayCreateMutable (NULL, count, &kCFTypeArrayCallBacks);
      if (families)
	for (i = 0; i < count; i++)
	  {
	    CFStringRef family = CFArrayGetValueAtIndex (orig_families, i);

	    if (!CFStringHasPrefix (family, CFSTR ("."))
		&& (CTFontManagerCompareFontFamilyNames (family,
							 CFSTR ("LastResort"),
							 NULL)
		    != kCFCompareEqualTo))
	      CFArrayAppendValue (families, family);
	  }
      CFRelease (orig_families);
    }

  return families;
}

static Boolean
mac_font_equal_in_postscript_name (CTFontRef font1, CTFontRef font2)
{
  Boolean result;
  CFStringRef name1, name2;

  if (font1 == font2)
    return true;

  result = false;
  name1 = CTFontCopyPostScriptName (font1);
  if (name1)
    {
      name2 = CTFontCopyPostScriptName (font2);
      if (name2)
        {
          result = CFEqual (name1, name2);
          CFRelease (name2);
        }
      CFRelease (name1);
    }

  return result;
}

static CTLineRef
mac_font_create_line_with_string_and_font (CFStringRef string,
					   CTFontRef macfont)
{
  CFStringRef keys[] = {kCTFontAttributeName, kCTKernAttributeName};
  CFTypeRef values[] = {NULL, NULL};
  CFDictionaryRef attributes = NULL;
  CFAttributedStringRef attr_string = NULL;
  CTLineRef ctline = NULL;
  float float_zero = 0.0f;

  values[0] = macfont;
  values[1] = CFNumberCreate (NULL, kCFNumberFloatType, &float_zero);
  if (values[1])
    {
      attributes = CFDictionaryCreate (NULL, (const void **) keys,
                                       (const void **) values,
                                       ARRAYELTS (keys),
                                       &kCFTypeDictionaryKeyCallBacks,
                                       &kCFTypeDictionaryValueCallBacks);
      CFRelease (values[1]);
    }
  if (attributes)
    {
      attr_string = CFAttributedStringCreate (NULL, string, attributes);
      CFRelease (attributes);
    }
  if (attr_string)
    {
      ctline = CTLineCreateWithAttributedString (attr_string);
      CFRelease (attr_string);
    }
  if (ctline)
    {
      /* Abandon if ctline contains some fonts other than the
         specified one.  */
      CFArrayRef runs = CTLineGetGlyphRuns (ctline);
      CFIndex i, nruns = CFArrayGetCount (runs);

      for (i = 0; i < nruns; i++)
        {
          CTRunRef run = CFArrayGetValueAtIndex (runs, i);
          CFDictionaryRef attributes = CTRunGetAttributes (run);
          CTFontRef font_in_run;

          if (attributes == NULL)
            break;
          font_in_run =
            CFDictionaryGetValue (attributes, kCTFontAttributeName);
          if (font_in_run == NULL)
            break;
          if (!mac_font_equal_in_postscript_name (macfont, font_in_run))
            break;
        }
      if (i < nruns)
        {
          CFRelease (ctline);
          ctline = NULL;
        }
    }

  return ctline;
}

static CFIndex
mac_font_shape (CTFontRef font, CFStringRef string,
		struct mac_glyph_layout *glyph_layouts, CFIndex glyph_len)
{
  CFIndex used, result = 0;
  CTLineRef ctline = mac_font_create_line_with_string_and_font (string, font);

  if (ctline == NULL)
    return 0;

  used = CTLineGetGlyphCount (ctline);
  if (used <= glyph_len)
    {
      CFArrayRef ctruns = CTLineGetGlyphRuns (ctline);
      CFIndex k, ctrun_count = CFArrayGetCount (ctruns);
      CGFloat total_advance = 0;
      CFIndex total_glyph_count = 0;

      for (k = 0; k < ctrun_count; k++)
        {
          CTRunRef ctrun = CFArrayGetValueAtIndex (ctruns, k);
          CFIndex i, min_location, glyph_count = CTRunGetGlyphCount (ctrun);
          struct mac_glyph_layout *glbuf = glyph_layouts + total_glyph_count;
          CFRange string_range, comp_range, range;
          CFIndex *permutation;

          if (CTRunGetStatus (ctrun) & kCTRunStatusRightToLeft)
            permutation = xmalloc (sizeof (CFIndex) * glyph_count);
          else
            permutation = NULL;

#define RIGHT_TO_LEFT_P permutation

          /* Now the `comp_range' member of struct mac_glyph_layout is
             temporarily used as a work area such that:
             glbuf[i].comp_range.location =
             min {compRange[i + 1].location, ...,
		     compRange[glyph_count - 1].location,
		     maxRange (stringRangeForCTRun)}
             glbuf[i].comp_range.length = maxRange (compRange[i])
             where compRange[i] is the range of composed characters
             containing i-th glyph.  */
          string_range = CTRunGetStringRange (ctrun);
          min_location = string_range.location + string_range.length;
          for (i = 0; i < glyph_count; i++)
            {
              struct mac_glyph_layout *gl = glbuf + glyph_count - i - 1;
              CFIndex glyph_index;
              CFRange rng;

              if (!RIGHT_TO_LEFT_P)
                glyph_index = glyph_count - i - 1;
              else
                glyph_index = i;
              CTRunGetStringIndices (ctrun, CFRangeMake (glyph_index, 1),
                                     &gl->string_index);
              rng =
                CFStringGetRangeOfComposedCharactersAtIndex (string,
                                                             gl->string_index);
              gl->comp_range.location = min_location;
              gl->comp_range.length = rng.location + rng.length;
              if (rng.location < min_location)
                min_location = rng.location;
            }

          /* Fill the `comp_range' member of struct mac_glyph_layout,
             and setup a permutation for right-to-left text.  */
          comp_range = CFRangeMake (string_range.location, 0);
          range = CFRangeMake (0, 0);
          while (1)
            {
              struct mac_glyph_layout *gl =
                glbuf + range.location + range.length;

              if (gl->comp_range.length
                  > comp_range.location + comp_range.length)
                comp_range.length = gl->comp_range.length - comp_range.location;
              min_location = gl->comp_range.location;
              range.length++;

              if (min_location >= comp_range.location + comp_range.length)
                {
                  comp_range.length = min_location - comp_range.location;
                  for (i = 0; i < range.length; i++)
                    {
                      glbuf[range.location + i].comp_range = comp_range;
                      if (RIGHT_TO_LEFT_P)
                        permutation[range.location + i] =
                          range.location + range.length - i - 1;
                    }

                  comp_range = CFRangeMake (min_location, 0);
                  range.location += range.length;
                  range.length = 0;
                  if (range.location == glyph_count)
                    break;
                }
            }

          /* Then fill the remaining members.  */
          for (range = CFRangeMake (0, 1); range.location < glyph_count;
               range.location++)
            {
              struct mac_glyph_layout *gl;
              CGPoint position;
	      CGFloat max_x;

              if (!RIGHT_TO_LEFT_P)
                gl = glbuf + range.location;
              else
                {
                  CFIndex src, dest;

                  src = glyph_count - 1 - range.location;
                  dest = permutation[src];
                  gl = glbuf + dest;
                  if (src < dest)
                    {
                      CFIndex tmp = gl->string_index;

                      gl->string_index = glbuf[src].string_index;
                      glbuf[src].string_index = tmp;
                    }
                }
              CTRunGetGlyphs (ctrun, range, &gl->glyph_id);

              CTRunGetPositions (ctrun, range, &position);
	      max_x = position.x + CTRunGetTypographicBounds (ctrun, range,
							      NULL, NULL, NULL);
	      max_x = max (max_x, total_advance);
              gl->advance_delta = position.x - total_advance;
              gl->baseline_delta = position.y;
              gl->advance = max_x - total_advance;
              total_advance = max_x;
            }

          if (RIGHT_TO_LEFT_P)
            xfree (permutation);

#undef RIGHT_TO_LEFT_P

          total_glyph_count += glyph_count;
        }

      result = used;
    }
  CFRelease (ctline);

  return result;
}

/* The function below seems to cause a memory leak for the CFString
   created by CFStringCreateWithCharacters as of Mac OS X 10.5.8 and
   10.6.3.  For now, we use the NSGlyphInfo version instead.  */
#if USE_CT_GLYPH_INFO
static CGGlyph
mac_ctfont_get_glyph_for_cid (CTFontRef font, CTCharacterCollection collection,
                              CGFontIndex cid)
{
  CGGlyph result = kCGFontIndexInvalid;
  UniChar characters[] = {0xfffd};
  CFStringRef string;
  CFAttributedStringRef attr_string = NULL;
  CTLineRef ctline = NULL;

  string = CFStringCreateWithCharacters (NULL, characters,
                                         ARRAYELTS (characters));

  if (string)
    {
      CTGlyphInfoRef glyph_info =
        CTGlyphInfoCreateWithCharacterIdentifier (cid, collection, string);
      CFDictionaryRef attributes = NULL;

      if (glyph_info)
        {
          CFStringRef keys[] = {kCTFontAttributeName,
                                kCTGlyphInfoAttributeName};
          CFTypeRef values[] = {font, glyph_info};

          attributes = CFDictionaryCreate (NULL, (const void **) keys,
                                           (const void **) values,
                                           ARRAYELTS (keys),
                                           &kCFTypeDictionaryKeyCallBacks,
                                           &kCFTypeDictionaryValueCallBacks);
          CFRelease (glyph_info);
        }
      if (attributes)
        {
          attr_string = CFAttributedStringCreate (NULL, string, attributes);
          CFRelease (attributes);
        }
      CFRelease (string);
    }
  if (attr_string)
    {
      ctline = CTLineCreateWithAttributedString (attr_string);
      CFRelease (attr_string);
    }
  if (ctline)
    {
      CFArrayRef runs = CTLineGetGlyphRuns (ctline);

      if (CFArrayGetCount (runs) > 0)
        {
          CTRunRef run = CFArrayGetValueAtIndex (runs, 0);
          CFDictionaryRef attributes = CTRunGetAttributes (run);

          if (attributes)
            {
              CTFontRef font_in_run =
                CFDictionaryGetValue (attributes, kCTFontAttributeName);

              if (font_in_run
                  && mac_font_equal_in_postscript_name (font_in_run, font))
                {
                  CTRunGetGlyphs (run, CFRangeMake (0, 1), &result);
                  if (result >= CTFontGetGlyphCount (font))
                    result = kCGFontIndexInvalid;
                }
            }
        }
      CFRelease (ctline);
    }

  return result;
}
#endif

static CFArrayRef
mac_font_copy_default_descriptors_for_language (CFStringRef language)
{
  CFArrayRef result = NULL;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1080
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1080
  if (CTFontCopyDefaultCascadeListForLanguages != NULL)
#endif
    {
      CTFontRef user_font =
	CTFontCreateUIFontForLanguage (kCTFontUIFontUser, 0, language);

      if (user_font)
        {
          CFArrayRef languages =
            CFArrayCreate (NULL, (const void **) &language, 1,
                           &kCFTypeArrayCallBacks);

          if (languages)
            {
              result = CTFontCopyDefaultCascadeListForLanguages (user_font,
                                                                 languages);
              CFRelease (languages);
            }
          CFRelease (user_font);
        }
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1080
  else		/* CTFontCopyDefaultCascadeListForLanguages == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1080 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1080
    {
      CFIndex i;

      for (i = 0; macfont_language_default_font_names[i].language; i++)
        {
          if (CFEqual (macfont_language_default_font_names[i].language,
                       language))
            {
              CFMutableArrayRef descriptors =
                CFArrayCreateMutable (NULL, 0, &kCFTypeArrayCallBacks);

              if (descriptors)
                {
                  CFIndex j;

                  for (j = 0;
                       macfont_language_default_font_names[i].font_names[j];
                       j++)
                    {
                      CFDictionaryRef attributes =
                        CFDictionaryCreate (NULL,
                                            ((const void **)
                                             &kCTFontNameAttribute),
                                            ((const void **)
                                             &macfont_language_default_font_names[i].font_names[j]),
                                            1, &kCFTypeDictionaryKeyCallBacks,
                                            &kCFTypeDictionaryValueCallBacks);

                      if (attributes)
                        {
                          CTFontDescriptorRef pat_desc =
                            CTFontDescriptorCreateWithAttributes (attributes);

                          if (pat_desc)
                            {
                              CTFontDescriptorRef descriptor =
                                CTFontDescriptorCreateMatchingFontDescriptor (pat_desc, NULL);

                              if (descriptor)
                                {
                                  CFArrayAppendValue (descriptors, descriptor);
                                  CFRelease (descriptor);
                                }
                              CFRelease (pat_desc);
                            }
                          CFRelease (attributes);
                        }
                    }
                  result = descriptors;
                }
              break;
            }
        }
    }
#endif

  return result;
}

static CFStringRef
mac_font_copy_default_name_for_charset_and_languages (CFCharacterSetRef charset,
                                                      CFArrayRef languages)
{
  CFStringRef result = NULL;
  CFStringRef language = CFArrayGetValueAtIndex (languages, 0);
  CFArrayRef descriptors =
    mac_font_copy_default_descriptors_for_language (language);

  if (descriptors)
    {
      CFIndex i, count = CFArrayGetCount (descriptors);

      for (i = 0; i < count; i++)
        {
          CTFontDescriptorRef descriptor =
            CFArrayGetValueAtIndex (descriptors, i);

          if (macfont_supports_charset_and_languages_p (descriptor, charset,
                                                        Qnil, languages))
            {
              CFStringRef family =
                CTFontDescriptorCopyAttribute (descriptor,
					       kCTFontFamilyNameAttribute);
              if (family)
                {
                  if (!CFStringHasPrefix (family, CFSTR ("."))
                      && !CFEqual (family, CFSTR ("LastResort")))
                    {
                      result = family;
                      break;
                    }
                  else
                    CFRelease (family);
                }
            }
        }
      CFRelease (descriptors);
    }

  return result;
}

void *
macfont_get_nsctfont (struct font *font)
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CTFontRef macfont = macfont_info->macfont;

  return (void *) macfont;
}

void
mac_register_font_driver (struct frame *f)
{
  register_font_driver (&macfont_driver, f);
}


void
syms_of_macfont (void)
{
  /* Core Text, for macOS.  */
  DEFSYM (Qmac_ct, "mac-ct");
  register_font_driver (&macfont_driver, NULL);

  /* The font property key specifying the font design destination.  The
     value is an unsigned integer code: 0 for WYSIWYG, and 1 for Video
     text.  (See the documentation of X Logical Font Description
     Conventions.)  In the Mac font driver, 1 means the screen font is
     used for calculating some glyph metrics.  You can see the
     difference with Monaco 8pt or 9pt, for example.  */
  DEFSYM (QCdestination, ":destination");

  /* The boolean-valued font property key specifying the use of leading.  */
  DEFSYM (QCminspace, ":minspace");

  macfont_family_cache = Qnil;
  staticpro (&macfont_family_cache);
}
