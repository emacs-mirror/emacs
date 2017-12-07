/* Interface definition for macOS Core text font backend.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

/* Structure used by Mac `shape' functions for storing layout
   information for each glyph.  */
struct mac_glyph_layout
{
  /* Range of indices of the characters composed into the group of
     glyphs that share the cursor position with this glyph.  The
     members `location' and `length' are in UTF-16 indices.  */
  CFRange comp_range;

  /* UTF-16 index in the source string for the first character
     associated with this glyph.  */
  CFIndex string_index;

  /* Horizontal and vertical adjustments of glyph position.  The
     coordinate space is that of Core Text.  So, the `baseline_delta'
     value is negative if the glyph should be placed below the
     baseline.  */
  CGFloat advance_delta, baseline_delta;

  /* Typographical width of the glyph.  */
  CGFloat advance;

  /* Glyph ID of the glyph.  */
  CGGlyph glyph_id;
};

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1080
enum {
  kCTFontTraitItalic = kCTFontItalicTrait,
  kCTFontTraitBold = kCTFontBoldTrait,
  kCTFontTraitMonoSpace = kCTFontMonoSpaceTrait,
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  kCTFontTraitColorGlyphs = kCTFontColorGlyphsTrait
#else
  kCTFontTraitColorGlyphs = (1 << 13)
#endif
};

enum {
  kCTCharacterCollectionIdentityMapping = kCTIdentityMappingCharacterCollection,
  kCTCharacterCollectionAdobeJapan1 = kCTAdobeJapan1CharacterCollection
};

enum {
  kCTFontOrientationDefault = kCTFontDefaultOrientation
};
#endif

#if USE_CT_GLYPH_INFO
#define mac_font_get_glyph_for_cid mac_ctfont_get_glyph_for_cid
#endif

#ifndef kCTVersionNumber10_9
#define kCTVersionNumber10_9 0x00060000
#endif
#define MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE \
  (CFSTR ("MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE"))

typedef const struct _EmacsScreenFont *ScreenFontRef; /* opaque */

extern void mac_register_font_driver (struct frame *f);
extern void *macfont_get_nsctfont (struct font *font);
extern void macfont_update_antialias_threshold (void);

/* This is an undocumented function. */
extern void CGContextSetFontSmoothingStyle(CGContextRef, int)
  __attribute__((weak_import));
