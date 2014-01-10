/* Interface definition for Mac OSX Core text font backend.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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

typedef CTFontDescriptorRef FontDescriptorRef;
typedef CTFontRef FontRef;
typedef CTFontSymbolicTraits FontSymbolicTraits;
typedef CTCharacterCollection CharacterCollection;

#define MAC_FONT_NAME_ATTRIBUTE kCTFontNameAttribute
#define MAC_FONT_FAMILY_NAME_ATTRIBUTE kCTFontFamilyNameAttribute
#define MAC_FONT_TRAITS_ATTRIBUTE kCTFontTraitsAttribute
#define MAC_FONT_SIZE_ATTRIBUTE kCTFontSizeAttribute
#define MAC_FONT_CASCADE_LIST_ATTRIBUTE kCTFontCascadeListAttribute
#define MAC_FONT_CHARACTER_SET_ATTRIBUTE kCTFontCharacterSetAttribute
#define MAC_FONT_LANGUAGES_ATTRIBUTE kCTFontLanguagesAttribute
#if MAC_OS_X_VERSION_MIN_REQUIRED >= 1060
#define MAC_FONT_FORMAT_ATTRIBUTE kCTFontFormatAttribute
#else
#define MAC_FONT_FORMAT_ATTRIBUTE (CFSTR ("NSCTFontFormatAttribute"))
#endif
#define MAC_FONT_SYMBOLIC_TRAIT kCTFontSymbolicTrait
#define MAC_FONT_WEIGHT_TRAIT kCTFontWeightTrait
#define MAC_FONT_WIDTH_TRAIT kCTFontWidthTrait
#define MAC_FONT_SLANT_TRAIT kCTFontSlantTrait

enum {
  MAC_FONT_TRAIT_ITALIC = kCTFontItalicTrait,
  MAC_FONT_TRAIT_BOLD = kCTFontBoldTrait,
  MAC_FONT_TRAIT_MONO_SPACE = kCTFontMonoSpaceTrait,
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1070
  MAC_FONT_TRAIT_COLOR_GLYPHS = kCTFontColorGlyphsTrait
#else
  MAC_FONT_TRAIT_COLOR_GLYPHS = (1 << 13)
#endif
};

enum {
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1060
  MAC_FONT_FORMAT_BITMAP = kCTFontFormatBitmap
#else
  MAC_FONT_FORMAT_BITMAP = 5
#endif
};

enum {
  MAC_CHARACTER_COLLECTION_IDENTITY_MAPPING = kCTIdentityMappingCharacterCollection,
  MAC_CHARACTER_COLLECTION_ADOBE_JAPAN1 = kCTAdobeJapan1CharacterCollection
};

#define mac_font_descriptor_create_with_attributes \
  CTFontDescriptorCreateWithAttributes
#define mac_font_descriptor_create_matching_font_descriptors \
  CTFontDescriptorCreateMatchingFontDescriptors
#define mac_font_descriptor_create_matching_font_descriptor \
  CTFontDescriptorCreateMatchingFontDescriptor
#define mac_font_descriptor_copy_attribute CTFontDescriptorCopyAttribute
#define mac_font_descriptor_supports_languages \
  mac_ctfont_descriptor_supports_languages
#define mac_font_create_with_name(name, size) \
  CTFontCreateWithName (name, size, NULL)
#define mac_font_get_size CTFontGetSize
#define mac_font_copy_family_name CTFontCopyFamilyName
#define mac_font_copy_character_set CTFontCopyCharacterSet
#define mac_font_get_glyphs_for_characters CTFontGetGlyphsForCharacters
#define mac_font_get_ascent CTFontGetAscent
#define mac_font_get_descent CTFontGetDescent
#define mac_font_get_leading CTFontGetLeading
#define mac_font_get_underline_position CTFontGetUnderlinePosition
#define mac_font_get_underline_thickness CTFontGetUnderlineThickness
#define mac_font_copy_graphics_font(font) CTFontCopyGraphicsFont (font, NULL)
#if MAC_OS_X_VERSION_MIN_REQUIRED >= 1060
#define mac_font_copy_non_synthetic_table(font, table) \
  CTFontCopyTable (font, table, kCTFontTableOptionNoOptions)
#else
#define mac_font_copy_non_synthetic_table(font, table) \
  CTFontCopyTable (font, table, kCTFontTableOptionExcludeSynthetic)
#endif

#define mac_font_create_preferred_family_for_attributes \
  mac_ctfont_create_preferred_family_for_attributes
#define mac_font_get_advance_width_for_glyph \
  mac_ctfont_get_advance_width_for_glyph
#define mac_font_get_bounding_rect_for_glyph \
  mac_ctfont_get_bounding_rect_for_glyph
#define mac_font_create_available_families mac_ctfont_create_available_families
#define mac_font_shape mac_ctfont_shape
#if USE_CT_GLYPH_INFO
#define mac_font_get_glyph_for_cid mac_ctfont_get_glyph_for_cid
#endif

#define mac_nsctfont_copy_font_descriptor CTFontCopyFontDescriptor

#ifndef kCTVersionNumber10_9
#define kCTVersionNumber10_9 0x00060000
#endif
#define MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE \
  (CFSTR ("MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE"))

typedef const struct _EmacsScreenFont *ScreenFontRef; /* opaque */

extern void mac_register_font_driver (struct frame *f);
extern void *macfont_get_nsctfont (struct font *font);

