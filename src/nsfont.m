/* Font back-end driver for the NeXT/Open/GNUstep and macOS window system.
   See font.h
   Copyright (C) 2006-2018 Free Software Foundation, Inc.

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

Author: Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "composite.h"
#include "blockinput.h"
#include "charset.h"
#include "frame.h"
#include "window.h"
#include "fontset.h"
#include "nsterm.h"
#include "character.h"
#include "font.h"
#include "termchar.h"

/* TODO: Drop once we can assume gnustep-gui 0.17.1. */
#ifdef NS_IMPL_GNUSTEP
#import <AppKit/NSFontDescriptor.h>
#endif

#define NSFONT_TRACE 0
#define LCD_SMOOTHING_MARGIN 2

/* font glyph and metrics caching functions, implemented at end */
static void ns_uni_to_glyphs (struct nsfont_info *font_info,
                              unsigned char block);
static void ns_glyph_metrics (struct nsfont_info *font_info,
                              unsigned char block);

#define INVALID_GLYPH 0xFFFF

/* ==========================================================================

    Utilities

   ========================================================================== */


/* Replace spaces w/another character so emacs core font parsing routines
   aren't thrown off. */
static void
ns_escape_name (char *name)
{
  for (; *name; name++)
    if (*name == ' ')
      *name = '_';
}


/* Reconstruct spaces in a font family name passed through emacs. */
static void
ns_unescape_name (char *name)
{
  for (; *name; name++)
    if (*name == '_')
      *name = ' ';
}


/* Extract family name from a font spec. */
static NSString *
ns_get_family (Lisp_Object font_spec)
{
  Lisp_Object tem = AREF (font_spec, FONT_FAMILY_INDEX);
  if (NILP (tem))
      return nil;
  else
    {
      char *tmp = xlispstrdup (SYMBOL_NAME (tem));
      NSString *family;
      ns_unescape_name (tmp);
      family = [NSString stringWithUTF8String: tmp];
      xfree (tmp);
      return family;
    }
}


/* Return 0 if attr not set, else value (which might also be 0).
   On Leopard 0 gets returned even on descriptors where the attribute
   was never set, so there's no way to distinguish between unspecified
   and set to not have.  Callers should assume 0 means unspecified. */
static float
ns_attribute_fvalue (NSFontDescriptor *fdesc, NSString *trait)
{
    NSDictionary *tdict = [fdesc objectForKey: NSFontTraitsAttribute];
    NSNumber *val = [tdict objectForKey: trait];
    return val == nil ? 0.0F : [val floatValue];
}


/* Converts FONT_WEIGHT, FONT_SLANT, FONT_WIDTH, plus family and script/lang
   to NSFont descriptor.  Information under extra only needed for matching. */
#define STYLE_REF 100
static NSFontDescriptor *
ns_spec_to_descriptor (Lisp_Object font_spec)
{
    NSFontDescriptor *fdesc;
    NSMutableDictionary *fdAttrs = [NSMutableDictionary new];
    NSMutableDictionary *tdict = [NSMutableDictionary new];
    NSString *family = ns_get_family (font_spec);
    float n;

    /* add each attr in font_spec to fdAttrs.. */
    n = min (FONT_WEIGHT_NUMERIC (font_spec), 200);
    if (n != -1 && n != STYLE_REF)
	[tdict setObject: [NSNumber numberWithFloat: (n - 100.0F) / 100.0F]
		  forKey: NSFontWeightTrait];
    n = min (FONT_SLANT_NUMERIC (font_spec), 200);
    if (n != -1 && n != STYLE_REF)
	[tdict setObject: [NSNumber numberWithFloat: (n - 100.0F) / 100.0F]
		  forKey: NSFontSlantTrait];
    n = min (FONT_WIDTH_NUMERIC (font_spec), 200);
    if (n > -1 && (n > STYLE_REF + 10 || n < STYLE_REF - 10))
	[tdict setObject: [NSNumber numberWithFloat: (n - 100.0F) / 100.0F]
		  forKey: NSFontWidthTrait];
    if ([tdict count] > 0)
	[fdAttrs setObject: tdict forKey: NSFontTraitsAttribute];

    fdesc = [[[NSFontDescriptor fontDescriptorWithFontAttributes: fdAttrs]
               retain] autorelease];

    if (family != nil)
      {
        NSFontDescriptor *fdesc2 = [fdesc fontDescriptorWithFamily: family];
        fdesc = [[fdesc2 retain] autorelease];
      }

    [fdAttrs release];
    [tdict release];
    return fdesc;
}


/* Converts NSFont descriptor to FONT_WEIGHT, FONT_SLANT, FONT_WIDTH, etc.. */
static Lisp_Object
ns_descriptor_to_entity (NSFontDescriptor *desc,
                         Lisp_Object extra,
                         const char *style)
{
    Lisp_Object font_entity = font_make_entity ();
    /*   NSString *psName = [desc postscriptName]; */
    NSString *family = [desc objectForKey: NSFontFamilyAttribute];
    unsigned int traits = [desc symbolicTraits];
    char *escapedFamily;

    /* Shouldn't happen, but on Tiger fallback desc gets name but no family. */
    if (family == nil)
      family = [desc objectForKey: NSFontNameAttribute];
    if (family == nil)
      family = [[NSFont userFixedPitchFontOfSize: 0] familyName];

    escapedFamily = xstrdup ([family UTF8String]);
    ns_escape_name (escapedFamily);

    ASET (font_entity, FONT_TYPE_INDEX, Qns);
    ASET (font_entity, FONT_FOUNDRY_INDEX, Qapple);
    ASET (font_entity, FONT_FAMILY_INDEX, intern (escapedFamily));
    ASET (font_entity, FONT_ADSTYLE_INDEX, style ? intern (style) : Qnil);
    ASET (font_entity, FONT_REGISTRY_INDEX, Qiso10646_1);

    FONT_SET_STYLE (font_entity, FONT_WEIGHT_INDEX,
		    traits & NSFontBoldTrait ? Qbold : Qmedium);
/*    FONT_SET_STYLE (font_entity, FONT_WEIGHT_INDEX,
		    make_number (100 + 100
			* ns_attribute_fvalue (desc, NSFontWeightTrait)));*/
    FONT_SET_STYLE (font_entity, FONT_SLANT_INDEX,
		    traits & NSFontItalicTrait ? Qitalic : Qnormal);
/*    FONT_SET_STYLE (font_entity, FONT_SLANT_INDEX,
		    make_number (100 + 100
			 * ns_attribute_fvalue (desc, NSFontSlantTrait)));*/
    FONT_SET_STYLE (font_entity, FONT_WIDTH_INDEX,
                    traits & NSFontCondensedTrait ? Qcondensed :
                    traits & NSFontExpandedTrait ? Qexpanded : Qnormal);
/*    FONT_SET_STYLE (font_entity, FONT_WIDTH_INDEX,
		    make_number (100 + 100
			 * ns_attribute_fvalue (desc, NSFontWidthTrait)));*/

    ASET (font_entity, FONT_SIZE_INDEX, make_number (0));
    ASET (font_entity, FONT_AVGWIDTH_INDEX, make_number (0));
    ASET (font_entity, FONT_SPACING_INDEX,
	  make_number([desc symbolicTraits] & NSFontMonoSpaceTrait
	      ? FONT_SPACING_MONO : FONT_SPACING_PROPORTIONAL));

    ASET (font_entity, FONT_EXTRA_INDEX, extra);
    ASET (font_entity, FONT_OBJLIST_INDEX, Qnil);

    if (NSFONT_TRACE)
      {
	fprintf (stderr, "created font_entity:\n    ");
	debug_print (font_entity);
      }

    xfree (escapedFamily);
    return font_entity;
}


/* Default font entity. */
static Lisp_Object
ns_fallback_entity (void)
{
  return ns_descriptor_to_entity ([[NSFont userFixedPitchFontOfSize: 0]
      fontDescriptor], Qnil, NULL);
}


/* Utility: get width of a char c in screen font SFONT */
static CGFloat
ns_char_width (NSFont *sfont, int c)
{
  CGFloat w = -1.0;
  NSString *cstr = [NSString stringWithFormat: @"%c", c];

#ifdef NS_IMPL_COCOA
  NSGlyph glyph = [sfont glyphWithName: cstr];
  if (glyph)
    w = [sfont advancementForGlyph: glyph].width;
#endif

  if (w < 0.0)
    {
      NSDictionary *attrsDictionary =
        [NSDictionary dictionaryWithObject: sfont forKey: NSFontAttributeName];
      w = [cstr sizeWithAttributes: attrsDictionary].width;
    }

  return max (w, 1.0);
}

/* Return average width over ASCII printable characters for SFONT.  */

static NSString *ascii_printable;

static int
ns_ascii_average_width (NSFont *sfont)
{
  CGFloat w = -1.0;

  if (!ascii_printable)
    {
      char chars[96];
      int ch;
      for (ch = 0; ch < 95; ch++)
	chars[ch] = ' ' + ch;
      chars[95] = '\0';

      ascii_printable = [[NSString alloc] initWithFormat: @"%s", chars];
    }

#ifdef NS_IMPL_COCOA
  NSGlyph glyph = [sfont glyphWithName: ascii_printable];
  if (glyph)
    w = [sfont advancementForGlyph: glyph].width;
#endif

  if (w < (CGFloat) 0.0)
    {
      NSDictionary *attrsDictionary =
	[NSDictionary dictionaryWithObject: sfont forKey: NSFontAttributeName];
      w = [ascii_printable sizeWithAttributes: attrsDictionary].width;
    }

  return lrint (w / (CGFloat) 95.0);
}


/* Return whether set1 covers set2 to a reasonable extent given by pct.
   We check, out of each 16 Unicode char range containing chars in set2,
   whether at least one character is present in set1.
   This must be true for pct of the pairs to consider it covering. */
static BOOL
ns_charset_covers(NSCharacterSet *set1, NSCharacterSet *set2, float pct)
{
    const unsigned short *bytes1 = [[set1 bitmapRepresentation] bytes];
    const unsigned short *bytes2 = [[set2 bitmapRepresentation] bytes];
    int i, off = 0, tot = 0;

    /* Work around what appears to be a GNUstep bug.
       See <https://bugs.gnu.org/11853>.  */
    if (! (bytes1 && bytes2))
      return NO;

    for (i=0; i<4096; i++, bytes1++, bytes2++)
	if (*bytes2)
	  {
	    tot++;
	    if (*bytes1 == 0)  // *bytes1 & *bytes2 != *bytes2
		off++;
	  }
//fprintf(stderr, "off = %d\ttot = %d\n", off,tot);
    return (float)off / tot < 1.0F - pct;
}


/* Convert :lang property to a script.  Use of :lang property by font backend
   seems to be limited for now (2009/05) to ja, zh, and ko. */
static NSString
*ns_lang_to_script (Lisp_Object lang)
{
    if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "ja"))
	return @"han";
    /* NOTE: ja given for any hanzi that's also a kanji, but Chinese fonts
             have more characters. */
    else if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "zh"))
	return @"han";
    else if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "ko"))
	return @"hangul";
    else
	return @"";
}


/* Convert OTF 4-letter script code to emacs script name.  (Why can't
   everyone just use some standard Unicode names for these?) */
static NSString
*ns_otf_to_script (Lisp_Object otf)
{
    Lisp_Object script = assq_no_quit (XCAR (otf), Votf_script_alist);
    return CONSP (script)
	? [NSString stringWithUTF8String: SSDATA (SYMBOL_NAME (XCDR ((script))))]
	: @"";
}


/* Convert a font registry, such as  */
static NSString
*ns_registry_to_script (char *reg)
{
    Lisp_Object script, r, rts = Vns_reg_to_script;
    while (CONSP (rts))
      {
        r = XCAR (XCAR (rts));
        if (!strncmp (SSDATA (r), reg, SBYTES (r)))
          {
            script = XCDR (XCAR (rts));
            return [NSString stringWithUTF8String: SSDATA (SYMBOL_NAME (script))];
          }
        rts = XCDR (rts);
      }
    return  @"";
}


/* Searches the :script, :lang, and :otf extra-bundle properties of the spec,
   plus registry regular property, for something that can be mapped to a
   Unicode script.  Empty string returned if no script spec found. */
static NSString
*ns_get_req_script (Lisp_Object font_spec)
{
    Lisp_Object reg = AREF (font_spec, FONT_REGISTRY_INDEX);
    Lisp_Object extra = AREF (font_spec, FONT_EXTRA_INDEX);

    /* The extra-bundle properties have priority. */
    for ( ; CONSP (extra); extra = XCDR (extra))
      {
	Lisp_Object tmp = XCAR (extra);
	if (CONSP (tmp))
	  {
	    Lisp_Object key = XCAR (tmp), val = XCDR (tmp);
	    if (EQ (key, QCscript) && SYMBOLP (val))
		return [NSString stringWithUTF8String:
		            SSDATA (SYMBOL_NAME (val))];
	    if (EQ (key, QClang) && SYMBOLP (val))
		return ns_lang_to_script (val);
	    if (EQ (key, QCotf) && CONSP (val) && SYMBOLP (XCAR (val)))
		return ns_otf_to_script (val);
	  }
      }

    /* If we get here, check the charset portion of the registry. */
    if (! NILP (reg))
      {
        /* XXX: iso10646 is passed in for non-ascii latin-1 characters
           (which causes box rendering if we don't treat it like iso8858-1)
           but also for ascii (which causes unnecessary font substitution). */
#if 0
        if (EQ (reg, Qiso10646_1))
          reg = Qiso8859_1;
#endif
        return ns_registry_to_script (SSDATA (SYMBOL_NAME (reg)));
      }

    return @"";
}


/* This small function is static in fontset.c.  If it can be made public for
   all ports, remove this, but otherwise it doesn't seem worth the ifdefs. */
static void
accumulate_script_ranges (Lisp_Object arg, Lisp_Object range, Lisp_Object val)
{
    if (EQ (XCAR (arg), val))
      {
	if (CONSP (range))
	  XSETCDR (arg, Fcons (Fcons (XCAR (range), XCDR (range)), XCDR (arg)));
	else
	  XSETCDR (arg, Fcons (Fcons (range, range), XCDR (arg)));
      }
}


/* Use the Unicode range information in Vchar_script_table to convert a script
   name into an NSCharacterSet. */
static NSCharacterSet
*ns_script_to_charset (NSString *scriptName)
{
    NSMutableCharacterSet *charset = [NSMutableCharacterSet new];
    Lisp_Object script = intern ([scriptName UTF8String]);
    Lisp_Object script_list = XCHAR_TABLE (Vchar_script_table)->extras[0];

    if (! NILP (Fmemq (script, script_list)))
      {
	Lisp_Object ranges, range_list;

	ranges = list1 (script);
	map_char_table (accumulate_script_ranges, Qnil, Vchar_script_table,
			ranges);
	range_list = Fnreverse (XCDR (ranges));
	if (! NILP (range_list))
	  {
	    for (; CONSP (range_list); range_list = XCDR (range_list))
	      {
		int start = XINT (XCAR (XCAR (range_list)));
		int end = XINT (XCDR (XCAR (range_list)));
		if (NSFONT_TRACE)
		    debug_print (XCAR (range_list));
		if (end < 0x10000)
		    [charset addCharactersInRange:
			NSMakeRange (start, end-start)];
	      }
	  }
      }
    return charset;
}


/* Return an array of font families containing characters for the given
   script, for the given coverage criterion, including at least LastResort.
   Results are cached by script for faster access.
   If none are found, we reduce the percentage and try again, until 5%.
   This provides a font with at least some characters if such can be found.
   We don't use isSupersetOfSet: because (a) it doesn't work on Tiger, and
   (b) need approximate match as fonts covering full Unicode ranges are rare. */
static NSSet
*ns_get_covering_families (NSString *script, float pct)
{
    static NSMutableDictionary *scriptToFamilies = nil;
    NSMutableSet *families;

    if (NSFONT_TRACE)
	NSLog(@"Request covering families for script: '%@'", script);

    if (scriptToFamilies == nil)
        scriptToFamilies = [[NSMutableDictionary alloc] init];

    if ((families = [scriptToFamilies objectForKey: script]) == nil)
      {
	NSFontManager *fontMgr = [NSFontManager sharedFontManager];
	NSArray *allFamilies = [fontMgr availableFontFamilies];

	if ([script length] == 0)
	    families = [NSMutableSet setWithArray: allFamilies];
	else
	  {
	    NSCharacterSet *charset = ns_script_to_charset (script);
	    NSString *family;
	    families = [NSMutableSet setWithCapacity: 10];
	    while (1)
	      {
		NSEnumerator *allFamiliesEnum = [allFamilies objectEnumerator];
		while ((family = [allFamiliesEnum nextObject]))
		  {
		    NSCharacterSet *fset = [[fontMgr fontWithFamily: family
                        traits: 0 weight: 5 size: 12.0]	coveredCharacterSet];
                    /* Some fonts on macOS, maybe many on GNUstep, return nil. */
                    if (fset == nil)
                      fset = [NSCharacterSet characterSetWithRange:
                                               NSMakeRange (0, 127)];
		    if (ns_charset_covers(fset, charset, pct))
			[families addObject: family];
		  }
                pct -= 0.2F;
		if ([families count] > 0 || pct < 0.05F)
		    break;
	      }
            [charset release];
	  }
#ifdef NS_IMPL_COCOA
	if ([families count] == 0)
	    [families addObject: @"LastResort"];
#endif
	[scriptToFamilies setObject: families forKey: script];
      }

    if (NSFONT_TRACE)
      NSLog(@"    returning %lu families", (unsigned long)[families count]);
    return families;
}


/* Implementation for list() and match().  List() can return nil, match()
must return something.  Strategy is to drop family name from attribute
matching set for match. */
static Lisp_Object
ns_findfonts (Lisp_Object font_spec, BOOL isMatch)
{
    Lisp_Object tem, list = Qnil;
    NSFontDescriptor *fdesc, *desc;
    NSMutableSet *fkeys;
    NSArray *matchingDescs;
    NSEnumerator *dEnum;
    NSString *family;
    NSSet *cFamilies;
    BOOL foundItal = NO;

    block_input ();
    if (NSFONT_TRACE)
      {
	fprintf (stderr, "nsfont: %s for fontspec:\n    ",
		 (isMatch ? "match" : "list"));
	debug_print (font_spec);
      }

    cFamilies = ns_get_covering_families (ns_get_req_script (font_spec), 0.90);

    fdesc = ns_spec_to_descriptor (font_spec);
    fkeys = [NSMutableSet setWithArray: [[fdesc fontAttributes] allKeys]];
    if (isMatch)
	[fkeys removeObject: NSFontFamilyAttribute];

    matchingDescs = [fdesc matchingFontDescriptorsWithMandatoryKeys: fkeys];

    if (NSFONT_TRACE)
	NSLog(@"Got desc %@ and found %lu matching fonts from it: ", fdesc,
	      (unsigned long)[matchingDescs count]);

    for (dEnum = [matchingDescs objectEnumerator]; (desc = [dEnum nextObject]);)
      {
	if (![cFamilies containsObject:
	         [desc objectForKey: NSFontFamilyAttribute]])
	    continue;
        tem = ns_descriptor_to_entity (desc,
					 AREF (font_spec, FONT_EXTRA_INDEX),
                                       NULL);
        if (isMatch)
          return tem;
	list = Fcons (tem, list);
	if (fabs (ns_attribute_fvalue (desc, NSFontSlantTrait)) > 0.05)
	    foundItal = YES;
      }

    /* Add synthItal member if needed. */
    family = [fdesc objectForKey: NSFontFamilyAttribute];
    if (family != nil && !foundItal && XINT (Flength (list)) > 0)
      {
        NSFontDescriptor *s1 = [NSFontDescriptor new];
        NSFontDescriptor *sDesc
          = [[s1 fontDescriptorWithSymbolicTraits: NSFontItalicTrait]
              fontDescriptorWithFamily: family];
	list = Fcons (ns_descriptor_to_entity (sDesc,
					 AREF (font_spec, FONT_EXTRA_INDEX),
					 "synthItal"), list);
        [s1 release];
      }

    unblock_input ();

    /* Return something if was a match and nothing found. */
    if (isMatch)
      return ns_fallback_entity ();

    if (NSFONT_TRACE)
	fprintf (stderr, "    Returning %"pI"d entities.\n",
                 XINT (Flength (list)));

    return list;
}



/* ==========================================================================

    Font driver implementation

   ========================================================================== */


/* Return a cache of font-entities on FRAME.  The cache must be a
   cons whose cdr part is the actual cache area.  */
static Lisp_Object
nsfont_get_cache (struct frame *frame)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (frame);
  return (dpyinfo->name_list_element);
}


/* List fonts exactly matching with FONT_SPEC on FRAME.  The value is a
   **list** of font-entities.  This and match () are sole APIs that allocate
   font-entities.  Properties to be considered (2009/05/19) are:
   regular: foundry, family, adstyle, registry
   extended: script, lang, otf
  "Extended" properties are not part of the vector but get stored as
   lisp properties under FONT_EXTRA_INDEX.

   The returned entities should have type set (to 'ns), plus the following:
   foundry, family, adstyle, registry,
   weight, slant, width, size (0 if scalable),
   dpi, spacing, avgwidth (0 if scalable)  */
static Lisp_Object
nsfont_list (struct frame *f, Lisp_Object font_spec)
{
  return ns_findfonts (font_spec, NO);
}


/* Return a font entity most closely matching with FONT_SPEC on
   FRAME.  The closeness is determined by the font backend, thus
   `face-font-selection-order' is ignored here.
   Properties to be considered are same as for list(). */
static Lisp_Object
nsfont_match (struct frame *f, Lisp_Object font_spec)
{
  return ns_findfonts (font_spec, YES);
}


/* List available families.  The value is a list of family names
   (symbols). */
static Lisp_Object
nsfont_list_family (struct frame *f)
{
  Lisp_Object list = Qnil;
  NSEnumerator *families;
  NSString *family;

  block_input ();
  families = [[[NSFontManager sharedFontManager] availableFontFamilies]
               objectEnumerator];
  while ((family = [families nextObject]))
      list = Fcons (intern ([family UTF8String]), list);
  /* FIXME: escape the name? */

  if (NSFONT_TRACE)
    fprintf (stderr, "nsfont: list families returning %"pI"d entries\n",
	     XINT (Flength (list)));

  unblock_input ();
  return list;
}


/* Open a font specified by FONT_ENTITY on frame F.  If the font is
   scalable, open it with PIXEL_SIZE.  */
static Lisp_Object
nsfont_open (struct frame *f, Lisp_Object font_entity, int pixel_size)
{
  BOOL synthItal;
  unsigned int traits = 0;
  struct nsfont_info *font_info;
  struct font *font;
  NSFontDescriptor *fontDesc = ns_spec_to_descriptor (font_entity);
  NSFontManager *fontMgr = [NSFontManager sharedFontManager];
  NSString *family;
  NSFont *nsfont, *sfont;
  Lisp_Object tem;
  NSRect brect;
  Lisp_Object font_object;
  int fixLeopardBug;

  block_input ();

  if (NSFONT_TRACE)
    {
      fprintf (stderr, "nsfont: open size %d of fontentity:\n    ", pixel_size);
      debug_print (font_entity);
    }

  if (pixel_size <= 0)
    {
      /* try to get it out of frame params */
        Lisp_Object tem = get_frame_param (f, Qfontsize);
        pixel_size = NILP (tem) ? 0 : XFASTINT (tem);
    }

  tem = AREF (font_entity, FONT_ADSTYLE_INDEX);
  synthItal = !NILP (tem) && !strncmp ("synthItal", SSDATA (SYMBOL_NAME (tem)),
                                       9);
  family = ns_get_family (font_entity);
  if (family == nil)
    family = [[NSFont userFixedPitchFontOfSize: 0] familyName];
  /* Should be > 0.23 as some font descriptors (e.g. Terminus) set to that
     when setting family in ns_spec_to_descriptor(). */
  if (ns_attribute_fvalue (fontDesc, NSFontWeightTrait) > 0.50F)
      traits |= NSBoldFontMask;
  if (fabs (ns_attribute_fvalue (fontDesc, NSFontSlantTrait) > 0.05F))
      traits |= NSItalicFontMask;

  /* see http://cocoadev.com/forums/comments.php?DiscussionID=74 */
  fixLeopardBug = traits & NSBoldFontMask ? 10 : 5;
  nsfont = [fontMgr fontWithFamily: family
                            traits: traits weight: fixLeopardBug
			      size: pixel_size];
  /* if didn't find, try synthetic italic */
  if (nsfont == nil && synthItal)
    {
      nsfont = [fontMgr fontWithFamily: family
                                traits: traits & ~NSItalicFontMask
                                weight: fixLeopardBug size: pixel_size];
    }
#ifdef NS_IMPL_COCOA
  /* LastResort not really a family */
  if (nsfont == nil && [@"LastResort" isEqualToString: family])
      nsfont = [NSFont fontWithName: @"LastResort" size: pixel_size];
#endif

  if (nsfont == nil)
    {
      message_with_string ("*** Warning: font in family `%s' not found",
                          build_string ([family UTF8String]), 1);
      nsfont = [NSFont userFixedPitchFontOfSize: pixel_size];
    }

  if (NSFONT_TRACE)
    NSLog (@"%@\n", nsfont);

  font_object = font_make_object (VECSIZE (struct nsfont_info),
                                  font_entity, pixel_size);
  ASET (font_object, FONT_TYPE_INDEX, Qns);
  font_info = (struct nsfont_info *) XFONT_OBJECT (font_object);
  font = (struct font *) font_info;
  if (!font)
    {
      unblock_input ();
      return Qnil; /* FIXME: other terms do, but return Qnil causes segfault */
    }

  font_info->glyphs = xzalloc (0x100 * sizeof *font_info->glyphs);
  font_info->metrics = xzalloc (0x100 * sizeof *font_info->metrics);

  /* for metrics */
#ifdef NS_IMPL_COCOA
  sfont = [nsfont screenFontWithRenderingMode:
                    NSFontAntialiasedIntegerAdvancementsRenderingMode];
#else
  sfont = [nsfont screenFont];
#endif

  if (sfont == nil)
    sfont = nsfont;

  /* non-metric backend font struct fields */
  font = (struct font *) font_info;
  font->pixel_size = [sfont pointSize];
  font->driver = &nsfont_driver;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  font->default_ascent = 0;
  font->vertical_centering = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;

  {
    const char *fontName = [[nsfont fontName] UTF8String];

    /* The values specified by fonts are not always exact. For
     * example, a 6x8 font could specify that the descender is
     * -2.00000405... (represented by 0xc000000220000000).  Without
     * adjustment, the code below would round the descender to -3,
     * resulting in a font that would be one pixel higher than
     * intended. */
    CGFloat adjusted_descender = [sfont descender] + 0.0001;

#ifdef NS_IMPL_GNUSTEP
    font_info->nsfont = sfont;
#else
    font_info->nsfont = nsfont;
#endif
    [font_info->nsfont retain];

    /* set up ns_font (defined in nsgui.h) */
    font_info->name = xstrdup (fontName);
    font_info->bold = [fontMgr traitsOfFont: nsfont] & NSBoldFontMask;
    font_info->ital =
      synthItal || ([fontMgr traitsOfFont: nsfont] & NSItalicFontMask);

    /* Metrics etc.; some fonts return an unusually large max advance, so we
       only use it for fonts that have wide characters. */
    font_info->width = ([sfont numberOfGlyphs] > 2000) ?
      [sfont maximumAdvancement].width : ns_char_width (sfont, '0');

    brect =  [sfont boundingRectForFont];

    font_info->underpos = [sfont underlinePosition];
    font_info->underwidth = [sfont underlineThickness];
    font_info->size = font->pixel_size;

    /* max bounds */
    font->ascent = font_info->max_bounds.ascent = lrint ([sfont ascender]);
    /* Descender is usually negative.  Use floor to avoid
       clipping descenders. */
    font->descent =
      font_info->max_bounds.descent = -lrint (floor(adjusted_descender));
    font_info->height =
      font_info->max_bounds.ascent + font_info->max_bounds.descent;
    font_info->max_bounds.width = lrint (font_info->width);
    font_info->max_bounds.lbearing = lrint (brect.origin.x);
    font_info->max_bounds.rbearing =
      lrint (brect.size.width - (CGFloat) font_info->width);

#ifdef NS_IMPL_COCOA
    /* set up synthItal and the CG font */
    font_info->synthItal = synthItal;
    {
      ATSFontRef atsFont = ATSFontFindFromPostScriptName
        ((CFStringRef)[nsfont fontName], kATSOptionFlagsDefault);

      if (atsFont == kATSFontRefUnspecified)
        {
          /* see if we can get it by dropping italic (then synthesizing) */
          atsFont = ATSFontFindFromPostScriptName ((CFStringRef)
              [[fontMgr convertFont: nsfont toNotHaveTrait: NSItalicFontMask]
                fontName], kATSOptionFlagsDefault);
          if (atsFont != kATSFontRefUnspecified)
              font_info->synthItal = YES;
          else
            {
              /* last resort fallback */
              atsFont = ATSFontFindFromPostScriptName
                ((CFStringRef)@"Monaco", kATSOptionFlagsDefault);
            }
        }
      font_info->cgfont = CGFontCreateWithPlatformFont ((void *) &atsFont);
    }
#endif

    /* set up metrics portion of font struct */
    font->ascent = lrint([sfont ascender]);
    font->descent = -lrint(floor(adjusted_descender));
    font->space_width = lrint (ns_char_width (sfont, ' '));
    font->max_width = lrint (font_info->max_bounds.width);
    font->min_width = font->space_width;  /* Approximate.  */
    font->average_width = ns_ascii_average_width (sfont);

    font->height = lrint (font_info->height);
    font->underline_position = lrint (font_info->underpos);
    font->underline_thickness = lrint (font_info->underwidth);

    font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);
    font->props[FONT_FULLNAME_INDEX] = build_unibyte_string (font_info->name);
  }
  unblock_input ();

  return font_object;
}


/* Close FONT. */
static void
nsfont_close (struct font *font)
{
  struct nsfont_info *font_info = (struct nsfont_info *) font;

  /* FIXME: font_info may be NULL due to same failure to detect
     same font that causes need for cache in nsfont_open.  */
  if (font_info && font_info->name)
    {
      int i;

      for (i = 0; i < 0x100; i++)
	{
	  xfree (font_info->glyphs[i]);
	  xfree (font_info->metrics[i]);
	}
      xfree (font_info->glyphs);
      xfree (font_info->metrics);
      [font_info->nsfont release];
#ifdef NS_IMPL_COCOA
      CGFontRelease (font_info->cgfont);
#endif
      xfree (font_info->name);
      font_info->name = NULL;
    }
}


/* If FONT_ENTITY has a glyph for character C (Unicode code point),
   return 1.  If not, return 0.  If a font must be opened to check
   it, return -1. */
static int
nsfont_has_char (Lisp_Object entity, int c)
{
  return -1;
}


/* Return a glyph code of FONT for character C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE. */
static unsigned int
nsfont_encode_char (struct font *font, int c)
{
  struct nsfont_info *font_info = (struct nsfont_info *)font;
  unsigned char high = (c & 0xff00) >> 8, low = c & 0x00ff;
  unsigned short g;

  if (c > 0xFFFF)
    return FONT_INVALID_CODE;

  /* did we already cache this block? */
  if (!font_info->glyphs[high])
    ns_uni_to_glyphs (font_info, high);

  g = font_info->glyphs[high][low];
  return g == INVALID_GLYPH ? FONT_INVALID_CODE : g;
}


/* Perform the size computation of glyphs of FONT and fill in members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS). */
static void
nsfont_text_extents (struct font *font, unsigned int *code,
		     int nglyphs, struct font_metrics *metrics)
{
  struct nsfont_info *font_info = (struct nsfont_info *)font;
  struct font_metrics *pcm;
  unsigned char high, low;
  int totalWidth = 0;
  int i;

  memset (metrics, 0, sizeof (struct font_metrics));

  for (i = 0; i < nglyphs; i++)
    {
      /* get metrics for this glyph, filling cache if need be */
      /* TODO: get metrics for whole string from an NSLayoutManager
               (if not too slow) */
      high = (code[i] & 0xFF00) >> 8;
      low = code[i] & 0x00FF;
      if (!font_info->metrics[high])
        ns_glyph_metrics (font_info, high);
      pcm = &(font_info->metrics[high][low]);

      if (metrics->lbearing > totalWidth + pcm->lbearing)
	metrics->lbearing = totalWidth + pcm->lbearing;
      if (metrics->rbearing < totalWidth + pcm->rbearing)
	metrics->rbearing = totalWidth + pcm->rbearing;
      if (metrics->ascent < pcm->ascent)
	metrics->ascent = pcm->ascent;
      if (metrics->descent < pcm->descent)
	metrics->descent = pcm->descent;

      totalWidth += pcm->width;
    }

  metrics->width = totalWidth;
}


/* Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
   position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND,
   fill the background in advance.  It is assured that WITH_BACKGROUND
   is false when (FROM > 0 || TO < S->nchars). */
static int
nsfont_draw (struct glyph_string *s, int from, int to, int x, int y,
             bool with_background)
/* NOTE: focus and clip must be set */
{
  static unsigned char cbuf[1024];
  unsigned char *c = cbuf;
#ifdef NS_IMPL_GNUSTEP
#if GNUSTEP_GUI_MAJOR_VERSION > 0 || GNUSTEP_GUI_MINOR_VERSION > 22
  static CGFloat advances[1024];
  CGFloat *adv = advances;
#else
  static float advances[1024];
  float *adv = advances;
#endif
#else
  static CGSize advances[1024];
  CGSize *adv = advances;
#endif
  struct face *face;
  NSRect r;
  struct nsfont_info *font;
  NSColor *col, *bgCol;
  unsigned short *t = s->char2b;
  int i, len, flags;
  char isComposite = s->first_glyph->type == COMPOSITE_GLYPH;

  block_input ();

  font = (struct nsfont_info *)s->face->font;
  if (font == NULL)
    font = (struct nsfont_info *)FRAME_FONT (s->f);

  /* Select face based on input flags */
  flags = s->hl == DRAW_CURSOR ? NS_DUMPGLYPH_CURSOR :
    (s->hl == DRAW_MOUSE_FACE ? NS_DUMPGLYPH_MOUSEFACE :
     (s->for_overlaps ? NS_DUMPGLYPH_FOREGROUND :
      NS_DUMPGLYPH_NORMAL));

  switch (flags)
    {
    case NS_DUMPGLYPH_CURSOR:
      face = s->face;
      break;
    case NS_DUMPGLYPH_MOUSEFACE:
      face = FACE_FROM_ID_OR_NULL (s->f,
				   MOUSE_HL_INFO (s->f)->mouse_face_face_id);
      if (!face)
        face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
      break;
    default:
      face = s->face;
    }

  r.origin.x = s->x;
  if (s->face->box != FACE_NO_BOX && s->first_glyph->left_box_line_p)
    r.origin.x += abs (s->face->box_line_width);

  r.origin.y = s->y;
  r.size.height = FONT_HEIGHT (font);

  /* Convert UTF-16 (?) to UTF-8 and determine advances.  Note if we just ask
     NS to render the string, it will come out differently from the individual
     character widths added up because of layout processing. */
  {
    int cwidth, twidth = 0;
    int hi, lo;
    /* FIXME: composition: no vertical displacement is considered. */
    t += from; /* advance into composition */
    for (i = from; i < to; i++, t++)
      {
        hi = (*t & 0xFF00) >> 8;
        lo = *t & 0x00FF;
        if (isComposite)
          {
	    if (!s->first_glyph->u.cmp.automatic)
		cwidth = s->cmp->offsets[i * 2] /* (H offset) */ - twidth;
	    else
	      {
		Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
		Lisp_Object glyph = LGSTRING_GLYPH (gstring, i);
		if (NILP (LGLYPH_ADJUSTMENT (glyph)))
		    cwidth = LGLYPH_WIDTH (glyph);
		else
		  {
		    cwidth = LGLYPH_WADJUST (glyph);
#ifdef NS_IMPL_GNUSTEP
		    *(adv-1) += LGLYPH_XOFF (glyph);
#else
		    (*(adv-1)).width += LGLYPH_XOFF (glyph);
#endif
		  }
	      }
          }
        else
          {
            if (!font->metrics[hi]) /* FIXME: why/how can we need this now? */
              ns_glyph_metrics (font, hi);
            cwidth = font->metrics[hi][lo].width;
          }
        twidth += cwidth;
#ifdef NS_IMPL_GNUSTEP
        *adv++ = cwidth;
        CHAR_STRING_ADVANCE (*t, c); /* this converts the char to UTF-8 */
#else
        (*adv++).width = cwidth;
#endif
      }
    len = adv - advances;
    r.size.width = twidth;
    *c = 0;
  }

  /* fill background if requested */
  if (with_background && !isComposite)
    {
      NSRect br = r;
      int fibw = FRAME_INTERNAL_BORDER_WIDTH (s->f);
      int mbox_line_width = max (s->face->box_line_width, 0);

      if (s->row->full_width_p)
        {
          if (br.origin.x <= fibw + 1 + mbox_line_width)
            {
              br.size.width += br.origin.x - mbox_line_width;
              br.origin.x = mbox_line_width;
            }
          if (FRAME_PIXEL_WIDTH (s->f) - (br.origin.x + br.size.width)
                <= fibw+1)
            br.size.width += fibw;
        }
      if (s->face->box == FACE_NO_BOX)
        {
          /* expand unboxed top row over internal border */
          if (br.origin.y <= fibw + 1 + mbox_line_width)
            {
              br.size.height += br.origin.y;
              br.origin.y = 0;
            }
        }
      else
        {
          int correction = abs (s->face->box_line_width)+1;
          br.origin.y += correction;
          br.size.height -= 2*correction;
          br.origin.x += correction;
          br.size.width -= 2*correction;
        }

      if (!s->face->stipple)
        [(NS_FACE_BACKGROUND (face) != 0
          ? ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f)
          : FRAME_BACKGROUND_COLOR (s->f)) set];
      else
        {
          struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (s->f);
          [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
        }
      NSRectFill (br);
    }


  /* set up for character rendering */
  r.origin.y = y;

  col = (NS_FACE_FOREGROUND (face) != 0
         ? ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f)
         : FRAME_FOREGROUND_COLOR (s->f));

  bgCol = (flags != NS_DUMPGLYPH_FOREGROUND ? nil
           : (NS_FACE_BACKGROUND (face) != 0
              ? ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f)
              : FRAME_BACKGROUND_COLOR (s->f)));

  /* render under GNUstep using DPS */
#ifdef NS_IMPL_GNUSTEP
  {
    NSGraphicsContext *context = GSCurrentContext ();

    DPSgsave (context);
    [font->nsfont set];

    /* do erase if "foreground" mode */
    if (bgCol != nil)
      {
        [bgCol set];
        DPSmoveto (context, r.origin.x, r.origin.y);
/*[context GSSetTextDrawingMode: GSTextFillStroke]; /// not implemented yet */
        DPSxshow (context, (const char *) cbuf, advances, len);
        DPSstroke (context);
        [col set];
/*[context GSSetTextDrawingMode: GSTextFill]; /// not implemented yet */
      }

    [col set];

    /* draw with DPSxshow () */
    DPSmoveto (context, r.origin.x, r.origin.y);
    DPSxshow (context, (const char *) cbuf, advances, len);
    DPSstroke (context);

    DPSgrestore (context);
  }

#else  /* NS_IMPL_COCOA */
  {
    CGContextRef gcontext =
      [[NSGraphicsContext currentContext] graphicsPort];
    static CGAffineTransform fliptf;
    static BOOL firstTime = YES;

    if (firstTime)
      {
        firstTime = NO;
        fliptf = CGAffineTransformMakeScale (1.0, -1.0);
      }

    CGContextSaveGState (gcontext);

    // Used to be Fix2X (kATSItalicQDSkew), but Fix2X is deprecated
    // and kATSItalicQDSkew is 0.25.
    fliptf.c =  font->synthItal ? 0.25 : 0.0;

    CGContextSetFont (gcontext, font->cgfont);
    CGContextSetFontSize (gcontext, font->size);
    if (NILP (ns_antialias_text) || font->size <= ns_antialias_threshold)
      CGContextSetShouldAntialias (gcontext, 0);
    else
      CGContextSetShouldAntialias (gcontext, 1);

    CGContextSetTextMatrix (gcontext, fliptf);

    if (bgCol != nil)
      {
        /* foreground drawing; erase first to avoid overstrike */
        [bgCol set];
        CGContextSetTextDrawingMode (gcontext, kCGTextFillStroke);
        CGContextSetTextPosition (gcontext, r.origin.x, r.origin.y);
        CGContextShowGlyphsWithAdvances (gcontext, s->char2b, advances, len);
        CGContextSetTextDrawingMode (gcontext, kCGTextFill);
      }

    [col set];

    CGContextSetTextPosition (gcontext, r.origin.x, r.origin.y);
    CGContextShowGlyphsWithAdvances (gcontext, s->char2b + from,
                                     advances, len);

    if (face->overstrike)
      {
        CGContextSetTextPosition (gcontext, r.origin.x+0.5, r.origin.y);
        CGContextShowGlyphsWithAdvances (gcontext, s->char2b + from,
                                         advances, len);
      }

    CGContextRestoreGState (gcontext);
  }
#endif  /* NS_IMPL_COCOA */

  unblock_input ();
  return to-from;
}



/* ==========================================================================

    Font glyph and metrics caching functions

   ========================================================================== */

/* Find and cache corresponding glyph codes for unicode values in given
   hi-byte block of 256. */
static void
ns_uni_to_glyphs (struct nsfont_info *font_info, unsigned char block)
{
#ifdef NS_IMPL_COCOA
  static EmacsGlyphStorage *glyphStorage;
  static char firstTime = 1;
#endif
  unichar *unichars = xmalloc (0x101 * sizeof (unichar));
  unsigned int i, g, idx;
  unsigned short *glyphs;

  if (NSFONT_TRACE)
    fprintf (stderr, "%p\tFinding glyphs for glyphs in block %d\n",
            font_info, block);

  block_input ();

#ifdef NS_IMPL_COCOA
  if (firstTime)
    {
      firstTime = 0;
      glyphStorage = [[EmacsGlyphStorage alloc] initWithCapacity: 0x100];
    }
#endif

  font_info->glyphs[block] = xmalloc (0x100 * sizeof (unsigned short));
  if (!unichars || !(font_info->glyphs[block]))
    emacs_abort ();

  /* create a string containing all Unicode characters in this block */
  for (idx = block<<8, i = 0; i < 0x100; idx++, i++)
    if (idx < 0xD800 || idx > 0xDFFF)
      unichars[i] = idx;
    else
      unichars[i] = 0xFEFF;
  unichars[0x100] = 0;

  {
#ifdef NS_IMPL_COCOA
    NSString *allChars = [[NSString alloc]
                               initWithCharactersNoCopy: unichars
                                                 length: 0x100
                                           freeWhenDone: NO];
    NSGlyphGenerator *glyphGenerator = [NSGlyphGenerator sharedGlyphGenerator];
    /*NSCharacterSet *coveredChars = [nsfont coveredCharacterSet]; */
    unsigned int numGlyphs = [font_info->nsfont numberOfGlyphs];
    NSUInteger gInd = 0, cInd = 0;

    [glyphStorage setString: allChars font: font_info->nsfont];
    [glyphGenerator generateGlyphsForGlyphStorage: glyphStorage
                        desiredNumberOfCharacters: glyphStorage->maxChar
                                       glyphIndex: &gInd characterIndex: &cInd];
#endif
    glyphs = font_info->glyphs[block];
    for (i = 0; i < 0x100; i++, glyphs++)
      {
#ifdef NS_IMPL_GNUSTEP
        g = unichars[i];
#else
        g = glyphStorage->cglyphs[i];
        /* TODO: is this a good check?  maybe need to use coveredChars.. */
        if (g > numGlyphs || g == NSNullGlyph)
          g = INVALID_GLYPH; /* hopefully unused... */
#endif
        *glyphs = g;
      }

#ifdef NS_IMPL_COCOA
    [allChars release];
#endif
  }

  unblock_input ();
  xfree (unichars);
}


/* Determine and cache metrics for corresponding glyph codes in given
   hi-byte block of 256. */
static void
ns_glyph_metrics (struct nsfont_info *font_info, unsigned char block)
{
  unsigned int i, g;
  unsigned int numGlyphs = [font_info->nsfont numberOfGlyphs];
  NSFont *sfont;
  struct font_metrics *metrics;

  if (NSFONT_TRACE)
    fprintf (stderr, "%p\tComputing metrics for glyphs in block %d\n",
            font_info, block);

#ifdef NS_IMPL_GNUSTEP
  /* not implemented yet (as of startup 0.18), so punt */
  if (numGlyphs == 0)
    numGlyphs = 0x10000;
#endif

  block_input ();
#ifdef NS_IMPL_COCOA
  sfont = [font_info->nsfont screenFontWithRenderingMode:
                      NSFontAntialiasedIntegerAdvancementsRenderingMode];
#else
  sfont = [font_info->nsfont screenFont];
#endif

  font_info->metrics[block] = xzalloc (0x100 * sizeof (struct font_metrics));
  if (!(font_info->metrics[block]))
    emacs_abort ();

  metrics = font_info->metrics[block];
  for (g = block<<8, i =0; i<0x100 && g < numGlyphs; g++, i++, metrics++)
    {
      CGFloat w, lb, rb;
      NSRect r = [sfont boundingRectForGlyph: g];

      w = max ([sfont advancementForGlyph: g].width, 2.0);
      metrics->width = lrint (w);

      lb = r.origin.x;
      rb = r.size.width - w;
      // Add to bearing for LCD smoothing.  We don't know if it is there.
      if (lb < 0)
        metrics->lbearing = round (lb - LCD_SMOOTHING_MARGIN);
      if (font_info->ital)
        rb += (CGFloat) (0.22F * font_info->height);
      metrics->rbearing = lrint (w + rb + LCD_SMOOTHING_MARGIN);

      metrics->descent = r.origin.y < 0 ? -r.origin.y : 0;
 /*lrint (hshrink * [sfont ascender] + expand * hd/2); */
      metrics->ascent = r.size.height - metrics->descent;
/*-lrint (hshrink* [sfont descender] - expand * hd/2); */
    }
  unblock_input ();
}


#ifdef NS_IMPL_COCOA
/* helper for font glyph setup */
@implementation EmacsGlyphStorage

- init
{
  return [self initWithCapacity: 1024];
}

- initWithCapacity: (unsigned long) c
{
  self = [super init];
  maxChar = 0;
  maxGlyph = 0;
  dict = [NSMutableDictionary new];
  cglyphs = xmalloc (c * sizeof (CGGlyph));
  return self;
}

- (void) dealloc
{
  if (attrStr != nil)
    [attrStr release];
  [dict release];
  xfree (cglyphs);
  [super dealloc];
}

- (void) setString: (NSString *)str font: (NSFont *)font
{
  [dict setObject: font forKey: NSFontAttributeName];
  if (attrStr != nil)
    [attrStr release];
  attrStr = [[NSAttributedString alloc] initWithString: str attributes: dict];
  maxChar = [str length];
  maxGlyph = 0;
}

/* NSGlyphStorage protocol */
- (NSUInteger)layoutOptions
{
  return 0;
}

- (NSAttributedString *)attributedString
{
  return attrStr;
}

- (void)insertGlyphs: (const NSGlyph *)glyphs length: (NSUInteger)length
        forStartingGlyphAtIndex: (NSUInteger)glyphIndex
        characterIndex: (NSUInteger)charIndex
{
  len = glyphIndex+length;
  for (i =glyphIndex; i<len; i++)
    cglyphs[i] = glyphs[i-glyphIndex];
  if (len > maxGlyph)
    maxGlyph = len;
}

- (void)setIntAttribute: (NSInteger)attributeTag value: (NSInteger)val
        forGlyphAtIndex: (NSUInteger)glyphIndex
{
  return;
}

@end
#endif /* NS_IMPL_COCOA */


/* Debugging */
void
ns_dump_glyphstring (struct glyph_string *s)
{
  int i;

  fprintf (stderr, "Glyph string len = %d at (%d, %d) overhang (%d, %d),"
"overlap = %d, bg_filled = %d:",
           s->nchars, s->x, s->y, s->left_overhang, s->right_overhang,
           s->row->overlapping_p, s->background_filled_p);
  for (i =0; i<s->nchars; i++)
    {
      int c = s->first_glyph[i].u.ch;
      fprintf (stderr, "%c", c);
    }
  fprintf (stderr, "\n");
}

struct font_driver const nsfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qns),
  .case_sensitive = true,
  .get_cache = nsfont_get_cache,
  .list = nsfont_list,
  .match = nsfont_match,
  .list_family = nsfont_list_family,
  .open = nsfont_open,
  .close = nsfont_close,
  .has_char = nsfont_has_char,
  .encode_char = nsfont_encode_char,
  .text_extents = nsfont_text_extents,
  .draw = nsfont_draw,
  };

void
syms_of_nsfont (void)
{
  register_font_driver (&nsfont_driver, NULL);
  DEFSYM (Qcondensed, "condensed");
  DEFSYM (Qexpanded, "expanded");
  DEFSYM (Qapple, "apple");
  DEFSYM (Qmedium, "medium");
  DEFVAR_LISP ("ns-reg-to-script", Vns_reg_to_script,
               doc: /* Internal use: maps font registry to Unicode script. */);

  ascii_printable = NULL;
}
