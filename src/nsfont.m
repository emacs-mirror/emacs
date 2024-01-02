/* Font back-end driver for the GNUstep window system.
   See font.h
   Copyright (C) 2006-2024 Free Software Foundation, Inc.

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
   interpretation of even the system includes.  */
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
#include "pdumper.h"

#import <Foundation/NSException.h>
#import <AppKit/NSFontDescriptor.h>
#import <AppKit/NSLayoutManager.h>
#import <GNUstepGUI/GSLayoutManager.h>
#import <GNUstepGUI/GSFontInfo.h>

#define NSFONT_TRACE 0

/* Structure used by GS `shape' functions for storing layout
   information for each glyph.  Borrowed from macfont.h.  */
struct ns_glyph_layout
{
  /* Range of indices of the characters composed into the group of
     glyphs that share the cursor position with this glyph.  The
     members `location' and `length' are in UTF-16 indices.  */
  NSRange comp_range;

  /* UTF-16 index in the source string for the first character
     associated with this glyph.  */
  NSUInteger string_index;

  /* Horizontal and vertical adjustments of glyph position.  The
     coordinate space is that of Core Text.  So, the `baseline_delta'
     value is negative if the glyph should be placed below the
     baseline.  */
  CGFloat advance_delta, baseline_delta;

  /* Typographical width of the glyph.  */
  CGFloat advance;

  /* Glyph ID of the glyph.  */
  NSGlyph glyph_id;
};


enum lgstring_direction
  {
    DIR_R2L = -1, DIR_UNKNOWN = 0, DIR_L2R = 1
  };

enum gs_font_slant
  {
    GS_FONT_SLANT_ITALIC,
    GS_FONT_SLANT_REVERSE_ITALIC,
    GS_FONT_SLANT_NORMAL
  };

enum gs_font_weight
  {
    GS_FONT_WEIGHT_LIGHT,
    GS_FONT_WEIGHT_BOLD,
    GS_FONT_WEIGHT_NORMAL
  };

enum gs_font_width
  {
    GS_FONT_WIDTH_CONDENSED,
    GS_FONT_WIDTH_EXPANDED,
    GS_FONT_WIDTH_NORMAL
  };

enum gs_specified
  {
    GS_SPECIFIED_SLANT = 1,
    GS_SPECIFIED_WEIGHT = 1 << 1,
    GS_SPECIFIED_WIDTH = 1 << 2,
    GS_SPECIFIED_FAMILY = 1 << 3,
    GS_SPECIFIED_SPACING = 1 << 4
  };

struct gs_font_data
{
  int specified;
  enum gs_font_slant slant;
  enum gs_font_weight weight;
  enum gs_font_width width;
  bool monospace_p;
  char *family_name;
};

static void
ns_done_font_data (struct gs_font_data *data)
{
  if (data->specified & GS_SPECIFIED_FAMILY)
    xfree (data->family_name);
}

static void
ns_get_font_data (NSFontDescriptor *desc, struct gs_font_data *dat)
{
  NSNumber *tem;
  NSFontSymbolicTraits traits = [desc symbolicTraits];
  NSDictionary *dict = [desc objectForKey: NSFontTraitsAttribute];
  NSString *family = [desc objectForKey: NSFontFamilyAttribute];

  dat->specified = 0;

  if (family != nil)
    {
      dat->specified |= GS_SPECIFIED_FAMILY;
      dat->family_name = xstrdup ([family cStringUsingEncoding: NSUTF8StringEncoding]);
    }

  tem = [desc objectForKey: NSFontFixedAdvanceAttribute];

  if ((tem != nil && [tem boolValue] != NO)
      || (traits & NSFontMonoSpaceTrait))
    {
      dat->specified |= GS_SPECIFIED_SPACING;
      dat->monospace_p = true;
    }
  else if (tem != nil && [tem boolValue] == NO)
    {
      dat->specified |= GS_SPECIFIED_SPACING;
      dat->monospace_p = false;
    }

  if (traits & NSFontBoldTrait)
    {
      dat->specified |= GS_SPECIFIED_WEIGHT;
      dat->weight = GS_FONT_WEIGHT_BOLD;
    }

  if (traits & NSFontItalicTrait)
    {
      dat->specified |= GS_SPECIFIED_SLANT;
      dat->slant = GS_FONT_SLANT_ITALIC;
    }

  if (traits & NSFontCondensedTrait)
    {
      dat->specified |= GS_SPECIFIED_WIDTH;
      dat->width = GS_FONT_WIDTH_CONDENSED;
    }
  else if (traits & NSFontExpandedTrait)
    {
      dat->specified |= GS_SPECIFIED_WIDTH;
      dat->width = GS_FONT_WIDTH_EXPANDED;
    }

  if (dict != nil)
    {
      tem = [dict objectForKey: NSFontSlantTrait];

      if (tem != nil)
	{
	  dat->specified |= GS_SPECIFIED_SLANT;

	  dat->slant = [tem floatValue] > 0
	    ? GS_FONT_SLANT_ITALIC
	    : ([tem floatValue] < 0
	       ? GS_FONT_SLANT_REVERSE_ITALIC
	       : GS_FONT_SLANT_NORMAL);
	}

      tem = [dict objectForKey: NSFontWeightTrait];

      if (tem != nil)
	{
	  dat->specified |= GS_SPECIFIED_WEIGHT;

	  dat->weight = [tem floatValue] > 0
	    ? GS_FONT_WEIGHT_BOLD
	    : ([tem floatValue] < -0.4f
	       ? GS_FONT_WEIGHT_LIGHT
	       : GS_FONT_WEIGHT_NORMAL);
	}

      tem = [dict objectForKey: NSFontWidthTrait];

      if (tem != nil)
	{
	  dat->specified |= GS_SPECIFIED_WIDTH;

	  dat->width = [tem floatValue] > 0
	    ? GS_FONT_WIDTH_EXPANDED
	    : ([tem floatValue] < 0
	       ? GS_FONT_WIDTH_NORMAL
	       : GS_FONT_WIDTH_CONDENSED);
	}
    }
}

static bool
ns_font_descs_match_p (NSFontDescriptor *desc, NSFontDescriptor *target)
{
  struct gs_font_data dat;
  struct gs_font_data t;

  ns_get_font_data (desc, &dat);
  ns_get_font_data (target, &t);

  if (!(t.specified & GS_SPECIFIED_WIDTH))
    t.width = GS_FONT_WIDTH_NORMAL;
  if (!(t.specified & GS_SPECIFIED_WEIGHT))
    t.weight = GS_FONT_WEIGHT_NORMAL;
  if (!(t.specified & GS_SPECIFIED_SPACING))
    t.monospace_p = false;
  if (!(t.specified & GS_SPECIFIED_SLANT))
    t.slant = GS_FONT_SLANT_NORMAL;

  if (!(t.specified & GS_SPECIFIED_FAMILY))
    emacs_abort ();

  bool match_p = true;

  if (dat.specified & GS_SPECIFIED_WIDTH
      && dat.width != t.width)
    {
      match_p = false;
      goto gout;
    }

  if (dat.specified & GS_SPECIFIED_WEIGHT
      && dat.weight != t.weight)
    {
      match_p = false;
      goto gout;
    }

  if (dat.specified & GS_SPECIFIED_SPACING
      && dat.monospace_p != t.monospace_p)
    {
      match_p = false;
      goto gout;
    }

  if (dat.specified & GS_SPECIFIED_SLANT
      && dat.monospace_p != t.monospace_p)
    {
      if (NSFONT_TRACE)
	printf ("Matching monospace for %s: %d %d\n",
		t.family_name, dat.monospace_p,
		t.monospace_p);
      match_p = false;
      goto gout;
    }

  if (dat.specified & GS_SPECIFIED_FAMILY
      && strcmp (dat.family_name, t.family_name))
    match_p = false;

 gout:
  ns_done_font_data (&dat);
  ns_done_font_data (&t);

  return match_p;
}

/* Font glyph and metrics caching functions, implemented at end.  */
static void ns_uni_to_glyphs (struct nsfont_info *font_info,
                              unsigned char block);
static void ns_glyph_metrics (struct nsfont_info *font_info,
                              unsigned int block);

#define INVALID_GLYPH 0xFFFF

/* ==========================================================================

    Utilities

   ========================================================================== */


/* Extract family name from a font spec.  */
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
      family = [NSString stringWithUTF8String: tmp];
      xfree (tmp);
      return family;
    }
}

/* Converts FONT_WEIGHT, FONT_SLANT, FONT_WIDTH, plus family and script/lang
   to NSFont descriptor.  Information under extra only needed for matching.  */
static NSFontDescriptor *
ns_spec_to_descriptor (Lisp_Object font_spec)
{
  NSFontDescriptor *fdesc;
  NSMutableDictionary *fdAttrs = [NSMutableDictionary new];
  NSString *family = ns_get_family (font_spec);
  NSMutableDictionary *tdict = [NSMutableDictionary new];

  Lisp_Object tem;

  tem = FONT_SLANT_SYMBOLIC (font_spec);
  if (!NILP (tem))
    {
      if (EQ (tem, Qitalic) || EQ (tem, Qoblique))
	[tdict setObject: [NSNumber numberWithFloat: 1.0]
		  forKey: NSFontSlantTrait];
      else if (EQ (tem, intern ("reverse-italic"))
	       || EQ (tem, intern ("reverse-oblique")))
	[tdict setObject: [NSNumber numberWithFloat: -1.0]
		  forKey: NSFontSlantTrait];
      else
	[tdict setObject: [NSNumber numberWithFloat: 0.0]
		  forKey: NSFontSlantTrait];
    }

  tem = FONT_WIDTH_SYMBOLIC (font_spec);
  if (!NILP (tem))
    {
      if (EQ (tem, Qcondensed))
	[tdict setObject: [NSNumber numberWithFloat: -1.0]
		  forKey: NSFontWidthTrait];
      else if (EQ (tem, Qexpanded))
	[tdict setObject: [NSNumber numberWithFloat: 1.0]
		  forKey: NSFontWidthTrait];
      else
	[tdict setObject: [NSNumber numberWithFloat: 0.0]
		  forKey: NSFontWidthTrait];
    }

  tem = FONT_WEIGHT_SYMBOLIC (font_spec);

  if (!NILP (tem))
    {
      if (EQ (tem, Qbold))
	{
	  [tdict setObject: [NSNumber numberWithFloat: 1.0]
		    forKey: NSFontWeightTrait];
	}
      else if (EQ (tem, Qlight))
	{
	  [tdict setObject: [NSNumber numberWithFloat: -1.0]
		    forKey: NSFontWeightTrait];
	}
      else
	{
	  [tdict setObject: [NSNumber numberWithFloat: 0.0]
		    forKey: NSFontWeightTrait];
	}
    }

  tem = AREF (font_spec, FONT_SPACING_INDEX);

  if (family != nil)
    [fdAttrs setObject: family
		forKey: NSFontFamilyAttribute];

  if (FIXNUMP (tem))
    {
      if (XFIXNUM (tem) != FONT_SPACING_PROPORTIONAL)
	[fdAttrs setObject: [NSNumber numberWithBool: YES]
		    forKey: NSFontFixedAdvanceAttribute];
      else
	[fdAttrs setObject: [NSNumber numberWithBool: NO]
		    forKey: NSFontFixedAdvanceAttribute];
    }

  /* Handle special families such as ``fixed'', ``monospace'' or
     ``Sans Serif''.  */

  if ([family isEqualToString: @"fixed"]
      || [family isEqualToString: @"monospace"])
    [fdAttrs setObject: [[NSFont userFixedPitchFontOfSize: 0] familyName]
		forKey: NSFontFamilyAttribute];
  else if ([family isEqualToString: @"Sans Serif"])
    [fdAttrs setObject: [[NSFont userFontOfSize: 0] familyName]
		forKey: NSFontFamilyAttribute];

  [fdAttrs setObject: tdict forKey: NSFontTraitsAttribute];

  fdesc = [[[NSFontDescriptor fontDescriptorWithFontAttributes: fdAttrs]
	     retain] autorelease];

  [tdict release];
  [fdAttrs release];
  return fdesc;
}


/* Converts NSFont descriptor to FONT_WEIGHT, FONT_SLANT, FONT_WIDTH, etc.  */
static Lisp_Object
ns_descriptor_to_entity (NSFontDescriptor *desc,
                         Lisp_Object extra,
                         const char *style)
{
  Lisp_Object font_entity = font_make_entity ();
  struct gs_font_data data;
  ns_get_font_data (desc, &data);

  ASET (font_entity, FONT_TYPE_INDEX, Qns);
  ASET (font_entity, FONT_FOUNDRY_INDEX, Qns);
  if (data.specified & GS_SPECIFIED_FAMILY)
    ASET (font_entity, FONT_FAMILY_INDEX, intern (data.family_name));
  ASET (font_entity, FONT_ADSTYLE_INDEX, style ? intern (style) : Qnil);
  ASET (font_entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  if (data.specified & GS_SPECIFIED_WEIGHT)
    {
      FONT_SET_STYLE (font_entity, FONT_WEIGHT_INDEX,
		      data.weight == GS_FONT_WEIGHT_BOLD
		      ? Qbold : (data.weight == GS_FONT_WEIGHT_LIGHT
				 ? Qlight : Qnormal));
    }
  else
    FONT_SET_STYLE (font_entity, FONT_WEIGHT_INDEX, Qnormal);

  if (data.specified & GS_SPECIFIED_SLANT)
    {
      FONT_SET_STYLE (font_entity, FONT_SLANT_INDEX,
		      data.slant == GS_FONT_SLANT_ITALIC
		      ? Qitalic : (data.slant == GS_FONT_SLANT_REVERSE_ITALIC
				   ? intern ("reverse-italic") : Qnormal));
    }
  else
    FONT_SET_STYLE (font_entity, FONT_SLANT_INDEX, Qnormal);

  if (data.specified & GS_SPECIFIED_WIDTH)
    {
      FONT_SET_STYLE (font_entity, FONT_WIDTH_INDEX,
		      data.width == GS_FONT_WIDTH_CONDENSED
		      ? Qcondensed : (data.width == GS_FONT_WIDTH_EXPANDED
				      ? intern ("expanded") : Qnormal));
    }
  else
    FONT_SET_STYLE (font_entity, FONT_WIDTH_INDEX, Qnormal);

  ASET (font_entity, FONT_SIZE_INDEX, make_fixnum (0));
  ASET (font_entity, FONT_AVGWIDTH_INDEX, make_fixnum (0));
  ASET (font_entity, FONT_SPACING_INDEX,
	make_fixnum ((data.specified & GS_SPECIFIED_SPACING && data.monospace_p)
		     ? FONT_SPACING_MONO : FONT_SPACING_PROPORTIONAL));

  ASET (font_entity, FONT_EXTRA_INDEX, extra);
  ASET (font_entity, FONT_OBJLIST_INDEX, Qnil);

  if (NSFONT_TRACE)
    {
      fputs ("created font_entity:\n    ", stderr);
      debug_print (font_entity);
    }

  ns_done_font_data (&data);
  return font_entity;
}


/* Default font entity.  */
static Lisp_Object
ns_fallback_entity (void)
{
  return ns_descriptor_to_entity ([[NSFont userFixedPitchFontOfSize: 1] fontDescriptor], Qnil, NULL);
}


/* Utility: get width of a char c in screen font SFONT.  */
static CGFloat
ns_char_width (NSFont *sfont, int c)
{
  CGFloat w = -1.0;
  NSString *cstr = [NSString stringWithFormat: @"%c", c];

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

  if (w < (CGFloat) 0.0)
    {
      NSDictionary *attrsDictionary =
	[NSDictionary dictionaryWithObject: sfont forKey: NSFontAttributeName];
      w = [ascii_printable sizeWithAttributes: attrsDictionary].width;
    }

  return lrint (w / (CGFloat) 95.0);
}


/* Return whether set1 covers set2 to a reasonable extent given by pct.

   The GNUstep bitmap representation doesn't match Apple's
   description.  It appears to be a single block of bytes, not broken
   up into planes, where the last byte contains the highest character
   the character set supports.  */
static BOOL
ns_charset_covers(NSCharacterSet *set1, NSCharacterSet *set2, float pct)
{
  NSData *font = [set1 bitmapRepresentation];
  NSData *script = [set2 bitmapRepresentation];

  uint8_t *fontPlane = (uint8_t *)[font bytes];
  uint8_t *scriptPlane = (uint8_t *)[script bytes];

  int covered = 0, total = 0;

  for (ptrdiff_t b = 0 ; b < [script length] ; b++)
    for (int i = 0 ; i < 8 ; i++)
      {
        if (*(scriptPlane + b) & (1 << i))
          {
            total++;

            if (b < [font length]
                && *(fontPlane + b) & (1 << i))
              covered++;
          }
      }

  return (float)covered / total >= 1.0F - pct;
}


/* Convert :lang property to a script.  Use of :lang property by font backend
   seems to be limited for now (2009/05) to ja, zh, and ko.  */
static NSString
*ns_lang_to_script (Lisp_Object lang)
{
    if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "ja"))
	return @"han";
    /* NOTE: ja given for any hanzi that's also a kanji, but Chinese fonts
             have more characters.  */
    else if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "zh"))
	return @"han";
    else if (!strcmp (SSDATA (SYMBOL_NAME (lang)), "ko"))
	return @"hangul";
    else
	return @"";
}


/* Convert OTF 4-letter script code to emacs script name.  (Why can't
   everyone just use some standard Unicode names for these?)  */
static NSString
*ns_otf_to_script (Lisp_Object otf)
{
    Lisp_Object script = assq_no_quit (XCAR (otf), Votf_script_alist);
    return CONSP (script)
	? [NSString stringWithLispString: SYMBOL_NAME (XCDR ((script)))]
	: @"";
}


/* Convert a font registry.  */
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
            return [NSString stringWithLispString: SYMBOL_NAME (script)];
          }
        rts = XCDR (rts);
      }
    return  @"";
}


/* Searches the :script, :lang, and :otf extra-bundle properties of the spec,
   plus registry regular property, for something that can be mapped to a
   Unicode script.  Empty string returned if no script spec found.  */
static NSString
*ns_get_req_script (Lisp_Object font_spec)
{
    Lisp_Object reg = AREF (font_spec, FONT_REGISTRY_INDEX);
    Lisp_Object extra = AREF (font_spec, FONT_EXTRA_INDEX);

    /* The extra-bundle properties have priority.  */
    for ( ; CONSP (extra); extra = XCDR (extra))
      {
	Lisp_Object tmp = XCAR (extra);
	if (CONSP (tmp))
	  {
	    Lisp_Object key = XCAR (tmp), val = XCDR (tmp);
	    if (EQ (key, QCscript) && SYMBOLP (val))
		return [NSString stringWithLispString: SYMBOL_NAME (val)];
	    if (EQ (key, QClang) && SYMBOLP (val))
		return ns_lang_to_script (val);
	    if (EQ (key, QCotf) && CONSP (val) && SYMBOLP (XCAR (val)))
		return ns_otf_to_script (val);
	  }
      }

    /* If we get here, check the charset portion of the registry.  */
    if (! NILP (reg))
      {
        /* XXX: iso10646 is passed in for non-ascii latin-1 characters
           (which causes box rendering if we don't treat it like iso8858-1)
           but also for ascii (which causes unnecessary font substitution).  */
#if 0
        if (EQ (reg, Qiso10646_1))
          reg = Qiso8859_1;
#endif
        return ns_registry_to_script (SSDATA (SYMBOL_NAME (reg)));
      }

    return @"";
}


/* This small function is static in fontset.c.  If it can be made public for
   all ports, remove this, but otherwise it doesn't seem worth the ifdefs.  */
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
   name into an NSCharacterSet.  */
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
		int start = XFIXNUM (XCAR (XCAR (range_list)));
		int end = XFIXNUM (XCDR (XCAR (range_list)));
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
   (b) need approximate match as fonts covering full Unicode ranges are rare.  */
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
                    /* Some fonts on macOS, maybe many on GNUstep, return nil.  */
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
	[scriptToFamilies setObject: families forKey: script];
      }

    if (NSFONT_TRACE)
      NSLog(@"    returning %lu families", (unsigned long)[families count]);
    return families;
}

/* GNUstep font matching is very mediocre (it can't even compare
   symbolic styles correctly), which is why our own font matching
   mechanism must be implemented.  */

/* Implementation for list and match.  */
static Lisp_Object
ns_findfonts (Lisp_Object font_spec, BOOL isMatch)
{
  Lisp_Object tem, list = Qnil;
  NSFontDescriptor *fdesc;
  NSArray *all_descs;
  GSFontEnumerator *enumerator = [GSFontEnumerator sharedEnumerator];

  NSSet *cFamilies;

  block_input ();
  if (NSFONT_TRACE)
    {
      fprintf (stderr, "nsfont: %s for fontspec:\n    ",
	       (isMatch ? "match" : "list"));
      debug_print (font_spec);
    }

  cFamilies = ns_get_covering_families (ns_get_req_script (font_spec), 0.90);

  fdesc = ns_spec_to_descriptor (font_spec);
  all_descs = [enumerator availableFontDescriptors];

  for (NSFontDescriptor *desc in all_descs)
    {
      if (![cFamilies containsObject:
		  [desc objectForKey: NSFontFamilyAttribute]])
	continue;
      if (!ns_font_descs_match_p (fdesc, desc))
	continue;

      tem = ns_descriptor_to_entity (desc,
				     AREF (font_spec, FONT_EXTRA_INDEX),
				     NULL);
      if (isMatch)
	return tem;
      list = Fcons (tem, list);
    }

  unblock_input ();

  /* Return something if was a match and nothing found.  */
  if (isMatch)
    return ns_fallback_entity ();

  if (NSFONT_TRACE)
    fprintf (stderr, "    Returning %"pD"d entities.\n",
	     list_length (list));

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
   Properties to be considered are same as for list().  */
static Lisp_Object
nsfont_match (struct frame *f, Lisp_Object font_spec)
{
  return ns_findfonts (font_spec, YES);
}


/* List available families.  The value is a list of family names
   (symbols).  */
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

  if (NSFONT_TRACE)
    fprintf (stderr, "nsfont: list families returning %"pD"d entries\n",
	     list_length (list));

  unblock_input ();
  return list;
}


/* Open a font specified by FONT_ENTITY on frame F.  If the font is
   scalable, open it with PIXEL_SIZE.  */
static Lisp_Object
nsfont_open (struct frame *f, Lisp_Object font_entity, int pixel_size)
{
  struct nsfont_info *font_info;
  struct font *font;
  NSFontDescriptor *fontDesc = ns_spec_to_descriptor (font_entity);
  NSFontManager *fontMgr = [NSFontManager sharedFontManager];
  NSString *family;
  NSFont *nsfont, *sfont;
  NSRect brect;
  Lisp_Object font_object;
  Lisp_Object tem;

  block_input ();

  if (NSFONT_TRACE)
    {
      fprintf (stderr, "nsfont: open size %d of fontentity:\n    ", pixel_size);
      debug_print (font_entity);
    }

  if (pixel_size <= 0)
    {
      /* try to get it out of frame params */
      tem = get_frame_param (f, Qfontsize);
      pixel_size = NILP (tem) ? 0 : XFIXNAT (tem);
    }

  tem = AREF (font_entity, FONT_ADSTYLE_INDEX);
  family = ns_get_family (font_entity);
  if (family == nil)
    family = [[NSFont userFixedPitchFontOfSize: 0] familyName];

  nsfont = [NSFont fontWithDescriptor: fontDesc
				 size: pixel_size];

  if (nsfont == nil)
    nsfont = [NSFont userFixedPitchFontOfSize: pixel_size];

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
      return Qnil;
    }

  font_info->glyphs = xzalloc (0x100 * sizeof *font_info->glyphs);
  font_info->metrics = xzalloc (0x100 * sizeof *font_info->metrics);

  /* for metrics */
  sfont = [nsfont screenFont];

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
     * intended.  */
    CGFloat adjusted_descender = [sfont descender] + 0.0001;

    font_info->nsfont = sfont;
    [font_info->nsfont retain];

    /* set up ns_font (defined in nsgui.h) */
    font_info->name = xstrdup (fontName);
    font_info->bold = [fontMgr traitsOfFont: nsfont] & NSBoldFontMask;
    font_info->ital =
      ([fontMgr traitsOfFont: nsfont] & NSItalicFontMask);

    /* Metrics etc.; some fonts return an unusually large max advance, so we
       only use it for fonts that have wide characters.  */
    font_info->width = ([sfont numberOfGlyphs] > 2000) ?
      [sfont maximumAdvancement].width : ns_char_width (sfont, '0');

    brect =  [sfont boundingRectForFont];

    font_info->underpos = [sfont underlinePosition];
    font_info->underwidth = [sfont underlineThickness];
    font_info->size = font->pixel_size;

    /* max bounds */
    font->ascent = font_info->max_bounds.ascent = lrint ([sfont ascender]);
    /* Descender is usually negative.  Use floor to avoid
       clipping descenders.  */
    font->descent =
      font_info->max_bounds.descent = -lrint (floor(adjusted_descender));
    font_info->height =
      font_info->max_bounds.ascent + font_info->max_bounds.descent;
    font_info->max_bounds.width = lrint (font_info->width);
    font_info->max_bounds.lbearing = lrint (brect.origin.x);
    font_info->max_bounds.rbearing =
      lrint (brect.size.width - (CGFloat) font_info->width);

    /* set up metrics portion of font struct */
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


/* Close FONT.  */
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
      xfree (font_info->name);
      font_info->name = NULL;
    }
}


/* If FONT_ENTITY has a glyph for character C (Unicode code point),
   return 1.  If not, return 0.  If a font must be opened to check
   it, return -1.  */
static int
nsfont_has_char (Lisp_Object entity, int c)
{
  return -1;
}


/* Return a glyph code of FONT for character C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
static unsigned int
nsfont_encode_char (struct font *font, int c)
{
  struct nsfont_info *font_info = (struct nsfont_info *)font;
  unsigned char high = (c & 0xff00) >> 8, low = c & 0x00ff;
  unsigned int g;

  if (c > 0xFFFF)
    return FONT_INVALID_CODE;

  /* Did we already cache this block?  */
  if (!font_info->glyphs[high])
    ns_uni_to_glyphs (font_info, high);

  g = font_info->glyphs[high][low];
  return g == INVALID_GLYPH ? FONT_INVALID_CODE : g;
}


/* Perform the size computation of glyphs of FONT and fill in members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS).  */
static void
nsfont_text_extents (struct font *font, const unsigned int *code,
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
   is false when (FROM > 0 || TO < S->nchars).  */
static int
nsfont_draw (struct glyph_string *s, int from, int to, int x, int y,
             bool with_background)
{
  NSGlyph *c = alloca ((to - from) * sizeof *c);

  struct face *face;
  NSRect r;
  struct nsfont_info *font;
  NSColor *col;
  int len = to - from;
  char isComposite = s->first_glyph->type == COMPOSITE_GLYPH;

  block_input ();

  font = (struct nsfont_info *) s->font;
  if (font == NULL)
    font = (struct nsfont_info *)FRAME_FONT (s->f);

  face = s->face;

  r.origin.x = x;
  r.origin.y = y;
  r.size.height = FONT_HEIGHT (font);

  for (int i = 0; i < len; ++i)
    c[i] = s->char2b[i + from];

  /* Fill background if requested.  */
  if (with_background && !isComposite)
    {
      NSRect br = NSMakeRect (x, y - FONT_BASE (s->font),
			      s->width, FONT_HEIGHT (s->font));

      if (!s->face->stipple)
	{
	  if (s->hl != DRAW_CURSOR)
	    [(NS_FACE_BACKGROUND (face) != 0
	      ? [NSColor colorWithUnsignedLong:NS_FACE_BACKGROUND (face)]
	      : FRAME_BACKGROUND_COLOR (s->f)) set];
	  else
	    [FRAME_CURSOR_COLOR (s->f) set];
	}
      else
        {
          struct ns_display_info *dpyinfo = FRAME_DISPLAY_INFO (s->f);
          [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
        }
      NSRectFill (br);
    }

  /* set up for character rendering */
  if (s->hl == DRAW_CURSOR)
    col = FRAME_BACKGROUND_COLOR (s->f);
  else
    col = (NS_FACE_FOREGROUND (face) != 0
	   ? [NSColor colorWithUnsignedLong:NS_FACE_FOREGROUND (face)]
	   : FRAME_FOREGROUND_COLOR (s->f));

  /* render under GNUstep using DPS */
  {
    NSGraphicsContext *context = [NSGraphicsContext currentContext];
    [font->nsfont set];
    [col set];
    DPSmoveto (context, r.origin.x, r.origin.y);
    GSShowGlyphs (context, c, len);
  }

  unblock_input ();
  return to-from;
}

static NSUInteger
ns_font_shape (NSFont *font, NSString *string,
	       struct ns_glyph_layout *glyph_layouts, NSUInteger glyph_len,
	       enum lgstring_direction dir)
{
  NSUInteger i;
  NSUInteger result = 0;
  NSTextStorage *textStorage;
  NSLayoutManager *layoutManager;
  NSTextContainer *textContainer;
  NSUInteger stringLength;
  NSPoint spaceLocation;
  /* numberOfGlyphs can't actually be 0, but this pacifies GCC */
  NSUInteger used, numberOfGlyphs = 0;

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
    emacs_abort ();

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
        [textStorage attribute: NSFontAttributeName
		       atIndex:i
                     longestEffectiveRange: &range
                       inRange: NSMakeRange (0, stringLength)];

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
      NSUInteger *permutation;
      NSRange compRange, range;
      CGFloat totalAdvance;

      glyphIndex = 0;
      while ([layoutManager notShownAttributeForGlyphAtIndex:glyphIndex])
        glyphIndex++;

      permutation = NULL;
#define RIGHT_TO_LEFT_P permutation

      /* Fill the `comp_range' member of struct mac_glyph_layout, and
         setup a permutation for right-to-left text.  */
      compRange = NSMakeRange (0, 0);
      for (range = NSMakeRange (0, 0); NSMaxRange (range) < used;
           range.length++)
        {
          struct ns_glyph_layout *gl = glyph_layouts + NSMaxRange (range);
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
          struct ns_glyph_layout *gl;
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
                  NSUInteger tmp = gl->string_index;

                  gl->string_index = glyph_layouts[i].string_index;
                  glyph_layouts[i].string_index = tmp;
                }
            }
          gl->glyph_id = [layoutManager glyphAtIndex: glyphIndex];

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

static Lisp_Object
nsfont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct nsfont_info *font_info = (struct nsfont_info *) font;
  struct ns_glyph_layout *glyph_layouts;
  NSFont *nsfont = font_info->nsfont;
  ptrdiff_t glyph_len, len, i;
  Lisp_Object tem;
  unichar *mb_buf;
  NSUInteger used;

  glyph_len = LGSTRING_GLYPH_LEN (lgstring);
  for (i = 0; i < glyph_len; ++i)
    {
      tem = LGSTRING_GLYPH (lgstring, i);

      if (NILP (tem))
	break;
    }

  len = i;

  if (INT_MAX / 2 < len)
    memory_full (SIZE_MAX);

  block_input ();

  mb_buf = alloca (len * sizeof *mb_buf);

  for (i = 0; i < len; ++i)
    {
      uint32_t c = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));
      mb_buf[i] = (unichar) c;
    }

  NSString *string = [NSString stringWithCharacters: mb_buf
					     length: len];
  unblock_input ();

  if (!string)
    return Qnil;

  block_input ();

  enum lgstring_direction dir = DIR_UNKNOWN;

  if (EQ (direction, QL2R))
    dir = DIR_L2R;
  else if (EQ (direction, QR2L))
    dir = DIR_R2L;
  glyph_layouts = alloca (sizeof (struct ns_glyph_layout) * glyph_len);
  used = ns_font_shape (nsfont, string, glyph_layouts, glyph_len, dir);

  for (i = 0; i < used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      struct ns_glyph_layout *gl = glyph_layouts + i;
      EMACS_INT from, to;
      struct font_metrics metrics;

      if (NILP (lglyph))
        {
          lglyph = LGLYPH_NEW ();
          LGSTRING_SET_GLYPH (lgstring, i, lglyph);
        }

      from = gl->comp_range.location;
      LGLYPH_SET_FROM (lglyph, from);

      to = gl->comp_range.location + gl->comp_range.length;
      LGLYPH_SET_TO (lglyph, to - 1);

      /* LGLYPH_CHAR is used in `describe-char' for checking whether
         the composition is trivial.  */
      {
        UTF32Char c;

        if (mb_buf[gl->string_index] >= 0xD800
            && mb_buf[gl->string_index] < 0xDC00)
          c = (((mb_buf[gl->string_index] - 0xD800) << 10)
               + (mb_buf[gl->string_index + 1] - 0xDC00) + 0x10000);
        else
          c = mb_buf[gl->string_index];

        LGLYPH_SET_CHAR (lglyph, c);
      }

      {
        unsigned long cc = gl->glyph_id;
        LGLYPH_SET_CODE (lglyph, cc);
      }

      nsfont_text_extents (font, &gl->glyph_id, 1, &metrics);
      LGLYPH_SET_WIDTH (lglyph, metrics.width);
      LGLYPH_SET_LBEARING (lglyph, metrics.lbearing);
      LGLYPH_SET_RBEARING (lglyph, metrics.rbearing);
      LGLYPH_SET_ASCENT (lglyph, metrics.ascent);
      LGLYPH_SET_DESCENT (lglyph, metrics.descent);
    }
  unblock_input ();

  return make_fixnum (used);
}


/* ==========================================================================

    Font glyph and metrics caching functions

   ========================================================================== */

static NSGlyph
ns_uni_to_glyphs_1 (struct nsfont_info *info, unsigned int c)
{
  unichar characters[] = { c };
  NSString *string =
    [NSString stringWithCharacters: characters
			    length: 1];
  NSDictionary *attributes =
    [NSDictionary dictionaryWithObjectsAndKeys:
		    info->nsfont, NSFontAttributeName, nil];
  NSTextStorage *storage = [[NSTextStorage alloc] initWithString: string
						      attributes: attributes];
  NSTextContainer *text_container = [[NSTextContainer alloc] init];
  NSLayoutManager *manager = [[NSLayoutManager alloc] init];

  [manager addTextContainer: text_container];
  [text_container release]; /* Retained by manager */
  [storage addLayoutManager: manager];
  [manager release]; /* Retained by storage */

  NSFont *font_in_storage = [storage attribute: NSFontAttributeName
				       atIndex:0
				effectiveRange: NULL];
  NSGlyph glyph = FONT_INVALID_CODE;

  if ((font_in_storage == info->nsfont
       || [[font_in_storage fontName] isEqualToString: [info->nsfont fontName]]))
    {
      @try
	{
	  glyph = [manager glyphAtIndex: 0];
	}
      @catch (NSException *e)
	{
	  /* GNUstep bug? */
	  glyph = 'X';
	}
    }

  [storage release];

  return glyph;
}

/* Find and cache corresponding glyph codes for unicode values in given
   hi-byte block of 256.  */
static void
ns_uni_to_glyphs (struct nsfont_info *font_info, unsigned char block)
{
  unichar *unichars = xmalloc (0x101 * sizeof (unichar));
  unsigned int i, g, idx;
  unsigned int *glyphs;

  if (NSFONT_TRACE)
    fprintf (stderr, "%p\tFinding glyphs for glyphs in block %d\n",
            font_info, block);

  block_input ();

  font_info->glyphs[block] = xmalloc (0x100 * sizeof (unsigned int));
  if (!unichars || !(font_info->glyphs[block]))
    emacs_abort ();

  /* Create a string containing all Unicode characters in this block.  */
  for (idx = block<<8, i = 0; i < 0x100; idx++, i++)
    if (idx < 0xD800 || idx > 0xDFFF)
      unichars[i] = idx;
    else
      unichars[i] = 0xFEFF;
  unichars[0x100] = 0;

  {
    glyphs = font_info->glyphs[block];
    for (i = 0; i < 0x100; i++, glyphs++)
      {
        g = unichars[i];
	NSGlyph glyph = ns_uni_to_glyphs_1 (font_info, g);
        *glyphs = glyph;
      }
  }

  unblock_input ();
  xfree (unichars);
}


/* Determine and cache metrics for glyphs in given hi-byte block of
   256.  */
static void
ns_glyph_metrics (struct nsfont_info *font_info, unsigned int block)
{
  unsigned int i;
  NSGlyph g;
  unsigned int numGlyphs = [font_info->nsfont numberOfGlyphs];
  NSFont *sfont;
  struct font_metrics *metrics;

  if (NSFONT_TRACE)
    fprintf (stderr, "%p\tComputing metrics for glyphs in block %u\n",
            font_info, block);

  /* not implemented yet (as of startup 0.18), so punt */
  if (numGlyphs == 0)
    numGlyphs = 0x10000;

  block_input ();
  sfont = [font_info->nsfont screenFont];

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

      lb = NSMinX (r);
      rb = NSMaxX (r);

      metrics->rbearing = lrint (rb);
      metrics->lbearing = lrint (lb);

      metrics->descent = - NSMaxY (r);
      metrics->ascent = - NSMinY (r);
    }
  unblock_input ();
}


/* Debugging */
void
ns_dump_glyphstring (struct glyph_string *s)
{
  int i;

  fprintf (stderr, ("Glyph string len = %d at (%d, %d) overhang (%d, %d),"
		    "overlap = %d, bg_filled = %d:"),
           s->nchars, s->x, s->y, s->left_overhang, s->right_overhang,
           s->row->overlapping_p, s->background_filled_p);
  for (i =0; i<s->nchars; i++)
    putc (s->first_glyph[i].u.ch, stderr);
  putc ('\n', stderr);
}

static void syms_of_nsfont_for_pdumper (void);

struct font_driver const nsfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qns),
  .case_sensitive = true,
  .get_cache = nsfont_get_cache,
  .list = nsfont_list,
  .match = nsfont_match,
  .list_family = nsfont_list_family,
  .open_font = nsfont_open,
  .close_font = nsfont_close,
  .has_char = nsfont_has_char,
  .encode_char = nsfont_encode_char,
  .text_extents = nsfont_text_extents,
  .shape = nsfont_shape,
  .draw = nsfont_draw,
  };

void
syms_of_nsfont (void)
{
  DEFSYM (Qcondensed, "condensed");
  DEFSYM (Qexpanded, "expanded");
  DEFSYM (Qmedium, "medium");

  DEFVAR_LISP ("ns-reg-to-script", Vns_reg_to_script,
    doc: /* Internal map of font registry to Unicode script.  */);
  Vns_reg_to_script = Qnil;

  pdumper_do_now_and_after_load (syms_of_nsfont_for_pdumper);
}

static void
syms_of_nsfont_for_pdumper (void)
{
  register_font_driver (&nsfont_driver, NULL);
}
