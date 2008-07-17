/* Font back-end driver for the NeXT/Open/GNUstep and MacOSX window system.
   See font.h
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

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

Author: Adrian Robert (arobert@cogsci.ucsd.edu)
*/

#include "config.h"

#include "lisp.h"
#include "dispextern.h"
#include "composite.h"
#include "blockinput.h"
#include "charset.h"
#include "frame.h"
#include "window.h"
#include "fontset.h"
#include "nsterm.h"
#include "frame.h"
#include "character.h"
#include "font.h"

#define NSFONT_TRACE 0

extern Lisp_Object Qns;
extern Lisp_Object Qnormal, Qbold, Qitalic, Qcondensed, Qexpanded;
static Lisp_Object Qapple, Qroman, Qmedium;
extern Lisp_Object ns_expand_space;
extern Lisp_Object Qappend;
extern int ns_antialias_text, ns_use_qd_smoothing;
extern float ns_antialias_threshold;
extern int ns_tmp_flags;
extern struct nsfont_info *ns_tmp_font;

/* font glyph and metrics caching functions, implemented at end */
static void ns_uni_to_glyphs (struct nsfont_info *font_info,
                              unsigned char block);
static void ns_glyph_metrics (struct nsfont_info *font_info,
                              unsigned char block);


/* ==========================================================================

    Utilities

   ========================================================================== */


/* Replace spaces w/another character so emacs core font parsing routines
   aren't thrown off. */
static void
nsfont_escape_name (char *name)
{
  int i =0, len =strlen (name);
  for ( ; i<len; i++)
    if (name[i] == ' ')
      name[i] = '_';
}


/* Reconstruct spaces in a font family name passed through emacs. */
static void
nsfont_unescape_name (char *name)
{
  int i =0, len =strlen (name);
  for ( ; i<len; i++)
    if (name[i] == '_')
      name[i] = ' ';
}


/* Extract family name from a font spec. */
static NSString *
nsfont_get_family (Lisp_Object font_spec)
{
  Lisp_Object tem = AREF (font_spec, FONT_FAMILY_INDEX);
  if (NILP (tem))
      return nil;
  else
    {
      char *tmp = strdup (SDATA (SYMBOL_NAME (tem)));
      NSString *family;
      nsfont_unescape_name (tmp);
      /* PENDING: this seems to be needed only for font names that are
         hard-coded into emacs, like 'helvetica' for splash screen */
      if (tmp)
        tmp[0] = toupper (tmp[0]);
      family = [NSString stringWithUTF8String: tmp];
      free (tmp);
      return family;
    }
}


/* Converts FONT_WEIGHT, FONT_SLANT, FONT_WIDTH to NSFont traits. */
/* PENDING (20080601): The font backend's strategy for handling font
           styles continues to evolve.  When/if this stabilizes, we
           can change the code here to be more sophisticated and accurate.
           For now, we rely on "normal/plain" style being numeric 100. */
#define STYLE_REF 100
static unsigned int
nsfont_spec_to_traits (Lisp_Object font_spec)
{
  unsigned int traits = 0;
  int n;

  n = FONT_WEIGHT_NUMERIC (font_spec);
  if (n != -1)
      traits |= (n > STYLE_REF) ? NSBoldFontMask : NSUnboldFontMask;

  n = FONT_SLANT_NUMERIC (font_spec);
  if (n != -1)
      traits |= (n > STYLE_REF) ? NSItalicFontMask : NSUnitalicFontMask;

  n = FONT_WIDTH_NUMERIC (font_spec);
  if (n > -1)
    {
      if (n < STYLE_REF - 10)
        traits |= NSCondensedFontMask;
      else if (n > STYLE_REF + 10)
        traits |= NSExpandedFontMask;
    }

/*fprintf (stderr, "  returning traits = %u\n", traits); */
  return traits;
}


/* Converts NSArray of PS name, non-family part, weight, and traits to a
   font backend font-entity. */
static Lisp_Object
nsfont_fmember_to_entity (NSString *family, NSArray *famMember)
{
  Lisp_Object font_entity = font_make_entity ();
  unsigned int traits = [[famMember objectAtIndex: 3] unsignedIntValue];
/*   NSString *psName = [famMember objectAtIndex: 0]; */
  NSMutableString *suffix = [[famMember objectAtIndex: 1] mutableCopy];
  char *escapedFamily = [family UTF8String];

  nsfont_escape_name (escapedFamily);
  [suffix replaceOccurrencesOfString: @" " withString: @"" options: 0
                               range: NSMakeRange (0, [suffix length])];

  ASET (font_entity, FONT_TYPE_INDEX, Qns);
  ASET (font_entity, FONT_FOUNDRY_INDEX, Qapple);
  ASET (font_entity, FONT_FAMILY_INDEX, intern (escapedFamily));
  ASET (font_entity, FONT_ADSTYLE_INDEX, intern ([suffix UTF8String]));
  ASET (font_entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  FONT_SET_STYLE (font_entity, FONT_WEIGHT_INDEX,
      traits & NSBoldFontMask ? Qbold : Qmedium);
  FONT_SET_STYLE (font_entity, FONT_SLANT_INDEX,
      traits & NSItalicFontMask ? Qitalic : Qnormal); /*XXX: should be Qroman */
  FONT_SET_STYLE (font_entity, FONT_WIDTH_INDEX,
      traits & NSCondensedFontMask ? Qcondensed :
        traits & NSExpandedFontMask ? Qexpanded : Qnormal);

  ASET (font_entity, FONT_SIZE_INDEX, make_number (0));
  ASET (font_entity, FONT_EXTRA_INDEX, Qnil);
  ASET (font_entity, FONT_OBJLIST_INDEX, Qnil);

  if (NSFONT_TRACE)
    {
      fprintf (stderr, "created font_entity:\n    ");
      debug_print (font_entity);
     }

  [suffix release];
  return font_entity;
}


/* Computes Hamming distance btwn two "vectors" of 0's and 1's. */
static int
nsfont_trait_distance (unsigned int traits1, unsigned int traits2)
{
  int i, d =0;
  for (i =0; i<sizeof (unsigned int)*8; i++)
    {
      d += (traits1 & 0x1) ^ (traits2 & 0x1);
      traits1 >> 1;
      traits2 >> 1;
    }
  return d;
}


/* Default font entity based on Monaco. */
static Lisp_Object
nsfont_fallback_entity ()
{
  NSString *family = [[NSFont userFixedPitchFontOfSize: 0] familyName];
  NSArray *famMemberSpec = [NSArray arrayWithObjects: family, @"",
                                    [NSNumber numberWithUnsignedInt: 5],
                                    [NSNumber numberWithUnsignedInt: 0], nil];
  return nsfont_fmember_to_entity (family, famMemberSpec);
}


/* ==========================================================================

    Font driver implementation

   ========================================================================== */

static Lisp_Object nsfont_get_cache (FRAME_PTR frame);
static Lisp_Object nsfont_list (Lisp_Object frame, Lisp_Object font_spec);
static Lisp_Object nsfont_match (Lisp_Object frame, Lisp_Object font_spec);
static Lisp_Object nsfont_list_family (Lisp_Object frame);
static Lisp_Object nsfont_open (FRAME_PTR f, Lisp_Object font_entity,
                                 int pixel_size);
static void nsfont_close (FRAME_PTR f, struct font *font);
static int nsfont_has_char (Lisp_Object entity, int c);
static unsigned int nsfont_encode_char (struct font *font, int c);
static int nsfont_text_extents (struct font *font, unsigned int *code,
                                int nglyphs, struct font_metrics *metrics);
static int nsfont_draw (struct glyph_string *s, int from, int to, int x, int y,
                        int with_background);

struct font_driver nsfont_driver =
  {
    0,				/* Qns */
    1,				/* case sensitive */
    nsfont_get_cache,
    nsfont_list,
    nsfont_match,
    nsfont_list_family,
    NULL,			/*free_entity */
    nsfont_open,
    nsfont_close,
    NULL,			/* prepare_face */
    NULL,			/* done_face */
    nsfont_has_char,
    nsfont_encode_char,
    nsfont_text_extents,
    nsfont_draw,
    /* excluded: get_bitmap, free_bitmap, get_outline, free_outline,
                 anchor_point, otf_capability, otf_driver,
      		 start_for_frame, end_for_frame, shape */
  };


/* Return a cache of font-entities on FRAME.  The cache must be a
   cons whose cdr part is the actual cache area.  */
static Lisp_Object
nsfont_get_cache (FRAME_PTR frame)
{
  Display_Info *dpyinfo = FRAME_NS_DISPLAY_INFO (frame);
  return (dpyinfo->name_list_element);
}


/* List fonts exactly matching with FONT_SPEC on FRAME.  The value
   is a vector of font-entities.  This is the sole API that
   allocates font-entities.  */
static Lisp_Object
nsfont_list (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object list = Qnil;
  Lisp_Object tem;
  NSString *family;
  NSArray *families;
  NSEnumerator *famEnum;
  NSFontManager *fontMgr;
  unsigned int traits = nsfont_spec_to_traits (font_spec);

  if (NSFONT_TRACE)
    {
      fprintf (stderr, "nsfont: list for fontspec:\n    ");
      debug_print (font_spec);
    }

  /* if has non-unicode registry, give up */
  tem = AREF (font_spec, FONT_REGISTRY_INDEX);
  if (!EQ (tem, Qiso10646_1) && !EQ (tem, Qunicode_bmp))
    return Qnil;

  fontMgr = [NSFontManager sharedFontManager];

  family = nsfont_get_family (font_spec);

  if (family != nil)
    families = [NSArray arrayWithObject: family];
  else
    families = [fontMgr availableFontFamilies];

  for (famEnum = [families objectEnumerator]; family = [famEnum nextObject]; )
    {
      NSEnumerator *fm;
      NSArray *fmember, *firstMember = nil;
      unsigned int mtraits;
      BOOL foundItal = NO || (traits & NSUnitalicFontMask);
      NSArray *famMembers = [fontMgr availableMembersOfFontFamily: family];
#ifdef NS_IMPL_COCOA
      /* LastResort is special: not a family but a font name only */
      if ([@"LastResort" isEqualToString: family] && [famMembers count] == 0)
        {
          famMembers = [NSArray arrayWithObject: [NSArray arrayWithObjects:
              @"LastResort", @"", [NSNumber numberWithUnsignedInt: 5],
              [NSNumber numberWithUnsignedInt: 0], nil]];
        }
#endif

      /* fmember = [postscriptName style weight traits] */
      fm  = [famMembers objectEnumerator];
      while (fmember = [fm nextObject])
        {
          mtraits = [[fmember objectAtIndex: 3] unsignedIntValue];
          if ((mtraits & traits) == traits)
            {
              list = Fcons (nsfont_fmember_to_entity (family, fmember), list);
              if (mtraits & NSItalicFontMask)
                foundItal = YES;
              if (firstMember == nil)
                firstMember = fmember;
            }
        }
      if (foundItal == NO && firstMember != nil)
        {
          /* no italic member found; add a synthesized one */
          NSMutableArray *smember = [firstMember mutableCopy];
          [smember replaceObjectAtIndex: 1 withObject: @"synthItal" ];
          mtraits = [[fmember objectAtIndex: 3] unsignedIntValue];
          mtraits |= NSItalicFontMask;
          [smember replaceObjectAtIndex: 3
                   withObject: [NSNumber numberWithUnsignedInt: mtraits]];
/*NSLog (@"-- adding synthItal member: %@", smember); */
          list = Fcons (nsfont_fmember_to_entity (family, smember), list);
          [smember release];
        }
    }

  if (NSFONT_TRACE)
      fprintf (stderr, "    Returning %d entities.\n", XINT (Flength (list)));

  return (NILP (list) ? Qnil : Fvconcat (1, &list));/* Qnil was null_vector */
}


/* Return a font entity most closely maching with FONT_SPEC on
   FRAME.  The closeness is determined by the font backend, thus
   `face-font-selection-order' is ignored here.  */
static Lisp_Object
nsfont_match (Lisp_Object frame, Lisp_Object font_spec)
{
  long traits = nsfont_spec_to_traits (font_spec);
  NSFontManager *fontMgr = [NSFontManager sharedFontManager];
  NSString *family;
  Lisp_Object tem;

  if (NSFONT_TRACE)
    {
      fprintf (stderr, "nsfont: match for fontspec:\n    ");
      debug_print (font_spec);
    }

  /* if has non-unicode registry, just return fallback */
#if 0
  tem = AREF (font_spec, FONT_ADSTYLE_INDEX);
  if (!NILP (tem))
    fprintf (stderr, "adstyle: '%s'\n", SDATA (tem));
#endif
  tem = AREF (font_spec, FONT_REGISTRY_INDEX);
  if (!EQ (tem, Qiso10646_1) && !EQ (tem, Qunicode_bmp))
    return nsfont_fallback_entity ();

  family = nsfont_get_family (font_spec);

  if (family != nil)
    {
      /* try to find close font in family */
      NSArray *famMembers = [fontMgr availableMembersOfFontFamily: family];
      NSEnumerator *fm = [famMembers objectEnumerator];
      NSArray *fmember;
      int minDist = sizeof (unsigned int) * 8 + 1;
      int bestMatchIdx = -1;
      int i;

      for (i =0; fmember = [fm nextObject]; i++)
        {
          unsigned int mtraits = [[fmember objectAtIndex: 3] unsignedIntValue];
          int dist = nsfont_trait_distance ((mtraits & traits), traits);
          if (dist < minDist)
            {
              bestMatchIdx = i;
              minDist = dist;
            }
        }
      if (bestMatchIdx != -1)
        return nsfont_fmember_to_entity
          (family, [famMembers objectAtIndex: bestMatchIdx]);
    }

  /* no family that had members was given; find any font matching traits */
  {
    NSArray *fontNames = [fontMgr availableFontNamesWithTraits: traits];
    if (fontNames && [fontNames count]  > 0)
      {
        NSString *fontName = [fontNames objectAtIndex: 0];
        /*PENDING: is there a more efficient way to get family name? */
        NSFont *font = [NSFont fontWithName: fontName size: 0];
        if (font != nil)
          {
            /* now need to get suffix part of name.. */
            NSString *family = [font familyName];
            NSEnumerator *fm = [[fontMgr availableMembersOfFontFamily: family]
                                 objectEnumerator];
            NSArray *fmember;
            while (fmember = [fm nextObject])
              {
                unsigned int mtraits =
                  [[fmember objectAtIndex: 3] unsignedIntValue];
                if (mtraits & traits == traits)
                  return nsfont_fmember_to_entity (family, fmember);
              }
          }
      }
  }

  /* if we get here, return the fallback */
  if (NSFONT_TRACE)
      fprintf (stderr, "    *** returning fallback\n");

  return nsfont_fallback_entity ();
}


/* List available families.  The value is a list of family names
   (symbols). */
static Lisp_Object
nsfont_list_family (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  NSEnumerator *families =
    [[[NSFontManager sharedFontManager] availableFontFamilies]
      objectEnumerator];
  NSString *family;
  while (family = [families nextObject])
      list = Fcons (intern ([family UTF8String]), list);
  /*PENDING: escape the name? */

  if (NSFONT_TRACE)
    fprintf (stderr, "nsfont: list families returning %d entries\n",
            XINT (Flength (list)));

  return list;
}


/* utility: get width of a char c in screen font sfont */
static float
nsfont_char_width (NSFont *sfont, int c)
{
  float w;
  NSString *cstr = [NSString stringWithFormat: @"%c", c];
#ifdef NS_IMPL_COCOA
  NSGlyph glyph = [sfont glyphWithName: cstr];
  if (glyph)
    {
      float w = [sfont advancementForGlyph: glyph].width;
      if (w >= 1.5)
        return w;
    }
#endif
  w = [sfont widthOfString: cstr];
  return max (w, 2.0);
}


/* Open a font specified by FONT_ENTITY on frame F.  If the font is
   scalable, open it with PIXEL_SIZE.  */
static Lisp_Object
nsfont_open (FRAME_PTR f, Lisp_Object font_entity, int pixel_size)
{
  BOOL synthItal;
  struct nsfont_info *font_info;
  struct font *font;
  unsigned int traits = nsfont_spec_to_traits (font_entity);
  NSFontManager *fontMgr = [NSFontManager sharedFontManager];
  NSString *family;
  NSFont *nsfont, *sfont;
  Lisp_Object tem;
  NSRect brect;
  Lisp_Object font_object;
  int i;
  int fixLeopardBug;
#if 0
  static NSMutableDictionary *fontCache = nil;

  /* 2008/03/08: The same font may end up being requested for different
     entities, due to small differences in numeric values or other issues,
     or for different copies of the same entity.  Therefore we cache to
     avoid creating multiple struct font objects (with metrics cache, etc.)
     for the same NSFont object.
     2008/06/01: This is still an issue, but after font backend refactoring
     caching will be more difficult, needs to be reworked before enabling. */
  if (fontCache == nil)
    fontCache = [[NSMutableDictionary alloc] init];
#endif

  font_object = font_make_object (VECSIZE (struct nsfont_info), font_entity,
                                  pixel_size);
  font_info = (struct nsfont_info *) XFONT_OBJECT (font_object);
  font = (struct font *)font_info;
  if (!font)
    return Qnil; /*PENDING: this copies w32, but causes a segfault */

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
  synthItal = !NILP (tem) && !strncmp ("synthItal", SDATA (SYMBOL_NAME (tem)),
                                       9);
  family = nsfont_get_family (font_entity);
  if (NSFONT_TRACE)
    {
      fprintf (stderr, "family: '%s'\ttraits = %ld\tbold = %d\n",
               [family UTF8String], traits, traits & NSBoldFontMask);
    }

  /* see http://cocoadev.com/forums/comments.php?DiscussionID =74 */
  fixLeopardBug = traits & NSBoldFontMask ? 10 : 5;
  nsfont = [fontMgr fontWithFamily: family
                            traits: traits weight: fixLeopardBug
			      size: pixel_size];
  /* if didn't find, try synthetic italic */
  if (nsfont == nil && synthItal && (traits & NSItalicFontMask))
    {
      nsfont = [fontMgr fontWithFamily: family
                                traits: traits & ~NSItalicFontMask
                                weight: fixLeopardBug size: pixel_size];
    }
#ifdef NS_IMPL_COCOA
  /* LastResort not really a family */
  if (nsfont == nil && [@"LastResort" isEqualToString: family])
    {
      nsfont = [NSFont fontWithName: @"LastResort" size: pixel_size];
    }
#endif

  if (nsfont == nil)
    {
      message_with_string ("*** Warning: font in family '%s' not found",
                          build_string ([family UTF8String]), 1);
      nsfont = [NSFont userFixedPitchFontOfSize: pixel_size];
      if (!nsfont)
        {
          fprintf (stderr, "*** Emacs.app: unable to load backup font\n");
          return Qnil;
        }
    }

#if 0
  {
    NSNumber *cached = [fontCache objectForKey: nsfont];
    if (cached != nil && !synthItal)
      {
fprintf (stderr, "*** CACHE HIT!\n");
        struct font_info *existing =
          (struct nsfont_info *)[cached unsignedLongValue];
        /* ... */
      }
    else
      {
        if (!synthItal)
          [fontCache
            setObject: [NSNumber numberWithUnsignedLong:
                                   (unsigned long)font_info]
               forKey: nsfont];
      }
  }
#endif

  font_info->glyphs = (unsigned short *)
    xmalloc (0x100 * sizeof (unsigned short *));
  font_info->metrics = (struct font_metrics *)
    xmalloc (0x100 * sizeof (struct font_metrics *));
  if (!font_info->glyphs || !font_info->metrics)
    return Qnil;
  bzero (font_info->glyphs, 0x100 * sizeof (unsigned short *));
  bzero (font_info->metrics, 0x100 * sizeof (struct font_metrics *));


BLOCK_INPUT;

  /* for metrics */
  sfont = [nsfont screenFont];
  if (sfont == nil)
    sfont = nsfont;

  /* non-metric backend font struct fields */
  font = (struct font *) font_info;
  font->pixel_size = [sfont pointSize];
  font->driver = &nsfont_driver;
  font->encoding_type = FONT_ENCODING_NOT_DECIDED;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  font->default_ascent = 0;
  font->vertical_centering = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->font_encoder = NULL;

  /*PENDING: does anything care about this? */
  font->props[FONT_FORMAT_INDEX] = Qns;
  font->props[FONT_FILE_INDEX] = Qnil;

  {
    double expand, shrink, hshrink;
    float full_height, min_height, hd;
    const char *fontName = [[nsfont fontName] UTF8String];
    int len = strlen (fontName);

#ifdef NS_IMPL_GNUSTEP
    font_info->nsfont = sfont;
#else
    font_info->nsfont = nsfont;
#endif
    [font_info->nsfont retain];

    /* set up ns_font (defined in nsgui.h) */
    font_info->name = (char *)xmalloc (strlen (fontName)+1);
    bcopy (fontName, font_info->name, strlen (fontName)+1);
    font_info->bold = [fontMgr traitsOfFont: nsfont] & NSBoldFontMask;
    font_info->ital =
      synthItal || ([fontMgr traitsOfFont: nsfont] & NSItalicFontMask);

    /* Metrics etc.; some fonts return an unusually large max advance, so we
       only use it for fonts that have wide characters. */
    font_info->width = ([sfont numberOfGlyphs] > 2000) ?
      [sfont maximumAdvancement].width : nsfont_char_width (sfont, '0');

    brect =  [sfont boundingRectForFont];
    full_height = brect.size.height;
    min_height = [sfont ascender] - [sfont descender];
    hd = full_height - min_height;

    if (!NUMBERP (ns_expand_space))
      error ("No expand space defined");

    /* ns_expand_space = 0.0 is use standard height; less shrink, more expand */
    expand = XFLOATINT (ns_expand_space) + 0.5;

    if (expand < 0.0)
      {
        shrink = 1 + expand;
        hshrink = 1 + expand / 2.0;
        expand = 0.0;
      }
    else
      shrink = hshrink = 1.0;

    font_info->underpos = 2; /*[sfont underlinePosition] is often clipped out */
    font_info->underwidth = [sfont underlineThickness];
    font_info->size = font->pixel_size;
    font_info->voffset = lrint (hshrink * [sfont ascender] + expand * hd / 2);

    /* max bounds */
    font_info->max_bounds.ascent =
      lrint (hshrink * [sfont ascender] + expand * hd/2);
    font_info->max_bounds.descent =
      -lrint (hshrink* [sfont descender] - expand*hd/2);
    font_info->height =
      font_info->max_bounds.ascent + font_info->max_bounds.descent;
    font_info->max_bounds.width = lrint (font_info->width);
    font_info->max_bounds.lbearing = lrint (brect.origin.x);
    font_info->max_bounds.rbearing =
      lrint (brect.size.width - font_info->width);
      /*font_info->width + (font_info->ital ? 0.2 * font_info->height : 0); */

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
      font_info->cgfont = CGFontCreateWithPlatformFont ((void*)&atsFont);
    }
#endif

    /* set up metrics portion of font struct */
    font->ascent = [sfont ascender];
    font->descent = -[sfont descender];
    font->min_width = [sfont widthOfString: @"|"]; /* PENDING */
    font->space_width = lrint (nsfont_char_width (sfont, ' '));
    font->average_width = lrint (font_info->width);
    font->max_width = lrint (font_info->max_bounds.width);
    font->height = lrint (font_info->height);
    font->underline_position = lrint (font_info->underpos);
    font->underline_thickness = lrint (font_info->underwidth);

    font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);
    font->props[FONT_FULLNAME_INDEX] =
      make_unibyte_string (font_info->name, strlen (font_info->name));
  }
  UNBLOCK_INPUT;

  return font_object;
}


/* Close FONT on frame F. */
static void
nsfont_close (FRAME_PTR f, struct font *font)
{
  struct nsfont_info *font_info = (struct nsfont_info *)font;
  int i;

  /* PENDING: this occurs apparently due to same failure to detect same font
     that causes need for cache in nsfont_open ()
     (came after unicode-2 -> trunk) */
  if (!font_info)
      return;

  for (i =0; i<0x100; i++)
    {
      if (font_info->glyphs[i])
        xfree (font_info->glyphs[i]);
      if (font_info->metrics[i])
        xfree (font_info->metrics[i]);
    }
  [font_info->nsfont release];
#ifdef NS_IMPL_COCOA
      CGFontRelease (font_info->cgfont);
#endif
      xfree (font_info->name);
      xfree (font_info);
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
/*fprintf (stderr, "mapping char %d -> %d\n", c, g); */
  return g == 0xFFFF ? FONT_INVALID_CODE : g;
}


/* Perform the size computation of glyphs of FONT and fill in members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS). */
static int
nsfont_text_extents (struct font *font, unsigned int *code, int nglyphs,
                     struct font_metrics *metrics)
{
  struct nsfont_info *font_info = (struct nsfont_info *)font;
  struct font_metrics *pcm;
  unsigned char high, low;
  int totalWidth = 0;
  int i;

  bzero (metrics, sizeof (struct font_metrics));

  for (i =0; i<nglyphs; i++)
    {
      /* get metrics for this glyph, filling cache if need be */
      /* PENDING: get metrics for whole string from an NSLayoutManager
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

  return totalWidth; /* not specified in doc, but xfont.c does it */
}


/* Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
   position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
   is nonzero, fill the background in advance.  It is assured that
   WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars). */
static int
nsfont_draw (struct glyph_string *s, int from, int to, int x, int y,
             int with_background)
/* NOTE: focus and clip must be set
     also, currently assumed (true in nsterm.m call) from ==0, to ==nchars */
{
  static char cbuf[1024];
  char *c = cbuf;
#ifdef NS_IMPL_GNUSTEP
  static float advances[1024];
  float *adv = advances;
#else
  static CGSize advances[1024];
  CGSize *adv = advances;
#endif
  struct face *face;
  NSRect r;
  struct nsfont_info *font = ns_tmp_font;
  NSColor *col, *bgCol;
  unsigned short *t = s->char2b;
  int i, len;

  /* Select face based on input flags */
  switch (ns_tmp_flags)
    {
    case NS_DUMPGLYPH_CURSOR:
      face = s->face;
      break;
    case NS_DUMPGLYPH_MOUSEFACE:
      face = FACE_FROM_ID (s->f,
                           FRAME_NS_DISPLAY_INFO (s->f)->mouse_face_face_id);
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
    XCharStruct *cs;
    int cwidth, twidth = 0;
    int hi, lo;
    char isComposite = 0; /* s->first_glyph->type == COMPOSITE_GLYPH; */
    /* PENDING: composition: no vertical displacement is considered. */
    t+= s->gidx; /* advance into composition */
    for (i =0; i<s->nchars - s->gidx; i++, t++)
      {
        hi = (*t & 0xFF00) >> 8;
        lo = *t & 0x00FF;
        if (isComposite)
          {
            cwidth = s->cmp->offsets[s->gidx++ * 2] - twidth;
          }
        else
          {
            if (!font->metrics[hi]) /*PENDING: why/how can we need this now? */
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
  if (with_background)
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
        [(NS_FACE_BACKGROUND (face) != nil
          ? ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f)
          : FRAME_BACKGROUND_COLOR (s->f)) set];
      else
        {
          struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (s->f);
          [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
        }
      NSRectFill (br);
    }


  /* set up for character rendering */
  r.origin.y += font->voffset + (s->height - font->height)/2;

  col = (NS_FACE_FOREGROUND (face) != nil
         ? ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f)
         : FRAME_FOREGROUND_COLOR (s->f));
  /*PENDING: find another way to pass this */
  bgCol = (ns_tmp_flags != NS_DUMPGLYPH_FOREGROUND ? nil
           : (NS_FACE_BACKGROUND (face) != nil
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
        DPSxshow (context, cbuf, advances, len);
        DPSstroke (context);
        [col set];
/*[context GSSetTextDrawingMode: GSTextFill]; /// not implemented yet */
      }

    /* do underline */
    if (face->underline_p)
      {
        if (face->underline_color != nil)
          [ns_lookup_indexed_color (face->underline_color, s->f) set];
        else
          [col set];
        DPSmoveto (context, r.origin.x, r.origin.y + font->underpos);
        DPSlineto (context, r.origin.x+r.size.width, r.origin.y+font->underpos);
        if (face->underline_color != nil)
          [col set];
      }
    else
      [col set];

    /* draw with DPSxshow () */
    DPSmoveto (context, r.origin.x, r.origin.y);
    DPSxshow (context, cbuf, advances, len);
    DPSstroke (context);

    DPSgrestore (context);
    return to-from;
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

    fliptf.c =  font->synthItal ? Fix2X (kATSItalicQDSkew) : 0.0;

    CGContextSetFont (gcontext, font->cgfont);
    CGContextSetFontSize (gcontext, font->size);
    if (ns_antialias_text == NO || font->size <= ns_antialias_threshold)
      CGContextSetShouldAntialias (gcontext, 0);
    else
      CGContextSetShouldAntialias (gcontext, 1);
    if (ns_use_qd_smoothing)
      CGContextSetFontRenderingMode (gcontext, 2); /* 0 is Cocoa, 2 is QD */

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

    if (face->underline_p)
      {
        if (face->underline_color != nil)
          [ns_lookup_indexed_color (face->underline_color, s->f) set];
        else
          [col set];
        CGContextBeginPath (gcontext);
        CGContextMoveToPoint (gcontext,
                              r.origin.x, r.origin.y + font->underpos);
        CGContextAddLineToPoint (gcontext, r.origin.x + r.size.width,
                                r.origin.y + font->underpos);
        CGContextStrokePath (gcontext);
        if (face->underline_color != nil)
          [col set];
      }
    else
      [col set];

    CGContextSetTextPosition (gcontext, r.origin.x, r.origin.y);
    CGContextShowGlyphsWithAdvances (gcontext, s->char2b + s->gidx,
                                    advances, len);

    if (face->overstrike)
      {
        CGContextSetTextPosition (gcontext, r.origin.x+0.5, r.origin.y);
        CGContextShowGlyphsWithAdvances (gcontext, s->char2b + s->gidx,
                                        advances, len);
      }

    CGContextRestoreGState (gcontext);
    return;
  }
#endif  /* NS_IMPL_COCOA */

}


/*  Auto-creates a fontset built around the font in font_object,
    by creating an attributed string with characters from each
    script, then requesting the NS text system to fix attributes
    in range. */
void nsfont_make_fontset_for_font (Lisp_Object name, Lisp_Object font_object)
{
  Lisp_Object script, famAndReg;
  struct nsfont_info *font_info =
    (struct nsfont_info *)XFONT_OBJECT (font_object);

  /* NS text system (and char buf) init */
  static NSTextStorage *store;
  static NSLayoutManager *layout;
  static NSRange range;
  static NSMutableDictionary *attribs;
  static Lisp_Object *scripts;
  static int nscripts;
  static int *scriptsNchars;
  static BOOL firstTime = YES;
  Lisp_Object regString = build_string ("iso10646-1");
  int i, j;

  if (firstTime == YES)
    {
      nscripts = XINT (Flength (Vscript_representative_chars));
      scriptsNchars = (int *) malloc (nscripts * sizeof (int));
      unsigned char *buf = malloc (4*nscripts*sizeof (char));
      Lisp_Object scriptsChars = Vscript_representative_chars;
      unsigned char *tpos = buf;

      scripts = (Lisp_Object *) malloc (nscripts * sizeof (Lisp_Object));

      for (i =0; i<nscripts; i++)
        {
          Lisp_Object sChars = XCAR (scriptsChars);
          Lisp_Object chars = XCDR (sChars);
          unsigned int ch, c =0;
          scripts[i] = XCAR (sChars);

          while (CONSP (chars))
            {
              ch = XUINT (XCAR (chars));
              chars = XCDR (chars);
              CHAR_STRING_ADVANCE (ch, tpos);
              c++;
            }
          scriptsNchars[i] = c;

          scriptsChars = XCDR (scriptsChars);
        }
      *tpos = '\0';

      store = [[NSTextStorage alloc] init];
      layout = [[NSLayoutManager alloc] init];
      [store addLayoutManager: layout];
      [layout release];

      [store beginEditing];
      [[store mutableString] appendString:
                               [NSString stringWithUTF8String: buf]];
      [store endEditing];

      free (buf);
      range = NSMakeRange (0, [store length]);

      attribs = [[NSMutableDictionary alloc] init];
      firstTime = NO;
    }

  /* set the fonts */
  [store beginEditing];
  [store removeAttribute: NSFontAttributeName range: range];
  [attribs setObject: font_info->nsfont forKey: NSFontAttributeName];
  [store addAttributes: attribs range: range];
  [store endEditing];

  /* read them out */
  {
    NSMutableDictionary *map =
      [NSMutableDictionary dictionaryWithCapacity: nscripts * 4];
    NSEnumerator *fonts;
    NSFont *cfont = nil, *tfont;
    NSNumber *n;
    int idx = 0;
    int max;
    for (i =0; i<nscripts; i++)
      {
        [map removeAllObjects];
        for (j =0; j<scriptsNchars[i]; j++)
          {
            cfont = [store attribute: NSFontAttributeName atIndex: idx++
                      effectiveRange: NULL];
            n = [map objectForKey: cfont];
            if (n == nil)
              n = [NSNumber numberWithInt: 1];
            else
              n = [NSNumber numberWithInt: [n intValue] + 1];
            [map setObject: n forKey: cfont];
          }

        /* majority rules */
        max = 0;
        fonts = [map keyEnumerator];
        while (tfont = [fonts nextObject])
          {
            n = [map objectForKey: tfont];
            if ([n intValue] > max)
              {
                cfont = tfont;
                max = [n intValue];
              }
          }

        if (cfont != nil)
          {
            char *family = [[cfont familyName] UTF8String];
            Lisp_Object famAndReg;

            nsfont_escape_name (family);
            famAndReg = Fcons (build_string (family), regString);

            if (NSFONT_TRACE)
              fprintf (stderr, "%s fontset: use '%s' for script '%s'\n",
                      font_info->name, family,
                       SDATA (SYMBOL_NAME (scripts[i])));

            Fset_fontset_font (name, scripts[i], famAndReg, Qnil, Qnil);
          }
        else
          {
            fprintf (stderr, "%s fontset: none found for script '%s'\n",
                    font_info->name, SDATA (SYMBOL_NAME (scripts[i])));
         }
      }  /* for i = script */
  }
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

 BLOCK_INPUT;

#ifdef NS_IMPL_COCOA
  if (firstTime)
    {
      firstTime = 0;
      glyphStorage = [[EmacsGlyphStorage alloc] initWithCapacity: 0x100];
    }
#endif

  font_info->glyphs[block] = xmalloc (0x100 * sizeof (unsigned short));
  if (!unichars || !(font_info->glyphs[block]))
    abort ();

  /* create a string containing all unicode characters in this block */
  for (idx = block<<8, i =0; i<0x100; idx++, i++)
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
    unsigned int gInd =0, cInd =0;

    [glyphStorage setString: allChars font: font_info->nsfont];
    [glyphGenerator generateGlyphsForGlyphStorage: glyphStorage
                        desiredNumberOfCharacters: glyphStorage->maxChar
                                       glyphIndex: &gInd characterIndex: &cInd];
#endif
    glyphs = font_info->glyphs[block];
    for (i =0; i<0x100; i++, glyphs++)
      {
#ifdef NS_IMPL_GNUSTEP
        g = unichars[i];
#else
        g = glyphStorage->cglyphs[i];
        /*PENDING: is this a good check?  maybe need to use coveredChars.. */
        if (g > numGlyphs)
          g = 0xFFFF; /* hopefully unused... */
#endif
        *glyphs = g;
      }

#ifdef NS_IMPL_COCOA
    [allChars release];
#endif
  }

  UNBLOCK_INPUT;
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

 BLOCK_INPUT;
 sfont = [font_info->nsfont screenFont];

  font_info->metrics[block] = xmalloc (0x100 * sizeof (struct font_metrics));
  bzero (font_info->metrics[block], 0x100 * sizeof (struct font_metrics));
  if (!(font_info->metrics[block]))
    abort ();

  metrics = font_info->metrics[block];
  for (g = block<<8, i =0; i<0x100 && g < numGlyphs; g++, i++, metrics++)
    {
      float w, lb, rb;
      NSRect r = [sfont boundingRectForGlyph: g];

#ifdef NS_IMPL_GNUSTEP
      {
        /* lord help us */
        NSString *s = [NSString stringWithFormat: @"%c", g];
        w = [sfont widthOfString: s];
      }
#else
      w = [sfont advancementForGlyph: g].width;
#endif
      w = max (w, 2.0);
      metrics->width = lrint (w);

      lb = r.origin.x;
      rb = r.size.width - w;
      if (lb < 0)
        metrics->lbearing = round (lb);
      if (font_info->ital)
        rb += 0.22 * font_info->height;
      metrics->rbearing = lrint (w + rb);

      metrics->descent = r.origin.y < 0 ? -r.origin.y : 0;
 /*lrint (hshrink * [sfont ascender] + expand * hd/2); */
      metrics->ascent = r.size.height - metrics->descent;
/*-lrint (hshrink* [sfont descender] - expand * hd/2); */
    }
  UNBLOCK_INPUT;
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
  cglyphs = (CGGlyph *)xmalloc (c * sizeof (CGGlyph));
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
  attrStr = [[NSAttributedString alloc] initWithString: str attributes: dict];
  maxChar = [str length];
  maxGlyph = 0;
}

/* NSGlyphStorage protocol */
- (unsigned int)layoutOptions
{
  return 0;
}

- (NSAttributedString *)attributedString
{
  return attrStr;
}

- (void)insertGlyphs: (const NSGlyph *)glyphs length: (unsigned int)length
        forStartingGlyphAtIndex: (unsigned int)glyphIndex
        characterIndex: (unsigned int)charIndex
{
  len = glyphIndex+length;
  for (i =glyphIndex; i<len; i++)
    cglyphs[i] = glyphs[i-glyphIndex];
  if (len > maxGlyph)
    maxGlyph = len;
}

- (void)setIntAttribute: (int)attributeTag value: (int)val
        forGlyphAtIndex: (unsigned)glyphIndex
{
  return;
}

@end
#endif /* NS_IMPL_COCOA */


/* Debugging */
void
dump_glyphstring (struct glyph_string *s)
{
  int i;

  fprintf (stderr, "Glyph string len = %d at (%d, %d) overhang (%d, %d), overlap = %d, bg_filled = %d:",
           s->nchars, s->x, s->y, s->left_overhang, s->right_overhang,
           s->row->overlapping_p, s->background_filled_p);
  for (i =0; i<s->nchars; i++)
    fprintf (stderr, "%c", s->first_glyph[i].u.ch);
  fprintf (stderr, "\n");
}



void
syms_of_nsfont ()
{
  nsfont_driver.type = Qns;
  register_font_driver (&nsfont_driver, NULL);
  DEFSYM (Qapple, "apple");
  DEFSYM (Qroman, "roman");
  DEFSYM (Qmedium, "medium");
}

// arch-tag: d6c3c6f0-62de-4978-8b1e-b7966fe02cae
