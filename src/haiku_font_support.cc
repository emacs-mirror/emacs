/* Haiku window system support.  Hey, Emacs, this is -*- C++ -*-
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <Font.h>
#include <Rect.h>
#include <AffineTransform.h>
#include <FindDirectory.h>
#include <Path.h>
#include <File.h>
#include <Message.h>
#include <OS.h>
#include <Locker.h>
#include <NodeMonitor.h>
#include <Looper.h>

#include <cstring>
#include <cmath>

#include "haiku_support.h"

/* Cache used during font lookup.  It contains an opened font object
   we can look inside, and some previously determined information.  */
struct font_object_cache_bucket
{
  struct font_object_cache_bucket *next;
  unsigned int hash;

  BFont *font_object;
};

static struct font_object_cache_bucket *font_object_cache[2048];

/* The current global monospace family and style.  */
static char *fixed_family, *fixed_style;

/* The current global variable-width family and style.  */
static char *default_family, *default_style;

/* The sizes of each of those fonts.  */
static float default_size, fixed_size;

/* The locker controlling access to those variables.  */
static BLocker default_locker;

/* Haiku doesn't expose font language data in BFont objects.  Thus, we
   select a few representative characters for each supported `:lang'
   (currently Chinese, Korean and Japanese,) and test for those
   instead.  */

static int language_code_points[MAX_LANGUAGE][3] =
  {
    {20154, 20754, 22996}, /* Chinese.  */
    {51312, 49440, 44544}, /* Korean.  */
    {26085, 26412, 12371}, /* Japanese.  */
  };

static void be_send_font_settings (void);

/* Looper used to track changes to system-wide font settings.  */
class EmacsFontMonitorLooper : public BLooper
{
  void
  MessageReceived (BMessage *msg)
  {
    int32 opcode;

    if (msg->what != B_NODE_MONITOR)
      return;

    if (msg->FindInt32 ("opcode", &opcode) != B_OK)
      return;

    if (opcode != B_STAT_CHANGED)
      return;

    /* Wait a little for any message to be completely written after
       the file's modification time changes.  */
    snooze (10000);

    /* Read and apply font settings.  */
    be_send_font_settings ();
  }
};

static unsigned int
hash_string (const char *name_or_style)
{
  unsigned int i;

  i = 3323198485ul;
  for (; *name_or_style; ++name_or_style)
    {
      i ^= *name_or_style;
      i *= 0x5bd1e995;
      i ^= i >> 15;
    }
  return i;
}

static struct font_object_cache_bucket *
cache_font_object_data (const char *family, const char *style,
			BFont *font_object)
{
  uint32_t hash;
  struct font_object_cache_bucket *bucket, *next;

  hash = hash_string (family) ^ hash_string (style);
  bucket = font_object_cache[hash % 2048];

  for (next = bucket; next; next = next->next)
    {
      if (next->hash == hash)
	{
	  delete next->font_object;
	  next->font_object = font_object;

	  return next;
	}
    }

  next = new struct font_object_cache_bucket;
  next->font_object = font_object;
  next->hash = hash;
  next->next = bucket;
  font_object_cache[hash % 2048] = next;
  return next;
}

static struct font_object_cache_bucket *
lookup_font_object_data (const char *family, const char *style)
{
  uint32_t hash;
  struct font_object_cache_bucket *bucket, *next;

  hash = hash_string (family) ^ hash_string (style);
  bucket = font_object_cache[hash % 2048];

  for (next = bucket; next; next = next->next)
    {
      if (next->hash == hash)
	return next;
    }

  return NULL;
}

static bool
font_object_has_chars (struct font_object_cache_bucket *cached,
		       int *chars, int nchars, bool just_one_of)
{
  int i;

  for (i = 0; i < nchars; ++i)
    {
      if (just_one_of
	  && cached->font_object->IncludesBlock (chars[i],
						 chars[i]))
	return true;

      if (!just_one_of
	  && !cached->font_object->IncludesBlock (chars[i],
						  chars[i]))
	return false;
    }

  return !just_one_of;
}

static void
estimate_font_ascii (BFont *font, int *max_width,
		     int *min_width, int *avg_width)
{
  char ch[2];
  bool tems[1];
  int total = 0;
  int count = 0;
  int min = 0;
  int max = 0;

  std::memset (ch, 0, sizeof ch);
  for (ch[0] = 32; ch[0] < 127; ++ch[0])
    {
      tems[0] = false;
      font->GetHasGlyphs (ch, 1, tems);
      if (tems[0])
	{
	  int w = font->StringWidth (ch);
	  ++count;
	  total += w;

	  if (!min || min > w)
	    min = w;
	  if (max < w)
	    max = w;
	}
    }

  *min_width = min;
  *max_width = max;

  if (count)
    *avg_width = total / count;
  else
    *avg_width = 0;
}

void
BFont_close (void *font)
{
  if (font != (void *) be_fixed_font &&
      font != (void *) be_plain_font &&
      font != (void *) be_bold_font)
    delete (BFont *) font;
}

void
BFont_metrics (void *font, int *px_size, int *min_width, int *max_width,
	       int *avg_width, int *height, int *space_width, int *ascent,
	       int *descent, int *underline_position, int *underline_thickness)
{
  BFont *ft = (BFont *) font;
  struct font_height fheight;
  bool have_space_p;

  char atem[1];
  bool otem[1];

  ft->GetHeight (&fheight);
  atem[0] = ' ';
  otem[0] = false;
  ft->GetHasGlyphs (atem, 1, otem);
  have_space_p = otem[0];

  estimate_font_ascii (ft, max_width, min_width, avg_width);
  *ascent = std::lrint (fheight.ascent);
  *descent = std::lrint (fheight.descent);
  *height = *ascent + *descent;

  *space_width = have_space_p ? ft->StringWidth (" ") : 0;

  *px_size = std::lrint (ft->Size ());
  *underline_position = 0;
  *underline_thickness = 0;
}

/* Return non-null if FONT contains CHR, a Unicode code-point.  */
int
BFont_have_char_p (void *font, int32_t chr)
{
  BFont *ft = (BFont *) font;
  return ft->IncludesBlock (chr, chr);
}

/* Return non-null if font contains a block from BEG to END.  */
int
BFont_have_char_block (void *font, int32_t beg, int32_t end)
{
  BFont *ft = (BFont *) font;
  return ft->IncludesBlock (beg, end);
}

/* Compute bounds for MB_STR, a character in multibyte encoding, used
   with FONT.  The distance to move rightwards before reaching to the
   next character's left escapement boundary is returned in ADVANCE,
   the left bearing in LB, and the right bearing in RB.

   The left bearing is the amount of pixels from the left escapement
   boundary (origin) to the left-most pixel that constitutes the glyph
   corresponding to mb_str, and RB is the amount of pixels from the
   origin to the right-most pixel constituting the glyph.

   Both the left and right bearings are positive values measured
   towards the right, which means that the left bearing will only be
   negative if the left-most pixel is to the left of the origin.

   The bearing values correspond to X11 XCharStruct semantics, which
   is what Emacs code operates on.  Haiku itself uses a slightly
   different scheme, where the "left edge" is the distance from the
   origin to the left-most pixel, where leftwards is negative and
   rightwards is positive, and the "right edge" is the distance (where
   leftwards is similarly negative) between the right-most pixel and
   the right escapement boundary, which is the left escapement
   boundary plus the advance.  */
void
BFont_char_bounds (void *font, const char *mb_str, int *advance,
		   int *lb, int *rb)
{
  BFont *ft = (BFont *) font;
  edge_info edge_info;
  float size, escapement;
  size = ft->Size ();

  ft->GetEdges (mb_str, 1, &edge_info);
  ft->GetEscapements (mb_str, 1, &escapement);
  *advance = std::lrint (escapement * size);
  *lb =  std::lrint (edge_info.left * size);
  *rb = *advance + std::lrint (edge_info.right * size);
}

/* The same, but for a variable amount of chars.  */
void
BFont_nchar_bounds (void *font, const char *mb_str, int *advance,
		    int *lb, int *rb, int32_t n)
{
  BFont *ft = (BFont *) font;
  edge_info edge_info[n];
  float size;
  float escapement[n];

  size = ft->Size ();

  ft->GetEdges (mb_str, n, edge_info);
  ft->GetEscapements (mb_str, n, (float *) escapement);

  for (int32_t i = 0; i < n; ++i)
    {
      advance[i] = std::lrint (escapement[i] * size);
      lb[i] = advance[i] - std::lrint (edge_info[i].left * size);
      rb[i] = advance[i] + std::lrint (edge_info[i].right * size);
    }
}

static void
font_style_to_flags (const char *style_string,
		     struct haiku_font_pattern *pattern)
{
  char *style;
  char *token;
  int tok = 0;

  style = strdup (style_string);

  if (!style)
    return;

  pattern->weight = NO_WEIGHT;
  pattern->width = NO_WIDTH;
  pattern->slant = NO_SLANT;

  while ((token = std::strtok (!tok ? style : NULL, " ")) && tok < 3)
    {
      if (token && !strcmp (token, "Thin"))
	pattern->weight = HAIKU_THIN;
      else if (token && (!strcmp (token, "UltraLight")
			 || !strcmp (token, "ExtraLight")))
	pattern->weight = HAIKU_EXTRALIGHT;
      else if (token && !strcmp (token, "Light"))
	pattern->weight = HAIKU_LIGHT;
      else if (token && !strcmp (token, "SemiLight"))
	pattern->weight = HAIKU_SEMI_LIGHT;
      else if (token && !strcmp (token, "Regular"))
	{
	  if (pattern->slant == NO_SLANT)
	    pattern->slant = SLANT_REGULAR;

	  if (pattern->width == NO_WIDTH)
	    pattern->width = NORMAL_WIDTH;

	  if (pattern->weight == NO_WEIGHT)
	    pattern->weight = HAIKU_REGULAR;
	}
      else if (token && (!strcmp (token, "SemiBold")
			 /* Likewise, this was reported by a user.  */
			 || !strcmp (token, "Semibold")))
	pattern->weight = HAIKU_SEMI_BOLD;
      else if (token && !strcmp (token, "Bold"))
	pattern->weight = HAIKU_BOLD;
      else if (token && (!strcmp (token, "ExtraBold")
			 /* This has actually been seen in the wild.  */
			 || !strcmp (token, "Extrabold")
			 || !strcmp (token, "UltraBold")))
	pattern->weight = HAIKU_EXTRA_BOLD;
      else if (token && !strcmp (token, "Book"))
	pattern->weight = HAIKU_BOOK;
      else if (token && !strcmp (token, "Heavy"))
	pattern->weight = HAIKU_HEAVY;
      else if (token && !strcmp (token, "UltraHeavy"))
	pattern->weight = HAIKU_ULTRA_HEAVY;
      else if (token && !strcmp (token, "Black"))
	pattern->weight = HAIKU_BLACK;
      else if (token && !strcmp (token, "Medium"))
	pattern->weight = HAIKU_MEDIUM;
      else if (token && !strcmp (token, "Oblique"))
	pattern->slant = SLANT_OBLIQUE;
      else if (token && !strcmp (token, "Italic"))
	pattern->slant = SLANT_ITALIC;
      else if (token && !strcmp (token, "UltraCondensed"))
	pattern->width = ULTRA_CONDENSED;
      else if (token && !strcmp (token, "ExtraCondensed"))
	pattern->width = EXTRA_CONDENSED;
      else if (token && !strcmp (token, "Condensed"))
	pattern->width = CONDENSED;
      else if (token && !strcmp (token, "SemiCondensed"))
	pattern->width = SEMI_CONDENSED;
      else if (token && !strcmp (token, "SemiExpanded"))
	pattern->width = SEMI_EXPANDED;
      else if (token && !strcmp (token, "Expanded"))
	pattern->width = EXPANDED;
      else if (token && !strcmp (token, "ExtraExpanded"))
	pattern->width = EXTRA_EXPANDED;
      else if (token && !strcmp (token, "UltraExpanded"))
	pattern->width = ULTRA_EXPANDED;
      else
	{
	  tok = 1000;
	  break;
	}
      tok++;
    }

  if (pattern->weight != NO_WEIGHT)
    pattern->specified |= FSPEC_WEIGHT;
  if (pattern->slant != NO_SLANT)
    pattern->specified |= FSPEC_SLANT;
  if (pattern->width != NO_WIDTH)
    pattern->specified |= FSPEC_WIDTH;

  if (tok > 3)
    {
      pattern->specified &= ~FSPEC_SLANT;
      pattern->specified &= ~FSPEC_WEIGHT;
      pattern->specified &= ~FSPEC_WIDTH;
      pattern->specified |= FSPEC_STYLE;
      std::strncpy ((char *) &pattern->style,
		    style_string,
		    sizeof pattern->style - 1);
      pattern->style[sizeof pattern->style - 1] = '\0';
    }

  free (style);
}

static bool
font_check_wanted_chars (struct haiku_font_pattern *pattern, font_family family,
			 char *style)
{
  BFont *ft;
  static struct font_object_cache_bucket *cached;
  unicode_block wanted_block;

  cached = lookup_font_object_data (family, style);
  if (cached)
    ft = cached->font_object;
  else
    {
      ft = new BFont;

      if (ft->SetFamilyAndStyle (family, style) != B_OK)
	{
	  delete ft;
	  return false;
	}

      cached = cache_font_object_data (family, style, ft);
    }

  return font_object_has_chars (cached, pattern->wanted_chars,
				pattern->want_chars_len, false);
}

static bool
font_check_one_of (struct haiku_font_pattern *pattern, font_family family,
		   char *style)
{
  BFont *ft;
  static struct font_object_cache_bucket *cached;
  unicode_block wanted_block;

  cached = lookup_font_object_data (family, style);
  if (cached)
    ft = cached->font_object;
  else
    {
      ft = new BFont;

      if (ft->SetFamilyAndStyle (family, style) != B_OK)
	{
	  delete ft;
	  return false;
	}

      cached = cache_font_object_data (family, style, ft);
    }

  return font_object_has_chars (cached, pattern->need_one_of,
				pattern->need_one_of_len, true);
}

static bool
font_check_language (struct haiku_font_pattern *pattern, font_family family,
		     char *style)
{
  BFont *ft;
  static struct font_object_cache_bucket *cached;

  cached = lookup_font_object_data (family, style);
  if (cached)
    ft = cached->font_object;
  else
    {
      ft = new BFont;

      if (ft->SetFamilyAndStyle (family, style) != B_OK)
	{
	  delete ft;
	  return false;
	}

      cached = cache_font_object_data (family, style, ft);
    }

  if (pattern->language == MAX_LANGUAGE)
    return false;

  return font_object_has_chars (cached, language_code_points[pattern->language],
				3, false);
}

static bool
font_family_style_matches_p (font_family family, char *style, uint32_t flags,
			     struct haiku_font_pattern *pattern,
			     int ignore_flags_p = 0)
{
  struct haiku_font_pattern m;
  m.specified = 0;

  if (style)
    font_style_to_flags (style, &m);

  if ((pattern->specified & FSPEC_FAMILY)
      && strcmp ((char *) &pattern->family, family))
    return false;

  if (!ignore_flags_p && (pattern->specified & FSPEC_SPACING)
      && !(pattern->mono_spacing_p) != !(flags & B_IS_FIXED))
    return false;

  if (pattern->specified & FSPEC_STYLE)
    return style && !strcmp (style, pattern->style);
  /* Don't allow matching fonts with an adstyle if no style was
     specified in the query pattern.  */
  else if (m.specified & FSPEC_STYLE)
    return false;

  if ((pattern->specified & FSPEC_WEIGHT)
      && (pattern->weight
	  != ((m.specified & FSPEC_WEIGHT) ? m.weight : HAIKU_REGULAR)))
    return false;

  if ((pattern->specified & FSPEC_SLANT)
      && (pattern->slant
	  != (m.specified & FSPEC_SLANT
	      ? m.slant : SLANT_REGULAR)))
    return false;

  if ((pattern->specified & FSPEC_WANTED)
      && !font_check_wanted_chars (pattern, family, style))
    return false;

  if ((pattern->specified & FSPEC_WIDTH)
      && (pattern->width
	  != (m.specified & FSPEC_WIDTH
	      ? m.width : NORMAL_WIDTH)))
    return false;

  if ((pattern->specified & FSPEC_NEED_ONE_OF)
      && !font_check_one_of (pattern, family, style))
    return false;

  if ((pattern->specified & FSPEC_LANGUAGE)
      && !font_check_language (pattern, family, style))
    return false;

  return true;
}

static void
haiku_font_fill_pattern (struct haiku_font_pattern *pattern,
			 font_family family, char *style,
			 uint32_t flags)
{
  if (style)
    font_style_to_flags (style, pattern);

  pattern->specified |= FSPEC_FAMILY;
  std::strncpy (pattern->family, family,
		sizeof pattern->family - 1);
  pattern->family[sizeof pattern->family - 1] = '\0';
  pattern->specified |= FSPEC_SPACING;
  pattern->mono_spacing_p = flags & B_IS_FIXED;
}

/* Delete every element of the font pattern PT.  */
void
haiku_font_pattern_free (struct haiku_font_pattern *pt)
{
  struct haiku_font_pattern *tem = pt;
  while (tem)
    {
      struct haiku_font_pattern *t = tem;
      tem = t->next;
      delete t;
    }
}

/* Find all fonts matching the font pattern PT.  */
struct haiku_font_pattern *
BFont_find (struct haiku_font_pattern *pt)
{
  struct haiku_font_pattern *r = NULL;
  font_family name;
  font_style sname;
  uint32 flags;
  int sty_count, fam_count, si, fi;
  struct haiku_font_pattern *p, *head, *n;
  bool oblique_seen_p;

  fam_count = count_font_families ();

  for (fi = 0; fi < fam_count; ++fi)
    {
      if (get_font_family (fi, &name, &flags) == B_OK)
	{
	  sty_count = count_font_styles (name);
	  if (!sty_count
	      && font_family_style_matches_p (name, NULL, flags, pt))
	    {
	      p = new struct haiku_font_pattern;
	      p->specified = 0;
	      p->oblique_seen_p = 1;
	      haiku_font_fill_pattern (p, name, NULL, flags);
	      p->next = r;
	      if (p->next)
		p->next->last = p;
	      p->last = NULL;
	      p->next_family = r;
	      r = p;

	      if (pt->specified & FSPEC_ANTIALIAS)
		{
		  p->specified |= FSPEC_ANTIALIAS;
		  p->use_antialiasing = pt->use_antialiasing;
		}
	    }
	  else if (sty_count)
	    {
	      for (si = 0; si < sty_count; ++si)
		{
		  oblique_seen_p = 0;
		  head = r;
		  p = NULL;

		  if (get_font_style (name, si, &sname, &flags) == B_OK)
		    {
		      if (font_family_style_matches_p (name, (char *) &sname, flags, pt))
			{
			  p = new struct haiku_font_pattern;
			  p->specified = 0;
			  haiku_font_fill_pattern (p, name, (char *) &sname, flags);

			  /* Add the indices to this font now so we
			     won't have to loop over each font in
			     order to open it later.  */

			  p->specified |= FSPEC_INDICES;
			  p->family_index = fi;
			  p->style_index = si;

			  if (pt->specified & FSPEC_ANTIALIAS)
			    {
			      p->specified |= FSPEC_ANTIALIAS;
			      p->use_antialiasing = pt->use_antialiasing;
			    }

			  if (p->specified & FSPEC_SLANT
			      && (p->slant == SLANT_OBLIQUE
				  || p->slant == SLANT_ITALIC))
			    oblique_seen_p = 1;

			  p->next = r;
			  if (p->next)
			    p->next->last = p;
			  r = p;
			  p->next_family = head;
			}
		    }

		  if (p)
		    p->last = NULL;

		  for (; head; head = head->last)
		    head->oblique_seen_p = oblique_seen_p;
		}
	    }
	}
    }

  /* There's a very good chance that this result will get cached if no
     slant is specified.  Thus, we look through each font that hasn't
     seen an oblique style, and add one.  */

  if (!(pt->specified & FSPEC_SLANT))
    {
      /* r->last is invalid from here onwards.  */
      for (p = r; p;)
	{
	  if (!p->oblique_seen_p)
	    {
	      n = new haiku_font_pattern;
	      *n = *p;

	      n->slant = SLANT_OBLIQUE;

	      /* Opening a font by its indices doesn't provide enough
		 information to synthesize the oblique font later.  */
	      n->specified &= ~FSPEC_INDICES;
	      p->next = n;
	      p = p->next_family;
	    }
	  else
	    p = p->next_family;
	}
    }

  return r;
}

/* Find and open a font with the family at FAMILY and the style at
   STYLE, and set its size to SIZE.  Value is NULL if opening the font
   failed.  */
void *
be_open_font_at_index (int family, int style, float size)
{
  font_family family_name;
  font_style style_name;
  uint32 flags;
  status_t rc;
  BFont *font;

  rc = get_font_family (family, &family_name, &flags);

  if (rc != B_OK)
    return NULL;

  rc = get_font_style (family_name, style, &style_name, &flags);

  if (rc != B_OK)
    return NULL;

  font = new BFont;

  rc = font->SetFamilyAndStyle (family_name, style_name);

  if (rc != B_OK)
    {
      delete font;
      return NULL;
    }

  font->SetSize (size);
  font->SetEncoding (B_UNICODE_UTF8);
  font->SetSpacing (B_BITMAP_SPACING);
  return font;
}

/* Find and open a font matching the pattern PAT, which must have its
   family set.  */
int
BFont_open_pattern (struct haiku_font_pattern *pat, void **font, float size)
{
  int sty_count, si, code;
  font_family name;
  font_style sname;
  BFont *ft;
  uint32 flags = 0;
  struct haiku_font_pattern copy;

  if (!(pat->specified & FSPEC_FAMILY))
    return 1;

  strncpy (name, pat->family, sizeof name - 1);
  name[sizeof name - 1] = '\0';

  sty_count = count_font_styles (name);

  if (!sty_count
      && font_family_style_matches_p (name, NULL, flags, pat, 1))
    {
      ft = new BFont;
      ft->SetSize (size);
      ft->SetEncoding (B_UNICODE_UTF8);
      ft->SetSpacing (B_BITMAP_SPACING);

      if (ft->SetFamilyAndStyle (name, NULL) != B_OK)
	{
	  delete ft;
	  return 1;
	}
      *font = (void *) ft;
      return 0;
    }
  else if (sty_count)
    {
      for (si = 0; si < sty_count; ++si)
	{
	  if (get_font_style (name, si, &sname, &flags) == B_OK
	      && font_family_style_matches_p (name, (char *) &sname,
					      flags, pat))
	    {
	      ft = new BFont;
	      ft->SetSize (size);
	      ft->SetEncoding (B_UNICODE_UTF8);
	      ft->SetSpacing (B_BITMAP_SPACING);

	      if (ft->SetFamilyAndStyle (name, sname) != B_OK)
		{
		  delete ft;
		  return 1;
		}

	      *font = (void *) ft;
	      return 0;
	    }
	}
    }

  if (pat->specified & FSPEC_SLANT && pat->slant == SLANT_OBLIQUE)
    {
      copy = *pat;
      copy.slant = SLANT_REGULAR;
      code = BFont_open_pattern (&copy, font, size);

      if (code)
	return code;

      ft = (BFont *) *font;
      /* XXX Font measurements don't respect shear.  Haiku bug?
	 This apparently worked in BeOS.
	 ft->SetShear (100.0); */
      ft->SetFace (B_ITALIC_FACE);
      return 0;
    }

  return 1;
}

/* Query the family of the default fixed font.  */
void
BFont_populate_fixed_family (struct haiku_font_pattern *ptn)
{
  font_family f;
  font_style s;
  be_fixed_font->GetFamilyAndStyle (&f, &s);

  ptn->specified |= FSPEC_FAMILY;
  strncpy (ptn->family, f, sizeof ptn->family - 1);
  ptn->family[sizeof ptn->family - 1] = '\0';
}

void
BFont_populate_plain_family (struct haiku_font_pattern *ptn)
{
  font_family f;
  font_style s;
  be_plain_font->GetFamilyAndStyle (&f, &s);

  ptn->specified |= FSPEC_FAMILY;
  strncpy (ptn->family, f, sizeof ptn->family - 1);
  ptn->family[sizeof ptn->family - 1] = '\0';
}

haiku_font_family_or_style *
be_list_font_families (size_t *length)
{
  int32 families = count_font_families ();
  haiku_font_family_or_style *array;
  int32 idx;
  uint32 flags;

  array = (haiku_font_family_or_style *) malloc (sizeof *array * families);

  if (!array)
    return NULL;

  for (idx = 0; idx < families; ++idx)
    {
      if (get_font_family (idx, &array[idx], &flags) != B_OK)
	array[idx][0] = '\0';
    }

  *length = families;

  return array;
}

void
be_init_font_data (void)
{
  memset (&font_object_cache, 0, sizeof font_object_cache);
}

/* Free the font object cache.  This is called every 50 updates of a
   frame.  */
void
be_evict_font_cache (void)
{
  struct font_object_cache_bucket *bucket, *last;
  int i;

  for (i = 0; i < 2048; ++i)
    {
      bucket = font_object_cache[i];

      while (bucket)
	{
	  last = bucket;
	  bucket = bucket->next;
	  delete last->font_object;
	  delete last;
	}

      font_object_cache[i] = NULL;
    }
}

void
be_font_style_to_flags (const char *style, struct haiku_font_pattern *pattern)
{
  pattern->specified = 0;

  font_style_to_flags (style, pattern);
}

int
be_find_font_indices (struct haiku_font_pattern *pattern,
		      int *family_index, int *style_index)
{
  int32 i, j, n_families, n_styles;
  font_family family;
  font_style style;
  uint32 flags;

  n_families = count_font_families ();

  for (i = 0; i < n_families; ++i)
    {
      if (get_font_family (i, &family, &flags) == B_OK)
	{
	  n_styles = count_font_styles (family);

	  for (j = 0; j < n_styles; ++j)
	    {
	      if (get_font_style (family, j, &style, &flags) == B_OK
		  && font_family_style_matches_p (family, style,
						  flags, pattern))
		{
		  *family_index = i;
		  *style_index = j;

		  return 0;
		}
	    }
	}
    }

  return 1;
}

void
be_set_font_antialiasing (void *font, bool antialias_p)
{
  BFont *font_object;

  font_object = (BFont *) font;
  font_object->SetFlags (antialias_p
			 ? B_FORCE_ANTIALIASING
			 : B_DISABLE_ANTIALIASING);
}

static void
be_send_font_settings (void)
{
  struct haiku_font_change_event rq;
  BFile file;
  BPath path;
  status_t rc;
  BMessage message;
  font_family family;
  font_style style;
  const char *new_family, *new_style;
  float new_size;

  rc = find_directory (B_USER_SETTINGS_DIRECTORY, &path);

  if (rc < B_OK)
    return;

  rc = path.Append ("system/app_server/fonts");

  if (rc < B_OK)
    return;

  if (file.SetTo (path.Path (), B_READ_ONLY) != B_OK)
    return;

  if (message.Unflatten (&file) != B_OK)
    return;

  /* Now, populate with new values.  */
  if (!default_locker.Lock ())
    gui_abort ("Failed to lock font data locker");

  /* Obtain default values.  */
  be_fixed_font->GetFamilyAndStyle (&family, &style);
  default_size = be_fixed_font->Size ();

  /* And the new values.  */
  new_family = message.GetString ("fixed family", family);
  new_style = message.GetString ("fixed style", style);
  new_size = message.GetFloat ("fixed size", default_size);

  /* If it turns out the fixed family changed, send the new family and
     style.  */

  if (!fixed_family || !fixed_style
      || new_size != fixed_size
      || strcmp (new_family, fixed_family)
      || strcmp (new_style, fixed_style))
    {
      memset (&rq, 0, sizeof rq);
      strncpy (rq.new_family, (char *) new_family,
	       sizeof rq.new_family - 1);
      strncpy (rq.new_style, (char *) new_style,
	       sizeof rq.new_style - 1);
      rq.new_size = new_size;
      rq.what = FIXED_FAMILY;

      haiku_write (FONT_CHANGE_EVENT, &rq);
    }

  if (fixed_family)
    free (fixed_family);

  if (fixed_style)
    free (fixed_style);

  fixed_family = strdup (new_family);
  fixed_style = strdup (new_style);
  fixed_size = new_size;

  /* Obtain default values.  */
  be_plain_font->GetFamilyAndStyle (&family, &style);
  default_size = be_plain_font->Size ();

  /* And the new values.  */
  new_family = message.GetString ("plain family", family);
  new_style = message.GetString ("plain style", style);
  new_size = message.GetFloat ("plain style", default_size);

  if (!default_family || !default_style
      || new_size != default_size
      || strcmp (new_family, default_family)
      || strcmp (new_style, default_style))
    {
      memset (&rq, 0, sizeof rq);
      strncpy (rq.new_family, (char *) new_family,
	       sizeof rq.new_family - 1);
      strncpy (rq.new_style, (char *) new_style,
	       sizeof rq.new_style - 1);
      rq.new_size = new_size;
      rq.what = DEFAULT_FAMILY;

      haiku_write (FONT_CHANGE_EVENT, &rq);
    }

  if (default_family)
    free (default_family);

  if (default_style)
    free (default_style);

  default_family = strdup (new_family);
  default_style = strdup (new_style);
  default_size = new_size;

  default_locker.Unlock ();
}

/* Begin listening to font settings changes, by installing a node
   watcher.  This relies on the settings file already being present
   and has several inherent race conditions, but users shouldn't be
   changing font settings very quickly.  */

void
be_listen_font_settings (void)
{
  BPath path;
  status_t rc;
  BNode node;
  node_ref node_ref;
  EmacsFontMonitorLooper *looper;
  font_family family;
  font_style style;

  /* Set up initial values.  */
  be_fixed_font->GetFamilyAndStyle (&family, &style);
  fixed_family = strdup (family);
  fixed_style = strdup (style);
  fixed_size = be_fixed_font->Size ();

  be_plain_font->GetFamilyAndStyle (&family, &style);
  default_family = strdup (family);
  default_style = strdup (style);
  default_size = be_plain_font->Size ();

  rc = find_directory (B_USER_SETTINGS_DIRECTORY, &path);

  if (rc < B_OK)
    return;

  rc = path.Append ("system/app_server/fonts");

  if (rc < B_OK)
    return;

  rc = node.SetTo (path.Path ());

  if (rc < B_OK)
    return;

  if (node.GetNodeRef (&node_ref) < B_OK)
    return;

  looper = new EmacsFontMonitorLooper;

  if (watch_node (&node_ref, B_WATCH_STAT, looper) < B_OK)
    {
      delete looper;
      return;
    }

  looper->Run ();
}

bool
be_lock_font_defaults (void)
{
  return default_locker.Lock ();
}

void
be_unlock_font_defaults (void)
{
  return default_locker.Unlock ();
}

const char *
be_get_font_default (enum haiku_what_font what)
{
  switch (what)
    {
    case FIXED_FAMILY:
      return fixed_family;

    case FIXED_STYLE:
      return fixed_style;

    case DEFAULT_FAMILY:
      return default_family;

    case DEFAULT_STYLE:
      return default_style;
    }

  return NULL;
}

int
be_get_font_size (enum haiku_what_font what)
{
  switch (what)
    {
    case FIXED_FAMILY:
      return fixed_size;

    case DEFAULT_FAMILY:
      return default_size;

    default:
      return 0;
    }
}
