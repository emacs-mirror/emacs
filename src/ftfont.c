/* ftfont.c -- FreeType font driver.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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
#include <fontconfig/fontconfig.h>
#include <fontconfig/fcfreetype.h>

/* These two blocks are here because this file is built when using XFT
   and when using Cairo, so struct font_info in ftfont.h needs access
   to the appropriate types.  */
#ifdef HAVE_XFT
# include <X11/Xlib.h>
# include <X11/Xft/Xft.h>
#endif
#ifdef USE_CAIRO
# include <cairo-ft.h>
#endif

#include <c-strcase.h>

#include "lisp.h"
#include "dispextern.h"
#include "character.h"
#include "charset.h"
#include "category.h"
#include "composite.h"
#include "font.h"
#include "ftfont.h"
#include "pdumper.h"

static struct font_driver const ftfont_driver;
#ifdef HAVE_HARFBUZZ
static struct font_driver fthbfont_driver;
#endif	/* HAVE_HARFBUZZ */

/* Flag to tell if FcInit is already called or not.  */
static bool fc_initialized;

/* Handle to a FreeType library instance.  */
static FT_Library ft_library;

/* Cache for FreeType fonts.  */
static Lisp_Object freetype_font_cache;

/* Cache for FT_Face and FcCharSet. */
static Lisp_Object ft_face_cache;

enum ftfont_cache_for
  {
    FTFONT_CACHE_FOR_FACE,
    FTFONT_CACHE_FOR_CHARSET,
    FTFONT_CACHE_FOR_ENTITY
  };

static Lisp_Object ftfont_lookup_cache (Lisp_Object,
                                        enum ftfont_cache_for);

#define SYMBOL_FcChar8(SYM) (FcChar8 *) SDATA (SYMBOL_NAME (SYM))

static struct
{
  /* registry name */
  const char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* additional constraint by language */
  const char *lang;
  /* set on demand */
  FcCharSet *fc_charset;
} fc_charset_table[] =
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
    { "gb2312.1980-0", { 0x4E13 }, "zh-cn"},
    { "big5-0", { 0x9C21 }, "zh-tw" },
    { "jisx0208.1983-0", { 0x4E55 }, "ja"},
    { "ksc5601.1985-0", { 0xAC00 }, "ko"},
    { "cns11643.1992-1", { 0xFE32 }, "zh-tw"},
    { "cns11643.1992-2", { 0x4E33, 0x7934 }},
    { "cns11643.1992-3", { 0x201A9 }},
    { "cns11643.1992-4", { 0x20057 }},
    { "cns11643.1992-5", { 0x20000 }},
    { "cns11643.1992-6", { 0x20003 }},
    { "cns11643.1992-7", { 0x20055 }},
    { "gbk-0", { 0x4E06 }, "zh-cn"},
    { "jisx0212.1990-0", { 0x4E44 }},
    { "jisx0213.2000-1", { 0xFA10 }, "ja"},
    { "jisx0213.2000-2", { 0xFA49 }},
    { "jisx0213.2004-1", { 0x20B9F }},
    { "viscii1.1-1", { 0x1EA0, 0x1EAE, 0x1ED2 }, "vi"},
    { "tis620.2529-1", { 0x0E01 }, "th"},
    { "microsoft-cp1251", { 0x0401, 0x0490 }, "ru"},
    { "koi8-r", { 0x0401, 0x2219 }, "ru"},
    { "mulelao-1", { 0x0E81 }, "lo"},
    { "unicode-sip", { 0x20000 }},
    { NULL }
  };

static bool
matching_prefix (char const *str, ptrdiff_t len, char const *pat)
{
  return len == strlen (pat) && c_strncasecmp (str, pat, len) == 0;
}

/* Dirty hack for handing ADSTYLE property.

   Fontconfig (actually the underlying FreeType) gives such ADSTYLE
   font property of PCF/BDF fonts in FC_STYLE.  And, "Bold",
   "Oblique", "Italic", or any non-normal SWIDTH property names
   (e.g. SemiCondensed) are appended.  In addition, if there's no
   ADSTYLE property nor non-normal WEIGHT/SLANT/SWIDTH properties,
   "Regular" is used for FC_STYLE (see the function
   pcf_interpret_style in src/pcf/pcfread.c of FreeType).

   Unfortunately this behavior is not documented, so the following
   code may fail if FreeType changes the behavior in the future.  */

static Lisp_Object
get_adstyle_property (FcPattern *p)
{
  FcChar8 *fcstr;
  char *str, *end;
  Lisp_Object adstyle;

#ifdef FC_FONTFORMAT
  if ((FcPatternGetString (p, FC_FONTFORMAT, 0, &fcstr) == FcResultMatch)
      && xstrcasecmp ((char *) fcstr, "bdf") != 0
      && xstrcasecmp ((char *) fcstr, "pcf") != 0)
    /* Not a BDF nor PCF font.  */
    return Qnil;
#endif
  if (FcPatternGetString (p, FC_STYLE, 0, &fcstr) != FcResultMatch)
    return Qnil;
  str = (char *) fcstr;
  for (end = str; *end && *end != ' '; end++);
  if (matching_prefix (str, end - str, "Regular")
      || matching_prefix (str, end - str, "Bold")
      || matching_prefix (str, end - str, "Oblique")
      || matching_prefix (str, end - str, "Italic"))
    return Qnil;
  adstyle = font_intern_prop (str, end - str, 1);
  if (font_style_to_value (FONT_WIDTH_INDEX, adstyle, 0) >= 0)
    return Qnil;
  return adstyle;
}

static Lisp_Object
ftfont_pattern_entity (FcPattern *p, Lisp_Object extra)
{
  Lisp_Object key, cache, entity;
  FcChar8 *str;
  char *file;
  int idx;
  int numeric;
  double dbl;
  FcBool b;

  if (FcPatternGetString (p, FC_FILE, 0, &str) != FcResultMatch)
    return Qnil;
  if (FcPatternGetInteger (p, FC_INDEX, 0, &idx) != FcResultMatch)
    return Qnil;

  file = (char *) str;
  key = Fcons (build_unibyte_string (file), make_fixnum (idx));
  cache = ftfont_lookup_cache (key, FTFONT_CACHE_FOR_ENTITY);
  entity = XCAR (cache);
  if (! NILP (entity))
    {
      Lisp_Object val = font_make_entity ();
      int i;

      for (i = 0; i < FONT_OBJLIST_INDEX; i++)
	ASET (val, i, AREF (entity, i));

      ASET (val, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
      font_put_extra (val, QCfont_entity, key);

      return val;
    }
  entity = font_make_entity ();
  XSETCAR (cache, entity);

  ASET (entity, FONT_TYPE_INDEX, Qfreetype);
  ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  if (FcPatternGetString (p, FC_FOUNDRY, 0, &str) == FcResultMatch)
    {
      char *s = (char *) str;
      ASET (entity, FONT_FOUNDRY_INDEX, font_intern_prop (s, strlen (s), 1));
    }
  if (FcPatternGetString (p, FC_FAMILY, 0, &str) == FcResultMatch)
    {
      char *s = (char *) str;
      ASET (entity, FONT_FAMILY_INDEX, font_intern_prop (s, strlen (s), 1));
    }
  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
    {
      if (numeric >= FC_WEIGHT_REGULAR && numeric < FC_WEIGHT_MEDIUM)
	numeric = FC_WEIGHT_MEDIUM;
      FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX, make_fixnum (numeric));
    }
  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
    {
      numeric += 100;
      FONT_SET_STYLE (entity, FONT_SLANT_INDEX, make_fixnum (numeric));
    }
  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
    {
      FONT_SET_STYLE (entity, FONT_WIDTH_INDEX, make_fixnum (numeric));
    }
  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
    {
      ASET (entity, FONT_SIZE_INDEX, make_fixnum (dbl));
    }
  else
    ASET (entity, FONT_SIZE_INDEX, make_fixnum (0));
  if (FcPatternGetInteger (p, FC_SPACING, 0, &numeric) == FcResultMatch)
    ASET (entity, FONT_SPACING_INDEX, make_fixnum (numeric));
  if (FcPatternGetDouble (p, FC_DPI, 0, &dbl) == FcResultMatch)
    {
      int dpi = dbl;
      ASET (entity, FONT_DPI_INDEX, make_fixnum (dpi));
    }
  if (FcPatternGetBool (p, FC_SCALABLE, 0, &b) == FcResultMatch
      && b == FcTrue)
    {
      ASET (entity, FONT_SIZE_INDEX, make_fixnum (0));
      ASET (entity, FONT_AVGWIDTH_INDEX, make_fixnum (0));
    }
  else
    {
      /* As this font is not scalable, perhaps this is a BDF or PCF
	 font. */
      FT_Face ft_face;

      ASET (entity, FONT_ADSTYLE_INDEX, get_adstyle_property (p));
      if ((ft_library || FT_Init_FreeType (&ft_library) == 0)
	  && FT_New_Face (ft_library, file, idx, &ft_face) == 0)
	{
	  BDF_PropertyRec rec;

	  if (FT_Get_BDF_Property (ft_face, "AVERAGE_WIDTH", &rec) == 0
	      && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	    ASET (entity, FONT_AVGWIDTH_INDEX, make_fixnum (rec.u.integer));
	  FT_Done_Face (ft_face);
	}
    }

  ASET (entity, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
  font_put_extra (entity, QCfont_entity, key);
  return entity;
}


static Lisp_Object ftfont_generic_family_list;

static Lisp_Object
ftfont_resolve_generic_family (Lisp_Object family, FcPattern *pattern)
{
  Lisp_Object slot;
  FcPattern *match;
  FcResult result;
  FcLangSet *langset;

  family = Fintern (Fdowncase (SYMBOL_NAME (family)), Qnil);
  if (EQ (family, Qmono))
    family = Qmonospace;
  else if (EQ (family, Qsans) || EQ (family, Qsans__serif))
    family = Qsans_serif;
  slot = assq_no_quit (family, ftfont_generic_family_list);
  if (! CONSP (slot))
    return Qnil;
  if (! EQ (XCDR (slot), Qt))
    return XCDR (slot);
  pattern = FcPatternDuplicate (pattern);
  if (! pattern)
    goto err;
  FcPatternDel (pattern, FC_FOUNDRY);
  FcPatternDel (pattern, FC_FAMILY);
  FcPatternAddString (pattern, FC_FAMILY, SYMBOL_FcChar8 (family));
  if (FcPatternGetLangSet (pattern, FC_LANG, 0, &langset) != FcResultMatch)
    {
      /* This is to avoid the effect of locale.  */
      static const FcChar8 lang[] = "en";
      langset = FcLangSetCreate ();
      FcLangSetAdd (langset, lang);
      FcPatternAddLangSet (pattern, FC_LANG, langset);
      FcLangSetDestroy (langset);
    }
  FcConfigSubstitute (NULL, pattern, FcMatchPattern);
  FcDefaultSubstitute (pattern);
  match = FcFontMatch (NULL, pattern, &result);
  if (match)
    {
      FcChar8 *fam;

      if (FcPatternGetString (match, FC_FAMILY, 0, &fam) == FcResultMatch)
	family = intern ((char *) fam);
    }
  else
    family = Qnil;
  XSETCDR (slot, family);
  if (match) FcPatternDestroy (match);
 err:
  if (pattern) FcPatternDestroy (pattern);
  return family;
}

struct ftfont_cache_data
{
  FT_Face ft_face;
  FcCharSet *fc_charset;
  intptr_t face_refcount;
};

static Lisp_Object
ftfont_lookup_cache (Lisp_Object key, enum ftfont_cache_for cache_for)
{
  Lisp_Object cache, val, entity;
  struct ftfont_cache_data *cache_data;

  if (FONT_ENTITY_P (key))
    {
      entity = key;
      val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
      eassert (CONSP (val));
      key = XCDR (val);
    }
  else
    entity = Qnil;

  if (NILP (ft_face_cache))
    cache = Qnil;
  else
    cache = Fgethash (key, ft_face_cache, Qnil);
  if (NILP (cache))
    {
      if (NILP (ft_face_cache))
	ft_face_cache = CALLN (Fmake_hash_table, QCtest, Qequal);
      cache_data = xzalloc (sizeof *cache_data);
      val = make_mint_ptr (cache_data);
      cache = Fcons (Qnil, val);
      Fputhash (key, cache, ft_face_cache);
    }
  else
    {
      val = XCDR (cache);
      cache_data = xmint_pointer (val);
    }

  if (cache_for == FTFONT_CACHE_FOR_ENTITY)
    return cache;

  if (cache_for == FTFONT_CACHE_FOR_FACE
      ? ! cache_data->ft_face : ! cache_data->fc_charset)
    {
      char *filename = SSDATA (XCAR (key));
      int idx = XFIXNUM (XCDR (key));

      if (cache_for == FTFONT_CACHE_FOR_FACE)
	{
	  if (! ft_library
	      && FT_Init_FreeType (&ft_library) != 0)
	    return Qnil;
	  if (FT_New_Face (ft_library, filename, idx, &cache_data->ft_face)
	      != 0)
	    return Qnil;
	}
      else
	{
	  FcPattern *pat = NULL;
	  FcFontSet *fontset = NULL;
	  FcObjectSet *objset = NULL;
	  FcCharSet *charset = NULL;

	  pat = FcPatternBuild (0, FC_FILE, FcTypeString, (FcChar8 *) filename,
				FC_INDEX, FcTypeInteger, idx, NULL);
	  if (! pat)
	    goto finish;
	  objset = FcObjectSetBuild (FC_CHARSET, FC_STYLE, NULL);
	  if (! objset)
	    goto finish;
	  fontset = FcFontList (NULL, pat, objset);
	  if (! fontset)
	    goto finish;
	  if (fontset && fontset->nfont > 0
	      && (FcPatternGetCharSet (fontset->fonts[0], FC_CHARSET, 0,
				       &charset)
		  == FcResultMatch))
	    cache_data->fc_charset = FcCharSetCopy (charset);
	  else
	    cache_data->fc_charset = FcCharSetCreate ();

	finish:
	  if (fontset)
	    FcFontSetDestroy (fontset);
	  if (objset)
	    FcObjectSetDestroy (objset);
	  if (pat)
	    FcPatternDestroy (pat);
	}
    }
  return cache;
}

static FcCharSet *
ftfont_get_fc_charset (Lisp_Object entity)
{
  Lisp_Object val, cache;
  struct ftfont_cache_data *cache_data;

  cache = ftfont_lookup_cache (entity, FTFONT_CACHE_FOR_CHARSET);
  val = XCDR (cache);
  cache_data = xmint_pointer (val);
  return cache_data->fc_charset;
}

#ifdef HAVE_LIBOTF
static OTF *
ftfont_get_otf (struct font_info *ftfont_info)
{
  OTF *otf;

  if (ftfont_info->otf)
    return ftfont_info->otf;
  if (! ftfont_info->maybe_otf)
    return NULL;
  otf = OTF_open_ft_face (ftfont_info->ft_size->face);
  if (! otf || OTF_get_table (otf, "head") < 0)
    {
      if (otf)
	OTF_close (otf);
      ftfont_info->maybe_otf = 0;
      return NULL;
    }
  ftfont_info->otf = otf;
  return otf;
}
#endif	/* HAVE_LIBOTF */

Lisp_Object
ftfont_get_cache (struct frame *f)
{
  return freetype_font_cache;
}

static int
ftfont_get_charset (Lisp_Object registry)
{
  char *str = SSDATA (SYMBOL_NAME (registry));
  USE_SAFE_ALLOCA;
  char *re = SAFE_ALLOCA (SBYTES (SYMBOL_NAME (registry)) * 2 + 1);
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
  AUTO_STRING_WITH_LEN (regexp, re, j);
  for (i = 0; fc_charset_table[i].name; i++)
    if (fast_c_string_match_ignore_case
	(regexp, fc_charset_table[i].name,
	 strlen (fc_charset_table[i].name)) >= 0)
      break;
  SAFE_FREE ();
  if (! fc_charset_table[i].name)
    return -1;
  if (! fc_charset_table[i].fc_charset)
    {
      FcCharSet *charset = FcCharSetCreate ();
      int *uniquifier = fc_charset_table[i].uniquifier;

      if (! charset)
	return -1;
      for (j = 0; uniquifier[j]; j++)
	if (! FcCharSetAddChar (charset, uniquifier[j]))
	  {
	    FcCharSetDestroy (charset);
	    return -1;
	  }
      fc_charset_table[i].fc_charset = charset;
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

#define OTF_SYM_TAG(SYM, TAG)					\
  do {								\
    unsigned char *p = SDATA (SYMBOL_NAME (SYM));		\
    TAG = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];	\
  } while (0)

#define OTF_TAG_STR(TAG, P)			\
  do {						\
    (P)[0] = (char) (TAG >> 24);		\
    (P)[1] = (char) ((TAG >> 16) & 0xFF);	\
    (P)[2] = (char) ((TAG >> 8) & 0xFF);	\
    (P)[3] = (char) (TAG & 0xFF);		\
    (P)[4] = '\0';				\
  } while (0)

#ifdef HAVE_LIBOTF
#define OTF_TAG_SYM(SYM, TAG)			\
  do {						\
    char str[5];				\
    						\
    OTF_TAG_STR (TAG, str);			\
    (SYM) = font_intern_prop (str, 4, 1);	\
  } while (0)
#elif defined HAVE_HARFBUZZ
/* Libotf emulations on HarfBuzz for the functions called from
   ftfont_list.  They are a bit slower than the original ones, so used
   as fallbacks when libotf is not available.  */
typedef hb_face_t OTF;
typedef unsigned int OTF_tag;
static OTF *hbotf_open (const char *);
static int hbotf_check_features (OTF *, int, OTF_tag, OTF_tag,
				 const OTF_tag *, int);
#define OTF_open hbotf_open
#define OTF_close hb_face_destroy
#define OTF_check_features hbotf_check_features
#endif	/* !HAVE_LIBOTF && HAVE_HARFBUZZ */


static struct OpenTypeSpec *
ftfont_get_open_type_spec (Lisp_Object otf_spec)
{
  struct OpenTypeSpec *spec = malloc (sizeof *spec);
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
      val = XCAR (otf_spec);
      if (NILP (val))
	continue;
      ptrdiff_t len = list_length (val);
      spec->features[i] =
	(min (PTRDIFF_MAX, SIZE_MAX) / sizeof (int) < len
	 ? 0
	 : malloc (len * sizeof *spec->features[i]));
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
	      spec->features[i][j++] = negative ? tag | 0x80000000 : tag;
	    }
	}
      spec->nfeatures[i] = j;
    }
  return spec;
}

static FcPattern *
ftfont_spec_pattern (Lisp_Object spec, char *otlayout, struct OpenTypeSpec **otspec, const char **langname)
{
  Lisp_Object tmp, extra;
  FcPattern *pattern = NULL;
  FcCharSet *charset = NULL;
  FcLangSet *langset = NULL;
  int n;
  int dpi = -1;
  int scalable = -1;
  Lisp_Object script = Qnil;
  Lisp_Object registry;
  int fc_charset_idx;

  if ((n = FONT_SLANT_NUMERIC (spec)) >= 0
      && n < 100)
    /* Fontconfig doesn't support reverse-italic/oblique.  */
    return NULL;

  if (FIXNUMP (AREF (spec, FONT_DPI_INDEX)))
    dpi = XFIXNUM (AREF (spec, FONT_DPI_INDEX));
  if (FIXNUMP (AREF (spec, FONT_AVGWIDTH_INDEX))
      && XFIXNUM (AREF (spec, FONT_AVGWIDTH_INDEX)) == 0)
    scalable = 1;

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry)
      || EQ (registry, Qascii_0)
      || EQ (registry, Qiso10646_1)
      || EQ (registry, Qunicode_bmp))
    fc_charset_idx = -1;
  else
    {
      FcChar8 *lang;

      fc_charset_idx = ftfont_get_charset (registry);
      if (fc_charset_idx < 0)
	return NULL;
      charset = fc_charset_table[fc_charset_idx].fc_charset;
      *langname = fc_charset_table[fc_charset_idx].lang;
      lang = (FcChar8 *) *langname;
      if (lang)
	{
	  langset = FcLangSetCreate ();
	  if (! langset)
	    goto err;
	  FcLangSetAdd (langset, lang);
	}
    }

  otlayout[0] = '\0';
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      key = XCAR (XCAR (extra)), val = XCDR (XCAR (extra));
      if (EQ (key, QCdpi))
	{
	  if (FIXNUMP (val))
	    dpi = XFIXNUM (val);
	}
      else if (EQ (key, QClang))
	{
	  if (! langset)
	    langset = FcLangSetCreate ();
	  if (! langset)
	    goto err;
	  if (SYMBOLP (val))
	    {
	      if (! FcLangSetAdd (langset, SYMBOL_FcChar8 (val)))
		goto err;
	    }
	  else
	    for (; CONSP (val); val = XCDR (val))
	      if (SYMBOLP (XCAR (val))
		  && ! FcLangSetAdd (langset, SYMBOL_FcChar8 (XCAR (val))))
		goto err;
	}
      else if (EQ (key, QCotf))
	{
	  if (CONSP (val))
	    {
	      *otspec = ftfont_get_open_type_spec (val);
	      if (! *otspec)
		return NULL;
	      strcpy (otlayout, "otlayout:");
	      OTF_TAG_STR ((*otspec)->script_tag, otlayout + 9);
	      script = (*otspec)->script;
	    }
	}
      else if (EQ (key, QCscript))
	script = val;
      else if (EQ (key, QCscalable))
	scalable = ! NILP (val);
    }

  if (! NILP (script) && ! charset)
    {
      Lisp_Object chars = assq_no_quit (script, Vscript_representative_chars);

      if (CONSP (chars) && CONSP (CDR (chars)))
	{
	  charset = FcCharSetCreate ();
	  if (! charset)
	    goto err;
	  for (chars = XCDR (chars); CONSP (chars); chars = XCDR (chars))
	    if (CHARACTERP (XCAR (chars))
		&& ! FcCharSetAddChar (charset, XFIXNAT (XCAR (chars))))
	      goto err;
	}
    }

  pattern = FcPatternCreate ();
  if (! pattern)
    goto err;
  tmp = AREF (spec, FONT_FOUNDRY_INDEX);
  if (! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FOUNDRY, SYMBOL_FcChar8 (tmp)))
    goto err;
  tmp = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FAMILY, SYMBOL_FcChar8 (tmp)))
    goto err;
  if (charset
      && ! FcPatternAddCharSet (pattern, FC_CHARSET, charset))
    goto err;
  if (langset
      && ! FcPatternAddLangSet (pattern, FC_LANG, langset))
    goto err;
  if (dpi >= 0
      && ! FcPatternAddDouble (pattern, FC_DPI, dpi))
    goto err;
  if (scalable >= 0
      && ! FcPatternAddBool (pattern, FC_SCALABLE, scalable ? FcTrue : FcFalse))
    goto err;
#if defined HAVE_XFT && defined FC_COLOR
  /* We really don't like color fonts, they cause Xft crashes.  See
     Bug#30874.  */
  if (Vxft_ignore_color_fonts
      && ! FcPatternAddBool (pattern, FC_COLOR, FcFalse))
    goto err;
#endif

  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  if (pattern)
    {
      FcPatternDestroy (pattern);
      pattern = NULL;
    }
  if (*otspec)
    {
      if ((*otspec)->nfeatures[0] > 0)
	free ((*otspec)->features[0]);
      if ((*otspec)->nfeatures[1] > 0)
	free ((*otspec)->features[1]);
      free (*otspec);
      *otspec = NULL;
    }

 finish:
  if (langset) FcLangSetDestroy (langset);
  if (charset && fc_charset_idx < 0) FcCharSetDestroy (charset);
  return pattern;
}

static Lisp_Object
ftfont_list (struct frame *f, Lisp_Object spec)
{
  Lisp_Object val = Qnil, family, adstyle;
  int i;
  FcPattern *pattern;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  FcCharSet *charset;
  Lisp_Object chars = Qnil;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  int spacing = -1;
  const char *langname = NULL;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, otlayout, &otspec, &langname);
  if (! pattern)
    return Qnil;
  if (FcPatternGetCharSet (pattern, FC_CHARSET, 0, &charset) != FcResultMatch)
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
  if (FIXNUMP (AREF (spec, FONT_SPACING_INDEX)))
    spacing = XFIXNUM (AREF (spec, FONT_SPACING_INDEX));
  family = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      Lisp_Object resolved;

      resolved = ftfont_resolve_generic_family (family, pattern);
      if (! NILP (resolved))
	{
	  FcPatternDel (pattern, FC_FAMILY);
	  if (! FcPatternAddString (pattern, FC_FAMILY,
				    SYMBOL_FcChar8 (resolved)))
	    goto err;
	}
    }
  adstyle = AREF (spec, FONT_ADSTYLE_INDEX);
  if (! NILP (adstyle) && SBYTES (SYMBOL_NAME (adstyle)) == 0)
    adstyle = Qnil;
  objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
			     FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING, FC_SCALABLE,
			     FC_STYLE, FC_FILE, FC_INDEX,
#ifdef FC_CAPABILITY
			     FC_CAPABILITY,
#endif	/* FC_CAPABILITY */
#ifdef FC_FONTFORMAT
			     FC_FONTFORMAT,
#endif
#if defined HAVE_XFT && defined FC_COLOR
                             FC_COLOR,
#endif
			     NULL);
  if (! objset)
    goto err;
  if (! NILP (chars))
    FcObjectSetAdd (objset, FC_CHARSET);

  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset || fontset->nfont == 0)
    goto finish;
#if 0
  /* Need fix because this finds any fonts.  */
  if (fontset->nfont == 0 && ! NILP (family))
    {
      /* Try matching with configuration.  For instance, the
	 configuration may specify "Nimbus Mono L" as an alias of
	 "Courier".  */
      FcPattern *pat = FcPatternBuild (0, FC_FAMILY, FcTypeString,
				       SYMBOL_FcChar8 (family), NULL);
      FcChar8 *fam;

      if (FcConfigSubstitute (NULL, pat, FcMatchPattern) == FcTrue)
	{
	  for (i = 0;
	       FcPatternGetString (pat, FC_FAMILY, i, &fam) == FcResultMatch;
	       i++)
	    {
	      FcPatternDel (pattern, FC_FAMILY);
	      FcPatternAddString (pattern, FC_FAMILY, fam);
	      FcFontSetDestroy (fontset);
	      fontset = FcFontList (NULL, pattern, objset);
	      if (fontset && fontset->nfont > 0)
		break;
	    }
	}
    }
#endif
  for (i = 0; i < fontset->nfont; i++)
    {
      Lisp_Object entity;
#if defined HAVE_XFT && defined FC_COLOR
      {
        /* Some fonts, notably NotoColorEmoji, have an FC_COLOR value
           that's neither FcTrue nor FcFalse, which means FcFontList
           returns them even when it shouldn't really do so, so we
           need to manually skip them here (Bug#37786).  */
        FcBool b;
        if (Vxft_ignore_color_fonts
            && FcPatternGetBool (fontset->fonts[i], FC_COLOR, 0, &b)
            == FcResultMatch && b != FcFalse)
            continue;
      }
#endif
      if (spacing >= 0)
	{
	  int this;

	  if ((FcPatternGetInteger (fontset->fonts[i], FC_SPACING, 0, &this)
	       == FcResultMatch)
	      && spacing != this)
	    continue;
	}

#ifdef FC_CAPABILITY
      if (otlayout[0])
	{
	  FcChar8 *this;

	  if (FcPatternGetString (fontset->fonts[i], FC_CAPABILITY, 0, &this)
	      != FcResultMatch
	      || ! strstr ((char *) this, otlayout))
	    continue;
	}
#endif	/* FC_CAPABILITY */
#if defined HAVE_LIBOTF || defined HAVE_HARFBUZZ
      if (otspec)
	{
	  FcChar8 *file;
	  bool passed;
	  OTF *otf;

	  if (FcPatternGetString (fontset->fonts[i], FC_FILE, 0, &file)
	      != FcResultMatch)
	    continue;
	  otf = OTF_open ((char *) file);
	  if (! otf)
	    continue;
	  passed = (OTF_check_features (otf, 1, otspec->script_tag,
					otspec->langsys_tag,
					otspec->features[0],
					otspec->nfeatures[0]) == 1
		    && OTF_check_features (otf, 0, otspec->script_tag,
					   otspec->langsys_tag,
					   otspec->features[1],
					   otspec->nfeatures[1]) == 1);
	  OTF_close (otf);
	  if (!passed)
	    continue;
	}
#endif	/* HAVE_LIBOTF || HAVE_HARFBUZZ */
      if (VECTORP (chars))
	{
	  ptrdiff_t j;

	  if (FcPatternGetCharSet (fontset->fonts[i], FC_CHARSET, 0, &charset)
	      != FcResultMatch)
	    continue;
	  for (j = 0; j < ASIZE (chars); j++)
	    if (TYPE_RANGED_FIXNUMP (FcChar32, AREF (chars, j))
		&& FcCharSetHasChar (charset, XFIXNAT (AREF (chars, j))))
	      break;
	  if (j == ASIZE (chars))
	    continue;
	}
      if (! NILP (adstyle) || langname)
	{
	  Lisp_Object this_adstyle = get_adstyle_property (fontset->fonts[i]);

	  if (! NILP (adstyle)
	      && (NILP (this_adstyle)
		  || xstrcasecmp (SSDATA (SYMBOL_NAME (adstyle)),
				  SSDATA (SYMBOL_NAME (this_adstyle))) != 0))
	    continue;
	  if (langname
	      && ! NILP (this_adstyle)
	      && xstrcasecmp (langname, SSDATA (SYMBOL_NAME (this_adstyle))))
	    continue;
	}
      entity = ftfont_pattern_entity (fontset->fonts[i],
				      AREF (spec, FONT_EXTRA_INDEX));
      if (! NILP (entity))
	val = Fcons (entity, val);
    }
  val = Fnreverse (val);
  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  val = Qnil;

 finish:
  FONT_ADD_LOG ("ftfont-list", spec, val);
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);
  return val;
}

Lisp_Object
ftfont_list2 (struct frame *f, Lisp_Object spec, Lisp_Object type)
{
  Lisp_Object list = ftfont_list (f, spec);

  for (Lisp_Object tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, type);
  return list;
}

static Lisp_Object
ftfont_match (struct frame *f, Lisp_Object spec)
{
  Lisp_Object entity = Qnil;
  FcPattern *pattern, *match = NULL;
  FcResult result;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  const char *langname = NULL;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, otlayout, &otspec, &langname);
  if (! pattern)
    return Qnil;

  if (FIXNUMP (AREF (spec, FONT_SIZE_INDEX)))
    {
      FcValue value;

      value.type = FcTypeDouble;
      value.u.d = XFIXNUM (AREF (spec, FONT_SIZE_INDEX));
      FcPatternAdd (pattern, FC_PIXEL_SIZE, value, FcFalse);
    }
  if (FcConfigSubstitute (NULL, pattern, FcMatchPattern) == FcTrue)
    {
      FcDefaultSubstitute (pattern);
      match = FcFontMatch (NULL, pattern, &result);
      if (match)
	{
	  entity = ftfont_pattern_entity (match, AREF (spec, FONT_EXTRA_INDEX));
	  FcPatternDestroy (match);
	  if (! NILP (AREF (spec, FONT_FAMILY_INDEX))
	      && NILP (assq_no_quit (AREF (spec, FONT_FAMILY_INDEX),
				     ftfont_generic_family_list))
	      && NILP (Fstring_equal (AREF (spec, FONT_FAMILY_INDEX),
				      AREF (entity, FONT_FAMILY_INDEX))))
	    entity = Qnil;
	}
    }
  FcPatternDestroy (pattern);

  FONT_ADD_LOG ("ftfont-match", spec, entity);
  return entity;
}

Lisp_Object
ftfont_match2 (struct frame *f, Lisp_Object spec, Lisp_Object type)
{
  Lisp_Object entity = ftfont_match (f, spec);

  if (! NILP (entity))
    ASET (entity, FONT_TYPE_INDEX, type);
  return entity;
}

Lisp_Object
ftfont_list_family (struct frame *f)
{
  Lisp_Object list = Qnil;
  FcPattern *pattern = NULL;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  int i;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = FcPatternCreate ();
  if (! pattern)
    goto finish;
  objset = FcObjectSetBuild (FC_FAMILY, NULL);
  if (! objset)
    goto finish;
  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset)
    goto finish;

  for (i = 0; i < fontset->nfont; i++)
    {
      FcPattern *pat = fontset->fonts[i];
      FcChar8 *str;

      if (FcPatternGetString (pat, FC_FAMILY, 0, &str) == FcResultMatch)
	list = Fcons (intern ((char *) str), list);
    }

 finish:
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);

  return list;
}

void
ftfont_fix_match (FcPattern *pat, FcPattern *match)
{
  /*  These values are not used for matching (except antialias), but for
      rendering, so make sure they are carried over to the match.
      We also put antialias here because most fonts are antialiased, so
      the match will have antialias true.  */

  FcBool b = FcTrue;
  int i;
  double dpi;

  FcPatternGetBool (pat, FC_ANTIALIAS, 0, &b);
  if (! b)
    {
      FcPatternDel (match, FC_ANTIALIAS);
      FcPatternAddBool (match, FC_ANTIALIAS, FcFalse);
    }
  FcPatternGetBool (pat, FC_HINTING, 0, &b);
  if (! b)
    {
      FcPatternDel (match, FC_HINTING);
      FcPatternAddBool (match, FC_HINTING, FcFalse);
    }
#ifndef FC_HINT_STYLE
# define FC_HINT_STYLE "hintstyle"
#endif
  if (FcResultMatch == FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &i))
    {
      FcPatternDel (match, FC_HINT_STYLE);
      FcPatternAddInteger (match, FC_HINT_STYLE, i);
    }
#ifndef FC_LCD_FILTER
  /* Older fontconfig versions don't have FC_LCD_FILTER. */
#define FC_LCD_FILTER "lcdfilter"
#endif
  if (FcResultMatch == FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &i))
    {
      FcPatternDel (match, FC_LCD_FILTER);
      FcPatternAddInteger (match, FC_LCD_FILTER, i);
    }
  if (FcResultMatch == FcPatternGetInteger (pat, FC_RGBA, 0, &i))
    {
      FcPatternDel (match, FC_RGBA);
      FcPatternAddInteger (match, FC_RGBA, i);
    }
  if (FcResultMatch == FcPatternGetDouble (pat, FC_DPI, 0, &dpi))
    {
      FcPatternDel (match, FC_DPI);
      FcPatternAddDouble (match, FC_DPI, dpi);
    }
}

void
ftfont_add_rendering_parameters (FcPattern *pat, Lisp_Object entity)
{
  Lisp_Object tail;
  int ival;

  for (tail = AREF (entity, FONT_EXTRA_INDEX); CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object key = XCAR (XCAR (tail));
      Lisp_Object val = XCDR (XCAR (tail));

      if (EQ (key, QCantialias))
          FcPatternAddBool (pat, FC_ANTIALIAS, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QChinting))
	FcPatternAddBool (pat, FC_HINTING, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QCautohint))
	FcPatternAddBool (pat, FC_AUTOHINT, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QChintstyle))
	{
	  if (FIXNUMP (val))
	    FcPatternAddInteger (pat, FC_HINT_STYLE, XFIXNUM (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_HINT_STYLE, ival);
	}
      else if (EQ (key, QCrgba))
	{
	  if (FIXNUMP (val))
	    FcPatternAddInteger (pat, FC_RGBA, XFIXNUM (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_RGBA, ival);
	}
      else if (EQ (key, QClcdfilter))
	{
	  if (FIXNUMP (val))
	    FcPatternAddInteger (pat, FC_LCD_FILTER, ival = XFIXNUM (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_LCD_FILTER, ival);
	}
#ifdef FC_EMBOLDEN
      else if (EQ (key, QCembolden))
	FcPatternAddBool (pat, FC_EMBOLDEN, NILP (val) ? FcFalse : FcTrue);
#endif
    }
}

FcPattern *
ftfont_entity_pattern (Lisp_Object entity, int pixel_size)
{
  Lisp_Object val, filename, idx;
  FcPattern *pat;
  int i;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  eassert (CONSP (val));
  val = XCDR (val);
  filename = XCAR (val);
  idx = XCDR (val);
  pat = FcPatternCreate ();
  FcPatternAddInteger (pat, FC_WEIGHT, FONT_WEIGHT_NUMERIC (entity));
  i = FONT_SLANT_NUMERIC (entity) - 100;
  if (i < 0) i = 0;
  FcPatternAddInteger (pat, FC_SLANT, i);
  FcPatternAddInteger (pat, FC_WIDTH, FONT_WIDTH_NUMERIC (entity));
  FcPatternAddDouble (pat, FC_PIXEL_SIZE, pixel_size);
  val = AREF (entity, FONT_FAMILY_INDEX);
  if (! NILP (val))
    FcPatternAddString (pat, FC_FAMILY, (FcChar8 *) SDATA (SYMBOL_NAME (val)));
  val = AREF (entity, FONT_FOUNDRY_INDEX);
  if (! NILP (val))
    FcPatternAddString (pat, FC_FOUNDRY, (FcChar8 *) SDATA (SYMBOL_NAME (val)));
  val = AREF (entity, FONT_SPACING_INDEX);
  if (! NILP (val))
    FcPatternAddInteger (pat, FC_SPACING, XFIXNUM (val));
  val = AREF (entity, FONT_DPI_INDEX);
  if (! NILP (val))
    {
      double dbl = XFIXNUM (val);

      FcPatternAddDouble (pat, FC_DPI, dbl);
    }
  val = AREF (entity, FONT_AVGWIDTH_INDEX);
  if (FIXNUMP (val) && XFIXNUM (val) == 0)
    FcPatternAddBool (pat, FC_SCALABLE, FcTrue);
  /* This is necessary to identify the exact font (e.g. 10x20.pcf.gz
     over 10x20-ISO8859-1.pcf.gz).  */
  FcPatternAddCharSet (pat, FC_CHARSET, ftfont_get_fc_charset (entity));

  ftfont_add_rendering_parameters (pat, entity);

  FcPatternAddString (pat, FC_FILE, (FcChar8 *) SDATA (filename));
  FcPatternAddInteger (pat, FC_INDEX, XFIXNUM (idx));

  return pat;
}

#ifndef USE_CAIRO

Lisp_Object
ftfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  struct font_info *ftfont_info;
  struct font *font;
  struct ftfont_cache_data *cache_data;
  FT_Face ft_face;
  FT_Size ft_size;
  FT_UInt size;
  Lisp_Object val, filename, idx, cache, font_object;
  bool scalable;
  int spacing;
  int i;
  double upEM;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  cache = ftfont_lookup_cache (entity, FTFONT_CACHE_FOR_FACE);
  if (NILP (cache))
    return Qnil;
  filename = XCAR (val);
  idx = XCDR (val);
  cache_data = xmint_pointer (XCDR (cache));
  ft_face = cache_data->ft_face;
  if (cache_data->face_refcount > 0)
    {
      /* FT_Face in this cache is already used by the different size.  */
      if (FT_New_Size (ft_face, &ft_size) != 0)
	return Qnil;
      if (FT_Activate_Size (ft_size) != 0)
	{
	  FT_Done_Size (ft_size);
	  return Qnil;
	}
    }
  size = XFIXNUM (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  if (FT_Set_Pixel_Sizes (ft_face, size, size) != 0)
    {
      if (cache_data->face_refcount == 0)
	{
	  FT_Done_Face (ft_face);
	  cache_data->ft_face = NULL;
	}
      return Qnil;
    }
  cache_data->face_refcount++;

  font_object = font_build_object (VECSIZE (struct font_info),
				   Qfreetype, entity, size);
  ASET (font_object, FONT_FILE_INDEX, filename);
  font = XFONT_OBJECT (font_object);
  ftfont_info = (struct font_info *) font;
  ftfont_info->ft_size = ft_face->size;
  ftfont_info->index = XFIXNUM (idx);
#ifdef HAVE_LIBOTF
  ftfont_info->maybe_otf = (ft_face->face_flags & FT_FACE_FLAG_SFNT) != 0;
  ftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
#ifdef HAVE_HARFBUZZ
  ftfont_info->hb_font = NULL;
#endif	/* HAVE_HARFBUZZ */
  /* This means that there's no need of transformation.  */
  ftfont_info->matrix.xx = 0;
  font->pixel_size = size;
#ifdef HAVE_HARFBUZZ
  if (EQ (AREF (font_object, FONT_TYPE_INDEX), Qfreetypehb))
    font->driver = &fthbfont_driver;
  else
#endif	/* HAVE_HARFBUZZ */
  font->driver = &ftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  val = assq_no_quit (QCminspace, AREF (entity, FONT_EXTRA_INDEX));
  bool no_leading_p = !(CONSP (val) && NILP (XCDR (val)));
  upEM = ft_face->units_per_EM;
  scalable = (FIXNUMP (AREF (entity, FONT_AVGWIDTH_INDEX))
	      && XFIXNUM (AREF (entity, FONT_AVGWIDTH_INDEX)) == 0);
  if (scalable)
    {
      font->ascent = ft_face->ascender * size / upEM + 0.5;
      if (no_leading_p)
	{
	  font->descent = - ft_face->descender * size / upEM + 0.5;
	  font->height = font->ascent + font->descent;
	}
      else
	{
	  font->height = ft_face->height * size / upEM + 0.5;
	  font->descent = font->height - font->ascent;
	}
    }
  else
    {
      font->ascent = ft_face->size->metrics.ascender >> 6;
      if (no_leading_p)
	{
	  font->descent = - ft_face->size->metrics.descender >> 6;
	  font->height = font->ascent + font->descent;
	}
      else
	{
	  font->height = ft_face->size->metrics.height >> 6;
	  font->descent = font->height - font->ascent;
	}
    }
  if (FIXNUMP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XFIXNUM (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (spacing != FC_PROPORTIONAL
#ifdef FC_DUAL
      && spacing != FC_DUAL
#endif	/* FC_DUAL */
      )
    font->min_width = font->average_width = font->space_width
      = (scalable ? ft_face->max_advance_width * size / upEM + 0.5
	 : ft_face->size->metrics.max_advance >> 6);
  else
    {
      int n;

      font->min_width = font->average_width = font->space_width = 0;
      for (i = 32, n = 0; i < 127; i++)
	if (FT_Load_Char (ft_face, i, FT_LOAD_DEFAULT) == 0)
	  {
	    int this_width = ft_face->glyph->metrics.horiAdvance >> 6;

	    if (this_width > 0
		&& (! font->min_width || font->min_width > this_width))
	      font->min_width = this_width;
	    if (i == 32)
	      font->space_width = this_width;
	    font->average_width += this_width;
	    n++;
	  }
      if (n > 0)
	font->average_width /= n;
    }

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;
  if (scalable)
    {
      font->underline_position = (-ft_face->underline_position * size / upEM
				  + 0.5);
      font->underline_thickness = (ft_face->underline_thickness * size / upEM
				   + 0.5);
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }

  return font_object;
}

void
ftfont_close (struct font *font)
{
  if (font_data_structures_may_be_ill_formed ())
    return;

  struct font_info *ftfont_info = (struct font_info *) font;
  Lisp_Object val, cache;

  val = Fcons (font->props[FONT_FILE_INDEX], make_fixnum (ftfont_info->index));
  cache = ftfont_lookup_cache (val, FTFONT_CACHE_FOR_FACE);
  eassert (CONSP (cache));
  val = XCDR (cache);
  struct ftfont_cache_data *cache_data = xmint_pointer (val);
  cache_data->face_refcount--;
  if (cache_data->face_refcount == 0)
    {
      FT_Done_Face (cache_data->ft_face);
#ifdef HAVE_LIBOTF
      if (ftfont_info->otf)
	OTF_close (ftfont_info->otf);
#endif
#ifdef HAVE_HARFBUZZ
      if (ftfont_info->hb_font)
	hb_font_destroy (ftfont_info->hb_font);
#endif
      cache_data->ft_face = NULL;
    }
  else
    FT_Done_Size (ftfont_info->ft_size);
}

#endif /* !USE_CAIRO */

int
ftfont_has_char (Lisp_Object font, int c)
{
  struct charset *cs = NULL;

  if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qja)
      && charset_jisx0208 >= 0)
    cs = CHARSET_FROM_ID (charset_jisx0208);
  else if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qko)
      && charset_ksc5601 >= 0)
    cs = CHARSET_FROM_ID (charset_ksc5601);
  if (cs)
    return (ENCODE_CHAR (cs, c) != CHARSET_INVALID_CODE (cs));

  if (FONT_ENTITY_P (font))
    {
      FcCharSet *charset = ftfont_get_fc_charset (font);

      return (FcCharSetHasChar (charset, c) == FcTrue);
    }
  else
    {
      struct font_info *ftfont_info;

      ftfont_info = (struct font_info *) XFONT_OBJECT (font);
      return (FT_Get_Char_Index (ftfont_info->ft_size->face, (FT_ULong) c)
	      != 0);
    }
}

#ifndef USE_CAIRO

unsigned
ftfont_encode_char (struct font *font, int c)
{
  struct font_info *ftfont_info = (struct font_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  FT_ULong charcode = c;
  FT_UInt code = FT_Get_Char_Index (ft_face, charcode);

  return (code > 0 ? code : FONT_INVALID_CODE);
}

static bool
ftfont_glyph_metrics (FT_Face ft_face, int c, int *advance, int *lbearing,
                      int *rbearing, int *ascent, int *descent)
{
  if (FT_Load_Glyph (ft_face, c, FT_LOAD_DEFAULT) == 0)
    {
      FT_Glyph_Metrics *m = &ft_face->glyph->metrics;
      *advance = m->horiAdvance >> 6;
      *lbearing = m->horiBearingX >> 6;
      *rbearing = (m->horiBearingX + m->width) >> 6;
      *ascent = m->horiBearingY >> 6;
      *descent = (m->height - m->horiBearingY) >> 6;
      return true;
    }

  return false;
}

void
ftfont_text_extents (struct font *font, const unsigned int *code,
		     int nglyphs, struct font_metrics *metrics)
{
  struct font_info *ftfont_info = (struct font_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  int i, width = 0;
  bool first;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);

  for (i = 0, first = 1; i < nglyphs; i++)
    {
      int advance, lbearing, rbearing, ascent, descent;
      if (ftfont_glyph_metrics (ft_face, code[i], &advance, &lbearing,
                                &rbearing, &ascent, &descent))
	{
	  if (first)
	    {
	      metrics->lbearing = lbearing;
	      metrics->rbearing = rbearing;
	      metrics->ascent = ascent;
	      metrics->descent = descent;
	      first = 0;
	    }
	  if (metrics->lbearing > width + lbearing)
	    metrics->lbearing = width + lbearing;
	  if (metrics->rbearing < width + rbearing)
	    metrics->rbearing = width + rbearing;
	  if (metrics->ascent < ascent)
	    metrics->ascent = ascent;
	  if (metrics->descent > descent)
	    metrics->descent = descent;
	  width += advance;
	}
      else
	width += font->space_width;
    }
  metrics->width = width;
}

#endif /* !USE_CAIRO */

int
ftfont_get_bitmap (struct font *font, unsigned int code, struct font_bitmap *bitmap, int bits_per_pixel)
{
  struct font_info *ftfont_info = (struct font_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  FT_Int32 load_flags = FT_LOAD_RENDER;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (bits_per_pixel == 1)
    {
#ifdef FT_LOAD_TARGET_MONO
      load_flags |= FT_LOAD_TARGET_MONO;
#else
      load_flags |= FT_LOAD_MONOCHROME;
#endif
    }
  else if (bits_per_pixel != 8)
    /* We don't support such a rendering.  */
    return -1;

  if (FT_Load_Glyph (ft_face, code, load_flags) != 0)
    return -1;
  bitmap->bits_per_pixel
    = (ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO ? 1
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY ? 8
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_LCD ? 8
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_LCD_V ? 8
       : -1);
  if (bitmap->bits_per_pixel < 0)
    /* We don't support that kind of pixel mode.  */
    return -1;
  bitmap->rows = ft_face->glyph->bitmap.rows;
  bitmap->width = ft_face->glyph->bitmap.width;
  bitmap->pitch = ft_face->glyph->bitmap.pitch;
  bitmap->buffer = ft_face->glyph->bitmap.buffer;
  bitmap->left = ft_face->glyph->bitmap_left;
  bitmap->top = ft_face->glyph->bitmap_top;
  bitmap->advance = ft_face->glyph->metrics.horiAdvance >> 6;

  return 0;
}

int
ftfont_anchor_point (struct font *font, unsigned int code, int idx,
		     int *x, int *y)
{
  struct font_info *ftfont_info = (struct font_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (FT_Load_Glyph (ft_face, code, FT_LOAD_DEFAULT) != 0)
    return -1;
  if (ft_face->glyph->format != FT_GLYPH_FORMAT_OUTLINE)
    return -1;
  if (idx >= ft_face->glyph->outline.n_points)
    return -1;
  *x = ft_face->glyph->outline.points[idx].x;
  *y = ft_face->glyph->outline.points[idx].y;
  return 0;
}

#ifdef HAVE_LIBOTF

static Lisp_Object
ftfont_otf_features (OTF_GSUB_GPOS *gsub_gpos)
{
  Lisp_Object scripts, langsyses, features, sym;
  int i, j, k, l;

  for (scripts = Qnil, i = gsub_gpos->ScriptList.ScriptCount - 1; i >= 0; i--)
    {
      OTF_Script *otf_script = gsub_gpos->ScriptList.Script + i;

      for (langsyses = Qnil, j = otf_script->LangSysCount - 1; j >= -1; j--)
	{
	  OTF_LangSys *otf_langsys;

	  if (j >= 0)
	    otf_langsys = otf_script->LangSys + j;
	  else if (otf_script->DefaultLangSysOffset)
	    otf_langsys = &otf_script->DefaultLangSys;
	  else
	    break;

	  for (features = Qnil, k = otf_langsys->FeatureCount - 1; k >= 0; k--)
	    {
	      l = otf_langsys->FeatureIndex[k];
	      if (l >= gsub_gpos->FeatureList.FeatureCount)
		continue;
	      OTF_TAG_SYM (sym, gsub_gpos->FeatureList.Feature[l].FeatureTag);
	      features = Fcons (sym, features);
	    }
	  if (j >= 0)
	    OTF_TAG_SYM (sym, otf_script->LangSysRecord[j].LangSysTag);
	  else
	    sym = Qnil;
	  langsyses = Fcons (Fcons (sym, features), langsyses);
	}

      OTF_TAG_SYM (sym, gsub_gpos->ScriptList.Script[i].ScriptTag);
      scripts = Fcons (Fcons (sym, langsyses), scripts);
    }
  return scripts;

}


Lisp_Object
ftfont_otf_capability (struct font *font)
{
  struct font_info *ftfont_info = (struct font_info *) font;
  OTF *otf = ftfont_get_otf (ftfont_info);
  Lisp_Object gsub_gpos;

  if (! otf)
    return Qnil;
  gsub_gpos = Fcons (Qnil, Qnil);
  if (OTF_get_table (otf, "GSUB") == 0
      && otf->gsub->FeatureList.FeatureCount > 0)
    XSETCAR (gsub_gpos, ftfont_otf_features (otf->gsub));
  if (OTF_get_table (otf, "GPOS") == 0
      && otf->gpos->FeatureList.FeatureCount > 0)
    XSETCDR (gsub_gpos, ftfont_otf_features (otf->gpos));
  return gsub_gpos;
}

#ifdef HAVE_M17N_FLT

#if (((LIBOTF_MAJOR_VERSION > 1) || (LIBOTF_RELEASE_NUMBER >= 10))	\
     && ((M17NLIB_MAJOR_VERSION > 1) || (M17NLIB_MINOR_VERSION >= 6)))
/* We can use the new feature of libotf and m17n-flt to handle the
   character encoding scheme introduced in Unicode 5.1 and 5.2 for
   some Agian scripts.  */
#define M17N_FLT_USE_NEW_FEATURE
#endif

struct MFLTFontFT
{
  MFLTFont flt_font;
  struct font *font;
  FT_Face ft_face;
  OTF *otf;
  FT_Matrix *matrix;
};

/* The actual type of elements in the array MFLTGlyphString.glyphs.
   We use this structure instead of MFLTGlyph to utilize the new
   feature of libotf ver.0.9.15 which requires saving and restoring
   the value of OTF_GlyphString.positioning_type in the succeeding
   calls of the callback function MFLTFont.drive_otf (which is set to
   ftfont_drive_otf).  */

typedef struct {
  MFLTGlyph g;
  unsigned int libotf_positioning_type;
} MFLTGlyphFT;

static int
ftfont_get_glyph_id (MFLTFont *font, MFLTGlyphString *gstring,
		     int from, int to)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyphFT *g;

  for (g = (MFLTGlyphFT *) (gstring->glyphs) + from; from < to; g++, from++)
    if (! g->g.encoded)
      {
	FT_UInt code = FT_Get_Char_Index (ft_face, g->g.code);

	g->g.code = code > 0 ? code : FONT_INVALID_CODE;
	g->g.encoded = 1;
      }
  return 0;
}

/* Operators for 26.6 fixed fractional pixel format */

#define FLOOR(x)    ((x) & -64)
#define CEIL(x)	    (((x)+63) & -64)
#define ROUND(x)    (((x)+32) & -64)

static int
ftfont_get_metrics (MFLTFont *font, MFLTGlyphString *gstring,
		    int from, int to)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyphFT *g;

  for (g = (MFLTGlyphFT *) (gstring->glyphs) + from; from < to; g++, from++)
    if (! g->g.measured)
      {
	if (g->g.code != FONT_INVALID_CODE)
	  {
	    FT_Glyph_Metrics *m;

	    if (FT_Load_Glyph (ft_face, g->g.code, FT_LOAD_DEFAULT) != 0
		&& FT_Load_Glyph (ft_face, g->g.code, FT_LOAD_NO_HINTING) != 0)
	      emacs_abort ();
	    m = &ft_face->glyph->metrics;
	    if (flt_font_ft->matrix)
	      {
		FT_Vector v[4];
		int i;

		v[0].x = v[1].x = m->horiBearingX;
		v[2].x = v[3].x = m->horiBearingX + m->width;
		v[0].y = v[2].y = m->horiBearingY;
		v[1].y = v[3].y = m->horiBearingY - m->height;
		for (i = 0; i < 4; i++)
		  FT_Vector_Transform (v + i, flt_font_ft->matrix);
		g->g.lbearing = v[0].x < v[1].x ? FLOOR (v[0].x) : FLOOR (v[1].x);
		g->g.rbearing = v[2].x > v[3].x ? CEIL (v[2].x) : CEIL (v[3].x);
		g->g.ascent = v[0].y > v[2].y ? CEIL (v[0].y) : CEIL (v[2].y);
		g->g.descent = v[1].y < v[3].y ? - FLOOR (v[1].y) : - FLOOR (v[3].y);
	      }
	    else
	      {
		g->g.lbearing = FLOOR (m->horiBearingX);
		g->g.rbearing = CEIL (m->horiBearingX + m->width);
		g->g.ascent = CEIL (m->horiBearingY);
		g->g.descent = - FLOOR (m->horiBearingY - m->height);
	      }
	    g->g.xadv = ROUND (ft_face->glyph->advance.x);
	  }
	else
	  {
	    g->g.lbearing = 0;
	    g->g.rbearing = g->g.xadv = flt_font_ft->font->space_width << 6;
	    g->g.ascent = flt_font_ft->font->ascent << 6;
	    g->g.descent = flt_font_ft->font->descent << 6;
	  }
	g->g.yadv = 0;
	g->g.measured = 1;
      }
  return 0;
}

static int
ftfont_check_otf (MFLTFont *font, MFLTOtfSpec *spec)
{
#define FEATURE_NONE(IDX) (! spec->features[IDX])

#define FEATURE_ANY(IDX)	\
  (spec->features[IDX]		\
   && spec->features[IDX][0] == 0xFFFFFFFF && spec->features[IDX][1] == 0)

  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  OTF *otf = flt_font_ft->otf;
  OTF_Tag *tags;
  int i, n;
  bool negative;

  if (FEATURE_ANY (0) && FEATURE_ANY (1))
    /* Return true iff any of GSUB or GPOS support the script (and
       language).  */
    return (otf
	    && (OTF_check_features (otf, 0, spec->script, spec->langsys,
				    NULL, 0) > 0
		|| OTF_check_features (otf, 1, spec->script, spec->langsys,
				       NULL, 0) > 0));

  for (i = 0; i < 2; i++)
    if (! FEATURE_ANY (i))
      {
	if (FEATURE_NONE (i))
	  {
	    if (otf
		&& OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				       NULL, 0) > 0)
	      return 0;
	    continue;
	  }
	if (spec->features[i][0] == 0xFFFFFFFF)
	  {
	    if (! otf
		|| OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				       NULL, 0) <= 0)
	      continue;
	  }
	else if (! otf)
	  return 0;
	for (n = 1; spec->features[i][n]; n++);
	USE_SAFE_ALLOCA;
	SAFE_NALLOCA (tags, 1, n);
	for (n = 0, negative = 0; spec->features[i][n]; n++)
	  {
	    if (spec->features[i][n] == 0xFFFFFFFF)
	      negative = 1;
	    else if (negative)
	      tags[n - 1] = spec->features[i][n] | 0x80000000;
	    else
	      tags[n] = spec->features[i][n];
	  }
	bool passed = true;
#ifndef M17N_FLT_USE_NEW_FEATURE
	passed = n - negative > 0;
#endif
	if (passed)
	  passed = (OTF_check_features (otf, i == 0, spec->script,
					spec->langsys, tags, n - negative)
		    != 1);
	SAFE_FREE ();
	if (passed)
	  return 0;
      }
  return 1;
#undef FEATURE_NONE
#undef FEATURE_ANY
}

#define DEVICE_DELTA(table, size)				\
  (((size) >= (table).StartSize && (size) <= (table).EndSize)	\
   ? (table).DeltaValue[(size) - (table).StartSize] << 6	\
   : 0)

static void
adjust_anchor (FT_Face ft_face, OTF_Anchor *anchor,
	       unsigned code, int x_ppem, int y_ppem, int *x, int *y)
{
  if (anchor->AnchorFormat == 2)
    {
      FT_Outline *outline;
      int ap = anchor->f.f1.AnchorPoint;

      FT_Load_Glyph (ft_face, (FT_UInt) code, FT_LOAD_MONOCHROME);
      outline = &ft_face->glyph->outline;
      if (ap < outline->n_points)
	{
	  *x = outline->points[ap].x << 6;
	  *y = outline->points[ap].y << 6;
	}
    }
  else if (anchor->AnchorFormat == 3)
    {
      if (anchor->f.f2.XDeviceTable.offset
	  && anchor->f.f2.XDeviceTable.DeltaValue)
	*x += DEVICE_DELTA (anchor->f.f2.XDeviceTable, x_ppem);
      if (anchor->f.f2.YDeviceTable.offset
	  && anchor->f.f2.YDeviceTable.DeltaValue)
	*y += DEVICE_DELTA (anchor->f.f2.YDeviceTable, y_ppem);
    }
}

static OTF_GlyphString otf_gstring;

static void
setup_otf_gstring (int size)
{
  if (otf_gstring.size < size)
    {
      ptrdiff_t new_size = otf_gstring.size;
      xfree (otf_gstring.glyphs);
      otf_gstring.glyphs = xpalloc (NULL, &new_size, size - otf_gstring.size,
				    INT_MAX, sizeof *otf_gstring.glyphs);
      otf_gstring.size = new_size;
    }
  otf_gstring.used = size;
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * size);
}

#ifdef M17N_FLT_USE_NEW_FEATURE

/* Pack 32-bit OTF tag (0x7F7F7F7F) into 28-bit (0x0FFFFFFF).  */
#define PACK_OTF_TAG(TAG)	\
  ((((TAG) & 0x7F000000) >> 3)	\
    | (((TAG) & 0x7F0000) >> 2)	\
    | (((TAG) & 0x7F00) >> 1)	\
    | ((TAG) & 0x7F))

/* Assuming that FONT is an OpenType font, apply OpenType features
   specified in SPEC on glyphs between FROM and TO of IN, and record
   the lastly applied feature in each glyph of IN.  If OUT is not
   NULL, append the resulting glyphs to OUT while storing glyph
   position adjustment information in ADJUSTMENT.  */

static int
ftfont_drive_otf (MFLTFont *font,
		  MFLTOtfSpec *spec,
		  MFLTGlyphString *in,
		  int from,
		  int to,
		  MFLTGlyphString *out,
		  MFLTGlyphAdjustment *adjustment)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  MFLTGlyphFT *in_glyphs = (MFLTGlyphFT *) (in->glyphs) + from;
  MFLTGlyphFT *out_glyphs = out ? (MFLTGlyphFT *) (out->glyphs) : NULL;
  FT_Face ft_face = flt_font_ft->ft_face;
  OTF *otf = flt_font_ft->otf;
  int len = to - from;
  int i, j, gidx;
  OTF_Glyph *otfg;
  char script[5], *langsys = NULL;
  char *gsub_features = NULL, *gpos_features = NULL;
  OTF_Feature *features;

  if (len == 0)
    return from;
  OTF_tag_name (spec->script, script);

  char langsysbuf[5];
  if (spec->langsys)
    {
      langsys = langsysbuf;
      OTF_tag_name (spec->langsys, langsys);
    }

  USE_SAFE_ALLOCA;
  for (i = 0; i < 2; i++)
    {
      char *p;

      if (spec->features[i] && spec->features[i][1] != 0xFFFFFFFF)
	{
	  for (j = 0; spec->features[i][j]; j++);
	  SAFE_NALLOCA (p, 6, j);
	  if (i == 0)
	    gsub_features = p;
	  else
	    gpos_features = p;
	  for (j = 0; spec->features[i][j]; j++)
	    {
	      if (spec->features[i][j] == 0xFFFFFFFF)
		*p++ = '*', *p++ = ',';
	      else
		{
		  OTF_tag_name (spec->features[i][j], p);
		  p[4] = ',';
		  p += 5;
		}
	    }
	  *--p = '\0';
	}
    }

  setup_otf_gstring (len);
  for (i = 0; i < len; i++)
    {
      otf_gstring.glyphs[i].c = in_glyphs[i].g.c & 0x11FFFF;
      otf_gstring.glyphs[i].glyph_id = in_glyphs[i].g.code;
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
      otf_gstring.glyphs[i].positioning_type = in_glyphs[i].libotf_positioning_type;
#endif
    }

  OTF_drive_gdef (otf, &otf_gstring);
  gidx = out ? out->used : from;

  if (gsub_features && out)
    {
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
      if (OTF_drive_gsub_features (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
#else
      if (OTF_drive_gsub_with_log (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
#endif
      if (out->allocated < out->used + otf_gstring.used)
	{
	  SAFE_FREE ();
	  return -2;
	}
      features = otf->gsub->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; )
	{
	  MFLTGlyphFT *g;
	  int min_from, max_to;
	  int feature_idx;

#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	  feature_idx = OTF_POSITIONING_TYPE_GET_FEATURE (otfg);
#else
	  feature_idx = otfg->positioning_type >> 4;
#endif
	  g = out_glyphs + out->used;
	  *g = in_glyphs[otfg->f.index.from];
	  if (g->g.code != otfg->glyph_id)
	    {
	      g->g.c = 0;
	      g->g.code = otfg->glyph_id;
	      g->g.measured = 0;
	    }
	  out->used++;
	  min_from = g->g.from;
	  max_to = g->g.to;
	  if (otfg->f.index.from < otfg->f.index.to)
	    {
	      /* OTFG substitutes multiple glyphs in IN.  */
	      for (j = otfg->f.index.from + 1; j <= otfg->f.index.to; j++)
		{
		  if (min_from > in_glyphs[j].g.from)
		    min_from = in_glyphs[j].g.from;
		  if (max_to < in_glyphs[j].g.to)
		    max_to = in_glyphs[j].g.to;
		}
	      g->g.from = min_from;
	      g->g.to = max_to;
	    }
	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      g->g.internal = (g->g.internal & ~0x1FFFFFFF) | tag;
	    }
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	  g->libotf_positioning_type
	    = otfg->positioning_type & OTF_positioning_type_components_mask;
#endif
	  for (i++, otfg++; (i < otf_gstring.used
			     && otfg->f.index.from == otfg[-1].f.index.from);
	       i++, otfg++)
	    {
	      g = out_glyphs + out->used;
	      *g = in_glyphs[otfg->f.index.to];
	      if (g->g.code != otfg->glyph_id)
		{
		  g->g.c = 0;
		  g->g.code = otfg->glyph_id;
		  g->g.measured = 0;
		}
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	      feature_idx = OTF_POSITIONING_TYPE_GET_FEATURE (otfg);
#else
	      feature_idx = otfg->positioning_type >> 4;
#endif
	      if (feature_idx)
		{
		  unsigned int tag = features[feature_idx - 1].FeatureTag;
		  tag = PACK_OTF_TAG (tag);
		  g->g.internal = (g->g.internal & ~0x1FFFFFFF) | tag;
		}
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	      g->libotf_positioning_type
		= otfg->positioning_type & OTF_positioning_type_components_mask;
#endif
	      out->used++;
	    }
	}
    }
  else if (gsub_features)
    {
      /* Just for checking which features will be applied.  */
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
      if (OTF_drive_gsub_features (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
#else
      if (OTF_drive_gsub_with_log (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
#endif
      features = otf->gsub->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; i++,
	     otfg++)
	{
	  int feature_idx;
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	  feature_idx = OTF_POSITIONING_TYPE_GET_FEATURE (otfg);
#else
	  feature_idx = otfg->positioning_type >> 4;
#endif
	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      for (j = otfg->f.index.from; j <= otfg->f.index.to; j++)
		{
		  MFLTGlyphFT *g = in_glyphs + j;
		  g->g.internal = (g->g.internal & ~0x1FFFFFFF) | tag;
		}
	    }
	}
    }
  else if (out)
    {
      if (out->allocated < out->used + len)
	{
	  SAFE_FREE ();
	  return -2;
	}
      for (i = 0; i < len; i++)
	out_glyphs[out->used++] = in_glyphs[i];
    }

  if (gpos_features && out)
    {
      MFLTGlyphFT *base = NULL, *mark = NULL, *g;
      int x_ppem, y_ppem, x_scale, y_scale;

#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
      if (OTF_drive_gpos_features (otf, &otf_gstring, script, langsys,
				   gpos_features) < 0)
	{
	  SAFE_FREE ();
	  return to;
	}
#else
      if (OTF_drive_gpos_with_log (otf, &otf_gstring, script, langsys,
				   gpos_features) < 0)
	{
	  SAFE_FREE ();
	  return to;
	}
#endif
      features = otf->gpos->FeatureList.Feature;
      x_ppem = ft_face->size->metrics.x_ppem;
      y_ppem = ft_face->size->metrics.y_ppem;
      x_scale = ft_face->size->metrics.x_scale;
      y_scale = ft_face->size->metrics.y_scale;

      for (i = 0, otfg = otf_gstring.glyphs, g = out_glyphs + gidx;
	   i < otf_gstring.used; i++, otfg++)
	{
	  MFLTGlyphAdjustment *adjust = adjustment;
	  MFLTGlyphFT *prev;
	  int positioning_type, feature_idx;

#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
	  positioning_type = OTF_POSITIONING_TYPE_GET_FORMAT (otfg);
	  feature_idx = OTF_POSITIONING_TYPE_GET_FEATURE (otfg);
#else
	  positioning_type = otfg->positioning_type & 0xF;
	  feature_idx = otfg->positioning_type >> 4;
#endif
	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      g->g.internal = (g->g.internal & ~0x1FFFFFFF) | tag;
	    }

	  if (! otfg->glyph_id)
	    /* This is a pseudo glyph that contains positioning
	       information to be accumulated to a real glyph.  */
	    adjust--;
	  switch (positioning_type)
	    {
	    case 0:
	      break;
	    case 1: 		/* Single */
	    case 2: 		/* Pair */
	      {
		int format = otfg->f.f1.format;

		if (format & OTF_XPlacement)
		  adjust->xoff
		    = otfg->f.f1.value->XPlacement * x_scale / 0x10000;
		if (format & OTF_XPlaDevice)
		  adjust->xoff
		    += DEVICE_DELTA (otfg->f.f1.value->XPlaDevice, x_ppem);
		if (format & OTF_YPlacement)
		  adjust->yoff
		    = - (otfg->f.f1.value->YPlacement * y_scale / 0x10000);
		if (format & OTF_YPlaDevice)
		  adjust->yoff
		    -= DEVICE_DELTA (otfg->f.f1.value->YPlaDevice, y_ppem);
		if (format & OTF_XAdvance)
		  adjust->xadv
		    += otfg->f.f1.value->XAdvance * x_scale / 0x10000;
		if (format & OTF_XAdvDevice)
		  adjust->xadv
		    += DEVICE_DELTA (otfg->f.f1.value->XAdvDevice, x_ppem);
		if (format & OTF_YAdvance)
		  adjust->yadv
		    += otfg->f.f1.value->YAdvance * y_scale / 0x10000;
		if (format & OTF_YAdvDevice)
		  adjust->yadv
		    += DEVICE_DELTA (otfg->f.f1.value->YAdvDevice, y_ppem);
		adjust->set = 1;
	      }
	      break;
	    case 3:		/* Cursive */
	      /* Not yet supported.  */
	      break;
	    case 4:		/* Mark-to-Base */
	    case 5:		/* Mark-to-Ligature */
	      if (! base)
		break;
	      prev = base;
	      goto label_adjust_anchor;
	    default:		/* i.e. case 6 Mark-to-Mark */
	      if (! mark)
		break;
	      prev = mark;
#ifdef OTF_POSITIONING_TYPE_GET_FORMAT
		  {
		    int distance = OTF_POSITIONING_TYPE_GET_MARKDISTANCE (otfg);

		    if (distance > 0)
		      {
			prev = g - distance;
			if (prev < out_glyphs)
			  prev = mark;
		      }
		  }
#endif

	    label_adjust_anchor:
	      {
		int base_x, base_y, mark_x, mark_y;
		int this_from, this_to;

		base_x = otfg->f.f4.base_anchor->XCoordinate * x_scale / 0x10000;
		base_y = otfg->f.f4.base_anchor->YCoordinate * y_scale / 0x10000;
		mark_x = otfg->f.f4.mark_anchor->XCoordinate * x_scale / 0x10000;
		mark_y = otfg->f.f4.mark_anchor->YCoordinate * y_scale / 0x10000;

		if (otfg->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.base_anchor, prev->g.code,
				 x_ppem, y_ppem, &base_x, &base_y);
		if (otfg->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.mark_anchor, g->g.code,
				 x_ppem, y_ppem, &mark_x, &mark_y);
		adjust->xoff = (base_x - mark_x);
		adjust->yoff = - (base_y - mark_y);
		adjust->back = (g - prev);
		adjust->xadv = 0;
		adjust->advance_is_absolute = 1;
		adjust->set = 1;
		this_from = g->g.from;
		this_to = g->g.to;
		for (j = 0; prev + j < g; j++)
		  {
		    if (this_from > prev[j].g.from)
		      this_from = prev[j].g.from;
		    if (this_to < prev[j].g.to)
		      this_to = prev[j].g.to;
		  }
		for (; prev <= g; prev++)
		  {
		    prev->g.from = this_from;
		    prev->g.to = this_to;
		  }
	      }
	    }
	  if (otfg->glyph_id)
	    {
	      if (otfg->GlyphClass == OTF_GlyphClass0)
		base = mark = g;
	      else if (otfg->GlyphClass == OTF_GlyphClassMark)
		mark = g;
	      else
		base = g;
	      g++, adjustment++;
	    }
	}
    }
  else if (gpos_features)
    {
      if (OTF_drive_gpos_with_log (otf, &otf_gstring, script, langsys,
				   gpos_features) < 0)
	{
	  SAFE_FREE ();
	  return to;
	}
      features = otf->gpos->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used;
	   i++, otfg++)
	if (otfg->positioning_type & 0xF)
	  {
	    int feature_idx = otfg->positioning_type >> 4;

	    if (feature_idx)
	      {
		unsigned int tag = features[feature_idx - 1].FeatureTag;
		tag = PACK_OTF_TAG (tag);
		for (j = otfg->f.index.from; j <= otfg->f.index.to; j++)
		  {
		    MFLTGlyphFT *g = in_glyphs + j;
		    g->g.internal = (g->g.internal & ~0x1FFFFFFF) | tag;
		  }
	      }
	  }
    }
  SAFE_FREE ();
  return to;

 simple_copy:
  SAFE_FREE ();
  if (! out)
    return to;
  if (out->allocated < out->used + len)
    return -2;
  font->get_metrics (font, in, from, to);
  memcpy (out->glyphs + out->used, in_glyphs, sizeof (MFLTGlyphFT) * len);
  out->used += len;
  return to;
}

static int
ftfont_try_otf (MFLTFont *font, MFLTOtfSpec *spec,
		MFLTGlyphString *in, int from, int to)
{
  return ftfont_drive_otf (font, spec, in, from, to, NULL, NULL);
}

#else  /* not M17N_FLT_USE_NEW_FEATURE */

static int
ftfont_drive_otf (MFLTFont *font, MFLTOtfSpec *spec, MFLTGlyphString *in,
		  int from, int to,
		  MFLTGlyphString *out, MFLTGlyphAdjustment *adjustment)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  MFLTGlyphFT *in_glyphs = (MFLTGlyphFT *) (in->glyphs) + from;
  MFLTGlyphFT *out_glyphs = out ? (MFLTGlyphFT *) (out->glyphs) : NULL;
  FT_Face ft_face = flt_font_ft->ft_face;
  OTF *otf = flt_font_ft->otf;
  int len = to - from;
  int i, j, gidx;
  OTF_Glyph *otfg;
  char script[5], *langsys = NULL;
  char *gsub_features = NULL, *gpos_features = NULL;

  if (len == 0)
    return from;
  OTF_tag_name (spec->script, script);

  char langsysbuf[5];
  if (spec->langsys)
    {
      langsys = langsysbuf;
      OTF_tag_name (spec->langsys, langsys);
    }

  USE_SAFE_ALLOCA;
  for (i = 0; i < 2; i++)
    {
      char *p;

      if (spec->features[i] && spec->features[i][1] != 0xFFFFFFFF)
	{
	  for (j = 0; spec->features[i][j]; j++);
	  SAFE_NALLOCA (p, 6, j);
	  if (i == 0)
	    gsub_features = p;
	  else
	    gpos_features = p;
	  for (j = 0; spec->features[i][j]; j++)
	    {
	      if (spec->features[i][j] == 0xFFFFFFFF)
		*p++ = '*', *p++ = ',';
	      else
		{
		  OTF_tag_name (spec->features[i][j], p);
		  p[4] = ',';
		  p += 5;
		}
	    }
	  *--p = '\0';
	}
    }

  setup_otf_gstring (len);
  for (i = 0; i < len; i++)
    {
      otf_gstring.glyphs[i].c = in_glyphs[i].g.c;
      otf_gstring.glyphs[i].glyph_id = in_glyphs[i].g.code;
    }

  OTF_drive_gdef (otf, &otf_gstring);
  gidx = out->used;

  if (gsub_features)
    {
      if (OTF_drive_gsub (otf, &otf_gstring, script, langsys, gsub_features)
	  < 0)
	goto simple_copy;
      if (out->allocated < out->used + otf_gstring.used)
	{
	  SAFE_FREE ();
	  return -2;
	}
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; )
	{
	  MFLTGlyphFT *g;
	  int min_from, max_to;
	  int j;

	  g = out_glyphs + out->used;
	  *g = in_glyphs[otfg->f.index.from];
	  if (g->g.code != otfg->glyph_id)
	    {
	      g->g.c = 0;
	      g->g.code = otfg->glyph_id;
	      g->g.measured = 0;
	    }
	  out->used++;
	  min_from = g->g.from;
	  max_to = g->g.to;
	  if (otfg->f.index.from < otfg->f.index.to)
	    {
	      /* OTFG substitutes multiple glyphs in IN.  */
	      for (j = from + otfg->f.index.from + 1;
		   j <= from + otfg->f.index.to; j++)
		{
		  if (min_from > in->glyphs[j].from)
		    min_from = in->glyphs[j].from;
		  if (max_to < in->glyphs[j].to)
		    max_to = in->glyphs[j].to;
		}
	      g->g.from = min_from;
	      g->g.to = max_to;
	    }
	  for (i++, otfg++; (i < otf_gstring.used
			     && otfg->f.index.from == otfg[-1].f.index.from);
	       i++, otfg++)
	    {
	      g = out_glyphs + out->used;
	      *g = in_glyphs[otfg->f.index.to];
	      if (g->g.code != otfg->glyph_id)
		{
		  g->g.c = 0;
		  g->g.code = otfg->glyph_id;
		  g->g.measured = 0;
		}
	      out->used++;
	    }
	}
    }
  else
    {
      if (out->allocated < out->used + len)
	{
	  SAFE_FREE ();
	  return -2;
	}
      for (i = 0; i < len; i++)
	out_glyphs[out->used++] = in_glyphs[i];
    }

  if (gpos_features)
    {
      MFLTGlyphFT *base = NULL, *mark = NULL, *g;
      int x_ppem, y_ppem, x_scale, y_scale;

      if (OTF_drive_gpos (otf, &otf_gstring, script, langsys, gpos_features)
	  < 0)
	{
	  SAFE_FREE ();
	  return to;
	}

      x_ppem = ft_face->size->metrics.x_ppem;
      y_ppem = ft_face->size->metrics.y_ppem;
      x_scale = ft_face->size->metrics.x_scale;
      y_scale = ft_face->size->metrics.y_scale;

      for (i = 0, otfg = otf_gstring.glyphs, g = out_glyphs + gidx;
	   i < otf_gstring.used; i++, otfg++, g++)
	{
	  MFLTGlyphFT *prev;

	  if (! otfg->glyph_id)
	    continue;
	  switch (otfg->positioning_type)
	    {
	    case 0:
	      break;
	    case 1: 		/* Single */
	    case 2: 		/* Pair */
	      {
		int format = otfg->f.f1.format;

		if (format & OTF_XPlacement)
		  adjustment[i].xoff
		    = otfg->f.f1.value->XPlacement * x_scale / 0x10000;
		if (format & OTF_XPlaDevice)
		  adjustment[i].xoff
		    += DEVICE_DELTA (otfg->f.f1.value->XPlaDevice, x_ppem);
		if (format & OTF_YPlacement)
		  adjustment[i].yoff
		    = - (otfg->f.f1.value->YPlacement * y_scale / 0x10000);
		if (format & OTF_YPlaDevice)
		  adjustment[i].yoff
		    -= DEVICE_DELTA (otfg->f.f1.value->YPlaDevice, y_ppem);
		if (format & OTF_XAdvance)
		  adjustment[i].xadv
		    += otfg->f.f1.value->XAdvance * x_scale / 0x10000;
		if (format & OTF_XAdvDevice)
		  adjustment[i].xadv
		    += DEVICE_DELTA (otfg->f.f1.value->XAdvDevice, x_ppem);
		if (format & OTF_YAdvance)
		  adjustment[i].yadv
		    += otfg->f.f1.value->YAdvance * y_scale / 0x10000;
		if (format & OTF_YAdvDevice)
		  adjustment[i].yadv
		    += DEVICE_DELTA (otfg->f.f1.value->YAdvDevice, y_ppem);
		adjustment[i].set = 1;
	      }
	      break;
	    case 3:		/* Cursive */
	      /* Not yet supported.  */
	      break;
	    case 4:		/* Mark-to-Base */
	    case 5:		/* Mark-to-Ligature */
	      if (! base)
		break;
	      prev = base;
	      goto label_adjust_anchor;
	    default:		/* i.e. case 6 Mark-to-Mark */
	      if (! mark)
		break;
	      prev = mark;

	    label_adjust_anchor:
	      {
		int base_x, base_y, mark_x, mark_y;
		int this_from, this_to;

		base_x = otfg->f.f4.base_anchor->XCoordinate * x_scale / 0x10000;
		base_y = otfg->f.f4.base_anchor->YCoordinate * y_scale / 0x10000;
		mark_x = otfg->f.f4.mark_anchor->XCoordinate * x_scale / 0x10000;
		mark_y = otfg->f.f4.mark_anchor->YCoordinate * y_scale / 0x10000;

		if (otfg->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.base_anchor, prev->g.code,
				 x_ppem, y_ppem, &base_x, &base_y);
		if (otfg->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.mark_anchor, g->g.code,
				 x_ppem, y_ppem, &mark_x, &mark_y);
		adjustment[i].xoff = (base_x - mark_x);
		adjustment[i].yoff = - (base_y - mark_y);
		adjustment[i].back = (g - prev);
		adjustment[i].xadv = 0;
		adjustment[i].advance_is_absolute = 1;
		adjustment[i].set = 1;
		this_from = g->g.from;
		this_to = g->g.to;
		for (j = 0; prev + j < g; j++)
		  {
		    if (this_from > prev[j].g.from)
		      this_from = prev[j].g.from;
		    if (this_to < prev[j].g.to)
		      this_to = prev[j].g.to;
		  }
		for (; prev <= g; prev++)
		  {
		    prev->g.from = this_from;
		    prev->g.to = this_to;
		  }
	      }
	    }
	  if (otfg->GlyphClass == OTF_GlyphClass0)
	    base = mark = g;
	  else if (otfg->GlyphClass == OTF_GlyphClassMark)
	    mark = g;
	  else
	    base = g;
	}
    }
  SAFE_FREE ();
  return to;

 simple_copy:
  SAFE_FREE ();
  if (out->allocated < out->used + len)
    return -2;
  font->get_metrics (font, in, from, to);
  memcpy (out_glyphs + out->used, in_glyphs,
	  sizeof (MFLTGlyphFT) * len);
  out->used += len;
  return to;
}

#endif	/* not M17N_FLT_USE_NEW_FEATURE */

static MFLTGlyphString gstring;

static bool m17n_flt_initialized;

static Lisp_Object
ftfont_shape_by_flt (Lisp_Object lgstring, struct font *font,
		     FT_Face ft_face, OTF *otf, FT_Matrix *matrix)
{
  ptrdiff_t len = LGSTRING_GLYPH_LEN (lgstring);
  ptrdiff_t i;
  struct MFLTFontFT flt_font_ft;
  MFLT *flt = NULL;
  bool with_variation_selector = false;

  if (! m17n_flt_initialized)
    {
      M17N_INIT ();
#ifdef M17N_FLT_USE_NEW_FEATURE
      mflt_enable_new_feature = 1;
      mflt_try_otf = ftfont_try_otf;
#endif	/* M17N_FLT_USE_NEW_FEATURE */
      m17n_flt_initialized = 1;
    }

  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (lgstring, i);
      int c;

      if (NILP (g))
	break;
      c = LGLYPH_CHAR (g);
      if (CHAR_VARIATION_SELECTOR_P (c))
	with_variation_selector = true;
    }

  len = i;

  if (otf && with_variation_selector)
    {
      setup_otf_gstring (len);
      for (i = 0; i < len; i++)
	{
	  Lisp_Object g = LGSTRING_GLYPH (lgstring, i);

	  otf_gstring.glyphs[i].c = LGLYPH_CHAR (g);
	  otf_gstring.glyphs[i].f.index.from = LGLYPH_FROM (g);
	  otf_gstring.glyphs[i].f.index.to = LGLYPH_TO (g);
	}
      OTF_drive_cmap (otf, &otf_gstring);
      for (i = 0; i < otf_gstring.used; i++)
	{
	  OTF_Glyph *otfg = otf_gstring.glyphs + i;
	  Lisp_Object g0 = LGSTRING_GLYPH (lgstring, otfg->f.index.from);
	  Lisp_Object g1 = LGSTRING_GLYPH (lgstring, otfg->f.index.to);

	  LGLYPH_SET_CODE (g0, otfg->glyph_id);
	  LGLYPH_SET_TO (g0, LGLYPH_TO (g1));
	  LGSTRING_SET_GLYPH (lgstring, i, g0);
	}
      if (len > otf_gstring.used)
	{
	  len = otf_gstring.used;
	  LGSTRING_SET_GLYPH (lgstring, len, Qnil);
	}
    }

  {
    Lisp_Object family = Ffont_get (LGSTRING_FONT (lgstring), QCfamily);

    if (NILP (family))
      flt_font_ft.flt_font.family = Mnil;
    else
      flt_font_ft.flt_font.family
	= msymbol (SSDATA (Fdowncase (SYMBOL_NAME (family))));
  }
  flt_font_ft.flt_font.x_ppem = ft_face->size->metrics.x_ppem;
  flt_font_ft.flt_font.y_ppem = ft_face->size->metrics.y_ppem;
  flt_font_ft.flt_font.get_glyph_id = ftfont_get_glyph_id;
  flt_font_ft.flt_font.get_metrics = ftfont_get_metrics;
  flt_font_ft.flt_font.check_otf = ftfont_check_otf;
  flt_font_ft.flt_font.drive_otf = ftfont_drive_otf;
  flt_font_ft.flt_font.internal = NULL;
  flt_font_ft.font = font;
  flt_font_ft.ft_face = ft_face;
  flt_font_ft.otf = otf;
  flt_font_ft.matrix = matrix->xx != 0 ? matrix : 0;

  if (1 < len)
    {
      /* A little bit ad hoc.  Perhaps, shaper must get script and
	 language information, and select a proper flt for them
	 here.  */
      int c1 = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, 1));
      /* For the combining characters in the range U+300..U+36F,
	 "combining" is the sole FLT provided by the m17n-lib.  In
	 addition, it is the sole FLT that can handle the other
	 combining characters with non-OTF fonts.  */
      if ((0x300 <= c1 && c1 <= 0x36F)
	  || (! otf && CHAR_HAS_CATEGORY (c1, '^')))
	flt = mflt_get (msymbol ("combining"));
    }
  if (! flt && ! otf)
    {
      flt = mflt_find (LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, 0)),
		       &flt_font_ft.flt_font);
      if (! flt)
	return make_fixnum (0);
    }

  MFLTGlyphFT *glyphs = (MFLTGlyphFT *) gstring.glyphs;
  ptrdiff_t allocated = gstring.allocated;
  ptrdiff_t incr_min = len - allocated;

  do
    {
      if (0 < incr_min)
	{
	  xfree (glyphs);
	  glyphs = xpalloc (NULL, &allocated, incr_min, INT_MAX, sizeof *glyphs);
	}
      incr_min = 1;

      for (i = 0; i < len; i++)
	{
	  Lisp_Object g = LGSTRING_GLYPH (lgstring, i);
	  memset (&glyphs[i], 0, sizeof glyphs[i]);
	  glyphs[i].g.c = LGLYPH_CHAR (g);
	  if (with_variation_selector)
	    {
	      glyphs[i].g.code = LGLYPH_CODE (g);
	      glyphs[i].g.encoded = 1;
	    }
	}

      gstring.glyph_size = sizeof *glyphs;
      gstring.glyphs = (MFLTGlyph *) glyphs;
      gstring.allocated = allocated;
      gstring.used = len;
      gstring.r2l = 0;
    }
  while (mflt_run (&gstring, 0, len, &flt_font_ft.flt_font, flt) == -2);

  if (gstring.used > LGSTRING_GLYPH_LEN (lgstring))
    return Qnil;
  for (i = 0; i < gstring.used; i++)
    {
      MFLTGlyphFT *g = (MFLTGlyphFT *) (gstring.glyphs) + i;

      g->g.from = LGLYPH_FROM (LGSTRING_GLYPH (lgstring, g->g.from));
      g->g.to = LGLYPH_TO (LGSTRING_GLYPH (lgstring, g->g.to));
    }

  for (i = 0; i < gstring.used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      MFLTGlyphFT *g = (MFLTGlyphFT *) (gstring.glyphs) + i;

      if (NILP (lglyph))
	{
	  lglyph = LGLYPH_NEW ();
	  LGSTRING_SET_GLYPH (lgstring, i, lglyph);
	}
      LGLYPH_SET_FROM (lglyph, g->g.from);
      LGLYPH_SET_TO (lglyph, g->g.to);
      LGLYPH_SET_CHAR (lglyph, g->g.c);
      LGLYPH_SET_CODE (lglyph, g->g.code);
      LGLYPH_SET_WIDTH (lglyph, g->g.xadv >> 6);
      LGLYPH_SET_LBEARING (lglyph, g->g.lbearing >> 6);
      LGLYPH_SET_RBEARING (lglyph, g->g.rbearing >> 6);
      LGLYPH_SET_ASCENT (lglyph, g->g.ascent >> 6);
      LGLYPH_SET_DESCENT (lglyph, g->g.descent >> 6);
      if (g->g.adjusted)
	{
	  Lisp_Object vec = make_uninit_vector (3);

	  ASET (vec, 0, make_fixnum (g->g.xoff >> 6));
	  ASET (vec, 1, make_fixnum (g->g.yoff >> 6));
	  ASET (vec, 2, make_fixnum (g->g.xadv >> 6));
	  LGLYPH_SET_ADJUSTMENT (lglyph, vec);
	}
    }
  return make_fixnum (i);
}

Lisp_Object
ftfont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct font_info *ftfont_info = (struct font_info *) font;
  OTF *otf = ftfont_get_otf (ftfont_info);

  return ftfont_shape_by_flt (lgstring, font, ftfont_info->ft_size->face, otf,
			      &ftfont_info->matrix);
}

#endif	/* HAVE_M17N_FLT */

#endif	/* HAVE_LIBOTF */

#if defined HAVE_OTF_GET_VARIATION_GLYPHS || defined HAVE_FT_FACE_GETCHARVARIANTINDEX
int
ftfont_variation_glyphs (struct font *font, int c, unsigned variations[256])
{
  struct font_info *ftfont_info = (struct font_info *) font;
#ifdef HAVE_OTF_GET_VARIATION_GLYPHS
  OTF *otf = ftfont_get_otf (ftfont_info);

  if (! otf)
    return 0;
  return OTF_get_variation_glyphs (otf, c, variations);
#else  /* !HAVE_OTF_GET_VARIATION_GLYPHS */
  FT_Face ft_face = ftfont_info->ft_size->face;
  int i, n = 0;

  for (i = 0; i < 16; i++)
    {
      variations[i] = FT_Face_GetCharVariantIndex (ft_face, c, 0xFE00 + i);
      if (variations[i])
	n++;
    }
  for (; i < 256; i++)
    {
      variations[i] = FT_Face_GetCharVariantIndex (ft_face, c,
						   0xE0100 + (i - 16));
      if (variations[i])
	n++;
    }

  return n;
#endif  /* !HAVE_OTF_GET_VARIATION_GLYPHS */
}
#endif /* HAVE_OTF_GET_VARIATION_GLYPHS || HAVE_FT_FACE_GETCHARVARIANTINDEX */

#ifdef HAVE_HARFBUZZ

hb_font_t *
fthbfont_begin_hb_font (struct font *font, double *position_unit)
{
  struct font_info *ftfont_info = (struct font_info *) font;

  *position_unit = 1.0 / (1 << 6);
  if (! ftfont_info->hb_font)
    ftfont_info->hb_font
      = hb_ft_font_create_referenced (ftfont_info->ft_size->face);
  return ftfont_info->hb_font;
}

#ifndef HAVE_LIBOTF
#include <hb-ot.h>

static OTF *
hbotf_open (const char *name)
{
  FT_Face ft_face;

  if (! ft_library
      && FT_Init_FreeType (&ft_library) != 0)
    return NULL;
  if (FT_New_Face (ft_library, name, 0, &ft_face)
      != 0)
    return NULL;

  hb_face_t *face = hb_ft_face_create_referenced (ft_face);
  FT_Done_Face (ft_face);

  return face;
}

static int
hbotf_check_features (OTF *otf, int gsubp,
		      OTF_tag script, OTF_tag language,
		      const OTF_tag *features, int n_features)
{
  hb_face_t *face = otf;
  hb_tag_t table_tag = gsubp ? HB_OT_TAG_GSUB : HB_OT_TAG_GPOS;
  hb_tag_t script_tag = script, language_tag = language;

  unsigned int script_count
    = hb_ot_layout_table_get_script_tags (face, table_tag, 0, NULL, NULL);
  hb_tag_t *script_tags = xnmalloc (script_count, sizeof *script_tags);
  hb_ot_layout_table_get_script_tags (face, table_tag, 0, &script_count,
				      script_tags);
  unsigned int script_index;
  for (script_index = 0; script_index < script_count; script_index++)
    if (script_tags[script_index] == script_tag)
      break;
  xfree (script_tags);
  if (script_index == script_count)
    return 0;

  unsigned int language_index;
  if (language_tag == 0)
    language_index = HB_OT_LAYOUT_DEFAULT_LANGUAGE_INDEX;
  else
    {
      unsigned int language_count
	= hb_ot_layout_script_get_language_tags (face, table_tag, script_index,
						 0, NULL, NULL);
      hb_tag_t *language_tags = xnmalloc (language_count,
					  sizeof *language_tags);
      hb_ot_layout_script_get_language_tags (face, table_tag, script_index, 0,
					     &language_count, language_tags);
      for (language_index = 0; language_index < script_count; language_index++)
	if (language_tags[language_index] == language_tag)
	  break;
      xfree (language_tags);
      if (language_index == language_count)
	return 0;
    }

  for (int j = 0; j < n_features; j++)
    {
      hb_tag_t feature_tag = features[j];
      hb_bool_t negate = 0;

      if (feature_tag == 0)
	continue;
      if (feature_tag & 0x80000000)
	{
	  feature_tag &= 0x7FFFFFFF;
	  negate = 1;
	}

      unsigned int feature_index;
      if (hb_ot_layout_language_find_feature (face, table_tag, script_index,
					      language_index, feature_tag,
					      &feature_index) == negate)
	return 0;
    }
  return 1;
}
#endif	/* !HAVE_LIBOTF */
#endif /* HAVE_HARFBUZZ */

static const char *const ftfont_booleans [] = {
  ":antialias",
  ":hinting",
  ":verticallayout",
  ":autohint",
  ":globaladvance",
  ":outline",
  ":scalable",
  ":minspace",
  ":embolden",
  NULL,
};

static const char *const ftfont_non_booleans [] = {
  ":family",
  ":familylang",
  ":style",
  ":stylelang",
  ":fullname",
  ":fullnamelang",
  ":slant",
  ":weight",
  ":size",
  ":width",
  ":aspect",
  ":pixelsize",
  ":spacing",
  ":foundry",
  ":hintstyle",
  ":file",
  ":index",
  ":ftface",
  ":rasterizer",
  ":scale",
  ":dpi",
  ":rgba",
  ":lcdfilter",
  ":charset",
  ":lang",
  ":fontversion",
  ":capability",
  NULL,
};

void
ftfont_filter_properties (Lisp_Object font, Lisp_Object alist)
{
  font_filter_properties (font, alist, ftfont_booleans, ftfont_non_booleans);
}


Lisp_Object
ftfont_combining_capability (struct font *font)
{
#ifdef HAVE_M17N_FLT
  return Qt;
#else
  return Qnil;
#endif
}

static void syms_of_ftfont_for_pdumper (void);

#ifndef USE_CAIRO

static struct font_driver const ftfont_driver =
  {
  /* We can't draw a text without device dependent functions.  */
  .type = LISPSYM_INITIALLY (Qfreetype),
  .get_cache = ftfont_get_cache,
  .list = ftfont_list,
  .match = ftfont_match,
  .list_family = ftfont_list_family,
  .open_font = ftfont_open,
  .close_font = ftfont_close,
  .has_char = ftfont_has_char,
  .encode_char = ftfont_encode_char,
  .text_extents = ftfont_text_extents,
  .get_bitmap = ftfont_get_bitmap,
  .anchor_point = ftfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftfont_otf_capability,
#endif
#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
  .shape = ftfont_shape,
#endif
#if defined HAVE_OTF_GET_VARIATION_GLYPHS || defined HAVE_FT_FACE_GETCHARVARIANTINDEX
  .get_variation_glyphs = ftfont_variation_glyphs,
#endif
  .filter_properties = ftfont_filter_properties,
  .combining_capability = ftfont_combining_capability,
  };

#endif /* !USE_CAIRO */

void
syms_of_ftfont (void)
{
  /* Symbolic type of this font-driver.  */
  DEFSYM (Qfreetype, "freetype");
#ifdef HAVE_HARFBUZZ
  DEFSYM (Qfreetypehb, "freetypehb");
  Fput (Qfreetype, Qfont_driver_superseded_by, Qfreetypehb);
#endif	/* HAVE_HARFBUZZ */

  /* Fontconfig's generic families and their aliases.  */
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qsans_serif, "sans-serif");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qsans__serif, "sans serif");

  /* The boolean-valued font property key specifying the use of leading.  */
  DEFSYM (QCminspace, ":minspace");

  /* Fontconfig's rendering parameters.  */
  DEFSYM (QChinting, ":hinting");
  DEFSYM (QCautohint, ":autohint");
  DEFSYM (QChintstyle, ":hintstyle");
  DEFSYM (QCrgba, ":rgba");
  DEFSYM (QCembolden, ":embolden");
  DEFSYM (QClcdfilter, ":lcdfilter");

  staticpro (&freetype_font_cache);
  freetype_font_cache = list1 (Qt);

  staticpro (&ftfont_generic_family_list);
  ftfont_generic_family_list = list3 (Fcons (Qmonospace, Qt),
				      Fcons (Qsans_serif, Qt),
				      Fcons (Qsans, Qt));

  staticpro (&ft_face_cache);
  ft_face_cache = Qnil;

  pdumper_do_now_and_after_load (syms_of_ftfont_for_pdumper);
}

static void
syms_of_ftfont_for_pdumper (void)
{
  PDUMPER_RESET_LV (ft_face_cache, Qnil);
  register_font_driver (&ftfont_driver, NULL);
#ifdef HAVE_HARFBUZZ
  fthbfont_driver = ftfont_driver;
  fthbfont_driver.type = Qfreetypehb;
  fthbfont_driver.otf_capability = hbfont_otf_capability;
  fthbfont_driver.shape = hbfont_shape;
  fthbfont_driver.combining_capability = hbfont_combining_capability;
  fthbfont_driver.begin_hb_font = fthbfont_begin_hb_font;
  register_font_driver (&fthbfont_driver, NULL);
#endif	/* HAVE_HARFBUZZ */
}
