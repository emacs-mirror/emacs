/* ftfont.c -- FreeType font driver.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

#include <config.h>
#include <stdio.h>

#include <fontconfig/fontconfig.h>
#include <fontconfig/fcfreetype.h>

#include "lisp.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "font.h"
#include "ftfont.h"

/* Symbolic type of this font-driver.  */
Lisp_Object Qfreetype;

/* Fontconfig's generic families and their aliases.  */
static Lisp_Object Qmonospace, Qsans_serif, Qserif, Qmono, Qsans, Qsans__serif;

/* Flag to tell if FcInit is areadly called or not.  */
static int fc_initialized;

/* Handle to a FreeType library instance.  */
static FT_Library ft_library;

/* Cache for FreeType fonts.  */
static Lisp_Object freetype_font_cache;

/* Cache for FT_Face */
static Lisp_Object ft_face_cache;

/* The actual structure for FreeType font that can be casted to struct
   font.  */

struct ftfont_info
{
  struct font font;
  FT_Size ft_size;
  int fc_charset_idx;
#ifdef HAVE_LIBOTF
  int maybe_otf;	/* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
};

static Lisp_Object ftfont_pattern_entity P_ ((FcPattern *, Lisp_Object, int));

static Lisp_Object ftfont_resolve_generic_family P_ ((Lisp_Object));
Lisp_Object ftfont_font_format P_ ((FcPattern *));

#define SYMBOL_FcChar8(SYM) (FcChar8 *) SDATA (SYMBOL_NAME (SYM))

static struct
{
  /* charset name */
  char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* set in syms_of_ftfont */
  int charset_id;
  /* set on demand */
  FcCharSet *fc_charset;
} fc_charset_table[] =
  { { "iso-8859-1", { 0x00A0, 0x00A1, 0x00B4, 0x00BC, 0x00D0 }, -1 },
    { "iso-8859-2", { 0x00A0, 0x010E }},
    { "iso-8859-3", { 0x00A0, 0x0108 }},
    { "iso-8859-4", { 0x00A0, 0x00AF, 0x0128, 0x0156, 0x02C7 }},
    { "iso-8859-5", { 0x00A0, 0x0401 }},
    { "iso-8859-6", { 0x00A0, 0x060C }},
    { "iso-8859-7", { 0x00A0, 0x0384 }},
    { "iso-8859-8", { 0x00A0, 0x05D0 }},
    { "iso-8859-9", { 0x00A0, 0x00A1, 0x00BC, 0x011E }},
    { "iso-8859-10", { 0x00A0, 0x00D0, 0x0128, 0x2015 }},
    { "iso-8859-11", { 0x00A0, 0x0E01 }},
    { "iso-8859-13", { 0x00A0, 0x201C }},
    { "iso-8859-14", { 0x00A0, 0x0174 }},
    { "iso-8859-15", { 0x00A0, 0x00A1, 0x00D0, 0x0152 }},
    { "iso-8859-16", { 0x00A0, 0x0218}},
    { "chinese-gb2312", { 0x4E13 }},
    { "big5", { 0xF6B1 }},
    { "japanese-jisx0208", { 0x4E55 }},
    { "korean-ksc5601", { 0xAC00 }},
    { "chinese-cns11643-1", { 0xFE32 }},
    { "chinese-cns11643-2", { 0x4E33, 0x7934 }},
    { "chinese-cns11643-3", { 0x201A9 }},
    { "chinese-cns11643-4", { 0x20057 }},
    { "chinese-cns11643-5", { 0x20000 }},
    { "chinese-cns11643-6", { 0x20003 }},
    { "chinese-cns11643-7", { 0x20055 }},
    { "chinese-gbk", { 0x4E06 }},
    { "japanese-jisx0212", { 0x4E44 }},
    { "japanese-jisx0213-1", { 0xFA10 }},
    { "japanese-jisx0213-2", { 0xFA49 }},
    { "japanese-jisx0213.2004-1", { 0x20B9F }},
    { "viscii", { 0x1EA0, 0x1EAE, 0x1ED2 }},
    { "tis620", { 0x0E01 }},
    { "windows-1251", { 0x0401, 0x0490 }},
    { "koi8-r", { 0x0401, 0x2219 }},
    { "mule-lao", { 0x0E81 }},
    { NULL }
  };

extern Lisp_Object Qc, Qm, Qp, Qd;

static Lisp_Object
ftfont_pattern_entity (p, registry, fc_charset_idx)
     FcPattern *p;
     Lisp_Object registry;
     int fc_charset_idx;
{
  Lisp_Object entity;
  char *file, *str;
  int numeric;
  double dbl;
  FcBool b;

  if (FcPatternGetString (p, FC_FILE, 0, (FcChar8 **) &file) != FcResultMatch)
    return Qnil;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, Qfreetype);
  ASET (entity, FONT_REGISTRY_INDEX, registry);

  if (FcPatternGetString (p, FC_FOUNDRY, 0, (FcChar8 **) &str) == FcResultMatch)
    ASET (entity, FONT_FOUNDRY_INDEX, font_intern_prop (str, strlen (str)));
  if (FcPatternGetString (p, FC_FAMILY, 0, (FcChar8 **) &str) == FcResultMatch)
    ASET (entity, FONT_FAMILY_INDEX, font_intern_prop (str, strlen (str)));
  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
    {
      if (numeric >= FC_WEIGHT_REGULAR && numeric < FC_WEIGHT_MEDIUM)
	numeric = FC_WEIGHT_MEDIUM;
      FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX, make_number (numeric));
    }
  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
    {
      numeric += 100;
      FONT_SET_STYLE (entity, FONT_SLANT_INDEX, make_number (numeric));
    }
  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
    {
      FONT_SET_STYLE (entity, FONT_WIDTH_INDEX, make_number (numeric));
    }
  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
    ASET (entity, FONT_SIZE_INDEX, make_number (dbl));
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));
  if (FcPatternGetInteger (p, FC_SPACING, 0, &numeric) == FcResultMatch)
    ASET (entity, FONT_SPACING_INDEX, make_number (numeric));
  if (FcPatternGetDouble (p, FC_DPI, 0, &dbl) == FcResultMatch)
    {
      int dpi = dbl;
      ASET (entity, FONT_DPI_INDEX, make_number (dpi));
    }
  if (FcPatternGetBool (p, FC_SCALABLE, 0, &b) == FcResultMatch
      && b == FcTrue)
    ASET (entity, FONT_AVGWIDTH_INDEX, make_number (0));

  font_put_extra (entity, QCfont_entity,
		  Fcons (make_unibyte_string ((char *) file,
					      strlen ((char *) file)),
			 make_number (fc_charset_idx)));
  return entity;
}


static Lisp_Object ftfont_generic_family_list;

static Lisp_Object
ftfont_resolve_generic_family (family)
     Lisp_Object family;
{
  Lisp_Object slot;
  FcPattern *pattern = NULL, *match;
  FcResult result;

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
  pattern = FcPatternBuild (NULL, FC_FAMILY, FcTypeString,
			    SYMBOL_FcChar8 (family), (char *) 0);
  if (! pattern)
    goto err;
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
 err:
  if (match) FcPatternDestroy (match);
  if (pattern) FcPatternDestroy (pattern);
  return family;
}

Lisp_Object
ftfont_lookup_cache (filename)
     Lisp_Object filename;
{
  Lisp_Object cache, val;

  cache = assoc_no_quit (filename, ft_face_cache);
  if (NILP (cache))
    {
      val = make_save_value (NULL, 0);
      cache = Fcons (filename, val);
      ft_face_cache = Fcons (cache, ft_face_cache);
    }
  else
    val = XCDR (cache);
  if (! XSAVE_VALUE (val)->pointer)
    {
      FT_Face ft_face;

      if (! ft_library
	  && FT_Init_FreeType (&ft_library) != 0)
	return Qnil;
      if (FT_New_Face (ft_library, (char *) SDATA (filename), 0, &ft_face) != 0)
	return Qnil;
      XSAVE_VALUE (val)->pointer = ft_face;
    }
  return cache;
}

static Lisp_Object ftfont_get_cache P_ ((FRAME_PTR));
static Lisp_Object ftfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object ftfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object ftfont_list_family P_ ((Lisp_Object));
static Lisp_Object ftfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void ftfont_close P_ ((FRAME_PTR, struct font *));
static int ftfont_has_char P_ ((Lisp_Object, int));
static unsigned ftfont_encode_char P_ ((struct font *, int));
static int ftfont_text_extents P_ ((struct font *, unsigned *, int,
				    struct font_metrics *));
static int ftfont_get_bitmap P_ ((struct font *, unsigned,
				  struct font_bitmap *, int));
static int ftfont_anchor_point P_ ((struct font *, unsigned, int,
				    int *, int *));
static Lisp_Object ftfont_shape P_ ((Lisp_Object));

struct font_driver ftfont_driver =
  {
    0,				/* Qfreetype */
    0,				/* case insensitive */
    ftfont_get_cache,
    ftfont_list,
    ftfont_match,
    ftfont_list_family,
    NULL,
    ftfont_open,
    ftfont_close,
    /* We can't draw a text without device dependent functions.  */
    NULL,
    NULL,
    ftfont_has_char,
    ftfont_encode_char,
    ftfont_text_extents,
    /* We can't draw a text without device dependent functions.  */
    NULL,
    ftfont_get_bitmap,
    NULL,
    NULL,
    NULL,
    ftfont_anchor_point,
    NULL,
    NULL,
    NULL,
    NULL,
#if defined (HAVE_M17N_FLT) && defined (HAVE_LIBOTF)
    ftfont_shape
#else  /* not (HAVE_M17N_FLT && HAVE_LIBOTF) */
    NULL
#endif	/* not (HAVE_M17N_FLT && HAVE_LIBOTF) */
  };

extern Lisp_Object QCname;

static Lisp_Object
ftfont_get_cache (f)
     FRAME_PTR f;
{
  return freetype_font_cache;
}

static int
ftfont_get_charset (registry)
     Lisp_Object registry;
{
  struct charset *encoding;
  int i, j;

  if (font_registry_charsets (registry, &encoding, NULL) < 0)
    return -1;
  if (fc_charset_table[0].charset_id < 0)
    /* Setup charset_id field of all elements.  */
    for (i = 0; fc_charset_table[i].name; i++)
      {
	Lisp_Object sym = intern (fc_charset_table[i].name);

	if (CHARSETP (sym))
	  fc_charset_table[i].charset_id = XINT (CHARSET_SYMBOL_ID (sym));
	else
	  fc_charset_table[i].charset_id = -1;
      }

  for (i = 0; fc_charset_table[i].name; i++)
    if (encoding->id == fc_charset_table[i].charset_id)
      break;
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

static struct OpenTypeSpec *
ftfont_get_open_type_spec (Lisp_Object otf_spec)
{
  struct OpenTypeSpec *spec = malloc (sizeof (struct OpenTypeSpec));
  Lisp_Object val;
  int i, j, negative;

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
  val = XCAR (otf_spec);
  if (! NILP (val))
    OTF_SYM_TAG (val, spec->langsys_tag);
  else
    spec->langsys_tag = 0;
  spec->nfeatures[0] = spec->nfeatures[1] = 0;
  for (i = 0; i < 2; i++)
    {
      Lisp_Object len;

      otf_spec = XCDR (otf_spec);    
      if (NILP (otf_spec))
	break;
      val = XCAR (otf_spec);
      if (NILP (val))
	continue;
      len = Flength (val);
      spec->features[i] = malloc (sizeof (int) * XINT (len));
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

static FcPattern *ftfont_spec_pattern P_ ((Lisp_Object, int *, char *,
					   struct OpenTypeSpec **));

static FcPattern *
ftfont_spec_pattern (spec, fc_charset_idx, otlayout, otspec)
     Lisp_Object spec;
     int *fc_charset_idx;
     char *otlayout;
     struct OpenTypeSpec **otspec;
{
  Lisp_Object tmp, extra;
  FcPattern *pattern = NULL;
  FcCharSet *charset = NULL;
  FcLangSet *langset = NULL;
  int n;
  int dpi = -1;
  int spacing = -1;
  int scalable = -1;
  Lisp_Object name = Qnil;
  Lisp_Object script = Qnil;
  Lisp_Object registry;

  if (! NILP (AREF (spec, FONT_ADSTYLE_INDEX))
      && SBYTES (SYMBOL_NAME (AREF (spec, FONT_ADSTYLE_INDEX))) > 0)
    /* Fontconfig doesn't support adstyle property.  */
    return NULL;
  if ((n = FONT_SLANT_NUMERIC (spec)) >= 0
      && n < 100)
    /* Fontconfig doesn't support reverse-italic/obligue.  */
    return NULL;

  if (INTEGERP (AREF (spec, FONT_DPI_INDEX)))
    dpi = XINT (AREF (spec, FONT_DPI_INDEX));
  if (INTEGERP (AREF (spec, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (spec, FONT_SPACING_INDEX));
  if (INTEGERP (AREF (spec, FONT_AVGWIDTH_INDEX))
      && XINT (AREF (spec, FONT_AVGWIDTH_INDEX)) == 0)
    scalable = 1;

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry)
      || EQ (registry, Qiso10646_1)
      || EQ (registry, Qunicode_bmp)
      || EQ (registry, Qunicode_sip))
    *fc_charset_idx = -1;
  else
    {
      *fc_charset_idx = ftfont_get_charset (registry);
      if (*fc_charset_idx < 0)
	return NULL;
      charset = fc_charset_table[*fc_charset_idx].fc_charset;
    }

  otlayout[0] = '\0';
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      key = XCAR (XCAR (extra)), val = XCDR (XCAR (extra));
      if (EQ (key, QCdpi))
	dpi = XINT (val);
      else if (EQ (key, QCfc_unknown_spec))
	name = val;
      else if (EQ (key, QClang))
	{
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
      else if (EQ (key, QCname))
	name = val;
      else if (EQ (key, QCotf))
	{
	  *otspec = ftfont_get_open_type_spec (val);
	  if (! *otspec)
	    return NULL;
	  strcat (otlayout, "otlayout:");
	  OTF_TAG_STR ((*otspec)->script_tag, otlayout + 9);
	  script = (*otspec)->script;
	}
      else if (EQ (key, QCscript))
	script = val;
      else if (EQ (key, QCscalable))
	scalable = ! NILP (val);
    }

  if (! NILP (script) && ! charset)
    {
      Lisp_Object chars = assq_no_quit (script, Vscript_representative_chars);

      if (CONSP (chars))
	{
	  charset = FcCharSetCreate ();
	  if (! charset)
	    goto err;
	  for (chars = XCDR (chars); CONSP (chars); chars = XCDR (chars))
	    if (CHARACTERP (XCAR (chars))
		&& ! FcCharSetAddChar (charset, XUINT (XCAR (chars))))
	      goto err;
	}
    }

  pattern = NILP (name) ? FcPatternCreate () : FcNameParse (SDATA (name));
  if (! pattern)
    goto err;
  FcPatternDel (pattern, FC_SIZE);
  FcPatternDel (pattern, FC_PIXEL_SIZE);
  FcPatternDel (pattern, FC_FOUNDRY);
  FcPatternDel (pattern, FC_FAMILY);
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
  if (spacing >= 0
      && ! FcPatternAddInteger (pattern, FC_SPACING, spacing))
    goto err;
  if (scalable >= 0
      && ! FcPatternAddBool (pattern, FC_SCALABLE, scalable ? FcTrue : FcFalse))
    goto err;
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
  return pattern;
}

static Lisp_Object
ftfont_list (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object val = Qnil, registry, family;
  int i;
  FcPattern *pattern;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  int fc_charset_idx;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  
  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, &fc_charset_idx, otlayout, &otspec);
  if (! pattern)
    return Qnil;
  registry = AREF (spec, FONT_REGISTRY_INDEX);
  family = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      Lisp_Object resolved;

      resolved = ftfont_resolve_generic_family (family);
      if (! NILP (resolved))
	{
	  FcPatternDel (pattern, FC_FAMILY);
	  if (! FcPatternAddString (pattern, FC_FAMILY,
				    SYMBOL_FcChar8 (resolved)))
	    goto err;
	}
    }

  objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
			     FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING, FC_SCALABLE,
			     FC_CHARSET, FC_FILE,
#ifdef FC_CAPABILITY
			     FC_CAPABILITY,
#endif	/* FC_CAPABILITY */
			     NULL);
  if (! objset)
    goto err;

  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset)
    goto err;
  for (i = 0; i < fontset->nfont; i++)
    {
      Lisp_Object entity;

#ifdef FC_CAPABILITY
      if (otlayout[0])
	{
	  FcChar8 *this;

	  if (FcPatternGetString (fontset->fonts[i], FC_CAPABILITY, 0,
				  &this) != FcResultMatch
	      || ! strstr ((char *) this, otlayout))
	    continue;
	}
#endif	/* FC_CAPABILITY */
#ifdef HAVE_LIBOTF
      if (otspec)
	{
	  FcChar8 *file;
	  OTF *otf;

	  if (FcPatternGetString (fontset->fonts[i], FC_FILE, 0, &file)
	      != FcResultMatch)
	    continue;
	  otf = OTF_open ((char *) file);
	  if (! otf)
	    continue;
	  if (OTF_check_features (otf, 1,
				  otspec->script_tag, otspec->langsys_tag,
				  otspec->features[0],
				  otspec->nfeatures[0]) != 1
	      || OTF_check_features (otf, 0,
				     otspec->script_tag, otspec->langsys_tag,
				     otspec->features[1],
				     otspec->nfeatures[1]) != 1)
	    continue;
	}
#endif	/* HAVE_LIBOTF */
      entity = ftfont_pattern_entity (fontset->fonts[i], registry,
				      fc_charset_idx);
      if (! NILP (entity))
	val = Fcons (entity, val);
    }
  font_add_log ("ftfont-list", spec, val);
  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  val = Qnil;

 finish:
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);
  return val;
}

static Lisp_Object
ftfont_match (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object entity;
  FcPattern *pattern, *match = NULL;
  FcResult result;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  int fc_charset_idx;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, &fc_charset_idx, otlayout, &otspec);
  if (! pattern)
    return Qnil;

  if (INTEGERP (AREF (spec, FONT_SIZE_INDEX)))
    {
      FcValue value;

      value.type = FcTypeDouble;
      value.u.d = XINT (AREF (spec, FONT_SIZE_INDEX));
      FcPatternAdd (pattern, FC_PIXEL_SIZE, value, FcFalse);
    }
  if (FcConfigSubstitute (NULL, pattern, FcMatchPattern) == FcTrue)
    {
      FcDefaultSubstitute (pattern);
      match = FcFontMatch (NULL, pattern, &result);
      if (match)
	{
	  entity = ftfont_pattern_entity (match, Qunicode_bmp, fc_charset_idx);
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

  font_add_log ("ftfont-match", spec, entity);
  return entity;
}

static Lisp_Object
ftfont_list_family (frame)
     Lisp_Object frame;
{
  Lisp_Object list;
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

  list = Qnil;
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


static Lisp_Object
ftfont_open (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  struct ftfont_info *ftfont_info;
  struct font *font;
  FT_Face ft_face;
  FT_Size ft_size;
  FT_UInt size;
  Lisp_Object val, filename, cache, font_object;
  int fc_charset_idx;
  FcPattern *pattern;
  int scalable;
  int spacing;
  char name[256];
  int i, len;
  int upEM;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  filename = XCAR (val);
  fc_charset_idx = XINT (XCDR (val));
  cache = ftfont_lookup_cache (filename);
  if (NILP (cache))
    return Qnil;
  filename = XCAR (cache);
  val = XCDR (cache);
  ft_face = XSAVE_VALUE (val)->pointer;
  if (XSAVE_VALUE (val)->integer > 0)
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
  XSAVE_VALUE (val)->integer++;
  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  if (FT_Set_Pixel_Sizes (ft_face, size, size) != 0)
    {
      if (XSAVE_VALUE (val)->integer == 0)
	FT_Done_Face (ft_face);
      return Qnil;
    }

  font_object = font_make_object (VECSIZE (struct ftfont_info));
  ASET (font_object, FONT_TYPE_INDEX, Qfreetype);
  for (i = 1;i < FONT_ENTITY_MAX; i++)
    ASET (font_object, i, AREF (entity, i));
  ASET (font_object, FONT_SIZE_INDEX, make_number (size));
  len = font_unparse_xlfd (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_NAME_INDEX, make_unibyte_string (name, len));
  len = font_unparse_fcname (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_FULLNAME_INDEX, make_unibyte_string (name, len));
  else
    ASET (font_object, FONT_FULLNAME_INDEX,
	  AREF (font_object, FONT_NAME_INDEX));
  ASET (font_object, FONT_FILE_INDEX, filename);
  ASET (font_object, FONT_FORMAT_INDEX, ftfont_font_format (pattern));
  font = XFONT_OBJECT (font_object);
  ftfont_info = (struct ftfont_info *) font;
  ftfont_info->ft_size = ft_face->size;
  ftfont_info->fc_charset_idx = fc_charset_idx;
#ifdef HAVE_LIBOTF
  ftfont_info->maybe_otf = ft_face->face_flags & FT_FACE_FLAG_SFNT;
  ftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
  font->pixel_size = size;
  font->driver = &ftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  upEM = ft_face->units_per_EM;
  scalable = (INTEGERP (AREF (entity, FONT_AVGWIDTH_INDEX))
	      && XINT (AREF (entity, FONT_AVGWIDTH_INDEX)) == 0);
  if (scalable)
    {
      font->ascent = ft_face->ascender * size / upEM;
      font->descent = - ft_face->descender * size / upEM;
      font->height = ft_face->height * size / upEM;
    }
  else
    {
      font->ascent = ft_face->size->metrics.ascender >> 6;
      font->descent = - ft_face->size->metrics.descender >> 6;
      font->height = ft_face->size->metrics.height >> 6;
    }
  if (INTEGERP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (spacing != FC_PROPORTIONAL)
    font->min_width = font->average_width = font->space_width
      = (scalable ? ft_face->max_advance_width * size / upEM
	 : ft_face->size->metrics.max_advance >> 6);
  else
    {
      int n;

      font->min_width = font->average_width = font->space_width = 0;
      for (i = 32, n = 0; i < 127; i++)
	if (FT_Load_Char (ft_face, i, FT_LOAD_DEFAULT) != 0)
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
      font->underline_position = -ft_face->underline_position * size / upEM;
      font->underline_thickness = -ft_face->underline_thickness * size / upEM;
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }

  return font_object;
}

static void
ftfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  Lisp_Object val, cache;

  cache = ftfont_lookup_cache (font->props[FONT_FILE_INDEX]);
  val = XCDR (cache);
  (XSAVE_VALUE (val)->integer)--;
  if (XSAVE_VALUE (val)->integer == 0)
    {
      FT_Face ft_face = XSAVE_VALUE (val)->pointer;

      FT_Done_Face (ft_face);
#ifdef HAVE_LIBOTF
      if (ftfont_info->otf)
	OTF_close (ftfont_info->otf);
#endif
      XSAVE_VALUE (val)->pointer = NULL;
    }
  else
    FT_Done_Size (ftfont_info->ft_size);
}

static int 
ftfont_has_char (entity, c)
     Lisp_Object entity;
     int c;
{
  Lisp_Object val;
  int fc_charset_idx;
  struct charset *charset;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  fc_charset_idx = XINT (XCDR (XCDR (val)));
  if (fc_charset_idx < 0)
    return -1;
  charset = CHARSET_FROM_ID (fc_charset_table[fc_charset_idx].charset_id);
  return (ENCODE_CHAR (charset, c) != CHARSET_INVALID_CODE (charset));
}

static unsigned
ftfont_encode_char (font, c)
     struct font *font;
     int c;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  FT_ULong charcode = c;
  FT_UInt code = FT_Get_Char_Index (ft_face, charcode);

  return (code > 0 ? code : FONT_INVALID_CODE);
}

static int
ftfont_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  int width = 0;
  int i;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (metrics)
    bzero (metrics, sizeof (struct font_metrics));
  for (i = 0; i < nglyphs; i++)
    {
      if (FT_Load_Glyph (ft_face, code[i], FT_LOAD_DEFAULT) == 0)
	{
	  FT_Glyph_Metrics *m = &ft_face->glyph->metrics;

	  if (metrics)
	    {
	      if (metrics->lbearing > width + (m->horiBearingX >> 6))
		metrics->lbearing = width + (m->horiBearingX >> 6);
	      if (metrics->rbearing
		  < width + ((m->horiBearingX + m->width) >> 6))
		metrics->rbearing
		  = width + ((m->horiBearingX + m->width) >> 6);
	      if (metrics->ascent < (m->horiBearingY >> 6))
		metrics->ascent = m->horiBearingY >> 6;
	      if (metrics->descent > ((m->horiBearingY + m->height) >> 6))
		metrics->descent = (m->horiBearingY + m->height) >> 6;
	    }
	  width += m->horiAdvance >> 6;
	}
      else
	{
	  width += font->space_width;
	}
    }
  if (metrics)
    metrics->width = width;

  return width;
}

static int
ftfont_get_bitmap (font, code, bitmap, bits_per_pixel)
     struct font *font;
     unsigned code;
     struct font_bitmap *bitmap;
     int bits_per_pixel;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
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
    /* We don't suport that kind of pixel mode.  */
    return -1;
  bitmap->rows = ft_face->glyph->bitmap.rows;
  bitmap->width = ft_face->glyph->bitmap.width;
  bitmap->pitch = ft_face->glyph->bitmap.pitch;
  bitmap->buffer = ft_face->glyph->bitmap.buffer;
  bitmap->left = ft_face->glyph->bitmap_left;
  bitmap->top = ft_face->glyph->bitmap_top;
  bitmap->advance = ft_face->glyph->metrics.horiAdvance >> 6;
  bitmap->extra = NULL;

  return 0;
}

static int
ftfont_anchor_point (font, code, index, x, y)
     struct font *font;
     unsigned code;
     int index;
     int *x, *y;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (FT_Load_Glyph (ft_face, code, FT_LOAD_DEFAULT) != 0)
    return -1;
  if (ft_face->glyph->format != FT_GLYPH_FORMAT_OUTLINE)
    return -1;
  if (index >= ft_face->glyph->outline.n_points)
    return -1;
  *x = ft_face->glyph->outline.points[index].x;
  *y = ft_face->glyph->outline.points[index].y;
  return 0;
}

#ifdef HAVE_LIBOTF
#ifdef HAVE_M17N_FLT

struct MFLTFontFT
{
  MFLTFont flt_font;
  struct font *font;
  FT_Face ft_face;
  OTF *otf;
};

static int
ftfont_get_glyph_id (font, gstring, from, to)
     MFLTFont *font;
     MFLTGlyphString *gstring;
     int from, to;
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyph *g;

  for (g = gstring->glyphs + from; from < to; g++, from++)
    if (! g->encoded)
      {
	FT_UInt code = FT_Get_Char_Index (ft_face, g->code);

	g->code = code > 0 ? code : FONT_INVALID_CODE;
	g->encoded = 1;
      }
  return 0;
}

static int
ftfont_get_metrics (font, gstring, from, to)
     MFLTFont *font;
     MFLTGlyphString *gstring;
     int from, to;
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyph *g;

  for (g = gstring->glyphs + from; from < to; g++, from++)
    if (! g->measured)
      {
	if (g->code != FONT_INVALID_CODE)
	  {
	    FT_Glyph_Metrics *m;

	    if (FT_Load_Glyph (ft_face, g->code, FT_LOAD_DEFAULT) != 0)
	      abort ();
	    m = &ft_face->glyph->metrics;

	    g->lbearing = m->horiBearingX;
	    g->rbearing = m->horiBearingX + m->width;
	    g->ascent = m->horiBearingY;
	    g->descent = m->height - m->horiBearingY;
	    g->xadv = m->horiAdvance;
	  }
	else
	  {
	    g->lbearing = 0;
	    g->rbearing = g->xadv = flt_font_ft->font->space_width << 6;
	    g->ascent = flt_font_ft->font->ascent << 6;
	    g->descent = flt_font_ft->font->descent << 6;
	  }
	g->yadv = 0;
	g->measured = 1;
      }
  return 0;
}

static int 
ftfont_check_otf (MFLTFont *font, MFLTOtfSpec *spec)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  OTF *otf = flt_font_ft->otf;
  OTF_Tag *tags;
  int i, n, negative;

  for (i = 0; i < 2; i++)
    {
      if (! spec->features[i])
	continue;
      for (n = 0; spec->features[i][n]; n++);
      tags = alloca (sizeof (OTF_Tag) * n);
      for (n = 0, negative = 0; spec->features[i][n]; n++)
	{
	  if (spec->features[i][n] == 0xFFFFFFFF)
	    negative = 1;
	  else if (negative)
	    tags[n - 1] = spec->features[i][n] | 0x80000000;
	  else
	    tags[n] = spec->features[i][n];
	}
      if (n - negative > 0
	  && OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				 tags, n - negative) != 1)
	return 0;
    }
  return 1;
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
      if (anchor->f.f2.XDeviceTable.offset)
	*x += DEVICE_DELTA (anchor->f.f2.XDeviceTable, x_ppem);
      if (anchor->f.f2.YDeviceTable.offset)
	*y += DEVICE_DELTA (anchor->f.f2.YDeviceTable, y_ppem);
    }
}

static OTF_GlyphString otf_gstring;

static int 
ftfont_drive_otf (font, spec, in, from, to, out, adjustment)
     MFLTFont *font;
     MFLTOtfSpec *spec;
     MFLTGlyphString *in;
     int from, to;
     MFLTGlyphString *out;
     MFLTGlyphAdjustment *adjustment;
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
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
  if (spec->langsys)
    {
      langsys = alloca (5);
      OTF_tag_name (spec->langsys, langsys);
    }
  for (i = 0; i < 2; i++)
    {
      char *p;

      if (spec->features[i] && spec->features[i][1] != 0xFFFFFFFF)
	{
	  for (j = 0; spec->features[i][j]; j++);
	  if (i == 0)
	    p = gsub_features = alloca (6 * j);
	  else
	    p = gpos_features = alloca (6 * j);
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

  if (otf_gstring.size == 0)
    {
      otf_gstring.glyphs = (OTF_Glyph *) malloc (sizeof (OTF_Glyph) * len);
      otf_gstring.size = len;
    }
  else if (otf_gstring.size < len)
    {
      otf_gstring.glyphs = (OTF_Glyph *) realloc (otf_gstring.glyphs,
						  sizeof (OTF_Glyph) * len);
      otf_gstring.size = len;
    }
  otf_gstring.used = len;
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * len);
  for (i = 0; i < len; i++)
    {
      otf_gstring.glyphs[i].c = in->glyphs[from + i].c;
      otf_gstring.glyphs[i].glyph_id = in->glyphs[from + i].code;
    }

  OTF_drive_gdef (otf, &otf_gstring);
  gidx = out->used;

  if (gsub_features)
    {
      if (OTF_drive_gsub (otf, &otf_gstring, script, langsys, gsub_features)
	  < 0)
	goto simple_copy;
      if (out->allocated < out->used + otf_gstring.used)
	return -2;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; )
	{
	  MFLTGlyph *g;
	  int min_from, max_to;
	  int j;

	  g = out->glyphs + out->used;
	  *g = in->glyphs[from + otfg->f.index.from];
	  if (g->code != otfg->glyph_id)
	    {
	      g->c = 0;
	      g->code = otfg->glyph_id;
	      g->measured = 0;
	    }
	  out->used++;
	  min_from = g->from;
	  max_to = g->to;
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
	      g->from = min_from;
	      g->to = max_to;
	    }
	  for (i++, otfg++; (i < otf_gstring.used
			     && otfg->f.index.from == otfg[-1].f.index.from);
	       i++, otfg++)
	    {
	      g = out->glyphs + out->used;
	      *g = in->glyphs[from + otfg->f.index.to];
	      if (g->code != otfg->glyph_id)
		{
		  g->c = 0;
		  g->code = otfg->glyph_id;
		  g->measured = 0;
		}
	      out->used++;
	    }
	}
    }
  else
    {
      if (out->allocated < out->used + len)
	return -2;
      for (i = 0; i < len; i++)
	out->glyphs[out->used++] = in->glyphs[from + i];
    }

  if (gpos_features)
    {
      MFLTGlyph *base = NULL, *mark = NULL, *g;
      int x_ppem, y_ppem, x_scale, y_scale;

      if (OTF_drive_gpos (otf, &otf_gstring, script, langsys, gpos_features)
	  < 0)
	return to;

      x_ppem = ft_face->size->metrics.x_ppem;
      y_ppem = ft_face->size->metrics.y_ppem;
      x_scale = ft_face->size->metrics.x_scale;
      y_scale = ft_face->size->metrics.y_scale;

      for (i = 0, otfg = otf_gstring.glyphs, g = out->glyphs + gidx;
	   i < otf_gstring.used; i++, otfg++, g++)
	{
	  MFLTGlyph *prev;

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
		mark_y = otfg->f.f4.mark_anchor->YCoordinate * y_scale / 0x10000;;

		if (otfg->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.base_anchor,
				 prev->code, x_ppem, y_ppem, &base_x, &base_y);
		if (otfg->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.mark_anchor, g->code,
				 x_ppem, y_ppem, &mark_x, &mark_y);
		adjustment[i].xoff = (base_x - mark_x);
		adjustment[i].yoff = - (base_y - mark_y);
		adjustment[i].back = (g - prev);
		adjustment[i].xadv = 0;
		adjustment[i].advance_is_absolute = 1;
		adjustment[i].set = 1;
		this_from = g->from;
		this_to = g->to;
		for (j = 0; prev + j < g; j++)
		  {
		    if (this_from > prev[j].from)
		      this_from = prev[j].from;
		    if (this_to < prev[j].to)
		      this_to = prev[j].to;
		  }
		for (; prev <= g; prev++)
		  {
		    prev->from = this_from;
		    prev->to = this_to;
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
  return to;

 simple_copy:
  if (out->allocated < out->used + len)
    return -2;
  font->get_metrics (font, in, from, to);
  memcpy (out->glyphs + out->used, in->glyphs + from,
	  sizeof (MFLTGlyph) * len);
  out->used += len;
  return to;
}

static MFLTGlyphString gstring;

static int m17n_flt_initialized;

extern Lisp_Object QCfamily;

Lisp_Object
ftfont_shape_by_flt (lgstring, font, ft_face, otf)
     Lisp_Object lgstring;
     struct font *font;
     FT_Face ft_face;
     OTF *otf;
{
  EMACS_UINT len = LGSTRING_LENGTH (lgstring);
  EMACS_UINT i;
  struct MFLTFontFT flt_font_ft;

  if (! m17n_flt_initialized)
    {
      M17N_INIT ();
      m17n_flt_initialized = 1;
    }

  for (i = 0; i < len; i++)
    if (NILP (LGSTRING_GLYPH (lgstring, i)))
      break;
  len = i;

  if (gstring.allocated == 0)
    {
      gstring.allocated = len * 2;
      gstring.glyph_size = sizeof (MFLTGlyph);
      gstring.glyphs = malloc (sizeof (MFLTGlyph) * gstring.allocated);
    }
  else if (gstring.allocated < len * 2)
    {
      gstring.allocated = len * 2;
      gstring.glyphs = realloc (gstring.glyphs,
				sizeof (MFLTGlyph) * gstring.allocated);
    }
  for (i = 0; i < len; i++)
    gstring.glyphs[i].c = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));
  gstring.used = len;
  gstring.r2l = 0;

  {
    Lisp_Object family = Ffont_get (LGSTRING_FONT (lgstring), QCfamily);

    if (NILP (family))
      flt_font_ft.flt_font.family = Mnil;
    else
      flt_font_ft.flt_font.family
	= msymbol ((char *) SDATA (SYMBOL_NAME (family)));
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
  for (i = 0; i < 3; i++)
    {
      int result = mflt_run (&gstring, 0, len, &flt_font_ft.flt_font, NULL);
      if (result != -2)
	break;
      gstring.allocated += gstring.allocated;
      gstring.glyphs = realloc (gstring.glyphs,
				sizeof (MFLTGlyph) * gstring.allocated);
    }
  if (gstring.used > LGSTRING_LENGTH (lgstring))
    return Qnil;
  for (i = 0; i < gstring.used; i++)
    {
      MFLTGlyph *g = gstring.glyphs + i;

      g->from = LGLYPH_FROM (LGSTRING_GLYPH (lgstring, g->from));
      g->to = LGLYPH_TO (LGSTRING_GLYPH (lgstring, g->to));
    }

  for (i = 0; i < gstring.used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      MFLTGlyph *g = gstring.glyphs + i;

      if (NILP (lglyph))
	{
	  lglyph = Fmake_vector (make_number (LGLYPH_SIZE), Qnil);
	  LGSTRING_SET_GLYPH (lgstring, i, lglyph);
	}
      LGLYPH_SET_FROM (lglyph, g->from);
      LGLYPH_SET_TO (lglyph, g->to);
      LGLYPH_SET_CHAR (lglyph, g->c);
      LGLYPH_SET_CODE (lglyph, g->code);
      LGLYPH_SET_WIDTH (lglyph, g->xadv >> 6);
      LGLYPH_SET_LBEARING (lglyph, g->lbearing >> 6);
      LGLYPH_SET_RBEARING (lglyph, g->rbearing >> 6);
      LGLYPH_SET_ASCENT (lglyph, g->ascent >> 6);
      LGLYPH_SET_DESCENT (lglyph, g->descent >> 6);
      if (g->adjusted)
	{
	  Lisp_Object vec;

	  vec = Fmake_vector (make_number (3), Qnil);
	  ASET (vec, 0, make_number (g->xoff >> 6));
	  ASET (vec, 1, make_number (g->yoff >> 6));
	  ASET (vec, 2, make_number (g->xadv >> 6));
	  LGLYPH_SET_ADJUSTMENT (lglyph, vec);
	}
    }
  return make_number (i);
}

Lisp_Object
ftfont_shape (lgstring)
     Lisp_Object lgstring;
{
  struct font *font;
  struct ftfont_info *ftfont_info;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  ftfont_info = (struct ftfont_info *) font;
  if (! ftfont_info->maybe_otf)
    return make_number (0);
  if (! ftfont_info->otf)
    {
      OTF *otf = OTF_open_ft_face (ftfont_info->ft_size->face);

      if (! otf || OTF_get_table (otf, "head") < 0)
	{
	  if (otf)
	    OTF_close (otf);
	  ftfont_info->maybe_otf = 0;
	  return make_number (0);
	}

      ftfont_info->otf = otf;
    }

  return ftfont_shape_by_flt (lgstring, font, ftfont_info->ft_size->face,
			      ftfont_info->otf);
}

#endif	/* HAVE_M17N_FLT */
#endif	/* HAVE_LIBOTF */

Lisp_Object
ftfont_font_format (FcPattern *pattern)
{
  FcChar8 *str;

#ifdef FC_FONTFORMAT
  if (FcPatternGetString (pattern, FC_FONTFORMAT, 0, &str) != FcResultMatch)
    return Qnil;
  if (strcmp ((char *) str, "TrueType") == 0)
    return intern ("truetype");
  if (strcmp ((char *) str, "Type 1") == 0)
    return intern ("type1");
  if (strcmp ((char *) str, "PCF") == 0)  
    return intern ("pcf");
  if (strcmp ((char *) str, "BDF") == 0)  
    return intern ("bdf");
#else  /* not FC_FONTFORMAT */
  if (FcPatternGetString (pattern, FC_FILE, 0, &str) != FcResultMatch)
    return Qnil;
  if (strcasestr ((char *) str, ".ttf") == 0)
    return intern ("truetype");
  if (strcasestr ((char *) str, "pfb") == 0)
    return intern ("type1");
  if (strcasestr ((char *) str, "pcf") == 0)  
    return intern ("pcf");
  if (strcasestr ((char *) str, "bdf") == 0)  
    return intern ("bdf");
#endif	/* not FC_FONTFORMAT */
  return intern ("unknown");
}


void
syms_of_ftfont ()
{
  DEFSYM (Qfreetype, "freetype");
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qsans_serif, "sans-serif");
  DEFSYM (Qserif, "serif");
  DEFSYM (Qmono, "mono");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qsans__serif, "sans serif");

  staticpro (&freetype_font_cache);
  freetype_font_cache = Fcons (Qt, Qnil);

  staticpro (&ftfont_generic_family_list);
  ftfont_generic_family_list
    = Fcons (Fcons (Qmonospace, Qt),
	     Fcons (Fcons (Qsans_serif, Qt),
		    Fcons (Fcons (Qsans, Qt), Qnil)));

  staticpro (&ft_face_cache);
  ft_face_cache = Qnil;

  ftfont_driver.type = Qfreetype;
  register_font_driver (&ftfont_driver, NULL);
}

/* arch-tag: 7cfa432c-33a6-4988-83d2-a82ed8604aca
   (do not change this comment) */
