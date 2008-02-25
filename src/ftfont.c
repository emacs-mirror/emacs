/* ftfont.c -- FreeType font driver.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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

/* Fontconfig's charset used for finding fonts of registry
   "iso8859-1".  */
static FcCharSet *cs_iso8859_1;

/* The actual structure for FreeType font that can be casted to struct
   font.  */

struct ftfont_info
{
  struct font font;
  FT_Size ft_size;
#ifdef HAVE_LIBOTF
  int maybe_otf;	/* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
};

static int ftfont_build_basic_charsets P_ ((void));
static Lisp_Object ftfont_pattern_entity P_ ((FcPattern *,
					      Lisp_Object, Lisp_Object));
static Lisp_Object ftfont_list_generic_family P_ ((Lisp_Object, Lisp_Object,
						   Lisp_Object));
Lisp_Object ftfont_font_format P_ ((FcPattern *));

#define SYMBOL_FcChar8(SYM) (FcChar8 *) SDATA (SYMBOL_NAME (SYM))

static int
ftfont_build_basic_charsets ()
{
  FcChar32 c;

  cs_iso8859_1 = FcCharSetCreate ();
  if (! cs_iso8859_1)
    return -1;
  for (c = ' '; c < 127; c++)
    if (! FcCharSetAddChar (cs_iso8859_1, c))
      return -1;
#if 0
  /* This part is currently disabled.  Should be fixed later.  */
  for (c = 192; c < 256; c++)
    if (! FcCharSetAddChar (cs_iso8859_1, c))
      return -1;
#endif
  return 0;
}

static Lisp_Object
ftfont_pattern_entity (p, frame, registry)
     FcPattern *p;
     Lisp_Object frame, registry;
{
  Lisp_Object entity;
  FcChar8 *file, *fontformat;
  FcCharSet *charset;
  char *str;
  int numeric;
  double dbl;

  if (FcPatternGetString (p, FC_FILE, 0, &file) != FcResultMatch)
    return Qnil;
  if (FcPatternGetCharSet (p, FC_CHARSET, 0, &charset) != FcResultMatch)
    charset = NULL;
#ifdef FC_FONTFORMAT
  if (FcPatternGetString (p, FC_FONTFORMAT, 0, &fontformat) != FcResultMatch)
#endif	/* FC_FONTFORMAT */
    fontformat = NULL;

  entity = Fmake_vector (make_number (FONT_ENTITY_MAX), null_string);

  ASET (entity, FONT_TYPE_INDEX, Qfreetype);
  ASET (entity, FONT_REGISTRY_INDEX, registry);
  ASET (entity, FONT_FRAME_INDEX, frame);
  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

  if (FcPatternGetString (p, FC_FOUNDRY, 0, (FcChar8 **) &str) == FcResultMatch)
    ASET (entity, FONT_FOUNDRY_INDEX, intern_downcase (str, strlen (str)));
  if (FcPatternGetString (p, FC_FAMILY, 0, (FcChar8 **) &str) == FcResultMatch)
    ASET (entity, FONT_FAMILY_INDEX, intern_downcase (str, strlen (str)));
  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
    {
      if (numeric == FC_WEIGHT_REGULAR)
	numeric = 100;
      ASET (entity, FONT_WEIGHT_INDEX, make_number (numeric));
    }
  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
    ASET (entity, FONT_SLANT_INDEX, make_number (numeric + 100));
  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
    ASET (entity, FONT_WIDTH_INDEX, make_number (numeric));
  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
    ASET (entity, FONT_SIZE_INDEX, make_number (dbl));
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));

  if (FcPatternGetInteger (p, FC_SPACING, 0, &numeric) != FcResultMatch)
    numeric = -1;
  file = FcStrCopy (file);
  if (! file)
    return Qnil;

  p = FcPatternCreate ();
  if (! p)
    return Qnil;

  if (FcPatternAddString (p, FC_FILE, file) == FcFalse
      || (charset
	  && FcPatternAddCharSet (p, FC_CHARSET, charset) == FcFalse)
#ifdef FC_FONTFORMAT
      || (fontformat
	  && FcPatternAddString (p, FC_FONTFORMAT, fontformat) == FcFalse)
#endif	/* FC_FONTFORMAT */
      || (numeric >= 0
	  && FcPatternAddInteger (p, FC_SPACING, numeric) == FcFalse))
    {
      FcPatternDestroy (p);
      return Qnil;
    }
  ASET (entity, FONT_EXTRA_INDEX, make_save_value (p, 0));
  return entity;
}

static Lisp_Object ftfont_generic_family_list;

static Lisp_Object
ftfont_list_generic_family (spec, frame, registry)
     Lisp_Object spec, frame, registry;
{
  Lisp_Object family = AREF (spec, FONT_FAMILY_INDEX);
  Lisp_Object slot, list, val;

  if (EQ (family, Qmono))
    family = Qmonospace;
  else if (EQ (family, Qsans) || EQ (family, Qsans__serif))
    family = Qsans_serif;
  slot = assq_no_quit (family, ftfont_generic_family_list);
  if (! CONSP (slot))
    return null_vector;
  list = XCDR (slot);
  if (EQ (list, Qt))
    {
      /* Not yet listed.  */
      FcObjectSet *objset = NULL;
      FcPattern *pattern = NULL, *pat = NULL;
      FcFontSet *fontset = NULL;
      FcChar8 *fam;
      int i, j;

      objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
				 FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING,
				 FC_CHARSET, FC_FILE,
#ifdef FC_FONTFORMAT
				 FC_FONTFORMAT,
#endif	/* FC_FONTFORMAT */
				 NULL);
      if (! objset)
	goto err;
      pattern = FcPatternBuild (NULL, FC_FAMILY, FcTypeString,
				SYMBOL_FcChar8 (family), (char *) 0);
      if (! pattern)
	goto err;
      pat = FcPatternCreate ();
      if (! pat)
	goto err;
      FcConfigSubstitute (NULL, pattern, FcMatchPattern);
      for (i = 0, val = Qnil;
	   FcPatternGetString (pattern, FC_FAMILY, i, &fam) == FcResultMatch;
	   i++)
	{
	  if (strcmp ((char *) fam, (char *) SYMBOL_FcChar8 (family)) == 0)
	    continue;
	  if (! FcPatternAddString (pat, FC_FAMILY, fam))
	    goto err;
	  fontset = FcFontList (NULL, pat, objset);
	  if (! fontset)
	    goto err;
	  /* Here we build the list in reverse order so that the last
	     loop in this function build a list in the correct
	     order.  */
	  for (j = 0; j < fontset->nfont; j++)
	    {
	      Lisp_Object entity;

	      entity = ftfont_pattern_entity (fontset->fonts[j],
					      frame, registry);
	      if (! NILP (entity))
		val = Fcons (entity, val);
	    }
	  FcFontSetDestroy (fontset);
	  fontset = NULL;
	  FcPatternDel (pat, FC_FAMILY);
	}
      list = val;
      XSETCDR (slot, list);
    err:
      if (pat) FcPatternDestroy (pat);
      if (pattern) FcPatternDestroy (pattern);
      if (fontset) FcFontSetDestroy (fontset);
      if (objset) FcObjectSetDestroy (objset);
      if (EQ (list, Qt))
	return Qnil;
    }
  ASET (spec, FONT_FAMILY_INDEX, Qnil);
  for (val = Qnil; CONSP (list); list = XCDR (list))
    if (font_match_p (spec, XCAR (list)))
      val = Fcons (XCAR (list), val);
  ASET (spec, FONT_FAMILY_INDEX, family);
  return Fvconcat (1, &val);
}


static Lisp_Object ftfont_get_cache P_ ((FRAME_PTR));
static Lisp_Object ftfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object ftfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object ftfont_list_family P_ ((Lisp_Object));
static void ftfont_free_entity P_ ((Lisp_Object));
static struct font *ftfont_open P_ ((FRAME_PTR, Lisp_Object, int));
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
    ftfont_get_cache,
    ftfont_list,
    ftfont_match,
    ftfont_list_family,
    ftfont_free_entity,
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
  if (! NILP (val))
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

static Lisp_Object
ftfont_list (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object val, tmp, extra;
  int i;
  FcPattern *pattern = NULL;
  FcCharSet *charset = NULL;
  FcLangSet *langset = NULL;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  Lisp_Object script;
  Lisp_Object registry = Qunicode_bmp;
  struct OpenTypeSpec *otspec= NULL;
  int weight = 0;
  double dpi = -1;
  int spacing = -1;
  int scalable = -1;
  char otlayout[15];		/* For "otlayout:XXXX" */
  
  val = null_vector;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  if (! NILP (AREF (spec, FONT_ADSTYLE_INDEX))
      && ! EQ (AREF (spec, FONT_ADSTYLE_INDEX), null_string))
    return val;
  if (! NILP (AREF (spec, FONT_SLANT_INDEX))
      && XINT (AREF (spec, FONT_SLANT_INDEX)) < 100)
    /* Fontconfig doesn't support reverse-italic/obligue.  */
    return val;

  if (! NILP (AREF (spec, FONT_REGISTRY_INDEX)))
    {
      registry = AREF (spec, FONT_REGISTRY_INDEX);
      if (EQ (registry, Qiso8859_1))
	{
	  if (! cs_iso8859_1
	      && ftfont_build_basic_charsets () < 0)
	    return Qnil;
	  charset = cs_iso8859_1;
	}
      else if (! EQ (registry, Qiso10646_1)
	       && ! EQ (registry, Qunicode_bmp)
	       && ! EQ (registry, Qunicode_sip))
	return val;
    }

  otlayout[0] = '\0';
  script = Qnil;
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      tmp = XCAR (extra);
      key = XCAR (tmp), val = XCDR (tmp);
      if (EQ (key, QCotf))
	{
	  otspec = ftfont_get_open_type_spec (val);
	  if (! otspec)
	    return null_vector;
	  strcat (otlayout, "otlayout:");
	  OTF_TAG_STR (otspec->script_tag, otlayout + 9);
	  script = otspec->script;
	}
      else if (EQ (key, QClanguage))
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
      else if (EQ (key, QCscript))
	script = val;
      else if (EQ (key, QCdpi))
	dpi = XINT (val);
      else if (EQ (key, QCspacing))
	spacing = XINT (val);
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

  pattern = FcPatternCreate ();
  if (! pattern)
    goto err;
  tmp = AREF (spec, FONT_FOUNDRY_INDEX);
  if (SYMBOLP (tmp) && ! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FOUNDRY, SYMBOL_FcChar8 (tmp)))
    goto err;
  tmp = AREF (spec, FONT_FAMILY_INDEX);
  if (SYMBOLP (tmp) && ! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FAMILY, SYMBOL_FcChar8 (tmp)))
    goto err;
  /* Emacs conventionally doesn't distinguish normal, regular, and
     medium weight, but fontconfig does.  So, we can't restrict font
     listing by weight.  We check it after getting a list.  */
  tmp = AREF (spec, FONT_WEIGHT_INDEX);
  if (INTEGERP (tmp))
    weight = XINT (tmp);
  tmp = AREF (spec, FONT_SLANT_INDEX);
  if (INTEGERP (tmp)
      && ! FcPatternAddInteger (pattern, FC_SLANT, XINT (tmp) - 100))
    goto err;
  tmp = AREF (spec, FONT_WIDTH_INDEX);
  if (INTEGERP (tmp)
      && ! FcPatternAddInteger (pattern, FC_WIDTH, XINT (tmp)))
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

  objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
			     FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING,
			     FC_CHARSET, FC_FILE,
#ifdef FC_FONTFORMAT
			     FC_FONTFORMAT,
#endif	/* FC_FONTFORMAT */
			     NULL);
  if (! objset)
    goto err;
  if (otlayout[0])
    {
#ifdef FC_CAPABILITY
      if (! FcObjectSetAdd (objset, FC_CAPABILITY))
	goto err;
#else  /* not FC_CAPABILITY */
      goto finish;
#endif	/* not FC_CAPABILITY */
    }

  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset)
    goto err;

  if (fontset->nfont > 0)
    {
      double pixel_size;

      if (NILP (AREF (spec, FONT_SIZE_INDEX)))
	pixel_size = 0;
      else
	pixel_size = XINT (AREF (spec, FONT_SIZE_INDEX));

      for (i = 0, val = Qnil; i < fontset->nfont; i++)
	{
	  Lisp_Object entity;

	  if (pixel_size > 0)
	    {
	      double this;

	      if (FcPatternGetDouble (fontset->fonts[i], FC_PIXEL_SIZE, 0,
				      &this) == FcResultMatch
		  && ((this < pixel_size - FONT_PIXEL_SIZE_QUANTUM)
		      || (this > pixel_size + FONT_PIXEL_SIZE_QUANTUM)))
		continue;
	    }
	  if (weight > 0)
	    {
	      int this;

	      if (FcPatternGetInteger (fontset->fonts[i], FC_WEIGHT, 0,
				       &this) != FcResultMatch
		  || (this != weight
		      && (weight != 100
			  || this < FC_WEIGHT_REGULAR
			  || this > FC_WEIGHT_MEDIUM)))
		continue;
	    }
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
	  entity = ftfont_pattern_entity (fontset->fonts[i], frame, registry);
	  if (! NILP (entity))
	    val = Fcons (entity, val);
	}
      val = Fvconcat (1, &val);
    }
  else if (! NILP (AREF (spec, FONT_FAMILY_INDEX)))
    val = ftfont_list_generic_family (spec, frame, registry);
  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  val = Qnil;

 finish:
  if (charset && charset != cs_iso8859_1) FcCharSetDestroy (charset);
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (langset) FcLangSetDestroy (langset);
  if (pattern) FcPatternDestroy (pattern);
  if (otspec)
    {
      if (otspec->nfeatures[0] > 0)
	free (otspec->features[0]);
      if (otspec->nfeatures[1] > 0)
	free (otspec->features[1]);
      free (otspec);
    }
  return val;
}

static Lisp_Object
ftfont_match (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object extra, val, entity;
  FcPattern *pattern = NULL, *match = NULL;
  FcResult result;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  extra = AREF (spec, FONT_EXTRA_INDEX);
  val = assq_no_quit (QCname, extra);
  if (! CONSP (val) || ! STRINGP (XCDR (val)))
    return Qnil;

  entity = Qnil;
  pattern = FcNameParse (SDATA (XCDR (val)));
  if (pattern)
    {
      if (FcConfigSubstitute (NULL, pattern, FcMatchPattern) == FcTrue)
	{
	  FcDefaultSubstitute (pattern);
	  match = FcFontMatch (NULL, pattern, &result);
	  if (match)
	    {
	      entity = ftfont_pattern_entity (match, frame, Qunicode_bmp);
	      FcPatternDestroy (match);
	    }
	}
      FcPatternDestroy (pattern);
    }

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
	list = Fcons (intern_downcase ((char *) str, strlen ((char *) str)),
		      list);
    }

 finish:
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);

  return list;
}


static void 
ftfont_free_entity (entity)
     Lisp_Object entity;
{
  Lisp_Object val = AREF (entity, FONT_EXTRA_INDEX);
  FcPattern *pattern = XSAVE_VALUE (val)->pointer;

  FcPatternDestroy (pattern);
}

static struct font *
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
  Lisp_Object val;
  FcPattern *pattern;
  FcChar8 *file;
  int spacing;
  char *name;
  int len;

  val = AREF (entity, FONT_EXTRA_INDEX);
  if (XTYPE (val) != Lisp_Misc
      || XMISCTYPE (val) != Lisp_Misc_Save_Value)
    return NULL;
  pattern = XSAVE_VALUE (val)->pointer;
  if (XSAVE_VALUE (val)->integer == 0)
    {
      /* We have not yet created FT_Face for this font.  */
      if (! ft_library
	  && FT_Init_FreeType (&ft_library) != 0)
	return NULL;
      if (FcPatternGetString (pattern, FC_FILE, 0, &file) != FcResultMatch)
	return NULL;
      if (FT_New_Face (ft_library, (char *) file, 0, &ft_face) != 0)
	return NULL;
      FcPatternAddFTFace (pattern, FC_FT_FACE, ft_face);
      ft_size = ft_face->size;
    }
  else
    {
      if (FcPatternGetFTFace (pattern, FC_FT_FACE, 0, &ft_face)
	  != FcResultMatch)
	return NULL;
      if (FT_New_Size (ft_face, &ft_size) != 0)
	return NULL;
      if (FT_Activate_Size (ft_size) != 0)
	{
	  FT_Done_Size (ft_size);
	  return NULL;
	}
    } 

  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  if (FT_Set_Pixel_Sizes (ft_face, size, size) != 0)
    {
      if (XSAVE_VALUE (val)->integer == 0)
	FT_Done_Face (ft_face);
      return NULL;
    }

  ftfont_info = malloc (sizeof (struct ftfont_info));
  if (! ftfont_info)
    return NULL;
  ftfont_info->ft_size = ft_size;
#ifdef HAVE_LIBOTF
  ftfont_info->maybe_otf = ft_face->face_flags & FT_FACE_FLAG_SFNT;
  ftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */

  font = (struct font *) ftfont_info;
  font->format = ftfont_font_format (pattern);
  font->entity = entity;
  font->pixel_size = size;
  font->driver = &ftfont_driver;
  len = 96;
  name = malloc (len);
  while (name && font_unparse_fcname (entity, pixel_size, name, len) < 0)
    {
      char *new = realloc (name, len += 32);

      if (! new)
	free (name);
      name = new;
    }
  font->font.full_name = font->font.name = name;
  font->file_name = (char *) file;
  font->font.size = ft_face->size->metrics.max_advance >> 6;
  if (font->font.size <= 0)
    font->font.size = size;
  font->font.charset = font->encoding_charset = font->repertory_charset = -1;
  font->ascent = ft_face->size->metrics.ascender >> 6;
  font->descent = - ft_face->size->metrics.descender >> 6;
  font->font.height = font->ascent + font->descent;
  if (FcPatternGetInteger (pattern, FC_SPACING, 0, &spacing) != FcResultMatch)
    spacing = FC_PROPORTIONAL;
  if (spacing != FC_PROPORTIONAL)
    font->font.average_width = font->font.space_width = font->font.size;
  else
    {
      int i;

      font->font.average_width = font->font.space_width = 0;
      for (i = 32; i < 127; i++)
	{
	  if (FT_Load_Char (ft_face, i, FT_LOAD_DEFAULT) != 0)
	    break;
	  if (i == 32)
	    font->font.space_width = ft_face->glyph->metrics.horiAdvance >> 6;
	  font->font.average_width += ft_face->glyph->metrics.horiAdvance >> 6;
	}
      if (i == 127)
	{
	  /* The font contains all ASCII printable characters.  */
	  font->font.average_width /= 95;
	}
      else
	{
	  if (i == 32)
	    font->font.space_width = font->font.size;
	  font->font.average_width = font->font.size;
	}
    }

  /* Unfortunately FreeType doesn't provide a way to get minimum char
     width.  So, we use space_width instead.  */
  font->min_width = font->font.space_width;

  font->font.baseline_offset = 0;
  font->font.relative_compose = 0;
  font->font.default_ascent = 0;
  font->font.vertical_centering = 0;

  (XSAVE_VALUE (val)->integer)++;

  return font;
}

static void
ftfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  Lisp_Object entity = font->entity;
  Lisp_Object val = AREF (entity, FONT_EXTRA_INDEX);

  (XSAVE_VALUE (val)->integer)--;
  if (XSAVE_VALUE (val)->integer == 0)
    {
      FT_Done_Face (ftfont_info->ft_size->face);
#ifdef HAVE_LIBOTF
      if (ftfont_info->otf)
	OTF_close (ftfont_info->otf);
#endif
    }
  else
    FT_Done_Size (ftfont_info->ft_size);

  free (font);
}

static int 
ftfont_has_char (entity, c)
     Lisp_Object entity;
     int c;
{
  Lisp_Object val;
  FcPattern *pattern;
  FcCharSet *charset;

  val = AREF (entity, FONT_EXTRA_INDEX);
  pattern = XSAVE_VALUE (val)->pointer;
  if (FcPatternGetCharSet (pattern, FC_CHARSET, 0, &charset) != FcResultMatch)
    return -1;
  return (FcCharSetHasChar (charset, (FcChar32) c) == FcTrue);
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
	  width += font->font.space_width;
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
	    g->rbearing = g->xadv = flt_font_ft->font->font.space_width << 6;
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
	  OTF_Glyph *endg;
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
      flt_font_ft.flt_font.family = msymbol (SDATA (SYMBOL_NAME (family)));
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

  ftfont_driver.type = Qfreetype;
  register_font_driver (&ftfont_driver, NULL);
}

/* arch-tag: 7cfa432c-33a6-4988-83d2-a82ed8604aca
   (do not change this comment) */
