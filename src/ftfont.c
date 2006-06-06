/* ftfont.c -- FreeType font driver.
   Copyright (C) 2006 Free Software Foundation, Inc.
   Copyright (C) 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
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

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SIZES_H
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

Lisp_Object Qfreetype;

static int fc_initialized;
static FT_Library ft_library;

static Lisp_Object freetype_font_cache;

static Lisp_Object Qiso8859_1, Qiso10646_1, Qunicode_bmp;

static FcCharSet *cs_iso8859_1;

/* The actual structure for FreeType font that can be casted to struct
   font.  */

struct ftfont_info
{
  struct font font;
  FT_Size ft_size;
};

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
  for (c = 192; c < 256; c++)
    if (! FcCharSetAddChar (cs_iso8859_1, c))
      return -1;
  return 0;
}

static Lisp_Object ftfont_get_cache P_ ((Lisp_Object));
static int ftfont_parse_name P_ ((FRAME_PTR, char *, Lisp_Object));
static Lisp_Object ftfont_list P_ ((Lisp_Object, Lisp_Object));
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

struct font_driver ftfont_driver =
  {
    (Lisp_Object) NULL,		/* Qfreetype */
    ftfont_get_cache,
    ftfont_parse_name,    
    ftfont_list,
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
#ifdef HAVE_LIBOTF
    font_otf_capability,
    font_otf_gsub,
    font_otf_gpos
#else
    NULL,
    NULL,
    NULL
#endif	/* HAVE_LIBOTF */
  };

#define SYMBOL_FcChar8(SYM) (FcChar8 *) SDATA (SYMBOL_NAME (SYM))

extern Lisp_Object QCname;

static Lisp_Object
ftfont_get_cache (frame)
     Lisp_Object frame;
{
  if (NILP (freetype_font_cache))
    freetype_font_cache = Fcons (Qt, Qnil);
  return freetype_font_cache;
}

static int
ftfont_parse_name (f, name, spec)
     FRAME_PTR f;
     char *name;
     Lisp_Object spec;
{
  FcPattern *p;
  FcChar8 *str;
  int numeric;
  double dbl;

  if (name[0] == '-' || strchr (name, '*'))
    /* It seems that NAME is XLFD.  */
    return -1;
  p = FcNameParse ((FcChar8 *) name);
  if (! p)
    return -1;
  if (FcPatternGetString (p, FC_FOUNDRY, 0, &str) == FcResultMatch)
    ASET (spec, FONT_FOUNDRY_INDEX,
	  intern_downcase ((char *) str, strlen ((char *) str)));
  if (FcPatternGetString (p, FC_FAMILY, 0, &str) == FcResultMatch)
    ASET (spec, FONT_FAMILY_INDEX,
	  intern_downcase ((char *) str, strlen ((char *) str)));
  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
    ASET (spec, FONT_WEIGHT_INDEX, make_number (numeric));
  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
    ASET (spec, FONT_SLANT_INDEX, make_number (numeric + 100));
  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
    ASET (spec, FONT_WIDTH_INDEX, make_number (numeric));
  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
    ASET (spec, FONT_SIZE_INDEX, make_number (dbl));
  else if (FcPatternGetDouble (p, FC_SIZE, 0, &dbl) == FcResultMatch)
    ASET (spec, FONT_SIZE_INDEX, make_float (dbl));
  return 0;
}

static Lisp_Object
ftfont_list (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object val, tmp, extra, font_name;
  int i;
  FcPattern *pattern = NULL;
  FcCharSet *charset = NULL;
  FcLangSet *langset = NULL;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  Lisp_Object registry = Qnil;
  
  val = null_vector;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  if (! NILP (AREF (spec, FONT_ADSTYLE_INDEX)))
    return val;
  if (! NILP (AREF (spec, FONT_REGISTRY_INDEX)))
    {
      registry = AREF (spec, FONT_REGISTRY_INDEX);
      if (EQ (registry, Qiso8859_1))
	{
	  if (! cs_iso8859_1
	      && ftfont_build_basic_charsets () < 0)
	    goto err;
	  charset = cs_iso8859_1;
	  registry = Qnil;
	}
    }

  extra = AREF (spec, FONT_EXTRA_INDEX);
  font_name = Qnil;
  if (CONSP (extra))
    {
      tmp = Fassq (QCotf, extra);
      if (! NILP (tmp))
	return val;
      tmp = Fassq (QClanguage, extra);
      if (CONSP (tmp))
	{
	  langset = FcLangSetCreate ();
	  if (! langset)
	    goto err;
	  tmp = XCDR (tmp);
	  if (SYMBOLP (tmp))
	    {
	      if (! FcLangSetAdd (langset, SYMBOL_FcChar8 (tmp)))
		goto err;
	    }
	  else
	    while (CONSP (tmp))
	      {
		if (SYMBOLP (XCAR (tmp))
		    && ! FcLangSetAdd (langset, SYMBOL_FcChar8 (XCAR (tmp))))
		  goto err;
		tmp = XCDR (tmp);
	      }
	}
      tmp = Fassq (QCname, extra);
      if (CONSP (tmp))
	font_name = XCDR (tmp);
      tmp = Fassq (QCscript, extra);
      if (CONSP (tmp) && ! charset)
	{
	  Lisp_Object script = XCDR (tmp);
	  Lisp_Object chars = assq_no_quit (script,
					    Vscript_representative_chars);

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
    }

  if (! NILP (registry) && ! charset)
    goto finish;

  if (STRINGP (font_name))
    {
      if (! isalpha (SDATA (font_name)[0]))
	goto finish;
      pattern = FcNameParse (SDATA (font_name));
      if (! pattern)
	goto err;
    }
  else
    {
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
      tmp = AREF (spec, FONT_WEIGHT_INDEX);
      if (INTEGERP (tmp)
	  && ! FcPatternAddInteger (pattern, FC_WEIGHT, XINT (tmp)))
	goto err;
      tmp = AREF (spec, FONT_SLANT_INDEX);
      if (INTEGERP (tmp)
	  && XINT (tmp) >= 100
	  && ! FcPatternAddInteger (pattern, FC_SLANT, XINT (tmp) - 100))
	goto err;
      tmp = AREF (spec, FONT_WIDTH_INDEX);
      if (INTEGERP (tmp)
	  && ! FcPatternAddInteger (pattern, FC_WIDTH, XINT (tmp)))
	goto err;
      if (! FcPatternAddBool (pattern, FC_SCALABLE, FcTrue))
	goto err;
    }

  if (charset
      && ! FcPatternAddCharSet (pattern, FC_CHARSET, charset))
    goto err;
  if (langset
      && ! FcPatternAddLangSet (pattern, FC_LANG, langset))
    goto err;
  objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
			     FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING,
			     FC_CHARSET, FC_FILE, NULL);
  if (! objset)
    goto err;

  BLOCK_INPUT;
  fontset = FcFontList (NULL, pattern, objset);
  UNBLOCK_INPUT;
  if (! fontset)
    goto err;
  val = Qnil;
  for (i = 0; i < fontset->nfont; i++)
    {
      FcPattern *p = fontset->fonts[i];
      FcChar8 *str, *file;

      if (FcPatternGetString (p, FC_FILE, 0, &file) == FcResultMatch
	  && FcPatternGetCharSet (p, FC_CHARSET, 0, &charset) == FcResultMatch)
	{
	  Lisp_Object entity = Fmake_vector (make_number (FONT_ENTITY_MAX),
					     null_string);
	  int numeric;
	  double dbl;
	  FcPattern *p0;

	  ASET (entity, FONT_TYPE_INDEX, Qfreetype);
	  ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);
	  ASET (entity, FONT_FRAME_INDEX, frame);
	  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

	  if (FcPatternGetString (p, FC_FOUNDRY, 0, &str) == FcResultMatch)
	    ASET (entity, FONT_FOUNDRY_INDEX,
		  intern_downcase ((char *) str, strlen ((char *) str)));
	  if (FcPatternGetString (p, FC_FAMILY, 0, &str) == FcResultMatch)
	    ASET (entity, FONT_FAMILY_INDEX,
		  intern_downcase ((char *) str, strlen ((char *) str)));
	  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
	    ASET (entity, FONT_WEIGHT_INDEX, make_number (numeric));
	  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
	    ASET (entity, FONT_SLANT_INDEX, make_number (numeric + 100));
	  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
	    ASET (entity, FONT_WIDTH_INDEX, make_number (numeric));
	  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
	    ASET (entity, FONT_SIZE_INDEX, make_number (dbl));
	  else
	    ASET (entity, FONT_SIZE_INDEX, make_number (0));

	  if (FcPatternGetInteger (p, FC_SPACING, 0, &numeric) != FcResultMatch)
	    numeric = FC_MONO;
	  p0 = FcPatternCreate ();
	  if (! p0
	      || FcPatternAddString (p0, FC_FILE, file) == FcFalse
	      || FcPatternAddCharSet (p0, FC_CHARSET, charset) == FcFalse
	      || FcPatternAddInteger (p0, FC_SPACING, numeric) == FcFalse)
	    break;
	  ASET (entity, FONT_EXTRA_INDEX, make_save_value (p0, 0));

	  val = Fcons (entity, val);
	}
    }
  val = Fvconcat (1, &val);
  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficiency memory).  */
  val = Qnil;

 finish:
  if (charset && charset != cs_iso8859_1) FcCharSetDestroy (charset);
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (langset) FcLangSetDestroy (langset);
  if (pattern) FcPatternDestroy (pattern);

  return val;
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
  objset = FcObjectSetBuild (FC_FAMILY);
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

  font = (struct font *) ftfont_info;
  font->entity = entity;
  font->pixel_size = size;
  font->driver = &ftfont_driver;
  font->font.name = font->font.full_name = NULL;
  font->file_name = (char *) file;
  font->font.size = ft_face->size->metrics.max_advance >> 6;
  font->ascent = ft_face->size->metrics.ascender >> 6;
  font->descent = - ft_face->size->metrics.descender >> 6;
  font->font.height = ft_face->size->metrics.height >> 6;
  if (FcPatternGetInteger (pattern, FC_SPACING, 0, &spacing) != FcResultMatch
      || spacing != FC_PROPORTIONAL)
    font->font.average_width = font->font.space_width = font->font.size;
  else
    {
      int i;

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
    FT_Done_Face (ftfont_info->ft_size->face);
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
  FcPatternGetCharSet (pattern, FC_CHARSET, 0, &charset);

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

  return (code > 0 ? code : 0xFFFFFFFF);
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


void
syms_of_ftfont ()
{
  staticpro (&freetype_font_cache);
  freetype_font_cache = Qnil;

  DEFSYM (Qfreetype, "freetype");
  DEFSYM (Qiso8859_1, "iso8859-1");
  DEFSYM (Qiso10646_1, "iso10646-1");
  DEFSYM (Qunicode_bmp, "unicode-bmp");

  ftfont_driver.type = Qfreetype;
  register_font_driver (&ftfont_driver, NULL);
}
