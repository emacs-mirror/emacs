/* font.c -- "Font" primitives.
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
#include <stdlib.h>
#include <ctype.h>
#ifdef HAVE_M17N_FLT
#include <m17n-flt.h>
#endif

#include "lisp.h"
#include "buffer.h"
#include "frame.h"
#include "window.h"
#include "dispextern.h"
#include "charset.h"
#include "character.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NTGUI
#include "w32term.h"
#endif /* HAVE_NTGUI */

#ifdef HAVE_NS
#include "nsterm.h"
#endif /* HAVE_NS */

Lisp_Object Qfont_spec, Qfont_entity, Qfont_object;
 
#ifdef HAVE_NS
extern Lisp_Object Qfontsize;
#endif

Lisp_Object Qopentype;

/* Important character set strings.  */
Lisp_Object Qascii_0, Qiso8859_1, Qiso10646_1, Qunicode_bmp, Qunicode_sip;

#ifdef HAVE_NS
#define DEFAULT_ENCODING Qiso10646_1
#else
#define DEFAULT_ENCODING Qiso8859_1
#endif

/* Unicode category `Cf'.  */
static Lisp_Object QCf;

/* Special vector of zero length.  This is repeatedly used by (struct
   font_driver *)->list when a specified font is not found. */
static Lisp_Object null_vector;

static Lisp_Object Vfont_weight_table, Vfont_slant_table, Vfont_width_table;

/* Vector of Vfont_weight_table, Vfont_slant_table, and Vfont_width_table. */
static Lisp_Object font_style_table;

/* Structure used for tables mapping weight, slant, and width numeric
   values and their names.  */

struct table_entry
{
  int numeric;
  /* The first one is a valid name as a face attribute.
     The second one (if any) is a typical name in XLFD field.  */
  char *names[5];
  Lisp_Object *symbols;
};

/* Table of weight numeric values and their names.  This table must be
   sorted by numeric values in ascending order.  */

static struct table_entry weight_table[] =
{
  { 0, { "thin" }},
  { 20, { "ultra-light", "ultralight" }},
  { 40, { "extra-light", "extralight" }},
  { 50, { "light" }},
  { 75, { "semi-light", "semilight", "demilight", "book" }},
  { 100, { "normal", "medium", "regular" }},
  { 180, { "semi-bold", "semibold", "demibold", "demi" }},
  { 200, { "bold" }},
  { 205, { "extra-bold", "extrabold" }},
  { 210, { "ultra-bold", "ultrabold", "black" }}
};

/* Table of slant numeric values and their names.  This table must be
   sorted by numeric values in ascending order.  */

static struct table_entry slant_table[] =
{
  { 0, { "reverse-oblique", "ro" }},
  { 10, { "reverse-italic", "ri" }},
  { 100, { "normal", "r" }},
  { 200, { "italic" ,"i", "ot" }},
  { 210, { "oblique", "o" }}
};

/* Table of width numeric values and their names.  This table must be
   sorted by numeric values in ascending order.  */

static struct table_entry width_table[] =
{
  { 50, { "ultra-condensed", "ultracondensed" }},
  { 63, { "extra-condensed", "extracondensed" }},
  { 75, { "condensed", "compressed", "narrow" }},
  { 87, { "semi-condensed", "semicondensed", "demicondensed" }},
  { 100, { "normal", "medium", "regular" }},
  { 113, { "semi-expanded", "semiexpanded", "demiexpanded" }},
  { 125, { "expanded" }},
  { 150, { "extra-expanded", "extraexpanded" }},
  { 200, { "ultra-expanded", "ultraexpanded", "wide" }}
};

extern Lisp_Object Qnormal;

/* Symbols representing keys of normal font properties.  */
extern Lisp_Object QCtype, QCfamily, QCweight, QCslant, QCwidth;
extern Lisp_Object QCheight, QCsize, QCname;

Lisp_Object QCfoundry, QCadstyle, QCregistry;
/* Symbols representing keys of font extra info.  */
Lisp_Object QCspacing, QCdpi, QCscalable, QCotf, QClang, QCscript, QCavgwidth;
Lisp_Object QCantialias, QCfont_entity, QCfc_unknown_spec;
/* Symbols representing values of font spacing property.  */
Lisp_Object Qc, Qm, Qp, Qd;

Lisp_Object Vfont_encoding_alist;

/* Alist of font registry symbol and the corresponding charsets
   information.  The information is retrieved from
   Vfont_encoding_alist on demand.

   Eash element has the form:
	(REGISTRY . (ENCODING-CHARSET-ID . REPERTORY-CHARSET-ID))
   or
	(REGISTRY . nil)

   In the former form, ENCODING-CHARSET-ID is an ID of a charset that
   encodes a character code to a glyph code of a font, and
   REPERTORY-CHARSET-ID is an ID of a charset that tells if a
   character is supported by a font.

   The latter form means that the information for REGISTRY couldn't be
   retrieved.  */
static Lisp_Object font_charset_alist;

/* List of all font drivers.  Each font-backend (XXXfont.c) calls
   register_font_driver in syms_of_XXXfont to register its font-driver
   here.  */
static struct font_driver_list *font_driver_list;



/* Creaters of font-related Lisp object.  */

Lisp_Object
font_make_spec ()
{
  Lisp_Object font_spec;
  struct font_spec *spec
    = ((struct font_spec *)
       allocate_pseudovector (VECSIZE (struct font_spec),
			      FONT_SPEC_MAX, PVEC_FONT));
  XSETFONT (font_spec, spec);
  return font_spec;
}

Lisp_Object
font_make_entity ()
{
  Lisp_Object font_entity;
  struct font_entity *entity
    = ((struct font_entity *)
       allocate_pseudovector (VECSIZE (struct font_entity),
			      FONT_ENTITY_MAX, PVEC_FONT));
  XSETFONT (font_entity, entity);
  return font_entity;
}

/* Create a font-object whose structure size is SIZE.  If ENTITY is
   not nil, copy properties from ENTITY to the font-object.  If
   PIXELSIZE is positive, set the `size' property to PIXELSIZE.  */
Lisp_Object
font_make_object (size, entity, pixelsize)
     int size;
     Lisp_Object entity;
     int pixelsize;
{
  Lisp_Object font_object;
  struct font *font
    = (struct font *) allocate_pseudovector (size, FONT_OBJECT_MAX, PVEC_FONT);
  int i;

  XSETFONT (font_object, font);

  if (! NILP (entity))
    {
      for (i = 1; i < FONT_SPEC_MAX; i++)
	font->props[i] = AREF (entity, i);
      if (! NILP (AREF (entity, FONT_EXTRA_INDEX)))
	font->props[FONT_EXTRA_INDEX]
	  = Fcopy_sequence (AREF (entity, FONT_EXTRA_INDEX));
    }
  if (size > 0)
    font->props[FONT_SIZE_INDEX] = make_number (pixelsize);
  return font_object;
}



static int font_pixel_size P_ ((FRAME_PTR f, Lisp_Object));
static Lisp_Object font_open_entity P_ ((FRAME_PTR, Lisp_Object, int));
static Lisp_Object font_matching_entity P_ ((FRAME_PTR, Lisp_Object *,
					     Lisp_Object));

/* Number of registered font drivers.  */
static int num_font_drivers;


/* Return a Lispy value of a font property value at STR and LEN bytes.
   If STR is "*", it returns nil.
   If FORCE_SYMBOL is zero and all characters in STR are digits, it
   returns an integer.  Otherwise, it returns a symbol interned from
   STR.  */

Lisp_Object
font_intern_prop (str, len, force_symbol)
     char *str;
     int len;
     int force_symbol;
{
  int i;
  Lisp_Object tem;
  Lisp_Object obarray;

  if (len == 1 && *str == '*')
    return Qnil;
  if (!force_symbol && len >=1 && isdigit (*str))
    {
      for (i = 1; i < len; i++)
	if (! isdigit (str[i]))
	  break;
      if (i == len)
	return make_number (atoi (str));
    }

  /* The following code is copied from the function intern (in lread.c).  */
  obarray = Vobarray;
  if (!VECTORP (obarray) || XVECTOR (obarray)->size == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, str, len, len);
  if (SYMBOLP (tem))
    return tem;
  return Fintern (make_unibyte_string (str, len), obarray);
}

/* Return a pixel size of font-spec SPEC on frame F.  */

static int
font_pixel_size (f, spec)
     FRAME_PTR f;
     Lisp_Object spec;
{
#ifdef HAVE_WINDOW_SYSTEM
  Lisp_Object size = AREF (spec, FONT_SIZE_INDEX);
  double point_size;
  int dpi, pixel_size;
  Lisp_Object val;

  if (INTEGERP (size))
    return XINT (size);
  if (NILP (size))
    return 0;
  font_assert (FLOATP (size));
  point_size = XFLOAT_DATA (size);
  val = AREF (spec, FONT_DPI_INDEX);
  if (INTEGERP (val))
    dpi = XINT (val);
  else
    dpi = f->resy;
  pixel_size = POINT_TO_PIXEL (point_size, dpi);
  return pixel_size;
#else
  return 1;
#endif
}


/* Return a value of PROP's VAL (symbol or integer) to be stored in a
   font vector.  If VAL is not valid (i.e. not registered in
   font_style_table), return -1 if NOERROR is zero, and return a
   proper index if NOERROR is nonzero.  In that case, register VAL in
   font_style_table if VAL is a symbol, and return a closest index if
   VAL is an integer.  */

int
font_style_to_value (prop, val, noerror)
     enum font_property_index prop;
     Lisp_Object val;
     int noerror;
{
  Lisp_Object table = AREF (font_style_table, prop - FONT_WEIGHT_INDEX);
  int len = ASIZE (table);
  int i, j;

  if (SYMBOLP (val))
    {
      unsigned char *s;
      Lisp_Object args[2], elt;

      /* At first try exact match.  */
      for (i = 0; i < len; i++)
	for (j = 1; j < ASIZE (AREF (table, i)); j++)
	  if (EQ (val, AREF (AREF (table, i), j)))
	    return ((XINT (AREF (AREF (table, i), 0)) << 8)
		    | (i << 4) | (j - 1));
      /* Try also with case-folding match.  */
      s = SDATA (SYMBOL_NAME (val));
      for (i = 0; i < len; i++)
	for (j = 1; j < ASIZE (AREF (table, i)); j++)
	  {
	    elt = AREF (AREF (table, i), j);
	    if (xstrcasecmp (s, SDATA (SYMBOL_NAME (elt))) == 0)
	      return ((XINT (AREF (AREF (table, i), 0)) << 8)
		      | (i << 4) | (j - 1));
	  }
      if (! noerror)
	return -1;
      if (len == 255)
	abort ();
      elt = Fmake_vector (make_number (2), make_number (255));
      ASET (elt, 1, val);
      args[0] = table;
      args[1] = Fmake_vector (make_number (1), elt);
      ASET (font_style_table, prop - FONT_WEIGHT_INDEX, Fvconcat (2, args));
      return (255 << 8) | (i << 4);
    }
  else
    {
      int i, last_n;
      int numeric = XINT (val);

      for (i = 0, last_n = -1; i < len; i++)
	{
	  int n = XINT (AREF (AREF (table, i), 0));

	  if (numeric == n)
	    return (n << 8) | (i << 4);
	  if (numeric < n)
	    {
	      if (! noerror)
		return -1;
	      return ((i == 0 || n - numeric < numeric - last_n)
		      ? (n << 8) | (i << 4): (last_n << 8 | ((i - 1) << 4)));
	    }
	  last_n = n;
	}
      if (! noerror)
	return -1;
      return ((last_n << 8) | ((i - 1) << 4));
    }
}

Lisp_Object
font_style_symbolic (font, prop, for_face)
     Lisp_Object font;
     enum font_property_index prop;
     int for_face;
{
  Lisp_Object val = AREF (font, prop);
  Lisp_Object table, elt;
  int i;

  if (NILP (val))
    return Qnil;
  table = AREF (font_style_table, prop - FONT_WEIGHT_INDEX);
  i = XINT (val) & 0xFF;
  font_assert (((i >> 4) & 0xF) < ASIZE (table));
  elt = AREF (table, ((i >> 4) & 0xF));
  font_assert ((i & 0xF) + 1 < ASIZE (elt));
  return (for_face ? AREF (elt, 1) : AREF (elt, (i & 0xF) + 1));  
}

extern Lisp_Object Vface_alternative_font_family_alist;

extern Lisp_Object find_font_encoding P_ ((Lisp_Object));


/* Return ENCODING or a cons of ENCODING and REPERTORY of the font
   FONTNAME.  ENCODING is a charset symbol that specifies the encoding
   of the font.  REPERTORY is a charset symbol or nil.  */

Lisp_Object
find_font_encoding (fontname)
     Lisp_Object fontname;
{
  Lisp_Object tail, elt;

  for (tail = Vfont_encoding_alist; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && fast_string_match_ignore_case (XCAR (elt), fontname) >= 0
	  && (SYMBOLP (XCDR (elt))
	      ? CHARSETP (XCDR (elt))
	      : CONSP (XCDR (elt)) && CHARSETP (XCAR (XCDR (elt)))))
	return (XCDR (elt));
    }
  /* We don't know the encoding of this font.  Let's assume `ascii'.  */
  return Qascii;
}

/* Return encoding charset and repertory charset for REGISTRY in
   ENCODING and REPERTORY correspondingly.  If correct information for
   REGISTRY is available, return 0.  Otherwise return -1.  */

int
font_registry_charsets (registry, encoding, repertory)
     Lisp_Object registry;
     struct charset **encoding, **repertory;
{
  Lisp_Object val;
  int encoding_id, repertory_id;

  val = Fassoc_string (registry, font_charset_alist, Qt);
  if (! NILP (val))
    {
      val = XCDR (val);
      if (NILP (val))
	return -1;
      encoding_id = XINT (XCAR (val));
      repertory_id = XINT (XCDR (val));
    }
  else
    {
      val = find_font_encoding (SYMBOL_NAME (registry));
      if (SYMBOLP (val) && CHARSETP (val))
	{
	  encoding_id = repertory_id = XINT (CHARSET_SYMBOL_ID (val));
	}
      else if (CONSP (val))
	{
	  if (! CHARSETP (XCAR (val)))
	    goto invalid_entry;
	  encoding_id = XINT (CHARSET_SYMBOL_ID (XCAR (val)));
	  if (NILP (XCDR (val)))
	    repertory_id = -1;
	  else
	    {
	      if (! CHARSETP (XCDR (val)))
		goto invalid_entry;
	      repertory_id = XINT (CHARSET_SYMBOL_ID (XCDR (val)));
	    }
	}
      else
	goto invalid_entry;
      val = Fcons (make_number (encoding_id), make_number (repertory_id));
      font_charset_alist
	= nconc2 (font_charset_alist, Fcons (Fcons (registry, val), Qnil));
    }

  if (encoding)
    *encoding = CHARSET_FROM_ID (encoding_id);
  if (repertory)
    *repertory = repertory_id >= 0 ? CHARSET_FROM_ID (repertory_id) : NULL;
  return 0;

 invalid_entry:
  font_charset_alist
    = nconc2 (font_charset_alist, Fcons (Fcons (registry, Qnil), Qnil));
  return -1;
}


/* Font property value validaters.  See the comment of
   font_property_table for the meaning of the arguments.  */

static Lisp_Object font_prop_validate P_ ((int, Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_symbol P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_style P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_non_neg P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_spacing P_ ((Lisp_Object, Lisp_Object));
static int get_font_prop_index P_ ((Lisp_Object));

static Lisp_Object
font_prop_validate_symbol (prop, val)
     Lisp_Object prop, val;
{
  if (STRINGP (val))
    val = Fintern (val, Qnil);
  if (! SYMBOLP (val))
    val = Qerror;
  else if (EQ (prop, QCregistry))
    val = Fintern (Fdowncase (SYMBOL_NAME (val)), Qnil);
  return val;
}


static Lisp_Object
font_prop_validate_style (style, val)
     Lisp_Object style, val;
{
  enum font_property_index prop = (EQ (style, QCweight) ? FONT_WEIGHT_INDEX
				   : EQ (style, QCslant) ? FONT_SLANT_INDEX
				   : FONT_WIDTH_INDEX);
  int n;
  if (INTEGERP (val))
    {
      n = XINT (val);
      if (((n >> 4) & 0xF)
	  >= ASIZE (AREF (font_style_table, prop - FONT_WEIGHT_INDEX)))
	val = Qerror;
      else
	{
	  Lisp_Object elt = AREF (AREF (font_style_table, prop - FONT_WEIGHT_INDEX), (n >> 4) & 0xF);

	  if ((n & 0xF) + 1 >= ASIZE (elt))
	    val = Qerror;
	  else if (XINT (AREF (elt, 0)) != (n >> 8))
	    val = Qerror;
	}
    }
  else if (SYMBOLP (val))
    {
      int n = font_style_to_value (prop, val, 0);

      val = n >= 0 ? make_number (n) : Qerror;
    }
  else
    val = Qerror;
  return val;
}

static Lisp_Object
font_prop_validate_non_neg (prop, val)
     Lisp_Object prop, val;
{
  return (NATNUMP (val) || (FLOATP (val) && XFLOAT_DATA (val) >= 0)
	  ? val : Qerror);
}

static Lisp_Object
font_prop_validate_spacing (prop, val)
     Lisp_Object prop, val;
{
  if (NILP (val) || (NATNUMP (val) && XINT (val) <= FONT_SPACING_CHARCELL))
    return val;
  if (SYMBOLP (val) && SBYTES (SYMBOL_NAME (val)) == 1)
    {
      char spacing = SDATA (SYMBOL_NAME (val))[0];

      if (spacing == 'c' || spacing == 'C')
	return make_number (FONT_SPACING_CHARCELL);
      if (spacing == 'm' || spacing == 'M')
	return make_number (FONT_SPACING_MONO);
      if (spacing == 'p' || spacing == 'P')
	return make_number (FONT_SPACING_PROPORTIONAL);
      if (spacing == 'd' || spacing == 'D')
	return make_number (FONT_SPACING_DUAL);
    }
  return Qerror;
}

static Lisp_Object
font_prop_validate_otf (prop, val)
     Lisp_Object prop, val;
{
  Lisp_Object tail, tmp;
  int i;

  /* VAL = (SCRIPT [ LANGSYS [ GSUB-FEATURES [ GPOS-FEATURES ]]])
     GSUB-FEATURES = (FEATURE ... [ nil FEATURE ... ]) | nil
     GPOS-FEATURES = (FEATURE ... [ nil FEATURE ... ]) | nil  */
  if (! CONSP (val))
    return Qerror;
  if (! SYMBOLP (XCAR (val)))
    return Qerror;
  tail = XCDR (val);
  if (NILP (tail))
    return val;
  if (! CONSP (tail) || ! SYMBOLP (XCAR (val)))
    return Qerror;
  for (i = 0; i < 2; i++)
    {
      tail = XCDR (tail);
      if (NILP (tail))
	return val;
      if (! CONSP (tail))
	return Qerror;
      for (tmp = XCAR (tail); CONSP (tmp); tmp = XCDR (tmp))
	if (! SYMBOLP (XCAR (tmp)))
	  return Qerror;
      if (! NILP (tmp))
	return Qerror;
    }
  return val;
}

/* Structure of known font property keys and validater of the
   values.  */
struct
{
  /* Pointer to the key symbol.  */
  Lisp_Object *key;
  /* Function to validate PROP's value VAL, or NULL if any value is
     ok.  The value is VAL or its regularized value if VAL is valid,
     and Qerror if not.  */
  Lisp_Object (*validater) P_ ((Lisp_Object prop, Lisp_Object val));
} font_property_table[] =
  { { &QCtype, font_prop_validate_symbol },
    { &QCfoundry, font_prop_validate_symbol },
    { &QCfamily, font_prop_validate_symbol },
    { &QCadstyle, font_prop_validate_symbol },
    { &QCregistry, font_prop_validate_symbol },
    { &QCweight, font_prop_validate_style },
    { &QCslant, font_prop_validate_style },
    { &QCwidth, font_prop_validate_style },
    { &QCsize, font_prop_validate_non_neg },
    { &QCdpi, font_prop_validate_non_neg },
    { &QCspacing, font_prop_validate_spacing },
    { &QCavgwidth, font_prop_validate_non_neg },
    /* The order of the above entries must match with enum
       font_property_index.  */
    { &QClang, font_prop_validate_symbol },
    { &QCscript, font_prop_validate_symbol },
    { &QCotf, font_prop_validate_otf }
  };

/* Size (number of elements) of the above table.  */
#define FONT_PROPERTY_TABLE_SIZE \
  ((sizeof font_property_table) / (sizeof *font_property_table))

/* Return an index number of font property KEY or -1 if KEY is not an
   already known property.  */

static int
get_font_prop_index (key)
     Lisp_Object key;
{
  int i;

  for (i = 0; i < FONT_PROPERTY_TABLE_SIZE; i++)
    if (EQ (key, *font_property_table[i].key))
      return i;
  return -1;
}

/* Validate the font property.  The property key is specified by the
   symbol PROP, or the index IDX (if PROP is nil).  If VAL is invalid,
   signal an error.  The value is VAL or the regularized one.  */

static Lisp_Object
font_prop_validate (idx, prop, val)
     int idx;
     Lisp_Object prop, val;
{
  Lisp_Object validated;

  if (NILP (val))
    return val;
  if (NILP (prop))
    prop = *font_property_table[idx].key;
  else
    {
      idx = get_font_prop_index (prop);
      if (idx < 0)
	return val;
    }
  validated = (font_property_table[idx].validater) (prop, val);
  if (EQ (validated, Qerror))
    signal_error ("invalid font property", Fcons (prop, val));
  return validated;
}


/* Store VAL as a value of extra font property PROP in FONT while
   keeping the sorting order.  Don't check the validity of VAL.  */

Lisp_Object
font_put_extra (font, prop, val)
     Lisp_Object font, prop, val;
{
  Lisp_Object extra = AREF (font, FONT_EXTRA_INDEX);
  Lisp_Object slot = (NILP (extra) ? Qnil : assq_no_quit (prop, extra));

  if (NILP (slot))
    {
      Lisp_Object prev = Qnil;

      while (CONSP (extra)
	     && NILP (Fstring_lessp (prop, XCAR (XCAR (extra)))))
	prev = extra, extra = XCDR (extra);
      if (NILP (prev))
	ASET (font, FONT_EXTRA_INDEX, Fcons (Fcons (prop, val), extra));
      else
	XSETCDR (prev, Fcons (Fcons (prop, val), extra));
      return val;
    }
  XSETCDR (slot, val);
  return val;
}


/* Font name parser and unparser */

static int parse_matrix P_ ((char *));
static int font_expand_wildcards P_ ((Lisp_Object *, int));
static int font_parse_name P_ ((char *, Lisp_Object));

/* An enumerator for each field of an XLFD font name.  */
enum xlfd_field_index
{
  XLFD_FOUNDRY_INDEX,
  XLFD_FAMILY_INDEX,
  XLFD_WEIGHT_INDEX,
  XLFD_SLANT_INDEX,
  XLFD_SWIDTH_INDEX,
  XLFD_ADSTYLE_INDEX,
  XLFD_PIXEL_INDEX,
  XLFD_POINT_INDEX,
  XLFD_RESX_INDEX,
  XLFD_RESY_INDEX,
  XLFD_SPACING_INDEX,
  XLFD_AVGWIDTH_INDEX,
  XLFD_REGISTRY_INDEX,
  XLFD_ENCODING_INDEX,
  XLFD_LAST_INDEX
};

/* An enumerator for mask bit corresponding to each XLFD field.  */
enum xlfd_field_mask
{
  XLFD_FOUNDRY_MASK = 0x0001,
  XLFD_FAMILY_MASK = 0x0002,
  XLFD_WEIGHT_MASK = 0x0004,
  XLFD_SLANT_MASK = 0x0008,
  XLFD_SWIDTH_MASK = 0x0010,
  XLFD_ADSTYLE_MASK = 0x0020,
  XLFD_PIXEL_MASK = 0x0040,
  XLFD_POINT_MASK = 0x0080,
  XLFD_RESX_MASK = 0x0100,
  XLFD_RESY_MASK = 0x0200,
  XLFD_SPACING_MASK = 0x0400,
  XLFD_AVGWIDTH_MASK = 0x0800,
  XLFD_REGISTRY_MASK = 0x1000,
  XLFD_ENCODING_MASK = 0x2000
};


/* Parse P pointing the pixel/point size field of the form
   `[A B C D]' which specifies a transformation matrix:

	A  B  0
	C  D  0
	0  0  1

   by which all glyphs of the font are transformed.  The spec says
   that scalar value N for the pixel/point size is equivalent to:
   A = N * resx/resy, B = C = 0, D = N.

   Return the scalar value N if the form is valid.  Otherwise return
   -1.  */

static int
parse_matrix (p)
     char *p;
{
  double matrix[4];
  char *end;
  int i;

  for (i = 0, p++; i < 4 && *p && *p != ']'; i++)
    {
      if (*p == '~')
	matrix[i] = - strtod (p + 1, &end);
      else
	matrix[i] = strtod (p, &end);
      p = end;
    }
  return (i == 4 ? (int) matrix[3] : -1);
}

/* Expand a wildcard field in FIELD (the first N fields are filled) to
   multiple fields to fill in all 14 XLFD fields while restring a
   field position by its contents.  */

static int
font_expand_wildcards (field, n)
     Lisp_Object field[XLFD_LAST_INDEX];
     int n;
{
  /* Copy of FIELD.  */
  Lisp_Object tmp[XLFD_LAST_INDEX];
  /* Array of information about where this element can go.  Nth
     element is for Nth element of FIELD. */
  struct {
    /* Minimum possible field.  */
    int from;
    /* Maxinum possible field.  */
    int to;
    /* Bit mask of possible field.  Nth bit corresponds to Nth field.  */
    int mask;
  } range[XLFD_LAST_INDEX];
  int i, j;
  int range_from, range_to;
  unsigned range_mask;

#define XLFD_SYMBOL_MASK (XLFD_FOUNDRY_MASK | XLFD_FAMILY_MASK \
			  | XLFD_ADSTYLE_MASK  | XLFD_REGISTRY_MASK)
#define XLFD_NULL_MASK (XLFD_FOUNDRY_MASK | XLFD_ADSTYLE_MASK)
#define XLFD_LARGENUM_MASK (XLFD_POINT_MASK | XLFD_RESX_MASK | XLFD_RESY_MASK \
			    | XLFD_AVGWIDTH_MASK)
#define XLFD_REGENC_MASK (XLFD_REGISTRY_MASK | XLFD_ENCODING_MASK)

  /* Initialize RANGE_MASK for FIELD[0] which can be 0th to (14 - N)th
     field.  The value is shifted to left one bit by one in the
     following loop.  */
  for (i = 0, range_mask = 0; i <= 14 - n; i++)
    range_mask = (range_mask << 1) | 1;

  /* The triplet RANGE_FROM, RANGE_TO, and RANGE_MASK is a
     position-based retriction for FIELD[I].  */
  for (i = 0, range_from = 0, range_to = 14 - n; i < n;
       i++, range_from++, range_to++, range_mask <<= 1)
    {
      Lisp_Object val = field[i];

      tmp[i] = val;
      if (NILP (val))
	{
	  /* Wildcard.  */
	  range[i].from = range_from;
	  range[i].to = range_to;
	  range[i].mask = range_mask;
	}
      else
	{
	  /* The triplet FROM, TO, and MASK is a value-based
	     retriction for FIELD[I].  */
	  int from, to;
	  unsigned mask;

	  if (INTEGERP (val))
	    {
	      int numeric = XINT (val);

	      if (i + 1 == n)
		from = to = XLFD_ENCODING_INDEX,
		  mask = XLFD_ENCODING_MASK;
	      else if (numeric == 0)
		from = XLFD_PIXEL_INDEX, to = XLFD_AVGWIDTH_INDEX,
		  mask = XLFD_PIXEL_MASK | XLFD_LARGENUM_MASK;
	      else if (numeric <= 48)
		from = to = XLFD_PIXEL_INDEX,
		  mask = XLFD_PIXEL_MASK;
	      else
		from = XLFD_POINT_INDEX, to = XLFD_AVGWIDTH_INDEX,
		  mask = XLFD_LARGENUM_MASK;
	    }
	  else if (SBYTES (SYMBOL_NAME (val)) == 0)
	    from = XLFD_FOUNDRY_INDEX, to = XLFD_ADSTYLE_INDEX,
	      mask = XLFD_NULL_MASK;
	  else if (i == 0)
	    from = to = XLFD_FOUNDRY_INDEX, mask = XLFD_FOUNDRY_MASK;
	  else if (i + 1 == n)
	    {
	      Lisp_Object name = SYMBOL_NAME (val);

	      if (SDATA (name)[SBYTES (name) - 1] == '*')
		from = XLFD_REGISTRY_INDEX, to = XLFD_ENCODING_INDEX,
		  mask = XLFD_REGENC_MASK;
	      else
		from = to = XLFD_ENCODING_INDEX,
		  mask = XLFD_ENCODING_MASK;
	    }
	  else if (range_from <= XLFD_WEIGHT_INDEX
		   && range_to >= XLFD_WEIGHT_INDEX
		   && FONT_WEIGHT_NAME_NUMERIC (val) >= 0)
	    from = to = XLFD_WEIGHT_INDEX, mask = XLFD_WEIGHT_MASK;
	  else if (range_from <= XLFD_SLANT_INDEX
		   && range_to >= XLFD_SLANT_INDEX
		   && FONT_SLANT_NAME_NUMERIC (val) >= 0)
	    from = to = XLFD_SLANT_INDEX, mask = XLFD_SLANT_MASK;
	  else if (range_from <= XLFD_SWIDTH_INDEX
		   && range_to >= XLFD_SWIDTH_INDEX
		   && FONT_WIDTH_NAME_NUMERIC (val) >= 0)
	    from = to = XLFD_SWIDTH_INDEX, mask = XLFD_SWIDTH_MASK;
	  else
	    {
	      if (EQ (val, Qc) || EQ (val, Qm) || EQ (val, Qp) || EQ (val, Qd))
		from = to = XLFD_SPACING_INDEX, mask = XLFD_SPACING_MASK;
	      else
		from = XLFD_FOUNDRY_INDEX, to = XLFD_ENCODING_INDEX,
		  mask = XLFD_SYMBOL_MASK;
	    }

	  /* Merge position-based and value-based restrictions.  */
	  mask &= range_mask;
	  while (from < range_from)
	    mask &= ~(1 << from++);
	  while (from < 14 && ! (mask & (1 << from)))
	    from++;
	  while (to > range_to)
	    mask &= ~(1 << to--);
	  while (to >= 0 && ! (mask & (1 << to)))
	    to--;
	  if (from > to)
	    return -1;
	  range[i].from = from;
	  range[i].to = to;
	  range[i].mask = mask;

	  if (from > range_from || to < range_to)
	    {
	      /* The range is narrowed by value-based restrictions.
		 Reflect it to the other fields.  */

	      /* Following fields should be after FROM.  */
	      range_from = from;
	      /* Preceding fields should be before TO.  */
	      for (j = i - 1, from--, to--; j >= 0; j--, from--, to--)
		{
		  /* Check FROM for non-wildcard field.  */
		  if (! NILP (tmp[j]) && range[j].from < from)
		    {
		      while (range[j].from < from)
			range[j].mask &= ~(1 << range[j].from++);
		      while (from < 14 && ! (range[j].mask & (1 << from)))
			from++;
		      range[j].from = from;
		    }
		  else
		    from = range[j].from;
		  if (range[j].to > to)
		    {
		      while (range[j].to > to)
			range[j].mask &= ~(1 << range[j].to--);
		      while (to >= 0 && ! (range[j].mask & (1 << to)))
			to--;
		      range[j].to = to;
		    }
		  else
		    to = range[j].to;
		  if (from > to)
		    return -1;
		}
	    }
	}
    }

  /* Decide all fileds from restrictions in RANGE.  */
  for (i = j = 0; i < n ; i++)
    {
      if (j < range[i].from)
	{
	  if (i == 0 || ! NILP (tmp[i - 1]))
	    /* None of TMP[X] corresponds to Jth field.  */
	    return -1;
	  for (; j < range[i].from; j++)
	    field[j] = Qnil;
	}
      field[j++] = tmp[i];
    }
  if (! NILP (tmp[n - 1]) && j < XLFD_REGISTRY_INDEX)
    return -1;
  for (; j < XLFD_LAST_INDEX; j++)
    field[j] = Qnil;
  if (INTEGERP (field[XLFD_ENCODING_INDEX]))
    field[XLFD_ENCODING_INDEX]
      = Fintern (Fnumber_to_string (field[XLFD_ENCODING_INDEX]), Qnil);
  return 0;
}


#ifdef ENABLE_CHECKING
/* Match a 14-field XLFD pattern against a full XLFD font name.  */
static int
font_match_xlfd (char *pattern, char *name)
{
  while (*pattern && *name)
    {
      if (*pattern == *name)
	pattern++;
      else if (*pattern == '*')
	if (*name == pattern[1])
	  pattern += 2;
	else
	  ;
      else
	return 0;
      name++;
    }
  return 1;
}

/* Make sure the font object matches the XLFD font name.  */
static int
font_check_xlfd_parse (Lisp_Object font, char *name)
{
  char name_check[256];
  font_unparse_xlfd (font, 0, name_check, 255);
  return font_match_xlfd (name_check, name);
}

#endif


/* Parse NAME (null terminated) as XLFD and store information in FONT
   (font-spec or font-entity).  Size property of FONT is set as
   follows:
	specified XLFD fields		FONT property
	---------------------		-------------
	PIXEL_SIZE			PIXEL_SIZE (Lisp integer)
	POINT_SIZE and RESY		calculated pixel size (Lisp integer)
	POINT_SIZE			POINT_SIZE/10 (Lisp float)

   If NAME is successfully parsed, return 0.  Otherwise return -1.

   FONT is usually a font-spec, but when this function is called from
   X font backend driver, it is a font-entity.  In that case, NAME is
   a fully specified XLFD.  */

int
font_parse_xlfd (name, font)
     char *name;
     Lisp_Object font;
{
  int len = strlen (name);
  int i, j, n;
  char *f[XLFD_LAST_INDEX + 1];
  Lisp_Object val;
  char *p;

  if (len > 255)
    /* Maximum XLFD name length is 255. */
    return -1;
  /* Accept "*-.." as a fully specified XLFD. */
  if (name[0] == '*' && name[1] == '-')
    i = 1, f[XLFD_FOUNDRY_INDEX] = name;
  else
    i = 0;
  for (p = name + i; *p; p++)
    if (*p == '-')
      {
	f[i++] = p + 1;
	if (i == XLFD_LAST_INDEX)
	  break;
      }
  f[i] = name + len;

#define INTERN_FIELD(N) font_intern_prop (f[N], f[(N) + 1] - 1 - f[N], 0)
#define INTERN_FIELD_SYM(N) font_intern_prop (f[N], f[(N) + 1] - 1 - f[N], 1)

  if (i == XLFD_LAST_INDEX)
    {
      /* Fully specified XLFD.  */
      int pixel_size;

      ASET (font, FONT_FOUNDRY_INDEX, INTERN_FIELD_SYM (XLFD_FOUNDRY_INDEX));
      ASET (font, FONT_FAMILY_INDEX, INTERN_FIELD_SYM (XLFD_FAMILY_INDEX));
      for (i = XLFD_WEIGHT_INDEX, j = FONT_WEIGHT_INDEX;
	   i <= XLFD_SWIDTH_INDEX; i++, j++)
	{
	  val = INTERN_FIELD_SYM (i);
	  if (! NILP (val))
	    {
	      if ((n = font_style_to_value (j, INTERN_FIELD_SYM (i), 0)) < 0)
		return -1;
	      ASET (font, j, make_number (n));
	    }
	}
      ASET (font, FONT_ADSTYLE_INDEX, INTERN_FIELD_SYM (XLFD_ADSTYLE_INDEX));
      if (strcmp (f[XLFD_REGISTRY_INDEX], "*-*") == 0)
	ASET (font, FONT_REGISTRY_INDEX, Qnil);
      else
	ASET (font, FONT_REGISTRY_INDEX,
	      font_intern_prop (f[XLFD_REGISTRY_INDEX],
				f[XLFD_LAST_INDEX] - f[XLFD_REGISTRY_INDEX],
				1));
      p = f[XLFD_PIXEL_INDEX];
      if (*p == '[' && (pixel_size = parse_matrix (p)) >= 0)
	ASET (font, FONT_SIZE_INDEX, make_number (pixel_size));
      else
	{
	  val = INTERN_FIELD (XLFD_PIXEL_INDEX);
	  if (INTEGERP (val))
	    ASET (font, FONT_SIZE_INDEX, val);
	  else
	    {
	      double point_size = -1;

	      font_assert (FONT_SPEC_P (font));
	      p = f[XLFD_POINT_INDEX];
	      if (*p == '[')
		point_size = parse_matrix (p);
	      else if (isdigit (*p))
		point_size = atoi (p), point_size /= 10;
	      if (point_size >= 0)
		ASET (font, FONT_SIZE_INDEX, make_float (point_size));
	    }
	}

      ASET (font, FONT_DPI_INDEX, INTERN_FIELD (XLFD_RESY_INDEX));
      val = INTERN_FIELD (XLFD_SPACING_INDEX);
      if (! NILP (val))
	{
	  val = font_prop_validate_spacing (QCspacing, val);
	  if (! INTEGERP (val))
	    return -1;
	  ASET (font, FONT_SPACING_INDEX, val);
	}
      p = f[XLFD_AVGWIDTH_INDEX];
      if (*p == '~')
	p++;
      ASET (font, FONT_AVGWIDTH_INDEX,
	    font_intern_prop (p, f[XLFD_REGISTRY_INDEX] - 1 - p, 0));
    }
  else
    {
      int wild_card_found = 0;
      Lisp_Object prop[XLFD_LAST_INDEX];

      if (FONT_ENTITY_P (font))
	return -1;
      for (j = 0; j < i; j++)
	{
	  if (*f[j] == '*')
	    {
	      if (f[j][1] && f[j][1] != '-')
		return -1;
	      prop[j] = Qnil;
	      wild_card_found = 1;
	    }
	  else if (j + 1 < i)
	    prop[j] = INTERN_FIELD (j);
	  else
	    prop[j] = font_intern_prop (f[j], f[i] - f[j], 0);
	}
      if (! wild_card_found)
	return -1;
      if (font_expand_wildcards (prop, i) < 0)
	return -1;

      ASET (font, FONT_FOUNDRY_INDEX, prop[XLFD_FOUNDRY_INDEX]);
      ASET (font, FONT_FAMILY_INDEX, prop[XLFD_FAMILY_INDEX]);
      for (i = XLFD_WEIGHT_INDEX, j = FONT_WEIGHT_INDEX;
	   i <= XLFD_SWIDTH_INDEX; i++, j++)
	if (! NILP (prop[i]))
	  {
	    if ((n = font_style_to_value (j, prop[i], 1)) < 0)
	      return -1;
	    ASET (font, j, make_number (n));
	  }
      ASET (font, FONT_ADSTYLE_INDEX, prop[XLFD_ADSTYLE_INDEX]);
      val = prop[XLFD_REGISTRY_INDEX];
      if (NILP (val))
	{
	  val = prop[XLFD_ENCODING_INDEX];
	  if (! NILP (val))
	    val = concat2 (build_string ("*-"), SYMBOL_NAME (val));
	}
      else if (NILP (prop[XLFD_ENCODING_INDEX]))
	val = concat2 (SYMBOL_NAME (val), build_string ("-*"));
      else
	val = concat3 (SYMBOL_NAME (val), build_string ("-"),
		       SYMBOL_NAME (prop[XLFD_ENCODING_INDEX]));
      if (! NILP (val))
	ASET (font, FONT_REGISTRY_INDEX, Fintern (val, Qnil));

      if (INTEGERP (prop[XLFD_PIXEL_INDEX]))
	ASET (font, FONT_SIZE_INDEX, prop[XLFD_PIXEL_INDEX]);
      else if (INTEGERP (prop[XLFD_POINT_INDEX]))
	{
	  double point_size = XINT (prop[XLFD_POINT_INDEX]);

	  ASET (font, FONT_SIZE_INDEX, make_float (point_size / 10));
	}

      if (INTEGERP (prop[XLFD_RESX_INDEX]))
	ASET (font, FONT_DPI_INDEX, prop[XLFD_RESY_INDEX]);
      if (! NILP (prop[XLFD_SPACING_INDEX]))
	{
	  val = font_prop_validate_spacing (QCspacing,
					    prop[XLFD_SPACING_INDEX]);
	  if (! INTEGERP (val))
	    return -1;
	  ASET (font, FONT_SPACING_INDEX, val);
	}
      if (INTEGERP (prop[XLFD_AVGWIDTH_INDEX]))
	ASET (font, FONT_AVGWIDTH_INDEX, prop[XLFD_AVGWIDTH_INDEX]);
    }

  return 0;
}

/* Store XLFD name of FONT (font-spec or font-entity) in NAME (NBYTES
   length), and return the name length.  If FONT_SIZE_INDEX of FONT is
   0, use PIXEL_SIZE instead.  */

int
font_unparse_xlfd (font, pixel_size, name, nbytes)
     Lisp_Object font;
     int pixel_size;
     char *name;
     int nbytes;
{
  char *f[XLFD_REGISTRY_INDEX + 1];
  Lisp_Object val;
  int i, j, len = 0;

  font_assert (FONTP (font));

  for (i = FONT_FOUNDRY_INDEX, j = XLFD_FOUNDRY_INDEX; i <= FONT_REGISTRY_INDEX;
       i++, j++)
    {
      if (i == FONT_ADSTYLE_INDEX)
	j = XLFD_ADSTYLE_INDEX;
      else if (i == FONT_REGISTRY_INDEX)
	j = XLFD_REGISTRY_INDEX;
      val = AREF (font, i);
      if (NILP (val))
	{
	  if (j == XLFD_REGISTRY_INDEX)
	    f[j] = "*-*", len += 4;
	  else
	    f[j] = "*", len += 2;
	}
      else
	{
	  if (SYMBOLP (val))
	    val = SYMBOL_NAME (val);
	  if (j == XLFD_REGISTRY_INDEX
	      && ! strchr ((char *) SDATA (val), '-'))
	    {
	      /* Change "jisx0208*" and "jisx0208" to "jisx0208*-*".  */
	      if (SDATA (val)[SBYTES (val) - 1] == '*')
		{
		  f[j] = alloca (SBYTES (val) + 3);
		  sprintf (f[j], "%s-*", SDATA (val));
		  len += SBYTES (val) + 3;
		}
	      else
		{
		  f[j] = alloca (SBYTES (val) + 4);
		  sprintf (f[j], "%s*-*", SDATA (val));
		  len += SBYTES (val) + 4;
		}
	    }
	  else
	    f[j] = (char *) SDATA (val), len += SBYTES (val) + 1;
	}
    }

  for (i = FONT_WEIGHT_INDEX, j = XLFD_WEIGHT_INDEX; i <= FONT_WIDTH_INDEX;
       i++, j++)
    {
      val = font_style_symbolic (font, i, 0);
      if (NILP (val))
	f[j] = "*", len += 2;
      else
	{
	  val = SYMBOL_NAME (val);
	  f[j] = (char *) SDATA (val), len += SBYTES (val) + 1;
	}
    }

  val = AREF (font, FONT_SIZE_INDEX);
  font_assert (NUMBERP (val) || NILP (val));
  if (INTEGERP (val))
    {
      i = XINT (val);
      if (i <= 0)
	i = pixel_size;
      if (i > 0)
	{
	  f[XLFD_PIXEL_INDEX] = alloca (22);
	  len += sprintf (f[XLFD_PIXEL_INDEX], "%d-*", i) + 1;
	}
      else
	f[XLFD_PIXEL_INDEX] = "*-*", len += 4;
    }
  else if (FLOATP (val))
    {
      i = XFLOAT_DATA (val) * 10;
      f[XLFD_PIXEL_INDEX] = alloca (12);
      len += sprintf (f[XLFD_PIXEL_INDEX], "*-%d", i) + 1;
    }
  else
    f[XLFD_PIXEL_INDEX] = "*-*", len += 4;

  if (INTEGERP (AREF (font, FONT_DPI_INDEX)))
    {
      i = XINT (AREF (font, FONT_DPI_INDEX));
      f[XLFD_RESX_INDEX] = alloca (22);
      len += sprintf (f[XLFD_RESX_INDEX],
		      "%d-%d", i, i) + 1;
    }
  else
    f[XLFD_RESX_INDEX] = "*-*", len += 4;
  if (INTEGERP (AREF (font, FONT_SPACING_INDEX)))
    {
      int spacing = XINT (AREF (font, FONT_SPACING_INDEX));

      f[XLFD_SPACING_INDEX] = (spacing <= FONT_SPACING_PROPORTIONAL ? "p"
			       : spacing <= FONT_SPACING_DUAL ? "d"
			       : spacing <= FONT_SPACING_MONO ? "m"
			       : "c");
      len += 2;
    }
  else
    f[XLFD_SPACING_INDEX] = "*", len += 2;
  if (INTEGERP (AREF (font,  FONT_AVGWIDTH_INDEX)))
    {
      f[XLFD_AVGWIDTH_INDEX] = alloca (11);
      len += sprintf (f[XLFD_AVGWIDTH_INDEX],
		      "%d", XINT (AREF (font, FONT_AVGWIDTH_INDEX))) + 1;
    }
  else
    f[XLFD_AVGWIDTH_INDEX] = "*", len += 2;
  len++;	/* for terminating '\0'.  */
  if (len >= nbytes)
    return -1;
  return sprintf (name, "-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s",
		  f[XLFD_FOUNDRY_INDEX], f[XLFD_FAMILY_INDEX],
		  f[XLFD_WEIGHT_INDEX], f[XLFD_SLANT_INDEX],
		  f[XLFD_SWIDTH_INDEX], f[XLFD_ADSTYLE_INDEX],
		  f[XLFD_PIXEL_INDEX], f[XLFD_RESX_INDEX],
		  f[XLFD_SPACING_INDEX], f[XLFD_AVGWIDTH_INDEX],
		  f[XLFD_REGISTRY_INDEX]);
}

/* Parse NAME (null terminated) and store information in FONT
   (font-spec or font-entity).  NAME is supplied in either the
   Fontconfig or GTK font name format.  If NAME is successfully
   parsed, return 0.  Otherwise return -1.

   The fontconfig format is

    FAMILY[-SIZE][:PROP1[=VAL1][:PROP2[=VAL2]...]]

   The GTK format is

    FAMILY [PROPS...] [SIZE]

   This function tries to guess which format it is.  */

int
font_parse_fcname (name, font)
     char *name;
     Lisp_Object font;
{
  char *p, *q;
  char *size_beg = NULL, *size_end = NULL;
  char *props_beg = NULL, *family_end = NULL;
  int len = strlen (name);

  if (len == 0)
    return -1;

  for (p = name; *p; p++)
    {
      if (*p == '\\' && p[1])
	p++;
      else if (*p == ':')
	{
	  props_beg = family_end = p;
	  break;
	}
      else if (*p == '-')
	{
	  int decimal = 0, size_found = 1;
	  for (q = p + 1; *q && *q != ':'; q++)
	    if (! isdigit(*q))
	      {
		if (*q != '.' || decimal)
		  {
		    size_found = 0;
		    break;
		  }
		decimal = 1;
	      }
	  if (size_found)
	    {
	      family_end = p;
	      size_beg = p + 1;
	      size_end = q;
	      break;
	    }
	}
    }

  if (family_end)
    {
      /* A fontconfig name with size and/or property data.  */
      if (family_end > name)
	{
	  Lisp_Object family;
	  family = font_intern_prop (name, family_end - name, 1);
	  ASET (font, FONT_FAMILY_INDEX, family);
	}
      if (size_beg)
	{
	  double point_size = strtod (size_beg, &size_end);
	  ASET (font, FONT_SIZE_INDEX, make_float (point_size));
	  if (*size_end == ':' && size_end[1])
	    props_beg = size_end;
	}
      if (props_beg)
	{
	  /* Now parse ":KEY=VAL" patterns.  */
	  Lisp_Object val;

	  for (p = props_beg; *p; p = q)
	    {
	      for (q = p + 1; *q && *q != '=' && *q != ':'; q++);
	      if (*q != '=')
		{
		  /* Must be an enumerated value.  */
		  int word_len;
		  p = p + 1;
		  word_len = q - p;
		  val = font_intern_prop (p, q - p, 1);

#define PROP_MATCH(STR,N) ((word_len == N) && memcmp (p, STR, N) == 0)

		  if (PROP_MATCH ("light", 5)
		      || PROP_MATCH ("medium", 6)
		      || PROP_MATCH ("demibold", 8)
		      || PROP_MATCH ("bold", 4)
		      || PROP_MATCH ("black", 5))
		    FONT_SET_STYLE (font, FONT_WEIGHT_INDEX, val);
		  else if (PROP_MATCH ("roman", 5)
			   || PROP_MATCH ("italic", 6)
			   || PROP_MATCH ("oblique", 7))
		    FONT_SET_STYLE (font, FONT_SLANT_INDEX, val);
		  else if (PROP_MATCH ("charcell", 8))
		    ASET (font, FONT_SPACING_INDEX,
			  make_number (FONT_SPACING_CHARCELL));
		  else if (PROP_MATCH ("mono", 4))
		    ASET (font, FONT_SPACING_INDEX,
			  make_number (FONT_SPACING_MONO));
		  else if (PROP_MATCH ("proportional", 12))
		    ASET (font, FONT_SPACING_INDEX,
			  make_number (FONT_SPACING_PROPORTIONAL));
#undef PROP_MATCH
		}
	      else
		{
		  /* KEY=VAL pairs  */
		  Lisp_Object key;
		  int prop;

		  if (q - p == 10 && memcmp (p + 1, "pixelsize", 9) == 0)
		    prop = FONT_SIZE_INDEX;
		  else
		    {
		      key = font_intern_prop (p, q - p, 1);
		      prop = get_font_prop_index (key);
		    }

		  p = q + 1;
		  for (q = p; *q && *q != ':'; q++);
		  val = font_intern_prop (p, q - p, 0);

		  if (prop >= FONT_FOUNDRY_INDEX
		      && prop < FONT_EXTRA_INDEX)
		    ASET (font, prop, font_prop_validate (prop, Qnil, val));
		  else
		    Ffont_put (font, key, val);
		}
	      p = q;
	    }
	}
    }
  else
    {
      /* Either a fontconfig-style name with no size and property
	 data, or a GTK-style name.  */
      Lisp_Object prop;
      int word_len, prop_found = 0;

      for (p = name; *p; p = *q ? q + 1 : q)
	{
	  if (isdigit (*p))
	    {
	      int size_found = 1;

	      for (q = p + 1; *q && *q != ' '; q++)
		if (! isdigit (*q))
		  {
		    size_found = 0;
		    break;
		  }
	      if (size_found)
		{
		  double point_size = strtod (p, &q);
		  ASET (font, FONT_SIZE_INDEX, make_float (point_size));
		  continue;
		}
	    }

	  for (q = p + 1; *q && *q != ' '; q++)
	    if (*q == '\\' && q[1])
	      q++;
	  word_len = q - p;

#define PROP_MATCH(STR,N) ((word_len == N) && memcmp (p, STR, N) == 0)

	  if (PROP_MATCH ("Ultra-Light", 11))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("ultra-light", 11, 1);
	      FONT_SET_STYLE (font, FONT_WEIGHT_INDEX, prop);
	    }
	  else if (PROP_MATCH ("Light", 5))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("light", 5, 1);
	      FONT_SET_STYLE (font, FONT_WEIGHT_INDEX, prop);
	    }
	  else if (PROP_MATCH ("Semi-Bold", 9))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("semi-bold", 9, 1);
	      FONT_SET_STYLE (font, FONT_WEIGHT_INDEX, prop);
	    }
	  else if (PROP_MATCH ("Bold", 4))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("bold", 4, 1);
	      FONT_SET_STYLE (font, FONT_WEIGHT_INDEX, prop);
	    }
	  else if (PROP_MATCH ("Italic", 6))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("italic", 4, 1);
	      FONT_SET_STYLE (font, FONT_SLANT_INDEX, prop);
	    }
	  else if (PROP_MATCH ("Oblique", 7))
	    {
	      prop_found = 1;
	      prop = font_intern_prop ("oblique", 7, 1);
	      FONT_SET_STYLE (font, FONT_SLANT_INDEX, prop);
	    }
	  else {
	    if (prop_found)
	      return -1; /* Unknown property in GTK-style font name.  */
	    family_end = q;
	  }
	}
#undef PROP_MATCH

      if (family_end)
	{
	  Lisp_Object family;
	  family = font_intern_prop (name, family_end - name, 1);
	  ASET (font, FONT_FAMILY_INDEX, family);
	}
    }

  return 0;
}

/* Store fontconfig's font name of FONT (font-spec or font-entity) in
   NAME (NBYTES length), and return the name length.  If
   FONT_SIZE_INDEX of FONT is 0, use PIXEL_SIZE instead.  */

int
font_unparse_fcname (font, pixel_size, name, nbytes)
     Lisp_Object font;
     int pixel_size;
     char *name;
     int nbytes;
{
  Lisp_Object family, foundry;
  Lisp_Object tail, val;
  int point_size;
  int dpi;
  int i, len = 1;
  char *p;
  Lisp_Object styles[3];
  char *style_names[3] = { "weight", "slant", "width" };
  char work[256];

  family = AREF (font, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      if (SYMBOLP (family))
	{
	  family = SYMBOL_NAME (family);
	  len += SBYTES (family);
	}
      else
	family = Qnil;
    }

  val = AREF (font, FONT_SIZE_INDEX);
  if (INTEGERP (val))
    {
      if (XINT (val) != 0)
	pixel_size = XINT (val);
      point_size = -1;
      len += 21;		/* for ":pixelsize=NUM" */
    }
  else if (FLOATP (val))
    {
      pixel_size = -1;
      point_size = (int) XFLOAT_DATA (val);
      len += 11;		/* for "-NUM" */
    }

  foundry = AREF (font, FONT_FOUNDRY_INDEX);
  if (! NILP (foundry))
    {
      if (SYMBOLP (foundry))
	{
	  foundry = SYMBOL_NAME (foundry);
	  len += 9 + SBYTES (foundry); /* ":foundry=NAME" */
	}
      else
	foundry = Qnil;
    }

  for (i = 0; i < 3; i++)
    {
      styles[i] = font_style_symbolic (font, FONT_WEIGHT_INDEX + i, 0);
      if (! NILP (styles[i]))
	len += sprintf (work, ":%s=%s", style_names[i],
			SDATA (SYMBOL_NAME (styles[i])));
    }

  if (INTEGERP (AREF (font, FONT_DPI_INDEX)))
    len += sprintf (work, ":dpi=%d", dpi);
  if (INTEGERP (AREF (font, FONT_SPACING_INDEX)))
    len += strlen (":spacing=100");
  if (INTEGERP (AREF (font, FONT_AVGWIDTH_INDEX)))
    len += strlen (":scalable=false"); /* or ":scalable=true" */
  for (tail = AREF (font, FONT_EXTRA_INDEX); CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object key = XCAR (XCAR (tail)), val = XCDR (XCAR (tail));

      len += SBYTES (SYMBOL_NAME (key)) + 1; /* for :KEY= */
      if (STRINGP (val))
	len += SBYTES (val);
      else if (INTEGERP (val))
	len += sprintf (work, "%d", XINT (val));
      else if (SYMBOLP (val))
	len += (NILP (val) ? 5 : 4); /* for "false" or "true" */
    }

  if (len > nbytes)
    return -1;
  p = name;
  if (! NILP (family))
    p += sprintf (p, "%s", SDATA (family));
  if (point_size > 0)
    {
      if (p == name)
	p += sprintf (p, "%d", point_size);
      else
	p += sprintf (p, "-%d", point_size);
    }
  else if (pixel_size > 0)
    p += sprintf (p, ":pixelsize=%d", pixel_size);
  if (! NILP (AREF (font, FONT_FOUNDRY_INDEX)))
    p += sprintf (p, ":foundry=%s",
		  SDATA (SYMBOL_NAME (AREF (font, FONT_FOUNDRY_INDEX))));
  for (i = 0; i < 3; i++)
    if (! NILP (styles[i]))
      p += sprintf (p, ":%s=%s", style_names[i],
		    SDATA (SYMBOL_NAME (styles[i])));
  if (INTEGERP (AREF (font, FONT_DPI_INDEX)))
    p += sprintf (p, ":dpi=%d", XINT (AREF (font, FONT_DPI_INDEX)));
  if (INTEGERP (AREF (font, FONT_SPACING_INDEX)))
    p += sprintf (p, ":spacing=%d", XINT (AREF (font, FONT_SPACING_INDEX)));
  if (INTEGERP (AREF (font, FONT_AVGWIDTH_INDEX)))
    {
      if (XINT (AREF (font, FONT_AVGWIDTH_INDEX)) == 0)
	p += sprintf (p, ":scalable=true");
      else
	p += sprintf (p, ":scalable=false");
    }
  return (p - name);
}

/* Store GTK-style font name of FONT (font-spec or font-entity) in
   NAME (NBYTES length), and return the name length.  F is the frame
   on which the font is displayed; it is used to calculate the point
   size.  */

int
font_unparse_gtkname (font, f, name, nbytes)
     Lisp_Object font;
     struct frame *f;
     char *name;
     int nbytes;
{
  char *p;
  int len = 1;
  Lisp_Object family, weight, slant, size;
  int point_size = -1;

  family = AREF (font, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      if (! SYMBOLP (family))
	return -1;
      family = SYMBOL_NAME (family);
      len += SBYTES (family);
    }

  weight = font_style_symbolic (font, FONT_WEIGHT_INDEX, 0);
  if (EQ (weight, Qnormal))
    weight = Qnil;
  else if (! NILP (weight))
    {
      weight = SYMBOL_NAME (weight);
      len += SBYTES (weight);
    }

  slant = font_style_symbolic (font, FONT_SLANT_INDEX, 0);
  if (EQ (slant, Qnormal))
    slant = Qnil;
  else if (! NILP (slant))
    {
      slant = SYMBOL_NAME (slant);
      len += SBYTES (slant);
    }

  size = AREF (font, FONT_SIZE_INDEX);
  /* Convert pixel size to point size.  */
  if (INTEGERP (size))
    {
      Lisp_Object font_dpi = AREF (font, FONT_DPI_INDEX);
      int dpi = 75;
      if (INTEGERP (font_dpi))
	dpi = XINT (font_dpi);
      else if (f)
	dpi = f->resy;
      point_size = PIXEL_TO_POINT (XINT (size), dpi);
      len += 11;
    }
  else if (FLOATP (size))
    {
      point_size = (int) XFLOAT_DATA (size);
      len += 11;
    }

  if (len > nbytes)
    return -1;

  p = name + sprintf (name, "%s", SDATA (family));

  if (! NILP (weight))
    {
      char *q = p;
      p += sprintf (p, " %s", SDATA (weight));
      q[1] = toupper (q[1]);
    }

  if (! NILP (slant))
    {
      char *q = p;
      p += sprintf (p, " %s", SDATA (slant));
      q[1] = toupper (q[1]);
    }

  if (point_size > 0)
    p += sprintf (p, " %d", point_size);

  return (p - name);
}

/* Parse NAME (null terminated) and store information in FONT
   (font-spec or font-entity).  If NAME is successfully parsed, return
   0.  Otherwise return -1.  */

static int
font_parse_name (name, font)
     char *name;
     Lisp_Object font;
{
  if (name[0] == '-' || index (name, '*'))
    return font_parse_xlfd (name, font);
  return font_parse_fcname (name, font);
}


/* Merge FAMILY and REGISTRY into FONT_SPEC.  FAMILY may have the form
   "FAMILY-FOUNDRY".  REGISTRY may not contain charset-encoding
   part.  */

void
font_parse_family_registry (family, registry, font_spec)
     Lisp_Object family, registry, font_spec;
{
  int len;
  char *p0, *p1;

  if (! NILP (family)
      && NILP (AREF (font_spec, FONT_FAMILY_INDEX)))
    {
      CHECK_STRING (family);
      len = SBYTES (family);
      p0 = (char *) SDATA (family);
      p1 = index (p0, '-');
      if (p1)
	{
	  if ((*p0 != '*' || p1 - p0 > 1)
	      && NILP (AREF (font_spec, FONT_FOUNDRY_INDEX)))
	    Ffont_put (font_spec, QCfoundry, font_intern_prop (p0, p1 - p0, 1));
	  p1++;
	  len -= p1 - p0;
	  Ffont_put (font_spec, QCfamily, font_intern_prop (p1, len, 1));
	}
      else
	ASET (font_spec, FONT_FAMILY_INDEX, Fintern (family, Qnil));
    }
  if (! NILP (registry))
    {
      /* Convert "XXX" and "XXX*" to "XXX*-*".  */
      CHECK_STRING (registry);
      len = SBYTES (registry);
      p0 = (char *) SDATA (registry);
      p1 = index (p0, '-');
      if (! p1)
	{
	  if (SDATA (registry)[len - 1] == '*')
	    registry = concat2 (registry, build_string ("-*"));
	  else
	    registry = concat2 (registry, build_string ("*-*"));
	}
      registry = Fdowncase (registry);
      ASET (font_spec, FONT_REGISTRY_INDEX, Fintern (registry, Qnil));
    }
}


/* This part (through the next ^L) is still experimental and not
   tested much.  We may drastically change codes.  */

/* OTF handler */

#if 0

#define LGSTRING_HEADER_SIZE 6
#define LGSTRING_GLYPH_SIZE 8

static int
check_gstring (gstring)
     Lisp_Object gstring;
{
  Lisp_Object val;
  int i, j;

  CHECK_VECTOR (gstring);
  val = AREF (gstring, 0);
  CHECK_VECTOR (val);
  if (ASIZE (val) < LGSTRING_HEADER_SIZE)
    goto err;
  CHECK_FONT_OBJECT (LGSTRING_FONT (gstring));
  if (!NILP (LGSTRING_SLOT (gstring, LGSTRING_IX_LBEARING)))
    CHECK_NUMBER (LGSTRING_SLOT (gstring, LGSTRING_IX_LBEARING));
  if (!NILP (LGSTRING_SLOT (gstring, LGSTRING_IX_RBEARING)))
    CHECK_NUMBER (LGSTRING_SLOT (gstring, LGSTRING_IX_RBEARING));
  if (!NILP (LGSTRING_SLOT (gstring, LGSTRING_IX_WIDTH)))
    CHECK_NATNUM (LGSTRING_SLOT (gstring, LGSTRING_IX_WIDTH));
  if (!NILP (LGSTRING_SLOT (gstring, LGSTRING_IX_ASCENT)))
    CHECK_NUMBER (LGSTRING_SLOT (gstring, LGSTRING_IX_ASCENT));
  if (!NILP (LGSTRING_SLOT (gstring, LGSTRING_IX_ASCENT)))
    CHECK_NUMBER (LGSTRING_SLOT (gstring, LGSTRING_IX_ASCENT));

  for (i = 0; i < LGSTRING_GLYPH_LEN (gstring); i++)
    {
      val = LGSTRING_GLYPH (gstring, i);
      CHECK_VECTOR (val);
      if (ASIZE (val) < LGSTRING_GLYPH_SIZE)
	goto err;
      if (NILP (AREF (val, LGLYPH_IX_CHAR)))
	break;
      CHECK_NATNUM (AREF (val, LGLYPH_IX_FROM));
      CHECK_NATNUM (AREF (val, LGLYPH_IX_TO));
      CHECK_CHARACTER (AREF (val, LGLYPH_IX_CHAR));
      if (!NILP (AREF (val, LGLYPH_IX_CODE)))
	CHECK_NATNUM (AREF (val, LGLYPH_IX_CODE));
      if (!NILP (AREF (val, LGLYPH_IX_WIDTH)))
	CHECK_NATNUM (AREF (val, LGLYPH_IX_WIDTH));
      if (!NILP (AREF (val, LGLYPH_IX_ADJUSTMENT)))
	{
	  val = AREF (val, LGLYPH_IX_ADJUSTMENT);
	  CHECK_VECTOR (val);
	  if (ASIZE (val) < 3)
	    goto err;
	  for (j = 0; j < 3; j++)
	    CHECK_NUMBER (AREF (val, j));
	}
    }
  return i;
 err:
  error ("Invalid glyph-string format");
  return -1;
}

static void
check_otf_features (otf_features)
     Lisp_Object otf_features;
{
  Lisp_Object val;

  CHECK_CONS (otf_features);
  CHECK_SYMBOL (XCAR (otf_features));
  otf_features = XCDR (otf_features);
  CHECK_CONS (otf_features);
  CHECK_SYMBOL (XCAR (otf_features));
  otf_features = XCDR (otf_features);
  for (val = Fcar (otf_features); ! NILP (val);  val = Fcdr (val))
    {
      CHECK_SYMBOL (Fcar (val));
      if (SBYTES (SYMBOL_NAME (XCAR (val))) > 4)
	error ("Invalid OTF GSUB feature: %s", SYMBOL_NAME (XCAR (val)));
    }
  otf_features = XCDR (otf_features);
  for (val = Fcar (otf_features); ! NILP (val);  val = Fcdr (val))
    {
      CHECK_SYMBOL (Fcar (val));
      if (SBYTES (SYMBOL_NAME (XCAR (val))) > 4)
	error ("Invalid OTF GPOS feature: %s", SYMBOL_NAME (XCAR (val)));
    }
}

#ifdef HAVE_LIBOTF
#include <otf.h>

Lisp_Object otf_list;

static Lisp_Object
otf_tag_symbol (tag)
     OTF_Tag tag;
{
  char name[5];

  OTF_tag_name (tag, name);
  return Fintern (make_unibyte_string (name, 4), Qnil);
}

static OTF *
otf_open (file)
     Lisp_Object file;
{
  Lisp_Object val = Fassoc (file, otf_list);
  OTF *otf;

  if (! NILP (val))
    otf = XSAVE_VALUE (XCDR (val))->pointer;
  else
    {
      otf = STRINGP (file) ? OTF_open ((char *) SDATA (file)) : NULL;
      val = make_save_value (otf, 0);
      otf_list = Fcons (Fcons (file, val), otf_list);
    }
  return otf;
}


/* Return a list describing which scripts/languages FONT supports by
   which GSUB/GPOS features of OpenType tables.  See the comment of
   (struct font_driver).otf_capability.  */

Lisp_Object
font_otf_capability (font)
     struct font *font;
{
  OTF *otf;
  Lisp_Object capability = Fcons (Qnil, Qnil);
  int i;

  otf = otf_open (font->props[FONT_FILE_INDEX]);
  if (! otf)
    return Qnil;
  for (i = 0; i < 2; i++)
    {
      OTF_GSUB_GPOS *gsub_gpos;
      Lisp_Object script_list = Qnil;
      int j;

      if (OTF_get_features (otf, i == 0) < 0)
	continue;
      gsub_gpos = i == 0 ? otf->gsub : otf->gpos;
      for (j = gsub_gpos->ScriptList.ScriptCount - 1; j >= 0; j--)
	{
	  OTF_Script *script = gsub_gpos->ScriptList.Script + j;
	  Lisp_Object langsys_list = Qnil;
	  Lisp_Object script_tag = otf_tag_symbol (script->ScriptTag);
	  int k;

	  for (k = script->LangSysCount; k >= 0; k--)
	    {
	      OTF_LangSys *langsys;
	      Lisp_Object feature_list = Qnil;
	      Lisp_Object langsys_tag;
	      int l;

	      if (k == script->LangSysCount)
		{
		  langsys = &script->DefaultLangSys;
		  langsys_tag = Qnil;
		}
	      else
		{
		  langsys = script->LangSys + k;
		  langsys_tag
		    = otf_tag_symbol (script->LangSysRecord[k].LangSysTag);
		}
	      for (l = langsys->FeatureCount - 1; l >= 0; l--)
		{
		  OTF_Feature *feature
		    = gsub_gpos->FeatureList.Feature + langsys->FeatureIndex[l];
		  Lisp_Object feature_tag
		    = otf_tag_symbol (feature->FeatureTag);

		  feature_list = Fcons (feature_tag, feature_list);
		}
	      langsys_list = Fcons (Fcons (langsys_tag, feature_list),
				    langsys_list);
	    }
	  script_list = Fcons (Fcons (script_tag, langsys_list),
			       script_list);
	}

      if (i == 0)
	XSETCAR (capability, script_list);
      else
	XSETCDR (capability, script_list);
    }

  return capability;
}

/* Parse OTF features in SPEC and write a proper features spec string
   in FEATURES for the call of OTF_drive_gsub/gpos (of libotf).  It is
   assured that the sufficient memory has already allocated for
   FEATURES.  */

static void
generate_otf_features (spec, features)
     Lisp_Object spec;
     char *features;
{
  Lisp_Object val;
  char *p;
  int asterisk;

  p = features;
  *p = '\0';
  for (asterisk = 0; CONSP (spec); spec = XCDR (spec))
    {
      val = XCAR (spec);
      CHECK_SYMBOL (val);
      if (p > features)
	*p++ = ',';
      if (SREF (SYMBOL_NAME (val), 0) == '*')
	{
	  asterisk = 1;
	  *p++ = '*';
	}
      else if (! asterisk)
	{
	  val = SYMBOL_NAME (val);
	  p += sprintf (p, "%s", SDATA (val));
	}
      else
	{
	  val = SYMBOL_NAME (val);
	  p += sprintf (p, "~%s", SDATA (val));
	}
    }
  if (CONSP (spec))
    error ("OTF spec too long");
}

Lisp_Object
font_otf_DeviceTable (device_table)
     OTF_DeviceTable *device_table;
{
  int len = device_table->StartSize - device_table->EndSize + 1;

  return Fcons (make_number (len),
		make_unibyte_string (device_table->DeltaValue, len));
}

Lisp_Object
font_otf_ValueRecord (value_format, value_record)
     int value_format;
     OTF_ValueRecord *value_record;
{
  Lisp_Object val = Fmake_vector (make_number (8), Qnil);

  if (value_format & OTF_XPlacement)
    ASET (val, 0, make_number (value_record->XPlacement));
  if (value_format & OTF_YPlacement)
    ASET (val, 1, make_number (value_record->YPlacement));
  if (value_format & OTF_XAdvance)
    ASET (val, 2, make_number (value_record->XAdvance));
  if (value_format & OTF_YAdvance)
    ASET (val, 3, make_number (value_record->YAdvance));
  if (value_format & OTF_XPlaDevice)
    ASET (val, 4, font_otf_DeviceTable (&value_record->XPlaDevice));
  if (value_format & OTF_YPlaDevice)
    ASET (val, 4, font_otf_DeviceTable (&value_record->YPlaDevice));
  if (value_format & OTF_XAdvDevice)
    ASET (val, 4, font_otf_DeviceTable (&value_record->XAdvDevice));
  if (value_format & OTF_YAdvDevice)
    ASET (val, 4, font_otf_DeviceTable (&value_record->YAdvDevice));
  return val;
}

Lisp_Object
font_otf_Anchor (anchor)
     OTF_Anchor *anchor;
{
  Lisp_Object val;

  val = Fmake_vector (make_number (anchor->AnchorFormat + 1), Qnil);
  ASET (val, 0, make_number (anchor->XCoordinate));
  ASET (val, 1, make_number (anchor->YCoordinate));
  if (anchor->AnchorFormat == 2)
    ASET (val, 2, make_number (anchor->f.f1.AnchorPoint));
  else
    {
      ASET (val, 3, font_otf_DeviceTable (&anchor->f.f2.XDeviceTable));
      ASET (val, 4, font_otf_DeviceTable (&anchor->f.f2.YDeviceTable));
    }
  return val;
}
#endif	/* HAVE_LIBOTF */
#endif	/* 0 */


/* Font sorting */

static unsigned font_score P_ ((Lisp_Object, Lisp_Object *));
static int font_compare P_ ((const void *, const void *));
static Lisp_Object font_sort_entites P_ ((Lisp_Object, Lisp_Object,
					  Lisp_Object, int));

/* We sort fonts by scoring each of them against a specified
   font-spec.  The score value is 32 bit (`unsigned'), and the smaller
   the value is, the closer the font is to the font-spec.

   The lowest 2 bits of the score is used for driver type.  The font
   available by the most preferred font driver is 0.

   Each 7-bit in the higher 28 bits are used for numeric properties
   WEIGHT, SLANT, WIDTH, and SIZE.  */

/* How many bits to shift to store the difference value of each font
   property in a score.  Note that flots for FONT_TYPE_INDEX and
   FONT_REGISTRY_INDEX are not used.  */
static int sort_shift_bits[FONT_SIZE_INDEX + 1];

/* Score font-entity ENTITY against properties of font-spec SPEC_PROP.
   The return value indicates how different ENTITY is compared with
   SPEC_PROP.  */

static unsigned
font_score (entity, spec_prop)
     Lisp_Object entity, *spec_prop;
{
  unsigned score = 0;
  int i;

  /* Score three style numeric fields.  Maximum difference is 127. */
  for (i = FONT_WEIGHT_INDEX; i <= FONT_WIDTH_INDEX; i++)
    if (! NILP (spec_prop[i]) && ! EQ (AREF (entity, i), spec_prop[i]))
      {
	int diff = (XINT (AREF (entity, i)) >> 8) - (XINT (spec_prop[i]) >> 8);

	if (diff < 0)
	  diff = - diff;
	if (diff > 0)
	  score |= min (diff, 127) << sort_shift_bits[i];
      }

  /* Score the size.  Maximum difference is 127.  */
  i = FONT_SIZE_INDEX;
  if (! NILP (spec_prop[i]) && XINT (AREF (entity, i)) > 0)
    {
      /* We use the higher 6-bit for the actual size difference.  The
	 lowest bit is set if the DPI is different.  */
      int diff = XINT (spec_prop[i]) - XINT (AREF (entity, i));

      if (diff < 0)
	diff = - diff;
      diff <<= 1;
      if (! NILP (spec_prop[FONT_DPI_INDEX])
	  && ! EQ (spec_prop[FONT_DPI_INDEX], AREF (entity, FONT_DPI_INDEX)))
	diff |= 1;
      score |= min (diff, 127) << sort_shift_bits[FONT_SIZE_INDEX];
    }

  return score;
}


/* The comparison function for qsort.  */

static int
font_compare (d1, d2)
     const void *d1, *d2;
{
  return (*(unsigned *) d1 - *(unsigned *) d2);
}


/* The structure for elements being sorted by qsort.  */
struct font_sort_data
{
  unsigned score;
  Lisp_Object entity;
};


/* Sort font-entities in vector VEC by closeness to font-spec PREFER.
   If PREFER specifies a point-size, calculate the corresponding
   pixel-size from QCdpi property of PREFER or from the Y-resolution
   of FRAME before sorting.

   If BEST-ONLY is nonzero, return the best matching entity.  Otherwise,
   return the sorted VEC.  */

static Lisp_Object
font_sort_entites (vec, prefer, frame, best_only)
     Lisp_Object vec, prefer, frame;
     int best_only;
{
  Lisp_Object prefer_prop[FONT_SPEC_MAX];
  int len, i;
  struct font_sort_data *data;
  unsigned best_score;
  Lisp_Object best_entity, driver_type;
  int driver_order;
  struct frame *f = XFRAME (frame);
  struct font_driver_list *list;
  USE_SAFE_ALLOCA;

  len = ASIZE (vec);
  if (len <= 1)
    return best_only ? AREF (vec, 0) : vec;

  for (i = FONT_WEIGHT_INDEX; i <= FONT_DPI_INDEX; i++)
    prefer_prop[i] = AREF (prefer, i);
  if (FLOATP (prefer_prop[FONT_SIZE_INDEX]))
    prefer_prop[FONT_SIZE_INDEX]
      = make_number (font_pixel_size (XFRAME (frame), prefer));

  /* Scoring and sorting.  */
  SAFE_ALLOCA (data, struct font_sort_data *, (sizeof *data) * len);
  best_score = 0xFFFFFFFF;
  /* We are sure that the length of VEC > 1.  */
  driver_type = AREF (AREF (vec, 0), FONT_TYPE_INDEX);
  for (driver_order = 0, list = f->font_driver_list; list;
       driver_order++, list = list->next)
    if (EQ (driver_type, list->driver->type))
      break;
  best_entity = data[0].entity = AREF (vec, 0);
  best_score = data[0].score
    = font_score (data[0].entity, prefer_prop) | driver_order;
  for (i = 0; i < len; i++)
    {
      if (!EQ (driver_type, AREF (AREF (vec, i), FONT_TYPE_INDEX)))
	for (driver_order = 0, list = f->font_driver_list; list;
	     driver_order++, list = list->next)
	  if (EQ (driver_type, list->driver->type))
	    break;
      data[i].entity = AREF (vec, i);
      data[i].score = font_score (data[i].entity, prefer_prop) | driver_order;
      if (best_only && best_score > data[i].score)
	{
	  best_score = data[i].score;
	  best_entity = data[i].entity;
	  if (best_score == 0)
	    break;
	}
    }
  if (! best_only)
    {
      qsort (data, len, sizeof *data, font_compare);
      for (i = 0; i < len; i++)
	ASET (vec, i, data[i].entity);
    }
  else
    vec = best_entity;
  SAFE_FREE ();

  font_add_log ("sort-by", prefer, vec);
  return vec;
}


/* API of Font Service Layer.  */

/* Reflect ORDER (see the variable font_sort_order in xfaces.c) to
   sort_shift_bits.  Finternal_set_font_selection_order calls this
   function with font_sort_order after setting up it.  */

void
font_update_sort_order (order)
     int *order;
{
  int i, shift_bits;

  for (i = 0, shift_bits = 23; i < 4; i++, shift_bits -= 7)
    {
      int xlfd_idx = order[i];

      if (xlfd_idx == XLFD_WEIGHT_INDEX)
	sort_shift_bits[FONT_WEIGHT_INDEX] = shift_bits;
      else if (xlfd_idx == XLFD_SLANT_INDEX)
	sort_shift_bits[FONT_SLANT_INDEX] = shift_bits;
      else if (xlfd_idx == XLFD_SWIDTH_INDEX)
	sort_shift_bits[FONT_WIDTH_INDEX] = shift_bits;
      else
	sort_shift_bits[FONT_SIZE_INDEX] = shift_bits;
    }
}

static int
font_check_otf_features (script, langsys, features, table)
     Lisp_Object script, langsys, features, table;
{
  Lisp_Object val;
  int negative;

  table = assq_no_quit (script, table);
  if (NILP (table))
    return 0;
  table = XCDR (table);
  if (! NILP (langsys))
    {
      table = assq_no_quit (langsys, table);
      if (NILP (table))
	return 0;
    }
  else
    {
      val = assq_no_quit (Qnil, table);
      if (NILP (val))
	table = XCAR (table);
      else
	table = val;
    }
  table = XCDR (table);
  for (negative = 0; CONSP (features); features = XCDR (features))
    {
      if (NILP (XCAR (features)))
	negative = 1;
      if (NILP (Fmemq (XCAR (features), table)) != negative)
	return 0;
    }
  return 1;
}

/* Check if OTF_CAPABILITY satisfies SPEC (otf-spec).  */

static int
font_check_otf (Lisp_Object spec, Lisp_Object otf_capability)
{
  Lisp_Object script, langsys = Qnil, gsub = Qnil, gpos = Qnil;

  script = XCAR (spec);
  spec = XCDR (spec);
  if (! NILP (spec))
    {
      langsys = XCAR (spec);
      spec = XCDR (spec);
      if (! NILP (spec))
	{
	  gsub = XCAR (spec);
	  spec = XCDR (spec);
	  if (! NILP (spec))
	    gpos = XCAR (spec);
	}
    }

  if (! NILP (gsub) && ! font_check_otf_features (script, langsys, gsub,
						  XCAR (otf_capability)))
    return 0;
  if (! NILP (gpos) && ! font_check_otf_features (script, langsys, gpos,
						  XCDR (otf_capability)))
    return 0;
  return 1;
}



/* Check if FONT (font-entity or font-object) matches with the font
   specification SPEC.  */

int
font_match_p (spec, font)
     Lisp_Object spec, font;
{
  Lisp_Object prop[FONT_SPEC_MAX], *props;
  Lisp_Object extra, font_extra;
  int i;

  for (i = FONT_FOUNDRY_INDEX; i <= FONT_REGISTRY_INDEX; i++)
    if (! NILP (AREF (spec, i))
	&& ! NILP (AREF (font, i))
	&& ! EQ (AREF (spec, i), AREF (font, i)))
      return 0;
  props = XFONT_SPEC (spec)->props;
  if (FLOATP (props[FONT_SIZE_INDEX]))
    {
      for (i = FONT_FOUNDRY_INDEX; i < FONT_SIZE_INDEX; i++)
	prop[i] = AREF (spec, i);
      prop[FONT_SIZE_INDEX]
	= make_number (font_pixel_size (XFRAME (selected_frame), spec));
      props = prop;
    }

  if (font_score (font, props) > 0)
    return 0;
  extra = AREF (spec, FONT_EXTRA_INDEX);
  font_extra = AREF (font, FONT_EXTRA_INDEX);
  for (; CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key = XCAR (XCAR (extra));
      Lisp_Object val = XCDR (XCAR (extra)), val2;

      if (EQ (key, QClang))
	{
	  val2 = assq_no_quit (key, font_extra);
	  if (NILP (val2))
	    return 0;
	  val2 = XCDR (val2);
	  if (CONSP (val))
	    {
	      if (! CONSP (val2))
		return 0;
	      while (CONSP (val))
		if (NILP (Fmemq (val, val2)))
		  return 0;
	    }
	  else
	    if (CONSP (val2)
		? NILP (Fmemq (val, XCDR (val2)))
		: ! EQ (val, val2))
	      return 0;
	}
      else if (EQ (key, QCscript))
	{
	  val2 = assq_no_quit (val, Vscript_representative_chars);
	  if (! NILP (val2))
	    for (val2 = XCDR (val2); CONSP (val2); val2 = XCDR (val2))
	      if (font_encode_char (font, XINT (XCAR (val2)))
		  == FONT_INVALID_CODE)
		return 0;
	}
      else if (EQ (key, QCotf))
	{
	  struct font *fontp;

	  if (! FONT_OBJECT_P (font))
	    return 0;
	  fontp = XFONT_OBJECT (font);
	  if (! fontp->driver->otf_capability)
	    return 0;
	  val2 = fontp->driver->otf_capability (fontp);
	  if (NILP (val2) || ! font_check_otf (val, val2))
	    return 0;
	}
    }

  return 1;
}


/* Font cache

   Each font backend has the callback function get_cache, and it
   returns a cons cell of which cdr part can be freely used for
   caching fonts.  The cons cell may be shared by multiple frames
   and/or multiple font drivers.  So, we arrange the cdr part as this:

	((DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...) ...)

   where DRIVER-TYPE is a symbol such as `x', `xft', etc., NUM-FRAMES
   is a number frames sharing this cache, and FONT-CACHE-DATA is a
   cons (FONT-SPEC FONT-ENTITY ...).  */

static void font_prepare_cache P_ ((FRAME_PTR, struct font_driver *));
static void font_finish_cache P_ ((FRAME_PTR, struct font_driver *));
static Lisp_Object font_get_cache P_ ((FRAME_PTR, struct font_driver *));
static void font_clear_cache P_ ((FRAME_PTR, Lisp_Object,
				  struct font_driver *));

static void
font_prepare_cache (f, driver)
     FRAME_PTR f;
     struct font_driver *driver;
{
  Lisp_Object cache, val;

  cache = driver->get_cache (f);
  val = XCDR (cache);
  while (CONSP (val) && ! EQ (XCAR (XCAR (val)), driver->type))
    val = XCDR (val);
  if (NILP (val))
    {
      val = Fcons (driver->type, Fcons (make_number (1), Qnil));
      XSETCDR (cache, Fcons (val, XCDR (cache)));
    }
  else
    {
      val = XCDR (XCAR (val));
      XSETCAR (val, make_number (XINT (XCAR (val)) + 1));
    }
}


static void
font_finish_cache (f, driver)
     FRAME_PTR f;
     struct font_driver *driver;
{
  Lisp_Object cache, val, tmp;


  cache = driver->get_cache (f);
  val = XCDR (cache);
  while (CONSP (val) && ! EQ (XCAR (XCAR (val)), driver->type))
    cache = val, val = XCDR (val);
  font_assert (! NILP (val));
  tmp = XCDR (XCAR (val));
  XSETCAR (tmp, make_number (XINT (XCAR (tmp)) - 1));
  if (XINT (XCAR (tmp)) == 0)
    {
      font_clear_cache (f, XCAR (val), driver);
      XSETCDR (cache, XCDR (val));
    }
}


static Lisp_Object
font_get_cache (f, driver)
     FRAME_PTR f;
     struct font_driver *driver;
{
  Lisp_Object val = driver->get_cache (f);
  Lisp_Object type = driver->type;

  font_assert (CONSP (val));
  for (val = XCDR (val); ! EQ (XCAR (XCAR (val)), type); val = XCDR (val));
  font_assert (CONSP (val));
  /* VAL = ((DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...) ...) */
  val = XCDR (XCAR (val));
  return val;
}

static int num_fonts;

static void
font_clear_cache (f, cache, driver)
     FRAME_PTR f;
     Lisp_Object cache;
     struct font_driver *driver;
{
  Lisp_Object tail, elt;

  /* CACHE = (DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...) */
  for (tail = XCDR (XCDR (cache)); CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (elt) && FONT_SPEC_P (XCAR (elt)) && VECTORP (XCDR (elt)))
	{
	  Lisp_Object vec = XCDR (elt);
	  int i;

	  for (i = 0; i < ASIZE (vec); i++)
	    {
	      Lisp_Object entity = AREF (vec, i);

	      if (EQ (driver->type, AREF (entity, FONT_TYPE_INDEX)))
		{
		  Lisp_Object objlist = AREF (entity, FONT_OBJLIST_INDEX);

		  for (; CONSP (objlist); objlist = XCDR (objlist))
		    {
		      Lisp_Object val = XCAR (objlist);
		      struct font *font = XFONT_OBJECT (val);

		      if (! NILP (AREF (val, FONT_TYPE_INDEX)))
			{
			  font_assert (font && driver == font->driver);
			  driver->close (f, font);
			  num_fonts--;
			}
		    }
		  if (driver->free_entity)
		    driver->free_entity (entity);
		}
	    }
	}
    }
  XSETCDR (cache, Qnil);
}


static Lisp_Object scratch_font_spec, scratch_font_prefer;

Lisp_Object
font_delete_unmatched (list, spec, size)
     Lisp_Object list, spec;
     int size;
{
  Lisp_Object entity, val;
  enum font_property_index prop;

  for (val = Qnil; CONSP (list); list = XCDR (list))
    {
      entity = XCAR (list);
      for (prop = FONT_WEIGHT_INDEX; prop < FONT_SIZE_INDEX; prop++)
	if (INTEGERP (AREF (spec, prop))
	    && ((XINT (AREF (spec, prop)) >> 8)
		!= (XINT (AREF (entity, prop)) >> 8)))
	  prop = FONT_SPEC_MAX;
      if (prop < FONT_SPEC_MAX
	  && size
	  && XINT (AREF (entity, FONT_SIZE_INDEX)) > 0)
	{
	  int diff = XINT (AREF (entity, FONT_SIZE_INDEX)) - size;

	  if (diff != 0
	      && (diff < 0 ? -diff > FONT_PIXEL_SIZE_QUANTUM
		  : diff > FONT_PIXEL_SIZE_QUANTUM))
	    prop = FONT_SPEC_MAX;
	}
      if (prop < FONT_SPEC_MAX
	  && INTEGERP (AREF (spec, FONT_DPI_INDEX))
	  && INTEGERP (AREF (entity, FONT_DPI_INDEX))
	  && ! EQ (AREF (spec, FONT_DPI_INDEX), AREF (entity, FONT_DPI_INDEX)))
	prop = FONT_SPEC_MAX;
      if (prop < FONT_SPEC_MAX
	  && INTEGERP (AREF (spec, FONT_AVGWIDTH_INDEX))
	  && INTEGERP (AREF (entity, FONT_AVGWIDTH_INDEX))
	  && ! EQ (AREF (spec, FONT_AVGWIDTH_INDEX),
		   AREF (entity, FONT_AVGWIDTH_INDEX)))
	prop = FONT_SPEC_MAX;
      if (prop < FONT_SPEC_MAX)
	val = Fcons (entity, val);
    }
  return val;
}


/* Return a vector of font-entities matching with SPEC on FRAME.  */

Lisp_Object
font_list_entities (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  struct font_driver_list *driver_list = f->font_driver_list;
  Lisp_Object ftype, val;
  Lisp_Object *vec;
  int size;
  int need_filtering = 0;
  int i;

  font_assert (FONT_SPEC_P (spec));

  if (INTEGERP (AREF (spec, FONT_SIZE_INDEX)))
    size = XINT (AREF (spec, FONT_SIZE_INDEX));
  else if (FLOATP (AREF (spec, FONT_SIZE_INDEX)))
    size = font_pixel_size (f, spec);
  else
    size = 0;

  ftype = AREF (spec, FONT_TYPE_INDEX);
  for (i = FONT_FOUNDRY_INDEX; i <= FONT_REGISTRY_INDEX; i++)
    ASET (scratch_font_spec, i, AREF (spec, i));
  for (i = FONT_WEIGHT_INDEX; i < FONT_EXTRA_INDEX; i++)
    {
      ASET (scratch_font_spec, i, Qnil);
      if (! NILP (AREF (spec, i)))
	need_filtering = 1;
      if (i == FONT_DPI_INDEX)
	/* Skip FONT_SPACING_INDEX  */
	i++;
    }
  ASET (scratch_font_spec, FONT_SPACING_INDEX, AREF (spec, FONT_SPACING_INDEX));
  ASET (scratch_font_spec, FONT_EXTRA_INDEX, AREF (spec, FONT_EXTRA_INDEX));

  vec = alloca (sizeof (Lisp_Object) * num_font_drivers);
  if (! vec)
    return null_vector;

  for (i = 0; driver_list; driver_list = driver_list->next)
    if (driver_list->on
	&& (NILP (ftype) || EQ (driver_list->driver->type, ftype)))
      {
	Lisp_Object cache = font_get_cache (f, driver_list->driver);

	ASET (scratch_font_spec, FONT_TYPE_INDEX, driver_list->driver->type);
	val = assoc_no_quit (scratch_font_spec, XCDR (cache));
	if (CONSP (val))
	  val = XCDR (val);
	else
	  {
	    Lisp_Object copy;

	    val = driver_list->driver->list (frame, scratch_font_spec);
	    copy = Fcopy_font_spec (scratch_font_spec);
	    ASET (copy, FONT_TYPE_INDEX, driver_list->driver->type);
	    XSETCDR (cache, Fcons (Fcons (copy, val), XCDR (cache)));
	  }
	if (! NILP (val) && need_filtering)
	  val = font_delete_unmatched (val, spec, size);
	if (! NILP (val))
	  vec[i++] = val;
      }

  val = (i > 0 ? Fvconcat (i, vec) : null_vector);
  font_add_log ("list", spec, val);
  return (val);
}


/* Return a font entity matching with SPEC on FRAME.  ATTRS, if non
   nil, is an array of face's attributes, which specifies preferred
   font-related attributes.  */

static Lisp_Object
font_matching_entity (f, attrs, spec)
     FRAME_PTR f;
     Lisp_Object *attrs, spec;
{
  struct font_driver_list *driver_list = f->font_driver_list;
  Lisp_Object ftype, size, entity;
  Lisp_Object frame;

  XSETFRAME (frame, f);
  ftype = AREF (spec, FONT_TYPE_INDEX);
  size = AREF (spec, FONT_SIZE_INDEX);
  if (FLOATP (size))
    ASET (spec, FONT_SIZE_INDEX, make_number (font_pixel_size (f, spec)));
  entity = Qnil;
  for (; driver_list; driver_list = driver_list->next)
    if (driver_list->on
	&& (NILP (ftype) || EQ (driver_list->driver->type, ftype)))
      {
	Lisp_Object cache = font_get_cache (f, driver_list->driver);
	Lisp_Object copy;

	ASET (spec, FONT_TYPE_INDEX, driver_list->driver->type);
	entity = assoc_no_quit (spec, XCDR (cache));
	if (CONSP (entity))
	  entity = XCDR (entity);
	else
	  {
	    entity = driver_list->driver->match (frame, spec);
	    copy = Fcopy_font_spec (spec);
	    ASET (copy, FONT_TYPE_INDEX, driver_list->driver->type);
	    XSETCDR (cache, Fcons (Fcons (copy, entity), XCDR (cache)));
	  }
	if (! NILP (entity))
	  break;
      }
  ASET (spec, FONT_TYPE_INDEX, ftype);
  ASET (spec, FONT_SIZE_INDEX, size);
  font_add_log ("match", spec, entity);
  return entity;
}


/* Open a font of ENTITY and PIXEL_SIZE on frame F, and return the
   opened font object.  */

static Lisp_Object
font_open_entity (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  struct font_driver_list *driver_list;
  Lisp_Object objlist, size, val, font_object;
  struct font *font;
  int min_width, height;

  font_assert (FONT_ENTITY_P (entity));
  size = AREF (entity, FONT_SIZE_INDEX);
  if (XINT (size) != 0)
    pixel_size = XINT (size);

  for (objlist = AREF (entity, FONT_OBJLIST_INDEX); CONSP (objlist);
       objlist = XCDR (objlist))
    if (! NILP (AREF (XCAR (objlist), FONT_TYPE_INDEX))
	&& XFONT_OBJECT (XCAR (objlist))->pixel_size == pixel_size)
      return  XCAR (objlist);

  val = AREF (entity, FONT_TYPE_INDEX);
  for (driver_list = f->font_driver_list;
       driver_list && ! EQ (driver_list->driver->type, val);
       driver_list = driver_list->next);
  if (! driver_list)
    return Qnil;

  font_object = driver_list->driver->open (f, entity, pixel_size);
  font_add_log ("open", entity, font_object);
  if (NILP (font_object))
    return Qnil;
  ASET (entity, FONT_OBJLIST_INDEX,
	Fcons (font_object, AREF (entity, FONT_OBJLIST_INDEX)));
  ASET (font_object, FONT_OBJLIST_INDEX, Qnil);
  num_fonts++;

  font = XFONT_OBJECT (font_object);
  min_width = (font->min_width ? font->min_width
	       : font->average_width ? font->average_width
	       : font->space_width ? font->space_width
	       : 1);
  height = (font->height ? font->height : 1);
#ifdef HAVE_WINDOW_SYSTEM
  FRAME_X_DISPLAY_INFO (f)->n_fonts++;
  if (FRAME_X_DISPLAY_INFO (f)->n_fonts == 1)
    {
      FRAME_SMALLEST_CHAR_WIDTH (f) = min_width;
      FRAME_SMALLEST_FONT_HEIGHT (f) = height;
      fonts_changed_p = 1;
    }
  else
    {
      if (FRAME_SMALLEST_CHAR_WIDTH (f) > min_width)
	FRAME_SMALLEST_CHAR_WIDTH (f) = min_width, fonts_changed_p = 1;
      if (FRAME_SMALLEST_FONT_HEIGHT (f) > height)
	FRAME_SMALLEST_FONT_HEIGHT (f) = height, fonts_changed_p = 1;
    }
#endif

  return font_object;
}


/* Close FONT_OBJECT that is opened on frame F.  */

void
font_close_object (f, font_object)
     FRAME_PTR f;
     Lisp_Object font_object;
{
  struct font *font = XFONT_OBJECT (font_object);

  if (NILP (AREF (font_object, FONT_TYPE_INDEX)))
    /* Already closed.  */
    return;
  font_add_log ("close", font_object, Qnil);
  font->driver->close (f, font);
#ifdef HAVE_WINDOW_SYSTEM
  font_assert (FRAME_X_DISPLAY_INFO (f)->n_fonts);
  FRAME_X_DISPLAY_INFO (f)->n_fonts--;
#endif
  num_fonts--;
}


/* Return 1 if FONT on F has a glyph for character C, 0 if not, -1 if
   FONT is a font-entity and it must be opened to check.  */

int
font_has_char (f, font, c)
     FRAME_PTR f;
     Lisp_Object font;
     int c;
{
  struct font *fontp;

  if (FONT_ENTITY_P (font))
    {
      Lisp_Object type = AREF (font, FONT_TYPE_INDEX);
      struct font_driver_list *driver_list;

      for (driver_list = f->font_driver_list;
	   driver_list && ! EQ (driver_list->driver->type, type);
	   driver_list = driver_list->next);
      if (! driver_list)
	return 0;
      if (! driver_list->driver->has_char)
	return -1;
      return driver_list->driver->has_char (font, c);
    }

  font_assert (FONT_OBJECT_P (font));
  fontp = XFONT_OBJECT (font);
  if (fontp->driver->has_char)
    {
      int result = fontp->driver->has_char (font, c);

      if (result >= 0)
	return result;
    }
  return (fontp->driver->encode_char (fontp, c) != FONT_INVALID_CODE);
}


/* Return the glyph ID of FONT_OBJECT for character C.  */

unsigned
font_encode_char (font_object, c)
     Lisp_Object font_object;
     int c;
{
  struct font *font;

  font_assert (FONT_OBJECT_P (font_object));
  font = XFONT_OBJECT (font_object);
  return font->driver->encode_char (font, c);
}


/* Return the name of FONT_OBJECT.  */

Lisp_Object
font_get_name (font_object)
     Lisp_Object font_object;
{
  font_assert (FONT_OBJECT_P (font_object));
  return AREF (font_object, FONT_NAME_INDEX);
}


/* Return the specification of FONT_OBJECT.  */

Lisp_Object
font_get_spec (font_object)
     Lisp_Object font_object;
{
  Lisp_Object spec = font_make_spec ();
  int i;

  for (i = 0; i < FONT_SIZE_INDEX; i++)
    ASET (spec, i, AREF (font_object, i));
  ASET (spec, FONT_SIZE_INDEX,
	make_number (XFONT_OBJECT (font_object)->pixel_size));
  return spec;
}

Lisp_Object
font_spec_from_name (font_name)
     Lisp_Object font_name;
{
  Lisp_Object args[2];

  args[0] = QCname;
  args[1] = font_name;
  return Ffont_spec (2, args);
}


void
font_clear_prop (attrs, prop)
     Lisp_Object *attrs;
     enum font_property_index prop;
{
  Lisp_Object font = attrs[LFACE_FONT_INDEX];

  if (! FONTP (font))
    return;
  if (NILP (AREF (font, prop))
      && prop != FONT_FAMILY_INDEX && prop != FONT_FOUNDRY_INDEX
      && prop != FONT_SIZE_INDEX)
    return;
  font = Fcopy_font_spec (font);
  ASET (font, prop, Qnil);
  if (prop == FONT_FAMILY_INDEX || prop == FONT_FOUNDRY_INDEX)
    {
      if (prop == FONT_FAMILY_INDEX)
	ASET (font, FONT_FOUNDRY_INDEX, Qnil);
      ASET (font, FONT_ADSTYLE_INDEX, Qnil);
      ASET (font, FONT_REGISTRY_INDEX, Qnil);
      ASET (font, FONT_SIZE_INDEX, Qnil);
      ASET (font, FONT_DPI_INDEX, Qnil);
      ASET (font, FONT_SPACING_INDEX, Qnil);
      ASET (font, FONT_AVGWIDTH_INDEX, Qnil);
    }
  else if (prop == FONT_SIZE_INDEX)
    {
      ASET (font, FONT_DPI_INDEX, Qnil);
      ASET (font, FONT_SPACING_INDEX, Qnil);
      ASET (font, FONT_AVGWIDTH_INDEX, Qnil);
    }
  attrs[LFACE_FONT_INDEX] = font;
}

void
font_update_lface (f, attrs)
     FRAME_PTR f;
     Lisp_Object *attrs;
{
  Lisp_Object spec;

  spec = attrs[LFACE_FONT_INDEX];
  if (! FONT_SPEC_P (spec))
    return;

  if (! NILP (AREF (spec, FONT_FOUNDRY_INDEX)))
    attrs[LFACE_FOUNDRY_INDEX] = SYMBOL_NAME (AREF (spec, FONT_FOUNDRY_INDEX));
  if (! NILP (AREF (spec, FONT_FAMILY_INDEX)))
    attrs[LFACE_FAMILY_INDEX] = SYMBOL_NAME (AREF (spec, FONT_FAMILY_INDEX));
  if (! NILP (AREF (spec, FONT_WEIGHT_INDEX)))
    attrs[LFACE_WEIGHT_INDEX] = FONT_WEIGHT_FOR_FACE (spec);
  if (! NILP (AREF (spec, FONT_SLANT_INDEX)))
    attrs[LFACE_SLANT_INDEX] = FONT_SLANT_FOR_FACE (spec);;
  if (! NILP (AREF (spec, FONT_WIDTH_INDEX)))
    attrs[LFACE_SWIDTH_INDEX] = FONT_WIDTH_FOR_FACE (spec);
  if (! NILP (AREF (spec, FONT_SIZE_INDEX)))
    {
      int point;

      if (INTEGERP (AREF (spec, FONT_SIZE_INDEX)))
	{
	  Lisp_Object val;
	  int dpi = f->resy;

	  val = Ffont_get (spec, QCdpi);
	  if (! NILP (val))
	    dpi = XINT (val);
	  point = PIXEL_TO_POINT (XINT (AREF (spec, FONT_SIZE_INDEX)) * 10,
				  dpi);
	}
      else if (FLOATP (AREF (spec, FONT_SIZE_INDEX)))
	point = XFLOAT_DATA (AREF (spec, FONT_SIZE_INDEX)) * 10;
      attrs[LFACE_HEIGHT_INDEX] = make_number (point);
    }
}


/* Return a font-entity satisfying SPEC and best matching with face's
   font related attributes in ATTRS.  C, if not negative, is a
   character that the entity must support.  */

Lisp_Object
font_find_for_lface (f, attrs, spec, c)
     FRAME_PTR f;
     Lisp_Object *attrs;
     Lisp_Object spec;
     int c;
{
  Lisp_Object work;
  Lisp_Object frame, entities, val, props[FONT_REGISTRY_INDEX + 1] ;
  Lisp_Object size, foundry[3], *family, registry[3], adstyle[3];
  int pixel_size;
  int i, j, k, l, result;

  registry[0] = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry[0]))
    {
      registry[0] = DEFAULT_ENCODING;
      registry[1] = Qascii_0;
      registry[2] = null_vector;
    }
  else
    registry[1] = null_vector;

  if (c >= 0 && ! NILP (AREF (spec, FONT_REGISTRY_INDEX)))
    {
      struct charset *encoding, *repertory;

      if (font_registry_charsets (AREF (spec, FONT_REGISTRY_INDEX),
				  &encoding, &repertory) < 0)
	return Qnil;
      if (repertory)
	{
	  if (ENCODE_CHAR (repertory, c) == CHARSET_INVALID_CODE (repertory))
	    return Qnil;
	  /* Any font of this registry support C.  So, let's
	     suppress the further checking.  */
	  c = -1;
	}
      else if (c > encoding->max_char)
	return Qnil;
    }

  work = Fcopy_font_spec (spec);
  XSETFRAME (frame, f);
  size = AREF (spec, FONT_SIZE_INDEX);
  pixel_size = font_pixel_size (f, spec);
  if (pixel_size == 0)
    {
      double pt = XINT (attrs[LFACE_HEIGHT_INDEX]);

      pixel_size = POINT_TO_PIXEL (pt / 10, f->resy);
    }
  ASET (work, FONT_SIZE_INDEX, Qnil);
  foundry[0] = AREF (work, FONT_FOUNDRY_INDEX);
  if (! NILP (foundry[0]))
    foundry[1] = null_vector;
  else if (STRINGP (attrs[LFACE_FOUNDRY_INDEX]))
    {
      val = attrs[LFACE_FOUNDRY_INDEX];
      foundry[0] = font_intern_prop ((char *) SDATA (val), SBYTES (val), 1);
      foundry[1] = Qnil;
      foundry[2] = null_vector;
    }
  else
    foundry[0] = Qnil, foundry[1] = null_vector;

  adstyle[0] = AREF (work, FONT_ADSTYLE_INDEX);
  if (! NILP (adstyle[0]))
    adstyle[1] = null_vector;
  else if (FONTP (attrs[LFACE_FONT_INDEX]))
    {
      Lisp_Object face_font = attrs[LFACE_FONT_INDEX];

      if (! NILP (AREF (face_font, FONT_ADSTYLE_INDEX)))
	{
	  adstyle[0] = AREF (face_font, FONT_ADSTYLE_INDEX);
	  adstyle[1] = Qnil;
	  adstyle[2] = null_vector;
	}
      else
	adstyle[0] = Qnil, adstyle[1] = null_vector;
    }
  else
    adstyle[0] = Qnil, adstyle[1] = null_vector;


  val = AREF (work, FONT_FAMILY_INDEX);
  if (NILP (val) && STRINGP (attrs[LFACE_FAMILY_INDEX]))
    {
      val = attrs[LFACE_FAMILY_INDEX];
      val = font_intern_prop ((char *) SDATA (val), SBYTES (val), 1);
    }
  if (NILP (val))
    {
      family = alloca ((sizeof family[0]) * 2);
      family[0] = Qnil;
      family[1] = null_vector;	/* terminator.  */
    }
  else
    {
      Lisp_Object alters
	= Fassoc_string (val, Vface_alternative_font_family_alist, Qt);

      if (! NILP (alters))
	{
	  family = alloca ((sizeof family[0]) * (XINT (Flength (alters)) + 2));
	  for (i = 0; CONSP (alters); i++, alters = XCDR (alters))
	    family[i] = XCAR (alters);
	  if (NILP (AREF (spec, FONT_FAMILY_INDEX)))
	    family[i++] = Qnil;
	  family[i] = null_vector;
	}
      else
	{
	  family = alloca ((sizeof family[0]) * 3);
	  i = 0;
	  family[i++] = val;
	  if (NILP (AREF (spec, FONT_FAMILY_INDEX)))
	    family[i++] = Qnil;
	  family[i] = null_vector;
	}
    }

  for (i = 0; SYMBOLP (family[i]); i++)
    {
      ASET (work, FONT_FAMILY_INDEX, family[i]);
      for (j = 0; SYMBOLP (foundry[j]); j++)
	{
	  ASET (work, FONT_FOUNDRY_INDEX, foundry[j]);
	  for (k = 0; SYMBOLP (registry[k]); k++)
	    {
	      ASET (work, FONT_REGISTRY_INDEX, registry[k]);
	      for (l = 0; SYMBOLP (adstyle[l]); l++)
		{
		  ASET (work, FONT_ADSTYLE_INDEX, adstyle[l]);
		  entities = font_list_entities (frame, work);
		  if (ASIZE (entities) > 0)
		    goto found;
		}
	    }
	}
    }
  return Qnil;
 found:
  if (ASIZE (entities) == 1)
    {
      if (c < 0)
	return AREF (entities, 0);
    }
  else
    {
      /* Sort fonts by properties specified in LFACE.  */
      Lisp_Object prefer = scratch_font_prefer;

      for (i = 0; i < FONT_EXTRA_INDEX; i++)
	ASET (prefer, i, AREF (work, i));
      if (FONTP (attrs[LFACE_FONT_INDEX]))
	{
	  Lisp_Object face_font = attrs[LFACE_FONT_INDEX];

	  for (i = 0; i < FONT_EXTRA_INDEX; i++)
	    if (NILP (AREF (prefer, i)))
	      ASET (prefer, i, AREF (face_font, i));
	}
      if (NILP (AREF (prefer, FONT_WEIGHT_INDEX)))
	FONT_SET_STYLE (prefer, FONT_WEIGHT_INDEX, attrs[LFACE_WEIGHT_INDEX]);
      if (NILP (AREF (prefer, FONT_SLANT_INDEX)))
	FONT_SET_STYLE (prefer, FONT_SLANT_INDEX, attrs[LFACE_SLANT_INDEX]);
      if (NILP (AREF (prefer, FONT_WIDTH_INDEX)))
	FONT_SET_STYLE (prefer, FONT_WIDTH_INDEX, attrs[LFACE_SWIDTH_INDEX]);
      ASET (prefer, FONT_SIZE_INDEX, make_number (pixel_size));
      entities = font_sort_entites (entities, prefer, frame, c < 0);
    }
  if (c < 0)
    return entities;

  for (i = 0; i < ASIZE (entities); i++)
    {
      int j;

      val = AREF (entities, i);
      if (i > 0)
	{
	  for (j = FONT_FOUNDRY_INDEX; j <= FONT_REGISTRY_INDEX; j++)
	    if (! EQ (AREF (val, j), props[j]))
	      break;
	  if (j > FONT_REGISTRY_INDEX)
	    continue;
	}
      for (j = FONT_FOUNDRY_INDEX; j <= FONT_REGISTRY_INDEX; j++)
	props[j] = AREF (val, j);
      result = font_has_char (f, val, c);
      if (result > 0)
	return val;
      if (result == 0)
	return Qnil;
      val = font_open_for_lface (f, val, attrs, spec);
      if (NILP (val))
	continue;
      result = font_has_char (f, val, c);
      font_close_object (f, val);
      if (result > 0)
	return AREF (entities, i);
    }
  return Qnil;
}


Lisp_Object
font_open_for_lface (f, entity, attrs, spec)
     FRAME_PTR f;
     Lisp_Object entity;
     Lisp_Object *attrs;
     Lisp_Object spec;
{
  int size;

  if (INTEGERP (AREF (entity, FONT_SIZE_INDEX))
      && XINT (AREF (entity, FONT_SIZE_INDEX)) > 0)
    size = XINT (AREF (entity, FONT_SIZE_INDEX));
  else if (FONT_SPEC_P (spec) && ! NILP (AREF (spec, FONT_SIZE_INDEX)))
    size = font_pixel_size (f, spec);
  else
    {
      double pt = XINT (attrs[LFACE_HEIGHT_INDEX]);

      pt /= 10;
      size = POINT_TO_PIXEL (pt, f->resy);
#ifdef HAVE_NS
      if (size == 0)
        {
          Lisp_Object ffsize = get_frame_param(f, Qfontsize);
          size = NUMBERP (ffsize) ? POINT_TO_PIXEL (XINT (ffsize), f->resy) : 0;
        }
#endif
    }
  return font_open_entity (f, entity, size);
}


/* Find a font satisfying SPEC and best matching with face's
   attributes in ATTRS on FRAME, and return the opened
   font-object.  */

Lisp_Object
font_load_for_lface (f, attrs, spec)
     FRAME_PTR f;
     Lisp_Object *attrs, spec;
{
  Lisp_Object entity;

  entity = font_find_for_lface (f, attrs, spec, -1);
  if (NILP (entity))
    {
      /* No font is listed for SPEC, but each font-backend may have
	 the different criteria about "font matching".  So, try
	 it.  */
      entity = font_matching_entity (f, attrs, spec);
      if (NILP (entity))
	return Qnil;
    }
  return font_open_for_lface (f, entity, attrs, spec);
}


/* Make FACE on frame F ready to use the font opened for FACE.  */

void
font_prepare_for_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  if (face->font->driver->prepare_face)
    face->font->driver->prepare_face (f, face);
}


/* Make FACE on frame F stop using the font opened for FACE.  */

void
font_done_for_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  if (face->font->driver->done_face)
    face->font->driver->done_face (f, face);
  face->extra = NULL;
}


/* Open a font best matching with NAME on frame F.  If no proper font
   is found, return Qnil.  */

Lisp_Object
font_open_by_name (f, name)
     FRAME_PTR f;
     char *name;
{
  Lisp_Object args[2];
  Lisp_Object spec, attrs[LFACE_VECTOR_SIZE];

  args[0] = QCname;
  args[1] = make_unibyte_string (name, strlen (name));
  spec = Ffont_spec (2, args);
  /* We set up the default font-related attributes of a face to prefer
     a moderate font.  */
  attrs[LFACE_FAMILY_INDEX] = attrs[LFACE_FOUNDRY_INDEX] = Qnil;
  attrs[LFACE_SWIDTH_INDEX] = attrs[LFACE_WEIGHT_INDEX]
    = attrs[LFACE_SLANT_INDEX] = Qnormal;
#ifndef HAVE_NS
  attrs[LFACE_HEIGHT_INDEX] = make_number (120);
#else
  attrs[LFACE_HEIGHT_INDEX] = make_number (0);
#endif
  attrs[LFACE_FONT_INDEX] = Qnil;

  return font_load_for_lface (f, attrs, spec);
}


/* Register font-driver DRIVER.  This function is used in two ways.

   The first is with frame F non-NULL.  In this case, make DRIVER
   available (but not yet activated) on F.  All frame creaters
   (e.g. Fx_create_frame) must call this function at least once with
   an available font-driver.

   The second is with frame F NULL.  In this case, DRIVER is globally
   registered in the variable `font_driver_list'.  All font-driver
   implementations must call this function in its syms_of_XXXX
   (e.g. syms_of_xfont).  */

void
register_font_driver (driver, f)
     struct font_driver *driver;
     FRAME_PTR f;
{
  struct font_driver_list *root = f ? f->font_driver_list : font_driver_list;
  struct font_driver_list *prev, *list;

  if (f && ! driver->draw)
    error ("Unusable font driver for a frame: %s",
	   SDATA (SYMBOL_NAME (driver->type)));

  for (prev = NULL, list = root; list; prev = list, list = list->next)
    if (EQ (list->driver->type, driver->type))
      error ("Duplicated font driver: %s", SDATA (SYMBOL_NAME (driver->type)));

  list = malloc (sizeof (struct font_driver_list));
  list->on = 0;
  list->driver = driver;
  list->next = NULL;
  if (prev)
    prev->next = list;
  else if (f)
    f->font_driver_list = list;
  else
    font_driver_list = list;
  if (! f)
    num_font_drivers++;
}


/* Make the frame F use font backends listed in NEW_DRIVERS (list of
   symbols, e.g. xft, x).  If NEW_DRIVERS is t, make F use all
   available font drivers.  If NEW_DRIVERS is nil, finalize all drivers.

   A caller must free all realized faces if any in advance.  The
   return value is a list of font backends actually made used on
   F.  */

Lisp_Object
font_update_drivers (f, new_drivers)
     FRAME_PTR f;
     Lisp_Object new_drivers;
{
  Lisp_Object active_drivers = Qnil;
  struct font_driver *driver;
  struct font_driver_list *list;

  /* At first, turn off non-requested drivers, and turn on requested
     drivers.  */
  for (list = f->font_driver_list; list; list = list->next)
    {
      driver = list->driver;
      if ((EQ (new_drivers, Qt) || ! NILP (Fmemq (driver->type, new_drivers)))
	  != list->on)
	{
	  if (list->on)
	    {
	      if (driver->end_for_frame)
		driver->end_for_frame (f);
	      font_finish_cache (f, driver);
	      list->on = 0;
	    }
	  else
	    {
	      if (! driver->start_for_frame
		  || driver->start_for_frame (f) == 0)
		{
		  font_prepare_cache (f, driver);
		  list->on = 1;
		}
	    }
	}
    }

  if (NILP (new_drivers))
    return Qnil;

  if (! EQ (new_drivers, Qt))
    {
      /* Re-order the driver list according to new_drivers.  */
      struct font_driver_list **list_table, **next;
      Lisp_Object tail;
      int i;

      list_table = alloca (sizeof list_table[0] * (num_font_drivers + 1));
      for (i = 0, tail = new_drivers; ! NILP (tail); tail = XCDR (tail))
	{
	  for (list = f->font_driver_list; list; list = list->next)
	    if (list->on && EQ (list->driver->type, XCAR (tail)))
	      break;
	  if (list)
	    list_table[i++] = list;
	}
      for (list = f->font_driver_list; list; list = list->next)
	if (! list->on)
	  list_table[i] = list;
      list_table[i] = NULL;

      next = &f->font_driver_list;
      for (i = 0; list_table[i]; i++)
	{
	  *next = list_table[i];
	  next = &(*next)->next;
	}
      *next = NULL;
    }

  for (list = f->font_driver_list; list; list = list->next)
    if (list->on)
      active_drivers = nconc2 (active_drivers,
			       Fcons (list->driver->type, Qnil));
  return active_drivers;
}

int
font_put_frame_data (f, driver, data)
     FRAME_PTR f;
     struct font_driver *driver;
     void *data;
{
  struct font_data_list *list, *prev;

  for (prev = NULL, list = f->font_data_list; list;
       prev = list, list = list->next)
    if (list->driver == driver)
      break;
  if (! data)
    {
      if (list)
	{
	  if (prev)
	    prev->next = list->next;
	  else
	    f->font_data_list = list->next;
	  free (list);
	}
      return 0;
    }

  if (! list)
    {
      list = malloc (sizeof (struct font_data_list));
      if (! list)
	return -1;
      list->driver = driver;
      list->next = f->font_data_list;
      f->font_data_list = list;
    }
  list->data = data;
  return 0;
}


void *
font_get_frame_data (f, driver)
     FRAME_PTR f;
     struct font_driver *driver;
{
  struct font_data_list *list;

  for (list = f->font_data_list; list; list = list->next)
    if (list->driver == driver)
      break;
  if (! list)
    return NULL;
  return list->data;
}


/* Return the font used to draw character C by FACE at buffer position
   POS in window W.  If STRING is non-nil, it is a string containing C
   at index POS.  If C is negative, get C from the current buffer or
   STRING.  */

Lisp_Object
font_at (c, pos, face, w, string)
     int c;
     EMACS_INT pos;
     struct face *face;
     struct window *w;
     Lisp_Object string;
{
  FRAME_PTR f;
  int multibyte;
  Lisp_Object font_object;

  if (c < 0)
    {
      if (NILP (string))
	{
	  multibyte = ! NILP (current_buffer->enable_multibyte_characters);
	  if (multibyte)
	    {
	      EMACS_INT pos_byte = CHAR_TO_BYTE (pos);

	      c = FETCH_CHAR (pos_byte);
	    }
	  else
	    c = FETCH_BYTE (pos);
	}
      else
	{
	  unsigned char *str;

	  multibyte = STRING_MULTIBYTE (string);
	  if (multibyte)
	    {
	      EMACS_INT pos_byte = string_char_to_byte (string, pos);

	      str = SDATA (string) + pos_byte;
	      c = STRING_CHAR (str, 0);
	    }
	  else
	    c = SDATA (string)[pos];
	}
    }

  f = XFRAME (w->frame);
  if (! FRAME_WINDOW_P (f))
    return Qnil;
  if (! face)
    {
      int face_id;
      EMACS_INT endptr;

      if (STRINGP (string))
	face_id = face_at_string_position (w, string, pos, 0, -1, -1, &endptr,
					   DEFAULT_FACE_ID, 0);
      else
	face_id = face_at_buffer_position (w, pos, -1, -1, &endptr,
					   pos + 100, 0);
      face = FACE_FROM_ID (f, face_id);
    }
  if (multibyte)
    {
      int face_id = FACE_FOR_CHAR (f, face, c, pos, string);
      face = FACE_FROM_ID (f, face_id);
    }
  if (! face->font)
    return Qnil;

  XSETFONT (font_object, face->font);
  return font_object;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Check how many characters after POS (at most to *LIMIT) can be
   displayed by the same font on the window W.  FACE, if non-NULL, is
   the face selected for the character at POS.  If STRING is not nil,
   it is the string to check instead of the current buffer.  In that
   case, FACE must be not NULL.

   The return value is the font-object for the character at POS.
   *LIMIT is set to the position where that font can't be used.

   It is assured that the current buffer (or STRING) is multibyte.  */

Lisp_Object
font_range (pos, limit, w, face, string)
     EMACS_INT pos, *limit;
     struct window *w;
     struct face *face;
     Lisp_Object string;
{
  EMACS_INT pos_byte, ignore, start, start_byte;
  int c;
  Lisp_Object font_object = Qnil;

  if (NILP (string))
    {
      pos_byte = CHAR_TO_BYTE (pos);
      if (! face)
	{
	  int face_id;

	  face_id = face_at_buffer_position (w, pos, 0, 0, &ignore, *limit, 0);
	  face = FACE_FROM_ID (XFRAME (w->frame), face_id);
	}
    }
  else
    {
      font_assert (face);
      pos_byte = string_char_to_byte (string, pos);
    }

  start = pos, start_byte = pos_byte;
  while (pos < *limit)
    {
      Lisp_Object category;

      if (NILP (string))
	FETCH_CHAR_ADVANCE_NO_CHECK (c, pos, pos_byte);
      else
	FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string, pos, pos_byte);
      if (NILP (font_object))
	{
	  font_object = font_for_char (face, c, pos - 1, string);
	  if (NILP (font_object))
	    return Qnil;
	  continue;
	}

      category = CHAR_TABLE_REF (Vunicode_category_table, c);
      if (! EQ (category, QCf)
	  && font_encode_char (font_object, c) == FONT_INVALID_CODE)
	{
	  Lisp_Object f = font_for_char (face, c, pos - 1, string);
	  EMACS_INT i, i_byte;


	  if (NILP (f))
	    {
	      *limit = pos - 1;
	      return font_object;
	    }
	  i = start, i_byte = start_byte;
	  while (i < pos - 1)
	    {

	      if (NILP (string))
		FETCH_CHAR_ADVANCE_NO_CHECK (c, i, i_byte);
	      else
		FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string, i, i_byte);
	      category = CHAR_TABLE_REF (Vunicode_category_table, c);
	      if (! EQ (category, QCf)
		  && font_encode_char (f, c) == FONT_INVALID_CODE)
		{
		  *limit = pos - 1;
		  return font_object;
		}
	    }
	  font_object = f;
	}
    }
  return font_object;
}
#endif


/* Lisp API */

DEFUN ("fontp", Ffontp, Sfontp, 1, 2, 0,
       doc: /* Return t if OBJECT is a font-spec, font-entity, or font-object.
Return nil otherwise.
Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
which kind of font it is.  It must be one of `font-spec', `font-entity',
`font-object'.  */)
     (object, extra_type)
     Lisp_Object object, extra_type;
{
  if (NILP (extra_type))
    return (FONTP (object) ? Qt : Qnil);
  if (EQ (extra_type, Qfont_spec))
    return (FONT_SPEC_P (object) ? Qt : Qnil);
  if (EQ (extra_type, Qfont_entity))
    return (FONT_ENTITY_P (object) ? Qt : Qnil);
  if (EQ (extra_type, Qfont_object))
    return (FONT_OBJECT_P (object) ? Qt : Qnil);
  wrong_type_argument (intern ("font-extra-type"), extra_type);
}

DEFUN ("font-spec", Ffont_spec, Sfont_spec, 0, MANY, 0,
       doc: /* Return a newly created font-spec with arguments as properties.

ARGS must come in pairs KEY VALUE of font properties.  KEY must be a
valid font property name listed below:

`:family', `:weight', `:slant', `:width'

They are the same as face attributes of the same name.  See
`set-face-attribute'.

`:foundry'

VALUE must be a string or a symbol specifying the font foundry, e.g. ``misc''.

`:adstyle'

VALUE must be a string or a symbol specifying the additional
typographic style information of a font, e.g. ``sans''.

`:registry'

VALUE must be a string or a symbol specifying the charset registry and
encoding of a font, e.g. ``iso8859-1''.

`:size'

VALUE must be a non-negative integer or a floating point number
specifying the font size.  It specifies the font size in pixels
(if VALUE is an integer), or in points (if VALUE is a float).

`:name'

VALUE must be a string of XLFD-style or fontconfig-style font name.
usage: (font-spec ARGS ...)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object spec = font_make_spec ();
  int i;

  for (i = 0; i < nargs; i += 2)
    {
      Lisp_Object key = args[i], val = args[i + 1];

      if (EQ (key, QCname))
	{
	  CHECK_STRING (val);
	  font_parse_name ((char *) SDATA (val), spec);
	  font_put_extra (spec, key, val);
	}
      else
	{
	  int idx = get_font_prop_index (key);

	  if (idx >= 0)
	    {
	      val = font_prop_validate (idx, Qnil, val);
	      if (idx < FONT_EXTRA_INDEX)
		ASET (spec, idx, val);
	      else
		font_put_extra (spec, key, val);
	    }
	  else
	    font_put_extra (spec, key, font_prop_validate (0, key, val));
	}
    }
  return spec;
}

DEFUN ("copy-font-spec", Fcopy_font_spec, Scopy_font_spec, 1, 1, 0,
       doc: /* Return a copy of FONT as a font-spec.  */)
     (font)
     Lisp_Object font;
{
  Lisp_Object new_spec, tail, prev, extra;
  int i;

  CHECK_FONT (font);
  new_spec = font_make_spec ();
  for (i = 1; i < FONT_EXTRA_INDEX; i++)
    ASET (new_spec, i, AREF (font, i));
  extra = Fcopy_sequence (AREF (font, FONT_EXTRA_INDEX));
  /* We must remove :font-entity property.  */
  for (prev = Qnil, tail = extra; CONSP (tail); prev = tail, tail = XCDR (tail))
    if (EQ (XCAR (XCAR (tail)), QCfont_entity))
      {
	if (NILP (prev))
	  extra = XCDR (extra);
	else
	  XSETCDR (prev, XCDR (tail));
	break;
      }
  ASET (new_spec, FONT_EXTRA_INDEX, extra);
  return new_spec;
}

DEFUN ("merge-font-spec", Fmerge_font_spec, Smerge_font_spec, 2, 2, 0,
       doc: /* Merge font-specs FROM and TO, and return a new font-spec.
Every specified properties in FROM override the corresponding
properties in TO.  */)
     (from, to)
     Lisp_Object from, to;
{
  Lisp_Object extra, tail;
  int i;

  CHECK_FONT (from);
  CHECK_FONT (to);
  to = Fcopy_font_spec (to);
  for (i = 0; i < FONT_EXTRA_INDEX; i++)
    ASET (to, i, AREF (from, i));
  extra = AREF (to, FONT_EXTRA_INDEX);
  for (tail = AREF (from, FONT_EXTRA_INDEX); CONSP (tail); tail = XCDR (tail))
    if (! EQ (XCAR (XCAR (tail)), Qfont_entity))
      {
	Lisp_Object slot = assq_no_quit (XCAR (XCAR (tail)), extra);

	if (! NILP (slot))
	  XSETCDR (slot, XCDR (XCAR (tail)));
	else
	  extra = Fcons (Fcons (XCAR (XCAR (tail)), XCDR (XCAR (tail))), extra);
      }
  ASET (to, FONT_EXTRA_INDEX, extra);
  return to;
}

DEFUN ("font-get", Ffont_get, Sfont_get, 2, 2, 0,
       doc: /* Return the value of FONT's property KEY.
FONT is a font-spec, a font-entity, or a font-object.  */)
     (font, key)
     Lisp_Object font, key;
{
  int idx;

  CHECK_FONT (font);
  CHECK_SYMBOL (key);

  idx = get_font_prop_index (key);
  if (idx >= FONT_WEIGHT_INDEX && idx <= FONT_WIDTH_INDEX)
    return font_style_symbolic (font, idx, 0);
  if (idx >= 0 && idx < FONT_EXTRA_INDEX)
    return AREF (font, idx);
  return Fcdr (Fassq (key, AREF (font, FONT_EXTRA_INDEX)));
}

#ifdef HAVE_WINDOW_SYSTEM

DEFUN ("font-face-attributes", Ffont_face_attributes, Sfont_face_attributes, 1, 2, 0,
       doc: /* Return a plist of face attributes generated by FONT.
FONT is a font name, a font-spec, a font-entity, or a font-object.
The return value is a list of the form

\(:family FAMILY :height HEIGHT :weight WEIGHT :slant SLANT :width WIDTH)

where FAMILY, HEIGHT, WEIGHT, SLANT, and WIDTH are face attribute values
compatible with `set-face-attribute'.  Some of these key-attribute pairs
may be omitted from the list if they are not specified by FONT.

The optional argument FRAME specifies the frame that the face attributes
are to be displayed on.  If omitted, the selected frame is used.  */)
     (font, frame)
     Lisp_Object font, frame;
{
  struct frame *f;
  Lisp_Object plist[10];
  Lisp_Object val;
  int n = 0;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (STRINGP (font))
    {
      int fontset = fs_query_fontset (font, 0);
      Lisp_Object name = font;
      if (fontset >= 0)
	font = fontset_ascii (fontset);
      font = font_spec_from_name (name);
      if (! FONTP (font))
	signal_error ("Invalid font name", name);
    }
  else if (! FONTP (font))
    signal_error ("Invalid font object", font);

  val = AREF (font, FONT_FAMILY_INDEX);
  if (! NILP (val))
    {
      plist[n++] = QCfamily;
      plist[n++] = SYMBOL_NAME (val);
    }

  val = AREF (font, FONT_SIZE_INDEX);
  if (INTEGERP (val))
    {
      Lisp_Object font_dpi = AREF (font, FONT_DPI_INDEX);
      int dpi = INTEGERP (font_dpi) ? XINT (font_dpi) : f->resy;
      plist[n++] = QCheight;
      plist[n++] = make_number (PIXEL_TO_POINT (XINT (val) * 10, dpi));
    }
  else if (FLOATP (val))
    {
      plist[n++] = QCheight;
      plist[n++] = make_number (10 * (int) XFLOAT_DATA (val));
    }

  val = FONT_WEIGHT_FOR_FACE (font);
  if (! NILP (val))
    {
      plist[n++] = QCweight;
      plist[n++] = val;
    }

  val = FONT_SLANT_FOR_FACE (font);
  if (! NILP (val))
    {
      plist[n++] = QCslant;
      plist[n++] = val;
    }

  val = FONT_WIDTH_FOR_FACE (font);
  if (! NILP (val))
    {
      plist[n++] = QCwidth;
      plist[n++] = val;
    }

  return Flist (n, plist);
}

#endif

DEFUN ("font-put", Ffont_put, Sfont_put, 3, 3, 0,
       doc: /* Set one property of FONT-SPEC: give property PROP value VAL.  */)
     (font_spec, prop, val)
     Lisp_Object font_spec, prop, val;
{
  int idx;

  CHECK_FONT_SPEC (font_spec);
  idx = get_font_prop_index (prop);
  if (idx >= 0 && idx < FONT_EXTRA_INDEX)
    ASET (font_spec, idx, font_prop_validate (idx, Qnil, val));
  else
    font_put_extra (font_spec, prop, font_prop_validate (0, prop, val));
  return val;
}

DEFUN ("list-fonts", Flist_fonts, Slist_fonts, 1, 4, 0,
       doc: /* List available fonts matching FONT-SPEC on the current frame.
Optional 2nd argument FRAME specifies the target frame.
Optional 3rd argument NUM, if non-nil, limits the number of returned fonts.
Optional 4th argument PREFER, if non-nil, is a font-spec to
control the order of the returned list.  Fonts are sorted by
how close they are to PREFER.  */)
     (font_spec, frame, num, prefer)
     Lisp_Object font_spec, frame, num, prefer;
{
  Lisp_Object vec, list, tail;
  int n = 0, i, len;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  CHECK_FONT_SPEC (font_spec);
  if (! NILP (num))
    {
      CHECK_NUMBER (num);
      n = XINT (num);
      if (n <= 0)
	return Qnil;
    }
  if (! NILP (prefer))
    CHECK_FONT_SPEC (prefer);

  vec = font_list_entities (frame, font_spec);
  len = ASIZE (vec);
  if (len == 0)
    return Qnil;
  if (len == 1)
    return Fcons (AREF (vec, 0), Qnil);

  if (! NILP (prefer))
    vec = font_sort_entites (vec, prefer, frame, 0);

  list = tail = Fcons (AREF (vec, 0), Qnil);
  if (n == 0 || n > len)
    n = len;
  for (i = 1; i < n; i++)
    {
      Lisp_Object val = Fcons (AREF (vec, i), Qnil);

      XSETCDR (tail, val);
      tail = val;
    }
  return list;
}

DEFUN ("font-family-list", Ffont_family_list, Sfont_family_list, 0, 1, 0,
       doc: /* List available font families on the current frame.
Optional argument FRAME, if non-nil, specifies the target frame.  */)
     (frame)
     Lisp_Object frame;
{
  FRAME_PTR f;
  struct font_driver_list *driver_list;
  Lisp_Object list;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  list = Qnil;
  for (driver_list = f->font_driver_list; driver_list;
       driver_list = driver_list->next)
    if (driver_list->driver->list_family)
      {
	Lisp_Object val = driver_list->driver->list_family (frame);

	if (NILP (list))
	  list = val;
	else
	  {
	    Lisp_Object tail = list;

	    for (; CONSP (val); val = XCDR (val))
	      if (NILP (Fmemq (XCAR (val), tail)))
		list = Fcons (XCAR (val), list);
	  }
      }
  return list;
}

DEFUN ("find-font", Ffind_font, Sfind_font, 1, 2, 0,
       doc: /* Return a font-entity matching with FONT-SPEC on the current frame.
Optional 2nd argument FRAME, if non-nil, specifies the target frame.  */)
     (font_spec, frame)
     Lisp_Object font_spec, frame;
{
  Lisp_Object val = Flist_fonts (font_spec, frame, make_number (1), Qnil);

  if (CONSP (val))
    val = XCAR (val);
  return val;
}

DEFUN ("font-xlfd-name", Ffont_xlfd_name, Sfont_xlfd_name, 1, 2, 0,
       doc: /*  Return XLFD name of FONT.
FONT is a font-spec, font-entity, or font-object.
If the name is too long for XLFD (maximum 255 chars), return nil.
If the 2nd optional arg FOLD-WILDCARDS is non-nil,
the consecutive wildcards are folded to one.  */)
     (font, fold_wildcards)
     Lisp_Object font, fold_wildcards;
{
  char name[256];
  int pixel_size = 0;

  CHECK_FONT (font);

  if (FONT_OBJECT_P (font))
    {
      Lisp_Object font_name = AREF (font, FONT_NAME_INDEX);

      if (STRINGP (font_name)
	  && SDATA (font_name)[0] == '-')
	{
	  if (NILP (fold_wildcards))
	    return font_name;
	  strcpy (name, (char *) SDATA (font_name));
	  goto done;
	}
      pixel_size = XFONT_OBJECT (font)->pixel_size;
    }
  if (font_unparse_xlfd (font, pixel_size, name, 256) < 0)
    return Qnil;
 done:
  if (! NILP (fold_wildcards))
    {
      char *p0 = name, *p1;

      while ((p1 = strstr (p0, "-*-*")))
	{
	  strcpy (p1, p1 + 2);
	  p0 = p1;
	}
    }

  return build_string (name);
}

DEFUN ("clear-font-cache", Fclear_font_cache, Sclear_font_cache, 0, 0, 0,
       doc: /* Clear font cache.  */)
     ()
{
  Lisp_Object list, frame;

  FOR_EACH_FRAME (list, frame)
    {
      FRAME_PTR f = XFRAME (frame);
      struct font_driver_list *driver_list = f->font_driver_list;

      for (; driver_list; driver_list = driver_list->next)
	if (driver_list->on)
	  {
	    Lisp_Object cache = driver_list->driver->get_cache (f);
	    Lisp_Object val;

	    val = XCDR (cache);
	    while (! NILP (val)
		   && ! EQ (XCAR (XCAR (val)), driver_list->driver->type))
	      val = XCDR (val);
	    font_assert (! NILP (val));
	    val = XCDR (XCAR (val));
	    if (XINT (XCAR (val)) == 0)
	      {
		font_clear_cache (f, XCAR (val), driver_list->driver);
		XSETCDR (cache, XCDR (val));
	      }
	  }
    }

  return Qnil;
}


void
font_fill_lglyph_metrics (glyph, font_object)
     Lisp_Object glyph, font_object;
{
  struct font *font = XFONT_OBJECT (font_object);
  unsigned code = font->driver->encode_char (font, LGLYPH_CHAR (glyph));
  struct font_metrics metrics;

  LGLYPH_SET_CODE (glyph, code);
  font->driver->text_extents (font, &code, 1, &metrics);
  LGLYPH_SET_LBEARING (glyph, metrics.lbearing);
  LGLYPH_SET_RBEARING (glyph, metrics.rbearing);
  LGLYPH_SET_WIDTH (glyph, metrics.width);
  LGLYPH_SET_ASCENT (glyph, metrics.ascent);
  LGLYPH_SET_DESCENT (glyph, metrics.descent);
}


DEFUN ("font-shape-gstring", Ffont_shape_gstring, Sfont_shape_gstring, 1, 1, 0,
       doc: /* Shape the glyph-string GSTRING.
Shaping means substituting glyphs and/or adjusting positions of glyphs
to get the correct visual image of character sequences set in the
header of the glyph-string.

If the shaping was successful, the value is GSTRING itself or a newly
created glyph-string.  Otherwise, the value is nil.  */)
     (gstring)
     Lisp_Object gstring;
{
  struct font *font;
  Lisp_Object font_object, n, glyph;
  int i;
  
  if (! composition_gstring_p (gstring))
    signal_error ("Invalid glyph-string: ", gstring);
  if (! NILP (LGSTRING_ID (gstring)))
    return gstring;
  font_object = LGSTRING_FONT (gstring);
  CHECK_FONT_OBJECT (font_object);
  font = XFONT_OBJECT (font_object);
  if (! font->driver->shape)
    return Qnil;

  /* Try at most three times with larger gstring each time.  */
  for (i = 0; i < 3; i++)
    {
      n = font->driver->shape (gstring);
      if (INTEGERP (n))
	break;
      gstring = larger_vector (gstring,
			       ASIZE (gstring) + LGSTRING_GLYPH_LEN (gstring),
			       Qnil);
    }
  if (i == 3 || XINT (n) == 0)
    return Qnil;
  
  glyph = LGSTRING_GLYPH (gstring, 0);
  for (i = 1; i < LGSTRING_GLYPH_LEN (gstring); i++)
    {
      Lisp_Object this = LGSTRING_GLYPH (gstring, i);

      if (NILP (this))
	break;
      if (NILP (LGLYPH_ADJUSTMENT (this)))
	glyph = this;
      else
	{
	  int from = LGLYPH_FROM (glyph);
	  int to = LGLYPH_TO (glyph);

	  LGLYPH_SET_FROM (this, from);
	  LGLYPH_SET_TO (this, to);
	}
    }
  return composition_gstring_put_cache (gstring, XINT (n));
}

#if 0

DEFUN ("font-drive-otf", Ffont_drive_otf, Sfont_drive_otf, 6, 6, 0,
       doc: /* Apply OpenType features on glyph-string GSTRING-IN.
OTF-FEATURES specifies which features to apply in this format:
  (SCRIPT LANGSYS GSUB GPOS)
where
  SCRIPT is a symbol specifying a script tag of OpenType,
  LANGSYS is a symbol specifying a langsys tag of OpenType,
  GSUB and GPOS, if non-nil, are lists of symbols specifying feature tags.

If LANGYS is nil, the default langsys is selected.

The features are applied in the order they appear in the list.  The
symbol `*' means to apply all available features not present in this
list, and the remaining features are ignored.  For instance, (vatu
pstf * haln) is to apply vatu and pstf in this order, then to apply
all available features other than vatu, pstf, and haln.

The features are applied to the glyphs in the range FROM and TO of
the glyph-string GSTRING-IN.

If some feature is actually applicable, the resulting glyphs are
produced in the glyph-string GSTRING-OUT from the index INDEX.  In
this case, the value is the number of produced glyphs.

If no feature is applicable, no glyph is produced in GSTRING-OUT, and
the value is 0.

If GSTRING-OUT is too short to hold produced glyphs, no glyphs are
produced in GSTRING-OUT, and the value is nil.

See the documentation of `font-make-gstring' for the format of
glyph-string.  */)
     (otf_features, gstring_in, from, to, gstring_out, index)
     Lisp_Object otf_features, gstring_in, from, to, gstring_out, index;
{
  Lisp_Object font_object = LGSTRING_FONT (gstring_in);
  Lisp_Object val;
  struct font *font;
  int len, num;

  check_otf_features (otf_features);
  CHECK_FONT_OBJECT (font_object);
  font = XFONT_OBJECT (font_object);
  if (! font->driver->otf_drive)
    error ("Font backend %s can't drive OpenType GSUB table",
	   SDATA (SYMBOL_NAME (font->driver->type)));
  CHECK_CONS (otf_features);
  CHECK_SYMBOL (XCAR (otf_features));
  val = XCDR (otf_features);
  CHECK_SYMBOL (XCAR (val));
  val = XCDR (otf_features);
  if (! NILP (val))
    CHECK_CONS (val);
  len = check_gstring (gstring_in);
  CHECK_VECTOR (gstring_out);
  CHECK_NATNUM (from);
  CHECK_NATNUM (to);
  CHECK_NATNUM (index);

  if (XINT (from) >= XINT (to) || XINT (to) > len)
    args_out_of_range_3 (from, to, make_number (len));
  if (XINT (index) >= ASIZE (gstring_out))
    args_out_of_range (index, make_number (ASIZE (gstring_out)));
  num = font->driver->otf_drive (font, otf_features,
				 gstring_in, XINT (from), XINT (to),
				 gstring_out, XINT (index), 0);
  if (num < 0)
    return Qnil;
  return make_number (num);
}

DEFUN ("font-otf-alternates", Ffont_otf_alternates, Sfont_otf_alternates,
       3, 3, 0,
       doc: /* Return a list of alternate glyphs of CHARACTER in FONT-OBJECT.
OTF-FEATURES specifies which features of the font FONT-OBJECT to apply
in this format:
  (SCRIPT LANGSYS FEATURE ...)
See the documentation of `font-drive-otf' for more detail.

The value is a list of cons cells of the format (GLYPH-ID . CHARACTER),
where GLYPH-ID is a glyph index of the font, and CHARACTER is a
character code corresponding to the glyph or nil if there's no
corresponding character.  */)
     (font_object, character, otf_features)
     Lisp_Object font_object, character, otf_features;
{
  struct font *font;
  Lisp_Object gstring_in, gstring_out, g;
  Lisp_Object alternates;
  int i, num;

  CHECK_FONT_GET_OBJECT (font_object, font);
  if (! font->driver->otf_drive)
    error ("Font backend %s can't drive OpenType GSUB table",
	   SDATA (SYMBOL_NAME (font->driver->type)));
  CHECK_CHARACTER (character);
  CHECK_CONS (otf_features);

  gstring_in = Ffont_make_gstring (font_object, make_number (1));
  g = LGSTRING_GLYPH (gstring_in, 0);
  LGLYPH_SET_CHAR (g, XINT (character));
  gstring_out = Ffont_make_gstring (font_object, make_number (10));
  while ((num = font->driver->otf_drive (font, otf_features, gstring_in, 0, 1,
					 gstring_out, 0, 1)) < 0)
    gstring_out = Ffont_make_gstring (font_object,
				      make_number (ASIZE (gstring_out) * 2));
  alternates = Qnil;
  for (i = 0; i < num; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring_out, i);
      int c = LGLYPH_CHAR (g);
      unsigned code = LGLYPH_CODE (g);

      alternates = Fcons (Fcons (make_number (code),
				 c > 0 ? make_number (c) : Qnil),
			  alternates);
    }
  return Fnreverse (alternates);
}
#endif	/* 0 */

#ifdef FONT_DEBUG

DEFUN ("open-font", Fopen_font, Sopen_font, 1, 3, 0,
       doc: /* Open FONT-ENTITY.  */)
     (font_entity, size, frame)
     Lisp_Object font_entity;
     Lisp_Object size;
     Lisp_Object frame;
{
  int isize;

  CHECK_FONT_ENTITY (font_entity);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);

  if (NILP (size))
    isize = XINT (AREF (font_entity, FONT_SIZE_INDEX));
  else
    {
      CHECK_NUMBER_OR_FLOAT (size);
      if (FLOATP (size))
	isize = POINT_TO_PIXEL (- isize, XFRAME (frame)->resy);
      else
	isize = XINT (size);
      if (isize == 0)
	isize = 120;
    }
  return font_open_entity (XFRAME (frame), font_entity, isize);
}

DEFUN ("close-font", Fclose_font, Sclose_font, 1, 2, 0,
       doc: /* Close FONT-OBJECT.  */)
     (font_object, frame)
     Lisp_Object font_object, frame;
{
  CHECK_FONT_OBJECT (font_object);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  font_close_object (XFRAME (frame), font_object);
  return Qnil;
}

DEFUN ("query-font", Fquery_font, Squery_font, 1, 1, 0,
       doc: /* Return information about FONT-OBJECT.
The value is a vector:
  [ NAME FILENAME PIXEL-SIZE SIZE ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
    CAPABILITY ]

NAME is a string of the font name (or nil if the font backend doesn't
provide a name).

FILENAME is a string of the font file (or nil if the font backend
doesn't provide a file name).

PIXEL-SIZE is a pixel size by which the font is opened.

SIZE is a maximum advance width of the font in pixels.

ASCENT, DESCENT, SPACE-WIDTH, AVERAGE-WIDTH are metrics of the font in
pixels.

CAPABILITY is a list whose first element is a symbol representing the
font format \(x, opentype, truetype, type1, pcf, or bdf) and the
remaining elements describe the details of the font capability.

If the font is OpenType font, the form of the list is
  \(opentype GSUB GPOS)
where GSUB shows which "GSUB" features the font supports, and GPOS
shows which "GPOS" features the font supports.  Both GSUB and GPOS are
lists of the format:
  \((SCRIPT (LANGSYS FEATURE ...) ...) ...)

If the font is not OpenType font, currently the length of the form is
one.

SCRIPT is a symbol representing OpenType script tag.

LANGSYS is a symbol representing OpenType langsys tag, or nil
representing the default langsys.

FEATURE is a symbol representing OpenType feature tag.

If the font is not OpenType font, CAPABILITY is nil.  */)
     (font_object)
     Lisp_Object font_object;
{
  struct font *font;
  Lisp_Object val;

  CHECK_FONT_GET_OBJECT (font_object, font);

  val = Fmake_vector (make_number (9), Qnil);
  ASET (val, 0, AREF (font_object, FONT_NAME_INDEX));
  ASET (val, 1, AREF (font_object, FONT_FILE_INDEX));
  ASET (val, 2, make_number (font->pixel_size));
  ASET (val, 3, make_number (font->max_width));
  ASET (val, 4, make_number (font->ascent));
  ASET (val, 5, make_number (font->descent));
  ASET (val, 6, make_number (font->space_width));
  ASET (val, 7, make_number (font->average_width));
  if (font->driver->otf_capability)
    ASET (val, 8, Fcons (Qopentype, font->driver->otf_capability (font)));
  return val;
}

DEFUN ("get-font-glyphs", Fget_font_glyphs, Sget_font_glyphs, 2, 2, 0,
       doc: /* Return a vector of glyphs of FONT-OBJECT for drawing STRING.
Each element is a vector [GLYPH-CODE LBEARING RBEARING WIDTH ASCENT DESCENT].  */)
     (font_object, string)
     Lisp_Object font_object, string;
{
  struct font *font;
  int i, len;
  Lisp_Object vec;

  CHECK_FONT_GET_OBJECT (font_object, font);
  CHECK_STRING (string);
  len = SCHARS (string);
  vec = Fmake_vector (make_number (len), Qnil);
  for (i = 0; i < len; i++)
    {
      Lisp_Object ch = Faref (string, make_number (i));
      Lisp_Object val;
      int c = XINT (ch);
      unsigned code;
      EMACS_INT cod;
      struct font_metrics metrics;

      cod = code = font->driver->encode_char (font, c);
      if (code == FONT_INVALID_CODE)
	continue;
      val = Fmake_vector (make_number (6), Qnil);
      if (cod <= MOST_POSITIVE_FIXNUM)
	ASET (val, 0, make_number (code));
      else
	ASET (val, 0, Fcons (make_number (code >> 16),
			     make_number (code & 0xFFFF)));
      font->driver->text_extents (font, &code, 1, &metrics);
      ASET (val, 1, make_number (metrics.lbearing));
      ASET (val, 2, make_number (metrics.rbearing));
      ASET (val, 3, make_number (metrics.width));
      ASET (val, 4, make_number (metrics.ascent));
      ASET (val, 5, make_number (metrics.descent));
      ASET (vec, i, val);
    }
  return vec;
}

DEFUN ("font-match-p", Ffont_match_p, Sfont_match_p, 2, 2, 0,
       doc: /* Return t if and only if font-spec SPEC matches with FONT.
FONT is a font-spec, font-entity, or font-object. */)
     (spec, font)
     Lisp_Object spec, font;
{
  CHECK_FONT_SPEC (spec);
  CHECK_FONT (font);

  return (font_match_p (spec, font) ? Qt : Qnil);
}

DEFUN ("font-at", Ffont_at, Sfont_at, 1, 3, 0,
       doc: /* Return a font-object for displaying a character at POSITION.
Optional second arg WINDOW, if non-nil, is a window displaying
the current buffer.  It defaults to the currently selected window.  */)
     (position, window, string)
     Lisp_Object position, window, string;
{
  struct window *w;
  EMACS_INT pos;

  if (NILP (string))
    {
      CHECK_NUMBER_COERCE_MARKER (position);
      pos = XINT (position);
      if (pos < BEGV || pos >= ZV)
	args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
    }
  else
    {
      CHECK_NUMBER (position);
      CHECK_STRING (string);
      pos = XINT (position);
      if (pos < 0 || pos >= SCHARS (string))
	args_out_of_range (string, position);
    }
  if (NILP (window))
    window = selected_window;
  CHECK_LIVE_WINDOW (window);
  w = XWINDOW (window);

  return font_at (-1, pos, NULL, w, string);
}

#if 0
DEFUN ("draw-string", Fdraw_string, Sdraw_string, 2, 2, 0,
       doc: /*  Draw STRING by FONT-OBJECT on the top left corner of the current frame.
The value is a number of glyphs drawn.
Type C-l to recover what previously shown.  */)
     (font_object, string)
     Lisp_Object font_object, string;
{
  Lisp_Object frame = selected_frame;
  FRAME_PTR f = XFRAME (frame);
  struct font *font;
  struct face *face;
  int i, len, width;
  unsigned *code;

  CHECK_FONT_GET_OBJECT (font_object, font);
  CHECK_STRING (string);
  len = SCHARS (string);
  code = alloca (sizeof (unsigned) * len);
  for (i = 0; i < len; i++)
    {
      Lisp_Object ch = Faref (string, make_number (i));
      Lisp_Object val;
      int c = XINT (ch);

      code[i] = font->driver->encode_char (font, c);
      if (code[i] == FONT_INVALID_CODE)
	break;
    }
  face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  face->fontp = font;
  if (font->driver->prepare_face)
    font->driver->prepare_face (f, face);
  width = font->driver->text_extents (font, code, i, NULL);
  len = font->driver->draw_text (f, face, 0, font->ascent, code, i, width);
  if (font->driver->done_face)
    font->driver->done_face (f, face);
  face->fontp = NULL;
  return make_number (len);
}
#endif

#endif	/* FONT_DEBUG */

#ifdef HAVE_WINDOW_SYSTEM

DEFUN ("font-info", Ffont_info, Sfont_info, 1, 2, 0,
       doc: /* Return information about a font named NAME on frame FRAME.
If FRAME is omitted or nil, use the selected frame.
The returned value is a vector of OPENED-NAME, FULL-NAME, CHARSET, SIZE,
  HEIGHT, BASELINE-OFFSET, RELATIVE-COMPOSE, and DEFAULT-ASCENT,
where
  OPENED-NAME is the name used for opening the font,
  FULL-NAME is the full name of the font,
  SIZE is the maximum bound width of the font,
  HEIGHT is the height of the font,
  BASELINE-OFFSET is the upward offset pixels from ASCII baseline,
  RELATIVE-COMPOSE and DEFAULT-ASCENT are the numbers controlling
    how to compose characters.
If the named font is not yet loaded, return nil.  */)
     (name, frame)
     Lisp_Object name, frame;
{
  FRAME_PTR f;
  struct font *font;
  Lisp_Object info;
  Lisp_Object font_object;

  (*check_window_system_func) ();

  if (! FONTP (name))
    CHECK_STRING (name);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (STRINGP (name))
    {
      int fontset = fs_query_fontset (name, 0);

      if (fontset >= 0)
	name = fontset_ascii (fontset);
      font_object = font_open_by_name (f, (char *) SDATA (name));
    }
  else if (FONT_OBJECT_P (name))
    font_object = name;
  else if (FONT_ENTITY_P (name))
    font_object = font_open_entity (f, name, 0);
  else
    {
      struct face *face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      Lisp_Object entity = font_matching_entity (f, face->lface, name);

      font_object = ! NILP (entity) ? font_open_entity (f, entity, 0) : Qnil;
    }
  if (NILP (font_object))
    return Qnil;
  font = XFONT_OBJECT (font_object);

  info = Fmake_vector (make_number (7), Qnil);
  XVECTOR (info)->contents[0] = AREF (font_object, FONT_NAME_INDEX);
  XVECTOR (info)->contents[1] = AREF (font_object, FONT_NAME_INDEX);
  XVECTOR (info)->contents[2] = make_number (font->pixel_size);
  XVECTOR (info)->contents[3] = make_number (font->height);
  XVECTOR (info)->contents[4] = make_number (font->baseline_offset);
  XVECTOR (info)->contents[5] = make_number (font->relative_compose);
  XVECTOR (info)->contents[6] = make_number (font->default_ascent);

#if 0
  /* As font_object is still in FONT_OBJLIST of the entity, we can't
     close it now.  Perhaps, we should manage font-objects
     by `reference-count'.  */
  font_close_object (f, font_object);
#endif
  return info;
}
#endif


#define BUILD_STYLE_TABLE(TBL) \
  build_style_table ((TBL), sizeof TBL / sizeof (struct table_entry))

static Lisp_Object
build_style_table (entry, nelement)
     struct table_entry *entry;
     int nelement;
{
  int i, j;
  Lisp_Object table, elt;

  table = Fmake_vector (make_number (nelement), Qnil);
  for (i = 0; i < nelement; i++)
    {
      for (j = 0; entry[i].names[j]; j++);
      elt = Fmake_vector (make_number (j + 1), Qnil);
      ASET (elt, 0, make_number (entry[i].numeric));
      for (j = 0; entry[i].names[j]; j++)
	ASET (elt, j + 1, intern (entry[i].names[j]));
      ASET (table, i, elt);
    }
  return table;
}

static Lisp_Object Vfont_log;
static int font_log_env_checked;

/* The deferred font-log data of the form [ACTION ARG RESULT].
   If ACTION is not nil, that is added to the log when font_add_log is
   called next time.  At that time, ACTION is set back to nil.  */
static Lisp_Object Vfont_log_deferred;

/* Prepend the font-related logging data in Vfont_log if it is not
   `t'.  ACTION describes a kind of font-related action (e.g. listing,
   opening), ARG is the argument for the action, and RESULT is the
   result of the action.  */
void
font_add_log (action, arg, result)
     char *action;
     Lisp_Object arg, result;
{
  Lisp_Object tail, val;
  int i;

  if (! font_log_env_checked)
    {
      Vfont_log = egetenv ("EMACS_FONT_LOG") ? Qnil : Qt;
      font_log_env_checked = 1;
    }
  if (EQ (Vfont_log, Qt))
    return;
  if (STRINGP (AREF (Vfont_log_deferred, 0)))
    {
      char *str = (char *) SDATA (AREF (Vfont_log_deferred, 0));

      ASET (Vfont_log_deferred, 0, Qnil);
      font_add_log (str, AREF (Vfont_log_deferred, 1),
		    AREF (Vfont_log_deferred, 2));
    }

  if (FONTP (arg))
    {
      Lisp_Object tail, elt;
      Lisp_Object equalstr = build_string ("=");

      val = Ffont_xlfd_name (arg, Qt);
      for (tail = AREF (arg, FONT_EXTRA_INDEX); CONSP (tail);
	   tail = XCDR (tail))
	{
	  elt = XCAR (tail);
	  if (EQ (XCAR (elt), QCscript))
	    val = concat3 (val, SYMBOL_NAME (QCscript),
			   concat2 (equalstr, SYMBOL_NAME (XCDR (elt))));
	  else if (EQ (XCAR (elt), QClang))
	    val = concat3 (val, SYMBOL_NAME (QClang),
			   concat2 (equalstr, SYMBOL_NAME (XCDR (elt))));
	  else if (EQ (XCAR (elt), QCotf) && CONSP (XCDR (elt)))
	    val = concat3 (val, SYMBOL_NAME (QCotf),
			   concat2 (equalstr,
				    SYMBOL_NAME (XCAR (XCDR (elt)))));
	}
      arg = val;
    }
  if (FONTP (result))
    {
      val = Ffont_xlfd_name (result, Qt);
      if (! FONT_SPEC_P (result))
	val = concat3 (SYMBOL_NAME (AREF (result, FONT_TYPE_INDEX)),
		       build_string (":"), val);
      result = val;
    }
  else if (CONSP (result))
    {
      result = Fcopy_sequence (result);
      for (tail = result; CONSP (tail); tail = XCDR (tail))
	{
	  val = XCAR (tail);
	  if (FONTP (val))
	    val = Ffont_xlfd_name (val, Qt);
	  XSETCAR (tail, val);
	}
    }
  else if (VECTORP (result))
    {
      result = Fcopy_sequence (result);
      for (i = 0; i < ASIZE (result); i++)
	{
	  val = AREF (result, i);
	  if (FONTP (val))
	    val = Ffont_xlfd_name (val, Qt);
	  ASET (result, i, val);
	}
    }
  Vfont_log = Fcons (list3 (intern (action), arg, result), Vfont_log);
}

/* Record a font-related logging data to be added to Vfont_log when
   font_add_log is called next time.  ACTION, ARG, RESULT are the same
   as font_add_log.  */

void
font_deferred_log (action, arg, result)
     char *action;
     Lisp_Object arg, result;
{
  ASET (Vfont_log_deferred, 0, build_string (action));
  ASET (Vfont_log_deferred, 1, arg);
  ASET (Vfont_log_deferred, 2, result);
}     

extern void syms_of_ftfont P_ (());
extern void syms_of_xfont P_ (());
extern void syms_of_xftfont P_ (());
extern void syms_of_ftxfont P_ (());
extern void syms_of_bdffont P_ (());
extern void syms_of_w32font P_ (());
extern void syms_of_atmfont P_ (());
extern void syms_of_nsfont P_ (());

void
syms_of_font ()
{
  sort_shift_bits[FONT_TYPE_INDEX] = 0;
  sort_shift_bits[FONT_SLANT_INDEX] = 2;
  sort_shift_bits[FONT_WEIGHT_INDEX] = 9;
  sort_shift_bits[FONT_SIZE_INDEX] = 16;
  sort_shift_bits[FONT_WIDTH_INDEX] = 23;
  /* Note that the other elements in sort_shift_bits are not used.  */

  staticpro (&font_charset_alist);
  font_charset_alist = Qnil;

  DEFSYM (Qfont_spec, "font-spec");
  DEFSYM (Qfont_entity, "font-entity");
  DEFSYM (Qfont_object, "font-object");

  DEFSYM (Qopentype, "opentype");

  DEFSYM (Qascii_0, "ascii-0");
  DEFSYM (Qiso8859_1, "iso8859-1");
  DEFSYM (Qiso10646_1, "iso10646-1");
  DEFSYM (Qunicode_bmp, "unicode-bmp");
  DEFSYM (Qunicode_sip, "unicode-sip");

  DEFSYM (QCf, "Cf");

  DEFSYM (QCotf, ":otf");
  DEFSYM (QClang, ":lang");
  DEFSYM (QCscript, ":script");
  DEFSYM (QCantialias, ":antialias");

  DEFSYM (QCfoundry, ":foundry");
  DEFSYM (QCadstyle, ":adstyle");
  DEFSYM (QCregistry, ":registry");
  DEFSYM (QCspacing, ":spacing");
  DEFSYM (QCdpi, ":dpi");
  DEFSYM (QCscalable, ":scalable");
  DEFSYM (QCavgwidth, ":avgwidth");
  DEFSYM (QCfont_entity, ":font-entity");
  DEFSYM (QCfc_unknown_spec, ":fc-unknown-spec");

  DEFSYM (Qc, "c");
  DEFSYM (Qm, "m");
  DEFSYM (Qp, "p");
  DEFSYM (Qd, "d");

  staticpro (&null_vector);
  null_vector = Fmake_vector (make_number (0), Qnil);

  staticpro (&scratch_font_spec);
  scratch_font_spec = Ffont_spec (0, NULL);
  staticpro (&scratch_font_prefer);
  scratch_font_prefer = Ffont_spec (0, NULL);

  staticpro (&Vfont_log_deferred);
  Vfont_log_deferred = Fmake_vector (make_number (3), Qnil);

#if 0
#ifdef HAVE_LIBOTF
  staticpro (&otf_list);
  otf_list = Qnil;
#endif	/* HAVE_LIBOTF */
#endif	/* 0 */

  defsubr (&Sfontp);
  defsubr (&Sfont_spec);
  defsubr (&Sfont_get);
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sfont_face_attributes);
#endif
  defsubr (&Sfont_put);
  defsubr (&Slist_fonts);
  defsubr (&Sfont_family_list);
  defsubr (&Sfind_font);
  defsubr (&Sfont_xlfd_name);
  defsubr (&Sclear_font_cache);
  defsubr (&Sfont_shape_gstring);
#if 0
  defsubr (&Sfont_drive_otf);
  defsubr (&Sfont_otf_alternates);
#endif	/* 0 */

#ifdef FONT_DEBUG
  defsubr (&Sopen_font);
  defsubr (&Sclose_font);
  defsubr (&Squery_font);
  defsubr (&Sget_font_glyphs);
  defsubr (&Sfont_match_p);
  defsubr (&Sfont_at);
#if 0
  defsubr (&Sdraw_string);
#endif
#endif	/* FONT_DEBUG */
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sfont_info);
#endif

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
	       doc: /*
Alist of fontname patterns vs the corresponding encoding and repertory info.
Each element looks like (REGEXP . (ENCODING . REPERTORY)),
where ENCODING is a charset or a char-table,
and REPERTORY is a charset, a char-table, or nil.

If ENCODING and REPERTORY are the same, the element can have the form
\(REGEXP . ENCODING).

ENCODING is for converting a character to a glyph code of the font.
If ENCODING is a charset, encoding a character by the charset gives
the corresponding glyph code.  If ENCODING is a char-table, looking up
the table by a character gives the corresponding glyph code.

REPERTORY specifies a repertory of characters supported by the font.
If REPERTORY is a charset, all characters beloging to the charset are
supported.  If REPERTORY is a char-table, all characters who have a
non-nil value in the table are supported.  If REPERTORY is nil, Emacs
gets the repertory information by an opened font and ENCODING.  */);
  Vfont_encoding_alist = Qnil;

  DEFVAR_LISP_NOPRO ("font-weight-table", &Vfont_weight_table,
	       doc: /*  Vector of valid font weight values.
Each element has the form:
    [NUMERIC-VALUE SYMBOLIC-NAME ALIAS-NAME ...]
NUMERIC-VALUE is an integer, and SYMBOLIC-NAME and ALIAS-NAME are symbols. */);
  Vfont_weight_table = BUILD_STYLE_TABLE (weight_table);

  DEFVAR_LISP_NOPRO ("font-slant-table", &Vfont_slant_table,
	       doc: /*  Vector of font slant symbols vs the corresponding numeric values.
See `font-weight-table' for the format of the vector. */);
  Vfont_slant_table = BUILD_STYLE_TABLE (slant_table);

  DEFVAR_LISP_NOPRO ("font-width-table", &Vfont_width_table,
	       doc: /*  Alist of font width symbols vs the corresponding numeric values.
See `font-weight-table' for the format of the vector. */);
  Vfont_width_table = BUILD_STYLE_TABLE (width_table);

  staticpro (&font_style_table);
  font_style_table = Fmake_vector (make_number (3), Qnil);
  ASET (font_style_table, 0, Vfont_weight_table);
  ASET (font_style_table, 1, Vfont_slant_table);
  ASET (font_style_table, 2, Vfont_width_table);

  DEFVAR_LISP ("font-log", &Vfont_log, doc: /*
*Logging list of font related actions and results.
The value t means to suppress the logging.
The initial value is set to nil if the environment variable
EMACS_FONT_LOG is set.  Otherwise, it is set to t.  */);
  Vfont_log = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
#ifdef HAVE_FREETYPE
  syms_of_ftfont ();
#ifdef HAVE_X_WINDOWS
  syms_of_xfont ();
  syms_of_ftxfont ();
#ifdef HAVE_XFT
  syms_of_xftfont ();
#endif  /* HAVE_XFT */
#endif	/* HAVE_X_WINDOWS */
#else	/* not HAVE_FREETYPE */
#ifdef HAVE_X_WINDOWS
  syms_of_xfont ();
#endif	/* HAVE_X_WINDOWS */
#endif	/* not HAVE_FREETYPE */
#ifdef HAVE_BDFFONT
  syms_of_bdffont ();
#endif	/* HAVE_BDFFONT */
#ifdef WINDOWSNT
  syms_of_w32font ();
#endif	/* WINDOWSNT */
#ifdef HAVE_NS
  syms_of_nsfont ();
#endif	/* HAVE_NS */
#endif	/* HAVE_WINDOW_SYSTEM */
}

/* arch-tag: 74c9475d-5976-4c93-a327-942ae3072846
   (do not change this comment) */
