/* font.c -- "Font" primitives.
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
#include <stdlib.h>
#include <ctype.h>

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

#ifndef FONT_DEBUG
#define FONT_DEBUG
#endif

#ifdef FONT_DEBUG
#undef xassert
#define xassert(X)	do {if (!(X)) abort ();} while (0)
#else
#define xassert(X)	(void) 0
#endif

int enable_font_backend;

Lisp_Object Qfontp;

Lisp_Object Qopentype;

/* Important character set symbols.  */
Lisp_Object Qiso8859_1, Qiso10646_1, Qunicode_bmp, Qunicode_sip;

/* Like CHECK_FONT_SPEC but also validate properties of the font-spec,
   and set X to the validated result.  */

#define CHECK_VALIDATE_FONT_SPEC(x)				\
  do {								\
    if (! FONT_SPEC_P (x)) x = wrong_type_argument (Qfont, x);	\
    x = font_prop_validate (x);					\
  } while (0)

/* Number of pt per inch (from the TeXbook).  */
#define PT_PER_INCH 72.27

/* Return a pixel size (integer) corresponding to POINT size (double)
   on resolution DPI.  */
#define POINT_TO_PIXEL(POINT, DPI) ((POINT) * (DPI) / PT_PER_INCH + 0.5)

/* Return a point size (double) corresponding to POINT size (integer)
   on resolution DPI.  */
#define PIXEL_TO_POINT(PIXEL, DPI) ((PIXEL) * PT_PER_INCH * 10 / (DPI) + 0.5)

/* Special string of zero length.  It is used to specify a NULL name
   in a font properties (e.g. adstyle).  We don't use the symbol of
   NULL name because it's confusing (Lisp printer prints nothing for
   it). */
Lisp_Object null_string;

/* Special vector of zero length.  This is repeatedly used by (struct
   font_driver *)->list when a specified font is not found. */
Lisp_Object null_vector;

/* Vector of 3 elements.  Each element is an alist for one of font
   style properties (weight, slant, width).  The alist contains a
   mapping between symbolic property values (e.g. `medium' for weight)
   and numeric property values (e.g. 100).  So, it looks like this:
	[((thin . 0) ... (heavy . 210))
	 ((ro . 0) ... (ot . 210))
	 ((ultracondensed . 50) ... (wide . 200))]  */
static Lisp_Object font_style_table;

/* Alist of font family vs the corresponding aliases.
   Each element has this form:
	(FAMILY ALIAS1 ALIAS2 ...)   */

static Lisp_Object font_family_alist;

/* Symbols representing keys of normal font properties.  */
extern Lisp_Object QCtype, QCfamily, QCweight, QCslant, QCwidth, QCsize, QCname;
Lisp_Object QCfoundry, QCadstyle, QCregistry, QCextra;
/* Symbols representing keys of font extra info.  */
Lisp_Object QCspacing, QCdpi, QCscalable, QCotf, QClanguage, QCscript;
/* Symbols representing values of font spacing property.  */
Lisp_Object Qc, Qm, Qp, Qd;

/* List of all font drivers.  All font-backends (XXXfont.c) call
   add_font_driver in syms_of_XXXfont to register the font-driver
   here.  */
static struct font_driver_list *font_driver_list;

static int font_pixel_size P_ ((FRAME_PTR f, Lisp_Object));
static Lisp_Object prop_name_to_numeric P_ ((enum font_property_index,
					     Lisp_Object));
static Lisp_Object prop_numeric_to_name P_ ((enum font_property_index, int));
static Lisp_Object font_open_entity P_ ((FRAME_PTR, Lisp_Object, int));
static void build_font_family_alist P_ ((void));

/* Number of registered font drivers.  */
static int num_font_drivers;

/* Return a pixel size of font-spec SPEC on frame F.  */

static int
font_pixel_size (f, spec)
     FRAME_PTR f;
     Lisp_Object spec;
{
  Lisp_Object size = AREF (spec, FONT_SIZE_INDEX);
  double point_size;
  int pixel_size, dpi;
  Lisp_Object extra, val;
      
  if (INTEGERP (size))
    return XINT (size);
  if (NILP (size))
    return 0;
  point_size = XFLOAT_DATA (size);
  extra = AREF (spec, FONT_EXTRA_INDEX);
  val = assq_no_quit (extra, QCdpi);
  if (CONSP (val))
    {
      if (INTEGERP (XCDR (val)))
	dpi = XINT (XCDR (val));
      else
	dpi = XFLOAT_DATA (XCDR (val)) + 0.5;
    }
  else
    dpi = f->resy;
  pixel_size = POINT_TO_PIXEL (point_size, dpi);
  return pixel_size;
}

/* Return a numeric value corresponding to PROP's NAME (symbol).  If
   NAME is not registered in font_style_table, return Qnil.  PROP must
   be one of FONT_{WEIGHT|SLANT|SWIDTH}_INDEX.  */

static Lisp_Object
prop_name_to_numeric (prop, name)
     enum font_property_index prop;
     Lisp_Object name;
{
  int table_index = prop - FONT_WEIGHT_INDEX;
  Lisp_Object val;

  val = assq_no_quit (name, AREF (font_style_table, table_index));
  return (NILP (val) ? Qnil : XCDR (val));
}


/* Return a name (symbol) corresponding to PROP's NUMERIC value.  If
   no name is registered for NUMERIC in font_style_table, return a
   symbol of integer name (e.g. `123').  PROP must be one of
   FONT_{WEIGHT|SLANT|SWIDTH}_INDEX.  */

static Lisp_Object
prop_numeric_to_name (prop, numeric)
     enum font_property_index prop;
     int numeric;
{
  int table_index = prop - FONT_WEIGHT_INDEX;
  Lisp_Object table = AREF (font_style_table, table_index);
  char buf[10];

  while (! NILP (table))
    {
      if (XINT (XCDR (XCAR (table))) >= numeric)
	{
	  if (XINT (XCDR (XCAR (table))) == numeric)
	    return XCAR (XCAR (table));
	  else
	    break;
	}
      table = XCDR (table);
    }
  sprintf (buf, "%d", numeric);
  return intern (buf);
}


/* Return a symbol whose name is STR (length LEN).  If STR contains
   uppercase letters, downcase them in advance.  */

Lisp_Object
intern_downcase (str, len)
     char *str;
     int len;
{
  char *buf;
  int i;

  for (i = 0; i < len; i++)
    if (isupper (str[i]))
      break;
  if (i == len)
    return Fintern (make_unibyte_string (str, len), Qnil);
  buf = alloca (len);
  if (! buf)
    return Fintern (null_string, Qnil);
  bcopy (str, buf, len);
  for (; i < len; i++)
    if (isascii (buf[i]))
      buf[i] = tolower (buf[i]);
  return Fintern (make_unibyte_string (buf, len), Qnil);
}

extern Lisp_Object Vface_alternative_font_family_alist;

static void
build_font_family_alist ()
{
  Lisp_Object alist = Vface_alternative_font_family_alist;

  for (; CONSP (alist); alist = XCDR (alist))
    {
      Lisp_Object tail, elt;

      for (tail = XCAR (alist), elt = Qnil ; CONSP (tail); tail = XCDR (tail))
	elt = nconc2 (elt, Fcons (Fintern (XCAR (tail), Qnil), Qnil));
      font_family_alist = Fcons (elt, font_family_alist);
    }
}


/* Font property validater.  */

static Lisp_Object font_prop_validate_symbol P_ ((enum font_property_index,
						  Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_style P_ ((enum font_property_index,
						 Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_non_neg P_ ((enum font_property_index,
						   Lisp_Object, Lisp_Object));
static Lisp_Object font_prop_validate_spacing P_ ((enum font_property_index,
						   Lisp_Object, Lisp_Object));
static int get_font_prop_index P_ ((Lisp_Object, int));
static Lisp_Object font_prop_validate P_ ((Lisp_Object));
static Lisp_Object font_put_extra P_ ((Lisp_Object, Lisp_Object, Lisp_Object));

static Lisp_Object
font_prop_validate_symbol (prop_index, prop, val)
     enum font_property_index prop_index;
     Lisp_Object prop, val;
{
  if (EQ (prop, QCotf))
    return (SYMBOLP (val) ? val : Qerror);
  if (STRINGP (val))
    val = (SCHARS (val) == 0 ? null_string
	   : intern_downcase ((char *) SDATA (val), SBYTES (val)));
  else if (SYMBOLP (val))
    {
      if (SCHARS (SYMBOL_NAME (val)) == 0)
	val = null_string;
    }
  else
    val = Qerror;
  return val;
}

static Lisp_Object
font_prop_validate_style (prop_index, prop, val)
     enum font_property_index prop_index;
     Lisp_Object prop, val;
{
  if (! INTEGERP (val))
    {
      if (STRINGP (val))
	val = intern_downcase ((char *) SDATA (val), SBYTES (val));
      if (! SYMBOLP (val))
	val = Qerror;
      else
	{
	  val = prop_name_to_numeric (prop_index, val);
	  if (NILP (val))
	    val = Qerror;
	}
    }
  return val;
}

static Lisp_Object
font_prop_validate_non_neg (prop_index, prop, val)
     enum font_property_index prop_index;
     Lisp_Object prop, val;
{
  return (NATNUMP (val) || (FLOATP (val) && XFLOAT_DATA (val) >= 0)
	  ? val : Qerror);
}

static Lisp_Object
font_prop_validate_spacing (prop_index, prop, val)
     enum font_property_index prop_index;
     Lisp_Object prop, val;
{
  if (NILP (val) || (NATNUMP (val) && XINT (val) <= FONT_SPACING_CHARCELL))
    return val;
  if (EQ (val, Qc))
    return make_number (FONT_SPACING_CHARCELL);
  if (EQ (val, Qm))
    return make_number (FONT_SPACING_MONO);
  if (EQ (val, Qp))
    return make_number (FONT_SPACING_PROPORTIONAL);
  return Qerror;
}

/* Structure of known font property keys and validater of the
   values.  */
struct
{
  /* Pointer to the key symbol.  */
  Lisp_Object *key;
  /* Function to validate the value VAL, or NULL if any value is ok.  */
  Lisp_Object (*validater) P_ ((enum font_property_index prop_index,
				Lisp_Object prop, Lisp_Object val));
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
    { &QClanguage, font_prop_validate_symbol },
    { &QCscript, font_prop_validate_symbol },
    { &QCdpi, font_prop_validate_non_neg },
    { &QCspacing, font_prop_validate_spacing },
    { &QCscalable, NULL },
    { &QCotf, font_prop_validate_symbol }
  };

#define FONT_PROPERTY_TABLE_SIZE \
  ((sizeof font_property_table) / (sizeof *font_property_table))

static int
get_font_prop_index (key, from)
     Lisp_Object key;
     int from;
{
  for (; from < FONT_PROPERTY_TABLE_SIZE; from++)
    if (EQ (key, *font_property_table[from].key))
      return from;
  return -1;
}

static Lisp_Object
font_prop_validate (spec)
     Lisp_Object spec;
{
  int i;
  Lisp_Object prop, val, extra;

  for (i = FONT_TYPE_INDEX; i < FONT_EXTRA_INDEX; i++)
    {
      if (! NILP (AREF (spec, i)))
	{
	  prop = *font_property_table[i].key;
	  val = (font_property_table[i].validater) (i, prop, AREF (spec, i));
	  if (EQ (val, Qerror))
	    Fsignal (Qfont, list2 (build_string ("invalid font property"),
				   Fcons (prop, AREF (spec, i))));
	  ASET (spec, i, val);
	}
    }
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object elt = XCAR (extra);

      prop = XCAR (elt);
      i = get_font_prop_index (prop, FONT_EXTRA_INDEX);
      if (i >= 0
	  && font_property_table[i].validater)
	{
	  val = (font_property_table[i].validater) (i, prop, XCDR (elt));
	  if (EQ (val, Qerror))
	    Fsignal (Qfont, list2 (build_string ("invalid font property"),
				   elt));
	  XSETCDR (elt, val);
	}
    }
  return spec;
}
      
static Lisp_Object
font_put_extra (font, prop, val)
     Lisp_Object font, prop, val;
{
  Lisp_Object extra = AREF (font, FONT_EXTRA_INDEX);
  Lisp_Object slot = (NILP (extra) ? Qnil : assq_no_quit (prop, extra));

  if (NILP (slot))
    {
      extra = Fcons (Fcons (prop, val), extra);
      ASET (font, FONT_EXTRA_INDEX, extra);
      return val;
    }
  XSETCDR (slot, val);
  return val;
}


/* Font name parser and unparser */

static Lisp_Object intern_font_field P_ ((char *, int));
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


/* Return a Lispy value of a XLFD font field at STR and LEN bytes.
   If LEN is zero, it returns `null_string'.
   If STR is "*", it returns nil.
   If all characters in STR are digits, it returns an integer.
   Otherwise, it returns a symbol interned from downcased STR.  */

static Lisp_Object
intern_font_field (str, len)
     char *str;
     int len;
{
  int i;

  if (len == 0)
    return null_string;
  if (*str == '*' && len == 1)
    return Qnil;
  if (isdigit (*str))
    {
      for (i = 1; i < len; i++)
	if (! isdigit (str[i]))
	  break;
      if (i == len)
	return make_number (atoi (str));
    }
  return intern_downcase (str, len);
}

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
	  else if (EQ (val, null_string))
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
		   && !NILP (prop_name_to_numeric (FONT_WEIGHT_INDEX, val)))
	    from = to = XLFD_WEIGHT_INDEX, mask = XLFD_WEIGHT_MASK;
	  else if (range_from <= XLFD_SLANT_INDEX
		   && range_to >= XLFD_SLANT_INDEX
		   && !NILP (prop_name_to_numeric (FONT_SLANT_INDEX, val)))
	    from = to = XLFD_SLANT_INDEX, mask = XLFD_SLANT_MASK;
	  else if (range_from <= XLFD_SWIDTH_INDEX
		   && range_to >= XLFD_SWIDTH_INDEX
		   && !NILP (prop_name_to_numeric (FONT_WIDTH_INDEX, val)))
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
   a fully specified XLFD, and we set FONT_EXTRA_INDEX of FONT to a
   symbol RESX-RESY-SPACING-AVGWIDTH.
*/

int
font_parse_xlfd (name, font)
     char *name;
     Lisp_Object font;
{
  int len = strlen (name);
  int i, j;
  Lisp_Object dpi, spacing;
  int avgwidth;
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
    if (*p == '-' && i < XLFD_LAST_INDEX)
      f[i++] = p + 1;
  f[i] = p;

  dpi = spacing = Qnil;
  avgwidth = -1;

  if (i == XLFD_LAST_INDEX)
    {
      int pixel_size;

      /* Fully specified XLFD.  */
      for (i = 0, j = FONT_FOUNDRY_INDEX; i < XLFD_WEIGHT_INDEX; i++, j++)
	{
	  val = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
	  if (! NILP (val))
	    ASET (font, j, val);
	}
      for (j = FONT_WEIGHT_INDEX; i < XLFD_ADSTYLE_INDEX; i++, j++)
	{
	  val = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
	  if (! NILP (val))
	    {
	      Lisp_Object numeric = prop_name_to_numeric (j, val);

	      if (INTEGERP (numeric))
		val = numeric;
	      ASET (font, j, val);
	    }
	}
      val = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
      if (! NILP (val))
	ASET (font, FONT_ADSTYLE_INDEX, val);
      i = XLFD_REGISTRY_INDEX;
      val = intern_font_field (f[i], f[i + 2] - f[i]);
      if (! NILP (val))
	ASET (font, FONT_REGISTRY_INDEX, val);

      p = f[XLFD_PIXEL_INDEX];
      if (*p == '[' && (pixel_size = parse_matrix (p)) >= 0)
	ASET (font, FONT_SIZE_INDEX, make_number (pixel_size));	      
      else
	{
	  i = XLFD_PIXEL_INDEX;
	  val = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
	  if (! NILP (val))
	    ASET (font, FONT_SIZE_INDEX, val);
	  else
	    {
	      double point_size = -1;

	      xassert (FONT_SPEC_P (font));
	      p = f[XLFD_POINT_INDEX];
	      if (*p == '[')
		point_size = parse_matrix (p);
	      else if (isdigit (*p))
		point_size = atoi (p), point_size /= 10;
	      if (point_size >= 0)
		ASET (font, FONT_SIZE_INDEX, make_float (point_size));
	      else
		{
		  i = XLFD_PIXEL_INDEX;
		  val = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
		  if (! NILP (val))
		    ASET (font, FONT_SIZE_INDEX, val);
		}
	    }
	}

      /* Parse RESX, RESY, SPACING, and AVGWIDTH.  */
      if (FONT_ENTITY_P (font))
	{
	  i = XLFD_RESX_INDEX;
	  ASET (font, FONT_EXTRA_INDEX,
		intern_font_field (f[i], f[XLFD_REGISTRY_INDEX] - 1 - f[i]));
	  return 0;
	}

      /* Here we just setup DPI, SPACING, and AVGWIDTH.  They are set
	 in FONT_EXTRA_INDEX later.  */
      i = XLFD_RESX_INDEX;
      dpi = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
      i = XLFD_SPACING_INDEX;
      spacing = intern_font_field (f[i], f[i + 1] - 1 - f[i]);
      p = f[XLFD_AVGWIDTH_INDEX];
      if (*p == '~')
	p++;
      if (isdigit (*p))
	avgwidth = atoi (p);
    }
  else
    {
      int wild_card_found = 0;
      Lisp_Object prop[XLFD_LAST_INDEX];

      for (j = 0; j < i; j++)
	{
	  if (*f[j] == '*')
	    {
	      if (f[j][1] && f[j][1] != '-')
		return -1;
	      prop[j] = Qnil;
	      wild_card_found = 1;
	    }
	  else if (isdigit (*f[j]))
	    {
	      for (p = f[j] + 1; isdigit (*p); p++);
	      if (*p && *p != '-')
		prop[j] = intern_downcase (f[j], p - f[j]);
	      else
		prop[j] = make_number (atoi (f[j]));
	    }
	  else if (j + 1 < i)
	    prop[j] = intern_font_field (f[j], f[j + 1] - 1 - f[j]);
	  else
	    prop[j] = intern_font_field (f[j], f[i] - f[j]);
	}
      if (! wild_card_found)
	return -1;
      if (font_expand_wildcards (prop, i) < 0)
	return -1;

      for (i = 0, j = FONT_FOUNDRY_INDEX; i < XLFD_WEIGHT_INDEX; i++, j++)
	if (! NILP (prop[i]))
	  ASET (font, j, prop[i]);
      for (j = FONT_WEIGHT_INDEX; i < XLFD_ADSTYLE_INDEX; i++, j++)
	if (! NILP (prop[i]))
	  ASET (font, j, prop[i]);
      if (! NILP (prop[XLFD_ADSTYLE_INDEX]))
	ASET (font, FONT_ADSTYLE_INDEX, prop[XLFD_ADSTYLE_INDEX]);
      val = prop[XLFD_REGISTRY_INDEX];
      if (NILP (val))
	{
	  val = prop[XLFD_ENCODING_INDEX];
	  if (! NILP (val))
	    val = Fintern (concat2 (build_string ("*-"), SYMBOL_NAME (val)),
			   Qnil);
	}
      else if (NILP (prop[XLFD_ENCODING_INDEX]))
	val = Fintern (concat2 (SYMBOL_NAME (val), build_string ("-*")),
		       Qnil);
      else
	val = Fintern (concat3 (SYMBOL_NAME (val), build_string ("-"),
				SYMBOL_NAME (prop[XLFD_ENCODING_INDEX])),
		       Qnil);
      if (! NILP (val))
	ASET (font, FONT_REGISTRY_INDEX, val);

      if (INTEGERP (prop[XLFD_PIXEL_INDEX]))
	ASET (font, FONT_SIZE_INDEX, prop[XLFD_PIXEL_INDEX]);
      else if (INTEGERP (prop[XLFD_POINT_INDEX]))
	{
	  double point_size = XINT (prop[XLFD_POINT_INDEX]);

	  ASET (font, FONT_SIZE_INDEX, make_float (point_size / 10));
	}

      dpi = prop[XLFD_RESX_INDEX];
      spacing = prop[XLFD_SPACING_INDEX];
      if (INTEGERP (prop[XLFD_AVGWIDTH_INDEX]))
	avgwidth = XINT (prop[XLFD_AVGWIDTH_INDEX]);
    }

  if (! NILP (dpi))
    font_put_extra (font, QCdpi, dpi);
  if (! NILP (spacing))
    font_put_extra (font, QCspacing, spacing);
  if (avgwidth >= 0)
    font_put_extra (font, QCscalable, avgwidth == 0 ? Qt : Qnil);

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

  xassert (FONTP (font));

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
      val = AREF (font, i);
      if (NILP (val))
	f[j] = "*", len += 2;
      else
	{
	  if (INTEGERP (val))
	    val = prop_numeric_to_name (i, XINT (val));
	  if (SYMBOLP (val))
	    val = SYMBOL_NAME (val);
	  xassert (STRINGP (val));
	  f[j] = (char *) SDATA (val), len += SBYTES (val) + 1;
	}
    }

  val = AREF (font, FONT_SIZE_INDEX);
  xassert (NUMBERP (val) || NILP (val));
  if (INTEGERP (val))
    {
      f[XLFD_PIXEL_INDEX] = alloca (22);
      i = XINT (val);
      if (i > 0)
	len += sprintf (f[XLFD_PIXEL_INDEX], "%d-*", i) + 1;
      else 			/* i == 0 */
	len += sprintf (f[XLFD_PIXEL_INDEX], "%d-*", pixel_size) + 1;
    }
  else if (FLOATP (val))
    {
      f[XLFD_PIXEL_INDEX] = alloca (12);
      i = XFLOAT_DATA (val) * 10;
      len += sprintf (f[XLFD_PIXEL_INDEX], "*-%d", i) + 1;
    }
  else
    f[XLFD_PIXEL_INDEX] = "*-*", len += 4;

  val = AREF (font, FONT_EXTRA_INDEX);

  if (FONT_ENTITY_P (font)
      && EQ (AREF (font, FONT_TYPE_INDEX), Qx))
    {
      /* Setup names for RESX-RESY-SPACING-AVWIDTH.  */
      if (SYMBOLP (val) && ! NILP (val))
	{
	  val = SYMBOL_NAME (val);
	  f[XLFD_RESX_INDEX] = (char *) SDATA (val), len += SBYTES (val) + 1;
	}
      else
	f[XLFD_RESX_INDEX] = "*-*-*-*", len += 6;
    }
  else
    {
      Lisp_Object dpi = assq_no_quit (QCdpi, val);
      Lisp_Object spacing = assq_no_quit (QCspacing, val);
      Lisp_Object scalable = assq_no_quit (QCscalable, val);

      if (CONSP (dpi) || CONSP (spacing) || CONSP (scalable))
	{
	  char *str = alloca (24);
	  int this_len;

	  if (CONSP (dpi) && INTEGERP (XCDR (dpi)))
	    this_len = sprintf (str, "%d-%d",
				XINT (XCDR (dpi)), XINT (XCDR (dpi)));
	  else
	    this_len = sprintf (str, "*-*");
	  if (CONSP (spacing) && ! NILP (XCDR (spacing)))
	    {
	      val = XCDR (spacing);
	      if (INTEGERP (val))
		{
		  if (XINT (val) < FONT_SPACING_MONO)
		    val = Qp;
		  else if (XINT (val) < FONT_SPACING_CHARCELL)
		    val = Qm;
		  else
		    val = Qc;
		}
	      xassert (SYMBOLP (val));
	      this_len += sprintf (str + this_len, "-%c",
				   SDATA (SYMBOL_NAME (val))[0]);
	    }
	  else
	    this_len += sprintf (str + this_len, "-*");
	  if (CONSP (scalable) && ! NILP (XCDR (spacing)))
	    this_len += sprintf (str + this_len, "-0");
	  else
	    this_len += sprintf (str + this_len, "-*");
	  f[XLFD_RESX_INDEX] = str;
	  len += this_len;
	}
      else
	f[XLFD_RESX_INDEX] = "*-*-*-*", len += 8;
    }

  len++;	/* for terminating '\0'.  */
  if (len >= nbytes)
    return -1;
  return sprintf (name, "-%s-%s-%s-%s-%s-%s-%s-%s-%s",
		  f[XLFD_FOUNDRY_INDEX], f[XLFD_FAMILY_INDEX],
		  f[XLFD_WEIGHT_INDEX], f[XLFD_SLANT_INDEX],
		  f[XLFD_SWIDTH_INDEX],
		  f[XLFD_ADSTYLE_INDEX], f[XLFD_PIXEL_INDEX],
		  f[XLFD_RESX_INDEX], f[XLFD_REGISTRY_INDEX]);
}

/* Parse NAME (null terminated) as Fonconfig's name format and store
   information in FONT (font-spec or font-entity).  If NAME is
   successfully parsed, return 0.  Otherwise return -1.  */

int
font_parse_fcname (name, font)
     char *name;
     Lisp_Object font;
{
  char *p0, *p1;
  int len = strlen (name);
  char *copy;

  if (len == 0)
    return -1;
  /* It is assured that (name[0] && name[0] != '-').  */
  if (name[0] == ':')
    p0 = name;
  else
    {
      Lisp_Object family;
      double point_size;

      for (p0 = name + 1; *p0 && (*p0 != '-' && *p0 != ':'); p0++)
	if (*p0 == '\\' && p0[1])
	  p0++;
      family = intern_font_field (name, p0 - name);
      if (*p0 == '-')
	{
	  if (! isdigit (p0[1]))
	    return -1;
	  point_size = strtod (p0 + 1, &p1);
	  if (*p1 && *p1 != ':')
	    return -1;
	  ASET (font, FONT_SIZE_INDEX, make_float (point_size));
	  p0 = p1;
	}
      ASET (font, FONT_FAMILY_INDEX, family);
    }

  len -= p0 - name;
  copy = alloca (len + 1);
  if (! copy)
    return -1;
  name = copy;
  
  /* Now parse ":KEY=VAL" patterns.  Store known keys and values in
     extra, copy unknown ones to COPY.  */
  while (*p0)
    {
      Lisp_Object key, val;
      int prop;

      for (p1 = p0 + 1; *p1 && *p1 != '=' && *p1 != ':'; p1++);
      if (*p1 != '=')
	{
	  /* Must be an enumerated value.  */
	  val = intern_font_field (p0 + 1, p1 - p0 - 1);
	  if (memcmp (p0 + 1, "light", 5) == 0
	      || memcmp (p0 + 1, "medium", 6) == 0
	      || memcmp (p0 + 1, "demibold", 8) == 0
	      || memcmp (p0 + 1, "bold", 4) == 0
	      || memcmp (p0 + 1, "black", 5) == 0)
	    {
	      ASET (font, FONT_WEIGHT_INDEX, val);
	    }
	  else if (memcmp (p0 + 1, "roman", 5) == 0
		   || memcmp (p0 + 1, "italic", 6) == 0
		   || memcmp (p0 + 1, "oblique", 7) == 0)
	    {
	      ASET (font, FONT_SLANT_INDEX, val);
	    }
	  else if (memcmp (p0 + 1, "charcell", 8) == 0
		   || memcmp (p0 + 1, "mono", 4) == 0
		   || memcmp (p0 + 1, "proportional", 12) == 0)
	    {
	      font_put_extra (font, QCspacing,
			      (p0[1] == 'c' ? Qc : p0[1] == 'm' ? Qm : Qp));
	    }
	  else
	    {
	      /* unknown key */
	      bcopy (p0, copy, p1 - p0);
	      copy += p1 - p0;
	    }
	}
      else
	{
	  if (memcmp (p0 + 1, "pixelsize=", 10) == 0)
	    prop = FONT_SIZE_INDEX;
	  else
	    {
	      key = intern_font_field (p0, p1 - p0);
	      prop = get_font_prop_index (key, 0);
	    }
	  p0 = p1 + 1;
	  for (p1 = p0; *p1 && *p1 != ':'; p1++);
	  val = intern_font_field (p0, p1 - p0);
	  if (! NILP (val))
	    {
	      if (prop >= 0 && prop < FONT_EXTRA_INDEX)
		{
		  ASET (font, prop, val);
		}
	      else
		font_put_extra (font, key, val);
	    }
	}
      p0 = p1;
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
  Lisp_Object val;
  int point_size;
  int dpi, spacing, scalable;
  int i, len = 1;
  char *p;
  Lisp_Object styles[3];
  char *style_names[3] = { "weight", "slant", "width" };

  val = AREF (font, FONT_FAMILY_INDEX);
  if (SYMBOLP (val) && ! NILP (val))
    len += SBYTES (SYMBOL_NAME (val));

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

  val = AREF (font, FONT_FOUNDRY_INDEX);
  if (SYMBOLP (val) && ! NILP (val))
    /* ":foundry=NAME" */
    len += 9 + SBYTES (SYMBOL_NAME (val));

  for (i = FONT_WEIGHT_INDEX; i <= FONT_WIDTH_INDEX; i++)
    {
      val = AREF (font, i);
      if (INTEGERP (val))
	{
	  val = prop_numeric_to_name (i, XINT (val));
	  len += (strlen (style_names[i - FONT_WEIGHT_INDEX])
		  + 2 + SBYTES (SYMBOL_NAME (val))); /* :xxx=NAME */
	}
      styles[i - FONT_WEIGHT_INDEX] = val;
    }

  val = AREF (font, FONT_EXTRA_INDEX);
  if (FONT_ENTITY_P (font)
      && EQ (AREF (font, FONT_TYPE_INDEX), Qx))
    {
      char *p;

      /* VAL is a symbol of name `RESX-RESY-SPACING-AVWIDTH'.  */
      p = (char *) SDATA (SYMBOL_NAME (val));
      dpi = atoi (p);
      for (p++; *p != '-'; p++);	/* skip RESX */
      for (p++; *p != '-'; p++);	/* skip RESY */
      spacing = (*p == 'c' ? FONT_SPACING_CHARCELL
		 : *p == 'm' ? FONT_SPACING_MONO
		 : FONT_SPACING_PROPORTIONAL);
      for (p++; *p != '-'; p++);	/* skip SPACING */
      scalable = (atoi (p) == 0);
      /* The longest pattern is ":dpi=NUM:scalable=False:spacing=100" */
      len += 42;
    }
  else
    {
      Lisp_Object elt;

      dpi = spacing = scalable = -1;
      elt = assq_no_quit (QCdpi, val);
      if (CONSP (elt))
	dpi = XINT (XCDR (elt)), len += 15; /* for ":dpi=NUM" */
      elt = assq_no_quit (QCspacing, val);
      if (CONSP (elt))
	spacing = XINT (XCDR (elt)), len += 12; /* for ":spacing=100" */
      elt = assq_no_quit (QCscalable, val);
      if (CONSP (elt))
	scalable = ! NILP (XCDR (elt)), len += 15; /* for ":scalable=False" */
    }

  if (len > nbytes)
    return -1;
  p = name;
  if (! NILP (AREF (font, FONT_FAMILY_INDEX)))
    p += sprintf(p, "%s",
		 SDATA (SYMBOL_NAME (AREF (font, FONT_FAMILY_INDEX))));
  if (point_size > 0)
    {
      if (p == name)
	p += sprintf (p, "%d", point_size);
      else
	p += sprintf (p, "-%d", point_size);
    }
  else if (pixel_size > 0)
    p += sprintf (p, ":pixelsize=%d", pixel_size);
  if (SYMBOLP (AREF (font, FONT_FOUNDRY_INDEX))
      && ! NILP (AREF (font, FONT_FOUNDRY_INDEX)))
    p += sprintf (p, ":foundry=%s",
		  SDATA (SYMBOL_NAME (AREF (font, FONT_FOUNDRY_INDEX))));
  for (i = 0; i < 3; i++)
    if (SYMBOLP (styles[i]) && ! NILP (styles [i]))
      p += sprintf (p, ":%s=%s", style_names[i],
		    SDATA (SYMBOL_NAME (styles [i])));
  if (dpi >= 0)
    p += sprintf (p, ":dpi=%d", dpi);
  if (spacing >= 0)
    p += sprintf (p, ":spacing=%d", spacing);
  if (scalable > 0)
    p += sprintf (p, ":scalable=True");
  else if (scalable == 0)
    p += sprintf (p, ":scalable=False");
  return (p - name);
}

/* Parse NAME (null terminated) and store information in FONT
   (font-spec or font-entity).  If NAME is successfully parsed, return
   0.  Otherwise return -1.

   If NAME is XLFD and FONT is a font-entity, store
   RESX-RESY-SPACING-AVWIDTH information as a symbol in
   FONT_EXTRA_INDEX.  */

static int
font_parse_name (name, font)
     char *name;
     Lisp_Object font;
{
  if (name[0] == '-' || index (name, '*'))
    return font_parse_xlfd (name, font);
  return font_parse_fcname (name, font);
}

void
font_merge_old_spec (name, family, registry, spec)
     Lisp_Object name, family, registry, spec;
{
  if (STRINGP (name))
    {
      if (font_parse_xlfd ((char *) SDATA (name), spec) < 0)
	{
	  Lisp_Object extra = Fcons (Fcons (QCname, name), Qnil);

	  ASET (spec, FONT_EXTRA_INDEX, extra);
	}
    }
  else
    {
      if (! NILP (family))
	{
	  int len;
	  char *p0, *p1;

	  xassert (STRINGP (family));
	  len = SBYTES (family);
	  p0 = (char *) SDATA (family);
	  p1 = index (p0, '-');
	  if (p1)
	    {
	      if ((*p0 != '*' || p1 - p0 > 1)
		  && NILP (AREF (spec, FONT_FOUNDRY_INDEX)))
		ASET (spec, FONT_FOUNDRY_INDEX,
		      intern_downcase (p0, p1 - p0));
	      if (NILP (AREF (spec, FONT_FAMILY_INDEX)))
		ASET (spec, FONT_FAMILY_INDEX,
		      intern_downcase (p1 + 1, len - (p1 + 1 - p0)));
	    }
	  else if (NILP (AREF (spec, FONT_FAMILY_INDEX)))
	    ASET (spec, FONT_FAMILY_INDEX, intern_downcase (p0, len));
	}
      if (! NILP (registry)
	  && NILP (AREF (spec, FONT_REGISTRY_INDEX)))
	ASET (spec, FONT_REGISTRY_INDEX,
	      intern_downcase ((char *) SDATA (registry), SBYTES (registry)));
    }
}

static Lisp_Object
font_lispy_object (font)
     struct font *font;
{
  Lisp_Object objlist = AREF (font->entity, FONT_OBJLIST_INDEX);

  for (; ! NILP (objlist); objlist = XCDR (objlist))
    {
      struct Lisp_Save_Value *p = XSAVE_VALUE (XCAR (objlist));

      if (font == (struct font *) p->pointer)
	break;
    }
  xassert (! NILP (objlist));
  return XCAR (objlist);
}

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
  if (! NILP (LGSTRING_LBEARING (gstring)))
    CHECK_NUMBER (LGSTRING_LBEARING (gstring));
  if (! NILP (LGSTRING_RBEARING (gstring)))
    CHECK_NUMBER (LGSTRING_RBEARING (gstring));
  if (! NILP (LGSTRING_WIDTH (gstring)))
    CHECK_NATNUM (LGSTRING_WIDTH (gstring));
  if (! NILP (LGSTRING_ASCENT (gstring)))
    CHECK_NUMBER (LGSTRING_ASCENT (gstring));
  if (! NILP (LGSTRING_DESCENT (gstring)))
    CHECK_NUMBER (LGSTRING_DESCENT(gstring));

  for (i = 0; i < LGSTRING_LENGTH (gstring); i++)
    {
      val = LGSTRING_GLYPH (gstring, i);
      CHECK_VECTOR (val);
      if (ASIZE (val) < LGSTRING_GLYPH_SIZE)
	goto err;
      if (NILP (LGLYPH_CHAR (val)))
	break;
      CHECK_NATNUM (LGLYPH_FROM (val));
      CHECK_NATNUM (LGLYPH_TO (val));
      CHECK_CHARACTER (LGLYPH_CHAR (val));
      if (! NILP (LGLYPH_CODE (val)))
	CHECK_NATNUM (LGLYPH_CODE (val));
      if (! NILP (LGLYPH_WIDTH (val)))
	CHECK_NATNUM (LGLYPH_WIDTH (val));
      if (! NILP (LGLYPH_ADJUSTMENT (val)))
	{
	  val = LGLYPH_ADJUSTMENT (val);
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


/* OTF handler */

static void
check_otf_features (otf_features)
     Lisp_Object otf_features;
{
  Lisp_Object val, elt;

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
otf_open (entity, file)
     Lisp_Object entity;
     char *file;
{
  Lisp_Object val = Fassoc (entity, otf_list);
  OTF *otf;

  if (! NILP (val))
    otf = XSAVE_VALUE (XCDR (val))->pointer;
  else
    {
      otf = file ? OTF_open (file) : NULL;
      val = make_save_value (otf, 0);
      otf_list = Fcons (Fcons (entity, val), otf_list);
    }
  return otf;
}


/* Return a list describing which scripts/languages FONT supports by
   which GSUB/GPOS features of OpenType tables.  See the comment of
   (sturct font_driver).otf_capability.  */

Lisp_Object
font_otf_capability (font)
     struct font *font;
{
  OTF *otf;
  Lisp_Object capability = Fcons (Qnil, Qnil);
  int i;

  otf = otf_open (font->entity, font->file_name);
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
  char *p, *pend;
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

#define DEVICE_DELTA(table, size)				\
  (((size) >= (table).StartSize && (size) <= (table).EndSize)	\
   ? (table).DeltaValue[(size) - (table).StartSize]		\
   : 0)

void
adjust_anchor (struct font *font, OTF_Anchor *anchor,
	       unsigned code, int size, int *x, int *y)
{
  if (anchor->AnchorFormat == 2)
    {
      int x0, y0;

      if (font->driver->anchor_point (font, code, anchor->f.f1.AnchorPoint,
				      &x0, &y0) >= 0)
	*x = x0, *y = y0;
    }
  else if (anchor->AnchorFormat == 3)
    {
      if (anchor->f.f2.XDeviceTable.offset)
	*x += DEVICE_DELTA (anchor->f.f2.XDeviceTable, size);
      if (anchor->f.f2.YDeviceTable.offset)
	*y += DEVICE_DELTA (anchor->f.f2.YDeviceTable, size);
    }
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
    ASET (val, 0, value_record->XPlacement);
  if (value_format & OTF_YPlacement)
    ASET (val, 1, value_record->YPlacement);
  if (value_format & OTF_XAdvance)
    ASET (val, 2, value_record->XAdvance);
  if (value_format & OTF_YAdvance)
    ASET (val, 3, value_record->YAdvance);
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

#define REPLACEMENT_CHARACTER 0xFFFD

/* Drive FONT's OpenType FEATURES.  See the comment of (sturct
   font_driver).drive_otf.  */

int
font_drive_otf (font, otf_features, gstring_in, from, to, gstring_out, idx,
	       alternate_subst)
     struct font *font;
     Lisp_Object otf_features;
     Lisp_Object gstring_in;
     int from, to;
     Lisp_Object gstring_out;
     int idx, alternate_subst;
{
  Lisp_Object val;
  int len;
  int i;
  OTF *otf;
  OTF_GlyphString otf_gstring;
  OTF_Glyph *g;
  char *script, *langsys = NULL, *gsub_features = NULL, *gpos_features = NULL;
  int need_cmap;

  val = XCAR (otf_features);
  script = SDATA (SYMBOL_NAME (val));
  otf_features = XCDR (otf_features);
  val = XCAR (otf_features);
  langsys = NILP (val) ? NULL : SDATA (SYMBOL_NAME (val));
  otf_features = XCDR (otf_features);
  val = XCAR (otf_features);
  if (! NILP (val))
    {
      gsub_features = alloca (XINT (Flength (val)) * 6);
      generate_otf_features (val, &script, &langsys, gsub_features);
    }
  otf_features = XCDR (otf_features);
  val = XCAR (otf_features);
  if (! NILP (val))
    {
      gpos_features = alloca (XINT (Flength (val)) * 6);
      generate_otf_features (val, &script, &langsys, gpos_features);
    }

  otf = otf_open (font->entity, font->file_name);
  if (! otf)
    return 0;
  if (OTF_get_table (otf, "head") < 0)
    return 0;
  if (OTF_get_table (otf, "cmap") < 0)
    return 0;
  if ((! gsub_features || OTF_check_table (otf, "GSUB") < 0)
      && (! gpos_features || OTF_check_table (otf, "GPOS") < 0))
    return 0;

  len = to - from;
  otf_gstring.size = otf_gstring.used = len;
  otf_gstring.glyphs = (OTF_Glyph *) malloc (sizeof (OTF_Glyph) * len);
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * len);
  for (i = 0, need_cmap = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring_in, from + i);

      otf_gstring.glyphs[i].c = XINT (LGLYPH_CHAR (g));
      if (otf_gstring.glyphs[i].c == REPLACEMENT_CHARACTER)
	otf_gstring.glyphs[i].c = 0;
      if (NILP (LGLYPH_CODE (g)))
	{
	  otf_gstring.glyphs[i].glyph_id = 0;
	  need_cmap = 1;
	}
      else
	otf_gstring.glyphs[i].glyph_id = XINT (LGLYPH_CODE (g));
    }
  if (need_cmap)
    OTF_drive_cmap (otf, &otf_gstring);
  OTF_drive_gdef (otf, &otf_gstring);

  if (gsub_features)
    {
      if ((alternate_subst
	   ? OTF_drive_gsub_alternate (otf, &otf_gstring, script, langsys,
				       gsub_features)
	   : OTF_drive_gsub (otf, &otf_gstring, script, langsys,
			     gsub_features)) < 0)
	{
	  free (otf_gstring.glyphs);
	  return 0;
	}
      if (ASIZE (gstring_out) < idx + otf_gstring.used)
	{
	  free (otf_gstring.glyphs);
	  return -1;
	}
      for (i = 0, g = otf_gstring.glyphs; i < otf_gstring.used;)
	{
	  int i0 = g->f.index.from, i1 = g->f.index.to;
	  Lisp_Object glyph = LGSTRING_GLYPH (gstring_in, from + i0);
	  Lisp_Object min_idx = AREF (glyph, 0);
	  Lisp_Object max_idx = AREF (glyph, 1);

	  if (i0 < i1)
	    {
	      int min_idx_i = XINT (min_idx), max_idx_i = XINT (max_idx);

	      for (i0++; i0 <= i1; i0++)
		{
		  glyph = LGSTRING_GLYPH (gstring_in, from + i0);
		  if (min_idx_i > XINT (AREF (glyph, 0)))
		    min_idx_i = XINT (AREF (glyph, 0));
		  if (max_idx_i < XINT (AREF (glyph, 1)))
		    max_idx_i = XINT (AREF (glyph, 1));
		}
	      min_idx = make_number (min_idx_i);
	      max_idx = make_number (max_idx_i);
	      i0 = g->f.index.from;
	    }
	  for (; i < otf_gstring.used && g->f.index.from == i0; i++, g++)
	    {
	      glyph = LGSTRING_GLYPH (gstring_out, idx + i);
	      ASET (glyph, 0, min_idx);
	      ASET (glyph, 1, max_idx);
	      if (g->c > 0)
		LGLYPH_SET_CHAR (glyph, make_number (g->c));
	      else
		LGLYPH_SET_CHAR (glyph, make_number (REPLACEMENT_CHARACTER));
	      LGLYPH_SET_CODE (glyph, make_number (g->glyph_id));
	    }
	}
    }

  if (gpos_features)
    {
      Lisp_Object glyph;
      int u = otf->head->unitsPerEm;
      int size = font->pixel_size;
      Lisp_Object base = Qnil, mark = Qnil;

      if (OTF_drive_gpos (otf, &otf_gstring, script, langsys,
			  gpos_features) < 0)
	{
	  free (otf_gstring.glyphs);
	  return 0;
	}
      for (i = 0, g = otf_gstring.glyphs; i < otf_gstring.used; i++, g++)
	{
	  Lisp_Object prev;
	  int xoff = 0, yoff = 0, width_adjust = 0;

	  if (! g->glyph_id)
	    continue;

	  switch (g->positioning_type)
	    {
	    case 0:
	      break;
	    case 1: case 2:
	      {
		int format = g->f.f1.format;

		if (format & OTF_XPlacement)
		  xoff = g->f.f1.value->XPlacement * size / u;
		if (format & OTF_XPlaDevice)
		  xoff += DEVICE_DELTA (g->f.f1.value->XPlaDevice, size);
		if (format & OTF_YPlacement)
		  yoff = - (g->f.f1.value->YPlacement * size / u);
		if (format & OTF_YPlaDevice)
		  yoff -= DEVICE_DELTA (g->f.f1.value->YPlaDevice, size);
		if (format & OTF_XAdvance)
		  width_adjust += g->f.f1.value->XAdvance * size / u;
		if (format & OTF_XAdvDevice)
		  width_adjust += DEVICE_DELTA (g->f.f1.value->XAdvDevice, size);
	      }
	      break;
	    case 3:
	      /* Not yet supported.  */
	      break;
	    case 4: case 5:
	      if (NILP (base))
		break;
	      prev = base;
	      goto label_adjust_anchor;
	    default:		/* i.e. case 6 */
	      if (NILP (mark))
		break;
	      prev = mark;

	    label_adjust_anchor:
	      {
		int base_x, base_y, mark_x, mark_y, width;
		unsigned code;

		base_x = g->f.f4.base_anchor->XCoordinate * size / u;
		base_y = g->f.f4.base_anchor->YCoordinate * size / u;
		mark_x = g->f.f4.mark_anchor->XCoordinate * size / u;
		mark_y = g->f.f4.mark_anchor->YCoordinate * size / u;

		code = XINT (LGLYPH_CODE (prev));
		if (g->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (font, g->f.f4.base_anchor,
				 code, size, &base_x, &base_y);
		if (g->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (font, g->f.f4.mark_anchor,
				 code, size, &mark_x, &mark_y);

		if (NILP (LGLYPH_WIDTH (prev)))
		  {
		    width = font->driver->text_extents (font, &code, 1, NULL);
		    LGLYPH_SET_WIDTH (prev, make_number (width));
		  }
		else
		  width = XINT (LGLYPH_WIDTH (prev));
		xoff = XINT (LGLYPH_XOFF (prev)) + (base_x - width) - mark_x;
		yoff = XINT (LGLYPH_YOFF (prev)) + mark_y - base_y;
	      }
	    }
	  if (xoff || yoff || width_adjust)
	    {
	      Lisp_Object adjustment = Fmake_vector (make_number (3), Qnil);

	      ASET (adjustment, 0, make_number (xoff));
	      ASET (adjustment, 1, make_number (yoff));
	      ASET (adjustment, 2, make_number (width_adjust));
	      LGLYPH_SET_ADJUSTMENT (glyph, adjustment);
	    }
	  if (g->GlyphClass == OTF_GlyphClass0)
	    base = mark = glyph;
	  else if (g->GlyphClass == OTF_GlyphClassMark)
	    mark = glyph;
	  else
	    base = glyph;
	}
    }

  free (otf_gstring.glyphs);  
  return i;
}

#endif	/* HAVE_LIBOTF */


/* G-string (glyph string) handler */

/* G-string is a vector of the form [HEADER GLYPH ...].
   See the docstring of `font-make-gstring' for more detail.  */

struct font *
font_prepare_composition (cmp)
     struct composition *cmp;
{
  Lisp_Object gstring
    = AREF (XHASH_TABLE (composition_hash_table)->key_and_value,
	    cmp->hash_index * 2);
  struct font *font = XSAVE_VALUE (LGSTRING_FONT (gstring))->pointer;
  int len = LGSTRING_LENGTH (gstring);
  int i;

  cmp->font = font;
  cmp->lbearing = cmp->rbearing = cmp->pixel_width = 0;
  cmp->ascent = font->ascent;
  cmp->descent = font->descent;

  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring, i);
      unsigned code;
      struct font_metrics metrics;

      if (NILP (LGLYPH_FROM (g)))
	break;
      code = XINT (LGLYPH_CODE (g));
      font->driver->text_extents (font, &code, 1, &metrics);
      LGLYPH_SET_WIDTH (g, make_number (metrics.width));
      metrics.lbearing += LGLYPH_XOFF (g);
      metrics.rbearing += LGLYPH_XOFF (g);
      metrics.ascent += LGLYPH_YOFF (g);
      metrics.descent += LGLYPH_YOFF (g);

      if (cmp->lbearing > cmp->pixel_width + metrics.lbearing)
	cmp->lbearing = cmp->pixel_width + metrics.lbearing;
      if (cmp->rbearing < cmp->pixel_width + metrics.rbearing)
	cmp->rbearing = cmp->pixel_width + metrics.rbearing;
      if (cmp->ascent < metrics.ascent)
	cmp->ascent = metrics.ascent;
      if (cmp->descent < metrics.descent)
	cmp->descent = metrics.descent;
      cmp->pixel_width += metrics.width + LGLYPH_WADJUST (g);
    }
  cmp->glyph_len = i;
  LGSTRING_SET_LBEARING (gstring, make_number (cmp->lbearing));
  LGSTRING_SET_RBEARING (gstring, make_number (cmp->rbearing));
  LGSTRING_SET_WIDTH (gstring, make_number (cmp->pixel_width));
  LGSTRING_SET_ASCENT (gstring, make_number (cmp->ascent));
  LGSTRING_SET_DESCENT (gstring, make_number (cmp->descent));

  return font;
}

int
font_gstring_produce (old, from, to, new, idx, code, n)
     Lisp_Object old;
     int from, to;
     Lisp_Object new;
     int idx;
     unsigned *code;
     int n;
{
  Lisp_Object min_idx, max_idx;
  int i;

  if (idx + n > ASIZE (new))
    return -1;
  if (from == to)
    {
      if (from == 0)
	{
	  min_idx = make_number (0);
	  max_idx = make_number (1);
	}
      else
	{
	  min_idx = AREF (AREF (old, from - 1), 0);
	  max_idx = AREF (AREF (old, from - 1), 1);
	}
    }
  else if (from + 1 == to)
    {
      min_idx = AREF (AREF (old, from), 0);
      max_idx = AREF (AREF (old, from), 1);
    }
  else
    {
      int min_idx_i = XINT (AREF (AREF (old, from), 0));
      int max_idx_i = XINT (AREF (AREF (old, from), 1));

      for (i = from + 1; i < to; i++)
	{
	  if (min_idx_i > XINT (AREF (AREF (old, i), 0)))
	    min_idx_i = XINT (AREF (AREF (old, i), 0));
	  if (max_idx_i < XINT (AREF (AREF (old, i), 1)))
	    max_idx_i = XINT (AREF (AREF (old, i), 1));
	}
      min_idx = make_number (min_idx_i);
      max_idx = make_number (max_idx_i);
    }

  for (i = 0; i < n; i++)
    {
      ASET (AREF (new, idx + i), 0, min_idx);
      ASET (AREF (new, idx + i), 1, max_idx);
      ASET (AREF (new, idx + i), 2, make_number (code[i]));
    }

  return 0;
}

/* Font sorting */

static unsigned font_score P_ ((Lisp_Object, Lisp_Object *));
static int font_compare P_ ((const void *, const void *));
static Lisp_Object font_sort_entites P_ ((Lisp_Object, Lisp_Object,
					  Lisp_Object, Lisp_Object));

/* We sort fonts by scoring each of them against a specified
   font-spec.  The score value is 32 bit (`unsigned'), and the smaller
   the value is, the closer the font is to the font-spec.

   Each 1-bit in the highest 4 bits of the score is used for atomic
   properties FOUNDRY, FAMILY, ADSTYLE, and REGISTRY.

   Each 7-bit in the lowest 28 bits are used for numeric properties
   WEIGHT, SLANT, WIDTH, and SIZE.  */

/* How many bits to shift to store the difference value of each font
   property in a score.  */
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
  /* Score four atomic fields.  Maximum difference is 1. */
  for (i = FONT_FOUNDRY_INDEX; i <= FONT_REGISTRY_INDEX; i++)
    if (! NILP (spec_prop[i])
	&& ! EQ (spec_prop[i], AREF (entity, i)))
      score |= 1 << sort_shift_bits[i];

  /* Score four numeric fields.  Maximum difference is 127. */
  for (i = FONT_WEIGHT_INDEX; i <= FONT_SIZE_INDEX; i++)
    {
      Lisp_Object entity_val = AREF (entity, i);

      if (! NILP (spec_prop[i]) && ! EQ (spec_prop[i], entity_val))
	{
	  if (! INTEGERP (entity_val))
	    score |= 127 << sort_shift_bits[i];
	  else
	    {
	      int diff = XINT (entity_val) - XINT (spec_prop[i]);

	      if (diff < 0)
		diff = - diff;
	      if (i == FONT_SIZE_INDEX)
		{
		  if (XINT (entity_val) > 0
		      && diff > FONT_PIXEL_SIZE_QUANTUM)
		    score |= min (diff, 127) << sort_shift_bits[i];
		}
	      else
		score |= min (diff, 127) << sort_shift_bits[i];
	    }
	}
    }

  return score;
}


/* The comparison function for qsort.  */

static int
font_compare (d1, d2)
     const void *d1, *d2;
{
  return (*(unsigned *) d1 < *(unsigned *) d2
	  ? -1 : *(unsigned *) d1 > *(unsigned *) d2);
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
   of FRAME before sorting.  If SPEC is not nil, it is a font-spec to
   get the font-entities in VEC.  */

static Lisp_Object
font_sort_entites (vec, prefer, frame, spec)
     Lisp_Object vec, prefer, frame, spec;
{
  Lisp_Object prefer_prop[FONT_SPEC_MAX];
  int len, i;
  struct font_sort_data *data;
  USE_SAFE_ALLOCA;

  len = ASIZE (vec);
  if (len <= 1)
    return vec;

  for (i = FONT_FOUNDRY_INDEX; i <= FONT_SIZE_INDEX; i++)
    prefer_prop[i] = AREF (prefer, i);

  if (! NILP (spec))
    {
      /* As it is assured that all fonts in VEC match with SPEC, we
	 should ignore properties specified in SPEC.  So, set the
	 corresponding properties in PREFER_PROP to nil. */
      for (i = FONT_WEIGHT_INDEX; i <= FONT_SIZE_INDEX; i++)
	if (! NILP (AREF (spec, i)))
	  prefer_prop[i++] = Qnil;
    }

  if (FLOATP (prefer_prop[FONT_SIZE_INDEX]))
    prefer_prop[FONT_SIZE_INDEX]
      = make_number (font_pixel_size (XFRAME (frame), prefer));

  /* Scoring and sorting.  */
  SAFE_ALLOCA (data, struct font_sort_data *, (sizeof *data) * len);
  for (i = 0; i < len; i++)
    {
      data[i].entity = AREF (vec, i);
      data[i].score = font_score (data[i].entity, prefer_prop);
    }
  qsort (data, len, sizeof *data, font_compare);
  for (i = 0; i < len; i++)
    ASET (vec, i, data[i].entity);
  SAFE_FREE ();

  return vec;
}


/* API of Font Service Layer.  */

void
font_update_sort_order (order)
     int *order;
{
  int i, shift_bits = 21;

  for (i = 0; i < 4; i++, shift_bits -= 7)
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

Lisp_Object
font_symbolic_weight (font)
     Lisp_Object font;
{
  Lisp_Object weight = AREF (font, FONT_WEIGHT_INDEX);

  if (INTEGERP (weight))
    weight = prop_numeric_to_name (FONT_WEIGHT_INDEX, XINT (weight));
  return weight;
}

Lisp_Object
font_symbolic_slant (font)
     Lisp_Object font;
{
  Lisp_Object slant = AREF (font, FONT_SLANT_INDEX);

  if (INTEGERP (slant))
    slant = prop_numeric_to_name (FONT_SLANT_INDEX, XINT (slant));
  return slant;
}

Lisp_Object
font_symbolic_width (font)
     Lisp_Object font;
{
  Lisp_Object width = AREF (font, FONT_WIDTH_INDEX);

  if (INTEGERP (width))
    width = prop_numeric_to_name (FONT_WIDTH_INDEX, XINT (width));
  return width;
}

int
font_match_p (spec, entity)
     Lisp_Object spec, entity;
{
  int i;

  for (i = FONT_FOUNDRY_INDEX; i < FONT_SIZE_INDEX; i++)
    if (! NILP (AREF (spec, i))
	&& ! EQ (AREF (spec, i), AREF (entity, i)))
      return 0;
  if (INTEGERP (AREF (spec, FONT_SIZE_INDEX))
      && XINT (AREF (entity, FONT_SIZE_INDEX)) > 0
      && (XINT (AREF (spec, FONT_SIZE_INDEX))
	  != XINT (AREF (entity, FONT_SIZE_INDEX))))
    return 0;
  return 1;
}

Lisp_Object
font_find_object (font)
     struct font *font;
{
  Lisp_Object tail, elt;

  for (tail = AREF (font->entity, FONT_OBJLIST_INDEX); CONSP (tail);
       tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (font == XSAVE_VALUE (elt)->pointer
	  && XSAVE_VALUE (elt)->integer > 0)
	return elt;
    }
  abort ();
  return Qnil;
}

static Lisp_Object scratch_font_spec, scratch_font_prefer;

/* Return a vector of font-entities matching with SPEC on frame F.  */

static Lisp_Object
font_list_entities (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  struct font_driver_list *driver_list = f->font_driver_list;
  Lisp_Object ftype, family, size, alternate_familes;
  Lisp_Object *vec = alloca (sizeof (Lisp_Object) * num_font_drivers);
  int i;

  if (! vec)
    return null_vector;

  family = AREF (spec, FONT_FAMILY_INDEX);
  if (NILP (family))
    alternate_familes = Qnil;
  else
    {
      if (NILP (font_family_alist)
	  && !NILP (Vface_alternative_font_family_alist))
	build_font_family_alist ();
      alternate_familes = assq_no_quit (family, font_family_alist);
      if (! NILP (alternate_familes))
	alternate_familes = XCDR (alternate_familes);
    }
  size = AREF (spec, FONT_SIZE_INDEX);
  if (FLOATP (size))
    ASET (spec, FONT_SIZE_INDEX, make_number (font_pixel_size (f, spec)));

  xassert (ASIZE (spec) == FONT_SPEC_MAX);
  ftype = AREF (spec, FONT_TYPE_INDEX);
  
  for (i = 0; driver_list; driver_list = driver_list->next)
    if (driver_list->on
	&& (NILP (ftype) || EQ (driver_list->driver->type, ftype)))
      {
	Lisp_Object cache = driver_list->driver->get_cache (frame);
	Lisp_Object tail = alternate_familes;
	Lisp_Object val;

	xassert (CONSP (cache));
	ASET (spec, FONT_TYPE_INDEX, driver_list->driver->type);
	ASET (spec, FONT_FAMILY_INDEX, family);

	while (1)
	  {
	    val = assoc_no_quit (spec, XCDR (cache));
	    if (CONSP (val))
	      val = XCDR (val);
	    else
	      {
		val = driver_list->driver->list (frame, spec);
		if (VECTORP (val))
		  XSETCDR (cache, Fcons (Fcons (Fcopy_sequence (spec), val),
					 XCDR (cache)));
	      }
	    if (VECTORP (val) && ASIZE (val) > 0)
	      {
		vec[i++] = val;
		break;
	      }
	    if (NILP (tail))
	      break;
	    ASET (spec, FONT_FAMILY_INDEX, XCAR (tail));
	    tail = XCDR (tail);
	  }
      }
  ASET (spec, FONT_TYPE_INDEX, ftype);
  ASET (spec, FONT_FAMILY_INDEX, family);
  ASET (spec, FONT_SIZE_INDEX, size);
  return (i > 0 ? Fvconcat (i, vec) : null_vector);
}

static Lisp_Object
font_matching_entity (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  struct font_driver_list *driver_list = f->font_driver_list;
  Lisp_Object ftype, size, entity;

  ftype = AREF (spec, FONT_TYPE_INDEX);
  size = AREF (spec, FONT_SIZE_INDEX);
  if (FLOATP (size))
    ASET (spec, FONT_SIZE_INDEX, make_number (font_pixel_size (f, spec)));
  entity = Qnil;
  for (; driver_list; driver_list = driver_list->next)
    if (driver_list->on
	&& (NILP (ftype) || EQ (driver_list->driver->type, ftype)))
      {
	Lisp_Object cache = driver_list->driver->get_cache (frame);
	Lisp_Object key;

	xassert (CONSP (cache));
	ASET (spec, FONT_TYPE_INDEX, driver_list->driver->type);
	key = Fcons (spec, Qnil);
	entity = assoc_no_quit (key, XCDR (cache));
	if (CONSP (entity))
	  entity = XCDR (entity);
	else
	  {
	    entity = driver_list->driver->match (frame, spec);
	    if (! NILP (entity))
	      {
		XSETCAR (key, Fcopy_sequence (spec));
		XSETCDR (cache, Fcons (Fcons (key, entity), XCDR (cache)));
	      }
	  }
	if (! NILP (entity))
	  break;
      }
  ASET (spec, FONT_TYPE_INDEX, ftype);
  ASET (spec, FONT_SIZE_INDEX, size);
  return entity;
}

static int num_fonts;

static Lisp_Object
font_open_entity (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  struct font_driver_list *driver_list;
  Lisp_Object objlist, size, val;
  struct font *font;

  size = AREF (entity, FONT_SIZE_INDEX);
  xassert (NATNUMP (size));
  if (XINT (size) != 0)
    pixel_size = XINT (size);

  for (objlist = AREF (entity, FONT_OBJLIST_INDEX); CONSP (objlist);
       objlist = XCDR (objlist))
    {
      font = XSAVE_VALUE (XCAR (objlist))->pointer;
      if (font->pixel_size == pixel_size)
	{
	  XSAVE_VALUE (XCAR (objlist))->integer++;
	  return XCAR (objlist);
	}
    }

  xassert (FONT_ENTITY_P (entity));
  val = AREF (entity, FONT_TYPE_INDEX);
  for (driver_list = f->font_driver_list;
       driver_list && ! EQ (driver_list->driver->type, val);
       driver_list = driver_list->next);
  if (! driver_list)
    return Qnil;

  font = driver_list->driver->open (f, entity, pixel_size);
  if (! font)
    return Qnil;
  font->scalable = XINT (size) == 0;

  val = make_save_value (font, 1);
  ASET (entity, FONT_OBJLIST_INDEX,
	Fcons (val, AREF (entity, FONT_OBJLIST_INDEX)));
  num_fonts++;
  return val;
}

void
font_close_object (f, font_object)
     FRAME_PTR f;
     Lisp_Object font_object;
{
  struct font *font = XSAVE_VALUE (font_object)->pointer;
  Lisp_Object objlist;
  Lisp_Object tail, prev = Qnil;

  XSAVE_VALUE (font_object)->integer--;
  xassert (XSAVE_VALUE (font_object)->integer >= 0);
  if (XSAVE_VALUE (font_object)->integer > 0)
    return;

  objlist = AREF (font->entity, FONT_OBJLIST_INDEX);
  for (prev = Qnil, tail = objlist; CONSP (tail);
       prev = tail, tail = XCDR (tail))
    if (EQ (font_object, XCAR (tail)))
      {
	if (font->driver->close)
	  font->driver->close (f, font);
	XSAVE_VALUE (font_object)->pointer = NULL;
	if (NILP (prev))
	  ASET (font->entity, FONT_OBJLIST_INDEX, XCDR (objlist));
	else
	  XSETCDR (prev, XCDR (objlist));
	return;
      }
  abort ();
}

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

  xassert (FONT_OBJECT_P (font));
  fontp = XSAVE_VALUE (font)->pointer;

  if (fontp->driver->has_char)
    {
      int result = fontp->driver->has_char (fontp->entity, c);

      if (result >= 0)
	return result;
    }
  return (fontp->driver->encode_char (fontp, c) != FONT_INVALID_CODE);
}

unsigned
font_encode_char (font_object, c)
     Lisp_Object font_object;
     int c;
{
  struct font *font = XSAVE_VALUE (font_object)->pointer;

  return font->driver->encode_char (font, c);
}

Lisp_Object
font_get_name (font_object)
     Lisp_Object font_object;
{
  struct font *font = XSAVE_VALUE (font_object)->pointer;
  char *name = (font->font.full_name ? font->font.full_name
		: font->font.name ? font->font.name
		: NULL);

  return (name ? make_unibyte_string (name, strlen (name)) : null_string);
}

Lisp_Object
font_get_spec (font_object)
     Lisp_Object font_object;
{
  struct font *font = XSAVE_VALUE (font_object)->pointer;
  Lisp_Object spec = Ffont_spec (0, NULL);
  int i;

  for (i = 0; i < FONT_SIZE_INDEX; i++)
    ASET (spec, i, AREF (font->entity, i));
  ASET (spec, FONT_SIZE_INDEX, make_number (font->pixel_size));
  return spec;
}

Lisp_Object
font_get_frame (font)
     Lisp_Object font;
{
  if (FONT_OBJECT_P (font))
    font = ((struct font *) XSAVE_VALUE (font)->pointer)->entity;
  xassert (FONT_ENTITY_P (font));
  return AREF (font, FONT_FRAME_INDEX);
}

/* Find a font entity best matching with LFACE.  If SPEC is non-nil,
   the font must exactly match with it.  */

Lisp_Object
font_find_for_lface (f, lface, spec)
     FRAME_PTR f;
     Lisp_Object *lface;
     Lisp_Object spec;
{
  Lisp_Object frame, entities;
  int i;

  XSETFRAME (frame, f);

  if (NILP (spec))
    {
      for (i = 0; i < FONT_SPEC_MAX; i++)
	ASET (scratch_font_spec, i, Qnil);
      ASET (scratch_font_spec, FONT_REGISTRY_INDEX, Qiso8859_1);

      if (! NILP (lface[LFACE_FAMILY_INDEX]))
	font_merge_old_spec (Qnil, lface[LFACE_FAMILY_INDEX], Qnil,
			     scratch_font_spec);
      entities = font_list_entities (frame, scratch_font_spec);
      while (ASIZE (entities) == 0)
	{
	  /* Try without FOUNDRY or FAMILY.  */
	  if (! NILP (AREF (scratch_font_spec, FONT_FOUNDRY_INDEX)))
	    {
	      ASET (scratch_font_spec, FONT_FOUNDRY_INDEX, Qnil);
	      entities = font_list_entities (frame, scratch_font_spec);
	    }
	  else if (! NILP (AREF (scratch_font_spec, FONT_FAMILY_INDEX)))
	    {
	      ASET (scratch_font_spec, FONT_FAMILY_INDEX, Qnil);
	      entities = font_list_entities (frame, scratch_font_spec);
	    }
	  else
	    break;
	}
    }
  else
    {
      for (i = 0; i < FONT_SPEC_MAX; i++)
	ASET (scratch_font_spec, i, AREF (spec, i));
      if (NILP (AREF (spec, FONT_REGISTRY_INDEX)))
	ASET (scratch_font_spec, FONT_REGISTRY_INDEX, Qiso8859_1);
      entities = font_list_entities (frame, scratch_font_spec);
    }

  if (ASIZE (entities) == 0)
    return Qnil;
  if (ASIZE (entities) > 1)
    {
      /* Sort fonts by properties specified in LFACE.  */
      Lisp_Object prefer = scratch_font_prefer;
      double pt;

      if (! NILP (lface[LFACE_FAMILY_INDEX]))
	font_merge_old_spec (Qnil, lface[LFACE_FAMILY_INDEX], Qnil, prefer);
      ASET (prefer, FONT_WEIGHT_INDEX,
	    font_prop_validate_style (FONT_WEIGHT_INDEX, QCweight,
				      lface[LFACE_WEIGHT_INDEX]));
      ASET (prefer, FONT_SLANT_INDEX,
	    font_prop_validate_style (FONT_SLANT_INDEX, QCslant,
				      lface[LFACE_SLANT_INDEX]));
      ASET (prefer, FONT_WIDTH_INDEX,
	    font_prop_validate_style (FONT_WIDTH_INDEX, QCwidth,
				      lface[LFACE_SWIDTH_INDEX]));
      pt = XINT (lface[LFACE_HEIGHT_INDEX]);
      ASET (prefer, FONT_SIZE_INDEX, make_float (pt / 10));

      font_sort_entites (entities, prefer, frame, spec);
    }

  return AREF (entities, 0);
}

Lisp_Object
font_open_for_lface (f, entity, lface, spec)
     FRAME_PTR f;
     Lisp_Object entity;
     Lisp_Object *lface;
     Lisp_Object spec;
{
  int size;

  if (FONT_SPEC_P (spec) && INTEGERP (AREF (spec, FONT_SIZE_INDEX)))
    size = XINT (AREF (spec, FONT_SIZE_INDEX));
  else
    {
      double pt = XINT (lface[LFACE_HEIGHT_INDEX]);

      pt /= 10;
      size = POINT_TO_PIXEL (pt, f->resy);
    }
  return font_open_entity (f, entity, size);
}

void
font_load_for_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  Lisp_Object font_object = face->lface[LFACE_FONT_INDEX];

  if (NILP (font_object))
    {
      Lisp_Object entity = font_find_for_lface (f, face->lface, Qnil);

      if (! NILP (entity))
	font_object = font_open_for_lface (f, entity, face->lface, Qnil);
    }

  if (! NILP (font_object))
    {
      struct font *font = XSAVE_VALUE (font_object)->pointer;

      face->font = font->font.font;
      face->font_info = (struct font_info *) font;
      face->font_info_id = 0;
      face->font_name = font->font.full_name;
    }
  else
    {
      face->font = NULL;
      face->font_info = NULL;
      face->font_info_id = -1;
      face->font_name = NULL;
      add_to_log ("Unable to load font for a face%s", null_string, Qnil);
    }
}

void
font_prepare_for_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  struct font *font = (struct font *) face->font_info;

  if (font->driver->prepare_face)
    font->driver->prepare_face (f, face);
}

void
font_done_for_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  struct font *font = (struct font *) face->font_info;

  if (font->driver->done_face)
    font->driver->done_face (f, face);
  face->extra = NULL;
}

Lisp_Object
font_open_by_name (f, name)
     FRAME_PTR f;
     char *name;
{
  Lisp_Object args[2];
  Lisp_Object spec, prefer, size, entity, entity_list;
  Lisp_Object frame;
  int i;
  int pixel_size;

  XSETFRAME (frame, f);

  args[0] = QCname;
  args[1] = make_unibyte_string (name, strlen (name));
  spec = Ffont_spec (2, args);
  prefer = scratch_font_prefer;
  for (i = FONT_WEIGHT_INDEX; i < FONT_SIZE_INDEX; i++)
    if (NILP (AREF (spec, i)))
      ASET (prefer, i, make_number (100));
  size = AREF (spec, FONT_SIZE_INDEX);
  if (NILP (size))
    pixel_size = 0;
  else if (INTEGERP (size))
    pixel_size = XINT (size);
  else				/* FLOATP (size) */
    {
      double pt = XFLOAT_DATA (size);

      pixel_size = POINT_TO_PIXEL (pt, f->resy);
      size = make_number (pixel_size);
      ASET (spec, FONT_SIZE_INDEX, size);
    }
  if (pixel_size == 0)
    {
      pixel_size = POINT_TO_PIXEL (12.0, f->resy);
      size = make_number (pixel_size);
    }
  ASET (prefer, FONT_SIZE_INDEX, size);
  if (NILP (AREF (spec, FONT_REGISTRY_INDEX)))
    ASET (spec, FONT_REGISTRY_INDEX, Qiso8859_1);

  entity_list = Flist_fonts (spec, frame, make_number (1), prefer);
  if (NILP (entity_list))
    entity = font_matching_entity (frame, spec);
  else
    entity = XCAR (entity_list);
  return (NILP (entity)
	  ? Qnil
	  : font_open_entity (f, entity, pixel_size));
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
    error ("Unsable font driver for a frame: %s",
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
  num_font_drivers++;
}

/* Free font-driver list on frame F.  It doesn't free font-drivers
   themselves.  */

void
free_font_driver_list (f)
     FRAME_PTR f;
{
  while (f->font_driver_list)
    {
      struct font_driver_list *next = f->font_driver_list->next;

      free (f->font_driver_list);
      f->font_driver_list = next;
    }
}

/* Make the frame F use font backends listed in NEW_BACKENDS (list of
   symbols).  If NEW_BACKENDS is nil, make F use all available font
   drivers.  If no backend is available, dont't alter
   f->font_driver_list.

   A caller must free all realized faces and clear all font caches if
   any in advance.  The return value is a list of font backends
   actually made used for on F.  */

Lisp_Object
font_update_drivers (f, new_drivers)
     FRAME_PTR f;
     Lisp_Object new_drivers;
{
  Lisp_Object active_drivers = Qnil;
  struct font_driver_list *list;

  /* At first check which font backends are available.  */
  for (list = f->font_driver_list; list; list = list->next)
    if (NILP (new_drivers)
	|| ! NILP (Fmemq (list->driver->type, new_drivers)))
      {
	list->on = 2;
	active_drivers = nconc2 (active_drivers,
				 Fcons (list->driver->type, Qnil));
      }
  /* If at least one backend is available, update all list->on.  */
  if (! NILP (active_drivers))
    for (list = f->font_driver_list; list; list = list->next)
      list->on = (list->on == 2);

  return active_drivers;
}


Lisp_Object
font_at (c, pos, face, w, object)
     int c;
     EMACS_INT pos;
     struct face *face;
     struct window *w;
     Lisp_Object object;
{
  FRAME_PTR f;
  int face_id;
  int dummy;

  f = XFRAME (w->frame);
  if (! FRAME_WINDOW_P (f))
    return Qnil;
  if (! face)
    {
      if (STRINGP (object))
	face_id = face_at_string_position (w, object, pos, 0, -1, -1, &dummy,
					   DEFAULT_FACE_ID, 0);
      else
	face_id = face_at_buffer_position (w, pos, -1, -1, &dummy,
					   pos + 100, 0);
      face = FACE_FROM_ID (f, face_id);
    }
  face_id = FACE_FOR_CHAR (f, face, c, pos, object);
  face = FACE_FROM_ID (f, face_id);
  if (! face->font_info)
    return Qnil;
  return font_lispy_object ((struct font *) face->font_info);
}


/* Lisp API */

DEFUN ("fontp", Ffontp, Sfontp, 1, 1, 0,
       doc: /* Return t if object is a font-spec or font-entity.  */)
     (object)
     Lisp_Object object;
{
  return (FONTP (object) ? Qt : Qnil);
}

DEFUN ("font-spec", Ffont_spec, Sfont_spec, 0, MANY, 0,
       doc: /* Return a newly created font-spec with specified arguments as properties.
usage: (font-spec &rest properties)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object spec = Fmake_vector (make_number (FONT_SPEC_MAX), Qnil);
  int i;

  for (i = 0; i < nargs; i += 2)
    {
      enum font_property_index prop;
      Lisp_Object key = args[i], val = args[i + 1];

      prop = get_font_prop_index (key, 0);
      if (prop < FONT_EXTRA_INDEX)
	ASET (spec, prop, val);
      else
	{
	  if (EQ (key, QCname))
	    {
	      CHECK_STRING (val);
	      font_parse_name ((char *) SDATA (val), spec);
	    }
	  font_put_extra (spec, key, val);
	}
    }
  CHECK_VALIDATE_FONT_SPEC (spec);
  return spec;
}


DEFUN ("font-get", Ffont_get, Sfont_get, 2, 2, 0,
       doc: /* Return the value of FONT's PROP property.
FONT is a font-spec, a font-entity, or a font-object.  */)
     (font, prop)
     Lisp_Object font, prop;
{
  enum font_property_index idx;

  if (FONT_OBJECT_P (font))
    {
      struct font *fontp = XSAVE_VALUE (font)->pointer;

      if (EQ (prop, QCotf))
	{
#ifdef HAVE_LIBOTF
	  return font_otf_capability (fontp);
#else  /* not HAVE_LIBOTF */
	  return Qnil;
#endif	/* not HAVE_LIBOTF */
	}
      font = fontp->entity;
    }
  else
    CHECK_FONT (font);
  idx = get_font_prop_index (prop, 0);
  if (idx < FONT_EXTRA_INDEX)
    return AREF (font, idx);
  if (FONT_ENTITY_P (font))
    return Qnil;
  return Fcdr (Fassoc (AREF (font, FONT_EXTRA_INDEX), prop));
}


DEFUN ("font-put", Ffont_put, Sfont_put, 3, 3, 0,
       doc: /* Set one property of FONT-SPEC: give property PROP value VALUE.  */)
     (font_spec, prop, val)
     Lisp_Object font_spec, prop, val;
{
  enum font_property_index idx;
  Lisp_Object extra, slot;

  CHECK_FONT_SPEC (font_spec);
  idx = get_font_prop_index (prop, 0);
  if (idx < FONT_EXTRA_INDEX)
    return ASET (font_spec, idx, val);
  extra = AREF (font_spec, FONT_EXTRA_INDEX);
  slot = Fassoc (extra, prop);
  if (NILP (slot))
    extra = Fcons (Fcons (prop, val), extra);
  else
    Fsetcdr (slot, val);
  return val;
}

DEFUN ("list-fonts", Flist_fonts, Slist_fonts, 1, 4, 0,
       doc: /* List available fonts matching FONT-SPEC on the current frame.
Optional 2nd argument FRAME specifies the target frame.
Optional 3rd argument NUM, if non-nil, limits the number of returned fonts.
Optional 4th argument PREFER, if non-nil, is a font-spec
to which closeness fonts are sorted.  */)
     (font_spec, frame, num, prefer)
     Lisp_Object font_spec, frame, num, prefer;
{
  Lisp_Object vec, list, tail;
  int n = 0, i, len;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  CHECK_VALIDATE_FONT_SPEC (font_spec);
  if (! NILP (num))
    {
      CHECK_NUMBER (num);
      n = XINT (num);
      if (n <= 0)
	return Qnil;
    }
  if (! NILP (prefer))
    CHECK_FONT (prefer);

  vec = font_list_entities (frame, font_spec);
  len = ASIZE (vec);
  if (len == 0)
    return Qnil;
  if (len == 1)
    return Fcons (AREF (vec, 0), Qnil);

  if (! NILP (prefer))
    vec = font_sort_entites (vec, prefer, frame, font_spec);

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

DEFUN ("list-families", Flist_families, Slist_families, 0, 1, 0,
       doc: /* List available font families on the current frame.
Optional 2nd argument FRAME specifies the target frame.  */)
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

DEFUN ("font-xlfd-name", Ffont_xlfd_name, Sfont_xlfd_name, 1, 1, 0,
       doc: /*  Return XLFD name of FONT.
FONT is a font-spec, font-entity, or font-object.
If the name is too long for XLFD (maximum 255 chars), return nil.  */)
     (font)
     Lisp_Object font;
{
  char name[256];
  int pixel_size = 0;

  if (FONT_SPEC_P (font))
    CHECK_VALIDATE_FONT_SPEC (font);
  else if (FONT_ENTITY_P (font))
    CHECK_FONT (font);
  else
    {
      struct font *fontp;

      CHECK_FONT_GET_OBJECT (font, fontp);
      font = fontp->entity;
      pixel_size = fontp->pixel_size;
    }

  if (font_unparse_xlfd (font, pixel_size, name, 256) < 0)
    return Qnil;
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
	    Lisp_Object cache = driver_list->driver->get_cache (frame);
	    Lisp_Object tail, elt;
	    
	    for (tail = XCDR (cache); CONSP (tail); tail = XCDR (tail))
	      {
		elt = XCAR (tail);
		if (CONSP (elt) && FONT_SPEC_P (XCAR (elt)))
		  {
		    Lisp_Object vec = XCDR (elt);
		    int i;

		    for (i = 0; i < ASIZE (vec); i++)
		      {
			Lisp_Object entity = AREF (vec, i);

			if (EQ (driver_list->driver->type,
				AREF (entity, FONT_TYPE_INDEX)))
			  {
			    Lisp_Object objlist
			      = AREF (entity, FONT_OBJLIST_INDEX);

			    for (; CONSP (objlist); objlist = XCDR (objlist))
			      {
				Lisp_Object val = XCAR (objlist);
				struct Lisp_Save_Value *p = XSAVE_VALUE (val);
				struct font *font = p->pointer;

				xassert (font && (driver_list->driver
						  == font->driver));
				driver_list->driver->close (f, font);
				p->pointer = NULL;
				p->integer = 0;
			      }
			    if (driver_list->driver->free_entity)
			      driver_list->driver->free_entity (entity);
			  }
		      }
		  }
	      }
	    XSETCDR (cache, Qnil);
	  }
    }

  return Qnil;
}

DEFUN ("internal-set-font-style-table", Finternal_set_font_style_table,
       Sinternal_set_font_style_table, 2, 2, 0,
       doc: /* Set font style table for PROP to TABLE.
PROP must be `:weight', `:slant', or `:width'.
TABLE must be an alist of symbols vs the corresponding numeric values
sorted by numeric values.  */)
     (prop, table)
     Lisp_Object prop, table;
{
  int table_index;
  int numeric;
  Lisp_Object tail, val;
  
  CHECK_SYMBOL (prop);
  table_index = (EQ (prop, QCweight) ? 0
		 : EQ (prop, QCslant) ? 1
		 : EQ (prop, QCwidth) ? 2
		 : 3);
  if (table_index >= ASIZE (font_style_table))
    error ("Invalid font style property: %s", SDATA (SYMBOL_NAME (prop)));
  table = Fcopy_sequence (table);
  numeric = -1;
  for (tail = table; ! NILP (tail); tail = Fcdr (tail))
    {
      prop = Fcar (Fcar (tail));
      val = Fcdr (Fcar (tail));
      CHECK_SYMBOL (prop);
      CHECK_NATNUM (val);
      if (numeric > XINT (val))
	error ("Numeric values not sorted for %s", SDATA (SYMBOL_NAME (prop)));
      numeric = XINT (val);
      XSETCAR (tail, Fcons (prop, val));
    }
  ASET (font_style_table, table_index, table);
  return Qnil;
}
  
DEFUN ("font-make-gstring", Ffont_make_gstring, Sfont_make_gstring, 2, 2, 0,
       doc: /* Return a newly created g-string for FONT-OBJECT with NUM glyphs.
FONT-OBJECT may be nil if it is not yet known.

G-string is sequence of glyphs of a specific font,
and is a vector of this form:
    [ HEADER GLYPH ... ]
HEADER is a vector of this form:
    [FONT-OBJECT LBEARING RBEARING WIDTH ASCENT DESCENT]
where
    FONT-OBJECT is a font-object for all glyphs in the g-string,
    LBEARING thry DESCENT is the metrics (in pixels) of the whole G-string.
GLYPH is a vector of this form:
    [ FROM-IDX TO-IDX C CODE WIDTH [ [X-OFF Y-OFF WADJUST] | nil] ]
where
    FROM-IDX and TO-IDX are used internally and should not be touched.
    C is the character of the glyph.
    CODE is the glyph-code of C in FONT-OBJECT.
    X-OFF and Y-OFF are offests to the base position for the glyph.
    WIDTH is the normal width of the glyph.
    WADJUST is the adjustment to the normal width of the glyph.  */)
     (font_object, num)
     Lisp_Object font_object, num;
{
  Lisp_Object gstring, g;
  int len;
  int i;

  if (! NILP (font_object))
    CHECK_FONT_OBJECT (font_object);
  CHECK_NATNUM (num);

  len = XINT (num) + 1;
  gstring = Fmake_vector (make_number (len), Qnil);
  g = Fmake_vector (make_number (6), Qnil);
  ASET (g, 0, font_object);
  ASET (gstring, 0, g);
  for (i = 1; i < len; i++)
    ASET (gstring, i, Fmake_vector (make_number (8), Qnil));
  return gstring;
}

DEFUN ("font-fill-gstring", Ffont_fill_gstring, Sfont_fill_gstring, 4, 5, 0,
       doc: /* Fillin glyph-string GSTRING by characters for FONT-OBJECT.
START and END specifies the region to extract characters.
If optional 3rd argument OBJECT is non-nil, it is a buffer or a string from
where to extract characters.
FONT-OBJECT may be nil if GSTRING already already contains one.  */)
     (gstring, font_object, start, end, object)
     Lisp_Object gstring, font_object, start, end, object;
{
  int len, i, c;
  unsigned code;
  struct font *font;

  CHECK_VECTOR (gstring);
  if (NILP (font_object))
    font_object = LGSTRING_FONT (gstring);
  CHECK_FONT_GET_OBJECT (font_object, font);

  if (STRINGP (object))
    {
      const unsigned char *p;

      CHECK_NATNUM (start);
      CHECK_NATNUM (end);
      if (XINT (start) > XINT (end)
	  || XINT (end) > ASIZE (object)
	  || XINT (end) - XINT (start) > LGSTRING_LENGTH (gstring))
	args_out_of_range (start, end);

      len = XINT (end) - XINT (start);
      p = SDATA (object) + string_char_to_byte (object, XINT (start));
      for (i = 0; i < len; i++)
	{
	  Lisp_Object g = LGSTRING_GLYPH (gstring, i);

	  c = STRING_CHAR_ADVANCE (p);
	  code = font->driver->encode_char (font, c);
	  if (code > MOST_POSITIVE_FIXNUM)
	    error ("Glyph code 0x%X is too large", code);
	  LGLYPH_SET_FROM (g, make_number (i));
	  LGLYPH_SET_TO (g, make_number (i + 1));
	  LGLYPH_SET_CHAR (g, make_number (c));
	  LGLYPH_SET_CODE (g, make_number (code));
	}
    }
  else
    {
      int pos, pos_byte;

      if (! NILP (object))
	Fset_buffer (object);
      validate_region (&start, &end);
      if (XINT (end) - XINT (start) > LGSTRING_LENGTH (gstring))
	args_out_of_range (start, end);
      len = XINT (end) - XINT (start);
      pos = XINT (start);
      pos_byte = CHAR_TO_BYTE (pos);
      for (i = 0; i < len; i++)
	{
	  Lisp_Object g = LGSTRING_GLYPH (gstring, i);

	  FETCH_CHAR_ADVANCE (c, pos, pos_byte);
	  code = font->driver->encode_char (font, c);
	  if (code > MOST_POSITIVE_FIXNUM)
	    error ("Glyph code 0x%X is too large", code);
	  LGLYPH_SET_FROM (g, make_number (i));
	  LGLYPH_SET_TO (g, make_number (i + 1));
	  LGLYPH_SET_CHAR (g, make_number (c));
	  LGLYPH_SET_CODE (g, make_number (code));
	}
    }
  for (i = LGSTRING_LENGTH (gstring) - 1; i >= len; i--)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring, i);

      LGLYPH_SET_FROM (g, Qnil);
    }
  return Qnil;
}

DEFUN ("font-drive-otf", Ffont_drive_otf, Sfont_drive_otf, 6, 6, 0,
       doc: /* Apply OpenType features on glyph-string GSTRING-IN.
OTF-SPEC specifies which featuress to apply in this format:
  (SCRIPT LANGSYS GSUB GPOS)
where
  SCRIPT is a symbol specifying a script tag of OpenType,
  LANGSYS is a symbol specifying a langsys tag of OpenType,
  GSUB and GPOS, if non-nil, are lists of symbols specifying feature tags.

If LANGYS is nil, the default langsys is selected.

The features are applied in the order appeared in the list.  The
symbol `*' means to apply all available features not appeared in this
list, and the remaining features are ignored.  For instance, (vatu
pstf * haln) is to apply vatu and pstf in this order, then to apply
all available features other than vatu, pstf, and haln.

The features are applied to the glyphs in the range FROM and TO of
the glyph-string GSTRING-IN.

If some of a feature is actually applicable, the resulting glyphs are
produced in the glyph-string GSTRING-OUT from the index INDEX.  In
this case, the value is the number of produced glyphs.

If no feature is applicable, no glyph is produced in GSTRING-OUT, and
the value is 0.

If GSTRING-OUT is too short to hold produced glyphs, no glyphs is
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
  CHECK_FONT_GET_OBJECT (font_object, font);
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
FEATURE-SPEC specifies which features of the font FONT-OBJECT to apply
in this format:
  (SCRIPT LANGSYS FEATURE ...)
See the documentation of `font-otf-gsub' for more detail.

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
  LGLYPH_SET_CHAR (g, character);
  gstring_out = Ffont_make_gstring (font_object, make_number (10));
  while ((num = font->driver->otf_drive (font, otf_features, gstring_in, 0, 1,
					 gstring_out, 0, 1)) < 0)
    gstring_out = Ffont_make_gstring (font_object,
				      make_number (ASIZE (gstring_out) * 2));
  alternates = Qnil;
  for (i = 0; i < num; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring_out, i);
      int c = XINT (LGLYPH_CHAR (g));
      unsigned code = XUINT (LGLYPH_CODE (g));

      alternates = Fcons (Fcons (make_number (code),
				 c > 0 ? make_number (c) : Qnil),
			  alternates);
    }
  return Fnreverse (alternates);
}


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
  if (NILP (size))
    size = AREF (font_entity, FONT_SIZE_INDEX);
  CHECK_NUMBER (size);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  
  isize = XINT (size);
  if (isize < 0)
    isize = POINT_TO_PIXEL (- isize, XFRAME (frame)->resy);

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

SIZE is a maximum advance width of the font in pixel.

ASCENT, DESCENT, SPACE-WIDTH, AVERAGE-WIDTH are metrics of the font in
pixel.

CAPABILITY is a list whose first element is a symbol representing the
font format \(x, opentype, truetype, type1, pcf, or bdf) and the
remaining elements describes a detail of the font capability.

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

If the font is not OpenType font, OTF-CAPABILITY is nil.  */)
     (font_object)
     Lisp_Object font_object;
{
  struct font *font;
  Lisp_Object val;

  CHECK_FONT_GET_OBJECT (font_object, font);

  val = Fmake_vector (make_number (9), Qnil);
  if (font->font.full_name)
    ASET (val, 0, make_unibyte_string (font->font.full_name,
				       strlen (font->font.full_name)));
  if (font->file_name)
    ASET (val, 1, make_unibyte_string (font->file_name,
				       strlen (font->file_name)));
  ASET (val, 2, make_number (font->pixel_size));
  ASET (val, 3, make_number (font->font.size));
  ASET (val, 4, make_number (font->ascent));
  ASET (val, 5, make_number (font->descent));
  ASET (val, 6, make_number (font->font.space_width));
  ASET (val, 7, make_number (font->font.average_width));
  if (font->driver->otf_capability)
    ASET (val, 8, Fcons (Qopentype, font->driver->otf_capability (font)));
  else
    ASET (val, 8, Fcons (font->format, Qnil));
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
      struct font_metrics metrics;

      code = font->driver->encode_char (font, c);
      if (code == FONT_INVALID_CODE)
	continue;
      val = Fmake_vector (make_number (6), Qnil);
      if (code <= MOST_POSITIVE_FIXNUM)
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
       doc: /* Return t iff font-spec SPEC matches with FONT.
FONT is a font-spec, font-entity, or font-object. */)
     (spec, font)
     Lisp_Object spec, font;
{
  CHECK_FONT_SPEC (spec);
  if (FONT_OBJECT_P (font))
    font = ((struct font *) XSAVE_VALUE (font)->pointer)->entity;
  else if (! FONT_ENTITY_P (font))
    CHECK_FONT_SPEC (font);

  return (font_match_p (spec, font) ? Qt : Qnil);
}

DEFUN ("font-at", Ffont_at, Sfont_at, 1, 2, 0,
       doc: /* Return a font-object for displaying a character at POSISTION.
Optional second arg WINDOW, if non-nil, is a window displaying
the current buffer.  It defaults to the currently selected window.  */)
     (position, window)
     Lisp_Object position, window;
{
  struct window *w;
  EMACS_INT pos, pos_byte;
  int c;

  CHECK_NUMBER_COERCE_MARKER (position);
  pos = XINT (position);
  if (pos < BEGV || pos >= ZV)
    args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
  pos_byte = CHAR_TO_BYTE (pos);
  c = FETCH_CHAR (pos_byte);
  if (NILP (window))
    window = selected_window;
  CHECK_LIVE_WINDOW (window);
  w = XWINDOW (selected_window);

  return font_at (c, pos, NULL, w, Qnil);
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


extern void syms_of_ftfont P_ (());
extern void syms_of_xfont P_ (());
extern void syms_of_xftfont P_ (());
extern void syms_of_ftxfont P_ (());
extern void syms_of_bdffont P_ (());
extern void syms_of_w32font P_ (());
extern void syms_of_atmfont P_ (());

void
syms_of_font ()
{
  sort_shift_bits[FONT_SLANT_INDEX] = 0;
  sort_shift_bits[FONT_WEIGHT_INDEX] = 7;
  sort_shift_bits[FONT_SIZE_INDEX] = 14;
  sort_shift_bits[FONT_WIDTH_INDEX] = 21;
  sort_shift_bits[FONT_ADSTYLE_INDEX] = 28;
  sort_shift_bits[FONT_FOUNDRY_INDEX] = 29;
  sort_shift_bits[FONT_FAMILY_INDEX] = 30;
  sort_shift_bits[FONT_REGISTRY_INDEX] = 31;
  /* Note that sort_shift_bits[FONT_TYPE_INDEX] is never used.  */

  staticpro (&font_style_table);
  font_style_table = Fmake_vector (make_number (3), Qnil);

  staticpro (&font_family_alist);
  font_family_alist = Qnil;

  DEFSYM (Qfontp, "fontp");
  DEFSYM (Qopentype, "opentype");

  DEFSYM (Qiso8859_1, "iso8859-1");
  DEFSYM (Qiso10646_1, "iso10646-1");
  DEFSYM (Qunicode_bmp, "unicode-bmp");
  DEFSYM (Qunicode_sip, "unicode-sip");

  DEFSYM (QCotf, ":otf");
  DEFSYM (QClanguage, ":language");
  DEFSYM (QCscript, ":script");

  DEFSYM (QCfoundry, ":foundry");
  DEFSYM (QCadstyle, ":adstyle");
  DEFSYM (QCregistry, ":registry");
  DEFSYM (QCspacing, ":spacing");
  DEFSYM (QCdpi, ":dpi");
  DEFSYM (QCscalable, ":scalable");
  DEFSYM (QCextra, ":extra");

  DEFSYM (Qc, "c");
  DEFSYM (Qm, "m");
  DEFSYM (Qp, "p");
  DEFSYM (Qd, "d");

  staticpro (&null_string);
  null_string = build_string ("");
  staticpro (&null_vector);
  null_vector = Fmake_vector (make_number (0), Qnil);

  staticpro (&scratch_font_spec);
  scratch_font_spec = Ffont_spec (0, NULL);
  staticpro (&scratch_font_prefer);
  scratch_font_prefer = Ffont_spec (0, NULL);

#ifdef HAVE_LIBOTF
  staticpro (&otf_list);
  otf_list = Qnil;
#endif

  defsubr (&Sfontp);
  defsubr (&Sfont_spec);
  defsubr (&Sfont_get);
  defsubr (&Sfont_put);
  defsubr (&Slist_fonts);
  defsubr (&Slist_families);
  defsubr (&Sfind_font);
  defsubr (&Sfont_xlfd_name);
  defsubr (&Sclear_font_cache);
  defsubr (&Sinternal_set_font_style_table);
  defsubr (&Sfont_make_gstring);
  defsubr (&Sfont_fill_gstring);
  defsubr (&Sfont_drive_otf);
  defsubr (&Sfont_otf_alternates);

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
#ifdef MAC_OS
  syms_of_atmfont ();
#endif	/* MAC_OS */
}

/* arch-tag: 74c9475d-5976-4c93-a327-942ae3072846
   (do not change this comment) */
