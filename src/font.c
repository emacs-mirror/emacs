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
#include "dispextern.h"
#include "charset.h"
#include "character.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"

#define FONT_DEBUG

#ifdef FONT_DEBUG
#undef xassert
#define xassert(X)	do {if (!(X)) abort ();} while (0)
#else
#define xassert(X)	(void) 0
#endif

int enable_font_backend;

Lisp_Object Qfontp;

/* Important character set symbols.  */
Lisp_Object Qiso8859_1, Qiso10646_1, Qunicode_bmp;

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
   on resolution RESY.  */
#define POINT_TO_PIXEL(POINT, RESY) ((POINT) * (RESY) / PT_PER_INCH + 0.5)

/* Return a point size (double) corresponding to POINT size (integer)
   on resolution RESY.  */
#define PIXEL_TO_POINT(PIXEL, RESY) ((PIXEL) * PT_PER_INCH * 10 / (RESY) + 0.5)

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
Lisp_Object QCspacing, QCdpi, QCotf, QClanguage, QCscript;

/* List of all font drivers.  All font-backends (XXXfont.c) call
   add_font_driver in syms_of_XXXfont to register the font-driver
   here.  */
static struct font_driver_list *font_driver_list;

static Lisp_Object prop_name_to_numeric P_ ((enum font_property_index,
					     Lisp_Object));
static Lisp_Object prop_numeric_to_name P_ ((enum font_property_index, int));
static Lisp_Object font_open_entity P_ ((FRAME_PTR, Lisp_Object, int));

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

  if (CONSP (val) && INTEGERP (XCDR (val)))
    dpi = XINT (XCDR (val));
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

static Lisp_Object
font_prop_validate_type (prop, val)
     enum font_property_index prop;
     Lisp_Object val;
{
  return (SYMBOLP (val) ? val : Qerror);
}

static Lisp_Object
font_prop_validate_symbol (prop, val)
     enum font_property_index prop;
     Lisp_Object val;
{
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
font_prop_validate_style (prop, val)
     enum font_property_index prop;
     Lisp_Object val;
{
  if (! INTEGERP (val))
    {
      if (STRINGP (val))
	val = intern_downcase ((char *) SDATA (val), SBYTES (val));
      if (! SYMBOLP (val))
	val = Qerror;
      else
	{
	  val = prop_name_to_numeric (prop, val);
	  if (NILP (val))
	    val = Qerror;
	}
    }
  return val;
}

static Lisp_Object
font_prop_validate_size (prop, val)
     enum font_property_index prop;
     Lisp_Object val;
{
  return (NATNUMP (val) || (FLOATP (val) && XFLOAT_DATA (val) >= 0)
	  ? val : Qerror);
}

static Lisp_Object
font_prop_validate_extra (prop, val)
     enum font_property_index prop;
     Lisp_Object val;
{
  Lisp_Object tail;

  for (tail = val; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object key = Fcar (XCAR (tail)), this_val = Fcdr (XCAR (tail));
      
      if (NILP (this_val))
	return Qnil;
      if (EQ (key, QClanguage))
	if (! SYMBOLP (this_val))
	  {
	    for (; CONSP (this_val); this_val = XCDR (this_val))
	      if (! SYMBOLP (XCAR (this_val)))
		return Qerror;
	    if (! NILP (this_val))
	      return Qerror;
	  }
      if (EQ (key, QCotf))
	if (! STRINGP (this_val))
	  return Qerror;
    }
  return (NILP (tail) ? val : Qerror);
}


struct
{
  Lisp_Object *key;
  Lisp_Object (*validater) P_ ((enum font_property_index prop,
				Lisp_Object val));
} font_property_table[FONT_SPEC_MAX] =
  { { &QCtype, font_prop_validate_type },
    { &QCfoundry, font_prop_validate_symbol },
    { &QCfamily, font_prop_validate_symbol },
    { &QCadstyle, font_prop_validate_symbol },
    { &QCregistry, font_prop_validate_symbol },
    { &QCweight, font_prop_validate_style },
    { &QCslant, font_prop_validate_style },
    { &QCwidth, font_prop_validate_style },
    { &QCsize, font_prop_validate_size },
    { &QCextra, font_prop_validate_extra }
  };

static enum font_property_index
check_font_prop_name (key)
     Lisp_Object key;
{
  enum font_property_index i;
  
  for (i = FONT_TYPE_INDEX; i < FONT_SPEC_MAX; i++)
    if (EQ (key, *font_property_table[i].key))
      break;
  return i;
}

static Lisp_Object
font_prop_validate (spec)
     Lisp_Object spec;
{
  enum font_property_index i;
  Lisp_Object val;

  for (i = FONT_TYPE_INDEX; i <= FONT_EXTRA_INDEX; i++)
    {
      if (! NILP (AREF (spec, i)))
	{
	  val = (font_property_table[i].validater) (i, AREF (spec, i));
	  if (EQ (val, Qerror))
	    Fsignal (Qerror, list3 (build_string ("invalid font property"),
				    *font_property_table[i].key,
				    AREF (spec, i)));
	  ASET (spec, i, val);
	}
    }
  return spec;
}
      
static void
font_put_extra (font, prop, val, force)
     Lisp_Object font, prop, val;
     int force;
{
  Lisp_Object extra = AREF (font, FONT_EXTRA_INDEX);
  Lisp_Object slot = (NILP (extra) ? Qnil : Fassq (prop, extra));

  if (NILP (slot))
    {
      extra = Fcons (Fcons (prop, val), extra);
      ASET (font, FONT_EXTRA_INDEX, extra);
      return;
    }
  if (! NILP (XCDR (slot)) && ! force)
    return;
  XSETCDR (slot, val);
  return;
}


/* Font name parser and unparser */

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


/* Return a Lispy value for string at STR and bytes LEN.
   If LEN == 0, return a null string.
   If the string is "*", return Qnil.
   It is assured that LEN < 256.   */

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

int
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

  for (i = 0; i < n; i++, range_mask <<= 1)
    {
      /* The triplet RANGE_FROM, RANGE_TO, and RANGE_MASK is a
	 position-based retriction for FIELD[I].  */
      int range_from = i, range_to = 14 - n + i;
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
	      else if (numeric <= 48)
		from = to = XLFD_PIXEL_INDEX,
		  mask = XLFD_PIXEL_MASK;
	      else
		from = XLFD_POINT_INDEX, to = XLFD_AVGWIDTH_MASK,
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
	      Lisp_Object name = SYMBOL_NAME (val);

	      if (SBYTES (name) == 1
		  && (SDATA (name)[0] == 'c'
		      || SDATA (name)[0] == 'm'
		      || SDATA (name)[0] == 'p'))
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
	    /* The range is narrowed by value-based restrictions.
	       Reflect it to the previous fields.  */
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

   If NAME is successfully parsed, return 2 (size is specified), 1
   (size is not specified), or 0 (size is not specified but resolution
   is specified).  Otherwise return -1.

   See font_parse_name for more detail.  */

int
font_parse_xlfd (name, font, merge)
     char *name;
     Lisp_Object font;
     int merge;
{
  int len = strlen (name);
  int i, j;
  int pixel_size, resy, avgwidth;
  double point_size;
  Lisp_Object f[XLFD_LAST_INDEX];
  Lisp_Object val;
  char *p;

  if (len > 255)
    /* Maximum XLFD name length is 255. */
    return -1;
  i = (name[0] == '*' && name[1] == '-');
  for (p = name + 1; *p; p++)
    {
      if (*p == '-')
	{
	  i++;
	  if (i == XLFD_ENCODING_INDEX)
	    break;
	}
    }

  pixel_size = resy = avgwidth = -1;
  point_size = -1;

  if (i == XLFD_ENCODING_INDEX)
    {
      /* Fully specified XLFD.  */
      if (name[0] == '-')
	name++;
      for (i = 0, p = name; ; p++)
	{
	  if (*p == '-')
	    {
	      if (i < XLFD_PIXEL_INDEX)
		f[i++] = intern_font_field (name, p - name);
	      else if (i == XLFD_PIXEL_INDEX)
		{
		  if (isdigit (*name))
		    pixel_size = atoi (name);
		  else if (*name == '[')
		    pixel_size = parse_matrix (name);
		  i++;
		}
	      else if (i == XLFD_POINT_INDEX)
		{
		  /* If PIXEL_SIZE is specified, we don't have to
		     calculate POINT_SIZE.  */
		  if (pixel_size < 0)
		    {
		      if (isdigit (*name))
			point_size = atoi (name);
		      else if (*name == '[')
			point_size = parse_matrix (name);
		    }
		  i++;
		}
	      else if (i == XLFD_RESX_INDEX)
		{
		  /* Skip this field.  */
		  f[i++] = Qnil;
		}
	      else if (i == XLFD_RESY_INDEX)
		{
		  /* Stuff RESY, SPACING, and AVGWIDTH.  */
		  /* If PIXEL_SIZE is specified, we don't have to
		     calculate RESY.  */
		  if (pixel_size < 0 && isdigit (*name))
		    resy = atoi (name);
		  for (p++; *p != '-'; p++);
		  if (isdigit (p[1]))
		    avgwidth = atoi (p + 1);
		  else if (p[1] == '~' && isdigit (p[2]))
		    avgwidth = atoi (p + 2);
		  for (p++; *p != '-'; p++);
		  if (FONT_ENTITY_P (font))
		    f[i] = intern_font_field (name, p - name);
		  else
		    f[i] = Qnil;
		  i = XLFD_REGISTRY_INDEX;
		}
	      else
		{
		  /* Stuff REGISTRY and ENCODING.  */
		  for (p++; *p; p++);
		  f[i++] = intern_font_field (name, p - name);
		  break;
		}
	      name = p + 1;
	    }
	}
      xassert (i == XLFD_ENCODING_INDEX);
    }
  else
    {
      int wild_card_found = 0;

      if (name[0] == '-')
	name++;
      for (i = 0, p = name; ; p++)
	{
	  if (*p == '-' || ! *p)
	    {
	      if (*name == '*')
		{
		  if (name + 1 != p)
		    return -1;
		  f[i++] = Qnil;
		  wild_card_found = 1;
		}
	      else if (isdigit (*name))
		{
		  f[i++] = make_number (atoi (name));
		  /* Check if all chars in this field is number.  */
		  name++;
		  while (isdigit (*name)) name++;
		  if (name != p)
		    return -1;
		}
	      else if (p == name)
		f[i++] = null_string;
	      else
		{
		  f[i++] = intern_downcase (name, p - name);
		}
	      if (! *p)
		break;
	      name = p + 1;
	    }
	}
      if (! wild_card_found)
	return -1;
      if (font_expand_wildcards (f, i) < 0)
	return -1;
      if (! NILP (f[XLFD_PIXEL_INDEX]))
	pixel_size = XINT (f[XLFD_PIXEL_INDEX]);
      /* If PIXEL_SIZE is specified, we don't have to
	 calculate POINT_SIZE and RESY.  */
      if (pixel_size < 0)
	{
	  if (! NILP (f[XLFD_POINT_INDEX]))
	    point_size = XINT (f[XLFD_POINT_INDEX]);
	  if (! NILP (f[XLFD_RESY_INDEX]))
	    resy = XINT (f[XLFD_RESY_INDEX]);
	}
      if (! NILP (f[XLFD_AVGWIDTH_INDEX]))
	avgwidth = XINT (f[XLFD_AVGWIDTH_INDEX]);
      if (NILP (f[XLFD_REGISTRY_INDEX]))
	{
	  if (! NILP (f[XLFD_ENCODING_INDEX]))
	    f[XLFD_REGISTRY_INDEX]
	      = Fintern (concat2 (build_string ("*-"),
				  SYMBOL_NAME (f[XLFD_ENCODING_INDEX])), Qnil);
	}
      else
	{
	  if (! NILP (f[XLFD_ENCODING_INDEX]))
	    f[XLFD_REGISTRY_INDEX]
	      = Fintern (concat3 (SYMBOL_NAME (f[XLFD_REGISTRY_INDEX]),
				  build_string ("-"),
				  SYMBOL_NAME (f[XLFD_ENCODING_INDEX])), Qnil);
	}
    }

  if (! merge || NILP (AREF (font, FONT_FOUNDRY_INDEX)))
    ASET (font, FONT_FOUNDRY_INDEX, f[XLFD_FOUNDRY_INDEX]);
  if (! merge || NILP (AREF (font, FONT_FAMILY_INDEX)))
    ASET (font, FONT_FAMILY_INDEX, f[XLFD_FAMILY_INDEX]);
  if (! merge || NILP (AREF (font, FONT_ADSTYLE_INDEX)))
    ASET (font, FONT_ADSTYLE_INDEX, f[XLFD_ADSTYLE_INDEX]);
  if (! merge || NILP (AREF (font, FONT_REGISTRY_INDEX)))
    ASET (font, FONT_REGISTRY_INDEX, f[XLFD_REGISTRY_INDEX]);

  for (i = FONT_WEIGHT_INDEX, j = XLFD_WEIGHT_INDEX;
       j <= XLFD_SWIDTH_INDEX; i++, j++)
    if (! merge || NILP (AREF (font, i)))
      {
	if (! INTEGERP (f[j]))
	  {
	    val = prop_name_to_numeric (i, f[j]);
	    if (INTEGERP (val))
	      f[j] = val;
	  }
	ASET (font, i, f[j]);
      }

  if (pixel_size < 0 && FONT_ENTITY_P (font))
    return -1;

  if (! merge || NILP (AREF (font, FONT_SIZE_INDEX)))
    {
      if (pixel_size >= 0)
	ASET (font, FONT_SIZE_INDEX, make_number (pixel_size));
      else if (point_size >= 0)
	ASET (font, FONT_SIZE_INDEX, make_float (point_size / 10));
    }

  if (FONT_ENTITY_P (font))
    {
      if (EQ (AREF (font, FONT_TYPE_INDEX), Qx))
	ASET (font, FONT_EXTRA_INDEX, f[XLFD_RESY_INDEX]);
    }
  else if (resy >= 0)
    font_put_extra (font, QCdpi, make_number (resy), merge);

  return (avgwidth > 0 ? 2 : resy == 0);
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
  char *f[XLFD_REGISTRY_INDEX + 1], *pixel_point;
  char work[256];
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
      i = XINT (val);
      if (i > 0)
	len += sprintf (work, "%d-*", i) + 1;
      else 			/* i == 0 */
	len += sprintf (work, "%d-*", pixel_size) + 1;
      pixel_point = work;
    }
  else if (FLOATP (val))
    {
      i = XFLOAT_DATA (val) * 10;
      len += sprintf (work, "*-%d", i) + 1;
      pixel_point = work;
    }
  else
    pixel_point = "*-*", len += 4;

  if (FONT_ENTITY_P (font)
      && EQ (AREF (font, FONT_TYPE_INDEX), Qx))
    {
      /* Setup names for RESY-SPACING-AVWIDTH.  */
      val = AREF (font, FONT_EXTRA_INDEX);
      if (SYMBOLP (val) && ! NILP (val))
	{
	  val = SYMBOL_NAME (val);
	  f[XLFD_RESY_INDEX] = (char *) SDATA (val), len += SBYTES (val) + 1;
	}
      else
	f[XLFD_RESY_INDEX] = "*-*-*", len += 6;
    }
  else
    f[XLFD_RESY_INDEX] = "*-*-*", len += 6;

  len += 3;	/* for "-*" of resx, and terminating '\0'.  */
  if (len >= nbytes)
    return -1;
  return sprintf (name, "-%s-%s-%s-%s-%s-%s-%s-*-%s-%s",
		  f[XLFD_FOUNDRY_INDEX], f[XLFD_FAMILY_INDEX],
		  f[XLFD_WEIGHT_INDEX], f[XLFD_SLANT_INDEX],
		  f[XLFD_SWIDTH_INDEX],
		  f[XLFD_ADSTYLE_INDEX], pixel_point,
		  f[XLFD_RESY_INDEX], f[XLFD_REGISTRY_INDEX]);
}

/* Parse NAME (null terminated) as Fonconfig's name format and store
   information in FONT (font-spec or font-entity).  If NAME is
   successfully parsed, return 0.  Otherwise return -1.  */

int
font_parse_fcname (name, font, merge)
     char *name;
     Lisp_Object font;
     int merge;
{
  char *p0, *p1;
  Lisp_Object family = Qnil;
  double point_size = 0;
  int pixel_size = 0;
  Lisp_Object extra = AREF (font, FONT_EXTRA_INDEX);
  int len = strlen (name);
  char *copy;

  /* It is assured that (name[0] && name[0] != '-').  */
  if (name[0] == ':')
    p0 = name;
  else
    {
      for (p0 = name + 1; *p0 && (*p0 != '-' && *p0 != ':'); p0++);
      if (isdigit (name[0]) && *p0 != '-')
	point_size = strtod (name, NULL);
      else
	{
	  family = intern_font_field (name, p0 - name);
	  if (*p0 == '-')
	    {
	      point_size = strtod (p0 + 1, &p1);
	      if (*p1 && *p1 != ':')
		return -1;
	      p0 = p1;
	    }
	}
      if (! merge || NILP (AREF (font, FONT_FAMILY_INDEX)))
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
      enum font_property_index prop;

      for (p1 = p0 + 1; islower (*p1); p1++);
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
	      if (! merge || NILP (AREF (font, FONT_WEIGHT_INDEX)))
		ASET (font, FONT_WEIGHT_INDEX,
		      prop_name_to_numeric (FONT_WEIGHT_INDEX, val));
	    }
	  else if (memcmp (p0 + 1, "roman", 5) == 0
		   || memcmp (p0 + 1, "italic", 6) == 0
		   || memcmp (p0 + 1, "oblique", 7) == 0)
	    {
	      if (! merge || NILP (AREF (font, FONT_SLANT_INDEX)))
		ASET (font, FONT_SLANT_INDEX,
		      prop_name_to_numeric (FONT_SLANT_INDEX, val));
	    }
	  else if (memcmp (p0 + 1, "charcell", 8) == 0
		   || memcmp (p0 + 1, "mono", 4) == 0
		   || memcmp (p0 + 1, "proportional", 12) == 0)
	    {
	      font_put_extra (font, QCspacing,
			      p0[1] == 'c' ? make_number (FONT_SPACING_CHARCELL)
			      : p0[1] == 'm' ? make_number (FONT_SPACING_MONO)
			      : make_number (FONT_SPACING_PROPORTIONAL),
			      merge);
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
	  char *pbeg = p0; 

	  if (memcmp (p0 + 1, "pixelsize=", 10) == 0)
	    prop = FONT_SIZE_INDEX;
	  else
	    {
	      key = intern_font_field (p0, p1 - p0);
	      prop = check_font_prop_name (key);
	    }
	  p0 = p1 + 1;
	  for (p1 = p0; *p1 && *p1 != ':'; p1++);
	  if (prop == FONT_SIZE_INDEX)
	    {
	      pixel_size = atoi (p0);
	    }
	  else if (prop < FONT_EXTRA_INDEX)
	    {
	      if (! merge || NILP (AREF (font, prop)))
		{
		  val = intern_font_field (p0, p1 - p0);
		  if (prop >= FONT_WEIGHT_INDEX && prop <= FONT_WIDTH_INDEX)
		    val = font_property_table[prop].validater (prop, val);
		  if (! EQ (val, Qerror))
		    ASET (font, prop, val);
		}
	    }
	  else if (EQ (key, QCdpi))
	    {
	      if (INTEGERP (val))
		font_put_extra (font, key, val, merge);
	    }
	  else
	    {
	      /* unknown key */
	      bcopy (pbeg, copy, p1 - pbeg);
	      copy += p1 - pbeg;
	    }
	}
      p0 = p1;
    }

  if (! merge || NILP (AREF (font, FONT_SIZE_INDEX)))
    {
      if (pixel_size > 0)
	ASET (font, FONT_SIZE_INDEX, make_number (pixel_size));
      else if (point_size > 0)
	ASET (font, FONT_SIZE_INDEX, make_float (point_size));
    }
  if (name < copy)
    font_put_extra (font, QCname, make_unibyte_string (name, copy - name),
		    merge);

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
  Lisp_Object val, size;
  int pt = 0;
  int i, j, len = 1;
  char *p;
  Lisp_Object styles[3];
  char *style_names[3] = { "weight", "slant", "swidth" };

  if (SYMBOLP (AREF (font, FONT_FAMILY_INDEX))
      && ! NILP (AREF (font, FONT_FAMILY_INDEX)))
    len += SBYTES (SYMBOL_NAME (AREF (font, FONT_FAMILY_INDEX)));
  size = AREF (font, FONT_SIZE_INDEX);
  if (INTEGERP (size))
    {
      if (XINT (size) > 0)
	pixel_size = XINT (size);
      if (pixel_size > 0)
	len += 21;		/* for ":pixelsize=NUM" */
    }
  else if (FLOATP (size))
    {
      pt = (int) XFLOAT_DATA (size);
      if (pt > 0)
	len += 11;		/* for "-NUM" */
    }
  if (SYMBOLP (AREF (font, FONT_FOUNDRY_INDEX))
      && ! NILP (AREF (font, FONT_FOUNDRY_INDEX)))
    /* ":foundry=NAME" */
    len += 9 + SBYTES (SYMBOL_NAME (AREF (font, FONT_FOUNDRY_INDEX)));
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
  if (len > nbytes)
    return -1;
  p = name;
  if (! NILP (AREF (font, FONT_FAMILY_INDEX)))
    p += sprintf(p, "%s",
		 SDATA (SYMBOL_NAME (AREF (font, FONT_FAMILY_INDEX))));
  if (pt > 0)
    p += sprintf (p, "-%d", pt);
  else if (pixel_size > 0)
    p += sprintf (p, ":pixelsize=%d", pixel_size);
  if (SYMBOLP (AREF (font, FONT_FOUNDRY_INDEX))
      && ! NILP (AREF (font, FONT_FOUNDRY_INDEX)))
    p += sprintf (p, ":foundry=%s",
		  SDATA (SYMBOL_NAME (AREF (font, FONT_FOUNDRY_INDEX))));
  for (i = 0; i < 3; i++)
    if (! NILP (styles [i]))
      p += sprintf (p, ":%s=%s", style_names[i],
		    SDATA (SYMBOL_NAME (styles [i])));
  return (p - name);
}

/* Parse NAME (null terminated) and store information in FONT
   (font-spec or font-entity).  If NAME is successfully parsed, return
   a non-negative value.  Otherwise return -1.

   If NAME is XLFD and FONT is a font-entity, store
   RESY-SPACING-AVWIDTH information as a symbol in FONT_EXTRA_INDEX.

   If MERGE is nonzero, set a property of FONT only when it's nil.  */

static int
font_parse_name (name, font, merge)
     char *name;
     Lisp_Object font;
     int merge;
{
  if (name[0] == '-' || index (name, '*'))
    return font_parse_xlfd (name, font, merge);
  if (name[0])
    return font_parse_fcname (name, font, merge);
  return -1;
}

void
font_merge_old_spec (name, family, registry, spec)
     Lisp_Object name, family, registry, spec;
{
  if (STRINGP (name))
    {
      if (font_parse_xlfd ((char *) SDATA (name), spec, 1) < 0)
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
	      if (NILP (AREF (spec, FONT_FOUNDRY_INDEX)))
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


/* OTF handler */

#ifdef HAVE_LIBOTF
#include <otf.h>

struct otf_list
{
  Lisp_Object entity;
  OTF *otf;
  struct otf_list *next;
};

static struct otf_list *otf_list;

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
  struct otf_list *list = otf_list;
  
  while (list && ! EQ (list->entity, entity))
    list = list->next;
  if (! list)
    {
      list = malloc (sizeof (struct otf_list));
      list->entity = entity;
      list->otf = file ? OTF_open (file) : NULL;
      list->next = otf_list;
      otf_list = list;
    }
  return list->otf;
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

	      if (j == script->LangSysCount)
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
	      for (l = langsys->FeatureCount -1; l >= 0; l--)
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

static int
parse_gsub_gpos_spec (spec, script, langsys, features)
     Lisp_Object spec;
     char **script, **langsys, **features;
{
  Lisp_Object val;
  int len;
  char *p;
  int asterisk;

  val = XCAR (spec);
  *script = (char *) SDATA (SYMBOL_NAME (val));
  spec = XCDR (spec);
  val = XCAR (spec);
  *langsys = NILP (val) ? NULL : (char *) SDATA (SYMBOL_NAME (val));
  spec = XCDR (spec);
  len = XINT (Flength (spec));
  *features = p = malloc (6 * len);
  if (! p)
    return -1;

  for (asterisk = 0; CONSP (spec); spec = XCDR (spec))
    {
      val = XCAR (spec);
      if (SREF (SYMBOL_NAME (val), 0) == '*')
	{
	  asterisk = 1;
	  p += sprintf (p, ",*");
	}
      else if (! asterisk)
	p += sprintf (p, ",%s", SDATA (SYMBOL_NAME (val)));
      else
	p += sprintf (p, ",~%s", SDATA (SYMBOL_NAME (val)));
    }
  return 0;
}

#define DEVICE_DELTA(table, size)				\
  (((size) >= (table).StartSize && (size) <= (table).EndSize)	\
   ? (table).DeltaValue[(size) >= (table).StartSize]		\
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


/* Drive FONT's OTF GSUB features according to GSUB_SPEC.  See the
   comment of (sturct font_driver).otf_gsub.  */

int
font_otf_gsub (font, gsub_spec, gstring_in, from, to, gstring_out, idx)
     struct font *font;
     Lisp_Object gsub_spec;
     Lisp_Object gstring_in;
     int from, to;
     Lisp_Object gstring_out;
     int idx;
{
  int len;
  int i;
  OTF *otf;
  OTF_GlyphString otf_gstring;
  OTF_Glyph *g;
  char *script, *langsys, *features;

  otf = otf_open (font->entity, font->file_name);
  if (! otf)
    return 0;
  if (OTF_get_table (otf, "head") < 0)
    return 0;
  if (OTF_check_table (otf, "GSUB") < 0)
    return 0;    
  if (parse_gsub_gpos_spec (gsub_spec, &script, &langsys, &features) < 0)
    return 0;
  len = to - from;
  otf_gstring.size = otf_gstring.used = len;
  otf_gstring.glyphs = (OTF_Glyph *) malloc (sizeof (OTF_Glyph) * len);
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * len);
  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring_in, from + i);

      otf_gstring.glyphs[i].c = XINT (LGLYPH_CHAR (g));
      otf_gstring.glyphs[i].glyph_id = XINT (LGLYPH_CODE (g));
    }

  OTF_drive_gdef (otf, &otf_gstring);
  if (OTF_drive_gsub (otf, &otf_gstring, script, langsys, features) < 0)
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
	  LGLYPH_SET_CHAR (glyph, make_number (g->c));
	  LGLYPH_SET_CODE (glyph, make_number (g->glyph_id));
	}
    }

  free (otf_gstring.glyphs);  
  return i;
}

/* Drive FONT's OTF GPOS features according to GPOS_SPEC.  See the
   comment of (sturct font_driver).otf_gpos.  */

int
font_otf_gpos (font, gpos_spec, gstring, from, to)
     struct font *font;
     Lisp_Object gpos_spec;
     Lisp_Object gstring;
     int from, to;
{
  int len;
  int i;
  OTF *otf;
  OTF_GlyphString otf_gstring;
  OTF_Glyph *g;
  char *script, *langsys, *features;
  Lisp_Object glyph;
  int u, size;
  Lisp_Object base, mark;

  otf = otf_open (font->entity, font->file_name);
  if (! otf)
    return 0;
  if (OTF_get_table (otf, "head") < 0)
    return 0;
  if (OTF_check_table (otf, "GPOS") < 0)
    return 0;    
  if (parse_gsub_gpos_spec (gpos_spec, &script, &langsys, &features) < 0)
    return 0;
  len = to - from;
  otf_gstring.size = otf_gstring.used = len;
  otf_gstring.glyphs = (OTF_Glyph *) malloc (sizeof (OTF_Glyph) * len);
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * len);
  for (i = 0; i < len; i++)
    {
      glyph = LGSTRING_GLYPH (gstring, from + i);
      otf_gstring.glyphs[i].glyph_id = XINT (LGLYPH_CODE (glyph));
    }

  OTF_drive_gdef (otf, &otf_gstring);

  if (OTF_drive_gpos (otf, &otf_gstring, script, langsys, features) < 0)
    {
      free (otf_gstring.glyphs);
      return 0;
    }

  u = otf->head->unitsPerEm;
  size = font->pixel_size;
  base = mark = Qnil;
  for (i = 0, g = otf_gstring.glyphs; i < otf_gstring.used; i++, g++)
    {
      Lisp_Object prev;
      int xoff = 0, yoff = 0,  width_adjust = 0;

      if (! g->glyph_id)
	continue;

      glyph = LGSTRING_GLYPH (gstring, from + i);
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
	    xoff = XINT (LGLYPH_XOFF (prev)) + (base_x - width) - mark_x;
	    yoff = XINT (LGLYPH_YOFF (prev)) + mark_y - base_y;
	  }
	}
      if (g->GlyphClass == OTF_GlyphClass0)
	base = mark = glyph;
      else if (g->GlyphClass == OTF_GlyphClassMark)
	mark = glyph;
      else
	base = glyph;

      LGLYPH_SET_XOFF (glyph, make_number (xoff));
      LGLYPH_SET_YOFF (glyph, make_number (yoff));
      LGLYPH_SET_WADJUST (glyph, make_number (width_adjust));
    }

  free (otf_gstring.glyphs);  
  return 0;
}

#endif	/* HAVE_LIBOTF */


/* glyph-string handler */

/* GSTRING is a vector of this form:
	[ [FONT-OBJECT LBEARING RBEARING WITH ASCENT DESCENT] GLYPH ... ]
   and GLYPH is a vector of this form:
	[ FROM-IDX TO-IDX C CODE X-OFF Y-OFF WIDTH WADJUST ]
   where
	FROM-IDX and TO-IDX are used internally and should not be touched.
	C is a character of the glyph.
	CODE is a glyph-code of C in FONT-OBJECT.
	X-OFF and Y-OFF are offests to the base position for the glyph.
	WIDTH is a normal width of the glyph.
	WADJUST is an adjustment to the normal width of the glyph.  */

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
      unsigned code = XINT (LGLYPH_CODE (g));
      struct font_metrics metrics;

      font->driver->text_extents (font, &code, 1, &metrics);
      LGLYPH_SET_WIDTH (g, make_number (metrics.width));
      metrics.lbearing += XINT (LGLYPH_XOFF (g));
      metrics.rbearing += XINT (LGLYPH_XOFF (g));
      metrics.ascent += XINT (LGLYPH_YOFF (g));
      metrics.descent += XINT (LGLYPH_YOFF (g));

      if (cmp->lbearing > cmp->pixel_width + metrics.lbearing)
	cmp->lbearing = cmp->pixel_width + metrics.lbearing;
      if (cmp->rbearing < cmp->pixel_width + metrics.rbearing)
	cmp->rbearing = cmp->pixel_width + metrics.rbearing;
      if (cmp->ascent < metrics.ascent)
	cmp->ascent = metrics.ascent;
      if (cmp->descent < metrics.descent)
	cmp->descent = metrics.descent;
      cmp->pixel_width += metrics.width + XINT (LGLYPH_WADJUST (g));
    }
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
    if (NILP (ftype) || EQ (driver_list->driver->type, ftype))
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
  struct font *font;
  Lisp_Object objlist = AREF (font->entity, FONT_OBJLIST_INDEX);
  Lisp_Object tail, prev = Qnil;

  for (prev = Qnil, tail = objlist; CONSP (tail);
       prev = tail, tail = XCDR (tail))
    if (EQ (font_object, XCAR (tail)))
      {
	struct Lisp_Save_Value *p = XSAVE_VALUE (font_object);

	xassert (p->integer > 0);
	p->integer--;
	if (p->integer == 0)
	  {
	    if (font->driver->close)
	      font->driver->close (f, p->pointer);
	    p->pointer = NULL;
	    if (NILP (prev))
	      ASET (font->entity, FONT_OBJLIST_INDEX, XCDR (objlist));
	    else
	      XSETCDR (prev, XCDR (objlist));
	  }
	break;
      }
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

  if (NILP (spec))
    for (i = 0; i < FONT_SPEC_MAX; i++)
      ASET (scratch_font_spec, i, Qnil);
  else
    for (i = 0; i < FONT_SPEC_MAX; i++)
      ASET (scratch_font_spec, i, AREF (spec, i));

  if (NILP (AREF (scratch_font_spec, FONT_FAMILY_INDEX))
      && ! NILP (lface[LFACE_FAMILY_INDEX]))
    font_merge_old_spec (Qnil, lface[LFACE_FAMILY_INDEX], Qnil,
			 scratch_font_spec);
  if (NILP (AREF (scratch_font_spec, FONT_REGISTRY_INDEX)))
    ASET (scratch_font_spec, FONT_REGISTRY_INDEX, intern ("iso8859-1"));

  if (NILP (AREF (scratch_font_spec, FONT_SIZE_INDEX))
      && ! NILP (lface[LFACE_HEIGHT_INDEX]))
    {
      double pt = XINT (lface[LFACE_HEIGHT_INDEX]);
      int pixel_size = POINT_TO_PIXEL (pt / 10, f->resy);

      ASET (scratch_font_spec, FONT_SIZE_INDEX, make_number (pixel_size));
    }

  XSETFRAME (frame, f);
  entities = font_list_entities (frame, scratch_font_spec);
  while (ASIZE (entities) == 0)
    {
      if (! NILP (AREF (scratch_font_spec, FONT_SIZE_INDEX))
	  && (NILP (spec) || NILP (AREF (spec, FONT_SIZE_INDEX))))
	{
	  ASET (scratch_font_spec, FONT_SIZE_INDEX, Qnil);
	  entities = font_list_entities (frame, scratch_font_spec);
	}
      else if (! NILP (AREF (scratch_font_spec, FONT_FOUNDRY_INDEX))
	  && (NILP (spec) || NILP (AREF (spec, FONT_FOUNDRY_INDEX))))
	{
	  ASET (scratch_font_spec, FONT_FOUNDRY_INDEX, Qnil);
	  entities = font_list_entities (frame, scratch_font_spec);
	}
      else if (! NILP (AREF (scratch_font_spec, FONT_FAMILY_INDEX))
	       && (NILP (spec) || NILP (AREF (spec, FONT_FAMILY_INDEX))))
	{
	  ASET (scratch_font_spec, FONT_FAMILY_INDEX, Qnil);
	  entities = font_list_entities (frame, scratch_font_spec);
	}
      else
	return Qnil;
    }

  if (ASIZE (entities) > 1)
    {
      Lisp_Object prefer = scratch_font_prefer, val;
      double pt;

      ASET (prefer, FONT_WEIGHT_INDEX,
	    font_prop_validate_style (FONT_WEIGHT_INDEX,
				      lface[LFACE_WEIGHT_INDEX]));
      ASET (prefer, FONT_SLANT_INDEX,
	    font_prop_validate_style (FONT_SLANT_INDEX,
				      lface[LFACE_SLANT_INDEX]));
      ASET (prefer, FONT_WIDTH_INDEX,
	    font_prop_validate_style (FONT_WIDTH_INDEX,
				      lface[LFACE_SWIDTH_INDEX]));
      pt = XINT (lface[LFACE_HEIGHT_INDEX]);
      ASET (prefer, FONT_SIZE_INDEX, make_float (pt / 10));

      font_sort_entites (entities, prefer, frame, spec);
    }

  return AREF (entities, 0);
}

Lisp_Object
font_open_for_lface (f, lface, entity)
     FRAME_PTR f;
     Lisp_Object *lface;
     Lisp_Object entity;
{
  double pt = XINT (lface[LFACE_HEIGHT_INDEX]);
  int size;

  pt /= 10;
  size = POINT_TO_PIXEL (pt, f->resy);
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
	font_object = font_open_for_lface (f, face->lface, entity);
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
  Lisp_Object spec, prefer, size, entities;
  Lisp_Object frame;
  struct font_driver_list *dlist;
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

  entities = Flist_fonts (spec, frame, make_number (1), prefer);
  return (NILP (entities)
	  ? Qnil
	  : font_open_entity (f, XCAR (entities), pixel_size));
}


/* Register font-driver DRIVER.  This function is used in two ways.

   The first is with frame F non-NULL.  In this case, DRIVER is
   registered to be used for drawing characters on F.  All frame
   creaters (e.g. Fx_create_frame) must call this function at least
   once with an available font-driver.

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
    if (list->driver->type == driver->type)
      error ("Duplicated font driver: %s", SDATA (SYMBOL_NAME (driver->type)));

  list = malloc (sizeof (struct font_driver_list));
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
  Lisp_Object extra = Qnil, name = Qnil;
  int i;

  for (i = 0; i < nargs; i += 2)
    {
      enum font_property_index prop;
      Lisp_Object key = args[i], val = args[i + 1];

      prop = check_font_prop_name (key);
      if (prop < FONT_EXTRA_INDEX)
	ASET (spec, prop, (font_property_table[prop].validater) (prop, val));
      else
	{
	  if (EQ (key, QCname))
	    name = val;
	  else
	    extra = Fcons (Fcons (key, val), extra);
	}
    }  
  ASET (spec, FONT_EXTRA_INDEX, extra);
  if (STRINGP (name))
    font_parse_name (SDATA (name), spec, 0);
  return spec;
}


DEFUN ("font-get", Ffont_get, Sfont_get, 2, 2, 0,
       doc: /* Return the value of FONT's PROP property.
FONT may be a font-spec or font-entity.
If FONT is font-entity and PROP is :extra, always nil is returned.  */)
     (font, prop)
     Lisp_Object font, prop;
{
  enum font_property_index idx;

  CHECK_FONT (font);
  idx = check_font_prop_name (prop);
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
  idx = check_font_prop_name (prop);
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
Optional 4th argument PREFER, if non-nil, is a font-spec to sort fonts
by closeness to PREFER.  */)
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
		      Lisp_Object objlist = AREF (entity, FONT_OBJLIST_INDEX);

		      for (; CONSP (objlist); objlist = XCDR (objlist))
			{
			  Lisp_Object val = XCAR (objlist);
			  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
			  struct font *font = p->pointer;

			  xassert (font
				   && driver_list->driver == font->driver);
			  driver_list->driver->close (f, font);
			  p->pointer = NULL;
			  p->integer = 0;
			}
		      if (driver_list->driver->free_entity)
			driver_list->driver->free_entity (entity);
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
       doc: /* Return a newly created glyph-string for FONT-OBJECT with NUM glyphs.
FONT-OBJECT may be nil if it is not yet known.  */)
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
    ASET (gstring, i, Fmake_vector (make_number (8), make_number (0)));
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
    font_object = Faref (Faref (gstring, make_number (0)), make_number (0));
  CHECK_FONT_GET_OBJECT (font_object, font);

  if (STRINGP (object))
    {
      const unsigned char *p;

      CHECK_NATNUM (start);
      CHECK_NATNUM (end);
      if (XINT (start) > XINT (end)
	  || XINT (end) > ASIZE (object)
	  || XINT (end) - XINT (start) >= XINT (Flength (gstring)))
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
	  ASET (g, 0, make_number (i));
	  ASET (g, 1, make_number (i + 1));
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
      if (XINT (end) - XINT (start) > len)
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
	  ASET (g, 0, make_number (i));
	  ASET (g, 1, make_number (i + 1));
	  LGLYPH_SET_CHAR (g, make_number (c));
	  LGLYPH_SET_CODE (g, make_number (code));
	}
    }
  return Qnil;
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
       doc: /* Return information about FONT-OBJECT.  */)
     (font_object)
     Lisp_Object font_object;
{
  struct font *font;
  Lisp_Object val;

  CHECK_FONT_GET_OBJECT (font_object, font);

  val = Fmake_vector (make_number (9), Qnil);
  ASET (val, 0, Ffont_xlfd_name (font_object));
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
    ASET (val, 8, font->driver->otf_capability (font));
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
  /* Note that sort_shift_bits[FONT_SLANT_TYPE] is never used.  */

  staticpro (&font_style_table);
  font_style_table = Fmake_vector (make_number (3), Qnil);

  staticpro (&font_family_alist);
  font_family_alist = Qnil;

  DEFSYM (Qfontp, "fontp");

  DEFSYM (Qiso8859_1, "iso8859-1");
  DEFSYM (Qiso10646_1, "iso10646-1");
  DEFSYM (Qunicode_bmp, "unicode-bmp");

  DEFSYM (QCotf, ":otf");
  DEFSYM (QClanguage, ":language");
  DEFSYM (QCscript, ":script");

  DEFSYM (QCfoundry, ":foundry");
  DEFSYM (QCadstyle, ":adstyle");
  DEFSYM (QCregistry, ":registry");
  DEFSYM (QCspacing, ":spacing");
  DEFSYM (QCdpi, ":dpi");
  DEFSYM (QCextra, ":extra");

  staticpro (&null_string);
  null_string = build_string ("");
  staticpro (&null_vector);
  null_vector = Fmake_vector (make_number (0), Qnil);

  staticpro (&scratch_font_spec);
  scratch_font_spec = Ffont_spec (0, NULL);
  staticpro (&scratch_font_prefer);
  scratch_font_prefer = Ffont_spec (0, NULL);

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

#ifdef FONT_DEBUG
  defsubr (&Sopen_font);
  defsubr (&Sclose_font);
  defsubr (&Squery_font);
  defsubr (&Sget_font_glyphs);
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
