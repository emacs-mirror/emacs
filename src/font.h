/* font.h -- Interface definition for font handling.
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

#ifndef EMACS_FONT_H
#define EMACS_FONT_H

#include "ccl.h"

/* We have three types of Lisp objects related to font.

   FONT-SPEC

	Pseudo vector (length FONT_SPEC_MAX) of font properties.  Some
	properties can be left unspecified (i.e. nil).  Emacs asks
	font-drivers to find a font by FONT-SPEC.  A fontset entry
	specifies requisite properties whereas a face specifies just
	preferable properties.

   FONT-ENTITY

	Pseudo vector (length FONT_ENTITY_MAX) of fully instanciated
	font properties that a font-driver returns upon a request of
	FONT-SPEC.

	Note: Only the method `list' and `match' of a font-driver can
	create this object, and should never be modified by Lisp.

   FONT-OBJECT

	Pseudo vector (length FONT_OBJECT_MAX) of a opend font.

	Lisp object encapsulating "struct font".  This corresponds to
	an opened font.

	Note: Only the method `open' of a font-driver can create this
	object, and should never be modified by Lisp.  */

extern Lisp_Object Qfont_spec, Qfont_entity, Qfont_object;


struct font_driver;
struct font;

/* An enumerator for each font property.  This is used as an index to
   the vector of FONT-SPEC and FONT-ENTITY.

   Note: The order is important and should not be changed.  */

enum font_property_index
  {
    /* FONT-TYPE is a symbol indicating a font backend; currently `x',
       `xft', `ftx' are available on X and gdi on Windows.
       For Windows, `bdf' and `uniscribe' backends are in progress.
       For Mac OS X, we need `atm'.  */
    FONT_TYPE_INDEX,

    /* FONT-FOUNDRY is a foundry name (symbol).  */
    FONT_FOUNDRY_INDEX,

    /* FONT-FAMILY is a family name (symbol).  */
    FONT_FAMILY_INDEX,

    /* FONT-ADSTYLE is an additional style name (symbol).  */
    FONT_ADSTYLE_INDEX,

    /* FONT-REGISTRY is a combination of a charset-registry and
       charset-encoding name (symbol).  */
    FONT_REGISTRY_INDEX,

    /* FONT-WEIGHT is a numeric value of weight (e.g. medium, bold) of
       the font.  The lowest 8-bit is an index determining the
       symbolic name, and the higher bits is the actual numeric value
       defined in `font-weight-table'. */
    FONT_WEIGHT_INDEX,

    /* FONT-SLANT is a numeric value of slant (e.g. r, i, o) of the
       font.  The lowest 8-bit is an index determining the symbolic
       name, and the higher bits is the actual numeric value defined
       in `font-slant-table'.  */
    FONT_SLANT_INDEX,

    /* FONT-WIDTH is a numeric value of setwidth (e.g. normal) of the
       font.  The lowest 8-bit is an index determining the symbolic
       name, and the higher bits is the actual numeric value defined
       `font-width-table'.  */
    FONT_WIDTH_INDEX,

    /* FONT-SIZE is a size of the font.  If integer, it is a pixel
       size.  For a font-spec, the value can be float specifying a
       point size.  The value zero means that the font is
       scalable.  */
    FONT_SIZE_INDEX,

    /* FONT-DPI is a resolution (dot per inch) for which the font is
       designed. */
    FONT_DPI_INDEX,

    /* FONT-SPACING is a spacing (mono, proportional, charcell) of the
       font (integer; one of enum font_spacing).  */
    FONT_SPACING_INDEX,

    /* FONT-AVGWIDTH is an average width (1/10 pixel unit) of the
       font.  */
    FONT_AVGWIDTH_INDEX,

#if 0
    /* The following two members are to substitute for the above 6
       members (FONT_WEIGHT_INDEX to FONT_AVGWIDTH_INDEX excluding
       FONT_SIZE_INDEX) if it is found that font-entities consumes too
       much memory.  */

    /* FONT-STYLE is a 24-bit integer containing indices for
       style-related properties WEIGHT, SLANT, and WIDTH.  The lowest
       8-bit is an indice to the weight table AREF (font_style_table,
       0), the next 8-bit is an indice to the slant table AREF
       (font_style_table, 1), the highest 8-bit is an indice to the
       slant table AREF (font_style_table, 2).  The indice 0 indicates
       that the corresponding style is not specified.  This way, we
       can represent at most 255 different names for each style, which
       is surely sufficient.  */
    FONT_STYLE_INDEX,

    /* FONT-METRICS is a 27-bit integer containing metrics-related
       properties DPI, AVGWIDTH, SPACING.  The lowest 12-bit is for
       DPI, the next 12-bit is for AVGWIDTH, the highest 3-bit is for
       SPACING.  In each bit field, the highest bit indicates that the
       corresponding value is set or not.  This way, we can represent
       DPI by 11-bit (0 to 2047), AVGWIDTH by 11-bit (0 to 2047),
       SPACING by 3-bit (0 for proportional, 1 for dual, 2 for mono, 3
       for charcell), which is surely sufficient.  */
    FONT_METRICS_INDEX,
#endif

    /* In a font-spec, the value is an alist of extra information of a
       font such as name, OpenType features, and language coverage.
       In addition, in a font-entity, the value may contain a pair
       (font-entity . INFO) where INFO is an extra infomation to
       identify a font (font-driver dependent).  */
    FONT_EXTRA_INDEX,		/* alist		alist */

    /* This value is the length of font-spec vector.  */
    FONT_SPEC_MAX,

    /* The followings are used only for a font-entity.  */

    /* List of font-objects opened from the font-entity.  */
    FONT_OBJLIST_INDEX = FONT_SPEC_MAX,

    /* This value is the length of font-entity vector.  */
    FONT_ENTITY_MAX,

    /* XLFD name of the font (string). */
    FONT_NAME_INDEX = FONT_ENTITY_MAX,
    
    /* Full name of the font (string).  It is the name extracted from
       the opend font, and may be different from the above.  It may be
       nil if the opened font doesn't give a name.  */
    FONT_FULLNAME_INDEX,

    /* File name of the font or nil if a file associated with the font
       is not available.  */
    FONT_FILE_INDEX,

    /* Format of the font (symbol).  */
    FONT_FORMAT_INDEX,

    /* This value is the length of font-object vector.  */
    FONT_OBJECT_MAX
  };

/* Return the numeric weight value of FONT.  */
#define FONT_WEIGHT_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_WEIGHT_INDEX))	\
   ? (XINT (AREF ((font), FONT_WEIGHT_INDEX)) >> 8) : -1)
/* Return the numeric slant value of FONT.  */
#define FONT_SLANT_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_SLANT_INDEX))	\
   ? (XINT (AREF ((font), FONT_SLANT_INDEX)) >> 8) : -1)
/* Return the numeric width value of FONT.  */
#define FONT_WIDTH_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_WIDTH_INDEX))	\
   ? (XINT (AREF ((font), FONT_WIDTH_INDEX)) >> 8) : -1)
/* Return the symbolic weight value of FONT.  */
#define FONT_WEIGHT_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_WEIGHT_INDEX, 0)
/* Return the symbolic slant value of FONT.  */
#define FONT_SLANT_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_SLANT_INDEX, 0)
/* Return the symbolic width value of FONT.  */
#define FONT_WIDTH_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_WIDTH_INDEX, 0)
/* Return the face-weight corresponding to the weight of FONT.  */
#define FONT_WEIGHT_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_WEIGHT_INDEX, 1)
/* Return the face-slant corresponding to the slant of FONT.  */
#define FONT_SLANT_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_SLANT_INDEX, 1)
/* Return the face-swidth corresponding to the slant of FONT.  */
#define FONT_WIDTH_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_WIDTH_INDEX, 1)

/* Return the numeric weight value corresponding ot the symbol NAME.  */
#define FONT_WEIGHT_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_WEIGHT_INDEX, (name), 0) >> 8)
/* Return the numeric slant value corresponding ot the symbol NAME.  */
#define FONT_SLANT_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_SLANT_INDEX, (name), 0) >> 8)
/* Return the numeric width value corresponding ot the symbol NAME.  */
#define FONT_WIDTH_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_WIDTH_INDEX, (name), 0) >> 8)

/* Set the font property PROP of FONT to VAL.  PROP is one of
   style-related font property index (FONT_WEIGHT/SLANT/WIDTH_INDEX).
   VAL (integer or symbol) is the numeric or symbolic style value.  */
#define FONT_SET_STYLE(font, prop, val)	\
  ASET ((font), prop, make_number (font_style_to_value (prop, val, 1)))

extern Lisp_Object QCspacing, QCdpi, QCscalable, QCotf, QClang, QCscript;
extern Lisp_Object QCavgwidth, QCfont_entity, QCfc_unknown_spec;

/* Important character set symbols.  */
extern Lisp_Object Qiso8859_1, Qiso10646_1, Qunicode_bmp, Qunicode_sip;

/* Structure for a font-spec.  */

struct font_spec
{
  EMACS_UINT size;
  struct Lisp_Vector *next;
  Lisp_Object props[FONT_SPEC_MAX];
};

/* Structure for a font-entity.  */

struct font_entity
{
  EMACS_UINT size;
  struct Lisp_Vector *next;
  Lisp_Object props[FONT_ENTITY_MAX];
};

/* A value which may appear in the member `encoding' of struct font
   indicating that a font itself doesn't tell which encoding to be
   used.  */
#define FONT_ENCODING_NOT_DECIDED 255

/* Structure for a font-object.  */

struct font
{
  EMACS_UINT size;
  struct Lisp_Vector *next;

  /* All Lisp_Object components must come first.
     That ensures they are all aligned normally.  */

  Lisp_Object props[FONT_OBJECT_MAX];

  /* Beyond here, there should be no more Lisp_Object components.  */

  /* Maximum bound width over all existing characters of the font.  On
     X window, this is same as (font->max_bounds.width).  */
  int max_width;

  /* By which pixel size the font is opened.  */
  int pixel_size;

  /* Height of the font.  On X window, this is the same as
     (font->ascent + font->descent).  */
  int height;

  /* Width of the space glyph of the font.  If the font doesn't have a
     SPACE glyph, the value is 0.  */
  int space_width;

  /* Average width of glyphs in the font.  If the font itself doesn't
     have that information but has glyphs of ASCII character, the
     value is the average with of those glyphs.  Otherwise, the value
     is 0.  */
  int average_width;

  /* Minimum glyph width (in pixels).  */
  int min_width;

  /* Ascent and descent of the font (in pixels).  */
  int ascent, descent;

  /* Vertical pixel width of the underline.  If is zero if that
     information is not in the font.  */
  int underline_thickness;

  /* Vertical pixel position (relative to the baseline) of the
     underline.  If it is positive, it is below the baseline.  It is
     negative if that information is not in the font.  */
  int underline_position;

  /* 1 if `vertical-centering-font-regexp' matches this font name.
     In this case, we render characters at vartical center positions
     of lines.  */
  int vertical_centering;

  /* Encoding type of the font.  The value is one of
     0, 1, 2, or 3:
	0: code points 0x20..0x7F or 0x2020..0x7F7F are used
	1: code points 0xA0..0xFF or 0xA0A0..0xFFFF are used
	2: code points 0x20A0..0x7FFF are used
	3: code points 0xA020..0xFF7F are used
     If the member `font_encoder' is not NULL, this member is ignored.  */
  unsigned char encoding_type;

  /* The baseline position of a font is normally `ascent' value of the
     font.  However, there exists many fonts which don't set `ascent'
     an appropriate value to be used as baseline position.  This is
     typical in such ASCII fonts which are designed to be used with
     Chinese, Japanese, Korean characters.  When we use mixture of
     such fonts and normal fonts (having correct `ascent' value), a
     display line gets very ugly.  Since we have no way to fix it
     automatically, it is users responsibility to supply well designed
     fonts or correct `ascent' value of fonts.  But, the latter
     requires heavy work (modifying all bitmap data in BDF files).
     So, Emacs accepts a private font property
     `_MULE_BASELINE_OFFSET'.  If a font has this property, we
     calculate the baseline position by subtracting the value from
     `ascent'.  In other words, the value indicates how many bits
     higher we should draw a character of the font than normal ASCII
     text for a better looking.

     We also have to consider the fact that the concept of `baseline'
     differs among scripts to which each character belongs.  For
     instance, baseline should be at the bottom most position of all
     glyphs for Chinese, Japanese, and Korean.  But, many of existing
     fonts for those characters doesn't have correct `ascent' values
     because they are designed to be used with ASCII fonts.  To
     display characters of different language on the same line, the
     best way will be to arrange them in the middle of the line.  So,
     in such a case, again, we utilize the font property
     `_MULE_BASELINE_OFFSET'.  If the value is larger than `ascent' we
     calculate baseline so that a character is arranged in the middle
     of a line.  */
  int baseline_offset;

  /* Non zero means a character should be composed at a position
     relative to the height (or depth) of previous glyphs in the
     following cases:
	(1) The bottom of the character is higher than this value.  In
	this case, the character is drawn above the previous glyphs.
	(2) The top of the character is lower than 0 (i.e. baseline
	height).  In this case, the character is drawn beneath the
	previous glyphs.

     This value is taken from a private font property
     `_MULE_RELATIVE_COMPOSE' which is introduced by Emacs.  */
  int relative_compose;

  /* Non zero means an ascent value to be used for a character
     registered in char-table `use-default-ascent'.  */
  int default_ascent;

  /* CCL program to calculate code points of the font.  */
  struct ccl_program *font_encoder;

  /* Font-driver for the font.  */
  struct font_driver *driver;

  /* Charset to encode a character code into a glyph code of the font.
     -1 means that the font doesn't require this information to encode
     a character.  */
  int encoding_charset;

  /* Charset to check if a character code is supported by the font.
     -1 means that the contents of the font must be looked up to
     determine it.  */
  int repertory_charset;

  /* There will be more to this structure, but they are private to a
     font-driver.  */
};

enum font_spacing
  {
    FONT_SPACING_PROPORTIONAL = 0,
    FONT_SPACING_DUAL = 90,
    FONT_SPACING_MONO = 100,
    FONT_SPACING_CHARCELL = 110
  };

struct font_metrics
{
  short lbearing, rbearing, width, ascent, descent;
};

struct font_bitmap
{
  int bits_per_pixel;
  int rows;
  int width;
  int pitch;
  unsigned char *buffer;
  int left;
  int top;
  int advance;
  void *extra;
};

/* Predicates to check various font-related objects.  */

/* 1 iff X is one of font-spec, font-entity, and font-object.  */
#define FONTP(x) PSEUDOVECTORP (x, PVEC_FONT)
/* 1 iff X is font-spec.  */
#define FONT_SPEC_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_SPEC_MAX)
/* 1 iff X is font-entity.  */
#define FONT_ENTITY_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_ENTITY_MAX)
/* 1 iff X is font-object.  */
#define FONT_OBJECT_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_OBJECT_MAX)

/* 1 iff ENTITY can't be loaded.  */
#define FONT_ENTITY_NOT_LOADABLE(entity)	\
  EQ (AREF (entity, FONT_OBJLIST_INDEX), Qt)

/* Flag ENTITY not loadable.  */
#define FONT_ENTITY_SET_NOT_LOADABLE(entity)	\
  ASET (entity, FONT_OBJLIST_INDEX, Qt)


/* Check macros for various font-related objects.  */

#define CHECK_FONT(x)	\
  do { if (! FONTP (x)) wrong_type_argument (Qfont, x); } while (0)
#define CHECK_FONT_SPEC(x)	\
  do { if (! FONT_SPEC_P (x)) wrong_type_argument (Qfont_spec, x); } while (0)
#define CHECK_FONT_ENTITY(x)	\
  do { if (! FONT_ENTITY_P (x)) wrong_type_argument (Qfont_entity, x); } while (0)
#define CHECK_FONT_OBJECT(x)	\
  do { if (! FONT_OBJECT_P (x)) wrong_type_argument (Qfont_object, x); } while (0)

#define CHECK_FONT_GET_OBJECT(x, font)	\
  do {					\
    CHECK_FONT_OBJECT (x);		\
    font = XFONT_OBJECT (x);		\
  } while (0)

#define XFONT_SPEC(p)	\
  (eassert (FONT_SPEC_P(p)), (struct font_spec *) XPNTR (p))
#define XFONT_ENTITY(p)	\
  (eassert (FONT_ENTITY_P(p)), (struct font_entity *) XPNTR (p))
#define XFONT_OBJECT(p)	\
  (eassert (FONT_OBJECT_P(p)), (struct font *) XPNTR (p))
#define XSETFONT(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_FONT))

/* Number of pt per inch (from the TeXbook).  */
#define PT_PER_INCH 72.27

/* Return a pixel size (integer) corresponding to POINT size (double)
   on resolution DPI.  */
#define POINT_TO_PIXEL(POINT, DPI) ((POINT) * (DPI) / PT_PER_INCH + 0.5)

/* Return a point size (double) corresponding to POINT size (integer)
   on resolution DPI.  */
#define PIXEL_TO_POINT(PIXEL, DPI) ((PIXEL) * PT_PER_INCH / (DPI) + 0.5)

/* Ignore the difference of font pixel sizes less than or equal to
   this value.  */
#define FONT_PIXEL_SIZE_QUANTUM 1

struct face;
struct composition;

/* Macros for lispy glyph-string.  */
enum lgstring_indices
  {
    LGSTRING_IX_FONT, LGSTRING_IX_WIDTH,
    LGSTRING_IX_LBEARING, LGSTRING_IX_RBEARING,
    LGSTRING_IX_ASCENT, LGSTRING_IX_DESCENT
  };
#define LGSTRING_SLOT(lgs, ix) AREF (AREF ((lgs), 0), ix)
#define LGSTRING_FONT(lgs) LGSTRING_SLOT (lgs, LGSTRING_IX_FONT)
#define LGSTRING_WIDTH(lgs) XINT (LGSTRING_SLOT (lgs, LGSTRING_IX_WIDTH))
#define LGSTRING_LBEARING(lgs) XINT (LGSTRING_SLOT (lgs, LGSTRING_IX_LBEARING))
#define LGSTRING_RBEARING(lgs) XINT (LGSTRING_SLOT (lgs, LGSTRING_IX_RBEARING))
#define LGSTRING_ASCENT(lgs) XINT (LGSTRING_SLOT (lgs, LGSTRING_IX_ASCENT))
#define LGSTRING_DESCENT(lgs) XINT (LGSTRING_SLOT (lgs, LGSTRING_IX_DESCENT))
#define LGSTRING_SET_SLOT(lgs, ix, val) ASET (AREF ((lgs), 0), ix, (val))
#define LGSTRING_SET_FONT(lgs, val)     \
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_FONT, (val))
#define LGSTRING_SET_WIDTH(lgs, val)	\
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_WIDTH, make_number (val))
#define LGSTRING_SET_LBEARING(lgs, val) \
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_LBEARING, make_number (val))
#define LGSTRING_SET_RBEARING(lgs, val)	\
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_RBEARING, make_number (val))
#define LGSTRING_SET_ASCENT(lgs, val)	\
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_ASCENT, make_number (val))
#define LGSTRING_SET_DESCENT(lgs, val)	\
  LGSTRING_SET_SLOT(lgs, LGSTRING_IX_DESCENT, make_number (val))

#define LGSTRING_LENGTH(lgs) (ASIZE ((lgs)) - 1)
#define LGSTRING_GLYPH(lgs, idx) AREF ((lgs), (idx) + 1)
#define LGSTRING_SET_GLYPH(lgs, idx, val) ASET ((lgs), (idx) + 1, (val))

/* Vector size of Lispy glyph.  */
enum lglyph_indices
  {
    LGLYPH_IX_FROM, LGLYPH_IX_TO,  LGLYPH_IX_CHAR, LGLYPH_IX_CODE,
    LGLYPH_IX_WIDTH, LGLYPH_IX_LBEARING, LGLYPH_IX_RBEARING,
    LGLYPH_IX_ASCENT, LGLYPH_IX_DESCENT, LGLYPH_IX_ADJUSTMENT,
    /* Not an index.  */
    LGLYPH_SIZE
  };
#define LGLYPH_FROM(g) XINT (AREF ((g), LGLYPH_IX_FROM))
#define LGLYPH_TO(g) XINT (AREF ((g), LGLYPH_IX_TO))
#define LGLYPH_CHAR(g) XINT (AREF ((g), LGLYPH_IX_CHAR))
#define LGLYPH_CODE(g) XUINT (AREF ((g), LGLYPH_IX_CODE))
#define LGLYPH_WIDTH(g) XINT (AREF ((g), LGLYPH_IX_WIDTH))
#define LGLYPH_LBEARING(g) XINT (AREF ((g), LGLYPH_IX_LBEARING))
#define LGLYPH_RBEARING(g) XINT (AREF ((g), LGLYPH_IX_RBEARING))
#define LGLYPH_ASCENT(g) XINT (AREF ((g), LGLYPH_IX_ASCENT))
#define LGLYPH_DESCENT(g) XINT (AREF ((g), LGLYPH_IX_DESCENT))
#define LGLYPH_ADJUSTMENT(g) AREF ((g), LGLYPH_IX_ADJUSTMENT)
#define LGLYPH_SET_FROM(g, val) ASET ((g), LGLYPH_IX_FROM, make_number (val))
#define LGLYPH_SET_TO(g, val) ASET ((g), LGLYPH_IX_TO, make_number (val))
#define LGLYPH_SET_CHAR(g, val) ASET ((g), LGLYPH_IX_CHAR, make_number (val))
/* FIXME: we should use make_uint_number here.  */
#define LGLYPH_SET_CODE(g, val) ASET ((g), LGLYPH_IX_CODE, make_number (val))
#define LGLYPH_SET_WIDTH(g, val) ASET ((g), LGLYPH_IX_WIDTH, make_number (val))
#define LGLYPH_SET_LBEARING(g, val) ASET ((g), LGLYPH_IX_RBEARING, make_number (val))
#define LGLYPH_SET_RBEARING(g, val) ASET ((g), LGLYPH_IX_LBEARING, make_number (val))
#define LGLYPH_SET_ASCENT(g, val) ASET ((g), LGLYPH_IX_ASCENT, make_number (val))
#define LGLYPH_SET_DESCENT(g, val) ASET ((g), LGLYPH_IX_DESCENT, make_number (val))
#define LGLYPH_SET_ADJUSTMENT(g, val) ASET ((g), LGLYPH_IX_ADJUSTMENT, (val))

#define LGLYPH_XOFF(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			? XINT (AREF (LGLYPH_ADJUSTMENT (g), 0)) : 0)
#define LGLYPH_YOFF(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			? XINT (AREF (LGLYPH_ADJUSTMENT (g), 1)) : 0)
#define LGLYPH_WADJUST(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			   ? XINT (AREF (LGLYPH_ADJUSTMENT (g), 2)) : 0)

#define FONT_INVALID_CODE 0xFFFFFFFF

/* Font driver.  Members specified as "optional" can be NULL.  */

struct font_driver
{
  /* Symbol indicating the type of the font-driver.  */
  Lisp_Object type;

  /* 1 iff the font's foundary, family, and adstyle names are case
     sensitve.  */
  int case_sensitive;

  /* Return a cache of font-entities on frame F.  The cache must be a
     cons whose cdr part is the actual cache area.  */
  Lisp_Object (*get_cache) P_ ((FRAME_PTR F));

  /* List fonts exactly matching with FONT_SPEC on FRAME.  The value
     is a list of font-entities.  It is assured that the properties
     WEIGHT to AVGWIDTH are all nil (i.e. not specified) in FONT_SPEC.
     This and the following `match' are the only APIs that allocate
     font-entities.  */
  Lisp_Object (*list) P_ ((Lisp_Object frame, Lisp_Object font_spec));

  /* Return a font entity most closely maching with FONT_SPEC on
     FRAME.  The closeness is detemined by the font backend, thus
     `face-font-selection-order' is ignored here.  */
  Lisp_Object (*match) P_ ((Lisp_Object frame, Lisp_Object font_spec));

  /* Optional.
     List available families.  The value is a list of family names
     (symbols).  */
  Lisp_Object (*list_family) P_ ((Lisp_Object frame));

  /* Optional (if FONT_EXTRA_INDEX is not Lisp_Save_Value).
     Free FONT_EXTRA_INDEX field of FONT_ENTITY.  */
  void (*free_entity) P_ ((Lisp_Object font_entity));

  /* Open a font specified by FONT_ENTITY on frame F.  If the font is
     scalable, open it with PIXEL_SIZE.  */
  Lisp_Object (*open) P_ ((FRAME_PTR f, Lisp_Object font_entity,
			   int pixel_size));

  /* Close FONT on frame F.  */
  void (*close) P_ ((FRAME_PTR f, struct font *font));

  /* Optional (if FACE->extra is not used).
     Prepare FACE for displaying characters by FONT on frame F by
     storing some data in FACE->extra.  If successful, return 0.
     Otherwise, return -1.  */
  int (*prepare_face) P_ ((FRAME_PTR f, struct face *face));

  /* Optional.
     Done FACE for displaying characters by FACE->font on frame F.  */
  void (*done_face) P_ ((FRAME_PTR f, struct face *face));

  /* Optional.
     If FONT_ENTITY has a glyph for character C (Unicode code point),
     return 1.  If not, return 0.  If a font must be opened to check
     it, return -1.  */
  int (*has_char) P_ ((Lisp_Object entity, int c));

  /* Return a glyph code of FONT for characer C (Unicode code point).
     If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
  unsigned (*encode_char) P_ ((struct font *font, int c));

  /* Computate the total metrics of the NGLYPHS glyphs specified by
     the font FONT and the sequence of glyph codes CODE, and store the
     result in METRICS.  */
  int (*text_extents) P_ ((struct font *font,
			   unsigned *code, int nglyphs,
			   struct font_metrics *metrics));

  /* Optional.
     Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
     position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
     is nonzero, fill the background in advance.  It is assured that
     WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars).  */
  int (*draw) P_ ((struct glyph_string *s, int from, int to,
		   int x, int y, int with_background));

  /* Optional.
     Store bitmap data for glyph-code CODE of FONT in BITMAP.  It is
     intended that this method is callled from the other font-driver
     for actual drawing.  */
  int (*get_bitmap) P_ ((struct font *font, unsigned code,
			 struct font_bitmap *bitmap,
			 int bits_per_pixel));

  /* Optional.
     Free bitmap data in BITMAP.  */
  void (*free_bitmap) P_ ((struct font *font, struct font_bitmap *bitmap));

  /* Optional.
     Return an outline data for glyph-code CODE of FONT.  The format
     of the outline data depends on the font-driver.  */
  void *(*get_outline) P_ ((struct font *font, unsigned code));

  /* Optional.
     Free OUTLINE (that is obtained by the above method).  */
  void (*free_outline) P_ ((struct font *font, void *outline));

  /* Optional.
     Get coordinates of the INDEXth anchor point of the glyph whose
     code is CODE.  Store the coordinates in *X and *Y.  Return 0 if
     the operations was successfull.  Otherwise return -1.  */
  int (*anchor_point) P_ ((struct font *font, unsigned code, int index,
			   int *x, int *y));

  /* Optional.
     Return a list describing which scripts/languages FONT
     supports by which GSUB/GPOS features of OpenType tables.  */
  Lisp_Object (*otf_capability) P_ ((struct font *font));

  /* Optional.
     Apply FONT's OTF-FEATURES to the glyph string.

     FEATURES specifies which OTF features to apply in this format:
	(SCRIPT LANGSYS GSUB-FEATURE GPOS-FEATURE)
     See the documentation of `font-drive-otf' for the detail.

     This method applies the specified features to the codes in the
     elements of GSTRING-IN (between FROMth and TOth).  The output
     codes are stored in GSTRING-OUT at the IDXth element and the
     following elements.

     Return the number of output codes.  If none of the features are
     applicable to the input data, return 0.  If GSTRING-OUT is too
     short, return -1.  */
  int (*otf_drive) P_ ((struct font *font, Lisp_Object features,
		       Lisp_Object gstring_in, int from, int to,
		       Lisp_Object gstring_out, int idx, int alternate_subst));

  /* Optional.
     Make the font driver ready for frame F.  Usually this function
     makes some data specific to F and store it in F by calling
     font_put_frame_data ().  */
  int (*start_for_frame) P_ ((FRAME_PTR f));
  
  /* Optional.
     End using the driver for frame F.  Usually this function free
     some data stored for F.  */
  int (*end_for_frame) P_ ((FRAME_PTR f));

  /* Optional.

     Shape text in LGSTRING.  See the docstring of `font-make-gstring'
     for the format of LGSTRING.  If the (N+1)th element of LGSTRING
     is nil, input of shaping is from the 1st to (N)th elements.  In
     each input glyph, FROM, TO, CHAR, and CODE are already set.

     This function updates all fields of the input glyphs.  If the
     output glyphs (M) are more than the input glyphs (N), (N+1)th
     through (M)th elements of LGSTRING are updated possibly by making
     a new glyph object and storing it in LGSTRING.  If (M) is greater
     than the length of LGSTRING, nil should be return.  In that case,
     this function is called again with the larger LGSTRING.  */
  Lisp_Object (*shape) P_ ((Lisp_Object lgstring));

  /* Optional.

     If FONT is usable on frame F, return 0.  Otherwise return -1.
   */
  int (*check) P_ ((FRAME_PTR F, struct font *font));
};


/* Chain of font drivers.  There's one global font driver list
   (font_driver_list in font.c).  In addition, each frame has it's own
   font driver list at FRAME_PTR->font_driver_list.  */

struct font_driver_list
{
  /* 1 iff this driver is currently used.  It is igonred in the global
     font driver list.*/
  int on;
  /* Pointer to the font driver.  */
  struct font_driver *driver;
  /* Pointer to the next element of the chain.  */
  struct font_driver_list *next;
};


/* Chain of arbitrary data specific to each font driver.  Each frame
   has it's own font data list at FRAME_PTR->font_data_list.  */

struct font_data_list
{
  /* Pointer to the font driver.  */
  struct font_driver *driver;
  /* Data specific to the font driver.  */
  void *data;
  /* Pointer to the next element of the chain.  */
  struct font_data_list *next;
};

EXFUN (Ffont_spec, MANY);
EXFUN (Fcopy_font_spec, 1);
EXFUN (Fmerge_font_spec, 2);
EXFUN (Ffont_get, 2);
EXFUN (Ffont_put, 3);
EXFUN (Flist_fonts, 4);
EXFUN (Ffont_family_list, 1);
EXFUN (Fclear_font_cache, 0);
EXFUN (Ffont_xlfd_name, 1);

extern Lisp_Object font_make_spec P_ ((void));
extern Lisp_Object font_make_entity P_ ((void));
extern Lisp_Object font_make_object P_ ((int));

extern int font_registry_charsets P_ ((Lisp_Object, struct charset **,
				       struct charset **));
extern int font_style_to_value P_ ((enum font_property_index prop,
				    Lisp_Object name, int noerror));
extern Lisp_Object font_style_symbolic P_ ((Lisp_Object font,
					    enum font_property_index prop,
					    int for_face));

extern int font_match_p P_ ((Lisp_Object spec, Lisp_Object entity));
extern Lisp_Object font_list_entities P_ ((Lisp_Object frame,
					   Lisp_Object spec));

extern Lisp_Object font_get_name P_ ((Lisp_Object font_object));
extern Lisp_Object font_spec_from_name P_ ((Lisp_Object font_name));
extern Lisp_Object font_get_frame P_ ((Lisp_Object font_object));
extern int font_has_char P_ ((FRAME_PTR, Lisp_Object, int));
extern unsigned font_encode_char P_ ((Lisp_Object, int));

extern void font_clear_prop P_ ((Lisp_Object *attrs,
				 enum font_property_index prop));
extern void font_update_lface P_ ((FRAME_PTR f, Lisp_Object *attrs));
extern Lisp_Object font_find_for_lface P_ ((FRAME_PTR f, Lisp_Object *lface,
					    Lisp_Object spec, int c));
extern Lisp_Object font_open_for_lface P_ ((FRAME_PTR f, Lisp_Object entity,
					    Lisp_Object *lface,
					    Lisp_Object spec));
extern Lisp_Object font_load_for_lface P_ ((FRAME_PTR f, Lisp_Object *lface,
					    Lisp_Object spec));
extern void font_prepare_for_face P_ ((FRAME_PTR f, struct face *face));
extern void font_done_for_face P_ ((FRAME_PTR f, struct face *face));

extern Lisp_Object font_open_by_name P_ ((FRAME_PTR f, char *name));
extern void font_close_object (FRAME_PTR f, Lisp_Object font_object);

extern Lisp_Object font_intern_prop P_ ((char *str, int len));
extern void font_update_sort_order P_ ((int *order));

extern void font_parse_family_registry P_ ((Lisp_Object family,
					    Lisp_Object registry,
					    Lisp_Object spec));
extern Lisp_Object font_spec_from_family_registry P_ ((Lisp_Object family,
						       Lisp_Object registry));

extern int font_parse_xlfd P_ ((char *name, Lisp_Object font));
extern int font_unparse_xlfd P_ ((Lisp_Object font, int pixel_size,
				  char *name, int bytes));
extern int font_parse_fcname P_ ((char *name, Lisp_Object font));
extern int font_unparse_fcname P_ ((Lisp_Object font, int pixel_size,
				  char *name, int bytes));
extern void register_font_driver P_ ((struct font_driver *driver, FRAME_PTR f));
extern void free_font_driver_list P_ ((FRAME_PTR f));
extern Lisp_Object font_update_drivers P_ ((FRAME_PTR f, Lisp_Object list));
extern Lisp_Object font_at P_ ((int c, EMACS_INT pos, struct face *face,
				struct window *w, Lisp_Object object));
extern EMACS_INT font_range P_ ((EMACS_INT pos, EMACS_INT limit,
				 struct face *face, FRAME_PTR f,
				 Lisp_Object object));

extern struct font *font_prepare_composition P_ ((struct composition *cmp,
						  FRAME_PTR f));

extern Lisp_Object font_put_extra P_ ((Lisp_Object font, Lisp_Object prop,
                                       Lisp_Object val));

extern int font_put_frame_data P_ ((FRAME_PTR f,
				    struct font_driver *driver,
				    void *data));
extern void *font_get_frame_data P_ ((FRAME_PTR f,
				      struct font_driver *driver));

#ifdef HAVE_FREETYPE
extern struct font_driver ftfont_driver;
#endif	/* HAVE_FREETYPE */
#ifdef HAVE_X_WINDOWS
extern struct font_driver xfont_driver;
extern struct font_driver ftxfont_driver;
#ifdef HAVE_XFT
extern struct font_driver xftfont_driver;
#endif	/* HAVE_XFT */
#endif	/* HAVE_X_WINDOWS */
#ifdef WINDOWSNT
extern struct font_driver w32font_driver;
extern struct font_driver uniscribe_font_driver;
#endif	/* WINDOWSNT */
#ifdef MAC_OS
extern struct font_driver atmfont_driver;
#endif	/* MAC_OS */

#endif	/* not EMACS_FONT_H */

/* arch-tag: 3b7260c3-5bec-4d6b-a0db-95c1b431b1a2
   (do not change this comment) */
