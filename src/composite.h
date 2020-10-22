/* Header for composite sequence handler.
   Copyright (C) 2001-2020 Free Software Foundation, Inc.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
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

#ifndef EMACS_COMPOSITE_H
#define EMACS_COMPOSITE_H

#include "font.h"
#include "dispextern.h"

INLINE_HEADER_BEGIN

/* Methods to display a sequence of components of a composition.  */
enum composition_method {
  /* Compose relatively without alternate characters.  */
  COMPOSITION_RELATIVE,
  /* Compose by specified composition rules.  This is not used in
     Emacs 21 but we need it to decode files saved in the older
     versions of Emacs.  */
  COMPOSITION_WITH_RULE,
  /* Compose relatively with alternate characters.  */
  COMPOSITION_WITH_ALTCHARS,
  /* Compose by specified composition rules with alternate characters.  */
  COMPOSITION_WITH_RULE_ALTCHARS,
  /* This is not a method.  */
  COMPOSITION_NO
};

/* Maximum number of components a single composition can have.  */
#define MAX_COMPOSITION_COMPONENTS 16

/* These operations access information about a composition that
   has `composition' property PROP.  PROP is:
	((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
   or
	(COMPOSITION-ID . (LENGTH COMPONENTS . MODIFICATION-FUNC))
   They don't check validity of PROP.  */

/* Return true if PROP is already registered.  */
INLINE bool
composition_registered_p (Lisp_Object prop)
{
  return FIXNUMP (XCAR (prop));
}

/* Return ID number of the already registered composition.  */
#define COMPOSITION_ID(prop) XFIXNUM (XCAR (prop))

/* Return length of the composition.  */
#define COMPOSITION_LENGTH(prop)	\
  (composition_registered_p (prop)	\
   ? XFIXNUM (XCAR (XCDR (prop)))		\
   : XFIXNUM (XCAR (XCAR (prop))))

/* Return components of the composition.  */
#define COMPOSITION_COMPONENTS(prop)	\
  (composition_registered_p (prop)	\
   ? XCAR (XCDR (XCDR (prop)))		\
   : XCDR (XCAR (prop)))

/* Return modification function of the composition.  */
#define COMPOSITION_MODIFICATION_FUNC(prop)	\
  (composition_registered_p (prop)		\
   ? XCDR (XCDR (XCDR (prop)))			\
   : CONSP (prop) ? XCDR (prop) : Qnil)

/* Return the Nth glyph of composition specified by CMP.  CMP is a
   pointer to `struct composition'.  */
#define COMPOSITION_GLYPH(cmp, n)					\
  XFIXNUM (XVECTOR (XVECTOR (XHASH_TABLE (composition_hash_table)		\
			  ->key_and_value)				\
		 ->contents[cmp->hash_index * 2])			\
	->contents[cmp->method == COMPOSITION_WITH_RULE_ALTCHARS	\
		  ? (n) * 2 : (n)])

/* Return the encoded composition rule to compose the Nth glyph of
   rule-base composition specified by CMP.  CMP is a pointer to
   `struct composition'. */
#define COMPOSITION_RULE(cmp, n)				\
  XFIXNUM (XVECTOR (XVECTOR (XHASH_TABLE (composition_hash_table)	\
			  ->key_and_value)			\
		 ->contents[cmp->hash_index * 2])		\
	->contents[(n) * 2 - 1])

/* Decode encoded composition rule RULE_CODE into GREF (global
   reference point code), NREF (new ref. point code).  Don't check RULE_CODE;
   always set GREF and NREF to valid values.  By side effect,
   RULE_CODE is modified.  */

#define COMPOSITION_DECODE_REFS(rule_code, gref, nref)			\
  do {									\
    rule_code &= 0xFF;							\
    gref = (rule_code) / 12;						\
    if (gref > 12) gref = 11;						\
    nref = (rule_code) % 12;						\
  } while (false)

/* Like COMPOSITION_DECODE_REFS (RULE_CODE, GREF, NREF), but also
   decode RULE_CODE into XOFF and YOFF (vertical offset).  */

#define COMPOSITION_DECODE_RULE(rule_code, gref, nref, xoff, yoff)	\
  do {									\
    xoff = (rule_code) >> 16;						\
    yoff = ((rule_code) >> 8) & 0xFF;					\
    COMPOSITION_DECODE_REFS (rule_code, gref, nref);			\
  } while (false)

/* True if the global reference point GREF and new reference point NREF are
   valid.  */
INLINE bool
COMPOSITION_ENCODE_RULE_VALID (int gref, int nref)
{
  return 0 <= gref && gref < 12 && 0 <= nref && nref < 12;
}

/* Return encoded composition rule for the pair of global reference
   point GREF and new reference point NREF.  Arguments must be valid.  */
#define COMPOSITION_ENCODE_RULE(gref, nref)		\
  ((gref) * 12 + (nref))

/* Data structure that records information about a composition
   currently used in some buffers or strings.

   When a composition is assigned an ID number (by
   get_composition_id), this structure is allocated for the
   composition and linked in composition_table[ID].

   Identical compositions appearing at different places have the same
   ID, and thus share the same instance of this structure.  */

struct composition {
  /* Number of glyphs of the composition components.  */
  int glyph_len;

  /* Width, ascent, and descent pixels of the composition.  */
  short pixel_width, ascent, descent;

  short lbearing, rbearing;

  /* How many columns the overall glyphs occupy on the screen.  This
     gives an approximate value for column calculation in
     Fcurrent_column, and etc.  */
  int width;

  /* Method of the composition.  */
  enum composition_method method;

  /* Index to the composition hash table.  */
  ptrdiff_t hash_index;

  /* For which font we have calculated the remaining members.  The
     actual type is device dependent.  */
  void *font;

  /* Pointer to an array of x-offset and y-offset (by pixels) of
     glyphs.  This points to a sufficient memory space (sizeof (short) *
     glyph_len * 2) that is allocated when the composition is
     registered in composition_table.  X-offset and Y-offset of Nth
     glyph are (2N)th and (2N+1)th elements respectively.  */
  short *offsets;
};

/* Table of pointers to the structure `composition' indexed by
   COMPOSITION-ID.  */
extern struct composition **composition_table;
/* Number of the currently registered compositions.  */
extern ptrdiff_t n_compositions;

/* Mask bits for CHECK_MASK arg to update_compositions.
   For a change in the region FROM and TO, check compositions ... */
#define CHECK_HEAD	1	/* adjacent to FROM */
#define CHECK_TAIL	2	/* adjacent to TO */
#define CHECK_INSIDE	4	/* between FROM and TO */
#define CHECK_BORDER	(CHECK_HEAD | CHECK_TAIL)
#define CHECK_ALL	(CHECK_BORDER | CHECK_INSIDE)

extern Lisp_Object composition_hash_table;
extern ptrdiff_t get_composition_id (ptrdiff_t, ptrdiff_t, ptrdiff_t,
				     Lisp_Object, Lisp_Object);
extern bool find_composition (ptrdiff_t, ptrdiff_t, ptrdiff_t *, ptrdiff_t *,
			      Lisp_Object *, Lisp_Object);
extern void update_compositions (ptrdiff_t, ptrdiff_t, int);
extern void make_composition_value_copy (Lisp_Object);
extern void syms_of_composite (void);
extern void compose_text (ptrdiff_t, ptrdiff_t, Lisp_Object, Lisp_Object,
                          Lisp_Object);

/* Return the method of a composition with property PROP.  */

INLINE enum composition_method
composition_method (Lisp_Object prop)
{
  if (composition_registered_p (prop))
    return composition_table[COMPOSITION_ID (prop)]->method;
  else
    {
      Lisp_Object temp = XCDR (XCAR (prop));
      return (NILP (temp)
	      ? COMPOSITION_RELATIVE
	      : FIXNUMP (temp) || STRINGP (temp)
	      ? COMPOSITION_WITH_ALTCHARS
	      : COMPOSITION_WITH_RULE_ALTCHARS);
    }
}

/* Given offsets START and END, return true if PROP is a valid composition
   property with length END - START.  */

INLINE bool
composition_valid_p (ptrdiff_t start, ptrdiff_t end, Lisp_Object prop)
{
  return (CONSP (prop)
	  && (composition_registered_p (prop)
	      ? (COMPOSITION_ID (prop) >= 0
		 && COMPOSITION_ID (prop) <= n_compositions
		 && CONSP (XCDR (prop)))
	      : (CONSP (XCAR (prop))
		  && (NILP (XCDR (XCAR (prop)))
		      || STRINGP (XCDR (XCAR (prop)))
		      || VECTORP (XCDR (XCAR (prop)))
		      || FIXNUMP (XCDR (XCAR (prop)))
		      || CONSP (XCDR (XCAR (prop))))))
	  && COMPOSITION_LENGTH (prop) == end - start);
}

/* Macros for lispy glyph-string.  This is completely different from
   struct glyph_string.  */

#define LGSTRING_HEADER(lgs) AREF (lgs, 0)
#define LGSTRING_SET_HEADER(lgs, header) ASET (lgs, 0, header)

#define LGSTRING_FONT(lgs) AREF (LGSTRING_HEADER (lgs), 0)
#define LGSTRING_CHAR(lgs, i) AREF (LGSTRING_HEADER (lgs), (i) + 1)
#define LGSTRING_CHAR_LEN(lgs) (ASIZE (LGSTRING_HEADER (lgs)) - 1)

#define LGSTRING_SET_FONT(lgs, val) ASET (LGSTRING_HEADER (lgs), 0, (val))
#define LGSTRING_SET_CHAR(lgs, i, c) ASET (LGSTRING_HEADER (lgs), (i) + 1, (c))

#define LGSTRING_ID(lgs) AREF (lgs, 1)
#define LGSTRING_SET_ID(lgs, id) ASET (lgs, 1, id)

#define LGSTRING_GLYPH_LEN(lgs) (ASIZE ((lgs)) - 2)
#define LGSTRING_GLYPH(lgs, idx) AREF ((lgs), (idx) + 2)
#define LGSTRING_SET_GLYPH(lgs, idx, val) ASET ((lgs), (idx) + 2, (val))
INLINE Lisp_Object *
lgstring_glyph_addr (Lisp_Object lgs, ptrdiff_t idx)
{
  return aref_addr (lgs, idx + 2);
}

/* Vector size of Lispy glyph.  */
enum lglyph_indices
  {
    LGLYPH_IX_FROM, LGLYPH_IX_TO,  LGLYPH_IX_CHAR, LGLYPH_IX_CODE,
    LGLYPH_IX_WIDTH, LGLYPH_IX_LBEARING, LGLYPH_IX_RBEARING,
    LGLYPH_IX_ASCENT, LGLYPH_IX_DESCENT, LGLYPH_IX_ADJUSTMENT,
    /* Not an index.  */
    LGLYPH_SIZE
  };

#define LGLYPH_NEW() make_nil_vector (LGLYPH_SIZE)
#define LGLYPH_FROM(g) XFIXNUM (AREF ((g), LGLYPH_IX_FROM))
#define LGLYPH_TO(g) XFIXNUM (AREF ((g), LGLYPH_IX_TO))
#define LGLYPH_CHAR(g) XFIXNUM (AREF ((g), LGLYPH_IX_CHAR))
#define LGLYPH_CODE(g)						\
  (NILP (AREF ((g), LGLYPH_IX_CODE))				\
   ? FONT_INVALID_CODE						\
   : cons_to_unsigned (AREF (g, LGLYPH_IX_CODE), TYPE_MAXIMUM (unsigned)))
#define LGLYPH_WIDTH(g) XFIXNUM (AREF ((g), LGLYPH_IX_WIDTH))
#define LGLYPH_LBEARING(g) XFIXNUM (AREF ((g), LGLYPH_IX_LBEARING))
#define LGLYPH_RBEARING(g) XFIXNUM (AREF ((g), LGLYPH_IX_RBEARING))
#define LGLYPH_ASCENT(g) XFIXNUM (AREF ((g), LGLYPH_IX_ASCENT))
#define LGLYPH_DESCENT(g) XFIXNUM (AREF ((g), LGLYPH_IX_DESCENT))
#define LGLYPH_ADJUSTMENT(g) AREF ((g), LGLYPH_IX_ADJUSTMENT)
#define LGLYPH_SET_FROM(g, val) ASET ((g), LGLYPH_IX_FROM, make_fixnum (val))
#define LGLYPH_SET_TO(g, val) ASET ((g), LGLYPH_IX_TO, make_fixnum (val))
#define LGLYPH_SET_CHAR(g, val) ASET ((g), LGLYPH_IX_CHAR, make_fixnum (val))
/* Callers must assure that VAL is not negative!  */
#define LGLYPH_SET_CODE(g, val)						\
  ASET (g, LGLYPH_IX_CODE,						\
	val == FONT_INVALID_CODE ? Qnil : INT_TO_INTEGER (val))

#define LGLYPH_SET_WIDTH(g, val) ASET ((g), LGLYPH_IX_WIDTH, make_fixnum (val))
#define LGLYPH_SET_LBEARING(g, val) ASET ((g), LGLYPH_IX_LBEARING, make_fixnum (val))
#define LGLYPH_SET_RBEARING(g, val) ASET ((g), LGLYPH_IX_RBEARING, make_fixnum (val))
#define LGLYPH_SET_ASCENT(g, val) ASET ((g), LGLYPH_IX_ASCENT, make_fixnum (val))
#define LGLYPH_SET_DESCENT(g, val) ASET ((g), LGLYPH_IX_DESCENT, make_fixnum (val))
#define LGLYPH_SET_ADJUSTMENT(g, val) ASET ((g), LGLYPH_IX_ADJUSTMENT, (val))

#define LGLYPH_XOFF(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			? XFIXNUM (AREF (LGLYPH_ADJUSTMENT (g), 0)) : 0)
#define LGLYPH_YOFF(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			? XFIXNUM (AREF (LGLYPH_ADJUSTMENT (g), 1)) : 0)
#define LGLYPH_WADJUST(g) (VECTORP (LGLYPH_ADJUSTMENT (g)) \
			   ? XFIXNUM (AREF (LGLYPH_ADJUSTMENT (g), 2)) : 0)

extern Lisp_Object composition_gstring_put_cache (Lisp_Object, ptrdiff_t);
extern Lisp_Object composition_gstring_from_id (ptrdiff_t);
extern bool composition_gstring_p (Lisp_Object);
extern int composition_gstring_width (Lisp_Object, ptrdiff_t, ptrdiff_t,
                                      struct font_metrics *);

extern void composition_compute_stop_pos (struct composition_it *,
                                          ptrdiff_t, ptrdiff_t, ptrdiff_t,
                                          Lisp_Object);
extern bool composition_reseat_it (struct composition_it *, ptrdiff_t,
				   ptrdiff_t, ptrdiff_t, struct window *,
				   signed char, struct face *, Lisp_Object);
extern int composition_update_it (struct composition_it *,
                                  ptrdiff_t, ptrdiff_t, Lisp_Object);

extern ptrdiff_t composition_adjust_point (ptrdiff_t, ptrdiff_t);

INLINE_HEADER_END

#endif /* not EMACS_COMPOSITE_H */
