/* Header for composite sequence handler.
   Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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

#ifndef EMACS_COMPOSITE_H
#define EMACS_COMPOSITE_H

/* Methods to display a sequence of components a composition.  */
enum composition_method {
  /* The first two are actually not methods, but used in code
     conversion to specify the current composing status.  */
  COMPOSITION_DISABLED,		/* Never handle composition data */
  COMPOSITION_NO,		/* Not processing composition data */
  /* Compose relatively without alternate characters.  */
  COMPOSITION_RELATIVE,
  /* Compose by specified composition rule.  This is not used in Emacs
     21 but we need it to decode files saved in the older versions of
     Emacs.  */
  COMPOSITION_WITH_RULE,
  /* Compose relatively with alternate characters.  */
  COMPOSITION_WITH_ALTCHARS,
  /* Compose by specified composition rule with alternate characters.  */
  COMPOSITION_WITH_RULE_ALTCHARS
};

/* Maximum number of compoments a single composition can have.  */
#define MAX_COMPOSITION_COMPONENTS 16

/* These macros access information about a composition that
   has `composition' property PROP.  PROP is:
	((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
   or
	(COMPOSITION-ID . (LENGTH COMPONENTS . MODIFICATION-FUNC))
   They don't check validity of PROP.  */

/* Temporary variable used only in the following macros.  */
extern Lisp_Object composition_temp;

/* Return 1 iff the composition is already registered.  */
#define COMPOSITION_REGISTERD_P(prop) INTEGERP (XCAR (prop))

/* Return ID number of the already registered composition.  */
#define COMPOSITION_ID(prop) XINT (XCAR (prop))

/* Return length of the composition.  */
#define COMPOSITION_LENGTH(prop)	\
  (COMPOSITION_REGISTERD_P (prop)	\
   ? XINT (XCAR (XCDR (prop)))		\
   : XINT (XCAR (XCAR (prop))))

/* Return components of the composition.  */
#define COMPOSITION_COMPONENTS(prop)	\
  (COMPOSITION_REGISTERD_P (prop)	\
   ? XCAR (XCDR (XCDR (prop)))		\
   : XCDR (XCAR (prop)))

/* Return modification function of the composition.  */
#define COMPOSITION_MODIFICATION_FUNC(prop)	\
  (COMPOSITION_REGISTERD_P (prop)		\
   ? XCDR (XCDR (XCDR (prop)))			\
   : CONSP (prop) ? XCDR (prop) : Qnil)

/* Return the method of composition.  */
#define COMPOSITION_METHOD(prop)					\
  (COMPOSITION_REGISTERD_P (prop)					\
   ? composition_table[COMPOSITION_ID (prop)]->method			\
   : (composition_temp = XCDR (XCAR (prop)),				\
      (NILP (composition_temp)						\
       ? COMPOSITION_RELATIVE						\
       : ((INTEGERP (composition_temp) || STRINGP (composition_temp))	\
	  ? COMPOSITION_WITH_ALTCHARS					\
	  : COMPOSITION_WITH_RULE_ALTCHARS))))

/* Return 1 iff the composition is valid.  It is valid if length of
   the composition equals to (END - START).  */
#define COMPOSITION_VALID_P(start, end, prop)			\
  (CONSP (prop)							\
   && (COMPOSITION_REGISTERD_P (prop)				\
       ? (COMPOSITION_ID (prop) >= 0				\
	  && COMPOSITION_ID (prop) <= n_compositions		\
	  && CONSP (XCDR (prop)))				\
       : (composition_temp = XCAR (prop),			\
	  (CONSP (composition_temp)				\
	   && (composition_temp = XCDR (composition_temp),	\
	       (NILP (composition_temp)				\
		|| STRINGP (composition_temp)			\
		|| VECTORP (composition_temp)			\
		|| INTEGERP (composition_temp)			\
		|| CONSP (composition_temp))))))		\
   && (end - start) == COMPOSITION_LENGTH (prop))

/* Return the Nth glyph of composition specified by CMP.  CMP is a
   pointer to `struct composition'. */
#define COMPOSITION_GLYPH(cmp, n)					\
  XINT (XVECTOR (XVECTOR (XHASH_TABLE (composition_hash_table)		\
			  ->key_and_value)				\
		 ->contents[cmp->hash_index * 2])			\
	->contents[cmp->method == COMPOSITION_WITH_RULE_ALTCHARS	\
		  ? (n) * 2 : (n)])

/* Return the encoded composition rule to compose the Nth glyph of
   rule-base composition specified by CMP.  CMP is a pointer to
   `struct composition'. */
#define COMPOSITION_RULE(cmp, n)				\
  XINT (XVECTOR (XVECTOR (XHASH_TABLE (composition_hash_table)	\
			  ->key_and_value)			\
		 ->contents[cmp->hash_index * 2])		\
	->contents[(n) * 2 - 1])

/* Decode encoded composition rule RULE_CODE into GREF (global
   reference point code) and NREF (new reference point code).  Don't
   check RULE_CODE, always set GREF and NREF to valid values.  */
#define COMPOSITION_DECODE_RULE(rule_code, gref, nref)	\
  do {							\
    gref = (rule_code) / 12;				\
    if (gref > 12) gref = 11;				\
    nref = (rule_code) % 12;				\
  } while (0)

/* Return encoded composition rule for the pair of global reference
   point GREF and new reference point NREF.  If arguments are invalid,
   return -1. */
#define COMPOSITION_ENCODE_RULE(gref, nref)		\
  ((unsigned) (gref) < 12 && (unsigned) (nref) < 12	\
   ? (gref) * 12 + (nref) : -1)

/* Data structure that records information about a composition
   currently used in some buffers or strings.

   When a composition is assigned an ID number (by
   get_composition_id), this structure is allocated for the
   composition and linked in composition_table[ID].

   Identical compositions appearing at different places have the same
   ID, and thus share the same instance of this structure.  */

struct composition {
  /* Number of glyphs of the composition components.  */
  unsigned glyph_len;

  /* Width, ascent, and descent pixels of the composition.  */
  short pixel_width, ascent, descent;

  /* How many columns the overall glyphs occupy on the screen.  This
     gives an approximate value for column calculation in
     Fcurrent_column, and etc.  */
  unsigned short width;

  /* Method of the composition.  */
  enum composition_method method;

  /* Index to the composition hash table.  */
  int hash_index;

  /* For which font we have calculated the remaining members.  The
     actual type is device dependent.  */
  void *font;

  /* Pointer to an array of x-offset and y-offset (by pixels) of
     glyphs.  This points to a sufficient memory space (sizeof (int) *
     glyph_len * 2) that is allocated when the composition is
     registered in composition_table.  X-offset and Y-offset of Nth
     glyph are (2N)th and (2N+1)th elements respectively.  */
  short *offsets;
};

/* Table of pointers to the structure `composition' indexed by
   COMPOSITION-ID.  */
extern struct composition **composition_table;
/* Number of the currently registered compositions.  */
extern int n_compositions;

/* Mask bits for CHECK_MASK arg to update_compositions.
   For a change in the region FROM and TO, check compositions ... */
#define CHECK_HEAD	1	/* adjacent to FROM */
#define CHECK_TAIL	2	/* adjacent to TO */
#define CHECK_INSIDE	4	/* between FROM and TO */
#define CHECK_BORDER	(CHECK_HEAD | CHECK_TAIL)
#define CHECK_ALL	(CHECK_BORDER | CHECK_INSIDE)

extern Lisp_Object Qcomposition;
extern Lisp_Object composition_hash_table;

extern int get_composition_id P_ ((int, int, int, Lisp_Object, Lisp_Object));
extern int find_composition P_ ((int, int, int *, int *, Lisp_Object *,
				 Lisp_Object));
extern void update_compositions P_ ((int, int, int));
extern void make_composition_value_copy P_ ((Lisp_Object));
extern void compose_region P_ ((int, int, Lisp_Object, Lisp_Object,
				Lisp_Object));
extern void syms_of_composite P_ ((void));
extern void compose_text P_ ((int, int, Lisp_Object, Lisp_Object,
			      Lisp_Object));
extern void compose_chars_in_text P_ ((int, int, Lisp_Object));

#endif /* not EMACS_COMPOSITE_H */

/* arch-tag: 59524d89-c645-47bd-b5e6-65e861690118
   (do not change this comment) */
