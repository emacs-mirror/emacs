/* Things for GLYPHS and glyph tables.
   Copyright (C) 1993, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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

/* Access the slots of a display-table, according to their purpose.  */

#define DISP_TABLE_P(obj)						    \
  (CHAR_TABLE_P (obj)							    \
   && EQ (XCHAR_TABLE (obj)->purpose, Qdisplay_table)			    \
   && CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (obj)) == DISP_TABLE_EXTRA_SLOTS)

#define DISP_TABLE_EXTRA_SLOTS 6
#define DISP_TRUNC_GLYPH(dp) ((dp)->extras[0])
#define DISP_CONTINUE_GLYPH(dp) ((dp)->extras[1])
#define DISP_ESCAPE_GLYPH(dp) ((dp)->extras[2])
#define DISP_CTRL_GLYPH(dp) ((dp)->extras[3])
#define DISP_INVIS_VECTOR(dp) ((dp)->extras[4])
#define DISP_BORDER_GLYPH(dp) ((dp)->extras[5])

extern Lisp_Object disp_char_vector P_ ((struct Lisp_Char_Table *, int));

#define DISP_CHAR_VECTOR(dp, c)				\
  (ASCII_CHAR_P(c)					\
   ? (NILP ((dp)->ascii)				\
      ? (dp)->defalt					\
      : (SUB_CHAR_TABLE_P ((dp)->ascii)			\
	 ? XSUB_CHAR_TABLE ((dp)->ascii)->contents[c]	\
	 : (dp)->ascii))				\
   : disp_char_vector ((dp), (c)))

/* Defined in window.c.  */
extern struct Lisp_Char_Table *window_display_table P_ ((struct window *));

/* Defined in indent.c.  */
extern struct Lisp_Char_Table *buffer_display_table P_ ((void));

/* Display table to use for vectors that don't specify their own.  */
extern Lisp_Object Vstandard_display_table;

/* This is the `purpose' slot of a display table.  */
extern Lisp_Object Qdisplay_table;

/* Vector of GLYPH definitions.  Indexed by GLYPH number,
   the contents are a string which is how to output the GLYPH.  */
extern Lisp_Object Vglyph_table;

/* Return the current length of the GLYPH table,
   or 0 if the table isn't currently valid.  */
#define GLYPH_TABLE_LENGTH  \
  ((VECTORP (Vglyph_table)) ? XVECTOR (Vglyph_table)->size : 0)

/* Return the current base (for indexing) of the GLYPH table,
   or 0 if the table isn't currently valid.  */
#define GLYPH_TABLE_BASE  \
  ((VECTORP (Vglyph_table)) ? XVECTOR (Vglyph_table)->contents : 0)

/* Given BASE and LEN returned by the two previous macros,
   return nonzero if the GLYPH code G should be output as a single
   character with code G.  Return zero if G has a string in the table.  */
#define GLYPH_SIMPLE_P(base,len,g) ((g) >= (len) || !STRINGP (base[g]))

/* Given BASE and LEN returned by the two previous macros,
   return nonzero if GLYPH code G is aliased to a different code.  */
#define GLYPH_ALIAS_P(base,len,g) ((g) < (len) && INTEGERP (base[g]))

/* Assuming that GLYPH_SIMPLE_P (BASE, LEN, G) is 1,
   return the alias for G.  */
#define GLYPH_ALIAS(base, g) XINT (base[g])

/* Follow all aliases for G in the glyph table given by (BASE,
   LENGTH), and set G to the final glyph.  */
#define GLYPH_FOLLOW_ALIASES(base, length, g)		\
  do {							\
    while (GLYPH_ALIAS_P ((base), (length), (g)))	\
      (g) = GLYPH_ALIAS ((base), (g));			\
    if (!GLYPH_CHAR_VALID_P (FAST_GLYPH_CHAR (g)))	\
      g = FAST_MAKE_GLYPH (' ', FAST_GLYPH_FACE (g));	\
  } while (0)

/* Assuming that GLYPH_SIMPLE_P (BASE, LEN, G) is 0,
   return the length and the address of the character-sequence
   used for outputting GLYPH G.  */
#define GLYPH_LENGTH(base,g)   SCHARS (base[g])
#define GLYPH_STRING(base,g)   SDATA (base[g])

/* GLYPH for a space character.  */

#define SPACEGLYPH 040
#define NULL_GLYPH 00

#define GLYPH_FROM_CHAR(c) (c)

/* arch-tag: d7f792d2-f59c-4904-a91e-91522e3ab349
   (do not change this comment) */
