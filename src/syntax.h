/* Declarations having to do with GNU Emacs syntax tables.
   Copyright (C) 1985, 1993, 1994, 1997 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


extern Lisp_Object Qsyntax_table_p;
extern Lisp_Object Fsyntax_table_p (), Fsyntax_table (), Fset_syntax_table ();
extern void update_syntax_table ();

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
#define Vstandard_syntax_table buffer_defaults.syntax_table

/* A syntax table is a chartable whose elements are cons cells
   (CODE+FLAGS . MATCHING-CHAR).  MATCHING-CHAR can be nil if the char
   is not a kind of parenthesis.

   The low 8 bits of CODE+FLAGS is a code, as follows:  */

enum syntaxcode
  {
    Swhitespace, /* for a whitespace character */
    Spunct,	 /* for random punctuation characters */
    Sword,	 /* for a word constituent */
    Ssymbol,	 /* symbol constituent but not word constituent */
    Sopen,	 /* for a beginning delimiter */
    Sclose,      /* for an ending delimiter */
    Squote,	 /* for a prefix character like Lisp ' */
    Sstring,	 /* for a string-grouping character like Lisp " */
    Smath,	 /* for delimiters like $ in Tex.  */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Sinherit,    /* use the standard syntax table for this character */
    Scomment_fence, /* Starts/ends comment which is delimited on the
		       other side by a char with the same syntaxcode.  */
    Sstring_fence,  /* Starts/ends string which is delimited on the
		       other side by a char with the same syntaxcode.  */
    Smax	 /* Upper bound on codes that are meaningful */
  };

/* Set the syntax entry VAL for char C in table TABLE.  */

#define SET_RAW_SYNTAX_ENTRY(table, c, val)				\
  ((c) < CHAR_TABLE_SINGLE_BYTE_SLOTS					\
   ? (XCHAR_TABLE (table)->contents[(unsigned char) (c)] = (val))	\
   : Faset ((table), make_number (c), (val)))

/* Fetch the syntax entry for char C in syntax table TABLE.
   This macro is called only when C is less than CHAR_TABLE_ORDINARY_SLOTS.
   Do inheritance.  */

#ifdef __GNUC__
#define SYNTAX_ENTRY_FOLLOW_PARENT(table, c)			\
  ({ Lisp_Object tbl = table;					\
     Lisp_Object temp = XCHAR_TABLE (tbl)->contents[(c)];	\
     while (NILP (temp))					\
       {							\
	 tbl = XCHAR_TABLE (tbl)->parent;			\
	 if (NILP (tbl))					\
	   break;						\
	 temp = XCHAR_TABLE (tbl)->contents[(c)];		\
       }							\
     temp; })
#else
extern Lisp_Object syntax_temp;
extern Lisp_Object syntax_parent_lookup ();

#define SYNTAX_ENTRY_FOLLOW_PARENT(table, c)	    	\
  (syntax_temp = XCHAR_TABLE (table)->contents[(c)],	\
   (NILP (syntax_temp)				    	\
    ? syntax_parent_lookup (table, (c))		    	\
    : syntax_temp))
#endif

/* SYNTAX_ENTRY fetches the information from the entry for character C
   in syntax table TABLE, or from globally kept data (gl_state).  
   Does inheritance.  */
/* CURRENT_SYNTAX_TABLE gives the syntax table valid for current
   position, it is either the buffer's syntax table, or syntax table
   found in text properties.  */

#ifdef SYNTAX_ENTRY_VIA_PROPERTY
#  define SYNTAX_ENTRY(c)                                             \
    (gl_state.use_global ? gl_state.global_code : SYNTAX_ENTRY_INT (c))
#  define CURRENT_SYNTAX_TABLE gl_state.current_syntax_table
#else
#  define SYNTAX_ENTRY SYNTAX_ENTRY_INT
#  define CURRENT_SYNTAX_TABLE current_buffer->syntax_table
#endif

#define SYNTAX_ENTRY_INT(c)						\
  ((c) < CHAR_TABLE_SINGLE_BYTE_SLOTS				\
   ? SYNTAX_ENTRY_FOLLOW_PARENT (CURRENT_SYNTAX_TABLE,	\
				 (unsigned char) (c))		\
   : Faref (CURRENT_SYNTAX_TABLE, make_number ((c))))

/* Extract the information from the entry for character C
   in the current syntax table.  */

#ifdef __GNUC__
#define SYNTAX(c)							\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? (enum syntaxcode) (XINT (XCONS (temp)->car) & 0xff)		\
      : Swhitespace); })

#define SYNTAX_WITH_FLAGS(c)						\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? XINT (XCONS (temp)->car)					\
      : (int) Swhitespace); })

#define SYNTAX_MATCH(c)							\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? XCONS (temp)->cdr						\
      : Qnil); })
#else
#define SYNTAX(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? (enum syntaxcode) (XINT (XCONS (syntax_temp)->car) & 0xff)	\
    : Swhitespace))

#define SYNTAX_WITH_FLAGS(c)						\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XINT (XCONS (syntax_temp)->car)					\
    : (int) Swhitespace))

#define SYNTAX_MATCH(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XCONS (syntax_temp)->cdr						\
    : Qnil))
#endif

/* Then there are six single-bit flags that have the following meanings:
  1. This character is the first of a two-character comment-start sequence.
  2. This character is the second of a two-character comment-start sequence.
  3. This character is the first of a two-character comment-end sequence.
  4. This character is the second of a two-character comment-end sequence.
  5. This character is a prefix, for backward-prefix-chars.
  Note that any two-character sequence whose first character has flag 1
  and whose second character has flag 2 will be interpreted as a comment start.

  bit 6 is used to discriminate between two different comment styles.
  Languages such as C++ allow two orthogonal syntax start/end pairs
  and bit 6 is used to determine whether a comment-end or Scommentend
  ends style a or b.  Comment start sequences can start style a or b.
  Style a is always the default.
  */

#define SYNTAX_COMSTART_FIRST(c) ((SYNTAX_WITH_FLAGS (c) >> 16) & 1)

#define SYNTAX_COMSTART_SECOND(c) ((SYNTAX_WITH_FLAGS (c) >> 17) & 1)

#define SYNTAX_COMEND_FIRST(c) ((SYNTAX_WITH_FLAGS (c) >> 18) & 1)

#define SYNTAX_COMEND_SECOND(c) ((SYNTAX_WITH_FLAGS (c) >> 19) & 1)

#define SYNTAX_PREFIX(c) ((SYNTAX_WITH_FLAGS (c) >> 20) & 1)

/* extract the comment style bit from the syntax table entry */
#define SYNTAX_COMMENT_STYLE(c) ((SYNTAX_WITH_FLAGS (c) >> 21) & 1)

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword.  */

extern unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it.  */

extern char syntax_code_spec[16];

/* Make syntax table state (gl_state) good for POS, assuming it is
   currently good for a position before POS.  */

#define UPDATE_SYNTAX_TABLE_FORWARD(pos)		\
  ((pos) >= gl_state.e_property - gl_state.offset	\
   ? (update_syntax_table ((pos) + gl_state.offset, 1, 0, Qnil), 1) : 0)

/* Make syntax table state (gl_state) good for POS, assuming it is
   currently good for a position after POS.  */

#define UPDATE_SYNTAX_TABLE_BACKWARD(pos)		\
  ((pos) <= gl_state.b_property - gl_state.offset	\
   ? (update_syntax_table ((pos) + gl_state.offset, -1, 0, Qnil), 1) : 0)

/* Make syntax table good for POS.  */

#define UPDATE_SYNTAX_TABLE(pos)					\
  ((pos) <= gl_state.b_property - gl_state.offset			\
   ? (update_syntax_table ((pos) + gl_state.offset, -1, 0, Qnil), 1)	\
   : ((pos) >= gl_state.e_property - gl_state.offset			\
      ? (update_syntax_table ((pos) + gl_state.offset, 1, 0, Qnil), 1) : 0))

/* This macro should be called with FROM at the start of forward
   search, or after the last position of the backward search.  It
   makes sure that the first char is picked up with correct table, so
   one does not need to call UPDATE_SYNTAX_TABLE immediately after the
   call. 
   Sign of COUNT gives the direction of the search.
 */

#define SETUP_SYNTAX_TABLE(from,count)					\
  gl_state.b_property = BEGV - 1;					\
  gl_state.e_property = ZV + 1;						\
  gl_state.use_global = 0;						\
  gl_state.offset = 0;							\
  gl_state.current_syntax_table = current_buffer->syntax_table;		\
  if (parse_sexp_lookup_properties) 					\
    update_syntax_table ((count) > 0 ? (from) : (from) - 1, (count),	\
			 1, Qnil);

/* Same as above, but in OBJECT.  If OBJECT is nil, use current buffer.
   If it is t, ignore properties altogether.

   This is meant for regex.c to use.  For buffers, regex.c passes arguments
   to the UPDATE_SYNTAX_TABLE macros which are relative to BEGV.
   So if it is a buffer,a we set the offset field to BEGV.  */

#define SETUP_SYNTAX_TABLE_FOR_OBJECT(object, from, count)		\
  if (BUFFERP (object) || NILP (object))				\
    {									\
      gl_state.b_property = BEGV - 1;					\
      gl_state.e_property = ZV;						\
      gl_state.offset = BEGV - 1;					\
    }									\
  else if (EQ (object, Qt))						\
    {									\
      gl_state.b_property = - 1;					\
      gl_state.e_property = 1500000000;					\
      gl_state.offset = 0;						\
    }									\
  else									\
    {									\
      gl_state.b_property = -1;						\
      gl_state.e_property = 1 + XSTRING (object)->size;			\
      gl_state.offset = 0;						\
    }									\
  gl_state.use_global = 0;						\
  gl_state.current_syntax_table = current_buffer->syntax_table;		\
  if (parse_sexp_lookup_properties) 					\
      update_syntax_table (count > 0 ? (from) : (from) - 1, count, 1, object);

struct gl_state_s
{
  int start;				/* Where to stop. */
  int stop;				/* Where to stop. */
  int use_global;			/* Whether to use global_code
					   or c_s_t. */
  Lisp_Object global_code;		/* Syntax code of current char. */
  Lisp_Object current_syntax_table;	/* Syntax table for current pos. */
  Lisp_Object old_prop;			/* Syntax-table prop at prev pos. */
  int b_property;			/* Last index where c_s_t is 
					   not valid. */
  int e_property;			/* First index where c_s_t is
					   not valid. */
  INTERVAL forward_i;			/* Where to start lookup on forward */
  INTERVAL backward_i;			/* or backward movement.  The
					   data in c_s_t is valid
					   between these intervals,
					   and possibly at the
					   intervals too, depending
					   on: */
  /* Offset for positions specified to UPDATE_SYNTAX_TABLE.  */
  int offset;
  char left_ok;
  char right_ok;
};

extern struct gl_state_s gl_state;
extern int parse_sexp_lookup_properties;
extern INTERVAL interval_of ();
