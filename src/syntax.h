/* Declarations having to do with GNU Emacs syntax tables.
   Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.

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
    Smath,	 /* for delimiters like $ in Tex. */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Sinherit,    /* use the standard syntax table for this character */
    Smax	 /* Upper bound on codes that are meaningful */
  };

/* Fetch the syntax entry for char C from table TABLE.
   This returns the whole entry (normally a cons cell)
   and does not do any kind of inheritance.  */

#if 1
#define RAW_SYNTAX_ENTRY(table, c)				\
  (XCHAR_TABLE (table)->contents[(unsigned char) (c)])

#define SET_RAW_SYNTAX_ENTRY(table, c, val)			\
  (XCHAR_TABLE (table)->contents[(unsigned char) (c)] = (val))
#else
#define RAW_SYNTAX_ENTRY(table, c)				\
  ((c) >= 128							\
   ? raw_syntax_table_lookup (table, c)				\
   : XCHAR_TABLE (table)->contents[(unsigned char) (c)])

#define SET_RAW_SYNTAX_ENTRY(table, c, val)			\
  ((c) >= 128							\
   ? set_raw_syntax_table_lookup (table, c, (val))		\
   : XCHAR_TABLE (table)->contents[(unsigned char) (c)] = (val))
#endif

/* Extract the information from the entry for character C
   in syntax table TABLE.  Do inheritance.  */

#ifdef __GNUC__
#define SYNTAX_ENTRY(c)							\
  ({ Lisp_Object temp, table;						\
     unsigned char cc = (c);						\
     table = current_buffer->syntax_table;				\
     while (!NILP (table))						\
       {								\
	 temp = RAW_SYNTAX_ENTRY (table, cc);				\
	 if (!NILP (temp))						\
	   break;							\
	 table = XCHAR_TABLE (table)->parent;				\
       }								\
     temp; })

#define SYNTAX(c)							\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? (enum syntaxcode) (XINT (XCONS (temp)->car) & 0xff)		\
      : wrong_type_argument (Qconsp, temp)); })

#define SYNTAX_WITH_FLAGS(c)						\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? XINT (XCONS (temp)->car)					\
      : wrong_type_argument (Qconsp, temp)); })

#define SYNTAX_MATCH(c)							\
  ({ Lisp_Object temp;							\
     temp = SYNTAX_ENTRY (c);						\
     (CONSP (temp)							\
      ? XINT (XCONS (temp)->cdr)					\
      : wrong_type_argument (Qconsp, temp)); })
#else
extern Lisp_Object syntax_temp;
extern Lisp_Object syntax_parent_lookup ();

#define SYNTAX_ENTRY(c)							\
  (syntax_temp								\
     = RAW_SYNTAX_ENTRY (current_buffer->syntax_table, (c)),		\
   (NILP (syntax_temp)							\
    ? (syntax_temp							\
       = syntax_parent_lookup (current_buffer->syntax_table,		\
			       (unsigned char) (c)))			\
    : syntax_temp))

#define SYNTAX(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? (enum syntaxcode) (XINT (XCONS (syntax_temp)->car) & 0xff)	\
    : wrong_type_argument (Qconsp, syntax_temp)))

#define SYNTAX_WITH_FLAGS(c)						\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XINT (XCONS (syntax_temp)->car)					\
    : wrong_type_argument (Qconsp, syntax_temp)))

#define SYNTAX_MATCH(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XINT (XCONS (syntax_temp)->cdr)					\
    : wrong_type_argument (Qconsp, syntax_temp)))
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
  ends style a or b. Comment start sequences can start style a or b.
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
 (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern char syntax_code_spec[14];
