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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


extern Lisp_Object Qsyntax_table_p;
extern Lisp_Object Fsyntax_table_p (), Fsyntax_table (), Fset_syntax_table ();

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
#define Vstandard_syntax_table buffer_defaults.syntax_table

/* A syntax table is a Lisp vector of length 0400, whose elements are integers.

The low 8 bits of the integer is a code, as follows:
*/

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

#define RAW_SYNTAX(table, c) \
  ((enum syntaxcode) (XINT (XVECTOR (table)->contents[(unsigned char) (c)]) & 0377))

#ifdef __GNUC__
#define SYNTAX(c)						\
 ({ unsigned char character = c;				\
    enum syntaxcode syntax					\
      = RAW_SYNTAX (current_buffer->syntax_table, character);	\
    if (syntax == Sinherit)					\
      syntax = RAW_SYNTAX (Vstandard_syntax_table, character);	\
    syntax; })
#else
#define SYNTAX(c)						\
 (RAW_SYNTAX (current_buffer->syntax_table, c) == Sinherit	\
  ? RAW_SYNTAX (Vstandard_syntax_table, c)			\
  : RAW_SYNTAX (current_buffer->syntax_table, c))
#endif

/* The next 8 bits of the number is a character,
 the matching delimiter in the case of Sopen or Sclose. */

#define RAW_SYNTAX_MATCH(table, c) \
  ((XINT (XVECTOR (table)->contents[(unsigned char) (c)]) >> 8) & 0377)

#ifdef __GNUC__
#define SYNTAX_MATCH(c)							    \
 ({ unsigned char character = c;					    \
    enum syntaxcode syntax						    \
      = RAW_SYNTAX (current_buffer->syntax_table, character);		    \
    int matcher;							    \
    if (syntax == Sinherit)						    \
      matcher = RAW_SYNTAX_MATCH (Vstandard_syntax_table, character);	    \
    else								    \
      matcher = RAW_SYNTAX_MATCH (current_buffer->syntax_table, character); \
    matcher; })
#else
#define SYNTAX_MATCH(c)						\
 (RAW_SYNTAX (current_buffer->syntax_table, c) == Sinherit	\
  ? RAW_SYNTAX_MATCH (Vstandard_syntax_table, c)			\
  : RAW_SYNTAX_MATCH (current_buffer->syntax_table, c))
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

#define SYNTAX_CHOOSE_TABLE(c)					\
 (RAW_SYNTAX (current_buffer->syntax_table, c) == Sinherit	\
  ? Vstandard_syntax_table : current_buffer->syntax_table)

#ifdef __GNUC__

#define SYNTAX_COMSTART_FIRST(c)			\
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 16) & 1;	\
   })

#define SYNTAX_COMSTART_SECOND(c) \
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 17) & 1;	\
   })

#define SYNTAX_COMEND_FIRST(c) \
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 18) & 1;	\
   })

#define SYNTAX_COMEND_SECOND(c) \
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 19) & 1;	\
   })

#define SYNTAX_PREFIX(c) \
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 20) & 1;	\
   })

/* extract the comment style bit from the syntax table entry */
#define SYNTAX_COMMENT_STYLE(c) \
  ({ unsigned char ch = c;				\
     Lisp_Object table = SYNTAX_CHOOSE_TABLE (ch);	\
     (XINT (XVECTOR (table)->contents[ch]) >> 21) & 1;	\
   })

#else

#define SYNTAX_COMSTART_FIRST(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 16) & 1)

#define SYNTAX_COMSTART_SECOND(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 17) & 1)

#define SYNTAX_COMEND_FIRST(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 18) & 1)

#define SYNTAX_COMEND_SECOND(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 19) & 1)

#define SYNTAX_PREFIX(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 20) & 1)

/* extract the comment style bit from the syntax table entry */
#define SYNTAX_COMMENT_STYLE(c) \
  ((XINT (XVECTOR (SYNTAX_CHOOSE_TABLE (c))->contents[(unsigned char) (c)]) >> 21) & 1)

#endif

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern char syntax_code_spec[14];
