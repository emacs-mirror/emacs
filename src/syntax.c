/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985, 1987, 1993, 1994, 1995 Free Software Foundation, Inc.

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


#include <config.h>
#include <ctype.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"

Lisp_Object Qsyntax_table_p;

static void scan_sexps_forward ();
static int char_quoted ();

int words_include_escapes;

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
  {
    int depth;		/* Depth at end of parsing */
    int instring;	/* -1 if not within string, else desired terminator. */
    int incomment;	/* Nonzero if within a comment at end of parsing */
    int comstyle;	/* comment style a=0, or b=1 */
    int quoted;		/* Nonzero if just after an escape char at end of parsing */
    int thislevelstart;	/* Char number of most recent start-of-expression at current level */
    int prevlevelstart; /* Char number of start of containing expression */
    int location;	/* Char number at which parsing stopped. */
    int mindepth;	/* Minimum depth seen while scanning.  */
    int comstart;	/* Position just after last comment starter.  */
  };

/* These variables are a cache for finding the start of a defun.
   find_start_pos is the place for which the defun start was found.
   find_start_value is the defun start position found for it.
   find_start_buffer is the buffer it was found in.
   find_start_begv is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static int find_start_pos;
static int find_start_value;
static struct buffer *find_start_buffer;
static int find_start_begv;
static int find_start_modiff;

/* Find a defun-start that is the last one before POS (or nearly the last).
   We record what we find, so that another call in the same area
   can return the same value right away.  */

static int
find_defun_start (pos)
     int pos;
{
  int tem;
  int shortage;

  /* Use previous finding, if it's valid and applies to this inquiry.  */
  if (current_buffer == find_start_buffer
      /* Reuse the defun-start even if POS is a little farther on.
	 POS might be in the next defun, but that's ok.
	 Our value may not be the best possible, but will still be usable.  */
      && pos <= find_start_pos + 1000
      && pos >= find_start_value
      && BEGV == find_start_begv
      && MODIFF == find_start_modiff)
    return find_start_value;

  /* Back up to start of line.  */
  tem = scan_buffer ('\n', pos, BEGV, -1, &shortage, 1);

  while (tem > BEGV)
    {
      /* Open-paren at start of line means we found our defun-start.  */
      if (SYNTAX (FETCH_CHAR (tem)) == Sopen)
	break;
      /* Move to beg of previous line.  */
      tem = scan_buffer ('\n', tem, BEGV, -2, &shortage, 1);
    }

  /* Record what we found, for the next try.  */
  find_start_value = tem;
  find_start_buffer = current_buffer;
  find_start_modiff = MODIFF;
  find_start_begv = BEGV;
  find_start_pos = pos;

  return find_start_value;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
  "Return t if ARG is a syntax table.\n\
Any vector of 256 elements will do.")
  (obj)
     Lisp_Object obj;
{
  if (VECTORP (obj) && XVECTOR (obj)->size == 0400)
    return Qt;
  return Qnil;
}

Lisp_Object
check_syntax_table (obj)
     Lisp_Object obj;
{
  register Lisp_Object tem;
  while (tem = Fsyntax_table_p (obj),
	 NILP (tem))
    obj = wrong_type_argument (Qsyntax_table_p, obj);
  return obj;
}   


DEFUN ("syntax-table", Fsyntax_table, Ssyntax_table, 0, 0, 0,
  "Return the current syntax table.\n\
This is the one specified by the current buffer.")
  ()
{
  return current_buffer->syntax_table;
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table,
   Sstandard_syntax_table, 0, 0, 0,
  "Return the standard syntax table.\n\
This is the one used for new buffers.")
  ()
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, Scopy_syntax_table, 0, 1, 0,
  "Construct a new syntax table and return it.\n\
It is a copy of the TABLE, which defaults to the standard syntax table.")
  (table)
     Lisp_Object table;
{
  Lisp_Object size, val;
  XSETFASTINT (size, 0400);
  XSETFASTINT (val, 0);
  val = Fmake_vector (size, val);
  if (!NILP (table))
    table = check_syntax_table (table);
  else if (NILP (Vstandard_syntax_table))
    /* Can only be null during initialization */
    return val;
  else table = Vstandard_syntax_table;

  bcopy (XVECTOR (table)->contents,
	 XVECTOR (val)->contents, 0400 * sizeof (Lisp_Object));
  return val;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
  "Select a new syntax table for the current buffer.\n\
One argument, a syntax table.")
  (table)
     Lisp_Object table;
{
  table = check_syntax_table (table);
  current_buffer->syntax_table = table;
  /* Indicate that this buffer now has a specified syntax table.  */
  current_buffer->local_var_flags
    |= XFASTINT (buffer_local_flags.syntax_table);
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things. */

unsigned char syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, 0377, (char) Sstring, 0377,
        (char) Smath, 0377, 0377, (char) Squote,
    (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
    (char) Sinherit, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it. */

char syntax_code_spec[14] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>', '@'
  };

DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
  "Return the syntax code of CHAR, described by a character.\n\
For example, if CHAR is a word constituent, the character `?w' is returned.\n\
The characters that correspond to various syntax codes\n\
are listed in the documentation of `modify-syntax-entry'.")
  (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch, 0);
  return make_number (syntax_code_spec[(int) SYNTAX (XINT (ch))]);
}

DEFUN ("matching-paren", Fmatching_paren, Smatching_paren, 1, 1, 0,
  "Return the matching parenthesis of CHAR, or nil if none.")
  (ch)
     Lisp_Object ch;
{
  int code;
  CHECK_NUMBER (ch, 0);
  code = SYNTAX (XINT (ch));
  if (code == Sopen || code == Sclose)
    return make_number (SYNTAX_MATCH (XINT (ch)));
  return Qnil;
}

/* This comment supplies the doc string for modify-syntax-entry,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("modify-syntax-entry", foo, bar, 2, 3, 0,
  "Set syntax for character CHAR according to string S.\n\
The syntax is changed only for table TABLE, which defaults to\n\
 the current buffer's syntax table.\n\
The first character of S should be one of the following:\n\
  Space or -  whitespace syntax.    w   word constituent.\n\
  _           symbol constituent.   .   punctuation.\n\
  (           open-parenthesis.     )   close-parenthesis.\n\
  \"           string quote.         \\   escape.\n\
  $           paired delimiter.     '   expression quote or prefix operator.\n\
  <           comment starter.      >   comment ender.\n\
  /           character-quote.      @   inherit from `standard-syntax-table'.\n\
\n\
Only single-character comment start and end sequences are represented thus.\n\
Two-character sequences are represented as described below.\n\
The second character of S is the matching parenthesis,\n\
 used only if the first character is `(' or `)'.\n\
Any additional characters are flags.\n\
Defined flags are the characters 1, 2, 3, 4, b, and p.\n\
 1 means C is the start of a two-char comment start sequence.\n\
 2 means C is the second character of such a sequence.\n\
 3 means C is the start of a two-char comment end sequence.\n\
 4 means C is the second character of such a sequence.\n\
\n\
There can be up to two orthogonal comment sequences. This is to support\n\
language modes such as C++.  By default, all comment sequences are of style\n\
a, but you can set the comment sequence style to b (on the second character\n\
of a comment-start, or the first character of a comment-end sequence) using\n\
this flag:\n\
 b means C is part of comment sequence b.\n\
\n\
 p means C is a prefix character for `backward-prefix-chars';\n\
   such characters are treated as whitespace when they occur\n\
   between expressions.")
  (char, s, table)
*/

DEFUN ("modify-syntax-entry", Fmodify_syntax_entry, Smodify_syntax_entry, 2, 3, 
  /* I really don't know why this is interactive
     help-form should at least be made useful whilst reading the second arg
   */
  "cSet syntax for character: \nsSet syntax for %s to: ",
  0 /* See immediately above */)
  (c, newentry, syntax_table)
     Lisp_Object c, newentry, syntax_table;
{
  register unsigned char *p, match;
  register enum syntaxcode code;
  int val;

  CHECK_NUMBER (c, 0);
  CHECK_STRING (newentry, 1);
  if (NILP (syntax_table))
    syntax_table = current_buffer->syntax_table;
  else
    syntax_table = check_syntax_table (syntax_table);

  p = XSTRING (newentry)->data;
  code = (enum syntaxcode) syntax_spec_code[*p++];
  if (((int) code & 0377) == 0377)
    error ("invalid syntax description letter: %c", c);

  match = *p;
  if (match) p++;
  if (match == ' ') match = 0;

  val = (match << 8) + (int) code;
  while (*p)
    switch (*p++)
      {
      case '1':
	val |= 1 << 16;
	break;

      case '2':
	val |= 1 << 17;
	break;

      case '3':
	val |= 1 << 18;
	break;

      case '4':
	val |= 1 << 19;
	break;

      case 'p':
	val |= 1 << 20;
	break;

      case 'b':
	val |= 1 << 21;
	break;
      }
	
  XSETFASTINT (XVECTOR (syntax_table)->contents[0xFF & XINT (c)], val);

  return Qnil;
}

/* Dump syntax table to buffer in human-readable format */

static void
describe_syntax (value)
    Lisp_Object value;
{
  register enum syntaxcode code;
  char desc, match, start1, start2, end1, end2, prefix, comstyle;
  char str[2];

  Findent_to (make_number (16), make_number (1));

  if (!INTEGERP (value))
    {
      insert_string ("invalid");
      return;
    }

  code = (enum syntaxcode) (XINT (value) & 0377);
  match = (XINT (value) >> 8) & 0377;
  start1 = (XINT (value) >> 16) & 1;
  start2 = (XINT (value) >> 17) & 1;
  end1 = (XINT (value) >> 18) & 1;
  end2 = (XINT (value) >> 19) & 1;
  prefix = (XINT (value) >> 20) & 1;
  comstyle = (XINT (value) >> 21) & 1;

  if ((int) code < 0 || (int) code >= (int) Smax)
    {
      insert_string ("invalid");
      return;
    }
  desc = syntax_code_spec[(int) code];

  str[0] = desc, str[1] = 0;
  insert (str, 1);

  str[0] = match ? match : ' ';
  insert (str, 1);


  if (start1)
    insert ("1", 1);
  if (start2)
    insert ("2", 1);

  if (end1)
    insert ("3", 1);
  if (end2)
    insert ("4", 1);

  if (prefix)
    insert ("p", 1);
  if (comstyle)
    insert ("b", 1);

  insert_string ("\twhich means: ");

  switch (SWITCH_ENUM_CAST (code))
    {
    case Swhitespace:
      insert_string ("whitespace"); break;
    case Spunct:
      insert_string ("punctuation"); break;
    case Sword:
      insert_string ("word"); break;
    case Ssymbol:
      insert_string ("symbol"); break;
    case Sopen:
      insert_string ("open"); break;
    case Sclose:
      insert_string ("close"); break;
    case Squote:
      insert_string ("quote"); break;
    case Sstring:
      insert_string ("string"); break;
    case Smath:
      insert_string ("math"); break;
    case Sescape:
      insert_string ("escape"); break;
    case Scharquote:
      insert_string ("charquote"); break;
    case Scomment:
      insert_string ("comment"); break;
    case Sendcomment:
      insert_string ("endcomment"); break;
    case Sinherit:
      insert_string ("inherit"); break;
    default:
      insert_string ("invalid");
      return;
    }

  if (match)
    {
      insert_string (", matches ");
      insert_char (match);
    }

  if (start1)
    insert_string (",\n\t  is the first character of a comment-start sequence");
  if (start2)
    insert_string (",\n\t  is the second character of a comment-start sequence");

  if (end1)
    insert_string (",\n\t  is the first character of a comment-end sequence");
  if (end2)
    insert_string (",\n\t  is the second character of a comment-end sequence");
  if (comstyle)
    insert_string (" (comment style b)");

  if (prefix)
    insert_string (",\n\t  is a prefix character for `backward-prefix-chars'");

  insert_string ("\n");
}

static Lisp_Object
describe_syntax_1 (vector)
     Lisp_Object vector;
{
  struct buffer *old = current_buffer;
  set_buffer_internal (XBUFFER (Vstandard_output));
  describe_vector (vector, Qnil, describe_syntax, 0, Qnil);
  call0 (intern ("help-mode"));
  set_buffer_internal (old);
  return Qnil;
}

DEFUN ("describe-syntax", Fdescribe_syntax, Sdescribe_syntax, 0, 0, "",
  "Describe the syntax specifications in the syntax table.\n\
The descriptions are inserted in a buffer, which is then displayed.")
  ()
{
  internal_with_output_to_temp_buffer
     ("*Help*", describe_syntax_1, current_buffer->syntax_table);

  return Qnil;
}

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

scan_words (from, count)
     register int from, count;
{
  register int beg = BEGV;
  register int end = ZV;
  register int code;

  immediate_quit = 1;
  QUIT;

  while (count > 0)
    {
      while (1)
	{
	  if (from == end)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  code = SYNTAX (FETCH_CHAR (from));
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  from++;
	}
      while (1)
	{
	  if (from == end) break;
	  code = SYNTAX (FETCH_CHAR (from));
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword)
	      break;
	  from++;
	}
      count--;
    }
  while (count < 0)
    {
      while (1)
	{
	  if (from == beg)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  code = SYNTAX (FETCH_CHAR (from - 1));
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  from--;
	}
      while (1)
	{
	  if (from == beg) break;
	  code = SYNTAX (FETCH_CHAR (from - 1));
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword)
	      break;
	  from--;
	}
      count++;
    }

  immediate_quit = 0;

  return from;
}

DEFUN ("forward-word", Fforward_word, Sforward_word, 1, 1, "p",
  "Move point forward ARG words (backward if ARG is negative).\n\
Normally returns t.\n\
If an edge of the buffer is reached, point is left there\n\
and nil is returned.")
  (count)
     Lisp_Object count;
{
  int val;
  CHECK_NUMBER (count, 0);

  if (!(val = scan_words (point, XINT (count))))
    {
      SET_PT (XINT (count) > 0 ? ZV : BEGV);
      return Qnil;
    }
  SET_PT (val);
  return Qt;
}

DEFUN ("forward-comment", Fforward_comment, Sforward_comment, 1, 1, 0,
  "Move forward across up to N comments.  If N is negative, move backward.\n\
Stop scanning if we find something other than a comment or whitespace.\n\
Set point to where scanning stops.\n\
If N comments are found as expected, with nothing except whitespace\n\
between them, return t; otherwise return nil.")
  (count)
     Lisp_Object count;
{
  register int from;
  register int stop;
  register int c;
  register enum syntaxcode code;
  int comstyle = 0;	    /* style of comment encountered */
  int found;
  int count1;

  CHECK_NUMBER (count, 0);
  count1 = XINT (count);

  immediate_quit = 1;
  QUIT;

  from = PT;

  while (count1 > 0)
    {
      stop = ZV;
      do
	{
	  if (from == stop)
	    {
	      SET_PT (from);
	      immediate_quit = 0;
	      return Qnil;
	    }
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  from++;
	  comstyle = 0;
	  if (from < stop && SYNTAX_COMSTART_FIRST (c)
	      && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from)))
	    {
	      /* We have encountered a comment start sequence and we 
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section.  */
	      code = Scomment;
	      comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from));
	      from++;
	    }
	}
      while (code == Swhitespace || code == Sendcomment);
      if (code != Scomment)
	{
	  immediate_quit = 0;
	  SET_PT (from - 1);
	  return Qnil;
	}
      /* We're at the start of a comment.  */
      while (1)
	{
	  if (from == stop)
	    {
	      immediate_quit = 0;
	      SET_PT (from);
	      return Qnil;
	    }
	  c = FETCH_CHAR (from);
	  from++;
	  if (SYNTAX (c) == Sendcomment
	      && SYNTAX_COMMENT_STYLE (c) == comstyle)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section */
	    break;
	  if (from < stop && SYNTAX_COMEND_FIRST (c)
	      && SYNTAX_COMEND_SECOND (FETCH_CHAR (from))
	      && SYNTAX_COMMENT_STYLE (c) == comstyle)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section */
	    { from++; break; }
	}
      /* We have skipped one comment.  */
      count1--;
    }

  while (count1 < 0)
    {
      stop = BEGV;
      while (from > stop)
	{
	  int quoted;

	  from--;
	  quoted = char_quoted (from);
	  if (quoted)
	    from--;
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  comstyle = 0;
	  if (code == Sendcomment)
	    comstyle = SYNTAX_COMMENT_STYLE (c);
	  if (from > stop && SYNTAX_COMEND_SECOND (c)
	      && SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1))
	      && !char_quoted (from - 1))
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      code = Sendcomment;
	      comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from - 1));
	      from--;
	    }

	  if (code == Sendcomment && !quoted)
	    {
#if 0
	      if (code != SYNTAX (c))
		/* For a two-char comment ender, we can assume
		   it does end a comment.  So scan back in a simple way.  */
		{
		  if (from != stop) from--;
		  while (1)
		    {
		      if (SYNTAX (c = FETCH_CHAR (from)) == Scomment
			  && SYNTAX_COMMENT_STYLE (c) == comstyle)
			break;
		      if (from == stop)
			{
			  immediate_quit = 0;
			  SET_PT (from);
			  return Qnil;
			}
		      from--;
		      if (SYNTAX_COMSTART_SECOND (c)
			  && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from))
			  && SYNTAX_COMMENT_STYLE (c) == comstyle
			  && !char_quoted (from))
			break;
		    }
		  break;
		}
#endif /* 0 */

	      /* Look back, counting the parity of string-quotes,
		 and recording the comment-starters seen.
		 When we reach a safe place, assume that's not in a string;
		 then step the main scan to the earliest comment-starter seen
		 an even number of string quotes away from the safe place.

		 OFROM[I] is position of the earliest comment-starter seen
		 which is I+2X quotes from the comment-end.
		 PARITY is current parity of quotes from the comment end.  */
	      {
		int parity = 0;
		char my_stringend = 0;
		int string_lossage = 0;
		int comment_end = from;
		int comstart_pos = 0;
		int comstart_parity = 0;
		int scanstart = from - 1;

		/* At beginning of range to scan, we're outside of strings;
		   that determines quote parity to the comment-end.  */
		while (from != stop)
		  {
		    /* Move back and examine a character.  */
		    from--;

		    c = FETCH_CHAR (from);
		    code = SYNTAX (c);

		    /* If this char is the second of a 2-char comment sequence,
		       back up and give the pair the appropriate syntax.  */
		    if (from > stop && SYNTAX_COMEND_SECOND (c)
			&& SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1)))
		      {
			code = Sendcomment;
			from--;
		        c = FETCH_CHAR (from);
		      }
			
		    /* If this char starts a 2-char comment start sequence,
		       treat it like a 1-char comment starter.  */
		    if (from < scanstart && SYNTAX_COMSTART_FIRST (c)
			&& SYNTAX_COMSTART_SECOND (FETCH_CHAR (from + 1))
			&& comstyle == SYNTAX_COMMENT_STYLE (FETCH_CHAR (from + 1)))
		      code = Scomment;

		    /* Ignore escaped characters.  */
		    if (char_quoted (from))
		      continue;

		    /* Track parity of quotes.  */
		    if (code == Sstring)
		      {
			parity ^= 1;
			if (my_stringend == 0)
			  my_stringend = c;
			/* If we have two kinds of string delimiters.
			   There's no way to grok this scanning backwards.  */
			else if (my_stringend != c)
			  string_lossage = 1;
		      }

		    /* Record comment-starters according to that
		       quote-parity to the comment-end.  */
		    if (code == Scomment)
		      {
			comstart_parity = parity;
			comstart_pos = from;
		      }

		    /* If we find another earlier comment-ender,
		       any comment-starts earlier than that don't count
		       (because they go with the earlier comment-ender).  */
		    if (code == Sendcomment
			&& SYNTAX_COMMENT_STYLE (FETCH_CHAR (from)) == comstyle)
		      break;

		    /* Assume a defun-start point is outside of strings.  */
		    if (code == Sopen
			&& (from == stop || FETCH_CHAR (from - 1) == '\n'))
		      break;
		  }

		if (comstart_pos == 0)
		  from = comment_end;
		/* If the earliest comment starter
		   is followed by uniform paired string quotes or none,
		   we know it can't be inside a string
		   since if it were then the comment ender would be inside one.
		   So it does start a comment.  Skip back to it.  */
		else if (comstart_parity == 0 && !string_lossage)
		  from = comstart_pos;
		else
		  {
		    /* We had two kinds of string delimiters mixed up
		       together.  Decode this going forwards.
		       Scan fwd from the previous comment ender
		       to the one in question; this records where we
		       last passed a comment starter.  */
		    struct lisp_parse_state state;
		    scan_sexps_forward (&state, find_defun_start (comment_end),
					comment_end - 1, -10000, 0, Qnil, 0);
		    if (state.incomment)
		      from = state.comstart;
		    else
		      /* We can't grok this as a comment; scan it normally.  */
		      from = comment_end;
		  }
	      }
	      /* We have skipped one comment.  */
	      break;
	    }
	  else if ((code != Swhitespace && code != Scomment) || quoted)
	    {
	      immediate_quit = 0;
	      SET_PT (from + 1);
	      return Qnil;
	    }
	}

      count1++;
    }

  SET_PT (from);
  immediate_quit = 0;
  return Qt;
}

int parse_sexp_ignore_comments;

Lisp_Object
scan_lists (from, count, depth, sexpflag)
     register int from;
     int count, depth, sexpflag;
{
  Lisp_Object val;
  register int stop;
  register int c;
  char stringterm;
  int quoted;
  int mathexit = 0;
  register enum syntaxcode code;
  int min_depth = depth;    /* Err out if depth gets less than this. */
  int comstyle = 0;	    /* style of comment encountered */

  if (depth > 0) min_depth = 0;

  immediate_quit = 1;
  QUIT;

  while (count > 0)
    {
      stop = ZV;
      while (from < stop)
	{
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  from++;
	  if (from < stop && SYNTAX_COMSTART_FIRST (c)
	      && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from))
	      && parse_sexp_ignore_comments)
	    {
	      /* we have encountered a comment start sequence and we 
		 are ignoring all text inside comments. we must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section */
	      code = Scomment;
	      comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from));
	      from++;
	    }
	  
	  if (SYNTAX_PREFIX (c))
	    continue;

	  switch (SWITCH_ENUM_CAST (code))
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      from++;
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it. */
	      while (from < stop)
		{
		  switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  from++;
		}
	      goto done;

	    case Scomment:
	      if (!parse_sexp_ignore_comments) break;
	      while (1)
		{
		  if (from == stop)
		    {
		      if (depth == 0)
			goto done;
		      goto lose;
		    }
		  c = FETCH_CHAR (from);
		  if (SYNTAX (c) == Sendcomment
		      && SYNTAX_COMMENT_STYLE (c) == comstyle)
		    /* we have encountered a comment end of the same style
		       as the comment sequence which began this comment
		       section */
		    break;
		  from++;
		  if (from < stop && SYNTAX_COMEND_FIRST (c)
		      && SYNTAX_COMEND_SECOND (FETCH_CHAR (from))
		      && SYNTAX_COMMENT_STYLE (c) == comstyle)
		    /* we have encountered a comment end of the same style
		       as the comment sequence which began this comment
		       section */
		    { from++; break; }
		}
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from))
		from++;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	      if (!--depth) goto done;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR (from - 1);
	      while (1)
		{
		  if (from >= stop) goto lose;
		  if (FETCH_CHAR (from) == stringterm) break;
		  switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		    }
		  from++;
		}
	      from++;
	      if (!depth && sexpflag) goto done;
	      break;
	    }
	}

      /* Reached end of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      stop = BEGV;
      while (from > stop)
	{
	  from--;
	  if (quoted = char_quoted (from))
	    from--;
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  comstyle = 0;
	  if (code == Sendcomment)
	    comstyle = SYNTAX_COMMENT_STYLE (c);
	  if (from > stop && SYNTAX_COMEND_SECOND (c)
	      && SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1))
	      && !char_quoted (from - 1)
	      && parse_sexp_ignore_comments)
	    {
	      /* we must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style */
	      code = Sendcomment;
	      comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from - 1));
	      from--;
	    }
	  
	  if (SYNTAX_PREFIX (c))
	    continue;

	  switch (SWITCH_ENUM_CAST (quoted ? Sword : code))
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished after passing it. */
	      while (from > stop)
		{
		  quoted = char_quoted (from - 1);
		  if (quoted)
		    from--;
		  if (! (quoted || SYNTAX (FETCH_CHAR (from - 1)) == Sword
			 || SYNTAX (FETCH_CHAR (from - 1)) == Ssymbol
			 || SYNTAX (FETCH_CHAR (from - 1)) == Squote))
            	    goto done2;
		  from--;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from - 1))
		from--;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	      if (!--depth) goto done2;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sendcomment:
	      if (!parse_sexp_ignore_comments)
		break;
#if 0
	      if (code != SYNTAX (c))
		/* For a two-char comment ender, we can assume
		   it does end a comment.  So scan back in a simple way.  */
		{
		  if (from != stop) from--;
		  while (1)
		    {
		      if (SYNTAX (c = FETCH_CHAR (from)) == Scomment
			  && SYNTAX_COMMENT_STYLE (c) == comstyle)
			break;
		      if (from == stop)
			{
			  if (depth == 0)
			    goto done2;
			  goto lose;
			}
		      from--;
		      if (SYNTAX_COMSTART_SECOND (c)
			  && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from))
			  && SYNTAX_COMMENT_STYLE (c) == comstyle
			  && !char_quoted (from))
			break;
		    }
		  break;
		}
#endif /* 0 */

	      /* Look back, counting the parity of string-quotes,
		 and recording the comment-starters seen.
		 When we reach a safe place, assume that's not in a string;
		 then step the main scan to the earliest comment-starter seen
		 an even number of string quotes away from the safe place.

		 OFROM[I] is position of the earliest comment-starter seen
		 which is I+2X quotes from the comment-end.
		 PARITY is current parity of quotes from the comment end.  */
	      {
		int parity = 0;
		char my_stringend = 0;
		int string_lossage = 0;
		int comment_end = from;
		int comstart_pos = 0;
		int comstart_parity = 0;
		int scanstart = from - 1;

		/* At beginning of range to scan, we're outside of strings;
		   that determines quote parity to the comment-end.  */
		while (from != stop)
		  {
		    /* Move back and examine a character.  */
		    from--;

		    c = FETCH_CHAR (from);
		    code = SYNTAX (c);

		    /* If this char is the second of a 2-char comment sequence,
		       back up and give the pair the appropriate syntax.  */
		    if (from > stop && SYNTAX_COMEND_SECOND (c)
			&& SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1)))
		      {
			code = Sendcomment;
			from--;
		        c = FETCH_CHAR (from);
		      }
			
		    /* If this char starts a 2-char comment start sequence,
		       treat it like a 1-char comment starter.  */
		    if (from < scanstart && SYNTAX_COMSTART_FIRST (c)
			&& SYNTAX_COMSTART_SECOND (FETCH_CHAR (from + 1))
			&& comstyle == SYNTAX_COMMENT_STYLE (FETCH_CHAR (from + 1)))
		      code = Scomment;

		    /* Ignore escaped characters.  */
		    if (char_quoted (from))
		      continue;

		    /* Track parity of quotes.  */
		    if (code == Sstring)
		      {
			parity ^= 1;
			if (my_stringend == 0)
			  my_stringend = c;
			/* If we have two kinds of string delimiters.
			   There's no way to grok this scanning backwards.  */
			else if (my_stringend != c)
			  string_lossage = 1;
		      }

		    /* Record comment-starters according to that
		       quote-parity to the comment-end.  */
		    if (code == Scomment)
		      {
			comstart_parity = parity;
			comstart_pos = from;
		      }

		    /* If we find another earlier comment-ender,
		       any comment-starts earlier than that don't count
		       (because they go with the earlier comment-ender).  */
		    if (code == Sendcomment
			&& SYNTAX_COMMENT_STYLE (FETCH_CHAR (from)) == comstyle)
		      break;

		    /* Assume a defun-start point is outside of strings.  */
		    if (code == Sopen
			&& (from == stop || FETCH_CHAR (from - 1) == '\n'))
		      break;
		  }

		if (comstart_pos == 0)
		  from = comment_end;
		/* If the earliest comment starter
		   is followed by uniform paired string quotes or none,
		   we know it can't be inside a string
		   since if it were then the comment ender would be inside one.
		   So it does start a comment.  Skip back to it.  */
		else if (comstart_parity == 0 && !string_lossage)
		  from = comstart_pos;
		else
		  {
		    /* We had two kinds of string delimiters mixed up
		       together.  Decode this going forwards.
		       Scan fwd from the previous comment ender
		       to the one in question; this records where we
		       last passed a comment starter.  */
		    struct lisp_parse_state state;
		    scan_sexps_forward (&state, find_defun_start (comment_end),
					comment_end - 1, -10000, 0, Qnil, 0);
		    if (state.incomment)
		      from = state.comstart;
		    else
		      /* We can't grok this as a comment; scan it normally.  */
		      from = comment_end;
		  }
	      }
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR (from);
	      while (1)
		{
		  if (from == stop) goto lose;
		  if (!char_quoted (from - 1)
		      && stringterm == FETCH_CHAR (from - 1))
		    break;
		  from--;
		}
	      from--;
	      if (!depth && sexpflag) goto done2;
	      break;
	    }
	}

      /* Reached start of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

    done2:
      count++;
    }


  immediate_quit = 0;
  XSETFASTINT (val, from);
  return val;

 lose:
  error ("Unbalanced parentheses");
  /* NOTREACHED */
}

static int
char_quoted (pos)
     register int pos;
{
  register enum syntaxcode code;
  register int beg = BEGV;
  register int quoted = 0;

  while (pos > beg
	 && ((code = SYNTAX (FETCH_CHAR (pos - 1))) == Scharquote
	     || code == Sescape))
    pos--, quoted = !quoted;
  return quoted;
}

DEFUN ("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
  "Scan from character number FROM by COUNT lists.\n\
Returns the character number of the position thus found.\n\
\n\
If DEPTH is nonzero, paren depth begins counting from that value,\n\
only places where the depth in parentheses becomes zero\n\
are candidates for stopping; COUNT such places are counted.\n\
Thus, a positive value for DEPTH means go out levels.\n\
\n\
Comments are ignored if `parse-sexp-ignore-comments' is non-nil.\n\
\n\
If the beginning or end of (the accessible part of) the buffer is reached\n\
and the depth is wrong, an error is signaled.\n\
If the depth is right but the count is not used up, nil is returned.")
  (from, count, depth)
     Lisp_Object from, count, depth;
{
  CHECK_NUMBER (from, 0);
  CHECK_NUMBER (count, 1);
  CHECK_NUMBER (depth, 2);

  return scan_lists (XINT (from), XINT (count), XINT (depth), 0);
}

DEFUN ("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
  "Scan from character number FROM by COUNT balanced expressions.\n\
If COUNT is negative, scan backwards.\n\
Returns the character number of the position thus found.\n\
\n\
Comments are ignored if `parse-sexp-ignore-comments' is non-nil.\n\
\n\
If the beginning or end of (the accessible part of) the buffer is reached\n\
in the middle of a parenthetical grouping, an error is signaled.\n\
If the beginning or end is reached between groupings\n\
but before count is used up, nil is returned.")
  (from, count)
     Lisp_Object from, count;
{
  CHECK_NUMBER (from, 0);
  CHECK_NUMBER (count, 1);

  return scan_lists (XINT (from), XINT (count), 0, 1);
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, Sbackward_prefix_chars,
  0, 0, 0,
  "Move point backward over any number of chars with prefix syntax.\n\
This includes chars with \"quote\" or \"prefix\" syntax (' or p).")
  ()
{
  int beg = BEGV;
  int pos = point;

  while (pos > beg && !char_quoted (pos - 1)
	 && (SYNTAX (FETCH_CHAR (pos - 1)) == Squote
	     || SYNTAX_PREFIX (FETCH_CHAR (pos - 1))))
    pos--;

  SET_PT (pos);

  return Qnil;
}

/* Parse forward from FROM to END,
   assuming that FROM has state OLDSTATE (nil means FROM is start of function),
   and return a description of the state of the parse at END.
   If STOPBEFORE is nonzero, stop at the start of an atom.
   If COMMENTSTOP is nonzero, stop at the start of a comment.  */

static void
scan_sexps_forward (stateptr, from, end, targetdepth,
		    stopbefore, oldstate, commentstop)
     struct lisp_parse_state *stateptr;
     register int from;
     int end, targetdepth, stopbefore;
     Lisp_Object oldstate;
     int commentstop;
{
  struct lisp_parse_state state;

  register enum syntaxcode code;
  struct level { int last, prev; };
  struct level levelstart[100];
  register struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  char prev;
  register int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  Lisp_Object tem;

  immediate_quit = 1;
  QUIT;

  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default */
    }
  else
    {
      tem = Fcar (oldstate);
      if (!NILP (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.instring = !NILP (tem) ? XINT (tem) : -1;

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.incomment = !NILP (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      start_quoted = !NILP (tem);

      /* if the eight element of the list is nil, we are in comment
	 style a. if it is non-nil, we are in comment style b */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.comstyle = !NILP (tem);
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state. */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      code = SYNTAX (FETCH_CHAR (from));
      from++;
      if (code == Scomment)
	state.comstart = from-1;
      
      else if (from < end && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from - 1))
	       && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from)))
	{
	  /* Record the comment style we have entered so that only
	     the comment-end sequence of the same style actually
	     terminates the comment section.  */
	  code = Scomment;
	  state.comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from));
	  state.comstart = from-1;
	  from++;
	}

      if (SYNTAX_PREFIX (FETCH_CHAR (from - 1)))
	continue;
      switch (SWITCH_ENUM_CAST (code))
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	startquoted:
	  if (from == end) goto endquoted;
	  from++;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	symstarted:
	  while (from < end)
	    {
	      switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		{
		case Scharquote:
		case Sescape:
		  from++;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      from++;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	startincomment:
	  if (commentstop)
	    goto done;
	  if (from != BEGV)
	    {
	      /* Enter the loop in the middle so that we find
		 a 2-char comment ender if we start in the middle of it.  */
	      prev = FETCH_CHAR (from - 1);
	      goto startincomment_1;
	    }
	  /* At beginning of buffer, enter the loop the ordinary way.  */

	case Scomment:
	  state.incomment = 1;
	  if (commentstop)
	    goto done;
	  while (1)
	    {
	      if (from == end) goto done;
	      prev = FETCH_CHAR (from);
	      if (SYNTAX (prev) == Sendcomment
		  && SYNTAX_COMMENT_STYLE (prev) == state.comstyle)
		/* Only terminate the comment section if the endcomment
		   of the same style as the start sequence has been
		   encountered.  */
		break;
	      from++;
	    startincomment_1:
	      if (from < end && SYNTAX_COMEND_FIRST (prev)
		  && SYNTAX_COMEND_SECOND (FETCH_CHAR (from))
		  && SYNTAX_COMMENT_STYLE (prev) == state.comstyle)
		/* Only terminate the comment section if the end-comment
		   sequence of the same style as the start sequence has
		   been encountered.  */
		{ from++; break; }
	    }
	  state.incomment = 0;
	  state.comstyle = 0;	/* reset the comment style */
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = from - 1;
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (!--targetdepth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (!++targetdepth) goto done;
	  break;

	case Sstring:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	  state.instring = FETCH_CHAR (from - 1);
	startinstring:
	  while (1)
	    {
	      if (from >= end) goto done;
	      if (FETCH_CHAR (from) == state.instring) break;
	      switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		{
		case Scharquote:
		case Sescape:
		  from++;
		startquotedinstring:
		  if (from >= end) goto endquoted;
		}
	      from++;
	    }
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  from++;
	  break;

	case Smath:
	  break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from--;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  immediate_quit = 0;

  *stateptr = state;
}

/* This comment supplies the doc string for parse-partial-sexp,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("parse-partial-sexp", Ffoo, Sfoo, 2, 6, 0,
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.\n\
Parsing stops at TO or when certain criteria are met;\n\
 point is set to where parsing stops.\n\
If fifth arg STATE is omitted or nil,\n\
 parsing assumes that FROM is the beginning of a function.\n\
Value is a list of eight elements describing final state of parsing:\n\
 0. depth in parens.\n\
 1. character address of start of innermost containing list; nil if none.\n\
 2. character address of start of last complete sexp terminated.\n\
 3. non-nil if inside a string.\n\
    (it is the character that will terminate the string.)\n\
 4. t if inside a comment.\n\
 5. t if following a quote character.\n\
 6. the minimum paren-depth encountered during this scan.\n\
 7. t if in a comment of style `b'.\n\
If third arg TARGETDEPTH is non-nil, parsing stops if the depth\n\
in parentheses becomes equal to TARGETDEPTH.\n\
Fourth arg STOPBEFORE non-nil means stop when come to\n\
 any character that starts a sexp.\n\
Fifth arg STATE is an eight-list like what this function returns.\n\
It is used to initialize the state of the parse.  Its second and third
elements are ignored.
Sixth args COMMENTSTOP non-nil means stop at the start of a comment.")
  (from, to, targetdepth, stopbefore, state, commentstop)
*/

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 6, 0,
  0 /* See immediately above */)
  (from, to, targetdepth, stopbefore, oldstate, commentstop)
     Lisp_Object from, to, targetdepth, stopbefore, oldstate, commentstop;
{
  struct lisp_parse_state state;
  int target;

  if (!NILP (targetdepth))
    {
      CHECK_NUMBER (targetdepth, 3);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  validate_region (&from, &to);
  scan_sexps_forward (&state, XINT (from), XINT (to),
		      target, !NILP (stopbefore), oldstate,
		      !NILP (commentstop));

  SET_PT (state.location);
  
  return Fcons (make_number (state.depth),
	   Fcons (state.prevlevelstart < 0 ? Qnil : make_number (state.prevlevelstart),
	     Fcons (state.thislevelstart < 0 ? Qnil : make_number (state.thislevelstart),
	       Fcons (state.instring >= 0 ? make_number (state.instring) : Qnil,
		 Fcons (state.incomment ? Qt : Qnil,
		   Fcons (state.quoted ? Qt : Qnil,
			  Fcons (make_number (state.mindepth),
				 Fcons (state.comstyle ? Qt : Qnil,
					Qnil))))))));
}

init_syntax_once ()
{
  register int i;
  register struct Lisp_Vector *v;

  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-syntax-table
    so that copy-syntax-table will know not to try to copy from garbage */
  Vstandard_syntax_table = Qnil;
  Vstandard_syntax_table = Fcopy_syntax_table (Qnil);

  v = XVECTOR (Vstandard_syntax_table);

  for (i = 'a'; i <= 'z'; i++)
    XSETFASTINT (v->contents[i], (int) Sword);
  for (i = 'A'; i <= 'Z'; i++)
    XSETFASTINT (v->contents[i], (int) Sword);
  for (i = '0'; i <= '9'; i++)
    XSETFASTINT (v->contents[i], (int) Sword);
  XSETFASTINT (v->contents['$'], (int) Sword);
  XSETFASTINT (v->contents['%'], (int) Sword);

  XSETFASTINT (v->contents['('], (int) Sopen + (')' << 8));
  XSETFASTINT (v->contents[')'], (int) Sclose + ('(' << 8));
  XSETFASTINT (v->contents['['], (int) Sopen + (']' << 8));
  XSETFASTINT (v->contents[']'], (int) Sclose + ('[' << 8));
  XSETFASTINT (v->contents['{'], (int) Sopen + ('}' << 8));
  XSETFASTINT (v->contents['}'], (int) Sclose + ('{' << 8));
  XSETFASTINT (v->contents['"'], (int) Sstring);
  XSETFASTINT (v->contents['\\'], (int) Sescape);

  for (i = 0; i < 10; i++)
    XSETFASTINT (v->contents["_-+*/&|<>="[i]], (int) Ssymbol);

  for (i = 0; i < 12; i++)
    XSETFASTINT (v->contents[".,;:?!#@~^'`"[i]], (int) Spunct);
}

syms_of_syntax ()
{
  Qsyntax_table_p = intern ("syntax-table-p");
  staticpro (&Qsyntax_table_p);

  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments,
    "Non-nil means `forward-sexp', etc., should treat comments as whitespace.");

  words_include_escapes = 0;
  DEFVAR_BOOL ("words-include-escapes", &words_include_escapes,
    "Non-nil means `forward-word', etc., should treat escape chars part of words.");

  defsubr (&Ssyntax_table_p);
  defsubr (&Ssyntax_table);
  defsubr (&Sstandard_syntax_table);
  defsubr (&Scopy_syntax_table);
  defsubr (&Sset_syntax_table);
  defsubr (&Schar_syntax);
  defsubr (&Smatching_paren);
  defsubr (&Smodify_syntax_entry);
  defsubr (&Sdescribe_syntax);

  defsubr (&Sforward_word);

  defsubr (&Sforward_comment);
  defsubr (&Sscan_lists);
  defsubr (&Sscan_sexps);
  defsubr (&Sbackward_prefix_chars);
  defsubr (&Sparse_partial_sexp);
}
