/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985, 87, 93, 94, 95, 1997 Free Software Foundation, Inc.

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


#include <config.h>
#include <ctype.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "charset.h"

/* Make syntax table lookup grant data in gl_state.  */
#define SYNTAX_ENTRY_VIA_PROPERTY

#include "syntax.h"
#include "intervals.h"

/* We use these constants in place for comment-style and
   string-ender-char to distinguish  comments/strings started by
   comment_fence and string_fence codes.  */

#define ST_COMMENT_STYLE (256 + 1)
#define ST_STRING_STYLE (256 + 2)
#include "category.h"

Lisp_Object Qsyntax_table_p, Qsyntax_table, Qscan_error;

static void scan_sexps_forward ();
static int char_quoted ();

int words_include_escapes;
int parse_sexp_lookup_properties;

/* Used as a temporary in SYNTAX_ENTRY and other macros in syntax.h,
   if not compiled with GCC.  No need to mark it, since it is used
   only very temporarily.  */
Lisp_Object syntax_temp;

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
  {
    int depth;		/* Depth at end of parsing.  */
    int instring;	/* -1 if not within string, else desired terminator.  */
    int incomment;	/* Nonzero if within a comment at end of parsing.  */
    int comstyle;	/* comment style a=0, or b=1, or ST_COMMENT_STYLE.  */
    int quoted;		/* Nonzero if just after an escape char at end of parsing */
    int thislevelstart;	/* Char number of most recent start-of-expression at current level */
    int prevlevelstart; /* Char number of start of containing expression */
    int location;	/* Char number at which parsing stopped.  */
    int mindepth;	/* Minimum depth seen while scanning.  */
    int comstr_start;	/* Position just after last comment/string starter.  */
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


struct gl_state_s gl_state;		/* Global state of syntax parser.  */

INTERVAL interval_of ();
#define INTERVALS_AT_ONCE 10		/* 1 + max-number of intervals
					   to scan to property-change.  */

/* 
   Update gl_state to an appropriate interval which contains POS.  The
   sign of COUNT give the relative position of POS wrt the previously
   valid interval.  If INIT, only [be]_property fields of gl_state are
   valid at start, the  rest is filled basing on OBJECT.

   `gl_state.*_i' are the intervals, and pos is further in the search
   direction than the intervals - or in an interval.  We update the
   current syntax-table basing on the property of this interval, and
   update the interval to start further than POS - or be
   NULL_INTERVAL.  We also update lim_property to be the next value of
   pos to call this subroutine again - or be before/after the
   start/end of OBJECT.  */

void
update_syntax_table (pos, count, init, object)
     int pos, count, init;
     Lisp_Object object;
{
  Lisp_Object tmp_table;
  int cnt = 0, doing_extra = 0, invalidate = 1;
  INTERVAL i, oldi;

  if (init)
    {
      gl_state.start = gl_state.b_property;
      gl_state.stop = gl_state.e_property;
      gl_state.forward_i = interval_of (pos, object);
      i = gl_state.backward_i = gl_state.forward_i;
      gl_state.left_ok = gl_state.right_ok = 1;
      invalidate = 0;
      if (NULL_INTERVAL_P (i))
	return;
      gl_state.b_property = i->position - 1;
      gl_state.e_property = INTERVAL_LAST_POS (i);
      goto update;
    }
  oldi = i = count > 0 ? gl_state.forward_i : gl_state.backward_i;

  /* We are guarantied to be called with pos either in i, of further off.  */
  if (NULL_INTERVAL_P (i))
    error ("Error in syntax_table logic for to-the-end intervals");
  else if (pos < i->position)		/* Move left.  */
    {
      if (count > 0)
	error ("Error in syntax_table logic for intervals <-.");
      /* Update the interval.  */
      i = update_interval (i, pos);
      if (oldi->position != INTERVAL_LAST_POS (i))
	{
	  invalidate = 0;
	  gl_state.right_ok = 1;	/* Invalidate the other end.  */
	  gl_state.forward_i = i;
	  gl_state.e_property = INTERVAL_LAST_POS (i);
	}
    } 
  else if (pos >= INTERVAL_LAST_POS (i)) /* Move right.  */
    {
      if (count < 0)
	error ("Error in syntax_table logic for intervals ->.");
      /* Update the interval.  */
      i = update_interval (i, pos);
      if (i->position != INTERVAL_LAST_POS (oldi))
	{
	  invalidate = 0;
	  gl_state.left_ok = 1;		/* Invalidate the other end.  */
	  gl_state.backward_i = i;
	  gl_state.b_property = i->position - 1;
	}
    }
  else if (count > 0 ? gl_state.right_ok : gl_state.left_ok)
    {
      /* We do not need to recalculate tmp_table.  */
      tmp_table = gl_state.old_prop;
    }

  update:
  tmp_table = textget (i->plist, Qsyntax_table);

  if (invalidate)
    invalidate = !EQ (tmp_table, gl_state.old_prop); /* Need to invalidate? */
      
  if (invalidate)			/* Did not get to adjacent interval.  */
    {					/* with the same table => */
					/* invalidate the old range.  */
      if (count > 0)
	{
	  gl_state.backward_i = i;
	  gl_state.left_ok = 1;		/* Invalidate the other end.  */
	  gl_state.b_property = i->position - 1;
	} 
      else 
	{
	  gl_state.forward_i = i;	
	  gl_state.right_ok = 1;	/* Invalidate the other end.  */
	  gl_state.e_property = INTERVAL_LAST_POS (i);
	}
    }

  gl_state.current_syntax_table = tmp_table;
  gl_state.old_prop = tmp_table;
  if (EQ (Fsyntax_table_p (tmp_table), Qt))
    {
      gl_state.use_global = 0;
    } 
  else if (CONSP (tmp_table))
    {
      gl_state.use_global = 1;
      gl_state.global_code = tmp_table;
    }
  else 
    {
      gl_state.use_global = 0;
      gl_state.current_syntax_table = current_buffer->syntax_table;
    }

  while (!NULL_INTERVAL_P (i))
    {
      if (cnt && !EQ (tmp_table, textget (i->plist, Qsyntax_table)))
	{
	  if (count > 0)
	    gl_state.right_ok = 0;
	  else 
	    gl_state.left_ok = 0;
	  break;	  
	}
      else if (cnt == INTERVALS_AT_ONCE) 
	{
	  if (count > 0)
	    gl_state.right_ok = 1;
	  else 
	    gl_state.left_ok = 1;
	  break;
	}
      cnt++;
      i = count > 0 ? next_interval (i) : previous_interval (i);
    }
  if (NULL_INTERVAL_P (i)) 
    {					/* This property goes to the end.  */
      if (count > 0)
	gl_state.e_property = gl_state.stop;
      else
	gl_state.b_property = gl_state.start;
    } 
  else 
    {
      if (count > 0) 
	{
	  gl_state.e_property = i->position;
	  gl_state.forward_i = i;
	}
      else 
	{
	  gl_state.b_property = i->position + LENGTH (i) - 1;
	  gl_state.backward_i = i;
	}
    }    
}

/* Returns TRUE if char at POS is quoted.
   Global syntax-table data should be set up already to be good at pos
   or after.  On return global syntax data is good for lookup at POS. */

static int
char_quoted (pos)
     register int pos;
{
  register enum syntaxcode code;
  register int beg = BEGV;
  register int quoted = 0;
  int temp_pos = pos;

  DEC_POS (temp_pos);
  while (temp_pos >= beg
	 && ( UPDATE_SYNTAX_TABLE_BACKWARD (temp_pos), 1)
	 && ((code = SYNTAX (FETCH_CHAR (temp_pos))) == Scharquote
	     || code == Sescape))
    {
      temp_pos--, quoted = !quoted;
    }
  UPDATE_SYNTAX_TABLE (pos);
  return quoted;
}

/* Find a defun-start that is the last one before POS (or nearly the last).
   We record what we find, so that another call in the same area
   can return the same value right away.  

   There is no promise at which position the global syntax data is
   valid on return from the subroutine, so the caller should explicitly
   update the global data.  */

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

  /* We optimize syntax-table lookup for rare updates.  Thus we accept
     only those `^\s(' which are good in global _and_ text-property
     syntax-tables.  */
  gl_state.current_syntax_table = current_buffer->syntax_table;
  gl_state.use_global = 0;
  while (tem > BEGV)
    {
      /* Open-paren at start of line means we found our defun-start.  */
      if (SYNTAX (FETCH_CHAR (tem)) == Sopen)
	{
	  SETUP_SYNTAX_TABLE (tem + 1, -1);	/* Try again... */
	  if (SYNTAX (FETCH_CHAR (tem)) == Sopen)
	    break;
	  /* Now fallback to the default value.  */
	  gl_state.current_syntax_table = current_buffer->syntax_table;
	  gl_state.use_global = 0;
	}
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

/* Checks whether FROM is the end of comment.  Does not try to
   fallback more than to STOP.
   Returns -1 if cannot find comment ending at from, otherwise start
   of comment.  Global syntax data remains valid for
   backward search starting at the returned value (or at FROM, if
   the search was not successful).  */

static int
back_comment (from, stop, comstyle)
     int from, stop, comstyle;
{
  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.

     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int parity = 0;
  int my_stringend = 0;
  int string_lossage = 0;
  int comment_end = from;
  int comstart_pos = 0;
  int comstart_parity = 0;
  int scanstart = from - 1;
  register enum syntaxcode code;
  int c;

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      /* Move back and examine a character.  */
      DEC_POS (from);
      UPDATE_SYNTAX_TABLE_BACKWARD (from);

      c = FETCH_CHAR (from);
      code = SYNTAX (c);

      /* If this char is the second of a 2-char comment sequence,
	 back up and give the pair the appropriate syntax.  */
      if (from > stop && SYNTAX_COMEND_SECOND (c)
	  && SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1)))
	{
	  code = Sendcomment;
	  DEC_POS (from);
	  /* This is apparently the best we can do: */
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  c = FETCH_CHAR (from);
	}
			
      /* If this char starts a 2-char comment start sequence,
	 treat it like a 1-char comment starter.  */
      if (from < scanstart && SYNTAX_COMSTART_SECOND (c)
	  && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from - 1))
	  && comstyle == SYNTAX_COMMENT_STYLE (c))
	{
	  code = Scomment;
	  DEC_POS (from);
	  /* This is apparently the best we can do: */
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  c = FETCH_CHAR (from);
	}

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

      if (code == Sstring_fence || code == Scomment_fence)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend = 
		code == Sstring_fence ? ST_STRING_STYLE : ST_COMMENT_STYLE;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != (code == Sstring_fence 
				    ? ST_STRING_STYLE : ST_COMMENT_STYLE))
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
    {
      from = comment_end;
      UPDATE_SYNTAX_TABLE_FORWARD (comment_end - 1);
    }
  /* If the earliest comment starter
     is followed by uniform paired string quotes or none,
     we know it can't be inside a string
     since if it were then the comment ender would be inside one.
     So it does start a comment.  Skip back to it.  */
  else if (comstart_parity == 0 && !string_lossage) 
    {
      from = comstart_pos;
      /* Globals are correct now.  */
    }
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
	{
	  /* scan_sexps_forward changed the direction of search in
	     global variables, so we need to update it completely.  */
	  
	  from = state.comstr_start;
	}
      else
	{
	  from = comment_end;	  
	}
      UPDATE_SYNTAX_TABLE_FORWARD (from - 1);
    }
    
  return from;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
  "Return t if OBJECT is a syntax table.\n\
Currently, any char-table counts as a syntax table.")
  (object)
     Lisp_Object object;
{
  if (CHAR_TABLE_P (object)
      && EQ (XCHAR_TABLE (object)->purpose, Qsyntax_table))
    return Qt;
  return Qnil;
}

static void
check_syntax_table (obj)
     Lisp_Object obj;
{
  if (!(CHAR_TABLE_P (obj)
	&& EQ (XCHAR_TABLE (obj)->purpose, Qsyntax_table)))
    wrong_type_argument (Qsyntax_table_p, obj);
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
  Lisp_Object copy;

  if (!NILP (table))
    check_syntax_table (table);
  else
    table = Vstandard_syntax_table;

  copy = Fcopy_sequence (table);

  /* Only the standard syntax table should have a default element.
     Other syntax tables should inherit from parents instead.  */
  XCHAR_TABLE (copy)->defalt = Qnil;

  /* Copied syntax tables should all have parents.
     If we copied one with no parent, such as the standard syntax table,
     use the standard syntax table as the copy's parent.  */
  if (NILP (XCHAR_TABLE (copy)->parent))
    Fset_char_table_parent (copy, Vstandard_syntax_table);
  return copy;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
  "Select a new syntax table for the current buffer.\n\
One argument, a syntax table.")
  (table)
     Lisp_Object table;
{
  check_syntax_table (table);
  current_buffer->syntax_table = table;
  /* Indicate that this buffer now has a specified syntax table.  */
  current_buffer->local_var_flags
    |= XFASTINT (buffer_local_flags.syntax_table);
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things.  */

unsigned char syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, (char) Scomment_fence, (char) Sstring, 0377,
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
    0377, 0377, 0377, 0377, (char) Sstring_fence, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it.  */

char syntax_code_spec[16] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>', '@',
    '!', '|'
  };

/* Indexed by syntax code, give the object (cons of syntax code and
   nil) to be stored in syntax table.  Since these objects can be
   shared among syntax tables, we generate them in advance.  By
   sharing objects, the function `describe-syntax' can give a more
   compact listing.  */
static Lisp_Object Vsyntax_code_object;


/* Look up the value for CHARACTER in syntax table TABLE's parent
   and its parents.  SYNTAX_ENTRY calls this, when TABLE itself has nil
   for CHARACTER.  It's actually used only when not compiled with GCC.  */

Lisp_Object
syntax_parent_lookup (table, character)
     Lisp_Object table;
     int character;
{
  Lisp_Object value;

  while (1)
    {
      table = XCHAR_TABLE (table)->parent;
      if (NILP (table))
	return Qnil;

      value = XCHAR_TABLE (table)->contents[character];
      if (!NILP (value))
	return value;
    }
}

DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
  "Return the syntax code of CHARACTER, described by a character.\n\
For example, if CHARACTER is a word constituent,\n\
the character `w' is returned.\n\
The characters that correspond to various syntax codes\n\
are listed in the documentation of `modify-syntax-entry'.")
  (character)
     Lisp_Object character;
{
  int char_int;
  gl_state.current_syntax_table = current_buffer->syntax_table;

  gl_state.use_global = 0;
  CHECK_NUMBER (character, 0);
  char_int = XINT (character);
  return make_number (syntax_code_spec[(int) SYNTAX (char_int)]);
}

DEFUN ("matching-paren", Fmatching_paren, Smatching_paren, 1, 1, 0,
  "Return the matching parenthesis of CHARACTER, or nil if none.")
  (character)
     Lisp_Object character;
{
  int char_int, code;
  gl_state.current_syntax_table = current_buffer->syntax_table;
  gl_state.use_global = 0;
  CHECK_NUMBER (character, 0);
  char_int = XINT (character);
  code = SYNTAX (char_int);
  if (code == Sopen || code == Sclose)
    return SYNTAX_MATCH (char_int);
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
 1 means CHAR is the start of a two-char comment start sequence.\n\
 2 means CHAR is the second character of such a sequence.\n\
 3 means CHAR is the start of a two-char comment end sequence.\n\
 4 means CHAR is the second character of such a sequence.\n\
\n\
There can be up to two orthogonal comment sequences.  This is to support\n\
language modes such as C++.  By default, all comment sequences are of style\n\
a, but you can set the comment sequence style to b (on the second character\n\
of a comment-start, or the first character of a comment-end sequence) using\n\
this flag:\n\
 b means CHAR is part of comment sequence b.\n\
\n\
 p means CHAR is a prefix character for `backward-prefix-chars';\n\
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
  register unsigned char *p;
  register enum syntaxcode code;
  int val;
  Lisp_Object match;

  CHECK_NUMBER (c, 0);
  CHECK_STRING (newentry, 1);

  if (NILP (syntax_table))
    syntax_table = current_buffer->syntax_table;
  else
    check_syntax_table (syntax_table);

  p = XSTRING (newentry)->data;
  code = (enum syntaxcode) syntax_spec_code[*p++];
  if (((int) code & 0377) == 0377)
    error ("invalid syntax description letter: %c", p[-1]);

  if (code == Sinherit)
    {
      SET_RAW_SYNTAX_ENTRY (syntax_table, XINT (c), Qnil);
      return Qnil;
    }

  if (*p)
    {
      int len;
      int character = STRING_CHAR_AND_LENGTH (p, XSTRING (newentry)->size - 1,
					      len);
      XSETINT (match, character);
      if (XFASTINT (match) == ' ')
	match = Qnil;
      p += len;
    }
  else
    match = Qnil;

  val = (int) code;
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
	
  if (val < XVECTOR (Vsyntax_code_object)->size && NILP (match))
    newentry = XVECTOR (Vsyntax_code_object)->contents[val];
  else
    /* Since we can't use a shared object, let's make a new one.  */
    newentry = Fcons (make_number (val), match);
    
  SET_RAW_SYNTAX_ENTRY (syntax_table, XINT (c), newentry);

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
  Lisp_Object first, match_lisp;

  Findent_to (make_number (16), make_number (1));

  if (NILP (value))
    {
      insert_string ("default\n");
      return;
    }

  if (CHAR_TABLE_P (value))
    {
      insert_string ("deeper char-table ...\n");
      return;
    }

  if (!CONSP (value))
    {
      insert_string ("invalid\n");
      return;
    }

  first = XCONS (value)->car;
  match_lisp = XCONS (value)->cdr;

  if (!INTEGERP (first) || !(NILP (match_lisp) || INTEGERP (match_lisp)))
    {
      insert_string ("invalid\n");
      return;
    }

  code = (enum syntaxcode) (XINT (first) & 0377);
  start1 = (XINT (first) >> 16) & 1;
  start2 = (XINT (first) >> 17) & 1;
  end1 = (XINT (first) >> 18) & 1;
  end2 = (XINT (first) >> 19) & 1;
  prefix = (XINT (first) >> 20) & 1;
  comstyle = (XINT (first) >> 21) & 1;

  if ((int) code < 0 || (int) code >= (int) Smax)
    {
      insert_string ("invalid");
      return;
    }
  desc = syntax_code_spec[(int) code];

  str[0] = desc, str[1] = 0;
  insert (str, 1);

  if (NILP (match_lisp))
    insert (" ", 1);
  else
    insert_char (XINT (match_lisp));

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
    default:
      insert_string ("invalid");
      return;
    }

  if (!NILP (match_lisp))
    {
      insert_string (", matches ");
      insert_char (XINT (match_lisp));
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
  describe_vector (vector, Qnil, describe_syntax, 0, Qnil, Qnil, (int *) 0, 0);
  while (! NILP (XCHAR_TABLE (vector)->parent))
    {
      vector = XCHAR_TABLE (vector)->parent;
      insert_string ("\nThe parent syntax table is:");
      describe_vector (vector, Qnil, describe_syntax, 0, Qnil, Qnil,
		       (int *) 0, 0);
    }
	
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

int parse_sexp_ignore_comments;

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

scan_words (from, count)
     register int from, count;
{
  register int beg = BEGV;
  register int end = ZV;
  register enum syntaxcode code;
  int ch0, ch1;
  int temp_pos;

  immediate_quit = 1;
  QUIT;

  SETUP_SYNTAX_TABLE (from, count);

  while (count > 0)
    {
      while (1)
	{
	  if (from == end)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  ch0 = FETCH_CHAR (from);
	  code = SYNTAX (ch0);
	  INC_POS (from);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}
      /* Now CH0 is a character which begins a word and FROM is the
         position of the next character.  */
      while (1)
	{
	  if (from == end) break;
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  ch1 = FETCH_CHAR (from);
	  code = SYNTAX (ch1);
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword || WORD_BOUNDARY_P (ch0, ch1))
	      break;
	  INC_POS (from);
	  ch0 = ch1;
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
	  DEC_POS (from);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  ch1 = FETCH_CHAR (from);
	  code = SYNTAX (ch1);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}
      /* Now CH1 is a character which ends a word and FROM is the
         position of it.  */
      while (1)
	{
	  if (from == beg) break;
	  temp_pos = from;
	  DEC_POS (temp_pos);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  ch0 = FETCH_CHAR (temp_pos);
	  code = SYNTAX (ch0);
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword || WORD_BOUNDARY_P (ch0, ch1))
	      break;
	  from = temp_pos;
	  ch1 = ch0;
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

  if (!(val = scan_words (PT, XINT (count))))
    {
      SET_PT (XINT (count) > 0 ? ZV : BEGV);
      return Qnil;
    }
  SET_PT (val);
  return Qt;
}

Lisp_Object skip_chars ();

DEFUN ("skip-chars-forward", Fskip_chars_forward, Sskip_chars_forward, 1, 2, 0,
  "Move point forward, stopping before a char not in STRING, or at pos LIM.\n\
STRING is like the inside of a `[...]' in a regular expression\n\
except that `]' is never special and `\\' quotes `^', `-' or `\\'.\n\
Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.\n\
With arg \"^a-zA-Z\", skips nonletters stopping before first letter.\n\
Returns the distance traveled, either zero or positive.")
  (string, lim)
     Lisp_Object string, lim;
{
  return skip_chars (1, 0, string, lim);
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, Sskip_chars_backward, 1, 2, 0,
  "Move point backward, stopping after a char not in STRING, or at pos LIM.\n\
See `skip-chars-forward' for details.\n\
Returns the distance traveled, either zero or negative.")
  (string, lim)
     Lisp_Object string, lim;
{
  return skip_chars (0, 0, string, lim);
}

DEFUN ("skip-syntax-forward", Fskip_syntax_forward, Sskip_syntax_forward, 1, 2, 0,
  "Move point forward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.\n\
Stop before a char whose syntax is not in SYNTAX, or at position LIM.\n\
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\n\
This function returns the distance traveled, either zero or positive.")
  (syntax, lim)
     Lisp_Object syntax, lim;
{
  return skip_chars (1, 1, syntax, lim);
}

DEFUN ("skip-syntax-backward", Fskip_syntax_backward, Sskip_syntax_backward, 1, 2, 0,
  "Move point backward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.\n\
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.\n\
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\n\
This function returns the distance traveled, either zero or negative.")
  (syntax, lim)
     Lisp_Object syntax, lim;
{
  return skip_chars (0, 1, syntax, lim);
}

Lisp_Object
skip_chars (forwardp, syntaxp, string, lim)
     int forwardp, syntaxp;
     Lisp_Object string, lim;
{
  register unsigned char *p, *pend;
  register unsigned int c;
  register int ch;
  unsigned char fastmap[0400];
  /* If SYNTAXP is 0, STRING may contain multi-byte form of characters
     of which codes don't fit in FASTMAP.  In that case, we set the
     first byte of multibyte form (i.e. base leading-code) in FASTMAP
     and set the actual ranges of characters in CHAR_RANGES.  In the
     form "X-Y" of STRING, both X and Y must belong to the same
     character set because a range striding across character sets is
     meaningless.  */
  int *char_ranges
    = (int *) alloca (XSTRING (string)->size * (sizeof (int)) * 2);
  int n_char_ranges = 0;
  int negate = 0;
  register int i;
  int multibyte = !NILP (current_buffer->enable_multibyte_characters);

  CHECK_STRING (string, 0);

  if (NILP (lim))
    XSETINT (lim, forwardp ? ZV : BEGV);
  else
    CHECK_NUMBER_COERCE_MARKER (lim, 1);

  /* In any case, don't allow scan outside bounds of buffer.  */
  /* jla turned this off, for no known reason.
     bfox turned the ZV part on, and rms turned the
     BEGV part back on.  */
  if (XINT (lim) > ZV)
    XSETFASTINT (lim, ZV);
  if (XINT (lim) < BEGV)
    XSETFASTINT (lim, BEGV);

  p = XSTRING (string)->data;
  pend = p + XSTRING (string)->size;
  bzero (fastmap, sizeof fastmap);

  if (p != pend && *p == '^')
    {
      negate = 1; p++;
    }

  /* Find the characters specified and set their elements of fastmap.
     If syntaxp, each character counts as itself.
     Otherwise, handle backslashes and ranges specially.  */

  while (p != pend)
    {
      c = *p;
      if (multibyte)
	{
	  ch = STRING_CHAR (p, pend - p);
	  p += BYTES_BY_CHAR_HEAD (*p);
	}
      else
	{
	  ch = c;
	  p++;
	}
      if (syntaxp)
	fastmap[syntax_spec_code[c]] = 1;
      else
	{
	  if (c == '\\')
	    {
	      if (p == pend) break;
	      c = *p++;
	    }
	  if (p != pend && *p == '-')
	    {
	      unsigned int ch2;

	      p++;
	      if (p == pend) break;
	      if (SINGLE_BYTE_CHAR_P (ch))
		while (c <= *p)
		  {
		    fastmap[c] = 1;
		    c++;
		  }
	      else
		{
		  fastmap[c] = 1; /* C is the base leading-code.  */
		  ch2 = STRING_CHAR (p, pend - p);
		  if (ch <= ch2)
		    char_ranges[n_char_ranges++] = ch,
		    char_ranges[n_char_ranges++] = ch2;
		}
	      p += multibyte ? BYTES_BY_CHAR_HEAD (*p) : 1;
	    }
	  else
	    {
	      fastmap[c] = 1;
	      if (!SINGLE_BYTE_CHAR_P (ch))
		{
		  char_ranges[n_char_ranges++] = ch;
		  char_ranges[n_char_ranges++] = ch;
		}
	    }
	}
    }

  /* If ^ was the first character, complement the fastmap.  In
     addition, as all multibyte characters have possibility of
     matching, set all entries for base leading codes, which is
     harmless even if SYNTAXP is 1.  */

  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      {
	if (!multibyte || !BASE_LEADING_CODE_P (i))
	  fastmap[i] ^= 1;
	else
	  fastmap[i] = 1;
      }

  {
    int start_point = PT;
    int pos = PT;

    immediate_quit = 1;
    if (syntaxp)
      {
        SETUP_SYNTAX_TABLE (pos, forwardp ? 1 : -1);
	if (forwardp)
	  {
	    if (multibyte)
	      {
		while (pos < XINT (lim)
		       && fastmap[(int) SYNTAX (FETCH_CHAR (pos))])
		  {
		    INC_POS (pos);
		    UPDATE_SYNTAX_TABLE_FORWARD (pos);
		  }
	      }
	    else
	      {
		while (pos < XINT (lim)
		       && fastmap[(int) SYNTAX (FETCH_BYTE (pos))])
		  {
		    pos++;
		    UPDATE_SYNTAX_TABLE_FORWARD (pos);
		  }
	      }
	  }
	else
	  {
	    if (multibyte)
	      {
		while (pos > XINT (lim))
		  {
		    int savepos = pos;
		    DEC_POS (pos);
		    UPDATE_SYNTAX_TABLE_BACKWARD (pos);
		    if (!fastmap[(int) SYNTAX (FETCH_CHAR (pos))])
		      {
			pos = savepos;
			break;
		      }
		  }
	      }
	    else
	      {
		while (pos > XINT (lim))
		  {
		    pos--;
		    UPDATE_SYNTAX_TABLE_BACKWARD (pos);
		    if (!fastmap[(int) SYNTAX (FETCH_BYTE (pos))])
		      {
			pos++;
			break;
		      }
		  }
	      }
	  }
      }
    else
      {
	if (forwardp)
	  {
	    if (multibyte)
	      while (pos < XINT (lim) && fastmap[(c = FETCH_BYTE (pos))])
		{
		  if (!BASE_LEADING_CODE_P (c))
		    pos++;
		  else if (n_char_ranges)
		    {
		      /* We much check CHAR_RANGES for a multibyte
			 character.  */
		      ch = FETCH_MULTIBYTE_CHAR (pos);
		      for (i = 0; i < n_char_ranges; i += 2)
			if ((ch >= char_ranges[i] && ch <= char_ranges[i + 1]))
			  break;
		      if (!(negate ^ (i < n_char_ranges)))
			break;

		      INC_POS (pos);
		    }
		  else
		    {
		      if (!negate) break;
		      INC_POS (pos);
		    }
		}
	    else
	      while (pos < XINT (lim) && fastmap[FETCH_BYTE (pos)])
		pos++;
	  }
	else
	  {
	    if (multibyte)
	      while (pos > XINT (lim))
		{
		  int savepos = pos;
		  DEC_POS (pos);
		  if (fastmap[(c = FETCH_BYTE (pos))])
		    {
		      if (!BASE_LEADING_CODE_P (c))
			;
		      else if (n_char_ranges)
			{
			  /* We much check CHAR_RANGES for a multibyte
			     character.  */
			  ch = FETCH_MULTIBYTE_CHAR (pos);
			  for (i = 0; i < n_char_ranges; i += 2)
			    if (ch >= char_ranges[i] && ch <= char_ranges[i + 1])
			      break;
			  if (!(negate ^ (i < n_char_ranges)))
			    {
			      pos = savepos;
			      break;
			    }
			}
		      else
			if (!negate)
			  {
			    pos = savepos;
			    break;
			  }
		    }
		  else
		    {
		      pos = savepos;
		      break;
		    }
		}
	    else
	      while (pos > XINT (lim) && fastmap[FETCH_BYTE (pos - 1)])
		pos--;
	  }
      }

    if (multibyte
	/* INC_POS or DEC_POS might have moved POS over LIM.  */
	&& (forwardp ? (pos > XINT (lim)) : (pos < XINT (lim))))
      pos = XINT (lim);

    SET_PT (pos);
    immediate_quit = 0;

    return make_number (PT - start_point);
  }
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
  register int c, c1;
  register enum syntaxcode code;
  int comstyle = 0;	    /* style of comment encountered */
  int found;
  int count1;
  int temp_pos;

  CHECK_NUMBER (count, 0);
  count1 = XINT (count);
  stop = count1 > 0 ? ZV : BEGV;

  immediate_quit = 1;
  QUIT;

  from = PT;

  SETUP_SYNTAX_TABLE (from, count1);
  while (count1 > 0)
    {
      do
	{
	  if (from == stop)
	    {
	      SET_PT (from);
	      immediate_quit = 0;
	      return Qnil;
	    }
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  INC_POS (from);
	  comstyle = 0;
	  if (from < stop && SYNTAX_COMSTART_FIRST (c)
	      && (c1 = FETCH_CHAR (from),
		  SYNTAX_COMSTART_SECOND (c1)))
	    {
	      /* We have encountered a comment start sequence and we 
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section.  */
	      code = Scomment;
	      comstyle = SYNTAX_COMMENT_STYLE (c1);
	      INC_POS (from);
	    }
	}
      while (code == Swhitespace || code == Sendcomment);
      if (code != Scomment && code != Scomment_fence)
	{
	  immediate_quit = 0;
	  DEC_POS (from);
	  SET_PT (from);
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
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  c = FETCH_CHAR (from);
	  INC_POS (from);
	  if (SYNTAX (c) == Sendcomment
	      && SYNTAX_COMMENT_STYLE (c) == comstyle)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section */
	    break;
	  if (SYNTAX (c) == Scomment_fence
	      && comstyle == ST_COMMENT_STYLE)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section.  */
	    break;
	  if (from < stop && SYNTAX_COMEND_FIRST (c)
	      && (c1 = FETCH_CHAR (from),
		  SYNTAX_COMEND_SECOND (c1))
	      && SYNTAX_COMMENT_STYLE (c) == comstyle)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section */
	    { INC_POS (from); break; }
	}
      /* We have skipped one comment.  */
      count1--;
    }

  while (count1 < 0)
    {
      while (1)
	{
	  int quoted;
	  if (from <= stop)
	    {
	      SET_PT (stop);
	      immediate_quit = 0;
	      return Qnil;
	    }

	  DEC_POS (from);
	  quoted = char_quoted (from);
	  if (quoted)
	    {
	      DEC_POS (from);
	      goto leave;		/* ????? XXXXX */
	    }
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  comstyle = 0;
	  if (code == Sendcomment)
	    comstyle = SYNTAX_COMMENT_STYLE (c);
	  temp_pos = from;
	  DEC_POS (temp_pos);
	  if (from > stop && SYNTAX_COMEND_SECOND (c)
	      && (c1 = FETCH_CHAR (temp_pos),
		  SYNTAX_COMEND_FIRST (c1))
	      && !char_quoted (temp_pos))
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      code = Sendcomment;
	      comstyle = SYNTAX_COMMENT_STYLE (c1);
	      from = temp_pos;
	    }
	  if (from > stop && SYNTAX_COMSTART_SECOND (c)
	      && (c1 = FETCH_CHAR (temp_pos),
		  SYNTAX_COMSTART_FIRST (c1))
	      && !char_quoted (temp_pos))
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      code = Scomment;
	      from = temp_pos;
	    }

	  if (code == Scomment_fence)
	    {
	      /* Skip until first preceding unquoted comment_fence.  */
	      int found = 0, ini = from;
	      
	      while (--from != stop)
		{
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  c = FETCH_CHAR (from);
		  if (SYNTAX (c) == Scomment_fence && !char_quoted (from)) 
		    {
		      found = 1; 
		      break;
		    }
		}
	      if (found == 0)
		{
		  from = ini;		/* Set point to ini + 1.  */
		  goto leave;
		}
	    }
	  else if (code == Sendcomment)
	    {
#if 0
	      if (code != SYNTAX (c))
		/* For a two-char comment ender, we can assume
		   it does end a comment.  So scan back in a simple way.  */
		{
		  if (from != stop) DEC_POS (from);
		  while (1)
		    {
		      if ((c = FETCH_CHAR (from),
			   SYNTAX (c) == Scomment)
			  && SYNTAX_COMMENT_STYLE (c) == comstyle)
			break;
		      if (from == stop)
			{
			  immediate_quit = 0;
			  SET_PT (from);
			  return Qnil;
			}
		      DEC_POS (from);
		      if (SYNTAX_COMSTART_SECOND (c)
			  && (c1 = FETCH_CHAR (from),
			      SYNTAX_COMSTART_FIRST (c1))
			  && SYNTAX_COMMENT_STYLE (c) == comstyle
			  && !char_quoted (from))
			break;
		    }
		  break;
		}
#endif /* 0 */
	      found = back_comment (from, stop, comstyle);
	      if (found != -1) from = found;
#if 0
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
		int scanstart = from;

		DEC_POS (scanstart);
		/* At beginning of range to scan, we're outside of strings;
		   that determines quote parity to the comment-end.  */
		while (from != stop)
		  {
		    /* Move back and examine a character.  */
		    DEC_POS (from);

		    UPDATE_SYNTAX_TABLE_BACKWARD (from);
		    c = FETCH_CHAR (from);
		    code = SYNTAX (c);

		    /* If this char is the second of a 2-char comment sequence,
		       back up and give the pair the appropriate syntax.  */
		    temp_pos = from;
		    DEC_POS (temp_pos);
		    if (from > stop && SYNTAX_COMEND_SECOND (c)
			&& (c1 = FETCH_CHAR (temp_pos),
			    SYNTAX_COMEND_FIRST (c1)))
		      {
			code = Sendcomment;
			from = temp_pos;
		        c = c1;
		      }
			
		    temp_pos = from;
		    INC_POS (temp_pos);
		    /* If this char starts a 2-char comment start sequence,
		       treat it like a 1-char comment starter.  */
		    if (from < scanstart && SYNTAX_COMSTART_FIRST (c)
			&& (c1 = FETCH_CHAR (temp_pos),
			    SYNTAX_COMSTART_SECOND (c1))
			&& comstyle == SYNTAX_COMMENT_STYLE (c1))
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
			&& (from == stop || FETCH_BYTE (from - 1) == '\n'))
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
		      from = state.comstr_start;
		    else
		      /* We can't grok this as a comment; scan it normally.  */
		      from = comment_end;
		  }
	      }
#endif /* 0 */
	      /* We have skipped one comment.  */
	      break;
	    }
	  else if (code != Swhitespace && code != Scomment)
	    {
	    leave:
	      immediate_quit = 0;
	      INC_POS (from);
	      SET_PT (from);
	      return Qnil;
	    }
	}

      count1++;
    }

  SET_PT (from);
  immediate_quit = 0;
  return Qt;
}

Lisp_Object
scan_lists (from, count, depth, sexpflag)
     register int from;
     int count, depth, sexpflag;
{
  Lisp_Object val;
  register int stop = count > 0 ? ZV : BEGV;
  register int c, c1;
  int stringterm;
  int quoted;
  int mathexit = 0;
  register enum syntaxcode code, temp_code;
  int min_depth = depth;    /* Err out if depth gets less than this.  */
  int comstyle = 0;	    /* style of comment encountered */
  int temp_pos;
  int last_good = from;
  int found;

  if (depth > 0) min_depth = 0;

  immediate_quit = 1;
  QUIT;

  SETUP_SYNTAX_TABLE (from, count);
  while (count > 0)
    {
      while (from < stop)
	{
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  if (depth == min_depth)
	    last_good = from;
	  INC_POS (from);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (from < stop && SYNTAX_COMSTART_FIRST (c)
	      && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from))
	      && parse_sexp_ignore_comments)
	    {
	      /* we have encountered a comment start sequence and we 
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section */
	      code = Scomment;
	      comstyle = SYNTAX_COMMENT_STYLE (FETCH_CHAR (from));
	      INC_POS (from);
	    }
	  
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (SYNTAX_PREFIX (c))
	    continue;

	  switch (SWITCH_ENUM_CAST (code))
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      INC_POS (from);
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it.  */
	      while (from < stop)
		{
		  UPDATE_SYNTAX_TABLE_FORWARD (from);
		  switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		    {
		    case Scharquote:
		    case Sescape:
		      INC_POS (from);
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  INC_POS (from);
		}
	      goto done;

	    case Scomment:
	    case Scomment_fence:
	      if (!parse_sexp_ignore_comments) break;
	      while (1)
		{
		  if (from == stop)
		    {
		      if (depth == 0)
			goto done;
		      goto lose;
		    }
		  UPDATE_SYNTAX_TABLE_FORWARD (from);
		  c = FETCH_CHAR (from);
		  if (code == Scomment 
		      ? (SYNTAX (c) == Sendcomment
			 && SYNTAX_COMMENT_STYLE (c) == comstyle)
		      : (SYNTAX (c) == Scomment_fence))
		    /* we have encountered a comment end of the same style
		       as the comment sequence which began this comment
		       section */
		    break;
		  INC_POS (from);
		  if (from < stop && SYNTAX_COMEND_FIRST (c)
		      && SYNTAX_COMEND_SECOND (FETCH_CHAR (from))
		      && SYNTAX_COMMENT_STYLE (c) == comstyle
		      && code == Scomment)
		    /* we have encountered a comment end of the same style
		       as the comment sequence which began this comment
		       section */
		    { INC_POS (from); break; }
		}
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from))
		INC_POS (from);
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
		Fsignal (Qscan_error,
			 Fcons (build_string ("Containing expression ends prematurely"),
				Fcons (make_number (last_good),
				       Fcons (make_number (from), Qnil))));
	      break;

	    case Sstring:
	    case Sstring_fence:
	      temp_pos = from;
	      DEC_POS (temp_pos);
	      stringterm = FETCH_CHAR (temp_pos);
	      while (1)
		{
		  if (from >= stop) goto lose;
		  UPDATE_SYNTAX_TABLE_FORWARD (from);
		  if (code == Sstring 
		      ? (FETCH_CHAR (from) == stringterm)
		      : SYNTAX (FETCH_CHAR (from)) == Sstring_fence) 
		    break;
		  switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		    {
		    case Scharquote:
		    case Sescape:
		      INC_POS (from);
		    }
		  INC_POS (from);
		}
	      INC_POS (from);
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
      while (from > stop)
	{
	  DEC_POS (from);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  if (quoted = char_quoted (from))
	    {
	      DEC_POS (from);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from);
	    }
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  if (depth == min_depth)
	    last_good = from;
	  comstyle = 0;
	  if (code == Sendcomment)
	    comstyle = SYNTAX_COMMENT_STYLE (c);
	  temp_pos = from;
	  DEC_POS (temp_pos);
	  if (from > stop && SYNTAX_COMEND_SECOND (c)
	      && (c1 = FETCH_CHAR (temp_pos), SYNTAX_COMEND_FIRST (c1))
	      && !char_quoted (temp_pos)
	      && parse_sexp_ignore_comments)
	    {
	      /* we must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style */
	      code = Sendcomment;
	      comstyle = SYNTAX_COMMENT_STYLE (c1);
	      from = temp_pos;
	    }
	  
	  if (SYNTAX_PREFIX (c))
	    continue;

	  switch (SWITCH_ENUM_CAST (quoted ? Sword : code))
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished
		 after passing it.  */
	      while (from > stop)
		{
		  temp_pos = from;
		  DEC_POS (temp_pos);
		  UPDATE_SYNTAX_TABLE_BACKWARD (temp_pos);
		  quoted = char_quoted (temp_pos);
		  if (quoted)
		    {
		      from = temp_pos;
		      DEC_POS (temp_pos);
		      UPDATE_SYNTAX_TABLE_BACKWARD (temp_pos);
		    }
		  c1 = FETCH_CHAR (temp_pos);
		  temp_code = SYNTAX (c1);
		  if (! (quoted || temp_code == Sword
			 || temp_code == Ssymbol
			 || temp_code == Squote))
            	    goto done2;
		  from = temp_pos;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      temp_pos = from;
	      DEC_POS (temp_pos);
	      UPDATE_SYNTAX_TABLE_BACKWARD (temp_pos);
	      if (from != stop && c == FETCH_CHAR (temp_pos))
		from = temp_pos;
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
		Fsignal (Qscan_error,
			 Fcons (build_string ("Containing expression ends prematurely"),
				Fcons (make_number (last_good),
				       Fcons (make_number (from), Qnil))));
	      break;

	    case Sendcomment:
	      if (!parse_sexp_ignore_comments)
		break;
#if 0
	      if (code != SYNTAX (c))
		/* For a two-char comment ender, we can assume
		   it does end a comment.  So scan back in a simple way.  */
		{
		  if (from != stop) DEC_POS (from);
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
		      DEC_POS (from);
		      if (SYNTAX_COMSTART_SECOND (c)
			  && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from))
			  && SYNTAX_COMMENT_STYLE (c) == comstyle
			  && !char_quoted (from))
			break;
		    }
		  break;
		}
#endif /* 0 */
	      found = back_comment (from, stop, comstyle);
	      if (found != -1) from = found;
#if 0
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
		int scanstart = from;

		DEC_POS (scanstart);

		/* At beginning of range to scan, we're outside of strings;
		   that determines quote parity to the comment-end.  */
		while (from != stop)
		  {
		    /* Move back and examine a character.  */
		    DEC_POS (from);

		    c = FETCH_CHAR (from);
		    code = SYNTAX (c);

		    /* If this char is the second of a 2-char comment sequence,
		       back up and give the pair the appropriate syntax.  */
		    temp_pos = from;
		    DEC_POS (temp_pos);
		    if (from > stop && SYNTAX_COMEND_SECOND (c)
			&& (c1 = FETCH_CHAR (temp_pos),
			    SYNTAX_COMEND_FIRST (c1)))
		      {
			code = Sendcomment;
			from = temp_pos;
		        c = c1;
		      }
			
		    /* If this char starts a 2-char comment start sequence,
		       treat it like a 1-char comment starter.  */
		    temp_pos = from;
		    INC_POS (temp_pos);
		    if (from < scanstart && SYNTAX_COMSTART_FIRST (c)
			&& (c1 = FETCH_CHAR (temp_pos),
			    SYNTAX_COMSTART_SECOND (c1))
			&& comstyle == SYNTAX_COMMENT_STYLE (c1))
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
			&& (from == stop || FETCH_BYTE (from - 1) == '\n'))
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
		      from = state.comstr_start;
		    else
		      /* We can't grok this as a comment; scan it normally.  */
		      from = comment_end;
		  }
	      }
#endif /* 0 */
	      break;

	    case Scomment_fence:
	    case Sstring_fence:
	      while (1)
		{
		  DEC_POS (from);
		  if (from == stop) goto lose;
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  if (!char_quoted (from) 
		      && SYNTAX (FETCH_CHAR (from)) == code)
		    break;
		}
	      if (code == Sstring_fence && !depth && sexpflag) goto done2;
	      break;
	      
	    case Sstring:
	      stringterm = FETCH_CHAR (from);
	      while (1)
		{
		  if (from == stop) goto lose;
		  temp_pos = from;
		  DEC_POS (temp_pos);
		  UPDATE_SYNTAX_TABLE_BACKWARD (temp_pos);
		  if (!char_quoted (temp_pos)
		      && stringterm == FETCH_CHAR (temp_pos))
		    break;
		  from = temp_pos;
		}
	      DEC_POS (from);
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
  Fsignal (Qscan_error,
	   Fcons (build_string ("Unbalanced parentheses"),
		  Fcons (make_number (last_good),
			 Fcons (make_number (from), Qnil))));

  /* NOTREACHED */
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
  int pos = PT;
  int c;
  int temp_pos = pos;

  if (pos > beg) 
    {
      SETUP_SYNTAX_TABLE (pos, -1);
    }
  DEC_POS (temp_pos);

  while (pos > beg && !char_quoted (temp_pos)
	 /* Previous statement updates syntax table.  */
	 && ((c = FETCH_CHAR (temp_pos), SYNTAX (c) == Squote)
	     || SYNTAX_PREFIX (c)))
    {
      pos = temp_pos;
      DEC_POS (temp_pos);
    }

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
  int prev;
  register int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  Lisp_Object tem;
  int prev_from;		/* Keep one character before FROM.  */
  int boundary_stop = commentstop == -1;
  int nofence;

  prev_from = from;
  DEC_POS (prev_from);

  /* Use this macro instead of `from++'.  */
#define INC_FROM do { prev_from = from; INC_POS (from); } while (0)

  immediate_quit = 1;
  QUIT;

  SETUP_SYNTAX_TABLE (from, 1);

  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default.  */
      state.comstr_start = -1;	/* no comment/string seen.  */
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
      /* Check whether we are inside string_fence-style string: */
      state.instring = ( !NILP (tem) 
			 ? ( INTEGERP (tem) ? XINT (tem) : ST_STRING_STYLE) 
			 : -1);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.incomment = !NILP (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      start_quoted = !NILP (tem);

      /* if the eight element of the list is nil, we are in comment
	 style a.  If it is non-nil, we are in comment style b */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.comstyle = NILP (tem) ? 0 : ( EQ (tem, Qsyntax_table) 
					  ? ST_COMMENT_STYLE : 1 );

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.comstr_start = NILP (tem) ? -1 : XINT (tem) ;
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state.  */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      nofence = state.instring != ST_STRING_STYLE;
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      UPDATE_SYNTAX_TABLE_FORWARD (from);
      code = SYNTAX (FETCH_CHAR (from));
      INC_FROM;

      if (code == Scomment)
	state.comstr_start = prev_from;
      else if (code == Scomment_fence)
	{
	  /* Record the comment style we have entered so that only
	     the comment-end sequence of the same style actually
	     terminates the comment section.  */
	  state.comstyle = ( code == Scomment_fence 
			     ? ST_COMMENT_STYLE 
			     : SYNTAX_COMMENT_STYLE (FETCH_CHAR (from)));
	  state.comstr_start = prev_from;
	  if (code != Scomment_fence) INC_FROM;
	  code = Scomment;
	}
     else if (from < end)
	if (SYNTAX_COMSTART_FIRST (FETCH_CHAR (prev_from)))
	  if (SYNTAX_COMSTART_SECOND (FETCH_CHAR (from)))
	    /* Duplicate code to avoid a very complex if-expression
	       which causes trouble for the SGI compiler.  */
	    {
	      /* Record the comment style we have entered so that only
		 the comment-end sequence of the same style actually
		 terminates the comment section.  */
	      state.comstyle = ( code == Scomment_fence 
				 ? ST_COMMENT_STYLE 
				 : SYNTAX_COMMENT_STYLE (FETCH_CHAR (from)));
	      state.comstr_start = prev_from;
	      if (code != Scomment_fence) INC_FROM;
	      code = Scomment;
	    }

      if (SYNTAX_PREFIX (FETCH_CHAR (prev_from)))
	continue;
      switch (SWITCH_ENUM_CAST (code))
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	startquoted:
	  if (from == end) goto endquoted;
	  INC_FROM;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	symstarted:
	  while (from < end)
	    {
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      switch (SWITCH_ENUM_CAST (SYNTAX (FETCH_CHAR (from))))
		{
		case Scharquote:
		case Sescape:
		  INC_FROM;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      INC_FROM;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	startincomment:
	  if (commentstop == 1)
	    goto done;
	  if (from != BEGV)
	    {
	      /* Enter the loop in the middle so that we find
		 a 2-char comment ender if we start in the middle of it.  */
	      prev = FETCH_CHAR (prev_from);
	      goto startincomment_1;
	    }
	  /* At beginning of buffer, enter the loop the ordinary way.  */
	  state.incomment = 1;
	  goto commentloop;

	case Scomment:
	  state.incomment = 1;
	  if (commentstop || boundary_stop) goto done;
	commentloop:
	  while (1)
	    {
	      if (from == end) goto done;
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      prev = FETCH_CHAR (from);
	      if (SYNTAX (prev) == Sendcomment
		  && SYNTAX_COMMENT_STYLE (prev) == state.comstyle)
		/* Only terminate the comment section if the endcomment
		   of the same style as the start sequence has been
		   encountered.  */
		break;
	      if (state.comstyle == ST_COMMENT_STYLE
		  && SYNTAX (prev) == Scomment_fence) 
		break;
	      INC_FROM;
	    startincomment_1:
	      if (from < end && SYNTAX_COMEND_FIRST (prev)
		  && SYNTAX_COMEND_SECOND (FETCH_CHAR (from))
		  && SYNTAX_COMMENT_STYLE (prev) == state.comstyle)
		/* Only terminate the comment section if the end-comment
		   sequence of the same style as the start sequence has
		   been encountered.  */
		{ break; }
	    }
	  INC_FROM; 
	  state.incomment = 0;
	  state.comstyle = 0;	/* reset the comment style */
	  if (boundary_stop) goto done;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = prev_from;
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (targetdepth == depth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (targetdepth == depth) goto done;
	  break;

	case Sstring:
	case Sstring_fence:
	  state.comstr_start = from - 1;
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	  state.instring = (code == Sstring 
			    ? (FETCH_CHAR (prev_from))
			    : ST_STRING_STYLE);
	  if (boundary_stop) goto done;
	startinstring:
	  {
	      nofence = state.instring != ST_STRING_STYLE;
	    
	      while (1)
		  {
		      int c;

		      if (from >= end) goto done;
		      c = FETCH_CHAR (from);
		      if (nofence && c == state.instring) break;
		      UPDATE_SYNTAX_TABLE_FORWARD (from);
		      switch (SWITCH_ENUM_CAST (SYNTAX (c)))
			  {
			  case Sstring_fence:
			      if (!nofence) goto string_end;
			      break;
			  case Scharquote:
			  case Sescape:
			      INC_FROM;
			    startquotedinstring:
			      if (from >= end) goto endquoted;
			  }
		      INC_FROM;
		  }
	  }
	string_end:
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  INC_FROM;
	  if (boundary_stop) goto done;
	  break;

	case Smath:
	  break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from = prev_from;    /* We have just fetched the char that starts it; */
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
Value is a list of nine elements describing final state of parsing:\n\
 0. depth in parens.\n\
 1. character address of start of innermost containing list; nil if none.\n\
 2. character address of start of last complete sexp terminated.\n\
 3. non-nil if inside a string.\n\
    (it is the character that will terminate the string,\n\
     or t if the string should be terminated by an explicit\n\
     `syntax-table' property.)\n\
 4. t if inside a comment.\n\
 5. t if following a quote character.\n\
 6. the minimum paren-depth encountered during this scan.\n\
 7. t if in a comment of style `b'; `syntax-table' if given by an explicit\n\
     `syntax-table' property.\n\
 8. character address of start of last comment or string; nil if none.\n\
If third arg TARGETDEPTH is non-nil, parsing stops if the depth\n\
in parentheses becomes equal to TARGETDEPTH.\n\
Fourth arg STOPBEFORE non-nil means stop when come to\n\
 any character that starts a sexp.\n\
Fifth arg STATE is an eight-list like what this function returns.\n\
It is used to initialize the state of the parse.  Its second and third
elements are ignored.
Sixth arg COMMENTSTOP non-nil means stop at the start of a comment.  If\n\
it is `syntax-table', stop after the start of a comment or a string, or\n\
after end of a comment or a string.")
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
		      (NILP (commentstop) 
		       ? 0 : (EQ (commentstop, Qsyntax_table) ? -1 : 1)));

  SET_PT (state.location);
  
  return Fcons (make_number (state.depth),
	   Fcons (state.prevlevelstart < 0 ? Qnil : make_number (state.prevlevelstart),
	     Fcons (state.thislevelstart < 0 ? Qnil : make_number (state.thislevelstart),
	       Fcons (state.instring >= 0 
		      ? (state.instring == ST_STRING_STYLE 
			 ? Qt : make_number (state.instring)) : Qnil,
		 Fcons (state.incomment ? Qt : Qnil,
		   Fcons (state.quoted ? Qt : Qnil,
			  Fcons (make_number (state.mindepth),
				 Fcons (state.comstyle 
					? (state.comstyle == ST_COMMENT_STYLE
					   ? Qsyntax_table : Qt) : Qnil,
					Fcons (state.comstr_start != -1 ? make_number (state.comstr_start) : Qnil,
					       Qnil)))))))));
}

init_syntax_once ()
{
  register int i, c;
  Lisp_Object temp;

  /* This has to be done here, before we call Fmake_char_table.  */
  Qsyntax_table = intern ("syntax-table");
  staticpro (&Qsyntax_table);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  /* Create objects which can be shared among syntax tables.  */
  Vsyntax_code_object = Fmake_vector (make_number (13), Qnil);
  for (i = 0; i < XVECTOR (Vsyntax_code_object)->size; i++)
    XVECTOR (Vsyntax_code_object)->contents[i]
      = Fcons (make_number (i), Qnil);

  /* Now we are ready to set up this property, so we can
     create syntax tables.  */
  Fput (Qsyntax_table, Qchar_table_extra_slots, make_number (0));

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Swhitespace];

  Vstandard_syntax_table = Fmake_char_table (Qsyntax_table, temp);

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Sword];
  for (i = 'a'; i <= 'z'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  for (i = 'A'; i <= 'Z'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  for (i = '0'; i <= '9'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);

  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '$', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '%', temp);

  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '(',
			Fcons (make_number (Sopen), make_number (')')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ')',
			Fcons (make_number (Sclose), make_number ('(')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '[',
			Fcons (make_number (Sopen), make_number (']')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ']',
			Fcons (make_number (Sclose), make_number ('[')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '{',
			Fcons (make_number (Sopen), make_number ('}')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '}',
			Fcons (make_number (Sclose), make_number ('{')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '"',
			Fcons (make_number ((int) Sstring), Qnil));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\\',
			Fcons (make_number ((int) Sescape), Qnil));

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Ssymbol];
  for (i = 0; i < 10; i++)
    {
      c = "_-+*/&|<>="[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Spunct];
  for (i = 0; i < 12; i++)
    {
      c = ".,;:?!#@~^'`"[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }
}

syms_of_syntax ()
{
  Qsyntax_table_p = intern ("syntax-table-p");
  staticpro (&Qsyntax_table_p);

  staticpro (&Vsyntax_code_object);

  Qscan_error = intern ("scan-error");
  staticpro (&Qscan_error);
  Fput (Qscan_error, Qerror_conditions,
	Fcons (Qerror, Qnil));
  Fput (Qscan_error, Qerror_message,
	build_string ("Scan error"));

  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments,
    "Non-nil means `forward-sexp', etc., should treat comments as whitespace.");

  DEFVAR_BOOL ("parse-sexp-lookup-properties", &parse_sexp_lookup_properties,
    "Non-nil means `forward-sexp', etc., grant `syntax-table' property.\n\
The value of this property should be either a syntax table, or a cons\n\
of the form (SYNTAXCODE . MATCHCHAR), SYNTAXCODE being the numeric\n\
syntax code, MATCHCHAR being nil or the character to match (which is\n\
relevant only for open/close type.");

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

  defsubr (&Sskip_chars_forward);
  defsubr (&Sskip_chars_backward);
  defsubr (&Sskip_syntax_forward);
  defsubr (&Sskip_syntax_backward);

  defsubr (&Sforward_comment);
  defsubr (&Sscan_lists);
  defsubr (&Sscan_sexps);
  defsubr (&Sbackward_prefix_chars);
  defsubr (&Sparse_partial_sexp);
}
