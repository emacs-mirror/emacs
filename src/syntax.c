/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985, 1987, 1993-1995, 1997-1999, 2001-2018 Free
   Software Foundation, Inc.

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


#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "regex.h"
#include "syntax.h"
#include "intervals.h"
#include "category.h"

/* Make syntax table lookup grant data in gl_state.  */
#define SYNTAX(c) syntax_property (c, 1)
#define SYNTAX_ENTRY(c) syntax_property_entry (c, 1)
#define SYNTAX_WITH_FLAGS(c) syntax_property_with_flags (c, 1)

/* Eight single-bit flags have the following meanings:
  1. This character is the first of a two-character comment-start sequence.
  2. This character is the second of a two-character comment-start sequence.
  3. This character is the first of a two-character comment-end sequence.
  4. This character is the second of a two-character comment-end sequence.
  5. This character is a prefix, for backward-prefix-chars.
  6. The char is part of a delimiter for comments of style "b".
  7. This character is part of a nestable comment sequence.
  8. The char is part of a delimiter for comments of style "c".
  Note that any two-character sequence whose first character has flag 1
  and whose second character has flag 2 will be interpreted as a comment start.

  Bits 6 and 8 discriminate among different comment styles.
  Languages such as C++ allow two orthogonal syntax start/end pairs
  and bit 6 determines whether a comment-end or Scommentend
  ends style a or b.  Comment markers can start style a, b, c, or bc.
  Style a is always the default.
  For 2-char comment markers, the style b flag is looked up only on the second
  char of the comment marker and on the first char of the comment ender.
  For style c (like the nested flag), the flag can be placed on any of
  the chars.  */

/* These functions extract specific flags from an integer
   that holds the syntax code and the flags.  */

static bool
SYNTAX_FLAGS_COMSTART_FIRST (int flags)
{
  return (flags >> 16) & 1;
}
static bool
SYNTAX_FLAGS_COMSTART_SECOND (int flags)
{
  return (flags >> 17) & 1;
}
static bool
SYNTAX_FLAGS_COMEND_FIRST (int flags)
{
  return (flags >> 18) & 1;
}
static bool
SYNTAX_FLAGS_COMEND_SECOND (int flags)
{
  return (flags >> 19) & 1;
}
static bool
SYNTAX_FLAGS_COMSTARTEND_FIRST (int flags)
{
  return (flags & 0x50000) != 0;
}
static bool
SYNTAX_FLAGS_PREFIX (int flags)
{
  return (flags >> 20) & 1;
}
static bool
SYNTAX_FLAGS_COMMENT_STYLEB (int flags)
{
  return (flags >> 21) & 1;
}
static bool
SYNTAX_FLAGS_COMMENT_STYLEC (int flags)
{
  return (flags >> 23) & 1;
}
static int
SYNTAX_FLAGS_COMMENT_STYLEC2 (int flags)
{
  return (flags >> 22) & 2; /* SYNTAX_FLAGS_COMMENT_STYLEC (flags) * 2 */
}
static bool
SYNTAX_FLAGS_COMMENT_NESTED (int flags)
{
  return (flags >> 22) & 1;
}

/* FLAGS should be the flags of the main char of the comment marker, e.g.
   the second for comstart and the first for comend.  */
static int
SYNTAX_FLAGS_COMMENT_STYLE (int flags, int other_flags)
{
  return (SYNTAX_FLAGS_COMMENT_STYLEB (flags)
	  | SYNTAX_FLAGS_COMMENT_STYLEC2 (flags)
	  | SYNTAX_FLAGS_COMMENT_STYLEC2 (other_flags));
}

/* Extract a particular flag for a given character.  */

static bool
SYNTAX_COMEND_FIRST (int c)
{
  return SYNTAX_FLAGS_COMEND_FIRST (SYNTAX_WITH_FLAGS (c));
}

/* We use these constants in place for comment-style and
   string-ender-char to distinguish comments/strings started by
   comment_fence and string_fence codes.  */

enum
  {
    ST_COMMENT_STYLE = 256 + 1,
    ST_STRING_STYLE = 256 + 2
  };

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
  {
    EMACS_INT depth;	/* Depth at end of parsing.  */
    int instring;  /* -1 if not within string, else desired terminator.  */
    EMACS_INT incomment; /* -1 if in unnestable comment else comment nesting */
    int comstyle;  /* comment style a=0, or b=1, or ST_COMMENT_STYLE.  */
    bool quoted;   /* True if just after an escape char at end of parsing.  */
    EMACS_INT mindepth;	/* Minimum depth seen while scanning.  */
    /* Char number of most recent start-of-expression at current level */
    ptrdiff_t thislevelstart;
    /* Char number of start of containing expression */
    ptrdiff_t prevlevelstart;
    ptrdiff_t location;	     /* Char number at which parsing stopped.  */
    ptrdiff_t location_byte; /* Corresponding byte position.  */
    ptrdiff_t comstr_start;  /* Position of last comment/string starter.  */
    Lisp_Object levelstarts; /* Char numbers of starts-of-expression
				of levels (starting from outermost).  */
    int prev_syntax; /* Syntax of previous position scanned, when
                        that position (potentially) holds the first char
                        of a 2-char construct, i.e. comment delimiter
                        or Sescape, etc.  Smax otherwise. */
  };

/* These variables are a cache for finding the start of a defun.
   find_start_pos is the place for which the defun start was found.
   find_start_value is the defun start position found for it.
   find_start_value_byte is the corresponding byte position.
   find_start_buffer is the buffer it was found in.
   find_start_begv is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static ptrdiff_t find_start_pos;
static ptrdiff_t find_start_value;
static ptrdiff_t find_start_value_byte;
static struct buffer *find_start_buffer;
static ptrdiff_t find_start_begv;
static EMACS_INT find_start_modiff;


static Lisp_Object skip_chars (bool, Lisp_Object, Lisp_Object, bool);
static Lisp_Object skip_syntaxes (bool, Lisp_Object, Lisp_Object);
static Lisp_Object scan_lists (EMACS_INT, EMACS_INT, EMACS_INT, bool);
static void scan_sexps_forward (struct lisp_parse_state *,
                                ptrdiff_t, ptrdiff_t, ptrdiff_t, EMACS_INT,
                                bool, int);
static void internalize_parse_state (Lisp_Object, struct lisp_parse_state *);
static bool in_classes (int, Lisp_Object);
static void parse_sexp_propertize (ptrdiff_t charpos);

/* This setter is used only in this file, so it can be private.  */
static void
bset_syntax_table (struct buffer *b, Lisp_Object val)
{
  b->syntax_table_ = val;
}

/* Whether the syntax of the character C has the prefix flag set.  */
bool
syntax_prefix_flag_p (int c)
{
  return SYNTAX_FLAGS_PREFIX (SYNTAX_WITH_FLAGS (c));
}

struct gl_state_s gl_state;		/* Global state of syntax parser.  */

enum { INTERVALS_AT_ONCE = 10 };	/* 1 + max-number of intervals
					   to scan to property-change.  */

/* Set the syntax entry VAL for char C in table TABLE.  */

static void
SET_RAW_SYNTAX_ENTRY (Lisp_Object table, int c, Lisp_Object val)
{
  CHAR_TABLE_SET (table, c, val);
}

/* Set the syntax entry VAL for char-range RANGE in table TABLE.
   RANGE is a cons (FROM . TO) specifying the range of characters.  */

static void
SET_RAW_SYNTAX_ENTRY_RANGE (Lisp_Object table, Lisp_Object range,
			    Lisp_Object val)
{
  Fset_char_table_range (table, range, val);
}

/* Extract the information from the entry for character C
   in the current syntax table.  */

static Lisp_Object
SYNTAX_MATCH (int c)
{
  Lisp_Object ent = SYNTAX_ENTRY (c);
  return CONSP (ent) ? XCDR (ent) : Qnil;
}

/* This should be called with FROM at the start of forward
   search, or after the last position of the backward search.  It
   makes sure that the first char is picked up with correct table, so
   one does not need to call UPDATE_SYNTAX_TABLE immediately after the
   call.
   Sign of COUNT gives the direction of the search.
 */

static void
SETUP_SYNTAX_TABLE (ptrdiff_t from, ptrdiff_t count)
{
  SETUP_BUFFER_SYNTAX_TABLE ();
  gl_state.b_property = BEGV;
  gl_state.e_property = ZV + 1;
  gl_state.object = Qnil;
  gl_state.offset = 0;
  if (parse_sexp_lookup_properties)
    {
      if (count > 0)
	update_syntax_table_forward (from, true, Qnil);
      else if (from > BEGV)
	{
	  update_syntax_table (from - 1, count, true, Qnil);
	  parse_sexp_propertize (from - 1);
	}
    }
}

/* Same as above, but in OBJECT.  If OBJECT is nil, use current buffer.
   If it is t (which is only used in fast_c_string_match_ignore_case),
   ignore properties altogether.

   This is meant for regex.c to use.  For buffers, regex.c passes arguments
   to the UPDATE_SYNTAX_TABLE functions which are relative to BEGV.
   So if it is a buffer, we set the offset field to BEGV.  */

void
SETUP_SYNTAX_TABLE_FOR_OBJECT (Lisp_Object object,
			       ptrdiff_t from, ptrdiff_t count)
{
  SETUP_BUFFER_SYNTAX_TABLE ();
  gl_state.object = object;
  if (BUFFERP (gl_state.object))
    {
      struct buffer *buf = XBUFFER (gl_state.object);
      gl_state.b_property = 1;
      gl_state.e_property = BUF_ZV (buf) - BUF_BEGV (buf) + 1;
      gl_state.offset = BUF_BEGV (buf) - 1;
    }
  else if (NILP (gl_state.object))
    {
      gl_state.b_property = 1;
      gl_state.e_property = ZV - BEGV + 1;
      gl_state.offset = BEGV - 1;
    }
  else if (EQ (gl_state.object, Qt))
    {
      gl_state.b_property = 0;
      gl_state.e_property = PTRDIFF_MAX;
      gl_state.offset = 0;
    }
  else
    {
      gl_state.b_property = 0;
      gl_state.e_property = 1 + SCHARS (gl_state.object);
      gl_state.offset = 0;
    }
  if (parse_sexp_lookup_properties)
    update_syntax_table (from + gl_state.offset - (count <= 0),
			 count, 1, gl_state.object);
}

/* Update gl_state to an appropriate interval which contains CHARPOS.  The
   sign of COUNT give the relative position of CHARPOS wrt the previously
   valid interval.  If INIT, only [be]_property fields of gl_state are
   valid at start, the rest is filled basing on OBJECT.

   `gl_state.*_i' are the intervals, and CHARPOS is further in the search
   direction than the intervals - or in an interval.  We update the
   current syntax-table basing on the property of this interval, and
   update the interval to start further than CHARPOS - or be
   NULL.  We also update lim_property to be the next value of
   charpos to call this subroutine again - or be before/after the
   start/end of OBJECT.  */

void
update_syntax_table (ptrdiff_t charpos, EMACS_INT count, bool init,
		     Lisp_Object object)
{
  Lisp_Object tmp_table;
  int cnt = 0;
  bool invalidate = true;
  INTERVAL i;

  if (init)
    {
      gl_state.old_prop = Qnil;
      gl_state.start = gl_state.b_property;
      gl_state.stop = gl_state.e_property;
      i = interval_of (charpos, object);
      gl_state.backward_i = gl_state.forward_i = i;
      invalidate = false;
      if (!i)
	return;
      /* interval_of updates only ->position of the return value, so
	 update the parents manually to speed up update_interval.  */
      while (!NULL_PARENT (i))
	{
	  if (AM_RIGHT_CHILD (i))
	    INTERVAL_PARENT (i)->position = i->position
	      - LEFT_TOTAL_LENGTH (i) + TOTAL_LENGTH (i) /* right end */
	      - TOTAL_LENGTH (INTERVAL_PARENT (i))
	      + LEFT_TOTAL_LENGTH (INTERVAL_PARENT (i));
	  else
	    INTERVAL_PARENT (i)->position = i->position - LEFT_TOTAL_LENGTH (i)
	      + TOTAL_LENGTH (i);
	  i = INTERVAL_PARENT (i);
	}
      i = gl_state.forward_i;
      gl_state.b_property = i->position - gl_state.offset;
      gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
      goto update;
    }
  i = count > 0 ? gl_state.forward_i : gl_state.backward_i;

  /* We are guaranteed to be called with CHARPOS either in i,
     or further off.  */
  if (!i)
    error ("Error in syntax_table logic for to-the-end intervals");
  else if (charpos < i->position)		/* Move left.  */
    {
      if (count > 0)
	error ("Error in syntax_table logic for intervals <-");
      /* Update the interval.  */
      i = update_interval (i, charpos);
      if (INTERVAL_LAST_POS (i) != gl_state.b_property)
	{
	  invalidate = false;
	  gl_state.forward_i = i;
	  gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
	}
    }
  else if (charpos >= INTERVAL_LAST_POS (i)) /* Move right.  */
    {
      if (count < 0)
	error ("Error in syntax_table logic for intervals ->");
      /* Update the interval.  */
      i = update_interval (i, charpos);
      if (i->position != gl_state.e_property)
	{
	  invalidate = false;
	  gl_state.backward_i = i;
	  gl_state.b_property = i->position - gl_state.offset;
	}
    }

  update:
  tmp_table = textget (i->plist, Qsyntax_table);

  if (invalidate)
    invalidate = !EQ (tmp_table, gl_state.old_prop); /* Need to invalidate? */

  if (invalidate)		/* Did not get to adjacent interval.  */
    {				/* with the same table => */
				/* invalidate the old range.  */
      if (count > 0)
	{
	  gl_state.backward_i = i;
	  gl_state.b_property = i->position - gl_state.offset;
	}
      else
	{
	  gl_state.forward_i = i;
	  gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
	}
    }

  if (!EQ (tmp_table, gl_state.old_prop))
    {
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
	  gl_state.current_syntax_table = BVAR (current_buffer, syntax_table);
	}
    }

  while (i)
    {
      if (cnt && !EQ (tmp_table, textget (i->plist, Qsyntax_table)))
	{
	  if (count > 0)
	    {
	      gl_state.e_property = i->position - gl_state.offset;
	      gl_state.forward_i = i;
	    }
	  else
	    {
	      gl_state.b_property
		= i->position + LENGTH (i) - gl_state.offset;
	      gl_state.backward_i = i;
	    }
	  return;
	}
      else if (cnt == INTERVALS_AT_ONCE)
	{
	  if (count > 0)
	    {
	      gl_state.e_property
		= i->position + LENGTH (i) - gl_state.offset
		/* e_property at EOB is not set to ZV but to ZV+1, so that
		   we can do INC(from);UPDATE_SYNTAX_TABLE_FORWARD without
		   having to check eob between the two.  */
		+ (next_interval (i) ? 0 : 1);
	      gl_state.forward_i = i;
	    }
	  else
	    {
	      gl_state.b_property = i->position - gl_state.offset;
	      gl_state.backward_i = i;
	    }
	  return;
	}
      cnt++;
      i = count > 0 ? next_interval (i) : previous_interval (i);
    }
  eassert (i == NULL); /* This property goes to the end.  */
  if (count > 0)
    {
      gl_state.e_property = gl_state.stop;
      gl_state.forward_i = i;
    }
  else
    gl_state.b_property = gl_state.start;
}

static void
parse_sexp_propertize (ptrdiff_t charpos)
{
  EMACS_INT zv = ZV;
  if (syntax_propertize__done <= charpos
      && syntax_propertize__done < zv)
    {
      EMACS_INT modiffs = CHARS_MODIFF;
      safe_call1 (Qinternal__syntax_propertize,
		  make_number (min (zv, 1 + charpos)));
      if (modiffs != CHARS_MODIFF)
	error ("parse-sexp-propertize-function modified the buffer!");
      if (syntax_propertize__done <= charpos
	  && syntax_propertize__done < zv)
	error ("parse-sexp-propertize-function did not move"
	       " syntax-propertize--done");
      SETUP_SYNTAX_TABLE (charpos, 1);
    }
  else if (gl_state.e_property > syntax_propertize__done)
    {
      gl_state.e_property = syntax_propertize__done;
      gl_state.e_property_truncated = true;
    }
  else if (gl_state.e_property_truncated
	   && gl_state.e_property < syntax_propertize__done)
    { /* When moving backward, e_property might be set without resetting
	 e_property_truncated, so the e_property_truncated flag may
	 occasionally be left raised spuriously.  This should be rare.  */
      gl_state.e_property_truncated = false;
      update_syntax_table_forward (charpos, false, Qnil);
    }
}

void
update_syntax_table_forward (ptrdiff_t charpos, bool init,
			     Lisp_Object object)
{
  if (gl_state.e_property_truncated)
    {
      eassert (NILP (object));
      eassert (charpos >= gl_state.e_property);
      parse_sexp_propertize (charpos);
    }
  else
    {
      update_syntax_table (charpos, 1, init, object);
      if (NILP (object) && gl_state.e_property > syntax_propertize__done)
	parse_sexp_propertize (charpos);
    }
}

/* Returns true if char at CHARPOS is quoted.
   Global syntax-table data should be set up already to be good at CHARPOS
   or after.  On return global syntax data is good for lookup at CHARPOS.  */

static bool
char_quoted (ptrdiff_t charpos, ptrdiff_t bytepos)
{
  enum syntaxcode code;
  ptrdiff_t beg = BEGV;
  bool quoted = 0;
  ptrdiff_t orig = charpos;

  while (charpos > beg)
    {
      int c;
      DEC_BOTH (charpos, bytepos);

      UPDATE_SYNTAX_TABLE_BACKWARD (charpos);
      c = FETCH_CHAR_AS_MULTIBYTE (bytepos);
      code = SYNTAX (c);
      if (! (code == Scharquote || code == Sescape))
	break;

      quoted = !quoted;
    }

  UPDATE_SYNTAX_TABLE (orig);
  return quoted;
}

/* Return the bytepos one character before BYTEPOS.
   We assume that BYTEPOS is not at the start of the buffer.  */

static ptrdiff_t
dec_bytepos (ptrdiff_t bytepos)
{
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return bytepos - 1;

  DEC_POS (bytepos);
  return bytepos;
}

/* Return a defun-start position before POS and not too far before.
   It should be the last one before POS, or nearly the last.

   When open_paren_in_column_0_is_defun_start is nonzero,
   only the beginning of the buffer is treated as a defun-start.

   We record the information about where the scan started
   and what its result was, so that another call in the same area
   can return the same value very quickly.

   There is no promise at which position the global syntax data is
   valid on return from the subroutine, so the caller should explicitly
   update the global data.  */

static ptrdiff_t
find_defun_start (ptrdiff_t pos, ptrdiff_t pos_byte)
{
  ptrdiff_t opoint = PT, opoint_byte = PT_BYTE;

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

  if (!NILP (Vcomment_use_syntax_ppss))
    {
      EMACS_INT modiffs = CHARS_MODIFF;
      Lisp_Object ppss = call1 (Qsyntax_ppss, make_number (pos));
      if (modiffs != CHARS_MODIFF)
	error ("syntax-ppss modified the buffer!");
      TEMP_SET_PT_BOTH (opoint, opoint_byte);
      Lisp_Object boc = Fnth (make_number (8), ppss);
      if (NUMBERP (boc))
        {
          find_start_value = XINT (boc);
          find_start_value_byte = CHAR_TO_BYTE (find_start_value);
        }
      else
        {
          find_start_value = pos;
          find_start_value_byte = pos_byte;
        }
      goto found;
    }
  if (!open_paren_in_column_0_is_defun_start)
    {
      find_start_value = BEGV;
      find_start_value_byte = BEGV_BYTE;
      goto found;
    }

  /* Back up to start of line.  */
  scan_newline (pos, pos_byte, BEGV, BEGV_BYTE, -1, 1);

  /* We optimize syntax-table lookup for rare updates.  Thus we accept
     only those `^\s(' which are good in global _and_ text-property
     syntax-tables.  */
  SETUP_BUFFER_SYNTAX_TABLE ();
  while (PT > BEGV)
    {
      /* Open-paren at start of line means we may have found our
	 defun-start.  */
      int c = FETCH_CHAR_AS_MULTIBYTE (PT_BYTE);
      if (SYNTAX (c) == Sopen)
	{
	  SETUP_SYNTAX_TABLE (PT + 1, -1);	/* Try again... */
	  c = FETCH_CHAR_AS_MULTIBYTE (PT_BYTE);
	  if (SYNTAX (c) == Sopen)
	    break;
	  /* Now fallback to the default value.  */
	  SETUP_BUFFER_SYNTAX_TABLE ();
	}
      /* Move to beg of previous line.  */
      scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, -2, 1);
    }

  /* Record what we found, for the next try.  */
  find_start_value = PT;
  find_start_value_byte = PT_BYTE;
  TEMP_SET_PT_BOTH (opoint, opoint_byte);

 found:
  find_start_buffer = current_buffer;
  find_start_modiff = MODIFF;
  find_start_begv = BEGV;
  find_start_pos = pos;

  return find_start_value;
}

/* Return the SYNTAX_COMEND_FIRST of the character before POS, POS_BYTE.  */

static bool
prev_char_comend_first (ptrdiff_t pos, ptrdiff_t pos_byte)
{
  int c;
  bool val;

  DEC_BOTH (pos, pos_byte);
  UPDATE_SYNTAX_TABLE_BACKWARD (pos);
  c = FETCH_CHAR (pos_byte);
  val = SYNTAX_COMEND_FIRST (c);
  UPDATE_SYNTAX_TABLE_FORWARD (pos + 1);
  return val;
}

/* Check whether charpos FROM is at the end of a comment.
   FROM_BYTE is the bytepos corresponding to FROM.
   Do not move back before STOP.

   Return true if we find a comment ending at FROM/FROM_BYTE.

   If successful, store the charpos of the comment's beginning
   into *CHARPOS_PTR, and the bytepos into *BYTEPOS_PTR.

   Global syntax data remains valid for backward search starting at
   the returned value (or at FROM, if the search was not successful).  */

static bool
back_comment (ptrdiff_t from, ptrdiff_t from_byte, ptrdiff_t stop,
	      bool comnested, int comstyle, ptrdiff_t *charpos_ptr,
	      ptrdiff_t *bytepos_ptr)
{
  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.

     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int string_style = -1;	/* Presumed outside of any string.  */
  bool string_lossage = 0;
  /* Not a real lossage: indicates that we have passed a matching comment
     starter plus a non-matching comment-ender, meaning that any matching
     comment-starter we might see later could be a false positive (hidden
     inside another comment).
     Test case:  { a (* b } c (* d *) */
  bool comment_lossage = 0;
  ptrdiff_t comment_end = from;
  ptrdiff_t comment_end_byte = from_byte;
  ptrdiff_t comstart_pos = 0;
  ptrdiff_t comstart_byte;
  /* Place where the containing defun starts,
     or 0 if we didn't come across it yet.  */
  ptrdiff_t defun_start = 0;
  ptrdiff_t defun_start_byte = 0;
  enum syntaxcode code;
  ptrdiff_t nesting = 1;		/* Current comment nesting.  */
  int c;
  int syntax = 0;
  unsigned short int quit_count = 0;

  /* FIXME: A }} comment-ender style leads to incorrect behavior
     in the case of {{ c }}} because we ignore the last two chars which are
     assumed to be comment-enders although they aren't.  */

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      rarely_quit (++quit_count);

      ptrdiff_t temp_byte;
      int prev_syntax;
      bool com2start, com2end, comstart;

      /* Move back and examine a character.  */
      DEC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_BACKWARD (from);

      prev_syntax = syntax;
      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
      syntax = SYNTAX_WITH_FLAGS (c);
      code = SYNTAX (c);

      /* Check for 2-char comment markers.  */
      com2start = (SYNTAX_FLAGS_COMSTART_FIRST (syntax)
		   && SYNTAX_FLAGS_COMSTART_SECOND (prev_syntax)
		   && (comstyle
		       == SYNTAX_FLAGS_COMMENT_STYLE (prev_syntax, syntax))
		   && (SYNTAX_FLAGS_COMMENT_NESTED (prev_syntax)
		       || SYNTAX_FLAGS_COMMENT_NESTED (syntax)) == comnested);
      com2end = (SYNTAX_FLAGS_COMEND_FIRST (syntax)
		 && SYNTAX_FLAGS_COMEND_SECOND (prev_syntax));
      comstart = (com2start || code == Scomment);

      /* Nasty cases with overlapping 2-char comment markers:
	 - snmp-mode: -- c -- foo -- c --
	              --- c --
		      ------ c --
	 - c-mode:    *||*
		      |* *|* *|
		      |*| |* |*|
		      ///   */

      /* If a 2-char comment sequence partly overlaps with another,
	 we don't try to be clever.  E.g. |*| in C, or }% in modes that
	 have %..\n and %{..}%.  */
      if (from > stop && (com2end || comstart))
	{
	  ptrdiff_t next = from, next_byte = from_byte;
	  int next_c, next_syntax;
	  DEC_BOTH (next, next_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (next);
	  next_c = FETCH_CHAR_AS_MULTIBYTE (next_byte);
	  next_syntax = SYNTAX_WITH_FLAGS (next_c);
	  if (((comstart || comnested)
	       && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	       && SYNTAX_FLAGS_COMEND_FIRST (next_syntax))
	      || ((com2end || comnested)
		  && SYNTAX_FLAGS_COMSTART_SECOND (syntax)
		  && (comstyle
		      == SYNTAX_FLAGS_COMMENT_STYLE (syntax, prev_syntax))
		  && SYNTAX_FLAGS_COMSTART_FIRST (next_syntax)))
	    goto lossage;
	  /* UPDATE_SYNTAX_TABLE_FORWARD (next + 1); */
	}

      if (com2start && comstart_pos == 0)
	/* We're looking at a comment starter.  But it might be a comment
	   ender as well (see snmp-mode).  The first time we see one, we
	   need to consider it as a comment starter,
	   and the subsequent times as a comment ender.  */
	com2end = 0;

      /* Turn a 2-char comment sequences into the appropriate syntax.  */
      if (com2end)
	code = Sendcomment;
      else if (com2start)
	code = Scomment;
      /* Ignore comment starters of a different style.  */
      else if (code == Scomment
	       && (comstyle != SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0)
		   || SYNTAX_FLAGS_COMMENT_NESTED (syntax) != comnested))
	continue;

      /* Ignore escaped characters, except comment-enders which cannot
         be escaped.  */
      if ((Vcomment_end_can_be_escaped || code != Sendcomment)
          && char_quoted (from, from_byte))
	continue;

      switch (code)
	{
	case Sstring_fence:
	case Scomment_fence:
	  c = (code == Sstring_fence ? ST_STRING_STYLE : ST_COMMENT_STYLE);
	  FALLTHROUGH;
	case Sstring:
	  /* Track parity of quotes.  */
	  if (string_style == -1)
	    /* Entering a string.  */
	    string_style = c;
	  else if (string_style == c)
	    /* Leaving the string.  */
	    string_style = -1;
	  else
	    /* If we have two kinds of string delimiters.
	       There's no way to grok this scanning backwards.  */
	    string_lossage = 1;
	  break;

	case Scomment:
	  /* We've already checked that it is the relevant comstyle.  */
	  if (string_style != -1 || comment_lossage || string_lossage)
	    /* There are odd string quotes involved, so let's be careful.
	       Test case in Pascal: " { " a { " } */
	    goto lossage;

	  if (!comnested)
	    {
	      /* Record best comment-starter so far.  */
	      comstart_pos = from;
	      comstart_byte = from_byte;
	    }
	  else if (--nesting <= 0)
	    /* nested comments have to be balanced, so we don't need to
	       keep looking for earlier ones.  We use here the same (slightly
	       incorrect) reasoning as below:  since it is followed by uniform
	       paired string quotes, this comment-start has to be outside of
	       strings, else the comment-end itself would be inside a string. */
	    goto done;
	  break;

	case Sendcomment:
	  if (SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == comstyle
	      && ((com2end && SYNTAX_FLAGS_COMMENT_NESTED (prev_syntax))
		  || SYNTAX_FLAGS_COMMENT_NESTED (syntax)) == comnested)
	    /* This is the same style of comment ender as ours. */
	    {
	      if (comnested)
		nesting++;
	      else
		/* Anything before that can't count because it would match
		   this comment-ender rather than ours.  */
		from = stop;	/* Break out of the loop.  */
	    }
	  else if (comstart_pos != 0 || c != '\n')
	    /* We're mixing comment styles here, so we'd better be careful.
	       The (comstart_pos != 0 || c != '\n') check is not quite correct
	       (we should just always set comment_lossage), but removing it
	       would imply that any multiline comment in C would go through
	       lossage, which seems overkill.
	       The failure should only happen in the rare cases such as
	         { (* } *)   */
	    comment_lossage = 1;
	  break;

	case Sopen:
	  /* Assume a defun-start point is outside of strings.  */
	  if (open_paren_in_column_0_is_defun_start
              && NILP (Vcomment_use_syntax_ppss)
	      && (from == stop
		  || (temp_byte = dec_bytepos (from_byte),
		      FETCH_CHAR (temp_byte) == '\n')))
	    {
	      defun_start = from;
	      defun_start_byte = from_byte;
	      from = stop;	/* Break out of the loop.  */
	    }
	  break;

	default:
	  break;
	}
    }

  if (comstart_pos == 0)
    {
      from = comment_end;
      from_byte = comment_end_byte;
      UPDATE_SYNTAX_TABLE_FORWARD (comment_end);
    }
  /* If comstart_pos is set and we get here (ie. didn't jump to `lossage'
     or `done'), then we've found the beginning of the non-nested comment.  */
  else if (1)	/* !comnested */
    {
      from = comstart_pos;
      from_byte = comstart_byte;
      UPDATE_SYNTAX_TABLE_FORWARD (from - 1);
    }
  else lossage:
    {
      struct lisp_parse_state state;
      bool adjusted = true;
      /* We had two kinds of string delimiters mixed up
	 together.  Decode this going forwards.
	 Scan fwd from a known safe place (beginning-of-defun)
	 to the one in question; this records where we
	 last passed a comment starter.  */
      /* If we did not already find the defun start, find it now.  */
      if (defun_start == 0)
	{
	  defun_start = find_defun_start (comment_end, comment_end_byte);
	  defun_start_byte = find_start_value_byte;
	  adjusted = (defun_start > BEGV);
	}
      do
	{
          internalize_parse_state (Qnil, &state);
	  scan_sexps_forward (&state,
			      defun_start, defun_start_byte,
			      comment_end, TYPE_MINIMUM (EMACS_INT),
			      0, 0);
	  defun_start = comment_end;
	  if (!adjusted)
	    {
	      adjusted = true;
	      find_start_value
		= CONSP (state.levelstarts) ? XINT (XCAR (state.levelstarts))
		: state.thislevelstart >= 0 ? state.thislevelstart
		: find_start_value;
	      find_start_value_byte = CHAR_TO_BYTE (find_start_value);
	    }

	  if (state.incomment == (comnested ? 1 : -1)
	      && state.comstyle == comstyle)
	    from = state.comstr_start;
	  else
	    {
	      from = comment_end;
	      if (state.incomment)
		/* If comment_end is inside some other comment, maybe ours
		   is nested, so we need to try again from within the
		   surrounding comment.  Example: { a (* " *)  */
		{
		  /* FIXME: We should advance by one or two chars.  */
		  defun_start = state.comstr_start + 2;
		  defun_start_byte = CHAR_TO_BYTE (defun_start);
		}
	    }
	  rarely_quit (++quit_count);
	}
      while (defun_start < comment_end);

      from_byte = CHAR_TO_BYTE (from);
      UPDATE_SYNTAX_TABLE_FORWARD (from - 1);
    }

 done:
  *charpos_ptr = from;
  *bytepos_ptr = from_byte;

  return from != comment_end;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a syntax table.
Currently, any char-table counts as a syntax table.  */)
  (Lisp_Object object)
{
  if (CHAR_TABLE_P (object)
      && EQ (XCHAR_TABLE (object)->purpose, Qsyntax_table))
    return Qt;
  return Qnil;
}

static void
check_syntax_table (Lisp_Object obj)
{
  CHECK_TYPE (CHAR_TABLE_P (obj) && EQ (XCHAR_TABLE (obj)->purpose, Qsyntax_table),
	      Qsyntax_table_p, obj);
}

DEFUN ("syntax-table", Fsyntax_table, Ssyntax_table, 0, 0, 0,
       doc: /* Return the current syntax table.
This is the one specified by the current buffer.  */)
  (void)
{
  return BVAR (current_buffer, syntax_table);
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table,
   Sstandard_syntax_table, 0, 0, 0,
       doc: /* Return the standard syntax table.
This is the one used for new buffers.  */)
  (void)
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, Scopy_syntax_table, 0, 1, 0,
       doc: /* Construct a new syntax table and return it.
It is a copy of the TABLE, which defaults to the standard syntax table.  */)
  (Lisp_Object table)
{
  Lisp_Object copy;

  if (!NILP (table))
    check_syntax_table (table);
  else
    table = Vstandard_syntax_table;

  copy = Fcopy_sequence (table);

  /* Only the standard syntax table should have a default element.
     Other syntax tables should inherit from parents instead.  */
  set_char_table_defalt (copy, Qnil);

  /* Copied syntax tables should all have parents.
     If we copied one with no parent, such as the standard syntax table,
     use the standard syntax table as the copy's parent.  */
  if (NILP (XCHAR_TABLE (copy)->parent))
    Fset_char_table_parent (copy, Vstandard_syntax_table);
  return copy;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
       doc: /* Select a new syntax table for the current buffer.
One argument, a syntax table.  */)
  (Lisp_Object table)
{
  int idx;
  check_syntax_table (table);
  bset_syntax_table (current_buffer, table);
  /* Indicate that this buffer now has a specified syntax table.  */
  idx = PER_BUFFER_VAR_IDX (syntax_table);
  SET_PER_BUFFER_VALUE_P (current_buffer, idx, 1);
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things.  */

unsigned char const syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    Swhitespace, Scomment_fence, Sstring, 0377, Smath, 0377, 0377, Squote,
    Sopen, Sclose, 0377, 0377, 0377, Swhitespace, Spunct, Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, Scomment, 0377, Sendcomment, 0377,
    Sinherit, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, Sword,
    0377, 0377, 0377, 0377, Sescape, 0377, 0377, Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, Sword,
    0377, 0377, 0377, 0377, Sstring_fence, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it.  */

char const syntax_code_spec[16] =
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


DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
       doc: /* Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent, the
character `w' (119) is returned.
The characters that correspond to various syntax codes
are listed in the documentation of `modify-syntax-entry'.

If you're trying to determine the syntax of characters in the buffer,
this is probably the wrong function to use, because it can't take
`syntax-table' text properties into account.  Consider using
`syntax-after' instead.  */)
  (Lisp_Object character)
{
  int char_int;
  CHECK_CHARACTER (character);
  char_int = XINT (character);
  SETUP_BUFFER_SYNTAX_TABLE ();
  return make_number (syntax_code_spec[SYNTAX (char_int)]);
}

DEFUN ("matching-paren", Fmatching_paren, Smatching_paren, 1, 1, 0,
       doc: /* Return the matching parenthesis of CHARACTER, or nil if none.  */)
  (Lisp_Object character)
{
  int char_int;
  enum syntaxcode code;
  CHECK_CHARACTER (character);
  char_int = XINT (character);
  SETUP_BUFFER_SYNTAX_TABLE ();
  code = SYNTAX (char_int);
  if (code == Sopen || code == Sclose)
    return SYNTAX_MATCH (char_int);
  return Qnil;
}

DEFUN ("string-to-syntax", Fstring_to_syntax, Sstring_to_syntax, 1, 1, 0,
       doc: /* Convert a syntax descriptor STRING into a raw syntax descriptor.
STRING should be a string of the form allowed as argument of
`modify-syntax-entry'.  The return value is a raw syntax descriptor: a
cons cell (CODE . MATCHING-CHAR) which can be used, for example, as
the value of a `syntax-table' text property.  */)
  (Lisp_Object string)
{
  const unsigned char *p;
  int val;
  Lisp_Object match;

  CHECK_STRING (string);

  p = SDATA (string);
  val = syntax_spec_code[*p++];
  if (val == 0377)
    error ("Invalid syntax description letter: %c", p[-1]);

  if (val == Sinherit)
    return Qnil;

  if (*p)
    {
      int len;
      int character = STRING_CHAR_AND_LENGTH (p, len);
      XSETINT (match, character);
      if (XFASTINT (match) == ' ')
	match = Qnil;
      p += len;
    }
  else
    match = Qnil;

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

      case 'n':
	val |= 1 << 22;
	break;

      case 'c':
	val |= 1 << 23;
	break;
      }

  if (val < ASIZE (Vsyntax_code_object) && NILP (match))
    return AREF (Vsyntax_code_object, val);
  else
    /* Since we can't use a shared object, let's make a new one.  */
    return Fcons (make_number (val), match);
}

/* I really don't know why this is interactive
   help-form should at least be made useful whilst reading the second arg.  */
DEFUN ("modify-syntax-entry", Fmodify_syntax_entry, Smodify_syntax_entry, 2, 3,
  "cSet syntax for character: \nsSet syntax for %s to: ",
       doc: /* Set syntax for character CHAR according to string NEWENTRY.
The syntax is changed only for table SYNTAX-TABLE, which defaults to
 the current buffer's syntax table.
CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
in the range MIN to MAX are changed.
The first character of NEWENTRY should be one of the following:
  Space or -  whitespace syntax.    w   word constituent.
  _           symbol constituent.   .   punctuation.
  (           open-parenthesis.     )   close-parenthesis.
  "           string quote.         \\   escape.
  $           paired delimiter.     \\='   expression quote or prefix operator.
  <           comment starter.      >   comment ender.
  /           character-quote.      @   inherit from parent table.
  |           generic string fence. !   generic comment fence.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of NEWENTRY is the matching parenthesis,
 used only if the first character is `(' or `)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, b, p, and n.
 1 means CHAR is the start of a two-char comment start sequence.
 2 means CHAR is the second character of such a sequence.
 3 means CHAR is the start of a two-char comment end sequence.
 4 means CHAR is the second character of such a sequence.

There can be several orthogonal comment sequences.  This is to support
language modes such as C++.  By default, all comment sequences are of style
a, but you can set the comment sequence style to b (on the second character
of a comment-start, and the first character of a comment-end sequence) and/or
c (on any of its chars) using this flag:
 b means CHAR is part of comment sequence b.
 c means CHAR is part of comment sequence c.
 n means CHAR is part of a nestable comment sequence.

 p means CHAR is a prefix character for `backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.
usage: (modify-syntax-entry CHAR NEWENTRY &optional SYNTAX-TABLE)  */)
  (Lisp_Object c, Lisp_Object newentry, Lisp_Object syntax_table)
{
  if (CONSP (c))
    {
      CHECK_CHARACTER_CAR (c);
      CHECK_CHARACTER_CDR (c);
    }
  else
    CHECK_CHARACTER (c);

  if (NILP (syntax_table))
    syntax_table = BVAR (current_buffer, syntax_table);
  else
    check_syntax_table (syntax_table);

  newentry = Fstring_to_syntax (newentry);
  if (CONSP (c))
    SET_RAW_SYNTAX_ENTRY_RANGE (syntax_table, c, newentry);
  else
    SET_RAW_SYNTAX_ENTRY (syntax_table, XINT (c), newentry);

  /* We clear the regexp cache, since character classes can now have
     different values from those in the compiled regexps.*/
  clear_regexp_cache ();

  return Qnil;
}

/* Dump syntax table to buffer in human-readable format */

DEFUN ("internal-describe-syntax-value", Finternal_describe_syntax_value,
       Sinternal_describe_syntax_value, 1, 1, 0,
       doc: /* Insert a description of the internal syntax description SYNTAX at point.  */)
  (Lisp_Object syntax)
{
  int code, syntax_code;
  bool start1, start2, end1, end2, prefix, comstyleb, comstylec, comnested;
  char str[2];
  Lisp_Object first, match_lisp, value = syntax;

  if (NILP (value))
    {
      insert_string ("default");
      return syntax;
    }

  if (CHAR_TABLE_P (value))
    {
      insert_string ("deeper char-table ...");
      return syntax;
    }

  if (!CONSP (value))
    {
      insert_string ("invalid");
      return syntax;
    }

  first = XCAR (value);
  match_lisp = XCDR (value);

  if (!INTEGERP (first) || !(NILP (match_lisp) || CHARACTERP (match_lisp)))
    {
      insert_string ("invalid");
      return syntax;
    }

  syntax_code = XINT (first) & INT_MAX;
  code = syntax_code & 0377;
  start1 = SYNTAX_FLAGS_COMSTART_FIRST (syntax_code);
  start2 = SYNTAX_FLAGS_COMSTART_SECOND (syntax_code);
  end1 = SYNTAX_FLAGS_COMEND_FIRST (syntax_code);
  end2 = SYNTAX_FLAGS_COMEND_SECOND (syntax_code);
  prefix = SYNTAX_FLAGS_PREFIX (syntax_code);
  comstyleb = SYNTAX_FLAGS_COMMENT_STYLEB (syntax_code);
  comstylec = SYNTAX_FLAGS_COMMENT_STYLEC (syntax_code);
  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax_code);

  if (Smax <= code)
    {
      insert_string ("invalid");
      return syntax;
    }

  str[0] = syntax_code_spec[code], str[1] = 0;
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
  if (comstyleb)
    insert ("b", 1);
  if (comstylec)
    insert ("c", 1);
  if (comnested)
    insert ("n", 1);

  insert_string ("\twhich means: ");

  switch (code)
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
      insert_string ("prefix"); break;
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
    case Scomment_fence:
      insert_string ("comment fence"); break;
    case Sstring_fence:
      insert_string ("string fence"); break;
    default:
      insert_string ("invalid");
      return syntax;
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
  if (comstyleb)
    insert_string (" (comment style b)");
  if (comstylec)
    insert_string (" (comment style c)");
  if (comnested)
    insert_string (" (nestable)");

  if (prefix)
    {
      AUTO_STRING (prefixdoc,
		   ",\n\t  is a prefix character for `backward-prefix-chars'");
      insert1 (Fsubstitute_command_keys (prefixdoc));
    }

  return syntax;
}

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

ptrdiff_t
scan_words (ptrdiff_t from, EMACS_INT count)
{
  ptrdiff_t beg = BEGV;
  ptrdiff_t end = ZV;
  ptrdiff_t from_byte = CHAR_TO_BYTE (from);
  enum syntaxcode code;
  int ch0, ch1;
  Lisp_Object func, pos;

  SETUP_SYNTAX_TABLE (from, count);

  while (count > 0)
    {
      while (true)
	{
	  if (from == end)
	    return 0;
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  ch0 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  code = SYNTAX (ch0);
	  INC_BOTH (from, from_byte);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  rarely_quit (from);
	}
      /* Now CH0 is a character which begins a word and FROM is the
         position of the next character.  */
      func = CHAR_TABLE_REF (Vfind_word_boundary_function_table, ch0);
      if (! NILP (Ffboundp (func)))
	{
	  pos = call2 (func, make_number (from - 1), make_number (end));
	  if (INTEGERP (pos) && from < XINT (pos) && XINT (pos) <= ZV)
	    {
	      from = XINT (pos);
	      from_byte = CHAR_TO_BYTE (from);
	    }
	}
      else
	{
	  while (1)
	    {
	      if (from == end) break;
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      ch1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      code = SYNTAX (ch1);
	      if ((code != Sword
		   && (! words_include_escapes
		       || (code != Sescape && code != Scharquote)))
		  || word_boundary_p (ch0, ch1))
		break;
	      INC_BOTH (from, from_byte);
	      ch0 = ch1;
	      rarely_quit (from);
	    }
	}
      count--;
    }
  while (count < 0)
    {
      while (true)
	{
	  if (from == beg)
	    return 0;
	  DEC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  ch1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  code = SYNTAX (ch1);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	  rarely_quit (from);
	}
      /* Now CH1 is a character which ends a word and FROM is the
         position of it.  */
      func = CHAR_TABLE_REF (Vfind_word_boundary_function_table, ch1);
      if (! NILP (Ffboundp (func)))
 	{
	  pos = call2 (func, make_number (from), make_number (beg));
	  if (INTEGERP (pos) && BEGV <= XINT (pos) && XINT (pos) < from)
	    {
	      from = XINT (pos);
	      from_byte = CHAR_TO_BYTE (from);
	    }
	}
      else
	{
	  while (1)
	    {
	      if (from == beg)
		break;
	      DEC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from);
	      ch0 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      code = SYNTAX (ch0);
	      if ((code != Sword
		   && (! words_include_escapes
		       || (code != Sescape && code != Scharquote)))
		  || word_boundary_p (ch0, ch1))
		{
		  INC_BOTH (from, from_byte);
		  break;
		}
	      ch1 = ch0;
	      rarely_quit (from);
	    }
	}
      count++;
    }

  return from;
}

DEFUN ("forward-word", Fforward_word, Sforward_word, 0, 1, "^p",
       doc: /* Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word.
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is
left there and the function returns nil.  Field boundaries are not
noticed if `inhibit-field-text-motion' is non-nil.

The word boundaries are normally determined by the buffer's syntax
table and character script (according to `char-script-table'), but
`find-word-boundary-function-table', such as set up by `subword-mode',
can change that.  If a Lisp program needs to move by words determined
strictly by the syntax table, it should use `forward-word-strictly'
instead.  See Info node `(elisp) Word Motion' for details.  */)
  (Lisp_Object arg)
{
  Lisp_Object tmp;
  ptrdiff_t orig_val, val;

  if (NILP (arg))
    XSETFASTINT (arg, 1);
  else
    CHECK_NUMBER (arg);

  val = orig_val = scan_words (PT, XINT (arg));
  if (! orig_val)
    val = XINT (arg) > 0 ? ZV : BEGV;

  /* Avoid jumping out of an input field.  */
  tmp = Fconstrain_to_field (make_number (val), make_number (PT),
			     Qnil, Qnil, Qnil);
  val = XFASTINT (tmp);

  SET_PT (val);
  return val == orig_val ? Qt : Qnil;
}

DEFUN ("skip-chars-forward", Fskip_chars_forward, Sskip_chars_forward, 1, 2, 0,
       doc: /* Move point forward, stopping before a char not in STRING, or at pos LIM.
STRING is like the inside of a `[...]' in a regular expression
except that `]' is never special and `\\' quotes `^', `-' or `\\'
 (but not at the end of a range; quoting is never needed there).
Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
With arg "^a-zA-Z", skips nonletters stopping before first letter.
Char classes, e.g. `[:alpha:]', are supported.

Returns the distance traveled, either zero or positive.  */)
  (Lisp_Object string, Lisp_Object lim)
{
  return skip_chars (1, string, lim, 1);
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, Sskip_chars_backward, 1, 2, 0,
       doc: /* Move point backward, stopping after a char not in STRING, or at pos LIM.
See `skip-chars-forward' for details.
Returns the distance traveled, either zero or negative.  */)
  (Lisp_Object string, Lisp_Object lim)
{
  return skip_chars (0, string, lim, 1);
}

DEFUN ("skip-syntax-forward", Fskip_syntax_forward, Sskip_syntax_forward, 1, 2, 0,
       doc: /* Move point forward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop before a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or positive.  */)
  (Lisp_Object syntax, Lisp_Object lim)
{
  return skip_syntaxes (1, syntax, lim);
}

DEFUN ("skip-syntax-backward", Fskip_syntax_backward, Sskip_syntax_backward, 1, 2, 0,
       doc: /* Move point backward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns either zero or a negative number, and the absolute value
of this is the distance traveled.  */)
  (Lisp_Object syntax, Lisp_Object lim)
{
  return skip_syntaxes (0, syntax, lim);
}

static Lisp_Object
skip_chars (bool forwardp, Lisp_Object string, Lisp_Object lim,
	    bool handle_iso_classes)
{
  int c;
  char fastmap[0400];
  /* Store the ranges of non-ASCII characters.  */
  int *char_ranges UNINIT;
  int n_char_ranges = 0;
  bool negate = 0;
  ptrdiff_t i, i_byte;
  /* True if the current buffer is multibyte and the region contains
     non-ASCII chars.  */
  bool multibyte;
  /* True if STRING is multibyte and it contains non-ASCII chars.  */
  bool string_multibyte;
  ptrdiff_t size_byte;
  const unsigned char *str;
  int len;
  Lisp_Object iso_classes;
  USE_SAFE_ALLOCA;

  CHECK_STRING (string);
  iso_classes = Qnil;

  if (NILP (lim))
    XSETINT (lim, forwardp ? ZV : BEGV);
  else
    CHECK_NUMBER_COERCE_MARKER (lim);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XINT (lim) > ZV)
    XSETFASTINT (lim, ZV);
  if (XINT (lim) < BEGV)
    XSETFASTINT (lim, BEGV);

  multibyte = (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	       && (XINT (lim) - PT != CHAR_TO_BYTE (XINT (lim)) - PT_BYTE));
  string_multibyte = SBYTES (string) > SCHARS (string);

  memset (fastmap, 0, sizeof fastmap);

  str = SDATA (string);
  size_byte = SBYTES (string);

  i_byte = 0;
  if (i_byte < size_byte
      && SREF (string, 0) == '^')
    {
      negate = 1; i_byte++;
    }

  /* Find the characters specified and set their elements of fastmap.
     Handle backslashes and ranges specially.

     If STRING contains non-ASCII characters, setup char_ranges for
     them and use fastmap only for their leading codes.  */

  if (! string_multibyte)
    {
      bool string_has_eight_bit = 0;

      /* At first setup fastmap.  */
      while (i_byte < size_byte)
	{
	  if (handle_iso_classes)
	    {
	      const unsigned char *ch = str + i_byte;
	      re_wctype_t cc = re_wctype_parse (&ch, size_byte - i_byte);
	      if (cc == 0)
		error ("Invalid ISO C character class");
	      if (cc != -1)
		{
		  iso_classes = Fcons (make_number (cc), iso_classes);
		  i_byte = ch - str;
		  continue;
		}
	    }

	  c = str[i_byte++];

	  if (c == '\\')
	    {
	      if (i_byte == size_byte)
		break;

	      c = str[i_byte++];
	    }
	  /* Treat `-' as range character only if another character
	     follows.  */
	  if (i_byte + 1 < size_byte
	      && str[i_byte] == '-')
	    {
	      int c2;

	      /* Skip over the dash.  */
	      i_byte++;

	      /* Get the end of the range.  */
	      c2 = str[i_byte++];
	      if (c2 == '\\'
		  && i_byte < size_byte)
		c2 = str[i_byte++];

	      if (c <= c2)
		{
		  int lim2 = c2 + 1;
		  while (c < lim2)
		    fastmap[c++] = 1;
		  if (! ASCII_CHAR_P (c2))
		    string_has_eight_bit = 1;
		}
	    }
	  else
	    {
	      fastmap[c] = 1;
	      if (! ASCII_CHAR_P (c))
		string_has_eight_bit = 1;
	    }
	}

      /* If the current range is multibyte and STRING contains
	 eight-bit chars, arrange fastmap and setup char_ranges for
	 the corresponding multibyte chars.  */
      if (multibyte && string_has_eight_bit)
	{
	  char *p1;
	  char himap[0200 + 1];
	  memcpy (himap, fastmap + 0200, 0200);
	  himap[0200] = 0;
	  memset (fastmap + 0200, 0, 0200);
	  SAFE_NALLOCA (char_ranges, 2, 128);
	  i = 0;

	  while ((p1 = memchr (himap + i, 1, 0200 - i)))
	    {
	      /* Deduce the next range C..C2 from the next clump of 1s
		 in HIMAP starting with &HIMAP[I].  HIMAP is the high
		 order half of the old FASTMAP.  */
	      int c2, leading_code;
	      i = p1 - himap;
	      c = BYTE8_TO_CHAR (i + 0200);
	      i += strlen (p1);
	      c2 = BYTE8_TO_CHAR (i + 0200 - 1);

	      char_ranges[n_char_ranges++] = c;
	      char_ranges[n_char_ranges++] = c2;
	      leading_code = CHAR_LEADING_CODE (c);
	      memset (fastmap + leading_code, 1,
		      CHAR_LEADING_CODE (c2) - leading_code + 1);
	    }
	}
    }
  else				/* STRING is multibyte */
    {
      SAFE_NALLOCA (char_ranges, 2, SCHARS (string));

      while (i_byte < size_byte)
	{
	  int leading_code = str[i_byte];

	  if (handle_iso_classes)
	    {
	      const unsigned char *ch = str + i_byte;
	      re_wctype_t cc = re_wctype_parse (&ch, size_byte - i_byte);
	      if (cc == 0)
		error ("Invalid ISO C character class");
	      if (cc != -1)
		{
		  iso_classes = Fcons (make_number (cc), iso_classes);
		  i_byte = ch - str;
		  continue;
		}
	    }

	  if (leading_code== '\\')
	    {
	      if (++i_byte == size_byte)
		break;

	      leading_code = str[i_byte];
	    }
	  c = STRING_CHAR_AND_LENGTH (str + i_byte, len);
	  i_byte += len;


	  /* Treat `-' as range character only if another character
	     follows.  */
	  if (i_byte + 1 < size_byte
	      && str[i_byte] == '-')
	    {
	      int c2, leading_code2;

	      /* Skip over the dash.  */
	      i_byte++;

	      /* Get the end of the range.  */
	      leading_code2 = str[i_byte];
	      c2 = STRING_CHAR_AND_LENGTH (str + i_byte, len);
	      i_byte += len;

	      if (c2 == '\\'
		  && i_byte < size_byte)
		{
		  leading_code2 = str[i_byte];
		  c2 = STRING_CHAR_AND_LENGTH (str + i_byte, len);
		  i_byte += len;
		}

	      if (c > c2)
		continue;
	      if (ASCII_CHAR_P (c))
		{
		  while (c <= c2 && c < 0x80)
		    fastmap[c++] = 1;
		  leading_code = CHAR_LEADING_CODE (c);
		}
	      if (! ASCII_CHAR_P (c))
		{
		  int lim2 = leading_code2 + 1;
		  while (leading_code < lim2)
		    fastmap[leading_code++] = 1;
		  if (c <= c2)
		    {
		      char_ranges[n_char_ranges++] = c;
		      char_ranges[n_char_ranges++] = c2;
		    }
		}
	    }
	  else
	    {
	      if (ASCII_CHAR_P (c))
		fastmap[c] = 1;
	      else
		{
		  fastmap[leading_code] = 1;
		  char_ranges[n_char_ranges++] = c;
		  char_ranges[n_char_ranges++] = c;
		}
	    }
	}

      /* If the current range is unibyte and STRING contains non-ASCII
	 chars, arrange fastmap for the corresponding unibyte
	 chars.  */

      if (! multibyte && n_char_ranges > 0)
	{
	  memset (fastmap + 0200, 0, 0200);
	  for (i = 0; i < n_char_ranges; i += 2)
	    {
	      int c1 = char_ranges[i];
	      int lim2 = char_ranges[i + 1] + 1;

	      for (; c1 < lim2; c1++)
		{
		  int b = CHAR_TO_BYTE_SAFE (c1);
		  if (b >= 0)
		    fastmap[b] = 1;
		}
	    }
	}
    }

  /* If ^ was the first character, complement the fastmap.  */
  if (negate)
    {
      if (! multibyte)
	for (i = 0; i < sizeof fastmap; i++)
	  fastmap[i] ^= 1;
      else
	{
	  for (i = 0; i < 0200; i++)
	    fastmap[i] ^= 1;
	  /* All non-ASCII chars possibly match.  */
	  for (; i < sizeof fastmap; i++)
	    fastmap[i] = 1;
	}
    }

  {
    ptrdiff_t start_point = PT;
    ptrdiff_t pos = PT;
    ptrdiff_t pos_byte = PT_BYTE;
    unsigned char *p = PT_ADDR, *endp, *stop;

    if (forwardp)
      {
	endp = (XINT (lim) == GPT) ? GPT_ADDR : CHAR_POS_ADDR (XINT (lim));
	stop = (pos < GPT && GPT < XINT (lim)) ? GPT_ADDR : endp;
      }
    else
      {
	endp = CHAR_POS_ADDR (XINT (lim));
	stop = (pos >= GPT && GPT > XINT (lim)) ? GAP_END_ADDR : endp;
      }

    /* This code may look up syntax tables using functions that rely on the
       gl_state object.  To make sure this object is not out of date,
       let's initialize it manually.
       We ignore syntax-table text-properties for now, since that's
       what we've done in the past.  */
    SETUP_BUFFER_SYNTAX_TABLE ();
    if (forwardp)
      {
	if (multibyte)
	  while (1)
	    {
	      int nbytes;

	      if (p >= stop)
		{
		  if (p >= endp)
		    break;
		  p = GAP_END_ADDR;
		  stop = endp;
		}
	      c = STRING_CHAR_AND_LENGTH (p, nbytes);
	      if (! NILP (iso_classes) && in_classes (c, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto fwd_ok;
		}

	      if (! fastmap[*p])
		break;
	      if (! ASCII_CHAR_P (c))
		{
		  /* As we are looking at a multibyte character, we
		     must look up the character in the table
		     CHAR_RANGES.  If there's no data in the table,
		     that character is not what we want to skip.  */

		  /* The following code do the right thing even if
		     n_char_ranges is zero (i.e. no data in
		     CHAR_RANGES).  */
		  for (i = 0; i < n_char_ranges; i += 2)
		    if (c >= char_ranges[i] && c <= char_ranges[i + 1])
		      break;
		  if (!(negate ^ (i < n_char_ranges)))
		    break;
		}
	    fwd_ok:
	      p += nbytes, pos++, pos_byte += nbytes;
	      rarely_quit (pos);
	    }
	else
	  while (true)
	    {
	      if (p >= stop)
		{
		  if (p >= endp)
		    break;
		  p = GAP_END_ADDR;
		  stop = endp;
		}

	      if (!NILP (iso_classes) && in_classes (*p, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto fwd_unibyte_ok;
		}

	      if (!fastmap[*p])
		break;
	    fwd_unibyte_ok:
	      p++, pos++, pos_byte++;
	      rarely_quit (pos);
	    }
      }
    else
      {
	if (multibyte)
	  while (true)
	    {
	      if (p <= stop)
		{
		  if (p <= endp)
		    break;
		  p = GPT_ADDR;
		  stop = endp;
		}
	      unsigned char *prev_p = p;
	      do
		p--;
	      while (stop <= p && ! CHAR_HEAD_P (*p));

	      c = STRING_CHAR (p);

	      if (! NILP (iso_classes) && in_classes (c, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto back_ok;
		}

	      if (! fastmap[*p])
		break;
	      if (! ASCII_CHAR_P (c))
		{
		  /* See the comment in the previous similar code.  */
		  for (i = 0; i < n_char_ranges; i += 2)
		    if (c >= char_ranges[i] && c <= char_ranges[i + 1])
		      break;
		  if (!(negate ^ (i < n_char_ranges)))
		    break;
		}
	    back_ok:
	      pos--, pos_byte -= prev_p - p;
	      rarely_quit (pos);
	    }
	else
	  while (true)
	    {
	      if (p <= stop)
		{
		  if (p <= endp)
		    break;
		  p = GPT_ADDR;
		  stop = endp;
		}

	      if (! NILP (iso_classes) && in_classes (p[-1], iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto back_unibyte_ok;
		}

	      if (!fastmap[p[-1]])
		break;
	    back_unibyte_ok:
	      p--, pos--, pos_byte--;
	      rarely_quit (pos);
	    }
      }

    SET_PT_BOTH (pos, pos_byte);

    SAFE_FREE ();
    return make_number (PT - start_point);
  }
}


static Lisp_Object
skip_syntaxes (bool forwardp, Lisp_Object string, Lisp_Object lim)
{
  int c;
  unsigned char fastmap[0400];
  bool negate = 0;
  ptrdiff_t i, i_byte;
  bool multibyte;
  ptrdiff_t size_byte;
  unsigned char *str;

  CHECK_STRING (string);

  if (NILP (lim))
    XSETINT (lim, forwardp ? ZV : BEGV);
  else
    CHECK_NUMBER_COERCE_MARKER (lim);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XINT (lim) > ZV)
    XSETFASTINT (lim, ZV);
  if (XINT (lim) < BEGV)
    XSETFASTINT (lim, BEGV);

  if (forwardp ? (PT >= XFASTINT (lim)) : (PT <= XFASTINT (lim)))
    return make_number (0);

  multibyte = (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	       && (XINT (lim) - PT != CHAR_TO_BYTE (XINT (lim)) - PT_BYTE));

  memset (fastmap, 0, sizeof fastmap);

  if (SBYTES (string) > SCHARS (string))
    /* As this is very rare case (syntax spec is ASCII only), don't
       consider efficiency.  */
    string = string_make_unibyte (string);

  str = SDATA (string);
  size_byte = SBYTES (string);

  i_byte = 0;
  if (i_byte < size_byte
      && SREF (string, 0) == '^')
    {
      negate = 1; i_byte++;
    }

  /* Find the syntaxes specified and set their elements of fastmap.  */

  while (i_byte < size_byte)
    {
      c = str[i_byte++];
      fastmap[syntax_spec_code[c]] = 1;
    }

  /* If ^ was the first character, complement the fastmap.  */
  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      fastmap[i] ^= 1;

  {
    ptrdiff_t start_point = PT;
    ptrdiff_t pos = PT;
    ptrdiff_t pos_byte = PT_BYTE;
    unsigned char *p, *endp, *stop;

    SETUP_SYNTAX_TABLE (pos, forwardp ? 1 : -1);

    if (forwardp)
      {
	while (true)
	  {
	    p = BYTE_POS_ADDR (pos_byte);
	    endp = XINT (lim) == GPT ? GPT_ADDR : CHAR_POS_ADDR (XINT (lim));
	    stop = pos < GPT && GPT < XINT (lim) ? GPT_ADDR : endp;

	    do
	      {
		int nbytes;

		if (p >= stop)
		  {
		    if (p >= endp)
		      goto done;
		    p = GAP_END_ADDR;
		    stop = endp;
		  }
		if (multibyte)
		  c = STRING_CHAR_AND_LENGTH (p, nbytes);
		else
		  c = *p, nbytes = 1;
		if (! fastmap[SYNTAX (c)])
		  goto done;
		p += nbytes, pos++, pos_byte += nbytes;
		rarely_quit (pos);
	      }
	    while (!parse_sexp_lookup_properties
		   || pos < gl_state.e_property);

	    update_syntax_table_forward (pos + gl_state.offset,
					 false, gl_state.object);
	  }
      }
    else
      {
	p = BYTE_POS_ADDR (pos_byte);
	endp = CHAR_POS_ADDR (XINT (lim));
	stop = pos >= GPT && GPT > XINT (lim) ? GAP_END_ADDR : endp;

	if (multibyte)
	  {
	    while (true)
	      {
		if (p <= stop)
		  {
		    if (p <= endp)
		      break;
		    p = GPT_ADDR;
		    stop = endp;
		  }
		UPDATE_SYNTAX_TABLE_BACKWARD (pos - 1);

		unsigned char *prev_p = p;
		do
		  p--;
		while (stop <= p && ! CHAR_HEAD_P (*p));

		c = STRING_CHAR (p);
		if (! fastmap[SYNTAX (c)])
		  break;
		pos--, pos_byte -= prev_p - p;
		rarely_quit (pos);
	      }
	  }
	else
	  {
	    while (true)
	      {
		if (p <= stop)
		  {
		    if (p <= endp)
		      break;
		    p = GPT_ADDR;
		    stop = endp;
		  }
		UPDATE_SYNTAX_TABLE_BACKWARD (pos - 1);
		if (! fastmap[SYNTAX (p[-1])])
		  break;
		p--, pos--, pos_byte--;
		rarely_quit (pos);
	      }
	  }
      }

  done:
    SET_PT_BOTH (pos, pos_byte);

    return make_number (PT - start_point);
  }
}

/* Return true if character C belongs to one of the ISO classes
   in the list ISO_CLASSES.  Each class is represented by an
   integer which is its type according to re_wctype.  */

static bool
in_classes (int c, Lisp_Object iso_classes)
{
  bool fits_class = 0;

  while (CONSP (iso_classes))
    {
      Lisp_Object elt;
      elt = XCAR (iso_classes);
      iso_classes = XCDR (iso_classes);

      if (re_iswctype (c, XFASTINT (elt)))
	fits_class = 1;
    }

  return fits_class;
}

/* Jump over a comment, assuming we are at the beginning of one.
   FROM is the current position.
   FROM_BYTE is the bytepos corresponding to FROM.
   Do not move past STOP (a charpos).
   The comment over which we have to jump is of style STYLE
     (either SYNTAX_FLAGS_COMMENT_STYLE (foo) or ST_COMMENT_STYLE).
   NESTING should be positive to indicate the nesting at the beginning
     for nested comments and should be zero or negative else.
     ST_COMMENT_STYLE cannot be nested.
   PREV_SYNTAX is the SYNTAX_WITH_FLAGS of the previous character
     (or 0 If the search cannot start in the middle of a two-character).

   If successful, return true and store the charpos of the comment's
   end into *CHARPOS_PTR and the corresponding bytepos into
   *BYTEPOS_PTR.  Else, return false and store the charpos STOP into
   *CHARPOS_PTR, the corresponding bytepos into *BYTEPOS_PTR and the
   current nesting (as defined for state->incomment) in
   *INCOMMENT_PTR.  Should the last character scanned in an incomplete
   comment be a possible first character of a two character construct,
   we store its SYNTAX_WITH_FLAGS into *last_syntax_ptr.  Otherwise,
   we store Smax into *last_syntax_ptr.

   The comment end is the last character of the comment rather than the
   character just after the comment.

   Global syntax data is assumed to initially be valid for FROM and
   remains valid for forward search starting at the returned position. */

static bool
forw_comment (ptrdiff_t from, ptrdiff_t from_byte, ptrdiff_t stop,
	      EMACS_INT nesting, int style, int prev_syntax,
	      ptrdiff_t *charpos_ptr, ptrdiff_t *bytepos_ptr,
	      EMACS_INT *incomment_ptr, int *last_syntax_ptr)
{
  unsigned short int quit_count = 0;
  int c, c1;
  enum syntaxcode code;
  int syntax, other_syntax;

  if (nesting <= 0) nesting = -1;

  /* Enter the loop in the middle so that we find
     a 2-char comment ender if we start in the middle of it.  */
  syntax = prev_syntax;
  code = syntax & 0xff;
  if (syntax != 0 && from < stop) goto forw_incomment;

  while (1)
    {
      if (from == stop)
	{
	  *incomment_ptr = nesting;
	  *charpos_ptr = from;
	  *bytepos_ptr = from_byte;
          *last_syntax_ptr =
            (code == Sescape || code == Scharquote
             || SYNTAX_FLAGS_COMEND_FIRST (syntax)
             || (nesting > 0
                 && SYNTAX_FLAGS_COMSTART_FIRST (syntax)))
            ? syntax : Smax ;
	  return 0;
	}
      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
      syntax = SYNTAX_WITH_FLAGS (c);
      code = syntax & 0xff;
      if (code == Sendcomment
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == style
	  && (SYNTAX_FLAGS_COMMENT_NESTED (syntax) ?
	      (nesting > 0 && --nesting == 0) : nesting < 0)
          && !(Vcomment_end_can_be_escaped && char_quoted (from, from_byte)))
	/* We have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section.  */
	break;
      if (code == Scomment_fence
	  && style == ST_COMMENT_STYLE)
	/* We have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section.  */
	break;
      if (nesting > 0
	  && code == Scomment
	  && SYNTAX_FLAGS_COMMENT_NESTED (syntax)
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == style)
	/* We have encountered a nested comment of the same style
	   as the comment sequence which began this comment section.  */
	nesting++;
      INC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_FORWARD (from);

    forw_incomment:
      if (from < stop && SYNTAX_FLAGS_COMEND_FIRST (syntax)
	  && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
	      other_syntax = SYNTAX_WITH_FLAGS (c1),
	      SYNTAX_FLAGS_COMEND_SECOND (other_syntax))
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, other_syntax) == style
	  && ((SYNTAX_FLAGS_COMMENT_NESTED (syntax) ||
	       SYNTAX_FLAGS_COMMENT_NESTED (other_syntax))
	      ? nesting > 0 : nesting < 0))
	{
	  syntax = Smax;        /* So that "|#" (lisp) can not return
                                   the syntax of "#" in *last_syntax_ptr. */
          if (--nesting <= 0)
	    /* We have encountered a comment end of the same style
	       as the comment sequence which began this comment section.  */
	    break;
	  else
	    {
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }
	}
      if (nesting > 0
	  && from < stop
	  && SYNTAX_FLAGS_COMSTART_FIRST (syntax)
	  && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
	      other_syntax = SYNTAX_WITH_FLAGS (c1),
	      SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax) == style
	      && SYNTAX_FLAGS_COMSTART_SECOND (other_syntax))
	  && (SYNTAX_FLAGS_COMMENT_NESTED (syntax) ||
	      SYNTAX_FLAGS_COMMENT_NESTED (other_syntax)))
	/* We have encountered a nested comment of the same style
	   as the comment sequence which began this comment section.  */
	{
          syntax = Smax; /* So that "#|#" isn't also a comment ender. */
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  nesting++;
	}

      rarely_quit (++quit_count);
    }
  *charpos_ptr = from;
  *bytepos_ptr = from_byte;
  *last_syntax_ptr = Smax; /* Any syntactic power the last byte had is
                              used up. */
  return 1;
}

DEFUN ("forward-comment", Fforward_comment, Sforward_comment, 1, 1, 0,
       doc: /*
Move forward across up to COUNT comments.  If COUNT is negative, move backward.
Stop scanning if we find something other than a comment or whitespace.
Set point to where scanning stops.
If COUNT comments are found as expected, with nothing except whitespace
between them, return t; otherwise return nil.  */)
  (Lisp_Object count)
{
  ptrdiff_t from, from_byte, stop;
  int c, c1;
  enum syntaxcode code;
  int comstyle = 0;	    /* style of comment encountered */
  bool comnested = 0;	    /* whether the comment is nestable or not */
  bool found;
  EMACS_INT count1;
  ptrdiff_t out_charpos, out_bytepos;
  EMACS_INT dummy;
  int dummy2;
  unsigned short int quit_count = 0;

  CHECK_NUMBER (count);
  count1 = XINT (count);
  stop = count1 > 0 ? ZV : BEGV;

  from = PT;
  from_byte = PT_BYTE;

  SETUP_SYNTAX_TABLE (from, count1);
  while (count1 > 0)
    {
      do
	{
	  bool comstart_first;
	  int syntax, other_syntax;

	  if (from == stop)
	    {
	      SET_PT_BOTH (from, from_byte);
	      return Qnil;
	    }
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax = SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX (c);
	  comstart_first = SYNTAX_FLAGS_COMSTART_FIRST (syntax);
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (from < stop && comstart_first
	      && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
		  other_syntax = SYNTAX_WITH_FLAGS (c1),
		  SYNTAX_FLAGS_COMSTART_SECOND (other_syntax)))
	    {
	      /* We have encountered a comment start sequence and we
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section.  */
	      code = Scomment;
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested |= SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }
	  rarely_quit (++quit_count);
	}
      while (code == Swhitespace || (code == Sendcomment && c == '\n'));

      if (code == Scomment_fence)
	comstyle = ST_COMMENT_STYLE;
      else if (code != Scomment)
	{
	  DEC_BOTH (from, from_byte);
	  SET_PT_BOTH (from, from_byte);
	  return Qnil;
	}
      /* We're at the start of a comment.  */
      found = forw_comment (from, from_byte, stop, comnested, comstyle, 0,
			    &out_charpos, &out_bytepos, &dummy, &dummy2);
      from = out_charpos; from_byte = out_bytepos;
      if (!found)
	{
	  SET_PT_BOTH (from, from_byte);
	  return Qnil;
	}
      INC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_FORWARD (from);
      /* We have skipped one comment.  */
      count1--;
    }

  while (count1 < 0)
    {
      while (true)
	{
	  if (from <= stop)
	    {
	      SET_PT_BOTH (BEGV, BEGV_BYTE);
	      return Qnil;
	    }

	  DEC_BOTH (from, from_byte);
	  /* char_quoted does UPDATE_SYNTAX_TABLE_BACKWARD (from).  */
	  bool quoted = char_quoted (from, from_byte);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  int syntax = SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX (c);
	  comstyle = 0;
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  if (code == Sendcomment)
	    comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  if (from > stop && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	      && prev_char_comend_first (from, from_byte)
	      && !char_quoted (from - 1, dec_bytepos (from_byte)))
	    {
	      int other_syntax;
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      DEC_BOTH (from, from_byte);
	      code = Sendcomment;
	      /* Calling char_quoted, above, set up global syntax position
		 at the new value of FROM.  */
	      c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      other_syntax = SYNTAX_WITH_FLAGS (c1);
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested |= SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	    }

	  if (code == Scomment_fence)
	    {
	      /* Skip until first preceding unquoted comment_fence.  */
	      bool fence_found = 0;
	      ptrdiff_t ini = from, ini_byte = from_byte;

	      while (1)
		{
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  if (SYNTAX (c) == Scomment_fence
		      && !char_quoted (from, from_byte))
		    {
		      fence_found = 1;
		      break;
		    }
		  else if (from == stop)
		    break;
		  rarely_quit (++quit_count);
		}
	      if (fence_found == 0)
		{
		  from = ini;		/* Set point to ini + 1.  */
		  from_byte = ini_byte;
		  goto leave;
		}
 	      else
		/* We have skipped one comment.  */
		break;
	    }
	  else if (code == Sendcomment)
	    {
	      found = back_comment (from, from_byte, stop, comnested, comstyle,
				    &out_charpos, &out_bytepos);
	      if (!found)
		{
		  if (c == '\n')
		    /* This end-of-line is not an end-of-comment.
		       Treat it like a whitespace.
		       CC-mode (and maybe others) relies on this behavior.  */
		    ;
		  else
		    {
		      /* Failure: we should go back to the end of this
			 not-quite-endcomment.  */
		      if (SYNTAX (c) != code)
			/* It was a two-char Sendcomment.  */
			INC_BOTH (from, from_byte);
		      goto leave;
		    }
		}
	      else
		{
		  /* We have skipped one comment.  */
		  from = out_charpos, from_byte = out_bytepos;
		  break;
		}
	    }
	  else if (code != Swhitespace || quoted)
	    {
	    leave:
	      INC_BOTH (from, from_byte);
	      SET_PT_BOTH (from, from_byte);
	      return Qnil;
	    }

	  rarely_quit (++quit_count);
	}

      count1++;
    }

  SET_PT_BOTH (from, from_byte);
  return Qt;
}

/* Return syntax code of character C if C is an ASCII character
   or if MULTIBYTE_SYMBOL_P is false.  Otherwise, return Ssymbol.  */

static enum syntaxcode
syntax_multibyte (int c, bool multibyte_symbol_p)
{
  return ASCII_CHAR_P (c) || !multibyte_symbol_p ? SYNTAX (c) : Ssymbol;
}

static Lisp_Object
scan_lists (EMACS_INT from, EMACS_INT count, EMACS_INT depth, bool sexpflag)
{
  Lisp_Object val;
  ptrdiff_t stop = count > 0 ? ZV : BEGV;
  int c, c1;
  int stringterm;
  bool quoted;
  bool mathexit = 0;
  enum syntaxcode code;
  EMACS_INT min_depth = depth;  /* Err out if depth gets less than this.  */
  int comstyle = 0;		/* Style of comment encountered.  */
  bool comnested = 0;		/* Whether the comment is nestable or not.  */
  ptrdiff_t temp_pos;
  EMACS_INT last_good = from;
  bool found;
  ptrdiff_t from_byte;
  ptrdiff_t out_bytepos, out_charpos;
  EMACS_INT dummy;
  int dummy2;
  bool multibyte_symbol_p = sexpflag && multibyte_syntax_as_symbol;
  unsigned short int quit_count = 0;

  if (depth > 0) min_depth = 0;

  if (from > ZV) from = ZV;
  if (from < BEGV) from = BEGV;

  from_byte = CHAR_TO_BYTE (from);

  maybe_quit ();

  SETUP_SYNTAX_TABLE (from, count);
  while (count > 0)
    {
      while (from < stop)
	{
	  rarely_quit (++quit_count);
	  bool comstart_first, prefix;
	  int syntax, other_syntax;
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax = SYNTAX_WITH_FLAGS (c);
	  code = syntax_multibyte (c, multibyte_symbol_p);
	  comstart_first = SYNTAX_FLAGS_COMSTART_FIRST (syntax);
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  prefix = SYNTAX_FLAGS_PREFIX (syntax);
	  if (depth == min_depth)
	    last_good = from;
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (from < stop && comstart_first
	      && (c = FETCH_CHAR_AS_MULTIBYTE (from_byte),
		  other_syntax = SYNTAX_WITH_FLAGS (c),
		  SYNTAX_FLAGS_COMSTART_SECOND (other_syntax))
	      && parse_sexp_ignore_comments)
	    {
	      /* We have encountered a comment start sequence and we
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section.  */
	      code = Scomment;
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested |= SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }

	  if (prefix)
	    continue;

	  switch (code)
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop)
		goto lose;
	      INC_BOTH (from, from_byte);
	      /* Treat following character as a word constituent.  */
	      FALLTHROUGH;
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it.  */
	      while (from < stop)
		{
		  UPDATE_SYNTAX_TABLE_FORWARD (from);

		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  switch (syntax_multibyte (c, multibyte_symbol_p))
		    {
		    case Scharquote:
		    case Sescape:
		      INC_BOTH (from, from_byte);
		      if (from == stop)
			goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  INC_BOTH (from, from_byte);
		  rarely_quit (++quit_count);
		}
	      goto done;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	      FALLTHROUGH;
	    case Scomment:
	      if (!parse_sexp_ignore_comments) break;
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      found = forw_comment (from, from_byte, stop,
				    comnested, comstyle, 0,
				    &out_charpos, &out_bytepos, &dummy,
                                    &dummy2);
	      from = out_charpos, from_byte = out_bytepos;
	      if (!found)
		{
		  if (depth == 0)
		    goto done;
		  goto lose;
		}
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR_AS_MULTIBYTE (from_byte))
		{
		  INC_BOTH (from, from_byte);
		}
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;
	      FALLTHROUGH;
	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	      if (!--depth) goto done;
	      if (depth < min_depth)
		xsignal3 (Qscan_error,
			  build_string ("Containing expression ends prematurely"),
			  make_number (last_good), make_number (from));
	      break;

	    case Sstring:
	    case Sstring_fence:
	      temp_pos = dec_bytepos (from_byte);
	      stringterm = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
	      while (1)
		{
		  enum syntaxcode c_code;
		  if (from >= stop)
		    goto lose;
		  UPDATE_SYNTAX_TABLE_FORWARD (from);
		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  c_code = syntax_multibyte (c, multibyte_symbol_p);
		  if (code == Sstring
		      ? c == stringterm && c_code == Sstring
		      : c_code == Sstring_fence)
		    break;

		  if (c_code == Scharquote || c_code == Sescape)
		    INC_BOTH (from, from_byte);
		  INC_BOTH (from, from_byte);
		  rarely_quit (++quit_count);
		}
	      INC_BOTH (from, from_byte);
	      if (!depth && sexpflag) goto done;
	      break;
	    default:
	      /* Ignore whitespace, punctuation, quote, endcomment.  */
	      break;
	    }
	}

      /* Reached end of buffer.  Error if within object, return nil if between */
      if (depth)
	goto lose;

      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      while (from > stop)
	{
	  rarely_quit (++quit_count);
	  DEC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  int syntax = SYNTAX_WITH_FLAGS (c);
	  code = syntax_multibyte (c, multibyte_symbol_p);
	  if (depth == min_depth)
	    last_good = from;
	  comstyle = 0;
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  if (code == Sendcomment)
	    comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  if (from > stop && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	      && prev_char_comend_first (from, from_byte)
	      && parse_sexp_ignore_comments)
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      int c2, other_syntax;
	      DEC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from);
	      code = Sendcomment;
	      c2 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      other_syntax = SYNTAX_WITH_FLAGS (c2);
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested |= SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	    }

	  /* Quoting turns anything except a comment-ender
	     into a word character.  Note that this cannot be true
	     if we decremented FROM in the if-statement above.  */
	  if (code != Sendcomment && char_quoted (from, from_byte))
	    {
	      DEC_BOTH (from, from_byte);
	      code = Sword;
	    }
	  else if (SYNTAX_FLAGS_PREFIX (syntax))
	    continue;

	  switch (code)
	    {
	    case Sword:
	    case Ssymbol:
	    case Sescape:
	    case Scharquote:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished
		 after passing it.  */
	      while (from > stop)
		{
		  temp_pos = from_byte;
		  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
		    DEC_POS (temp_pos);
		  else
		    temp_pos--;
		  UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
		  c1 = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
		  /* Don't allow comment-end to be quoted.  */
		  if (syntax_multibyte (c1, multibyte_symbol_p) == Sendcomment)
		    goto done2;
		  quoted = char_quoted (from - 1, temp_pos);
		  if (quoted)
		    {
		      DEC_BOTH (from, from_byte);
		      temp_pos = dec_bytepos (temp_pos);
		      UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
		    }
		  c1 = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
		  if (! quoted)
		    switch (syntax_multibyte (c1, multibyte_symbol_p))
		      {
		      case Sword: case Ssymbol: case Squote: break;
		      default: goto done2;
		      }
		  DEC_BOTH (from, from_byte);
		  rarely_quit (++quit_count);
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from > BEGV)
		{
		  temp_pos = dec_bytepos (from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
		  if (from != stop && c == FETCH_CHAR_AS_MULTIBYTE (temp_pos))
		    DEC_BOTH (from, from_byte);
		}
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;
	      FALLTHROUGH;
	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	      if (!--depth) goto done2;
	      if (depth < min_depth)
		xsignal3 (Qscan_error,
			  build_string ("Containing expression ends prematurely"),
			  make_number (last_good), make_number (from));
	      break;

	    case Sendcomment:
	      if (!parse_sexp_ignore_comments)
		break;
	      found = back_comment (from, from_byte, stop, comnested, comstyle,
				    &out_charpos, &out_bytepos);
	      /* FIXME:  if !found, it really wasn't a comment-end.
		 For single-char Sendcomment, we can't do much about it apart
		 from skipping the char.
		 For 2-char endcomments, we could try again, taking both
		 chars as separate entities, but it's a lot of trouble
		 for very little gain, so we don't bother either.  -sm */
	      if (found)
		from = out_charpos, from_byte = out_bytepos;
	      break;

	    case Scomment_fence:
	    case Sstring_fence:
	      while (1)
		{
		  if (from == stop)
		    goto lose;
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  if (!char_quoted (from, from_byte))
		    {
		      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		      if (syntax_multibyte (c, multibyte_symbol_p) == code)
			break;
		    }
		  rarely_quit (++quit_count);
		}
	      if (code == Sstring_fence && !depth && sexpflag) goto done2;
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      while (true)
		{
		  if (from == stop)
		    goto lose;
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  if (!char_quoted (from, from_byte))
		    {
		      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		      if (c == stringterm
			  && (syntax_multibyte (c, multibyte_symbol_p)
			      == Sstring))
			break;
		    }
		  rarely_quit (++quit_count);
		}
	      if (!depth && sexpflag) goto done2;
	      break;
	    default:
	      /* Ignore whitespace, punctuation, quote, endcomment.  */
	      break;
	    }
	}

      /* Reached start of buffer.  Error if within object, return nil if between */
      if (depth)
	goto lose;

      return Qnil;

    done2:
      count++;
    }


  XSETFASTINT (val, from);
  return val;

 lose:
  xsignal3 (Qscan_error,
	    build_string ("Unbalanced parentheses"),
	    make_number (last_good), make_number (from));
}

DEFUN ("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
       doc: /* Scan from character number FROM by COUNT lists.
Scan forward if COUNT is positive, backward if COUNT is negative.
Return the character number of the position thus found.

A \"list", in this context, refers to a balanced parenthetical
grouping, as determined by the syntax table.

If DEPTH is nonzero, treat that as the nesting depth of the starting
point (i.e. the starting point is DEPTH parentheses deep).  This
function scans over parentheses until the depth goes to zero COUNT
times.  Hence, positive DEPTH moves out that number of levels of
parentheses, while negative DEPTH moves to a deeper level.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If we reach the beginning or end of the accessible part of the buffer
before we have scanned over COUNT lists, return nil if the depth at
that point is zero, and signal an error if the depth is nonzero.  */)
  (Lisp_Object from, Lisp_Object count, Lisp_Object depth)
{
  CHECK_NUMBER (from);
  CHECK_NUMBER (count);
  CHECK_NUMBER (depth);

  return scan_lists (XINT (from), XINT (count), XINT (depth), 0);
}

DEFUN ("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
       doc: /* Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned.  */)
  (Lisp_Object from, Lisp_Object count)
{
  CHECK_NUMBER (from);
  CHECK_NUMBER (count);

  return scan_lists (XINT (from), XINT (count), 0, 1);
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, Sbackward_prefix_chars,
       0, 0, 0,
       doc: /* Move point backward over any number of chars with prefix syntax.
This includes chars with expression prefix syntax class (\\=') and those with
the prefix syntax flag (p).  */)
  (void)
{
  ptrdiff_t beg = BEGV;
  ptrdiff_t opoint = PT;
  ptrdiff_t opoint_byte = PT_BYTE;
  ptrdiff_t pos = PT;
  ptrdiff_t pos_byte = PT_BYTE;
  int c;

  if (pos <= beg)
    {
      SET_PT_BOTH (opoint, opoint_byte);

      return Qnil;
    }

  SETUP_SYNTAX_TABLE (pos, -1);

  DEC_BOTH (pos, pos_byte);

  while (!char_quoted (pos, pos_byte)
	 /* Previous statement updates syntax table.  */
	 && ((c = FETCH_CHAR_AS_MULTIBYTE (pos_byte), SYNTAX (c) == Squote)
	     || syntax_prefix_flag_p (c)))
    {
      opoint = pos;
      opoint_byte = pos_byte;

      if (pos <= beg)
        break;
      DEC_BOTH (pos, pos_byte);
      rarely_quit (pos);
    }

  SET_PT_BOTH (opoint, opoint_byte);

  return Qnil;
}


/* If the character at FROM_BYTE is the second part of a 2-character
   comment opener based on PREV_FROM_SYNTAX, update STATE and return
   true.  */
static bool
in_2char_comment_start (struct lisp_parse_state *state,
                        int prev_from_syntax,
                        ptrdiff_t prev_from,
                        ptrdiff_t from_byte)
{
  int c1, syntax;
  if (SYNTAX_FLAGS_COMSTART_FIRST (prev_from_syntax)
      && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
          syntax = SYNTAX_WITH_FLAGS (c1),
          SYNTAX_FLAGS_COMSTART_SECOND (syntax)))
    {
      /* Record the comment style we have entered so that only
         the comment-end sequence of the same style actually
         terminates the comment section.  */
      state->comstyle
        = SYNTAX_FLAGS_COMMENT_STYLE (syntax, prev_from_syntax);
      bool comnested = (SYNTAX_FLAGS_COMMENT_NESTED (prev_from_syntax)
                        | SYNTAX_FLAGS_COMMENT_NESTED (syntax));
      state->incomment = comnested ? 1 : -1;
      state->comstr_start = prev_from;
      return true;
    }
  return false;
}

/* Parse forward from FROM / FROM_BYTE to END,
   assuming that FROM has state STATE,
   and return a description of the state of the parse at END.
   If STOPBEFORE, stop at the start of an atom.
   If COMMENTSTOP is 1, stop at the start of a comment.
   If COMMENTSTOP is -1, stop at the start or end of a comment,
   after the beginning of a string, or after the end of a string.  */

static void
scan_sexps_forward (struct lisp_parse_state *state,
		    ptrdiff_t from, ptrdiff_t from_byte, ptrdiff_t end,
		    EMACS_INT targetdepth, bool stopbefore,
		    int commentstop)
{
  enum syntaxcode code;
  struct level { ptrdiff_t last, prev; };
  struct level levelstart[100];
  struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  EMACS_INT depth;      /* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  EMACS_INT mindepth;		/* Lowest DEPTH value seen.  */
  bool start_quoted = 0;	/* True means starting after a char quote.  */
  Lisp_Object tem;
  ptrdiff_t prev_from;		/* Keep one character before FROM.  */
  ptrdiff_t prev_from_byte;
  int prev_from_syntax, prev_prev_from_syntax;
  bool boundary_stop = commentstop == -1;
  bool nofence;
  bool found;
  ptrdiff_t out_bytepos, out_charpos;
  int temp;
  unsigned short int quit_count = 0;

  prev_from = from;
  prev_from_byte = from_byte;
  if (from != BEGV)
    DEC_BOTH (prev_from, prev_from_byte);

  /* Use this macro instead of `from++'.  */
#define INC_FROM				\
do { prev_from = from;				\
     prev_from_byte = from_byte; 		\
     temp = FETCH_CHAR_AS_MULTIBYTE (prev_from_byte);	\
     prev_prev_from_syntax = prev_from_syntax;  \
     prev_from_syntax = SYNTAX_WITH_FLAGS (temp); \
     INC_BOTH (from, from_byte);		\
     if (from < end)				\
       UPDATE_SYNTAX_TABLE_FORWARD (from);	\
  } while (0)

  maybe_quit ();

  depth = state->depth;
  start_quoted = state->quoted;
  prev_prev_from_syntax = Smax;
  prev_from_syntax = state->prev_syntax;

  tem = state->levelstarts;
  while (!NILP (tem))		/* >= second enclosing sexps.  */
    {
      Lisp_Object temhd = Fcar (tem);
      if (RANGED_INTEGERP (PTRDIFF_MIN, temhd, PTRDIFF_MAX))
        curlevel->last = XINT (temhd);
      if (++curlevel == endlevel)
        curlevel--; /* error ("Nesting too deep for parser"); */
      curlevel->prev = -1;
      curlevel->last = -1;
      tem = Fcdr (tem);
    }
  curlevel->prev = -1;
  curlevel->last = -1;

  state->quoted = 0;
  mindepth = depth;

  SETUP_SYNTAX_TABLE (from, 1);

  /* Enter the loop at a place appropriate for initial state.  */

  if (state->incomment)
    goto startincomment;
  if (state->instring >= 0)
    {
      nofence = state->instring != ST_STRING_STYLE;
      if (start_quoted)
	goto startquotedinstring;
      goto startinstring;
    }
  else if (start_quoted)
    goto startquoted;
  else if ((from < end)
           && (in_2char_comment_start (state, prev_from_syntax,
                                       prev_from, from_byte)))
    {
      INC_FROM;
      prev_from_syntax = Smax; /* the syntax has already been "used up". */
      goto atcomment;
    }

  while (from < end)
    {
      rarely_quit (++quit_count);
      INC_FROM;

      if ((from < end)
          && (in_2char_comment_start (state, prev_from_syntax,
                                      prev_from, from_byte)))
        {
          INC_FROM;
          prev_from_syntax = Smax; /* the syntax has already been "used up". */
          goto atcomment;
        }

      if (SYNTAX_FLAGS_PREFIX (prev_from_syntax))
	continue;
      code = prev_from_syntax & 0xff;
      switch (code)
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
              if (in_2char_comment_start (state, prev_from_syntax,
                                          prev_from, from_byte))
                {
                  INC_FROM;
                  prev_from_syntax = Smax; /* the syntax has already been "used up". */
                  goto atcomment;
                }

	      int symchar = FETCH_CHAR_AS_MULTIBYTE (from_byte);
              switch (SYNTAX (symchar))
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
	      rarely_quit (++quit_count);
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment_fence:
          /* Record the comment style we have entered so that only
             the comment-end sequence of the same style actually
             terminates the comment section.  */
          state->comstyle = ST_COMMENT_STYLE;
          state->incomment = -1;
          state->comstr_start = prev_from;
          goto atcomment;
	case Scomment:
          state->comstyle = SYNTAX_FLAGS_COMMENT_STYLE (prev_from_syntax, 0);
          state->incomment = (SYNTAX_FLAGS_COMMENT_NESTED (prev_from_syntax) ?
                              1 : -1);
          state->comstr_start = prev_from;
        atcomment:
          if (commentstop || boundary_stop) goto done;
	startincomment:
	  /* The (from == BEGV) test was to enter the loop in the middle so
	     that we find a 2-char comment ender even if we start in the
	     middle of it.  We don't want to do that if we're just at the
	     beginning of the comment (think of (*) ... (*)).  */
	  found = forw_comment (from, from_byte, end,
				state->incomment, state->comstyle,
				from == BEGV ? 0 : prev_from_syntax,
				&out_charpos, &out_bytepos, &state->incomment,
                                &prev_from_syntax);
	  from = out_charpos; from_byte = out_bytepos;
	  /* Beware!  prev_from and friends (except prev_from_syntax)
	     are invalid now.  Luckily, the `done' doesn't use them
	     and the INC_FROM sets them to a sane value without
	     looking at them. */
	  if (!found) goto done;
	  INC_FROM;
	  state->incomment = 0;
	  state->comstyle = 0;	/* reset the comment style */
	  prev_from_syntax = Smax; /* For the comment closer */
          if (boundary_stop) goto done;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = prev_from;
	  if (++curlevel == endlevel)
	    curlevel--; /* error ("Nesting too deep for parser"); */
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
	  state->comstr_start = from - 1;
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	  state->instring = (code == Sstring
			    ? (FETCH_CHAR_AS_MULTIBYTE (prev_from_byte))
			    : ST_STRING_STYLE);
	  if (boundary_stop) goto done;
	startinstring:
	  {
	    nofence = state->instring != ST_STRING_STYLE;

	    while (1)
	      {
		int c;
		enum syntaxcode c_code;

		if (from >= end) goto done;
		c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		c_code = SYNTAX (c);

		/* Check C_CODE here so that if the char has
		   a syntax-table property which says it is NOT
		   a string character, it does not end the string.  */
		if (nofence && c == state->instring && c_code == Sstring)
		  break;

		switch (c_code)
		  {
		  case Sstring_fence:
		    if (!nofence) goto string_end;
		    break;

		  case Scharquote:
		  case Sescape:
		    INC_FROM;
		  startquotedinstring:
		    if (from >= end) goto endquoted;
		    break;

		  default:
		    break;
		  }
		INC_FROM;
		rarely_quit (++quit_count);
	      }
	  }
	string_end:
	  state->instring = -1;
	  curlevel->prev = curlevel->last;
	  INC_FROM;
	  if (boundary_stop) goto done;
	  break;

	case Smath:
	  /* FIXME: We should do something with it.  */
	  break;
	default:
	  /* Ignore whitespace, punctuation, quote, endcomment.  */
	  break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from = prev_from;    /* We have just fetched the char that starts it; */
  from_byte = prev_from_byte;
  prev_from_syntax = prev_prev_from_syntax;
  goto done; /* but return the position before it. */

 endquoted:
  state->quoted = 1;
 done:
  state->depth = depth;
  state->mindepth = mindepth;
  state->thislevelstart = curlevel->prev;
  state->prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state->location = from;
  state->location_byte = from_byte;
  state->levelstarts = Qnil;
  while (curlevel > levelstart)
    state->levelstarts = Fcons (make_number ((--curlevel)->last),
                                state->levelstarts);
  state->prev_syntax = (SYNTAX_FLAGS_COMSTARTEND_FIRST (prev_from_syntax)
                        || state->quoted) ? prev_from_syntax : Smax;
}

/* Convert a (lisp) parse state to the internal form used in
   scan_sexps_forward.  */
static void
internalize_parse_state (Lisp_Object external, struct lisp_parse_state *state)
{
  Lisp_Object tem;

  if (NILP (external))
    {
      state->depth = 0;
      state->instring = -1;
      state->incomment = 0;
      state->quoted = 0;
      state->comstyle = 0;	/* comment style a by default.  */
      state->comstr_start = -1;	/* no comment/string seen.  */
      state->levelstarts = Qnil;
      state->prev_syntax = Smax;
    }
  else
    {
      tem = Fcar (external);
      if (!NILP (tem))
	state->depth = XINT (tem);
      else
	state->depth = 0;

      external = Fcdr (external);
      external = Fcdr (external);
      external = Fcdr (external);
      tem = Fcar (external);
      /* Check whether we are inside string_fence-style string: */
      state->instring = (!NILP (tem)
                         ? (CHARACTERP (tem) ? XFASTINT (tem) : ST_STRING_STYLE)
                         : -1);

      external = Fcdr (external);
      tem = Fcar (external);
      state->incomment = (!NILP (tem)
                          ? (INTEGERP (tem) ? XINT (tem) : -1)
                          : 0);

      external = Fcdr (external);
      tem = Fcar (external);
      state->quoted = !NILP (tem);

      /* if the eighth element of the list is nil, we are in comment
	 style a.  If it is non-nil, we are in comment style b */
      external = Fcdr (external);
      external = Fcdr (external);
      tem = Fcar (external);
      state->comstyle = (NILP (tem)
                         ? 0
                         : (RANGED_INTEGERP (0, tem, ST_COMMENT_STYLE)
                            ? XINT (tem)
                            : ST_COMMENT_STYLE));

      external = Fcdr (external);
      tem = Fcar (external);
      state->comstr_start =
	RANGED_INTEGERP (PTRDIFF_MIN, tem, PTRDIFF_MAX) ? XINT (tem) : -1;
      external = Fcdr (external);
      tem = Fcar (external);
      state->levelstarts = tem;

      external = Fcdr (external);
      tem = Fcar (external);
      state->prev_syntax = NILP (tem) ? Smax : XINT (tem);
    }
}

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 6, 0,
       doc: /* Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.

Value is a list of elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)
 4. nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting).
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. style of comment, if any.
 8. character address of start of comment or string; nil if not in one.
 9. List of positions of currently open parens, outermost first.
10. When the last position scanned holds the first character of a
    (potential) two character construct, the syntax of that position,
    otherwise nil.  That construct can be a two character comment
    delimiter or an Escaped or Char-quoted character.
11..... Possible further internal information used by `parse-partial-sexp'.

If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when we come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a list like what this function returns.
 It is used to initialize the state of the parse.  Elements number 1, 2, 6
 are ignored.
Sixth arg COMMENTSTOP non-nil means stop after the start of a comment.
 If it is the symbol `syntax-table', stop after the start of a comment or a
 string, or after end of a comment or a string.  */)
  (Lisp_Object from, Lisp_Object to, Lisp_Object targetdepth,
   Lisp_Object stopbefore, Lisp_Object oldstate, Lisp_Object commentstop)
{
  struct lisp_parse_state state;
  EMACS_INT target;

  if (!NILP (targetdepth))
    {
      CHECK_NUMBER (targetdepth);
      target = XINT (targetdepth);
    }
  else
    target = TYPE_MINIMUM (EMACS_INT);	/* We won't reach this depth.  */

  validate_region (&from, &to);
  internalize_parse_state (oldstate, &state);
  scan_sexps_forward (&state, XINT (from), CHAR_TO_BYTE (XINT (from)),
		      XINT (to),
		      target, !NILP (stopbefore),
		      (NILP (commentstop)
		       ? 0 : (EQ (commentstop, Qsyntax_table) ? -1 : 1)));

  SET_PT_BOTH (state.location, state.location_byte);

  return
    Fcons (make_number (state.depth),
	   Fcons (state.prevlevelstart < 0
		  ? Qnil : make_number (state.prevlevelstart),
	     Fcons (state.thislevelstart < 0
		    ? Qnil : make_number (state.thislevelstart),
	       Fcons (state.instring >= 0
		      ? (state.instring == ST_STRING_STYLE
			 ? Qt : make_number (state.instring)) : Qnil,
		 Fcons (state.incomment < 0 ? Qt :
			(state.incomment == 0 ? Qnil :
			 make_number (state.incomment)),
		   Fcons (state.quoted ? Qt : Qnil,
		     Fcons (make_number (state.mindepth),
		       Fcons ((state.comstyle
			       ? (state.comstyle == ST_COMMENT_STYLE
				  ? Qsyntax_table
				  : make_number (state.comstyle))
			       : Qnil),
		         Fcons (((state.incomment
                                  || (state.instring >= 0))
                                 ? make_number (state.comstr_start)
                                 : Qnil),
			   Fcons (state.levelstarts,
                             Fcons (state.prev_syntax == Smax
                                    ? Qnil
                                    : make_number (state.prev_syntax),
                                Qnil)))))))))));
}

void
init_syntax_once (void)
{
  register int i, c;
  Lisp_Object temp;

  /* This has to be done here, before we call Fmake_char_table.  */
  DEFSYM (Qsyntax_table, "syntax-table");

  /* Create objects which can be shared among syntax tables.  */
  Vsyntax_code_object = make_uninit_vector (Smax);
  for (i = 0; i < Smax; i++)
    ASET (Vsyntax_code_object, i, Fcons (make_number (i), Qnil));

  /* Now we are ready to set up this property, so we can
     create syntax tables.  */
  Fput (Qsyntax_table, Qchar_table_extra_slots, make_number (0));

  temp = AREF (Vsyntax_code_object, Swhitespace);

  Vstandard_syntax_table = Fmake_char_table (Qsyntax_table, temp);

  /* Control characters should not be whitespace.  */
  temp = AREF (Vsyntax_code_object, Spunct);
  for (i = 0; i <= ' ' - 1; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 0177, temp);

  /* Except that a few really are whitespace.  */
  temp = AREF (Vsyntax_code_object, Swhitespace);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ' ', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\t', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\n', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 015, temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 014, temp);

  temp = AREF (Vsyntax_code_object, Sword);
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
			Fcons (make_number (Sstring), Qnil));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\\',
			Fcons (make_number (Sescape), Qnil));

  temp = AREF (Vsyntax_code_object, Ssymbol);
  for (i = 0; i < 10; i++)
    {
      c = "_-+*/&|<>="[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }

  temp = AREF (Vsyntax_code_object, Spunct);
  for (i = 0; i < 12; i++)
    {
      c = ".,;:?!#@~^'`"[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }

  /* All multibyte characters have syntax `word' by default.  */
  temp = AREF (Vsyntax_code_object, Sword);
  char_table_set_range (Vstandard_syntax_table, 0x80, MAX_CHAR, temp);
}

void
syms_of_syntax (void)
{
  DEFSYM (Qsyntax_table_p, "syntax-table-p");
  DEFSYM (Qsyntax_ppss, "syntax-ppss");
  DEFVAR_LISP ("comment-use-syntax-ppss",
	       Vcomment_use_syntax_ppss,
	       doc: /* Non-nil means `forward-comment' can use `syntax-ppss' internally.  */);
  Vcomment_use_syntax_ppss = Qt;

  staticpro (&Vsyntax_code_object);

  staticpro (&gl_state.object);
  staticpro (&gl_state.global_code);
  staticpro (&gl_state.current_syntax_table);
  staticpro (&gl_state.old_prop);

  /* Defined in regex.c.  */
  staticpro (&re_match_object);

  DEFSYM (Qscan_error, "scan-error");
  Fput (Qscan_error, Qerror_conditions,
	listn (CONSTYPE_PURE, 2, Qscan_error, Qerror));
  Fput (Qscan_error, Qerror_message,
	build_pure_c_string ("Scan error"));

  DEFVAR_BOOL ("parse-sexp-ignore-comments", parse_sexp_ignore_comments,
	       doc: /* Non-nil means `forward-sexp', etc., should treat comments as whitespace.  */);

  DEFVAR_BOOL ("parse-sexp-lookup-properties", parse_sexp_lookup_properties,
	       doc: /* Non-nil means `forward-sexp', etc., obey `syntax-table' property.
Otherwise, that text property is simply ignored.
See the info node `(elisp)Syntax Properties' for a description of the
`syntax-table' property.  */);

  DEFVAR_INT ("syntax-propertize--done", syntax_propertize__done,
	      doc: /* Position up to which syntax-table properties have been set.  */);
  syntax_propertize__done = -1;
  DEFSYM (Qinternal__syntax_propertize, "internal--syntax-propertize");
  Fmake_variable_buffer_local (intern ("syntax-propertize--done"));

  words_include_escapes = 0;
  DEFVAR_BOOL ("words-include-escapes", words_include_escapes,
	       doc: /* Non-nil means `forward-word', etc., should treat escape chars part of words.  */);

  DEFVAR_BOOL ("multibyte-syntax-as-symbol", multibyte_syntax_as_symbol,
	       doc: /* Non-nil means `scan-sexps' treats all multibyte characters as symbol.  */);
  multibyte_syntax_as_symbol = 0;

  DEFVAR_BOOL ("open-paren-in-column-0-is-defun-start",
	       open_paren_in_column_0_is_defun_start,
	       doc: /* Non-nil means an open paren in column 0 denotes the start of a defun.  */);
  open_paren_in_column_0_is_defun_start = 1;


  DEFVAR_LISP ("find-word-boundary-function-table",
	       Vfind_word_boundary_function_table,
	       doc: /*
Char table of functions to search for the word boundary.
Each function is called with two arguments; POS and LIMIT.
POS and LIMIT are character positions in the current buffer.

If POS is less than LIMIT, POS is at the first character of a word,
and the return value of a function should be a position after the
last character of that word.

If POS is not less than LIMIT, POS is at the last character of a word,
and the return value of a function should be a position at the first
character of that word.

In both cases, LIMIT bounds the search. */);
  Vfind_word_boundary_function_table = Fmake_char_table (Qnil, Qnil);

  DEFVAR_BOOL ("comment-end-can-be-escaped", Vcomment_end_can_be_escaped,
               doc: /* Non-nil means an escaped ender inside a comment doesn't end the comment.  */);
  Vcomment_end_can_be_escaped = 0;
  DEFSYM (Qcomment_end_can_be_escaped, "comment-end-can-be-escaped");
  Fmake_variable_buffer_local (Qcomment_end_can_be_escaped);

  defsubr (&Ssyntax_table_p);
  defsubr (&Ssyntax_table);
  defsubr (&Sstandard_syntax_table);
  defsubr (&Scopy_syntax_table);
  defsubr (&Sset_syntax_table);
  defsubr (&Schar_syntax);
  defsubr (&Smatching_paren);
  defsubr (&Sstring_to_syntax);
  defsubr (&Smodify_syntax_entry);
  defsubr (&Sinternal_describe_syntax_value);

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
