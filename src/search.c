/* String search routines for GNU Emacs.

Copyright (C) 1985-1987, 1993-1994, 1997-1999, 2001-2018 Free Software
Foundation, Inc.

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
#include "syntax.h"
#include "charset.h"
#include "region-cache.h"
#include "blockinput.h"
#include "intervals.h"

#include "regex.h"

#define REGEXP_CACHE_SIZE 20

/* If the regexp is non-nil, then the buffer contains the compiled form
   of that regexp, suitable for searching.  */
struct regexp_cache
{
  struct regexp_cache *next;
  Lisp_Object regexp, f_whitespace_regexp;
  /* Syntax table for which the regexp applies.  We need this because
     of character classes.  If this is t, then the compiled pattern is valid
     for any syntax-table.  */
  Lisp_Object syntax_table;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  /* True means regexp was compiled to do full POSIX backtracking.  */
  bool posix;
};

/* The instances of that struct.  */
static struct regexp_cache searchbufs[REGEXP_CACHE_SIZE];

/* The head of the linked list; points to the most recently used buffer.  */
static struct regexp_cache *searchbuf_head;


/* Every call to re_match, etc., must pass &search_regs as the regs
   argument unless you can show it is unnecessary (i.e., if re_match
   is certainly going to be called again before region-around-match
   can be called).

   Since the registers are now dynamically allocated, we need to make
   sure not to refer to the Nth register before checking that it has
   been allocated by checking search_regs.num_regs.

   The regex code keeps track of whether it has allocated the search
   buffer using bits in the re_pattern_buffer.  This means that whenever
   you compile a new pattern, it completely forgets whether it has
   allocated any registers, and will allocate new registers the next
   time you call a searching or matching function.  Therefore, we need
   to call re_set_registers after compiling a new pattern or after
   setting the match registers, so that the regex functions will be
   able to free or re-allocate it properly.  */
/* static struct re_registers search_regs; */

/* The buffer in which the last search was performed, or
   Qt if the last search was done in a string;
   Qnil if no searching has been done yet.  */
/* static Lisp_Object last_thing_searched; */

static void set_search_regs (ptrdiff_t, ptrdiff_t);
static void save_search_regs (void);
static EMACS_INT simple_search (EMACS_INT, unsigned char *, ptrdiff_t,
				ptrdiff_t, Lisp_Object, ptrdiff_t, ptrdiff_t,
                                ptrdiff_t, ptrdiff_t);
static EMACS_INT boyer_moore (EMACS_INT, unsigned char *, ptrdiff_t,
                              Lisp_Object, Lisp_Object, ptrdiff_t,
                              ptrdiff_t, int);
static EMACS_INT search_buffer (Lisp_Object, ptrdiff_t, ptrdiff_t,
                                ptrdiff_t, ptrdiff_t, EMACS_INT, int,
                                Lisp_Object, Lisp_Object, bool);

static _Noreturn void
matcher_overflow (void)
{
  error ("Stack overflow in regexp matcher");
}

static void
freeze_buffer_relocation (void)
{
#ifdef REL_ALLOC
  /* Prevent ralloc.c from relocating the current buffer while
     searching it.  */
  r_alloc_inhibit_buffer_relocation (1);
  record_unwind_protect_int (r_alloc_inhibit_buffer_relocation, 0);
#endif
}

static void
thaw_buffer_relocation (void)
{
#ifdef REL_ALLOC
  unbind_to (SPECPDL_INDEX () - 1, Qnil);
#endif
}

/* Compile a regexp and signal a Lisp error if anything goes wrong.
   PATTERN is the pattern to compile.
   CP is the place to put the result.
   TRANSLATE is a translation table for ignoring case, or nil for none.
   POSIX is true if we want full backtracking (POSIX style) for this pattern.
   False means backtrack only enough to get a valid match.

   The behavior also depends on Vsearch_spaces_regexp.  */

static void
compile_pattern_1 (struct regexp_cache *cp, Lisp_Object pattern,
		   Lisp_Object translate, bool posix)
{
  const char *whitespace_regexp;
  char *val;

  cp->regexp = Qnil;
  cp->buf.translate = (! NILP (translate) ? translate : make_number (0));
  cp->posix = posix;
  cp->buf.multibyte = STRING_MULTIBYTE (pattern);
  cp->buf.charset_unibyte = charset_unibyte;
  if (STRINGP (Vsearch_spaces_regexp))
    cp->f_whitespace_regexp = Vsearch_spaces_regexp;
  else
    cp->f_whitespace_regexp = Qnil;

  /* rms: I think BLOCK_INPUT is not needed here any more,
     because regex.c defines malloc to call xmalloc.
     Using BLOCK_INPUT here means the debugger won't run if an error occurs.
     So let's turn it off.  */
  /*  BLOCK_INPUT;  */

  whitespace_regexp = STRINGP (Vsearch_spaces_regexp) ?
    SSDATA (Vsearch_spaces_regexp) : NULL;

  val = (char *) re_compile_pattern (SSDATA (pattern), SBYTES (pattern),
				     posix, whitespace_regexp, &cp->buf);

  /* If the compiled pattern hard codes some of the contents of the
     syntax-table, it can only be reused with *this* syntax table.  */
  cp->syntax_table = cp->buf.used_syntax ? BVAR (current_buffer, syntax_table) : Qt;

  /* unblock_input ();  */
  if (val)
    xsignal1 (Qinvalid_regexp, build_string (val));

  cp->regexp = Fcopy_sequence (pattern);
}

/* Shrink each compiled regexp buffer in the cache
   to the size actually used right now.
   This is called from garbage collection.  */

void
shrink_regexp_cache (void)
{
  struct regexp_cache *cp;

  for (cp = searchbuf_head; cp != 0; cp = cp->next)
    {
      cp->buf.allocated = cp->buf.used;
      cp->buf.buffer = xrealloc (cp->buf.buffer, cp->buf.used);
    }
}

/* Clear the regexp cache w.r.t. a particular syntax table,
   because it was changed.
   There is no danger of memory leak here because re_compile_pattern
   automagically manages the memory in each re_pattern_buffer struct,
   based on its `allocated' and `buffer' values.  */
void
clear_regexp_cache (void)
{
  int i;

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    /* It's tempting to compare with the syntax-table we've actually changed,
       but it's not sufficient because char-table inheritance means that
       modifying one syntax-table can change others at the same time.  */
    if (!EQ (searchbufs[i].syntax_table, Qt))
      searchbufs[i].regexp = Qnil;
}

/* Compile a regexp if necessary, but first check to see if there's one in
   the cache.
   PATTERN is the pattern to compile.
   TRANSLATE is a translation table for ignoring case, or nil for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is true if we want full backtracking (POSIX style) for this pattern.
   False means backtrack only enough to get a valid match.  */

struct re_pattern_buffer *
compile_pattern (Lisp_Object pattern, struct re_registers *regp,
		 Lisp_Object translate, bool posix, bool multibyte)
{
  struct regexp_cache *cp, **cpp;

  for (cpp = &searchbuf_head; ; cpp = &cp->next)
    {
      cp = *cpp;
      /* Entries are initialized to nil, and may be set to nil by
	 compile_pattern_1 if the pattern isn't valid.  Don't apply
	 string accessors in those cases.  However, compile_pattern_1
	 is only applied to the cache entry we pick here to reuse.  So
	 nil should never appear before a non-nil entry.  */
      if (NILP (cp->regexp))
	goto compile_it;
      if (SCHARS (cp->regexp) == SCHARS (pattern)
	  && STRING_MULTIBYTE (cp->regexp) == STRING_MULTIBYTE (pattern)
	  && !NILP (Fstring_equal (cp->regexp, pattern))
	  && EQ (cp->buf.translate, (! NILP (translate) ? translate : make_number (0)))
	  && cp->posix == posix
	  && (EQ (cp->syntax_table, Qt)
	      || EQ (cp->syntax_table, BVAR (current_buffer, syntax_table)))
	  && !NILP (Fequal (cp->f_whitespace_regexp, Vsearch_spaces_regexp))
	  && cp->buf.charset_unibyte == charset_unibyte)
	break;

      /* If we're at the end of the cache, compile into the nil cell
	 we found, or the last (least recently used) cell with a
	 string value.  */
      if (cp->next == 0)
	{
	compile_it:
	  compile_pattern_1 (cp, pattern, translate, posix);
	  break;
	}
    }

  /* When we get here, cp (aka *cpp) contains the compiled pattern,
     either because we found it in the cache or because we just compiled it.
     Move it to the front of the queue to mark it as most recently used.  */
  *cpp = cp->next;
  cp->next = searchbuf_head;
  searchbuf_head = cp;

  /* Advise the searching functions about the space we have allocated
     for register data.  */
  if (regp)
    re_set_registers (&cp->buf, regp, regp->num_regs, regp->start, regp->end);

  /* The compiled pattern can be used both for multibyte and unibyte
     target.  But, we have to tell which the pattern is used for. */
  cp->buf.target_multibyte = multibyte;

  return &cp->buf;
}


static Lisp_Object
looking_at_1 (Lisp_Object string, bool posix)
{
  Lisp_Object val;
  unsigned char *p1, *p2;
  ptrdiff_t s1, s2;
  register ptrdiff_t i;
  struct re_pattern_buffer *bufp;

  if (running_asynch_code)
    save_search_regs ();

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  set_char_table_extras (BVAR (current_buffer, case_canon_table), 2,
			 BVAR (current_buffer, case_eqv_table));

  CHECK_STRING (string);
  bufp = compile_pattern (string,
			  (NILP (Vinhibit_changing_match_data)
			   ? &search_regs : NULL),
			  (!NILP (BVAR (current_buffer, case_fold_search))
			   ? BVAR (current_buffer, case_canon_table) : Qnil),
			  posix,
			  !NILP (BVAR (current_buffer, enable_multibyte_characters)));

  /* Do a pending quit right away, to avoid paradoxical behavior */
  maybe_quit ();

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BEGV_ADDR;
  s1 = GPT_BYTE - BEGV_BYTE;
  p2 = GAP_END_ADDR;
  s2 = ZV_BYTE - GPT_BYTE;
  if (s1 < 0)
    {
      p2 = p1;
      s2 = ZV_BYTE - BEGV_BYTE;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 = ZV_BYTE - BEGV_BYTE;
      s2 = 0;
    }

  re_match_object = Qnil;

  freeze_buffer_relocation ();
  i = re_match_2 (bufp, (char *) p1, s1, (char *) p2, s2,
		  PT_BYTE - BEGV_BYTE,
		  (NILP (Vinhibit_changing_match_data)
		   ? &search_regs : NULL),
		  ZV_BYTE - BEGV_BYTE);
  thaw_buffer_relocation ();

  if (i == -2)
    matcher_overflow ();

  val = (i >= 0 ? Qt : Qnil);
  if (NILP (Vinhibit_changing_match_data) && i >= 0)
  {
    for (i = 0; i < search_regs.num_regs; i++)
      if (search_regs.start[i] >= 0)
	{
	  search_regs.start[i]
	    = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
         search_regs.end[i]
           = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
       }
    /* Set last_thing_searched only when match data is changed.  */
    XSETBUFFER (last_thing_searched, current_buffer);
  }

  return val;
}

DEFUN ("looking-at", Flooking_at, Slooking_at, 1, 1, 0,
       doc: /* Return t if text after point matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.  */)
  (Lisp_Object regexp)
{
  return looking_at_1 (regexp, 0);
}

DEFUN ("posix-looking-at", Fposix_looking_at, Sposix_looking_at, 1, 1, 0,
       doc: /* Return t if text after point matches regular expression REGEXP.
Find the longest match, in accord with Posix regular expression rules.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.  */)
  (Lisp_Object regexp)
{
  return looking_at_1 (regexp, 1);
}

static Lisp_Object
string_match_1 (Lisp_Object regexp, Lisp_Object string, Lisp_Object start,
		bool posix)
{
  ptrdiff_t val;
  struct re_pattern_buffer *bufp;
  EMACS_INT pos;
  ptrdiff_t pos_byte, i;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_STRING (regexp);
  CHECK_STRING (string);

  if (NILP (start))
    pos = 0, pos_byte = 0;
  else
    {
      ptrdiff_t len = SCHARS (string);

      CHECK_NUMBER (start);
      pos = XINT (start);
      if (pos < 0 && -pos <= len)
	pos = len + pos;
      else if (0 > pos || pos > len)
	args_out_of_range (string, start);
      pos_byte = string_char_to_byte (string, pos);
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  set_char_table_extras (BVAR (current_buffer, case_canon_table), 2,
			 BVAR (current_buffer, case_eqv_table));

  bufp = compile_pattern (regexp,
			  (NILP (Vinhibit_changing_match_data)
			   ? &search_regs : NULL),
			  (!NILP (BVAR (current_buffer, case_fold_search))
			   ? BVAR (current_buffer, case_canon_table) : Qnil),
			  posix,
			  STRING_MULTIBYTE (string));
  re_match_object = string;

  val = re_search (bufp, SSDATA (string),
		   SBYTES (string), pos_byte,
		   SBYTES (string) - pos_byte,
		   (NILP (Vinhibit_changing_match_data)
		    ? &search_regs : NULL));

  /* Set last_thing_searched only when match data is changed.  */
  if (NILP (Vinhibit_changing_match_data))
    last_thing_searched = Qt;

  if (val == -2)
    matcher_overflow ();
  if (val < 0) return Qnil;

  if (NILP (Vinhibit_changing_match_data))
    for (i = 0; i < search_regs.num_regs; i++)
      if (search_regs.start[i] >= 0)
	{
	  search_regs.start[i]
	    = string_byte_to_char (string, search_regs.start[i]);
	  search_regs.end[i]
	    = string_byte_to_char (string, search_regs.end[i]);
	}

  return make_number (string_byte_to_char (string, val));
}

DEFUN ("string-match", Fstring_match, Sstring_match, 2, 3, 0,
       doc: /* Return index of start of first match for REGEXP in STRING, or nil.
Matching ignores case if `case-fold-search' is non-nil.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.

You can use the function `match-string' to extract the substrings
matched by the parenthesis constructions in REGEXP. */)
  (Lisp_Object regexp, Lisp_Object string, Lisp_Object start)
{
  return string_match_1 (regexp, string, start, 0);
}

DEFUN ("posix-string-match", Fposix_string_match, Sposix_string_match, 2, 3, 0,
       doc: /* Return index of start of first match for REGEXP in STRING, or nil.
Find the longest match, in accord with Posix regular expression rules.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.  */)
  (Lisp_Object regexp, Lisp_Object string, Lisp_Object start)
{
  return string_match_1 (regexp, string, start, 1);
}

/* Match REGEXP against STRING using translation table TABLE,
   searching all of STRING, and return the index of the match,
   or negative on failure.  This does not clobber the match data.  */

ptrdiff_t
fast_string_match_internal (Lisp_Object regexp, Lisp_Object string,
			    Lisp_Object table)
{
  ptrdiff_t val;
  struct re_pattern_buffer *bufp;

  bufp = compile_pattern (regexp, 0, table,
			  0, STRING_MULTIBYTE (string));
  re_match_object = string;

  val = re_search (bufp, SSDATA (string),
		   SBYTES (string), 0,
		   SBYTES (string), 0);
  return val;
}

/* Match REGEXP against STRING, searching all of STRING ignoring case,
   and return the index of the match, or negative on failure.
   This does not clobber the match data.
   We assume that STRING contains single-byte characters.  */

ptrdiff_t
fast_c_string_match_ignore_case (Lisp_Object regexp,
				 const char *string, ptrdiff_t len)
{
  ptrdiff_t val;
  struct re_pattern_buffer *bufp;

  regexp = string_make_unibyte (regexp);
  re_match_object = Qt;
  bufp = compile_pattern (regexp, 0,
			  Vascii_canon_table, 0,
			  0);
  val = re_search (bufp, string, len, 0, len, 0);
  return val;
}

/* Match REGEXP against the characters after POS to LIMIT, and return
   the number of matched characters.  If STRING is non-nil, match
   against the characters in it.  In that case, POS and LIMIT are
   indices into the string.  This function doesn't modify the match
   data.  */

ptrdiff_t
fast_looking_at (Lisp_Object regexp, ptrdiff_t pos, ptrdiff_t pos_byte,
		 ptrdiff_t limit, ptrdiff_t limit_byte, Lisp_Object string)
{
  bool multibyte;
  struct re_pattern_buffer *buf;
  unsigned char *p1, *p2;
  ptrdiff_t s1, s2;
  ptrdiff_t len;

  if (STRINGP (string))
    {
      if (pos_byte < 0)
	pos_byte = string_char_to_byte (string, pos);
      if (limit_byte < 0)
	limit_byte = string_char_to_byte (string, limit);
      p1 = NULL;
      s1 = 0;
      p2 = SDATA (string);
      s2 = SBYTES (string);
      re_match_object = string;
      multibyte = STRING_MULTIBYTE (string);
    }
  else
    {
      if (pos_byte < 0)
	pos_byte = CHAR_TO_BYTE (pos);
      if (limit_byte < 0)
	limit_byte = CHAR_TO_BYTE (limit);
      pos_byte -= BEGV_BYTE;
      limit_byte -= BEGV_BYTE;
      p1 = BEGV_ADDR;
      s1 = GPT_BYTE - BEGV_BYTE;
      p2 = GAP_END_ADDR;
      s2 = ZV_BYTE - GPT_BYTE;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV_BYTE - BEGV_BYTE;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV_BYTE - BEGV_BYTE;
	  s2 = 0;
	}
      re_match_object = Qnil;
      multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
    }

  buf = compile_pattern (regexp, 0, Qnil, 0, multibyte);
  freeze_buffer_relocation ();
  len = re_match_2 (buf, (char *) p1, s1, (char *) p2, s2,
		    pos_byte, NULL, limit_byte);
  thaw_buffer_relocation ();

  return len;
}


/* The newline cache: remembering which sections of text have no newlines.  */

/* If the user has requested the long scans caching, make sure it's on.
   Otherwise, make sure it's off.
   This is our cheezy way of associating an action with the change of
   state of a buffer-local variable.  */
static struct region_cache *
newline_cache_on_off (struct buffer *buf)
{
  struct buffer *base_buf = buf;
  bool indirect_p = false;

  if (buf->base_buffer)
    {
      base_buf = buf->base_buffer;
      indirect_p = true;
    }

  /* Don't turn on or off the cache in the base buffer, if the value
     of cache-long-scans of the base buffer is inconsistent with that.
     This is because doing so will just make the cache pure overhead,
     since if we turn it on via indirect buffer, it will be
     immediately turned off by its base buffer.  */
  if (NILP (BVAR (buf, cache_long_scans)))
    {
      if (!indirect_p
	  || NILP (BVAR (base_buf, cache_long_scans)))
	{
	  /* It should be off.  */
	  if (base_buf->newline_cache)
	    {
	      free_region_cache (base_buf->newline_cache);
	      base_buf->newline_cache = 0;
	    }
	}
      return NULL;
    }
  else
    {
      if (!indirect_p
	  || !NILP (BVAR (base_buf, cache_long_scans)))
	{
	  /* It should be on.  */
	  if (base_buf->newline_cache == 0)
	    base_buf->newline_cache = new_region_cache ();
	}
      return base_buf->newline_cache;
    }
}


/* Search for COUNT newlines between START/START_BYTE and END/END_BYTE.

   If COUNT is positive, search forwards; END must be >= START.
   If COUNT is negative, search backwards for the -COUNTth instance;
      END must be <= START.
   If COUNT is zero, do anything you please; run rogue, for all I care.

   If END is zero, use BEGV or ZV instead, as appropriate for the
   direction indicated by COUNT.

   If we find COUNT instances, set *SHORTAGE to zero, and return the
   position past the COUNTth match.  Note that for reverse motion
   this is not the same as the usual convention for Emacs motion commands.

   If we don't find COUNT instances before reaching END, set *SHORTAGE
   to the number of newlines left unfound, and return END.

   If BYTEPOS is not NULL, set *BYTEPOS to the byte position corresponding
   to the returned character position.

   If ALLOW_QUIT, check for quitting.  That's good to do
   except when inside redisplay.  */

ptrdiff_t
find_newline (ptrdiff_t start, ptrdiff_t start_byte, ptrdiff_t end,
	      ptrdiff_t end_byte, ptrdiff_t count, ptrdiff_t *shortage,
	      ptrdiff_t *bytepos, bool allow_quit)
{
  struct region_cache *newline_cache;
  int direction;
  struct buffer *cache_buffer;

  if (count > 0)
    {
      direction = 1;
      if (!end)
	end = ZV, end_byte = ZV_BYTE;
    }
  else
    {
      direction = -1;
      if (!end)
	end = BEGV, end_byte = BEGV_BYTE;
    }
  if (end_byte == -1)
    end_byte = CHAR_TO_BYTE (end);

  newline_cache = newline_cache_on_off (current_buffer);
  if (current_buffer->base_buffer)
    cache_buffer = current_buffer->base_buffer;
  else
    cache_buffer = current_buffer;

  if (shortage != 0)
    *shortage = 0;

  if (count > 0)
    while (start != end)
      {
        /* Our innermost scanning loop is very simple; it doesn't know
           about gaps, buffer ends, or the newline cache.  ceiling is
           the position of the last character before the next such
           obstacle --- the last character the dumb search loop should
           examine.  */
	ptrdiff_t tem, ceiling_byte = end_byte - 1;

        /* If we're using the newline cache, consult it to see whether
           we can avoid some scanning.  */
        if (newline_cache)
          {
            ptrdiff_t next_change;
	    int result = 1;

            while (start < end && result)
	      {
		ptrdiff_t lim1;

		result = region_cache_forward (cache_buffer, newline_cache,
					       start, &next_change);
		if (result)
		  {
		    /* When the cache revalidation is deferred,
		       next-change might point beyond ZV, which will
		       cause assertion violation in CHAR_TO_BYTE below.
		       Limit next_change to ZV to avoid that.  */
		    if (next_change > ZV)
		      next_change = ZV;
		    start = next_change;
		    lim1 = next_change = end;
		  }
		else
		  lim1 = min (next_change, end);

		/* The cache returned zero for this region; see if
		   this is because the region is known and includes
		   only newlines.  While at that, count any newlines
		   we bump into, and exit if we found enough off them.  */
		start_byte = CHAR_TO_BYTE (start);
		while (start < lim1
		       && FETCH_BYTE (start_byte) == '\n')
		  {
		    start_byte++;
		    start++;
		    if (--count == 0)
		      {
			if (bytepos)
			  *bytepos = start_byte;
			return start;
		      }
		  }
		/* If we found a non-newline character before hitting
		   position where the cache will again return non-zero
		   (i.e. no newlines beyond that position), it means
		   this region is not yet known to the cache, and we
		   must resort to the "dumb loop" method.  */
		if (start < next_change && !result)
		  break;
		result = 1;
	      }
	    if (start >= end)
	      {
		start = end;
		start_byte = end_byte;
		break;
	      }

            /* START should never be after END.  */
            if (start_byte > ceiling_byte)
              start_byte = ceiling_byte;

            /* Now the text after start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling_byte = min (CHAR_TO_BYTE (next_change) - 1, ceiling_byte);
          }
	else if (start_byte == -1)
	  start_byte = CHAR_TO_BYTE (start);

        /* The dumb loop can only scan text stored in contiguous
           bytes. BUFFER_CEILING_OF returns the last character
           position that is contiguous, so the ceiling is the
           position after that.  */
	tem = BUFFER_CEILING_OF (start_byte);
	ceiling_byte = min (tem, ceiling_byte);

        {
          /* The termination address of the dumb loop.  */
	  unsigned char *lim_addr = BYTE_POS_ADDR (ceiling_byte) + 1;
	  ptrdiff_t lim_byte = ceiling_byte + 1;

	  /* Nonpositive offsets (relative to LIM_ADDR and LIM_BYTE)
	     of the base, the cursor, and the next line.  */
	  ptrdiff_t base = start_byte - lim_byte;
	  ptrdiff_t cursor, next;

	  for (cursor = base; cursor < 0; cursor = next)
	    {
              /* The dumb loop.  */
	      unsigned char *nl = memchr (lim_addr + cursor, '\n', - cursor);
	      next = nl ? nl - lim_addr : 0;

              /* If we're using the newline cache, cache the fact that
                 the region we just traversed is free of newlines. */
              if (newline_cache && cursor != next)
		{
		  know_region_cache (cache_buffer, newline_cache,
				     BYTE_TO_CHAR (lim_byte + cursor),
				     BYTE_TO_CHAR (lim_byte + next));
		  /* know_region_cache can relocate buffer text.  */
		  lim_addr = BYTE_POS_ADDR (ceiling_byte) + 1;
		}

              if (! nl)
		break;
	      next++;

	      if (--count == 0)
		{
		  if (bytepos)
		    *bytepos = lim_byte + next;
		  return BYTE_TO_CHAR (lim_byte + next);
		}
	      if (allow_quit)
		maybe_quit ();
            }

	  start_byte = lim_byte;
	  start = BYTE_TO_CHAR (start_byte);
        }
      }
  else
    while (start > end)
      {
        /* The last character to check before the next obstacle.  */
	ptrdiff_t tem, ceiling_byte = end_byte;

        /* Consult the newline cache, if appropriate.  */
        if (newline_cache)
          {
            ptrdiff_t next_change;
	    int result = 1;

            while (start > end && result)
	      {
		ptrdiff_t lim1;

		result = region_cache_backward (cache_buffer, newline_cache,
						start, &next_change);
		if (result)
		  {
		    start = next_change;
		    lim1 = next_change = end;
		  }
		else
		  lim1 = max (next_change, end);
		start_byte = CHAR_TO_BYTE (start);
		while (start > lim1
		       && FETCH_BYTE (start_byte - 1) == '\n')
		  {
		    if (++count == 0)
		      {
			if (bytepos)
			  *bytepos = start_byte;
			return start;
		      }
		    start_byte--;
		    start--;
		  }
		if (start > next_change && !result)
		  break;
		result = 1;
	      }
	    if (start <= end)
	      {
		start = end;
		start_byte = end_byte;
		break;
	      }

            /* Start should never be at or before end.  */
            if (start_byte <= ceiling_byte)
              start_byte = ceiling_byte + 1;

            /* Now the text before start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling_byte = max (CHAR_TO_BYTE (next_change), ceiling_byte);
          }
	else if (start_byte == -1)
	  start_byte = CHAR_TO_BYTE (start);

        /* Stop scanning before the gap.  */
	tem = BUFFER_FLOOR_OF (start_byte - 1);
	ceiling_byte = max (tem, ceiling_byte);

        {
          /* The termination address of the dumb loop.  */
	  unsigned char *ceiling_addr = BYTE_POS_ADDR (ceiling_byte);

	  /* Offsets (relative to CEILING_ADDR and CEILING_BYTE) of
	     the base, the cursor, and the previous line.  These
	     offsets are at least -1.  */
	  ptrdiff_t base = start_byte - ceiling_byte;
	  ptrdiff_t cursor, prev;

	  for (cursor = base; 0 < cursor; cursor = prev)
            {
	      unsigned char *nl = memrchr (ceiling_addr, '\n', cursor);
	      prev = nl ? nl - ceiling_addr : -1;

              /* If we're looking for newlines, cache the fact that
                 this line's region is free of them. */
              if (newline_cache && cursor != prev + 1)
		{
		  know_region_cache (cache_buffer, newline_cache,
				     BYTE_TO_CHAR (ceiling_byte + prev + 1),
				     BYTE_TO_CHAR (ceiling_byte + cursor));
		  /* know_region_cache can relocate buffer text.  */
		  ceiling_addr = BYTE_POS_ADDR (ceiling_byte);
		}

              if (! nl)
		break;

	      if (++count >= 0)
		{
		  if (bytepos)
		    *bytepos = ceiling_byte + prev + 1;
		  return BYTE_TO_CHAR (ceiling_byte + prev + 1);
		}
	      if (allow_quit)
		maybe_quit ();
            }

	  start_byte = ceiling_byte;
	  start = BYTE_TO_CHAR (start_byte);
        }
      }

  if (shortage)
    *shortage = count * direction;
  if (bytepos)
    {
      *bytepos = start_byte == -1 ? CHAR_TO_BYTE (start) : start_byte;
      eassert (*bytepos == CHAR_TO_BYTE (start));
    }
  return start;
}

/* Search for COUNT instances of a line boundary.
   Start at START.  If COUNT is negative, search backwards.

   We report the resulting position by calling TEMP_SET_PT_BOTH.

   If we find COUNT instances. we position after (always after,
   even if scanning backwards) the COUNTth match, and return 0.

   If we don't find COUNT instances before reaching the end of the
   buffer (or the beginning, if scanning backwards), we return
   the number of line boundaries left unfound, and position at
   the limit we bumped up against.

   If ALLOW_QUIT, check for quitting.  That's good to do
   except in special cases.  */

ptrdiff_t
scan_newline (ptrdiff_t start, ptrdiff_t start_byte,
	      ptrdiff_t limit, ptrdiff_t limit_byte,
	      ptrdiff_t count, bool allow_quit)
{
  ptrdiff_t charpos, bytepos, shortage;

  charpos = find_newline (start, start_byte, limit, limit_byte,
			  count, &shortage, &bytepos, allow_quit);
  if (shortage)
    TEMP_SET_PT_BOTH (limit, limit_byte);
  else
    TEMP_SET_PT_BOTH (charpos, bytepos);
  return shortage;
}

/* Like above, but always scan from point and report the
   resulting position in *CHARPOS and *BYTEPOS.  */

ptrdiff_t
scan_newline_from_point (ptrdiff_t count, ptrdiff_t *charpos,
			 ptrdiff_t *bytepos)
{
  ptrdiff_t shortage;

  if (count <= 0)
    *charpos = find_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, count - 1,
			     &shortage, bytepos, 1);
  else
    *charpos = find_newline (PT, PT_BYTE, ZV, ZV_BYTE, count,
			     &shortage, bytepos, 1);
  return shortage;
}

/* Like find_newline, but doesn't allow QUITting and doesn't return
   SHORTAGE.  */
ptrdiff_t
find_newline_no_quit (ptrdiff_t from, ptrdiff_t frombyte,
		      ptrdiff_t cnt, ptrdiff_t *bytepos)
{
  return find_newline (from, frombyte, 0, -1, cnt, NULL, bytepos, 0);
}

/* Like find_newline, but returns position before the newline, not
   after, and only search up to TO.
   This isn't just find_newline_no_quit (...)-1, because you might hit TO.  */

ptrdiff_t
find_before_next_newline (ptrdiff_t from, ptrdiff_t to,
			  ptrdiff_t cnt, ptrdiff_t *bytepos)
{
  ptrdiff_t shortage;
  ptrdiff_t pos = find_newline (from, -1, to, -1, cnt, &shortage, bytepos, 1);

  if (shortage == 0)
    {
      if (bytepos)
	DEC_BOTH (pos, *bytepos);
      else
	pos--;
    }
  return pos;
}

/* Subroutines of Lisp buffer search functions. */

static Lisp_Object
search_command (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror,
		Lisp_Object count, int direction, int RE, bool posix)
{
  EMACS_INT np;
  EMACS_INT lim;
  ptrdiff_t lim_byte;
  EMACS_INT n = direction;

  if (!NILP (count))
    {
      CHECK_NUMBER (count);
      n *= XINT (count);
    }

  CHECK_STRING (string);
  if (NILP (bound))
    {
      if (n > 0)
	lim = ZV, lim_byte = ZV_BYTE;
      else
	lim = BEGV, lim_byte = BEGV_BYTE;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (bound);
      lim = XINT (bound);
      if (n > 0 ? lim < PT : lim > PT)
	error ("Invalid search bound (wrong side of point)");
      if (lim > ZV)
	lim = ZV, lim_byte = ZV_BYTE;
      else if (lim < BEGV)
	lim = BEGV, lim_byte = BEGV_BYTE;
      else
	lim_byte = CHAR_TO_BYTE (lim);
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  set_char_table_extras (BVAR (current_buffer, case_canon_table), 2,
			 BVAR (current_buffer, case_eqv_table));

  np = search_buffer (string, PT, PT_BYTE, lim, lim_byte, n, RE,
		      (!NILP (BVAR (current_buffer, case_fold_search))
		       ? BVAR (current_buffer, case_canon_table)
		       : Qnil),
		      (!NILP (BVAR (current_buffer, case_fold_search))
		       ? BVAR (current_buffer, case_eqv_table)
		       : Qnil),
		      posix);
  if (np <= 0)
    {
      if (NILP (noerror))
	xsignal1 (Qsearch_failed, string);

      if (!EQ (noerror, Qt))
	{
	  eassert (BEGV <= lim && lim <= ZV);
	  SET_PT_BOTH (lim, lim_byte);
	  return Qnil;
#if 0 /* This would be clean, but maybe programs depend on
	 a value of nil here.  */
	  np = lim;
#endif
	}
      else
	return Qnil;
    }

  eassert (BEGV <= np && np <= ZV);
  SET_PT (np);

  return make_number (np);
}

/* Return true if REGEXP it matches just one constant string.  */

static bool
trivial_regexp_p (Lisp_Object regexp)
{
  ptrdiff_t len = SBYTES (regexp);
  unsigned char *s = SDATA (regexp);
  while (--len >= 0)
    {
      switch (*s++)
	{
	case '.': case '*': case '+': case '?': case '[': case '^': case '$':
	  return 0;
	case '\\':
	  if (--len < 0)
	    return 0;
	  switch (*s++)
	    {
	    case '|': case '(': case ')': case '`': case '\'': case 'b':
	    case 'B': case '<': case '>': case 'w': case 'W': case 's':
	    case 'S': case '=': case '{': case '}': case '_':
	    case 'c': case 'C':	/* for categoryspec and notcategoryspec */
	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      return 0;
	    }
	}
    }
  return 1;
}

/* Search for the n'th occurrence of STRING in the current buffer,
   starting at position POS and stopping at position LIM,
   treating STRING as a literal string if RE is false or as
   a regular expression if RE is true.

   If N is positive, searching is forward and LIM must be greater than POS.
   If N is negative, searching is backward and LIM must be less than POS.

   Returns -x if x occurrences remain to be found (x > 0),
   or else the position at the beginning of the Nth occurrence
   (if searching backward) or the end (if searching forward).

   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

#define TRANSLATE(out, trt, d)			\
do						\
  {						\
    if (! NILP (trt))				\
      {						\
	Lisp_Object temp;			\
	temp = Faref (trt, make_number (d));	\
	if (INTEGERP (temp))			\
	  out = XINT (temp);			\
	else					\
	  out = d;				\
      }						\
    else					\
      out = d;					\
  }						\
while (0)

/* Only used in search_buffer, to record the end position of the match
   when searching regexps and SEARCH_REGS should not be changed
   (i.e. Vinhibit_changing_match_data is non-nil).  */
static struct re_registers search_regs_1;

static EMACS_INT
search_buffer (Lisp_Object string, ptrdiff_t pos, ptrdiff_t pos_byte,
	       ptrdiff_t lim, ptrdiff_t lim_byte, EMACS_INT n,
	       int RE, Lisp_Object trt, Lisp_Object inverse_trt, bool posix)
{
  ptrdiff_t len = SCHARS (string);
  ptrdiff_t len_byte = SBYTES (string);
  register ptrdiff_t i;

  if (running_asynch_code)
    save_search_regs ();

  /* Searching 0 times means don't move.  */
  /* Null string is found at starting position.  */
  if (len == 0 || n == 0)
    {
      set_search_regs (pos_byte, 0);
      return pos;
    }

  if (RE && !(trivial_regexp_p (string) && NILP (Vsearch_spaces_regexp)))
    {
      unsigned char *p1, *p2;
      ptrdiff_t s1, s2;
      struct re_pattern_buffer *bufp;

      bufp = compile_pattern (string,
			      (NILP (Vinhibit_changing_match_data)
			       ? &search_regs : &search_regs_1),
			      trt, posix,
			      !NILP (BVAR (current_buffer, enable_multibyte_characters)));

      maybe_quit ();		/* Do a pending quit right away,
				   to avoid paradoxical behavior */
      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BEGV_ADDR;
      s1 = GPT_BYTE - BEGV_BYTE;
      p2 = GAP_END_ADDR;
      s2 = ZV_BYTE - GPT_BYTE;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV_BYTE - BEGV_BYTE;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV_BYTE - BEGV_BYTE;
	  s2 = 0;
	}
      re_match_object = Qnil;

      freeze_buffer_relocation ();

      while (n < 0)
	{
	  ptrdiff_t val;

	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos_byte - BEGV_BYTE, lim_byte - pos_byte,
			     (NILP (Vinhibit_changing_match_data)
			      ? &search_regs : &search_regs_1),
			     /* Don't allow match past current point */
			     pos_byte - BEGV_BYTE);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      if (NILP (Vinhibit_changing_match_data))
		{
		  pos_byte = search_regs.start[0] + BEGV_BYTE;
		  for (i = 0; i < search_regs.num_regs; i++)
		    if (search_regs.start[i] >= 0)
		      {
			search_regs.start[i]
			  = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
			search_regs.end[i]
			  = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
		      }
		  XSETBUFFER (last_thing_searched, current_buffer);
		  /* Set pos to the new position. */
		  pos = search_regs.start[0];
		}
	      else
		{
		  pos_byte = search_regs_1.start[0] + BEGV_BYTE;
		  /* Set pos to the new position.  */
		  pos = BYTE_TO_CHAR (search_regs_1.start[0] + BEGV_BYTE);
		}
	    }
	  else
	    {
	      thaw_buffer_relocation ();
	      return (n);
	    }
	  n++;
	  maybe_quit ();
	}
      while (n > 0)
	{
	  ptrdiff_t val;

	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos_byte - BEGV_BYTE, lim_byte - pos_byte,
			     (NILP (Vinhibit_changing_match_data)
			      ? &search_regs : &search_regs_1),
			     lim_byte - BEGV_BYTE);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      if (NILP (Vinhibit_changing_match_data))
		{
		  pos_byte = search_regs.end[0] + BEGV_BYTE;
		  for (i = 0; i < search_regs.num_regs; i++)
		    if (search_regs.start[i] >= 0)
		      {
			search_regs.start[i]
			  = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
			search_regs.end[i]
			  = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
		      }
		  XSETBUFFER (last_thing_searched, current_buffer);
		  pos = search_regs.end[0];
		}
	      else
		{
		  pos_byte = search_regs_1.end[0] + BEGV_BYTE;
		  pos = BYTE_TO_CHAR (search_regs_1.end[0] + BEGV_BYTE);
		}
	    }
	  else
	    {
	      thaw_buffer_relocation ();
	      return (0 - n);
	    }
	  n--;
	  maybe_quit ();
	}
      thaw_buffer_relocation ();
      return (pos);
    }
  else				/* non-RE case */
    {
      unsigned char *raw_pattern, *pat;
      ptrdiff_t raw_pattern_size;
      ptrdiff_t raw_pattern_size_byte;
      unsigned char *patbuf;
      bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
      unsigned char *base_pat;
      /* Set to positive if we find a non-ASCII char that need
	 translation.  Otherwise set to zero later.  */
      int char_base = -1;
      bool boyer_moore_ok = 1;
      USE_SAFE_ALLOCA;

      /* MULTIBYTE says whether the text to be searched is multibyte.
	 We must convert PATTERN to match that, or we will not really
	 find things right.  */

      if (multibyte == STRING_MULTIBYTE (string))
	{
	  raw_pattern = SDATA (string);
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte = SBYTES (string);
	}
      else if (multibyte)
	{
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte
	    = count_size_as_multibyte (SDATA (string),
				       raw_pattern_size);
	  raw_pattern = SAFE_ALLOCA (raw_pattern_size_byte + 1);
	  copy_text (SDATA (string), raw_pattern,
		     SCHARS (string), 0, 1);
	}
      else
	{
	  /* Converting multibyte to single-byte.

	     ??? Perhaps this conversion should be done in a special way
	     by subtracting nonascii-insert-offset from each non-ASCII char,
	     so that only the multibyte chars which really correspond to
	     the chosen single-byte character set can possibly match.  */
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte = SCHARS (string);
	  raw_pattern = SAFE_ALLOCA (raw_pattern_size + 1);
	  copy_text (SDATA (string), raw_pattern,
		     SBYTES (string), 1, 0);
	}

      /* Copy and optionally translate the pattern.  */
      len = raw_pattern_size;
      len_byte = raw_pattern_size_byte;
      SAFE_NALLOCA (patbuf, MAX_MULTIBYTE_LENGTH, len);
      pat = patbuf;
      base_pat = raw_pattern;
      if (multibyte)
	{
	  /* Fill patbuf by translated characters in STRING while
	     checking if we can use boyer-moore search.  If TRT is
	     non-nil, we can use boyer-moore search only if TRT can be
	     represented by the byte array of 256 elements.  For that,
	     all non-ASCII case-equivalents of all case-sensitive
	     characters in STRING must belong to the same character
	     group (two characters belong to the same group iff their
	     multibyte forms are the same except for the last byte;
	     i.e. every 64 characters form a group; U+0000..U+003F,
	     U+0040..U+007F, U+0080..U+00BF, ...).  */

	  while (--len >= 0)
	    {
	      unsigned char str_base[MAX_MULTIBYTE_LENGTH], *str;
	      int c, translated, inverse;
	      int in_charlen, charlen;

	      /* If we got here and the RE flag is set, it's because we're
		 dealing with a regexp known to be trivial, so the backslash
		 just quotes the next character.  */
	      if (RE && *base_pat == '\\')
		{
		  len--;
		  raw_pattern_size--;
		  len_byte--;
		  base_pat++;
		}

	      c = STRING_CHAR_AND_LENGTH (base_pat, in_charlen);

	      if (NILP (trt))
		{
		  str = base_pat;
		  charlen = in_charlen;
		}
	      else
		{
		  /* Translate the character.  */
		  TRANSLATE (translated, trt, c);
		  charlen = CHAR_STRING (translated, str_base);
		  str = str_base;

		  /* Check if C has any other case-equivalents.  */
		  TRANSLATE (inverse, inverse_trt, c);
		  /* If so, check if we can use boyer-moore.  */
		  if (c != inverse && boyer_moore_ok)
		    {
		      /* Check if all equivalents belong to the same
			 group of characters.  Note that the check of C
			 itself is done by the last iteration.  */
		      int this_char_base = -1;

		      while (boyer_moore_ok)
			{
			  if (ASCII_CHAR_P (inverse))
			    {
			      if (this_char_base > 0)
				boyer_moore_ok = 0;
			      else
				this_char_base = 0;
			    }
			  else if (CHAR_BYTE8_P (inverse))
			    /* Boyer-moore search can't handle a
			       translation of an eight-bit
			       character.  */
			    boyer_moore_ok = 0;
			  else if (this_char_base < 0)
			    {
			      this_char_base = inverse & ~0x3F;
			      if (char_base < 0)
				char_base = this_char_base;
			      else if (this_char_base != char_base)
				boyer_moore_ok = 0;
			    }
			  else if ((inverse & ~0x3F) != this_char_base)
			    boyer_moore_ok = 0;
			  if (c == inverse)
			    break;
			  TRANSLATE (inverse, inverse_trt, inverse);
			}
		    }
		}

	      /* Store this character into the translated pattern.  */
	      memcpy (pat, str, charlen);
	      pat += charlen;
	      base_pat += in_charlen;
	      len_byte -= in_charlen;
	    }

	  /* If char_base is still negative we didn't find any translated
	     non-ASCII characters.  */
	  if (char_base < 0)
	    char_base = 0;
	}
      else
	{
	  /* Unibyte buffer.  */
	  char_base = 0;
	  while (--len >= 0)
	    {
	      int c, translated, inverse;

	      /* If we got here and the RE flag is set, it's because we're
		 dealing with a regexp known to be trivial, so the backslash
		 just quotes the next character.  */
	      if (RE && *base_pat == '\\')
		{
		  len--;
		  raw_pattern_size--;
		  base_pat++;
		}
	      c = *base_pat++;
	      TRANSLATE (translated, trt, c);
	      *pat++ = translated;
	      /* Check that none of C's equivalents violates the
		 assumptions of boyer_moore.  */
	      TRANSLATE (inverse, inverse_trt, c);
	      while (1)
		{
		  if (inverse >= 0200)
		    {
		      boyer_moore_ok = 0;
		      break;
		    }
		  if (c == inverse)
		    break;
		  TRANSLATE (inverse, inverse_trt, inverse);
		}
	    }
	}

      len_byte = pat - patbuf;
      pat = base_pat = patbuf;

      EMACS_INT result
	= (boyer_moore_ok
	   ? boyer_moore (n, pat, len_byte, trt, inverse_trt,
			  pos_byte, lim_byte,
			  char_base)
	   : simple_search (n, pat, raw_pattern_size, len_byte, trt,
			    pos, pos_byte, lim, lim_byte));
      SAFE_FREE ();
      return result;
    }
}

/* Do a simple string search N times for the string PAT,
   whose length is LEN/LEN_BYTE,
   from buffer position POS/POS_BYTE until LIM/LIM_BYTE.
   TRT is the translation table.

   Return the character position where the match is found.
   Otherwise, if M matches remained to be found, return -M.

   This kind of search works regardless of what is in PAT and
   regardless of what is in TRT.  It is used in cases where
   boyer_moore cannot work.  */

static EMACS_INT
simple_search (EMACS_INT n, unsigned char *pat,
	       ptrdiff_t len, ptrdiff_t len_byte, Lisp_Object trt,
	       ptrdiff_t pos, ptrdiff_t pos_byte,
	       ptrdiff_t lim, ptrdiff_t lim_byte)
{
  bool multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
  bool forward = n > 0;
  /* Number of buffer bytes matched.  Note that this may be different
     from len_byte in a multibyte buffer.  */
  ptrdiff_t match_byte = PTRDIFF_MIN;

  if (lim > pos && multibyte)
    while (n > 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    ptrdiff_t this_pos = pos;
	    ptrdiff_t this_pos_byte = pos_byte;
	    ptrdiff_t this_len = len;
	    unsigned char *p = pat;
	    if (pos + len > lim || pos_byte + len_byte > lim_byte)
	      goto stop;

	    while (this_len > 0)
	      {
		int charlen, buf_charlen;
		int pat_ch, buf_ch;

		pat_ch = STRING_CHAR_AND_LENGTH (p, charlen);
		buf_ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (this_pos_byte),
						 buf_charlen);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
		p += charlen;

		this_pos_byte += buf_charlen;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = this_pos_byte - pos_byte;
		pos += len;
		pos_byte += match_byte;
		break;
	      }

	    INC_BOTH (pos, pos_byte);
	  }

	n--;
      }
  else if (lim > pos)
    while (n > 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    ptrdiff_t this_pos = pos;
	    ptrdiff_t this_len = len;
	    unsigned char *p = pat;

	    if (pos + len > lim)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch = *p++;
		int buf_ch = FETCH_BYTE (this_pos);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = len;
		pos += len;
		break;
	      }

	    pos++;
	  }

	n--;
      }
  /* Backwards search.  */
  else if (lim < pos && multibyte)
    while (n < 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    ptrdiff_t this_pos = pos;
	    ptrdiff_t this_pos_byte = pos_byte;
	    ptrdiff_t this_len = len;
	    const unsigned char *p = pat + len_byte;

	    if (this_pos - len < lim || (pos_byte - len_byte) < lim_byte)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch, buf_ch;

		DEC_BOTH (this_pos, this_pos_byte);
		PREV_CHAR_BOUNDARY (p, pat);
		pat_ch = STRING_CHAR (p);
		buf_ch = STRING_CHAR (BYTE_POS_ADDR (this_pos_byte));
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
	      }

	    if (this_len == 0)
	      {
		match_byte = pos_byte - this_pos_byte;
		pos = this_pos;
		pos_byte = this_pos_byte;
		break;
	      }

	    DEC_BOTH (pos, pos_byte);
	  }

	n++;
      }
  else if (lim < pos)
    while (n < 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    ptrdiff_t this_pos = pos - len;
	    ptrdiff_t this_len = len;
	    unsigned char *p = pat;

	    if (this_pos < lim)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch = *p++;
		int buf_ch = FETCH_BYTE (this_pos);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;
		this_len--;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = len;
		pos -= len;
		break;
	      }

	    pos--;
	  }

	n++;
      }

 stop:
  if (n == 0)
    {
      eassert (match_byte != PTRDIFF_MIN);
      if (forward)
	set_search_regs ((multibyte ? pos_byte : pos) - match_byte, match_byte);
      else
	set_search_regs (multibyte ? pos_byte : pos, match_byte);

      return pos;
    }
  else if (n > 0)
    return -n;
  else
    return n;
}

/* Do Boyer-Moore search N times for the string BASE_PAT,
   whose length is LEN_BYTE,
   from buffer position POS_BYTE until LIM_BYTE.
   DIRECTION says which direction we search in.
   TRT and INVERSE_TRT are translation tables.
   Characters in PAT are already translated by TRT.

   This kind of search works if all the characters in BASE_PAT that
   have nontrivial translation are the same aside from the last byte.
   This makes it possible to translate just the last byte of a
   character, and do so after just a simple test of the context.
   CHAR_BASE is nonzero if there is such a non-ASCII character.

   If that criterion is not satisfied, do not call this function.  */

static EMACS_INT
boyer_moore (EMACS_INT n, unsigned char *base_pat,
	     ptrdiff_t len_byte,
	     Lisp_Object trt, Lisp_Object inverse_trt,
	     ptrdiff_t pos_byte, ptrdiff_t lim_byte,
             int char_base)
{
  int direction = ((n > 0) ? 1 : -1);
  register ptrdiff_t dirlen;
  ptrdiff_t limit;
  int stride_for_teases = 0;
  int BM_tab[0400];
  register unsigned char *cursor, *p_limit;
  register ptrdiff_t i;
  register int j;
  unsigned char *pat, *pat_end;
  bool multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  unsigned char simple_translate[0400];
  /* These are set to the preceding bytes of a byte to be translated
     if char_base is nonzero.  As the maximum byte length of a
     multibyte character is 5, we have to check at most four previous
     bytes.  */
  int translate_prev_byte1 = 0;
  int translate_prev_byte2 = 0;
  int translate_prev_byte3 = 0;

  /* The general approach is that we are going to maintain that we know
     the first (closest to the present position, in whatever direction
     we're searching) character that could possibly be the last
     (furthest from present position) character of a valid match.  We
     advance the state of our knowledge by looking at that character
     and seeing whether it indeed matches the last character of the
     pattern.  If it does, we take a closer look.  If it does not, we
     move our pointer (to putative last characters) as far as is
     logically possible.  This amount of movement, which I call a
     stride, will be the length of the pattern if the actual character
     appears nowhere in the pattern, otherwise it will be the distance
     from the last occurrence of that character to the end of the
     pattern.  If the amount is zero we have a possible match.  */

  /* Here we make a "mickey mouse" BM table.  The stride of the search
     is determined only by the last character of the putative match.
     If that character does not match, we will stride the proper
     distance to propose a match that superimposes it on the last
     instance of a character that matches it (per trt), or misses
     it entirely if there is none. */

  dirlen = len_byte * direction;

  /* Record position after the end of the pattern.  */
  pat_end = base_pat + len_byte;
  /* BASE_PAT points to a character that we start scanning from.
     It is the first character in a forward search,
     the last character in a backward search.  */
  if (direction < 0)
    base_pat = pat_end - 1;

  /* A character that does not appear in the pattern induces a
     stride equal to the pattern length.  */
  for (i = 0; i < 0400; i++)
    BM_tab[i] = dirlen;

  /* We use this for translation, instead of TRT itself.
     We fill this in to handle the characters that actually
     occur in the pattern.  Others don't matter anyway!  */
  for (i = 0; i < 0400; i++)
    simple_translate[i] = i;

  if (char_base)
    {
      /* Setup translate_prev_byte1/2/3/4 from CHAR_BASE.  Only a
	 byte following them are the target of translation.  */
      eassume (0x80 <= char_base && char_base <= MAX_CHAR);
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int cblen = CHAR_STRING (char_base, str);

      translate_prev_byte1 = str[cblen - 2];
      if (cblen > 2)
	{
	  translate_prev_byte2 = str[cblen - 3];
	  if (cblen > 3)
	    translate_prev_byte3 = str[cblen - 4];
	}
    }

  i = 0;
  while (i != dirlen)
    {
      unsigned char *ptr = base_pat + i;
      i += direction;
      if (! NILP (trt))
	{
	  /* If the byte currently looking at is the last of a
	     character to check case-equivalents, set CH to that
	     character.  An ASCII character and a non-ASCII character
	     matching with CHAR_BASE are to be checked.  */
	  int ch = -1;

	  if (ASCII_CHAR_P (*ptr) || ! multibyte)
	    ch = *ptr;
	  else if (char_base
		   && ((pat_end - ptr) == 1 || CHAR_HEAD_P (ptr[1])))
	    {
	      unsigned char *charstart = ptr - 1;

	      while (! (CHAR_HEAD_P (*charstart)))
		charstart--;
	      ch = STRING_CHAR (charstart);
	      if (char_base != (ch & ~0x3F))
		ch = -1;
	    }

	  if (ch >= 0200 && multibyte)
	    j = (ch & 0x3F) | 0200;
	  else
	    j = *ptr;

	  if (i == dirlen)
	    stride_for_teases = BM_tab[j];

	  BM_tab[j] = dirlen - i;
	  /* A translation table is accompanied by its inverse -- see
	     comment following downcase_table for details.  */
	  if (ch >= 0)
	    {
	      int starting_ch = ch;
	      int starting_j = j;

	      while (1)
		{
		  TRANSLATE (ch, inverse_trt, ch);
		  if (ch >= 0200 && multibyte)
		    j = (ch & 0x3F) | 0200;
		  else
		    j = ch;

		  /* For all the characters that map into CH,
		     set up simple_translate to map the last byte
		     into STARTING_J.  */
		  simple_translate[j] = starting_j;
		  if (ch == starting_ch)
		    break;
		  BM_tab[j] = dirlen - i;
		}
	    }
	}
      else
	{
	  j = *ptr;

	  if (i == dirlen)
	    stride_for_teases = BM_tab[j];
	  BM_tab[j] = dirlen - i;
	}
      /* stride_for_teases tells how much to stride if we get a
	 match on the far character but are subsequently
	 disappointed, by recording what the stride would have been
	 for that character if the last character had been
	 different.  */
    }
  pos_byte += dirlen - ((direction > 0) ? direction : 0);
  /* loop invariant - POS_BYTE points at where last char (first
     char if reverse) of pattern would align in a possible match.  */
  while (n != 0)
    {
      ptrdiff_t tail_end;
      unsigned char *tail_end_ptr;

      /* It's been reported that some (broken) compiler thinks that
	 Boolean expressions in an arithmetic context are unsigned.
	 Using an explicit ?1:0 prevents this.  */
      if ((lim_byte - pos_byte - ((direction > 0) ? 1 : 0)) * direction
	  < 0)
	return (n * (0 - direction));
      /* First we do the part we can by pointers (maybe nothing) */
      maybe_quit ();
      pat = base_pat;
      limit = pos_byte - dirlen + direction;
      if (direction > 0)
	{
	  limit = BUFFER_CEILING_OF (limit);
	  /* LIMIT is now the last (not beyond-last!) value POS_BYTE
	     can take on without hitting edge of buffer or the gap.  */
	  limit = min (limit, pos_byte + 20000);
	  limit = min (limit, lim_byte - 1);
	}
      else
	{
	  limit = BUFFER_FLOOR_OF (limit);
	  /* LIMIT is now the last (not beyond-last!) value POS_BYTE
	     can take on without hitting edge of buffer or the gap.  */
	  limit = max (limit, pos_byte - 20000);
	  limit = max (limit, lim_byte);
	}
      tail_end = BUFFER_CEILING_OF (pos_byte) + 1;
      tail_end_ptr = BYTE_POS_ADDR (tail_end);

      if ((limit - pos_byte) * direction > 20)
	{
	  unsigned char *p2;

	  p_limit = BYTE_POS_ADDR (limit);
	  p2 = (cursor = BYTE_POS_ADDR (pos_byte));
	  /* In this loop, pos + cursor - p2 is the surrogate for pos.  */
	  while (1)		/* use one cursor setting as long as i can */
	    {
	      if (direction > 0) /* worth duplicating */
		{
		  while (cursor <= p_limit)
		    {
		      if (BM_tab[*cursor] == 0)
			goto hit;
		      cursor += BM_tab[*cursor];
		    }
		}
	      else
		{
		  while (cursor >= p_limit)
		    {
		      if (BM_tab[*cursor] == 0)
			goto hit;
		      cursor += BM_tab[*cursor];
		    }
		}
	      /* If you are here, cursor is beyond the end of the
		 searched region.  You fail to match within the
		 permitted region and would otherwise try a character
		 beyond that region.  */
	      break;

	    hit:
	      i = dirlen - direction;
	      if (! NILP (trt))
		{
		  while ((i -= direction) + direction != 0)
		    {
		      int ch;
		      cursor -= direction;
		      /* Translate only the last byte of a character.  */
		      if (! multibyte
			  || ((cursor == tail_end_ptr
			       || CHAR_HEAD_P (cursor[1]))
			      && (CHAR_HEAD_P (cursor[0])
				  /* Check if this is the last byte of
				     a translatable character.  */
				  || (translate_prev_byte1 == cursor[-1]
				      && (CHAR_HEAD_P (translate_prev_byte1)
					  || (translate_prev_byte2 == cursor[-2]
					      && (CHAR_HEAD_P (translate_prev_byte2)
						  || (translate_prev_byte3 == cursor[-3]))))))))
			ch = simple_translate[*cursor];
		      else
			ch = *cursor;
		      if (pat[i] != ch)
			break;
		    }
		}
	      else
		{
		  while ((i -= direction) + direction != 0)
		    {
		      cursor -= direction;
		      if (pat[i] != *cursor)
			break;
		    }
		}
	      cursor += dirlen - i - direction;	/* fix cursor */
	      if (i + direction == 0)
		{
		  ptrdiff_t position, start, end;
#ifdef REL_ALLOC
		  ptrdiff_t cursor_off;
#endif

		  cursor -= direction;

		  position = pos_byte + cursor - p2 + ((direction > 0)
						       ? 1 - len_byte : 0);
#ifdef REL_ALLOC
		  /* set_search_regs might call malloc, which could
		     cause ralloc.c relocate buffer text.  We need to
		     update pointers into buffer text due to that.  */
		  cursor_off = cursor - p2;
#endif
		  set_search_regs (position, len_byte);
#ifdef REL_ALLOC
		  p_limit = BYTE_POS_ADDR (limit);
		  p2 = BYTE_POS_ADDR (pos_byte);
		  cursor = p2 + cursor_off;
#endif

		  if (NILP (Vinhibit_changing_match_data))
		    {
		      start = search_regs.start[0];
		      end = search_regs.end[0];
		    }
		  else
		    /* If Vinhibit_changing_match_data is non-nil,
		       search_regs will not be changed.  So let's
		       compute start and end here.  */
		    {
		      start = BYTE_TO_CHAR (position);
		      end = BYTE_TO_CHAR (position + len_byte);
		    }

		  if ((n -= direction) != 0)
		    cursor += dirlen; /* to resume search */
		  else
		    return direction > 0 ? end : start;
		}
	      else
		cursor += stride_for_teases; /* <sigh> we lose -  */
	    }
	  pos_byte += cursor - p2;
	}
      else
	/* Now we'll pick up a clump that has to be done the hard
	   way because it covers a discontinuity.  */
	{
	  limit = ((direction > 0)
		   ? BUFFER_CEILING_OF (pos_byte - dirlen + 1)
		   : BUFFER_FLOOR_OF (pos_byte - dirlen - 1));
	  limit = ((direction > 0)
		   ? min (limit + len_byte, lim_byte - 1)
		   : max (limit - len_byte, lim_byte));
	  /* LIMIT is now the last value POS_BYTE can have
	     and still be valid for a possible match.  */
	  while (1)
	    {
	      /* This loop can be coded for space rather than
		 speed because it will usually run only once.
		 (the reach is at most len + 21, and typically
		 does not exceed len).  */
	      while ((limit - pos_byte) * direction >= 0)
		{
		  int ch = FETCH_BYTE (pos_byte);
		  if (BM_tab[ch] == 0)
		    goto hit2;
		  pos_byte += BM_tab[ch];
		}
	      break;	/* ran off the end */

	    hit2:
	      /* Found what might be a match.  */
	      i = dirlen - direction;
	      while ((i -= direction) + direction != 0)
		{
		  int ch;
		  unsigned char *ptr;
		  pos_byte -= direction;
		  ptr = BYTE_POS_ADDR (pos_byte);
		  /* Translate only the last byte of a character.  */
		  if (! multibyte
		      || ((ptr == tail_end_ptr
			   || CHAR_HEAD_P (ptr[1]))
			  && (CHAR_HEAD_P (ptr[0])
			      /* Check if this is the last byte of a
				 translatable character.  */
			      || (translate_prev_byte1 == ptr[-1]
				  && (CHAR_HEAD_P (translate_prev_byte1)
				      || (translate_prev_byte2 == ptr[-2]
					  && (CHAR_HEAD_P (translate_prev_byte2)
					      || translate_prev_byte3 == ptr[-3])))))))
		    ch = simple_translate[*ptr];
		  else
		    ch = *ptr;
		  if (pat[i] != ch)
		    break;
		}
	      /* Above loop has moved POS_BYTE part or all the way
		 back to the first pos (last pos if reverse).
		 Set it once again at the last (first if reverse) char.  */
	      pos_byte += dirlen - i - direction;
	      if (i + direction == 0)
		{
		  ptrdiff_t position, start, end;
		  pos_byte -= direction;

		  position = pos_byte + ((direction > 0) ? 1 - len_byte : 0);
		  set_search_regs (position, len_byte);

		  if (NILP (Vinhibit_changing_match_data))
		    {
		      start = search_regs.start[0];
		      end = search_regs.end[0];
		    }
		  else
		    /* If Vinhibit_changing_match_data is non-nil,
		       search_regs will not be changed.  So let's
		       compute start and end here.  */
		    {
		      start = BYTE_TO_CHAR (position);
		      end = BYTE_TO_CHAR (position + len_byte);
		    }

		  if ((n -= direction) != 0)
		    pos_byte += dirlen; /* to resume search */
		  else
		    return direction > 0 ? end : start;
		}
	      else
		pos_byte += stride_for_teases;
	    }
	  }
      /* We have done one clump.  Can we continue? */
      if ((lim_byte - pos_byte) * direction < 0)
	return ((0 - n) * direction);
    }
  return BYTE_TO_CHAR (pos_byte);
}

/* Record beginning BEG_BYTE and end BEG_BYTE + NBYTES
   for the overall match just found in the current buffer.
   Also clear out the match data for registers 1 and up.  */

static void
set_search_regs (ptrdiff_t beg_byte, ptrdiff_t nbytes)
{
  ptrdiff_t i;

  if (!NILP (Vinhibit_changing_match_data))
    return;

  /* Make sure we have registers in which to store
     the match position.  */
  if (search_regs.num_regs == 0)
    {
      search_regs.start = xmalloc (2 * sizeof (regoff_t));
      search_regs.end = xmalloc (2 * sizeof (regoff_t));
      search_regs.num_regs = 2;
    }

  /* Clear out the other registers.  */
  for (i = 1; i < search_regs.num_regs; i++)
    {
      search_regs.start[i] = -1;
      search_regs.end[i] = -1;
    }

  search_regs.start[0] = BYTE_TO_CHAR (beg_byte);
  search_regs.end[0] = BYTE_TO_CHAR (beg_byte + nbytes);
  XSETBUFFER (last_thing_searched, current_buffer);
}

DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4,
       "MSearch backward: ",
       doc: /* Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not begin before that position.  A value of nil
  means search to the beginning of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, position at limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  forward, instead of backward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth to last one (or
  last, if COUNT is 1 or nil) in the buffer located entirely before
  the origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (string, bound, noerror, count, -1, 0, 0);
}

DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "MSearch: ",
       doc: /* Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not end after that position.  A value of nil
  means search to the end of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  backward, instead of forward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth one (or first,
  if COUNT is 1 or nil) in the buffer located entirely after the
  origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (string, bound, noerror, count, 1, 0, 0);
}

DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4,
       "sRE search backward: ",
       doc: /* Search backward from point for regular expression REGEXP.
This function is almost identical to `re-search-forward', except that
by default it searches backward instead of forward, and the sign of
COUNT also indicates exactly the opposite searching direction.

See `re-search-forward' for details.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, -1, 1, 0);
}

DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
       "sRE search: ",
       doc: /* Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional third argument NOERROR indicates how errors are handled
  when the search fails.  If it is nil or omitted, emit an error; if
  it is t, simply return nil and do nothing; if it is neither nil nor
  t, move to the limit of search and return nil.
The optional fourth argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for.  If it
  is positive, search forward for COUNT successive occurrences; if it
  is negative, search backward, instead of forward, for -COUNT
  occurrences.  A value of nil means the same as 1.
With COUNT positive/negative, the match found is the COUNTth/-COUNTth
  one in the buffer located entirely after/before the origin of the
  search.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, 1, 1, 0);
}

DEFUN ("posix-search-backward", Fposix_search_backward, Sposix_search_backward, 1, 4,
       "sPosix search backward: ",
       doc: /* Search backward from point for match for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not begin before that position.  A value of nil
  means search to the beginning of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, position at limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  forward, instead of backward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth to last one (or
  last, if COUNT is 1 or nil) in the buffer located entirely before
  the origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, -1, 1, 1);
}

DEFUN ("posix-search-forward", Fposix_search_forward, Sposix_search_forward, 1, 4,
       "sPosix search: ",
       doc: /* Search forward from point for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not end after that position.  A value of nil
  means search to the end of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  backward, instead of forward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth one (or first,
  if COUNT is 1 or nil) in the buffer located entirely after the
  origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, 1, 1, 1);
}

DEFUN ("replace-match", Freplace_match, Sreplace_match, 1, 5, 0,
       doc: /* Replace text matched by last search with NEWTEXT.
Leave point at the end of the replacement text.

If optional second arg FIXEDCASE is non-nil, do not alter the case of
the replacement text.  Otherwise, maybe capitalize the whole text, or
maybe just word initials, based on the replaced text.  If the replaced
text has only capital letters and has at least one multiletter word,
convert NEWTEXT to all caps.  Otherwise if all words are capitalized
in the replaced text, capitalize each word in NEWTEXT.

If optional third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\\' as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\?' is treated literally
       (for compatibility with `query-replace-regexp').
  Any other character following `\\' signals an error.
Case conversion does not apply to these substitutions.

If optional fourth argument STRING is non-nil, it should be a string
to act on; this should be the string on which the previous match was
done via `string-match'.  In this case, `replace-match' creates and
returns a new string, made by copying STRING and replacing the part of
STRING that was matched (the original STRING itself is not altered).

The optional fifth argument SUBEXP specifies a subexpression;
it says to replace just that subexpression with NEWTEXT,
rather than replacing the entire matched text.
This is, in a vague sense, the inverse of using `\\N' in NEWTEXT;
`\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
NEWTEXT in place of subexp N.
This is useful only after a regular expression search or match,
since only regular expressions have distinguished subexpressions.  */)
  (Lisp_Object newtext, Lisp_Object fixedcase, Lisp_Object literal, Lisp_Object string, Lisp_Object subexp)
{
  enum { nochange, all_caps, cap_initial } case_action;
  ptrdiff_t pos, pos_byte;
  bool some_multiletter_word;
  bool some_lowercase;
  bool some_uppercase;
  bool some_nonuppercase_initial;
  int c, prevc;
  ptrdiff_t sub;
  ptrdiff_t opoint, newpoint;

  CHECK_STRING (newtext);

  if (! NILP (string))
    CHECK_STRING (string);

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */

  if (search_regs.num_regs <= 0)
    error ("`replace-match' called before any match found");

  if (NILP (subexp))
    sub = 0;
  else
    {
      CHECK_NUMBER (subexp);
      if (! (0 <= XINT (subexp) && XINT (subexp) < search_regs.num_regs))
	args_out_of_range (subexp, make_number (search_regs.num_regs));
      sub = XINT (subexp);
    }

  if (NILP (string))
    {
      if (search_regs.start[sub] < BEGV
	  || search_regs.start[sub] > search_regs.end[sub]
	  || search_regs.end[sub] > ZV)
	args_out_of_range (make_number (search_regs.start[sub]),
			   make_number (search_regs.end[sub]));
    }
  else
    {
      if (search_regs.start[sub] < 0
	  || search_regs.start[sub] > search_regs.end[sub]
	  || search_regs.end[sub] > SCHARS (string))
	args_out_of_range (make_number (search_regs.start[sub]),
			   make_number (search_regs.end[sub]));
    }

  if (NILP (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */
      ptrdiff_t last;

      pos = search_regs.start[sub];
      last = search_regs.end[sub];

      if (NILP (string))
	pos_byte = CHAR_TO_BYTE (pos);
      else
	pos_byte = string_char_to_byte (string, pos);

      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;
      some_lowercase = 0;
      some_nonuppercase_initial = 0;
      some_uppercase = 0;

      while (pos < last)
	{
	  if (NILP (string))
	    {
	      c = FETCH_CHAR_AS_MULTIBYTE (pos_byte);
	      INC_BOTH (pos, pos_byte);
	    }
	  else
	    FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c, string, pos, pos_byte);

	  if (lowercasep (c))
	    {
	      /* Cannot be all caps if any original char is lower case */

	      some_lowercase = 1;
	      if (SYNTAX (prevc) != Sword)
		some_nonuppercase_initial = 1;
	      else
		some_multiletter_word = 1;
	    }
	  else if (uppercasep (c))
	    {
	      some_uppercase = 1;
	      if (SYNTAX (prevc) != Sword)
		;
	      else
		some_multiletter_word = 1;
	    }
	  else
	    {
	      /* If the initial is a caseless word constituent,
		 treat that like a lowercase initial.  */
	      if (SYNTAX (prevc) != Sword)
		some_nonuppercase_initial = 1;
	    }

	  prevc = c;
	}

      /* Convert to all caps if the old text is all caps
	 and has at least one multiletter word.  */
      if (! some_lowercase && some_multiletter_word)
	case_action = all_caps;
      /* Capitalize each word, if the old text has all capitalized words.  */
      else if (!some_nonuppercase_initial && some_multiletter_word)
	case_action = cap_initial;
      else if (!some_nonuppercase_initial && some_uppercase)
	/* Should x -> yz, operating on X, give Yz or YZ?
	   We'll assume the latter.  */
	case_action = all_caps;
      else
	case_action = nochange;
    }

  /* Do replacement in a string.  */
  if (!NILP (string))
    {
      Lisp_Object before, after;

      before = Fsubstring (string, make_number (0),
			   make_number (search_regs.start[sub]));
      after = Fsubstring (string, make_number (search_regs.end[sub]), Qnil);

      /* Substitute parts of the match into NEWTEXT
	 if desired.  */
      if (NILP (literal))
	{
	  ptrdiff_t lastpos = 0;
	  ptrdiff_t lastpos_byte = 0;
	  /* We build up the substituted string in ACCUM.  */
	  Lisp_Object accum;
	  Lisp_Object middle;
	  ptrdiff_t length = SBYTES (newtext);

	  accum = Qnil;

	  for (pos_byte = 0, pos = 0; pos_byte < length;)
	    {
	      ptrdiff_t substart = -1;
	      ptrdiff_t subend = 0;
	      bool delbackslash = 0;

	      FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

	      if (c == '\\')
		{
		  FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

		  if (c == '&')
		    {
		      substart = search_regs.start[sub];
		      subend = search_regs.end[sub];
		    }
		  else if (c >= '1' && c <= '9')
		    {
		      if (c - '0' < search_regs.num_regs
			  && search_regs.start[c - '0'] >= 0)
			{
			  substart = search_regs.start[c - '0'];
			  subend = search_regs.end[c - '0'];
			}
		      else
			{
			  /* If that subexp did not match,
			     replace \\N with nothing.  */
			  substart = 0;
			  subend = 0;
			}
		    }
		  else if (c == '\\')
		    delbackslash = 1;
		  else if (c != '?')
		    error ("Invalid use of `\\' in replacement text");
		}
	      if (substart >= 0)
		{
		  if (pos - 2 != lastpos)
		    middle = substring_both (newtext, lastpos,
					     lastpos_byte,
					     pos - 2, pos_byte - 2);
		  else
		    middle = Qnil;
		  accum = concat3 (accum, middle,
				   Fsubstring (string,
					       make_number (substart),
					       make_number (subend)));
		  lastpos = pos;
		  lastpos_byte = pos_byte;
		}
	      else if (delbackslash)
		{
		  middle = substring_both (newtext, lastpos,
					   lastpos_byte,
					   pos - 1, pos_byte - 1);

		  accum = concat2 (accum, middle);
		  lastpos = pos;
		  lastpos_byte = pos_byte;
		}
	    }

	  if (pos != lastpos)
	    middle = substring_both (newtext, lastpos,
				     lastpos_byte,
				     pos, pos_byte);
	  else
	    middle = Qnil;

	  newtext = concat2 (accum, middle);
	}

      /* Do case substitution in NEWTEXT if desired.  */
      if (case_action == all_caps)
	newtext = Fupcase (newtext);
      else if (case_action == cap_initial)
	newtext = Fupcase_initials (newtext);

      return concat3 (before, newtext, after);
    }

  /* Record point, then move (quietly) to the start of the match.  */
  if (PT >= search_regs.end[sub])
    opoint = PT - ZV;
  else if (PT > search_regs.start[sub])
    opoint = search_regs.end[sub] - ZV;
  else
    opoint = PT;

  /* If we want non-literal replacement,
     perform substitution on the replacement string.  */
  if (NILP (literal))
    {
      ptrdiff_t length = SBYTES (newtext);
      unsigned char *substed;
      ptrdiff_t substed_alloc_size, substed_len;
      bool buf_multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
      bool str_multibyte = STRING_MULTIBYTE (newtext);
      bool really_changed = 0;

      substed_alloc_size = (length <= (STRING_BYTES_BOUND - 100) / 2
			    ? length * 2 + 100
			    : STRING_BYTES_BOUND);
      substed = xmalloc (substed_alloc_size);
      substed_len = 0;

      /* Go thru NEWTEXT, producing the actual text to insert in
	 SUBSTED while adjusting multibyteness to that of the current
	 buffer.  */

      for (pos_byte = 0, pos = 0; pos_byte < length;)
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  const unsigned char *add_stuff = NULL;
	  ptrdiff_t add_len = 0;
	  ptrdiff_t idx = -1;
	  ptrdiff_t begbyte UNINIT;

	  if (str_multibyte)
	    {
	      FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, newtext, pos, pos_byte);
	      if (!buf_multibyte)
		c = CHAR_TO_BYTE8 (c);
	    }
	  else
	    {
	      /* Note that we don't have to increment POS.  */
	      c = SREF (newtext, pos_byte++);
	      if (buf_multibyte)
		MAKE_CHAR_MULTIBYTE (c);
	    }

	  /* Either set ADD_STUFF and ADD_LEN to the text to put in SUBSTED,
	     or set IDX to a match index, which means put that part
	     of the buffer text into SUBSTED.  */

	  if (c == '\\')
	    {
	      really_changed = 1;

	      if (str_multibyte)
		{
		  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, newtext,
						      pos, pos_byte);
		  if (!buf_multibyte && !ASCII_CHAR_P (c))
		    c = CHAR_TO_BYTE8 (c);
		}
	      else
		{
		  c = SREF (newtext, pos_byte++);
		  if (buf_multibyte)
		    MAKE_CHAR_MULTIBYTE (c);
		}

	      if (c == '&')
		idx = sub;
	      else if (c >= '1' && c <= '9' && c - '0' < search_regs.num_regs)
		{
		  if (search_regs.start[c - '0'] >= 1)
		    idx = c - '0';
		}
	      else if (c == '\\')
		add_len = 1, add_stuff = (unsigned char *) "\\";
	      else
		{
		  xfree (substed);
		  error ("Invalid use of `\\' in replacement text");
		}
	    }
	  else
	    {
	      add_len = CHAR_STRING (c, str);
	      add_stuff = str;
	    }

	  /* If we want to copy part of a previous match,
	     set up ADD_STUFF and ADD_LEN to point to it.  */
	  if (idx >= 0)
	    {
	      begbyte = CHAR_TO_BYTE (search_regs.start[idx]);
	      add_len = CHAR_TO_BYTE (search_regs.end[idx]) - begbyte;
	      if (search_regs.start[idx] < GPT && GPT < search_regs.end[idx])
		move_gap_both (search_regs.start[idx], begbyte);
	    }

	  /* Now the stuff we want to add to SUBSTED
	     is invariably ADD_LEN bytes starting at ADD_STUFF.  */

	  /* Make sure SUBSTED is big enough.  */
	  if (substed_alloc_size - substed_len < add_len)
	    substed =
	      xpalloc (substed, &substed_alloc_size,
		       add_len - (substed_alloc_size - substed_len),
		       STRING_BYTES_BOUND, 1);

	  /* We compute this after the call to xpalloc, because that
	     could cause buffer text be relocated when ralloc.c is used.  */
	  if (idx >= 0)
	    add_stuff = BYTE_POS_ADDR (begbyte);

	  /* Now add to the end of SUBSTED.  */
	  if (add_stuff)
	    {
	      memcpy (substed + substed_len, add_stuff, add_len);
	      substed_len += add_len;
	    }
	}

      if (really_changed)
	newtext = make_specified_string ((const char *) substed, -1,
					 substed_len, buf_multibyte);
      xfree (substed);
    }

  /* The functions below modify the buffer, so they could trigger
     various modification hooks (see signal_before_change and
     signal_after_change).  If these hooks clobber the match data we
     error out since otherwise this will result in confusing bugs.  */
  ptrdiff_t sub_start = search_regs.start[sub];
  ptrdiff_t sub_end = search_regs.end[sub];
  unsigned  num_regs = search_regs.num_regs;
  newpoint = search_regs.start[sub] + SCHARS (newtext);

  /* Replace the old text with the new in the cleanest possible way.  */
  replace_range (search_regs.start[sub], search_regs.end[sub],
                 newtext, 1, 0, 1, 1);
  /* Update saved data to match adjustment made by replace_range.  */
  {
    ptrdiff_t change = newpoint - sub_end;
    if (sub_start >= sub_end)
      sub_start += change;
    sub_end += change;
  }

  if (case_action == all_caps)
    Fupcase_region (make_number (search_regs.start[sub]),
		    make_number (newpoint),
		    Qnil);
  else if (case_action == cap_initial)
    Fupcase_initials_region (make_number (search_regs.start[sub]),
			     make_number (newpoint));

  if (search_regs.start[sub] != sub_start
      || search_regs.end[sub] != sub_end
      || search_regs.num_regs != num_regs)
    error ("Match data clobbered by buffer modification hooks");

  /* Put point back where it was in the text.  */
  if (opoint <= 0)
    TEMP_SET_PT (opoint + ZV);
  else
    TEMP_SET_PT (opoint);

  /* Now move point "officially" to the start of the inserted replacement.  */
  move_if_not_intangible (newpoint);

  return Qnil;
}

static Lisp_Object
match_limit (Lisp_Object num, bool beginningp)
{
  EMACS_INT n;

  CHECK_NUMBER (num);
  n = XINT (num);
  if (n < 0)
    args_out_of_range (num, make_number (0));
  if (search_regs.num_regs <= 0)
    error ("No match data, because no search succeeded");
  if (n >= search_regs.num_regs
      || search_regs.start[n] < 0)
    return Qnil;
  return (make_number ((beginningp) ? search_regs.start[n]
		                    : search_regs.end[n]));
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
       doc: /* Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.

Return value is undefined if the last search failed.  */)
  (Lisp_Object subexp)
{
  return match_limit (subexp, 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
       doc: /* Return position of end of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.

Return value is undefined if the last search failed.  */)
  (Lisp_Object subexp)
{
  return match_limit (subexp, 0);
}

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 3, 0,
       doc: /* Return a list describing what the last search matched.
Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.
All the elements are markers or nil (nil if the Nth pair didn't match)
if the last match was on a buffer; integers or nil if a string was matched.
Use `set-match-data' to reinstate the data in this list.

If INTEGERS (the optional first argument) is non-nil, always use
integers (rather than markers) to represent buffer positions.  In
this case, and if the last match was in a buffer, the buffer will get
stored as one additional element at the end of the list.

If REUSE is a list, reuse it as part of the value.  If REUSE is long
enough to hold all the values, and if INTEGERS is non-nil, no consing
is done.

If optional third arg RESEAT is non-nil, any previous markers on the
REUSE list will be modified to point to nowhere.

Return value is undefined if the last search failed.  */)
  (Lisp_Object integers, Lisp_Object reuse, Lisp_Object reseat)
{
  Lisp_Object tail, prev;
  Lisp_Object *data;
  ptrdiff_t i, len;

  if (!NILP (reseat))
    for (tail = reuse; CONSP (tail); tail = XCDR (tail))
      if (MARKERP (XCAR (tail)))
	{
	  unchain_marker (XMARKER (XCAR (tail)));
	  XSETCAR (tail, Qnil);
	}

  if (NILP (last_thing_searched))
    return Qnil;

  prev = Qnil;

  USE_SAFE_ALLOCA;
  SAFE_NALLOCA (data, 1, 2 * search_regs.num_regs + 1);

  len = 0;
  for (i = 0; i < search_regs.num_regs; i++)
    {
      ptrdiff_t start = search_regs.start[i];
      if (start >= 0)
	{
	  if (EQ (last_thing_searched, Qt)
	      || ! NILP (integers))
	    {
	      XSETFASTINT (data[2 * i], start);
	      XSETFASTINT (data[2 * i + 1], search_regs.end[i]);
	    }
	  else if (BUFFERP (last_thing_searched))
	    {
	      data[2 * i] = Fmake_marker ();
	      Fset_marker (data[2 * i],
			   make_number (start),
			   last_thing_searched);
	      data[2 * i + 1] = Fmake_marker ();
	      Fset_marker (data[2 * i + 1],
			   make_number (search_regs.end[i]),
			   last_thing_searched);
	    }
	  else
	    /* last_thing_searched must always be Qt, a buffer, or Qnil.  */
	    emacs_abort ();

	  len = 2 * i + 2;
	}
      else
	data[2 * i] = data[2 * i + 1] = Qnil;
    }

  if (BUFFERP (last_thing_searched) && !NILP (integers))
    {
      data[len] = last_thing_searched;
      len++;
    }

  /* If REUSE is not usable, cons up the values and return them.  */
  if (! CONSP (reuse))
    reuse = Flist (len, data);
  else
    {
      /* If REUSE is a list, store as many value elements as will fit
	 into the elements of REUSE.  */
      for (i = 0, tail = reuse; CONSP (tail);
	   i++, tail = XCDR (tail))
	{
	  if (i < len)
	    XSETCAR (tail, data[i]);
	  else
	    XSETCAR (tail, Qnil);
	  prev = tail;
	}

      /* If we couldn't fit all value elements into REUSE,
	 cons up the rest of them and add them to the end of REUSE.  */
      if (i < len)
	XSETCDR (prev, Flist (len - i, data + i));
    }

  SAFE_FREE ();
  return reuse;
}

/* We used to have an internal use variant of `reseat' described as:

      If RESEAT is `evaporate', put the markers back on the free list
      immediately.  No other references to the markers must exist in this
      case, so it is used only internally on the unwind stack and
      save-match-data from Lisp.

   But it was ill-conceived: those supposedly-internal markers get exposed via
   the undo-list, so freeing them here is unsafe.  */

DEFUN ("set-match-data", Fset_match_data, Sset_match_data, 1, 2, 0,
       doc: /* Set internal data on last search match from elements of LIST.
LIST should have been created by calling `match-data' previously.

If optional arg RESEAT is non-nil, make markers on LIST point nowhere.  */)
  (register Lisp_Object list, Lisp_Object reseat)
{
  ptrdiff_t i;
  register Lisp_Object marker;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_LIST (list);

  /* Unless we find a marker with a buffer or an explicit buffer
     in LIST, assume that this match data came from a string.  */
  last_thing_searched = Qt;

  /* Allocate registers if they don't already exist.  */
  {
    EMACS_INT length = XFASTINT (Flength (list)) / 2;

    if (length > search_regs.num_regs)
      {
	ptrdiff_t num_regs = search_regs.num_regs;
	if (PTRDIFF_MAX < length)
	  memory_full (SIZE_MAX);
	search_regs.start =
	  xpalloc (search_regs.start, &num_regs, length - num_regs,
		   min (PTRDIFF_MAX, UINT_MAX), sizeof (regoff_t));
	search_regs.end =
	  xrealloc (search_regs.end, num_regs * sizeof (regoff_t));

	for (i = search_regs.num_regs; i < num_regs; i++)
	  search_regs.start[i] = -1;

	search_regs.num_regs = num_regs;
      }

    for (i = 0; CONSP (list); i++)
      {
	marker = XCAR (list);
	if (BUFFERP (marker))
	  {
	    last_thing_searched = marker;
	    break;
	  }
	if (i >= length)
	  break;
	if (NILP (marker))
	  {
	    search_regs.start[i] = -1;
	    list = XCDR (list);
	  }
	else
	  {
	    Lisp_Object from;
	    Lisp_Object m;

	    m = marker;
	    if (MARKERP (marker))
	      {
		if (XMARKER (marker)->buffer == 0)
		  XSETFASTINT (marker, 0);
		else
		  XSETBUFFER (last_thing_searched, XMARKER (marker)->buffer);
	      }

	    CHECK_NUMBER_COERCE_MARKER (marker);
	    from = marker;

	    if (!NILP (reseat) && MARKERP (m))
	      {
		unchain_marker (XMARKER (m));
		XSETCAR (list, Qnil);
	      }

	    if ((list = XCDR (list), !CONSP (list)))
	      break;

	    m = marker = XCAR (list);

	    if (MARKERP (marker) && XMARKER (marker)->buffer == 0)
	      XSETFASTINT (marker, 0);

	    CHECK_NUMBER_COERCE_MARKER (marker);
	    if ((XINT (from) < 0
		 ? TYPE_MINIMUM (regoff_t) <= XINT (from)
		 : XINT (from) <= TYPE_MAXIMUM (regoff_t))
		&& (XINT (marker) < 0
		    ? TYPE_MINIMUM (regoff_t) <= XINT (marker)
		    : XINT (marker) <= TYPE_MAXIMUM (regoff_t)))
	      {
		search_regs.start[i] = XINT (from);
		search_regs.end[i] = XINT (marker);
	      }
	    else
	      {
		search_regs.start[i] = -1;
	      }

	    if (!NILP (reseat) && MARKERP (m))
	      {
		unchain_marker (XMARKER (m));
		XSETCAR (list, Qnil);
	      }
	  }
	list = XCDR (list);
      }

    for (; i < search_regs.num_regs; i++)
      search_regs.start[i] = -1;
  }

  return Qnil;
}

/* If true the match data have been saved in saved_search_regs
   during the execution of a sentinel or filter. */
/* static bool search_regs_saved; */
/* static struct re_registers saved_search_regs; */
/* static Lisp_Object saved_last_thing_searched; */

/* Called from Flooking_at, Fstring_match, search_buffer, Fstore_match_data
   if asynchronous code (filter or sentinel) is running. */
static void
save_search_regs (void)
{
  if (!search_regs_saved)
    {
      saved_search_regs.num_regs = search_regs.num_regs;
      saved_search_regs.start = search_regs.start;
      saved_search_regs.end = search_regs.end;
      saved_last_thing_searched = last_thing_searched;
      last_thing_searched = Qnil;
      search_regs.num_regs = 0;
      search_regs.start = 0;
      search_regs.end = 0;

      search_regs_saved = 1;
    }
}

/* Called upon exit from filters and sentinels. */
void
restore_search_regs (void)
{
  if (search_regs_saved)
    {
      if (search_regs.num_regs > 0)
	{
	  xfree (search_regs.start);
	  xfree (search_regs.end);
	}
      search_regs.num_regs = saved_search_regs.num_regs;
      search_regs.start = saved_search_regs.start;
      search_regs.end = saved_search_regs.end;
      last_thing_searched = saved_last_thing_searched;
      saved_last_thing_searched = Qnil;
      search_regs_saved = 0;
    }
}

/* Called from replace-match via replace_range.  */
void
update_search_regs (ptrdiff_t oldstart, ptrdiff_t oldend, ptrdiff_t newend)
{
  /* Adjust search data for this change.  */
  ptrdiff_t change = newend - oldend;
  ptrdiff_t i;

  for (i = 0; i < search_regs.num_regs; i++)
    {
      if (search_regs.start[i] >= oldend)
        search_regs.start[i] += change;
      else if (search_regs.start[i] > oldstart)
        search_regs.start[i] = oldstart;
      if (search_regs.end[i] >= oldend)
        search_regs.end[i] += change;
      else if (search_regs.end[i] > oldstart)
        search_regs.end[i] = oldstart;
    }
}

static void
unwind_set_match_data (Lisp_Object list)
{
  /* It is NOT ALWAYS safe to free (evaporate) the markers immediately.  */
  Fset_match_data (list, Qt);
}

/* Called to unwind protect the match data.  */
void
record_unwind_save_match_data (void)
{
  record_unwind_protect (unwind_set_match_data,
			 Fmatch_data (Qnil, Qnil, Qnil));
}

/* Quote a string to deactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, Sregexp_quote, 1, 1, 0,
       doc: /* Return a regexp string which matches exactly STRING and nothing else.  */)
  (Lisp_Object string)
{
  char *in, *out, *end;
  char *temp;
  ptrdiff_t backslashes_added = 0;

  CHECK_STRING (string);

  USE_SAFE_ALLOCA;
  SAFE_NALLOCA (temp, 2, SBYTES (string));

  /* Now copy the data into the new string, inserting escapes. */

  in = SSDATA (string);
  end = in + SBYTES (string);
  out = temp;

  for (; in != end; in++)
    {
      if (*in == '['
	  || *in == '*' || *in == '.' || *in == '\\'
	  || *in == '?' || *in == '+'
	  || *in == '^' || *in == '$')
	*out++ = '\\', backslashes_added++;
      *out++ = *in;
    }

  Lisp_Object result
    = make_specified_string (temp,
			     SCHARS (string) + backslashes_added,
			     out - temp,
			     STRING_MULTIBYTE (string));
  SAFE_FREE ();
  return result;
}

/* Like find_newline, but doesn't use the cache, and only searches forward.  */
static ptrdiff_t
find_newline1 (ptrdiff_t start, ptrdiff_t start_byte, ptrdiff_t end,
	       ptrdiff_t end_byte, ptrdiff_t count, ptrdiff_t *shortage,
	       ptrdiff_t *bytepos, bool allow_quit)
{
  if (count > 0)
    {
      if (!end)
	end = ZV, end_byte = ZV_BYTE;
    }
  else
    {
      if (!end)
	end = BEGV, end_byte = BEGV_BYTE;
    }
  if (end_byte == -1)
    end_byte = CHAR_TO_BYTE (end);

  if (shortage != 0)
    *shortage = 0;

  if (count > 0)
    while (start != end)
      {
        /* Our innermost scanning loop is very simple; it doesn't know
           about gaps, buffer ends, or the newline cache.  ceiling is
           the position of the last character before the next such
           obstacle --- the last character the dumb search loop should
           examine.  */
	ptrdiff_t tem, ceiling_byte = end_byte - 1;

	if (start_byte == -1)
	  start_byte = CHAR_TO_BYTE (start);

        /* The dumb loop can only scan text stored in contiguous
           bytes. BUFFER_CEILING_OF returns the last character
           position that is contiguous, so the ceiling is the
           position after that.  */
	tem = BUFFER_CEILING_OF (start_byte);
	ceiling_byte = min (tem, ceiling_byte);

        {
          /* The termination address of the dumb loop.  */
	  unsigned char *lim_addr = BYTE_POS_ADDR (ceiling_byte) + 1;
	  ptrdiff_t lim_byte = ceiling_byte + 1;

	  /* Nonpositive offsets (relative to LIM_ADDR and LIM_BYTE)
	     of the base, the cursor, and the next line.  */
	  ptrdiff_t base = start_byte - lim_byte;
	  ptrdiff_t cursor, next;

	  for (cursor = base; cursor < 0; cursor = next)
	    {
              /* The dumb loop.  */
	      unsigned char *nl = memchr (lim_addr + cursor, '\n', - cursor);
	      next = nl ? nl - lim_addr : 0;

              if (! nl)
		break;
	      next++;

	      if (--count == 0)
		{
		  if (bytepos)
		    *bytepos = lim_byte + next;
		  return BYTE_TO_CHAR (lim_byte + next);
		}
	      if (allow_quit)
		maybe_quit ();
            }

	  start_byte = lim_byte;
	  start = BYTE_TO_CHAR (start_byte);
        }
      }

  if (shortage)
    *shortage = count;
  if (bytepos)
    {
      *bytepos = start_byte == -1 ? CHAR_TO_BYTE (start) : start_byte;
      eassert (*bytepos == CHAR_TO_BYTE (start));
    }
  return start;
}

DEFUN ("newline-cache-check", Fnewline_cache_check, Snewline_cache_check,
       0, 1, 0,
       doc: /* Check the newline cache of BUFFER against buffer contents.

BUFFER defaults to the current buffer.

Value is an array of 2 sub-arrays of buffer positions for newlines,
the first based on the cache, the second based on actually scanning
the buffer.  If the buffer doesn't have a cache, the value is nil.  */)
  (Lisp_Object buffer)
{
  struct buffer *buf, *old = NULL;
  ptrdiff_t shortage, nl_count_cache, nl_count_buf;
  Lisp_Object cache_newlines, buf_newlines, val;
  ptrdiff_t from, found, i;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
      old = current_buffer;
    }
  if (buf->base_buffer)
    buf = buf->base_buffer;

  /* If the buffer doesn't have a newline cache, return nil.  */
  if (NILP (BVAR (buf, cache_long_scans))
      || buf->newline_cache == NULL)
    return Qnil;

  /* find_newline can only work on the current buffer.  */
  if (old != NULL)
    set_buffer_internal_1 (buf);

  /* How many newlines are there according to the cache?  */
  find_newline (BEGV, BEGV_BYTE, ZV, ZV_BYTE,
		TYPE_MAXIMUM (ptrdiff_t), &shortage, NULL, true);
  nl_count_cache = TYPE_MAXIMUM (ptrdiff_t) - shortage;

  /* Create vector and populate it.  */
  cache_newlines = make_uninit_vector (nl_count_cache);

  if (nl_count_cache)
    {
      for (from = BEGV, found = from, i = 0; from < ZV; from = found, i++)
	{
	  ptrdiff_t from_byte = CHAR_TO_BYTE (from);

	  found = find_newline (from, from_byte, 0, -1, 1, &shortage,
				NULL, true);
	  if (shortage != 0 || i >= nl_count_cache)
	    break;
	  ASET (cache_newlines, i, make_number (found - 1));
	}
      /* Fill the rest of slots with an invalid position.  */
      for ( ; i < nl_count_cache; i++)
	ASET (cache_newlines, i, make_number (-1));
    }

  /* Now do the same, but without using the cache.  */
  find_newline1 (BEGV, BEGV_BYTE, ZV, ZV_BYTE,
		 TYPE_MAXIMUM (ptrdiff_t), &shortage, NULL, true);
  nl_count_buf = TYPE_MAXIMUM (ptrdiff_t) - shortage;
  buf_newlines = make_uninit_vector (nl_count_buf);
  if (nl_count_buf)
    {
      for (from = BEGV, found = from, i = 0; from < ZV; from = found, i++)
	{
	  ptrdiff_t from_byte = CHAR_TO_BYTE (from);

	  found = find_newline1 (from, from_byte, 0, -1, 1, &shortage,
				 NULL, true);
	  if (shortage != 0 || i >= nl_count_buf)
	    break;
	  ASET (buf_newlines, i, make_number (found - 1));
	}
      for ( ; i < nl_count_buf; i++)
	ASET (buf_newlines, i, make_number (-1));
    }

  /* Construct the value and return it.  */
  val = make_uninit_vector (2);
  ASET (val, 0, cache_newlines);
  ASET (val, 1, buf_newlines);

  if (old != NULL)
    set_buffer_internal_1 (old);
  return val;
}

void
syms_of_search (void)
{
  register int i;

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    {
      searchbufs[i].buf.allocated = 100;
      searchbufs[i].buf.buffer = xmalloc (100);
      searchbufs[i].buf.fastmap = searchbufs[i].fastmap;
      searchbufs[i].regexp = Qnil;
      searchbufs[i].f_whitespace_regexp = Qnil;
      searchbufs[i].syntax_table = Qnil;
      staticpro (&searchbufs[i].regexp);
      staticpro (&searchbufs[i].f_whitespace_regexp);
      staticpro (&searchbufs[i].syntax_table);
      searchbufs[i].next = (i == REGEXP_CACHE_SIZE-1 ? 0 : &searchbufs[i+1]);
    }
  searchbuf_head = &searchbufs[0];

  /* Error condition used for failing searches.  */
  DEFSYM (Qsearch_failed, "search-failed");

  /* Error condition used for failing searches started by user, i.e.,
     where failure should not invoke the debugger.  */
  DEFSYM (Quser_search_failed, "user-search-failed");

  /* Error condition signaled when regexp compile_pattern fails.  */
  DEFSYM (Qinvalid_regexp, "invalid-regexp");

  Fput (Qsearch_failed, Qerror_conditions,
	listn (CONSTYPE_PURE, 2, Qsearch_failed, Qerror));
  Fput (Qsearch_failed, Qerror_message,
	build_pure_c_string ("Search failed"));

  Fput (Quser_search_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 4,
               Quser_search_failed, Quser_error, Qsearch_failed, Qerror));
  Fput (Quser_search_failed, Qerror_message,
        build_pure_c_string ("Search failed"));

  Fput (Qinvalid_regexp, Qerror_conditions,
	listn (CONSTYPE_PURE, 2, Qinvalid_regexp, Qerror));
  Fput (Qinvalid_regexp, Qerror_message,
	build_pure_c_string ("Invalid regexp"));

  last_thing_searched = Qnil;
  staticpro (&last_thing_searched);

  saved_last_thing_searched = Qnil;
  staticpro (&saved_last_thing_searched);

  DEFVAR_LISP ("search-spaces-regexp", Vsearch_spaces_regexp,
      doc: /* Regexp to substitute for bunches of spaces in regexp search.
Some commands use this for user-specified regexps.
Spaces that occur inside character classes or repetition operators
or other such regexp constructs are not replaced with this.
A value of nil (which is the normal value) means treat spaces literally.  */);
  Vsearch_spaces_regexp = Qnil;

  DEFSYM (Qinhibit_changing_match_data, "inhibit-changing-match-data");
  DEFVAR_LISP ("inhibit-changing-match-data", Vinhibit_changing_match_data,
      doc: /* Internal use only.
If non-nil, the primitive searching and matching functions
such as `looking-at', `string-match', `re-search-forward', etc.,
do not set the match data.  The proper way to use this variable
is to bind it with `let' around a small expression.  */);
  Vinhibit_changing_match_data = Qnil;

  defsubr (&Slooking_at);
  defsubr (&Sposix_looking_at);
  defsubr (&Sstring_match);
  defsubr (&Sposix_string_match);
  defsubr (&Ssearch_forward);
  defsubr (&Ssearch_backward);
  defsubr (&Sre_search_forward);
  defsubr (&Sre_search_backward);
  defsubr (&Sposix_search_forward);
  defsubr (&Sposix_search_backward);
  defsubr (&Sreplace_match);
  defsubr (&Smatch_beginning);
  defsubr (&Smatch_end);
  defsubr (&Smatch_data);
  defsubr (&Sset_match_data);
  defsubr (&Sregexp_quote);
  defsubr (&Snewline_cache_check);
}
