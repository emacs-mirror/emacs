/* String search routines for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <config.h>
#include "lisp.h"
#include "syntax.h"
#include "buffer.h"
#include "region-cache.h"
#include "commands.h"
#include "blockinput.h"

#include <sys/types.h>
#include "regex.h"

#define REGEXP_CACHE_SIZE 5

/* If the regexp is non-nil, then the buffer contains the compiled form
   of that regexp, suitable for searching.  */
struct regexp_cache {
  struct regexp_cache *next;
  Lisp_Object regexp;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  /* Nonzero means regexp was compiled to do full POSIX backtracking.  */
  char posix;
};

/* The instances of that struct.  */
struct regexp_cache searchbufs[REGEXP_CACHE_SIZE];

/* The head of the linked list; points to the most recently used buffer.  */
struct regexp_cache *searchbuf_head;


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
static struct re_registers search_regs;

/* The buffer in which the last search was performed, or
   Qt if the last search was done in a string;
   Qnil if no searching has been done yet.  */
static Lisp_Object last_thing_searched;

/* error condition signalled when regexp compile_pattern fails */

Lisp_Object Qinvalid_regexp;

static void set_search_regs ();
static void save_search_regs ();

static int search_buffer ();

static void
matcher_overflow ()
{
  error ("Stack overflow in regexp matcher");
}

#ifdef __STDC__
#define CONST const
#else
#define CONST
#endif

/* Compile a regexp and signal a Lisp error if anything goes wrong.
   PATTERN is the pattern to compile.
   CP is the place to put the result.
   TRANSLATE is a translation table for ignoring case, or NULL for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

static void
compile_pattern_1 (cp, pattern, translate, regp, posix)
     struct regexp_cache *cp;
     Lisp_Object pattern;
     char *translate;
     struct re_registers *regp;
     int posix;
{
  CONST char *val;
  reg_syntax_t old;

  cp->regexp = Qnil;
  cp->buf.translate = translate;
  cp->posix = posix;
  BLOCK_INPUT;
  old = re_set_syntax (RE_SYNTAX_EMACS
		       | (posix ? 0 : RE_NO_POSIX_BACKTRACKING));
  val = (CONST char *) re_compile_pattern ((char *) XSTRING (pattern)->data,
					   XSTRING (pattern)->size, &cp->buf);
  re_set_syntax (old);
  UNBLOCK_INPUT;
  if (val)
    Fsignal (Qinvalid_regexp, Fcons (build_string (val), Qnil));

  cp->regexp = Fcopy_sequence (pattern);

  /* Advise the searching functions about the space we have allocated
     for register data.  */
  BLOCK_INPUT;
  if (regp)
    re_set_registers (&cp->buf, regp, regp->num_regs, regp->start, regp->end);
  UNBLOCK_INPUT;
}

/* Compile a regexp if necessary, but first check to see if there's one in
   the cache.
   PATTERN is the pattern to compile.
   TRANSLATE is a translation table for ignoring case, or NULL for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

struct re_pattern_buffer *
compile_pattern (pattern, regp, translate, posix)
     Lisp_Object pattern;
     struct re_registers *regp;
     char *translate;
     int posix;
{
  struct regexp_cache *cp, **cpp;

  for (cpp = &searchbuf_head; ; cpp = &cp->next)
    {
      cp = *cpp;
      if (!NILP (Fstring_equal (cp->regexp, pattern))
	  && cp->buf.translate == translate
	  && cp->posix == posix)
	break;

      /* If we're at the end of the cache, compile into the last cell.  */
      if (cp->next == 0)
	{
	  compile_pattern_1 (cp, pattern, translate, regp, posix);
	  break;
	}
    }

  /* When we get here, cp (aka *cpp) contains the compiled pattern,
     either because we found it in the cache or because we just compiled it.
     Move it to the front of the queue to mark it as most recently used.  */
  *cpp = cp->next;
  cp->next = searchbuf_head;
  searchbuf_head = cp;

  return &cp->buf;
}

/* Error condition used for failing searches */
Lisp_Object Qsearch_failed;

Lisp_Object
signal_failure (arg)
     Lisp_Object arg;
{
  Fsignal (Qsearch_failed, Fcons (arg, Qnil));
  return Qnil;
}

static Lisp_Object
looking_at_1 (string, posix)
     Lisp_Object string;
     int posix;
{
  Lisp_Object val;
  unsigned char *p1, *p2;
  int s1, s2;
  register int i;
  struct re_pattern_buffer *bufp;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_STRING (string, 0);
  bufp = compile_pattern (string, &search_regs,
			  (!NILP (current_buffer->case_fold_search)
			   ? DOWNCASE_TABLE : 0),
			  posix);

  immediate_quit = 1;
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BEGV_ADDR;
  s1 = GPT - BEGV;
  p2 = GAP_END_ADDR;
  s2 = ZV - GPT;
  if (s1 < 0)
    {
      p2 = p1;
      s2 = ZV - BEGV;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 = ZV - BEGV;
      s2 = 0;
    }
  
  i = re_match_2 (bufp, (char *) p1, s1, (char *) p2, s2,
		  point - BEGV, &search_regs,
		  ZV - BEGV);
  if (i == -2)
    matcher_overflow ();

  val = (0 <= i ? Qt : Qnil);
  for (i = 0; i < search_regs.num_regs; i++)
    if (search_regs.start[i] >= 0)
      {
	search_regs.start[i] += BEGV;
	search_regs.end[i] += BEGV;
      }
  XSETBUFFER (last_thing_searched, current_buffer);
  immediate_quit = 0;
  return val;
}

DEFUN ("looking-at", Flooking_at, Slooking_at, 1, 1, 0,
  "Return t if text after point matches regular expression PAT.\n\
This function modifies the match data that `match-beginning',\n\
`match-end' and `match-data' access; save and restore the match\n\
data if you want to preserve them.")
  (string)
     Lisp_Object string;
{
  return looking_at_1 (string, 0);
}

DEFUN ("posix-looking-at", Fposix_looking_at, Sposix_looking_at, 1, 1, 0,
  "Return t if text after point matches regular expression PAT.\n\
Find the longest match, in accord with Posix regular expression rules.\n\
This function modifies the match data that `match-beginning',\n\
`match-end' and `match-data' access; save and restore the match\n\
data if you want to preserve them.")
  (string)
     Lisp_Object string;
{
  return looking_at_1 (string, 1);
}

static Lisp_Object
string_match_1 (regexp, string, start, posix)
     Lisp_Object regexp, string, start;
     int posix;
{
  int val;
  int s;
  struct re_pattern_buffer *bufp;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_STRING (regexp, 0);
  CHECK_STRING (string, 1);

  if (NILP (start))
    s = 0;
  else
    {
      int len = XSTRING (string)->size;

      CHECK_NUMBER (start, 2);
      s = XINT (start);
      if (s < 0 && -s <= len)
	s = len + s;
      else if (0 > s || s > len)
	args_out_of_range (string, start);
    }

  bufp = compile_pattern (regexp, &search_regs,
			  (!NILP (current_buffer->case_fold_search)
			   ? DOWNCASE_TABLE : 0),
			  0);
  immediate_quit = 1;
  val = re_search (bufp, (char *) XSTRING (string)->data,
		   XSTRING (string)->size, s, XSTRING (string)->size - s,
		   &search_regs);
  immediate_quit = 0;
  last_thing_searched = Qt;
  if (val == -2)
    matcher_overflow ();
  if (val < 0) return Qnil;
  return make_number (val);
}

DEFUN ("string-match", Fstring_match, Sstring_match, 2, 3, 0,
  "Return index of start of first match for REGEXP in STRING, or nil.\n\
If third arg START is non-nil, start search at that index in STRING.\n\
For index of first char beyond the match, do (match-end 0).\n\
`match-end' and `match-beginning' also give indices of substrings\n\
matched by parenthesis constructs in the pattern.")
  (regexp, string, start)
     Lisp_Object regexp, string, start;
{
  return string_match_1 (regexp, string, start, 0);
}

DEFUN ("posix-string-match", Fposix_string_match, Sposix_string_match, 2, 3, 0,
  "Return index of start of first match for REGEXP in STRING, or nil.\n\
Find the longest match, in accord with Posix regular expression rules.\n\
If third arg START is non-nil, start search at that index in STRING.\n\
For index of first char beyond the match, do (match-end 0).\n\
`match-end' and `match-beginning' also give indices of substrings\n\
matched by parenthesis constructs in the pattern.")
  (regexp, string, start)
     Lisp_Object regexp, string, start;
{
  return string_match_1 (regexp, string, start, 1);
}

/* Match REGEXP against STRING, searching all of STRING,
   and return the index of the match, or negative on failure.
   This does not clobber the match data.  */

int
fast_string_match (regexp, string)
     Lisp_Object regexp, string;
{
  int val;
  struct re_pattern_buffer *bufp;

  bufp = compile_pattern (regexp, 0, 0, 0);
  immediate_quit = 1;
  val = re_search (bufp, (char *) XSTRING (string)->data,
		   XSTRING (string)->size, 0, XSTRING (string)->size,
		   0);
  immediate_quit = 0;
  return val;
}

/* max and min.  */

static int
max (a, b)
     int a, b;
{
  return ((a > b) ? a : b);
}

static int
min (a, b)
     int a, b;
{
  return ((a < b) ? a : b);
}


/* The newline cache: remembering which sections of text have no newlines.  */

/* If the user has requested newline caching, make sure it's on.
   Otherwise, make sure it's off.
   This is our cheezy way of associating an action with the change of
   state of a buffer-local variable.  */
static void
newline_cache_on_off (buf)
     struct buffer *buf;
{
  if (NILP (buf->cache_long_line_scans))
    {
      /* It should be off.  */
      if (buf->newline_cache)
        {
          free_region_cache (buf->newline_cache);
          buf->newline_cache = 0;
        }
    }
  else
    {
      /* It should be on.  */
      if (buf->newline_cache == 0)
        buf->newline_cache = new_region_cache ();
    }
}


/* Search for COUNT instances of the character TARGET between START and END.

   If COUNT is positive, search forwards; END must be >= START.
   If COUNT is negative, search backwards for the -COUNTth instance;
      END must be <= START.
   If COUNT is zero, do anything you please; run rogue, for all I care.

   If END is zero, use BEGV or ZV instead, as appropriate for the
   direction indicated by COUNT.

   If we find COUNT instances, set *SHORTAGE to zero, and return the
   position after the COUNTth match.  Note that for reverse motion
   this is not the same as the usual convention for Emacs motion commands.

   If we don't find COUNT instances before reaching END, set *SHORTAGE
   to the number of TARGETs left unfound, and return END.

   If ALLOW_QUIT is non-zero, set immediate_quit.  That's good to do
   except when inside redisplay.  */

scan_buffer (target, start, end, count, shortage, allow_quit)
     register int target;
     int start, end;
     int count;
     int *shortage;
     int allow_quit;
{
  struct region_cache *newline_cache;
  int direction; 

  if (count > 0)
    {
      direction = 1;
      if (! end) end = ZV;
    }
  else
    {
      direction = -1;
      if (! end) end = BEGV;
    }

  newline_cache_on_off (current_buffer);
  newline_cache = current_buffer->newline_cache;

  if (shortage != 0)
    *shortage = 0;

  immediate_quit = allow_quit;

  if (count > 0)
    while (start != end)
      {
        /* Our innermost scanning loop is very simple; it doesn't know
           about gaps, buffer ends, or the newline cache.  ceiling is
           the position of the last character before the next such
           obstacle --- the last character the dumb search loop should
           examine.  */
        register int ceiling = end - 1;

        /* If we're looking for a newline, consult the newline cache
           to see where we can avoid some scanning.  */
        if (target == '\n' && newline_cache)
          {
            int next_change;
            immediate_quit = 0;
            while (region_cache_forward
                   (current_buffer, newline_cache, start, &next_change))
              start = next_change;
            immediate_quit = allow_quit;

            /* start should never be after end.  */
            if (start >= end)
              start = end - 1;

            /* Now the text after start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling = min (next_change - 1, ceiling);
          }

        /* The dumb loop can only scan text stored in contiguous
           bytes. BUFFER_CEILING_OF returns the last character
           position that is contiguous, so the ceiling is the
           position after that.  */
        ceiling = min (BUFFER_CEILING_OF (start), ceiling);

        {
          /* The termination address of the dumb loop.  */ 
          register unsigned char *ceiling_addr = &FETCH_CHAR (ceiling) + 1;
          register unsigned char *cursor = &FETCH_CHAR (start);
          unsigned char *base = cursor;

          while (cursor < ceiling_addr)
            {
              unsigned char *scan_start = cursor;

              /* The dumb loop.  */
              while (*cursor != target && ++cursor < ceiling_addr)
                ;

              /* If we're looking for newlines, cache the fact that
                 the region from start to cursor is free of them. */
              if (target == '\n' && newline_cache)
                know_region_cache (current_buffer, newline_cache,
                                   start + scan_start - base,
                                   start + cursor - base);

              /* Did we find the target character?  */
              if (cursor < ceiling_addr)
                {
                  if (--count == 0)
                    {
                      immediate_quit = 0;
                      return (start + cursor - base + 1);
                    }
                  cursor++;
                }
            }

          start += cursor - base;
        }
      }
  else
    while (start > end)
      {
        /* The last character to check before the next obstacle.  */
        register int ceiling = end;

        /* Consult the newline cache, if appropriate.  */
        if (target == '\n' && newline_cache)
          {
            int next_change;
            immediate_quit = 0;
            while (region_cache_backward
                   (current_buffer, newline_cache, start, &next_change))
              start = next_change;
            immediate_quit = allow_quit;

            /* Start should never be at or before end.  */
            if (start <= end)
              start = end + 1;

            /* Now the text before start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling = max (next_change, ceiling);
          }

        /* Stop scanning before the gap.  */
        ceiling = max (BUFFER_FLOOR_OF (start - 1), ceiling);

        {
          /* The termination address of the dumb loop.  */
          register unsigned char *ceiling_addr = &FETCH_CHAR (ceiling);
          register unsigned char *cursor = &FETCH_CHAR (start - 1);
          unsigned char *base = cursor;

          while (cursor >= ceiling_addr)
            {
              unsigned char *scan_start = cursor;

              while (*cursor != target && --cursor >= ceiling_addr)
                ;

              /* If we're looking for newlines, cache the fact that
                 the region from after the cursor to start is free of them.  */
              if (target == '\n' && newline_cache)
                know_region_cache (current_buffer, newline_cache,
                                   start + cursor - base,
                                   start + scan_start - base);

              /* Did we find the target character?  */
              if (cursor >= ceiling_addr)
                {
                  if (++count >= 0)
                    {
                      immediate_quit = 0;
                      return (start + cursor - base);
                    }
                  cursor--;
                }
            }

          start += cursor - base;
        }
      }

  immediate_quit = 0;
  if (shortage != 0)
    *shortage = count * direction;
  return start;
}

int
find_next_newline_no_quit (from, cnt)
     register int from, cnt;
{
  return scan_buffer ('\n', from, 0, cnt, (int *) 0, 0);
}

int
find_next_newline (from, cnt)
     register int from, cnt;
{
  return scan_buffer ('\n', from, 0, cnt, (int *) 0, 1);
}


/* Like find_next_newline, but returns position before the newline,
   not after, and only search up to TO.  This isn't just
   find_next_newline (...)-1, because you might hit TO.  */
int
find_before_next_newline (from, to, cnt)
     int from, to, cnt;
{
  int shortage;
  int pos = scan_buffer ('\n', from, to, cnt, &shortage, 1);

  if (shortage == 0)
    pos--;
  
  return pos;
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
  register unsigned char c;
  unsigned char fastmap[0400];
  int negate = 0;
  register int i;

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
     Otherwise, handle backslashes and ranges specially  */

  while (p != pend)
    {
      c = *p++;
      if (syntaxp)
	fastmap[c] = 1;
      else
	{
	  if (c == '\\')
	    {
	      if (p == pend) break;
	      c = *p++;
	    }
	  if (p != pend && *p == '-')
	    {
	      p++;
	      if (p == pend) break;
	      while (c <= *p)
		{
		  fastmap[c] = 1;
		  c++;
		}
	      p++;
	    }
	  else
	    fastmap[c] = 1;
	}
    }

  if (syntaxp && fastmap['-'] != 0)
    fastmap[' '] = 1;

  /* If ^ was the first character, complement the fastmap. */

  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      fastmap[i] ^= 1;

  {
    int start_point = point;

    immediate_quit = 1;
    if (syntaxp)
      {

	if (forwardp)
	  {
	    while (point < XINT (lim)
		   && fastmap[(unsigned char) syntax_code_spec[(int) SYNTAX (FETCH_CHAR (point))]])
	      SET_PT (point + 1);
	  }
	else
	  {
	    while (point > XINT (lim)
		   && fastmap[(unsigned char) syntax_code_spec[(int) SYNTAX (FETCH_CHAR (point - 1))]])
	      SET_PT (point - 1);
	  }
      }
    else
      {
	if (forwardp)
	  {
	    while (point < XINT (lim) && fastmap[FETCH_CHAR (point)])
	      SET_PT (point + 1);
	  }
	else
	  {
	    while (point > XINT (lim) && fastmap[FETCH_CHAR (point - 1)])
	      SET_PT (point - 1);
	  }
      }
    immediate_quit = 0;

    return make_number (point - start_point);
  }
}

/* Subroutines of Lisp buffer search functions. */

static Lisp_Object
search_command (string, bound, noerror, count, direction, RE, posix)
     Lisp_Object string, bound, noerror, count;
     int direction;
     int RE;
     int posix;
{
  register int np;
  int lim;
  int n = direction;

  if (!NILP (count))
    {
      CHECK_NUMBER (count, 3);
      n *= XINT (count);
    }

  CHECK_STRING (string, 0);
  if (NILP (bound))
    lim = n > 0 ? ZV : BEGV;
  else
    {
      CHECK_NUMBER_COERCE_MARKER (bound, 1);
      lim = XINT (bound);
      if (n > 0 ? lim < point : lim > point)
	error ("Invalid search bound (wrong side of point)");
      if (lim > ZV)
	lim = ZV;
      if (lim < BEGV)
	lim = BEGV;
    }

  np = search_buffer (string, point, lim, n, RE,
		      (!NILP (current_buffer->case_fold_search)
		       ? XSTRING (current_buffer->case_canon_table)->data : 0),
		      (!NILP (current_buffer->case_fold_search)
		       ? XSTRING (current_buffer->case_eqv_table)->data : 0),
		      posix);
  if (np <= 0)
    {
      if (NILP (noerror))
	return signal_failure (string);
      if (!EQ (noerror, Qt))
	{
	  if (lim < BEGV || lim > ZV)
	    abort ();
	  SET_PT (lim);
	  return Qnil;
#if 0 /* This would be clean, but maybe programs depend on
	 a value of nil here.  */
	  np = lim;
#endif
	}
      else
	return Qnil;
    }

  if (np < BEGV || np > ZV)
    abort ();

  SET_PT (np);

  return make_number (np);
}

static int
trivial_regexp_p (regexp)
     Lisp_Object regexp;
{
  int len = XSTRING (regexp)->size;
  unsigned char *s = XSTRING (regexp)->data;
  unsigned char c;
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
	    case 'S': case '1': case '2': case '3': case '4': case '5':
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

   Returns -x if only N-x occurrences found (x > 0),
   or else the position at the beginning of the Nth occurrence
   (if searching backward) or the end (if searching forward).

   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

static int
search_buffer (string, pos, lim, n, RE, trt, inverse_trt, posix)
     Lisp_Object string;
     int pos;
     int lim;
     int n;
     int RE;
     register unsigned char *trt;
     register unsigned char *inverse_trt;
     int posix;
{
  int len = XSTRING (string)->size;
  unsigned char *base_pat = XSTRING (string)->data;
  register int *BM_tab;
  int *BM_tab_base;
  register int direction = ((n > 0) ? 1 : -1);
  register int dirlen;
  int infinity, limit, k, stride_for_teases;
  register unsigned char *pat, *cursor, *p_limit;  
  register int i, j;
  unsigned char *p1, *p2;
  int s1, s2;

  if (running_asynch_code)
    save_search_regs ();

  /* Null string is found at starting position.  */
  if (len == 0)
    {
      set_search_regs (pos, 0);
      return pos;
    }

  /* Searching 0 times means don't move.  */
  if (n == 0)
    return pos;

  if (RE && !trivial_regexp_p (string))
    {
      struct re_pattern_buffer *bufp;

      bufp = compile_pattern (string, &search_regs, (char *) trt, posix);

      immediate_quit = 1;	/* Quit immediately if user types ^G,
				   because letting this function finish
				   can take too long. */
      QUIT;			/* Do a pending quit right away,
				   to avoid paradoxical behavior */
      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BEGV_ADDR;
      s1 = GPT - BEGV;
      p2 = GAP_END_ADDR;
      s2 = ZV - GPT;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV - BEGV;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV - BEGV;
	  s2 = 0;
	}
      while (n < 0)
	{
	  int val;
	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos - BEGV, lim - pos, &search_regs,
			     /* Don't allow match past current point */
			     pos - BEGV);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < search_regs.num_regs; i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      XSETBUFFER (last_thing_searched, current_buffer);
	      /* Set pos to the new position. */
	      pos = search_regs.start[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (n);
	    }
	  n++;
	}
      while (n > 0)
	{
	  int val;
	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos - BEGV, lim - pos, &search_regs,
			     lim - BEGV);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < search_regs.num_regs; i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      XSETBUFFER (last_thing_searched, current_buffer);
	      pos = search_regs.end[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (0 - n);
	    }
	  n--;
	}
      immediate_quit = 0;
      return (pos);
    }
  else				/* non-RE case */
    {
#ifdef C_ALLOCA
      int BM_tab_space[0400];
      BM_tab = &BM_tab_space[0];
#else
      BM_tab = (int *) alloca (0400 * sizeof (int));
#endif
      {
	unsigned char *patbuf = (unsigned char *) alloca (len);
	pat = patbuf;
	while (--len >= 0)
	  {
	    /* If we got here and the RE flag is set, it's because we're
	       dealing with a regexp known to be trivial, so the backslash
	       just quotes the next character.  */
	    if (RE && *base_pat == '\\')
	      {
		len--;
		base_pat++;
	      }
	    *pat++ = (trt ? trt[*base_pat++] : *base_pat++);
	  }
	len = pat - patbuf;
	pat = base_pat = patbuf;
      }
      /* The general approach is that we are going to maintain that we know */
      /* the first (closest to the present position, in whatever direction */
      /* we're searching) character that could possibly be the last */
      /* (furthest from present position) character of a valid match.  We */
      /* advance the state of our knowledge by looking at that character */
      /* and seeing whether it indeed matches the last character of the */
      /* pattern.  If it does, we take a closer look.  If it does not, we */
      /* move our pointer (to putative last characters) as far as is */
      /* logically possible.  This amount of movement, which I call a */
      /* stride, will be the length of the pattern if the actual character */
      /* appears nowhere in the pattern, otherwise it will be the distance */
      /* from the last occurrence of that character to the end of the */
      /* pattern. */
      /* As a coding trick, an enormous stride is coded into the table for */
      /* characters that match the last character.  This allows use of only */
      /* a single test, a test for having gone past the end of the */
      /* permissible match region, to test for both possible matches (when */
      /* the stride goes past the end immediately) and failure to */
      /* match (where you get nudged past the end one stride at a time). */ 

      /* Here we make a "mickey mouse" BM table.  The stride of the search */
      /* is determined only by the last character of the putative match. */
      /* If that character does not match, we will stride the proper */
      /* distance to propose a match that superimposes it on the last */
      /* instance of a character that matches it (per trt), or misses */
      /* it entirely if there is none. */  

      dirlen = len * direction;
      infinity = dirlen - (lim + pos + len + len) * direction;
      if (direction < 0)
	pat = (base_pat += len - 1);
      BM_tab_base = BM_tab;
      BM_tab += 0400;
      j = dirlen;		/* to get it in a register */
      /* A character that does not appear in the pattern induces a */
      /* stride equal to the pattern length. */
      while (BM_tab_base != BM_tab)
	{
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	}
      i = 0;
      while (i != infinity)
	{
	  j = pat[i]; i += direction;
	  if (i == dirlen) i = infinity;
	  if ((int) trt)
	    {
	      k = (j = trt[j]);
	      if (i == infinity)
		stride_for_teases = BM_tab[j];
	      BM_tab[j] = dirlen - i;
	      /* A translation table is accompanied by its inverse -- see */
	      /* comment following downcase_table for details */ 
	      while ((j = inverse_trt[j]) != k)
		BM_tab[j] = dirlen - i;
	    }
	  else
	    {
	      if (i == infinity)
		stride_for_teases = BM_tab[j];
	      BM_tab[j] = dirlen - i;
	    }
	  /* stride_for_teases tells how much to stride if we get a */
	  /* match on the far character but are subsequently */
	  /* disappointed, by recording what the stride would have been */
	  /* for that character if the last character had been */
	  /* different. */
	}
      infinity = dirlen - infinity;
      pos += dirlen - ((direction > 0) ? direction : 0);
      /* loop invariant - pos points at where last char (first char if reverse)
	 of pattern would align in a possible match.  */
      while (n != 0)
	{
	  /* It's been reported that some (broken) compiler thinks that
	     Boolean expressions in an arithmetic context are unsigned.
	     Using an explicit ?1:0 prevents this.  */
	  if ((lim - pos - ((direction > 0) ? 1 : 0)) * direction < 0)
	    return (n * (0 - direction));
	  /* First we do the part we can by pointers (maybe nothing) */
	  QUIT;
	  pat = base_pat;
	  limit = pos - dirlen + direction;
	  limit = ((direction > 0)
		   ? BUFFER_CEILING_OF (limit)
		   : BUFFER_FLOOR_OF (limit));
	  /* LIMIT is now the last (not beyond-last!) value
	     POS can take on without hitting edge of buffer or the gap.  */
	  limit = ((direction > 0)
		   ? min (lim - 1, min (limit, pos + 20000))
		   : max (lim, max (limit, pos - 20000)));
	  if ((limit - pos) * direction > 20)
	    {
	      p_limit = &FETCH_CHAR (limit);
	      p2 = (cursor = &FETCH_CHAR (pos));
	      /* In this loop, pos + cursor - p2 is the surrogate for pos */
	      while (1)		/* use one cursor setting as long as i can */
		{
		  if (direction > 0) /* worth duplicating */
		    {
		      /* Use signed comparison if appropriate
			 to make cursor+infinity sure to be > p_limit.
			 Assuming that the buffer lies in a range of addresses
			 that are all "positive" (as ints) or all "negative",
			 either kind of comparison will work as long
			 as we don't step by infinity.  So pick the kind
			 that works when we do step by infinity.  */
		      if ((int) (p_limit + infinity) > (int) p_limit)
			while ((int) cursor <= (int) p_limit)
			  cursor += BM_tab[*cursor];
		      else
			while ((unsigned int) cursor <= (unsigned int) p_limit)
			  cursor += BM_tab[*cursor];
		    }
		  else
		    {
		      if ((int) (p_limit + infinity) < (int) p_limit)
			while ((int) cursor >= (int) p_limit)
			  cursor += BM_tab[*cursor];
		      else
			while ((unsigned int) cursor >= (unsigned int) p_limit)
			  cursor += BM_tab[*cursor];
		    }
/* If you are here, cursor is beyond the end of the searched region. */
 /* This can happen if you match on the far character of the pattern, */
 /* because the "stride" of that character is infinity, a number able */
 /* to throw you well beyond the end of the search.  It can also */
 /* happen if you fail to match within the permitted region and would */
 /* otherwise try a character beyond that region */
		  if ((cursor - p_limit) * direction <= len)
		    break;	/* a small overrun is genuine */
		  cursor -= infinity; /* large overrun = hit */
		  i = dirlen - direction;
		  if ((int) trt)
		    {
		      while ((i -= direction) + direction != 0)
			if (pat[i] != trt[*(cursor -= direction)])
			  break;
		    }
		  else
		    {
		      while ((i -= direction) + direction != 0)
			if (pat[i] != *(cursor -= direction))
			  break;
		    }
		  cursor += dirlen - i - direction;	/* fix cursor */
		  if (i + direction == 0)
		    {
		      cursor -= direction;

		      set_search_regs (pos + cursor - p2 + ((direction > 0)
							    ? 1 - len : 0),
				       len);

		      if ((n -= direction) != 0)
			cursor += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    cursor += stride_for_teases; /* <sigh> we lose -  */
		}
	      pos += cursor - p2;
	    }
	  else
	    /* Now we'll pick up a clump that has to be done the hard */
	    /* way because it covers a discontinuity */
	    {
	      limit = ((direction > 0)
		       ? BUFFER_CEILING_OF (pos - dirlen + 1)
		       : BUFFER_FLOOR_OF (pos - dirlen - 1));
	      limit = ((direction > 0)
		       ? min (limit + len, lim - 1)
		       : max (limit - len, lim));
	      /* LIMIT is now the last value POS can have
		 and still be valid for a possible match.  */
	      while (1)
		{
		  /* This loop can be coded for space rather than */
		  /* speed because it will usually run only once. */
		  /* (the reach is at most len + 21, and typically */
		  /* does not exceed len) */    
		  while ((limit - pos) * direction >= 0)
		    pos += BM_tab[FETCH_CHAR(pos)];
		  /* now run the same tests to distinguish going off the */
		  /* end, a match or a phony match. */
		  if ((pos - limit) * direction <= len)
		    break;	/* ran off the end */
		  /* Found what might be a match.
		     Set POS back to last (first if reverse) char pos.  */
		  pos -= infinity;
		  i = dirlen - direction;
		  while ((i -= direction) + direction != 0)
		    {
		      pos -= direction;
		      if (pat[i] != (((int) trt)
				     ? trt[FETCH_CHAR(pos)]
				     : FETCH_CHAR (pos)))
			break;
		    }
		  /* Above loop has moved POS part or all the way
		     back to the first char pos (last char pos if reverse).
		     Set it once again at the last (first if reverse) char.  */
		  pos += dirlen - i- direction;
		  if (i + direction == 0)
		    {
		      pos -= direction;

		      set_search_regs (pos + ((direction > 0) ? 1 - len : 0),
				       len);

		      if ((n -= direction) != 0)
			pos += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    pos += stride_for_teases;
		}
	      }
	  /* We have done one clump.  Can we continue? */
	  if ((lim - pos) * direction < 0)
	    return ((0 - n) * direction);
	}
      return pos;
    }
}

/* Record beginning BEG and end BEG + LEN
   for a match just found in the current buffer.  */

static void
set_search_regs (beg, len)
     int beg, len;
{
  /* Make sure we have registers in which to store
     the match position.  */
  if (search_regs.num_regs == 0)
    {
      regoff_t *starts, *ends;

      starts = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
      ends = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
      search_regs.num_regs = 2;
    }

  search_regs.start[0] = beg;
  search_regs.end[0] = beg + len;
  XSETBUFFER (last_thing_searched, current_buffer);
}

/* Given a string of words separated by word delimiters,
  compute a regexp that matches those exact words
  separated by arbitrary punctuation.  */

static Lisp_Object
wordify (string)
     Lisp_Object string;
{
  register unsigned char *p, *o;
  register int i, len, punct_count = 0, word_count = 0;
  Lisp_Object val;

  CHECK_STRING (string, 0);
  p = XSTRING (string)->data;
  len = XSTRING (string)->size;

  for (i = 0; i < len; i++)
    if (SYNTAX (p[i]) != Sword)
      {
	punct_count++;
	if (i > 0 && SYNTAX (p[i-1]) == Sword) word_count++;
      }
  if (SYNTAX (p[len-1]) == Sword) word_count++;
  if (!word_count) return build_string ("");

  val = make_string (p, len - punct_count + 5 * (word_count - 1) + 4);

  o = XSTRING (val)->data;
  *o++ = '\\';
  *o++ = 'b';

  for (i = 0; i < len; i++)
    if (SYNTAX (p[i]) == Sword)
      *o++ = p[i];
    else if (i > 0 && SYNTAX (p[i-1]) == Sword && --word_count)
      {
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '*';
      }

  *o++ = '\\';
  *o++ = 'b';

  return val;
}

DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4,
  "sSearch backward: ",
  "Search backward from point for STRING.\n\
Set point to the beginning of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
 If not nil and not t, position at limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, -1, 0, 0);
}

DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "sSearch: ",
  "Search forward from point for STRING.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.  nil is equivalent\n\
  to (point-max).\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, 1, 0, 0);
}

DEFUN ("word-search-backward", Fword_search_backward, Sword_search_backward, 1, 4,
  "sWord search backward: ",
  "Search backward from point for STRING, ignoring differences in punctuation.\n\
Set point to the beginning of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, -1, 1, 0);
}

DEFUN ("word-search-forward", Fword_search_forward, Sword_search_forward, 1, 4,
  "sWord search: ",
  "Search forward from point for STRING, ignoring differences in punctuation.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, 1, 1, 0);
}

DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4,
  "sRE search backward: ",
  "Search backward from point for match for regular expression REGEXP.\n\
Set point to the beginning of the match, and return point.\n\
The match found is the one starting last in the buffer\n\
and yet ending before the origin of the search.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must start at or after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, -1, 1, 0);
}

DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
  "sRE search: ",
  "Search forward from point for regular expression REGEXP.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, 1, 1, 0);
}

DEFUN ("posix-search-backward", Fposix_search_backward, Sposix_search_backward, 1, 4,
  "sPosix search backward: ",
  "Search backward from point for match for regular expression REGEXP.\n\
Find the longest match in accord with Posix regular expression rules.\n\
Set point to the beginning of the match, and return point.\n\
The match found is the one starting last in the buffer\n\
and yet ending before the origin of the search.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must start at or after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, -1, 1, 1);
}

DEFUN ("posix-search-forward", Fposix_search_forward, Sposix_search_forward, 1, 4,
  "sPosix search: ",
  "Search forward from point for regular expression REGEXP.\n\
Find the longest match in accord with Posix regular expression rules.\n\
Set point to the end of the occurrence found, and return point.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions `match-beginning', `match-end' and `replace-match'.")
  (regexp, bound, noerror, count)
     Lisp_Object regexp, bound, noerror, count;
{
  return search_command (regexp, bound, noerror, count, 1, 1, 1);
}

DEFUN ("replace-match", Freplace_match, Sreplace_match, 1, 4, 0,
  "Replace text matched by last search with NEWTEXT.\n\
If second arg FIXEDCASE is non-nil, do not alter case of replacement text.\n\
Otherwise maybe capitalize the whole text, or maybe just word initials,\n\
based on the replaced text.\n\
If the replaced text has only capital letters\n\
and has at least one multiletter word, convert NEWTEXT to all caps.\n\
If the replaced text has at least one word starting with a capital letter,\n\
then capitalize each word in NEWTEXT.\n\n\
If third arg LITERAL is non-nil, insert NEWTEXT literally.\n\
Otherwise treat `\\' as special:\n\
  `\\&' in NEWTEXT means substitute original matched text.\n\
  `\\N' means substitute what matched the Nth `\\(...\\)'.\n\
       If Nth parens didn't match, substitute nothing.\n\
  `\\\\' means insert one `\\'.\n\
FIXEDCASE and LITERAL are optional arguments.\n\
Leaves point at end of replacement text.\n\
\n\
The optional fourth argument STRING can be a string to modify.\n\
In that case, this function creates and returns a new string\n\
which is made by replacing the part of STRING that was matched.")
  (newtext, fixedcase, literal, string)
     Lisp_Object newtext, fixedcase, literal, string;
{
  enum { nochange, all_caps, cap_initial } case_action;
  register int pos, last;
  int some_multiletter_word;
  int some_lowercase;
  int some_uppercase;
  int some_nonuppercase_initial;
  register int c, prevc;
  int inslen;

  CHECK_STRING (newtext, 0);

  if (! NILP (string))
    CHECK_STRING (string, 4);

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */

  if (search_regs.num_regs <= 0)
    error ("replace-match called before any match found");

  if (NILP (string))
    {
      if (search_regs.start[0] < BEGV
	  || search_regs.start[0] > search_regs.end[0]
	  || search_regs.end[0] > ZV)
	args_out_of_range (make_number (search_regs.start[0]),
			   make_number (search_regs.end[0]));
    }
  else
    {
      if (search_regs.start[0] < 0
	  || search_regs.start[0] > search_regs.end[0]
	  || search_regs.end[0] > XSTRING (string)->size)
	args_out_of_range (make_number (search_regs.start[0]),
			   make_number (search_regs.end[0]));
    }

  if (NILP (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */

      last = search_regs.end[0];
      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;
      some_lowercase = 0;
      some_nonuppercase_initial = 0;
      some_uppercase = 0;

      for (pos = search_regs.start[0]; pos < last; pos++)
	{
	  if (NILP (string))
	    c = FETCH_CHAR (pos);
	  else
	    c = XSTRING (string)->data[pos];

	  if (LOWERCASEP (c))
	    {
	      /* Cannot be all caps if any original char is lower case */

	      some_lowercase = 1;
	      if (SYNTAX (prevc) != Sword)
		some_nonuppercase_initial = 1;
	      else
		some_multiletter_word = 1;
	    }
	  else if (!NOCASEP (c))
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
			   make_number (search_regs.start[0]));
      after = Fsubstring (string, make_number (search_regs.end[0]), Qnil);

      /* Do case substitution into NEWTEXT if desired.  */
      if (NILP (literal))
	{
	  int lastpos = -1;
	  /* We build up the substituted string in ACCUM.  */
	  Lisp_Object accum;
	  Lisp_Object middle;

	  accum = Qnil;

	  for (pos = 0; pos < XSTRING (newtext)->size; pos++)
	    {
	      int substart = -1;
	      int subend;

	      c = XSTRING (newtext)->data[pos];
	      if (c == '\\')
		{
		  c = XSTRING (newtext)->data[++pos];
		  if (c == '&')
		    {
		      substart = search_regs.start[0];
		      subend = search_regs.end[0];
		    }
		  else if (c >= '1' && c <= '9' && c <= search_regs.num_regs + '0')
		    {
		      if (search_regs.start[c - '0'] >= 1)
			{
			  substart = search_regs.start[c - '0'];
			  subend = search_regs.end[c - '0'];
			}
		    }
		}
	      if (substart >= 0)
		{
		  if (pos - 1 != lastpos + 1)
		    middle = Fsubstring (newtext, lastpos + 1, pos - 1);
		  else
		    middle = Qnil;
		  accum = concat3 (accum, middle,
				   Fsubstring (string, make_number (substart),
					       make_number (subend)));
		  lastpos = pos;
		}
	    }

	  if (pos != lastpos + 1)
	    middle = Fsubstring (newtext, lastpos + 1, pos);
	  else
	    middle = Qnil;

	  newtext = concat2 (accum, middle);
	}

      if (case_action == all_caps)
	newtext = Fupcase (newtext);
      else if (case_action == cap_initial)
	newtext = upcase_initials (newtext);

      return concat3 (before, newtext, after);
    }

  /* We insert the replacement text before the old text, and then
     delete the original text.  This means that markers at the
     beginning or end of the original will float to the corresponding
     position in the replacement.  */
  SET_PT (search_regs.start[0]);
  if (!NILP (literal))
    Finsert_and_inherit (1, &newtext);
  else
    {
      struct gcpro gcpro1;
      GCPRO1 (newtext);

      for (pos = 0; pos < XSTRING (newtext)->size; pos++)
	{
	  int offset = point - search_regs.start[0];

	  c = XSTRING (newtext)->data[pos];
	  if (c == '\\')
	    {
	      c = XSTRING (newtext)->data[++pos];
	      if (c == '&')
		Finsert_buffer_substring
		  (Fcurrent_buffer (),
		   make_number (search_regs.start[0] + offset),
		   make_number (search_regs.end[0] + offset));
	      else if (c >= '1' && c <= '9' && c <= search_regs.num_regs + '0')
		{
		  if (search_regs.start[c - '0'] >= 1)
		    Finsert_buffer_substring
		      (Fcurrent_buffer (),
		       make_number (search_regs.start[c - '0'] + offset),
		       make_number (search_regs.end[c - '0'] + offset));
		}
	      else
		insert_char (c);
	    }
	  else
	    insert_char (c);
	}
      UNGCPRO;
    }

  inslen = point - (search_regs.start[0]);
  del_range (search_regs.start[0] + inslen, search_regs.end[0] + inslen);

  if (case_action == all_caps)
    Fupcase_region (make_number (point - inslen), make_number (point));
  else if (case_action == cap_initial)
    upcase_initials_region (make_number (point - inslen), make_number (point));
  return Qnil;
}

static Lisp_Object
match_limit (num, beginningp)
     Lisp_Object num;
     int beginningp;
{
  register int n;

  CHECK_NUMBER (num, 0);
  n = XINT (num);
  if (n < 0 || n >= search_regs.num_regs)
    args_out_of_range (num, make_number (search_regs.num_regs));
  if (search_regs.num_regs <= 0
      || search_regs.start[n] < 0)
    return Qnil;
  return (make_number ((beginningp) ? search_regs.start[n]
		                    : search_regs.end[n]));
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
  "Return position of start of text matched by last search.\n\
NUM specifies which parenthesized expression in the last regexp.\n\
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.\n\
Zero means the entire text matched by the whole regexp or whole string.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
  "Return position of end of text matched by last search.\n\
ARG, a number, specifies which parenthesized expression in the last regexp.\n\
 Value is nil if ARGth pair didn't match, or there were less than ARG pairs.\n\
Zero means the entire text matched by the whole regexp or whole string.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 0);
} 

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 0, 0,
  "Return a list containing all info on what the last search matched.\n\
Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.\n\
All the elements are markers or nil (nil if the Nth pair didn't match)\n\
if the last match was on a buffer; integers or nil if a string was matched.\n\
Use `store-match-data' to reinstate the data in this list.")
  ()
{
  Lisp_Object *data;
  int i, len;

  if (NILP (last_thing_searched))
    error ("match-data called before any match found");

  data = (Lisp_Object *) alloca ((2 * search_regs.num_regs)
				 * sizeof (Lisp_Object));

  len = -1;
  for (i = 0; i < search_regs.num_regs; i++)
    {
      int start = search_regs.start[i];
      if (start >= 0)
	{
	  if (EQ (last_thing_searched, Qt))
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
	    abort ();

	  len = i;
	}
      else
	data[2 * i] = data [2 * i + 1] = Qnil;
    }
  return Flist (2 * len + 2, data);
}


DEFUN ("store-match-data", Fstore_match_data, Sstore_match_data, 1, 1, 0,
  "Set internal data on last search match from elements of LIST.\n\
LIST should have been created by calling `match-data' previously.")
  (list)
     register Lisp_Object list;
{
  register int i;
  register Lisp_Object marker;

  if (running_asynch_code)
    save_search_regs ();

  if (!CONSP (list) && !NILP (list))
    list = wrong_type_argument (Qconsp, list);

  /* Unless we find a marker with a buffer in LIST, assume that this 
     match data came from a string.  */
  last_thing_searched = Qt;

  /* Allocate registers if they don't already exist.  */
  {
    int length = XFASTINT (Flength (list)) / 2;

    if (length > search_regs.num_regs)
      {
	if (search_regs.num_regs == 0)
	  {
	    search_regs.start
	      = (regoff_t *) xmalloc (length * sizeof (regoff_t));
	    search_regs.end
	      = (regoff_t *) xmalloc (length * sizeof (regoff_t));
	  }
	else
	  {
	    search_regs.start
	      = (regoff_t *) xrealloc (search_regs.start,
				       length * sizeof (regoff_t));
	    search_regs.end
	      = (regoff_t *) xrealloc (search_regs.end,
				       length * sizeof (regoff_t));
	  }

	search_regs.num_regs = length;
      }
  }

  for (i = 0; i < search_regs.num_regs; i++)
    {
      marker = Fcar (list);
      if (NILP (marker))
	{
	  search_regs.start[i] = -1;
	  list = Fcdr (list);
	}
      else
	{
	  if (MARKERP (marker))
	    {
	      if (XMARKER (marker)->buffer == 0)
		XSETFASTINT (marker, 0);
	      else
		XSETBUFFER (last_thing_searched, XMARKER (marker)->buffer);
	    }

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.start[i] = XINT (marker);
	  list = Fcdr (list);

	  marker = Fcar (list);
	  if (MARKERP (marker) && XMARKER (marker)->buffer == 0)
	    XSETFASTINT (marker, 0);

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.end[i] = XINT (marker);
	}
      list = Fcdr (list);
    }

  return Qnil;  
}

/* If non-zero the match data have been saved in saved_search_regs
   during the execution of a sentinel or filter. */
static int search_regs_saved;
static struct re_registers saved_search_regs;

/* Called from Flooking_at, Fstring_match, search_buffer, Fstore_match_data
   if asynchronous code (filter or sentinel) is running. */
static void
save_search_regs ()
{
  if (!search_regs_saved)
    {
      saved_search_regs.num_regs = search_regs.num_regs;
      saved_search_regs.start = search_regs.start;
      saved_search_regs.end = search_regs.end;
      search_regs.num_regs = 0;

      search_regs_saved = 1;
    }
}

/* Called upon exit from filters and sentinels. */
void
restore_match_data ()
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

      search_regs_saved = 0;
    }
}

/* Quote a string to inactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, Sregexp_quote, 1, 1, 0,
  "Return a regexp string which matches exactly STRING and nothing else.")
  (str)
     Lisp_Object str;
{
  register unsigned char *in, *out, *end;
  register unsigned char *temp;

  CHECK_STRING (str, 0);

  temp = (unsigned char *) alloca (XSTRING (str)->size * 2);

  /* Now copy the data into the new string, inserting escapes. */

  in = XSTRING (str)->data;
  end = in + XSTRING (str)->size;
  out = temp; 

  for (; in != end; in++)
    {
      if (*in == '[' || *in == ']'
	  || *in == '*' || *in == '.' || *in == '\\'
	  || *in == '?' || *in == '+'
	  || *in == '^' || *in == '$')
	*out++ = '\\';
      *out++ = *in;
    }

  return make_string (temp, out - temp);
}
  
syms_of_search ()
{
  register int i;

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    {
      searchbufs[i].buf.allocated = 100;
      searchbufs[i].buf.buffer = (unsigned char *) malloc (100);
      searchbufs[i].buf.fastmap = searchbufs[i].fastmap;
      searchbufs[i].regexp = Qnil;
      staticpro (&searchbufs[i].regexp);
      searchbufs[i].next = (i == REGEXP_CACHE_SIZE-1 ? 0 : &searchbufs[i+1]);
    }
  searchbuf_head = &searchbufs[0];

  Qsearch_failed = intern ("search-failed");
  staticpro (&Qsearch_failed);
  Qinvalid_regexp = intern ("invalid-regexp");
  staticpro (&Qinvalid_regexp);

  Fput (Qsearch_failed, Qerror_conditions,
	Fcons (Qsearch_failed, Fcons (Qerror, Qnil)));
  Fput (Qsearch_failed, Qerror_message,
	build_string ("Search failed"));

  Fput (Qinvalid_regexp, Qerror_conditions,
	Fcons (Qinvalid_regexp, Fcons (Qerror, Qnil)));
  Fput (Qinvalid_regexp, Qerror_message,
	build_string ("Invalid regexp"));

  last_thing_searched = Qnil;
  staticpro (&last_thing_searched);

  defsubr (&Slooking_at);
  defsubr (&Sposix_looking_at);
  defsubr (&Sstring_match);
  defsubr (&Sposix_string_match);
  defsubr (&Sskip_chars_forward);
  defsubr (&Sskip_chars_backward);
  defsubr (&Sskip_syntax_forward);
  defsubr (&Sskip_syntax_backward);
  defsubr (&Ssearch_forward);
  defsubr (&Ssearch_backward);
  defsubr (&Sword_search_forward);
  defsubr (&Sword_search_backward);
  defsubr (&Sre_search_forward);
  defsubr (&Sre_search_backward);
  defsubr (&Sposix_search_forward);
  defsubr (&Sposix_search_backward);
  defsubr (&Sreplace_match);
  defsubr (&Smatch_beginning);
  defsubr (&Smatch_end);
  defsubr (&Smatch_data);
  defsubr (&Sstore_match_data);
  defsubr (&Sregexp_quote);
}
