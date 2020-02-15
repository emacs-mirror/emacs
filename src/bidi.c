/* Low-level bidirectional buffer/string-scanning functions for GNU Emacs.

Copyright (C) 2000-2001, 2004-2005, 2009-2020 Free Software Foundation,
Inc.

Author: Eli Zaretskii <eliz@gnu.org>

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

/* A sequential implementation of the Unicode Bidirectional algorithm,
   (UBA) as per UAX#9, a part of the Unicode Standard.

   Unlike the Reference Implementation and most other implementations,
   this one is designed to be called once for every character in the
   buffer or string.  That way, we can leave intact the design of the
   Emacs display engine, whereby an iterator object is used to
   traverse buffer or string text character by character, and generate
   the necessary data for displaying each character in 'struct glyph'
   objects.  (See xdisp.c for the details of that iteration.)  The
   functions on this file replace the original linear iteration in the
   logical order of the text with a non-linear iteration in the visual
   order, i.e. in the order characters should be shown on display.

   The main entry point is bidi_move_to_visually_next.  Each time it
   is called, it finds the next character in the visual order, and
   returns its information in a special structure.  The caller is then
   expected to process this character for display or any other
   purposes, and call bidi_move_to_visually_next for the next
   character.  See the comments in bidi_move_to_visually_next for more
   details about its algorithm that finds the next visual-order
   character by resolving their levels on the fly.

   Two other entry points are bidi_paragraph_init and
   bidi_mirror_char.  The first determines the base direction of a
   paragraph, while the second returns the mirrored version of its
   argument character.

   A few auxiliary entry points are used to initialize the bidi
   iterator for iterating an object (buffer or string), push and pop
   the bidi iterator state, and save and restore the state of the bidi
   cache.

   If you want to understand the code, you will have to read it
   together with the relevant portions of UAX#9.  The comments include
   references to UAX#9 rules, for that very reason.

   A note about references to UAX#9 rules: if the reference says
   something like "X9/Retaining", it means that you need to refer to
   rule X9 and to its modifications described in the "Implementation
   Notes" section of UAX#9, under "Retaining Format Codes".

   Here's the overview of the design of the reordering engine
   implemented by this file.

   Basic implementation structure
   ------------------------------

   The sequential processing steps described by UAX#9 are implemented
   as recursive levels of processing, all of which examine the next
   character in the logical order.  This hierarchy of processing looks
   as follows, from the innermost (deepest) to the outermost level,
   omitting some subroutines used by each level:

     bidi_fetch_char         -- fetch next character
     bidi_resolve_explicit   -- resolve explicit levels and directions
     bidi_resolve_weak       -- resolve weak types
     bidi_resolve_brackets   -- resolve "paired brackets" neutral types
     bidi_resolve_neutral    -- resolve neutral types
     bidi_level_of_next_char -- resolve implicit levels

   Each level calls the level below it, and works on the result
   returned by the lower level, including all of its sub-levels.

   Unlike all the levels below it, bidi_level_of_next_char can return
   the information about either the next or previous character in the
   logical order, depending on the current direction of scanning the
   buffer or string.  For the next character, it calls all the levels
   below it; for the previous character, it uses the cache, described
   below.

   Thus, the result of calling bidi_level_of_next_char is the resolved
   level of the next or the previous character in the logical order.
   Based on this information, the function bidi_move_to_visually_next
   finds the next character in the visual order and updates the
   direction in which the buffer is scanned, either forward or
   backward, to find the next character to be displayed.  (Text is
   scanned backwards when it needs to be reversed for display, i.e. if
   the visual order is the inverse of the logical order.)  This
   implements the last, reordering steps of the UBA, by successively
   calling bidi_level_of_next_char until the character of the required
   embedding level is found; the scan direction is dynamically updated
   as a side effect.  See the commentary before the 'while' loop in
   bidi_move_to_visually_next, for the details.

   Fetching characters
   -------------------

   In a nutshell, fetching the next character boils down to calling
   STRING_CHAR_AND_LENGTH, passing it the address of a buffer or
   string position.  See bidi_fetch_char.  However, if the next
   character is "covered" by a display property of some kind,
   bidi_fetch_char returns the u+FFFC "object replacement character"
   that represents the entire run of text covered by the display
   property.  (The ch_len and nchars members of 'struct bidi_it'
   reflect the length in bytes and characters of that text.)  This is
   so we reorder text on both sides of the display property as
   appropriate for an image or embedded string.  Similarly, text
   covered by a display spec of the form '(space ...)', is replaced
   with the u+2029 paragraph separator character, so such display
   specs produce the same effect as a TAB under UBA.  Both these
   special characters are not actually displayed -- the display
   property is displayed instead -- but just used to compute the
   embedding level of the surrounding text so as to produce the
   required effect.

   Bidi iterator states
   --------------------

   The UBA is highly context dependent in some of its parts,
   i.e. results of processing a character can generally depend on
   characters very far away.  The UAX#9 description of the UBA
   prescribes a stateful processing of each character, whereby the
   results of this processing depend on various state variables, such
   as the current embedding level, level stack, and directional
   override status.  In addition, the UAX#9 description includes many
   passages like this (from rule W2 in this case):

     Search backward from each instance of a European number until the
     first strong type (R, L, AL, or sos) is found. If an AL is found,
     change the type of the European number to Arabic number.

   To support this, we use a bidi iterator object, 'struct bidi_it',
   which is a sub-structure of 'struct it' used by xdisp.c (see
   dispextern.h for the definition of both of these structures).  The
   bidi iterator holds the entire state of the iteration required by
   the UBA, and is updated as the text is traversed.  In particular,
   the embedding level of the current character being resolved is
   recorded in the iterator state.  To avoid costly searches backward
   in support of rules like W2 above, the necessary character types
   are also recorded in the iterator state as they are found during
   the forward scan, and then used when such rules need to be applied.
   (Forward scans cannot be avoided in this way; they need to be
   performed at least once, and the results recorded in the iterator
   state, to be reused until the forward scan oversteps the recorded
   position.)

   In this manner, the iterator state acts as a mini-cache of
   contextual information required for resolving the level of the
   current character by various UBA rules.

   Caching of bidi iterator states
   -------------------------------

   As described above, the reordering engine uses the information
   recorded in the bidi iterator state in order to resolve the
   embedding level of the current character.  When the reordering
   engine needs to process the next character in the logical order, it
   fetches it and applies to it all the UBA levels, updating the
   iterator state as it goes.  But when the buffer or string is
   scanned backwards, i.e. in the reverse order of buffer/string
   positions, the scanned characters were already processed during the
   preceding forward scan (see bidi_find_other_level_edge).  To avoid
   costly re-processing of characters that were already processed
   during the forward scan, the iterator states computed while
   scanning forward are cached.

   The cache is just a linear array of 'struct bidi_it' objects, which
   is dynamically allocated and reallocated as needed, since the size
   of the cache depends on the text being processed.  We only need the
   cache while processing embedded levels higher than the base
   paragraph embedding level, because these higher levels require
   changes in scan direction.  Therefore, as soon as we are back to
   the base embedding level, we can free the cache; see the calls to
   bidi_cache_reset and bidi_cache_shrink, for the conditions to do
   this.

   The cache maintains the index of the next unused cache slot -- this
   is where the next iterator state will be cached.  The function
   bidi_cache_iterator_state saves an instance of the state in the
   cache and increments the unused slot index.  The companion function
   bidi_cache_find looks up a cached state that corresponds to a given
   buffer/string position.  All of the cached states must correspond
   1:1 to the buffer or string region whose processing they reflect;
   bidi.c will abort if it finds cache slots that violate this 1:1
   correspondence.

   When the parent iterator 'struct it' is pushed (see push_it in
   xdisp.c) to pause the current iteration and start iterating over a
   different object (e.g., a 'display' string that covers some buffer
   text), the bidi iterator cache needs to be "pushed" as well, so
   that a new empty cache could be used while iterating over the new
   object.  Later, when the new object is exhausted, and xdisp.c calls
   pop_it, we need to "pop" the bidi cache as well and return to the
   original cache.  See bidi_push_it and bidi_pop_it for how this is
   done.

   Some functions of the display engine save copies of 'struct it' in
   local variables, and restore them later.  For examples, see
   pos_visible_p and move_it_in_display_line_to in xdisp.c, and
   window_scroll_pixel_based in window.c.  When this happens, we need
   to save and restore the bidi cache as well, because conceptually
   the cache is part of the 'struct it' state, and needs to be in
   perfect sync with the portion of the buffer/string that is being
   processed.  This saving and restoring of the cache state is handled
   by bidi_shelve_cache and bidi_unshelve_cache, and the helper macros
   SAVE_IT and RESTORE_IT defined on xdisp.c.

   Note that, because reordering is implemented below the level in
   xdisp.c that breaks glyphs into screen lines, we are violating
   paragraph 3.4 of UAX#9. which mandates that line breaking shall be
   done before reordering each screen line separately.  However,
   following UAX#9 to the letter in this matter goes against the basic
   design of the Emacs display engine, and so we choose here this
   minor deviation from the UBA letter in preference to redesign of
   the display engine.  The effect of this is only seen in continued
   lines that are broken into screen lines in the middle of a run
   whose direction is opposite to the paragraph's base direction.

   Important design and implementation note: when the code needs to
   scan far ahead, be sure to avoid such scans as much as possible
   when the buffer/string doesn't contain any RTL characters.  Users
   of left-to-right scripts will never forgive you if you introduce
   some slow-down due to bidi in situations that don't involve any
   bidirectional text.  See the large comment near the beginning of
   bidi_resolve_neutral, for one situation where such shortcut was
   necessary.  */

#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "dispextern.h"
#include "region-cache.h"
#include "sysstdio.h"

static bool bidi_initialized = 0;

static Lisp_Object bidi_type_table, bidi_mirror_table, bidi_brackets_table;

#define BIDI_EOB   (-1)

/* Data type for describing the bidirectional character categories.  */
typedef enum {
  UNKNOWN_BC,
  NEUTRAL,
  WEAK,
  STRONG,
  EXPLICIT_FORMATTING
} bidi_category_t;

static Lisp_Object paragraph_start_re, paragraph_separate_re;


/***********************************************************************
			Utilities
 ***********************************************************************/

/* Return the bidi type of a character CH, subject to the current
   directional OVERRIDE.  */
static bidi_type_t
bidi_get_type (int ch, bidi_dir_t override)
{
  bidi_type_t default_type;

  if (ch == BIDI_EOB)
    return NEUTRAL_B;
  if (ch < 0 || ch > MAX_CHAR)
    emacs_abort ();

  default_type = (bidi_type_t) XFIXNUM (CHAR_TABLE_REF (bidi_type_table, ch));
  /* Every valid character code, even those that are unassigned by the
     UCD, have some bidi-class property, according to
     DerivedBidiClass.txt file.  Therefore, if we ever get UNKNOWN_BT
     (= zero) code from CHAR_TABLE_REF, that's a bug.  */
  if (default_type == UNKNOWN_BT)
    emacs_abort ();

  switch (default_type)
    {
      case WEAK_BN:
      case NEUTRAL_B:
      case LRE:
      case LRO:
      case RLE:
      case RLO:
      case PDF:
      case LRI:
      case RLI:
      case FSI:
      case PDI:
	return default_type;
      default:
	if (override == L2R)
	  return STRONG_L;
	else if (override == R2L)
	  return STRONG_R;
	else
	  return default_type;
    }
}

static void
bidi_check_type (bidi_type_t type)
{
  eassert (UNKNOWN_BT <= type && type <= NEUTRAL_ON);
}

/* Given a bidi TYPE of a character, return its category.  */
static bidi_category_t
bidi_get_category (bidi_type_t type)
{
  switch (type)
    {
      case UNKNOWN_BT:
	return UNKNOWN_BC;
      case STRONG_L:
      case STRONG_R:
      case STRONG_AL:
	return STRONG;
      case WEAK_EN:
      case WEAK_ES:
      case WEAK_ET:
      case WEAK_AN:
      case WEAK_CS:
      case WEAK_NSM:
      case WEAK_BN:
	return WEAK;
      case NEUTRAL_B:
      case NEUTRAL_S:
      case NEUTRAL_WS:
      case NEUTRAL_ON:
	return NEUTRAL;
      case LRE:
      case LRO:
      case RLE:
      case RLO:
      case PDF:
      case LRI:
      case RLI:
      case FSI:
      case PDI:
	return EXPLICIT_FORMATTING;
      default:
	emacs_abort ();
    }
}

static bool
bidi_isolate_fmt_char (bidi_type_t ch_type)
{
  return (ch_type == LRI || ch_type == RLI || ch_type == PDI || ch_type == FSI);
}

/* Return the mirrored character of C, if it has one.  If C has no
   mirrored counterpart, return C.
   Note: The conditions in UAX#9 clause L4 regarding the surrounding
   context must be tested by the caller.  */
int
bidi_mirror_char (int c)
{
  Lisp_Object val;

  if (c == BIDI_EOB)
    return c;
  if (c < 0 || c > MAX_CHAR)
    emacs_abort ();

  val = CHAR_TABLE_REF (bidi_mirror_table, c);
  if (FIXNUMP (val))
    {
      int v;

      /* When debugging, check before assigning to V, so that the check
	 isn't broken by undefined behavior due to int overflow.  */
      eassert (CHAR_VALID_P (XFIXNUM (val)));

      v = XFIXNUM (val);

      /* Minimal test we must do in optimized builds, to prevent weird
	 crashes further down the road.  */
      if (v < 0 || v > MAX_CHAR)
	emacs_abort ();

      return v;
    }

  return c;
}

/* Return the Bidi_Paired_Bracket_Type property of the character C.  */
static bidi_bracket_type_t
bidi_paired_bracket_type (int c)
{
  if (c == BIDI_EOB || bidi_inhibit_bpa)
    return BIDI_BRACKET_NONE;
  if (c < 0 || c > MAX_CHAR)
    emacs_abort ();

  return (bidi_bracket_type_t) XFIXNUM (CHAR_TABLE_REF (bidi_brackets_table, c));
}

/* Determine the start-of-sequence (sos) directional type given the two
   embedding levels on either side of the run boundary.  Also, update
   the saved info about previously seen characters, since that info is
   generally valid for a single level run.  */
static void
bidi_set_sos_type (struct bidi_it *bidi_it, int level_before, int level_after)
{
  int higher_level = (level_before > level_after ? level_before : level_after);

  /* FIXME: should the default sos direction be user selectable?  */
  bidi_it->sos = ((higher_level & 1) != 0 ? R2L : L2R); /* X10 */

  bidi_it->prev.type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.type = (bidi_it->sos == R2L ? STRONG_R : STRONG_L);
  bidi_it->prev_for_neutral.charpos = bidi_it->charpos;
  bidi_it->next_for_neutral.type
    = bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
}

#define ISOLATE_STATUS(BIDI_IT, IDX)  ((BIDI_IT)->level_stack[IDX].flags & 1)
#define OVERRIDE(BIDI_IT, IDX)  (((BIDI_IT)->level_stack[IDX].flags >> 1) & 3)

/* Push the current embedding level and override status; reset the
   current level to LEVEL and the current override status to OVERRIDE.  */
static void
bidi_push_embedding_level (struct bidi_it *bidi_it,
			   int level, bidi_dir_t override, bool isolate_status)
{
  struct bidi_stack *st;
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;

  bidi_it->stack_idx++;
  eassert (bidi_it->stack_idx < BIDI_MAXDEPTH+2+1);
  st = &bidi_it->level_stack[bidi_it->stack_idx];
  eassert (level <= (1 << 7));
  st->level = level;
  st->flags = (((override & 3) << 1) | (isolate_status != 0));
  if (isolate_status)
    {
      st->last_strong_type = bidi_it->last_strong.type;
      st->prev_for_neutral_type = bidi_it->prev_for_neutral.type;
      st->next_for_neutral_type = bidi_it->next_for_neutral.type;
      st->next_for_neutral_pos = bidi_it->next_for_neutral.charpos;
      st->flags |= ((bidi_it->sos == L2R ? 0 : 1) << 3);
    }
  /* We've got a new isolating sequence, compute the directional type
     of sos and initialize per-sequence variables (UAX#9, clause X10).  */
  bidi_set_sos_type (bidi_it, prev_level, level);
}

/* Pop from the stack the embedding level, the directional override
   status, and optionally saved information for the isolating run
   sequence.  Return the new level.  */
static int
bidi_pop_embedding_level (struct bidi_it *bidi_it)
{
  int level;

  /* UAX#9 says to ignore invalid PDFs (X7, last bullet)
     and PDIs (X6a, 2nd bullet).  */
  if (bidi_it->stack_idx > 0)
    {
      bool isolate_status = ISOLATE_STATUS (bidi_it, bidi_it->stack_idx);
      int old_level = bidi_it->level_stack[bidi_it->stack_idx].level;

      struct bidi_stack st;

      st = bidi_it->level_stack[bidi_it->stack_idx];
      if (isolate_status)
	{
	  bidi_dir_t sos = ((st.flags >> 3) & 1);
	  /* PREV is used in W1 for resolving WEAK_NSM.  By the time
	     we get to an NSM, we must have gotten past at least one
	     character: the PDI that ends the isolate from which we
	     are popping here.  So PREV will have been filled up by
	     the time we first use it.  We initialize it here to
	     UNKNOWN_BT to be able to catch any blunders in this
	     logic.  */
	  bidi_it->prev.orig_type = bidi_it->prev.type = UNKNOWN_BT;
	  bidi_it->last_strong.type = st.last_strong_type;
	  bidi_it->prev_for_neutral.type = st.prev_for_neutral_type;
	  bidi_it->next_for_neutral.type = st.next_for_neutral_type;
	  bidi_it->next_for_neutral.charpos = st.next_for_neutral_pos;
	  bidi_it->sos = (sos == 0 ? L2R : R2L);
	}
      else
	bidi_set_sos_type (bidi_it, old_level,
			   bidi_it->level_stack[bidi_it->stack_idx - 1].level);

      bidi_it->stack_idx--;
    }
  level = bidi_it->level_stack[bidi_it->stack_idx].level;
  eassert (0 <= level && level <= BIDI_MAXDEPTH + 1);
  return level;
}

/* Record in SAVED_INFO the information about the current character.  */
static void
bidi_remember_char (struct bidi_saved_info *saved_info,
		    struct bidi_it *bidi_it, bool from_type)
{
  saved_info->charpos = bidi_it->charpos;
  if (from_type)
    saved_info->type = bidi_it->type;
  else
    saved_info->type = bidi_it->type_after_wn;
  bidi_check_type (saved_info->type);
  saved_info->orig_type = bidi_it->orig_type;
  bidi_check_type (saved_info->orig_type);
}

/* Copy the bidi iterator from FROM to TO.  To save cycles, this only
   copies the part of the level stack that is actually in use.  */
static void
bidi_copy_it (struct bidi_it *to, struct bidi_it *from)
{
  /* Copy everything from the start through the active part of
     the level stack.  */
  memcpy (to, from,
	  (offsetof (struct bidi_it, level_stack) + sizeof from->level_stack[0]
	   + from->stack_idx * sizeof from->level_stack[0]));
}


/***********************************************************************
			Caching the bidi iterator states
 ***********************************************************************/

/* We allocate and de-allocate the cache in chunks of this size (in
   characters).  200 was chosen as an upper limit for reasonably-long
   lines in a text file/buffer.  */
#define BIDI_CACHE_CHUNK 200
/* Maximum size we allow the cache to become, per iterator stack slot,
   in units of struct bidi_it size.  If we allow unlimited growth, we
   could run out of memory for pathologically long bracketed text or
   very long text lines that need to be reordered.  This is aggravated
   when word-wrap is in effect, since then functions display_line and
   move_it_in_display_line_to need to keep up to 4 copies of the
   cache.

   This limitation means there can be no more than that amount of
   contiguous RTL text on any single physical line in a LTR paragraph,
   and similarly with contiguous LTR + numeric text in a RTL
   paragraph.  (LTR text in a LTR paragraph and RTL text in a RTL
   paragraph are not reordered, and so don't need the cache, and
   cannot hit this limit.)  More importantly, no single line can have
   text longer than this inside paired brackets (because bracket pairs
   resolution uses the cache).  If the limit is exceeded, the fallback
   code will produce visual order that will be incorrect if there are
   RTL characters in the offending line of text.  */
/* Do we need to allow customization of this limit?  */
#define BIDI_CACHE_MAX_ELTS_PER_SLOT 50000
verify (BIDI_CACHE_CHUNK < BIDI_CACHE_MAX_ELTS_PER_SLOT);
static ptrdiff_t bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT;
static struct bidi_it *bidi_cache;
static ptrdiff_t bidi_cache_size = 0;
enum { elsz = sizeof (struct bidi_it) };
static ptrdiff_t bidi_cache_idx;	/* next unused cache slot */
static ptrdiff_t bidi_cache_last_idx;	/* slot of last cache hit */
static ptrdiff_t bidi_cache_start = 0;	/* start of cache for this
					   "stack" level */

/* 5-slot stack for saving the start of the previous level of the
   cache.  xdisp.c maintains a 5-slot stack for its iterator state,
   and we need the same size of our stack.  */
static ptrdiff_t bidi_cache_start_stack[IT_STACK_SIZE];
static int bidi_cache_sp;

/* Size of header used by bidi_shelve_cache.  */
enum
  {
    bidi_shelve_header_size
      = (sizeof (bidi_cache_idx) + sizeof (bidi_cache_start_stack)
	 + sizeof (bidi_cache_sp) + sizeof (bidi_cache_start)
	 + sizeof (bidi_cache_last_idx) + sizeof (bidi_cache_max_elts))
  };

/* Effectively remove the cached states beyond the Nth state from the
   part of the cache relevant to iteration of the current object
   (buffer or string).  */
static void
bidi_cache_reset_to (int n)
{
  bidi_cache_idx = bidi_cache_start + n;
  bidi_cache_last_idx = -1;
}

/* Reset the cache state to the empty state.  We only reset the part
   of the cache relevant to iteration of the current object.  Previous
   objects, which are pushed on the display iterator's stack, are left
   intact.  This is called when the cached information is no more
   useful for the current iteration, e.g. when we were reseated to a
   new position on the same object.  */
static void
bidi_cache_reset (void)
{
  bidi_cache_reset_to (0);
}

/* Shrink the cache to its minimal size.  Called when we init the bidi
   iterator for reordering a buffer or a string that does not come
   from display properties, because that means all the previously
   cached info is of no further use.  */
static void
bidi_cache_shrink (void)
{
  if (bidi_cache_size > BIDI_CACHE_CHUNK)
    {
      bidi_cache = xrealloc (bidi_cache, BIDI_CACHE_CHUNK * elsz);
      bidi_cache_size = BIDI_CACHE_CHUNK;
    }
  bidi_cache_reset ();
  bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT;
}

static void
bidi_cache_fetch_state (ptrdiff_t idx, struct bidi_it *bidi_it)
{
  int current_scan_dir = bidi_it->scan_dir;

  if (idx < bidi_cache_start || idx >= bidi_cache_idx)
    emacs_abort ();

  bidi_copy_it (bidi_it, &bidi_cache[idx]);
  bidi_it->scan_dir = current_scan_dir;
  bidi_cache_last_idx = idx;
}

/* Find a cached state with a given CHARPOS and resolved embedding
   level less or equal to LEVEL.  If LEVEL is -1, disregard the
   resolved levels in cached states.  DIR, if non-zero, means search
   in that direction from the last cache hit.

   Value is the index of the cached state, or -1 if not found.  */
static ptrdiff_t
bidi_cache_search (ptrdiff_t charpos, int level, int dir)
{
  ptrdiff_t i, i_start;

  if (bidi_cache_idx > bidi_cache_start)
    {
      if (bidi_cache_last_idx == -1)
	bidi_cache_last_idx = bidi_cache_idx - 1;
      if (charpos < bidi_cache[bidi_cache_last_idx].charpos)
	{
	  dir = -1;
	  i_start = bidi_cache_last_idx - 1;
	}
      else if (charpos > (bidi_cache[bidi_cache_last_idx].charpos
			  + bidi_cache[bidi_cache_last_idx].nchars - 1))
	{
	  dir = 1;
	  i_start = bidi_cache_last_idx + 1;
	}
      else if (dir)
	i_start = bidi_cache_last_idx;
      else
	{
	  dir = -1;
	  i_start = bidi_cache_idx - 1;
	}

      if (dir < 0)
	{
	  /* Linear search for now; FIXME!  */
	  for (i = i_start; i >= bidi_cache_start; i--)
	    if (bidi_cache[i].charpos <= charpos
		&& charpos < bidi_cache[i].charpos + bidi_cache[i].nchars
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
      else
	{
	  for (i = i_start; i < bidi_cache_idx; i++)
	    if (bidi_cache[i].charpos <= charpos
		&& charpos < bidi_cache[i].charpos + bidi_cache[i].nchars
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
    }

  return -1;
}

/* Find a cached state where the resolved level changes to a value
   that is lower than LEVEL, and return its cache slot index.  DIR is
   the direction to search, starting with the last used cache slot.
   If DIR is zero, we search backwards from the last occupied cache
   slot.  BEFORE means return the index of the slot that
   is ``before'' the level change in the search direction.  That is,
   given the cached levels like this:

	 1122333442211
	  AB        C

   and assuming we are at the position cached at the slot marked with
   C, searching backwards (DIR = -1) for LEVEL = 2 will return the
   index of slot B or A, depending whether BEFORE is, respectively,
   true or false.  */
static ptrdiff_t
bidi_cache_find_level_change (int level, int dir, bool before)
{
  if (bidi_cache_idx)
    {
      ptrdiff_t i = dir ? bidi_cache_last_idx : bidi_cache_idx - 1;
      int incr = before ? 1 : 0;

      if (i < 0)  /* cache overflowed? */
	i = 0;

      if (!dir)
	dir = -1;
      else if (!incr)
	i += dir;

      if (dir < 0)
	{
	  while (i >= bidi_cache_start + incr)
	    {
	      if (bidi_cache[i - incr].resolved_level >= 0
		  && bidi_cache[i - incr].resolved_level < level)
		return i;
	      i--;
	    }
	}
      else
	{
	  while (i < bidi_cache_idx - incr)
	    {
	      if (bidi_cache[i + incr].resolved_level >= 0
		  && bidi_cache[i + incr].resolved_level < level)
		return i;
	      i++;
	    }
	}
    }

  return -1;
}

static void
bidi_cache_ensure_space (ptrdiff_t idx)
{
  /* Enlarge the cache as needed.  */
  if (idx >= bidi_cache_size)
    {
      ptrdiff_t chunk_size = BIDI_CACHE_CHUNK;

      if (bidi_cache_size > bidi_cache_max_elts - chunk_size)
	chunk_size = bidi_cache_max_elts - bidi_cache_size;

      if (max (idx + 1,
	       bidi_cache_size + chunk_size) <= bidi_cache_max_elts)
	{
	  /* The bidi cache cannot be larger than the largest Lisp
	     string or buffer.  */
	  ptrdiff_t string_or_buffer_bound
	    = max (BUF_BYTES_MAX, STRING_BYTES_BOUND);

	  /* Also, it cannot be larger than what C can represent.  */
	  ptrdiff_t c_bound
	    = (min (PTRDIFF_MAX, SIZE_MAX) - bidi_shelve_header_size) / elsz;
	  ptrdiff_t max_elts = bidi_cache_max_elts;

	  max_elts = min (max_elts, min (string_or_buffer_bound, c_bound));

	  /* Force xpalloc not to over-allocate by passing it MAX_ELTS
	     as its 4th argument.  */
	  bidi_cache = xpalloc (bidi_cache, &bidi_cache_size,
				max (chunk_size, idx - bidi_cache_size + 1),
				max_elts, elsz);
	  eassert (bidi_cache_size > idx);
	}
    }
}

static int
bidi_cache_iterator_state (struct bidi_it *bidi_it, bool resolved,
			   bool update_only)
{
  ptrdiff_t idx;

  /* We should never cache on backward scans.  */
  if (bidi_it->scan_dir == -1)
    emacs_abort ();
  idx = bidi_cache_search (bidi_it->charpos, -1, 1);

  if (idx < 0 && update_only)
    return 0;

  if (idx < 0)
    {
      idx = bidi_cache_idx;
      bidi_cache_ensure_space (idx);
      /* Character positions should correspond to cache positions 1:1.
	 If we are outside the range of cached positions, the cache is
	 useless and must be reset.  */
      if (bidi_cache_start < idx && idx < bidi_cache_size
	  && (bidi_it->charpos > (bidi_cache[idx - 1].charpos
				  + bidi_cache[idx - 1].nchars)
	      || bidi_it->charpos < bidi_cache[bidi_cache_start].charpos))
	{
	  bidi_cache_reset ();
	  idx = bidi_cache_start;
	}
      if (bidi_it->nchars <= 0)
	emacs_abort ();
      /* Don't cache if no available space in the cache.  */
      if (bidi_cache_size > idx)
	{
	  bidi_copy_it (&bidi_cache[idx], bidi_it);
	  if (!resolved)
	    bidi_cache[idx].resolved_level = -1;
	}
    }
  else
    {
      /* Copy only the members which could have changed, to avoid
	 costly copying of the entire struct.  */
      bidi_cache[idx].type = bidi_it->type;
      bidi_check_type (bidi_it->type);
      bidi_cache[idx].type_after_wn = bidi_it->type_after_wn;
      bidi_check_type (bidi_it->type_after_wn);
      if (resolved)
	bidi_cache[idx].resolved_level = bidi_it->resolved_level;
      else
	bidi_cache[idx].resolved_level = -1;
      bidi_cache[idx].invalid_levels = bidi_it->invalid_levels;
      bidi_cache[idx].next_for_neutral = bidi_it->next_for_neutral;
      bidi_cache[idx].next_for_ws = bidi_it->next_for_ws;
      bidi_cache[idx].disp_pos = bidi_it->disp_pos;
      bidi_cache[idx].disp_prop = bidi_it->disp_prop;
      bidi_cache[idx].bracket_pairing_pos = bidi_it->bracket_pairing_pos;
      bidi_cache[idx].bracket_enclosed_type = bidi_it->bracket_enclosed_type;
    }

  if (bidi_cache_size > idx)
    {
      bidi_cache_last_idx = idx;
      if (idx >= bidi_cache_idx)
	bidi_cache_idx = idx + 1;
      return 1;
    }
  else
    {
      /* The cache overflowed.  */
      bidi_cache_last_idx = -1;
      return 0;
    }
}

/* Look for a cached iterator state that corresponds to CHARPOS.  If
   found, copy the cached state into BIDI_IT and return the type of
   the cached entry.  If not found, return UNKNOWN_BT.  RESOLVED_ONLY
   zero means it is OK to return cached states that were not fully
   resolved yet.  This can happen if the state was cached before it
   was resolved in bidi_resolve_neutral.  */
static bidi_type_t
bidi_cache_find (ptrdiff_t charpos, bool resolved_only, struct bidi_it *bidi_it)
{
  ptrdiff_t i = bidi_cache_search (charpos, -1, bidi_it->scan_dir);

  if (i >= bidi_cache_start
      && (!resolved_only
	  /* Callers that want only fully resolved states (and set
	     resolved_only = true) need to be sure that there's enough
	     info in the cached state to return the state as final,
	     and if not, they don't want the cached state.  */
	  || bidi_cache[i].resolved_level >= 0))
    {
      bidi_dir_t current_scan_dir = bidi_it->scan_dir;

      bidi_copy_it (bidi_it, &bidi_cache[i]);
      bidi_cache_last_idx = i;
      /* Don't let scan direction from the cached state override
	 the current scan direction.  */
      bidi_it->scan_dir = current_scan_dir;
      return bidi_it->type;
    }

  return UNKNOWN_BT;
}

static int
bidi_peek_at_next_level (struct bidi_it *bidi_it)
{
  if (bidi_cache_idx == bidi_cache_start)
    emacs_abort ();
  /* If the cache overflowed, return the level of the last cached
     character.  */
  if (bidi_cache_last_idx == -1
      || (bidi_cache_last_idx >= bidi_cache_idx - 1 && bidi_it->scan_dir > 0))
    return bidi_cache[bidi_cache_idx - 1].resolved_level;
  return bidi_cache[bidi_cache_last_idx + bidi_it->scan_dir].resolved_level;
}


/***********************************************************************
	     Pushing and popping the bidi iterator state
 ***********************************************************************/

/* Push the bidi iterator state in preparation for reordering a
   different object, e.g. display string found at certain buffer
   position.  Pushing the bidi iterator boils down to saving its
   entire state on the cache and starting a new cache "stacked" on top
   of the current cache.  */
void
bidi_push_it (struct bidi_it *bidi_it)
{
  /* Give this stack slot its cache room.  */
  bidi_cache_max_elts += BIDI_CACHE_MAX_ELTS_PER_SLOT;
  /* Save the current iterator state in its entirety after the last
     used cache slot.  */
  bidi_cache_ensure_space (bidi_cache_idx);
  bidi_cache[bidi_cache_idx++] = *bidi_it;

  /* Push the current cache start onto the stack.  */
  eassert (bidi_cache_sp < IT_STACK_SIZE);
  bidi_cache_start_stack[bidi_cache_sp++] = bidi_cache_start;

  /* Start a new level of cache, and make it empty.  */
  bidi_cache_start = bidi_cache_idx;
  bidi_cache_last_idx = -1;
}

/* Restore the iterator state saved by bidi_push_it and return the
   cache to the corresponding state.  */
void
bidi_pop_it (struct bidi_it *bidi_it)
{
  if (bidi_cache_start <= 0)
    emacs_abort ();

  /* Reset the next free cache slot index to what it was before the
     call to bidi_push_it.  */
  bidi_cache_idx = bidi_cache_start - 1;

  /* Restore the bidi iterator state saved in the cache.  */
  *bidi_it = bidi_cache[bidi_cache_idx];

  /* Pop the previous cache start from the stack.  */
  if (bidi_cache_sp <= 0)
    emacs_abort ();
  bidi_cache_start = bidi_cache_start_stack[--bidi_cache_sp];

  /* Invalidate the last-used cache slot data.  */
  bidi_cache_last_idx = -1;

  bidi_cache_max_elts -= BIDI_CACHE_MAX_ELTS_PER_SLOT;
  eassert (bidi_cache_max_elts > 0);
}

static ptrdiff_t bidi_cache_total_alloc;

/* Stash away a copy of the cache and its control variables.  */
void *
bidi_shelve_cache (void)
{
  unsigned char *databuf;
  ptrdiff_t alloc;

  /* Empty cache.  */
  if (bidi_cache_idx == 0)
    return NULL;

  alloc = (bidi_shelve_header_size
	   + bidi_cache_idx * sizeof (struct bidi_it));
  databuf = xmalloc (alloc);
  bidi_cache_total_alloc += alloc;

  memcpy (databuf, &bidi_cache_idx, sizeof (bidi_cache_idx));
  memcpy (databuf + sizeof (bidi_cache_idx),
	  bidi_cache, bidi_cache_idx * sizeof (struct bidi_it));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it),
	  bidi_cache_start_stack, sizeof (bidi_cache_start_stack));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack),
	  &bidi_cache_sp, sizeof (bidi_cache_sp));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp),
	  &bidi_cache_start, sizeof (bidi_cache_start));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
	  + sizeof (bidi_cache_start),
	  &bidi_cache_last_idx, sizeof (bidi_cache_last_idx));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
	  + sizeof (bidi_cache_start) + sizeof (bidi_cache_last_idx),
	  &bidi_cache_max_elts, sizeof (bidi_cache_max_elts));

  return databuf;
}

/* Restore the cache state from a copy stashed away by
   bidi_shelve_cache, and free the buffer used to stash that copy.
   JUST_FREE means free the buffer, but don't restore the
   cache; used when the corresponding iterator is discarded instead of
   being restored.  */
void
bidi_unshelve_cache (void *databuf, bool just_free)
{
  unsigned char *p = databuf;

  if (!p)
    {
      if (!just_free)
	{
	  /* A NULL pointer means an empty cache.  */
	  bidi_cache_start = 0;
	  bidi_cache_sp = 0;
	  bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT;
	  bidi_cache_reset ();
	}
    }
  else
    {
      if (just_free)
	{
	  ptrdiff_t idx;

	  memcpy (&idx, p, sizeof (bidi_cache_idx));
	  bidi_cache_total_alloc
	    -= bidi_shelve_header_size + idx * sizeof (struct bidi_it);
	}
      else
	{
	  memcpy (&bidi_cache_idx, p, sizeof (bidi_cache_idx));
	  bidi_cache_ensure_space (bidi_cache_idx);
	  memcpy (bidi_cache, p + sizeof (bidi_cache_idx),
		  bidi_cache_idx * sizeof (struct bidi_it));
	  memcpy (bidi_cache_start_stack,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it),
		  sizeof (bidi_cache_start_stack));
	  memcpy (&bidi_cache_sp,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack),
		  sizeof (bidi_cache_sp));
	  memcpy (&bidi_cache_start,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp),
		  sizeof (bidi_cache_start));
	  memcpy (&bidi_cache_last_idx,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
		  + sizeof (bidi_cache_start),
		  sizeof (bidi_cache_last_idx));
	  memcpy (&bidi_cache_max_elts,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
		  + sizeof (bidi_cache_start) + sizeof (bidi_cache_last_idx),
		  sizeof (bidi_cache_max_elts));
	  bidi_cache_total_alloc
	    -= (bidi_shelve_header_size
		+ bidi_cache_idx * sizeof (struct bidi_it));
	}

      xfree (p);
    }
}


/***********************************************************************
			Initialization
 ***********************************************************************/
static void
bidi_initialize (void)
{
  bidi_type_table = uniprop_table (intern ("bidi-class"));
  if (NILP (bidi_type_table))
    emacs_abort ();
  staticpro (&bidi_type_table);

  bidi_mirror_table = uniprop_table (intern ("mirroring"));
  if (NILP (bidi_mirror_table))
    emacs_abort ();
  staticpro (&bidi_mirror_table);

  bidi_brackets_table = uniprop_table (intern ("bracket-type"));
  if (NILP (bidi_brackets_table))
    emacs_abort ();
  staticpro (&bidi_brackets_table);

  paragraph_start_re = build_string ("^\\(\f\\|[ \t]*\\)$");
  staticpro (&paragraph_start_re);
  paragraph_separate_re = build_string ("^[ \t\f]*$");
  staticpro (&paragraph_separate_re);

  bidi_cache_sp = 0;
  bidi_cache_total_alloc = 0;
  bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT;

  bidi_initialized = 1;
}

/* Do whatever UAX#9 clause X8 says should be done at paragraph's
   end.  */
static void
bidi_set_paragraph_end (struct bidi_it *bidi_it)
{
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_isolates = 0;
  bidi_it->stack_idx = 0;
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
}

/* Initialize the bidi iterator from buffer/string position CHARPOS.  */
void
bidi_init_it (ptrdiff_t charpos, ptrdiff_t bytepos, bool frame_window_p,
	      struct bidi_it *bidi_it)
{
  if (! bidi_initialized)
    bidi_initialize ();
  if (charpos >= 0)
    bidi_it->charpos = charpos;
  if (bytepos >= 0)
    bidi_it->bytepos = bytepos;
  bidi_it->frame_window_p = frame_window_p;
  bidi_it->nchars = -1;	/* to be computed in bidi_resolve_explicit */
  bidi_it->first_elt = 1;
  bidi_set_paragraph_end (bidi_it);
  bidi_it->new_paragraph = 1;
  bidi_it->separator_limit = -1;
  bidi_it->type = NEUTRAL_B;
  bidi_it->type_after_wn = NEUTRAL_B;
  bidi_it->orig_type = NEUTRAL_B;
  /* FIXME: Review this!!! */
  bidi_it->prev.type = bidi_it->prev.orig_type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->next_for_neutral.charpos = -1;
  bidi_it->next_for_neutral.type
    = bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.charpos = -1;
  bidi_it->prev_for_neutral.type
    = bidi_it->prev_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->bracket_pairing_pos = -1;
  bidi_it->sos = L2R;	 /* FIXME: should it be user-selectable? */
  bidi_it->disp_pos = -1;	/* invalid/unknown */
  bidi_it->disp_prop = 0;
  /* We can only shrink the cache if we are at the bottom level of its
     "stack".  */
  if (bidi_cache_start == 0)
    bidi_cache_shrink ();
  else
    bidi_cache_reset ();
}

/* Perform initializations for reordering a new line of bidi text.  */
static void
bidi_line_init (struct bidi_it *bidi_it)
{
  bidi_it->scan_dir = 1; /* FIXME: do we need to have control on this? */
  bidi_it->stack_idx = 0;
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
  bidi_it->level_stack[0].flags = 0; /* NEUTRAL_DIR, false per X1 */
  bidi_it->invalid_levels = 0;
  bidi_it->isolate_level = 0;	 /* X1 */
  bidi_it->invalid_isolates = 0; /* X1 */
  /* Setting this to zero will force its recomputation the first time
     we need it for W5.  */
  bidi_it->next_en_pos = 0;
  bidi_it->next_en_type = UNKNOWN_BT;
  bidi_it->next_for_ws.charpos = -1;
  bidi_it->next_for_ws.type = UNKNOWN_BT;
  bidi_it->bracket_pairing_pos = -1;
  bidi_set_sos_type (bidi_it,
		     (bidi_it->paragraph_dir == R2L ? 1 : 0),
		     bidi_it->level_stack[0].level); /* X10 */

  bidi_cache_reset ();
}


/***********************************************************************
			Fetching characters
 ***********************************************************************/

/* Count bytes in string S between BEG/BEGBYTE and END.  BEG and END
   are zero-based character positions in S, BEGBYTE is byte position
   corresponding to BEG.  UNIBYTE means S is a unibyte string.  */
static ptrdiff_t
bidi_count_bytes (const unsigned char *s, ptrdiff_t beg,
		  ptrdiff_t begbyte, ptrdiff_t end, bool unibyte)
{
  ptrdiff_t pos = beg;
  const unsigned char *p = s + begbyte, *start = p;

  if (unibyte)
    p = s + end;
  else
    {
      if (!CHAR_HEAD_P (*p))
	emacs_abort ();

      while (pos < end)
	{
	  p += BYTES_BY_CHAR_HEAD (*p);
	  pos++;
	}
    }

  return p - start;
}

/* Fetch and return the character at byte position BYTEPOS.  If S is
   non-NULL, fetch the character from string S; otherwise fetch the
   character from the current buffer.  UNIBYTE means S is a
   unibyte string.  */
static int
bidi_char_at_pos (ptrdiff_t bytepos, const unsigned char *s, bool unibyte)
{
  if (s)
    {
      s += bytepos;
      if (unibyte)
	return *s;
    }
  else
    s = BYTE_POS_ADDR (bytepos);
  return STRING_CHAR (s);
}

/* Fetch and return the character at CHARPOS/BYTEPOS.  If that
   character is covered by a display string, treat the entire run of
   covered characters as a single character, either u+2029 or u+FFFC,
   and return their combined length in CH_LEN and NCHARS.  DISP_POS
   specifies the character position of the next display string, or -1
   if not yet computed.  When the next character is at or beyond that
   position, the function updates DISP_POS with the position of the
   next display string.  *DISP_PROP non-zero means that there's really
   a display string at DISP_POS, as opposed to when we searched till
   DISP_POS without finding one.  If *DISP_PROP is 2, it means the
   display spec is of the form `(space ...)', which is replaced with
   u+2029 to handle it as a paragraph separator.  STRING->s is the C
   string to iterate, or NULL if iterating over a buffer or a Lisp
   string; in the latter case, STRING->lstring is the Lisp string.  */
static int
bidi_fetch_char (ptrdiff_t charpos, ptrdiff_t bytepos, ptrdiff_t *disp_pos,
		 int *disp_prop, struct bidi_string_data *string,
		 struct window *w,
		 bool frame_window_p, ptrdiff_t *ch_len, ptrdiff_t *nchars)
{
  int ch;
  ptrdiff_t endpos
    = (string->s || STRINGP (string->lstring)) ? string->schars : ZV;
  struct text_pos pos;
  int len;

  /* If we got past the last known position of display string, compute
     the position of the next one.  That position could be at CHARPOS.  */
  if (charpos < endpos && charpos > *disp_pos)
    {
      SET_TEXT_POS (pos, charpos, bytepos);
      *disp_pos = compute_display_string_pos (&pos, string, w, frame_window_p,
					      disp_prop);
    }

  /* Fetch the character at BYTEPOS.  */
  if (charpos >= endpos)
    {
      ch = BIDI_EOB;
      *ch_len = 1;
      *nchars = 1;
      *disp_pos = endpos;
      *disp_prop = 0;
    }
  else if (charpos >= *disp_pos && *disp_prop)
    {
      ptrdiff_t disp_end_pos;

      /* We don't expect to find ourselves in the middle of a display
	 property.  Hopefully, it will never be needed.  */
      if (charpos > *disp_pos)
	emacs_abort ();
      /* Text covered by `display' properties and overlays with
	 display properties or display strings is handled as a single
	 character that represents the entire run of characters
	 covered by the display property.  */
      if (*disp_prop == 2)
	{
	  /* `(space ...)' display specs are handled as paragraph
	     separators for the purposes of the reordering; see UAX#9
	     section 3 and clause HL1 in section 4.3 there.  */
	  ch = PARAGRAPH_SEPARATOR;
	}
      else
	{
	  /* All other display specs are handled as the Unicode Object
	     Replacement Character.  */
	  ch = OBJECT_REPLACEMENT_CHARACTER;
	}
      disp_end_pos = compute_display_string_end (*disp_pos, string);
      if (disp_end_pos < 0)
	{
	  /* Somebody removed the display string from the buffer
	     behind our back.  Recover by processing this buffer
	     position as if no display property were present there to
	     begin with.  */
	  *disp_prop = 0;
	  goto normal_char;
	}
      *nchars = disp_end_pos - *disp_pos;
      if (*nchars <= 0)
	emacs_abort ();
      if (string->s)
	*ch_len = bidi_count_bytes (string->s, *disp_pos, bytepos,
				    disp_end_pos, string->unibyte);
      else if (STRINGP (string->lstring))
	*ch_len = bidi_count_bytes (SDATA (string->lstring), *disp_pos,
				    bytepos, disp_end_pos, string->unibyte);
      else
	*ch_len = CHAR_TO_BYTE (disp_end_pos) - bytepos;
    }
  else
    {
    normal_char:
      if (string->s)
	{

	  if (!string->unibyte)
	    {
	      ch = STRING_CHAR_AND_LENGTH (string->s + bytepos, len);
	      *ch_len = len;
	    }
	  else
	    {
	      ch = UNIBYTE_TO_CHAR (string->s[bytepos]);
	      *ch_len = 1;
	    }
	}
      else if (STRINGP (string->lstring))
	{
	  if (!string->unibyte)
	    {
	      ch = STRING_CHAR_AND_LENGTH (SDATA (string->lstring) + bytepos,
					   len);
	      *ch_len = len;
	    }
	  else
	    {
	      ch = UNIBYTE_TO_CHAR (SREF (string->lstring, bytepos));
	      *ch_len = 1;
	    }
	}
      else
	{
	  ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (bytepos), len);
	  *ch_len = len;
	}
      *nchars = 1;
    }

  /* If we just entered a run of characters covered by a display
     string, compute the position of the next display string.  */
  if (charpos + *nchars <= endpos && charpos + *nchars > *disp_pos
      && *disp_prop)
    {
      SET_TEXT_POS (pos, charpos + *nchars, bytepos + *ch_len);
      *disp_pos = compute_display_string_pos (&pos, string, w, frame_window_p,
					      disp_prop);
    }

  return ch;
}

/* Like bidi_fetch_char, but ignore any text between an isolate
   initiator and its matching PDI or, if it has no matching PDI, the
   end of the paragraph.  If isolates were skipped, CH_LEN and NCHARS
   are set to the number of bytes and characters between BYTEPOS/CHARPOS
   and the character that was fetched after skipping the isolates.  */
static int
bidi_fetch_char_skip_isolates (ptrdiff_t charpos, ptrdiff_t bytepos,
			       ptrdiff_t *disp_pos, int *disp_prop,
			       struct bidi_string_data *string,
			       struct window *w, bool frame_window_p,
			       ptrdiff_t *ch_len, ptrdiff_t *nchars)
{
  ptrdiff_t orig_charpos = charpos, orig_bytepos = bytepos;
  int ch = bidi_fetch_char (charpos, bytepos, disp_pos, disp_prop, string, w,
			    frame_window_p, ch_len, nchars);
  bidi_type_t ch_type = bidi_get_type (ch, NEUTRAL_DIR);
  ptrdiff_t level = 0;

  if (ch_type == LRI || ch_type == RLI || ch_type == FSI)
    {
      level++;
      while (level > 0 && ch_type != NEUTRAL_B)
	{
	  charpos += *nchars;
	  bytepos += *ch_len;
	  ch = bidi_fetch_char (charpos, bytepos, disp_pos, disp_prop, string,
				w, frame_window_p, ch_len, nchars);
	  ch_type = bidi_get_type (ch, NEUTRAL_DIR);
	  /* A Note to P2 says to ignore max_depth limit.  */
	  if (ch_type == LRI || ch_type == RLI || ch_type == FSI)
	    level++;
	  else if (ch_type == PDI)
	    level--;
	}
    }

  /* Communicate to the caller how much did we skip, so it could get
     past the last character position we examined.  */
  *nchars += charpos - orig_charpos;
  *ch_len += bytepos - orig_bytepos;
  return ch;
}



/***********************************************************************
			Determining paragraph direction
 ***********************************************************************/

/* Check if buffer position CHARPOS/BYTEPOS is the end of a paragraph.
   Value is the non-negative length of the paragraph separator
   following the buffer position, -1 if position is at the beginning
   of a new paragraph, or -2 if position is neither at beginning nor
   at end of a paragraph.  */
static ptrdiff_t
bidi_at_paragraph_end (ptrdiff_t charpos, ptrdiff_t bytepos)
{
  Lisp_Object sep_re;
  Lisp_Object start_re;
  ptrdiff_t val;

  if (STRINGP (BVAR (current_buffer, bidi_paragraph_separate_re)))
    sep_re = BVAR (current_buffer, bidi_paragraph_separate_re);
  else
    sep_re = paragraph_separate_re;
  if (STRINGP (BVAR (current_buffer, bidi_paragraph_start_re)))
    start_re = BVAR (current_buffer, bidi_paragraph_start_re);
  else
    start_re = paragraph_start_re;

  val = fast_looking_at (sep_re, charpos, bytepos, ZV, ZV_BYTE, Qnil);
  if (val < 0)
    {
      if (fast_looking_at (start_re, charpos, bytepos, ZV, ZV_BYTE, Qnil) >= 0)
	val = -1;
      else
	val = -2;
    }

  return val;
}

/* If the user has requested the long scans caching, make sure that
   BIDI cache is enabled.  Otherwise, make sure it's disabled.  */

static struct region_cache *
bidi_paragraph_cache_on_off (void)
{
  struct buffer *cache_buffer = current_buffer;
  bool indirect_p = false;

  /* For indirect buffers, make sure to use the cache of their base
     buffer.  */
  if (cache_buffer->base_buffer)
    {
      cache_buffer = cache_buffer->base_buffer;
      indirect_p = true;
    }

  /* Don't turn on or off the cache in the base buffer, if the value
     of cache-long-scans of the base buffer is inconsistent with that.
     This is because doing so will just make the cache pure overhead,
     since if we turn it on via indirect buffer, it will be
     immediately turned off by its base buffer.  */
  if (NILP (BVAR (current_buffer, cache_long_scans)))
    {
      if (!indirect_p
	  || NILP (BVAR (cache_buffer, cache_long_scans)))
	{
	  if (cache_buffer->bidi_paragraph_cache)
	    {
	      free_region_cache (cache_buffer->bidi_paragraph_cache);
	      cache_buffer->bidi_paragraph_cache = 0;
	    }
	}
      return NULL;
    }
  else
    {
      if (!indirect_p
	  || !NILP (BVAR (cache_buffer, cache_long_scans)))
	{
	  if (!cache_buffer->bidi_paragraph_cache)
	    cache_buffer->bidi_paragraph_cache = new_region_cache ();
	}
      return cache_buffer->bidi_paragraph_cache;
    }
}

/* On my 2005-vintage machine, searching back for paragraph start
   takes ~1 ms per line.  And bidi_paragraph_init is called 4 times
   when user types C-p.  The number below limits each call to
   bidi_paragraph_init to about 10 ms.  */
#define MAX_PARAGRAPH_SEARCH 7500

/* Find the beginning of this paragraph by looking back in the buffer.
   Value is the byte position of the paragraph's beginning, or
   BEGV_BYTE if paragraph_start_re is still not found after looking
   back MAX_PARAGRAPH_SEARCH lines in the buffer.  */
static ptrdiff_t
bidi_find_paragraph_start (ptrdiff_t pos, ptrdiff_t pos_byte)
{
  Lisp_Object re =
    STRINGP (BVAR (current_buffer, bidi_paragraph_start_re))
    ? BVAR (current_buffer, bidi_paragraph_start_re)
    : paragraph_start_re;
  ptrdiff_t limit = ZV, limit_byte = ZV_BYTE;
  struct region_cache *bpc = bidi_paragraph_cache_on_off ();
  ptrdiff_t n = 0, oldpos = pos, next;
  struct buffer *cache_buffer = current_buffer;

  if (cache_buffer->base_buffer)
    cache_buffer = cache_buffer->base_buffer;

  while (pos_byte > BEGV_BYTE
	 && n++ < MAX_PARAGRAPH_SEARCH
	 && fast_looking_at (re, pos, pos_byte, limit, limit_byte, Qnil) < 0)
    {
      /* FIXME: What if the paragraph beginning is covered by a
	 display string?  And what if a display string covering some
	 of the text over which we scan back includes
	 paragraph_start_re?  */
      DEC_BOTH (pos, pos_byte);
      if (bpc && region_cache_backward (cache_buffer, bpc, pos, &next))
	{
	  pos = next, pos_byte = CHAR_TO_BYTE (pos);
	  break;
	}
      else
	pos = find_newline_no_quit (pos, pos_byte, -1, &pos_byte);
    }
  if (n >= MAX_PARAGRAPH_SEARCH)
    pos = BEGV, pos_byte = BEGV_BYTE;
  if (bpc)
    know_region_cache (cache_buffer, bpc, pos, oldpos);
  /* Positions returned by the region cache are not limited to
     BEGV..ZV range, so we limit them here.  */
  pos_byte = clip_to_bounds (BEGV_BYTE, pos_byte, ZV_BYTE);
  return pos_byte;
}

/* On a 3.4 GHz machine, searching forward for a strong directional
   character in a long paragraph full of weaks or neutrals takes about
   1 ms for each 20K characters.  The number below limits each call to
   bidi_paragraph_init to less than 10 ms even on slow machines.  */
#define MAX_STRONG_CHAR_SEARCH 100000

/* Starting from POS, find the first strong (L, R, or AL) character,
   while skipping over any characters between an isolate initiator and
   its matching PDI.  STOP_AT_PDI non-zero means stop at the PDI that
   matches the isolate initiator at POS.  Return the bidi type of the
   character where the search stopped.  Give up if after examining
   MAX_STRONG_CHAR_SEARCH buffer or string positions no strong
   character was found.  */
static bidi_type_t
find_first_strong_char (ptrdiff_t pos, ptrdiff_t bytepos, ptrdiff_t end,
			ptrdiff_t *disp_pos, int *disp_prop,
			struct bidi_string_data *string, struct window *w,
			bool string_p, bool frame_window_p,
			ptrdiff_t *ch_len, ptrdiff_t *nchars, bool stop_at_pdi)
{
  ptrdiff_t pos1;
  bidi_type_t type;
  int ch;

  if (stop_at_pdi)
    {
      /* If STOP_AT_PDI is non-zero, we must have been called with FSI
	 at POS.  Get past it.  */
#ifdef ENABLE_CHECKING
      ch = bidi_fetch_char (pos, bytepos, disp_pos, disp_prop, string, w,
			    frame_window_p, ch_len, nchars);
      type = bidi_get_type (ch, NEUTRAL_DIR);
      eassert (type == FSI /* || type == LRI || type == RLI */);
#endif
      pos += *nchars;
      bytepos += *ch_len;
    }
  ch = bidi_fetch_char_skip_isolates (pos, bytepos, disp_pos, disp_prop, string,
				      w, frame_window_p, ch_len, nchars);
  type = bidi_get_type (ch, NEUTRAL_DIR);

  pos1 = pos;
  for (pos += *nchars, bytepos += *ch_len;
       bidi_get_category (type) != STRONG
	 /* If requested to stop at first PDI, stop there.  */
	 && !(stop_at_pdi && type == PDI)
	 /* Stop when searched too far into an abnormally large
	    paragraph full of weak or neutral characters.  */
	 && pos - pos1 < MAX_STRONG_CHAR_SEARCH;
       type = bidi_get_type (ch, NEUTRAL_DIR))
    {
      if (pos >= end)
	{
	  /* Pretend there's a paragraph separator at end of
	     buffer/string.  */
	  type = NEUTRAL_B;
	  break;
	}
      if (!string_p
	  && type == NEUTRAL_B
	  && bidi_at_paragraph_end (pos, bytepos) >= -1)
	break;
      /* Fetch next character and advance to get past it.  */
      ch = bidi_fetch_char_skip_isolates (pos, bytepos, disp_pos, disp_prop,
					  string, w, frame_window_p,
					  ch_len, nchars);
      pos += *nchars;
      bytepos += *ch_len;
    }
  return type;
}

/* Determine the base direction, a.k.a. base embedding level, of the
   paragraph we are about to iterate through.  If DIR is either L2R or
   R2L, just use that.  Otherwise, determine the paragraph direction
   from the first strong directional character of the paragraph.

   NO_DEFAULT_P means don't default to L2R if the paragraph
   has no strong directional characters and both DIR and
   bidi_it->paragraph_dir are NEUTRAL_DIR.  In that case, search back
   in the buffer until a paragraph is found with a strong character,
   or until hitting BEGV.  In the latter case, fall back to L2R.  This
   flag is used in current-bidi-paragraph-direction.

   Note that this function gives the paragraph separator the same
   direction as the preceding paragraph, even though Emacs generally
   views the separator as not belonging to any paragraph.  */
void
bidi_paragraph_init (bidi_dir_t dir, struct bidi_it *bidi_it, bool no_default_p)
{
  ptrdiff_t bytepos = bidi_it->bytepos;
  bool string_p = bidi_it->string.s || STRINGP (bidi_it->string.lstring);
  ptrdiff_t pstartbyte;
  /* Note that begbyte is a byte position, while end is a character
     position.  Yes, this is ugly, but we are trying to avoid costly
     calls to BYTE_TO_CHAR and its ilk.  */
  ptrdiff_t begbyte = string_p ? 0 : BEGV_BYTE;
  ptrdiff_t end = string_p ? bidi_it->string.schars : ZV;

  /* Special case for an empty buffer. */
  if (bytepos == begbyte && bidi_it->charpos == end)
    dir = L2R;
  /* We should never be called at EOB or before BEGV.  */
  else if (bidi_it->charpos >= end || bytepos < begbyte)
    emacs_abort ();

  if (dir == L2R)
    {
      bidi_it->paragraph_dir = L2R;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == R2L)
    {
      bidi_it->paragraph_dir = R2L;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == NEUTRAL_DIR)	/* P2 */
    {
      ptrdiff_t ch_len, nchars;
      ptrdiff_t pos, disp_pos = -1;
      int disp_prop = 0;
      bidi_type_t type;
      const unsigned char *s;

      if (!bidi_initialized)
	bidi_initialize ();

      /* If we are inside a paragraph separator, we are just waiting
	 for the separator to be exhausted; use the previous paragraph
	 direction.  But don't do that if we have been just reseated,
	 because we need to reinitialize below in that case.  */
      if (!bidi_it->first_elt
	  && bidi_it->charpos < bidi_it->separator_limit)
	return;

      /* If we are on a newline, get past it to where the next
	 paragraph might start.  But don't do that at BEGV since then
	 we are potentially in a new paragraph that doesn't yet
	 exist.  */
      pos = bidi_it->charpos;
      s = (STRINGP (bidi_it->string.lstring)
	   ? SDATA (bidi_it->string.lstring)
	   : bidi_it->string.s);
      if (bytepos > begbyte
	  && bidi_char_at_pos (bytepos, s, bidi_it->string.unibyte) == '\n')
	{
	  bytepos++;
	  pos++;
	}

      /* We are either at the beginning of a paragraph or in the
	 middle of it.  Find where this paragraph starts.  */
      if (string_p)
	{
	  /* We don't support changes of paragraph direction inside a
	     string.  It is treated as a single paragraph.  */
	  pstartbyte = 0;
	}
      else
	pstartbyte = bidi_find_paragraph_start (pos, bytepos);
      bidi_it->separator_limit = -1;
      bidi_it->new_paragraph = 0;

      /* The following loop is run more than once only if NO_DEFAULT_P,
	 and only if we are iterating on a buffer.  */
      do {
	bytepos = pstartbyte;
	if (!string_p)
	  pos = BYTE_TO_CHAR (bytepos);
	type = find_first_strong_char (pos, bytepos, end, &disp_pos, &disp_prop,
				       &bidi_it->string, bidi_it->w,
				       string_p, bidi_it->frame_window_p,
				       &ch_len, &nchars, false);
	if (type == STRONG_R || type == STRONG_AL) /* P3 */
	  bidi_it->paragraph_dir = R2L;
	else if (type == STRONG_L)
	  bidi_it->paragraph_dir = L2R;
	if (!string_p
	    && no_default_p && bidi_it->paragraph_dir == NEUTRAL_DIR)
	  {
	    /* If this paragraph is at BEGV, default to L2R.  */
	    if (pstartbyte == BEGV_BYTE)
	      bidi_it->paragraph_dir = L2R; /* P3 and HL1 */
	    else
	      {
		ptrdiff_t prevpbyte = pstartbyte;
		ptrdiff_t p = BYTE_TO_CHAR (pstartbyte), pbyte = pstartbyte;

		/* Find the beginning of the previous paragraph, if any.  */
		while (pbyte > BEGV_BYTE && prevpbyte >= pstartbyte)
		  {
		    /* FXIME: What if p is covered by a display
		       string?  See also a FIXME inside
		       bidi_find_paragraph_start.  */
		    DEC_BOTH (p, pbyte);
		    prevpbyte = bidi_find_paragraph_start (p, pbyte);
		  }
		pstartbyte = prevpbyte;
	      }
	  }
      } while (!string_p
	       && no_default_p && bidi_it->paragraph_dir == NEUTRAL_DIR);
    }
  else
    emacs_abort ();

  /* Contrary to UAX#9 clause P3, we only default the paragraph
     direction to L2R if we have no previous usable paragraph
     direction.  This is allowed by the HL1 clause.  */
  if (bidi_it->paragraph_dir != L2R && bidi_it->paragraph_dir != R2L)
    bidi_it->paragraph_dir = L2R; /* P3 and HL1 ``higher-level protocols'' */
  if (bidi_it->paragraph_dir == R2L)
    bidi_it->level_stack[0].level = 1;
  else
    bidi_it->level_stack[0].level = 0;

  bidi_line_init (bidi_it);
}


/***********************************************************************
		 Resolving explicit and implicit levels.
  The rest of this file constitutes the core of the UBA implementation.
 ***********************************************************************/

static bool
bidi_explicit_dir_char (int ch)
{
  bidi_type_t ch_type;

  if (!bidi_initialized)
    emacs_abort ();
  if (ch < 0)
    {
      eassert (ch == BIDI_EOB);
      return false;
    }
  ch_type = (bidi_type_t) XFIXNUM (CHAR_TABLE_REF (bidi_type_table, ch));
  return (ch_type == LRE || ch_type == LRO
	  || ch_type == RLE || ch_type == RLO
	  || ch_type == PDF);
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer/string to the next character (in the logical order),
   resolve any explicit embeddings, directional overrides, and isolate
   initiators and terminators, and return the embedding level of the
   character after resolving these explicit directives.  */
static int
bidi_resolve_explicit (struct bidi_it *bidi_it)
{
  int curchar;
  bidi_type_t type, typ1, prev_type = UNKNOWN_BT;
  int current_level;
  int new_level;
  bidi_dir_t override;
  bool isolate_status;
  bool string_p = bidi_it->string.s || STRINGP (bidi_it->string.lstring);
  ptrdiff_t ch_len, nchars, disp_pos, end;
  int disp_prop;
  ptrdiff_t eob
    = ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
       ? bidi_it->string.schars : ZV);

  /* Record the info about the previous character.  */
  if (bidi_it->type_after_wn != WEAK_BN /* W1/Retaining */
      && bidi_it->type != WEAK_BN)
    {
      /* This special case is needed in support of Unicode 8.0
	 correction to N0, as implemented in bidi_resolve_weak/W1
	 below.  */
      if (bidi_it->type_after_wn == NEUTRAL_ON
	  && bidi_get_category (bidi_it->type) == STRONG
	  && bidi_paired_bracket_type (bidi_it->ch) == BIDI_BRACKET_CLOSE)
	bidi_remember_char (&bidi_it->prev, bidi_it, 1);
      else
	bidi_remember_char (&bidi_it->prev, bidi_it, 0);
    }
  if (bidi_it->type_after_wn == STRONG_R
      || bidi_it->type_after_wn == STRONG_L
      || bidi_it->type_after_wn == STRONG_AL)
    bidi_remember_char (&bidi_it->last_strong, bidi_it, 0);
  if (bidi_it->type == STRONG_R || bidi_it->type == STRONG_L
      || bidi_it->type == WEAK_EN || bidi_it->type == WEAK_AN)
    bidi_remember_char (&bidi_it->prev_for_neutral, bidi_it, 1);

  /* If we overstepped the characters used for resolving neutrals
     and whitespace, invalidate their info in the iterator.  */
  if (bidi_it->charpos >= bidi_it->next_for_neutral.charpos)
    {
      bidi_it->next_for_neutral.type = UNKNOWN_BT;
      /* If needed, reset the "magical" value of pairing bracket
	 position, so that bidi_resolve_brackets will resume
	 resolution of brackets according to BPA.  */
      if (bidi_it->bracket_pairing_pos == eob)
	bidi_it->bracket_pairing_pos = -1;
    }
  if (bidi_it->next_en_pos >= 0
      && bidi_it->charpos >= bidi_it->next_en_pos)
    {
      bidi_it->next_en_pos = 0;
      bidi_it->next_en_type = UNKNOWN_BT;
    }

  /* Reset the bracket resolution info, unless we previously decided
     (in bidi_find_bracket_pairs) that brackets in this level run
     should be resolved as neutrals.  */
  if (bidi_it->bracket_pairing_pos != eob)
    {
      bidi_it->bracket_pairing_pos = -1;
      bidi_it->bracket_enclosed_type = UNKNOWN_BT;
    }

  /* If reseat()'ed, don't advance, so as to start iteration from the
     position where we were reseated.  bidi_it->bytepos can be less
     than BEGV_BYTE after reseat to BEGV.  */
  if (bidi_it->bytepos < (string_p ? 0 : BEGV_BYTE)
      || bidi_it->first_elt)
    {
      bidi_it->first_elt = 0;
      if (string_p)
	{
	  const unsigned char *p
	    = (STRINGP (bidi_it->string.lstring)
	       ? SDATA (bidi_it->string.lstring)
	       : bidi_it->string.s);

	  if (bidi_it->charpos < 0)
	    bidi_it->charpos = bidi_it->bytepos = 0;
	  eassert (bidi_it->bytepos == bidi_count_bytes (p, 0, 0,
							 bidi_it->charpos,
							 bidi_it->string.unibyte));
	}
      else
	{
	  if (bidi_it->charpos < BEGV)
	    {
	      bidi_it->charpos = BEGV;
	      bidi_it->bytepos = BEGV_BYTE;
	    }
	  eassert (bidi_it->bytepos == CHAR_TO_BYTE (bidi_it->charpos));
	}
      /* Determine the original bidi type of the previous character,
	 which is needed for handling isolate initiators and PDF.  The
	 type of the previous character will be non-trivial only if
	 our caller moved through some previous text in
	 get_visually_first_element, in which case bidi_it->prev holds
	 the information we want.  */
      if (bidi_it->first_elt && bidi_it->prev.type != UNKNOWN_BT)
	{
	  eassert (bidi_it->prev.charpos == bidi_it->charpos - 1);
	  prev_type = bidi_it->prev.orig_type;
	}
    }
  /* Don't move at end of buffer/string.  */
  else if (bidi_it->charpos < (string_p ? bidi_it->string.schars : ZV))
    {
      /* Advance to the next character, skipping characters covered by
	 display strings (nchars > 1).  */
      if (bidi_it->nchars <= 0)
	emacs_abort ();
      bidi_it->charpos += bidi_it->nchars;
      if (bidi_it->ch_len == 0)
	emacs_abort ();
      bidi_it->bytepos += bidi_it->ch_len;
      prev_type = bidi_it->orig_type;
    }
  else	/* EOB or end of string */
    prev_type = NEUTRAL_B;

  current_level = bidi_it->level_stack[bidi_it->stack_idx].level; /* X1 */
  isolate_status = ISOLATE_STATUS (bidi_it, bidi_it->stack_idx);
  override = OVERRIDE (bidi_it, bidi_it->stack_idx);
  new_level = current_level;

  if (bidi_it->charpos >= (string_p ? bidi_it->string.schars : ZV))
    {
      curchar = BIDI_EOB;
      bidi_it->ch_len = 1;
      bidi_it->nchars = 1;
      bidi_it->disp_pos = (string_p ? bidi_it->string.schars : ZV);
      bidi_it->disp_prop = 0;
    }
  else
    {
      /* LRI, RLI, and FSI increment, and PDF decrements, the
	 embedding level of the _following_ characters, so we must
	 first look at the type of the previous character to support
	 that.  */
      switch (prev_type)
	{
	case RLI:	/* X5a */
	  if (current_level < BIDI_MAXDEPTH
	      && bidi_it->invalid_levels == 0
	      && bidi_it->invalid_isolates == 0)
	    {
	      new_level = ((current_level + 1) & ~1) + 1;
	      bidi_it->isolate_level++;
	      bidi_push_embedding_level (bidi_it, new_level,
					 NEUTRAL_DIR, true);
	    }
	  else
	    bidi_it->invalid_isolates++;
	  break;
	case LRI:	/* X5b */
	  if (current_level < BIDI_MAXDEPTH - 1
	      && bidi_it->invalid_levels == 0
	      && bidi_it->invalid_isolates == 0)
	    {
	      new_level = ((current_level + 2) & ~1);
	      bidi_it->isolate_level++;
	      bidi_push_embedding_level (bidi_it, new_level,
					 NEUTRAL_DIR, true);
	    }
	  else
	    bidi_it->invalid_isolates++;
	  break;
	case PDF:	/* X7 */
	  if (!bidi_it->invalid_isolates)
	    {
	      if (bidi_it->invalid_levels)
		bidi_it->invalid_levels--;
	      else if (!isolate_status && bidi_it->stack_idx >= 1)
		new_level = bidi_pop_embedding_level (bidi_it);
	    }
	  break;
	default:
	  eassert (prev_type != FSI);
	  /* Nothing.  */
	  break;
	}
      /* Fetch the character at BYTEPOS.  If it is covered by a
	 display string, treat the entire run of covered characters as
	 a single character u+FFFC.  */
      curchar = bidi_fetch_char (bidi_it->charpos, bidi_it->bytepos,
				 &bidi_it->disp_pos, &bidi_it->disp_prop,
				 &bidi_it->string, bidi_it->w,
				 bidi_it->frame_window_p,
				 &bidi_it->ch_len, &bidi_it->nchars);
    }
  bidi_it->ch = curchar;
  bidi_it->resolved_level = new_level;

  /* Don't apply directional override here, as all the types we handle
     below will not be affected by the override anyway, and we need
     the original type unaltered.  The override will be applied in
     bidi_resolve_weak.  */
  type = bidi_get_type (curchar, NEUTRAL_DIR);
  bidi_it->orig_type = type;
  bidi_check_type (bidi_it->orig_type);

  bidi_it->type_after_wn = UNKNOWN_BT;

  switch (type)
    {
    case RLE:	/* X2 */
    case RLO:	/* X4 */
      bidi_it->type_after_wn = type;
      bidi_check_type (bidi_it->type_after_wn);
      type = WEAK_BN; /* X9/Retaining */
      if (new_level < BIDI_MAXDEPTH
	  && bidi_it->invalid_levels == 0
	  && bidi_it->invalid_isolates == 0)
	{
	  /* Compute the least odd embedding level greater than
	     the current level.  */
	  new_level = ((new_level + 1) & ~1) + 1;
	  if (bidi_it->type_after_wn == RLE)
	    override = NEUTRAL_DIR;
	  else
	    override = R2L;
	  bidi_push_embedding_level (bidi_it, new_level, override, false);
	  bidi_it->resolved_level = new_level;
	}
      else
	{
	  if (bidi_it->invalid_isolates == 0)
	    bidi_it->invalid_levels++;
	}
      break;
    case LRE:	/* X3 */
    case LRO:	/* X5 */
      bidi_it->type_after_wn = type;
      bidi_check_type (bidi_it->type_after_wn);
      type = WEAK_BN; /* X9/Retaining */
      if (new_level < BIDI_MAXDEPTH - 1
	  && bidi_it->invalid_levels == 0
	  && bidi_it->invalid_isolates == 0)
	{
	  /* Compute the least even embedding level greater than
	     the current level.  */
	  new_level = ((new_level + 2) & ~1);
	  if (bidi_it->type_after_wn == LRE)
	    override = NEUTRAL_DIR;
	  else
	    override = L2R;
	  bidi_push_embedding_level (bidi_it, new_level, override, false);
	  bidi_it->resolved_level = new_level;
	}
      else
	{
	  if (bidi_it->invalid_isolates == 0)
	    bidi_it->invalid_levels++;
	}
      break;
    case FSI:	/* X5c */
      end = string_p ? bidi_it->string.schars : ZV;
      disp_pos = bidi_it->disp_pos;
      disp_prop = bidi_it->disp_prop;
      nchars = bidi_it->nchars;
      ch_len = bidi_it->ch_len;
      typ1 = find_first_strong_char (bidi_it->charpos,
				     bidi_it->bytepos, end,
				     &disp_pos, &disp_prop,
				     &bidi_it->string, bidi_it->w,
				     string_p, bidi_it->frame_window_p,
				     &ch_len, &nchars, true);
      if (typ1 != STRONG_R && typ1 != STRONG_AL)
	{
	  type = LRI;
	  /* Override orig_type, which will be needed when we come to
	     examine the next character, which is the first character
	     inside the isolate.  */
	  bidi_it->orig_type = type;
	  goto fsi_as_lri;
	}
      else
	{
	  type = RLI;
	  bidi_it->orig_type = type;
	}
      FALLTHROUGH;
    case RLI:	/* X5a */
      if (override == NEUTRAL_DIR)
	bidi_it->type_after_wn = type;
      else	/* Unicode 8.0 correction.  */
	bidi_it->type_after_wn = (override == L2R ? STRONG_L : STRONG_R);
      bidi_check_type (bidi_it->type_after_wn);
      break;
    case LRI:	/* X5b */
    fsi_as_lri:
      if (override == NEUTRAL_DIR)
	bidi_it->type_after_wn = type;
      else	/* Unicode 8.0 correction.  */
	bidi_it->type_after_wn = (override == L2R ? STRONG_L : STRONG_R);
      bidi_check_type (bidi_it->type_after_wn);
      break;
    case PDI:	/* X6a */
      if (bidi_it->invalid_isolates)
	bidi_it->invalid_isolates--;
      else if (bidi_it->isolate_level > 0)
	{
	  bidi_it->invalid_levels = 0;
	  while (!ISOLATE_STATUS (bidi_it, bidi_it->stack_idx))
	    bidi_pop_embedding_level (bidi_it);
	  eassert (bidi_it->stack_idx > 0);
	  new_level = bidi_pop_embedding_level (bidi_it);
	  bidi_it->isolate_level--;
	}
      bidi_it->resolved_level = new_level;
      /* Unicode 8.0 correction.  */
      {
	bidi_dir_t stack_override = OVERRIDE (bidi_it, bidi_it->stack_idx);
	if (stack_override == L2R)
	  bidi_it->type_after_wn = STRONG_L;
	else if (stack_override == R2L)
	  bidi_it->type_after_wn = STRONG_R;
	else
	  bidi_it->type_after_wn = type;
      }
      break;
    case PDF:	/* X7 */
      bidi_it->type_after_wn = type;
      bidi_check_type (bidi_it->type_after_wn);
      type = WEAK_BN; /* X9/Retaining */
      break;
    default:
      /* Nothing.  */
      break;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  if (bidi_it->type == NEUTRAL_B)	/* X8 */
    {
      bidi_set_paragraph_end (bidi_it);
      /* This is needed by bidi_resolve_weak below, and in L1.  */
      bidi_it->type_after_wn = bidi_it->type;
    }

  eassert (bidi_it->resolved_level >= 0);
  return bidi_it->resolved_level;
}

/* Advance in the buffer/string, resolve weak types and return the
   type of the next character after weak type resolution.  */
static bidi_type_t
bidi_resolve_weak (struct bidi_it *bidi_it)
{
  bidi_type_t type;
  bidi_dir_t override;
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  int new_level  = bidi_resolve_explicit (bidi_it);
  int next_char;
  bidi_type_t type_of_next;
  struct bidi_it saved_it;
  ptrdiff_t eob
    = ((STRINGP (bidi_it->string.lstring) || bidi_it->string.s)
       ? bidi_it->string.schars : ZV);

  type = bidi_it->type;
  override = OVERRIDE (bidi_it, bidi_it->stack_idx);

  eassert (!(type == UNKNOWN_BT
	     || type == LRE
	     || type == LRO
	     || type == RLE
	     || type == RLO
	     || type == PDF));

  eassert (prev_level >= 0);
  if (bidi_it->type == NEUTRAL_B)
    {
      /* We've got a new isolating sequence, compute the directional
         type of sos and initialize per-run variables (UAX#9, clause
         X10).  */
      bidi_set_sos_type (bidi_it, prev_level, new_level);
    }
  if (type == NEUTRAL_S || type == NEUTRAL_WS
      || type == WEAK_BN || type == STRONG_AL)
    bidi_it->type_after_wn = type;	/* needed in L1 */
  bidi_check_type (bidi_it->type_after_wn);

  /* Level and directional override status are already recorded in
     bidi_it, and do not need any change; see X6.  */
  if (override == R2L)		/* X6 */
    type = STRONG_R;
  else if (override == L2R)
    type = STRONG_L;
  else
    {
      if (type == WEAK_NSM)	/* W1 */
	{
	  /* Note that we don't need to consider the case where the
	     prev character has its type overridden by an RLO or LRO,
	     because then either the type of this NSM would have been
	     also overridden, or the previous character is outside the
	     current level run, and thus not relevant to this NSM.
	     This is why NSM gets the type_after_wn of the previous
	     character.  */
	  /* bidi_set_sos_type sets type_after_wn to UNKNOWN_BT.  */
	  if (bidi_it->prev.type != UNKNOWN_BT
	      /* If type_after_wn is NEUTRAL_B, this NSM is at sos.  */
	      && bidi_it->prev.type != NEUTRAL_B)
	    {
	      if (bidi_isolate_fmt_char (bidi_it->prev.type))
		{
		  /* From W1: "Note that in an isolating run sequence,
		     an isolate initiator followed by an NSM or any
		     type other than PDI must be an overflow isolate
		     initiator."  */
		  eassert (bidi_it->invalid_isolates > 0);
		  type = NEUTRAL_ON;
		}
	      else
		{
		  /* This includes the Unicode 8.0 correction for N0,
		     due to how we set prev.type in bidi_resolve_explicit,
		     which see.  */
		  type = bidi_it->prev.type;
		}
	    }
	  else if (bidi_it->sos == R2L)
	    type = STRONG_R;
	  else if (bidi_it->sos == L2R)
	    type = STRONG_L;
	  else /* shouldn't happen! */
	    emacs_abort ();
	}
      if (type == WEAK_EN	/* W2 */
	  && bidi_it->last_strong.type == STRONG_AL)
	type = WEAK_AN;
      else if (type == STRONG_AL) /* W3 */
	type = STRONG_R;
      else if ((type == WEAK_ES	/* W4 */
		&& bidi_it->prev.type == WEAK_EN
		&& bidi_it->prev.orig_type == WEAK_EN)
	       || (type == WEAK_CS
		   && ((bidi_it->prev.type == WEAK_EN
			&& bidi_it->prev.orig_type == WEAK_EN)
		       || bidi_it->prev.type == WEAK_AN)))
	{
	  const unsigned char *s
	    = (STRINGP (bidi_it->string.lstring)
	       ? SDATA (bidi_it->string.lstring)
	       : bidi_it->string.s);

	  next_char = (bidi_it->charpos + bidi_it->nchars >= eob
		       ? BIDI_EOB
		       : bidi_char_at_pos (bidi_it->bytepos + bidi_it->ch_len,
					   s, bidi_it->string.unibyte));
	  type_of_next = bidi_get_type (next_char, override);

	  if (type_of_next == WEAK_BN
	      || bidi_explicit_dir_char (next_char))
	    {
	      bidi_copy_it (&saved_it, bidi_it);
	      while (bidi_resolve_explicit (bidi_it) == new_level
		     && bidi_it->type == WEAK_BN)
		type_of_next = bidi_it->type;
	      bidi_copy_it (bidi_it, &saved_it);
	    }

	  /* If the next character is EN, but the last strong-type
	     character is AL, that next EN will be changed to AN when
	     we process it in W2 above.  So in that case, this ES
	     should not be changed into EN.  */
	  if (type == WEAK_ES
	      && type_of_next == WEAK_EN
	      && bidi_it->last_strong.type != STRONG_AL)
	    type = WEAK_EN;
	  else if (type == WEAK_CS)
	    {
	      if (bidi_it->prev.type == WEAK_AN
		  && (type_of_next == WEAK_AN
		      /* If the next character is EN, but the last
			 strong-type character is AL, EN will be later
			 changed to AN when we process it in W2 above.
			 So in that case, this ES should not be
			 changed into EN.  */
		      || (type_of_next == WEAK_EN
			  && bidi_it->last_strong.type == STRONG_AL)))
		type = WEAK_AN;
	      else if (bidi_it->prev.type == WEAK_EN
		       && type_of_next == WEAK_EN
		       && bidi_it->last_strong.type != STRONG_AL)
		type = WEAK_EN;
	    }
	}
      else if (type == WEAK_ET	/* W5: ET with EN before or after it */
	       || type == WEAK_BN)	/* W5/Retaining */
	{
	  if (bidi_it->prev.type == WEAK_EN) /* ET/BN w/EN before it */
	    type = WEAK_EN;
	  else if (bidi_it->next_en_pos > bidi_it->charpos
		   && bidi_it->next_en_type != WEAK_BN)
	    {
	      if (bidi_it->next_en_type == WEAK_EN) /* ET/BN with EN after it */
		type = WEAK_EN;
	    }
	  else if (type == WEAK_BN
		   /* This condition is for the following important case:

		      . we are at level zero
		      . either previous strong character was L,
			 or we've seen no strong characters since sos
			 and the base paragraph direction is L2R
		      . this BN is NOT a bidi directional control

		      For such a situation, either this BN will be
		      converted to EN per W5, and then to L by virtue
		      of W7; or it will become ON per W6, and then L
		      because of N1/N2.  So we take a shortcut here
		      and make it L right away, to avoid the
		      potentially costly loop below.  This is
		      important when the buffer has a long series of
		      control characters, like binary NULs, and no
		      R2L characters at all.  */
		   && new_level == 0
		   && !bidi_explicit_dir_char (bidi_it->ch)
		   && ((bidi_it->last_strong.type == STRONG_L)
		       || (bidi_it->last_strong.type == UNKNOWN_BT
			   && bidi_it->sos == L2R)))
	    type = STRONG_L;
	  else if (bidi_it->next_en_pos >= 0)
	    {
	      /* We overstepped the last known position for ET
		 resolution but there could be other such characters
		 in this paragraph (when we are sure there are no more
		 such positions, we set next_en_pos to a negative
		 value).  Try to find the next position for ET
		 resolution.  */
	      ptrdiff_t en_pos = bidi_it->charpos + bidi_it->nchars;
	      const unsigned char *s = (STRINGP (bidi_it->string.lstring)
					? SDATA (bidi_it->string.lstring)
					: bidi_it->string.s);

	      if (bidi_it->nchars <= 0)
		emacs_abort ();
	      next_char
		= (bidi_it->charpos + bidi_it->nchars >= eob
		   ? BIDI_EOB
		   : bidi_char_at_pos (bidi_it->bytepos + bidi_it->ch_len, s,
				       bidi_it->string.unibyte));
	      type_of_next = bidi_get_type (next_char, override);

	      if (type_of_next == WEAK_ET
		  || type_of_next == WEAK_BN
		  || bidi_explicit_dir_char (next_char))
		{
		  bidi_copy_it (&saved_it, bidi_it);
		  while (bidi_resolve_explicit (bidi_it) == new_level
			 && (bidi_it->type == WEAK_BN
			     || bidi_it->type == WEAK_ET))
		    type_of_next = bidi_it->type;
		  if (type == WEAK_BN
		      && bidi_it->charpos == saved_it.charpos + saved_it.nchars)
		    {
		      /* If we entered the above loop with a BN that
			 changes the level, the type of next
			 character, which is in a different level, is
			 not relevant to resolving this series of ET
			 and BN.  */
		      en_pos = saved_it.charpos;
		      type_of_next = type;
		    }
		  else
		    en_pos = bidi_it->charpos;
		  bidi_copy_it (bidi_it, &saved_it);
		}
	      /* Remember this position, to speed up processing of the
		 next ETs.  */
	      bidi_it->next_en_pos = en_pos;
	      if (type_of_next == WEAK_EN)
		{
		  /* If the last strong character is AL, the EN we've
		     found will become AN when we get to it (W2). */
		  if (bidi_it->last_strong.type == STRONG_AL)
		    type_of_next = WEAK_AN;
		  else if (type == WEAK_BN)
		    type = NEUTRAL_ON; /* W6/Retaining */
		  else
		    type = WEAK_EN;
		}
	      else if (type_of_next == NEUTRAL_B)
		/* Record the fact that there are no more ENs from
		   here to the end of paragraph, to avoid entering the
		   loop above ever again in this paragraph.  */
		bidi_it->next_en_pos = -1;
	      /* Record the type of the character where we ended our search.  */
	      bidi_it->next_en_type = type_of_next;
	    }
	}
    }

  if (type == WEAK_ES || type == WEAK_ET || type == WEAK_CS /* W6 */
      || (type == WEAK_BN
	  && (bidi_it->prev.type == WEAK_CS	    /* W6/Retaining */
	      || bidi_it->prev.type == WEAK_ES
	      || bidi_it->prev.type == WEAK_ET)))
    type = NEUTRAL_ON;

  /* Store the type we've got so far, before we clobber it with strong
     types in W7 and while resolving neutral types.  But leave alone
     the original types that were recorded above, because we will need
     them for the L1 clause.  */
  if (bidi_it->type_after_wn == UNKNOWN_BT)
    bidi_it->type_after_wn = type;
  bidi_check_type (bidi_it->type_after_wn);

  if (type == WEAK_EN)	/* W7 */
    {
      if ((bidi_it->last_strong.type == STRONG_L)
	  || (bidi_it->last_strong.type == UNKNOWN_BT && bidi_it->sos == L2R))
	type = STRONG_L;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);
  return type;
}

/* Resolve the type of a neutral character according to the type of
   surrounding strong text and the current embedding level.  */
static bidi_type_t
bidi_resolve_neutral_1 (bidi_type_t prev_type, bidi_type_t next_type, int lev)
{
  /* N1: "European and Arabic numbers act as if they were R in terms
     of their influence on NIs."  */
  if (next_type == WEAK_EN || next_type == WEAK_AN)
    next_type = STRONG_R;
  if (prev_type == WEAK_EN || prev_type == WEAK_AN)
    prev_type = STRONG_R;

  if (next_type == prev_type)	/* N1 */
    return next_type;
  else if ((lev & 1) == 0)	/* N2 */
    return STRONG_L;
  else
    return STRONG_R;
}

#define FLAG_EMBEDDING_INSIDE  1
#define FLAG_OPPOSITE_INSIDE   2

/* A data type used in the stack maintained by
   bidi_find_bracket_pairs below.  */
typedef struct bpa_stack_entry {
  int close_bracket_char;
  int open_bracket_idx;
#ifdef ENABLE_CHECKING
  ptrdiff_t open_bracket_pos;
#endif
  unsigned flags : 2;
} bpa_stack_entry;

/* Allow for the two struct bidi_it objects too, since they can be big.
   With MAX_ALLOCA of 16 KiB, this should allow at least 900 slots in the
   BPA stack, which should be more than enough for actual bidi text.  */
enum { MAX_BPA_STACK = max (1, ((MAX_ALLOCA - 2 * sizeof (struct bidi_it))
				/ sizeof (bpa_stack_entry))) };

/* UAX#9 says to match opening brackets with the matching closing
   brackets or their canonical equivalents.  As of Unicode 8.0, there
   are only 2 bracket characters that have canonical equivalence
   decompositions: u+2329 and u+232A.  So instead of accessing the
   table in uni-decomposition.el, we just handle these 2 characters
   with this simple macro.  Note that ASCII characters don't have
   canonical equivalents by definition.  */

/* To find all the characters that need to be processed by
   CANONICAL_EQU, first find all the characters which have
   decompositions in UnicodeData.txt, with this Awk script:

    awk -F ";" " {if ($6 != \"\") print $1, $6}" UnicodeData.txt

   Then produce a list of all the bracket characters in BidiBrackets.txt:

    awk -F "[ ;]" " {if ($1 != \"#\" && $1 != \"\") print $1}" BidiBrackets.txt

   And finally, cross-reference these two:

    grep -Fw -f brackets.txt decompositions.txt

   where "decompositions.txt" was produced by the 1st script, and
   "brackets.txt" by the 2nd script.  In the output of grep, look
   only for decompositions that don't begin with some compatibility
   formatting tag, such as "<compat>".  Only decompositions that
   consist solely of character codepoints are relevant to bidi
   brackets processing.  */

#define CANONICAL_EQU(c)					\
  ( ASCII_CHAR_P (c) ? c					\
    : (c) == LEFT_POINTING_ANGLE_BRACKET ? LEFT_ANGLE_BRACKET	\
    : (c) == RIGHT_POINTING_ANGLE_BRACKET ? RIGHT_ANGLE_BRACKET	\
    : c )

#ifdef ENABLE_CHECKING
# define STORE_BRACKET_CHARPOS \
   bpa_stack[bpa_sp].open_bracket_pos = bidi_it->charpos
#else
# define STORE_BRACKET_CHARPOS	/* nothing */
#endif

#define PUSH_BPA_STACK							\
  do {									\
    int ch;								\
    if (bpa_sp < MAX_BPA_STACK - 1 && bidi_cache_last_idx <= INT_MAX)	\
      {									\
	bpa_sp++;							\
	ch = CANONICAL_EQU (bidi_it->ch);				\
	bpa_stack[bpa_sp].close_bracket_char = bidi_mirror_char (ch);	\
	bpa_stack[bpa_sp].open_bracket_idx = bidi_cache_last_idx;	\
	bpa_stack[bpa_sp].flags = 0;					\
	STORE_BRACKET_CHARPOS;						\
      }									\
  } while (0)


/* This function implements BPA, the Bidi Parenthesis Algorithm,
   described in BD16 and N0 of UAX#9.  It finds all the bracket pairs
   in the current isolating sequence, and records the enclosed type
   and the position of the matching bracket in the cache.  It returns
   non-zero if called with the iterator on the opening bracket which
   has a matching closing bracket in the current isolating sequence,
   zero otherwise.  */
static bool
bidi_find_bracket_pairs (struct bidi_it *bidi_it)
{
  bidi_bracket_type_t btype;
  bidi_type_t type = bidi_it->type;
  bool retval = false;

  /* When scanning backwards, we don't expect any unresolved bidi
     bracket characters.  */
  if (bidi_it->scan_dir != 1)
    emacs_abort ();

  btype = bidi_paired_bracket_type (bidi_it->ch);
  if (btype == BIDI_BRACKET_OPEN)
    {
      bpa_stack_entry bpa_stack[MAX_BPA_STACK];
      int bpa_sp = -1;
      struct bidi_it saved_it;
      int base_level = bidi_it->level_stack[0].level;
      int embedding_level = bidi_it->level_stack[bidi_it->stack_idx].level;
      int maxlevel = embedding_level;
      bidi_type_t embedding_type = (embedding_level & 1) ? STRONG_R : STRONG_L;
      struct bidi_it tem_it;
      bool l2r_seen = false, r2l_seen = false;
      ptrdiff_t pairing_pos;
      int idx_at_entry = bidi_cache_idx;

      verify (MAX_BPA_STACK >= 100);
      bidi_copy_it (&saved_it, bidi_it);
      /* bidi_cache_iterator_state refuses to cache on backward scans,
	 and bidi_cache_fetch_state doesn't bring scan_dir from the
	 cache, so we must initialize this explicitly.  */
      tem_it.scan_dir = 1;

      while (1)
	{
	  int old_sidx, new_sidx;
	  int current_level = bidi_it->level_stack[bidi_it->stack_idx].level;

	  if (maxlevel < current_level)
	    maxlevel = current_level;
	  /* Mark every opening bracket character we've traversed by
	     putting its own position into bracket_pairing_pos.  This
	     is examined in bidi_resolve_brackets to distinguish
	     brackets that were already resolved to stay NEUTRAL_ON,
	     and those that were not yet processed by this function
	     (because they were skipped when we skip higher embedding
	     levels below).  */
	  if (btype == BIDI_BRACKET_OPEN && bidi_it->bracket_pairing_pos == -1)
	    bidi_it->bracket_pairing_pos = bidi_it->charpos;
	  if (!bidi_cache_iterator_state (bidi_it, type == NEUTRAL_B, 0))
	    {
	      /* No more space in cache -- give up and let the opening
		 bracket that started this be processed as a
		 NEUTRAL_ON.  */
	      bidi_cache_reset_to (idx_at_entry - bidi_cache_start);
	      bidi_copy_it (bidi_it, &saved_it);
	      goto give_up;
	    }
	  if (btype == BIDI_BRACKET_OPEN)
	    PUSH_BPA_STACK;
	  else if (btype == BIDI_BRACKET_CLOSE)
	    {
	      int sp = bpa_sp;
	      int curchar = CANONICAL_EQU (bidi_it->ch);

	      eassert (sp >= 0);
	      while (sp >= 0 && bpa_stack[sp].close_bracket_char != curchar)
		sp--;
	      if (sp >= 0)
		{
		  /* Update and cache the corresponding opening bracket.  */
		  bidi_cache_fetch_state (bpa_stack[sp].open_bracket_idx,
					  &tem_it);
#ifdef ENABLE_CHECKING
		  eassert (bpa_stack[sp].open_bracket_pos == tem_it.charpos);
#endif
		  /* Determine the enclosed type for this bracket
		     pair's type resolution according to N0.  */
		  if (bpa_stack[sp].flags & FLAG_EMBEDDING_INSIDE)
		    tem_it.bracket_enclosed_type = embedding_type; /* N0b */
		  else if (bpa_stack[sp].flags & FLAG_OPPOSITE_INSIDE)
		    tem_it.bracket_enclosed_type		   /* N0c */
		      = (embedding_type == STRONG_L ? STRONG_R : STRONG_L);
		  else						   /* N0d */
		    tem_it.bracket_enclosed_type = UNKNOWN_BT;

		  /* Record the position of the matching closing
		     bracket, and update the cache.  */
		  tem_it.bracket_pairing_pos = bidi_it->charpos;
		  bidi_cache_iterator_state (&tem_it, 0, 1);

		  /* Pop the BPA stack.  */
		  bpa_sp = sp - 1;
		}
	      if (bpa_sp < 0)
		{
		  retval = true;
		  break;
		}
	    }
	  else if (bidi_get_category (bidi_it->type_after_wn) != NEUTRAL)
	    {
	      unsigned flag = 0;
	      int sp;

	      /* Whenever we see a strong type, update the flags of
		 all the slots on the stack.  */
	      switch (bidi_it->type)
		{
		case STRONG_L:
		  flag = ((embedding_level & 1) == 0
			  ? FLAG_EMBEDDING_INSIDE
			  : FLAG_OPPOSITE_INSIDE);
		  l2r_seen = true;
		  break;
		case STRONG_R:
		case WEAK_EN:
		case WEAK_AN:
		  flag = ((embedding_level & 1) == 1
			  ? FLAG_EMBEDDING_INSIDE
			  : FLAG_OPPOSITE_INSIDE);
		  r2l_seen = true;
		  break;
		default:
		  break;
		}
	      if (flag)
		{
		  for (sp = bpa_sp; sp >= 0; sp--)
		    bpa_stack[sp].flags |= flag;
		}
	    }
	  old_sidx = bidi_it->stack_idx;
	  type = bidi_resolve_weak (bidi_it);
	  /* Skip level runs excluded from this isolating run sequence.  */
	  new_sidx = bidi_it->stack_idx;
	  if (bidi_it->level_stack[new_sidx].level > current_level
	      && (ISOLATE_STATUS (bidi_it, new_sidx)
		  || (new_sidx > old_sidx + 1
		      && ISOLATE_STATUS (bidi_it, new_sidx - 1))))
	    {
	      while (bidi_it->level_stack[bidi_it->stack_idx].level
		     > current_level)
		{
		  if (maxlevel < bidi_it->level_stack[bidi_it->stack_idx].level)
		    maxlevel = bidi_it->level_stack[bidi_it->stack_idx].level;
		  if (!bidi_cache_iterator_state (bidi_it,
						  type == NEUTRAL_B, 0))
		    {
		      /* No more space in cache -- give up and let the
			 opening bracket that started this be
			 processed as any other NEUTRAL_ON.  */
		      bidi_cache_reset_to (idx_at_entry - bidi_cache_start);
		      bidi_copy_it (bidi_it, &saved_it);
		      goto give_up;
		    }
		  type = bidi_resolve_weak (bidi_it);
		}
	    }
	  if (type == NEUTRAL_B
	      || (bidi_it->level_stack[bidi_it->stack_idx].level
		  != current_level))
	    {
	      /* We've marched all the way to the end of this
		 isolating run sequence, and didn't find matching
		 closing brackets for some opening brackets.  Leave
		 their type unchanged.  */
	      pairing_pos = bidi_it->charpos;
	      break;
	    }
	  if (bidi_it->type_after_wn == NEUTRAL_ON) /* Unicode 8.0 correction */
	    btype = bidi_paired_bracket_type (bidi_it->ch);
	  else
	    btype = BIDI_BRACKET_NONE;
	}

      /* Restore bidi_it from the cache, which should have the bracket
	 resolution members set as determined by the above loop.  */
      type = bidi_cache_find (saved_it.charpos, 0, bidi_it);
      eassert (type == NEUTRAL_ON);

      /* The following is an optimization for bracketed text that has
	 only one level which is equal to the paragraph's base
	 embedding level.  That is, only L2R and weak/neutral
	 characters in a L2R paragraph, or only R2L and weak/neutral
	 characters in a R2L paragraph.  Such brackets can be resolved
	 by bidi_resolve_neutral, which has a further shortcut for
	 this case.  So we pretend we did not resolve the brackets in
	 this case, set up next_for_neutral for the entire bracketed
	 text, and reset the cache to the character before the opening
	 bracket.  The upshot is to allow bidi_move_to_visually_next
	 reset the cache when it returns this opening bracket, thus
	 cutting significantly on the size of the cache, which is
	 important with long lines, especially if word-wrap is non-nil
	 (which requires the display engine to copy the cache back and
	 forth many times).  */
      if (maxlevel == base_level
	  && ((base_level == 0 && !r2l_seen)
	      || (base_level == 1 && !l2r_seen)))
	{
	  ptrdiff_t eob
	    = ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
	       ? bidi_it->string.schars : ZV);

	  if (retval)
	    pairing_pos = bidi_it->bracket_pairing_pos;

	  /* This special value (which cannot possibly happen when
	     brackets are resolved, since there's no character at ZV)
	     will be noticed by bidi_resolve_explicit, and will be
	     copied to the following iterator states, instead of being
	     reset to -1.  */
	  bidi_it->bracket_pairing_pos = eob;
	  /* This type value will be used for resolving the outermost
	     closing bracket in bidi_resolve_brackets.  */
	  bidi_it->bracket_enclosed_type = embedding_type;
	  /* bidi_cache_last_idx is set to the index of the current
	     state, because we just called bidi_cache_find above.
	     That state describes the outermost opening bracket, the
	     one with which we entered this function.  Force the cache
	     to "forget" all the cached states starting from that state.  */
	  bidi_cache_reset_to (bidi_cache_last_idx - bidi_cache_start);
	  /* Set up the next_for_neutral member, to help
	     bidi_resolve_neutral.  */
	  bidi_it->next_for_neutral.type = embedding_type;
	  bidi_it->next_for_neutral.charpos = pairing_pos;
	  /* Pretend we didn't resolve this bracket.  */
	  retval = false;
	}
    }

 give_up:
  return retval;
}

static void
bidi_record_type_for_neutral (struct bidi_saved_info *info, int level,
			      bool nextp)
{
  int idx;

  for (idx = bidi_cache_last_idx + 1; idx < bidi_cache_idx; idx++)
    {
      int lev = bidi_cache[idx].level_stack[bidi_cache[idx].stack_idx].level;

      if (lev <= level)
	{
	  eassert (lev == level);
	  if (nextp)
	    bidi_cache[idx].next_for_neutral = *info;
	  else
	    bidi_cache[idx].prev_for_neutral = *info;
	  break;
	}
    }
}

static bidi_type_t
bidi_resolve_brackets (struct bidi_it *bidi_it)
{
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  bool resolve_bracket = false;
  bidi_type_t type = UNKNOWN_BT;
  int ch;
  struct bidi_saved_info prev_for_neutral, next_for_neutral;
  ptrdiff_t eob
    = ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
       ? bidi_it->string.schars : ZV);

  /* Record the prev_for_neutral type either from the previous
     character, if it was a strong or AN/EN, or from the
     prev_for_neutral information recorded previously.  */
  if (bidi_it->type == STRONG_L || bidi_it->type == STRONG_R
      || bidi_it->type == WEAK_AN || bidi_it->type == WEAK_EN)
    bidi_remember_char (&prev_for_neutral, bidi_it, 1);
  else
    prev_for_neutral = bidi_it->prev_for_neutral;
  /* Record the next_for_neutral type information.  */
  if (bidi_it->next_for_neutral.charpos > bidi_it->charpos)
    next_for_neutral = bidi_it->next_for_neutral;
  else
    next_for_neutral.charpos = -1;
  if (!bidi_it->first_elt)
    {
      type = bidi_cache_find (bidi_it->charpos + bidi_it->nchars, 0, bidi_it);
      ch = bidi_it->ch;
    }
  if (type == UNKNOWN_BT)
    {
      type = bidi_resolve_weak (bidi_it);
      if (type == NEUTRAL_ON)
	{
	  /* bracket_pairing_pos == eob means this bracket does not
	     need to be resolved as a bracket, but as a neutral, see
	     the optimization trick we play near the end of
	     bidi_find_bracket_pairs.  */
	  if (bidi_it->bracket_pairing_pos == eob)
	    {
	      /* If this is the outermost closing bracket of a run of
		 characters in which we decided to resolve brackets as
		 neutrals, use the embedding level's type, recorded in
		 bracket_enclosed_type, to resolve the bracket.  */
	      if (bidi_it->next_for_neutral.charpos == bidi_it->charpos
		  && bidi_paired_bracket_type (bidi_it->ch) == BIDI_BRACKET_CLOSE)
		type = bidi_it->bracket_enclosed_type;
	    }
	  else if (bidi_find_bracket_pairs (bidi_it))
	    resolve_bracket = true;
	}
    }
  else if (bidi_it->bracket_pairing_pos != eob)
    {
      eassert (bidi_it->resolved_level == -1);
      /* If the cached state shows an increase of embedding level due
	 to an isolate initiator, we need to update the 1st cached
	 state of the next run of the current isolating sequence with
	 the prev_for_neutral and next_for_neutral information, so
	 that it will be picked up when we advance to that next run.  */
      if (bidi_it->level_stack[bidi_it->stack_idx].level > prev_level
	  && ISOLATE_STATUS (bidi_it, bidi_it->stack_idx))
	{
	  bidi_record_type_for_neutral (&prev_for_neutral, prev_level, 0);
	  bidi_record_type_for_neutral (&next_for_neutral, prev_level, 1);
	}
      if (type == NEUTRAL_ON
	  && bidi_paired_bracket_type (ch) == BIDI_BRACKET_OPEN)
	{
	  if (bidi_it->bracket_pairing_pos > bidi_it->charpos)
	    {
	      /* A cached opening bracket that wasn't completely
		 resolved yet.  */
	      resolve_bracket = true;
	    }
	  else if (bidi_it->bracket_pairing_pos == -1)
	    {
	      /* Higher levels were not BPA-resolved yet, even if
		 cached by bidi_find_bracket_pairs.  Force application
		 of BPA to the new level now.  */
	      if (bidi_find_bracket_pairs (bidi_it))
		resolve_bracket = true;
	    }
	}
      /* Keep track of the prev_for_neutral and next_for_neutral
	 types, needed for resolving brackets below and for resolving
	 neutrals in bidi_resolve_neutral.  */
      if (bidi_it->level_stack[bidi_it->stack_idx].level == prev_level)
	{
	  bidi_it->prev_for_neutral = prev_for_neutral;
	  if (next_for_neutral.charpos > 0)
	    bidi_it->next_for_neutral = next_for_neutral;
	}
    }

  /* If needed, resolve the bracket type according to N0.  */
  if (resolve_bracket)
    {
      int embedding_level = bidi_it->level_stack[bidi_it->stack_idx].level;
      bidi_type_t embedding_type = (embedding_level & 1) ? STRONG_R : STRONG_L;

      eassert (bidi_it->prev_for_neutral.type != UNKNOWN_BT);
      eassert (bidi_it->bracket_pairing_pos > bidi_it->charpos);
      if (bidi_it->bracket_enclosed_type == embedding_type) /* N0b */
	type = embedding_type;
      else
	{
	  switch (bidi_it->prev_for_neutral.type)
	    {
	    case STRONG_R:
	    case WEAK_EN:
	    case WEAK_AN:
	      type =
		(bidi_it->bracket_enclosed_type == STRONG_R) /* N0c */
		? STRONG_R				     /* N0c1 */
		: embedding_type;			     /* N0c2 */
	      break;
	    case STRONG_L:
	      type =
		(bidi_it->bracket_enclosed_type == STRONG_L) /* N0c */
		? STRONG_L				     /* N0c1 */
		: embedding_type;			     /* N0c2 */
	      break;
	    default:
	      /* N0d: Do not set the type for that bracket pair.  */
	      break;
	    }
	}
      eassert (type == STRONG_L || type == STRONG_R || type == NEUTRAL_ON);

      /* Update the type of the paired closing bracket to the same
	 type as for the resolved opening bracket.  */
      if (type != NEUTRAL_ON)
	{
	  ptrdiff_t idx = bidi_cache_search (bidi_it->bracket_pairing_pos,
					     -1, 1);

	  if (idx < bidi_cache_start)
	    emacs_abort ();
	  bidi_cache[idx].type = type;
	}
    }

  return type;
}

static bidi_type_t
bidi_resolve_neutral (struct bidi_it *bidi_it)
{
  bidi_type_t type = bidi_resolve_brackets (bidi_it);
  int current_level;
  bool is_neutral;

  eassert (type == STRONG_R
	   || type == STRONG_L
	   || type == WEAK_BN
	   || type == WEAK_EN
	   || type == WEAK_AN
	   || type == NEUTRAL_B
	   || type == NEUTRAL_S
	   || type == NEUTRAL_WS
	   || type == NEUTRAL_ON
	   || type == LRI
	   || type == RLI
	   || type == PDI);

  current_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  eassert (current_level >= 0);
  is_neutral = bidi_get_category (type) == NEUTRAL;

  if ((type != NEUTRAL_B /* Don't risk entering the long loop below if
			    we are already at paragraph end.  */
       && (is_neutral || bidi_isolate_fmt_char (type)))
      /* N1-N2/Retaining */
      || type == WEAK_BN)
    {
      if (bidi_it->next_for_neutral.type != UNKNOWN_BT
	  && (bidi_it->next_for_neutral.charpos > bidi_it->charpos
	      /* PDI defines an eos, so it's OK for it to serve as its
		 own next_for_neutral.  */
	      || (bidi_it->next_for_neutral.charpos == bidi_it->charpos
		  && bidi_it->type == PDI)))
	{
	  type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
					 bidi_it->next_for_neutral.type,
					 current_level);
	}
      /* The next two "else if" clauses are shortcuts for the
	 important special case when we have a long sequence of
	 neutral or WEAK_BN characters, such as whitespace or NULs or
	 other control characters, on the base embedding level of the
	 paragraph, and that sequence goes all the way to the end of
	 the paragraph and follows a character whose resolved
	 directionality is identical to the base embedding level.
	 (This is what happens in a buffer with plain L2R text that
	 happens to include long sequences of control characters.)  By
	 virtue of N1, the result of examining this long sequence will
	 always be either STRONG_L or STRONG_R, depending on the base
	 embedding level.  So we use this fact directly instead of
	 entering the expensive loop in the "else" clause.  */
      else if (current_level == 0
	       && bidi_it->prev_for_neutral.type == STRONG_L
	       && (ASCII_CHAR_P (bidi_it->ch)
		   || (type != WEAK_BN
		       && !bidi_explicit_dir_char (bidi_it->ch)
		       && !bidi_isolate_fmt_char (type))))
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       STRONG_L, current_level);
      else if (/* current level is 1 */
	       current_level == 1
	       /* base embedding level is also 1 */
	       && bidi_it->level_stack[0].level == 1
	       /* previous character is one of those considered R for
		  the purposes of W5 */
	       && (bidi_it->prev_for_neutral.type == STRONG_R
		   || bidi_it->prev_for_neutral.type == WEAK_EN
		   || bidi_it->prev_for_neutral.type == WEAK_AN)
	       && type != WEAK_BN
	       && !bidi_explicit_dir_char (bidi_it->ch)
	       && !bidi_isolate_fmt_char (type))
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       STRONG_R, current_level);
      else
	{
	  /* Arrrgh!!  The UAX#9 algorithm is too deeply entrenched in
	     the assumption of batch-style processing; see clauses W4,
	     W5, and especially N1, which require looking far forward
	     (as well as back) in the buffer/string.  May the fleas of
	     a thousand camels infest the armpits of those who design
	     supposedly general-purpose algorithms by looking at their
	     own implementations, and fail to consider other possible
	     implementations!  */
	  struct bidi_it saved_it;
	  bidi_type_t next_type;
	  bool adjacent_to_neutrals = is_neutral;

	  bidi_copy_it (&saved_it, bidi_it);
	  /* Scan the text forward until we find the first non-neutral
	     character, and then use that to resolve the neutral we
	     are dealing with now.  We also cache the scanned iterator
	     states, to salvage some of the effort later.  */
	  do {
	    int old_sidx, new_sidx;

	    /* Paragraph separators have their levels fully resolved
	       at this point, so cache them as resolved.  */
	    bidi_cache_iterator_state (bidi_it, type == NEUTRAL_B, 0);
	    old_sidx = bidi_it->stack_idx;
	    type = bidi_resolve_brackets (bidi_it);
	    /* Skip level runs excluded from this isolating run sequence.  */
	    new_sidx = bidi_it->stack_idx;
	    if (bidi_it->level_stack[new_sidx].level > current_level
		&& (ISOLATE_STATUS (bidi_it, new_sidx)
		    /* This is for when we have an isolate initiator
		       immediately followed by an embedding or
		       override initiator, in which case we get the
		       level stack pushed twice by the single call to
		       bidi_resolve_weak above.  */
		    || (new_sidx > old_sidx + 1
			&& ISOLATE_STATUS (bidi_it, new_sidx - 1))))
	      {
		while (bidi_it->level_stack[bidi_it->stack_idx].level
		       > current_level)
		  {
		    bidi_cache_iterator_state (bidi_it, type == NEUTRAL_B, 0);
		    type = bidi_resolve_brackets (bidi_it);
		  }
	      }
	    if (!adjacent_to_neutrals
		&& (bidi_get_category (type) == NEUTRAL
		    || bidi_isolate_fmt_char (type)))
	      adjacent_to_neutrals = true;
	  } while (!(type == NEUTRAL_B
		     || (type != WEAK_BN
			 && bidi_get_category (type) != NEUTRAL
			 && !bidi_isolate_fmt_char (type))
		     /* This is all per level run, so stop when we
			reach the end of this level run.  */
		     || (bidi_it->level_stack[bidi_it->stack_idx].level
			 != current_level)));

	  /* Record the character we stopped at.  */
	  bidi_remember_char (&saved_it.next_for_neutral, bidi_it, 1);

	  if ((bidi_it->level_stack[bidi_it->stack_idx].level != current_level)
	      || type == NEUTRAL_B)
	    {
	      /* Marched all the way to the end of this level run.  We
		 need to use the eos type, whose information is stored
		 by bidi_set_sos_type in the prev_for_neutral
		 member.  */
	      if (adjacent_to_neutrals)
		next_type = bidi_it->prev_for_neutral.type;
	      else
		{
		  /* This is a BN which does not adjoin neutrals.
		     Leave its type alone.  */
		  bidi_copy_it (bidi_it, &saved_it);
		  return bidi_it->type;
		}
	    }
	  else
	    {
	      switch (type)
		{
		case STRONG_L:
		case STRONG_R:
		case STRONG_AL:
		  /* Actually, STRONG_AL cannot happen here, because
		     bidi_resolve_weak converts it to STRONG_R, per W3.  */
		  eassert (type != STRONG_AL);
		  next_type = type;
		  break;
		case WEAK_EN:
		case WEAK_AN:
		  /* N1: "European and Arabic numbers act as if they
		     were R in terms of their influence on NIs."  */
		  next_type = STRONG_R;
		  break;
		default:
		  emacs_abort ();
		  break;
		}
	    }
	  /* Resolve the type of all the NIs found during the above loop.  */
	  type = bidi_resolve_neutral_1 (saved_it.prev_for_neutral.type,
					 next_type, current_level);
	  /* Update next_for_neutral with the resolved type, so we
	     could use it for all the other NIs up to the place where
	     we exited the loop.  */
	  saved_it.next_for_neutral.type = next_type;
	  bidi_check_type (type);
	  /* Update the character which caused us to enter the above loop.  */
	  saved_it.type = type;
	  bidi_check_type (next_type);
	  bidi_copy_it (bidi_it, &saved_it);
	}
    }
  return type;
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer/string to the next character (in the logical order),
   resolve the bidi type of that next character, and return that
   type.  */
static bidi_type_t
bidi_type_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type;

  /* This should always be called during a forward scan.  */
  if (bidi_it->scan_dir != 1)
    emacs_abort ();

  type = bidi_resolve_neutral (bidi_it);

  return type;
}

/* Given an iterator state BIDI_IT, advance one character position in
   the buffer/string to the next character (in the current scan
   direction), resolve the embedding and implicit levels of that next
   character, and return the resulting level.  */
static int
bidi_level_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type = UNKNOWN_BT;
  int level;
  ptrdiff_t next_char_pos = -2;

  if (bidi_it->scan_dir == 1)
    {
      ptrdiff_t eob
	= ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
	   ? bidi_it->string.schars : ZV);

      /* There's no sense in trying to advance if we've already hit
	 the end of text.  */
      if (bidi_it->charpos >= eob)
	{
	  eassert (bidi_it->resolved_level >= 0);
	  return bidi_it->resolved_level;
	}
    }

  /* Perhaps the character we want is already cached as fully resolved.
     If it is, the call to bidi_cache_find below will return a type
     other than UNKNOWN_BT.  */
  if (bidi_cache_idx > bidi_cache_start && !bidi_it->first_elt)
    {
      int bob = ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
		 ? 0 : 1);

      if (bidi_it->scan_dir > 0)
	{
	  if (bidi_it->nchars <= 0)
	    emacs_abort ();
	  next_char_pos = bidi_it->charpos + bidi_it->nchars;
	}
      else if (bidi_it->charpos >= bob)
	/* Implementation note: we allow next_char_pos to be as low as
	   0 for buffers or -1 for strings, and that is okay because
	   that's the "position" of the sentinel iterator state we
	   cached at the beginning of the iteration.  */
	next_char_pos = bidi_it->charpos - 1;
      if (next_char_pos >= bob - 1)
	type = bidi_cache_find (next_char_pos, 1, bidi_it);
      if (type != UNKNOWN_BT)
	{
	  /* We asked the cache for fully resolved states.  */
	  eassert (bidi_it->resolved_level >= 0);
	  return bidi_it->resolved_level;
	}
    }

  if (bidi_it->scan_dir == -1)
    /* If we are going backwards, the iterator state is already cached
       from previous scans, and should be fully resolved.  */
    emacs_abort ();

  if (type == UNKNOWN_BT)
    type = bidi_type_of_next_char (bidi_it);

  if (type == NEUTRAL_B)
    {
      eassert (bidi_it->resolved_level >= 0);
      return bidi_it->resolved_level;
    }

  level = bidi_it->level_stack[bidi_it->stack_idx].level;

  eassert ((type == STRONG_R
	    || type == STRONG_L
	    || type == WEAK_BN
	    || type == WEAK_EN
	    || type == WEAK_AN));
  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  /* For L1 below, we need to know, for each WS character, whether
     it belongs to a sequence of WS characters preceding a newline
     or a TAB or a paragraph separator.  */
  if ((bidi_it->orig_type == NEUTRAL_WS
       || bidi_it->orig_type == WEAK_BN
       || bidi_isolate_fmt_char (bidi_it->orig_type))
      && bidi_it->next_for_ws.charpos < bidi_it->charpos
      /* If this character is already at base level, we don't need to
	 reset it, so avoid the potentially costly loop below.  */
      && level != bidi_it->level_stack[0].level)
    {
      int ch;
      ptrdiff_t clen = bidi_it->ch_len;
      ptrdiff_t bpos = bidi_it->bytepos;
      ptrdiff_t cpos = bidi_it->charpos;
      ptrdiff_t disp_pos = bidi_it->disp_pos;
      ptrdiff_t nc = bidi_it->nchars;
      struct bidi_string_data bs = bidi_it->string;
      bidi_type_t chtype;
      bool fwp = bidi_it->frame_window_p;
      int dpp = bidi_it->disp_prop;

      if (bidi_it->nchars <= 0)
	emacs_abort ();
      do {
	ch = bidi_fetch_char (cpos += nc, bpos += clen, &disp_pos, &dpp, &bs,
			      bidi_it->w, fwp, &clen, &nc);
	chtype = bidi_get_type (ch, NEUTRAL_DIR);
      } while (chtype == NEUTRAL_WS || chtype == WEAK_BN
	       || bidi_isolate_fmt_char (chtype)
	       || bidi_explicit_dir_char (ch)); /* L1/Retaining */
      bidi_it->next_for_ws.type = chtype;
      bidi_check_type (bidi_it->next_for_ws.type);
      bidi_it->next_for_ws.charpos = cpos;
    }

  /* Update the cache, but only if this state was already cached.  */
  bidi_cache_iterator_state (bidi_it, 1, 1);

  /* Resolve implicit levels.  */
  if (bidi_it->orig_type == NEUTRAL_B /* L1 */
      || bidi_it->orig_type == NEUTRAL_S
      || bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB
      || ((bidi_it->orig_type == NEUTRAL_WS
	   || bidi_it->orig_type == WEAK_BN
	   || bidi_isolate_fmt_char (bidi_it->orig_type)
	   || bidi_explicit_dir_char (bidi_it->ch))
	  && (bidi_it->next_for_ws.type == NEUTRAL_B
	      || bidi_it->next_for_ws.type == NEUTRAL_S)))
    level = bidi_it->level_stack[0].level;
  else if ((level & 1) == 0) /* I1 */
    {
      if (type == STRONG_R)
	level++;
      else if (type == WEAK_EN || type == WEAK_AN)
	level += 2;
    }
  else			/* I2 */
    {
      if (type == STRONG_L || type == WEAK_EN || type == WEAK_AN)
	level++;
    }

  bidi_it->resolved_level = level;
  return level;
}

/* Move to the other edge of a level given by LEVEL.  If END_FLAG,
   we are at the end of a level, and we need to prepare to
   resume the scan of the lower level.

   If this level's other edge is cached, we simply jump to it, filling
   the iterator structure with the iterator state on the other edge.
   Otherwise, we walk the buffer or string until we come back to the
   same level as LEVEL.

   Note: we are not talking here about a ``level run'' in the UAX#9
   sense of the term, but rather about a ``level'' which includes
   all the levels higher than it.  In other words, given the levels
   like this:

         11111112222222333333334443343222222111111112223322111
                A      B                    C

   and assuming we are at point A scanning left to right, this
   function moves to point C, whereas the UAX#9 ``level 2 run'' ends
   at point B.  */
static void
bidi_find_other_level_edge (struct bidi_it *bidi_it, int level, bool end_flag)
{
  int dir = end_flag ? -bidi_it->scan_dir : bidi_it->scan_dir;
  ptrdiff_t idx;

  /* Try the cache first.  */
  if ((idx = bidi_cache_find_level_change (level, dir, end_flag))
      >= bidi_cache_start)
    bidi_cache_fetch_state (idx, bidi_it);
  else
    {
      int new_level;

      /* If we are at end of level, its edges must be cached.  */
      if (end_flag)
	emacs_abort ();

      if (!bidi_cache_iterator_state (bidi_it, 1, 0))
	{
	  /* Can't happen: if the cache needs to grow, it means we
	     were at base embedding level, so the cache should have
	     been either empty or already large enough to cover this
	     character position.  */
	  emacs_abort ();
	}
      do {
	new_level = bidi_level_of_next_char (bidi_it);
	/* If the cache is full, perform an emergency return by
	   pretending that the level ended.  */
	if (!bidi_cache_iterator_state (bidi_it, 1, 0))
	  {
	    new_level = level - 1;
	    /* Since the cache should only grow when we are scanning
	       forward looking for the edge of the level that is one
	       above the base embedding level, we can only have this
	       contingency when LEVEL - 1 is the base embedding
	       level.  */
	    eassert (new_level == bidi_it->level_stack[0].level);
	    /* Plan B, for when the cache overflows: Back up to the
	       previous character by fetching the last cached state,
	       and force the resolved level of that character be the
	       base embedding level.  */
	    bidi_cache_fetch_state (bidi_cache_idx - 1, bidi_it);
	    bidi_it->resolved_level = new_level;
	    bidi_cache_iterator_state (bidi_it, 1, 1);
	  }
      } while (new_level >= level);
    }
}

void
bidi_move_to_visually_next (struct bidi_it *bidi_it)
{
  int old_level, new_level, next_level;
  struct bidi_it sentinel;

  if (bidi_it->charpos < 0 || bidi_it->bytepos < 0)
    emacs_abort ();

  if (bidi_it->scan_dir == 0)
    {
      bidi_it->scan_dir = 1;	/* default to logical order */
    }

  /* If we just passed a newline, initialize for the next line.  */
  if (!bidi_it->first_elt
      && (bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB))
    bidi_line_init (bidi_it);

  /* Prepare the sentinel iterator state, and cache it.  When we bump
     into it, scanning backwards, we'll know that the last non-base
     level is exhausted.  */
  if (bidi_cache_idx == bidi_cache_start)
    {
      bidi_copy_it (&sentinel, bidi_it);
      if (bidi_it->first_elt)
	{
	  sentinel.charpos--;	/* cached charpos needs to be monotonic */
	  sentinel.bytepos--;
	  sentinel.ch = '\n';	/* doesn't matter, but why not? */
	  sentinel.ch_len = 1;
	  sentinel.nchars = 1;
	}
      bidi_cache_iterator_state (&sentinel, 1, 0);
    }

  old_level = bidi_it->resolved_level;
  new_level = bidi_level_of_next_char (bidi_it);

  /* Reordering of resolved levels (clause L2) is implemented by
     jumping to the other edge of the level and flipping direction of
     scanning the text whenever we find a level change.  */
  if (new_level != old_level)
    {
      bool ascending = new_level > old_level;
      int level_to_search = ascending ? old_level + 1 : old_level;
      int incr = ascending ? 1 : -1;
      int expected_next_level = old_level + incr;

      /* Jump (or walk) to the other edge of this level.  */
      bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
      /* Switch scan direction and peek at the next character in the
	 new direction.  */
      bidi_it->scan_dir = -bidi_it->scan_dir;

      /* The following loop handles the case where the resolved level
	 jumps by more than one.  This is typical for numbers inside a
	 run of text with left-to-right embedding direction, but can
	 also happen in other situations.  In those cases the decision
	 where to continue after a level change, and in what direction,
	 is tricky.  For example, given a text like below:

	          abcdefgh
	          11336622

	 (where the numbers below the text show the resolved levels),
	 the result of reordering according to UAX#9 should be this:

		  efdcghba

	 This is implemented by the loop below which flips direction
	 and jumps to the other edge of the level each time it finds
	 the new level not to be the expected one.  The expected level
	 is always one more or one less than the previous one.  */
      next_level = bidi_peek_at_next_level (bidi_it);
      while (next_level != expected_next_level)
	{
	  /* If next_level is -1, it means we have an unresolved level
	     in the cache, which at this point should not happen.  If
	     it does, we will infloop.  */
	  eassert (next_level >= 0);
	  /* If next_level is not consistent with incr, we might
	     infloop.  */
	  eassert (incr > 0
		   ? next_level > expected_next_level
		   : next_level < expected_next_level);
	  expected_next_level += incr;
	  level_to_search += incr;
	  bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
	  bidi_it->scan_dir = -bidi_it->scan_dir;
	  next_level = bidi_peek_at_next_level (bidi_it);
	}

      /* Finally, deliver the next character in the new direction.  */
      next_level = bidi_level_of_next_char (bidi_it);
    }

  /* Take note when we have just processed the newline that precedes
     the end of the paragraph.  The next time we are about to be
     called, set_iterator_to_next will automatically reinit the
     paragraph direction, if needed.  We do this at the newline before
     the paragraph separator, because the next character might not be
     the first character of the next paragraph, due to the bidi
     reordering, whereas we _must_ know the paragraph base direction
     _before_ we process the paragraph's text, since the base
     direction affects the reordering.  */
  if (bidi_it->scan_dir == 1
      && (bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB))
    {
      /* The paragraph direction of the entire string, once
	 determined, is in effect for the entire string.  Setting the
	 separator limit to the end of the string prevents
	 bidi_paragraph_init from being called automatically on this
	 string.  */
      if (bidi_it->string.s || STRINGP (bidi_it->string.lstring))
	bidi_it->separator_limit = bidi_it->string.schars;
      else if (bidi_it->bytepos < ZV_BYTE)
	{
	  ptrdiff_t sep_len
	    = bidi_at_paragraph_end (bidi_it->charpos + bidi_it->nchars,
				     bidi_it->bytepos + bidi_it->ch_len);
	  if (bidi_it->nchars <= 0)
	    emacs_abort ();
	  if (sep_len >= 0)
	    {
	      bidi_it->new_paragraph = 1;
	      /* Record the buffer position of the last character of
		 the paragraph separator.  If the paragraph separator
		 is an empty string (e.g., the regex is "^"), the
		 newline that precedes the end of the paragraph is
		 that last character.  */
	      if (sep_len > 0)
		bidi_it->separator_limit
		  = bidi_it->charpos + bidi_it->nchars + sep_len;
	      else
		bidi_it->separator_limit = bidi_it->charpos;
	    }
	}
    }

  if (bidi_it->scan_dir == 1 && bidi_cache_idx > bidi_cache_start)
    {
      /* If we are at paragraph's base embedding level and beyond the
	 last cached position, the cache's job is done and we can
	 discard it.  */
      if (bidi_it->resolved_level == bidi_it->level_stack[0].level
	  && bidi_it->charpos > (bidi_cache[bidi_cache_idx - 1].charpos
				 + bidi_cache[bidi_cache_idx - 1].nchars - 1))
	bidi_cache_reset ();
      /* Also reset the cache if it overflowed and we have just
	 emergency-exited using Plan B.  */
      else if (bidi_it->resolved_level == bidi_it->level_stack[0].level
	       && bidi_cache_idx >= bidi_cache_size
	       && bidi_it->charpos == bidi_cache[bidi_cache_idx - 1].charpos)
	bidi_cache_reset ();
	/* But as long as we are caching during forward scan, we must
	   cache each state, or else the cache integrity will be
	   compromised: it assumes cached states correspond to buffer
	   positions 1:1.  */
      else
	bidi_cache_iterator_state (bidi_it, 1, 0);
    }

  eassert (bidi_it->resolved_level >= 0
	   && bidi_it->resolved_level <= BIDI_MAXDEPTH + 2);
}

/* Utility function for looking for strong directional characters
   whose bidi type was overridden by a directional override.  */
ptrdiff_t
bidi_find_first_overridden (struct bidi_it *bidi_it)
{
  ptrdiff_t found_pos = ZV;

  do
    {
      /* Need to call bidi_resolve_weak, not bidi_resolve_explicit,
	 because the directional overrides are applied by the
	 former.  */
      bidi_type_t type = bidi_resolve_weak (bidi_it);

      if ((type == STRONG_R && bidi_it->orig_type == STRONG_L)
	  || (type == STRONG_L
	      && (bidi_it->orig_type == STRONG_R
		  || bidi_it->orig_type == STRONG_AL)))
	found_pos = bidi_it->charpos;
    } while (found_pos == ZV
	     && bidi_it->charpos < ZV
	     && bidi_it->ch != BIDI_EOB
	     && bidi_it->ch != '\n');

  return found_pos;
}

/* This is meant to be called from within the debugger, whenever you
   wish to examine the cache contents.  */
void bidi_dump_cached_states (void) EXTERNALLY_VISIBLE;
void
bidi_dump_cached_states (void)
{
  ptrdiff_t i;
  int ndigits = 1;

  if (bidi_cache_idx == 0)
    {
      fputs ("The cache is empty.\n", stderr);
      return;
    }
  fprintf (stderr, "Total of  %"pD"d state%s in cache:\n",
	   bidi_cache_idx, bidi_cache_idx == 1 ? "" : "s");

  for (i = bidi_cache[bidi_cache_idx - 1].charpos; i > 0; i /= 10)
    ndigits++;
  fputs ("ch  ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*c", ndigits, bidi_cache[i].ch);
  fputs ("\nlvl ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*d", ndigits, bidi_cache[i].resolved_level);
  fputs ("\npos ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*"pD"d", ndigits, bidi_cache[i].charpos);
  putc ('\n', stderr);
}
