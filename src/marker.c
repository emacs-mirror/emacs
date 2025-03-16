/* Markers: examining, setting and deleting.
   Copyright (C) 1985, 1997-1998, 2001-2025 Free Software Foundation,
   Inc.

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

/* Work around GCC bug 113253.  */
#if __GNUC__ == 13 && __GNUC_MINOR__ < 3
# pragma GCC diagnostic ignored "-Wanalyzer-deref-before-check"
#endif

#include "lisp.h"
#include "marker.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "stdlib.h"

#ifdef ABSTRACT_LISP_MARKER
struct Lisp_Marker
{
  union vectorlike_header header;

  /* This is the buffer that the marker points into, or 0 if it points nowhere.
     Note: a chain of markers can contain markers pointing into different
     buffers (the chain is per buffer_text rather than per buffer, so it's
     shared between indirect buffers).  */
  /* This is used for (other than NULL-checking):
     - Fmarker_buffer
     - Fset_marker: check eq(oldbuf, newbuf) to avoid unchain+rechain.
     - unchain_marker: to find the list from which to unchain.
     - Fkill_buffer: to only unchain the markers of current indirect buffer.
     */
  struct buffer *buffer;

  /* This flag is temporarily used in the functions
     decode/encode_coding_object to record that the marker position
     must be adjusted after the conversion.  */
  bool_bf need_adjustment : 1;
  /* True means normal insertion at the marker's position
     leaves the marker after the inserted text.  */
  bool_bf insertion_type : 1;
  /* True means that this is a chars<->bytes conversion cache entry
     rather than a true marker.  */
  bool_bf cache : 1;

  /* The remaining fields are meaningless in a marker that
     does not point anywhere.  */

  /* This is the char position where the marker points.  */
  ptrdiff_t charpos;
  /* This is the byte position.
     It's mostly used as a charpos<->bytepos cache (i.e. it's not directly
     used to implement the functionality of markers, but rather to (ab)use
     markers as a cache for char<->byte mappings).  */
  ptrdiff_t bytepos;
} GCALIGNED_STRUCT;
#endif

/* Record one cached position found recently by
   buf_charpos_to_bytepos or buf_bytepos_to_charpos.  */

static ptrdiff_t cached_charpos;
static ptrdiff_t cached_bytepos;
static struct buffer *cached_buffer;
static modiff_count cached_modiff;

/*****************************************************************/
/* Set of markers represented as a sorted array-with-gap (AWG).  */
/*****************************************************************/

typedef unsigned int m_index_t;

struct Lisp_Markers
{
  m_index_t size;
  m_index_t gap_beg;
  m_index_t gap_end;
  struct Lisp_Marker *markers[FLEXIBLE_ARRAY_MEMBER];
};

static void
markers_sanity_check (struct Lisp_Markers *t)
{
#ifdef MARKER_DEBUG
  eassert (t->markers);
  eassert (t->size > 0);
  eassert (t->gap_beg >= 0);
  eassert (t->gap_beg <= t->gap_end);
  eassert (t->gap_end <= t->size);

  m_index_t i;
  ptrdiff_t lastpos = BEG;
  for (i = 0; i < t->size; i++)
    {
      if (i == t->gap_beg)
	{
	  for (; i < t->gap_end; i++)
	    eassert (t->markers[i] == NULL);
	  if (i == t->size)
	    break;
	}
      eassert (t->markers[i]->buffer);
      /* eassert (BUF_ALL_MARKERS (t->markers[i]->buffer) == t); */
      eassert (t->markers[i]->charpos >= lastpos);
      lastpos = t->markers[i]->charpos;
    }
#endif
}

#define DEFINE_SEARCH_FUN(funname, thepos) 			       	     \
  static m_index_t			   			       	     \
  funname (struct Lisp_Markers *t, ptrdiff_t thepos) 	    	       	     \
  {								       	     \
    m_index_t bot, top;					       	     	     \
    markers_sanity_check (t);						     \
    /* See whether to search before or after the gap.  */		     \
    if (t->gap_beg >= 1 && t->markers[t->gap_beg - 1]->thepos >= thepos)     \
      (bot = 0, top = t->gap_beg);				       	     \
    else							       	     \
      (bot = t->gap_end, top = t->size);			       	     \
									     \
    /* Binary search.  */						     \
    while (bot < top)						       	     \
      {								       	     \
	m_index_t mid = bot + ((top - bot) / 2);		       	     \
	eassert (mid < top);					       	     \
	if (t->markers[mid]->thepos >= thepos)			       	     \
	  top = mid;						       	     \
	else							       	     \
	  bot = mid + 1;					       	     \
      }								       	     \
    eassert (bot == top);					       	     \
    eassert (bot <= t->size);					       	     \
    eassert (bot == t->size || t->markers[bot]->thepos >= thepos);     	     \
    eassert ((bot == t->gap_end ? t->gap_beg : bot) == 0	 	     \
	     || t->markers[(bot == t->gap_end ? t->gap_beg : bot)-1]->thepos \
		< thepos); 						     \
    return bot;							       	     \
  }

/* Return the best approximation of the index in the AWG
   corresponding to a given charpos or bytepos.  */
DEFINE_SEARCH_FUN (markers_search_charpos, charpos);
DEFINE_SEARCH_FUN (markers_search_bytepos, bytepos);

/* Return the index in the AWG where the marker M is found.
   This function presumes that M is indeed in the AWG.  */
static m_index_t
markers_search_marker (struct Lisp_Markers *t, struct Lisp_Marker *m)
{
  m_index_t beg = markers_search_charpos (t, m->charpos);
  eassert (t->markers[beg]->charpos == m->charpos);
  while (t->markers[beg] != m)
    {
      beg = (beg == t->gap_beg - 1) ? t->gap_end : beg + 1;
      eassert (t->markers[beg]->charpos == m->charpos);
    }
  return beg;
}

#ifdef MARKER_DEBUG
/* Return the index in the AWG where the marker M is found.
   This function presumes that M is indeed in the AWG.  */
bool
markers_member_p (struct Lisp_Markers *t, struct Lisp_Marker *m)
{
  m_index_t beg = markers_search_charpos (t, m->charpos);
  while (beg < t->size
	 && t->markers[beg] != m
	 && t->markers[beg]->charpos == m->charpos)
    beg = (beg == t->gap_beg - 1) ? t->gap_end : beg + 1;
  return t->markers[beg] == m;
}
#endif

static void
markers_move_gap (struct Lisp_Markers *t, m_index_t new_beg)
{
  m_index_t old_beg = t->gap_beg;
  m_index_t old_end = t->gap_end;
  eassert (new_beg <= t->gap_beg || new_beg >= t->gap_end);
  if (new_beg > old_beg)
    new_beg = old_beg + (new_beg - old_end);
  if (old_beg == new_beg)
    return;
  else if (new_beg > old_beg)
    memmove (&t->markers[old_beg], &t->markers[old_end],
	     (new_beg - old_beg) * sizeof (struct Lisp_Marker *));
  else
    {
      m_index_t size = old_beg - new_beg;
      memmove (&t->markers[old_end - size], &t->markers[new_beg],
	       size * sizeof (struct Lisp_Marker *));
    }
  t->gap_beg = new_beg;
  t->gap_end = new_beg + (old_end - old_beg);
  /* FIME: Get rid of it?  */
  memset (&t->markers[new_beg], 0,
	  (old_end - old_beg) * sizeof (struct Lisp_Marker *));
  markers_sanity_check (t);
}

static void
markers_move_gap_to_charpos (struct Lisp_Markers *t, ptrdiff_t charpos)
{
  m_index_t i = markers_search_charpos (t, charpos);
  markers_move_gap (t, i);
  eassert (t->gap_beg == 0
	   || t->markers[t->gap_beg - 1]->charpos < charpos);
  eassert (t->gap_end == t->size
	   || t->markers[t->gap_end]->charpos >= charpos);
}

void
markers_kill (struct Lisp_Markers *t, struct Lisp_Marker *m)
{
  m_index_t i;
  if (t->gap_beg > 0 && t->markers[t->gap_beg - 1] == m)
    /* Optimize common case, where we just added this marker.  */
    i = --t->gap_beg;
  else if (t->gap_end < t->size && t->markers[t->gap_end] == m)
    /* Since we add at the beginning of the gap, you might think this case
       is less common yet, in practice it seems to happen about as much
       as the previous case.  */
    i = t->gap_end++;
  else
    {
      i = markers_search_marker (t, m);
      if (i < t->gap_beg)
	{
	  markers_move_gap (t, i + 1);
	  eassert (t->gap_beg == i + 1);
	  t->gap_beg = i;
	}
      else
	{
	  markers_move_gap (t, i);
	  eassert (t->gap_end == i);
	  t->gap_end = i + 1;
	}
    }
  eassert (t->markers[i] == m);
  t->markers[i] = NULL;
  markers_sanity_check (t);
}

static struct Lisp_Markers *
markers_grow (struct Lisp_Markers *t)
{
  eassert (t->gap_beg == t->gap_end);
  m_index_t oldsize = t->size;
  m_index_t increment = max (2, oldsize / 2);
  m_index_t newsize = oldsize + increment;
  if (newsize < oldsize)	/* Overflow!  */
    /* This can happen only if you have ~4G markers, in which case the
       algorithmic complexity of the code in this file should make Emacs
       unusable anyway.  */
    error ("Table of markers full!");
  struct Lisp_Marker **oldmarkers = t->markers;
  struct Lisp_Markers *newt
    = xmalloc (sizeof (struct Lisp_Markers)
	       + newsize * sizeof (struct Lisp_Marker *));
  struct Lisp_Marker **newmarkers = newt->markers;
  memcpy (newmarkers, oldmarkers, oldsize * sizeof (struct Lisp_Marker *));
  memset (&newmarkers[oldsize], 0, increment * sizeof (struct Lisp_Marker *));
  xfree (t);
  newt->size = newsize;
  newt->gap_beg = oldsize;
  newt->gap_end = newsize;
  markers_sanity_check (newt);
  return newt;
}

struct Lisp_Markers *
markers_add (struct Lisp_Markers *t, struct Lisp_Marker *m)
{
  if (t->gap_beg == t->gap_end)
    t = markers_grow (t);
  ptrdiff_t charpos = m->charpos;
  /* In the vast majority of cases, the gap doesn't need to be moved.  */
  if ((t->gap_beg > 0 && t->markers[t->gap_beg - 1]->charpos > charpos)
      || (t->gap_end < t->size && t->markers[t->gap_end]->charpos < charpos))
    markers_move_gap_to_charpos (t, charpos);
  eassert (t->markers[t->gap_beg] == NULL);
  t->markers[t->gap_beg++] = m;
  markers_sanity_check (t);
  return t;
}

void
markers_adjust_for_insert (struct Lisp_Markers *t,
			   ptrdiff_t from, ptrdiff_t from_byte,
			   ptrdiff_t to, ptrdiff_t to_byte,
			   bool before_markers)
{
  markers_move_gap_to_charpos (t, from);
  m_index_t i = t->gap_end;
  m_index_t size = t->size;
  ptrdiff_t charoffset = to - from;
  ptrdiff_t byteoffset = to_byte - from_byte;
  if (charoffset == 0)
    /* return;  */ abort ();
  eassert (charoffset > 0);
  eassert (byteoffset >= charoffset);
  /* Adjusting for markers can require changing the order of markers
     because we move some but not all markers at 'from'. */
  if (!before_markers)
    while (i < size && t->markers[i]->charpos == from
	   && !t->markers[i]->insertion_type)
      i++;
  m_index_t first_that_moved = i;
  for (; i < size && t->markers[i]->charpos == from; i++)
    {
      if (before_markers || t->markers[i]->insertion_type)
	{
	  t->markers[i]->charpos += charoffset;
	  t->markers[i]->bytepos += byteoffset;
	}
      else
	{
	  /* We've moved some previous marker but we won't move this one,
	     so they would be out of order.  Swap them.  */
	  eassert (first_that_moved < i);
	  eassert (t->markers[first_that_moved]->charpos
		   == t->markers[i]->charpos + charoffset);
	  struct Lisp_Marker *m = t->markers[i];
	  t->markers[i] = t->markers[first_that_moved];
	  t->markers[first_that_moved] = m;
	  first_that_moved++;
	}
    }
  for (; i < size; i++)
    {
      t->markers[i]->charpos += charoffset;
      t->markers[i]->bytepos += byteoffset;
    }
  markers_sanity_check (t);
}

void
markers_adjust_for_replace (struct Lisp_Markers *t,
			    ptrdiff_t from, ptrdiff_t from_byte,
			    ptrdiff_t old_chars, ptrdiff_t old_bytes,
			    ptrdiff_t new_chars, ptrdiff_t new_bytes)
{
  /* If old_chars == 0 and new_chars > 0, this is an insertion, which
     requires more care with 'insertion_type' than what we do here.
     FIXME: Apparently we're sometimes called incorrectly, e.g. during
     src/search-tests and lisp/replace-tests.  */
  /* eassert (old_chars > 0 || new_chars == 0); */
  markers_move_gap_to_charpos (t, from);
  m_index_t i = t->gap_end;
  m_index_t size = t->size;
  ptrdiff_t prev_to = from + old_chars;
  for (; i < size && t->markers[i]->charpos < prev_to; i++)
    {
      t->markers[i]->charpos = from;
      t->markers[i]->bytepos = from_byte;
    }
  ptrdiff_t charoffset = new_chars - old_chars;
  ptrdiff_t byteoffset = new_bytes - old_bytes;
  for (; i < size; i++)
    {
      t->markers[i]->charpos += charoffset;
      t->markers[i]->bytepos += byteoffset;
    }
  markers_sanity_check (t);
}

void
markers_adjust_for_delete (struct Lisp_Markers *t,
			   ptrdiff_t from, ptrdiff_t from_byte,
			   ptrdiff_t to, ptrdiff_t to_byte)
{
  eassert (to >= from);
  markers_adjust_for_replace (t, from, from_byte,
			      to - from, to_byte - from_byte,
			      0, 0);
}

bool
markers_full_p (struct Lisp_Markers *t)
{
  return t->gap_beg == t->gap_end;
}

struct Lisp_Markers *
markers_new (unsigned int size)
{
  struct Lisp_Markers *t = xmalloc (sizeof (struct Lisp_Markers)
				    + size * sizeof (struct Lisp_Marker *));
  t->size = size;
  t->gap_beg = 0;
  t->gap_end = size;
  memset (t->markers, 0, size * sizeof (struct Lisp_Marker *));
  return t;
}

struct markers__iterator
{
  struct Lisp_Markers *t;
  m_index_t i;
  ptrdiff_t to;
};

struct markers_iterator
markers_iterator_all (struct Lisp_Markers *t)
{
  struct markers_iterator it = { .t = t, .i = t->gap_beg ? 0 : t->gap_end };
  it.m = it.i < t->size ? t->markers[it.i] : NULL;
  return it;
}

void
markers_iterator_next (struct markers_iterator *it)
{
  it->i++;
  if (it->i >= it->t->gap_beg && it->i < it->t->gap_end)
    it->i = it->t->gap_end;
  it->m = it->i < it->t->size ? it->t->markers[it->i] : NULL;
  eassert (it->i == it->t->size || it->m);
}


/***********************************************************/
/* Older set of markers as an unsorted linked list         */
/***********************************************************/


/* Juanma Barranquero <lekktu@gmail.com> reported ~3x increased
   bootstrap time when byte_char_debug_check is enabled; so this
   is never turned on by --enable-checking configure option.  */

#ifdef MARKER_DEBUG

extern int count_markers (struct buffer *) EXTERNALLY_VISIBLE;
extern ptrdiff_t verify_bytepos (ptrdiff_t charpos) EXTERNALLY_VISIBLE;

static void
byte_char_debug_check (struct buffer *b, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  ptrdiff_t nchars;

  if (NILP (BVAR (b, enable_multibyte_characters)))
    return;

  if (bytepos > BUF_GPT_BYTE (b))
    nchars
      = multibyte_chars_in_text (BUF_BEG_ADDR (b),
				 BUF_GPT_BYTE (b) - BUF_BEG_BYTE (b))
      + multibyte_chars_in_text (BUF_GAP_END_ADDR (b),
				 bytepos - BUF_GPT_BYTE (b));
  else
    nchars = multibyte_chars_in_text (BUF_BEG_ADDR (b),
				      bytepos - BUF_BEG_BYTE (b));

  if (charpos - 1 != nchars)
    emacs_abort ();
}

#else /* not MARKER_DEBUG */

#define byte_char_debug_check(b, charpos, bytepos) do { } while (0)

#endif /* MARKER_DEBUG */

void
clear_charpos_cache (struct buffer *b)
{
  if (cached_buffer == b)
    cached_buffer = 0;
}

/* Converting between character positions and byte positions.  */

/* There are several places in the buffer where we know
   the correspondence: BEG, BEGV, PT, GPT, ZV and Z,
   and everywhere there is a marker.  So we find the one of these places
   that is closest to the specified position, and scan from there.  */

/* This macro is a subroutine of buf_charpos_to_bytepos.
   Note that it is desirable that BYTEPOS is not evaluated
   except when we really want its value.  */

#define CONSIDER(CHARPOS, BYTEPOS)					\
{									\
  ptrdiff_t this_charpos = (CHARPOS);					\
  bool changed = false;							\
									\
  if (this_charpos == charpos)						\
    {									\
      ptrdiff_t value = (BYTEPOS);				       	\
									\
      byte_char_debug_check (b, charpos, value);			\
      return value;							\
    }									\
  else if (this_charpos > charpos)					\
    {									\
      if (this_charpos < best_above)					\
	{								\
	  best_above = this_charpos;					\
	  best_above_byte = (BYTEPOS);					\
	  changed = true;						\
	}								\
    }									\
  else if (this_charpos > best_below)					\
    {									\
      best_below = this_charpos;					\
      best_below_byte = (BYTEPOS);					\
      changed = true;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
        {								\
	  ptrdiff_t value = best_below_byte + (charpos - best_below);	\
									\
	  byte_char_debug_check (b, charpos, value);			\
	  return value;							\
	}								\
    }									\
}

static void
CHECK_MARKER (Lisp_Object x)
{
  CHECK_TYPE (MARKERP (x), Qmarkerp, x);
}

static void
cache_bytechar (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  Lisp_Object m = build_marker (buf, charpos, bytepos);
  XMARKER (m)->cache = true;
}

/* When converting bytes from/to chars, we look through the list of
   markers to try and find a good starting point (since markers keep
   track of both bytepos and charpos at the same time).
   But if there are many markers, it can take too much time to find a "good"
   marker from which to start.  Worse yet: if it takes a long time and we end
   up finding a nearby markers, we won't add a new marker to cache this
   result, so next time around we'll have to go through this same long list
   to (re)find this best marker.  So the further down the list of
   markers we go, the less demanding we are w.r.t what is a good marker.

   The previous code used INITIAL=50 and INCREMENT=0 and this lead to
   really poor performance when there are many markers.
   I haven't tried to tweak INITIAL, but experiments on my trusty Thinkpad
   T61 using various artificial test cases seem to suggest that INCREMENT=50
   might be "the best compromise": it significantly improved the
   worst case and it was rarely slower and never by much.

   The asymptotic behavior is still poor, tho, so in largish buffers with many
   overlays (e.g. 300KB and 30K overlays), it can still be a bottleneck.  */
#define BYTECHAR_DISTANCE_INITIAL 50

/* Return the byte position corresponding to CHARPOS in B.  */

ptrdiff_t
buf_charpos_to_bytepos (struct buffer *b, ptrdiff_t charpos)
{
  ptrdiff_t best_above, best_above_byte;
  ptrdiff_t best_below, best_below_byte;
  ptrdiff_t distance = BYTECHAR_DISTANCE_INITIAL;

  eassert (BUF_BEG (b) <= charpos && charpos <= BUF_Z (b));

  best_above = BUF_Z (b);
  best_above_byte = BUF_Z_BYTE (b);

  /* If this buffer has as many characters as bytes,
     each character must be one byte.
     This takes care of the case where enable-multibyte-characters is nil.  */
  if (best_above == best_above_byte)
    return charpos;

  best_below = BEG;
  best_below_byte = BEG_BYTE;

  /* We find in best_above and best_above_byte
     the closest known point above CHARPOS,
     and in best_below and best_below_byte
     the closest known point below CHARPOS,

     If at any point we can tell that the space between those
     two best approximations is all single-byte,
     we interpolate the result immediately.  */

  CONSIDER (BUF_PT (b), BUF_PT_BYTE (b));
  CONSIDER (BUF_GPT (b), BUF_GPT_BYTE (b));
  CONSIDER (BUF_BEGV (b), BUF_BEGV_BYTE (b));
  CONSIDER (BUF_ZV (b), BUF_ZV_BYTE (b));

  if (b == cached_buffer && BUF_MODIFF (b) == cached_modiff)
    CONSIDER (cached_charpos, cached_bytepos);

  if (!(best_above - charpos < distance
	|| charpos - best_below < distance))
    {
      struct Lisp_Markers *t = BUF_ALL_MARKERS (b);
      m_index_t i = markers_search_charpos (t, charpos);
      if (i < t->size)
	{
	  struct Lisp_Marker *m = t->markers[i];
	  CONSIDER (m->charpos, m->bytepos);
	}
      if (i == t->gap_end)
	i = t->gap_beg;
      if (i > 0)
	{
	  struct Lisp_Marker *m = t->markers[i - 1];
	  CONSIDER (m->charpos, m->bytepos);
	}
    }
  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  eassert (best_below <= charpos && charpos <= best_above);
  if (charpos - best_below < best_above - charpos)
    {
      bool record = charpos - best_below > 5000;

      while (best_below < charpos)
	{
	  best_below++;
	  best_below_byte += buf_next_char_len (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	cache_bytechar (b, best_below, best_below_byte);

      byte_char_debug_check (b, best_below, best_below_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      eassert (best_below_byte >= charpos);
      return best_below_byte;
    }
  else
    {
      bool record = best_above - charpos > 5000;

      while (best_above > charpos)
	{
	  best_above--;
	  best_above_byte -= buf_prev_char_len (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	cache_bytechar (b, best_above, best_above_byte);

      byte_char_debug_check (b, best_above, best_above_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      eassert (best_above_byte >= charpos);
      return best_above_byte;
    }
}

#undef CONSIDER

/* This macro is a subroutine of buf_bytepos_to_charpos.
   It is used when BYTEPOS is actually the byte position.  */

#define CONSIDER(BYTEPOS, CHARPOS)					\
{									\
  ptrdiff_t this_bytepos = (BYTEPOS);					\
  int changed = false;							\
									\
  if (this_bytepos == bytepos)						\
    {									\
      ptrdiff_t value = (CHARPOS);				       	\
									\
      byte_char_debug_check (b, value, bytepos);			\
      return value;							\
    }									\
  else if (this_bytepos > bytepos)					\
    {									\
      if (this_bytepos < best_above_byte)				\
	{								\
	  best_above = (CHARPOS);					\
	  best_above_byte = this_bytepos;				\
	  changed = true;						\
	}								\
    }									\
  else if (this_bytepos > best_below_byte)				\
    {									\
      best_below = (CHARPOS);						\
      best_below_byte = this_bytepos;					\
      changed = true;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
	{								\
	  ptrdiff_t value = best_below + (bytepos - best_below_byte);	\
									\
	  byte_char_debug_check (b, value, bytepos);			\
	  return value;							\
	}								\
    }									\
}

/* Return the character position corresponding to BYTEPOS in B.  */

ptrdiff_t
buf_bytepos_to_charpos (struct buffer *b, ptrdiff_t bytepos)
{
  ptrdiff_t best_above, best_above_byte;
  ptrdiff_t best_below, best_below_byte;
  ptrdiff_t distance = BYTECHAR_DISTANCE_INITIAL;

  eassert (BUF_BEG_BYTE (b) <= bytepos && bytepos <= BUF_Z_BYTE (b));

  best_above = BUF_Z (b);
  best_above_byte = BUF_Z_BYTE (b);

  /* If this buffer has as many characters as bytes,
     each character must be one byte.
     This takes care of the case where enable-multibyte-characters is nil.  */
  if (best_above == best_above_byte)
    return bytepos;

  /* Check bytepos is not in the middle of a character. */
  eassert (bytepos >= BUF_Z_BYTE (b)
           || CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)));

  best_below = BEG;
  best_below_byte = BEG_BYTE;

  CONSIDER (BUF_PT_BYTE (b), BUF_PT (b));
  CONSIDER (BUF_GPT_BYTE (b), BUF_GPT (b));
  CONSIDER (BUF_BEGV_BYTE (b), BUF_BEGV (b));
  CONSIDER (BUF_ZV_BYTE (b), BUF_ZV (b));

  if (b == cached_buffer && BUF_MODIFF (b) == cached_modiff)
    CONSIDER (cached_bytepos, cached_charpos);

  if (!(best_above_byte - bytepos < distance
        || bytepos - best_below_byte < distance))
    {
      struct Lisp_Markers *t = BUF_ALL_MARKERS (b);
      m_index_t i = markers_search_bytepos (t, bytepos);
      if (i < t->size)
	{
	  struct Lisp_Marker *m = t->markers[i];
	  CONSIDER (m->bytepos, m->charpos);
	}
      if (i == t->gap_end)
	i = t->gap_beg;
      if (i > 0)
	{
	  struct Lisp_Marker *m = t->markers[i - 1];
	  CONSIDER (m->bytepos, m->charpos);
	}
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (bytepos - best_below_byte < best_above_byte - bytepos)
    {
      bool record = bytepos - best_below_byte > 5000;

      while (best_below_byte < bytepos)
	{
	  best_below++;
	  best_below_byte += buf_next_char_len (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && BUF_ALL_MARKERS (b))
	cache_bytechar (b, best_below, best_below_byte);

      byte_char_debug_check (b, best_below, best_below_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      eassert (best_below <= bytepos);
      return best_below;
    }
  else
    {
      bool record = best_above_byte - bytepos > 5000;

      while (best_above_byte > bytepos)
	{
	  best_above--;
	  best_above_byte -= buf_prev_char_len (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && BUF_ALL_MARKERS (b))
	cache_bytechar (b, best_above, best_above_byte);

      byte_char_debug_check (b, best_above, best_above_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      eassert (best_above <= bytepos);
      return best_above;
    }
}

#undef CONSIDER

/* Operations on markers. */

DEFUN ("marker-buffer", Fmarker_buffer, Smarker_buffer, 1, 1, 0,
       doc: /* Return the buffer that MARKER points into, or nil if none.
Returns nil if MARKER points into a dead buffer.  */)
  (register Lisp_Object marker)
{
  register Lisp_Object buf;
  CHECK_MARKER (marker);
  if (XMARKER (marker)->buffer)
    {
      XSETBUFFER (buf, XMARKER (marker)->buffer);
      /* If the buffer is dead, we're in trouble: the buffer pointer here
	 does not preserve the buffer from being GC'd (it's weak), so
	 markers have to be unlinked from their buffer as soon as the buffer
	 is killed.  */
      eassert (BUFFER_LIVE_P (XBUFFER (buf)));
      return buf;
    }
  return Qnil;
}

DEFUN ("marker-position", Fmarker_position, Smarker_position, 1, 1, 0,
       doc: /* Return the position of MARKER, or nil if it points nowhere.  */)
  (Lisp_Object marker)
{
  CHECK_MARKER (marker);
  if (XMARKER (marker)->buffer)
    return make_fixnum (XMARKER (marker)->charpos);

  return Qnil;
}

DEFUN ("marker-last-position", Fmarker_last_position, Smarker_last_position, 1, 1, 0,
       doc: /* Return last position of MARKER in its buffer.
This is like `marker-position' with one exception:  If the buffer of
MARKER is dead, it returns the last position of MARKER in that buffer
before it was killed.  */)
  (Lisp_Object marker)
{
  CHECK_MARKER (marker);

  return make_fixnum (XMARKER (marker)->charpos);
}

/* Change M so it points to B at CHARPOS and BYTEPOS.  */

static void
attach_marker (struct Lisp_Marker *m, struct buffer *b,
	       ptrdiff_t charpos, ptrdiff_t bytepos)
{
  /* In a single-byte buffer, two positions must be equal.
     Otherwise, every character is at least one byte.  */
  if (BUF_Z (b) == BUF_Z_BYTE (b))
    eassert (charpos == bytepos);
  else
    eassert (charpos <= bytepos);

  if (m->buffer)
    markers_kill (BUF_ALL_MARKERS (m->buffer), m);

  m->charpos = charpos;
  m->bytepos = bytepos;
  m->buffer = b;

  BUF_ALL_MARKERS (b) = markers_add (BUF_ALL_MARKERS (b), m);
}

/* If BUFFER is nil, return current buffer pointer.  Next, check
   whether BUFFER is a buffer object and return buffer pointer
   corresponding to BUFFER if BUFFER is live, or NULL otherwise.  */

static struct buffer *
live_buffer (Lisp_Object buffer)
{
  struct buffer *b = decode_buffer (buffer);
  return BUFFER_LIVE_P (b) ? b : NULL;
}

/* Internal function to set MARKER in BUFFER at POSITION.  Non-zero
   RESTRICTED means limit the POSITION by the visible part of BUFFER.  */

static Lisp_Object
set_marker_internal (Lisp_Object marker, Lisp_Object position,
		     Lisp_Object buffer, bool restricted)
{
  struct Lisp_Marker *m;
  struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  /* Set MARKER to point nowhere if BUFFER is dead, or
     POSITION is nil or a marker points to nowhere.  */
  if (NILP (position)
      || (MARKERP (position) && !XMARKER (position)->buffer)
      || !b)
    unchain_marker (m);

  /* Optimize the special case where we are copying the position of
     an existing marker, and MARKER is already in the same buffer.  */
  else if (MARKERP (position) && b == XMARKER (position)->buffer)
    {
      attach_marker (m, b, XMARKER (position)->charpos,
		     XMARKER (position)->bytepos);
    }

  else
    {
      ptrdiff_t charpos = fix_position (position);
      charpos = clip_to_bounds
	(restricted ? BUF_BEGV (b) : BUF_BEG (b), charpos,
	 restricted ? BUF_ZV (b) : BUF_Z (b));
      ptrdiff_t bytepos = buf_charpos_to_bytepos (b, charpos);
      attach_marker (m, b, charpos, bytepos);
    }

#ifdef HAVE_TEXT_CONVERSION

  /* If B is the buffer's mark and there is a window displaying B, and
     text conversion is enabled while the mark is active, redisplay
     the buffer.

     propagate_window_redisplay will propagate this redisplay to the
     window, which will eventually reach
     mark_window_display_accurate_1.  At that point,
     report_point_change will be told to update the mark as seen by
     the input method.

     This is done all the way in (the seemingly irrelevant) redisplay
     because the selection reported to the input method is actually what
     is visible on screen, namely w->last_point.  */

  if (m->buffer
      && EQ (marker, BVAR (m->buffer, mark))
      && !NILP (BVAR (m->buffer, mark_active))
      && buffer_window_count (m->buffer))
    bset_redisplay (m->buffer);

#endif

  return marker;
}

DEFUN ("set-marker", Fset_marker, Sset_marker, 2, 3, 0,
       doc: /* Position MARKER before character number POSITION in BUFFER.
If BUFFER is omitted or nil, it defaults to the current buffer.  If
POSITION is nil, makes marker point nowhere so it no longer slows down
editing in any buffer.  Returns MARKER.  */)
  (Lisp_Object marker, Lisp_Object position, Lisp_Object buffer)
{
  return set_marker_internal (marker, position, buffer, false);
}

/* Like the above, but won't let the position be outside the visible part.  */

Lisp_Object
set_marker_restricted (Lisp_Object marker, Lisp_Object position,
		       Lisp_Object buffer)
{
  return set_marker_internal (marker, position, buffer, true);
}

/* Set the position of MARKER, specifying both the
   character position and the corresponding byte position.  */

Lisp_Object
set_marker_both (Lisp_Object marker, Lisp_Object buffer,
		 ptrdiff_t charpos, ptrdiff_t bytepos)
{
  register struct Lisp_Marker *m;
  register struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  if (b)
    attach_marker (m, b, charpos, bytepos);
  else
    unchain_marker (m);
  return marker;
}

/* Like the above, but won't let the position be outside the visible part.  */

Lisp_Object
set_marker_restricted_both (Lisp_Object marker, Lisp_Object buffer,
			    ptrdiff_t charpos, ptrdiff_t bytepos)
{
  register struct Lisp_Marker *m;
  register struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  if (b)
    {
      attach_marker
	(m, b,
	 clip_to_bounds (BUF_BEGV (b), charpos, BUF_ZV (b)),
	 clip_to_bounds (BUF_BEGV_BYTE (b), bytepos, BUF_ZV_BYTE (b)));
    }
  else
    unchain_marker (m);
  return marker;
}

/* Detach a marker so that it no longer points anywhere and no longer
   slows down editing.  Do not free the marker, though, as a change
   function could have inserted it into an undo list (Bug#30931).  */

void
detach_marker (Lisp_Object marker)
{
  /* FIXME: Roundabout way to call `unchain_marker`.  */
  Fset_marker (marker, Qnil, Qnil);
}

/* Remove MARKER from the chain of whatever buffer it is in.  Set its
   buffer NULL.  */

void
unchain_marker (register struct Lisp_Marker *marker)
{
  register struct buffer *b = marker->buffer;

  if (b)
    {
      /* No dead buffers here.  */
      eassert (BUFFER_LIVE_P (b));

      markers_kill (BUF_ALL_MARKERS (b), marker);
      marker->buffer = NULL;
    }
}

/* Return the char position of marker MARKER, as a C integer.  */

ptrdiff_t
marker_position (Lisp_Object marker)
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;

  if (!buf)
    error ("Marker does not point anywhere");

  eassert (BUF_BEG (buf) <= m->charpos && m->charpos <= BUF_Z (buf));

  return m->charpos;
}

/* Return the byte position of marker MARKER, as a C integer.  */

ptrdiff_t
marker_byte_position (Lisp_Object marker)
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;

  if (!buf)
    error ("Marker does not point anywhere");

  eassert (BUF_BEG_BYTE (buf) <= m->bytepos && m->bytepos <= BUF_Z_BYTE (buf));

  return m->bytepos;
}

DEFUN ("copy-marker", Fcopy_marker, Scopy_marker, 0, 2, 0,
       doc: /* Return a new marker pointing at the same place as MARKER.
If argument is a number, makes a new marker pointing
at that position in the current buffer.
If MARKER is not specified, the new marker does not point anywhere.
The optional argument TYPE specifies the insertion type of the new marker;
see `marker-insertion-type'.  */)
  (register Lisp_Object marker, Lisp_Object type)
{
  register Lisp_Object new;

  if (!NILP (marker))
  CHECK_TYPE (FIXNUMP (marker) || MARKERP (marker), Qinteger_or_marker_p, marker);

  new = Fmake_marker ();
  Fset_marker (new, marker,
	       (MARKERP (marker) ? Fmarker_buffer (marker) : Qnil));
  XMARKER (new)->insertion_type = !NILP (type);
  return new;
}

DEFUN ("marker-insertion-type", Fmarker_insertion_type,
       Smarker_insertion_type, 1, 1, 0,
       doc: /* Return insertion type of MARKER: t if it stays after inserted text.
The value nil means the marker stays before text inserted there.  */)
  (register Lisp_Object marker)
{
  CHECK_MARKER (marker);
  return XMARKER (marker)->insertion_type ? Qt : Qnil;
}

DEFUN ("set-marker-insertion-type", Fset_marker_insertion_type,
       Sset_marker_insertion_type, 2, 2, 0,
       doc: /* Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it.  */)
  (Lisp_Object marker, Lisp_Object type)
{
  CHECK_MARKER (marker);

  XMARKER (marker)->insertion_type = ! NILP (type);
  return type;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
  (void)
{
  struct Lisp_Marker *p = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  p->buffer = 0;
  p->bytepos = 0;
  p->charpos = 0;
  p->insertion_type = false;
  p->need_adjustment = false;
  p->cache = false;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

/* Return a newly allocated marker which points into BUF
   at character position CHARPOS and byte position BYTEPOS.  */

Lisp_Object
build_marker (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  /* No dead buffers here.  */
  eassert (BUFFER_LIVE_P (buf));

  /* Every character is at least one byte.  */
  eassert (charpos <= bytepos);

  struct Lisp_Marker *m = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  m->buffer = buf;
  m->charpos = charpos;
  m->bytepos = bytepos;
  m->insertion_type = false;
  m->need_adjustment = false;
  m->cache = false;
  BUF_ALL_MARKERS (buf) = markers_add (BUF_ALL_MARKERS (buf), m);
  return make_lisp_ptr (m, Lisp_Vectorlike);
}

#ifdef MARKER_DEBUG

/* For debugging -- count the markers in buffer BUF.  */

int
count_markers (struct buffer *buf)
{
  struct Lisp_Markers *t = BUF_ALL_MARKERS (current_buffer);
  return t->size - (t->gap_end - t->gap_beg);
}

/* For debugging -- recompute the bytepos corresponding
   to CHARPOS in the simplest, most reliable way.  */

ptrdiff_t
verify_bytepos (ptrdiff_t charpos)
{
  ptrdiff_t below = BEG;
  ptrdiff_t below_byte = BEG_BYTE;

  while (below != charpos)
    {
      below++;
      below_byte += buf_next_char_len (current_buffer, below_byte);
    }

  return below_byte;
}

#endif /* MARKER_DEBUG */

void
syms_of_marker (void)
{
  defsubr (&Smake_marker);
  defsubr (&Smarker_position);
  defsubr (&Smarker_last_position);
  defsubr (&Smarker_buffer);
  defsubr (&Sset_marker);
  defsubr (&Scopy_marker);
  defsubr (&Smarker_insertion_type);
  defsubr (&Sset_marker_insertion_type);
}
