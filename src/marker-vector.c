/* Marker vectors.
   Copyright (C) 2025 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

/* A marker vector is used to hold the markers of a buffer.  The vector
   is a normal Lisp vector that consists of a header and a number of
   entries for each marker.  A Lisp vector is used because the vector
   references markers "weakly", and that's what easy for igc.

   +------+-----------+---------+---------+--------------+
   | FREE | MAX_ENTRY | entry 0 | entry 1 | ...          |
   +------+-----------+---------+---------+--------------+
   |<----- header --->|

   Entries consist of 2 vector slots MARKER and CHARPOS. MARKER holds a
   marker, if the entry is in use.  CHARPOS is not yet used.  (The idea is
   to move the positions from Lisp_Marker here, which speeds up
   adjusting positions when the text changes.)

   FREE is the array index of the start of the next free entry in the
   marker vector.  Free entries form a singly-linked list using the
   MARKER field of entries.

   MAX_ENTRY is the largest index ever used to store a marker.  This is
   used to (supposedly) speed up iteration over the marker vector, with
   the assumption that there might be a tail of slots in the marker
   vector that is never used.  Or, IOW, that we over-allocate room in the
   marker vector.

   Lisp_Marker objects contain the index under which they are stored in
   the marker vector.

   The use of a free list gives O(1) for adding a marker.  The index
   stored in the Lisp_Marker provides O(1) deletion of a marker from
   the markers of a buffer.

   Iteration over marker vectors is done by iterating over all slots of
   the vector that can contain markers, skipping those that don't.

   Iteration over markers is O(N) where N is the size of the marker
   vector.  This could be improved to N being the number of live markers
   by putting marker entries in a doubly-linked list.  The downside is
   that iteration then might access the marker vector slots in an
   unpredictable order, while the current method scans the vector
   sequentially which should be fast.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "text-index.h"
#include "marker-vector.h"
#ifdef HAVE_MPS
#include "igc.h"
#endif

/* Number of entries to allocate initially.  */
#define MARKER_VECTOR_INITIAL_ENTRIES 20

/* Access fields of an entry E of marker vector V as lvalues.  */
#define MARKER(v, e) (v)->contents[(e) + MARKER_VECTOR_OFFSET_MARKER]
#define CHARPOS(v, e) (v)->contents[(e) + MARKER_VECTOR_OFFSET_CHARPOS]

/* Index of next free entry in the header of a marker vector.  This must
   use the marker field so that putting an entry on the free list
   implicitly sets the marker slot to a non-marker.  See
   fix_marker_vector in igc.c.  */
#define NEXT_FREE(v, e) MARKER (v, e)

/* Value denoting end of the free list.  */
#define FREE_LIST_END make_fixnum (0)
#define IS_FREE_LIST_END(x) EQ ((x), FREE_LIST_END)

/* Access header fields of marker vector V as lvalues.  */
#define FREE(v) (v)->contents[MARKER_VECTOR_FREE]
#define MAX_ENTRY(v) (v)->contents[MARKER_VECTOR_MAX_ENTRY]

/* Check that index ENTRY is a valid entry start index in vector V.  */

static void
check_is_entry (const struct Lisp_Vector *v, ptrdiff_t entry)
{
  eassert (entry >= MARKER_VECTOR_HEADER_SIZE);
  eassert (entry < gc_vsize (v));
  eassert ((entry - MARKER_VECTOR_HEADER_SIZE) % MARKER_VECTOR_ENTRY_SIZE == 0);
}

/* Push entry ENTRY of V to its free-list. This must set MARKER to not
   be a marker, which is done by using the MARKER field of entry
   to form the free-list.  */

static void
push_free (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  check_is_entry (v, entry);
  NEXT_FREE (v, entry) = FREE (v);
  FREE (v) = make_fixnum (entry);
}

/* Pop the next free entry from the free-list of V and return its entry
   start index.  */

static ptrdiff_t
pop_free (struct Lisp_Vector *v)
{
  const ptrdiff_t free = XFIXNUM (FREE (v));
  FREE (v) = NEXT_FREE (v, free);
  check_is_entry (v, free);
  return free;
}

/* Add a new entry for marker M to marker vector MV and return its entry
   start index.  */

static ptrdiff_t
add_entry (Lisp_Object mv, struct Lisp_Marker *m)
{
  struct Lisp_Vector *v = XVECTOR (mv);
  const ptrdiff_t entry = pop_free (v);
  MARKER (v, entry) = make_lisp_ptr (m, Lisp_Vectorlike);
  const ptrdiff_t max_entry = XFIXNUM (MAX_ENTRY (v));
  MAX_ENTRY (v) = make_fixnum (max (entry, max_entry));
  return entry;
}

/* Allocate a marker vector of length LEN.  */

Lisp_Object
alloc_marker_vector (ptrdiff_t len)
{
#ifdef HAVE_MPS
  return igc_alloc_marker_vector (len, FREE_LIST_END);
#else
  return make_vector (len, FREE_LIST_END);
#endif
}

/* Expensive pre- and post-condition checking. V is the marker vector to
   check.  ALLOCATING true means we are called from allocation functions
   where V may be different from the underlying buffer's marker
   vector.  */

static void
check_marker_vector (struct Lisp_Vector *v, bool allocating)
{
#ifdef ENABLE_CHECKING
  size_t nfree = 0;
  for (Lisp_Object e = FREE (v); !IS_FREE_LIST_END (e);
       e = NEXT_FREE (v, XFIXNUM (e)))
    ++nfree;

  size_t nused = 0;
  Lisp_Object mv = make_lisp_ptr (v, Lisp_Vectorlike);
  DO_MARKERS_OF_VECTOR (mv, m)
    {
      eassert (m->entry == e_);
      eassert (m->buffer != NULL);
      if (!allocating)
	{
	  struct Lisp_Vector *mv = XVECTOR (BUF_MARKERS (m->buffer));
	  eassert (mv == v);
	}
      ++nused;
    }
  END_DO_MARKERS;

  eassert ((nused + nfree) * MARKER_VECTOR_ENTRY_SIZE
	   + MARKER_VECTOR_HEADER_SIZE == gc_vsize (v));
#endif
}

/* Add all entries of MV starting with FIRST to the end of marker vector MV
   to its free list.  */

static void
add_to_free_list (Lisp_Object mv, ptrdiff_t first)
{
  struct Lisp_Vector *v = XVECTOR (mv);
  for (ptrdiff_t e = ASIZE (mv) - MARKER_VECTOR_ENTRY_SIZE;
       e >= first; e -= MARKER_VECTOR_ENTRY_SIZE)
    push_free (v, e);
}

/* Make a new marker vector.  */

Lisp_Object
make_marker_vector (void)
{
  const ptrdiff_t len
    = (MARKER_VECTOR_INITIAL_ENTRIES * MARKER_VECTOR_ENTRY_SIZE
       + MARKER_VECTOR_HEADER_SIZE);
  Lisp_Object mv = alloc_marker_vector (len);
  add_to_free_list (mv, MARKER_VECTOR_HEADER_SIZE);
  check_marker_vector (XVECTOR (mv), true);
  return mv;
}

/* Return a new marker vector that is like OLD_MV but larger.  */

static Lisp_Object
larger_marker_vector (Lisp_Object old_mv)
{
  const ptrdiff_t old_size = ASIZE (old_mv);
  const ptrdiff_t old_entries_size = old_size - MARKER_VECTOR_HEADER_SIZE;
  const ptrdiff_t new_size = 2 * old_entries_size + MARKER_VECTOR_HEADER_SIZE;

  /* Allocate a new marker vector.  */
  Lisp_Object new_mv = alloc_marker_vector (new_size);
  struct Lisp_Vector *new_v = XVECTOR (new_mv);

  /* Copy existing entries. */
  const struct Lisp_Vector *old_v = XVECTOR (old_mv);
  const size_t nbytes = old_size * sizeof (Lisp_Object);
  memcpy (new_v->contents, old_v->contents, nbytes);

  /* Add new entries to free-list.  */
  add_to_free_list (new_mv, old_size);
  check_marker_vector (new_v, true);
  return new_mv;
}

/* Make sure that the marker vector of buffer B has room for a new
   entry.  Make a larger marker vector if not.  Value is the marker
   vector of B at the end.  */

static Lisp_Object
ensure_room (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (IS_FREE_LIST_END (FREE (XVECTOR (mv))))
    {
      mv = larger_marker_vector (mv);
      BUF_MARKERS (b) = mv;
    }
  return mv;
}

/* Add marker M to the marker vector of buffer B.  */

void
marker_vector_add (struct buffer *b, struct Lisp_Marker *m)
{
  const Lisp_Object mv = ensure_room (b);
  check_marker_vector (XVECTOR (mv), false);
  m->buffer = b;
  m->entry = add_entry (mv, m);
  check_marker_vector (XVECTOR (mv), false);
}

/* Remove marker M from marker vector V.  */

void
marker_vector_remove (struct Lisp_Vector *v, struct Lisp_Marker *m)
{
  check_marker_vector (v, false);
  check_is_entry (v, m->entry);
  eassert (MARKERP (MARKER (v, m->entry)));
  eassert (XMARKER (MARKER (v, m->entry)) == m);
  push_free (v, m->entry);
  /* The old GC contains at least one assertion that unchaining markers
     in kill-buffer resets the markers' buffers.  IGC does not do this,
     can't do this, and does not need it.  */
  m->buffer = NULL;
  m->entry = - XFIXNUM (CHARPOS (v, m->entry));
  check_marker_vector (v, false);
}

/* Reset markers of buffer B.  Called from kill-buffer.  */

void
marker_vector_reset (struct buffer *b)
{
  /* The old GC contains at least one assertion that unchaining markers
     in kill-buffer resets the markers' buffers.  IGC does not do this,
     can't do this, and does not need it.  */
  DO_MARKERS (b, m)
    {
      m->buffer = NULL;
    }
  END_DO_MARKERS;
  BUF_MARKERS (b) = Qnil;
}

/* Set marker M's character position to CHARPOS.  */

void
marker_vector_set_charpos (struct Lisp_Marker *m, ptrdiff_t charpos)
{
  eassert (m->buffer);
  struct Lisp_Vector *v = XVECTOR (BUF_MARKERS (m->buffer));
  check_is_entry (v, m->entry);
  CHARPOS (v, m->entry) = make_fixnum (charpos);
}

/* Return marker M's character position.  */

ptrdiff_t
marker_vector_charpos (const struct Lisp_Marker *m)
{
  eassert (m->buffer);
  struct Lisp_Vector *v = XVECTOR (BUF_MARKERS (m->buffer));
  check_is_entry (v, m->entry);
  return XFIXNUM (CHARPOS (v, m->entry));
}

/* Return marker M's last character position.  */

ptrdiff_t
marker_vector_last_charpos (const struct Lisp_Marker *m)
{
  eassert (m->buffer == NULL);
  eassert (m->entry < 0);
  return - m->entry;
}

/* Return marker M's byte position.  */

ptrdiff_t
marker_vector_bytepos (const struct Lisp_Marker *m)
{
  const ptrdiff_t charpos = marker_vector_charpos (m);
  return buf_charpos_to_bytepos (m->buffer, charpos);
}

/* Adjust marker positions in buffer B for an insertion that stretches
   from FROM_CHARPOS to TO_CHARPOS.  When a marker points at the
   insertion point FROM_CHARPOS, we advance it if either its
   insertion-type is t or BEFORE_MARKERS is true.  */

void
marker_vector_adjust_for_insert (struct buffer *b,
				 const ptrdiff_t from_charpos,
				 const ptrdiff_t to_charpos,
				 const bool before_markers)
{
  const ptrdiff_t nchars = to_charpos - from_charpos;
  struct Lisp_Vector *v = XVECTOR (BUF_MARKERS (b));
  DO_MARKERS (b, m)
    {
      const ptrdiff_t charpos = XFIXNUM (CHARPOS (v, m->entry));
      if (charpos == from_charpos)
	{
	  if (m->insertion_type || before_markers)
	    CHARPOS (v, m->entry) = make_fixnum (to_charpos);
	}
      else if (charpos > from_charpos)
	CHARPOS (v, m->entry) = make_fixnum (charpos + nchars);
    }
  END_DO_MARKERS;
}

/* Adjust marker positions of buffer Bs for a replacement of text at
   FROM_CHARPOS of length OLD_NCHARS to a new text of length NEW_NCHARS.
   It is assumed that OLD_CHARS > 0, i.e., this is not an insertion.  */

void
marker_vector_adjust_for_replace (struct buffer *b,
				  const ptrdiff_t from_charpos,
				  const ptrdiff_t old_nchars,
				  const ptrdiff_t new_nchars)
{
  const ptrdiff_t diff_nchars = new_nchars - old_nchars;
  const ptrdiff_t old_to_charpos = from_charpos + old_nchars;
  struct Lisp_Vector *v = XVECTOR (BUF_MARKERS (b));
  DO_MARKERS (b, m)
    {
      const ptrdiff_t charpos = XFIXNUM (CHARPOS (v, m->entry));
      if (charpos >= old_to_charpos)
	CHARPOS (v, m->entry) = make_fixnum (charpos + diff_nchars);
      else if (charpos > from_charpos)
	CHARPOS (v, m->entry) = make_fixnum (from_charpos);
    }
  END_DO_MARKERS;
}
