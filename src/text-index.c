/* Text index for character positions.
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

/* A text index is used to map character positions in buffer text to
   byte positions and vice versa.  (One could also think of using it for
   other things like line numbers, but that is currently not done.)

   The index divides buffer text into intervals of constant size =
   number of bytes.

   BEG_BYTE                                        Z_BYTE
   |-------------------------------------------------|
   | interval | interval | interval | interval |          |
   0          1          2          3          4          -
		   index in character position array

   The index consists of an array of character positions.  The entry at
   index ENTRY is the character position of the character containing the
   byte position ENTRY * interval size.  There is no entry for a partial
   interval at the end of the text.  The position (Z, Z_BYTE) is instead
   handled specially in the code.

   Note that the byte positions at interval boundaries can be in the
   middle of a multi-byte character.

	     character start byte position
		|
	  ------01234-------- bytes of a character (up to 5 in Emacs'
		   |          internal encoding)
	       N * interval

   To find the character position corresponding to a byte position
   BYTEPOS, we look up the character position in the index at BYTEPOS /
   interval.  From there, we can scan forward in the text until we reach
   BYTEPOS, counting characters, or we scan backward from the interval
   end, if that is closer.

   To find the byte position BYTEPOS corresponding to a given character
   position CHARPOS, we search the index for the last entry ENTRY whose
   character position is <= CHARPOS.  That entry corresponds to a byte
   position ENTRY * interval size.  From there, we scan the text until
   we reach BYTEPOS, counting characters until we reach CHARPOS.  The
   byte position reached at the end is BYTEPOS.  We also scan backward
   from the interval end, if that looks advantageous.

   Why divide the text into intervals of bytes instead of characters?
   Dividing the text into intervals of characters makes scanning
   overhead less uniform, since characters can be of different lengths
   (1 to 5 bytes).  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "dispextern.h"		/* for struct text_pos */
#include "text-index.h"

// clang-format off

struct text_index
{
  /* Value at index IDX is the character position of byte position IDX *
     INTERVAL.  Note that that byte position may be in the middle of a
     character.  The value at index 0 is BEG.  */
  ptrdiff_t *charpos;

  /* Number of valid entries in the above array.  This is always at least 1
     because the first entry is for BEG.  */
  size_t nentries;

  /* Number of entries allocated.  */
  size_t capacity;

  /* Known position cache.  This is the last position conversion result.  */
  struct text_pos cache;
};

enum
{
  /* Number of bytes in an interval.  */
  TEXT_INDEX_INTERVAL = 4096,

  /* Default capacity in number of intervals for text indices.  */
  TEXT_INDEX_DEFAULT_CAPACITY = 20,

  /* Value indicating a non-position.  */
  TEXT_INDEX_INVALID_POSITION = -1
};

/* Get PT, GPT, Z as text_pos structures..  Use these instead of BUF_PT,
   and so to make sure to never try to get positions from markers, which
   could lead to infinite recursion.  */

static struct text_pos
z_pos (const struct buffer *b)
{
  return (struct text_pos)
    {.charpos = b->text->z, .bytepos = b->text->z_byte};
}

static struct text_pos
gpt_pos (const struct buffer *b)
{
  return (struct text_pos)
    {.charpos = b->text->gpt, .bytepos = b->text->gpt_byte};
}

static struct text_pos
pt_pos (const struct buffer *b)
{
  return (struct text_pos)
    {.charpos = b->pt, .bytepos = b->pt_byte};
}

/* Cache (CHARPOS, BYTEPOS) as known position in index TI.  */

static void
cache (struct text_index *ti, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  ti->cache.charpos = charpos;
  ti->cache.bytepos = bytepos;
}

/* Invalidate known position cache of TI.  */

static void
invalidate_cache (struct text_index *ti)
{
  ti->cache.charpos = TEXT_INDEX_INVALID_POSITION;
  ti->cache.bytepos = TEXT_INDEX_INVALID_POSITION;
}

/* Value is true is known position cache of TI is valid.  */

static bool
is_cache_valid (const struct text_index *ti)
{
  return ti->cache.bytepos != TEXT_INDEX_INVALID_POSITION;
}

/* Return the byte position in index TI corresponding to index entry
   ENTRY.  Note that this position cab be in the middle of a multi-byte
   character.  */

static ptrdiff_t
index_bytepos (const struct text_index *ti, ptrdiff_t entry)
{
  return BEG_BYTE + entry * TEXT_INDEX_INTERVAL;
}

/* Return the character position in index TI corresponding index entry
   ENTRY.  */

static ptrdiff_t
index_charpos (const struct text_index *ti, ptrdiff_t entry)
{
  eassert (entry >= 0 && entry < ti->nentries);
  return ti->charpos[entry];
}

/* Return the index entry for BYTEPOS in index TI.  */

static ptrdiff_t
index_bytepos_entry (const struct text_index *ti, ptrdiff_t bytepos)
{
  return (bytepos - BEG_BYTE) / TEXT_INDEX_INTERVAL;
}

/* Return the entry of index TI for the largest character position <=
   CHARPOS.  */

static ptrdiff_t
index_charpos_entry (const struct text_index *ti, ptrdiff_t charpos)
{
  ptrdiff_t entry = -1;
  for (ptrdiff_t low = 0, high = ti->nentries - 1; low <= high;)
    {
      ptrdiff_t mid = low + (high - low) / 2;
      if (ti->charpos[mid] <= charpos)
	{
	  entry = mid;
	  low = mid + 1;
	}
      else
	high = mid - 1;
    }
  eassert (entry >= 0 && entry < ti->nentries);
  return entry;
}

/* Return TI's index entry ENTRY as a struct text_pos.  */

static struct text_pos
index_text_pos (const struct text_index *ti, ptrdiff_t entry)
{
  eassert (entry >= 0 && entry < ti->nentries);
  return (struct text_pos) {
    .charpos = index_charpos (ti, entry),
    .bytepos = index_bytepos (ti, entry)
  };
}

/* Return index TI's maximum indexed character position.  */

static ptrdiff_t
max_indexed_charpos (const struct text_index *ti)
{
  return index_charpos (ti, ti->nentries - 1);
}

/* Return index TI's maximum indexed byte position.  */

static ptrdiff_t
max_indexed_bytepos (const struct text_index *ti)
{
  return index_bytepos (ti, ti->nentries - 1);
}

/* Given a byte position BYTEPOS in buffer B, return the byte position
   where the character starts that contains BYTEPOS: */

static ptrdiff_t
char_start_bytepos (struct buffer *b, ptrdiff_t bytepos)
{
  while (!CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
    --bytepos;
  return bytepos;
}

/* Allocate and return a text index structure with enough room for a
   text of length NBYTES bytes.  */

static struct text_index *
make_text_index (size_t nbytes)
{
  struct text_index *ti = xzalloc (sizeof *ti);
  ti->capacity = TEXT_INDEX_DEFAULT_CAPACITY;
  ti->charpos = xnmalloc (ti->capacity, sizeof *ti->charpos);
  ti->charpos[0] = BEG;
  ti->nentries = 1;
  invalidate_cache (ti);
  return ti;
}

/* Free the text index TI.  TI may be NULL.  */

void
text_index_free (struct text_index *ti)
{
  if (ti == NULL)
    return;
  xfree (ti->charpos);
  xfree (ti);
}

/* Append entry for CHARPOS to index TI.  */

static void
append_entry (struct text_index *ti, ptrdiff_t charpos)
{
  if (ti->nentries == ti->capacity)
    {
      eassert (ti->capacity > 0);
      ti->capacity = 2 * ti->capacity;
      ti->charpos = xnrealloc (ti->charpos, ti->capacity,
			       sizeof *ti->charpos);
    }
  ti->charpos[ti->nentries] = charpos;
  ++ti->nentries;
}

/* Build text index of buffer B up to and including position TO.
   One of TO.charpos or TO.bytepos must be non-zero.  */

static void
build_index (struct buffer *b, const struct text_pos to)
{
  struct text_index *ti = b->text->index;

  eassert (to.charpos != TEXT_INDEX_INVALID_POSITION
	   || to.bytepos != TEXT_INDEX_INVALID_POSITION);
  eassert (to.charpos == TEXT_INDEX_INVALID_POSITION
	   || to.bytepos == TEXT_INDEX_INVALID_POSITION);
  eassert (to.bytepos == TEXT_INDEX_INVALID_POSITION
	   || (to.bytepos >= BEG_BYTE
	       && to.bytepos <= z_pos (b).bytepos));
  eassert (to.bytepos == TEXT_INDEX_INVALID_POSITION
	   || to.bytepos > max_indexed_bytepos (ti));
  eassert (to.charpos == TEXT_INDEX_INVALID_POSITION
	   || (to.charpos >= BEG && to.charpos <= z_pos (b).charpos));
  eassert (to.charpos == TEXT_INDEX_INVALID_POSITION
	   || to.charpos > max_indexed_charpos (ti));

  /* Start at the byte position of the last index entry.  if TO_BYTEPOS
     equals the byte position of that entry, this is okay, because the
     character position at that byte position cannot have changed.  */
  const ptrdiff_t last_entry = ti->nentries - 1;
  ptrdiff_t charpos = index_charpos (ti, last_entry);
  ptrdiff_t bytepos = index_bytepos (ti, last_entry);
  ptrdiff_t next_stop = bytepos + TEXT_INDEX_INTERVAL;

  /* Quickly give up if there are not enough bytes left to scan to make
     a new index entry.  */
  const ptrdiff_t z_byte = z_pos (b).bytepos;
  if (next_stop >= z_byte)
    return;

  /* Loop over bytes, starting one after the index entry we start from
     because we are only interested in yet unknown entries, and the
     one at EMTRY can be assumed to stay unchanged.  */
  for (++bytepos; bytepos < z_byte; ++bytepos)
    {
      if (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
	++charpos;

      if (bytepos == next_stop)
	{
	  /* Add a new index entry.  */
	  append_entry (ti, charpos);

	  /* If we reached the one after the position we are interested
	     in, we're done since we can then scan forward and backward
	     to BYTEPOS.  */
	  if ((to.bytepos != TEXT_INDEX_INVALID_POSITION
	       && bytepos > to.bytepos)
	      || (to.charpos != TEXT_INDEX_INVALID_POSITION
		  && charpos > to.charpos))
	    break;

	  /* Compute next stop. We are done if no next entry
	     can be built.  */
	  next_stop += TEXT_INDEX_INTERVAL;
	  if (next_stop >= z_byte)
	    break;
	}
    }
}

/* Make sure that buffer B has a text index.  */

static struct text_index *
ensure_has_index (struct buffer *b)
{
  if (b->text->index == NULL)
    b->text->index = make_text_index (z_pos (b).bytepos);
  return b->text->index;
}

/* Make sure that buffer B's text index contains BYTEPOS.  */

static void
ensure_bytepos_indexed (struct buffer *b, ptrdiff_t bytepos)
{
  struct text_index *ti = ensure_has_index (b);
  if (bytepos > max_indexed_bytepos (ti))
    {
      struct text_pos to
	= {.charpos = TEXT_INDEX_INVALID_POSITION, .bytepos = bytepos};
      build_index (b, to);
    }
}

/* Make sure that buffer B's text index contains CHARPOS.  */

static void
ensure_charpos_indexed (struct buffer *b, ptrdiff_t charpos)
{
  struct text_index *ti = ensure_has_index (b);
  if (charpos > max_indexed_charpos (ti))
    {
      struct text_pos to
	= {.charpos = charpos, .bytepos = TEXT_INDEX_INVALID_POSITION};
      build_index (b, to);
    }
}

/* In buffer B, starting from index entry ENTRY, scan forward in B's
   text to TO_BYTEPOS, and return the corresponding character
   position.  */

static ptrdiff_t
charpos_forward_to_bytepos (struct buffer *b, const struct text_pos from,
			    const ptrdiff_t to_bytepos)
{
  eassert (from.bytepos <= to_bytepos);
  ptrdiff_t bytepos = from.bytepos;
  ptrdiff_t charpos = from.charpos;
  while (bytepos < to_bytepos)
    {
      ++bytepos;
      if (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
	++charpos;
    }
  return charpos;
}

/* In buffer B, starting from FROM, scan backward in B's text to
   TO_BYTEPOS, and return the corresponding character position.  */

static ptrdiff_t
charpos_backward_to_bytepos (struct buffer *b, const struct text_pos from,
			     const ptrdiff_t to_bytepos)
{
  eassert (from.bytepos >= to_bytepos);
  ptrdiff_t bytepos = char_start_bytepos (b, from.bytepos);
  ptrdiff_t charpos = from.charpos;
  while (bytepos > to_bytepos)
    {
      --bytepos;
      if (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
	--charpos;
    }
  return charpos;
}

/* In buffer B, starting from FROM, scan forward in B's text to
   TO_CHARPOS, and return the corresponding byte position.  The byte
   position is the one of the character start.  FROM's charpos
   must be < TO_CHARPOS.  */

static ptrdiff_t
bytepos_forward_to_charpos (struct buffer *b, const struct text_pos from,
			    ptrdiff_t to_charpos)
{
  eassert (from.charpos < to_charpos);
  ptrdiff_t bytepos = from.bytepos;
  ptrdiff_t charpos = from.charpos;
  while (charpos < to_charpos)
    {
      ++bytepos;
      if (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
	++charpos;
    }
  eassert (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)));
  return bytepos;
}

/* In buffer B, starting from FROM, scan backward in B's text to
   TO_CHARPOS, and return the corresponding byte position.  The byte
   position is the one of the character start.  FROM's charpos must be
   >= TO_CHARPOS.  */

static ptrdiff_t
bytepos_backward_to_charpos (struct buffer *b, const struct text_pos from,
			     const ptrdiff_t to_charpos)
{
  eassert (from.charpos >= to_charpos);
  ptrdiff_t bytepos = char_start_bytepos (b, from.bytepos);
  ptrdiff_t charpos = from.charpos;
  while (charpos > to_charpos)
    {
      --bytepos;
      if (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
	--charpos;
    }
  eassert (CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)));
  return bytepos;
}

/* Return the next known (char, byte) position in buffer B after the one
   in index entry ENTRY.  */

static struct text_pos
next_known_text_pos (struct buffer *b, ptrdiff_t entry)
{
  const struct text_index *ti = b->text->index;
  if (entry + 1 < ti->nentries)
    return index_text_pos (ti, entry + 1);
  return z_pos (b);
}

/* Improve the known bytepos bounds *PREV and *NEXT if KNOWN is closer
   to BYTEPOS.  If KNOWN is an exact match for BYTEPOS return true.  */

static bool
narrow_bytepos_bounds_1 (const struct text_pos known, struct text_pos *prev,
			 struct text_pos *next, const ptrdiff_t bytepos)
{
  eassert (bytepos >= prev->bytepos && bytepos <= next->bytepos);
  eassert (known.bytepos != TEXT_INDEX_INVALID_POSITION);
  if (known.bytepos == bytepos)
    return true;

  /* If KNOWN is in (PREV, BYTEPOS] it is a better PREV. */
  if (known.bytepos < bytepos
      && known.bytepos > prev->bytepos)
    *prev = known;

  /* If KNOWN is in [BYTEPOS NEXT) it is a better NEXT. */
  if (known.bytepos > bytepos
      && known.bytepos < next->bytepos)
    *next = known;

  return false;
}

/* Improve the known bytepos bounds *PREV and *NEXT of buffer B using
   known positions in B.  BYTEPOS is a byte position to convert to a
   character position.  If an exact match for BYTEPOS is found, return
   its charpos, otherwise return TEXT_INDEX_INVALID_POSITION.  */

static ptrdiff_t
narrow_bytepos_bounds (struct buffer *b, struct text_pos *prev,
		       struct text_pos *next, const ptrdiff_t bytepos)
{
  const struct text_pos pt = pt_pos (b);
  if (narrow_bytepos_bounds_1 (pt, prev, next, bytepos))
    return pt.charpos;

  const struct text_pos gpt = gpt_pos (b);
  if (narrow_bytepos_bounds_1 (gpt, prev, next, bytepos))
    return gpt.charpos;

  struct text_index *ti = b->text->index;
  if (is_cache_valid (ti)
      && narrow_bytepos_bounds_1 (ti->cache, prev, next, bytepos))
    return ti->cache.charpos;

  return TEXT_INDEX_INVALID_POSITION;
}

/* Improve the known bytepos bounds *PREV and *NEXT if KNOWN is closer
   to BYTEPOS.  If KNOWN is an exact match for BYTEPOS return true.  */

static bool
narrow_charpos_bounds_1 (const struct text_pos known, struct text_pos *prev,
			 struct text_pos *next, const ptrdiff_t charpos)
{
  eassert (charpos >= prev->charpos && charpos <= next->charpos);
  eassert (known.charpos != TEXT_INDEX_INVALID_POSITION);
  if (known.charpos == charpos)
    return true;

  /* If KNOWN is in (PREV, BYTEPOS] it is a better PREV. */
  if (known.charpos < charpos
      && known.charpos > prev->charpos)
    *prev = known;

  /* If KNOWN is in [BYTEPOS NEXT) it is a better NEXT. */
  if (known.charpos > charpos
      && known.charpos < next->charpos)
    *next = known;

  return false;
}

/* Improve the known bytepos bounds *PREV and *NEXT of buffer B using
   known positions in B.  BYTEPOS is a byte position to convert to a
   character position.  If an exact match for BYTEPOS is found, return
   its charpos, otherwise return TEXT_INDEX_INVALID_POSITION.  */

static ptrdiff_t
narrow_charpos_bounds (struct buffer *b, struct text_pos *prev,
		       struct text_pos *next, const ptrdiff_t charpos)
{
  const struct text_pos pt = pt_pos (b);
  if (narrow_charpos_bounds_1 (pt, prev, next, charpos))
    return pt.bytepos;

  const struct text_pos gpt = gpt_pos (b);
  if (narrow_charpos_bounds_1 (gpt, prev, next, charpos))
    return gpt.bytepos;

  struct text_index *ti = b->text->index;
  if (is_cache_valid (ti)
      && narrow_charpos_bounds_1 (ti->cache, prev, next, charpos))
    return ti->cache.bytepos;

  return TEXT_INDEX_INVALID_POSITION;
}

/* Return the character position in buffer B corresponding to
   byte position BYTEPOS.  */

ptrdiff_t
buf_bytepos_to_charpos (struct buffer *b, const ptrdiff_t bytepos)
{
  /* If this buffer has as many characters as bytes, each character must
     be one byte.  This takes care of the case where
     enable-multibyte-characters is nil.  */
  const struct text_pos z = z_pos (b);
  if (z.charpos == z.bytepos)
    return bytepos;

  /* BYTEPOS == Z_BYTE, and BYTEPOS is an interval boundary,
     then BYTEPOS does not have an index entry because we don't want
     extra entries for (Z, Z_BYTE).  Changing that would be possible
     but leads to more code than this if-statement, so it's probably
     not worth it.  */
  if (bytepos == z.bytepos)
    return z.charpos;
  ensure_bytepos_indexed (b, bytepos);

  struct text_index *ti = b->text->index;
  const ptrdiff_t entry = index_bytepos_entry (ti, bytepos);
  struct text_pos prev = index_text_pos (ti, entry);
  struct text_pos next = next_known_text_pos (b, entry);

  ptrdiff_t charpos = narrow_bytepos_bounds (b, &prev, &next, bytepos);
  if (charpos != TEXT_INDEX_INVALID_POSITION)
    return charpos;

  /* Scan forward if the distance to the previous known position is
     smaller than the distance to the next known position.  */
  if (bytepos - prev.bytepos < next.bytepos - bytepos)
    charpos = charpos_forward_to_bytepos (b, prev, bytepos);
  else
    charpos = charpos_backward_to_bytepos (b, next, bytepos);

  cache (ti, charpos, bytepos);
  return charpos;
}

/* Return the byte position in buffer B corresponding to character
   position CHARPOS.  */

ptrdiff_t
buf_charpos_to_bytepos (struct buffer *b, const ptrdiff_t charpos)
{
  /* If this buffer has as many characters as bytes, each character must
     be one byte.  This takes care of the case where
     enable-multibyte-characters is nil.  */
  const struct text_pos z = z_pos (b);
  if (z.charpos == z.bytepos)
    return charpos;

  if (charpos == z.charpos)
    return z.bytepos;
  ensure_charpos_indexed (b, charpos);

  struct text_index *ti = b->text->index;
  const ptrdiff_t entry = index_charpos_entry (ti, charpos);
  struct text_pos prev = index_text_pos (ti, entry);
  struct text_pos next = next_known_text_pos (b, entry);

  ptrdiff_t bytepos = narrow_charpos_bounds (b, &prev, &next, charpos);
  if (bytepos != TEXT_INDEX_INVALID_POSITION)
    return bytepos;

  /* Don't scan forward if CHARPOS is exactly on the previous know
     position because the index bytepos can be in the middle of a
     character, which is found by scanning backwards.  Otherwise, scan
     forward if we believe that's less expensive.  */
  if (charpos > prev.charpos
      && charpos - prev.charpos < next.charpos - charpos)
    bytepos = bytepos_forward_to_charpos (b, prev, charpos);
  else
    bytepos = bytepos_backward_to_charpos (b, next, charpos);

  cache (ti, charpos, bytepos);
  return bytepos;
}

/* Invalidate index entries for all positions > BYTEPOS in buffer B.
   Note that the entry for BYTEPOS itself, if it is at an interval
   boundary, remains unchanged.  */

void
text_index_invalidate (struct buffer *b, ptrdiff_t bytepos)
{
  struct text_index *ti = b->text->index;
  if (ti == NULL)
    return;

  const ptrdiff_t last_valid_entry = index_bytepos_entry (ti, bytepos);
  ti->nentries = min (ti->nentries, last_valid_entry + 1);

  if (ti->cache.bytepos > bytepos)
    invalidate_cache (ti);
}

DEFUN ("text-index--charpos-to-bytepos", Ftext_index__charpos_to_bytepos,
       Stext_index__charpos_to_bytepos, 1, 1, 0,
       doc: /* Convert CHARPOS to a bytepos in current buffer.
If POSITION is out of range, the value is nil.  */)
  (Lisp_Object charpos)
{
  const EMACS_INT pos = fix_position (charpos);
  if (pos < BEG || pos > Z)
    return Qnil;
  ptrdiff_t bytepos = buf_charpos_to_bytepos (current_buffer, pos);
  return make_fixnum (bytepos);
}

DEFUN ("text-index--bytepos-to-charpos", Ftext_index__bytepos_to_charpos,
       Stext_index__bytepos_to_charpos, 1, 1, 0,
       doc: /* Convert BYTEPOS to a charpos in current buffer.
If BYTEPOS is out of range, the value is nil.  */)
  (Lisp_Object bytepos)
{
  CHECK_FIXNUM (bytepos);
  const ptrdiff_t pos_byte = XFIXNUM (bytepos);
  if (pos_byte < BEG_BYTE || pos_byte > Z_BYTE)
    return Qnil;
  ptrdiff_t charpos = buf_bytepos_to_charpos (current_buffer, pos_byte);
  return make_fixnum (charpos);
}

DEFUN ("text-index--charpos-to-bytepos-brute",
       Ftext_index__charpos_to_bytepos_brute,
       Stext_index__charpos_to_bytepos_brute, 1, 1, 0,
       doc: /* Convert CHARPOS to a bytepos in current buffer.
Compute with brute force.  */)
  (Lisp_Object pos)
{
  const EMACS_INT to_charpos = fix_position (pos);
  if (to_charpos < BEG || to_charpos > Z)
    return Qnil;
  ptrdiff_t charpos = BEG, bytepos = BEG_BYTE;
  while (charpos < to_charpos)
    {
      ++bytepos;
      if (CHAR_HEAD_P (FETCH_BYTE (bytepos)))
	++charpos;
    }
  return make_fixnum (bytepos);
}

void
syms_of_text_index (void)
{
  defsubr (&Stext_index__charpos_to_bytepos);
  defsubr (&Stext_index__bytepos_to_charpos);
  defsubr (&Stext_index__charpos_to_bytepos_brute);
}
