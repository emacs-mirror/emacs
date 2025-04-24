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
#include "character.h"
#include "buffer.h"
#include "text-index.h"
#include "window.h"

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

/* Converting between character positions and byte positions.  */

static void
CHECK_MARKER (Lisp_Object x)
{
  CHECK_TYPE (MARKERP (x), Qmarkerp, x);
}


/* Operations on markers.  */

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
    return make_fixnum (marker_vector_charpos (XMARKER (marker)));

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

  return make_fixnum (marker_vector_last_charpos (XMARKER (marker)));
}

/* Change M so it points to B at CHARPOS and BYTEPOS.  */

static void
attach_marker (struct Lisp_Marker *m, struct buffer *b,
	       ptrdiff_t charpos)
{
   if (m->buffer != b)
    {
      unchain_marker (m);
      marker_vector_add (b, m);
    }

   eassert (m->buffer == b);
   marker_vector_set_charpos (m, charpos);
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
  else if (MARKERP (position) && b == XMARKER (position)->buffer
	   && b == m->buffer)
    {
      const ptrdiff_t charpos = marker_vector_charpos (XMARKER (position));
      marker_vector_set_charpos (m, charpos);
    }

  else
    {
      register ptrdiff_t charpos;

      /* Do not use CHECK_FIXNUM_COERCE_MARKER because we
	 don't want to call buf_charpos_to_bytepos if POSITION
	 is a marker and so we know the bytepos already.  */
      if (FIXNUMP (position))
	{
#if EMACS_INT_MAX > PTRDIFF_MAX
	  /* A --with-wide-int build.  */
	  EMACS_INT cpos = XFIXNUM (position);
	  if (cpos > PTRDIFF_MAX)
	    cpos = PTRDIFF_MAX;
	  charpos = cpos;
#else
	  charpos = XFIXNUM (position);
#endif
	}
      else if (MARKERP (position))
	{
	  charpos = marker_vector_charpos (XMARKER (position));
	}
      else
	wrong_type_argument (Qinteger_or_marker_p, position);

      charpos = clip_to_bounds
	(restricted ? BUF_BEGV (b) : BUF_BEG (b), charpos,
	 restricted ? BUF_ZV (b) : BUF_Z (b));

      attach_marker (m, b, charpos);
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
		 ptrdiff_t charpos)
{
  register struct Lisp_Marker *m;
  register struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  if (b)
    attach_marker (m, b, charpos);
  else
    unchain_marker (m);
  return marker;
}

/* Like the above, but won't let the position be outside the visible part.  */

Lisp_Object
set_marker_restricted_both (Lisp_Object marker, Lisp_Object buffer,
			    ptrdiff_t charpos)
{
  register struct Lisp_Marker *m;
  register struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  if (b)
    {
      attach_marker (m, b,
		     clip_to_bounds (BUF_BEGV (b),
				     charpos, BUF_ZV (b)));
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
  Fset_marker (marker, Qnil, Qnil);
}

/* Remove MARKER from the chain of whatever buffer it is in.  Set its
   buffer NULL.  */

void
unchain_marker (struct Lisp_Marker *marker)
{
  if (marker->buffer)
    {
      Lisp_Object mv = BUF_MARKERS (marker->buffer);
      marker_vector_remove (XVECTOR (mv), marker);
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

  const ptrdiff_t charpos = marker_vector_charpos (m);
  eassert (BUF_BEG (buf) <= charpos && charpos <= BUF_Z (buf));
  return charpos;
}

/* Return the byte position of marker MARKER, as a C integer.  */

ptrdiff_t
marker_byte_position (Lisp_Object marker)
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;

  if (!buf)
    error ("Marker does not point anywhere");

  const ptrdiff_t bytepos = marker_vector_bytepos (m);
  eassert (BUF_BEG_BYTE (buf) <= bytepos && bytepos <= BUF_Z_BYTE (buf));
  return bytepos;
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

#ifdef MARKER_DEBUG

/* For debugging -- count the markers in buffer BUF.  */

int
count_markers (struct buffer *buf)
{
  int total = 0;
  struct Lisp_Marker *tail;

  for (tail = BUF_MARKERS (buf); tail; tail = tail->next)
    total++;

  return total;
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
  defsubr (&Smarker_position);
  defsubr (&Smarker_last_position);
  defsubr (&Smarker_buffer);
  defsubr (&Sset_marker);
  defsubr (&Scopy_marker);
  defsubr (&Smarker_insertion_type);
  defsubr (&Sset_marker_insertion_type);
}
