/* Markers: examining, setting and deleting.
   Copyright (C) 1985, 1997, 1998 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "buffer.h"
#include "charset.h"

/* Record one cached position found recently by
   buf_charpos_to_bytepos or buf_bytepos_to_charpos.  */

static int cached_charpos;
static int cached_bytepos;
static struct buffer *cached_buffer;
static int cached_modiff;

static void byte_char_debug_check P_ ((struct buffer *, int, int));

/* Nonzero means enable debugging checks on byte/char correspondences.  */

static int byte_debug_flag;

void
clear_charpos_cache (b)
     struct buffer *b;
{
  if (cached_buffer == b)
    cached_buffer = 0;
}

/* Converting between character positions and byte positions.  */

/* There are several places in the buffer where we know
   the correspondence: BEG, BEGV, PT, GPT, ZV and Z,
   and everywhere there is a marker.  So we find the one of these places
   that is closest to the specified position, and scan from there.  */

/* charpos_to_bytepos returns the byte position corresponding to CHARPOS.  */

/* This macro is a subroutine of charpos_to_bytepos.
   Note that it is desirable that BYTEPOS is not evaluated
   except when we really want its value.  */

#define CONSIDER(CHARPOS, BYTEPOS)					\
{									\
  int this_charpos = (CHARPOS);						\
  int changed = 0;							\
									\
  if (this_charpos == charpos)						\
    {									\
      int value = (BYTEPOS);						\
      if (byte_debug_flag)						\
	byte_char_debug_check (b, charpos, value);			\
      return value;							\
    }									\
  else if (this_charpos > charpos)					\
    {									\
      if (this_charpos < best_above)					\
	{								\
	  best_above = this_charpos;					\
	  best_above_byte = (BYTEPOS);					\
	  changed = 1;							\
	}								\
    }									\
  else if (this_charpos > best_below)					\
    {									\
      best_below = this_charpos;					\
      best_below_byte = (BYTEPOS);					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
        {								\
	  int value = best_below_byte + (charpos - best_below);		\
	  if (byte_debug_flag)						\
	    byte_char_debug_check (b, charpos, value);			\
	  return value;							\
	}								\
    }									\
}

static void
byte_char_debug_check (b, charpos, bytepos)
     struct buffer *b;
     int charpos, bytepos;
{
  int nchars = 0;

  if (bytepos > BUF_GPT_BYTE (b))
    {
      nchars = multibyte_chars_in_text (BUF_BEG_ADDR (b),
					BUF_GPT_BYTE (b) - BUF_BEG_BYTE (b));
      nchars += multibyte_chars_in_text (BUF_GAP_END_ADDR (b),
					 bytepos - BUF_GPT_BYTE (b));
    }
  else
    nchars = multibyte_chars_in_text (BUF_BEG_ADDR (b),
				      bytepos - BUF_BEG_BYTE (b));

  if (charpos - 1 != nchars)
    abort ();
}

int
charpos_to_bytepos (charpos)
     int charpos;
{
  return buf_charpos_to_bytepos (current_buffer, charpos);
}

int
buf_charpos_to_bytepos (b, charpos)
     struct buffer *b;
     int charpos;
{
  Lisp_Object tail;
  int best_above, best_above_byte;
  int best_below, best_below_byte;

  if (charpos < BUF_BEG (b) || charpos > BUF_Z (b))
    abort ();

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

  tail = BUF_MARKERS (b);
  while (! NILP (tail))
    {
      CONSIDER (XMARKER (tail)->charpos, XMARKER (tail)->bytepos);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;

      tail = XMARKER (tail)->chain;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (charpos - best_below < best_above - charpos)
    {
      int record = charpos - best_below > 5000;

      while (best_below != charpos)
	{
	  best_below++;
	  BUF_INC_POS (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	{
	  Lisp_Object marker, buffer;
	  marker = Fmake_marker ();
	  XSETBUFFER (buffer, b);
	  set_marker_both (marker, buffer, best_below, best_below_byte);
	}

      if (byte_debug_flag)
	byte_char_debug_check (b, charpos, best_below_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      return best_below_byte;
    }
  else
    {
      int record = best_above - charpos > 5000;

      while (best_above != charpos)
	{
	  best_above--;
	  BUF_DEC_POS (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	{
	  Lisp_Object marker, buffer;
	  marker = Fmake_marker ();
	  XSETBUFFER (buffer, b);
	  set_marker_both (marker, buffer, best_above, best_above_byte);
	}

      if (byte_debug_flag)
	byte_char_debug_check (b, charpos, best_above_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      return best_above_byte;
    }
}

#undef CONSIDER

/* bytepos_to_charpos returns the char position corresponding to BYTEPOS.  */

/* This macro is a subroutine of bytepos_to_charpos.
   It is used when BYTEPOS is actually the byte position.  */

#define CONSIDER(BYTEPOS, CHARPOS)					\
{									\
  int this_bytepos = (BYTEPOS);						\
  int changed = 0;							\
									\
  if (this_bytepos == bytepos)						\
    {									\
      int value = (CHARPOS);						\
      if (byte_debug_flag)						\
	byte_char_debug_check (b, value, bytepos);			\
      return value;							\
    }									\
  else if (this_bytepos > bytepos)					\
    {									\
      if (this_bytepos < best_above_byte)				\
	{								\
	  best_above = (CHARPOS);					\
	  best_above_byte = this_bytepos;				\
	  changed = 1;							\
	}								\
    }									\
  else if (this_bytepos > best_below_byte)				\
    {									\
      best_below = (CHARPOS);						\
      best_below_byte = this_bytepos;					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
	{								\
	  int value = best_below + (bytepos - best_below_byte);		\
	  if (byte_debug_flag)						\
	    byte_char_debug_check (b, value, bytepos);			\
	  return value;							\
	}								\
    }									\
}

int
bytepos_to_charpos (bytepos)
     int bytepos;
{
  return buf_bytepos_to_charpos (current_buffer, bytepos);
}

int
buf_bytepos_to_charpos (b, bytepos)
     struct buffer *b;
     int bytepos;
{
  Lisp_Object tail;
  int best_above, best_above_byte;
  int best_below, best_below_byte;

  if (bytepos < BUF_BEG_BYTE (b) || bytepos > BUF_Z_BYTE (b))
    abort ();

  best_above = BUF_Z (b);
  best_above_byte = BUF_Z_BYTE (b);

  /* If this buffer has as many characters as bytes,
     each character must be one byte.
     This takes care of the case where enable-multibyte-characters is nil.  */
  if (best_above == best_above_byte)
    return bytepos;

  best_below = BEG;
  best_below_byte = BEG_BYTE;

  CONSIDER (BUF_PT_BYTE (b), BUF_PT (b));
  CONSIDER (BUF_GPT_BYTE (b), BUF_GPT (b));
  CONSIDER (BUF_BEGV_BYTE (b), BUF_BEGV (b));
  CONSIDER (BUF_ZV_BYTE (b), BUF_ZV (b));

  if (b == cached_buffer && BUF_MODIFF (b) == cached_modiff)
    CONSIDER (cached_bytepos, cached_charpos);

  tail = BUF_MARKERS (b);
  while (! NILP (tail))
    {
      CONSIDER (XMARKER (tail)->bytepos, XMARKER (tail)->charpos);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;

      tail = XMARKER (tail)->chain;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (bytepos - best_below_byte < best_above_byte - bytepos)
    {
      int record = bytepos - best_below_byte > 5000;

      while (best_below_byte < bytepos)
	{
	  best_below++;
	  BUF_INC_POS (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && ! NILP (BUF_MARKERS (b)))
	{
	  Lisp_Object marker, buffer;
	  marker = Fmake_marker ();
	  XSETBUFFER (buffer, b);
	  set_marker_both (marker, buffer, best_below, best_below_byte);
	}

      if (byte_debug_flag)
	byte_char_debug_check (b, best_below, bytepos);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      return best_below;
    }
  else
    {
      int record = best_above_byte - bytepos > 5000;

      while (best_above_byte > bytepos)
	{
	  best_above--;
	  BUF_DEC_POS (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && ! NILP (BUF_MARKERS (b)))
	{
	  Lisp_Object marker, buffer;
	  marker = Fmake_marker ();
	  XSETBUFFER (buffer, b);
	  set_marker_both (marker, buffer, best_above, best_above_byte);
	}

      if (byte_debug_flag)
	byte_char_debug_check (b, best_above, bytepos);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      return best_above;
    }
}

#undef CONSIDER

/* Operations on markers. */

DEFUN ("marker-buffer", Fmarker_buffer, Smarker_buffer, 1, 1, 0,
       doc: /* Return the buffer that MARKER points into, or nil if none.
Returns nil if MARKER points into a dead buffer.  */)
     (marker)
     register Lisp_Object marker;
{
  register Lisp_Object buf;
  CHECK_MARKER (marker);
  if (XMARKER (marker)->buffer)
    {
      XSETBUFFER (buf, XMARKER (marker)->buffer);
      /* Return marker's buffer only if it is not dead.  */
      if (!NILP (XBUFFER (buf)->name))
	return buf;
    }
  return Qnil;
}

DEFUN ("marker-position", Fmarker_position, Smarker_position, 1, 1, 0,
       doc: /* Return the position MARKER points at, as a character number.  */)
     (marker)
     Lisp_Object marker;
{
  CHECK_MARKER (marker);
  if (XMARKER (marker)->buffer)
    return make_number (XMARKER (marker)->charpos);

  return Qnil;
}

DEFUN ("set-marker", Fset_marker, Sset_marker, 2, 3, 0,
       doc: /* Position MARKER before character number POSITION in BUFFER.
BUFFER defaults to the current buffer.
If POSITION is nil, makes marker point nowhere.
Then it no longer slows down editing in any buffer.
Returns MARKER.  */)
     (marker, position, buffer)
     Lisp_Object marker, position, buffer;
{
  register int charno, bytepos;
  register struct buffer *b;
  register struct Lisp_Marker *m;

  CHECK_MARKER (marker);
  /* If position is nil or a marker that points nowhere,
     make this marker point nowhere.  */
  if (NILP (position)
      || (MARKERP (position) && !XMARKER (position)->buffer))
    {
      unchain_marker (marker);
      return marker;
    }

  if (NILP (buffer))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      b = XBUFFER (buffer);
      /* If buffer is dead, set marker to point nowhere.  */
      if (EQ (b->name, Qnil))
	{
	  unchain_marker (marker);
	  return marker;
	}
    }

  m = XMARKER (marker);

  /* Optimize the special case where we are copying the position
     of an existing marker, and MARKER is already in the same buffer.  */
  if (MARKERP (position) && b == XMARKER (position)->buffer
      && b == m->buffer)
    {
      m->bytepos = XMARKER (position)->bytepos;
      m->charpos = XMARKER (position)->charpos;
      return marker;
    }

  CHECK_NUMBER_COERCE_MARKER (position);

  charno = XINT (position);

  if (charno < BUF_BEG (b))
    charno = BUF_BEG (b);
  if (charno > BUF_Z (b))
    charno = BUF_Z (b);

  bytepos = buf_charpos_to_bytepos (b, charno);

  /* Every character is at least one byte.  */
  if (charno > bytepos)
    abort ();

  m->bytepos = bytepos;
  m->charpos = charno;

  if (m->buffer != b)
    {
      unchain_marker (marker);
      m->buffer = b;
      m->chain = BUF_MARKERS (b);
      BUF_MARKERS (b) = marker;
    }
  
  return marker;
}

/* This version of Fset_marker won't let the position
   be outside the visible part.  */

Lisp_Object 
set_marker_restricted (marker, pos, buffer)
     Lisp_Object marker, pos, buffer;
{
  register int charno, bytepos;
  register struct buffer *b;
  register struct Lisp_Marker *m;

  CHECK_MARKER (marker);
  /* If position is nil or a marker that points nowhere,
     make this marker point nowhere.  */
  if (NILP (pos)
      || (MARKERP (pos) && !XMARKER (pos)->buffer))
    {
      unchain_marker (marker);
      return marker;
    }

  if (NILP (buffer))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      b = XBUFFER (buffer);
      /* If buffer is dead, set marker to point nowhere.  */
      if (EQ (b->name, Qnil))
	{
	  unchain_marker (marker);
	  return marker;
	}
    }

  m = XMARKER (marker);

  /* Optimize the special case where we are copying the position
     of an existing marker, and MARKER is already in the same buffer.  */
  if (MARKERP (pos) && b == XMARKER (pos)->buffer
      && b == m->buffer)
    {
      m->bytepos = XMARKER (pos)->bytepos;
      m->charpos = XMARKER (pos)->charpos;
      return marker;
    }

  CHECK_NUMBER_COERCE_MARKER (pos);

  charno = XINT (pos);

  if (charno < BUF_BEGV (b))
    charno = BUF_BEGV (b);
  if (charno > BUF_ZV (b))
    charno = BUF_ZV (b);

  bytepos = buf_charpos_to_bytepos (b, charno);

  /* Every character is at least one byte.  */
  if (charno > bytepos)
    abort ();

  m->bytepos = bytepos;
  m->charpos = charno;

  if (m->buffer != b)
    {
      unchain_marker (marker);
      m->buffer = b;
      m->chain = BUF_MARKERS (b);
      BUF_MARKERS (b) = marker;
    }
  
  return marker;
}

/* Set the position of MARKER, specifying both the
   character position and the corresponding byte position.  */

Lisp_Object 
set_marker_both (marker, buffer, charpos, bytepos)
     Lisp_Object marker, buffer;
     int charpos, bytepos;
{
  register struct buffer *b;
  register struct Lisp_Marker *m;

  CHECK_MARKER (marker);

  if (NILP (buffer))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      b = XBUFFER (buffer);
      /* If buffer is dead, set marker to point nowhere.  */
      if (EQ (b->name, Qnil))
	{
	  unchain_marker (marker);
	  return marker;
	}
    }

  m = XMARKER (marker);

  /* In a single-byte buffer, the two positions must be equal.  */
  if (BUF_Z (b) == BUF_Z_BYTE (b)
      && charpos != bytepos)
    abort ();
  /* Every character is at least one byte.  */
  if (charpos > bytepos)
    abort ();

  m->bytepos = bytepos;
  m->charpos = charpos;

  if (m->buffer != b)
    {
      unchain_marker (marker);
      m->buffer = b;
      m->chain = BUF_MARKERS (b);
      BUF_MARKERS (b) = marker;
    }
  
  return marker;
}

/* This version of set_marker_both won't let the position
   be outside the visible part.  */

Lisp_Object 
set_marker_restricted_both (marker, buffer, charpos, bytepos)
     Lisp_Object marker, buffer;
     int charpos, bytepos;
{
  register struct buffer *b;
  register struct Lisp_Marker *m;

  CHECK_MARKER (marker);

  if (NILP (buffer))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      b = XBUFFER (buffer);
      /* If buffer is dead, set marker to point nowhere.  */
      if (EQ (b->name, Qnil))
	{
	  unchain_marker (marker);
	  return marker;
	}
    }

  m = XMARKER (marker);

  if (charpos < BUF_BEGV (b))
    charpos = BUF_BEGV (b);
  if (charpos > BUF_ZV (b))
    charpos = BUF_ZV (b);
  if (bytepos < BUF_BEGV_BYTE (b))
    bytepos = BUF_BEGV_BYTE (b);
  if (bytepos > BUF_ZV_BYTE (b))
    bytepos = BUF_ZV_BYTE (b);

  /* In a single-byte buffer, the two positions must be equal.  */
  if (BUF_Z (b) == BUF_Z_BYTE (b)
      && charpos != bytepos)
    abort ();
  /* Every character is at least one byte.  */
  if (charpos > bytepos)
    abort ();

  m->bytepos = bytepos;
  m->charpos = charpos;

  if (m->buffer != b)
    {
      unchain_marker (marker);
      m->buffer = b;
      m->chain = BUF_MARKERS (b);
      BUF_MARKERS (b) = marker;
    }
  
  return marker;
}

/* Remove MARKER from the chain of whatever buffer it is in.
   Leave it "in no buffer".

   This is called during garbage collection,
   so we must be careful to ignore and preserve mark bits,
   including those in chain fields of markers.  */

void
unchain_marker (marker)
     register Lisp_Object marker;
{
  register Lisp_Object tail, prev, next;
  register EMACS_INT omark;
  register struct buffer *b;

  b = XMARKER (marker)->buffer;
  if (b == 0)
    return;

  if (EQ (b->name, Qnil))
    abort ();

  XMARKER (marker)->buffer = 0;

  tail = BUF_MARKERS (b);
  prev = Qnil;
  while (! GC_NILP (tail))
    {
      next = XMARKER (tail)->chain;
      XUNMARK (next);

      if (XMARKER (marker) == XMARKER (tail))
	{
	  if (NILP (prev))
	    {
	      BUF_MARKERS (b) = next;
	      /* Deleting first marker from the buffer's chain.  Crash
		 if new first marker in chain does not say it belongs
		 to the same buffer, or at least that they have the same
		 base buffer.  */
	      if (!NILP (next) && b->text != XMARKER (next)->buffer->text)
		abort ();
	    }
	  else
	    {
	      omark = XMARKBIT (XMARKER (prev)->chain);
	      XMARKER (prev)->chain = next;
	      XSETMARKBIT (XMARKER (prev)->chain, omark);
	    }
	  /* We have removed the marker from the chain;
	     no need to scan the rest of the chain.  */
	  return;
	}
      else
	prev = tail;
      tail = next;
    }

  /* Marker was not in its chain.  */
  abort ();
}

/* Return the char position of marker MARKER, as a C integer.  */

int
marker_position (marker)
     Lisp_Object marker;
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;

  if (!buf)
    error ("Marker does not point anywhere");

  return m->charpos;
}

/* Return the byte position of marker MARKER, as a C integer.  */

int
marker_byte_position (marker)
     Lisp_Object marker;
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;
  register int i = m->bytepos;

  if (!buf)
    error ("Marker does not point anywhere");

  if (i < BUF_BEG_BYTE (buf) || i > BUF_Z_BYTE (buf))
    abort ();

  return i;
}

DEFUN ("copy-marker", Fcopy_marker, Scopy_marker, 1, 2, 0,
       doc: /* Return a new marker pointing at the same place as MARKER.
If argument is a number, makes a new marker pointing
at that position in the current buffer.
The optional argument TYPE specifies the insertion type of the new marker;
see `marker-insertion-type'.  */)
     (marker, type)
     register Lisp_Object marker, type;
{
  register Lisp_Object new;

  if (! (INTEGERP (marker) || MARKERP (marker)))
    marker = wrong_type_argument (Qinteger_or_marker_p, marker);

  new = Fmake_marker ();
  Fset_marker (new, marker,
	       (MARKERP (marker) ? Fmarker_buffer (marker) : Qnil));
  XMARKER (new)->insertion_type = !NILP (type);
  return new;
}

DEFUN ("marker-insertion-type", Fmarker_insertion_type,
       Smarker_insertion_type, 1, 1, 0,
       doc: /* Return insertion type of MARKER: t if it stays after inserted text.
nil means the marker stays before text inserted there.  */)
     (marker)
     register Lisp_Object marker;
{
  CHECK_MARKER (marker);
  return XMARKER (marker)->insertion_type ? Qt : Qnil;
}

DEFUN ("set-marker-insertion-type", Fset_marker_insertion_type,
       Sset_marker_insertion_type, 2, 2, 0,
       doc: /* Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it.  */)
     (marker, type)
     Lisp_Object marker, type;
{
  CHECK_MARKER (marker);

  XMARKER (marker)->insertion_type = ! NILP (type);
  return type;
}

DEFUN ("buffer-has-markers-at", Fbuffer_has_markers_at, Sbuffer_has_markers_at,
       1, 1, 0,
       doc: /* Return t if there are markers pointing at POSITION in the current buffer.  */)
     (position)
     Lisp_Object position;
{
  register Lisp_Object tail;
  register int charno;

  charno = XINT (position);

  if (charno < BEG)
    charno = BEG;
  if (charno > Z)
    charno = Z;

  for (tail = BUF_MARKERS (current_buffer);
       !NILP (tail);
       tail = XMARKER (tail)->chain)
    if (XMARKER (tail)->charpos == charno)
      return Qt;

  return Qnil;
}

/* For debugging -- count the markers in buffer BUF.  */

int
count_markers (buf)
     struct buffer *buf;
{
  int total = 0;
  Lisp_Object tail;

  for (tail = BUF_MARKERS (buf);
       !NILP (tail);
       tail = XMARKER (tail)->chain)
    total++;

  return total;
}

void
syms_of_marker ()
{
  defsubr (&Smarker_position);
  defsubr (&Smarker_buffer);
  defsubr (&Sset_marker);
  defsubr (&Scopy_marker);
  defsubr (&Smarker_insertion_type);
  defsubr (&Sset_marker_insertion_type);
  defsubr (&Sbuffer_has_markers_at);

  DEFVAR_BOOL ("byte-debug-flag", &byte_debug_flag,
	       doc: /* Non-nil enables debugging checks in byte/char position conversions.  */);
  byte_debug_flag = 0;
}
