/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985, 1986, 1993, 1994, 1995 Free Software Foundation, Inc.

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
#include "intervals.h"
#include "buffer.h"
#include "charset.h"
#include "window.h"
#include "blockinput.h"

#ifndef NULL
#define NULL 0
#endif

#define min(x, y) ((x) < (y) ? (x) : (y))

static void insert_from_string_1 ();
static void insert_from_buffer_1 ();
static void gap_left ();
static void gap_right ();
static void adjust_markers ();
static void adjust_point ();

Lisp_Object Fcombine_after_change_execute ();

/* Non-nil means don't call the after-change-functions right away,
   just record an element in Vcombine_after_change_calls_list.  */
Lisp_Object Vcombine_after_change_calls;

/* List of elements of the form (BEG-UNCHANGED END-UNCHANGED CHANGE-AMOUNT)
   describing changes which happened while combine_after_change_calls
   was nonzero.  We use this to decide how to call them
   once the deferral ends.

   In each element.
   BEG-UNCHANGED is the number of chars before the changed range.
   END-UNCHANGED is the number of chars after the changed range,
   and CHANGE-AMOUNT is the number of characters inserted by the change
   (negative for a deletion).  */
Lisp_Object combine_after_change_list;

/* Buffer which combine_after_change_list is about.  */
Lisp_Object combine_after_change_buffer;

/* Move gap to position `pos'.
   Note that this can quit!  */

void
move_gap (pos)
     int pos;
{
  if (pos < GPT)
    gap_left (pos, 0);
  else if (pos > GPT)
    gap_right (pos);
}

/* Move the gap to POS, which is less than the current GPT.
   If NEWGAP is nonzero, then don't update beg_unchanged and end_unchanged.  */

static void
gap_left (pos, newgap)
     register int pos;
     int newgap;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (!newgap)
    {
      if (unchanged_modified == MODIFF
	  && overlay_unchanged_modified == OVERLAY_MODIFF)
	{
	  beg_unchanged = pos;
	  end_unchanged = Z - pos - 1;
	}
      else
	{
	  if (Z - GPT < end_unchanged)
	    end_unchanged = Z - GPT;
	  if (pos < beg_unchanged)
	    beg_unchanged = pos;
	}
    }

  i = GPT;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT - BEG;

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving upward.  */
	  && (BCOPY_UPWARD_SAFE
	      || to - from >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_UPWARD_SAFE && i > to - from)
	    i = to - from;
	  new_s1 -= i;
	  from -= i, to -= i;
	  bcopy (from, to, i);
	}
      else
#endif
	{
	  new_s1 -= i;
	  while (--i >= 0)
	    *--to = *--from;
	}
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (pos + 1, GPT, GAP_SIZE);
  GPT = pos + 1;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  QUIT;
}

static void
gap_right (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (unchanged_modified == MODIFF
      && overlay_unchanged_modified == OVERLAY_MODIFF)

    {
      beg_unchanged = pos;
      end_unchanged = Z - pos - 1;
    }
  else
    {
      if (Z - pos - 1 < end_unchanged)
	end_unchanged = Z - pos - 1;
      if (GPT - BEG < beg_unchanged)
	beg_unchanged = GPT - BEG;
    }

  i = GPT;
  from = GAP_END_ADDR;
  to = GPT_ADDR;
  new_s1 = GPT - 1;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
#ifdef GAP_USE_BCOPY
      if (i >= 128
	  /* bcopy is safe if the two areas of memory do not overlap
	     or on systems where bcopy is always safe for moving downward.  */
	  && (BCOPY_DOWNWARD_SAFE
	      || from - to >= 128))
	{
	  /* If overlap is not safe, avoid it by not moving too many
	     characters at once.  */
	  if (!BCOPY_DOWNWARD_SAFE && i > from - to)
	    i = from - to;
	  new_s1 += i;
	  bcopy (from, to, i);
	  from += i, to += i;
	}
      else
#endif
	{
	  new_s1 += i;
	  while (--i >= 0)
	    *to++ = *from++;
	}
    }

  adjust_markers (GPT + GAP_SIZE, pos + 1 + GAP_SIZE, - GAP_SIZE);
  GPT = pos + 1;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  QUIT;
}

/* Add AMOUNT to the position of every marker in the current buffer
   whose current position is between FROM (exclusive) and TO (inclusive).

   Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by AMOUNT.

   When the latter adjustment is done, if AMOUNT is negative,
   we record the adjustment for undo.  (This case happens only for
   deletion.)  */

static void
adjust_markers (from, to, amount)
     register int from, to, amount;
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register int mpos;

  marker = BUF_MARKERS (current_buffer);

  while (!NILP (marker))
    {
      m = XMARKER (marker);
      mpos = m->bufpos;
      if (amount > 0)
	{
	  if (mpos > to && mpos < to + amount)
	    mpos = to + amount;
	}
      else
	{
	  /* Here's the case where a marker is inside text being deleted.
	     AMOUNT can be negative for gap motion, too,
	     but then this range contains no markers.  */
	  if (mpos > from + amount && mpos <= from)
	    {
	      int before = mpos;
	      int after = from + amount;

	      mpos = after;

	      /* Compute the before and after positions
		 as buffer positions.  */
	      if (before > GPT + GAP_SIZE)
		before -= GAP_SIZE;
	      else if (before > GPT)
		before = GPT;

	      if (after > GPT + GAP_SIZE)
		after -= GAP_SIZE;
	      else if (after > GPT)
		after = GPT;

	      record_marker_adjustment (marker, after - before);
	    }
	}
      if (mpos > from && mpos <= to)
	mpos += amount;
      m->bufpos = mpos;
      marker = m->chain;
    }
}

/* Adjust markers whose insertion-type is t
   for an insertion of AMOUNT characters at POS.  */

static void
adjust_markers_for_insert (pos, amount)
     register int pos, amount;
{
  Lisp_Object marker;
  int adjusted = 0;

  marker = BUF_MARKERS (current_buffer);

  while (!NILP (marker))
    {
      register struct Lisp_Marker *m = XMARKER (marker);
      if (m->insertion_type && m->bufpos == pos)
	{
	  m->bufpos += amount;
	  adjusted = 1;
	}
      marker = m->chain;
    }
  if (adjusted)
    /* Adjusting only markers whose insertion-type is t may result in
       disordered overlays in the slot `overlays_before'.  */
    fix_overlays_before (current_buffer, pos, pos + amount);
}

/* Add the specified amount to point.  This is used only when the value
   of point changes due to an insert or delete; it does not represent
   a conceptual change in point as a marker.  In particular, point is
   not crossing any interval boundaries, so there's no need to use the
   usual SET_PT macro.  In fact it would be incorrect to do so, because
   either the old or the new value of point is out of sync with the
   current set of intervals.  */
static void
adjust_point (amount)
     int amount;
{
  BUF_PT (current_buffer) += amount;
}

/* Make the gap INCREMENT characters longer.  */

void
make_gap (increment)
     int increment;
{
  unsigned char *result;
  Lisp_Object tem;
  int real_gap_loc;
  int old_gap_size;

  /* If we have to get more space, get enough to last a while.  */
  increment += 2000;

  /* Don't allow a buffer size that won't fit in an int
     even if it will fit in a Lisp integer.
     That won't work because so many places use `int'.  */
     
  if (Z - BEG + GAP_SIZE + increment
      >= ((unsigned) 1 << (min (BITS_PER_INT, VALBITS) - 1)))
    error ("Buffer exceeds maximum size");

  BLOCK_INPUT;
  /* We allocate extra 1-byte `\0' at the tail for anchoring a search.  */
  result = BUFFER_REALLOC (BEG_ADDR, (Z - BEG + GAP_SIZE + increment + 1));

  if (result == 0)
    {
      UNBLOCK_INPUT;
      memory_full ();
    }

  /* We can't unblock until the new address is properly stored.  */
  BEG_ADDR = result;
  UNBLOCK_INPUT;

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  old_gap_size = GAP_SIZE;

  /* Call the newly allocated space a gap at the end of the whole space.  */
  GPT = Z + GAP_SIZE;
  GAP_SIZE = increment;

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (real_gap_loc + old_gap_size, 1);

  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;

  /* Put an anchor.  */
  *(Z_ADDR) = 0;

  Vinhibit_quit = tem;
}

/* Insert a string of specified length before point.
   DO NOT use this for the contents of a Lisp string or a Lisp buffer!
   prepare_to_modify_buffer could relocate the text.  */

void
insert (string, length)
     register unsigned char *string;
     register length;
{
  if (length > 0)
    {
      insert_1 (string, length, 0, 1);
      signal_after_change (PT-length, 0, length);
    }
}

void
insert_and_inherit (string, length)
     register unsigned char *string;
     register length;
{
  if (length > 0)
    {
      insert_1 (string, length, 1, 1);
      signal_after_change (PT-length, 0, length);
    }
}

void
insert_1 (string, length, inherit, prepare)
     register unsigned char *string;
     register int length;
     int inherit, prepare;
{
  register Lisp_Object temp;

  if (prepare)
    prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (PT, length);
  MODIFF++;

  bcopy (string, GPT_ADDR, length);

#ifdef USE_TEXT_PROPERTIES
  if (BUF_INTERVALS (current_buffer) != 0)
    /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES.  */
    offset_intervals (current_buffer, PT, length);
#endif

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  adjust_overlays_for_insert (PT, length);
  adjust_markers_for_insert (PT, length);
  adjust_point (length);

#ifdef USE_TEXT_PROPERTIES
  if (!inherit && BUF_INTERVALS (current_buffer) != 0)
    Fset_text_properties (make_number (PT - length), make_number (PT),
			  Qnil, Qnil);
#endif
}

/* Insert the part of the text of STRING, a Lisp object assumed to be
   of type string, consisting of the LENGTH characters starting at
   position POS.  If the text of STRING has properties, they are absorbed
   into the buffer.

   It does not work to use `insert' for this, because a GC could happen
   before we bcopy the stuff into the buffer, and relocate the string
   without insert noticing.  */

void
insert_from_string (string, pos, length, inherit)
     Lisp_Object string;
     register int pos, length;
     int inherit;
{
  if (length > 0)
    {
      insert_from_string_1 (string, pos, length, inherit);
      signal_after_change (PT-length, 0, length);
    }
}

static void
insert_from_string_1 (string, pos, length, inherit)
     Lisp_Object string;
     register int pos, length;
     int inherit;
{
  register Lisp_Object temp;
  struct gcpro gcpro1;

  /* Make sure point-max won't overflow after this insertion.  */
  XSETINT (temp, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  GCPRO1 (string);
  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (PT, length);
  MODIFF++;
  UNGCPRO;

  bcopy (XSTRING (string)->data, GPT_ADDR, length);

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  offset_intervals (current_buffer, PT, length);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  adjust_overlays_for_insert (PT, length);
  adjust_markers_for_insert (PT, length);

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  graft_intervals_into_buffer (XSTRING (string)->intervals, PT, length,
			       current_buffer, inherit);

  adjust_point (length);
}

/* Insert text from BUF, starting at POS and having length LENGTH, into the
   current buffer.  If the text in BUF has properties, they are absorbed
   into the current buffer.

   It does not work to use `insert' for this, because a malloc could happen
   and relocate BUF's text before the bcopy happens.  */

void
insert_from_buffer (buf, pos, length, inherit)
     struct buffer *buf;
     int pos, length;
     int inherit;
{
  if (length > 0)
    {
      insert_from_buffer_1 (buf, pos, length, inherit);
      signal_after_change (PT-length, 0, length);
    }
}

static void
insert_from_buffer_1 (buf, pos, length, inherit)
     struct buffer *buf;
     int pos, length;
     int inherit;
{
  register Lisp_Object temp;
  int chunk;

  /* Make sure point-max won't overflow after this insertion.  */
  XSETINT (temp, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap (PT);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (PT, length);
  MODIFF++;

  if (pos < BUF_GPT (buf))
    {
      chunk = BUF_GPT (buf) - pos;
      if (chunk > length)
	chunk = length;
      bcopy (BUF_CHAR_ADDRESS (buf, pos), GPT_ADDR, chunk);
    }
  else
    chunk = 0;
  if (chunk < length)
    bcopy (BUF_CHAR_ADDRESS (buf, pos + chunk),
	   GPT_ADDR + chunk, length - chunk);

#ifdef USE_TEXT_PROPERTIES
  if (BUF_INTERVALS (current_buffer) != 0)
    offset_intervals (current_buffer, PT, length);
#endif

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  adjust_overlays_for_insert (PT, length);
  adjust_markers_for_insert (PT, length);
  adjust_point (length);

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  graft_intervals_into_buffer (copy_intervals (BUF_INTERVALS (buf),
					       pos, length),
			       PT - length, length, current_buffer, inherit);
}

/* Insert the character C before point */

void
insert_char (c)
     int c;
{
  unsigned char workbuf[4], *str;
  int len = CHAR_STRING (c, workbuf, str);

  insert (str, len);
}

/* Insert the null-terminated string S before point */

void
insert_string (s)
     char *s;
{
  insert (s, strlen (s));
}

/* Like `insert' except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.
   Don't use this function to insert part of a Lisp string,
   since gc could happen and relocate it.  */

void
insert_before_markers (string, length)
     unsigned char *string;
     register int length;
{
  if (length > 0)
    {
      register int opoint = PT;
      insert_1 (string, length, 0, 1);
      adjust_markers (opoint - 1, opoint, length);
      signal_after_change (PT-length, 0, length);
    }
}

void
insert_before_markers_and_inherit (string, length)
     unsigned char *string;
     register int length;
{
  if (length > 0)
    {
      register int opoint = PT;
      insert_1 (string, length, 1, 1);
      adjust_markers (opoint - 1, opoint, length);
      signal_after_change (PT-length, 0, length);
    }
}

/* Insert part of a Lisp string, relocating markers after.  */

void
insert_from_string_before_markers (string, pos, length, inherit)
     Lisp_Object string;
     register int pos, length;
     int inherit;
{
  if (length > 0)
    {
      register int opoint = PT;
      insert_from_string_1 (string, pos, length, inherit);
      adjust_markers (opoint - 1, opoint, length);
      signal_after_change (PT-length, 0, length);
    }
}

/* Replace the text from FROM to TO with NEW,
   If PREPARE is nonzero, call prepare_to_modify_buffer.
   If INHERIT, the newly inserted text should inherit text properties
   from the surrounding non-deleted text.  */

/* Note that this does not yet handle markers quite right.
   Also it needs to record a single undo-entry that does a replacement
   rather than a separate delete and insert.
   That way, undo will also handle markers properly.  */

void
replace_range (from, to, new, prepare, inherit)
     Lisp_Object new;
     int from, to, prepare, inherit;
{
  int numdel;
  int inslen = XSTRING (new)->size;
  register Lisp_Object temp;
  struct gcpro gcpro1;

  GCPRO1 (new);

  if (prepare)
    {
      int range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;
    }

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  UNGCPRO;

  numdel = to - from;

  /* Make sure point-max won't overflow after this insertion.  */
  XSETINT (temp, Z - numdel + inslen);
  if (Z - numdel + inslen != XINT (temp))
    error ("maximum buffer size exceeded");

  if (numdel <= 0 && inslen == 0)
    return;

  GCPRO1 (new);

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from);
  if (to < GPT)
    gap_left (to, 0);

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.
     This has to be done before recording the deletion,
     so undo handles this after reinserting the text.  */
  adjust_markers (to + GAP_SIZE, to + GAP_SIZE, - numdel - GAP_SIZE);

  record_delete (from, numdel);

  GAP_SIZE += numdel;
  ZV -= numdel;
  Z -= numdel;
  GPT = from;
  *(GPT_ADDR) = 0;		/* Put an anchor.  */

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  if (GAP_SIZE < inslen)
    make_gap (inslen - GAP_SIZE);

  record_insert (from, inslen);

  bcopy (XSTRING (new)->data, GPT_ADDR, inslen);

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point (from + inslen - (PT < to ? PT : to));

#ifdef USE_TEXT_PROPERTIES
  offset_intervals (current_buffer, PT, inslen - numdel);
#endif

  GAP_SIZE -= inslen;
  GPT += inslen;
  ZV += inslen;
  Z += inslen;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  /* Adjust the overlay center as needed.  This must be done after
     adjusting the markers that bound the overlays.  */
  adjust_overlays_for_delete (from, numdel);
  adjust_overlays_for_insert (from, inslen);
  adjust_markers_for_insert (from, inslen);

#ifdef USE_TEXT_PROPERTIES
  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  graft_intervals_into_buffer (XSTRING (new)->intervals, from, inslen,
			       current_buffer, inherit);
#endif

  if (inslen == 0)
    evaporate_overlays (from);

  MODIFF++;
  UNGCPRO;

  signal_after_change (from, numdel, inslen);
}

/* Delete characters in current buffer
   from FROM up to (but not including) TO.  */

void
del_range (from, to)
     register int from, to;
{
  del_range_1 (from, to, 1);
}

/* Like del_range; PREPARE says whether to call prepare_to_modify_buffer.  */

void
del_range_1 (from, to, prepare)
     int from, to, prepare;
{
  register int numdel;

  if (prepare)
    {
      int range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;
    }

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if ((numdel = to - from) <= 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from);
  if (to < GPT)
    gap_left (to, 0);

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.
     This has to be done before recording the deletion,
     so undo handles this after reinserting the text.  */
  adjust_markers (to + GAP_SIZE, to + GAP_SIZE, - numdel - GAP_SIZE);

  record_delete (from, numdel);
  MODIFF++;

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point (from - (PT < to ? PT : to));

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  offset_intervals (current_buffer, from, - numdel);

  /* Adjust the overlay center as needed.  This must be done after
     adjusting the markers that bound the overlays.  */
  adjust_overlays_for_delete (from, numdel);

  GAP_SIZE += numdel;
  ZV -= numdel;
  Z -= numdel;
  GPT = from;
  *(GPT_ADDR) = 0;		/* Put an anchor.  */

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  evaporate_overlays (from);
  signal_after_change (from, numdel, 0);
}

/* Call this if you're about to change the region of BUFFER from START
   to END.  This checks the read-only properties of the region, calls
   the necessary modification hooks, and warns the next redisplay that
   it should pay attention to that area.  */
void
modify_region (buffer, start, end)
     struct buffer *buffer;
     int start, end;
{
  struct buffer *old_buffer = current_buffer;

  if (buffer != old_buffer)
    set_buffer_internal (buffer);

  prepare_to_modify_buffer (start, end, NULL);

  if (start - 1 < beg_unchanged
      || (unchanged_modified == MODIFF
	  && overlay_unchanged_modified == OVERLAY_MODIFF))
    beg_unchanged = start - 1;
  if (Z - end < end_unchanged
      || (unchanged_modified == MODIFF
	  && overlay_unchanged_modified == OVERLAY_MODIFF))
    end_unchanged = Z - end;

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();
  MODIFF++;

  buffer->point_before_scroll = Qnil;

  if (buffer != old_buffer)
    set_buffer_internal (old_buffer);
}

/* Check that it is okay to modify the buffer between START and END.
   Run the before-change-function, if any.  If intervals are in use,
   verify that the text to be modified is not read-only, and call
   any modification properties the text may have.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.  */

void
prepare_to_modify_buffer (start, end, preserve_ptr)
     int start, end;
     int *preserve_ptr;
{
  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
  if (BUF_INTERVALS (current_buffer) != 0)
    {
      if (preserve_ptr)
	{
	  Lisp_Object preserve_marker;
	  struct gcpro gcpro1;
	  preserve_marker = Fcopy_marker (make_number (*preserve_ptr), Qnil);
	  GCPRO1 (preserve_marker);
	  verify_interval_modification (current_buffer, start, end);
	  *preserve_ptr = marker_position (preserve_marker);
	  unchain_marker (preserve_marker);
	  UNGCPRO;
	}
      else
	verify_interval_modification (current_buffer, start, end);
    }

#ifdef CLASH_DETECTION
  if (!NILP (current_buffer->file_truename)
      /* Make binding buffer-file-name to nil effective.  */
      && !NILP (current_buffer->filename)
      && SAVE_MODIFF >= MODIFF)
    lock_file (current_buffer->file_truename);
#else
  /* At least warn if this file has changed on disk since it was visited.  */
  if (!NILP (current_buffer->filename)
      && SAVE_MODIFF >= MODIFF
      && NILP (Fverify_visited_file_modtime (Fcurrent_buffer ()))
      && !NILP (Ffile_exists_p (current_buffer->filename)))
    call1 (intern ("ask-user-about-supersession-threat"),
	   current_buffer->filename);
#endif /* not CLASH_DETECTION */

  signal_before_change (start, end, preserve_ptr);

  if (current_buffer->newline_cache)
    invalidate_region_cache (current_buffer,
                             current_buffer->newline_cache,
                             start - BEG, Z - end);
  if (current_buffer->width_run_cache)
    invalidate_region_cache (current_buffer,
                             current_buffer->width_run_cache,
                             start - BEG, Z - end);

  Vdeactivate_mark = Qt;
}

/* These macros work with an argument named `preserve_ptr'
   and a local variable named `preserve_marker'.  */

#define PRESERVE_VALUE							\
  if (preserve_ptr && NILP (preserve_marker))				\
    preserve_marker = Fcopy_marker (make_number (*preserve_ptr), Qnil)

#define RESTORE_VALUE						\
  if (! NILP (preserve_marker))					\
    {								\
      *preserve_ptr = marker_position (preserve_marker);	\
      unchain_marker (preserve_marker);				\
    }

#define PRESERVE_START_END			\
  if (NILP (start_marker))			\
    start_marker = Fcopy_marker (start, Qnil);	\
  if (NILP (end_marker))			\
    end_marker = Fcopy_marker (end, Qnil);

#define FETCH_START				\
  (! NILP (start_marker) ? Fmarker_position (start_marker) : start)

#define FETCH_END				\
  (! NILP (end_marker) ? Fmarker_position (end_marker) : end)

/* Signal a change to the buffer immediately before it happens.
   START_INT and END_INT are the bounds of the text to be changed.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.  */

void
signal_before_change (start_int, end_int, preserve_ptr)
     int start_int, end_int;
     int *preserve_ptr;
{
  Lisp_Object start, end;
  Lisp_Object start_marker, end_marker;
  Lisp_Object preserve_marker;
  struct gcpro gcpro1, gcpro2, gcpro3;

  start = make_number (start_int);
  end = make_number (end_int);
  preserve_marker = Qnil;
  start_marker = Qnil;
  end_marker = Qnil;
  GCPRO3 (preserve_marker, start_marker, end_marker);

  /* If buffer is unmodified, run a special hook for that case.  */
  if (SAVE_MODIFF >= MODIFF
      && !NILP (Vfirst_change_hook)
      && !NILP (Vrun_hooks))
    {
      PRESERVE_VALUE;
      PRESERVE_START_END;
      call1 (Vrun_hooks, Qfirst_change_hook);
    }

  /* Run the before-change-function if any.
     We don't bother "binding" this variable to nil
     because it is obsolete anyway and new code should not use it.  */
  if (!NILP (Vbefore_change_function))
    {
      PRESERVE_VALUE;
      PRESERVE_START_END;
      call2 (Vbefore_change_function, FETCH_START, FETCH_END);
    }

  /* Now run the before-change-functions if any.  */
  if (!NILP (Vbefore_change_functions))
    {
      Lisp_Object args[3];
      Lisp_Object before_change_functions;
      Lisp_Object after_change_functions;
      struct gcpro gcpro1, gcpro2;

      PRESERVE_VALUE;
      PRESERVE_START_END;

      /* "Bind" before-change-functions and after-change-functions
	 to nil--but in a way that errors don't know about.
	 That way, if there's an error in them, they will stay nil.  */
      before_change_functions = Vbefore_change_functions;
      after_change_functions = Vafter_change_functions;
      Vbefore_change_functions = Qnil;
      Vafter_change_functions = Qnil;
      GCPRO2 (before_change_functions, after_change_functions);

      /* Actually run the hook functions.  */
      args[0] = Qbefore_change_functions;
      args[1] = FETCH_START;
      args[2] = FETCH_END;
      run_hook_list_with_args (before_change_functions, 3, args);

      /* "Unbind" the variables we "bound" to nil.  */
      Vbefore_change_functions = before_change_functions;
      Vafter_change_functions = after_change_functions;
      UNGCPRO;
    }

  if (!NILP (current_buffer->overlays_before)
      || !NILP (current_buffer->overlays_after))
    {
      PRESERVE_VALUE;
      report_overlay_modification (FETCH_START, FETCH_END, 0,
				   FETCH_START, FETCH_END, Qnil);
    }

  if (! NILP (start_marker))
    free_marker (start_marker);
  if (! NILP (end_marker))
    free_marker (end_marker);
  RESTORE_VALUE;
  UNGCPRO;
}

/* Signal a change immediately after it happens.
   POS is the address of the start of the changed text.
   LENDEL is the number of characters of the text before the change.
   (Not the whole buffer; just the part that was changed.)
   LENINS is the number of characters in that part of the text
   after the change.  */

void
signal_after_change (pos, lendel, lenins)
     int pos, lendel, lenins;
{
  /* If we are deferring calls to the after-change functions
     and there are no before-change functions,
     just record the args that we were going to use.  */
  if (! NILP (Vcombine_after_change_calls)
      && NILP (Vbefore_change_function) && NILP (Vbefore_change_functions)
      && NILP (current_buffer->overlays_before)
      && NILP (current_buffer->overlays_after))
    {
      Lisp_Object elt;

      if (!NILP (combine_after_change_list)
	  && current_buffer != XBUFFER (combine_after_change_buffer))
	Fcombine_after_change_execute ();

      elt = Fcons (make_number (pos - BEG),
		   Fcons (make_number (Z - (pos - lendel + lenins)),
			  Fcons (make_number (lenins - lendel), Qnil)));
      combine_after_change_list
	= Fcons (elt, combine_after_change_list);
      combine_after_change_buffer = Fcurrent_buffer ();

      return;
    }

  if (!NILP (combine_after_change_list)) 
    Fcombine_after_change_execute ();

  /* Run the after-change-function if any.
     We don't bother "binding" this variable to nil
     because it is obsolete anyway and new code should not use it.  */
  if (!NILP (Vafter_change_function))
    call3 (Vafter_change_function,
	   make_number (pos), make_number (pos + lenins),
	   make_number (lendel));

  if (!NILP (Vafter_change_functions))
    {
      Lisp_Object args[4];
      Lisp_Object before_change_functions;
      Lisp_Object after_change_functions;
      struct gcpro gcpro1, gcpro2;

      /* "Bind" before-change-functions and after-change-functions
	 to nil--but in a way that errors don't know about.
	 That way, if there's an error in them, they will stay nil.  */
      before_change_functions = Vbefore_change_functions;
      after_change_functions = Vafter_change_functions;
      Vbefore_change_functions = Qnil;
      Vafter_change_functions = Qnil;
      GCPRO2 (before_change_functions, after_change_functions);

      /* Actually run the hook functions.  */
      args[0] = Qafter_change_functions;
      XSETFASTINT (args[1], pos);
      XSETFASTINT (args[2], pos + lenins);
      XSETFASTINT (args[3], lendel);
      run_hook_list_with_args (after_change_functions,
			       4, args);

      /* "Unbind" the variables we "bound" to nil.  */
      Vbefore_change_functions = before_change_functions;
      Vafter_change_functions = after_change_functions;
      UNGCPRO;
    }

  if (!NILP (current_buffer->overlays_before)
      || !NILP (current_buffer->overlays_after))
    report_overlay_modification (make_number (pos),
				 make_number (pos + lenins),
				 1,
				 make_number (pos), make_number (pos + lenins),
				 make_number (lendel));

  /* After an insertion, call the text properties
     insert-behind-hooks or insert-in-front-hooks.  */
  if (lendel == 0)
    report_interval_modification (pos, pos + lenins);
}

Lisp_Object
Fcombine_after_change_execute_1 (val)
     Lisp_Object val;
{
  Vcombine_after_change_calls = val;
  return val;
}

DEFUN ("combine-after-change-execute", Fcombine_after_change_execute,
  Scombine_after_change_execute, 0, 0, 0,
  "This function is for use internally in `combine-after-change-calls'.")
  ()
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  int beg, end, change;
  int begpos, endpos;
  Lisp_Object tail;

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

  Fset_buffer (combine_after_change_buffer);

  /* # chars unchanged at beginning of buffer.  */
  beg = Z - BEG;
  /* # chars unchanged at end of buffer.  */
  end = beg;
  /* Total amount of insertion (negative for deletion).  */
  change = 0;

  /* Scan the various individual changes,
     accumulating the range info in BEG, END and CHANGE.  */
  for (tail = combine_after_change_list; CONSP (tail);
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object elt;
      int thisbeg, thisend, thischange;

      /* Extract the info from the next element.  */
      elt = XCONS (tail)->car;
      if (! CONSP (elt))
	continue;
      thisbeg = XINT (XCONS (elt)->car);

      elt = XCONS (elt)->cdr;
      if (! CONSP (elt))
	continue;
      thisend = XINT (XCONS (elt)->car);

      elt = XCONS (elt)->cdr;
      if (! CONSP (elt))
	continue;
      thischange = XINT (XCONS (elt)->car);

      /* Merge this range into the accumulated range.  */
      change += thischange;
      if (thisbeg < beg)
	beg = thisbeg;
      if (thisend < end)
	end = thisend;
    }

  /* Get the current start and end positions of the range
     that was changed.  */
  begpos = BEG + beg;
  endpos = Z - end;
  
  /* We are about to handle these, so discard them.  */
  combine_after_change_list = Qnil;

  /* Now run the after-change functions for real.
     Turn off the flag that defers them.  */
  record_unwind_protect (Fcombine_after_change_execute_1,
			 Vcombine_after_change_calls);
  signal_after_change (begpos, endpos - begpos - change, endpos - begpos);

  return unbind_to (count, val);
}

syms_of_insdel ()
{
  staticpro (&combine_after_change_list);
  combine_after_change_list = Qnil;

  DEFVAR_LISP ("combine-after-change-calls", &Vcombine_after_change_calls,
     "Used internally by the `combine-after-change-calls' macro.");
  Vcombine_after_change_calls = Qnil;

  defsubr (&Scombine_after_change_execute);
}
